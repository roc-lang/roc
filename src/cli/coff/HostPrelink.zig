//! The pre-linked host of a fast dev link of a Windows executable.
//!
//! A platform's host is linked once, with the embedded LLD flags of a
//! Windows link plus a file alignment of one page, from the platform's link
//! inputs plus one generated object, and filed in the cache as a host
//! record: the linked executable's bytes followed by a side table describing
//! what the patch link needs of them (`PatchLink.zig`).
//!
//! The generated object (`linkObject`) defines every `provides` symbol as a
//! window the host's references resolve to at prelink time: a procedure's
//! window is 16 bytes in `.rocprov`, which each build fills with a jump to
//! the entry wrapper; a data symbol's window is the value's layout size in
//! `.rocpval`, which each build fills with the value's bytes and base
//! relocations for the pointers in it. Which provides are procedures, and
//! how large the values are, is part of the host's key. Every symbol the
//! app region may import (`keep`) is named to the link with `/include`,
//! which keeps its definition, pulls its archive member in, and gives a
//! symbol a DLL exports its import thunk; one nothing defines is left
//! unresolved (`/force:unresolved`), so the host links, and the side table
//! lists it as absent: an app importing it gets the missing-host-symbols
//! error. The page file alignment leaves the header page room for the
//! section headers the patch link appends.
const std = @import("std");
const host_prelink = @import("../host_prelink.zig");
const Allocator = std.mem.Allocator;

pub const ProvideKind = host_prelink.ProvideKind;
pub const Provide = host_prelink.Provide;
/// A procedure window: a jump to the entry wrapper and traps.
pub const procedure_window = host_prelink.procedure_window;
const window_align = host_prelink.window_align;
/// The section and file alignment of the host, which the prelink sets.
pub const page_size: u64 = 0x1000;

/// The section holding the procedure windows.
pub const procedure_section = ".rocprov";
/// The section holding the value windows.
pub const value_section = ".rocpval";

pub const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
pub const IMAGE_FILE_MACHINE_ARM64: u16 = 0xaa64;

const IMAGE_SCN_CNT_CODE: u32 = 0x0000_0020;
const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x0000_0040;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
const IMAGE_SCN_MEM_WRITE: u32 = 0x8000_0000;
const IMAGE_SCN_ALIGN_16BYTES: u32 = 0x0050_0000;
const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;
const IMAGE_SYM_DTYPE_FUNCTION: u16 = 0x20;
const coff_header_size = 20;
const coff_section_header_size = 40;
const coff_symbol_size = 18;

/// The generated object of a host link, for an x86_64 or arm64 host.
/// `provides` must be sorted by name and distinct.
pub fn linkObject(allocator: Allocator, arm64: bool, provides: []const Provide) Allocator.Error![]u8 {
    var procedures: u32 = 0;
    var value_size: u64 = 0;
    for (provides) |provide| switch (provide.kind) {
        .procedure => procedures += 1,
        .data => value_size += provide.windowSize(),
    };
    const procedure_size: u64 = procedures * procedure_window;
    const SectionSpec = struct { name: []const u8, size: u64, characteristics: u32 };
    var specs: [2]SectionSpec = undefined;
    var section_count: usize = 0;
    var procedure_number: u16 = 0;
    var value_number: u16 = 0;
    if (procedure_size != 0) {
        specs[section_count] = .{ .name = procedure_section, .size = procedure_size, .characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES };
        section_count += 1;
        procedure_number = @intCast(section_count);
    }
    if (value_size != 0) {
        specs[section_count] = .{ .name = value_section, .size = value_size, .characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES };
        section_count += 1;
        value_number = @intCast(section_count);
    }
    const sections = specs[0..section_count];
    const contents_offset: usize = coff_header_size + sections.len * coff_section_header_size;
    var offsets: [2]u32 = undefined;
    var contents_size: u64 = 0;
    for (sections, 0..) |section, index| {
        offsets[index] = @intCast(contents_offset + contents_size);
        contents_size += section.size;
    }
    const symtab_offset: usize = contents_offset + @as(usize, @intCast(contents_size));

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendNTimes(allocator, 0, symtab_offset);
    const bytes = out.items;
    std.mem.writeInt(u16, bytes[0..2], if (arm64) IMAGE_FILE_MACHINE_ARM64 else IMAGE_FILE_MACHINE_AMD64, .little);
    std.mem.writeInt(u16, bytes[2..4], @intCast(sections.len), .little);
    std.mem.writeInt(u32, bytes[8..12], @intCast(symtab_offset), .little);
    std.mem.writeInt(u32, bytes[12..16], @intCast(provides.len), .little);
    for (sections, 0..) |section, index| {
        const header = bytes[coff_header_size + index * coff_section_header_size ..][0..coff_section_header_size];
        @memcpy(header[0..section.name.len], section.name);
        std.mem.writeInt(u32, header[16..20], @intCast(section.size), .little);
        std.mem.writeInt(u32, header[20..24], offsets[index], .little);
        std.mem.writeInt(u32, header[36..40], section.characteristics, .little);
    }
    // Procedure windows trap until a build fills them.
    if (procedure_number != 0) {
        const window_bytes = bytes[offsets[procedure_number - 1]..][0..@intCast(procedure_size)];
        if (arm64) {
            var word: usize = 0;
            while (word < window_bytes.len) : (word += 4) std.mem.writeInt(u32, window_bytes[word..][0..4], brk_instruction, .little);
        } else {
            @memset(window_bytes, int3_instruction);
        }
    }

    // Symbols: the provides as defined externals, in order.
    var strtab: std.ArrayList(u8) = .empty;
    defer strtab.deinit(allocator);
    try strtab.appendNTimes(allocator, 0, 4);
    var procedure_at: u64 = 0;
    var value_at: u64 = 0;
    for (provides) |provide| {
        var entry: [coff_symbol_size]u8 = @splat(0);
        if (provide.name.len <= 8) {
            @memcpy(entry[0..provide.name.len], provide.name);
        } else {
            std.mem.writeInt(u32, entry[4..8], @intCast(strtab.items.len), .little);
            try strtab.appendSlice(allocator, provide.name);
            try strtab.append(allocator, 0);
        }
        switch (provide.kind) {
            .procedure => {
                std.mem.writeInt(u32, entry[8..12], @intCast(procedure_at), .little);
                std.mem.writeInt(u16, entry[12..14], procedure_number, .little);
                std.mem.writeInt(u16, entry[14..16], IMAGE_SYM_DTYPE_FUNCTION, .little);
                procedure_at += procedure_window;
            },
            .data => {
                std.mem.writeInt(u32, entry[8..12], @intCast(value_at), .little);
                std.mem.writeInt(u16, entry[12..14], value_number, .little);
                value_at += provide.windowSize();
            },
        }
        entry[16] = IMAGE_SYM_CLASS_EXTERNAL;
        try out.appendSlice(allocator, &entry);
    }
    std.mem.writeInt(u32, strtab.items[0..4], @intCast(strtab.items.len), .little);
    try out.appendSlice(allocator, strtab.items);
    return try out.toOwnedSlice(allocator);
}

const brk_instruction: u32 = 0xd420_0000;
const int3_instruction: u8 = 0xcc;

/// The record format; bumped whenever `finish`'s output changes.
pub const format_version: u32 = 1;
const magic = "RHSC".*;

/// One symbol the app region may import, as the host defines it.
pub const HostSymbol = extern struct {
    name: u32,
    reserved: u32 = 0,
    /// The symbol's image-relative address: a definition's, or the import
    /// thunk's for a symbol a DLL exports.
    rva: u64,
};

/// One `provides` symbol's window in the host.
pub const ProvideRecord = extern struct {
    name: u32,
    kind: u8,
    reserved: [3]u8 = .{ 0, 0, 0 },
    rva: u64,
    /// The window's offset in the host's bytes.
    offset: u64,
    /// The window's bytes.
    size: u64,
};

/// The record's trailer: the last bytes of the record.
const Trailer = extern struct {
    magic: [4]u8,
    version: u32,
    /// The host's bytes: the whole executable.
    host_size: u64,
    /// Where the host's symbol table starts, which is where its headers and
    /// section contents end.
    sections_end: u64,
    symbol_count: u32,
    provide_count: u32,
    symbols_offset: u64,
    provides_offset: u64,
    strings_offset: u64,
    strings_size: u64,
    total_size: u64,
    /// BLAKE3 of the host's bytes: what identifies the host in a link's
    /// build id, whatever compiler or key produced it.
    host_digest: [32]u8,
};

/// Why a linked host cannot be made into a record. Each is reported to the
/// user as a failed link of the platform's host.
pub const FinishError = Allocator.Error || error{
    /// The bytes are not the kind of executable this prelink links.
    UnexpectedHostLayout,
    /// The host does not fit the record.
    HostTooLarge,
};

/// Make the host executable LLD linked from `linkObject(provides)` and the
/// platform's inputs into a host record. `keep` must be sorted and
/// distinct.
pub fn finish(allocator: Allocator, exe: []const u8, provides: []const Provide, keep: []const []const u8) FinishError![]align(16) u8 {
    var parsed: Executable.Parsed = undefined;
    const image = parsed.parse(exe) catch return error.UnexpectedHostLayout;
    if (image.file_alignment != page_size or image.section_alignment != page_size) return error.UnexpectedHostLayout;
    if (image.symtab_offset == 0) return error.UnexpectedHostLayout;
    // The symbol table and its strings end the file, after every section.
    if (image.symtab_offset < image.sectionsEnd()) return error.UnexpectedHostLayout;
    const strtab_offset = image.symtab_offset + image.symbol_count * coff_symbol_size;
    if (strtab_offset + 4 > exe.len) return error.UnexpectedHostLayout;
    const strtab_size = std.mem.readInt(u32, exe[strtab_offset..][0..4], .little);
    if (strtab_size < 4 or strtab_offset + strtab_size != exe.len) return error.UnexpectedHostLayout;
    const strtab = exe[strtab_offset..][0..strtab_size];

    // The windows, where LLD placed the sections.
    var procedure_section_index: ?usize = null;
    var value_section_index: ?usize = null;
    for (image.sections, 0..) |section, index| {
        if (std.mem.eql(u8, section.name, procedure_section)) procedure_section_index = index;
        if (std.mem.eql(u8, section.name, value_section)) value_section_index = index;
    }
    var strings: std.ArrayList(u8) = .empty;
    defer strings.deinit(allocator);
    try strings.append(allocator, 0);
    const provide_records = try allocator.alloc(ProvideRecord, provides.len);
    defer allocator.free(provide_records);
    var procedure_at: u64 = 0;
    var value_at: u64 = 0;
    for (provides, provide_records) |provide, *record| {
        const section = image.sections[
            switch (provide.kind) {
                .procedure => procedure_section_index orelse return error.UnexpectedHostLayout,
                .data => value_section_index orelse return error.UnexpectedHostLayout,
            }
        ];
        const within = switch (provide.kind) {
            .procedure => procedure_at,
            .data => value_at,
        };
        const size = provide.windowSize();
        if (within + size > section.raw_size or section.raw_offset == 0) return error.UnexpectedHostLayout;
        record.* = .{
            .name = @intCast(strings.items.len),
            .kind = @backingInt(provide.kind),
            .rva = section.rva + within,
            .offset = section.raw_offset + within,
            .size = size,
        };
        try strings.appendSlice(allocator, provide.name);
        try strings.append(allocator, 0);
        switch (provide.kind) {
            .procedure => procedure_at += size,
            .data => value_at += size,
        }
    }

    // Each kept symbol the host defines, at its image-relative address; a
    // symbol left unresolved, or present only as an import address table
    // entry, is absent.
    const found = try allocator.alloc(?u64, keep.len);
    defer allocator.free(found);
    @memset(found, null);
    var index: usize = 0;
    while (index < image.symbol_count) {
        const entry = exe[image.symtab_offset + index * coff_symbol_size ..][0..coff_symbol_size];
        const aux_count: usize = entry[17];
        defer index += 1 + aux_count;
        const section_number: i16 = @bitCast(std.mem.readInt(u16, entry[12..14], .little));
        if (section_number <= 0 or @as(usize, @intCast(section_number)) > image.sections.len) continue;
        const name = symbolName(entry[0..8], strtab) catch return error.UnexpectedHostLayout;
        const keep_index = findSorted(keep, name) orelse continue;
        const value = std.mem.readInt(u32, entry[8..12], .little);
        found[keep_index] = image.sections[@intCast(section_number - 1)].rva + value;
    }
    var symbols: std.ArrayList(HostSymbol) = .empty;
    defer symbols.deinit(allocator);
    for (keep, found) |name, rva_or_null| {
        const rva = rva_or_null orelse continue;
        try symbols.append(allocator, .{ .name = @intCast(strings.items.len), .rva = rva });
        try strings.appendSlice(allocator, name);
        try strings.append(allocator, 0);
    }

    var end: u64 = std.mem.alignForward(u64, exe.len, 16);
    const symbols_offset = end;
    end += symbols.items.len * @sizeOf(HostSymbol);
    const provides_offset = end;
    end += provide_records.len * @sizeOf(ProvideRecord);
    const strings_offset = end;
    end += strings.items.len;
    end = std.mem.alignForward(u64, end, 16);
    const trailer_offset = end;
    end += @sizeOf(Trailer);
    const total = std.mem.alignForward(u64, end, 16);
    if (total > std.math.maxInt(u32)) return error.HostTooLarge;

    const out = try allocator.alignedAlloc(u8, .@"16", @intCast(total));
    @memset(out, 0);
    @memcpy(out[0..exe.len], exe);
    @memcpy(out[@intCast(symbols_offset)..][0 .. symbols.items.len * @sizeOf(HostSymbol)], std.mem.sliceAsBytes(symbols.items));
    @memcpy(out[@intCast(provides_offset)..][0 .. provide_records.len * @sizeOf(ProvideRecord)], std.mem.sliceAsBytes(provide_records));
    @memcpy(out[@intCast(strings_offset)..][0..strings.items.len], strings.items);
    var host_digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(exe, &host_digest, .{});
    const trailer: Trailer = .{
        .magic = magic,
        .version = format_version,
        .host_size = exe.len,
        .sections_end = image.symtab_offset,
        .symbol_count = @intCast(symbols.items.len),
        .provide_count = @intCast(provide_records.len),
        .symbols_offset = symbols_offset,
        .provides_offset = provides_offset,
        .strings_offset = strings_offset,
        .strings_size = strings.items.len,
        .total_size = total,
        .host_digest = host_digest,
    };
    @memcpy(out[@intCast(trailer_offset)..][0..@sizeOf(Trailer)], std.mem.asBytes(&trailer));
    return out;
}

/// A COFF symbol's name: inline, or an offset into the string table when
/// the field's first word is zero.
pub fn symbolName(field: *const [8]u8, strtab: []const u8) error{InvalidExecutable}![]const u8 {
    if (std.mem.readInt(u32, field[0..4], .little) != 0) return std.mem.sliceTo(field, 0);
    const offset = std.mem.readInt(u32, field[4..8], .little);
    if (offset >= strtab.len) return error.InvalidExecutable;
    return std.mem.sliceTo(strtab[offset..], 0);
}

/// Why bytes are not a host record this compiler wrote.
pub const ReadError = error{InvalidRecord};

/// Read access to a host record, borrowing its bytes.
pub const View = struct {
    bytes: []align(16) const u8,
    trailer: Trailer,

    pub fn init(bytes: []align(16) const u8) ReadError!View {
        if (bytes.len < @sizeOf(Trailer) or bytes.len % 16 != 0) return error.InvalidRecord;
        const trailer_offset = std.mem.alignBackward(usize, bytes.len - @sizeOf(Trailer), 16);
        const trailer = std.mem.bytesToValue(Trailer, bytes[trailer_offset..][0..@sizeOf(Trailer)]);
        if (!std.mem.eql(u8, &trailer.magic, &magic) or trailer.version != format_version) return error.InvalidRecord;
        if (trailer.total_size != bytes.len) return error.InvalidRecord;
        if (trailer.host_size > trailer_offset or trailer.sections_end > trailer.host_size) return error.InvalidRecord;
        if (trailer.symbols_offset % 8 != 0 or trailer.provides_offset % 8 != 0) return error.InvalidRecord;
        if (trailer.symbols_offset + @as(u64, trailer.symbol_count) * @sizeOf(HostSymbol) > trailer_offset) return error.InvalidRecord;
        if (trailer.provides_offset + @as(u64, trailer.provide_count) * @sizeOf(ProvideRecord) > trailer_offset) return error.InvalidRecord;
        if (trailer.strings_offset + trailer.strings_size > trailer_offset or trailer.strings_size == 0) return error.InvalidRecord;
        if (bytes[@intCast(trailer.strings_offset + trailer.strings_size - 1)] != 0) return error.InvalidRecord;
        return .{ .bytes = bytes, .trailer = trailer };
    }

    /// The host executable's bytes.
    pub fn host(self: View) []const u8 {
        return self.bytes[0..@intCast(self.trailer.host_size)];
    }
    /// The host's headers and section contents: its bytes before the
    /// symbol table.
    pub fn sections(self: View) []const u8 {
        return self.bytes[0..@intCast(self.trailer.sections_end)];
    }
    pub fn symbols(self: View) []const HostSymbol {
        const start: [*]const HostSymbol = @ptrCast(@alignCast(self.bytes.ptr + self.trailer.symbols_offset));
        return start[0..self.trailer.symbol_count];
    }
    pub fn provides(self: View) []const ProvideRecord {
        const start: [*]const ProvideRecord = @ptrCast(@alignCast(self.bytes.ptr + self.trailer.provides_offset));
        return start[0..self.trailer.provide_count];
    }
    pub fn name(self: View, offset: u32) []const u8 {
        return std.mem.sliceTo(self.bytes[@intCast(self.trailer.strings_offset + offset)..], 0);
    }
    /// The kept symbol `wanted`, when the host provides it.
    pub fn findSymbol(self: View, wanted: []const u8) ?HostSymbol {
        const all = self.symbols();
        var low: usize = 0;
        var high: usize = all.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            switch (std.mem.order(u8, self.name(all[mid].name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return all[mid],
            }
        }
        return null;
    }
};

/// The headers of a PE32+ executable the patch link rewrites.
pub const Executable = struct {
    machine: u16,
    /// The file offset of the COFF file header, after the DOS stub.
    coff_header_offset: u32,
    optional_header_size: u16,
    section_headers_offset: u32,
    sections: []const Section,
    image_base: u64,
    section_alignment: u32,
    file_alignment: u32,
    size_of_headers: u32,
    size_of_image: u32,
    symtab_offset: u32,
    symbol_count: u32,
    directories: [16]Directory,

    pub const Section = struct {
        name: []const u8,
        rva: u32,
        virtual_size: u32,
        raw_size: u32,
        raw_offset: u32,
        characteristics: u32,
    };
    pub const Directory = struct { rva: u32, size: u32 };

    pub const ParseError = error{InvalidExecutable};

    /// The data directory indices the patch link reads or writes.
    pub const exception_directory = 3;
    pub const base_relocation_directory = 5;
    pub const debug_directory = 6;

    pub const max_sections = 96;

    /// Storage for a parse's lists, which outlive the call only inside the
    /// returned `Executable` when the caller keeps it beside the struct.
    pub const Parsed = struct {
        sections: [max_sections]Section,

        pub fn parse(self: *Parsed, bytes: []const u8) ParseError!Executable {
            if (bytes.len < 0x40 or bytes[0] != 'M' or bytes[1] != 'Z') return error.InvalidExecutable;
            const pe_offset = std.mem.readInt(u32, bytes[0x3c..0x40], .little);
            if (@as(u64, pe_offset) + 4 + coff_header_size + 240 > bytes.len) return error.InvalidExecutable;
            if (!std.mem.eql(u8, bytes[pe_offset..][0..4], "PE\x00\x00")) return error.InvalidExecutable;
            const coff = pe_offset + 4;
            const header = bytes[coff..][0..coff_header_size];
            const machine = std.mem.readInt(u16, header[0..2], .little);
            if (machine != IMAGE_FILE_MACHINE_AMD64 and machine != IMAGE_FILE_MACHINE_ARM64) return error.InvalidExecutable;
            const section_count = std.mem.readInt(u16, header[2..4], .little);
            const optional_header_size = std.mem.readInt(u16, header[16..18], .little);
            if (section_count > max_sections or optional_header_size < 240) return error.InvalidExecutable;
            const optional = bytes[coff + coff_header_size ..][0..240];
            if (std.mem.readInt(u16, optional[0..2], .little) != 0x20b) return error.InvalidExecutable;
            const number_of_directories = std.mem.readInt(u32, optional[108..112], .little);
            if (number_of_directories != 16) return error.InvalidExecutable;
            var exe: Executable = .{
                .machine = machine,
                .coff_header_offset = coff,
                .optional_header_size = optional_header_size,
                .section_headers_offset = coff + coff_header_size + optional_header_size,
                .sections = &.{},
                .image_base = std.mem.readInt(u64, optional[24..32], .little),
                .section_alignment = std.mem.readInt(u32, optional[32..36], .little),
                .file_alignment = std.mem.readInt(u32, optional[36..40], .little),
                .size_of_headers = std.mem.readInt(u32, optional[60..64], .little),
                .size_of_image = std.mem.readInt(u32, optional[56..60], .little),
                .symtab_offset = std.mem.readInt(u32, header[8..12], .little),
                .symbol_count = std.mem.readInt(u32, header[12..16], .little),
                .directories = undefined,
            };
            for (&exe.directories, 0..) |*directory, index| {
                directory.* = .{ .rva = std.mem.readInt(u32, optional[112 + index * 8 ..][0..4], .little), .size = std.mem.readInt(u32, optional[116 + index * 8 ..][0..4], .little) };
            }
            if (@as(u64, exe.section_headers_offset) + @as(u64, section_count) * coff_section_header_size > exe.size_of_headers or exe.size_of_headers > bytes.len) return error.InvalidExecutable;
            if (@as(u64, exe.symtab_offset) + @as(u64, exe.symbol_count) * coff_symbol_size > bytes.len) return error.InvalidExecutable;
            // Long section names are read through the string table.
            var strtab: []const u8 = &.{};
            if (exe.symtab_offset != 0) {
                const strtab_offset = exe.symtab_offset + exe.symbol_count * coff_symbol_size;
                if (@as(u64, strtab_offset) + 4 > bytes.len) return error.InvalidExecutable;
                const strtab_size = std.mem.readInt(u32, bytes[strtab_offset..][0..4], .little);
                if (strtab_size < 4 or @as(u64, strtab_offset) + strtab_size > bytes.len) return error.InvalidExecutable;
                strtab = bytes[strtab_offset..][0..strtab_size];
            }
            var previous_end: u64 = 0;
            for (0..section_count) |index| {
                const section_header = bytes[exe.section_headers_offset + index * coff_section_header_size ..][0..coff_section_header_size];
                const section: Section = .{
                    .name = try sectionName(section_header[0..8], strtab),
                    .virtual_size = std.mem.readInt(u32, section_header[8..12], .little),
                    .rva = std.mem.readInt(u32, section_header[12..16], .little),
                    .raw_size = std.mem.readInt(u32, section_header[16..20], .little),
                    .raw_offset = std.mem.readInt(u32, section_header[20..24], .little),
                    .characteristics = std.mem.readInt(u32, section_header[36..40], .little),
                };
                // Sections ascend without overlap in memory and in the file.
                if (section.rva < previous_end) return error.InvalidExecutable;
                previous_end = section.rva + @max(section.virtual_size, section.raw_size);
                if (section.raw_size != 0 and (section.raw_offset < exe.size_of_headers or @as(u64, section.raw_offset) + section.raw_size > bytes.len)) return error.InvalidExecutable;
                self.sections[index] = section;
            }
            exe.sections = self.sections[0..section_count];
            return exe;
        }
    };

    /// The end of the last section's contents in the file.
    pub fn sectionsEnd(self: Executable) u64 {
        var end: u64 = self.size_of_headers;
        for (self.sections) |section| {
            if (section.raw_size != 0) end = @max(end, @as(u64, section.raw_offset) + section.raw_size);
        }
        return end;
    }

    /// The section holding `rva`, if any.
    pub fn sectionOf(self: Executable, rva: u64) ?usize {
        for (self.sections, 0..) |section, index| {
            if (rva >= section.rva and rva < section.rva + @max(section.virtual_size, section.raw_size)) return index;
        }
        return null;
    }
};

/// A section header's name: inline, or a "/decimal" offset into the string
/// table.
fn sectionName(field: *const [8]u8, strtab: []const u8) error{InvalidExecutable}![]const u8 {
    if (field[0] != '/') return std.mem.sliceTo(field, 0);
    const digits = std.mem.sliceTo(field[1..], 0);
    if (digits.len == 0) return error.InvalidExecutable;
    var offset: u64 = 0;
    for (digits) |digit| {
        if (digit < '0' or digit > '9') return error.InvalidExecutable;
        offset = offset * 10 + (digit - '0');
    }
    if (offset >= strtab.len) return error.InvalidExecutable;
    return std.mem.sliceTo(strtab[@intCast(offset)..], 0);
}

fn findSorted(list: []const []const u8, wanted: []const u8) ?usize {
    var low: usize = 0;
    var high: usize = list.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        switch (std.mem.order(u8, list[mid], wanted)) {
            .lt => low = mid + 1,
            .gt => high = mid,
            .eq => return mid,
        }
    }
    return null;
}

test "the generated object defines each provide in its window section" {
    const allocator = std.testing.allocator;
    const provides = [_]Provide{
        .{ .name = "roc__main_for_host_1_exposed_generic", .kind = .procedure, .size = 0 },
        .{ .name = "roc__value", .kind = .data, .size = 24 },
    };
    const object = try linkObject(allocator, false, &provides);
    defer allocator.free(object);
    try std.testing.expectEqual(IMAGE_FILE_MACHINE_AMD64, std.mem.readInt(u16, object[0..2], .little));
    try std.testing.expectEqual(@as(u16, 2), std.mem.readInt(u16, object[2..4], .little));
    const first = object[coff_header_size..][0..coff_section_header_size];
    try std.testing.expectEqualStrings(procedure_section, std.mem.sliceTo(first[0..8], 0));
    try std.testing.expectEqual(@as(u32, procedure_window), std.mem.readInt(u32, first[16..20], .little));
    const second = object[coff_header_size + coff_section_header_size ..][0..coff_section_header_size];
    try std.testing.expectEqualStrings(value_section, std.mem.sliceTo(second[0..8], 0));
    try std.testing.expectEqual(@as(u32, 32), std.mem.readInt(u32, second[16..20], .little));
    // The procedure window traps.
    const windows = std.mem.readInt(u32, first[20..24], .little);
    try std.testing.expectEqual(int3_instruction, object[windows]);
    const symtab = std.mem.readInt(u32, object[8..12], .little);
    try std.testing.expectEqual(@as(u32, 2), std.mem.readInt(u32, object[12..16], .little));
    const procedure = object[symtab..][0..coff_symbol_size];
    try std.testing.expectEqual(@as(u16, 1), std.mem.readInt(u16, procedure[12..14], .little));
    try std.testing.expectEqual(IMAGE_SYM_DTYPE_FUNCTION, std.mem.readInt(u16, procedure[14..16], .little));
    const strtab = object[symtab + 2 * coff_symbol_size ..];
    try std.testing.expectEqualStrings("roc__main_for_host_1_exposed_generic", try symbolName(procedure[0..8], strtab));
    const value = object[symtab + coff_symbol_size ..][0..coff_symbol_size];
    try std.testing.expectEqual(@as(u16, 2), std.mem.readInt(u16, value[12..14], .little));
    try std.testing.expectEqualStrings("roc__value", try symbolName(value[0..8], strtab));
    const arm = try linkObject(allocator, true, &provides);
    defer allocator.free(arm);
    try std.testing.expectEqual(IMAGE_FILE_MACHINE_ARM64, std.mem.readInt(u16, arm[0..2], .little));
    try std.testing.expectEqual(brk_instruction, std.mem.readInt(u32, arm[windows..][0..4], .little));
}

test "a record reads back its symbols and provides" {
    const allocator = std.testing.allocator;
    const provides = [_]Provide{
        .{ .name = "roc__main", .kind = .procedure, .size = 0 },
        .{ .name = "roc__value", .kind = .data, .size = 8 },
    };
    const keep = [_][]const u8{ "WriteFile", "memcpy", "roc_alloc" };
    const exe = try TestExecutable.encode(allocator, &.{
        .{ .name = ".text", .bytes = &@as([16]u8, @splat(0x90)), .characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ },
        .{ .name = procedure_section, .bytes = &@as([16]u8, @splat(0xcc)), .characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ },
        .{ .name = value_section, .bytes = &@as([16]u8, @splat(0)), .characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE },
        .{ .name = ".debug_abbrev", .bytes = "abbrev", .characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ },
    }, &.{
        .{ .name = "WriteFile", .section = 1, .value = 8 },
        .{ .name = "__imp_memcpy", .section = 1, .value = 0 },
        .{ .name = "roc__main", .section = 2, .value = 0 },
    });
    defer allocator.free(exe);
    const record = try finish(allocator, exe, &provides, &keep);
    defer allocator.free(record);
    const view = try View.init(record);
    try std.testing.expectEqualSlices(u8, exe, view.host());
    try std.testing.expectEqual(@as(usize, 1), view.symbols().len);
    try std.testing.expectEqual(@as(u64, 0x1008), view.findSymbol("WriteFile").?.rva);
    try std.testing.expect(view.findSymbol("memcpy") == null);
    try std.testing.expect(view.findSymbol("roc_alloc") == null);
    try std.testing.expectEqual(@as(usize, 2), view.provides().len);
    try std.testing.expectEqual(@as(u64, 0x2000), view.provides()[0].rva);
    try std.testing.expectEqual(@as(u64, 0x3000), view.provides()[1].rva);
    try std.testing.expectEqual(@as(u64, 16), view.provides()[1].size);
    try std.testing.expectEqualStrings("roc__value", view.name(view.provides()[1].name));
    var parsed: Executable.Parsed = undefined;
    const image = try parsed.parse(view.host());
    try std.testing.expectEqual(@as(usize, 4), image.sections.len);
    try std.testing.expectEqualStrings(".debug_abbrev", image.sections[3].name);
    try std.testing.expectEqual(view.trailer.sections_end, image.sectionsEnd());
}

/// A minimal PE32+ executable for tests: one page of headers, each section
/// in its own page, then a symbol table and string table.
pub const TestExecutable = struct {
    pub const TestSection = struct { name: []const u8, bytes: []const u8, characteristics: u32 };
    pub const TestSymbol = struct { name: []const u8, section: u16, value: u32 };

    pub fn encode(allocator: Allocator, sections: []const TestSection, symbols: []const TestSymbol) Allocator.Error![]u8 {
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(allocator);
        try out.appendNTimes(allocator, 0, @intCast(page_size * (1 + sections.len)));
        var strtab: std.ArrayList(u8) = .empty;
        defer strtab.deinit(allocator);
        try strtab.appendNTimes(allocator, 0, 4);
        const bytes = out.items;
        bytes[0] = 'M';
        bytes[1] = 'Z';
        const pe: u32 = 0x80;
        std.mem.writeInt(u32, bytes[0x3c..0x40], pe, .little);
        @memcpy(bytes[pe..][0..4], "PE\x00\x00");
        const coff = pe + 4;
        std.mem.writeInt(u16, bytes[coff..][0..2], IMAGE_FILE_MACHINE_AMD64, .little);
        std.mem.writeInt(u16, bytes[coff + 2 ..][0..2], @intCast(sections.len), .little);
        std.mem.writeInt(u32, bytes[coff + 8 ..][0..4], @intCast(page_size * (1 + sections.len)), .little);
        std.mem.writeInt(u32, bytes[coff + 12 ..][0..4], @intCast(symbols.len), .little);
        std.mem.writeInt(u16, bytes[coff + 16 ..][0..2], 240, .little);
        const optional = bytes[coff + coff_header_size ..][0..240];
        std.mem.writeInt(u16, optional[0..2], 0x20b, .little);
        std.mem.writeInt(u64, optional[24..32], 0x1_4000_0000, .little);
        std.mem.writeInt(u32, optional[32..36], @intCast(page_size), .little);
        std.mem.writeInt(u32, optional[36..40], @intCast(page_size), .little);
        std.mem.writeInt(u32, optional[56..60], @intCast(page_size * (1 + sections.len)), .little);
        std.mem.writeInt(u32, optional[60..64], @intCast(page_size), .little);
        std.mem.writeInt(u32, optional[108..112], 16, .little);
        const headers = coff + coff_header_size + 240;
        for (sections, 0..) |section, index| {
            const header = bytes[headers + index * coff_section_header_size ..][0..coff_section_header_size];
            if (section.name.len <= 8) {
                @memcpy(header[0..section.name.len], section.name);
            } else {
                header[0] = '/';
                _ = std.fmt.printInt(header[1..8], strtab.items.len, 10, .lower, .{});
                try strtab.appendSlice(allocator, section.name);
                try strtab.append(allocator, 0);
            }
            const rva: u32 = @intCast(page_size * (1 + index));
            std.mem.writeInt(u32, header[8..12], @intCast(section.bytes.len), .little);
            std.mem.writeInt(u32, header[12..16], rva, .little);
            std.mem.writeInt(u32, header[16..20], @intCast(page_size), .little);
            std.mem.writeInt(u32, header[20..24], rva, .little);
            std.mem.writeInt(u32, header[36..40], section.characteristics, .little);
            @memcpy(bytes[rva..][0..section.bytes.len], section.bytes);
        }
        for (symbols) |symbol| {
            var entry: [coff_symbol_size]u8 = @splat(0);
            if (symbol.name.len <= 8) {
                @memcpy(entry[0..symbol.name.len], symbol.name);
            } else {
                std.mem.writeInt(u32, entry[4..8], @intCast(strtab.items.len), .little);
                try strtab.appendSlice(allocator, symbol.name);
                try strtab.append(allocator, 0);
            }
            std.mem.writeInt(u32, entry[8..12], symbol.value, .little);
            std.mem.writeInt(u16, entry[12..14], symbol.section, .little);
            entry[16] = IMAGE_SYM_CLASS_EXTERNAL;
            try out.appendSlice(allocator, &entry);
        }
        std.mem.writeInt(u32, strtab.items[0..4], @intCast(strtab.items.len), .little);
        try out.appendSlice(allocator, strtab.items);
        return try out.toOwnedSlice(allocator);
    }
};
