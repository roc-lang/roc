//! The pre-linked host of a fast dev link of a macOS arm64 executable.
//!
//! A platform's host is linked once, with the embedded LLD flags of a macOS
//! link plus `-no_fixup_chains` and a header pad for the app's load
//! commands, from the platform's link inputs plus one generated object, and
//! filed in the cache as a host record: the linked executable's bytes up to
//! its code signature, followed by a side table describing what the patch
//! link needs of them (`PatchLink.zig`).
//!
//! The generated object (`linkObject`) defines every `provides` symbol as a
//! window the host's references resolve to at prelink time: a procedure's
//! window is 16 bytes in `__TEXT,__roc_prov`, which each build fills with a
//! long jump to the entry wrapper; a data symbol's window is the value's
//! layout size in `__DATA,__roc_prov`, which each build fills with the
//! value's bytes and rebase entries for the pointers in it. Which provides
//! are procedures, and how large the values are, is part of the host's key.
//! The object also holds a trampoline, `B _name`, for every symbol the app
//! region may import (`keep`): a host-defined symbol is reached directly at
//! its address, and a symbol from a dylib through its trampoline, which
//! LLD binds through a stub; the trampolines are also what keeps the
//! symbols' archive members in the link. A kept symbol nothing defines is
//! left to dynamic lookup (`-U`), so the host links, and the side table
//! lists it as absent: an app importing it gets the missing-host-symbols
//! error.
//!
//! The record puts the host first, so an output file on the same APFS
//! volume as the cache can begin as a clone of it.
const std = @import("std");
const host_prelink = @import("../host_prelink.zig");
const Allocator = std.mem.Allocator;
const macho = std.macho;

pub const ProvideKind = host_prelink.ProvideKind;
pub const Provide = host_prelink.Provide;
/// A procedure window: ADRP, ADD, BR and a trap.
pub const procedure_window = host_prelink.procedure_window;
const window_align = host_prelink.window_align;
/// A trampoline: one `B`.
pub const trampoline_size: u64 = 4;
/// The segment page of an arm64 macOS executable.
pub const page_size: u64 = 0x4000;
/// The load commands a patch link adds: the text segment with one section,
/// the data segment with two, the debug segment with three.
pub const app_load_commands: u32 = (72 + 80) + (72 + 2 * 80) + (72 + 3 * 80);
/// Room reserved in the host's load commands for them.
pub const headerpad: u32 = 0x1000;

const prov_section = "__roc_prov";
const tramp_section = "__roc_tramp";

/// The generated object of a host link. `provides` and `keep` must be sorted
/// and distinct.
pub fn linkObject(allocator: Allocator, provides: []const Provide, keep: []const []const u8) Allocator.Error![]u8 {
    var procedures: u32 = 0;
    var data_size: u64 = 0;
    for (provides) |provide| switch (provide.kind) {
        .procedure => procedures += 1,
        .data => data_size += provide.windowSize(),
    };
    const text_prov_size: u64 = procedures * procedure_window;
    const tramp_size: u64 = keep.len * trampoline_size;
    const SectionSpec = struct { segname: []const u8, sectname: []const u8, size: u64, flags: u32, @"align": u32 };
    var specs: [3]SectionSpec = undefined;
    var section_count: usize = 0;
    // Section ordinals the symbols refer to, 1-based; 0 when absent.
    var text_prov_ordinal: u8 = 0;
    var tramp_ordinal: u8 = 0;
    var data_prov_ordinal: u8 = 0;
    const code_flags: u32 = macho.S_ATTR_PURE_INSTRUCTIONS | macho.S_ATTR_SOME_INSTRUCTIONS | macho.S_ATTR_NO_DEAD_STRIP;
    if (text_prov_size != 0) {
        specs[section_count] = .{ .segname = "__TEXT", .sectname = prov_section, .size = text_prov_size, .flags = code_flags, .@"align" = 4 };
        section_count += 1;
        text_prov_ordinal = @intCast(section_count);
    }
    if (tramp_size != 0) {
        specs[section_count] = .{ .segname = "__TEXT", .sectname = tramp_section, .size = tramp_size, .flags = code_flags, .@"align" = 2 };
        section_count += 1;
        tramp_ordinal = @intCast(section_count);
    }
    if (data_size != 0) {
        specs[section_count] = .{ .segname = "__DATA", .sectname = prov_section, .size = data_size, .flags = macho.S_ATTR_NO_DEAD_STRIP, .@"align" = 4 };
        section_count += 1;
        data_prov_ordinal = @intCast(section_count);
    }
    const sections = specs[0..section_count];

    const header_size: usize = 32;
    const segment_size: usize = 72 + sections.len * 80;
    const symtab_size: usize = 24;
    const dysymtab_size: usize = 80;
    const build_version_size: usize = 24;
    const commands_size = segment_size + symtab_size + dysymtab_size + build_version_size;
    const contents_offset = header_size + commands_size;
    // Sections contiguous in one address space, each 16-aligned.
    var addrs: [3]u64 = undefined;
    var total: u64 = 0;
    for (sections, 0..) |section, index| {
        total = std.mem.alignForward(u64, total, window_align);
        addrs[index] = total;
        total += section.size;
    }
    const contents_size: usize = @intCast(total);
    const reloc_offset = contents_offset + contents_size;
    const reloc_size = keep.len * 8;
    const symtab_offset = std.mem.alignForward(usize, reloc_offset + reloc_size, 8);
    const symbol_count = provides.len + keep.len;
    const strtab_offset = symtab_offset + symbol_count * 16;

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendNTimes(allocator, 0, strtab_offset);
    const bytes = out.items;
    std.mem.writeInt(u32, bytes[0..4], macho.MH_MAGIC_64, .little);
    std.mem.writeInt(u32, bytes[4..8], macho.CPU_TYPE_ARM64, .little);
    std.mem.writeInt(u32, bytes[8..12], 0, .little);
    std.mem.writeInt(u32, bytes[12..16], macho.MH_OBJECT, .little);
    std.mem.writeInt(u32, bytes[16..20], 4, .little);
    std.mem.writeInt(u32, bytes[20..24], @intCast(commands_size), .little);

    var at: usize = header_size;
    std.mem.writeInt(u32, bytes[at..][0..4], @backingInt(macho.LC.SEGMENT_64), .little);
    std.mem.writeInt(u32, bytes[at + 4 ..][0..4], @intCast(segment_size), .little);
    std.mem.writeInt(u64, bytes[at + 24 ..][0..8], 0, .little);
    std.mem.writeInt(u64, bytes[at + 32 ..][0..8], total, .little);
    std.mem.writeInt(u64, bytes[at + 40 ..][0..8], contents_offset, .little);
    std.mem.writeInt(u64, bytes[at + 48 ..][0..8], contents_size, .little);
    std.mem.writeInt(u32, bytes[at + 56 ..][0..4], 7, .little);
    std.mem.writeInt(u32, bytes[at + 60 ..][0..4], 7, .little);
    std.mem.writeInt(u32, bytes[at + 64 ..][0..4], @intCast(sections.len), .little);
    at += 72;
    for (sections, 0..) |section, index| {
        const entry = bytes[at..][0..80];
        @memset(entry, 0);
        @memcpy(entry[0..section.sectname.len], section.sectname);
        @memcpy(entry[16..][0..section.segname.len], section.segname);
        std.mem.writeInt(u64, entry[32..40], addrs[index], .little);
        std.mem.writeInt(u64, entry[40..48], section.size, .little);
        std.mem.writeInt(u32, entry[48..52], @intCast(contents_offset + addrs[index]), .little);
        std.mem.writeInt(u32, entry[52..56], section.@"align", .little);
        const is_tramp = index + 1 == tramp_ordinal;
        std.mem.writeInt(u32, entry[56..60], if (is_tramp) @intCast(reloc_offset) else 0, .little);
        std.mem.writeInt(u32, entry[60..64], if (is_tramp) @intCast(keep.len) else 0, .little);
        std.mem.writeInt(u32, entry[64..68], section.flags, .little);
        at += 80;
    }
    std.mem.writeInt(u32, bytes[at..][0..4], @backingInt(macho.LC.SYMTAB), .little);
    std.mem.writeInt(u32, bytes[at + 4 ..][0..4], symtab_size, .little);
    std.mem.writeInt(u32, bytes[at + 8 ..][0..4], @intCast(symtab_offset), .little);
    std.mem.writeInt(u32, bytes[at + 12 ..][0..4], @intCast(symbol_count), .little);
    std.mem.writeInt(u32, bytes[at + 16 ..][0..4], @intCast(strtab_offset), .little);
    // strsize is written once the string table is built.
    const strsize_at = at + 20;
    at += symtab_size;
    std.mem.writeInt(u32, bytes[at..][0..4], @backingInt(macho.LC.DYSYMTAB), .little);
    std.mem.writeInt(u32, bytes[at + 4 ..][0..4], dysymtab_size, .little);
    // ilocalsym 0, nlocalsym 0; iextdefsym 0, nextdefsym; iundefsym, nundefsym.
    std.mem.writeInt(u32, bytes[at + 20 ..][0..4], @intCast(provides.len), .little);
    std.mem.writeInt(u32, bytes[at + 24 ..][0..4], @intCast(provides.len), .little);
    std.mem.writeInt(u32, bytes[at + 28 ..][0..4], @intCast(keep.len), .little);
    at += dysymtab_size;
    std.mem.writeInt(u32, bytes[at..][0..4], @backingInt(macho.LC.BUILD_VERSION), .little);
    std.mem.writeInt(u32, bytes[at + 4 ..][0..4], build_version_size, .little);
    std.mem.writeInt(u32, bytes[at + 8 ..][0..4], macho_platform_macos, .little);
    std.mem.writeInt(u32, bytes[at + 12 ..][0..4], macos_11, .little);
    std.mem.writeInt(u32, bytes[at + 16 ..][0..4], macos_11, .little);
    at += build_version_size;
    std.debug.assert(at == contents_offset);

    // Windows trap until a build fills them; trampolines branch.
    if (text_prov_ordinal != 0) {
        const start = contents_offset + addrs[text_prov_ordinal - 1];
        var word: usize = 0;
        while (word < text_prov_size) : (word += 4) std.mem.writeInt(u32, bytes[start + word ..][0..4], brk_instruction, .little);
    }
    if (tramp_ordinal != 0) {
        const start = contents_offset + addrs[tramp_ordinal - 1];
        for (0..keep.len) |index| {
            std.mem.writeInt(u32, bytes[start + index * 4 ..][0..4], b_instruction, .little);
            const reloc = bytes[reloc_offset + index * 8 ..][0..8];
            std.mem.writeInt(u32, reloc[0..4], @intCast(index * 4), .little);
            const symbolnum: u32 = @intCast(provides.len + index);
            const info: u32 = symbolnum | (1 << 24) | (2 << 25) | (1 << 27) | (@as(u32, arm64_reloc_branch26) << 28);
            std.mem.writeInt(u32, reloc[4..8], info, .little);
        }
    }

    // Symbols: the provides as defined externals, then the kept names as
    // undefined ones; names with the Mach-O underscore.
    var strtab: std.ArrayList(u8) = .empty;
    defer strtab.deinit(allocator);
    try strtab.appendSlice(allocator, " \x00");
    var procedure_index: u64 = 0;
    var data_offset: u64 = 0;
    for (provides, 0..) |provide, index| {
        const entry = bytes[symtab_offset + index * 16 ..][0..16];
        std.mem.writeInt(u32, entry[0..4], @intCast(strtab.items.len), .little);
        try strtab.append(allocator, '_');
        try strtab.appendSlice(allocator, provide.name);
        try strtab.append(allocator, 0);
        entry[4] = macho.N_SECT | macho.N_EXT;
        switch (provide.kind) {
            .procedure => {
                entry[5] = text_prov_ordinal;
                std.mem.writeInt(u64, entry[8..16], addrs[text_prov_ordinal - 1] + procedure_index * procedure_window, .little);
                procedure_index += 1;
            },
            .data => {
                entry[5] = data_prov_ordinal;
                std.mem.writeInt(u64, entry[8..16], addrs[data_prov_ordinal - 1] + data_offset, .little);
                data_offset += provide.windowSize();
            },
        }
    }
    for (keep, 0..) |name, index| {
        const entry = bytes[symtab_offset + (provides.len + index) * 16 ..][0..16];
        std.mem.writeInt(u32, entry[0..4], @intCast(strtab.items.len), .little);
        try strtab.append(allocator, '_');
        try strtab.appendSlice(allocator, name);
        try strtab.append(allocator, 0);
        entry[4] = macho.N_UNDF | macho.N_EXT;
    }
    std.mem.writeInt(u32, bytes[strsize_at..][0..4], @intCast(strtab.items.len), .little);
    try out.appendSlice(allocator, strtab.items);
    return try out.toOwnedSlice(allocator);
}

const brk_instruction: u32 = 0xd420_0000;
const b_instruction: u32 = 0x1400_0000;
const arm64_reloc_branch26: u32 = 2;
const macho_platform_macos: u32 = 1;
const macos_11: u32 = 0x000b_0000;

/// The record format; bumped whenever `finish`'s output changes.
pub const format_version: u32 = 1;
const magic = "RHSM".*;

/// One symbol the app region may import, as the host provides it.
pub const HostSymbol = extern struct {
    name: u32,
    /// The library ordinal of a symbol the host imports from a dylib
    /// (`n_desc` of its undefined entry); 0 for one the host defines.
    ordinal: u16,
    /// `symbol_weak_import` when the host imports it weakly.
    flags: u16,
    /// The symbol's address for a definition, else its trampoline's.
    vaddr: u64,
};

/// `HostSymbol.flags` bit: the host imports the symbol weakly.
pub const symbol_weak_import: u16 = 1;

/// One `provides` symbol's window in the host.
pub const ProvideRecord = extern struct {
    name: u32,
    kind: u8,
    reserved: [3]u8 = .{ 0, 0, 0 },
    vaddr: u64,
    /// The window's offset in the host's bytes.
    offset: u64,
    /// The window's bytes.
    size: u64,
};

/// The record's trailer: the last bytes of the record.
const Trailer = extern struct {
    magic: [4]u8,
    version: u32,
    /// The host's bytes: the file up to its code signature.
    host_size: u64,
    /// Where `__LINKEDIT` starts in the host, which is where its mapped
    /// segments end.
    linkedit_offset: u64,
    linkedit_vmaddr: u64,
    /// The end of the last mapped segment before `__LINKEDIT`.
    host_end_vaddr: u64,
    /// `__TEXT`, the executable segment of the code signature.
    text_offset: u64,
    text_size: u64,
    /// Every segment in the host, `__PAGEZERO` and `__LINKEDIT` included.
    segment_count: u32,
    symbol_count: u32,
    provide_count: u32,
    /// The host's page hashes (SHA-256 per `page_size` of the mapped
    /// segments), after the symbols, provides and strings.
    page_hash_count: u32,
    symbols_offset: u64,
    provides_offset: u64,
    strings_offset: u64,
    strings_size: u64,
    page_hashes_offset: u64,
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

/// Make the host executable LLD linked from `linkObject(provides, keep)` and
/// the platform's inputs into a host record.
pub fn finish(allocator: Allocator, exe: []const u8, provides: []const Provide, keep: []const []const u8) FinishError![]align(16) u8 {
    var parsed: Executable.Parsed = undefined;
    const image = parsed.parse(exe) catch return error.UnexpectedHostLayout;
    const signature = image.code_signature orelse return error.UnexpectedHostLayout;
    if (image.first_content < image.commands_end + app_load_commands) return error.UnexpectedHostLayout;
    if (@as(u64, signature.dataoff) + signature.datasize != exe.len) return error.UnexpectedHostLayout;
    if (image.chained_fixups or image.dyld_info == null or image.symtab == null or image.dysymtab == null) return error.UnexpectedHostLayout;
    const host_size: u64 = signature.dataoff;
    const linkedit = image.segments[image.linkedit];
    if (linkedit.fileoff % page_size != 0 or linkedit.fileoff > host_size) return error.UnexpectedHostLayout;
    // Every __LINKEDIT piece lies before the signature.
    if (image.linkeditEnd() > host_size) return error.UnexpectedHostLayout;
    const text = image.segments[image.text];

    // The windows and the trampolines, where LLD placed the sections.
    var text_prov: ?Executable.Section = null;
    var data_prov: ?Executable.Section = null;
    var tramp: ?Executable.Section = null;
    for (image.sections) |section| {
        if (std.mem.eql(u8, section.sectname, prov_section)) {
            if (std.mem.eql(u8, section.segname, "__TEXT")) text_prov = section;
            if (std.mem.eql(u8, section.segname, "__DATA")) data_prov = section;
        } else if (std.mem.eql(u8, section.sectname, tramp_section) and std.mem.eql(u8, section.segname, "__TEXT")) {
            tramp = section;
        }
    }
    var strings: std.ArrayList(u8) = .empty;
    defer strings.deinit(allocator);
    try strings.append(allocator, 0);
    const provide_records = try allocator.alloc(ProvideRecord, provides.len);
    defer allocator.free(provide_records);
    var procedure_at: u64 = 0;
    var data_at: u64 = 0;
    for (provides, provide_records) |provide, *record| {
        const section = switch (provide.kind) {
            .procedure => text_prov orelse return error.UnexpectedHostLayout,
            .data => data_prov orelse return error.UnexpectedHostLayout,
        };
        const within = switch (provide.kind) {
            .procedure => procedure_at,
            .data => data_at,
        };
        const size = provide.windowSize();
        if (within + size > section.size or section.offset == 0) return error.UnexpectedHostLayout;
        record.* = .{
            .name = @intCast(strings.items.len),
            .kind = @backingInt(provide.kind),
            .vaddr = section.addr + within,
            .offset = section.offset + within,
            .size = size,
        };
        try strings.appendSlice(allocator, provide.name);
        try strings.append(allocator, 0);
        switch (provide.kind) {
            .procedure => procedure_at += size,
            .data => data_at += size,
        }
    }
    if (keep.len != 0) {
        const section = tramp orelse return error.UnexpectedHostLayout;
        if (section.size != keep.len * trampoline_size) return error.UnexpectedHostLayout;
    }

    // Each kept symbol: the host's definition, or its trampoline when a
    // dylib defines it; one left to dynamic lookup is absent.
    const symtab = image.symtab.?;
    const symbol_names = exe[symtab.stroff..][0..symtab.strsize];
    var symbols: std.ArrayList(HostSymbol) = .empty;
    defer symbols.deinit(allocator);
    const found = try allocator.alloc(?HostSymbol, keep.len);
    defer allocator.free(found);
    @memset(found, null);
    for (0..symtab.nsyms) |index| {
        const entry = exe[symtab.symoff + index * 16 ..][0..16];
        const n_strx = std.mem.readInt(u32, entry[0..4], .little);
        const n_type = entry[4];
        const n_desc = std.mem.readInt(u16, entry[6..8], .little);
        const n_value = std.mem.readInt(u64, entry[8..16], .little);
        if (n_type & macho.N_STAB != 0) continue;
        if (n_strx >= symbol_names.len) return error.UnexpectedHostLayout;
        const full_name = std.mem.sliceTo(symbol_names[n_strx..], 0);
        if (full_name.len == 0 or full_name[0] != '_') continue;
        const keep_index = findSorted(keep, full_name[1..]) orelse continue;
        const n_kind = n_type & macho.N_TYPE;
        if (n_kind == macho.N_SECT) {
            found[keep_index] = .{ .name = 0, .ordinal = 0, .flags = 0, .vaddr = n_value };
        } else if (n_kind == macho.N_UNDF) {
            const ordinal: u16 = (n_desc >> 8) & 0xff;
            // Self, dynamic lookup and executable ordinals name no dylib.
            if (ordinal == 0 or ordinal >= 0xfe) continue;
            const section = tramp orelse return error.UnexpectedHostLayout;
            found[keep_index] = .{
                .name = 0,
                .ordinal = ordinal,
                .flags = if (n_desc & macho.N_WEAK_REF != 0) symbol_weak_import else 0,
                .vaddr = section.addr + keep_index * trampoline_size,
            };
        }
    }
    for (keep, found) |name, entry_or_null| {
        var entry = entry_or_null orelse continue;
        entry.name = @intCast(strings.items.len);
        try strings.appendSlice(allocator, name);
        try strings.append(allocator, 0);
        try symbols.append(allocator, entry);
    }

    // The mapped segments' page hashes, for the signature.
    const page_count: u64 = linkedit.fileoff / page_size;
    var host_end: u64 = 0;
    for (image.segments, 0..) |segment, index| {
        if (index == image.linkedit) continue;
        host_end = @max(host_end, segment.vmaddr + segment.vmsize);
    }
    if (host_end > linkedit.vmaddr) return error.UnexpectedHostLayout;

    var end: u64 = std.mem.alignForward(u64, host_size, 16);
    const symbols_offset = end;
    end += symbols.items.len * @sizeOf(HostSymbol);
    const provides_offset = end;
    end += provide_records.len * @sizeOf(ProvideRecord);
    const strings_offset = end;
    end += strings.items.len;
    end = std.mem.alignForward(u64, end, 16);
    const page_hashes_offset = end;
    end += page_count * 32;
    end = std.mem.alignForward(u64, end, 16);
    const trailer_offset = end;
    end += @sizeOf(Trailer);
    const total = std.mem.alignForward(u64, end, 16);
    if (total > std.math.maxInt(u32)) return error.HostTooLarge;

    const out = try allocator.alignedAlloc(u8, .@"16", @intCast(total));
    @memset(out, 0);
    @memcpy(out[0..@intCast(host_size)], exe[0..@intCast(host_size)]);
    @memcpy(out[@intCast(symbols_offset)..][0 .. symbols.items.len * @sizeOf(HostSymbol)], std.mem.sliceAsBytes(symbols.items));
    @memcpy(out[@intCast(provides_offset)..][0 .. provide_records.len * @sizeOf(ProvideRecord)], std.mem.sliceAsBytes(provide_records));
    @memcpy(out[@intCast(strings_offset)..][0..strings.items.len], strings.items);
    for (0..page_count) |page| {
        std.crypto.hash.sha2.Sha256.hash(exe[page * page_size ..][0..page_size], out[@intCast(page_hashes_offset + page * 32)..][0..32], .{});
    }
    var host_digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(exe[0..@intCast(host_size)], &host_digest, .{});
    const trailer: Trailer = .{
        .magic = magic,
        .version = format_version,
        .host_size = host_size,
        .linkedit_offset = linkedit.fileoff,
        .linkedit_vmaddr = linkedit.vmaddr,
        .host_end_vaddr = host_end,
        .text_offset = text.fileoff,
        .text_size = text.filesize,
        .segment_count = @intCast(image.segments.len),
        .symbol_count = @intCast(symbols.items.len),
        .provide_count = @intCast(provide_records.len),
        .page_hash_count = @intCast(page_count),
        .symbols_offset = symbols_offset,
        .provides_offset = provides_offset,
        .strings_offset = strings_offset,
        .strings_size = strings.items.len,
        .page_hashes_offset = page_hashes_offset,
        .total_size = total,
        .host_digest = host_digest,
    };
    @memcpy(out[@intCast(trailer_offset)..][0..@sizeOf(Trailer)], std.mem.asBytes(&trailer));
    return out;
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
        if (trailer.host_size > trailer_offset or trailer.linkedit_offset > trailer.host_size) return error.InvalidRecord;
        if (trailer.linkedit_offset % page_size != 0 or trailer.page_hash_count != trailer.linkedit_offset / page_size) return error.InvalidRecord;
        if (trailer.symbols_offset % 8 != 0 or trailer.provides_offset % 8 != 0 or trailer.page_hashes_offset % 16 != 0) return error.InvalidRecord;
        if (trailer.symbols_offset + @as(u64, trailer.symbol_count) * @sizeOf(HostSymbol) > trailer_offset) return error.InvalidRecord;
        if (trailer.provides_offset + @as(u64, trailer.provide_count) * @sizeOf(ProvideRecord) > trailer_offset) return error.InvalidRecord;
        if (trailer.strings_offset + trailer.strings_size > trailer_offset or trailer.strings_size == 0) return error.InvalidRecord;
        if (bytes[@intCast(trailer.strings_offset + trailer.strings_size - 1)] != 0) return error.InvalidRecord;
        if (trailer.page_hashes_offset + @as(u64, trailer.page_hash_count) * 32 > trailer_offset) return error.InvalidRecord;
        if (trailer.text_offset + trailer.text_size > trailer.linkedit_offset) return error.InvalidRecord;
        return .{ .bytes = bytes, .trailer = trailer };
    }

    /// The host executable's bytes up to its code signature.
    pub fn host(self: View) []const u8 {
        return self.bytes[0..@intCast(self.trailer.host_size)];
    }
    /// The host's mapped segments: its bytes before `__LINKEDIT`.
    pub fn segments(self: View) []const u8 {
        return self.bytes[0..@intCast(self.trailer.linkedit_offset)];
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
    /// The SHA-256 of each `page_size` page of `segments()`.
    pub fn pageHashes(self: View) []const [32]u8 {
        const start: [*]const [32]u8 = @ptrCast(self.bytes.ptr + self.trailer.page_hashes_offset);
        return start[0..self.trailer.page_hash_count];
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

/// The load commands of a Mach-O arm64 executable the patch link rewrites.
pub const Executable = struct {
    header: Header,
    segments: []const Segment,
    sections: []const Section,
    /// Indices into `segments`.
    text: usize,
    linkedit: usize,
    symtab: ?Symtab,
    dysymtab: ?Dysymtab,
    dyld_info: ?DyldInfo,
    function_starts: ?LinkeditData,
    data_in_code: ?LinkeditData,
    code_signature: ?LinkeditData,
    chained_fixups: bool,
    /// The end of the load commands.
    commands_end: u32,
    /// The lowest file offset of any section's contents: where the load
    /// commands may grow to.
    first_content: u32,

    pub const Header = struct { ncmds: u32, sizeofcmds: u32, flags: u32 };
    pub const Segment = struct {
        name: []const u8,
        /// The load command's offset in the file.
        command_offset: u32,
        vmaddr: u64,
        vmsize: u64,
        fileoff: u64,
        filesize: u64,
        maxprot: u32,
        initprot: u32,
        nsects: u32,
    };
    pub const Section = struct {
        sectname: []const u8,
        segname: []const u8,
        addr: u64,
        size: u64,
        offset: u32,
        /// The index of the section's segment.
        segment: usize,
    };
    pub const Symtab = struct { command_offset: u32, symoff: u32, nsyms: u32, stroff: u32, strsize: u32 };
    pub const Dysymtab = struct {
        command_offset: u32,
        ilocalsym: u32,
        nlocalsym: u32,
        iextdefsym: u32,
        nextdefsym: u32,
        iundefsym: u32,
        nundefsym: u32,
        indirectsymoff: u32,
        nindirectsyms: u32,
    };
    pub const DyldInfo = struct {
        command_offset: u32,
        rebase_off: u32,
        rebase_size: u32,
        bind_off: u32,
        bind_size: u32,
        weak_bind_off: u32,
        weak_bind_size: u32,
        lazy_bind_off: u32,
        lazy_bind_size: u32,
        export_off: u32,
        export_size: u32,
    };
    pub const LinkeditData = struct { command_offset: u32, dataoff: u32, datasize: u32 };

    pub const ParseError = error{InvalidExecutable};

    const max_segments = 16;
    const max_sections = 64;

    /// Storage for a parse's lists, which outlive the call only inside the
    /// returned `Executable` when the caller keeps it beside the struct.
    pub const Parsed = struct {
        segments: [max_segments]Segment,
        sections: [max_sections]Section,

        pub fn parse(self: *Parsed, bytes: []const u8) ParseError!Executable {
            if (bytes.len < 32 or std.mem.readInt(u32, bytes[0..4], .little) != macho.MH_MAGIC_64) return error.InvalidExecutable;
            if (std.mem.readInt(u32, bytes[4..8], .little) != macho.CPU_TYPE_ARM64 or std.mem.readInt(u32, bytes[12..16], .little) != macho.MH_EXECUTE) return error.InvalidExecutable;
            const ncmds = std.mem.readInt(u32, bytes[16..20], .little);
            const sizeofcmds = std.mem.readInt(u32, bytes[20..24], .little);
            if (32 + @as(u64, sizeofcmds) > bytes.len) return error.InvalidExecutable;
            var exe: Executable = .{
                .header = .{ .ncmds = ncmds, .sizeofcmds = sizeofcmds, .flags = std.mem.readInt(u32, bytes[24..28], .little) },
                .segments = &.{},
                .sections = &.{},
                .text = 0,
                .linkedit = 0,
                .symtab = null,
                .dysymtab = null,
                .dyld_info = null,
                .function_starts = null,
                .data_in_code = null,
                .code_signature = null,
                .chained_fixups = false,
                .commands_end = 32 + sizeofcmds,
                .first_content = std.math.maxInt(u32),
            };
            var segment_count: usize = 0;
            var section_count: usize = 0;
            var text: ?usize = null;
            var linkedit: ?usize = null;
            var at: u32 = 32;
            for (0..ncmds) |_| {
                if (at + 8 > 32 + sizeofcmds) return error.InvalidExecutable;
                const cmd = std.mem.readInt(u32, bytes[at..][0..4], .little);
                const cmdsize = std.mem.readInt(u32, bytes[at + 4 ..][0..4], .little);
                if (cmdsize < 8 or at + cmdsize > 32 + sizeofcmds) return error.InvalidExecutable;
                if (cmd == @backingInt(macho.LC.SEGMENT_64)) {
                    if (cmdsize < 72 or segment_count == max_segments) return error.InvalidExecutable;
                    const nsects = std.mem.readInt(u32, bytes[at + 64 ..][0..4], .little);
                    if (cmdsize < 72 + nsects * 80 or section_count + nsects > max_sections) return error.InvalidExecutable;
                    const name = std.mem.sliceTo(bytes[at + 8 ..][0..16], 0);
                    self.segments[segment_count] = .{
                        .name = name,
                        .command_offset = at,
                        .vmaddr = std.mem.readInt(u64, bytes[at + 24 ..][0..8], .little),
                        .vmsize = std.mem.readInt(u64, bytes[at + 32 ..][0..8], .little),
                        .fileoff = std.mem.readInt(u64, bytes[at + 40 ..][0..8], .little),
                        .filesize = std.mem.readInt(u64, bytes[at + 48 ..][0..8], .little),
                        .maxprot = std.mem.readInt(u32, bytes[at + 56 ..][0..4], .little),
                        .initprot = std.mem.readInt(u32, bytes[at + 60 ..][0..4], .little),
                        .nsects = nsects,
                    };
                    if (std.mem.eql(u8, name, "__TEXT")) text = segment_count;
                    if (std.mem.eql(u8, name, "__LINKEDIT")) linkedit = segment_count;
                    for (0..nsects) |index| {
                        const header = at + 72 + @as(u32, @intCast(index)) * 80;
                        const section: Section = .{
                            .sectname = std.mem.sliceTo(bytes[header..][0..16], 0),
                            .segname = std.mem.sliceTo(bytes[header + 16 ..][0..16], 0),
                            .addr = std.mem.readInt(u64, bytes[header + 32 ..][0..8], .little),
                            .size = std.mem.readInt(u64, bytes[header + 40 ..][0..8], .little),
                            .offset = std.mem.readInt(u32, bytes[header + 48 ..][0..4], .little),
                            .segment = segment_count,
                        };
                        if (section.offset != 0 and section.size != 0) exe.first_content = @min(exe.first_content, section.offset);
                        self.sections[section_count] = section;
                        section_count += 1;
                    }
                    segment_count += 1;
                } else if (cmd == @backingInt(macho.LC.SYMTAB)) {
                    if (cmdsize < 24) return error.InvalidExecutable;
                    exe.symtab = .{
                        .command_offset = at,
                        .symoff = std.mem.readInt(u32, bytes[at + 8 ..][0..4], .little),
                        .nsyms = std.mem.readInt(u32, bytes[at + 12 ..][0..4], .little),
                        .stroff = std.mem.readInt(u32, bytes[at + 16 ..][0..4], .little),
                        .strsize = std.mem.readInt(u32, bytes[at + 20 ..][0..4], .little),
                    };
                } else if (cmd == @backingInt(macho.LC.DYSYMTAB)) {
                    if (cmdsize < 80) return error.InvalidExecutable;
                    exe.dysymtab = .{
                        .command_offset = at,
                        .ilocalsym = std.mem.readInt(u32, bytes[at + 8 ..][0..4], .little),
                        .nlocalsym = std.mem.readInt(u32, bytes[at + 12 ..][0..4], .little),
                        .iextdefsym = std.mem.readInt(u32, bytes[at + 16 ..][0..4], .little),
                        .nextdefsym = std.mem.readInt(u32, bytes[at + 20 ..][0..4], .little),
                        .iundefsym = std.mem.readInt(u32, bytes[at + 24 ..][0..4], .little),
                        .nundefsym = std.mem.readInt(u32, bytes[at + 28 ..][0..4], .little),
                        .indirectsymoff = std.mem.readInt(u32, bytes[at + 56 ..][0..4], .little),
                        .nindirectsyms = std.mem.readInt(u32, bytes[at + 60 ..][0..4], .little),
                    };
                    // Tables of contents, module tables and external
                    // references belong to dylibs.
                    for ([_]u32{ 32, 36, 40, 44, 48, 52, 64, 68, 72, 76 }) |field| {
                        if (std.mem.readInt(u32, bytes[at + field ..][0..4], .little) != 0) return error.InvalidExecutable;
                    }
                } else if (cmd == @backingInt(macho.LC.DYLD_INFO) or cmd == @backingInt(macho.LC.DYLD_INFO_ONLY)) {
                    if (cmdsize < 48) return error.InvalidExecutable;
                    exe.dyld_info = .{
                        .command_offset = at,
                        .rebase_off = std.mem.readInt(u32, bytes[at + 8 ..][0..4], .little),
                        .rebase_size = std.mem.readInt(u32, bytes[at + 12 ..][0..4], .little),
                        .bind_off = std.mem.readInt(u32, bytes[at + 16 ..][0..4], .little),
                        .bind_size = std.mem.readInt(u32, bytes[at + 20 ..][0..4], .little),
                        .weak_bind_off = std.mem.readInt(u32, bytes[at + 24 ..][0..4], .little),
                        .weak_bind_size = std.mem.readInt(u32, bytes[at + 28 ..][0..4], .little),
                        .lazy_bind_off = std.mem.readInt(u32, bytes[at + 32 ..][0..4], .little),
                        .lazy_bind_size = std.mem.readInt(u32, bytes[at + 36 ..][0..4], .little),
                        .export_off = std.mem.readInt(u32, bytes[at + 40 ..][0..4], .little),
                        .export_size = std.mem.readInt(u32, bytes[at + 44 ..][0..4], .little),
                    };
                } else if (cmd == @backingInt(macho.LC.FUNCTION_STARTS) or cmd == @backingInt(macho.LC.DATA_IN_CODE) or cmd == @backingInt(macho.LC.CODE_SIGNATURE)) {
                    if (cmdsize < 16) return error.InvalidExecutable;
                    const data: LinkeditData = .{
                        .command_offset = at,
                        .dataoff = std.mem.readInt(u32, bytes[at + 8 ..][0..4], .little),
                        .datasize = std.mem.readInt(u32, bytes[at + 12 ..][0..4], .little),
                    };
                    if (cmd == @backingInt(macho.LC.FUNCTION_STARTS)) {
                        exe.function_starts = data;
                    } else if (cmd == @backingInt(macho.LC.DATA_IN_CODE)) {
                        exe.data_in_code = data;
                    } else {
                        exe.code_signature = data;
                    }
                } else if (cmd == @backingInt(macho.LC.DYLD_CHAINED_FIXUPS) or cmd == @backingInt(macho.LC.DYLD_EXPORTS_TRIE)) {
                    exe.chained_fixups = true;
                }
                at += cmdsize;
            }
            exe.segments = self.segments[0..segment_count];
            exe.sections = self.sections[0..section_count];
            exe.text = text orelse return error.InvalidExecutable;
            exe.linkedit = linkedit orelse return error.InvalidExecutable;
            // __LINKEDIT is last, in the file and in memory.
            const last = exe.segments[exe.linkedit];
            for (exe.segments, 0..) |segment, index| {
                if (index == exe.linkedit) continue;
                if (segment.fileoff + segment.filesize > last.fileoff or segment.vmaddr + segment.vmsize > last.vmaddr) return error.InvalidExecutable;
            }
            if (exe.first_content < exe.commands_end) return error.InvalidExecutable;
            return exe;
        }
    };

    /// The end of the last `__LINKEDIT` piece other than the signature.
    pub fn linkeditEnd(self: Executable) u64 {
        var end: u64 = 0;
        if (self.symtab) |symtab| {
            end = @max(end, @as(u64, symtab.symoff) + @as(u64, symtab.nsyms) * 16);
            end = @max(end, @as(u64, symtab.stroff) + symtab.strsize);
        }
        if (self.dysymtab) |dysymtab| end = @max(end, @as(u64, dysymtab.indirectsymoff) + @as(u64, dysymtab.nindirectsyms) * 4);
        if (self.dyld_info) |info| {
            end = @max(end, @as(u64, info.rebase_off) + info.rebase_size);
            end = @max(end, @as(u64, info.bind_off) + info.bind_size);
            end = @max(end, @as(u64, info.weak_bind_off) + info.weak_bind_size);
            end = @max(end, @as(u64, info.lazy_bind_off) + info.lazy_bind_size);
            end = @max(end, @as(u64, info.export_off) + info.export_size);
        }
        if (self.function_starts) |data| end = @max(end, @as(u64, data.dataoff) + data.datasize);
        if (self.data_in_code) |data| end = @max(end, @as(u64, data.dataoff) + data.datasize);
        return end;
    }
};

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

test "the host link object defines windows and trampolines" {
    const UnitImage = @import("backend").dev.UnitImage;
    const allocator = std.testing.allocator;
    const object = try linkObject(allocator, &.{
        .{ .name = "roc_config", .kind = .data, .size = 24 },
        .{ .name = "roc_main_for_host", .kind = .procedure, .size = 0 },
    }, &.{ "memcpy", "roc_alloc" });
    defer allocator.free(object);
    // The object is a Mach-O relocatable the image converter reads: the
    // windows are its exports, the trampolines' targets its imports.
    const image = try UnitImage.build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(image);
    const view = try UnitImage.View.init(image);
    try std.testing.expectEqual(@as(usize, 2), view.exports().len);
    const main_window = view.findExport("roc_main_for_host").?;
    try std.testing.expectEqual(@backingInt(UnitImage.SymbolType.func), main_window.type);
    try std.testing.expectEqual(@as(u32, procedure_window), main_window.size);
    const config_window = view.findExport("roc_config").?;
    try std.testing.expectEqual(@backingInt(UnitImage.Region.data), config_window.region);
    try std.testing.expectEqual(@as(u32, 32), config_window.size);
    try std.testing.expectEqual(@as(usize, 2), view.imports().len);
    try std.testing.expectEqualStrings("memcpy", view.name(view.imports()[0].name));
    try std.testing.expectEqualStrings("roc_alloc", view.name(view.imports()[1].name));
    try std.testing.expectEqual(@backingInt(UnitImage.SiteKind.branch26), view.importSites(view.imports()[1])[0].kind);
    try std.testing.expectEqual(@as(u32, 20), view.importSites(view.imports()[1])[0].offset);
    try std.testing.expectEqual(brk_instruction, std.mem.readInt(u32, view.block()[0..4], .little));
    try std.testing.expectEqual(b_instruction, std.mem.readInt(u32, view.block()[16..20], .little));
}

test "a host record round-trips its trailer" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.InvalidRecord, View.init(&[_]u8{}));
    const bytes = try allocator.alignedAlloc(u8, .@"16", 256);
    defer allocator.free(bytes);
    @memset(bytes, 0);
    try std.testing.expectError(error.InvalidRecord, View.init(bytes));
}
