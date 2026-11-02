//! The pre-linked host of a fast dev link (Linux x86_64 executables).
//!
//! A platform's host is linked once, with the existing embedded LLD flags,
//! from the platform's link inputs plus one generated object, and filed in
//! the cache as a host record: the linked executable's bytes, made ready for
//! app code to be appended, and a side table describing what the patch link
//! needs of them (`patch_link.zig`).
//!
//! The generated object (`linkObject`) carries no code. It defines every
//! `provides` symbol as an absolute address in the app region, at a fixed
//! window per symbol (`provideAddress`), so the host's references to the app
//! resolve at prelink time to bytes each build fills in: a jump to the entry
//! wrapper for a procedure, the value's bytes for a data symbol. It also
//! reserves, in the host's first loadable segment, room for a larger copy of
//! the program header table, which the post-processing moves there with
//! spare entries for the app's segments. The table keeps the distance from
//! its file offset to its address that the first segment has, so every
//! kernel computes `AT_PHDR` for it correctly.
//!
//! Nothing in the host link is compiler-owned code: the builtins and the
//! Boxy runtime are part of the app region (design.md "Sealed Roc Object
//! Boundary"). The link keeps (`-u`) exactly the symbols the app region may
//! import: the platform's hosted symbols, the fixed runtime set, and the
//! support image's imports. Its key is a function of contents only, never
//! of a path (`key`).
const std = @import("std");
const build_options = @import("build_options");
const Allocator = std.mem.Allocator;

/// The address the app region starts at. The host must end below it, and
/// app code reaches host code with 32-bit displacements, so the app region
/// stays within 2 GiB of the host.
pub const app_base: u64 = 0x4000_0000;
/// The bytes each `provides` symbol owns at the start of the app region.
pub const provide_window: u64 = 4096;
/// Program header entries the reserved table holds.
pub const phdr_capacity: u32 = 32;
/// Spare program header entries kept for the app's segments; two are used.
pub const spare_phdrs: u32 = 4;

const phdr_size = 56;
const page_size = 4096;

/// The address of the window of the `index`th `provides` symbol, in name
/// order.
pub fn provideAddress(index: usize) u64 {
    return app_base + @as(u64, index) * provide_window;
}

const reserve_section_name = ".roc_phdrs";

/// The generated object of a host link: the reserved program header table
/// and an absolute definition of every name in `provides`, which must be
/// sorted and distinct.
pub fn linkObject(allocator: Allocator, provides: []const []const u8) Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendNTimes(allocator, 0, 64);
    const reserve_offset = out.items.len;
    const reserve_size = phdr_capacity * phdr_size;
    try out.appendNTimes(allocator, 0, reserve_size);

    var strtab: std.ArrayList(u8) = .empty;
    defer strtab.deinit(allocator);
    try strtab.append(allocator, 0);
    while (out.items.len % 8 != 0) try out.append(allocator, 0);
    const symtab_offset = out.items.len;
    try out.appendNTimes(allocator, 0, 24);
    for (provides, 0..) |name, index| {
        var entry: [24]u8 = @splat(0);
        std.mem.writeInt(u32, entry[0..4], @intCast(strtab.items.len), .little);
        try strtab.appendSlice(allocator, name);
        try strtab.append(allocator, 0);
        entry[4] = (STB_GLOBAL << 4) | STT_NOTYPE;
        std.mem.writeInt(u16, entry[6..8], SHN_ABS, .little);
        std.mem.writeInt(u64, entry[8..16], provideAddress(index), .little);
        try out.appendSlice(allocator, &entry);
    }
    const symtab_size = out.items.len - symtab_offset;
    const strtab_offset = out.items.len;
    try out.appendSlice(allocator, strtab.items);
    const shstrtab = "\x00" ++ reserve_section_name ++ "\x00.symtab\x00.strtab\x00.shstrtab\x00";
    const name_reserve = 1;
    const name_symtab = name_reserve + reserve_section_name.len + 1;
    const name_strtab = name_symtab + ".symtab".len + 1;
    const name_shstrtab = name_strtab + ".strtab".len + 1;
    const shstrtab_offset = out.items.len;
    try out.appendSlice(allocator, shstrtab);
    while (out.items.len % 8 != 0) try out.append(allocator, 0);
    const shoff = out.items.len;
    const ObjectHeader = struct { name: u32, type: u32, flags: u64, offset: u64, size: u64, link: u32, info: u32, addralign: u64, entsize: u64 };
    const headers = [_]ObjectHeader{
        std.mem.zeroes(ObjectHeader),
        .{ .name = name_reserve, .type = SHT_PROGBITS, .flags = SHF_ALLOC | SHF_GNU_RETAIN, .offset = reserve_offset, .size = reserve_size, .link = 0, .info = 0, .addralign = 8, .entsize = 0 },
        .{ .name = name_symtab, .type = SHT_SYMTAB, .flags = 0, .offset = symtab_offset, .size = symtab_size, .link = 3, .info = 1, .addralign = 8, .entsize = 24 },
        .{ .name = name_strtab, .type = SHT_STRTAB, .flags = 0, .offset = strtab_offset, .size = strtab.items.len, .link = 0, .info = 0, .addralign = 1, .entsize = 0 },
        .{ .name = name_shstrtab, .type = SHT_STRTAB, .flags = 0, .offset = shstrtab_offset, .size = shstrtab.len, .link = 0, .info = 0, .addralign = 1, .entsize = 0 },
    };
    for (headers) |header| {
        var entry: [64]u8 = undefined;
        std.mem.writeInt(u32, entry[0..4], header.name, .little);
        std.mem.writeInt(u32, entry[4..8], header.type, .little);
        std.mem.writeInt(u64, entry[8..16], header.flags, .little);
        std.mem.writeInt(u64, entry[16..24], 0, .little);
        std.mem.writeInt(u64, entry[24..32], header.offset, .little);
        std.mem.writeInt(u64, entry[32..40], header.size, .little);
        std.mem.writeInt(u32, entry[40..44], header.link, .little);
        std.mem.writeInt(u32, entry[44..48], header.info, .little);
        std.mem.writeInt(u64, entry[48..56], header.addralign, .little);
        std.mem.writeInt(u64, entry[56..64], header.entsize, .little);
        try out.appendSlice(allocator, &entry);
    }
    const ehdr = out.items[0..64];
    @memcpy(ehdr[0..4], "\x7fELF");
    ehdr[4] = 2;
    ehdr[5] = 1;
    ehdr[6] = 1;
    std.mem.writeInt(u16, ehdr[16..18], ET_REL, .little);
    std.mem.writeInt(u16, ehdr[18..20], EM_X86_64, .little);
    std.mem.writeInt(u32, ehdr[20..24], 1, .little);
    std.mem.writeInt(u64, ehdr[40..48], shoff, .little);
    std.mem.writeInt(u16, ehdr[52..54], 64, .little);
    std.mem.writeInt(u16, ehdr[58..60], 64, .little);
    std.mem.writeInt(u16, ehdr[60..62], headers.len, .little);
    std.mem.writeInt(u16, ehdr[62..64], headers.len - 1, .little);
    return try out.toOwnedSlice(allocator);
}

/// Everything a pre-linked host is a function of, as content.
pub const KeyInput = struct {
    target: []const u8,
    /// The link ABI the host links with (`linker.TargetAbi`).
    link_abi: []const u8,
    /// Sorted, distinct.
    provides: []const []const u8,
    /// The symbols kept for the app region; sorted, distinct.
    keep: []const []const u8,
    /// The platform's link items as its header names them, relative to its
    /// target directory, in order, with the app's position.
    link_items: []const []const u8,
    /// For a downloaded platform, its verified bundle hash, which covers
    /// every input file.
    bundle_hash: ?[]const u8,
    /// Otherwise the content digest of every input file, in link order.
    input_digests: []const [32]u8,
};

/// The cache key of a pre-linked host.
pub fn key(input: KeyInput) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-prelinked-host-v1");
    hasher.update(&build_options.compiler_compatibility_hash);
    hashU64(&hasher, format_version);
    hashU64(&hasher, app_base);
    hashU64(&hasher, provide_window);
    hashU64(&hasher, phdr_capacity);
    hashU64(&hasher, spare_phdrs);
    hashBytes(&hasher, input.target);
    hashBytes(&hasher, input.link_abi);
    hashList(&hasher, input.provides);
    hashList(&hasher, input.keep);
    hashList(&hasher, input.link_items);
    if (input.bundle_hash) |bundle| {
        hasher.update(&.{1});
        hashBytes(&hasher, bundle);
    } else {
        hasher.update(&.{0});
        hashU64(&hasher, input.input_digests.len);
        for (input.input_digests) |digest| hasher.update(&digest);
    }
    return hasher.finalResult();
}

fn hashU64(hasher: *std.crypto.hash.sha2.Sha256, value: u64) void {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    hashU64(hasher, bytes.len);
    hasher.update(bytes);
}

fn hashList(hasher: *std.crypto.hash.sha2.Sha256, list: []const []const u8) void {
    hashU64(hasher, list.len);
    for (list) |item| hashBytes(hasher, item);
}

/// The record format; bumped whenever `finish`'s output changes.
pub const format_version: u32 = 1;
const magic = "RHST".*;
/// The offset of the host's bytes in a record, so they can be copied by
/// file range from a page boundary.
pub const host_offset: u32 = page_size;

/// One symbol the host defines that the app region may import.
pub const HostSymbol = extern struct {
    name: u32,
    reserved: u32 = 0,
    vaddr: u64,
};

const Header = extern struct {
    magic: [4]u8,
    version: u32,
    host_offset: u32,
    reserved0: u32 = 0,
    host_size: u64,
    /// The end of the host's highest loadable segment.
    host_end_vaddr: u64,
    /// The program header table in the host: its file offset, its entry
    /// count including the spares, and the index of the first spare.
    phdr_offset: u64,
    phnum: u32,
    first_spare: u32,
    build_id_offset: u64,
    build_id_size: u32,
    shnum: u32,
    shoff: u64,
    shstrndx: u32,
    /// Section indices; 0 when the host has none.
    symtab_index: u32,
    strtab_index: u32,
    debug_line_index: u32,
    debug_info_index: u32,
    debug_abbrev_index: u32,
    symbols_offset: u32,
    symbol_count: u32,
    provides_offset: u32,
    provide_count: u32,
    strings_offset: u32,
    strings_size: u32,
    total_size: u64,
};

/// Why a linked host cannot be made into a record. Each is reported to the
/// user as a failed link of the platform's host.
pub const FinishError = Allocator.Error || error{
    /// The bytes are not the kind of executable this prelink links.
    UnexpectedHostLayout,
    /// The host's segments reach the app region.
    HostTooLarge,
};

/// Make the host executable LLD linked from `linkObject(provides)` and the
/// platform's inputs into a host record. `keep` is the sorted list of
/// symbols the app region may import; the record holds the address of each
/// one the host defines.
pub fn finish(allocator: Allocator, exe: []u8, provides: []const []const u8, keep: []const []const u8) FinishError![]align(16) u8 {
    if (exe.len < 64 or !std.mem.eql(u8, exe[0..4], "\x7fELF") or exe[4] != 2 or exe[5] != 1) return error.UnexpectedHostLayout;
    if (readU16(exe, 16) != ET_EXEC or readU16(exe, 18) != EM_X86_64) return error.UnexpectedHostLayout;
    const phoff = readU64(exe, 32);
    const shoff = readU64(exe, 40);
    const phentsize = readU16(exe, 54);
    const phnum = readU16(exe, 56);
    const shentsize = readU16(exe, 58);
    const shnum = readU16(exe, 60);
    const shstrndx = readU16(exe, 62);
    if (phentsize != phdr_size or shentsize != 64 or shstrndx >= shnum) return error.UnexpectedHostLayout;
    if (phoff + @as(u64, phnum) * phdr_size > exe.len or shoff + @as(u64, shnum) * 64 > exe.len) return error.UnexpectedHostLayout;
    if (phnum + spare_phdrs > phdr_capacity) return error.UnexpectedHostLayout;

    // The first loadable segment maps file offset 0; the reserved table
    // must keep its distance from offset to address.
    var first_delta: ?u64 = null;
    var host_end: u64 = 0;
    for (0..phnum) |index| {
        const at: usize = @intCast(phoff + index * phdr_size);
        if (readU32(exe, at) != PT_LOAD) continue;
        const offset = readU64(exe, at + 8);
        const vaddr = readU64(exe, at + 16);
        const memsz = readU64(exe, at + 40);
        if (offset == 0 and first_delta == null) first_delta = vaddr;
        host_end = @max(host_end, vaddr + memsz);
    }
    const delta = first_delta orelse return error.UnexpectedHostLayout;
    if (host_end > app_base) return error.HostTooLarge;

    const names_header: usize = @intCast(shoff + @as(u64, shstrndx) * 64);
    const names = sliceAt(exe, readU64(exe, names_header + 24), readU64(exe, names_header + 32)) orelse return error.UnexpectedHostLayout;
    var reserve_offset: ?u64 = null;
    var build_id_offset: u64 = 0;
    var build_id_size: u32 = 0;
    var symtab_index: u32 = 0;
    var debug_indices = [_]u32{ 0, 0, 0 };
    for (0..shnum) |index| {
        const at: usize = @intCast(shoff + index * 64);
        const name_offset = readU32(exe, at);
        if (name_offset >= names.len) return error.UnexpectedHostLayout;
        const name = std.mem.sliceTo(names[name_offset..], 0);
        const section_type = readU32(exe, at + 4);
        const addr = readU64(exe, at + 16);
        const offset = readU64(exe, at + 24);
        const size = readU64(exe, at + 32);
        if (std.mem.eql(u8, name, reserve_section_name)) {
            if (size != phdr_capacity * phdr_size or addr != offset + delta) return error.UnexpectedHostLayout;
            reserve_offset = offset;
        } else if (std.mem.eql(u8, name, ".note.gnu.build-id")) {
            const note = sliceAt(exe, offset, size) orelse return error.UnexpectedHostLayout;
            if (note.len < 12) return error.UnexpectedHostLayout;
            const namesz = readU32(note, 0);
            const descsz = readU32(note, 4);
            const desc = 12 + std.mem.alignForward(u64, namesz, 4);
            if (desc + descsz > note.len) return error.UnexpectedHostLayout;
            build_id_offset = offset + desc;
            build_id_size = descsz;
        } else if (section_type == SHT_SYMTAB) {
            symtab_index = @intCast(index);
        } else if (std.mem.eql(u8, name, ".debug_line")) {
            debug_indices[0] = @intCast(index);
        } else if (std.mem.eql(u8, name, ".debug_info")) {
            debug_indices[1] = @intCast(index);
        } else if (std.mem.eql(u8, name, ".debug_abbrev")) {
            debug_indices[2] = @intCast(index);
        }
    }
    const table_offset = reserve_offset orelse return error.UnexpectedHostLayout;
    if (build_id_size == 0) return error.UnexpectedHostLayout;

    // Move the program header table into the reserved space, with spares.
    const new_phnum = phnum + spare_phdrs;
    const table = exe[@intCast(table_offset)..][0 .. new_phnum * phdr_size];
    @memmove(table[0 .. phnum * phdr_size], exe[@intCast(phoff)..][0 .. phnum * phdr_size]);
    @memset(table[phnum * phdr_size ..], 0);
    for (0..phnum) |index| {
        const entry = table[index * phdr_size ..][0..phdr_size];
        if (readU32(entry, 0) != PT_PHDR) continue;
        std.mem.writeInt(u64, entry[8..16], table_offset, .little);
        std.mem.writeInt(u64, entry[16..24], table_offset + delta, .little);
        std.mem.writeInt(u64, entry[24..32], table_offset + delta, .little);
        std.mem.writeInt(u64, entry[32..40], new_phnum * phdr_size, .little);
        std.mem.writeInt(u64, entry[40..48], new_phnum * phdr_size, .little);
    }
    std.mem.writeInt(u64, exe[32..40], table_offset, .little);
    std.mem.writeInt(u16, exe[56..58], @intCast(new_phnum), .little);

    // The address of every kept symbol the host defines: a global or weak
    // definition, or a hidden one LLD bound locally in the executable.
    var strings: std.ArrayList(u8) = .empty;
    defer strings.deinit(allocator);
    try strings.append(allocator, 0);
    var symbols: std.ArrayList(HostSymbol) = .empty;
    defer symbols.deinit(allocator);
    var strtab_index: u32 = 0;
    if (symtab_index != 0) {
        const at: usize = @intCast(shoff + @as(u64, symtab_index) * 64);
        strtab_index = readU32(exe, at + 40);
        if (strtab_index >= shnum) return error.UnexpectedHostLayout;
        const entries = sliceAt(exe, readU64(exe, at + 24), readU64(exe, at + 32)) orelse return error.UnexpectedHostLayout;
        const strtab_at: usize = @intCast(shoff + @as(u64, strtab_index) * 64);
        const symbol_names = sliceAt(exe, readU64(exe, strtab_at + 24), readU64(exe, strtab_at + 32)) orelse return error.UnexpectedHostLayout;
        var found = try allocator.alloc(?u64, keep.len);
        defer allocator.free(found);
        @memset(found, null);
        var entry_at: usize = 0;
        while (entry_at + 24 <= entries.len) : (entry_at += 24) {
            const shndx = readU16(entries, entry_at + 6);
            if (shndx == 0) continue;
            const info = entries[entry_at + 4];
            const bind = info >> 4;
            const visibility = entries[entry_at + 5] & 3;
            const eligible = bind == STB_GLOBAL or bind == STB_WEAK or (bind == STB_LOCAL and visibility == STV_HIDDEN);
            if (!eligible) continue;
            const name_offset = readU32(entries, entry_at);
            if (name_offset >= symbol_names.len) return error.UnexpectedHostLayout;
            const name = std.mem.sliceTo(symbol_names[name_offset..], 0);
            const index = findSorted(keep, name) orelse continue;
            found[index] = readU64(entries, entry_at + 8);
        }
        for (keep, found) |name, vaddr_or_null| {
            const vaddr = vaddr_or_null orelse continue;
            const offset: u32 = @intCast(strings.items.len);
            try strings.appendSlice(allocator, name);
            try strings.append(allocator, 0);
            try symbols.append(allocator, .{ .name = offset, .vaddr = vaddr });
        }
    }
    const provide_names = try allocator.alloc(u32, provides.len);
    defer allocator.free(provide_names);
    for (provides, provide_names) |name, *offset| {
        offset.* = @intCast(strings.items.len);
        try strings.appendSlice(allocator, name);
        try strings.append(allocator, 0);
    }

    var end: u64 = host_offset + exe.len;
    end = std.mem.alignForward(u64, end, 8);
    const symbols_offset = end;
    end += symbols.items.len * @sizeOf(HostSymbol);
    const provides_offset = end;
    end += provide_names.len * @sizeOf(u32);
    const strings_offset = end;
    end += strings.items.len;
    const total = std.mem.alignForward(u64, end, 16);
    if (total > std.math.maxInt(u32)) return error.HostTooLarge;

    const out = try allocator.alignedAlloc(u8, .@"16", @intCast(total));
    @memset(out, 0);
    const header: Header = .{
        .magic = magic,
        .version = format_version,
        .host_offset = host_offset,
        .host_size = exe.len,
        .host_end_vaddr = host_end,
        .phdr_offset = table_offset,
        .phnum = new_phnum,
        .first_spare = phnum,
        .build_id_offset = build_id_offset,
        .build_id_size = build_id_size,
        .shnum = shnum,
        .shoff = shoff,
        .shstrndx = shstrndx,
        .symtab_index = symtab_index,
        .strtab_index = strtab_index,
        .debug_line_index = debug_indices[0],
        .debug_info_index = debug_indices[1],
        .debug_abbrev_index = debug_indices[2],
        .symbols_offset = @intCast(symbols_offset),
        .symbol_count = @intCast(symbols.items.len),
        .provides_offset = @intCast(provides_offset),
        .provide_count = @intCast(provide_names.len),
        .strings_offset = @intCast(strings_offset),
        .strings_size = @intCast(strings.items.len),
        .total_size = total,
    };
    @memcpy(out[0..@sizeOf(Header)], std.mem.asBytes(&header));
    @memcpy(out[host_offset..][0..exe.len], exe);
    @memcpy(out[@intCast(symbols_offset)..][0 .. symbols.items.len * @sizeOf(HostSymbol)], std.mem.sliceAsBytes(symbols.items));
    @memcpy(out[@intCast(provides_offset)..][0 .. provide_names.len * @sizeOf(u32)], std.mem.sliceAsBytes(provide_names));
    @memcpy(out[@intCast(strings_offset)..][0..strings.items.len], strings.items);
    return out;
}

/// Why bytes are not a host record this compiler wrote.
pub const ReadError = error{InvalidRecord};

/// Read access to a host record, borrowing its bytes.
pub const View = struct {
    bytes: []align(16) const u8,
    header: Header,

    pub fn init(bytes: []align(16) const u8) ReadError!View {
        if (bytes.len < @sizeOf(Header)) return error.InvalidRecord;
        const header = std.mem.bytesToValue(Header, bytes[0..@sizeOf(Header)]);
        if (!std.mem.eql(u8, &header.magic, &magic) or header.version != format_version) return error.InvalidRecord;
        if (header.total_size != bytes.len or header.host_offset != host_offset) return error.InvalidRecord;
        if (host_offset + header.host_size > bytes.len) return error.InvalidRecord;
        if (header.symbols_offset % 8 != 0 or header.provides_offset % 4 != 0) return error.InvalidRecord;
        if (@as(u64, header.symbols_offset) + @as(u64, header.symbol_count) * @sizeOf(HostSymbol) > bytes.len) return error.InvalidRecord;
        if (@as(u64, header.provides_offset) + @as(u64, header.provide_count) * 4 > bytes.len) return error.InvalidRecord;
        if (@as(u64, header.strings_offset) + header.strings_size > bytes.len or header.strings_size == 0) return error.InvalidRecord;
        if (bytes[header.strings_offset + header.strings_size - 1] != 0) return error.InvalidRecord;
        if (header.phdr_offset + @as(u64, header.phnum) * phdr_size > header.host_size) return error.InvalidRecord;
        if (header.shoff + @as(u64, header.shnum) * 64 > header.host_size) return error.InvalidRecord;
        if (header.build_id_offset + header.build_id_size > header.host_size) return error.InvalidRecord;
        if (header.host_end_vaddr > app_base) return error.InvalidRecord;
        return .{ .bytes = bytes, .header = header };
    }

    /// The host executable's bytes.
    pub fn host(self: View) []const u8 {
        return self.bytes[host_offset..][0..@intCast(self.header.host_size)];
    }
    pub fn symbols(self: View) []const HostSymbol {
        const start: [*]const HostSymbol = @ptrCast(@alignCast(self.bytes.ptr + self.header.symbols_offset));
        return start[0..self.header.symbol_count];
    }
    pub fn name(self: View, offset: u32) []const u8 {
        return std.mem.sliceTo(self.bytes[self.header.strings_offset + offset ..], 0);
    }
    /// The `provides` symbols in name order; the `index`th owns the window
    /// at `provideAddress(index)`.
    pub fn provideName(self: View, index: usize) []const u8 {
        const offsets: [*]const u32 = @ptrCast(@alignCast(self.bytes.ptr + self.header.provides_offset));
        return self.name(offsets[index]);
    }
    pub fn provideCount(self: View) usize {
        return self.header.provide_count;
    }
    /// The address of the kept symbol `wanted`, when the host defines it.
    pub fn findSymbol(self: View, wanted: []const u8) ?u64 {
        const all = self.symbols();
        var low: usize = 0;
        var high: usize = all.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            switch (std.mem.order(u8, self.name(all[mid].name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return all[mid].vaddr,
            }
        }
        return null;
    }
    /// The host's section header `index`.
    pub fn section(self: View, index: u32) SectionHeader {
        const at: usize = @intCast(self.header.shoff + @as(u64, index) * 64);
        const bytes = self.host();
        return .{
            .name = readU32(bytes, at),
            .type = readU32(bytes, at + 4),
            .flags = readU64(bytes, at + 8),
            .addr = readU64(bytes, at + 16),
            .offset = readU64(bytes, at + 24),
            .size = readU64(bytes, at + 32),
            .link = readU32(bytes, at + 40),
            .info = readU32(bytes, at + 44),
            .addralign = readU64(bytes, at + 48),
            .entsize = readU64(bytes, at + 56),
        };
    }
};

pub const SectionHeader = struct {
    name: u32,
    type: u32,
    flags: u64,
    addr: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addralign: u64,
    entsize: u64,

    pub fn write(self: SectionHeader, out: *[64]u8) void {
        std.mem.writeInt(u32, out[0..4], self.name, .little);
        std.mem.writeInt(u32, out[4..8], self.type, .little);
        std.mem.writeInt(u64, out[8..16], self.flags, .little);
        std.mem.writeInt(u64, out[16..24], self.addr, .little);
        std.mem.writeInt(u64, out[24..32], self.offset, .little);
        std.mem.writeInt(u64, out[32..40], self.size, .little);
        std.mem.writeInt(u32, out[40..44], self.link, .little);
        std.mem.writeInt(u32, out[44..48], self.info, .little);
        std.mem.writeInt(u64, out[48..56], self.addralign, .little);
        std.mem.writeInt(u64, out[56..64], self.entsize, .little);
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

fn sliceAt(bytes: []const u8, offset: u64, size: u64) ?[]const u8 {
    if (offset > bytes.len or bytes.len - offset < size) return null;
    return bytes[@intCast(offset)..][0..@intCast(size)];
}

fn readU16(bytes: []const u8, at: usize) u16 {
    return std.mem.readInt(u16, bytes[at..][0..2], .little);
}
fn readU32(bytes: []const u8, at: usize) u32 {
    return std.mem.readInt(u32, bytes[at..][0..4], .little);
}
fn readU64(bytes: []const u8, at: usize) u64 {
    return std.mem.readInt(u64, bytes[at..][0..8], .little);
}

pub const PT_LOAD = 1;
pub const PT_PHDR = 6;
pub const PF_X = 1;
pub const PF_W = 2;
pub const PF_R = 4;
const ET_REL = 1;
const ET_EXEC = 2;
const EM_X86_64 = 62;
pub const SHT_PROGBITS = 1;
pub const SHT_SYMTAB = 2;
pub const SHT_STRTAB = 3;
pub const SHT_NOBITS = 8;
pub const SHF_WRITE = 0x1;
pub const SHF_ALLOC = 0x2;
pub const SHF_EXECINSTR = 0x4;
const SHF_GNU_RETAIN = 0x200000;
const SHN_ABS = 0xfff1;
pub const STB_LOCAL = 0;
pub const STB_GLOBAL = 1;
pub const STB_WEAK = 2;
const STT_NOTYPE = 0;
const STV_HIDDEN = 2;

test "the host link object defines each provide at its window" {
    const allocator = std.testing.allocator;
    const object = try linkObject(allocator, &.{ "roc_answer", "roc_main" });
    defer allocator.free(object);
    try std.testing.expectEqual(@as(u16, 5), readU16(object, 60));
    const shoff = readU64(object, 40);
    const symtab: usize = @intCast(shoff + 2 * 64);
    const entries = object[@intCast(readU64(object, symtab + 24))..][0..@intCast(readU64(object, symtab + 32))];
    try std.testing.expectEqual(@as(usize, 3 * 24), entries.len);
    try std.testing.expectEqual(provideAddress(1), readU64(entries, 2 * 24 + 8));
    try std.testing.expectEqual(@as(u16, SHN_ABS), readU16(entries, 24 + 6));
}

test "a host key is a function of contents, not of where they are" {
    const input: KeyInput = .{ .target = "x64musl", .link_abi = "musl", .provides = &.{"roc_main"}, .keep = &.{"roc_alloc"}, .link_items = &.{ "crt1.o", "libhost.a" }, .bundle_hash = null, .input_digests = &.{ @splat(1), @splat(2) } };
    const baseline = key(input);
    try std.testing.expectEqual(baseline, key(input));
    var changed = input;
    changed.input_digests = &.{ @splat(1), @splat(3) };
    try std.testing.expect(!std.mem.eql(u8, &baseline, &key(changed)));
    var bundled = input;
    bundled.bundle_hash = "abc";
    try std.testing.expect(!std.mem.eql(u8, &baseline, &key(bundled)));
}
