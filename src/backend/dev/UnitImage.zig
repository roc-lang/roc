//! A unit image: one module's machine code laid out once, ready to be placed
//! into a dev executable by copying and patching (plan "Fast dev linking",
//! design.md "Per-Module Output").
//!
//! An image holds a code block (the unit's text followed by its read-only
//! data, at a fixed distance from each other), its writable data and the
//! size of its zero-initialized data, its symbols, and the patch sites that
//! still need an address: every reference to another image or to the host
//! (an import), and every reference whose distance depends on where the
//! linker places the unit's regions (a local site). References inside the
//! code block that are position independent are resolved when the image is
//! built, so placing a unit never touches them.
//!
//! Images are built from x86_64 ELF relocatable objects (`build`): a unit's
//! single-unit object, or the compiler's embedded support objects (the
//! builtins and the Boxy runtime), which form one image together. The
//! encoding is a pure function of the input bytes: fixed-width
//! little-endian tables in input order, names sorted where the reader
//! searches them. Two builds of the same input write identical bytes.
//!
//! This file depends on `std` alone, so Roc's build graph can convert the
//! support objects with it.
const std = @import("std");
const Allocator = std.mem.Allocator;

const magic = "RBXU".*;
/// Format version; bump whenever the encoding or its contents change.
pub const format_version: u32 = 1;
/// Alignment of the code block inside an image, and of every region the
/// linker places.
pub const page_size: u32 = 4096;

/// Where a symbol or a patch site lives, or what a local site points at.
pub const Region = enum(u8) {
    /// Text, then read-only data.
    block = 0,
    data = 1,
    bss = 2,
    debug_line = 3,
    debug_info = 4,
    debug_abbrev = 5,

    /// Whether the region is mapped into the executable's address space.
    /// A debug region's addresses are offsets in its output section.
    pub fn isAllocated(self: Region) bool {
        return switch (self) {
            .block, .data, .bss => true,
            .debug_line, .debug_info, .debug_abbrev => false,
        };
    }
};

/// How a patch site encodes its target.
pub const SiteKind = enum(u8) {
    /// 64-bit absolute address (`R_X86_64_64`).
    abs64 = 0,
    /// 32-bit displacement from the end of a 4-byte field
    /// (`R_X86_64_PC32`, `R_X86_64_PLT32`; the addend carries the -4).
    pc32 = 1,
    /// 32-bit zero-extended absolute value (`R_X86_64_32`).
    abs32 = 2,
    /// 32-bit sign-extended absolute value (`R_X86_64_32S`).
    abs32s = 3,
};

/// `Site.target_region` of a site whose target is an import.
pub const import_target: u8 = 0xff;

pub const SymbolType = enum(u8) { notype = 0, object = 1, func = 2 };

pub const symbol_exported: u8 = 1;
pub const symbol_weak: u8 = 2;

/// One symbol the image defines.
pub const Symbol = extern struct {
    /// Offset of the NUL-terminated name in the string table.
    name: u32,
    /// Offset of the symbol in its region.
    value: u32,
    size: u32,
    region: u8,
    type: u8,
    flags: u8,
    reserved: u8 = 0,
};

/// One external symbol the image references, with the range of its sites.
pub const Import = extern struct {
    name: u32,
    first_site: u32,
    site_count: u32,
    /// `symbol_weak` when every reference is weak.
    flags: u32,
};

/// One field to fill once the image is placed.
pub const Site = extern struct {
    /// Offset of the field in `region`.
    offset: u32,
    /// The import index, for an import site.
    target: u32,
    /// Added to the target address; for a local site the target's offset
    /// in `target_region` is folded in.
    addend: i64,
    region: u8,
    kind: u8,
    /// A `Region`, or `import_target`.
    target_region: u8,
    reserved: [5]u8 = @splat(0),
};

const Header = extern struct {
    magic: [4]u8,
    version: u32,
    block_offset: u32,
    block_size: u32,
    block_align: u32,
    text_size: u32,
    data_offset: u32,
    data_size: u32,
    data_align: u32,
    bss_size: u32,
    bss_align: u32,
    symbols_offset: u32,
    symbol_count: u32,
    exports_offset: u32,
    export_count: u32,
    imports_offset: u32,
    import_count: u32,
    sites_offset: u32,
    site_count: u32,
    import_site_count: u32,
    strings_offset: u32,
    strings_size: u32,
    debug_line_offset: u32,
    debug_line_size: u32,
    debug_info_offset: u32,
    debug_info_size: u32,
    debug_abbrev_offset: u32,
    debug_abbrev_size: u32,
    total_size: u32,
    reserved: u32 = 0,
};

/// The alignment an image's bytes need in memory for `View`.
pub const alignment: std.mem.Alignment = .@"16";

/// Why bytes are not an image this compiler wrote.
pub const ReadError = error{InvalidImage};

/// Read access to an encoded image, borrowing its bytes.
pub const View = struct {
    bytes: []align(alignment.toByteUnits()) const u8,
    header: Header,

    pub fn init(bytes: []align(alignment.toByteUnits()) const u8) ReadError!View {
        if (bytes.len < @sizeOf(Header)) return error.InvalidImage;
        const header = std.mem.bytesToValue(Header, bytes[0..@sizeOf(Header)]);
        if (!std.mem.eql(u8, &header.magic, &magic)) return error.InvalidImage;
        if (header.version != format_version) return error.InvalidImage;
        if (header.total_size != bytes.len) return error.InvalidImage;
        try checkRange(bytes.len, header.block_offset, header.block_size, 1);
        try checkRange(bytes.len, header.data_offset, header.data_size, 1);
        try checkRange(bytes.len, header.symbols_offset, header.symbol_count, @sizeOf(Symbol));
        try checkRange(bytes.len, header.exports_offset, header.export_count, @sizeOf(u32));
        try checkRange(bytes.len, header.imports_offset, header.import_count, @sizeOf(Import));
        try checkRange(bytes.len, header.sites_offset, header.site_count, @sizeOf(Site));
        try checkRange(bytes.len, header.strings_offset, header.strings_size, 1);
        try checkRange(bytes.len, header.debug_line_offset, header.debug_line_size, 1);
        try checkRange(bytes.len, header.debug_info_offset, header.debug_info_size, 1);
        try checkRange(bytes.len, header.debug_abbrev_offset, header.debug_abbrev_size, 1);
        if (header.text_size > header.block_size or header.import_site_count > header.site_count) return error.InvalidImage;
        if (header.strings_size == 0 or bytes[header.strings_offset + header.strings_size - 1] != 0) return error.InvalidImage;
        if (header.block_offset % page_size != 0 or header.symbols_offset % 8 != 0 or header.exports_offset % 8 != 0 or
            header.imports_offset % 8 != 0 or header.sites_offset % 8 != 0) return error.InvalidImage;
        return .{ .bytes = bytes, .header = header };
    }

    pub fn block(self: View) []const u8 {
        return self.bytes[self.header.block_offset..][0..self.header.block_size];
    }
    pub fn blockAlign(self: View) u32 {
        return self.header.block_align;
    }
    pub fn textSize(self: View) u32 {
        return self.header.text_size;
    }
    pub fn data(self: View) []const u8 {
        return self.bytes[self.header.data_offset..][0..self.header.data_size];
    }
    pub fn dataAlign(self: View) u32 {
        return self.header.data_align;
    }
    pub fn bssSize(self: View) u32 {
        return self.header.bss_size;
    }
    pub fn bssAlign(self: View) u32 {
        return self.header.bss_align;
    }
    pub fn symbols(self: View) []const Symbol {
        return table(Symbol, self.bytes, self.header.symbols_offset, self.header.symbol_count);
    }
    /// Indices into `symbols` of the exported symbols, in name order.
    pub fn exports(self: View) []const u32 {
        return table(u32, self.bytes, self.header.exports_offset, self.header.export_count);
    }
    /// The imports, in name order.
    pub fn imports(self: View) []const Import {
        return table(Import, self.bytes, self.header.imports_offset, self.header.import_count);
    }
    /// Every site: the import sites grouped by import in import order, then
    /// the local sites.
    pub fn sites(self: View) []const Site {
        return table(Site, self.bytes, self.header.sites_offset, self.header.site_count);
    }
    pub fn importSites(self: View, import: Import) []const Site {
        return self.sites()[import.first_site..][0..import.site_count];
    }
    pub fn localSites(self: View) []const Site {
        return self.sites()[self.header.import_site_count..];
    }
    /// The string table: NUL-terminated names, starting with an empty one.
    /// It is also the image's ELF string table blob.
    pub fn strings(self: View) []const u8 {
        return self.bytes[self.header.strings_offset..][0..self.header.strings_size];
    }
    pub fn name(self: View, offset: u32) []const u8 {
        const all = self.strings();
        return std.mem.sliceTo(all[offset..], 0);
    }
    pub fn debugLine(self: View) []const u8 {
        return self.bytes[self.header.debug_line_offset..][0..self.header.debug_line_size];
    }
    pub fn debugInfo(self: View) []const u8 {
        return self.bytes[self.header.debug_info_offset..][0..self.header.debug_info_size];
    }
    pub fn debugAbbrev(self: View) []const u8 {
        return self.bytes[self.header.debug_abbrev_offset..][0..self.header.debug_abbrev_size];
    }
    pub fn regionBytes(self: View, region: Region) []const u8 {
        return switch (region) {
            .block => self.block(),
            .data => self.data(),
            .bss => &.{},
            .debug_line => self.debugLine(),
            .debug_info => self.debugInfo(),
            .debug_abbrev => self.debugAbbrev(),
        };
    }

    /// The exported symbol named `wanted`, if the image exports one.
    pub fn findExport(self: View, wanted: []const u8) ?Symbol {
        const all = self.symbols();
        const order = self.exports();
        var low: usize = 0;
        var high: usize = order.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            const symbol = all[order[mid]];
            switch (std.mem.order(u8, self.name(symbol.name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return symbol,
            }
        }
        return null;
    }
};

fn checkRange(len: usize, offset: u32, count: u32, size: usize) ReadError!void {
    const bytes = std.math.mul(usize, count, size) catch return error.InvalidImage;
    const end = std.math.add(usize, offset, bytes) catch return error.InvalidImage;
    if (end > len) return error.InvalidImage;
}

fn table(comptime T: type, bytes: []align(alignment.toByteUnits()) const u8, offset: u32, count: u32) []const T {
    const start: [*]const u8 = bytes.ptr + offset;
    const typed: [*]const T = @ptrCast(@alignCast(start));
    return typed[0..count];
}

/// Why objects could not be made into an image. Every one of these means
/// the input is not an object this converter was written for: the dev
/// backend's own object writer and the compiler's embedded support objects
/// use exactly the sections, symbols and relocations it handles.
pub const BuildError = Allocator.Error || error{
    InvalidObject,
    UnsupportedSection,
    UnsupportedSymbol,
    UnsupportedRelocation,
    DuplicateDefinition,
    RelocationOutOfRange,
    ImageTooLarge,
};

pub const BuildOptions = struct {
    /// Keep `.debug_line`, `.debug_info` and `.debug_abbrev`. Without it,
    /// every debug section is dropped.
    keep_debug: bool,
};

/// Build one image from `objects`, linking them to each other: a symbol one
/// object references and another defines globally resolves inside the
/// image, a strong definition taking precedence over a weak one. What no
/// object defines becomes an import.
pub fn build(allocator: Allocator, objects: []const []const u8, options: BuildOptions) BuildError![]align(alignment.toByteUnits()) u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const parsed = try arena.alloc(Object, objects.len);
    for (objects, parsed) |bytes, *object| object.* = try Object.parse(arena, bytes);

    // Place every kept section: text, then read-only data, in the block;
    // writable data; zero-fill; and the debug sections by name.
    const places = try arena.alloc([]?Place, parsed.len);
    for (parsed, places) |object, *object_places| {
        object_places.* = try arena.alloc(?Place, object.sections.len);
        @memset(object_places.*, null);
    }
    var block_size: u64 = 0;
    var block_align: u64 = 16;
    var data_size: u64 = 0;
    var data_align: u64 = 8;
    var bss_size: u64 = 0;
    var bss_align: u64 = 8;
    var debug_sizes = [_]u64{ 0, 0, 0 };
    for (parsed, places) |object, object_places| {
        for (object.sections, 0..) |section, index| {
            if (try classify(section, options) != .text) continue;
            block_align = @max(block_align, section.addralign);
            block_size = std.mem.alignForward(u64, block_size, section.addralign);
            object_places[index] = .{ .region = .block, .offset = block_size };
            block_size += section.size;
        }
    }
    const text_size = block_size;
    for (parsed, places) |object, object_places| {
        for (object.sections, 0..) |section, index| {
            const class = try classify(section, options);
            switch (class) {
                .text, .skip => {},
                .rodata => {
                    block_align = @max(block_align, section.addralign);
                    block_size = std.mem.alignForward(u64, block_size, section.addralign);
                    object_places[index] = .{ .region = .block, .offset = block_size };
                    block_size += section.size;
                },
                .data => {
                    data_align = @max(data_align, section.addralign);
                    data_size = std.mem.alignForward(u64, data_size, section.addralign);
                    object_places[index] = .{ .region = .data, .offset = data_size };
                    data_size += section.size;
                },
                .bss => {
                    bss_align = @max(bss_align, section.addralign);
                    bss_size = std.mem.alignForward(u64, bss_size, section.addralign);
                    object_places[index] = .{ .region = .bss, .offset = bss_size };
                    bss_size += section.size;
                },
                .debug_line, .debug_info, .debug_abbrev => {
                    const slot: usize = switch (class) {
                        .debug_line => 0,
                        .debug_info => 1,
                        .debug_abbrev => 2,
                        else => unreachable, // the enclosing prong matches only the three debug classes
                    };
                    const region: Region = switch (class) {
                        .debug_line => .debug_line,
                        .debug_info => .debug_info,
                        .debug_abbrev => .debug_abbrev,
                        else => unreachable, // the enclosing prong matches only the three debug classes
                    };
                    object_places[index] = .{ .region = region, .offset = debug_sizes[slot] };
                    debug_sizes[slot] += section.size;
                },
            }
        }
    }
    if (block_align > page_size or data_align > page_size or bss_align > page_size) return error.UnsupportedSection;
    const limit = std.math.maxInt(u32);
    if (block_size > limit or data_size > limit or bss_size > limit or debug_sizes[0] > limit or debug_sizes[1] > limit or debug_sizes[2] > limit) return error.ImageTooLarge;

    const block_bytes = try arena.alloc(u8, @intCast(block_size));
    @memset(block_bytes, 0);
    const data_bytes = try arena.alloc(u8, @intCast(data_size));
    @memset(data_bytes, 0);
    var debug_bytes: [3][]u8 = undefined;
    for (&debug_bytes, debug_sizes) |*slot, size| {
        slot.* = try arena.alloc(u8, @intCast(size));
        @memset(slot.*, 0);
    }
    for (parsed, places) |object, object_places| {
        for (object.sections, object_places) |section, place_or_null| {
            const place = place_or_null orelse continue;
            if (section.type == SHT_NOBITS) continue;
            const contents = try object.sectionBytes(section);
            const destination: []u8 = switch (place.region) {
                .block => block_bytes,
                .data => data_bytes,
                .bss => unreachable, // only NOBITS sections are placed in zero-fill, and they have no bytes
                .debug_line => debug_bytes[0],
                .debug_info => debug_bytes[1],
                .debug_abbrev => debug_bytes[2],
            };
            @memcpy(destination[@intCast(place.offset)..][0..contents.len], contents);
        }
    }

    // Global definitions across the objects: a strong one wins over weak
    // ones, the first weak one wins among weak ones.
    var definitions: std.StringArrayHashMapUnmanaged(Definition) = .empty;
    for (parsed, places, 0..) |object, object_places, object_index| {
        for (object.symbols, 0..) |symbol, symbol_index| {
            if (symbol.bind == STB_LOCAL or symbol.shndx == SHN_UNDEF) continue;
            if (symbol.shndx >= SHN_LORESERVE) return error.UnsupportedSymbol;
            const place = object_places[symbol.shndx] orelse return error.UnsupportedSymbol;
            const weak = symbol.bind == STB_WEAK;
            const candidate: Definition = .{ .object = @intCast(object_index), .symbol = @intCast(symbol_index), .weak = weak, .region = place.region, .offset = place.offset + symbol.value };
            const gop = try definitions.getOrPut(arena, symbol.name);
            if (!gop.found_existing) {
                gop.value_ptr.* = candidate;
            } else if (gop.value_ptr.weak and !weak) {
                gop.value_ptr.* = candidate;
            } else if (!gop.value_ptr.weak and !weak) {
                return error.DuplicateDefinition;
            }
        }
    }

    var strings = Strings{ .arena = arena };
    try strings.bytes.append(arena, 0);

    // Symbols in object order, each object's in its symbol table's order.
    var symbol_records: std.ArrayList(Symbol) = .empty;
    var exported: std.ArrayList(u32) = .empty;
    for (parsed, places, 0..) |object, object_places, object_index| {
        for (object.symbols, 0..) |symbol, symbol_index| {
            if (symbol_index == 0 or symbol.shndx == SHN_UNDEF) continue;
            if (symbol.type == STT_SECTION or symbol.type == STT_FILE) continue;
            if (symbol.shndx >= SHN_LORESERVE) return error.UnsupportedSymbol;
            const place = object_places[symbol.shndx] orelse continue;
            if (!place.region.isAllocated()) continue;
            if (symbol.name.len == 0) continue;
            const is_global = symbol.bind != STB_LOCAL;
            if (is_global) {
                const winner = definitions.get(symbol.name).?;
                if (winner.object != object_index or winner.symbol != symbol_index) continue;
            }
            const visible = is_global and (symbol.visibility == STV_DEFAULT or symbol.visibility == STV_PROTECTED);
            const symbol_type: SymbolType = switch (symbol.type) {
                STT_FUNC => .func,
                STT_OBJECT => .object,
                STT_NOTYPE => .notype,
                else => return error.UnsupportedSymbol,
            };
            const index: u32 = @intCast(symbol_records.items.len);
            try symbol_records.append(arena, .{
                .name = try strings.add(symbol.name),
                .value = @intCast(place.offset + symbol.value),
                .size = std.math.cast(u32, symbol.size) orelse return error.ImageTooLarge,
                .region = @backingInt(place.region),
                .type = @backingInt(symbol_type),
                .flags = (if (visible) symbol_exported else 0) | (if (symbol.bind == STB_WEAK) symbol_weak else 0),
            });
            if (visible) try exported.append(arena, index);
        }
    }

    // Relocations: resolve what is position independent inside the block,
    // keep the rest as sites.
    var import_index: std.StringArrayHashMapUnmanaged(PendingImport) = .empty;
    var import_sites: std.ArrayList(PendingSite) = .empty;
    var local_sites: std.ArrayList(Site) = .empty;
    for (parsed, places) |object, object_places| {
        for (object.sections) |section| {
            if (section.type != SHT_RELA) continue;
            if (section.info >= object.sections.len) return error.InvalidObject;
            const site_place = object_places[section.info] orelse continue;
            const site_section = object.sections[section.info];
            const entries = try object.sectionBytes(section);
            if (entries.len % 24 != 0) return error.InvalidObject;
            var at: usize = 0;
            while (at < entries.len) : (at += 24) {
                const r_offset = readU64(entries, at);
                const r_info = readU64(entries, at + 8);
                const r_addend: i64 = @bitCast(readU64(entries, at + 16));
                const r_sym: u32 = @truncate(r_info >> 32);
                const r_type: u32 = @truncate(r_info);
                if (r_sym >= object.symbols.len) return error.InvalidObject;
                const kind: SiteKind = switch (r_type) {
                    R_X86_64_64 => .abs64,
                    R_X86_64_PC32, R_X86_64_PLT32 => .pc32,
                    R_X86_64_32 => .abs32,
                    R_X86_64_32S => .abs32s,
                    else => return error.UnsupportedRelocation,
                };
                if (r_offset + kindWidth(kind) > site_section.size) return error.InvalidObject;
                const site_offset = site_place.offset + r_offset;
                const symbol = object.symbols[r_sym];
                const target: ?Definition = target: {
                    if (symbol.shndx == SHN_UNDEF) break :target definitions.get(symbol.name);
                    if (symbol.bind != STB_LOCAL) break :target definitions.get(symbol.name).?;
                    if (symbol.shndx >= SHN_LORESERVE) return error.UnsupportedSymbol;
                    const place = object_places[symbol.shndx] orelse return error.UnsupportedRelocation;
                    break :target .{ .object = 0, .symbol = 0, .weak = false, .region = place.region, .offset = place.offset + symbol.value };
                };
                if (target) |local| {
                    const addend = r_addend + @as(i64, @intCast(local.offset));
                    if (kind == .pc32 and site_place.region == .block and local.region == .block) {
                        const value = addend - @as(i64, @intCast(site_offset));
                        const narrow = std.math.cast(i32, value) orelse return error.RelocationOutOfRange;
                        std.mem.writeInt(i32, block_bytes[@intCast(site_offset)..][0..4], narrow, .little);
                        continue;
                    }
                    if (kind == .pc32 and !(site_place.region.isAllocated() and local.region.isAllocated())) return error.UnsupportedRelocation;
                    try local_sites.append(arena, .{
                        .offset = @intCast(site_offset),
                        .target = 0,
                        .addend = addend,
                        .region = @backingInt(site_place.region),
                        .kind = @backingInt(kind),
                        .target_region = @backingInt(local.region),
                    });
                } else {
                    if (!site_place.region.isAllocated()) return error.UnsupportedRelocation;
                    const gop = try import_index.getOrPut(arena, symbol.name);
                    if (!gop.found_existing) gop.value_ptr.* = .{ .weak = true };
                    if (symbol.bind != STB_WEAK) gop.value_ptr.weak = false;
                    try import_sites.append(arena, .{ .import = @intCast(gop.index), .site = .{
                        .offset = @intCast(site_offset),
                        .target = 0,
                        .addend = r_addend,
                        .region = @backingInt(site_place.region),
                        .kind = @backingInt(kind),
                        .target_region = import_target,
                    } });
                }
            }
        }
    }

    // Imports in name order, each with its sites in the order found.
    const import_count = import_index.count();
    const import_order = try arena.alloc(u32, import_count);
    for (import_order, 0..) |*slot, index| slot.* = @intCast(index);
    const import_names = import_index.keys();
    std.mem.sort(u32, import_order, import_names, struct {
        fn lessThan(names: []const []const u8, a: u32, b: u32) bool {
            return std.mem.lessThan(u8, names[a], names[b]);
        }
    }.lessThan);
    const rank = try arena.alloc(u32, import_count);
    for (import_order, 0..) |original, sorted| rank[original] = @intCast(sorted);
    const site_counts = try arena.alloc(u32, import_count);
    @memset(site_counts, 0);
    for (import_sites.items) |pending| site_counts[rank[pending.import]] += 1;
    const import_records = try arena.alloc(Import, import_count);
    var first: u32 = 0;
    for (import_records, import_order, site_counts) |*record, original, count| {
        record.* = .{
            .name = try strings.add(import_names[original]),
            .first_site = first,
            .site_count = count,
            .flags = if (import_index.values()[original].weak) symbol_weak else 0,
        };
        first += count;
    }
    const all_sites = try arena.alloc(Site, import_sites.items.len + local_sites.items.len);
    const fill = try arena.alloc(u32, import_count);
    @memset(fill, 0);
    for (import_sites.items) |pending| {
        const sorted = rank[pending.import];
        var site = pending.site;
        site.target = sorted;
        all_sites[import_records[sorted].first_site + fill[sorted]] = site;
        fill[sorted] += 1;
    }
    @memcpy(all_sites[import_sites.items.len..], local_sites.items);

    // Exports in name order.
    std.mem.sort(u32, exported.items, Exports{ .symbols = symbol_records.items, .strings = &strings }, Exports.lessThan);

    // Encode.
    var layout = Layout{};
    const header_end = layout.reserve(@sizeOf(Header), 8);
    _ = header_end;
    const block_offset = layout.reserve(block_bytes.len, page_size);
    const data_offset = layout.reserve(data_bytes.len, 16);
    const symbols_offset = layout.reserve(symbol_records.items.len * @sizeOf(Symbol), 8);
    const exports_offset = layout.reserve(exported.items.len * @sizeOf(u32), 8);
    const imports_offset = layout.reserve(import_records.len * @sizeOf(Import), 8);
    const sites_offset = layout.reserve(all_sites.len * @sizeOf(Site), 8);
    const strings_offset = layout.reserve(strings.bytes.items.len, 8);
    const debug_line_offset = layout.reserve(debug_bytes[0].len, 8);
    const debug_info_offset = layout.reserve(debug_bytes[1].len, 8);
    const debug_abbrev_offset = layout.reserve(debug_bytes[2].len, 8);
    const total = std.mem.alignForward(u64, layout.end, 16);
    if (total > limit) return error.ImageTooLarge;

    const out = try allocator.alignedAlloc(u8, alignment, @intCast(total));
    @memset(out, 0);
    const header: Header = .{
        .magic = magic,
        .version = format_version,
        .block_offset = block_offset,
        .block_size = @intCast(block_bytes.len),
        .block_align = @intCast(block_align),
        .text_size = @intCast(text_size),
        .data_offset = data_offset,
        .data_size = @intCast(data_bytes.len),
        .data_align = @intCast(data_align),
        .bss_size = @intCast(bss_size),
        .bss_align = @intCast(bss_align),
        .symbols_offset = symbols_offset,
        .symbol_count = @intCast(symbol_records.items.len),
        .exports_offset = exports_offset,
        .export_count = @intCast(exported.items.len),
        .imports_offset = imports_offset,
        .import_count = @intCast(import_records.len),
        .sites_offset = sites_offset,
        .site_count = @intCast(all_sites.len),
        .import_site_count = @intCast(import_sites.items.len),
        .strings_offset = strings_offset,
        .strings_size = @intCast(strings.bytes.items.len),
        .debug_line_offset = debug_line_offset,
        .debug_line_size = @intCast(debug_bytes[0].len),
        .debug_info_offset = debug_info_offset,
        .debug_info_size = @intCast(debug_bytes[1].len),
        .debug_abbrev_offset = debug_abbrev_offset,
        .debug_abbrev_size = @intCast(debug_bytes[2].len),
        .total_size = @intCast(total),
    };
    @memcpy(out[0..@sizeOf(Header)], std.mem.asBytes(&header));
    @memcpy(out[block_offset..][0..block_bytes.len], block_bytes);
    @memcpy(out[data_offset..][0..data_bytes.len], data_bytes);
    @memcpy(out[symbols_offset..][0 .. symbol_records.items.len * @sizeOf(Symbol)], std.mem.sliceAsBytes(symbol_records.items));
    @memcpy(out[exports_offset..][0 .. exported.items.len * @sizeOf(u32)], std.mem.sliceAsBytes(exported.items));
    @memcpy(out[imports_offset..][0 .. import_records.len * @sizeOf(Import)], std.mem.sliceAsBytes(import_records));
    @memcpy(out[sites_offset..][0 .. all_sites.len * @sizeOf(Site)], std.mem.sliceAsBytes(all_sites));
    @memcpy(out[strings_offset..][0..strings.bytes.items.len], strings.bytes.items);
    @memcpy(out[debug_line_offset..][0..debug_bytes[0].len], debug_bytes[0]);
    @memcpy(out[debug_info_offset..][0..debug_bytes[1].len], debug_bytes[1]);
    @memcpy(out[debug_abbrev_offset..][0..debug_bytes[2].len], debug_bytes[2]);
    return out;
}

/// The width in bytes of a site's field.
pub fn kindWidth(kind: SiteKind) u64 {
    return switch (kind) {
        .abs64 => 8,
        .pc32, .abs32, .abs32s => 4,
    };
}

/// Write `target + addend` into `field` as `kind` encodes it, where the
/// field sits at address `site_address` (used only by `pc32`). Returns
/// false when the value does not fit the field.
pub fn applySite(field: []u8, kind: SiteKind, target_plus_addend: i64, site_address: u64) bool {
    switch (kind) {
        .abs64 => std.mem.writeInt(u64, field[0..8], @bitCast(target_plus_addend), .little),
        .pc32 => {
            const value = target_plus_addend - @as(i64, @intCast(site_address));
            const narrow = std.math.cast(i32, value) orelse return false;
            std.mem.writeInt(i32, field[0..4], narrow, .little);
        },
        .abs32 => {
            const narrow = std.math.cast(u32, target_plus_addend) orelse return false;
            std.mem.writeInt(u32, field[0..4], narrow, .little);
        },
        .abs32s => {
            const narrow = std.math.cast(i32, target_plus_addend) orelse return false;
            std.mem.writeInt(i32, field[0..4], narrow, .little);
        },
    }
    return true;
}

const Place = struct { region: Region, offset: u64 };

const Definition = struct {
    object: u32,
    symbol: u32,
    weak: bool,
    region: Region,
    offset: u64,
};

const PendingImport = struct { weak: bool };
const PendingSite = struct { import: u32, site: Site };

const Strings = struct {
    arena: Allocator,
    bytes: std.ArrayList(u8) = .empty,
    offsets: std.StringHashMapUnmanaged(u32) = .empty,

    fn add(self: *Strings, text: []const u8) Allocator.Error!u32 {
        if (text.len == 0) return 0;
        const gop = try self.offsets.getOrPut(self.arena, text);
        if (gop.found_existing) return gop.value_ptr.*;
        const offset: u32 = @intCast(self.bytes.items.len);
        try self.bytes.appendSlice(self.arena, text);
        try self.bytes.append(self.arena, 0);
        gop.value_ptr.* = offset;
        return offset;
    }

    fn get(self: *const Strings, offset: u32) []const u8 {
        return std.mem.sliceTo(self.bytes.items[offset..], 0);
    }
};

const Exports = struct {
    symbols: []const Symbol,
    strings: *const Strings,

    fn lessThan(self: Exports, a: u32, b: u32) bool {
        return std.mem.lessThan(u8, self.strings.get(self.symbols[a].name), self.strings.get(self.symbols[b].name));
    }
};

const Layout = struct {
    end: u64 = 0,

    fn reserve(self: *Layout, size: usize, align_to: u64) u32 {
        const start = std.mem.alignForward(u64, self.end, align_to);
        self.end = start + size;
        return @intCast(start);
    }
};

const SectionClass = enum { skip, text, rodata, data, bss, debug_line, debug_info, debug_abbrev };

fn classify(section: Section, options: BuildOptions) BuildError!SectionClass {
    switch (section.type) {
        SHT_NULL, SHT_SYMTAB, SHT_STRTAB, SHT_RELA => return .skip,
        SHT_X86_64_UNWIND => return .skip,
        SHT_LLVM_ADDRSIG => return .skip,
        else => {},
    }
    if (section.flags & SHF_TLS != 0) return error.UnsupportedSection;
    if (section.flags & SHF_ALLOC != 0) {
        if (section.addralign > page_size) return error.UnsupportedSection;
        return switch (section.type) {
            SHT_PROGBITS => if (section.flags & SHF_EXECINSTR != 0)
                .text
            else if (section.flags & SHF_WRITE != 0)
                .data
            else
                .rodata,
            SHT_NOBITS => .bss,
            else => error.UnsupportedSection,
        };
    }
    if (section.type == SHT_PROGBITS and std.mem.startsWith(u8, section.name, ".debug_")) {
        if (!options.keep_debug) return .skip;
        if (std.mem.eql(u8, section.name, ".debug_line")) return .debug_line;
        if (std.mem.eql(u8, section.name, ".debug_info")) return .debug_info;
        if (std.mem.eql(u8, section.name, ".debug_abbrev")) return .debug_abbrev;
        return error.UnsupportedSection;
    }
    if (std.mem.eql(u8, section.name, ".note.GNU-stack") or std.mem.eql(u8, section.name, ".comment")) return .skip;
    return error.UnsupportedSection;
}

const Section = struct {
    name: []const u8,
    type: u32,
    flags: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addralign: u64,
};

const ObjectSymbol = struct {
    name: []const u8,
    value: u64,
    size: u64,
    shndx: u16,
    bind: u4,
    type: u4,
    visibility: u2,
};

const Object = struct {
    bytes: []const u8,
    sections: []Section,
    symbols: []ObjectSymbol,

    fn parse(arena: Allocator, bytes: []const u8) BuildError!Object {
        if (bytes.len < 64 or !std.mem.eql(u8, bytes[0..4], "\x7fELF")) return error.InvalidObject;
        if (bytes[4] != 2 or bytes[5] != 1) return error.InvalidObject;
        if (readU16(bytes, 16) != ET_REL or readU16(bytes, 18) != EM_X86_64) return error.InvalidObject;
        const shoff = readU64(bytes, 40);
        const shentsize = readU16(bytes, 58);
        const shnum = readU16(bytes, 60);
        const shstrndx = readU16(bytes, 62);
        if (shentsize != 64 or shnum == 0 or shstrndx >= shnum) return error.InvalidObject;
        if (shoff > bytes.len or (bytes.len - shoff) / 64 < shnum) return error.InvalidObject;
        const sections = try arena.alloc(Section, shnum);
        for (sections, 0..) |*section, index| {
            const at: usize = @intCast(shoff + index * 64);
            section.* = .{
                .name = "",
                .type = readU32(bytes, at + 4),
                .flags = readU64(bytes, at + 8),
                .offset = readU64(bytes, at + 24),
                .size = readU64(bytes, at + 32),
                .link = readU32(bytes, at + 40),
                .info = readU32(bytes, at + 44),
                .addralign = @max(1, readU64(bytes, at + 48)),
            };
            if (!std.math.isPowerOfTwo(section.addralign)) return error.InvalidObject;
            if (section.type != SHT_NOBITS and section.type != SHT_NULL and (section.offset > bytes.len or bytes.len - section.offset < section.size)) return error.InvalidObject;
        }
        const shstrtab = sections[shstrndx];
        const names = bytes[@intCast(shstrtab.offset)..][0..@intCast(shstrtab.size)];
        for (sections, 0..) |*section, index| {
            const at: usize = @intCast(shoff + index * 64);
            const name_offset = readU32(bytes, at);
            if (name_offset >= names.len) return error.InvalidObject;
            section.name = std.mem.sliceTo(names[name_offset..], 0);
        }
        var symbols: []ObjectSymbol = &.{};
        var symtab_seen = false;
        for (sections) |section| {
            if (section.type != SHT_SYMTAB) continue;
            if (symtab_seen) return error.InvalidObject;
            symtab_seen = true;
            if (section.link >= sections.len) return error.InvalidObject;
            const strtab = sections[section.link];
            const symbol_names = bytes[@intCast(strtab.offset)..][0..@intCast(strtab.size)];
            const entries = bytes[@intCast(section.offset)..][0..@intCast(section.size)];
            if (entries.len % 24 != 0) return error.InvalidObject;
            symbols = try arena.alloc(ObjectSymbol, entries.len / 24);
            for (symbols, 0..) |*symbol, index| {
                const at = index * 24;
                const name_offset = readU32(entries, at);
                if (name_offset >= symbol_names.len and name_offset != 0) return error.InvalidObject;
                const info = entries[at + 4];
                symbol.* = .{
                    .name = if (symbol_names.len == 0) "" else std.mem.sliceTo(symbol_names[name_offset..], 0),
                    .value = readU64(entries, at + 8),
                    .size = readU64(entries, at + 16),
                    .shndx = readU16(entries, at + 6),
                    .bind = @truncate(info >> 4),
                    .type = @truncate(info),
                    .visibility = @truncate(entries[at + 5]),
                };
                switch (symbol.bind) {
                    STB_LOCAL, STB_GLOBAL, STB_WEAK => {},
                    else => return error.UnsupportedSymbol,
                }
                if (symbol.shndx == SHN_COMMON or (symbol.shndx == SHN_ABS and symbol.type != STT_FILE)) return error.UnsupportedSymbol;
                if (symbol.shndx != SHN_UNDEF and symbol.shndx < SHN_LORESERVE and symbol.shndx >= sections.len) return error.InvalidObject;
            }
        }
        return .{ .bytes = bytes, .sections = sections, .symbols = symbols };
    }

    fn sectionBytes(self: Object, section: Section) BuildError![]const u8 {
        return self.bytes[@intCast(section.offset)..][0..@intCast(section.size)];
    }
};

fn readU16(bytes: []const u8, at: usize) u16 {
    return std.mem.readInt(u16, bytes[at..][0..2], .little);
}
fn readU32(bytes: []const u8, at: usize) u32 {
    return std.mem.readInt(u32, bytes[at..][0..4], .little);
}
fn readU64(bytes: []const u8, at: usize) u64 {
    return std.mem.readInt(u64, bytes[at..][0..8], .little);
}

const ET_REL = 1;
const EM_X86_64 = 62;
const SHT_NULL = 0;
const SHT_PROGBITS = 1;
const SHT_SYMTAB = 2;
const SHT_STRTAB = 3;
const SHT_RELA = 4;
const SHT_NOBITS = 8;
const SHT_X86_64_UNWIND = 0x70000001;
const SHT_LLVM_ADDRSIG = 0x6fff4c03;
const SHF_WRITE = 0x1;
const SHF_ALLOC = 0x2;
const SHF_EXECINSTR = 0x4;
const SHF_TLS = 0x400;
const SHN_UNDEF = 0;
const SHN_LORESERVE = 0xff00;
const SHN_ABS = 0xfff1;
const SHN_COMMON = 0xfff2;
const STB_LOCAL = 0;
const STB_GLOBAL = 1;
const STB_WEAK = 2;
const STT_NOTYPE = 0;
const STT_OBJECT = 1;
const STT_FUNC = 2;
const STT_SECTION = 3;
const STT_FILE = 4;
const STV_DEFAULT = 0;
const STV_PROTECTED = 3;
const R_X86_64_64 = 1;
const R_X86_64_PC32 = 2;
const R_X86_64_PLT32 = 4;
const R_X86_64_32 = 10;
const R_X86_64_32S = 11;

/// A minimal x86_64 relocatable object for tests: sections and symbols
/// given as data, encoded the way an assembler lays them out.
pub const TestObject = struct {
    pub const TestSection = struct {
        name: []const u8,
        type: u32 = SHT_PROGBITS,
        flags: u64,
        bytes: []const u8 = &.{},
        size: u64 = 0,
        addralign: u64 = 16,
    };
    pub const TestSymbol = struct {
        name: []const u8,
        /// 0 for undefined, else a 1-based index into the sections.
        section: u16,
        value: u64 = 0,
        size: u64 = 0,
        global: bool = false,
        weak: bool = false,
        func: bool = false,
        hidden: bool = false,
    };
    pub const TestRelocation = struct {
        /// 1-based index of the section the field is in.
        section: u16,
        offset: u64,
        /// 1-based index into the symbols.
        symbol: u32,
        type: u32,
        addend: i64,
    };

    pub fn encode(allocator: Allocator, sections: []const TestSection, symbols: []const TestSymbol, relocations: []const TestRelocation) Allocator.Error![]u8 {
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(allocator);
        try out.appendNTimes(allocator, 0, 64);
        var shstrtab: std.ArrayList(u8) = .empty;
        defer shstrtab.deinit(allocator);
        try shstrtab.append(allocator, 0);
        var strtab: std.ArrayList(u8) = .empty;
        defer strtab.deinit(allocator);
        try strtab.append(allocator, 0);
        const Header_ = struct { name: u32, type: u32, flags: u64, offset: u64, size: u64, link: u32, info: u32, addralign: u64, entsize: u64 };
        var headers: std.ArrayList(Header_) = .empty;
        defer headers.deinit(allocator);
        try headers.append(allocator, std.mem.zeroes(Header_));
        for (sections) |section| {
            const name: u32 = @intCast(shstrtab.items.len);
            try shstrtab.appendSlice(allocator, section.name);
            try shstrtab.append(allocator, 0);
            while (out.items.len % 16 != 0) try out.append(allocator, 0);
            const offset = out.items.len;
            try out.appendSlice(allocator, section.bytes);
            try headers.append(allocator, .{ .name = name, .type = section.type, .flags = section.flags, .offset = offset, .size = if (section.type == SHT_NOBITS) section.size else section.bytes.len, .link = 0, .info = 0, .addralign = section.addralign, .entsize = 0 });
        }
        // Locals first, as ELF requires.
        var ordered: std.ArrayList(u32) = .empty;
        defer ordered.deinit(allocator);
        for (symbols, 0..) |symbol, index| if (!symbol.global and !symbol.weak) try ordered.append(allocator, @intCast(index));
        const first_global: u32 = @intCast(ordered.items.len + 1);
        for (symbols, 0..) |symbol, index| if (symbol.global or symbol.weak) try ordered.append(allocator, @intCast(index));
        const position = try allocator.alloc(u32, symbols.len);
        defer allocator.free(position);
        for (ordered.items, 0..) |original, at| position[original] = @intCast(at + 1);
        while (out.items.len % 8 != 0) try out.append(allocator, 0);
        const symtab_offset = out.items.len;
        try out.appendNTimes(allocator, 0, 24);
        for (ordered.items) |original| {
            const symbol = symbols[original];
            var entry: [24]u8 = @splat(0);
            std.mem.writeInt(u32, entry[0..4], @intCast(strtab.items.len), .little);
            try strtab.appendSlice(allocator, symbol.name);
            try strtab.append(allocator, 0);
            const bind: u8 = if (symbol.weak) STB_WEAK else if (symbol.global) STB_GLOBAL else STB_LOCAL;
            const kind: u8 = if (symbol.func) STT_FUNC else if (symbol.section == 0) STT_NOTYPE else STT_OBJECT;
            entry[4] = (bind << 4) | kind;
            entry[5] = if (symbol.hidden) 2 else 0;
            std.mem.writeInt(u16, entry[6..8], symbol.section, .little);
            std.mem.writeInt(u64, entry[8..16], symbol.value, .little);
            std.mem.writeInt(u64, entry[16..24], symbol.size, .little);
            try out.appendSlice(allocator, &entry);
        }
        const symtab_size = out.items.len - symtab_offset;
        const symtab_index: u32 = @intCast(headers.items.len);
        const symtab_name: u32 = @intCast(shstrtab.items.len);
        try shstrtab.appendSlice(allocator, ".symtab\x00");
        try headers.append(allocator, .{ .name = symtab_name, .type = SHT_SYMTAB, .flags = 0, .offset = symtab_offset, .size = symtab_size, .link = symtab_index + 1, .info = first_global, .addralign = 8, .entsize = 24 });
        const strtab_name: u32 = @intCast(shstrtab.items.len);
        try shstrtab.appendSlice(allocator, ".strtab\x00");
        const strtab_offset = out.items.len;
        try out.appendSlice(allocator, strtab.items);
        try headers.append(allocator, .{ .name = strtab_name, .type = SHT_STRTAB, .flags = 0, .offset = strtab_offset, .size = strtab.items.len, .link = 0, .info = 0, .addralign = 1, .entsize = 0 });
        for (sections, 1..) |_, section_index| {
            var count: usize = 0;
            for (relocations) |relocation| if (relocation.section == section_index) {
                count += 1;
            };
            if (count == 0) continue;
            while (out.items.len % 8 != 0) try out.append(allocator, 0);
            const offset = out.items.len;
            for (relocations) |relocation| if (relocation.section == section_index) {
                var entry: [24]u8 = undefined;
                std.mem.writeInt(u64, entry[0..8], relocation.offset, .little);
                std.mem.writeInt(u64, entry[8..16], (@as(u64, position[relocation.symbol - 1]) << 32) | relocation.type, .little);
                std.mem.writeInt(i64, entry[16..24], relocation.addend, .little);
                try out.appendSlice(allocator, &entry);
            };
            const name: u32 = @intCast(shstrtab.items.len);
            try shstrtab.appendSlice(allocator, ".rela\x00");
            try headers.append(allocator, .{ .name = name, .type = SHT_RELA, .flags = 0, .offset = offset, .size = count * 24, .link = symtab_index, .info = @intCast(section_index), .addralign = 8, .entsize = 24 });
        }
        const shstrtab_name: u32 = @intCast(shstrtab.items.len);
        try shstrtab.appendSlice(allocator, ".shstrtab\x00");
        const shstrtab_offset = out.items.len;
        try out.appendSlice(allocator, shstrtab.items);
        try headers.append(allocator, .{ .name = shstrtab_name, .type = SHT_STRTAB, .flags = 0, .offset = shstrtab_offset, .size = shstrtab.items.len, .link = 0, .info = 0, .addralign = 1, .entsize = 0 });
        while (out.items.len % 8 != 0) try out.append(allocator, 0);
        const shoff = out.items.len;
        for (headers.items) |header| {
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
        const header = out.items[0..64];
        @memcpy(header[0..4], "\x7fELF");
        header[4] = 2;
        header[5] = 1;
        header[6] = 1;
        std.mem.writeInt(u16, header[16..18], ET_REL, .little);
        std.mem.writeInt(u16, header[18..20], EM_X86_64, .little);
        std.mem.writeInt(u32, header[20..24], 1, .little);
        std.mem.writeInt(u64, header[40..48], shoff, .little);
        std.mem.writeInt(u16, header[52..54], 64, .little);
        std.mem.writeInt(u16, header[58..60], 64, .little);
        std.mem.writeInt(u16, header[60..62], @intCast(headers.items.len), .little);
        std.mem.writeInt(u16, header[62..64], @intCast(headers.items.len - 1), .little);
        return try out.toOwnedSlice(allocator);
    }
};

test "an image resolves block-internal references and keeps the rest as sites" {
    const allocator = std.testing.allocator;
    // text: call local (pc32 inside the block), call extern (import),
    // lea rodata (pc32 inside the block), lea bss (local site).
    var text: [32]u8 = @splat(0x90);
    const object = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
        .{ .name = ".rodata", .flags = SHF_ALLOC, .bytes = "constant", .addralign = 8 },
        .{ .name = ".bss", .type = SHT_NOBITS, .flags = SHF_ALLOC | SHF_WRITE, .size = 16, .addralign = 8 },
        .{ .name = ".data", .flags = SHF_ALLOC | SHF_WRITE, .bytes = &@as([8]u8, @splat(0)), .addralign = 8 },
    }, &.{
        .{ .name = "helper", .section = 1, .value = 16, .func = true },
        .{ .name = "main", .section = 1, .value = 0, .size = 16, .global = true, .func = true },
        .{ .name = "constant", .section = 2, .value = 0, .size = 8 },
        .{ .name = "counter", .section = 3, .value = 0, .size = 16 },
        .{ .name = "roc_alloc", .section = 0, .global = true },
        .{ .name = "hidden_global", .section = 1, .value = 24, .global = true, .hidden = true },
    }, &.{
        .{ .section = 1, .offset = 1, .symbol = 1, .type = R_X86_64_PLT32, .addend = -4 },
        .{ .section = 1, .offset = 6, .symbol = 5, .type = R_X86_64_PLT32, .addend = -4 },
        .{ .section = 1, .offset = 11, .symbol = 3, .type = R_X86_64_PC32, .addend = -4 },
        .{ .section = 1, .offset = 20, .symbol = 4, .type = R_X86_64_PC32, .addend = -4 },
        .{ .section = 4, .offset = 0, .symbol = 2, .type = R_X86_64_64, .addend = 0 },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = false });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqual(@as(u32, 32), view.textSize());
    // rodata follows the text at its alignment.
    try std.testing.expectEqualStrings("constant", view.block()[32..40]);
    // helper at 16: displacement from the end of the field at 1..5.
    try std.testing.expectEqual(@as(i32, 16 - 5), std.mem.readInt(i32, view.block()[1..5], .little));
    // constant at 32 from the field at 11..15.
    try std.testing.expectEqual(@as(i32, 32 - 15), std.mem.readInt(i32, view.block()[11..15], .little));
    try std.testing.expectEqual(@as(usize, 1), view.imports().len);
    try std.testing.expectEqualStrings("roc_alloc", view.name(view.imports()[0].name));
    try std.testing.expectEqual(@as(u32, 6), view.importSites(view.imports()[0])[0].offset);
    try std.testing.expectEqual(@as(usize, 2), view.localSites().len);
    try std.testing.expectEqual(@backingInt(Region.bss), view.localSites()[0].target_region);
    try std.testing.expectEqual(@backingInt(Region.data), view.localSites()[1].region);
    try std.testing.expectEqual(@as(i64, 0), view.localSites()[1].addend);
    try std.testing.expectEqual(@as(u32, 16), view.bssSize());
    try std.testing.expectEqual(@as(usize, 1), view.exports().len);
    try std.testing.expect(view.findExport("main") != null);
    try std.testing.expect(view.findExport("hidden_global") == null);
    try std.testing.expect(view.findExport("helper") == null);
    const again = try build(allocator, &.{object}, .{ .keep_debug = false });
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, image, again);
}

test "objects link to each other inside one image, strong over weak" {
    const allocator = std.testing.allocator;
    var text: [16]u8 = @splat(0x90);
    const first = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
    }, &.{
        .{ .name = "memcpy", .section = 1, .value = 0, .weak = true, .func = true },
        .{ .name = "caller", .section = 1, .value = 8, .global = true, .func = true },
        .{ .name = "provided", .section = 0, .global = true },
    }, &.{
        .{ .section = 1, .offset = 9, .symbol = 3, .type = R_X86_64_PLT32, .addend = -4 },
    });
    defer allocator.free(first);
    const second = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
    }, &.{
        .{ .name = "provided", .section = 1, .value = 4, .global = true, .func = true },
        .{ .name = "memcpy", .section = 0, .global = true },
    }, &.{
        .{ .section = 1, .offset = 1, .symbol = 2, .type = R_X86_64_PLT32, .addend = -4 },
    });
    defer allocator.free(second);
    const image = try build(allocator, &.{ first, second }, .{ .keep_debug = false });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqual(@as(usize, 0), view.imports().len);
    // first's call at 9 reaches `provided` at 16 + 4.
    try std.testing.expectEqual(@as(i32, 20 - 13), std.mem.readInt(i32, view.block()[9..13], .little));
    // second's call at 16 + 1 reaches the weak memcpy at 0.
    try std.testing.expectEqual(@as(i32, 0 - 21), std.mem.readInt(i32, view.block()[17..21], .little));
    try std.testing.expectEqual(@as(usize, 3), view.exports().len);
}

test "a corrupt image is rejected" {
    const allocator = std.testing.allocator;
    var text: [4]u8 = @splat(0xc3);
    const object = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
    }, &.{
        .{ .name = "f", .section = 1, .global = true, .func = true },
    }, &.{});
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = false });
    defer allocator.free(image);
    try std.testing.expectError(error.InvalidImage, View.init(image[0 .. image.len - 16]));
    image[0] = 'X';
    try std.testing.expectError(error.InvalidImage, View.init(image));
}
