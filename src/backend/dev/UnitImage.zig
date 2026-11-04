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
//! Images are built from relocatable objects (`build`): x86_64 ELF objects
//! for Linux, arm64 Mach-O objects for macOS, x86_64 and arm64 COFF objects
//! for Windows. A COFF object's unwind tables (`.pdata` entries and their
//! `.xdata`) are kept as regions of their own, which a link places in the
//! executable's exception directory. The inputs are a unit's
//! single-unit object, or the compiler's embedded support objects (the
//! builtins and the Boxy runtime), which form one image together. The
//! encoding is a pure function of the input bytes: fixed-width
//! little-endian tables in input order, names sorted where the reader
//! searches them. Two builds of the same input write identical bytes.
//!
//! Symbol names are the C names: a Mach-O object's leading underscore is
//! removed, so an image's imports and exports read the same on every
//! target. A COFF weak external that aliases a default definition is that
//! definition under the weak name.
//!
//! This file depends on `std` alone, so Roc's build graph can convert the
//! support objects with it.
const std = @import("std");
const Allocator = std.mem.Allocator;

const magic = "RBXU".*;
/// Format version; bump whenever the encoding or its contents change.
pub const format_version: u32 = 5;
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
    /// A COFF object's `.pdata`: the function table entries the
    /// executable's exception directory lists.
    pdata = 6,
    /// A COFF object's `.xdata`: the unwind information the entries name.
    xdata = 7,

    /// Whether the region is mapped into the executable's address space.
    /// A debug region's addresses are offsets in its output section.
    pub fn isAllocated(self: Region) bool {
        return switch (self) {
            .block, .data, .bss, .pdata, .xdata => true,
            .debug_line, .debug_info, .debug_abbrev => false,
        };
    }
};

/// How a patch site encodes its target.
pub const SiteKind = enum(u8) {
    /// 64-bit absolute address (`R_X86_64_64`, `ARM64_RELOC_UNSIGNED`).
    abs64 = 0,
    /// 32-bit displacement from the end of a 4-byte field
    /// (`R_X86_64_PC32`, `R_X86_64_PLT32`; the addend carries the -4).
    pc32 = 1,
    /// 32-bit zero-extended absolute value (`R_X86_64_32`, a 4-byte
    /// `ARM64_RELOC_UNSIGNED`).
    abs32 = 2,
    /// 32-bit sign-extended absolute value (`R_X86_64_32S`).
    abs32s = 3,
    /// The 26-bit word displacement of an arm64 `B` or `BL`
    /// (`ARM64_RELOC_BRANCH26`).
    branch26 = 4,
    /// The 21-bit page displacement of an arm64 `ADRP`
    /// (`ARM64_RELOC_PAGE21`).
    page21 = 5,
    /// The low 12 bits of the target in an arm64 `ADD` or load/store
    /// immediate, scaled by the access width (`ARM64_RELOC_PAGEOFF12`).
    pageoff12 = 6,
    /// A 32-bit address relative to the executable's image base
    /// (`IMAGE_REL_AMD64_ADDR32NB`, `IMAGE_REL_ARM64_ADDR32NB`); the link
    /// gives `applySite` the relative value.
    rva32 = 7,

    /// Whether the field encodes a distance from its own address.
    pub fn isPcRelative(self: SiteKind) bool {
        return switch (self) {
            .pc32, .branch26, .page21 => true,
            .abs64, .abs32, .abs32s, .pageoff12, .rva32 => false,
        };
    }
};

/// `Site.target_region` of a site whose target is an import.
pub const import_target: u8 = 0xff;

/// The type a symbol keeps in the output's symbol table.
pub const SymbolType = enum(u8) { notype = 0, object = 1, func = 2 };

/// `Symbol.flags` bit: other images may resolve imports to the symbol.
pub const symbol_exported: u8 = 1;
/// `Symbol.flags` and `Import.flags` bit: a weak definition or reference.
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
    /// The atom holding the field, for a field in the block; `no_atom`
    /// for a field the link always places.
    atom: u32,
    region: u8,
    kind: u8,
    /// A `Region`, or `import_target`.
    target_region: u8,
    reserved: u8 = 0,
};

/// `Site.atom` of a field outside the block.
pub const no_atom: u32 = std.math.maxInt(u32);

/// `edges()` entry bit marking an import index rather than an atom.
pub const edge_import: u32 = 1 << 31;

/// A code reference the object's assembly resolved in place, with no
/// relocation: the offsets, in the first object's text section, of the
/// referring field and of its target.
pub const CodeEdge = struct {
    site: u64,
    target: u64,
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
    pdata_offset: u32,
    pdata_size: u32,
    xdata_offset: u32,
    xdata_size: u32,
    /// The block's atoms: the start offset of each, ascending, the first
    /// at 0; an atom runs to the next one's start or the block's end.
    atoms_offset: u32,
    atom_count: u32,
    /// For each symbol, the atom holding it (`no_atom` outside the block).
    symbol_atoms_offset: u32,
    /// For each atom, the start of its outgoing edges, and one end entry.
    edge_starts_offset: u32,
    edges_offset: u32,
    edge_count: u32,
    /// What is reached whenever the image is placed: the targets of its
    /// writable data, encoded like `edges()` entries.
    roots_offset: u32,
    root_count: u32,
    total_size: u32,
    reserved: u32 = 0,
    /// BLAKE3 of the image's bytes with this field zeroed: what places the
    /// image in a link and identifies its content, whatever compiler or key
    /// produced it.
    digest: [32]u8,
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
        try checkRange(bytes.len, header.pdata_offset, header.pdata_size, 1);
        try checkRange(bytes.len, header.xdata_offset, header.xdata_size, 1);
        try checkRange(bytes.len, header.atoms_offset, header.atom_count, @sizeOf(u32));
        try checkRange(bytes.len, header.symbol_atoms_offset, header.symbol_count, @sizeOf(u32));
        try checkRange(bytes.len, header.edge_starts_offset, header.atom_count + 1, @sizeOf(u32));
        try checkRange(bytes.len, header.edges_offset, header.edge_count, @sizeOf(u32));
        try checkRange(bytes.len, header.roots_offset, header.root_count, @sizeOf(u32));
        if (header.atom_count == 0) return error.InvalidImage;
        if (header.text_size > header.block_size or header.import_site_count > header.site_count) return error.InvalidImage;
        if (header.strings_size == 0 or bytes[header.strings_offset + header.strings_size - 1] != 0) return error.InvalidImage;
        if (header.block_offset % page_size != 0 or header.symbols_offset % 8 != 0 or header.exports_offset % 8 != 0 or
            header.imports_offset % 8 != 0 or header.sites_offset % 8 != 0) return error.InvalidImage;
        return .{ .bytes = bytes, .header = header };
    }

    pub fn block(self: View) []const u8 {
        return self.bytes[self.header.block_offset..][0..self.header.block_size];
    }
    /// The image's content digest.
    pub fn digest(self: View) [32]u8 {
        return self.header.digest;
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
    /// The function table entries of a COFF image, empty for any other.
    pub fn pdata(self: View) []const u8 {
        return self.bytes[self.header.pdata_offset..][0..self.header.pdata_size];
    }
    /// The unwind information of a COFF image, empty for any other.
    pub fn xdata(self: View) []const u8 {
        return self.bytes[self.header.xdata_offset..][0..self.header.xdata_size];
    }
    /// The start offsets of the block's atoms, ascending.
    pub fn atoms(self: View) []const u32 {
        return table(u32, self.bytes, self.header.atoms_offset, self.header.atom_count);
    }
    /// The block range of atom `index`.
    pub fn atomRange(self: View, index: u32) struct { start: u32, end: u32 } {
        const starts = self.atoms();
        return .{ .start = starts[index], .end = if (index + 1 < starts.len) starts[index + 1] else self.header.block_size };
    }
    /// For each symbol, the atom holding it.
    pub fn symbolAtoms(self: View) []const u32 {
        return table(u32, self.bytes, self.header.symbol_atoms_offset, self.header.symbol_count);
    }
    /// The outgoing edges of atom `index`: atom indices, and import indices
    /// marked with `edge_import`.
    pub fn atomEdges(self: View, index: u32) []const u32 {
        const starts = table(u32, self.bytes, self.header.edge_starts_offset, self.header.atom_count + 1);
        return table(u32, self.bytes, self.header.edges_offset, self.header.edge_count)[starts[index]..starts[index + 1]];
    }
    /// What is reached whenever the image is placed, encoded like
    /// `atomEdges` entries.
    pub fn roots(self: View) []const u32 {
        return table(u32, self.bytes, self.header.roots_offset, self.header.root_count);
    }

    pub fn regionBytes(self: View, region: Region) []const u8 {
        return switch (region) {
            .block => self.block(),
            .data => self.data(),
            .bss => &.{},
            .debug_line => self.debugLine(),
            .debug_info => self.debugInfo(),
            .debug_abbrev => self.debugAbbrev(),
            .pdata => self.pdata(),
            .xdata => self.xdata(),
        };
    }

    /// The exported symbol named `wanted`, if the image exports one.
    pub fn findExport(self: View, wanted: []const u8) ?Symbol {
        return self.symbols()[self.findExportIndex(wanted) orelse return null];
    }

    /// The index in `symbols` of the export named `wanted`, if any.
    pub fn findExportIndex(self: View, wanted: []const u8) ?u32 {
        const all = self.symbols();
        const order = self.exports();
        var low: usize = 0;
        var high: usize = order.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            switch (std.mem.order(u8, self.name(all[order[mid]].name), wanted)) {
                .lt => low = mid + 1,
                .gt => high = mid,
                .eq => return order[mid],
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
/// backend's own object writers and the compiler's embedded support objects
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

/// How `build` treats its inputs.
pub const BuildOptions = struct {
    /// Keep the line, info and abbrev debug sections. Without it, every
    /// debug section is dropped.
    keep_debug: bool,
    /// How the block divides into atoms, the pieces a link places only when
    /// something it places reaches them.
    atoms: union(enum) {
        /// One atom: the image is placed whole.
        whole,
        /// One atom per symbol in the block, with every reference between
        /// them a relocation or one of these patched code references.
        symbols: []const CodeEdge,
    },
};

/// Build one image from `objects`, linking them to each other: a symbol one
/// object references and another defines globally resolves inside the
/// image, a strong definition taking precedence over a weak one. What no
/// object defines becomes an import.
///
/// A load through the global offset table (`ARM64_RELOC_GOT_LOAD_*`) reads a
/// slot the image keeps in its writable data, one per referenced symbol,
/// holding the symbol's absolute address.
pub fn build(allocator: Allocator, objects: []const []const u8, options: BuildOptions) BuildError![]align(alignment.toByteUnits()) u8 {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const parsed = try arena.alloc(Object, objects.len);
    for (objects, parsed) |bytes, *object| object.* = try Object.parse(arena, bytes, options);

    // The symbols read through the global offset table, each owning one
    // slot: a global or undefined symbol by name, a local by its object and
    // index.
    var got_slots: std.StringArrayHashMapUnmanaged(SymbolRef) = .empty;
    for (parsed, 0..) |object, object_index| {
        for (object.relocs) |reloc| {
            if (reloc.kind != .got_load_page21 and reloc.kind != .got_load_pageoff12) continue;
            const symbol = object.symbols[reloc.symbol];
            const key = if (symbol.bind == .local) try std.fmt.allocPrint(arena, "\x00{d}:{d}", .{ object_index, reloc.symbol }) else symbol.name;
            const gop = try got_slots.getOrPut(arena, key);
            if (!gop.found_existing) gop.value_ptr.* = .{ .object = @intCast(object_index), .symbol = reloc.symbol };
        }
    }

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
    var unwind_sizes = [_]u64{ 0, 0 };
    for (parsed, places) |object, object_places| {
        for (object.sections, 0..) |section, index| {
            if (section.class != .text) continue;
            block_align = @max(block_align, section.addralign);
            block_size = std.mem.alignForward(u64, block_size, section.addralign);
            object_places[index] = .{ .region = .block, .offset = block_size };
            block_size += section.size;
        }
    }
    const text_size = block_size;
    for (parsed, places) |object, object_places| {
        for (object.sections, 0..) |section, index| {
            switch (section.class) {
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
                .debug_line => placeDebug(&object_places[index], &debug_sizes[0], .debug_line, section.size),
                .debug_info => placeDebug(&object_places[index], &debug_sizes[1], .debug_info, section.size),
                .debug_abbrev => placeDebug(&object_places[index], &debug_sizes[2], .debug_abbrev, section.size),
                .pdata => placeAligned(&object_places[index], &unwind_sizes[0], .pdata, section.size, 4),
                .xdata => placeAligned(&object_places[index], &unwind_sizes[1], .xdata, section.size, section.addralign),
            }
        }
    }
    // The global offset table slots end the writable data.
    var got_offset: u64 = 0;
    if (got_slots.count() != 0) {
        data_align = @max(data_align, 8);
        data_size = std.mem.alignForward(u64, data_size, 8);
        got_offset = data_size;
        data_size += got_slots.count() * 8;
    }
    if (block_align > page_size or data_align > page_size or bss_align > page_size) return error.UnsupportedSection;
    const limit = std.math.maxInt(u32);
    if (block_size > limit or data_size > limit or bss_size > limit or debug_sizes[0] > limit or debug_sizes[1] > limit or debug_sizes[2] > limit) return error.ImageTooLarge;
    if (unwind_sizes[0] > limit or unwind_sizes[1] > limit) return error.ImageTooLarge;

    const block_bytes = try arena.alloc(u8, @intCast(block_size));
    @memset(block_bytes, 0);
    const data_bytes = try arena.alloc(u8, @intCast(data_size));
    @memset(data_bytes, 0);
    var debug_bytes: [3][]u8 = undefined;
    for (&debug_bytes, debug_sizes) |*slot, size| {
        slot.* = try arena.alloc(u8, @intCast(size));
        @memset(slot.*, 0);
    }
    var unwind_bytes: [2][]u8 = undefined;
    for (&unwind_bytes, unwind_sizes) |*slot, size| {
        slot.* = try arena.alloc(u8, @intCast(size));
        @memset(slot.*, 0);
    }
    for (parsed, places) |object, object_places| {
        for (object.sections, object_places) |section, place_or_null| {
            const place = place_or_null orelse continue;
            if (section.class == .bss) continue;
            const destination: []u8 = switch (place.region) {
                .block => block_bytes,
                .data => data_bytes,
                .bss => unreachable, // only zero-fill sections are placed there, and they have no bytes
                .debug_line => debug_bytes[0],
                .debug_info => debug_bytes[1],
                .debug_abbrev => debug_bytes[2],
                .pdata => unwind_bytes[0],
                .xdata => unwind_bytes[1],
            };
            @memcpy(destination[@intCast(place.offset)..][0..section.bytes.len], section.bytes);
        }
    }

    // Global definitions across the objects: a strong one wins over weak
    // ones, the first weak one wins among weak ones.
    var definitions: std.StringArrayHashMapUnmanaged(Definition) = .empty;
    for (parsed, places, 0..) |object, object_places, object_index| {
        for (object.symbols, 0..) |symbol, symbol_index| {
            if (symbol.skip or symbol.bind == .local) continue;
            const section = symbol.section orelse continue;
            const place = object_places[section] orelse return error.UnsupportedSymbol;
            const weak = symbol.bind == .weak;
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
            if (symbol.skip or symbol.kind == .section) continue;
            const section = symbol.section orelse continue;
            const place = object_places[section] orelse continue;
            if (!place.region.isAllocated()) continue;
            if (symbol.name.len == 0) continue;
            const is_global = symbol.bind != .local;
            if (is_global) {
                const winner = definitions.get(symbol.name).?;
                if (winner.object != object_index or winner.symbol != symbol_index) continue;
            }
            const visible = is_global and !symbol.hidden;
            const symbol_type: SymbolType = switch (symbol.kind) {
                .func => .func,
                .object => .object,
                .notype => .notype,
                .section => unreachable, // skipped above
            };
            const index: u32 = @intCast(symbol_records.items.len);
            try symbol_records.append(arena, .{
                .name = try strings.add(symbol.name),
                .value = @intCast(place.offset + symbol.value),
                .size = std.math.cast(u32, symbol.size) orelse return error.ImageTooLarge,
                .region = @backingInt(place.region),
                .type = @backingInt(symbol_type),
                .flags = (if (visible) symbol_exported else 0) | (if (symbol.bind == .weak) symbol_weak else 0),
            });
            if (visible) try exported.append(arena, index);
        }
    }

    // Atoms: the whole block, or one per symbol start in it.
    var atom_starts: std.ArrayList(u32) = .empty;
    try atom_starts.append(arena, 0);
    switch (options.atoms) {
        .whole => {},
        .symbols => {
            for (symbol_records.items) |symbol| {
                if (symbol.region == @backingInt(Region.block)) try atom_starts.append(arena, symbol.value);
            }
            std.mem.sort(u32, atom_starts.items, {}, std.sort.asc(u32));
            var kept: usize = 1;
            for (atom_starts.items[1..]) |start| {
                if (start == atom_starts.items[kept - 1]) continue;
                atom_starts.items[kept] = start;
                kept += 1;
            }
            atom_starts.shrinkRetainingCapacity(kept);
        },
    }
    var sites = Sites{
        .arena = arena,
        .options = options,
        .atom_list = .{ .starts = atom_starts.items },
        .block_bytes = block_bytes,
    };
    switch (options.atoms) {
        .whole => {},
        .symbols => |code_edges| {
            const text_index = parsed[0].text_section orelse return error.InvalidObject;
            const text_place = places[0][text_index] orelse return error.InvalidObject;
            for (code_edges) |edge| {
                try sites.edge_pairs.append(arena, .{ .from = sites.atom_list.of(text_place.offset + edge.site), .to = sites.atom_list.of(text_place.offset + edge.target) });
            }
        },
    }

    // Relocations: resolve what is position independent inside the block,
    // keep the rest as sites.
    for (parsed, places, 0..) |object, object_places, object_index| {
        for (object.relocs) |reloc| {
            const site_place = object_places[reloc.section] orelse continue;
            const site_section = object.sections[reloc.section];
            const symbol = object.symbols[reloc.symbol];
            const target = try resolveTarget(&definitions, object_places, symbol);
            switch (reloc.kind) {
                .got_load_page21, .got_load_pageoff12 => {
                    // The site reads the slot; what the slot holds is what
                    // the site's atom reaches.
                    const kind: SiteKind = if (reloc.kind == .got_load_page21) .page21 else .pageoff12;
                    if (reloc.offset + kindWidth(kind) > site_section.size) return error.InvalidObject;
                    const site_offset = site_place.offset + reloc.offset;
                    const key = if (symbol.bind == .local) try std.fmt.allocPrint(arena, "\x00{d}:{d}", .{ object_index, reloc.symbol }) else symbol.name;
                    const slot = got_slots.getIndex(key).?;
                    const slot_definition: Definition = .{ .object = 0, .symbol = 0, .weak = false, .region = .data, .offset = got_offset + slot * 8 };
                    try sites.noteEdge(site_place, site_offset, target, symbol);
                    try sites.add(site_place, site_offset, kind, slot_definition, "", false, reloc.addend);
                },
                .abs64, .pc32, .abs32, .abs32s, .branch26, .page21, .pageoff12, .rva32 => {
                    const kind = reloc.kind.siteKind();
                    if (reloc.offset + kindWidth(kind) > site_section.size) return error.InvalidObject;
                    const site_offset = site_place.offset + reloc.offset;
                    try sites.noteEdge(site_place, site_offset, target, symbol);
                    try sites.add(site_place, site_offset, kind, target, symbol.name, symbol.bind == .weak, reloc.addend);
                },
            }
        }
    }
    // The slots themselves: absolute addresses in the writable data.
    const slot_place: Place = .{ .region = .data, .offset = got_offset };
    for (got_slots.values(), 0..) |ref, slot| {
        const object = parsed[ref.object];
        const symbol = object.symbols[ref.symbol];
        const target = try resolveTarget(&definitions, places[ref.object], symbol);
        const site_offset = got_offset + slot * 8;
        try sites.noteEdge(slot_place, site_offset, target, symbol);
        try sites.add(slot_place, site_offset, .abs64, target, symbol.name, symbol.bind == .weak, 0);
    }

    // Imports in name order, each with its sites in the order found.
    const import_index = &sites.import_index;
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
    for (sites.import_sites.items) |pending| site_counts[rank[pending.import]] += 1;
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
    const all_sites = try arena.alloc(Site, sites.import_sites.items.len + sites.local_sites.items.len);
    const fill = try arena.alloc(u32, import_count);
    @memset(fill, 0);
    for (sites.import_sites.items) |pending| {
        const sorted = rank[pending.import];
        var site = pending.site;
        site.target = sorted;
        all_sites[import_records[sorted].first_site + fill[sorted]] = site;
        fill[sorted] += 1;
    }
    @memcpy(all_sites[sites.import_sites.items.len..], sites.local_sites.items);

    // Edges by atom, imports by their sorted index, without repeats.
    const atom_list = sites.atom_list;
    for (sites.edge_pairs.items) |*pair| {
        if (pair.to & edge_import != 0) pair.to = edge_import | rank[pair.to & ~edge_import];
    }
    std.mem.sort(EdgePair, sites.edge_pairs.items, {}, EdgePair.lessThan);
    var edge_targets: std.ArrayList(u32) = .empty;
    const edge_starts = try arena.alloc(u32, atom_list.starts.len + 1);
    var pair_at: usize = 0;
    for (0..atom_list.starts.len) |atom| {
        edge_starts[atom] = @intCast(edge_targets.items.len);
        var previous: ?u32 = null;
        while (pair_at < sites.edge_pairs.items.len and sites.edge_pairs.items[pair_at].from == atom) : (pair_at += 1) {
            const to = sites.edge_pairs.items[pair_at].to;
            if (previous != null and previous.? == to) continue;
            if (to == atom) continue;
            try edge_targets.append(arena, to);
            previous = to;
        }
    }
    edge_starts[atom_list.starts.len] = @intCast(edge_targets.items.len);
    const root_atoms = &sites.root_atoms;
    for (root_atoms.items) |*root| {
        if (root.* & edge_import != 0) root.* = edge_import | rank[root.* & ~edge_import];
    }
    std.mem.sort(u32, root_atoms.items, {}, std.sort.asc(u32));
    var roots_kept: usize = 0;
    for (root_atoms.items) |root| {
        if (roots_kept != 0 and root_atoms.items[roots_kept - 1] == root) continue;
        root_atoms.items[roots_kept] = root;
        roots_kept += 1;
    }
    root_atoms.shrinkRetainingCapacity(roots_kept);
    const symbol_atoms = try arena.alloc(u32, symbol_records.items.len);
    for (symbol_records.items, symbol_atoms) |symbol, *atom| {
        atom.* = if (symbol.region == @backingInt(Region.block)) atom_list.of(symbol.value) else no_atom;
    }

    // Exports in name order.
    std.mem.sort(u32, exported.items, Exports{ .symbols = symbol_records.items, .strings = &strings }, Exports.lessThan);

    // Encode.
    var layout = Layout{};
    // The header begins the image.
    const header_offset = layout.reserve(@sizeOf(Header), 8);
    std.debug.assert(header_offset == 0);
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
    const pdata_offset = layout.reserve(unwind_bytes[0].len, 8);
    const xdata_offset = layout.reserve(unwind_bytes[1].len, 8);
    const atoms_offset = layout.reserve(atom_list.starts.len * @sizeOf(u32), 8);
    const symbol_atoms_offset = layout.reserve(symbol_atoms.len * @sizeOf(u32), 8);
    const edge_starts_offset = layout.reserve(edge_starts.len * @sizeOf(u32), 8);
    const edges_offset = layout.reserve(edge_targets.items.len * @sizeOf(u32), 8);
    const roots_offset = layout.reserve(root_atoms.items.len * @sizeOf(u32), 8);
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
        .import_site_count = @intCast(sites.import_sites.items.len),
        .strings_offset = strings_offset,
        .strings_size = @intCast(strings.bytes.items.len),
        .debug_line_offset = debug_line_offset,
        .debug_line_size = @intCast(debug_bytes[0].len),
        .debug_info_offset = debug_info_offset,
        .debug_info_size = @intCast(debug_bytes[1].len),
        .debug_abbrev_offset = debug_abbrev_offset,
        .debug_abbrev_size = @intCast(debug_bytes[2].len),
        .pdata_offset = pdata_offset,
        .pdata_size = @intCast(unwind_bytes[0].len),
        .xdata_offset = xdata_offset,
        .xdata_size = @intCast(unwind_bytes[1].len),
        .atoms_offset = atoms_offset,
        .atom_count = @intCast(atom_list.starts.len),
        .symbol_atoms_offset = symbol_atoms_offset,
        .edge_starts_offset = edge_starts_offset,
        .edges_offset = edges_offset,
        .edge_count = @intCast(edge_targets.items.len),
        .roots_offset = roots_offset,
        .root_count = @intCast(root_atoms.items.len),
        .total_size = @intCast(total),
        .digest = @splat(0),
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
    @memcpy(out[pdata_offset..][0..unwind_bytes[0].len], unwind_bytes[0]);
    @memcpy(out[xdata_offset..][0..unwind_bytes[1].len], unwind_bytes[1]);
    @memcpy(out[atoms_offset..][0 .. atom_list.starts.len * @sizeOf(u32)], std.mem.sliceAsBytes(atom_list.starts));
    @memcpy(out[symbol_atoms_offset..][0 .. symbol_atoms.len * @sizeOf(u32)], std.mem.sliceAsBytes(symbol_atoms));
    @memcpy(out[edge_starts_offset..][0 .. edge_starts.len * @sizeOf(u32)], std.mem.sliceAsBytes(edge_starts));
    @memcpy(out[edges_offset..][0 .. edge_targets.items.len * @sizeOf(u32)], std.mem.sliceAsBytes(edge_targets.items));
    @memcpy(out[roots_offset..][0 .. root_atoms.items.len * @sizeOf(u32)], std.mem.sliceAsBytes(root_atoms.items));
    var content_digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(out, &content_digest, .{});
    @memcpy(out[@offsetOf(Header, "digest")..][0..32], &content_digest);
    return out;
}

/// The width in bytes of a site's field.
pub fn kindWidth(kind: SiteKind) u64 {
    return switch (kind) {
        .abs64 => 8,
        .pc32, .abs32, .abs32s, .branch26, .page21, .pageoff12, .rva32 => 4,
    };
}

/// Write `target + addend` into `field` as `kind` encodes it, where the
/// field sits at address `site_address` (used by the pc-relative kinds).
/// For `rva32` the value is the target's image-relative address. Returns
/// false when the value does not fit the field.
pub fn applySite(field: []u8, kind: SiteKind, target_plus_addend: i64, site_address: u64) bool {
    switch (kind) {
        .abs64 => std.mem.writeInt(u64, field[0..8], @bitCast(target_plus_addend), .little),
        .rva32 => {
            const narrow = std.math.cast(u32, target_plus_addend) orelse return false;
            std.mem.writeInt(u32, field[0..4], narrow, .little);
        },
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
        .branch26 => {
            const value = target_plus_addend - @as(i64, @intCast(site_address));
            if (value & 3 != 0) return false;
            const word_delta = std.math.cast(i26, value >> 2) orelse return false;
            const instruction = std.mem.readInt(u32, field[0..4], .little);
            std.mem.writeInt(u32, field[0..4], (instruction & 0xfc00_0000) | @as(u32, @as(u26, @bitCast(word_delta))), .little);
        },
        .page21 => {
            const target_page = target_plus_addend & ~@as(i64, 0xfff);
            const site_page: i64 = @intCast(site_address & ~@as(u64, 0xfff));
            const pages = std.math.cast(i21, (target_page - site_page) >> 12) orelse return false;
            const imm: u32 = @as(u21, @bitCast(pages));
            var instruction = std.mem.readInt(u32, field[0..4], .little);
            instruction &= ~(@as(u32, 0b11) << 29);
            instruction &= ~(@as(u32, 0x7ffff) << 5);
            instruction |= (imm & 0b11) << 29;
            instruction |= (imm >> 2) << 5;
            std.mem.writeInt(u32, field[0..4], instruction, .little);
        },
        .pageoff12 => {
            const low: u32 = @intCast(target_plus_addend & 0xfff);
            var instruction = std.mem.readInt(u32, field[0..4], .little);
            const shift = pageOffsetShift(instruction) orelse return false;
            if (low & ((@as(u32, 1) << shift) - 1) != 0) return false;
            instruction &= ~(@as(u32, 0xfff) << 10);
            instruction |= (low >> shift) << 10;
            std.mem.writeInt(u32, field[0..4], instruction, .little);
        },
    }
    return true;
}

/// How many bits an arm64 instruction's 12-bit immediate is scaled by: an
/// `ADD` takes the offset as is, a load or store scales it by its access
/// width. Null for an instruction a page offset cannot patch.
fn pageOffsetShift(instruction: u32) ?u5 {
    // ADD (immediate), 32- or 64-bit, unshifted.
    if (instruction & 0x7f80_0000 == 0x1100_0000) return 0;
    // Load/store register (unsigned immediate).
    if (instruction & 0x3b00_0000 == 0x3900_0000) {
        const size: u5 = @intCast(instruction >> 30);
        const vector = instruction & (1 << 26) != 0;
        const opc_high = instruction & (1 << 23) != 0;
        if (vector and opc_high and size == 0) return 4;
        return size;
    }
    return null;
}

const Place = struct { region: Region, offset: u64 };

/// The block's atom starts, ascending, the first at 0.
const Atoms = struct {
    starts: []const u32,

    /// The atom holding block offset `offset`.
    fn of(self: Atoms, offset: u64) u32 {
        var low: usize = 0;
        var high: usize = self.starts.len;
        while (high - low > 1) {
            const mid = low + (high - low) / 2;
            if (self.starts[mid] <= offset) low = mid else high = mid;
        }
        return @intCast(low);
    }
};

const EdgePair = struct {
    from: u32,
    to: u32,

    fn lessThan(_: void, a: EdgePair, b: EdgePair) bool {
        if (a.from != b.from) return a.from < b.from;
        return a.to < b.to;
    }
};

fn placeDebug(place: *?Place, size: *u64, region: Region, section_size: u64) void {
    place.* = .{ .region = region, .offset = size.* };
    size.* += section_size;
}

fn placeAligned(place: *?Place, size: *u64, region: Region, section_size: u64, addralign: u64) void {
    size.* = std.mem.alignForward(u64, size.*, addralign);
    placeDebug(place, size, region, section_size);
}

const Definition = struct {
    object: u32,
    symbol: u32,
    weak: bool,
    region: Region,
    offset: u64,
};

const SymbolRef = struct { object: u32, symbol: u32 };

/// What a relocation's symbol resolves to inside the image: a definition,
/// or null for an import.
fn resolveTarget(definitions: *const std.StringArrayHashMapUnmanaged(Definition), object_places: []const ?Place, symbol: ObjectSymbol) BuildError!?Definition {
    const section = symbol.section orelse return definitions.get(symbol.name);
    if (symbol.bind != .local) return definitions.get(symbol.name).?;
    const place = object_places[section] orelse return error.UnsupportedRelocation;
    return .{ .object = 0, .symbol = 0, .weak = false, .region = place.region, .offset = place.offset + symbol.value };
}

const PendingImport = struct { weak: bool };
const PendingSite = struct { import: u32, site: Site };

/// The sites and edges `build` collects.
const Sites = struct {
    arena: Allocator,
    options: BuildOptions,
    atom_list: Atoms,
    block_bytes: []u8,
    import_index: std.StringArrayHashMapUnmanaged(PendingImport) = .empty,
    import_sites: std.ArrayList(PendingSite) = .empty,
    local_sites: std.ArrayList(Site) = .empty,
    edge_pairs: std.ArrayList(EdgePair) = .empty,
    root_atoms: std.ArrayList(u32) = .empty,

    /// What the field's atom reaches. A field in writable data is always
    /// placed, so what it reaches is a root. A reference through a section
    /// symbol names no symbol whose atom it reaches, so atoms by symbol
    /// admit none from the block.
    fn noteEdge(self: *Sites, site_place: Place, site_offset: u64, target: ?Definition, symbol: ObjectSymbol) BuildError!void {
        const site_atom: u32 = if (site_place.region == .block) self.atom_list.of(site_offset) else no_atom;
        if (target) |local| {
            if (local.region == .block and self.options.atoms != .whole) {
                if (symbol.kind == .section and site_place.region == .block) return error.UnsupportedRelocation;
                const to = self.atom_list.of(local.offset);
                switch (site_place.region) {
                    .block => try self.edge_pairs.append(self.arena, .{ .from = site_atom, .to = to }),
                    .data => try self.root_atoms.append(self.arena, to),
                    // An unwind entry names its function without reaching
                    // it: a link keeps the entries of the atoms it places.
                    .bss, .debug_line, .debug_info, .debug_abbrev, .pdata, .xdata => {},
                }
            }
        } else if (site_place.region.isAllocated()) {
            const gop = try self.import_index.getOrPut(self.arena, symbol.name);
            if (!gop.found_existing) gop.value_ptr.* = .{ .weak = true };
            const to = edge_import | @as(u32, @intCast(gop.index));
            if (site_place.region == .block) try self.edge_pairs.append(self.arena, .{ .from = site_atom, .to = to }) else try self.root_atoms.append(self.arena, to);
        }
    }

    /// Resolve the field in place when its target is in the block with it,
    /// else record it as a local or import site.
    fn add(self: *Sites, site_place: Place, site_offset: u64, kind: SiteKind, target: ?Definition, symbol_name: []const u8, weak_reference: bool, addend: i64) BuildError!void {
        const site_atom: u32 = if (site_place.region == .block) self.atom_list.of(site_offset) else no_atom;
        if (target) |local| {
            const total = addend + @as(i64, @intCast(local.offset));
            const relative = kind.isPcRelative() or kind == .pageoff12;
            if (relative and site_place.region == .block and local.region == .block) {
                // The block is placed page aligned, so page and low-bit
                // arithmetic over block offsets holds wherever it lands.
                const field = self.block_bytes[@intCast(site_offset)..][0..@intCast(kindWidth(kind))];
                if (!applySite(field, kind, total, site_offset)) return error.RelocationOutOfRange;
                return;
            }
            if (relative and !(site_place.region.isAllocated() and local.region.isAllocated())) return error.UnsupportedRelocation;
            try self.local_sites.append(self.arena, .{
                .offset = @intCast(site_offset),
                .target = 0,
                .addend = total,
                .atom = site_atom,
                .region = @backingInt(site_place.region),
                .kind = @backingInt(kind),
                .target_region = @backingInt(local.region),
            });
            return;
        }
        if (!site_place.region.isAllocated()) return error.UnsupportedRelocation;
        const gop = try self.import_index.getOrPut(self.arena, symbol_name);
        if (!gop.found_existing) gop.value_ptr.* = .{ .weak = true };
        if (!weak_reference) gop.value_ptr.weak = false;
        try self.import_sites.append(self.arena, .{ .import = @intCast(gop.index), .site = .{
            .offset = @intCast(site_offset),
            .target = 0,
            .addend = addend,
            .atom = site_atom,
            .region = @backingInt(site_place.region),
            .kind = @backingInt(kind),
            .target_region = import_target,
        } });
    }
};

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

/// What a section of an input object is to the image.
const SectionClass = enum {
    skip,
    text,
    rodata,
    data,
    bss,
    debug_line,
    debug_info,
    debug_abbrev,
    pdata,
    xdata,

    /// Whether the section's bytes are mapped into the executable.
    fn isAllocated(self: SectionClass) bool {
        return switch (self) {
            .text, .rodata, .data, .bss, .pdata, .xdata => true,
            .skip, .debug_line, .debug_info, .debug_abbrev => false,
        };
    }
};

/// An input object's section, classified.
const Section = struct {
    class: SectionClass,
    /// The section's contents; empty for zero-fill.
    bytes: []const u8,
    size: u64,
    addralign: u64,
};

const SymbolBind = enum { local, global, weak };
const SymbolKind = enum { notype, object, func, section };

/// An input object's symbol, in its symbol table's order so relocations
/// can index it.
const ObjectSymbol = struct {
    /// The C name: a Mach-O name without its leading underscore.
    name: []const u8,
    /// Offset in the symbol's section.
    value: u64,
    size: u64,
    /// Index of the section, or null for an undefined symbol.
    section: ?u32,
    bind: SymbolBind,
    kind: SymbolKind,
    /// A definition other images cannot resolve imports to.
    hidden: bool,
    /// An entry that is not a symbol: an ELF null or file symbol, a Mach-O
    /// debugging stab.
    skip: bool,
};

/// How a relocation's field encodes its target: a site kind, or a load
/// through the global offset table.
const RelocKind = enum {
    abs64,
    pc32,
    abs32,
    abs32s,
    branch26,
    page21,
    pageoff12,
    rva32,
    got_load_page21,
    got_load_pageoff12,

    fn siteKind(self: RelocKind) SiteKind {
        return switch (self) {
            .abs64 => .abs64,
            .pc32 => .pc32,
            .abs32 => .abs32,
            .abs32s => .abs32s,
            .branch26 => .branch26,
            .page21 => .page21,
            .pageoff12 => .pageoff12,
            .rva32 => .rva32,
            .got_load_page21, .got_load_pageoff12 => unreachable, // read through a slot, never a site of their own
        };
    }
};

const Reloc = struct {
    /// Index of the section the field is in.
    section: u32,
    /// Offset of the field in that section.
    offset: u64,
    kind: RelocKind,
    /// Index into the object's symbols.
    symbol: u32,
    addend: i64,
};

/// An input object in the form `build` consumes, whatever format it came
/// in.
const Object = struct {
    sections: []Section,
    symbols: []ObjectSymbol,
    relocs: []Reloc,
    /// The index of the object's text section, which code edges are
    /// offsets in.
    text_section: ?u32,

    fn parse(arena: Allocator, bytes: []const u8, options: BuildOptions) BuildError!Object {
        if (bytes.len < 4) return error.InvalidObject;
        if (std.mem.eql(u8, bytes[0..4], "\x7fELF")) return parseElf(arena, bytes, options);
        if (readU32(bytes, 0) == MH_MAGIC_64) return parseMachO(arena, bytes, options);
        const machine = readU16(bytes, 0);
        if (machine == IMAGE_FILE_MACHINE_AMD64 or machine == IMAGE_FILE_MACHINE_ARM64) return parseCoff(arena, bytes, options);
        return error.InvalidObject;
    }
};

fn parseElf(arena: Allocator, bytes: []const u8, options: BuildOptions) BuildError!Object {
    if (bytes.len < 64) return error.InvalidObject;
    if (bytes[4] != 2 or bytes[5] != 1) return error.InvalidObject;
    if (readU16(bytes, 16) != ET_REL or readU16(bytes, 18) != EM_X86_64) return error.InvalidObject;
    const shoff = readU64(bytes, 40);
    const shentsize = readU16(bytes, 58);
    const shnum = readU16(bytes, 60);
    const shstrndx = readU16(bytes, 62);
    if (shentsize != 64 or shnum == 0 or shstrndx >= shnum) return error.InvalidObject;
    if (shoff > bytes.len or (bytes.len - shoff) / 64 < shnum) return error.InvalidObject;
    const ElfSection = struct { name: []const u8, type: u32, flags: u64, offset: u64, size: u64, link: u32, info: u32, addralign: u64 };
    const raw = try arena.alloc(ElfSection, shnum);
    for (raw, 0..) |*section, index| {
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
    const shstrtab = raw[shstrndx];
    const names = bytes[@intCast(shstrtab.offset)..][0..@intCast(shstrtab.size)];
    for (raw, 0..) |*section, index| {
        const at: usize = @intCast(shoff + index * 64);
        const name_offset = readU32(bytes, at);
        if (name_offset >= names.len) return error.InvalidObject;
        section.name = std.mem.sliceTo(names[name_offset..], 0);
    }
    const sections = try arena.alloc(Section, shnum);
    var text_section: ?u32 = null;
    for (raw, sections, 0..) |section, *out, index| {
        const class = try classifyElf(section.name, section.type, section.flags, section.addralign, options);
        if (class == .text and text_section == null and std.mem.eql(u8, section.name, ".text")) text_section = @intCast(index);
        out.* = .{
            .class = class,
            .bytes = if (section.type == SHT_NOBITS or section.type == SHT_NULL) &.{} else bytes[@intCast(section.offset)..][0..@intCast(section.size)],
            .size = section.size,
            .addralign = section.addralign,
        };
    }
    var symbols: []ObjectSymbol = &.{};
    var symtab_seen = false;
    for (raw) |section| {
        if (section.type != SHT_SYMTAB) continue;
        if (symtab_seen) return error.InvalidObject;
        symtab_seen = true;
        if (section.link >= raw.len) return error.InvalidObject;
        const strtab = raw[section.link];
        const symbol_names = bytes[@intCast(strtab.offset)..][0..@intCast(strtab.size)];
        const entries = bytes[@intCast(section.offset)..][0..@intCast(section.size)];
        if (entries.len % 24 != 0) return error.InvalidObject;
        symbols = try arena.alloc(ObjectSymbol, entries.len / 24);
        for (symbols, 0..) |*symbol, index| {
            const at = index * 24;
            const name_offset = readU32(entries, at);
            if (name_offset >= symbol_names.len and name_offset != 0) return error.InvalidObject;
            const info = entries[at + 4];
            const bind: u4 = @truncate(info >> 4);
            const symbol_type: u4 = @truncate(info);
            const visibility: u2 = @truncate(entries[at + 5]);
            const shndx = readU16(entries, at + 6);
            if (bind != STB_LOCAL and bind != STB_GLOBAL and bind != STB_WEAK) return error.UnsupportedSymbol;
            if (shndx == SHN_COMMON or (shndx == SHN_ABS and symbol_type != STT_FILE)) return error.UnsupportedSymbol;
            if (shndx != SHN_UNDEF and shndx < SHN_LORESERVE and shndx >= raw.len) return error.InvalidObject;
            if (symbol_type != STT_NOTYPE and symbol_type != STT_OBJECT and symbol_type != STT_FUNC and symbol_type != STT_SECTION and symbol_type != STT_FILE) return error.UnsupportedSymbol;
            const skip = index == 0 or symbol_type == STT_FILE;
            symbol.* = .{
                .name = if (symbol_names.len == 0) "" else std.mem.sliceTo(symbol_names[name_offset..], 0),
                .value = readU64(entries, at + 8),
                .size = readU64(entries, at + 16),
                .section = if (shndx == SHN_UNDEF or skip) null else if (shndx >= SHN_LORESERVE) return error.UnsupportedSymbol else shndx,
                .bind = if (bind == STB_LOCAL) .local else if (bind == STB_GLOBAL) .global else .weak,
                .kind = if (symbol_type == STT_OBJECT)
                    .object
                else if (symbol_type == STT_FUNC)
                    .func
                else if (symbol_type == STT_SECTION)
                    .section
                else
                    .notype,
                .hidden = visibility != STV_DEFAULT and visibility != STV_PROTECTED,
                .skip = skip,
            };
        }
    }
    var relocs: std.ArrayList(Reloc) = .empty;
    for (raw) |section| {
        if (section.type != SHT_RELA) continue;
        if (section.info >= raw.len) return error.InvalidObject;
        const entries = bytes[@intCast(section.offset)..][0..@intCast(section.size)];
        if (entries.len % 24 != 0) return error.InvalidObject;
        var at: usize = 0;
        while (at < entries.len) : (at += 24) {
            const r_info = readU64(entries, at + 8);
            const r_sym: u32 = @truncate(r_info >> 32);
            const r_type: u32 = @truncate(r_info);
            if (r_sym >= symbols.len) return error.InvalidObject;
            try relocs.append(arena, .{
                .section = section.info,
                .offset = readU64(entries, at),
                .kind = if (r_type == R_X86_64_64)
                    .abs64
                else if (r_type == R_X86_64_PC32 or r_type == R_X86_64_PLT32)
                    .pc32
                else if (r_type == R_X86_64_32)
                    .abs32
                else if (r_type == R_X86_64_32S)
                    .abs32s
                else
                    return error.UnsupportedRelocation,
                .symbol = r_sym,
                .addend = @bitCast(readU64(entries, at + 16)),
            });
        }
    }
    return .{ .sections = sections, .symbols = symbols, .relocs = relocs.items, .text_section = text_section };
}

fn classifyElf(name: []const u8, section_type: u32, flags: u64, addralign: u64, options: BuildOptions) BuildError!SectionClass {
    const consumed = section_type == SHT_NULL or section_type == SHT_SYMTAB or section_type == SHT_STRTAB or section_type == SHT_RELA;
    // Unwind tables and the address-significance table are not placed.
    const dropped = section_type == SHT_X86_64_UNWIND or section_type == SHT_LLVM_ADDRSIG;
    if (consumed or dropped) return .skip;
    if (flags & SHF_TLS != 0) return error.UnsupportedSection;
    if (flags & SHF_ALLOC != 0) {
        if (addralign > page_size) return error.UnsupportedSection;
        if (section_type == SHT_NOBITS) return .bss;
        if (section_type != SHT_PROGBITS) return error.UnsupportedSection;
        if (flags & SHF_EXECINSTR != 0) return .text;
        if (flags & SHF_WRITE != 0) return .data;
        return .rodata;
    }
    if (section_type == SHT_PROGBITS and std.mem.startsWith(u8, name, ".debug_")) {
        if (!options.keep_debug) return .skip;
        if (std.mem.eql(u8, name, ".debug_line")) return .debug_line;
        if (std.mem.eql(u8, name, ".debug_info")) return .debug_info;
        if (std.mem.eql(u8, name, ".debug_abbrev")) return .debug_abbrev;
        return error.UnsupportedSection;
    }
    if (std.mem.eql(u8, name, ".note.GNU-stack") or std.mem.eql(u8, name, ".comment")) return .skip;
    return error.UnsupportedSection;
}

/// A Mach-O arm64 relocatable object. Section addresses are one object-wide
/// address space, which a symbol's value and a section-relative
/// relocation's field are in.
fn parseMachO(arena: Allocator, bytes: []const u8, options: BuildOptions) BuildError!Object {
    if (bytes.len < 32) return error.InvalidObject;
    if (readU32(bytes, 4) != CPU_TYPE_ARM64 or readU32(bytes, 12) != MH_OBJECT) return error.InvalidObject;
    const ncmds = readU32(bytes, 16);
    const sizeofcmds = readU32(bytes, 20);
    if (32 + @as(u64, sizeofcmds) > bytes.len) return error.InvalidObject;
    const MachSection = struct { segname: []const u8, sectname: []const u8, addr: u64, size: u64, offset: u32, @"align": u32, reloff: u32, nreloc: u32, flags: u32 };
    var raw: std.ArrayList(MachSection) = .empty;
    var symoff: u32 = 0;
    var nsyms: u32 = 0;
    var stroff: u32 = 0;
    var strsize: u32 = 0;
    var at: usize = 32;
    for (0..ncmds) |_| {
        if (at + 8 > 32 + sizeofcmds) return error.InvalidObject;
        const cmd = readU32(bytes, at);
        const cmdsize = readU32(bytes, at + 4);
        if (cmdsize < 8 or at + cmdsize > 32 + sizeofcmds) return error.InvalidObject;
        if (cmd == LC_SEGMENT_64) {
            if (cmdsize < 72) return error.InvalidObject;
            const nsects = readU32(bytes, at + 64);
            if (cmdsize < 72 + @as(u64, nsects) * 80) return error.InvalidObject;
            for (0..nsects) |index| {
                const header = at + 72 + index * 80;
                const section: MachSection = .{
                    .sectname = std.mem.sliceTo(bytes[header..][0..16], 0),
                    .segname = std.mem.sliceTo(bytes[header + 16 ..][0..16], 0),
                    .addr = readU64(bytes, header + 32),
                    .size = readU64(bytes, header + 40),
                    .offset = readU32(bytes, header + 48),
                    .@"align" = readU32(bytes, header + 52),
                    .reloff = readU32(bytes, header + 56),
                    .nreloc = readU32(bytes, header + 60),
                    .flags = readU32(bytes, header + 64),
                };
                const zero_fill = section.flags & SECTION_TYPE == S_ZEROFILL or section.flags & SECTION_TYPE == S_GB_ZEROFILL;
                if (!zero_fill and (section.offset > bytes.len or bytes.len - section.offset < section.size)) return error.InvalidObject;
                if (@as(u64, section.reloff) + @as(u64, section.nreloc) * 8 > bytes.len) return error.InvalidObject;
                try raw.append(arena, section);
            }
        } else if (cmd == LC_SYMTAB) {
            if (cmdsize < 24) return error.InvalidObject;
            symoff = readU32(bytes, at + 8);
            nsyms = readU32(bytes, at + 12);
            stroff = readU32(bytes, at + 16);
            strsize = readU32(bytes, at + 20);
            if (@as(u64, symoff) + @as(u64, nsyms) * 16 > bytes.len or @as(u64, stroff) + strsize > bytes.len) return error.InvalidObject;
        }
        at += cmdsize;
    }
    const sections = try arena.alloc(Section, raw.items.len);
    var text_section: ?u32 = null;
    for (raw.items, sections, 0..) |section, *out, index| {
        if (section.@"align" > 12) return error.UnsupportedSection;
        const class = try classifyMachO(section.segname, section.sectname, section.flags, options);
        if (class == .text and text_section == null and std.mem.eql(u8, section.sectname, "__text")) text_section = @intCast(index);
        out.* = .{
            .class = class,
            .bytes = if (class == .bss) &.{} else bytes[section.offset..][0..@intCast(section.size)],
            .size = section.size,
            .addralign = @as(u64, 1) << @intCast(section.@"align"),
        };
    }
    // The object's symbols, then one section symbol per section, which a
    // section-relative relocation names.
    const symbols = try arena.alloc(ObjectSymbol, nsyms + raw.items.len);
    const symbol_names = bytes[stroff..][0..strsize];
    for (symbols[0..nsyms], 0..) |*symbol, index| {
        const entry = symoff + index * 16;
        const n_strx = readU32(bytes, entry);
        const n_type = bytes[entry + 4];
        const n_sect = bytes[entry + 5];
        const n_desc = readU16(bytes, entry + 6);
        const n_value = readU64(bytes, entry + 8);
        if (n_strx >= symbol_names.len and n_strx != 0) return error.InvalidObject;
        const full_name = if (symbol_names.len == 0) "" else std.mem.sliceTo(symbol_names[n_strx..], 0);
        const name = if (full_name.len != 0 and full_name[0] == '_') full_name[1..] else full_name;
        if (n_type & N_STAB != 0) {
            symbol.* = .{ .name = name, .value = 0, .size = 0, .section = null, .bind = .local, .kind = .notype, .hidden = false, .skip = true };
            continue;
        }
        const external = n_type & N_EXT != 0;
        const n_kind = n_type & N_TYPE;
        if (n_kind == N_UNDF) {
            // A nonzero value is a common symbol.
            if (n_value != 0) return error.UnsupportedSymbol;
            symbol.* = .{ .name = name, .value = 0, .size = 0, .section = null, .bind = if (n_desc & N_WEAK_REF != 0) .weak else .global, .kind = .notype, .hidden = false, .skip = false };
        } else if (n_kind == N_SECT) {
            if (n_sect == 0 or n_sect > raw.items.len) return error.InvalidObject;
            const section = raw.items[n_sect - 1];
            if (n_value < section.addr or n_value - section.addr > section.size) return error.InvalidObject;
            symbol.* = .{
                .name = name,
                .value = n_value - section.addr,
                .size = 0,
                .section = n_sect - 1,
                .bind = if (!external) .local else if (n_desc & N_WEAK_DEF != 0) .weak else .global,
                .kind = if (sections[n_sect - 1].class == .text) .func else .object,
                .hidden = external and n_type & N_PEXT != 0,
                .skip = false,
            };
        } else return error.UnsupportedSymbol;
    }
    for (symbols[nsyms..], 0..) |*symbol, index| {
        symbol.* = .{ .name = "", .value = 0, .size = 0, .section = @as(u32, @intCast(index)), .bind = .local, .kind = .section, .hidden = false, .skip = false };
    }
    // Mach-O symbols have no size: a symbol runs to the next one in its
    // section, or to the section's end.
    const by_position = try arena.alloc(u32, nsyms);
    var positioned: u32 = 0;
    for (symbols[0..nsyms], 0..) |symbol, index| {
        if (symbol.skip or symbol.section == null) continue;
        by_position[positioned] = @intCast(index);
        positioned += 1;
    }
    std.mem.sort(u32, by_position[0..positioned], symbols, struct {
        fn lessThan(all: []const ObjectSymbol, a: u32, b: u32) bool {
            if (all[a].section.? != all[b].section.?) return all[a].section.? < all[b].section.?;
            return all[a].value < all[b].value;
        }
    }.lessThan);
    for (by_position[0..positioned], 0..) |index, position| {
        const symbol = &symbols[index];
        const section = symbol.section.?;
        var end = raw.items[section].size;
        if (position + 1 < positioned) {
            const next = symbols[by_position[position + 1]];
            if (next.section.? == section) end = next.value;
        }
        symbol.size = end - symbol.value;
    }
    var relocs: std.ArrayList(Reloc) = .empty;
    for (raw.items, sections, 0..) |section, classified, section_index| {
        if (classified.class == .skip) continue;
        var pending_addend: ?i64 = null;
        for (0..section.nreloc) |index| {
            const entry = section.reloff + index * 8;
            const r_address = readU32(bytes, entry);
            const r_info = readU32(bytes, entry + 4);
            // A scattered relocation has the high bit of its address set.
            if (r_address & 0x8000_0000 != 0) return error.UnsupportedRelocation;
            const r_symbolnum = r_info & 0xff_ffff;
            const r_pcrel = (r_info >> 24) & 1 != 0;
            const r_length = (r_info >> 25) & 3;
            const r_extern = (r_info >> 27) & 1 != 0;
            const r_type = r_info >> 28;
            if (r_type == ARM64_RELOC_ADDEND) {
                if (pending_addend != null) return error.InvalidObject;
                // The addend is the 24-bit symbol number, sign extended.
                pending_addend = @as(i24, @bitCast(@as(u24, @intCast(r_symbolnum))));
                continue;
            }
            const addend_before = pending_addend;
            pending_addend = null;
            const kind: RelocKind = if (r_type == ARM64_RELOC_UNSIGNED)
                (if (r_pcrel) return error.UnsupportedRelocation else switch (r_length) {
                    3 => .abs64,
                    2 => .abs32,
                    else => return error.UnsupportedRelocation,
                })
            else if (r_type == ARM64_RELOC_BRANCH26)
                .branch26
            else if (r_type == ARM64_RELOC_PAGE21)
                .page21
            else if (r_type == ARM64_RELOC_PAGEOFF12)
                .pageoff12
            else if (r_type == ARM64_RELOC_GOT_LOAD_PAGE21)
                .got_load_page21
            else if (r_type == ARM64_RELOC_GOT_LOAD_PAGEOFF12)
                .got_load_pageoff12
            else
                return error.UnsupportedRelocation;
            const width: u64 = if (kind == .abs64) 8 else 4;
            if (r_length != 2 and kind != .abs64) return error.UnsupportedRelocation;
            if (@as(u64, r_address) + width > section.size) return error.InvalidObject;
            const field = classified.bytes[r_address..][0..@intCast(width)];
            var symbol: u32 = undefined;
            var addend: i64 = undefined;
            if (r_extern) {
                if (r_symbolnum >= nsyms) return error.InvalidObject;
                symbol = r_symbolnum;
                addend = switch (kind) {
                    .abs64 => @bitCast(readU64(field, 0)),
                    .abs32 => readU32(field, 0),
                    .pc32, .abs32s, .branch26, .page21, .pageoff12, .rva32, .got_load_page21, .got_load_pageoff12 => addend_before orelse 0,
                };
                if (addend_before != null and (kind == .abs64 or kind == .abs32)) return error.InvalidObject;
            } else {
                // The field holds the target's address in the object's
                // address space.
                if (kind != .abs64 and kind != .abs32) return error.UnsupportedRelocation;
                if (r_symbolnum == 0 or r_symbolnum > raw.items.len) return error.InvalidObject;
                if (addend_before != null) return error.InvalidObject;
                symbol = nsyms + r_symbolnum - 1;
                const address: i64 = if (kind == .abs64) @bitCast(readU64(field, 0)) else readU32(field, 0);
                addend = address - @as(i64, @intCast(raw.items[r_symbolnum - 1].addr));
            }
            try relocs.append(arena, .{ .section = @intCast(section_index), .offset = r_address, .kind = kind, .symbol = symbol, .addend = addend });
        }
        if (pending_addend != null) return error.InvalidObject;
    }
    return .{ .sections = sections, .symbols = symbols, .relocs = relocs.items, .text_section = text_section };
}

fn classifyMachO(segname: []const u8, sectname: []const u8, flags: u32, options: BuildOptions) BuildError!SectionClass {
    const section_type = flags & SECTION_TYPE;
    if (std.mem.eql(u8, segname, "__TEXT")) {
        // Unwind tables are not placed.
        if (std.mem.eql(u8, sectname, "__eh_frame") or std.mem.eql(u8, sectname, "__unwind_info")) return .skip;
        if (std.mem.eql(u8, sectname, "__text") or flags & S_ATTR_PURE_INSTRUCTIONS != 0) {
            if (section_type != S_REGULAR) return error.UnsupportedSection;
            return .text;
        }
        if (section_type == S_REGULAR or section_type == S_CSTRING_LITERALS or section_type == S_4BYTE_LITERALS or section_type == S_8BYTE_LITERALS or section_type == S_16BYTE_LITERALS) return .rodata;
        return error.UnsupportedSection;
    }
    if (std.mem.eql(u8, segname, "__DATA") or std.mem.eql(u8, segname, "__DATA_CONST")) {
        if (section_type == S_REGULAR) return .data;
        if (section_type == S_ZEROFILL or section_type == S_GB_ZEROFILL) return .bss;
        return error.UnsupportedSection;
    }
    if (std.mem.eql(u8, segname, "__LD") and std.mem.eql(u8, sectname, "__compact_unwind")) return .skip;
    if (std.mem.eql(u8, segname, "__DWARF")) {
        if (!options.keep_debug) return .skip;
        if (std.mem.eql(u8, sectname, "__debug_line")) return .debug_line;
        if (std.mem.eql(u8, sectname, "__debug_info")) return .debug_info;
        if (std.mem.eql(u8, sectname, "__debug_abbrev")) return .debug_abbrev;
        return error.UnsupportedSection;
    }
    return error.UnsupportedSection;
}

/// A COFF relocatable object for x86_64 or arm64 Windows. Section names are
/// read through the string table when long; a COMDAT section's external
/// symbols are weak unless the section allows no duplicates; a weak
/// external aliasing a definition becomes that definition under the weak
/// name; auxiliary symbol records keep their table index as skipped entries
/// so relocations index the table directly.
fn parseCoff(arena: Allocator, bytes: []const u8, options: BuildOptions) BuildError!Object {
    if (bytes.len < 20) return error.InvalidObject;
    const machine = readU16(bytes, 0);
    const arm64 = machine == IMAGE_FILE_MACHINE_ARM64;
    const section_count: usize = readU16(bytes, 2);
    const symtab_offset: usize = readU32(bytes, 8);
    const symbol_count: usize = readU32(bytes, 12);
    const headers_at: usize = 20 + @as(usize, readU16(bytes, 16));
    if (headers_at + section_count * coff_section_header_size > bytes.len) return error.InvalidObject;
    if (symtab_offset + symbol_count * coff_symbol_size > bytes.len) return error.InvalidObject;
    var strtab: []const u8 = &.{};
    if (symtab_offset != 0) {
        const strtab_at = symtab_offset + symbol_count * coff_symbol_size;
        if (strtab_at + 4 > bytes.len) return error.InvalidObject;
        const strtab_size = readU32(bytes, strtab_at);
        if (strtab_size < 4 or strtab_at + strtab_size > bytes.len) return error.InvalidObject;
        strtab = bytes[strtab_at..][0..strtab_size];
    }

    const CoffSection = struct {
        name: []const u8,
        size: u64,
        offset: u32,
        reloc_offset: u32,
        reloc_count: u32,
        characteristics: u32,
        /// For a COMDAT section, the selection its section symbol's
        /// auxiliary record names; set while the symbols are read.
        comdat_selection: u8 = 0,
    };
    const raw = try arena.alloc(CoffSection, section_count);
    for (raw, 0..) |*section, index| {
        const header = bytes[headers_at + index * coff_section_header_size ..][0..coff_section_header_size];
        const size = readU32(header, 16);
        const offset = readU32(header, 20);
        const characteristics = readU32(header, 36);
        var reloc_count: u32 = readU16(header, 32);
        var reloc_offset = readU32(header, 24);
        if (characteristics & IMAGE_SCN_LNK_NRELOC_OVFL != 0 and reloc_count == 0xffff) {
            if (@as(u64, reloc_offset) + coff_reloc_size > bytes.len) return error.InvalidObject;
            reloc_count = readU32(bytes, reloc_offset);
            if (reloc_count == 0) return error.InvalidObject;
            reloc_count -= 1;
            reloc_offset += coff_reloc_size;
        }
        if (@as(u64, reloc_offset) + @as(u64, reloc_count) * coff_reloc_size > bytes.len) return error.InvalidObject;
        const uninitialized = characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA != 0;
        if (!uninitialized and (offset > bytes.len or bytes.len - offset < size)) return error.InvalidObject;
        section.* = .{
            .name = try coffSectionName(header[0..8], strtab),
            .size = size,
            .offset = offset,
            .reloc_offset = reloc_offset,
            .reloc_count = reloc_count,
            .characteristics = characteristics,
        };
    }

    // Symbols by table index, auxiliary records included as skipped
    // entries. A weak external is filled once its tag symbol is read.
    const symbols = try arena.alloc(ObjectSymbol, symbol_count);
    const WeakTag = struct { symbol: u32, tag: u32 };
    var weak_tags: std.ArrayList(WeakTag) = .empty;
    var index: usize = 0;
    while (index < symbol_count) {
        const entry = bytes[symtab_offset + index * coff_symbol_size ..][0..coff_symbol_size];
        const name = try coffSymbolName(entry[0..8], strtab);
        const value = readU32(entry, 8);
        const section_number: i16 = @bitCast(readU16(entry, 12));
        const symbol_type = readU16(entry, 14);
        const storage_class = entry[16];
        const aux_count: usize = entry[17];
        if (index + 1 + aux_count > symbol_count) return error.InvalidObject;
        const skipped: ObjectSymbol = .{ .name = name, .value = 0, .size = 0, .section = null, .bind = .local, .kind = .notype, .hidden = false, .skip = true };
        if (section_number > 0 and @as(usize, @intCast(section_number)) > section_count) return error.InvalidObject;
        const section: ?u32 = if (section_number > 0) @as(u32, @intCast(section_number - 1)) else null;
        const is_function = symbol_type & 0xf0 == IMAGE_SYM_DTYPE_FUNCTION << 4;
        if (storage_class == IMAGE_SYM_CLASS_EXTERNAL) {
            if (section_number == IMAGE_SYM_ABSOLUTE or section_number == IMAGE_SYM_DEBUG) return error.UnsupportedSymbol;
            if (section == null) {
                // A nonzero value is a common symbol.
                if (value != 0) return error.UnsupportedSymbol;
                symbols[index] = .{ .name = name, .value = 0, .size = 0, .section = null, .bind = .global, .kind = .notype, .hidden = false, .skip = false };
            } else {
                const in = raw[section.?];
                if (value > in.size) return error.InvalidObject;
                const comdat = in.characteristics & IMAGE_SCN_LNK_COMDAT != 0 and in.comdat_selection != IMAGE_COMDAT_SELECT_NODUPLICATES and in.comdat_selection != IMAGE_COMDAT_SELECT_ASSOCIATIVE;
                symbols[index] = .{
                    .name = name,
                    .value = value,
                    .size = 0,
                    .section = section,
                    .bind = if (comdat) .weak else .global,
                    .kind = if (is_function or in.characteristics & IMAGE_SCN_CNT_CODE != 0) .func else .object,
                    .hidden = false,
                    .skip = false,
                };
            }
        } else if (storage_class == IMAGE_SYM_CLASS_STATIC) {
            if (section == null) {
                // An absolute or debug static, such as a feature flag.
                symbols[index] = skipped;
            } else if (aux_count != 0 and value == 0 and std.mem.eql(u8, name, raw[section.?].name)) {
                // The section's own symbol, whose auxiliary record
                // carries the COMDAT selection.
                const aux = bytes[symtab_offset + (index + 1) * coff_symbol_size ..][0..coff_symbol_size];
                raw[section.?].comdat_selection = aux[14];
                symbols[index] = .{ .name = "", .value = 0, .size = 0, .section = section, .bind = .local, .kind = .section, .hidden = false, .skip = false };
            } else {
                const in = raw[section.?];
                if (value > in.size) return error.InvalidObject;
                symbols[index] = .{
                    .name = name,
                    .value = value,
                    .size = 0,
                    .section = section,
                    .bind = .local,
                    .kind = if (is_function or in.characteristics & IMAGE_SCN_CNT_CODE != 0) .func else .object,
                    .hidden = false,
                    .skip = false,
                };
            }
        } else if (storage_class == IMAGE_SYM_CLASS_WEAK_EXTERNAL) {
            if (aux_count == 0 or section != null) return error.UnsupportedSymbol;
            const aux = bytes[symtab_offset + (index + 1) * coff_symbol_size ..][0..coff_symbol_size];
            const tag = readU32(aux, 0);
            if (tag >= symbol_count) return error.InvalidObject;
            try weak_tags.append(arena, .{ .symbol = @intCast(index), .tag = tag });
            symbols[index] = .{ .name = name, .value = 0, .size = 0, .section = null, .bind = .weak, .kind = .notype, .hidden = false, .skip = false };
        } else {
            symbols[index] = skipped;
        }
        for (1..aux_count + 1) |aux| symbols[index + aux] = skipped;
        index += 1 + aux_count;
    }
    for (weak_tags.items) |pending| {
        const tag = symbols[pending.tag];
        if (tag.skip) return error.InvalidObject;
        const section = tag.section orelse continue;
        const symbol = &symbols[pending.symbol];
        symbol.section = section;
        symbol.value = tag.value;
        symbol.kind = tag.kind;
    }

    const sections = try arena.alloc(Section, section_count);
    var text_section: ?u32 = null;
    for (raw, sections, 0..) |section, *out, section_index| {
        const class = try classifyCoff(section.name, section.characteristics, options);
        if (class == .text and text_section == null and std.mem.eql(u8, section.name, ".text")) text_section = @intCast(section_index);
        const align_field = (section.characteristics >> 20) & 0xf;
        const addralign: u64 = if (align_field == 0) 16 else @as(u64, 1) << @intCast(align_field - 1);
        if (addralign > page_size) return error.UnsupportedSection;
        out.* = .{
            .class = class,
            .bytes = if (class == .bss or class == .skip) &.{} else bytes[section.offset..][0..@intCast(section.size)],
            .size = section.size,
            .addralign = addralign,
        };
    }
    // COFF symbols have no size: a symbol runs to the next one in its
    // section, or to the section's end.
    const by_position = try arena.alloc(u32, symbol_count);
    var positioned: u32 = 0;
    for (symbols, 0..) |symbol, symbol_index| {
        if (symbol.skip or symbol.section == null or symbol.kind == .section) continue;
        by_position[positioned] = @intCast(symbol_index);
        positioned += 1;
    }
    std.mem.sort(u32, by_position[0..positioned], symbols, struct {
        fn lessThan(all: []const ObjectSymbol, a: u32, b: u32) bool {
            if (all[a].section.? != all[b].section.?) return all[a].section.? < all[b].section.?;
            return all[a].value < all[b].value;
        }
    }.lessThan);
    for (by_position[0..positioned], 0..) |symbol_index, position| {
        const symbol = &symbols[symbol_index];
        const section = symbol.section.?;
        var end = raw[section].size;
        if (position + 1 < positioned) {
            const next = symbols[by_position[position + 1]];
            if (next.section.? == section) end = next.value;
        }
        symbol.size = end - symbol.value;
    }

    var relocs: std.ArrayList(Reloc) = .empty;
    for (raw, sections, 0..) |section, classified, section_index| {
        if (classified.class == .skip or classified.class == .bss) continue;
        for (0..section.reloc_count) |reloc_index| {
            const entry = bytes[section.reloc_offset + reloc_index * coff_reloc_size ..][0..coff_reloc_size];
            const offset = readU32(entry, 0);
            const symbol_index = readU32(entry, 4);
            const reloc_type = readU16(entry, 8);
            if (symbol_index >= symbol_count) return error.InvalidObject;
            const target = symbols[symbol_index];
            if (target.skip) return error.InvalidObject;
            const decoded = try decodeCoffReloc(arm64, reloc_type, classified.bytes, offset, section.size);
            if (decoded.kind == .abs32 and decoded.section_relative) {
                // A section-relative field names an offset in a debug
                // section, which is what an absolute site into a debug
                // region resolves to.
                const target_section = target.section orelse return error.UnsupportedRelocation;
                if (sections[target_section].class.isAllocated()) return error.UnsupportedRelocation;
            }
            try relocs.append(arena, .{ .section = @intCast(section_index), .offset = offset, .kind = decoded.kind, .symbol = symbol_index, .addend = decoded.addend });
        }
    }
    return .{ .sections = sections, .symbols = symbols, .relocs = relocs.items, .text_section = text_section };
}

const DecodedCoffReloc = struct { kind: RelocKind, addend: i64, section_relative: bool = false };

/// The site kind and addend of a COFF relocation: the addend is the
/// field's contents, which an instruction field keeps in its immediate.
fn decodeCoffReloc(arm64: bool, reloc_type: u16, section_bytes: []const u8, offset: u32, section_size: u64) BuildError!DecodedCoffReloc {
    const width: u64 = if (!arm64 and reloc_type == IMAGE_REL_AMD64_ADDR64 or arm64 and reloc_type == IMAGE_REL_ARM64_ADDR64) 8 else 4;
    if (@as(u64, offset) + width > section_size) return error.InvalidObject;
    const field = section_bytes[offset..][0..@intCast(width)];
    if (!arm64) {
        if (reloc_type == IMAGE_REL_AMD64_ADDR64) return .{ .kind = .abs64, .addend = @bitCast(readU64(field, 0)) };
        if (reloc_type == IMAGE_REL_AMD64_ADDR32) return .{ .kind = .abs32, .addend = readU32(field, 0) };
        if (reloc_type == IMAGE_REL_AMD64_ADDR32NB) return .{ .kind = .rva32, .addend = readU32(field, 0) };
        if (reloc_type >= IMAGE_REL_AMD64_REL32 and reloc_type <= IMAGE_REL_AMD64_REL32_5) return .{ .kind = .pc32, .addend = @as(i64, @as(i32, @bitCast(readU32(field, 0)))) - 4 - @as(i64, reloc_type - IMAGE_REL_AMD64_REL32) };
        if (reloc_type == IMAGE_REL_AMD64_SECREL) return .{ .kind = .abs32, .addend = readU32(field, 0), .section_relative = true };
        return error.UnsupportedRelocation;
    }
    const instruction = readU32(field, 0);
    if (reloc_type == IMAGE_REL_ARM64_ADDR32) return .{ .kind = .abs32, .addend = instruction };
    if (reloc_type == IMAGE_REL_ARM64_ADDR32NB) return .{ .kind = .rva32, .addend = instruction };
    if (reloc_type == IMAGE_REL_ARM64_BRANCH26) return .{ .kind = .branch26, .addend = @as(i64, @as(i26, @bitCast(@as(u26, @truncate(instruction))))) << 2 };
    if (reloc_type == IMAGE_REL_ARM64_PAGEBASE_REL21) return .{ .kind = .page21, .addend = ((instruction >> 29) & 0x3) | ((instruction >> 3) & 0x1f_fffc) };
    if (reloc_type == IMAGE_REL_ARM64_PAGEOFFSET_12A) return .{ .kind = .pageoff12, .addend = (instruction >> 10) & 0xfff };
    if (reloc_type == IMAGE_REL_ARM64_PAGEOFFSET_12L) return .{ .kind = .pageoff12, .addend = @as(i64, (instruction >> 10) & 0xfff) << (pageOffsetShift(instruction) orelse return error.UnsupportedRelocation) };
    if (reloc_type == IMAGE_REL_ARM64_SECREL) return .{ .kind = .abs32, .addend = instruction, .section_relative = true };
    if (reloc_type == IMAGE_REL_ARM64_ADDR64) return .{ .kind = .abs64, .addend = @bitCast(readU64(field, 0)) };
    return error.UnsupportedRelocation;
}

fn classifyCoff(name: []const u8, characteristics: u32, options: BuildOptions) BuildError!SectionClass {
    // Linker directives and the address-significance table are consumed by
    // a linker, never placed.
    if (characteristics & (IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE) != 0) return .skip;
    if (std.mem.startsWith(u8, name, ".debug_")) {
        if (characteristics & IMAGE_SCN_MEM_DISCARDABLE == 0) return error.UnsupportedSection;
        if (!options.keep_debug) return .skip;
        if (std.mem.eql(u8, name, ".debug_line")) return .debug_line;
        if (std.mem.eql(u8, name, ".debug_info")) return .debug_info;
        if (std.mem.eql(u8, name, ".debug_abbrev")) return .debug_abbrev;
        return error.UnsupportedSection;
    }
    if (characteristics & IMAGE_SCN_CNT_CODE != 0) return .text;
    if (characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA != 0) return .bss;
    if (characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA != 0) {
        if (std.mem.startsWith(u8, name, ".pdata")) return .pdata;
        if (std.mem.startsWith(u8, name, ".xdata")) return .xdata;
        // Thread-local storage and C runtime initializer tables are the
        // host's to lay out.
        if (std.mem.startsWith(u8, name, ".tls") or std.mem.startsWith(u8, name, ".CRT")) return error.UnsupportedSection;
        if (characteristics & IMAGE_SCN_MEM_WRITE != 0) return .data;
        return .rodata;
    }
    return error.UnsupportedSection;
}

/// A section header's name: inline, or "/decimal" and "//base64" offsets
/// into the string table.
fn coffSectionName(field: *const [8]u8, strtab: []const u8) BuildError![]const u8 {
    if (field[0] != '/') return std.mem.sliceTo(field, 0);
    var offset: u64 = 0;
    if (field[1] == '/') {
        const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        for (field[2..]) |digit| {
            const value = std.mem.findScalar(u8, alphabet, digit) orelse return error.InvalidObject;
            offset = offset * 64 + value;
        }
    } else {
        const digits = std.mem.sliceTo(field[1..], 0);
        if (digits.len == 0) return error.InvalidObject;
        for (digits) |digit| {
            if (digit < '0' or digit > '9') return error.InvalidObject;
            offset = offset * 10 + (digit - '0');
        }
    }
    if (offset >= strtab.len) return error.InvalidObject;
    return std.mem.sliceTo(strtab[@intCast(offset)..], 0);
}

/// A symbol's name: inline, or an offset into the string table when the
/// field's first word is zero.
fn coffSymbolName(field: *const [8]u8, strtab: []const u8) BuildError![]const u8 {
    if (readU32(field, 0) != 0) return std.mem.sliceTo(field, 0);
    const offset = readU32(field, 4);
    if (offset >= strtab.len) return error.InvalidObject;
    return std.mem.sliceTo(strtab[offset..], 0);
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

const IMAGE_FILE_MACHINE_AMD64 = 0x8664;
const IMAGE_FILE_MACHINE_ARM64 = 0xaa64;
const coff_section_header_size = 40;
const coff_symbol_size = 18;
const coff_reloc_size = 10;
const IMAGE_SCN_LNK_INFO = 0x0000_0200;
const IMAGE_SCN_LNK_REMOVE = 0x0000_0800;
const IMAGE_SCN_LNK_COMDAT = 0x0000_1000;
const IMAGE_SCN_CNT_CODE = 0x0000_0020;
const IMAGE_SCN_CNT_INITIALIZED_DATA = 0x0000_0040;
const IMAGE_SCN_CNT_UNINITIALIZED_DATA = 0x0000_0080;
const IMAGE_SCN_LNK_NRELOC_OVFL = 0x0100_0000;
const IMAGE_SCN_MEM_DISCARDABLE = 0x0200_0000;
const IMAGE_SCN_MEM_EXECUTE = 0x2000_0000;
const IMAGE_SCN_MEM_READ = 0x4000_0000;
const IMAGE_SCN_MEM_WRITE = 0x8000_0000;
const IMAGE_SYM_ABSOLUTE = -1;
const IMAGE_SYM_DEBUG = -2;
const IMAGE_SYM_CLASS_EXTERNAL = 2;
const IMAGE_SYM_CLASS_STATIC = 3;
const IMAGE_SYM_CLASS_WEAK_EXTERNAL = 105;
const IMAGE_SYM_DTYPE_FUNCTION = 2;
const IMAGE_COMDAT_SELECT_NODUPLICATES = 1;
const IMAGE_COMDAT_SELECT_ANY = 2;
const IMAGE_COMDAT_SELECT_ASSOCIATIVE = 5;
const IMAGE_WEAK_EXTERN_SEARCH_ALIAS = 3;
const IMAGE_REL_AMD64_ADDR64 = 0x0001;
const IMAGE_REL_AMD64_ADDR32 = 0x0002;
const IMAGE_REL_AMD64_ADDR32NB = 0x0003;
const IMAGE_REL_AMD64_REL32 = 0x0004;
const IMAGE_REL_AMD64_REL32_5 = 0x0009;
const IMAGE_REL_AMD64_SECREL = 0x000b;
const IMAGE_REL_ARM64_ADDR32 = 0x0001;
const IMAGE_REL_ARM64_ADDR32NB = 0x0002;
const IMAGE_REL_ARM64_BRANCH26 = 0x0003;
const IMAGE_REL_ARM64_PAGEBASE_REL21 = 0x0004;
const IMAGE_REL_ARM64_PAGEOFFSET_12A = 0x0006;
const IMAGE_REL_ARM64_PAGEOFFSET_12L = 0x0007;
const IMAGE_REL_ARM64_SECREL = 0x0008;
const IMAGE_REL_ARM64_ADDR64 = 0x000e;

const MH_MAGIC_64 = 0xfeedfacf;
const MH_OBJECT = 1;
const CPU_TYPE_ARM64 = 0x0100000c;
const CPU_SUBTYPE_ARM64_ALL = 0;
const LC_SEGMENT_64 = 0x19;
const LC_SYMTAB = 0x2;
const SECTION_TYPE = 0xff;
const S_REGULAR = 0;
const S_ZEROFILL = 1;
const S_CSTRING_LITERALS = 2;
const S_4BYTE_LITERALS = 3;
const S_8BYTE_LITERALS = 4;
const S_GB_ZEROFILL = 0xc;
const S_16BYTE_LITERALS = 0xe;
const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
const S_ATTR_DEBUG = 0x02000000;
const N_STAB = 0xe0;
const N_PEXT = 0x10;
const N_TYPE = 0x0e;
const N_EXT = 0x01;
const N_UNDF = 0x0;
const N_SECT = 0xe;
const N_WEAK_REF = 0x40;
const N_WEAK_DEF = 0x80;
const ARM64_RELOC_UNSIGNED = 0;
const ARM64_RELOC_BRANCH26 = 2;
const ARM64_RELOC_PAGE21 = 3;
const ARM64_RELOC_PAGEOFF12 = 4;
const ARM64_RELOC_GOT_LOAD_PAGE21 = 5;
const ARM64_RELOC_GOT_LOAD_PAGEOFF12 = 6;
const ARM64_RELOC_ADDEND = 10;

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

/// A minimal arm64 Mach-O relocatable object for tests, laid out the way
/// the assembler lays one out: the sections contiguous in one address
/// space, each symbol's value in it.
pub const TestMachObject = struct {
    pub const TestSection = struct {
        segname: []const u8,
        sectname: []const u8,
        bytes: []const u8 = &.{},
        /// For a zero-fill section.
        size: u64 = 0,
        /// log2 of the alignment.
        @"align": u32 = 4,
        flags: u32 = 0,
    };
    pub const TestSymbol = struct {
        /// With its underscore, as the assembler writes it.
        name: []const u8,
        /// 0 for undefined, else a 1-based index into the sections.
        section: u8,
        /// Offset in the section.
        value: u64 = 0,
        external: bool = false,
        private: bool = false,
        weak_def: bool = false,
        weak_ref: bool = false,
    };
    pub const TestRelocation = struct {
        /// 1-based index of the section the field is in.
        section: u8,
        offset: u32,
        /// 1-based index into the symbols for an external relocation, else
        /// a 1-based section index.
        symbol: u32,
        external: bool = true,
        type: u4,
        pcrel: bool,
        /// log2 of the field width.
        length: u2 = 2,
        /// Written as an `ARM64_RELOC_ADDEND` entry before the relocation
        /// for the instruction kinds, into the field for the others.
        addend: i64 = 0,
    };

    pub fn encode(allocator: Allocator, sections: []const TestSection, symbols: []const TestSymbol, relocations: []const TestRelocation) Allocator.Error![]u8 {
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(allocator);
        const header_size: usize = 32;
        const segment_size: usize = 72 + sections.len * 80;
        const symtab_size: usize = 24;
        const contents_offset = header_size + segment_size + symtab_size;
        // Addresses and file offsets of the sections.
        const addrs = try allocator.alloc(u64, sections.len);
        defer allocator.free(addrs);
        const offsets = try allocator.alloc(u32, sections.len);
        defer allocator.free(offsets);
        var addr: u64 = 0;
        var offset: usize = contents_offset;
        for (sections, addrs, offsets) |section, *section_addr, *section_offset| {
            const alignment_bytes = @as(u64, 1) << @intCast(section.@"align");
            addr = std.mem.alignForward(u64, addr, alignment_bytes);
            section_addr.* = addr;
            const zero_fill = section.flags & SECTION_TYPE == S_ZEROFILL;
            if (zero_fill) {
                section_offset.* = 0;
                addr += section.size;
            } else {
                offset = std.mem.alignForward(usize, offset, @intCast(alignment_bytes));
                section_offset.* = @intCast(offset);
                offset += section.bytes.len;
                addr += section.bytes.len;
            }
        }
        var contents: std.ArrayList(u8) = .empty;
        defer contents.deinit(allocator);
        try contents.appendNTimes(allocator, 0, offset - contents_offset);
        for (sections, offsets) |section, section_offset| {
            if (section_offset == 0) continue;
            @memcpy(contents.items[section_offset - contents_offset ..][0..section.bytes.len], section.bytes);
        }
        // Relocation entries per section, with addends: instruction kinds
        // get an ADDEND entry, absolute kinds have it in the field.
        const reloffs = try allocator.alloc(u32, sections.len);
        defer allocator.free(reloffs);
        const nrelocs = try allocator.alloc(u32, sections.len);
        defer allocator.free(nrelocs);
        var reloc_bytes: std.ArrayList(u8) = .empty;
        defer reloc_bytes.deinit(allocator);
        for (sections, reloffs, nrelocs, 1..) |_, *reloff, *nreloc, section_index| {
            reloff.* = @intCast(offset + reloc_bytes.items.len);
            nreloc.* = 0;
            for (relocations) |relocation| {
                if (relocation.section != section_index) continue;
                const instruction_kind = relocation.type != ARM64_RELOC_UNSIGNED;
                if (instruction_kind and relocation.addend != 0) {
                    try appendReloc(allocator, &reloc_bytes, relocation.offset, @intCast(@as(u24, @bitCast(@as(i24, @intCast(relocation.addend))))), false, 2, false, ARM64_RELOC_ADDEND);
                    nreloc.* += 1;
                }
                if (!instruction_kind) {
                    const field = contents.items[offsets[section_index - 1] - contents_offset + relocation.offset ..];
                    const value: i64 = if (relocation.external) relocation.addend else @as(i64, @intCast(addrs[relocation.symbol - 1])) + relocation.addend;
                    if (relocation.length == 3) std.mem.writeInt(i64, field[0..8], value, .little) else std.mem.writeInt(u32, field[0..4], @intCast(value), .little);
                }
                const symbolnum: u24 = @intCast(if (relocation.external) relocation.symbol - 1 else relocation.symbol);
                try appendReloc(allocator, &reloc_bytes, relocation.offset, symbolnum, relocation.pcrel, relocation.length, relocation.external, relocation.type);
                nreloc.* += 1;
            }
            if (nreloc.* == 0) reloff.* = 0;
        }
        offset += reloc_bytes.items.len;
        // Symbols.
        var strtab: std.ArrayList(u8) = .empty;
        defer strtab.deinit(allocator);
        try strtab.appendSlice(allocator, " \x00");
        var nlists: std.ArrayList(u8) = .empty;
        defer nlists.deinit(allocator);
        for (symbols) |symbol| {
            var entry: [16]u8 = @splat(0);
            std.mem.writeInt(u32, entry[0..4], @intCast(strtab.items.len), .little);
            try strtab.appendSlice(allocator, symbol.name);
            try strtab.append(allocator, 0);
            var n_type: u8 = if (symbol.section == 0) N_UNDF else N_SECT;
            if (symbol.external) n_type |= N_EXT;
            if (symbol.private) n_type |= N_PEXT;
            entry[4] = n_type;
            entry[5] = symbol.section;
            var n_desc: u16 = 0;
            if (symbol.weak_def) n_desc |= N_WEAK_DEF;
            if (symbol.weak_ref) n_desc |= N_WEAK_REF;
            std.mem.writeInt(u16, entry[6..8], n_desc, .little);
            std.mem.writeInt(u64, entry[8..16], if (symbol.section == 0) 0 else addrs[symbol.section - 1] + symbol.value, .little);
            try nlists.appendSlice(allocator, &entry);
        }
        const symoff = offset;
        offset += nlists.items.len;
        const stroff = offset;
        // Header.
        try out.appendNTimes(allocator, 0, header_size);
        std.mem.writeInt(u32, out.items[0..4], MH_MAGIC_64, .little);
        std.mem.writeInt(u32, out.items[4..8], CPU_TYPE_ARM64, .little);
        std.mem.writeInt(u32, out.items[8..12], CPU_SUBTYPE_ARM64_ALL, .little);
        std.mem.writeInt(u32, out.items[12..16], MH_OBJECT, .little);
        std.mem.writeInt(u32, out.items[16..20], 2, .little);
        std.mem.writeInt(u32, out.items[20..24], @intCast(segment_size + symtab_size), .little);
        // Segment.
        var segment: [72]u8 = @splat(0);
        std.mem.writeInt(u32, segment[0..4], LC_SEGMENT_64, .little);
        std.mem.writeInt(u32, segment[4..8], @intCast(segment_size), .little);
        std.mem.writeInt(u64, segment[24..32], addr, .little);
        std.mem.writeInt(u64, segment[32..40], contents_offset, .little);
        std.mem.writeInt(u64, segment[40..48], contents.items.len, .little);
        std.mem.writeInt(u32, segment[56..60], 7, .little);
        std.mem.writeInt(u32, segment[60..64], 7, .little);
        std.mem.writeInt(u32, segment[64..68], @intCast(sections.len), .little);
        try out.appendSlice(allocator, &segment);
        for (sections, addrs, offsets, reloffs, nrelocs) |section, section_addr, section_offset, reloff, nreloc| {
            var entry: [80]u8 = @splat(0);
            @memcpy(entry[0..section.sectname.len], section.sectname);
            @memcpy(entry[16..][0..section.segname.len], section.segname);
            std.mem.writeInt(u64, entry[32..40], section_addr, .little);
            std.mem.writeInt(u64, entry[40..48], if (section_offset == 0) section.size else section.bytes.len, .little);
            std.mem.writeInt(u32, entry[48..52], section_offset, .little);
            std.mem.writeInt(u32, entry[52..56], section.@"align", .little);
            std.mem.writeInt(u32, entry[56..60], reloff, .little);
            std.mem.writeInt(u32, entry[60..64], nreloc, .little);
            std.mem.writeInt(u32, entry[64..68], section.flags, .little);
            try out.appendSlice(allocator, &entry);
        }
        var symtab: [24]u8 = @splat(0);
        std.mem.writeInt(u32, symtab[0..4], LC_SYMTAB, .little);
        std.mem.writeInt(u32, symtab[4..8], symtab_size, .little);
        std.mem.writeInt(u32, symtab[8..12], @intCast(symoff), .little);
        std.mem.writeInt(u32, symtab[12..16], @intCast(symbols.len), .little);
        std.mem.writeInt(u32, symtab[16..20], @intCast(stroff), .little);
        std.mem.writeInt(u32, symtab[20..24], @intCast(strtab.items.len), .little);
        try out.appendSlice(allocator, &symtab);
        std.debug.assert(out.items.len == contents_offset);
        try out.appendSlice(allocator, contents.items);
        try out.appendSlice(allocator, reloc_bytes.items);
        try out.appendSlice(allocator, nlists.items);
        try out.appendSlice(allocator, strtab.items);
        return try out.toOwnedSlice(allocator);
    }

    fn appendReloc(allocator: Allocator, out: *std.ArrayList(u8), address: u32, symbolnum: u24, pcrel: bool, length: u2, external: bool, reloc_type: u4) Allocator.Error!void {
        var entry: [8]u8 = undefined;
        std.mem.writeInt(u32, entry[0..4], address, .little);
        const info: u32 = @as(u32, symbolnum) | (@as(u32, @intFromBool(pcrel)) << 24) | (@as(u32, length) << 25) | (@as(u32, @intFromBool(external)) << 27) | (@as(u32, reloc_type) << 28);
        std.mem.writeInt(u32, entry[4..8], info, .little);
        try out.appendSlice(allocator, &entry);
    }
};

/// A minimal COFF relocatable object for tests: sections and symbols given
/// as data, encoded the way a compiler lays them out, with the auxiliary
/// records a section symbol and a weak external carry.
pub const TestCoffObject = struct {
    pub const TestSection = struct {
        name: []const u8,
        bytes: []const u8 = &.{},
        /// For an uninitialized section.
        size: u64 = 0,
        characteristics: u32,
        /// The COMDAT selection the section symbol's record names.
        selection: u8 = 0,
    };
    pub const TestSymbol = struct {
        name: []const u8,
        /// 0 for undefined, else a 1-based index into the sections.
        section: u16,
        value: u32 = 0,
        class: enum { external, static, section, weak_external } = .external,
        function: bool = false,
        /// For a weak external, the index in the symbols of its tag.
        tag: u32 = 0,
    };
    pub const TestRelocation = struct {
        /// 1-based index of the section the field is in.
        section: u16,
        offset: u32,
        /// Index into the symbols.
        symbol: u32,
        type: u16,
    };

    pub fn encode(allocator: Allocator, machine: u16, sections: []const TestSection, symbols: []const TestSymbol, relocations: []const TestRelocation) Allocator.Error![]u8 {
        var strtab: std.ArrayList(u8) = .empty;
        defer strtab.deinit(allocator);
        try strtab.appendNTimes(allocator, 0, 4);
        // Table indices: a section symbol and a weak external carry one
        // auxiliary record each.
        const table_index = try allocator.alloc(u32, symbols.len);
        defer allocator.free(table_index);
        var table_count: u32 = 0;
        for (symbols, table_index) |symbol, *index| {
            index.* = table_count;
            table_count += if (symbol.class == .section or symbol.class == .weak_external) 2 else 1;
        }
        const headers_end: usize = 20 + sections.len * coff_section_header_size;
        var out: std.ArrayList(u8) = .empty;
        errdefer out.deinit(allocator);
        try out.appendNTimes(allocator, 0, headers_end);
        // Section data, then each section's relocations.
        const data_offsets = try allocator.alloc(u32, sections.len);
        defer allocator.free(data_offsets);
        for (sections, data_offsets) |section, *offset| {
            offset.* = if (section.bytes.len == 0) 0 else @intCast(out.items.len);
            try out.appendSlice(allocator, section.bytes);
        }
        const reloc_offsets = try allocator.alloc(u32, sections.len);
        defer allocator.free(reloc_offsets);
        const reloc_counts = try allocator.alloc(u16, sections.len);
        defer allocator.free(reloc_counts);
        for (sections, reloc_offsets, reloc_counts, 1..) |_, *offset, *count, section_index| {
            offset.* = @intCast(out.items.len);
            count.* = 0;
            for (relocations) |relocation| {
                if (relocation.section != section_index) continue;
                var entry: [coff_reloc_size]u8 = undefined;
                std.mem.writeInt(u32, entry[0..4], relocation.offset, .little);
                std.mem.writeInt(u32, entry[4..8], table_index[relocation.symbol], .little);
                std.mem.writeInt(u16, entry[8..10], relocation.type, .little);
                try out.appendSlice(allocator, &entry);
                count.* += 1;
            }
            if (count.* == 0) offset.* = 0;
        }
        const symtab_offset: u32 = @intCast(out.items.len);
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
            std.mem.writeInt(u16, entry[14..16], if (symbol.function) IMAGE_SYM_DTYPE_FUNCTION << 4 else 0, .little);
            entry[16] = switch (symbol.class) {
                .external => IMAGE_SYM_CLASS_EXTERNAL,
                .static, .section => IMAGE_SYM_CLASS_STATIC,
                .weak_external => IMAGE_SYM_CLASS_WEAK_EXTERNAL,
            };
            entry[17] = if (symbol.class == .section or symbol.class == .weak_external) 1 else 0;
            try out.appendSlice(allocator, &entry);
            var aux: [coff_symbol_size]u8 = @splat(0);
            switch (symbol.class) {
                .section => {
                    const section = sections[symbol.section - 1];
                    std.mem.writeInt(u32, aux[0..4], @intCast(section.bytes.len), .little);
                    std.mem.writeInt(u16, aux[4..6], reloc_counts[symbol.section - 1], .little);
                    aux[14] = section.selection;
                    try out.appendSlice(allocator, &aux);
                },
                .weak_external => {
                    std.mem.writeInt(u32, aux[0..4], table_index[symbol.tag], .little);
                    std.mem.writeInt(u32, aux[4..8], IMAGE_WEAK_EXTERN_SEARCH_ALIAS, .little);
                    try out.appendSlice(allocator, &aux);
                },
                .external, .static => {},
            }
        }
        // Section headers, with long names in the string table.
        for (sections, 0..) |section, index| {
            const header = out.items[20 + index * coff_section_header_size ..][0..coff_section_header_size];
            if (section.name.len <= 8) {
                @memcpy(header[0..section.name.len], section.name);
            } else {
                header[0] = '/';
                _ = std.fmt.printInt(header[1..8], strtab.items.len, 10, .lower, .{});
                try strtab.appendSlice(allocator, section.name);
                try strtab.append(allocator, 0);
            }
            std.mem.writeInt(u32, header[16..20], @intCast(if (section.bytes.len == 0) section.size else section.bytes.len), .little);
            std.mem.writeInt(u32, header[20..24], data_offsets[index], .little);
            std.mem.writeInt(u32, header[24..28], reloc_offsets[index], .little);
            std.mem.writeInt(u16, header[32..34], reloc_counts[index], .little);
            std.mem.writeInt(u32, header[36..40], section.characteristics, .little);
        }
        std.mem.writeInt(u32, strtab.items[0..4], @intCast(strtab.items.len), .little);
        try out.appendSlice(allocator, strtab.items);
        const file_header = out.items[0..20];
        std.mem.writeInt(u16, file_header[0..2], machine, .little);
        std.mem.writeInt(u16, file_header[2..4], @intCast(sections.len), .little);
        std.mem.writeInt(u32, file_header[8..12], symtab_offset, .little);
        std.mem.writeInt(u32, file_header[12..16], table_count, .little);
        return try out.toOwnedSlice(allocator);
    }
};

const coff_text_flags: u32 = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ | (5 << 20);
const coff_rdata_flags: u32 = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | (4 << 20);
const coff_data_flags: u32 = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | (4 << 20);
const coff_bss_flags: u32 = IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | (5 << 20);
const coff_unwind_flags: u32 = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | (3 << 20);
const coff_directive_flags: u32 = IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE | (1 << 20);
const coff_debug_flags: u32 = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE | (1 << 20);

test "an x86_64 COFF object becomes an image with unwind tables and weak aliases" {
    const allocator = std.testing.allocator;
    // entry: call helper; call roc_alloc; lea rax, [rip + table]; nops
    // helper at 24: ret; memcpy's default at 28: ret
    var text: [32]u8 = @splat(0x90);
    text[0] = 0xe8;
    text[5] = 0xe8;
    text[10] = 0x48;
    text[11] = 0x8d;
    text[12] = 0x05;
    @memset(text[1..5], 0);
    @memset(text[6..10], 0);
    @memset(text[13..17], 0);
    text[24] = 0xc3;
    text[28] = 0xc3;
    var strong: [16]u8 = @splat(0x90);
    strong[0] = 0xc3;
    var rdata: [8]u8 = @splat(0);
    std.mem.writeInt(u64, &rdata, 24, .little);
    var pdata: [12]u8 = @splat(0);
    std.mem.writeInt(u32, pdata[4..8], 24, .little);
    var info: [16]u8 = @splat(0);
    std.mem.writeInt(u32, info[8..12], 2, .little);
    const object = try TestCoffObject.encode(allocator, IMAGE_FILE_MACHINE_AMD64, &.{
        .{ .name = ".text", .bytes = &text, .characteristics = coff_text_flags | IMAGE_SCN_LNK_COMDAT, .selection = IMAGE_COMDAT_SELECT_ANY },
        .{ .name = ".text2", .bytes = &strong, .characteristics = coff_text_flags | IMAGE_SCN_LNK_COMDAT, .selection = IMAGE_COMDAT_SELECT_NODUPLICATES },
        .{ .name = ".rdata", .bytes = &rdata, .characteristics = coff_rdata_flags },
        .{ .name = ".data", .bytes = &@as([8]u8, @splat(0)), .characteristics = coff_data_flags },
        .{ .name = ".bss", .size = 16, .characteristics = coff_bss_flags },
        .{ .name = ".pdata", .bytes = &pdata, .characteristics = coff_unwind_flags },
        .{ .name = ".xdata", .bytes = "\x01\x00\x00\x00", .characteristics = coff_unwind_flags },
        .{ .name = ".drectve", .bytes = " /DEFAULTLIB:foo", .characteristics = coff_directive_flags },
        .{ .name = ".debug_abbrev", .bytes = "abbrev", .characteristics = coff_debug_flags },
        .{ .name = ".debug_info", .bytes = &info, .characteristics = coff_debug_flags },
    }, &.{
        .{ .name = ".text", .section = 1, .class = .section },
        .{ .name = "entry", .section = 1, .value = 0, .function = true },
        .{ .name = "helper", .section = 1, .value = 24, .class = .static, .function = true },
        .{ .name = ".weak.memcpy.default", .section = 1, .value = 28, .class = .static, .function = true },
        .{ .name = "memcpy", .section = 0, .class = .weak_external, .tag = 3 },
        .{ .name = ".text2", .section = 2, .class = .section },
        .{ .name = "strong_fn", .section = 2, .value = 0, .function = true },
        .{ .name = "roc_alloc", .section = 0 },
        .{ .name = "table", .section = 3, .value = 0 },
        .{ .name = "counter", .section = 5, .value = 0 },
        .{ .name = ".xdata", .section = 7, .class = .section },
        .{ .name = ".debug_abbrev", .section = 9, .class = .section },
        .{ .name = "roc_expect_err_region", .section = 0, .class = .weak_external, .tag = 7 },
    }, &.{
        .{ .section = 1, .offset = 1, .symbol = 2, .type = IMAGE_REL_AMD64_REL32 },
        .{ .section = 1, .offset = 6, .symbol = 7, .type = IMAGE_REL_AMD64_REL32 },
        .{ .section = 1, .offset = 13, .symbol = 8, .type = IMAGE_REL_AMD64_REL32 },
        .{ .section = 3, .offset = 0, .symbol = 1, .type = IMAGE_REL_AMD64_ADDR64 },
        .{ .section = 4, .offset = 0, .symbol = 9, .type = IMAGE_REL_AMD64_ADDR64 },
        .{ .section = 6, .offset = 0, .symbol = 1, .type = IMAGE_REL_AMD64_ADDR32NB },
        .{ .section = 6, .offset = 4, .symbol = 1, .type = IMAGE_REL_AMD64_ADDR32NB },
        .{ .section = 6, .offset = 8, .symbol = 10, .type = IMAGE_REL_AMD64_ADDR32NB },
        .{ .section = 10, .offset = 0, .symbol = 1, .type = IMAGE_REL_AMD64_ADDR64 },
        .{ .section = 10, .offset = 8, .symbol = 11, .type = IMAGE_REL_AMD64_SECREL },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = true, .atoms = .whole });
    defer allocator.free(image);
    const view = try View.init(image);
    // Text: .text, then .text2 at 32; read-only data at 48.
    try std.testing.expectEqual(@as(u32, 48), view.textSize());
    try std.testing.expectEqual(@as(usize, 56), view.block().len);
    // call helper: 24 - 5; lea table: 48 - 17.
    try std.testing.expectEqual(@as(i32, 19), std.mem.readInt(i32, view.block()[1..5], .little));
    try std.testing.expectEqual(@as(i32, 31), std.mem.readInt(i32, view.block()[13..17], .little));
    try std.testing.expectEqualSlices(u8, &pdata, view.pdata());
    try std.testing.expectEqualStrings("\x01\x00\x00\x00", view.xdata());
    try std.testing.expectEqualStrings("abbrev", view.debugAbbrev());
    // The import: roc_alloc's call, relative to the end of its field.
    try std.testing.expectEqual(@as(usize, 1), view.imports().len);
    try std.testing.expectEqualStrings("roc_alloc", view.name(view.imports()[0].name));
    const call = view.importSites(view.imports()[0]);
    try std.testing.expectEqual(@as(usize, 1), call.len);
    try std.testing.expectEqual(@backingInt(SiteKind.pc32), call[0].kind);
    try std.testing.expectEqual(@as(u32, 6), call[0].offset);
    try std.testing.expectEqual(@as(i64, -4), call[0].addend);
    // Local sites: the table's pointer, the data's pointer, the three
    // unwind fields, then the debug fields.
    const local = view.localSites();
    try std.testing.expectEqual(@as(usize, 7), local.len);
    try std.testing.expectEqual(@backingInt(SiteKind.abs64), local[0].kind);
    try std.testing.expectEqual(@as(u32, 48), local[0].offset);
    try std.testing.expectEqual(@backingInt(Region.block), local[0].target_region);
    try std.testing.expectEqual(@as(i64, 24), local[0].addend);
    try std.testing.expectEqual(@backingInt(Region.data), local[1].region);
    try std.testing.expectEqual(@backingInt(Region.bss), local[1].target_region);
    for (local[2..5], [_]u32{ 0, 4, 8 }, [_]Region{ .block, .block, .xdata }, [_]i64{ 0, 24, 0 }) |site, offset, target, addend| {
        try std.testing.expectEqual(@backingInt(SiteKind.rva32), site.kind);
        try std.testing.expectEqual(@backingInt(Region.pdata), site.region);
        try std.testing.expectEqual(offset, site.offset);
        try std.testing.expectEqual(@backingInt(target), site.target_region);
        try std.testing.expectEqual(addend, site.addend);
    }
    try std.testing.expectEqual(@backingInt(SiteKind.abs64), local[5].kind);
    try std.testing.expectEqual(@backingInt(Region.debug_info), local[5].region);
    try std.testing.expectEqual(@backingInt(SiteKind.abs32), local[6].kind);
    try std.testing.expectEqual(@backingInt(Region.debug_abbrev), local[6].target_region);
    try std.testing.expectEqual(@as(i64, 2), local[6].addend);
    // Exports: entry is weak (a COMDAT any section), strong_fn is not,
    // memcpy is its default under the weak name; the statics are not
    // exported, and the weak reference to a missing default is no import.
    try std.testing.expectEqual(@as(usize, 5), view.exports().len);
    const entry = view.findExport("entry").?;
    try std.testing.expectEqual(symbol_exported | symbol_weak, entry.flags);
    try std.testing.expectEqual(@as(u32, 24), entry.size);
    try std.testing.expectEqual(symbol_exported, view.findExport("strong_fn").?.flags);
    const memcpy = view.findExport("memcpy").?;
    try std.testing.expectEqual(@as(u32, 28), memcpy.value);
    try std.testing.expectEqual(@backingInt(SymbolType.func), memcpy.type);
    try std.testing.expectEqual(symbol_exported | symbol_weak, memcpy.flags);
    try std.testing.expect(view.findExport("helper") == null);
    try std.testing.expectEqual(@as(u32, 16), view.findExport("counter").?.size);
    try std.testing.expectEqual(@as(u32, 16), view.bssSize());
    const again = try build(allocator, &.{object}, .{ .keep_debug = true, .atoms = .whole });
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, image, again);
}

test "an arm64 COFF object keeps its instruction addends" {
    const allocator = std.testing.allocator;
    // entry: bl roc_alloc; adrp/add x16, table + 4; adrp/ldr x16, counter + 8; ret
    const text = words(&.{ bl_instruction, adrp_x16, add_x16_x16 | (4 << 10), adrp_x16, ldr_x16_x16 | (1 << 10), ret_instruction });
    var pdata: [8]u8 = @splat(0);
    const object = try TestCoffObject.encode(allocator, IMAGE_FILE_MACHINE_ARM64, &.{
        .{ .name = ".text", .bytes = &text, .characteristics = coff_text_flags },
        .{ .name = ".rdata", .bytes = "constant", .characteristics = coff_rdata_flags },
        .{ .name = ".data", .bytes = &@as([16]u8, @splat(0)), .characteristics = coff_data_flags },
        .{ .name = ".pdata", .bytes = &pdata, .characteristics = coff_unwind_flags },
        .{ .name = ".xdata", .bytes = "\x00\x00\x00\x00", .characteristics = coff_unwind_flags },
    }, &.{
        .{ .name = "entry", .section = 1, .value = 0, .function = true },
        .{ .name = "roc_alloc", .section = 0 },
        .{ .name = "table", .section = 2, .value = 0 },
        .{ .name = "counter", .section = 3, .value = 0 },
        .{ .name = ".xdata", .section = 5, .class = .section },
    }, &.{
        .{ .section = 1, .offset = 0, .symbol = 1, .type = IMAGE_REL_ARM64_BRANCH26 },
        .{ .section = 1, .offset = 4, .symbol = 2, .type = IMAGE_REL_ARM64_PAGEBASE_REL21 },
        .{ .section = 1, .offset = 8, .symbol = 2, .type = IMAGE_REL_ARM64_PAGEOFFSET_12A },
        .{ .section = 1, .offset = 12, .symbol = 3, .type = IMAGE_REL_ARM64_PAGEBASE_REL21 },
        .{ .section = 1, .offset = 16, .symbol = 3, .type = IMAGE_REL_ARM64_PAGEOFFSET_12L },
        .{ .section = 4, .offset = 0, .symbol = 0, .type = IMAGE_REL_ARM64_ADDR32NB },
        .{ .section = 4, .offset = 4, .symbol = 4, .type = IMAGE_REL_ARM64_ADDR32NB },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqual(@as(u32, 24), view.textSize());
    try std.testing.expectEqualStrings("constant", view.block()[24..32]);
    // table + 4 in the block: same page, low bits 28.
    try std.testing.expectEqual(adrp_x16, std.mem.readInt(u32, view.block()[4..8], .little));
    try std.testing.expectEqual(add_x16_x16 | (28 << 10), std.mem.readInt(u32, view.block()[8..12], .little));
    const branch = view.importSites(view.imports()[0]);
    try std.testing.expectEqual(@backingInt(SiteKind.branch26), branch[0].kind);
    try std.testing.expectEqual(@as(i64, 0), branch[0].addend);
    // counter + 8 in the data: a page site and a scaled offset site.
    const local = view.localSites();
    try std.testing.expectEqual(@as(usize, 4), local.len);
    try std.testing.expectEqual(@backingInt(SiteKind.page21), local[0].kind);
    try std.testing.expectEqual(@as(i64, 0), local[0].addend);
    try std.testing.expectEqual(@backingInt(SiteKind.pageoff12), local[1].kind);
    try std.testing.expectEqual(@as(i64, 8), local[1].addend);
    try std.testing.expectEqual(@backingInt(Region.data), local[1].target_region);
    try std.testing.expectEqual(@backingInt(Region.pdata), local[2].region);
    try std.testing.expectEqual(@backingInt(Region.xdata), local[3].target_region);
}

test "an image-relative site holds the value it is given" {
    var field: [4]u8 = @splat(0xff);
    try std.testing.expect(applySite(&field, .rva32, 0x1234, 0x1_0000_0000));
    try std.testing.expectEqual(@as(u32, 0x1234), std.mem.readInt(u32, &field, .little));
    try std.testing.expect(!applySite(&field, .rva32, 1 << 32, 0));
    try std.testing.expect(!applySite(&field, .rva32, -1, 0));
}

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
    const image = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
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
    const again = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
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
    const image = try build(allocator, &.{ first, second }, .{ .keep_debug = false, .atoms = .whole });
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
    const image = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(image);
    try std.testing.expectError(error.InvalidImage, View.init(image[0 .. image.len - 16]));
    image[0] = 'X';
    try std.testing.expectError(error.InvalidImage, View.init(image));
}

test "atoms by symbol record every reference between them" {
    const allocator = std.testing.allocator;
    var text: [24]u8 = @splat(0x90);
    const object = try TestObject.encode(allocator, &.{
        .{ .name = ".text", .flags = SHF_ALLOC | SHF_EXECINSTR, .bytes = &text },
        .{ .name = ".rodata", .flags = SHF_ALLOC, .bytes = "constant", .addralign = 8 },
        .{ .name = ".data", .flags = SHF_ALLOC | SHF_WRITE, .bytes = &@as([8]u8, @splat(0)), .addralign = 8 },
    }, &.{
        .{ .name = "first", .section = 1, .value = 0, .size = 8, .global = true, .func = true },
        .{ .name = "second", .section = 1, .value = 8, .size = 8, .func = true },
        .{ .name = "third", .section = 1, .value = 16, .size = 8, .func = true },
        .{ .name = "constant", .section = 2, .value = 0, .size = 8 },
        .{ .name = "roc_alloc", .section = 0, .global = true },
    }, &.{
        .{ .section = 1, .offset = 1, .symbol = 2, .type = R_X86_64_PLT32, .addend = -4 },
        .{ .section = 1, .offset = 9, .symbol = 5, .type = R_X86_64_PLT32, .addend = -4 },
        .{ .section = 1, .offset = 12, .symbol = 4, .type = R_X86_64_PC32, .addend = -4 },
        .{ .section = 3, .offset = 0, .symbol = 3, .type = R_X86_64_64, .addend = 0 },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .{ .symbols = &.{.{ .site = 17, .target = 0 }} } });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqualSlices(u32, &.{ 0, 8, 16, 24 }, view.atoms());
    try std.testing.expectEqualSlices(u32, &.{1}, view.atomEdges(0));
    try std.testing.expectEqualSlices(u32, &.{ 3, edge_import | 0 }, view.atomEdges(1));
    try std.testing.expectEqualSlices(u32, &.{0}, view.atomEdges(2));
    try std.testing.expectEqualSlices(u32, &.{}, view.atomEdges(3));
    try std.testing.expectEqualSlices(u32, &.{2}, view.roots());
    try std.testing.expectEqual(@as(u32, 1), view.importSites(view.imports()[0])[0].atom);
    try std.testing.expectEqual(no_atom, view.localSites()[0].atom);
    try std.testing.expectEqual(@as(u32, 24), view.atomRange(3).start);
    try std.testing.expectEqual(@as(u32, 32), view.atomRange(3).end);
}

const bl_instruction: u32 = 0x9400_0000;
const adrp_x16: u32 = 0x9000_0010;
const add_x16_x16: u32 = 0x9100_0210;
const ldr_x16_x16: u32 = 0xf940_0210;
const ret_instruction: u32 = 0xd65f_03c0;

fn words(comptime list: []const u32) [list.len * 4]u8 {
    var out: [list.len * 4]u8 = undefined;
    for (list, 0..) |word, index| std.mem.writeInt(u32, out[index * 4 ..][0..4], word, .little);
    return out;
}

test "an arm64 Mach-O object becomes an image with its C names" {
    const allocator = std.testing.allocator;
    // _entry: bl _helper; adrp/add x16, _constant; adrp/ldr x16, _counter@GOT; bl _roc_alloc; ret
    // _helper: ret
    const text = words(&.{ bl_instruction, adrp_x16, add_x16_x16, adrp_x16, ldr_x16_x16, bl_instruction, ret_instruction, ret_instruction });
    const object = try TestMachObject.encode(allocator, &.{
        .{ .segname = "__TEXT", .sectname = "__text", .bytes = &text, .flags = S_ATTR_PURE_INSTRUCTIONS },
        .{ .segname = "__TEXT", .sectname = "__const", .bytes = "constant", .@"align" = 3 },
        .{ .segname = "__DATA", .sectname = "__const", .bytes = &@as([8]u8, @splat(0)), .@"align" = 3 },
        .{ .segname = "__DATA", .sectname = "__bss", .size = 16, .@"align" = 3, .flags = S_ZEROFILL },
        .{ .segname = "__LD", .sectname = "__compact_unwind", .bytes = &@as([32]u8, @splat(0)), .@"align" = 3 },
    }, &.{
        .{ .name = "_entry", .section = 1, .value = 0, .external = true },
        .{ .name = "_helper", .section = 1, .value = 28, .external = true, .private = true },
        .{ .name = "_constant", .section = 2, .value = 0 },
        .{ .name = "_counter", .section = 4, .value = 0, .external = true },
        .{ .name = "_roc_alloc", .section = 0, .external = true },
        .{ .name = "_table", .section = 3, .value = 0, .external = true },
        .{ .name = "_roc_expect_err_region", .section = 0, .external = true, .weak_ref = true },
    }, &.{
        .{ .section = 1, .offset = 0, .symbol = 2, .type = ARM64_RELOC_BRANCH26, .pcrel = true },
        .{ .section = 1, .offset = 4, .symbol = 3, .type = ARM64_RELOC_PAGE21, .pcrel = true, .addend = 4 },
        .{ .section = 1, .offset = 8, .symbol = 3, .type = ARM64_RELOC_PAGEOFF12, .pcrel = false, .addend = 4 },
        .{ .section = 1, .offset = 12, .symbol = 4, .type = ARM64_RELOC_GOT_LOAD_PAGE21, .pcrel = true },
        .{ .section = 1, .offset = 16, .symbol = 4, .type = ARM64_RELOC_GOT_LOAD_PAGEOFF12, .pcrel = false },
        .{ .section = 1, .offset = 20, .symbol = 5, .type = ARM64_RELOC_BRANCH26, .pcrel = true },
        .{ .section = 3, .offset = 0, .symbol = 1, .type = ARM64_RELOC_UNSIGNED, .pcrel = false, .length = 3, .addend = 28 },
        .{ .section = 5, .offset = 0, .symbol = 1, .type = ARM64_RELOC_UNSIGNED, .pcrel = false, .length = 3 },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqual(@as(u32, 32), view.textSize());
    try std.testing.expectEqualStrings("constant", view.block()[32..40]);
    // bl _helper at 0 reaches 28: 7 words forward.
    try std.testing.expectEqual(bl_instruction | 7, std.mem.readInt(u32, view.block()[0..4], .little));
    // adrp/add x16, _constant + 4: same page, low bits 36.
    try std.testing.expectEqual(adrp_x16, std.mem.readInt(u32, view.block()[4..8], .little));
    try std.testing.expectEqual(add_x16_x16 | (36 << 10), std.mem.readInt(u32, view.block()[8..12], .little));
    // The GOT slot follows the writable data; the load's page sites point
    // at it, and its own site holds _counter's address in the zero-fill.
    try std.testing.expectEqual(@as(usize, 16), view.data().len);
    const local = view.localSites();
    try std.testing.expectEqual(@as(usize, 4), local.len);
    try std.testing.expectEqual(@backingInt(SiteKind.page21), local[0].kind);
    try std.testing.expectEqual(@backingInt(Region.data), local[0].target_region);
    try std.testing.expectEqual(@as(i64, 8), local[0].addend);
    try std.testing.expectEqual(@backingInt(SiteKind.pageoff12), local[1].kind);
    try std.testing.expectEqual(@as(u32, 16), local[1].offset);
    // _table holds _entry + 28 (an absolute pointer into the block).
    try std.testing.expectEqual(@backingInt(SiteKind.abs64), local[2].kind);
    try std.testing.expectEqual(@backingInt(Region.data), local[2].region);
    try std.testing.expectEqual(@as(i64, 28), local[2].addend);
    try std.testing.expectEqual(@backingInt(Region.block), local[2].target_region);
    // The slot: _counter in the zero-fill.
    try std.testing.expectEqual(@as(u32, 8), local[3].offset);
    try std.testing.expectEqual(@backingInt(Region.bss), local[3].target_region);
    // Imports: the strong roc_alloc branch, named without its underscore.
    try std.testing.expectEqual(@as(usize, 1), view.imports().len);
    try std.testing.expectEqualStrings("roc_alloc", view.name(view.imports()[0].name));
    try std.testing.expectEqual(@as(u32, 0), view.imports()[0].flags & symbol_weak);
    try std.testing.expectEqual(@backingInt(SiteKind.branch26), view.importSites(view.imports()[0])[0].kind);
    try std.testing.expectEqual(@as(u32, 20), view.importSites(view.imports()[0])[0].offset);
    // Exports: entry, counter and table; helper is private.
    try std.testing.expectEqual(@as(usize, 3), view.exports().len);
    const entry = view.findExport("entry").?;
    try std.testing.expectEqual(@backingInt(SymbolType.func), entry.type);
    try std.testing.expectEqual(@as(u32, 28), entry.size);
    try std.testing.expect(view.findExport("helper") == null);
    try std.testing.expectEqual(@backingInt(SymbolType.object), view.findExport("table").?.type);
    try std.testing.expectEqual(@as(u32, 16), view.findExport("counter").?.size);
    try std.testing.expectEqual(@as(u32, 16), view.bssSize());
    const again = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, image, again);
}

test "a Mach-O debug section relocates text addresses by section" {
    const allocator = std.testing.allocator;
    const text = words(&.{ret_instruction});
    var info: [16]u8 = @splat(0);
    const object = try TestMachObject.encode(allocator, &.{
        .{ .segname = "__TEXT", .sectname = "__text", .bytes = &text, .flags = S_ATTR_PURE_INSTRUCTIONS },
        .{ .segname = "__DWARF", .sectname = "__debug_info", .bytes = &info, .@"align" = 0, .flags = S_ATTR_DEBUG },
        .{ .segname = "__DWARF", .sectname = "__debug_abbrev", .bytes = "abbrev", .@"align" = 0, .flags = S_ATTR_DEBUG },
    }, &.{
        .{ .name = "_f", .section = 1, .value = 0, .external = true },
    }, &.{
        .{ .section = 2, .offset = 8, .symbol = 1, .external = false, .type = ARM64_RELOC_UNSIGNED, .pcrel = false, .length = 3 },
    });
    defer allocator.free(object);
    const image = try build(allocator, &.{object}, .{ .keep_debug = true, .atoms = .whole });
    defer allocator.free(image);
    const view = try View.init(image);
    try std.testing.expectEqualStrings("abbrev", view.debugAbbrev());
    try std.testing.expectEqual(@as(usize, 1), view.localSites().len);
    try std.testing.expectEqual(@backingInt(Region.debug_info), view.localSites()[0].region);
    try std.testing.expectEqual(@backingInt(Region.block), view.localSites()[0].target_region);
    try std.testing.expectEqual(@as(u32, 8), view.localSites()[0].offset);
    const dropped = try build(allocator, &.{object}, .{ .keep_debug = false, .atoms = .whole });
    defer allocator.free(dropped);
    try std.testing.expectEqual(@as(usize, 0), (try View.init(dropped)).debugInfo().len);
}

test "arm64 sites encode branches, pages and scaled offsets" {
    var field = words(&.{bl_instruction});
    try std.testing.expect(applySite(&field, .branch26, 0x1000, 0x2000));
    try std.testing.expectEqual(bl_instruction | (@as(u32, @as(u26, @bitCast(@as(i26, -0x400))))), std.mem.readInt(u32, &field, .little));
    try std.testing.expect(!applySite(&field, .branch26, 0x1002, 0x2000));
    try std.testing.expect(!applySite(&field, .branch26, 0x8000_0000, 0));

    field = words(&.{adrp_x16});
    try std.testing.expect(applySite(&field, .page21, 0x7123, 0x4ffc));
    // Pages 7 and 4: 3 pages forward, immlo 3, immhi 0.
    try std.testing.expectEqual(adrp_x16 | (3 << 29), std.mem.readInt(u32, &field, .little));
    try std.testing.expect(applySite(&field, .page21, 0x0123, 0x5ffc));
    try std.testing.expectEqual(adrp_x16 | (@as(u32, @as(u21, @bitCast(@as(i21, -5)))) & 3) << 29 | (@as(u32, @as(u21, @bitCast(@as(i21, -5)))) >> 2) << 5, std.mem.readInt(u32, &field, .little));

    field = words(&.{add_x16_x16});
    try std.testing.expect(applySite(&field, .pageoff12, 0x7123, 0));
    try std.testing.expectEqual(add_x16_x16 | (0x123 << 10), std.mem.readInt(u32, &field, .little));
    field = words(&.{ldr_x16_x16});
    try std.testing.expect(applySite(&field, .pageoff12, 0x7128, 0));
    try std.testing.expectEqual(ldr_x16_x16 | ((0x128 >> 3) << 10), std.mem.readInt(u32, &field, .little));
    try std.testing.expect(!applySite(&field, .pageoff12, 0x7124, 0));
    // A 128-bit vector load scales by 16.
    const ldr_q0_x16: u32 = 0x3dc0_0200;
    field = words(&.{ldr_q0_x16});
    try std.testing.expect(applySite(&field, .pageoff12, 0x7130, 0));
    try std.testing.expectEqual(ldr_q0_x16 | ((0x130 >> 4) << 10), std.mem.readInt(u32, &field, .little));
    field = words(&.{ret_instruction});
    try std.testing.expect(!applySite(&field, .pageoff12, 0, 0));
}
