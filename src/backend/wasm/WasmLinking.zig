//! Data structures for WASM relocatable module metadata.
//!
//! These structures represent the `linking` and `reloc.*` custom sections
//! defined by the WebAssembly Tool Conventions:
//! https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md
//!
//! They form the "map" that makes surgical linking possible: the symbol table
//! records what symbols exist and the relocation entries record where each
//! symbol is referenced so we can patch those sites in-place.

const std = @import("std");
const WasmModule = @import("WasmModule.zig");
const Import = WasmModule.Import;

// --- Relocation Types ---

/// Index-based relocation types (no addend).
/// These patch indices in instructions like `call`, `global.get`, `call_indirect`.
pub const IndexRelocType = enum(u8) {
    function_index_leb = 0, // R_WASM_FUNCTION_INDEX_LEB — function index in `call`
    table_index_sleb = 1, // R_WASM_TABLE_INDEX_SLEB — signed table index in `i32.const`
    table_index_i32 = 2, // R_WASM_TABLE_INDEX_I32 — table index as raw u32 in data
    type_index_leb = 6, // R_WASM_TYPE_INDEX_LEB — type index in `call_indirect`
    global_index_leb = 7, // R_WASM_GLOBAL_INDEX_LEB — global index in `global.get/set`
    event_index_leb = 10, // R_WASM_EVENT_INDEX_LEB
    global_index_i32 = 13, // R_WASM_GLOBAL_INDEX_I32
    table_number_leb = 20, // R_WASM_TABLE_NUMBER_LEB
};

/// Offset-based relocation types (have an addend).
/// These patch memory addresses in load/store instructions and data segments.
pub const OffsetRelocType = enum(u8) {
    memory_addr_leb = 3, // R_WASM_MEMORY_ADDR_LEB — unsigned addr in load/store
    memory_addr_sleb = 4, // R_WASM_MEMORY_ADDR_SLEB — signed addr in `i32.const`
    memory_addr_i32 = 5, // R_WASM_MEMORY_ADDR_I32 — raw u32 addr in data segment
    function_offset_i32 = 8, // R_WASM_FUNCTION_OFFSET_I32
    section_offset_i32 = 9, // R_WASM_SECTION_OFFSET_I32
    memory_addr_rel_sleb = 11, // R_WASM_MEMORY_ADDR_REL_SLEB — PIC relative signed addr
};

// --- Relocation Entry ---

/// A single relocation entry. Describes one site in the code or data section
/// that references a symbol and needs patching when that symbol's value changes.
pub const RelocationEntry = union(enum) {
    /// Index relocations: the value at `offset` is a symbol index (function, type, global).
    /// No addend — the patched value is the symbol's resolved index directly.
    index: struct {
        type_id: IndexRelocType,
        offset: u32, // byte offset within the target section body
        symbol_index: u32, // index into the linking section's symbol table
    },

    /// Offset relocations: the value at `offset` is a memory address.
    /// The patched value is the symbol's address + addend.
    offset: struct {
        type_id: OffsetRelocType,
        offset: u32,
        symbol_index: u32,
        addend: i32,
    },

    pub fn getSymbolIndex(self: RelocationEntry) u32 {
        return switch (self) {
            .index => |i| i.symbol_index,
            .offset => |o| o.symbol_index,
        };
    }

    pub fn getOffset(self: RelocationEntry) u32 {
        return switch (self) {
            .index => |i| i.offset,
            .offset => |o| o.offset,
        };
    }

    /// Parse a single relocation entry from bytes at cursor position.
    pub fn parse(bytes: []const u8, cursor: *usize) WasmModule.ParseError!RelocationEntry {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const type_byte = bytes[cursor.*];
        cursor.* += 1;
        const offset = try WasmModule.readU32(bytes, cursor);
        const symbol_index = try WasmModule.readU32(bytes, cursor);

        // Try index relocation types first (no addend)
        if (std.meta.intToEnum(IndexRelocType, type_byte)) |type_id| {
            return .{ .index = .{
                .type_id = type_id,
                .offset = offset,
                .symbol_index = symbol_index,
            } };
        } else |_| {}

        // Try offset relocation types (with addend)
        if (std.meta.intToEnum(OffsetRelocType, type_byte)) |type_id| {
            const addend = try WasmModule.readI32(bytes, cursor);
            return .{ .offset = .{
                .type_id = type_id,
                .offset = offset,
                .symbol_index = symbol_index,
                .addend = addend,
            } };
        } else |_| {}

        return error.InvalidSection;
    }
};

// --- Symbol Info ---

/// Flags for symbol table entries.
pub const SymFlag = struct {
    pub const BINDING_WEAK: u32 = 0x01;
    pub const BINDING_LOCAL: u32 = 0x02;
    pub const VISIBILITY_HIDDEN: u32 = 0x04;
    pub const UNDEFINED: u32 = 0x10;
    pub const EXPORTED: u32 = 0x20;
    pub const EXPLICIT_NAME: u32 = 0x40;
    pub const NO_STRIP: u32 = 0x80;
};

/// Symbol kinds in the linking section's symbol table.
pub const SymKind = enum(u8) {
    function = 0,
    data = 1,
    global = 2,
    section = 3,
    event = 4,
    table = 5,
};

/// A symbol table entry. Each symbol has a kind, flags, and an index
/// into the relevant index space (function index, global index, etc.).
///
/// Function/global symbols can be **explicitly named** (name stored in the linking
/// section) or **implicitly named** (undefined symbols that inherit their name from
/// the import section entry they reference).
///
/// Parsing rule: a function/global symbol gets a name from the linking section if
/// `(flags & WASM_SYM_EXPLICIT_NAME) != 0` OR `(flags & WASM_SYM_UNDEFINED) == 0`
/// (i.e. defined symbols always have names). Undefined symbols without EXPLICIT_NAME
/// have `name = null` — their name must be looked up from the import section at the
/// symbol's `index`.
pub const SymInfo = struct {
    kind: SymKind,
    flags: u32,
    /// Explicit name from the linking section, or null for implicitly-named
    /// imported symbols (whose name comes from the import section).
    name: ?[]const u8,
    /// For function symbols: the function index (import or defined).
    /// For global symbols: the global index.
    /// For data symbols: segment index (stored here, offset/size stored separately).
    index: u32,
    /// Data symbols only: offset within segment.
    data_offset: u32 = 0,
    /// Data symbols only: size in bytes.
    data_size: u32 = 0,

    pub fn isUndefined(self: SymInfo) bool {
        return (self.flags & SymFlag.UNDEFINED) != 0;
    }

    pub fn isImplicitlyNamed(self: SymInfo) bool {
        return self.name == null;
    }

    pub fn isLocal(self: SymInfo) bool {
        return (self.flags & SymFlag.BINDING_LOCAL) != 0;
    }

    pub fn isFunction(self: SymInfo) bool {
        return self.kind == .function;
    }

    /// Parse a single symbol table entry from the linking section.
    pub fn parse(bytes: []const u8, cursor: *usize) WasmModule.ParseError!SymInfo {
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const kind_byte = bytes[cursor.*];
        cursor.* += 1;
        const kind = std.meta.intToEnum(SymKind, kind_byte) catch return error.InvalidSection;
        const flags = try WasmModule.readU32(bytes, cursor);

        switch (kind) {
            .function, .global, .event, .table => {
                const index = try WasmModule.readU32(bytes, cursor);
                const is_import = (flags & SymFlag.UNDEFINED) != 0;
                const has_explicit_name = (flags & SymFlag.EXPLICIT_NAME) != 0;
                // Defined symbols always have names; undefined ones only if EXPLICIT_NAME is set
                const name: ?[]const u8 = if (!is_import or has_explicit_name)
                    try WasmModule.readString(bytes, cursor)
                else
                    null;
                return .{ .kind = kind, .flags = flags, .name = name, .index = index };
            },
            .data => {
                const name = try WasmModule.readString(bytes, cursor);
                if ((flags & SymFlag.UNDEFINED) != 0) {
                    // Imported data symbol — no segment info
                    return .{ .kind = kind, .flags = flags, .name = name, .index = 0 };
                }
                const segment_index = try WasmModule.readU32(bytes, cursor);
                const data_offset = try WasmModule.readU32(bytes, cursor);
                const data_size = try WasmModule.readU32(bytes, cursor);
                return .{
                    .kind = kind,
                    .flags = flags,
                    .name = name,
                    .index = segment_index,
                    .data_offset = data_offset,
                    .data_size = data_size,
                };
            },
            .section => {
                const index = try WasmModule.readU32(bytes, cursor);
                return .{ .kind = kind, .flags = flags, .name = null, .index = index };
            },
        }
    }

    /// Resolve this symbol's name when one is available.
    ///
    /// Explicitly named symbols return their stored name.
    /// Implicitly named undefined function/global/event/table symbols inherit
    /// their name from the import section.
    /// Section symbols and other unnamed non-import symbols return null.
    pub fn resolveName(self: SymInfo, imports: []const Import) ?[]const u8 {
        if (self.name) |n| return n;

        if (!self.isUndefined()) return null;

        return switch (self.kind) {
            .function, .global, .event, .table => imports[self.index].field_name,
            else => null,
        };
    }
};

// --- Relocation Section ---

/// Holds all relocation entries for one section (either "reloc.CODE" or "reloc.DATA").
pub const RelocationSection = struct {
    /// Name of this reloc section (e.g. "reloc.CODE").
    name: []const u8,
    /// Index of the target section these relocations apply to.
    target_section_index: u32,
    /// The relocation entries, sorted by offset.
    entries: std.ArrayList(RelocationEntry),

    /// Parse a relocation section from its body bytes (after the section name has
    /// been consumed). `name` is the section name (e.g. "reloc.CODE").
    pub fn parse(
        allocator: std.mem.Allocator,
        name: []const u8,
        bytes: []const u8,
        cursor: *usize,
        section_end: usize,
    ) WasmModule.ParseError!RelocationSection {
        const target_section_index = try WasmModule.readU32(bytes, cursor);
        const count = try WasmModule.readU32(bytes, cursor);

        var entries: std.ArrayList(RelocationEntry) = .empty;
        errdefer entries.deinit(allocator);
        try entries.ensureTotalCapacity(allocator, count);

        for (0..count) |_| {
            if (cursor.* > section_end) return error.UnexpectedEnd;
            const entry = try RelocationEntry.parse(bytes, cursor);
            entries.appendAssumeCapacity(entry);
        }

        return .{
            .name = name,
            .target_section_index = target_section_index,
            .entries = entries,
        };
    }

    /// Patch all sites in `section_bytes` that reference `sym_index` with `value`.
    /// This is the core surgical linking primitive.
    pub fn applyRelocsU32(
        self: *const RelocationSection,
        section_bytes: []u8,
        sym_index: u32,
        value: u32,
    ) void {
        for (self.entries.items) |entry| {
            if (entry.getSymbolIndex() != sym_index) continue;
            switch (entry) {
                .index => |idx| {
                    switch (idx.type_id) {
                        .function_index_leb,
                        .type_index_leb,
                        .global_index_leb,
                        .event_index_leb,
                        .table_number_leb,
                        => WasmModule.overwritePaddedU32(section_bytes, idx.offset, value),
                        .table_index_sleb => WasmModule.overwritePaddedI32(
                            section_bytes,
                            idx.offset,
                            @as(i32, @intCast(value)),
                        ),
                        .table_index_i32, .global_index_i32 => {
                            const off: usize = @intCast(idx.offset);
                            std.mem.writeInt(u32, section_bytes[off..][0..4], value, .little);
                        },
                    }
                },
                .offset => |off| {
                    const patched = @as(i64, value) + @as(i64, off.addend);
                    switch (off.type_id) {
                        .memory_addr_leb => WasmModule.overwritePaddedU32(
                            section_bytes,
                            off.offset,
                            @intCast(patched),
                        ),
                        .memory_addr_sleb,
                        .memory_addr_rel_sleb,
                        => WasmModule.overwritePaddedI32(
                            section_bytes,
                            off.offset,
                            @intCast(patched),
                        ),
                        .memory_addr_i32,
                        .function_offset_i32,
                        .section_offset_i32,
                        => {
                            const o: usize = @intCast(off.offset);
                            std.mem.writeInt(u32, section_bytes[o..][0..4], @intCast(patched), .little);
                        },
                    }
                },
            }
        }
    }
};

// --- Linking Section ---

/// Version of the linking metadata format (tool conventions v2).
pub const LINKING_VERSION: u32 = 2;

/// Linking subsection types (within the "linking" custom section).
pub const LinkingSubsection = enum(u8) {
    segment_info = 5,
    init_funcs = 6,
    comdat_info = 7,
    symbol_table = 8,
};

/// Container for all linking metadata from a relocatable WASM module.
pub const LinkingSection = struct {
    symbol_table: std.ArrayList(SymInfo),
    segment_info: std.ArrayList(SegmentInfo),
    init_funcs: std.ArrayList(InitFunc),

    /// Parse a linking section body (after the "linking" name has been consumed).
    pub fn parse(
        allocator: std.mem.Allocator,
        bytes: []const u8,
        cursor: *usize,
        section_end: usize,
    ) WasmModule.ParseError!LinkingSection {
        // Version must be 2
        if (cursor.* >= bytes.len) return error.UnexpectedEnd;
        const version = bytes[cursor.*];
        cursor.* += 1;
        if (version != LINKING_VERSION) return error.InvalidLinkingVersion;

        var symbol_table: std.ArrayList(SymInfo) = .empty;
        errdefer symbol_table.deinit(allocator);
        var segment_info: std.ArrayList(SegmentInfo) = .empty;
        errdefer segment_info.deinit(allocator);
        var init_funcs: std.ArrayList(InitFunc) = .empty;
        errdefer init_funcs.deinit(allocator);

        while (cursor.* < section_end) {
            if (cursor.* >= bytes.len) return error.UnexpectedEnd;
            const subsection_id = bytes[cursor.*];
            cursor.* += 1;
            const subsection_len = try WasmModule.readU32(bytes, cursor);
            const subsection_end = cursor.* + subsection_len;

            if (std.meta.intToEnum(LinkingSubsection, subsection_id)) |sub| {
                switch (sub) {
                    .symbol_table => {
                        const count = try WasmModule.readU32(bytes, cursor);
                        try symbol_table.ensureTotalCapacity(allocator, count);
                        for (0..count) |_| {
                            const sym = try SymInfo.parse(bytes, cursor);
                            symbol_table.appendAssumeCapacity(sym);
                        }
                    },
                    .segment_info => {
                        const count = try WasmModule.readU32(bytes, cursor);
                        try segment_info.ensureTotalCapacity(allocator, count);
                        for (0..count) |_| {
                            const name = try WasmModule.readString(bytes, cursor);
                            const alignment = try WasmModule.readU32(bytes, cursor);
                            const flags = try WasmModule.readU32(bytes, cursor);
                            segment_info.appendAssumeCapacity(.{
                                .name = name,
                                .alignment = alignment,
                                .flags = flags,
                            });
                        }
                    },
                    .init_funcs, .comdat_info => {
                        // Skip these subsections for now
                        cursor.* = subsection_end;
                    },
                }
            } else |_| {
                // Unknown subsection, skip
                cursor.* = subsection_end;
            }
        }

        return .{
            .symbol_table = symbol_table,
            .segment_info = segment_info,
            .init_funcs = init_funcs,
        };
    }

    /// Find a symbol by name. For implicitly-named imported symbols, resolves
    /// the name from the import section. Returns the symbol index, or null.
    pub fn findSymbolByName(
        self: *const LinkingSection,
        name: []const u8,
        imports: []const Import,
    ) ?u32 {
        for (self.symbol_table.items, 0..) |sym, i| {
            if (sym.resolveName(imports)) |sym_name| {
                if (std.mem.eql(u8, sym_name, name)) return @intCast(i);
            }
        }
        return null;
    }

    /// Find the symbol table index for an imported function at the given function index.
    pub fn findImportedFnSymIndex(self: *const LinkingSection, fn_index: u32) ?u32 {
        for (self.symbol_table.items, 0..) |sym, i| {
            if (sym.kind == .function and sym.isUndefined() and sym.index == fn_index) {
                return @intCast(i);
            }
        }
        return null;
    }

    /// Find the symbol for an imported function at `old_fn_index` and update it
    /// to point to `new_fn_index`. Returns the symbol index.
    pub fn findAndReindexImportedFn(
        self: *LinkingSection,
        old_fn_index: u32,
        new_fn_index: u32,
    ) ?u32 {
        for (self.symbol_table.items, 0..) |*sym, i| {
            if (sym.kind == .function and sym.isUndefined() and sym.index == old_fn_index) {
                sym.index = new_fn_index;
                return @intCast(i);
            }
        }
        return null;
    }
};

/// Metadata for a data segment from the linking section.
pub const SegmentInfo = struct {
    name: []const u8,
    alignment: u32,
    flags: u32,
};

/// An initialization function entry from the linking section.
pub const InitFunc = struct {
    priority: u32,
    symbol_index: u32,
};

// --- Tests ---

const testing = std.testing;

test "RelocationSection.applyRelocsU32 — patches function_index_leb at correct offset" {
    // Set up a 10-byte buffer with 5 zero bytes at offset 2 (the relocation site)
    var buf = [_]u8{ 0xAA, 0xBB, 0x80, 0x80, 0x80, 0x80, 0x00, 0xCC, 0xDD, 0xEE };

    var entries: std.ArrayList(RelocationEntry) = .empty;
    defer entries.deinit(testing.allocator);
    try entries.append(testing.allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 2,
        .symbol_index = 0,
    } });

    const reloc = RelocationSection{
        .name = "reloc.CODE",
        .target_section_index = 0,
        .entries = entries,
    };

    reloc.applyRelocsU32(&buf, 0, 42);

    // Verify the 5 bytes at offset 2 encode 42 in padded LEB128
    var expected = [_]u8{0} ** 5;
    WasmModule.overwritePaddedU32(&expected, 0, 42);
    try testing.expectEqualSlices(u8, &expected, buf[2..7]);

    // Verify surrounding bytes are untouched
    try testing.expectEqual(@as(u8, 0xAA), buf[0]);
    try testing.expectEqual(@as(u8, 0xBB), buf[1]);
    try testing.expectEqual(@as(u8, 0xCC), buf[7]);
}

test "RelocationSection.applyRelocsU32 — patches multiple sites for same symbol" {
    var buf = [_]u8{0} ** 15;

    var entries: std.ArrayList(RelocationEntry) = .empty;
    defer entries.deinit(testing.allocator);
    // Two relocation sites for the same symbol
    try entries.append(testing.allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 0,
        .symbol_index = 3,
    } });
    try entries.append(testing.allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 10,
        .symbol_index = 3,
    } });

    const reloc = RelocationSection{
        .name = "reloc.CODE",
        .target_section_index = 0,
        .entries = entries,
    };

    reloc.applyRelocsU32(&buf, 3, 99);

    // Both sites should be patched
    var expected = [_]u8{0} ** 5;
    WasmModule.overwritePaddedU32(&expected, 0, 99);
    try testing.expectEqualSlices(u8, &expected, buf[0..5]);
    try testing.expectEqualSlices(u8, &expected, buf[10..15]);
}

test "RelocationSection.applyRelocsU32 — ignores entries for different symbols" {
    var buf = [_]u8{0} ** 10;

    var entries: std.ArrayList(RelocationEntry) = .empty;
    defer entries.deinit(testing.allocator);
    try entries.append(testing.allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 0,
        .symbol_index = 1,
    } });
    try entries.append(testing.allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .offset = 5,
        .symbol_index = 2,
    } });

    const reloc = RelocationSection{
        .name = "reloc.CODE",
        .target_section_index = 0,
        .entries = entries,
    };

    // Only patch symbol 1
    reloc.applyRelocsU32(&buf, 1, 77);

    var expected = [_]u8{0} ** 5;
    WasmModule.overwritePaddedU32(&expected, 0, 77);
    try testing.expectEqualSlices(u8, &expected, buf[0..5]);

    // Symbol 2's site should be untouched (all zeros)
    try testing.expectEqualSlices(u8, &.{ 0, 0, 0, 0, 0 }, buf[5..10]);
}

test "RelocationSection.applyRelocsU32 — memory_addr_leb adds addend correctly" {
    var buf = [_]u8{0} ** 5;

    var entries: std.ArrayList(RelocationEntry) = .empty;
    defer entries.deinit(testing.allocator);
    try entries.append(testing.allocator, .{ .offset = .{
        .type_id = .memory_addr_leb,
        .offset = 0,
        .symbol_index = 0,
        .addend = 16,
    } });

    const reloc = RelocationSection{
        .name = "reloc.CODE",
        .target_section_index = 0,
        .entries = entries,
    };

    // value=100, addend=16, patched address should be 116
    reloc.applyRelocsU32(&buf, 0, 100);

    var expected = [_]u8{0} ** 5;
    WasmModule.overwritePaddedU32(&expected, 0, 116);
    try testing.expectEqualSlices(u8, &expected, &buf);
}

test "RelocationSection.applyRelocsU32 — memory_addr_sleb handles negative addend" {
    var buf = [_]u8{0} ** 5;

    var entries: std.ArrayList(RelocationEntry) = .empty;
    defer entries.deinit(testing.allocator);
    try entries.append(testing.allocator, .{ .offset = .{
        .type_id = .memory_addr_sleb,
        .offset = 0,
        .symbol_index = 0,
        .addend = -4,
    } });

    const reloc = RelocationSection{
        .name = "reloc.CODE",
        .target_section_index = 0,
        .entries = entries,
    };

    // value=100, addend=-4, patched address should be 96
    reloc.applyRelocsU32(&buf, 0, 100);

    var expected = [_]u8{0} ** 5;
    WasmModule.overwritePaddedI32(&expected, 0, 96);
    try testing.expectEqualSlices(u8, &expected, &buf);
}

test "LinkingSection.findSymbolByName — finds existing symbol" {
    var sym_table: std.ArrayList(SymInfo) = .empty;
    defer sym_table.deinit(testing.allocator);
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = 0,
        .name = "foo",
        .index = 0,
    });
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = 0,
        .name = "bar",
        .index = 1,
    });

    const seg_info: std.ArrayList(SegmentInfo) = .empty;
    const init_funcs: std.ArrayList(InitFunc) = .empty;

    const section = LinkingSection{
        .symbol_table = sym_table,
        .segment_info = seg_info,
        .init_funcs = init_funcs,
    };

    const imports: []const Import = &.{};
    try testing.expectEqual(@as(?u32, 1), section.findSymbolByName("bar", imports));
}

test "LinkingSection.findSymbolByName — returns null for missing symbol" {
    var sym_table: std.ArrayList(SymInfo) = .empty;
    defer sym_table.deinit(testing.allocator);
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = 0,
        .name = "foo",
        .index = 0,
    });

    const seg_info: std.ArrayList(SegmentInfo) = .empty;
    const init_funcs: std.ArrayList(InitFunc) = .empty;

    const section = LinkingSection{
        .symbol_table = sym_table,
        .segment_info = seg_info,
        .init_funcs = init_funcs,
    };

    const imports: []const Import = &.{};
    try testing.expectEqual(@as(?u32, null), section.findSymbolByName("missing", imports));
}

test "LinkingSection.findImportedFnSymIndex — finds undefined function symbol" {
    var sym_table: std.ArrayList(SymInfo) = .empty;
    defer sym_table.deinit(testing.allocator);
    // A defined function
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = 0,
        .name = "defined_fn",
        .index = 0,
    });
    // An undefined (imported) function at fn_index 5
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = SymFlag.UNDEFINED,
        .name = null,
        .index = 5,
    });

    const seg_info: std.ArrayList(SegmentInfo) = .empty;
    const init_funcs: std.ArrayList(InitFunc) = .empty;

    const section = LinkingSection{
        .symbol_table = sym_table,
        .segment_info = seg_info,
        .init_funcs = init_funcs,
    };

    try testing.expectEqual(@as(?u32, 1), section.findImportedFnSymIndex(5));
    try testing.expectEqual(@as(?u32, null), section.findImportedFnSymIndex(99));
}

test "LinkingSection.findAndReindexImportedFn — updates index and returns sym index" {
    var sym_table: std.ArrayList(SymInfo) = .empty;
    defer sym_table.deinit(testing.allocator);
    try sym_table.append(testing.allocator, .{
        .kind = .function,
        .flags = SymFlag.UNDEFINED,
        .name = null,
        .index = 3,
    });

    const seg_info: std.ArrayList(SegmentInfo) = .empty;
    const init_funcs: std.ArrayList(InitFunc) = .empty;

    var section = LinkingSection{
        .symbol_table = sym_table,
        .segment_info = seg_info,
        .init_funcs = init_funcs,
    };

    const result = section.findAndReindexImportedFn(3, 10);
    try testing.expectEqual(@as(?u32, 0), result);
    // Verify the index was actually updated
    try testing.expectEqual(@as(u32, 10), section.symbol_table.items[0].index);
    // Old index should no longer be found
    try testing.expectEqual(@as(?u32, null), section.findAndReindexImportedFn(3, 20));
}

// --- Parsing tests ---

test "RelocationEntry.parse — parses index relocation (function_index_leb)" {
    // type=0 (function_index_leb), offset=5, symbol_index=2
    var bytes: [3]u8 = .{ 0x00, 0x05, 0x02 };
    var cursor: usize = 0;
    const entry = try RelocationEntry.parse(&bytes, &cursor);
    try testing.expectEqual(@as(usize, 3), cursor);
    switch (entry) {
        .index => |idx| {
            try testing.expectEqual(IndexRelocType.function_index_leb, idx.type_id);
            try testing.expectEqual(@as(u32, 5), idx.offset);
            try testing.expectEqual(@as(u32, 2), idx.symbol_index);
        },
        .offset => unreachable,
    }
}

test "RelocationEntry.parse — parses offset relocation (memory_addr_leb) with addend" {
    // type=3 (memory_addr_leb), offset=10, symbol_index=1, addend=16
    var bytes: [4]u8 = .{ 0x03, 0x0A, 0x01, 0x10 };
    var cursor: usize = 0;
    const entry = try RelocationEntry.parse(&bytes, &cursor);
    try testing.expectEqual(@as(usize, 4), cursor);
    switch (entry) {
        .offset => |off| {
            try testing.expectEqual(OffsetRelocType.memory_addr_leb, off.type_id);
            try testing.expectEqual(@as(u32, 10), off.offset);
            try testing.expectEqual(@as(u32, 1), off.symbol_index);
            try testing.expectEqual(@as(i32, 16), off.addend);
        },
        .index => unreachable,
    }
}

test "SymInfo.parse — parses undefined function symbol (implicitly named)" {
    // kind=0 (function), flags=0x10 (UNDEFINED), index=3
    var bytes: [3]u8 = .{ 0x00, 0x10, 0x03 };
    var cursor: usize = 0;
    const sym = try SymInfo.parse(&bytes, &cursor);
    try testing.expectEqual(SymKind.function, sym.kind);
    try testing.expect(sym.isUndefined());
    try testing.expect(sym.isImplicitlyNamed());
    try testing.expectEqual(@as(u32, 3), sym.index);
}

test "SymInfo.parse — parses defined function symbol (explicitly named)" {
    // kind=0 (function), flags=0 (defined), index=1, name="my_func" (7 bytes)
    const bytes = [_]u8{ 0x00, 0x00, 0x01, 0x07 } ++ "my_func".*;
    var cursor: usize = 0;
    const sym = try SymInfo.parse(&bytes, &cursor);
    try testing.expectEqual(SymKind.function, sym.kind);
    try testing.expect(!sym.isUndefined());
    try testing.expectEqualStrings("my_func", sym.name.?);
    try testing.expectEqual(@as(u32, 1), sym.index);
}

test "SymInfo.parse — parses data symbol with segment info" {
    // kind=1 (data), flags=0 (defined), name="data_sym" (8 bytes),
    // segment_index=0, data_offset=16, data_size=4
    const bytes = [_]u8{ 0x01, 0x00, 0x08 } ++ "data_sym".* ++ [_]u8{ 0x00, 0x10, 0x04 };
    var cursor: usize = 0;
    const sym = try SymInfo.parse(&bytes, &cursor);
    try testing.expectEqual(SymKind.data, sym.kind);
    try testing.expectEqualStrings("data_sym", sym.name.?);
    try testing.expectEqual(@as(u32, 0), sym.index); // segment index
    try testing.expectEqual(@as(u32, 16), sym.data_offset);
    try testing.expectEqual(@as(u32, 4), sym.data_size);
}

test "RelocationSection.parse — parses section with multiple entries" {
    // Build reloc section body: target_section=10, count=2, then 2 index relocs
    var bytes_list: std.ArrayList(u8) = .empty;
    defer bytes_list.deinit(testing.allocator);
    try WasmModule.leb128WriteU32(testing.allocator, &bytes_list, 10); // target section
    try WasmModule.leb128WriteU32(testing.allocator, &bytes_list, 2); // count
    // Entry 1: function_index_leb, offset=5, sym=0
    try bytes_list.appendSlice(testing.allocator, &.{ 0x00, 0x05, 0x00 });
    // Entry 2: global_index_leb, offset=12, sym=1
    try bytes_list.appendSlice(testing.allocator, &.{ 0x07, 0x0C, 0x01 });

    var cursor: usize = 0;
    var reloc = try RelocationSection.parse(
        testing.allocator,
        "reloc.CODE",
        bytes_list.items,
        &cursor,
        bytes_list.items.len,
    );
    defer reloc.entries.deinit(testing.allocator);

    try testing.expectEqual(@as(u32, 10), reloc.target_section_index);
    try testing.expectEqual(@as(usize, 2), reloc.entries.items.len);
    try testing.expectEqual(@as(u32, 5), reloc.entries.items[0].getOffset());
    try testing.expectEqual(@as(u32, 12), reloc.entries.items[1].getOffset());
}
