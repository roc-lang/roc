//! Converting a relocatable wasm object to an image (`WasmUnitImage`), so a
//! program is linked with it by copying and patching (plan "Wasm fast dev
//! link"). Two kinds of object are converted, each once and then cached:
//! the compiler's support code for a target (compiler-rt, the builtins, the
//! Boxy runtime and the stack support functions, composed by `wasm-ld -r`)
//! and a platform's host (its link inputs, composed the same way).
//!
//! The object is read with `WasmModule.preload`, and each of its defined
//! functions and data segments becomes an atom of the image, each of its
//! relocations a site, each undefined symbol a relocation names an import
//! (weak when every reference to it is weak), and each defined symbol that
//! is not local an export. Globals the object
//! defines keep their initial values, and its constructors their order.
//!
//! An object holding something an image cannot represent (thread-local
//! data, a global whose initializer is not a constant, a relocation that
//! only debug information uses, a relocated field that is not a padded
//! LEB128) is reported as `error.UnsupportedObject` with the reason.
const std = @import("std");

const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");
const WasmUnitImage = @import("WasmUnitImage.zig");
const StackRoom = @import("StackRoom.zig");

const Allocator = std.mem.Allocator;

/// How an object is converted.
pub const Options = struct {
    /// Whether the object's code checks its stack bounds against
    /// `__stack_low` (the compiler's stack-checked support code): every
    /// function then carries its frame facts, and every check's bound is a
    /// `stack_room_sleb` site.
    stack_checks: bool,
    /// Whether what the object keeps whether or not anything uses it
    /// (symbols marked no-strip or exported, segments marked retained) is a
    /// root of the image, and its functions marked exported are exported by
    /// a finished module: a platform's host. The compiler's support code is
    /// linked only as far as the program reaches it.
    retain: bool,
};

/// What an image cannot represent.
pub const Unsupported = enum {
    thread_local_data,
    global_initializer,
    debug_relocation,
    event_symbol,
    unpadded_site,
    exported_data,
    zero_fill_relocation,
    weak_undefined_global,

    /// A sentence describing it, for the report.
    pub fn describe(self: Unsupported) []const u8 {
        return switch (self) {
            .thread_local_data => "it defines thread-local data",
            .global_initializer => "it defines a global whose initial value is not a constant",
            .debug_relocation => "its code or data holds a relocation only debug information uses",
            .event_symbol => "it uses exception events",
            .unpadded_site => "a relocated field in its code is not a 5-byte LEB128",
            .exported_data => "it marks a data symbol for export from the finished module",
            .zero_fill_relocation => "a zero-filled data segment holds a relocation",
            .weak_undefined_global => "it references a global only weakly, which no link gives a null value",
        };
    }
};

/// Why an object could not be converted.
pub const Error = Allocator.Error || WasmModule.ParseError || WasmUnitImage.EncodeError || StackRoom.Error || error{UnsupportedObject};

/// Convert the relocatable object `bytes` to an image. The caller owns the
/// result. On `error.UnsupportedObject`, `unsupported` says why.
pub fn convert(gpa: Allocator, bytes: []const u8, options: Options, unsupported: *?Unsupported) Error![]align(WasmUnitImage.alignment.toByteUnits()) u8 {
    var module = try WasmModule.preload(gpa, bytes, .relocatable_for_image);
    defer module.deinit();

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const signatures = try parseSignatures(arena, bytes);
    const symbols = module.linking.symbol_table.items;
    const import_count: u32 = @intCast(module.imports.items.len);
    const global_import_count: u32 = @intCast(module.global_imports.items.len);

    for (symbols) |symbol| if (symbol.kind == .event) {
        unsupported.* = .event_symbol;
        return error.UnsupportedObject;
    };
    for (module.data_segments.items) |segment| if (segment.flags & segment_flag_tls != 0) {
        unsupported.* = .thread_local_data;
        return error.UnsupportedObject;
    };

    var types: Types = .{ .arena = arena, .signatures = signatures };
    var imports: Imports = .{ .arena = arena, .module = &module };
    try imports.collect();
    for (imports.list.items) |import| if (import.input.weak and import.input.kind == .global) {
        unsupported.* = .weak_undefined_global;
        return error.UnsupportedObject;
    };

    // The checks of stack-checked code, which a composer gives their rooms.
    const frames: []const StackRoom.UnitFrame = if (options.stack_checks) try StackRoom.unitFrames(arena, &module) else &.{};
    var check_relocs: std.AutoHashMapUnmanaged(u32, void) = .empty;
    for (frames) |frame| for (frame.check_relocs) |reloc_index| try check_relocs.put(arena, reloc_index, {});

    // Functions, their bodies and their sites.
    const code_relocs = module.reloc_code.entries.items;
    const code_order = try arena.alloc(u32, code_relocs.len);
    for (code_order, 0..) |*slot, index| slot.* = @intCast(index);
    std.mem.sort(u32, code_order, code_relocs, struct {
        fn lessThan(entries: []const WasmLinking.RelocationEntry, a: u32, b: u32) bool {
            return entries[a].getOffset() < entries[b].getOffset();
        }
    }.lessThan);
    const function_count = module.function_offsets.items.len;
    const functions = try arena.alloc(WasmUnitImage.InputFunction, function_count);
    const requested = try arena.alloc(bool, function_count);
    @memset(requested, false);
    var next_reloc: usize = 0;
    for (functions, 0..) |*function, local| {
        var cursor: usize = module.function_offsets.items[local];
        const body_size = try WasmModule.readU32(module.code_bytes.items, &cursor);
        const body_start: u32 = @intCast(cursor);
        const body_end = body_start + body_size;
        if (body_end > module.code_bytes.items.len) return error.UnexpectedEnd;
        const body = try arena.dupe(u8, module.code_bytes.items[body_start..body_end]);
        var sites: std.ArrayList(WasmUnitImage.InputSite) = .empty;
        while (next_reloc < code_order.len and code_relocs[code_order[next_reloc]].getOffset() < body_end) : (next_reloc += 1) {
            const reloc_index = code_order[next_reloc];
            const entry = code_relocs[reloc_index];
            if (entry.getOffset() < body_start) return error.InvalidSection;
            const site = try siteOf(entry, entry.getOffset() - body_start, false, symbols, import_count, global_import_count, &types, &imports, check_relocs.contains(reloc_index), unsupported);
            if (site.offset + 5 > body.len or !paddedLeb(body[site.offset..][0..5])) {
                unsupported.* = .unpadded_site;
                return error.UnsupportedObject;
            }
            WasmModule.overwritePaddedU32(body, site.offset, 0);
            markTableSite(site, requested, &imports);
            try sites.append(arena, site);
        }
        if (module.func_type_indices.items[local] >= signatures.len) return error.InvalidSection;
        function.* = .{
            .body = body,
            .type = try types.of(module.func_type_indices.items[local]),
            .export_name = null,
            .sites = sites.items,
        };
    }
    if (next_reloc != code_order.len) return error.InvalidSection;

    // Segments and their sites.
    const data_relocs = try arena.dupe(WasmLinking.RelocationEntry, module.reloc_data.entries.items);
    std.mem.sort(WasmLinking.RelocationEntry, data_relocs, {}, struct {
        fn lessThan(_: void, a: WasmLinking.RelocationEntry, b: WasmLinking.RelocationEntry) bool {
            if (segmentOf(a) != segmentOf(b)) return segmentOf(a) < segmentOf(b);
            return a.getOffset() < b.getOffset();
        }
    }.lessThan);
    const segments = try arena.alloc(WasmUnitImage.InputSegment, module.data_segments.items.len);
    next_reloc = 0;
    for (segments, module.data_segments.items, 0..) |*segment, source, index| {
        const segment_bytes = try arena.dupe(u8, source.data);
        var sites: std.ArrayList(WasmUnitImage.InputSite) = .empty;
        while (next_reloc < data_relocs.len and segmentOf(data_relocs[next_reloc]) == index) : (next_reloc += 1) {
            const entry = data_relocs[next_reloc];
            const site = try siteOf(entry, entry.getOffset(), true, symbols, import_count, global_import_count, &types, &imports, false, unsupported);
            if (site.offset + 4 > segment_bytes.len) return error.InvalidSection;
            @memset(segment_bytes[site.offset..][0..4], 0);
            markTableSite(site, requested, &imports);
            try sites.append(arena, site);
        }
        if (source.zero_fill and sites.items.len != 0) {
            unsupported.* = .zero_fill_relocation;
            return error.UnsupportedObject;
        }
        segment.* = .{
            .bytes = if (source.zero_fill) null else segment_bytes,
            .size = @intCast(source.data.len),
            .alignment_log2 = source.alignment,
            .sites = sites.items,
        };
    }
    if (next_reloc != data_relocs.len) return error.InvalidSection;

    // Globals the object defines, with their initial values.
    std.debug.assert(module.preloaded_globals.items.len == module.preloaded_defined_global_count); // `.relocatable_for_image` reads every defined global
    const globals = try arena.alloc(WasmUnitImage.InputGlobal, module.preloaded_globals.items.len);
    for (globals, module.preloaded_globals.items) |*global, source| {
        global.* = .{
            .valtype = source.val_type,
            .mutable = source.mutable,
            .init = source.init orelse {
                unsupported.* = .global_initializer;
                return error.UnsupportedObject;
            },
            .export_name = null,
        };
    }

    // Exports: every definition that is not local, hidden ones included,
    // since another object of the program can still name them. Names for
    // the name section come from every defined function symbol.
    var aliases: std.ArrayList(WasmUnitImage.InputFunctionExport) = .empty;
    var data_exports: std.ArrayList(WasmUnitImage.InputDataExport) = .empty;
    var roots: std.AutoArrayHashMapUnmanaged(u32, void) = .empty;
    for (symbols) |symbol| {
        if (symbol.isUndefined()) continue;
        const weak = symbol.flags & WasmLinking.SymFlag.BINDING_WEAK != 0;
        const exported = symbol.flags & WasmLinking.SymFlag.EXPORTED != 0;
        const retained = exported or symbol.flags & WasmLinking.SymFlag.NO_STRIP != 0;
        switch (symbol.kind) {
            .function => {
                if (symbol.index < import_count or symbol.index - import_count >= function_count) return error.InvalidSection;
                const local = symbol.index - import_count;
                const name = symbol.name orelse return error.InvalidSection;
                if (functions[local].name == null) functions[local].name = name;
                if (options.retain and retained) try roots.put(arena, local, {});
                if (symbol.isLocal()) continue;
                try aliases.append(arena, .{ .name = name, .function = local, .weak = weak, .object_exported = options.retain and exported });
            },
            .data => {
                if (symbol.index >= segments.len) return error.InvalidSection;
                if (options.retain and retained) try roots.put(arena, @intCast(function_count + symbol.index), {});
                if (symbol.isLocal()) continue;
                if (options.retain and exported) {
                    unsupported.* = .exported_data;
                    return error.UnsupportedObject;
                }
                const end = std.math.add(u32, symbol.data_offset, symbol.data_size) catch return error.InvalidSection;
                if (end > segments[symbol.index].size) return error.InvalidSection;
                try data_exports.append(arena, .{
                    .name = symbol.name orelse return error.InvalidSection,
                    .segment = symbol.index,
                    .offset = symbol.data_offset,
                    .size = symbol.data_size,
                    .weak = weak,
                });
            },
            .global => {
                if (symbol.index < global_import_count or symbol.index - global_import_count >= globals.len) return error.InvalidSection;
                if (symbol.isLocal()) continue;
                const global = &globals[symbol.index - global_import_count];
                if (global.export_name == null) {
                    global.export_name = symbol.name orelse return error.InvalidSection;
                    global.weak = weak;
                }
            },
            .table, .section => {},
            .event => unreachable, // rejected above, before anything is converted
        }
    }
    if (options.retain) for (module.data_segments.items, 0..) |segment, index| {
        if (segment.flags & segment_flag_retain != 0) try roots.put(arena, @intCast(function_count + index), {});
    };
    const root_list = try arena.dupe(u32, roots.keys());
    std.mem.sort(u32, root_list, {}, std.sort.asc(u32));

    // Constructors, by priority, keeping the object's order among equals.
    const init_funcs = try arena.alloc(WasmUnitImage.InitFunc, module.linking.init_funcs.items.len);
    for (init_funcs, module.linking.init_funcs.items) |*out, init_func| {
        if (init_func.symbol_index >= symbols.len) return error.InvalidSection;
        const symbol = symbols[init_func.symbol_index];
        if (symbol.kind != .function or symbol.isUndefined() or symbol.index < import_count) return error.InvalidSection;
        out.* = .{ .priority = init_func.priority, .function = symbol.index - import_count };
    }
    std.mem.sort(WasmUnitImage.InitFunc, init_funcs, {}, struct {
        fn lessThan(_: void, a: WasmUnitImage.InitFunc, b: WasmUnitImage.InitFunc) bool {
            return a.priority < b.priority;
        }
    }.lessThan);

    if (options.stack_checks) {
        for (functions, frames) |*function, frame| {
            const calls = try arena.alloc(WasmUnitImage.InputCall, frame.calls.len);
            for (calls, frame.calls) |*call, source| call.* = switch (source.callee) {
                .defined => |local| .{ .target_kind = .function, .target = local, .depth = try depth32(source.depth) },
                .import_symbol => |symbol| .{ .target_kind = .import, .target = imports.indexOfSymbol(symbol) orelse return error.InvalidSection, .depth = try depth32(source.depth) },
                .indirect => |type_index| .{ .target_kind = .type, .target = try types.of(type_index), .depth = try depth32(source.depth) },
            };
            std.mem.sort(WasmUnitImage.InputCall, calls, {}, struct {
                fn lessThan(_: void, a: WasmUnitImage.InputCall, b: WasmUnitImage.InputCall) bool {
                    if (a.target_kind != b.target_kind) return @backingInt(a.target_kind) < @backingInt(b.target_kind);
                    if (a.target != b.target) return a.target < b.target;
                    return a.depth < b.depth;
                }
            }.lessThan);
            function.frame_depth = try depth32(frame.own_depth);
            function.calls = calls;
            function.checked = frame.checked;
        }
    }

    var table_requests: std.ArrayList(u32) = .empty;
    for (requested, 0..) |wanted, local| if (wanted) try table_requests.append(arena, @intCast(local));

    const image_imports = try arena.alloc(WasmUnitImage.InputImport, imports.list.items.len);
    for (image_imports, imports.list.items) |*out, import| {
        out.* = import.input;
        if (import.module_type) |module_type| out.type = try types.of(module_type);
    }

    return WasmUnitImage.encode(gpa, .{
        .stack_checks = options.stack_checks,
        .types = types.list.items,
        .functions = functions,
        .segments = segments,
        .data_exports = data_exports.items,
        .table_requests = table_requests.items,
        .imports = image_imports,
        .globals = globals,
        .init_funcs = init_funcs,
        .aliases = aliases.items,
        .roots = root_list,
    });
}

/// `SegmentInfo.flags` bits.
const segment_flag_tls: u32 = 2;
const segment_flag_retain: u32 = 4;

fn segmentOf(entry: WasmLinking.RelocationEntry) u32 {
    return switch (entry) {
        .index => |index| index.data_segment_index,
        .offset => |offset| offset.data_segment_index,
    };
}

fn depth32(depth: u64) error{ImageTooLarge}!u32 {
    return std.math.cast(u32, depth) orelse error.ImageTooLarge;
}

/// Whether `field` is a 5-byte LEB128: four bytes that continue, then one
/// that ends it.
fn paddedLeb(field: *const [5]u8) bool {
    for (field[0..4]) |byte| if (byte & 0x80 == 0) return false;
    return field[4] & 0x80 == 0;
}

fn isTableKind(kind: WasmUnitImage.SiteKind) bool {
    return switch (kind) {
        .table_index_sleb, .table_index_rel_sleb, .table_index_i32 => true,
        .function_index_leb, .type_index_leb, .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .global_index_leb, .table_number_leb, .stack_room_sleb, .memory_addr_i32 => false,
    };
}

fn markTableSite(site: WasmUnitImage.InputSite, requested: []bool, imports: *Imports) void {
    if (!isTableKind(site.kind)) return;
    switch (site.target_kind) {
        .function => requested[site.target] = true,
        .import => imports.list.items[site.target].input.address_taken = true,
        .data, .type, .global => unreachable, // `siteOf` names a function or an import for every table relocation
    }
}

/// The site a relocation of the object becomes.
fn siteOf(
    entry: WasmLinking.RelocationEntry,
    offset: u32,
    in_data: bool,
    symbols: []const WasmLinking.SymInfo,
    import_count: u32,
    global_import_count: u32,
    types: *Types,
    imports: *const Imports,
    is_check: bool,
    unsupported: *?Unsupported,
) Error!WasmUnitImage.InputSite {
    switch (entry) {
        .index => |index| {
            const kind: WasmUnitImage.SiteKind = switch (index.type_id) {
                .function_index_leb => .function_index_leb,
                .type_index_leb => .type_index_leb,
                .table_index_sleb => .table_index_sleb,
                .table_index_rel_sleb => .table_index_rel_sleb,
                .global_index_leb => .global_index_leb,
                .table_number_leb => .table_number_leb,
                .table_index_i32 => .table_index_i32,
                .event_index_leb, .global_index_i32 => {
                    unsupported.* = .debug_relocation;
                    return error.UnsupportedObject;
                },
            };
            if (kind.inData() != in_data) return error.InvalidSection;
            if (kind == .type_index_leb) {
                if (index.symbol_index >= types.signatures.len) return error.InvalidSection;
                return .{ .offset = offset, .kind = kind, .target_kind = .type, .target = try types.of(index.symbol_index), .addend = 0 };
            }
            const target = try targetOf(symbols, index.symbol_index, import_count, global_import_count, imports);
            const allowed = switch (kind) {
                .function_index_leb, .table_index_sleb, .table_index_rel_sleb, .table_index_i32 => target.symbol_kind == .function,
                .global_index_leb => target.symbol_kind == .global,
                .table_number_leb => target.symbol_kind == .table,
                .type_index_leb, .memory_addr_leb, .memory_addr_sleb, .memory_addr_rel_sleb, .stack_room_sleb, .memory_addr_i32 => unreachable, // `kind` comes from an index relocation other than a type index
            };
            if (!allowed) return error.InvalidSection;
            // A table the object defines is the program's one table, which
            // a link always numbers 0.
            if (target.kind == .defined_table) return .{ .offset = offset, .kind = kind, .target_kind = .import, .target = imports.table_import orelse return error.InvalidSection, .addend = 0 };
            return .{ .offset = offset, .kind = kind, .target_kind = target.kind.toImage(), .target = target.index, .addend = 0 };
        },
        .offset => |reloc| {
            var kind: WasmUnitImage.SiteKind = switch (reloc.type_id) {
                .memory_addr_leb => .memory_addr_leb,
                .memory_addr_sleb => .memory_addr_sleb,
                .memory_addr_rel_sleb => .memory_addr_rel_sleb,
                .memory_addr_i32 => .memory_addr_i32,
                .function_offset_i32, .section_offset_i32 => {
                    unsupported.* = .debug_relocation;
                    return error.UnsupportedObject;
                },
            };
            if (kind.inData() != in_data) return error.InvalidSection;
            const target = try targetOf(symbols, reloc.symbol_index, import_count, global_import_count, imports);
            if (target.symbol_kind != .data) return error.InvalidSection;
            if (is_check) {
                if (kind != .memory_addr_sleb or target.kind != .import) return error.InvalidSection;
                kind = .stack_room_sleb;
            }
            return .{ .offset = offset, .kind = kind, .target_kind = target.kind.toImage(), .target = target.index, .addend = reloc.addend +% @as(i32, @bitCast(target.data_offset)) };
        },
    }
}

const Target = struct {
    kind: Kind,
    symbol_kind: WasmLinking.SymKind,
    index: u32,
    data_offset: u32 = 0,

    const Kind = enum {
        function,
        data,
        global,
        import,
        defined_table,

        fn toImage(self: Kind) WasmUnitImage.TargetKind {
            return switch (self) {
                .function => .function,
                .data => .data,
                .global => .global,
                .import => .import,
                .defined_table => unreachable, // `siteOf` sends a defined table to the table import first
            };
        }
    };
};

fn targetOf(symbols: []const WasmLinking.SymInfo, symbol_index: u32, import_count: u32, global_import_count: u32, imports: *const Imports) Error!Target {
    if (symbol_index >= symbols.len) return error.InvalidSection;
    const symbol = symbols[symbol_index];
    if (symbol.isUndefined()) return .{ .kind = .import, .symbol_kind = symbol.kind, .index = imports.indexOfSymbol(symbol_index) orelse return error.InvalidSection };
    return switch (symbol.kind) {
        .function => .{ .kind = .function, .symbol_kind = .function, .index = std.math.sub(u32, symbol.index, import_count) catch return error.InvalidSection },
        .data => .{ .kind = .data, .symbol_kind = .data, .index = symbol.index, .data_offset = symbol.data_offset },
        .global => .{ .kind = .global, .symbol_kind = .global, .index = std.math.sub(u32, symbol.index, global_import_count) catch return error.InvalidSection },
        .table => .{ .kind = .defined_table, .symbol_kind = .table, .index = 0 },
        .section, .event => error.InvalidSection,
    };
}

/// A signature's parameter and result types, read from the type section
/// itself so every result of a multi-value signature is kept.
const Signature = struct {
    params: []const u8,
    results: []const u8,
};

fn parseSignatures(arena: Allocator, bytes: []const u8) Error![]Signature {
    var cursor: usize = 8;
    while (cursor < bytes.len) {
        const id = bytes[cursor];
        cursor += 1;
        const size = try WasmModule.readU32(bytes, &cursor);
        const end = cursor + size;
        if (end > bytes.len) return error.UnexpectedEnd;
        if (id != @backingInt(WasmModule.SectionId.type_section)) {
            cursor = end;
            continue;
        }
        const section = bytes[0..end];
        const count = try WasmModule.readU32(section, &cursor);
        const signatures = try arena.alloc(Signature, count);
        for (signatures) |*signature| {
            if (cursor >= section.len or section[cursor] != 0x60) return error.InvalidSection;
            cursor += 1;
            const params = try WasmModule.readU32(section, &cursor);
            if (cursor + params > section.len) return error.UnexpectedEnd;
            const param_bytes = section[cursor..][0..params];
            cursor += params;
            const results = try WasmModule.readU32(section, &cursor);
            if (cursor + results > section.len) return error.UnexpectedEnd;
            signature.* = .{ .params = param_bytes, .results = section[cursor..][0..results] };
            cursor += results;
        }
        return signatures;
    }
    return &.{};
}

/// The image's signatures, each object signature once by shape, in
/// first-use order.
const Types = struct {
    arena: Allocator,
    signatures: []const Signature,
    list: std.ArrayList(WasmUnitImage.InputType) = .empty,
    by_module_type: std.AutoHashMapUnmanaged(u32, u32) = .empty,
    by_shape: std.StringHashMapUnmanaged(u32) = .empty,

    fn of(self: *Types, module_type: u32) Error!u32 {
        if (self.by_module_type.get(module_type)) |index| return index;
        if (module_type >= self.signatures.len) return error.InvalidSection;
        const signature = self.signatures[module_type];
        // A byte no value type uses separates parameters from results.
        const shape = try std.mem.concat(self.arena, u8, &.{ signature.params, &.{0x40}, signature.results });
        const gop = try self.by_shape.getOrPut(self.arena, shape);
        if (!gop.found_existing) {
            gop.value_ptr.* = @intCast(self.list.items.len);
            try self.list.append(self.arena, .{ .params = signature.params, .results = signature.results });
        }
        try self.by_module_type.put(self.arena, module_type, gop.value_ptr.*);
        return gop.value_ptr.*;
    }
};

/// The image's imports: every undefined symbol a relocation names, one
/// import per kind and symbol name, in the object's symbol order. An import
/// is weak when every symbol it stands for is.
const Imports = struct {
    arena: Allocator,
    module: *const WasmModule,
    list: std.ArrayList(Import) = .empty,
    by_symbol: std.AutoHashMapUnmanaged(u32, u32) = .empty,
    /// The import standing for the object's own table, when a relocation
    /// names a table the object defines.
    table_import: ?u32 = null,

    const Import = struct {
        input: WasmUnitImage.InputImport,
        module_type: ?u32,
    };

    fn collect(self: *Imports) Error!void {
        const module = self.module;
        const symbols = module.linking.symbol_table.items;
        var referenced: std.AutoArrayHashMapUnmanaged(u32, void) = .empty;
        var names_defined_table = false;
        for ([_][]const WasmLinking.RelocationEntry{ module.reloc_code.entries.items, module.reloc_data.entries.items }) |entries| for (entries) |entry| {
            switch (entry) {
                .index => |index| if (index.type_id == .type_index_leb) continue,
                .offset => {},
            }
            const symbol_index = entry.getSymbolIndex();
            if (symbol_index >= symbols.len) return error.InvalidSection;
            if (symbols[symbol_index].isUndefined()) {
                try referenced.put(self.arena, symbol_index, {});
            } else if (symbols[symbol_index].kind == .table) {
                names_defined_table = true;
            }
        };
        const order = try self.arena.dupe(u32, referenced.keys());
        std.mem.sort(u32, order, {}, std.sort.asc(u32));
        var by_name: std.StringHashMapUnmanaged(u32) = .empty;
        for (order) |symbol_index| {
            const symbol = symbols[symbol_index];
            const name = symbol.resolveName(module.imports.items, module.global_imports.items, module.table_imports.items) orelse return error.InvalidSection;
            const import: Import = switch (symbol.kind) {
                .function => blk: {
                    if (symbol.index >= module.imports.items.len) return error.InvalidSection;
                    const source = module.imports.items[symbol.index];
                    break :blk .{ .input = .{ .module = source.module_name, .name = name, .field = source.field_name, .kind = .function }, .module_type = source.type_idx };
                },
                .global => blk: {
                    if (symbol.index >= module.global_imports.items.len) return error.InvalidSection;
                    const source = module.global_imports.items[symbol.index];
                    break :blk .{ .input = .{ .module = source.module_name, .name = name, .field = source.field_name, .kind = .global, .valtype = source.val_type, .mutable = source.mutable }, .module_type = null };
                },
                .table => blk: {
                    if (symbol.index >= module.table_imports.items.len) return error.InvalidSection;
                    const source = module.table_imports.items[symbol.index];
                    break :blk .{ .input = .{ .module = source.module_name, .name = name, .field = source.field_name, .kind = .table }, .module_type = null };
                },
                .data => .{ .input = .{ .module = "env", .name = name, .kind = .data }, .module_type = null },
                .section, .event => return error.InvalidSection,
            };
            const weak = symbol.flags & WasmLinking.SymFlag.BINDING_WEAK != 0;
            const key = try std.mem.concat(self.arena, u8, &.{ &.{@backingInt(import.input.kind)}, name });
            const gop = try by_name.getOrPut(self.arena, key);
            if (gop.found_existing) {
                const existing = &self.list.items[gop.value_ptr.*].input;
                existing.weak = existing.weak and weak;
            } else {
                gop.value_ptr.* = @intCast(self.list.items.len);
                var weak_import = import;
                weak_import.input.weak = weak;
                try self.list.append(self.arena, weak_import);
            }
            try self.by_symbol.put(self.arena, symbol_index, gop.value_ptr.*);
            if (symbol.kind == .table and self.table_import == null) self.table_import = gop.value_ptr.*;
        }
        if (names_defined_table and self.table_import == null) {
            self.table_import = @intCast(self.list.items.len);
            try self.list.append(self.arena, .{ .input = .{ .module = "env", .name = "__indirect_function_table", .kind = .table }, .module_type = null });
        }
    }

    fn indexOfSymbol(self: *const Imports, symbol_index: u32) ?u32 {
        return self.by_symbol.get(symbol_index);
    }
};

test "a relocatable object converts to an image of its functions, exports and sites" {
    const allocator = std.testing.allocator;
    const shim_symbols = @import("builtins").shim_symbols;
    const StackSupport = @import("StackSupport.zig");
    const object = try StackSupport.encodeObject(allocator, .{ .accessors = true, .overflow = true });
    defer allocator.free(object);

    for ([_]bool{ false, true }) |stack_checks| {
        var unsupported: ?Unsupported = null;
        const image = try convert(allocator, object, .{ .stack_checks = stack_checks, .retain = false }, &unsupported);
        defer allocator.free(image);
        const view = try WasmUnitImage.View.init(image);
        try std.testing.expectEqual(stack_checks, view.stackChecks());
        try std.testing.expectEqual(@as(usize, 3), view.functions().len);
        const getter = view.findExport(shim_symbols.roc_wasm_stack_pointer) orelse return error.TestUnexpectedResult;
        _ = view.findExport(shim_symbols.roc_wasm_set_stack_pointer) orelse return error.TestUnexpectedResult;
        const overflow = view.findExport(shim_symbols.roc_wasm_stack_overflow) orelse return error.TestUnexpectedResult;
        // Each body names the stack pointer once, through an import whose
        // field holds zero now.
        const import = view.imports()[view.functionSites(view.functions()[getter])[0].target];
        try std.testing.expectEqualStrings("__stack_pointer", view.name(import.name));
        try std.testing.expectEqual(@backingInt(ImportKindTag.global), import.kind);
        for (view.functions()) |function| {
            const sites = view.functionSites(function);
            try std.testing.expectEqual(@as(usize, 1), sites.len);
            try std.testing.expectEqualSlices(u8, &.{ 0x80, 0x80, 0x80, 0x80, 0x00 }, view.body(function)[sites[0].offset..][0..5]);
        }
        // The overflow function starts no call chain a check must cover.
        if (stack_checks) try std.testing.expect(view.functions()[overflow].flags & WasmUnitImage.function_checked != 0);

        // Converting again writes the same image.
        const again = try convert(allocator, object, .{ .stack_checks = stack_checks, .retain = false }, &unsupported);
        defer allocator.free(again);
        try std.testing.expectEqualSlices(u8, image, again);
    }
}

const ImportKindTag = WasmUnitImage.ImportKind;
