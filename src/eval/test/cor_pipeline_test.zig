//! Focused eval tests for the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const backend = @import("backend");
const builtins = @import("builtins");
const collections = @import("collections");
const layout = @import("layout");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("RuntimeHostEnv.zig");
const helpers = @import("helpers.zig");

const testing = std.testing;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;

fn mainProcArgLayouts(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
) ![]layout.Idx {
    const proc = compiled.lowered.lir_result.store.getProcSpec(compiled.lowered.main_proc);
    const arg_locals = compiled.lowered.lir_result.store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_locals.len);
    for (arg_locals, 0..) |local_id, i| {
        arg_layouts[i] = compiled.lowered.lir_result.store.getLocal(local_id).layout_idx;
    }
    return arg_layouts;
}

fn zeroedEntrypointArgBuffer(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    arg_layouts: []const layout.Idx,
) !?[]u8 {
    const EntrypointArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const arg_offsets = try allocator.alloc(u32, arg_layouts.len);
    defer allocator.free(arg_offsets);
    if (arg_layouts.len != 0) {
        const ordered = try allocator.alloc(EntrypointArgOrder, arg_layouts.len);
        defer allocator.free(ordered);

        for (arg_layouts, 0..) |arg_layout, i| {
            const size_align = compiled.lowered.lir_result.layouts.layoutSizeAlign(
                compiled.lowered.lir_result.layouts.getLayout(arg_layout),
            );
            const slot_size = entrypointParamSlotSize(compiled, arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(size_align.alignment.toByteUnits()),
                .size = slot_size,
            };
        }

        const SortCtx = struct {
            fn lessThan(_: void, lhs: EntrypointArgOrder, rhs: EntrypointArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(EntrypointArgOrder, ordered, {}, SortCtx.lessThan);

        var current_offset: u32 = 0;
        for (ordered) |arg| {
            current_offset = std.mem.alignForward(u32, current_offset, arg.alignment);
            arg_offsets[arg.index] = current_offset;
            current_offset += arg.size;
        }
    }

    var total_size: usize = 0;
    for (arg_layouts, 0..) |arg_layout, i| {
        total_size = @max(total_size, @as(usize, arg_offsets[i]) + entrypointParamSlotSize(compiled, arg_layout));
    }

    if (total_size == 0) return null;

    const buffer = try allocator.alignedAlloc(u8, collections.max_roc_alignment, @max(total_size, 1));
    @memset(buffer, 0);
    return buffer;
}

fn entrypointParamSlotSize(compiled: *const helpers.CompiledProgram, layout_idx: layout.Idx) u32 {
    const layouts = &compiled.lowered.lir_result.layouts;
    const runtime_layout_idx = layouts.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_layout_idx == .str) return 24;
    if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;

    if (@intFromEnum(runtime_layout_idx) < layouts.layouts.len()) {
        const layout_val = layouts.getLayout(runtime_layout_idx);
        const size = layouts.layoutSizeAlign(layout_val).size;
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
    }

    const size = layouts.layoutSizeAlign(layouts.getLayout(layout_idx)).size;
    return if (size == 0) 0 else 8;
}

fn expectInspect(comptime source: []const u8, expected: []const u8) !void {
    var compiled = try helpers.compileInspectedExpr(testing.allocator, source);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(expected, actual);
}

fn expectInspectProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected: []const u8,
) !void {
    var compiled = try helpers.compileInspectedProgram(testing.allocator, source_kind, source, imports);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(expected, actual);
}

fn expectInspectProgramWithArena(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected: []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var compiled = try helpers.compileInspectedProgram(arena_allocator, source_kind, source, imports);
    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings(expected, actual);
}

fn countLowLevelOp(compiled: *const helpers.CompiledInspectedExpr, op: base.LowLevel) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_low_level => |assign| {
                if (assign.op == op) count += 1;
            },
            else => {},
        }
    }
    return count;
}

fn countDirectCalls(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call => count += 1,
            else => {},
        }
    }
    return count;
}

fn countIndirectCalls(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call_indirect => count += 1,
            else => {},
        }
    }
    return count;
}

const CompiledExecutableProgram = struct {
    resources: helpers.ParsedResources,
    executable: lambdamono.Lower.Result,

    pub fn deinit(self: *CompiledExecutableProgram, allocator: std.mem.Allocator) void {
        self.executable.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

const CompiledIrProgram = struct {
    resources: helpers.ParsedResources,
    ir_result: ir.Lower.Result,

    pub fn deinit(self: *CompiledIrProgram, allocator: std.mem.Allocator) void {
        self.ir_result.deinit();
        helpers.cleanupParseAndCanonical(allocator, self.resources);
    }
};

fn compileExecutableProgram(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) !CompiledExecutableProgram {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, source_kind, source, imports);
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    const executable = try lambdamono.Lower.run(allocator, &solved);

    return .{
        .resources = resources,
        .executable = executable,
    };
}

fn compileIrProgram(
    allocator: std.mem.Allocator,
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
) !CompiledIrProgram {
    var resources = try helpers.parseAndCanonicalizeProgram(allocator, source_kind, source, imports);
    errdefer helpers.cleanupParseAndCanonical(allocator, resources);

    var mono_lowerer = try monotype.Lower.Lowerer.init(allocator, &resources.typed_cir_modules, 1, null);
    defer mono_lowerer.deinit();
    var mono = try mono_lowerer.run(0);
    defer mono.deinit();
    var lifted = try monotype_lifted.Lower.run(allocator, &mono);
    defer lifted.deinit();
    var solved = try lambdasolved.Lower.run(allocator, &lifted);
    defer solved.deinit();
    var executable = try lambdamono.Lower.run(allocator, &solved);
    defer executable.deinit();
    const ir_result = try ir.Lower.run(allocator, &executable);

    return .{
        .resources = resources,
        .ir_result = ir_result,
    };
}

fn countExecutableDirectCalls(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutableIndirectCalls(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call_indirect => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutablePackedFns(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .packed_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutableErasedFnTypes(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.types.types.items) |content| {
        switch (content) {
            .erased_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn countExecutableBridgeNodes(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .bridge => count += 1,
            else => {},
        }
    }
    return count;
}

fn countUniqueDirectCallTargetsNamed(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) !usize {
    var seen = std.AutoHashMap(u32, void).init(allocator);
    defer seen.deinit();

    for (executable.store.exprs.items) |expr| {
        switch (expr.data) {
            .call => |call| {
                const entry = executable.symbols.get(call.proc);
                switch (entry.origin) {
                    .specialized_local_fn => {},
                    else => continue,
                }
                try seen.put(call.proc.raw(), {});
            },
            else => {},
        }
    }

    return seen.count();
}

fn countIrBridgeNodes(ir_result: *const ir.Lower.Result) usize {
    var count: usize = 0;
    for (ir_result.store.exprs.items) |expr| {
        switch (expr) {
            .bridge => count += 1,
            else => {},
        }
    }
    return count;
}

fn countSpecializedLocalFnDefs(executable: *const lambdamono.Lower.Result) usize {
    var count: usize = 0;
    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => count += 1,
            else => {},
        }
    }
    return count;
}

fn collectSpecializedLocalFnResultTypes(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) ![]const lambdamono.Type.TypeId {
    var out = std.ArrayList(lambdamono.Type.TypeId).empty;
    errdefer out.deinit(allocator);

    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => {
                const result_ty = def.result_ty orelse
                    @panic("cor pipeline test expected specialized def result type");
                try out.append(allocator, result_ty);
            },
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn collectSpecializedLocalFnFirstArgTypes(
    allocator: std.mem.Allocator,
    executable: *const lambdamono.Lower.Result,
) ![]const lambdamono.Type.TypeId {
    var out = std.ArrayList(lambdamono.Type.TypeId).empty;
    errdefer out.deinit(allocator);

    for (executable.store.defsSlice()) |def| {
        const origin = executable.symbols.get(def.bind).origin;
        switch (origin) {
            .specialized_local_fn => switch (def.value) {
                .fn_ => |fn_def| {
                    const args = executable.store.sliceTypedSymbolSpan(fn_def.args);
                    if (args.len == 0) {
                        @panic("cor pipeline test expected specialized local fn to have at least one arg");
                    }
                    try out.append(allocator, args[0].ty);
                },
                else => @panic("cor pipeline test expected specialized local fn def"),
            },
            else => {},
        }
    }

    return try out.toOwnedSlice(allocator);
}

fn expectDirectOnlyLoweringProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected_executable_direct_calls: usize,
    expected_lir_direct_calls: usize,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);

    const executable_direct = countExecutableDirectCalls(&executable.executable);
    const executable_indirect = countExecutableIndirectCalls(&executable.executable);
    const executable_packed = countExecutablePackedFns(&executable.executable);
    const executable_erased = countExecutableErasedFnTypes(&executable.executable);

    var compiled = try helpers.compileProgram(arena_allocator, source_kind, source, imports);
    defer compiled.deinit(arena_allocator);

    const lir_direct = countDirectCalls(&compiled);
    const lir_indirect = countIndirectCalls(&compiled);
    try testing.expectEqual(expected_executable_direct_calls, executable_direct);
    try testing.expectEqual(@as(usize, 0), executable_indirect);
    try testing.expectEqual(@as(usize, 0), executable_packed);
    try testing.expectEqual(@as(usize, 0), executable_erased);
    try testing.expectEqual(expected_lir_direct_calls, lir_direct);
    try testing.expectEqual(@as(usize, 0), lir_indirect);
}

fn expectErasedLoweringProgram(
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource,
    expected_executable_indirect_calls: usize,
    expected_executable_packed_fns: usize,
    expected_executable_erased_fn_types: usize,
    expected_lir_indirect_calls: usize,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, source_kind, source, imports);
    defer executable.deinit(arena_allocator);

    const executable_indirect = countExecutableIndirectCalls(&executable.executable);
    const executable_packed = countExecutablePackedFns(&executable.executable);
    const executable_erased = countExecutableErasedFnTypes(&executable.executable);

    var compiled = try helpers.compileProgram(arena_allocator, source_kind, source, imports);
    defer compiled.deinit(arena_allocator);

    const lir_indirect = countIndirectCalls(&compiled);
    try testing.expectEqual(expected_executable_indirect_calls, executable_indirect);
    try testing.expectEqual(expected_executable_packed_fns, executable_packed);
    try testing.expectEqual(expected_executable_erased_fn_types, executable_erased);
    try testing.expectEqual(expected_lir_indirect_calls, lir_indirect);
}

const DirectCallCase = struct {
    name: []const u8,
    source_kind: helpers.SourceKind = .module,
    source: []const u8,
    imports: []const helpers.ModuleSource = &.{},
    expected: []const u8,
    executable_direct_calls: usize,
    lir_direct_calls: usize,
};

const annotated_callback_param_source =
    \\apply : (I64 -> I64), I64 -> I64
    \\apply = |f, x| f(x)
    \\
    \\main = apply(|n| n + 1.I64, 41.I64)
;

const annotated_return_source =
    \\make_adder : I64 -> (I64 -> I64)
    \\make_adder = |n| |x| x + n
    \\
    \\main = make_adder(1.I64)(41.I64)
;

const abstract_apply_source =
    \\main =
    \\    {
    \\        apply = |f, x| f(x)
    \\        apply(|n| n + 1.I64, 41.I64)
    \\    }
;

const nested_polymorphic_helper_source =
    \\main = ((|f| (|x| f(x)))(|n| n + 1.I64))(41.I64)
;

const apply_twice_source =
    \\main =
    \\    {
    \\        twice = |f, x| f(f(x))
    \\        twice(|n| n + 1.I64, 40.I64)
    \\    }
;

const annotated_local_binding_source =
    \\main =
    \\    {
    \\        f : I64 -> I64
    \\        f = |x| x + 1.I64
    \\        f(41.I64)
    \\    }
;

const captured_callback_param_source =
    \\main =
    \\    {
    \\        apply = |f, x| f(x)
    \\        y = 10.I64
    \\        apply(|x| x + y, 32.I64)
    \\    }
;

const cross_module_captured_callback_param_source =
    \\import Helpers
    \\
    \\main =
    \\    {
    \\        y = 10.I64
    \\        Helpers.apply(|x| x + y, 32.I64)
    \\    }
;

const cross_module_captured_callback_param_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply = |f, x| f(x)
        ,
    },
};

const two_callback_params_source =
    \\main =
    \\    {
    \\        combine = |f, g, x| f(x) + g(x)
    \\        a = 10.I64
    \\        b = 20.I64
    \\        combine(|x| x + a, |x| x + b, 6.I64)
    \\    }
;

const record_field_closure_extraction_source =
    \\main =
    \\    {
    \\        a = 10.I64
    \\        b = 20.I64
    \\        rec = { add_a: |x| x + a, add_b: |x| x + b }
    \\        add_a = rec.add_a
    \\        add_a(32.I64)
    \\    }
;

const opaque_function_field_lookup_source =
    \\W(a) := { f : {} -> [V(a)] }.{
    \\    run : W(a) -> [V(a)]
    \\    run = |w| (w.f)({})
    \\
    \\    mk : a -> W(a)
    \\    mk = |val| { f: |_| V(val) }
    \\}
    \\
    \\main = W.run(W.mk("x")) == V("x")
;

const additional_specialization_via_list_append_source =
    \\main =
    \\    {
    \\        append_one = |acc, x| List.append(acc, x)
    \\        _first_len = List.fold([1.I64, 2.I64], List.with_capacity(1), append_one).len()
    \\        List.fold([[1.I64, 2.I64], [3.I64, 4.I64]], List.with_capacity(1), append_one).len()
    \\    }
;

const polymorphic_two_list_types_source =
    \\main =
    \\    {
    \\        my_len = |list| list.len()
    \\        a : List(I64)
    \\        a = [1, 2, 3]
    \\        b : List(Str)
    \\        b = ["x", "y"]
    \\        my_len(a) + my_len(b)
    \\    }
;

const simple_direct_call_source =
    \\main =
    \\    {
    \\        add_one = |x| x + 1.I64
    \\        add_one(41.I64)
    \\    }
;

const typed_nested_captures_source =
    \\(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)
;

const cross_module_annotated_callback_param_source =
    \\import Helpers
    \\
    \\main = Helpers.apply(|n| n + 1.I64, 41.I64)
;

const cross_module_annotated_callback_param_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply : (I64 -> I64), I64 -> I64
        \\apply = |f, x| f(x)
        ,
    },
};

const cross_module_annotated_return_source =
    \\import Makers
    \\
    \\main = Makers.make_adder(1.I64)(41.I64)
;

const cross_module_annotated_return_imports = [_]helpers.ModuleSource{
    .{
        .name = "Makers",
        .source =
        \\module [make_adder]
        \\
        \\make_adder : I64 -> (I64 -> I64)
        \\make_adder = |n| |x| x + n
        ,
    },
};

const cross_module_abstract_apply_source =
    \\import Helpers
    \\
    \\main = Helpers.apply(|n| n + 1.I64, 41.I64)
;

const cross_module_abstract_apply_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [apply]
        \\
        \\apply = |f, x| f(x)
        ,
    },
};

const cross_module_nested_bridge_source =
    \\import Helpers
    \\
    \\main = Helpers.bridge(|n| n + 1.I64)(41.I64)
;

const cross_module_nested_bridge_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [bridge]
        \\
        \\bridge = |f| (|x| f(x))
        ,
    },
};

const cross_module_apply_twice_source =
    \\import Helpers
    \\
    \\main = Helpers.twice(|n| n + 1.I64, 40.I64)
;

const cross_module_apply_twice_imports = [_]helpers.ModuleSource{
    .{
        .name = "Helpers",
        .source =
        \\module [twice]
        \\
        \\twice = |f, x| f(f(x))
        ,
    },
};

const direct_call_cases = [_]DirectCallCase{
    .{
        .name = "annotated callback parameter slot",
        .source = annotated_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "annotated function return slot",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "abstract higher-order apply",
        .source = abstract_apply_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "nested polymorphic helper",
        .source = nested_polymorphic_helper_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "apply twice",
        .source = apply_twice_source,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 15,
    },
    .{
        .name = "annotated local binding",
        .source = annotated_local_binding_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 6,
    },
    .{
        .name = "passing captured closure to callback parameter",
        .source = captured_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "annotated return of concrete lambda",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "passing concrete lambda to annotated callback parameter",
        .source = annotated_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module annotated callback parameter slot",
        .source = cross_module_annotated_callback_param_source,
        .imports = &cross_module_annotated_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module annotated function return slot",
        .source = cross_module_annotated_return_source,
        .imports = &cross_module_annotated_return_imports,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 9,
    },
    .{
        .name = "cross-module abstract higher-order apply",
        .source = cross_module_abstract_apply_source,
        .imports = &cross_module_abstract_apply_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module nested higher-order bridge",
        .source = cross_module_nested_bridge_source,
        .imports = &cross_module_nested_bridge_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "cross-module apply twice",
        .source = cross_module_apply_twice_source,
        .imports = &cross_module_apply_twice_imports,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 15,
    },
    .{
        .name = "cross-module passing captured closure to callback parameter",
        .source = cross_module_captured_callback_param_source,
        .imports = &cross_module_captured_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 12,
    },
    .{
        .name = "two callback params with different captures",
        .source = two_callback_params_source,
        .expected = "42",
        .executable_direct_calls = 7,
        .lir_direct_calls = 18,
    },
    .{
        .name = "record field closure extraction then call",
        .source = record_field_closure_extraction_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 6,
    },
};

const ErasedCallCase = struct {
    source_kind: helpers.SourceKind,
    source: []const u8,
    imports: []const helpers.ModuleSource = &.{},
    expected_executable_indirect_calls: usize,
    expected_executable_packed_fns: usize,
    expected_executable_erased_fn_types: usize,
    expected_lir_indirect_calls: usize,
};

const boxed_lambda_round_trip_erased_case = ErasedCallCase{
    .source_kind = .expr,
    .source =
    \\{
    \\    wrap = |boxed| { value: boxed }
    \\    unwrap = |record| record.value
    \\    f = Box.unbox(unwrap(wrap(Box.box(|x| x + 1.I64))))
    \\    f(41.I64)
    \\}
    ,
    .expected_executable_indirect_calls = 2,
    .expected_executable_packed_fns = 1,
    .expected_executable_erased_fn_types = 1,
    .expected_lir_indirect_calls = 3,
};

fn countHostedProcSpecs(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted != null) count += 1;
    }
    return count;
}

fn countHostedProcSpecsWithoutBody(compiled: *const helpers.CompiledProgram) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted != null and proc.body == null) count += 1;
    }
    return count;
}

fn echoHostedFn(ops_raw: *anyopaque, _: *anyopaque, args_raw: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(args_raw));
    ops.dbg(roc_str.asSlice());
}

fn tickHostedFn(ops_raw: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    ops.dbg("tick");
}

const PairArgs = extern struct {
    first: builtins.str.RocStr,
    second: builtins.str.RocStr,
};

fn pairHostedFn(ops_raw: *anyopaque, _: *anyopaque, args_raw: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    const args: *const PairArgs = @ptrCast(@alignCast(args_raw));
    ops.dbg(args.first.asSlice());
    ops.dbg(args.second.asSlice());
}

fn attachHostedFns(runtime_env: *RuntimeHostEnv, hosted_fns: []const builtins.host_abi.HostedFn) void {
    const ops = runtime_env.get_ops();
    ops.hosted_fns = .{
        .count = @intCast(hosted_fns.len),
        .fns = @ptrCast(@constCast(hosted_fns.ptr)),
    };
}

fn runModuleWithInterpreter(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    hosted_fns: []const builtins.host_abi.HostedFn,
) !RuntimeHostEnv.RecordedRun {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    attachHostedFns(&runtime_env, hosted_fns);

    var interp = try Interpreter.init(
        allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try mainProcArgLayouts(allocator, compiled);
    defer allocator.free(arg_layouts);
    _ = try interp.eval(.{
        .proc_id = compiled.lowered.main_proc,
        .arg_layouts = arg_layouts,
    });
    return try runtime_env.snapshot(allocator);
}

fn runModuleWithDevBackend(
    allocator: std.mem.Allocator,
    compiled: *const helpers.CompiledProgram,
    hosted_fns: []const builtins.host_abi.HostedFn,
) !RuntimeHostEnv.RecordedRun {
    var codegen = try HostLirCodeGen.init(
        allocator,
        &compiled.lowered.lir_result.store,
        &compiled.lowered.lir_result.layouts,
        null,
    );
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(compiled.lowered.lir_result.store.getProcSpecs());

    const proc = compiled.lowered.lir_result.store.getProcSpec(compiled.lowered.main_proc);
    const arg_layouts = try mainProcArgLayouts(allocator, compiled);
    defer allocator.free(arg_layouts);
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_hosted_test_main",
        compiled.lowered.main_proc,
        arg_layouts,
        proc.ret_layout,
    );
    var exec_mem = try ExecutableMemory.initWithEntryOffset(
        codegen.getGeneratedCode(),
        entrypoint.offset,
    );
    defer exec_mem.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();
    attachHostedFns(&runtime_env, hosted_fns);

    const ret_layout = proc.ret_layout;
    const size_align = compiled.lowered.lir_result.layouts.layoutSizeAlign(
        compiled.lowered.lir_result.layouts.getLayout(ret_layout),
    );
    const alloc_len = @max(size_align.size, 1);
    const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
    defer allocator.free(ret_buf);
    @memset(ret_buf, 0);

    const arg_buf = try zeroedEntrypointArgBuffer(allocator, compiled, arg_layouts);
    defer if (arg_buf) |buf| allocator.free(buf);

    exec_mem.callRocABI(
        @ptrCast(runtime_env.get_ops()),
        @ptrCast(ret_buf.ptr),
        if (arg_buf) |buf| @ptrCast(buf.ptr) else null,
    );
    return try runtime_env.snapshot(allocator);
}

test "cor pipeline - recursive lambda factorial" {
    try expectInspect(
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
    ,
        "120",
    );
}

test "cor pipeline - mutual recursion in local lambdas" {
    try expectInspect(
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
    ,
        "True",
    );
}

test "cor pipeline - for loop sums list" {
    try expectInspect(
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
    ,
        "60",
    );
}

test "cor pipeline - for loop in lambda body" {
    try expectInspect(
        \\{
        \\    sum = |xs| {
        \\        var $sum = 0.I64
        \\        for item in xs {
        \\            $sum = $sum + item
        \\        }
        \\        $sum
        \\    }
        \\    sum([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
    ,
        "10",
    );
}

test "cor pipeline - generic local attached method specialization on nominal" {
    try expectInspectProgram(
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
        "(5, 8)",
    );
}

test "cor pipeline - generic local attached method specialization on nominal with arena allocator" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    var compiled = try helpers.compileInspectedProgram(
        allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(allocator);

    const actual = try helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered);
    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - generic local attached method specialization on nominal dev backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.devEvaluatorInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - generic local attached method specialization on nominal wasm backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.wasmEvaluatorInspectedStr(testing.allocator, &compiled.wasm_lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("(5, 8)", actual);
}

test "cor pipeline - generic local attached method specialization picks different nominal targets" {
    try expectInspectProgram(
        .module,
        \\Box := [Box(U64)].{
        \\  get : Box -> U64
        \\  get = |Box.Box(n)| n
        \\}
        \\
        \\Count := [Count(U64)].{
        \\  get : Count -> U64
        \\  get = |Count.Count(n)| n + 100
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Box.Box(5)), read(Count.Count(8)))
    ,
        &.{},
        "(5, 108)",
    );
}

test "cor pipeline - cross-module attached method specialization on imported nominal" {
    try expectInspectProgram(
        .module,
        \\import CounterMod
        \\
        \\main = CounterMod.Counter(41).get()
    ,
        &.{.{
            .name = "CounterMod",
            .source =
            \\Counter := [Counter(U64)].{
            \\  get : Counter -> U64
            \\  get = |Counter.Counter(n)| n
            \\}
            ,
        }},
        "41",
    );
}

test "cor pipeline - cross-module polymorphic attached method specialization from helper module" {
    try expectInspectProgram(
        .module,
        \\import BoxMod
        \\import CountMod
        \\import Helpers
        \\
        \\main = (Helpers.read(BoxMod.Box(5)), Helpers.read(CountMod.Count(8)))
    ,
        &.{
            .{
                .name = "BoxMod",
                .source =
                \\Box := [Box(U64)].{
                \\  get : Box -> U64
                \\  get = |Box.Box(n)| n
                \\}
                ,
            },
            .{
                .name = "CountMod",
                .source =
                \\Count := [Count(U64)].{
                \\  get : Count -> U64
                \\  get = |Count.Count(n)| n + 100
                \\}
                ,
            },
            .{
                .name = "Helpers",
                .source =
                \\module [read]
                \\
                \\read = |value| value.get()
                ,
            },
        },
        "(5, 108)",
    );
}

test "cor pipeline - record field access remains separate from method calls" {
    try expectInspectProgram(
        .expr,
        \\{
        \\    record = { get: |n| n + 1, value: 41 }
        \\    getter = record.get
        \\    getter(record.value)
        \\}
    ,
        &.{},
        "42.0",
    );
}

test "cor pipeline - recursive tag payload match issue 8754" {
    try expectInspectProgram(
        .module,
        \\Tree := [Node(Str, List(Tree)), Text(Str), Wrapper(Tree)]
        \\
        \\inner : Tree
        \\inner = Text("hello")
        \\
        \\wrapped : Tree
        \\wrapped = Wrapper(inner)
        \\
        \\main = match wrapped {
        \\    Wrapper(inner_tree) =>
        \\        match inner_tree {
        \\            Text(_) => 1
        \\            Node(_, _) => 2
        \\            Wrapper(_) => 3
        \\        }
        \\    _ => 0
        \\}
    ,
        &.{},
        "1.0",
    );
}

test "cor pipeline - recursive lambda with record" {
    try expectInspect(
        \\{
        \\    f = |n|
        \\        if n <= 0.I64
        \\            0.I64
        \\        else
        \\            { a: n, b: n * 2.I64, c: n * 3.I64, d: n * 4.I64 }.a + f(n - 1.I64)
        \\    f(100.I64)
        \\}
    ,
        "5050",
    );
}

test "cor pipeline - for loop early return" {
    try expectInspect(
        \\{
        \\    f = |list| {
        \\        for _item in list {
        \\            if True { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
    ,
        "True",
    );
}

test "cor pipeline - for loop closure early return" {
    try expectInspect(
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
        \\}
    ,
        "True",
    );
}

test "cor pipeline - inspect recursive nominal arithmetic expr" {
    try expectInspectProgramWithArena(
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
        "Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))",
    );
}

test "cor pipeline - inspect recursive nominal arithmetic expr dev backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.devEvaluatorInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))", actual);
}

test "cor pipeline - inspect recursive nominal arithmetic expr wasm backend" {
    var compiled = try helpers.compileInspectedProgram(
        testing.allocator,
        .module,
        \\Arith := [Lit(I64), Add(Arith, Arith), Mul(Arith, Arith), Neg(Arith)]
        \\
        \\main = Arith.Mul(
        \\    Arith.Add(Arith.Lit(2), Arith.Lit(3)),
        \\    Arith.Neg(Arith.Lit(4))
        \\)
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.wasmEvaluatorInspectedStr(testing.allocator, &compiled.wasm_lowered);
    defer testing.allocator.free(actual);

    try testing.expectEqualStrings("Mul(Add(Lit(2), Lit(3)), Neg(Lit(4)))", actual);
}

test "cor pipeline - eval direct-only higher-order call annotated callback parameter slot" {
    const case = direct_call_cases[0];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated callback parameter slot" {
    const case = direct_call_cases[0];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated function return slot" {
    const case = direct_call_cases[1];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated function return slot" {
    const case = direct_call_cases[1];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call abstract higher-order apply" {
    const case = direct_call_cases[2];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call abstract higher-order apply" {
    const case = direct_call_cases[2];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call nested polymorphic helper" {
    const case = direct_call_cases[3];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call nested polymorphic helper" {
    const case = direct_call_cases[3];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call apply twice" {
    const case = direct_call_cases[4];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call apply twice" {
    const case = direct_call_cases[4];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated local binding" {
    const case = direct_call_cases[5];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated local binding" {
    const case = direct_call_cases[5];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call passing captured closure to callback parameter" {
    const case = direct_call_cases[6];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call passing captured closure to callback parameter" {
    const case = direct_call_cases[6];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call annotated return of concrete lambda" {
    const case = direct_call_cases[7];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call annotated return of concrete lambda" {
    const case = direct_call_cases[7];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call passing concrete lambda to annotated callback parameter" {
    const case = direct_call_cases[8];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call passing concrete lambda to annotated callback parameter" {
    const case = direct_call_cases[8];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module annotated callback parameter slot" {
    const case = direct_call_cases[9];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module annotated callback parameter slot" {
    const case = direct_call_cases[9];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module annotated function return slot" {
    const case = direct_call_cases[10];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module annotated function return slot" {
    const case = direct_call_cases[10];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module abstract higher-order apply" {
    const case = direct_call_cases[11];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module abstract higher-order apply" {
    const case = direct_call_cases[11];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module nested higher-order bridge" {
    const case = direct_call_cases[12];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module nested higher-order bridge" {
    const case = direct_call_cases[12];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module apply twice" {
    const case = direct_call_cases[13];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module apply twice" {
    const case = direct_call_cases[13];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call cross-module passing captured closure to callback parameter" {
    const case = direct_call_cases[14];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call cross-module passing captured closure to callback parameter" {
    const case = direct_call_cases[14];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call two callback params with different captures" {
    const case = direct_call_cases[15];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call two callback params with different captures" {
    const case = direct_call_cases[15];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - eval direct-only higher-order call record field closure extraction then call" {
    const case = direct_call_cases[16];
    try expectInspectProgramWithArena(case.source_kind, case.source, case.imports, case.expected);
}

test "cor pipeline - lowering direct-only higher-order call record field closure extraction then call" {
    const case = direct_call_cases[16];
    try expectDirectOnlyLoweringProgram(case.source_kind, case.source, case.imports, case.executable_direct_calls, case.lir_direct_calls);
}

test "cor pipeline - lowering opaque function field lookup issue 9262 has no erased callables" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, opaque_function_field_lookup_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countExecutableIndirectCalls(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutablePackedFns(&executable.executable));
    try testing.expectEqual(@as(usize, 0), countExecutableErasedFnTypes(&executable.executable));

    var compiled = try helpers.compileProgram(arena_allocator, .module, opaque_function_field_lookup_source, &.{});
    defer compiled.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 0), countIndirectCalls(&compiled));
}

test "cor pipeline - lowering local callback specialization uses two distinct concrete defs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, additional_specialization_via_list_append_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 2), countSpecializedLocalFnDefs(&executable.executable));

    const result_tys = try collectSpecializedLocalFnResultTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), result_tys.len);
    try testing.expect(!executable.executable.types.equalIds(result_tys[0], result_tys[1]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[1]));
}

test "cor pipeline - lowering polymorphic two-list helper uses two distinct concrete defs" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, polymorphic_two_list_types_source, &.{});
    defer executable.deinit(arena_allocator);

    try testing.expectEqual(@as(usize, 2), countSpecializedLocalFnDefs(&executable.executable));
    try testing.expectEqual(@as(usize, 2), try countUniqueDirectCallTargetsNamed(
        arena_allocator,
        &executable.executable,
    ));

    const arg_tys = try collectSpecializedLocalFnFirstArgTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), arg_tys.len);
    try testing.expect(!executable.executable.types.equalIds(arg_tys[0], arg_tys[1]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(arg_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(arg_tys[1]));

    const result_tys = try collectSpecializedLocalFnResultTypes(
        arena_allocator,
        &executable.executable,
    );
    try testing.expectEqual(@as(usize, 2), result_tys.len);
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[0]));
    try testing.expect(!executable.executable.types.containsAbstractLeaf(result_tys[1]));
    try testing.expectEqual(@as(usize, 0), countExecutableBridgeNodes(&executable.executable));

    var ir_program = try compileIrProgram(arena_allocator, .module, polymorphic_two_list_types_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - lowering direct-only higher-order call has no executable or ir bridges" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, simple_direct_call_source, &.{});
    defer executable.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countExecutableBridgeNodes(&executable.executable));

    var ir_program = try compileIrProgram(arena_allocator, .module, simple_direct_call_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(@as(usize, 0), countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - typed nested captures" {
    try expectInspectProgram(.expr, typed_nested_captures_source, &.{}, "6");
}

test "cor pipeline - lowering emits explicit bridges only at real representation boundaries" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var executable = try compileExecutableProgram(arena_allocator, .module, annotated_callback_param_source, &.{});
    defer executable.deinit(arena_allocator);
    const executable_bridges = countExecutableBridgeNodes(&executable.executable);
    try testing.expect(executable_bridges >= 1);

    var ir_program = try compileIrProgram(arena_allocator, .module, annotated_callback_param_source, &.{});
    defer ir_program.deinit(arena_allocator);
    try testing.expectEqual(executable_bridges, countIrBridgeNodes(&ir_program.ir_result));
}

test "cor pipeline - boxed lambda lowering uses erased indirect-call path" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();
    var compiled = try helpers.compileInspectedExpr(
        arena_allocator,
        \\{
        \\    wrap = |boxed| { value: boxed }
        \\    unwrap = |record| record.value
        \\    f = Box.unbox(unwrap(wrap(Box.box(|x| x + 1.I64))))
        \\    f(41.I64)
        \\}
        ,
    );
    defer compiled.deinit(arena_allocator);

    const actual = try helpers.lirInterpreterInspectedStr(arena_allocator, &compiled.lowered);
    try testing.expectEqualStrings("42", actual);

    try testing.expect(countIndirectCalls(&compiled) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_box) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_unbox) >= 1);
    try expectErasedLoweringProgram(
        boxed_lambda_round_trip_erased_case.source_kind,
        boxed_lambda_round_trip_erased_case.source,
        boxed_lambda_round_trip_erased_case.imports,
        boxed_lambda_round_trip_erased_case.expected_executable_indirect_calls,
        boxed_lambda_round_trip_erased_case.expected_executable_packed_fns,
        boxed_lambda_round_trip_erased_case.expected_executable_erased_fn_types,
        boxed_lambda_round_trip_erased_case.expected_lir_indirect_calls,
    );
}

test "cor pipeline - canonical hosted lambda fact has no fake body field" {
    try testing.expect(!@hasField(@FieldType(can.CIR.Expr, "e_hosted_lambda"), "body"));
}

test "cor pipeline - echo hosted proc metadata reaches lir" {
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\main! = |_args| {
        \\    echo!("Hello from hosted")
        \\}
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    try testing.expect(countHostedProcSpecs(&compiled) >= 1);
    try testing.expectEqual(countHostedProcSpecs(&compiled), countHostedProcSpecsWithoutBody(&compiled));

    var saw_echo = false;
    for (compiled.lowered.lir_result.store.getProcSpecs()) |proc| {
        if (proc.hosted) |hosted| {
            try testing.expectEqual(@as(u32, 0), hosted.index);
            try testing.expectEqualStrings("echo!", compiled.resources.module_env.getIdent(hosted.symbol_name));
            try testing.expect(proc.body == null);
            saw_echo = true;
        }
    }
    try testing.expect(saw_echo);
}

test "cor pipeline - echo hosted proc call reaches interpreter and dev backend" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\main! = |_args| {
        \\    echo!("Hello from hosted")
        \\    echo!("Again")
        \\}
    ,
        &.{},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("Hello from hosted", interp_run.events[0].bytes());
    try testing.expectEqualStrings("Again", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("Hello from hosted", dev_run.events[0].bytes());
    try testing.expectEqualStrings("Again", dev_run.events[1].bytes());
}

test "cor pipeline - hosted function can flow as a first-class argument" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    ["hello"].for_each!(Platform.line!)
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\module [line!]
            \\
            \\line! : Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 1), interp_run.events.len);
    try testing.expectEqualStrings("hello", interp_run.events[0].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 1), dev_run.events.len);
    try testing.expectEqualStrings("hello", dev_run.events[0].bytes());
}

test "cor pipeline - hosted function survives boxed indirect-call round trip" {
    // Blocked by a pre-existing monotype_lifted placeholder invariant failure.
    return error.SkipZigTest;
}

test "cor pipeline - boxed lambda round trip through host boundary" {
    // Blocked by a pre-existing parse/check failure in the hosted helper module.
    return error.SkipZigTest;
}

test "cor pipeline - zero-arg hosted proc call reaches host abi" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&tickHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    Platform.tick!()
        \\    Platform.tick!()
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\tick! : {} => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("tick", interp_run.events[0].bytes());
    try testing.expectEqualStrings("tick", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("tick", dev_run.events[0].bytes());
    try testing.expectEqualStrings("tick", dev_run.events[1].bytes());
}

test "cor pipeline - multi-arg hosted proc call preserves argument marshaling" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&pairHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    Platform.pair!("left", "right")
        \\}
    ,
        &.{.{
            .name = "Platform",
            .source =
            \\pair! : Str, Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("left", interp_run.events[0].bytes());
    try testing.expectEqualStrings("right", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("left", dev_run.events[0].bytes());
    try testing.expectEqualStrings("right", dev_run.events[1].bytes());
}
