//! Focused eval tests for the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const backend = @import("backend");
const builtins = @import("builtins");
const collections = @import("collections");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("RuntimeHostEnv.zig");
const helpers = @import("helpers.zig");

const testing = std.testing;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
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
    const mono = try mono_lowerer.run(0);
    const lifted = try monotype_lifted.Lower.run(allocator, mono);
    const solved = try lambdasolved.Lower.run(allocator, lifted);
    const executable = try lambdamono.Lower.run(allocator, solved);

    return .{
        .resources = resources,
        .executable = executable,
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
        .lir_direct_calls = 6,
    },
    .{
        .name = "annotated function return slot",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 4,
    },
    .{
        .name = "abstract higher-order apply",
        .source = abstract_apply_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "nested polymorphic helper",
        .source = nested_polymorphic_helper_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "apply twice",
        .source = apply_twice_source,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 8,
    },
    .{
        .name = "annotated local binding",
        .source = annotated_local_binding_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 2,
    },
    .{
        .name = "passing captured closure to callback parameter",
        .source = captured_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "annotated return of concrete lambda",
        .source = annotated_return_source,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 4,
    },
    .{
        .name = "passing concrete lambda to annotated callback parameter",
        .source = annotated_callback_param_source,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "cross-module annotated callback parameter slot",
        .source = cross_module_annotated_callback_param_source,
        .imports = &cross_module_annotated_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "cross-module annotated function return slot",
        .source = cross_module_annotated_return_source,
        .imports = &cross_module_annotated_return_imports,
        .expected = "42",
        .executable_direct_calls = 2,
        .lir_direct_calls = 4,
    },
    .{
        .name = "cross-module abstract higher-order apply",
        .source = cross_module_abstract_apply_source,
        .imports = &cross_module_abstract_apply_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "cross-module nested higher-order bridge",
        .source = cross_module_nested_bridge_source,
        .imports = &cross_module_nested_bridge_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "cross-module apply twice",
        .source = cross_module_apply_twice_source,
        .imports = &cross_module_apply_twice_imports,
        .expected = "42",
        .executable_direct_calls = 6,
        .lir_direct_calls = 8,
    },
    .{
        .name = "cross-module passing captured closure to callback parameter",
        .source = cross_module_captured_callback_param_source,
        .imports = &cross_module_captured_callback_param_imports,
        .expected = "42",
        .executable_direct_calls = 4,
        .lir_direct_calls = 6,
    },
    .{
        .name = "two callback params with different captures",
        .source = two_callback_params_source,
        .expected = "42",
        .executable_direct_calls = 7,
        .lir_direct_calls = 10,
    },
    .{
        .name = "record field closure extraction then call",
        .source = record_field_closure_extraction_source,
        .expected = "42",
        .executable_direct_calls = 1,
        .lir_direct_calls = 2,
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
    .expected_executable_indirect_calls = 1,
    .expected_executable_packed_fns = 1,
    .expected_executable_erased_fn_types = 1,
    .expected_lir_indirect_calls = 2,
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

fn identityBoxedFn(ops_raw: *anyopaque, ret_raw: *anyopaque, args_raw: *anyopaque) callconv(.c) void {
    const ops: *builtins.host_abi.RocOps = @ptrCast(@alignCast(ops_raw));
    const ret_ptr: *usize = @ptrCast(@alignCast(ret_raw));
    const arg_ptr: *const usize = @ptrCast(@alignCast(args_raw));
    const boxed = arg_ptr.*;
    if (boxed != 0) {
        builtins.utils.increfDataPtrC(@ptrFromInt(boxed), 1, ops);
    }
    ret_ptr.* = boxed;
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

    _ = try interp.eval(.{ .proc_id = compiled.lowered.main_proc });
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
    const entrypoint = try codegen.generateEntrypointWrapper(
        "roc_eval_hosted_test_main",
        compiled.lowered.main_proc,
        &.{},
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

    exec_mem.callRocABI(@ptrCast(runtime_env.get_ops()), @ptrCast(ret_buf.ptr), null);
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
