//! Focused eval tests for the new cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check_test_env = @import("check_test_env");
const monotype = @import("monotype");
const monotype_lifted = @import("monotype_lifted");
const lambdasolved = @import("lambdasolved");
const lambdamono = @import("lambdamono");
const ir = @import("ir");
const lir = @import("lir");
const layout = @import("layout");

const FromIr = lir.FromIr;
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeEnv = @import("TestEnv.zig");
const CheckTestEnv = check_test_env.TestEnv;

const testing = std.testing;

fn entryProcId(result: *const FromIr.Result) !lir.LIR.LirProcSpecId {
    if (result.root_procs.items.len == 0) return error.NoRootProc;
    return result.root_procs.items[result.root_procs.items.len - 1];
}

fn evalI64Expr(comptime source: []const u8) !i64 {
    var test_env = try CheckTestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const all_module_envs = [_]*const can.ModuleEnv{
        test_env.module_env,
        test_env.builtin_module.env,
    };
    test_env.module_env.imports.resolveImports(test_env.module_env, &all_module_envs);

    var mono_lowerer = monotype.Lower.Lowerer.init(testing.allocator, &all_module_envs, 1);
    defer mono_lowerer.deinit();
    const mono = try mono_lowerer.run(0);
    const lifted = try monotype_lifted.Lower.run(testing.allocator, mono);
    const solved = try lambdasolved.Lower.run(testing.allocator, lifted);
    const executable = try lambdamono.Lower.run(testing.allocator, solved);
    const lowered_ir = try ir.Lower.run(testing.allocator, executable);
    var lowered_lir = try FromIr.run(
        testing.allocator,
        &all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lowered_lir.deinit();

    var runtime_env = RuntimeEnv.init(testing.allocator);
    defer runtime_env.deinit();
    const roc_ops = runtime_env.get_ops();

    var interp = try Interpreter.init(testing.allocator, &lowered_lir.store, &lowered_lir.layouts, roc_ops);
    defer interp.deinit();

    const main_proc = try entryProcId(&lowered_lir);
    const result = try interp.eval(.{ .proc_id = main_proc });
    const ret_layout = lowered_lir.store.getProcSpec(main_proc).ret_layout;
    try testing.expectEqual(layout.Idx.i64, ret_layout);
    return result.value.read(i64);
}

fn evalBoolExpr(comptime source: []const u8) !bool {
    var test_env = try CheckTestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();

    const all_module_envs = [_]*const can.ModuleEnv{
        test_env.module_env,
        test_env.builtin_module.env,
    };
    test_env.module_env.imports.resolveImports(test_env.module_env, &all_module_envs);

    var mono_lowerer = monotype.Lower.Lowerer.init(testing.allocator, &all_module_envs, 1);
    defer mono_lowerer.deinit();
    const mono = try mono_lowerer.run(0);
    const lifted = try monotype_lifted.Lower.run(testing.allocator, mono);
    const solved = try lambdasolved.Lower.run(testing.allocator, lifted);
    const executable = try lambdamono.Lower.run(testing.allocator, solved);
    const lowered_ir = try ir.Lower.run(testing.allocator, executable);
    var lowered_lir = try FromIr.run(
        testing.allocator,
        &all_module_envs,
        null,
        base.target.TargetUsize.native,
        lowered_ir,
    );
    defer lowered_lir.deinit();

    var runtime_env = RuntimeEnv.init(testing.allocator);
    defer runtime_env.deinit();
    const roc_ops = runtime_env.get_ops();

    var interp = try Interpreter.init(testing.allocator, &lowered_lir.store, &lowered_lir.layouts, roc_ops);
    defer interp.deinit();

    const main_proc = try entryProcId(&lowered_lir);
    const result = try interp.eval(.{ .proc_id = main_proc });
    const ret_layout = lowered_lir.store.getProcSpec(main_proc).ret_layout;
    try testing.expectEqual(layout.Idx.bool, ret_layout);
    return result.value.read(u8) != 0;
}

fn expectI64(comptime source: []const u8, expected: i64) !void {
    try testing.expectEqual(expected, try evalI64Expr(source));
}

fn expectBool(comptime source: []const u8, expected: bool) !void {
    try testing.expectEqual(expected, try evalBoolExpr(source));
}

test "cor pipeline - recursive lambda factorial" {
    try expectI64(
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
        ,
        120,
    );
}

test "cor pipeline - mutual recursion in local lambdas" {
    try expectBool(
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
        ,
        true,
    );
}

test "cor pipeline - for loop sums list" {
    try expectI64(
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        60,
    );
}

test "cor pipeline - for loop in lambda body" {
    try expectI64(
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
        10,
    );
}

test "cor pipeline - recursive lambda with record" {
    try expectI64(
        \\{
        \\    f = |n|
        \\        if n <= 0.I64
        \\            0.I64
        \\        else
        \\            { a: n, b: n * 2.I64, c: n * 3.I64, d: n * 4.I64 }.a + f(n - 1.I64)
        \\    f(100.I64)
        \\}
        ,
        5050,
    );
}

test "cor pipeline - for loop early return" {
    try expectBool(
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
        true,
    );
}

test "cor pipeline - for loop closure early return" {
    try expectBool(
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
        true,
    );
}
