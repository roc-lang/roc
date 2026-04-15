//! Focused eval tests for the cor-style lowering pipeline.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const eval = @import("eval");
const backend = @import("backend");
const builtins = @import("builtins");
const collections = @import("collections");
const helpers = @import("helpers.zig");

const testing = std.testing;
const Interpreter = eval.Interpreter;
const RuntimeHostEnv = eval.RuntimeHostEnv;
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
fn expectInspect(comptime source: []const u8, expected: []const u8) !void {
    var compiled = try helpers.compileInspectedExpr(testing.allocator, source);
    defer compiled.deinit(testing.allocator);
    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
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

fn countIndirectCalls(compiled: *const helpers.CompiledInspectedExpr) usize {
    var count: usize = 0;
    for (compiled.lowered.lir_result.store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_call_indirect => count += 1,
            else => {},
        }
    }
    return count;
}

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

    try interp.eval(.{ .proc_id = compiled.lowered.main_proc });
    const snapshot = try runtime_env.snapshot(allocator);
    try runtime_env.checkForLeaks();
    return snapshot;
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
    const snapshot = try runtime_env.snapshot(allocator);
    try runtime_env.checkForLeaks();
    return snapshot;
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

test "cor pipeline - boxed lambda lowering uses erased indirect-call path" {
    var compiled = try helpers.compileInspectedExpr(
        testing.allocator,
        \\{
        \\    wrap = |boxed| { value: boxed }
        \\    unwrap = |record| record.value
        \\    f = Box.unbox(unwrap(wrap(Box.box(|x| x + 1.I64))))
        \\    f(41.I64)
        \\}
        ,
    );
    defer compiled.deinit(testing.allocator);

    const actual = try helpers.lirInterpreterInspectedStr(testing.allocator, &compiled.lowered);
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings("42", actual);

    try testing.expect(countIndirectCalls(&compiled) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_box) >= 1);
    try testing.expect(countLowLevelOp(&compiled, .box_unbox) >= 1);
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

test "cor pipeline - recursive list_first branch into hosted proc keeps ownership explicit" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\Node := [Text(Str), Element(Str, List(Node))]
        \\
        \\main! = || {
        \\    text_node : Node
        \\    text_node = Text("hello")
        \\    children : List(Node)
        \\    children = [text_node]
        \\    match List.first(children) {
        \\        Ok(child) =>
        \\            match child {
        \\                Text(_) => Platform.line!("Text")
        \\                Element(_, _) => Platform.line!("Element")
        \\            }
        \\        Err(_) => Platform.line!("Err")
        \\    }
        \\}
        ,
        &.{.{
            .name = "Platform",
            .source =
            \\line! : Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 1), interp_run.events.len);
    try testing.expectEqualStrings("Text", interp_run.events[0].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 1), dev_run.events.len);
    try testing.expectEqualStrings("Text", dev_run.events[0].bytes());
}

test "cor pipeline - hosted function survives boxed indirect-call round trip" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    f = Box.unbox(Box.box(Platform.line!))
        \\    f("hello")
        \\    f("again")
        \\}
        ,
        &.{.{
            .name = "Platform",
            .source =
            \\line! : Str => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    try testing.expect(countIndirectCalls(&compiled) >= 1);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("hello", interp_run.events[0].bytes());
    try testing.expectEqualStrings("again", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("hello", dev_run.events[0].bytes());
    try testing.expectEqualStrings("again", dev_run.events[1].bytes());
}

test "cor pipeline - boxed lambda round trip through host boundary" {
    const hosted_fns = [_]builtins.host_abi.HostedFn{
        builtins.host_abi.hostedFn(&identityBoxedFn),
        builtins.host_abi.hostedFn(&echoHostedFn),
    };
    var compiled = try helpers.compileProgram(
        testing.allocator,
        .module,
        \\import Platform
        \\
        \\main! = || {
        \\    make = |n: I64| |x: I64| x + n
        \\    boxed = Box.box(make(5))
        \\    round = Platform.identity!(boxed)
        \\    f = Box.unbox(round)
        \\    Platform.line!(I64.to_str(f(1)))
        \\    Platform.line!(I64.to_str(f(2)))
        \\}
        ,
        &.{.{
            .name = "Platform",
            .source =
            \\line! : Str => {}
            \\identity! : Box(I64 -> I64) -> Box(I64 -> I64) => {}
            ,
        }},
    );
    defer compiled.deinit(testing.allocator);

    try testing.expect(countIndirectCalls(&compiled) >= 1);

    var interp_run = try runModuleWithInterpreter(testing.allocator, &compiled, &hosted_fns);
    defer interp_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, interp_run.termination);
    try testing.expectEqual(@as(usize, 2), interp_run.events.len);
    try testing.expectEqualStrings("6", interp_run.events[0].bytes());
    try testing.expectEqualStrings("7", interp_run.events[1].bytes());

    var dev_run = try runModuleWithDevBackend(testing.allocator, &compiled, &hosted_fns);
    defer dev_run.deinit(testing.allocator);
    try testing.expectEqual(RuntimeHostEnv.Termination.returned, dev_run.termination);
    try testing.expectEqual(@as(usize, 2), dev_run.events.len);
    try testing.expectEqualStrings("6", dev_run.events[0].bytes());
    try testing.expectEqualStrings("7", dev_run.events[1].bytes());
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
