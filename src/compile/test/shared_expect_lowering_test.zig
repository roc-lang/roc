//! One specialization must preserve both expect execution and its omission.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const GuardedList = lir.LirStore.GuardedList;

const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestExpectedError, TestUnexpectedError };

fn runConsumer(lowered: *const lir.CheckedPipeline.LoweredProgram, expected_crash: bool, expected_exit: i8) RunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    const program = &lowered.lir_result;
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        eval.LirInterpreter.BoxyTables.fromResult(program),
        host.get_ops(),
        .preserve,
    );
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    var exit_code: i8 = -1;
    const result = interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&exit_code),
    });
    if (expected_crash) {
        try std.testing.expectError(error.Crash, result);
    } else {
        _ = try result;
        try std.testing.expectEqual(expected_exit, exit_code);
        try host.checkForLeaks();
    }
}

fn inspectConsumers(prepared: *const lir.CheckedPipeline.PreparedMonotype, run_crashes: bool, omit_crashes: bool) harness.LowerToLirHarnessError!void {
    const source_expr_count = prepared.program.view().exprs.len;
    var solved = try lir.CheckedPipeline.prepareMonotypeToSolved(try prepared.forkForConsumer(prepared.target.target_usize, .run));
    defer solved.deinit();
    for ([_]lir.CheckedPipeline.InlineExpectMode{ .run, .omit }, [_]bool{ run_crashes, omit_crashes }) |mode, expected_crash| {
        const fork = try solved.forkForConsumer(prepared.target.target_usize, mode);
        var lowered = try lir.CheckedPipeline.lowerPreparedSolvedToLir(fork);
        defer lowered.deinit();
        runConsumer(&lowered, expected_crash, 7) catch |err| {
            std.log.err("shared expect consumer {s}: {s}", .{ @tagName(mode), @errorName(err) });
            return error.TestUnexpectedResult;
        };
    }
    try std.testing.expectEqual(source_expr_count, prepared.program.view().exprs.len);
}

fn expectConditionDiverges(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    try inspectConsumers(prepared, true, false);
}

fn ordinaryConditionDiverges(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    try inspectConsumers(prepared, true, true);
}

fn neitherConsumerDiverges(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    try inspectConsumers(prepared, false, false);
}

test "shared expect lowering preserves the omitted continuation after a divergent condition" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    expect if List.is_empty(args) { crash "empty expect" } else { crash "nonempty expect" }
        \\    Err(Exit(7))
        \\}
    , .{ .shared_comptime_reads = true, .prepared_inspect = expectConditionDiverges });
}

test "shared expect lowering retains ordinary divergence outside an expect" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    _ = if List.is_empty(args) { crash "empty ordinary" } else { crash "nonempty ordinary" }
        \\    Err(Exit(7))
        \\}
    , .{ .shared_comptime_reads = true, .prepared_inspect = ordinaryConditionDiverges });
}

test "shared expect lowering retains the continuation of a returning condition" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    expect List.is_empty(args)
        \\    Err(Exit(7))
        \\}
    , .{ .shared_comptime_reads = true, .prepared_inspect = neitherConsumerDiverges });
}

fn inspectMutation(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    var solved = try lir.CheckedPipeline.prepareMonotypeToSolved(try prepared.forkForConsumer(prepared.target.target_usize, .run));
    defer solved.deinit();
    for ([_]lir.CheckedPipeline.InlineExpectMode{ .run, .omit }, [_]i8{ 8, 7 }) |mode, expected_exit| {
        const fork = try solved.forkForConsumer(prepared.target.target_usize, mode);
        var lowered = try lir.CheckedPipeline.lowerPreparedSolvedToLir(fork);
        defer lowered.deinit();
        runConsumer(&lowered, false, expected_exit) catch return error.TestUnexpectedResult;
    }
}

test "shared expect lowering preserves consumer-specific outer variable mutation" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    var code = 7
        \\    expect {
        \\        code = 8
        \\        List.is_empty(args)
        \\    }
        \\    Err(Exit(code))
        \\}
    , .{ .shared_comptime_reads = true, .prepared_inspect = inspectMutation });
}

test "shared expect lowering merges callable identities assigned by the condition" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |args| {
        \\    seven : {} -> I8
        \\    seven = |_| 7
        \\    eight : {} -> I8
        \\    eight = |_| 8
        \\    var decide = seven
        \\    expect {
        \\        decide = eight
        \\        List.is_empty(args)
        \\    }
        \\    Err(Exit(decide({})))
        \\}
    , .{ .shared_comptime_reads = true, .prepared_inspect = inspectMutation });
}
