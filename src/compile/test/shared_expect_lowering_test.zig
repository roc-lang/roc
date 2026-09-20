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
        var lowered = try lir.CheckedPipeline.lowerConsumerToLir(&solved, consumerFor(&solved, mode));
        defer lowered.deinit();
        runConsumer(&lowered, expected_crash, 7) catch |err| {
            std.log.err("shared expect consumer {s}: {s}", .{ @tagName(mode), @errorName(err) });
            return error.TestUnexpectedResult;
        };
    }
    try std.testing.expectEqual(source_expr_count, prepared.program.view().exprs.len);
}

/// Both expect modes are consumers of one borrowed solved program: the
/// program is never copied, and neither mode repeats any producer stage.
fn consumerFor(
    solved: *const lir.CheckedPipeline.PreparedSolved,
    mode: lir.CheckedPipeline.InlineExpectMode,
) lir.CheckedPipeline.Consumer {
    return .{
        .roots = .{},
        .target_usize = solved.target.target_usize,
        .inline_expects = mode,
        .observers = lir.CheckedPipeline.Observers.fromTarget(solved.target),
    };
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
        var lowered = try lir.CheckedPipeline.lowerConsumerToLir(&solved, consumerFor(&solved, mode));
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

/// A copy of the producer program, owned by `allocator`, for an entrance that
/// consumes what it is given.
fn preparedCopy(
    allocator: std.mem.Allocator,
    prepared: *const lir.CheckedPipeline.PreparedMonotype,
) harness.LowerToLirHarnessError!lir.CheckedPipeline.PreparedMonotype {
    var program = try prepared.program.cloneFrozen(allocator);
    errdefer program.deinit();
    const metadata = try allocator.dupe(lir.CheckedPipeline.RootTestPlanMetadata, prepared.test_plan_metadata);
    return .{
        .allocator = allocator,
        .program = program,
        .target = prepared.target,
        .root_count = prepared.root_count,
        .test_plan_metadata = metadata,
    };
}

/// The consuming entrance owns the producer program from the call onward, so
/// it releases it exactly once whether its continuation succeeds or fails.
/// Every byte below is one of these allocators', so an entrance that leaked
/// the program or freed it twice is what the totals would report.
fn inspectConsumedOwnership(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    const solved = try lir.CheckedPipeline.prepareMonotypeToSolved(try preparedCopy(failing.allocator(), prepared));
    // Preparation is done, so the next allocation belongs to the consumer
    // continuation that has taken ownership of the solved program.
    failing.fail_index = failing.alloc_index;
    if (lir.CheckedPipeline.lowerPreparedSolvedToLir(solved)) |lowered| {
        var owned = lowered;
        owned.deinit();
        return error.TestUnexpectedResult;
    } else |err| switch (err) {
        error.OutOfMemory => {},
        error.HostedFunctionNotBound => return error.TestUnexpectedResult,
    }
    try std.testing.expectEqual(failing.allocated_bytes, failing.freed_bytes);

    var succeeding = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    const complete = try lir.CheckedPipeline.prepareMonotypeToSolved(try preparedCopy(succeeding.allocator(), prepared));
    var lowered = try lir.CheckedPipeline.lowerPreparedSolvedToLir(complete);
    lowered.deinit();
    try std.testing.expectEqual(succeeding.allocated_bytes, succeeding.freed_bytes);
}

test "a consumed producer program is released whether its continuation succeeds or fails" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| Ok({})
    , .{ .prepared_inspect = inspectConsumedOwnership });
}
