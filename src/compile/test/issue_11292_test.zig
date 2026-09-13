//! Regression tests for generalized literal conversion and equality guards.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestExpectedError, TestUnexpectedError };
const app_path = "test/postcheck/issue_11292_generalized_string_pattern/app.roc";

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram, expected_crash: bool) RunError!void {
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
        return;
    }
    _ = try result;
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    try host.checkForLeaks();
}

fn expectAppRunsSuccessfully(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered, false) catch |err| {
        std.log.err("generalized string pattern run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn expectInvalidLiteral(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered, true) catch |err| {
        std.log.err("invalid quote conversion failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11292: Boxy lowers and runs a generalized string literal pattern" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11292: LSS lowers and runs a generalized string literal pattern" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

test "issue 11292: custom conversion and equality with generic forwarding and guard fallthrough" {
    inline for (.{ .boxy, .lss }) |strategy| {
        try harness.runAppPathLoweredInspection(
            "test/postcheck/issue_11292_generalized_string_pattern/custom.roc",
            .{ .specialization_strategy = strategy },
            expectAppRunsSuccessfully,
        );
    }
}

test "issue 11292: a rejected runtime quote conversion crashes instead of missing the pattern" {
    inline for (.{ .boxy, .lss }) |strategy| {
        try harness.runAppPathLoweredInspection(
            "test/postcheck/issue_11292_generalized_string_pattern/rejected.roc",
            .{ .specialization_strategy = strategy },
            expectInvalidLiteral,
        );
    }
}
