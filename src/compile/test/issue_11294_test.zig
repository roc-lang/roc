//! Regression test for issue #11294: nominal descriptors through a dynamic formal.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestExpectedError, TestUnexpectedError };
const app_path = "test/postcheck/issue_11294_map2_dynamic_element_formal/app.roc";

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram) RunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    const program = &lowered.lir_result;
    var static_strings = try eval.LirInterpreter.buildStaticStrings(std.testing.allocator, &program.store);
    defer static_strings.deinit();
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        eval.LirInterpreter.BoxyTables.fromResult(program),
        static_strings.view(),
        host.get_ops(),
    );
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    var exit_code: i8 = -1;
    _ = try interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&exit_code),
    });
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    try host.checkForLeaks();
}

fn expectAppRunsSuccessfully(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("map2 element formal run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11294: LSS lowers and runs map2 at a concrete element type" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

test "issue 11294: Boxy lowers and runs map2 at a concrete element type" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11294: map2 preserves nested ownership and independent instantiations" {
    inline for (.{ .boxy, .lss }) |strategy| {
        try harness.runAppPathLoweredInspection(
            "test/postcheck/issue_11294_map2_dynamic_element_formal/ownership.roc",
            .{ .specialization_strategy = strategy },
            expectAppRunsSuccessfully,
        );
    }
}
