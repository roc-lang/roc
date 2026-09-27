//! Repro tests for https://github.com/roc-lang/roc/issues/11732
//!
//! The app passes `args.db` to a callback field and also uses it for method
//! dispatch (`args.db.query(...)`). Post-check Monotype lowering must lower
//! the app to LIR without hitting the invariant "checked target contract
//! differed from substitution-derived evidence kind".
const base = @import("base");
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const fixture_dir = "test/postcheck/issue_11732_callback_field_and_method_dispatch/";

const LowerToLirHarnessError = harness.LowerToLirHarnessError;

fn noInspection(_: *const lir.CheckedPipeline.LoweredProgram) LowerToLirHarnessError!void {}

test "issue 11732: LSS value passed to a callback field and used for method dispatch" {
    try harness.runAppPathLoweredInspection(
        fixture_dir ++ "app.roc",
        .{ .specialization_strategy = .lss },
        noInspection,
    );
}

test "issue 11732: Boxy value passed to a callback field and used for method dispatch" {
    try harness.runAppPathLoweredInspection(
        fixture_dir ++ "app.roc",
        .{ .specialization_strategy = .boxy },
        noInspection,
    );
}

fn inspectRuntimeResult(lowered: *const lir.CheckedPipeline.LoweredProgram) LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("issue 11732 runtime result failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram) !void {
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
    const arg_layout = program.store.getLocal(lir.LirStore.GuardedList.at(args, 0)).layout_idx;
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

test "issue 11732: reversed dispatch order preserves distinct hidden receivers at runtime" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.runAppPathLoweredInspection(
            fixture_dir ++ "reordered.roc",
            .{ .specialization_strategy = strategy },
            inspectRuntimeResult,
        );
    }
}
