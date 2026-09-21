//! Regression tests for issue #11441: forwarded composite method evidence.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const postcheck = @import("postcheck");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };
const app_path = "test/postcheck/issue_11441_method_json_record_payload/app.roc";

fn runApp(lowered: *const lir.CheckedPipeline.LoweredProgram) RunError!void {
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
        std.log.err("helper-dispatched codec run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11441: a helper dispatching a method that encodes a record of its generic argument lowers and runs" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

test "issue 11441: generic forwarding preserves separate record payload specializations" {
    try harness.runAppPathLoweredInspection(
        "test/postcheck/issue_11441_method_json_record_payload/generic.roc",
        .{ .specialization_strategy = .lss },
        expectAppRunsSuccessfully,
    );
}

test "issue 11441: a local helper forwards composite method evidence" {
    try harness.runAppPathLoweredInspection(
        "test/postcheck/issue_11441_method_json_record_payload/local.roc",
        .{ .specialization_strategy = .lss },
        expectAppRunsSuccessfully,
    );
}

test "issue 11441: method dispatch forwards tuple encoder evidence" {
    try harness.runAppPathLoweredInspection(
        "test/postcheck/issue_11441_method_json_record_payload/tuple.roc",
        .{ .specialization_strategy = .lss },
        expectAppRunsSuccessfully,
    );
}

test "issue 11441: method dispatch forwards record parser evidence" {
    try harness.runAppPathLoweredInspection(
        "test/postcheck/issue_11441_method_json_record_payload/parse.roc",
        .{ .specialization_strategy = .lss },
        expectAppRunsSuccessfully,
    );
}

test "issue 11441: equivalent method codec calls reuse specializations" {
    const prefix =
        \\Page := [P].{
        \\    send = |_page, payload| Json.to_str({ a: payload })
        \\}
        \\helper = |page, payload| page.send(payload)
        \\main! = |args| {
        \\    echo!(helper(Page.P, args))
        \\
    ;
    const suffix =
        \\    Ok({})
        \\}
    ;
    var once: postcheck.Monotype.Lower.Diagnostics = .{};
    var repeated: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(prefix ++ suffix, .{ .monotype_only = true, .monotype_diagnostics_out = &once });
    try harness.expectLowersToLirWithOptions(prefix ++ "    echo!(helper(Page.P, args))\n" ++ suffix, .{ .monotype_only = true, .monotype_diagnostics_out = &repeated });
    try std.testing.expectEqual(once.specialization.template_misses, repeated.specialization.template_misses);
    try std.testing.expectEqual(once.specialization.nested_misses, repeated.specialization.nested_misses);
    try std.testing.expectEqual(@as(u64, 0), repeated.specialization.evidence_missing);
}

test "issue 11441: materialized method contracts bind signature-only intermediate types" {
    try harness.runAppPathLoweredInspection(
        "test/postcheck/issue_11441_method_json_record_payload/methods.roc",
        .{ .specialization_strategy = .lss },
        expectAppRunsSuccessfully,
    );
}
