//! Regression tests for issue #11560: Boxy call boundaries where an unannotated
//! callee names a structure that the call site wraps in an alias or nominal.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestExpectedError, TestUnexpectedError };
const app_path = "test/postcheck/issue_11560_unannotated_try_match/app.roc";
const list_payload_app_path = "test/postcheck/issue_11560_unannotated_try_match/list_payload.roc";
const alias_payload_app_path = "test/postcheck/issue_11560_unannotated_try_match/alias_payload.roc";
const user_nominal_app_path = "test/postcheck/issue_11560_unannotated_try_match/user_nominal.roc";
const opaque_record_app_path = "test/postcheck/issue_11560_unannotated_try_match/opaque_record.roc";
const record_field_app_path = "test/postcheck/issue_11560_unannotated_try_match/record_field.roc";

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
        std.log.err("unannotated Try match run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11560: LSS lowers and runs a call to an unannotated Try match" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// repro for https://github.com/roc-lang/roc/issues/11560
//
// `h` is unannotated and matches on its `Try` argument, and `main!` calls `h`
// with a `Try(U64, U8)`. Under the boxy strategy (`--specialize=no`), planning
// that direct call must materialize the call's hidden descriptor arguments and
// lower the app; running it must then produce the correct result
// (`h(Ok(3)) == 3`, so `main!` returns `Ok({})` and exits 0).
test "issue 11560: Boxy lowers and runs a call to an unannotated Try match" {
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11560: LSS lowers and runs the unannotated Try match with a payload descriptor" {
    try harness.runAppPathLoweredInspection(list_payload_app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// The payload-carrying `Try` form of the issue's unannotated match: Boxy must
// map the worker's hidden descriptor parameters onto the direct call's
// `Try(List(U64), U8)` representation, lower the app, and run it to the
// correct result (`h(Ok([1, 2])) == [1, 2]`, so `main!` returns `Ok({})`).
test "issue 11560: Boxy lowers and runs the unannotated Try match with a payload descriptor" {
    try harness.runAppPathLoweredInspection(list_payload_app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11560: LSS lowers and runs the unannotated Try match through an alias" {
    try harness.runAppPathLoweredInspection(alias_payload_app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// A call-site alias of `Try` wraps the nominal that wraps the tag union the
// callee names; Boxy sees through both to align the payload's descriptors.
test "issue 11560: Boxy lowers and runs the unannotated Try match through an alias" {
    try harness.runAppPathLoweredInspection(alias_payload_app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11560: LSS lowers and runs the unannotated match on a transparent nominal" {
    try harness.runAppPathLoweredInspection(user_nominal_app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// The callee's `==` evidence path names its tag union's `Ok` payload; Boxy
// follows it through the call's transparent nominal backing.
test "issue 11560: Boxy lowers and runs the unannotated match on a transparent nominal" {
    try harness.runAppPathLoweredInspection(user_nominal_app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11560: LSS lowers and runs the unannotated record match on an opaque nominal" {
    try harness.runAppPathLoweredInspection(opaque_record_app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// Inside its declaring module an opaque nominal is related to its backing
// record, so Boxy aligns the callee's record through that backing.
test "issue 11560: Boxy lowers and runs the unannotated record match on an opaque nominal" {
    try harness.runAppPathLoweredInspection(opaque_record_app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}

test "issue 11560: LSS lowers and runs the unannotated Try match through a record field" {
    try harness.runAppPathLoweredInspection(record_field_app_path, .{ .specialization_strategy = .lss }, expectAppRunsSuccessfully);
}

// The callee's record field is an open tag row where the call's is `Try`;
// Boxy aligns both its descriptor and dictionary parameters through `Try`.
test "issue 11560: Boxy lowers and runs the unannotated Try match through a record field" {
    try harness.runAppPathLoweredInspection(record_field_app_path, .{ .specialization_strategy = .boxy }, expectAppRunsSuccessfully);
}
