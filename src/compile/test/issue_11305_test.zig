//! Regression tests for dispatches whose receiver no edge pins, and for
//! generic functions stored in record fields crossing representation
//! boundaries.
const std = @import("std");
const base = @import("base");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestExpectedError, TestUnexpectedError };
const fixture_dir = "test/postcheck/issue_11305_derived_map_erased_field/";

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
        std.log.err("issue 11305 fixture run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn expectFixtureRuns(comptime file: []const u8, strategy: base.SpecializationStrategy) harness.LowerToLirHarnessError!void {
    try harness.runAppPathLoweredInspection(fixture_dir ++ file, .{ .specialization_strategy = strategy }, expectAppRunsSuccessfully);
}

test "issue 11305: Boxy never-instantiated map receiver in an annotated record field" {
    try expectFixtureRuns("app.roc", .boxy);
}

test "issue 11305: LSS never-instantiated map receiver in an annotated record field" {
    try expectFixtureRuns("app.roc", .lss);
}

test "issue 11305: Boxy never-instantiated map receiver in an inferred record field" {
    try expectFixtureRuns("unannotated.roc", .boxy);
}

test "issue 11305: LSS never-instantiated map receiver in an inferred record field" {
    try expectFixtureRuns("unannotated.roc", .lss);
}

test "issue 11305: Boxy never-instantiated method receiver in a record field" {
    try expectFixtureRuns("to_str.roc", .boxy);
}

test "issue 11305: LSS never-instantiated method receiver in a record field" {
    try expectFixtureRuns("to_str.roc", .lss);
}

test "issue 11305: Boxy generic record field function called at a concrete type" {
    try expectFixtureRuns("called_field.roc", .boxy);
}

test "issue 11305: LSS generic record field function called at a concrete type" {
    try expectFixtureRuns("called_field.roc", .lss);
}

test "issue 11305: Boxy generic record field function with a direct call called at a concrete type" {
    try expectFixtureRuns("called_field_direct.roc", .boxy);
}

test "issue 11305: LSS generic record field function with a direct call called at a concrete type" {
    try expectFixtureRuns("called_field_direct.roc", .lss);
}

test "issue 11305: Boxy generic record field function calling map called at a concrete type" {
    try expectFixtureRuns("called_field_map.roc", .boxy);
}

test "issue 11305: LSS generic record field function calling map called at a concrete type" {
    try expectFixtureRuns("called_field_map.roc", .lss);
}

test "issue 11305: Boxy top-level generic map function bound to a local and called" {
    try expectFixtureRuns("aliased_top_level.roc", .boxy);
}

test "issue 11305: LSS top-level generic map function bound to a local and called" {
    try expectFixtureRuns("aliased_top_level.roc", .lss);
}

test "issue 11305: Boxy generic record field function mapping with an identity callback called at a concrete type" {
    try expectFixtureRuns("called_field_map_identity.roc", .boxy);
}

test "issue 11305: LSS generic record field function mapping with an identity callback called at a concrete type" {
    try expectFixtureRuns("called_field_map_identity.roc", .lss);
}

test "issue 11305: Boxy top-level generic function mapping with a numeric callback" {
    try expectFixtureRuns("top_level_map_callback.roc", .boxy);
}

test "issue 11305: LSS top-level generic function mapping with a numeric callback" {
    try expectFixtureRuns("top_level_map_callback.roc", .lss);
}

test "issue 11305: Boxy top-level generic method function bound to a local and called" {
    try expectFixtureRuns("aliased_top_level_method.roc", .boxy);
}

test "issue 11305: LSS top-level generic method function bound to a local and called" {
    try expectFixtureRuns("aliased_top_level_method.roc", .lss);
}
