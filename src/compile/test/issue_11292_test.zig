//! Regression tests for generalized literal conversion and equality guards.
const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual, TestUnexpectedError };
const app_path = "test/postcheck/issue_11292_generalized_string_pattern/app.roc";

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
    const result = interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&exit_code),
    });
    _ = try result;
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    try host.checkForLeaks();
}

fn expectAppRunsSuccessfully(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("generalized string pattern run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

/// Checks the app the way `roc check` does, with no runtime lowering
/// configured, and requires exactly one report of each expected title.
fn expectCheckReports(fixture: []const u8, expected_titles: []const []const u8) (harness.LowerToLirHarnessError || error{SkipZigTest})!void {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        allocator,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = fixture });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());
    try coord.finishCheckedProgram(.executable_artifacts);

    for (expected_titles) |title| {
        var count: usize = 0;
        var reports = coord.iterReports();
        while (reports.next()) |entry| {
            if (std.mem.eql(u8, entry.report.title, title)) count += 1;
        }
        try std.testing.expectEqual(@as(usize, 1), count);
    }
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

// Every pattern literal's conversion is hoisted, and hoisting is eager: a
// conversion that fails is a compile-time error even when matching would
// never reach its branch at runtime.
test "issue 11292: checking reports a rejected pattern literal conversion" {
    try expectCheckReports(
        "test/postcheck/issue_11292_generalized_string_pattern/rejected.roc",
        &.{"Invalid String"},
    );
}

test "issue 11292: checking reports a crashing pattern literal conversion that matching never reaches" {
    try expectCheckReports(
        "test/postcheck/issue_11292_generalized_string_pattern/unreached.roc",
        &.{"Compile Time Crash"},
    );
}
