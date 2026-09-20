//! Regression test for issue #11393: imported method dispatch must retain
//! the codec requirements captured by the method's complete scheme.

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const HostRunError = std.mem.Allocator.Error ||
    eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError ||
    error{ TestUnexpectedResult, TestExpectedEqual };

const app_path = "test/postcheck/issue_11393_nominal_method_open_error_json/app.roc";
const expected_line = "\"about:blank\"";

var host_ops: *builtins.host_abi.RocOps = undefined;
var printed_buf: [64]u8 = undefined;
var printed_len: usize = 0;
var host_call_count: usize = 0;

fn stdoutLine(text: builtins.str.RocStr) callconv(.c) void {
    var owned = text;
    defer owned.decref(host_ops);
    const message = owned.asSlice();
    printed_len = @min(message.len, printed_buf.len);
    @memcpy(printed_buf[0..printed_len], message[0..printed_len]);
    host_call_count += 1;
}

fn runNavigate(lowered: *const lir.CheckedPipeline.LoweredProgram) HostRunError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    host_ops = host.get_ops();
    var hosted_fns = [_]builtins.host_abi.HostedFn{builtins.host_abi.hostedFn(&stdoutLine)};
    host_ops.hosted_fns = .{ .count = 1, .fns = &hosted_fns };

    const program = &lowered.lir_result;
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        eval.LirInterpreter.BoxyTables.fromResult(program),
        host_ops,
    );
    defer interpreter.deinit();

    try std.testing.expectEqual(@as(usize, 1), program.root_procs.items.len);
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;

    // A zeroed RocList is the empty args list, selecting the fallback URL.
    var empty_args = [_]usize{ 0, 0, 0 };
    var result: i32 = -1;
    host_call_count = 0;
    printed_len = 0;
    _ = try interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&empty_args),
        .ret_ptr = @ptrCast(&result),
    });

    try std.testing.expectEqual(@as(usize, 1), host_call_count);
    try std.testing.expectEqualStrings(expected_line, printed_buf[0..printed_len]);
    try std.testing.expectEqual(@as(i32, 0), result);
    try host.checkForLeaks();
}

fn expectNavigateReachesHost(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runNavigate(lowered) catch |err| {
        std.log.err("running the nominal's method against the host failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11393: a nominal method returning an open error union around Json.to_str runs" {
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    try harness.runAppPathLoweredInspection(
        app_path,
        .{ .specialization_strategy = .lss, .target_usize = .u64 },
        expectNavigateReachesHost,
    );
}
