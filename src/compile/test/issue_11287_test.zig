//! Regression test for issue #11287.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const eval = @import("eval");
const builtins = @import("builtins");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const HostAbiTestError = std.mem.Allocator.Error || eval.LirInterpreter.Error || eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };
const app_path = "test/postcheck/issue_11287_hosted_try_host_abi_layout/app.roc";
const hosted_symbol = "roc_stdout_report";

fn expectHostedArgKeepsCheckedHostAbiLayout(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var found: usize = 0;
    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const spec = store.getProcSpec(proc_id);
        const hosted = spec.hosted orelse continue;
        if (!std.mem.eql(u8, store.getString(hosted.symbol), hosted_symbol)) continue;
        found += 1;
        const args = store.getLocalSpan(spec.args);
        try std.testing.expectEqual(@as(usize, 1), args.len);
        const arg_layout = store.getLocal(GuardedList.at(args, 0)).layout_idx;
        const arg = layouts.getLayout(arg_layout);
        try std.testing.expectEqual(layout.LayoutTag.tag_union, arg.tag);
        const info = layouts.getTagUnionInfo(arg);
        try std.testing.expectEqual(@as(u32, 32), info.byte_size);
        try std.testing.expectEqual(@as(u16, 24), info.discriminant_offset);
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "issue 11287: LSS hosted proc keeps the checked host ABI layout" {
    try harness.runAppPathLirInspection(app_path, .{ .specialization_strategy = .lss, .target_usize = .u64 }, expectHostedArgKeepsCheckedHostAbiLayout);
}

test "issue 11287: Boxy hosted proc keeps the checked host ABI layout" {
    try harness.runAppPathLirInspection(app_path, .{ .specialization_strategy = .boxy, .target_usize = .u64 }, expectHostedArgKeepsCheckedHostAbiLayout);
}

const return_app_path = "test/postcheck/issue_11287_hosted_try_return/app.roc";

fn expectReturnAbi(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const result = &lowered.lir_result;
    var found: usize = 0;
    for (result.store.getProcSpecs(), 0..) |_, index| {
        const spec = result.store.getProcSpec(@enumFromInt(@as(u32, @intCast(index))));
        if (spec.hosted) |hosted| {
            if (!std.mem.eql(u8, result.store.getString(hosted.symbol), "roc_stdout_line")) continue;
            const value = result.layouts.getLayout(spec.ret_layout);
            try std.testing.expectEqual(layout.LayoutTag.tag_union, value.tag);
            const info = result.layouts.getTagUnionInfo(value);
            try std.testing.expectEqual(@as(u32, 32), info.byte_size);
            try std.testing.expectEqual(@as(u16, 24), info.discriminant_offset);
            found += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), found);
    try std.testing.expectEqual(@as(usize, 1), result.root_procs.items.len);
    const root = result.store.getProcSpec(result.root_procs.items[0]);
    const value = result.layouts.getLayout(root.ret_layout);
    try std.testing.expectEqual(layout.LayoutTag.tag_union, value.tag);
    const info = result.layouts.getTagUnionInfo(value);
    try std.testing.expectEqual(@as(u32, 32), info.byte_size);
    try std.testing.expectEqual(@as(u16, 24), info.discriminant_offset);
}

test "issue 11287: LSS hosted and exported results keep the checked host ABI" {
    try harness.runAppPathLoweredInspection(return_app_path, .{ .specialization_strategy = .lss, .target_usize = .u64 }, expectReturnAbi);
}

test "issue 11287: Boxy hosted and exported results keep the checked host ABI" {
    try harness.runAppPathLoweredInspection(return_app_path, .{ .specialization_strategy = .boxy, .target_usize = .u64 }, expectReturnAbi);
}

const HostResult = extern struct {
    payload: builtins.str.RocStr,
    tag: u8,
};
const error_message = "an allocated stdout error payload crossing both ABI adapters";
var return_error: bool = false;
var host_call_count: usize = 0;
var test_host_ops: *builtins.host_abi.RocOps = undefined;

fn stdoutLine(text: builtins.str.RocStr) callconv(.c) HostResult {
    const ops = test_host_ops;
    std.debug.assert(std.mem.eql(u8, text.asSlice(), "hello"));
    text.decref(ops);
    host_call_count += 1;
    return .{
        .payload = if (return_error) builtins.str.RocStr.fromSlice(error_message, ops) else std.mem.zeroes(builtins.str.RocStr),
        .tag = if (return_error) 0 else 1,
    };
}

fn runHostReturnRoundtrip(lowered: *const lir.CheckedPipeline.LoweredProgram) HostAbiTestError!void {
    comptime {
        std.debug.assert(@sizeOf(HostResult) == 32);
        std.debug.assert(@offsetOf(HostResult, "tag") == 24);
    }
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    var hosted_fns = [_]builtins.host_abi.HostedFn{builtins.host_abi.hostedFn(&stdoutLine)};
    const ops = host.get_ops();
    test_host_ops = ops;
    ops.hosted_fns = .{ .count = 1, .fns = &hosted_fns };
    const program = &lowered.lir_result;
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(std.testing.allocator, &program.store, &program.layouts, eval.LirInterpreter.BoxyTables.fromResult(program), ops, .preserve);
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    host_call_count = 0;
    for ([_]bool{ false, true }) |is_error| {
        return_error = is_error;
        var result: HostResult = undefined;
        _ = try interpreter.eval(.{ .proc_id = root_id, .arg_layouts = &.{arg_layout}, .ret_layout = root.ret_layout, .arg_ptr = @ptrCast(&empty_args), .ret_ptr = @ptrCast(&result) });
        try std.testing.expectEqual(@as(u8, if (is_error) 0 else 1), result.tag);
        if (is_error) {
            defer result.payload.decref(ops);
            try std.testing.expectEqualStrings(error_message, result.payload.asSlice());
        }
        try host.checkForLeaks();
    }
    try std.testing.expectEqual(@as(usize, 2), host_call_count);
}

fn expectHostReturnRoundtrip(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    try expectReturnAbi(lowered);
    runHostReturnRoundtrip(lowered) catch |err| {
        std.log.err("hosted ABI roundtrip failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11287: Boxy executes both hosted result tags and preserves owned payloads" {
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    try harness.runAppPathLoweredInspection(return_app_path, .{ .specialization_strategy = .boxy, .target_usize = .u64 }, expectHostReturnRoundtrip);
}

test "issue 11287: LSS executes both hosted result tags and preserves owned payloads" {
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    try harness.runAppPathLoweredInspection(return_app_path, .{ .specialization_strategy = .lss, .target_usize = .u64 }, expectHostReturnRoundtrip);
}

var reported_expected_value: bool = false;

fn stdoutReport(result: HostResult) callconv(.c) void {
    std.debug.assert(result.tag == 0);
    reported_expected_value = std.mem.eql(u8, result.payload.asSlice(), "boom");
    result.payload.decref(test_host_ops);
}

fn runHostArgument(lowered: *const lir.CheckedPipeline.LoweredProgram) HostAbiTestError!void {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    test_host_ops = host.get_ops();
    var hosted_fns = [_]builtins.host_abi.HostedFn{builtins.host_abi.hostedFn(&stdoutReport)};
    test_host_ops.hosted_fns = .{ .count = 1, .fns = &hosted_fns };
    const program = &lowered.lir_result;
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(std.testing.allocator, &program.store, &program.layouts, eval.LirInterpreter.BoxyTables.fromResult(program), test_host_ops, .preserve);
    defer interpreter.deinit();
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    var result: i32 = -1;
    reported_expected_value = false;
    _ = try interpreter.eval(.{ .proc_id = root_id, .arg_layouts = &.{arg_layout}, .ret_layout = root.ret_layout, .arg_ptr = @ptrCast(&empty_args), .ret_ptr = @ptrCast(&result) });
    try std.testing.expect(reported_expected_value);
    try std.testing.expectEqual(@as(i32, 0), result);
    try host.checkForLeaks();
}

fn expectHostArgument(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runHostArgument(lowered) catch |err| {
        std.log.err("hosted ABI argument failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

test "issue 11287: Boxy passes the exact owned argument to the host" {
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .boxy, .target_usize = .u64 }, expectHostArgument);
}

test "issue 11287: LSS passes the exact owned argument to the host" {
    if (@sizeOf(usize) != 8) return error.SkipZigTest;
    try harness.runAppPathLoweredInspection(app_path, .{ .specialization_strategy = .lss, .target_usize = .u64 }, expectHostArgument);
}

fn expectHostedArg32(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var found: usize = 0;
    for (store.getProcSpecs(), 0..) |_, index| {
        const spec = store.getProcSpec(@enumFromInt(@as(u32, @intCast(index))));
        const hosted = spec.hosted orelse continue;
        if (!std.mem.eql(u8, store.getString(hosted.symbol), hosted_symbol)) continue;
        const args = store.getLocalSpan(spec.args);
        const value = layouts.getLayout(store.getLocal(GuardedList.at(args, 0)).layout_idx);
        try std.testing.expectEqual(layout.LayoutTag.tag_union, value.tag);
        const info = layouts.getTagUnionInfo(value);
        try std.testing.expectEqual(@as(u32, 16), info.byte_size);
        try std.testing.expectEqual(@as(u16, 12), info.discriminant_offset);
        found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "issue 11287: Boxy retains the 32-bit hosted ABI" {
    try harness.runAppPathLirInspection(app_path, .{ .specialization_strategy = .boxy, .target_usize = .u32 }, expectHostedArg32);
}

test "issue 11287: LSS retains the 32-bit hosted ABI" {
    try harness.runAppPathLirInspection(app_path, .{ .specialization_strategy = .lss, .target_usize = .u32 }, expectHostedArg32);
}
