//! Specialized callable execution has no descriptor runtime or side tables.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const source =
    \\make : I64 -> (I64 -> I64)
    \\make = |n| |x| n + x
    \\countdown : I64 -> I64
    \\countdown = |n| if n == 0 42 else Box.unbox(Box.box(countdown))(n - 1)
    \\main! = |args| {
    \\    n = List.len(args).to_i64_wrap() + 7
    \\    make_again = Box.unbox(Box.box(make))
    \\    add = Box.unbox(Box.box(make_again(n)))
    \\    if add(1) == 8 and countdown(10000 + n) == 42 { Ok({}) } else { Err(Exit(1)) }
    \\}
;

fn inspect(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const result = &lowered.lir_result;
    try std.testing.expectEqual(.specialized, result.store.runtime_contract);
    var erased_workers: usize = 0;
    for (result.store.getProcSpecs()) |proc| {
        if (proc.abi == .erased_callable) erased_workers += 1;
    }
    try std.testing.expect(erased_workers != 0);
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    var static_data = try harness.interpreterStaticData(lowered);
    defer if (static_data) |*data| data.deinit();
    var strings = try eval.LirInterpreter.buildStaticStrings(std.testing.allocator, &result.store);
    defer strings.deinit();
    var interpreter = try eval.LirInterpreter.init(std.testing.allocator, &result.store, &result.layouts, strings.view(), host.get_ops());
    defer interpreter.deinit();
    if (static_data) |*data| {
        data.install(&interpreter);
        data.ownByInterpreter(&interpreter);
    }
    const root_id = result.root_procs.items[0];
    const root = result.store.getProcSpec(root_id);
    const args = result.store.getLocalSpan(root.args);
    const arg_layout = result.store.getLocal(lir.LirStore.GuardedList.at(args, 0)).layout_idx;
    var empty_args = [_]usize{ 0, 0, 0 };
    var exit_code: i8 = -1;
    _ = interpreter.eval(.{ .proc_id = root_id, .arg_layouts = &.{arg_layout}, .ret_layout = root.ret_layout, .arg_ptr = @ptrCast(&empty_args), .ret_ptr = @ptrCast(&exit_code) }) catch return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(i8, 0), exit_code);
    host.checkForLeaks() catch return error.TestUnexpectedResult;
}

test "issue 12569: specialized captured returned and recursive callables need no Boxy work" {
    try harness.runLoweredInspection(source, .{}, inspect);
}
