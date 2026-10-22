//! Derived `map`/`map!` on structural tag unions selects the payload checking
//! chose for the relation's callable class, whether the relation is checked
//! at its call or at an instantiation of a generic function, and the program
//! maps exactly that payload. `main!` exits nonzero when a value comes back
//! wrong.

const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const GuardedList = lir.LirStore.GuardedList;
const RunError = std.mem.Allocator.Error || eval.LirInterpreter.Error ||
    eval.RuntimeHostEnv.LeakError || error{ TestUnexpectedResult, TestExpectedEqual };

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

fn expectAppRuns(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    runApp(lowered) catch |err| {
        std.log.err("derived map app run failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
}

fn expectBodyRuns(body: []const u8) harness.LowerToLirHarnessError!void {
    try harness.runLoweredInspection(body, .{ .specialization_strategy = .lss }, expectAppRuns);
}

test "derived map selects the only payload at a direct call and at a generic function's instantiations" {
    try expectBodyRuns(
        \\wrap : Str -> [Found(Str), Missing]
        \\wrap = |s| if Str.is_empty(s) Missing else Found(s)
        \\
        \\shout = |w| w.map(|s| Str.concat(s, "!"))
        \\
        \\main! = |_args| {
        \\    direct = wrap("hi").map(|s| Str.concat(s, "?"))
        \\    generic = shout(wrap("yo"))
        \\    missing = shout(wrap(""))
        \\    out = match (direct, generic, missing) {
        \\        (Found(a), Found(b), Missing) => Str.concat(a, b)
        \\        _ => "wrong"
        \\    }
        \\    if out == "hi?yo!" { Ok({}) } else { Err(Exit(1)) }
        \\}
        \\
    );
}
