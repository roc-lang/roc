//! Regression test for https://github.com/roc-lang/roc/issues/13009.
//!
//! The derived `to_hash` of a record of tag unions feeds the hasher the
//! writes its specification orders: fields in record order, and for each tag
//! union its variant index then its payloads. The fixture returns the
//! derived hash beside the same writes made by hand in that order and in a
//! reordered one, from one starting hasher state, and the test runs it in
//! the LIR interpreter and compares the final states.
const std = @import("std");
const eval = @import("eval");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const fixture = "test/postcheck/issue_13009_derived_hash_write_order/app.roc";

const Hashes = extern struct {
    derived: u64,
    manual: u64,
    reordered: u64,
};

fn probe(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const hashes = runProbe(lowered) catch |err| {
        std.log.err("issue 13009 hash probe failed: {s}", .{@errorName(err)});
        return error.TestUnexpectedResult;
    };
    std.testing.expectEqual(hashes.manual, hashes.derived) catch return error.TestUnexpectedResult;
    std.testing.expect(hashes.reordered != hashes.derived) catch return error.TestUnexpectedResult;
}

fn runProbe(lowered: *const lir.CheckedPipeline.LoweredProgram) (eval.LirInterpreter.Error || eval.RuntimeHostEnv.LeakError)!Hashes {
    var host = eval.RuntimeHostEnv.init(std.testing.allocator);
    defer host.deinit();
    const program = &lowered.lir_result;
    var static_data = try harness.interpreterStaticData(lowered);
    defer if (static_data) |*data| data.deinit();
    var static_strings = try eval.LirInterpreter.buildStaticStrings(std.testing.allocator, &program.store);
    defer static_strings.deinit();
    var interpreter = try eval.LirInterpreter.init(
        std.testing.allocator,
        &program.store,
        &program.layouts,
        static_strings.view(),
        host.get_ops(),
    );
    defer interpreter.deinit();
    if (static_data) |*data| {
        data.install(&interpreter);
        data.ownByInterpreter(&interpreter);
    }
    const root_id = program.root_procs.items[0];
    const root = program.store.getProcSpec(root_id);
    const args = program.store.getLocalSpan(root.args);
    const arg_layout = program.store.getLocal(lir.LirStore.GuardedList.at(args, 0)).layout_idx;
    var seed: u64 = 0x5eed_1300_9000_0001;
    var hashes: Hashes = undefined;
    _ = try interpreter.eval(.{
        .proc_id = root_id,
        .arg_layouts = &.{arg_layout},
        .ret_layout = root.ret_layout,
        .arg_ptr = @ptrCast(&seed),
        .ret_ptr = @ptrCast(&hashes),
    });
    try host.checkForLeaks();
    return hashes;
}

test "issue 13009: derived to_hash of a record of tag unions writes fields in order, each variant index before its payloads" {
    try harness.runAppPathLoweredInspection(fixture, .{}, probe);
}
