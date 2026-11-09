//! Regression test for https://github.com/roc-lang/roc/issues/12230.
//!
//! `Enc.encode : Form -> Try(Str, [Unencodable])` encodes a concrete record
//! with `Json.to_str`. Each caller supplies the record's encoder, since a
//! caller may pass a nominal with an encoder of its own. Plain-record callers
//! in different modules supply equal encoders, so they must share one
//! specialization of `Json.to_str` rather than get one per calling module.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const fixture_dir = "test/postcheck/issue_12230_codec_result_specialized_per_module/";

const LowerToLirHarnessError = harness.LowerToLirHarnessError;

fn countNamedProcs(store: *const lir.LirStore, expected_name: []const u8) usize {
    var count: usize = 0;
    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, expected_name)) count += 1;
    }
    return count;
}

fn expectOneEncoderSpecialization(lowered: *const lir.CheckedPipeline.LoweredProgram) LowerToLirHarnessError!void {
    const store = &lowered.lir_result.store;
    try std.testing.expectEqual(@as(usize, 1), countNamedProcs(store, "Builtin.Encoding.Json.to_str"));
}

fn expectPlainAndLiftedEncoderSpecializations(lowered: *const lir.CheckedPipeline.LoweredProgram) LowerToLirHarnessError!void {
    const store = &lowered.lir_result.store;
    try std.testing.expectEqual(@as(usize, 2), countNamedProcs(store, "Builtin.Encoding.Json.to_str"));
}

test "issue 12230: a function returning Ok(Json.to_str(record)) is specialized once across calling modules" {
    try harness.runAppPathLoweredInspection(
        fixture_dir ++ "app.roc",
        .{ .proc_debug_names = true },
        expectOneEncoderSpecialization,
    );
}

test "issue 12230: a caller passing a nominal with its own encoder keeps a separate specialization" {
    try harness.runAppPathLoweredInspection(
        fixture_dir ++ "lifted.roc",
        .{ .proc_debug_names = true },
        expectPlainAndLiftedEncoderSpecializations,
    );
}
