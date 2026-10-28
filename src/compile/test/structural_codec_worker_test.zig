//! `.boxy` lowers one generated codec constructor worker per checked
//! structural codec derivation, whichever path reaches it: a direct structural
//! `parser_for`/`encoder_for` call or structural callable-dictionary evidence
//! (`structuralCodecWorkerSource`).

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

test "structural codec: one derivation reached by dictionary evidence at several uses plans one worker" {
    const source =
        \\to_json = |a, b| Json.to_str({ a, b })
        \\forward = |a, b| to_json(a, b)
        \\main! = |args| {
        \\    echo!(forward(args, {}))
        \\    echo!(forward(True, args))
        \\    echo!(to_json(args, {}))
        \\    Ok({})
        \\}
    ;
    // `.boxy` lowers one codec constructor worker per derivation, however
    // many uses reach it: the two uses at `{ a : List(Str), b : {} }` share
    // one, and the use at `{ a : Bool, b : List(Str) }` has its own.
    try harness.runLoweredInspection(source, .{
        .specialization_strategy = .boxy,
        .proc_debug_names = true,
    }, expectTwoCodecConstructors);
}

/// Exactly one generated codec constructor worker per record encoder
/// derivation.
fn expectTwoCodecConstructors(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    var constructors: usize = 0;
    const store = &lowered.lir_result.store;
    for (0..store.getProcSpecs().len) |index| {
        const id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(index));
        const name = store.procDebugName(id) orelse continue;
        if (std.mem.eql(u8, name, "boxy.codec.constructor")) constructors += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), constructors);
}
