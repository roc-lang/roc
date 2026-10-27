//! `.boxy` lowers one generated codec constructor worker per checked
//! structural codec derivation, whichever path reaches it: a direct structural
//! `parser_for`/`encoder_for` call or structural callable-dictionary evidence
//! (`structuralCodecWorkerSource`).

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
    _ = source;
    _ = harness;
    // `.boxy` must lower one codec constructor worker per derivation.
    return error.BoxyLoweringBeingReimplemented;
}
