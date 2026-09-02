//! Regression tests for issue #10977.

const expectAppPathLowersToMonotype = @import("lower_to_lir_harness.zig").expectAppPathLowersToMonotype;

test "issue 10977: nominal lift with derived parser reaches Monotype" {
    // This is the valid counterpart to the missing-parser CLI repro. The
    // nominal opts into structural parser derivation, so finalizing the lifted
    // receiver must publish the matching generated-codec contract. Monotype
    // lowering used to panic because checking had frozen the contract for the
    // receiver's earlier structural shape.
    try expectAppPathLowersToMonotype(
        "test/postcheck/issue_10977_decoded_structural_tag_lifted_to_nominal/app_derived_parser.roc",
    );
}
