//! Regression test for issue #11158.

const expectLowersToLir = @import("lower_to_lir_harness.zig").expectLowersToLir;

test "issue 11158: derived parser for an empty record lowers to LIR" {
    // The parsed value's shape comes from the comparison, so the derived
    // parser's shape is an empty record: a record whose field set is empty
    // still parses, and its generated parser has no field name to rename.
    try expectLowersToLir(
        \\main! = |_args| {
        \\    _ = Json.parse("{}") == Ok({})
        \\    Ok({})
        \\}
    );
}
