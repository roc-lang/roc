const std = @import("std");
const testing = std.testing;
const common_misspellings = @import("common_misspellings.zig");

test "CommonMisspellings integration - token tips" {
    // Test that common token misspellings return helpful tips
    const amp_amp_tip = common_misspellings.CommonMisspellings.getTokenTip("&&");
    try testing.expect(amp_amp_tip != null);
    try testing.expectEqualStrings(
        "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`.",
        amp_amp_tip.?,
    );

    const pipe_pipe_tip = common_misspellings.CommonMisspellings.getTokenTip("||");
    try testing.expect(pipe_pipe_tip != null);

    const bang_tip = common_misspellings.CommonMisspellings.getTokenTip("!");
    try testing.expect(bang_tip != null);
    try testing.expectEqualStrings(
        "Roc uses the keyword `not` instead of `!` for boolean negation.",
        bang_tip.?,
    );
}

test "CommonMisspellings integration - identifier tips" {
    // Test that common identifier misspellings return helpful tips
    const case_tip = common_misspellings.CommonMisspellings.getIdentifierTip("case");
    try testing.expect(case_tip != null);
    try testing.expectEqualStrings(
        "`case` is not a keyword in Roc. Use `when` for pattern matching.",
        case_tip.?,
    );

    const null_tip = common_misspellings.CommonMisspellings.getIdentifierTip("null");
    try testing.expect(null_tip != null);
    try testing.expectEqualStrings(
        "Roc doesn't have `null`. Use `Result` or custom tags like `[Some a, None]` to represent optional values.",
        null_tip.?,
    );

    const let_tip = common_misspellings.CommonMisspellings.getIdentifierTip("let");
    try testing.expect(let_tip != null);
    try testing.expectEqualStrings(
        "`let` is not needed in Roc. Use direct assignment like `name = value`.",
        let_tip.?,
    );

    // Test that various common type name mistakes are covered
    const int_tip = common_misspellings.CommonMisspellings.getIdentifierTip("int");
    try testing.expect(int_tip != null);

    const string_tip = common_misspellings.CommonMisspellings.getIdentifierTip("string");
    try testing.expect(string_tip != null);

    const array_tip = common_misspellings.CommonMisspellings.getIdentifierTip("array");
    try testing.expect(array_tip != null);
}

test "CommonMisspellings integration - no false positives" {
    // Ensure valid identifiers/tokens don't match
    try testing.expect(common_misspellings.CommonMisspellings.getTokenTip("and") == null);
    try testing.expect(common_misspellings.CommonMisspellings.getTokenTip("or") == null);
    try testing.expect(common_misspellings.CommonMisspellings.getTokenTip("+") == null);

    try testing.expect(common_misspellings.CommonMisspellings.getIdentifierTip("when") == null);
    try testing.expect(common_misspellings.CommonMisspellings.getIdentifierTip("Str") == null);
    try testing.expect(common_misspellings.CommonMisspellings.getIdentifierTip("List") == null);
    try testing.expect(common_misspellings.CommonMisspellings.getIdentifierTip("validIdentifier") == null);
}
