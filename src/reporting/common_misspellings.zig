//! Maps of common syntax mistakes and misspellings to helpful error messages.

const std = @import("std");

/// A static map of common misspellings and mistakes in Roc code.
/// Keys are the incorrect tokens that users might write.
/// Values are helpful tip messages to guide users toward the correct Roc syntax.
pub const CommonMisspellings = struct {
    /// Token misspellings - mistakes that appear during tokenization
    pub const tokens = std.StaticStringMap([]const u8).initComptime(.{
        .{ "&&", "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`." },
        .{ "||", "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`." },
        .{ "\\", "Roc syntax does not use single backslashes. Roc lambda syntax is `|arg1, arg2| body`, and double backslash (`\\\\`) begins a line in a multiline string." },
        .{ "/=", "Roc uses `!=` for inequality, not `/=`." },
        .{ "===", "Roc uses `==` for equality comparison, not `===`." },
        .{ "!==", "Roc uses `!=` for inequality comparison, not `!==`." },
        .{ "++", "Roc doesn't have a `++` operator. Use `x + 1` for incrementing, or `List.concat` for concatenating lists." },
        .{ "--", "Roc doesn't have a `--` operator. Use `x - 1` for decrementing." },
    });

    /// Look up a token misspelling and return the associated tip, if any.
    pub fn getTokenTip(token: []const u8) ?[]const u8 {
        return tokens.get(token);
    }
};

test "token misspellings lookup" {
    const tip = CommonMisspellings.getTokenTip("&&");
    try std.testing.expect(tip != null);
    try std.testing.expectEqualStrings(
        "Roc uses the keyword `and` instead of `&&`, and the keyword `or` instead of `||`.",
        tip.?,
    );
}

test "non-existent misspelling returns null" {
    try std.testing.expect(CommonMisspellings.getTokenTip("validtoken") == null);
}
