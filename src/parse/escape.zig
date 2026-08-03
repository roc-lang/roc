//! Single source of truth for the string/char escape alphabet.
//!
//! The escape bytes (`\n \r \t \\ \" \' \$` and the `\u(...)` marker) are
//! validated by the tokenizer and interpreted by the canonicalizer for both
//! char literals and string literals. Encoding the alphabet once here lets the
//! tokenizer accept exactly the domain the interpreters understand, so a new
//! escape is a one-row change and the three sites cannot disagree.

const std = @import("std");

/// What an escape byte means once the leading backslash has been consumed.
pub const Interpretation = union(enum) {
    /// A simple escape that produces exactly one byte (e.g. `\n` -> 0x0A).
    byte: u8,
    /// The `\u(...)` marker: the following `(hex)` names a Unicode codepoint.
    /// The surrounding code owns parsing and encoding the codepoint.
    unicode,
};

/// Map an escape byte (the byte following a backslash) to its interpretation,
/// or `null` if the byte does not name a valid escape. The set of bytes for
/// which this returns non-`null` is the escape alphabet's domain.
pub fn lookup(escape_byte: u8) ?Interpretation {
    return switch (escape_byte) {
        'n' => .{ .byte = '\n' },
        'r' => .{ .byte = '\r' },
        't' => .{ .byte = '\t' },
        '\\' => .{ .byte = '\\' },
        '"' => .{ .byte = '"' },
        '\'' => .{ .byte = '\'' },
        '$' => .{ .byte = '$' },
        'u' => .unicode,
        else => null,
    };
}

test "escape table domain is interpreted consistently" {
    // Every byte in the domain has a well-formed interpretation, and a
    // representative non-member byte is rejected.
    try std.testing.expectEqual(@as(u8, '\n'), lookup('n').?.byte);
    try std.testing.expectEqual(@as(u8, '\r'), lookup('r').?.byte);
    try std.testing.expectEqual(@as(u8, '\t'), lookup('t').?.byte);
    try std.testing.expectEqual(@as(u8, '\\'), lookup('\\').?.byte);
    try std.testing.expectEqual(@as(u8, '"'), lookup('"').?.byte);
    try std.testing.expectEqual(@as(u8, '\''), lookup('\'').?.byte);
    try std.testing.expectEqual(@as(u8, '$'), lookup('$').?.byte);
    try std.testing.expect(lookup('u').? == .unicode);
    try std.testing.expect(lookup('x') == null);
    try std.testing.expect(lookup('0') == null);
    try std.testing.expect(lookup('a') == null);
}
