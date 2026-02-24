//! Completion context detection module.
//!
//! This module provides pure text analysis functions to determine
//! what kind of completion is needed at a given cursor position.

const std = @import("std");

/// Completion context types
pub const CompletionContext = union(enum) {
    /// After a dot with a module prefix: "Str." or "List."
    after_module_dot: []const u8,
    /// After a dot with a value, either a record or a normal value for method completion : "myVal."
    after_value_dot: struct {
        /// The full dotted access chain before the dot (e.g., "myrec.subrec")
        access_chain: []const u8,
        /// Byte offset of the start of the chain
        chain_start: u32,
        /// Byte offset of the start of the segment before the dot
        member_start: u32,

        dot_offset: u32,
    },
    /// After a dot whose receiver is not a plain identifier (e.g., "func().")
    after_receiver_dot: struct {
        /// Byte offset of the dot character
        dot_offset: u32,
        /// The identifier chain before the parenthesized call, if any.
        /// For `Record2.SubVal("hi").` this is "Record2.SubVal".
        /// For `testFunc("hi").` this is "testFunc".
        call_chain: ?[]const u8 = null,
        /// Byte offset of the start of the call chain
        chain_start: u32 = 0,
    },
    /// After a colon (type annotation context)
    after_colon,
    /// General expression context
    expression,
};

/// Detect completion context from source text at the given position.
/// Analyzes the source text to determine what kind of completion is appropriate
/// based on what precedes the cursor position.
pub fn detectCompletionContext(source: []const u8, line: u32, character: u32) CompletionContext {
    const cursor_offset = computeOffset(source, line, character);

    if (cursor_offset == 0 or cursor_offset > source.len) {
        return .expression;
    }

    // Look backwards from cursor to find context
    var pos = cursor_offset;

    // Skip any partial identifier being typed
    while (pos > 0 and (std.ascii.isAlphanumeric(source[pos - 1]) or source[pos - 1] == '_')) {
        pos -= 1;
    }

    // Check for dot immediately (no whitespace skip). Roc dot access syntax
    // does not allow whitespace between receiver and dot (`record.field`, not
    // `record .field`), so we only recognize a dot that is directly adjacent.
    if (pos > 0 and source[pos - 1] == '.') {
        return detectDotContext(source, pos);
    }

    // For non-dot contexts, skip whitespace to find the context character.
    // This handles `x : Str` where there is a space between colon and type.
    while (pos > 0 and (source[pos - 1] == ' ' or source[pos - 1] == '\t')) {
        pos -= 1;
    }

    if (pos > 0 and source[pos - 1] == ':') {
        return .after_colon;
    }

    return .expression;
}

/// Detect the specific dot context once we know source[pos-1] == '.'.
fn detectDotContext(source: []const u8, pos: anytype) CompletionContext {
    // After a dot - could be module access or record field access
    // Look for identifier before the dot
    const ident_end = pos - 1;
    var ident_start = ident_end;

    while (ident_start > 0 and (std.ascii.isAlphanumeric(source[ident_start - 1]) or source[ident_start - 1] == '_')) {
        ident_start -= 1;
    }

    if (ident_start < ident_end) {
        const ident_name = source[ident_start..ident_end];
        if (ident_name.len > 0) {
            var chain_start = ident_start;
            while (chain_start > 0) {
                const c = source[chain_start - 1];
                if (std.ascii.isAlphanumeric(c) or c == '_' or c == '.') {
                    chain_start -= 1;
                } else {
                    break;
                }
            }

            const access_chain = source[chain_start..ident_end];

            if (std.ascii.isUpper(ident_name[0]) and std.mem.indexOfScalar(u8, access_chain, '.') == null) {
                // Uppercase without dots - module access (e.g., "Str.")
                return .{ .after_module_dot = ident_name };
            } else {
                // Lowercase or dotted chain - value/record field access (e.g., "myRecord.")
                return .{ .after_value_dot = .{
                    .access_chain = access_chain,
                    .chain_start = @intCast(chain_start),
                    .dot_offset = @intCast(pos - 1),
                    .member_start = @intCast(ident_start),
                } };
            }
        }
    }

    // No identifier before the dot; treat as a receiver dot (e.g., call chaining).
    // Try to extract the call chain by matching parentheses backwards.
    const dot_off: u32 = @intCast(pos - 1);
    var result: CompletionContext = .{ .after_receiver_dot = .{ .dot_offset = dot_off } };

    if (ident_end > 0 and source[ident_end - 1] == ')') {
        // Walk backwards to find the matching '('
        var paren_depth: u32 = 1;
        var scan = ident_end - 1; // position of ')'
        while (scan > 0) {
            scan -= 1;
            if (source[scan] == ')') {
                paren_depth += 1;
            } else if (source[scan] == '(') {
                paren_depth -= 1;
                if (paren_depth == 0) break;
            }
        }
        // scan now points to '(' (if matched)
        if (paren_depth == 0 and scan > 0) {
            // Extract the identifier chain before '('
            const chain_end = scan;
            var chain_start = chain_end;
            while (chain_start > 0) {
                const c = source[chain_start - 1];
                if (std.ascii.isAlphanumeric(c) or c == '_' or c == '.') {
                    chain_start -= 1;
                } else {
                    break;
                }
            }
            if (chain_start < chain_end) {
                result.after_receiver_dot.call_chain = source[chain_start..chain_end];
                result.after_receiver_dot.chain_start = @intCast(chain_start);
            }
        }
    }

    return result;
}

/// Compute byte offset from line and character position in source text.
/// Converts LSP-style line/character coordinates to a byte offset.
pub fn computeOffset(source: []const u8, line: u32, character: u32) u32 {
    var current_line: u32 = 0;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (current_line == line) {
            line_start = i;
            break;
        }
        if (c == '\n') {
            current_line += 1;
        }
    }

    const offset = line_start + character;
    return @intCast(@min(offset, source.len));
}

// Tests

test "detectCompletionContext: after module dot" {
    const source = "Str.";
    const ctx = detectCompletionContext(source, 0, 4);
    try std.testing.expect(ctx == .after_module_dot);
    try std.testing.expectEqualStrings("Str", ctx.after_module_dot);
}

test "detectCompletionContext: after record dot" {
    const source = "myRecord.";
    const ctx = detectCompletionContext(source, 0, 9);
    try std.testing.expect(ctx == .after_value_dot);
    try std.testing.expectEqualStrings("myRecord", ctx.after_value_dot.access_chain);
}

test "detectCompletionContext: after nested record dot" {
    const source = "myrec.subrec.";
    const ctx = detectCompletionContext(source, 0, 13);
    try std.testing.expect(ctx == .after_value_dot);
    try std.testing.expectEqualStrings("myrec.subrec", ctx.after_value_dot.access_chain);
    try std.testing.expectEqual(@as(u32, 0), ctx.after_value_dot.chain_start);
    try std.testing.expectEqual(@as(u32, 6), ctx.after_value_dot.member_start);
}

test "detectCompletionContext: after receiver dot" {
    const source = "val.func().";
    const ctx = detectCompletionContext(source, 0, 11);
    try std.testing.expect(ctx == .after_receiver_dot);
    try std.testing.expectEqual(@as(u32, 10), ctx.after_receiver_dot.dot_offset);
    try std.testing.expectEqualStrings("val.func", ctx.after_receiver_dot.call_chain.?);
    try std.testing.expectEqual(@as(u32, 0), ctx.after_receiver_dot.chain_start);
}

test "detectCompletionContext: after receiver dot with nominal constructor" {
    const source = "Record2.SubVal(\"hi\").";
    const ctx = detectCompletionContext(source, 0, 21);
    try std.testing.expect(ctx == .after_receiver_dot);
    try std.testing.expectEqual(@as(u32, 20), ctx.after_receiver_dot.dot_offset);
    try std.testing.expectEqualStrings("Record2.SubVal", ctx.after_receiver_dot.call_chain.?);
    try std.testing.expectEqual(@as(u32, 0), ctx.after_receiver_dot.chain_start);
}

test "detectCompletionContext: after receiver dot with simple function" {
    const source = "testFunc(\"hi\").";
    const ctx = detectCompletionContext(source, 0, 15);
    try std.testing.expect(ctx == .after_receiver_dot);
    try std.testing.expectEqual(@as(u32, 14), ctx.after_receiver_dot.dot_offset);
    try std.testing.expectEqualStrings("testFunc", ctx.after_receiver_dot.call_chain.?);
    try std.testing.expectEqual(@as(u32, 0), ctx.after_receiver_dot.chain_start);
}

test "detectCompletionContext: after colon" {
    const source = "x :";
    const ctx = detectCompletionContext(source, 0, 3);
    try std.testing.expectEqual(CompletionContext.after_colon, ctx);
}

test "detectCompletionContext: expression context" {
    const source = "x = ";
    const ctx = detectCompletionContext(source, 0, 4);
    try std.testing.expectEqual(CompletionContext.expression, ctx);
}

test "computeOffset: first line" {
    const source = "hello world";
    const offset = computeOffset(source, 0, 6);
    try std.testing.expectEqual(@as(u32, 6), offset);
}

test "computeOffset: second line" {
    const source = "hello\nworld";
    const offset = computeOffset(source, 1, 3);
    try std.testing.expectEqual(@as(u32, 9), offset); // 6 (after newline) + 3
}

test "computeOffset: clamps to source length" {
    const source = "hi";
    const offset = computeOffset(source, 0, 100);
    try std.testing.expectEqual(@as(u32, 2), offset);
}
