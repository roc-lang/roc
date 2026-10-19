//! The `unreachable` rationale lint: every `unreachable` keyword in Zig
//! source carries a same-line `//` comment naming the invariant that makes
//! the case impossible.
//!
//! Candidates come from `std.zig.Tokenizer`, so `unreachable` inside string,
//! char, and multiline-string literals, inside comments, and in `@"unreachable"`
//! identifiers is never a candidate. A candidate's rationale is the `//`
//! comment ending its physical line, and it counts only when nothing but
//! closing delimiters, `;`, and `,` stands between the `unreachable` and the
//! comment; otherwise the comment could be about the rest of the line.

const std = @import("std");

/// Why a candidate `unreachable` is not compliant.
pub const Problem = enum {
    /// No `//` comment ends the line.
    missing_rationale,
    /// The comment is empty or only restates that the case cannot happen.
    restatement,
    /// Code other than closing delimiters, `;`, and `,` follows the
    /// `unreachable` on its line, so a trailing comment cannot be attributed
    /// to it.
    code_after,
};

/// One non-compliant `unreachable`.
pub const Violation = struct {
    /// 1-based physical line of the `unreachable` token.
    line: u32,
    problem: Problem,
};

/// The tokens that may stand between an `unreachable` and its rationale.
const closing_tags = [_]std.zig.Token.Tag{ .r_paren, .r_brace, .r_bracket, .semicolon, .comma };

/// Comment texts that state no invariant. Compared case-insensitively after
/// trimming whitespace and trailing punctuation.
const banned_rationales = [_][]const u8{
    "",
    "unreachable",
    "impossible",
    "can't happen",
    "cannot happen",
    "can not happen",
    "should not happen",
    "shouldn't happen",
    "should never happen",
    "never happens",
    "not reachable",
    "not possible",
    "never",
    "todo",
};

/// Appends a `Violation` for each non-compliant `unreachable` in `source`.
pub fn findViolations(
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    violations: *std.ArrayList(Violation),
) !void {
    var tokenizer = std.zig.Tokenizer.init(source);
    var line: u32 = 1;
    var line_counted_to: usize = 0;
    // The token after the current one, so a candidate can look ahead.
    var token = tokenizer.next();
    while (token.tag != .eof) {
        const next = tokenizer.next();
        if (token.tag == .keyword_unreachable) {
            line += @intCast(std.mem.countScalar(u8, source[line_counted_to..token.loc.start], '\n'));
            line_counted_to = token.loc.start;
            if (check(source, token, next, &tokenizer)) |problem| {
                try violations.append(allocator, .{ .line = line, .problem = problem });
            }
        }
        token = next;
    }
}

/// The problem with the candidate `unreachable` token, if any. `following`
/// is the token after it; `tokenizer` is positioned after `following` and is
/// only copied, never advanced.
fn check(source: [:0]const u8, candidate: std.zig.Token, following: std.zig.Token, tokenizer: *const std.zig.Tokenizer) ?Problem {
    const line_end = std.mem.findScalarPos(u8, source, candidate.loc.end, '\n') orelse source.len;
    var lookahead = tokenizer.*;
    var tail_start = candidate.loc.end;
    var next = following;
    while (next.tag != .eof and next.loc.start < line_end) : (next = lookahead.next()) {
        if (std.mem.findScalar(std.zig.Token.Tag, &closing_tags, next.tag) == null) return .code_after;
        tail_start = next.loc.end;
    }
    const tail = source[tail_start..line_end];
    const comment_start = std.mem.find(u8, tail, "//") orelse return .missing_rationale;
    if (std.mem.trim(u8, tail[0..comment_start], " \t\r").len != 0) return .missing_rationale;
    const reason = std.mem.trim(u8, tail[comment_start + 2 ..], " \t\r.!/");
    for (banned_rationales) |banned| {
        if (std.ascii.eqlIgnoreCase(reason, banned)) return .restatement;
    }
    return null;
}

fn expectViolations(source: [:0]const u8, expected: []const Violation) !void {
    var violations: std.ArrayList(Violation) = .empty;
    defer violations.deinit(std.testing.allocator);
    try findViolations(std.testing.allocator, source, &violations);
    try std.testing.expectEqualSlices(Violation, expected, violations.items);
}

test "every compliant form passes" {
    try expectViolations(
        \\fn f(x: ?u8, y: error{Failed}!u8, e: enum { a, b, c }) u8 {
        \\    switch (e) {
        \\        .a => {},
        \\        .b, .c => unreachable, // callers pass only .a
        \\    }
        \\    if (x == null) unreachable; // checked by the caller
        \\    const a = x orelse unreachable; // checked above
        \\    const b = y catch unreachable; // y is never an error here
        \\    const c = if (a > b) a else unreachable; // a > b by construction
        \\    const d = (x orelse unreachable); // checked above
        \\    _ = .{ c, d };
        \\    unreachable; // every path above returns
        \\}
    , &.{});
}

test "bare and mis-attributed forms are flagged" {
    try expectViolations(
        \\fn f(x: ?u8, y: error{Failed}!u8, e: enum { a, b }) u8 {
        \\    switch (e) {
        \\        .a => {},
        \\        .b => unreachable,
        \\    }
        \\    const a = x orelse unreachable;
        \\    const b = y catch unreachable;
        \\    const c = (x orelse unreachable) + 1; // c is the successor of a
        \\    const d = x orelse unreachable; //
        \\    const g = x orelse unreachable; // Impossible.
        \\    const h = if (a > b) a else unreachable; // can't happen
        \\    _ = .{ c, d, g, h };
        \\    unreachable;
        \\}
    , &.{
        .{ .line = 4, .problem = .missing_rationale },
        .{ .line = 6, .problem = .missing_rationale },
        .{ .line = 7, .problem = .missing_rationale },
        .{ .line = 8, .problem = .code_after },
        .{ .line = 9, .problem = .restatement },
        .{ .line = 10, .problem = .restatement },
        .{ .line = 11, .problem = .restatement },
        .{ .line = 13, .problem = .missing_rationale },
    });
}

test "two claims on one line need the first split off" {
    try expectViolations(
        \\fn f(x: ?struct { v: ?u8 }) u8 {
        \\    return (x orelse unreachable).v orelse unreachable; // x and v are set together
        \\}
    , &.{.{ .line = 2, .problem = .code_after }});
}

test "literals, comments, and quoted identifiers are not candidates" {
    try expectViolations(
        \\const msg = "reached unreachable code";
        \\const ch = 'u';
        \\const text =
        \\    \\unreachable
        \\;
        \\// unreachable
        \\const Op = enum { @"unreachable", nop };
        \\const op = Op.@"unreachable";
    , &.{});
}
