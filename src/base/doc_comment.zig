//! Character-level rules for recognizing and stripping Roc doc comment lines.
//!
//! In Roc, doc comments use the `##` prefix. A `###` prefix is a section-header
//! comment, which some consumers treat as a doc comment and others do not, so
//! this module exposes both a strict predicate (`isDocCommentLine`, which
//! excludes `###`) and a permissive one (`startsWithHashHash`, which does not).
//! Content extraction (`stripPrefix`) is shared by all consumers.

const std = @import("std");

/// Returns true if `trimmed` starts with `##`. Does NOT exclude `###`.
pub fn startsWithHashHash(trimmed: []const u8) bool {
    return trimmed.len >= 2 and trimmed[0] == '#' and trimmed[1] == '#';
}

/// Returns true if `trimmed` is a doc comment line: starts with `##` but not `###`.
pub fn isDocCommentLine(trimmed: []const u8) bool {
    if (trimmed.len < 2) return false;
    if (trimmed[0] != '#' or trimmed[1] != '#') return false;
    // Make sure it's not ### (section header)
    if (trimmed.len >= 3 and trimmed[2] == '#') return false;
    return true;
}

/// Strips the leading `##` and one optional following space from a line that
/// already begins with `##`, returning the doc comment content.
pub fn stripPrefix(line: []const u8) []const u8 {
    // Skip the ## prefix
    var start: usize = 2;

    // Skip a single space after ## if present (standard formatting)
    if (start < line.len and line[start] == ' ') {
        start += 1;
    }

    return line[start..];
}

test "startsWithHashHash: various cases" {
    try std.testing.expect(startsWithHashHash("## doc"));
    try std.testing.expect(startsWithHashHash("##"));
    try std.testing.expect(startsWithHashHash("##doc"));
    try std.testing.expect(startsWithHashHash("### header"));
    try std.testing.expect(!startsWithHashHash("# comment"));
    try std.testing.expect(!startsWithHashHash("#"));
    try std.testing.expect(!startsWithHashHash(""));
}

test "isDocCommentLine: various cases" {
    try std.testing.expect(isDocCommentLine("## doc"));
    try std.testing.expect(isDocCommentLine("##"));
    try std.testing.expect(isDocCommentLine("##doc"));
    try std.testing.expect(!isDocCommentLine("# comment"));
    try std.testing.expect(!isDocCommentLine("### header"));
    try std.testing.expect(!isDocCommentLine(""));
    try std.testing.expect(!isDocCommentLine("#"));
}

test "stripPrefix: with and without space and empty content" {
    try std.testing.expectEqualStrings("doc", stripPrefix("## doc"));
    try std.testing.expectEqualStrings("doc", stripPrefix("##doc"));
    try std.testing.expectEqualStrings("", stripPrefix("##"));
    try std.testing.expectEqualStrings("", stripPrefix("## "));
    try std.testing.expectEqualStrings(" doc", stripPrefix("##  doc"));
}
