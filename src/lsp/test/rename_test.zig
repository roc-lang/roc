//! Unit tests for the rename name rules.

const std = @import("std");
const lsp = @import("lsp");

const rename = lsp.rename;
const allocator = std.testing.allocator;

test "rename accepts a plain lowercase rename" {
    try std.testing.expectEqual(@as(?rename.Rejection, null), try rename.checkNewName(allocator, "foo", "bar"));
}

test "rename accepts an uppercase rename" {
    try std.testing.expectEqual(@as(?rename.Rejection, null), try rename.checkNewName(allocator, "Foo", "Bar"));
}

test "rename accepts renaming an effectful name to another effectful name" {
    try std.testing.expectEqual(@as(?rename.Rejection, null), try rename.checkNewName(allocator, "read!", "load!"));
}

test "rename accepts renaming an ignored name to another ignored name" {
    try std.testing.expectEqual(@as(?rename.Rejection, null), try rename.checkNewName(allocator, "_unused", "_spare"));
}

test "rename rejects gaining an effectful marker" {
    // `foo` to `foo!` is not a rename: the `!` is what marks a value effectful.
    try std.testing.expectEqual(rename.Rejection.changes_ident_kind, try rename.checkNewName(allocator, "foo", "foo!"));
}

test "rename rejects losing an effectful marker" {
    try std.testing.expectEqual(rename.Rejection.changes_ident_kind, try rename.checkNewName(allocator, "read!", "read"));
}

test "rename rejects swapping identifier case" {
    try std.testing.expectEqual(rename.Rejection.changes_ident_kind, try rename.checkNewName(allocator, "foo", "Foo"));
    try std.testing.expectEqual(rename.Rejection.changes_ident_kind, try rename.checkNewName(allocator, "Foo", "foo"));
}

test "rename rejects gaining an ignored marker" {
    try std.testing.expectEqual(rename.Rejection.changes_ident_kind, try rename.checkNewName(allocator, "used", "_used"));
}

test "rename rejects an empty name" {
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", ""));
}

test "rename rejects a name that is several tokens" {
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "bar baz"));
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "bar = 1"));
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "bar."));
}

test "rename rejects punctuation and numbers" {
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "42"));
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "+"));
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "\"bar\""));
}

test "rename rejects leading and trailing whitespace" {
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", " bar"));
    try std.testing.expectEqual(rename.Rejection.not_an_identifier, try rename.checkNewName(allocator, "foo", "bar "));
}
