//! Tests for converting between filesystem paths and `file://` URIs.

const std = @import("std");
const builtin = @import("builtin");
const uri = @import("lsp").uri;

fn expectPathUri(path: []const u8, expected: []const u8) !void {
    const allocator = std.testing.allocator;
    const actual = try uri.pathToUri(allocator, path);
    defer allocator.free(actual);
    try std.testing.expectEqualStrings(expected, actual);

    const round_trip = try uri.uriToPath(allocator, actual);
    defer allocator.free(round_trip);
    try std.testing.expectEqualStrings(path, round_trip);
}

test "path characters allowed in a URI path stay unescaped" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    try expectPathUri("/home/a+b/x-y_z.~/c:d@e/!$&'()*,;=.roc", "file:///home/a+b/x-y_z.~/c:d@e/!$&'()*,;=.roc");
}

test "characters not allowed in a URI path are percent-encoded" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    try expectPathUri("/tmp/a b/#?%/[x]{y}|^`\"<>\\.roc", "file:///tmp/a%20b/%23%3F%25/%5Bx%5D%7By%7D%7C%5E%60%22%3C%3E%5C.roc");
}

test "each byte of a non-ASCII path character is percent-encoded" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    try expectPathUri("/tmp/café/日.roc", "file:///tmp/caf%C3%A9/%E6%97%A5.roc");
}
