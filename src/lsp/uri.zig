//! URI utilities for converting between file:// URIs and filesystem paths.

const std = @import("std");
const builtin = @import("builtin");

fn percentDecode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    errdefer out.deinit(allocator);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        const ch = input[i];
        if (ch == '%' and i + 2 < input.len) {
            const hi = std.fmt.charToDigit(input[i + 1], 16) catch null;
            const lo = std.fmt.charToDigit(input[i + 2], 16) catch null;
            if (hi) |h| {
                if (lo) |l| {
                    try out.append(allocator, @intCast(h * 16 + l));
                    i += 2;
                    continue;
                }
            }
        }

        try out.append(allocator, ch);
    }

    return out.toOwnedSlice(allocator);
}

fn percentEncode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    errdefer out.deinit(allocator);

    for (input) |ch| {
        const needs_escape = ch <= 0x20 or ch == '#' or ch == '%' or ch == '?';
        if (needs_escape) {
            var buf: [3]u8 = .{ '%', 0, 0 };
            const hi = std.fmt.digitToChar((ch >> 4) & 0xF, .upper);
            const lo = std.fmt.digitToChar(ch & 0xF, .upper);
            buf[1] = hi;
            buf[2] = lo;
            try out.appendSlice(allocator, &buf);
        } else {
            try out.append(allocator, ch);
        }
    }

    return out.toOwnedSlice(allocator);
}

/// Convert a `file://` URI into a local filesystem path.
pub fn uriToPath(allocator: std.mem.Allocator, uri: []const u8) ![]u8 {
    if (!std.mem.startsWith(u8, uri, "file://")) {
        return allocator.dupe(u8, uri);
    }

    const rest = uri["file://".len..];
    var decoded = try percentDecode(allocator, rest);
    errdefer allocator.free(decoded);

    // On Windows URIs start with /C:/..., drop the leading slash
    if (builtin.os.tag == .windows and decoded.len >= 3 and decoded[0] == '/' and std.ascii.isAlpha(decoded[1]) and decoded[2] == ':') {
        const trimmed = decoded[1..];
        decoded = try allocator.dupe(u8, trimmed);
    }

    return decoded;
}

/// Convert a filesystem path into a `file://` URI (best-effort encoding).
pub fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const encoded_path = try percentEncode(allocator, path);
    defer allocator.free(encoded_path);

    if (builtin.os.tag == .windows and encoded_path.len >= 2 and encoded_path[1] == ':') {
        return std.fmt.allocPrint(allocator, "file:///{s}", .{encoded_path});
    }

    return std.fmt.allocPrint(allocator, "file://{s}", .{encoded_path});
}
