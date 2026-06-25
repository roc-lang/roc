//! Adapted from the Zig standard library at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!
//!
//! A wrapper over a byte-slice, providing useful methods for parsing string floating point values.

const std = @import("std");
const FloatStream = @This();
const common = @import("common.zig");

slice: []const u8,
offset: usize,
underscore_count: usize,

/// Create a stream over a float literal byte slice.
pub fn init(s: []const u8) FloatStream {
    return .{ .slice = s, .offset = 0, .underscore_count = 0 };
}

/// Return the offset from the start excluding underscores already consumed.
pub fn offsetTrue(self: FloatStream) usize {
    return self.offset - self.underscore_count;
}

/// Reset the stream to the beginning of the slice.
pub fn reset(self: *FloatStream) void {
    self.offset = 0;
    self.underscore_count = 0;
}

/// Return the number of unread bytes.
pub fn len(self: FloatStream) usize {
    if (self.offset > self.slice.len) {
        return 0;
    }
    return self.slice.len - self.offset;
}

/// Return whether at least `n` bytes remain unread.
pub fn hasLen(self: FloatStream, n: usize) bool {
    return self.offset + n <= self.slice.len;
}

/// Return the first unread byte without checking bounds.
pub fn firstUnchecked(self: FloatStream) u8 {
    return self.slice[self.offset];
}

/// Return the first unread byte, or null at end of input.
pub fn first(self: FloatStream) ?u8 {
    return if (self.hasLen(1))
        return self.firstUnchecked()
    else
        null;
}

/// Return whether the stream has no unread bytes.
pub fn isEmpty(self: FloatStream) bool {
    return !self.hasLen(1);
}

/// Return whether the first unread byte matches one of `cs`.
pub fn firstIs(self: FloatStream, comptime cs: []const u8) bool {
    if (self.first()) |ok| {
        inline for (cs) |c| if (ok == c) return true;
    }
    return false;
}

/// Return whether the first unread byte case-insensitively matches one of `cs`.
pub fn firstIsLower(self: FloatStream, comptime cs: []const u8) bool {
    if (self.first()) |ok| {
        inline for (cs) |c| if (ok | 0x20 == c) return true;
    }
    return false;
}

/// Return whether the first unread byte is a digit in the requested base.
pub fn firstIsDigit(self: FloatStream, comptime base: u8) bool {
    comptime std.debug.assert(base == 10 or base == 16);

    if (self.first()) |ok| {
        return common.isDigit(ok, base);
    }
    return false;
}

/// Advance the unread cursor by `n` bytes.
pub fn advance(self: *FloatStream, n: usize) void {
    self.offset += n;
}

/// Skip all leading bytes that match one of `cs`.
pub fn skipChars(self: *FloatStream, comptime cs: []const u8) void {
    while (self.firstIs(cs)) : (self.advance(1)) {}
}

/// Read the next 8 bytes as little-endian u64 without checking bounds.
pub fn readU64Unchecked(self: FloatStream) u64 {
    return std.mem.readInt(u64, self.slice[self.offset..][0..8], .little);
}

/// Read the next 8 bytes as little-endian u64, or null if fewer remain.
pub fn readU64(self: FloatStream) ?u64 {
    if (self.hasLen(8)) {
        return self.readU64Unchecked();
    }
    return null;
}

/// Return the unread byte at offset `i` without checking bounds.
pub fn atUnchecked(self: *FloatStream, i: usize) u8 {
    return self.slice[self.offset + i];
}

/// Consume and return one digit in the requested base, skipping underscores.
pub fn scanDigit(self: *FloatStream, comptime base: u8) ?u8 {
    comptime std.debug.assert(base == 10 or base == 16);

    retry: while (true) {
        if (self.first()) |ok| {
            if ('0' <= ok and ok <= '9') {
                self.advance(1);
                return ok - '0';
            } else if (base == 16 and 'a' <= ok and ok <= 'f') {
                self.advance(1);
                return ok - 'a' + 10;
            } else if (base == 16 and 'A' <= ok and ok <= 'F') {
                self.advance(1);
                return ok - 'A' + 10;
            } else if (ok == '_') {
                self.advance(1);
                self.underscore_count += 1;
                continue :retry;
            }
        }
        return null;
    }
}
