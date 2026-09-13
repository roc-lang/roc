//! Deterministic non-cryptographic 128-bit hashes for compiler type and evidence identity.
//! Artifact and module identity hashes use SHA-256 separately.
const std = @import("std");
const TypeDigestHasher = @This();

/// Width of canonical type and evidence digest outputs in bytes.
pub const digest_length = 16;

first: std.hash.XxHash3,
second: std.hash.XxHash3,

/// The fixed, distinct seeds and output byte order are part of the cache format.
pub fn init() TypeDigestHasher {
    return .{
        .first = std.hash.XxHash3.init(0),
        .second = std.hash.XxHash3.init(0x9e3779b97f4a7c15),
    };
}

/// Feed the same canonical encoding to both independently seeded streams.
pub fn update(self: *TypeDigestHasher, bytes: []const u8) void {
    self.first.update(bytes);
    self.second.update(bytes);
}

/// Concatenate both stream results in a target-independent byte order.
pub fn finalResult(self: *TypeDigestHasher) [digest_length]u8 {
    var result: [digest_length]u8 = undefined;
    std.mem.writeInt(u64, result[0..8], self.first.final(), .little);
    std.mem.writeInt(u64, result[8..16], self.second.final(), .little);
    return result;
}

/// Hash one complete canonical encoding.
pub fn hash(bytes: []const u8) [digest_length]u8 {
    var hasher = init();
    hasher.update(bytes);
    return hasher.finalResult();
}

test "type digest golden value pins seeds and little-endian stream order" {
    // Computed independently using XxHash3.hash for each fixed seed, with each
    // 64-bit result serialized little-endian in first-then-second order.
    const expected = [digest_length]u8{
        0x71, 0x09, 0xf1, 0x27, 0x00, 0xd2, 0x9e, 0x8b,
        0x0b, 0x24, 0xd3, 0xc0, 0x9a, 0x2e, 0x21, 0xba,
    };
    try std.testing.expectEqual(expected, hash("roc.type-digest"));
}

test "type digests are deterministic and sensitive to every input byte" {
    var input: [257]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i);
    const expected = hash(&input);
    try std.testing.expectEqual(expected, hash(&input));
    for (&input) |*byte| {
        byte.* ^= 1;
        try std.testing.expect(!std.mem.eql(u8, &expected, &hash(&input)));
        byte.* ^= 1;
    }
}

test "type digest streaming preserves the encoding across chunk boundaries" {
    var input: [1025]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i);
    for ([_]usize{ 0, 1, 16, 64, 128, 240, 241, 1024, 1025 }) |len| {
        const expected = hash(input[0..len]);
        for (0..len + 1) |split| {
            var hasher = init();
            hasher.update(input[0..split]);
            hasher.update(input[split..len]);
            try std.testing.expectEqual(expected, hasher.finalResult());
        }
    }
}
