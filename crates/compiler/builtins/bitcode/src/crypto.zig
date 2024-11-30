const std = @import("std");
const builtin = @import("builtin");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const list = @import("list.zig");
const utils = @import("utils.zig");
const testing = std.testing;

const Sha256 = extern struct {
    location: *sha2.Sha256,
};

fn create(comptime T: type) *T {
    if (builtin.is_test) {
        return std.testing.allocator.create(T) catch unreachable;
    }
    return @alignCast(@ptrCast(utils.allocateWithRefcount(@sizeOf(sha2.Sha256), @alignOf(sha2.Sha256), false)));
}

pub fn emptySha256() callconv(.C) Sha256 {
    const location: *sha2.Sha256 = create(sha2.Sha256);
    location.* = sha2.Sha256.init(.{});
    return Sha256{
        .location = location,
    };
}

test "emptySha256" {
    const empty_sha = emptySha256();
    defer std.testing.allocator.destroy(empty_sha.location);
    const empty_hash = empty_sha.location.*.peek();
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", empty_hash[0..empty_hash.len]));
}

pub fn sha256AddBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256 {
    var out = emptySha256();
    out.location.* = sha.location.*;
    if (data.bytes) |bytes| {
        const byteSlice: []u8 = bytes[0..data.length];
        out.location.*.update(byteSlice);
    }
    return out;
}

fn rcNone(_: ?[*]u8) callconv(.C) void {}

test "sha256AddBytes" {
    const empty_sha = emptySha256();
    defer std.testing.allocator.destroy(empty_sha.location);
    const abc = list.RocList.fromSlice(u8, "abc", false);
    defer abc.decref(@alignOf(u8), @sizeOf(u8), false, rcNone);
    const abc_sha = sha256AddBytes(empty_sha, abc);
    defer std.testing.allocator.destroy(abc_sha.location);
    const abc_hash = abc_sha.location.*.peek();
    try std.testing.expect(sameBytesAsHex("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", abc_hash[0..abc_hash.len]));
}

pub const Digest256 = extern struct {
    firstHalf: u128,
    secondHalf: u128,
};

pub fn sha256Digest(sha: Sha256) callconv(.C) Digest256 {
    return @bitCast(sha.location.*.peek());
}

fn sameBytesAsHex(comptime expected_hex: [:0]const u8, input: []const u8) bool {
    for (input, 0..) |input_byte, i| {
        const hex_byte = std.fmt.parseInt(u8, expected_hex[2 * i .. 2 * i + 2], 16) catch unreachable;
        if (hex_byte != input_byte) {
            return false;
        }
    }
    return true;
}
