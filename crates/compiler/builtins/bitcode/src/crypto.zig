const std = @import("std");
const builtin = @import("builtin");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const list = @import("list.zig");
const utils = @import("utils.zig");
const testing = std.testing;

const Sha256 = extern struct {
    data: [@sizeOf(sha2.Sha256)]u8 align(@alignOf(sha2.Sha256)),
};

test "Sha256 size and alignment" {
    try std.testing.expectEqual(@sizeOf(Sha256), 128);
    try std.testing.expectEqual(@alignOf(Sha256), 16);
}

pub fn emptySha256() callconv(.C) Sha256 {
    var sha: Sha256 = undefined;
    const ptr: *sha2.Sha256 = @constCast(@ptrCast(&sha));
    ptr.* = sha2.Sha256.init(.{});
    return sha;
}

test "emptySha256" {
    const empty_sha = emptySha256();
    const ptr: *sha2.Sha256 = @constCast(@ptrCast(&empty_sha));
    const empty_hash = ptr.*.peek();
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", empty_hash[0..empty_hash.len]));
}

pub fn sha256AddBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256 {
    var out = sha;
    const out_ptr: *sha2.Sha256 = @constCast(@ptrCast(&out));
    if (data.bytes) |bytes| {
        const byteSlice: []u8 = bytes[0..data.length];
        out_ptr.*.update(byteSlice);
    }
    return out;
}

test "sha256AddBytes" {
    const empty_sha = emptySha256();
    const abc = list.RocList.fromSlice(u8, "abc", false);
    defer abc.decref(@alignOf(u8), @sizeOf(u8), false, rcNone);
    const abc_sha = sha256AddBytes(empty_sha, abc);
    const abc_ptr: *sha2.Sha256 = @constCast(@ptrCast(&abc_sha));
    const abc_hash = abc_ptr.*.peek();
    try std.testing.expect(sameBytesAsHex("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", abc_hash[0..abc_hash.len]));
}

pub const Digest256 = extern struct {
    first_half: u128,
    second_half: u128,
};

pub fn sha256Digest(sha: Sha256) callconv(.C) Digest256 {
    const ptr: *sha2.Sha256 = @constCast(@ptrCast(&sha));
    return @bitCast(ptr.*.peek());
}

test "sha256Digest" {
    const empty_sha = emptySha256();
    const digest = sha256Digest(empty_sha);
    const first_half_bytes: [16]u8 = @bitCast(digest.first_half);
    const second_half_bytes: [16]u8 = @bitCast(digest.second_half);
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb924", first_half_bytes[0..first_half_bytes.len]));
    try std.testing.expect(sameBytesAsHex("27ae41e4649b934ca495991b7852b855", second_half_bytes[0..second_half_bytes.len]));
}
// ----------------test utilities ------------------------
fn rcNone(_: ?[*]u8) callconv(.C) void {}

fn sameBytesAsHex(comptime expected_hex: [:0]const u8, input: []const u8) bool {
    if (expected_hex.len != 2 * input.len) {
        return false;
    }

    for (input, 0..) |input_byte, i| {
        const hex_byte = std.fmt.parseInt(u8, expected_hex[2 * i .. 2 * i + 2], 16) catch unreachable;
        if (hex_byte != input_byte) {
            return false;
        }
    }

    return true;
}
