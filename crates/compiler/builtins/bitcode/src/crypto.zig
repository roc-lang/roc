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

const ThirtyTwoBytes = extern struct {
    first: u128,
    second: u128,
    const zero = .{ .first = 0, .second = 0 };
};

const Dummy = extern struct {
    first: ThirtyTwoBytes,
    second: ThirtyTwoBytes,
    const zero = .{ .first = ThirtyTwoBytes.zero, .second = ThirtyTwoBytes.zero };
};

// const Dummy = extern struct {
//     first: SixtyFourBytes,
//     second: SixtyFourBytes,
//     const zero = .{ .first = SixtyFourBytes.zero, .second = SixtyFourBytes.zero };
// };

// test "Dummy size and alignment" {
//     try std.testing.expectEqual(@sizeOf(Dummy), 128);
//     try std.testing.expectEqual(@sizeOf(Dummy), @sizeOf(sha2.Sha256));
//     try std.testing.expectEqual(@alignOf(Dummy), 16);
//     try std.testing.expectEqual(@alignOf(Dummy), @alignOf(sha2.Sha256));
// }

fn create(comptime T: type) *T {
    //test_roc_alloc ignores alignment
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

pub fn emptyDummy() callconv(.C) Dummy {
    // var sha: Dummy = undefined;
    // const ptr: *sha2.Sha256 = @ptrCast(sha.data[0..Dummy.len]);
    // ptr.* = sha2.Sha256.init(.{});
    // return sha;
    return Dummy.zero;
}

test "emptyDummy" {
    const empty_sha = emptyDummy();
    const ptr: *sha2.Sha256 = @constCast(@ptrCast(&empty_sha));
    const empty_hash = ptr.*.peek();
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", empty_hash[0..empty_hash.len]));
}

pub fn sha256AddBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256 {
    const out = emptySha256();
    out.location.* = sha.location.*;
    if (data.bytes) |bytes| {
        const byteSlice: []u8 = bytes[0..data.length];
        out.location.*.update(byteSlice);
    }
    return out;
}

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

pub fn dummyAddBytes(sha: Dummy, data: list.RocList) callconv(.C) Dummy {
    var out = sha;
    const out_ptr: *sha2.Sha256 = @ptrCast(out.data[0..Dummy.len]);
    if (data.bytes) |bytes| {
        const byteSlice: []u8 = bytes[0..data.length];
        out_ptr.*.update(byteSlice);
    }
    return out;
}

test "dummy256AddBytes" {
    const empty_sha = emptyDummy();
    const abc = list.RocList.fromSlice(u8, "abc", false);
    defer abc.decref(@alignOf(u8), @sizeOf(u8), false, rcNone);
    const abc_sha = dummyAddBytes(empty_sha, abc);
    const abc_ptr: *sha2.Sha256 = @constCast(@ptrCast(&abc_sha));
    const abc_hash = abc_ptr.*.peek();
    try std.testing.expect(sameBytesAsHex("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad", abc_hash[0..abc_hash.len]));
}

pub const Digest256 = extern struct {
    first_half: u128,
    second_half: u128,
};

pub fn sha256Digest(sha: Sha256) callconv(.C) Digest256 {
    return @bitCast(sha.location.*.peek());
}

test "sha256Digest" {
    const empty_sha = emptySha256();
    defer std.testing.allocator.destroy(empty_sha.location);
    const digest = sha256Digest(empty_sha);
    const first_half_bytes: [16]u8 = @bitCast(digest.first_half);
    const second_half_bytes: [16]u8 = @bitCast(digest.second_half);
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb924", first_half_bytes[0..first_half_bytes.len]));
    try std.testing.expect(sameBytesAsHex("27ae41e4649b934ca495991b7852b855", second_half_bytes[0..second_half_bytes.len]));
}

pub fn dummyDigest(sha: Dummy) callconv(.C) Digest256 {
    const ptr: *const sha2.Sha256 = @ptrCast(sha.data[0..Dummy.len]);
    return @bitCast(ptr.*.peek());
}

test "dummy256Digest" {
    const empty_sha = emptyDummy();
    const digest = dummyDigest(empty_sha);
    const first_half_bytes: [16]u8 = @bitCast(digest.first_half);
    const second_half_bytes: [16]u8 = @bitCast(digest.second_half);
    try std.testing.expect(sameBytesAsHex("e3b0c44298fc1c149afbf4c8996fb924", first_half_bytes[0..first_half_bytes.len]));
    try std.testing.expect(sameBytesAsHex("27ae41e4649b934ca495991b7852b855", second_half_bytes[0..second_half_bytes.len]));
}

//----------------test utilities ------------------------
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
