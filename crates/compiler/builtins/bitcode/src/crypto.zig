const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const list = @import("list.zig");
const utils = @import("utils.zig");
const testing = std.testing;

const Sha256 = extern struct {
    location: [*]u8,
    fn pointer(self: Sha256) *sha2.Sha256 {
        return @alignCast(@ptrCast(self.location));
    }
};

pub fn emptySha256() callconv(.C) Sha256 {
    const allocation = utils.allocateWithRefcount(@sizeOf(sha2.Sha256), @alignOf(sha2.Sha256), false);
    const ptr: *sha2.Sha256 = @alignCast(@ptrCast(allocation));
    ptr.* = sha2.Sha256.init(.{});
    return Sha256{
        .location = @alignCast(@ptrCast(ptr)),
    };
}

test "emptySha256" {
    const emptyHash = emptySha256().pointer().*.peek();
    try std.testing.expectEqualSlices(u8, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", emptyHash[0..emptyHash.len]);
    }

pub fn sha256AddBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256 {
    var out = emptySha256();
    out.pointer().* = sha.pointer().*;
    if (data.bytes) |bytes| {
        const byteSlice: []u8 = bytes[0..data.length];
        out.pointer().*.update(byteSlice);
    }
    return out;
}

pub const Digest256 = extern struct {
    firstHalf: u128,
    secondHalf: u128,
};

pub fn sha256Digest(sha: Sha256) callconv(.C) Digest256 {
    return @bitCast(sha.pointer().*.peek());
}
