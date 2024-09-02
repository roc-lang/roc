const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const mem = std.mem;
const list = @import("list.zig");
const utils = @import("utils.zig");

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

pub fn addBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256 {
    var out = emptySha256();
    out.pointer().* = sha.pointer().*;
    if (data.bytes) |bytes| {
        const byteSlice = bytes[0..data.length];
        out.pointer().*.update(byteSlice);
    }
    return out;
}

pub const Digest256 = extern struct {
    bytes:[32]u8,
};

pub fn digest(sha: Sha256) callconv(.C) Digest256 {
    return @bitCast(sha.pointer().*.peek());
}

pub fn digest256Eq(left : Digest256, right : Digest256) callconv(.C) bool{
    return mem.eql(u8, &left.bytes, &right.bytes);
}

pub fn digest256ByteList(dig : Digest256) callconv(.C) list.RocList{
    return list.RocList.fromSlice(u8, dig.bytes[0..32],false);
}