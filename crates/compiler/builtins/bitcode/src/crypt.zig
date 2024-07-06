const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const list = @import("list.zig");
const utils = @import("utils.zig");

const Sha256 = extern struct{
    location:usize,
    fn pointer(self : Sha256) *sha2.Sha256{
        return @ptrFromInt(self.location);
    }
    };

const EmptyStruct = extern struct{};

pub fn emptySha256(_: EmptyStruct) callconv(.C) Sha256{
    const allocation = utils.allocateWithRefcount(@sizeOf(sha2.Sha256), @alignOf(sha2.Sha256));
    const ptr:*sha2.Sha256 = @alignCast(@ptrCast(allocation));
    ptr.* = sha2.Sha256.init(.{});
    return Sha256{.location = @intFromPtr(ptr),};
}

pub fn addBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256{
    var out = emptySha256(undefined);
    out.pointer().* = sha.pointer().*;
    if(data.bytes)|bytes|{
        const byteSlice : []u8 = bytes[0..data.length];
        out.pointer().*.update(byteSlice);
    }
    return out;
}

pub const Digest256 =  extern struct{firstHalf:u128, secondHalf:u128,};

pub fn digest(sha: Sha256 ) callconv(.C) Digest256{
    return @bitCast(sha.pointer().*.peek());
}