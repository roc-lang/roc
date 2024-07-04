const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;
const list = @importg("list.zig")
const utils = @import("utils.zig");

const Sha256 = extern struct{
    location:usize,
    pub fn pointer(self : Sha256) *sha2.Sha256{
        return @ptrFromInt(self.location);
    }
    }

pub fn sha256() callconv(.C) Sha256{
    const allocation = utils.allocateWithRefcount(@sizeOf(sha2.Sha256), @alignOf(sha2.Sha256));
    const ptr:*sha2.Sha256 = @ptrCast(allocation);
    ptr.* = sha2.Sha256.init(.{});
    return Sha256{.location = @intFromPtr(ptr)),};
}

pub fn addBytes(sha: Sha256, data: list.RocList) callconv(.C) Sha256{
    var out = sha256();
    if(data.bytes)|bytes|{
        out.pointer.*.update(bytes);
    }
    return out;
}

const sha256_digest_length = 32;

pub const Digest256 =  [sha256_digest_length]u8;

pub fn digest(sha: Sha256 ) callCov(.C) Digest256{
    return sha.pointer.*.peek();
}