const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;

pub fn sha256( )sha2.Sha256{
    return sha2.Sha256.init(.{});
}
