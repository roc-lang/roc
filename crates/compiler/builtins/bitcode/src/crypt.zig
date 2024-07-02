const std = @import("std");
const crypto = std.crypto;
const sha2 = crypto.hash.sha2;

pub fn sha256( )sha2.Sha256{
    return sha2.Sha256.init(.{});
}

pub fn addBytes(hasher: sha2.Sha256, bytes:[] const u8)sha2.Sha256{
    var out = hasher;
    out.update(bytes);
    return out;
}

const sha256_digest_length = 32;

pub const sha256_digest =  [sha256_digest_length]u8;

pub fn digest(hasher: sha2.Sha256 ) sha256_digest{
    return hasher.peek();
}