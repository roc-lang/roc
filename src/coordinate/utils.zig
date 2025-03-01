//! This file exists because I (Sam) couldn't think of a good name
//! for these... please be kind
const std = @import("std");

/// Hashes the given data using the BLAKE3 algorithm.
pub fn blake3Hash(data: []const u8) [32]u8 {
    var digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(data, &digest, .{});

    return digest;
}
