//! Base58 encoding and decoding for BLAKE3 hashes
//!
//! This module provides base58 encoding/decoding specifically optimized for 256-bit BLAKE3 hashes.
//! The base58 alphabet excludes visually similar characters (0, O, I, l) to prevent confusion.
//!
//! This is a standalone utility module used by both bundle and unbundle modules.

const base58 = @import("base58.zig");

// Re-export all public declarations
pub const base58_hash_bytes = base58.base58_hash_bytes;
pub const encode = base58.encode;
pub const decode = base58.decode;

test {
    _ = base58;
}
