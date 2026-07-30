//! Unbundle functionality for Roc packages using Zig's standard library
//!
//! This module provides functionality to:
//! - Unbundle compressed tar archives (.tar.zst files)
//! - Validate and decode base58-encoded hashes
//! - Extract files with security and cross-platform path validation
//! - Download and extract bundled archives from HTTPS URLs
//!
//! This module uses Zig's std.compress.zstandard for decompression,
//! making it compatible with WebAssembly targets.

const std = @import("std");

pub const unbundle = @import("unbundle.zig");
pub const download = @import("download.zig");
pub const localhost = @import("localhost.zig");

/// Shared `.tar.zst` format constants, referenced by both `unbundle` and `bundle`.
pub const format = @import("format.zig");

// Re-export commonly used functions and types
pub const unbundleFiles = unbundle.unbundle;
pub const unbundleStream = unbundle.unbundleStream;
pub const validateBase58Hash = unbundle.validateBase58Hash;
pub const pathHasUnbundleErr = unbundle.pathHasUnbundleErr;

// Re-export error types
pub const UnbundleError = unbundle.UnbundleError;
pub const PathValidationError = unbundle.PathValidationError;
pub const PathValidationReason = unbundle.PathValidationReason;
pub const ErrorContext = unbundle.ErrorContext;

// Re-export extract writer types
pub const ExtractWriter = unbundle.ExtractWriter;
pub const DirExtractWriter = unbundle.DirExtractWriter;
pub const BufferExtractWriter = unbundle.BufferExtractWriter;

// Re-export download functionality
pub const downloadAndExtract = download.downloadAndExtract;
pub const downloadAndExtractToBuffer = download.downloadAndExtractToBuffer;

// Include tests
test {
    const tests = @import("test_unbundle.zig");
    std.testing.refAllDecls(tests);
}

// `base.url.isBase58Char` re-expresses `base58.base58_alphabet` as character
// ranges because the `base` module cannot import `base58`. This module imports
// both, so it hosts the guard that the two definitions agree.
test "base.url.isBase58Char accepts exactly the base58 alphabet" {
    const base = @import("base");
    const base58 = @import("base58");
    var byte: usize = 0;
    while (byte <= 255) : (byte += 1) {
        const char: u8 = @intCast(byte);
        const in_alphabet = std.mem.findScalar(u8, base58.base58_alphabet, char) != null;
        try std.testing.expectEqual(in_alphabet, base.url.isBase58Char(char));
    }
    try std.testing.expectEqual(@as(usize, 58), base58.base58_alphabet.len);
}
