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

pub const unbundle = @import("unbundle.zig");
pub const download = @import("download.zig");

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
    _ = @import("test_unbundle.zig");
}
