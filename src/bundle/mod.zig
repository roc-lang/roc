//! Bundle and unbundle functionality for Roc packages
//!
//! This module provides functionality to:
//! - Bundle Roc packages and their dependencies into compressed tar archives
//! - Unbundle these archives to restore the original files
//! - Download and extract bundled archives from HTTPS URLs
//! - Validate paths for security and cross-platform compatibility

pub const bundle = @import("bundle.zig");
pub const download = @import("download.zig");
pub const streaming_reader = @import("streaming_reader.zig");
pub const streaming_writer = @import("streaming_writer.zig");
pub const base58 = @import("base58.zig");

// Re-export commonly used functions and types
pub const bundleFiles = bundle.bundle;
pub const unbundle = bundle.unbundle;
pub const unbundleStream = bundle.unbundleStream;
pub const validateBase58Hash = bundle.validateBase58Hash;
pub const pathHasBundleErr = bundle.pathHasBundleErr;
pub const pathHasUnbundleErr = bundle.pathHasUnbundleErr;

// Re-export error types
pub const BundleError = bundle.BundleError;
pub const UnbundleError = bundle.UnbundleError;
pub const PathValidationError = bundle.PathValidationError;
pub const PathValidationReason = bundle.PathValidationReason;
pub const ErrorContext = bundle.ErrorContext;

// Re-export extract writer types
pub const ExtractWriter = bundle.ExtractWriter;
pub const DirExtractWriter = bundle.DirExtractWriter;

// Re-export constants
pub const STREAM_BUFFER_SIZE = bundle.STREAM_BUFFER_SIZE;
pub const DEFAULT_COMPRESSION_LEVEL = bundle.DEFAULT_COMPRESSION_LEVEL;

// Re-export allocator functions for zstd
pub const allocForZstd = bundle.allocForZstd;
pub const freeForZstd = bundle.freeForZstd;

// Re-export download functions
pub const downloadBundle = download.download;
pub const validateUrl = download.validateUrl;
pub const DownloadError = download.DownloadError;

// Test coverage includes:
// - Path validation for security and cross-platform compatibility
// - Bundle/unbundle roundtrip with various file types and sizes
// - Hash verification and corruption detection
// - Streaming compression/decompression
// - Download URL validation and security checks
// - Large file handling (with std.tar limitations)
test {
    _ = @import("test_bundle.zig");
    _ = @import("test_streaming.zig");
    _ = bundle;
    _ = download;
    _ = base58;
}