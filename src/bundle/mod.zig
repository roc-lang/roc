//! Bundle functionality for Roc packages
//!
//! This module provides functionality to:
//! - Bundle Roc packages and their dependencies into compressed tar archives
//! - Validate paths for security and cross-platform compatibility
//!
//! Note: This module requires the C zstd library for compression.
//! For unbundling functionality that works on all platforms (including WebAssembly),
//! see the separate `unbundle` module.

pub const bundle = @import("bundle.zig");
pub const streaming_writer = @import("streaming_writer.zig");

// Re-export commonly used functions and types
pub const bundleFiles = bundle.bundle;
pub const pathHasBundleErr = bundle.pathHasBundleErr;
pub const validateBase58Hash = bundle.validateBase58Hash;

// Re-export error types
pub const BundleError = bundle.BundleError;
pub const PathValidationError = bundle.PathValidationError;
pub const PathValidationReason = bundle.PathValidationReason;
pub const ErrorContext = bundle.ErrorContext;

// Re-export constants
pub const STREAM_BUFFER_SIZE = bundle.STREAM_BUFFER_SIZE;
pub const DEFAULT_COMPRESSION_LEVEL = bundle.DEFAULT_COMPRESSION_LEVEL;

// Re-export allocator functions for zstd
pub const allocForZstd = bundle.allocForZstd;
pub const freeForZstd = bundle.freeForZstd;

// Test coverage includes:
// - Path validation for security and cross-platform compatibility
// - Bundle creation with various file types and sizes
// - Hash generation
// - Streaming compression
// - Large file handling
test {
    _ = @import("test_bundle.zig");
    //_ = @import("test_streaming.zig");
    _ = bundle;
}
