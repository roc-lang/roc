//! Serialization format definitions for embedded module data.
//!
//! This module defines the binary format used to serialize Roc modules
//! for embedding in compiled binaries. The format is designed to be
//! portable across architectures by using fixed-size types.
//!
//! ## Format Overview
//!
//! The serialized format consists of:
//! 1. A header with magic number, version, and metadata
//! 2. An array of module info structures
//! 3. Source bytes and module names for each module
//! 4. Serialized ModuleEnv structures
//! 5. Entry point definition indices
//!
//! ## Usage
//!
//! **Writing (in roc build):**
//! - CLI compiles all modules and serializes using CompactWriter
//! - Serialized bytes are embedded into the interpreter shim binary
//!
//! **Reading (in interpreter shim):**
//! - Check magic number to identify format
//! - Read header to get module count and offsets
//! - Deserialize each ModuleEnv using offsets from SerializedModuleInfo

const std = @import("std");

/// Magic number for serialized embedded format: "RSER" (Roc Serialized)
///
/// This 4-byte magic number identifies the portable serialization format.
/// The bytes spell "RSER" when read as ASCII in little-endian order:
///   Byte 0: 0x52 = 'R', Byte 1: 0x53 = 'S', Byte 2: 0x45 = 'E', Byte 3: 0x52 = 'R'
///
/// This format uses ModuleEnv.Serialized with fixed-size fields for
/// cross-architecture support. The magic number distinguishes it from
/// the legacy raw format (which has no magic number).
///
/// ## Format Versioning Policy
///
/// - The magic number identifies the format family (RSER)
/// - The format_version field in SerializedHeader tracks breaking changes
/// - Bump format_version when:
///   * Field layouts change in SerializedHeader or SerializedModuleInfo
///   * The ModuleEnv.Serialized structure changes
///   * Offset calculation or alignment requirements change
/// - Backward compatibility: readers should check format_version and
///   reject versions they don't understand with a clear error message
pub const SERIALIZED_FORMAT_MAGIC: u32 = 0x52455352;

/// Current format version number.
///
/// Version history:
/// - v1: Initial portable serialization format for cross-architecture builds
///
/// Increment this when making breaking changes to the serialization format.
/// See SERIALIZED_FORMAT_MAGIC documentation for versioning policy.
pub const SERIALIZED_FORMAT_VERSION: u32 = 1;

/// Header for the serialized embedded format.
///
/// This header appears at the start of serialized module data.
/// All fields use fixed-size types for cross-architecture portability
/// (e.g., building on x86_64 for wasm32 target).
///
/// The structure is marked `extern` to ensure consistent memory layout
/// across compiler optimizations and platforms.
pub const SerializedHeader = extern struct {
    /// Magic number identifies this serialized format.
    /// Used to distinguish from legacy raw format.
    magic: u32,
    /// Format version (currently 1) - see SERIALIZED_FORMAT_VERSION
    format_version: u32,
    /// Number of modules in this serialized bundle
    module_count: u32,
    /// Number of entry points (exported functions)
    entry_count: u32,
    /// Index of primary environment (platform main or app)
    primary_env_index: u32,
    /// Index of app environment (for e_lookup_required resolution)
    app_env_index: u32,
    /// Offset to entry point def indices array (relative to buffer start)
    def_indices_offset: u64,
    /// Offset to module info array (relative to buffer start)
    module_infos_offset: u64,
};

/// Info for each serialized module.
///
/// An array of these structures is stored at `module_infos_offset` in the
/// serialized buffer. Each entry describes one module's data locations
/// within the buffer, enabling random access to any module.
///
/// All offsets are relative to the start of the serialized buffer.
pub const SerializedModuleInfo = extern struct {
    /// Offset to source bytes (relative to buffer start)
    source_offset: u64,
    /// Length of source bytes
    source_len: u64,
    /// Offset to module name string (relative to buffer start)
    module_name_offset: u64,
    /// Length of module name string
    module_name_len: u64,
    /// Offset to ModuleEnv.Serialized struct (relative to buffer start)
    env_serialized_offset: u64,
};

test "serialization constants" {
    // Verify magic number spells "RSER" in little-endian ASCII
    const magic_bytes: [4]u8 = @bitCast(SERIALIZED_FORMAT_MAGIC);
    try std.testing.expectEqualStrings("RSER", &magic_bytes);

    // Verify struct sizes are stable (important for binary compatibility)
    try std.testing.expectEqual(@as(usize, 40), @sizeOf(SerializedHeader));
    try std.testing.expectEqual(@as(usize, 40), @sizeOf(SerializedModuleInfo));
}
