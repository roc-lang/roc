//! Cache key generation and management for uniquely identifying cached compilation results.

const std = @import("std");
const fs_mod = @import("fs");

const Filesystem = fs_mod.Filesystem;
const Allocator = std.mem.Allocator;

/// Cache key that uniquely identifies a cached compilation result.
///
/// The cache key captures all factors that affect compilation output:
/// - Source content hash: Invalidates when file content changes
/// - File modification time: Additional validation layer
/// - Compiler version: Invalidates when compiler changes
///
/// Future extensions could include dependency hashes for import tracking.
pub const CacheKey = struct {
    content_hash: [32]u8, // SHA-256 of source content
    file_mtime: i128, // File modification time (nanoseconds since epoch)
    compiler_version: [32]u8, // Hash of compiler version/build info
    source_path: []const u8, // Path to the source file

    const Self = @This();

    /// Generate a cache key for the given source content and file path.
    ///
    /// This function computes all necessary hashes and retrieves file metadata
    /// to create a comprehensive cache key.
    pub fn generate(
        source: []const u8,
        file_path: []const u8,
        fs: Filesystem,
        allocator: Allocator,
    ) !Self {
        // Hash the source content
        var content_hasher = std.crypto.hash.sha2.Sha256.init(.{});
        content_hasher.update(source);
        const content_hash = content_hasher.finalResult();

        // Get file modification time
        const file_mtime = getFileModTime(file_path, fs) catch |err| switch (err) {
            error.FileNotFound => 0, // Use 0 for non-existent files (e.g., in-memory sources)
            else => return err,
        };

        // Get compiler version hash
        const compiler_version = getCompilerVersionHash();

        return Self{
            .content_hash = content_hash,
            .file_mtime = file_mtime,
            .compiler_version = compiler_version,
            .source_path = try allocator.dupe(u8, file_path),
        };
    }

    /// Convert cache key to a filesystem-safe filename.
    ///
    /// Returns a hex string representation that can be used as a cache filename.
    /// The filename includes enough information to avoid collisions while being
    /// filesystem-safe across different platforms.
    pub fn toCacheFileName(self: Self, allocator: Allocator) ![]u8 {
        // Create a combined hash of all key components
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        hasher.update(&self.content_hash);
        hasher.update(std.mem.asBytes(&self.file_mtime));
        hasher.update(&self.compiler_version);
        const combined_hash = hasher.finalResult();

        // Convert to hex string
        const filename = try allocator.alloc(u8, combined_hash.len * 2);
        _ = std.fmt.bufPrint(filename, "{}", .{std.fmt.fmtSliceHexLower(&combined_hash)}) catch unreachable;

        return filename;
    }

    /// Check if this cache key is equal to another.
    pub fn eql(self: Self, other: Self) bool {
        return std.mem.eql(u8, &self.content_hash, &other.content_hash) and
            self.file_mtime == other.file_mtime and
            std.mem.eql(u8, &self.compiler_version, &other.compiler_version);
    }

    /// Format cache key for debugging output.
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("CacheKey{{ content: {}, mtime: {}, compiler: {} }}", .{
            std.fmt.fmtSliceHexLower(self.content_hash[0..8]), // First 8 bytes for readability
            self.file_mtime,
            std.fmt.fmtSliceHexLower(self.compiler_version[0..8]), // First 8 bytes for readability
        });
    }

    /// Get the source file path from the cache key.
    pub fn getSourcePath(self: Self, allocator: Allocator) ![]u8 {
        return allocator.dupe(u8, self.source_path);
    }

    /// Free the source path when the key is no longer needed.
    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.source_path);
    }
};

/// Get file modification time in nanoseconds since epoch.
///
/// This provides a quick validation that the file hasn't changed since caching.
/// While the content hash is the primary validation, mtime provides an additional
/// layer of validation and can help detect file system-level changes.
fn getFileModTime(file_path: []const u8, fs: Filesystem) !i128 {
    const file_info = fs.getFileInfo(file_path) catch |err| switch (err) {
        error.FileNotFound => return 0, // Use 0 for non-existent files (e.g., in-memory sources)
        else => return err,
    };

    return file_info.mtime_ns;
}

/// Get a hash representing the current compiler version.
///
/// This ensures cache invalidation when the compiler version changes.
/// The hash should include version info, build flags, and other factors
/// that could affect compilation output.
fn getCompilerVersionHash() [32]u8 {
    // For now, we'll create a simple version hash based on compile-time information
    // In a real implementation, this would include version numbers, git hashes, etc.

    const version_info = comptime blk: {
        // Include Zig version and build mode as factors
        const zig_version = @import("builtin").zig_version;
        const build_mode = @import("builtin").mode;

        break :blk std.fmt.comptimePrint("roc-zig-{d}.{d}.{d}-{s}", .{
            zig_version.major,
            zig_version.minor,
            zig_version.patch,
            @tagName(build_mode),
        });
    };

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(version_info);

    // Add additional compile-time factors that could affect output
    hasher.update(@tagName(@import("builtin").target.cpu.arch));
    hasher.update(@tagName(@import("builtin").target.os.tag));

    return hasher.finalResult();
}
