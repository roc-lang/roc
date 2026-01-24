//! Module cache for Roc files
//!
//! This module provides memory-mapped caching for compiled Roc modules,
//! allowing fast serialization and deserialization of ModuleEnv and CIR data.

const std = @import("std");
const can = @import("can");
const collections = @import("collections");

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
// Note: We use SHA256 instead of Blake3 because std.crypto.hash.Blake3 has a bug
// that prevents comptime evaluation (integer truncation issue in fillBlockBuf).
const Sha256 = std.crypto.hash.sha2.Sha256;

const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

/// Magic number for cache validation
const CACHE_MAGIC: u32 = 0x524F4343; // "ROCC" in ASCII

/// Compute a version hash for a struct type using SHA256 at comptime.
/// This hash changes when the struct layout changes, enabling automatic cache invalidation.
fn computeVersionHash(comptime StructType: type) [32]u8 {
    @setEvalBranchQuota(100000);

    const type_info = @typeInfo(StructType);
    const layout_str = if (type_info != .@"struct")
        "not_a_struct"
    else blk: {
        var result: []const u8 = @typeName(StructType);
        for (type_info.@"struct".fields) |field| {
            result = result ++ ";" ++ field.name ++ ":" ++ @typeName(field.type);
        }
        break :blk result;
    };

    var hasher = Sha256.init(.{});
    hasher.update(layout_str);
    var result: [32]u8 = undefined;
    hasher.final(&result);
    return result;
}

/// Version hash of ModuleEnv.Serialized computed at comptime
const MODULE_ENV_VERSION_HASH: [32]u8 = computeVersionHash(ModuleEnv.Serialized);

/// Cache header that gets written to disk before the cached data
pub const Header = struct {
    /// Magic number for validation
    magic: u32,

    /// Version hash of ModuleEnv.Serialized layout.
    /// Invalidates cache if ModuleEnv.Serialized layout changes.
    version_hash: [32]u8,

    /// Total size of the data section (excluding this header)
    data_size: u32,

    /// Diagnostic counts for accurate reporting when loading from cache
    error_count: u32,
    warning_count: u32,

    /// Padding to ensure alignment
    _padding: [4]u8 = [_]u8{0} ** 4,

    /// Error specific to initializing a Header from bytes
    pub const InitError = error{
        PartialRead,
        InvalidMagic,
        InvalidVersionHash,
    };

    /// Verify that the given buffer begins with a valid Header
    pub fn initFromBytes(buf: []align(@alignOf(Header)) u8) InitError!*Header {
        if (buf.len < @sizeOf(Header)) {
            return InitError.PartialRead;
        }

        const header = @as(*Header, @ptrCast(buf.ptr));
        const data_start = @sizeOf(Header);
        const data_end = data_start + header.data_size;

        // The buffer might not contain complete data after the header
        if (buf.len < data_end) {
            return InitError.PartialRead;
        }

        // Validate magic
        if (header.magic != CACHE_MAGIC) return InitError.InvalidMagic;

        // Validate version hash
        if (!std.mem.eql(u8, &header.version_hash, &MODULE_ENV_VERSION_HASH)) {
            return InitError.InvalidVersionHash;
        }

        return header;
    }
};

/// Memory-mapped cache that can be read directly from disk
pub const CacheModule = struct {
    header: *const Header,
    data: []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8,

    /// Create a cache by serializing ModuleEnv and CIR data.
    /// The provided allocator is used for the returned cache data, while
    /// the arena allocator is used for temporary serialization data.
    pub fn create(
        allocator: Allocator,
        arena_allocator: Allocator,
        module_env: *const ModuleEnv,
        _: *const ModuleEnv, // ModuleEnv contains the canonical IR
        error_count: u32,
        warning_count: u32,
    ) ![]align(SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
        const CompactWriter = collections.CompactWriter;

        // Create CompactWriter
        var writer = CompactWriter.init();

        // Allocate space for ModuleEnv.Serialized
        const env_ptr = try writer.appendAlloc(arena_allocator, ModuleEnv);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));

        // Serialize the ModuleEnv
        try serialized_ptr.serialize(module_env, arena_allocator, &writer);

        // Get the total size
        const total_data_size = writer.total_bytes;

        // Allocate cache_data for header + data
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT.toByteUnits());
        const total_size = header_size + total_data_size;
        const cache_data = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, total_size);
        errdefer allocator.free(cache_data);

        // Initialize header
        const header = @as(*Header, @ptrCast(cache_data.ptr));
        header.* = Header{
            .magic = CACHE_MAGIC,
            .version_hash = MODULE_ENV_VERSION_HASH,
            .data_size = @intCast(total_data_size),
            .error_count = error_count,
            .warning_count = warning_count,
            ._padding = [_]u8{0} ** 4,
        };

        // Consolidate the scattered iovecs into the cache data buffer
        const data_section = cache_data[header_size..];
        var offset: usize = 0;
        for (writer.iovecs.items) |iovec| {
            const end = offset + iovec.iov_len;
            @memcpy(data_section[offset..end], iovec.iov_base[0..iovec.iov_len]);
            offset = end;
        }

        return cache_data;
    }

    /// Load a cache from memory-mapped data
    pub fn fromMappedMemory(mapped_data: []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8) !CacheModule {
        if (mapped_data.len < @sizeOf(Header)) {
            return error.BufferTooSmall;
        }

        const header = @as(*const Header, @ptrCast(mapped_data.ptr));

        // Validate header (including version hash)
        _ = Header.initFromBytes(@constCast(mapped_data)) catch |err| {
            return switch (err) {
                error.PartialRead => error.BufferTooSmall,
                error.InvalidMagic => error.InvalidMagicNumber,
                error.InvalidVersionHash => error.CacheVersionHashMismatch,
            };
        };

        // Validate data size
        const expected_total_size = @sizeOf(Header) + header.data_size;
        if (mapped_data.len < expected_total_size) return error.BufferTooSmall;

        // Get data section (must be aligned)
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT.toByteUnits());
        const data = mapped_data[header_size .. header_size + header.data_size];

        return CacheModule{
            .header = header,
            .data = @as([]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @alignCast(data)),
        };
    }

    /// Restore ModuleEnv from the cached data
    /// IMPORTANT: This expects source to remain valid for the lifetime of the restored ModuleEnv.
    pub fn restore(self: *const CacheModule, allocator: Allocator, module_name: []const u8, source: []const u8) !*ModuleEnv {
        // The entire data section contains the serialized ModuleEnv
        const serialized_data = self.data;

        // The ModuleEnv.Serialized should be at the beginning of the data
        // Note: Check against Serialized size, not ModuleEnv size, since we're deserializing from Serialized format
        if (serialized_data.len < @sizeOf(ModuleEnv.Serialized)) {
            return error.BufferTooSmall;
        }

        // Get pointer to the serialized ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(@constCast(serialized_data.ptr))));

        // Calculate the base address of the serialized data
        const base_addr = @intFromPtr(serialized_data.ptr);

        // Deserialize the ModuleEnv with mutable types so it can be type-checked further
        const module_env_ptr: *ModuleEnv = try deserialized_ptr.deserializeWithMutableTypes(base_addr, allocator, source, module_name);

        return module_env_ptr;
    }

    /// Get diagnostic information about the cache
    pub fn getDiagnostics(self: *const CacheModule) Diagnostics {
        return Diagnostics{
            .total_size = @sizeOf(Header) + self.header.data_size,
            .header_size = @sizeOf(Header),
            .data_size = self.header.data_size,
        };
    }

    /// Validate the cache structure and integrity
    pub fn validate(self: *const CacheModule) !void {
        // Just validate that we have data
        if (self.data.len != self.header.data_size) {
            return error.DataSizeMismatch;
        }
    }

    /// Convenience function for reading cache files
    pub fn readFromFile(
        allocator: Allocator,
        file_path: []const u8,
        filesystem: anytype,
    ) ![]align(SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
        const file_data = try filesystem.readFile(file_path, allocator);
        defer allocator.free(file_data);

        const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, file_data.len);
        @memcpy(buffer, file_data);

        return buffer;
    }

    /// Tagged union to represent cache data that can be either memory-mapped or heap-allocated
    pub const CacheData = union(enum) {
        mapped: struct {
            ptr: [*]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8,
            len: usize,
            unaligned_ptr: [*]const u8,
            unaligned_len: usize,
        },
        allocated: []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8,

        pub fn data(self: CacheData) []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8 {
            return switch (self) {
                .mapped => |m| m.ptr[0..m.len],
                .allocated => |a| a,
            };
        }

        pub fn deinit(self: CacheData, allocator: Allocator) void {
            switch (self) {
                .mapped => |m| {
                    // Use the unaligned pointer for munmap
                    if (comptime @hasDecl(std.posix, "munmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .freestanding) {
                        const page_aligned_ptr = @as([*]align(std.heap.page_size_min) const u8, @alignCast(m.unaligned_ptr));
                        std.posix.munmap(page_aligned_ptr[0..m.unaligned_len]);
                    }
                },
                .allocated => |a| allocator.free(a),
            }
        }
    };

    /// Read cache file using memory mapping for better performance when available
    pub fn readFromFileMapped(
        allocator: Allocator,
        file_path: []const u8,
        filesystem: anytype,
    ) !CacheData {
        // TEMPORARILY DISABLED: mmap for debugging - always use allocated memory
        // Try to use memory mapping on supported platforms
        if (false and comptime @hasDecl(std.posix, "mmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .freestanding) {
            // Open the file
            const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch {
                // Fall back to regular reading on open error
                const data = try readFromFile(allocator, file_path, filesystem);
                return CacheData{ .allocated = data };
            };
            defer file.close();

            // Get file size
            const stat = try file.stat();
            const file_size = stat.size;

            // Check if file size exceeds usize limits on 32-bit systems
            if (file_size > std.math.maxInt(usize)) {
                // Fall back to regular reading for very large files
                const data = try readFromFile(allocator, file_path, filesystem);
                return CacheData{ .allocated = data };
            }

            const file_size_usize = @as(usize, @intCast(file_size));

            // Memory map the file
            const mapped_memory = if (comptime @import("builtin").target.os.tag == .macos or
                @import("builtin").target.os.tag == .ios or
                @import("builtin").target.os.tag == .tvos or
                @import("builtin").target.os.tag == .watchos)
                std.posix.mmap(
                    null,
                    file_size_usize,
                    std.posix.PROT.READ,
                    .{ .TYPE = .PRIVATE },
                    file.handle,
                    0,
                )
            else
                std.posix.mmap(
                    null,
                    file_size_usize,
                    std.posix.PROT.READ,
                    .{ .TYPE = .PRIVATE },
                    file.handle,
                    0,
                );

            const result = mapped_memory catch {
                // Fall back to regular reading on mmap error
                const data = try readFromFile(allocator, file_path, filesystem);
                return CacheData{ .allocated = data };
            };

            // Find the aligned portion within the mapped memory
            const unaligned_ptr = @as([*]const u8, @ptrCast(result.ptr));
            const addr = @intFromPtr(unaligned_ptr);
            const aligned_addr = std.mem.alignForward(usize, addr, SERIALIZATION_ALIGNMENT.toByteUnits());
            const offset = aligned_addr - addr;

            if (offset >= file_size_usize) {
                // File is too small to contain aligned data
                if (comptime @hasDecl(std.posix, "munmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .freestanding) {
                    std.posix.munmap(result);
                }
                const data = try readFromFile(allocator, file_path, filesystem);
                return CacheData{ .allocated = data };
            }

            const aligned_ptr = @as([*]align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8, @ptrFromInt(aligned_addr));
            const aligned_len = file_size_usize - offset;

            return CacheData{
                .mapped = .{
                    .ptr = aligned_ptr,
                    .len = aligned_len,
                    .unaligned_ptr = unaligned_ptr,
                    .unaligned_len = file_size_usize,
                },
            };
        } else {
            // Platform doesn't support mmap, use regular file reading
            const data = try readFromFile(allocator, file_path, filesystem);
            return CacheData{ .allocated = data };
        }
    }
};

/// Diagnostic information about a cache
pub const Diagnostics = struct {
    total_size: u32,
    header_size: u32,
    data_size: u32,
};
