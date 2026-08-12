//! Module cache for Roc files
//!
//! This module provides memory-mapped caching for compiled Roc modules,
//! allowing fast serialization and deserialization of ModuleEnv and CIR data.

const std = @import("std");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const ctx_mod = @import("ctx");

const Constants = @import("cache_config.zig").Constants;
const CoreCtx = ctx_mod.CoreCtx;

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;

/// Magic number for cache validation
const CACHE_MAGIC: u32 = 0x524F4343; // "ROCC" in ASCII

/// The cache header's layout-version hash, computed at comptime. It changes when
/// the struct layout changes, enabling automatic cache invalidation. Delegates to
/// the shared `check.layoutVersionHash`, which recurses into nested aggregates and
/// serialized element layouts, so a change nested inside a field's type also
/// invalidates the cache. `cache_version` is a manual discriminant for semantic
/// changes the structural walk cannot observe.
fn computeVersionHash(comptime StructType: type, comptime cache_version: u32) [32]u8 {
    return check.layoutVersionHash(StructType, cache_version);
}

/// Version hash of ModuleEnv.Serialized computed at comptime
pub const MODULE_ENV_VERSION_HASH: [32]u8 = computeVersionHash(ModuleEnv.Serialized, Constants.CACHE_VERSION);

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

    /// The 16-byte alignment every serialized cache buffer requires.
    pub const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

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
    ) Allocator.Error![]align(SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
        const CompactWriter = collections.CompactWriter;

        // Create CompactWriter
        var writer = CompactWriter.init();

        // Allocate space for ModuleEnv.Serialized
        const serialized_ptr = try writer.appendAlloc(arena_allocator, ModuleEnv.Serialized);

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
    pub fn fromMappedMemory(mapped_data: []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8) (Allocator.Error || error{ BufferTooSmall, InvalidMagicNumber, CacheVersionHashMismatch })!CacheModule {
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
    pub fn restore(self: *const CacheModule, allocator: Allocator, module_name: []const u8, source: []const u8) (Allocator.Error || error{ BufferTooSmall, CorruptSerializedModuleEnv })!*ModuleEnv {
        // The entire data section contains the serialized ModuleEnv
        const serialized_data = self.data;

        // The ModuleEnv.Serialized should be at the beginning of the data
        // Note: Check against Serialized size, not ModuleEnv size, since we're deserializing from Serialized format
        if (serialized_data.len < @sizeOf(ModuleEnv.Serialized)) {
            return error.BufferTooSmall;
        }

        // Get pointer to the serialized ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(@constCast(serialized_data.ptr))));
        deserialized_ptr.validate(serialized_data.len) catch return error.CorruptSerializedModuleEnv;

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
    pub fn validate(self: *const CacheModule) Allocator.Error!void {
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
    ) Allocator.Error![]align(SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
        const file_data = try filesystem.readFile(file_path, allocator);
        defer allocator.free(file_data);

        const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, file_data.len);
        @memcpy(buffer, file_data);

        return buffer;
    }

    /// Tagged union to represent cache data that can be either memory-mapped or heap-allocated
    pub const CacheData = union(enum) {
        mapped: CoreCtx.MappedFile,
        allocated: []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8,

        pub fn data(self: CacheData) []align(SERIALIZATION_ALIGNMENT.toByteUnits()) const u8 {
            return switch (self) {
                .mapped => |m| blk: {
                    // Mappings are page-aligned, which satisfies the 16-byte
                    // serialization alignment.
                    comptime std.debug.assert(std.heap.page_size_min >= SERIALIZATION_ALIGNMENT.toByteUnits());
                    break :blk @alignCast(m.bytes());
                },
                .allocated => |a| a,
            };
        }

        pub fn deinit(self: CacheData, allocator: Allocator) void {
            switch (self) {
                .mapped => |m| m.unmap(),
                .allocated => |a| allocator.free(a),
            }
        }
    };

    /// Load a cache file, memory-mapping it when `roc_ctx` can map files and
    /// reading it onto the heap otherwise.
    ///
    /// The mapping is copy-on-write (`CoreCtx.mapFilePrivate`), so callers may
    /// write into the returned bytes (for example, relocating serialized
    /// pointers in place) without those writes ever reaching the cache file on
    /// disk. If a caller keeps references into the mapped bytes, the mapping
    /// must outlive them; free it through `CacheData.deinit`, which unmaps a
    /// `.mapped` value and never routes it through the heap allocator.
    ///
    /// An empty file, a mapping error, or a `roc_ctx` that cannot map files (a
    /// virtual filesystem, a target without `mmap`) uses the heap-read path via
    /// `filesystem.readFile`, so a mapping failure never fails the load.
    pub fn readFromFileMapped(
        allocator: Allocator,
        roc_ctx: CoreCtx,
        file_path: []const u8,
        filesystem: anytype,
    ) Allocator.Error!CacheData {
        if (tryMapCacheFile(roc_ctx, file_path)) |mapped| {
            return mapped;
        }

        const data = try readFromFile(allocator, file_path, filesystem);
        return CacheData{ .allocated = data };
    }

    /// Copy-on-write map `file_path` into a `CacheData.mapped` value, or return
    /// `null` when `roc_ctx` cannot map it (a virtual filesystem, a target without
    /// `mmap`, or a missing, empty, or kernel-refused file). A `null` result is
    /// the caller's cue to read the file onto the heap.
    pub fn tryMapCacheFile(roc_ctx: CoreCtx, file_path: []const u8) ?CacheData {
        const mapped = roc_ctx.mapFilePrivate(file_path) orelse return null;
        return CacheData{ .mapped = mapped };
    }
};

/// Diagnostic information about a cache
pub const Diagnostics = struct {
    total_size: u32,
    header_size: u32,
    data_size: u32,
};

test "MODULE_ENV_VERSION_HASH golden value" {
    // Tripwire: an *accidental* change to `ModuleEnv.Serialized`'s layout silently
    // invalidates every on-disk module cache. It flips this hash and fails here. On
    // an *intentional* layout change, bump `Constants.CACHE_VERSION` and replace the
    // golden bytes below with the ones this assertion prints.
    const golden: [32]u8 = .{
        0xD5, 0xC9, 0x13, 0x8B, 0xC4, 0x2B, 0xCA, 0x5D, 0x21, 0xC2, 0x62, 0xAF, 0x88, 0xAA, 0x36, 0xAC,
        0x71, 0xBC, 0xBC, 0x72, 0xC2, 0x4C, 0xF4, 0xAA, 0x94, 0xBA, 0x7D, 0x8A, 0x72, 0x6F, 0xF5, 0x84,
    };
    try std.testing.expectEqualSlices(u8, &golden, &MODULE_ENV_VERSION_HASH);
}

test "readFromFileMapped memory-maps a cache file copy-on-write" {
    if (comptime !CoreCtx.can_map_files) return;

    const testing = std.testing;
    const roc_ctx = CoreCtx.default(testing.allocator, testing.allocator, testing.io);

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = try tmp_dir.dir.realPathFileAlloc(testing.io, ".", testing.allocator);
    defer testing.allocator.free(tmp_path);
    const path = try std.fs.path.join(testing.allocator, &.{ tmp_path, "mmap-roundtrip.bin" });
    defer testing.allocator.free(path);

    // A page of deterministic bytes so the mapped data spans the whole page.
    var payload: [4096]u8 = undefined;
    for (&payload, 0..) |*b, i| b.* = @truncate(i);
    try roc_ctx.writeFile(path, &payload);

    // A filesystem whose read errors, proving the heap-read path is not taken when
    // the mapping succeeds.
    const StubFs = struct {
        fn readFile(_: @This(), _: []const u8, _: Allocator) Allocator.Error![]u8 {
            return error.OutOfMemory;
        }
    };

    const cache_data = try CacheModule.readFromFileMapped(testing.allocator, roc_ctx, path, StubFs{});
    defer cache_data.deinit(testing.allocator);

    try testing.expect(std.meta.activeTag(cache_data) == .mapped);
    try testing.expectEqualSlices(u8, &payload, cache_data.data());

    // Copy-on-write: mutating the mapping (as relocation does) must not reach disk.
    const mutable: [*]u8 = @constCast(cache_data.data().ptr);
    mutable[0] +%= 1;
    mutable[payload.len - 1] +%= 1;

    const reread = try roc_ctx.readFile(path, testing.allocator);
    defer testing.allocator.free(reread);
    try testing.expectEqualSlices(u8, &payload, reread);
}

test "readFromFileMapped heap-reads when the path is not a real on-disk file" {
    const testing = std.testing;
    const roc_ctx = CoreCtx.default(testing.allocator, testing.allocator, testing.io);

    // Opening this path fails to map, so the load drops to the heap-read path;
    // on targets that cannot map files, the heap-read path is the only path.
    const HeapFs = struct {
        payload: []const u8,
        fn readFile(self: @This(), _: []const u8, allocator: Allocator) Allocator.Error![]u8 {
            return allocator.dupe(u8, self.payload);
        }
    };

    const payload = "roc cache heap-read payload bytes";
    const cache_data = try CacheModule.readFromFileMapped(
        testing.allocator,
        roc_ctx,
        "/roc-nonexistent/cache/entry.bin",
        HeapFs{ .payload = payload },
    );
    defer cache_data.deinit(testing.allocator);

    try testing.expect(std.meta.activeTag(cache_data) == .allocated);
    try testing.expectEqualSlices(u8, payload, cache_data.data());
}
