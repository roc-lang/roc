//! Module cache for Roc files using iovec serialization and relocation-based deserialization
//!
//! This module provides zero-copy caching for compiled Roc modules,
//! using iovecs for serialization and relocations for deserialization.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const canonicalize = @import("../check/canonicalize.zig");
const collections = @import("collections");
const types = @import("types");
const parse = @import("parse");
const compile = @import("compile");
const SExprTree = base.SExprTree;
const Filesystem = @import("../fs/Filesystem.zig");
const serialization = @import("serialization");
const IovecWriter = serialization.IovecWriter;

const SERIALIZATION_ALIGNMENT = 16;

const Allocator = std.mem.Allocator;
const TypeStore = types.Store;
const ModuleEnv = compile.ModuleEnv;
const Node = ModuleEnv.Node;
const NodeStore = ModuleEnv.NodeStore;
const SafeList = collections.SafeList;
const SafeStringHashMap = collections.SafeStringHashMap;

/// Magic number for cache validation
const CACHE_MAGIC: u32 = 0x524F4343; // "ROCC" in ASCII
const CACHE_VERSION: u32 = 2; // Increment version for iovec-based serialization

/// Cache header for zero-copy deserialization
pub const Header = extern struct {
    /// Magic number for validation
    magic: u32,

    /// Version for compatibility checking
    version: u32,

    /// Platform info for compatibility
    pointer_size: u8,
    endianness: u8,
    _reserved: u16 = 0,

    /// Total size of the data section (excluding this header)
    data_size: u64,

    /// Checksum of the data section
    checksum: u64,

    /// Offset to main structure in the data section
    module_env_offset: u64,

    /// Original memory base address for relocation calculation
    original_memory_base: u64,

    /// Diagnostic counts for accurate reporting when loading from cache
    error_count: u32,
    warning_count: u32,

    /// Fixed padding to ensure alignment (Header must be 80 bytes total for 16-byte alignment)
    _padding: [20]u8 = [_]u8{0} ** 20,

    /// Error specific to initializing a Header from bytes
    pub const InitError = error{
        PartialRead,
        InvalidMagic,
        InvalidVersion,
        ChecksumMismatch,
        IncompatiblePlatform,
    };

    /// Verify that the given buffer begins with a valid Header
    pub fn initFromBytes(buf: []align(@alignOf(Header)) u8) InitError!*Header {
        if (buf.len < @sizeOf(Header)) {
            return InitError.PartialRead;
        }

        const header = @as(*Header, @ptrCast(buf.ptr));
        // Use aligned header size to match how data is actually laid out
        const data_start = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const data_end = data_start + header.data_size;

        // The buffer might not contain complete data after the header
        if (buf.len < data_end) {
            return InitError.PartialRead;
        }

        // Validate magic and version
        if (header.magic != CACHE_MAGIC) return InitError.InvalidMagic;
        if (header.version != CACHE_VERSION) return InitError.InvalidVersion;

        // Validate platform compatibility
        if (header.pointer_size != @sizeOf(usize)) return InitError.IncompatiblePlatform;
        const expected_endian: u8 = if (@import("builtin").target.cpu.arch.endian() == .little) 0 else 1;
        if (header.endianness != expected_endian) return InitError.IncompatiblePlatform;

        return header;
    }
};

/// Memory-mapped cache that can be read directly from disk
pub const CacheModule = struct {
    header: *const Header,
    data: []align(SERIALIZATION_ALIGNMENT) const u8,

    /// Create a cache by serializing ModuleEnv and CIR data using iovec serialization
    pub fn create(
        allocator: Allocator,
        module_env: *const ModuleEnv,
        _: *const ModuleEnv, // ModuleEnv contains the canonical IR
        error_count: u32,
        warning_count: u32,
    ) ![]align(SERIALIZATION_ALIGNMENT) u8 {

        // Create iovec writer for gathering serialization data
        var writer = IovecWriter.init(allocator);
        defer writer.deinit();

        // Serialize ModuleEnv (which now contains CIR) using iovecs
        const module_env_offset = try module_env.appendToIovecs(&writer);

        // Serialize the data section
        const data_buffer = try writer.serialize(allocator);
        defer allocator.free(data_buffer);

        // Calculate total size: header + data
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const total_size = header_size + data_buffer.len;

        // Create final aligned buffer
        const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, total_size);

        // Create the header
        const header = Header{
            .magic = CACHE_MAGIC,
            .version = CACHE_VERSION,
            .pointer_size = @sizeOf(usize),
            .endianness = if (@import("builtin").target.cpu.arch.endian() == .little) 0 else 1,
            .data_size = data_buffer.len,
            .checksum = calculateChecksum(data_buffer),
            .module_env_offset = module_env_offset,
            .original_memory_base = 0, // All offsets are relative to writer buffer which starts at 0
            .error_count = error_count,
            .warning_count = warning_count,
        };

        // Write header at the beginning
        @memcpy(buffer[0..@sizeOf(Header)], std.mem.asBytes(&header));

        // Zero-fill padding between header and data
        @memset(buffer[@sizeOf(Header)..header_size], 0);

        // Write data section
        @memcpy(buffer[header_size..], data_buffer);

        return buffer;
    }

    /// Load a cache from memory-mapped data using iovec serialization
    pub fn fromMappedMemory(mapped_data: []align(SERIALIZATION_ALIGNMENT) const u8) !CacheModule {
        if (mapped_data.len < @sizeOf(Header)) {
            return error.BufferTooSmall;
        }

        const header = @as(*const Header, @ptrCast(mapped_data.ptr));

        // Validate header - pass the entire buffer so it can validate data is present
        _ = try Header.initFromBytes(@constCast(mapped_data));

        // Get data section (must be aligned)
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const data = mapped_data[header_size .. header_size + header.data_size];

        // Validate checksum
        const calculated_checksum = calculateChecksum(data);
        if (header.checksum != calculated_checksum) return error.ChecksumMismatch;

        return CacheModule{
            .header = header,
            .data = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(data)),
        };
    }

    /// Restored data from cache using relocations
    pub const RestoredData = struct {
        module_env: ModuleEnv,
        // CIR is now stored inside module_env as *anyopaque fields

        // For backward compatibility, provide a getter
        pub fn cir(self: *const RestoredData) *const ModuleEnv {
            return &self.module_env;
        }
    };

    /// Restore ModuleEnv and CIR from the cached data using relocations
    /// IMPORTANT: This expects source to remain valid for the lifetime of the restored ModuleEnv.
    pub fn restore(self: *const CacheModule, allocator: Allocator, module_name: []const u8, source: []const u8) !RestoredData {
        _ = allocator; // Not needed for relocation-based deserialization
        _ = module_name; // Module name is stored in the cached data
        _ = source; // Source is stored in the cached data

        // Validate cache data is present
        if (self.header.data_size == 0) {
            return error.EmptyCache;
        }

        // Verify offset is valid
        if (self.header.module_env_offset >= self.header.data_size) {
            return error.InvalidCacheOffsets;
        }

        // Get the data section
        const data_without_footer = self.data;

        // Calculate relocation offset based on where data is now vs where it was during serialization
        // During serialization, all offsets were relative to IovecWriter buffer starting at 0
        // Now we need to relocate them to point to the actual data location
        const current_data_base_addr = @intFromPtr(data_without_footer.ptr);
        const relocation_offset = @as(isize, @intCast(current_data_base_addr)) - @as(isize, @intCast(self.header.original_memory_base));

        // Copy ModuleEnv bytes from the correct offset to properly aligned memory
        var module_env: ModuleEnv = undefined;
        const module_env_start = self.header.module_env_offset;
        const module_env_end = module_env_start + @sizeOf(ModuleEnv);
        if (module_env_end > self.header.data_size) {
            return error.CacheTooSmall;
        }
        @memcpy(std.mem.asBytes(&module_env), data_without_footer[module_env_start..module_env_end]);

        // Apply relocations to fix up all the offset-based pointers
        module_env.relocate(relocation_offset);

        // Note: store, diagnostics, external_decls, and imports are not serialized
        // They are heap-allocated during canonicalization and should be recreated
        // after cache restoration if needed

        return RestoredData{
            .module_env = module_env,
        };
    }

    /// Memory-mapped cache result that manages the mapped memory
    pub const MappedCache = struct {
        cache: CacheModule,
        cache_data: CacheData,

        pub fn deinit(self: *const MappedCache, allocator: Allocator) void {
            // Use the CacheData's proper cleanup that handles both mmap and alloc
            self.cache_data.deinit(allocator);
        }

        pub fn data(self: *const MappedCache) []align(SERIALIZATION_ALIGNMENT) const u8 {
            return self.cache_data.data();
        }

        /// Create a MappedCache from CacheData
        pub fn fromCacheData(cache_data: CacheData) !MappedCache {
            const cache = try CacheModule.fromMappedMemory(cache_data.data());
            return MappedCache{
                .cache = cache,
                .cache_data = cache_data,
            };
        }
    };

    /// Get diagnostic information about the cache
    pub fn getDiagnostics(self: *const CacheModule) Diagnostics {
        // Since we use iovec serialization without individual component size tracking,
        // we can estimate component sizes by analyzing the serialized ModuleEnv structure
        const component_sizes = self.estimateComponentSizes();

        return Diagnostics{
            .total_size = @sizeOf(Header) + @as(u32, @intCast(self.header.data_size)),
            .header_size = @sizeOf(Header),
            .data_size = @as(u32, @intCast(self.header.data_size)),
            .checksum = @as(u32, @truncate(self.header.checksum)),
            .component_sizes = component_sizes,
        };
    }

    /// Estimate component sizes based on the serialized data
    /// This provides approximate sizes since exact boundaries aren't stored in the current format
    fn estimateComponentSizes(self: *const CacheModule) ComponentSizes {
        // Since the iovec serialization doesn't store individual component sizes,
        // we provide rough estimates based on typical component relationships
        const data_size = @as(u32, @intCast(self.header.data_size));
        const module_env_size = @sizeOf(ModuleEnv);

        // Reserve space for validation footer (16 bytes)
        const content_size = if (data_size > 16) data_size - 16 else data_size;
        const available_size = if (content_size > module_env_size) content_size - module_env_size else 0;

        // Distribute estimated sizes based on typical usage patterns
        // These are rough estimates since exact sizes would require format changes
        const base_estimate = available_size / 8; // Divide among 8 components

        return .{
            .node_store = base_estimate, // Usually the largest component
            .string_store = base_estimate / 2, // Moderate size
            .ident_ids_for_slicing = base_estimate / 4, // Small arrays
            .ident_store = base_estimate / 2, // Moderate size
            .line_starts = base_estimate / 4, // Small arrays
            .types_store = base_estimate, // Can be large
            .exposed_items = base_estimate / 8, // Usually small
            .external_decls = base_estimate / 4, // Variable size
        };
    }

    /// Validate the cache structure and integrity (intended for debug builds and tests)
    pub fn validate(self: *const CacheModule) !void {
        // Basic validation - check that offsets are within bounds
        if (self.header.module_env_offset >= self.header.data_size) {
            return error.ComponentOutOfBounds;
        }
    }

    /// Convenience functions for reading/writing cache files
    pub fn writeToFile(
        allocator: Allocator,
        cache_data: []const u8,
        file_path: []const u8,
        filesystem: anytype,
    ) !void {
        _ = allocator;
        try filesystem.writeFile(file_path, cache_data);
    }

    /// Convenience function for reading cache files
    pub fn readFromFile(
        allocator: Allocator,
        file_path: []const u8,
        filesystem: anytype,
    ) ![]align(SERIALIZATION_ALIGNMENT) u8 {
        const file_data = try filesystem.readFile(file_path, allocator);
        defer allocator.free(file_data);

        const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, file_data.len);
        @memcpy(buffer, file_data);

        return buffer;
    }

    /// Tagged union to represent cache data that can be either memory-mapped or heap-allocated
    pub const CacheData = union(enum) {
        mapped: struct {
            ptr: [*]align(SERIALIZATION_ALIGNMENT) const u8,
            len: usize,
            unaligned_ptr: [*]const u8,
            unaligned_len: usize,
        },
        allocated: []align(SERIALIZATION_ALIGNMENT) const u8,

        pub fn data(self: CacheData) []align(SERIALIZATION_ALIGNMENT) const u8 {
            return switch (self) {
                .mapped => |m| m.ptr[0..m.len],
                .allocated => |a| a,
            };
        }

        pub fn deinit(self: CacheData, allocator: Allocator) void {
            switch (self) {
                .mapped => |m| {
                    // Use the unaligned pointer for munmap
                    if (comptime @hasDecl(std.posix, "munmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .wasi) {
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
        // Try to use memory mapping on supported platforms
        if (comptime @hasDecl(std.posix, "mmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .wasi) {
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
            const aligned_addr = std.mem.alignForward(usize, addr, SERIALIZATION_ALIGNMENT);
            const offset = aligned_addr - addr;

            if (offset >= file_size_usize) {
                // File is too small to contain aligned data
                if (comptime @hasDecl(std.posix, "munmap") and @import("builtin").target.os.tag != .windows and @import("builtin").target.os.tag != .wasi) {
                    std.posix.munmap(result);
                }
                const data = try readFromFile(allocator, file_path, filesystem);
                return CacheData{ .allocated = data };
            }

            const aligned_ptr = @as([*]align(SERIALIZATION_ALIGNMENT) const u8, @ptrFromInt(aligned_addr));
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

/// Component size estimates for diagnostics
pub const ComponentSizes = struct {
    node_store: u32,
    string_store: u32,
    ident_ids_for_slicing: u32,
    ident_store: u32,
    line_starts: u32,
    types_store: u32,
    exposed_items: u32,
    external_decls: u32,
};

/// Diagnostic information about a cache
pub const Diagnostics = struct {
    total_size: u32,
    header_size: u32,
    data_size: u32,
    checksum: u32,
    component_sizes: ComponentSizes,
};

/// Simple checksum calculation
fn calculateChecksum(data: []const u8) u64 {
    return std.hash.Wyhash.hash(0, data);
}

test "Header alignment" {
    // Verify the header is properly aligned
    try std.testing.expect(@sizeOf(Header) % SERIALIZATION_ALIGNMENT == 0);
}

test "create and restore cache" {
    const gpa = std.testing.allocator;

    // Real Roc module source for comprehensive testing
    const source =
        \\module [foo]
        \\
        \\foo : U64 -> Str
        \\foo = |num| Num.toStr num
    ;

    // Create ModuleEnv
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Calculate line starts
    try module_env.calcLineStarts();

    // Freeze interners after adding all data
    module_env.freezeInterners();

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, &module_env, undefined, 0, 0);
    defer gpa.free(cache_data);

    // Load cache
    const cache = try CacheModule.fromMappedMemory(cache_data);

    // Validate structure
    try cache.validate();

    // Restore data
    const restored = try cache.restore(gpa, "test", source);

    // Verify restoration
    try std.testing.expectEqual(module_env.source.len, restored.module_env.source.len);
    try std.testing.expectEqualStrings(module_env.source, restored.module_env.source);
}

test "cache filesystem roundtrip with in-memory storage" {
    const gpa = std.testing.allocator;

    // Real Roc module source for comprehensive testing
    const source =
        \\module [foo]
        \\
        \\foo : U64 -> Str
        \\foo = |num| Num.toStr num
    ;

    // Create ModuleEnv
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    // Calculate line starts
    try module_env.calcLineStarts();

    // Parse and canonicalize
    var ast = try parse.parse(&module_env);
    defer ast.deinit(gpa);

    var canonicalizer = try canonicalize.init(&module_env, &ast, null);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    // Generate original S-expression for comparison
    var original_tree = SExprTree.init(gpa);
    defer original_tree.deinit();
    try module_env.pushToSExprTree(null, &original_tree);

    var original_sexpr = std.ArrayList(u8).init(gpa);
    defer original_sexpr.deinit();
    try original_tree.toStringPretty(original_sexpr.writer().any());

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, &module_env, undefined, 0, 0);
    defer gpa.free(cache_data);

    // In-memory file storage for comprehensive mock filesystem
    var file_storage = std.StringHashMap([]const u8).init(gpa);
    defer {
        var iterator = file_storage.iterator();
        while (iterator.next()) |entry| {
            gpa.free(entry.value_ptr.*);
        }
        file_storage.deinit();
    }

    // Create comprehensive mock filesystem with proper storage using static variables
    var filesystem = Filesystem.testing();

    const MockFS = struct {
        var storage: ?*std.StringHashMap([]const u8) = null;
        var allocator: ?Allocator = null;

        fn writeFile(path: []const u8, contents: []const u8) Filesystem.WriteError!void {
            const store = storage orelse return error.SystemResources;
            const alloc = allocator orelse return error.SystemResources;

            // Store a copy of the contents in our storage
            const stored_contents = alloc.dupe(u8, contents) catch return error.SystemResources;

            // Free existing content if path already exists
            if (store.get(path)) |existing| {
                alloc.free(existing);
            }

            // Store the new content
            store.put(path, stored_contents) catch {
                alloc.free(stored_contents);
                return error.SystemResources;
            };
        }

        fn readFile(path: []const u8, alloc: Allocator) Filesystem.ReadError![]const u8 {
            const store = storage orelse return error.FileNotFound;

            if (store.get(path)) |contents| {
                return alloc.dupe(u8, contents) catch return error.OutOfMemory;
            } else {
                return error.FileNotFound;
            }
        }
    };

    // Initialize the static variables
    MockFS.storage = &file_storage;
    MockFS.allocator = gpa;

    filesystem.writeFile = MockFS.writeFile;
    filesystem.readFile = MockFS.readFile;

    // Test full roundtrip: write cache to mock filesystem
    const test_path = "comprehensive_test_cache.bin";
    try CacheModule.writeToFile(gpa, cache_data, test_path, filesystem);

    // Verify the data was stored
    try std.testing.expect(file_storage.contains(test_path));

    // Read the cache back from mock filesystem
    const read_cache_data = try CacheModule.readFromFile(gpa, test_path, filesystem);
    defer gpa.free(read_cache_data);

    // Verify the read data matches the original
    try std.testing.expectEqualSlices(u8, cache_data, read_cache_data);

    // Load and validate the cache from the roundtrip data
    var roundtrip_cache = try CacheModule.fromMappedMemory(read_cache_data);
    if (builtin.mode == .Debug) {
        try roundtrip_cache.validate();
    }

    // Get diagnostics to ensure they're preserved
    const diagnostics = roundtrip_cache.getDiagnostics();
    try std.testing.expect(diagnostics.total_size > 0);
}
