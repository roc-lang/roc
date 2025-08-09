//! Module cache for Roc files
//!
//! This module provides memory-mapped caching for compiled Roc modules,
//! allowing fast serialization and deserialization of ModuleEnv and CIR data.

const std = @import("std");
const base = @import("base");
const Can = @import("can");
const collections = @import("collections");
const types = @import("types");
const parse = @import("parse");
const compile = @import("compile");
const serialization = @import("serialization");
const SExprTree = base.SExprTree;
const fs_mod = @import("fs");
const Filesystem = fs_mod.Filesystem;

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
const CACHE_VERSION: u32 = 1;

/// Cache header that gets written to disk before the cached data
pub const Header = struct {
    /// Magic number for validation
    magic: u32,

    /// Version for compatibility checking
    version: u32,

    /// Total size of the data section (excluding this header)
    data_size: u32,

    /// Diagnostic counts for accurate reporting when loading from cache
    error_count: u32,
    warning_count: u32,

    /// Padding to ensure alignment
    _padding: [12]u8 = [_]u8{0} ** 12,

    /// Error specific to initializing a Header from bytes
    pub const InitError = error{
        PartialRead,
        InvalidMagic,
        InvalidVersion,
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

        // Validate magic and version
        if (header.magic != CACHE_MAGIC) return InitError.InvalidMagic;
        if (header.version != CACHE_VERSION) return InitError.InvalidVersion;

        return header;
    }
};

/// Memory-mapped cache that can be read directly from disk
pub const CacheModule = struct {
    header: *const Header,
    data: []align(SERIALIZATION_ALIGNMENT) const u8,

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
    ) ![]align(SERIALIZATION_ALIGNMENT) u8 {
        const CompactWriter = serialization.CompactWriter;

        // Create CompactWriter
        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
        };

        // Allocate space for ModuleEnv.Serialized
        const env_ptr = try writer.appendAlloc(arena_allocator, ModuleEnv);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));

        // Serialize the ModuleEnv
        try serialized_ptr.serialize(module_env, arena_allocator, &writer);

        // Get the total size
        const total_data_size = writer.total_bytes;

        // Allocate cache_data for header + data
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const total_size = header_size + total_data_size;
        const cache_data = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, total_size);
        errdefer allocator.free(cache_data);

        // Initialize header
        const header = @as(*Header, @ptrCast(cache_data.ptr));
        header.* = Header{
            .magic = CACHE_MAGIC,
            .version = CACHE_VERSION,
            .data_size = @intCast(total_data_size),
            .error_count = error_count,
            .warning_count = warning_count,
            ._padding = [_]u8{0} ** 12,
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
    pub fn fromMappedMemory(mapped_data: []align(SERIALIZATION_ALIGNMENT) const u8) !CacheModule {
        if (mapped_data.len < @sizeOf(Header)) {
            return error.BufferTooSmall;
        }

        const header = @as(*const Header, @ptrCast(mapped_data.ptr));

        // Validate magic number and version
        if (header.magic != CACHE_MAGIC) return error.InvalidMagicNumber;
        if (header.version != CACHE_VERSION) return error.InvalidVersion;

        // Validate data size
        const expected_total_size = @sizeOf(Header) + header.data_size;
        if (mapped_data.len < expected_total_size) return error.BufferTooSmall;

        // Get data section (must be aligned)
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const data = mapped_data[header_size .. header_size + header.data_size];

        return CacheModule{
            .header = header,
            .data = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(data)),
        };
    }

    /// Restore ModuleEnv from the cached data
    /// IMPORTANT: This expects source to remain valid for the lifetime of the restored ModuleEnv.
    pub fn restore(self: *const CacheModule, allocator: Allocator, module_name: []const u8, source: []const u8) !*ModuleEnv {
        // The entire data section contains the serialized ModuleEnv
        const serialized_data = self.data;

        // The ModuleEnv.Serialized should be at the beginning of the data
        if (serialized_data.len < @sizeOf(ModuleEnv)) {
            return error.BufferTooSmall;
        }

        // Get pointer to the serialized ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(@constCast(serialized_data.ptr))));

        // Calculate the offset from the beginning of the serialized data
        const buffer_start = @intFromPtr(serialized_data.ptr);
        const offset = @as(i64, @intCast(buffer_start));

        // Deserialize the ModuleEnv
        const module_env_ptr: *ModuleEnv = deserialized_ptr.deserialize(offset, allocator, source, module_name);

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

/// Diagnostic information about a cache
pub const Diagnostics = struct {
    total_size: u32,
    header_size: u32,
    data_size: u32,
};

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
        \\foo = |num|
        \\    when num is
        \\        42 -> "forty-two"
        \\        _ -> Num.toStr num
        \\
    ;

    // Parse the source
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    try module_env.initCIRFields(gpa, "TestModule");
    // CIR is now just an alias for ModuleEnv, so use module_env directly
    const cir = &module_env;

    // Parse and canonicalize
    var ast = try parse(&module_env);
    defer ast.deinit(gpa);

    var czer = try Can.init(cir, &ast, null);
    defer czer
        .deinit();
    try czer
        .canonicalizeFile();

    // Generate original S-expression for comparison
    var original_tree = SExprTree.init(gpa);
    defer original_tree.deinit();
    try module_env.pushToSExprTree(null, &original_tree);

    var original_sexpr = std.ArrayList(u8).init(gpa);
    defer original_sexpr.deinit();
    try original_tree.toStringPretty(original_sexpr.writer().any());

    // Create arena for serialization
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, arena.allocator(), &module_env, cir, 0, 0);
    defer gpa.free(cache_data);

    // Load cache
    var cache = try CacheModule.fromMappedMemory(cache_data);

    // Validate cache
    try cache.validate();

    // Restore ModuleEnv
    // Duplicate source since restore takes ownership
    const restored_env = try cache.restore(gpa, "TestModule", source);
    // Note: restored_env points to data within the cache, so we don't free it

    // Generate S-expression from restored ModuleEnv
    var restored_tree = SExprTree.init(gpa);
    defer restored_tree.deinit();

    try restored_env.pushToSExprTree(null, &restored_tree);

    var restored_sexpr = std.ArrayList(u8).init(gpa);
    defer restored_sexpr.deinit();

    try restored_tree.toStringPretty(restored_sexpr.writer().any());

    // Verify round-trip integrity
    try std.testing.expect(std.mem.eql(u8, original_sexpr.items, restored_sexpr.items));

    // Get diagnostics
    const diagnostics = cache.getDiagnostics();
    try std.testing.expect(diagnostics.total_size > 0);
}

test "cache filesystem roundtrip with in-memory storage" {
    const gpa = std.testing.allocator;

    // Real Roc module source for comprehensive testing
    const source =
        \\module [foo]
        \\
        \\foo : U64 -> Str
        \\foo = |num| num.to_str()
    ;

    // Parse the source
    var module_env = try ModuleEnv.init(gpa, source);
    defer module_env.deinit();

    try module_env.initCIRFields(gpa, "TestModule");
    // CIR is now just an alias for ModuleEnv, so use module_env directly
    const cir = &module_env;

    // Parse and canonicalize
    var ast = try parse.parse(&module_env);
    defer ast.deinit(gpa);

    var czer = try Can.init(cir, &ast, null);
    defer czer
        .deinit();
    try czer
        .canonicalizeFile();

    // Generate original S-expression for comparison
    var original_tree = SExprTree.init(gpa);
    defer original_tree.deinit();
    try module_env.pushToSExprTree(null, &original_tree);

    var original_sexpr = std.ArrayList(u8).init(gpa);
    defer original_sexpr.deinit();
    try original_tree.toStringPretty(original_sexpr.writer().any());

    // Create arena for serialization
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, arena.allocator(), &module_env, cir, 0, 0);
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
    // Don't free immediately - the restored ModuleEnv references this data

    // Verify the read data matches the original
    try std.testing.expectEqualSlices(u8, cache_data, read_cache_data);

    // Load and validate the cache from the roundtrip data
    var roundtrip_cache = try CacheModule.fromMappedMemory(read_cache_data);
    try roundtrip_cache.validate();

    // Restore from the roundtrip cache
    // Duplicate source since restore takes ownership
    const restored_env = try roundtrip_cache.restore(gpa, "TestModule", source);
    // Note: restored_env points to data within the cache, so we don't free it

    // Generate S-expression from restored ModuleEnv
    var restored_tree = SExprTree.init(gpa);
    defer restored_tree.deinit();

    try restored_env.pushToSExprTree(null, &restored_tree);

    var restored_sexpr = std.ArrayList(u8).init(gpa);
    defer restored_sexpr.deinit();

    try restored_tree.toStringPretty(restored_sexpr.writer().any());

    // Verify complete roundtrip integrity
    try std.testing.expect(std.mem.eql(u8, original_sexpr.items, restored_sexpr.items));

    // Get diagnostics to ensure they're preserved
    const diagnostics = roundtrip_cache.getDiagnostics();
    try std.testing.expect(diagnostics.total_size > 0);

    // Free the cache data after we're done with the ModuleEnv
    gpa.free(read_cache_data);
}
