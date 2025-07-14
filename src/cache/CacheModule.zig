//! Module cache for Roc files
//!
//! This module provides memory-mapped caching for compiled Roc modules,
//! allowing fast serialization and deserialization of ModuleEnv and CIR data.

const std = @import("std");
const base = @import("../base.zig");
const canonicalize = @import("../check/canonicalize.zig");
const collections = @import("../collections.zig");
const types = @import("../types.zig");
const parse = @import("../check/parse.zig").parse;
const SExprTree = @import("../base/SExprTree.zig");
const Filesystem = @import("../fs/Filesystem.zig");
const SERIALIZATION_ALIGNMENT = @import("../serialization/mod.zig").SERIALIZATION_ALIGNMENT;

const Allocator = std.mem.Allocator;
const TypeStore = types.Store;
const CIR = canonicalize.CIR;
const Node = CIR.Node;
const NodeStore = CIR.NodeStore;
const SafeList = collections.SafeList;
const SafeStringHashMap = collections.SafeStringHashMap;

/// Magic number for cache validation
const CACHE_MAGIC: u32 = 0x524F4343; // "ROCC" in ASCII
const CACHE_VERSION: u32 = 1;

/// Component metadata for locating data in the cache
const ComponentInfo = struct {
    offset: u32,
    length: u32,
};

/// Cache header that gets written to disk before the cached data
pub const Header = struct {
    /// Magic number for validation
    magic: u32,

    /// Version for compatibility checking
    version: u32,

    /// Total size of the data section (excluding this header)
    data_size: u32,

    // TODO implement this properly.. just stubbed out for now.
    // CRC32 checksum of the data section
    checksum: u32,

    /// Component locations in the data section
    node_store: ComponentInfo,
    string_store: ComponentInfo,
    ident_ids_for_slicing: ComponentInfo,
    ident_store: ComponentInfo,
    line_starts: ComponentInfo,
    types_store: ComponentInfo,
    exposed_by_str: ComponentInfo,
    exposed_nodes: ComponentInfo,
    external_decls: ComponentInfo,

    /// Spans can be stored directly since they're small
    all_defs: CIR.Def.Span,
    all_statements: CIR.Statement.Span,

    /// Diagnostic counts for accurate reporting when loading from cache
    error_count: u32,
    warning_count: u32,

    /// Fixed padding to ensure alignment
    _padding: [16]u8 = [_]u8{0} ** 16,

    /// Error specific to initializing a Header from bytes
    pub const InitError = error{
        PartialRead,
        InvalidMagic,
        InvalidVersion,
        ChecksumMismatch,
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

    /// Create a cache by serializing ModuleEnv and CIR data
    pub fn create(
        allocator: Allocator,
        module_env: *const base.ModuleEnv,
        cir: *const CIR,
        error_count: u32,
        warning_count: u32,
    ) ![]align(SERIALIZATION_ALIGNMENT) u8 {
        // Calculate component sizes
        const node_store_size = cir.store.serializedSize();
        const string_store_size = module_env.strings.serializedSize();
        const ident_ids_size = module_env.ident_ids_for_slicing.serializedSize();
        const ident_store_size = module_env.idents.serializedSize();
        const line_starts_size = module_env.line_starts.serializedSize();
        const types_store_size = module_env.types.serializedSize();
        const exposed_by_str_size = module_env.exposed_by_str.serializedSize();
        const exposed_nodes_size = module_env.exposed_nodes.serializedSize();
        const external_decls_size = cir.external_decls.serializedSize();

        // Calculate aligned offsets
        var offset: u32 = 0;

        // Ensure each offset is aligned to SERIALIZATION_ALIGNMENT
        const node_store_offset = offset;
        offset += @intCast(node_store_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const string_store_offset = offset;
        offset += @intCast(string_store_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const ident_ids_offset = offset;
        offset += @intCast(ident_ids_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const ident_store_offset = offset;
        offset += @intCast(ident_store_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const line_starts_offset = offset;
        offset += @intCast(line_starts_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const types_store_offset = offset;
        offset += @intCast(types_store_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const exposed_by_str_offset = offset;
        offset += @intCast(exposed_by_str_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const exposed_nodes_offset = offset;
        offset += @intCast(exposed_nodes_size);

        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));
        const external_decls_offset = offset;
        offset += @intCast(external_decls_size);
        offset = @intCast(std.mem.alignForward(usize, offset, SERIALIZATION_ALIGNMENT));

        const total_data_size = offset;

        // Allocate buffer for header + data
        const header_size = std.mem.alignForward(usize, @sizeOf(Header), SERIALIZATION_ALIGNMENT);
        const total_size = header_size + total_data_size;
        const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, total_size);

        // Zero-initialize buffer for proper CRC calculation
        @memset(buffer, 0);

        // Initialize header
        const header = @as(*Header, @ptrCast(buffer.ptr));
        header.* = Header{
            .magic = CACHE_MAGIC,
            .version = CACHE_VERSION,
            .data_size = total_data_size,
            .checksum = 0, // Will be calculated after data is written
            .node_store = .{ .offset = node_store_offset, .length = @intCast(node_store_size) },
            .string_store = .{ .offset = string_store_offset, .length = @intCast(string_store_size) },
            .ident_ids_for_slicing = .{ .offset = ident_ids_offset, .length = @intCast(ident_ids_size) },
            .ident_store = .{ .offset = ident_store_offset, .length = @intCast(ident_store_size) },
            .line_starts = .{ .offset = line_starts_offset, .length = @intCast(line_starts_size) },
            .types_store = .{ .offset = types_store_offset, .length = @intCast(types_store_size) },
            .exposed_by_str = .{ .offset = exposed_by_str_offset, .length = @intCast(exposed_by_str_size) },
            .exposed_nodes = .{ .offset = exposed_nodes_offset, .length = @intCast(exposed_nodes_size) },
            .external_decls = .{ .offset = external_decls_offset, .length = @intCast(external_decls_size) },
            .all_defs = cir.all_defs,
            .all_statements = cir.all_statements,
            .error_count = error_count,
            .warning_count = warning_count,
        };

        // Get data section (must be aligned)
        const data_section = @as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(buffer[header_size..]));

        // Assert all offsets are aligned (in debug mode)
        std.debug.assert(node_store_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(string_store_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(ident_ids_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(ident_store_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(line_starts_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(types_store_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(exposed_by_str_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(exposed_nodes_offset % SERIALIZATION_ALIGNMENT == 0);
        std.debug.assert(external_decls_offset % SERIALIZATION_ALIGNMENT == 0);

        // Serialize each component
        // Since we've ensured all offsets are aligned, we can safely alignCast the slices
        _ = try cir.store.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(data_section[node_store_offset .. node_store_offset + node_store_size])));
        _ = try module_env.strings.serializeInto(data_section[string_store_offset .. string_store_offset + string_store_size]);
        _ = try module_env.ident_ids_for_slicing.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(data_section[ident_ids_offset .. ident_ids_offset + ident_ids_size])));
        _ = try module_env.idents.serializeInto(data_section[ident_store_offset .. ident_store_offset + ident_store_size], allocator);
        _ = try module_env.line_starts.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(data_section[line_starts_offset .. line_starts_offset + line_starts_size])));
        _ = try module_env.types.serializeInto(data_section[types_store_offset .. types_store_offset + types_store_size], allocator);
        _ = try module_env.exposed_by_str.serializeInto(data_section[exposed_by_str_offset .. exposed_by_str_offset + exposed_by_str_size]);
        _ = try module_env.exposed_nodes.serializeInto(data_section[exposed_nodes_offset .. exposed_nodes_offset + exposed_nodes_size]);
        _ = try cir.external_decls.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(data_section[external_decls_offset .. external_decls_offset + external_decls_size])));

        // TODO Calculate and store checksum
        // header.checksum = std.hash.Crc32.hash(data_section[0..total_data_size]);

        return buffer;
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

        // TODO Validate checksum
        // const calculated_checksum = std.hash.Crc32.hash(data);
        // if (header.checksum != calculated_checksum) return error.ChecksumMismatch;

        return CacheModule{
            .header = header,
            .data = @as([]align(SERIALIZATION_ALIGNMENT) const u8, @alignCast(data)),
        };
    }

    /// Restored data from cache
    pub const RestoredData = struct {
        module_env: base.ModuleEnv,
        cir: CIR,
    };

    /// Restore ModuleEnv and CIR from the cached data
    /// IMPORTANT: This function takes ownership of `source`.
    /// The caller must not free it after calling this function.
    pub fn restore(self: *const CacheModule, allocator: Allocator, module_name: []const u8, source: []const u8) !RestoredData {
        // Deserialize each component
        const node_store = try NodeStore.deserializeFrom(
            @as([]align(@alignOf(Node)) const u8, @alignCast(self.getComponentData(.node_store))),
            allocator,
        );

        const strings = try base.StringLiteral.Store.deserializeFrom(self.getComponentData(.string_store), allocator);
        const ident_ids_for_slicing = try SafeList(base.Ident.Idx).deserializeFrom(
            @as([]align(@alignOf(base.Ident.Idx)) const u8, @alignCast(self.getComponentData(.ident_ids_for_slicing))),
            allocator,
        );
        const idents = try base.Ident.Store.deserializeFrom(self.getComponentData(.ident_store), allocator);
        const line_starts = try SafeList(u32).deserializeFrom(
            @as([]align(@alignOf(u32)) const u8, @alignCast(self.getComponentData(.line_starts))),
            allocator,
        );
        const types_store = try TypeStore.deserializeFrom(self.getComponentData(.types_store), allocator);
        const exposed_by_str = try SafeStringHashMap(void).deserializeFrom(self.getComponentData(.exposed_by_str), allocator);
        const exposed_nodes = try SafeStringHashMap(u16).deserializeFrom(self.getComponentData(.exposed_nodes), allocator);

        // Create ModuleEnv from deserialized components
        var module_env = base.ModuleEnv{
            .gpa = allocator,
            .idents = idents,
            .ident_ids_for_slicing = ident_ids_for_slicing,
            .strings = strings,
            .types = types_store,
            .exposed_by_str = exposed_by_str,
            .exposed_nodes = exposed_nodes,
            .line_starts = line_starts,
            .source = source,
        };
        errdefer module_env.deinit();

        // Deserialize external_decls
        const external_decls = try CIR.ExternalDecl.SafeList.deserializeFrom(
            @as([]align(@alignOf(CIR.ExternalDecl)) const u8, @alignCast(self.getComponentData(.external_decls))),
            allocator,
        );

        // Create result struct
        var result = RestoredData{
            .module_env = module_env,
            .cir = CIR{
                .env = undefined, // Will be set below
                .store = node_store,
                .temp_source_for_sexpr = null,
                .all_defs = self.header.all_defs,
                .all_statements = self.header.all_statements,
                .external_decls = external_decls,
                .imports = CIR.Import.Store.init(),
                .module_name = module_name,
            },
        };

        // Fix env pointer to point to the correct module_env location
        result.cir.env = &result.module_env;

        return result;
    }

    /// Get the raw data for a specific component
    pub fn getComponentData(self: *const CacheModule, comptime component: ComponentType) []const u8 {
        const info = switch (component) {
            .node_store => self.header.node_store,
            .string_store => self.header.string_store,
            .ident_ids_for_slicing => self.header.ident_ids_for_slicing,
            .ident_store => self.header.ident_store,
            .line_starts => self.header.line_starts,
            .types_store => self.header.types_store,
            .exposed_by_str => self.header.exposed_by_str,
            .exposed_nodes => self.header.exposed_nodes,
            .external_decls => self.header.external_decls,
        };
        return self.data[info.offset .. info.offset + info.length];
    }

    /// Get diagnostic information about the cache
    pub fn getDiagnostics(self: *const CacheModule) Diagnostics {
        return Diagnostics{
            .total_size = @sizeOf(Header) + self.header.data_size,
            .header_size = @sizeOf(Header),
            .data_size = self.header.data_size,
            .checksum = self.header.checksum,
            .component_sizes = .{
                .node_store = self.header.node_store.length,
                .string_store = self.header.string_store.length,
                .ident_ids_for_slicing = self.header.ident_ids_for_slicing.length,
                .ident_store = self.header.ident_store.length,
                .line_starts = self.header.line_starts.length,
                .types_store = self.header.types_store.length,
                .exposed_by_str = self.header.exposed_by_str.length,
                .exposed_nodes = self.header.exposed_nodes.length,
                .external_decls = self.header.external_decls.length,
            },
        };
    }

    /// Validate the cache structure and integrity
    pub fn validate(self: *const CacheModule) !void {
        // Validate component offsets are within bounds
        inline for (std.meta.fields(ComponentType)) |field| {
            const component = @field(ComponentType, field.name);
            const info = switch (component) {
                .node_store => self.header.node_store,
                .string_store => self.header.string_store,
                .ident_ids_for_slicing => self.header.ident_ids_for_slicing,
                .ident_store => self.header.ident_store,
                .line_starts => self.header.line_starts,
                .types_store => self.header.types_store,
                .exposed_by_str => self.header.exposed_by_str,
                .exposed_nodes => self.header.exposed_nodes,
                .external_decls => self.header.external_decls,
            };

            if (info.offset + info.length > self.header.data_size) {
                return error.ComponentOutOfBounds;
            }
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

/// Enum for component types
const ComponentType = enum {
    node_store,
    string_store,
    ident_ids_for_slicing,
    ident_store,
    line_starts,
    types_store,
    exposed_by_str,
    exposed_nodes,
    external_decls,
};

/// Diagnostic information about a cache
pub const Diagnostics = struct {
    total_size: u32,
    header_size: u32,
    data_size: u32,
    checksum: u32,
    component_sizes: struct {
        node_store: u32,
        string_store: u32,
        ident_ids_for_slicing: u32,
        ident_store: u32,
        line_starts: u32,
        types_store: u32,
        exposed_by_str: u32,
        exposed_nodes: u32,
        external_decls: u32,
    },
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
    var module_env = try base.ModuleEnv.init(gpa, try gpa.dupe(u8, source));
    defer module_env.deinit();

    var cir = try CIR.init(&module_env, "TestModule");
    defer cir.deinit();

    // Parse and canonicalize
    var ast = try parse(&module_env, source);
    defer ast.deinit(gpa);

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    // Generate original S-expression for comparison
    var original_tree = SExprTree.init(gpa);
    defer original_tree.deinit();
    try CIR.pushToSExprTree(&cir, null, &original_tree, source);

    var original_sexpr = std.ArrayList(u8).init(gpa);
    defer original_sexpr.deinit();
    try original_tree.toStringPretty(original_sexpr.writer().any());

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, &module_env, &cir, 0, 0);
    defer gpa.free(cache_data);

    // Load cache
    var cache = try CacheModule.fromMappedMemory(cache_data);

    // Validate cache
    try cache.validate();

    // Restore ModuleEnv and CIR
    // Duplicate source since restore takes ownership
    const restored = try cache.restore(gpa, "TestModule", try gpa.dupe(u8, source));

    var restored_module_env = restored.module_env;
    defer restored_module_env.deinit();
    var restored_cir = restored.cir;
    defer restored_cir.deinit();

    // Fix env pointer after struct move
    restored_cir.env = &restored_module_env;

    // Generate S-expression from restored CIR
    var restored_tree = SExprTree.init(gpa);
    defer restored_tree.deinit();

    try CIR.pushToSExprTree(&restored_cir, null, &restored_tree, source);

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
    var module_env = try base.ModuleEnv.init(gpa, try gpa.dupe(u8, source));
    defer module_env.deinit();

    var cir = try CIR.init(&module_env, "TestModule");
    defer cir.deinit();

    // Parse and canonicalize
    var ast = try parse(&module_env, source);
    defer ast.deinit(gpa);

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();
    try canonicalizer.canonicalizeFile();

    // Generate original S-expression for comparison
    var original_tree = SExprTree.init(gpa);
    defer original_tree.deinit();
    try CIR.pushToSExprTree(&cir, null, &original_tree, source);

    var original_sexpr = std.ArrayList(u8).init(gpa);
    defer original_sexpr.deinit();
    try original_tree.toStringPretty(original_sexpr.writer().any());

    // Create cache from real data
    const cache_data = try CacheModule.create(gpa, &module_env, &cir, 0, 0);
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
    try roundtrip_cache.validate();

    // Restore from the roundtrip cache
    // Duplicate source since restore takes ownership
    const restored = try roundtrip_cache.restore(gpa, "TestModule", try gpa.dupe(u8, source));

    var restored_module_env = restored.module_env;
    defer restored_module_env.deinit();
    var restored_cir = restored.cir;
    defer restored_cir.deinit();

    // Fix env pointer after struct move
    restored_cir.env = &restored_module_env;

    // Generate S-expression from restored CIR
    var restored_tree = SExprTree.init(gpa);
    defer restored_tree.deinit();

    try CIR.pushToSExprTree(&restored_cir, null, &restored_tree, source);

    var restored_sexpr = std.ArrayList(u8).init(gpa);
    defer restored_sexpr.deinit();

    try restored_tree.toStringPretty(restored_sexpr.writer().any());

    // Verify complete roundtrip integrity
    try std.testing.expect(std.mem.eql(u8, original_sexpr.items, restored_sexpr.items));

    // Get diagnostics to ensure they're preserved
    const diagnostics = roundtrip_cache.getDiagnostics();
    try std.testing.expect(diagnostics.total_size > 0);
}
