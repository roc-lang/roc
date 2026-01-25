//! Global interner for all static data (strings, etc.) that will end up in .rodata
//!
//! This interner lives at the compilation session level, shared across all modules.
//! It provides deduplication of identical byte sequences and supports pluggable
//! backends for different compilation modes (JIT vs object file generation).
//!
//! Design:
//! - Global across all modules - a single giant intern table
//! - Backend abstraction allows JIT (direct memory) or object file (.rodata section)
//! - Hash-based deduplication with collision checking

const std = @import("std");
const Allocator = std.mem.Allocator;

const Self = @This();

/// Result of interning data - contains pointer and length
pub const InternedData = struct {
    /// Pointer to the actual data (for JIT, this is the runtime address)
    ptr: [*]const u8,
    /// Length of the data
    len: usize,

    /// Get as a slice
    pub fn slice(self: InternedData) []const u8 {
        return self.ptr[0..self.len];
    }
};

/// Backend interface for actual allocation
pub const Backend = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        /// Allocate space for static data, returns pointer to write data and runtime address
        /// The backend is responsible for copying the data.
        alloc: *const fn (ptr: *anyopaque, data: []const u8, alignment: usize) ?InternedData,
    };

    pub fn alloc(self: Backend, data: []const u8, alignment: usize) ?InternedData {
        return self.vtable.alloc(self.ptr, data, alignment);
    }
};

/// Maps byte content hash → allocation info
intern_map: std.AutoHashMap(u64, InternEntry),

/// Allocator for the hash map
allocator: Allocator,

/// Pluggable backend for actual allocation
backend: Backend,

/// Entry in the intern table - stores hash for collision detection
const InternEntry = struct {
    data: InternedData,
    /// Store first few bytes for quick collision check
    prefix: [8]u8,
    prefix_len: u8,
};

/// Initialize a new StaticDataInterner with the given backend
pub fn init(allocator: Allocator, backend: Backend) Self {
    return .{
        .intern_map = std.AutoHashMap(u64, InternEntry).init(allocator),
        .allocator = allocator,
        .backend = backend,
    };
}

/// Deinitialize the interner
pub fn deinit(self: *Self) void {
    self.intern_map.deinit();
}

/// Intern a byte blob - returns existing allocation if already interned, else allocates new
pub fn intern(self: *Self, data: []const u8, alignment: usize) ?InternedData {
    const hash = std.hash.Wyhash.hash(0, data);

    if (self.intern_map.get(hash)) |existing| {
        // Quick prefix check first
        const check_len = @min(data.len, existing.prefix_len);
        if (check_len > 0 and !std.mem.eql(u8, existing.prefix[0..check_len], data[0..check_len])) {
            // Hash collision - different data
            // For now, just allocate new (in practice, collisions are rare)
            return self.allocateNew(data, alignment, hash);
        }

        // Full verification for same length
        if (existing.data.len == data.len) {
            if (std.mem.eql(u8, existing.data.slice(), data)) {
                return existing.data;
            }
        }

        // Hash collision with different data - allocate new
        // Note: This is a limitation - we could use a multi-map for collision handling
        return self.allocateNew(data, alignment, hash);
    }

    // Not found, allocate via backend
    return self.allocateNew(data, alignment, hash);
}

fn allocateNew(self: *Self, data: []const u8, alignment: usize, hash: u64) ?InternedData {
    const result = self.backend.alloc(data, alignment) orelse return null;

    // Create entry with prefix for quick collision detection
    var entry = InternEntry{
        .data = result,
        .prefix = undefined,
        .prefix_len = @intCast(@min(data.len, 8)),
    };
    if (entry.prefix_len > 0) {
        @memcpy(entry.prefix[0..entry.prefix_len], data[0..entry.prefix_len]);
    }

    self.intern_map.put(hash, entry) catch return null;
    return result;
}

/// Convenience for interning strings (alignment 1)
pub fn internString(self: *Self, str: []const u8) ?InternedData {
    return self.intern(str, 1);
}

/// Get the number of interned entries
pub fn count(self: *const Self) usize {
    return self.intern_map.count();
}

/// Simple JIT backend that allocates from an arena
/// Data lives as long as the JIT code execution
pub const JitStaticBackend = struct {
    /// Arena for allocations - memory is freed when the backend is deinitialized
    arena: std.heap.ArenaAllocator,

    const vtable = Backend.VTable{
        .alloc = alloc,
    };

    pub fn init(backing_allocator: Allocator) JitStaticBackend {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    pub fn deinit(self: *JitStaticBackend) void {
        self.arena.deinit();
    }

    pub fn backend(self: *JitStaticBackend) Backend {
        return .{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    /// Static refcount marker (isize::MIN) - RC operations skip values with this refcount
    const REFCOUNT_STATIC: isize = std.math.minInt(isize);

    fn alloc(ptr: *anyopaque, data: []const u8, alignment: usize) ?InternedData {
        const self: *JitStaticBackend = @ptrCast(@alignCast(ptr));
        const allocator = self.arena.allocator();

        // Allocate with refcount prefix: [refcount: 8 bytes][data]
        // This allows RC operations to safely check and skip static data
        _ = alignment; // Strings just need byte alignment
        const total_size = @sizeOf(isize) + data.len;
        const allocated = allocator.alignedAlloc(u8, .@"8", total_size) catch return null;

        // Write static refcount at the start
        const refcount_ptr: *isize = @ptrCast(@alignCast(allocated.ptr));
        refcount_ptr.* = REFCOUNT_STATIC;

        // Copy data after refcount
        const data_ptr = allocated.ptr + @sizeOf(isize);
        @memcpy(data_ptr[0..data.len], data);

        // Return pointer to data (after refcount), matching RocStr layout
        return .{
            .ptr = data_ptr,
            .len = data.len,
        };
    }
};

test "basic interning" {
    const allocator = std.testing.allocator;

    var jit_backend = JitStaticBackend.init(allocator);
    defer jit_backend.deinit();

    var interner = init(allocator, jit_backend.backend());
    defer interner.deinit();

    // Intern a string
    const result1 = interner.internString("hello world");
    try std.testing.expect(result1 != null);
    try std.testing.expectEqualStrings("hello world", result1.?.slice());

    // Intern the same string - should return same pointer
    const result2 = interner.internString("hello world");
    try std.testing.expect(result2 != null);
    try std.testing.expectEqual(result1.?.ptr, result2.?.ptr);

    // Intern a different string - should return different pointer
    const result3 = interner.internString("goodbye world");
    try std.testing.expect(result3 != null);
    try std.testing.expect(result1.?.ptr != result3.?.ptr);

    // Should have 2 unique entries
    try std.testing.expectEqual(@as(usize, 2), interner.count());
}

test "empty string interning" {
    const allocator = std.testing.allocator;

    var jit_backend = JitStaticBackend.init(allocator);
    defer jit_backend.deinit();

    var interner = init(allocator, jit_backend.backend());
    defer interner.deinit();

    const result = interner.internString("");
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(usize, 0), result.?.len);
}

test "deduplication of identical long strings" {
    const allocator = std.testing.allocator;

    var jit_backend = JitStaticBackend.init(allocator);
    defer jit_backend.deinit();

    var interner = init(allocator, jit_backend.backend());
    defer interner.deinit();

    const long_string = "this is a very long string that exceeds the small string optimization threshold of 23 bytes";

    const result1 = interner.internString(long_string);
    const result2 = interner.internString(long_string);

    try std.testing.expect(result1 != null);
    try std.testing.expect(result2 != null);
    try std.testing.expectEqual(result1.?.ptr, result2.?.ptr);
    try std.testing.expectEqual(@as(usize, 1), interner.count());
}
