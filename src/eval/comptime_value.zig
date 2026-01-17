//! Top-level bindings for compile-time evaluation.
//!
//! Maps pattern indices directly to memory addresses where evaluated
//! constants are stored. Used during compilation to track top-level
//! constant values that have already been evaluated.

const std = @import("std");

/// Environment for top-level compile-time evaluation.
/// Maps pattern indices directly to memory addresses.
pub const TopLevelBindings = struct {
    /// pattern_idx -> address of evaluated value
    addresses: std.AutoHashMap(u32, [*]u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TopLevelBindings {
        return .{
            .addresses = std.AutoHashMap(u32, [*]u8).init(allocator),
            .allocator = allocator,
        };
    }

    /// Bind a pattern index to its evaluated value's address.
    pub fn bind(self: *TopLevelBindings, pattern_idx: u32, address: [*]u8) !void {
        try self.addresses.put(pattern_idx, address);
    }

    /// Look up the address of an evaluated value by pattern index.
    pub fn lookup(self: *const TopLevelBindings, pattern_idx: u32) ?[*]u8 {
        return self.addresses.get(pattern_idx);
    }

    pub fn deinit(self: *TopLevelBindings) void {
        self.addresses.deinit();
    }
};

// =============================================================================
// Legacy types for backward compatibility during migration
// These will be removed once the full refactor is complete
// =============================================================================

/// Simple heap for allocating memory during evaluation.
/// Uses an arena so everything can be freed at once.
pub const ComptimeHeap = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) ComptimeHeap {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    pub fn alloc(self: *ComptimeHeap, size: usize, comptime log2_alignment: u8) ![]u8 {
        const alignment: std.mem.Alignment = @enumFromInt(log2_alignment);
        return self.arena.allocator().alignedAlloc(u8, alignment, size);
    }

    pub fn allocator(self: *ComptimeHeap) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *ComptimeHeap) void {
        self.arena.deinit();
    }
};

/// Simple wrapper for a pointer to bytes.
/// Used for storing intermediate values during evaluation.
pub const ComptimeValue = struct {
    bytes: [*]u8,
    size: usize,

    pub fn fromBytes(bytes_slice: []u8, _: anytype) ComptimeValue {
        return .{
            .bytes = bytes_slice.ptr,
            .size = bytes_slice.len,
        };
    }

    pub fn as(self: ComptimeValue, comptime T: type) T {
        std.debug.assert(self.size >= @sizeOf(T));
        return @as(*const T, @ptrCast(@alignCast(self.bytes))).*;
    }

    pub fn set(self: ComptimeValue, comptime T: type, value: T) void {
        std.debug.assert(self.size >= @sizeOf(T));
        @as(*T, @ptrCast(@alignCast(self.bytes))).* = value;
    }

    pub fn toSlice(self: ComptimeValue) []u8 {
        return self.bytes[0..self.size];
    }

    pub fn toConstSlice(self: ComptimeValue) []const u8 {
        return self.bytes[0..self.size];
    }
};

/// Simple environment mapping pattern indices to values.
/// Flat map, no child scopes.
pub const ComptimeEnv = struct {
    const can = @import("can");
    const CIR = can.CIR;

    heap: *ComptimeHeap,
    bindings: std.AutoHashMap(u32, ComptimeValue),
    closure_refs: std.AutoHashMap(u32, CIR.Expr.Idx),

    pub fn init(heap: *ComptimeHeap, _: anytype) ComptimeEnv {
        return .{
            .heap = heap,
            .bindings = std.AutoHashMap(u32, ComptimeValue).init(heap.allocator()),
            .closure_refs = std.AutoHashMap(u32, CIR.Expr.Idx).init(heap.allocator()),
        };
    }

    pub fn bind(self: *ComptimeEnv, pattern_idx: u32, value: ComptimeValue) !void {
        try self.bindings.put(pattern_idx, value);
    }

    pub fn bindClosure(self: *ComptimeEnv, pattern_idx: u32, expr_idx: CIR.Expr.Idx) !void {
        try self.closure_refs.put(pattern_idx, expr_idx);
    }

    pub fn lookup(self: *const ComptimeEnv, pattern_idx: u32) ?ComptimeValue {
        return self.bindings.get(pattern_idx);
    }

    pub fn lookupClosure(self: *const ComptimeEnv, pattern_idx: u32) ?CIR.Expr.Idx {
        return self.closure_refs.get(pattern_idx);
    }

    /// Create a child environment - for compatibility, just creates a copy
    pub fn child(self: *const ComptimeEnv) !ComptimeEnv {
        var new_env = ComptimeEnv.init(self.heap, null);
        var iter = self.bindings.iterator();
        while (iter.next()) |entry| {
            try new_env.bindings.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        var closure_iter = self.closure_refs.iterator();
        while (closure_iter.next()) |entry| {
            try new_env.closure_refs.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        return new_env;
    }

    pub fn deinit(self: *ComptimeEnv) void {
        self.bindings.deinit();
        self.closure_refs.deinit();
    }
};

// =============================================================================
// Tests
// =============================================================================

test "TopLevelBindings bind and lookup" {
    var bindings = TopLevelBindings.init(std.testing.allocator);
    defer bindings.deinit();

    var buffer: [8]u8 = undefined;
    const ptr: [*]u8 = &buffer;

    try bindings.bind(42, ptr);

    const looked_up = bindings.lookup(42);
    try std.testing.expect(looked_up != null);
    try std.testing.expectEqual(ptr, looked_up.?);
}

test "TopLevelBindings lookup missing returns null" {
    var bindings = TopLevelBindings.init(std.testing.allocator);
    defer bindings.deinit();

    try std.testing.expectEqual(@as(?[*]u8, null), bindings.lookup(999));
}

test "ComptimeHeap basic allocation" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    const bytes = try heap.alloc(8, 3);
    try std.testing.expectEqual(@as(usize, 8), bytes.len);
}

test "ComptimeValue read/write i64" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    const bytes = try heap.alloc(8, 3);
    const value = ComptimeValue.fromBytes(bytes, undefined);

    value.set(i64, 42);
    try std.testing.expectEqual(@as(i64, 42), value.as(i64));
}

test "ComptimeEnv bind and lookup" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    var env = ComptimeEnv.init(&heap, null);
    defer env.deinit();

    const bytes = try heap.alloc(8, 3);
    const value = ComptimeValue.fromBytes(bytes, undefined);
    value.set(i64, 123);

    try env.bind(42, value);

    const looked_up = env.lookup(42);
    try std.testing.expect(looked_up != null);
    try std.testing.expectEqual(@as(i64, 123), looked_up.?.as(i64));
}
