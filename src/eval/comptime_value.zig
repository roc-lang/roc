//! Compile-time value representation for the dev backend.
//!
//! These data structures support compile-time evaluation that produces actual bytes
//! in memory, which can then be copied into the final binary's readonly section.
//!
//! This is independent from the interpreter's StackValue - we intentionally create
//! our own simple data structures for the dev backend.

const std = @import("std");
const layout_mod = @import("layout");
const can = @import("can");
const Layout = layout_mod.Layout;
const LayoutIdx = layout_mod.Idx;
const LayoutStore = layout_mod.Store;
const CIR = can.CIR;

/// Heap memory for compile-time evaluation.
///
/// Bytes allocated here may end up in the final binary's readonly section.
/// Uses an arena allocator so all allocations can be freed at once when
/// compile-time evaluation is complete.
pub const ComptimeHeap = struct {
    /// Arena for all compile-time allocations
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) ComptimeHeap {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    /// Allocate bytes with the given size (uses default alignment).
    pub fn alloc(self: *ComptimeHeap, size: usize) ![]u8 {
        return self.arena.allocator().alloc(u8, size);
    }

    /// Allocate bytes with 8-byte alignment (sufficient for i64, f64, pointers).
    pub fn alloc8(self: *ComptimeHeap, size: usize) ![]align(8) u8 {
        return self.arena.allocator().alignedAlloc(u8, .@"8", size);
    }

    /// Allocate bytes with 16-byte alignment (for i128, etc).
    pub fn alloc16(self: *ComptimeHeap, size: usize) ![]align(16) u8 {
        return self.arena.allocator().alignedAlloc(u8, .@"16", size);
    }

    /// Get the underlying allocator for more complex allocations.
    pub fn allocator(self: *ComptimeHeap) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *ComptimeHeap) void {
        self.arena.deinit();
    }
};

/// A value produced by compile-time evaluation.
///
/// Contains actual bytes in memory (allocated from ComptimeHeap).
/// The bytes represent the value in its runtime memory layout.
pub const ComptimeValue = struct {
    /// Pointer to the actual bytes
    bytes: [*]u8,
    /// Size in bytes
    size: usize,
    /// Layout information for interpreting the bytes
    layout_idx: LayoutIdx,

    /// Create a ComptimeValue from a slice of bytes.
    pub fn fromBytes(bytes_slice: []u8, layout_idx: LayoutIdx) ComptimeValue {
        return .{
            .bytes = bytes_slice.ptr,
            .size = bytes_slice.len,
            .layout_idx = layout_idx,
        };
    }

    /// Read the bytes as a specific type.
    pub fn as(self: ComptimeValue, comptime T: type) T {
        std.debug.assert(self.size >= @sizeOf(T));
        return @as(*const T, @ptrCast(@alignCast(self.bytes))).*;
    }

    /// Write a value of a specific type to the bytes.
    pub fn set(self: ComptimeValue, comptime T: type, value: T) void {
        std.debug.assert(self.size >= @sizeOf(T));
        @as(*T, @ptrCast(@alignCast(self.bytes))).* = value;
    }

    /// Get a slice of the bytes.
    pub fn toSlice(self: ComptimeValue) []u8 {
        return self.bytes[0..self.size];
    }

    /// Get a const slice of the bytes.
    pub fn toConstSlice(self: ComptimeValue) []const u8 {
        return self.bytes[0..self.size];
    }
};

/// Environment for compile-time evaluation.
///
/// Maps pattern indices to their computed values.
/// All values are allocated from the associated ComptimeHeap.
/// Closures are stored separately by expression index to avoid mixing
/// evaluated values with deferred expressions.
pub const ComptimeEnv = struct {
    heap: *ComptimeHeap,
    /// Evaluated values (numbers, records, etc.)
    bindings: std.AutoHashMap(u32, ComptimeValue),
    /// Closures stored by expression index (deferred evaluation)
    closure_refs: std.AutoHashMap(u32, CIR.Expr.Idx),
    /// Optional layout store for interpreting values. Can be null during
    /// initial development when we only need bind/lookup functionality.
    layout_store: ?*LayoutStore,

    pub fn init(heap: *ComptimeHeap, layout_store: ?*LayoutStore) ComptimeEnv {
        return .{
            .heap = heap,
            .bindings = std.AutoHashMap(u32, ComptimeValue).init(heap.allocator()),
            .closure_refs = std.AutoHashMap(u32, CIR.Expr.Idx).init(heap.allocator()),
            .layout_store = layout_store,
        };
    }

    /// Bind a pattern index to an evaluated value.
    pub fn bind(self: *ComptimeEnv, pattern_idx: u32, value: ComptimeValue) !void {
        try self.bindings.put(pattern_idx, value);
    }

    /// Bind a pattern index to a closure (stored by expression index).
    pub fn bindClosure(self: *ComptimeEnv, pattern_idx: u32, expr_idx: CIR.Expr.Idx) !void {
        try self.closure_refs.put(pattern_idx, expr_idx);
    }

    /// Look up an evaluated value by pattern index.
    pub fn lookup(self: *const ComptimeEnv, pattern_idx: u32) ?ComptimeValue {
        return self.bindings.get(pattern_idx);
    }

    /// Look up a closure by pattern index.
    pub fn lookupClosure(self: *const ComptimeEnv, pattern_idx: u32) ?CIR.Expr.Idx {
        return self.closure_refs.get(pattern_idx);
    }

    /// Create a child environment that inherits bindings from this one.
    /// The child uses the same heap but has its own bindings maps.
    pub fn child(self: *const ComptimeEnv) !ComptimeEnv {
        var new_env = ComptimeEnv.init(self.heap, self.layout_store);
        // Copy parent value bindings
        var iter = self.bindings.iterator();
        while (iter.next()) |entry| {
            try new_env.bindings.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        // Copy parent closure bindings
        var closure_iter = self.closure_refs.iterator();
        while (closure_iter.next()) |entry| {
            try new_env.closure_refs.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        return new_env;
    }

    /// Clean up the bindings maps.
    /// Note: The actual bytes are owned by the heap and freed when the heap is deinited.
    pub fn deinit(self: *ComptimeEnv) void {
        self.bindings.deinit();
        self.closure_refs.deinit();
    }
};

// Tests

test "ComptimeHeap basic allocation" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    const bytes = try heap.alloc(8);
    try std.testing.expectEqual(@as(usize, 8), bytes.len);
}

test "ComptimeValue read/write i64" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    const bytes = try heap.alloc8(8);
    const value = ComptimeValue.fromBytes(bytes, undefined);

    value.set(i64, 42);
    try std.testing.expectEqual(@as(i64, 42), value.as(i64));
}

test "ComptimeEnv bind and lookup" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    var env = ComptimeEnv.init(&heap, null);
    defer env.deinit();

    const bytes = try heap.alloc8(8);
    const value = ComptimeValue.fromBytes(bytes, undefined);
    value.set(i64, 123);

    try env.bind(42, value);

    const looked_up = env.lookup(42);
    try std.testing.expect(looked_up != null);
    try std.testing.expectEqual(@as(i64, 123), looked_up.?.as(i64));
}

test "ComptimeEnv child inherits bindings" {
    var heap = ComptimeHeap.init(std.testing.allocator);
    defer heap.deinit();

    var parent = ComptimeEnv.init(&heap, null);
    defer parent.deinit();

    const bytes = try heap.alloc8(8);
    const value = ComptimeValue.fromBytes(bytes, undefined);
    value.set(i64, 999);
    try parent.bind(1, value);

    var child_env = try parent.child();
    defer child_env.deinit();

    // Child should see parent's binding
    const from_child = child_env.lookup(1);
    try std.testing.expect(from_child != null);
    try std.testing.expectEqual(@as(i64, 999), from_child.?.as(i64));

    // Add new binding to child
    const bytes2 = try heap.alloc8(8);
    const value2 = ComptimeValue.fromBytes(bytes2, undefined);
    value2.set(i64, 777);
    try child_env.bind(2, value2);

    // Parent should NOT see child's new binding
    try std.testing.expect(parent.lookup(2) == null);
}
