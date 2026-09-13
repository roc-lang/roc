//! Transient lexicographic ranks for an append-only string store.
//! The owning store is runtime storage even through its read-only views. Only
//! this derived cache is logically mutable; no interned or serialized data changes.
//! Readers may build the same generation concurrently. Insertion and destruction,
//! like the owning interner itself, require exclusive access.
const std = @import("std");
const Self = @This();

ranks: []u32 = &.{},
generation: std.atomic.Value(usize) = .init(0),
mutex: std.atomic.Mutex = .unlocked,

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.ranks);
    self.* = .{};
}

pub fn isCurrent(self: *const Self, slots: usize) bool {
    return self.generation.load(.acquire) == slots;
}

/// Build once per interner length, publishing only a complete table. `fillIds`
/// supplies exactly the live ids; sparse byte-offset ids need no string rescans.
pub fn ensure(
    self: *const Self,
    allocator: std.mem.Allocator,
    slots: usize,
    count: usize,
    context: anytype,
    comptime fillIds: fn (@TypeOf(context), []u32) void,
    comptime lessThan: fn (@TypeOf(context), u32, u32) bool,
) std.mem.Allocator.Error!void {
    if (self.isCurrent(slots)) return;
    const cache: *Self = @constCast(self);
    while (!cache.mutex.tryLock()) std.atomic.spinLoopHint();
    defer cache.mutex.unlock();
    if (self.isCurrent(slots)) return;

    const ids = try allocator.alloc(u32, count);
    defer allocator.free(ids);
    fillIds(context, ids);
    std.sort.pdq(u32, ids, context, lessThan);
    const ranks = try allocator.alloc(u32, slots);
    // Only live ids may be read; holes in a byte-offset table are unused.
    for (ids, 0..) |id, position| ranks[id] = @intCast(position);
    allocator.free(cache.ranks);
    cache.ranks = ranks;
    cache.generation.store(slots, .release);
}

pub fn rank(self: *const Self, id: u32) u32 {
    return self.ranks[id];
}
