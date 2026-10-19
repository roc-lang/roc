//! Backings that growing lists replaced while readers may still hold slices
//! of them.
//!
//! A store whose captured prefix another thread reads in place grows its
//! lists by allocating a new backing and copying into it, never by remapping
//! or freeing the old one: the old backing joins this set, tagged with the
//! writer's current generation, and stays valid until the writer learns that
//! no reader can still hold a slice of it. Entries below a captured prefix's
//! length are never rewritten, so the old and new backings agree on them.

const std = @import("std");

const Allocator = std.mem.Allocator;

const SupersededBackings = @This();

const Backing = struct {
    bytes: []u8,
    alignment: std.mem.Alignment,
    generation: u64,
};

backings: std.ArrayList(Backing) = .empty,
/// The tag of backings superseded from now on.
generation: u64 = 0,

pub fn deinit(self: *SupersededBackings, allocator: Allocator) void {
    for (self.backings.items) |backing| allocator.rawFree(backing.bytes, backing.alignment, @returnAddress());
    self.backings.deinit(allocator);
    self.* = undefined;
}

/// The number of superseded backings still held.
pub fn count(self: *const SupersededBackings) usize {
    return self.backings.items.len;
}

/// Free every backing superseded at or before `generation`.
pub fn releaseThrough(self: *SupersededBackings, allocator: Allocator, generation: u64) void {
    var kept: usize = 0;
    for (self.backings.items) |backing| {
        if (backing.generation <= generation) {
            allocator.rawFree(backing.bytes, backing.alignment, @returnAddress());
        } else {
            self.backings.items[kept] = backing;
            kept += 1;
        }
    }
    self.backings.shrinkRetainingCapacity(kept);
}

/// Ensure `list` can hold `additional` more items. When it must grow, its
/// items move to a new backing and the old backing is held here instead of
/// being freed. Returns whether the items moved.
pub fn ensureUnusedCapacity(
    self: *SupersededBackings,
    comptime T: type,
    list: *std.ArrayList(T),
    allocator: Allocator,
    additional: usize,
) Allocator.Error!bool {
    const needed = std.math.add(usize, list.items.len, additional) catch return error.OutOfMemory;
    if (needed <= list.capacity) return false;
    try self.backings.ensureUnusedCapacity(allocator, 1);
    const capacity = std.ArrayList(T).growCapacity(needed);
    const memory = try allocator.alloc(T, capacity);
    @memcpy(memory[0..list.items.len], list.items);
    const old = list.allocatedSlice();
    if (old.len != 0) self.backings.appendAssumeCapacity(.{
        .bytes = std.mem.sliceAsBytes(old),
        .alignment = .of(T),
        .generation = self.generation,
    });
    list.items = memory[0..list.items.len];
    list.capacity = capacity;
    return true;
}

test "growth keeps every superseded backing readable until it is released" {
    const gpa = std.testing.allocator;
    var superseded: SupersededBackings = .{};
    defer superseded.deinit(gpa);
    var list: std.ArrayList(u32) = .empty;
    defer list.deinit(gpa);

    _ = try superseded.ensureUnusedCapacity(u32, &list, gpa, 1);
    list.appendAssumeCapacity(7);
    const first_view: []const u32 = list.items;

    superseded.generation = 1;
    try std.testing.expect(try superseded.ensureUnusedCapacity(u32, &list, gpa, list.capacity));
    for (0..list.capacity - list.items.len) |i| list.appendAssumeCapacity(@intCast(i));
    try std.testing.expect(first_view.ptr != list.items.ptr);
    try std.testing.expectEqualSlices(u32, &.{7}, first_view);

    superseded.generation = 2;
    try std.testing.expect(try superseded.ensureUnusedCapacity(u32, &list, gpa, 1));
    try std.testing.expectEqual(@as(usize, 2), superseded.count());

    // The first growth replaced no backing: the list was empty.
    superseded.releaseThrough(gpa, 0);
    try std.testing.expectEqual(@as(usize, 2), superseded.count());
    // Only the backing superseded at generation 1 is released.
    superseded.releaseThrough(gpa, 1);
    try std.testing.expectEqual(@as(usize, 1), superseded.count());
    try std.testing.expectEqual(@as(u32, 7), list.items[0]);
    superseded.releaseThrough(gpa, 2);
    try std.testing.expectEqual(@as(usize, 0), superseded.count());
}
