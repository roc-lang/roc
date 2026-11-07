//! An append-only list that one writer at a time grows while any number of
//! threads read it without a lock.
//!
//! The writer appends through `list` and publishes its backing pointer and
//! length after every change. A reader loads the published length, then the
//! published pointer, and reads that many items. Growth copies the items
//! into a new backing and keeps the old one in `superseded` until
//! `releaseSuperseded`, so a reader that loaded an older pointer still reads
//! valid memory, and every item below an older length is equal in both
//! backings. An item is rewritten in place only before anything that names
//! it reaches another thread.

const std = @import("std");

const SupersededBackings = @import("SupersededBackings.zig");

const Allocator = std.mem.Allocator;

/// A list readers read in place while one writer grows it.
pub fn PublishedList(comptime T: type) type {
    return struct {
        const Self = @This();

        /// The writer's list. Only the thread that holds the writer role
        /// touches it; every change is followed by `publish`.
        list: std.ArrayList(T) = .empty,
        superseded: SupersededBackings = .{},
        published_ptr: std.atomic.Value(?[*]T) = .init(null),
        published_len: std.atomic.Value(usize) = .init(0),
        /// Whether `list` aliases memory someone else owns.
        borrowed: bool = false,

        pub const empty: Self = .{};

        /// A list whose items alias `items`, owned by someone else. The
        /// caller copies it with `own` before growing it.
        pub fn borrowing(items_: []const T) Self {
            const mutable = @constCast(items_);
            return .{
                .list = .fromOwnedSlice(mutable),
                .published_ptr = .init(if (mutable.len == 0) null else mutable.ptr),
                .published_len = .init(mutable.len),
                .borrowed = true,
            };
        }

        /// The published items. Safe from any thread.
        pub fn items(self: *const Self) []T {
            const len = self.published_len.load(.acquire);
            const ptr = self.published_ptr.load(.acquire) orelse return @constCast(&[_]T{});
            return ptr[0..len];
        }

        pub fn count(self: *const Self) usize {
            return self.published_len.load(.acquire);
        }

        fn publish(self: *Self) void {
            self.published_ptr.store(if (self.list.capacity == 0) null else self.list.items.ptr, .release);
            self.published_len.store(self.list.items.len, .release);
        }

        /// Replace a borrowed backing with an owned copy of its items.
        pub fn own(self: *Self, allocator: Allocator) Allocator.Error!void {
            if (!self.borrowed) return;
            const copy = try allocator.alloc(T, self.list.items.len);
            @memcpy(copy, self.list.items);
            self.list = .fromOwnedSlice(copy);
            self.borrowed = false;
            self.publish();
        }

        pub fn ensureUnusedCapacity(self: *Self, allocator: Allocator, additional: usize) Allocator.Error!void {
            std.debug.assert(!self.borrowed);
            if (try self.superseded.ensureUnusedCapacity(T, &self.list, allocator, additional)) self.publish();
        }

        pub fn append(self: *Self, allocator: Allocator, item: T) Allocator.Error!void {
            try self.ensureUnusedCapacity(allocator, 1);
            self.appendAssumeCapacity(item);
        }

        pub fn appendSlice(self: *Self, allocator: Allocator, new_items: []const T) Allocator.Error!void {
            try self.ensureUnusedCapacity(allocator, new_items.len);
            self.list.appendSliceAssumeCapacity(new_items);
            self.publish();
        }

        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            self.list.appendAssumeCapacity(item);
            self.publish();
        }

        /// Drop the items from `new_len` on. Only items no other thread can
        /// name are dropped.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            self.list.shrinkRetainingCapacity(new_len);
            self.publish();
        }

        pub fn pop(self: *Self) ?T {
            const item = self.list.pop();
            self.publish();
            return item;
        }

        /// Free the backings growth replaced. Only call this when no other
        /// thread can hold a slice of them.
        pub fn releaseSuperseded(self: *Self, allocator: Allocator) void {
            self.superseded.releaseThrough(allocator, self.superseded.generation);
        }

        /// Free the owned backing and every superseded one.
        pub fn deinit(self: *Self, allocator: Allocator) void {
            if (!self.borrowed) self.list.deinit(allocator);
            self.superseded.deinit(allocator);
            self.* = .{};
        }
    };
}

test "readers see every published item while the writer grows the list" {
    const gpa = std.testing.allocator;
    var list: PublishedList(u32) = .empty;
    defer list.deinit(gpa);
    try list.append(gpa, 1);
    const first = list.items();
    for (0..100) |i| try list.append(gpa, @intCast(i + 2));
    try std.testing.expectEqual(@as(u32, 1), first[0]);
    try std.testing.expectEqual(@as(usize, 101), list.count());
    try std.testing.expectEqual(@as(u32, 101), list.items()[100]);
    list.releaseSuperseded(gpa);
    try std.testing.expectEqual(@as(u32, 1), list.items()[0]);
}

test "a borrowed list is copied before it grows" {
    const gpa = std.testing.allocator;
    const source = [_]u32{ 4, 5 };
    var list = PublishedList(u32).borrowing(&source);
    try std.testing.expectEqualSlices(u32, &source, list.items());
    try list.own(gpa);
    defer list.deinit(gpa);
    try list.append(gpa, 6);
    try std.testing.expectEqualSlices(u32, &.{ 4, 5, 6 }, list.items());
    try std.testing.expectEqualSlices(u32, &.{ 4, 5 }, &source);
}
