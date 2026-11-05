//! An append-only list whose items never move, so a thread may keep a
//! pointer to an item (an atomic, a write-once slot) while one writer at a
//! time appends.
//!
//! Items live in shelves whose sizes double: shelf `k` holds
//! `first_shelf << k` items. A shelf is allocated when the writer first
//! needs it and is never moved or freed until `deinit`. A reader loads the
//! published count, then the shelf pointer for an index below it.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// An append-only list of `T` with stable item addresses.
pub fn StableList(comptime T: type) type {
    return struct {
        const Self = @This();
        const first_shelf: usize = 64;
        const shelf_count = @bitSizeOf(usize) - @as(usize, std.math.log2_int(usize, first_shelf));

        shelves: [shelf_count]std.atomic.Value(?[*]T) = @splat(.init(null)),
        published_len: std.atomic.Value(usize) = .init(0),

        pub const empty: Self = .{};

        fn shelfOf(index: usize) struct { shelf: usize, offset: usize } {
            const shelf = std.math.log2_int(usize, index / first_shelf + 1);
            return .{ .shelf = shelf, .offset = index - first_shelf * ((@as(usize, 1) << @intCast(shelf)) - 1) };
        }

        fn shelfLen(shelf: usize) usize {
            return first_shelf << @intCast(shelf);
        }

        pub fn count(self: *const Self) usize {
            return self.published_len.load(.acquire);
        }

        /// The item at `index`, which must be below `count()`. Safe from any
        /// thread.
        pub fn at(self: *const Self, index: usize) *T {
            std.debug.assert(index < self.published_len.load(.acquire));
            const place = shelfOf(index);
            const shelf = self.shelves[place.shelf].load(.acquire) orelse unreachable;
            return &shelf[place.offset];
        }

        /// Append `value` until the list holds `new_len` items. Only the
        /// writer calls this.
        pub fn growTo(self: *Self, allocator: Allocator, new_len: usize, value: T) Allocator.Error!void {
            var len = self.published_len.load(.monotonic);
            while (len < new_len) : (len += 1) {
                const place = shelfOf(len);
                const shelf = self.shelves[place.shelf].load(.monotonic) orelse blk: {
                    const memory = try allocator.alloc(T, shelfLen(place.shelf));
                    self.shelves[place.shelf].store(memory.ptr, .release);
                    break :blk memory.ptr;
                };
                shelf[place.offset] = value;
                self.published_len.store(len + 1, .release);
            }
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            for (&self.shelves, 0..) |*shelf, index| {
                if (shelf.load(.monotonic)) |ptr| allocator.free(ptr[0..shelfLen(index)]);
            }
            self.* = .{};
        }
    };
}

test "items keep their addresses while the list grows" {
    const gpa = std.testing.allocator;
    var list: StableList(u32) = .empty;
    defer list.deinit(gpa);
    try list.growTo(gpa, 1, 7);
    const first = list.at(0);
    try list.growTo(gpa, 1000, 3);
    try std.testing.expectEqual(first, list.at(0));
    try std.testing.expectEqual(@as(u32, 7), first.*);
    try std.testing.expectEqual(@as(u32, 3), list.at(999).*);
    try std.testing.expectEqual(@as(usize, 1000), list.count());
}
