//! A reusable FIFO whose removals never move the undispatched suffix.
const std = @import("std");

/// A power-of-two ring retaining capacity between drain cycles.
pub fn RingQueue(comptime T: type) type {
    return struct {
        const Self = @This();
        buffer: []T = &.{},
        head: usize = 0,
        len: usize = 0,

        pub const empty: Self = .{};

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.buffer);
            self.* = undefined;
        }

        pub fn get(self: *const Self, index: usize) T {
            std.debug.assert(index < self.len);
            return self.buffer[(self.head + index) & (self.buffer.len - 1)];
        }

        pub fn append(self: *Self, allocator: std.mem.Allocator, value: T) std.mem.Allocator.Error!void {
            if (self.len == self.buffer.len) {
                const capacity = std.math.mul(usize, @max(self.buffer.len, 1), 2) catch return error.OutOfMemory;
                const buffer = try allocator.alloc(T, capacity);
                for (buffer[0..self.len], 0..) |*slot, index| slot.* = self.get(index);
                allocator.free(self.buffer);
                self.buffer = buffer;
                self.head = 0;
            }
            self.buffer[(self.head + self.len) & (self.buffer.len - 1)] = value;
            self.len += 1;
        }

        pub fn pop(self: *Self) T {
            const value = self.get(0);
            self.head = (self.head + 1) & (self.buffer.len - 1);
            self.len -= 1;
            return value;
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            self.head = 0;
            self.len = 0;
        }
    };
}

test "ring queue preserves FIFO across wrapping, growth, and reuse" {
    var queue: RingQueue(u32) = .empty;
    defer queue.deinit(std.testing.allocator);
    for (0..100) |i| try queue.append(std.testing.allocator, @intCast(i));
    for (0..75) |i| try std.testing.expectEqual(i, queue.pop());
    for (100..300) |i| try queue.append(std.testing.allocator, @intCast(i));
    for (75..300) |i| try std.testing.expectEqual(i, queue.pop());
    queue.clearRetainingCapacity();
    const buffer = queue.buffer.ptr;
    for (0..10_000) |i| {
        try queue.append(std.testing.failing_allocator, @intCast(i));
        try std.testing.expectEqual(i, queue.pop());
    }
    try std.testing.expectEqual(buffer, queue.buffer.ptr);
}

test "ring queue popping a large backlog does not move its entries" {
    var queue: RingQueue(u32) = .empty;
    defer queue.deinit(std.testing.allocator);
    for (0..8192) |i| try queue.append(std.testing.allocator, @intCast(i));
    for (0..8192) |i| {
        try std.testing.expectEqual(i, queue.head);
        try std.testing.expectEqual(i, queue.buffer[i]);
        try std.testing.expectEqual(i, queue.pop());
    }
}

test "ring queue allocation failure preserves a wrapped backlog" {
    var queue: RingQueue(u32) = .empty;
    defer queue.deinit(std.testing.allocator);
    try queue.append(std.testing.allocator, 1);
    try queue.append(std.testing.allocator, 2);
    try std.testing.expectEqual(1, queue.pop());
    try queue.append(std.testing.allocator, 3);
    try std.testing.expectError(error.OutOfMemory, queue.append(std.testing.failing_allocator, 4));
    try std.testing.expectEqual(2, queue.pop());
    try std.testing.expectEqual(3, queue.pop());
}
