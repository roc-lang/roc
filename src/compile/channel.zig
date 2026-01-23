//! Bounded MPSC (Multi-Producer, Single-Consumer) channel for the actor model.
//!
//! This channel provides the communication backbone for the compilation pipeline:
//! - Workers (producers) send results back to the coordinator
//! - Coordinator (consumer) receives and processes results
//!
//! Features:
//! - Bounded capacity provides backpressure
//! - Blocking operations for flow control
//! - Non-blocking tryRecv for polling
//! - Graceful shutdown via close()

const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const Mutex = std.Thread.Mutex;
const Condition = std.Thread.Condition;

/// Default channel capacity
pub const DEFAULT_CAPACITY: usize = 1024;

/// Channel errors
pub const ChannelError = error{
    /// Channel has been closed
    Closed,
    /// Operation timed out
    Timeout,
    /// Out of memory
    OutOfMemory,
};

/// Bounded MPSC channel with blocking operations
pub fn Channel(comptime T: type) type {
    return struct {
        const Self = @This();

        /// Ring buffer for storing items
        buffer: []T,
        /// Current write position (producers increment this)
        write_pos: usize,
        /// Current read position (consumer increments this)
        read_pos: usize,
        /// Number of items currently in the channel
        count: usize,
        /// Protects all mutable state
        mutex: Mutex,
        /// Signaled when channel becomes non-empty (for consumer)
        not_empty: Condition,
        /// Signaled when channel becomes non-full (for producers)
        not_full: Condition,
        /// Whether the channel has been closed
        closed: bool,
        /// Allocator used for the buffer
        gpa: Allocator,

        /// Initialize a channel with the given capacity
        pub fn init(gpa: Allocator, cap_size: usize) !Self {
            const cap = if (cap_size == 0) DEFAULT_CAPACITY else cap_size;
            const buffer = try gpa.alloc(T, cap);
            return .{
                .buffer = buffer,
                .write_pos = 0,
                .read_pos = 0,
                .count = 0,
                .mutex = .{},
                .not_empty = .{},
                .not_full = .{},
                .closed = false,
                .gpa = gpa,
            };
        }

        /// Free the channel's resources
        pub fn deinit(self: *Self) void {
            self.gpa.free(self.buffer);
        }

        /// Send an item to the channel, blocking if full.
        /// Returns error.Closed if the channel has been closed.
        pub fn send(self: *Self, item: T) ChannelError!void {
            self.mutex.lock();
            defer self.mutex.unlock();

            // Wait while channel is full and not closed
            while (self.count >= self.buffer.len and !self.closed) {
                self.not_full.wait(&self.mutex);
            }

            if (self.closed) {
                return error.Closed;
            }

            // Add item to buffer
            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % self.buffer.len;
            self.count += 1;

            // Signal that channel is non-empty
            self.not_empty.signal();
        }

        /// Send an item with a timeout (in nanoseconds).
        /// Returns error.Timeout if the operation times out.
        /// Returns error.Closed if the channel has been closed.
        pub fn sendTimeout(self: *Self, item: T, timeout_ns: u64) ChannelError!void {
            self.mutex.lock();
            defer self.mutex.unlock();

            const deadline = std.time.nanoTimestamp() + @as(i128, timeout_ns);

            // Wait while channel is full and not closed
            while (self.count >= self.buffer.len and !self.closed) {
                const now = std.time.nanoTimestamp();
                if (now >= deadline) {
                    return error.Timeout;
                }
                const remaining = @as(u64, @intCast(deadline - now));
                _ = self.not_full.timedWait(&self.mutex, remaining) catch {};
            }

            if (self.closed) {
                return error.Closed;
            }

            // Add item to buffer
            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % self.buffer.len;
            self.count += 1;

            // Signal that channel is non-empty
            self.not_empty.signal();
        }

        /// Receive an item from the channel, blocking if empty.
        /// Returns null if the channel is closed and empty.
        pub fn recv(self: *Self) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();

            // Wait while channel is empty and not closed
            while (self.count == 0 and !self.closed) {
                self.not_empty.wait(&self.mutex);
            }

            // If empty and closed, return null
            if (self.count == 0) {
                return null;
            }

            return self.recvLocked();
        }

        /// Receive an item with a timeout (in nanoseconds).
        /// Returns null if the operation times out or channel is closed and empty.
        pub fn recvTimeout(self: *Self, timeout_ns: u64) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();

            const deadline = std.time.nanoTimestamp() + @as(i128, timeout_ns);

            // Wait while channel is empty and not closed
            while (self.count == 0 and !self.closed) {
                const now = std.time.nanoTimestamp();
                if (now >= deadline) {
                    return null; // Timeout
                }
                const remaining = @as(u64, @intCast(deadline - now));
                _ = self.not_empty.timedWait(&self.mutex, remaining) catch {};
            }

            // If empty and closed, return null
            if (self.count == 0) {
                return null;
            }

            return self.recvLocked();
        }

        /// Try to receive an item without blocking.
        /// Returns null if the channel is empty.
        pub fn tryRecv(self: *Self) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();

            if (self.count == 0) {
                return null;
            }

            return self.recvLocked();
        }

        /// Internal: receive item while holding the lock
        fn recvLocked(self: *Self) T {
            const item = self.buffer[self.read_pos];
            self.read_pos = (self.read_pos + 1) % self.buffer.len;
            self.count -= 1;

            // Signal that channel is non-full
            self.not_full.signal();

            return item;
        }

        /// Close the channel.
        /// Any blocked senders will receive error.Closed.
        /// Any blocked receivers will be woken and return null if empty.
        pub fn close(self: *Self) void {
            self.mutex.lock();
            defer self.mutex.unlock();

            self.closed = true;

            // Wake all waiting threads
            self.not_empty.broadcast();
            self.not_full.broadcast();
        }

        /// Check if the channel is closed
        pub fn isClosed(self: *Self) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.closed;
        }

        /// Get the number of items currently in the channel
        pub fn len(self: *Self) usize {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.count;
        }

        /// Check if the channel is empty
        pub fn isEmpty(self: *Self) bool {
            return self.len() == 0;
        }

        /// Check if the channel is full
        pub fn isFull(self: *Self) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.count >= self.buffer.len;
        }

        /// Get the capacity of the channel
        pub fn capacity(self: *Self) usize {
            return self.buffer.len;
        }
    };
}

// ============================================================================
// Tests
// ============================================================================

test "Channel basic send/recv" {
    var ch = try Channel(u32).init(std.testing.allocator, 4);
    defer ch.deinit();

    try ch.send(1);
    try ch.send(2);
    try ch.send(3);

    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 2), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 3), ch.recv().?);
}

test "Channel tryRecv empty" {
    var ch = try Channel(u32).init(std.testing.allocator, 4);
    defer ch.deinit();

    try std.testing.expect(ch.tryRecv() == null);
}

test "Channel tryRecv non-empty" {
    var ch = try Channel(u32).init(std.testing.allocator, 4);
    defer ch.deinit();

    try ch.send(42);
    try std.testing.expectEqual(@as(u32, 42), ch.tryRecv().?);
    try std.testing.expect(ch.tryRecv() == null);
}

test "Channel close" {
    var ch = try Channel(u32).init(std.testing.allocator, 4);
    defer ch.deinit();

    try ch.send(1);
    ch.close();

    // Can still receive pending items
    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);
    // Then returns null
    try std.testing.expect(ch.recv() == null);
}

test "Channel send after close" {
    var ch = try Channel(u32).init(std.testing.allocator, 4);
    defer ch.deinit();

    ch.close();

    const result = ch.send(1);
    try std.testing.expectError(error.Closed, result);
}

test "Channel len and capacity" {
    var ch = try Channel(u32).init(std.testing.allocator, 8);
    defer ch.deinit();

    try std.testing.expectEqual(@as(usize, 8), ch.capacity());
    try std.testing.expectEqual(@as(usize, 0), ch.len());
    try std.testing.expect(ch.isEmpty());

    try ch.send(1);
    try ch.send(2);

    try std.testing.expectEqual(@as(usize, 2), ch.len());
    try std.testing.expect(!ch.isEmpty());
}

test "Channel ring buffer wrap-around" {
    var ch = try Channel(u32).init(std.testing.allocator, 3);
    defer ch.deinit();

    // Fill buffer
    try ch.send(1);
    try ch.send(2);
    try ch.send(3);

    // Consume one
    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);

    // Add another (should wrap around)
    try ch.send(4);

    // Verify order
    try std.testing.expectEqual(@as(u32, 2), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 3), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 4), ch.recv().?);
}

test "Channel with struct type" {
    const Item = struct {
        id: u32,
        name: []const u8,
    };

    var ch = try Channel(Item).init(std.testing.allocator, 4);
    defer ch.deinit();

    try ch.send(.{ .id = 1, .name = "first" });
    try ch.send(.{ .id = 2, .name = "second" });

    const item1 = ch.recv().?;
    try std.testing.expectEqual(@as(u32, 1), item1.id);
    try std.testing.expectEqualStrings("first", item1.name);

    const item2 = ch.recv().?;
    try std.testing.expectEqual(@as(u32, 2), item2.id);
    try std.testing.expectEqualStrings("second", item2.name);
}
