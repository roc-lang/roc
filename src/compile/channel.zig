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
const threading = @import("threading.zig");
const Allocator = std.mem.Allocator;

const Mutex = threading.Mutex;
const Condition = threading.Condition;

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
        /// System IO for mutex/condvar/timestamp operations
        std_io: std.Io,

        /// Initialize a channel with the given capacity
        pub fn init(gpa: Allocator, cap_size: usize, std_io: std.Io) !Self {
            const cap = if (cap_size == 0) DEFAULT_CAPACITY else cap_size;
            const buffer = try gpa.alloc(T, cap);
            return .{
                .buffer = buffer,
                .write_pos = 0,
                .read_pos = 0,
                .count = 0,
                .mutex = Mutex.init,
                .not_empty = Condition.init,
                .not_full = Condition.init,
                .closed = false,
                .gpa = gpa,
                .std_io = std_io,
            };
        }

        /// Free the channel's resources
        pub fn deinit(self: *Self) void {
            self.gpa.free(self.buffer);
        }

        /// Send an item to the channel, blocking if full.
        /// Returns error.Closed if the channel has been closed.
        pub fn send(self: *Self, item: T) ChannelError!void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);

            // Wait while channel is full and not closed
            while (self.count >= self.buffer.len and !self.closed) {
                self.not_full.waitUncancelable(self.std_io, &self.mutex);
            }

            if (self.closed) {
                return error.Closed;
            }

            // Add item to buffer
            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % self.buffer.len;
            self.count += 1;

            // Signal that channel is non-empty
            self.not_empty.signal(self.std_io);
        }

        /// Send an item, growing the buffer if full (never blocks on capacity).
        /// Use this when the sender must remain responsive and cannot afford to
        /// block — e.g. a coordinator that also needs to drain another channel.
        pub fn sendGrowable(self: *Self, item: T) error{ Closed, OutOfMemory }!void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);

            if (self.closed) return error.Closed;

            // Grow ring buffer if full
            if (self.count >= self.buffer.len) {
                const new_cap = self.buffer.len * 2;
                const new_buffer = self.gpa.alloc(T, new_cap) catch return error.OutOfMemory;

                // Linearize the ring into the new buffer
                var i: usize = 0;
                var pos = self.read_pos;
                while (i < self.count) : (i += 1) {
                    new_buffer[i] = self.buffer[pos];
                    pos = (pos + 1) % self.buffer.len;
                }

                self.gpa.free(self.buffer);
                self.buffer = new_buffer;
                self.read_pos = 0;
                self.write_pos = self.count;

                // Wake any producers blocked in send() — there is room now.
                self.not_full.broadcast(self.std_io);
            }

            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % self.buffer.len;
            self.count += 1;

            self.not_empty.signal(self.std_io);
        }

        /// Send an item with a timeout (in nanoseconds).
        /// Returns error.Timeout if the operation times out.
        /// Returns error.Closed if the channel has been closed.
        pub fn sendTimeout(self: *Self, item: T, timeout_ns: u64) ChannelError!void {
            const deadline_ns = std.Io.Timestamp.now(self.std_io, .real).nanoseconds + @as(i96, @intCast(timeout_ns));

            while (true) {
                self.mutex.lockUncancelable(self.std_io);

                if (self.count < self.buffer.len or self.closed) break;

                const now_ns = std.Io.Timestamp.now(self.std_io, .real).nanoseconds;
                if (now_ns >= deadline_ns) {
                    self.mutex.unlock(self.std_io);
                    return error.Timeout;
                }

                self.mutex.unlock(self.std_io);

                // Sleep for up to 1ms, waking early if deadline arrives.
                // std.Io.Condition has no waitTimeout; polling is the simplest
                // correct approach for the coordinator's coarse timeouts.
                if (comptime !threading.is_freestanding) {
                    const remaining: i96 = deadline_ns - now_ns;
                    std.Io.sleep(self.std_io, .{ .nanoseconds = @min(remaining, 1_000_000) }, .real) catch {};
                }
            }
            defer self.mutex.unlock(self.std_io);

            if (self.closed) return error.Closed;

            self.buffer[self.write_pos] = item;
            self.write_pos = (self.write_pos + 1) % self.buffer.len;
            self.count += 1;
            self.not_empty.signal(self.std_io);
        }

        /// Receive an item from the channel, blocking if empty.
        /// Returns null if the channel is closed and empty.
        pub fn recv(self: *Self) ?T {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);

            // Wait while channel is empty and not closed
            while (self.count == 0 and !self.closed) {
                self.not_empty.waitUncancelable(self.std_io, &self.mutex);
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
            const deadline_ns = std.Io.Timestamp.now(self.std_io, .real).nanoseconds + @as(i96, @intCast(timeout_ns));

            while (true) {
                self.mutex.lockUncancelable(self.std_io);

                if (self.count > 0 or self.closed) break;

                const now_ns = std.Io.Timestamp.now(self.std_io, .real).nanoseconds;
                if (now_ns >= deadline_ns) {
                    self.mutex.unlock(self.std_io);
                    return null;
                }

                self.mutex.unlock(self.std_io);

                if (comptime !threading.is_freestanding) {
                    const remaining: i96 = deadline_ns - now_ns;
                    std.Io.sleep(self.std_io, .{ .nanoseconds = @min(remaining, 1_000_000) }, .real) catch {};
                }
            }
            defer self.mutex.unlock(self.std_io);

            if (self.count == 0) return null;
            return self.recvLocked();
        }

        /// Try to receive an item without blocking.
        /// Returns null if the channel is empty.
        pub fn tryRecv(self: *Self) ?T {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);

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
            self.not_full.signal(self.std_io);

            return item;
        }

        /// Close the channel.
        /// Any blocked senders will receive error.Closed.
        /// Any blocked receivers will be woken and return null if empty.
        pub fn close(self: *Self) void {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);

            self.closed = true;

            // Wake all waiting threads
            self.not_empty.broadcast(self.std_io);
            self.not_full.broadcast(self.std_io);
        }

        /// Check if the channel is closed
        pub fn isClosed(self: *Self) bool {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            return self.closed;
        }

        /// Get the number of items currently in the channel
        pub fn len(self: *Self) usize {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            return self.count;
        }

        /// Check if the channel is empty
        pub fn isEmpty(self: *Self) bool {
            return self.len() == 0;
        }

        /// Check if the channel is full
        pub fn isFull(self: *Self) bool {
            self.mutex.lockUncancelable(self.std_io);
            defer self.mutex.unlock(self.std_io);
            return self.count >= self.buffer.len;
        }

        /// Get the capacity of the channel
        pub fn capacity(self: *Self) usize {
            return self.buffer.len;
        }
    };
}

test "Channel basic send/recv" {
    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    try ch.send(1);
    try ch.send(2);
    try ch.send(3);

    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 2), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 3), ch.recv().?);
}

test "Channel tryRecv empty" {
    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    try std.testing.expect(ch.tryRecv() == null);
}

test "Channel tryRecv non-empty" {
    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    try ch.send(42);
    try std.testing.expectEqual(@as(u32, 42), ch.tryRecv().?);
    try std.testing.expect(ch.tryRecv() == null);
}

test "Channel close" {
    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    try ch.send(1);
    ch.close();

    // Can still receive pending items
    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);
    // Then returns null
    try std.testing.expect(ch.recv() == null);
}

test "Channel send after close" {
    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    ch.close();

    const result = ch.send(1);
    try std.testing.expectError(error.Closed, result);
}

test "Channel sendGrowable grows buffer when full" {
    var ch = try Channel(u32).init(std.testing.allocator, 2, std.testing.io);
    defer ch.deinit();

    // Fill to capacity
    try ch.sendGrowable(1);
    try ch.sendGrowable(2);
    try std.testing.expectEqual(@as(usize, 2), ch.capacity());

    // This should grow the buffer instead of blocking
    try ch.sendGrowable(3);
    try std.testing.expectEqual(@as(usize, 4), ch.capacity());
    try std.testing.expectEqual(@as(usize, 3), ch.len());

    // Verify items come out in order
    try std.testing.expectEqual(@as(u32, 1), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 2), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 3), ch.recv().?);
}

test "Channel sendGrowable with wrap-around growth" {
    var ch = try Channel(u32).init(std.testing.allocator, 3, std.testing.io);
    defer ch.deinit();

    // Fill and partially drain to create wrap-around state
    try ch.send(10);
    try ch.send(20);
    try ch.send(30);
    try std.testing.expectEqual(@as(u32, 10), ch.recv().?); // read_pos=1
    try ch.send(40); // wraps: write_pos=0

    // Buffer is full with [40, _, 20, 30] (read_pos=1, write_pos=1, count=3)
    // sendGrowable should linearize and grow
    try ch.sendGrowable(50);
    try std.testing.expectEqual(@as(usize, 6), ch.capacity());
    try std.testing.expectEqual(@as(usize, 4), ch.len());

    // Verify order is preserved
    try std.testing.expectEqual(@as(u32, 20), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 30), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 40), ch.recv().?);
    try std.testing.expectEqual(@as(u32, 50), ch.recv().?);
}

test "Channel len and capacity" {
    var ch = try Channel(u32).init(std.testing.allocator, 8, std.testing.io);
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
    var ch = try Channel(u32).init(std.testing.allocator, 3, std.testing.io);
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

    var ch = try Channel(Item).init(std.testing.allocator, 4, std.testing.io);
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

test "Channel multi-producer single-consumer" {
    // Skip on wasm where threads aren't available
    if (threading.is_freestanding) return error.SkipZigTest;

    var ch = try Channel(u32).init(std.testing.allocator, 16, std.testing.io);
    defer ch.deinit();

    const num_producers = 4;
    const items_per_producer = 100;

    // Spawn producer threads
    var threads: [num_producers]std.Thread = undefined;
    for (0..num_producers) |i| {
        threads[i] = try std.Thread.spawn(.{}, struct {
            fn producer(channel: *Channel(u32), producer_id: u32) void {
                for (0..items_per_producer) |j| {
                    const value = producer_id * 1000 + @as(u32, @intCast(j));
                    channel.send(value) catch return;
                }
            }
        }.producer, .{ &ch, @as(u32, @intCast(i)) });
    }

    // Consumer: receive all items
    var received: usize = 0;
    const expected_total = num_producers * items_per_producer;
    while (received < expected_total) {
        if (ch.recv()) |_| {
            received += 1;
        }
    }

    // Wait for all producers to finish
    for (&threads) |*t| {
        t.join();
    }

    try std.testing.expectEqual(expected_total, received);
    try std.testing.expect(ch.isEmpty());
}

test "Channel blocking recv with timeout" {
    // Skip on wasm where threads aren't available
    if (threading.is_freestanding) return error.SkipZigTest;

    var ch = try Channel(u32).init(std.testing.allocator, 4, std.testing.io);
    defer ch.deinit();

    // recvTimeout on empty channel should return null after timeout
    const test_io = std.testing.io;
    const start = std.Io.Timestamp.now(test_io, .real).nanoseconds;
    const result = ch.recvTimeout(10_000_000); // 10ms
    const elapsed = std.Io.Timestamp.now(test_io, .real).nanoseconds - start;

    try std.testing.expect(result == null);
    try std.testing.expect(elapsed >= 10_000_000); // Should have waited at least 10ms
}

test "Channel producer-consumer coordination" {
    // Skip on wasm where threads aren't available
    if (threading.is_freestanding) return error.SkipZigTest;

    var ch = try Channel(u32).init(std.testing.allocator, 2, std.testing.io); // Small buffer to test blocking
    defer ch.deinit();

    // Producer thread sends values with small delay
    const producer = try std.Thread.spawn(.{}, struct {
        fn run(channel: *Channel(u32)) void {
            for (0..5) |i| {
                // 1ms delay
                _ = std.c.nanosleep(&.{ .sec = 0, .nsec = 1_000_000 }, null);
                channel.send(@as(u32, @intCast(i))) catch return;
            }
        }
    }.run, .{&ch});

    // Consumer receives all values
    var sum: u32 = 0;
    for (0..5) |_| {
        if (ch.recv()) |val| {
            sum += val;
        }
    }

    producer.join();

    try std.testing.expectEqual(@as(u32, 0 + 1 + 2 + 3 + 4), sum);
}
