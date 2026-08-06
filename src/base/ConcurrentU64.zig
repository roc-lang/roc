//! A u64 counter that remains usable on targets without native 64-bit atomics.

const std = @import("std");
const builtin = @import("builtin");

const use_scalar = builtin.single_threaded or
    builtin.target.os.tag == .freestanding or
    builtin.target.cpu.arch == .wasm32;
const use_atomic_u64 = !use_scalar and @bitSizeOf(usize) >= @bitSizeOf(u64);

/// A full-width unsigned counter for shared timing and memory statistics.
pub const ConcurrentU64 = if (use_scalar) struct {
    value: u64 = 0,

    pub fn init(value: u64) @This() {
        return .{ .value = value };
    }

    pub fn load(self: *const @This()) u64 {
        return self.value;
    }

    pub fn add(self: *@This(), amount: u64) void {
        self.value +%= amount;
    }

    pub fn min(self: *@This(), sample: u64) void {
        if (sample < self.value) self.value = sample;
    }

    pub fn max(self: *@This(), sample: u64) void {
        if (sample > self.value) self.value = sample;
    }
} else if (use_atomic_u64) struct {
    value: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),

    pub fn init(value: u64) @This() {
        return .{ .value = std.atomic.Value(u64).init(value) };
    }

    pub fn load(self: *const @This()) u64 {
        return self.value.load(.monotonic);
    }

    pub fn add(self: *@This(), amount: u64) void {
        _ = self.value.fetchAdd(amount, .monotonic);
    }

    pub fn min(self: *@This(), sample: u64) void {
        var current = self.value.load(.monotonic);
        while (sample < current) {
            current = self.value.cmpxchgWeak(current, sample, .monotonic, .monotonic) orelse return;
        }
    }

    pub fn max(self: *@This(), sample: u64) void {
        var current = self.value.load(.monotonic);
        while (sample > current) {
            current = self.value.cmpxchgWeak(current, sample, .monotonic, .monotonic) orelse return;
        }
    }
} else struct {
    mutex: std.atomic.Mutex = .unlocked,
    value: u64 = 0,

    pub fn init(value: u64) @This() {
        return .{ .value = value };
    }

    pub fn load(self: *const @This()) u64 {
        const mutable: *@This() = @constCast(self);
        mutable.lock();
        defer mutable.unlock();
        return mutable.value;
    }

    pub fn add(self: *@This(), amount: u64) void {
        self.lock();
        defer self.unlock();
        self.value +%= amount;
    }

    pub fn min(self: *@This(), sample: u64) void {
        self.lock();
        defer self.unlock();
        if (sample < self.value) self.value = sample;
    }

    pub fn max(self: *@This(), sample: u64) void {
        self.lock();
        defer self.unlock();
        if (sample > self.value) self.value = sample;
    }

    fn lock(self: *@This()) void {
        while (!self.mutex.tryLock()) {
            std.atomic.spinLoopHint();
        }
    }

    fn unlock(self: *@This()) void {
        self.mutex.unlock();
    }
};

test "ConcurrentU64 supports totals and extrema" {
    var total = ConcurrentU64.init(1);
    total.add(2);
    total.add(3);
    try std.testing.expectEqual(@as(u64, 6), total.load());

    var min_value = ConcurrentU64.init(std.math.maxInt(u64));
    min_value.min(10);
    min_value.min(12);
    min_value.min(4);
    try std.testing.expectEqual(@as(u64, 4), min_value.load());

    var max_value = ConcurrentU64.init(0);
    max_value.max(10);
    max_value.max(4);
    max_value.max(12);
    try std.testing.expectEqual(@as(u64, 12), max_value.load());
}
