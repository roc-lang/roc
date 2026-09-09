//! A dense bit set with isolated, nested scopes and sparse clearing.
//!
//! Each machine word records its owning scope depth. A scope's first write to
//! a word saves the previous word in an undo log; subsequent writes only set
//! bits. Clearing or leaving the scope restores just those words. Entering a
//! scope neither scans nor copies the suspended scope, even when the scopes
//! touch the same words. Depths are reused only after restoring their words,
//! so there is no generation counter to exhaust.
const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Self = @This();
const word_bits = @bitSizeOf(usize);
const Shift = std.math.Log2Int(usize);

const Word = struct {
    bits: usize = 0,
    depth: usize = 0,
};

const Undo = struct {
    index: usize,
    previous: Word,
};

/// Token for leaving a scope, in strict reverse entry order.
pub const Scope = struct {
    parent_start: usize,
    parent_depth: usize,
};

bit_length: usize,
words: []Word,
undo: std.ArrayList(Undo) = .empty,
scope_start: usize = 0,
depth: usize = 1,
/// Deterministic reset-work counter; absent from compiler builds.
test_restored_words: if (builtin.is_test) usize else void = if (builtin.is_test) 0 else {},

/// Allocate the word domain once. Later scope entries allocate nothing.
pub fn initEmpty(allocator: Allocator, bit_length: usize) Allocator.Error!Self {
    const words = try allocator.alloc(Word, bit_length / word_bits + @intFromBool(bit_length % word_bits != 0));
    @memset(words, .{});
    return .{ .bit_length = bit_length, .words = words };
}

/// Free the word domain and retained undo storage.
pub fn deinit(self: *Self, allocator: Allocator) void {
    self.undo.deinit(allocator);
    allocator.free(self.words);
    self.* = undefined;
}

/// Test membership in the active scope only.
pub fn isSet(self: *const Self, bit: usize) bool {
    std.debug.assert(bit < self.bit_length);
    const word = self.words[bit / word_bits];
    return word.depth == self.depth and word.bits & (@as(usize, 1) << @as(Shift, @intCast(bit % word_bits))) != 0;
}

/// Allocation failure leaves the set and the suspended scopes unchanged.
pub fn set(self: *Self, allocator: Allocator, bit: usize) Allocator.Error!void {
    std.debug.assert(bit < self.bit_length);
    const index = bit / word_bits;
    const word = &self.words[index];
    const mask = @as(usize, 1) << @as(Shift, @intCast(bit % word_bits));
    if (word.depth != self.depth) {
        try self.undo.append(allocator, .{ .index = index, .previous = word.* });
        word.* = .{ .depth = self.depth, .bits = mask };
    } else {
        word.bits |= mask;
    }
}

/// Work is proportional to the words touched in this scope, independent of
/// the bit domain and of the size of any suspended scope. Capacity is reused.
pub fn unsetAll(self: *Self) void {
    while (self.undo.items.len > self.scope_start) {
        const saved = self.undo.pop().?;
        self.words[saved.index] = saved.previous;
        if (builtin.is_test) self.test_restored_words += 1;
    }
}

/// Suspend the current scope and enter an empty one in constant time.
pub fn enterScope(self: *Self) Scope {
    const scope = Scope{ .parent_start = self.scope_start, .parent_depth = self.depth };
    self.scope_start = self.undo.items.len;
    self.depth += 1;
    return scope;
}

/// Discard this scope's marks and restore its parent, retaining undo capacity.
pub fn leaveScope(self: *Self, scope: Scope) void {
    std.debug.assert(self.depth == scope.parent_depth + 1);
    self.unsetAll();
    self.scope_start = scope.parent_start;
    self.depth = scope.parent_depth;
}

test "ScopedBitSet isolates overlapping words and restores scopes after clearing" {
    const gpa = std.testing.allocator;
    var set_ = try Self.initEmpty(gpa, 129);
    defer set_.deinit(gpa);
    try set_.set(gpa, 1);
    try set_.set(gpa, 128);
    const outer = set_.enterScope();
    try std.testing.expect(!set_.isSet(1));
    try set_.set(gpa, 2);
    const inner = set_.enterScope();
    try set_.set(gpa, 1);
    try set_.set(gpa, 2);
    set_.unsetAll();
    try std.testing.expect(!set_.isSet(1));
    try std.testing.expect(!set_.isSet(2));
    try set_.set(gpa, 128);
    set_.leaveScope(inner);
    try std.testing.expect(set_.isSet(2));
    try std.testing.expect(!set_.isSet(1));
    try std.testing.expect(!set_.isSet(128));
    set_.leaveScope(outer);
    try std.testing.expect(set_.isSet(1));
    try std.testing.expect(set_.isSet(128));
    try std.testing.expect(!set_.isSet(2));
    set_.unsetAll();
    try std.testing.expect(!set_.isSet(1));
    try std.testing.expect(!set_.isSet(128));
}

test "ScopedBitSet work and scope entry allocations depend only on touched words" {
    var counter = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    const gpa = counter.allocator();
    var set_ = try Self.initEmpty(gpa, 1 << 20);
    defer set_.deinit(gpa);
    // A large suspended scope must not be copied on each small child entry.
    for (0..4096) |i| try set_.set(gpa, i * word_bits);
    const parent_words = set_.undo.items.len;
    const warmup = set_.enterScope();
    try set_.set(gpa, 3);
    set_.leaveScope(warmup);
    const allocated = counter.allocated_bytes;
    const restored = set_.test_restored_words;
    for (0..4096) |_| {
        const scope = set_.enterScope();
        try set_.set(gpa, 3);
        try set_.set(gpa, 4);
        try std.testing.expectEqual(parent_words + 1, set_.undo.items.len);
        set_.unsetAll();
        try std.testing.expectEqual(parent_words, set_.undo.items.len);
        set_.leaveScope(scope);
        try std.testing.expect(set_.isSet(0));
    }
    try std.testing.expectEqual(allocated, counter.allocated_bytes);
    try std.testing.expectEqual(@as(usize, 4096), set_.test_restored_words - restored);
}

test "ScopedBitSet randomized scopes agree with independent bit sets" {
    const gpa = std.testing.allocator;
    var actual = try Self.initEmpty(gpa, 257);
    defer actual.deinit(gpa);
    var expected: [8]std.bit_set.IntegerBitSet(257) = @splat(.initEmpty());
    var scopes: [7]Scope = undefined;
    var depth: usize = 0;
    var prng = std.Random.DefaultPrng.init(11128);
    const random = prng.random();
    for (0..10000) |_| {
        switch (random.uintLessThan(u8, 5)) {
            0 => if (depth < scopes.len) {
                scopes[depth] = actual.enterScope();
                depth += 1;
                expected[depth] = .initEmpty();
            },
            1 => if (depth > 0) {
                depth -= 1;
                actual.leaveScope(scopes[depth]);
            },
            2 => {
                actual.unsetAll();
                expected[depth] = .initEmpty();
            },
            else => {
                const bit = random.uintLessThan(usize, 257);
                try actual.set(gpa, bit);
                expected[depth].set(bit);
            },
        }
        for (0..257) |bit| try std.testing.expectEqual(expected[depth].isSet(bit), actual.isSet(bit));
    }
}

fn allocationFailureScenario(gpa: Allocator) Allocator.Error!void {
    var set_ = try Self.initEmpty(gpa, 4096);
    defer set_.deinit(gpa);
    try set_.set(gpa, 1);
    const scope = set_.enterScope();
    defer {
        set_.leaveScope(scope);
        std.debug.assert(set_.isSet(1));
        std.debug.assert(!set_.isSet(2));
    }
    for (0..4096) |i| try set_.set(gpa, i);
}

test "ScopedBitSet allocation failure preserves suspended scopes" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationFailureScenario, .{});
}
