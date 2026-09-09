//! Exact persistent sets of committed struct-field identities for certification.

const std = @import("std");
const Snapshot = @import("arc_state.zig").Snapshot;

/// Ordinary records use one inline word. Higher semantic field indices use
/// shared sparse snapshots, so a control-flow fork never copies a wide bitmap.
pub const Set = struct {
    low: u64 = 0,
    high: ?*const High = null,

    const High = struct {
        words: Snapshot(u64, 0),
        last_word: u16,
        count: u16,
    };

    pub fn isEmpty(self: Set) bool {
        return self.low == 0 and self.high == null;
    }

    pub fn contains(self: Set, field: u16) bool {
        const bit = @as(u64, 1) << @as(u6, @intCast(field % 64));
        if (field < 64) return self.low & bit != 0;
        const high = self.high orelse return false;
        return high.words.get(field / 64) & bit != 0;
    }

    /// Use an arena that outlives every snapshot sharing this set.
    pub fn withField(self: Set, allocator: std.mem.Allocator, field: u16) std.mem.Allocator.Error!Set {
        if (self.contains(field)) return self;
        var result = self;
        const bit = @as(u64, 1) << @as(u6, @intCast(field % 64));
        if (field < 64) {
            result.low |= bit;
        } else {
            var high: High = if (self.high) |previous| previous.* else .{
                .words = Snapshot(u64, 0).init(allocator, @as(usize, std.math.maxInt(u16)) / 64 + 1),
                .last_word = 0,
                .count = 0,
            };
            const word = field / 64;
            high.words.allocator = allocator;
            try high.words.put(word, high.words.get(word) | bit);
            high.last_word = @max(high.last_word, word);
            high.count += 1;
            const stored = try allocator.create(High);
            stored.* = high;
            result.high = stored;
        }
        return result;
    }

    pub fn isSingleton(self: Set, field: u16) bool {
        const count: usize = @as(usize, @popCount(self.low)) + if (self.high) |high| high.count else @as(usize, 0);
        return count == 1 and self.contains(field);
    }

    pub fn eql(self: Set, other: Set) bool {
        if (self.low != other.low) return false;
        if (self.high == other.high) return true;
        const left = self.high orelse return false;
        const right = other.high orelse return false;
        return left.count == right.count and left.last_word == right.last_word and left.words.eql(&right.words);
    }

    pub fn hashInto(self: Set, hasher: *std.hash.Wyhash) void {
        hasher.update(std.mem.asBytes(&self.low));
        const last_word: u16 = if (self.high) |high| high.last_word else 0;
        hasher.update(std.mem.asBytes(&last_word));
        if (self.high) |high| {
            for (1..@as(usize, last_word) + 1) |index| {
                const word = high.words.get(@intCast(index));
                hasher.update(std.mem.asBytes(&word));
            }
        }
    }
};

test "field claims preserve inline and wide identities across forks" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const fields = [_]u16{ 0, 63, 64, 128, std.math.maxInt(u16) };
    var forward: Set = .{};
    for (fields) |field| {
        const prior = forward;
        forward = try forward.withField(allocator, field);
        try std.testing.expect(!prior.contains(field));
        try std.testing.expect(forward.contains(field));
    }
    var reverse: Set = .{};
    var remaining = fields.len;
    while (remaining != 0) {
        remaining -= 1;
        reverse = try reverse.withField(allocator, fields[remaining]);
    }
    try std.testing.expect(forward.eql(reverse));
    var forward_hash = std.hash.Wyhash.init(0);
    var reverse_hash = std.hash.Wyhash.init(0);
    forward.hashInto(&forward_hash);
    reverse.hashInto(&reverse_hash);
    try std.testing.expectEqual(forward_hash.final(), reverse_hash.final());
    const different = try reverse.withField(allocator, 129);
    try std.testing.expect(!forward.eql(different));
}

test "inline field claims allocate no storage" {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
    var fields: Set = .{};
    for (0..64) |index| fields = try fields.withField(failing.allocator(), @intCast(index));
    try std.testing.expectEqual(std.math.maxInt(u64), fields.low);
    try std.testing.expect(fields.high == null);
    try std.testing.expectError(error.OutOfMemory, fields.withField(failing.allocator(), 64));
    try std.testing.expect(!fields.contains(64));
}
