//! The source region each of a module's type variables reports problems at.
//!
//! A variable that is a CIR node present when checking began has that node's
//! region, read from the node store. Every later variable gets its region
//! once, when the checker creates it, and variables are created in increasing
//! order, so their regions are kept as segments in that order: a run of
//! variables sharing one region is one segment, and the variables one
//! instantiation copied are one segment naming the variable each was copied
//! from, whose region the copy has. Nothing is stored per variable except a
//! copy's origin.

const std = @import("std");
const base = @import("base");
const types = @import("types");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const Var = types.Var;

const Self = @This();

/// Variables below this are the CIR nodes present when checking began.
node_count: u32,
/// Segments cover the variables from `node_count` up to this one. A later
/// variable has no region yet and reads as the zero region.
covered: u32,
/// The first variable of each segment, increasing. A segment runs to the
/// next one's first variable, the last to `covered`.
firsts: std.ArrayListUnmanaged(u32) = .empty,
/// What each segment's variables report, parallel to `firsts`.
sources: std.ArrayListUnmanaged(Source) = .empty,
/// For the segments of copied variables, the variable each was copied from.
origins: std.ArrayListUnmanaged(Var) = .empty,

const Source = union(enum) {
    /// Every variable of the segment has this region.
    region: Region,
    /// The segment's variables are copies: variable `first + i` has the
    /// region of `origins[start + i]`.
    copies: u32,
};

/// The origin of a copy that was copied from no variable; it reads as the
/// zero region.
pub const no_origin: Var = @fromBackingInt(std.math.maxInt(u32));

/// The variables one instantiation copied: `len` of them from `first` on.
pub const Copies = struct {
    first: u32,
    len: u32,
    /// Where their origins start in `origins`.
    start: u32,

    /// No copies: every origin set on it is of a variable it does not hold.
    pub const none: Copies = .{ .first = 0, .len = 0, .start = 0 };
};

/// A position to truncate back to, undoing every region given since.
pub const Mark = struct { covered: u32, segments: u32, origins: u32 };

pub fn init(node_count: u32) Self {
    return .{ .node_count = node_count, .covered = node_count };
}

pub fn deinit(self: *Self, gpa: Allocator) void {
    self.firsts.deinit(gpa);
    self.sources.deinit(gpa);
    self.origins.deinit(gpa);
}

/// The current position, for `truncate`.
pub fn mark(self: *const Self) Mark {
    return .{ .covered = self.covered, .segments = @intCast(self.firsts.items.len), .origins = @intCast(self.origins.items.len) };
}

/// Forget the regions of every variable given one since `to` was taken.
/// Those variables no longer exist.
pub fn truncate(self: *Self, to: Mark) void {
    std.debug.assert(to.covered <= self.covered and to.segments <= self.firsts.items.len and to.origins <= self.origins.items.len);
    self.covered = to.covered;
    self.firsts.shrinkRetainingCapacity(to.segments);
    self.sources.shrinkRetainingCapacity(to.segments);
    self.origins.shrinkRetainingCapacity(to.origins);
}

/// The region of `var_`. `node_regions` is the node store's region of each
/// CIR node.
pub fn get(self: *const Self, var_: Var, node_regions: []const Region) Region {
    var current: u32 = @backingInt(var_);
    while (true) {
        if (current < self.node_count) return node_regions[current];
        if (current >= self.covered) return Region.zero();
        const segment = self.segmentOf(current);
        switch (self.sources.items[segment]) {
            .region => |region| return region,
            .copies => |start| {
                const origin = self.origins.items[start + (current - self.firsts.items[segment])];
                if (origin == no_origin) return Region.zero();
                // A copy is made after the variable it copies.
                std.debug.assert(@backingInt(origin) < current);
                current = @backingInt(origin);
            },
        }
    }
}

/// The segment holding `var_index`, which a segment covers.
fn segmentOf(self: *const Self, var_index: u32) usize {
    const firsts = self.firsts.items;
    std.debug.assert(firsts.len != 0 and var_index >= firsts[0] and var_index < self.covered);
    // Most lookups are of a recently created variable.
    if (var_index >= firsts[firsts.len - 1]) return firsts.len - 1;
    // The last segment that starts at or before `var_index` is always among
    // the `span` segments from `first`. Each step selects a half instead of
    // branching on the comparison, whose outcome is unpredictable.
    var first: usize = 0;
    var span: usize = firsts.len;
    while (span > 1) {
        const half = span / 2;
        first = if (firsts[first + half] <= var_index) first + half else first;
        span -= half;
    }
    return first;
}

/// Give `var_` the region `region`. A variable is given its region once, and
/// no variable created before it is given one afterwards.
pub fn set(self: *Self, gpa: Allocator, var_: Var, region: Region) Allocator.Error!void {
    const index: u32 = @backingInt(var_);
    try self.setRange(gpa, index, index + 1, region);
}

/// Whether `var_` has been given a region, or passed over without one.
pub fn isCovered(self: *const Self, var_: Var) bool {
    return @backingInt(var_) < self.covered;
}

/// Give every variable in `[first, end)` the region `region`. None of them
/// has a region yet.
pub fn setRange(self: *Self, gpa: Allocator, first: u32, end: u32, region: Region) Allocator.Error!void {
    std.debug.assert(first >= self.covered and end >= first);
    if (end == first) return;
    try self.cover(gpa, first);
    try self.appendRegion(gpa, first, region);
    self.covered = end;
}

/// Make the variables in `[first, end)` copies, none of which has a region
/// yet. Each is copied from no variable until `setOrigin` names one.
pub fn addCopies(self: *Self, gpa: Allocator, first: u32, end: u32) Allocator.Error!Copies {
    std.debug.assert(first >= self.covered and end >= first);
    const start: u32 = @intCast(self.origins.items.len);
    if (end == first) return .{ .first = first, .len = 0, .start = start };
    try self.cover(gpa, first);
    try self.firsts.ensureUnusedCapacity(gpa, 1);
    try self.sources.ensureUnusedCapacity(gpa, 1);
    try self.origins.appendNTimes(gpa, no_origin, end - first);
    self.firsts.appendAssumeCapacity(first);
    self.sources.appendAssumeCapacity(.{ .copies = start });
    self.covered = end;
    return .{ .first = first, .len = end - first, .start = start };
}

/// Record that `copy` was copied from `origin`, when `copy` is one of
/// `copies`. A variable the instantiation mapped to without creating it is
/// not, and keeps its own region.
pub fn setOrigin(self: *Self, copies: Copies, copy: Var, origin: Var) void {
    const index: u32 = @backingInt(copy);
    if (index < copies.first or index - copies.first >= copies.len) return;
    self.origins.items[copies.start + (index - copies.first)] = origin;
}

/// Cover the variables between `covered` and `first`, which were created
/// without a region, with the zero region.
fn cover(self: *Self, gpa: Allocator, first: u32) Allocator.Error!void {
    if (first == self.covered) return;
    try self.appendRegion(gpa, self.covered, Region.zero());
    self.covered = first;
}

/// Start a segment of `region` at `first`, which is `covered`, unless the
/// last segment already has that region and so extends over it.
fn appendRegion(self: *Self, gpa: Allocator, first: u32, region: Region) Allocator.Error!void {
    std.debug.assert(first == self.covered);
    if (self.sources.items.len != 0) {
        switch (self.sources.items[self.sources.items.len - 1]) {
            .region => |last| if (regionsEql(last, region)) return,
            .copies => {},
        }
    }
    try self.firsts.ensureUnusedCapacity(gpa, 1);
    try self.sources.ensureUnusedCapacity(gpa, 1);
    self.firsts.appendAssumeCapacity(first);
    self.sources.appendAssumeCapacity(.{ .region = region });
}

fn regionsEql(a: Region, b: Region) bool {
    return a.start.offset == b.start.offset and a.end.offset == b.end.offset;
}

fn testRegion(start: u32, end: u32) Region {
    return .{ .start = .{ .offset = start }, .end = .{ .offset = end } };
}

test "a node variable has its node's region and later variables their segment's" {
    const gpa = std.testing.allocator;
    const nodes = [_]Region{ testRegion(0, 1), testRegion(1, 2), testRegion(2, 3) };
    var regions = Self.init(nodes.len);
    defer regions.deinit(gpa);

    try std.testing.expectEqual(testRegion(1, 2), regions.get(@fromBackingInt(1), &nodes));
    // A variable with no region yet reads as the zero region.
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(3), &nodes));

    try regions.set(gpa, @fromBackingInt(3), testRegion(10, 20));
    try regions.set(gpa, @fromBackingInt(4), testRegion(10, 20));
    try regions.set(gpa, @fromBackingInt(5), testRegion(30, 40));
    // Variables 6 and 7 were created without a region.
    try regions.setRange(gpa, 8, 11, testRegion(30, 40));
    try std.testing.expectEqual(testRegion(10, 20), regions.get(@fromBackingInt(3), &nodes));
    try std.testing.expectEqual(testRegion(10, 20), regions.get(@fromBackingInt(4), &nodes));
    try std.testing.expectEqual(testRegion(30, 40), regions.get(@fromBackingInt(5), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(6), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(7), &nodes));
    try std.testing.expectEqual(testRegion(30, 40), regions.get(@fromBackingInt(10), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(11), &nodes));
    // One segment per run: 3-4, 5, the gap 6-7, 8-10.
    try std.testing.expectEqual(@as(usize, 4), regions.firsts.items.len);
}

test "a copy has the region of the variable it was copied from" {
    const gpa = std.testing.allocator;
    const nodes = [_]Region{ testRegion(0, 1), testRegion(1, 2) };
    var regions = Self.init(nodes.len);
    defer regions.deinit(gpa);

    try regions.set(gpa, @fromBackingInt(2), testRegion(5, 6));
    const first_copies = try regions.addCopies(gpa, 3, 6);
    regions.setOrigin(first_copies, @fromBackingInt(3), @fromBackingInt(1));
    regions.setOrigin(first_copies, @fromBackingInt(5), @fromBackingInt(2));
    // A variable the instantiation shared is not one of its copies.
    regions.setOrigin(first_copies, @fromBackingInt(2), @fromBackingInt(1));
    // A copy of a copy reads through both.
    const second_copies = try regions.addCopies(gpa, 6, 8);
    regions.setOrigin(second_copies, @fromBackingInt(6), @fromBackingInt(5));
    regions.setOrigin(second_copies, @fromBackingInt(7), @fromBackingInt(4));

    try std.testing.expectEqual(testRegion(1, 2), regions.get(@fromBackingInt(3), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(4), &nodes));
    try std.testing.expectEqual(testRegion(5, 6), regions.get(@fromBackingInt(5), &nodes));
    try std.testing.expectEqual(testRegion(5, 6), regions.get(@fromBackingInt(6), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(7), &nodes));
    // The shared variable kept its own region.
    try std.testing.expectEqual(testRegion(5, 6), regions.get(@fromBackingInt(2), &nodes));
}

test "truncating forgets every region given since the mark" {
    const gpa = std.testing.allocator;
    const nodes = [_]Region{testRegion(0, 1)};
    var regions = Self.init(nodes.len);
    defer regions.deinit(gpa);

    try regions.set(gpa, @fromBackingInt(1), testRegion(5, 6));
    const before = regions.mark();
    // The run continues into the speculative variables, then copies follow.
    try regions.set(gpa, @fromBackingInt(2), testRegion(5, 6));
    const copies = try regions.addCopies(gpa, 3, 5);
    regions.setOrigin(copies, @fromBackingInt(3), @fromBackingInt(1));
    regions.truncate(before);

    try std.testing.expectEqual(testRegion(5, 6), regions.get(@fromBackingInt(1), &nodes));
    try std.testing.expectEqual(Region.zero(), regions.get(@fromBackingInt(2), &nodes));
    // The same variables, created again, get regions again.
    try regions.set(gpa, @fromBackingInt(2), testRegion(7, 8));
    try std.testing.expectEqual(testRegion(7, 8), regions.get(@fromBackingInt(2), &nodes));
    try std.testing.expectEqual(testRegion(5, 6), regions.get(@fromBackingInt(1), &nodes));
}

test "a lookup below the last segment finds the segment holding the variable" {
    const gpa = std.testing.allocator;
    const nodes = [_]Region{};
    var regions = Self.init(0);
    defer regions.deinit(gpa);

    // Segment `i` holds variables `3 * i` to `3 * i + 2`.
    for (0..50) |i| {
        try regions.setRange(gpa, @intCast(3 * i), @intCast(3 * i + 3), testRegion(@intCast(i), @intCast(i + 1)));
    }
    for (0..150) |index| {
        const segment: u32 = @intCast(index / 3);
        try std.testing.expectEqual(testRegion(segment, segment + 1), regions.get(@fromBackingInt(@intCast(index)), &nodes));
    }
}
