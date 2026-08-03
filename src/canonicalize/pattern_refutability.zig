//! Shared syntactic refutability rules for Roc patterns.

const std = @import("std");

/// Adapter-reported syntactic shape used to recurse through a pattern.
pub const PatternClass = enum {
    cannot_miss,
    can_miss,
    child,
    sequence,
    record,
    list,
};

/// Returns whether a pattern can fail to match using only adapter-provided syntax.
pub fn canMiss(comptime Adapter: type, adapter: Adapter, pattern_id: Adapter.PatternId) bool {
    return switch (adapter.patternClass(pattern_id)) {
        .cannot_miss => false,
        .can_miss => true,
        .child => canMiss(Adapter, adapter, adapter.child(pattern_id)),
        .sequence => blk: {
            var index: usize = 0;
            while (index < adapter.sequenceLen(pattern_id)) : (index += 1) {
                if (canMiss(Adapter, adapter, adapter.sequenceChild(pattern_id, index))) break :blk true;
            }
            break :blk false;
        },
        .record => blk: {
            var index: usize = 0;
            while (index < adapter.recordLen(pattern_id)) : (index += 1) {
                if (canMiss(Adapter, adapter, adapter.recordChild(pattern_id, index))) break :blk true;
            }
            break :blk false;
        },
        .list => listCanMiss(Adapter, adapter, pattern_id),
    };
}

fn listCanMiss(comptime Adapter: type, adapter: Adapter, pattern_id: Adapter.PatternId) bool {
    if (adapter.listFixedLen(pattern_id) != 0) return true;
    if (!adapter.listHasRest(pattern_id)) return true;
    if (adapter.listRestPattern(pattern_id)) |rest| return canMiss(Adapter, adapter, rest);
    return false;
}

const TestPatternId = enum(u8) {
    wildcard,
    literal,
    as_literal,
    tuple_wildcard,
    tuple_literal,
    record_wildcard,
    record_literal,
    empty_list,
    rest_list,
    rest_bind_list,
    fixed_rest_list,
    rest_literal_list,
};

const TestPattern = union(enum) {
    wildcard,
    literal,
    child: TestPatternId,
    sequence: []const TestPatternId,
    record: []const TestPatternId,
    list: struct {
        fixed: []const TestPatternId,
        rest: ?TestPatternId,
        has_rest: bool,
    },
};

const test_patterns = [_]TestPattern{
    .wildcard,
    .literal,
    .{ .child = .literal },
    .{ .sequence = &.{.wildcard} },
    .{ .sequence = &.{.literal} },
    .{ .record = &.{.wildcard} },
    .{ .record = &.{.literal} },
    .{ .list = .{ .fixed = &.{}, .rest = null, .has_rest = false } },
    .{ .list = .{ .fixed = &.{}, .rest = null, .has_rest = true } },
    .{ .list = .{ .fixed = &.{}, .rest = .wildcard, .has_rest = true } },
    .{ .list = .{ .fixed = &.{.wildcard}, .rest = null, .has_rest = true } },
    .{ .list = .{ .fixed = &.{}, .rest = .literal, .has_rest = true } },
};

const TestAdapter = struct {
    pub const PatternId = TestPatternId;

    fn pattern(id: PatternId) TestPattern {
        return test_patterns[@intFromEnum(id)];
    }

    pub fn patternClass(_: @This(), id: PatternId) PatternClass {
        return switch (pattern(id)) {
            .wildcard => .cannot_miss,
            .literal => .can_miss,
            .child => .child,
            .sequence => .sequence,
            .record => .record,
            .list => .list,
        };
    }

    pub fn child(_: @This(), id: PatternId) PatternId {
        return switch (pattern(id)) {
            .child => |child_id| child_id,
            .wildcard, .literal, .sequence, .record, .list => unreachable,
        };
    }

    pub fn sequenceLen(_: @This(), id: PatternId) usize {
        return switch (pattern(id)) {
            .sequence => |children| children.len,
            .wildcard, .literal, .child, .record, .list => unreachable,
        };
    }

    pub fn sequenceChild(_: @This(), id: PatternId, index: usize) PatternId {
        return switch (pattern(id)) {
            .sequence => |children| children[index],
            .wildcard, .literal, .child, .record, .list => unreachable,
        };
    }

    pub fn recordLen(_: @This(), id: PatternId) usize {
        return switch (pattern(id)) {
            .record => |children| children.len,
            .wildcard, .literal, .child, .sequence, .list => unreachable,
        };
    }

    pub fn recordChild(_: @This(), id: PatternId, index: usize) PatternId {
        return switch (pattern(id)) {
            .record => |children| children[index],
            .wildcard, .literal, .child, .sequence, .list => unreachable,
        };
    }

    pub fn listFixedLen(_: @This(), id: PatternId) usize {
        return switch (pattern(id)) {
            .list => |list| list.fixed.len,
            .wildcard, .literal, .child, .sequence, .record => unreachable,
        };
    }

    pub fn listHasRest(_: @This(), id: PatternId) bool {
        return switch (pattern(id)) {
            .list => |list| list.has_rest,
            .wildcard, .literal, .child, .sequence, .record => unreachable,
        };
    }

    pub fn listRestPattern(_: @This(), id: PatternId) ?PatternId {
        return switch (pattern(id)) {
            .list => |list| list.rest,
            .wildcard, .literal, .child, .sequence, .record => unreachable,
        };
    }
};

test "list refutability distinguishes rest-only patterns" {
    const adapter = TestAdapter{};

    try std.testing.expect(canMiss(TestAdapter, adapter, .empty_list));
    try std.testing.expect(!canMiss(TestAdapter, adapter, .rest_list));
    try std.testing.expect(!canMiss(TestAdapter, adapter, .rest_bind_list));
    try std.testing.expect(canMiss(TestAdapter, adapter, .fixed_rest_list));
    try std.testing.expect(canMiss(TestAdapter, adapter, .rest_literal_list));
}

test "children determine compound pattern refutability" {
    const adapter = TestAdapter{};

    try std.testing.expect(!canMiss(TestAdapter, adapter, .tuple_wildcard));
    try std.testing.expect(canMiss(TestAdapter, adapter, .tuple_literal));
    try std.testing.expect(!canMiss(TestAdapter, adapter, .record_wildcard));
    try std.testing.expect(canMiss(TestAdapter, adapter, .record_literal));
    try std.testing.expect(canMiss(TestAdapter, adapter, .as_literal));
}
