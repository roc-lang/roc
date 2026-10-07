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
pub fn canMiss(
    comptime Adapter: type,
    adapter: Adapter,
    allocator: std.mem.Allocator,
    pattern_id: Adapter.PatternId,
) std.mem.Allocator.Error!bool {
    // A pattern can miss when any component can, so components are checked
    // from a worklist in any order.
    var stack_allocator_state = std.heap.stackFallback(1024, allocator);
    const stack_allocator = stack_allocator_state.get();
    var pending = std.ArrayList(Adapter.PatternId).empty;
    defer pending.deinit(stack_allocator);
    try pending.append(stack_allocator, pattern_id);
    while (pending.pop()) |current| switch (adapter.patternClass(current)) {
        .cannot_miss => {},
        .can_miss => return true,
        .child => try pending.append(stack_allocator, adapter.child(current)),
        .sequence => {
            var index: usize = 0;
            while (index < adapter.sequenceLen(current)) : (index += 1) {
                try pending.append(stack_allocator, adapter.sequenceChild(current, index));
            }
        },
        .record => {
            var index: usize = 0;
            while (index < adapter.recordLen(current)) : (index += 1) {
                try pending.append(stack_allocator, adapter.recordChild(current, index));
            }
        },
        .list => {
            if (adapter.listFixedLen(current) != 0) return true;
            if (!adapter.listHasRest(current)) return true;
            if (adapter.listRestPattern(current)) |rest| try pending.append(stack_allocator, rest);
        },
    };
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

    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .empty_list));
    try std.testing.expect(!try canMiss(TestAdapter, adapter, std.testing.allocator, .rest_list));
    try std.testing.expect(!try canMiss(TestAdapter, adapter, std.testing.allocator, .rest_bind_list));
    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .fixed_rest_list));
    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .rest_literal_list));
}

test "children determine compound pattern refutability" {
    const adapter = TestAdapter{};

    try std.testing.expect(!try canMiss(TestAdapter, adapter, std.testing.allocator, .tuple_wildcard));
    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .tuple_literal));
    try std.testing.expect(!try canMiss(TestAdapter, adapter, std.testing.allocator, .record_wildcard));
    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .record_literal));
    try std.testing.expect(try canMiss(TestAdapter, adapter, std.testing.allocator, .as_literal));
}
