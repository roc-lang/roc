//! Persistent sparse snapshots for exact procedure-local ARC facts.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Debug-only work counter for exact sparse range queries.
pub var range_query_node_visits: u64 = 0;

/// Debug-only work counter for sparse enumeration, independent of ID width.
pub var iterator_node_visits: u64 = 0;

/// Debug-only count of non-shared, nonempty nodes examined by structural difference.
pub var difference_node_visits: u64 = 0;

/// A bounded-depth radix tree whose absent entries have one caller-declared
/// value. Copying a snapshot shares its root; changing one entry allocates
/// only the nodes on that entry's path. Depth grows only as needed and is
/// bounded for every `u32` index, so update cost does not depend on procedure
/// width.
pub fn Snapshot(comptime T: type, comptime empty: T) type {
    return struct {
        const Self = @This();
        // Eight-way nodes balance lookup depth against copied null child
        // pointers; wider nodes make sparse ARC path-state updates larger,
        // while narrower nodes make every query need too many branches.
        const radix_bits = 3;
        const radix = 1 << radix_bits;
        const radix_mask = radix - 1;
        const leaf_bits = radix_bits;
        const max_branch_depth = 10;

        const Branch = struct {
            children: [radix]?*const anyopaque,
        };

        const Leaf = struct {
            values: [radix]T,
        };

        allocator: Allocator,
        depth: u8,
        root: ?*const anyopaque = null,

        pub fn init(allocator: Allocator, entry_count: usize) Self {
            const highest: u32 = if (entry_count == 0) 0 else @intCast(entry_count - 1);
            const depth = depthFor(highest);
            std.debug.assert(depth <= max_branch_depth);
            return .{ .allocator = allocator, .depth = depth };
        }

        pub fn clone(self: *const Self) Self {
            return self.*;
        }

        pub fn assignShared(self: *Self, source: *const Self) void {
            std.debug.assert(self.depth == source.depth);
            self.root = source.root;
        }

        pub fn clear(self: *Self) void {
            self.root = null;
        }

        pub fn get(self: *const Self, index: u32) T {
            if (depthFor(index) > self.depth) return empty;
            var node = self.root orelse return empty;
            var depth: usize = self.depth;
            while (depth > 0) : (depth -= 1) {
                const branch: *const Branch = @ptrCast(@alignCast(node));
                const shift: u5 = @intCast(leaf_bits + (depth - 1) * radix_bits);
                const slot: usize = @intCast((index >> shift) & radix_mask);
                node = branch.children[slot] orelse return empty;
            }
            const leaf: *const Leaf = @ptrCast(@alignCast(node));
            return leaf.values[index & radix_mask];
        }

        /// Enumerates non-default entries in ascending index order. The
        /// iterator borrows this root, so shared updates may proceed while
        /// iterating; unique updates require finishing the iteration first.
        pub fn iterator(self: *const Self) Iterator {
            return self.iteratorDirection(.forward);
        }

        /// Enumerates occupied entries in the requested exact index order.
        pub fn iteratorDirection(self: *const Self, direction: @FieldType(std.bit_set.IteratorOptions, "direction")) Iterator {
            var result = Iterator{ .depth = self.depth, .reverse = direction == .reverse };
            if (self.root) |root| result.push(root, 0);
            return result;
        }

        /// Bounded-stack traversal of the borrowed immutable root.
        pub const Iterator = struct {
            /// A non-default entry and its exact snapshot index.
            pub const Entry = struct { index: u32, value: T };
            const Frame = struct { node: *const anyopaque, base: u32, slot: u8 = 0 };

            frames: [max_branch_depth + 1]Frame = undefined,
            len: usize = 0,
            depth: u8,
            reverse: bool,

            fn push(self: *Iterator, node: *const anyopaque, base: u32) void {
                if (@import("builtin").mode == .Debug) _ = @atomicRmw(u64, &iterator_node_visits, .Add, 1, .monotonic);
                self.frames[self.len] = .{ .node = node, .base = base };
                self.len += 1;
            }

            /// Returns the next occupied entry, skipping absent subtrees.
            pub fn next(self: *Iterator) ?Entry {
                while (self.len != 0) {
                    const frame = &self.frames[self.len - 1];
                    if (frame.slot == radix) {
                        self.len -= 1;
                        continue;
                    }
                    const slot = if (self.reverse) radix - 1 - frame.slot else frame.slot;
                    frame.slot += 1;
                    const remaining_depth = self.depth + 1 - self.len;
                    if (remaining_depth == 0) {
                        const leaf: *const Leaf = @ptrCast(@alignCast(frame.node));
                        const value = leaf.values[slot];
                        if (!std.meta.eql(value, empty)) return .{ .index = frame.base + slot, .value = value };
                    } else {
                        const branch: *const Branch = @ptrCast(@alignCast(frame.node));
                        if (branch.children[slot]) |child| {
                            const shift: u5 = @intCast(leaf_bits + (remaining_depth - 1) * radix_bits);
                            self.push(child, frame.base | (@as(u32, slot) << shift));
                        }
                    }
                }
                return null;
            }
        };

        /// Whether a half-open index range contains a non-default entry.
        /// Canonical empty subtrees are null, so fully covered subtrees need
        /// no descent. Only the two range boundaries can descend at each
        /// level, independently of the range's width or population.
        pub fn hasNonEmptyInRange(self: *const Self, start: u32, end: u64) bool {
            std.debug.assert(start <= end and end <= @as(u64, 1) << 32);
            if (start == end) return false;
            return rangeHasValue(self.root, self.depth, 0, start, end);
        }

        fn rangeHasValue(maybe_node: ?*const anyopaque, depth: u8, base: u64, start: u64, end: u64) bool {
            if (@import("builtin").mode == .Debug) _ = @atomicRmw(u64, &range_query_node_visits, .Add, 1, .monotonic);
            const node = maybe_node orelse return false;
            const width = @as(u64, 1) << @as(u6, @intCast(leaf_bits + @as(usize, depth) * radix_bits));
            if (end <= base or start >= base + width) return false;
            if (start <= base and base + width <= end) return true;
            if (depth == 0) {
                const leaf: *const Leaf = @ptrCast(@alignCast(node));
                const first: usize = @intCast(@max(start, base) - base);
                const last: usize = @intCast(@min(end, base + width) - base);
                for (leaf.values[first..last]) |value| {
                    if (!std.meta.eql(value, empty)) return true;
                }
                return false;
            }
            const branch: *const Branch = @ptrCast(@alignCast(node));
            const child_width = width / radix;
            const first: usize = @intCast((@max(start, base) - base) / child_width);
            const last: usize = @intCast((@min(end, base + width) - 1 - base) / child_width + 1);
            for (first..last) |slot| {
                if (rangeHasValue(branch.children[slot], depth - 1, base + slot * child_width, start, end)) return true;
            }
            return false;
        }

        pub fn put(self: *Self, index: u32, value: T) Allocator.Error!void {
            if (std.meta.eql(self.get(index), value)) return;
            const required_depth = depthFor(index);
            while (self.depth < required_depth) {
                var children = [_]?*const anyopaque{null} ** radix;
                children[0] = self.root;
                const branch = try self.allocator.create(Branch);
                branch.* = .{ .children = children };
                self.root = branch;
                self.depth += 1;
            }
            self.root = try self.putNode(self.root, self.depth, index, value);
        }

        /// Updates a snapshot whose entire tree is known to have exactly one
        /// owner. This is for constructing a fresh path state before its first
        /// control-flow fork; shared snapshots must use `put`.
        pub fn putUnique(self: *Self, index: u32, value: T) Allocator.Error!void {
            if (std.meta.eql(self.get(index), value)) return;
            const required_depth = depthFor(index);
            while (self.depth < required_depth) {
                var children = [_]?*const anyopaque{null} ** radix;
                children[0] = self.root;
                const branch = try self.allocator.create(Branch);
                branch.* = .{ .children = children };
                self.root = branch;
                self.depth += 1;
            }
            self.root = try self.putNodeUnique(self.root, self.depth, index, value);
        }

        pub fn eql(self: *const Self, other: *const Self) bool {
            if (self.root == other.root) return true;
            std.debug.assert(self.depth == other.depth);
            return eqlNode(self.root, other.root, self.depth);
        }

        /// Enumerates nonempty left entries in differing subtrees in ascending
        /// index order. Equal roots and absent left subtrees are skipped; the
        /// callback must treat equal values as no difference. Both snapshots
        /// borrow immutable roots for the duration of the traversal.
        pub fn differenceWith(
            self: *const Self,
            other: *const Self,
            context: anytype,
            comptime emitFn: fn (@TypeOf(context), u32, T, T) Allocator.Error!void,
        ) Allocator.Error!void {
            std.debug.assert(self.depth == other.depth);
            try differenceNode(self.root, other.root, self.depth, 0, context, emitFn);
        }

        fn differenceNode(
            lhs: ?*const anyopaque,
            rhs: ?*const anyopaque,
            depth: u8,
            base: u32,
            context: anytype,
            comptime emitFn: fn (@TypeOf(context), u32, T, T) Allocator.Error!void,
        ) Allocator.Error!void {
            if (lhs == rhs or lhs == null) return;
            if (@import("builtin").mode == .Debug) _ = @atomicRmw(u64, &difference_node_visits, .Add, 1, .monotonic);
            if (depth == 0) {
                const left: *const Leaf = @ptrCast(@alignCast(lhs.?));
                const right: ?*const Leaf = @ptrCast(@alignCast(rhs));
                for (left.values, 0..) |value, slot| {
                    if (std.meta.eql(value, empty)) continue;
                    try emitFn(context, base + @as(u32, @intCast(slot)), value, if (right) |leaf| leaf.values[slot] else empty);
                }
            } else {
                const left: *const Branch = @ptrCast(@alignCast(lhs.?));
                const right: ?*const Branch = @ptrCast(@alignCast(rhs));
                const shift: u5 = @intCast(leaf_bits + (depth - 1) * radix_bits);
                for (left.children, 0..) |child, slot| {
                    try differenceNode(child, if (right) |branch| branch.children[slot] else null, depth - 1, base | (@as(u32, @intCast(slot)) << shift), context, emitFn);
                }
            }
        }

        /// Pointwise meet over two snapshots. `meetFn` must be idempotent and
        /// must return `empty` when either input is `empty`; those are the
        /// lattice laws that permit pointer sharing and absent-subtree skips.
        pub fn meetWith(
            self: *Self,
            other: *const Self,
            context: anytype,
            comptime meetFn: fn (@TypeOf(context), T, T) T,
        ) Allocator.Error!bool {
            if (self.root == other.root) return false;
            std.debug.assert(self.depth == other.depth);
            const old_root = self.root;
            self.root = try self.meetNode(self.root, other.root, self.depth, context, meetFn);
            return self.root != old_root;
        }

        /// Pointwise join over two snapshots. `joinFn` must be idempotent and
        /// must return its nonempty input when the other input is `empty`.
        pub fn joinWith(
            self: *Self,
            other: *const Self,
            context: anytype,
            comptime joinFn: fn (@TypeOf(context), T, T) T,
        ) Allocator.Error!bool {
            if (self.root == other.root or other.root == null) return false;
            std.debug.assert(self.depth == other.depth);
            const old_root = self.root;
            self.root = try self.joinNode(self.root, other.root, self.depth, context, joinFn);
            return self.root != old_root;
        }

        fn putNode(
            self: *Self,
            maybe_node: ?*const anyopaque,
            depth: usize,
            index: u32,
            value: T,
        ) Allocator.Error!?*const anyopaque {
            if (depth == 0) {
                var values = [_]T{empty} ** radix;
                if (maybe_node) |node| {
                    const leaf: *const Leaf = @ptrCast(@alignCast(node));
                    values = leaf.values;
                }
                values[index & radix_mask] = value;
                var all_empty = true;
                for (values) |entry| {
                    if (!std.meta.eql(entry, empty)) {
                        all_empty = false;
                        break;
                    }
                }
                if (all_empty) return null;
                const leaf = try self.allocator.create(Leaf);
                leaf.* = .{ .values = values };
                return leaf;
            }

            var children = [_]?*const anyopaque{null} ** radix;
            if (maybe_node) |node| {
                const branch: *const Branch = @ptrCast(@alignCast(node));
                children = branch.children;
            }
            const shift: u5 = @intCast(leaf_bits + (depth - 1) * radix_bits);
            const slot: usize = @intCast((index >> shift) & radix_mask);
            children[slot] = try self.putNode(children[slot], depth - 1, index, value);
            for (children) |child| {
                if (child != null) {
                    const branch = try self.allocator.create(Branch);
                    branch.* = .{ .children = children };
                    return branch;
                }
            }
            return null;
        }

        fn putNodeUnique(
            self: *Self,
            maybe_node: ?*const anyopaque,
            depth: usize,
            index: u32,
            value: T,
        ) Allocator.Error!?*const anyopaque {
            if (depth == 0) {
                const leaf = if (maybe_node) |node|
                    @as(*Leaf, @ptrCast(@alignCast(@constCast(node))))
                else blk: {
                    const fresh = try self.allocator.create(Leaf);
                    fresh.* = .{ .values = [_]T{empty} ** radix };
                    break :blk fresh;
                };
                leaf.values[index & radix_mask] = value;
                for (leaf.values) |entry| {
                    if (!std.meta.eql(entry, empty)) return leaf;
                }
                return null;
            }

            const branch = if (maybe_node) |node|
                @as(*Branch, @ptrCast(@alignCast(@constCast(node))))
            else blk: {
                const fresh = try self.allocator.create(Branch);
                fresh.* = .{ .children = [_]?*const anyopaque{null} ** radix };
                break :blk fresh;
            };
            const shift: u5 = @intCast(leaf_bits + (depth - 1) * radix_bits);
            const slot: usize = @intCast((index >> shift) & radix_mask);
            branch.children[slot] = try self.putNodeUnique(branch.children[slot], depth - 1, index, value);
            for (branch.children) |child| {
                if (child != null) return branch;
            }
            return null;
        }

        fn meetNode(
            self: *Self,
            lhs: ?*const anyopaque,
            rhs: ?*const anyopaque,
            depth: usize,
            context: anytype,
            comptime meetFn: fn (@TypeOf(context), T, T) T,
        ) Allocator.Error!?*const anyopaque {
            if (lhs == rhs) return lhs;
            if (lhs == null or rhs == null) return null;

            if (depth == 0) {
                const lhs_leaf: *const Leaf = @ptrCast(@alignCast(lhs.?));
                const rhs_leaf: *const Leaf = @ptrCast(@alignCast(rhs.?));
                var values: [radix]T = undefined;
                var same_as_lhs = true;
                var all_empty = true;
                for (&values, lhs_leaf.values, rhs_leaf.values) |*result, left, right| {
                    result.* = meetFn(context, left, right);
                    if (!std.meta.eql(result.*, left)) same_as_lhs = false;
                    if (!std.meta.eql(result.*, empty)) all_empty = false;
                }
                if (same_as_lhs) return lhs;
                if (all_empty) return null;
                const leaf = try self.allocator.create(Leaf);
                leaf.* = .{ .values = values };
                return leaf;
            }

            const lhs_branch: *const Branch = @ptrCast(@alignCast(lhs.?));
            const rhs_branch: *const Branch = @ptrCast(@alignCast(rhs.?));
            var children: [radix]?*const anyopaque = undefined;
            var same_as_lhs = true;
            var all_empty = true;
            for (&children, lhs_branch.children, rhs_branch.children) |*result, left, right| {
                result.* = try self.meetNode(left, right, depth - 1, context, meetFn);
                if (result.* != left) same_as_lhs = false;
                if (result.* != null) all_empty = false;
            }
            if (same_as_lhs) return lhs;
            if (all_empty) return null;
            const branch = try self.allocator.create(Branch);
            branch.* = .{ .children = children };
            return branch;
        }

        fn joinNode(
            self: *Self,
            lhs: ?*const anyopaque,
            rhs: ?*const anyopaque,
            depth: usize,
            context: anytype,
            comptime joinFn: fn (@TypeOf(context), T, T) T,
        ) Allocator.Error!?*const anyopaque {
            if (lhs == rhs or rhs == null) return lhs;
            if (lhs == null) return rhs;

            if (depth == 0) {
                const lhs_leaf: *const Leaf = @ptrCast(@alignCast(lhs.?));
                const rhs_leaf: *const Leaf = @ptrCast(@alignCast(rhs.?));
                var values: [radix]T = undefined;
                var same_as_lhs = true;
                for (&values, lhs_leaf.values, rhs_leaf.values) |*result, left, right| {
                    result.* = joinFn(context, left, right);
                    if (!std.meta.eql(result.*, left)) same_as_lhs = false;
                }
                if (same_as_lhs) return lhs;
                const leaf = try self.allocator.create(Leaf);
                leaf.* = .{ .values = values };
                return leaf;
            }

            const lhs_branch: *const Branch = @ptrCast(@alignCast(lhs.?));
            const rhs_branch: *const Branch = @ptrCast(@alignCast(rhs.?));
            var children: [radix]?*const anyopaque = undefined;
            var same_as_lhs = true;
            for (&children, lhs_branch.children, rhs_branch.children) |*result, left, right| {
                result.* = try self.joinNode(left, right, depth - 1, context, joinFn);
                if (result.* != left) same_as_lhs = false;
            }
            if (same_as_lhs) return lhs;
            const branch = try self.allocator.create(Branch);
            branch.* = .{ .children = children };
            return branch;
        }

        fn eqlNode(lhs: ?*const anyopaque, rhs: ?*const anyopaque, depth: usize) bool {
            if (lhs == rhs) return true;
            if (lhs == null or rhs == null) return false;
            if (depth == 0) {
                const lhs_leaf: *const Leaf = @ptrCast(@alignCast(lhs.?));
                const rhs_leaf: *const Leaf = @ptrCast(@alignCast(rhs.?));
                for (lhs_leaf.values, rhs_leaf.values) |left, right| {
                    if (!std.meta.eql(left, right)) return false;
                }
                return true;
            }
            const lhs_branch: *const Branch = @ptrCast(@alignCast(lhs.?));
            const rhs_branch: *const Branch = @ptrCast(@alignCast(rhs.?));
            for (lhs_branch.children, rhs_branch.children) |left, right| {
                if (!eqlNode(left, right, depth - 1)) return false;
            }
            return true;
        }

        fn depthFor(index: u32) u8 {
            var depth: u8 = 0;
            var covered_bits: u8 = leaf_bits;
            while (covered_bits < 32 and (index >> @intCast(covered_bits)) != 0) {
                depth += 1;
                covered_bits += radix_bits;
            }
            return depth;
        }
    };
}

test "persistent sparse snapshots share forks and meet exactly" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const Sparse = Snapshot(u32, 0);
    var left = Sparse.init(arena.allocator(), 100_000);
    try left.putUnique(4, 7);
    try left.putUnique(70_000, 9);

    var right = left.clone();
    try right.put(4, 3);
    try right.put(90_000, 11);

    try std.testing.expectEqual(@as(u32, 7), left.get(4));
    try std.testing.expectEqual(@as(u32, 0), left.get(90_000));
    try std.testing.expectEqual(@as(u32, 3), right.get(4));

    const meet = struct {
        fn run(_: void, lhs: u32, rhs: u32) u32 {
            if (lhs == 0 or rhs == 0) return 0;
            return @min(lhs, rhs);
        }
    }.run;
    try std.testing.expect(try left.meetWith(&right, {}, meet));
    try std.testing.expectEqual(@as(u32, 3), left.get(4));
    try std.testing.expectEqual(@as(u32, 9), left.get(70_000));
    try std.testing.expectEqual(@as(u32, 0), left.get(90_000));
    try std.testing.expect(left.eql(&left.clone()));
}

test "sparse range queries are exact across tree boundaries and shared updates" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const Sparse = Snapshot(u32, 0);
    var original = Sparse.init(arena.allocator(), 1);
    const keys = [_]u32{ 0, 7, 8, 63, 64, 511, 512, 99999, std.math.maxInt(u32) };
    for (keys) |key| try original.putUnique(key, 1);
    var changed = original.clone();
    for (keys, 0..) |key, i| {
        if (i % 2 == 0) try changed.put(key, 0);
    }
    const boundaries = [_]u64{ 0, 1, 7, 8, 9, 63, 64, 65, 511, 512, 513, 99999, 100000, std.math.maxInt(u32), @as(u64, 1) << 32 };
    for (boundaries[0 .. boundaries.len - 1]) |start| {
        for (boundaries) |end| {
            if (end < start) continue;
            var original_expected = false;
            var changed_expected = false;
            for (keys, 0..) |key, i| {
                if (start <= key and key < end) {
                    original_expected = true;
                    if (i % 2 != 0) changed_expected = true;
                }
            }
            try std.testing.expectEqual(original_expected, original.hasNonEmptyInRange(@intCast(start), end));
            try std.testing.expectEqual(changed_expected, changed.hasNonEmptyInRange(@intCast(start), end));
        }
    }
    // A nearly full u32 range containing no entries must not scan its width.
    const before = range_query_node_visits;
    try std.testing.expect(!original.hasNonEmptyInRange(100000, std.math.maxInt(u32)));
    if (@import("builtin").mode == .Debug) {
        try std.testing.expect(range_query_node_visits - before <= 2 * 8 * 11);
    }
    for (keys) |key| try changed.put(key, 0);
    try std.testing.expect(!changed.hasNonEmptyInRange(0, @as(u64, 1) << 32));
    try std.testing.expect(original.hasNonEmptyInRange(0, @as(u64, 1) << 32));
}

test "sparse iteration preserves forks and skips absent history" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const Sparse = Snapshot(i32, 0);
    var original = Sparse.init(arena.allocator(), 0);
    var empty_iter = original.iterator();
    try std.testing.expectEqual(null, empty_iter.next());

    const keys = [_]u32{ 0, 7, 8, 63, 64, 511, 512, 99999, std.math.maxInt(u32) };
    for (keys, 0..) |key, i| try original.putUnique(key, @intCast(i + 1));
    var changed = original.clone();
    var original_iter = original.iterator();
    for (keys) |key| try changed.put(key, 0);
    try changed.put(99999, -1);
    for (keys, 0..) |key, i| {
        const entry = original_iter.next().?;
        try std.testing.expectEqual(key, entry.index);
        try std.testing.expectEqual(@as(i32, @intCast(i + 1)), entry.value);
    }
    try std.testing.expectEqual(null, original_iter.next());

    const before = iterator_node_visits;
    var changed_iter = changed.iterator();
    try std.testing.expectEqual(Sparse.Iterator.Entry{ .index = 99999, .value = -1 }, changed_iter.next().?);
    try std.testing.expectEqual(null, changed_iter.next());
    if (@import("builtin").mode == .Debug) try std.testing.expect(iterator_node_visits - before <= 11);
}

test "sparse structural difference skips shared subtrees and preserves exact values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const Sparse = Snapshot(u64, 0);
    var owned = Sparse.init(arena.allocator(), 512);
    for (0..512) |index| try owned.putUnique(@intCast(index), 15);
    var keep = owned.clone();
    try keep.put(3, 5);
    try keep.put(6, 0);
    const Difference = struct {
        const Entry = struct { index: u32, lhs: u64, rhs: u64 };
        entries: std.ArrayList(Entry) = .empty,
        calls: usize = 0,

        fn emit(self: *@This(), index: u32, lhs: u64, rhs: u64) std.mem.Allocator.Error!void {
            self.calls += 1;
            if (lhs != rhs) try self.entries.append(std.testing.allocator, .{ .index = index, .lhs = lhs, .rhs = rhs });
        }
    };
    var difference: Difference = .{};
    defer difference.entries.deinit(std.testing.allocator);
    const before = difference_node_visits;
    try owned.differenceWith(&keep, &difference, Difference.emit);
    try std.testing.expectEqualSlices(Difference.Entry, &.{
        .{ .index = 3, .lhs = 15, .rhs = 5 },
        .{ .index = 6, .lhs = 15, .rhs = 0 },
    }, difference.entries.items);
    // Only the changed leaf and its two ancestors are visited. All other
    // leaves share pointers, so even their callbacks are skipped.
    try std.testing.expectEqual(@as(usize, 8), difference.calls);
    if (@import("builtin").mode == .Debug) try std.testing.expectEqual(@as(u64, 3), difference_node_visits - before);

    difference.entries.clearRetainingCapacity();
    difference.calls = 0;
    try owned.differenceWith(&owned, &difference, Difference.emit);
    try std.testing.expectEqual(@as(usize, 0), difference.calls);
    var absent = Sparse.init(arena.allocator(), 512);
    try absent.differenceWith(&owned, &difference, Difference.emit);
    try std.testing.expectEqual(@as(usize, 0), difference.calls);
    try keep.differenceWith(&absent, &difference, Difference.emit);
    try std.testing.expectEqual(@as(usize, 511), difference.entries.items.len);
    var iter = keep.iterator();
    for (difference.entries.items) |entry| {
        const expected = iter.next().?;
        try std.testing.expectEqual(expected.index, entry.index);
        try std.testing.expectEqual(expected.value, entry.lhs);
        try std.testing.expectEqual(@as(u64, 0), entry.rhs);
    }
    try std.testing.expectEqual(null, iter.next());
}

test "sparse reverse iteration carries values across radix boundaries" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const Sparse = Snapshot(u32, 0);
    var state = Sparse.init(arena.allocator(), 1);
    const keys = [_]u32{ 0, 7, 8, 63, 64, 511, 512, 99999, std.math.maxInt(u32) };
    for (keys, 0..) |key, index| try state.putUnique(key, @intCast(index + 1));
    var iter = state.iteratorDirection(.reverse);
    var remaining = keys.len;
    while (remaining != 0) {
        remaining -= 1;
        const entry = iter.next().?;
        try std.testing.expectEqual(keys[remaining], entry.index);
        try std.testing.expectEqual(@as(u32, @intCast(remaining + 1)), entry.value);
    }
    try std.testing.expectEqual(null, iter.next());
    state.clear();
    var empty_iter = state.iteratorDirection(.reverse);
    try std.testing.expectEqual(null, empty_iter.next());
}
