//! Persistent sparse snapshots for exact procedure-local ARC facts.

const std = @import("std");

const Allocator = std.mem.Allocator;

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
