//! Complete inhabitedness query answers for one mutation-free analysis.
//!
//! Recursive traversal assumptions are deliberately not memo entries: only a
//! finished root query is reusable independently of its traversal context.

const std = @import("std");
const types = @import("types");
const Var = types.Var;

pub const Memo = struct {
    answers: std.HashMapUnmanaged(Key, bool, Context, std.hash_map.default_max_load_percentage) = .empty,

    pub const Key = struct {
        root: Var,
        /// Sorted, unique resolved roots. Owned by the memo for stored keys.
        known_empty: []const Var,
    };

    const Context = struct {
        pub fn hash(_: Context, key: Key) u64 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hasher, key.root);
            for (key.known_empty) |root| std.hash.autoHash(&hasher, root);
            return hasher.final();
        }

        pub fn eql(_: Context, a: Key, b: Key) bool {
            return a.root == b.root and std.mem.eql(Var, a.known_empty, b.known_empty);
        }
    };

    pub fn deinit(self: *Memo, gpa: std.mem.Allocator) void {
        var keys = self.answers.keyIterator();
        while (keys.next()) |key| gpa.free(key.known_empty);
        self.answers.deinit(gpa);
    }

    pub fn get(self: *const Memo, key: Key) ?bool {
        return self.answers.get(key);
    }

    /// Call only with a complete root-query answer, never a cycle assumption.
    pub fn put(self: *Memo, gpa: std.mem.Allocator, key: Key, answer: bool) std.mem.Allocator.Error!void {
        const owned = try gpa.dupe(Var, key.known_empty);
        errdefer gpa.free(owned);
        try self.answers.putNoClobber(gpa, .{ .root = key.root, .known_empty = owned }, answer);
    }

    /// Canonicalize query assumptions without changing the solver graph.
    pub fn assumptions(
        gpa: std.mem.Allocator,
        store: anytype,
        vars: []const Var,
    ) std.mem.Allocator.Error!std.ArrayList(Var) {
        var roots: std.ArrayList(Var) = .empty;
        errdefer roots.deinit(gpa);
        try roots.ensureTotalCapacity(gpa, vars.len);
        for (vars) |var_| roots.appendAssumeCapacity(if (@hasDecl(@TypeOf(store.*), "root"))
            store.root(var_)
        else
            store.resolveVar(var_).var_);
        std.mem.sort(Var, roots.items, {}, lessThan);
        var count: usize = 0;
        for (roots.items) |root| {
            if (count == 0 or roots.items[count - 1] != root) {
                roots.items[count] = root;
                count += 1;
            }
        }
        roots.shrinkRetainingCapacity(count);
        return roots;
    }

    fn lessThan(_: void, a: Var, b: Var) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }
};

test "inhabitedness memo keeps known-empty query identities separate" {
    const gpa = std.testing.allocator;
    var memo: Memo = .{};
    defer memo.deinit(gpa);
    const root: Var = @enumFromInt(1);
    const empty = [_]Var{@enumFromInt(2)};
    const ordinary: Memo.Key = .{ .root = root, .known_empty = &.{} };
    const restricted: Memo.Key = .{ .root = root, .known_empty = &empty };
    try memo.put(gpa, ordinary, true);
    try std.testing.expectEqual(@as(?bool, null), memo.get(restricted));
    try memo.put(gpa, restricted, false);
    try std.testing.expectEqual(@as(?bool, true), memo.get(ordinary));
    try std.testing.expectEqual(@as(?bool, false), memo.get(restricted));
    try std.testing.expectEqual(@as(?bool, null), memo.get(.{ .root = @enumFromInt(3), .known_empty = &empty }));
}

test "inhabitedness memo canonicalizes resolved assumption sets" {
    const gpa = std.testing.allocator;
    var store = try types.Store.initCapacity(gpa, 4, 0);
    defer store.deinit();
    const a = try store.fresh();
    const b = try store.fresh();
    const redirect = try store.freshRedirect(a);
    var first = try Memo.assumptions(gpa, &store, &.{ b, redirect, a, b });
    defer first.deinit(gpa);
    var second = try Memo.assumptions(gpa, &store, &.{ a, b });
    defer second.deinit(gpa);
    try std.testing.expectEqualSlices(Var, first.items, second.items);
    try std.testing.expectEqual(@as(usize, 2), first.items.len);
}

fn allocationFailureCase(gpa: std.mem.Allocator) !void {
    var memo: Memo = .{};
    defer memo.deinit(gpa);
    var empty = [_]Var{@enumFromInt(2)};
    const root: Var = @enumFromInt(1);
    try memo.put(gpa, .{ .root = root, .known_empty = &empty }, false);
    // Inserting a key must own its assumptions, not borrow caller scratch.
    empty[0] = @enumFromInt(3);
    const original = [_]Var{@enumFromInt(2)};
    try std.testing.expectEqual(@as(?bool, false), memo.get(.{ .root = root, .known_empty = &original }));
}

test "inhabitedness memo owns keys and handles allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationFailureCase, .{});
}
