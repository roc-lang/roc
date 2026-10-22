//! Complete inhabitedness query answers for one mutation-free analysis.
//!
//! Recursive traversal assumptions are deliberately not memo entries: only a
//! finished root query is reusable independently of its traversal context.

const std = @import("std");
const types = @import("types");
const Var = types.Var;

/// Reuse completed root answers only while the analyzed graph is immutable.
pub const Memo = struct {
    answers: std.HashMapUnmanaged(StoredKey, bool, StoredContext, std.hash_map.default_max_load_percentage) = .empty,
    /// Every stored key's assumptions, back to back.
    known_empty_pool: std.ArrayListUnmanaged(Var) = .empty,

    pub const Key = struct {
        root: Var,
        /// Sorted, unique resolved roots.
        known_empty: []const Var,
    };

    /// A stored key, whose assumptions are a span of `known_empty_pool`.
    const StoredKey = struct {
        root: Var,
        start: u32,
        len: u32,
    };

    fn hashKey(root: Var, known_empty: []const Var) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, root);
        for (known_empty) |known| std.hash.autoHash(&hasher, known);
        return hasher.final();
    }

    fn storedKnownEmpty(pool: []const Var, key: StoredKey) []const Var {
        return pool[key.start..][0..key.len];
    }

    const StoredContext = struct {
        pool: []const Var,

        pub fn hash(self: StoredContext, key: StoredKey) u64 {
            return hashKey(key.root, storedKnownEmpty(self.pool, key));
        }

        pub fn eql(_: StoredContext, a: StoredKey, b: StoredKey) bool {
            // Stored keys are distinct, so they are equal exactly when they
            // name the same span.
            return a.root == b.root and a.start == b.start and a.len == b.len;
        }
    };

    /// Looks stored keys up by a query key.
    const QueryContext = struct {
        pool: []const Var,

        pub fn hash(_: QueryContext, key: Key) u64 {
            return hashKey(key.root, key.known_empty);
        }

        pub fn eql(self: QueryContext, query: Key, stored: StoredKey) bool {
            return query.root == stored.root and varsEqual(query.known_empty, storedKnownEmpty(self.pool, stored));
        }
    };

    pub fn deinit(self: *Memo, gpa: std.mem.Allocator) void {
        self.answers.deinit(gpa);
        self.known_empty_pool.deinit(gpa);
    }

    pub fn get(self: *const Memo, key: Key) ?bool {
        return self.answers.getAdapted(key, QueryContext{ .pool = self.known_empty_pool.items });
    }

    /// Call only with a complete root-query answer, never a cycle assumption.
    pub fn put(self: *Memo, gpa: std.mem.Allocator, key: Key, answer: bool) std.mem.Allocator.Error!void {
        std.debug.assert(self.get(key) == null);
        const start = std.math.cast(u32, self.known_empty_pool.items.len) orelse return error.OutOfMemory;
        const len = std.math.cast(u32, key.known_empty.len) orelse return error.OutOfMemory;
        try self.known_empty_pool.appendSlice(gpa, key.known_empty);
        errdefer self.known_empty_pool.shrinkRetainingCapacity(start);
        const stored: StoredKey = .{ .root = key.root, .start = start, .len = len };
        try self.answers.putNoClobberContext(gpa, stored, answer, StoredContext{ .pool = self.known_empty_pool.items });
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
        return @backingInt(a) < @backingInt(b);
    }
};

test "inhabitedness memo keeps known-empty query identities separate" {
    const gpa = std.testing.allocator;
    var memo: Memo = .{};
    defer memo.deinit(gpa);
    const root: Var = @fromBackingInt(1);
    const empty = [_]Var{@fromBackingInt(2)};
    const ordinary: Memo.Key = .{ .root = root, .known_empty = &.{} };
    const restricted: Memo.Key = .{ .root = root, .known_empty = &empty };
    try memo.put(gpa, ordinary, true);
    try std.testing.expectEqual(@as(?bool, null), memo.get(restricted));
    try memo.put(gpa, restricted, false);
    try std.testing.expectEqual(@as(?bool, true), memo.get(ordinary));
    try std.testing.expectEqual(@as(?bool, false), memo.get(restricted));
    try std.testing.expectEqual(@as(?bool, null), memo.get(.{ .root = @fromBackingInt(3), .known_empty = &empty }));
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

fn allocationFailureCase(gpa: std.mem.Allocator) (std.mem.Allocator.Error || error{TestExpectedEqual})!void {
    var memo: Memo = .{};
    defer memo.deinit(gpa);
    var empty = [_]Var{@fromBackingInt(2)};
    const root: Var = @fromBackingInt(1);
    try memo.put(gpa, .{ .root = root, .known_empty = &empty }, false);
    // Inserting a key must own its assumptions, not borrow caller scratch.
    empty[0] = @fromBackingInt(3);
    const original = [_]Var{@fromBackingInt(2)};
    try std.testing.expectEqual(@as(?bool, false), memo.get(.{ .root = root, .known_empty = &original }));
}

test "inhabitedness memo owns keys and handles allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationFailureCase, .{});
}

/// Whether two type-variable lists name the same variables in the same order.
fn varsEqual(a: []const Var, b: []const Var) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (left != right) return false;
    }
    return true;
}
