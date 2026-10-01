//! Shared cycle discipline for checked-type graph traversals.
//!
//! Every traversal here walks on explicit heap-backed stacks: a context
//! never recurses into the traversal, it lists a node's children, and the
//! traversal visits them. Type nesting therefore never becomes native call
//! depth.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Policy for pending checked-type payloads reached by identity-variable scans.
pub const PendingPolicy = enum {
    /// Treat a pending payload as identity-containing so callers cannot
    /// accidentally publish a completed non-identity result from an unfinished root.
    forbid,
    /// Delegate pending payloads to the caller. Used by traversals that can
    /// legitimately see their own reserved roots while building a recursive result.
    tolerate,
};

/// The children a context lists for the node it is visiting, in visit order.
pub fn Children(comptime Key: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        list: *std.ArrayList(Key),

        pub fn add(self: Self, key: Key) Allocator.Error!void {
            try self.list.append(self.allocator, key);
        }

        pub fn addSlice(self: Self, keys: []const Key) Allocator.Error!void {
            try self.list.appendSlice(self.allocator, keys);
        }
    };
}

/// Memoized boolean traversal with active-cycle hits returning false.
///
/// `Context.visit(context, children, key)` returns a node's own result, or
/// null after listing its children with `children.add`; the node's result is
/// then whether any child's is true, visiting children in order and stopping
/// at the first true one.
pub fn BoolPredicateTraversal(comptime Key: type, comptime Context: type) type {
    return struct {
        const Self = @This();

        const State = union(enum) {
            active,
            complete: bool,
        };

        const Frame = struct {
            key: Key,
            /// Where this node's children start in `children`.
            children_start: usize,
            next: usize,
        };

        allocator: Allocator,
        context: *Context,
        memo: std.AutoHashMap(Key, State),
        frames: std.ArrayList(Frame) = .empty,
        children: std.ArrayList(Key) = .empty,

        pub fn init(allocator: Allocator, context: *Context) Self {
            return .{
                .allocator = allocator,
                .context = context,
                .memo = std.AutoHashMap(Key, State).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.children.deinit(self.allocator);
            self.frames.deinit(self.allocator);
            self.memo.deinit();
        }

        pub fn resetRetainingCapacity(self: *Self) void {
            self.memo.clearRetainingCapacity();
        }

        pub fn visit(self: *Self, root: Key) Allocator.Error!bool {
            errdefer {
                for (self.frames.items) |frame| _ = self.memo.remove(frame.key);
                self.frames.clearRetainingCapacity();
                self.children.clearRetainingCapacity();
            }
            if (try self.begin(root)) |value| return value;
            while (true) {
                const frame = &self.frames.items[self.frames.items.len - 1];
                if (frame.next < self.children.items.len) {
                    const child = self.children.items[frame.next];
                    frame.next += 1;
                    const child_result = (try self.begin(child)) orelse continue;
                    if (!child_result) continue;
                    if (self.finish(true)) |done| return done;
                } else if (self.finish(false)) |done| return done;
            }
        }

        /// The memoized or immediate result of `key`, or null after pushing
        /// its frame.
        fn begin(self: *Self, key: Key) Allocator.Error!?bool {
            const entry = try self.memo.getOrPut(key);
            if (entry.found_existing) {
                return switch (entry.value_ptr.*) {
                    .active => false,
                    .complete => |value| value,
                };
            }
            entry.value_ptr.* = .active;
            const children_start = self.children.items.len;
            const own = self.context.visit(Children(Key){ .allocator = self.allocator, .list = &self.children }, key) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.memo.remove(key);
                return err;
            };
            if (own) |value| {
                self.children.shrinkRetainingCapacity(children_start);
                self.memo.getPtr(key).?.* = .{ .complete = value };
                return value;
            }
            self.frames.append(self.allocator, .{ .key = key, .children_start = children_start, .next = children_start }) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.memo.remove(key);
                return err;
            };
            return null;
        }

        /// Complete the innermost frame with `value`, then every enclosing
        /// frame a true value decides; the root's result once the last frame
        /// completes.
        fn finish(self: *Self, value: bool) ?bool {
            while (true) {
                const frame = self.frames.pop().?;
                self.children.shrinkRetainingCapacity(frame.children_start);
                self.memo.getPtr(frame.key).?.* = .{ .complete = value };
                if (self.frames.items.len == 0) return value;
                if (!value) return null;
            }
        }
    };
}

/// Reserve-then-fill traversal for building recursive graph results.
///
/// `Context.reserve(context, key)` reserves a node's result before its
/// children are visited, `Context.fill(context, children, key, reserved)`
/// lists its children, and `Context.filled(context, key, reserved,
/// child_results)` completes it from its children's results. A child already
/// in progress yields its reserved result.
pub fn ReserveThenFillTraversal(comptime Key: type, comptime Result: type, comptime Context: type) type {
    return struct {
        const Self = @This();

        const Frame = struct {
            key: Key,
            reserved: Result,
            children_start: usize,
            results_start: usize,
        };

        allocator: Allocator,
        context: *Context,
        active: std.AutoHashMap(Key, Result),
        frames: std.ArrayList(Frame) = .empty,
        children: std.ArrayList(Key) = .empty,
        results: std.ArrayList(Result) = .empty,

        pub fn init(allocator: Allocator, context: *Context) Self {
            return .{
                .allocator = allocator,
                .context = context,
                .active = std.AutoHashMap(Key, Result).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.results.deinit(self.allocator);
            self.children.deinit(self.allocator);
            self.frames.deinit(self.allocator);
            self.active.deinit();
        }

        pub fn resetRetainingCapacity(self: *Self) void {
            self.active.clearRetainingCapacity();
        }

        /// Return whether `result` is currently the reserved value for some
        /// in-progress key. Used by pending-tolerant scans that must recognize a
        /// root they are themselves mid-way through building.
        pub fn hasReservedResult(self: *const Self, result: Result) bool {
            var it = self.active.valueIterator();
            while (it.next()) |value| {
                if (std.meta.eql(value.*, result)) return true;
            }
            return false;
        }

        pub fn visit(self: *Self, root: Key) Allocator.Error!Result {
            errdefer {
                for (self.frames.items) |frame| _ = self.active.remove(frame.key);
                self.frames.clearRetainingCapacity();
                self.children.clearRetainingCapacity();
                self.results.clearRetainingCapacity();
            }
            if (self.active.get(root)) |reserved| return reserved;
            try self.begin(root);
            while (true) {
                const frame = &self.frames.items[self.frames.items.len - 1];
                const visited = self.results.items.len - frame.results_start;
                if (frame.children_start + visited < self.children.items.len) {
                    const child = self.children.items[frame.children_start + visited];
                    if (self.active.get(child)) |reserved| {
                        try self.results.append(self.allocator, reserved);
                    } else {
                        try self.begin(child);
                    }
                    continue;
                }
                const finished = frame.*;
                try self.context.filled(finished.key, finished.reserved, self.results.items[finished.results_start..]);
                _ = self.frames.pop();
                _ = self.active.remove(finished.key);
                self.children.shrinkRetainingCapacity(finished.children_start);
                self.results.shrinkRetainingCapacity(finished.results_start);
                if (self.frames.items.len == 0) return finished.reserved;
                try self.results.append(self.allocator, finished.reserved);
            }
        }

        fn begin(self: *Self, key: Key) Allocator.Error!void {
            const reserved = try self.context.reserve(key);
            try self.active.put(key, reserved);
            const children_start = self.children.items.len;
            self.context.fill(Children(Key){ .allocator = self.allocator, .list = &self.children }, key, reserved) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.active.remove(key);
                return err;
            };
            self.frames.append(self.allocator, .{
                .key = key,
                .reserved = reserved,
                .children_start = children_start,
                .results_start = self.results.items.len,
            }) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.active.remove(key);
                return err;
            };
        }
    };
}

/// Active-path traversal for digest builders that encode back edges by depth.
///
/// `Context.visit(context, children, key)` records a node and lists its
/// children; a child already on the active path is reported through
/// `Context.backEdge(context, depth)` instead of being visited.
pub fn DigestTraversal(comptime Key: type, comptime Context: type) type {
    return struct {
        const Self = @This();

        const Frame = struct {
            key: Key,
            children_start: usize,
            next: usize,
        };

        allocator: Allocator,
        context: *Context,
        active: std.AutoHashMap(Key, u32),
        frames: std.ArrayList(Frame) = .empty,
        children: std.ArrayList(Key) = .empty,

        pub fn init(allocator: Allocator, context: *Context) Self {
            return .{
                .allocator = allocator,
                .context = context,
                .active = std.AutoHashMap(Key, u32).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.children.deinit(self.allocator);
            self.frames.deinit(self.allocator);
            self.active.deinit();
        }

        pub fn resetRetainingCapacity(self: *Self) void {
            self.active.clearRetainingCapacity();
        }

        pub fn activeCount(self: *const Self) u32 {
            return @intCast(self.active.count());
        }

        pub fn visit(self: *Self, root: Key) Allocator.Error!void {
            defer {
                for (self.frames.items) |frame| _ = self.active.remove(frame.key);
                self.frames.clearRetainingCapacity();
                self.children.clearRetainingCapacity();
            }
            if (self.active.get(root)) |depth| {
                self.context.backEdge(depth);
                return;
            }
            try self.begin(root);
            while (self.frames.items.len != 0) {
                const frame = &self.frames.items[self.frames.items.len - 1];
                if (frame.next < self.children.items.len) {
                    const child = self.children.items[frame.next];
                    frame.next += 1;
                    if (self.active.get(child)) |depth| {
                        self.context.backEdge(depth);
                    } else {
                        try self.begin(child);
                    }
                    continue;
                }
                const finished = self.frames.pop().?;
                _ = self.active.remove(finished.key);
                self.children.shrinkRetainingCapacity(finished.children_start);
            }
        }

        fn begin(self: *Self, key: Key) Allocator.Error!void {
            try self.active.put(key, self.context.activeDepth());
            const children_start = self.children.items.len;
            self.context.visit(Children(Key){ .allocator = self.allocator, .list = &self.children }, key) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.active.remove(key);
                return err;
            };
            self.frames.append(self.allocator, .{ .key = key, .children_start = children_start, .next = children_start }) catch |err| {
                self.children.shrinkRetainingCapacity(children_start);
                _ = self.active.remove(key);
                return err;
            };
        }
    };
}

/// Return whether a checked-type root contains any identity variables.
pub fn checkedTypeContainsIdentityVariables(
    comptime Key: type,
    comptime Context: type,
    allocator: Allocator,
    context: *Context,
    root: Key,
) Allocator.Error!bool {
    var traversal = BoolPredicateTraversal(Key, Context).init(allocator, context);
    defer traversal.deinit();
    return try traversal.visit(root);
}

/// Return whether any checked-type root in a slice contains identity variables.
pub fn checkedTypeSliceContainsIdentityVariables(
    comptime Key: type,
    comptime Context: type,
    allocator: Allocator,
    context: *Context,
    roots: []const Key,
) Allocator.Error!bool {
    var traversal = BoolPredicateTraversal(Key, Context).init(allocator, context);
    defer traversal.deinit();
    for (roots) |root| {
        if (try traversal.visit(root)) return true;
    }
    return false;
}

/// A checked-type payload's own identity-variable result, or null after
/// listing the child roots whose results decide it.
pub fn checkedTypePayloadIdentityVariableChildren(
    comptime pending_policy: PendingPolicy,
    children: anytype,
    pool_owner: anytype,
    root: anytype,
    payload: anytype,
    context: anytype,
) Allocator.Error!?bool {
    switch (payload) {
        .pending => return switch (pending_policy) {
            .forbid => true,
            .tolerate => context.pendingContainsIdentityVariables(root),
        },
        .err => return false,
        .flex,
        .rigid,
        => return true,
        .empty_record,
        .empty_tag_union,
        => return false,
        .alias => |alias| {
            try children.add(alias.backing);
            try children.addSlice(alias.args);
        },
        .record => |record| {
            for (record.fields) |field| try children.add(field.ty);
            try children.add(record.ext);
        },
        .tuple => |items| try children.addSlice(items),
        .nominal => |nominal| try children.addSlice(nominal.args),
        .function => |function| {
            try children.addSlice(function.args);
            try children.add(function.ret);
        },
        .tag_union => |tag_union| {
            for (tag_union.tags) |tag| try children.addSlice(tag.argsSlice(pool_owner));
            try children.add(tag_union.ext);
        },
    }
    return null;
}

const TestEdge = struct {
    key: u8,
    result: bool = false,
    children: []const u8 = &.{},
};

const PredicateTestContext = struct {
    edges: []const TestEdge,
    visits: *[8]u8,

    fn visit(self: *@This(), children: anytype, key: u8) Allocator.Error!?bool {
        self.visits[key] += 1;
        const entry = self.findEdge(key);
        if (entry.result) return true;
        try children.addSlice(entry.children);
        return null;
    }

    fn findEdge(self: *const @This(), key: u8) TestEdge {
        for (self.edges) |entry| {
            if (entry.key == key) return entry;
        }
        unreachable;
    }
};

test "BoolPredicateTraversal memoizes shared DAG nodes" {
    const children_1 = [_]u8{ 2, 3 };
    const children_2 = [_]u8{4};
    const children_3 = [_]u8{4};
    const edges = [_]TestEdge{
        .{ .key = 1, .children = &children_1 },
        .{ .key = 2, .children = &children_2 },
        .{ .key = 3, .children = &children_3 },
        .{ .key = 4 },
    };
    var visits = [_]u8{0} ** 8;
    var context = PredicateTestContext{ .edges = &edges, .visits = &visits };
    var traversal = BoolPredicateTraversal(u8, PredicateTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(!try traversal.visit(1));
    try std.testing.expectEqual(@as(u8, 1), visits[4]);
}

test "BoolPredicateTraversal active cycle hit returns false" {
    const children_1 = [_]u8{2};
    const children_2 = [_]u8{1};
    const edges = [_]TestEdge{
        .{ .key = 1, .children = &children_1 },
        .{ .key = 2, .children = &children_2 },
    };
    var visits = [_]u8{0} ** 8;
    var context = PredicateTestContext{ .edges = &edges, .visits = &visits };
    var traversal = BoolPredicateTraversal(u8, PredicateTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(!try traversal.visit(1));
    try std.testing.expectEqual(@as(u8, 1), visits[1]);
    try std.testing.expectEqual(@as(u8, 1), visits[2]);
}

test "BoolPredicateTraversal still finds true branch beside a cycle" {
    const children_1 = [_]u8{ 2, 3 };
    const children_2 = [_]u8{1};
    const edges = [_]TestEdge{
        .{ .key = 1, .children = &children_1 },
        .{ .key = 2, .children = &children_2 },
        .{ .key = 3, .result = true },
    };
    var visits = [_]u8{0} ** 8;
    var context = PredicateTestContext{ .edges = &edges, .visits = &visits };
    var traversal = BoolPredicateTraversal(u8, PredicateTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(try traversal.visit(1));
    try std.testing.expectEqual(@as(u8, 1), visits[3]);
}

const RehashPredicateTestContext = struct {
    visits: *[256]u8,

    fn visit(self: *@This(), children: anytype, key: u8) Allocator.Error!?bool {
        self.visits[key] += 1;
        if (key != 1) return false;
        var child: u8 = 2;
        while (child < 200) : (child += 1) try children.add(child);
        return null;
    }
};

test "BoolPredicateTraversal completes root after recursive inserts rehash memo" {
    var visits = [_]u8{0} ** 256;
    var context = RehashPredicateTestContext{ .visits = &visits };
    var traversal = BoolPredicateTraversal(u8, RehashPredicateTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(!try traversal.visit(1));
    try std.testing.expect(!try traversal.visit(1));
    try std.testing.expectEqual(@as(u8, 1), visits[1]);
    try std.testing.expectEqual(@as(u8, 1), visits[199]);
}

const ReserveTestContext = struct {
    edges: []const TestEdge,
    next: u8 = 10,
    back_edge_result: u8 = 0,

    fn reserve(self: *@This(), _: u8) Allocator.Error!u8 {
        const out = self.next;
        self.next += 1;
        return out;
    }

    fn fill(self: *@This(), children: anytype, key: u8, _: u8) Allocator.Error!void {
        try children.addSlice(self.findEdge(key).children);
    }

    fn filled(self: *@This(), key: u8, _: u8, child_results: []const u8) Allocator.Error!void {
        const entry = self.findEdge(key);
        for (entry.children, child_results) |child, child_result| {
            if (key == 2 and child == 1) {
                self.back_edge_result = child_result;
            }
        }
    }

    fn findEdge(self: *const @This(), key: u8) TestEdge {
        for (self.edges) |entry| {
            if (entry.key == key) return entry;
        }
        unreachable;
    }
};

test "ReserveThenFillTraversal returns reserved result on back edge" {
    const children_1 = [_]u8{2};
    const children_2 = [_]u8{1};
    const edges = [_]TestEdge{
        .{ .key = 1, .children = &children_1 },
        .{ .key = 2, .children = &children_2 },
    };
    var context = ReserveTestContext{ .edges = &edges };
    var traversal = ReserveThenFillTraversal(u8, u8, ReserveTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    const root = try traversal.visit(1);
    try std.testing.expectEqual(@as(u8, 10), root);
    try std.testing.expectEqual(root, context.back_edge_result);
}

const DigestTestContext = struct {
    edges: []const TestEdge,
    bytes: std.ArrayList(u8),
    traversal: ?*DigestTraversal(u8, @This()) = null,

    fn deinit(self: *@This(), allocator: Allocator) void {
        self.bytes.deinit(allocator);
    }

    fn activeDepth(self: *@This()) u32 {
        return self.traversal.?.activeCount();
    }

    fn visit(self: *@This(), children: anytype, key: u8) Allocator.Error!void {
        try self.bytes.append(std.testing.allocator, key);
        try children.addSlice(self.findEdge(key).children);
    }

    fn backEdge(self: *@This(), depth: u32) void {
        self.bytes.append(std.testing.allocator, @intCast(depth)) catch unreachable;
    }

    fn findEdge(self: *const @This(), key: u8) TestEdge {
        for (self.edges) |entry| {
            if (entry.key == key) return entry;
        }
        unreachable;
    }
};

test "DigestTraversal emits active depth for back edge" {
    const children_1 = [_]u8{2};
    const children_2 = [_]u8{1};
    const edges = [_]TestEdge{
        .{ .key = 1, .children = &children_1 },
        .{ .key = 2, .children = &children_2 },
    };
    var context = DigestTestContext{ .edges = &edges, .bytes = .empty };
    defer context.deinit(std.testing.allocator);
    var traversal = DigestTraversal(u8, DigestTestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();
    context.traversal = &traversal;

    try traversal.visit(1);
    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 0 }, context.bytes.items);
}

/// Composite merge-input key mirroring the platform-relation resolver's
/// `PlatformAppRelationMergeInput`: a walk over two roots plus a context tag.
const TestMergeInput = struct {
    platform: u8,
    app: u8,
    context: u8 = 0,
};

const CompositeEdge = struct {
    key: TestMergeInput,
    result: bool = false,
    children: []const TestMergeInput = &.{},
};

const CompositePredicateContext = struct {
    edges: []const CompositeEdge,
    visits: *u32,

    fn visit(self: *@This(), children: anytype, key: TestMergeInput) Allocator.Error!?bool {
        self.visits.* += 1;
        const entry = self.findEdge(key);
        if (entry.result) return true;
        try children.addSlice(entry.children);
        return null;
    }

    fn findEdge(self: *const @This(), key: TestMergeInput) CompositeEdge {
        for (self.edges) |entry| {
            if (std.meta.eql(entry.key, key)) return entry;
        }
        unreachable;
    }
};

test "BoolPredicateTraversal memoizes composite merge-input keys through a cycle" {
    const a = TestMergeInput{ .platform = 1, .app = 10 };
    const b = TestMergeInput{ .platform = 2, .app = 20 };
    const children_a = [_]TestMergeInput{b};
    const children_b = [_]TestMergeInput{a};
    const edges = [_]CompositeEdge{
        .{ .key = a, .children = &children_a },
        .{ .key = b, .children = &children_b },
    };
    var visits: u32 = 0;
    var context = CompositePredicateContext{ .edges = &edges, .visits = &visits };
    var traversal = BoolPredicateTraversal(TestMergeInput, CompositePredicateContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(!try traversal.visit(a));
    try std.testing.expectEqual(@as(u32, 2), visits);
}

test "BoolPredicateTraversal finds a true branch beside a composite-key cycle" {
    const a = TestMergeInput{ .platform = 1, .app = 10 };
    const b = TestMergeInput{ .platform = 2, .app = 20 };
    const c = TestMergeInput{ .platform = 3, .app = 30, .context = 1 };
    const children_a = [_]TestMergeInput{ b, c };
    const children_b = [_]TestMergeInput{a};
    const edges = [_]CompositeEdge{
        .{ .key = a, .children = &children_a },
        .{ .key = b, .children = &children_b },
        .{ .key = c, .result = true },
    };
    var visits: u32 = 0;
    var context = CompositePredicateContext{ .edges = &edges, .visits = &visits };
    var traversal = BoolPredicateTraversal(TestMergeInput, CompositePredicateContext).init(std.testing.allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(try traversal.visit(a));
}

/// Composite finalize-input key mirroring `PlatformAppRelationFinalizeInput`.
const TestFinalizeInput = struct {
    root: u8,
    context: u8 = 0,
};

const CompositeDigestEdge = struct {
    key: TestFinalizeInput,
    children: []const TestFinalizeInput = &.{},
};

const CompositeDigestContext = struct {
    edges: []const CompositeDigestEdge,
    bytes: std.ArrayList(u8),
    traversal: ?*DigestTraversal(TestFinalizeInput, @This()) = null,

    fn deinit(self: *@This(), allocator: Allocator) void {
        self.bytes.deinit(allocator);
    }

    fn activeDepth(self: *@This()) u32 {
        return self.traversal.?.activeCount();
    }

    fn visit(self: *@This(), children: anytype, key: TestFinalizeInput) Allocator.Error!void {
        try self.bytes.append(std.testing.allocator, key.root);
        try children.addSlice(self.findEdge(key).children);
    }

    fn backEdge(self: *@This(), depth: u32) void {
        self.bytes.append(std.testing.allocator, @as(u8, @intCast(depth)) | 0x80) catch unreachable;
    }

    fn findEdge(self: *const @This(), key: TestFinalizeInput) CompositeDigestEdge {
        for (self.edges) |entry| {
            if (std.meta.eql(entry.key, key)) return entry;
        }
        unreachable;
    }
};

test "DigestTraversal emits active depth for a composite finalize-input back edge" {
    const a = TestFinalizeInput{ .root = 1 };
    const b = TestFinalizeInput{ .root = 2 };
    const children_a = [_]TestFinalizeInput{b};
    const children_b = [_]TestFinalizeInput{a};
    const edges = [_]CompositeDigestEdge{
        .{ .key = a, .children = &children_a },
        .{ .key = b, .children = &children_b },
    };
    var context = CompositeDigestContext{ .edges = &edges, .bytes = .empty };
    defer context.deinit(std.testing.allocator);
    var traversal = DigestTraversal(TestFinalizeInput, CompositeDigestContext).init(std.testing.allocator, &context);
    defer traversal.deinit();
    context.traversal = &traversal;

    try traversal.visit(a);
    // node 1, node 2, then a back edge to the active root at depth 0.
    try std.testing.expectEqualSlices(u8, &.{ 1, 2, 0x80 }, context.bytes.items);
}

const StressNode = struct {
    children: []const u32,
    is_identity: bool = false,
};

const StressContext = struct {
    nodes: []const StressNode,
    steps: *u32,
    budget: u32,
    exceeded: *bool,

    fn visit(self: *@This(), children: anytype, key: u32) Allocator.Error!?bool {
        if (self.steps.* >= self.budget) {
            self.exceeded.* = true;
            return false;
        }
        self.steps.* += 1;
        const node = self.nodes[key];
        if (node.is_identity) return true;
        try children.addSlice(node.children);
        return null;
    }
};

test "BoolPredicateTraversal stays within a step budget on deep chains and wide mutual recursion" {
    const allocator = std.testing.allocator;

    // Deep alias/backing chain flowing into a wide mutually-recursive
    // tag-union family. A missing pre-descent memo write would livelock on the
    // family's cycles; the step budget bounds and detects that, and the chain
    // is deep enough that a traversal nesting native calls per level would
    // overflow a thread stack.
    const chain_len: u32 = 100_000;
    const family_size: u32 = 48;
    const node_count: u32 = chain_len + family_size;

    var child_lists = std.ArrayList([]u32).empty;
    defer {
        for (child_lists.items) |c| allocator.free(c);
        child_lists.deinit(allocator);
    }
    const nodes = try allocator.alloc(StressNode, node_count);
    defer allocator.free(nodes);

    var i: u32 = 0;
    while (i < chain_len) : (i += 1) {
        const child = try allocator.alloc(u32, 1);
        child[0] = if (i + 1 < chain_len) i + 1 else chain_len;
        try child_lists.append(allocator, child);
        nodes[i] = .{ .children = child };
    }
    var f: u32 = 0;
    while (f < family_size) : (f += 1) {
        const kids = try allocator.alloc(u32, family_size);
        var k: u32 = 0;
        while (k < family_size) : (k += 1) kids[k] = chain_len + k;
        try child_lists.append(allocator, kids);
        nodes[chain_len + f] = .{ .children = kids };
    }

    var steps: u32 = 0;
    var exceeded = false;
    var context = StressContext{
        .nodes = nodes,
        .steps = &steps,
        .budget = node_count + 1,
        .exceeded = &exceeded,
    };
    var traversal = BoolPredicateTraversal(u32, StressContext).init(allocator, &context);
    defer traversal.deinit();

    try std.testing.expect(!try traversal.visit(0));
    try std.testing.expect(!exceeded);
    // Correct memoization visits each reachable node exactly once.
    try std.testing.expectEqual(node_count, steps);
}
