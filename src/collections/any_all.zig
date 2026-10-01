//! Any/all evaluation over nested groups of leaves on explicit stacks.
//!
//! Many compiler questions (whether a type is proven uninhabited, whether
//! two types are equivalent, whether an expression diverges) combine their
//! answers for a node's components with "any" or "all". Evaluating them here
//! keeps the depth of the structure being asked about from ever becoming
//! native call depth.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Which children decide a group: `any` is true at its first true child and
/// false when none is; `all` is false at its first false child and true when
/// none is.
pub const Op = enum { any, all };

/// Evaluates nested any/all groups over leaves that `Context` expands, on
/// explicit stacks so the leaves' nesting never becomes native call depth.
///
/// `Context.enter(context, items, leaf)` either decides `leaf` at once or
/// returns the op of a group after listing its items: `items.add(leaf)` for a
/// leaf and `items.group(op, count)` for a nested group of the next `count`
/// leaves. `Context.exit(context, leaf, result)` runs after a leaf that
/// returned a group completes; when the evaluation fails it runs with a null
/// result instead, and then must not fail itself.
pub fn Evaluation(comptime Leaf: type, comptime Context: type) type {
    return struct {
        pub const Expansion = union(enum) {
            value: bool,
            group: Op,
        };

        const Item = union(enum) {
            leaf: Leaf,
            /// A nested group of the `count` leaves after it.
            group: struct { op: Op, count: usize },
        };

        pub const Items = struct {
            allocator: Allocator,
            list: *std.ArrayList(Item),

            /// Leaves are added one at a time from inner loops, so the
            /// common case of spare capacity stays inline.
            pub inline fn add(self: Items, leaf: Leaf) Allocator.Error!void {
                if (self.list.items.len == self.list.capacity) try self.list.ensureUnusedCapacity(self.allocator, 1);
                self.list.appendAssumeCapacity(.{ .leaf = leaf });
            }

            pub inline fn group(self: Items, op: Op, count: usize) Allocator.Error!void {
                if (self.list.items.len == self.list.capacity) try self.list.ensureUnusedCapacity(self.allocator, 1);
                self.list.appendAssumeCapacity(.{ .group = .{ .op = op, .count = count } });
            }
        };

        const Frame = struct {
            op: Op,
            /// The leaf whose expansion this group is; null for a nested
            /// group, whose items belong to the enclosing frame.
            owner: ?Leaf,
            items_start: usize,
            items_end: usize,
            next: usize,
        };

        /// The stacks one evaluation runs on. An owner that evaluates
        /// repeatedly keeps one and hands it to every run, so the runs reuse
        /// its allocated capacity; a run leaves it as it found it, so nested
        /// runs may share it.
        pub const Scratch = struct {
            frames: std.ArrayList(Frame) = .empty,
            items: std.ArrayList(Item) = .empty,

            pub fn deinit(self: *Scratch, allocator: Allocator) void {
                self.frames.deinit(allocator);
                self.items.deinit(allocator);
            }
        };

        pub fn run(allocator: Allocator, context: *Context, root: Leaf) Allocator.Error!bool {
            var scratch: Scratch = .{};
            defer scratch.deinit(allocator);
            return runWith(allocator, &scratch, context, root);
        }

        pub fn runWith(allocator: Allocator, scratch: *Scratch, context: *Context, root: Leaf) Allocator.Error!bool {
            const frames = &scratch.frames;
            const items = &scratch.items;
            const frames_base = frames.items.len;
            const items_base = items.items.len;
            defer {
                frames.shrinkRetainingCapacity(frames_base);
                items.shrinkRetainingCapacity(items_base);
            }
            errdefer {
                var index = frames.items.len;
                while (index > frames_base) {
                    index -= 1;
                    if (frames.items[index].owner) |owner| context.exit(owner, null) catch unreachable;
                }
            }

            var pending: ?bool = try begin(allocator, context, frames, items, root);
            while (true) {
                if (pending) |value| {
                    if (frames.items.len == frames_base) return value;
                    const top = frames.items[frames.items.len - 1];
                    const decided = switch (top.op) {
                        .any => value,
                        .all => !value,
                    };
                    pending = if (decided) try finish(context, frames, items, value) else null;
                    continue;
                }
                const frame = &frames.items[frames.items.len - 1];
                if (frame.next == frame.items_end) {
                    pending = try finish(context, frames, items, frame.op == .all);
                    continue;
                }
                const item = items.items[frame.next];
                frame.next += 1;
                switch (item) {
                    .leaf => |leaf| pending = try begin(allocator, context, frames, items, leaf),
                    .group => |nested| {
                        const start = frame.next;
                        frame.next += nested.count;
                        try frames.append(allocator, .{
                            .op = nested.op,
                            .owner = null,
                            .items_start = start,
                            .items_end = start + nested.count,
                            .next = start,
                        });
                    },
                }
            }
        }

        /// The value `leaf` decides at once, or null after pushing its group.
        fn begin(
            allocator: Allocator,
            context: *Context,
            frames: *std.ArrayList(Frame),
            items: *std.ArrayList(Item),
            leaf: Leaf,
        ) Allocator.Error!?bool {
            const start = items.items.len;
            const expansion = context.enter(Items{ .allocator = allocator, .list = items }, leaf) catch |err| {
                items.shrinkRetainingCapacity(start);
                return err;
            };
            switch (expansion) {
                .value => |value| {
                    items.shrinkRetainingCapacity(start);
                    return value;
                },
                .group => |op| {
                    frames.append(allocator, .{
                        .op = op,
                        .owner = leaf,
                        .items_start = start,
                        .items_end = items.items.len,
                        .next = start,
                    }) catch |err| {
                        items.shrinkRetainingCapacity(start);
                        context.exit(leaf, null) catch unreachable;
                        return err;
                    };
                    return null;
                },
            }
        }

        fn finish(context: *Context, frames: *std.ArrayList(Frame), items: *std.ArrayList(Item), value: bool) Allocator.Error!bool {
            const frame = frames.pop().?;
            if (frame.owner) |owner| {
                items.shrinkRetainingCapacity(frame.items_start);
                try context.exit(owner, value);
            }
            return value;
        }
    };
}

const TestTree = struct {
    /// Node `i`'s expansion: a value, or a group over its children.
    nodes: []const union(enum) { value: bool, any: []const u8, all: []const u8 },
    exits: u32 = 0,

    const Eval = Evaluation(u8, TestTree);

    fn enter(self: *TestTree, items: Eval.Items, leaf: u8) Allocator.Error!Eval.Expansion {
        return switch (self.nodes[leaf]) {
            .value => |value| .{ .value = value },
            .any => |children| blk: {
                for (children) |child| try items.add(child);
                break :blk .{ .group = .any };
            },
            .all => |children| blk: {
                for (children) |child| try items.add(child);
                break :blk .{ .group = .all };
            },
        };
    }

    fn exit(self: *TestTree, _: u8, _: ?bool) std.mem.Allocator.Error!void {
        self.exits += 1;
    }
};

test "any/all evaluation short-circuits groups and exits every expanded leaf" {
    var tree = TestTree{ .nodes = &.{
        .{ .all = &.{ 1, 2 } },
        .{ .any = &.{ 3, 4 } },
        .{ .value = true },
        .{ .value = false },
        .{ .value = true },
    } };
    try std.testing.expect(try TestTree.Eval.run(std.testing.allocator, &tree, 0));
    try std.testing.expectEqual(@as(u32, 2), tree.exits);

    var short = TestTree{ .nodes = &.{
        .{ .all = &.{ 1, 2 } },
        .{ .value = false },
        .{ .any = &.{} },
    } };
    try std.testing.expect(!try TestTree.Eval.run(std.testing.allocator, &short, 0));
    try std.testing.expectEqual(@as(u32, 1), short.exits);
}

test "any/all evaluation handles nesting deeper than a thread stack allows recursion" {
    const depth = 200_000;
    const allocator = std.testing.allocator;
    const Chain = struct {
        const Eval = Evaluation(u32, @This());

        fn enter(_: *@This(), items: Eval.Items, leaf: u32) Allocator.Error!Eval.Expansion {
            if (leaf == depth) return .{ .value = true };
            try items.group(.all, 1);
            try items.add(leaf + 1);
            return .{ .group = .any };
        }

        fn exit(_: *@This(), _: u32, _: ?bool) std.mem.Allocator.Error!void {}
    };
    var chain = Chain{};
    try std.testing.expect(try Chain.Eval.run(allocator, &chain, 0));
}
