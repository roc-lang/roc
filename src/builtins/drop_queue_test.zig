//! Tests for queued teardown (`utils.DropQueue`): dropping a value of a
//! recursive type uses native stack bounded by the type, never by the value.
//!
//! Each test builds a value in a loop, as a Roc program would, and drops it
//! on a thread whose stack is far too small to hold one frame per level of
//! the value. The element and payload callbacks stand in for the compiled
//! `decref_queued` helpers: each passes the queue it is given to the queued
//! teardowns it reaches.

const std = @import("std");

const utils = @import("utils.zig");
const str = @import("str.zig");
const list = @import("list.zig");
const dev_wrappers = @import("dev_wrappers.zig");
const in_process_host = @import("in_process_host.zig");

const RocOps = utils.RocOps;
const RocStr = str.RocStr;
const RocList = list.RocList;
const DropQueue = utils.DropQueue;

/// Deep enough that one native frame per level overflows `small_stack` many
/// times over.
const depth: usize = 100_000;

const small_stack: usize = 256 * 1024;

/// Run `drop` on a thread with a small stack, inside `env`'s host.
fn runOnSmallStack(env: *utils.TestEnv, comptime drop: fn (*RocOps) void) std.Thread.SpawnError!void {
    const Worker = struct {
        fn run(test_env: *utils.TestEnv) void {
            const saved_host = in_process_host.enter(test_env.getOps(), null);
            defer in_process_host.leave(saved_host);
            drop(test_env.getOps());
        }
    };
    const thread = try std.Thread.spawn(.{ .stack_size = small_stack }, Worker.run, .{env});
    thread.join();
}

/// A list holding `items`, each an owned value of type `T`.
fn listOf(comptime T: type, items: []const T, ops: *RocOps) RocList {
    return RocList.fromSlice(T, items, true, ops);
}

/// A box holding `payload`.
fn boxOf(comptime T: type, payload: T, ops: *RocOps) [*]u8 {
    const data = utils.allocateWithRefcount(@sizeOf(T), @alignOf(T), true, ops);
    utils.alignedPtrCast(*T, data, @src()).* = payload;
    return data;
}

/// `Nest := List(Nest)`: the shape of `V := [Leaf, Node(List(V))]`.
const Nest = struct {
    var root: RocList = undefined;

    fn dropElement(element: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        dropList(utils.alignedPtrCast(*const RocList, element.?, @src()), queue, .decref_atomic);
    }

    fn dropList(value: *const RocList, queue: ?*DropQueue, mode: utils.DropMode) void {
        dev_wrappers.roc_builtins_list_drop_queued(value, @alignOf(RocList), @sizeOf(RocList), &dropElement, queue, mode);
    }

    fn build(levels: usize, ops: *RocOps) RocList {
        var value = RocList.empty();
        for (0..levels) |_| value = listOf(RocList, &.{value}, ops);
        return value;
    }
};

test "dropping a deeply nested list uses stack bounded by its type" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    Nest.root = Nest.build(depth, env.getOps());
    try std.testing.expectEqual(depth, env.getAllocationCount());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Nest.dropList(&Nest.root, null, .decref_atomic);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "freeing a deeply nested list the caller owns uses stack bounded by its type" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    Nest.root = Nest.build(depth, env.getOps());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Nest.dropList(&Nest.root, null, .free);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "a queued teardown stops at an allocation another reference keeps alive" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const ops = env.getOps();
    const saved_host = in_process_host.enter(ops, null);
    defer in_process_host.leave(saved_host);

    const kept = Nest.build(3, ops);
    // The second reference, held outside the value being dropped.
    kept.incref(1, true, ops);
    var value = kept;
    for (0..4) |_| value = listOf(RocList, &.{value}, ops);
    try std.testing.expectEqual(@as(usize, 7), env.getAllocationCount());

    Nest.dropList(&value, null, .decref_single_thread);
    try std.testing.expectEqual(@as(usize, 3), env.getAllocationCount());
    Nest.dropList(&kept, null, .decref_single_thread);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

/// `Rose := List({ label : Str, children : Rose })`: wide and deep, with a
/// refcounted field no cycle passes through.
const Rose = struct {
    const Node = extern struct { label: RocStr, children: RocList };

    var root: RocList = undefined;

    fn dropElement(element: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        const node = utils.alignedPtrCast(*const Node, element.?, @src());
        // The label is on no cycle, so it is dropped where it is met.
        node.label.decref(in_process_host.ops());
        dropList(&node.children, queue);
    }

    fn dropList(value: *const RocList, queue: ?*DropQueue) void {
        dev_wrappers.roc_builtins_list_drop_queued(value, @alignOf(Node), @sizeOf(Node), &dropElement, queue, .decref_atomic);
    }

    fn label(ops: *RocOps) RocStr {
        return RocStr.fromSlice("a label long enough to live on the heap", ops);
    }

    /// A spine `levels` deep where every node also has two leaf siblings.
    fn build(levels: usize, ops: *RocOps) RocList {
        var value = RocList.empty();
        for (0..levels) |_| {
            value = listOf(Node, &.{
                .{ .label = label(ops), .children = RocList.empty() },
                .{ .label = label(ops), .children = value },
                .{ .label = label(ops), .children = RocList.empty() },
            }, ops);
        }
        return value;
    }
};

test "dropping a wide and deep list releases every item of every level" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const levels = depth / 4;
    Rose.root = Rose.build(levels, env.getOps());
    // One list and three labels per level.
    try std.testing.expectEqual(levels * 4, env.getAllocationCount());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Rose.dropList(&Rose.root, null);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

/// `Chain := Box({ next : Chain })`, with a null pointer for the end: the
/// shape of `Chain := [End, Link(U64, Chain)]`.
const Chain = struct {
    const Link = extern struct { number: u64, next: ?[*]u8 };

    var root: ?[*]u8 = undefined;

    fn dropPayload(payload: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        dropBox(utils.alignedPtrCast(*const Link, payload.?, @src()).next, queue);
    }

    fn dropBox(value: ?[*]u8, queue: ?*DropQueue) void {
        dev_wrappers.roc_builtins_box_drop_queued(value, @alignOf(Link), &dropPayload, queue, .decref_atomic);
    }

    fn build(levels: usize, ops: *RocOps) ?[*]u8 {
        var value: ?[*]u8 = null;
        for (0..levels) |index| value = boxOf(Link, .{ .number = index, .next = value }, ops);
        return value;
    }
};

test "dropping a long chain of boxes uses stack bounded by its type" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    Chain.root = Chain.build(depth, env.getOps());
    try std.testing.expectEqual(depth, env.getAllocationCount());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Chain.dropBox(Chain.root, null);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

/// `Tree := Box({ left : Tree, right : Tree })`, with null for a leaf: the
/// shape of `Tree := [Leaf, Node(Tree, Tree)]`.
const Tree = struct {
    const Node = extern struct { left: ?[*]u8, right: ?[*]u8 };

    var root: ?[*]u8 = undefined;

    fn dropPayload(payload: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        const node = utils.alignedPtrCast(*const Node, payload.?, @src());
        dropBox(node.left, queue);
        dropBox(node.right, queue);
    }

    fn dropBox(value: ?[*]u8, queue: ?*DropQueue) void {
        dev_wrappers.roc_builtins_box_drop_queued(value, @alignOf(Node), &dropPayload, queue, .decref_atomic);
    }

    /// A spine that turns left and right by turns, with a small subtree
    /// hanging off every node.
    fn build(levels: usize, ops: *RocOps) ?[*]u8 {
        var value: ?[*]u8 = null;
        for (0..levels) |index| {
            const twig = boxOf(Node, .{ .left = null, .right = null }, ops);
            value = boxOf(Node, if (index % 2 == 0) .{ .left = value, .right = twig } else .{ .left = twig, .right = value }, ops);
        }
        return value;
    }
};

test "dropping a deep tree of boxes releases both branches of every node" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    Tree.root = Tree.build(depth, env.getOps());
    try std.testing.expectEqual(depth * 2, env.getAllocationCount());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Tree.dropBox(Tree.root, null);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

/// `Mixed := List(Box(Mixed))`: two teardowns on one cycle, so each nests
/// inside the other once and finds the other's queue from then on.
const Mixed = struct {
    var root: RocList = undefined;

    fn dropListElement(element: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        const boxed = utils.alignedPtrCast(*const ?[*]u8, element.?, @src()).*;
        dev_wrappers.roc_builtins_box_drop_queued(boxed, @alignOf(RocList), &dropBoxPayload, queue, .decref_atomic);
    }

    fn dropBoxPayload(payload: ?[*]u8, queue: ?*DropQueue) callconv(.c) void {
        dropList(utils.alignedPtrCast(*const RocList, payload.?, @src()), queue);
    }

    fn dropList(value: *const RocList, queue: ?*DropQueue) void {
        dev_wrappers.roc_builtins_list_drop_queued(value, @alignOf(?[*]u8), @sizeOf(?[*]u8), &dropListElement, queue, .decref_atomic);
    }

    fn build(levels: usize, ops: *RocOps) RocList {
        var value = RocList.empty();
        for (0..levels) |_| {
            const first: ?[*]u8 = boxOf(RocList, value, ops);
            const second: ?[*]u8 = boxOf(RocList, RocList.empty(), ops);
            value = listOf(?[*]u8, &.{ first, second }, ops);
        }
        return value;
    }
};

test "teardowns of two kinds on one cycle share their queues" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    Mixed.root = Mixed.build(depth, env.getOps());
    // One list and two boxes per level.
    try std.testing.expectEqual(depth * 3, env.getAllocationCount());

    try runOnSmallStack(&env, struct {
        fn drop(_: *RocOps) void {
            Mixed.dropList(&Mixed.root, null);
        }
    }.drop);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}
