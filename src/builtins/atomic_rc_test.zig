//! Tests for atomic reference counting when several threads release
//! references to one allocation at the same time.
//!
//! Two properties must hold for every atomic release path:
//!
//! - Exactly one releasing thread observes that it took the count to zero,
//!   and that thread tears down the allocation's children before freeing it.
//!   Deciding "unique" from a separate load of the count, ahead of the
//!   decrement, would let two threads holding the last two references both
//!   see "shared", skip teardown, and then free the allocation with its
//!   children still alive. The leak tests below count live host allocations
//!   after every round, so one lost round fails the test.
//!
//! - Every access a thread makes to the allocation before its release happens
//!   before the free performed by whichever thread releases last. This needs
//!   release ordering on the decrement and acquire ordering before teardown.
//!   x86 provides it for any atomic read-modify-write; weaker architectures
//!   such as aarch64 do not. ThreadSanitizer models the orderings exactly, so
//!   `zig build run-test-builtins-tsan`, which compiles this file with
//!   `-fsanitize-thread`, reports a missing ordering on any architecture.
//!
//! The leak tests detect a lost round only when the two threads actually
//! interleave inside the window, so each runs many rounds with both threads
//! released from a spin barrier at the same moment.

const std = @import("std");
const builtin = @import("builtin");

const utils = @import("utils.zig");
const str = @import("str.zig");
const list = @import("list.zig");
const host_abi = @import("host_abi.zig");
const dev_wrappers = @import("dev_wrappers.zig");
const in_process_host = @import("in_process_host.zig");
const erased_callable = @import("erased_callable.zig");

const RocOps = host_abi.RocOps;
const RocStr = str.RocStr;
const RocList = list.RocList;

/// Rounds per leak test. ThreadSanitizer slows every memory access, and it
/// reports the first racing round anyway, so it runs far fewer.
const rounds: usize = if (builtin.sanitize_thread) 2_000 else 200_000;

/// A host whose allocator may be called from several threads at once and
/// which counts the allocations still live.
const ThreadSafeHost = struct {
    live: std.atomic.Value(isize) = .init(0),
    ops: RocOps = undefined,

    fn init(self: *ThreadSafeHost) void {
        self.ops = .{
            .env = @ptrCast(self),
            .roc_alloc = rocAlloc,
            .roc_dealloc = rocDealloc,
            .roc_realloc = rocRealloc,
            .roc_dbg = rocDbg,
            .roc_expect_failed = rocExpectFailed,
            .roc_crashed = rocCrashed,
            .hosted_fns = .{ .count = 0, .fns = undefined },
        };
    }

    fn liveAllocations(self: *const ThreadSafeHost) isize {
        return self.live.load(.seq_cst);
    }

    fn fromOps(ops: *RocOps) *ThreadSafeHost {
        return @ptrCast(@alignCast(ops.env));
    }

    fn rocAlloc(ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
        // `malloc` returns memory aligned for every fundamental type, which
        // covers every alignment these tests request.
        std.debug.assert(alignment <= 16);
        const ptr = std.c.malloc(length) orelse @trap();
        _ = fromOps(ops).live.fetchAdd(1, .seq_cst);
        return ptr;
    }

    fn rocDealloc(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
        std.debug.assert(alignment <= 16);
        _ = fromOps(ops).live.fetchSub(1, .seq_cst);
        std.c.free(ptr);
    }

    fn rocRealloc(_: *RocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) *anyopaque {
        // Releasing references never reallocates.
        @trap();
    }

    fn rocDbg(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {}

    fn rocExpectFailed(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {}

    fn rocCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        std.debug.print("Roc crashed: {s}\n", .{bytes[0..len]});
        @trap();
    }
};

/// Runs `release` on two threads at the same moment, `rounds` times, with a
/// fresh value from `prepare` each round, and returns how many rounds left
/// allocations live afterward.
///
/// `Scenario` provides `prepare(*RocOps) Value`, which builds a value holding
/// two references, and `release(Value)`, which drops one of them.
fn countLeakedRounds(comptime Scenario: type) std.Thread.SpawnError!usize {
    const Value = @typeInfo(@TypeOf(Scenario.prepare)).@"fn".return_type.?;

    const Shared = struct {
        host: *ThreadSafeHost,
        value: Value = undefined,
        /// The round both workers may start; 0 before the first round.
        started: std.atomic.Value(usize) = .init(0),
        /// Releases finished across all rounds.
        finished: std.atomic.Value(usize) = .init(0),

        fn worker(shared: *@This()) void {
            const saved_host = in_process_host.enter(&shared.host.ops, null);
            defer in_process_host.leave(saved_host);

            var round: usize = 1;
            while (round <= rounds) : (round += 1) {
                while (shared.started.load(.acquire) < round) std.atomic.spinLoopHint();
                Scenario.release(shared.value);
                _ = shared.finished.fetchAdd(1, .acq_rel);
            }
        }
    };

    var host: ThreadSafeHost = .{};
    host.init();
    const saved_host = in_process_host.enter(&host.ops, null);
    defer in_process_host.leave(saved_host);

    var shared: Shared = .{ .host = &host };
    const first = try std.Thread.spawn(.{}, Shared.worker, .{&shared});
    const second = try std.Thread.spawn(.{}, Shared.worker, .{&shared});

    var leaked_rounds: usize = 0;
    var round: usize = 1;
    while (round <= rounds) : (round += 1) {
        shared.value = Scenario.prepare(&host.ops);
        shared.started.store(round, .release);
        while (shared.finished.load(.acquire) < 2 * round) std.atomic.spinLoopHint();
        if (host.liveAllocations() != 0) {
            leaked_rounds += 1;
            // Later rounds count only their own leaks.
            host.live.store(0, .seq_cst);
        }
    }

    first.join();
    second.join();
    return leaked_rounds;
}

/// A string long enough to live on the heap.
const heap_text = "a string long enough to need its own heap allocation";

/// Stand-in for a compiled atomic decref helper for a `Str` element or payload.
fn strDecrefAtomic(element: ?[*]u8) callconv(.c) void {
    const str_ptr: *RocStr = utils.alignedPtrCast(*RocStr, element.?, @src());
    str_ptr.decref(in_process_host.ops());
}

const ListOfStrs = struct {
    const element_count = 4;

    fn prepare(ops: *RocOps) RocList {
        var elements: [element_count]RocStr = undefined;
        for (&elements) |*element| element.* = RocStr.fromSlice(heap_text, ops);
        const l = RocList.fromSlice(RocStr, elements[0..], true, ops);
        // The second of the two references the threads release.
        l.incref(1, true, ops);
        return l;
    }

    fn release(l: RocList) void {
        dev_wrappers.roc_builtins_list_decref_with(
            l.bytes,
            l.length,
            l.capacity_or_alloc_ptr,
            @alignOf(RocStr),
            @sizeOf(RocStr),
            &strDecrefAtomic,
        );
    }
};

const BoxOfStr = struct {
    fn prepare(ops: *RocOps) [*]u8 {
        const payload = utils.allocateWithRefcount(@sizeOf(RocStr), @alignOf(RocStr), true, ops);
        utils.alignedPtrCast(*RocStr, payload, @src()).* = RocStr.fromSlice(heap_text, ops);
        // The second of the two references the threads release.
        utils.increfDataPtr(payload, 1, .atomic, ops);
        return payload;
    }

    fn release(payload: [*]u8) void {
        dev_wrappers.roc_builtins_box_decref_with(payload, @alignOf(RocStr), &strDecrefAtomic);
    }
};

/// An erased callable whose capture is one heap string, released by its
/// `on_drop`.
const ErasedCallableCapturingStr = struct {
    fn prepare(ops: *RocOps) [*]u8 {
        const callable = erased_callable.allocate(&call, &dropCapture, @sizeOf(RocStr), ops);
        utils.alignedPtrCast(*RocStr, erased_callable.capturePtr(callable), @src()).* = RocStr.fromSlice(heap_text, ops);
        // The second of the two references the threads release.
        erased_callable.incref(callable, 1, ops);
        return callable;
    }

    fn release(callable: [*]u8) void {
        dev_wrappers.roc_builtins_erased_callable_decref(callable);
    }

    fn call(_: *RocOps, _: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {}

    fn dropCapture(capture: ?[*]u8, ops: *RocOps) callconv(.c) void {
        utils.alignedPtrCast(*RocStr, capture.?, @src()).decref(ops);
    }
};

/// An allocation with no refcounted children: the only teardown is the free,
/// so a run of this scenario exercises ordering alone. Each thread reads the
/// payload before releasing its reference, as any reader of a shared value
/// does.
const ChildlessAllocation = struct {
    const payload_len = 64;

    fn prepare(ops: *RocOps) [*]u8 {
        const payload = utils.allocateWithRefcount(payload_len, @alignOf(usize), false, ops);
        @memset(payload[0..payload_len], 0x5a);
        utils.increfDataPtr(payload, 1, .atomic, ops);
        return payload;
    }

    fn release(payload: [*]u8) void {
        var sum: usize = 0;
        for (payload[0..payload_len]) |byte| sum +%= byte;
        std.mem.doNotOptimizeAway(sum);
        utils.decrefDataPtrC(payload, @alignOf(usize), false, in_process_host.ops());
    }
};

test "atomic list decref tears down the elements when two threads release the last two references" {
    try std.testing.expectEqual(@as(usize, 0), try countLeakedRounds(ListOfStrs));
}

test "atomic box decref tears down the payload when two threads release the last two references" {
    try std.testing.expectEqual(@as(usize, 0), try countLeakedRounds(BoxOfStr));
}

test "atomic erased callable decref runs on_drop when two threads release the last two references" {
    try std.testing.expectEqual(@as(usize, 0), try countLeakedRounds(ErasedCallableCapturingStr));
}

test "atomic decref frees an allocation exactly once when two threads release the last two references" {
    try std.testing.expectEqual(@as(usize, 0), try countLeakedRounds(ChildlessAllocation));
}

test "releasing both references on one thread frees every allocation" {
    var host: ThreadSafeHost = .{};
    host.init();
    const saved_host = in_process_host.enter(&host.ops, null);
    defer in_process_host.leave(saved_host);

    inline for (.{ ListOfStrs, BoxOfStr, ErasedCallableCapturingStr, ChildlessAllocation }) |Scenario| {
        const value = Scenario.prepare(&host.ops);
        Scenario.release(value);
        try std.testing.expect(host.liveAllocations() != 0);
        Scenario.release(value);
        try std.testing.expectEqual(@as(isize, 0), host.liveAllocations());
    }
}
