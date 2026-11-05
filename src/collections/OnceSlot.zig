//! Write-once slots for facts derived on first demand from immutable data.
//!
//! A slot is one state word plus the result it guards. The word moves
//! from `empty` to `claimed` (one compare-and-swap by the consumer that
//! computes the result), to `contended` when another consumer parks on it,
//! and to `done` when the claimant publishes. A consumer that finds the slot
//! claimed parks on the word with a futex until it is done. A consumer that
//! finds it done reads the result after an acquire load of the word; that is
//! the whole read path, and it takes no lock.
//!
//! A claimant whose computation fails abandons the slot: the word returns to
//! `empty`, parked consumers wake, and the next one claims it.
//!
//! A thread never waits on a slot of a kind it holds a claim of. Claims of one
//! kind are taken in no fixed order, so two threads each holding one claim and
//! waiting on the other's slot would never wake. Analyses that are fixpoints
//! over several roots claim one slot for the whole group instead of nesting
//! claims. Debug builds assert the rule; release builds treat a violation as
//! unreachable.
//!
//! A thread holding a lock another thread may need (a type store's creation
//! lock, a name interner's writer lock) acquires no slot: the slot may be
//! claimed by a thread that needs that lock. Each such lock records itself
//! in `HeldLocks` while held, and debug builds assert that a thread acquiring
//! a slot holds none.

const std = @import("std");

const Io = std.Io;

/// The slot's state word.
pub const State = enum(u32) {
    empty,
    claimed,
    /// Claimed, with at least one consumer parked on the word.
    contended,
    done,
};

/// Per-thread count of claims held on slots of `kind`.
fn ClaimLedger(comptime kind: @EnumLiteral()) type {
    return struct {
        threadlocal var held: u32 = 0;

        fn assertHoldsNone() void {
            if (held != 0) std.debug.panic("a thread waited on a ." ++ @tagName(kind) ++ " slot while holding {d} claim(s) of that kind", .{held});
        }
    };
}

/// Per-thread count of held locks that another thread may need while it
/// holds a slot's claim.
pub const HeldLocks = struct {
    threadlocal var held: u32 = 0;

    /// Record that this thread took such a lock.
    pub fn acquired() void {
        if (std.debug.runtime_safety) held += 1;
    }

    /// Record that this thread released such a lock.
    pub fn released() void {
        if (std.debug.runtime_safety) {
            std.debug.assert(held > 0);
            held -= 1;
        }
    }

    fn assertNone() void {
        if (held != 0) std.debug.panic("a thread acquired a slot while holding {d} lock(s) another thread may need", .{held});
    }
};

/// A write-once slot holding a `T`. Slots sharing a `kind` share the rule
/// that a thread holding a claim of that kind never waits on another.
pub fn OnceSlot(comptime T: type, comptime kind: @EnumLiteral()) type {
    return struct {
        const Self = @This();
        const Ledger = ClaimLedger(kind);

        state: std.atomic.Value(State) = .init(.empty),
        result: T = undefined,

        pub const empty: Self = .{};

        /// What `acquire` found.
        pub const Acquired = union(enum) {
            /// The caller holds the claim and must `publish` or `abandon`.
            claimed,
            done: T,
        };

        /// The result, or null when no consumer has published one.
        pub fn peek(self: *const Self) ?T {
            if (self.state.load(.acquire) != .done) return null;
            return self.result;
        }

        /// The published result, or the claim on the slot when it is empty.
        /// Waits while another thread holds the claim.
        pub fn acquire(self: *Self, io: Io) Acquired {
            if (std.debug.runtime_safety) HeldLocks.assertNone();
            var observed = self.state.load(.acquire);
            while (true) switch (observed) {
                .done => return .{ .done = self.result },
                .empty => {
                    observed = self.state.cmpxchgWeak(.empty, .claimed, .acquire, .acquire) orelse {
                        if (std.debug.runtime_safety) Ledger.held += 1;
                        return .claimed;
                    };
                },
                .claimed => {
                    if (std.debug.runtime_safety) Ledger.assertHoldsNone();
                    observed = self.state.cmpxchgWeak(.claimed, .contended, .acquire, .acquire) orelse .contended;
                },
                .contended => {
                    if (std.debug.runtime_safety) Ledger.assertHoldsNone();
                    Io.futexWaitUncancelable(io, State, &self.state.raw, .contended);
                    observed = self.state.load(.acquire);
                },
            };
        }

        /// Store the claimed slot's result and wake every parked consumer.
        pub fn publish(self: *Self, io: Io, result: T) void {
            self.result = result;
            self.release(io, .done);
        }

        /// Return the claimed slot to empty so another consumer claims it.
        pub fn abandon(self: *Self, io: Io) void {
            self.release(io, .empty);
        }

        fn release(self: *Self, io: Io, next: State) void {
            if (std.debug.runtime_safety) {
                std.debug.assert(Ledger.held > 0);
                Ledger.held -= 1;
            }
            const previous = self.state.swap(next, .release);
            std.debug.assert(previous == .claimed or previous == .contended);
            if (previous == .contended) Io.futexWake(io, State, &self.state.raw, std.math.maxInt(u32));
        }

        /// The published result, computing it with `compute(context)` when
        /// the slot is empty. A failed computation abandons the slot and
        /// returns its error.
        pub fn getOrCompute(
            self: *Self,
            io: Io,
            comptime E: type,
            context: anytype,
            comptime compute: fn (@TypeOf(context)) E!T,
        ) E!T {
            switch (self.acquire(io)) {
                .done => |result| return result,
                .claimed => {
                    const result = compute(context) catch |err| {
                        self.abandon(io);
                        return err;
                    };
                    self.publish(io, result);
                    return result;
                },
            }
        }
    };
}

test "an empty slot is claimed once and read after publication" {
    const io = std.testing.io;
    var slot: OnceSlot(u64, .once_slot_test_single) = .empty;
    try std.testing.expectEqual(@as(?u64, null), slot.peek());
    try std.testing.expect(slot.acquire(io) == .claimed);
    try std.testing.expectEqual(@as(?u64, null), slot.peek());
    slot.publish(io, 42);
    try std.testing.expectEqual(@as(?u64, 42), slot.peek());
    switch (slot.acquire(io)) {
        .claimed => return error.TestUnexpectedResult,
        .done => |result| try std.testing.expectEqual(@as(u64, 42), result),
    }
}

test "an abandoned slot can be claimed again" {
    const io = std.testing.io;
    var slot: OnceSlot(u32, .once_slot_test_abandon) = .empty;
    const Compute = struct {
        fn fail(_: void) error{Failed}!u32 {
            return error.Failed;
        }
        fn succeed(_: void) error{Failed}!u32 {
            return 7;
        }
    };
    try std.testing.expectError(error.Failed, slot.getOrCompute(io, error{Failed}, {}, Compute.fail));
    try std.testing.expectEqual(@as(?u32, null), slot.peek());
    try std.testing.expectEqual(@as(u32, 7), try slot.getOrCompute(io, error{Failed}, {}, Compute.succeed));
    try std.testing.expectEqual(@as(u32, 7), try slot.getOrCompute(io, error{Failed}, {}, Compute.fail));
}

test "waiters park until the claimant publishes, and every consumer reads one result" {
    if (@import("builtin").single_threaded) return error.SkipZigTest;
    const io = std.testing.io;
    const Slot = OnceSlot(u64, .once_slot_test_waiters);
    const thread_count = 8;

    const Shared = struct {
        slot: Slot = .empty,
        computations: std.atomic.Value(u32) = .init(0),
        /// Holds the claimant inside its computation until every other
        /// consumer has started waiting.
        release_claimant: std.atomic.Value(u32) = .init(0),
        results: [thread_count]u64 = @splat(0),

        fn compute(self: *@This()) error{}!u64 {
            _ = self.computations.fetchAdd(1, .monotonic);
            while (self.release_claimant.load(.acquire) == 0) {
                Io.futexWaitUncancelable(io, u32, &self.release_claimant.raw, 0);
            }
            return 0xfeed;
        }

        fn run(self: *@This(), index: usize) void {
            self.results[index] = self.slot.getOrCompute(io, error{}, self, compute) catch |err| switch (err) {};
        }
    };

    var shared: Shared = .{};
    var threads: [thread_count]std.Thread = undefined;
    for (&threads, 0..) |*thread, index| {
        thread.* = try std.Thread.spawn(.{}, Shared.run, .{ &shared, index });
    }
    // Let the claimant finish only after the slot reports contention, so at
    // least one consumer has parked on the state word.
    while (shared.slot.state.load(.acquire) != .contended) std.atomic.spinLoopHint();
    shared.release_claimant.store(1, .release);
    Io.futexWake(io, u32, &shared.release_claimant.raw, std.math.maxInt(u32));
    for (threads) |thread| thread.join();

    try std.testing.expectEqual(@as(u32, 1), shared.computations.load(.acquire));
    for (shared.results) |result| try std.testing.expectEqual(@as(u64, 0xfeed), result);
    try std.testing.expectEqual(@as(?u64, 0xfeed), shared.slot.peek());
}

test "held locks are counted per thread" {
    HeldLocks.acquired();
    HeldLocks.acquired();
    if (std.debug.runtime_safety) try std.testing.expectEqual(@as(u32, 2), HeldLocks.held);
    HeldLocks.released();
    HeldLocks.released();
    if (std.debug.runtime_safety) try std.testing.expectEqual(@as(u32, 0), HeldLocks.held);
}

test "claims of one kind are tracked per thread" {
    const io = std.testing.io;
    const Slot = OnceSlot(u8, .once_slot_test_ledger);
    var first: Slot = .empty;
    var second: Slot = .empty;
    try std.testing.expect(first.acquire(io) == .claimed);
    try std.testing.expect(second.acquire(io) == .claimed);
    if (std.debug.runtime_safety) try std.testing.expectEqual(@as(u32, 2), ClaimLedger(.once_slot_test_ledger).held);
    second.publish(io, 2);
    first.abandon(io);
    if (std.debug.runtime_safety) try std.testing.expectEqual(@as(u32, 0), ClaimLedger(.once_slot_test_ledger).held);
}
