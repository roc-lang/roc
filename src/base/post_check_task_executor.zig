//! Type-erased task execution shared by compiler stages without introducing stage dependencies.

const std = @import("std");
const invariant = @import("invariant.zig").invariant;
const SingleThreadArena = @import("collections").SingleThreadArena;

/// Type-erased state retained by one exclusive executor lane.
///
/// Entries are initialized lazily by task callbacks and destroyed with the
/// physical worker which owns this lane. The executor never interprets an
/// entry; keys and cleanup callbacks are owned by the stage using it.
pub const LaneState = struct {
    const Entry = struct {
        key: *const anyopaque,
        value: *anyopaque,
        deinitFn: *const fn (*anyopaque) void,
    };

    allocator: std.mem.Allocator,
    entries: std.ArrayList(Entry) = .empty,

    pub fn init(allocator: std.mem.Allocator) LaneState {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *LaneState) void {
        for (self.entries.items) |entry| entry.deinitFn(entry.value);
        self.entries.deinit(self.allocator);
        self.* = undefined;
    }

    /// Return state registered under `key`, if this lane has initialized it.
    ///
    /// No synchronization is needed: callbacks holding the same lane never
    /// overlap, and coordinator access is restricted to executor barriers.
    pub fn get(self: *LaneState, key: *const anyopaque) ?*anyopaque {
        for (self.entries.items) |entry| {
            if (entry.key == key) return entry.value;
        }
        return null;
    }

    /// Retain one initialized value until the lane itself is destroyed.
    pub fn put(
        self: *LaneState,
        key: *const anyopaque,
        value: *anyopaque,
        deinitFn: *const fn (*anyopaque) void,
    ) std.mem.Allocator.Error!void {
        if (self.get(key) != null) {
            invariant("{s}", .{"post-check executor lane state key was registered more than once"});
        }
        try self.entries.append(self.allocator, .{
            .key = key,
            .value = value,
            .deinitFn = deinitFn,
        });
    }
};

/// Exclusive execution lane and its persistent and task-scoped allocators.
/// A thread has one lane per nesting depth of the sessions it is waiting
/// in, so a task it runs while waiting in a session never shares a lane with
/// the task that waits (`Executor.lane_count`).
pub const Worker = struct {
    id: usize,
    /// Allocator for task-owned output. The caller owns cleanup of allocations
    /// retained in `Completion.value`. Its backing state must outlive retained
    /// owners and permit their use on another lane after a completion barrier.
    /// Owners remain exclusively accessed; the allocator itself is thread-safe.
    allocator: std.mem.Allocator,
    /// Temporary storage which becomes invalid when the task callback returns.
    scratch: std.mem.Allocator,
    /// Type-erased mutable state owned by this physical lane. Values registered
    /// here survive synchronous batch boundaries and are destroyed with the
    /// worker pool, except for an ordered run's items, whose lane state
    /// belongs to the run and is destroyed when it returns (`runOrdered`).
    lane_state: *LaneState,
};

/// Type-erased work item whose context remains caller-owned through completion.
pub const Task = struct {
    id: usize,
    context: *anyopaque,
    run: *const fn (context: *anyopaque, worker: Worker) ?*anyopaque,
};

/// One finished task, tagged with its logical task and physical worker identities.
pub const Completion = struct {
    id: usize,
    worker_id: usize,
    value: ?*anyopaque,
};

/// Bounded executor borrowed from a compilation coordinator.
///
/// A session may be opened from any thread, a task's callback included: an
/// owner waiting for a completion runs its own session's queued tasks itself
/// in the meantime, so a task that opens a session and waits never blocks a
/// thread the session's tasks need, and the same pool serves every nesting
/// level.
pub const Executor = struct {
    context: *anyopaque,
    /// The threads the executor runs tasks on, which bounds how many tasks a
    /// session has out at once.
    worker_count: usize,
    /// The distinct lane ids a `Worker` can carry: every thread has a lane
    /// per nesting depth. A consumer keeping state per lane sizes it by
    /// this.
    lane_count: usize,
    /// Open a session, returning its handle.
    beginFn: *const fn (*anyopaque) std.mem.Allocator.Error!*anyopaque,
    /// Success accepts exactly one task. Failure accepts none: no queued or
    /// running callback and no future completion may belong to that submission.
    submitFn: *const fn (*anyopaque, *anyopaque, Task) std.mem.Allocator.Error!void,
    /// Must synchronize with the completed callback so its writes are visible
    /// before the returned value is consumed.
    receiveFn: *const fn (*anyopaque, *anyopaque) Completion,
    /// A completion already filed with the session, without waiting or
    /// running anything.
    tryReceiveFn: *const fn (*anyopaque, *anyopaque) ?Completion,
    endFn: *const fn (*anyopaque, *anyopaque) void,

    /// Start a streaming session.
    pub fn begin(self: Executor) std.mem.Allocator.Error!Session {
        std.debug.assert(self.worker_count > 0);
        return .{ .executor = self, .handle = try self.beginFn(self.context) };
    }

    /// Run one bounded batch. `completions` must have exactly `tasks.len` entries.
    ///
    /// Threaded executors write completions in completion/arrival order, not task
    /// order. Callers must use `Completion.id` when logical ordering matters.
    /// Inline execution naturally has task order as its completion order.
    ///
    /// A worker id denotes one exclusive persistent execution lane: callbacks
    /// with the same id never overlap, observe tasks in input order, and receive
    /// the same output allocator and `LaneState` for the borrowed executor's
    /// lifetime. Callers may therefore retain lane-local state between
    /// synchronous `run` calls.
    ///
    /// A task's `Worker.scratch` storage dies when its callback returns.
    /// `Completion.value` must therefore point to task/caller-owned storage or
    /// storage allocated by `Worker.allocator`; the caller owns its cleanup.
    /// Values registered in `Worker.lane_state` instead belong to the executor.
    pub fn run(self: Executor, tasks: []const Task, completions: []Completion) std.mem.Allocator.Error!void {
        std.debug.assert(tasks.len == completions.len);
        var session = try self.begin();
        var submitted: usize = 0;
        var received: usize = 0;
        var submit_error: ?std.mem.Allocator.Error = null;

        while (received < submitted or (submit_error == null and submitted < tasks.len)) {
            while (submit_error == null and submitted < tasks.len and session.canSubmit()) {
                session.submit(tasks[submitted]) catch |err| {
                    submit_error = err;
                    break;
                };
                submitted += 1;
            }
            if (received < submitted) {
                completions[received] = session.receive();
                received += 1;
            }
        }
        session.end();
        if (submit_error) |err| return err;
    }
};

/// The callback time of a parallel section's tasks, measured only when a
/// caller asks for timing. Each task runs through a wrapper that reads the
/// clock around its callback and records the duration in the task's own
/// slot, so no two lanes write one counter; the owner sums the slots once
/// every completion is received.
pub const TimedTasks = struct {
    std_io: std.Io,
    entries: []Entry,

    const Entry = struct {
        inner: Task,
        std_io: std.Io,
        ns: u64 = 0,
    };

    /// Slots for `count` measured tasks.
    pub fn init(allocator: std.mem.Allocator, count: usize, std_io: std.Io) std.mem.Allocator.Error!TimedTasks {
        return .{ .std_io = std_io, .entries = try allocator.alloc(Entry, count) };
    }

    /// Release the slots.
    pub fn deinit(self: *TimedTasks, allocator: std.mem.Allocator) void {
        allocator.free(self.entries);
        self.* = undefined;
    }

    /// `task`, measured in slot `slot`. Its id, context, and completion
    /// value are unchanged.
    pub fn wrap(self: *TimedTasks, slot: usize, task: Task) Task {
        self.entries[slot] = .{ .inner = task, .std_io = self.std_io };
        return .{ .id = task.id, .context = &self.entries[slot], .run = runEntry };
    }

    fn runEntry(context: *anyopaque, worker: Worker) ?*anyopaque {
        const entry: *Entry = @ptrCast(@alignCast(context));
        const started = std.Io.Timestamp.now(entry.std_io, .awake).nanoseconds;
        const value = entry.inner.run(entry.inner.context, worker);
        entry.ns = @intCast(@max(0, std.Io.Timestamp.now(entry.std_io, .awake).nanoseconds - started));
        return value;
    }

    /// The summed callback time of the first `count` slots, every one of
    /// whose tasks has completed.
    pub fn totalNs(self: *const TimedTasks, count: usize) u64 {
        var total: u64 = 0;
        for (self.entries[0..count]) |entry| total +|= entry.ns;
        return total;
    }
};

/// The scratch capacity a lane keeps between the items of one ordered run. A
/// lane's item scratch returns to one retained chunk of at most this many
/// bytes after every item, so steady-state items allocate nothing from the
/// backing allocator, and idle capacity is bounded by this times the lanes
/// running the run's items.
pub const lane_scratch_retain_limit: usize = 4 << 20;

/// The scratch arena a lane lends every item it runs, registered in the
/// lane's `LaneState`.
const LaneArena = struct {
    arena: SingleThreadArena,

    var key: u8 = 0;

    fn of(lane: *LaneState) std.mem.Allocator.Error!*SingleThreadArena {
        if (lane.get(&key)) |state| return &@as(*LaneArena, @ptrCast(@alignCast(state))).arena;
        const box = try lane.allocator.create(LaneArena);
        box.* = .{ .arena = SingleThreadArena.init(lane.allocator) };
        errdefer lane.allocator.destroy(box);
        try lane.put(&key, box, destroy);
        return &box.arena;
    }

    fn destroy(opaque_box: *anyopaque) void {
        const box: *LaneArena = @ptrCast(@alignCast(opaque_box));
        const allocator = box.arena.child_allocator;
        box.arena.deinit();
        allocator.destroy(box);
    }
};

/// Durations and counts of one ordered run, recorded only when a caller asks
/// for timing. Item time is summed per lane in slots no two lanes share; the
/// owner folds the slots after the last completion.
pub const OrderedTiming = struct {
    std_io: std.Io,
    /// Elapsed time from the first submission to the last commit.
    elapsed_ns: u64 = 0,
    /// Item callbacks' own time, summed over lanes.
    worker_ns: u64 = 0,
    /// The workers' capacity over the run: its elapsed time times the
    /// executor's worker count, or its elapsed time when it ran inline.
    capacity_ns: u64 = 0,
    /// The owner's commits.
    commit_ns: u64 = 0,
    items: u64 = 0,
    chunks: u64 = 0,
    /// Executor tasks submitted; every task drains chunks until none is
    /// claimable.
    tasks: u64 = 0,
    lanes: []Lane = &.{},

    const Lane = struct {
        ns: u64 = 0,
        _padding: [std.atomic.cache_line]u8 = undefined,
    };

    fn now(self: *const OrderedTiming) i96 {
        return std.Io.Timestamp.now(self.std_io, .awake).nanoseconds;
    }

    fn since(self: *const OrderedTiming, started: i96) u64 {
        return @intCast(@max(0, self.now() - started));
    }

    /// Idle worker capacity: the capacity the item callbacks did not use.
    pub fn idleNs(self: *const OrderedTiming) u64 {
        return self.capacity_ns -| self.worker_ns;
    }

    /// Add another run's totals to these.
    pub fn add(self: *OrderedTiming, other: *const OrderedTiming) void {
        self.elapsed_ns +|= other.elapsed_ns;
        self.worker_ns +|= other.worker_ns;
        self.capacity_ns +|= other.capacity_ns;
        self.commit_ns +|= other.commit_ns;
        self.items +|= other.items;
        self.chunks +|= other.chunks;
        self.tasks +|= other.tasks;
    }
};

/// How an ordered run divides and bounds its work. Neither field affects
/// what the run commits or in which order: they decide only which lane runs
/// an item and how far ahead of the oldest uncommitted item lanes may run.
pub const OrderedOptions = struct {
    /// The smallest summed weight a chunk is given before it is closed, so
    /// many light items share one claim.
    min_chunk_weight: u64 = 1,
    timing: ?*OrderedTiming = null,
    /// Receives the most items the owner saw claimed and not yet committed,
    /// a bound on the results held at once.
    peak_claimed: ?*usize = null,
};

/// The ordered run a thread is the owner of while it waits in that run's
/// session: a drain task the owner runs inline returns after one chunk so the
/// owner gets back to committing.
threadlocal var waiting_owner: ?*const anyopaque = null;

/// Run `ctx.runItem(i, worker)` for every `i` in `0..count` on the executor's
/// lanes, and `ctx.commitItem(i)` on the calling thread in increasing `i`,
/// each as soon as every item before it has committed and item `i` has run.
///
/// `Ctx` declares `const Error`, an error set containing `OutOfMemory`;
/// `fn weight(*const Ctx, usize) u64`, a scheduling estimate of an item's
/// cost; `fn runItem(*Ctx, usize, Worker) Error!void`; and
/// `fn commitItem(*Ctx, usize) Error!void`.
///
/// Lanes claim consecutive items in chunks from one shared counter, never
/// more than a bounded window of chunks past the oldest uncommitted one, so
/// results waiting for their commit are bounded too. Every lane running the
/// run's items gets a lane state of the run's own, which the run destroys
/// before it returns: what an item registers in `Worker.lane_state` serves the
/// later items that lane runs and outlives none of them. Each lane keeps its
/// item scratch between items (`lane_scratch_retain_limit`); an item's
/// `Worker.scratch` dies when its `runItem` returns. Commits run while lanes
/// keep running later items, so `commitItem` must not change anything a
/// `runItem` reads. Commit order is item order for every worker count and
/// chunking, so output depends only on `ctx`.
///
/// The first error, from an item or a commit, stops lanes claiming further
/// chunks; every item already claimed finishes, no further item commits, and
/// the error is returned. Without an executor, items run and commit in order
/// on `sequential_lane`, whose lifetime is the caller's.
pub fn runOrdered(
    comptime Ctx: type,
    allocator: std.mem.Allocator,
    ctx: *Ctx,
    count: usize,
    executor: ?Executor,
    sequential_lane: *LaneState,
    options: OrderedOptions,
) Ctx.Error!void {
    if (count == 0) return;
    const timing = options.timing;
    const started: i96 = if (timing) |active| active.now() else 0;
    if (timing) |active| active.items +|= count;
    const exec = executor orelse {
        if (options.peak_claimed) |peak| peak.* = @max(peak.*, 1);
        const arena = try LaneArena.of(sequential_lane);
        for (0..count) |index| {
            const worker: Worker = .{ .id = 0, .allocator = allocator, .scratch = arena.allocator(), .lane_state = sequential_lane };
            const item_started: i96 = if (timing) |active| active.now() else 0;
            const item_result = ctx.runItem(index, worker);
            _ = arena.reset(.{ .retain_with_limit = lane_scratch_retain_limit });
            const commit_started: i96 = if (timing) |active| active.now() else 0;
            if (timing) |active| active.worker_ns +|= @intCast(@max(0, commit_started - item_started));
            try item_result;
            try ctx.commitItem(index);
            if (timing) |active| active.commit_ns +|= active.since(commit_started);
        }
        if (timing) |active| {
            const elapsed = active.since(started);
            active.elapsed_ns +|= elapsed;
            active.capacity_ns +|= elapsed;
            active.chunks +|= count;
        }
        return;
    };
    var run = try OrderedRun(Ctx).init(allocator, ctx, count, exec, options);
    defer run.deinit(allocator);
    if (timing) |active| active.chunks +|= run.chunks.len;
    const result = run.drive();
    if (options.peak_claimed) |peak| peak.* = @max(peak.*, run.peak_claimed, 1);
    if (timing) |active| {
        const elapsed = active.since(started);
        active.elapsed_ns +|= elapsed;
        active.capacity_ns +|= elapsed *| @as(u64, exec.worker_count);
        active.commit_ns +|= run.commit_ns;
        active.tasks +|= run.tasks;
        for (run.lane_ns) |lane| active.worker_ns +|= lane.ns;
    }
    return result;
}

/// The state one `runOrdered` call shares between its owner and the drain
/// tasks it submits.
fn OrderedRun(comptime Ctx: type) type {
    return struct {
        const Self = @This();

        const ChunkState = enum(u8) { pending, done, failed };

        const Chunk = struct {
            start: usize,
            end: usize,
            state: std.atomic.Value(ChunkState) = .init(.pending),
            /// The item error, written by the lane that ran the chunk before
            /// it publishes `failed`.
            err: Ctx.Error = undefined,
        };

        ctx: *Ctx,
        executor: Executor,
        timing: ?*OrderedTiming,
        chunks: []Chunk,
        window: usize,
        /// The next chunk a lane may claim.
        next_claim: std.atomic.Value(usize) = .init(0),
        /// Chunks committed so far; lanes claim only below this plus `window`.
        committed: std.atomic.Value(usize) = .init(0),
        /// The index plus one of the chunk whose completion the owner is
        /// blocked on, or zero when it awaits none. The lane finishing it
        /// returns from its drain task, so its completion wakes the owner.
        awaited: std.atomic.Value(usize) = .init(0),
        stop: std.atomic.Value(bool) = .init(false),
        /// The run's lane states, by worker id, each created by the first
        /// item its lane runs and touched only by that lane until the run
        /// has received every task.
        lanes: []?LaneState,
        lane_ns: []OrderedTiming.Lane = &.{},
        commit_ns: u64 = 0,
        tasks: u64 = 0,
        peak_claimed: usize = 0,

        fn init(allocator: std.mem.Allocator, ctx: *Ctx, count: usize, executor: Executor, options: OrderedOptions) std.mem.Allocator.Error!Self {
            var total: u64 = 0;
            for (0..count) |index| total +|= ctx.weight(index);
            // Enough chunks per worker that a heavy chunk late in the run
            // leaves the others little to wait for.
            const chunks_per_worker = 16;
            const target = @max(options.min_chunk_weight, total / @as(u64, @intCast(executor.worker_count * chunks_per_worker)), 1);
            var chunk_count: usize = 0;
            {
                var weight: u64 = 0;
                for (0..count) |index| {
                    weight +|= ctx.weight(index);
                    if (weight >= target or index + 1 == count) {
                        chunk_count += 1;
                        weight = 0;
                    }
                }
            }
            const chunks = try allocator.alloc(Chunk, chunk_count);
            errdefer allocator.free(chunks);
            {
                var weight: u64 = 0;
                var start: usize = 0;
                var next: usize = 0;
                for (0..count) |index| {
                    weight +|= ctx.weight(index);
                    if (weight >= target or index + 1 == count) {
                        chunks[next] = .{ .start = start, .end = index + 1 };
                        next += 1;
                        start = index + 1;
                        weight = 0;
                    }
                }
                std.debug.assert(next == chunk_count);
            }
            const lanes = try allocator.alloc(?LaneState, executor.lane_count);
            errdefer allocator.free(lanes);
            @memset(lanes, null);
            const lane_ns: []OrderedTiming.Lane = if (options.timing != null) try allocator.alloc(OrderedTiming.Lane, executor.lane_count) else &.{};
            for (lane_ns) |*lane| lane.* = .{};
            return .{
                .ctx = ctx,
                .executor = executor,
                .timing = options.timing,
                .chunks = chunks,
                .window = @max(4 * executor.worker_count, 8),
                .lanes = lanes,
                .lane_ns = lane_ns,
            };
        }

        /// Runs once every task of the run has been received.
        fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            for (self.lanes) |*slot| if (slot.*) |*lane| lane.deinit();
            allocator.free(self.lanes);
            allocator.free(self.chunks);
            if (self.lane_ns.len != 0) allocator.free(self.lane_ns);
        }

        /// The run's lane state for the lane `worker` names, allocating from
        /// the allocator of the worker's own lane state.
        fn laneOf(self: *Self, worker: Worker) *LaneState {
            std.debug.assert(worker.id < self.lanes.len);
            const slot = &self.lanes[worker.id];
            if (slot.* == null) slot.* = LaneState.init(worker.lane_state.allocator);
            return &slot.*.?;
        }

        /// The next claimable chunk, or null once every chunk is claimed, the
        /// window is full, or the run is stopping.
        fn claim(self: *Self) ?usize {
            var next = self.next_claim.load(.monotonic);
            while (true) {
                if (self.stop.load(.monotonic)) return null;
                if (next >= self.chunks.len or next >= self.committed.load(.acquire) + self.window) return null;
                next = self.next_claim.cmpxchgWeak(next, next + 1, .monotonic, .monotonic) orelse return next;
            }
        }

        fn drainTask(context: *anyopaque, worker: Worker) ?*anyopaque {
            const self: *Self = @ptrCast(@alignCast(context));
            const inline_owner = if (waiting_owner) |owner| owner == @as(*const anyopaque, self) else false;
            const lane = self.laneOf(worker);
            const arena = LaneArena.of(lane) catch {
                // Claim one chunk only to report the failure in order.
                if (self.claim()) |index| self.finish(index, error.OutOfMemory);
                return self;
            };
            while (self.claim()) |index| {
                const chunk = &self.chunks[index];
                var failure: ?Ctx.Error = null;
                for (chunk.start..chunk.end) |item| {
                    const item_started: i96 = if (self.timing) |active| active.now() else 0;
                    const item_result = self.ctx.runItem(item, .{
                        .id = worker.id,
                        .allocator = worker.allocator,
                        .scratch = arena.allocator(),
                        .lane_state = lane,
                    });
                    _ = arena.reset(.{ .retain_with_limit = lane_scratch_retain_limit });
                    if (self.timing) |active| self.lane_ns[worker.id].ns +|= active.since(item_started);
                    item_result catch |err| {
                        failure = err;
                        break;
                    };
                }
                self.finish(index, failure);
                if (inline_owner or self.awaited.load(.seq_cst) == index + 1) break;
            }
            return self;
        }

        fn finish(self: *Self, index: usize, failure: ?Ctx.Error) void {
            const chunk = &self.chunks[index];
            if (failure) |err| {
                chunk.err = err;
                self.stop.store(true, .monotonic);
                chunk.state.store(.failed, .seq_cst);
            } else {
                chunk.state.store(.done, .seq_cst);
            }
        }

        /// Commit, top up and wait until every chunk is committed or the run
        /// stops, then receive every outstanding task.
        fn drive(self: *Self) Ctx.Error!void {
            var session = try self.executor.begin();
            const failure = self.loop(&session);
            self.stop.store(true, .monotonic);
            while (session.outstanding != 0) _ = self.receive(&session);
            session.end();
            if (failure) |err| return err;
        }

        fn receive(self: *Self, session: *Session) Completion {
            const previous = waiting_owner;
            waiting_owner = self;
            defer waiting_owner = previous;
            return session.receive();
        }

        fn loop(self: *Self, session: *Session) ?Ctx.Error {
            var committed: usize = 0;
            while (true) {
                while (committed < self.chunks.len) {
                    const chunk = &self.chunks[committed];
                    switch (chunk.state.load(.acquire)) {
                        .pending => break,
                        .failed => return chunk.err,
                        .done => {},
                    }
                    const commit_started: i96 = if (self.timing) |active| active.now() else 0;
                    for (chunk.start..chunk.end) |item| self.ctx.commitItem(item) catch |err| return err;
                    if (self.timing) |active| self.commit_ns +|= active.since(commit_started);
                    committed += 1;
                    self.committed.store(committed, .release);
                }
                if (committed == self.chunks.len) return null;
                const claimed = self.next_claim.load(.monotonic);
                if (claimed > committed) self.peak_claimed = @max(self.peak_claimed, self.chunks[claimed - 1].end - self.chunks[committed].start);
                while (session.tryReceive()) |_| {}
                const claimable = @min(self.chunks.len, committed + self.window) -| self.next_claim.load(.monotonic);
                while (session.outstanding < claimable and session.canSubmit()) {
                    // Task ids count the run's submissions, so observers can
                    // tell its drain tasks apart.
                    session.submit(.{ .id = @intCast(self.tasks), .context = self, .run = drainTask }) catch |err| return err;
                    self.tasks += 1;
                }
                self.awaited.store(committed + 1, .seq_cst);
                if (self.chunks[committed].state.load(.seq_cst) == .pending) {
                    // The awaited chunk is claimed by a running task, which
                    // returns once it finishes the chunk, or is claimable by
                    // a queued or running one, so a completion arrives.
                    std.debug.assert(session.outstanding != 0);
                    _ = self.receive(session);
                }
                self.awaited.store(0, .monotonic);
            }
        }
    };
}

/// Single-owner handle for incrementally feeding one bounded execution window.
///
/// Sessions are neither thread-safe nor copyable after `begin`. Every accepted
/// task must be received before `end`, so caller-owned task contexts remain live
/// until their callbacks have returned.
pub const Session = struct {
    executor: Executor,
    handle: *anyopaque,
    outstanding: usize = 0,
    ended: bool = false,

    pub fn canSubmit(self: *const Session) bool {
        return !self.ended and self.outstanding < self.executor.worker_count;
    }

    pub fn submit(self: *Session, task: Task) std.mem.Allocator.Error!void {
        std.debug.assert(self.canSubmit());
        try self.executor.submitFn(self.executor.context, self.handle, task);
        self.outstanding += 1;
    }

    /// Receive the next completion in arrival order, running the session's
    /// queued tasks on this thread while none is filed.
    pub fn receive(self: *Session) Completion {
        std.debug.assert(!self.ended and self.outstanding > 0);
        const completion = self.executor.receiveFn(self.executor.context, self.handle);
        self.outstanding -= 1;
        return completion;
    }

    /// A completion already filed, without waiting or running anything.
    pub fn tryReceive(self: *Session) ?Completion {
        std.debug.assert(!self.ended);
        if (self.outstanding == 0) return null;
        const completion = self.executor.tryReceiveFn(self.executor.context, self.handle) orelse return null;
        self.outstanding -= 1;
        return completion;
    }

    /// Release the executor only after every accepted completion was consumed.
    pub fn end(self: *Session) void {
        std.debug.assert(!self.ended and self.outstanding == 0);
        self.executor.endFn(self.executor.context, self.handle);
        self.ended = true;
    }
};

test "post-check executor run drains accepted tasks after submit OOM" {
    const TaskContext = struct {
        ran: bool = false,

        fn run(context_opaque: *anyopaque, _: Worker) ?*anyopaque {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            self.ran = true;
            return self;
        }
    };
    const Harness = struct {
        accepted: [2]Task = undefined,
        accepted_len: usize = 0,
        received: usize = 0,
        lane_state: *LaneState,

        fn begin(context_opaque: *anyopaque) std.mem.Allocator.Error!*anyopaque {
            return context_opaque;
        }
        fn submitTask(context_opaque: *anyopaque, _: *anyopaque, task: Task) std.mem.Allocator.Error!void {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            if (self.accepted_len == self.accepted.len) return error.OutOfMemory;
            self.accepted[self.accepted_len] = task;
            self.accepted_len += 1;
        }
        fn receive(context_opaque: *anyopaque, _: *anyopaque) Completion {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            const task = self.accepted[self.received];
            self.received += 1;
            return .{
                .id = task.id,
                .worker_id = 0,
                .value = task.run(task.context, .{
                    .id = 0,
                    .allocator = std.testing.allocator,
                    .scratch = std.testing.allocator,
                    .lane_state = self.lane_state,
                }),
            };
        }
        fn tryReceive(_: *anyopaque, _: *anyopaque) ?Completion {
            return null;
        }
        fn end(context_opaque: *anyopaque, _: *anyopaque) void {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            std.debug.assert(self.received == self.accepted_len);
        }
    };

    var lane_state = LaneState.init(std.testing.allocator);
    defer lane_state.deinit();
    var harness = Harness{ .lane_state = &lane_state };
    const executor = Executor{
        .context = &harness,
        .worker_count = 3,
        .lane_count = 3,
        .beginFn = Harness.begin,
        .submitFn = Harness.submitTask,
        .receiveFn = Harness.receive,
        .tryReceiveFn = Harness.tryReceive,
        .endFn = Harness.end,
    };
    var contexts = [_]TaskContext{ .{}, .{}, .{} };
    const tasks = [_]Task{
        .{ .id = 10, .context = &contexts[0], .run = TaskContext.run },
        .{ .id = 11, .context = &contexts[1], .run = TaskContext.run },
        .{ .id = 12, .context = &contexts[2], .run = TaskContext.run },
    };
    var completions: [3]Completion = undefined;
    try std.testing.expectError(error.OutOfMemory, executor.run(&tasks, &completions));
    try std.testing.expectEqual(@as(usize, 2), harness.received);
    try std.testing.expectEqual(@as(usize, 10), completions[0].id);
    try std.testing.expectEqual(@as(usize, 11), completions[1].id);
    try std.testing.expect(contexts[0].ran);
    try std.testing.expect(contexts[1].ran);
    try std.testing.expect(!contexts[2].ran);
}

test "post-check executor never admits more tasks than worker capacity" {
    const TaskContext = struct {
        ran: bool = false,

        fn run(context_opaque: *anyopaque, _: Worker) ?*anyopaque {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            self.ran = true;
            return self;
        }
    };
    const Harness = struct {
        accepted: [7]Task = undefined,
        accepted_len: usize = 0,
        received: usize = 0,
        active: usize = 0,
        peak_active: usize = 0,
        lane_state: *LaneState,

        fn begin(context_opaque: *anyopaque) std.mem.Allocator.Error!*anyopaque {
            return context_opaque;
        }

        fn submitTask(context_opaque: *anyopaque, _: *anyopaque, task: Task) std.mem.Allocator.Error!void {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            if (self.active >= 2) {
                invariant("{s}", .{"post-check executor admitted work beyond its worker capacity"});
            }
            self.accepted[self.accepted_len] = task;
            self.accepted_len += 1;
            self.active += 1;
            self.peak_active = @max(self.peak_active, self.active);
        }

        fn receive(context_opaque: *anyopaque, _: *anyopaque) Completion {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            const task = self.accepted[self.received];
            self.received += 1;
            const value = task.run(task.context, .{
                .id = task.id % 2,
                .allocator = std.testing.allocator,
                .scratch = std.testing.allocator,
                .lane_state = self.lane_state,
            });
            self.active -= 1;
            return .{
                .id = task.id,
                .worker_id = task.id % 2,
                .value = value,
            };
        }

        fn tryReceive(_: *anyopaque, _: *anyopaque) ?Completion {
            return null;
        }

        fn end(context_opaque: *anyopaque, _: *anyopaque) void {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            std.debug.assert(self.active == 0);
            std.debug.assert(self.received == self.accepted_len);
        }
    };

    var lane_state = LaneState.init(std.testing.allocator);
    defer lane_state.deinit();
    var harness = Harness{ .lane_state = &lane_state };
    const executor = Executor{
        .context = &harness,
        .worker_count = 2,
        .lane_count = 2,
        .beginFn = Harness.begin,
        .submitFn = Harness.submitTask,
        .receiveFn = Harness.receive,
        .tryReceiveFn = Harness.tryReceive,
        .endFn = Harness.end,
    };
    var contexts = [_]TaskContext{ .{}, .{}, .{}, .{}, .{}, .{}, .{} };
    var tasks: [contexts.len]Task = undefined;
    var completions: [contexts.len]Completion = undefined;
    for (&tasks, &contexts, 0..) |*task, *context, index| {
        task.* = .{ .id = index, .context = context, .run = TaskContext.run };
    }

    try executor.run(&tasks, &completions);

    try std.testing.expectEqual(@as(usize, 2), harness.peak_active);
    try std.testing.expectEqual(@as(usize, 0), harness.active);
    try std.testing.expectEqual(tasks.len, harness.received);
    for (contexts) |context| try std.testing.expect(context.ran);
}

/// An executor over real threads for tests, with the coordinator's
/// contract: a session's owner runs its queued tasks itself while it waits,
/// and every thread has its own lane. Only test code may reference it.
pub const ThreadedTestExecutor = struct {
    const Mutex = std.Io.Mutex;
    const Condition = std.Io.Condition;
    const io = std.testing.io;

    mutex: Mutex = .init,
    task_ready: Condition = .init,
    completion_ready: Condition = .init,
    tasks: std.ArrayList(Task) = .empty,
    completions: std.ArrayList(Completion) = .empty,
    threads: []std.Thread = &.{},
    lanes: []LaneState = &.{},
    closing: bool = false,
    /// Submissions accepted before every further one fails; `unlimited`
    /// accepts every submission.
    submit_limit: usize = unlimited,
    submitted: usize = 0,

    /// A `submit_limit` above every submission count.
    const unlimited: usize = std.math.maxInt(usize);

    pub fn start(self: *ThreadedTestExecutor, thread_count: usize) (std.mem.Allocator.Error || std.Thread.SpawnError)!void {
        self.lanes = try std.testing.allocator.alloc(LaneState, thread_count + 1);
        for (self.lanes) |*lane| lane.* = LaneState.init(std.testing.allocator);
        self.threads = try std.testing.allocator.alloc(std.Thread, thread_count);
        for (self.threads, 0..) |*thread, index| thread.* = try std.Thread.spawn(.{}, workerMain, .{ self, index });
    }

    pub fn stop(self: *ThreadedTestExecutor) void {
        self.mutex.lockUncancelable(io);
        self.closing = true;
        self.task_ready.broadcast(io);
        self.mutex.unlock(io);
        for (self.threads) |thread| thread.join();
        std.testing.allocator.free(self.threads);
        for (self.lanes) |*lane| lane.deinit();
        std.testing.allocator.free(self.lanes);
        self.tasks.deinit(std.testing.allocator);
        self.completions.deinit(std.testing.allocator);
    }

    pub fn executor(self: *ThreadedTestExecutor) Executor {
        return .{
            .context = self,
            .worker_count = @max(self.threads.len, 1),
            .lane_count = self.lanes.len,
            .beginFn = begin,
            .submitFn = submit,
            .receiveFn = receive,
            .tryReceiveFn = tryReceive,
            .endFn = end,
        };
    }

    fn runTask(self: *ThreadedTestExecutor, task: Task, lane: usize) void {
        const value = task.run(task.context, .{
            .id = lane,
            .allocator = std.testing.allocator,
            .scratch = std.testing.allocator,
            .lane_state = &self.lanes[lane],
        });
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        self.completions.append(std.testing.allocator, .{ .id = task.id, .worker_id = lane, .value = value }) catch @panic("test executor completion list");
        self.completion_ready.broadcast(io);
    }

    fn workerMain(self: *ThreadedTestExecutor, lane: usize) void {
        while (true) {
            self.mutex.lockUncancelable(io);
            while (self.tasks.items.len == 0 and !self.closing) self.task_ready.waitUncancelable(io, &self.mutex);
            if (self.tasks.items.len == 0) {
                self.mutex.unlock(io);
                return;
            }
            const task = self.tasks.orderedRemove(0);
            self.mutex.unlock(io);
            self.runTask(task, lane);
        }
    }

    fn begin(context: *anyopaque) std.mem.Allocator.Error!*anyopaque {
        return context;
    }

    fn submit(context: *anyopaque, _: *anyopaque, task: Task) std.mem.Allocator.Error!void {
        const self: *ThreadedTestExecutor = @ptrCast(@alignCast(context));
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        if (self.submitted >= self.submit_limit) return error.OutOfMemory;
        self.submitted += 1;
        try self.tasks.append(std.testing.allocator, task);
        self.task_ready.signal(io);
    }

    fn receive(context: *anyopaque, _: *anyopaque) Completion {
        const self: *ThreadedTestExecutor = @ptrCast(@alignCast(context));
        while (true) {
            self.mutex.lockUncancelable(io);
            if (self.completions.items.len != 0) {
                defer self.mutex.unlock(io);
                return self.completions.orderedRemove(0);
            }
            if (self.tasks.items.len != 0) {
                const task = self.tasks.orderedRemove(0);
                self.mutex.unlock(io);
                self.runTask(task, self.threads.len);
                continue;
            }
            while (self.completions.items.len == 0) self.completion_ready.waitUncancelable(io, &self.mutex);
            self.mutex.unlock(io);
        }
    }

    fn tryReceive(context: *anyopaque, _: *anyopaque) ?Completion {
        const self: *ThreadedTestExecutor = @ptrCast(@alignCast(context));
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        if (self.completions.items.len == 0) return null;
        return self.completions.orderedRemove(0);
    }

    fn end(context: *anyopaque, _: *anyopaque) void {
        const self: *ThreadedTestExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.tasks.items.len == 0 and self.completions.items.len == 0);
    }
};

/// Items whose weights and work vary with their index, recording how often
/// each ran and the order of commits.
const OrderedTestItems = struct {
    pub const Error = error{ OutOfMemory, Canceled };

    runs: []std.atomic.Value(u32),
    commits: std.ArrayList(usize) = .empty,
    running: std.atomic.Value(usize) = .init(0),
    fail_item: ?usize = null,
    cancel_commit: ?usize = null,
    lanes_seen: std.atomic.Value(usize) = .init(0),

    fn init(count: usize) std.mem.Allocator.Error!OrderedTestItems {
        const runs = try std.testing.allocator.alloc(std.atomic.Value(u32), count);
        for (runs) |*run| run.* = .init(0);
        return .{ .runs = runs };
    }

    fn deinit(self: *OrderedTestItems) void {
        std.testing.allocator.free(self.runs);
        self.commits.deinit(std.testing.allocator);
    }

    pub fn weight(_: *const OrderedTestItems, index: usize) u64 {
        return 1 + (index * 7919) % 13;
    }

    pub fn runItem(self: *OrderedTestItems, index: usize, worker: Worker) Error!void {
        _ = self.running.fetchAdd(1, .seq_cst);
        defer _ = self.running.fetchSub(1, .seq_cst);
        _ = self.lanes_seen.fetchOr(@as(usize, 1) << @intCast(worker.id % @bitSizeOf(usize)), .monotonic);
        // Scratch dies with the item; use it so a lane's retained arena is
        // exercised.
        const bytes = try worker.scratch.alloc(u8, 64 + (index % 5) * 1024);
        @memset(bytes, @truncate(index));
        var spin: u64 = 0;
        for (0..(index % 17) * 200) |step| spin +%= step *% bytes[step % bytes.len];
        std.mem.doNotOptimizeAway(spin);
        _ = self.runs[index].fetchAdd(1, .seq_cst);
        if (self.fail_item == index) return error.OutOfMemory;
    }

    pub fn commitItem(self: *OrderedTestItems, index: usize) Error!void {
        if (self.runs[index].load(.seq_cst) != 1) return error.Canceled;
        if (self.cancel_commit == index) return error.Canceled;
        try self.commits.append(std.testing.allocator, index);
    }
};

test "ordered run runs every item once and commits in order at every worker count" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    for ([_]usize{ 0, 1, 2, 3, 4, 8 }) |threads| {
        for ([_]usize{ 1, 2, 31, 500 }) |count| {
            var pool = ThreadedTestExecutor{};
            try pool.start(threads);
            defer pool.stop();
            var items = try OrderedTestItems.init(count);
            defer items.deinit();
            var timing = OrderedTiming{ .std_io = std.testing.io };
            try runOrdered(OrderedTestItems, std.testing.allocator, &items, count, pool.executor(), &sequential, .{ .timing = &timing });
            try std.testing.expectEqual(count, items.commits.items.len);
            for (items.commits.items, 0..) |committed, index| try std.testing.expectEqual(index, committed);
            for (items.runs) |run| try std.testing.expectEqual(@as(u32, 1), run.load(.seq_cst));
            try std.testing.expectEqual(@as(u64, count), timing.items);
            try std.testing.expect(timing.tasks >= 1);
        }
    }
}

test "ordered run without an executor runs and commits in order on the sequential lane" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    var items = try OrderedTestItems.init(40);
    defer items.deinit();
    try runOrdered(OrderedTestItems, std.testing.allocator, &items, 40, null, &sequential, .{});
    for (items.commits.items, 0..) |committed, index| try std.testing.expectEqual(index, committed);
    try std.testing.expectEqual(@as(usize, 40), items.commits.items.len);
}

test "ordered run stops at a failing item and drains every claimed item" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    for ([_]?usize{ null, 0, 1, 2, 4 }) |maybe_threads| {
        for ([_]usize{ 0, 7, 199 }) |fail_item| {
            var pool = ThreadedTestExecutor{};
            if (maybe_threads) |threads| try pool.start(threads);
            defer if (maybe_threads != null) pool.stop();
            var items = try OrderedTestItems.init(200);
            defer items.deinit();
            items.fail_item = fail_item;
            const executor: ?Executor = if (maybe_threads != null) pool.executor() else null;
            try std.testing.expectError(error.OutOfMemory, runOrdered(OrderedTestItems, std.testing.allocator, &items, 200, executor, &sequential, .{}));
            try std.testing.expectEqual(@as(usize, 0), items.running.load(.seq_cst));
            try std.testing.expect(items.commits.items.len <= fail_item);
            for (items.commits.items, 0..) |committed, index| try std.testing.expectEqual(index, committed);
            for (items.runs) |run| try std.testing.expect(run.load(.seq_cst) <= 1);
        }
    }
}

test "ordered run stops at a failing commit and commits nothing after it" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    for ([_]?usize{ null, 0, 1, 3, 8 }) |maybe_threads| {
        var pool = ThreadedTestExecutor{};
        if (maybe_threads) |threads| try pool.start(threads);
        defer if (maybe_threads != null) pool.stop();
        var items = try OrderedTestItems.init(300);
        defer items.deinit();
        items.cancel_commit = 120;
        const executor: ?Executor = if (maybe_threads != null) pool.executor() else null;
        try std.testing.expectError(error.Canceled, runOrdered(OrderedTestItems, std.testing.allocator, &items, 300, executor, &sequential, .{}));
        try std.testing.expectEqual(@as(usize, 0), items.running.load(.seq_cst));
        try std.testing.expectEqual(@as(usize, 120), items.commits.items.len);
        for (items.commits.items, 0..) |committed, index| try std.testing.expectEqual(index, committed);
    }
}

test "ordered run reports a refused submission after draining accepted tasks" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    for ([_]usize{ 0, 1, 2 }) |accepted| {
        var pool = ThreadedTestExecutor{ .submit_limit = accepted };
        try pool.start(4);
        defer pool.stop();
        var items = try OrderedTestItems.init(400);
        defer items.deinit();
        try std.testing.expectError(error.OutOfMemory, runOrdered(OrderedTestItems, std.testing.allocator, &items, 400, pool.executor(), &sequential, .{}));
        try std.testing.expectEqual(@as(usize, 0), items.running.load(.seq_cst));
        for (items.commits.items, 0..) |committed, index| try std.testing.expectEqual(index, committed);
    }
}

/// Items that each register one value per lane in `Worker.lane_state`,
/// counting how many values were created and destroyed.
const LaneStateItems = struct {
    pub const Error = std.mem.Allocator.Error;

    created: std.atomic.Value(usize) = .init(0),
    destroyed: std.atomic.Value(usize) = .init(0),

    const Value = struct {
        items: *LaneStateItems,
        uses: usize = 0,

        fn destroy(opaque_value: *anyopaque) void {
            const value: *Value = @ptrCast(@alignCast(opaque_value));
            _ = value.items.destroyed.fetchAdd(1, .seq_cst);
            std.testing.allocator.destroy(value);
        }
    };

    var key: u8 = 0;

    pub fn weight(_: *const LaneStateItems, _: usize) u64 {
        return 1;
    }

    pub fn runItem(self: *LaneStateItems, _: usize, worker: Worker) Error!void {
        const value: *Value = if (worker.lane_state.get(&key)) |existing| @ptrCast(@alignCast(existing)) else blk: {
            const fresh = try std.testing.allocator.create(Value);
            fresh.* = .{ .items = self };
            try worker.lane_state.put(&key, fresh, Value.destroy);
            _ = self.created.fetchAdd(1, .seq_cst);
            break :blk fresh;
        };
        value.uses += 1;
    }

    pub fn commitItem(_: *LaneStateItems, _: usize) Error!void {}
};

test "ordered run lane state serves its items and is destroyed with the run" {
    var sequential = LaneState.init(std.testing.allocator);
    defer sequential.deinit();
    for ([_]usize{ 1, 2, 4 }) |threads| {
        var pool = ThreadedTestExecutor{};
        try pool.start(threads);
        defer pool.stop();
        var items = LaneStateItems{};
        try runOrdered(LaneStateItems, std.testing.allocator, &items, 300, pool.executor(), &sequential, .{});
        const created = items.created.load(.seq_cst);
        try std.testing.expect(created >= 1);
        try std.testing.expect(created <= pool.executor().lane_count);
        try std.testing.expectEqual(created, items.destroyed.load(.seq_cst));
    }
}

test "ordered run lane scratch keeps its capacity between items" {
    var meter = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    var lane = LaneState.init(meter.allocator());
    defer lane.deinit();
    // Item 4 asks for the most scratch, so every later item fits in what the
    // lane kept.
    var warmup = try OrderedTestItems.init(5);
    defer warmup.deinit();
    try runOrdered(OrderedTestItems, std.testing.allocator, &warmup, 5, null, &lane, .{});
    const warmed = meter.allocations;
    var items = try OrderedTestItems.init(64);
    defer items.deinit();
    try runOrdered(OrderedTestItems, std.testing.allocator, &items, 64, null, &lane, .{});
    try std.testing.expectEqual(warmed, meter.allocations);
}
