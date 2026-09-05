//! Type-erased task execution shared by compiler stages without introducing stage dependencies.

const std = @import("std");

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
            @panic("post-check executor lane state key was registered more than once");
        }
        try self.entries.append(self.allocator, .{
            .key = key,
            .value = value,
            .deinitFn = deinitFn,
        });
    }
};

/// Exclusive execution lane and its persistent and task-scoped allocators.
pub const Worker = struct {
    id: usize,
    /// Allocator for task-owned output. The caller owns cleanup of allocations
    /// retained in `Completion.value`.
    allocator: std.mem.Allocator,
    /// Temporary storage which becomes invalid when the task callback returns.
    scratch: std.mem.Allocator,
    /// Type-erased mutable state owned by this physical lane. Values registered
    /// here survive synchronous batch boundaries and are destroyed with the
    /// worker pool.
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
pub const Executor = struct {
    context: *anyopaque,
    worker_count: usize,
    beginFn: *const fn (*anyopaque) void,
    submitFn: *const fn (*anyopaque, Task) std.mem.Allocator.Error!void,
    /// Must synchronize with the completed callback so its writes are visible
    /// before the returned value is consumed.
    receiveFn: *const fn (*anyopaque) Completion,
    endFn: *const fn (*anyopaque) void,

    /// Start an exclusive streaming session.
    pub fn begin(self: Executor) Session {
        std.debug.assert(self.worker_count > 0);
        self.beginFn(self.context);
        return .{ .executor = self };
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
        var session = self.begin();
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

/// Single-owner handle for incrementally feeding one bounded execution window.
///
/// Sessions are neither thread-safe nor copyable after `begin`. Every accepted
/// task must be received before `end`, so caller-owned task contexts remain live
/// until their callbacks have returned.
pub const Session = struct {
    executor: Executor,
    outstanding: usize = 0,
    ended: bool = false,

    pub fn canSubmit(self: *const Session) bool {
        return !self.ended and self.outstanding < self.executor.worker_count;
    }

    pub fn submit(self: *Session, task: Task) std.mem.Allocator.Error!void {
        std.debug.assert(self.canSubmit());
        try self.executor.submitFn(self.executor.context, task);
        self.outstanding += 1;
    }

    /// Receive the next completion in arrival order.
    pub fn receive(self: *Session) Completion {
        std.debug.assert(!self.ended and self.outstanding > 0);
        const completion = self.executor.receiveFn(self.executor.context);
        self.outstanding -= 1;
        return completion;
    }

    /// Release the executor only after every accepted completion was consumed.
    pub fn end(self: *Session) void {
        std.debug.assert(!self.ended and self.outstanding == 0);
        self.executor.endFn(self.executor.context);
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

        fn begin(_: *anyopaque) void {}
        fn submitTask(context_opaque: *anyopaque, task: Task) std.mem.Allocator.Error!void {
            const self: *@This() = @ptrCast(@alignCast(context_opaque));
            if (self.accepted_len == self.accepted.len) return error.OutOfMemory;
            self.accepted[self.accepted_len] = task;
            self.accepted_len += 1;
        }
        fn receive(context_opaque: *anyopaque) Completion {
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
        fn end(context_opaque: *anyopaque) void {
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
        .beginFn = Harness.begin,
        .submitFn = Harness.submitTask,
        .receiveFn = Harness.receive,
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
