//! Type-erased task execution shared by compiler stages without introducing stage dependencies.

const std = @import("std");

/// Exclusive execution lane and its persistent and task-scoped allocators.
pub const Worker = struct {
    id: usize,
    /// Allocator for task-owned output. The caller owns cleanup of allocations
    /// retained in `Completion.value`.
    allocator: std.mem.Allocator,
    /// Temporary storage which becomes invalid when the task callback returns.
    scratch: std.mem.Allocator,
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

/// Streaming execution uses the same exclusive lanes as batch execution.
/// A session owns the executor until every accepted task has been received.
pub const Streaming = struct {
    beginFn: *const fn (*anyopaque) void,
    submitFn: *const fn (*anyopaque, Task) std.mem.Allocator.Error!void,
    waitOneFn: *const fn (*anyopaque) Completion,
    endFn: *const fn (*anyopaque) void,
};

/// Explicit lifetime for one stream. The caller bounds unreceived submissions by
/// `Executor.worker_count`, retains task contexts, and drains even on error.
pub const Session = struct {
    context: *anyopaque,
    functions: Streaming,

    /// Enqueue without waiting for another task. Inline executors run immediately.
    pub fn submit(self: Session, task: Task) std.mem.Allocator.Error!void {
        return self.functions.submitFn(self.context, task);
    }

    /// Receive any one accepted task, transferring result ownership to the caller.
    pub fn waitOne(self: Session) Completion {
        return self.functions.waitOneFn(self.context);
    }

    /// Close after receiving every accepted task, including on allocation failure.
    pub fn end(self: Session) void {
        self.functions.endFn(self.context);
    }
};

/// Bounded task execution borrowed from a compilation coordinator.
pub const Executor = struct {
    context: *anyopaque,
    worker_count: usize,
    runFn: *const fn (*anyopaque, []const Task, []Completion) std.mem.Allocator.Error!void,
    streaming: Streaming,

    /// Open an exclusive stream.
    pub fn begin(self: Executor) Session {
        const functions = self.streaming;
        functions.beginFn(self.context);
        return .{ .context = self.context, .functions = functions };
    }

    /// Run one bounded batch. `completions` must have exactly `tasks.len` entries.
    ///
    /// Threaded executors write completions in completion/arrival order, not task
    /// order. Callers must use `Completion.id` when logical ordering matters.
    /// Inline execution naturally has task order as its completion order.
    ///
    /// A worker id denotes one exclusive persistent execution lane: callbacks
    /// with the same id never overlap, observe tasks in input order, and receive
    /// the same output allocator for the borrowed executor's lifetime. Callers
    /// may therefore retain lane-local state between synchronous `run` calls.
    ///
    /// A task's `Worker.scratch` storage dies when its callback returns.
    /// `Completion.value` must therefore point to task/caller-owned storage or
    /// storage allocated by `Worker.allocator`; the caller owns its cleanup.
    pub fn run(self: Executor, tasks: []const Task, completions: []Completion) std.mem.Allocator.Error!void {
        std.debug.assert(tasks.len == completions.len);
        return self.runFn(self.context, tasks, completions);
    }
};
