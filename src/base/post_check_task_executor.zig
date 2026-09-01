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

/// Synchronous bounded-batch executor borrowed from a compilation coordinator.
pub const Executor = struct {
    context: *anyopaque,
    worker_count: usize,
    runFn: *const fn (*anyopaque, []const Task, []Completion) std.mem.Allocator.Error!void,

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
