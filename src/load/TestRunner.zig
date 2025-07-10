//! Test runner for simulating multi-threaded execution in a controlled manner

const std = @import("std");
const Builder = @import("Builder.zig");
const Task = @import("Task.zig");

const Allocator = std.mem.Allocator;

const Self = @This();

/// The builder being tested
builder: *Builder,
/// Execution log for debugging
execution_log: std.ArrayList(LogEntry),
/// Simulated time
current_time: u64 = 0,
/// Random number generator for chaos testing
rng: std.Random.DefaultPrng,

/// Log entry for execution history
pub const LogEntry = struct {
    time: u64,
    event: Event,
};

/// Events that can be logged
pub const Event = union(enum) {
    task_started: TaskInfo,
    task_completed: TaskInfo,
    task_failed: struct {
        info: TaskInfo,
        error_msg: []const u8,
    },
};

/// Information about a task
pub const TaskInfo = struct {
    task_type: []const u8,
    module_id: ?Builder.ModuleId,
};

/// Initialize a test runner
pub fn init(builder: *Builder, seed: u64) Self {
    return .{
        .builder = builder,
        .execution_log = std.ArrayList(LogEntry).init(builder.config.allocator),
        .rng = std.Random.DefaultPrng.init(seed),
    };
}

/// Deinitialize the test runner
pub fn deinit(self: *Self) void {
    for (self.execution_log.items) |entry| {
        switch (entry.event) {
            .task_failed => |failed| {
                self.builder.config.allocator.free(failed.error_msg);
            },
            else => {},
        }
    }
    self.execution_log.deinit();
}

/// Step through one task execution
pub fn step(self: *Self) !bool {
    // Get the next task
    const task = self.builder.getNextTask() orelse return false;

    // Log task start
    const task_info = self.getTaskInfo(task);
    try self.log(.{ .task_started = task_info });

    // Simulate some delay
    self.current_time += self.rng.random().intRangeAtMost(u64, 1, 100);

    // Process the task
    self.builder.processTask(task) catch |err| {
        const error_msg = try std.fmt.allocPrint(
            self.builder.config.allocator,
            "{}",
            .{err},
        );
        try self.log(.{ .task_failed = .{
            .info = task_info,
            .error_msg = error_msg,
        } });
        return err;
    };

    // Log task completion
    try self.log(.{ .task_completed = task_info });

    return true;
}

/// Run all tasks to completion
pub fn runAll(self: *Self) !void {
    while (try self.step()) {}
}

/// Run with simulated race conditions
pub fn runWithRaceConditions(self: *Self, max_concurrent: usize) !void {
    var active_tasks = std.ArrayList(Task).init(self.builder.config.allocator);
    defer active_tasks.deinit();

    while (true) {
        // Try to start new tasks up to the limit
        while (active_tasks.items.len < max_concurrent) {
            const task = self.builder.getNextTask() orelse break;
            try active_tasks.append(task);
        }

        // If no active tasks, we're done
        if (active_tasks.items.len == 0) break;

        // Randomly pick a task to complete
        const index = self.rng.random().intRangeLessThan(usize, 0, active_tasks.items.len);
        const task = active_tasks.orderedRemove(index);

        // Process it
        const task_info = self.getTaskInfo(task);
        try self.log(.{ .task_started = task_info });

        self.builder.processTask(task) catch |err| {
            const error_msg = try std.fmt.allocPrint(
                self.builder.config.allocator,
                "{}",
                .{err},
            );
            try self.log(.{ .task_failed = .{
                .info = task_info,
                .error_msg = error_msg,
            } });
            return err;
        };

        try self.log(.{ .task_completed = task_info });
        self.current_time += self.rng.random().intRangeAtMost(u64, 1, 100);
    }
}

/// Simulate a deadlock scenario
pub fn simulateDeadlock(self: *Self) !void {
    // Simulate a deadlock by creating circular dependencies
    // This creates tasks that depend on each other in a cycle

    // First, let's drain any existing tasks
    while (self.builder.getNextTask()) |_| {}

    // Create a scenario where modules depend on each other circularly
    // Module A imports Module B, Module B imports Module C, Module C imports Module A

    // Add parse tasks for three interdependent modules
    const module_a_id: Builder.ModuleId = 100;
    const module_b_id: Builder.ModuleId = 101;
    const module_c_id: Builder.ModuleId = 102;

    // Create parse tasks
    const parse_a = Task{
        .kind = .{ .parse = .{
            .module_id = module_a_id,
            .source = try self.builder.config.allocator.dupe(u8,
                \\interface ModuleA
                \\    imports [ModuleB]
                \\    exposes [valueA]
                \\
                \\valueA = ModuleB.valueB + 1
            ),
            .path = try self.builder.config.allocator.dupe(u8, "ModuleA.roc"),
        } },
    };

    const parse_b = Task{
        .kind = .{ .parse = .{
            .module_id = module_b_id,
            .source = try self.builder.config.allocator.dupe(u8,
                \\interface ModuleB
                \\    imports [ModuleC]
                \\    exposes [valueB]
                \\
                \\valueB = ModuleC.valueC + 1
            ),
            .path = try self.builder.config.allocator.dupe(u8, "ModuleB.roc"),
        } },
    };

    const parse_c = Task{
        .kind = .{ .parse = .{
            .module_id = module_c_id,
            .source = try self.builder.config.allocator.dupe(u8,
                \\interface ModuleC
                \\    imports [ModuleA]
                \\    exposes [valueC]
                \\
                \\valueC = ModuleA.valueA + 1
            ),
            .path = try self.builder.config.allocator.dupe(u8, "ModuleC.roc"),
        } },
    };

    // Push all parse tasks
    try self.builder.task_queue.push(parse_a);
    try self.builder.task_queue.push(parse_b);
    try self.builder.task_queue.push(parse_c);

    // Log the deadlock simulation start
    try self.log(.{ .task_started = .{
        .task_type = "deadlock_simulation",
        .module_id = null,
    } });

    // Try to process tasks - this should eventually detect the circular dependency
    var iterations: usize = 0;
    const max_iterations = 100;

    while (iterations < max_iterations) : (iterations += 1) {
        const task = self.builder.getNextTask() orelse break;

        // Process the task
        self.builder.processTask(task) catch |err| {
            // Log that we detected the deadlock
            const error_msg = try std.fmt.allocPrint(
                self.builder.config.allocator,
                "Circular dependency detected: {}",
                .{err},
            );
            try self.log(.{ .task_failed = .{
                .info = .{
                    .task_type = "deadlock_resolution",
                    .module_id = null,
                },
                .error_msg = error_msg,
            } });

            // This is expected - circular dependencies should fail
            return;
        };

        // Add a small delay to simulate processing
        self.current_time += 10;
    }

    // If we get here, log that deadlock simulation completed
    try self.log(.{ .task_completed = .{
        .task_type = "deadlock_simulation",
        .module_id = null,
    } });
}

/// Get task info for logging
fn getTaskInfo(self: *Self, task: Task) TaskInfo {
    _ = self;
    return switch (task.kind) {
        .load_file => |load| .{
            .task_type = "load_file",
            .module_id = load.module_id,
        },
        .parse => |parse_task| .{
            .task_type = "parse",
            .module_id = parse_task.module_id,
        },
        .canonicalize => |canon| .{
            .task_type = "canonicalize",
            .module_id = canon.module_id,
        },
        .type_check => |type_check| .{
            .task_type = "type_check",
            .module_id = type_check.module_id,
        },
    };
}

/// Log an event
fn log(self: *Self, event: Event) !void {
    try self.execution_log.append(.{
        .time = self.current_time,
        .event = event,
    });
}

/// Print the execution log
pub fn printLog(self: *Self, writer: anytype) !void {
    for (self.execution_log.items) |entry| {
        try writer.print("[{d:>6}] ", .{entry.time});
        switch (entry.event) {
            .task_started => |info| {
                try writer.print("Started {s}", .{info.task_type});
                if (info.module_id) |id| {
                    try writer.print(" (module {d})", .{id});
                }
            },
            .task_completed => |info| {
                try writer.print("Completed {s}", .{info.task_type});
                if (info.module_id) |id| {
                    try writer.print(" (module {d})", .{id});
                }
            },
            .task_failed => |failed| {
                try writer.print("Failed {s}", .{failed.info.task_type});
                if (failed.info.module_id) |id| {
                    try writer.print(" (module {d})", .{id});
                }
                try writer.print(": {s}", .{failed.error_msg});
            },
        }
        try writer.print("\n", .{});
    }
}
