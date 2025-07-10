//! Worker thread for multi-threaded module building

const std = @import("std");
const Builder = @import("Builder.zig");
const Task = @import("Task.zig");

const Allocator = std.mem.Allocator;

const Self = @This();

/// Worker ID for debugging
id: usize,
/// Allocator for this worker
allocator: Allocator,
/// Whether the worker should stop
should_stop: std.atomic.Value(bool),
/// Whether the worker is currently idle
is_idle: std.atomic.Value(bool),

/// Initialize a new worker
pub fn init(allocator: Allocator, id: usize) Self {
    return .{
        .id = id,
        .allocator = allocator,
        .should_stop = std.atomic.Value(bool).init(false),
        .is_idle = std.atomic.Value(bool).init(true),
    };
}

/// Deinitialize the worker
pub fn deinit(self: *Self) void {
    _ = self;
    // Nothing to clean up currently
}

/// Run the worker thread
pub fn run(self: *Self, builder: *Builder) void {
    while (!self.should_stop.load(.acquire)) {
        // Try to get a task
        if (builder.getNextTask()) |task| {
            self.is_idle.store(false, .release);

            // Process the task
            builder.processTask(task) catch |err| {
                std.log.err("Worker {d} failed to process task: {}", .{ self.id, err });
            };

            self.is_idle.store(true, .release);
        } else {
            // No task available, wait for one
            self.is_idle.store(true, .release);

            if (builder.config.mode == .multi_threaded) {
                builder.waitForTask();
            } else {
                // In test mode, just yield
                std.Thread.yield() catch {};
            }
        }
    }
}

/// Signal the worker to stop
pub fn stop(self: *Self) void {
    self.should_stop.store(true, .release);
}

/// Check if the worker is idle
pub fn isIdle(self: *const Self) bool {
    return self.is_idle.load(.acquire);
}
