//! Host-managed storage for crash messages emitted through `RocOps` callbacks.

const std = @import("std");

/// Result of the most recent crash callback invocation.
pub const CrashState = union(enum) {
    did_not_crash,
    crashed: []u8,
};

/// Host-owned bookkeeping for interpreter crashes.
///
/// A `CrashContext` captures the UTF-8 message passed to `RocOps.crash` so tests,
/// REPL integrations, or other hosts can inspect the result after evaluation
/// finishes. Each helper resets the previous state before recording new data so
/// the context can be reused across evaluations.
pub const CrashContext = struct {
    allocator: std.mem.Allocator,
    state: CrashState = .did_not_crash,

    /// Create a crash context with an initial `did_not_crash` state.
    pub fn init(allocator: std.mem.Allocator) CrashContext {
        return CrashContext{
            .allocator = allocator,
            .state = .did_not_crash,
        };
    }

    /// Release any owned crash message and reset the state.
    pub fn deinit(self: *CrashContext) void {
        self.reset();
    }

    /// Clear the recorded crash state and free owned message bytes.
    pub fn reset(self: *CrashContext) void {
        switch (self.state) {
            .did_not_crash => {},
            .crashed => |msg| self.allocator.free(msg),
        }
        self.state = .did_not_crash;
    }

    /// Store a crash message (taking ownership of a duplicated copy).
    pub fn recordCrash(self: *CrashContext, message: []const u8) !void {
        self.reset();
        const copy = try self.allocator.dupe(u8, message);
        self.state = .{ .crashed = copy };
    }

    /// Return the recorded crash message if one exists.
    pub fn crashMessage(self: *CrashContext) ?[]const u8 {
        return switch (self.state) {
            .did_not_crash => null,
            .crashed => |msg| msg,
        };
    }
};
