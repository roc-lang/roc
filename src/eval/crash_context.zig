const std = @import("std");

pub const CrashState = union(enum) {
    did_not_crash,
    crashed: []u8,
};

pub const CrashContext = struct {
    allocator: std.mem.Allocator,
    state: CrashState = .did_not_crash,

    pub fn init(allocator: std.mem.Allocator) CrashContext {
        return CrashContext{
            .allocator = allocator,
            .state = .did_not_crash,
        };
    }

    pub fn deinit(self: *CrashContext) void {
        self.reset();
    }

    pub fn reset(self: *CrashContext) void {
        switch (self.state) {
            .did_not_crash => {},
            .crashed => |msg| self.allocator.free(msg),
        }
        self.state = .did_not_crash;
    }

    pub fn recordCrash(self: *CrashContext, message: []const u8) !void {
        self.reset();
        const copy = try self.allocator.dupe(u8, message);
        self.state = .{ .crashed = copy };
    }

    pub fn crashMessage(self: *CrashContext) ?[]const u8 {
        return switch (self.state) {
            .did_not_crash => null,
            .crashed => |msg| msg,
        };
    }
};
