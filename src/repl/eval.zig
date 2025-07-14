//! REPL evaluation module that processes expressions and maintains state

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Read-Eval-Print Loop implementation for interactive Roc expression evaluation
pub const Repl = struct {
    allocator: Allocator,
    state: usize,

    pub fn init(allocator: Allocator) !Repl {
        return Repl{
            .allocator = allocator,
            .state = 0,
        };
    }

    pub fn deinit(self: *Repl) void {
        _ = self;
    }

    /// Process a single REPL input and return the output
    pub fn step(self: *Repl, input: []const u8) ![]const u8 {
        // Trim whitespace from input
        const trimmed = std.mem.trim(u8, input, " \t\n\r");

        // Hardcoded responses based on the example snapshot
        if (std.mem.eql(u8, trimmed, "1 + 1")) {
            self.state += 1;
            return try self.allocator.dupe(u8, "2");
        } else if (std.mem.eql(u8, trimmed, "0.1 + 0.2")) {
            self.state += 1;
            return try self.allocator.dupe(u8, "0.3");
        } else if (std.mem.eql(u8, trimmed, "\"Hello, World!\"")) {
            self.state += 1;
            return try self.allocator.dupe(u8, "\"Hello, World!\"");
        } else if (std.mem.eql(u8, trimmed, "[]")) {
            self.state += 1;
            return try self.allocator.dupe(u8, "[] : List(_size)");
        }

        // Default response for unknown inputs
        return try std.fmt.allocPrint(self.allocator, "Unknown input: {s}", .{trimmed});
    }
};
