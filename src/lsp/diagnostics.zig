const std = @import("std");

pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Diagnostic = struct {
    range: Range,
    severity: ?u32 = null,
    source: ?[]const u8 = null,
    message: []const u8,
};

/// Container for diagnostics destined for a single URI.
pub const PublishDiagnostics = struct {
    uri: []u8,
    diagnostics: []Diagnostic,

    pub fn deinit(self: *PublishDiagnostics, allocator: std.mem.Allocator) void {
        allocator.free(self.uri);
        for (self.diagnostics) |diag| {
            allocator.free(diag.message);
        }
        allocator.free(self.diagnostics);
        self.* = undefined;
    }
};
