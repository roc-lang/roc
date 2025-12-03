//! LSP diagnostic types for reporting errors, warnings, and hints to editors.

const std = @import("std");

/// The position of the diagnostic
pub const Position = struct {
    line: u32,
    character: u32,
};

/// The range of characters that the diagnostic applies to
pub const Range = struct {
    start: Position,
    end: Position,
};

/// Diagnostic expected by LSP specifications
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
