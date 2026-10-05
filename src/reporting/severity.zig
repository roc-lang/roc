//! Severity levels for warning and error problem reports.

/// Represents the severity level of a problem.
pub const Severity = enum {
    /// Non-blocking issues that should be addressed.
    /// Will return a non-zero exit code to block committing to CI.
    warning,

    /// Compilation-blocking errors that are replaced with runtime-error nodes.
    /// The program will crash if it reaches the invalid code path at runtime.
    runtime_error,

    /// Critical errors that prevent compilation and cannot be recovered from.
    /// Usually indicates a bug in the compiler itself. These should be very rare.
    fatal,

    /// Returns a human-readable string representation.
    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .warning => "WARNING",
            .runtime_error => "ERROR",
            .fatal => "FATAL",
        };
    }

    /// Whether a problem of this severity is an error: it counts toward the
    /// error total and fails the command. A warning does neither.
    pub fn isError(self: Severity) bool {
        return switch (self) {
            .runtime_error, .fatal => true,
            .warning => false,
        };
    }

    /// The Language Server Protocol `DiagnosticSeverity` number.
    pub fn toLspSeverity(self: Severity) u8 {
        return switch (self) {
            .runtime_error, .fatal => 1,
            .warning => 2,
        };
    }
};

test "each severity is an error or a warning, in the compiler and in LSP" {
    const std = @import("std");
    const cases = [_]struct { severity: Severity, is_error: bool, lsp: u8 }{
        .{ .severity = .warning, .is_error = false, .lsp = 2 },
        .{ .severity = .runtime_error, .is_error = true, .lsp = 1 },
        .{ .severity = .fatal, .is_error = true, .lsp = 1 },
    };
    for (cases) |case| {
        try std.testing.expectEqual(case.is_error, case.severity.isError());
        try std.testing.expectEqual(case.lsp, case.severity.toLspSeverity());
    }
}
