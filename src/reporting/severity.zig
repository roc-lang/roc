//! Severity levels for warning and error problem reports.

const std = @import("std");

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

    /// Returns a short code suitable for prefixing error messages.
    pub fn toCode(self: Severity) []const u8 {
        return switch (self) {
            .warning => "W",
            .runtime_error => "E",
            .fatal => "F",
        };
    }
};
