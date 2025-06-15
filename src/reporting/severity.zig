//! Severity levels for compilation problems.
//!
//! This module defines the different severity levels that can be assigned to
//! compilation problems, from warnings that don't block compilation to fatal
//! errors that prevent any further processing.

const std = @import("std");

/// Represents the severity level of a compilation problem.
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

    /// Returns true if this severity should block compilation.
    pub fn isBlocking(self: Severity) bool {
        return switch (self) {
            .warning => false,
            .runtime_error => false, // Allows compilation to continue with malformed nodes
            .fatal => true,
        };
    }

    /// Returns true if this severity indicates a compiler bug.
    pub fn isCompilerBug(self: Severity) bool {
        return switch (self) {
            .warning, .runtime_error => false,
            .fatal => true,
        };
    }

    /// Returns the priority order for displaying problems (lower is higher priority).
    pub fn priority(self: Severity) u8 {
        return switch (self) {
            .fatal => 0,
            .runtime_error => 1,
            .warning => 2,
        };
    }

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

// Tests
const testing = std.testing;

test "Severity blocking behavior" {
    try testing.expect(!Severity.warning.isBlocking());
    try testing.expect(!Severity.runtime_error.isBlocking());
    try testing.expect(Severity.fatal.isBlocking());
}

test "Severity compiler bug detection" {
    try testing.expect(!Severity.warning.isCompilerBug());
    try testing.expect(!Severity.runtime_error.isCompilerBug());
    try testing.expect(Severity.fatal.isCompilerBug());
}

test "Severity priority ordering" {
    try testing.expect(Severity.fatal.priority() < Severity.runtime_error.priority());
    try testing.expect(Severity.runtime_error.priority() < Severity.warning.priority());
}

test "Severity string representations" {
    try testing.expectEqualStrings("WARNING", Severity.warning.toString());
    try testing.expectEqualStrings("ERROR", Severity.runtime_error.toString());
    try testing.expectEqualStrings("FATAL", Severity.fatal.toString());

    try testing.expectEqualStrings("W", Severity.warning.toCode());
    try testing.expectEqualStrings("E", Severity.runtime_error.toCode());
    try testing.expectEqualStrings("F", Severity.fatal.toCode());
}
