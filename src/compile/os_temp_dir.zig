//! Platform-aware temporary directory resolution.
//!
//! Shared between the compiler cache system and the test runner
//! so that both use the same OS-appropriate temp base path.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

/// Returns the OS-appropriate temporary directory base path.
/// - Windows: %TEMP%, %TMP%, or C:\Windows\Temp
/// - Unix: $TMPDIR or /tmp
pub fn getOsTempDir(allocator: Allocator) ![]u8 {
    return switch (builtin.target.os.tag) {
        .windows => std.process.getEnvVarOwned(allocator, "TEMP") catch
            std.process.getEnvVarOwned(allocator, "TMP") catch
            try allocator.dupe(u8, "C:\\Windows\\Temp"),
        else => std.process.getEnvVarOwned(allocator, "TMPDIR") catch
            try allocator.dupe(u8, "/tmp"),
    };
}
