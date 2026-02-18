//! Debug utilities for type checking
//!
//! These utilities are only active in debug builds and help catch infinite loops
//! in type-checking code by limiting the number of iterations.

const std = @import("std");
const builtin = @import("builtin");

/// Maximum number of iterations before panicking in debug builds.
/// This is set high enough to handle legitimate complex types but low enough
/// to catch infinite loops quickly during development.
pub const MAX_ITERATIONS: u32 = 100_000;

/// A debug-only iteration guard that panics if a loop exceeds MAX_ITERATIONS.
/// In release builds, this is a no-op.
///
/// Usage:
/// ```
/// var guard = IterationGuard.init("myFunction");
/// while (condition) {
///     guard.tick();
///     // ... loop body
/// }
/// ```
pub const IterationGuard = struct {
    count: u32,
    location: []const u8,

    const Self = @This();

    pub fn init(location: []const u8) Self {
        return .{
            .count = 0,
            .location = location,
        };
    }

    /// Call this at the start of each loop iteration.
    /// In debug builds, panics if MAX_ITERATIONS is exceeded.
    /// In release builds, this is a no-op that should be optimized away.
    pub inline fn tick(self: *Self) void {
        if (builtin.mode == .Debug) {
            self.count += 1;
            if (self.count > MAX_ITERATIONS) {
                std.debug.panic(
                    "Infinite loop detected in type-checking at '{s}' after {d} iterations. " ++
                        "This usually indicates a cyclic type or bug in the type checker.",
                    .{ self.location, self.count },
                );
            }
        }
    }

    /// Returns the current iteration count (useful for debugging).
    pub fn getCount(self: *const Self) u32 {
        return self.count;
    }
};

test "IterationGuard does not panic for normal iteration counts" {
    var guard = IterationGuard.init("test");
    var i: u32 = 0;
    while (i < 1000) : (i += 1) {
        guard.tick();
    }
    // In release builds, tick() is a no-op so count stays at 0.
    // In debug builds, count should be 1000.
    const expected: u32 = if (builtin.mode == .Debug) 1000 else 0;
    try std.testing.expectEqual(expected, guard.getCount());
}
