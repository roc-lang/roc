//! Boxy post-check planning.
//!
//! This module consumes checked checked_modules directly. It does not depend on
//! Monotype, Lambda Solved, Lambda Mono, lambda sets, or backend details.

pub const Plan = @import("plan.zig");
pub const Layouts = @import("layouts.zig");
pub const Lower = @import("lower.zig");

/// Shared checked-type fixtures for the Boxy stage tests.
pub const TestFixtures = @import("test_fixtures.zig");

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
