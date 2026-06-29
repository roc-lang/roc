//! Boxy post-check planning.
//!
//! This module consumes checked artifacts directly. It does not depend on
//! Monotype, Lambda Solved, Lambda Mono, lambda sets, or backend details.

pub const Plan = @import("plan.zig");

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
