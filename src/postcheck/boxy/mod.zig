//! Boxy post-check planning.
//!
//! This module consumes checked artifacts directly. It does not depend on
//! Monotype, Lambda Solved, Lambda Mono, lambda sets, or backend details.

pub const Plan = @import("plan.zig");
pub const Layouts = @import("layouts.zig");
pub const Lower = @import("lower.zig");

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
