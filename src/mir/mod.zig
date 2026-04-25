//! MIR-family post-check lowering pipeline.
//!
//! `plan.md` is the source of truth for these type-state boundaries. The
//! submodules here are the implementation work areas for the hard cutover from
//! the old top-level post-check stages to the final MIR-family architecture.

const std = @import("std");

pub const Mono = @import("mono/mod.zig");
pub const MonoRow = @import("mono_row/mod.zig");
pub const Lifted = @import("lifted/mod.zig");
pub const LambdaSolved = @import("lambda_solved/mod.zig");
pub const Executable = @import("executable/mod.zig");
pub const Ids = @import("ids.zig");
pub const DebugVerify = @import("debug_verify.zig");

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Mono);
    std.testing.refAllDecls(MonoRow);
    std.testing.refAllDecls(Lifted);
    std.testing.refAllDecls(LambdaSolved);
    std.testing.refAllDecls(Executable);
    std.testing.refAllDecls(Ids);
    std.testing.refAllDecls(DebugVerify);
}
