//! MIR-family post-check lowering pipeline.
//!
//! `design.md` is the source of truth for these type-state boundaries. The
//! submodules here implement the post-check MIR-family architecture.

const std = @import("std");

pub const Mono = @import("mono/mod.zig");
pub const MonoRow = @import("mono_row/mod.zig");
pub const Lifted = @import("lifted/mod.zig");
pub const LambdaSolved = @import("lambda_solved/mod.zig");
pub const Executable = @import("executable/mod.zig");
pub const Ids = @import("ids.zig");
pub const DebugVerify = @import("debug_verify.zig");
pub const ConcreteSourceType = @import("concrete_source_type.zig");
pub const ArtifactNames = @import("artifact_names.zig");
pub const Hosted = @import("hosted.zig");
pub const StructuralTest = @import("structural_test.zig");

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Mono);
    std.testing.refAllDecls(MonoRow);
    std.testing.refAllDecls(Lifted);
    std.testing.refAllDecls(LambdaSolved);
    std.testing.refAllDecls(Executable);
    std.testing.refAllDecls(Ids);
    std.testing.refAllDecls(DebugVerify);
    std.testing.refAllDecls(ConcreteSourceType);
    std.testing.refAllDecls(ArtifactNames);
    std.testing.refAllDecls(Hosted);
    std.testing.refAllDecls(StructuralTest);
}
