//! Core CIR pipeline stages that sit between canonical CIR and executable MIR.
//!
//! Phase order:
//! 1. CoreCIR normalization
//! 2. ContextMono exact source-level monotypes
//! 3. LambdaSolved solved lambda-set semantics
//! 4. LambdaSpecialize executable callable specialization
//! 5. SpecializedCIR -> MIR lowering

const std = @import("std");

pub const CoreCIR = @import("CoreCIR.zig");
pub const Monotype = @import("Monotype.zig");
pub const ContextMono = @import("ContextMono.zig");
pub const LambdaSolved = @import("LambdaSolved.zig");
pub const LambdaSpecialize = @import("LambdaSpecialize.zig");
pub const Pipeline = @import("Pipeline.zig");
pub const SpecializedCIR = @import("SpecializedCIR.zig");

test "corecir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(CoreCIR);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(ContextMono);
    std.testing.refAllDecls(LambdaSolved);
    std.testing.refAllDecls(LambdaSpecialize);
    std.testing.refAllDecls(Pipeline);
    std.testing.refAllDecls(SpecializedCIR);
}
