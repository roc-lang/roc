//! Core CIR pipeline stages that sit between canonical CIR and executable MIR.
//!
//! Phase order:
//! 1. CoreCIR normalization
//! 2. ContextMono exact source-level monotypes
//! 3. Lambdasolved solved lambda-set semantics
//! 4. Lambdamono executable callable specialization
//! 5. Lambdamono -> MIR lowering

const std = @import("std");

pub const CoreCIR = @import("CoreCIR.zig");
pub const Monotype = @import("Monotype.zig");
pub const ContextMono = @import("ContextMono.zig");
pub const Lambdasolved = @import("Lambdasolved.zig");
pub const Lambdamono = @import("Lambdamono.zig");
pub const Pipeline = @import("Pipeline.zig");

test "corecir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(CoreCIR);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(ContextMono);
    std.testing.refAllDecls(Lambdasolved);
    std.testing.refAllDecls(Lambdamono);
    std.testing.refAllDecls(Pipeline);
}
