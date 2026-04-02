//! Core CIR pipeline stages that sit between canonical CIR and executable MIR.
//!
//! Phase order:
//! 1. CoreCIR normalization
//! 2. TemplateCatalog callable template discovery
//! 3. ContextMono exact source-level monotypes
//! 4. DispatchSolved exact static dispatch
//! 5. Lambdasolved solved lambda-set semantics
//! 6. Lambdamono executable callable specialization
//! 7. Lambdamono -> MIR lowering

const std = @import("std");

pub const CoreCIR = @import("CoreCIR.zig");
pub const Monotype = @import("Monotype.zig");
pub const TemplateCatalog = @import("TemplateCatalog.zig");
pub const ContextMono = @import("ContextMono.zig");
pub const DispatchSolved = @import("DispatchSolved.zig");
pub const Lambdasolved = @import("Lambdasolved.zig");
pub const Lambdamono = @import("Lambdamono.zig");
pub const Pipeline = @import("Pipeline.zig");

test "corecir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(CoreCIR);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(TemplateCatalog);
    std.testing.refAllDecls(ContextMono);
    std.testing.refAllDecls(DispatchSolved);
    std.testing.refAllDecls(Lambdasolved);
    std.testing.refAllDecls(Lambdamono);
    std.testing.refAllDecls(Pipeline);
}
