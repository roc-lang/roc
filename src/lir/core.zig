//! Core statement-only LIR data shared by lowering and LIR consumers.

const std = @import("std");

/// Core statement-only LIR type definitions.
pub const LIR = @import("LIR.zig");
/// Flat storage for statement-only LIR nodes and spans.
pub const LirStore = @import("LirStore.zig");
/// LIR-owned root metadata.
pub const RootMetadata = @import("root_metadata.zig");
/// Hosted ABI metadata carried by LIR proc specs.
pub const Hosted = @import("hosted.zig");
/// LIR program result shared by post-check lowering and consumers.
pub const Program = @import("program.zig");

test "lir core declarations are referenced" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirStore);
    std.testing.refAllDecls(RootMetadata);
    std.testing.refAllDecls(Hosted);
    std.testing.refAllDecls(Program);
}
