//! Hosted-procedure metadata shared across compiler stages.

const Ident = @import("Ident.zig");

/// Public struct `HostedProc`.
pub const HostedProc = struct {
    symbol_name: Ident.Idx,
    index: u32,
};
