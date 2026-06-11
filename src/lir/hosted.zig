//! Hosted procedure metadata owned by the LIR boundary.
//!
//! Hosted procedures are discovered during checking. Post-check lowering carries
//! the checked hosted id and exported symbol into LIR.

const base = @import("base");

/// Hosted procedure entry recorded at the LIR boundary.
pub const Proc = struct {
    /// The hosted function's linker symbol, interned in the LirStore string
    /// table so backends can emit direct symbol references.
    symbol: base.StringLiteral.Idx,
    dispatch_index: u32,
};

test "hosted declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
