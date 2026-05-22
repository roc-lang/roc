//! Hosted procedure metadata owned by the LIR boundary.
//!
//! Hosted procedures are discovered during checking. Post-check lowering carries
//! the checked hosted id and exported symbol into LIR instead of reaching
//! through the old middle namespace.

const check = @import("check");

const names = check.CheckedNames;

pub const Proc = struct {
    external_symbol_name: names.ExternalSymbolNameId,
    dispatch_index: u32,
};

test "hosted declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
