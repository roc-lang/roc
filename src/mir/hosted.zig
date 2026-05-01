//! Hosted procedure metadata after checked artifact publication.
//!
//! Hosted procedures are discovered and ordered during checked artifact
//! publication. Post-check stages must carry this explicit metadata; they must
//! not recover host symbol names from source syntax or module-local identifiers.

const check = @import("check");

const canonical = check.CanonicalNames;

/// Canonical-name store type used by hosted metadata ids.
pub const CanonicalNameStore = canonical.CanonicalNameStore;

/// Explicit lowering-time identity for a platform-hosted procedure.
pub const Proc = struct {
    /// External host symbol name in the lowering-run canonical-name store.
    external_symbol_name: canonical.ExternalSymbolNameId,
    /// Stable index into the platform-provided hosted-function table.
    dispatch_index: u32,
};

test "hosted metadata declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
