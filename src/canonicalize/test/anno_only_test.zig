//! Tests for standalone type annotation canonicalization.
//!
//! This module contains unit tests that verify the e_anno_only expression variant
//! works correctly in the compiler's canonical internal representation (CIR).

const CIR = @import("../CIR.zig");
const base = @import("base");

test "e_anno_only expression variant exists" {
    // Create an e_anno_only expression with a dummy identifier
    const test_ident = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = 0,
    };
    const expr = CIR.Expr{ .e_anno_only = .{ .ident = test_ident } };

    // Verify it's the correct variant
    switch (expr) {
        .e_anno_only => {},
        else => return error.WrongExprVariant,
    }
}
