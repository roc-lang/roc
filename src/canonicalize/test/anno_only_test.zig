//! Tests for standalone type annotation canonicalization.
//!
//! This module contains unit tests that verify the e_anno_only expression variant
//! works correctly in the compiler's canonical internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const CIR = @import("../CIR.zig");

test "e_anno_only expression variant exists" {
    // Create an e_anno_only expression
    const expr = CIR.Expr{ .e_anno_only = .{} };

    // Verify it's the correct variant
    switch (expr) {
        .e_anno_only => {},
        else => return error.WrongExprVariant,
    }
}
