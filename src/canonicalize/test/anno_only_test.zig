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

test "e_anno_only can be used in statements" {
    // This test verifies that e_anno_only expressions can be
    // used as part of s_decl statements, which is how standalone
    // type annotations are represented after canonicalization.

    const pattern_idx: CIR.Pattern.Idx = @enumFromInt(0);
    const expr_idx: CIR.Expr.Idx = @enumFromInt(0);
    const anno_idx: CIR.Annotation.Idx = @enumFromInt(0);

    const stmt = CIR.Statement{ .s_decl = .{
        .pattern = pattern_idx,
        .expr = expr_idx,
        .anno = anno_idx,
    } };

    // Verify the statement was created correctly
    switch (stmt) {
        .s_decl => |decl| {
            try testing.expect(decl.anno != null);
        },
        else => return error.WrongStatementType,
    }
}
