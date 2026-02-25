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
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_typed_int,
        .e_typed_frac,
        .e_str_segment,
        .e_str,
        .e_lookup_local,
        .e_lookup_external,
        .e_lookup_pending,
        .e_lookup_required,
        .e_list,
        .e_empty_list,
        .e_tuple,
        .e_match,
        .e_if,
        .e_call,
        .e_record,
        .e_empty_record,
        .e_block,
        .e_tag,
        .e_nominal,
        .e_nominal_external,
        .e_zero_argument_tag,
        .e_closure,
        .e_lambda,
        .e_binop,
        .e_unary_minus,
        .e_unary_not,
        .e_dot_access,
        .e_tuple_access,
        .e_runtime_error,
        .e_crash,
        .e_dbg,
        .e_expect,
        .e_ellipsis,
        .e_return,
        .e_type_var_dispatch,
        .e_for,
        .e_hosted_lambda,
        .e_run_low_level,
        => return error.WrongExprVariant,
    }
}
