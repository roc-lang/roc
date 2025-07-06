//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

data_1: u32,
data_2: u32,
data_3: u32,
tag: Tag,

/// A list of nodes.
pub const List = collections.SafeMultiList(@This());

/// Internal representation for where a node is stored
/// in the tree.
pub const Idx = List.Idx;

/// This is the tag associated with a raw Node in the list
pub const Tag = enum {
    // Statements
    statement_decl,
    statement_var,
    statement_reassign,
    statement_crash,
    statement_expr,
    statement_expect,
    statement_for,
    statement_return,
    statement_import,
    statement_alias_decl,
    statement_nominal_decl,
    statement_type_anno,
    // Expressions
    expr_var,
    expr_tuple,
    expr_list,
    expr_empty_list,
    expr_call,
    expr_record,
    expr_empty_record,
    record_field,
    record_destruct,
    expr_field_access,
    expr_static_dispatch,
    expr_external_lookup,
    expr_dot_access,
    expr_apply,
    expr_string,
    expr_string_segment,
    expr_num,
    expr_int,
    expr_frac_f64,
    expr_frac_dec,
    expr_dec_small,
    expr_tag,
    expr_zero_argument_tag,
    expr_lambda,
    expr_record_update,
    expr_bin_op,
    expr_unary,
    expr_suffix_single_question,
    expr_if_then_else,
    expr_match,
    expr_dbg,
    expr_block,
    expr_ellipsis,
    expr_record_builder,
    match_branch,
    match_branch_pattern,
    where_clause,
    type_header,
    annotation,
    // Type Annotation
    ty_apply,
    ty_var,
    ty_ident,
    ty_underscore,
    ty_tag_union,
    ty_tuple,
    ty_record,
    ty_record_field,
    ty_fn,
    ty_parens,
    ty_lookup_external,
    ty_malformed,
    // Patterns
    pattern_identifier,
    pattern_as,
    pattern_applied_tag,
    pattern_record_destructure,
    pattern_list,
    pattern_tuple,
    pattern_num_literal,
    pattern_int_literal,
    pattern_dec_literal,
    pattern_f64_literal,
    pattern_small_dec_literal,
    pattern_str_literal,
    pattern_char_literal,
    pattern_underscore,
    // Definitions
    def,
    // Exposed Items
    exposed_item,

    // todo -- put me somewhere and rename maybe
    if_branch,

    // used to represent an extra node, solely for type-checking purposes
    type_var_slot,

    // Runtime Error Node
    //
    // Malformed nodes represent runtime errors in the IR following the "Inform Don't Block" principle.
    // They allow compilation to continue while preserving error information. When encountered during
    // execution, they will crash with the associated diagnostic.
    malformed,

    // Diagnostic Nodes
    //
    // Diagnostic nodes store error information separately from the main IR. They contain details
    // about compilation errors and are referenced by malformed nodes. These nodes are never
    // directly converted to IR - they exist only for error reporting and are accessed via
    // diagnostic indices stored in malformed nodes.
    diag_not_implemented,
    diag_invalid_num_literal,
    diag_invalid_single_quote,
    diag_too_long_single_quote,
    diag_empty_single_quote,
    diag_empty_tuple,
    diag_ident_already_in_scope,
    diag_ident_not_in_scope,
    diag_invalid_top_level_statement,
    diag_expr_not_canonicalized,
    diag_invalid_string_interpolation,
    diag_pattern_arg_invalid,
    diag_pattern_not_canonicalized,
    diag_can_lambda_not_implemented,
    diag_lambda_body_not_canonicalized,
    diag_if_condition_not_canonicalized,
    diag_if_then_not_canonicalized,
    diag_if_else_not_canonicalized,
    diag_malformed_type_annotation,
    diag_var_across_function_boundary,
    diag_shadowing_warning,
    diag_type_redeclared,
    diag_undeclared_type,
    diag_undeclared_type_var,
    diag_type_alias_redeclared,
    diag_tuple_elem_not_canonicalized,
    diag_nominal_type_redeclared,
    diag_type_shadowed_warning,
    diag_type_parameter_conflict,
    diag_unused_variable,
    diag_used_underscore_variable,
    diag_duplicate_record_field,
    diag_f64_pattern_literal,
};
