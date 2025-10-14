//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

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
    statement_dbg,
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
    expr_frac_f32,
    expr_frac_f64,
    expr_dec,
    expr_dec_small,
    expr_tag,
    expr_nominal,
    expr_nominal_external,
    expr_zero_argument_tag,
    expr_closure,
    expr_lambda,
    expr_record_update,
    expr_bin_op,
    expr_unary_minus,
    expr_unary_not,
    expr_suffix_single_question,
    expr_if_then_else,
    expr_match,
    expr_dbg,
    expr_crash,
    expr_block,
    expr_ellipsis,
    expr_expect,
    expr_record_builder,
    match_branch,
    match_branch_pattern,
    where_clause,
    type_header,
    annotation,
    // Type Annotation
    ty_apply,
    ty_apply_external,
    ty_rigid_var,
    ty_rigid_var_lookup,
    ty_lookup,
    ty_underscore,
    ty_tag_union,
    ty_tag,
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
    pattern_nominal,
    pattern_nominal_external,
    pattern_record_destructure,
    pattern_list,
    pattern_tuple,
    pattern_num_literal,
    pattern_dec_literal,
    pattern_f32_literal,
    pattern_f64_literal,
    pattern_small_dec_literal,
    pattern_str_literal,
    pattern_underscore,

    // Lambda Capture
    lambda_capture,

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
    diag_malformed_where_clause,
    diag_where_clause_not_allowed_in_type_decl,
    diag_type_module_missing_matching_type,
    diag_default_app_missing_main,
    diag_default_app_wrong_arity,
    diag_cannot_import_default_app,
    diag_execution_requires_app_or_default_app,
    diag_type_name_case_mismatch,
    diag_module_header_deprecated,
    diag_redundant_expose_main_type,
    diag_invalid_main_type_rename_in_exposing,
    diag_var_across_function_boundary,
    diag_shadowing_warning,
    diag_type_redeclared,
    diag_undeclared_type,
    diag_undeclared_type_var,
    diag_type_alias_but_needed_nominal,
    diag_type_alias_redeclared,
    diag_tuple_elem_not_canonicalized,
    diag_module_not_found,
    diag_value_not_exposed,
    diag_type_not_exposed,
    diag_module_not_imported,
    diag_too_many_exports,
    diag_nominal_type_redeclared,
    diag_type_shadowed_warning,
    diag_type_parameter_conflict,
    diag_unused_variable,
    diag_used_underscore_variable,
    diag_duplicate_record_field,
    diag_crash_expects_string,
    diag_f64_pattern_literal,
    diag_unused_type_var_name,
    diag_type_var_marked_unused,
    diag_type_var_ending_in_underscore,
    diag_underscore_in_type_declaration,
    diagnostic_exposed_but_not_implemented,
    diag_redundant_exposed,
};
