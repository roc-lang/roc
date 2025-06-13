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
region: base.Region,
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
    statement_crash,
    statement_expr,
    statement_expect,
    statement_for,
    statement_return,
    statement_import,
    statement_type_decl,
    statement_type_anno,
    // Expressions
    expr_var,
    expr_tuple,
    expr_list,
    expr_call,
    expr_record,
    expr_field_access,
    expr_static_dispatch,
    expr_apply,
    expr_string,
    expr_string_segment,
    expr_int,
    expr_float,
    expr_tag,
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
    // Type Header
    type_decl_header,
    // Type Annotation
    type_anno_apply,
    type_anno_var,
    type_anno_ty,
    type_anno_underscore,
    type_anno_mod_ty,
    type_anno_union,
    type_anno_tuple,
    type_anno_record,
    type_anno_fn,
    type_anno_parens,
    // Patterns
    pattern_identifier,
    pattern_as,
    pattern_applied_tag,
    pattern_record_destructure,
    pattern_list,
    pattern_num_literal,
    pattern_int_literal,
    pattern_float_literal,
    pattern_str_literal,
    pattern_char_literal,
    pattern_underscore,
    // Definitions
    def,

    // todo -- put me somewhere and rename maybe
    if_branch,

    // Runtime Error
    malformed,
};
