//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const std = @import("std");
const collections = @import("collections");

data_1: u32,
data_2: u32,
data_3: u32,
tag: Tag,

/// A list of nodes.
pub const List = collections.SafeMultiList(@This());

/// Get the payload as a typed extern union for type-safe access to node data.
/// This reinterprets data_1/data_2/data_3 as a Payload union without copying.
pub fn getPayload(self: *const @This()) Payload {
    return @as(*const Payload, @ptrCast(&self.data_1)).*;
}

/// Set the payload from a typed union value.
/// This writes the payload data to data_1/data_2/data_3.
pub fn setPayload(self: *@This(), p: Payload) void {
    const raw = @as(*const [3]u32, @ptrCast(&p)).*;
    self.data_1 = raw[0];
    self.data_2 = raw[1];
    self.data_3 = raw[2];
}

/// Internal representation for where a node is stored
/// in the tree.
pub const Idx = List.Idx;

/// This is the tag associated with a raw Node in the list
pub const Tag = enum {
    // Statements
    statement_decl,
    statement_decl_gen,
    statement_var,
    statement_reassign,
    statement_crash,
    statement_dbg,
    statement_expr,
    statement_expect,
    statement_for,
    statement_while,
    statement_break,
    statement_return,
    statement_import,
    statement_alias_decl,
    statement_nominal_decl,
    statement_type_anno,
    statement_type_var_alias,
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
    expr_required_lookup,
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
    expr_typed_int,
    expr_typed_frac,
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
    expr_anno_only,
    expr_hosted_lambda,
    expr_low_level,
    expr_expect,
    expr_for,
    expr_record_builder,
    expr_return,
    expr_type_var_dispatch,
    match_branch,
    match_branch_pattern,
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
    // Where clause
    where_method,
    where_alias,
    where_malformed,
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
    diag_empty_single_quote,
    diag_empty_tuple,
    diag_ident_already_in_scope,
    diag_ident_not_in_scope,
    diag_self_referential_definition,
    diag_qualified_ident_does_not_exist,
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
    diag_type_from_missing_module,
    diag_module_not_imported,
    diag_nested_type_not_found,
    diag_nested_value_not_found,
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
    diag_type_var_starting_with_dollar,
    diag_underscore_in_type_declaration,
    diagnostic_exposed_but_not_implemented,
    diag_redundant_exposed,
    diag_if_expr_without_else,
    diag_break_outside_loop,
    diag_mutually_recursive_type_aliases,
    diag_deprecated_number_suffix,
};

/// Typed payload union for accessing node data in a type-safe manner.
/// This is an extern union that overlays the data_1/data_2/data_3 fields (12 bytes = 3 × u32).
/// Each variant corresponds to a Node.Tag and provides semantic field names.
///
/// IMPORTANT: This must be an extern union to ensure consistent size across debug/release builds.
/// All variants must be exactly 12 bytes (3 × u32).
pub const Payload = extern union {
    /// Raw access to data fields (for backward compatibility during migration)
    raw: Raw,

    // === Statement payloads ===
    statement_decl: StatementDecl,
    statement_var: StatementVar,
    statement_crash: StatementCrash,
    statement_return: StatementReturn,
    statement_import: StatementImport,

    // === Expression payloads ===
    expr_var: ExprVar,
    expr_tuple: ExprTuple,
    expr_list: ExprList,
    expr_call: ExprCall,
    expr_record: ExprRecord,
    expr_tag: ExprTag,
    expr_closure: ExprClosure,
    expr_lambda: ExprLambda,
    expr_bin_op: ExprBinOp,
    expr_unary: ExprUnary,
    expr_block: ExprBlock,
    expr_if_then_else: ExprIfThenElse,
    expr_match: ExprMatch,
    expr_frac_f32: ExprFracF32,
    expr_frac_f64: ExprFracF64,
    expr_int: ExprInt,
    expr_num: ExprNum,
    expr_dec: ExprDec,
    expr_string: ExprString,
    expr_dot_access: ExprDotAccess,
    expr_field_access: ExprFieldAccess,
    expr_hosted_lambda: ExprHostedLambda,
    expr_low_level: ExprLowLevel,

    // === Pattern payloads ===
    pattern_identifier: PatternIdentifier,
    pattern_as: PatternAs,
    pattern_applied_tag: PatternAppliedTag,
    pattern_record_destructure: PatternRecordDestructure,
    pattern_list: PatternList,
    pattern_tuple: PatternTuple,
    pattern_num_literal: PatternNumLiteral,

    // === Type annotation payloads ===
    ty_apply: TyApply,
    ty_tag_union: TyTagUnion,
    ty_tag: TyTag,
    ty_tuple: TyTuple,
    ty_record: TyRecord,
    ty_fn: TyFn,
    ty_lookup: TyLookup,

    // === Other payloads ===
    record_field: RecordField,
    record_destruct: RecordDestruct,
    match_branch: MatchBranch,
    where_clause: WhereClause,
    def: Def,
    lambda_capture: LambdaCapture,
    annotation: Annotation,
    diagnostic: Diagnostic,

    // ============================================================
    // Payload struct definitions - all must be exactly 12 bytes
    // ============================================================

    pub const Raw = extern struct {
        data_1: u32,
        data_2: u32,
        data_3: u32,
    };

    // --- Statements ---

    pub const StatementDecl = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementVar = extern struct {
        ident: u32,
        expr: u32,
        _unused: u32,
    };

    pub const StatementCrash = extern struct {
        msg_expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementReturn = extern struct {
        expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementImport = extern struct {
        module_idx: u32,
        qualifier: u32,
        exposes_start: u32,
    };

    // --- Expressions ---

    pub const ExprVar = extern struct {
        ident: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprTuple = extern struct {
        elems_start: u32,
        elems_len: u32,
        _unused: u32,
    };

    pub const ExprList = extern struct {
        elems_start: u32,
        elems_len: u32,
        _unused: u32,
    };

    pub const ExprCall = extern struct {
        func: u32,
        extra_data_idx: u32,
        called_via: u32,
    };

    pub const ExprRecord = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprTag = extern struct {
        name: u32,
        args_start: u32,
        args_len: u32,
    };

    pub const ExprClosure = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprLambda = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprBinOp = extern struct {
        op: u32,
        lhs: u32,
        rhs: u32,
    };

    pub const ExprUnary = extern struct {
        expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprBlock = extern struct {
        stmts_start: u32,
        stmts_len: u32,
        final_expr: u32,
    };

    pub const ExprIfThenElse = extern struct {
        extra_data_idx: u32,
        extra_data_end: u32,
        _unused: u32,
    };

    pub const ExprMatch = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprFracF32 = extern struct {
        value: u32,
        has_suffix: u32,
        _unused: u32,
    };

    pub const ExprFracF64 = extern struct {
        value_lo: u32,
        value_hi: u32,
        has_suffix: u32,
    };

    pub const ExprInt = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprNum = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprDec = extern struct {
        extra_data_idx: u32,
        has_suffix: u32,
        _unused: u32,
    };

    pub const ExprString = extern struct {
        segments_start: u32,
        segments_len: u32,
        _unused: u32,
    };

    pub const ExprDotAccess = extern struct {
        receiver: u32,
        field_name: u32,
        extra_data_idx: u32,
    };

    pub const ExprFieldAccess = extern struct {
        receiver: u32,
        field_name: u32,
        _unused: u32,
    };

    pub const ExprHostedLambda = extern struct {
        symbol_name: u32,
        index: u32,
        extra_data_idx: u32,
    };

    pub const ExprLowLevel = extern struct {
        op: u32,
        extra_data_idx: u32,
        _unused: u32,
    };

    // --- Patterns ---

    pub const PatternIdentifier = extern struct {
        ident: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternAs = extern struct {
        ident: u32,
        pattern: u32,
        _unused: u32,
    };

    pub const PatternAppliedTag = extern struct {
        args_start: u32,
        args_len: u32,
        name: u32,
    };

    pub const PatternRecordDestructure = extern struct {
        destructs_start: u32,
        destructs_len: u32,
        _unused: u32,
    };

    pub const PatternList = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternTuple = extern struct {
        patterns_start: u32,
        patterns_len: u32,
        _unused: u32,
    };

    pub const PatternNumLiteral = extern struct {
        kind: u32,
        value_kind: u32,
        extra_data_idx: u32,
    };

    // --- Type annotations ---

    pub const TyApply = extern struct {
        name: u32,
        extra_data_idx: u32,
        _unused: u32,
    };

    pub const TyTagUnion = extern struct {
        tags_start: u32,
        tags_len: u32,
        ext_plus_one: u32,
    };

    pub const TyTag = extern struct {
        name: u32,
        args_start: u32,
        args_len: u32,
    };

    pub const TyTuple = extern struct {
        elems_start: u32,
        elems_len: u32,
        _unused: u32,
    };

    pub const TyRecord = extern struct {
        fields_start: u32,
        fields_len: u32,
        ext_plus_one: u32,
    };

    pub const TyFn = extern struct {
        args_start: u32,
        args_len: u32,
        extra_data_idx: u32,
    };

    pub const TyLookup = extern struct {
        name: u32,
        base: u32,
        _unused: u32,
    };

    // --- Other ---

    pub const RecordField = extern struct {
        name: u32,
        expr: u32,
        _unused: u32,
    };

    pub const RecordDestruct = extern struct {
        label: u32,
        ident: u32,
        extra_data_idx: u32,
    };

    pub const MatchBranch = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const WhereClause = extern struct {
        var_idx: u32,
        name: u32,
        extra_data_idx: u32,
    };

    pub const Def = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const LambdaCapture = extern struct {
        name: u32,
        scope_depth: u32,
        pattern_idx: u32,
    };

    pub const Annotation = extern struct {
        ident: u32,
        type_anno: u32,
        has_where: u32,
    };

    /// Diagnostic payload - used for all diagnostic node types.
    /// Fields are interpreted based on the specific diagnostic tag.
    pub const Diagnostic = extern struct {
        /// Primary value (ident, feature ID, count, etc.)
        primary: u32,
        /// Secondary value (ident, region start, etc.)
        secondary: u32,
        /// Tertiary value (region end, extra data index, etc.)
        tertiary: u32,
    };

    // Compile-time size verification
    comptime {
        std.debug.assert(@sizeOf(Payload) == 12);
    }
};
