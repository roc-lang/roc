//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const std = @import("std");
const collections = @import("collections");

/// Typed payload - 12 bytes accessed via semantic field names per node type.
payload: Payload,
tag: Tag,

/// A list of nodes.
pub const List = collections.SafeMultiList(@This());

/// Create a new Node with the given tag and zeroed payload.
pub fn init(tag: Tag) @This() {
    return .{ .tag = tag, .payload = std.mem.zeroes(Payload) };
}

/// Get the payload for type-safe access to node data.
pub fn getPayload(self: *const @This()) Payload {
    return self.payload;
}

/// Set the payload from a typed union value.
pub fn setPayload(self: *@This(), p: Payload) void {
    self.payload = p;
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
/// This is an extern union of exactly 12 bytes (3 × u32).
/// Each variant corresponds to a Node.Tag and provides semantic field names.
///
/// IMPORTANT: This must be an extern union to ensure consistent size across debug/release builds.
/// All variants must be exactly 12 bytes (3 × u32).
pub const Payload = extern union {
    // === Statement payloads ===
    statement_decl: StatementDecl,
    statement_var: StatementVar,
    statement_reassign: StatementReassign,
    statement_crash: StatementCrash,
    statement_single_expr: StatementSingleExpr,
    statement_for: StatementFor,
    statement_while: StatementWhile,
    statement_return: StatementReturn,
    statement_import: StatementImport,
    statement_alias_decl: StatementAliasDecl,
    statement_nominal_decl: StatementNominalDecl,
    statement_type_anno: StatementTypeAnno,
    statement_type_var_alias: StatementTypeVarAlias,

    // === Expression payloads ===
    expr_var: ExprVar,
    expr_external_lookup: ExprExternalLookup,
    expr_required_lookup: ExprRequiredLookup,
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
    expr_dec_small: ExprDecSmall,
    expr_string: ExprString,
    expr_dot_access: ExprDotAccess,
    expr_field_access: ExprFieldAccess,
    expr_hosted_lambda: ExprHostedLambda,
    expr_low_level: ExprLowLevel,
    expr_zero_argument_tag: ExprZeroArgumentTag,
    expr_for: ExprFor,
    expr_expect: ExprExpect,
    expr_typed_int: ExprTypedInt,
    expr_typed_frac: ExprTypedFrac,
    expr_string_segment: ExprStringSegment,
    expr_nominal: ExprNominal,
    expr_nominal_external: ExprNominalExternal,
    expr_crash: ExprCrash,
    expr_dbg: ExprDbg,
    expr_anno_only: ExprAnnoOnly,
    expr_return: ExprReturn,
    expr_type_var_dispatch: ExprTypeVarDispatch,

    // === Pattern payloads ===
    pattern_identifier: PatternIdentifier,
    pattern_as: PatternAs,
    pattern_applied_tag: PatternAppliedTag,
    pattern_record_destructure: PatternRecordDestructure,
    pattern_list: PatternList,
    pattern_tuple: PatternTuple,
    pattern_num_literal: PatternNumLiteral,
    pattern_nominal: PatternNominal,
    pattern_nominal_external: PatternNominalExternal,
    pattern_small_dec_literal: PatternSmallDecLiteral,
    pattern_dec_literal: PatternDecLiteral,
    pattern_str_literal: PatternStrLiteral,
    pattern_frac_f32: PatternFracF32,
    pattern_frac_f64: PatternFracF64,
    pattern_malformed: PatternMalformed,

    // === Type annotation payloads ===
    ty_apply: TyApply,
    ty_tag_union: TyTagUnion,
    ty_tag: TyTag,
    ty_tuple: TyTuple,
    ty_record: TyRecord,
    ty_fn: TyFn,
    ty_lookup: TyLookup,
    ty_rigid_var: TyRigidVar,
    ty_rigid_var_lookup: TyRigidVarLookup,
    ty_parens: TyParens,
    ty_malformed: TyMalformed,

    // === Other payloads ===
    record_field: RecordField,
    record_destruct: RecordDestruct,
    match_branch: MatchBranch,
    match_branch_pattern: MatchBranchPattern,
    where_clause: WhereClause,
    where_alias: WhereAlias,
    where_malformed: WhereMalformed,
    def: Def,
    lambda_capture: LambdaCapture,
    annotation: Annotation,
    // === Diagnostic payloads (typed variants) ===
    diag_empty: DiagEmpty,
    diag_single_ident: DiagSingleIdent,
    diag_single_value: DiagSingleValue,
    diag_two_idents: DiagTwoIdents,
    diag_ident_with_region: DiagIdentWithRegion,
    diag_two_idents_extra: DiagTwoIdentsExtra,
    diag_single_ident_extra: DiagSingleIdentExtra,
    diag_two_enums: DiagTwoEnums,
    type_header: TypeHeader,
    ty_record_field: TyRecordField,
    exposed_item: ExposedItem,
    if_branch: IfBranch,
    type_var_slot: TypeVarSlot,

    // Payload struct definitions - all must be exactly 12 bytes

    /// statement_decl, statement_decl_gen: pattern + expr + extra_data for anno
    pub const StatementDecl = extern struct {
        pattern: u32,
        expr: u32,
        extra_data_idx: u32,
    };

    /// statement_var: pattern_idx + expr + extra_data for anno
    pub const StatementVar = extern struct {
        pattern_idx: u32,
        expr: u32,
        extra_data_idx: u32,
    };

    /// statement_reassign: pattern_idx + expr
    pub const StatementReassign = extern struct {
        pattern_idx: u32,
        expr: u32,
        _unused: u32,
    };

    /// statement_crash: msg expr
    pub const StatementCrash = extern struct {
        msg: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// statement_dbg, statement_expr, statement_expect: single expr/body
    pub const StatementSingleExpr = extern struct {
        expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// statement_for: patt + expr + body
    pub const StatementFor = extern struct {
        patt: u32,
        expr: u32,
        body: u32,
    };

    /// statement_while: cond + body
    pub const StatementWhile = extern struct {
        cond: u32,
        body: u32,
        _unused: u32,
    };

    /// statement_return: expr + optional lambda
    pub const StatementReturn = extern struct {
        expr: u32,
        lambda_plus_one: u32,
        _unused: u32,
    };

    /// statement_import: module_name_tok + extra_data_idx
    pub const StatementImport = extern struct {
        module_name_tok: u32,
        extra_data_idx: u32,
        _unused: u32,
    };

    /// statement_alias_decl: header + anno
    pub const StatementAliasDecl = extern struct {
        header: u32,
        anno: u32,
        _unused: u32,
    };

    /// statement_nominal_decl: header + anno + extra_data_idx
    pub const StatementNominalDecl = extern struct {
        header: u32,
        anno: u32,
        extra_data_idx: u32,
    };

    /// statement_type_anno: extra_data_idx contains all fields
    pub const StatementTypeAnno = extern struct {
        extra_data_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// statement_type_var_alias: alias_name + type_var_name + type_var_anno
    pub const StatementTypeVarAlias = extern struct {
        alias_name: u32,
        type_var_name: u32,
        type_var_anno: u32,
    };

    // --- Expressions ---

    /// expr_var: local variable lookup by pattern index
    pub const ExprVar = extern struct {
        pattern_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_external_lookup: lookup from another module
    pub const ExprExternalLookup = extern struct {
        module_idx: u32,
        target_node_idx: u32,
        ident_idx: u32,
    };

    /// expr_required_lookup: lookup from platform requires clause
    pub const ExprRequiredLookup = extern struct {
        requires_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_dec_small: small decimal value
    pub const ExprDecSmall = extern struct {
        numerator: u32,
        denom_power: u32,
        has_suffix: u32,
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
        args_span2_idx: u32,
        called_via: u32,
    };

    pub const ExprRecord = extern struct {
        fields_ext_idx: u32, // Index into span_with_node_data: (fields.start, fields.len, ext_value)
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprTag = extern struct {
        name: u32,
        args_start: u32,
        args_len: u32,
    };

    pub const ExprClosure = extern struct {
        closure_data_idx: u32, // Index into closure_data
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprLambda = extern struct {
        args_body_idx: u32, // Index into span_with_node_data: (args.start, args.len, body)
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
        branches_else_idx: u32, // Index into span_with_node_data: (branches.start, branches.len, final_else)
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprMatch = extern struct {
        match_data_idx: u32, // Index into match_data: (cond, branches_start, branches_len, exhaustive, is_try_suffix)
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

    /// expr_num: numeric literal with kind and value in int128_values
    pub const ExprNum = extern struct {
        kind: u32,
        val_kind: u32,
        int128_idx: u32,
    };

    /// expr_dec: decimal literal with value in int128_values
    pub const ExprDec = extern struct {
        int128_idx: u32,
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
        args_body_idx: u32, // Index into span_with_node_data: (args.start, args.len, body)
    };

    pub const ExprLowLevel = extern struct {
        op: u32,
        args_body_idx: u32, // Index into span_with_node_data: (args.start, args.len, body)
        _unused: u32,
    };

    pub const ExprZeroArgumentTag = extern struct {
        zero_arg_tag_idx: u32, // Index into zero_arg_tag_data
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprFor = extern struct {
        patt: u32,
        expr: u32,
        body: u32,
    };

    pub const ExprExpect = extern struct {
        body: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_typed_int: typed integer with type name and value in int128_values
    pub const ExprTypedInt = extern struct {
        type_name: u32,
        val_kind: u32,
        int128_idx: u32,
    };

    /// expr_typed_frac: typed fraction with type name and value in int128_values
    pub const ExprTypedFrac = extern struct {
        type_name: u32,
        val_kind: u32,
        int128_idx: u32,
    };

    /// expr_string_segment: string segment reference
    pub const ExprStringSegment = extern struct {
        segment_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_nominal: nominal type expression
    pub const ExprNominal = extern struct {
        nominal_type_decl: u32,
        backing_expr: u32,
        backing_type: u32,
    };

    /// expr_nominal_external: external nominal type
    pub const ExprNominalExternal = extern struct {
        module_idx: u32,
        target_node_idx: u32,
        backing_span2_idx: u32, // Index into span2_data: (backing_expr, backing_type)
    };

    /// expr_crash: crash expression with message
    pub const ExprCrash = extern struct {
        msg: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_dbg: debug expression
    pub const ExprDbg = extern struct {
        expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_anno_only: annotation-only expression
    pub const ExprAnnoOnly = extern struct {
        ident: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_return: return expression
    pub const ExprReturn = extern struct {
        expr: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// expr_type_var_dispatch: type variable method dispatch
    pub const ExprTypeVarDispatch = extern struct {
        type_var_alias_stmt: u32,
        method_name: u32,
        args_span2_idx: u32,
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

    /// pattern_num_literal: numeric pattern with value in int128_values
    pub const PatternNumLiteral = extern struct {
        kind: u32,
        value_kind: u32,
        int128_idx: u32,
    };

    pub const PatternNominal = extern struct {
        nominal_type_decl: u32,
        backing_pattern: u32,
        backing_type: u32,
    };

    pub const PatternNominalExternal = extern struct {
        module_idx: u32,
        target_node_idx: u32,
        backing_span2_idx: u32, // Index into span2_data: (backing_pattern, backing_type)
    };

    pub const PatternSmallDecLiteral = extern struct {
        numerator: u32,
        denominator_power: u32,
        has_suffix: u32,
    };

    /// pattern_dec_literal: decimal pattern with value in int128_values
    pub const PatternDecLiteral = extern struct {
        int128_idx: u32,
        has_suffix: u32,
        _unused: u32,
    };

    pub const PatternStrLiteral = extern struct {
        literal: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternFracF32 = extern struct {
        value: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternFracF64 = extern struct {
        value_lo: u32,
        value_hi: u32,
        _unused: u32,
    };

    pub const PatternMalformed = extern struct {
        diagnostic: u32,
        _unused1: u32,
        _unused2: u32,
    };

    // --- Type annotations ---

    pub const TyApply = extern struct {
        name: u32,
        args_start: u32,
        extra_data_idx: u32,
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
        extra_data_idx: u32,
    };

    pub const TyRigidVar = extern struct {
        name: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// ty_rigid_var_lookup: lookup reference to a rigid type variable
    pub const TyRigidVarLookup = extern struct {
        ref: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const TyParens = extern struct {
        anno: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const TyMalformed = extern struct {
        diagnostic: u32,
        _unused1: u32,
        _unused2: u32,
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
        match_branch_idx: u32, // Index into match_branch_data
        _unused1: u32,
        _unused2: u32,
    };

    pub const MatchBranchPattern = extern struct {
        pattern: u32,
        degenerate: u32,
        _unused: u32,
    };

    pub const WhereClause = extern struct {
        var_idx: u32,
        name: u32,
        args_ret_idx: u32, // Index into span_with_node_data: (args.start, args.len, ret)
    };

    pub const WhereMalformed = extern struct {
        diagnostic: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// where_alias: type variable alias in where clause
    pub const WhereAlias = extern struct {
        var_idx: u32,
        alias_name: u32,
        _unused: u32,
    };

    pub const Def = extern struct {
        def_data_idx: u32, // Index into def_data
        _unused1: u32,
        _unused2: u32,
    };

    pub const LambdaCapture = extern struct {
        name: u32,
        scope_depth: u32,
        pattern_idx: u32,
    };

    pub const Annotation = extern struct {
        anno: u32,
        has_where: u32,
        extra_data_idx: u32,
    };

    // === Diagnostic payload structs ===

    /// Diagnostics that only need region (stored separately), no payload data.
    /// Used by: diag_invalid_num_literal, diag_empty_tuple, diag_break_outside_loop, etc.
    pub const DiagEmpty = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    /// Diagnostics with a single identifier.
    /// Used by: diag_ident_not_in_scope, diag_unused_variable, diag_undeclared_type, etc.
    pub const DiagSingleIdent = extern struct {
        ident: u32, // @bitCast(Ident.Idx)
        _unused1: u32,
        _unused2: u32,
    };

    /// Diagnostics with a single u32 value (feature ID, count, bool, enum, etc.)
    /// Used by: diag_not_implemented, diag_too_many_exports, diag_default_app_wrong_arity, etc.
    pub const DiagSingleValue = extern struct {
        value: u32,
        _unused1: u32,
        _unused2: u32,
    };

    /// Diagnostics with two identifiers.
    /// Used by: diag_value_not_exposed, diag_type_name_case_mismatch, diag_nested_type_not_found, etc.
    pub const DiagTwoIdents = extern struct {
        ident1: u32, // @bitCast(Ident.Idx)
        ident2: u32, // @bitCast(Ident.Idx)
        _unused: u32,
    };

    /// Diagnostics with an identifier and inline region offsets.
    /// Used by: diag_shadowing_warning, diag_type_redeclared, diag_duplicate_record_field, etc.
    pub const DiagIdentWithRegion = extern struct {
        ident: u32,        // @bitCast(Ident.Idx)
        region_start: u32, // offset
        region_end: u32,   // offset
    };

    /// Diagnostics with two values plus an extra_data index for region.
    /// Used by: diag_type_shadowed_warning, diag_type_parameter_conflict, diag_mutually_recursive_type_aliases
    pub const DiagTwoIdentsExtra = extern struct {
        ident1: u32,    // @bitCast(Ident.Idx) or value
        ident2: u32,    // @bitCast(Ident.Idx) or bool flag
        extra_idx: u32, // index into extra_data for region offsets
    };

    /// Diagnostics with a single identifier plus extra_data index.
    /// Used by: diag_redundant_exposed
    pub const DiagSingleIdentExtra = extern struct {
        ident: u32,     // @bitCast(Ident.Idx)
        extra_idx: u32, // index into extra_data for region offsets
        _unused: u32,
    };

    /// Diagnostics with two enum values.
    /// Used by: diag_deprecated_number_suffix
    pub const DiagTwoEnums = extern struct {
        enum1: u32, // @intFromEnum
        enum2: u32, // @intFromEnum
        _unused: u32,
    };

    pub const TypeHeader = extern struct {
        name: u32,
        relative_name: u32,
        packed_args: u32,
    };

    pub const TyRecordField = extern struct {
        name: u32,
        ty: u32,
        _unused: u32,
    };

    pub const ExposedItem = extern struct {
        name: u32,
        alias: u32,
        is_wildcard: u32,
    };

    pub const IfBranch = extern struct {
        cond: u32,
        body: u32,
        _unused: u32,
    };

    pub const TypeVarSlot = extern struct {
        parent_node_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    // Compile-time size verification
    comptime {
        std.debug.assert(@sizeOf(Payload) == 12);
    }
};
