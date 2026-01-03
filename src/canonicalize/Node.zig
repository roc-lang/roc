//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how the payload fields should be interpreted.

const std = @import("std");
const collections = @import("collections");

const Node = @This();

/// Legacy field access - maps to payload.raw.data_1
/// TODO: Remove once all code is migrated to use payload union fields
data_1: u32,
/// Legacy field access - maps to payload.raw.data_2
data_2: u32,
/// Legacy field access - maps to payload.raw.data_3
data_3: u32,
tag: Tag,

/// Access the data as a typed payload union.
/// Use this to access named fields for specific node types.
pub fn getPayload(self: *const Node) Payload {
    return @as(*const Payload, @ptrCast(&self.data_1)).*;
}

/// Set the data from a typed payload union.
pub fn setPayload(self: *Node, p: Payload) void {
    const raw = @as(*const [3]u32, @ptrCast(&p)).*;
    self.data_1 = raw[0];
    self.data_2 = raw[1];
    self.data_3 = raw[2];
}

/// A list of nodes.
pub const List = collections.SafeMultiList(Node);

/// Internal representation for where a node is stored
/// in the tree.
pub const Idx = List.Idx;

/// The payload is an extern union that fits in exactly 12 bytes (3 x u32).
/// Each variant is a packed struct that encodes the data for that node type.
/// The tag field determines which variant is active.
pub const Payload = extern union {
    // Generic access to raw u32 fields (for gradual migration)
    raw: Raw,

    // Statements
    statement_decl: StatementDecl,
    statement_var: StatementVar,
    statement_reassign: StatementReassign,
    statement_crash: StatementCrash,
    statement_dbg: StatementDbg,
    statement_expr: StatementExpr,
    statement_expect: StatementExpect,
    statement_for: StatementFor,
    statement_while: StatementWhile,
    statement_break: StatementBreak,
    statement_return: StatementReturn,
    statement_import: StatementImport,
    statement_alias_decl: StatementAliasDecl,
    statement_nominal_decl: StatementNominalDecl,
    statement_type_anno: StatementTypeAnno,
    statement_type_var_alias: StatementTypeVarAlias,

    // Expressions
    expr_var: ExprVar,
    expr_tuple: ExprTuple,
    expr_list: ExprList,
    expr_empty_list: ExprEmpty,
    expr_call: ExprCall,
    expr_record: ExprRecord,
    expr_empty_record: ExprEmpty,
    record_field: RecordField,
    record_destruct: RecordDestruct,
    expr_external_lookup: ExprExternalLookup,
    expr_required_lookup: ExprRequiredLookup,
    expr_dot_access: ExprDotAccess,
    expr_string: ExprString,
    expr_string_segment: ExprStringSegment,
    expr_num: ExprNum,
    expr_frac_f32: ExprFracF32,
    expr_frac_f64: ExprFracF64,
    expr_dec: ExprDec,
    expr_dec_small: ExprDecSmall,
    expr_tag: ExprTag,
    expr_nominal: ExprNominal,
    expr_nominal_external: ExprNominalExternal,
    expr_zero_argument_tag: ExprZeroArgumentTag,
    expr_closure: ExprClosure,
    expr_lambda: ExprLambda,
    expr_bin_op: ExprBinOp,
    expr_unary_minus: ExprUnaryOp,
    expr_unary_not: ExprUnaryOp,
    expr_if_then_else: ExprIfThenElse,
    expr_match: ExprMatch,
    expr_dbg: ExprDbg,
    expr_crash: ExprCrash,
    expr_block: ExprBlock,
    expr_ellipsis: ExprEmpty,
    expr_anno_only: ExprEmpty,
    expr_hosted_lambda: ExprHostedLambda,
    expr_low_level: ExprLowLevel,
    expr_expect: ExprExpect,
    expr_for: ExprFor,
    expr_return: ExprReturn,
    expr_type_var_dispatch: ExprTypeVarDispatch,
    match_branch: MatchBranch,
    match_branch_pattern: MatchBranchPattern,

    // Type Header and Annotation
    type_header: TypeHeader,
    annotation: Annotation,

    // Type Annotations
    ty_apply: TyApply,
    ty_apply_external: TyApplyExternal,
    ty_rigid_var: TyRigidVar,
    ty_lookup: TyLookup,
    ty_underscore: TyUnderscore,
    ty_tag_union: TyTagUnion,
    ty_tag: TyTag,
    ty_tuple: TyTuple,
    ty_record: TyRecord,
    ty_record_field: TyRecordField,
    ty_fn: TyFn,
    ty_parens: TyParens,
    ty_lookup_external: TyLookupExternal,
    ty_malformed: TyMalformed,

    // Where clauses
    where_method: WhereMethod,
    where_alias: WhereAlias,
    where_malformed: WhereMalformed,

    // Patterns
    pattern_identifier: PatternIdentifier,
    pattern_as: PatternAs,
    pattern_applied_tag: PatternAppliedTag,
    pattern_nominal: PatternNominal,
    pattern_nominal_external: PatternNominalExternal,
    pattern_record_destructure: PatternRecordDestructure,
    pattern_list: PatternList,
    pattern_tuple: PatternTuple,
    pattern_num_literal: PatternNumLiteral,
    pattern_dec_literal: PatternDecLiteral,
    pattern_f32_literal: PatternF32Literal,
    pattern_f64_literal: PatternF64Literal,
    pattern_small_dec_literal: PatternSmallDecLiteral,
    pattern_str_literal: PatternStrLiteral,
    pattern_underscore: PatternUnderscore,

    // Lambda Capture
    lambda_capture: LambdaCapture,

    // Definitions
    def: Def,

    // Exposed Items
    exposed_item: ExposedItem,

    // If branch
    if_branch: IfBranch,

    // Type var slot
    type_var_slot: TypeVarSlot,

    // Malformed (runtime error)
    malformed: Malformed,

    // Diagnostics - all share a common structure
    diagnostic: Diagnostic,

    // Raw access to the 3 u32 fields for compatibility
    pub const Raw = extern struct {
        data_1: u32,
        data_2: u32,
        data_3: u32,
    };

    // Statement payload types
    pub const StatementDecl = extern struct {
        pattern: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        /// 0 = no annotation, otherwise (annotation_idx + 1)
        anno_plus_one: u32,
    };

    pub const StatementVar = extern struct {
        pattern_idx: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        /// 0 = no annotation, otherwise (annotation_idx + 1)
        anno_plus_one: u32,
    };

    pub const StatementReassign = extern struct {
        pattern_idx: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        _unused: u32,
    };

    pub const StatementCrash = extern struct {
        msg: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementDbg = extern struct {
        expr: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementExpr = extern struct {
        expr: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementExpect = extern struct {
        body: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const StatementFor = extern struct {
        patt: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        body: u32, // CIR.Expr.Idx
    };

    pub const StatementWhile = extern struct {
        cond: u32, // CIR.Expr.Idx
        body: u32, // CIR.Expr.Idx
        _unused: u32,
    };

    pub const StatementBreak = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    pub const StatementReturn = extern struct {
        expr: u32, // CIR.Expr.Idx
        /// 0 = no lambda, otherwise (lambda_idx + 1)
        lambda_plus_one: u32,
        _unused: u32,
    };

    pub const StatementImport = extern struct {
        module_name_tok: u32, // Ident.Idx (bitcasted)
        alias_tok: u32, // Ident.Idx (bitcasted) or 0 for None
        packed_qualifier_and_exposes: packed struct(u32) {
            qualifier: u16, // Ident.Idx (low 16 bits) or 0 for None
            exposes_start: u11, // packed into remaining bits
            exposes_len: u5, // max 31, but max used is ~20
        },
    };

    pub const StatementAliasDecl = extern struct {
        header: u32, // CIR.TypeHeader.Idx
        anno: u32, // CIR.TypeAnno.Idx
        _unused: u32,
    };

    pub const StatementNominalDecl = extern struct {
        header: u32, // CIR.TypeHeader.Idx
        anno: u32, // CIR.TypeAnno.Idx
        is_opaque: u32, // 0 or 1
    };

    pub const StatementTypeAnno = extern struct {
        anno: u32, // CIR.TypeAnno.Idx
        name: u32, // Ident.Idx
        /// Packed: where_start (20 bits), where_len (11 bits), has_where (1 bit)
        packed_where: u32,
    };

    pub const StatementTypeVarAlias = extern struct {
        alias_name: u32, // Ident.Idx
        type_var_name: u32, // Ident.Idx
        type_var_anno: u32, // CIR.TypeAnno.Idx
    };

    // Expression payload types
    pub const ExprVar = extern struct {
        pattern_idx: u32, // CIR.Pattern.Idx
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

    pub const ExprEmpty = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    pub const ExprCall = extern struct {
        func: u32, // CIR.Expr.Idx
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        called_via: u32, // CalledVia enum
    };

    pub const ExprRecord = extern struct {
        /// Packed: fields_start (20 bits), fields_len (12 bits)
        packed_fields: u32,
        /// 0 = no ext, otherwise (ext_idx + 1)
        ext_plus_one: u32,
        _unused: u32,
    };

    pub const RecordField = extern struct {
        name: u32, // Ident.Idx
        expr: u32, // CIR.Expr.Idx
        _unused: u32,
    };

    pub const RecordDestruct = extern struct {
        ident: u32, // Ident.Idx
        /// Packed: kind (2 bits), pattern_idx (30 bits)
        packed_kind_and_pattern: u32,
        _unused: u32,
    };

    pub const ExprExternalLookup = extern struct {
        module_idx: u32, // CIR.Import.Idx
        target_node_idx: u32,
        ident_idx: u32, // Ident.Idx
    };

    pub const ExprRequiredLookup = extern struct {
        requires_idx: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprDotAccess = extern struct {
        receiver: u32, // CIR.Expr.Idx
        field_name: u32, // Ident.Idx
        /// Packed: args as FunctionArgs (20+12 bits) or 0 if no args
        packed_args_plus_one: u32,
    };

    pub const ExprString = extern struct {
        segments_start: u32,
        segments_len: u32,
        _unused: u32,
    };

    pub const ExprStringSegment = extern struct {
        segment: u32, // CIR.Expr.Idx for interpolation or string literal
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprNum = extern struct {
        kind: u32, // CIR.NumKind
        val_kind: u32, // CIR.IntValue.IntKind
        value_idx: u32, // index into int_values list
    };

    pub const ExprFracF32 = extern struct {
        value: u32, // f32 bitcast
        has_suffix: u32,
        _unused: u32,
    };

    pub const ExprFracF64 = extern struct {
        value_lo: u32, // lower 32 bits of f64
        value_hi: u32, // upper 32 bits of f64
        has_suffix: u32,
    };

    pub const ExprDec = extern struct {
        value_idx: u32, // index into dec_values list
        has_suffix: u32,
        _unused: u32,
    };

    pub const ExprDecSmall = extern struct {
        /// numerator as i16 stored in lower 16 bits
        numerator: u32,
        /// denominator_power_of_ten in lower 8 bits
        denominator_power: u32,
        has_suffix: u32,
    };

    pub const ExprTag = extern struct {
        name: u32, // Ident.Idx
        args_start: u32,
        args_len: u32,
    };

    pub const ExprNominal = extern struct {
        nominal_type_decl: u32, // CIR.Statement.Idx
        backing_expr: u32, // CIR.Expr.Idx
        backing_type: u32, // CIR.Expr.NominalBackingType
    };

    pub const ExprNominalExternal = extern struct {
        module_idx: u32, // CIR.Import.Idx
        /// Packed: target_node_idx (16 bits), backing_type (16 bits)
        packed_target_and_type: u32,
        backing_expr: u32, // CIR.Expr.Idx
    };

    pub const ExprZeroArgumentTag = extern struct {
        closure_name: u32, // Ident.Idx
        /// Packed: variant_var (16 bits), ext_var (16 bits)
        packed_vars: u32,
        name: u32, // Ident.Idx
    };

    pub const ExprClosure = extern struct {
        lambda_idx: u32, // CIR.Expr.Idx
        /// Packed: captures_start (20 bits), captures_len (12 bits)
        packed_captures: u32,
        tag_name: u32, // Ident.Idx
    };

    pub const ExprLambda = extern struct {
        body: u32, // CIR.Expr.Idx
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        _unused: u32,
    };

    pub const ExprBinOp = extern struct {
        lhs: u32, // CIR.Expr.Idx
        rhs: u32, // CIR.Expr.Idx
        op: u32, // BinOp enum
    };

    pub const ExprUnaryOp = extern struct {
        expr: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprIfThenElse = extern struct {
        /// Packed: branches_start (20 bits), branches_len (12 bits)
        packed_branches: u32,
        final_else: u32, // CIR.Expr.Idx
        _unused: u32,
    };

    pub const ExprMatch = extern struct {
        cond: u32, // CIR.Expr.Idx
        /// Packed: branches_start (20 bits), branches_len (10 bits), is_try_suffix (1 bit), _reserved (1 bit)
        packed_branches: u32,
        exhaustive: u32, // types.Var
    };

    pub const ExprDbg = extern struct {
        expr: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprCrash = extern struct {
        msg: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprBlock = extern struct {
        stmts_start: u32,
        stmts_len: u32,
        final_expr: u32, // CIR.Expr.Idx
    };

    pub const ExprHostedLambda = extern struct {
        symbol_name: u32, // Ident.Idx
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        /// Packed: body (24 bits), index (8 bits)
        packed_body_and_index: u32,
    };

    pub const ExprLowLevel = extern struct {
        op: u32, // CIR.Expr.LowLevel
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        body: u32, // CIR.Expr.Idx
    };

    pub const ExprExpect = extern struct {
        body: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprFor = extern struct {
        patt: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        body: u32, // CIR.Expr.Idx
    };

    pub const ExprReturn = extern struct {
        expr: u32, // CIR.Expr.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const ExprTypeVarDispatch = extern struct {
        type_var_alias_stmt: u32, // CIR.Statement.Idx
        method_name: u32, // Ident.Idx
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
    };

    pub const MatchBranch = extern struct {
        patterns: packed struct(u32) {
            start: u20,
            len: u12,
        },
        value: u32, // CIR.Expr.Idx
        guard_and_redundant_idx: packed struct(u32) {
            guard_plus_one: u16,
            redundant_data_idx: u16,
        },
    };

    pub const MatchBranchPattern = extern struct {
        pattern: u32, // CIR.Pattern.Idx
        degenerate: u32,
        _unused: u32,
    };

    // Type Header and Annotation
    pub const TypeHeader = extern struct {
        name: u32, // Ident.Idx
        /// Packed: type_vars_start (20 bits), type_vars_len (12 bits)
        packed_type_vars: u32,
        _unused: u32,
    };

    pub const Annotation = extern struct {
        type_anno: u32, // CIR.TypeAnno.Idx
        /// Packed: where_start (20 bits), where_len (12 bits)
        packed_where: u32,
        _unused: u32,
    };

    // Type Annotation payload types
    pub const TyApply = extern struct {
        /// Packed: local/external discriminant (1 bit) and data (31 bits)
        local_or_external: u32,
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        /// For external: additional module info
        external_data: u32,
    };

    pub const TyApplyExternal = extern struct {
        module_idx: u32, // CIR.Import.Idx
        target_node_idx: u32,
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
    };

    pub const TyRigidVar = extern struct {
        name: u32, // Ident.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const TyLookup = extern struct {
        /// Packed: local/external discriminant (1 bit) and data (31 bits)
        local_or_external: u32,
        /// For external: module info
        external_data: u32,
        _unused: u32,
    };

    pub const TyUnderscore = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    pub const TyTagUnion = extern struct {
        /// Packed: tags_start (20 bits), tags_len (12 bits)
        packed_tags: u32,
        /// 0 = no ext, 1 = open, 2+ = (ext_idx + 2)
        ext_kind: u32,
        _unused: u32,
    };

    pub const TyTag = extern struct {
        name: u32, // Ident.Idx
        /// Packed: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
        _unused: u32,
    };

    pub const TyTuple = extern struct {
        /// Packed: elems_start (20 bits), elems_len (12 bits)
        packed_elems: u32,
        _unused1: u32,
        _unused2: u32,
    };

    pub const TyRecord = extern struct {
        /// Packed: fields_start (20 bits), fields_len (12 bits)
        packed_fields: u32,
        /// 0 = no ext, otherwise (ext_idx + 1)
        ext_plus_one: u32,
        _unused: u32,
    };

    pub const TyRecordField = extern struct {
        name: u32, // Ident.Idx
        anno: u32, // CIR.TypeAnno.Idx
        /// 0 = required, 1 = optional
        optional: u32,
    };

    pub const TyFn = extern struct {
        /// Packed: args_start (20 bits), args_len (11 bits), effectful (1 bit)
        packed_args: u32,
        ret: u32, // CIR.TypeAnno.Idx
        _unused: u32,
    };

    pub const TyParens = extern struct {
        inner: u32, // CIR.TypeAnno.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const TyLookupExternal = extern struct {
        module_idx: u32, // CIR.Import.Idx
        target_node_idx: u32,
        ident_idx: u32, // Ident.Idx
    };

    pub const TyMalformed = extern struct {
        diagnostic: u32, // CIR.Diagnostic.Idx
        _unused1: u32,
        _unused2: u32,
    };

    // Where clause payload types
    pub const WhereMethod = extern struct {
        var_: u32, // CIR.TypeAnno.Idx
        method_name: u32, // Ident.Idx
        /// Packed: args_start (16 bits), args_len (8 bits), ret (8 bits as offset)
        /// Actually we need full ret idx, so: args_start (20 bits), args_len (12 bits)
        packed_args: u32,
    };

    pub const WhereAlias = extern struct {
        var_: u32, // CIR.TypeAnno.Idx
        alias_name: u32, // Ident.Idx
        _unused: u32,
    };

    pub const WhereMalformed = extern struct {
        diagnostic: u32, // CIR.Diagnostic.Idx
        _unused1: u32,
        _unused2: u32,
    };

    // Pattern payload types
    pub const PatternIdentifier = extern struct {
        ident: u32, // Ident.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternAs = extern struct {
        ident: u32, // Ident.Idx
        pattern: u32, // CIR.Pattern.Idx
        _unused: u32,
    };

    pub const PatternAppliedTag = extern struct {
        args_start: u32,
        args_len: u32,
        name: u32, // Ident.Idx
    };

    pub const PatternNominal = extern struct {
        nominal_type_decl: u32, // CIR.Statement.Idx
        backing_pattern: u32, // CIR.Pattern.Idx
        backing_type: u32, // CIR.Expr.NominalBackingType
    };

    pub const PatternNominalExternal = extern struct {
        module_idx: u32, // CIR.Import.Idx
        /// Packed: target_node_idx (16 bits), backing_type (16 bits)
        packed_target_and_type: u32,
        backing_pattern: u32, // CIR.Pattern.Idx
    };

    pub const PatternRecordDestructure = extern struct {
        destructs_start: u32,
        destructs_len: u32,
        _unused: u32,
    };

    pub const PatternList = extern struct {
        /// Packed: patterns_start (20 bits), patterns_len (12 bits)
        packed_patterns: u32,
        /// Packed: rest_info encoding
        rest_info: u32,
        _unused: u32,
    };

    pub const PatternTuple = extern struct {
        patterns_start: u32,
        patterns_len: u32,
        _unused: u32,
    };

    pub const PatternNumLiteral = extern struct {
        kind: u32, // CIR.NumKind
        val_kind: u32, // CIR.IntValue.IntKind
        value_idx: u32, // index into int_values list
    };

    pub const PatternDecLiteral = extern struct {
        value_idx: u32, // index into dec_values list
        has_suffix: u32, // 0 or 1
        _unused: u32,
    };

    pub const PatternF32Literal = extern struct {
        value: u32, // f32 bitcast
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternF64Literal = extern struct {
        value_lo: u32, // lower 32 bits of f64
        value_hi: u32, // upper 32 bits of f64
        _unused: u32,
    };

    pub const PatternSmallDecLiteral = extern struct {
        numerator: u32,
        denominator_power: u32,
        has_suffix: u32,
    };

    pub const PatternStrLiteral = extern struct {
        literal: u32, // CIR.StrLiteral.Idx
        _unused1: u32,
        _unused2: u32,
    };

    pub const PatternUnderscore = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    // Lambda Capture
    pub const LambdaCapture = extern struct {
        name: u32, // Ident.Idx
        scope_depth: u32,
        pattern_idx: u32, // CIR.Pattern.Idx
    };

    // Definition
    pub const Def = extern struct {
        pattern: u32, // CIR.Pattern.Idx
        expr: u32, // CIR.Expr.Idx
        /// Packed: kind (2 bits), kind_data (14 bits), anno_plus_one (16 bits)
        packed_kind_and_anno: u32,
    };

    // Exposed Item
    pub const ExposedItem = extern struct {
        name: u32, // Ident.Idx
        alias: u32, // Ident.Idx or 0 if no alias
        is_wildcard: u32,
    };

    // If Branch
    pub const IfBranch = extern struct {
        cond: u32, // CIR.Expr.Idx
        body: u32, // CIR.Expr.Idx
        _unused: u32,
    };

    // Type var slot
    pub const TypeVarSlot = extern struct {
        _unused1: u32,
        _unused2: u32,
        _unused3: u32,
    };

    // Malformed (runtime error)
    pub const Malformed = extern struct {
        diagnostic: u32, // CIR.Diagnostic.Idx
        _unused1: u32,
        _unused2: u32,
    };

    // Diagnostic - all diagnostic nodes share this structure
    pub const Diagnostic = extern struct {
        data_1: u32,
        data_2: u32,
        data_3: u32,
    };
};

// Compile-time size verification
comptime {
    // Verify Payload is exactly 12 bytes (3 x u32)
    std.debug.assert(@sizeOf(Payload) == 12);
    // Verify Node is exactly 16 bytes (3 x u32 data + tag)
    // The data_1, data_2, data_3 fields are laid out identically to Payload
    std.debug.assert(@sizeOf(Node) == 16);
    // Verify we can safely reinterpret the data fields as a Payload
    std.debug.assert(@offsetOf(Node, "data_1") == 0);
    std.debug.assert(@offsetOf(Node, "data_2") == 4);
    std.debug.assert(@offsetOf(Node, "data_3") == 8);
    std.debug.assert(@offsetOf(Node, "tag") == 12);
}

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
};
