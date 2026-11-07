//! The reader over a checked module's canonical bodies and the checked facts
//! attached to their nodes.
//!
//! Lowering walks the module's CIR expressions, patterns and statements
//! directly, keyed by their CIR ids. This reader answers the questions
//! lowering asks of one node: its normalized shape, its checked type, the
//! dispatch plan and resolved value reference publication attached to it,
//! the facts checking recorded for it, and the body-level analyses computed
//! on first demand for the definition that contains it.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const body_analyses = @import("body_analyses.zig");
const canonical = @import("canonical_names.zig");
const checked_artifact = @import("checked_artifact.zig");
const checked_type_ids = @import("checked_type_ids.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Ident = base.Ident;
const Region = base.Region;
const Var = types.Var;

const Allocator = std.mem.Allocator;
const CheckedTypeId = checked_artifact.CheckedTypeId;
const ComptimeRootId = checked_artifact.ComptimeRootId;
const ResolvedValueId = checked_artifact.ResolvedValueId;
const ResolvedValueRefId = checked_artifact.ResolvedValueRefId;
const StaticDispatchPlanId = static_dispatch.StaticDispatchPlanId;
const IteratorForPlanId = static_dispatch.IteratorForPlanId;

/// An expression of the module's bodies: its CIR node.
pub const ExprId = CIR.Expr.Idx;
/// A pattern of the module's bodies: its CIR node.
pub const PatternId = CIR.Pattern.Idx;
/// A statement of the module's bodies: its CIR node.
pub const StatementId = CIR.Statement.Idx;
/// A binder is the assign, var-assign, as, or custom literal pattern that
/// introduces it.
pub const BinderId = CIR.Pattern.Idx;

fn invariant(comptime message: []const u8) noreturn {
    base.invariant("checked body reader invariant violated: " ++ message, .{});
}

/// A normalized expression node.
pub const Expr = struct {
    id: ExprId,
    ty: CheckedTypeId,
    source_region: Region,
    data: ExprData,
};

/// A normalized pattern node.
pub const Pattern = struct {
    id: PatternId,
    ty: CheckedTypeId,
    source_region: Region,
    data: PatternData,
};

/// A normalized statement node.
pub const Statement = struct {
    id: StatementId,
    source_region: Region,
    data: StatementData,
};

/// Text a body node carries: a literal of the module's string store, or a
/// literal-only string expression whose segments concatenate to the text.
pub const Text = union(enum) {
    literal: base.StringLiteral.Idx,
    quote: []const ExprId,
};

/// A string built from its parts in order: `first`, then every expression of
/// `rest`. `first` is null for a string literal written as one span.
pub const StrParts = struct {
    first: ?ExprId = null,
    rest: []const ExprId,

    pub fn len(self: StrParts) usize {
        return self.rest.len + @intFromBool(self.first != null);
    }

    pub fn at(self: StrParts, index: usize) ExprId {
        if (self.first) |first| {
            if (index == 0) return first;
            return self.rest[index - 1];
        }
        return self.rest[index];
    }
};

/// A string interpolation: literal segments alternating with interpolated
/// values. `parts` alternates value, segment.
pub const InterpolationData = struct {
    first: ExprId,
    parts: []const ExprId,

    pub fn segmentCount(self: InterpolationData) usize {
        return self.parts.len / 2 + 1;
    }

    pub fn segment(self: InterpolationData, index: usize) ExprId {
        return if (index == 0) self.first else self.parts[2 * index - 1];
    }

    pub fn valueCount(self: InterpolationData) usize {
        return self.parts.len / 2;
    }

    pub fn value(self: InterpolationData, index: usize) ExprId {
        return self.parts[2 * index];
    }

    /// Every part in order: the first segment, then `parts`.
    pub fn strParts(self: InterpolationData) StrParts {
        return .{ .first = self.first, .rest = self.parts };
    }
};

/// An equality that compares only a payload-free tag's discriminant: `value`
/// against `tag`.
pub const TagDiscriminantEquality = struct {
    value: ExprId,
    tag: canonical.TagLabelId,
};

/// The shape of an expression node, read from CIR. Facts checking and
/// publication attached to the node are separate accessors of the reader.
pub const ExprData = union(enum) {
    /// Any numeric literal (`numeralLiteral`, `numeralPlan`,
    /// `literalConversionRoot`).
    numeral,
    /// A string literal's segments (`quoteIsCustom`, `quotePlan`,
    /// `literalConversionRoot`).
    str: []const ExprId,
    str_segment: base.StringLiteral.Idx,
    bytes_literal: base.StringLiteral.Idx,
    /// A local lookup and the pattern it reads (`resolvedRef`).
    lookup_local: PatternId,
    /// An external or resolved associated lookup (`resolvedRef`).
    lookup_external,
    /// A platform requirement lookup (`resolvedRef`).
    lookup_required,
    list: []const ExprId,
    empty_list,
    tuple: []const ExprId,
    match_: struct {
        cond: ExprId,
        branches: []const CIR.Expr.Match.Branch.Idx,
        is_try_suffix: bool,
        skip_exhaustiveness: bool,
    },
    if_: struct {
        branches: []const CIR.Expr.IfBranch.Idx,
        final_else: ExprId,
    },
    /// A call (`callDirectTarget`, `callFnType`).
    call: struct {
        func: ExprId,
        args: []const ExprId,
        called_via: base.CalledVia,
    },
    record: struct {
        fields: []const CIR.RecordField.Idx,
        unsets: []const CIR.UnsetField.Idx,
        ext: ?ExprId,
    },
    empty_record,
    block: struct {
        statements: []const StatementId,
        final_expr: ExprId,
    },
    /// A tag application (`tagLabel` of `name`).
    tag: struct {
        name: Ident.Idx,
        args: []const ExprId,
    },
    nominal: struct {
        backing_expr: ExprId,
        backing_type: CIR.Expr.NominalBackingType,
    },
    zero_argument_tag: struct {
        closure_name: Ident.Idx,
        name: Ident.Idx,
    },
    closure: struct {
        lambda: ExprId,
        captures: []const CIR.Expr.Capture.Idx,
        tag_name: Ident.Idx,
    },
    lambda: struct {
        args: []const PatternId,
        body: ExprId,
    },
    binop: struct {
        op: CIR.Expr.Binop.Op,
        lhs: ExprId,
        rhs: ExprId,
    },
    unary_minus: ExprId,
    field_access: struct {
        receiver: ExprId,
        segments: CIR.Expr.FieldAccessSegment.Span,
    },
    /// A method dispatch (`dispatchPlan`).
    dispatch_call,
    /// A string interpolation (`interpolationIsBuiltinStr`, `dispatchPlan`,
    /// `interpolationAssemblerType`).
    interpolation: InterpolationData,
    /// A structural equality (`tagDiscriminantEquality`).
    structural_eq: struct {
        lhs: ExprId,
        rhs: ExprId,
        negated: bool,
    },
    structural_hash: struct {
        value: ExprId,
        hasher: ExprId,
    },
    /// An equality dispatch (`dispatchPlan`).
    method_eq,
    /// A type method dispatch (`dispatchPlan`).
    type_dispatch_call,
    tuple_access: struct {
        tuple: ExprId,
        elem_index: u32,
    },
    /// A runtime error. `evaluated` lists the operands a retired expression
    /// still evaluates, in order, before it crashes.
    runtime_error: struct { evaluated: []const ExprId = &.{} },
    crash: base.StringLiteral.Idx,
    dbg: ExprId,
    expect_err: struct {
        expr: ExprId,
        snippet: base.StringLiteral.Idx,
    },
    expect: ExprId,
    ellipsis,
    anno_only,
    break_,
    return_: struct {
        expr: ExprId,
        lambda: ExprId,
        context: checked_artifact.CheckedReturnContext,
    },
    /// A `for` expression (`iteratorForPlan`).
    for_: ForData,
    /// A hosted function (`externalSymbolName` of `symbol_name`).
    hosted_lambda: struct {
        symbol_name: Ident.Idx,
        args: []const PatternId,
    },
    run_low_level: struct {
        op: CIR.Expr.LowLevel,
        args: []const ExprId,
    },
};

/// A `for` loop's binding pattern, iterable and body.
pub const ForData = struct {
    pattern: PatternId,
    expr: ExprId,
    body: ExprId,
};

/// The shape of a pattern node, read from CIR.
pub const PatternData = union(enum) {
    assign,
    as: struct { pattern: PatternId },
    /// A tag pattern (`tagLabel` of `name`).
    applied_tag: struct {
        name: Ident.Idx,
        args: []const PatternId,
    },
    nominal: struct {
        backing_pattern: PatternId,
        backing_type: CIR.Expr.NominalBackingType,
    },
    record_destructure: []const CIR.Pattern.RecordDestruct.Idx,
    list: struct {
        patterns: []const PatternId,
        rest: ?checked_artifact.CheckedListRestPattern,
    },
    tuple: []const PatternId,
    /// Any numeric literal pattern (`numeralLiteral`,
    /// `literalPatternIsCustom`).
    numeral_literal,
    /// A string literal pattern (`literalPatternIsCustom`).
    str_literal: base.StringLiteral.Idx,
    str_interpolation: struct {
        prefix: base.StringLiteral.Idx,
        steps: CIR.Pattern.StrPatternStep.Span,
        end: checked_artifact.CheckedStrPatternEnd,
    },
    underscore,
    runtime_error,
};

/// A loop whose condition runs on every iteration.
pub const ConditionLoop = struct {
    cond: ExprId,
    body: ExprId,
};

/// The shape of a statement node, read from CIR.
pub const StatementData = union(enum) {
    /// A declaration (`declIsRecursive`).
    decl: struct { pattern: PatternId, expr: ExprId },
    /// A local function binding promoted to a procedure of its own.
    promoted_proc: struct { pattern: PatternId, expr: ExprId },
    var_: struct { pattern: PatternId, expr: ExprId },
    var_uninitialized: struct { pattern: PatternId },
    reassign: struct { pattern: PatternId, expr: ExprId },
    crash: base.StringLiteral.Idx,
    dbg: ExprId,
    expr: ExprId,
    expect: ExprId,
    /// A `for` statement (`iteratorForPlan`).
    for_: ForData,
    while_: ConditionLoop,
    infinite_loop: ConditionLoop,
    breakable_loop: ConditionLoop,
    break_,
    return_: struct { expr: ExprId, lambda: ExprId },
    /// The declaration of a method whose right-hand side checking rejected:
    /// it evaluates nothing.
    rejected_method_decl,
    import_,
    alias_decl,
    where_alias_decl,
    nominal_decl,
    type_anno,
    type_var_alias,
    runtime_error,
};

/// One branch of a `match` expression.
pub const MatchBranch = struct {
    patterns: []const CIR.Expr.Match.BranchPattern.Idx,
    value: ExprId,
    guard: ?ExprId,
};

/// One alternative pattern of a match branch.
pub const MatchBranchPattern = struct {
    pattern: PatternId,
    degenerate: bool,
};

/// One condition and body of an `if` expression.
pub const IfBranch = struct {
    cond: ExprId,
    body: ExprId,
};

/// One field of a record construction.
pub const RecordExprField = struct {
    label: canonical.RecordFieldLabelId,
    value: ExprId,
};

/// One source-ordered segment of a record field access path. `success_ty` is
/// the type of the path prefix's successful payload: for a `.?` segment
/// too, the field's value type.
pub const FieldAccessSegment = struct {
    field_name: canonical.RecordFieldLabelId,
    success_ty: CheckedTypeId,
    source_region: Region,
    mode: checked_artifact.CheckedFieldAccessMode,
};

/// One capture of a closure and its canonical capture identity.
pub const Capture = struct {
    pattern: PatternId,
    scope_depth: u32,
    capture_id: checked_artifact.CaptureId,
};

/// How a record destructure binds one field.
pub const RecordDestructKind = union(enum) {
    required: PatternId,
    sub_pattern: PatternId,
    rest: PatternId,
};

/// One field of a record destructure pattern.
pub const RecordDestruct = struct {
    label: canonical.RecordFieldLabelId,
    kind: RecordDestructKind,
};

/// One delimited capture step of a string interpolation pattern.
pub const StrPatternStep = struct {
    capture: ?PatternId,
    delimiter: base.StringLiteral.Idx,
};

/// The facts of a binder: the assign, var-assign, as, or custom literal
/// pattern that introduces a name or a matched value.
pub const Binder = struct {
    pattern: PatternId,
    reassignable: bool,
    /// A generalized callable lookup binding.
    is_scheme_alias: bool,
    /// The binder of a promoted local procedure, which names a top-level
    /// procedure rather than a value of the body declaring it.
    is_promoted: bool,
    /// Bound by a recursive value declaration.
    recursive_value: bool,
};

/// The publication tables a reader answers by-node questions from. A reader
/// built before a table exists reports that table's facts as absent; the
/// body-level analyses read every table and require them all.
pub const Tables = struct {
    /// The identity of the checked module the bodies belong to.
    module: ?canonical.CheckedModuleDigest = null,
    plans: ?*const static_dispatch.StaticDispatchPlanTable = null,
    refs: ?*const checked_artifact.ResolvedValueRefTable = null,
    roots: ?*const checked_artifact.CompileTimeRootTable = null,
    const_templates: ?*const checked_artifact.ConstTemplateTable = null,
    templates: ?*const checked_artifact.CheckedProcedureTemplateTable = null,
    top_level_procedure_bindings: ?*const checked_artifact.TopLevelProcedureBindingTable = null,
    nested_proc_sites: ?*const checked_artifact.NestedProcSiteTable = null,
    exported_defs: []const CIR.Def.Idx = &.{},
    /// The pairing's types for a platform read under a platform/app pairing.
    pairing_types: ?*const checked_artifact.PairingTypeColumn = null,
};

/// The value a custom literal pattern converts its literal to before
/// comparing it with the matched value.
pub const LiteralConversion = struct {
    literal: union(enum) {
        numeral: ModuleEnv.NumeralLiteral,
        quote: Text,
    },
    /// The `from_numeral` or `from_quote` plan, absent before plans exist.
    plan: ?StaticDispatchPlanId,
    conversion_root: ?ComptimeRootId,
    /// The type the literal converts to.
    ty: CheckedTypeId,
};

/// The equality test a custom literal pattern performs: the pattern matches
/// when `is_eq(value, conversion)` holds, where `value` is the matched value,
/// read through the pattern itself as its binder, and `conversion` is the
/// pattern's literal converted at the pattern's type. The plan's operands are
/// `.literal_pattern_value` and `.literal_pattern_conversion` of the pattern.
pub const LiteralPatternGuard = struct {
    pattern: PatternId,
    conversion: LiteralConversion,
    /// The `is_eq` plan, absent before plans exist.
    equality_plan: ?StaticDispatchPlanId,
    /// The equality's result type.
    equality_ty: CheckedTypeId,
};

/// The reader over one checked module's bodies.
pub const CheckedBodies = struct {
    env: *const ModuleEnv,
    names: *const canonical.CanonicalNameStore,
    /// The module's checked type view, read without the pairing.
    types: checked_artifact.CheckedTypeStoreView,
    analyses: *body_analyses.BodyAnalyses,
    tables: Tables = .{},

    pub fn index(self: CheckedBodies) *const body_analyses.ModuleIndex {
        return &self.analyses.index;
    }

    /// The number of nodes of the module, which bounds every node id.
    pub fn nodeCount(self: CheckedBodies) usize {
        return self.env.store.nodes.len();
    }

    /// A node's CIR tag.
    pub fn nodeTag(self: CheckedBodies, raw_node: u32) CIR.Node.Tag {
        return self.env.store.nodes.fieldItem(.tag, @fromBackingInt(raw_node));
    }

    fn store(self: CheckedBodies) *const can.NodeStore {
        return &self.env.store;
    }

    fn regionOf(self: CheckedBodies, raw_node: u32) Region {
        return self.store().getNodeRegion(@fromBackingInt(raw_node));
    }

    /// The checked type of `var_`, a variable of this module's sealed store.
    pub fn typeOfVar(self: CheckedBodies, var_: Var) CheckedTypeId {
        return checked_type_ids.fromVar(self.env.types.resolveVar(var_).var_);
    }

    /// The checked type a node with solver variable `var_` has: the
    /// variable's own, or the pairing's image of it when the module is read
    /// under a pairing.
    pub fn typeOf(self: CheckedBodies, var_: Var) CheckedTypeId {
        const own = self.typeOfVar(var_);
        if (self.tables.pairing_types) |column| return column.image(own);
        return own;
    }

    pub fn exprType(self: CheckedBodies, id: ExprId) CheckedTypeId {
        return self.typeOf(ModuleEnv.varFrom(id));
    }

    /// A pattern's type; a global definition's pattern has its definition's
    /// type.
    pub fn patternType(self: CheckedBodies, id: PatternId) CheckedTypeId {
        const var_: Var = if (self.index().topLevelDef(id)) |def| ModuleEnv.varFrom(def) else ModuleEnv.varFrom(id);
        return self.typeOf(var_);
    }

    /// The type a parent receives an expression's value at: the widened type
    /// of its row coercion when checking recorded one, its own type
    /// otherwise.
    pub fn valueType(self: CheckedBodies, id: ExprId) CheckedTypeId {
        return self.rowCoercion(id) orelse self.exprType(id);
    }

    /// The widened type a row-coerced result flows into its parent at, when
    /// checking recorded one (`ModuleEnv.RowCoercedResult`). Lowering
    /// coerces the node's value to it wherever a parent reads the node.
    pub fn rowCoercion(self: CheckedBodies, id: ExprId) ?CheckedTypeId {
        const wide = self.env.rowCoercedResultWideVar(ModuleEnv.nodeIdxFrom(id)) orelse return null;
        return self.typeOf(wide);
    }

    /// Whether checking recorded the node as erroneous in its own right.
    pub fn nodeIsErroneous(self: CheckedBodies, raw_node: u32) bool {
        return self.env.nodeIsErroneous(@fromBackingInt(raw_node));
    }

    pub fn tagLabel(self: CheckedBodies, ident: Ident.Idx) canonical.TagLabelId {
        return self.names.lookupTagIdent(&self.env.common.idents, ident) orelse invariant("a body tag name was not interned");
    }

    pub fn recordFieldLabel(self: CheckedBodies, ident: Ident.Idx) canonical.RecordFieldLabelId {
        return self.names.lookupRecordFieldIdent(&self.env.common.idents, ident) orelse invariant("a body record field name was not interned");
    }

    pub fn text(self: CheckedBodies, literal: base.StringLiteral.Idx) []const u8 {
        return self.env.getString(literal);
    }

    /// The text a quote literal node carries: a string expression's
    /// segments, or a string literal pattern's literal.
    pub fn quoteText(self: CheckedBodies, raw_node: u32) Text {
        switch (self.index().nodeKind(raw_node)) {
            .expr => {
                const data = self.store().getExpr(@fromBackingInt(@intCast(raw_node)));
                if (data != .e_str) invariant("a quote literal expression was not a string");
                return .{ .quote = self.store().sliceExpr(data.e_str.span) };
            },
            .pattern => {
                const data = self.store().getPattern(@fromBackingInt(@intCast(raw_node)));
                if (data != .str_literal) invariant("a quote literal pattern was not a string literal");
                return .{ .literal = data.str_literal.literal };
            },
            .statement, .unreached => invariant("a quote literal was neither a reached expression nor a reached pattern"),
        }
    }

    /// Append the bytes of `value` to `out`.
    pub fn appendText(self: CheckedBodies, allocator: std.mem.Allocator, value: Text, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        switch (value) {
            .literal => |literal| try out.appendSlice(allocator, self.text(literal)),
            .quote => |segments| for (segments) |segment| {
                const data = self.store().getExpr(segment);
                if (data != .e_str_segment) invariant("a literal-only string contained an interpolation");
                try out.appendSlice(allocator, self.text(data.e_str_segment.literal));
            },
        }
    }

    pub fn expr(self: CheckedBodies, id: ExprId) Expr {
        return .{
            .id = id,
            .ty = self.exprType(id),
            .source_region = self.regionOf(@backingInt(id)),
            .data = self.exprData(id),
        };
    }

    pub fn pattern(self: CheckedBodies, id: PatternId) Pattern {
        return .{
            .id = id,
            .ty = self.patternType(id),
            .source_region = self.regionOf(@backingInt(id)),
            .data = self.patternData(id),
        };
    }

    pub fn statement(self: CheckedBodies, id: StatementId) Statement {
        return .{
            .id = id,
            .source_region = self.regionOf(@backingInt(id)),
            .data = self.statementData(id),
        };
    }

    /// The dispatch plan keyed by a node: a method, type method, equality
    /// or interpolation dispatch, or a custom literal pattern's equality.
    /// Absent before plans exist.
    pub fn dispatchPlan(self: CheckedBodies, raw_node: u32) ?StaticDispatchPlanId {
        const plans = self.tables.plans orelse return null;
        return plans.lookupByNode(raw_node);
    }

    /// A numeric literal's `from_numeral` plan, when its target is not
    /// builtin. Absent before plans exist.
    pub fn numeralPlan(self: CheckedBodies, raw_node: u32) ?StaticDispatchPlanId {
        const plans = self.tables.plans orelse return null;
        return plans.numeralPlanForNode(raw_node);
    }

    /// A string literal's `from_quote` plan. Absent before plans exist.
    pub fn quotePlan(self: CheckedBodies, raw_node: u32) ?StaticDispatchPlanId {
        const plans = self.tables.plans orelse return null;
        return plans.quotePlanForNode(raw_node);
    }

    /// A `for` expression's or statement's iterator plan. Absent before plans
    /// exist.
    pub fn iteratorForPlan(self: CheckedBodies, raw_node: u32) ?IteratorForPlanId {
        const plans = self.tables.plans orelse return null;
        return plans.iteratorForPlanForNode(raw_node);
    }

    /// A lookup's resolved value reference. Absent before references exist.
    pub fn resolvedRef(self: CheckedBodies, id: ExprId) ?ResolvedValueRefId {
        const refs = self.tables.refs orelse return null;
        return refs.lookupIdByExpr(id);
    }

    /// A call's direct procedure target, when it has one.
    pub fn callDirectTarget(self: CheckedBodies, id: ExprId) ?ResolvedValueId {
        const refs = self.tables.refs orelse return null;
        return refs.callDirectTarget(id);
    }

    /// A call's function constraint type.
    pub fn callFnType(self: CheckedBodies, id: ExprId) CheckedTypeId {
        const call = self.store().getExpr(id);
        if (call != .e_call) invariant("a call type was requested for an expression that is not a call");
        return self.typeOf(call.e_call.constraint_fn_var orelse invariant("a checked call had no function constraint type"));
    }

    /// The type of the assembler a custom interpolation's conversion returns.
    pub fn interpolationAssemblerType(self: CheckedBodies, id: ExprId) CheckedTypeId {
        const interpolation = self.store().getExpr(id);
        if (interpolation != .e_interpolation) invariant("an assembler type was requested for an expression that is not an interpolation");
        return self.typeOf(interpolation.e_interpolation.assembler_fn_var orelse invariant("a checked interpolation had no assembler type"));
    }

    /// The compile-time root that evaluates a literal's conversion, when that
    /// conversion is independent of specialization.
    pub fn literalConversionRoot(self: CheckedBodies, raw_node: u32) ?ComptimeRootId {
        const roots = self.tables.roots orelse return null;
        return roots.literalConversionRootForNode(raw_node);
    }

    /// A numeric literal expression's or pattern's exact digits.
    pub fn numeralLiteral(self: CheckedBodies, raw_node: u32) ModuleEnv.NumeralLiteral {
        return self.env.numeralLiteralForNode(@fromBackingInt(@intCast(raw_node))) orelse invariant("a numeral literal had no exact digits");
    }

    /// Whether lowering evaluates an expression as a runtime error because
    /// reference resolution rejected it.
    pub fn exprIsRetired(self: CheckedBodies, id: ExprId) bool {
        const refs = self.tables.refs orelse return false;
        return refs.exprIsRetired(id);
    }

    /// Whether a declaration statement binds a recursive value.
    pub fn declIsRecursive(self: CheckedBodies, id: StatementId) bool {
        return self.index().isRecursiveDecl(id);
    }

    pub fn externalSymbolName(self: CheckedBodies, ident: Ident.Idx) canonical.ExternalSymbolNameId {
        return self.names.lookupExternalSymbolIdent(&self.env.common.idents, ident) orelse invariant("a hosted symbol name was not interned");
    }

    pub fn exprData(self: CheckedBodies, id: ExprId) ExprData {
        // Rejected binding uses and calls of retired callees evaluate as
        // runtime errors (`ResolvedValueRefTable.retired_exprs`).
        if (self.exprIsRetired(id)) return .{ .runtime_error = .{} };
        const node_store = self.store();
        return switch (node_store.getExpr(id)) {
            .e_num, .e_dec, .e_dec_small, .e_num_from_numeral, .e_typed_int, .e_typed_frac, .e_typed_num_from_numeral => .numeral,
            .e_frac_f32, .e_frac_f64 => invariant("a float-valued literal without exact digits reached a checked body"),
            .e_str_segment => |segment| .{ .str_segment = segment.literal },
            .e_str => |str| .{ .str = node_store.sliceExpr(str.span) },
            .e_bytes_literal => |bytes| .{ .bytes_literal = bytes.literal },
            .e_lookup_local => |lookup| .{ .lookup_local = lookup.pattern_idx },
            .e_lookup_external, .e_lookup_associated_resolved => .lookup_external,
            .e_lookup_required => .lookup_required,
            .e_lookup_associated_local, .e_lookup_associated => invariant("an unresolved associated lookup reached a checked body"),
            .e_list => |list| .{ .list = node_store.sliceExpr(list.elems) },
            .e_empty_list => .empty_list,
            .e_tuple => |tuple| .{ .tuple = node_store.sliceExpr(tuple.elems) },
            .e_match => |match| .{ .match_ = .{
                .cond = match.cond,
                .branches = node_store.sliceMatchBranches(match.branches),
                .is_try_suffix = match.is_try_suffix,
                .skip_exhaustiveness = match.skip_exhaustiveness,
            } },
            .e_if => |if_| .{ .if_ = .{ .branches = node_store.sliceIfBranches(if_.branches), .final_else = if_.final_else } },
            .e_call => |call| .{ .call = .{
                .func = call.func,
                .args = node_store.sliceExpr(call.args),
                .called_via = call.called_via,
            } },
            .e_record => |record| .{ .record = .{
                .fields = node_store.sliceRecordFields(record.fields),
                .unsets = node_store.sliceUnsetFields(record.unsets),
                .ext = record.ext,
            } },
            .e_empty_record => .empty_record,
            .e_block => |block| .{ .block = .{ .statements = node_store.sliceStatements(block.stmts), .final_expr = block.final_expr } },
            .e_tag => |tag| .{ .tag = .{ .name = tag.name, .args = node_store.sliceExpr(tag.args) } },
            .e_nominal => |nominal| .{ .nominal = .{ .backing_expr = nominal.backing_expr, .backing_type = nominal.backing_type } },
            .e_nominal_external => |nominal| .{ .nominal = .{ .backing_expr = nominal.backing_expr, .backing_type = nominal.backing_type } },
            .e_zero_argument_tag => |tag| .{ .zero_argument_tag = .{ .closure_name = tag.closure_name, .name = tag.name } },
            .e_closure => |closure| .{ .closure = .{
                .lambda = closure.lambda_idx,
                .captures = node_store.sliceCaptures(closure.captures),
                .tag_name = closure.tag_name,
            } },
            .e_lambda => |lambda| .{ .lambda = .{ .args = node_store.slicePatterns(lambda.args), .body = lambda.body } },
            .e_binop => |binop| .{ .binop = .{ .op = binop.op, .lhs = binop.lhs, .rhs = binop.rhs } },
            .e_unary_minus => |unary| .{ .unary_minus = unary.expr },
            .e_field_access => |access| .{ .field_access = .{ .receiver = access.receiver, .segments = access.segments } },
            .e_method_call, .e_type_method_call => invariant("an ordinary method call reached a checked body"),
            .e_dispatch_call => .dispatch_call,
            .e_interpolation => |interpolation| .{ .interpolation = .{
                .first = interpolation.first,
                .parts = node_store.sliceExpr(interpolation.parts),
            } },
            .e_structural_eq => |eq| .{ .structural_eq = .{ .lhs = eq.lhs, .rhs = eq.rhs, .negated = eq.negated } },
            .e_structural_hash => |hash| .{ .structural_hash = .{ .value = hash.value, .hasher = hash.hasher } },
            .e_method_eq => .method_eq,
            .e_type_dispatch_call => .type_dispatch_call,
            .e_tuple_access => |access| .{ .tuple_access = .{ .tuple = access.tuple, .elem_index = access.elem_index } },
            .e_runtime_error => |runtime_error| .{ .runtime_error = .{ .evaluated = node_store.sliceExpr(runtime_error.evaluated) } },
            .e_crash => |crash| .{ .crash = crash.msg },
            .e_dbg => |dbg| .{ .dbg = dbg.expr },
            .e_expect_err => |expect_err| .{ .expect_err = .{ .expr = expect_err.expr, .snippet = expect_err.snippet } },
            .e_expect => |expect| .{ .expect = expect.body },
            .e_ellipsis => .ellipsis,
            .e_anno_only, .e_derived_method => .anno_only,
            .e_break => .break_,
            .e_return => |ret| .{ .return_ = .{
                .expr = ret.expr,
                .lambda = ret.lambda,
                .context = switch (ret.context) {
                    .return_expr => .return_expr,
                    .try_suffix => .try_suffix,
                },
            } },
            .e_for => |for_| .{ .for_ = .{ .pattern = for_.patt, .expr = for_.expr, .body = for_.body } },
            .e_hosted_lambda => |hosted| .{ .hosted_lambda = .{ .symbol_name = hosted.symbol_name, .args = node_store.slicePatterns(hosted.args) } },
            .e_run_low_level => |run| .{ .run_low_level = .{ .op = run.op, .args = node_store.sliceExpr(run.args) } },
            .e_deferred_import_ref => invariant("a deferred import reference reached a checked body"),
        };
    }

    /// Whether `id` is a literal string segment with no bytes. Such a
    /// segment writes nothing into the string it is part of, so a lowering
    /// that writes a literal's parts once leaves it out.
    pub fn isEmptyStrSegment(self: CheckedBodies, id: ExprId) bool {
        const data = self.store().getExpr(id);
        return data == .e_str_segment and self.text(data.e_str_segment.literal).len == 0;
    }

    /// Whether a string literal's conversion dispatches to its target's
    /// `from_quote`.
    pub fn quoteIsCustom(self: CheckedBodies, raw_node: u32) bool {
        const plan = self.env.quoteDispatchPlanForNode(@fromBackingInt(@intCast(raw_node))) orelse return false;
        return switch (plan.dispatchResolution()) {
            .builtin_direct, .checked_error => false,
            .custom_dispatch, .specialization_dispatch => true,
            .unresolved => invariant("an unresolved quote literal reached a checked body"),
        };
    }

    /// Whether an interpolation's target is concretely the builtin `Str`, so
    /// it concatenates its parts directly. An open target can instantiate at
    /// a custom `from_interpolation` type and keeps its dispatch.
    pub fn interpolationIsBuiltinStr(self: CheckedBodies, id: ExprId) bool {
        return checked_artifact.checkedConcreteBuiltinForLiteralTarget(self.types, self.typeOfVar(ModuleEnv.varFrom(id))) == .str;
    }

    /// The tag a structural equality compares by discriminant alone: a
    /// payload-free tag on either side.
    pub fn tagDiscriminantEquality(self: CheckedBodies, lhs: ExprId, rhs: ExprId) ?TagDiscriminantEquality {
        if (self.zeroPayloadTag(rhs)) |tag| return .{ .value = lhs, .tag = self.tagLabel(tag) };
        if (self.zeroPayloadTag(lhs)) |tag| return .{ .value = rhs, .tag = self.tagLabel(tag) };
        return null;
    }

    fn zeroPayloadTag(self: CheckedBodies, id: ExprId) ?Ident.Idx {
        const data = self.store().getExpr(id);
        if (data == .e_zero_argument_tag) return data.e_zero_argument_tag.name;
        if (data == .e_tag and data.e_tag.args.span.len == 0) return data.e_tag.name;
        return null;
    }

    pub fn patternData(self: CheckedBodies, id: PatternId) PatternData {
        const node_store = self.store();
        return switch (node_store.getPattern(id)) {
            .assign, .var_assign => .assign,
            .as => |as| .{ .as = .{ .pattern = as.pattern } },
            .applied_tag => |tag| .{ .applied_tag = .{ .name = tag.name, .args = node_store.slicePatterns(tag.args) } },
            .nominal => |nominal| .{ .nominal = .{ .backing_pattern = nominal.backing_pattern, .backing_type = nominal.backing_type } },
            .nominal_external => |nominal| .{ .nominal = .{ .backing_pattern = nominal.backing_pattern, .backing_type = nominal.backing_type } },
            .record_destructure => |record| .{ .record_destructure = node_store.sliceRecordDestructs(record.destructs) },
            .list => |list| .{ .list = .{
                .patterns = node_store.slicePatterns(list.patterns),
                .rest = if (list.rest_info) |rest| .{ .index = rest.index, .pattern = rest.pattern } else null,
            } },
            .tuple => |tuple| .{ .tuple = node_store.slicePatterns(tuple.patterns) },
            .num_literal, .num_from_numeral_literal, .small_dec_literal, .dec_literal => .numeral_literal,
            .frac_f32_literal, .frac_f64_literal => invariant("a float-valued literal pattern without exact digits reached a checked body"),
            .str_literal => |str| .{ .str_literal = str.literal },
            .str_interpolation => |str| .{ .str_interpolation = .{
                .prefix = str.prefix,
                .steps = str.steps,
                .end = switch (str.end) {
                    .exact => .exact,
                    .tail => .tail,
                },
            } },
            .underscore => .underscore,
            .runtime_error => .runtime_error,
            .deferred_import_ref => invariant("a deferred import reference pattern reached a checked body"),
        };
    }

    /// Whether a literal pattern's comparison dispatches to its type's
    /// `from_numeral` or `from_quote` and `is_eq` (`literalPatternGuard`).
    pub fn literalPatternIsCustom(self: CheckedBodies, id: PatternId) bool {
        const node = ModuleEnv.nodeIdxFrom(id);
        return switch (self.store().getPattern(id)) {
            .num_literal, .num_from_numeral_literal => customLiteralPlan(self.env.numeralDispatchPlanForNode(node)),
            inline .small_dec_literal, .dec_literal => |dec| !dec.has_suffix and customLiteralPlan(self.env.numeralDispatchPlanForNode(node)),
            .str_literal => customLiteralPlan(self.env.quoteDispatchPlanForNode(node)),
            .assign, .var_assign, .as, .applied_tag, .nominal, .nominal_external, .record_destructure, .list, .tuple, .frac_f32_literal, .frac_f64_literal, .str_interpolation, .underscore, .runtime_error, .deferred_import_ref => false,
        };
    }

    fn customLiteralPlan(plan: ?can.NodeStore.LiteralDispatchPlan) bool {
        const resolved = plan orelse return false;
        return switch (resolved.dispatchResolution()) {
            .builtin_direct => false,
            .custom_dispatch, .specialization_dispatch => true,
            .checked_error => invariant("a checked-error literal pattern reached a checked body"),
            .unresolved => invariant("an unresolved literal pattern reached a checked body"),
        };
    }

    pub fn statementData(self: CheckedBodies, id: StatementId) StatementData {
        const node_store = self.store();
        return switch (node_store.getStatement(id)) {
            .s_decl => |decl| blk: {
                const module_index = self.index();
                if (module_index.isRejectedMethodDecl(id)) break :blk .rejected_method_decl;
                if (module_index.isPromotedPattern(decl.pattern)) break :blk .{ .promoted_proc = .{ .pattern = decl.pattern, .expr = decl.expr } };
                break :blk .{ .decl = .{ .pattern = decl.pattern, .expr = decl.expr } };
            },
            .s_var => |var_| .{ .var_ = .{ .pattern = var_.pattern_idx, .expr = var_.expr } },
            .s_var_uninitialized => |var_| .{ .var_uninitialized = .{ .pattern = var_.pattern_idx } },
            .s_reassign => |reassign| .{ .reassign = .{ .pattern = reassign.pattern_idx, .expr = reassign.expr } },
            .s_crash => |crash| .{ .crash = crash.msg },
            .s_dbg => |dbg| .{ .dbg = dbg.expr },
            .s_expr => |expr_stmt| .{ .expr = expr_stmt.expr },
            .s_expect => |expect| .{ .expect = expect.body },
            .s_for => |for_| .{ .for_ = .{ .pattern = for_.patt, .expr = for_.expr, .body = for_.body } },
            .s_while => |while_| .{ .while_ = .{ .cond = while_.cond, .body = while_.body } },
            .s_infinite_loop => |loop| .{ .infinite_loop = .{ .cond = loop.cond, .body = loop.body } },
            .s_breakable_loop => |loop| .{ .breakable_loop = .{ .cond = loop.cond, .body = loop.body } },
            .s_break => .break_,
            .s_return => |ret| .{ .return_ = .{ .expr = ret.expr, .lambda = ret.lambda } },
            .s_import => .import_,
            .s_alias_decl => .alias_decl,
            .s_where_alias_decl => .where_alias_decl,
            .s_nominal_decl => .nominal_decl,
            .s_type_anno => .type_anno,
            .s_type_var_alias => .type_var_alias,
            // A statement that still evaluates its retired value expression
            // runs that expression, whose evaluation crashes.
            .s_runtime_error => |runtime_error| switch (runtime_error.evaluated.span.len) {
                0 => .runtime_error,
                1 => .{ .expr = node_store.sliceExpr(runtime_error.evaluated)[0] },
                else => invariant("a retired statement evaluated more than its value expression"),
            },
        };
    }

    /// The conversion of a custom literal pattern's literal at the pattern's
    /// type, or of a literal expression at its own type.
    pub fn literalConversion(self: CheckedBodies, raw_node: u32) LiteralConversion {
        const node: CIR.Node.Idx = @fromBackingInt(@intCast(raw_node));
        const plan = self.env.store.literalDispatchPlanForNode(node) orelse invariant("a literal conversion was requested for a node with no literal dispatch plan");
        return switch (plan.dispatchKind()) {
            .numeral => .{
                .literal = .{ .numeral = self.numeralLiteral(raw_node) },
                .plan = self.numeralPlan(raw_node),
                .conversion_root = self.literalConversionRoot(raw_node),
                .ty = self.typeOf(ModuleEnv.varFrom(node)),
            },
            .quote => .{
                .literal = .{ .quote = self.quoteText(raw_node) },
                .plan = self.quotePlan(raw_node),
                .conversion_root = self.literalConversionRoot(raw_node),
                .ty = self.typeOf(ModuleEnv.varFrom(node)),
            },
        };
    }

    /// The equality test of a literal pattern whose comparison dispatches to
    /// its type's `from_numeral` or `from_quote` and `is_eq`, or null for a
    /// pattern compared directly.
    pub fn literalPatternGuard(self: CheckedBodies, pattern_id: PatternId) ?LiteralPatternGuard {
        if (!self.literalPatternIsCustom(pattern_id)) return null;
        const raw = @backingInt(pattern_id);
        const node = ModuleEnv.nodeIdxFrom(pattern_id);
        const plan = self.env.store.literalDispatchPlanForNode(node) orelse invariant("a custom literal pattern had no literal dispatch plan");
        const context = plan.patternContext(&self.env.store) orelse invariant("a custom literal pattern had no pattern context");
        if (context.equality_fn_var_plus_one == 0) invariant("a custom literal pattern had no equality function");
        const equality_fn: Var = @fromBackingInt(@intCast(context.equality_fn_var_plus_one - 1));
        const equality_fn_ty = self.typeOf(equality_fn);
        return .{
            .pattern = pattern_id,
            .conversion = self.literalConversion(raw),
            .equality_plan = self.dispatchPlan(raw),
            .equality_ty = checked_artifact.checkedFunctionPayload(self.types, equality_fn_ty, "literal pattern equality").ret,
        };
    }

    /// A locally declared field default's expression (design.md "Defaulted
    /// Fields"), or null for a node this module declares no default on: a
    /// foreign default resolves in its declaring module's reader.
    pub fn defaultExpr(self: CheckedBodies, expr_node: u32) ?ExprId {
        const default: ExprId = @fromBackingInt(@intCast(expr_node));
        if (!self.index().isDefault(default)) return null;
        return default;
    }

    /// The defaulted-field expressions this module declares, ascending.
    pub fn defaultExprs(self: CheckedBodies) []const u32 {
        return self.index().defaults;
    }

    /// The checker-selected defaults of fields omitted at record
    /// construction sites, by site, in checking order.
    pub fn recordOmittedDefaults(self: CheckedBodies) []const ModuleEnv.RecordOmittedDefault {
        return self.env.record_omitted_defaults.items.items;
    }

    /// One omitted default in the checked module's names.
    pub fn recordOmittedDefault(self: CheckedBodies, omitted: ModuleEnv.RecordOmittedDefault) checked_artifact.CheckedRecordOmittedDefault {
        const origin = self.names.lookupModuleIdentity(self.env.moduleIdentityHash(omitted.origin_module)) orelse
            invariant("an omitted default's origin module was not interned");
        return .{
            .expr = omitted.expr,
            .field_name = self.recordFieldLabel(omitted.field_name),
            .default = checked_artifact.CheckedFieldDefault.fromParts(origin, omitted.default_expr_node),
        };
    }

    /// Whether evaluating an expression always diverges.
    pub fn exprDiverges(self: CheckedBodies, id: ExprId, mode: body_analyses.InlineExpectMode) Allocator.Error!bool {
        return self.analyses.exprDiverges(self, id, mode);
    }

    /// Whether executing a statement always diverges.
    pub fn statementDiverges(self: CheckedBodies, id: StatementId, mode: body_analyses.InlineExpectMode) Allocator.Error!bool {
        return self.analyses.statementDiverges(self, id, mode);
    }

    /// Whether evaluating an expression may be omitted when a consumer needs
    /// only its callable identity.
    pub fn exprEvaluationMayBeElidedForInspect(self: CheckedBodies, id: ExprId) Allocator.Error!bool {
        return self.analyses.exprEvaluationMayBeElidedForInspect(self, id);
    }

    /// Whether an expression or one of the nodes it evaluates or binds
    /// contains a checker-authored diagnostic error.
    pub fn exprContainsDiagnosticError(self: CheckedBodies, id: ExprId) Allocator.Error!bool {
        return self.analyses.exprContainsDiagnosticError(self, id);
    }

    /// Whether a pattern or one of the nodes it binds contains a
    /// checker-authored diagnostic error.
    pub fn patternContainsDiagnosticError(self: CheckedBodies, id: PatternId) Allocator.Error!bool {
        return self.analyses.patternContainsDiagnosticError(self, id);
    }

    /// Whether the body a compile-time root evaluates contains a
    /// checker-authored diagnostic error: its expression, the pattern a
    /// hoisted pattern root matches it against, or the literal a literal
    /// conversion root converts. A root that crashes for a
    /// rejected destructure binder always does.
    pub fn rootContainsDiagnosticError(self: CheckedBodies, root: checked_artifact.CompileTimeRoot) Allocator.Error!bool {
        if (root.hoisted_body) |body| if (body == .pattern_error) return true;
        const parts = root.bodyParts();
        if (parts.expr) |body_expr| if (try self.exprContainsDiagnosticError(body_expr)) return true;
        if (parts.pattern) |body_pattern| if (try self.patternContainsDiagnosticError(body_pattern)) return true;
        if (parts.literal) |literal| switch (literal) {
            .expr => |literal_expr| if (try self.exprContainsDiagnosticError(literal_expr)) return true,
            .pattern => |literal_pattern| if (try self.patternContainsDiagnosticError(literal_pattern)) return true,
        };
        return false;
    }

    /// The binders an `if`, `match`, or `for` expression reassigns anywhere
    /// inside it, in source order with each binder's first occurrence kept.
    /// An `expect` body can reassign only vars declared inside it, so the
    /// set is the same whether or not inline expects run.
    pub fn stateMergeBinders(self: CheckedBodies, control_flow: ExprId) Allocator.Error![]const BinderId {
        return self.analyses.stateMergeBinders(self, @backingInt(control_flow));
    }

    /// The binders an expression statement's expression reassigns.
    pub fn statementStateMergeBinders(self: CheckedBodies, expr_statement: StatementId) Allocator.Error![]const BinderId {
        return self.analyses.stateMergeBinders(self, @backingInt(expr_statement));
    }

    /// The binders a loop's condition and body reassign. `loop` is a `for`
    /// expression or a loop statement.
    pub fn loopMutationBinders(self: CheckedBodies, loop: u32) Allocator.Error![]const BinderId {
        return self.analyses.loopMutationBinders(self, loop);
    }

    /// Whether every call of a local template, in every program, is the one
    /// call the module's source makes of it.
    pub fn templateHasSingleSourceCall(self: CheckedBodies, template: canonical.CheckedProcedureTemplateId) Allocator.Error!bool {
        return self.analyses.templateHasSingleSourceCall(self, template);
    }

    /// Whether the module's source uses the function a nested procedure site
    /// constructs exactly once, as the function of a call.
    pub fn nestedProcSiteIsSingleSourceCall(self: CheckedBodies, site: canonical.NestedProcSiteId) Allocator.Error!bool {
        return self.analyses.nestedProcSiteIsSingleSourceCall(self, site);
    }

    pub fn matchBranch(self: CheckedBodies, id: CIR.Expr.Match.Branch.Idx) MatchBranch {
        const branch = self.store().getMatchBranch(id);
        return .{ .patterns = self.store().sliceMatchBranchPatterns(branch.patterns), .value = branch.value, .guard = branch.guard };
    }

    pub fn matchBranchPattern(self: CheckedBodies, id: CIR.Expr.Match.BranchPattern.Idx) MatchBranchPattern {
        const branch_pattern = self.store().getMatchBranchPattern(id);
        return .{ .pattern = branch_pattern.pattern, .degenerate = branch_pattern.degenerate };
    }

    pub fn ifBranch(self: CheckedBodies, id: CIR.Expr.IfBranch.Idx) IfBranch {
        const branch = self.store().getIfBranch(id);
        return .{ .cond = branch.cond, .body = branch.body };
    }

    /// A record construction field's value expression.
    pub fn recordFieldValue(self: CheckedBodies, id: CIR.RecordField.Idx) ExprId {
        return self.store().getRecordField(id).value;
    }

    pub fn recordField(self: CheckedBodies, id: CIR.RecordField.Idx) RecordExprField {
        const field = self.store().getRecordField(id);
        return .{ .label = self.recordFieldLabel(field.name), .value = field.value };
    }

    pub fn unsetField(self: CheckedBodies, id: CIR.UnsetField.Idx) canonical.RecordFieldLabelId {
        return self.recordFieldLabel(self.store().getUnsetField(id).name);
    }

    pub fn fieldAccessSegment(self: CheckedBodies, span: CIR.Expr.FieldAccessSegment.Span, position: u32) FieldAccessSegment {
        const node_store = self.store();
        const segment_idx = node_store.fieldAccessSegmentAt(span, position);
        const segment = node_store.getFieldAccessSegment(segment_idx);
        return .{
            .field_name = self.recordFieldLabel(segment.name),
            .success_ty = self.typeOf(ModuleEnv.varFrom(segment_idx)),
            .source_region = node_store.getFieldAccessSegmentRegion(segment_idx),
            .mode = switch (segment.mode) {
                .required => .required,
                .optional => .optional,
            },
        };
    }

    pub fn capture(self: CheckedBodies, id: CIR.Expr.Capture.Idx) Capture {
        const source = self.store().getCapture(id);
        return .{
            .pattern = source.pattern_idx,
            .scope_depth = source.scope_depth,
            .capture_id = checked_artifact.CaptureId.fromBinder(source.pattern_idx),
        };
    }

    /// The pattern a record destructure binds its field to.
    pub fn recordDestructPattern(self: CheckedBodies, id: CIR.Pattern.RecordDestruct.Idx) PatternId {
        return self.store().getRecordDestruct(id).kind.toPatternIdx();
    }

    pub fn recordDestruct(self: CheckedBodies, id: CIR.Pattern.RecordDestruct.Idx) RecordDestruct {
        const destruct = self.store().getRecordDestruct(id);
        return .{
            .label = self.recordFieldLabel(destruct.label),
            .kind = switch (destruct.kind) {
                .Required => |child| .{ .required = child },
                .SubPattern => |child| .{ .sub_pattern = child },
                .Rest => |child| .{ .rest = child },
            },
        };
    }

    pub fn strPatternStep(self: CheckedBodies, span: CIR.Pattern.StrPatternStep.Span, offset: u32) StrPatternStep {
        const step = self.store().getStrPatternStep(span, offset);
        return .{ .capture = step.capture, .delimiter = step.delimiter };
    }

    /// The binder `pattern` introduces.
    pub fn binder(self: CheckedBodies, pattern_id: BinderId) Binder {
        const module_index = self.index();
        return .{
            .pattern = pattern_id,
            .reassignable = self.store().getPattern(pattern_id) == .var_assign,
            .is_scheme_alias = module_index.isSchemeAlias(pattern_id),
            .is_promoted = module_index.isPromotedPattern(pattern_id),
            .recursive_value = module_index.isRecursiveBinder(pattern_id),
        };
    }

    /// Append the reassignable binders a reassigned pattern rebinds, in
    /// preorder, each once.
    pub fn appendReassignedBinders(self: CheckedBodies, allocator: Allocator, pattern_id: PatternId, out: *std.ArrayList(BinderId)) Allocator.Error!void {
        var pending: std.ArrayList(PatternId) = .empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, pattern_id);
        const node_store = self.store();
        const first = out.items.len;
        while (pending.pop()) |current| {
            const source = node_store.getPattern(current);
            if (source == .var_assign) {
                const seen = for (out.items[first..]) |existing| {
                    if (existing == current) break true;
                } else false;
                if (!seen) try out.append(allocator, current);
            }
            const start = pending.items.len;
            switch (source) {
                .as => |as| try pending.append(allocator, as.pattern),
                .applied_tag => |tag| try pending.appendSlice(allocator, node_store.slicePatterns(tag.args)),
                .nominal => |nominal| try pending.append(allocator, nominal.backing_pattern),
                .nominal_external => |nominal| try pending.append(allocator, nominal.backing_pattern),
                .record_destructure => |record| for (node_store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    try pending.append(allocator, node_store.getRecordDestruct(destruct_idx).kind.toPatternIdx());
                },
                .list => |list| {
                    try pending.appendSlice(allocator, node_store.slicePatterns(list.patterns));
                    if (list.rest_info) |rest| if (rest.pattern) |child| try pending.append(allocator, child);
                },
                .tuple => |tuple| try pending.appendSlice(allocator, node_store.slicePatterns(tuple.patterns)),
                .str_interpolation => |str| {
                    var offset: u32 = 0;
                    while (offset < str.steps.span.len) : (offset += 1) {
                        if (node_store.getStrPatternStep(str.steps, offset).capture) |child| try pending.append(allocator, child);
                    }
                },
                .assign, .var_assign, .num_literal, .num_from_numeral_literal, .small_dec_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal, .str_literal, .underscore, .runtime_error => {},
                .deferred_import_ref => invariant("a deferred import reference pattern reached a checked body"),
            }
            std.mem.reverse(PatternId, pending.items[start..]);
        }
    }

    /// The candidate binders of a non-degenerate alternative pattern, each
    /// paired with the binder of the branch's first pattern of the same
    /// name, appended to `out`.
    pub fn appendAlternativeBinderRemaps(
        self: CheckedBodies,
        allocator: std.mem.Allocator,
        representative: PatternId,
        candidate: PatternId,
        out: *std.ArrayList(checked_artifact.AlternativeBinderRemap),
    ) std.mem.Allocator.Error!void {
        var representatives: std.ArrayList(NamedBinder) = .empty;
        defer representatives.deinit(allocator);
        try self.appendNamedBinders(allocator, representative, &representatives);
        var candidates: std.ArrayList(NamedBinder) = .empty;
        defer candidates.deinit(allocator);
        try self.appendNamedBinders(allocator, candidate, &candidates);
        if (candidates.items.len != representatives.items.len) invariant("an alternative's binder count differs from its branch's first pattern");
        try out.ensureUnusedCapacity(allocator, candidates.items.len);
        for (candidates.items) |candidate_binder| {
            const match = for (representatives.items) |representative_binder| {
                if (representative_binder.ident.eql(candidate_binder.ident)) break representative_binder.pattern;
            } else invariant("an alternative's binder has no binder of the same name in its branch's first pattern");
            out.appendAssumeCapacity(.{ .candidate_binder = candidate_binder.pattern, .representative_binder = match });
        }
    }

    const NamedBinder = struct { ident: Ident.Idx, pattern: PatternId };

    /// The binders a pattern introduces, in preorder; an as-pattern's own
    /// binder follows its inner pattern's.
    fn appendNamedBinders(self: CheckedBodies, allocator: std.mem.Allocator, root: PatternId, out: *std.ArrayList(NamedBinder)) std.mem.Allocator.Error!void {
        const Pending = union(enum) { pattern: PatternId, as_binder: PatternId };
        var pending: std.ArrayList(Pending) = .empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, .{ .pattern = root });
        const node_store = self.store();
        while (pending.pop()) |item| {
            const current = switch (item) {
                .as_binder => |as_idx| {
                    try out.append(allocator, .{ .ident = node_store.getPattern(as_idx).as.ident, .pattern = as_idx });
                    continue;
                },
                .pattern => |current| current,
            };
            const source = node_store.getPattern(current);
            const start = pending.items.len;
            switch (source) {
                .assign => |assign| try out.append(allocator, .{ .ident = assign.ident, .pattern = current }),
                .var_assign => |assign| try out.append(allocator, .{ .ident = assign.ident, .pattern = current }),
                .as => |as| {
                    try pending.append(allocator, .{ .as_binder = current });
                    try pending.append(allocator, .{ .pattern = as.pattern });
                    continue;
                },
                .applied_tag => |tag| for (node_store.slicePatterns(tag.args)) |child| try pending.append(allocator, .{ .pattern = child }),
                .nominal => |nominal| try pending.append(allocator, .{ .pattern = nominal.backing_pattern }),
                .nominal_external => |nominal| try pending.append(allocator, .{ .pattern = nominal.backing_pattern }),
                .record_destructure => |record| for (node_store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    try pending.append(allocator, .{ .pattern = node_store.getRecordDestruct(destruct_idx).kind.toPatternIdx() });
                },
                .list => |list| {
                    for (node_store.slicePatterns(list.patterns)) |child| try pending.append(allocator, .{ .pattern = child });
                    if (list.rest_info) |rest| if (rest.pattern) |child| try pending.append(allocator, .{ .pattern = child });
                },
                .tuple => |tuple| for (node_store.slicePatterns(tuple.patterns)) |child| try pending.append(allocator, .{ .pattern = child }),
                .str_interpolation => |str| {
                    var offset: u32 = 0;
                    while (offset < str.steps.span.len) : (offset += 1) {
                        if (node_store.getStrPatternStep(str.steps, offset).capture) |child| try pending.append(allocator, .{ .pattern = child });
                    }
                },
                .num_literal, .num_from_numeral_literal, .small_dec_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal, .str_literal, .underscore, .runtime_error => {},
                .deferred_import_ref => invariant("a deferred import reference pattern reached a checked body"),
            }
            std.mem.reverse(Pending, pending.items[start..]);
        }
    }
};
