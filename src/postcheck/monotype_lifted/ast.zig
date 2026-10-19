//! Monotype Lifted IR.
//!
//! This stage uses the Monotype type store, but no expression-position lambda
//! remains. Every function body is stored as a lifted function with explicit
//! capture locals.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const names = check.CheckedNames;
const TypeDigestHasher = base.TypeDigestHasher;
const GuardedList = collections.GuardedList;
const BodyShard = @import("body_shard.zig");

/// Guarded growable list for mutable Monotype Lifted program storage.
pub fn ProgramList(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.List(T, "monotype_lifted.Program." ++ field_name);
}

/// Guarded immutable span borrow for a named Monotype Lifted program list.
pub fn ProgramSpanBorrow(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpan(T, "monotype_lifted.Program." ++ field_name);
}

/// Identifier for an expression in Monotype Lifted IR.
pub const ExprId = Mono.ExprId;
/// Identifier for a pattern in Monotype Lifted IR.
pub const PatId = Mono.PatId;
/// Identifier for a statement in Monotype Lifted IR.
pub const StmtId = Mono.StmtId;
/// Identifier for a lifted function body.
pub const FnId = Mono.LiftedFnId;

/// Slice descriptor shared with Monotype IR.
pub const Span = Mono.Span;
/// Local binding id shared with Monotype IR.
pub const LocalId = Mono.LocalId;
/// Lexically scoped lifted join-point identity.
pub const JoinPointId = Mono.JoinPointId;
/// Local binding shared with Monotype IR.
pub const Local = Mono.Local;
/// Local id paired with a monomorphic type.
pub const TypedLocal = Mono.TypedLocal;
/// Owned string literal id shared with Monotype IR.
pub const StringLiteralId = Mono.StringLiteralId;
/// Compile-time site id shared with Monotype IR.
pub const ComptimeSiteId = Mono.ComptimeSiteId;
/// Compile-time site kind shared with Monotype IR.
pub const ComptimeSiteKind = Mono.ComptimeSiteKind;
/// Compile-time site metadata shared with Monotype IR.
pub const ComptimeSite = Mono.ComptimeSite;
/// Record field expression entry.
pub const FieldExpr = Mono.FieldExpr;
/// Record update expression.
pub const RecordUpdate = Mono.RecordUpdate;
/// Explicit producer-to-consumer result relation.
pub const TypedBoundary = Mono.TypedBoundary;
/// One source-ordered segment in a flattened record-field access path.
pub const FieldAccessSegment = Mono.FieldAccessSegment;
/// Keyed pre-lift function capture operand.
pub const FnDefCapture = Mono.FnDefCapture;
/// Keyed lifted capture operand (CaptureId + supplying expression).
pub const CaptureOperand = Mono.CaptureOperand;
/// Record destructuring field pattern.
pub const RecordDestruct = Mono.RecordDestruct;
/// Compiler-generated initialized-payload switch shared with Monotype IR.
pub const InitializedPayloadSwitch = Mono.InitializedPayloadSwitch;
/// List destructuring pattern.
pub const ListPattern = Mono.ListPattern;
/// `..`/`.. as name` portion of a list pattern.
pub const ListRestPattern = Mono.ListRestPattern;
/// Typed shared continuation introduced by lifted optimization.
pub const JoinPointExpr = Mono.JoinPointExpr;
/// Transfer to a lexically enclosing lifted join point.
pub const JumpExpr = Mono.JumpExpr;

/// Typed Monotype Lifted expression.
pub const Expr = Mono.Expr;

/// Monotype Lifted expression forms.
pub const ExprData = Mono.ExprData;

/// Typed Monotype Lifted pattern.
pub const Pat = Mono.Pat;

/// Monotype Lifted pattern forms.
pub const PatData = Mono.PatData;
/// Monotype Lifted string interpolation pattern.
pub const StrPattern = Mono.StrPattern;
/// Monotype Lifted delimited capture step inside a string interpolation pattern.
pub const StrPatternStep = Mono.StrPatternStep;
/// Monotype Lifted end behavior for a string interpolation pattern.
pub const StrPatternEnd = Mono.StrPatternEnd;

/// Match branch.
pub const Branch = Mono.Branch;

/// Conditional branch in an if expression.
pub const IfBranch = Mono.IfBranch;

/// Monotype Lifted statement forms.
pub const Stmt = Mono.Stmt;

/// Lifted function body with explicit captures.
pub const Fn = struct {
    symbol: Common.Symbol,
    source: ?Mono.FnTemplate = null,
    /// Exact producer-authored Monotype signature graph. This is present only
    /// while the lifted function still has that signature; transformations
    /// that synthesize a different ABI clear it explicitly.
    signature: ?Type.TypeId = null,
    /// The iterator-fusion pass normalized this function from explicit
    /// checker-stamped iterator producers. Later structural LIR passes consume
    /// this provenance directly instead of rediscovering iterator intent from
    /// procedure names or body shape.
    iterator_fusion_scope: bool = false,
    /// Digest of the SpecConstr call pattern this function was cloned for, or
    /// null for a function that is not a call-pattern clone. Clones share
    /// their source's checked template, so this is what keeps two clones of
    /// one function at different argument shapes distinct.
    spec_constr_pattern: ?names.TypeDigest = null,
    /// Content identity of a function lifted from a Monotype definition that
    /// has no function template (`Mono.Def.root_identity`). Null when
    /// `source` is present.
    root_identity: ?names.TypeDigest = null,
    args: Span(TypedLocal),
    captures: Span(TypedLocal),
    body: FnBody,
    ret: Type.TypeId,
    /// What the body contains, for SpecConstr phase admission.
    shapes: FnShapes = .{},
};

/// What a lifted function body contains, recorded by whoever emitted its
/// expressions: the lifter as it rewrites each expression of a body, and the
/// program's expression creation for bodies cloned afterwards. SpecConstr
/// selects the functions each of its phases can change by these shapes
/// instead of walking every body. A shape flag is a superset: it may be set for a
/// body a phase then leaves alone, but a body a phase would change always
/// carries the shape flag, and Debug builds verify that by also running each phase
/// on the functions its shapes excluded.
pub const FnShapes = packed struct(u8) {
    /// A direct call.
    direct_call: bool = false,
    /// A tag, record, tuple, nominal, list, closure or compile-time value
    /// construction: the only sources of a known-shaped call argument.
    constructs_value: bool = false,
    /// A direct call to an iterator procedure.
    iterator_call: bool = false,
    /// A direct call to an iterator procedure that produces an iterator value.
    iterator_producer: bool = false,
    /// A loop.
    loop: bool = false,
    /// A loop whose result is a tuple of at least two values.
    loop_tuple_result: bool = false,
    /// A direct call to the function itself.
    self_call: bool = false,
    /// A `return` expression.
    contains_return: bool = false,

    pub fn merged(self: FnShapes, other: FnShapes) FnShapes {
        return @bitCast(@as(u8, @bitCast(self)) | @as(u8, @bitCast(other)));
    }
};

/// Source procedure names for runtime diagnostics, keyed by generated symbol.
pub const ProcDebugNameMap = Mono.ProcDebugNameMap;

/// Body availability for a lifted function.
pub const FnBody = union(enum) {
    roc: ExprId,
    hosted,
};

/// Root request bound to a lifted function.
pub const Root = struct {
    fn_id: FnId,
    request: check.CheckedModule.RootRequest,
    /// See `Mono.Root.owner`.
    owner: Common.LoweringModuleId,
};

/// See `Mono.LiteralRoot`.
pub const LiteralRoot = struct {
    fn_id: FnId,
    module: check.CheckedModule.ModuleId,
    site: Common.LiteralRejectionSite,
};

/// Runtime layout requested for a checked data value.
pub const LayoutRequest = struct {
    checked_type: check.CheckedModule.CheckedTypeId,
    ty: Type.TypeId,
    fn_id: ?FnId = null,
    const_locator: ?check.CheckedModule.ConstLocator = null,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = Mono.RuntimeSchemaRequest;
/// Request to make a lifted value available as static data.
pub const StaticDataValue = Mono.StaticDataValue;
/// A virtual source frame introduced by post-check inlining.
pub const InlineScopeId = enum(u32) {
    _,

    pub const none: InlineScopeId = @fromBackingInt(@intCast(std.math.maxInt(u32)));
};

/// One source-level procedure frame retained across post-check inlining.
pub const InlineScope = struct {
    source_symbol: Common.Symbol,
    source_loc: base.SourceLoc,
    call_site: base.SourceLoc,
    parent: InlineScopeId = InlineScopeId.none,
};

/// Read-only Monotype Lifted program view.
///
/// This view borrows `Program` arrays. Lambda Solved consumes this shape so a
/// builder split does not require a consumer rewrite.
pub const ProgramView = struct {
    names: *const names.NameStore,
    next_symbol: u32,
    types: Type.Store.View,
    fns: []const Fn,
    const_fn_evidence: []const check.ConstStore.ConstFnEvidence,
    const_fn_evidence_frames: []const check.ConstStore.ConstFnEvidenceFrame,
    exprs: []const Expr,
    pats: []const Pat,
    stmts: []const Stmt,
    locals: []const Local,
    expr_ids: []const ExprId,
    pat_ids: []const PatId,
    typed_locals: []const TypedLocal,
    stmt_ids: []const StmtId,
    field_exprs: []const FieldExpr,
    field_access_segments: []const FieldAccessSegment,
    fn_def_captures: []const FnDefCapture,
    capture_operands: []const CaptureOperand,
    record_destructs: []const RecordDestruct,
    str_pattern_steps: []const Mono.StrPatternStep,
    branches: []const Branch,
    if_branches: []const IfBranch,
    string_literals: []const Mono.StringLiteral,
    proc_debug_names: *const ProcDebugNameMap,
    roots: []const Root,
    literal_roots: []const LiteralRoot,
    layout_requests: []const LayoutRequest,
    /// Evaluated roots this program reads a completed value of, recorded once
    /// each by Monotype lowering and carried unchanged.
    comptime_value_reads: []const Common.ComptimeValueRoot,
    runtime_schema_requests: []const RuntimeSchemaRequest,
    static_data_values: []const StaticDataValue,
    comptime_value_roots: []const Common.ComptimeValueRoot,
    comptime_sites: []const ComptimeSite,
    /// See `Mono.ProgramBuilder.lowering_modules`.
    lowering_modules: []const check.CheckedModule.ModuleId,
    platform_requirement_filling: ?Common.PlatformRequirementFilling,
    source_files: []const base.SourceFileEntry,
    expr_locs: []const base.SourceLoc,
    expr_regions: []const base.Region,
    stmt_locs: []const base.SourceLoc,
    stmt_regions: []const base.Region,
    inline_scopes: []const InlineScope,
    expr_inline_scopes: []const InlineScopeId,
    stmt_inline_scopes: []const InlineScopeId,
    local_names: []const Mono.StringLiteral,

    pub fn getComptimeValueRoot(self: ProgramView, id: Common.ComptimeValueRootId) Common.ComptimeValueRoot {
        return self.comptime_value_roots[@backingInt(id)];
    }

    pub fn procDebugName(self: ProgramView, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    pub fn exprLoc(self: ProgramView, id: ExprId) base.SourceLoc {
        return self.expr_locs[@backingInt(id)];
    }

    pub fn exprRegion(self: ProgramView, id: ExprId) base.Region {
        return self.expr_regions[@backingInt(id)];
    }

    pub fn stmtLoc(self: ProgramView, id: StmtId) base.SourceLoc {
        return self.stmt_locs[@backingInt(id)];
    }

    pub fn stmtRegion(self: ProgramView, id: StmtId) base.Region {
        return self.stmt_regions[@backingInt(id)];
    }

    pub fn comptimeSite(self: ProgramView, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites[@backingInt(id)];
    }

    pub fn localName(self: ProgramView, id: LocalId) []const u8 {
        return self.local_names[@backingInt(id)].text();
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: ProgramView, id: LocalId) check.CheckedModule.CaptureId {
        return self.locals[@backingInt(id)].capture_id orelse
            Common.invariant("lifted capture local had no CaptureId");
    }

    pub fn exprSpan(self: ProgramView, span_: Span(ExprId)) []const ExprId {
        return self.expr_ids[span_.start..][0..span_.len];
    }

    pub fn patSpan(self: ProgramView, span_: Span(PatId)) []const PatId {
        return self.pat_ids[span_.start..][0..span_.len];
    }

    pub fn typedLocalSpan(self: ProgramView, span_: Span(TypedLocal)) []const TypedLocal {
        return self.typed_locals[span_.start..][0..span_.len];
    }

    pub fn captureOperandSpan(self: ProgramView, span_: Span(CaptureOperand)) []const CaptureOperand {
        return self.capture_operands[span_.start..][0..span_.len];
    }

    pub fn stmtSpan(self: ProgramView, span_: Span(StmtId)) []const StmtId {
        return self.stmt_ids[span_.start..][0..span_.len];
    }

    pub fn fieldExprSpan(self: ProgramView, span_: Span(FieldExpr)) []const FieldExpr {
        return self.field_exprs[span_.start..][0..span_.len];
    }

    pub fn fieldAccessSegmentSpan(self: ProgramView, span_: Span(FieldAccessSegment)) []const FieldAccessSegment {
        return self.field_access_segments[span_.start..][0..span_.len];
    }

    pub fn recordDestructSpan(self: ProgramView, span_: Span(RecordDestruct)) []const RecordDestruct {
        return self.record_destructs[span_.start..][0..span_.len];
    }

    pub fn strPatternStepSpan(self: ProgramView, span_: Span(Mono.StrPatternStep)) []const Mono.StrPatternStep {
        return self.str_pattern_steps[span_.start..][0..span_.len];
    }

    pub fn branchSpan(self: ProgramView, span_: Span(Branch)) []const Branch {
        return self.branches[span_.start..][0..span_.len];
    }

    pub fn getExpr(self: ProgramView, id: ExprId) Expr {
        return self.exprs[@backingInt(id)];
    }

    pub fn getPat(self: ProgramView, id: PatId) Pat {
        return self.pats[@backingInt(id)];
    }

    pub fn getFn(self: ProgramView, id: FnId) Fn {
        return self.fns[@backingInt(id)];
    }

    pub fn ifBranchSpan(self: ProgramView, span_: Span(IfBranch)) []const IfBranch {
        return self.if_branches[span_.start..][0..span_.len];
    }
};

/// Direct call target after Monotype lifting.
pub const DirectCallee = union(enum(u8)) {
    local: FnId,
};

/// Return the lifted direct-call target after Monotype lifting.
pub fn directCallee(call: Mono.CallProc) DirectCallee {
    return switch (call.callee) {
        .lifted => |fn_id| .{ .local = fn_id },
        .func => Common.invariant("Monotype Lifted direct call still referenced a Monotype function id"),
    };
}

/// Return the local lifted function id for a direct call.
pub fn localDirectCallee(call: Mono.CallProc) ?FnId {
    return switch (directCallee(call)) {
        .local => |fn_id| fn_id,
    };
}

/// Which functions' code reaches the app filling a platform requirement from
/// outside it, what each depends on there, and which app functions it
/// reaches (`Program.fnReachesApp`).
pub const AppDependencies = struct {
    reaching: std.DynamicBitSetUnmanaged,
    /// Per function, meaningful for a reaching one: the digest of what the
    /// app functions it reaches, directly or through other functions, make
    /// its code depend on. A closed app function named by site contributes
    /// its own source identity, so editing its body leaves the digest
    /// unchanged; any other contributes the platform/app relation.
    digests: [][32]u8,
    /// One column per app function referenced from outside the app, set in
    /// the row of each function reaching it.
    columns: std.DynamicBitSetUnmanaged,
    column_callees: []FnId,

    pub fn deinit(self: *AppDependencies, allocator: std.mem.Allocator) void {
        self.reaching.deinit(allocator);
        allocator.free(self.digests);
        self.columns.deinit(allocator);
        allocator.free(self.column_callees);
    }
};

/// Complete Monotype Lifted program plus side arrays.
pub const Program = struct {
    /// A shard owns only its raw list suffixes. Source rows remain immutable
    /// until all workers have stopped reading; row commits use saved boundaries.
    body_prefix: ?BodyShard.Prefix = null,
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    fns: ProgramList(Fn, "fns"),
    const_fn_evidence: ProgramList(check.ConstStore.ConstFnEvidence, "const_fn_evidence"),
    const_fn_evidence_frames: ProgramList(check.ConstStore.ConstFnEvidenceFrame, "const_fn_evidence_frames"),
    exprs: ProgramList(Expr, "exprs"),
    pats: ProgramList(Pat, "pats"),
    stmts: ProgramList(Stmt, "stmts"),
    locals: ProgramList(Local, "locals"),
    expr_ids: ProgramList(ExprId, "expr_ids"),
    pat_ids: ProgramList(PatId, "pat_ids"),
    typed_locals: ProgramList(TypedLocal, "typed_locals"),
    stmt_ids: ProgramList(StmtId, "stmt_ids"),
    field_exprs: ProgramList(FieldExpr, "field_exprs"),
    field_access_segments: ProgramList(FieldAccessSegment, "field_access_segments"),
    fn_def_captures: ProgramList(FnDefCapture, "fn_def_captures"),
    /// Backing pool for `Span(CaptureOperand)` capture operand spans on lifted
    /// `fn_ref`/`call_proc` nodes.
    capture_operands: ProgramList(CaptureOperand, "capture_operands"),
    record_destructs: ProgramList(RecordDestruct, "record_destructs"),
    str_pattern_steps: ProgramList(Mono.StrPatternStep, "str_pattern_steps"),
    branches: ProgramList(Branch, "branches"),
    if_branches: ProgramList(IfBranch, "if_branches"),
    string_literals: ProgramList(Mono.StringLiteral, "string_literals"),
    proc_debug_names: ProcDebugNameMap,
    /// Next generated `CaptureId` index for a lift-synthesized capturable local.
    next_lift_capture_id: u32,
    /// Shapes of the expressions created or rewritten since the accumulator was
    /// last started; `beginFnShapes`/`finishFnShapes` bracket one body.
    shapes: FnShapes = .{},
    /// The function whose body the accumulator is collecting for, so a call
    /// to it is recorded as a self call.
    shapes_owner: ?FnId = null,
    roots: ProgramList(Root, "roots"),
    literal_roots: ProgramList(LiteralRoot, "literal_roots"),
    layout_requests: ProgramList(LayoutRequest, "layout_requests"),
    /// See `ProgramView.comptime_value_reads`.
    comptime_value_reads: ProgramList(Common.ComptimeValueRoot, "comptime_value_reads"),
    runtime_schema_requests: ProgramList(RuntimeSchemaRequest, "runtime_schema_requests"),
    static_data_values: ProgramList(StaticDataValue, "static_data_values"),
    /// Frozen shared metadata for SpecConstr; workers never append descriptors.
    comptime_value_roots: ProgramList(Common.ComptimeValueRoot, "comptime_value_roots") = .empty,
    comptime_sites: ProgramList(ComptimeSite, "comptime_sites"),
    /// Checked modules of this lowering's input, moved from Monotype and
    /// addressed by `Common.LoweringModuleId`.
    lowering_modules: ProgramList(check.CheckedModule.ModuleId, "lowering_modules") = .empty,
    /// See `Common.PlatformRequirementFilling` (moved from Monotype).
    platform_requirement_filling: ?Common.PlatformRequirementFilling = null,
    /// Functions whose code reaches the app from outside it, computed on
    /// first use by `fnReachesApp`.
    app_dependencies: ?AppDependencies = null,
    /// Source file table for `SourceLoc.file` indices (moved from Monotype).
    source_files: ProgramList(base.SourceFileEntry, "source_files"),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: ProgramList(base.SourceLoc, "expr_locs"),
    /// Checked source region per expression, parallel to `exprs`.
    expr_regions: ProgramList(base.Region, "expr_regions"),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: ProgramList(base.SourceLoc, "stmt_locs"),
    /// Checked source region per statement, parallel to `stmts`.
    stmt_regions: ProgramList(base.Region, "stmt_regions"),
    /// Interned virtual source frames introduced by inlining.
    inline_scopes: ProgramList(InlineScope, "inline_scopes"),
    /// Virtual inline scope per expression, parallel to `exprs`.
    expr_inline_scopes: ProgramList(InlineScopeId, "expr_inline_scopes"),
    /// Virtual inline scope per statement, parallel to `stmts`.
    stmt_inline_scopes: ProgramList(InlineScopeId, "stmt_inline_scopes"),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; moved from Monotype).
    local_names: ProgramList(Mono.StringLiteral, "local_names"),
    /// Ambient location recorded by `addExpr`/`addStmt`. Passes that add
    /// nodes set this so synthetic nodes inherit a source location.
    current_loc: base.SourceLoc,
    /// Ambient checked source region recorded by `addExpr`/`addStmt`.
    current_region: base.Region,
    /// Ambient virtual source frame recorded by `addExpr`/`addStmt`.
    current_inline_scope: InlineScopeId,

    /// Borrow a frozen program without copying unrelated bodies or type/name stores.
    /// The source must outlive the shard and remain frozen while it is queried.
    pub fn cloneForSpecConstrBody(self: *const Program, allocator: std.mem.Allocator, source_fn: FnId) std.mem.Allocator.Error!Program {
        std.debug.assert(self.body_prefix == null);
        var result: Program = undefined;
        inline for (BodyShard.all_fields) |field| @field(result, field) = .empty;
        result.body_prefix = BodyShard.Prefix.init(self, source_fn);
        result.allocator = allocator;
        result.names = self.names.borrowReadOnly(allocator);
        result.types = self.types.borrowReadOnly(allocator);
        result.next_symbol = self.next_symbol;
        result.next_lift_capture_id = self.next_lift_capture_id;
        result.shapes = .{};
        result.shapes_owner = null;
        result.proc_debug_names = ProcDebugNameMap.init(allocator);
        result.current_loc = self.current_loc;
        result.current_region = self.current_region;
        result.current_inline_scope = self.current_inline_scope;
        result.platform_requirement_filling = self.platform_requirement_filling;
        result.app_dependencies = null;
        return result;
    }

    /// Commit one completed private body in coordinator order. Failure leaves
    /// all logical rows and the selected function unchanged.
    pub fn appendSpecConstrBody(self: *Program, worker: *const Program, source_symbol_start: u32, symbol_offset: u32, source_join_start: u32, join_offset: u32) std.mem.Allocator.Error!void {
        return BodyShard.append(self, worker, source_symbol_start, symbol_offset, source_join_start, join_offset);
    }

    /// Whether an expression belongs to the frozen source a body shard
    /// borrows, and therefore never changes while the shard is queried.
    pub fn isFrozenExpr(self: *const Program, id: ExprId) bool {
        return @backingInt(id) < self.prefixLen("exprs");
    }

    fn prefixLen(self: *const Program, comptime field: []const u8) usize {
        return if (self.body_prefix) |prefix| prefix.len(field) else 0;
    }

    fn rowCount(self: *const Program, comptime field: []const u8) usize {
        return self.prefixLen(field) + @field(self, field).len();
    }

    fn row(self: *const Program, comptime field: []const u8, index: usize) @TypeOf(@field(self, field).get(0)) {
        const prefix_len = self.prefixLen(field);
        if (index < prefix_len) return @field(self.body_prefix.?.source, field).get(index);
        return @field(self, field).get(index - prefix_len);
    }

    fn ownedIndex(self: *const Program, comptime field: []const u8, index: usize) usize {
        const prefix_len = self.prefixLen(field);
        if (index < prefix_len) Common.invariant("body shard attempted to mutate frozen source");
        return index - prefix_len;
    }

    fn bodySpan(self: *const Program, comptime T: type, comptime field: []const u8, span_: Span(T)) ProgramSpanBorrow(T, field) {
        const prefix_len = self.prefixLen(field);
        if (span_.start < prefix_len) {
            std.debug.assert(span_.len <= prefix_len - span_.start);
            return @field(self.body_prefix.?.source, field).borrowSpan(span_.start, span_.len);
        }
        // Empty spans carry no identity and may start at zero.
        const start = if (span_.len == 0) 0 else span_.start - prefix_len;
        return @field(self, field).borrowSpan(start, span_.len);
    }

    fn appendBodySpan(self: *Program, comptime T: type, comptime field: []const u8, values: []const T) std.mem.Allocator.Error!Span(T) {
        var span_ = try Common.appendSpan(T, &@field(self, field), self.allocator, values);
        span_.start += @intCast(self.prefixLen(field));
        return span_;
    }

    /// Append-only section lengths at the start of speculative SpecConstr work.
    ///
    /// Value-aware pattern discovery evaluates expressions symbolically through
    /// the same cloner used by the mutating rewrite, and loop-shape discovery may
    /// reject a complete clone attempt. Their temporary ids must not become
    /// durable lifted IR.
    pub const SpecConstrAnalysisMark = struct {
        fns: usize,
        exprs: usize,
        pats: usize,
        stmts: usize,
        locals: usize,
        expr_ids: usize,
        pat_ids: usize,
        typed_locals: usize,
        stmt_ids: usize,
        field_exprs: usize,
        field_access_segments: usize,
        capture_operands: usize,
        record_destructs: usize,
        str_pattern_steps: usize,
        branches: usize,
        if_branches: usize,
        expr_locs: usize,
        expr_regions: usize,
        stmt_locs: usize,
        stmt_regions: usize,
        inline_scopes: usize,
        expr_inline_scopes: usize,
        stmt_inline_scopes: usize,
        local_names: usize,
        string_literals: usize,
        fn_def_captures: usize,
        comptime_value_roots: usize,
        body_patch: ?Fn = null,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: names.NameStore,
        types: Type.Store,
        const_fn_evidence: std.ArrayList(check.ConstStore.ConstFnEvidence),
        const_fn_evidence_frames: std.ArrayList(check.ConstStore.ConstFnEvidenceFrame),
        exprs: std.ArrayList(Expr),
        pats: std.ArrayList(Pat),
        stmts: std.ArrayList(Stmt),
        locals: std.ArrayList(Local),
        expr_ids: std.ArrayList(ExprId),
        pat_ids: std.ArrayList(PatId),
        typed_locals: std.ArrayList(TypedLocal),
        stmt_ids: std.ArrayList(StmtId),
        field_exprs: std.ArrayList(FieldExpr),
        field_access_segments: std.ArrayList(FieldAccessSegment),
        fn_def_captures: std.ArrayList(FnDefCapture),
        capture_operands: std.ArrayList(CaptureOperand),
        record_destructs: std.ArrayList(RecordDestruct),
        str_pattern_steps: std.ArrayList(Mono.StrPatternStep),
        branches: std.ArrayList(Branch),
        if_branches: std.ArrayList(IfBranch),
        string_literals: std.ArrayList(Mono.StringLiteral),
        proc_debug_names: ProcDebugNameMap,
        source_files: std.ArrayList(base.SourceFileEntry),
        expr_locs: std.ArrayList(base.SourceLoc),
        expr_regions: std.ArrayList(base.Region),
        stmt_locs: std.ArrayList(base.SourceLoc),
        stmt_regions: std.ArrayList(base.Region),
        inline_scopes: std.ArrayList(InlineScope),
        expr_inline_scopes: std.ArrayList(InlineScopeId),
        stmt_inline_scopes: std.ArrayList(InlineScopeId),
        local_names: std.ArrayList(Mono.StringLiteral),
        static_data_values: std.ArrayList(StaticDataValue),
        comptime_sites: std.ArrayList(ComptimeSite),
        next_symbol: u32,
    ) Program {
        const first_synthesized_capture_index: u32 = @intCast(locals.items.len);
        return .{
            .allocator = allocator,
            .names = name_store,
            .next_symbol = next_symbol,
            .types = types,
            .fns = .empty,
            .const_fn_evidence = ProgramList(check.ConstStore.ConstFnEvidence, "const_fn_evidence").fromArrayList(const_fn_evidence),
            .const_fn_evidence_frames = ProgramList(check.ConstStore.ConstFnEvidenceFrame, "const_fn_evidence_frames").fromArrayList(const_fn_evidence_frames),
            .exprs = ProgramList(Expr, "exprs").fromArrayList(exprs),
            .pats = ProgramList(Pat, "pats").fromArrayList(pats),
            .stmts = ProgramList(Stmt, "stmts").fromArrayList(stmts),
            .locals = ProgramList(Local, "locals").fromArrayList(locals),
            .expr_ids = ProgramList(ExprId, "expr_ids").fromArrayList(expr_ids),
            .pat_ids = ProgramList(PatId, "pat_ids").fromArrayList(pat_ids),
            .typed_locals = ProgramList(TypedLocal, "typed_locals").fromArrayList(typed_locals),
            .stmt_ids = ProgramList(StmtId, "stmt_ids").fromArrayList(stmt_ids),
            .field_exprs = ProgramList(FieldExpr, "field_exprs").fromArrayList(field_exprs),
            .field_access_segments = ProgramList(FieldAccessSegment, "field_access_segments").fromArrayList(field_access_segments),
            .fn_def_captures = ProgramList(FnDefCapture, "fn_def_captures").fromArrayList(fn_def_captures),
            .capture_operands = ProgramList(CaptureOperand, "capture_operands").fromArrayList(capture_operands),
            .record_destructs = ProgramList(RecordDestruct, "record_destructs").fromArrayList(record_destructs),
            .str_pattern_steps = ProgramList(Mono.StrPatternStep, "str_pattern_steps").fromArrayList(str_pattern_steps),
            .branches = ProgramList(Branch, "branches").fromArrayList(branches),
            .if_branches = ProgramList(IfBranch, "if_branches").fromArrayList(if_branches),
            .string_literals = ProgramList(Mono.StringLiteral, "string_literals").fromArrayList(string_literals),
            .proc_debug_names = proc_debug_names,
            // Final Monotype locals use generatedLift(LocalId), so ids minted
            // by lifting and spec_constr begin after the entire input arena.
            .next_lift_capture_id = first_synthesized_capture_index,
            .roots = .empty,
            .literal_roots = .empty,
            .layout_requests = .empty,
            .comptime_value_reads = .empty,
            .runtime_schema_requests = .empty,
            .static_data_values = ProgramList(StaticDataValue, "static_data_values").fromArrayList(static_data_values),
            .comptime_sites = ProgramList(ComptimeSite, "comptime_sites").fromArrayList(comptime_sites),
            .source_files = ProgramList(base.SourceFileEntry, "source_files").fromArrayList(source_files),
            .expr_locs = ProgramList(base.SourceLoc, "expr_locs").fromArrayList(expr_locs),
            .expr_regions = ProgramList(base.Region, "expr_regions").fromArrayList(expr_regions),
            .stmt_locs = ProgramList(base.SourceLoc, "stmt_locs").fromArrayList(stmt_locs),
            .stmt_regions = ProgramList(base.Region, "stmt_regions").fromArrayList(stmt_regions),
            .inline_scopes = ProgramList(InlineScope, "inline_scopes").fromArrayList(inline_scopes),
            .expr_inline_scopes = ProgramList(InlineScopeId, "expr_inline_scopes").fromArrayList(expr_inline_scopes),
            .stmt_inline_scopes = ProgramList(InlineScopeId, "stmt_inline_scopes").fromArrayList(stmt_inline_scopes),
            .local_names = ProgramList(Mono.StringLiteral, "local_names").fromArrayList(local_names),
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
            .current_inline_scope = InlineScopeId.none,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.unsafeRawItemsForView()) |name| name.deinit(self.allocator);
        self.local_names.deinit(self.allocator);
        self.stmt_inline_scopes.deinit(self.allocator);
        self.expr_inline_scopes.deinit(self.allocator);
        self.inline_scopes.deinit(self.allocator);
        self.stmt_regions.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.expr_regions.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        for (self.source_files.unsafeRawItemsForView()) |file| {
            self.allocator.free(file.name);
            self.allocator.free(file.qualified_name);
        }
        self.source_files.deinit(self.allocator);
        self.lowering_modules.deinit(self.allocator);
        if (self.app_dependencies) |*dependencies| dependencies.deinit(self.allocator);
        for (self.comptime_sites.unsafeRawItemsForView()) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.comptime_value_roots.deinit(self.allocator);
        self.static_data_values.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.comptime_value_reads.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.literal_roots.deinit(self.allocator);
        self.proc_debug_names.deinit();
        for (self.string_literals.unsafeRawItemsForView()) |literal| literal.deinit(self.allocator);
        self.string_literals.deinit(self.allocator);
        self.if_branches.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.str_pattern_steps.deinit(self.allocator);
        self.record_destructs.deinit(self.allocator);
        self.fn_def_captures.deinit(self.allocator);
        self.capture_operands.deinit(self.allocator);
        self.field_access_segments.deinit(self.allocator);
        self.field_exprs.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.typed_locals.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        self.locals.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.pats.deinit(self.allocator);
        self.exprs.deinit(self.allocator);
        self.fns.deinit(self.allocator);
        self.const_fn_evidence.deinit(self.allocator);
        self.const_fn_evidence_frames.deinit(self.allocator);
        self.types.deinit();
        self.names.deinit();
    }

    pub fn view(self: *const Program) ProgramView {
        std.debug.assert(self.body_prefix == null);
        return .{
            .names = &self.names,
            .next_symbol = self.next_symbol,
            .types = self.types.view(),
            .fns = self.fns.unsafeRawItemsForView(),
            .const_fn_evidence = self.const_fn_evidence.unsafeRawItemsForView(),
            .const_fn_evidence_frames = self.const_fn_evidence_frames.unsafeRawItemsForView(),
            .exprs = self.exprs.unsafeRawItemsForView(),
            .pats = self.pats.unsafeRawItemsForView(),
            .stmts = self.stmts.unsafeRawItemsForView(),
            .locals = self.locals.unsafeRawItemsForView(),
            .expr_ids = self.expr_ids.unsafeRawItemsForView(),
            .pat_ids = self.pat_ids.unsafeRawItemsForView(),
            .typed_locals = self.typed_locals.unsafeRawItemsForView(),
            .stmt_ids = self.stmt_ids.unsafeRawItemsForView(),
            .field_exprs = self.field_exprs.unsafeRawItemsForView(),
            .field_access_segments = self.field_access_segments.unsafeRawItemsForView(),
            .fn_def_captures = self.fn_def_captures.unsafeRawItemsForView(),
            .capture_operands = self.capture_operands.unsafeRawItemsForView(),
            .record_destructs = self.record_destructs.unsafeRawItemsForView(),
            .str_pattern_steps = self.str_pattern_steps.unsafeRawItemsForView(),
            .branches = self.branches.unsafeRawItemsForView(),
            .if_branches = self.if_branches.unsafeRawItemsForView(),
            .string_literals = self.string_literals.unsafeRawItemsForView(),
            .proc_debug_names = &self.proc_debug_names,
            .roots = self.roots.unsafeRawItemsForView(),
            .literal_roots = self.literal_roots.unsafeRawItemsForView(),
            .layout_requests = self.layout_requests.unsafeRawItemsForView(),
            .comptime_value_reads = self.comptime_value_reads.unsafeRawItemsForView(),
            .runtime_schema_requests = self.runtime_schema_requests.unsafeRawItemsForView(),
            .static_data_values = self.static_data_values.unsafeRawItemsForView(),
            .comptime_value_roots = self.comptime_value_roots.unsafeRawItemsForView(),
            .comptime_sites = self.comptime_sites.unsafeRawItemsForView(),
            .lowering_modules = self.loweringModules(),
            .platform_requirement_filling = self.platform_requirement_filling,
            .source_files = self.source_files.unsafeRawItemsForView(),
            .expr_locs = self.expr_locs.unsafeRawItemsForView(),
            .expr_regions = self.expr_regions.unsafeRawItemsForView(),
            .stmt_locs = self.stmt_locs.unsafeRawItemsForView(),
            .stmt_regions = self.stmt_regions.unsafeRawItemsForView(),
            .inline_scopes = self.inline_scopes.unsafeRawItemsForView(),
            .expr_inline_scopes = self.expr_inline_scopes.unsafeRawItemsForView(),
            .stmt_inline_scopes = self.stmt_inline_scopes.unsafeRawItemsForView(),
            .local_names = self.local_names.unsafeRawItemsForView(),
        };
    }

    pub fn getComptimeValueRoot(self: *const Program, id: Common.ComptimeValueRootId) Common.ComptimeValueRoot {
        return self.row("comptime_value_roots", @backingInt(id));
    }

    pub fn addComptimeValueRoot(self: *Program, root: Common.ComptimeValueRoot) std.mem.Allocator.Error!Common.ComptimeValueRootId {
        if (self.body_prefix != null) Common.invariant("body shard attempted to append frozen compile-time metadata");
        const id: Common.ComptimeValueRootId = @fromBackingInt(@intCast(@as(u32, @intCast(self.comptime_value_roots.len()))));
        try self.comptime_value_roots.append(self.allocator, root);
        return id;
    }

    pub fn markSpecConstrAnalysis(self: *const Program) SpecConstrAnalysisMark {
        var mark: SpecConstrAnalysisMark = .{
            .fns = self.fns.len(),
            .exprs = self.exprs.len(),
            .pats = self.pats.len(),
            .stmts = self.stmts.len(),
            .locals = self.locals.len(),
            .expr_ids = self.expr_ids.len(),
            .pat_ids = self.pat_ids.len(),
            .typed_locals = self.typed_locals.len(),
            .stmt_ids = self.stmt_ids.len(),
            .field_exprs = self.field_exprs.len(),
            .field_access_segments = self.field_access_segments.len(),
            .capture_operands = self.capture_operands.len(),
            .record_destructs = self.record_destructs.len(),
            .str_pattern_steps = self.str_pattern_steps.len(),
            .branches = self.branches.len(),
            .if_branches = self.if_branches.len(),
            .expr_locs = self.expr_locs.len(),
            .expr_regions = self.expr_regions.len(),
            .stmt_locs = self.stmt_locs.len(),
            .stmt_regions = self.stmt_regions.len(),
            .inline_scopes = self.inline_scopes.len(),
            .expr_inline_scopes = self.expr_inline_scopes.len(),
            .stmt_inline_scopes = self.stmt_inline_scopes.len(),
            .local_names = self.local_names.len(),
            .string_literals = self.string_literals.len(),
            .fn_def_captures = self.fn_def_captures.len(),
            .comptime_value_roots = self.comptime_value_roots.len(),
        };
        inline for (@typeInfo(SpecConstrAnalysisMark).@"struct".field_names) |field_name| {
            if (comptime !std.mem.eql(u8, field_name, "body_patch")) {
                @field(mark, field_name) += self.prefixLen(field_name);
            }
        }
        if (self.body_prefix) |prefix| mark.body_patch = prefix.patch;
        return mark;
    }

    /// Discard speculative SpecConstr appends while retaining their capacity for
    /// a retry or the next analysis walk.
    pub fn rewindSpecConstrAnalysis(self: *Program, virtual_mark: SpecConstrAnalysisMark) void {
        const mark = self.ownedAnalysisMark(virtual_mark);
        self.freeAnalysisLocalNames(mark.local_names);
        self.freeAnalysisStrings(mark.string_literals);
        self.string_literals.restoreLen(mark.string_literals);
        self.fn_def_captures.restoreLen(mark.fn_def_captures);
        self.comptime_value_roots.restoreLen(mark.comptime_value_roots);
        self.exprs.restoreLen(mark.exprs);
        self.pats.restoreLen(mark.pats);
        self.stmts.restoreLen(mark.stmts);
        self.locals.restoreLen(mark.locals);
        self.expr_ids.restoreLen(mark.expr_ids);
        self.pat_ids.restoreLen(mark.pat_ids);
        self.typed_locals.restoreLen(mark.typed_locals);
        self.stmt_ids.restoreLen(mark.stmt_ids);
        self.field_exprs.restoreLen(mark.field_exprs);
        self.field_access_segments.restoreLen(mark.field_access_segments);
        self.capture_operands.restoreLen(mark.capture_operands);
        self.record_destructs.restoreLen(mark.record_destructs);
        self.str_pattern_steps.restoreLen(mark.str_pattern_steps);
        self.branches.restoreLen(mark.branches);
        self.if_branches.restoreLen(mark.if_branches);
        self.expr_locs.restoreLen(mark.expr_locs);
        self.expr_regions.restoreLen(mark.expr_regions);
        self.stmt_locs.restoreLen(mark.stmt_locs);
        self.stmt_regions.restoreLen(mark.stmt_regions);
        self.inline_scopes.restoreLen(mark.inline_scopes);
        self.expr_inline_scopes.restoreLen(mark.expr_inline_scopes);
        self.stmt_inline_scopes.restoreLen(mark.stmt_inline_scopes);
        self.local_names.restoreLen(mark.local_names);
    }

    /// Finish a SpecConstr analysis transaction and release capacity used only
    /// by its temporary nodes.
    pub fn finishSpecConstrAnalysis(self: *Program, virtual_mark: SpecConstrAnalysisMark) void {
        const mark = self.ownedAnalysisMark(virtual_mark);
        self.freeAnalysisLocalNames(mark.local_names);
        self.freeAnalysisStrings(mark.string_literals);
        self.string_literals.shrinkAndFree(self.allocator, mark.string_literals);
        self.fn_def_captures.shrinkAndFree(self.allocator, mark.fn_def_captures);
        self.comptime_value_roots.shrinkAndFree(self.allocator, mark.comptime_value_roots);
        self.exprs.shrinkAndFree(self.allocator, mark.exprs);
        self.pats.shrinkAndFree(self.allocator, mark.pats);
        self.stmts.shrinkAndFree(self.allocator, mark.stmts);
        self.locals.shrinkAndFree(self.allocator, mark.locals);
        self.expr_ids.shrinkAndFree(self.allocator, mark.expr_ids);
        self.pat_ids.shrinkAndFree(self.allocator, mark.pat_ids);
        self.typed_locals.shrinkAndFree(self.allocator, mark.typed_locals);
        self.stmt_ids.shrinkAndFree(self.allocator, mark.stmt_ids);
        self.field_exprs.shrinkAndFree(self.allocator, mark.field_exprs);
        self.field_access_segments.shrinkAndFree(self.allocator, mark.field_access_segments);
        self.capture_operands.shrinkAndFree(self.allocator, mark.capture_operands);
        self.record_destructs.shrinkAndFree(self.allocator, mark.record_destructs);
        self.str_pattern_steps.shrinkAndFree(self.allocator, mark.str_pattern_steps);
        self.branches.shrinkAndFree(self.allocator, mark.branches);
        self.if_branches.shrinkAndFree(self.allocator, mark.if_branches);
        self.expr_locs.shrinkAndFree(self.allocator, mark.expr_locs);
        self.expr_regions.shrinkAndFree(self.allocator, mark.expr_regions);
        self.stmt_locs.shrinkAndFree(self.allocator, mark.stmt_locs);
        self.stmt_regions.shrinkAndFree(self.allocator, mark.stmt_regions);
        self.inline_scopes.shrinkAndFree(self.allocator, mark.inline_scopes);
        self.expr_inline_scopes.shrinkAndFree(self.allocator, mark.expr_inline_scopes);
        self.stmt_inline_scopes.shrinkAndFree(self.allocator, mark.stmt_inline_scopes);
        self.local_names.shrinkAndFree(self.allocator, mark.local_names);
    }

    fn ownedAnalysisMark(self: *Program, virtual_mark: SpecConstrAnalysisMark) SpecConstrAnalysisMark {
        if (self.fnCount() < virtual_mark.fns) {
            Common.invariant("SpecConstr analysis removed a durable lifted function");
        }
        var mark = virtual_mark;
        inline for (@typeInfo(SpecConstrAnalysisMark).@"struct".field_names) |field_name| {
            if (comptime !std.mem.eql(u8, field_name, "body_patch")) {
                @field(mark, field_name) = self.ownedIndex(field_name, @field(mark, field_name));
            }
        }
        if (self.body_prefix) |*prefix| prefix.patch = mark.body_patch.?;
        return mark;
    }

    fn freeAnalysisStrings(self: *Program, start: usize) void {
        for (self.string_literals.unsafeRawItemsForView()[start..]) |literal| literal.deinit(self.allocator);
    }

    fn freeAnalysisLocalNames(self: *Program, start: usize) void {
        for (self.local_names.unsafeRawItemsForView()[start..]) |name| name.deinit(self.allocator);
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        std.debug.assert(self.body_prefix == null);
        const id: FnId = @fromBackingInt(@intCast(@as(u32, @intCast(self.fns.len()))));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn reserveFnSlot(self: *Program) std.mem.Allocator.Error!FnId {
        std.debug.assert(self.body_prefix == null);
        const id: FnId = @fromBackingInt(@intCast(@as(u32, @intCast(self.fns.len()))));
        try self.fns.append(self.allocator, undefined);
        return id;
    }

    pub fn setFn(self: *Program, id: FnId, fn_: Fn) void {
        if (self.body_prefix) |*prefix| {
            if (id != prefix.source_fn) Common.invariant("body shard patched another function");
            prefix.patch = fn_;
            return;
        }
        self.fns.set(@backingInt(id), fn_);
    }

    pub fn setFnAt(self: *Program, index: usize, fn_: Fn) void {
        self.setFn(@fromBackingInt(@intCast(index)), fn_);
    }

    pub fn setFnCaptures(self: *Program, id: FnId, captures: Span(TypedLocal)) void {
        var fn_ = self.getFn(id);
        fn_.captures = captures;
        self.setFn(id, fn_);
    }

    pub fn setProcDebugName(self: *Program, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const Program, symbol: Common.Symbol) ?names.ExportNameId {
        if (self.body_prefix) |prefix| return prefix.source.procDebugName(symbol);
        return self.proc_debug_names.get(symbol);
    }

    /// The accumulator state an enclosing body emission owns while a nested
    /// body is collected.
    pub const FnShapesScope = struct {
        shapes: FnShapes = .{},
        owner: ?FnId = null,
    };

    /// Start collecting the shapes of one function body's expressions. The
    /// returned outer scope goes back to `finishFnShapes`, so a body emitted
    /// while another is in progress keeps both sets exact.
    pub fn beginFnShapes(self: *Program, owner: FnId) FnShapesScope {
        const outer: FnShapesScope = .{ .shapes = self.shapes, .owner = self.shapes_owner };
        self.shapes = .{};
        self.shapes_owner = owner;
        return outer;
    }

    /// Finish the body started by `beginFnShapes` and return its shapes.
    pub fn finishFnShapes(self: *Program, outer: FnShapesScope) FnShapes {
        const shapes = self.shapes;
        self.shapes = outer.shapes;
        self.shapes_owner = outer.owner;
        return shapes;
    }

    /// Record the shapes one expression implies for the body being emitted.
    pub fn noteExprShapes(self: *Program, expr: Expr) void {
        switch (expr.data) {
            .call_proc => |call| {
                self.shapes.direct_call = true;
                if (call.iterator_procedure) |procedure| {
                    self.shapes.iterator_call = true;
                    if (procedure.producesIteratorValue()) self.shapes.iterator_producer = true;
                }
                // The lifter records a call before and after it rewrites the
                // callee to a lifted function; only the lifted form can name
                // the owner.
                switch (call.callee) {
                    .lifted => |callee| if (callee == self.shapes_owner) {
                        self.shapes.self_call = true;
                    },
                    .func => {},
                }
            },
            .return_ => self.shapes.contains_return = true,
            .def_ref => {
                self.shapes.direct_call = true;
                self.shapes.constructs_value = true;
            },
            .tag, .record, .record_update, .tuple, .nominal, .list, .fn_ref, .lambda, .fn_def, .static_data_candidate, .comptime_value => self.shapes.constructs_value = true,
            .loop_ => {
                self.shapes.loop = true;
                const result_ty = self.types.get(expr.ty);
                if (result_ty == .tuple and result_ty.tuple.len >= 2) {
                    self.shapes.loop_tuple_result = true;
                }
            },
            .local, .int_lit, .dec_lit, .str_lit, .bytes_lit, .typed_boundary, .row_widen, .let_, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .break_, .continue_, .join_point, .jump, .crash, .checked_error, .comptime_branch_taken, .comptime_exhaustiveness_failed, .dbg, .expect_err, .literal_rejected, .expect, .@"unreachable", .unit, .frac_f32_lit, .frac_f64_lit, .uninitialized => {},
        }
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        self.noteExprShapes(expr);
        const id: ExprId = @fromBackingInt(@intCast(@as(u32, @intCast(self.exprCount()))));
        try self.exprs.ensureUnusedCapacity(self.allocator, 1);
        try self.expr_locs.ensureUnusedCapacity(self.allocator, 1);
        try self.expr_regions.ensureUnusedCapacity(self.allocator, 1);
        try self.expr_inline_scopes.ensureUnusedCapacity(self.allocator, 1);
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        try self.expr_regions.append(self.allocator, self.current_region);
        try self.expr_inline_scopes.append(self.allocator, self.current_inline_scope);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const Program, id: ExprId) base.SourceLoc {
        return self.row("expr_locs", @backingInt(id));
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const Program, id: ExprId) base.Region {
        return self.row("expr_regions", @backingInt(id));
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.row("stmt_locs", @backingInt(id));
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const Program, id: StmtId) base.Region {
        return self.row("stmt_regions", @backingInt(id));
    }

    pub fn exprInlineScope(self: *const Program, id: ExprId) InlineScopeId {
        return self.row("expr_inline_scopes", @backingInt(id));
    }

    pub fn stmtInlineScope(self: *const Program, id: StmtId) InlineScopeId {
        return self.row("stmt_inline_scopes", @backingInt(id));
    }

    pub fn inlineScope(self: *const Program, id: InlineScopeId) InlineScope {
        return self.row("inline_scopes", @backingInt(id));
    }

    pub fn addInlineScope(self: *Program, scope: InlineScope) std.mem.Allocator.Error!InlineScopeId {
        const id: InlineScopeId = @fromBackingInt(@intCast(@as(u32, @intCast(self.rowCount("inline_scopes")))));
        try self.inline_scopes.append(self.allocator, scope);
        return id;
    }

    pub fn addPat(self: *Program, pat_: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @fromBackingInt(@intCast(@as(u32, @intCast(self.patCount()))));
        try self.pats.append(self.allocator, pat_);
        return id;
    }

    /// Record the shapes one statement implies for the body being emitted.
    pub fn noteStmtShapes(self: *Program, stmt_: Stmt) void {
        switch (stmt_) {
            .return_ => self.shapes.contains_return = true,
            .uninitialized, .let_, .expr, .expect, .dbg, .crash, .checked_error => {},
        }
    }

    pub fn addStmt(self: *Program, stmt_: Stmt) std.mem.Allocator.Error!StmtId {
        self.noteStmtShapes(stmt_);
        const id: StmtId = @fromBackingInt(@intCast(@as(u32, @intCast(self.stmtCount()))));
        try self.stmts.ensureUnusedCapacity(self.allocator, 1);
        try self.stmt_locs.ensureUnusedCapacity(self.allocator, 1);
        try self.stmt_regions.ensureUnusedCapacity(self.allocator, 1);
        try self.stmt_inline_scopes.ensureUnusedCapacity(self.allocator, 1);
        try self.stmts.append(self.allocator, stmt_);
        try self.stmt_locs.append(self.allocator, self.current_loc);
        try self.stmt_regions.append(self.allocator, self.current_region);
        try self.stmt_inline_scopes.append(self.allocator, self.current_inline_scope);
        return id;
    }

    pub fn comptimeSite(self: *const Program, id: ComptimeSiteId) ComptimeSite {
        return self.row("comptime_sites", @backingInt(id));
    }

    pub fn comptimeSiteCount(self: *const Program) usize {
        return self.rowCount("comptime_sites");
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const Program, id: LocalId) []const u8 {
        return self.row("local_names", @backingInt(id)).text();
    }

    /// Replace a suffix local's diagnostic name with independently owned bytes.
    pub fn setLocalName(self: *Program, id: LocalId, text: []const u8) std.mem.Allocator.Error!void {
        const index = self.ownedIndex("local_names", @backingInt(id));
        const owned = try Mono.StringLiteral.initOwned(self.allocator, text);
        const old = self.local_names.get(index);
        old.deinit(self.allocator);
        self.local_names.set(index, owned);
    }

    pub fn sourceFiles(self: *const Program) []const base.SourceFileEntry {
        if (self.body_prefix) |prefix| return prefix.source.sourceFiles();
        return self.source_files.unsafeRawItemsForView();
    }

    pub fn loweringModules(self: *const Program) []const check.CheckedModule.ModuleId {
        if (self.body_prefix) |prefix| return prefix.source.loweringModules();
        return self.lowering_modules.unsafeRawItemsForView();
    }

    pub fn stringLiteralsView(self: *const Program) []const Mono.StringLiteral {
        std.debug.assert(self.body_prefix == null);
        return self.string_literals.unsafeRawItemsForView();
    }

    pub fn rootsView(self: *const Program) []const Root {
        if (self.body_prefix) |prefix| return prefix.source.rootsView();
        return self.roots.unsafeRawItemsForView();
    }

    pub fn literalRootsView(self: *const Program) []const LiteralRoot {
        if (self.body_prefix) |prefix| return prefix.source.literalRootsView();
        return self.literal_roots.unsafeRawItemsForView();
    }

    pub fn fnCount(self: *const Program) usize {
        return self.rowCount("fns");
    }

    pub fn getFn(self: *const Program, id: FnId) Fn {
        if (self.body_prefix) |prefix| {
            if (id == prefix.source_fn) return prefix.patch;
        }
        return self.row("fns", @backingInt(id));
    }

    /// Content digest of a lifted function's checked source identity: which
    /// checked callable it came from, the generated-body key when that
    /// callable is a compiler-generated body, its dispatch evidence, the
    /// Monotype type it was requested at, and for a SpecConstr clone the call
    /// pattern it was cloned for. Nothing here depends on per-program
    /// numbering, so the same specialization digests identically in every
    /// program. Null for a function with no checked source.
    ///
    /// Caller provenance is deliberately absent (`Mono.generatedBodyKey`):
    /// one specialization records whichever requester reserved it, which
    /// differs between programs that reach it through differently annotated
    /// call sites. Hashing it would give the same procedure two identities
    /// while `Mono.specIdentityKey` gave it one object-cache key, and an
    /// entry written under that key would then disagree with the identity the
    /// reading program lowered for it.
    pub fn fnSourceDigest(self: *Program, fn_id: FnId) std.mem.Allocator.Error!?[TypeDigestHasher.digest_length]u8 {
        var hasher = TypeDigestHasher.init();
        if (!self.writeFnOwnSource(&hasher, fn_id)) return null;
        // The app procedures a function's code reaches from outside the app
        // are not determined by its source or types.
        if (try self.fnAppDependency(fn_id)) |dependency| {
            writeIdentityBytes(&hasher, "app-dependency");
            hasher.update(&dependency);
        }
        return hasher.finalResult();
    }

    /// Everything `fnSourceDigest` digests except what the function reaches
    /// in the app. False for a function with no checked source.
    fn writeFnOwnSource(self: *Program, hasher: *TypeDigestHasher, fn_id: FnId) bool {
        const fn_ = self.getFn(fn_id);
        writeIdentityBytes(hasher, "roc.lifted.fn-source.v2");
        if (fn_.source) |template| {
            writeIdentityBytes(hasher, "template");
            writeFnDefDigest(hasher, &self.names, template.fn_def, template.site_identity);
            if (Mono.generatedBodyKey(template)) |body_key| {
                writeIdentityBytes(hasher, "generated_body");
                hasher.update(&body_key.bytes);
            } else {
                writeIdentityBytes(hasher, "no_generated_body");
            }
            hasher.update(&(template.evidence_site_digest orelse template.evidence_digest).bytes);
            // The equality digest, not the stored-identity digest: the latter
            // names a nominal type by the checked type id of whichever
            // module's store lowered it, and the same specialization lowered
            // by two programs must have one identity.
            hasher.update(&self.types.equalityDigest(&self.names, template.mono_fn_ty).bytes);
            if (template.discovery) writeIdentityBytes(hasher, "discovery");
        } else {
            const root = fn_.root_identity orelse return false;
            writeIdentityBytes(hasher, "root");
            hasher.update(&root.bytes);
        }
        if (fn_.spec_constr_pattern) |pattern| {
            writeIdentityBytes(hasher, "spec-constr-clone");
            hasher.update(&pattern.bytes);
        } else {
            writeIdentityBytes(hasher, "source");
        }
        writeIdentityBytes(hasher, if (fn_.iterator_fusion_scope) "iterator-fusion" else "plain");
        return true;
    }

    /// Whether a function's code reaches the app filling a platform
    /// requirement, through the requirement or a method of an app type: it
    /// references a function of the app from outside the app, or it
    /// references a function that reaches one. A function of the app itself
    /// does not; its code is the program's own (`LIR.LirProcSpec.code_key`).
    pub fn fnReachesApp(self: *Program, fn_id: FnId) std.mem.Allocator.Error!bool {
        const dependencies = (try self.appDependencies()) orelse return false;
        return dependencies.reaching.isSet(@backingInt(fn_id));
    }

    /// What a function's code depends on in the app, for a function that
    /// reaches it (`AppDependencies.digests`).
    fn fnAppDependency(self: *Program, fn_id: FnId) std.mem.Allocator.Error!?[32]u8 {
        const dependencies = (try self.appDependencies()) orelse return null;
        if (!dependencies.reaching.isSet(@backingInt(fn_id))) return null;
        return dependencies.digests[@backingInt(fn_id)];
    }

    fn appDependencies(self: *Program) std.mem.Allocator.Error!?*const AppDependencies {
        const filling = self.platform_requirement_filling orelse return null;
        if (self.app_dependencies) |*dependencies| {
            // Functions added since the last computation have no entry yet.
            if (dependencies.reaching.bit_length != self.fnCount()) {
                dependencies.deinit(self.allocator);
                self.app_dependencies = null;
            }
        }
        if (self.app_dependencies == null) {
            self.app_dependencies = try self.computeAppDependencies(filling);
        }
        return &self.app_dependencies.?;
    }

    fn frozenAppDependencies(self: *const Program) *const AppDependencies {
        const dependencies = &(self.app_dependencies orelse
            Common.invariant("consumer read platform dependencies before producer preparation"));
        if (dependencies.reaching.bit_length != self.fnCount())
            Common.invariant("consumer read a stale platform dependency record");
        return dependencies;
    }

    /// Whether `fn_id`'s code reaches the app, from the frozen record
    /// preparation computed before any consumer borrows it. Such code is
    /// never offered under a specialization key: Monotype would skip its
    /// body, and with it the app functions the program must lower for it
    /// (`appCalleesReachedBy`).
    pub fn fnReachesAppRecord(self: *const Program, fn_id: FnId) bool {
        if (self.platform_requirement_filling == null) return false;
        return self.frozenAppDependencies().reaching.isSet(@backingInt(fn_id));
    }

    /// Append each app function `fn_id`'s code reaches from outside the app.
    /// A program serving `fn_id`'s code from the object cache lowers these
    /// itself: the served code calls them by identity, an app function's
    /// identity names it by site rather than by its code, and only the
    /// program has its current body.
    pub fn appCalleesReachedBy(self: *const Program, fn_id: FnId, out: *std.ArrayList(FnId), allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
        if (self.platform_requirement_filling == null) return;
        const dependencies = self.frozenAppDependencies();
        const raw: usize = @backingInt(fn_id);
        if (!dependencies.reaching.isSet(raw)) return;
        const width = dependencies.column_callees.len;
        for (dependencies.column_callees, 0..) |callee, column| {
            if (dependencies.columns.isSet(raw * width + column)) try out.append(allocator, callee);
        }
    }

    fn fnIsInApp(self: *const Program, fn_id: FnId, filling: Common.PlatformRequirementFilling) bool {
        const template = self.getFn(fn_id).source orelse return false;
        const proc_template = switch (template.fn_def) {
            .local_template, .imported_template, .checked_generated => |proc_template| proc_template,
            .nested => |nested| nested.owner,
            .local_hosted, .imported_hosted => |hosted_fn| hosted_fn.template,
            inline .parser_runtime, .encoder_for_runtime => |runtime| runtime.owner,
        };
        return std.mem.eql(u8, &proc_template.artifact.bytes, &filling.app_module);
    }

    /// Whether the procedure of the app function `callee` is determined by
    /// its own source identity: its template is named by site and its
    /// request is closed (`Mono.FnTemplate.spec_key`), so its signature has
    /// no lambda sets for the app's bodies to fill.
    fn appCalleeIsSiteNamed(self: *const Program, callee: FnId) bool {
        const template = self.getFn(callee).source orelse return false;
        return template.site_identity != null and template.spec_key != null;
    }

    /// What a reference from outside the app to the app function `callee`
    /// makes the referencing code depend on: a site-named callee's own source
    /// identity, or else the app's code, which the relation names.
    fn appCalleeDependency(self: *Program, callee: FnId, filling: Common.PlatformRequirementFilling) [32]u8 {
        var hasher = TypeDigestHasher.init();
        if (self.appCalleeIsSiteNamed(callee)) {
            writeIdentityBytes(&hasher, "site-callee");
            if (!self.writeFnOwnSource(&hasher, callee)) Common.invariant("app function named by site had no checked source");
        } else {
            writeIdentityBytes(&hasher, "relation");
            hasher.update(&filling.relation);
        }
        return hasher.finalResult();
    }

    fn computeAppDependencies(self: *Program, filling: Common.PlatformRequirementFilling) std.mem.Allocator.Error!AppDependencies {
        const fn_count = self.fnCount();
        var reaching = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, fn_count);
        errdefer reaching.deinit(self.allocator);

        // Every function each function references, in one flat list.
        const edge_starts = try self.allocator.alloc(u32, fn_count + 1);
        defer self.allocator.free(edge_starts);
        var edges = std.ArrayList(FnId).empty;
        defer edges.deinit(self.allocator);
        const Collector = struct {
            program: *const Program,
            allocator: std.mem.Allocator,
            edges: *std.ArrayList(FnId),

            pub fn enterExpr(collector: @This(), expr_id: ExprId) std.mem.Allocator.Error!ExprWalk {
                const data = collector.program.getExpr(expr_id).data;
                if (data == .fn_ref) try collector.edges.append(collector.allocator, data.fn_ref.fn_id);
                if (data == .call_proc) switch (data.call_proc.callee) {
                    .lifted => |callee| try collector.edges.append(collector.allocator, callee),
                    .func => Common.invariant("unlifted call target reached Monotype Lifted requirement reachability"),
                };
                return .descend;
            }

            pub fn exitExpr(_: @This(), _: ExprId) std.mem.Allocator.Error!void {}
        };
        for (0..fn_count) |raw| {
            edge_starts[raw] = @intCast(edges.items.len);
            const fn_id: FnId = @fromBackingInt(@intCast(@as(u32, @intCast(raw))));
            switch (self.getFn(fn_id).body) {
                .roc => |body_expr| try walkExprs(self.allocator, self, .{ .expr = body_expr }, Collector{ .program = self, .allocator = self.allocator, .edges = &edges }),
                .hosted => {},
            }
        }
        edge_starts[fn_count] = @intCast(edges.items.len);

        // One column per app function referenced from outside the app, with
        // the dependency it contributes; `seeds` pairs a function with a
        // column it references directly. A function Monotype completed from
        // the object cache has no body and reaches nothing: code reaching
        // the app is never offered under a specialization key.
        var column_callees = std.ArrayList(FnId).empty;
        defer column_callees.deinit(self.allocator);
        var column_digests = std.ArrayList([32]u8).empty;
        defer column_digests.deinit(self.allocator);
        var column_by_callee = collections.DenseMap(FnId, u32).init(self.allocator);
        defer column_by_callee.deinit();
        const Seed = struct { fn_index: u32, column: u32 };
        var seeds = std.ArrayList(Seed).empty;
        defer seeds.deinit(self.allocator);
        for (0..fn_count) |raw| {
            const fn_id: FnId = @fromBackingInt(@intCast(@as(u32, @intCast(raw))));
            if (self.fnIsInApp(fn_id, filling)) continue;
            for (edges.items[edge_starts[raw]..edge_starts[raw + 1]]) |callee| {
                if (!self.fnIsInApp(callee, filling)) continue;
                const entry = try column_by_callee.getOrPut(callee);
                if (!entry.found_existing) {
                    entry.value_ptr.* = @intCast(column_callees.items.len);
                    try column_callees.append(self.allocator, callee);
                    try column_digests.append(self.allocator, self.appCalleeDependency(callee, filling));
                }
                try seeds.append(self.allocator, .{ .fn_index = @intCast(raw), .column = entry.value_ptr.* });
            }
        }

        // One row of column bits per function, propagated to every function
        // referencing one with bits set.
        const width = column_callees.items.len;
        var columns = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, fn_count * width);
        errdefer columns.deinit(self.allocator);
        for (seeds.items) |seed| {
            columns.set(seed.fn_index * width + seed.column);
            reaching.set(seed.fn_index);
        }
        var changed = width != 0;
        while (changed) {
            changed = false;
            for (0..fn_count) |raw| {
                const fn_id: FnId = @fromBackingInt(@intCast(@as(u32, @intCast(raw))));
                if (self.fnIsInApp(fn_id, filling)) continue;
                for (edges.items[edge_starts[raw]..edge_starts[raw + 1]]) |callee| {
                    const callee_raw: usize = @backingInt(callee);
                    if (!reaching.isSet(callee_raw)) continue;
                    for (0..width) |column| {
                        if (columns.isSet(callee_raw * width + column) and !columns.isSet(raw * width + column)) {
                            columns.set(raw * width + column);
                            reaching.set(raw);
                            changed = true;
                        }
                    }
                }
            }
        }

        // A reaching function's digest is that of the distinct dependencies
        // of its columns, in digest order so it does not depend on discovery
        // order.
        const digests = try self.allocator.alloc([32]u8, fn_count);
        errdefer self.allocator.free(digests);
        @memset(digests, @splat(0));
        var members = std.ArrayList([32]u8).empty;
        defer members.deinit(self.allocator);
        for (0..fn_count) |raw| {
            if (!reaching.isSet(raw)) continue;
            members.clearRetainingCapacity();
            for (0..width) |column| {
                if (columns.isSet(raw * width + column)) try members.append(self.allocator, column_digests.items[column]);
            }
            std.mem.sort([32]u8, members.items, {}, struct {
                fn lessThan(_: void, lhs: [32]u8, rhs: [32]u8) bool {
                    return std.mem.order(u8, &lhs, &rhs) == .lt;
                }
            }.lessThan);
            var hasher = TypeDigestHasher.init();
            writeIdentityBytes(&hasher, "roc.lifted.app-dependency.v1");
            var previous: ?[32]u8 = null;
            for (members.items) |member| {
                if (previous) |prior| if (std.mem.eql(u8, &prior, &member)) continue;
                hasher.update(&member);
                previous = member;
            }
            digests[raw] = hasher.finalResult();
        }
        return .{
            .reaching = reaching,
            .digests = digests,
            .columns = columns,
            .column_callees = try column_callees.toOwnedSlice(self.allocator),
        };
    }

    /// Checked source identity of a function whose template is keyed by
    /// layout (`Mono.FnTemplate.procedure_keyed_by_layout`): the template and
    /// its evidence, without the Monotype type, whose place in the procedure
    /// identity Direct LIR fills with the procedure's argument and result
    /// layouts. Null for every other function, including a SpecConstr clone
    /// of a layout-keyed template, whose body is no longer the template's.
    pub fn fnLayoutKeyedSourceDigest(self: *Program, fn_id: FnId) ?[TypeDigestHasher.digest_length]u8 {
        const fn_ = self.getFn(fn_id);
        const template = fn_.source orelse return null;
        if (!template.procedure_keyed_by_layout or fn_.spec_constr_pattern != null) return null;
        var hasher = TypeDigestHasher.init();
        writeIdentityBytes(&hasher, "roc.lifted.fn-layout-keyed-source.v1");
        writeFnDefDigest(&hasher, &self.names, template.fn_def, template.site_identity);
        hasher.update(&template.evidence_digest.bytes);
        writeIdentityBytes(&hasher, if (fn_.iterator_fusion_scope) "iterator-fusion" else "plain");
        return hasher.finalResult();
    }

    pub fn getFnAt(self: *const Program, index: usize) Fn {
        return self.getFn(@fromBackingInt(@intCast(index)));
    }

    pub fn fnsView(self: *const Program) []const Fn {
        std.debug.assert(self.body_prefix == null);
        return self.fns.unsafeRawItemsForView();
    }

    pub fn getExpr(self: *const Program, id: ExprId) Expr {
        return self.row("exprs", @backingInt(id));
    }

    pub fn exprsView(self: *const Program) []const Expr {
        std.debug.assert(self.body_prefix == null);
        return self.exprs.unsafeRawItemsForView();
    }

    pub fn getExprAt(self: *const Program, index: usize) Expr {
        return self.row("exprs", index);
    }

    pub fn setExprData(self: *Program, id: ExprId, data: ExprData) void {
        self.noteExprShapes(.{ .ty = self.getExpr(id).ty, .data = data });
        self.setExprDataAt(@backingInt(id), data);
    }

    pub fn setExprDataAt(self: *Program, index: usize, data: ExprData) void {
        self.exprs.getPtrImmediate(self.ownedIndex("exprs", index)).data = data;
    }

    pub fn getPat(self: *const Program, id: PatId) Pat {
        return self.row("pats", @backingInt(id));
    }

    pub fn getStmt(self: *const Program, id: StmtId) Stmt {
        return self.row("stmts", @backingInt(id));
    }

    pub fn stmtsView(self: *const Program) []const Stmt {
        std.debug.assert(self.body_prefix == null);
        return self.stmts.unsafeRawItemsForView();
    }

    pub fn getLocal(self: *const Program, id: LocalId) Local {
        return self.row("locals", @backingInt(id));
    }

    pub fn localsView(self: *const Program) []const Local {
        std.debug.assert(self.body_prefix == null);
        return self.locals.unsafeRawItemsForView();
    }

    pub fn getStringLiteral(self: *const Program, id: StringLiteralId) Mono.StringLiteral {
        return self.row("string_literals", @backingInt(id));
    }

    pub fn addStringLiteral(self: *Program, text: []const u8) std.mem.Allocator.Error!StringLiteralId {
        const id: StringLiteralId = @fromBackingInt(@intCast(@as(u32, @intCast(self.rowCount("string_literals")))));
        const owned = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(owned);
        try self.string_literals.append(self.allocator, .{
            .backing = owned,
            .offset = 0,
            .len = @intCast(text.len),
        });
        return id;
    }

    pub fn addRoot(self: *Program, root: Root) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.roots.append(self.allocator, root);
    }

    pub fn addLiteralRoot(self: *Program, root: LiteralRoot) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.literal_roots.append(self.allocator, root);
    }

    pub fn addLayoutRequest(self: *Program, request: LayoutRequest) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.layout_requests.append(self.allocator, request);
    }

    pub fn comptimeValueReadsView(self: *const Program) []const Common.ComptimeValueRoot {
        return self.comptime_value_reads.unsafeRawItemsForView();
    }

    pub fn addComptimeValueRead(self: *Program, root: Common.ComptimeValueRoot) std.mem.Allocator.Error!void {
        try self.comptime_value_reads.append(self.allocator, root);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @fromBackingInt(@intCast(@as(u32, @intCast(self.localCount()))));
        try self.locals.ensureUnusedCapacity(self.allocator, 1);
        try self.local_names.ensureUnusedCapacity(self.allocator, 1);
        const checked_capture_id = if (binder) |b| check.CheckedModule.CaptureId.fromBinder(b) else null;
        try self.locals.append(self.allocator, .{
            .id = id,
            .symbol = symbol,
            .ty = ty,
            .binder = binder,
            // A binder-backed local carries the exact capture identity of
            // its binding, so any function that captures it joins by CaptureId.
            .capture_id = checked_capture_id,
            .checked_capture_id = checked_capture_id,
        });
        try self.local_names.append(self.allocator, .empty);
        return id;
    }

    /// Add a replacement local that retains a source capture's complete
    /// identity while changing its monomorphic type. One-to-one capture ABI
    /// rewrites must preserve both the checked binder and the CaptureId; only a
    /// one-to-many split may mint a new generated identity.
    pub fn addLocalWithCaptureIdentity(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
        capture_id: check.CheckedModule.CaptureId,
        checked_capture_id: ?check.CheckedModule.CaptureId,
        capture_kind: check.CheckedModule.ConstCaptureKind,
    ) std.mem.Allocator.Error!LocalId {
        if (checked_capture_id) |checked_id| {
            if (checked_id.isCanonical()) {
                const source_binder = binder orelse
                    Common.invariant("source capture identity had no checked binder");
                if (checked_id != check.CheckedModule.CaptureId.fromBinder(source_binder)) {
                    Common.invariant("checked capture identity disagreed with its binder");
                }
            }
        }
        if (binder == null and capture_id.isCanonical()) {
            Common.invariant("capture replacement CaptureId had no checked binder");
        }

        const id: LocalId = @fromBackingInt(@intCast(@as(u32, @intCast(self.localCount()))));
        try self.locals.ensureUnusedCapacity(self.allocator, 1);
        try self.local_names.ensureUnusedCapacity(self.allocator, 1);
        try self.locals.append(self.allocator, .{
            .id = id,
            .symbol = symbol,
            .ty = ty,
            .binder = binder,
            .capture_id = capture_id,
            .checked_capture_id = checked_capture_id,
            .capture_kind = capture_kind,
        });
        try self.local_names.append(self.allocator, .empty);
        return id;
    }

    /// Allocate the next generated `CaptureId` for a lift-synthesized capturable
    /// local (a free local with no checked binder). The counter lives on the
    /// program so the identity is stable across fixpoint rounds and unique
    /// within the program.
    pub fn nextLiftCaptureId(self: *Program) check.CheckedModule.CaptureId {
        if (self.body_prefix != null) Common.invariant("body shard cannot generate capture identities");
        const index = self.next_lift_capture_id;
        if (index > check.CheckedModule.CaptureId.max_generated_index) {
            Common.invariant("lifted program exhausted durable capture identities");
        }
        self.next_lift_capture_id += 1;
        return check.CheckedModule.CaptureId.generatedLift(index);
    }

    pub fn addTypedLocalSpan(self: *Program, values: []const TypedLocal) std.mem.Allocator.Error!Span(TypedLocal) {
        return self.appendBodySpan(TypedLocal, "typed_locals", values);
    }

    pub fn addExprSpan(self: *Program, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        return self.appendBodySpan(ExprId, "expr_ids", ids);
    }

    pub fn addPatSpan(self: *Program, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        return self.appendBodySpan(PatId, "pat_ids", ids);
    }

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        return self.appendBodySpan(StmtId, "stmt_ids", ids);
    }

    pub fn addFieldExprSpan(self: *Program, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        return self.appendBodySpan(FieldExpr, "field_exprs", values);
    }

    pub fn addFieldAccessSegmentSpan(self: *Program, values: []const FieldAccessSegment) std.mem.Allocator.Error!Span(FieldAccessSegment) {
        if (values.len == 0) Common.invariant("field access segment span must be nonempty");
        return self.appendBodySpan(FieldAccessSegment, "field_access_segments", values);
    }

    pub fn addFnDefCaptureSpan(self: *Program, values: []const FnDefCapture) std.mem.Allocator.Error!Span(FnDefCapture) {
        return self.appendBodySpan(FnDefCapture, "fn_def_captures", values);
    }

    pub fn addCaptureOperandSpan(self: *Program, values: []const CaptureOperand) std.mem.Allocator.Error!Span(CaptureOperand) {
        return self.appendBodySpan(CaptureOperand, "capture_operands", values);
    }

    /// Read one operand by value from a stable span identity. Unlike
    /// `captureOperandSpan`, this retains no borrow across a recursive walk
    /// that may append to `capture_operands`.
    pub fn captureOperandAt(self: *const Program, span_: Span(CaptureOperand), index: usize) CaptureOperand {
        if (index >= span_.len) Common.invariant("capture operand index was outside span");
        return self.row("capture_operands", span_.start + index);
    }

    pub fn setCaptureOperandInSpan(self: *Program, span_: Span(CaptureOperand), index: usize, operand: CaptureOperand) void {
        if (index >= span_.len) Common.invariant("capture operand index was outside span");
        self.capture_operands.set(self.ownedIndex("capture_operands", span_.start + index), operand);
    }

    pub fn addRecordDestructSpan(self: *Program, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        return self.appendBodySpan(RecordDestruct, "record_destructs", values);
    }

    pub fn addStrPatternStepSpan(self: *Program, values: []const Mono.StrPatternStep) std.mem.Allocator.Error!Span(Mono.StrPatternStep) {
        return self.appendBodySpan(Mono.StrPatternStep, "str_pattern_steps", values);
    }

    pub fn addBranchSpan(self: *Program, values: []const Branch) std.mem.Allocator.Error!Span(Branch) {
        return self.appendBodySpan(Branch, "branches", values);
    }

    /// Read one branch by value from a stable span identity. Unlike
    /// `branchSpan`, this retains no borrow across a recursive walk that may
    /// append to `branches`.
    pub fn branchAt(self: *const Program, span_: Span(Branch), index: usize) Branch {
        if (index >= span_.len) Common.invariant("branch index was outside span");
        return self.row("branches", span_.start + index);
    }

    pub fn addIfBranchSpan(self: *Program, values: []const IfBranch) std.mem.Allocator.Error!Span(IfBranch) {
        return self.appendBodySpan(IfBranch, "if_branches", values);
    }

    pub fn exprSpan(self: *const Program, span_: Span(ExprId)) ProgramSpanBorrow(ExprId, "expr_ids") {
        return self.bodySpan(ExprId, "expr_ids", span_);
    }

    pub fn patSpan(self: *const Program, span_: Span(PatId)) ProgramSpanBorrow(PatId, "pat_ids") {
        return self.bodySpan(PatId, "pat_ids", span_);
    }

    pub fn typedLocalSpan(self: *const Program, span_: Span(TypedLocal)) ProgramSpanBorrow(TypedLocal, "typed_locals") {
        return self.bodySpan(TypedLocal, "typed_locals", span_);
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: *const Program, id: LocalId) check.CheckedModule.CaptureId {
        return self.getLocal(id).capture_id orelse
            Common.invariant("lifted capture local had no CaptureId");
    }

    pub fn ensureLiftCaptureId(self: *Program, id: LocalId) check.CheckedModule.CaptureId {
        if (self.body_prefix != null) return self.captureIdOfLocal(id);
        const local = self.locals.getPtrImmediate(@backingInt(id));
        if (local.capture_id == null) {
            local.capture_id = self.nextLiftCaptureId();
        }
        return local.capture_id.?;
    }

    pub fn stmtSpan(self: *const Program, span_: Span(StmtId)) ProgramSpanBorrow(StmtId, "stmt_ids") {
        return self.bodySpan(StmtId, "stmt_ids", span_);
    }

    pub fn fieldExprSpan(self: *const Program, span_: Span(FieldExpr)) ProgramSpanBorrow(FieldExpr, "field_exprs") {
        return self.bodySpan(FieldExpr, "field_exprs", span_);
    }

    pub fn fieldAccessSegmentSpan(self: *const Program, span_: Span(FieldAccessSegment)) ProgramSpanBorrow(FieldAccessSegment, "field_access_segments") {
        return self.bodySpan(FieldAccessSegment, "field_access_segments", span_);
    }

    pub fn fieldAccessSegmentAt(self: *const Program, span_: Span(FieldAccessSegment), index: usize) FieldAccessSegment {
        if (index >= span_.len) Common.invariant("field access segment index was outside span");
        return self.row("field_access_segments", span_.start + index);
    }

    pub fn fnDefCaptureSpan(self: *const Program, span_: Span(FnDefCapture)) ProgramSpanBorrow(FnDefCapture, "fn_def_captures") {
        return self.bodySpan(FnDefCapture, "fn_def_captures", span_);
    }

    pub fn captureOperandSpan(self: *const Program, span_: Span(CaptureOperand)) ProgramSpanBorrow(CaptureOperand, "capture_operands") {
        return self.bodySpan(CaptureOperand, "capture_operands", span_);
    }

    pub fn recordDestructSpan(self: *const Program, span_: Span(RecordDestruct)) ProgramSpanBorrow(RecordDestruct, "record_destructs") {
        return self.bodySpan(RecordDestruct, "record_destructs", span_);
    }

    pub fn strPatternStepSpan(self: *const Program, span_: Span(Mono.StrPatternStep)) ProgramSpanBorrow(Mono.StrPatternStep, "str_pattern_steps") {
        return self.bodySpan(Mono.StrPatternStep, "str_pattern_steps", span_);
    }

    pub fn branchSpan(self: *const Program, span_: Span(Branch)) ProgramSpanBorrow(Branch, "branches") {
        return self.bodySpan(Branch, "branches", span_);
    }

    /// See `layoutResolvedMatchIn`.
    pub fn layoutResolvedMatch(
        self: *const Program,
        scrutinee: ExprId,
        branches_span: Span(Branch),
    ) ?LayoutResolvedMatch {
        return layoutResolvedMatchIn(self, scrutinee, branches_span);
    }

    /// See `branchForAnswerIn`.
    pub fn branchForAnswer(self: *const Program, branches_span: Span(Branch), answer: u8) ?ExprId {
        return branchForAnswerIn(self, branches_span, answer);
    }

    pub fn ifBranchSpan(self: *const Program, span_: Span(IfBranch)) ProgramSpanBorrow(IfBranch, "if_branches") {
        return self.bodySpan(IfBranch, "if_branches", span_);
    }

    pub fn exprCount(self: *const Program) usize {
        return self.rowCount("exprs");
    }

    pub fn patCount(self: *const Program) usize {
        return self.rowCount("pats");
    }

    pub fn stmtCount(self: *const Program) usize {
        return self.rowCount("stmts");
    }

    pub fn localCount(self: *const Program) usize {
        return self.rowCount("locals");
    }
};

/// A match direct LIR lowering may fold to one branch: its scrutinee calls
/// the Builtin wrapper of a low-level whose answer the lowerer decides from
/// committed layouts, and its branches are guard-free, binding-free integer
/// literals and wildcards. The lowerer computes the answer (it needs the
/// layouts) and `branchForAnswer` names the branch that answer selects.
pub const LayoutResolvedMatch = struct {
    op: can.CIR.Expr.LowLevel.LayoutResolved,
    call_args: Span(ExprId),
};

/// One match statically resolved by direct LIR lowering, recorded so the
/// debug Lambda Mono materializer replays the identical resolution and the
/// two derivations demand the same set of functions. Keyed by the match's
/// scrutinee expression, which belongs to exactly one match.
///
/// The fold choice is the only part the materializer copies: it re-derives
/// the site with `layoutResolvedMatch` and asserts the recorded body is the
/// branch the recorded answer selects. Skipping the scrutinee is sound only
/// because that recognizer admits nothing but a call to a compiler-synthesized
/// layout-resolved low-level wrapper over guard-free, binding-free literal and
/// wildcard branches; a new kind of fold must extend the recognizer and this
/// contract together.
pub const FoldedMatch = struct {
    scrutinee: ExprId,
    body: ExprId,
    /// The low-level's answer the fold assumed. `list_map_can_reuse` only
    /// ever folds to 0: a possible reuse keeps the match for the backends.
    answer: u8,

    /// Whether this record resolves the match `self.scrutinee` heads, whose
    /// branches are `branches_span`: the match is a layout-resolved site and
    /// `self.body` is the branch `self.answer` selects. `program` is a
    /// `Program` or a `ProgramView`.
    pub fn isResolutionOf(self: FoldedMatch, program: anytype, branches_span: Span(Branch)) bool {
        const site = layoutResolvedMatchIn(program, self.scrutinee, branches_span) orelse return false;
        if (site.op == .list_map_can_reuse and self.answer != 0) return false;
        return branchForAnswerIn(program, branches_span, self.answer) == self.body;
    }
};

/// Recognizes a layout-resolved match: one whose scrutinee calls the Builtin
/// wrapper of `list_map_can_reuse` or `list_item_size_at_most`, with
/// guard-free, binding-free integer-literal and wildcard branches. Returns
/// null for any other shape. Whether to fold is the caller's layout-aware
/// decision; this only identifies the site. `program` is a `Program` or a
/// `ProgramView`.
fn layoutResolvedMatchIn(
    program: anytype,
    scrutinee: ExprId,
    branches_span: Span(Branch),
) ?LayoutResolvedMatch {
    const scrutinee_data = program.getExpr(scrutinee).data;
    if (std.meta.activeTag(scrutinee_data) != .call_proc) return null;
    const call = scrutinee_data.call_proc;
    const callee = switch (call.callee) {
        .lifted => |fn_id| fn_id,
        .func => return null,
    };
    const callee_body = switch (program.getFn(callee).body) {
        .roc => |body| body,
        .hosted => return null,
    };
    const op = layoutResolvedOpOf(program, callee_body) orelse return null;

    const branches = program.branchSpan(branches_span);
    for (0..GuardedList.borrowLen(branches)) |index| {
        const branch = GuardedList.at(branches, index);
        if (branch.guard != null or branch.bindings.len != 0) return null;
        const tag = std.meta.activeTag(program.getPat(branch.pat).data);
        if (tag != .wildcard and tag != .int_lit) return null;
    }
    return .{ .op = op, .call_args = call.args };
}

/// The branch of a layout-resolved match that the answer selects: the first
/// whose pattern is that integer literal or a wildcard. Null when no branch
/// matches, which an exhaustive match never has.
fn branchForAnswerIn(program: anytype, branches_span: Span(Branch), answer: u8) ?ExprId {
    const branches = program.branchSpan(branches_span);
    for (0..GuardedList.borrowLen(branches)) |index| {
        const branch = GuardedList.at(branches, index);
        const pat_data = program.getPat(branch.pat).data;
        const tag = std.meta.activeTag(pat_data);
        if (tag == .wildcard or (tag == .int_lit and pat_data.int_lit.toI128() == answer)) {
            return branch.body;
        }
    }
    return null;
}

/// The layout-resolved low-level a Builtin wrapper body runs, if it is one.
fn layoutResolvedOpOf(program: anytype, root: ExprId) ?can.CIR.Expr.LowLevel.LayoutResolved {
    var expr_id = root;
    while (true) {
        const data = program.getExpr(expr_id).data;
        const tag = std.meta.activeTag(data);
        if (tag == .low_level) return data.low_level.op.layoutResolved();
        if (tag != .block or data.block.statements.len != 0) return null;
        expr_id = data.block.final_expr;
    }
}

/// Visit every local a pattern binds, innermost first, in binding order.
///
/// Three consumers need this walk—the lift pass's bound-set scan, its capture
/// graph builder, and SpecConstr's body-local scope—and they differ only in
/// what they do at a binding site, never in which pattern positions bind. The
/// walk lives here so a new `PatData` variant is one edit, and so the three
/// cannot come to disagree about, say, whether a list rest pattern binds.
/// It keeps its own work stack, so pattern nesting never becomes native call
/// depth.
///
/// `binder` is any value exposing `bindLocal(LocalId) !void`.
pub fn forEachBoundLocal(allocator: std.mem.Allocator, program: *const Program, pat_id: PatId, binder: anytype) std.mem.Allocator.Error!void {
    const Work = union(enum) { pat: PatId, bind: LocalId };
    var work: std.ArrayList(Work) = .empty;
    defer work.deinit(allocator);
    try work.append(allocator, .{ .pat = pat_id });
    while (work.pop()) |item| switch (item) {
        .bind => |local| try binder.bindLocal(local),
        .pat => |id| {
            // Children are appended in binding order, then reversed onto the stack.
            const children_start = work.items.len;
            switch (program.getPat(id).data) {
                .bind => |local| try work.append(allocator, .{ .bind = local }),
                .wildcard,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                => {},
                .str_pattern => |str| {
                    const steps = program.strPatternStepSpan(str.steps);
                    for (0..steps.len) |index| {
                        if (GuardedList.at(steps, index).capture) |capture| {
                            try work.append(allocator, .{ .pat = capture });
                        }
                    }
                },
                .as => |as| {
                    try work.append(allocator, .{ .pat = as.pattern });
                    try work.append(allocator, .{ .bind = as.local });
                },
                .record => |fields| {
                    const destructs = program.recordDestructSpan(fields);
                    for (0..destructs.len) |index| {
                        try work.append(allocator, .{ .pat = GuardedList.at(destructs, index).pattern });
                    }
                },
                .tuple => |items| {
                    const children = program.patSpan(items);
                    for (0..children.len) |index| try work.append(allocator, .{ .pat = GuardedList.at(children, index) });
                },
                .list => |list| {
                    const children = program.patSpan(list.patterns);
                    for (0..children.len) |index| try work.append(allocator, .{ .pat = GuardedList.at(children, index) });
                    if (list.rest) |rest| if (rest.pattern) |rest_pattern| {
                        try work.append(allocator, .{ .pat = rest_pattern });
                    };
                },
                .tag => |tag| {
                    const payloads = program.patSpan(tag.payloads);
                    for (0..payloads.len) |index| try work.append(allocator, .{ .pat = GuardedList.at(payloads, index) });
                },
                .nominal => |backing| try work.append(allocator, .{ .pat = backing }),
            }
            std.mem.reverse(Work, work.items[children_start..]);
        },
    };
}

/// One position directly beneath an expression: a child expression, or a
/// statement of a block or match-branch binding list.
pub const ExprChild = union(enum) {
    expr: ExprId,
    stmt: StmtId,
};

/// Append every position directly beneath `expr_id`, in source order, to
/// `out`. Consumers that walk expression trees on an explicit stack use this
/// as their single definition of which positions hold child expressions and
/// statements; a consumer that treats a particular form differently handles
/// that form itself.
pub fn appendChildren(allocator: std.mem.Allocator, program: *const Program, expr_id: ExprId, out: *std.ArrayList(ExprChild)) std.mem.Allocator.Error!void {
    const Out = struct {
        list: *std.ArrayList(ExprChild),
        allocator: std.mem.Allocator,
        program: *const Program,

        fn one(sink: @This(), child: ExprId) std.mem.Allocator.Error!void {
            try sink.list.append(sink.allocator, .{ .expr = child });
        }

        fn span(sink: @This(), items: Span(ExprId)) std.mem.Allocator.Error!void {
            const values = sink.program.exprSpan(items);
            for (0..values.len) |index| try sink.one(GuardedList.at(values, index));
        }

        fn fields(sink: @This(), items: Span(FieldExpr)) std.mem.Allocator.Error!void {
            const values = sink.program.fieldExprSpan(items);
            for (0..values.len) |index| try sink.one(GuardedList.at(values, index).value);
        }

        fn captures(sink: @This(), items: Span(CaptureOperand)) std.mem.Allocator.Error!void {
            const values = sink.program.captureOperandSpan(items);
            for (0..values.len) |index| try sink.one(GuardedList.at(values, index).value);
        }

        fn stmts(sink: @This(), items: Span(StmtId)) std.mem.Allocator.Error!void {
            const values = sink.program.stmtSpan(items);
            for (0..values.len) |index| try sink.list.append(sink.allocator, .{ .stmt = GuardedList.at(values, index) });
        }
    };
    const sink = Out{ .list = out, .allocator = allocator, .program = program };
    switch (program.getExpr(expr_id).data) {
        .local,
        .unit,
        .@"unreachable",
        .int_lit,
        .frac_f32_lit,
        .frac_f64_lit,
        .dec_lit,
        .str_lit,
        .bytes_lit,
        .crash,
        .checked_error,
        .comptime_exhaustiveness_failed,
        .uninitialized,
        .uninitialized_payload,
        .def_ref,
        => {},
        .fn_ref => |fn_ref| try sink.captures(fn_ref.captures),
        .fn_def => |fn_def| {
            const values = program.fnDefCaptureSpan(fn_def.captures);
            for (0..values.len) |index| try sink.one(GuardedList.at(values, index).value);
        },
        .lambda => |lambda| try sink.one(lambda.body),
        .list, .tuple => |items| try sink.span(items),
        .record => |fields| try sink.fields(fields),
        .record_update => |update| {
            try sink.one(update.base);
            try sink.fields(update.fields);
        },
        .tag => |tag| try sink.span(tag.payloads),
        .static_data_candidate => |candidate| try sink.one(candidate.runtime_expr),
        .comptime_value => |value| try sink.one(value.initializer),
        .typed_boundary => |boundary| try sink.one(boundary.value),
        .row_widen => |widen| try sink.one(widen.value),
        .nominal, .dbg, .expect => |child| try sink.one(child),
        .return_ => |ret| try sink.one(ret.value),
        .expect_err => |expect_err| try sink.one(expect_err.msg),
        .literal_rejected => |rejected| try sink.one(rejected.msg),
        .comptime_branch_taken => |taken| try sink.one(taken.body),
        .let_ => |let_| {
            try sink.one(let_.value);
            try sink.one(let_.rest);
        },
        .call_value => |call| {
            try sink.one(call.callee);
            try sink.span(call.args);
        },
        .call_proc => |call| {
            try sink.span(call.args);
            try sink.captures(call.captures);
        },
        .low_level => |call| try sink.span(call.args),
        .field_access => |field| try sink.one(field.receiver),
        .tuple_access => |access| try sink.one(access.tuple),
        .structural_eq => |eq| {
            try sink.one(eq.lhs);
            try sink.one(eq.rhs);
        },
        .structural_hash => |h| {
            try sink.one(h.value);
            try sink.one(h.hasher);
        },
        .match_ => |match| {
            try sink.one(match.scrutinee);
            const branches = program.branchSpan(match.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                try sink.stmts(branch.bindings);
                if (branch.guard) |guard| try sink.one(guard);
                try sink.one(branch.body);
            }
        },
        .if_ => |if_| {
            const branches = program.ifBranchSpan(if_.branches);
            for (0..branches.len) |index| {
                const branch = GuardedList.at(branches, index);
                try sink.one(branch.cond);
                try sink.one(branch.body);
            }
            try sink.one(if_.final_else);
        },
        .block => |block| {
            try sink.stmts(block.statements);
            try sink.one(block.final_expr);
        },
        .loop_ => |loop| {
            try sink.span(loop.initial_values);
            try sink.one(loop.body);
        },
        .break_ => |maybe| if (maybe) |value| try sink.one(value),
        .continue_ => |continue_| try sink.span(continue_.values),
        .join_point => |join_point| {
            try sink.one(join_point.body);
            try sink.one(join_point.remainder);
        },
        .jump => |jump| {
            try sink.span(jump.loop_values);
            try sink.span(jump.args);
        },
        .if_initialized_payload => |payload_switch| {
            try sink.one(payload_switch.cond);
            try sink.one(payload_switch.initialized);
            try sink.one(payload_switch.uninitialized);
        },
        .try_sequence => |sequence| {
            try sink.one(sequence.try_expr);
            try sink.one(sequence.ok_body);
        },
        .try_record_sequence => |sequence| {
            try sink.one(sequence.try_expr);
            try sink.one(sequence.ok_body);
        },
    }
}

/// Append the expression a statement evaluates, if any.
pub fn appendStmtChildren(allocator: std.mem.Allocator, program: *const Program, stmt_id: StmtId, out: *std.ArrayList(ExprChild)) std.mem.Allocator.Error!void {
    switch (program.getStmt(stmt_id)) {
        .let_ => |let_| try out.append(allocator, .{ .expr = let_.value }),
        .expr, .expect, .dbg => |expr| try out.append(allocator, .{ .expr = expr }),
        .return_ => |ret| try out.append(allocator, .{ .expr = ret.value }),
        .uninitialized, .crash, .checked_error => {},
    }
}

/// Whether any position in the tree rooted at `root` satisfies `visitor`,
/// searched on an explicit stack. `visitor.visitExpr(expr_id, stack)` and
/// `visitor.visitStmt(stmt_id, stack)` return `.found` to stop, `.skip` to
/// leave the position's children unvisited, `.descend` to visit its children
/// as `appendChildren`/`appendStmtChildren` list them, or `.children` after
/// pushing a custom child list onto `stack` themselves. The search has no
/// side effects of its own, so child order does not affect the answer.
pub fn anyExpr(allocator: std.mem.Allocator, program: *const Program, root: ExprId, visitor: anytype) std.mem.Allocator.Error!bool {
    return anyChild(allocator, program, .{ .expr = root }, visitor);
}

/// `anyExpr` rooted at an expression or a statement.
pub fn anyChild(allocator: std.mem.Allocator, program: *const Program, root: ExprChild, visitor: anytype) std.mem.Allocator.Error!bool {
    var stack: std.ArrayList(ExprChild) = .empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, root);
    while (stack.pop()) |child| {
        switch (child) {
            .expr => |expr_id| switch (try visitor.visitExpr(expr_id, &stack)) {
                .found => return true,
                .skip, .children => {},
                .descend => try appendChildren(allocator, program, expr_id, &stack),
            },
            .stmt => |stmt_id| switch (try visitor.visitStmt(stmt_id, &stack)) {
                .found => return true,
                .skip, .children => {},
                .descend => try appendStmtChildren(allocator, program, stmt_id, &stack),
            },
        }
    }
    return false;
}

/// A decision `anyExpr` asks its visitor for at each expression.
pub const ExprSearch = enum { found, skip, descend, children };

/// What `walkExprs` does at one expression.
pub const ExprWalk = enum {
    /// Leave the expression's children unvisited.
    skip,
    /// Visit the expression's children.
    descend,
    /// Visit the expression's children, then call the visitor's `exitExpr`.
    descend_then_exit,
};

/// Walk the tree rooted at `root` depth-first, in source order, on an
/// explicit stack. `visitor.enterExpr(expr_id)` returns an `ExprWalk`; the
/// children it visits are those `appendChildren` lists, read when the
/// expression is entered. Statements visit the expressions they evaluate.
pub fn walkExprs(allocator: std.mem.Allocator, program: *const Program, root: ExprChild, visitor: anytype) std.mem.Allocator.Error!void {
    const Item = union(enum) { enter: ExprChild, exit: ExprId };
    var stack: std.ArrayList(Item) = .empty;
    defer stack.deinit(allocator);
    var children: std.ArrayList(ExprChild) = .empty;
    defer children.deinit(allocator);
    try stack.append(allocator, .{ .enter = root });
    while (stack.pop()) |item| {
        children.clearRetainingCapacity();
        switch (item) {
            .exit => |expr_id| {
                try visitor.exitExpr(expr_id);
                continue;
            },
            .enter => |child| switch (child) {
                .stmt => |stmt_id| try appendStmtChildren(allocator, program, stmt_id, &children),
                .expr => |expr_id| switch (try visitor.enterExpr(expr_id)) {
                    .skip => continue,
                    .descend => try appendChildren(allocator, program, expr_id, &children),
                    .descend_then_exit => {
                        try stack.append(allocator, .{ .exit = expr_id });
                        try appendChildren(allocator, program, expr_id, &children);
                    },
                },
            },
        }
        var index = children.items.len;
        while (index > 0) {
            index -= 1;
            try stack.append(allocator, .{ .enter = children.items[index] });
        }
    }
}

fn testLiftedProgram(allocator: std.mem.Allocator) Program {
    return Program.init(
        allocator,
        names.NameStore.init(allocator),
        Type.Store.init(allocator),
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0,
    );
}

fn testSourceDigestTemplate(name_store: *names.NameStore, template_id: u32) std.mem.Allocator.Error!names.ProcTemplate {
    return .{
        .artifact = .{},
        .proc_base = try name_store.internProcBase(.{
            .module_name = try name_store.internModuleName("SourceDigest"),
            .export_name = null,
            .kind = .checked_source,
            .ordinal = 0,
        }),
        .template = @fromBackingInt(@intCast(template_id)),
    };
}

fn testSourceDigestKey(comptime byte: u8) names.TypeDigest {
    var digest: names.TypeDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}

/// One lifted function carrying `source`, with no body of its own: the source
/// digest reads the template, the clone pattern, and the fusion scope only.
fn addSourceDigestFn(
    program: *Program,
    symbols: *Common.SymbolGen,
    source: Mono.FnTemplate,
    ret_ty: Type.TypeId,
) std.mem.Allocator.Error!FnId {
    return program.addFn(.{
        .symbol = symbols.fresh(),
        .source = source,
        .args = Span(TypedLocal).empty(),
        .captures = Span(TypedLocal).empty(),
        .body = .hosted,
        .ret = ret_ty,
    });
}

test "lifted source digest drops caller provenance and keeps generated bodies apart" {
    // Distinct allocated symbols prove the digest ignores per-program identity.
    var symbols: Common.SymbolGen = .{};
    var program = testLiftedProgram(std.testing.allocator);
    defer program.deinit();

    const ret_ty = try program.types.add(.zst);
    const template = try testSourceDigestTemplate(&program.names, 1);

    // Two requesters reserved one specialization of one checked template at
    // one closed type. They instantiated it from different checked types, and
    // the record keeps whichever reserved it first, so the digest that names
    // the procedure must not read that slot: `specIdentityKey` gives both
    // programs one object-cache key, and an entry written under it carries
    // this digest for the reading program to check.
    const ordinary: Mono.FnTemplate = .{
        .fn_def = .{ .local_template = template },
        .source_fn_ty = @fromBackingInt(@intCast(7)),
        .source_fn_key = testSourceDigestKey(1),
        .mono_fn_ty = ret_ty,
    };
    var other_requester = ordinary;
    other_requester.source_fn_ty = @fromBackingInt(@intCast(9));
    other_requester.source_fn_key = testSourceDigestKey(2);

    const ordinary_fn = try addSourceDigestFn(&program, &symbols, ordinary, ret_ty);
    const other_requester_fn = try addSourceDigestFn(&program, &symbols, other_requester, ret_ty);
    const ordinary_digest = (try program.fnSourceDigest(ordinary_fn)) orelse return error.TestUnexpectedResult;
    const other_requester_digest = (try program.fnSourceDigest(other_requester_fn)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(u8, ordinary_digest[0..], other_requester_digest[0..]);

    // A generated body has no checked declaration to name, so its producer
    // writes the body's own key into that slot instead. Every interpolation
    // step of one expression shares this `fn_def`, its evidence, and its type.
    const first_step: Mono.FnTemplate = .{
        .fn_def = .{ .checked_generated = template },
        .source_fn_ty = @fromBackingInt(@intCast(7)),
        .source_fn_key = testSourceDigestKey(1),
        .mono_fn_ty = ret_ty,
    };
    var second_step = first_step;
    second_step.source_fn_key = testSourceDigestKey(2);
    const first_step_fn = try addSourceDigestFn(&program, &symbols, first_step, ret_ty);
    const second_step_fn = try addSourceDigestFn(&program, &symbols, second_step, ret_ty);
    const first_step_digest = (try program.fnSourceDigest(first_step_fn)) orelse return error.TestUnexpectedResult;
    const second_step_digest = (try program.fnSourceDigest(second_step_fn)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!std.mem.eql(u8, first_step_digest[0..], second_step_digest[0..]));

    // Generated runtime callbacks of one encoder carry the key the same way.
    const first_callback: Mono.FnTemplate = .{
        .fn_def = .{ .encoder_for_runtime = .{ .owner = template, .expr = @fromBackingInt(@intCast(3)) } },
        .source_fn_ty = @fromBackingInt(@intCast(7)),
        .source_fn_key = testSourceDigestKey(1),
        .mono_fn_ty = ret_ty,
    };
    var second_callback = first_callback;
    second_callback.source_fn_key = testSourceDigestKey(2);
    const first_callback_fn = try addSourceDigestFn(&program, &symbols, first_callback, ret_ty);
    const second_callback_fn = try addSourceDigestFn(&program, &symbols, second_callback, ret_ty);
    const first_callback_digest = (try program.fnSourceDigest(first_callback_fn)) orelse return error.TestUnexpectedResult;
    const second_callback_digest = (try program.fnSourceDigest(second_callback_fn)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!std.mem.eql(u8, first_callback_digest[0..], second_callback_digest[0..]));

    // What the digest does name still separates procedures.
    var other_callable = ordinary;
    other_callable.fn_def = .{ .local_template = try testSourceDigestTemplate(&program.names, 2) };
    var other_evidence = ordinary;
    other_evidence.evidence_digest = .{ .bytes = testSourceDigestKey(5).bytes };
    var other_type = ordinary;
    other_type.mono_fn_ty = try program.types.add(.{ .primitive = .str });
    // A body lowered only for discovery may call never-run stubs, so it must
    // never be shared with the evaluation procedure of the same template.
    var discovery = ordinary;
    discovery.discovery = true;
    for ([_]Mono.FnTemplate{ other_callable, other_evidence, other_type, discovery }) |distinct| {
        const distinct_fn = try addSourceDigestFn(&program, &symbols, distinct, ret_ty);
        const distinct_digest = (try program.fnSourceDigest(distinct_fn)) orelse return error.TestUnexpectedResult;
        try std.testing.expect(!std.mem.eql(u8, ordinary_digest[0..], distinct_digest[0..]));
    }

    // A SpecConstr clone shares its source's template and is a distinct
    // procedure at a distinct call pattern.
    const clone_fn = try program.addFn(.{
        .symbol = symbols.fresh(),
        .source = ordinary,
        .spec_constr_pattern = testSourceDigestKey(9),
        .args = Span(TypedLocal).empty(),
        .captures = Span(TypedLocal).empty(),
        .body = .hosted,
        .ret = ret_ty,
    });
    program.next_symbol = symbols.next;
    const clone_digest = (try program.fnSourceDigest(clone_fn)) orelse return error.TestUnexpectedResult;
    try std.testing.expect(!std.mem.eql(u8, ordinary_digest[0..], clone_digest[0..]));
}

test "code reaching the app digests what it reaches there and names its app callees" {
    var symbols: Common.SymbolGen = .{};
    var program = testLiftedProgram(std.testing.allocator);
    defer program.deinit();
    const app_identity = @as([32]u8, @splat(11));
    program.platform_requirement_filling = .{ .app_module = app_identity, .relation = @splat(17) };
    const ret_ty = try program.types.add(.zst);
    const fn_ty = try program.types.add(.{ .func = .{ .args = Type.Span.empty(), .ret = ret_ty } });
    var app_template = try testSourceDigestTemplate(&program.names, 1);
    app_template.artifact.bytes = app_identity;
    const app_source: Mono.FnTemplate = .{
        .fn_def = .{ .local_template = app_template },
        .source_fn_ty = @fromBackingInt(7),
        .source_fn_key = .{},
        .mono_fn_ty = fn_ty,
    };
    var site_app_source = app_source;
    site_app_source.site_identity = testSourceDigestKey(3);
    site_app_source.spec_key = testSourceDigestKey(4);
    const app_fn = try addSourceDigestFn(&program, &symbols, app_source, ret_ty);
    const site_app_fn = try addSourceDigestFn(&program, &symbols, site_app_source, ret_ty);
    const template = try testSourceDigestTemplate(&program.names, 2);
    const ordinary: Mono.FnTemplate = .{
        .fn_def = .{ .local_template = template },
        .source_fn_ty = @fromBackingInt(7),
        .source_fn_key = .{},
        .mono_fn_ty = fn_ty,
    };
    var callers: [2]FnId = undefined;
    for ([_]FnId{ app_fn, site_app_fn }, 0..) |callee, index| {
        const body = try program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .call_proc = .{ .callee = .{ .lifted = callee }, .args = Span(ExprId).empty() } },
        });
        callers[index] = try program.addFn(.{
            .symbol = symbols.fresh(),
            .source = ordinary,
            .args = Span(TypedLocal).empty(),
            .captures = Span(TypedLocal).empty(),
            .body = .{ .roc = body },
            .ret = ret_ty,
        });
    }
    // A function reaching the app through another function reaches it too.
    const outer_body = try program.addExpr(.{
        .ty = ret_ty,
        .data = .{ .call_proc = .{ .callee = .{ .lifted = callers[1] }, .args = Span(ExprId).empty() } },
    });
    const outer = try program.addFn(.{
        .symbol = symbols.fresh(),
        .source = ordinary,
        .args = Span(TypedLocal).empty(),
        .captures = Span(TypedLocal).empty(),
        .body = .{ .roc = outer_body },
        .ret = ret_ty,
    });
    const independent = try addSourceDigestFn(&program, &symbols, ordinary, ret_ty);
    for ([_]FnId{ callers[0], callers[1], outer }) |fn_id| try std.testing.expect(try program.fnReachesApp(fn_id));
    for ([_]FnId{ app_fn, site_app_fn, independent }) |fn_id| try std.testing.expect(!(try program.fnReachesApp(fn_id)));

    var reached = std.ArrayList(FnId).empty;
    defer reached.deinit(std.testing.allocator);
    try program.appCalleesReachedBy(outer, &reached, std.testing.allocator);
    try std.testing.expectEqualSlices(FnId, &.{site_app_fn}, reached.items);

    // A closed app function named by site contributes its own source
    // identity, so a different app filling leaves its callers' digests
    // unchanged; any other app function contributes the relation.
    var before: [3][32]u8 = undefined;
    for ([_]FnId{ callers[0], callers[1], outer }, 0..) |fn_id, index| before[index] = (try program.fnSourceDigest(fn_id)).?;
    try std.testing.expect(!std.mem.eql(u8, &before[1], &(try program.fnSourceDigest(independent)).?));
    program.app_dependencies.?.deinit(std.testing.allocator);
    program.app_dependencies = null;
    program.platform_requirement_filling.?.relation = @splat(18);
    try std.testing.expect(!std.mem.eql(u8, &before[0], &(try program.fnSourceDigest(callers[0])).?));
    try std.testing.expectEqualSlices(u8, &before[1], &(try program.fnSourceDigest(callers[1])).?);
    try std.testing.expectEqualSlices(u8, &before[2], &(try program.fnSourceDigest(outer)).?);
}

test "a folded match resolves only a layout-resolved site to the branch its answer selects" {
    var program = testLiftedProgram(std.testing.allocator);
    defer program.deinit();
    const ty = try program.types.add(.zst);
    const zero: can.CIR.IntValue = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 };
    const one: can.CIR.IntValue = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 };

    const reuse_op = try program.addExpr(.{ .ty = ty, .data = .{ .low_level = .{ .op = .list_map_can_reuse, .args = .empty() } } });
    const reuse_wrapper = try program.addFn(.{ .symbol = @fromBackingInt(@intCast(1)), .args = .empty(), .captures = .empty(), .body = .{ .roc = reuse_op }, .ret = ty });
    const size_op = try program.addExpr(.{ .ty = ty, .data = .{ .low_level = .{ .op = .list_item_size_at_most, .args = .empty() } } });
    const size_wrapper = try program.addFn(.{ .symbol = @fromBackingInt(@intCast(3)), .args = .empty(), .captures = .empty(), .body = .{ .roc = size_op }, .ret = ty });
    const other_body = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = zero } });
    const other_fn = try program.addFn(.{ .symbol = @fromBackingInt(@intCast(2)), .args = .empty(), .captures = .empty(), .body = .{ .roc = other_body }, .ret = ty });

    const reuse_scrutinee = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{ .callee = .{ .lifted = reuse_wrapper }, .args = .empty() } } });
    const size_scrutinee = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{ .callee = .{ .lifted = size_wrapper }, .args = .empty() } } });
    const other_scrutinee = try program.addExpr(.{ .ty = ty, .data = .{ .call_proc = .{ .callee = .{ .lifted = other_fn }, .args = .empty() } } });
    const literal_scrutinee = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = zero } });

    const zero_body = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = one } });
    const other_branch_body = try program.addExpr(.{ .ty = ty, .data = .{ .int_lit = zero } });
    const branches = try program.addBranchSpan(&.{
        .{ .pat = try program.addPat(.{ .ty = ty, .data = .{ .int_lit = zero } }), .body = zero_body },
        .{ .pat = try program.addPat(.{ .ty = ty, .data = .wildcard }), .body = other_branch_body },
    });

    const resolved: FoldedMatch = .{ .scrutinee = reuse_scrutinee, .body = zero_body, .answer = 0 };
    try std.testing.expect(resolved.isResolutionOf(&program, branches));
    try std.testing.expect(resolved.isResolutionOf(program.view(), branches));

    const wrong_branch: FoldedMatch = .{ .scrutinee = reuse_scrutinee, .body = other_branch_body, .answer = 0 };
    try std.testing.expect(!wrong_branch.isResolutionOf(program.view(), branches));
    // A possible reuse never folds.
    const reuse_possible: FoldedMatch = .{ .scrutinee = reuse_scrutinee, .body = other_branch_body, .answer = 1 };
    try std.testing.expect(!reuse_possible.isResolutionOf(program.view(), branches));
    // The item-size check folds to either answer.
    const narrow_items: FoldedMatch = .{ .scrutinee = size_scrutinee, .body = other_branch_body, .answer = 1 };
    try std.testing.expect(narrow_items.isResolutionOf(program.view(), branches));
    const wide_items: FoldedMatch = .{ .scrutinee = size_scrutinee, .body = zero_body, .answer = 0 };
    try std.testing.expect(wide_items.isResolutionOf(program.view(), branches));
    const wide_wrong_branch: FoldedMatch = .{ .scrutinee = size_scrutinee, .body = other_branch_body, .answer = 0 };
    try std.testing.expect(!wide_wrong_branch.isResolutionOf(program.view(), branches));
    const user_call: FoldedMatch = .{ .scrutinee = other_scrutinee, .body = zero_body, .answer = 0 };
    try std.testing.expect(!user_call.isResolutionOf(program.view(), branches));
    const not_a_call: FoldedMatch = .{ .scrutinee = literal_scrutinee, .body = zero_body, .answer = 0 };
    try std.testing.expect(!not_a_call.isResolutionOf(program.view(), branches));

    const binding = try program.addStmtSpan(&.{try program.addStmt(.{ .expr = other_branch_body })});
    const binding_branches = try program.addBranchSpan(&.{
        .{ .pat = try program.addPat(.{ .ty = ty, .data = .{ .int_lit = zero } }), .bindings = binding, .body = zero_body },
    });
    try std.testing.expect(!resolved.isResolutionOf(program.view(), binding_branches));
}

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = @import("body_shard_test.zig");
}

/// `site_identity` names the function's template (its own or its nested
/// owner's) in place of the template's module content key and ordinals
/// (`Mono.FnTemplate.site_identity`).
fn writeFnDefDigest(hasher: *TypeDigestHasher, name_store: *const names.NameStore, fn_def: Mono.FnDef, site_identity: ?names.TypeDigest) void {
    // Whether a template was requested from its own module or from an
    // importer changes nothing about the code it lowers to, so both spellings
    // digest alike; otherwise a package's pack and the apps that import it
    // would name the same specialization differently.
    writeIdentityBytes(hasher, switch (fn_def) {
        .local_template, .imported_template => "template",
        .local_hosted, .imported_hosted => "hosted",
        .nested => "nested",
        .checked_generated => "checked_generated",
        .parser_runtime => "parser_runtime",
        .encoder_for_runtime => "encoder_for_runtime",
    });
    switch (fn_def) {
        .local_template, .imported_template, .checked_generated => |template| writeTemplateDigest(hasher, template, site_identity),
        .nested => |nested| {
            writeTemplateDigest(hasher, nested.owner, site_identity);
            writeIdentityU32(hasher, @backingInt(nested.site));
            if (nested.default_root) |root| {
                writeIdentityBytes(hasher, "default-root");
                hasher.update(&root.bytes);
            } else {
                writeIdentityBytes(hasher, "template-site");
            }
            hasher.update(&nested.context_fn_key.bytes);
            if (nested.local_proc_context_digest) |digest| {
                writeIdentityBytes(hasher, "local-proc-context");
                hasher.update(&digest.bytes);
            } else {
                writeIdentityBytes(hasher, "no-local-proc-context");
            }
        },
        // A hosted function is named by its external symbol; its dispatch
        // slot is assigned per program and is not part of the code it names.
        .local_hosted, .imported_hosted => |hosted_fn| {
            writeProcTemplateDigest(hasher, hosted_fn.template);
            writeIdentityBytes(hasher, name_store.externalSymbolNameText(hosted_fn.external_symbol_name));
        },
        .parser_runtime => |runtime| {
            writeProcTemplateDigest(hasher, runtime.owner);
            writeIdentityU32(hasher, @backingInt(runtime.expr));
        },
        .encoder_for_runtime => |runtime| {
            writeProcTemplateDigest(hasher, runtime.owner);
            writeIdentityU32(hasher, @backingInt(runtime.expr));
        },
    }
}

fn writeTemplateDigest(hasher: *TypeDigestHasher, template: names.ProcTemplate, site_identity: ?names.TypeDigest) void {
    if (site_identity) |site| {
        writeIdentityBytes(hasher, "site");
        hasher.update(&site.bytes);
    } else {
        writeIdentityBytes(hasher, "content");
        writeProcTemplateDigest(hasher, template);
    }
}

fn writeProcTemplateDigest(hasher: *TypeDigestHasher, template: names.ProcTemplate) void {
    hasher.update(&template.artifact.bytes);
    writeIdentityU32(hasher, @backingInt(template.proc_base));
    writeIdentityU32(hasher, @backingInt(template.template));
}

fn writeIdentityBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeIdentityU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeIdentityU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
    hasher.update(&buffer);
}
