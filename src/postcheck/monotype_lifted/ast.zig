//! Monotype Lifted IR.
//!
//! This stage uses the Monotype type store, but no expression-position lambda
//! remains. Every function body is stored as a lifted function with explicit
//! capture locals.

const std = @import("std");
const base = @import("base");
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

    pub const none: InlineScopeId = @enumFromInt(std.math.maxInt(u32));
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
    layout_requests: []const LayoutRequest,
    runtime_schema_requests: []const RuntimeSchemaRequest,
    static_data_values: []const StaticDataValue,
    comptime_value_roots: []const Common.ComptimeValueRoot,
    comptime_sites: []const ComptimeSite,
    source_files: []const base.SourceFileEntry,
    expr_locs: []const base.SourceLoc,
    expr_regions: []const base.Region,
    stmt_locs: []const base.SourceLoc,
    stmt_regions: []const base.Region,
    inline_scopes: []const InlineScope,
    expr_inline_scopes: []const InlineScopeId,
    stmt_inline_scopes: []const InlineScopeId,
    local_names: []const []const u8,

    pub fn getComptimeValueRoot(self: ProgramView, id: Common.ComptimeValueRootId) Common.ComptimeValueRoot {
        return self.comptime_value_roots[@intFromEnum(id)];
    }

    pub fn procDebugName(self: ProgramView, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    pub fn exprLoc(self: ProgramView, id: ExprId) base.SourceLoc {
        return self.expr_locs[@intFromEnum(id)];
    }

    pub fn exprRegion(self: ProgramView, id: ExprId) base.Region {
        return self.expr_regions[@intFromEnum(id)];
    }

    pub fn stmtLoc(self: ProgramView, id: StmtId) base.SourceLoc {
        return self.stmt_locs[@intFromEnum(id)];
    }

    pub fn stmtRegion(self: ProgramView, id: StmtId) base.Region {
        return self.stmt_regions[@intFromEnum(id)];
    }

    pub fn exprInlineScope(self: ProgramView, id: ExprId) InlineScopeId {
        return self.expr_inline_scopes[@intFromEnum(id)];
    }

    pub fn stmtInlineScope(self: ProgramView, id: StmtId) InlineScopeId {
        return self.stmt_inline_scopes[@intFromEnum(id)];
    }

    pub fn inlineScope(self: ProgramView, id: InlineScopeId) InlineScope {
        return self.inline_scopes[@intFromEnum(id)];
    }

    pub fn comptimeSite(self: ProgramView, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites[@intFromEnum(id)];
    }

    pub fn localName(self: ProgramView, id: LocalId) []const u8 {
        return self.local_names[@intFromEnum(id)];
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: ProgramView, id: LocalId) check.CheckedModule.CaptureId {
        return self.locals[@intFromEnum(id)].capture_id orelse
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

    pub fn fieldAccessSegmentAt(self: ProgramView, span_: Span(FieldAccessSegment), index: usize) FieldAccessSegment {
        if (index >= span_.len) Common.invariant("field access segment index was outside span");
        return self.field_access_segments[span_.start + index];
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

    pub fn ifBranchSpan(self: ProgramView, span_: Span(IfBranch)) []const IfBranch {
        return self.if_branches[span_.start..][0..span_.len];
    }

    pub fn exprCount(self: ProgramView) usize {
        return self.exprs.len;
    }

    pub fn patCount(self: ProgramView) usize {
        return self.pats.len;
    }

    pub fn stmtCount(self: ProgramView) usize {
        return self.stmts.len;
    }

    pub fn localCount(self: ProgramView) usize {
        return self.locals.len;
    }

    pub fn exprTy(self: ProgramView, id: ExprId) Type.TypeId {
        return self.exprs[@intFromEnum(id)].ty;
    }

    pub fn patTy(self: ProgramView, id: PatId) Type.TypeId {
        return self.pats[@intFromEnum(id)].ty;
    }

    pub fn pat(self: ProgramView, id: PatId) Pat {
        return self.pats[@intFromEnum(id)];
    }

    pub fn stmt(self: ProgramView, id: StmtId) Stmt {
        return self.stmts[@intFromEnum(id)];
    }

    /// The two pieces direct LIR lowering needs to consider folding away the
    /// in-place `List.map` branch: the `list_map_can_reuse` call's arguments
    /// (to compute layout eligibility) and the body a constant-0 scrutinee
    /// selects.
    pub const ListMapCanReuseMatch = struct {
        call_args: Span(ExprId),
        zero_branch_body: ExprId,
    };

    /// Recognizes the `List.map` reuse match: a match whose scrutinee calls
    /// the Builtin `list_map_can_reuse` wrapper, with guard-free
    /// integer-literal and wildcard branches. Returns null for any other
    /// shape. Whether to fold is the caller's layout-aware decision; this
    /// only identifies the site and the branch a constant 0 reaches.
    pub fn listMapCanReuseMatch(
        self: ProgramView,
        scrutinee: ExprId,
        branches_span: Span(Branch),
    ) ?ListMapCanReuseMatch {
        const scrutinee_data = self.exprs[@intFromEnum(scrutinee)].data;
        if (std.meta.activeTag(scrutinee_data) != .call_proc) return null;
        const call = scrutinee_data.call_proc;
        const callee = switch (call.callee) {
            .lifted => |fn_id| fn_id,
            .func => return null,
        };
        const callee_body = switch (self.fns[@intFromEnum(callee)].body) {
            .roc => |body| body,
            .hosted => return null,
        };
        if (!self.exprIsListMapCanReuseOp(callee_body)) return null;

        for (self.branchSpan(branches_span)) |branch| {
            if (branch.guard != null or branch.bindings.len != 0) return null;
            const pat_data = self.pats[@intFromEnum(branch.pat)].data;
            const tag = std.meta.activeTag(pat_data);
            if (tag == .wildcard or (tag == .int_lit and pat_data.int_lit.toI128() == 0)) {
                return .{ .call_args = call.args, .zero_branch_body = branch.body };
            }
            return null;
        }
        return null;
    }

    fn exprIsListMapCanReuseOp(self: ProgramView, expr_id: ExprId) bool {
        const data = self.exprs[@intFromEnum(expr_id)].data;
        const tag = std.meta.activeTag(data);
        if (tag == .low_level) return data.low_level.op == .list_map_can_reuse;
        return tag == .block and data.block.statements.len == 0 and self.exprIsListMapCanReuseOp(data.block.final_expr);
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
    roots: ProgramList(Root, "roots"),
    layout_requests: ProgramList(LayoutRequest, "layout_requests"),
    runtime_schema_requests: ProgramList(RuntimeSchemaRequest, "runtime_schema_requests"),
    static_data_values: ProgramList(StaticDataValue, "static_data_values"),
    /// Frozen shared metadata for SpecConstr; workers never append descriptors.
    comptime_value_roots: ProgramList(Common.ComptimeValueRoot, "comptime_value_roots") = .empty,
    comptime_sites: ProgramList(ComptimeSite, "comptime_sites"),
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
    local_names: ProgramList([]const u8, "local_names"),
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
        result.proc_debug_names = ProcDebugNameMap.init(allocator);
        result.current_loc = self.current_loc;
        result.current_region = self.current_region;
        result.current_inline_scope = self.current_inline_scope;
        return result;
    }

    /// Commit one completed private body in coordinator order. Failure leaves
    /// all logical rows and the selected function unchanged.
    pub fn appendSpecConstrBody(self: *Program, worker: *const Program, source_symbol_start: u32, symbol_offset: u32, source_join_start: u32, join_offset: u32) std.mem.Allocator.Error!void {
        return BodyShard.append(self, worker, source_symbol_start, symbol_offset, source_join_start, join_offset);
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
        local_names: std.ArrayList([]const u8),
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
            .layout_requests = .empty,
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
            .local_names = ProgramList([]const u8, "local_names").fromArrayList(local_names),
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
            .current_inline_scope = InlineScopeId.none,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.unsafeRawItemsForView()) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
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
        for (self.comptime_sites.unsafeRawItemsForView()) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.comptime_value_roots.deinit(self.allocator);
        self.static_data_values.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
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
            .layout_requests = self.layout_requests.unsafeRawItemsForView(),
            .runtime_schema_requests = self.runtime_schema_requests.unsafeRawItemsForView(),
            .static_data_values = self.static_data_values.unsafeRawItemsForView(),
            .comptime_value_roots = self.comptime_value_roots.unsafeRawItemsForView(),
            .comptime_sites = self.comptime_sites.unsafeRawItemsForView(),
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
        return self.row("comptime_value_roots", @intFromEnum(id));
    }

    pub fn addComptimeValueRoot(self: *Program, root: Common.ComptimeValueRoot) std.mem.Allocator.Error!Common.ComptimeValueRootId {
        if (self.body_prefix != null) Common.invariant("body shard attempted to append frozen compile-time metadata");
        const id: Common.ComptimeValueRootId = @enumFromInt(@as(u32, @intCast(self.comptime_value_roots.len())));
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
        inline for (std.meta.fields(SpecConstrAnalysisMark)) |field| {
            if (comptime !std.mem.eql(u8, field.name, "body_patch")) {
                @field(mark, field.name) += self.prefixLen(field.name);
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
        inline for (std.meta.fields(SpecConstrAnalysisMark)) |field| {
            if (comptime !std.mem.eql(u8, field.name, "body_patch")) {
                @field(mark, field.name) = self.ownedIndex(field.name, @field(mark, field.name));
            }
        }
        if (self.body_prefix) |*prefix| prefix.patch = mark.body_patch.?;
        return mark;
    }

    fn freeAnalysisStrings(self: *Program, start: usize) void {
        for (self.string_literals.unsafeRawItemsForView()[start..]) |literal| literal.deinit(self.allocator);
    }

    fn freeAnalysisLocalNames(self: *Program, start: usize) void {
        for (self.local_names.unsafeRawItemsForView()[start..]) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        std.debug.assert(self.body_prefix == null);
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.len())));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn reserveFnSlot(self: *Program) std.mem.Allocator.Error!FnId {
        std.debug.assert(self.body_prefix == null);
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.len())));
        try self.fns.append(self.allocator, undefined);
        return id;
    }

    pub fn setFn(self: *Program, id: FnId, fn_: Fn) void {
        if (self.body_prefix) |*prefix| {
            if (id != prefix.source_fn) Common.invariant("body shard patched another function");
            prefix.patch = fn_;
            return;
        }
        self.fns.set(@intFromEnum(id), fn_);
    }

    pub fn setFnAt(self: *Program, index: usize, fn_: Fn) void {
        self.setFn(@enumFromInt(index), fn_);
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

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprCount())));
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
        return self.row("expr_locs", @intFromEnum(id));
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const Program, id: ExprId) base.Region {
        return self.row("expr_regions", @intFromEnum(id));
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.row("stmt_locs", @intFromEnum(id));
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const Program, id: StmtId) base.Region {
        return self.row("stmt_regions", @intFromEnum(id));
    }

    pub fn exprInlineScope(self: *const Program, id: ExprId) InlineScopeId {
        return self.row("expr_inline_scopes", @intFromEnum(id));
    }

    pub fn stmtInlineScope(self: *const Program, id: StmtId) InlineScopeId {
        return self.row("stmt_inline_scopes", @intFromEnum(id));
    }

    pub fn inlineScope(self: *const Program, id: InlineScopeId) InlineScope {
        return self.row("inline_scopes", @intFromEnum(id));
    }

    pub fn addInlineScope(self: *Program, scope: InlineScope) std.mem.Allocator.Error!InlineScopeId {
        const id: InlineScopeId = @enumFromInt(@as(u32, @intCast(self.rowCount("inline_scopes"))));
        try self.inline_scopes.append(self.allocator, scope);
        return id;
    }

    pub fn addPat(self: *Program, pat_: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.patCount())));
        try self.pats.append(self.allocator, pat_);
        return id;
    }

    pub fn addStmt(self: *Program, stmt_: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmtCount())));
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
        return self.row("comptime_sites", @intFromEnum(id));
    }

    pub fn comptimeSiteCount(self: *const Program) usize {
        return self.rowCount("comptime_sites");
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const Program, id: LocalId) []const u8 {
        return self.row("local_names", @intFromEnum(id));
    }

    /// Replace a suffix local's diagnostic name with independently owned bytes.
    pub fn setLocalName(self: *Program, id: LocalId, text: []const u8) std.mem.Allocator.Error!void {
        const index = self.ownedIndex("local_names", @intFromEnum(id));
        const owned = if (text.len == 0) "" else try self.allocator.dupe(u8, text);
        const old = self.local_names.get(index);
        if (old.len > 0) self.allocator.free(old);
        self.local_names.set(index, owned);
    }

    pub fn sourceFiles(self: *const Program) []const base.SourceFileEntry {
        if (self.body_prefix) |prefix| return prefix.source.sourceFiles();
        return self.source_files.unsafeRawItemsForView();
    }

    pub fn takeStringLiterals(self: *Program) std.ArrayList(Mono.StringLiteral) {
        std.debug.assert(self.body_prefix == null);
        return self.string_literals.takeArrayList();
    }

    pub fn takeSourceFiles(self: *Program) std.ArrayList(base.SourceFileEntry) {
        std.debug.assert(self.body_prefix == null);
        return self.source_files.takeArrayList();
    }

    pub fn takeStaticDataValues(self: *Program) std.ArrayList(StaticDataValue) {
        std.debug.assert(self.body_prefix == null);
        return self.static_data_values.takeArrayList();
    }

    pub fn stringLiteralsView(self: *const Program) []const Mono.StringLiteral {
        std.debug.assert(self.body_prefix == null);
        return self.string_literals.unsafeRawItemsForView();
    }

    pub fn rootCount(self: *const Program) usize {
        return self.rowCount("roots");
    }

    pub fn rootsView(self: *const Program) []const Root {
        if (self.body_prefix) |prefix| return prefix.source.rootsView();
        return self.roots.unsafeRawItemsForView();
    }

    pub fn fnCount(self: *const Program) usize {
        return self.rowCount("fns");
    }

    pub fn getFn(self: *const Program, id: FnId) Fn {
        if (self.body_prefix) |prefix| {
            if (id == prefix.source_fn) return prefix.patch;
        }
        return self.row("fns", @intFromEnum(id));
    }

    /// Content digest of a lifted function's checked source identity: which
    /// checked callable it came from, its checked source type, its dispatch
    /// evidence, the Monotype type it was requested at, and for a SpecConstr
    /// clone the call pattern it was cloned for. Nothing here depends on
    /// per-program numbering, so the same specialization digests identically
    /// in every program. Null for a function with no checked source.
    pub fn fnSourceDigest(self: *Program, fn_id: FnId) ?[TypeDigestHasher.digest_length]u8 {
        const fn_ = self.getFn(fn_id);
        var hasher = TypeDigestHasher.init();
        writeIdentityBytes(&hasher, "roc.lifted.fn-source.v1");
        if (fn_.source) |template| {
            writeIdentityBytes(&hasher, "template");
            writeFnDefDigest(&hasher, &self.names, template.fn_def);
            hasher.update(&template.source_fn_key.bytes);
            hasher.update(&template.evidence_digest.bytes);
            // The equality digest, not the stored-identity digest: the latter
            // names a nominal type by the checked type id of whichever
            // module's store lowered it, and the same specialization lowered
            // by two programs must have one identity.
            const mono_digest = self.types.equalityDigest(&self.names, template.mono_fn_ty);
            hasher.update(&mono_digest.bytes);
        } else {
            const root = fn_.root_identity orelse return null;
            writeIdentityBytes(&hasher, "root");
            hasher.update(&root.bytes);
        }
        if (fn_.spec_constr_pattern) |pattern| {
            writeIdentityBytes(&hasher, "spec-constr-clone");
            hasher.update(&pattern.bytes);
        } else {
            writeIdentityBytes(&hasher, "source");
        }
        writeIdentityBytes(&hasher, if (fn_.iterator_fusion_scope) "iterator-fusion" else "plain");
        return hasher.finalResult();
    }

    pub fn getFnAt(self: *const Program, index: usize) Fn {
        return self.getFn(@enumFromInt(index));
    }

    pub fn fnsView(self: *const Program) []const Fn {
        std.debug.assert(self.body_prefix == null);
        return self.fns.unsafeRawItemsForView();
    }

    pub fn getExpr(self: *const Program, id: ExprId) Expr {
        return self.row("exprs", @intFromEnum(id));
    }

    pub fn exprsView(self: *const Program) []const Expr {
        std.debug.assert(self.body_prefix == null);
        return self.exprs.unsafeRawItemsForView();
    }

    pub fn setExpr(self: *Program, id: ExprId, expr: Expr) void {
        self.exprs.set(self.ownedIndex("exprs", @intFromEnum(id)), expr);
    }

    pub fn getExprAt(self: *const Program, index: usize) Expr {
        return self.row("exprs", index);
    }

    pub fn setExprData(self: *Program, id: ExprId, data: ExprData) void {
        self.setExprDataAt(@intFromEnum(id), data);
    }

    pub fn setExprDataAt(self: *Program, index: usize, data: ExprData) void {
        self.exprs.getPtrImmediate(self.ownedIndex("exprs", index)).data = data;
    }

    pub fn getPat(self: *const Program, id: PatId) Pat {
        return self.row("pats", @intFromEnum(id));
    }

    pub fn getPatAt(self: *const Program, index: usize) Pat {
        return self.row("pats", index);
    }

    pub fn getStmt(self: *const Program, id: StmtId) Stmt {
        return self.row("stmts", @intFromEnum(id));
    }

    pub fn getStmtAt(self: *const Program, index: usize) Stmt {
        return self.row("stmts", index);
    }

    pub fn stmtsView(self: *const Program) []const Stmt {
        std.debug.assert(self.body_prefix == null);
        return self.stmts.unsafeRawItemsForView();
    }

    pub fn getLocal(self: *const Program, id: LocalId) Local {
        return self.row("locals", @intFromEnum(id));
    }

    pub fn getLocalAt(self: *const Program, index: usize) Local {
        return self.row("locals", index);
    }

    pub fn localsView(self: *const Program) []const Local {
        std.debug.assert(self.body_prefix == null);
        return self.locals.unsafeRawItemsForView();
    }

    pub fn getStringLiteral(self: *const Program, id: StringLiteralId) Mono.StringLiteral {
        return self.row("string_literals", @intFromEnum(id));
    }

    pub fn addStringLiteral(self: *Program, text: []const u8) std.mem.Allocator.Error!StringLiteralId {
        const id: StringLiteralId = @enumFromInt(@as(u32, @intCast(self.rowCount("string_literals"))));
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

    pub fn addLayoutRequest(self: *Program, request: LayoutRequest) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.layout_requests.append(self.allocator, request);
    }

    pub fn addRuntimeSchemaRequest(self: *Program, request: RuntimeSchemaRequest) std.mem.Allocator.Error!void {
        std.debug.assert(self.body_prefix == null);
        try self.runtime_schema_requests.append(self.allocator, request);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.localCount())));
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
        try self.local_names.append(self.allocator, "");
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

        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.localCount())));
        try self.locals.ensureUnusedCapacity(self.allocator, 1);
        try self.local_names.ensureUnusedCapacity(self.allocator, 1);
        try self.locals.append(self.allocator, .{
            .id = id,
            .symbol = symbol,
            .ty = ty,
            .binder = binder,
            .capture_id = capture_id,
            .checked_capture_id = checked_capture_id,
        });
        try self.local_names.append(self.allocator, "");
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
        const local = self.locals.getPtrImmediate(@intFromEnum(id));
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

    /// The two pieces direct LIR lowering needs to consider folding away the
    /// in-place `List.map` branch: the `list_map_can_reuse` call's arguments
    /// (to compute layout eligibility) and the body a constant-0 scrutinee
    /// selects.
    pub const ListMapCanReuseMatch = struct {
        call_args: Span(ExprId),
        zero_branch_body: ExprId,
    };

    /// Recognizes the `List.map` reuse match: a match whose scrutinee calls
    /// the Builtin `list_map_can_reuse` wrapper, with guard-free
    /// integer-literal and wildcard branches. Returns null for any other
    /// shape. Whether to fold is the caller's layout-aware decision; this
    /// only identifies the site and the branch a constant 0 reaches.
    pub fn listMapCanReuseMatch(
        self: *const Program,
        scrutinee: ExprId,
        branches_span: Span(Branch),
    ) ?ListMapCanReuseMatch {
        const scrutinee_data = self.getExpr(scrutinee).data;
        if (std.meta.activeTag(scrutinee_data) != .call_proc) return null;
        const call = scrutinee_data.call_proc;
        const callee = switch (call.callee) {
            .lifted => |fn_id| fn_id,
            .func => return null,
        };
        const callee_body = switch (self.getFn(callee).body) {
            .roc => |body| body,
            .hosted => return null,
        };
        if (!self.exprIsListMapCanReuseOp(callee_body)) return null;

        const branches = self.branchSpan(branches_span);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.guard != null or branch.bindings.len != 0) return null;
            const pat_data = self.getPat(branch.pat).data;
            const tag = std.meta.activeTag(pat_data);
            if (tag == .wildcard or (tag == .int_lit and pat_data.int_lit.toI128() == 0)) {
                return .{ .call_args = call.args, .zero_branch_body = branch.body };
            }
            return null;
        }
        return null;
    }

    /// One match statically resolved by direct LIR lowering, recorded so the
    /// debug Lambda Mono materializer replays the identical resolution and
    /// the two derivations demand the same set of functions. Keyed by the
    /// match's scrutinee expression, which belongs to exactly one match.
    pub const FoldedMatch = struct {
        scrutinee: ExprId,
        body: ExprId,
    };

    fn exprIsListMapCanReuseOp(self: *const Program, expr_id: ExprId) bool {
        const data = self.getExpr(expr_id).data;
        const tag = std.meta.activeTag(data);
        if (tag == .low_level) return data.low_level.op == .list_map_can_reuse;
        return tag == .block and data.block.statements.len == 0 and self.exprIsListMapCanReuseOp(data.block.final_expr);
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

    pub fn exprTy(self: *const Program, id: ExprId) Type.TypeId {
        return self.getExpr(id).ty;
    }

    pub fn patTy(self: *const Program, id: PatId) Type.TypeId {
        return self.getPat(id).ty;
    }

    pub fn pat(self: *const Program, id: PatId) Pat {
        return self.getPat(id);
    }

    pub fn stmt(self: *const Program, id: StmtId) Stmt {
        return self.getStmt(id);
    }
};

/// Visit every local a pattern binds, innermost first, in binding order.
///
/// Three consumers need this walk—the lift pass's bound-set scan, its capture
/// graph builder, and SpecConstr's body-local scope—and they differ only in
/// what they do at a binding site, never in which pattern positions bind. The
/// walk lives here so a new `PatData` variant is one edit, and so the three
/// cannot come to disagree about, say, whether a list rest pattern binds.
///
/// `binder` is any value exposing `bindLocal(LocalId) !void`.
pub fn forEachBoundLocal(program: *const Program, pat_id: PatId, binder: anytype) std.mem.Allocator.Error!void {
    switch (program.getPat(pat_id).data) {
        .bind => |local| try binder.bindLocal(local),
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
                    try forEachBoundLocal(program, capture, binder);
                }
            }
        },
        .as => |as| {
            try forEachBoundLocal(program, as.pattern, binder);
            try binder.bindLocal(as.local);
        },
        .record => |fields| {
            const destructs = program.recordDestructSpan(fields);
            for (0..destructs.len) |index| {
                try forEachBoundLocal(program, GuardedList.at(destructs, index).pattern, binder);
            }
        },
        .tuple => |items| {
            const children = program.patSpan(items);
            for (0..children.len) |index| {
                try forEachBoundLocal(program, GuardedList.at(children, index), binder);
            }
        },
        .list => |list| {
            const children = program.patSpan(list.patterns);
            for (0..children.len) |index| {
                try forEachBoundLocal(program, GuardedList.at(children, index), binder);
            }
            if (list.rest) |rest| if (rest.pattern) |rest_pattern| {
                try forEachBoundLocal(program, rest_pattern, binder);
            };
        },
        .tag => |tag| {
            const payloads = program.patSpan(tag.payloads);
            for (0..payloads.len) |index| {
                try forEachBoundLocal(program, GuardedList.at(payloads, index), binder);
            }
        },
        .nominal => |backing| try forEachBoundLocal(program, backing, binder),
    }
}

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = @import("body_shard_test.zig");
}

fn writeFnDefDigest(hasher: *TypeDigestHasher, name_store: *const names.NameStore, fn_def: Mono.FnDef) void {
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
        .local_template, .imported_template, .checked_generated => |template| writeProcTemplateDigest(hasher, template),
        .nested => |nested| {
            writeProcTemplateDigest(hasher, nested.owner);
            writeIdentityU32(hasher, @intFromEnum(nested.site));
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
            writeIdentityU32(hasher, @intFromEnum(runtime.expr));
        },
        .encoder_for_runtime => |runtime| {
            writeProcTemplateDigest(hasher, runtime.owner);
            writeIdentityU32(hasher, @intFromEnum(runtime.expr));
        },
    }
}

fn writeProcTemplateDigest(hasher: *TypeDigestHasher, template: names.ProcTemplate) void {
    hasher.update(&template.artifact.bytes);
    writeIdentityU32(hasher, @intFromEnum(template.proc_base));
    writeIdentityU32(hasher, @intFromEnum(template.template));
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
