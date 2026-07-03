//! Monotype Lifted IR.
//!
//! This stage uses the Monotype type store, but no expression-position lambda
//! remains. Every function body is stored as a lifted function with explicit
//! capture locals.

const std = @import("std");
const base = @import("base");
const check = @import("check");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const Type = @import("../monotype/type.zig");
const names = check.CheckedNames;

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
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = Mono.RuntimeSchemaRequest;
/// Function imported from another Monotype shard.
pub const ImportedFn = Mono.ImportedFn;
/// Identifier for an imported function table entry.
pub const ImportedFnId = Mono.ImportedFnId;

/// Read-only Monotype Lifted program view.
///
/// Today this view borrows `Program` arrays. Lambda Solved consumes this shape
/// so later cache-backed or builder-split lifted programs do not require a
/// consumer rewrite.
pub const ProgramView = struct {
    names: *const names.NameStore,
    next_symbol: u32,
    types: Type.Store.View,
    imported_fns: []const ImportedFn,
    fns: []const Fn,
    exprs: []const Expr,
    pats: []const Pat,
    stmts: []const Stmt,
    locals: []const Local,
    expr_ids: []const ExprId,
    pat_ids: []const PatId,
    typed_locals: []const TypedLocal,
    stmt_ids: []const StmtId,
    field_exprs: []const FieldExpr,
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
    comptime_sites: []const ComptimeSite,
    source_files: []const []const u8,
    expr_locs: []const base.SourceLoc,
    expr_regions: []const base.Region,
    stmt_locs: []const base.SourceLoc,
    stmt_regions: []const base.Region,
    local_names: []const []const u8,

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
        const call = switch (self.exprs[@intFromEnum(scrutinee)].data) {
            .call_proc => |call| call,
            else => return null,
        };
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
            if (branch.guard != null) return null;
            switch (self.pats[@intFromEnum(branch.pat)].data) {
                .wildcard => return .{ .call_args = call.args, .zero_branch_body = branch.body },
                .int_lit => |value| if (value.toI128() == 0) {
                    return .{ .call_args = call.args, .zero_branch_body = branch.body };
                },
                else => return null,
            }
        }
        return null;
    }

    fn exprIsListMapCanReuseOp(self: ProgramView, expr_id: ExprId) bool {
        return switch (self.exprs[@intFromEnum(expr_id)].data) {
            .low_level => |ll| ll.op == .list_map_can_reuse,
            .block => |block| block.statements.len == 0 and self.exprIsListMapCanReuseOp(block.final_expr),
            else => false,
        };
    }
};

/// Direct call target after Monotype lifting.
pub const DirectCallee = union(enum(u8)) {
    local: FnId,
    imported: ImportedFnId,
};

/// Return the lifted direct-call target after Monotype lifting.
pub fn directCallee(call: Mono.CallProc) DirectCallee {
    return switch (call.callee) {
        .lifted => |fn_id| .{ .local = fn_id },
        .func => |slot| switch (slot) {
            .local => Common.invariant("Monotype Lifted direct call still referenced a Monotype function id"),
            .imported => |imported| .{ .imported = imported },
        },
    };
}

/// Return the local lifted function id for a direct call, or null when it
/// targets an imported shard.
pub fn localDirectCallee(call: Mono.CallProc) ?FnId {
    return switch (directCallee(call)) {
        .local => |fn_id| fn_id,
        .imported => null,
    };
}

/// Complete Monotype Lifted program plus side arrays.
pub const Program = struct {
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    imported_fns: std.ArrayList(ImportedFn),
    fns: std.ArrayList(Fn),
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    stmts: std.ArrayList(Stmt),
    locals: std.ArrayList(Local),
    expr_ids: std.ArrayList(ExprId),
    pat_ids: std.ArrayList(PatId),
    typed_locals: std.ArrayList(TypedLocal),
    stmt_ids: std.ArrayList(StmtId),
    field_exprs: std.ArrayList(FieldExpr),
    fn_def_captures: std.ArrayList(FnDefCapture),
    /// Backing pool for `Span(CaptureOperand)` capture operand spans on lifted
    /// `fn_ref`/`call_proc` nodes.
    capture_operands: std.ArrayList(CaptureOperand),
    record_destructs: std.ArrayList(RecordDestruct),
    str_pattern_steps: std.ArrayList(Mono.StrPatternStep),
    branches: std.ArrayList(Branch),
    if_branches: std.ArrayList(IfBranch),
    string_literals: std.ArrayList(Mono.StringLiteral),
    proc_debug_names: ProcDebugNameMap,
    /// Next generated `CaptureId` index for a lift-synthesized capturable local.
    next_lift_capture_id: u32,
    roots: std.ArrayList(Root),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),
    comptime_sites: std.ArrayList(ComptimeSite),
    /// Source file table for `SourceLoc.file` indices (moved from Monotype).
    source_files: std.ArrayList([]const u8),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: std.ArrayList(base.SourceLoc),
    /// Checked source region per expression, parallel to `exprs`.
    expr_regions: std.ArrayList(base.Region),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: std.ArrayList(base.SourceLoc),
    /// Checked source region per statement, parallel to `stmts`.
    stmt_regions: std.ArrayList(base.Region),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; moved from Monotype).
    local_names: std.ArrayList([]const u8),
    /// Ambient location recorded by `addExpr`/`addStmt`. Passes that add
    /// nodes set this so synthetic nodes inherit a source location.
    current_loc: base.SourceLoc,
    /// Ambient checked source region recorded by `addExpr`/`addStmt`.
    current_region: base.Region,

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: names.NameStore,
        types: Type.Store,
        imported_fns: std.ArrayList(ImportedFn),
        exprs: std.ArrayList(Expr),
        pats: std.ArrayList(Pat),
        stmts: std.ArrayList(Stmt),
        locals: std.ArrayList(Local),
        expr_ids: std.ArrayList(ExprId),
        pat_ids: std.ArrayList(PatId),
        typed_locals: std.ArrayList(TypedLocal),
        stmt_ids: std.ArrayList(StmtId),
        field_exprs: std.ArrayList(FieldExpr),
        fn_def_captures: std.ArrayList(FnDefCapture),
        record_destructs: std.ArrayList(RecordDestruct),
        str_pattern_steps: std.ArrayList(Mono.StrPatternStep),
        branches: std.ArrayList(Branch),
        if_branches: std.ArrayList(IfBranch),
        string_literals: std.ArrayList(Mono.StringLiteral),
        proc_debug_names: ProcDebugNameMap,
        source_files: std.ArrayList([]const u8),
        expr_locs: std.ArrayList(base.SourceLoc),
        expr_regions: std.ArrayList(base.Region),
        stmt_locs: std.ArrayList(base.SourceLoc),
        stmt_regions: std.ArrayList(base.Region),
        local_names: std.ArrayList([]const u8),
        comptime_sites: std.ArrayList(ComptimeSite),
        next_symbol: u32,
    ) Program {
        return .{
            .allocator = allocator,
            .names = name_store,
            .next_symbol = next_symbol,
            .types = types,
            .imported_fns = imported_fns,
            .fns = .empty,
            .exprs = exprs,
            .pats = pats,
            .stmts = stmts,
            .locals = locals,
            .expr_ids = expr_ids,
            .pat_ids = pat_ids,
            .typed_locals = typed_locals,
            .stmt_ids = stmt_ids,
            .field_exprs = field_exprs,
            .fn_def_captures = fn_def_captures,
            .capture_operands = .empty,
            .record_destructs = record_destructs,
            .str_pattern_steps = str_pattern_steps,
            .branches = branches,
            .if_branches = if_branches,
            .string_literals = string_literals,
            .proc_debug_names = proc_debug_names,
            .next_lift_capture_id = 0,
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .comptime_sites = comptime_sites,
            .source_files = source_files,
            .expr_locs = expr_locs,
            .expr_regions = expr_regions,
            .stmt_locs = stmt_locs,
            .stmt_regions = stmt_regions,
            .local_names = local_names,
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.items) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
        self.local_names.deinit(self.allocator);
        self.stmt_regions.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.expr_regions.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        for (self.source_files.items) |file| self.allocator.free(file);
        self.source_files.deinit(self.allocator);
        for (self.comptime_sites.items) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.proc_debug_names.deinit();
        for (self.string_literals.items) |literal| self.allocator.free(literal.backing);
        self.string_literals.deinit(self.allocator);
        self.if_branches.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.str_pattern_steps.deinit(self.allocator);
        self.record_destructs.deinit(self.allocator);
        self.fn_def_captures.deinit(self.allocator);
        self.capture_operands.deinit(self.allocator);
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
        self.imported_fns.deinit(self.allocator);
        self.types.deinit();
        self.names.deinit();
    }

    pub fn view(self: *const Program) ProgramView {
        return .{
            .names = &self.names,
            .next_symbol = self.next_symbol,
            .types = self.types.view(),
            .imported_fns = self.imported_fns.items,
            .fns = self.fns.items,
            .exprs = self.exprs.items,
            .pats = self.pats.items,
            .stmts = self.stmts.items,
            .locals = self.locals.items,
            .expr_ids = self.expr_ids.items,
            .pat_ids = self.pat_ids.items,
            .typed_locals = self.typed_locals.items,
            .stmt_ids = self.stmt_ids.items,
            .field_exprs = self.field_exprs.items,
            .capture_operands = self.capture_operands.items,
            .record_destructs = self.record_destructs.items,
            .str_pattern_steps = self.str_pattern_steps.items,
            .branches = self.branches.items,
            .if_branches = self.if_branches.items,
            .string_literals = self.string_literals.items,
            .proc_debug_names = &self.proc_debug_names,
            .roots = self.roots.items,
            .layout_requests = self.layout_requests.items,
            .runtime_schema_requests = self.runtime_schema_requests.items,
            .comptime_sites = self.comptime_sites.items,
            .source_files = self.source_files.items,
            .expr_locs = self.expr_locs.items,
            .expr_regions = self.expr_regions.items,
            .stmt_locs = self.stmt_locs.items,
            .stmt_regions = self.stmt_regions.items,
            .local_names = self.local_names.items,
        };
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn setProcDebugName(self: *Program, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const Program, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.items.len)));
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        try self.expr_regions.append(self.allocator, self.current_region);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const Program, id: ExprId) base.SourceLoc {
        return self.expr_locs.items[@intFromEnum(id)];
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const Program, id: ExprId) base.Region {
        return self.expr_regions.items[@intFromEnum(id)];
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.stmt_locs.items[@intFromEnum(id)];
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const Program, id: StmtId) base.Region {
        return self.stmt_regions.items[@intFromEnum(id)];
    }

    pub fn addPat(self: *Program, pat_: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.items.len)));
        try self.pats.append(self.allocator, pat_);
        return id;
    }

    pub fn addStmt(self: *Program, stmt_: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.items.len)));
        try self.stmts.append(self.allocator, stmt_);
        try self.stmt_locs.append(self.allocator, self.current_loc);
        try self.stmt_regions.append(self.allocator, self.current_region);
        return id;
    }

    pub fn comptimeSite(self: *const Program, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites.items[@intFromEnum(id)];
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const Program, id: LocalId) []const u8 {
        return self.local_names.items[@intFromEnum(id)];
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.items.len)));
        try self.locals.append(self.allocator, .{
            .id = id,
            .symbol = symbol,
            .ty = ty,
            .binder = binder,
            // A binder-backed local carries the exact capture identity of
            // its binding, so any function that captures it joins by CaptureId.
            .capture_id = if (binder) |b| check.CheckedModule.CaptureId.fromBinder(b) else null,
        });
        try self.local_names.append(self.allocator, "");
        return id;
    }

    /// Allocate the next generated `CaptureId` for a lift-synthesized capturable
    /// local (a free local with no checked binder). The counter lives on the
    /// program so the identity is stable across fixpoint rounds and unique
    /// within the program.
    pub fn nextLiftCaptureId(self: *Program) check.CheckedModule.CaptureId {
        const index = self.next_lift_capture_id;
        self.next_lift_capture_id += 1;
        return check.CheckedModule.CaptureId.generatedLift(index);
    }

    pub fn addTypedLocalSpan(self: *Program, values: []const TypedLocal) std.mem.Allocator.Error!Span(TypedLocal) {
        const start: u32 = @intCast(self.typed_locals.items.len);
        try self.typed_locals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addExprSpan(self: *Program, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addPatSpan(self: *Program, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addFieldExprSpan(self: *Program, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        const start: u32 = @intCast(self.field_exprs.items.len);
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnDefCaptureSpan(self: *Program, values: []const FnDefCapture) std.mem.Allocator.Error!Span(FnDefCapture) {
        const start: u32 = @intCast(self.fn_def_captures.items.len);
        try self.fn_def_captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptureOperandSpan(self: *Program, values: []const CaptureOperand) std.mem.Allocator.Error!Span(CaptureOperand) {
        const start: u32 = @intCast(self.capture_operands.items.len);
        try self.capture_operands.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addRecordDestructSpan(self: *Program, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        const start: u32 = @intCast(self.record_destructs.items.len);
        try self.record_destructs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStrPatternStepSpan(self: *Program, values: []const Mono.StrPatternStep) std.mem.Allocator.Error!Span(Mono.StrPatternStep) {
        const start: u32 = @intCast(self.str_pattern_steps.items.len);
        try self.str_pattern_steps.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addBranchSpan(self: *Program, values: []const Branch) std.mem.Allocator.Error!Span(Branch) {
        const start: u32 = @intCast(self.branches.items.len);
        try self.branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addIfBranchSpan(self: *Program, values: []const IfBranch) std.mem.Allocator.Error!Span(IfBranch) {
        const start: u32 = @intCast(self.if_branches.items.len);
        try self.if_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn exprSpan(self: *const Program, span_: Span(ExprId)) []const ExprId {
        return self.expr_ids.items[span_.start..][0..span_.len];
    }

    pub fn patSpan(self: *const Program, span_: Span(PatId)) []const PatId {
        return self.pat_ids.items[span_.start..][0..span_.len];
    }

    pub fn typedLocalSpan(self: *const Program, span_: Span(TypedLocal)) []const TypedLocal {
        return self.typed_locals.items[span_.start..][0..span_.len];
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: *const Program, id: LocalId) check.CheckedModule.CaptureId {
        return self.locals.items[@intFromEnum(id)].capture_id orelse
            Common.invariant("lifted capture local had no CaptureId");
    }

    pub fn stmtSpan(self: *const Program, span_: Span(StmtId)) []const StmtId {
        return self.stmt_ids.items[span_.start..][0..span_.len];
    }

    pub fn fieldExprSpan(self: *const Program, span_: Span(FieldExpr)) []const FieldExpr {
        return self.field_exprs.items[span_.start..][0..span_.len];
    }

    pub fn fnDefCaptureSpan(self: *const Program, span_: Span(FnDefCapture)) []const FnDefCapture {
        return self.fn_def_captures.items[span_.start..][0..span_.len];
    }

    pub fn captureOperandSpan(self: *const Program, span_: Span(CaptureOperand)) []const CaptureOperand {
        return self.capture_operands.items[span_.start..][0..span_.len];
    }

    pub fn recordDestructSpan(self: *const Program, span_: Span(RecordDestruct)) []const RecordDestruct {
        return self.record_destructs.items[span_.start..][0..span_.len];
    }

    pub fn strPatternStepSpan(self: *const Program, span_: Span(Mono.StrPatternStep)) []const Mono.StrPatternStep {
        return self.str_pattern_steps.items[span_.start..][0..span_.len];
    }

    pub fn branchSpan(self: *const Program, span_: Span(Branch)) []const Branch {
        return self.branches.items[span_.start..][0..span_.len];
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
        const call = switch (self.exprs.items[@intFromEnum(scrutinee)].data) {
            .call_proc => |call| call,
            else => return null,
        };
        const callee = switch (call.callee) {
            .lifted => |fn_id| fn_id,
            .func => return null,
        };
        const callee_body = switch (self.fns.items[@intFromEnum(callee)].body) {
            .roc => |body| body,
            .hosted => return null,
        };
        if (!self.exprIsListMapCanReuseOp(callee_body)) return null;

        for (self.branchSpan(branches_span)) |branch| {
            if (branch.guard != null) return null;
            switch (self.pats.items[@intFromEnum(branch.pat)].data) {
                .wildcard => return .{ .call_args = call.args, .zero_branch_body = branch.body },
                .int_lit => |value| if (value.toI128() == 0) {
                    return .{ .call_args = call.args, .zero_branch_body = branch.body };
                },
                else => return null,
            }
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
        return switch (self.exprs.items[@intFromEnum(expr_id)].data) {
            .low_level => |ll| ll.op == .list_map_can_reuse,
            .block => |block| block.statements.len == 0 and self.exprIsListMapCanReuseOp(block.final_expr),
            else => false,
        };
    }

    pub fn ifBranchSpan(self: *const Program, span_: Span(IfBranch)) []const IfBranch {
        return self.if_branches.items[span_.start..][0..span_.len];
    }

    pub fn exprCount(self: *const Program) usize {
        return self.exprs.items.len;
    }

    pub fn patCount(self: *const Program) usize {
        return self.pats.items.len;
    }

    pub fn stmtCount(self: *const Program) usize {
        return self.stmts.items.len;
    }

    pub fn localCount(self: *const Program) usize {
        return self.locals.items.len;
    }

    pub fn exprTy(self: *const Program, id: ExprId) Type.TypeId {
        return self.exprs.items[@intFromEnum(id)].ty;
    }

    pub fn patTy(self: *const Program, id: PatId) Type.TypeId {
        return self.pats.items[@intFromEnum(id)].ty;
    }

    pub fn pat(self: *const Program, id: PatId) Pat {
        return self.pats.items[@intFromEnum(id)];
    }

    pub fn stmt(self: *const Program, id: StmtId) Stmt {
        return self.stmts.items[@intFromEnum(id)];
    }
};

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
}
