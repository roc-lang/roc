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
const GuardedList = collections.GuardedList;

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
    imported_fns: ProgramList(ImportedFn, "imported_fns"),
    fns: ProgramList(Fn, "fns"),
    exprs: ProgramList(Expr, "exprs"),
    pats: ProgramList(Pat, "pats"),
    stmts: ProgramList(Stmt, "stmts"),
    locals: ProgramList(Local, "locals"),
    expr_ids: ProgramList(ExprId, "expr_ids"),
    pat_ids: ProgramList(PatId, "pat_ids"),
    typed_locals: ProgramList(TypedLocal, "typed_locals"),
    stmt_ids: ProgramList(StmtId, "stmt_ids"),
    field_exprs: ProgramList(FieldExpr, "field_exprs"),
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
    comptime_sites: ProgramList(ComptimeSite, "comptime_sites"),
    /// Source file table for `SourceLoc.file` indices (moved from Monotype).
    source_files: ProgramList([]const u8, "source_files"),
    /// Source location per expression, parallel to `exprs`.
    expr_locs: ProgramList(base.SourceLoc, "expr_locs"),
    /// Checked source region per expression, parallel to `exprs`.
    expr_regions: ProgramList(base.Region, "expr_regions"),
    /// Source location per statement, parallel to `stmts`.
    stmt_locs: ProgramList(base.SourceLoc, "stmt_locs"),
    /// Checked source region per statement, parallel to `stmts`.
    stmt_regions: ProgramList(base.Region, "stmt_regions"),
    /// Source-level name per local, parallel to `locals` (empty for
    /// compiler-generated temporaries; moved from Monotype).
    local_names: ProgramList([]const u8, "local_names"),
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
            .imported_fns = ProgramList(ImportedFn, "imported_fns").fromArrayList(imported_fns),
            .fns = .empty,
            .exprs = ProgramList(Expr, "exprs").fromArrayList(exprs),
            .pats = ProgramList(Pat, "pats").fromArrayList(pats),
            .stmts = ProgramList(Stmt, "stmts").fromArrayList(stmts),
            .locals = ProgramList(Local, "locals").fromArrayList(locals),
            .expr_ids = ProgramList(ExprId, "expr_ids").fromArrayList(expr_ids),
            .pat_ids = ProgramList(PatId, "pat_ids").fromArrayList(pat_ids),
            .typed_locals = ProgramList(TypedLocal, "typed_locals").fromArrayList(typed_locals),
            .stmt_ids = ProgramList(StmtId, "stmt_ids").fromArrayList(stmt_ids),
            .field_exprs = ProgramList(FieldExpr, "field_exprs").fromArrayList(field_exprs),
            .fn_def_captures = ProgramList(FnDefCapture, "fn_def_captures").fromArrayList(fn_def_captures),
            .capture_operands = .empty,
            .record_destructs = ProgramList(RecordDestruct, "record_destructs").fromArrayList(record_destructs),
            .str_pattern_steps = ProgramList(Mono.StrPatternStep, "str_pattern_steps").fromArrayList(str_pattern_steps),
            .branches = ProgramList(Branch, "branches").fromArrayList(branches),
            .if_branches = ProgramList(IfBranch, "if_branches").fromArrayList(if_branches),
            .string_literals = ProgramList(Mono.StringLiteral, "string_literals").fromArrayList(string_literals),
            .proc_debug_names = proc_debug_names,
            .next_lift_capture_id = 0,
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .comptime_sites = ProgramList(ComptimeSite, "comptime_sites").fromArrayList(comptime_sites),
            .source_files = ProgramList([]const u8, "source_files").fromArrayList(source_files),
            .expr_locs = ProgramList(base.SourceLoc, "expr_locs").fromArrayList(expr_locs),
            .expr_regions = ProgramList(base.Region, "expr_regions").fromArrayList(expr_regions),
            .stmt_locs = ProgramList(base.SourceLoc, "stmt_locs").fromArrayList(stmt_locs),
            .stmt_regions = ProgramList(base.Region, "stmt_regions").fromArrayList(stmt_regions),
            .local_names = ProgramList([]const u8, "local_names").fromArrayList(local_names),
            .current_loc = base.SourceLoc.none,
            .current_region = base.Region.zero(),
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.local_names.unsafeRawItemsForView()) |name| {
            if (name.len > 0) self.allocator.free(name);
        }
        self.local_names.deinit(self.allocator);
        self.stmt_regions.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.expr_regions.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        for (self.source_files.unsafeRawItemsForView()) |file| self.allocator.free(file);
        self.source_files.deinit(self.allocator);
        for (self.comptime_sites.unsafeRawItemsForView()) |site| {
            self.allocator.free(site.branch_regions);
        }
        self.comptime_sites.deinit(self.allocator);
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.proc_debug_names.deinit();
        for (self.string_literals.unsafeRawItemsForView()) |literal| self.allocator.free(literal.backing);
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
            .imported_fns = self.imported_fns.unsafeRawItemsForView(),
            .fns = self.fns.unsafeRawItemsForView(),
            .exprs = self.exprs.unsafeRawItemsForView(),
            .pats = self.pats.unsafeRawItemsForView(),
            .stmts = self.stmts.unsafeRawItemsForView(),
            .locals = self.locals.unsafeRawItemsForView(),
            .expr_ids = self.expr_ids.unsafeRawItemsForView(),
            .pat_ids = self.pat_ids.unsafeRawItemsForView(),
            .typed_locals = self.typed_locals.unsafeRawItemsForView(),
            .stmt_ids = self.stmt_ids.unsafeRawItemsForView(),
            .field_exprs = self.field_exprs.unsafeRawItemsForView(),
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
            .comptime_sites = self.comptime_sites.unsafeRawItemsForView(),
            .source_files = self.source_files.unsafeRawItemsForView(),
            .expr_locs = self.expr_locs.unsafeRawItemsForView(),
            .expr_regions = self.expr_regions.unsafeRawItemsForView(),
            .stmt_locs = self.stmt_locs.unsafeRawItemsForView(),
            .stmt_regions = self.stmt_regions.unsafeRawItemsForView(),
            .local_names = self.local_names.unsafeRawItemsForView(),
        };
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.len())));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn reserveFnSlot(self: *Program) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.len())));
        try self.fns.append(self.allocator, undefined);
        return id;
    }

    pub fn setFn(self: *Program, id: FnId, fn_: Fn) void {
        self.fns.set(@intFromEnum(id), fn_);
    }

    pub fn setFnAt(self: *Program, index: usize, fn_: Fn) void {
        self.fns.set(index, fn_);
    }

    pub fn setFnCaptures(self: *Program, id: FnId, captures: Span(TypedLocal)) void {
        self.fns.getPtrImmediate(@intFromEnum(id)).captures = captures;
    }

    pub fn setProcDebugName(self: *Program, symbol: Common.Symbol, name: names.ExportNameId) std.mem.Allocator.Error!void {
        try self.proc_debug_names.put(symbol, name);
    }

    pub fn procDebugName(self: *const Program, symbol: Common.Symbol) ?names.ExportNameId {
        return self.proc_debug_names.get(symbol);
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.len())));
        try self.exprs.append(self.allocator, expr);
        try self.expr_locs.append(self.allocator, self.current_loc);
        try self.expr_regions.append(self.allocator, self.current_region);
        return id;
    }

    /// Source location of an expression.
    pub fn exprLoc(self: *const Program, id: ExprId) base.SourceLoc {
        return self.expr_locs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Checked source region of an expression.
    pub fn exprRegion(self: *const Program, id: ExprId) base.Region {
        return self.expr_regions.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Source location of a statement.
    pub fn stmtLoc(self: *const Program, id: StmtId) base.SourceLoc {
        return self.stmt_locs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    /// Checked source region of a statement.
    pub fn stmtRegion(self: *const Program, id: StmtId) base.Region {
        return self.stmt_regions.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn addPat(self: *Program, pat_: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.len())));
        try self.pats.append(self.allocator, pat_);
        return id;
    }

    pub fn addStmt(self: *Program, stmt_: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.len())));
        try self.stmts.append(self.allocator, stmt_);
        try self.stmt_locs.append(self.allocator, self.current_loc);
        try self.stmt_regions.append(self.allocator, self.current_region);
        return id;
    }

    pub fn comptimeSite(self: *const Program, id: ComptimeSiteId) ComptimeSite {
        return self.comptime_sites.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn comptimeSiteCount(self: *const Program) usize {
        return self.comptime_sites.len();
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    /// Source-level name of a local; empty for compiler-generated temporaries.
    pub fn localName(self: *const Program, id: LocalId) []const u8 {
        return self.local_names.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn sourceFileNames(self: *const Program) []const []const u8 {
        return self.source_files.unsafeRawItemsForView();
    }

    pub fn takeStringLiterals(self: *Program) std.ArrayList(Mono.StringLiteral) {
        return self.string_literals.takeArrayList();
    }

    pub fn takeSourceFiles(self: *Program) std.ArrayList([]const u8) {
        return self.source_files.takeArrayList();
    }

    pub fn stringLiteralsView(self: *const Program) []const Mono.StringLiteral {
        return self.string_literals.unsafeRawItemsForView();
    }

    pub fn rootCount(self: *const Program) usize {
        return self.roots.len();
    }

    pub fn rootsView(self: *const Program) []const Root {
        return self.roots.unsafeRawItemsForView();
    }

    pub fn fnCount(self: *const Program) usize {
        return self.fns.len();
    }

    pub fn getFn(self: *const Program, id: FnId) Fn {
        return self.fns.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getFnAt(self: *const Program, index: usize) Fn {
        return self.fns.get(index);
    }

    pub fn fnsView(self: *const Program) []const Fn {
        return self.fns.unsafeRawItemsForView();
    }

    pub fn getExpr(self: *const Program, id: ExprId) Expr {
        return self.exprs.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn exprsView(self: *const Program) []const Expr {
        return self.exprs.unsafeRawItemsForView();
    }

    pub fn setExpr(self: *Program, id: ExprId, expr: Expr) void {
        self.exprs.set(@intFromEnum(id), expr);
    }

    pub fn getExprAt(self: *const Program, index: usize) Expr {
        return self.exprs.get(index);
    }

    pub fn setExprData(self: *Program, id: ExprId, data: ExprData) void {
        self.exprs.getPtrImmediate(@intFromEnum(id)).data = data;
    }

    pub fn setExprDataAt(self: *Program, index: usize, data: ExprData) void {
        self.exprs.getPtrImmediate(index).data = data;
    }

    pub fn getPat(self: *const Program, id: PatId) Pat {
        return self.pats.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getPatAt(self: *const Program, index: usize) Pat {
        return self.pats.get(index);
    }

    pub fn getStmt(self: *const Program, id: StmtId) Stmt {
        return self.stmts.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getStmtAt(self: *const Program, index: usize) Stmt {
        return self.stmts.get(index);
    }

    pub fn stmtsView(self: *const Program) []const Stmt {
        return self.stmts.unsafeRawItemsForView();
    }

    pub fn getLocal(self: *const Program, id: LocalId) Local {
        return self.locals.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn getLocalAt(self: *const Program, index: usize) Local {
        return self.locals.get(index);
    }

    pub fn localsView(self: *const Program) []const Local {
        return self.locals.unsafeRawItemsForView();
    }

    pub fn getStringLiteral(self: *const Program, id: StringLiteralId) Mono.StringLiteral {
        return self.string_literals.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn importedFnCount(self: *const Program) usize {
        return self.imported_fns.len();
    }

    pub fn addRoot(self: *Program, root: Root) std.mem.Allocator.Error!void {
        try self.roots.append(self.allocator, root);
    }

    pub fn addLayoutRequest(self: *Program, request: LayoutRequest) std.mem.Allocator.Error!void {
        try self.layout_requests.append(self.allocator, request);
    }

    pub fn addRuntimeSchemaRequest(self: *Program, request: RuntimeSchemaRequest) std.mem.Allocator.Error!void {
        try self.runtime_schema_requests.append(self.allocator, request);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.len())));
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
        const start: u32 = @intCast(self.typed_locals.len());
        try self.typed_locals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addExprSpan(self: *Program, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        const start: u32 = @intCast(self.expr_ids.len());
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addPatSpan(self: *Program, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        const start: u32 = @intCast(self.pat_ids.len());
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.len());
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addFieldExprSpan(self: *Program, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        const start: u32 = @intCast(self.field_exprs.len());
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnDefCaptureSpan(self: *Program, values: []const FnDefCapture) std.mem.Allocator.Error!Span(FnDefCapture) {
        const start: u32 = @intCast(self.fn_def_captures.len());
        try self.fn_def_captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptureOperandSpan(self: *Program, values: []const CaptureOperand) std.mem.Allocator.Error!Span(CaptureOperand) {
        const start: u32 = @intCast(self.capture_operands.len());
        try self.capture_operands.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn setCaptureOperandInSpan(self: *Program, span_: Span(CaptureOperand), index: usize, operand: CaptureOperand) void {
        if (index >= span_.len) Common.invariant("capture operand index was outside span");
        self.capture_operands.set(span_.start + index, operand);
    }

    pub fn addRecordDestructSpan(self: *Program, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        const start: u32 = @intCast(self.record_destructs.len());
        try self.record_destructs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addStrPatternStepSpan(self: *Program, values: []const Mono.StrPatternStep) std.mem.Allocator.Error!Span(Mono.StrPatternStep) {
        const start: u32 = @intCast(self.str_pattern_steps.len());
        try self.str_pattern_steps.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addBranchSpan(self: *Program, values: []const Branch) std.mem.Allocator.Error!Span(Branch) {
        const start: u32 = @intCast(self.branches.len());
        try self.branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addIfBranchSpan(self: *Program, values: []const IfBranch) std.mem.Allocator.Error!Span(IfBranch) {
        const start: u32 = @intCast(self.if_branches.len());
        try self.if_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn exprSpan(self: *const Program, span_: Span(ExprId)) ProgramSpanBorrow(ExprId, "expr_ids") {
        return self.expr_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn patSpan(self: *const Program, span_: Span(PatId)) ProgramSpanBorrow(PatId, "pat_ids") {
        return self.pat_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn typedLocalSpan(self: *const Program, span_: Span(TypedLocal)) ProgramSpanBorrow(TypedLocal, "typed_locals") {
        return self.typed_locals.borrowSpan(span_.start, span_.len);
    }

    /// The CaptureId of a local. Every local that participates in a capture set
    /// carries one; asserts it is present.
    pub fn captureIdOfLocal(self: *const Program, id: LocalId) check.CheckedModule.CaptureId {
        return self.locals.unsafeRawItemsForView()[@intFromEnum(id)].capture_id orelse
            Common.invariant("lifted capture local had no CaptureId");
    }

    pub fn ensureLiftCaptureId(self: *Program, id: LocalId) check.CheckedModule.CaptureId {
        const local = self.locals.getPtrImmediate(@intFromEnum(id));
        if (local.capture_id == null) {
            local.capture_id = self.nextLiftCaptureId();
        }
        return local.capture_id.?;
    }

    pub fn stmtSpan(self: *const Program, span_: Span(StmtId)) ProgramSpanBorrow(StmtId, "stmt_ids") {
        return self.stmt_ids.borrowSpan(span_.start, span_.len);
    }

    pub fn fieldExprSpan(self: *const Program, span_: Span(FieldExpr)) ProgramSpanBorrow(FieldExpr, "field_exprs") {
        return self.field_exprs.borrowSpan(span_.start, span_.len);
    }

    pub fn fnDefCaptureSpan(self: *const Program, span_: Span(FnDefCapture)) ProgramSpanBorrow(FnDefCapture, "fn_def_captures") {
        return self.fn_def_captures.borrowSpan(span_.start, span_.len);
    }

    pub fn captureOperandSpan(self: *const Program, span_: Span(CaptureOperand)) ProgramSpanBorrow(CaptureOperand, "capture_operands") {
        return self.capture_operands.borrowSpan(span_.start, span_.len);
    }

    pub fn recordDestructSpan(self: *const Program, span_: Span(RecordDestruct)) ProgramSpanBorrow(RecordDestruct, "record_destructs") {
        return self.record_destructs.borrowSpan(span_.start, span_.len);
    }

    pub fn strPatternStepSpan(self: *const Program, span_: Span(Mono.StrPatternStep)) ProgramSpanBorrow(Mono.StrPatternStep, "str_pattern_steps") {
        return self.str_pattern_steps.borrowSpan(span_.start, span_.len);
    }

    pub fn branchSpan(self: *const Program, span_: Span(Branch)) ProgramSpanBorrow(Branch, "branches") {
        return self.branches.borrowSpan(span_.start, span_.len);
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
        const call = switch (self.exprs.unsafeRawItemsForView()[@intFromEnum(scrutinee)].data) {
            .call_proc => |call| call,
            else => return null,
        };
        const callee = switch (call.callee) {
            .lifted => |fn_id| fn_id,
            .func => return null,
        };
        const callee_body = switch (self.fns.unsafeRawItemsForView()[@intFromEnum(callee)].body) {
            .roc => |body| body,
            .hosted => return null,
        };
        if (!self.exprIsListMapCanReuseOp(callee_body)) return null;

        const branches = self.branchSpan(branches_span);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.guard != null) return null;
            switch (self.pats.unsafeRawItemsForView()[@intFromEnum(branch.pat)].data) {
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
        return switch (self.exprs.unsafeRawItemsForView()[@intFromEnum(expr_id)].data) {
            .low_level => |ll| ll.op == .list_map_can_reuse,
            .block => |block| block.statements.len == 0 and self.exprIsListMapCanReuseOp(block.final_expr),
            else => false,
        };
    }

    pub fn ifBranchSpan(self: *const Program, span_: Span(IfBranch)) ProgramSpanBorrow(IfBranch, "if_branches") {
        return self.if_branches.borrowSpan(span_.start, span_.len);
    }

    pub fn exprCount(self: *const Program) usize {
        return self.exprs.len();
    }

    pub fn patCount(self: *const Program) usize {
        return self.pats.len();
    }

    pub fn stmtCount(self: *const Program) usize {
        return self.stmts.len();
    }

    pub fn localCount(self: *const Program) usize {
        return self.locals.len();
    }

    pub fn exprTy(self: *const Program, id: ExprId) Type.TypeId {
        return self.exprs.unsafeRawItemsForView()[@intFromEnum(id)].ty;
    }

    pub fn patTy(self: *const Program, id: PatId) Type.TypeId {
        return self.pats.unsafeRawItemsForView()[@intFromEnum(id)].ty;
    }

    pub fn pat(self: *const Program, id: PatId) Pat {
        return self.pats.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn stmt(self: *const Program, id: StmtId) Stmt {
        return self.stmts.unsafeRawItemsForView()[@intFromEnum(id)];
    }
};

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
}
