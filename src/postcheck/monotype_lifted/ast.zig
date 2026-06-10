//! Monotype Lifted IR.
//!
//! This stage uses the Monotype type store, but no expression-position lambda
//! remains. Every function body is stored as a lifted function with explicit
//! capture locals.

const std = @import("std");
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
/// Record field expression entry.
pub const FieldExpr = Mono.FieldExpr;
/// Record destructuring field pattern.
pub const RecordDestruct = Mono.RecordDestruct;

/// Typed Monotype Lifted expression.
pub const Expr = Mono.Expr;

/// Monotype Lifted expression forms.
pub const ExprData = Mono.ExprData;

/// Typed Monotype Lifted pattern.
pub const Pat = Mono.Pat;

/// Monotype Lifted pattern forms.
pub const PatData = Mono.PatData;

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

/// Return the lifted function id for a direct call after Monotype lifting.
pub fn callProcCallee(call: Mono.CallProc) FnId {
    return switch (call.callee) {
        .lifted => |fn_id| fn_id,
        .template => Common.invariant("Monotype Lifted direct call still referenced a Monotype function template"),
    };
}

/// Complete Monotype Lifted program plus side arrays.
pub const Program = struct {
    allocator: std.mem.Allocator,
    names: names.NameStore,
    next_symbol: u32,
    types: Type.Store,
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
    record_destructs: std.ArrayList(RecordDestruct),
    branches: std.ArrayList(Branch),
    if_branches: std.ArrayList(IfBranch),
    string_literals: std.ArrayList(Mono.StringLiteral),
    roots: std.ArrayList(Root),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: names.NameStore,
        types: Type.Store,
        exprs: std.ArrayList(Expr),
        pats: std.ArrayList(Pat),
        stmts: std.ArrayList(Stmt),
        locals: std.ArrayList(Local),
        expr_ids: std.ArrayList(ExprId),
        pat_ids: std.ArrayList(PatId),
        typed_locals: std.ArrayList(TypedLocal),
        stmt_ids: std.ArrayList(StmtId),
        field_exprs: std.ArrayList(FieldExpr),
        record_destructs: std.ArrayList(RecordDestruct),
        branches: std.ArrayList(Branch),
        if_branches: std.ArrayList(IfBranch),
        string_literals: std.ArrayList(Mono.StringLiteral),
        next_symbol: u32,
    ) Program {
        return .{
            .allocator = allocator,
            .names = name_store,
            .next_symbol = next_symbol,
            .types = types,
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
            .record_destructs = record_destructs,
            .branches = branches,
            .if_branches = if_branches,
            .string_literals = string_literals,
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        for (self.string_literals.items) |literal| self.allocator.free(literal.backing);
        self.string_literals.deinit(self.allocator);
        self.if_branches.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.record_destructs.deinit(self.allocator);
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
        self.types.deinit();
        self.names.deinit();
    }

    pub fn addFn(self: *Program, fn_: Fn) std.mem.Allocator.Error!FnId {
        const id: FnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        try self.fns.append(self.allocator, fn_);
        return id;
    }

    pub fn addExpr(self: *Program, expr: Expr) std.mem.Allocator.Error!ExprId {
        const id: ExprId = @enumFromInt(@as(u32, @intCast(self.exprs.items.len)));
        try self.exprs.append(self.allocator, expr);
        return id;
    }

    pub fn addPat(self: *Program, pat_: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.items.len)));
        try self.pats.append(self.allocator, pat_);
        return id;
    }

    pub fn addStmt(self: *Program, stmt_: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.items.len)));
        try self.stmts.append(self.allocator, stmt_);
        return id;
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?check.CheckedModule.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.items.len)));
        try self.locals.append(self.allocator, .{ .id = id, .symbol = symbol, .ty = ty, .binder = binder });
        return id;
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

    pub fn addRecordDestructSpan(self: *Program, values: []const RecordDestruct) std.mem.Allocator.Error!Span(RecordDestruct) {
        const start: u32 = @intCast(self.record_destructs.items.len);
        try self.record_destructs.appendSlice(self.allocator, values);
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

    pub fn stmtSpan(self: *const Program, span_: Span(StmtId)) []const StmtId {
        return self.stmt_ids.items[span_.start..][0..span_.len];
    }

    pub fn fieldExprSpan(self: *const Program, span_: Span(FieldExpr)) []const FieldExpr {
        return self.field_exprs.items[span_.start..][0..span_.len];
    }

    pub fn recordDestructSpan(self: *const Program, span_: Span(RecordDestruct)) []const RecordDestruct {
        return self.record_destructs.items[span_.start..][0..span_.len];
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
            .template => return null,
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
