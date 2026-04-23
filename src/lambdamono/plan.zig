//! Source-blind executable representation plan.

const std = @import("std");
const base = @import("base");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const layouts_mod = @import("layouts.zig");

/// Source-blind planned executable node store.
///
/// This mirrors the executable AST store shape so planning can assign stable ids
/// while still keeping final AST ownership in the emitter.
pub const Store = struct {
    allocator: std.mem.Allocator,
    exprs: std.ArrayList(ast.Expr),
    pats: std.ArrayList(ast.Pat),
    branches: std.ArrayList(ast.Branch),
    stmts: std.ArrayList(ast.Stmt),
    defs: std.ArrayList(ast.Def),
    expr_ids: std.ArrayList(ast.ExprId),
    pat_ids: std.ArrayList(ast.PatId),
    stmt_ids: std.ArrayList(ast.StmtId),
    branch_ids: std.ArrayList(ast.BranchId),
    field_exprs: std.ArrayList(ast.FieldExpr),
    typed_symbols: std.ArrayList(ast.TypedSymbol),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .exprs = .empty,
            .pats = .empty,
            .branches = .empty,
            .stmts = .empty,
            .defs = .empty,
            .expr_ids = .empty,
            .pat_ids = .empty,
            .stmt_ids = .empty,
            .branch_ids = .empty,
            .field_exprs = .empty,
            .typed_symbols = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.exprs.deinit(self.allocator);
        self.pats.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.branch_ids.deinit(self.allocator);
        self.field_exprs.deinit(self.allocator);
        self.typed_symbols.deinit(self.allocator);
    }

    pub fn addExpr(self: *Store, expr: ast.Expr) std.mem.Allocator.Error!ast.ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, expr);
        return @enumFromInt(idx);
    }

    pub fn getExpr(self: *const Store, id: ast.ExprId) ast.Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    pub fn addPat(self: *Store, pat: ast.Pat) std.mem.Allocator.Error!ast.PatId {
        const idx: u32 = @intCast(self.pats.items.len);
        try self.pats.append(self.allocator, pat);
        return @enumFromInt(idx);
    }

    pub fn getPat(self: *const Store, id: ast.PatId) ast.Pat {
        return self.pats.items[@intFromEnum(id)];
    }

    pub fn addBranchSpan(self: *Store, values: []const ast.Branch) std.mem.Allocator.Error!ast.Span(ast.BranchId) {
        if (values.len == 0) return ast.Span(ast.BranchId).empty();
        const start: u32 = @intCast(self.branch_ids.items.len);
        for (values) |value| {
            const idx: u32 = @intCast(self.branches.items.len);
            try self.branches.append(self.allocator, value);
            try self.branch_ids.append(self.allocator, @enumFromInt(idx));
        }
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn getBranch(self: *const Store, id: ast.BranchId) ast.Branch {
        return self.branches.items[@intFromEnum(id)];
    }

    pub fn addStmt(self: *Store, stmt: ast.Stmt) std.mem.Allocator.Error!ast.StmtId {
        const idx: u32 = @intCast(self.stmts.items.len);
        try self.stmts.append(self.allocator, stmt);
        return @enumFromInt(idx);
    }

    pub fn getStmt(self: *const Store, id: ast.StmtId) ast.Stmt {
        return self.stmts.items[@intFromEnum(id)];
    }

    pub fn addDef(self: *Store, def: ast.Def) std.mem.Allocator.Error!ast.DefId {
        const idx: u32 = @intCast(self.defs.items.len);
        try self.defs.append(self.allocator, def);
        return @enumFromInt(idx);
    }

    pub fn getDef(self: *const Store, id: ast.DefId) ast.Def {
        return self.defs.items[@intFromEnum(id)];
    }

    pub fn defsSlice(self: *const Store) []const ast.Def {
        return self.defs.items;
    }

    pub fn addExprSpan(self: *Store, ids: []const ast.ExprId) std.mem.Allocator.Error!ast.Span(ast.ExprId) {
        if (ids.len == 0) return ast.Span(ast.ExprId).empty();
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceExprSpan(self: *const Store, span: ast.Span(ast.ExprId)) []const ast.ExprId {
        if (span.len == 0) return &.{};
        return self.expr_ids.items[span.start..][0..span.len];
    }

    pub fn addPatSpan(self: *Store, ids: []const ast.PatId) std.mem.Allocator.Error!ast.Span(ast.PatId) {
        if (ids.len == 0) return ast.Span(ast.PatId).empty();
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn slicePatSpan(self: *const Store, span: ast.Span(ast.PatId)) []const ast.PatId {
        if (span.len == 0) return &.{};
        return self.pat_ids.items[span.start..][0..span.len];
    }

    pub fn addStmtSpan(self: *Store, ids: []const ast.StmtId) std.mem.Allocator.Error!ast.Span(ast.StmtId) {
        if (ids.len == 0) return ast.Span(ast.StmtId).empty();
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceStmtSpan(self: *const Store, span: ast.Span(ast.StmtId)) []const ast.StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_ids.items[span.start..][0..span.len];
    }

    pub fn sliceBranchSpan(self: *const Store, span: ast.Span(ast.BranchId)) []const ast.BranchId {
        if (span.len == 0) return &.{};
        return self.branch_ids.items[span.start..][0..span.len];
    }

    pub fn addFieldExprSpan(self: *Store, values: []const ast.FieldExpr) std.mem.Allocator.Error!ast.Span(ast.FieldExpr) {
        if (values.len == 0) return ast.Span(ast.FieldExpr).empty();
        const start: u32 = @intCast(self.field_exprs.items.len);
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceFieldExprSpan(self: *const Store, span: ast.Span(ast.FieldExpr)) []const ast.FieldExpr {
        if (span.len == 0) return &.{};
        return self.field_exprs.items[span.start..][0..span.len];
    }

    pub fn addTypedSymbolSpan(self: *Store, values: []const ast.TypedSymbol) std.mem.Allocator.Error!ast.Span(ast.TypedSymbol) {
        if (values.len == 0) return ast.Span(ast.TypedSymbol).empty();
        const start: u32 = @intCast(self.typed_symbols.items.len);
        try self.typed_symbols.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTypedSymbolSpan(self: *const Store, span: ast.Span(ast.TypedSymbol)) []const ast.TypedSymbol {
        if (span.len == 0) return &.{};
        return self.typed_symbols.items[span.start..][0..span.len];
    }
};

/// Complete executable representation plan produced by the lambdamono planner.
pub const ExecPlan = struct {
    store: Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layouts: layouts_mod.Layouts,
    strings: base.StringLiteral.Store,
    entrypoint_wrappers: []symbol_mod.Symbol,

    /// Release all memory owned by the executable representation plan.
    pub fn deinit(self: *ExecPlan) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layouts.deinit(self.store.allocator);
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
        if (self.entrypoint_wrappers.len > 0) {
            self.store.allocator.free(self.entrypoint_wrappers);
        }
    }

};
