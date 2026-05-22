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

pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const FnId = enum(u32) { _ };

pub const Span = Mono.Span;
pub const LocalId = Mono.LocalId;
pub const Local = Mono.Local;
pub const TypedLocal = Mono.TypedLocal;
pub const StringLiteralId = Mono.StringLiteralId;
pub const FieldExpr = struct {
    name: names.RecordFieldNameId,
    value: ExprId,
};
pub const RecordDestruct = struct {
    name: names.RecordFieldNameId,
    pattern: PatId,
};

pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

pub const ExprData = union(enum) {
    local: LocalId,
    unit,
    int_lit: @import("can").CIR.IntValue,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    dec_lit: @import("builtins").dec.RocDec,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    tag: struct {
        name: names.TagNameId,
        payloads: Span(ExprId),
    },
    nominal: ExprId,
    let_: struct {
        bind: PatId,
        value: ExprId,
        rest: ExprId,
    },
    fn_ref: FnId,
    fn_def: Mono.FnTemplate,
    call_value: struct {
        callee: ExprId,
        args: Span(ExprId),
    },
    call_proc: struct {
        callee: Mono.FnTemplate,
        args: Span(ExprId),
    },
    low_level: struct {
        op: @import("can").CIR.Expr.LowLevel,
        args: Span(ExprId),
    },
    field_access: struct {
        receiver: ExprId,
        field: names.RecordFieldNameId,
    },
    tuple_access: struct {
        tuple: ExprId,
        elem_index: u32,
    },
    structural_eq: struct {
        lhs: ExprId,
        rhs: ExprId,
        negated: bool,
    },
    match_: struct {
        scrutinee: ExprId,
        branches: Span(Branch),
    },
    if_: struct {
        branches: Span(IfBranch),
        final_else: ExprId,
    },
    block: struct {
        statements: Span(StmtId),
        final_expr: ExprId,
    },
    loop_: struct {
        params: Span(TypedLocal),
        initial_values: Span(ExprId),
        body: ExprId,
    },
    break_: ?ExprId,
    continue_: struct {
        values: Span(ExprId),
    },
    return_: ExprId,
    crash: StringLiteralId,
    dbg: ExprId,
    expect: ExprId,
};

pub const Pat = struct {
    ty: Type.TypeId,
    data: PatData,
};

pub const PatData = union(enum) {
    bind: LocalId,
    wildcard,
    as: struct {
        pattern: PatId,
        local: LocalId,
    },
    record: Span(RecordDestruct),
    tuple: Span(PatId),
    tag: struct {
        name: names.TagNameId,
        payloads: Span(PatId),
    },
    nominal: PatId,
    int_lit: @import("can").CIR.IntValue,
    dec_lit: @import("builtins").dec.RocDec,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    str_lit: StringLiteralId,
};

pub const Branch = struct {
    pat: PatId,
    guard: ?ExprId = null,
    body: ExprId,
};

pub const IfBranch = struct {
    cond: ExprId,
    body: ExprId,
};

pub const Stmt = union(enum) {
    let_: struct {
        pat: PatId,
        value: ExprId,
    },
    expr: ExprId,
    expect: ExprId,
    dbg: ExprId,
    return_: ExprId,
    crash: StringLiteralId,
};

pub const Fn = struct {
    symbol: Common.Symbol,
    source: ?Mono.FnTemplate = null,
    args: Span(TypedLocal),
    captures: Span(TypedLocal),
    body: ExprId,
    ret: Type.TypeId,
};

pub const Root = struct {
    fn_id: FnId,
    request: check.CheckedModule.RootRequest,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    names: *const names.NameStore,
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
    string_literals: std.ArrayList([]const u8),
    roots: std.ArrayList(Root),

    pub fn init(
        allocator: std.mem.Allocator,
        names: *const names.NameStore,
        types: Type.Store,
        string_literals: std.ArrayList([]const u8),
        next_symbol: u32,
    ) Program {
        return .{
            .allocator = allocator,
            .names = names,
            .next_symbol = next_symbol,
            .types = types,
            .fns = .empty,
            .exprs = .empty,
            .pats = .empty,
            .stmts = .empty,
            .locals = .empty,
            .expr_ids = .empty,
            .pat_ids = .empty,
            .typed_locals = .empty,
            .stmt_ids = .empty,
            .field_exprs = .empty,
            .record_destructs = .empty,
            .branches = .empty,
            .if_branches = .empty,
            .string_literals = string_literals,
            .roots = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.roots.deinit(self.allocator);
        for (self.string_literals.items) |literal| self.allocator.free(literal);
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

    pub fn addPat(self: *Program, pat: Pat) std.mem.Allocator.Error!PatId {
        const id: PatId = @enumFromInt(@as(u32, @intCast(self.pats.items.len)));
        try self.pats.append(self.allocator, pat);
        return id;
    }

    pub fn addStmt(self: *Program, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const id: StmtId = @enumFromInt(@as(u32, @intCast(self.stmts.items.len)));
        try self.stmts.append(self.allocator, stmt);
        return id;
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

    pub fn addTypedLocalSpan(self: *Program, values: []const TypedLocal) std.mem.Allocator.Error!Span(TypedLocal) {
        const start: u32 = @intCast(self.typed_locals.items.len);
        try self.typed_locals.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
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
};

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
}
