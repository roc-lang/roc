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
pub const ExprId = enum(u32) { _ };
/// Identifier for a pattern in Monotype Lifted IR.
pub const PatId = enum(u32) { _ };
/// Identifier for a statement in Monotype Lifted IR.
pub const StmtId = enum(u32) { _ };
/// Identifier for a lifted function body.
pub const FnId = enum(u32) { _ };

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
pub const FieldExpr = struct {
    name: names.RecordFieldNameId,
    value: ExprId,
};
/// Record destructuring field pattern.
pub const RecordDestruct = struct {
    name: names.RecordFieldNameId,
    pattern: PatId,
};

/// Typed Monotype Lifted expression.
pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

/// Monotype Lifted expression forms.
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
    call_value: struct {
        callee: ExprId,
        args: Span(ExprId),
    },
    call_proc: struct {
        callee: FnId,
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

/// Typed Monotype Lifted pattern.
pub const Pat = struct {
    ty: Type.TypeId,
    data: PatData,
};

/// Monotype Lifted pattern forms.
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

/// Match branch.
pub const Branch = struct {
    pat: PatId,
    guard: ?ExprId = null,
    body: ExprId,
};

/// Conditional branch in an if expression.
pub const IfBranch = struct {
    cond: ExprId,
    body: ExprId,
};

/// Monotype Lifted statement forms.
pub const Stmt = union(enum) {
    let_: struct {
        pat: PatId,
        value: ExprId,
        recursive: bool = false,
    },
    expr: ExprId,
    expect: ExprId,
    dbg: ExprId,
    return_: ExprId,
    crash: StringLiteralId,
};

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
    string_literals: std.ArrayList([]const u8),
    roots: std.ArrayList(Root),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: names.NameStore,
        types: Type.Store,
        string_literals: std.ArrayList([]const u8),
        next_symbol: u32,
    ) Program {
        return .{
            .allocator = allocator,
            .names = name_store,
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
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
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

    pub fn ifBranchSpan(self: *const Program, span_: Span(IfBranch)) []const IfBranch {
        return self.if_branches.items[span_.start..][0..span_.len];
    }
};

test "monotype lifted declarations are referenced" {
    std.testing.refAllDecls(@This());
}
