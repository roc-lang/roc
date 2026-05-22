//! Monotype IR.
//!
//! This is closed, monomorphic, and source-level dispatch-free.

const std = @import("std");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");

const Common = @import("../common.zig");
const Type = @import("type.zig");

const checked = check.CheckedModule;
const names = check.CheckedNames;

pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const LocalId = enum(u32) { _ };
pub const StringLiteralId = enum(u32) { _ };

pub fn Span(comptime T: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const FnDef = union(enum) {
    local_template: names.ProcTemplate,
    imported_template: names.ProcTemplate,
    nested: NestedFn,
    local_hosted: names.ProcTemplate,
    imported_hosted: names.ProcTemplate,
    checked_generated: names.ProcTemplate,
};

pub const NestedFn = struct {
    owner: names.ProcTemplate,
    site: names.ProcSiteId,
};

pub const FnTemplate = struct {
    fn_def: FnDef,
    source_fn_ty: checked.CheckedTypeId,
};

pub const Local = struct {
    id: LocalId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
    binder: ?checked.PatternBinderId = null,
};

pub const TypedLocal = struct {
    local: LocalId,
    ty: Type.TypeId,
};

pub const FieldExpr = struct {
    name: names.RecordFieldNameId,
    value: ExprId,
};

pub const TagExpr = struct {
    name: names.TagNameId,
    payloads: Span(ExprId),
};

pub const LambdaExpr = struct {
    args: Span(TypedLocal),
    body: ExprId,
    source: FnTemplate,
};

pub const CallValue = struct {
    callee: ExprId,
    args: Span(ExprId),
};

pub const CallProc = struct {
    callee: FnTemplate,
    args: Span(ExprId),
};

pub const LowLevelCall = struct {
    op: can.CIR.Expr.LowLevel,
    args: Span(ExprId),
};

pub const MatchExpr = struct {
    scrutinee: ExprId,
    branches: Span(Branch),
};

pub const IfExpr = struct {
    branches: Span(IfBranch),
    final_else: ExprId,
};

pub const BlockExpr = struct {
    statements: Span(StmtId),
    final_expr: ExprId,
};

pub const LoopExpr = struct {
    params: Span(TypedLocal),
    initial_values: Span(ExprId),
    body: ExprId,
};

pub const ContinueExpr = struct {
    values: Span(ExprId),
};

pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

pub const ExprData = union(enum) {
    local: LocalId,
    unit,
    int_lit: can.CIR.IntValue,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    dec_lit: builtins.dec.RocDec,
    str_lit: StringLiteralId,
    list: Span(ExprId),
    tuple: Span(ExprId),
    record: Span(FieldExpr),
    tag: TagExpr,
    nominal: ExprId,
    let_: struct {
        bind: PatId,
        value: ExprId,
        rest: ExprId,
    },
    lambda: LambdaExpr,
    fn_def: FnTemplate,
    call_value: CallValue,
    call_proc: CallProc,
    low_level: LowLevelCall,
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
    match_: MatchExpr,
    if_: IfExpr,
    block: BlockExpr,
    loop_: LoopExpr,
    break_: ?ExprId,
    continue_: ContinueExpr,
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
    int_lit: can.CIR.IntValue,
    dec_lit: builtins.dec.RocDec,
    frac_f32_lit: f32,
    frac_f64_lit: f64,
    str_lit: StringLiteralId,
};

pub const RecordDestruct = struct {
    name: names.RecordFieldNameId,
    pattern: PatId,
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

pub const StmtId = enum(u32) { _ };

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

pub const Def = struct {
    symbol: Common.Symbol,
    fn_def: FnTemplate,
    args: Span(TypedLocal),
    body: ExprId,
    ret: Type.TypeId,
};

pub const Root = struct {
    def: DefId,
    request: checked.RootRequest,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    names: *const names.NameStore,
    next_symbol: u32,
    types: Type.Store,
    defs: std.ArrayList(Def),
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

    pub fn init(allocator: std.mem.Allocator, names: *const names.NameStore) Program {
        return .{
            .allocator = allocator,
            .names = names,
            .next_symbol = 0,
            .types = Type.Store.init(allocator),
            .defs = .empty,
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
            .string_literals = .empty,
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
        self.defs.deinit(self.allocator);
        self.types.deinit();
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

    pub fn addStringLiteral(self: *Program, text: []const u8) std.mem.Allocator.Error!StringLiteralId {
        const id: StringLiteralId = @enumFromInt(@as(u32, @intCast(self.string_literals.items.len)));
        const owned = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(owned);
        try self.string_literals.append(self.allocator, owned);
        return id;
    }

    pub fn stringLiteralText(self: *const Program, id: StringLiteralId) []const u8 {
        return self.string_literals.items[@intFromEnum(id)];
    }

    pub fn addLocal(self: *Program, symbol: Common.Symbol, ty: Type.TypeId) std.mem.Allocator.Error!LocalId {
        return try self.addLocalWithBinder(symbol, ty, null);
    }

    pub fn addLocalWithBinder(
        self: *Program,
        symbol: Common.Symbol,
        ty: Type.TypeId,
        binder: ?checked.PatternBinderId,
    ) std.mem.Allocator.Error!LocalId {
        const id: LocalId = @enumFromInt(@as(u32, @intCast(self.locals.items.len)));
        try self.locals.append(self.allocator, .{ .id = id, .symbol = symbol, .ty = ty, .binder = binder });
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

    pub fn addStmtSpan(self: *Program, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
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

test "monotype ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
