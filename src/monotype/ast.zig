//! Cor-style monotype AST, with explicit Roc-only extensions for loops, returns,
//! and mutable statements.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const symbol_mod = @import("symbol");
const type_mod = @import("type.zig");

pub const Symbol = symbol_mod.Symbol;
pub const TypeId = type_mod.TypeId;

pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const BranchId = enum(u32) { _ };

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const TypedSymbol = struct {
    ty: TypeId,
    symbol: Symbol,
};

pub const Pat = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        bool_lit: bool,
        tag: struct {
            name: base.Ident.Idx,
            discriminant: u16,
            args: Span(PatId),
        },
        var_: Symbol,
    };
};

pub const LetFn = struct {
    recursive: bool,
    bind: TypedSymbol,
    arg: TypedSymbol,
    body: ExprId,
};

pub const LetVal = struct {
    bind: TypedSymbol,
    body: ExprId,
};

pub const LetDef = union(enum) {
    let_fn: LetFn,
    let_val: LetVal,
};

pub const Branch = struct {
    pat: PatId,
    body: ExprId,
};

pub const FieldExpr = struct {
    name: base.Ident.Idx,
    value: ExprId,
};

pub const Expr = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        var_: Symbol,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        bool_lit: bool,
        str_lit: base.StringLiteral.Idx,
        tag: struct {
            name: base.Ident.Idx,
            discriminant: u16,
            args: Span(ExprId),
        },
        record: Span(FieldExpr),
        access: struct {
            record: ExprId,
            field: base.Ident.Idx,
            field_index: u16,
        },
        let_: struct {
            def: LetDef,
            rest: ExprId,
        },
        clos: struct {
            arg: TypedSymbol,
            body: ExprId,
        },
        call: struct {
            func: ExprId,
            arg: ExprId,
        },
        inspect: ExprId,
        low_level: struct {
            op: base.LowLevel,
            args: Span(ExprId),
        },
        when: struct {
            cond: ExprId,
            branches: Span(BranchId),
        },
        if_: struct {
            cond: ExprId,
            then_body: ExprId,
            else_body: ExprId,
        },
        block: struct {
            stmts: Span(StmtId),
            final_expr: ExprId,
        },
        tuple: Span(ExprId),
        tuple_access: struct {
            tuple: ExprId,
            elem_index: u32,
        },
        list: Span(ExprId),
        unit,
        return_: ExprId,
        runtime_error: base.StringLiteral.Idx,
        for_: struct {
            patt: PatId,
            iterable: ExprId,
            body: ExprId,
        },
    };
};

pub const Stmt = union(enum) {
    local_fn: LetFn,
    decl: struct {
        bind: TypedSymbol,
        body: ExprId,
    },
    var_decl: struct {
        bind: TypedSymbol,
        body: ExprId,
    },
    reassign: struct {
        target: Symbol,
        body: ExprId,
    },
    expr: ExprId,
    debug: ExprId,
    expect: ExprId,
    crash: base.StringLiteral.Idx,
    return_: ExprId,
    break_,
    for_: struct {
        patt: PatId,
        iterable: ExprId,
        body: ExprId,
    },
    while_: struct {
        cond: ExprId,
        body: ExprId,
    },
};

pub const RunDef = struct {
    bind: TypedSymbol,
    body: ExprId,
    entry_ty: types.Var,
};

pub const DefVal = union(enum) {
    fn_: LetFn,
    val: ExprId,
    run: RunDef,
};

pub const Def = struct {
    bind: TypedSymbol,
    value: DefVal,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    branches: std.ArrayList(Branch),
    stmts: std.ArrayList(Stmt),
    defs: std.ArrayList(Def),
    expr_ids: std.ArrayList(ExprId),
    pat_ids: std.ArrayList(PatId),
    stmt_ids: std.ArrayList(StmtId),
    branch_ids: std.ArrayList(BranchId),
    field_exprs: std.ArrayList(FieldExpr),

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
    }

    pub fn addExpr(self: *Store, expr: Expr) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, expr);
        return @enumFromInt(idx);
    }

    pub fn getExpr(self: *const Store, id: ExprId) Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    pub fn addPat(self: *Store, pat: Pat) std.mem.Allocator.Error!PatId {
        const idx: u32 = @intCast(self.pats.items.len);
        try self.pats.append(self.allocator, pat);
        return @enumFromInt(idx);
    }

    pub fn getPat(self: *const Store, id: PatId) Pat {
        return self.pats.items[@intFromEnum(id)];
    }

    pub fn addBranch(self: *Store, branch: Branch) std.mem.Allocator.Error!BranchId {
        const idx: u32 = @intCast(self.branches.items.len);
        try self.branches.append(self.allocator, branch);
        return @enumFromInt(idx);
    }

    pub fn getBranch(self: *const Store, id: BranchId) Branch {
        return self.branches.items[@intFromEnum(id)];
    }

    pub fn addStmt(self: *Store, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const idx: u32 = @intCast(self.stmts.items.len);
        try self.stmts.append(self.allocator, stmt);
        return @enumFromInt(idx);
    }

    pub fn getStmt(self: *const Store, id: StmtId) Stmt {
        return self.stmts.items[@intFromEnum(id)];
    }

    pub fn addExprSpan(self: *Store, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        if (ids.len == 0) return Span(ExprId).empty();
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceExprSpan(self: *const Store, span: Span(ExprId)) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_ids.items[span.start..][0..span.len];
    }

    pub fn addPatSpan(self: *Store, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        if (ids.len == 0) return Span(PatId).empty();
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn slicePatSpan(self: *const Store, span: Span(PatId)) []const PatId {
        if (span.len == 0) return &.{};
        return self.pat_ids.items[span.start..][0..span.len];
    }

    pub fn addStmtSpan(self: *Store, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        if (ids.len == 0) return Span(StmtId).empty();
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceStmtSpan(self: *const Store, span: Span(StmtId)) []const StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_ids.items[span.start..][0..span.len];
    }

    pub fn addBranchSpan(self: *Store, ids: []const Branch) std.mem.Allocator.Error!Span(BranchId) {
        if (ids.len == 0) return Span(BranchId).empty();
        const start: u32 = @intCast(self.branches.items.len);
        for (ids) |branch| {
            try self.branches.append(self.allocator, branch);
        }
        const count: u32 = @intCast(ids.len);
        try self.branch_ids.ensureTotalCapacity(self.allocator, self.branch_ids.items.len + ids.len);
        for (0..ids.len) |offset| {
            self.branch_ids.appendAssumeCapacity(@enumFromInt(start + @as(u32, @intCast(offset))));
        }
        return .{ .start = @intCast(self.branch_ids.items.len - ids.len), .len = count };
    }

    pub fn sliceBranchSpan(self: *const Store, span: Span(BranchId)) []const BranchId {
        if (span.len == 0) return &.{};
        return self.branch_ids.items[span.start..][0..span.len];
    }

    pub fn addFieldExprSpan(self: *Store, values: []const FieldExpr) std.mem.Allocator.Error!Span(FieldExpr) {
        if (values.len == 0) return Span(FieldExpr).empty();
        const start: u32 = @intCast(self.field_exprs.items.len);
        try self.field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceFieldExprSpan(self: *const Store, span: Span(FieldExpr)) []const FieldExpr {
        if (span.len == 0) return &.{};
        return self.field_exprs.items[span.start..][0..span.len];
    }

    pub fn addDef(self: *Store, def: Def) std.mem.Allocator.Error!DefId {
        const idx: u32 = @intCast(self.defs.items.len);
        try self.defs.append(self.allocator, def);
        return @enumFromInt(idx);
    }

    pub fn getDef(self: *const Store, id: DefId) Def {
        return self.defs.items[@intFromEnum(id)];
    }

    pub fn defsSlice(self: *const Store) []const Def {
        return self.defs.items;
    }
};

test "monotype ast tests" {
    std.testing.refAllDecls(@This());
}
