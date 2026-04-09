//! Cor-style lowered executable IR.
//!
//! This keeps cor's "vars + let/switch" shape, with Roc-only extensions only
//! for explicit early-return, crash/runtime-error, and list iteration.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const symbol_mod = @import("symbol");
const layout_mod = @import("layout.zig");

pub const Symbol = symbol_mod.Symbol;
pub const LayoutRef = layout_mod.Ref;

pub const ExprId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const BlockId = enum(u32) { _ };
pub const BranchId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const Var = struct {
    layout: LayoutRef,
    symbol: Symbol,
};

pub const Lit = union(enum) {
    int: i128,
    f32: f32,
    f64: f64,
    dec: i128,
    str: base.StringLiteral.Idx,
    bool: bool,
};

pub const Expr = union(enum) {
    var_: Var,
    lit: Lit,
    fn_ptr: Symbol,
    null_ptr,
    make_union: struct {
        discriminant: u16,
        payload: ?Var,
    },
    get_union_id: Var,
    get_union_struct: struct {
        value: Var,
        tag_discriminant: u16,
    },
    make_struct: Span(Var),
    make_list: Span(Var),
    get_struct_field: struct {
        record: Var,
        field_index: u16,
    },
    call_direct: struct {
        proc: Symbol,
        args: Span(Var),
    },
    call_indirect: struct {
        func: Var,
        args: Span(Var),
    },
    call_low_level: struct {
        op: base.LowLevel,
        args: Span(Var),
    },
};

pub const Term = union(enum) {
    value: Var,
    return_: Var,
    crash: base.StringLiteral.Idx,
    runtime_error,
    @"unreachable": void,
};

pub const Block = struct {
    stmts: Span(StmtId),
    term: Term,
};

pub const Branch = struct {
    value: u64,
    block: BlockId,
};

pub const Stmt = union(enum) {
    let_: struct {
        bind: Var,
        expr: ExprId,
    },
    set: struct {
        target: Var,
        value: Var,
    },
    switch_: struct {
        cond: Var,
        branches: Span(BranchId),
        default_block: BlockId,
        join: ?Var,
    },
    debug: Var,
    expect: Var,
    break_,
    for_list: struct {
        elem: Var,
        iterable: Var,
        body: BlockId,
    },
    while_: struct {
        cond: BlockId,
        body: BlockId,
    },
};

pub const Def = struct {
    name: Symbol,
    args: Span(Var),
    body: ?BlockId = null,
    ret_layout: LayoutRef,
    hosted: ?base.HostedProc = null,
    entry_ty: ?types.Var = null,
};

pub const RuntimeReprClassMap = std.AutoHashMap(u64, u32);

pub const Store = struct {
    allocator: std.mem.Allocator,
    exprs: std.ArrayList(Expr),
    stmts: std.ArrayList(Stmt),
    blocks: std.ArrayList(Block),
    branches: std.ArrayList(Branch),
    defs: std.ArrayList(Def),
    vars: std.ArrayList(Var),
    expr_ids: std.ArrayList(ExprId),
    stmt_ids: std.ArrayList(StmtId),
    branch_ids: std.ArrayList(BranchId),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .exprs = .empty,
            .stmts = .empty,
            .blocks = .empty,
            .branches = .empty,
            .defs = .empty,
            .vars = .empty,
            .expr_ids = .empty,
            .stmt_ids = .empty,
            .branch_ids = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.exprs.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.vars.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.branch_ids.deinit(self.allocator);
    }

    pub fn addExpr(self: *Store, expr: Expr) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, expr);
        return @enumFromInt(idx);
    }

    pub fn getExpr(self: *const Store, id: ExprId) Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    pub fn addStmt(self: *Store, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const idx: u32 = @intCast(self.stmts.items.len);
        try self.stmts.append(self.allocator, stmt);
        return @enumFromInt(idx);
    }

    pub fn getStmt(self: *const Store, id: StmtId) Stmt {
        return self.stmts.items[@intFromEnum(id)];
    }

    pub fn addBlock(self: *Store, block: Block) std.mem.Allocator.Error!BlockId {
        const idx: u32 = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, block);
        return @enumFromInt(idx);
    }

    pub fn getBlock(self: *const Store, id: BlockId) Block {
        return self.blocks.items[@intFromEnum(id)];
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

    pub fn addVarSpan(self: *Store, vars: []const Var) std.mem.Allocator.Error!Span(Var) {
        if (vars.len == 0) return Span(Var).empty();
        const start: u32 = @intCast(self.vars.items.len);
        try self.vars.appendSlice(self.allocator, vars);
        return .{ .start = start, .len = @intCast(vars.len) };
    }

    pub fn sliceVarSpan(self: *const Store, span: Span(Var)) []const Var {
        if (span.len == 0) return &.{};
        return self.vars.items[span.start..][0..span.len];
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

    pub fn addBranchSpan(self: *Store, branches: []const Branch) std.mem.Allocator.Error!Span(BranchId) {
        if (branches.len == 0) return Span(BranchId).empty();
        const start: u32 = @intCast(self.branch_ids.items.len);
        for (branches) |branch| {
            const idx: u32 = @intCast(self.branches.items.len);
            try self.branches.append(self.allocator, branch);
            try self.branch_ids.append(self.allocator, @enumFromInt(idx));
        }
        return .{ .start = start, .len = @intCast(branches.len) };
    }

    pub fn getBranch(self: *const Store, id: BranchId) Branch {
        return self.branches.items[@intFromEnum(id)];
    }

    pub fn sliceBranchSpan(self: *const Store, span: Span(BranchId)) []const BranchId {
        if (span.len == 0) return &.{};
        return self.branch_ids.items[span.start..][0..span.len];
    }
};

test "ir ast tests" {
    std.testing.refAllDecls(@This());
}
