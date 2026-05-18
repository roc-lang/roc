//! Source-blind executable IR.

const std = @import("std");
const base = @import("base");
const mir = @import("mir");
const symbol_mod = @import("symbol");
const layout_mod = @import("layout.zig");
const row = mir.MonoRow;
const repr = mir.LambdaSolved.Representation;

/// Interned symbol identifiers referenced by lowered IR nodes.
pub const Symbol = symbol_mod.Symbol;
/// Logical layout references assigned during IR lowering.
pub const LayoutRef = layout_mod.Ref;
/// Executable procedure selected before IR lowering.
pub const ProcRef = mir.Executable.Ast.ExecutableProcId;
/// Lowered-program string literal payload.
pub const ProgramLiteralId = mir.Ids.ProgramLiteralId;
/// Platform-hosted procedure metadata.
pub const HostedProc = mir.Hosted.Proc;
/// Executable procedure origin preserved for ABI-sensitive lowering.
pub const ProcOrigin = mir.Executable.Ast.ProcOrigin;

/// Identifier for a lowered IR expression node.
pub const ExprId = enum(u32) { _ };
/// Identifier for a lowered IR statement node.
pub const StmtId = enum(u32) { _ };
/// Identifier for a lowered IR block.
pub const BlockId = enum(u32) { _ };
/// Identifier for a lowered IR switch branch.
pub const BranchId = enum(u32) { _ };
/// Identifier for a lowered IR definition.
pub const DefId = enum(u32) { _ };
/// Identifier for an explicit bridge plan.
pub const BridgePlanId = enum(u32) { _ };
/// Identifier for a callable-set runtime encoding published during IR lowering.
pub const CallableSetRuntimeEncodingId = enum(u32) { _ };

/// Slice metadata for contiguous ids stored in side arrays.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        /// Return an empty span.
        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

/// A symbol paired with its assigned logical layout.
pub const Var = struct {
    layout: LayoutRef,
    symbol: Symbol,
};

/// Literal payloads lowered into IR.
pub const Lit = union(enum) {
    int: i128,
    f32: f32,
    f64: f64,
    dec: i128,
    str: ProgramLiteralId,
};

/// Explicit bridge operation chosen before IR is lowered into LIR.
pub const BridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: BridgePlanId,
    box_box: BridgePlanId,
    struct_: Span(BridgePlanId),
    tag_union: Span(BridgePlanId),
    singleton_to_tag_union: struct {
        source_payload: LayoutRef,
        target_discriminant: u16,
        payload_plan: ?BridgePlanId,
    },
    tag_union_to_singleton: struct {
        target_payload: LayoutRef,
        source_discriminant: u16,
        payload_plan: ?BridgePlanId,
    },
};

/// Explicit logical source for a discriminant read.
pub const DiscriminantSource = union(enum) {
    runtime_tag_union: row.TagUnionShapeId,
    runtime_callable_set: CallableSetRuntimeEncodingId,
    known_singleton: u16,
    known_singleton_callable_set: struct {
        encoding: CallableSetRuntimeEncodingId,
        discriminant: u16,
    },
};

/// Layout-owned runtime encoding for a callable-set member in IR layout refs.
pub const CallableSetRuntimeEncodingMember = struct {
    member: repr.CallableSetMemberId,
    variant_index: u16,
    discriminant: u16,
    payload_layout: LayoutRef,
    payload_exec_key: ?repr.CanonicalExecValueTypeKey = null,
};

/// Runtime encoding table for one callable-set value layout in lowered IR.
pub const CallableSetRuntimeEncoding = struct {
    callable_set_key: repr.CanonicalCallableSetKey,
    value_layout: LayoutRef,
    members: Span(CallableSetRuntimeEncodingMember),
};

/// Lowered IR expression node.
pub const Expr = union(enum) {
    var_: Var,
    lit: Lit,
    fn_ptr: ProcRef,
    null_ptr,
    make_union: struct {
        variant_index: u16,
        discriminant: u16,
        payload: ?Var,
        payload_bridge_plan: ?BridgePlanId,
    },
    get_union_id: struct {
        value: Var,
        source: DiscriminantSource,
    },
    get_union_struct: struct {
        value: Var,
        variant_index: u16,
        tag_discriminant: u16,
    },
    make_struct: struct {
        fields: Span(Var),
        field_bridge_plans: Span(BridgePlanId),
    },
    make_list: struct {
        elems: Span(Var),
        elem_bridge_plans: Span(BridgePlanId),
    },
    get_struct_field: struct {
        record: Var,
        field_index: u16,
        field_bridge_plan: BridgePlanId,
    },
    nominal_reinterpret: Var,
    bridge: struct {
        value: Var,
        plan: BridgePlanId,
    },
    layout_size: LayoutRef,
    call_direct: struct {
        proc: ProcRef,
        args: Span(Var),
    },
    structural_eq: struct {
        lhs: Var,
        rhs: Var,
    },
    call_erased: struct {
        func: Var,
        args: Span(Var),
    },
    packed_erased_fn: struct {
        proc: ProcRef,
        capture: ?Var,
        capture_layout: ?LayoutRef,
    },
    call_low_level: struct {
        op: base.LowLevel,
        rc_effect: base.LowLevel.RcEffect,
        args: Span(Var),
    },
};

/// Block terminator.
pub const Term = union(enum) {
    value: Var,
    return_: Var,
    crash: ProgramLiteralId,
    runtime_error,
    @"unreachable": void,
};

/// Lowered IR block.
pub const Block = struct {
    stmts: Span(StmtId),
    term: Term,
};

/// Lowered IR switch branch.
pub const Branch = struct {
    value: u64,
    block: BlockId,
};

/// Lowered IR statement node.
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
    return_: Var,
    crash: ProgramLiteralId,
    runtime_error,
    break_,
    for_list: struct {
        elem: Var,
        iterable: Var,
        body: BlockId,
        elem_bridge_plan: BridgePlanId,
    },
    while_: struct {
        cond: BlockId,
        body: BlockId,
    },
};

/// Lowered IR definition.
pub const Def = struct {
    proc: ProcRef,
    origin: ProcOrigin,
    debug_name: ?Symbol = null,
    args: Span(Var),
    body: ?BlockId = null,
    ret_layout: LayoutRef,
    hosted: ?HostedProc = null,
};

/// Owning store for lowered IR nodes and side arrays.
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
    bridge_plans: std.ArrayList(BridgePlan),
    bridge_plan_ids: std.ArrayList(BridgePlanId),

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
            .bridge_plans = .empty,
            .bridge_plan_ids = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.bridge_plan_ids.deinit(self.allocator);
        self.bridge_plans.deinit(self.allocator);
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

    pub fn addBridgePlan(self: *Store, plan: BridgePlan) std.mem.Allocator.Error!BridgePlanId {
        const idx: u32 = @intCast(self.bridge_plans.items.len);
        try self.bridge_plans.append(self.allocator, plan);
        return @enumFromInt(idx);
    }

    pub fn getBridgePlan(self: *const Store, id: BridgePlanId) BridgePlan {
        return self.bridge_plans.items[@intFromEnum(id)];
    }

    pub fn addBridgePlanSpan(self: *Store, ids: []const BridgePlanId) std.mem.Allocator.Error!Span(BridgePlanId) {
        if (ids.len == 0) return Span(BridgePlanId).empty();
        const start: u32 = @intCast(self.bridge_plan_ids.items.len);
        try self.bridge_plan_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceBridgePlanSpan(self: *const Store, span: Span(BridgePlanId)) []const BridgePlanId {
        if (span.len == 0) return &.{};
        return self.bridge_plan_ids.items[span.start..][0..span.len];
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
