//! Lambda Mono IR.
//!
//! This IR has no function type and no value-call node. Function values are
//! ordinary generated callable values or erased callable values.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");

const Common = @import("../common.zig");
const Mono = @import("../monotype/ast.zig");
const MonoType = @import("../monotype/type.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Type = @import("type.zig");
const names = check.CheckedNames;

const checked = check.CheckedModule;

/// Identifier for an expression in Lambda Mono IR.
pub const ExprId = enum(u32) { _ };
/// Identifier for a pattern in Lambda Mono IR.
pub const PatId = enum(u32) { _ };
/// Identifier for a statement in Lambda Mono IR.
pub const StmtId = enum(u32) { _ };
/// Identifier for a function body in Lambda Mono IR.
pub const FnId = Type.FnId;
/// Identifier for a local binding in Lambda Mono IR.
pub const LocalId = enum(u32) { _ };
/// Owned string literal id shared with the lifted stage.
pub const StringLiteralId = Lifted.StringLiteralId;

/// Slice descriptor over one of the program side arrays.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

/// Local binding with its symbol, type, and optional checked binder.
pub const Local = struct {
    id: LocalId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
    binder: ?checked.PatternBinderId = null,
};

/// Local id paired with its Lambda Mono type.
pub const TypedLocal = struct {
    local: LocalId,
    ty: Type.TypeId,
};

/// Record field expression entry.
pub const FieldExpr = struct {
    name: Type.names.RecordFieldNameId,
    value: ExprId,
};

/// Record destructuring field pattern.
pub const RecordDestruct = struct {
    name: Type.names.RecordFieldNameId,
    pattern: PatId,
};

/// Read of one capture from a capture record.
pub const CaptureSlot = struct {
    record: ExprId,
    symbol: Common.Symbol,
};

/// Direct call to a known Lambda Mono function.
pub const DirectCall = struct {
    target: FnId,
    args: Span(ExprId),
};

/// Indirect call through an erased callable value.
pub const ErasedCall = struct {
    callee: ExprId,
    args: Span(ExprId),
};

/// Erased callable value packed with an optional capture payload.
pub const PackedErasedFn = struct {
    target: FnId,
    capture: ?ExprId,
};

/// Finite callable value with variant id and optional payload.
pub const CallableValue = struct {
    ty: Type.TypeId,
    variant: Type.FnVariantId,
    payload: ?ExprId,
};

/// Typed Lambda Mono expression.
pub const Expr = struct {
    ty: Type.TypeId,
    data: ExprData,
};

/// Lambda Mono expression forms.
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
    capture_record: Span(ExprId),
    tag: struct {
        name: Type.names.TagNameId,
        payloads: Span(ExprId),
    },
    callable: CallableValue,
    nominal: ExprId,
    let_: struct {
        bind: PatId,
        value: ExprId,
        rest: ExprId,
    },
    direct_call: DirectCall,
    indirect_erased_call: ErasedCall,
    packed_erased_fn: PackedErasedFn,
    low_level: struct {
        op: can.CIR.Expr.LowLevel,
        args: Span(ExprId),
    },
    field_access: struct {
        receiver: ExprId,
        field: Type.names.RecordFieldNameId,
    },
    capture_access: CaptureSlot,
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
    expect_err: ExpectErrExpr,
    expect: ExprId,
};

/// The Err arm of a `?` operator used directly inside a top-level `expect`.
/// Fails the enclosing expect at runtime with the pre-composed message and
/// the source region of the `?` itself. Never returns.
pub const ExpectErrExpr = struct {
    /// String-typed expression producing the failure message (includes the
    /// rendered Err value).
    msg: ExprId,
    /// Source region of the `?` expression, for failure reporting.
    region: base.Region,
};

/// Typed Lambda Mono pattern.
pub const Pat = struct {
    ty: Type.TypeId,
    data: PatData,
};

/// Lambda Mono pattern forms.
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
        name: Type.names.TagNameId,
        payloads: Span(PatId),
    },
    callable: struct {
        variant: Type.FnVariantId,
        payload: ?PatId,
    },
    nominal: PatId,
    int_lit: can.CIR.IntValue,
    dec_lit: builtins.dec.RocDec,
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

/// Lambda Mono statement forms.
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

/// Lambda Mono function body.
pub const Fn = struct {
    symbol: Common.Symbol,
    source: ?Mono.FnTemplate = null,
    args: Span(TypedLocal),
    body: FnBody,
    ret: Type.TypeId,
};

/// Body availability for a Lambda Mono function.
pub const FnBody = union(enum) {
    roc: ExprId,
    hosted,
};

/// Root request bound to a Lambda Mono function.
pub const Root = struct {
    fn_id: FnId,
    request: checked.RootRequest,
};

/// Runtime layout requested for a checked data value.
pub const LayoutRequest = struct {
    checked_type: checked.CheckedTypeId,
    ty: Type.TypeId,
};

/// Runtime schema requested for a named runtime value shape.
pub const RuntimeSchemaRequest = struct {
    def: MonoType.TypeDef,
    ty: Type.TypeId,
};

/// Complete Lambda Mono program plus side arrays.
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
        string_literals: std.ArrayList(Mono.StringLiteral),
    ) Program {
        return .{
            .allocator = allocator,
            .names = name_store,
            .next_symbol = 0,
            .types = Type.Store.init(allocator),
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

    pub fn stringLiteralText(self: *const Program, id: StringLiteralId) []const u8 {
        return self.stringLiteral(id).text();
    }

    pub fn stringLiteral(self: *const Program, id: StringLiteralId) Mono.StringLiteral {
        return self.string_literals.items[@intFromEnum(id)];
    }
};

test "lambda mono ast declarations are referenced" {
    std.testing.refAllDecls(@This());
}
