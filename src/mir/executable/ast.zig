//! Executable MIR AST.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const row = @import("../mono_row/mod.zig");
const solved = @import("../lambda_solved/mod.zig");
const type_mod = @import("type.zig");
const ids = @import("../ids.zig");

const canonical = check.CanonicalNames;
const repr = solved.Representation;

pub const TypeId = type_mod.TypeId;
pub const ProgramLiteralId = ids.ProgramLiteralId;
pub const ExprId = enum(u32) { _ };
pub const PatId = enum(u32) { _ };
pub const DefId = enum(u32) { _ };
pub const StmtId = enum(u32) { _ };
pub const BranchId = enum(u32) { _ };
pub const ExecutableProcId = enum(u32) { _ };
pub const ExecutableValueRef = enum(u32) { _ };
pub const BridgeId = enum(u32) { _ };
pub const PatternDecisionPlanId = enum(u32) { _ };

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const TypedValue = struct {
    ty: TypeId,
    value: ExecutableValueRef,
};

pub const RecordFieldExpr = struct {
    field: row.RecordFieldId,
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
};

pub const TagPayloadExpr = struct {
    payload: row.TagPayloadId,
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
};

pub const Pat = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        bool_lit: bool,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        str_lit: ProgramLiteralId,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            payloads: Span(TagPayloadPattern),
        },
        bind: ExecutableValueRef,
        wildcard,
    };
};

pub const TagPayloadPattern = struct {
    payload: row.TagPayloadId,
    pattern: PatId,
};

pub const Branch = struct {
    pat: PatId,
    body: ExprId,
};

pub const DirectCallArg = struct {
    value: ExecutableValueRef,
    bridge: ?BridgeId = null,
};

pub const CallDirectPlan = struct {
    source: canonical.ProcedureValueRef,
    executable_specialization_key: repr.ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(DirectCallArg),
    result_bridge: ?BridgeId = null,
};

pub const CallableSetMemberRef = repr.CallableSetMemberRef;

pub const CaptureValueRef = struct {
    slot: u32,
    value: ExecutableValueRef,
    exec_ty: TypeId,
};

pub const CallableCaptureRecord = struct {
    capture_shape_key: repr.CaptureShapeKey,
    values: Span(CaptureValueRef),
    record_tmp: ExecutableValueRef,
};

pub const CallableSetValue = struct {
    construction_plan: repr.CallableSetConstructionPlanId,
    callable_set_key: repr.CanonicalCallableSetKey,
    member: CallableSetMemberRef,
    capture_record: ?CallableCaptureRecord = null,
};

pub const CallableMatchBranch = struct {
    member: CallableSetMemberRef,
    source_fn_ty: canonical.CanonicalTypeKey,
    executable_specialization_key: repr.ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(DirectCallArg),
    result_bridge: ?BridgeId = null,
};

pub const SourceMatch = struct {
    scrutinee_exprs: Span(ExprId),
    scrutinees: Span(ExecutableValueRef),
    decision_plan: PatternDecisionPlanId,
    branches: Span(BranchId),
};

pub const PatternDecisionPlan = struct {
    scrutinees: Span(ExecutableValueRef),
    branches: Span(BranchId),
};

pub const PackedErasedFn = struct {
    sig_key: repr.ErasedFnSigKey,
    code: repr.ErasedAdapterKey,
    capture: repr.CaptureShapeKey,
};

pub const Expr = struct {
    ty: TypeId,
    value: ExecutableValueRef,
    data: Data,

    pub const Data = union(enum) {
        value_ref: ExecutableValueRef,
        int_lit: i128,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        dec_lit: i128,
        str_lit: ProgramLiteralId,
        bool_lit: bool,
        unit,
        const_ref: check.CheckedArtifact.ConstRef,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            payloads: Span(TagPayloadExpr),
        },
        record: struct {
            shape: row.RecordShapeId,
            fields: Span(RecordFieldExpr),
        },
        nominal_reinterpret: ExprId,
        access: struct {
            record: ExprId,
            field: row.RecordFieldId,
        },
        bridge: struct {
            bridge: BridgeId,
            value: ExecutableValueRef,
        },
        call_direct: CallDirectPlan,
        call_erased: struct {
            func: ExecutableValueRef,
            args: Span(ExecutableValueRef),
            sig_key: repr.ErasedFnSigKey,
        },
        callable_set_value: CallableSetValue,
        callable_match: struct {
            callable_set_key: repr.CanonicalCallableSetKey,
            requested_source_fn_ty: canonical.CanonicalTypeKey,
            callee: ExecutableValueRef,
            args: Span(ExecutableValueRef),
            branches: Span(CallableMatchBranch),
            result_ty: TypeId,
            result_value: ExecutableValueRef,
        },
        packed_erased_fn: PackedErasedFn,
        low_level: struct {
            op: base.LowLevel,
            args: Span(ExprId),
        },
        source_match: SourceMatch,
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
        tag_payload: struct {
            tag_union: ExprId,
            payload: row.TagPayloadId,
        },
        tuple_access: struct {
            tuple: ExprId,
            elem_index: u32,
        },
        list: Span(ExprId),
        bool_not: ExprId,
        return_: ExprId,
        crash: ProgramLiteralId,
        runtime_error,
        for_: struct {
            patt: PatId,
            iterable: ExprId,
            body: ExprId,
        },
    };
};

pub const Stmt = union(enum) {
    decl: struct {
        value: ExecutableValueRef,
        body: ExprId,
    },
    reassign: struct {
        target: ExecutableValueRef,
        body: ExprId,
    },
    expr: ExprId,
    debug: ExprId,
    expect: ExprId,
    crash: ProgramLiteralId,
    return_: ExecutableValueRef,
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

pub const FnDef = struct {
    args: Span(TypedValue),
    body: ExprId,
};

pub const Def = struct {
    proc: ExecutableProcId,
    source_proc: canonical.MonoSpecializedProcRef,
    specialization_key: repr.ExecutableSpecializationKey,
    value: FnDef,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    next_value_ref: u32 = 0,
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    branches: std.ArrayList(Branch),
    stmts: std.ArrayList(Stmt),
    defs: std.ArrayList(Def),
    expr_ids: std.ArrayList(ExprId),
    pat_ids: std.ArrayList(PatId),
    branch_ids: std.ArrayList(BranchId),
    value_refs: std.ArrayList(ExecutableValueRef),
    capture_value_refs: std.ArrayList(CaptureValueRef),
    direct_call_args: std.ArrayList(DirectCallArg),
    callable_match_branches: std.ArrayList(CallableMatchBranch),
    pattern_decision_plans: std.ArrayList(PatternDecisionPlan),
    tag_payload_patterns: std.ArrayList(TagPayloadPattern),
    typed_values: std.ArrayList(TypedValue),
    record_field_exprs: std.ArrayList(RecordFieldExpr),
    tag_payload_exprs: std.ArrayList(TagPayloadExpr),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .next_value_ref = 0,
            .exprs = .empty,
            .pats = .empty,
            .branches = .empty,
            .stmts = .empty,
            .defs = .empty,
            .expr_ids = .empty,
            .pat_ids = .empty,
            .branch_ids = .empty,
            .value_refs = .empty,
            .capture_value_refs = .empty,
            .direct_call_args = .empty,
            .callable_match_branches = .empty,
            .pattern_decision_plans = .empty,
            .tag_payload_patterns = .empty,
            .typed_values = .empty,
            .record_field_exprs = .empty,
            .tag_payload_exprs = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.tag_payload_exprs.deinit(self.allocator);
        self.record_field_exprs.deinit(self.allocator);
        self.typed_values.deinit(self.allocator);
        self.tag_payload_patterns.deinit(self.allocator);
        self.pattern_decision_plans.deinit(self.allocator);
        self.callable_match_branches.deinit(self.allocator);
        self.direct_call_args.deinit(self.allocator);
        self.capture_value_refs.deinit(self.allocator);
        self.value_refs.deinit(self.allocator);
        self.branch_ids.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        for (self.defs.items) |*def| {
            repr.deinitExecutableSpecializationKey(self.allocator, &def.specialization_key);
        }
        self.defs.deinit(self.allocator);
        self.stmts.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.pats.deinit(self.allocator);
        self.exprs.deinit(self.allocator);
    }

    pub fn addExpr(self: *Store, ty: TypeId, value: ExecutableValueRef, data: Expr.Data) std.mem.Allocator.Error!ExprId {
        const idx: u32 = @intCast(self.exprs.items.len);
        try self.exprs.append(self.allocator, .{ .ty = ty, .value = value, .data = data });
        return @enumFromInt(idx);
    }

    pub fn freshValueRef(self: *Store) ExecutableValueRef {
        const id: ExecutableValueRef = @enumFromInt(self.next_value_ref);
        self.next_value_ref += 1;
        return id;
    }

    pub fn getExpr(self: *const Store, id: ExprId) Expr {
        return self.exprs.items[@intFromEnum(id)];
    }

    pub fn addStmt(self: *Store, stmt: Stmt) std.mem.Allocator.Error!StmtId {
        const idx: u32 = @intCast(self.stmts.items.len);
        try self.stmts.append(self.allocator, stmt);
        return @enumFromInt(idx);
    }

    pub fn addPat(self: *Store, pat: Pat) std.mem.Allocator.Error!PatId {
        const idx: u32 = @intCast(self.pats.items.len);
        try self.pats.append(self.allocator, pat);
        return @enumFromInt(idx);
    }

    pub fn addBranch(self: *Store, branch: Branch) std.mem.Allocator.Error!BranchId {
        const idx: u32 = @intCast(self.branches.items.len);
        try self.branches.append(self.allocator, branch);
        return @enumFromInt(idx);
    }

    pub fn addPatternDecisionPlan(self: *Store, plan: PatternDecisionPlan) std.mem.Allocator.Error!PatternDecisionPlanId {
        const idx: u32 = @intCast(self.pattern_decision_plans.items.len);
        try self.pattern_decision_plans.append(self.allocator, plan);
        return @enumFromInt(idx);
    }

    pub fn addTagPayloadPatternSpan(self: *Store, values: []const TagPayloadPattern) std.mem.Allocator.Error!Span(TagPayloadPattern) {
        if (values.len == 0) return Span(TagPayloadPattern).empty();
        const start: u32 = @intCast(self.tag_payload_patterns.items.len);
        try self.tag_payload_patterns.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addDef(self: *Store, def: Def) std.mem.Allocator.Error!DefId {
        const idx: u32 = @intCast(self.defs.items.len);
        try self.defs.append(self.allocator, def);
        return @enumFromInt(idx);
    }

    pub fn addExprSpan(self: *Store, ids: []const ExprId) std.mem.Allocator.Error!Span(ExprId) {
        if (ids.len == 0) return Span(ExprId).empty();
        const start: u32 = @intCast(self.expr_ids.items.len);
        try self.expr_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addPatSpan(self: *Store, ids: []const PatId) std.mem.Allocator.Error!Span(PatId) {
        if (ids.len == 0) return Span(PatId).empty();
        const start: u32 = @intCast(self.pat_ids.items.len);
        try self.pat_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addBranchSpan(self: *Store, ids: []const BranchId) std.mem.Allocator.Error!Span(BranchId) {
        if (ids.len == 0) return Span(BranchId).empty();
        const start: u32 = @intCast(self.branch_ids.items.len);
        try self.branch_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addValueRefSpan(self: *Store, refs: []const ExecutableValueRef) std.mem.Allocator.Error!Span(ExecutableValueRef) {
        if (refs.len == 0) return Span(ExecutableValueRef).empty();
        const start: u32 = @intCast(self.value_refs.items.len);
        try self.value_refs.appendSlice(self.allocator, refs);
        return .{ .start = start, .len = @intCast(refs.len) };
    }

    pub fn addStmtSpan(self: *Store, ids: []const StmtId) std.mem.Allocator.Error!Span(StmtId) {
        if (ids.len == 0) return Span(StmtId).empty();
        const start: u32 = @intCast(self.stmt_ids.items.len);
        try self.stmt_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn addTypedValueSpan(self: *Store, values: []const TypedValue) std.mem.Allocator.Error!Span(TypedValue) {
        if (values.len == 0) return Span(TypedValue).empty();
        const start: u32 = @intCast(self.typed_values.items.len);
        try self.typed_values.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addRecordFieldExprSpan(self: *Store, values: []const RecordFieldExpr) std.mem.Allocator.Error!Span(RecordFieldExpr) {
        if (values.len == 0) return Span(RecordFieldExpr).empty();
        const start: u32 = @intCast(self.record_field_exprs.items.len);
        try self.record_field_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTagPayloadExprSpan(self: *Store, values: []const TagPayloadExpr) std.mem.Allocator.Error!Span(TagPayloadExpr) {
        if (values.len == 0) return Span(TagPayloadExpr).empty();
        const start: u32 = @intCast(self.tag_payload_exprs.items.len);
        try self.tag_payload_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addDirectCallArgSpan(self: *Store, values: []const DirectCallArg) std.mem.Allocator.Error!Span(DirectCallArg) {
        if (values.len == 0) return Span(DirectCallArg).empty();
        const start: u32 = @intCast(self.direct_call_args.items.len);
        try self.direct_call_args.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }
};

test "executable ast tests" {
    std.testing.refAllDecls(@This());
}
