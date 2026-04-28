//! Executable MIR AST.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const row = @import("../mono_row/mod.zig");
const solved = @import("../lambda_solved/mod.zig");
const type_mod = @import("type.zig");

const canonical = check.CanonicalNames;
const repr = solved.Representation;

pub const TypeId = type_mod.TypeId;
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

pub const Pat = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
        bool_lit: bool,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            payloads: Span(PatId),
        },
        bind: ExecutableValueRef,
    };
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
    executable_specialization_key: canonical.CanonicalTypeKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(DirectCallArg),
    result_bridge: ?BridgeId = null,
};

pub const CallableSetMemberRef = repr.CallableSetMemberRef;

pub const CallableMatchBranch = struct {
    member: CallableSetMemberRef,
    executable_specialization_key: canonical.CanonicalTypeKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(DirectCallArg),
    result_bridge: ?BridgeId = null,
};

pub const SourceMatch = struct {
    scrutinees: Span(ExecutableValueRef),
    decision_plan: PatternDecisionPlanId,
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
        str_lit: base.StringLiteral.Idx,
        bool_lit: bool,
        unit,
        const_ref: check.CheckedArtifact.ConstRef,
        tag: struct {
            union_shape: row.TagUnionShapeId,
            tag: row.TagId,
            payloads: Span(ExecutableValueRef),
        },
        record: struct {
            shape: row.RecordShapeId,
            fields: Span(TypedValue),
        },
        access: struct {
            record: ExecutableValueRef,
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
        callable_set_value: struct {
            key: repr.CanonicalCallableSetKey,
            members: Span(CallableSetMemberRef),
        },
        callable_match: struct {
            callee: ExecutableValueRef,
            args: Span(ExecutableValueRef),
            branches: Span(CallableMatchBranch),
        },
        packed_erased_fn: PackedErasedFn,
        low_level: struct {
            op: base.LowLevel,
            args: Span(ExecutableValueRef),
        },
        source_match: SourceMatch,
        if_: struct {
            cond: ExecutableValueRef,
            then_body: ExprId,
            else_body: ExprId,
        },
        block: struct {
            stmts: Span(StmtId),
            final_expr: ExprId,
        },
        tuple: Span(ExecutableValueRef),
        tag_payload: struct {
            tag_union: ExecutableValueRef,
            payload: row.TagPayloadId,
        },
        tuple_access: struct {
            tuple: ExecutableValueRef,
            elem_index: u32,
        },
        list: Span(ExecutableValueRef),
        return_: ExecutableValueRef,
        runtime_error: base.StringLiteral.Idx,
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
    crash: base.StringLiteral.Idx,
    return_: ExecutableValueRef,
    break_,
};

pub const FnDef = struct {
    args: Span(TypedValue),
    body: ExprId,
};

pub const Def = struct {
    proc: ExecutableProcId,
    source_proc: canonical.ProcedureValueRef,
    specialization_key: canonical.CanonicalTypeKey,
    value: FnDef,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    exprs: std.ArrayList(Expr),
    pats: std.ArrayList(Pat),
    branches: std.ArrayList(Branch),
    stmts: std.ArrayList(Stmt),
    defs: std.ArrayList(Def),
    expr_ids: std.ArrayList(ExprId),
    value_refs: std.ArrayList(ExecutableValueRef),
    direct_call_args: std.ArrayList(DirectCallArg),
    callable_match_branches: std.ArrayList(CallableMatchBranch),
    typed_values: std.ArrayList(TypedValue),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .exprs = .empty,
            .pats = .empty,
            .branches = .empty,
            .stmts = .empty,
            .defs = .empty,
            .expr_ids = .empty,
            .value_refs = .empty,
            .direct_call_args = .empty,
            .callable_match_branches = .empty,
            .typed_values = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.typed_values.deinit(self.allocator);
        self.callable_match_branches.deinit(self.allocator);
        self.direct_call_args.deinit(self.allocator);
        self.value_refs.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
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
};

test "executable ast tests" {
    std.testing.refAllDecls(@This());
}
