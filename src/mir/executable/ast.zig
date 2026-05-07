//! Executable MIR AST.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const row = @import("../mono_row/mod.zig");
const solved = @import("../lambda_solved/mod.zig");
const type_mod = @import("type.zig");
const mir_ids = @import("../ids.zig");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;
const repr = solved.Representation;

pub const TypeId = type_mod.TypeId;
pub const ProgramLiteralId = mir_ids.ProgramLiteralId;
/// Public `ExprId` declaration.
pub const ExprId = enum(u32) { _ };
/// Public `PatId` declaration.
pub const PatId = enum(u32) { _ };
/// Public `DefId` declaration.
pub const DefId = enum(u32) { _ };
/// Public `StmtId` declaration.
pub const StmtId = enum(u32) { _ };
/// Public `BranchId` declaration.
pub const BranchId = enum(u32) { _ };
/// Public `ExecutableProcId` declaration.
pub const ExecutableProcId = enum(u32) { _ };
/// Public `ExecutableValueRef` declaration.
pub const ExecutableValueRef = enum(u32) { _ };
/// Public `BridgeId` declaration.
pub const BridgeId = enum(u32) { _ };
/// Public `PatternDecisionPlanId` declaration.
pub const PatternDecisionPlanId = enum(u32) { _ };
/// Public `PatternPathValuePlanId` declaration.
pub const PatternPathValuePlanId = enum(u32) { _ };
/// Public `DecisionNodeId` declaration.
pub const DecisionNodeId = enum(u32) { _ };
/// Public `DecisionLeafId` declaration.
pub const DecisionLeafId = enum(u32) { _ };
/// Public `RecordRestProjectionId` declaration.
pub const RecordRestProjectionId = enum(u32) { _ };

/// Public `Span` function.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

/// Public `TypedValue` declaration.
pub const TypedValue = struct {
    ty: TypeId,
    value: ExecutableValueRef,
};

/// Public `RecordFieldExpr` declaration.
pub const RecordFieldExpr = struct {
    field: row.RecordFieldId,
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
    bridge: BridgeId,
};

/// Public `TagPayloadExpr` declaration.
pub const TagPayloadExpr = struct {
    payload: row.TagPayloadId,
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
    bridge: BridgeId,
};

/// Public `TupleItemExpr` declaration.
pub const TupleItemExpr = struct {
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
    bridge: BridgeId,
};

/// Public `ListItemExpr` declaration.
pub const ListItemExpr = struct {
    expr: ExprId,
    ty: TypeId,
    value: ExecutableValueRef,
    bridge: BridgeId,
};

/// Public `Pat` declaration.
pub const Pat = struct {
    ty: TypeId,
    data: Data,

    pub const Data = union(enum) {
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
        record: struct {
            shape: row.RecordShapeId,
            fields: Span(RecordFieldPattern),
            rest: ?PatId = null,
        },
        list: struct {
            items: Span(PatId),
            rest: ?ListRestPattern = null,
        },
        nominal: PatId,
        tuple: Span(PatId),
        as: struct {
            pattern: PatId,
            bind: ExecutableValueRef,
        },
        bind: ExecutableValueRef,
        wildcard,
    };
};

/// Public `TagPayloadPattern` declaration.
pub const TagPayloadPattern = struct {
    payload: row.TagPayloadId,
    pattern: PatId,
};

/// Public `RecordFieldPattern` declaration.
pub const RecordFieldPattern = struct {
    field: row.RecordFieldId,
    pattern: PatId,
};

/// Public `ListRestPattern` declaration.
pub const ListRestPattern = struct {
    index: u32,
    pattern: ?PatId = null,
};

/// Public `Branch` declaration.
pub const Branch = struct {
    pat: PatId,
    guard: ?ExprId = null,
    body: ExprId,
    degenerate: bool = false,
};

/// Public `DirectCallArg` declaration.
pub const DirectCallArg = struct {
    value: ExecutableValueRef,
};

/// Public `CallDirectPlan` declaration.
pub const CallDirectPlan = struct {
    source: canonical.ProcedureValueRef,
    executable_specialization_key: repr.ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(DirectCallArg),
};

pub const CallableSetMemberRef = repr.CallableSetMemberRef;

/// Public `CaptureValueRef` declaration.
pub const CaptureValueRef = struct {
    slot: u32,
    value: ExecutableValueRef,
    exec_ty: TypeId,
};

/// Public `CallableCaptureRecord` declaration.
pub const CallableCaptureRecord = struct {
    capture_shape_key: repr.CaptureShapeKey,
    values: Span(CaptureValueRef),
    record_tmp: ExecutableValueRef,
};

/// Public `BridgePlan` declaration.
pub const BridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: BridgeId,
    box_box: BridgeId,
    struct_: Span(BridgeId),
    tag_union: Span(BridgeId),
    singleton_to_tag_union: struct {
        source_payload: TypeId,
        target_discriminant: u16,
        payload_plan: ?BridgeId,
    },
    tag_union_to_singleton: struct {
        target_payload: TypeId,
        source_discriminant: u16,
        payload_plan: ?BridgeId,
    },
};

/// Public `CallableSetValue` declaration.
pub const CallableSetValue = struct {
    construction_plan: ?repr.CallableSetConstructionPlanId = null,
    callable_set_key: repr.CanonicalCallableSetKey,
    member: CallableSetMemberRef,
    capture_record: ?CallableCaptureRecord = null,
};

/// Public `CallableMatchBranch` declaration.
pub const CallableMatchBranch = struct {
    member: CallableSetMemberRef,
    source_fn_ty: canonical.CanonicalTypeKey,
    capture_payload: ?ExecutableValueRef = null,
    capture_payload_ty: ?TypeId = null,
    executable_specialization_key: repr.ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    arg_transforms: Span(checked_artifact.ExecutableValueTransformRef),
    direct_args: Span(DirectCallArg),
    body: ExprId,
};

/// Public `SourceMatch` declaration.
pub const SourceMatch = struct {
    scrutinee_exprs: Span(ExprId),
    scrutinees: Span(ExecutableValueRef),
    decision_plan: PatternDecisionPlanId,
    branches: Span(BranchId),
};

/// Public `ValueTransformTagBranch` declaration.
pub const ValueTransformTagBranch = struct {
    discriminant: u16,
    body: ExprId,
};

/// Public `ValueTransformList` declaration.
pub const ValueTransformList = struct {
    source: ExecutableValueRef,
    source_elem: ExecutableValueRef,
    source_elem_ty: TypeId,
    target_elem_ty: TypeId,
    body: ExprId,
};

/// Public `PatternDecisionPlan` declaration.
pub const PatternDecisionPlan = struct {
    scrutinees: Span(ExecutableValueRef),
    path_value_plans: Span(PatternPathValuePlanId),
    root: DecisionNodeId,
    leaves: Span(DecisionLeafId),
    branches: Span(BranchId),
};

/// Public `PatternPathValuePlan` declaration.
pub const PatternPathValuePlan = struct {
    path: PatternPath,
    source: PatternPathValueSource,
    ty: TypeId,
};

/// Public `PatternPath` declaration.
pub const PatternPath = struct {
    scrutinee: u32,
    steps: Span(PatternPathStep),
};

/// Public `PatternPathStep` declaration.
pub const PatternPathStep = union(enum) {
    tag_payload_record: row.TagId,
    tag_payload: row.TagPayloadId,
    record_field: row.RecordFieldId,
    record_rest: RecordRestProjectionId,
    tuple_field: u32,
    list_index: ListElementProbe,
    list_rest: ListRestProbe,
    nominal_payload,
};

/// Public `PatternPathValueSource` declaration.
pub const PatternPathValueSource = union(enum) {
    scrutinee: u32,
    tag_payload_record: struct {
        parent: PatternPathValuePlanId,
        tag: row.TagId,
    },
    tag_payload_field: struct {
        parent_payload_record: PatternPathValuePlanId,
        payload: row.TagPayloadId,
    },
    record_field: struct {
        parent: PatternPathValuePlanId,
        field: row.RecordFieldId,
    },
    record_rest: RecordRestProjectionId,
    tuple_field: struct {
        parent: PatternPathValuePlanId,
        field: u32,
    },
    list_element: struct {
        parent: PatternPathValuePlanId,
        probe: ListElementProbe,
    },
    list_rest: struct {
        parent: PatternPathValuePlanId,
        probe: ListRestProbe,
    },
    nominal_payload: PatternPathValuePlanId,
};

/// Public `RecordRestProjection` declaration.
pub const RecordRestProjection = struct {
    parent: PatternPathValuePlanId,
    source_shape: row.RecordShapeId,
    result_shape: row.RecordShapeId,
    projected_fields: Span(RecordRestProjectedField),
};

/// Public `RecordRestProjectedField` declaration.
pub const RecordRestProjectedField = struct {
    source_field: row.RecordFieldId,
    result_field: row.RecordFieldId,
    ty: TypeId,
    result_logical_index: u32,
};

/// Public `ListElementProbe` declaration.
pub const ListElementProbe = struct {
    index: u32,
    from_end: bool = false,
};

/// Public `ListRestProbe` declaration.
pub const ListRestProbe = struct {
    start: u32,
    from_end_count: u32,
};

/// Public `DecisionNode` declaration.
pub const DecisionNode = union(enum) {
    leaf: DecisionLeafId,
    decision_test: DecisionTestNode,
};

/// Public `DecisionTestNode` declaration.
pub const DecisionTestNode = struct {
    path_value: PatternPathValuePlanId,
    edges: Span(DecisionEdge),
    default: ?DecisionNodeId,
};

/// Public `DecisionEdge` declaration.
pub const DecisionEdge = struct {
    pattern_test: PatternTest,
    next: DecisionNodeId,
};

/// Public `PatternTest` declaration.
pub const PatternTest = union(enum) {
    tag: row.TagId,
    int_literal: i128,
    float_f32_literal: f32,
    float_f64_literal: f64,
    decimal_literal: i128,
    str_literal: ProgramLiteralId,
    list_len_exact: u32,
    list_len_at_least: u32,
    guard: ExprId,
};

/// Public `DecisionLeaf` declaration.
pub const DecisionLeaf = struct {
    branch: BranchId,
    degenerate: bool,
    guard: ?ExprId = null,
    body: ExprId,
    fallback: ?DecisionNodeId = null,
    bindings: Span(PatternBinding),
};

/// Public `PatternBinding` declaration.
pub const PatternBinding = struct {
    binder: ExecutableValueRef,
    source: PatternPathValuePlanId,
    bridge: BridgeId,
    ty: TypeId,
};

/// Public `PackedErasedFn` declaration.
pub const PackedErasedFn = struct {
    sig_key: repr.ErasedFnSigKey,
    code: ExecutableProcId,
    capture: ?ExecutableValueRef = null,
    capture_ty: ?TypeId = null,
    capture_shape: repr.CaptureShapeKey,
};

/// Public `Expr` declaration.
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
        unit,
        const_instance: check.CheckedArtifact.ConstInstanceRef,
        const_ref: check.CheckedArtifact.ConstInstantiationKey,
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
        structural_eq: struct {
            lhs: ExprId,
            rhs: ExprId,
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
            capture_ty: ?TypeId = null,
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
            rc_effect: base.LowLevel.RcEffect,
            args: Span(ExprId),
        },
        source_match: SourceMatch,
        value_transform_tag_union: struct {
            source: ExecutableValueRef,
            branches: Span(ValueTransformTagBranch),
        },
        value_transform_list: ValueTransformList,
        if_: struct {
            cond: ExprId,
            true_discriminant: u16,
            then_body: ExprId,
            else_body: ExprId,
        },
        block: struct {
            stmts: Span(StmtId),
            final_expr: ExprId,
        },
        tuple: Span(TupleItemExpr),
        tag_payload: struct {
            tag_union: ExprId,
            payload: row.TagPayloadId,
        },
        tuple_access: struct {
            tuple: ExprId,
            elem_index: u32,
        },
        list: Span(ListItemExpr),
        return_: ExprId,
        crash: ProgramLiteralId,
        runtime_error,
        @"unreachable",
        for_: struct {
            patt: PatId,
            iterable: ExprId,
            body: ExprId,
        },
    };
};

/// Public `Stmt` declaration.
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

/// Public `FnDef` declaration.
pub const FnDef = struct {
    args: Span(TypedValue),
    body: ExprId,
};

/// Public `HostedFnDef` declaration.
pub const HostedFnDef = struct {
    args: Span(TypedValue),
    ret_ty: TypeId,
    hosted: @import("../hosted.zig").Proc,
};

/// Public `DefVal` declaration.
pub const DefVal = union(enum) {
    fn_: FnDef,
    hosted_fn: HostedFnDef,
};

/// Public `ProcOrigin` declaration.
pub const ProcOrigin = union(enum) {
    source: canonical.MirProcedureRef,
    erased_adapter: repr.ErasedAdapterKey,
};

/// Public `Def` declaration.
pub const Def = struct {
    proc: ExecutableProcId,
    origin: ProcOrigin,
    specialization_key: repr.ExecutableSpecializationKey,
    value: DefVal,
};

/// Public `Store` declaration.
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
    stmt_ids: std.ArrayList(StmtId),
    bridge_ids: std.ArrayList(BridgeId),
    value_refs: std.ArrayList(ExecutableValueRef),
    executable_value_transform_refs: std.ArrayList(checked_artifact.ExecutableValueTransformRef),
    capture_value_refs: std.ArrayList(CaptureValueRef),
    direct_call_args: std.ArrayList(DirectCallArg),
    callable_match_branches: std.ArrayList(CallableMatchBranch),
    value_transform_tag_branches: std.ArrayList(ValueTransformTagBranch),
    pattern_decision_plans: std.ArrayList(PatternDecisionPlan),
    pattern_path_value_plans: std.ArrayList(PatternPathValuePlan),
    pattern_path_value_plan_ids: std.ArrayList(PatternPathValuePlanId),
    pattern_path_steps: std.ArrayList(PatternPathStep),
    decision_nodes: std.ArrayList(DecisionNode),
    decision_edges: std.ArrayList(DecisionEdge),
    decision_leaves: std.ArrayList(DecisionLeaf),
    decision_leaf_ids: std.ArrayList(DecisionLeafId),
    pattern_bindings: std.ArrayList(PatternBinding),
    record_rest_projections: std.ArrayList(RecordRestProjection),
    record_rest_projected_fields: std.ArrayList(RecordRestProjectedField),
    bridge_plans: std.ArrayList(BridgePlan),
    tag_payload_patterns: std.ArrayList(TagPayloadPattern),
    record_field_patterns: std.ArrayList(RecordFieldPattern),
    typed_values: std.ArrayList(TypedValue),
    record_field_exprs: std.ArrayList(RecordFieldExpr),
    tag_payload_exprs: std.ArrayList(TagPayloadExpr),
    tuple_item_exprs: std.ArrayList(TupleItemExpr),
    list_item_exprs: std.ArrayList(ListItemExpr),

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
            .stmt_ids = .empty,
            .bridge_ids = .empty,
            .value_refs = .empty,
            .executable_value_transform_refs = .empty,
            .capture_value_refs = .empty,
            .direct_call_args = .empty,
            .callable_match_branches = .empty,
            .value_transform_tag_branches = .empty,
            .pattern_decision_plans = .empty,
            .pattern_path_value_plans = .empty,
            .pattern_path_value_plan_ids = .empty,
            .pattern_path_steps = .empty,
            .decision_nodes = .empty,
            .decision_edges = .empty,
            .decision_leaves = .empty,
            .decision_leaf_ids = .empty,
            .pattern_bindings = .empty,
            .record_rest_projections = .empty,
            .record_rest_projected_fields = .empty,
            .bridge_plans = .empty,
            .tag_payload_patterns = .empty,
            .record_field_patterns = .empty,
            .typed_values = .empty,
            .record_field_exprs = .empty,
            .tag_payload_exprs = .empty,
            .tuple_item_exprs = .empty,
            .list_item_exprs = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.list_item_exprs.deinit(self.allocator);
        self.tuple_item_exprs.deinit(self.allocator);
        self.tag_payload_exprs.deinit(self.allocator);
        self.record_field_exprs.deinit(self.allocator);
        self.typed_values.deinit(self.allocator);
        self.record_field_patterns.deinit(self.allocator);
        self.tag_payload_patterns.deinit(self.allocator);
        self.bridge_plans.deinit(self.allocator);
        self.record_rest_projected_fields.deinit(self.allocator);
        self.record_rest_projections.deinit(self.allocator);
        self.pattern_bindings.deinit(self.allocator);
        self.decision_leaf_ids.deinit(self.allocator);
        self.decision_leaves.deinit(self.allocator);
        self.decision_edges.deinit(self.allocator);
        self.decision_nodes.deinit(self.allocator);
        self.pattern_path_steps.deinit(self.allocator);
        self.pattern_path_value_plan_ids.deinit(self.allocator);
        self.pattern_path_value_plans.deinit(self.allocator);
        self.pattern_decision_plans.deinit(self.allocator);
        for (self.callable_match_branches.items) |*branch| {
            repr.deinitExecutableSpecializationKey(self.allocator, &branch.executable_specialization_key);
        }
        self.callable_match_branches.deinit(self.allocator);
        self.value_transform_tag_branches.deinit(self.allocator);
        self.direct_call_args.deinit(self.allocator);
        self.capture_value_refs.deinit(self.allocator);
        self.executable_value_transform_refs.deinit(self.allocator);
        self.value_refs.deinit(self.allocator);
        self.bridge_ids.deinit(self.allocator);
        self.stmt_ids.deinit(self.allocator);
        self.branch_ids.deinit(self.allocator);
        self.pat_ids.deinit(self.allocator);
        self.expr_ids.deinit(self.allocator);
        for (self.defs.items) |*def| {
            repr.deinitExecutableSpecializationKey(self.allocator, &def.specialization_key);
        }
        self.defs.deinit(self.allocator);
        for (self.exprs.items) |*expr| {
            switch (expr.data) {
                .call_direct => |*call| repr.deinitExecutableSpecializationKey(self.allocator, &call.executable_specialization_key),
                else => {},
            }
        }
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

    pub fn addValueRefExpr(self: *Store, ty: TypeId, value: ExecutableValueRef) std.mem.Allocator.Error!ExprId {
        return self.addExpr(ty, self.freshValueRef(), .{ .value_ref = value });
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

    pub fn getPatternDecisionPlan(self: *const Store, id: PatternDecisionPlanId) PatternDecisionPlan {
        return self.pattern_decision_plans.items[@intFromEnum(id)];
    }

    pub fn addPatternPathValuePlan(self: *Store, plan: PatternPathValuePlan) std.mem.Allocator.Error!PatternPathValuePlanId {
        const idx: u32 = @intCast(self.pattern_path_value_plans.items.len);
        try self.pattern_path_value_plans.append(self.allocator, plan);
        return @enumFromInt(idx);
    }

    pub fn getPatternPathValuePlan(self: *const Store, id: PatternPathValuePlanId) PatternPathValuePlan {
        return self.pattern_path_value_plans.items[@intFromEnum(id)];
    }

    pub fn addDecisionNode(self: *Store, node: DecisionNode) std.mem.Allocator.Error!DecisionNodeId {
        const idx: u32 = @intCast(self.decision_nodes.items.len);
        try self.decision_nodes.append(self.allocator, node);
        return @enumFromInt(idx);
    }

    pub fn getDecisionNode(self: *const Store, id: DecisionNodeId) DecisionNode {
        return self.decision_nodes.items[@intFromEnum(id)];
    }

    pub fn addDecisionLeaf(self: *Store, leaf: DecisionLeaf) std.mem.Allocator.Error!DecisionLeafId {
        const idx: u32 = @intCast(self.decision_leaves.items.len);
        try self.decision_leaves.append(self.allocator, leaf);
        return @enumFromInt(idx);
    }

    pub fn getDecisionLeaf(self: *const Store, id: DecisionLeafId) DecisionLeaf {
        return self.decision_leaves.items[@intFromEnum(id)];
    }

    pub fn addBridgePlan(self: *Store, plan: BridgePlan) std.mem.Allocator.Error!BridgeId {
        const idx: u32 = @intCast(self.bridge_plans.items.len);
        try self.bridge_plans.append(self.allocator, plan);
        return @enumFromInt(idx);
    }

    pub fn getBridgePlan(self: *const Store, id: BridgeId) BridgePlan {
        return self.bridge_plans.items[@intFromEnum(id)];
    }

    pub fn addBridgePlanSpan(self: *Store, ids: []const BridgeId) std.mem.Allocator.Error!Span(BridgeId) {
        if (ids.len == 0) return Span(BridgeId).empty();
        const start: u32 = @intCast(self.bridge_ids.items.len);
        try self.bridge_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceBridgePlanSpan(self: *const Store, span: Span(BridgeId)) []const BridgeId {
        if (span.len == 0) return &.{};
        return self.bridge_ids.items[span.start..][0..span.len];
    }

    pub fn addTagPayloadPatternSpan(self: *Store, values: []const TagPayloadPattern) std.mem.Allocator.Error!Span(TagPayloadPattern) {
        if (values.len == 0) return Span(TagPayloadPattern).empty();
        const start: u32 = @intCast(self.tag_payload_patterns.items.len);
        try self.tag_payload_patterns.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addRecordFieldPatternSpan(self: *Store, values: []const RecordFieldPattern) std.mem.Allocator.Error!Span(RecordFieldPattern) {
        if (values.len == 0) return Span(RecordFieldPattern).empty();
        const start: u32 = @intCast(self.record_field_patterns.items.len);
        try self.record_field_patterns.appendSlice(self.allocator, values);
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

    pub fn addTupleItemExprSpan(self: *Store, values: []const TupleItemExpr) std.mem.Allocator.Error!Span(TupleItemExpr) {
        if (values.len == 0) return Span(TupleItemExpr).empty();
        const start: u32 = @intCast(self.tuple_item_exprs.items.len);
        try self.tuple_item_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addListItemExprSpan(self: *Store, values: []const ListItemExpr) std.mem.Allocator.Error!Span(ListItemExpr) {
        if (values.len == 0) return Span(ListItemExpr).empty();
        const start: u32 = @intCast(self.list_item_exprs.items.len);
        try self.list_item_exprs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addDirectCallArgSpan(self: *Store, values: []const DirectCallArg) std.mem.Allocator.Error!Span(DirectCallArg) {
        if (values.len == 0) return Span(DirectCallArg).empty();
        const start: u32 = @intCast(self.direct_call_args.items.len);
        try self.direct_call_args.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addExecutableValueTransformRefSpan(
        self: *Store,
        values: []const checked_artifact.ExecutableValueTransformRef,
    ) std.mem.Allocator.Error!Span(checked_artifact.ExecutableValueTransformRef) {
        if (values.len == 0) return Span(checked_artifact.ExecutableValueTransformRef).empty();
        const start: u32 = @intCast(self.executable_value_transform_refs.items.len);
        try self.executable_value_transform_refs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptureValueRefSpan(self: *Store, values: []const CaptureValueRef) std.mem.Allocator.Error!Span(CaptureValueRef) {
        if (values.len == 0) return Span(CaptureValueRef).empty();
        const start: u32 = @intCast(self.capture_value_refs.items.len);
        try self.capture_value_refs.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCallableMatchBranchSpan(self: *Store, values: []const CallableMatchBranch) std.mem.Allocator.Error!Span(CallableMatchBranch) {
        if (values.len == 0) return Span(CallableMatchBranch).empty();
        const start: u32 = @intCast(self.callable_match_branches.items.len);
        try self.callable_match_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addValueTransformTagBranchSpan(self: *Store, values: []const ValueTransformTagBranch) std.mem.Allocator.Error!Span(ValueTransformTagBranch) {
        if (values.len == 0) return Span(ValueTransformTagBranch).empty();
        const start: u32 = @intCast(self.value_transform_tag_branches.items.len);
        try self.value_transform_tag_branches.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addPatternPathStepSpan(self: *Store, values: []const PatternPathStep) std.mem.Allocator.Error!Span(PatternPathStep) {
        if (values.len == 0) return Span(PatternPathStep).empty();
        const start: u32 = @intCast(self.pattern_path_steps.items.len);
        try self.pattern_path_steps.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn slicePatternPathStepSpan(self: *const Store, span: Span(PatternPathStep)) []const PatternPathStep {
        if (span.len == 0) return &.{};
        return self.pattern_path_steps.items[span.start..][0..span.len];
    }

    pub fn addPatternPathValuePlanSpan(self: *Store, ids: []const PatternPathValuePlanId) std.mem.Allocator.Error!Span(PatternPathValuePlanId) {
        if (ids.len == 0) return Span(PatternPathValuePlanId).empty();
        const start: u32 = @intCast(self.pattern_path_value_plan_ids.items.len);
        try self.pattern_path_value_plan_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn slicePatternPathValuePlanSpan(self: *const Store, span: Span(PatternPathValuePlanId)) []const PatternPathValuePlanId {
        if (span.len == 0) return &.{};
        return self.pattern_path_value_plan_ids.items[span.start..][0..span.len];
    }

    pub fn addDecisionEdgeSpan(self: *Store, values: []const DecisionEdge) std.mem.Allocator.Error!Span(DecisionEdge) {
        if (values.len == 0) return Span(DecisionEdge).empty();
        const start: u32 = @intCast(self.decision_edges.items.len);
        try self.decision_edges.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceDecisionEdgeSpan(self: *const Store, span: Span(DecisionEdge)) []const DecisionEdge {
        if (span.len == 0) return &.{};
        return self.decision_edges.items[span.start..][0..span.len];
    }

    pub fn addDecisionLeafSpan(self: *Store, ids: []const DecisionLeafId) std.mem.Allocator.Error!Span(DecisionLeafId) {
        if (ids.len == 0) return Span(DecisionLeafId).empty();
        const start: u32 = @intCast(self.decision_leaf_ids.items.len);
        try self.decision_leaf_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceDecisionLeafSpan(self: *const Store, span: Span(DecisionLeafId)) []const DecisionLeafId {
        if (span.len == 0) return &.{};
        return self.decision_leaf_ids.items[span.start..][0..span.len];
    }

    pub fn addPatternBindingSpan(self: *Store, values: []const PatternBinding) std.mem.Allocator.Error!Span(PatternBinding) {
        if (values.len == 0) return Span(PatternBinding).empty();
        const start: u32 = @intCast(self.pattern_bindings.items.len);
        try self.pattern_bindings.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn slicePatternBindingSpan(self: *const Store, span: Span(PatternBinding)) []const PatternBinding {
        if (span.len == 0) return &.{};
        return self.pattern_bindings.items[span.start..][0..span.len];
    }

    pub fn addRecordRestProjectedFieldSpan(self: *Store, values: []const RecordRestProjectedField) std.mem.Allocator.Error!Span(RecordRestProjectedField) {
        if (values.len == 0) return Span(RecordRestProjectedField).empty();
        const start: u32 = @intCast(self.record_rest_projected_fields.items.len);
        try self.record_rest_projected_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceRecordRestProjectedFieldSpan(self: *const Store, span: Span(RecordRestProjectedField)) []const RecordRestProjectedField {
        if (span.len == 0) return &.{};
        return self.record_rest_projected_fields.items[span.start..][0..span.len];
    }

    pub fn addRecordRestProjection(self: *Store, projection: RecordRestProjection) std.mem.Allocator.Error!RecordRestProjectionId {
        const idx: u32 = @intCast(self.record_rest_projections.items.len);
        try self.record_rest_projections.append(self.allocator, projection);
        return @enumFromInt(idx);
    }

    pub fn getRecordRestProjection(self: *const Store, id: RecordRestProjectionId) RecordRestProjection {
        return self.record_rest_projections.items[@intFromEnum(id)];
    }
};

test "executable ast tests" {
    std.testing.refAllDecls(@This());
}
