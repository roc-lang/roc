//! Specialization from solved callable semantics to executable structured callable IR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const ContextMono = @import("ContextMono.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

/// Identifies one executable callable specialization chosen from solved
/// callable semantics.
pub const CallableInstId = enum(u32) {
    _,
};

/// One structural projection applied before demanding executable callable
/// specializations from a higher-order parameter.
pub const CallableParamProjection = union(enum) {
    field: Monotype.Name,
    tuple_elem: u32,
};

pub const CallableParamProjectionSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamProjectionSpan) bool {
        return self.len == 0;
    }
};

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    member_set_id: PlanMemberSetId,
};

pub const CallableParamSpecSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamSpecSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamSpecSpan) bool {
        return self.len == 0;
    }
};

pub const CallableInst = struct {
    template: Lambdasolved.CallableTemplateId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_context_callable_inst: ?CallableInstId,
    callable_param_specs: CallableParamSpecSpan = .empty(),
    callable_def: CallableDefId,
    runtime_value: RuntimeValue = .direct_lambda,
};

pub const RuntimeValue = union(enum) {
    direct_lambda,
    closure: struct {
        capture_tuple_monotype: ContextMono.ResolvedMonotype,
    },
};

pub const PackedFnId = enum(u32) {
    _,
};

pub const PackedFn = struct {
    members: PlanMemberSpan,
};

pub const IndirectCallId = enum(u32) {
    _,
};

pub const IndirectCall = struct {
    members: PlanMemberSpan,
};

pub const ExprId = enum(u32) {
    _,
};

pub const ExprIdSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ExprIdSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ExprIdSpan) bool {
        return self.len == 0;
    }
};

pub const StmtId = enum(u32) {
    _,
};

pub const StmtIdSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() StmtIdSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: StmtIdSpan) bool {
        return self.len == 0;
    }
};

pub const RootExprId = enum(u32) {
    _,
};

pub const ValuePlanId = enum(u32) {
    _,
};

pub const ValuePlan = union(enum) {
    direct: CallableInstId,
    packed_fn: PackedFnId,
};

pub const CallPlan = union(enum) {
    direct: CallableInstId,
    indirect: IndirectCallId,
};

pub const LookupResolution = union(enum) {
    source_expr: Lambdasolved.ExprSource,
    def: Lambdasolved.ExternalDefSource,
};

pub const PlanMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() PlanMemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: PlanMemberSpan) bool {
        return self.len == 0;
    }
};

const PlanMemberSet = struct {
    members: PlanMemberSpan,
};

pub const PlanMemberSetId = enum(u32) {
    _,
};

pub const ContextExprKey = ContextMono.ContextExprKey;
pub const ContextPatternKey = ContextMono.ContextPatternKey;
pub const SourceContext = ContextMono.SourceContext;

pub const CaptureValueSource = union(enum) {
    lexical_pattern: struct {
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    source_expr: struct {
        source: Lambdasolved.ExprSource,
    },
};

pub const CaptureStorage = union(enum) {
    runtime_field: struct {
        field_monotype: ContextMono.ResolvedMonotype,
    },
    callable_only,
    recursive_member,
};

pub const CallableDefId = enum(u32) {
    _,
};

pub const CaptureField = struct {
    pattern_idx: CIR.Pattern.Idx,
    local_monotype: ContextMono.ResolvedMonotype,
    exact_callable_inst: ?CallableInstId = null,
    source: CaptureValueSource,
    storage: CaptureStorage,
};

pub const CaptureFieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureFieldSpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const CallableDef = struct {
    source_member: Lambdasolved.LambdaSetMemberId,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    fn_monotype: ContextMono.ResolvedMonotype,
    param_value_plan_entries: CallableParamValuePlanSpan = .empty(),
    captures: CaptureFieldSpan = .empty(),
    source_region: Region,
};

pub const ValuePlanSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ValuePlanSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ValuePlanSpan) bool {
        return self.len == 0;
    }
};

pub const CallableParamValuePlanEntry = struct {
    pattern_idx: CIR.Pattern.Idx,
    plan: ValuePlanId,
};

pub const CallableParamValuePlanSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamValuePlanSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamValuePlanSpan) bool {
        return self.len == 0;
    }
};

pub const ExprSemantics = struct {
    value_plan: ?ValuePlanId = null,
    call_plan: ?CallPlan = null,
    dispatch_target: ?ContextMono.DispatchExprTarget = null,
    lookup_resolution: ?LookupResolution = null,
};

pub const Expr = struct {
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    semantics: ExprSemantics = .{},
};

pub const Stmt = struct {
    module_idx: u32,
    source_stmt: CIR.Statement.Idx,
    child_exprs: ExprIdSpan = .empty(),
};

pub const RootExpr = struct {
    key: ContextExprKey,
    body_expr: ExprId,
};

pub const Program = struct {
    callable_insts: std.ArrayListUnmanaged(CallableInst),
    callable_param_spec_entries: std.ArrayListUnmanaged(CallableParamSpecEntry),
    callable_param_projection_entries: std.ArrayListUnmanaged(CallableParamProjection),
    plan_member_sets: std.ArrayListUnmanaged(PlanMemberSet),
    plan_member_set_entries: std.ArrayListUnmanaged(CallableInstId),
    singleton_plan_member_set_ids_by_callable_inst: std.AutoHashMapUnmanaged(CallableInstId, PlanMemberSetId),
    packed_fn_ids_by_member_set: std.AutoHashMapUnmanaged(PlanMemberSetId, PackedFnId),
    indirect_call_ids_by_member_set: std.AutoHashMapUnmanaged(PlanMemberSetId, IndirectCallId),
    value_plan_entries: std.ArrayListUnmanaged(ValuePlan),
    callable_param_value_plan_entries: std.ArrayListUnmanaged(CallableParamValuePlanEntry),
    exprs: std.ArrayListUnmanaged(Expr),
    expr_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, ExprId),
    expr_child_entries: std.ArrayListUnmanaged(ExprId),
    stmts: std.ArrayListUnmanaged(Stmt),
    stmt_child_entries: std.ArrayListUnmanaged(StmtId),
    root_exprs: std.ArrayListUnmanaged(RootExpr),
    root_expr_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, RootExprId),
    callable_defs: std.ArrayListUnmanaged(CallableDef),
    capture_fields: std.ArrayListUnmanaged(CaptureField),
    plan_member_entries: std.ArrayListUnmanaged(CallableInstId),
    packed_fns: std.ArrayListUnmanaged(PackedFn),
    indirect_calls: std.ArrayListUnmanaged(IndirectCall),

    pub fn init() Program {
        return .{
            .callable_insts = .empty,
            .callable_param_spec_entries = .empty,
            .callable_param_projection_entries = .empty,
            .plan_member_sets = .empty,
            .plan_member_set_entries = .empty,
            .singleton_plan_member_set_ids_by_callable_inst = .empty,
            .packed_fn_ids_by_member_set = .empty,
            .indirect_call_ids_by_member_set = .empty,
            .value_plan_entries = .empty,
            .callable_param_value_plan_entries = .empty,
            .exprs = .empty,
            .expr_ids_by_key = .empty,
            .expr_child_entries = .empty,
            .stmts = .empty,
            .stmt_child_entries = .empty,
            .root_exprs = .empty,
            .root_expr_ids_by_key = .empty,
            .callable_defs = .empty,
            .capture_fields = .empty,
            .plan_member_entries = .empty,
            .packed_fns = .empty,
            .indirect_calls = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.callable_param_projection_entries.deinit(allocator);
        self.plan_member_sets.deinit(allocator);
        self.plan_member_set_entries.deinit(allocator);
        self.singleton_plan_member_set_ids_by_callable_inst.deinit(allocator);
        self.packed_fn_ids_by_member_set.deinit(allocator);
        self.indirect_call_ids_by_member_set.deinit(allocator);
        self.value_plan_entries.deinit(allocator);
        self.callable_param_value_plan_entries.deinit(allocator);
        self.exprs.deinit(allocator);
        self.expr_ids_by_key.deinit(allocator);
        self.expr_child_entries.deinit(allocator);
        self.stmts.deinit(allocator);
        self.stmt_child_entries.deinit(allocator);
        self.root_exprs.deinit(allocator);
        self.root_expr_ids_by_key.deinit(allocator);
        self.callable_defs.deinit(allocator);
        self.capture_fields.deinit(allocator);
        self.plan_member_entries.deinit(allocator);
        self.packed_fns.deinit(allocator);
        self.indirect_calls.deinit(allocator);
    }

    pub fn getCallableInst(self: *const Program, callable_inst_id: CallableInstId) *const CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn getCallableParamSpecEntries(
        self: *const Program,
        span: CallableParamSpecSpan,
    ) []const CallableParamSpecEntry {
        if (span.len == 0) return &.{};
        return self.callable_param_spec_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Program,
        span: CallableParamProjectionSpan,
    ) []const CallableParamProjection {
        if (span.len == 0) return &.{};
        return self.callable_param_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getPlanMemberSetMembers(self: *const Program, member_set_id: PlanMemberSetId) []const CallableInstId {
        const member_set = self.plan_member_sets.items[@intFromEnum(member_set_id)];
        if (member_set.members.len == 0) return &.{};
        return self.plan_member_set_entries.items[member_set.members.start..][0..member_set.members.len];
    }

    pub fn getSingletonPlanMemberSetMembers(self: *const Program, callable_inst_id: CallableInstId) []const CallableInstId {
        const member_set_id = self.singleton_plan_member_set_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getPlanMemberSetMembers(member_set_id);
    }

    pub fn getValuePlan(self: *const Program, value_plan_id: ValuePlanId) ValuePlan {
        return self.value_plan_entries.items[@intFromEnum(value_plan_id)];
    }

    pub fn getValuePlanEntries(self: *const Program, span: ValuePlanSpan) []const ValuePlan {
        if (span.len == 0) return &.{};
        return self.value_plan_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableParamValuePlanEntries(
        self: *const Program,
        span: CallableParamValuePlanSpan,
    ) []const CallableParamValuePlanEntry {
        if (span.len == 0) return &.{};
        return self.callable_param_value_plan_entries.items[span.start..][0..span.len];
    }

    pub fn getExpr(self: *const Program, expr_id: ExprId) *const Expr {
        return &self.exprs.items[@intFromEnum(expr_id)];
    }

    pub fn getExprId(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprId {
        return self.expr_ids_by_key.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExprChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getExprChild(self: *const Program, expr_id: ExprId, index: usize) ExprId {
        return self.getExprChildren(self.getExpr(expr_id).child_exprs)[index];
    }

    pub fn getStmt(self: *const Program, stmt_id: StmtId) *const Stmt {
        return &self.stmts.items[@intFromEnum(stmt_id)];
    }

    pub fn getStmtChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getBlockStmtChildren(self: *const Program, span: StmtIdSpan) []const StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_child_entries.items[span.start..][0..span.len];
    }

    pub fn getRootExpr(self: *const Program, source_context: SourceContext, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ExprId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const root_id = self.root_expr_ids_by_key.get(key) orelse return self.getExprId(source_context, module_idx, expr_idx);
        return self.root_exprs.items[@intFromEnum(root_id)].body_expr;
    }

    pub fn valuePlanFromExprSemantics(self: *const Program, expr_id: ExprId) ?ValuePlan {
        const value_plan_id = self.getExpr(expr_id).semantics.value_plan orelse return null;
        return self.getValuePlan(value_plan_id);
    }

    pub fn callPlanFromExprSemantics(self: *const Program, expr_id: ExprId) ?CallPlan {
        return self.getExpr(expr_id).semantics.call_plan;
    }

    pub fn dispatchTargetFromExprSemantics(self: *const Program, expr_id: ExprId) ?ContextMono.DispatchExprTarget {
        return self.getExpr(expr_id).semantics.dispatch_target;
    }

    pub fn lookupResolutionFromExprSemantics(self: *const Program, expr_id: ExprId) ?LookupResolution {
        return self.getExpr(expr_id).semantics.lookup_resolution;
    }
};

pub fn getCallableDef(program: *const Program, callable_def_id: CallableDefId) *const CallableDef {
    return &program.callable_defs.items[@intFromEnum(callable_def_id)];
}

pub fn getCaptureFields(program: *const Program, span: CaptureFieldSpan) []const CaptureField {
    if (span.len == 0) return &.{};
    return program.capture_fields.items[span.start..][0..span.len];
}

pub fn getPackedFn(program: *const Program, packed_fn_id: PackedFnId) *const PackedFn {
    return &program.packed_fns.items[@intFromEnum(packed_fn_id)];
}

pub fn getIndirectCall(program: *const Program, indirect_call_id: IndirectCallId) *const IndirectCall {
    return &program.indirect_calls.items[@intFromEnum(indirect_call_id)];
}

pub fn getPlanMembers(program: *const Program, span: PlanMemberSpan) []const CallableInstId {
    if (span.len == 0) return &.{};
    return program.plan_member_entries.items[span.start..][0..span.len];
}
