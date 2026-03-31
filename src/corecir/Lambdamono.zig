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

pub const ExprValueSiteId = enum(u32) {
    _,
};

pub const LookupExprValueSiteId = enum(u32) {
    _,
};

pub const ContextPatternValueSiteId = enum(u32) {
    _,
};

pub const CallSiteId = enum(u32) {
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

pub const RuntimeCallableKind = enum {
    direct,
    closure,
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
    captures: CaptureFieldSpan = .empty(),
    callable_kind: RuntimeCallableKind,
    source_region: Region,
};

pub const ExprValueSite = struct {
    key: ContextExprKey,
    plan: ValuePlan,
};

pub const LookupExprValueSite = struct {
    key: ContextExprKey,
    plan: ValuePlan,
};

pub const ContextPatternValueSite = struct {
    key: ContextPatternKey,
    plan: ValuePlan,
};

pub const CallSite = struct {
    key: ContextExprKey,
    plan: CallPlan,
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
    expr_value_sites: std.ArrayListUnmanaged(ExprValueSite),
    expr_value_site_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, ExprValueSiteId),
    lookup_expr_value_sites: std.ArrayListUnmanaged(LookupExprValueSite),
    lookup_expr_value_site_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, LookupExprValueSiteId),
    context_pattern_value_sites: std.ArrayListUnmanaged(ContextPatternValueSite),
    context_pattern_value_site_ids_by_key: std.AutoHashMapUnmanaged(ContextPatternKey, ContextPatternValueSiteId),
    call_sites: std.ArrayListUnmanaged(CallSite),
    call_site_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, CallSiteId),
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
            .expr_value_sites = .empty,
            .expr_value_site_ids_by_key = .empty,
            .lookup_expr_value_sites = .empty,
            .lookup_expr_value_site_ids_by_key = .empty,
            .context_pattern_value_sites = .empty,
            .context_pattern_value_site_ids_by_key = .empty,
            .call_sites = .empty,
            .call_site_ids_by_key = .empty,
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
        self.expr_value_sites.deinit(allocator);
        self.expr_value_site_ids_by_key.deinit(allocator);
        self.lookup_expr_value_sites.deinit(allocator);
        self.lookup_expr_value_site_ids_by_key.deinit(allocator);
        self.context_pattern_value_sites.deinit(allocator);
        self.context_pattern_value_site_ids_by_key.deinit(allocator);
        self.call_sites.deinit(allocator);
        self.call_site_ids_by_key.deinit(allocator);
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

    pub fn getCallSiteCallableInst(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.call_site_ids_by_key.get(key) orelse return null;
        const plan = self.call_sites.items[@intFromEnum(site_id)].plan;
        return switch (plan) {
            .direct => |callable_inst_id| callable_inst_id,
            .indirect => null,
        };
    }

    pub fn getCallSitePlan(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallPlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.call_site_ids_by_key.get(key) orelse return null;
        return self.call_sites.items[@intFromEnum(site_id)].plan;
    }

    pub fn getExprCallableInst(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.expr_value_site_ids_by_key.get(key) orelse return null;
        const plan = self.expr_value_sites.items[@intFromEnum(site_id)].plan;
        return switch (plan) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_fn => null,
        };
    }

    pub fn getExprValuePlan(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ValuePlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.expr_value_site_ids_by_key.get(key) orelse return null;
        return self.expr_value_sites.items[@intFromEnum(site_id)].plan;
    }

    pub fn getLookupExprCallableInst(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.lookup_expr_value_site_ids_by_key.get(key) orelse return null;
        const plan = self.lookup_expr_value_sites.items[@intFromEnum(site_id)].plan;
        return switch (plan) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_fn => null,
        };
    }

    pub fn getLookupExprValuePlan(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ValuePlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const site_id = self.lookup_expr_value_site_ids_by_key.get(key) orelse return null;
        return self.lookup_expr_value_sites.items[@intFromEnum(site_id)].plan;
    }

    pub fn getContextPatternCallableInst(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const site_id = self.context_pattern_value_site_ids_by_key.get(key) orelse return null;
        const plan = self.context_pattern_value_sites.items[@intFromEnum(site_id)].plan;
        return switch (plan) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_fn => null,
        };
    }

    pub fn getContextPatternValuePlan(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ValuePlan {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const site_id = self.context_pattern_value_site_ids_by_key.get(key) orelse return null;
        return self.context_pattern_value_sites.items[@intFromEnum(site_id)].plan;
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
