//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const cm = @import("ContextMono.zig");
const ValueProjection = @import("ValueProjection.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

pub const CallableSourceNamespace = enum(u2) {
    local_pattern = 0,
    external_def = 1,
    expr = 2,
};

pub fn packCallableSourceKey(namespace: CallableSourceNamespace, module_idx: u32, local_id: u32) u64 {
    if (std.debug.runtime_safety) {
        std.debug.assert(module_idx <= std.math.maxInt(u31));
        std.debug.assert(local_id <= std.math.maxInt(u31));
    }

    return (@as(u64, @intFromEnum(namespace)) << 62) |
        (@as(u64, module_idx) << 31) |
        @as(u64, local_id);
}

pub fn packLocalPatternSourceKey(module_idx: u32, pattern_idx: CIR.Pattern.Idx) u64 {
    return packCallableSourceKey(.local_pattern, module_idx, @intFromEnum(pattern_idx));
}

pub fn packExternalDefSourceKey(module_idx: u32, def_node_idx: u16) u64 {
    return packCallableSourceKey(.external_def, module_idx, def_node_idx);
}

pub fn packExprSourceKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
    return packCallableSourceKey(.expr, module_idx, @intFromEnum(expr_idx));
}

pub const CallableTemplateId = enum(u32) {
    _,
};

/// One monomorphic callable specialization referenced by solved lambda sets.
pub const CallableInstId = enum(u32) {
    _,
};

pub const CallableTemplateKind = enum {
    top_level_def,
    lambda,
    closure,
    hosted_lambda,
};

pub const ExternalDefSource = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const CallableTemplate = struct {
    source_key: u64,
    module_idx: u32,
    cir_expr: CIR.Expr.Idx,
    runtime_expr: CIR.Expr.Idx,
    type_root: types.Var,
    binding_pattern: ?CIR.Pattern.Idx = null,
    kind: CallableTemplateKind = .top_level_def,
    lexical_owner_template: ?CallableTemplateId = null,
    external_def: ?ExternalDefSource = null,
    source_region: Region = Region.zero(),
};

pub const ExprSource = struct {
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: ValueProjection.ProjectionSpan = .empty(),
};

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const LambdaSetMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LambdaSetMemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LambdaSetMemberSpan) bool {
        return self.len == 0;
    }
};

pub const LambdaSet = struct {
    members: LambdaSetMemberSpan,
};

pub const LambdaSetId = enum(u32) {
    _,
};

/// Solved callable semantics for a function-valued source expression or pattern.
pub const SolvedCallableValue = union(enum) {
    direct: CallableInstId,
    lambda_set: LambdaSetId,
};

/// Solved call semantics for a source call site before executable lowering.
pub const SolvedCall = union(enum) {
    direct: CallableInstId,
    lambda_set: LambdaSetId,
};

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    value_projection_entries: std.ArrayListUnmanaged(ValueProjection.Projection),
    expr_source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    pattern_source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    external_def_source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    local_callable_template_ids: std.AutoHashMapUnmanaged(u64, CallableTemplateId),
    external_callable_template_ids: std.AutoHashMapUnmanaged(u64, CallableTemplateId),
    expr_callable_template_ids: std.AutoHashMapUnmanaged(u64, CallableTemplateId),
    lambda_set_member_entries: std.ArrayListUnmanaged(CallableInstId),
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    context_expr_callable_values: std.AutoHashMapUnmanaged(ContextExprKey, SolvedCallableValue),
    context_expr_calls: std.AutoHashMapUnmanaged(ContextExprKey, SolvedCall),
    context_pattern_callable_values: std.AutoHashMapUnmanaged(ContextPatternKey, SolvedCallableValue),

    pub fn init(allocator: Allocator) !Result {
        _ = allocator;
        return .{
            .callable_templates = .empty,
            .value_projection_entries = .empty,
            .expr_source_exprs = .empty,
            .pattern_source_exprs = .empty,
            .external_def_source_exprs = .empty,
            .local_callable_template_ids = .empty,
            .external_callable_template_ids = .empty,
            .expr_callable_template_ids = .empty,
            .lambda_set_member_entries = .empty,
            .lambda_sets = .empty,
            .context_expr_callable_values = .empty,
            .context_expr_calls = .empty,
            .context_pattern_callable_values = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.value_projection_entries.deinit(allocator);
        self.expr_source_exprs.deinit(allocator);
        self.pattern_source_exprs.deinit(allocator);
        self.external_def_source_exprs.deinit(allocator);
        self.local_callable_template_ids.deinit(allocator);
        self.external_callable_template_ids.deinit(allocator);
        self.expr_callable_template_ids.deinit(allocator);
        self.lambda_set_member_entries.deinit(allocator);
        self.lambda_sets.deinit(allocator);
        self.context_expr_callable_values.deinit(allocator);
        self.context_expr_calls.deinit(allocator);
        self.context_pattern_callable_values.deinit(allocator);
    }

    pub fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return &self.callable_templates.items[@intFromEnum(callable_template_id)];
    }

    pub fn getValueProjectionEntries(
        self: *const Result,
        span: ValueProjection.ProjectionSpan,
    ) []const ValueProjection.Projection {
        if (span.len == 0) return &.{};
        return self.value_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getLocalCallableTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?CallableTemplateId {
        return self.local_callable_template_ids.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.external_callable_template_ids.get(packExternalDefSourceKey(module_idx, def_node_idx));
    }

    pub fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.expr_callable_template_ids.get(packExprSourceKey(module_idx, expr_idx));
    }

    pub fn getPatternSourceExpr(
        self: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprSource {
        return self.pattern_source_exprs.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getExprSourceExpr(
        self: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprSource {
        return self.expr_source_exprs.get(packExprSourceKey(module_idx, expr_idx));
    }

    pub fn getExternalDefSourceExpr(
        self: *const Result,
        module_idx: u32,
        def_node_idx: u16,
    ) ?ExprSource {
        return self.external_def_source_exprs.get(packExternalDefSourceKey(module_idx, def_node_idx));
    }

    pub fn getLambdaSet(self: *const Result, lambda_set_id: LambdaSetId) *const LambdaSet {
        return &self.lambda_sets.items[@intFromEnum(lambda_set_id)];
    }

    pub fn getLambdaSetMembers(self: *const Result, lambda_set_id: LambdaSetId) []const CallableInstId {
        const lambda_set = self.getLambdaSet(lambda_set_id);
        if (lambda_set.members.len == 0) return &.{};
        return self.lambda_set_member_entries.items[lambda_set.members.start..][0..lambda_set.members.len];
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?SolvedCallableValue {
        return self.context_expr_callable_values.get(cm.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExprCall(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?SolvedCall {
        return self.context_expr_calls.get(cm.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getPatternCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?SolvedCallableValue {
        return self.context_pattern_callable_values.get(cm.Result.contextPatternKey(source_context, module_idx, pattern_idx));
    }

};
