//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const cm = @import("ContextMono.zig");

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

    pub const none: CallableTemplateId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableTemplateId) bool {
        return self == none;
    }
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
    type_root: types.Var,
    binding_pattern: ?CIR.Pattern.Idx = null,
    kind: CallableTemplateKind = .top_level_def,
    lexical_owner_template: ?CallableTemplateId = null,
    external_def: ?ExternalDefSource = null,
    source_region: Region = Region.zero(),
};

pub const DeferredLocalCallable = struct {
    pattern_idx: CIR.Pattern.Idx,
    cir_expr: CIR.Expr.Idx,
    module_idx: u32,
    source_key: u64,
    type_root: types.Var,
};

pub const ExprSource = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

pub const CapturePlanId = enum(u32) {
    _,

    pub const none: CapturePlanId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CapturePlanId) bool {
        return self == none;
    }
};

pub const LambdaSetMemberId = enum(u32) {
    _,

    pub const none: LambdaSetMemberId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LambdaSetMemberId) bool {
        return self == none;
    }
};

pub const CaptureSource = union(enum) {
    lexical_pattern: struct {
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    parent_capture: struct {
        capture_plan: CapturePlanId,
        capture_index: u16,
    },
    top_level_def: struct {
        module_idx: u32,
        def_idx: CIR.Def.Idx,
    },
    source_expr: struct {
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    },
    exact_callable: struct {
        member: LambdaSetMemberId,
    },
};

pub const CaptureEntry = struct {
    source: CaptureSource,
    monotype: cm.ResolvedMonotype,
};

pub const CaptureEntrySpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureEntrySpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const CapturePlan = struct {
    entries: CaptureEntrySpan,
};

pub const SolvedCallableKind = enum {
    direct,
    closure,
    hosted,
};

pub const LambdaSetMember = struct {
    template: CallableTemplateId,
    context_id: cm.ContextId,
    fn_monotype: cm.ResolvedMonotype,
    capture_plan: CapturePlanId = .none,
    kind: SolvedCallableKind,
};

pub const LambdaSetId = enum(u32) {
    _,

    pub const none: LambdaSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LambdaSetId) bool {
        return self == none;
    }
};

pub const LambdaSetMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LambdaSetMemberSpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const LambdaSet = struct {
    members: LambdaSetMemberSpan,
};

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const Result = struct {
    callable_templates: std.ArrayListUnmanaged(CallableTemplate),
    source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    callable_template_ids_by_source: std.AutoHashMapUnmanaged(u64, CallableTemplateId),
    deferred_local_callables: std.AutoHashMapUnmanaged(u64, DeferredLocalCallable),
    capture_entries: std.ArrayListUnmanaged(CaptureEntry),
    capture_plans: std.ArrayListUnmanaged(CapturePlan),
    lambda_set_members: std.ArrayListUnmanaged(LambdaSetMember),
    lambda_set_member_entries: std.ArrayListUnmanaged(LambdaSetMemberId),
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    call_site_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    lookup_expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    context_pattern_lambda_sets: std.AutoHashMapUnmanaged(ContextPatternKey, LambdaSetId),

    pub fn init() Result {
        return .{
            .callable_templates = .empty,
            .source_exprs = .empty,
            .callable_template_ids_by_source = .empty,
            .deferred_local_callables = .empty,
            .capture_entries = .empty,
            .capture_plans = .empty,
            .lambda_set_members = .empty,
            .lambda_set_member_entries = .empty,
            .lambda_sets = .empty,
            .expr_lambda_sets = .empty,
            .call_site_lambda_sets = .empty,
            .lookup_expr_lambda_sets = .empty,
            .context_pattern_lambda_sets = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_templates.deinit(allocator);
        self.source_exprs.deinit(allocator);
        self.callable_template_ids_by_source.deinit(allocator);
        self.deferred_local_callables.deinit(allocator);
        self.capture_entries.deinit(allocator);
        self.capture_plans.deinit(allocator);
        self.lambda_set_members.deinit(allocator);
        self.lambda_set_member_entries.deinit(allocator);
        self.lambda_sets.deinit(allocator);
        self.expr_lambda_sets.deinit(allocator);
        self.call_site_lambda_sets.deinit(allocator);
        self.lookup_expr_lambda_sets.deinit(allocator);
        self.context_pattern_lambda_sets.deinit(allocator);
    }

    pub fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return &self.callable_templates.items[@intFromEnum(callable_template_id)];
    }

    pub fn getLocalCallableTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packExternalDefSourceKey(module_idx, def_node_idx));
    }

    pub fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.callable_template_ids_by_source.get(packExprSourceKey(module_idx, expr_idx));
    }

    pub fn getDeferredLocalCallable(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?DeferredLocalCallable {
        return self.deferred_local_callables.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getPatternSourceExpr(
        self: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprSource {
        return self.source_exprs.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getLambdaSet(self: *const Result, lambda_set_id: LambdaSetId) *const LambdaSet {
        return &self.lambda_sets.items[@intFromEnum(lambda_set_id)];
    }

    pub fn getLambdaSetMember(self: *const Result, member_id: LambdaSetMemberId) *const LambdaSetMember {
        return &self.lambda_set_members.items[@intFromEnum(member_id)];
    }

    pub fn getLambdaSetMembers(self: *const Result, span: LambdaSetMemberSpan) []const LambdaSetMemberId {
        if (span.len == 0) return &.{};
        return self.lambda_set_member_entries.items[span.start..][0..span.len];
    }

    pub fn getExprLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const LambdaSetMemberId {
        const key = cm.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.expr_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getCallSiteLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const LambdaSetMemberId {
        const key = cm.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.call_site_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getLookupExprLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const LambdaSetMemberId {
        const key = cm.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.lookup_expr_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getContextPatternLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const LambdaSetMemberId {
        const key = cm.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const set_id = self.context_pattern_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }
};
