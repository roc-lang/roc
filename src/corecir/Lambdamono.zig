//! Specialization from solved lambda sets to executable structured callable IR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const CoreCIR = @import("CoreCIR.zig");
const ContextMono = @import("ContextMono.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

/// Identifies one executable callable specialization chosen from solved
/// lambda-set semantics.
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

pub const LambdaSetId = enum(u32) {
    _,

    pub const none: LambdaSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: LambdaSetId) bool {
        return self == none;
    }
};

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    lambda_set_id: LambdaSetId,
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

pub const CallablePlan = union(enum) {
    exact: CallableInstId,
    lambda_set: LambdaSetId,
};

pub const LambdaSetSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() LambdaSetSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: LambdaSetSpan) bool {
        return self.len == 0;
    }
};

pub const LambdaSet = struct {
    members: LambdaSetSpan,
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

    pub const none: CallableDefId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableDefId) bool {
        return self == none;
    }
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

pub const Program = struct {
    callable_defs: std.ArrayListUnmanaged(CallableDef),
    capture_fields: std.ArrayListUnmanaged(CaptureField),

    pub fn init() Program {
        return .{
            .callable_defs = .empty,
            .capture_fields = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_defs.deinit(allocator);
        self.capture_fields.deinit(allocator);
    }
};

pub const Result = struct {
    callable_insts: std.ArrayListUnmanaged(CallableInst),
    callable_param_spec_entries: std.ArrayListUnmanaged(CallableParamSpecEntry),
    callable_param_projection_entries: std.ArrayListUnmanaged(CallableParamProjection),
    lambda_set_entries: std.ArrayListUnmanaged(CallableInstId),
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    expr_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    call_site_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    call_site_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    lookup_expr_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    lookup_expr_lambda_sets: std.AutoHashMapUnmanaged(ContextExprKey, LambdaSetId),
    context_pattern_callable_insts: std.AutoHashMapUnmanaged(ContextPatternKey, CallableInstId),
    context_pattern_lambda_sets: std.AutoHashMapUnmanaged(ContextPatternKey, LambdaSetId),

    pub fn init() Result {
        return .{
            .callable_insts = .empty,
            .callable_param_spec_entries = .empty,
            .callable_param_projection_entries = .empty,
            .lambda_set_entries = .empty,
            .lambda_sets = .empty,
            .expr_callable_insts = .empty,
            .expr_lambda_sets = .empty,
            .call_site_callable_insts = .empty,
            .call_site_lambda_sets = .empty,
            .lookup_expr_callable_insts = .empty,
            .lookup_expr_lambda_sets = .empty,
            .context_pattern_callable_insts = .empty,
            .context_pattern_lambda_sets = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.callable_param_projection_entries.deinit(allocator);
        self.lambda_set_entries.deinit(allocator);
        self.lambda_sets.deinit(allocator);
        self.expr_callable_insts.deinit(allocator);
        self.expr_lambda_sets.deinit(allocator);
        self.call_site_callable_insts.deinit(allocator);
        self.call_site_lambda_sets.deinit(allocator);
        self.lookup_expr_callable_insts.deinit(allocator);
        self.lookup_expr_lambda_sets.deinit(allocator);
        self.context_pattern_callable_insts.deinit(allocator);
        self.context_pattern_lambda_sets.deinit(allocator);
    }

    pub fn getLambdaSet(self: *const Result, lambda_set_id: LambdaSetId) *const LambdaSet {
        return &self.lambda_sets.items[@intFromEnum(lambda_set_id)];
    }

    pub fn getLambdaSetMembers(self: *const Result, span: LambdaSetSpan) []const CallableInstId {
        if (span.len == 0) return &.{};
        return self.lambda_set_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableInst(self: *const Result, callable_inst_id: CallableInstId) *const CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn getCallableParamSpecEntries(
        self: *const Result,
        span: CallableParamSpecSpan,
    ) []const CallableParamSpecEntry {
        if (span.len == 0) return &.{};
        return self.callable_param_spec_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Result,
        span: CallableParamProjectionSpan,
    ) []const CallableParamProjection {
        if (span.len == 0) return &.{};
        return self.callable_param_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getCallSiteCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        return self.call_site_callable_insts.get(key);
    }

    pub fn getCallSiteCallablePlan(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallablePlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        if (self.call_site_callable_insts.get(key)) |callable_inst_id| return .{ .exact = callable_inst_id };
        if (self.call_site_lambda_sets.get(key)) |lambda_set_id| return .{ .lambda_set = lambda_set_id };
        return null;
    }

    pub fn getCallSiteLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.call_site_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getExprCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        return self.expr_callable_insts.get(key);
    }

    pub fn getExprCallablePlan(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallablePlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        if (self.expr_callable_insts.get(key)) |callable_inst_id| return .{ .exact = callable_inst_id };
        if (self.expr_lambda_sets.get(key)) |lambda_set_id| return .{ .lambda_set = lambda_set_id };
        return null;
    }

    pub fn getExprLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.expr_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getLookupExprCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        return self.lookup_expr_callable_insts.get(key);
    }

    pub fn getLookupExprCallablePlan(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallablePlan {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        if (self.lookup_expr_callable_insts.get(key)) |callable_inst_id| return .{ .exact = callable_inst_id };
        if (self.lookup_expr_lambda_sets.get(key)) |lambda_set_id| return .{ .lambda_set = lambda_set_id };
        return null;
    }

    pub fn getLookupExprLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const set_id = self.lookup_expr_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }

    pub fn getContextPatternCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        return self.context_pattern_callable_insts.get(key);
    }

    pub fn getContextPatternCallablePlan(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallablePlan {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        if (self.context_pattern_callable_insts.get(key)) |callable_inst_id| return .{ .exact = callable_inst_id };
        if (self.context_pattern_lambda_sets.get(key)) |lambda_set_id| return .{ .lambda_set = lambda_set_id };
        return null;
    }

    pub fn getContextPatternLambdaSetMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const set_id = self.context_pattern_lambda_sets.get(key) orelse return null;
        return self.getLambdaSetMembers(self.getLambdaSet(set_id).members);
    }
};

pub const StageResult = struct {
    corecir: CoreCIR.Program,
    context_mono: ContextMono.Result,
    lambdasolved: Lambdasolved.Result,
    lambdamono: Result,
    program: Program,

    pub fn deinit(self: *StageResult, allocator: Allocator) void {
        self.program.deinit(allocator);
        self.lambdamono.deinit(allocator);
        self.lambdasolved.deinit(allocator);
        self.context_mono.deinit(allocator);
        self.corecir.deinit(allocator);
    }
};

pub fn getCallableDef(program: *const Program, callable_def_id: CallableDefId) *const CallableDef {
    return &program.callable_defs.items[@intFromEnum(callable_def_id)];
}

pub fn getCaptureFields(program: *const Program, span: CaptureFieldSpan) []const CaptureField {
    if (span.len == 0) return &.{};
    return program.capture_fields.items[span.start..][0..span.len];
}
