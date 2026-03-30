//! Specialization from solved lambda sets to executable structured callable IR.

const std = @import("std");
const can = @import("can");
const CoreCIR = @import("CoreCIR.zig");
const ContextMono = @import("ContextMono.zig");
const LambdaSolved = @import("LambdaSolved.zig");
const SpecializedCIR = @import("SpecializedCIR.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;

/// Identifies one executable callable specialization chosen from solved
/// lambda-set semantics.
pub const CallableInstId = enum(u32) {
    _,

    pub const none: CallableInstId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableInstId) bool {
        return self == none;
    }
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

pub const CallableInstSetId = enum(u32) {
    _,

    pub const none: CallableInstSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: CallableInstSetId) bool {
        return self == none;
    }
};

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    callable_inst_set_id: CallableInstSetId,
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
    template: LambdaSolved.CallableTemplateId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_context_callable_inst: CallableInstId,
    callable_param_specs: CallableParamSpecSpan = .empty(),
};

pub const CallableInstSetSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableInstSetSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableInstSetSpan) bool {
        return self.len == 0;
    }
};

pub const CallableInstSet = struct {
    members: CallableInstSetSpan,
};

pub const ContextExprKey = ContextMono.ContextExprKey;
pub const ContextPatternKey = ContextMono.ContextPatternKey;

pub const ContextCaptureKey = struct {
    closure_callable_inst_raw: u32,
    module_idx: u32,
    closure_expr_raw: u32,
    pattern_raw: u32,
};

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const Result = struct {
    callable_insts: std.ArrayListUnmanaged(CallableInst),
    callable_param_spec_entries: std.ArrayListUnmanaged(CallableParamSpecEntry),
    callable_param_projection_entries: std.ArrayListUnmanaged(CallableParamProjection),
    callable_inst_set_entries: std.ArrayListUnmanaged(CallableInstId),
    callable_inst_sets: std.ArrayListUnmanaged(CallableInstSet),
    expr_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    expr_callable_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstSetId),
    call_site_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    call_site_callable_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstSetId),
    dispatch_expr_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    dispatch_expr_targets: std.AutoHashMapUnmanaged(ContextExprKey, DispatchExprTarget),
    lookup_expr_callable_insts: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstId),
    lookup_expr_callable_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, CallableInstSetId),
    closure_capture_monotypes: std.AutoHashMapUnmanaged(ContextCaptureKey, ContextMono.ResolvedMonotype),
    closure_capture_callable_insts: std.AutoHashMapUnmanaged(ContextCaptureKey, CallableInstId),
    context_pattern_callable_insts: std.AutoHashMapUnmanaged(ContextPatternKey, CallableInstId),
    context_pattern_callable_inst_sets: std.AutoHashMapUnmanaged(ContextPatternKey, CallableInstSetId),

    pub fn init() Result {
        return .{
            .callable_insts = .empty,
            .callable_param_spec_entries = .empty,
            .callable_param_projection_entries = .empty,
            .callable_inst_set_entries = .empty,
            .callable_inst_sets = .empty,
            .expr_callable_insts = .empty,
            .expr_callable_inst_sets = .empty,
            .call_site_callable_insts = .empty,
            .call_site_callable_inst_sets = .empty,
            .dispatch_expr_callable_insts = .empty,
            .dispatch_expr_targets = .empty,
            .lookup_expr_callable_insts = .empty,
            .lookup_expr_callable_inst_sets = .empty,
            .closure_capture_monotypes = .empty,
            .closure_capture_callable_insts = .empty,
            .context_pattern_callable_insts = .empty,
            .context_pattern_callable_inst_sets = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.callable_param_projection_entries.deinit(allocator);
        self.callable_inst_set_entries.deinit(allocator);
        self.callable_inst_sets.deinit(allocator);
        self.expr_callable_insts.deinit(allocator);
        self.expr_callable_inst_sets.deinit(allocator);
        self.call_site_callable_insts.deinit(allocator);
        self.call_site_callable_inst_sets.deinit(allocator);
        self.dispatch_expr_callable_insts.deinit(allocator);
        self.dispatch_expr_targets.deinit(allocator);
        self.lookup_expr_callable_insts.deinit(allocator);
        self.lookup_expr_callable_inst_sets.deinit(allocator);
        self.closure_capture_monotypes.deinit(allocator);
        self.closure_capture_callable_insts.deinit(allocator);
        self.context_pattern_callable_insts.deinit(allocator);
        self.context_pattern_callable_inst_sets.deinit(allocator);
    }

    pub fn getCallableInstSet(self: *const Result, callable_inst_set_id: CallableInstSetId) *const CallableInstSet {
        return &self.callable_inst_sets.items[@intFromEnum(callable_inst_set_id)];
    }

    pub fn getCallableInstSetMembers(self: *const Result, span: CallableInstSetSpan) []const CallableInstId {
        if (span.len == 0) return &.{};
        return self.callable_inst_set_entries.items[span.start..][0..span.len];
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

    pub fn contextCaptureKey(
        closure_callable_inst: CallableInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextCaptureKey {
        return .{
            .closure_callable_inst_raw = @intFromEnum(closure_callable_inst),
            .module_idx = module_idx,
            .closure_expr_raw = @intFromEnum(closure_expr_idx),
            .pattern_raw = @intFromEnum(pattern_idx),
        };
    }

    pub fn getCallSiteCallableInst(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const callable_inst_id = self.call_site_callable_insts.get(key) orelse return null;
        if (callable_inst_id.isNone()) return null;
        return callable_inst_id;
    }

    pub fn getCallSiteCallableInsts(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const set_id = self.call_site_callable_inst_sets.get(key) orelse return null;
        return self.getCallableInstSetMembers(self.getCallableInstSet(set_id).members);
    }

    pub fn getExprCallableInst(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const callable_inst_id = self.expr_callable_insts.get(key) orelse return null;
        if (callable_inst_id.isNone()) return null;
        return callable_inst_id;
    }

    pub fn getExprCallableInsts(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const set_id = self.expr_callable_inst_sets.get(key) orelse return null;
        return self.getCallableInstSetMembers(self.getCallableInstSet(set_id).members);
    }

    pub fn getDispatchExprCallableInst(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        return self.dispatch_expr_callable_insts.get(key);
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        return self.dispatch_expr_targets.get(key);
    }

    pub fn getLookupExprCallableInst(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const callable_inst_id = self.lookup_expr_callable_insts.get(key) orelse return null;
        if (callable_inst_id.isNone()) return null;
        return callable_inst_id;
    }

    pub fn getLookupExprCallableInsts(
        self: *const Result,
        context_callable_inst: CallableInstId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextExprKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            root_source_expr_context,
            module_idx,
            expr_idx,
        );
        const set_id = self.lookup_expr_callable_inst_sets.get(key) orelse return null;
        return self.getCallableInstSetMembers(self.getCallableInstSet(set_id).members);
    }

    pub fn getClosureCaptureCallableInst(
        self: *const Result,
        closure_callable_inst: CallableInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        return self.closure_capture_callable_insts.get(contextCaptureKey(
            closure_callable_inst,
            module_idx,
            closure_expr_idx,
            pattern_idx,
        ));
    }

    pub fn getClosureCaptureMonotype(
        self: *const Result,
        closure_callable_inst: CallableInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ContextMono.ResolvedMonotype {
        return self.closure_capture_monotypes.get(contextCaptureKey(
            closure_callable_inst,
            module_idx,
            closure_expr_idx,
            pattern_idx,
        ));
    }

    pub fn getContextPatternCallableInst(
        self: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const key = ContextMono.Result.contextPatternKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            module_idx,
            pattern_idx,
        );
        const callable_inst_id = self.context_pattern_callable_insts.get(key) orelse return null;
        if (callable_inst_id.isNone()) return null;
        return callable_inst_id;
    }

    pub fn getContextPatternCallableInsts(
        self: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const CallableInstId {
        const key = ContextMono.Result.contextPatternKey(
            @enumFromInt(@intFromEnum(context_callable_inst)),
            module_idx,
            pattern_idx,
        );
        const set_id = self.context_pattern_callable_inst_sets.get(key) orelse return null;
        return self.getCallableInstSetMembers(self.getCallableInstSet(set_id).members);
    }
};

pub const StageResult = struct {
    corecir: CoreCIR.Program,
    context_mono: ContextMono.Result,
    lambda_solved: LambdaSolved.Result,
    lambda_specialize: Result,
    specialized: SpecializedCIR.Program,

    pub fn deinit(self: *StageResult, allocator: Allocator) void {
        self.specialized.deinit(allocator);
        self.lambda_specialize.deinit(allocator);
        self.lambda_solved.deinit(allocator);
        self.context_mono.deinit(allocator);
        self.corecir.deinit(allocator);
    }
};
