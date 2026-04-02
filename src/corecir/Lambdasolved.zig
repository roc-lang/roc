//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const can = @import("can");
const cm = @import("ContextMono.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");

const CIR = can.CIR;
pub const ExprTemplateSource = TemplateCatalog.ExprTemplateSource;
pub const LocalPatternTemplateSource = TemplateCatalog.LocalPatternTemplateSource;
pub const CallableTemplateId = TemplateCatalog.CallableTemplateId;

/// One monomorphic callable specialization referenced by solved lambda sets.
pub const CallableInstId = enum(u32) {
    _,
};

pub const CallableTemplateKind = TemplateCatalog.CallableTemplateKind;
pub const ExternalDefSource = TemplateCatalog.ExternalDefSource;
pub const CallableTemplateSource = TemplateCatalog.CallableTemplateSource;
pub const CallableTemplateBinding = TemplateCatalog.CallableTemplateBinding;
pub const CallableTemplateOwner = TemplateCatalog.CallableTemplateOwner;
pub const CallableTemplate = TemplateCatalog.CallableTemplate;

pub const SourceContext = cm.SourceContext;
pub const ContextExprKey = cm.ContextExprKey;
pub const ContextPatternKey = cm.ContextPatternKey;

pub const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

pub const ValueDefResolutionState = struct {
    in_progress_exprs: std.AutoHashMapUnmanaged(ContextExprKey, void),

    pub fn init() ValueDefResolutionState {
        return .{ .in_progress_exprs = .empty };
    }

    pub fn deinit(self: *ValueDefResolutionState, allocator: std.mem.Allocator) void {
        self.in_progress_exprs.deinit(allocator);
    }

    pub fn clear(self: *ValueDefResolutionState) void {
        self.in_progress_exprs.clearRetainingCapacity();
    }

    pub fn beginExpr(
        self: *ValueDefResolutionState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        if (self.in_progress_exprs.contains(key)) return false;
        try self.in_progress_exprs.put(allocator, key, {});
        return true;
    }

    pub fn endExpr(self: *ValueDefResolutionState, key: ContextExprKey) void {
        _ = self.in_progress_exprs.remove(key);
    }
};

pub const CallResultResolutionState = struct {
    in_progress_calls: std.AutoHashMapUnmanaged(CallResultCallableInstKey, void),

    pub fn init() CallResultResolutionState {
        return .{ .in_progress_calls = .empty };
    }

    pub fn deinit(self: *CallResultResolutionState, allocator: std.mem.Allocator) void {
        self.in_progress_calls.deinit(allocator);
    }

    pub fn clear(self: *CallResultResolutionState) void {
        self.in_progress_calls.clearRetainingCapacity();
    }

    pub fn beginCall(
        self: *CallResultResolutionState,
        allocator: std.mem.Allocator,
        key: CallResultCallableInstKey,
    ) std.mem.Allocator.Error!bool {
        if (self.in_progress_calls.contains(key)) return false;
        try self.in_progress_calls.put(allocator, key, {});
        return true;
    }

    pub fn endCall(self: *CallResultResolutionState, key: CallResultCallableInstKey) void {
        _ = self.in_progress_calls.remove(key);
    }
};

pub const RootAnalysisState = struct {
    expr_traversal: cm.ExprTraversalState,
    value_def_resolution: ValueDefResolutionState,
    call_result_resolution: CallResultResolutionState,

    pub fn init() RootAnalysisState {
        return .{
            .expr_traversal = cm.ExprTraversalState.init(),
            .value_def_resolution = ValueDefResolutionState.init(),
            .call_result_resolution = CallResultResolutionState.init(),
        };
    }

    pub fn deinit(self: *RootAnalysisState, allocator: std.mem.Allocator) void {
        self.expr_traversal.deinit(allocator);
        self.value_def_resolution.deinit(allocator);
        self.call_result_resolution.deinit(allocator);
    }

    pub fn clearAll(self: *RootAnalysisState) void {
        self.expr_traversal.clearAll();
        self.value_def_resolution.clear();
        self.call_result_resolution.clear();
    }

    pub fn clearPerScan(self: *RootAnalysisState) void {
        self.expr_traversal.clearPerScan();
        self.value_def_resolution.clear();
        self.call_result_resolution.clear();
    }

    pub fn hasVisitedExpr(self: *const RootAnalysisState, key: ContextExprKey) bool {
        return self.expr_traversal.hasVisited(key);
    }

    pub fn beginExprVisit(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        return self.expr_traversal.beginVisit(allocator, key);
    }

    pub fn beginValueDefExpr(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        return self.value_def_resolution.beginExpr(allocator, key);
    }

    pub fn endValueDefExpr(self: *RootAnalysisState, key: ContextExprKey) void {
        self.value_def_resolution.endExpr(key);
    }

    pub fn beginCallResult(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: CallResultCallableInstKey,
    ) std.mem.Allocator.Error!bool {
        return self.call_result_resolution.beginCall(allocator, key);
    }

    pub fn endCallResult(self: *RootAnalysisState, key: CallResultCallableInstKey) void {
        self.call_result_resolution.endCall(key);
    }
};

pub const Result = struct {
    pub fn init(allocator: std.mem.Allocator) !Result {
        _ = allocator;
        return .{};
    }

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};
