//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const can = @import("can");
const cm = @import("ContextMono.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");

const CIR = can.CIR;
const RootExprContext = cm.RootExprContext;
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

pub const SemanticThread = struct {
    source_context: SourceContext,

    pub fn trackedThread(source_context: SourceContext) SemanticThread {
        return .{
            .source_context = source_context,
        };
    }

    pub fn requireSourceContext(self: SemanticThread) SourceContext {
        return self.source_context;
    }

    pub fn hasCallableInst(self: SemanticThread) bool {
        return switch (self.source_context) {
            .callable_inst => true,
            .root_expr, .provenance_expr, .template_expr => false,
        };
    }

    pub fn callableInst(self: SemanticThread) ?CallableInstId {
        return switch (self.source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => null,
        };
    }

    pub fn requireCallableInst(self: SemanticThread) CallableInstId {
        return self.callableInst() orelse blk: {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: source context {s} had no callable inst",
                    .{@tagName(self.source_context)},
                );
            }
            break :blk unreachable;
        };
    }

    pub fn callableInstRawForDebug(self: SemanticThread) u32 {
        return switch (self.source_context) {
            .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }

    pub fn rootExprRawForDebug(self: SemanticThread) u32 {
        return switch (self.source_context) {
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .callable_inst, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }
};

pub const ExprTraversalState = struct {
    visited_exprs: std.AutoHashMapUnmanaged(ContextExprKey, void),

    pub fn init() ExprTraversalState {
        return .{ .visited_exprs = .empty };
    }

    pub fn deinit(self: *ExprTraversalState, allocator: std.mem.Allocator) void {
        self.visited_exprs.deinit(allocator);
    }

    pub fn clearAll(self: *ExprTraversalState) void {
        self.visited_exprs.clearRetainingCapacity();
    }

    pub fn clearPerScan(self: *ExprTraversalState) void {
        self.visited_exprs.clearRetainingCapacity();
    }

    pub fn hasVisited(self: *const ExprTraversalState, key: ContextExprKey) bool {
        return self.visited_exprs.contains(key);
    }

    pub fn beginVisit(
        self: *ExprTraversalState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        if (self.visited_exprs.contains(key)) return false;
        try self.visited_exprs.put(allocator, key, {});
        return true;
    }
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
    expr_traversal: ExprTraversalState,
    value_def_resolution: ValueDefResolutionState,
    call_result_resolution: CallResultResolutionState,

    pub fn init() RootAnalysisState {
        return .{
            .expr_traversal = ExprTraversalState.init(),
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

    pub fn exprVisitStillPending(self: *const RootAnalysisState, key: ContextExprKey) bool {
        return !self.hasVisitedExpr(key);
    }

    pub fn beginExprVisit(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        return self.expr_traversal.beginVisit(allocator, key);
    }

    pub fn visitExprOnce(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("RootAnalysisState.visitExprOnce requires a worker value with a run() method");
            }
        }

        if (!try self.beginExprVisit(allocator, key)) return;
        try worker.run();
    }

    pub fn scanRoots(
        self: *RootAnalysisState,
        current_module_idx: u32,
        root_exprs: []const CIR.Expr.Idx,
        scanner: anytype,
    ) std.mem.Allocator.Error!void {
        const Scanner = @TypeOf(scanner);
        comptime {
            if (!@hasDecl(Scanner, "scan")) {
                @compileError("RootAnalysisState.scanRoots requires a scanner value with a scan(root: RootExprContext) method");
            }
        }

        for (root_exprs) |expr_idx| {
            self.clearPerScan();
            try scanner.scan(.{
                .module_idx = current_module_idx,
                .expr_idx = expr_idx,
            });
        }
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

    pub fn withValueDefExpr(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("RootAnalysisState.withValueDefExpr requires a worker value with a run() method");
            }
        }

        if (!try self.beginValueDefExpr(allocator, key)) return;
        defer self.endValueDefExpr(key);
        try worker.run();
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

    pub fn withCallResult(
        self: *RootAnalysisState,
        allocator: std.mem.Allocator,
        key: CallResultCallableInstKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("RootAnalysisState.withCallResult requires a worker value with a run() method");
            }
        }

        if (!try self.beginCallResult(allocator, key)) return;
        defer self.endCallResult(key);
        try worker.run();
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
