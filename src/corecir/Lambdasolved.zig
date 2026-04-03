//! Solved lambda-set semantics over context-monomorphic source IR.
//!
//! This stage owns callable-set membership and solved capture provenance.
//! It must not emit executable MIR or LIR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const cm = @import("ContextMono.zig");
const DispatchSolved = @import("DispatchSolved.zig");
const Lambdamono = @import("Lambdamono.zig");
const Monotype = @import("Monotype.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");
const ValueProjection = @import("ValueProjection.zig");

const CIR = can.CIR;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
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
pub const ResolvedMonotype = cm.ResolvedMonotype;
pub const RuntimeValue = Lambdamono.RuntimeValue;
pub const ValueProjectionSpan = Lambdamono.CallableParamProjectionSpan;
pub const CaptureValueSource = Lambdamono.CaptureValueSource;
pub const CaptureStorage = Lambdamono.CaptureStorage;
pub const CaptureField = Lambdamono.CaptureField;
pub const CaptureFieldSpan = Lambdamono.CaptureFieldSpan;
pub const CallableValue = Lambdamono.CallableValue;
pub const CallSite = Lambdamono.CallSite;
pub const ExprRef = Lambdamono.ExprRef;
const DispatchExprTarget = DispatchSolved.DispatchExprTarget;
const DispatchIntrinsic = Lambdamono.DispatchIntrinsic;

const MaterializeCallableValueFailure = enum {
    non_function_monotype,
    requires_owner_callable_inst,
    nested_callable_value_shape,
};

pub const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
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

fn RootRunner(comptime Driver: type) type {
    return struct {
        allocator: std.mem.Allocator,
        current_module_idx: u32,
        root_analysis: RootAnalysisState,
        driver: Driver,

        const Self = @This();

        fn init(
            allocator: std.mem.Allocator,
            current_module_idx: u32,
            driver: Driver,
        ) Self {
            return .{
                .allocator = allocator,
                .current_module_idx = current_module_idx,
                .root_analysis = RootAnalysisState.init(),
                .driver = driver,
            };
        }

        fn deinit(self: *Self) void {
            self.root_analysis.deinit(self.allocator);
        }

        fn scan(self: *Self, root: RootExprContext) std.mem.Allocator.Error!void {
            try self.driver.scanRootExpr(&self.root_analysis, root);
        }

        fn run(self: *Self, root_exprs: []const CIR.Expr.Idx) std.mem.Allocator.Error!void {
            try self.root_analysis.scanRoots(self.current_module_idx, root_exprs, self);
        }
    };
}

pub fn analyzeRoots(
    allocator: std.mem.Allocator,
    current_module_idx: u32,
    root_exprs: []const CIR.Expr.Idx,
    driver: anytype,
) std.mem.Allocator.Error!void {
    var runner = RootRunner(@TypeOf(driver)).init(
        allocator,
        current_module_idx,
        driver,
    );
    defer runner.deinit();
    try runner.run(root_exprs);
}

pub fn withRootAnalysis(
    allocator: std.mem.Allocator,
    worker: anytype,
) std.mem.Allocator.Error!void {
    const Worker = @TypeOf(worker);
    comptime {
        if (!@hasDecl(Worker, "run")) {
            @compileError("Lambdasolved.withRootAnalysis requires a worker value with a run(root_analysis: *RootAnalysisState) method");
        }
    }

    var root_analysis = RootAnalysisState.init();
    defer root_analysis.deinit(allocator);
    try worker.run(&root_analysis);
}

pub fn exprVisitStillPendingForThread(
    root_analysis: *const RootAnalysisState,
    driver: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) bool {
    _ = driver;
    return root_analysis.exprVisitStillPending(cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx));
}

fn RootAnalysisDriver(comptime PassPtr: type, comptime ResultPtr: type) type {
    return struct {
        pass: PassPtr,
        result: ResultPtr,

        fn scanRootExpr(
            self: @This(),
            root_analysis: *RootAnalysisState,
            root: RootExprContext,
        ) std.mem.Allocator.Error!void {
            try scanCirValueExpr(
                self.pass,
                root_analysis,
                self.result,
                SemanticThread.trackedThread(.{ .root_expr = root }),
                root.module_idx,
                root.expr_idx,
            );
        }
    };
}

pub fn rootAnalysisDriver(pass: anytype, result: anytype) RootAnalysisDriver(@TypeOf(pass), @TypeOf(result)) {
    return .{
        .pass = pass,
        .result = result,
    };
}

pub fn scanDemandedValueDefExpr(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const key = cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx);
    const Worker = struct {
        driver_inner: @TypeOf(driver),
        root_analysis_inner: *RootAnalysisState,
        result_inner: @TypeOf(result),
        thread_inner: SemanticThread,
        module_idx_inner: u32,
        expr_idx_inner: CIR.Expr.Idx,

        fn run(self: @This()) std.mem.Allocator.Error!void {
            try self.driver_inner.scanCirValueExpr(
                self.root_analysis_inner,
                self.result_inner,
                self.thread_inner,
                self.module_idx_inner,
                self.expr_idx_inner,
            );

            if (self.result_inner.getExprTemplateId(
                self.thread_inner.requireSourceContext(),
                self.module_idx_inner,
                self.expr_idx_inner,
            )) |template_id| {
                try self.driver_inner.materializeDemandedExprCallableInst(
                    self.result_inner,
                    self.thread_inner,
                    self.module_idx_inner,
                    self.expr_idx_inner,
                    template_id,
                );
            }
        }
    };

    try root_analysis.withValueDefExpr(
        driver.allocator,
        key,
        Worker{
            .driver_inner = driver,
            .root_analysis_inner = root_analysis,
            .result_inner = result,
            .thread_inner = thread,
            .module_idx_inner = module_idx,
            .expr_idx_inner = expr_idx,
        },
    );
}

pub fn scanDemandedValueDefExprInSourceContext(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    try scanDemandedValueDefExpr(
        driver,
        root_analysis,
        result,
        SemanticThread.trackedThread(source_context),
        module_idx,
        expr_idx,
    );
}

pub fn scanCirExpr(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirExprWithDirectCallResolution(
        driver,
        root_analysis,
        result,
        thread,
        module_idx,
        expr_idx,
        true,
    );
}

pub fn scanCirValueExpr(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirValueExprWithDirectCallResolution(
        driver,
        root_analysis,
        result,
        thread,
        module_idx,
        expr_idx,
        true,
    );
}

pub fn scanCirExprWithDirectCallResolution(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    return scanCirExprInternal(
        driver,
        root_analysis,
        result,
        thread,
        module_idx,
        expr_idx,
        false,
        resolve_direct_calls,
    );
}

pub fn scanCirValueExprWithDirectCallResolution(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    return scanCirExprInternal(
        driver,
        root_analysis,
        result,
        thread,
        module_idx,
        expr_idx,
        true,
        resolve_direct_calls,
    );
}

pub fn scanCirExprSpanWithDirectCallResolution(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirExprWithDirectCallResolution(
            driver,
            root_analysis,
            result,
            thread,
            module_idx,
            child_expr,
            resolve_direct_calls,
        );
    }
}

pub fn scanCirValueExprSpanWithDirectCallResolution(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirValueExprWithDirectCallResolution(
            driver,
            root_analysis,
            result,
            thread,
            module_idx,
            child_expr,
            resolve_direct_calls,
        );
    }
}

pub fn scanCirValueExprWithoutDirectCallResolution(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirValueExprWithDirectCallResolution(
        driver,
        root_analysis,
        result,
        thread,
        module_idx,
        expr_idx,
        false,
    );
}

pub fn scanCirExprSpan(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirExpr(driver, root_analysis, result, thread, module_idx, child_expr);
    }
}

pub fn scanCirValueExprSpan(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirValueExpr(driver, root_analysis, result, thread, module_idx, child_expr);
    }
}

pub fn scanClosureCaptureSources(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    closure_expr_idx: CIR.Expr.Idx,
    closure_expr: CIR.Expr.Closure,
    analysis_callable_inst: ?CallableInstId,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];

    const current_template = switch (source_context) {
        .callable_inst => |context_id| result.getCallableTemplate(
            result.getCallableInst(@enumFromInt(@intFromEnum(context_id))).template,
        ).*,
        .root_expr, .provenance_expr, .template_expr => null,
    };

    for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);

        if (current_template) |template| {
            if (template.module_idx == module_idx and
                template.cir_expr == closure_expr_idx and
                switch (template.binding) {
                    .pattern => |binding_pattern| binding_pattern == capture.pattern_idx,
                    .anonymous => false,
                })
            {
                continue;
            }
        }

        const capture_source = blk: {
            const source = (try captureValueSourceForClosurePattern(
                driver,
                result,
                source_context,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue;
            if (analysis_callable_inst) |capture_context_callable_inst| {
                break :blk contextualizeCaptureValueSource(capture_context_callable_inst, source);
            }
            break :blk source;
        };
        switch (capture_source) {
            .bound_expr => |bound_expr| try scanDemandedValueDefExprInSourceContext(
                driver,
                root_analysis,
                result,
                bound_expr.expr_ref.source_context,
                bound_expr.expr_ref.module_idx,
                bound_expr.expr_ref.expr_idx,
            ),
            .lexical_pattern => {},
        }
    }
}

pub fn findDefByPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
    }
    return null;
}

fn captureFieldsEqual(lhs: []const CaptureField, rhs: []const CaptureField) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_field, rhs_field| {
        if (!std.meta.eql(lhs_field, rhs_field)) return false;
    }
    return true;
}

fn callableInstCaptureGraphReaches(
    driver: anytype,
    result: anytype,
    from_callable_inst_id: CallableInstId,
    target_callable_inst_id: CallableInstId,
    visited: *std.AutoHashMapUnmanaged(u32, void),
) std.mem.Allocator.Error!bool {
    if (from_callable_inst_id == target_callable_inst_id) return true;

    const from_key = @intFromEnum(from_callable_inst_id);
    const entry = try visited.getOrPut(driver.allocator, from_key);
    if (entry.found_existing) return false;

    const callable_def = result.getCallableDefForInst(from_callable_inst_id);
    for (result.getCaptureFields(callable_def.captures)) |capture_field| {
        if (capture_field.callable_value) |callable_value| {
            for (result.lambdamono.getCallableValueVariants(callable_value)) |capture_callable_inst_id| {
                if (try callableInstCaptureGraphReaches(
                    driver,
                    result,
                    capture_callable_inst_id,
                    target_callable_inst_id,
                    visited,
                )) {
                    return true;
                }
            }
        }
    }

    return false;
}

fn callableInstSharesRecursiveEnvironment(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    capture_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!bool {
    var visited: std.AutoHashMapUnmanaged(u32, void) = .empty;
    defer visited.deinit(driver.allocator);
    return callableInstCaptureGraphReaches(
        driver,
        result,
        capture_callable_inst_id,
        callable_inst_id,
        &visited,
    );
}

pub fn closureCaptureAnalysisCallableInst(
    enclosing_source_context: SourceContext,
    closure_callable_inst_id: CallableInstId,
) CallableInstId {
    return switch (enclosing_source_context) {
        .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
        .root_expr, .provenance_expr, .template_expr => closure_callable_inst_id,
    };
}

pub fn contextualizeCaptureValueSource(
    _: CallableInstId,
    capture_value_source: CaptureValueSource,
) CaptureValueSource {
    return switch (capture_value_source) {
        .bound_expr => capture_value_source,
        .lexical_pattern => capture_value_source,
    };
}

pub fn captureMaterializationExprRef(
    capture_context_callable_inst: CallableInstId,
    expr_ref: ExprRef,
) ExprRef {
    return switch (expr_ref.source_context) {
        .callable_inst => expr_ref,
        .root_expr, .provenance_expr, .template_expr => .{
            .source_context = .{ .callable_inst = @enumFromInt(@intFromEnum(capture_context_callable_inst)) },
            .module_idx = expr_ref.module_idx,
            .expr_idx = expr_ref.expr_idx,
            .projections = expr_ref.projections,
        },
    };
}

pub fn captureValueSourceForClosurePattern(
    _: anytype,
    result: anytype,
    source_context: SourceContext,
    module_env: *const ModuleEnv,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!?CaptureValueSource {
    if (findDefByPattern(module_env, pattern_idx) != null) {
        return null;
    }
    if (result.getContextPatternSourceExpr(source_context, module_idx, pattern_idx)) |source| {
        return .{ .bound_expr = .{
            .binding_source_context = source_context,
            .binding_module_idx = module_idx,
            .binding_pattern_idx = pattern_idx,
            .expr_ref = source,
        } };
    }
    return .{ .lexical_pattern = .{
        .source_context = source_context,
        .module_idx = module_idx,
        .pattern_idx = pattern_idx,
    } };
}

pub fn resolveBoundCapturePatternMonotype(
    driver: anytype,
    result: anytype,
    bound_expr: @FieldType(CaptureValueSource, "bound_expr"),
) std.mem.Allocator.Error!?ResolvedMonotype {
    const binding_thread = SemanticThread.trackedThread(bound_expr.binding_source_context);
    const binding_mono = try cm.resolvePatternMonotypeResolved(
        driver,
        result,
        binding_thread,
        bound_expr.binding_module_idx,
        bound_expr.binding_pattern_idx,
    );
    if (!binding_mono.isNone()) return binding_mono;

    const source_thread = SemanticThread.trackedThread(bound_expr.expr_ref.source_context);
    var source_mono = (try cm.resolveExprMonotypeResolved(
        driver,
        result,
        source_thread,
        bound_expr.expr_ref.module_idx,
        bound_expr.expr_ref.expr_idx,
    )) orelse return null;
    for (result.lambdamono.getValueProjectionEntries(bound_expr.expr_ref.projections)) |projection| {
        source_mono = try cm.projectResolvedMonotypeByValueProjection(driver, result, source_mono, projection);
        if (source_mono.isNone()) return null;
    }
    try cm.mergeContextPatternMonotype(
        driver,
        result,
        cm.Result.contextPatternKey(
            bound_expr.binding_source_context,
            bound_expr.binding_module_idx,
            bound_expr.binding_pattern_idx,
        ),
        source_mono,
    );
    return source_mono;
}

pub fn resolveCaptureValueSourceMonotype(
    driver: anytype,
    result: anytype,
    capture_context_callable_inst: CallableInstId,
    capture_value_source: CaptureValueSource,
    capture_callable_value: ?CallableValue,
) std.mem.Allocator.Error!ResolvedMonotype {
    return switch (contextualizeCaptureValueSource(
        capture_context_callable_inst,
        capture_value_source,
    )) {
        .bound_expr => |bound_expr| blk: {
            if (capture_callable_value) |callable_value| {
                break :blk result.getCallableValueSourceMonotype(callable_value);
            }
            if (try resolveBoundCapturePatternMonotype(driver, result, bound_expr)) |binding_mono| {
                break :blk binding_mono;
            }
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: missing exact contextual monotype for captured bound pattern module={d} pattern={d} source_context={s} context_callable_inst={d}",
                    .{
                        bound_expr.binding_module_idx,
                        @intFromEnum(bound_expr.binding_pattern_idx),
                        @tagName(bound_expr.binding_source_context),
                        @intFromEnum(capture_context_callable_inst),
                    },
                );
            }
            unreachable;
        },
        .lexical_pattern => |lexical| blk: {
            if (result.getContextPatternMonotype(
                lexical.source_context,
                lexical.module_idx,
                lexical.pattern_idx,
            )) |pattern_mono| break :blk pattern_mono;
            if (cm.lookupContextTypeVarMonotype(
                driver,
                result,
                lexical.source_context,
                lexical.module_idx,
                ModuleEnv.varFrom(lexical.pattern_idx),
            )) |binding_mono| break :blk binding_mono;
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: missing exact contextual monotype for lexical capture pattern module={d} pattern={d} source_context={s} context_callable_inst={d}",
                    .{
                        lexical.module_idx,
                        @intFromEnum(lexical.pattern_idx),
                        @tagName(lexical.source_context),
                        @intFromEnum(capture_context_callable_inst),
                    },
                );
            }
            unreachable;
        },
    };
}

pub fn resolveCaptureCallableValue(
    driver: anytype,
    result: anytype,
    capture_context_callable_inst: CallableInstId,
    capture_value_source: CaptureValueSource,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    return switch (contextualizeCaptureValueSource(
        capture_context_callable_inst,
        capture_value_source,
    )) {
        .bound_expr => |bound_expr| blk: {
            if (readCallableParamValue(
                result,
                bound_expr.binding_source_context,
                bound_expr.binding_module_idx,
                bound_expr.binding_pattern_idx,
            )) |callable_value| {
                break :blk callable_value;
            }
            if (try resolveExprRefCallableValue(driver, result, bound_expr.expr_ref, visiting)) |callable_value| {
                try setCallableParamValue(
                    driver,
                    result,
                    bound_expr.binding_source_context,
                    bound_expr.binding_module_idx,
                    bound_expr.binding_pattern_idx,
                    callable_value,
                );
                break :blk callable_value;
            }
            if (bound_expr.expr_ref.projections.isEmpty()) {
                const materialization_expr_ref = captureMaterializationExprRef(
                    capture_context_callable_inst,
                    bound_expr.expr_ref,
                );
                if (try resolveExprRefCallableValue(driver, result, materialization_expr_ref, visiting)) |callable_value| {
                    break :blk callable_value;
                }

                const maybe_template_id = result.getExprTemplateId(
                    materialization_expr_ref.source_context,
                    materialization_expr_ref.module_idx,
                    materialization_expr_ref.expr_idx,
                );
                if (maybe_template_id) |template_id| {
                    const fn_monotype = cm.resolvedIfFunctionMonotype(result,
                        try cm.resolveExprMonotypeResolved(
                            driver,
                            result,
                            SemanticThread.trackedThread(materialization_expr_ref.source_context),
                            materialization_expr_ref.module_idx,
                            materialization_expr_ref.expr_idx,
                        ) orelse break :blk null,
                    ) orelse break :blk null;
                    _ = try materializeExprCallableValueWithKnownFnMonotype(
                        driver,
                        result,
                        materialization_expr_ref.source_context,
                        materialization_expr_ref.module_idx,
                        materialization_expr_ref.expr_idx,
                        template_id,
                        fn_monotype,
                    );
                    if (try resolveExprRefCallableValue(driver, result, materialization_expr_ref, visiting)) |callable_value| {
                        break :blk callable_value;
                    }
                    if (readCallableParamValue(
                        result,
                        bound_expr.binding_source_context,
                        bound_expr.binding_module_idx,
                        bound_expr.binding_pattern_idx,
                    )) |callable_value| {
                        break :blk callable_value;
                    }
                } else if (std.debug.runtime_safety) {
                    const fn_monotype = (try cm.resolveExprMonotypeResolved(
                        driver,
                        result,
                        SemanticThread.trackedThread(materialization_expr_ref.source_context),
                        materialization_expr_ref.module_idx,
                        materialization_expr_ref.expr_idx,
                    )) orelse break :blk null;
                    if (result.context_mono.monotype_store.getMonotype(fn_monotype.idx) == .func) {
                        std.debug.panic(
                            "Lambdasolved invariant violated: function-valued capture materialization expr {d} in module {d} under source context {s} had no registered callable template",
                            .{
                                @intFromEnum(materialization_expr_ref.expr_idx),
                                materialization_expr_ref.module_idx,
                                @tagName(materialization_expr_ref.source_context),
                            },
                        );
                    }
                }
            }
            break :blk null;
        },
        .lexical_pattern => |lexical| readCallableParamValue(
            result,
            lexical.source_context,
            lexical.module_idx,
            lexical.pattern_idx,
        ),
    };
}

pub fn updateCallableDefRuntimeValue(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    runtime_expr_ref: ExprRef,
    body_expr_ref: ExprRef,
    capture_fields: []const CaptureField,
) std.mem.Allocator.Error!void {
    var runtime_field_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer runtime_field_monotypes.deinit(driver.allocator);
    const callable_def = result.getCallableDefForInst(callable_inst_id);

    for (capture_fields) |capture_field| {
        switch (capture_field.storage) {
            .runtime_field => |runtime_field| {
                const field_monotype = if (runtime_field.field_monotype.module_idx == callable_def.module_idx)
                    runtime_field.field_monotype.idx
                else
                    try cm.remapMonotypeBetweenModules(driver, result,
                        runtime_field.field_monotype.idx,
                        runtime_field.field_monotype.module_idx,
                        callable_def.module_idx,
                    );
                try runtime_field_monotypes.append(driver.allocator, field_monotype);
            },
            .callable_only, .recursive_member => {},
        }
    }

    const next_runtime_value: RuntimeValue =
        if (runtime_field_monotypes.items.len == 0)
            .direct_lambda
        else blk: {
            const field_span = try result.context_mono.monotype_store.addIdxSpan(driver.allocator, runtime_field_monotypes.items);
            break :blk .{ .closure = .{
                .capture_tuple_monotype = cm.resolvedMonotype(
                    try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .tuple = .{ .elems = field_span } }),
                    callable_def.module_idx,
                ),
            } };
        };
    try Lambdamono.updateCallableDefRuntimeSemantics(
        driver,
        result,
        callable_inst_id,
        runtime_expr_ref,
        body_expr_ref,
        capture_fields,
        next_runtime_value,
    );
}

pub fn finalizeCallableDefForCallableInst(
    driver: anytype,
    result: anytype,
    enclosing_source_context: SourceContext,
    module_idx: u32,
    closure_expr_idx: CIR.Expr.Idx,
    closure_expr: CIR.Expr.Closure,
    closure_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    var capture_fields = std.ArrayList(CaptureField).empty;
    defer capture_fields.deinit(driver.allocator);
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);
    const closure_template = result.getCallableTemplate(result.getCallableInst(closure_callable_inst_id).template);

    for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
            enclosing_source_context,
            closure_callable_inst_id,
        );
        const capture_value_source = contextualizeCaptureValueSource(
            capture_context_callable_inst,
            (try captureValueSourceForClosurePattern(
                driver,
                result,
                enclosing_source_context,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue,
        );
        const capture_callable_value = try resolveCaptureCallableValue(
            driver,
            result,
            capture_context_callable_inst,
            capture_value_source,
            &visiting,
        );
        const capture_mono = try resolveCaptureValueSourceMonotype(
            driver,
            result,
            capture_context_callable_inst,
            capture_value_source,
            capture_callable_value,
        );
        if (capture_mono.isNone()) {
            std.debug.panic(
                "Lambdasolved invariant violated: missing exact capture monotype for closure callable_inst={d} module={d} closure_expr={d} pattern={d}",
                .{
                    @intFromEnum(closure_callable_inst_id),
                    module_idx,
                    @intFromEnum(closure_expr_idx),
                    @intFromEnum(capture.pattern_idx),
                },
            );
        }

        const resolved_capture_callable_value = if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func)
            (capture_callable_value orelse std.debug.panic(
                "Lambdasolved invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no executable callable value fact",
                .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
            ))
        else
            null;

        const capture_storage: CaptureStorage = blk: {
            if (resolved_capture_callable_value) |callable_value| {
                switch (callable_value) {
                    .direct => |capture_callable_inst_id| {
                        const capture_template = result.getCallableTemplate(
                            result.getCallableInst(capture_callable_inst_id).template,
                        );
                        switch (capture_template.binding) {
                            .pattern => |binding_pattern| if (binding_pattern == capture.pattern_idx and
                                try callableInstSharesRecursiveEnvironment(
                                    driver,
                                    result,
                                    closure_callable_inst_id,
                                    capture_callable_inst_id,
                                ))
                            {
                                break :blk .recursive_member;
                            },
                            .anonymous => {},
                        }
                    },
                    .packed_fn => {},
                }
            }

            if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func) {
                const callable_value = resolved_capture_callable_value orelse std.debug.panic(
                    "Lambdasolved invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no executable callable value",
                    .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
                );
                switch (callable_value) {
                    .direct => |capture_callable_inst_id| switch (result.getCallableInst(capture_callable_inst_id).runtime_value) {
                        .direct_lambda => break :blk .callable_only,
                        .closure => |closure_value| break :blk .{ .runtime_field = .{
                            .field_monotype = closure_value.capture_tuple_monotype,
                        } },
                    },
                    .packed_fn => |packed_fn| break :blk .{ .runtime_field = .{
                        .field_monotype = packed_fn.runtime_monotype,
                    } },
                }
            }

            break :blk .{ .runtime_field = .{
                .field_monotype = capture_mono,
            } };
        };

        try capture_fields.append(driver.allocator, .{
            .pattern_idx = capture.pattern_idx,
            .local_monotype = capture_mono,
            .callable_value = resolved_capture_callable_value,
            .source = switch (capture_value_source) {
                .bound_expr => |bound_expr| .{ .bound_expr = .{
                    .binding_source_context = bound_expr.binding_source_context,
                    .binding_module_idx = bound_expr.binding_module_idx,
                    .binding_pattern_idx = bound_expr.binding_pattern_idx,
                    .expr_ref = captureMaterializationExprRef(
                        capture_context_callable_inst,
                        bound_expr.expr_ref,
                    ),
                } },
                .lexical_pattern => capture_value_source,
            },
            .storage = capture_storage,
        });
    }

    try updateCallableDefRuntimeValue(
        driver,
        result,
        closure_callable_inst_id,
        .{
            .source_context = .{ .callable_inst = @enumFromInt(@intFromEnum(closure_callable_inst_id)) },
            .module_idx = module_idx,
            .expr_idx = closure_expr_idx,
        },
        .{
            .source_context = .{ .callable_inst = @enumFromInt(@intFromEnum(closure_callable_inst_id)) },
            .module_idx = closure_template.module_idx,
            .expr_idx = closure_template.body_expr,
        },
        capture_fields.items,
    );
}

pub fn seedClosureCaptureCallableValues(
    driver: anytype,
    result: anytype,
    enclosing_source_context: SourceContext,
    module_idx: u32,
    closure_expr: CIR.Expr.Closure,
    closure_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);

    for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
            enclosing_source_context,
            closure_callable_inst_id,
        );
        const capture_value_source = contextualizeCaptureValueSource(
            capture_context_callable_inst,
            (try captureValueSourceForClosurePattern(
                driver,
                result,
                enclosing_source_context,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue,
        );
        const capture_callable_value = (try resolveCaptureCallableValue(
            driver,
            result,
            capture_context_callable_inst,
            capture_value_source,
            &visiting,
        )) orelse continue;
        try setCallableParamValueInCallableContext(
            driver,
            result,
            closure_callable_inst_id,
            module_idx,
            capture.pattern_idx,
            capture_callable_value,
        );
    }
}

pub fn seedClosureCapturePatternSources(
    driver: anytype,
    result: anytype,
    defining_source_context: SourceContext,
    module_idx: u32,
    closure_expr: CIR.Expr.Closure,
    closure_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const closure_source_context: SourceContext = .{
        .callable_inst = @enumFromInt(@intFromEnum(closure_callable_inst_id)),
    };

    for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_value_source = (try captureValueSourceForClosurePattern(
            driver,
            result,
            defining_source_context,
            module_env,
            module_idx,
            capture.pattern_idx,
        )) orelse continue;

        switch (capture_value_source) {
            .bound_expr => |bound_expr| {
                const capture_expr_ref: ExprRef = switch (bound_expr.expr_ref.source_context) {
                    .callable_inst => bound_expr.expr_ref,
                    .root_expr, .provenance_expr, .template_expr => .{
                        .source_context = closure_source_context,
                        .module_idx = bound_expr.expr_ref.module_idx,
                        .expr_idx = bound_expr.expr_ref.expr_idx,
                        .projections = bound_expr.expr_ref.projections,
                    },
                };
                try recordContextPatternSourceExpr(
                    driver,
                    result,
                    closure_source_context,
                    module_idx,
                    capture.pattern_idx,
                    capture_expr_ref,
                );
            },
            .lexical_pattern => {},
        }
    }
}

pub fn realizeCallableArgSemantics(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    args: CIR.Expr.Span,
) std.mem.Allocator.Error!void {
    try withRootAnalysis(driver.allocator, struct {
        driver_inner: @TypeOf(driver),
        result_inner: @TypeOf(result),
        thread_inner: SemanticThread,
        module_idx_inner: u32,
        args_inner: CIR.Expr.Span,

        fn run(self: @This(), root_analysis: *RootAnalysisState) std.mem.Allocator.Error!void {
            const module_env = self.driver_inner.all_module_envs[self.module_idx_inner];
            for (module_env.store.sliceExpr(self.args_inner)) |arg_expr_idx| {
                try self.driver_inner.scanCirValueExpr(
                    root_analysis,
                    self.result_inner,
                    self.thread_inner,
                    self.module_idx_inner,
                    arg_expr_idx,
                );
                var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
                defer visiting.deinit(self.driver_inner.allocator);
                try realizeStructuredExprCallableSemantics(
                    self.driver_inner,
                    self.result_inner,
                    self.thread_inner.requireSourceContext(),
                    self.module_idx_inner,
                    arg_expr_idx,
                    &visiting,
                );
                _ = getValueExprCallableValueForThread(
                    self.driver_inner,
                    self.result_inner,
                    self.thread_inner,
                    self.module_idx_inner,
                    arg_expr_idx,
                );
            }
        }
    }{
        .driver_inner = driver,
        .result_inner = result,
        .thread_inner = thread,
        .module_idx_inner = module_idx,
        .args_inner = args,
    });
}

pub fn realizeCallableArgSemanticsSlice(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (arg_exprs) |arg_expr_idx| {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        try realizeStructuredExprCallableSemantics(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            arg_expr_idx,
            &visiting,
        );
        _ = getValueExprCallableValueForThread(driver, result, thread, module_idx, arg_expr_idx);
    }
}

pub fn collectCallableParamSpecsFromFunctionArgument(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    param_index: u16,
    projections: *std.ArrayListUnmanaged(ValueProjection.Projection),
    out: anytype,
) std.mem.Allocator.Error!bool {
    const Worker = struct {
        driver_inner: @TypeOf(driver),
        result_inner: @TypeOf(result),
        source_context_inner: SourceContext,
        module_idx_inner: u32,
        expr_idx_inner: CIR.Expr.Idx,
        param_index_inner: u16,
        projections_inner: *std.ArrayListUnmanaged(ValueProjection.Projection),
        out_inner: @TypeOf(out),
        completed: bool = false,

        fn run(self: *@This(), root_analysis: *RootAnalysisState) std.mem.Allocator.Error!void {
            var demand_visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
            defer demand_visiting.deinit(self.driver_inner.allocator);
            try self.driver_inner.scanCirValueExpr(
                root_analysis,
                self.result_inner,
                SemanticThread.trackedThread(self.source_context_inner),
                self.module_idx_inner,
                self.expr_idx_inner,
            );

            if (try resolveProjectedExprCallableValue(
                self.driver_inner,
                self.result_inner,
                self.source_context_inner,
                self.module_idx_inner,
                self.expr_idx_inner,
                self.projections_inner.items,
                &demand_visiting,
            )) |callable_value| {
                try Lambdamono.appendCallableParamSpecEntry(self.driver_inner, self.result_inner, self.out_inner, .{
                    .param_index = self.param_index_inner,
                    .projections = try Lambdamono.addCallableParamProjectionEntries(
                        self.driver_inner,
                        self.result_inner,
                        self.projections_inner.items,
                    ),
                    .callable_value = callable_value,
                });
                self.completed = true;
            }
        }
    };

    var worker = Worker{
        .driver_inner = driver,
        .result_inner = result,
        .source_context_inner = source_context,
        .module_idx_inner = module_idx,
        .expr_idx_inner = expr_idx,
        .param_index_inner = param_index,
        .projections_inner = projections,
        .out_inner = out,
    };
    try withRootAnalysis(driver.allocator, &worker);
    return worker.completed;
}

pub fn collectDirectCallCallableParamSpecs(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    caller_module_idx: u32,
    fn_monotype: anytype,
    fn_monotype_module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    out: anytype,
) std.mem.Allocator.Error!bool {
    const func_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => return true,
    };
    if (func_mono.args.len != arg_exprs.len) unreachable;

    var projections = std.ArrayListUnmanaged(ValueProjection.Projection).empty;
    defer projections.deinit(driver.allocator);
    for (arg_exprs, 0..) |arg_expr_idx, param_index| {
        projections.clearRetainingCapacity();
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(func_mono.args, param_index);
        if (!try collectCallableParamSpecsFromArgument(
            driver,
            result,
            source_context,
            caller_module_idx,
            arg_expr_idx,
            param_mono,
            fn_monotype_module_idx,
            @intCast(param_index),
            &projections,
            out,
        )) return false;
    }

    return true;
}

pub fn ensureCallableInstRealized(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_def = result.getCallableDefForInst(callable_inst_id);
    if (result.getExprCallableValue(
        callable_def.runtime_expr.source_context,
        callable_def.runtime_expr.module_idx,
        callable_def.runtime_expr.expr_idx,
    ) != null) return;
    try realizeCallableInstWithFreshRootAnalysis(driver, result, callable_inst_id);
}

pub fn requireCallableInst(
    driver: anytype,
    result: anytype,
    defining_source_context: SourceContext,
    template_id: CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!CallableInstId {
    const callable_inst_id = (try Lambdamono.ensureCallableInstRecord(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        &.{},
    )).id;
    try ensureCallableInstRealized(driver, result, callable_inst_id);
    return callable_inst_id;
}

pub fn requireCallableInstWithCallableParamSpecs(
    driver: anytype,
    result: anytype,
    defining_source_context: SourceContext,
    template_id: CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    callable_param_specs: []const Lambdamono.CallableParamSpecEntry,
) std.mem.Allocator.Error!CallableInstId {
    const callable_inst_id = (try Lambdamono.ensureCallableInstRecord(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs,
    )).id;
    try ensureCallableInstRealized(driver, result, callable_inst_id);
    return callable_inst_id;
}

pub fn collectCallableParamSpecsFromArgument(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: anytype,
    monotype_module_idx: u32,
    param_index: u16,
    projections: *std.ArrayListUnmanaged(ValueProjection.Projection),
    out: anytype,
) std.mem.Allocator.Error!bool {
    try propagateDemandedValueMonotypeToValueExpr(
        result,
        source_context,
        module_idx,
        expr_idx,
        monotype,
        monotype_module_idx,
    );

    if (result.context_mono.monotype_store.isOpaque(monotype)) return true;

    switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .func => {
            return collectCallableParamSpecsFromFunctionArgument(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                param_index,
                projections,
                out,
            );
        },
        .record => |record| {
            for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                try projections.append(driver.allocator, .{ .field = field.name });
                defer _ = projections.pop();

                if (!try collectCallableParamSpecsFromArgument(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    field.type_idx,
                    monotype_module_idx,
                    param_index,
                    projections,
                    out,
                )) return false;
            }
            return true;
        },
        .tuple => |tuple_mono| {
            const elems = result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems);
            for (elems, 0..) |elem_mono, elem_index| {
                try projections.append(driver.allocator, .{ .tuple_elem = @intCast(elem_index) });
                defer _ = projections.pop();

                if (!try collectCallableParamSpecsFromArgument(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    elem_mono,
                    monotype_module_idx,
                    param_index,
                    projections,
                    out,
                )) return false;
            }
            return true;
        },
        else => return true,
    }
}

pub fn bindCurrentCallFromCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };

    const arg_exprs = driver.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
    if (arg_exprs.len != fn_mono.args.len) unreachable;

    for (arg_exprs, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) {
            try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            try propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
            );
        }
    }

    if (result.context_mono.monotype_store.getMonotype(fn_mono.ret) != .func) {
        try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, call_expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
    }
}

pub fn bindCurrentCallFromFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
    fn_monotype: anytype,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => return,
    };

    const arg_exprs = driver.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
    if (arg_exprs.len != fn_mono.args.len) return;

    for (arg_exprs, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) {
            try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, arg_expr_idx, param_mono, fn_monotype_module_idx);
            try propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                fn_monotype_module_idx,
            );
        }
    }

    if (result.context_mono.monotype_store.getMonotype(fn_mono.ret) != .func) {
        try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
    }
}

pub fn specializeDirectCallExactCallable(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
    template_id: CallableTemplateId,
    template_source_context: SourceContext,
) std.mem.Allocator.Error!?CallableInstId {
    if (try directCallContainsErrorType(driver, module_idx, call_expr_idx, call_expr)) {
        return null;
    }

    const template = result.getCallableTemplate(template_id).*;
    const defining_source_context = Lambdamono.resolveTemplateDefiningSourceContext(result, template_source_context, template);
    const arg_exprs = driver.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
    const exact_desired_fn_monotype = (try resolveDemandedDirectCallFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
    )) orelse return null;
    const fn_monotype = exact_desired_fn_monotype.idx;
    const fn_monotype_module_idx = exact_desired_fn_monotype.module_idx;

    var callable_param_specs = std.ArrayListUnmanaged(Lambdamono.CallableParamSpecEntry).empty;
    defer callable_param_specs.deinit(driver.allocator);
    const callable_param_specs_complete = try collectDirectCallCallableParamSpecs(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        fn_monotype,
        fn_monotype_module_idx,
        arg_exprs,
        &callable_param_specs,
    );
    if (!callable_param_specs_complete) return null;
    if (std.debug.runtime_safety) {
        const func_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };
        for (result.context_mono.monotype_store.getIdxSpan(func_mono.args), 0..) |param_mono, param_index| {
            if (!try monotypeRequiresCallableParamSpec(driver, result, param_mono)) continue;
            for (callable_param_specs.items) |spec| {
                if (spec.param_index == param_index) break;
            } else {
                std.debug.panic(
                    "Pipeline invariant violated: direct-call specialization for template {d} expr {d} in module {d} produced no callable-param spec for higher-order param {d}; source_context={s} call_expr={d} fn_monotype={d}@{d}",
                    .{
                        @intFromEnum(template_id),
                        @intFromEnum(template.cir_expr),
                        template.module_idx,
                        param_index,
                        @tagName(thread.requireSourceContext()),
                        @intFromEnum(call_expr_idx),
                        @intFromEnum(fn_monotype),
                        fn_monotype_module_idx,
                    },
                );
            }
        }
    }

    return try requireCallableInstWithCallableParamSpecs(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs.items,
    );
}

pub fn assignCallableArgCallableInstsFromParams(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    param_monos: anytype,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    if (arg_exprs.len != param_monos.len) unreachable;

    for (arg_exprs, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(param_monos, i);
        const maybe_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, arg_expr_idx);
        const template_id = maybe_template_id orelse continue;
        const template = result.getCallableTemplate(template_id);
        if (!thread.hasCallableInst() and template.kind == .closure) {
            continue;
        }
        const callable_inst_id = (try Lambdamono.ensureCallableInstRecord(
            driver,
            result,
            thread.requireSourceContext(),
            template_id,
            param_mono,
            fn_monotype_module_idx,
            &.{},
        )).id;

        try setExprDirectCallable(driver,
            result,
            thread.requireSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );

        switch (module_env.store.getExpr(arg_expr_idx)) {
            .e_lookup_local => |lookup| {
                try setExprDirectCallable(driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    arg_expr_idx,
                    callable_inst_id,
                );
                try setCallableParamDirectValue(driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    lookup.pattern_idx,
                    callable_inst_id,
                );
                try propagateLookupSourceExprCallableValue(driver, result, thread, module_idx, arg_expr_idx, callable_inst_id);
            },
            .e_lookup_external, .e_lookup_required => {
                try setExprDirectCallable(driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    arg_expr_idx,
                    callable_inst_id,
                );
                try propagateLookupSourceExprCallableValue(driver, result, thread, module_idx, arg_expr_idx, callable_inst_id);
            },
            .e_lambda, .e_closure, .e_hosted_lambda => try setExprDirectCallable(driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                callable_inst_id,
            ),
            else => {},
        }
    }
}

pub fn seedCallableParamValuesFromActualArgs(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    callee_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
    const template = result.getCallableTemplate(callee_callable_inst.template);
    const arg_patterns = driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
    if (arg_patterns.len != arg_exprs.len) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: callable param arity mismatch for callable inst {d} (patterns={d}, args={d})",
                .{ @intFromEnum(callee_callable_inst_id), arg_patterns.len, arg_exprs.len },
            );
        }
        unreachable;
    }

    for (arg_patterns, arg_exprs) |pattern_idx, arg_expr_idx| {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        try realizeStructuredExprCallableSemantics(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            arg_expr_idx,
            &visiting,
        );
        const callable_value = getValueExprCallableValueForSourceContext(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            arg_expr_idx,
        ) orelse continue;

        switch (callable_value) {
            .direct => |callable_inst_id| try setCallableParamDirectValueInCallableContext(driver,
                result,
                callee_callable_inst_id,
                template.module_idx,
                pattern_idx,
                callable_inst_id,
            ),
            .packed_fn => try setCallableParamValueInCallableContext(driver,
                result,
                callee_callable_inst_id,
                template.module_idx,
                pattern_idx,
                callable_value,
            ),
        }
    }
}

pub fn prepareCallableArgsForCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const callable_inst_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };

    try assignCallableArgCallableInstsFromParams(
        driver,
        result,
        thread,
        module_idx,
        arg_exprs,
        callable_inst_fn_mono.args,
        callable_inst.fn_monotype_module_idx,
    );
    try seedCallableParamValuesFromActualArgs(
        driver,
        result,
        thread,
        module_idx,
        arg_exprs,
        callable_inst_id,
    );
}

pub fn assignCallableArgCallableInstsFromCallMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!void {
    if (getCallSiteCallableInstForThread(result, thread, module_idx, call_expr_idx) != null) return;

    const call_fn_monotype = try resolveDemandedDirectCallFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
    );
    const resolved_call_fn_monotype = call_fn_monotype orelse return;

    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(resolved_call_fn_monotype.idx)) {
        .func => |func| func,
        else => return,
    };

    const arg_exprs = driver.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
    if (arg_exprs.len != fn_mono.args.len) return;

    try assignCallableArgCallableInstsFromParams(
        driver,
        result,
        thread,
        module_idx,
        arg_exprs,
        fn_mono.args,
        call_fn_monotype.?.module_idx,
    );
    try realizeCallableArgSemantics(driver, result, thread, module_idx, call_expr.args);
}

pub fn finalizeResolvedDirectCallCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
    callee_expr: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    try ensureCallableInstRealized(driver, result, callable_inst_id);
    const module_env = driver.all_module_envs[module_idx];
    const source_context = thread.requireSourceContext();

    try setExprDirectCallSite(driver,
        result,
        source_context,
        module_idx,
        call_expr_idx,
        callable_inst_id,
    );
    try setExprDirectCallable(driver,
        result,
        source_context,
        module_idx,
        call_expr.func,
        callable_inst_id,
    );
    switch (callee_expr) {
        .e_lookup_local => |lookup| {
            try setExprDirectCallable(driver,
                result,
                source_context,
                module_idx,
                call_expr.func,
                callable_inst_id,
            );
            if (switch (source_context) {
                .callable_inst => true,
                .root_expr, .provenance_expr, .template_expr => false,
            }) {
                try setCallableParamDirectValue(driver,
                    result,
                    source_context,
                    module_idx,
                    lookup.pattern_idx,
                    callable_inst_id,
                );
            }
        },
        .e_lookup_external, .e_lookup_required => try setExprDirectCallable(driver,
            result,
            source_context,
            module_idx,
            call_expr.func,
            callable_inst_id,
        ),
        else => {},
    }

    const callable_inst = result.getCallableInst(callable_inst_id).*;
    const callee_template = result.getCallableTemplate(callable_inst.template);
    try setExprDirectCallable(driver,
        result,
        source_context,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    try setExprDirectCallable(driver,
        result,
        callable_inst.defining_source_context,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    try setExprDirectCallableInCallableContext(driver,
        result,
        callable_inst_id,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    try bindCurrentCallFromCallableInst(driver, result, thread, module_idx, call_expr_idx, call_expr, callable_inst_id);
    const callable_inst_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };
    const arg_exprs = module_env.store.sliceExpr(call_expr.args);
    try prepareCallableArgsForCallableInst(driver, result, thread, module_idx, arg_exprs, callable_inst_id);
    const callable_def = result.getCallableDefForInst(callable_inst_id);
    try recordExprSourceExpr(
        driver,
        result,
        source_context,
        module_idx,
        call_expr_idx,
        .{
            .source_context = callable_def.body_expr.source_context,
            .module_idx = callable_def.body_expr.module_idx,
            .expr_idx = callable_def.body_expr.expr_idx,
            .projections = callable_def.body_expr.projections,
        },
    );
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);
    var variant_builder = CallableVariantBuilder.init();
    defer variant_builder.deinit(driver.allocator);
    try recordCallResultCallableValueFromCallee(
        driver,
        result,
        source_context,
        module_idx,
        call_expr_idx,
        callable_inst_id,
        &visiting,
        &variant_builder,
    );
    if (try variant_builder.finishValue(driver, result)) |call_result_value| {
        try setExprCallableValue(driver,
            result,
            source_context,
            module_idx,
            call_expr_idx,
            call_result_value,
        );
    }
    try realizeCallableArgSemantics(driver, result, thread, module_idx, call_expr.args);
    if (result.context_mono.monotype_store.getMonotype(callable_inst_fn_mono.ret) != .func) {
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            call_expr_idx,
            callable_inst_fn_mono.ret,
            callable_inst.fn_monotype_module_idx,
        );
    }
}

fn containsU16(values: []const u16, target: u16) bool {
    for (values) |value| {
        if (value == target) return true;
    }
    return false;
}

fn appendUniqueU16(
    allocator: std.mem.Allocator,
    values: *std.ArrayListUnmanaged(u16),
    target: u16,
) std.mem.Allocator.Error!void {
    if (containsU16(values.items, target)) return;
    try values.append(allocator, target);
}

pub fn inferCallableParamPatternValueForCallableInst(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    param_index: u16,
    expected_monotype: cm.ResolvedMonotype,
) std.mem.Allocator.Error!?CallableValue {
    const callable_inst = result.getCallableInst(callable_inst_id);
    var variant_builder = CallableVariantBuilder.init();
    defer variant_builder.deinit(driver.allocator);

    for (result.lambdamono.getCallableInsts()) |other_callable_inst| {
        if (other_callable_inst.template != callable_inst.template) continue;
        for (result.lambdamono.getCallableParamSpecEntries(other_callable_inst.callable_param_specs)) |spec| {
            if (spec.param_index != param_index or !spec.projections.isEmpty()) continue;

            const spec_monotype = result.getCallableValueSourceMonotype(spec.callable_value);
            if (!try cm.monotypesStructurallyEqualAcrossModules(
                driver,
                result,
                expected_monotype.idx,
                expected_monotype.module_idx,
                spec_monotype.idx,
                spec_monotype.module_idx,
            )) {
                continue;
            }

            try variant_builder.includeCallableValue(driver, result, spec.callable_value);
        }
    }

    return try variant_builder.finishValue(driver, result);
}

pub fn recordCallableParamPatternFactsForCallableInst(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    const arg_patterns = driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => return,
    };
    if (fn_mono.args.len != arg_patterns.len) {
        std.debug.panic(
            "Pipeline invariant violated: callable inst {d} arg-pattern arity {d} did not match fn monotype arity {d}",
            .{ @intFromEnum(callable_inst_id), arg_patterns.len, fn_mono.args.len },
        );
    }

    var explicit_param_indices = std.ArrayListUnmanaged(u16).empty;
    defer explicit_param_indices.deinit(driver.allocator);

    for (result.lambdamono.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
        if (!spec.projections.isEmpty()) continue;
        if (spec.param_index >= arg_patterns.len) {
            std.debug.panic(
                "Pipeline: callable param spec index {d} out of bounds for callable inst {d} (arg_patterns={d})",
                .{ spec.param_index, @intFromEnum(callable_inst_id), arg_patterns.len },
            );
        }
        try appendUniqueU16(driver.allocator, &explicit_param_indices, spec.param_index);

        switch (spec.callable_value) {
            .direct => |variant_callable_inst| try setCallableParamDirectValueInCallableContext(driver,
                result,
                callable_inst_id,
                template.module_idx,
                arg_patterns[spec.param_index],
                variant_callable_inst,
            ),
            .packed_fn => {
                if (result.lambdamono.getCallableValueVariants(spec.callable_value).len == 0) {
                    std.debug.panic(
                        "Pipeline: callable param spec for callable inst {d} param {d} had an empty packed callable variant set",
                        .{ @intFromEnum(callable_inst_id), spec.param_index },
                    );
                }

                try setCallableParamValueInCallableContext(driver,
                    result,
                    callable_inst_id,
                    template.module_idx,
                    arg_patterns[spec.param_index],
                    spec.callable_value,
                );
            },
        }
    }

    for (arg_patterns, 0..) |pattern_idx, param_index_usize| {
        const param_index: u16 = @intCast(param_index_usize);
        if (containsU16(explicit_param_indices.items, param_index)) continue;

        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, param_index_usize);
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) continue;

        const inferred_callable_value = try inferCallableParamPatternValueForCallableInst(
            driver,
            result,
            callable_inst_id,
            param_index,
            cm.resolvedMonotype(param_mono, callable_inst.fn_monotype_module_idx),
        ) orelse continue;

        try setCallableParamValueInCallableContext(driver,
            result,
            callable_inst_id,
            template.module_idx,
            pattern_idx,
            inferred_callable_value,
        );
    }
}

pub fn realizeCallableInstWithFreshRootAnalysis(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    try withRootAnalysis(driver.allocator, struct {
        driver_inner: @TypeOf(driver),
        result_inner: @TypeOf(result),
        callable_inst_id_inner: CallableInstId,

        fn run(self: @This(), root_analysis: *RootAnalysisState) std.mem.Allocator.Error!void {
            try realizeCallableInst(
                self.driver_inner,
                root_analysis,
                self.result_inner,
                self.callable_inst_id_inner,
            );
        }
    }{
        .driver_inner = driver,
        .result_inner = result,
        .callable_inst_id_inner = callable_inst_id,
    });
}

pub fn realizeCallableInst(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id).*;
    const template = result.getCallableTemplate(callable_inst.template).*;
    const defining_source_context = callable_inst.defining_source_context;
    const callable_source_context: SourceContext = .{
        .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)),
    };
    if (result.getExprCallableValue(
        callable_source_context,
        template.module_idx,
        template.runtime_expr,
    ) != null) return;

    const module_env = driver.all_module_envs[template.module_idx];

    try seedCallableBoundaryContextMonotypes(
        driver,
        result,
        callable_source_context,
        template.module_idx,
        template.cir_expr,
        template.arg_patterns,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
    );
    const thread = SemanticThread.trackedThread(callable_source_context);
    switch (template.binding) {
        .pattern => |binding_pattern| {
            try cm.mergeContextPatternMonotype(
                driver,
                result,
                cm.Result.contextPatternKey(
                    callableInstSourceContext(callable_inst_id),
                    template.module_idx,
                    binding_pattern,
                ),
                cm.resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
            );
            try setCallableParamDirectValueInCallableContext(driver,
                result,
                callable_inst_id,
                template.module_idx,
                binding_pattern,
                callable_inst_id,
            );
        },
        .anonymous => {},
    }

    try recordCallableParamPatternFactsForCallableInst(driver, result, callable_inst_id);

    if (template.kind == .closure) {
        const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
            .e_closure => |closure| closure,
            else => unreachable,
        };
        try seedClosureCapturePatternSources(
            driver,
            result,
            defining_source_context,
            template.module_idx,
            closure_expr,
            callable_inst_id,
        );
        try scanClosureCaptureSources(
            driver,
            root_analysis,
            result,
            defining_source_context,
            template.module_idx,
            template.runtime_expr,
            closure_expr,
            switch (defining_source_context) {
                .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
                .root_expr, .provenance_expr, .template_expr => callable_inst_id,
            },
        );
        try seedClosureCaptureCallableValues(
            driver,
            result,
            defining_source_context,
            template.module_idx,
            closure_expr,
            callable_inst_id,
        );
    }

    try scanCirValueExpr(
        driver,
        root_analysis,
        result,
        thread,
        template.module_idx,
        template.body_expr,
    );

    switch (template.kind) {
        .closure => {
            const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
                .e_closure => |closure| closure,
                else => unreachable,
            };
            try finalizeCallableDefForCallableInst(
                driver,
                result,
                defining_source_context,
                template.module_idx,
                template.runtime_expr,
                closure_expr,
                callable_inst_id,
            );
        },
        .lambda, .hosted_lambda => {
            try updateCallableDefRuntimeValue(
                driver,
                result,
                callable_inst_id,
                .{
                    .source_context = callable_source_context,
                    .module_idx = template.module_idx,
                    .expr_idx = template.runtime_expr,
                },
                .{
                    .source_context = callable_source_context,
                    .module_idx = template.module_idx,
                    .expr_idx = template.body_expr,
                },
                &.{},
            );
        },
        .top_level_def => unreachable,
    }

    try setExprDirectCallable(driver,
        result,
        callable_source_context,
        template.module_idx,
        template.runtime_expr,
        callable_inst_id,
    );
}

fn sourceContextsEqual(lhs: SourceContext, rhs: SourceContext) bool {
    return switch (lhs) {
        .callable_inst => |lhs_id| switch (rhs) {
            .callable_inst => |rhs_id| lhs_id == rhs_id,
            else => false,
        },
        .root_expr => |lhs_root| switch (rhs) {
            .root_expr => |rhs_root| lhs_root.module_idx == rhs_root.module_idx and lhs_root.expr_idx == rhs_root.expr_idx,
            else => false,
        },
        .provenance_expr => |lhs_source| switch (rhs) {
            .provenance_expr => |rhs_source| lhs_source.module_idx == rhs_source.module_idx and lhs_source.expr_idx == rhs_source.expr_idx,
            else => false,
        },
        .template_expr => |lhs_template| switch (rhs) {
            .template_expr => |rhs_template| lhs_template.module_idx == rhs_template.module_idx and lhs_template.expr_idx == rhs_template.expr_idx,
            else => false,
        },
    };
}

fn callableInstSourceContext(callable_inst_id: CallableInstId) SourceContext {
    return .{ .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)) };
}

fn sourceContextHasCallableInst(source_context: SourceContext) bool {
    return switch (source_context) {
        .callable_inst => true,
        .root_expr, .provenance_expr, .template_expr => false,
    };
}

fn requireSourceContextCallableInst(source_context: SourceContext) CallableInstId {
    return switch (source_context) {
        .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
        .root_expr, .provenance_expr, .template_expr => std.debug.panic(
            "Lambdasolved invariant violated: source context {s} did not carry a callable inst",
            .{@tagName(source_context)},
        ),
    };
}

fn sourceContextCallableInstId(source_context: SourceContext) ?CallableInstId {
    return switch (source_context) {
        .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
        .root_expr, .provenance_expr, .template_expr => null,
    };
}

fn calleeUsesFirstClassCallableValuePath(expr: CIR.Expr) bool {
    return Lambdamono.calleeUsesFirstClassCallableValuePath(expr);
}

fn findDefByPatternInModule(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
    }
    return null;
}

fn dotCallUsesRuntimeReceiverExpr(module_env: *const ModuleEnv, receiver_expr_idx: CIR.Expr.Idx) bool {
    return switch (module_env.store.getExpr(receiver_expr_idx)) {
        .e_nominal, .e_nominal_external => false,
        else => true,
    };
}

fn dispatchMethodIdentForExprBinop(module_env: *const ModuleEnv, op: CIR.Expr.Binop.Op) ?Ident.Idx {
    return switch (op) {
        .@"and", .@"or" => null,
        .add => module_env.idents.plus,
        .sub => module_env.idents.minus,
        .mul => module_env.idents.times,
        .div => module_env.idents.div_by,
        .div_trunc => module_env.idents.div_trunc_by,
        .rem => module_env.idents.rem_by,
        .lt => module_env.idents.is_lt,
        .le => module_env.idents.is_lte,
        .gt => module_env.idents.is_gt,
        .ge => module_env.idents.is_gte,
        .eq, .ne => module_env.idents.is_eq,
    };
}

fn exprMonotypeOwnedByInvocation(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_call,
        .e_binop,
        .e_unary_minus,
        .e_type_var_dispatch,
        => true,
        .e_dot_access => |dot_expr| dot_expr.args != null,
        else => false,
    };
}

fn templateSourceContext(template: CallableTemplate) SourceContext {
    return .{ .template_expr = .{
        .module_idx = template.module_idx,
        .expr_idx = template.runtime_expr,
    } };
}

fn resolveRequiredLookupTarget(
    driver: anytype,
    module_env: *const ModuleEnv,
    lookup: @TypeOf(@as(CIR.Expr, undefined).e_lookup_required),
) ?RequiredLookupTarget {
    const app_idx = driver.app_module_idx orelse return null;
    const required_type = module_env.requires_types.get(lookup.requires_idx);
    const required_name = module_env.getIdent(required_type.ident);

    const app_env = driver.all_module_envs[app_idx];
    const app_ident = app_env.common.findIdent(required_name) orelse return null;
    const app_exports = app_env.store.sliceDefs(app_env.exports);
    for (app_exports) |def_idx| {
        const def = app_env.store.getDef(def_idx);
        const pat = app_env.store.getPattern(def.pattern);
        if (pat == .assign and pat.assign.ident.eql(app_ident)) {
            return .{
                .module_idx = app_idx,
                .def_idx = def_idx,
            };
        }
    }

    return null;
}

fn resolveImportedModuleIdx(
    driver: anytype,
    caller_env: *const ModuleEnv,
    import_idx: CIR.Import.Idx,
) ?u32 {
    if (caller_env.imports.getResolvedModule(import_idx)) |module_idx| {
        if (module_idx < driver.all_module_envs.len) return module_idx;
    }

    const import_pos = @intFromEnum(import_idx);
    if (import_pos >= caller_env.imports.imports.len()) return null;

    const import_name = caller_env.common.getString(caller_env.imports.imports.items.items[import_pos]);
    const base_name = identLastSegment(import_name);

    for (driver.all_module_envs, 0..) |module_env_entry, module_idx| {
        if (std.mem.eql(u8, module_env_entry.module_name, import_name) or
            std.mem.eql(u8, module_env_entry.module_name, base_name))
        {
            @constCast(&caller_env.imports).setResolvedModule(import_idx, @intCast(module_idx));
            return @intCast(module_idx);
        }
    }

    return null;
}

fn identLastSegment(ident: []const u8) []const u8 {
    var i = ident.len;
    while (i > 0) : (i -= 1) {
        if (ident[i - 1] == '.') return ident[i..];
    }
    return ident;
}

fn monotypeContainsNestedCallableValueShape(
    driver: anytype,
    result: anytype,
    monotype: ResolvedMonotype,
) std.mem.Allocator.Error!bool {
    if (monotype.isNone()) return false;
    var visited: std.AutoHashMapUnmanaged(u64, void) = .empty;
    defer visited.deinit(driver.allocator);
    return monotypeContainsNestedCallableValueShapeRec(
        driver,
        result,
        monotype.idx,
        monotype.module_idx,
        &visited,
    );
}

fn monotypeContainsNestedCallableValueShapeRec(
    driver: anytype,
    result: anytype,
    monotype_idx: Monotype.Idx,
    monotype_module_idx: u32,
    visited: *std.AutoHashMapUnmanaged(u64, void),
) std.mem.Allocator.Error!bool {
    if (monotype_idx.isNone()) return false;
    if (result.context_mono.monotype_store.isOpaque(monotype_idx)) return false;

    const visit_key = (@as(u64, monotype_module_idx) << 32) | @as(u64, @intFromEnum(monotype_idx));
    const gop = try visited.getOrPut(driver.allocator, visit_key);
    if (gop.found_existing) return false;

    return switch (result.context_mono.monotype_store.getMonotype(monotype_idx)) {
        .func => |func_mono| blk: {
            for (result.context_mono.monotype_store.getIdxSpan(func_mono.args)) |arg_mono| {
                if (try monotypeContainsNestedCallableValueShapeRec(driver, result, arg_mono, monotype_module_idx, visited)) {
                    break :blk true;
                }
            }
            break :blk try monotypeContainsNestedCallableValueShapeRec(
                driver,
                result,
                func_mono.ret,
                monotype_module_idx,
                visited,
            );
        },
        .box => |box_mono| monotypeContainsNestedCallableValueShapeRec(driver, result, box_mono.inner, monotype_module_idx, visited),
        .list => |list_mono| monotypeContainsNestedCallableValueShapeRec(driver, result, list_mono.elem, monotype_module_idx, visited),
        .tuple => |tuple_mono| blk: {
            for (result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                if (try monotypeContainsNestedCallableValueShapeRec(driver, result, elem_mono, monotype_module_idx, visited)) break :blk true;
            }
            break :blk false;
        },
        .record => |record_mono| blk: {
            for (result.context_mono.monotype_store.getFields(record_mono.fields)) |field| {
                if (try monotypeContainsNestedCallableValueShapeRec(driver, result, field.type_idx, monotype_module_idx, visited)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tag_union_mono| blk: {
            for (result.context_mono.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                for (result.context_mono.monotype_store.getIdxSpan(tag.payloads)) |payload_mono| {
                    if (try monotypeContainsNestedCallableValueShapeRec(driver, result, payload_mono, monotype_module_idx, visited)) break :blk true;
                }
            }
            break :blk false;
        },
        else => false,
    };
}

fn monotypeRequiresCallableParamSpec(
    driver: anytype,
    result: anytype,
    monotype: Monotype.Idx,
) std.mem.Allocator.Error!bool {
    var visited: std.AutoHashMapUnmanaged(Monotype.Idx, void) = .empty;
    defer visited.deinit(driver.allocator);
    return monotypeRequiresCallableParamSpecRec(driver, result, monotype, &visited);
}

fn monotypeRequiresCallableParamSpecRec(
    driver: anytype,
    result: anytype,
    monotype: Monotype.Idx,
    visited: *std.AutoHashMapUnmanaged(Monotype.Idx, void),
) std.mem.Allocator.Error!bool {
    if (monotype.isNone()) return false;
    if (result.context_mono.monotype_store.isOpaque(monotype)) return false;
    const gop = try visited.getOrPut(driver.allocator, monotype);
    if (gop.found_existing) return false;

    return switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .func => true,
        .box => |box_mono| monotypeRequiresCallableParamSpecRec(driver, result, box_mono.inner, visited),
        .list => |list_mono| monotypeRequiresCallableParamSpecRec(driver, result, list_mono.elem, visited),
        .tuple => |tuple_mono| blk: {
            for (result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                if (try monotypeRequiresCallableParamSpecRec(driver, result, elem_mono, visited)) break :blk true;
            }
            break :blk false;
        },
        .record => |record_mono| blk: {
            for (result.context_mono.monotype_store.getFields(record_mono.fields)) |field| {
                if (try monotypeRequiresCallableParamSpecRec(driver, result, field.type_idx, visited)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tag_union_mono| blk: {
            for (result.context_mono.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                for (result.context_mono.monotype_store.getIdxSpan(tag.payloads)) |payload_mono| {
                    if (try monotypeRequiresCallableParamSpecRec(driver, result, payload_mono, visited)) break :blk true;
                }
            }
            break :blk false;
        },
        else => false,
    };
}

fn directCallContainsErrorType(
    driver: anytype,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!bool {
    const module_env = driver.all_module_envs[module_idx];
    var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
    defer seen.deinit(driver.allocator);

    if (try typeVarContainsError(driver, &module_env.types, ModuleEnv.varFrom(call_expr_idx), &seen)) return true;
    if (try typeVarContainsError(driver, &module_env.types, ModuleEnv.varFrom(call_expr.func), &seen)) return true;

    for (module_env.store.sliceExpr(call_expr.args)) |arg_expr_idx| {
        if (try typeVarContainsError(driver, &module_env.types, ModuleEnv.varFrom(arg_expr_idx), &seen)) return true;
    }

    return false;
}

fn typeVarContainsError(
    driver: anytype,
    store_types: *const types.Store,
    var_: types.Var,
    seen: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!bool {
    const resolved = store_types.resolveVar(var_);
    if (seen.contains(resolved.var_)) return false;
    try seen.put(driver.allocator, resolved.var_, {});

    return switch (resolved.desc.content) {
        .err => true,
        .flex, .rigid => false,
        .alias => |alias| typeVarContainsError(driver, store_types, store_types.getAliasBackingVar(alias), seen),
        .structure => |flat_type| switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                for (store_types.sliceVars(func.args)) |arg_var| {
                    if (try typeVarContainsError(driver, store_types, arg_var, seen)) break :blk true;
                }
                break :blk try typeVarContainsError(driver, store_types, func.ret, seen);
            },
            .nominal_type => |nominal| blk: {
                for (store_types.sliceNominalArgs(nominal)) |arg_var| {
                    if (try typeVarContainsError(driver, store_types, arg_var, seen)) break :blk true;
                }
                break :blk try typeVarContainsError(
                    driver,
                    store_types,
                    store_types.getNominalBackingVar(nominal),
                    seen,
                );
            },
            .record => |record| recordTypeContainsError(driver, store_types, record, seen),
            .record_unbound => |fields_range| recordFieldsContainError(
                driver,
                store_types,
                store_types.getRecordFieldsSlice(fields_range).items(.var_),
                seen,
            ),
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (try typeVarContainsError(driver, store_types, elem_var, seen)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| tagUnionContainsError(driver, store_types, tag_union, seen),
            .empty_record, .empty_tag_union => false,
        },
    };
}

fn recordFieldsContainError(
    driver: anytype,
    store_types: *const types.Store,
    field_vars: []const types.Var,
    seen: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!bool {
    for (field_vars) |field_var| {
        if (try typeVarContainsError(driver, store_types, field_var, seen)) return true;
    }
    return false;
}

fn recordTypeContainsError(
    driver: anytype,
    store_types: *const types.Store,
    record: types.Record,
    seen: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!bool {
    var current_row = record;

    rows: while (true) {
        const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
        if (try recordFieldsContainError(driver, store_types, fields_slice.items(.var_), seen)) return true;

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .err => return true,
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .flex, .rigid => return false,
                .structure => |ext_flat| switch (ext_flat) {
                    .record => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .record_unbound => |fields_range| {
                        return recordFieldsContainError(
                            driver,
                            store_types,
                            store_types.getRecordFieldsSlice(fields_range).items(.var_),
                            seen,
                        );
                    },
                    .empty_record => return false,
                    else => return false,
                },
            }
        }
    }
}

fn tagUnionContainsError(
    driver: anytype,
    store_types: *const types.Store,
    tag_union: types.TagUnion,
    seen: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!bool {
    var current_row = tag_union;

    rows: while (true) {
        const tags_slice = store_types.getTagsSlice(current_row.tags);
        for (tags_slice.items(.args)) |payloads_range| {
            for (store_types.sliceVars(payloads_range)) |payload_var| {
                if (try typeVarContainsError(driver, store_types, payload_var, seen)) return true;
            }
        }

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .err => return true,
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .flex, .rigid => return false,
                .structure => |ext_flat| switch (ext_flat) {
                    .tag_union => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .empty_tag_union => return false,
                    else => return false,
                },
            }
        }
    }
}

fn specializeDispatchExactCallable(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    template_id: CallableTemplateId,
) std.mem.Allocator.Error!?CallableInstId {
    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try Lambdamono.appendDispatchActualArgsFromProgram(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        &actual_args,
    );
    const defining_source_context = requireTemplateDemandSourceContextForExpr(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        template_id,
    );
    const resolved_target = switch (expr) {
        .e_binop => |binop_expr| try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dispatchMethodIdentForExprBinop(driver.all_module_envs[module_idx], binop_expr.op).?),
        .e_dot_access => |dot_expr| try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dot_expr.field_name),
        .e_type_var_dispatch => |dispatch_expr| try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dispatch_expr.method_name),
        else => unreachable,
    };
    const resolved_template_id = try DispatchSolved.lookupResolvedDispatchTemplate(driver, result, module_idx, resolved_target);
    if (resolved_template_id != template_id) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: dispatch expr {d} in module {d} resolved target template mismatch",
                .{ @intFromEnum(expr_idx), module_idx },
            );
        }
        unreachable;
    }
    const exact_desired_fn_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, resolved_target.fn_var);
    if (exact_desired_fn_monotype.isNone()) return null;
    const fn_monotype = exact_desired_fn_monotype.idx;
    const fn_monotype_module_idx = exact_desired_fn_monotype.module_idx;

    var callable_param_specs = std.ArrayListUnmanaged(Lambdamono.CallableParamSpecEntry).empty;
    defer callable_param_specs.deinit(driver.allocator);
    const callable_param_specs_complete = try collectDirectCallCallableParamSpecs(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        fn_monotype,
        fn_monotype_module_idx,
        actual_args.items,
        &callable_param_specs,
    );
    if (!callable_param_specs_complete) return null;

    const callable_inst_id = try requireCallableInstWithCallableParamSpecs(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs.items,
    );
    return callable_inst_id;
}

fn ensureBuiltinBoxUnboxCallableInst(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    box_monotype: Monotype.Idx,
    inner_monotype: Monotype.Idx,
) std.mem.Allocator.Error!void {
    const source_env = driver.all_module_envs[source_module_idx];
    const common = ModuleEnv.CommonIdents.find(&source_env.common);
    const builtin_module_idx = TemplateCatalog.findModuleForOrigin(
        driver.all_module_envs,
        source_env,
        common.builtin_module,
    );
    const builtin_env = driver.all_module_envs[builtin_module_idx];
    const method_name = source_env.getIdent(common.builtin_box_unbox);
    const target_ident = builtin_env.common.findIdent(method_name) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: builtin box/unbox method '{s}' not found in builtin module {d}",
                .{ method_name, builtin_module_idx },
            );
        }
        unreachable;
    };
    const node_idx = builtin_env.getExposedNodeIndexById(target_ident) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: builtin box/unbox method '{s}' has no exposed def node in builtin module {d}",
                .{ method_name, builtin_module_idx },
            );
        }
        unreachable;
    };
    if (!builtin_env.store.isDefNode(node_idx)) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: builtin box/unbox method '{s}' exposed node {d} is not a def in builtin module {d}",
                .{ method_name, node_idx, builtin_module_idx },
            );
        }
        unreachable;
    }

    const template_id = result.template_catalog.requireExternalCallableTemplate(
        builtin_module_idx,
        node_idx,
        "builtin box/unbox specialization",
    );

    const args = try result.context_mono.monotype_store.addIdxSpan(driver.allocator, &.{box_monotype});
    const fn_monotype = try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .func = .{
        .args = args,
        .ret = inner_monotype,
        .effectful = false,
    } });

    _ = try requireCallableInst(
        driver,
        result,
        templateSourceContext(result.getCallableTemplate(template_id).*),
        template_id,
        fn_monotype,
        source_module_idx,
    );
}

fn resolveStrInspectHelperCallableInstsForTypeVar(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    type_var: types.Var,
) std.mem.Allocator.Error!void {
    var visiting: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
    defer visiting.deinit(driver.allocator);
    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, type_var, &visiting);
}

fn resolveStrInspectHelperCallableInstsForTypeVarWithSeen(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    type_var: types.Var,
    visiting: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];

    var resolved = module_env.types.resolveVar(type_var);
    while (resolved.desc.content == .alias) {
        resolved = module_env.types.resolveVar(module_env.types.getAliasBackingVar(resolved.desc.content.alias));
    }

    if (visiting.contains(resolved.var_)) return;
    try visiting.put(driver.allocator, resolved.var_, {});
    defer _ = visiting.remove(resolved.var_);

    if (resolved.desc.content == .structure) {
        switch (resolved.desc.content.structure) {
            .nominal_type => |nominal| {
                const common = ModuleEnv.CommonIdents.find(&module_env.common);
                const ident = nominal.ident.ident_idx;

                if (nominal.origin_module.eql(common.builtin_module)) {
                    if (DispatchSolved.builtinPrimForNominal(ident, common) != null) return;
                    if (ident.eql(common.bool)) return;
                    if (ident.eql(common.list)) {
                        const type_args = module_env.types.sliceNominalArgs(nominal);
                        if (type_args.len == 1) {
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, type_args[0], visiting);
                        }
                        return;
                    }
                    if (ident.eql(common.box)) {
                        const type_args = module_env.types.sliceNominalArgs(nominal);
                        const outer_mono = try cm.resolveTypeVarMonotype(driver, result, thread, module_idx, resolved.var_);
                        const outer_box = result.context_mono.monotype_store.getMonotype(outer_mono).box;
                        try ensureBuiltinBoxUnboxCallableInst(driver, result, module_idx, outer_mono, outer_box.inner);
                        if (type_args.len == 1) {
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, type_args[0], visiting);
                        }
                        return;
                    }
                }

                if (try DispatchSolved.lookupAssociatedMethodTemplate(driver, result, module_idx, nominal, module_env.idents.to_inspect)) |method_info| {
                    if (cm.resolveFuncTypeInStore(&method_info.target_env.types, method_info.type_var)) |resolved_func| {
                        if (!resolved_func.effectful) {
                            const param_vars = method_info.target_env.types.sliceVars(resolved_func.func.args);
                            if (param_vars.len == 1) {
                                var bindings = std.AutoHashMap(cm.BoundTypeVarKey, ResolvedMonotype).init(driver.allocator);
                                defer bindings.deinit();
                                var ordered_entries = std.ArrayList(cm.TypeSubstEntry).empty;
                                defer ordered_entries.deinit(driver.allocator);

                                const arg_mono = try cm.resolveTypeVarMonotype(driver, result, thread, module_idx, resolved.var_);
                                try cm.bindTypeVarMonotypes(
                                    driver,
                                    result,
                                    method_info.module_idx,
                                    &method_info.target_env.types,
                                    &bindings,
                                    &ordered_entries,
                                    param_vars[0],
                                    arg_mono,
                                    module_idx,
                                );

                                const method_func_mono = try cm.resolveTypeVarMonotypeWithBindings(
                                    driver,
                                    result,
                                    method_info.module_idx,
                                    &method_info.target_env.types,
                                    method_info.type_var,
                                    &bindings,
                                );
                                if (!method_func_mono.isNone()) {
                                    _ = try requireCallableInst(
                                        driver,
                                        result,
                                        templateSourceContext(result.getCallableTemplate(method_info.template_id).*),
                                        method_info.template_id,
                                        method_func_mono,
                                        method_info.module_idx,
                                    );

                                    const method_func = switch (result.context_mono.monotype_store.getMonotype(method_func_mono)) {
                                        .func => |func| func,
                                        else => unreachable,
                                    };
                                    const ret_mono = result.context_mono.monotype_store.getMonotype(method_func.ret);
                                    switch (ret_mono) {
                                        .func => {},
                                        else => try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, method_func.ret),
                                    }
                                }
                            }
                        }
                    }
                    return;
                }
            },
            .record => |record| {
                try resolveStrInspectHelperCallableInstsForRecordType(driver, result, thread, module_idx, &module_env.types, record, visiting);
                return;
            },
            .record_unbound => |fields_range| {
                for (module_env.types.getRecordFieldsSlice(fields_range).items(.var_)) |field_var| {
                    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, field_var, visiting);
                }
                return;
            },
            .tuple => |tuple| {
                for (module_env.types.sliceVars(tuple.elems)) |elem_var| {
                    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, elem_var, visiting);
                }
                return;
            },
            .tag_union => |tag_union| {
                try resolveStrInspectHelperCallableInstsForTagUnionType(driver, result, thread, module_idx, &module_env.types, tag_union, visiting);
                return;
            },
            .empty_record, .empty_tag_union => return,
            else => {},
        }
    }

    try resolveStrInspectHelperCallableInstsForMonotype(
        driver,
        result,
        module_idx,
        try cm.resolveTypeVarMonotype(driver, result, thread, module_idx, resolved.var_),
    );
}

fn resolveStrInspectHelperCallableInstsForRecordType(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    store_types: *const types.Store,
    record: types.Record,
    visiting: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!void {
    var current_row = record;

    rows: while (true) {
        const fields = store_types.getRecordFieldsSlice(current_row.fields);
        for (fields.items(.var_)) |field_var| {
            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, field_var, visiting);
        }

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .record => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .record_unbound => |fields_range| {
                        const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                        for (ext_fields.items(.var_)) |field_var| {
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, field_var, visiting);
                        }
                        break :rows;
                    },
                    .empty_record => break :rows,
                    else => break :rows,
                },
                .flex, .rigid, .err => break :rows,
            }
        }
    }
}

fn resolveStrInspectHelperCallableInstsForTagUnionType(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    store_types: *const types.Store,
    tag_union: types.TagUnion,
    visiting: *std.AutoHashMapUnmanaged(types.Var, void),
) std.mem.Allocator.Error!void {
    var current_row = tag_union;

    rows: while (true) {
        const tags = store_types.getTagsSlice(current_row.tags);
        for (tags.items(.args)) |args_range| {
            for (store_types.sliceVars(args_range)) |payload_var| {
                try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, result, thread, module_idx, payload_var, visiting);
            }
        }

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .tag_union => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .empty_tag_union => break :rows,
                    else => break :rows,
                },
                .flex, .rigid, .err => break :rows,
            }
        }
    }
}

fn resolveStrInspectHelperCallableInstsForMonotype(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    monotype: Monotype.Idx,
) std.mem.Allocator.Error!void {
    if (monotype.isNone()) return;

    switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .unit, .prim => {},
        .list => |list_mono| try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, list_mono.elem),
        .box => |box_mono| {
            try ensureBuiltinBoxUnboxCallableInst(driver, result, module_idx, monotype, box_mono.inner);
            try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, box_mono.inner);
        },
        .tuple => |tuple_mono| {
            var elem_i: usize = 0;
            while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, elem_mono);
            }
        },
        .func => {},
        .record => |record_mono| {
            var field_i: usize = 0;
            while (field_i < record_mono.fields.len) : (field_i += 1) {
                const field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, field_i);
                try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, field.type_idx);
            }
        },
        .tag_union => |tag_union_mono| {
            var tag_i: usize = 0;
            while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                const tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                var payload_i: usize = 0;
                while (payload_i < tag.payloads.len) : (payload_i += 1) {
                    const payload_mono = result.context_mono.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                    try resolveStrInspectHelperCallableInstsForMonotype(driver, result, module_idx, payload_mono);
                }
            }
        },
        .recursive_placeholder => unreachable,
    }
}

fn readExprCallableValue(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    return result.getExprCallableValue(source_context, module_idx, expr_idx);
}

fn readCallableParamValue(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableValue {
    return result.getPatternCallableValue(source_context, module_idx, pattern_idx);
}

fn readExprCallSite(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallSite {
    return result.getExprCallSite(source_context, module_idx, expr_idx);
}

const CallableVariantBuilder = struct {
    variants: std.ArrayListUnmanaged(CallableInstId),

    fn init() CallableVariantBuilder {
        return .{ .variants = .empty };
    }

    fn deinit(self: *CallableVariantBuilder, allocator: std.mem.Allocator) void {
        self.variants.deinit(allocator);
    }

    fn includeCallableInst(
        self: *CallableVariantBuilder,
        driver: anytype,
        callable_inst_id: CallableInstId,
    ) std.mem.Allocator.Error!void {
        for (self.variants.items) |existing_callable_inst_id| {
            if (existing_callable_inst_id == callable_inst_id) return;
        }
        try self.variants.append(driver.allocator, callable_inst_id);
    }

    fn includeCallableValue(
        self: *CallableVariantBuilder,
        driver: anytype,
        result: anytype,
        callable_value: CallableValue,
    ) std.mem.Allocator.Error!void {
        for (result.lambdamono.getCallableValueVariants(callable_value)) |callable_inst_id| {
            try self.includeCallableInst(driver, callable_inst_id);
        }
    }

    fn includeCallSite(
        self: *CallableVariantBuilder,
        driver: anytype,
        result: anytype,
        call_site: CallSite,
    ) std.mem.Allocator.Error!void {
        for (result.lambdamono.getCallSiteVariants(call_site)) |callable_inst_id| {
            try self.includeCallableInst(driver, callable_inst_id);
        }
    }

    fn finishValue(
        self: *CallableVariantBuilder,
        driver: anytype,
        result: anytype,
    ) std.mem.Allocator.Error!?CallableValue {
        if (self.variants.items.len == 0) return null;
        std.mem.sortUnstable(CallableInstId, self.variants.items, {}, struct {
            fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                return @intFromEnum(lhs) < @intFromEnum(rhs);
            }
        }.lessThan);
        if (self.variants.items.len == 1) {
            return .{ .direct = self.variants.items[0] };
        }
        return .{ .packed_fn = try result.lambdamono.makePackedFnForCallableInsts(
            driver.allocator,
            driver.all_module_envs,
            driver.current_module_idx,
            &result.context_mono,
            self.variants.items,
        ) };
    }

    fn finishCallSite(
        self: *CallableVariantBuilder,
        driver: anytype,
        result: anytype,
    ) std.mem.Allocator.Error!?CallSite {
        if (self.variants.items.len == 0) return null;
        std.mem.sortUnstable(CallableInstId, self.variants.items, {}, struct {
            fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                return @intFromEnum(lhs) < @intFromEnum(rhs);
            }
        }.lessThan);
        if (self.variants.items.len == 1) {
            return .{ .direct = self.variants.items[0] };
        }
        return .{ .indirect_call = try result.lambdamono.makeIndirectCallForCallableInsts(
            driver.allocator,
            driver.all_module_envs,
            driver.current_module_idx,
            &result.context_mono,
            self.variants.items,
        ) };
    }
};

fn getContextPatternMonotypeInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?ResolvedMonotype {
    return result.getContextPatternMonotype(
        callableInstSourceContext(context_callable_inst),
        module_idx,
        pattern_idx,
    );
}

fn getContextPatternMonotypeForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?ResolvedMonotype {
    return result.getContextPatternMonotype(thread.requireSourceContext(), module_idx, pattern_idx);
}

fn resolveBoundPatternCallableValueInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableValue {
    return readCallableParamValue(
        result,
        callableInstSourceContext(context_callable_inst),
        module_idx,
        pattern_idx,
    );
}

fn resolveBoundPatternCallableValueForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableValue {
    return readCallableParamValue(result, thread.requireSourceContext(), module_idx, pattern_idx);
}

fn resolveBoundPatternCallableInstInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableInstId {
    const callable_value = resolveBoundPatternCallableValueInContext(
        result,
        context_callable_inst,
        module_idx,
        pattern_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

fn resolveBoundPatternCallableInstForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableInstId {
    const callable_value = resolveBoundPatternCallableValueForThread(
        result,
        thread,
        module_idx,
        pattern_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

fn getExprCallableValueInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    return readExprCallableValue(
        result,
        callableInstSourceContext(context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn getExprCallableValueForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    return readExprCallableValue(result, thread.requireSourceContext(), module_idx, expr_idx);
}

fn getExprCallableInstInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const callable_value = getExprCallableValueInContext(
        result,
        context_callable_inst,
        module_idx,
        expr_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

fn getExprCallableInstForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const callable_value = getExprCallableValueForThread(
        result,
        thread,
        module_idx,
        expr_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

fn getCallSiteInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallSite {
    return readExprCallSite(
        result,
        callableInstSourceContext(context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn getCallSiteForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallSite {
    return readExprCallSite(result, thread.requireSourceContext(), module_idx, expr_idx);
}

fn getCallSiteForSourceContext(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallSite {
    return readExprCallSite(result, source_context, module_idx, expr_idx);
}

fn getCallSiteCallableInstInContext(
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const call_site = getCallSiteInContext(result, context_callable_inst, module_idx, expr_idx) orelse return null;
    return Lambdamono.exactCallableInstFromCallSite(call_site);
}

fn getCallSiteCallableInstForThread(
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const call_site = getCallSiteForThread(result, thread, module_idx, expr_idx) orelse return null;
    return Lambdamono.exactCallableInstFromCallSite(call_site);
}

fn resolveCallableParamProjectionRef(
    driver: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    out: *std.ArrayListUnmanaged(Lambdamono.CallableParamProjection),
) ?CIR.Pattern.Idx {
    const module_env = driver.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| lookup.pattern_idx,
        .e_dot_access => |dot_expr| blk: {
            if (dot_expr.args != null) break :blk null;
            const base_pattern = resolveCallableParamProjectionRef(driver, module_idx, dot_expr.receiver, out) orelse break :blk null;
            out.append(driver.allocator, .{ .field = .{
                .module_idx = module_idx,
                .ident = dot_expr.field_name,
            } }) catch return null;
            break :blk base_pattern;
        },
        .e_tuple_access => |tuple_access| blk: {
            const base_pattern = resolveCallableParamProjectionRef(driver, module_idx, tuple_access.tuple, out) orelse break :blk null;
            out.append(driver.allocator, .{ .tuple_elem = tuple_access.elem_index }) catch return null;
            break :blk base_pattern;
        },
        .e_block => |block| resolveCallableParamProjectionRef(driver, module_idx, block.final_expr, out),
        .e_dbg => |dbg_expr| resolveCallableParamProjectionRef(driver, module_idx, dbg_expr.expr, out),
        .e_expect => |expect_expr| resolveCallableParamProjectionRef(driver, module_idx, expect_expr.body, out),
        .e_return => |return_expr| resolveCallableParamProjectionRef(driver, module_idx, return_expr.expr, out),
        else => null,
    };
}

fn callableParamProjectionSeqEqual(
    driver: anytype,
    lhs: []const Lambdamono.CallableParamProjection,
    rhs: []const Lambdamono.CallableParamProjection,
) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_proj, rhs_proj| {
        switch (lhs_proj) {
            .field => |lhs_field| switch (rhs_proj) {
                .field => |rhs_field| {
                    if (!lhs_field.textEqual(driver.all_module_envs, rhs_field)) return false;
                },
                else => return false,
            },
            .tuple_elem => |lhs_index| switch (rhs_proj) {
                .tuple_elem => |rhs_index| if (lhs_index != rhs_index) return false,
                else => return false,
            },
            .tag_payload => |lhs_payload| switch (rhs_proj) {
                .tag_payload => |rhs_payload| if (lhs_payload.payload_index != rhs_payload.payload_index or !lhs_payload.tag_name.textEqual(driver.all_module_envs, rhs_payload.tag_name)) return false,
                else => return false,
            },
            .list_elem => |lhs_index| switch (rhs_proj) {
                .list_elem => |rhs_index| if (lhs_index != rhs_index) return false,
                else => return false,
            },
        }
    }
    return true;
}

fn getCallableParamSpecCallableValueForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    if (!sourceContextHasCallableInst(source_context)) return null;
    const resolved_context_callable_inst = requireSourceContextCallableInst(source_context);

    var projections: std.ArrayListUnmanaged(Lambdamono.CallableParamProjection) = .empty;
    defer projections.deinit(driver.allocator);
    const pattern_idx = resolveCallableParamProjectionRef(
        driver,
        module_idx,
        expr_idx,
        &projections,
    ) orelse return null;

    const context_callable = result.getCallableInst(resolved_context_callable_inst);
    const template = result.getCallableTemplate(context_callable.template);
    const param_patterns = driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);

    const param_index = for (param_patterns, 0..) |param_pattern_idx, idx| {
        if (param_pattern_idx == pattern_idx) break idx;
    } else return null;

    for (result.lambdamono.getCallableParamSpecEntries(result.getCallableInst(resolved_context_callable_inst).callable_param_specs)) |spec| {
        if (spec.param_index != param_index) continue;
        if (!callableParamProjectionSeqEqual(
            driver,
            result.lambdamono.getCallableParamProjectionEntries(spec.projections),
            projections.items,
        )) continue;
        return spec.callable_value;
    }

    return null;
}

fn getCallableParamSpecCallableValueInContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    return getCallableParamSpecCallableValueForSourceContext(
        driver,
        result,
        callableInstSourceContext(context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn getCallableParamSpecCallableValueForThread(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    return getCallableParamSpecCallableValueForSourceContext(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
    );
}

fn getValueExprCallableValueInContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_lookup_local => |lookup| {
            if (resolveBoundPatternCallableValueInContext(result, context_callable_inst, module_idx, lookup.pattern_idx)) |callable_value| {
                return callable_value;
            }
        },
        else => {},
    }

    if (getCallableParamSpecCallableValueInContext(driver, result, context_callable_inst, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    return getExprCallableValueInContext(result, context_callable_inst, module_idx, expr_idx);
}

fn getValueExprCallableValueForThread(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_lookup_local => |lookup| {
            if (resolveBoundPatternCallableValueForThread(result, thread, module_idx, lookup.pattern_idx)) |callable_value| {
                return callable_value;
            }
        },
        else => {},
    }

    if (getCallableParamSpecCallableValueForThread(driver, result, thread, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    return getExprCallableValueForThread(result, thread, module_idx, expr_idx);
}

fn getValueExprCallableInstForThread(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const callable_value = getValueExprCallableValueForThread(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

pub fn getValueExprCallableValueForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    if (sourceContextHasCallableInst(source_context)) {
        return getValueExprCallableValueInContext(
            driver,
            result,
            requireSourceContextCallableInst(source_context),
            module_idx,
            expr_idx,
        );
    }

    if (getCallableParamSpecCallableValueForSourceContext(driver, result, source_context, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    if (readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (source.projections.isEmpty()) {
            if (sourceContextHasCallableInst(source.source_context)) {
                return getValueExprCallableValueInContext(
                    driver,
                    result,
                    requireSourceContextCallableInst(source.source_context),
                    source.module_idx,
                    source.expr_idx,
                );
            }
            if (getCallableParamSpecCallableValueForSourceContext(
                driver,
                result,
                source.source_context,
                source.module_idx,
                source.expr_idx,
            )) |callable_value| {
                return callable_value;
            }
            if (readExprCallableValue(result, source.source_context, source.module_idx, source.expr_idx)) |callable_value| {
                return callable_value;
            }
        }
    }

    return null;
}

pub fn resolveExprRefCallableValue(
    driver: anytype,
    result: anytype,
    expr_ref: ExprRef,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    return resolveProjectedExprCallableValue(
        driver,
        result,
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        result.lambdamono.getValueProjectionEntries(expr_ref.projections),
        visiting,
    );
}

pub fn resolveProjectedExprCallableValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const ValueProjection.Projection,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    if (projections.len == 0) {
        try realizeStructuredExprCallableSemantics(
            driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            visiting,
        );
        return getValueExprCallableValueForSourceContext(driver, result, source_context, module_idx, expr_idx);
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        const combined_projections = try Lambdamono.appendValueProjectionEntries(
            driver,
            result,
            origin.projections,
            projections,
        );
        return resolveProjectedExprCallableValue(
            driver,
            result,
            origin.source_context,
            origin.module_idx,
            origin.expr_idx,
            result.lambdamono.getValueProjectionEntries(combined_projections),
            visiting,
        );
    }

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const projection = projections[0];
    const rest = projections[1..];
    switch (projection) {
        .field => |field_name| switch (expr) {
            .e_record => |record_expr| {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (!cm.identsStructurallyEqualAcrossModules(
                        driver.all_module_envs,
                        module_idx,
                        field.name,
                        field_name.module_idx,
                        field_name.ident,
                    )) continue;
                    return resolveProjectedExprCallableValue(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        field.value,
                        rest,
                        visiting,
                    );
                }
                return null;
            },
            else => return null,
        },
        .tuple_elem => |elem_index| switch (expr) {
            .e_tuple => |tuple_expr| {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (builtin.mode == .Debug and elem_index >= elems.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: tuple projection elem_index {d} out of bounds for expr {d} in module {d}",
                        .{ elem_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                return resolveProjectedExprCallableValue(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    visiting,
                );
            },
            else => return null,
        },
        .tag_payload => |payload| switch (expr) {
            .e_tag => |tag_expr| {
                if (!cm.identsStructurallyEqualAcrossModules(
                    driver.all_module_envs,
                    module_idx,
                    tag_expr.name,
                    payload.tag_name.module_idx,
                    payload.tag_name.ident,
                )) return null;
                const args = module_env.store.sliceExpr(tag_expr.args);
                if (builtin.mode == .Debug and payload.payload_index >= args.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: tag payload projection index {d} out of bounds for expr {d} in module {d}",
                        .{ payload.payload_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                return resolveProjectedExprCallableValue(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    args[payload.payload_index],
                    rest,
                    visiting,
                );
            },
            else => return null,
        },
        .list_elem => |elem_index| switch (expr) {
            .e_list => |list_expr| {
                const elems = module_env.store.sliceExpr(list_expr.elems);
                if (builtin.mode == .Debug and elem_index >= elems.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: list projection elem_index {d} out of bounds for expr {d} in module {d}",
                        .{ elem_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                return resolveProjectedExprCallableValue(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    visiting,
                );
            },
            else => return null,
        },
    }
}

pub fn copyExprCallableValueFromRef(
    driver: anytype,
    result: anytype,
    target_source_context: SourceContext,
    target_module_idx: u32,
    target_expr_idx: CIR.Expr.Idx,
    maybe_pattern_idx: ?CIR.Pattern.Idx,
    source: ExprRef,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    var resolved_callable_value = try resolveExprRefCallableValue(driver, result, source, visiting);
    if (resolved_callable_value == null and source.projections.isEmpty()) {
        if (result.getExprTemplateId(source.source_context, source.module_idx, source.expr_idx)) |template_id| {
            const target_fn_monotype = (try cm.resolveExprMonotypeResolved(
                driver,
                result,
                SemanticThread.trackedThread(target_source_context),
                target_module_idx,
                target_expr_idx,
            )) orelse return;
            {
                const materialize_failure = try materializeExprCallableValueWithKnownFnMonotype(
                    driver,
                    result,
                    source.source_context,
                    source.module_idx,
                    source.expr_idx,
                    template_id,
                    target_fn_monotype,
                );
                resolved_callable_value = try resolveExprRefCallableValue(driver, result, source, visiting);
                if (resolved_callable_value == null and std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: callable source expr {d} in module {d} under source context {s} failed to materialize callable value for target expr {d} in module {d} under target context {s} using exact fn monotype {d}@{d}; reason={?s} template={d} target_tag={s}",
                        .{
                            @intFromEnum(source.expr_idx),
                            source.module_idx,
                            @tagName(source.source_context),
                            @intFromEnum(target_expr_idx),
                            target_module_idx,
                            @tagName(target_source_context),
                            @intFromEnum(target_fn_monotype.idx),
                            target_fn_monotype.module_idx,
                            if (materialize_failure) |failure| @tagName(failure) else null,
                            @intFromEnum(template_id),
                            @tagName(driver.all_module_envs[target_module_idx].store.getExpr(target_expr_idx)),
                        },
                    );
                }
            }
        }
    }
    const callable_value = resolved_callable_value orelse return;

    try setExprCallableValue(
        driver,
        result,
        target_source_context,
        target_module_idx,
        target_expr_idx,
        callable_value,
    );
    if (maybe_pattern_idx) |pattern_idx| {
        try setCallableParamValue(
            driver,
            result,
            target_source_context,
            target_module_idx,
            pattern_idx,
            callable_value,
        );
    }
}

fn bindCurrentPatternFromResolvedMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    resolved_mono: ResolvedMonotype,
) std.mem.Allocator.Error!void {
    if (resolved_mono.isNone()) return;
    if (result.context_mono.monotype_store.getMonotype(resolved_mono.idx) == .func) return;
    try cm.recordTypeVarMonotypeForThread(
        driver,
        result,
        thread,
        module_idx,
        ModuleEnv.varFrom(pattern_idx),
        resolved_mono.idx,
        resolved_mono.module_idx,
    );
    try cm.mergeContextPatternMonotype(
        driver,
        result,
        cm.Result.contextPatternKey(thread.requireSourceContext(), module_idx, pattern_idx),
        resolved_mono,
    );

    const module_env = driver.all_module_envs[module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign,
        .underscore,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .runtime_error,
        => {},
        .as => |as_pat| {
            try bindCurrentPatternFromResolvedMonotype(
                driver,
                result,
                thread,
                module_idx,
                as_pat.pattern,
                resolved_mono,
            );
        },
        .nominal => |nominal_pat| {
            try bindCurrentPatternFromResolvedMonotype(
                driver,
                result,
                thread,
                module_idx,
                nominal_pat.backing_pattern,
                resolved_mono,
            );
        },
        .nominal_external => |nominal_pat| {
            try bindCurrentPatternFromResolvedMonotype(
                driver,
                result,
                thread,
                module_idx,
                nominal_pat.backing_pattern,
                resolved_mono,
            );
        },
        .applied_tag => |tag_pat| {
            const mono_tags = switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
                .tag_union => |tag_union| result.context_mono.monotype_store.getTags(tag_union.tags),
                else => return,
            };
            const tag_idx = cm.tagIndexByNameInSpan(
                driver,
                result,
                module_idx,
                tag_pat.name,
                resolved_mono.module_idx,
                switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
                    .tag_union => |tag_union| tag_union.tags,
                    else => unreachable,
                },
            );
            const mono_tag = mono_tags[tag_idx];
            const mono_payloads = result.context_mono.monotype_store.getIdxSpan(mono_tag.payloads);
            const payload_patterns = module_env.store.slicePatterns(tag_pat.args);
            if (payload_patterns.len != mono_payloads.len) unreachable;

            for (payload_patterns, mono_payloads) |payload_pattern_idx, payload_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    payload_pattern_idx,
                    cm.resolvedMonotype(payload_mono, resolved_mono.module_idx),
                );
            }
        },
        .record_destructure => |record_pat| {
            const mono_fields = switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
                .record => |record_mono| result.context_mono.monotype_store.getFields(record_mono.fields),
                .unit => &.{},
                else => return,
            };
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required, .SubPattern => |sub_pattern_idx| {
                        const field_idx = cm.recordFieldIndexByName(
                            driver,
                            module_idx,
                            destruct.label,
                            resolved_mono.module_idx,
                            mono_fields,
                        );
                        try bindCurrentPatternFromResolvedMonotype(
                            driver,
                            result,
                            thread,
                            module_idx,
                            sub_pattern_idx,
                            cm.resolvedMonotype(mono_fields[field_idx].type_idx, resolved_mono.module_idx),
                        );
                    },
                    .Rest => {},
                }
            }
        },
        .list => |list_pat| {
            const elem_mono = switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
                .list => |list_mono| list_mono.elem,
                else => return,
            };
            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern_idx| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    elem_pattern_idx,
                    cm.resolvedMonotype(elem_mono, resolved_mono.module_idx),
                );
            }
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try bindCurrentPatternFromResolvedMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        rest_pattern_idx,
                        resolved_mono,
                    );
                }
            }
        },
        .tuple => |tuple_pat| {
            const mono_elems = switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
                .tuple => |tuple_mono| result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems),
                else => return,
            };
            const elem_patterns = module_env.store.slicePatterns(tuple_pat.patterns);
            if (elem_patterns.len != mono_elems.len) unreachable;

            for (elem_patterns, mono_elems) |elem_pattern_idx, elem_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    elem_pattern_idx,
                    cm.resolvedMonotype(elem_mono, resolved_mono.module_idx),
                );
            }
        },
    }
}

fn bindMatchBranchPatternsFromCondMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    match_expr: @TypeOf(@as(CIR.Expr, undefined).e_match),
    cond_mono: ResolvedMonotype,
) std.mem.Allocator.Error!void {
    if (cond_mono.isNone()) return;

    const module_env = driver.all_module_envs[module_idx];
    const branches = module_env.store.sliceMatchBranches(match_expr.branches);
    for (branches) |branch_idx| {
        const branch = module_env.store.getMatchBranch(branch_idx);
        for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
            try bindCurrentPatternFromResolvedMonotype(
                driver,
                result,
                thread,
                module_idx,
                branch_pattern.pattern,
                cond_mono,
            );
        }
    }
}

fn recordMatchBranchPatternSources(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    match_expr: @TypeOf(@as(CIR.Expr, undefined).e_match),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const cond_source = ExprRef{
        .source_context = thread.requireSourceContext(),
        .module_idx = module_idx,
        .expr_idx = match_expr.cond,
        .projections = .empty(),
    };
    const branches = module_env.store.sliceMatchBranches(match_expr.branches);
    for (branches) |branch_idx| {
        const branch = module_env.store.getMatchBranch(branch_idx);
        for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
            try recordContextPatternSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                branch_pattern.pattern,
                cond_source,
            );
        }
    }
}

fn propagateDemandedValueMonotypeToValueExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const demanded = cm.resolvedMonotype(monotype, monotype_module_idx);
    try recordExprMonotypeIfUnsetOrEqualForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        demanded.idx,
        demanded.module_idx,
    );

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (!(sourceContextsEqual(source.source_context, source_context) and
            source.module_idx == module_idx and
            source.expr_idx == expr_idx and
            source.projections.isEmpty()))
        {
            try propagateDemandedValueMonotypeToExprRef(
                driver,
                result,
                source,
                demanded.idx,
                demanded.module_idx,
            );
        }
    }

    switch (expr) {
        .e_record => |record_expr| switch (result.context_mono.monotype_store.getMonotype(demanded.idx)) {
            .record => |record_mono| {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    const mono_field_idx = cm.recordFieldIndexByNameInSpan(
                        driver,
                        result,
                        module_idx,
                        field.name,
                        demanded.module_idx,
                        record_mono.fields,
                    );
                    const mono_field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, mono_field_idx);
                    try propagateDemandedValueMonotypeToValueExpr(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        field.value,
                        mono_field.type_idx,
                        demanded.module_idx,
                    );
                }
            },
            .unit => {},
            else => {},
        },
        .e_tuple => |tuple_expr| switch (result.context_mono.monotype_store.getMonotype(demanded.idx)) {
            .tuple => |tuple_mono| {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                const demanded_elems = result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems);
                if (std.debug.runtime_safety and elems.len != demanded_elems.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: demanded tuple monotype arity {d} did not match tuple expr arity {d} for expr {d} in module {d}",
                        .{ demanded_elems.len, elems.len, @intFromEnum(expr_idx), module_idx },
                    );
                }
                for (elems, demanded_elems) |elem_expr_idx, elem_mono| {
                    try propagateDemandedValueMonotypeToValueExpr(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        elem_expr_idx,
                        elem_mono,
                        demanded.module_idx,
                    );
                }
            },
            else => {},
        },
        .e_tag => |tag_expr| switch (result.context_mono.monotype_store.getMonotype(demanded.idx)) {
            .tag_union => |tag_union_mono| {
                const tag_idx = cm.tagIndexByNameInSpan(
                    driver,
                    result,
                    module_idx,
                    tag_expr.name,
                    demanded.module_idx,
                    tag_union_mono.tags,
                );
                const mono_tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_idx);
                const payload_monos = result.context_mono.monotype_store.getIdxSpan(mono_tag.payloads);
                const payload_exprs = module_env.store.sliceExpr(tag_expr.args);
                if (std.debug.runtime_safety and payload_exprs.len != payload_monos.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: demanded tag payload arity {d} did not match tag expr arity {d} for expr {d} in module {d}",
                        .{ payload_monos.len, payload_exprs.len, @intFromEnum(expr_idx), module_idx },
                    );
                }
                for (payload_exprs, payload_monos) |payload_expr_idx, payload_mono| {
                    try propagateDemandedValueMonotypeToValueExpr(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        payload_expr_idx,
                        payload_mono,
                        demanded.module_idx,
                    );
                }
            },
            else => {},
        },
        .e_list => |list_expr| switch (result.context_mono.monotype_store.getMonotype(demanded.idx)) {
            .list => |list_mono| {
                for (module_env.store.sliceExpr(list_expr.elems)) |elem_expr_idx| {
                    try propagateDemandedValueMonotypeToValueExpr(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        elem_expr_idx,
                        list_mono.elem,
                        demanded.module_idx,
                    );
                }
            },
            else => {},
        },
        else => {},
    }
}

fn propagateDemandedValueMonotypeToExprRef(
    driver: anytype,
    result: anytype,
    expr_ref: ExprRef,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    return propagateDemandedValueMonotypeToExprRefProjections(
        driver,
        result,
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        result.lambdamono.getValueProjectionEntries(expr_ref.projections),
        monotype,
        monotype_module_idx,
    );
}

fn propagateDemandedValueMonotypeToExprRefProjections(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const ValueProjection,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    if (projections.len == 0) {
        return propagateDemandedValueMonotypeToValueExpr(
            driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const rest = projections[1..];

    switch (projections[0]) {
        .field => |field_name| switch (expr) {
            .e_record => |record_expr| {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (field.name.eql(field_name.ident)) {
                        return propagateDemandedValueMonotypeToExprRefProjections(
                            driver,
                            result,
                            source_context,
                            module_idx,
                            field.value,
                            rest,
                            monotype,
                            monotype_module_idx,
                        );
                    }
                }
                std.debug.panic(
                    "Lambdasolved invariant violated: field projection demand for expr {d} in module {d} could not find field {d}",
                    .{ @intFromEnum(expr_idx), module_idx, @as(u32, @bitCast(field_name.ident)) },
                );
            },
            else => return requireProjectedExprDemandMatchesResolvedMonotype(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                projections,
                monotype,
                monotype_module_idx,
            ),
        },
        .tuple_elem => |elem_index| switch (expr) {
            .e_tuple => |tuple_expr| {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (elem_index >= elems.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: tuple projection demand out of bounds for expr {d} elem {d} in module {d}",
                        .{ @intFromEnum(expr_idx), elem_index, module_idx },
                    );
                }
                return propagateDemandedValueMonotypeToExprRefProjections(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    monotype,
                    monotype_module_idx,
                );
            },
            else => return requireProjectedExprDemandMatchesResolvedMonotype(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                projections,
                monotype,
                monotype_module_idx,
            ),
        },
        .tag_payload => |payload| switch (expr) {
            .e_tag => |tag_expr| {
                const expr_tag_name: @TypeOf(payload.tag_name) = .{
                    .module_idx = module_idx,
                    .ident = tag_expr.name,
                };
                if (!expr_tag_name.textEqual(driver.all_module_envs, payload.tag_name)) {
                    return;
                }
                const args = module_env.store.sliceExpr(tag_expr.args);
                if (payload.payload_index >= args.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: tag projection demand out of bounds for expr {d} payload {d} in module {d}",
                        .{ @intFromEnum(expr_idx), payload.payload_index, module_idx },
                    );
                }
                return propagateDemandedValueMonotypeToExprRefProjections(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    args[payload.payload_index],
                    rest,
                    monotype,
                    monotype_module_idx,
                );
            },
            else => return requireProjectedExprDemandMatchesResolvedMonotype(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                projections,
                monotype,
                monotype_module_idx,
            ),
        },
        .list_elem => |elem_index| switch (expr) {
            .e_list => |list_expr| {
                const elems = module_env.store.sliceExpr(list_expr.elems);
                if (elem_index >= elems.len) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: list projection demand out of bounds for expr {d} elem {d} in module {d}",
                        .{ @intFromEnum(expr_idx), elem_index, module_idx },
                    );
                }
                return propagateDemandedValueMonotypeToExprRefProjections(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    monotype,
                    monotype_module_idx,
                );
            },
            else => return requireProjectedExprDemandMatchesResolvedMonotype(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                projections,
                monotype,
                monotype_module_idx,
            ),
        },
    }
}

fn requireProjectedExprDemandMatchesResolvedMonotype(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const ValueProjection,
    demanded_monotype: Monotype.Idx,
    demanded_module_idx: u32,
) std.mem.Allocator.Error!void {
    var projected = try cm.requireRecordedExprMonotypeForSourceContext(
        driver,
        result,
        SemanticThread.trackedThread(source_context),
        source_context,
        module_idx,
        expr_idx,
    );
    for (projections) |projection| {
        projected = try cm.projectResolvedMonotypeByValueProjection(driver, result, projected, projection);
    }
    if (!try cm.monotypesStructurallyEqualAcrossModules(
        driver,
        result,
        projected.idx,
        projected.module_idx,
        demanded_monotype,
        demanded_module_idx,
    )) {
        std.debug.panic(
            "Lambdasolved invariant violated: projected demand monotype mismatch for expr {d} in module {d} under source context {s}",
            .{
                @intFromEnum(expr_idx),
                module_idx,
                @tagName(source_context),
            },
        );
    }
}

fn recordExprMonotypeIfUnsetOrEqualForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    const thread = SemanticThread.trackedThread(source_context);
    const existing = try cm.lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    );
    if (existing) |resolved_existing| {
        if (try cm.monotypesStructurallyEqualAcrossModules(
            driver,
            result,
            resolved_existing.idx,
            resolved_existing.module_idx,
            monotype,
            monotype_module_idx,
        )) {
            return;
        }

        const module_env = driver.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const expr_region = module_env.store.getExprRegion(expr_idx);
        const source = module_env.getSourceAll();
        const snippet_start = @min(expr_region.start.offset, source.len);
        const snippet_end = @min(expr_region.end.offset, source.len);
        std.debug.panic(
            "Lambdasolved invariant violated: demanded exact monotype conflicted with existing exact monotype for expr {d} kind={s} region={any} snippet=\"{s}\" in module {d} ctx={s}; existing={d}@{d} existing_mono={any} demanded={d}@{d} demanded_mono={any}",
            .{
                @intFromEnum(expr_idx),
                @tagName(expr),
                expr_region,
                source[snippet_start..snippet_end],
                module_idx,
                @tagName(source_context),
                @intFromEnum(resolved_existing.idx),
                resolved_existing.module_idx,
                result.context_mono.monotype_store.getMonotype(resolved_existing.idx),
                @intFromEnum(monotype),
                monotype_module_idx,
                result.context_mono.monotype_store.getMonotype(monotype),
            },
        );
    }

    try cm.recordExprMonotypeForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        monotype,
        monotype_module_idx,
    );
}

pub fn propagateDemandedCallableFnMonotypeToValueExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    const visit_key = cm.Result.contextExprKey(source_context, module_idx, expr_idx);
    if (visiting.contains(visit_key)) return;
    try visiting.put(driver.allocator, visit_key, {});
    defer _ = visiting.remove(visit_key);

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);

    try cm.recordExprMonotypeForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    );

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (source.projections.isEmpty() and
            !(sourceContextsEqual(source.source_context, source_context) and
            source.module_idx == module_idx and
            source.expr_idx == expr_idx))
        {
            try propagateDemandedCallableFnMonotypeToValueExpr(
                driver,
                result,
                source.source_context,
                source.module_idx,
                source.expr_idx,
                fn_monotype,
                fn_monotype_module_idx,
                visiting,
            );
            return;
        }
    }

    switch (expr) {
        .e_if => |if_expr| {
            for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try propagateDemandedCallableFnMonotypeToValueExpr(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    branch.body,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            }
            try propagateDemandedCallableFnMonotypeToValueExpr(
                driver,
                result,
                source_context,
                module_idx,
                if_expr.final_else,
                fn_monotype,
                fn_monotype_module_idx,
                visiting,
            );
        },
        .e_match => |match_expr| {
            for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = module_env.store.getMatchBranch(branch_idx);
                try propagateDemandedCallableFnMonotypeToValueExpr(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    branch.value,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            }
        },
        else => {},
    }
}

pub fn recordDemandedRecordFieldMonotypes(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    record_expr: @TypeOf(@as(CIR.Expr, undefined).e_record),
) std.mem.Allocator.Error!void {
    const record_mono = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx)) orelse return;

    const mono = result.context_mono.monotype_store.getMonotype(record_mono.idx);
    const mono_record = switch (mono) {
        .record => |record| record,
        else => return,
    };

    const module_env = driver.all_module_envs[module_idx];
    for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const field_expr = module_env.store.getExpr(field.value);
        const mono_field_idx = cm.recordFieldIndexByNameInSpan(
            driver,
            result,
            module_idx,
            field.name,
            record_mono.module_idx,
            mono_record.fields,
        );
        const mono_field = result.context_mono.monotype_store.getFieldItem(mono_record.fields, mono_field_idx);
        if (exprMonotypeOwnedByInvocation(field_expr)) continue;
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            field.value,
            mono_field.type_idx,
            record_mono.module_idx,
        );
    }
}

pub fn recordDemandedTupleElemMonotypes(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    tuple_expr: @TypeOf(@as(CIR.Expr, undefined).e_tuple),
) std.mem.Allocator.Error!void {
    const tuple_mono = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx)) orelse return;

    const mono = result.context_mono.monotype_store.getMonotype(tuple_mono.idx);
    const module_env = driver.all_module_envs[module_idx];
    const elems = module_env.store.sliceExpr(tuple_expr.elems);

    switch (mono) {
        .record => |record| {
            const fields = result.context_mono.monotype_store.getFields(record.fields);
            if (fields.len != elems.len) return;
            for (elems, 0..) |elem_expr_idx, i| {
                const elem_expr = module_env.store.getExpr(elem_expr_idx);
                const field = result.context_mono.monotype_store.getFieldItem(record.fields, i);
                if (exprMonotypeOwnedByInvocation(elem_expr)) continue;
                try cm.recordExprMonotypeForThread(
                    driver,
                    result,
                    thread,
                    module_idx,
                    elem_expr_idx,
                    field.type_idx,
                    tuple_mono.module_idx,
                );
            }
        },
        .tuple => |tup| {
            if (tup.elems.len != elems.len) return;
            for (elems, 0..) |elem_expr_idx, i| {
                const elem_expr = module_env.store.getExpr(elem_expr_idx);
                const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tup.elems, i);
                if (exprMonotypeOwnedByInvocation(elem_expr)) continue;
                try cm.recordExprMonotypeForThread(
                    driver,
                    result,
                    thread,
                    module_idx,
                    elem_expr_idx,
                    elem_mono,
                    tuple_mono.module_idx,
                );
            }
        },
        else => {},
    }
}

pub fn propagateDemandedValueResultMonotypeToChild(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    parent_expr_idx: CIR.Expr.Idx,
    child_expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const parent_mono = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, parent_expr_idx)) orelse return;
    const child_expr = driver.all_module_envs[module_idx].store.getExpr(child_expr_idx);
    if (exprMonotypeOwnedByInvocation(child_expr)) return;
    try cm.recordExprMonotypeForThread(
        driver,
        result,
        thread,
        module_idx,
        child_expr_idx,
        parent_mono.idx,
        parent_mono.module_idx,
    );
}

pub fn bindCurrentIntrinsicDispatchSemantics(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) std.mem.Allocator.Error!void {
    switch (expr) {
        .e_binop => |binop_expr| {
            if (binop_expr.op == .eq or binop_expr.op == .ne) {
                if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, binop_expr.lhs)) |lhs_monotype| {
                    if (result.context_mono.monotype_store.getMonotype(lhs_monotype.idx) != .func) {
                        try cm.recordExprMonotypeForThread(
                            driver,
                            result,
                            thread,
                            module_idx,
                            binop_expr.rhs,
                            lhs_monotype.idx,
                            lhs_monotype.module_idx,
                        );
                        try propagateDemandedValueMonotypeToValueExpr(
                            driver,
                            result,
                            thread.requireSourceContext(),
                            module_idx,
                            binop_expr.rhs,
                            lhs_monotype.idx,
                            lhs_monotype.module_idx,
                        );
                    }
                }
            }
        },
        else => {},
    }

    const expr_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, ModuleEnv.varFrom(expr_idx));
    if (expr_monotype.isNone()) {
        std.debug.panic(
            "Lambdasolved invariant violated: intrinsic/no-callable dispatch expr {d} kind={s} in module {d} had no exact result monotype",
            .{ @intFromEnum(expr_idx), @tagName(expr), module_idx },
        );
    }
    if (result.context_mono.monotype_store.getMonotype(expr_monotype.idx) != .func) {
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr_monotype.idx,
            expr_monotype.module_idx,
        );
    }
}

pub fn scanStmt(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    stmt_idx: CIR.Statement.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const stmt = module_env.store.getStatement(stmt_idx);

    switch (stmt) {
        .s_decl => |decl| {
            try recordContextPatternSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                decl.pattern,
                exprRefInContext(thread.requireSourceContext(), module_idx, decl.expr),
            );
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, decl.pattern, decl.expr);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, decl.expr, resolve_direct_calls);
            try bindPatternCallableValueCallableInsts(driver, result, thread, module_idx, decl.pattern, decl.expr);
            if (readCallableParamValue(result, thread.requireSourceContext(), module_idx, decl.pattern) == null) {
                if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, decl.expr)) |expr_mono| {
                    try bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        decl.pattern,
                        expr_mono,
                    );
                }
            }
        },
        .s_var => |var_decl| {
            try recordContextPatternSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                var_decl.pattern_idx,
                exprRefInContext(thread.requireSourceContext(), module_idx, var_decl.expr),
            );
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, var_decl.expr, resolve_direct_calls);
            try bindPatternCallableValueCallableInsts(driver, result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
            if (readCallableParamValue(result, thread.requireSourceContext(), module_idx, var_decl.pattern_idx) == null) {
                if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, var_decl.expr)) |expr_mono| {
                    try bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        var_decl.pattern_idx,
                        expr_mono,
                    );
                }
            }
        },
        .s_reassign => |reassign| {
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, reassign.pattern_idx, reassign.expr);
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, reassign.expr, resolve_direct_calls);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, reassign.expr)) |expr_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    result,
                    thread,
                    module_idx,
                    reassign.pattern_idx,
                    expr_mono,
                );
            }
        },
        .s_dbg => |dbg_stmt| try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, dbg_stmt.expr, resolve_direct_calls),
        .s_expr => |expr_stmt| try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, expr_stmt.expr, resolve_direct_calls),
        .s_expect => |expect_stmt| try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, expect_stmt.body, resolve_direct_calls),
        .s_for => |for_stmt| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, for_stmt.expr, resolve_direct_calls);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, for_stmt.expr)) |iter_mono| {
                const item_mono = switch (result.context_mono.monotype_store.getMonotype(iter_mono.idx)) {
                    .list => |list| cm.resolvedMonotype(list.elem, iter_mono.module_idx),
                    else => std.debug.panic(
                        "Lambdasolved invariant violated: for-loop expr {d} in module {d} had non-list exact monotype",
                        .{ @intFromEnum(for_stmt.expr), module_idx },
                    ),
                };
                try bindCurrentPatternFromResolvedMonotype(driver, result, thread, module_idx, for_stmt.patt, item_mono);
            }
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, for_stmt.body, resolve_direct_calls);
        },
        .s_while => |while_stmt| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, while_stmt.cond, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, while_stmt.body, resolve_direct_calls);
        },
        .s_return => |return_stmt| try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, return_stmt.expr, resolve_direct_calls),
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_break,
        .s_crash,
        .s_runtime_error,
        => {},
    }
}

pub fn scanCirExprInternal(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    materialize_if_callable: bool,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);

    if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx)) |monotype| {
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            monotype.idx,
            monotype.module_idx,
        );
    }

    const callable_kind: ?CallableTemplateKind = switch (expr) {
        .e_lambda => .lambda,
        .e_closure => .closure,
        .e_hosted_lambda => .hosted_lambda,
        else => null,
    };
    if (callable_kind) |kind| {
        const template_id = try result.template_catalog.registerCallableTemplate(
            driver.allocator,
            driver.all_module_envs,
            callableTemplateOwnerForSourceContext(result, thread.requireSourceContext()),
            .{ .expr = TemplateCatalog.exprTemplateSource(module_idx, expr_idx) },
            module_idx,
            expr_idx,
            expr_idx,
            ModuleEnv.varFrom(expr_idx),
            null,
            kind,
            module_env.store.getExprRegion(expr_idx),
        );
        if (materialize_if_callable) {
            try materializeDemandedExprCallableInst(driver, result, thread, module_idx, expr_idx, template_id);
            if (!thread.hasCallableInst()) {
                try completeCurrentExprMonotype(driver, result, thread, module_idx, expr_idx);
                return;
            }
        }
    }

    const visit_key = cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx);
    const Worker = struct {
        driver: @TypeOf(driver),
        root_analysis: *RootAnalysisState,
        result: @TypeOf(result),
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        materialize_if_callable: bool,
        resolve_direct_calls: bool,

        fn run(self: @This()) std.mem.Allocator.Error!void {
            try scanCirExprChildren(
                self.driver,
                self.root_analysis,
                self.result,
                self.thread,
                self.module_idx,
                self.expr_idx,
                self.expr,
                self.resolve_direct_calls,
            );

            if (self.materialize_if_callable and
                self.result.getExprTemplateId(self.thread.requireSourceContext(), self.module_idx, self.expr_idx) == null)
            {
                if (getValueExprCallableValueForThread(self.driver, self.result, self.thread, self.module_idx, self.expr_idx) == null) {
                    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
                    defer visiting.deinit(self.driver.allocator);
                    try realizeStructuredExprCallableSemantics(
                        self.driver,
                        self.result,
                        self.thread.requireSourceContext(),
                        self.module_idx,
                        self.expr_idx,
                        &visiting,
                    );
                }
            }

            try completeCurrentExprMonotype(
                self.driver,
                self.result,
                self.thread,
                self.module_idx,
                self.expr_idx,
            );
        }
    };

    try root_analysis.visitExprOnce(
        driver.allocator,
        visit_key,
        Worker{
            .driver = driver,
            .root_analysis = root_analysis,
            .result = result,
            .thread = thread,
            .module_idx = module_idx,
            .expr_idx = expr_idx,
            .expr = expr,
            .materialize_if_callable = materialize_if_callable,
            .resolve_direct_calls = resolve_direct_calls,
        },
    );
}

pub fn completeCurrentExprMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const resolved = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx)) orelse return;
    try cm.recordExprMonotypeForThread(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        resolved.idx,
        resolved.module_idx,
    );
}

pub fn materializeDemandedExprCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
) std.mem.Allocator.Error!void {
    const source_context = thread.requireSourceContext();

    const fn_monotype = try cm.lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    ) orelse return;
    if (result.context_mono.monotype_store.getMonotype(fn_monotype.idx) != .func) return;
    _ = try materializeExprCallableValueWithKnownFnMonotype(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        template_id,
        fn_monotype,
    );
}

pub fn templateRequiresConcreteOwnerCallableInst(
    result: anytype,
    template_id: CallableTemplateId,
) bool {
    const template = result.getCallableTemplate(template_id);
    return template.kind == .closure and template.owner == .lexical_template;
}

pub fn callableTemplateOwnerForSourceContext(
    result: anytype,
    source_context: SourceContext,
) CallableTemplateOwner {
    if (result.getSourceContextTemplateId(source_context)) |template_id| {
        return .{ .lexical_template = template_id };
    }
    return .root_scope;
}

pub fn exprRefInContext(
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ExprRef {
    return .{
        .source_context = source_context,
        .module_idx = module_idx,
        .expr_idx = expr_idx,
        .projections = .empty(),
    };
}

pub fn exprRefAliasOrSelf(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ExprRef {
    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        return origin;
    }
    return exprRefInContext(source_context, module_idx, expr_idx);
}

pub fn extendExprRef(
    driver: anytype,
    result: anytype,
    source: ExprRef,
    projection: Lambdamono.CallableParamProjection,
) std.mem.Allocator.Error!ExprRef {
    return .{
        .source_context = source.source_context,
        .module_idx = source.module_idx,
        .expr_idx = source.expr_idx,
        .projections = try Lambdamono.appendValueProjectionEntries(
            driver,
            result,
            source.projections,
            &.{projection},
        ),
    };
}

pub fn writePatternOriginExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    expr_ref: ExprRef,
) std.mem.Allocator.Error!void {
    const binding = try result.lambdamono.ensurePatternBinding(driver.allocator, source_context, module_idx, pattern_idx);
    const canonical_ref = blk: {
        const origin = result.getExprOriginExpr(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx) orelse break :blk expr_ref;
        if (result.getExprOriginExpr(origin.source_context, origin.module_idx, origin.expr_idx) != null) {
            std.debug.panic(
                "Lambdasolved invariant violated: pattern value-origin for ctx={s} module={d} pattern={d} was not canonical",
                .{
                    @tagName(source_context),
                    module_idx,
                    @intFromEnum(pattern_idx),
                },
            );
        }
        break :blk ExprRef{
            .source_context = origin.source_context,
            .module_idx = origin.module_idx,
            .expr_idx = origin.expr_idx,
            .projections = try Lambdamono.appendValueProjectionEntries(
                driver,
                result,
                origin.projections,
                result.lambdamono.getValueProjectionEntries(expr_ref.projections),
            ),
        };
    };
    const next_origin: Lambdamono.ValueOrigin = .{ .expr = canonical_ref };
    if (!std.meta.eql(binding.origin, next_origin)) binding.origin = next_origin;
}

pub fn writeExprCallableValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_value: CallableValue,
) std.mem.Allocator.Error!void {
    const semantics = try Lambdamono.ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const owns_callable_intro = result.getExprTemplateId(source_context, module_idx, expr_idx) != null;
    const common = semantics.common().*;
    const next_expr: Lambdamono.Expr = switch (callable_value) {
        .direct => |callable_inst_id| if (owns_callable_intro)
            .{ .callable_intro = .{
                .common = common,
                .intro = .{
                    .callable_value = callable_value,
                    .callable_inst = callable_inst_id,
                },
            } }
        else
            .{ .callable_value = .{
                .common = common,
                .callable_value = callable_value,
            } },
        .packed_fn => |packed_fn| blk: {
            if (owns_callable_intro) {
                switch (semantics.getCallable() orelse break :blk .{ .callable_value = .{
                    .common = common,
                    .callable_value = callable_value,
                } }) {
                    .callable => |existing_callable_value| switch (existing_callable_value) {
                        .direct => |callable_inst_id| break :blk .{ .callable_intro = .{
                            .common = common,
                            .intro = .{
                                .callable_value = .{ .packed_fn = packed_fn },
                                .callable_inst = callable_inst_id,
                            },
                        } },
                        .packed_fn => {},
                    },
                    .intro => |existing_intro| break :blk .{ .callable_intro = .{
                        .common = common,
                        .intro = .{
                            .callable_value = .{ .packed_fn = packed_fn },
                            .callable_inst = existing_intro.callable_inst,
                        },
                    } },
                }
            }
            break :blk .{ .callable_value = .{
                .common = common,
                .callable_value = callable_value,
            } };
        },
    };
    if (!std.meta.eql(semantics.*, next_expr)) semantics.* = next_expr;
}

pub fn writeCallableParamValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_value: CallableValue,
) std.mem.Allocator.Error!void {
    const binding = try result.lambdamono.ensurePatternBinding(driver.allocator, source_context, module_idx, pattern_idx);
    const next_callable_value: ?CallableValue = callable_value;
    if (!std.meta.eql(binding.callable_value, next_callable_value)) binding.callable_value = next_callable_value;
}

pub fn writeExprCallSite(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    call_site: CallSite,
) std.mem.Allocator.Error!void {
    const semantics = try Lambdamono.ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const common = semantics.common().*;
    const next_expr: Lambdamono.Expr = switch (call_site) {
        .direct => |callable_inst| .{ .direct_call = .{
            .common = common,
            .callable_inst = callable_inst,
        } },
        .indirect_call => |indirect_call| .{ .indirect_call = .{
            .common = common,
            .indirect_call = indirect_call,
        } },
        .low_level => |low_level| .{ .low_level_call = .{
            .common = common,
            .low_level = low_level,
        } },
    };
    if (!std.meta.eql(semantics.*, next_expr)) semantics.* = next_expr;
}

pub fn writeExprOriginExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr_ref: ExprRef,
) std.mem.Allocator.Error!void {
    const canonical_ref = blk: {
        const origin = result.getExprOriginExpr(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx) orelse break :blk expr_ref;
        if (result.getExprOriginExpr(origin.source_context, origin.module_idx, origin.expr_idx) != null) {
            std.debug.panic(
                "Lambdasolved invariant violated: expr value-origin for ctx={s} module={d} expr={d} was not canonical",
                .{
                    @tagName(expr_ref.source_context),
                    expr_ref.module_idx,
                    @intFromEnum(expr_ref.expr_idx),
                },
            );
        }
        break :blk ExprRef{
            .source_context = origin.source_context,
            .module_idx = origin.module_idx,
            .expr_idx = origin.expr_idx,
            .projections = try Lambdamono.appendValueProjectionEntries(
                driver,
                result,
                origin.projections,
                result.lambdamono.getValueProjectionEntries(expr_ref.projections),
            ),
        };
    };
    const semantics = try Lambdamono.ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const next_origin: Lambdamono.ValueOrigin = .{ .expr = canonical_ref };
    if (!std.meta.eql(semantics.common().origin, next_origin)) semantics.commonMut().origin = next_origin;
}

pub fn writeExprLookupResolution(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    lookup_resolution: Lambdamono.LookupResolution,
) std.mem.Allocator.Error!void {
    const semantics = try Lambdamono.ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const common = semantics.common().*;
    const next_expr: Lambdamono.Expr = switch (lookup_resolution) {
        .expr => |expr_ref| .{ .lookup_expr = .{
            .common = common,
            .expr_ref = expr_ref,
        } },
        .def => |def_source| .{ .lookup_def = .{
            .common = common,
            .def_source = def_source,
        } },
    };
    if (!std.meta.eql(semantics.*, next_expr)) semantics.* = next_expr;
}

pub fn recordContextPatternSourceExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    source: ExprRef,
) std.mem.Allocator.Error!void {
    try writePatternOriginExpr(
        driver,
        result,
        source_context,
        module_idx,
        pattern_idx,
        source,
    );

    const module_env = driver.all_module_envs[module_idx];
    switch (module_env.store.getPattern(pattern_idx)) {
        .assign,
        .underscore,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .runtime_error,
        => {},
        .as => |as_pat| try recordContextPatternSourceExpr(driver, result, source_context, module_idx, as_pat.pattern, source),
        .nominal => |nominal_pat| try recordContextPatternSourceExpr(driver, result, source_context, module_idx, nominal_pat.backing_pattern, source),
        .nominal_external => |nominal_pat| try recordContextPatternSourceExpr(driver, result, source_context, module_idx, nominal_pat.backing_pattern, source),
        .tuple => |tuple_pat| {
            for (module_env.store.slicePatterns(tuple_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                const elem_source = try extendExprRef(driver, result, source, .{ .tuple_elem = @intCast(elem_index) });
                try recordContextPatternSourceExpr(driver, result, source_context, module_idx, elem_pattern_idx, elem_source);
            }
        },
        .applied_tag => |tag_pat| {
            for (module_env.store.slicePatterns(tag_pat.args), 0..) |arg_pattern_idx, arg_index| {
                const arg_source = try extendExprRef(driver, result, source, .{ .tag_payload = .{
                    .tag_name = .{
                        .module_idx = module_idx,
                        .ident = tag_pat.name,
                    },
                    .payload_index = @intCast(arg_index),
                } });
                try recordContextPatternSourceExpr(driver, result, source_context, module_idx, arg_pattern_idx, arg_source);
            }
        },
        .record_destructure => |record_pat| {
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required, .SubPattern => |sub_pattern_idx| {
                        const field_source = try extendExprRef(driver, result, source, .{ .field = .{
                            .module_idx = module_idx,
                            .ident = destruct.label,
                        } });
                        try recordContextPatternSourceExpr(driver, result, source_context, module_idx, sub_pattern_idx, field_source);
                    },
                    .Rest => {},
                }
            }
        },
        .list => |list_pat| {
            for (module_env.store.slicePatterns(list_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                const elem_source = try extendExprRef(driver, result, source, .{ .list_elem = @intCast(elem_index) });
                try recordContextPatternSourceExpr(driver, result, source_context, module_idx, elem_pattern_idx, elem_source);
            }
        },
    }
}

pub fn recordExprSourceExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    source: ExprRef,
) std.mem.Allocator.Error!void {
    try writeExprOriginExpr(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        source,
    );
}

pub fn propagatePatternDemandToExpr(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const pattern_mono = try cm.resolvePatternMonotypeResolved(driver, result, thread, module_idx,
        pattern_idx,
    );
    if (pattern_mono.isNone()) return;

    if (result.getContextPatternSourceExpr(thread.requireSourceContext(), module_idx, pattern_idx)) |source| {
        try propagateDemandedValueMonotypeToExprRef(
            result,
            source,
            pattern_mono.idx,
            pattern_mono.module_idx,
        );
    } else {
        try propagateDemandedValueMonotypeToValueExpr(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            pattern_mono.idx,
            pattern_mono.module_idx,
        );
    }
}

pub fn propagatePatternDemandSubtree(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    try propagatePatternDemandToExpr(driver, result, thread, module_idx, pattern_idx, expr_idx);

    const module_env = driver.all_module_envs[module_idx];
    switch (module_env.store.getPattern(pattern_idx)) {
        .as => |as_pat| try propagatePatternDemandSubtree(driver, result, thread, module_idx, as_pat.pattern, expr_idx),
        .nominal => |nominal_pat| try propagatePatternDemandSubtree(driver, result, thread, module_idx, nominal_pat.backing_pattern, expr_idx),
        .nominal_external => |nominal_pat| try propagatePatternDemandSubtree(driver, result, thread, module_idx, nominal_pat.backing_pattern, expr_idx),
        .tuple => |tuple_pat| for (module_env.store.slicePatterns(tuple_pat.patterns)) |child_pattern_idx| {
            try propagatePatternDemandSubtree(driver, result, thread, module_idx, child_pattern_idx, expr_idx);
        },
        .applied_tag => |tag_pat| for (module_env.store.slicePatterns(tag_pat.args)) |child_pattern_idx| {
            try propagatePatternDemandSubtree(driver, result, thread, module_idx, child_pattern_idx, expr_idx);
        },
        .record_destructure => |record_pat| for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
            const destruct = module_env.store.getRecordDestruct(destruct_idx);
            switch (destruct.kind) {
                .Required, .SubPattern => |child_pattern_idx| {
                    try propagatePatternDemandSubtree(driver, result, thread, module_idx, child_pattern_idx, expr_idx);
                },
                .Rest => {},
            }
        },
        .list => |list_pat| for (module_env.store.slicePatterns(list_pat.patterns)) |child_pattern_idx| {
            try propagatePatternDemandSubtree(driver, result, thread, module_idx, child_pattern_idx, expr_idx);
        },
        else => {},
    }
}

pub fn propagateMatchBranchPatternDemandsToCond(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    match_expr: @TypeOf(@as(CIR.Expr, undefined).e_match),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const branches = module_env.store.sliceMatchBranches(match_expr.branches);
    for (branches) |branch_idx| {
        const branch = module_env.store.getMatchBranch(branch_idx);
        for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
            try propagatePatternDemandSubtree(
                driver,
                result,
                thread,
                module_idx,
                branch_pattern.pattern,
                match_expr.cond,
            );
        }
    }
}

pub fn bindPatternCallableValueCallableInsts(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
        try materializeLookupExprCallableValue(driver, result, thread, module_idx, expr_idx, template_id);
    } else {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        var variant_builder = CallableVariantBuilder.init();
        defer variant_builder.deinit(driver.allocator);
        try includeExprCallableValue(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            &visiting,
            &variant_builder,
        );
    }

    const callable_value = getValueExprCallableValueForSourceContext(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
    ) orelse return;

    switch (callable_value) {
        .direct => |callable_inst_id| {
            const callable_inst = result.getCallableInst(callable_inst_id);
            try setCallableParamDirectValue(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                pattern_idx,
                callable_inst_id,
            );
            try setExprDirectCallable(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                callable_inst_id,
            );
            try bindCurrentPatternFromResolvedMonotype(
                result,
                thread,
                module_idx,
                pattern_idx,
                cm.resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
            );
            if (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype) != .func) {
                try cm.recordExprMonotypeForThread(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    callable_inst.fn_monotype,
                    callable_inst.fn_monotype_module_idx,
                );
            }
        },
        .packed_fn => {
            try setExprCallableValue(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                callable_value,
            );
            try setCallableParamValue(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                pattern_idx,
                callable_value,
            );
            try bindCurrentPatternFromResolvedMonotype(
                result,
                thread,
                module_idx,
                pattern_idx,
                result.getCallableValueSourceMonotype(callable_value),
            );
        },
    }
}

pub fn seedCallableBoundaryContextMonotypes(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    callable_expr_idx: CIR.Expr.Idx,
    arg_patterns: CIR.Pattern.Span,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    switch (source_context) {
        .template_expr => return,
        .callable_inst, .root_expr, .provenance_expr => {},
    }
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => return,
    };
    const boundary_arg_patterns = driver.all_module_envs[module_idx].store.slicePatterns(arg_patterns);
    if (boundary_arg_patterns.len != fn_mono.args.len) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved: callable boundary arity mismatch for expr {d} (patterns={d}, monos={d})",
                .{
                    @intFromEnum(callable_expr_idx),
                    boundary_arg_patterns.len,
                    fn_mono.args.len,
                },
            );
        }
        unreachable;
    }

    try cm.recordExprMonotypeForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        callable_expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    );

    for (boundary_arg_patterns, 0..) |pattern_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        try cm.mergeContextPatternMonotype(
            driver,
            result,
            cm.Result.contextPatternKey(source_context, module_idx, pattern_idx),
            cm.resolvedMonotype(param_mono, fn_monotype_module_idx),
        );
    }

    const boundary = result.getCallableTemplate(result.requireExprTemplateId(source_context, module_idx, callable_expr_idx));
    try cm.recordExprMonotypeForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        boundary.body_expr,
        fn_mono.ret,
        fn_monotype_module_idx,
    );
}

pub fn preRegisterBlockCallableStmtTemplates(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    stmts: []const CIR.Statement.Idx,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];

    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_decl => |decl| {
                _ = try result.template_catalog.preRegisterCallableTemplateForDefExpr(
                    driver.allocator,
                    driver.all_module_envs,
                    callableTemplateOwnerForSourceContext(result, thread.requireSourceContext()),
                    module_idx,
                    decl.expr,
                    ModuleEnv.varFrom(decl.pattern),
                    decl.pattern,
                    &.{.{ .local_pattern = TemplateCatalog.localPatternTemplateSource(module_idx, decl.pattern) }},
                );
            },
            .s_var => |var_decl| {
                _ = try result.template_catalog.preRegisterCallableTemplateForDefExpr(
                    driver.allocator,
                    driver.all_module_envs,
                    callableTemplateOwnerForSourceContext(result, thread.requireSourceContext()),
                    module_idx,
                    var_decl.expr,
                    ModuleEnv.varFrom(var_decl.pattern_idx),
                    var_decl.pattern_idx,
                    &.{.{ .local_pattern = TemplateCatalog.localPatternTemplateSource(module_idx, var_decl.pattern_idx) }},
                );
            },
            else => {},
        }
    }
}

pub fn includeExprCallableValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
    variant_builder: anytype,
) std.mem.Allocator.Error!void {
    try realizeStructuredExprCallableSemantics(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        visiting,
    );
    if (getValueExprCallableValueForSourceContext(driver, result, source_context, module_idx, expr_idx)) |callable_value| {
        try variant_builder.includeCallableValue(driver, result, callable_value);
    }
}

pub fn materializeExprCallableValueWithKnownFnMonotype(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
    fn_monotype: ResolvedMonotype,
) std.mem.Allocator.Error!?MaterializeCallableValueFailure {
    if (fn_monotype.isNone()) return null;
    if (result.context_mono.monotype_store.getMonotype(fn_monotype.idx) != .func) {
        return .non_function_monotype;
    }
    if (templateRequiresConcreteOwnerCallableInst(result, template_id) and
        !sourceContextHasCallableInst(source_context))
    {
        return .requires_owner_callable_inst;
    }
    if (try monotypeContainsNestedCallableValueShape(driver, result, fn_monotype)) {
        return .nested_callable_value_shape;
    }

    const callable_inst_id = (try Lambdamono.ensureCallableInstRecord(
        driver,
        result,
        source_context,
        template_id,
        fn_monotype.idx,
        fn_monotype.module_idx,
        &.{},
    )).id;
    try setExprDirectCallable(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        callable_inst_id,
    );
    return null;
}

pub fn propagateLookupSourceExprCallableValue(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    lookup_expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const source_expr = result.getExprOriginExpr(thread.requireSourceContext(), module_idx, lookup_expr_idx) orelse return;
    if (!source_expr.projections.isEmpty()) return;
    try setExprDirectCallable(
        driver,
        result,
        thread.requireSourceContext(),
        source_expr.module_idx,
        source_expr.expr_idx,
        callable_inst_id,
    );
}

pub fn materializeLookupExprCallableValue(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const template = result.getCallableTemplate(template_id).*;
    const desired_fn_monotype = cm.resolvedIfFunctionMonotype(result,
        try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx),
    );
    const existing_callable_inst_id = if (module_env.store.getExpr(expr_idx) == .e_lookup_local) blk: {
        const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
        if (readCallableParamValue(result, thread.requireSourceContext(), module_idx, lookup.pattern_idx)) |callable_value| {
            switch (callable_value) {
                .direct => |callable_inst_id| break :blk callable_inst_id,
                .packed_fn => {
                    try setExprCallableValue(
                        driver,
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        callable_value,
                    );
                    return;
                },
            }
        }
        break :blk null;
    } else null;

    const callable_inst_id = if (existing_callable_inst_id) |callable_inst_id| blk: {
        break :blk callable_inst_id;
    } else blk: {
        const fn_monotype = desired_fn_monotype orelse return;
        if (templateRequiresConcreteOwnerCallableInst(result, template_id) and
            !thread.hasCallableInst())
        {
            return;
        }
        if (try monotypeContainsNestedCallableValueShape(driver, result, fn_monotype)) {
            return;
        }
        break :blk (try Lambdamono.ensureCallableInstRecord(
            driver,
            result,
            thread.requireSourceContext(),
            template_id,
            fn_monotype.idx,
            fn_monotype.module_idx,
            &.{},
        )).id;
    };
    try setExprDirectCallable(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        callable_inst_id,
    );
    if (module_env.store.getExpr(expr_idx) == .e_lookup_local) {
        const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
        try setCallableParamDirectValue(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            lookup.pattern_idx,
            callable_inst_id,
        );
    }
    try setExprDirectCallable(
        driver,
        result,
        thread.requireSourceContext(),
        template.module_idx,
        template.cir_expr,
        callable_inst_id,
    );
    try propagateLookupSourceExprCallableValue(driver, result, thread, module_idx, expr_idx, callable_inst_id);
}

pub fn setExprDirectCallableInCallableContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    return setExprDirectCallable(
        driver,
        result,
        .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) },
        module_idx,
        expr_idx,
        callable_inst_id,
    );
}

pub fn setExprDirectCallable(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    try writeExprCallableValue(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        .{ .direct = callable_inst_id },
    );
}

pub fn setExprCallableValueInCallableContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_value: anytype,
) std.mem.Allocator.Error!void {
    return setExprCallableValue(
        driver,
        result,
        .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) },
        module_idx,
        expr_idx,
        callable_value,
    );
}

pub fn setExprCallableValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_value: anytype,
) std.mem.Allocator.Error!void {
    try writeExprCallableValue(driver, result, source_context, module_idx, expr_idx, callable_value);
}

pub fn setExprDirectCallSite(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    return writeExprCallSite(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        .{ .direct = callable_inst_id },
    );
}

pub fn setCallableParamDirectValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    return writeCallableParamValue(
        driver,
        result,
        source_context,
        module_idx,
        pattern_idx,
        .{ .direct = callable_inst_id },
    );
}

pub fn setCallableParamDirectValueInCallableContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    return setCallableParamDirectValue(
        driver,
        result,
        .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) },
        module_idx,
        pattern_idx,
        callable_inst_id,
    );
}

pub fn setCallableParamValueInCallableContext(
    driver: anytype,
    result: anytype,
    context_callable_inst: CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_value: anytype,
) std.mem.Allocator.Error!void {
    return setCallableParamValue(
        driver,
        result,
        .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) },
        module_idx,
        pattern_idx,
        callable_value,
    );
}

pub fn setCallableParamValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_value: anytype,
) std.mem.Allocator.Error!void {
    try writeCallableParamValue(
        driver,
        result,
        source_context,
        module_idx,
        pattern_idx,
        callable_value,
    );
}

pub fn realizeLookupExprSemantics(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) std.mem.Allocator.Error!void {
    const source_context = thread.requireSourceContext();
    const module_env = driver.all_module_envs[module_idx];

    switch (expr) {
        .e_lookup_local => |lookup| {
            if (result.getContextPatternSourceExpr(source_context, module_idx, lookup.pattern_idx)) |source| {
                try recordExprSourceExpr(driver, result, source_context, module_idx, expr_idx, source);
                try writeExprLookupResolution(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    .{ .expr = source },
                );
            } else if (findDefByPatternInModule(module_env, lookup.pattern_idx)) |def_idx| {
                try writeExprLookupResolution(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    .{ .def = .{
                        .module_idx = module_idx,
                        .def_idx = def_idx,
                    } },
                );
            }
        },
        .e_lookup_external => |lookup| {
            const target_module_idx = resolveImportedModuleIdx(driver, module_env, lookup.module_idx) orelse return;
            const target_env = driver.all_module_envs[target_module_idx];
            if (!target_env.store.isDefNode(lookup.target_node_idx)) return;

            const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
            const def = target_env.store.getDef(def_idx);
            const root_source_context: SourceContext = .{ .root_expr = .{
                .module_idx = target_module_idx,
                .expr_idx = def.expr,
            } };
            try writeExprLookupResolution(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                .{ .def = .{
                    .module_idx = target_module_idx,
                    .def_idx = def_idx,
                } },
            );
            try recordExprSourceExpr(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                .{
                    .source_context = root_source_context,
                    .module_idx = target_module_idx,
                    .expr_idx = def.expr,
                },
            );
        },
        .e_lookup_required => |lookup| {
            const target = resolveRequiredLookupTarget(driver, module_env, lookup) orelse return;
            const target_env = driver.all_module_envs[target.module_idx];
            const def = target_env.store.getDef(target.def_idx);
            const root_source_context: SourceContext = .{ .root_expr = .{
                .module_idx = target.module_idx,
                .expr_idx = def.expr,
            } };
            try writeExprLookupResolution(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                .{ .def = .{
                    .module_idx = target.module_idx,
                    .def_idx = target.def_idx,
                } },
            );
            try recordExprSourceExpr(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                .{
                    .source_context = root_source_context,
                    .module_idx = target.module_idx,
                    .expr_idx = def.expr,
                },
            );
        },
        else => unreachable,
    }
}

pub fn scanCirExprChildren(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];

    switch (expr) {
        .e_num,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_typed_int,
        .e_typed_frac,
        .e_str_segment,
        .e_bytes_literal,
        .e_lookup_pending,
        .e_empty_list,
        .e_empty_record,
        .e_zero_argument_tag,
        .e_runtime_error,
        .e_crash,
        .e_ellipsis,
        .e_anno_only,
        => {},
        .e_lookup_local, .e_lookup_external, .e_lookup_required => try realizeLookupExprSemantics(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
        ),
        .e_str => |str_expr| try scanCirValueExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(str_expr.span), resolve_direct_calls),
        .e_list => |list_expr| try scanCirValueExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(list_expr.elems), resolve_direct_calls),
        .e_tuple => |tuple_expr| {
            try recordDemandedTupleElemMonotypes(driver, result, thread, module_idx, expr_idx, tuple_expr);
            try scanCirValueExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(tuple_expr.elems), resolve_direct_calls);
        },
        .e_match => |match_expr| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, match_expr.cond, resolve_direct_calls);
            try recordMatchBranchPatternSources(driver, result, thread, module_idx, match_expr);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, match_expr.cond)) |cond_mono| {
                try bindMatchBranchPatternsFromCondMonotype(driver, result, thread, module_idx, match_expr, cond_mono);
            }

            const branches = module_env.store.sliceMatchBranches(match_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getMatchBranch(branch_idx);
                try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, branch.value);
                try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, branch.value, resolve_direct_calls);
                if (branch.guard) |guard_expr| {
                    try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, guard_expr, resolve_direct_calls);
                }
            }

            try propagateMatchBranchPatternDemandsToCond(driver, result, thread, module_idx, match_expr);
            try completeCurrentExprMonotype(driver, result, thread, module_idx, match_expr.cond);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, match_expr.cond)) |cond_mono| {
                try bindMatchBranchPatternsFromCondMonotype(driver, result, thread, module_idx, match_expr, cond_mono);
            }
        },
        .e_if => |if_expr| {
            const branches = module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, branch.cond, resolve_direct_calls);
                try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, branch.body);
                try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, branch.body, resolve_direct_calls);
            }
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, if_expr.final_else);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, if_expr.final_else, resolve_direct_calls);
        },
        .e_call => |call_expr| {
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            const callee_expr = module_env.store.getExpr(call_expr.func);
            if (!calleeUsesFirstClassCallableValuePath(callee_expr)) {
                try scanCirValueExprWithDirectCallResolution(
                    driver,
                    root_analysis,
                    result,
                    thread,
                    module_idx,
                    call_expr.func,
                    resolve_direct_calls,
                );
            }
            if (result.getExprLowLevelOp(thread.requireSourceContext(), module_idx, call_expr.func)) |low_level_op| {
                if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                    try resolveStrInspectHelperCallableInstsForTypeVar(
                        driver,
                        result,
                        thread,
                        module_idx,
                        ModuleEnv.varFrom(arg_exprs[0]),
                    );
                }
            }
            try scanCirValueExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, arg_exprs, resolve_direct_calls);
            if (resolve_direct_calls) {
                try resolveDirectCallSite(driver, root_analysis, result, thread, module_idx, expr_idx, call_expr);
                if (getCallSiteForThread(result, thread, module_idx, expr_idx) == null and
                    calleeUsesFirstClassCallableValuePath(callee_expr))
                {
                    try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, call_expr.func, resolve_direct_calls);
                    try resolveDirectCallSite(driver, root_analysis, result, thread, module_idx, expr_idx, call_expr);
                }
                try assignCallableArgCallableInstsFromCallMonotype(driver, result, thread, module_idx, expr_idx, call_expr);
            } else {
                try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, call_expr.func, resolve_direct_calls);
            }
            if (resolve_direct_calls) {
                try assignCallableArgCallableInstsFromCallMonotype(driver, result, thread, module_idx, expr_idx, call_expr);
            }
            if (resolve_direct_calls and
                getCallSiteCallableInstForThread(result, thread, module_idx, expr_idx) == null)
            {
                try resolveDirectCallSite(driver, root_analysis, result, thread, module_idx, expr_idx, call_expr);
            }
        },
        .e_record => |record_expr| {
            if (record_expr.ext) |ext_expr| {
                try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, ext_expr, resolve_direct_calls);
            }

            try recordDemandedRecordFieldMonotypes(driver, result, thread, module_idx, expr_idx, record_expr);

            const fields = module_env.store.sliceRecordFields(record_expr.fields);
            for (fields) |field_idx| {
                const field = module_env.store.getRecordField(field_idx);
                try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, field.value, resolve_direct_calls);
            }
        },
        .e_block => |block_expr| {
            const stmts = module_env.store.sliceStatements(block_expr.stmts);
            try preRegisterBlockCallableStmtTemplates(driver, result, thread, module_idx, stmts);
            for (stmts) |stmt_idx| {
                try scanStmt(driver, root_analysis, result, thread, module_idx, stmt_idx, resolve_direct_calls);
            }
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, block_expr.final_expr);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, block_expr.final_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, block_expr.final_expr),
            );
        },
        .e_tag => |tag_expr| try scanCirValueExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(tag_expr.args), resolve_direct_calls),
        .e_nominal => |nominal_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
            );
        },
        .e_nominal_external => |nominal_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
            );
        },
        .e_closure => |closure_expr| {
            const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
            if (lambda_expr == .e_lambda) {
                _ = try result.template_catalog.registerCallableTemplate(
                    driver.allocator,
                    driver.all_module_envs,
                    callableTemplateOwnerForSourceContext(result, thread.requireSourceContext()),
                    .{ .expr = TemplateCatalog.exprTemplateSource(module_idx, closure_expr.lambda_idx) },
                    module_idx,
                    closure_expr.lambda_idx,
                    expr_idx,
                    ModuleEnv.varFrom(closure_expr.lambda_idx),
                    null,
                    .lambda,
                    module_env.store.getExprRegion(closure_expr.lambda_idx),
                );
            }
            try scanClosureCaptureSources(
                driver,
                root_analysis,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                closure_expr,
                null,
            );
        },
        .e_lambda => {},
        .e_binop => |binop_expr| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, binop_expr.lhs, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, binop_expr.rhs, resolve_direct_calls);
            try realizeDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr);
        },
        .e_unary_minus => |unary_expr| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, unary_expr.expr, resolve_direct_calls);
        },
        .e_unary_not => |unary_expr| try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, unary_expr.expr, resolve_direct_calls),
        .e_dot_access => |dot_expr| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, dot_expr.receiver, resolve_direct_calls);
            if (dot_expr.args) |args| {
                try scanCirExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(args), resolve_direct_calls);
                try realizeDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr);
            } else {
                const receiver_source = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dot_expr.receiver);
                const field_source = try extendExprRef(driver, result, receiver_source, .{ .field = .{
                    .module_idx = module_idx,
                    .ident = dot_expr.field_name,
                } });
                try recordExprSourceExpr(driver, result, thread.requireSourceContext(), module_idx, expr_idx, field_source);
            }
        },
        .e_tuple_access => |tuple_access| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, tuple_access.tuple, resolve_direct_calls);
            const tuple_source = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, tuple_access.tuple);
            const elem_source = try extendExprRef(driver, result, tuple_source, .{ .tuple_elem = tuple_access.elem_index });
            try recordExprSourceExpr(driver, result, thread.requireSourceContext(), module_idx, expr_idx, elem_source);
        },
        .e_dbg => |dbg_expr| {
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, dbg_expr.expr);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, dbg_expr.expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dbg_expr.expr),
            );
        },
        .e_expect => |expect_expr| {
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, expect_expr.body);
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, expect_expr.body, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, expect_expr.body),
            );
        },
        .e_return => |return_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, return_expr.expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, return_expr.expr),
            );
        },
        .e_type_var_dispatch => |dispatch_expr| {
            try scanCirExprSpanWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, module_env.store.sliceExpr(dispatch_expr.args), resolve_direct_calls);
            try realizeDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr);
        },
        .e_for => |for_expr| {
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, for_expr.expr, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, root_analysis, result, thread, module_idx, for_expr.body, resolve_direct_calls);
        },
        .e_hosted_lambda => {},
        .e_run_low_level => |run_low_level| {
            const args = module_env.store.sliceExpr(run_low_level.args);
            try scanCirExprSpan(driver, root_analysis, result, thread, module_idx, args);
            if (run_low_level.op == .str_inspect and args.len != 0) {
                try resolveStrInspectHelperCallableInstsForTypeVar(
                    driver,
                    result,
                    thread,
                    module_idx,
                    ModuleEnv.varFrom(args[0]),
                );
            }
        },
    }
}

pub fn recordCallResultCallableValueFromCallee(
    driver: anytype,
    result: anytype,
    target_source_context: SourceContext,
    target_module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    callee_callable_inst_id: CallableInstId,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
    variant_builder: anytype,
) std.mem.Allocator.Error!void {
    try withRootAnalysis(driver.allocator, struct {
        driver_inner: @TypeOf(driver),
        result_inner: @TypeOf(result),
        target_source_context_inner: SourceContext,
        target_module_idx_inner: u32,
        call_expr_idx_inner: CIR.Expr.Idx,
        callee_callable_inst_id_inner: CallableInstId,
        visiting_inner: *std.AutoHashMapUnmanaged(ContextExprKey, void),
        variant_builder_inner: @TypeOf(variant_builder),

        fn run(self: @This(), root_analysis: *RootAnalysisState) std.mem.Allocator.Error!void {
            try ensureCallableInstRealized(self.driver_inner, self.result_inner, self.callee_callable_inst_id_inner);
            const callee_callable_inst = self.result_inner.getCallableInst(self.callee_callable_inst_id_inner);
            const callee_fn_mono = switch (self.result_inner.context_mono.monotype_store.getMonotype(callee_callable_inst.fn_monotype)) {
                .func => |func| func,
                else => unreachable,
            };
            if (self.result_inner.context_mono.monotype_store.getMonotype(callee_fn_mono.ret) != .func) return;

            const in_progress_key = CallResultCallableInstKey{
                .context_expr = @TypeOf(self.result_inner.*).contextExprKey(
                    self.target_source_context_inner,
                    self.target_module_idx_inner,
                    self.call_expr_idx_inner,
                ),
                .callee_callable_inst_raw = @intFromEnum(self.callee_callable_inst_id_inner),
            };

            const Worker = struct {
                driver_worker: @TypeOf(driver),
                result_worker: @TypeOf(result),
                callee_callable_inst_id_worker: CallableInstId,
                callee_fn_ret_worker: Monotype.Idx,
                callee_fn_module_idx_worker: u32,
                visiting_worker: *std.AutoHashMapUnmanaged(ContextExprKey, void),
                variant_builder_worker: @TypeOf(variant_builder),

                fn run(self: @This()) std.mem.Allocator.Error!void {
                    const callable_def = self.result_worker.getCallableDefForInst(self.callee_callable_inst_id_worker);
                    if (self.result_worker.getExprCallableValue(
                        callable_def.body_expr.source_context,
                        callable_def.body_expr.module_idx,
                        callable_def.body_expr.expr_idx,
                    ) == null) {
                        const template_id = self.result_worker.getExprTemplateId(
                            callable_def.body_expr.source_context,
                            callable_def.body_expr.module_idx,
                            callable_def.body_expr.expr_idx,
                        ) orelse std.debug.panic(
                            "Lambdasolved invariant violated: function-valued body expr {d} for callable inst {d} had no executable callable value fact and no registered callable template",
                            .{
                                @intFromEnum(callable_def.body_expr.expr_idx),
                                @intFromEnum(self.callee_callable_inst_id_worker),
                            },
                        );
                        const materialize_failure = try self.driver_worker.materializeExprCallableValueWithKnownFnMonotype(
                            self.result_worker,
                            callable_def.body_expr.source_context,
                            callable_def.body_expr.module_idx,
                            callable_def.body_expr.expr_idx,
                            template_id,
                            cm.resolvedMonotype(self.callee_fn_ret_worker, self.callee_fn_module_idx_worker),
                        );
                        if (std.debug.runtime_safety and
                            self.result_worker.getExprCallableValue(
                                callable_def.body_expr.source_context,
                                callable_def.body_expr.module_idx,
                                callable_def.body_expr.expr_idx,
                            ) == null)
                        {
                            std.debug.panic(
                                "Lambdasolved invariant violated: function-valued body expr {d} for callable inst {d} failed to materialize callable value; reason={?s} template={d}",
                                .{
                                    @intFromEnum(callable_def.body_expr.expr_idx),
                                    @intFromEnum(self.callee_callable_inst_id_worker),
                                    if (materialize_failure) |failure| @tagName(failure) else null,
                                    @intFromEnum(template_id),
                                },
                            );
                        }
                    }
                    try self.driver_worker.includeExprCallableValue(
                        self.result_worker,
                        callable_def.body_expr.source_context,
                        callable_def.body_expr.module_idx,
                        callable_def.body_expr.expr_idx,
                        self.visiting_worker,
                        self.variant_builder_worker,
                    );
                }
            };

            try root_analysis.withCallResult(
                self.driver_inner.allocator,
                in_progress_key,
                Worker{
                    .driver_worker = self.driver_inner,
                    .result_worker = self.result_inner,
                    .callee_callable_inst_id_worker = self.callee_callable_inst_id_inner,
                    .callee_fn_ret_worker = callee_fn_mono.ret,
                    .callee_fn_module_idx_worker = callee_callable_inst.fn_monotype_module_idx,
                    .visiting_worker = self.visiting_inner,
                    .variant_builder_worker = self.variant_builder_inner,
                },
            );
        }
    }{
        .driver_inner = driver,
        .result_inner = result,
        .target_source_context_inner = target_source_context,
        .target_module_idx_inner = target_module_idx,
        .call_expr_idx_inner = call_expr_idx,
        .callee_callable_inst_id_inner = callee_callable_inst_id,
        .visiting_inner = visiting,
        .variant_builder_inner = variant_builder,
    });
}

pub fn resolveDirectCallSiteWithFreshRootAnalysis(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!void {
    try withRootAnalysis(driver.allocator, struct {
        driver_inner: @TypeOf(driver),
        result_inner: @TypeOf(result),
        thread_inner: SemanticThread,
        module_idx_inner: u32,
        call_expr_idx_inner: CIR.Expr.Idx,
        call_expr_inner: @TypeOf(call_expr),

        fn run(self: @This(), root_analysis: *RootAnalysisState) std.mem.Allocator.Error!void {
            try resolveDirectCallSite(
                self.driver_inner,
                root_analysis,
                self.result_inner,
                self.thread_inner,
                self.module_idx_inner,
                self.call_expr_idx_inner,
                self.call_expr_inner,
            );
        }
    }{
        .driver_inner = driver,
        .result_inner = result,
        .thread_inner = thread,
        .module_idx_inner = module_idx,
        .call_expr_idx_inner = call_expr_idx,
        .call_expr_inner = call_expr,
    });
}

pub fn callableInstSatisfiesCallSiteRequirements(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    desired_fn_monotype: cm.ResolvedMonotype,
    required_template: ?CallableTemplateId,
    required_callable_param_specs: []const CallableParamSpecEntry,
) std.mem.Allocator.Error!bool {
    const callable_inst = result.getCallableInst(callable_inst_id);

    if (required_template) |template_id| {
        if (callable_inst.template != template_id) return false;
    }

    if (!desired_fn_monotype.isNone()) {
        if (!try cm.monotypesStructurallyEqualAcrossModules(
            driver,
            result,
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
            desired_fn_monotype.idx,
            desired_fn_monotype.module_idx,
        )) return false;
    }

    return callableParamSpecsEqual(
        result,
        result.lambdamono.getCallableParamSpecEntries(result.getCallableInst(callable_inst_id).callable_param_specs),
        required_callable_param_specs,
    );
}

pub fn requireTemplateDemandSourceContextForExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
) SourceContext {
    if (!templateRequiresConcreteOwnerCallableInst(result, template_id)) {
        return source_context;
    }
    if (switch (source_context) { .callable_inst => true, .root_expr, .provenance_expr, .template_expr => false }) {
        return source_context;
    }

    if (getValueExprCallableValueForSourceContext(driver, result, source_context, module_idx, expr_idx)) |callable_value| {
        switch (callable_value) {
            .direct => |callable_inst_id| return result.getCallableInst(callable_inst_id).defining_source_context,
            .packed_fn => {},
        }
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (switch (source.source_context) { .callable_inst => true, .root_expr, .provenance_expr, .template_expr => false }) {
            return source.source_context;
        }
        if (getValueExprCallableValueForSourceContext(
            driver,
            result,
            source.source_context,
            source.module_idx,
            source.expr_idx,
        )) |callable_value| {
            switch (callable_value) {
                .direct => |callable_inst_id| return result.getCallableInst(callable_inst_id).defining_source_context,
                .packed_fn => {},
            }
        }
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "Lambdasolved invariant violated: expr {d} in module {d} required a concrete owner callable-inst source context for template {d} but none was available in ctx={s}",
            .{
                @intFromEnum(expr_idx),
                module_idx,
                @intFromEnum(template_id),
                @tagName(source_context),
            },
        );
    }
    unreachable;
}

pub fn resolveDirectCallFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!?Monotype.Idx {
    _ = call_expr_idx;
    const callee_monotype = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, call_expr.func)) orelse return null;
    if (callee_monotype.module_idx == module_idx) return callee_monotype.idx;
    return try cm.remapMonotypeBetweenModules(driver, result,
        callee_monotype.idx,
        callee_monotype.module_idx,
        module_idx,
    );
}

pub fn resolveDemandedDirectCallFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!?cm.ResolvedMonotype {
    const fn_monotype = (try resolveDirectCallFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
    )) orelse return null;
    return cm.resolvedMonotype(fn_monotype, module_idx);
}

pub fn resolveDirectCallSite(
    driver: anytype,
    root_analysis: *RootAnalysisState,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!void {
    const callee_expr_idx = call_expr.func;
    const module_env = driver.all_module_envs[module_idx];
    const callee_expr = module_env.store.getExpr(callee_expr_idx);
    const desired_fn_monotype = try resolveDemandedDirectCallFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
    );
    if (desired_fn_monotype) |resolved_fn_monotype| {
        try bindCurrentCallFromFnMonotype(
            driver,
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr,
            resolved_fn_monotype.idx,
            resolved_fn_monotype.module_idx,
        );
    }
    var resolved_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, callee_expr_idx);
    if (resolved_template_id == null and desired_fn_monotype != null) {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        try propagateDemandedCallableFnMonotypeToValueExpr(
            result,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            desired_fn_monotype.?.idx,
            desired_fn_monotype.?.module_idx,
            &visiting,
        );
        resolved_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, callee_expr_idx);
    }
    if (result.getExprLowLevelOp(thread.requireSourceContext(), module_idx, call_expr.func)) |low_level_op| {
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        try writeExprCallSite(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            call_expr_idx,
            .{ .low_level = low_level_op },
        );
        if (low_level_op == .str_inspect and arg_exprs.len != 0) {
            try resolveStrInspectHelperCallableInstsForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                ModuleEnv.varFrom(arg_exprs[0]),
            );
        }
        return;
    }

    if (resolved_template_id == null) {
        if (getValueExprCallableInstForThread(driver, result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
            resolved_template_id = result.getCallableInst(callable_inst_id).template;
        }
    }
    if (resolved_template_id == null and !calleeUsesFirstClassCallableValuePath(callee_expr)) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: non-first-class callee expr {d} in module {d} under source context {s} reached direct-call specialization without a registered callable template or exact callable inst",
                .{
                    @intFromEnum(callee_expr_idx),
                    module_idx,
                    @tagName(thread.requireSourceContext()),
                },
            );
        }
        unreachable;
    }

    var required_callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
    defer required_callable_param_specs.deinit(driver.allocator);
    if (desired_fn_monotype != null) {
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        const specs_complete = try collectDirectCallCallableParamSpecs(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            desired_fn_monotype.?.idx,
            desired_fn_monotype.?.module_idx,
            arg_exprs,
            &required_callable_param_specs,
        );
        if (!specs_complete) return;
    }

    if (resolved_template_id) |template_id| {
        const template_source_context = requireTemplateDemandSourceContextForExpr(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            template_id,
        );
        if (try specializeDirectCallExactCallable(
            driver,
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr,
            template_id,
            template_source_context,
        )) |callable_inst_id| {
            try finalizeResolvedDirectCallCallableInst(
                driver,
                result,
                thread,
                module_idx,
                call_expr_idx,
                call_expr,
                callee_expr,
                callable_inst_id,
            );
            return;
        }
        if (std.debug.runtime_safety) switch (callee_expr) {
            .e_lookup_external, .e_lookup_required => {
                const arg_exprs = module_env.store.sliceExpr(call_expr.args);
                var arg_monos = std.ArrayList(cm.ResolvedMonotype).empty;
                defer arg_monos.deinit(driver.allocator);
                for (arg_exprs) |arg_expr_idx| {
                    try arg_monos.append(
                        driver.allocator,
                        try cm.requireFullyBoundExprMonotype(
                            driver,
                            result,
                            thread,
                            module_idx,
                            arg_expr_idx,
                        ),
                    );
                }
                const exact_call_fn_monotype = cm.resolvedMonotype(
                    try resolveDirectCallFnMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                    ),
                    module_idx,
                );
                const template = result.getCallableTemplate(template_id);
                std.debug.panic(
                    "Lambdasolved invariant violated: exact direct-call specialization failed for external callee expr {d} in module {d}; source_context={s} template={d} template_kind={s} template_owner={s} template_source_context={s} call_fn_monotype={any} arg_monotypes={any}",
                    .{
                        @intFromEnum(callee_expr_idx),
                        module_idx,
                        @tagName(thread.requireSourceContext()),
                        @intFromEnum(template_id),
                        @tagName(template.kind),
                        @tagName(template.owner),
                        @tagName(template_source_context),
                        exact_call_fn_monotype,
                        arg_monos.items,
                    },
                );
            },
            else => {},
        }
    }

    if (std.debug.runtime_safety) switch (callee_expr) {
        .e_lookup_external, .e_lookup_required => {
            const current_call_site = getCallSiteForThread(result, thread, module_idx, call_expr_idx);
            const template_source_context = if (resolved_template_id) |template_id|
                requireTemplateDemandSourceContextForExpr(
                    driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    callee_expr_idx,
                    template_id,
                )
            else
                null;
            switch (current_call_site orelse {
                std.debug.panic(
                    "Lambdasolved invariant violated: external callee expr {d} in module {d} reached callable-value materialization with no exact call-site; source_context={s} resolved_template={?d} template_source_context={?s}",
                    .{
                        @intFromEnum(callee_expr_idx),
                        module_idx,
                        @tagName(thread.requireSourceContext()),
                        if (resolved_template_id) |template_id| @intFromEnum(template_id) else null,
                        if (template_source_context) |ctx| @tagName(ctx) else null,
                    },
                );
            }) {
                .direct => {},
                .indirect_call => |indirect| std.debug.panic(
                    "Lambdasolved invariant violated: external callee expr {d} in module {d} resolved to indirect_call; source_context={s} resolved_template={?d} template_source_context={?s} variants={d}",
                    .{
                        @intFromEnum(callee_expr_idx),
                        module_idx,
                        @tagName(thread.requireSourceContext()),
                        if (resolved_template_id) |template_id| @intFromEnum(template_id) else null,
                        if (template_source_context) |ctx| @tagName(ctx) else null,
                        result.lambdamono.getCallableVariantGroupVariants(indirect.packed_fn.variant_group).len,
                    },
                ),
                .low_level => unreachable,
            }
        },
        else => {},
    };

    if (calleeUsesFirstClassCallableValuePath(callee_expr)) {
        var callee_visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer callee_visiting.deinit(driver.allocator);
        try realizeStructuredExprCallableSemantics(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            &callee_visiting,
        );
    }

    if (getValueExprCallableInstForThread(driver, result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
        const satisfies = try callableInstSatisfiesCallSiteRequirements(
            driver,
            result,
            callable_inst_id,
            desired_fn_monotype,
            resolved_template_id,
            required_callable_param_specs.items,
        );
        if (satisfies) {
            try finalizeResolvedDirectCallCallableInst(
                driver,
                result,
                thread,
                module_idx,
                call_expr_idx,
                call_expr,
                callee_expr,
                callable_inst_id,
            );
            return;
        }
    }

    if (getValueExprCallableValueForThread(driver, result, thread, module_idx, callee_expr_idx)) |callable_value| {
        var all_satisfy = true;
        for (result.lambdamono.getCallableValueVariants(callable_value)) |callable_inst_id| {
            if (!try callableInstSatisfiesCallSiteRequirements(
                driver,
                result,
                callable_inst_id,
                desired_fn_monotype,
                resolved_template_id,
                required_callable_param_specs.items,
            )) {
                all_satisfy = false;
                break;
            }
        }

        if (all_satisfy) {
            var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
            defer visiting.deinit(driver.allocator);
            var variant_builder = CallableVariantBuilder.init();
            defer variant_builder.deinit(driver.allocator);
            var call_variant_builder = CallableVariantBuilder.init();
            defer call_variant_builder.deinit(driver.allocator);
            try call_variant_builder.includeCallableValue(driver, result, callable_value);
            try writeExprCallSite(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                call_expr_idx,
                (try call_variant_builder.finishCallSite(driver, result)).?,
            );
            for (result.lambdamono.getCallableValueVariants(callable_value)) |callable_inst_id| {
                try recordCallResultCallableValueFromCallee(
                    driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    call_expr_idx,
                    callable_inst_id,
                    &visiting,
                    &variant_builder,
                );
            }
            if (try variant_builder.finishValue(driver, result)) |call_result_value| {
                try setExprCallableValue(driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    call_expr_idx,
                    call_result_value,
                );
            }
            if (readExprCallSite(result, thread.requireSourceContext(), module_idx, call_expr_idx)) |call_site| {
                switch (call_site) {
                    .direct => |direct_callable_inst_id| try finalizeResolvedDirectCallCallableInst(
                        driver,
                        result,
                        thread,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                        callee_expr,
                        direct_callable_inst_id,
                    ),
                    .indirect_call, .low_level => {},
                }
            }
            return;
        }
    }

    const template_id = resolved_template_id orelse {
        switch (callee_expr) {
            .e_lookup_local, .e_lookup_external, .e_lookup_required => {
                if (!desired_fn_monotype.isNone()) {
                    try bindCurrentCallFromFnMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                        desired_fn_monotype.idx,
                        desired_fn_monotype.module_idx,
                    );
                } else if (std.debug.runtime_safety and !thread.hasCallableInst()) {
                    if (exprVisitStillPendingForThread(
                        root_analysis,
                        driver,
                        thread,
                        module_idx,
                        callee_expr_idx,
                    )) {
                        return;
                    }
                    std.debug.panic(
                        "Lambdasolved missing direct-callee template for expr={d} module={d} callee_expr={d} tag={s}",
                        .{
                            @intFromEnum(call_expr_idx),
                            module_idx,
                            @intFromEnum(callee_expr_idx),
                            @tagName(callee_expr),
                        },
                    );
                }
                return;
            },
            .e_lambda, .e_closure, .e_hosted_lambda => {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: demanded direct call expr {d} in module {d} has direct-callable callee expr {d} ({s}) without a callable template",
                        .{
                            @intFromEnum(call_expr_idx),
                            module_idx,
                            @intFromEnum(callee_expr_idx),
                            @tagName(callee_expr),
                        },
                    );
                }
                unreachable;
            },
            else => return,
        }
    };
    const template_source_context = requireTemplateDemandSourceContextForExpr(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        callee_expr_idx,
        template_id,
    );
    const callable_inst_id = try specializeDirectCallExactCallable(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
        template_id,
        template_source_context,
    ) orelse {
        if (desired_fn_monotype.isNone() and std.debug.runtime_safety) {
            const template = result.getCallableTemplate(template_id);
            const template_env = driver.all_module_envs[template.module_idx];
            const call_region = module_env.store.getExprRegion(call_expr_idx);
            const call_source = module_env.getSourceAll();
            const call_snippet_start = @min(call_region.start.offset, call_source.len);
            const call_snippet_end = @min(call_region.end.offset, call_source.len);
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            const binding_name = switch (template.binding) {
                .pattern => |binding_pattern| switch (template_env.store.getPattern(binding_pattern)) {
                    .assign => |assign_pat| template_env.getIdent(assign_pat.ident),
                    else => "<non-assign>",
                },
                .anonymous => "<anonymous>",
            };
            std.debug.panic(
                "Lambdasolved unresolved direct callee '{s}' template={d} kind={s} template_module={d} template_expr={d} call_module={d} call_expr={d} context={d} root_source_expr={d} region={any} snippet=\"{s}\" arg_count={d}",
                .{
                    binding_name,
                    @intFromEnum(template_id),
                    @tagName(template.kind),
                    template.module_idx,
                    @intFromEnum(template.cir_expr),
                    module_idx,
                    @intFromEnum(call_expr_idx),
                    thread.callableInstRawForDebug(),
                    thread.rootExprRawForDebug(),
                    call_region,
                    call_source[call_snippet_start..call_snippet_end],
                    arg_exprs.len,
                },
            );
        }
        return;
    };
    try finalizeResolvedDirectCallCallableInst(
        driver,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
        callee_expr,
        callable_inst_id,
    );
}

pub fn realizeStructuredExprCallableSemantics(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    const visit_key = cm.Result.contextExprKey(source_context, module_idx, expr_idx);
    if (visiting.contains(visit_key)) return;
    try visiting.put(driver.allocator, visit_key, {});
    defer _ = visiting.remove(visit_key);

    if (readExprCallableValue(result, source_context, module_idx, expr_idx) != null) {
        return;
    }

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
            if (!(sourceContextsEqual(source.source_context, source_context) and
            source.module_idx == module_idx and
            source.expr_idx == expr_idx and
            source.projections.isEmpty()))
        {
            const maybe_pattern_idx = switch (expr) {
                .e_lookup_local => |lookup| lookup.pattern_idx,
                else => null,
            };
            try copyExprCallableValueFromRef(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                maybe_pattern_idx,
                source,
                visiting,
            );
            if (readExprCallableValue(result, source_context, module_idx, expr_idx) != null) {
                return;
            }
        }
    }

    var variant_builder = CallableVariantBuilder.init();
    defer variant_builder.deinit(driver.allocator);
    switch (expr) {
        .e_lookup_local, .e_lookup_external, .e_lookup_required => {
            if (getValueExprCallableValueForSourceContext(driver, result, source_context, module_idx, expr_idx)) |callable_value| {
                try setExprCallableValue(driver, result, source_context, module_idx, expr_idx, callable_value);
                return;
            }

            if (result.getExprTemplateId(source_context, module_idx, expr_idx)) |template_id| {
                try materializeLookupExprCallableValue(
                    driver,
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    expr_idx,
                    template_id,
                );
                if (std.debug.runtime_safety and
                    readExprCallableValue(result, source_context, module_idx, expr_idx) == null)
                {
                    const expr_region = module_env.store.getExprRegion(expr_idx);
                    const source = module_env.getSourceAll();
                    const snippet_start = @min(expr_region.start.offset, source.len);
                    const snippet_end = @min(expr_region.end.offset, source.len);
                    const context_template = if (sourceContextCallableInstId(source_context)) |context_callable_inst|
                        result.getCallableInst(context_callable_inst).template
                    else
                        null;
                    std.debug.panic(
                        "Lambdasolved invariant violated: callable lookup expr {d} in module {d} failed to materialize callable value on explicit demand; source_context={s} template={d} expr_tag={s} snippet=\"{s}\" context_template={?d} lookup={any}",
                        .{
                            @intFromEnum(expr_idx),
                            module_idx,
                            @tagName(source_context),
                            @intFromEnum(template_id),
                            @tagName(expr),
                            source[snippet_start..snippet_end],
                            if (context_template) |id| @intFromEnum(id) else null,
                            expr,
                        },
                    );
                }
            }
            return;
        },
        .e_if => |if_expr| {
            for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try includeExprCallableValue(driver, result, source_context, module_idx, branch.body, visiting, &variant_builder);
            }
            try includeExprCallableValue(driver, result, source_context, module_idx, if_expr.final_else, visiting, &variant_builder);
        },
        .e_match => |match_expr| {
            for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = module_env.store.getMatchBranch(branch_idx);
                try includeExprCallableValue(driver, result, source_context, module_idx, branch.value, visiting, &variant_builder);
            }
        },
        .e_call => |call_expr| {
            if (getCallSiteForSourceContext(
                result,
                source_context,
                module_idx,
                expr_idx,
            )) |call_site| {
                switch (call_site) {
                    .low_level => return,
                    .direct, .indirect_call => {},
                }
            }
            if (result.getExprLowLevelOp(source_context, module_idx, call_expr.func) != null) return;

            var call_site = getCallSiteForSourceContext(
                result,
                source_context,
                module_idx,
                expr_idx,
            );
            if (call_site == null) {
                try resolveDirectCallSiteWithFreshRootAnalysis(
                    driver,
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    expr_idx,
                    call_expr,
                );
                call_site = getCallSiteForSourceContext(
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                );
            }

            const resolved_call_site = call_site orelse {
                const callee_expr_idx = call_expr.func;
                const callee_expr = module_env.store.getExpr(callee_expr_idx);
                const call_region = module_env.store.getExprRegion(expr_idx);
                const source = module_env.getSourceAll();
                const call_snippet_start = @min(call_region.start.offset, source.len);
                const call_snippet_end = @min(call_region.end.offset, source.len);
                const callee_value = getValueExprCallableValueForSourceContext(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    callee_expr_idx,
                );
                const callee_template = result.getExprTemplateId(
                    source_context,
                    module_idx,
                    callee_expr_idx,
                );
                const callee_source = result.getExprOriginExpr(
                    source_context,
                    module_idx,
                    callee_expr_idx,
                );
                const callee_pattern_idx = switch (callee_expr) {
                    .e_lookup_local => |lookup| lookup.pattern_idx,
                    else => null,
                };
                const callee_pattern_value = if (callee_pattern_idx) |pattern_idx|
                    readCallableParamValue(result, source_context, module_idx, pattern_idx)
                else
                    null;
                const source_context_callable_inst = sourceContextCallableInstId(source_context);
                const callable_param_spec_count: ?usize = if (source_context_callable_inst) |context_callable_inst|
                    result.lambdamono.getCallableParamSpecEntries(result.getCallableInst(context_callable_inst).callable_param_specs).len
                else
                    null;
                const current_callable_template = if (source_context_callable_inst) |context_callable_inst|
                    result.getCallableTemplate(result.getCallableInst(context_callable_inst).template).*
                else
                    null;
                const callee_pattern_monotype = if (callee_pattern_idx) |pattern_idx|
                    result.getContextPatternMonotype(source_context, module_idx, pattern_idx)
                else
                    null;
                const call_fn_monotype = try resolveDirectCallFnMonotype(
                    driver,
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    expr_idx,
                    call_expr,
                );
                const callee_expr_monotype = try cm.requireFullyBoundExprMonotype(
                    driver,
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    callee_expr_idx,
                );
                const callee_pattern_is_arg = if (current_callable_template) |template|
                    if (callee_pattern_idx) |pattern_idx| blk: {
                        for (driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns)) |arg_pattern_idx| {
                            if (arg_pattern_idx == pattern_idx) break :blk true;
                        }
                        break :blk false;
                    } else false
                else
                    false;
                const callee_source_expr = if (callee_source) |source_expr_ref|
                    driver.all_module_envs[source_expr_ref.module_idx].store.getExpr(source_expr_ref.expr_idx)
                else
                    null;
                const callee_source_value = if (callee_source) |source_expr_ref|
                    getValueExprCallableValueForSourceContext(
                        driver,
                        result,
                        source_expr_ref.source_context,
                        source_expr_ref.module_idx,
                        source_expr_ref.expr_idx,
                    )
                else
                    null;
                const callee_source_template = if (callee_source) |source_expr_ref|
                    result.getExprTemplateId(source_expr_ref.source_context, source_expr_ref.module_idx, source_expr_ref.expr_idx)
                else
                    null;
                std.debug.panic(
                    "Lambdasolved invariant violated: callable-valued call expr {d} in module {d} had no explicit solved call-site in source context {s}; region={any} snippet=\"{s}\" call_fn_mono={d}@{d} call_fn_kind={s} callee_expr={d} callee_tag={s} callee_expr_mono={d}@{d} callee_expr_mono_kind={s} callee_value={?any} callee_template={?d} callee_source={?any} callee_pattern={?d} callee_pattern_value={?any} callee_pattern_monotype={?any} callee_pattern_mono_kind={?s} callee_pattern_is_arg={} context_callable_inst={?d} current_callable_fn_mono={?d}@{?d} current_callable_fn_mono_kind={?s} callable_param_specs={?d} current_callable_template_expr={?d} current_callable_binding={?d} current_callable_arg_count={?d} callee_source_tag={?s} callee_source_value={?any} callee_source_template={?d}",
                    .{
                        @intFromEnum(expr_idx),
                        module_idx,
                        @tagName(source_context),
                        call_region,
                        source[call_snippet_start..call_snippet_end],
                        @intFromEnum(call_fn_monotype),
                        module_idx,
                        @tagName(result.context_mono.monotype_store.getMonotype(call_fn_monotype)),
                        @intFromEnum(callee_expr_idx),
                        @tagName(callee_expr),
                        @intFromEnum(callee_expr_monotype.idx),
                        callee_expr_monotype.module_idx,
                        @tagName(result.context_mono.monotype_store.getMonotype(callee_expr_monotype.idx)),
                        callee_value,
                        if (callee_template) |template_id| @intFromEnum(template_id) else null,
                        callee_source,
                        if (callee_pattern_idx) |pattern_idx| @intFromEnum(pattern_idx) else null,
                        callee_pattern_value,
                        callee_pattern_monotype,
                        if (callee_pattern_monotype) |pattern_mono| @tagName(result.context_mono.monotype_store.getMonotype(pattern_mono.idx)) else null,
                        callee_pattern_is_arg,
                        if (source_context_callable_inst) |context_callable_inst| @intFromEnum(context_callable_inst) else null,
                        if (source_context_callable_inst) |context_callable_inst| @intFromEnum(result.getCallableInst(context_callable_inst).fn_monotype) else null,
                        if (source_context_callable_inst) |context_callable_inst| result.getCallableInst(context_callable_inst).fn_monotype_module_idx else null,
                        if (source_context_callable_inst) |context_callable_inst| @tagName(result.context_mono.monotype_store.getMonotype(result.getCallableInst(context_callable_inst).fn_monotype)) else null,
                        callable_param_spec_count,
                        if (current_callable_template) |template| @intFromEnum(template.cir_expr) else null,
                        if (current_callable_template) |template| switch (template.binding) {
                            .anonymous => null,
                            .pattern => |pattern_idx| @intFromEnum(pattern_idx),
                        } else null,
                        if (current_callable_template) |template| driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns).len else null,
                        if (callee_source_expr) |source_expr| @tagName(source_expr) else null,
                        callee_source_value,
                        if (callee_source_template) |template_id| @intFromEnum(template_id) else null,
                    },
                );
            };

            for (result.lambdamono.getCallSiteVariants(resolved_call_site)) |callee_callable_inst_id| {
                try recordCallResultCallableValueFromCallee(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    callee_callable_inst_id,
                    visiting,
                    &variant_builder,
                );
            }
        },
        .e_closure, .e_lambda, .e_hosted_lambda => {
            const template_id = result.getExprTemplateId(
                source_context,
                module_idx,
                expr_idx,
            ) orelse blk: {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: callable expr {d} in module {d} under source context {s} reached callable-value realization without a registered template",
                        .{
                            @intFromEnum(expr_idx),
                            module_idx,
                            @tagName(source_context),
                        },
                    );
                }
                break :blk unreachable;
            };
            try materializeLookupExprCallableValue(
                driver,
                result,
                SemanticThread.trackedThread(source_context),
                module_idx,
                expr_idx,
                template_id,
            );
            return;
        },
        else => {},
    }

    if (try variant_builder.finishValue(driver, result)) |callable_value| {
        try setExprCallableValue(driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_value,
        );
    }
}

pub fn realizeDispatchExprSemantics(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const dispatch_method_name = switch (expr) {
        .e_binop => |binop_expr| dispatchMethodIdentForExprBinop(module_env, binop_expr.op),
        .e_unary_minus => module_env.idents.negate,
        .e_dot_access => |dot_expr| if (dot_expr.args != null) dot_expr.field_name else null,
        .e_type_var_dispatch => |dispatch_expr| dispatch_expr.method_name,
        else => null,
    };
    if (dispatch_method_name) |method_name| {
        try DispatchSolved.ensureRecordedExactDispatchSiteForExpr(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            method_name,
        );
    }
    var associated_target: ?DispatchSolved.ResolvedDispatchTarget = null;
    if (expr == .e_binop) {
        const binop_expr = expr.e_binop;
        if (try binopDispatchHandledWithoutCallableInst(driver, result, thread, module_idx, expr_idx, binop_expr)) {
            try bindCurrentIntrinsicDispatchSemantics(driver, result, thread, module_idx, expr_idx, expr);
            return;
        }
        const method_name = dispatchMethodIdentForExprBinop(module_env, binop_expr.op) orelse return;
        if (try resolveAssociatedMethodCallableInstForTypeVar(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(binop_expr.lhs),
            method_name,
        )) |callable_inst_id| {
            try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
            return;
        }
        associated_target = try resolveAssociatedMethodDispatchTargetForTypeVar(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(binop_expr.lhs),
            method_name,
        );
        if (associated_target == null) {
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, binop_expr.lhs)) |lhs_monotype| {
                if (try resolveAssociatedMethodCallableInstForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                )) |callable_inst_id| {
                    try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                );
            }
        }
    }

    if (expr == .e_dot_access) {
        const dot_expr = expr.e_dot_access;
        if (dot_expr.args != null) {
            if (try dotDispatchHandledWithoutCallableInst(driver, result, thread, module_idx, expr_idx, dot_expr)) {
                try bindCurrentIntrinsicDispatchSemantics(driver, result, thread, module_idx, expr_idx, expr);
                return;
            }
            if (try resolveAssociatedMethodCallableInstForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(dot_expr.receiver),
                dot_expr.field_name,
            )) |callable_inst_id| {
                try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(dot_expr.receiver),
                dot_expr.field_name,
            );
            if (associated_target == null) {
                if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver)) |receiver_monotype| {
                    if (try resolveAssociatedMethodCallableInstForMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    )) |callable_inst_id| {
                        try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
                        return;
                    }
                    associated_target = try resolveAssociatedMethodDispatchTargetForMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    );
                }
            }
        }
    }

    if (associated_target == null and expr == .e_type_var_dispatch) {
        const dispatch_expr = expr.e_type_var_dispatch;
        if (try typeVarDispatchHandledWithoutCallableInst(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            dispatch_expr,
        )) {
            try bindCurrentIntrinsicDispatchSemantics(driver, result, thread, module_idx, expr_idx, expr);
            return;
        }
        const alias_stmt = module_env.store.getStatement(dispatch_expr.type_var_alias_stmt).s_type_var_alias;
        if (try resolveAssociatedMethodCallableInstForTypeVar(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(alias_stmt.type_var_anno),
            dispatch_expr.method_name,
        )) |callable_inst_id| {
            try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
            return;
        }
        associated_target = try resolveAssociatedMethodDispatchTargetForTypeVar(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(alias_stmt.type_var_anno),
            dispatch_expr.method_name,
        );
        const alias_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, ModuleEnv.varFrom(alias_stmt.type_var_anno));
        if (associated_target == null and !alias_monotype.isNone()) {
            if (try resolveAssociatedMethodCallableInstForMonotype(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                alias_monotype.idx,
                dispatch_expr.method_name,
            )) |callable_inst_id| {
                try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try resolveAssociatedMethodDispatchTargetForMonotype(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                alias_monotype.idx,
                dispatch_expr.method_name,
            );
        }
    }

    const resolved_target = if (associated_target) |target| target else switch (expr) {
        .e_binop => |binop_expr| blk: {
            const method_name = dispatchMethodIdentForExprBinop(module_env, binop_expr.op) orelse return;
            break :blk try resolveBinopDispatchTarget(driver, result, thread, module_idx, expr_idx, binop_expr, method_name);
        },
        .e_unary_minus => blk: {
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, module_env.idents.negate);
        },
        .e_dot_access => |dot_expr| blk: {
            if (dot_expr.args == null) return;
            if (dotCallUsesRuntimeReceiverExpr(module_env, dot_expr.receiver)) {
                _ = try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver);
                break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dot_expr.field_name);
            }
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dot_expr.field_name);
        },
        .e_type_var_dispatch => |dispatch_expr| blk: {
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dispatch_expr.method_name);
        },
        else => return,
    };
    const target_def = DispatchSolved.resolveDispatchTargetToExternalDef(
        driver.all_module_envs,
        module_idx,
        resolved_target,
    );
    try Lambdamono.recordDispatchExprTarget(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        .{
            .module_idx = target_def.module_idx,
            .def_idx = @enumFromInt(target_def.def_node_idx),
        },
    );
    const template_id = try DispatchSolved.lookupResolvedDispatchTemplate(driver, result, module_idx, resolved_target);
    const callable_inst_id = blk: {
        if (try specializeDispatchExactCallable(driver, result, thread, module_idx, expr_idx, expr, template_id)) |exact_callable_inst| {
            break :blk exact_callable_inst;
        }

        const fn_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, resolved_target.fn_var);
        if (!fn_monotype.isNone()) {
            break :blk try requireCallableInst(
                driver,
                result,
                thread.requireSourceContext(),
                template_id,
                fn_monotype.idx,
                fn_monotype.module_idx,
            );
        }

        return;
    };
    try commitDirectDispatchExprSemantics(driver, result, thread, module_idx, expr_idx, expr, callable_inst_id);
}

pub fn seedModuleDefPatternOrigins(
    driver: anytype,
    result: anytype,
    module_idx: u32,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const defs = module_env.store.sliceDefs(module_env.all_defs);

    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        try recordContextPatternSourceExpr(driver, result, .{ .root_expr = .{
            .module_idx = module_idx,
            .expr_idx = def.expr,
        } }, module_idx, def.pattern, .{
            .source_context = .{ .root_expr = .{
                .module_idx = module_idx,
                .expr_idx = def.expr,
            } },
            .module_idx = module_idx,
            .expr_idx = def.expr,
        });
    }
}

pub fn seedAllModuleDefPatternOrigins(
    driver: anytype,
    result: anytype,
) std.mem.Allocator.Error!void {
    for (driver.all_module_envs, 0..) |_, module_idx| {
        try seedModuleDefPatternOrigins(driver, result, @intCast(module_idx));
    }
}

pub fn dispatchExprRequiresRecordedTarget(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_dot_access => |dot_expr| dot_expr.args != null,
        .e_type_var_dispatch => true,
        else => false,
    };
}

pub fn resolveDirectDispatchExprTarget(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) std.mem.Allocator.Error!?DispatchExprTarget {
    const resolved_target = switch (expr) {
        .e_dot_access => |dot_expr| blk: {
            if (dot_expr.args == null) break :blk null;
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dot_expr.field_name);
        },
        .e_type_var_dispatch => |dispatch_expr| try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, dispatch_expr.method_name),
        else => null,
    } orelse return null;
    const target_def = DispatchSolved.resolveDispatchTargetToExternalDef(
        driver.all_module_envs,
        module_idx,
        resolved_target,
    );
    return .{
        .module_idx = target_def.module_idx,
        .def_idx = @enumFromInt(target_def.def_node_idx),
    };
}

pub fn dispatchTargetForCallableInst(
    result: anytype,
    callable_inst_id: CallableInstId,
) ?DispatchExprTarget {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    const external_def = switch (template.source) {
        .external_def => |source| source,
        .expr, .local_pattern => return null,
    };
    return .{
        .module_idx = external_def.module_idx,
        .def_idx = external_def.def_idx,
    };
}

fn primitiveUsesIntrinsicToStr(prim: Monotype.Prim) bool {
    return switch (prim) {
        .u8,
        .i8,
        .u16,
        .i16,
        .u32,
        .i32,
        .u64,
        .i64,
        .u128,
        .i128,
        .f32,
        .f64,
        .dec,
        => true,
        .str => false,
    };
}

pub fn dotDispatchHandledWithoutCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dot_expr: @TypeOf(@as(CIR.Expr, undefined).e_dot_access),
) std.mem.Allocator.Error!bool {
    const module_env = driver.all_module_envs[module_idx];
    if (!dotCallUsesRuntimeReceiverExpr(module_env, dot_expr.receiver)) return false;
    if (std.mem.eql(u8, module_env.getIdent(dot_expr.field_name), "to_str")) {
        const receiver_monotype = try cm.resolveExprMonotype(driver, result, thread, module_idx, dot_expr.receiver);
        if (receiver_monotype == null) {
            return dispatchHandledAsPrimitiveToStrIntrinsic(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                dot_expr.field_name,
            );
        }
        return switch (result.context_mono.monotype_store.getMonotype(receiver_monotype.?)) {
            .prim => |prim| primitiveUsesIntrinsicToStr(prim),
            else => false,
        };
    }
    if (!dot_expr.field_name.eql(module_env.idents.is_eq)) return false;

    const receiver_monotype = try cm.resolveExprMonotype(driver, result, thread, module_idx, dot_expr.receiver);
    if (receiver_monotype == null) {
        const eq_site = DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, module_env.idents.is_eq);
        return DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx) == null and eq_site != null;
    }

    return switch (result.context_mono.monotype_store.getMonotype(receiver_monotype.?)) {
        .record, .tuple, .list, .unit => true,
        .tag_union => DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx) == null,
        else => false,
    };
}

pub fn dispatchHandledAsPrimitiveToStrIntrinsic(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!bool {
    const module_env = driver.all_module_envs[module_idx];
    const constraint = DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, method_name) orelse return false;
    if (DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx) != null) return false;

    const fn_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, constraint.fn_var);
    if (fn_monotype.isNone()) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: cached exact dispatch expr={d} method='{s}' had no exact function monotype",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        unreachable;
    }

    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype.idx)) {
        .func => |func| func,
        else => return false,
    };

    const fn_args = result.context_mono.monotype_store.getIdxSpan(fn_mono.args);
    if (fn_args.len != 1) return false;

    const receiver_prim = switch (result.context_mono.monotype_store.getMonotype(fn_args[0])) {
        .prim => |prim| prim,
        else => return false,
    };
    if (!primitiveUsesIntrinsicToStr(receiver_prim)) return false;

    switch (result.context_mono.monotype_store.getMonotype(fn_mono.ret)) {
        .prim => |ret_prim| return ret_prim == .str,
        else => return false,
    }
}

pub fn binopDispatchHandledWithoutCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    binop_expr: CIR.Expr.Binop,
) std.mem.Allocator.Error!bool {
    if (binop_expr.op != .eq and binop_expr.op != .ne) return false;

    const module_env = driver.all_module_envs[module_idx];
    const lhs_monotype = try cm.resolveExprMonotype(driver, result, thread, module_idx, binop_expr.lhs);
    if (lhs_monotype == null) {
        const eq_site = DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, module_env.idents.is_eq);
        return DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx) == null and eq_site != null;
    }

    const lhs_mono = result.context_mono.monotype_store.getMonotype(lhs_monotype.?);
    return switch (lhs_mono) {
        .record, .tuple, .list, .unit, .prim => true,
        .tag_union => DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx) == null,
        else => false,
    };
}

pub fn typeVarDispatchHandledWithoutCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dispatch_expr: @TypeOf(@as(CIR.Expr, undefined).e_type_var_dispatch),
) std.mem.Allocator.Error!bool {
    const module_env = driver.all_module_envs[module_idx];
    if (!dispatch_expr.method_name.eql(module_env.idents.negate)) return false;

    var arg_exprs = std.ArrayList(CIR.Expr.Idx).empty;
    defer arg_exprs.deinit(driver.allocator);
    try Lambdamono.appendDispatchActualArgsFromProgram(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        &arg_exprs,
    );
    if (arg_exprs.items.len != 1) return false;

    const arg_monotype = (try cm.resolveExprMonotype(driver, result, thread, module_idx, arg_exprs.items[0])) orelse return false;

    const arg_mono = result.context_mono.monotype_store.getMonotype(arg_monotype);
    const intrinsic = switch (arg_mono) {
        .prim => |prim| switch (prim) {
            .i8, .i16, .i32, .i64, .i128, .f32, .f64, .dec => DispatchIntrinsic.negate,
            else => return false,
        },
        else => return false,
    };

    try Lambdamono.recordDispatchExprIntrinsic(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        intrinsic,
    );
    return true;
}

pub fn resolveBinopDispatchTarget(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    binop_expr: CIR.Expr.Binop,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!DispatchSolved.ResolvedDispatchTarget {
    const lhs_monotype = (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, binop_expr.lhs)) orelse {
        return resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
    };

    try cm.recordTypeVarMonotypeForThread(
        driver,
        result,
        thread,
        module_idx,
        ModuleEnv.varFrom(binop_expr.rhs),
        lhs_monotype.idx,
        lhs_monotype.module_idx,
    );
    return resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
}

pub fn resolveAssociatedMethodCallableInstForTypeVar(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    receiver_type_var: types.Var,
    method_ident: Ident.Idx,
) std.mem.Allocator.Error!?CallableInstId {
    const module_env = driver.all_module_envs[module_idx];
    const receiver_nominal = cm.resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
    const method_info = try DispatchSolved.lookupAssociatedMethodTemplate(driver, result, module_idx, receiver_nominal, method_ident) orelse return null;
    return try specializeDispatchExactCallable(driver, result, thread, module_idx, expr_idx, expr, method_info.template_id);
}

pub fn resolveAssociatedMethodDispatchTargetForTypeVar(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    receiver_type_var: types.Var,
    method_ident: Ident.Idx,
) std.mem.Allocator.Error!?DispatchSolved.ResolvedDispatchTarget {
    const module_env = driver.all_module_envs[module_idx];
    const receiver_nominal = cm.resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
    const method_info = try DispatchSolved.lookupAssociatedMethodTemplate(driver, result, module_idx, receiver_nominal, method_ident) orelse return null;
    const receiver_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, receiver_type_var);
    const site = try exactAssociatedMethodDispatchSite(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        method_ident,
        method_info,
        receiver_monotype.idx,
    ) orelse return null;
    return .{
        .origin = method_info.target_env.qualified_module_ident,
        .method_ident = method_info.qualified_method_ident,
        .fn_var = site.fn_var,
        .module_idx = method_info.module_idx,
    };
}

pub fn resolveAssociatedMethodCallableInstForMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    receiver_monotype: Monotype.Idx,
    method_ident: Ident.Idx,
) std.mem.Allocator.Error!?CallableInstId {
    const method_info = try DispatchSolved.lookupAssociatedMethodTemplateForMonotype(
        driver,
        result,
        module_idx,
        receiver_monotype,
        method_ident,
    ) orelse return null;
    return try specializeDispatchExactCallable(driver, result, thread, module_idx, expr_idx, expr, method_info.template_id);
}

pub fn resolveAssociatedMethodDispatchTargetForMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    receiver_monotype: Monotype.Idx,
    method_ident: Ident.Idx,
) std.mem.Allocator.Error!?DispatchSolved.ResolvedDispatchTarget {
    const method_info = try DispatchSolved.lookupAssociatedMethodTemplateForMonotype(
        driver,
        result,
        module_idx,
        receiver_monotype,
        method_ident,
    ) orelse return null;
    const site = try exactAssociatedMethodDispatchSite(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        method_ident,
        method_info,
        receiver_monotype,
    ) orelse return null;
    return .{
        .origin = method_info.target_env.qualified_module_ident,
        .method_ident = method_info.qualified_method_ident,
        .fn_var = site.fn_var,
        .module_idx = method_info.module_idx,
    };
}

pub fn exactAssociatedMethodDispatchSite(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    method_name: Ident.Idx,
    method_info: anytype,
    receiver_monotype: Monotype.Idx,
) std.mem.Allocator.Error!?DispatchSolved.ExactDispatchSite {
    _ = expr;
    const module_env = driver.all_module_envs[module_idx];
    _ = method_info;
    const site = DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, method_name) orelse return null;

    const fn_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, site.fn_var);
    if (fn_monotype.isNone()) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: associated dispatch expr={d} method='{s}' cached an exact dispatch site without an exact function monotype",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        unreachable;
    }

    if (!receiver_monotype.isNone()) {
        const mono = result.context_mono.monotype_store.getMonotype(fn_monotype.idx);
        if (mono != .func) return null;

        const fn_args = result.context_mono.monotype_store.getIdxSpan(mono.func.args);
        if (fn_args.len == 0) return null;

        if (!try monotypeDispatchCompatible(
            driver,
            result,
            fn_args[0],
            module_idx,
            receiver_monotype,
            fn_monotype.module_idx,
        )) return null;
    }

    if (receiver_monotype.isNone()) {
        return null;
    }

    return site;
}

pub fn resolveDispatchTargetForExpr(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!DispatchSolved.ResolvedDispatchTarget {
    const module_env = driver.all_module_envs[module_idx];
    const site = DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, method_name) orelse {
        if (std.debug.runtime_safety) {
            const expr = module_env.store.getExpr(expr_idx);
            const receiver_monotype: ?ResolvedMonotype = switch (expr) {
                .e_dot_access => |dot_expr| if (dot_expr.args != null)
                    try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver)
                else
                    null,
                .e_type_var_dispatch => blk: {
                    var args = std.ArrayList(CIR.Expr.Idx).empty;
                    defer args.deinit(driver.allocator);
                    try Lambdamono.appendDispatchActualArgsFromProgram(
                        driver,
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        &args,
                    );
                    if (args.items.len == 0) break :blk null;
                    break :blk try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, args.items[0]);
                },
                else => null,
            };
            const receiver_lookup_pattern: ?CIR.Pattern.Idx = switch (expr) {
                .e_dot_access => |dot_expr| switch (module_env.store.getExpr(dot_expr.receiver)) {
                    .e_lookup_local => |lookup| lookup.pattern_idx,
                    else => null,
                },
                else => null,
            };
            const receiver_pattern_callable_value = if (receiver_lookup_pattern) |pattern_idx|
                result.getPatternCallableValue(thread.requireSourceContext(), module_idx, pattern_idx)
            else
                null;
            const receiver_pattern_monotype = if (receiver_lookup_pattern) |pattern_idx|
                result.context_mono.getContextPatternMonotype(thread.requireSourceContext(), module_idx, pattern_idx)
            else
                null;
            const receiver_pattern_source = if (receiver_lookup_pattern) |pattern_idx|
                result.getContextPatternSourceExpr(thread.requireSourceContext(), module_idx, pattern_idx)
            else
                null;
            const current_callable_inst: ?CallableInstId = switch (thread.requireSourceContext()) {
                .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
                .root_expr, .provenance_expr, .template_expr => null,
            };
            const current_callable_fn = if (current_callable_inst) |callable_inst_id|
                result.getCallableInst(callable_inst_id)
            else
                null;
            std.debug.panic(
                "Lambdasolved invariant violated: no exact cached dispatch site for expr={d} method='{s}' in ctx={s}; expr_tag={s} receiver={?d}@{?d} receiver_shape={?any} receiver_pattern={?d} receiver_pattern_value={?any} receiver_pattern_mono={?any} receiver_pattern_source={?any} current_callable_inst={?d} current_callable_fn={?d}@{?d} current_callable_fn_shape={?any} current_callable_template_expr={?d}",
                .{
                    @intFromEnum(expr_idx),
                    module_env.getIdent(method_name),
                    @tagName(thread.requireSourceContext()),
                    @tagName(expr),
                    if (receiver_monotype) |mono| @intFromEnum(mono.idx) else null,
                    if (receiver_monotype) |mono| mono.module_idx else null,
                    if (receiver_monotype) |mono|
                        if (mono.isNone()) null else result.context_mono.monotype_store.getMonotype(mono.idx)
                    else
                        null,
                    if (receiver_lookup_pattern) |pattern_idx| @intFromEnum(pattern_idx) else null,
                    receiver_pattern_callable_value,
                    if (receiver_pattern_monotype) |mono|
                        if (mono.isNone()) null else result.context_mono.monotype_store.getMonotype(mono.idx)
                    else
                        null,
                    receiver_pattern_source,
                    if (current_callable_inst) |callable_inst_id| @intFromEnum(callable_inst_id) else null,
                    if (current_callable_fn) |callable_inst| @intFromEnum(callable_inst.fn_monotype) else null,
                    if (current_callable_fn) |callable_inst| callable_inst.fn_monotype_module_idx else null,
                    if (current_callable_fn) |callable_inst| result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype) else null,
                    if (current_callable_fn) |callable_inst| @intFromEnum(result.getCallableTemplate(callable_inst.template).cir_expr) else null,
                },
            );
        }
        unreachable;
    };

    const desired_func_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, site.fn_var);
    const resolved_target = try resolveDispatchTargetFromExactSite(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        module_env.store.getExpr(expr_idx),
        method_name,
        site.fn_var,
    ) orelse {
        const desired_func = if (!desired_func_monotype.isNone())
            result.context_mono.monotype_store.getMonotype(desired_func_monotype.idx)
        else
            null;
        const desired_receiver = if (desired_func_monotype.isNone()) null else switch (result.context_mono.monotype_store.getMonotype(desired_func_monotype.idx)) {
            .func => |func| if (func.args.len == 0)
                null
            else
                result.context_mono.monotype_store.getMonotype(result.context_mono.monotype_store.getIdxSpanItem(func.args, 0)),
            else => null,
        };
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: dispatch expr={d} method='{s}' reached specialization without a resolvable exact target (fn_var={d} desired={d}@{d} desired_mono={any} receiver_mono={any})",
                .{
                    @intFromEnum(expr_idx),
                    module_env.getIdent(method_name),
                    @intFromEnum(site.fn_var),
                    @intFromEnum(desired_func_monotype.idx),
                    desired_func_monotype.module_idx,
                    desired_func,
                    desired_receiver,
                },
            );
        }
        unreachable;
    };
    if (!desired_func_monotype.isNone() and
        !try resolvedDispatchTargetMatchesMonotype(driver, result, module_idx, resolved_target, desired_func_monotype.idx))
    {
        std.debug.panic(
            "Lambdasolved invariant violated: dispatch expr={d} method='{s}' resolved target did not match the exact function monotype",
            .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
        );
    }
    return resolved_target;
}

pub fn resolveDispatchTargetFromExactSite(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    method_name: Ident.Idx,
    fn_var: types.Var,
) std.mem.Allocator.Error!?DispatchSolved.ResolvedDispatchTarget {
    const resolved = switch (expr) {
        .e_binop => |binop_expr| blk: {
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            )) |target| break :blk target;
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, binop_expr.lhs)) |lhs_monotype| {
                break :blk try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                );
            }
            break :blk null;
        },
        .e_unary_minus => |unary_expr| blk: {
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(unary_expr.expr),
                method_name,
            )) |target| break :blk target;
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, unary_expr.expr)) |operand_monotype| {
                break :blk try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    operand_monotype.idx,
                    method_name,
                );
            }
            break :blk null;
        },
        .e_dot_access => |dot_expr| blk: {
            if (dot_expr.args == null) break :blk null;
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(dot_expr.receiver),
                method_name,
            )) |target| break :blk target;
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver)) |receiver_monotype| {
                break :blk try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    receiver_monotype.idx,
                    method_name,
                );
            }
            break :blk null;
        },
        .e_type_var_dispatch => |dispatch_expr| blk: {
            const alias_stmt = driver.all_module_envs[module_idx].store.getStatement(dispatch_expr.type_var_alias_stmt).s_type_var_alias;
            const receiver_type_var = ModuleEnv.varFrom(alias_stmt.type_var_anno);
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                receiver_type_var,
                method_name,
            )) |target| break :blk target;
            const alias_monotype = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, receiver_type_var);
            if (!alias_monotype.isNone()) {
                break :blk try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    alias_monotype.idx,
                    method_name,
                );
            }
            break :blk null;
        },
        else => null,
    };
    if (resolved) |target| {
        if (std.debug.runtime_safety and target.fn_var != fn_var) {
            std.debug.panic(
                "Lambdasolved invariant violated: dispatch target fn var {d} did not match exact dispatch site fn var {d} for expr={d} method='{s}'",
                .{
                    @intFromEnum(target.fn_var),
                    @intFromEnum(fn_var),
                    @intFromEnum(expr_idx),
                    driver.all_module_envs[module_idx].getIdent(method_name),
                },
            );
        }
    }
    return resolved;
}

pub fn monotypeDispatchCompatible(
    driver: anytype,
    result: anytype,
    expected: Monotype.Idx,
    expected_module_idx: u32,
    actual: Monotype.Idx,
    actual_module_idx: u32,
) std.mem.Allocator.Error!bool {
    if (expected.isNone() or actual.isNone()) return true;
    return cm.monotypesStructurallyEqualAcrossModules(
        driver,
        result,
        expected,
        expected_module_idx,
        actual,
        actual_module_idx,
    );
}

pub fn resolvedDispatchTargetMatchesMonotype(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    target: DispatchSolved.ResolvedDispatchTarget,
    desired_func_monotype: Monotype.Idx,
) std.mem.Allocator.Error!bool {
    if (desired_func_monotype.isNone()) return true;

    const target_def = DispatchSolved.resolveDispatchTargetToExternalDef(
        driver.all_module_envs,
        source_module_idx,
        target,
    );
    const target_env = driver.all_module_envs[target_def.module_idx];
    const def_idx: CIR.Def.Idx = @enumFromInt(target_def.def_node_idx);
    const def = target_env.store.getDef(def_idx);
    const target_mono = result.getExprMonotype(
        .{ .root_expr = .{ .module_idx = target_def.module_idx, .expr_idx = def.expr } },
        target_def.module_idx,
        def.expr,
    ) orelse return false;
    if (target_mono.isNone()) return false;

    return cm.monotypesStructurallyEqualAcrossModules(
        driver,
        result,
        target_mono.idx,
        target_mono.module_idx,
        desired_func_monotype,
        source_module_idx,
    );
}

pub fn bindCurrentDispatchFromCallableInst(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };

    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try Lambdamono.appendDispatchActualArgsFromProgram(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        &actual_args,
    );

    if (actual_args.items.len != fn_mono.args.len) unreachable;

    for (actual_args.items, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) {
            try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            try propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
            );
        }
    }

    if (result.context_mono.monotype_store.getMonotype(fn_mono.ret) != .func) {
        try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
    }
}

pub fn commitDirectDispatchExprSemantics(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const dispatch_target = blk: {
        if (DispatchSolved.lookupResolvedDispatchTarget(result, thread.requireSourceContext(), module_idx, expr_idx)) |existing| break :blk existing;
        if (dispatchTargetForCallableInst(result, callable_inst_id)) |template_target| break :blk template_target;
        if (try resolveDirectDispatchExprTarget(driver, result, thread, module_idx, expr_idx, expr)) |resolved| break :blk resolved;
        if (!dispatchExprRequiresRecordedTarget(expr)) break :blk null;
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: dispatch expr {d} in module {d} resolved to callable inst {d} (template {d}) without a concrete dispatch target",
                .{
                    @intFromEnum(expr_idx),
                    module_idx,
                    @intFromEnum(callable_inst_id),
                    @intFromEnum(result.getCallableInst(callable_inst_id).template),
                },
            );
        }
        unreachable;
    };
    if (dispatch_target) |target_def| {
        try Lambdamono.recordDispatchExprTarget(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            target_def,
        );
    }
    try setExprDirectCallSite(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        callable_inst_id,
    );
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    try setExprDirectCallable(
        driver,
        result,
        thread.requireSourceContext(),
        template.module_idx,
        template.cir_expr,
        callable_inst_id,
    );
    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try Lambdamono.appendDispatchActualArgsFromProgram(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        &actual_args,
    );
    try prepareCallableArgsForCallableInst(driver, result, thread, module_idx, actual_args.items, callable_inst_id);
    try realizeCallableArgSemanticsSlice(driver, result, thread, module_idx, actual_args.items);
    try bindCurrentDispatchFromCallableInst(driver, result, thread, module_idx, expr_idx, callable_inst_id);
}

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
