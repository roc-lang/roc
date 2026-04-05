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
pub const ResolvedMonotype = cm.ResolvedMonotype;
pub const RuntimeValue = Lambdamono.RuntimeValue;
pub const ValueProjectionSpan = Lambdamono.CallableParamProjectionSpan;
pub const CaptureValueSource = Lambdamono.CaptureValueSource;
pub const CaptureStorage = Lambdamono.CaptureStorage;
pub const CaptureField = Lambdamono.CaptureField;
pub const CaptureFieldSpan = Lambdamono.CaptureFieldSpan;
pub const CallableParamSpecEntry = Lambdamono.CallableParamSpecEntry;
pub const CallableValue = Lambdamono.CallableValue;
pub const CallSite = Lambdamono.CallSite;
pub const ExprRef = Lambdamono.ExprRef;
const DispatchExprTarget = DispatchSolved.DispatchExprTarget;
const DispatchIntrinsic = Lambdamono.DispatchIntrinsic;

pub const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

pub const CallableLexicalBinding = struct {
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    callable_value: CallableValue,
};

pub const ValueLexicalBinding = struct {
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    source: CaptureValueSource,
};

pub const ValueLexicalBindingSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ValueLexicalBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ValueLexicalBindingSpan) bool {
        return self.len == 0;
    }
};

pub const CallableLexicalEnv = struct {
    parent: ?*const CallableLexicalEnv = null,
    bindings: []const CallableLexicalBinding,

    pub fn lookup(
        self: *const CallableLexicalEnv,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        var current: ?*const CallableLexicalEnv = self;
        while (current) |env| {
            for (env.bindings) |binding| {
                if (binding.module_idx == module_idx and binding.pattern_idx == pattern_idx) {
                    return binding.callable_value;
                }
            }
            current = env.parent;
        }
        return null;
    }
};

pub const ValueLexicalEnv = struct {
    parent: ?*const ValueLexicalEnv = null,
    bindings: []const ValueLexicalBinding,

    pub fn lookup(
        self: *const ValueLexicalEnv,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CaptureValueSource {
        var current: ?*const ValueLexicalEnv = self;
        while (current) |env| {
            for (env.bindings) |binding| {
                if (binding.module_idx == module_idx and binding.pattern_idx == pattern_idx) {
                    return binding.source;
                }
            }
            current = env.parent;
        }
        return null;
    }
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const EnsuredCallableInst = struct {
    id: CallableInstId,
    created: bool,
};

pub const SemanticThread = struct {
    source_context: SourceContext,
    value_env: ?*const ValueLexicalEnv = null,
    callable_env: ?*const CallableLexicalEnv = null,

    pub fn trackedThread(source_context: SourceContext) SemanticThread {
        return .{
            .source_context = source_context,
            .value_env = null,
            .callable_env = null,
        };
    }

    pub fn withValueEnv(
        self: SemanticThread,
        value_env: ?*const ValueLexicalEnv,
    ) SemanticThread {
        return .{
            .source_context = self.source_context,
            .value_env = value_env,
            .callable_env = self.callable_env,
        };
    }

    pub fn withCallableEnv(
        self: SemanticThread,
        callable_env: ?*const CallableLexicalEnv,
    ) SemanticThread {
        return .{
            .source_context = self.source_context,
            .value_env = self.value_env,
            .callable_env = callable_env,
        };
    }

    pub fn requireSourceContext(self: SemanticThread) SourceContext {
        return self.source_context;
    }

    pub fn callableInst(self: SemanticThread) ?CallableInstId {
        return switch (self.source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => null,
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

    pub fn lookupCallableBinding(
        self: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        const callable_env = self.callable_env orelse return null;
        return callable_env.lookup(module_idx, pattern_idx);
    }

    pub fn lookupValueBinding(
        self: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CaptureValueSource {
        const value_env = self.value_env orelse return null;
        return value_env.lookup(module_idx, pattern_idx);
    }
};

// Private traversal memo for one Lambdasolved run. This is implementation
// detail only; solved facts live in `Result`, not in this memo.
const VisitMemo = struct {
    visited_exprs: std.AutoHashMapUnmanaged(ContextExprKey, void),
    in_progress_value_def_exprs: std.AutoHashMapUnmanaged(ContextExprKey, void),
    in_progress_call_results: std.AutoHashMapUnmanaged(CallResultCallableInstKey, void),

    fn init() VisitMemo {
        return .{
            .visited_exprs = .empty,
            .in_progress_value_def_exprs = .empty,
            .in_progress_call_results = .empty,
        };
    }

    fn deinit(self: *VisitMemo, allocator: std.mem.Allocator) void {
        self.visited_exprs.deinit(allocator);
        self.in_progress_value_def_exprs.deinit(allocator);
        self.in_progress_call_results.deinit(allocator);
    }

    fn clearPerScan(self: *VisitMemo) void {
        self.visited_exprs.clearRetainingCapacity();
        self.in_progress_value_def_exprs.clearRetainingCapacity();
        self.in_progress_call_results.clearRetainingCapacity();
    }

    fn hasVisitedExpr(self: *const VisitMemo, key: ContextExprKey) bool {
        return self.visited_exprs.contains(key);
    }

    fn exprVisitStillPending(self: *const VisitMemo, key: ContextExprKey) bool {
        return !self.hasVisitedExpr(key);
    }

    fn beginExprVisit(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        if (self.visited_exprs.contains(key)) return false;
        try self.visited_exprs.put(allocator, key, {});
        return true;
    }

    fn visitExprOnce(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("VisitMemo.visitExprOnce requires a worker value with a run() method");
            }
        }

        if (!try self.beginExprVisit(allocator, key)) return;
        try worker.run();
    }

    fn beginValueDefExpr(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
    ) std.mem.Allocator.Error!bool {
        if (self.in_progress_value_def_exprs.contains(key)) return false;
        try self.in_progress_value_def_exprs.put(allocator, key, {});
        return true;
    }

    fn endValueDefExpr(self: *VisitMemo, key: ContextExprKey) void {
        _ = self.in_progress_value_def_exprs.remove(key);
    }

    fn withValueDefExpr(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: ContextExprKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("VisitMemo.withValueDefExpr requires a worker value with a run() method");
            }
        }

        if (!try self.beginValueDefExpr(allocator, key)) return;
        defer self.endValueDefExpr(key);
        try worker.run();
    }

    fn beginCallResult(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: CallResultCallableInstKey,
    ) std.mem.Allocator.Error!bool {
        if (self.in_progress_call_results.contains(key)) return false;
        try self.in_progress_call_results.put(allocator, key, {});
        return true;
    }

    fn endCallResult(self: *VisitMemo, key: CallResultCallableInstKey) void {
        _ = self.in_progress_call_results.remove(key);
    }

    fn withCallResult(
        self: *VisitMemo,
        allocator: std.mem.Allocator,
        key: CallResultCallableInstKey,
        worker: anytype,
    ) std.mem.Allocator.Error!void {
        const Worker = @TypeOf(worker);
        comptime {
            if (!@hasDecl(Worker, "run")) {
                @compileError("VisitMemo.withCallResult requires a worker value with a run() method");
            }
        }

        if (!try self.beginCallResult(allocator, key)) return;
        defer self.endCallResult(key);
        try worker.run();
    }
};

fn Transform(comptime Driver: type, comptime ResultPtr: type) type {
    return struct {
        allocator: std.mem.Allocator,
        driver: Driver,
        result: ResultPtr,
        traversal: VisitMemo,

        const Self = @This();

        fn init(
            allocator: std.mem.Allocator,
            driver: Driver,
            result: ResultPtr,
        ) Self {
            return .{
                .allocator = allocator,
                .driver = driver,
                .result = result,
                .traversal = VisitMemo.init(),
            };
        }

        fn deinit(self: *Self) void {
            self.traversal.deinit(self.allocator);
        }

        fn run(self: *Self, current_module_idx: u32, root_exprs: []const CIR.Expr.Idx) std.mem.Allocator.Error!void {
            for (root_exprs) |expr_idx| {
                self.traversal.clearPerScan();
                try scanCirValueExpr(
                    self.driver,
                    &self.traversal,
                    self.result,
                    SemanticThread.trackedThread(.{
                        .root_expr = .{
                            .module_idx = current_module_idx,
                            .expr_idx = expr_idx,
                        },
                    }),
                    current_module_idx,
                    expr_idx,
                );
            }

            while (true) {
                var made_progress = false;
                const callable_inst_count = self.result.lambdasolved.callable_insts.items.len;
                var i: usize = 0;
                while (i < callable_inst_count) : (i += 1) {
                    const callable_inst_id: CallableInstId = @enumFromInt(i);
                    if (callableInstHasRealizedRuntimeExpr(self.result, callable_inst_id)) continue;
                    if (!try callableInstReadyForRealization(self.driver, self.result, callable_inst_id)) continue;
                    self.traversal.clearPerScan();
                    try ensureCallableInstRealized(
                        self.driver,
                        &self.traversal,
                        self.result,
                        callable_inst_id,
                    );
                    made_progress = true;
                }
                if (!made_progress) break;
            }
        }
    };
}

pub fn run(
    allocator: std.mem.Allocator,
    current_module_idx: u32,
    root_exprs: []const CIR.Expr.Idx,
    pass: anytype,
    result: anytype,
) std.mem.Allocator.Error!void {
    // This is Lambdasolved's one public CIR traversal boundary: analyze the
    // reachable roots once under explicit semantic contexts, record solved
    // facts, and let later stages consume those facts mechanically.
    var transform = Transform(@TypeOf(pass), @TypeOf(result)).init(allocator, pass, result);
    defer transform.deinit();
    try transform.run(current_module_idx, root_exprs);
}

fn exprVisitStillPendingForThread(
    visit_memo: *const VisitMemo,
    driver: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) bool {
    _ = driver;
    return visit_memo.exprVisitStillPending(cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx));
}

fn scanDemandedValueDefExpr(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const key = cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx);
    const Worker = struct {
        driver_inner: @TypeOf(driver),
        visit_memo_inner: *VisitMemo,
        result_inner: @TypeOf(result),
        thread_inner: SemanticThread,
        module_idx_inner: u32,
        expr_idx_inner: CIR.Expr.Idx,

        fn run(self: @This()) std.mem.Allocator.Error!void {
            try scanCirValueExpr(
                self.driver_inner,
                self.visit_memo_inner,
                self.result_inner,
                self.thread_inner,
                self.module_idx_inner,
                self.expr_idx_inner,
            );
        }
    };

    try visit_memo.withValueDefExpr(
        driver.allocator,
        key,
        Worker{
            .driver_inner = driver,
            .visit_memo_inner = visit_memo,
            .result_inner = result,
            .thread_inner = thread,
            .module_idx_inner = module_idx,
            .expr_idx_inner = expr_idx,
        },
    );
}

fn scanDemandedValueDefExprForExprRef(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    expr_ref: ExprRef,
) std.mem.Allocator.Error!void {
    const demand_thread = if (thread) |current_thread|
        if (sourceContextsEqual(current_thread.requireSourceContext(), expr_ref.source_context))
            current_thread
        else
            try threadForSourceContext(driver, result, expr_ref.source_context)
    else
        try threadForSourceContext(driver, result, expr_ref.source_context);
    try scanDemandedValueDefExpr(
        driver,
        visit_memo,
        result,
        demand_thread,
        expr_ref.module_idx,
        expr_ref.expr_idx,
    );
}

fn scanCirExpr(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirExprWithDirectCallResolution(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
        true,
    );
}

fn scanCirValueExpr(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirValueExprWithDirectCallResolution(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
        true,
    );
}

fn scanCirExprWithDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    return scanCirExprInternal(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
        resolve_direct_calls,
    );
}

fn scanCirValueExprWithDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    return scanCirExprInternal(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
        resolve_direct_calls,
    );
}

fn scanCirExprSpanWithDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirExprWithDirectCallResolution(
            driver,
            visit_memo,
            result,
            thread,
            module_idx,
            child_expr,
            resolve_direct_calls,
        );
    }
}

fn scanCirValueExprSpanWithDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirValueExprWithDirectCallResolution(
            driver,
            visit_memo,
            result,
            thread,
            module_idx,
            child_expr,
            resolve_direct_calls,
        );
    }
}

fn scanCirValueExprWithoutDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    return scanCirValueExprWithDirectCallResolution(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
        false,
    );
}

fn scanCirExprSpan(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirExpr(driver, visit_memo, result, thread, module_idx, child_expr);
    }
}

fn scanCirValueExprSpan(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr| {
        try scanCirValueExpr(driver, visit_memo, result, thread, module_idx, child_expr);
    }
}

fn materializeTemplateExprCallableValueForThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const source_context = thread.requireSourceContext();
    if (readExprCallableValue(result, source_context, module_idx, expr_idx) != null) return;
    const template_id = result.getExprTemplateId(source_context, module_idx, expr_idx) orelse return;
    const template = result.getCallableTemplate(template_id);
    if (template.owner == .root_scope and template.binding == .pattern) {
        return;
    }
    const fn_monotype = try resolveTemplateFnMonotypeForThread(
        driver,
        result,
        thread,
        template_id,
    ) orelse return;
    const callable_inst_id = (try introduceExprCallableValueWithKnownFnMonotype(
        driver,
        visit_memo,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
        template_id,
        fn_monotype,
    )) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: callable template expr ctx={s} module={d} expr={d} template={d} kind={s} owner={s} did not produce a callable value during materialization",
                .{
                    @tagName(source_context),
                    module_idx,
                    @intFromEnum(expr_idx),
                    @intFromEnum(template_id),
                    @tagName(template.kind),
                    @tagName(template.owner),
                },
            );
        }
        unreachable;
    };
    if (template.binding == .anonymous) {
        try ensureCallableValueVariantsRealizedFromThread(
            driver,
            visit_memo,
            result,
            thread,
            .{ .direct = callable_inst_id },
        );
    }
}

fn resolveTemplateFnMonotypeForThread(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    template_id: CallableTemplateId,
) std.mem.Allocator.Error!?ResolvedMonotype {
    const template = result.getCallableTemplate(template_id);
    if (try cm.resolveExprExactMonotypeResolved(
        driver,
        result,
        thread,
        template.module_idx,
        template.cir_expr,
    )) |expr_monotype| {
        if (cm.resolvedIfFunctionMonotype(result, expr_monotype)) |fn_monotype| {
            return fn_monotype;
        }
    }
    return null;
}

fn collectClosureCaptureValueBindings(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    closure_expr_idx: CIR.Expr.Idx,
    closure_expr: CIR.Expr.Closure,
    out: *std.ArrayListUnmanaged(ValueLexicalBinding),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const source_context = thread.requireSourceContext();

    const current_template = if (result.getExprTemplateId(source_context, module_idx, closure_expr_idx)) |template_id|
        result.getCallableTemplate(template_id).*
    else
        null;

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
                visit_memo,
                result,
                thread,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue;
            break :blk source;
        };

        switch (capture_source) {
            .bound_expr => |bound_expr| try scanDemandedValueDefExprForExprRef(
                driver,
                visit_memo,
                result,
                thread,
                bound_expr.expr_ref,
            ),
            .specialized_param, .lexical_binding => {},
        }

        try out.append(driver.allocator, .{
            .module_idx = module_idx,
            .pattern_idx = capture.pattern_idx,
            .source = capture_source,
        });
    }
}

fn ensureCallableInstCaptureBindingsFromThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    active_thread: ?SemanticThread,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    const template = result.getCallableTemplate(result.getCallableInst(callable_inst_id).template).*;
    if (template.kind != .closure) return;
    if (result.lambdasolved.getCallableInstCaptureBindings(callable_inst_id).len != 0) return;

    const module_env = driver.all_module_envs[template.module_idx];
    const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
        .e_closure => |closure| closure,
        else => unreachable,
    };
    const capture_thread = active_thread orelse blk: {
        if (sourceContextCallableInstId(result.getCallableInst(callable_inst_id).defining_source_context)) |owner_callable_inst_id| {
            break :blk try threadForCallableInst(driver, result, owner_callable_inst_id);
        }
        if (module_env.store.sliceCaptures(closure_expr.captures).len == 0 or
            closureHasOnlySelfRecursiveCaptures(module_env, template, closure_expr))
        {
            return;
        }
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: closure callable inst {d} template expr {d} in module {d} required an active lexical thread to materialize capture bindings",
                .{
                    @intFromEnum(callable_inst_id),
                    @intFromEnum(template.cir_expr),
                    template.module_idx,
                },
            );
        }
        unreachable;
    };
    var capture_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
    defer capture_bindings.deinit(driver.allocator);
    try collectClosureCaptureValueBindings(
        driver,
        visit_memo,
        result,
        capture_thread,
        template.module_idx,
        template.runtime_expr,
        closure_expr,
        &capture_bindings,
    );
    for (capture_bindings.items) |*capture_binding| {
        switch (capture_binding.source) {
            .bound_expr => {
                const capture_callable_value = if (capture_thread.lookupCallableBinding(capture_binding.module_idx, capture_binding.pattern_idx)) |callable_value|
                    callable_value
                else
                    resolveBoundBindingCallableValueForThread(
                        driver,
                        visit_memo,
                        result,
                        capture_thread,
                        capture_binding.module_idx,
                        capture_binding.pattern_idx,
                    ) orelse continue;
                capture_binding.source = .{ .lexical_binding = .{
                    .callable_inst = callable_inst_id,
                    .module_idx = capture_binding.module_idx,
                    .pattern_idx = capture_binding.pattern_idx,
                    .callable_value = capture_callable_value,
                } };
            },
            .specialized_param, .lexical_binding => {},
        }
    }
    if (capture_bindings.items.len == 0) return;
    try result.lambdasolved.recordCallableInstCaptureBindings(
        driver.allocator,
        callable_inst_id,
        capture_bindings.items,
    );
}

fn captureBindingThreadForCallableInst(
    driver: anytype,
    result: anytype,
    active_thread: ?SemanticThread,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!SemanticThread {
    const defining_source_context = result.getCallableInst(callable_inst_id).defining_source_context;
    if (active_thread) |thread| {
        if (std.meta.eql(thread.requireSourceContext(), defining_source_context)) {
            return thread;
        }
    }
    return threadForSourceContext(driver, result, defining_source_context);
}

fn closureHasOnlySelfRecursiveCaptures(
    module_env: *const ModuleEnv,
    template: CallableTemplate,
    closure_expr: CIR.Expr.Closure,
) bool {
    const binding_pattern = switch (template.binding) {
        .pattern => |pattern_idx| pattern_idx,
        .anonymous => return false,
    };
    const captures = module_env.store.sliceCaptures(closure_expr.captures);
    if (captures.len == 0) return false;
    for (captures) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        if (capture.pattern_idx != binding_pattern) return false;
    }
    return true;
}

pub fn findDefByPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    for (defs) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
    }
    return null;
}

fn callableInstsShareRecursiveGroup(
    result: anytype,
    callable_inst_id: CallableInstId,
    capture_callable_inst_id: CallableInstId,
) bool {
    const recursive_group = result.getCallableInst(callable_inst_id).recursive_group;
    if (recursive_group.isEmpty()) return false;
    for (result.lambdasolved.getCallableInstGroupEntries(recursive_group)) |group_member| {
        if (group_member == capture_callable_inst_id) return true;
    }
    return false;
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

fn bindingSourceExprRef(source: CaptureValueSource) ?ExprRef {
    return switch (source) {
        .bound_expr => |bound_expr| bound_expr.expr_ref,
        .specialized_param, .lexical_binding => null,
    };
}

fn directCalleeLowLevelOp(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CIR.Expr.LowLevel {
    const source_context = thread.requireSourceContext();
    if (result.getExprLowLevelOp(source_context, module_idx, expr_idx)) |low_level_op| {
        return low_level_op;
    }
    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        if (result.getExprLowLevelOp(origin.source_context, origin.module_idx, origin.expr_idx)) |low_level_op| {
            return low_level_op;
        }
    }

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    if (expr == .e_lookup_local) {
        const lookup = expr.e_lookup_local;
        if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
            if (bindingSourceExprRef(source)) |source_expr_ref| {
                if (result.getExprLowLevelOp(
                    source_expr_ref.source_context,
                    source_expr_ref.module_idx,
                    source_expr_ref.expr_idx,
                )) |low_level_op| {
                    return low_level_op;
                }
                if (result.getExprOriginExpr(
                    source_expr_ref.source_context,
                    source_expr_ref.module_idx,
                    source_expr_ref.expr_idx,
                )) |origin| {
                    if (result.getExprLowLevelOp(origin.source_context, origin.module_idx, origin.expr_idx)) |low_level_op| {
                        return low_level_op;
                    }
                }
            }
        }
    }

    return null;
}

fn requireSpecializedParamMonotype(
    result: anytype,
    specialized_param: @FieldType(CaptureValueSource, "specialized_param"),
) ResolvedMonotype {
    const callable_inst = result.getCallableInst(specialized_param.callable_inst);
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };
    if (specialized_param.param_index >= fn_mono.args.len) unreachable;
    return cm.resolvedMonotype(
        result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, specialized_param.param_index),
        callable_inst.fn_monotype_module_idx,
    );
}

fn resolveSpecializedParamCallableValue(
    result: anytype,
    specialized_param: @FieldType(CaptureValueSource, "specialized_param"),
    projections: []const Lambdamono.CallableParamProjection,
) ?CallableValue {
    const callable_inst = result.getCallableInst(specialized_param.callable_inst);
    for (result.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
        if (spec.param_index != specialized_param.param_index) continue;
        if (!callableParamProjectionsEqual(
            result.getCallableParamProjectionEntries(spec.projections),
            projections,
        )) continue;
        return spec.callable_value;
    }
    return null;
}

pub fn captureValueSourceForClosurePattern(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_env: *const ModuleEnv,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) std.mem.Allocator.Error!?CaptureValueSource {
    if (findDefByPattern(module_env, pattern_idx) != null) return null;
    if (thread.lookupValueBinding(module_idx, pattern_idx)) |source| {
        return switch (source) {
            .lexical_binding => |lexical| blk: {
                if (thread.lookupCallableBinding(module_idx, pattern_idx)) |callable_value| {
                    var updated = lexical;
                    updated.callable_value = callable_value;
                    if (updated.callable_inst == null) updated.callable_inst = thread.callableInst();
                    break :blk .{ .lexical_binding = updated };
                }
                if (resolveBoundBindingCallableValueForThread(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    module_idx,
                    pattern_idx,
                )) |callable_value| {
                    var updated = lexical;
                    updated.callable_value = callable_value;
                    if (updated.callable_inst == null) updated.callable_inst = thread.callableInst();
                    break :blk .{ .lexical_binding = updated };
                }
                break :blk source;
            },
            .bound_expr, .specialized_param => source,
        };
    }
    if (thread.lookupCallableBinding(module_idx, pattern_idx)) |callable_value| {
        return .{ .lexical_binding = .{
            .callable_inst = thread.callableInst(),
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
            .callable_value = callable_value,
        } };
    }
    const lexical_callable_inst = thread.callableInst() orelse {
        if (std.debug.runtime_safety) {
            var pattern_buf: [64]u8 = undefined;
            std.debug.panic(
                "Lambdasolved invariant violated: unresolved lexical capture pattern {d} ({s}) in module {d} escaped callable-inst context; source_context={s} top_level_def={?d}",
                .{
                    @intFromEnum(pattern_idx),
                    debugPatternSummary(module_env, pattern_idx, &pattern_buf),
                    module_idx,
                    @tagName(thread.requireSourceContext()),
                    if (findDefByPattern(module_env, pattern_idx)) |def_idx| @intFromEnum(def_idx) else null,
                },
            );
        }
        unreachable;
    };
    return .{ .lexical_binding = .{
        .callable_inst = lexical_callable_inst,
        .module_idx = module_idx,
        .pattern_idx = pattern_idx,
        .callable_value = null,
    } };
}

pub fn resolveBoundCaptureExprMonotype(
    driver: anytype,
    result: anytype,
    bound_expr: @FieldType(CaptureValueSource, "bound_expr"),
) std.mem.Allocator.Error!?ResolvedMonotype {
    if (std.debug.runtime_safety) {
        const all_module_envs_len = driver.all_module_envs.len;
        if (bound_expr.expr_ref.module_idx >= all_module_envs_len) {
            std.debug.panic(
                "Lambdasolved invariant violated: bound capture expr ref had invalid module_idx={d} expr_idx={d} source_context={s} projections={d} all_module_envs_len={d}",
                .{
                    bound_expr.expr_ref.module_idx,
                    @intFromEnum(bound_expr.expr_ref.expr_idx),
                    @tagName(bound_expr.expr_ref.source_context),
                    bound_expr.expr_ref.projections.len,
                    all_module_envs_len,
                },
            );
        }
    }
    const source_thread = SemanticThread.trackedThread(bound_expr.expr_ref.source_context);
    var source_mono = (try cm.resolveExprMonotypeResolved(
        driver,
        result,
        source_thread,
        bound_expr.expr_ref.module_idx,
        bound_expr.expr_ref.expr_idx,
    )) orelse return null;
    for (result.getValueProjectionEntries(bound_expr.expr_ref.projections)) |projection| {
        source_mono = try cm.projectResolvedMonotypeByValueProjection(driver, result, source_mono, projection);
        if (source_mono.isNone()) return null;
    }
    return source_mono;
}

fn resolveLexicalBindingThread(
    driver: anytype,
    result: anytype,
    lexical_binding: @FieldType(CaptureValueSource, "lexical_binding"),
    lexical_thread: ?SemanticThread,
) std.mem.Allocator.Error!SemanticThread {
    const lexical_callable_inst = lexical_binding.callable_inst orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: lexical binding pattern {d} in module {d} required deferred callable-inst reconstruction without explicit callable-inst owner",
                .{ @intFromEnum(lexical_binding.pattern_idx), lexical_binding.module_idx },
            );
        }
        unreachable;
    };
    return if (lexical_thread) |thread|
        if (thread.callableInst()) |thread_callable_inst|
            if (thread_callable_inst == lexical_callable_inst)
                thread
            else
                try threadForCallableInst(driver, result, lexical_callable_inst)
        else
            try threadForCallableInst(driver, result, lexical_callable_inst)
    else
        try threadForCallableInst(driver, result, lexical_callable_inst);
}

pub fn resolveCaptureValueSourceMonotype(
    driver: anytype,
    result: anytype,
    lexical_thread: ?SemanticThread,
    capture_context_callable_inst: CallableInstId,
    capture_value_source: CaptureValueSource,
    capture_callable_value: ?CallableValue,
) std.mem.Allocator.Error!ResolvedMonotype {
    return switch (capture_value_source) {
        .bound_expr => |bound_expr| blk: {
            if (capture_callable_value) |callable_value| {
                break :blk result.getCallableValueSourceMonotype(callable_value);
            }
            if (try resolveBoundCaptureExprMonotype(driver, result, bound_expr)) |binding_mono| {
                break :blk binding_mono;
            }
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: missing exact contextual monotype for captured bound expr source_ctx={s} module={d} expr={d} projections={d} context_callable_inst={d}",
                    .{
                        @tagName(bound_expr.expr_ref.source_context),
                        bound_expr.expr_ref.module_idx,
                        @intFromEnum(bound_expr.expr_ref.expr_idx),
                        bound_expr.expr_ref.projections.len,
                        @intFromEnum(capture_context_callable_inst),
                    },
                );
            }
            unreachable;
        },
        .specialized_param => |specialized_param| requireSpecializedParamMonotype(result, specialized_param),
        .lexical_binding => |lexical| blk: {
            if (lexical.callable_value) |callable_value| {
                break :blk result.getCallableValueSourceMonotype(callable_value);
            }
            const resolved_lexical_thread = try resolveLexicalBindingThread(
                driver,
                result,
                lexical,
                lexical_thread,
            );
            const binding_mono = try cm.resolveTypeVarMonotypeResolved(
                driver,
                result,
                resolved_lexical_thread,
                lexical.module_idx,
                ModuleEnv.varFrom(lexical.pattern_idx),
            );
            if (!binding_mono.isNone()) break :blk binding_mono;
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: missing exact contextual monotype for lexical capture pattern module={d} pattern={d} lexical_callable_inst={d} context_callable_inst={d}",
                    .{
                        lexical.module_idx,
                        @intFromEnum(lexical.pattern_idx),
                        @intFromEnum(lexical.callable_inst orelse unreachable),
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
    visit_memo: *VisitMemo,
    result: anytype,
    lexical_thread: ?SemanticThread,
    capture_context_callable_inst: CallableInstId,
    capture_value_source: CaptureValueSource,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    return switch (capture_value_source) {
        .bound_expr => |bound_expr| blk: {
            if (try resolveExprRefCallableValue(driver, visit_memo, result, lexical_thread, bound_expr.expr_ref, visiting)) |callable_value| {
                break :blk callable_value;
            }
            if (bound_expr.expr_ref.projections.isEmpty()) {
                const maybe_template_id = result.getExprTemplateId(
                    bound_expr.expr_ref.source_context,
                    bound_expr.expr_ref.module_idx,
                    bound_expr.expr_ref.expr_idx,
                );
                if (maybe_template_id) |template_id| {
                    const fn_monotype = (try resolveBoundCaptureExprMonotype(
                        driver,
                        result,
                        bound_expr,
                    )) orelse break :blk null;
                    _ = try introduceExprCallableValueWithKnownFnMonotype(
                        driver,
                        visit_memo,
                        result,
                        lexical_thread,
                        bound_expr.expr_ref.source_context,
                        bound_expr.expr_ref.module_idx,
                        bound_expr.expr_ref.expr_idx,
                        template_id,
                        fn_monotype,
                    );
                    if (try resolveExprRefCallableValue(driver, visit_memo, result, lexical_thread, bound_expr.expr_ref, visiting)) |callable_value| {
                        break :blk callable_value;
                    }
                }
            }
            break :blk null;
        },
        .specialized_param => |specialized_param| blk: {
            break :blk resolveSpecializedParamCallableValue(result, specialized_param, &.{});
        },
        .lexical_binding => |lexical| blk: {
            if (lexical.callable_value) |callable_value| {
                break :blk callable_value;
            }
            const resolved_lexical_thread = try resolveLexicalBindingThread(
                driver,
                result,
                lexical,
                lexical_thread,
            );
            if (resolveBoundBindingCallableValueForThread(
                driver,
                visit_memo,
                result,
                resolved_lexical_thread,
                lexical.module_idx,
                lexical.pattern_idx,
            )) |callable_value| {
                break :blk callable_value;
            }

            if (std.debug.runtime_safety) {
                const pattern_mono = try cm.resolveTypeVarMonotypeResolved(
                    driver,
                    result,
                    lexical_thread,
                    lexical.module_idx,
                    ModuleEnv.varFrom(lexical.pattern_idx),
                );
                if (!pattern_mono.isNone()) {
                    if (result.context_mono.monotype_store.getMonotype(pattern_mono.idx) == .func) {
                        const lexical_callable_inst = lexical.callable_inst orelse unreachable;
                        const current_template =
                            result.getCallableTemplate(result.getCallableInst(lexical_callable_inst).template).*;
                        const lexical_binding_pattern = switch (current_template.binding) {
                            .pattern => |binding_pattern| @intFromEnum(binding_pattern),
                            .anonymous => null,
                        };
                        const stored_capture_bindings =
                            result.lambdasolved.getCallableInstCaptureBindings(lexical_callable_inst);
                        const debug_lexical_thread = try resolveLexicalBindingThread(
                            driver,
                            result,
                            lexical,
                            lexical_thread,
                        );
                        const resolved_lookup_callable = debug_lexical_thread.lookupCallableBinding(
                            lexical.module_idx,
                            lexical.pattern_idx,
                        );
                        const resolved_lookup_value = debug_lexical_thread.lookupValueBinding(
                            lexical.module_idx,
                            lexical.pattern_idx,
                        );
                        const lexical_module_env = driver.all_module_envs[lexical.module_idx];
                        var capture_pattern_buf: [64]u8 = undefined;
                        var binding_pattern_buf: [64]u8 = undefined;
                        const capture_pattern_summary = debugPatternSummary(
                            lexical_module_env,
                            lexical.pattern_idx,
                            &capture_pattern_buf,
                        );
                        const lexical_binding_summary = switch (current_template.binding) {
                            .pattern => |binding_pattern| debugPatternSummary(
                                driver.all_module_envs[current_template.module_idx],
                                binding_pattern,
                                &binding_pattern_buf,
                            ),
                            .anonymous => "anonymous",
                        };
                        std.debug.panic(
                            "Lambdasolved invariant violated: lexical function capture pattern {d} ({s}) in module {d} under lexical callable_inst {d} had no callable value while seeding closure callable_inst {d}; callable_param_specs={d} template_expr={d} template_binding_pattern={?d} ({s}) template_arg_patterns={any} stored_capture_bindings={any} runtime_capture_fields={d} resolved_lookup_callable={?any} resolved_lookup_value={?s}",
                            .{
                                @intFromEnum(lexical.pattern_idx),
                                capture_pattern_summary,
                                lexical.module_idx,
                                @intFromEnum(lexical_callable_inst),
                                @intFromEnum(capture_context_callable_inst),
                                result.getCallableParamSpecEntries(result.getCallableInst(lexical_callable_inst).callable_param_specs).len,
                                @intFromEnum(current_template.cir_expr),
                                lexical_binding_pattern,
                                lexical_binding_summary,
                                driver.all_module_envs[current_template.module_idx].store.slicePatterns(current_template.arg_patterns),
                                stored_capture_bindings,
                                result.getCaptureFields(result.getCallableDefForInst(lexical_callable_inst).captures).len,
                                resolved_lookup_callable,
                                if (resolved_lookup_value) |value| @tagName(value) else null,
                            },
                        );
                    }
                }
            }

            break :blk null;
        },
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
                    try cm.remapMonotypeBetweenModules(
                        driver,
                        result,
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
    try updateCallableDefRuntimeSemantics(
        driver,
        result,
        callable_inst_id,
        runtime_expr_ref,
        body_expr_ref,
        capture_fields,
        next_runtime_value,
    );
}

fn resolveCallableInstRuntimeValueWithoutRealization(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!RuntimeValue {
    if (callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) {
        return result.getCallableInst(callable_inst_id).runtime_value;
    }

    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    switch (template.kind) {
        .lambda, .hosted_lambda => return .direct_lambda,
        .top_level_def => unreachable,
        .closure => {},
    }

    const closure_thread = try threadForCallableInst(driver, result, callable_inst_id);
    return resolveClosureRuntimeValueFromCaptureBindings(
        driver,
        visit_memo,
        result,
        closure_thread,
        template.module_idx,
        template.runtime_expr,
        result.lambdasolved.getCallableInstCaptureBindings(callable_inst_id),
        callable_inst_id,
    );
}

fn resolveClosureRuntimeValueFromCaptureBindings(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    enclosing_thread: SemanticThread,
    module_idx: u32,
    closure_expr_idx: CIR.Expr.Idx,
    capture_bindings: []const ValueLexicalBinding,
    closure_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!RuntimeValue {
    var runtime_field_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer runtime_field_monotypes.deinit(driver.allocator);
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);

    const closure_callable_def = result.getCallableDefForInst(closure_callable_inst_id);

    for (capture_bindings) |capture| {
        const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
            enclosing_thread.requireSourceContext(),
            closure_callable_inst_id,
        );
        const capture_callable_value = try resolveCaptureCallableValue(
            driver,
            visit_memo,
            result,
            enclosing_thread,
            capture_context_callable_inst,
            capture.source,
            &visiting,
        );
        const capture_mono = try resolveCaptureValueSourceMonotype(
            driver,
            result,
            enclosing_thread,
            capture_context_callable_inst,
            capture.source,
            capture_callable_value,
        );
        if (capture_mono.isNone()) {
            std.debug.panic(
                "Lambdasolved invariant violated: missing exact capture monotype while resolving runtime shape for closure callable_inst={d} module={d} closure_expr={d} pattern={d}",
                .{
                    @intFromEnum(closure_callable_inst_id),
                    module_idx,
                    @intFromEnum(closure_expr_idx),
                    @intFromEnum(capture.pattern_idx),
                },
            );
        }

        if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func) {
            const resolved_capture_callable_value = capture_callable_value orelse std.debug.panic(
                "Lambdasolved invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no executable callable value fact while resolving runtime shape",
                .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
            );
            switch (resolved_capture_callable_value) {
                .direct => |capture_callable_inst_id| switch (try resolveCallableInstRuntimeValueWithoutRealization(
                    driver,
                    visit_memo,
                    result,
                    capture_callable_inst_id,
                )) {
                    .direct_lambda => {},
                    .closure => |closure_value| {
                        const field_monotype = if (closure_value.capture_tuple_monotype.module_idx == closure_callable_def.module_idx)
                            closure_value.capture_tuple_monotype.idx
                        else
                            try cm.remapMonotypeBetweenModules(
                                driver,
                                result,
                                closure_value.capture_tuple_monotype.idx,
                                closure_value.capture_tuple_monotype.module_idx,
                                closure_callable_def.module_idx,
                            );
                        try runtime_field_monotypes.append(driver.allocator, field_monotype);
                    },
                },
                .packed_fn => |packed_fn| {
                    const field_monotype = if (packed_fn.runtime_monotype.module_idx == closure_callable_def.module_idx)
                        packed_fn.runtime_monotype.idx
                    else
                        try cm.remapMonotypeBetweenModules(
                            driver,
                            result,
                            packed_fn.runtime_monotype.idx,
                            packed_fn.runtime_monotype.module_idx,
                            closure_callable_def.module_idx,
                        );
                    try runtime_field_monotypes.append(driver.allocator, field_monotype);
                },
            }
            continue;
        }

        const field_monotype = if (capture_mono.module_idx == closure_callable_def.module_idx)
            capture_mono.idx
        else
            try cm.remapMonotypeBetweenModules(
                driver,
                result,
                capture_mono.idx,
                capture_mono.module_idx,
                closure_callable_def.module_idx,
            );
        try runtime_field_monotypes.append(driver.allocator, field_monotype);
    }

    if (runtime_field_monotypes.items.len == 0) return .direct_lambda;

    const field_span = try result.context_mono.monotype_store.addIdxSpan(driver.allocator, runtime_field_monotypes.items);
    return .{ .closure = .{
        .capture_tuple_monotype = cm.resolvedMonotype(
            try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .tuple = .{ .elems = field_span } }),
            closure_callable_def.module_idx,
        ),
    } };
}

pub fn finalizeCallableDefForCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    enclosing_thread: SemanticThread,
    module_idx: u32,
    closure_expr_idx: CIR.Expr.Idx,
    capture_bindings: []const ValueLexicalBinding,
    closure_callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    var capture_fields = std.ArrayList(CaptureField).empty;
    defer capture_fields.deinit(driver.allocator);
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);
    const closure_template = result.getCallableTemplate(result.getCallableInst(closure_callable_inst_id).template);

    for (capture_bindings) |capture| {
        const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
            enclosing_thread.requireSourceContext(),
            closure_callable_inst_id,
        );
        const capture_callable_value = try resolveCaptureCallableValue(
            driver,
            visit_memo,
            result,
            enclosing_thread,
            capture_context_callable_inst,
            capture.source,
            &visiting,
        );
        const capture_mono = try resolveCaptureValueSourceMonotype(
            driver,
            result,
            enclosing_thread,
            capture_context_callable_inst,
            capture.source,
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
                                callableInstsShareRecursiveGroup(
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
                    .direct => |capture_callable_inst_id| switch (try resolveCallableInstRuntimeValueWithoutRealization(
                        driver,
                        visit_memo,
                        result,
                        capture_callable_inst_id,
                    )) {
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
            .source = switch (capture.source) {
                .bound_expr => |bound_expr| if (resolved_capture_callable_value) |callable_value|
                    .{ .lexical_binding = .{
                        .callable_inst = capture_context_callable_inst,
                        .module_idx = module_idx,
                        .pattern_idx = capture.pattern_idx,
                        .callable_value = callable_value,
                    } }
                else
                    .{ .bound_expr = .{
                        .expr_ref = bound_expr.expr_ref,
                    } },
                .specialized_param, .lexical_binding => capture.source,
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

pub fn appendClosureCaptureCallableBindings(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    enclosing_thread: SemanticThread,
    capture_bindings: []const ValueLexicalBinding,
    closure_callable_inst_id: CallableInstId,
    out: *std.ArrayListUnmanaged(CallableLexicalBinding),
) std.mem.Allocator.Error!void {
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);

    for (capture_bindings) |capture| {
        const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
            enclosing_thread.requireSourceContext(),
            closure_callable_inst_id,
        );
        const capture_callable_value = (try resolveCaptureCallableValue(
            driver,
            visit_memo,
            result,
            enclosing_thread,
            capture_context_callable_inst,
            capture.source,
            &visiting,
        )) orelse continue;
        try out.append(driver.allocator, .{
            .module_idx = capture.module_idx,
            .pattern_idx = capture.pattern_idx,
            .callable_value = capture_callable_value,
        });
    }
}

pub fn appendClosureCaptureValueBindings(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    enclosing_thread: SemanticThread,
    module_idx: u32,
    closure_expr: CIR.Expr.Closure,
    out: *std.ArrayListUnmanaged(ValueLexicalBinding),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];

    for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_value_source = (try captureValueSourceForClosurePattern(
            driver,
            visit_memo,
            result,
            enclosing_thread,
            module_env,
            module_idx,
            capture.pattern_idx,
        )) orelse continue;
        try out.append(driver.allocator, .{
            .module_idx = module_idx,
            .pattern_idx = capture.pattern_idx,
            .source = capture_value_source,
        });
    }
}

fn realizeCallableArgSemanticsSlice(
    driver: anytype,
    visit_memo: *VisitMemo,
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
            visit_memo,
            result,
            thread,
            thread.requireSourceContext(),
            module_idx,
            arg_expr_idx,
            &visiting,
        );
        _ = getValueExprCallableValueForThread(driver, visit_memo, result, thread, module_idx, arg_expr_idx);
    }
}

pub fn collectCallableParamSpecsFromFunctionArgument(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    param_index: u16,
    projections: *std.ArrayListUnmanaged(ValueProjection.Projection),
    out: anytype,
) std.mem.Allocator.Error!bool {
    var demand_visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer demand_visiting.deinit(driver.allocator);

    const module_env = driver.all_module_envs[module_idx];
    const projection_entries = try addCallableParamProjectionEntries(
        driver,
        result,
        projections.items,
    );
    const source_expr_ref: ?ExprRef = blk: {
        const expr = module_env.store.getExpr(expr_idx);
        if (expr == .e_lookup_local) {
            const lookup = expr.e_lookup_local;
            if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
                switch (source) {
                    .bound_expr => |bound_expr| break :blk .{
                        .source_context = bound_expr.expr_ref.source_context,
                        .module_idx = bound_expr.expr_ref.module_idx,
                        .expr_idx = bound_expr.expr_ref.expr_idx,
                        .projections = if (bound_expr.expr_ref.projections.isEmpty())
                            projection_entries
                        else
                            try appendValueProjectionEntries(
                                driver,
                                result,
                                bound_expr.expr_ref.projections,
                                result.getValueProjectionEntries(projection_entries),
                            ),
                    },
                    .lexical_binding, .specialized_param => break :blk null,
                }
            }
        }
        break :blk .{
            .source_context = source_context,
            .module_idx = module_idx,
            .expr_idx = expr_idx,
            .projections = projection_entries,
        };
    };

    if (source_expr_ref) |expr_ref| {
        try propagateDemandedCallableFnMonotypeToExprRef(
            driver,
            visit_memo,
            result,
            thread,
            expr_ref,
            fn_monotype,
            fn_monotype_module_idx,
            &demand_visiting,
        );
    }

    if (try resolveProjectedExprCallableValue(
        driver,
        visit_memo,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
        projections.items,
        &demand_visiting,
    )) |callable_value| {
        try appendCallableParamSpecEntry(driver, result, out, .{
            .param_index = param_index,
            .projections = try addCallableParamProjectionEntries(
                driver,
                result,
                projections.items,
            ),
            .callable_value = callable_value,
        });
        return true;
    }

    return false;
}

pub fn collectDirectCallCallableParamSpecs(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
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
            visit_memo,
            result,
            thread,
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

fn ensureCallableInstRealized(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    if (callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) return;
    try realizeCallableInst(driver, visit_memo, result, callable_inst_id);
}

fn callableInstReadyForRealization(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!bool {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => return true,
    };
    const arg_patterns = driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
    if (fn_mono.args.len != arg_patterns.len) return false;

    for (result.context_mono.monotype_store.getIdxSpan(fn_mono.args), 0..) |param_mono, param_index| {
        if (!try monotypeRequiresCallableParamSpec(driver, result, param_mono)) continue;
        for (result.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
            if (spec.param_index == param_index and spec.projections.isEmpty()) break;
        } else {
            if ((try inferCallableParamPatternValueForCallableInst(
                driver,
                result,
                callable_inst_id,
                @intCast(param_index),
                cm.resolvedMonotype(param_mono, callable_inst.fn_monotype_module_idx),
            )) == null) {
                return false;
            }
        }
    }

    return true;
}

fn requireCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    defining_source_context: SourceContext,
    template_id: CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!CallableInstId {
    const callable_inst_id = (try ensureCallableInstRecord(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        &.{},
    )).id;
    const capture_thread = try captureBindingThreadForCallableInst(
        driver,
        result,
        thread,
        callable_inst_id,
    );
    try ensureCallableInstCaptureBindingsFromThread(
        driver,
        visit_memo,
        result,
        capture_thread,
        callable_inst_id,
    );
    if (try callableInstReadyForRealization(driver, result, callable_inst_id)) {
        try ensureCallableInstRealized(driver, visit_memo, result, callable_inst_id);
    }
    return callable_inst_id;
}

fn requireCallableInstWithCallableParamSpecs(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    defining_source_context: SourceContext,
    template_id: CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    callable_param_specs: []const Lambdamono.CallableParamSpecEntry,
) std.mem.Allocator.Error!CallableInstId {
    const callable_inst_id = (try ensureCallableInstRecord(
        driver,
        result,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs,
    )).id;
    const capture_thread = try captureBindingThreadForCallableInst(
        driver,
        result,
        thread,
        callable_inst_id,
    );
    try ensureCallableInstCaptureBindingsFromThread(
        driver,
        visit_memo,
        result,
        capture_thread,
        callable_inst_id,
    );
    try ensureCallableInstRealized(driver, visit_memo, result, callable_inst_id);
    return callable_inst_id;
}

pub fn collectCallableParamSpecsFromArgument(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: anytype,
    monotype_module_idx: u32,
    param_index: u16,
    projections: *std.ArrayListUnmanaged(ValueProjection.Projection),
    out: anytype,
) std.mem.Allocator.Error!bool {
    switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .func => {
            return collectCallableParamSpecsFromFunctionArgument(
                driver,
                visit_memo,
                result,
                thread,
                source_context,
                module_idx,
                expr_idx,
                monotype,
                monotype_module_idx,
                param_index,
                projections,
                out,
            );
        },
        else => {},
    }

    if (result.context_mono.monotype_store.isOpaque(monotype)) return true;

    switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .record => |record| {
            for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                try projections.append(driver.allocator, .{ .field = field.name });
                defer _ = projections.pop();

                if (!try collectCallableParamSpecsFromArgument(
                    driver,
                    visit_memo,
                    result,
                    thread,
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
                    visit_memo,
                    result,
                    thread,
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

fn bindCurrentDispatchFromFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) std.mem.Allocator.Error!void {
    const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => return,
    };

    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);

    if (actual_args.items.len != fn_mono.args.len) return;

    for (actual_args.items, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            arg_expr_idx,
            param_mono,
            fn_monotype_module_idx,
        );
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) {
            try propagateDemandedValueMonotypeToValueExpr(
                driver,
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
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            fn_mono.ret,
            fn_monotype_module_idx,
        );
    }
}

fn exprUsesFunctionMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!bool {
    const expr_mono = (try cm.resolveExprMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
    )) orelse return false;
    return result.context_mono.monotype_store.getMonotype(expr_mono.idx) == .func;
}

fn scanDispatchArgsWithDirectCallResolution(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    resolve_direct_calls: bool,
    scan_function_args: bool,
) std.mem.Allocator.Error!void {
    for (arg_exprs) |arg_expr_idx| {
        const is_function_arg = try exprUsesFunctionMonotype(
            driver,
            result,
            thread,
            module_idx,
            arg_expr_idx,
        );
        if (is_function_arg != scan_function_args) continue;
        try scanCirExprWithDirectCallResolution(
            driver,
            visit_memo,
            result,
            thread,
            module_idx,
            arg_expr_idx,
            resolve_direct_calls,
        );
    }
}

pub fn specializeDirectCallExactCallable(
    driver: anytype,
    visit_memo: *VisitMemo,
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
    const defining_source_context = resolveTemplateDefiningSourceContext(result, template_source_context, template);
    const arg_exprs = driver.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
    const exact_desired_fn_monotype = (try resolveDemandedDirectCallFnMonotype(
        driver,
        visit_memo,
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
        visit_memo,
        result,
        thread,
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
        visit_memo,
        result,
        thread,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs.items,
    );
}

pub fn assignCallableArgCallableInstsFromParams(
    driver: anytype,
    visit_memo: *VisitMemo,
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
        if (thread.callableInst() == null and template.kind == .closure) {
            continue;
        }
        const template_source_context = requireTemplateDemandSourceContextForExpr(
            driver,
            visit_memo,
            result,
            thread.requireSourceContext(),
            module_idx,
            arg_expr_idx,
            template_id,
        );
        const defining_source_context = resolveTemplateDefiningSourceContext(
            result,
            template_source_context,
            template.*,
        );
        const callable_inst_id = (try ensureCallableInstRecord(
            driver,
            result,
            defining_source_context,
            template_id,
            param_mono,
            fn_monotype_module_idx,
            &.{},
        )).id;
        const capture_thread = try captureBindingThreadForCallableInst(
            driver,
            result,
            thread,
            callable_inst_id,
        );
        try ensureCallableInstCaptureBindingsFromThread(
            driver,
            visit_memo,
            result,
            capture_thread,
            callable_inst_id,
        );
        if (try callableInstReadyForRealization(driver, result, callable_inst_id)) {
            try ensureCallableInstRealized(driver, visit_memo, result, callable_inst_id);
        }

        try setExprDirectCallable(
            driver,
            result,
            thread.requireSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );

        switch (module_env.store.getExpr(arg_expr_idx)) {
            .e_lookup_local => {
                try setExprDirectCallable(
                    driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    arg_expr_idx,
                    callable_inst_id,
                );
                try propagateLookupSourceExprCallableValue(driver, result, thread, module_idx, arg_expr_idx, callable_inst_id);
            },
            .e_lookup_external, .e_lookup_required => {
                try setExprDirectCallable(
                    driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    arg_expr_idx,
                    callable_inst_id,
                );
                try propagateLookupSourceExprCallableValue(driver, result, thread, module_idx, arg_expr_idx, callable_inst_id);
            },
            .e_lambda, .e_closure, .e_hosted_lambda => try setExprDirectCallable(
                driver,
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

pub fn prepareCallableArgsForCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
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
        visit_memo,
        result,
        thread,
        module_idx,
        arg_exprs,
        callable_inst_fn_mono.args,
        callable_inst.fn_monotype_module_idx,
    );
}

pub fn assignCallableArgCallableInstsFromCallMonotype(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!void {
    if (readExprCallSite(result, thread.requireSourceContext(), module_idx, call_expr_idx)) |call_site| {
        if (Lambdamono.exactCallableInstFromCallSite(call_site) != null) return;
    }

    const call_fn_monotype = try resolveDemandedDirectCallFnMonotype(
        driver,
        visit_memo,
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
        visit_memo,
        result,
        thread,
        module_idx,
        arg_exprs,
        fn_mono.args,
        call_fn_monotype.?.module_idx,
    );
    try realizeCallableArgSemanticsSlice(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        arg_exprs,
    );
}

pub fn finalizeResolvedDirectCallCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
    callee_expr: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    if (try callableInstReadyForRealization(driver, result, callable_inst_id)) {
        try ensureCallableInstRealized(driver, visit_memo, result, callable_inst_id);
    }
    const module_env = driver.all_module_envs[module_idx];
    const source_context = thread.requireSourceContext();

    try setExprDirectCallSite(
        driver,
        result,
        source_context,
        module_idx,
        call_expr_idx,
        callable_inst_id,
    );
    try setExprDirectCallable(
        driver,
        result,
        source_context,
        module_idx,
        call_expr.func,
        callable_inst_id,
    );
    switch (callee_expr) {
        .e_lookup_local => {
            try setExprDirectCallable(
                driver,
                result,
                source_context,
                module_idx,
                call_expr.func,
                callable_inst_id,
            );
        },
        .e_lookup_external, .e_lookup_required => try setExprDirectCallable(
            driver,
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
    try setExprDirectCallable(
        driver,
        result,
        source_context,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    try setExprDirectCallable(
        driver,
        result,
        callable_inst.defining_source_context,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    try setExprDirectCallableInCallableContext(
        driver,
        result,
        callable_inst_id,
        callee_template.module_idx,
        callee_template.cir_expr,
        callable_inst_id,
    );
    const callable_inst_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };
    if (result.context_mono.monotype_store.getMonotype(callable_inst_fn_mono.ret) != .func) {
        const call_expr_monotype = try cm.resolveTypeVarMonotypeResolved(
            driver,
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(call_expr_idx),
        );
        if (!call_expr_monotype.isNone()) {
        try cm.recordExprMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr_monotype.idx,
            call_expr_monotype.module_idx,
        );
        }
    }
    const arg_exprs = module_env.store.sliceExpr(call_expr.args);
    try prepareCallableArgsForCallableInst(driver, visit_memo, result, thread, module_idx, arg_exprs, callable_inst_id);
    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);
    var variant_builder = CallableVariantBuilder.init();
    defer variant_builder.deinit(driver.allocator);
    try recordCallResultCallableValueFromCallee(
        driver,
        visit_memo,
        result,
        source_context,
        module_idx,
        call_expr_idx,
        callable_inst_id,
        &visiting,
        &variant_builder,
    );
    if (try variant_builder.finishValue(driver, result)) |call_result_value| {
        try setExprCallableValue(
            driver,
            result,
            source_context,
            module_idx,
            call_expr_idx,
            call_result_value,
        );
    }
    try realizeCallableArgSemanticsSlice(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        arg_exprs,
    );
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

    for (result.lambdasolved.getCallableInsts()) |other_callable_inst| {
        if (other_callable_inst.template != callable_inst.template) continue;
        for (result.getCallableParamSpecEntries(other_callable_inst.callable_param_specs)) |spec| {
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

fn appendCallableInstLexicalBindings(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    out: *std.ArrayListUnmanaged(CallableLexicalBinding),
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    switch (template.binding) {
        .pattern => |binding_pattern| try out.append(driver.allocator, .{
            .module_idx = template.module_idx,
            .pattern_idx = binding_pattern,
            .callable_value = .{ .direct = callable_inst_id },
        }),
        .anonymous => {},
    }

    const arg_patterns = driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
    for (arg_patterns) |pattern_idx| {
        var callable_value: ?CallableValue = null;

        for (result.getCaptureFields(result.getCallableDefForInst(callable_inst_id).captures)) |capture_field| {
            if (capture_field.pattern_idx != pattern_idx) continue;
            if (capture_field.callable_value) |resolved| {
                callable_value = resolved;
                break;
            }
        }

        if (callable_value == null) {
            for (result.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
                if (!spec.projections.isEmpty()) continue;
                if (spec.param_index >= arg_patterns.len) continue;
                if (arg_patterns[spec.param_index] == pattern_idx) {
                    callable_value = spec.callable_value;
                    break;
                }
            }
        }

        if (callable_value == null) {
            const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
                .func => |func| func,
                else => continue,
            };
            for (arg_patterns, 0..) |arg_pattern_idx, param_index_usize| {
                if (arg_pattern_idx != pattern_idx) continue;
                const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, param_index_usize);
                if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) break;
                callable_value = try inferCallableParamPatternValueForCallableInst(
                    driver,
                    result,
                    callable_inst_id,
                    @intCast(param_index_usize),
                    cm.resolvedMonotype(param_mono, callable_inst.fn_monotype_module_idx),
                );
                break;
            }
        }

        const resolved_callable_value = callable_value orelse continue;
        try out.append(driver.allocator, .{
            .module_idx = template.module_idx,
            .pattern_idx = pattern_idx,
            .callable_value = resolved_callable_value,
        });
    }
}

fn appendCallableInstBoundaryValueBindings(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    out: *std.ArrayListUnmanaged(ValueLexicalBinding),
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);

    switch (template.binding) {
        .pattern => |binding_pattern| try out.append(driver.allocator, .{
            .module_idx = template.module_idx,
            .pattern_idx = binding_pattern,
            .source = .{ .lexical_binding = .{
                .callable_inst = callable_inst_id,
                .module_idx = template.module_idx,
                .pattern_idx = binding_pattern,
                .callable_value = .{ .direct = callable_inst_id },
            } },
        }),
        .anonymous => {},
    }

    for (driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns), 0..) |arg_pattern_idx, param_index| {
        try out.append(driver.allocator, .{
            .module_idx = template.module_idx,
            .pattern_idx = arg_pattern_idx,
            .source = .{ .specialized_param = .{
                .callable_inst = callable_inst_id,
                .param_index = @intCast(param_index),
            } },
        });
    }
}

fn appendCallableInstValueBindings(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    out: *std.ArrayListUnmanaged(ValueLexicalBinding),
) std.mem.Allocator.Error!void {
    try appendCallableInstBoundaryValueBindings(driver, result, callable_inst_id, out);
    try out.appendSlice(
        driver.allocator,
        result.lambdasolved.getCallableInstCaptureBindings(callable_inst_id),
    );
}

fn threadForCallableInst(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!SemanticThread {
    var thread = SemanticThread.trackedThread(callableInstSourceContext(callable_inst_id));

    var value_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
    defer value_bindings.deinit(driver.allocator);
    try appendCallableInstValueBindings(driver, result, callable_inst_id, &value_bindings);
    thread = try extendThreadWithValueBindings(driver, thread, value_bindings.items);

    var callable_bindings = std.ArrayListUnmanaged(CallableLexicalBinding).empty;
    defer callable_bindings.deinit(driver.allocator);
    try appendCallableInstLexicalBindings(driver, result, callable_inst_id, &callable_bindings);
    return extendThreadWithCallableBindings(driver, thread, callable_bindings.items);
}

fn threadForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
) std.mem.Allocator.Error!SemanticThread {
    if (sourceContextCallableInstId(source_context)) |callable_inst_id| {
        return threadForCallableInst(driver, result, callable_inst_id);
    }
    return SemanticThread.trackedThread(source_context);
}

fn realizeCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    if (callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) return;
    const callable_inst = result.getCallableInst(callable_inst_id).*;
    const template = result.getCallableTemplate(callable_inst.template).*;
    const callable_source_context: SourceContext = .{
        .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)),
    };

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
    var lexical_bindings = std.ArrayListUnmanaged(CallableLexicalBinding).empty;
    defer lexical_bindings.deinit(driver.allocator);
    try appendCallableInstLexicalBindings(driver, result, callable_inst_id, &lexical_bindings);
    const base_callable_env = CallableLexicalEnv{
        .parent = null,
        .bindings = lexical_bindings.items,
    };
    var boundary_value_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
    defer boundary_value_bindings.deinit(driver.allocator);
    try appendCallableInstBoundaryValueBindings(driver, result, callable_inst_id, &boundary_value_bindings);
    const base_value_env = ValueLexicalEnv{
        .parent = null,
        .bindings = boundary_value_bindings.items,
    };
    var thread = SemanticThread.trackedThread(callable_source_context)
        .withValueEnv(if (boundary_value_bindings.items.len == 0) null else &base_value_env)
        .withCallableEnv(&base_callable_env);
    var capture_callable_env: ?CallableLexicalEnv = null;
    var capture_value_env: ?ValueLexicalEnv = null;
    var capture_value_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
    defer capture_value_bindings.deinit(driver.allocator);
    switch (template.binding) {
        .pattern => |binding_pattern| try cm.recordTypeVarMonotypeForThread(
            driver,
            result,
            thread,
            template.module_idx,
            ModuleEnv.varFrom(binding_pattern),
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
        ),
        .anonymous => {},
    }

    if (template.kind == .closure) {
        const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
            .e_closure => |closure| closure,
            else => unreachable,
        };
        const stored_capture_bindings = result.lambdasolved.getCallableInstCaptureBindings(callable_inst_id);
        if (stored_capture_bindings.len == 0 and
            module_env.store.sliceCaptures(closure_expr.captures).len != 0 and
            !closureHasOnlySelfRecursiveCaptures(module_env, template, closure_expr))
        {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: closure callable inst {d} template expr {d} in module {d} reached realization without explicit stored capture bindings",
                    .{
                        @intFromEnum(callable_inst_id),
                        @intFromEnum(template.cir_expr),
                        template.module_idx,
                    },
                );
            }
            unreachable;
        }
        try capture_value_bindings.appendSlice(driver.allocator, stored_capture_bindings);
        var capture_lexical_bindings = std.ArrayListUnmanaged(CallableLexicalBinding).empty;
        defer capture_lexical_bindings.deinit(driver.allocator);
        try appendClosureCaptureCallableBindings(
            driver,
            visit_memo,
            result,
            thread,
            capture_value_bindings.items,
            callable_inst_id,
            &capture_lexical_bindings,
        );
        if (capture_value_bindings.items.len != 0) {
            capture_value_env = .{
                .parent = if (boundary_value_bindings.items.len == 0) null else &base_value_env,
                .bindings = capture_value_bindings.items,
            };
            thread = thread.withValueEnv(&capture_value_env.?);
        }
        if (capture_lexical_bindings.items.len != 0) {
            capture_callable_env = .{
                .parent = &base_callable_env,
                .bindings = capture_lexical_bindings.items,
            };
            thread = thread.withCallableEnv(&capture_callable_env.?);
        }
    }

    try scanCirValueExpr(
        driver,
        visit_memo,
        result,
        thread,
        template.module_idx,
        template.body_expr,
    );

    switch (template.kind) {
        .closure => {
            try finalizeCallableDefForCallableInst(
                driver,
                visit_memo,
                result,
                thread,
                template.module_idx,
                template.runtime_expr,
                capture_value_bindings.items,
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

    if (result.getExprCallableValue(callable_source_context, template.module_idx, template.body_expr)) |body_callable_value| {
        try ensureCallableValueVariantsRealizedFromThread(
            driver,
            visit_memo,
            result,
            thread,
            body_callable_value,
        );
    }

    try setExprDirectCallable(
        driver,
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

fn sourceContextCallableInstId(source_context: SourceContext) ?CallableInstId {
    return switch (source_context) {
        .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
        .root_expr, .provenance_expr, .template_expr => null,
    };
}

fn callableInstHasRealizedRuntimeExpr(result: anytype, callable_inst_id: CallableInstId) bool {
    const callable_def = result.getCallableDefForInst(callable_inst_id);
    return callable_def.realized;
}

fn resolveTemplateDefiningSourceContext(
    result: anytype,
    active_source_context: SourceContext,
    template: CallableTemplate,
) SourceContext {
    switch (template.kind) {
        .top_level_def => return .{ .template_expr = .{
            .module_idx = template.module_idx,
            .expr_idx = template.cir_expr,
        } },
        .lambda, .hosted_lambda => {
            if (template.owner == .root_scope) {
                return .{ .template_expr = .{
                    .module_idx = template.module_idx,
                    .expr_idx = template.cir_expr,
                } };
            }
        },
        .closure => {},
    }

    switch (template.owner) {
        .root_scope => {
            var current = active_source_context;
            while (true) switch (current) {
                .callable_inst => |context_id| {
                    current = result.getCallableInst(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))).defining_source_context;
                },
                .root_expr, .provenance_expr, .template_expr => return current,
            };
        },
        .lexical_template => |lexical_owner_template| {
            if (result.getSourceContextTemplateId(active_source_context)) |active_template| {
                if (active_template == lexical_owner_template) {
                    switch (active_source_context) {
                        .root_expr, .provenance_expr => return active_source_context,
                        .callable_inst, .template_expr => {},
                    }
                }
            }
            var current = switch (active_source_context) {
                .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
                .root_expr, .provenance_expr, .template_expr => {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Lambdasolved invariant violated: template expr={d} requires lexical owner template={d} but active source context was {s}",
                            .{
                                @intFromEnum(template.cir_expr),
                                @intFromEnum(lexical_owner_template),
                                @tagName(active_source_context),
                            },
                        );
                    }
                    unreachable;
                },
            };
            while (true) {
                const current_callable_inst = result.getCallableInst(current);
                if (current_callable_inst.template == lexical_owner_template) {
                    return callableInstSourceContext(current);
                }
                current = sourceContextCallableInstId(current_callable_inst.defining_source_context) orelse break;
            }

            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: template expr={d} could not resolve lexical owner template={d} from active callable inst={d}",
                    .{
                        @intFromEnum(template.cir_expr),
                        @intFromEnum(lexical_owner_template),
                        switch (active_source_context) {
                            .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
                            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
                        },
                    },
                );
            }
            unreachable;
        },
    }
}

fn callableParamProjectionsEqual(
    lhs: []const Lambdamono.CallableParamProjection,
    rhs: []const Lambdamono.CallableParamProjection,
) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_proj, rhs_proj| {
        switch (lhs_proj) {
            .field => |lhs_field| switch (rhs_proj) {
                .field => |rhs_field| if (!lhs_field.eql(rhs_field)) return false,
                else => return false,
            },
            .tuple_elem => |lhs_index| switch (rhs_proj) {
                .tuple_elem => |rhs_index| if (lhs_index != rhs_index) return false,
                else => return false,
            },
            .tag_payload => |lhs_payload| switch (rhs_proj) {
                .tag_payload => |rhs_payload| if (lhs_payload.payload_index != rhs_payload.payload_index or !lhs_payload.tag_name.eql(rhs_payload.tag_name)) return false,
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

fn callableParamSpecsEqual(
    result: anytype,
    lhs: []const CallableParamSpecEntry,
    rhs: []const CallableParamSpecEntry,
) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_entry, rhs_entry| {
        if (lhs_entry.param_index != rhs_entry.param_index) return false;
        if (!callableParamProjectionsEqual(
            result.getCallableParamProjectionEntries(lhs_entry.projections),
            result.getCallableParamProjectionEntries(rhs_entry.projections),
        )) return false;
        if (!std.meta.eql(lhs_entry.callable_value, rhs_entry.callable_value)) return false;
    }
    return true;
}

fn addCallableParamProjectionEntries(
    driver: anytype,
    result: anytype,
    entries: []const Lambdamono.CallableParamProjection,
) std.mem.Allocator.Error!Lambdamono.CallableParamProjectionSpan {
    return result.lambdasolved.addValueProjectionEntries(driver.allocator, entries);
}

fn appendValueProjectionEntries(
    driver: anytype,
    result: anytype,
    existing: ValueProjection.ProjectionSpan,
    appended: []const Lambdamono.CallableParamProjection,
) std.mem.Allocator.Error!ValueProjection.ProjectionSpan {
    if (existing.isEmpty() and appended.len == 0) return .empty();

    var combined = std.ArrayListUnmanaged(Lambdamono.CallableParamProjection).empty;
    defer combined.deinit(driver.allocator);

    try combined.appendSlice(driver.allocator, result.getValueProjectionEntries(existing));
    try combined.appendSlice(driver.allocator, appended);
    return try addCallableParamProjectionEntries(driver, result, combined.items);
}

fn appendCallableParamSpecEntry(
    driver: anytype,
    result: anytype,
    out: *std.ArrayListUnmanaged(CallableParamSpecEntry),
    entry: CallableParamSpecEntry,
) std.mem.Allocator.Error!void {
    for (out.items) |existing| {
        if (existing.param_index != entry.param_index) continue;
        if (!std.meta.eql(existing.callable_value, entry.callable_value)) continue;
        if (!callableParamProjectionsEqual(
            result.getCallableParamProjectionEntries(existing.projections),
            result.getCallableParamProjectionEntries(entry.projections),
        )) continue;
        return;
    }
    try out.append(driver.allocator, entry);
}

fn ensureCallableInstRecord(
    driver: anytype,
    result: anytype,
    active_source_context: SourceContext,
    template_id: CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    callable_param_specs: []const CallableParamSpecEntry,
) std.mem.Allocator.Error!EnsuredCallableInst {
    const template = result.getCallableTemplate(template_id);
    const defining_source_context = resolveTemplateDefiningSourceContext(
        result,
        active_source_context,
        template.*,
    );
    const canonical_fn_monotype = if (fn_monotype_module_idx == template.module_idx)
        fn_monotype
    else
        try cm.remapMonotypeBetweenModules(
            driver,
            result,
            fn_monotype,
            fn_monotype_module_idx,
            template.module_idx,
        );
    const canonical_fn_monotype_module_idx = template.module_idx;
    if (std.debug.runtime_safety) {
        const mono = result.context_mono.monotype_store.getMonotype(canonical_fn_monotype);
        switch (mono) {
            .func => |func| {
                if (func.args.len != 0) {
                    const first_arg = result.context_mono.monotype_store.getIdxSpanItem(func.args, 0);
                    if (result.context_mono.monotype_store.getMonotype(first_arg) == .unit) {
                        std.debug.print(
                            "DEBUG ensureCallableInstRecord: template={d} expr={d} module={d} active_ctx={s} defining_ctx={s} fn_monotype={d}@{d} fn_shape={any} callable_param_specs={d}\n",
                            .{
                                @intFromEnum(template_id),
                                @intFromEnum(template.cir_expr),
                                template.module_idx,
                                @tagName(active_source_context),
                                @tagName(defining_source_context),
                                @intFromEnum(canonical_fn_monotype),
                                canonical_fn_monotype_module_idx,
                                mono,
                                callable_param_specs.len,
                            },
                        );
                    }
                }
            },
            else => {},
        }
    }
    const subst_id = if (driver.all_module_envs[template.module_idx].types.needsInstantiation(template.type_root)) blk: {
        break :blk try cm.ensureTypeSubst(
            driver,
            result,
            template.*,
            canonical_fn_monotype,
            canonical_fn_monotype_module_idx,
        );
    } else result.context_mono.getEmptyTypeSubstId();

    for (result.lambdasolved.callable_insts.items, 0..) |existing_callable_inst, idx| {
        if (existing_callable_inst.template != template_id) continue;
        if (existing_callable_inst.fn_monotype_module_idx != canonical_fn_monotype_module_idx) continue;
        if (!std.meta.eql(existing_callable_inst.defining_source_context, defining_source_context)) continue;
        if (!callableParamSpecsEqual(
            result,
            result.getCallableParamSpecEntries(existing_callable_inst.callable_param_specs),
            callable_param_specs,
        )) continue;
        const mono_equal = try cm.monotypesStructurallyEqual(
            driver,
            result,
            existing_callable_inst.fn_monotype,
            canonical_fn_monotype,
        );
        if (mono_equal) {
            if (existing_callable_inst.subst != subst_id) continue;
            const existing_id: CallableInstId = @enumFromInt(idx);
            try result.lambdasolved.ensureDirectCallableVariantGroup(driver.allocator, existing_id);
            return .{ .id = existing_id, .created = false };
        }
    }

    const runtime_kind: Lambdamono.CallableRuntimeKind = switch (template.kind) {
        .lambda => .lambda,
        .closure => .closure,
        .hosted_lambda => .hosted_lambda,
        .top_level_def => std.debug.panic(
            "Lambdasolved invariant violated: callable template {d} runtime expr {d} in module {d} had top_level_def kind at instantiation time",
            .{ @intFromEnum(template_id), @intFromEnum(template.runtime_expr), template.module_idx },
        ),
    };
    const callable_inst_id: CallableInstId = @enumFromInt(result.lambdasolved.callable_insts.items.len);
    const callable_def_id: Lambdamono.CallableDefId = @enumFromInt(result.lambdasolved.callable_defs.items.len);
    try result.lambdasolved.callable_defs.append(driver.allocator, .{
        .module_idx = template.module_idx,
        .runtime_kind = runtime_kind,
        .arg_patterns = try result.lambdasolved.appendPatternEntries(
            driver.allocator,
            driver.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns),
        ),
        .runtime_expr = .{
            .source_context = callableInstSourceContext(callable_inst_id),
            .module_idx = template.module_idx,
            .expr_idx = template.runtime_expr,
        },
        .body_expr = .{
            .source_context = callableInstSourceContext(callable_inst_id),
            .module_idx = template.module_idx,
            .expr_idx = template.body_expr,
        },
        .fn_monotype = cm.resolvedMonotype(canonical_fn_monotype, canonical_fn_monotype_module_idx),
        .captures = .empty(),
        .source_region = template.source_region,
        .realized = false,
    });
    try result.lambdasolved.callable_insts.append(driver.allocator, .{
        .template = template_id,
        .subst = subst_id,
        .fn_monotype = canonical_fn_monotype,
        .fn_monotype_module_idx = canonical_fn_monotype_module_idx,
        .defining_source_context = defining_source_context,
        .callable_def = callable_def_id,
        .runtime_value = .direct_lambda,
        .callable_param_specs = try result.lambdasolved.addCallableParamSpecEntries(driver.allocator, callable_param_specs),
    });
    try result.lambdasolved.callable_inst_capture_bindings.append(driver.allocator, .empty());
    try result.lambdasolved.ensureDirectCallableVariantGroup(driver.allocator, callable_inst_id);
    return .{ .id = callable_inst_id, .created = true };
}

fn recordExprCallableValue(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_value: CallableValue,
) std.mem.Allocator.Error!void {
    try result.lambdasolved.recordExprCallableValue(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        callable_value,
    );
}

fn recordExprCallSite(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    call_site: CallSite,
) std.mem.Allocator.Error!void {
    try result.lambdasolved.recordExprCallSite(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        call_site,
    );
}

fn recordExprOriginExpr(
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
            .projections = try appendValueProjectionEntries(
                driver,
                result,
                origin.projections,
                result.getValueProjectionEntries(expr_ref.projections),
            ),
        };
    };
    try result.lambdasolved.recordExprOriginExpr(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        canonical_ref,
    );
}

fn recordExprSourceExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    source: ExprRef,
) std.mem.Allocator.Error!void {
    try recordExprOriginExpr(driver, result, source_context, module_idx, expr_idx, source);
}

fn recordExprLookupResolution(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    lookup_resolution: Lambdamono.LookupResolution,
) std.mem.Allocator.Error!void {
    try result.lambdasolved.recordExprLookupResolution(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        lookup_resolution,
    );
}

fn recordDispatchExprTarget(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dispatch_target: DispatchSolved.DispatchExprTarget,
) std.mem.Allocator.Error!void {
    try result.dispatch_solved.recordDispatchExprTarget(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        dispatch_target,
    );
}

fn recordDispatchExprIntrinsic(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dispatch_intrinsic: DispatchIntrinsic,
) std.mem.Allocator.Error!void {
    try result.lambdasolved.recordExprDispatchIntrinsic(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        dispatch_intrinsic,
    );
}

fn captureFieldsEqual(lhs: []const CaptureField, rhs: []const CaptureField) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |lhs_field, rhs_field| {
        if (!std.meta.eql(lhs_field, rhs_field)) return false;
    }
    return true;
}

fn updateCallableDefRuntimeSemantics(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    runtime_expr_ref: ExprRef,
    body_expr_ref: ExprRef,
    capture_fields: []const CaptureField,
    runtime_value: RuntimeValue,
) std.mem.Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const callable_def = result.lambdasolved.getCallableDefMut(callable_inst.callable_def);

    if (!std.meta.eql(callable_def.runtime_expr, runtime_expr_ref)) {
        if (std.debug.runtime_safety and callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) {
            std.debug.panic(
                "Lambdasolved invariant violated: callable inst {d} attempted to rewrite finalized runtime expr",
                .{@intFromEnum(callable_inst_id)},
            );
        }
        callable_def.runtime_expr = runtime_expr_ref;
    }
    if (!std.meta.eql(callable_def.body_expr, body_expr_ref)) {
        if (std.debug.runtime_safety and callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) {
            std.debug.panic(
                "Lambdasolved invariant violated: callable inst {d} attempted to rewrite finalized body expr",
                .{@intFromEnum(callable_inst_id)},
            );
        }
        callable_def.body_expr = body_expr_ref;
    }

    const existing_capture_fields = result.getCaptureFields(callable_def.captures);
    callable_def.captures = if (capture_fields.len == 0)
        .empty()
    else if (captureFieldsEqual(existing_capture_fields, capture_fields))
        callable_def.captures
    else if (!callable_def.captures.isEmpty()) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdasolved invariant violated: callable inst {d} attempted to rewrite finalized capture fields",
                .{@intFromEnum(callable_inst_id)},
            );
        }
        unreachable;
    } else try result.lambdasolved.appendCaptureFields(driver.allocator, capture_fields);

    if (!std.meta.eql(result.lambdasolved.callable_insts.items[@intFromEnum(callable_inst_id)].runtime_value, runtime_value)) {
        result.lambdasolved.callable_insts.items[@intFromEnum(callable_inst_id)].runtime_value = runtime_value;
    }
    callable_def.realized = true;
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

fn debugPatternSummary(
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    buf: []u8,
) []const u8 {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => |assign_pat| std.fmt.bufPrint(buf, "assign({s})", .{
            module_env.getIdent(assign_pat.ident),
        }) catch "assign(?)",
        .as => |as_pat| std.fmt.bufPrint(buf, "as({s})", .{
            module_env.getIdent(as_pat.ident),
        }) catch "as(?)",
        else => @tagName(module_env.store.getPattern(pattern_idx)),
    };
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
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    template_id: CallableTemplateId,
) std.mem.Allocator.Error!?CallableInstId {
    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);
    const module_env = driver.all_module_envs[module_idx];
    const method_name = switch (expr) {
        .e_binop => |binop_expr| dispatchMethodIdentForExprBinop(module_env, binop_expr.op).?,
        .e_dot_access => |dot_expr| dot_expr.field_name,
        .e_type_var_dispatch => |dispatch_expr| dispatch_expr.method_name,
        else => unreachable,
    };
    const site = DispatchSolved.exactDispatchSiteForExpr(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        method_name,
    ) orelse return null;
    const defining_source_context = requireTemplateDemandSourceContextForExpr(
        driver,
        visit_memo,
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
    const fn_monotype = site.fn_monotype.idx;
    const fn_monotype_module_idx = site.fn_monotype.module_idx;

    var callable_param_specs = std.ArrayListUnmanaged(Lambdamono.CallableParamSpecEntry).empty;
    defer callable_param_specs.deinit(driver.allocator);
    const callable_param_specs_complete = try collectDirectCallCallableParamSpecs(
        driver,
        visit_memo,
        result,
        thread,
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
        visit_memo,
        result,
        thread,
        defining_source_context,
        template_id,
        fn_monotype,
        fn_monotype_module_idx,
        callable_param_specs.items,
    );
    return callable_inst_id;
}

fn appendDispatchActualArgsFromExpr(
    driver: anytype,
    module_idx: u32,
    expr: CIR.Expr,
    actual_args: *std.ArrayList(CIR.Expr.Idx),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    switch (expr) {
        .e_binop => |binop_expr| {
            try actual_args.append(driver.allocator, binop_expr.lhs);
            try actual_args.append(driver.allocator, binop_expr.rhs);
        },
        .e_dot_access => |dot_expr| {
            try actual_args.append(driver.allocator, dot_expr.receiver);
            if (dot_expr.args) |args| try actual_args.appendSlice(driver.allocator, module_env.store.sliceExpr(args));
        },
        .e_type_var_dispatch => |dispatch_expr| {
            try actual_args.appendSlice(driver.allocator, module_env.store.sliceExpr(dispatch_expr.args));
        },
        else => unreachable,
    }
}

fn resolveDemandedDispatchFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    fn_var: types.Var,
) std.mem.Allocator.Error!?cm.ResolvedMonotype {
    const module_env = driver.all_module_envs[module_idx];
    const method_name = switch (expr) {
        .e_binop => |binop_expr| dispatchMethodIdentForExprBinop(module_env, binop_expr.op) orelse return null,
        .e_unary_minus => module_env.idents.negate,
        .e_dot_access => |dot_expr| if (dot_expr.args != null) dot_expr.field_name else return null,
        .e_type_var_dispatch => |dispatch_expr| dispatch_expr.method_name,
        else => return null,
    };
    if (DispatchSolved.exactDispatchSiteForExpr(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        method_name,
    )) |site| {
        if (std.debug.runtime_safety and site.fn_var != fn_var) {
            std.debug.panic(
                "Lambdasolved invariant violated: dispatch expr {d} in module {d} requested fn var {d} but cached exact dispatch site owned fn var {d}",
                .{
                    @intFromEnum(expr_idx),
                    module_idx,
                    @intFromEnum(fn_var),
                    @intFromEnum(site.fn_var),
                },
            );
        }
        return site.fn_monotype;
    }
    if (try cm.resolveExprExactMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
    )) |expr_exact| {
        if (cm.resolvedIfFunctionMonotype(result, expr_exact)) |fn_monotype| {
            return fn_monotype;
        }
    }
    const fn_var_exact = try cm.resolveTypeVarExactMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        fn_var,
    );
    if (!fn_var_exact.isNone()) {
        if (cm.resolvedIfFunctionMonotype(result, fn_var_exact)) |fn_monotype| {
            return fn_monotype;
        }
    }
    return null;
}

fn ensureRecordedExactDispatchSiteFromDemandedFnMonotype(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    method_name: Ident.Idx,
) std.mem.Allocator.Error!?DispatchSolved.ExactDispatchSite {
    if (DispatchSolved.exactDispatchSiteForExpr(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        method_name,
    )) |site| {
        return site;
    }

    const dispatch_fn_var = blk: {
        if (driver.all_module_envs[module_idx].types.findStaticDispatchSiteRequirement(
            ModuleEnv.varFrom(expr_idx),
            method_name,
        )) |requirement| {
            break :blk requirement.fn_var;
        }
        const receiver_type_var = DispatchSolved.dispatchConstraintReceiverTypeVar(
            driver,
            module_idx,
            expr_idx,
            method_name,
        ) orelse break :blk null;
        for (DispatchSolved.dispatchConstraintsForTypeVar(driver, module_idx, receiver_type_var)) |constraint| {
            if (constraint.fn_name.eql(method_name)) break :blk constraint.fn_var;
        }
        break :blk null;
    };

    if (dispatch_fn_var) |fn_var| {
        if (try resolveDispatchFnMonotypeFromExactActuals(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            fn_var,
        )) |fn_monotype| {
            const site: DispatchSolved.ExactDispatchSite = .{
                .method_name = method_name,
                .fn_var = fn_var,
                .fn_monotype = fn_monotype,
            };
            try result.dispatch_solved.recordExactDispatchSite(
                driver.allocator,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                site,
            );
            return site;
        }
    }

    switch (expr) {
        .e_dot_access => |dot_expr| {
            if (dot_expr.args != null) {
                if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    method_name,
                )) |_| {
                    return DispatchSolved.exactDispatchSiteForExpr(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        method_name,
                    );
                }
                if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver)) |receiver_mono| {
                    if (try resolveAssociatedMethodDispatchTargetForMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_mono.idx,
                        method_name,
                    )) |_| {
                        return DispatchSolved.exactDispatchSiteForExpr(
                            driver,
                            result,
                            thread,
                            module_idx,
                            expr_idx,
                            method_name,
                        );
                    }
                }
            }
        },
        .e_binop => |binop_expr| {
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            )) |_| {
                return DispatchSolved.exactDispatchSiteForExpr(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    method_name,
                );
            }
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, binop_expr.lhs)) |lhs_mono| {
                if (try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_mono.idx,
                    method_name,
                )) |_| {
                    return DispatchSolved.exactDispatchSiteForExpr(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        method_name,
                    );
                }
            }
        },
        .e_unary_minus => |unary_expr| {
            if (try resolveAssociatedMethodDispatchTargetForTypeVar(
                driver,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(unary_expr.expr),
                method_name,
            )) |_| {
                return DispatchSolved.exactDispatchSiteForExpr(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    method_name,
                );
            }
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, unary_expr.expr)) |operand_mono| {
                if (try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    operand_mono.idx,
                    method_name,
                )) |_| {
                    return DispatchSolved.exactDispatchSiteForExpr(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        method_name,
                    );
                }
            }
        },
        .e_type_var_dispatch => |dispatch_expr| {
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
            )) |_| {
                return DispatchSolved.exactDispatchSiteForExpr(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    method_name,
                );
            }
            const alias_mono = try cm.resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, receiver_type_var);
            if (!alias_mono.isNone()) {
                if (try resolveAssociatedMethodDispatchTargetForMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    alias_mono.idx,
                    method_name,
                )) |_| {
                    return DispatchSolved.exactDispatchSiteForExpr(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        method_name,
                    );
                }
            }
        },
        else => {},
    }

    const site = (try DispatchSolved.extractExactDispatchSiteForExpr(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        method_name,
    )) orelse return null;
    try result.dispatch_solved.recordExactDispatchSite(
        driver.allocator,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        site,
    );
    return site;
}

fn resolveDispatchFnMonotypeFromExactActuals(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    fn_var: types.Var,
) std.mem.Allocator.Error!?cm.ResolvedMonotype {
    const module_env = driver.all_module_envs[module_idx];
    const resolved_func = cm.resolveFuncTypeInStore(&module_env.types, fn_var) orelse return null;

    var extra_bindings = std.AutoHashMap(cm.BoundTypeVarKey, cm.ResolvedMonotype).init(driver.allocator);
    defer extra_bindings.deinit();

    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);

    const param_vars = module_env.types.sliceVars(resolved_func.func.args);
    if (actual_args.items.len != param_vars.len) return null;

    for (actual_args.items, param_vars) |arg_expr_idx, arg_var| {
        const arg_mono = (try cm.resolveExprExactMonotypeResolved(
            driver,
            result,
            thread,
            module_idx,
            arg_expr_idx,
        )) orelse continue;
        const resolved_arg_var = module_env.types.resolveVar(arg_var).var_;
        try extra_bindings.put(
            .{ .module_idx = module_idx, .type_var = resolved_arg_var },
            arg_mono,
        );
    }

    if (try cm.resolveExprExactMonotypeResolved(driver, result, thread, module_idx, expr_idx)) |ret_mono| {
        const resolved_ret_var = module_env.types.resolveVar(resolved_func.func.ret).var_;
        try extra_bindings.put(
            .{ .module_idx = module_idx, .type_var = resolved_ret_var },
            ret_mono,
        );
    }

    if (std.debug.runtime_safety) {
        for (param_vars, 0..) |arg_var, arg_index| {
            const resolved_arg_var = module_env.types.resolveVar(arg_var).var_;
            const arg_mono = try resolveExactTypeVarMonotypeFromBindings(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                &module_env.types,
                resolved_arg_var,
                &extra_bindings,
            );
            std.debug.print(
                "DEBUG resolveDispatchFnMonotypeFromExactActuals param: module={d} expr={d} fn_var={d} arg_index={d} arg_var={d} resolved_arg_var={d} mono={any}\n",
                .{
                    module_idx,
                    @intFromEnum(expr_idx),
                    @intFromEnum(fn_var),
                    arg_index,
                    @intFromEnum(arg_var),
                    @intFromEnum(resolved_arg_var),
                    if (arg_mono.isNone()) null else result.context_mono.monotype_store.getMonotype(arg_mono),
                },
            );
        }
        const resolved_ret_var = module_env.types.resolveVar(resolved_func.func.ret).var_;
            const ret_mono = try resolveExactTypeVarMonotypeFromBindings(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                &module_env.types,
            resolved_ret_var,
            &extra_bindings,
        );
        std.debug.print(
            "DEBUG resolveDispatchFnMonotypeFromExactActuals ret: module={d} expr={d} fn_var={d} ret_var={d} resolved_ret_var={d} mono={any}\n",
            .{
                module_idx,
                @intFromEnum(expr_idx),
                @intFromEnum(fn_var),
                @intFromEnum(resolved_func.func.ret),
                @intFromEnum(resolved_ret_var),
                if (ret_mono.isNone()) null else result.context_mono.monotype_store.getMonotype(ret_mono),
            },
        );
    }

    const fn_monotype = try resolveExactTypeVarMonotypeFromBindings(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        &module_env.types,
        fn_var,
        &extra_bindings,
    );
    if (fn_monotype.isNone()) return null;
    return if (cm.resolvedIfFunctionMonotype(result, cm.resolvedMonotype(fn_monotype, module_idx))) |resolved|
        resolved
    else
        null;
}

fn resolveExactTypeVarMonotypeFromBindings(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    store_types: *const types.Store,
    type_var: types.Var,
    extra_bindings: *const std.AutoHashMap(cm.BoundTypeVarKey, cm.ResolvedMonotype),
) std.mem.Allocator.Error!Monotype.Idx {
    const direct = try cm.monotypeFromTypeVarInSourceContextWithExtraBindings(
        driver,
        result,
        source_context,
        module_idx,
        store_types,
        type_var,
        extra_bindings,
    );
    if (!direct.isNone()) return direct;

    const resolved_func = cm.resolveFuncTypeInStore(store_types, type_var) orelse return .none;
    var arg_monos = std.ArrayListUnmanaged(Monotype.Idx).empty;
    defer arg_monos.deinit(driver.allocator);

    for (store_types.sliceVars(resolved_func.func.args)) |arg_var| {
        const arg_mono = try resolveExactTypeVarMonotypeFromBindings(
            driver,
            result,
            source_context,
            module_idx,
            store_types,
            arg_var,
            extra_bindings,
        );
        if (arg_mono.isNone()) return .none;
        try arg_monos.append(driver.allocator, arg_mono);
    }

    const ret_mono = try resolveExactTypeVarMonotypeFromBindings(
        driver,
        result,
        source_context,
        module_idx,
        store_types,
        resolved_func.func.ret,
        extra_bindings,
    );
    if (ret_mono.isNone()) return .none;

    const args = try result.context_mono.monotype_store.addIdxSpan(driver.allocator, arg_monos.items);
    return try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .func = .{
        .args = args,
        .ret = ret_mono,
        .effectful = resolved_func.effectful,
    } });
}

fn resolveTemplateFnMonotypeFromExactActuals(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    caller_module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
    template: *const CallableTemplate,
) std.mem.Allocator.Error!?cm.ResolvedMonotype {
    const template_env = driver.all_module_envs[template.module_idx];
    const resolved_func = cm.resolveFuncTypeInStore(&template_env.types, template.type_root) orelse return null;

    var extra_bindings = std.AutoHashMap(cm.BoundTypeVarKey, cm.ResolvedMonotype).init(driver.allocator);
    defer extra_bindings.deinit();

    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try appendDispatchActualArgsFromExpr(driver, caller_module_idx, expr, &actual_args);

    const param_vars = template_env.types.sliceVars(resolved_func.func.args);
    if (actual_args.items.len != param_vars.len) return null;

    for (actual_args.items, param_vars) |arg_expr_idx, arg_var| {
        const arg_mono = (try cm.resolveExprExactMonotypeResolved(
            driver,
            result,
            thread,
            caller_module_idx,
            arg_expr_idx,
        )) orelse continue;
        try extra_bindings.put(
            .{
                .module_idx = template.module_idx,
                .type_var = template_env.types.resolveVar(arg_var).var_,
            },
            arg_mono,
        );
    }

    if (try cm.resolveExprExactMonotypeResolved(driver, result, thread, caller_module_idx, expr_idx)) |ret_mono| {
        try extra_bindings.put(
            .{
                .module_idx = template.module_idx,
                .type_var = template_env.types.resolveVar(resolved_func.func.ret).var_,
            },
            ret_mono,
        );
    }

    if (std.debug.runtime_safety) {
        for (param_vars, 0..) |arg_var, arg_index| {
            const resolved_arg_var = template_env.types.resolveVar(arg_var).var_;
            const arg_mono = try resolveExactTypeVarMonotypeFromBindings(
                driver,
                result,
                thread.requireSourceContext(),
                template.module_idx,
                &template_env.types,
                resolved_arg_var,
                &extra_bindings,
            );
            std.debug.print(
                "DEBUG resolveTemplateFnMonotypeFromExactActuals param: caller_module={d} template_module={d} expr={d} type_root={d} arg_index={d} arg_var={d} resolved_arg_var={d} mono={any}\n",
                .{
                    caller_module_idx,
                    template.module_idx,
                    @intFromEnum(expr_idx),
                    @intFromEnum(template.type_root),
                    arg_index,
                    @intFromEnum(arg_var),
                    @intFromEnum(resolved_arg_var),
                    if (arg_mono.isNone()) null else result.context_mono.monotype_store.getMonotype(arg_mono),
                },
            );
            if (arg_mono.isNone()) {
                const raw_func = cm.resolveFuncTypeInStore(&template_env.types, arg_var);
                std.debug.print(
                    "DEBUG resolveTemplateFnMonotypeFromExactActuals raw_arg_content: arg_index={d} content_tag={s} func={any}",
                    .{
                        arg_index,
                        @tagName(template_env.types.resolveVar(arg_var).desc.content),
                        raw_func,
                    },
                );
                if (raw_func) |func_info| {
                    std.debug.print(
                        " callback_args={any} callback_ret={d}\n",
                        .{
                            template_env.types.sliceVars(func_info.func.args),
                            @intFromEnum(func_info.func.ret),
                        },
                    );
                } else {
                    std.debug.print("\n", .{});
                }
            }
        }
        const resolved_ret_var = template_env.types.resolveVar(resolved_func.func.ret).var_;
        const ret_mono = try resolveExactTypeVarMonotypeFromBindings(
            driver,
            result,
            thread.requireSourceContext(),
            template.module_idx,
            &template_env.types,
            resolved_ret_var,
            &extra_bindings,
        );
        std.debug.print(
            "DEBUG resolveTemplateFnMonotypeFromExactActuals ret: caller_module={d} template_module={d} expr={d} type_root={d} ret_var={d} resolved_ret_var={d} mono={any}\n",
            .{
                caller_module_idx,
                template.module_idx,
                @intFromEnum(expr_idx),
                @intFromEnum(template.type_root),
                @intFromEnum(resolved_func.func.ret),
                @intFromEnum(resolved_ret_var),
                if (ret_mono.isNone()) null else result.context_mono.monotype_store.getMonotype(ret_mono),
            },
        );
    }

    const fn_monotype = try resolveExactTypeVarMonotypeFromBindings(
        driver,
        result,
        thread.requireSourceContext(),
        template.module_idx,
        &template_env.types,
        template.type_root,
        &extra_bindings,
    );
    if (fn_monotype.isNone()) return null;
    return if (cm.resolvedIfFunctionMonotype(result, cm.resolvedMonotype(fn_monotype, template.module_idx))) |resolved|
        resolved
    else
        null;
}

fn ensureBuiltinBoxUnboxCallableInst(
    driver: anytype,
    visit_memo: *VisitMemo,
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
        visit_memo,
        result,
        null,
        templateSourceContext(result.getCallableTemplate(template_id).*),
        template_id,
        fn_monotype,
        source_module_idx,
    );
}

fn resolveStrInspectHelperCallableInstsForTypeVar(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    type_var: types.Var,
) std.mem.Allocator.Error!void {
    var visiting: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
    defer visiting.deinit(driver.allocator);
    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, type_var, &visiting);
}

fn resolveStrInspectHelperCallableInstsForTypeVarWithSeen(
    driver: anytype,
    visit_memo: *VisitMemo,
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
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, type_args[0], visiting);
                        }
                        return;
                    }
                    if (ident.eql(common.box)) {
                        const type_args = module_env.types.sliceNominalArgs(nominal);
                        const outer_mono = try cm.resolveTypeVarMonotype(driver, result, thread, module_idx, resolved.var_);
                        const outer_box = result.context_mono.monotype_store.getMonotype(outer_mono).box;
                        try ensureBuiltinBoxUnboxCallableInst(driver, visit_memo, result, module_idx, outer_mono, outer_box.inner);
                        if (type_args.len == 1) {
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, type_args[0], visiting);
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
                                        visit_memo,
                                        result,
                                        null,
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
                                        else => try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, method_func.ret),
                                    }
                                }
                            }
                        }
                    }
                    return;
                }
            },
            .record => |record| {
                try resolveStrInspectHelperCallableInstsForRecordType(driver, visit_memo, result, thread, module_idx, &module_env.types, record, visiting);
                return;
            },
            .record_unbound => |fields_range| {
                for (module_env.types.getRecordFieldsSlice(fields_range).items(.var_)) |field_var| {
                    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, field_var, visiting);
                }
                return;
            },
            .tuple => |tuple| {
                for (module_env.types.sliceVars(tuple.elems)) |elem_var| {
                    try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, elem_var, visiting);
                }
                return;
            },
            .tag_union => |tag_union| {
                try resolveStrInspectHelperCallableInstsForTagUnionType(driver, visit_memo, result, thread, module_idx, &module_env.types, tag_union, visiting);
                return;
            },
            .empty_record, .empty_tag_union => return,
            else => {},
        }
    }

    try resolveStrInspectHelperCallableInstsForMonotype(
        driver,
        visit_memo,
        result,
        module_idx,
        try cm.resolveTypeVarMonotype(driver, result, thread, module_idx, resolved.var_),
    );
}

fn resolveStrInspectHelperCallableInstsForRecordType(
    driver: anytype,
    visit_memo: *VisitMemo,
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
            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, field_var, visiting);
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
                            try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, field_var, visiting);
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
    visit_memo: *VisitMemo,
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
                try resolveStrInspectHelperCallableInstsForTypeVarWithSeen(driver, visit_memo, result, thread, module_idx, payload_var, visiting);
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
    visit_memo: *VisitMemo,
    result: anytype,
    module_idx: u32,
    monotype: Monotype.Idx,
) std.mem.Allocator.Error!void {
    if (monotype.isNone()) return;

    switch (result.context_mono.monotype_store.getMonotype(monotype)) {
        .unit, .prim => {},
        .list => |list_mono| try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, list_mono.elem),
        .box => |box_mono| {
            try ensureBuiltinBoxUnboxCallableInst(driver, visit_memo, result, module_idx, monotype, box_mono.inner);
            try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, box_mono.inner);
        },
        .tuple => |tuple_mono| {
            var elem_i: usize = 0;
            while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, elem_mono);
            }
        },
        .func => {},
        .record => |record_mono| {
            var field_i: usize = 0;
            while (field_i < record_mono.fields.len) : (field_i += 1) {
                const field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, field_i);
                try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, field.type_idx);
            }
        },
        .tag_union => |tag_union_mono| {
            var tag_i: usize = 0;
            while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                const tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                var payload_i: usize = 0;
                while (payload_i < tag.payloads.len) : (payload_i += 1) {
                    const payload_mono = result.context_mono.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                    try resolveStrInspectHelperCallableInstsForMonotype(driver, visit_memo, result, module_idx, payload_mono);
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
        for (result.getCallableValueVariants(callable_value)) |callable_inst_id| {
            try self.includeCallableInst(driver, callable_inst_id);
        }
    }

    fn includeCallSite(
        self: *CallableVariantBuilder,
        driver: anytype,
        result: anytype,
        call_site: CallSite,
    ) std.mem.Allocator.Error!void {
        for (result.getCallSiteVariants(call_site)) |callable_inst_id| {
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
        return .{ .packed_fn = try result.lambdasolved.makePackedFnForCallableInsts(
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
        return .{ .indirect_call = try result.lambdasolved.makeIndirectCallForCallableInsts(
            driver.allocator,
            driver.all_module_envs,
            driver.current_module_idx,
            &result.context_mono,
            self.variants.items,
        ) };
    }
};

fn resolveBoundBindingCallableValueForThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?CallableValue {
    if (thread.lookupCallableBinding(module_idx, pattern_idx)) |callable_value| {
        return callable_value;
    } else if (thread.lookupValueBinding(module_idx, pattern_idx)) |source| {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        switch (source) {
            .lexical_binding => |lexical| if (lexical.callable_value) |callable_value| return callable_value,
            .bound_expr => |bound_expr| {
                if (resolveExprRefCallableValue(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    bound_expr.expr_ref,
                    &visiting,
                ) catch unreachable) |callable_value| {
                    return callable_value;
                }
            },
            .specialized_param => {},
        }
        const context_callable_inst = thread.callableInst() orelse return switch (source) {
            .bound_expr => null,
            .specialized_param => |specialized_param| resolveSpecializedParamCallableValue(result, specialized_param, &.{}),
            .lexical_binding => null,
        };
        switch (source) {
            .specialized_param => |specialized_param| if (specialized_param.callable_inst == context_callable_inst)
                return resolveSpecializedParamCallableValue(result, specialized_param, &.{}),
            .lexical_binding => |lexical| if (lexical.callable_inst != null and lexical.callable_inst.? == context_callable_inst and lexical.module_idx == module_idx and lexical.pattern_idx == pattern_idx)
                return null,
            .bound_expr => {},
        }
        return resolveCaptureCallableValue(
            driver,
            visit_memo,
            result,
            thread,
            context_callable_inst,
            source,
            &visiting,
        ) catch unreachable;
    }
    return null;
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

fn getCallableParamSpecCallableValueForThread(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    const resolved_context_callable_inst = thread.callableInst() orelse return null;

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

    for (result.getCallableParamSpecEntries(result.getCallableInst(resolved_context_callable_inst).callable_param_specs)) |spec| {
        if (spec.param_index != param_index) continue;
        if (!callableParamProjectionSeqEqual(
            driver,
            result.getCallableParamProjectionEntries(spec.projections),
            projections.items,
        )) continue;
        return spec.callable_value;
    }

    return null;
}

fn getValueExprCallableValueForThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_lookup_local => |lookup| {
            if (resolveBoundBindingCallableValueForThread(driver, visit_memo, result, thread, module_idx, lookup.pattern_idx)) |callable_value| {
                return callable_value;
            }
            if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| switch (source) {
                .bound_expr => |bound_expr| {
                    if (bound_expr.expr_ref.projections.isEmpty()) {
                        if (result.getExprTemplateId(
                            bound_expr.expr_ref.source_context,
                            bound_expr.expr_ref.module_idx,
                            bound_expr.expr_ref.expr_idx,
                        )) |template_id| {
                            const fn_monotype = (cm.resolveExprExactMonotypeResolved(
                                driver,
                                result,
                                thread,
                                module_idx,
                                expr_idx,
                            ) catch unreachable) orelse return null;
                            if (cm.resolvedIfFunctionMonotype(result, fn_monotype)) |exact_fn_monotype| {
                                _ = introduceExprCallableValueWithKnownFnMonotype(
                                    driver,
                                    visit_memo,
                                    result,
                                    thread,
                                    thread.requireSourceContext(),
                                    module_idx,
                                    expr_idx,
                                    template_id,
                                    exact_fn_monotype,
                                ) catch unreachable;
                                if (readExprCallableValue(result, thread.requireSourceContext(), module_idx, expr_idx)) |callable_value| {
                                    return callable_value;
                                }
                            }
                        }
                    }
                },
                .specialized_param, .lexical_binding => {},
            };
        },
        else => {},
    }

    if (getCallableParamSpecCallableValueForThread(driver, result, thread, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    return readExprCallableValue(result, thread.requireSourceContext(), module_idx, expr_idx);
}

fn getValueExprCallableInstForThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableInstId {
    const callable_value = getValueExprCallableValueForThread(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        expr_idx,
    ) orelse return null;
    return Lambdamono.exactCallableInstFromValue(callable_value);
}

pub fn getValueExprCallableValueForSourceContext(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?CallableValue {
    const thread = threadForSourceContext(driver, result, source_context) catch unreachable;
    if (getValueExprCallableValueForThread(driver, visit_memo, result, thread, module_idx, expr_idx)) |callable_value| {
        return callable_value;
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (source.projections.isEmpty()) {
            const source_thread = threadForSourceContext(driver, result, source.source_context) catch unreachable;
            if (getValueExprCallableValueForThread(
                driver,
                visit_memo,
                result,
                source_thread,
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
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    expr_ref: ExprRef,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    return resolveProjectedExprCallableValue(
        driver,
        visit_memo,
        result,
        thread,
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        result.getValueProjectionEntries(expr_ref.projections),
        visiting,
    );
}

pub fn resolveProjectedExprCallableValue(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const ValueProjection.Projection,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!?CallableValue {
    if (projections.len == 0) {
        try realizeStructuredExprCallableSemantics(
            driver,
            visit_memo,
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
            visiting,
        );
        if (getValueExprCallableValueForSourceContext(driver, visit_memo, result, source_context, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }
        return null;
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        const combined_projections = try appendValueProjectionEntries(
            driver,
            result,
            origin.projections,
            projections,
        );
        return resolveProjectedExprCallableValue(
            driver,
            visit_memo,
            result,
            thread,
            origin.source_context,
            origin.module_idx,
            origin.expr_idx,
            result.getValueProjectionEntries(combined_projections),
            visiting,
        );
    }

    if (readExprCallSite(result, source_context, module_idx, expr_idx)) |call_site| {
        var variant_builder = CallableVariantBuilder.init();
        defer variant_builder.deinit(driver.allocator);
        for (result.getCallSiteVariants(call_site)) |callee_callable_inst_id| {
            try ensureCallableInstRealized(driver, visit_memo, result, callee_callable_inst_id);
            const callable_def = result.getCallableDefForInst(callee_callable_inst_id).*;
            const combined_projections = try appendValueProjectionEntries(
                driver,
                result,
                callable_def.body_expr.projections,
                projections,
            );
            if (try resolveProjectedExprCallableValue(
                driver,
                visit_memo,
                result,
                null,
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
                result.getValueProjectionEntries(combined_projections),
                visiting,
            )) |callable_value| {
                try variant_builder.includeCallableValue(driver, result, callable_value);
            }
        }
        return try variant_builder.finishValue(driver, result);
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
                        visit_memo,
                        result,
                        thread,
                        source_context,
                        module_idx,
                        field.value,
                        rest,
                        visiting,
                    );
                }
                if (record_expr.ext) |ext_expr_idx| {
                    return resolveProjectedExprCallableValue(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        source_context,
                        module_idx,
                        ext_expr_idx,
                        projections,
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
                    visit_memo,
                    result,
                    thread,
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
                    visit_memo,
                    result,
                    thread,
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
                    visit_memo,
                    result,
                    thread,
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
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    target_source_context: SourceContext,
    target_module_idx: u32,
    target_expr_idx: CIR.Expr.Idx,
    maybe_pattern_idx: ?CIR.Pattern.Idx,
    source: ExprRef,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    const resolved_callable_value = try resolveExprRefCallableValue(driver, visit_memo, result, thread, source, visiting);
    const callable_value = resolved_callable_value orelse return;

    try setExprCallableValue(
        driver,
        result,
        target_source_context,
        target_module_idx,
        target_expr_idx,
        callable_value,
    );
    _ = maybe_pattern_idx;
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
    switch (expr) {
        .e_lookup_local, .e_lookup_external, .e_lookup_required => {
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
                    return;
                }
            }
        },
        else => {},
    }
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
        result.getValueProjectionEntries(expr_ref.projections),
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
    projections: []const ValueProjection.Projection,
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
                    if (field_name.textEqualsIdent(driver.all_module_envs, module_idx, field.name)) {
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
                if (record_expr.ext) |ext_expr_idx| {
                    return propagateDemandedValueMonotypeToExprRefProjections(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        ext_expr_idx,
                        projections,
                        monotype,
                        monotype_module_idx,
                    );
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
    projections: []const ValueProjection.Projection,
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
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
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
    const resolved_fn_monotype = cm.resolvedMonotype(fn_monotype, fn_monotype_module_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
                if (bindingSourceExprRef(source)) |source_expr_ref| {
                    try propagateDemandedCallableFnMonotypeToExprRef(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        source_expr_ref,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                    return;
                }
            }
        },
        else => {},
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (source.projections.isEmpty() and
            !(sourceContextsEqual(source.source_context, source_context) and
                source.module_idx == module_idx and
                source.expr_idx == expr_idx))
        {
            try propagateDemandedCallableFnMonotypeToValueExpr(
                driver,
                visit_memo,
                result,
                thread,
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

    if (result.getExprTemplateId(source_context, module_idx, expr_idx)) |template_id| {
        _ = try introduceExprCallableValueWithKnownFnMonotype(
            driver,
            visit_memo,
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
            template_id,
            resolved_fn_monotype,
        );
        return;
    }

    switch (expr) {
        .e_if => |if_expr| {
            for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try propagateDemandedCallableFnMonotypeToValueExpr(
                    driver,
                    visit_memo,
                    result,
                    thread,
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
                visit_memo,
                result,
                thread,
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
                    visit_memo,
                    result,
                    thread,
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

fn propagateDemandedCallableFnMonotypeToExprRef(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    expr_ref: ExprRef,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    return propagateDemandedCallableFnMonotypeToExprRefProjections(
        driver,
        visit_memo,
        result,
        thread,
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        result.getValueProjectionEntries(expr_ref.projections),
        fn_monotype,
        fn_monotype_module_idx,
        visiting,
    );
}

fn propagateDemandedCallableFnMonotypeToExprRefProjections(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const ValueProjection.Projection,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
) std.mem.Allocator.Error!void {
    if (projections.len == 0) {
        return propagateDemandedCallableFnMonotypeToValueExpr(
            driver,
            visit_memo,
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
            visiting,
        );
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        const combined_projections = try appendValueProjectionEntries(
            driver,
            result,
            origin.projections,
            projections,
        );
        return propagateDemandedCallableFnMonotypeToExprRefProjections(
            driver,
            visit_memo,
            result,
            thread,
            origin.source_context,
            origin.module_idx,
            origin.expr_idx,
            result.getValueProjectionEntries(combined_projections),
            fn_monotype,
            fn_monotype_module_idx,
            visiting,
        );
    }

    if (readExprCallSite(result, source_context, module_idx, expr_idx)) |call_site| {
        for (result.getCallSiteVariants(call_site)) |callee_callable_inst_id| {
            try ensureCallableInstRealized(driver, visit_memo, result, callee_callable_inst_id);
            const callable_def = result.getCallableDefForInst(callee_callable_inst_id).*;
            const combined_projections = try appendValueProjectionEntries(
                driver,
                result,
                callable_def.body_expr.projections,
                projections,
            );
            const body_thread = try threadForSourceContext(
                driver,
                result,
                callable_def.body_expr.source_context,
            );
            try propagateDemandedCallableFnMonotypeToExprRefProjections(
                driver,
                visit_memo,
                result,
                body_thread,
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
                result.getValueProjectionEntries(combined_projections),
                fn_monotype,
                fn_monotype_module_idx,
                visiting,
            );
        }
        return;
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
                    if (!field_name.textEqualsIdent(driver.all_module_envs, module_idx, field.name)) continue;
                    return propagateDemandedCallableFnMonotypeToExprRefProjections(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        source_context,
                        module_idx,
                        field.value,
                        rest,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                }
            },
            else => {},
        },
        .tuple_elem => |elem_index| switch (expr) {
            .e_tuple => |tuple_expr| {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (elem_index >= elems.len) return;
                return propagateDemandedCallableFnMonotypeToExprRefProjections(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            else => {},
        },
        .tag_payload => |payload| switch (expr) {
            .e_tag => |tag_expr| {
                if (!cm.identsStructurallyEqualAcrossModules(
                    driver.all_module_envs,
                    module_idx,
                    tag_expr.name,
                    payload.tag_name.module_idx,
                    payload.tag_name.ident,
                )) return;
                const args = module_env.store.sliceExpr(tag_expr.args);
                if (payload.payload_index >= args.len) return;
                return propagateDemandedCallableFnMonotypeToExprRefProjections(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    source_context,
                    module_idx,
                    args[payload.payload_index],
                    rest,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            else => {},
        },
        .list_elem => |elem_index| switch (expr) {
            .e_list => |list_expr| {
                const elems = module_env.store.sliceExpr(list_expr.elems);
                if (elem_index >= elems.len) return;
                return propagateDemandedCallableFnMonotypeToExprRefProjections(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            else => {},
        },
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

fn scanStmt(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    stmt_idx: CIR.Statement.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!SemanticThread {
    const module_env = driver.all_module_envs[module_idx];
    const stmt = module_env.store.getStatement(stmt_idx);

    switch (stmt) {
        .s_decl => |decl| {
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, decl.pattern, decl.expr);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, decl.expr, resolve_direct_calls);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, decl.expr)) |expr_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    decl.pattern,
                    expr_mono,
                );
            }
            const source_expr_ref = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, decl.expr);
            var value_bindings: std.ArrayListUnmanaged(ValueLexicalBinding) = .empty;
            defer value_bindings.deinit(driver.allocator);
            try appendPatternValueBindingsFromSourceExpr(
                driver,
                result,
                module_idx,
                decl.pattern,
                source_expr_ref,
                &value_bindings,
            );
            return extendThreadWithValueBindings(driver, thread, value_bindings.items);
        },
        .s_var => |var_decl| {
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, var_decl.expr, resolve_direct_calls);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, var_decl.expr)) |expr_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    var_decl.pattern_idx,
                    expr_mono,
                );
            }
            const source_expr_ref = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, var_decl.expr);
            var value_bindings: std.ArrayListUnmanaged(ValueLexicalBinding) = .empty;
            defer value_bindings.deinit(driver.allocator);
            try appendPatternValueBindingsFromSourceExpr(
                driver,
                result,
                module_idx,
                var_decl.pattern_idx,
                source_expr_ref,
                &value_bindings,
            );
            return extendThreadWithValueBindings(driver, thread, value_bindings.items);
        },
        .s_reassign => |reassign| {
            try propagatePatternDemandToExpr(driver, result, thread, module_idx, reassign.pattern_idx, reassign.expr);
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, reassign.expr, resolve_direct_calls);
            if (try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, reassign.expr)) |expr_mono| {
                try bindCurrentPatternFromResolvedMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    reassign.pattern_idx,
                    expr_mono,
                );
            }
            return thread;
        },
        .s_dbg => |dbg_stmt| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, dbg_stmt.expr, resolve_direct_calls);
            return thread;
        },
        .s_expr => |expr_stmt| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, expr_stmt.expr, resolve_direct_calls);
            return thread;
        },
        .s_expect => |expect_stmt| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, expect_stmt.body, resolve_direct_calls);
            return thread;
        },
        .s_for => |for_stmt| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, for_stmt.expr, resolve_direct_calls);
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
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, for_stmt.body, resolve_direct_calls);
            return thread;
        },
        .s_while => |while_stmt| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, while_stmt.cond, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, while_stmt.body, resolve_direct_calls);
            return thread;
        },
        .s_return => |return_stmt| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, return_stmt.expr, resolve_direct_calls);
            return thread;
        },
        .s_nominal_decl => |nominal_decl| {
            const header = module_env.store.getTypeHeader(nominal_decl.header);
            var value_bindings: std.ArrayListUnmanaged(ValueLexicalBinding) = .empty;
            defer value_bindings.deinit(driver.allocator);

            for (module_env.method_idents.entries.items) |entry| {
                if (entry.key.type_ident != header.name and entry.key.type_ident != header.relative_name) continue;
                const method_node_idx = module_env.getExposedNodeIndexById(entry.value) orelse continue;
                if (!module_env.store.isDefNode(method_node_idx)) continue;

                const def_idx: CIR.Def.Idx = @enumFromInt(method_node_idx);
                const def = module_env.store.getDef(def_idx);
                const source_expr_ref: ExprRef = .{
                    .source_context = .{ .root_expr = .{
                        .module_idx = module_idx,
                        .expr_idx = def.expr,
                    } },
                    .module_idx = module_idx,
                    .expr_idx = def.expr,
                };
                try appendPatternValueBindingsFromSourceExpr(
                    driver,
                    result,
                    module_idx,
                    def.pattern,
                    source_expr_ref,
                    &value_bindings,
                );
            }

            return extendThreadWithValueBindings(driver, thread, value_bindings.items);
        },
        .s_import,
        .s_alias_decl,
        .s_type_anno,
        .s_type_var_alias,
        .s_break,
        .s_crash,
        .s_runtime_error,
        => return thread,
    }
}

fn scanCirExprInternal(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    resolve_direct_calls: bool,
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);

    _ = try cm.resolveExprExactMonotypeResolved(driver, result, thread, module_idx, expr_idx);

    const callable_kind: ?CallableTemplateKind = switch (expr) {
        .e_lambda => .lambda,
        .e_closure => .closure,
        .e_hosted_lambda => .hosted_lambda,
        else => null,
    };
    if (callable_kind) |kind| {
        _ = try result.template_catalog.registerCallableTemplate(
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
    }

    const visit_key = cm.Result.contextExprKey(thread.requireSourceContext(), module_idx, expr_idx);
    const Worker = struct {
        driver: @TypeOf(driver),
        visit_memo: *VisitMemo,
        result: @TypeOf(result),
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        resolve_direct_calls: bool,

        fn run(self: @This()) std.mem.Allocator.Error!void {
            try scanCirExprChildren(
                self.driver,
                self.visit_memo,
                self.result,
                self.thread,
                self.module_idx,
                self.expr_idx,
                self.expr,
                self.resolve_direct_calls,
            );

            try completeCurrentExprMonotype(
                self.driver,
                self.result,
                self.thread,
                self.module_idx,
                self.expr_idx,
            );
        }
    };

    try visit_memo.visitExprOnce(
        driver.allocator,
        visit_key,
        Worker{
            .driver = driver,
            .visit_memo = visit_memo,
            .result = result,
            .thread = thread,
            .module_idx = module_idx,
            .expr_idx = expr_idx,
            .expr = expr,
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
    _ = try cm.resolveExprExactMonotypeResolved(driver, result, thread, module_idx, expr_idx);
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

fn extendThreadWithCallableBindings(
    driver: anytype,
    thread: SemanticThread,
    bindings: []const CallableLexicalBinding,
) std.mem.Allocator.Error!SemanticThread {
    if (bindings.len == 0) return thread;
    const bindings_copy = try driver.allocator.alloc(CallableLexicalBinding, bindings.len);
    @memcpy(bindings_copy, bindings);
    const env = try driver.allocator.create(CallableLexicalEnv);
    env.* = .{
        .parent = thread.callable_env,
        .bindings = bindings_copy,
    };
    return thread.withCallableEnv(env);
}

fn extendThreadWithValueBindings(
    driver: anytype,
    thread: SemanticThread,
    bindings: []const ValueLexicalBinding,
) std.mem.Allocator.Error!SemanticThread {
    if (bindings.len == 0) return thread;
    const bindings_copy = try driver.allocator.alloc(ValueLexicalBinding, bindings.len);
    @memcpy(bindings_copy, bindings);
    const env = try driver.allocator.create(ValueLexicalEnv);
    env.* = .{
        .parent = thread.value_env,
        .bindings = bindings_copy,
    };
    return thread.withValueEnv(env);
}

fn materializeExprCallableSemanticsIfNeeded(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    const source_context = thread.requireSourceContext();
    if (readExprCallableValue(result, source_context, module_idx, expr_idx) != null or
        readExprCallSite(result, source_context, module_idx, expr_idx) != null or
        result.getExprIntroCallableInst(source_context, module_idx, expr_idx) != null)
    {
        return;
    }

    const template_id = result.getExprTemplateId(source_context, module_idx, expr_idx);
    if (template_id == null) {
        const resolved = (try cm.resolveExprMonotypeResolved(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
        )) orelse return;
        if (result.context_mono.monotype_store.getMonotype(resolved.idx) != .func) {
            return;
        }
    }

    var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
    defer visiting.deinit(driver.allocator);
    try realizeStructuredExprCallableSemantics(
        driver,
        visit_memo,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
        &visiting,
    );
}

fn materializeExprSpanCallableSemanticsIfNeeded(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    exprs: []const CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    for (exprs) |child_expr_idx| {
        try materializeExprCallableSemanticsIfNeeded(
            driver,
            visit_memo,
            result,
            thread,
            module_idx,
            child_expr_idx,
        );
    }
}

fn appendPatternValueBindingsFromSourceExpr(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    source: ExprRef,
    out: *std.ArrayListUnmanaged(ValueLexicalBinding),
) std.mem.Allocator.Error!void {
    const module_env = driver.all_module_envs[module_idx];
    try out.append(driver.allocator, .{
        .module_idx = module_idx,
        .pattern_idx = pattern_idx,
        .source = .{ .bound_expr = .{ .expr_ref = source } },
    });

    switch (module_env.store.getPattern(pattern_idx)) {
        .as => |as_pat| try appendPatternValueBindingsFromSourceExpr(
            driver,
            result,
            module_idx,
            as_pat.pattern,
            source,
            out,
        ),
        .nominal => |nominal_pat| try appendPatternValueBindingsFromSourceExpr(
            driver,
            result,
            module_idx,
            nominal_pat.backing_pattern,
            source,
            out,
        ),
        .nominal_external => |nominal_pat| try appendPatternValueBindingsFromSourceExpr(
            driver,
            result,
            module_idx,
            nominal_pat.backing_pattern,
            source,
            out,
        ),
        .tuple => |tuple_pat| {
            for (module_env.store.slicePatterns(tuple_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                const elem_source = try extendExprRef(driver, result, source, .{ .tuple_elem = @intCast(elem_index) });
                try appendPatternValueBindingsFromSourceExpr(
                    driver,
                    result,
                    module_idx,
                    elem_pattern_idx,
                    elem_source,
                    out,
                );
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
                try appendPatternValueBindingsFromSourceExpr(
                    driver,
                    result,
                    module_idx,
                    arg_pattern_idx,
                    arg_source,
                    out,
                );
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
                        try appendPatternValueBindingsFromSourceExpr(
                            driver,
                            result,
                            module_idx,
                            sub_pattern_idx,
                            field_source,
                            out,
                        );
                    },
                    .Rest => {},
                }
            }
        },
        .list => |list_pat| {
            for (module_env.store.slicePatterns(list_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                const elem_source = try extendExprRef(driver, result, source, .{ .list_elem = @intCast(elem_index) });
                try appendPatternValueBindingsFromSourceExpr(
                    driver,
                    result,
                    module_idx,
                    elem_pattern_idx,
                    elem_source,
                    out,
                );
            }
        },
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
    }
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
    return .{
        .source_context = source_context,
        .module_idx = module_idx,
        .expr_idx = expr_idx,
        .projections = .empty(),
    };
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
        .projections = try appendValueProjectionEntries(
            driver,
            result,
            source.projections,
            &.{projection},
        ),
    };
}

pub fn propagatePatternDemandToExpr(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
) std.mem.Allocator.Error!void {
    if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx) != null) {
        return;
    }

    const pattern_mono = try cm.resolvePatternMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        pattern_idx,
    );
    if (pattern_mono.isNone()) return;

    if (thread.lookupValueBinding(module_idx, pattern_idx)) |source| {
        if (bindingSourceExprRef(source)) |source_expr_ref| {
            try propagateDemandedValueMonotypeToExprRef(
                driver,
                result,
                source_expr_ref,
                pattern_mono.idx,
                pattern_mono.module_idx,
            );
        }
    } else {
        try propagateDemandedValueMonotypeToValueExpr(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            pattern_mono.idx,
            pattern_mono.module_idx,
        );
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
        try bindCurrentPatternFromResolvedMonotype(
            driver,
            result,
            SemanticThread.trackedThread(source_context),
            module_idx,
            pattern_idx,
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

fn prebindBlockStmtValueBindings(
    driver: anytype,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    stmts: []const CIR.Statement.Idx,
) std.mem.Allocator.Error!SemanticThread {
    const module_env = driver.all_module_envs[module_idx];
    var bound_thread = thread;

    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const pattern_idx, const expr_idx = switch (stmt) {
            .s_decl => |decl| .{ decl.pattern, decl.expr },
            .s_var => |var_decl| .{ var_decl.pattern_idx, var_decl.expr },
            else => continue,
        };
        const source_expr_ref = exprRefAliasOrSelf(result, bound_thread.requireSourceContext(), module_idx, expr_idx);
        var value_bindings: std.ArrayListUnmanaged(ValueLexicalBinding) = .empty;
        defer value_bindings.deinit(driver.allocator);
        try appendPatternValueBindingsFromSourceExpr(
            driver,
            result,
            module_idx,
            pattern_idx,
            source_expr_ref,
            &value_bindings,
        );
        bound_thread = try extendThreadWithValueBindings(driver, bound_thread, value_bindings.items);
    }

    return bound_thread;
}

const RecursiveBlockCallableEntry = struct {
    pattern_idx: CIR.Pattern.Idx,
    template_id: CallableTemplateId,
    fn_monotype: ResolvedMonotype,
    callable_inst_id: ?CallableInstId = null,
};

fn prebindBlockRecursiveCallableBindings(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    stmts: []const CIR.Statement.Idx,
) std.mem.Allocator.Error!SemanticThread {
    const module_env = driver.all_module_envs[module_idx];
    var entries = std.ArrayListUnmanaged(RecursiveBlockCallableEntry).empty;
    defer entries.deinit(driver.allocator);
    var entry_index_by_pattern = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, usize).empty;
    defer entry_index_by_pattern.deinit(driver.allocator);

    for (stmts) |stmt_idx| {
        const stmt = module_env.store.getStatement(stmt_idx);
        const pattern_idx, const expr_idx = switch (stmt) {
            .s_decl => |decl| .{ decl.pattern, decl.expr },
            .s_var => |var_decl| .{ var_decl.pattern_idx, var_decl.expr },
            else => continue,
        };
        const template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx) orelse continue;
        const template = result.getCallableTemplate(template_id);
        if (template.kind != .closure) continue;
        const fn_monotype = try resolveTemplateFnMonotypeForThread(driver, result, thread, template_id) orelse continue;
        try entry_index_by_pattern.put(driver.allocator, pattern_idx, entries.items.len);
        try entries.append(driver.allocator, .{
            .pattern_idx = pattern_idx,
            .template_id = template_id,
            .fn_monotype = fn_monotype,
        });
    }

    if (entries.items.len == 0) return thread;

    const n = entries.items.len;
    const reach = try driver.allocator.alloc(bool, n * n);
    defer driver.allocator.free(reach);
    @memset(reach, false);

    for (entries.items, 0..) |entry, i| {
        const template = result.getCallableTemplate(entry.template_id);
        const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
            .e_closure => |closure| closure,
            else => continue,
        };
        for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
            const capture = module_env.store.getCapture(capture_idx);
            const target_index = entry_index_by_pattern.get(capture.pattern_idx) orelse continue;
            reach[i * n + target_index] = true;
        }
    }

    var k: usize = 0;
    while (k < n) : (k += 1) {
        var i: usize = 0;
        while (i < n) : (i += 1) {
            if (!reach[i * n + k]) continue;
            var j: usize = 0;
            while (j < n) : (j += 1) {
                if (reach[k * n + j]) {
                    reach[i * n + j] = true;
                }
            }
        }
    }

    var assigned = try driver.allocator.alloc(bool, n);
    defer driver.allocator.free(assigned);
    @memset(assigned, false);

    var recursive_bindings = std.ArrayListUnmanaged(CallableLexicalBinding).empty;
    defer recursive_bindings.deinit(driver.allocator);
    var recursive_group_ids = std.ArrayListUnmanaged(CallableInstId).empty;
    defer recursive_group_ids.deinit(driver.allocator);
    var recursive_group_spans = std.ArrayListUnmanaged(Lambdamono.CallableInstIdSpan).empty;
    defer recursive_group_spans.deinit(driver.allocator);

    var i: usize = 0;
    while (i < n) : (i += 1) {
        if (assigned[i]) continue;
        if (!reach[i * n + i]) continue;

        const group_start: u32 = @intCast(recursive_group_ids.items.len);
        var j: usize = i;
        while (j < n) : (j += 1) {
            if (!(reach[i * n + j] and reach[j * n + i])) continue;
            assigned[j] = true;

            const entry = &entries.items[j];
            const callable_inst_id = (try ensureCallableInstRecord(
                driver,
                result,
                thread.requireSourceContext(),
                entry.template_id,
                entry.fn_monotype.idx,
                entry.fn_monotype.module_idx,
                &.{},
            )).id;
            entry.callable_inst_id = callable_inst_id;
            try recursive_bindings.append(driver.allocator, .{
                .module_idx = module_idx,
                .pattern_idx = entry.pattern_idx,
                .callable_value = .{ .direct = callable_inst_id },
            });
            try recursive_group_ids.append(driver.allocator, callable_inst_id);
        }

        const group_len: u16 = @intCast(recursive_group_ids.items.len - group_start);
        if (group_len != 0) {
            try recursive_group_spans.append(driver.allocator, .{
                .start = group_start,
                .len = group_len,
            });
        }
    }

    if (recursive_bindings.items.len == 0) return thread;

    const bound_thread = try extendThreadWithCallableBindings(
        driver,
        thread,
        recursive_bindings.items,
    );

    for (recursive_group_spans.items) |span| {
        try result.lambdasolved.recordCallableInstRecursiveGroup(
            driver.allocator,
            recursive_group_ids.items[span.start..][0..span.len],
        );
    }
    for (entries.items) |entry| {
        const callable_inst_id = entry.callable_inst_id orelse continue;
        const template = result.getCallableTemplate(entry.template_id);
        const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
            .e_closure => |closure| closure,
            else => unreachable,
        };
        var capture_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
        defer capture_bindings.deinit(driver.allocator);
        try collectClosureCaptureValueBindings(
            driver,
            visit_memo,
            result,
            bound_thread,
            template.module_idx,
            template.runtime_expr,
            closure_expr,
            &capture_bindings,
        );
        if (capture_bindings.items.len != 0) {
            try result.lambdasolved.recordCallableInstCaptureBindings(
                driver.allocator,
                callable_inst_id,
                capture_bindings.items,
            );
        }
    }

    return bound_thread;
}

pub fn includeExprCallableValue(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
    variant_builder: anytype,
) std.mem.Allocator.Error!void {
    try realizeStructuredExprCallableSemantics(
        driver,
        visit_memo,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
        visiting,
    );
    if (getValueExprCallableValueForSourceContext(driver, visit_memo, result, source_context, module_idx, expr_idx)) |callable_value| {
        try variant_builder.includeCallableValue(driver, result, callable_value);
    }
}

fn recordPackedCallableShapeForExprIfCallable(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    packed_fn: Lambdamono.PackedFn,
) std.mem.Allocator.Error!void {
    if (result.getExprCallableValue(source_context, module_idx, expr_idx) == null and
        result.getExprIntroCallableInst(source_context, module_idx, expr_idx) == null)
    {
        return;
    }
    try recordExprCallableValue(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        .{ .packed_fn = packed_fn },
    );
}

fn introduceExprCallableValueWithKnownFnMonotype(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
    fn_monotype: ResolvedMonotype,
) std.mem.Allocator.Error!?CallableInstId {
    if (fn_monotype.isNone()) return null;
    if (result.context_mono.monotype_store.getMonotype(fn_monotype.idx) != .func) {
        return null;
    }
    const template = result.getCallableTemplate(template_id);
    if (template.kind == .closure and switch (source_context) {
        .template_expr => true,
        .callable_inst, .root_expr, .provenance_expr => false,
    }) {
        return null;
    }
    const callable_inst_id = (try ensureCallableInstRecord(
        driver,
        result,
        source_context,
        template_id,
        fn_monotype.idx,
        fn_monotype.module_idx,
        &.{},
    )).id;
    if (template.kind == .closure and thread != null) {
        const capture_thread = try captureBindingThreadForCallableInst(
            driver,
            result,
            thread,
            callable_inst_id,
        );
        try ensureCallableInstCaptureBindingsFromThread(
            driver,
            visit_memo,
            result,
            capture_thread,
            callable_inst_id,
        );
    }
    try setExprDirectCallable(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        callable_inst_id,
    );
    return callable_inst_id;
}

fn ensureCallableValueVariantsRealizedFromThread(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    callable_value: CallableValue,
) std.mem.Allocator.Error!void {
    for (result.getCallableValueVariants(callable_value)) |callable_inst_id| {
        const capture_thread = try captureBindingThreadForCallableInst(
            driver,
            result,
            thread,
            callable_inst_id,
        );
        try ensureCallableInstCaptureBindingsFromThread(
            driver,
            visit_memo,
            result,
            capture_thread,
            callable_inst_id,
        );
        if (try callableInstReadyForRealization(driver, result, callable_inst_id)) {
            try ensureCallableInstRealized(driver, visit_memo, result, callable_inst_id);
        }
    }
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
    try recordExprCallableValue(
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
    try recordExprCallableValue(driver, result, source_context, module_idx, expr_idx, callable_value);
}

pub fn setExprDirectCallSite(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst_id: CallableInstId,
) std.mem.Allocator.Error!void {
    return recordExprCallSite(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        .{ .direct = callable_inst_id },
    );
}

fn realizeLookupExprSemantics(
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
            if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
                if (bindingSourceExprRef(source)) |source_expr_ref| {
                    try recordExprSourceExpr(driver, result, source_context, module_idx, expr_idx, source_expr_ref);
                    try recordExprLookupResolution(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                        .{ .expr = source_expr_ref },
                    );
                }
            } else if (result.template_catalog.getLocalCallableTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                const template = result.getCallableTemplate(template_id);
                if (std.debug.runtime_safety and template.owner != .root_scope) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: lookup_local expr {d} in module {d} resolved to non-root callable template {d} through local-pattern catalog path",
                        .{ @intFromEnum(expr_idx), module_idx, @intFromEnum(template_id) },
                    );
                }
                const source_expr_ref: ExprRef = .{
                    .source_context = .{ .root_expr = .{
                        .module_idx = template.module_idx,
                        .expr_idx = template.cir_expr,
                    } },
                    .module_idx = template.module_idx,
                    .expr_idx = template.cir_expr,
                };
                try recordExprSourceExpr(driver, result, source_context, module_idx, expr_idx, source_expr_ref);
                try recordExprLookupResolution(
                    driver,
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    .{ .expr = source_expr_ref },
                );
            } else if (findDefByPatternInModule(module_env, lookup.pattern_idx)) |def_idx| {
                try recordExprLookupResolution(
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
            try recordExprLookupResolution(
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
            try recordExprLookupResolution(
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

fn scanCirExprChildren(
    driver: anytype,
    visit_memo: *VisitMemo,
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
        .e_str => |str_expr| try scanCirValueExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, module_env.store.sliceExpr(str_expr.span), resolve_direct_calls),
        .e_list => |list_expr| {
            const elems = module_env.store.sliceExpr(list_expr.elems);
            try scanCirValueExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, elems, resolve_direct_calls);
            try materializeExprSpanCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, elems);
        },
        .e_tuple => |tuple_expr| {
            try recordDemandedTupleElemMonotypes(driver, result, thread, module_idx, expr_idx, tuple_expr);
            const elems = module_env.store.sliceExpr(tuple_expr.elems);
            try scanCirValueExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, elems, resolve_direct_calls);
            try materializeExprSpanCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, elems);
        },
        .e_match => |match_expr| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, match_expr.cond, resolve_direct_calls);
            const cond_source = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, match_expr.cond);
            const branches = module_env.store.sliceMatchBranches(match_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getMatchBranch(branch_idx);
                var branch_value_bindings: std.ArrayListUnmanaged(ValueLexicalBinding) = .empty;
                defer branch_value_bindings.deinit(driver.allocator);
                for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                    const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                    try appendPatternValueBindingsFromSourceExpr(
                        driver,
                        result,
                        module_idx,
                        branch_pattern.pattern,
                        cond_source,
                        &branch_value_bindings,
                    );
                }
                const branch_thread = try extendThreadWithValueBindings(
                    driver,
                    thread,
                    branch_value_bindings.items,
                );
                try propagateDemandedValueResultMonotypeToChild(driver, result, branch_thread, module_idx, expr_idx, branch.value);
                try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, branch_thread, module_idx, branch.value, resolve_direct_calls);
                if (branch.guard) |guard_expr| {
                    try scanCirExprWithDirectCallResolution(driver, visit_memo, result, branch_thread, module_idx, guard_expr, resolve_direct_calls);
                }
            }
            try completeCurrentExprMonotype(driver, result, thread, module_idx, match_expr.cond);
        },
        .e_if => |if_expr| {
            const branches = module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, branch.cond, resolve_direct_calls);
                try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, branch.body);
                try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, branch.body, resolve_direct_calls);
            }
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, if_expr.final_else);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, if_expr.final_else, resolve_direct_calls);
        },
        .e_call => |call_expr| {
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            try scanCirValueExprWithDirectCallResolution(
                driver,
                visit_memo,
                result,
                thread,
                module_idx,
                call_expr.func,
                resolve_direct_calls,
            );
            try scanCirValueExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, arg_exprs, resolve_direct_calls);
            if (directCalleeLowLevelOp(driver, result, thread, module_idx, call_expr.func)) |low_level_op| {
                if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                    try resolveStrInspectHelperCallableInstsForTypeVar(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        ModuleEnv.varFrom(arg_exprs[0]),
                    );
                }
            }
            if (resolve_direct_calls) {
                try assignCallableArgCallableInstsFromCallMonotype(driver, visit_memo, result, thread, module_idx, expr_idx, call_expr);
                const callee_template = if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, call_expr.func)) |template_id|
                    template_id
                else if (result.getExprOriginExpr(thread.requireSourceContext(), module_idx, call_expr.func)) |origin|
                    result.getExprTemplateId(origin.source_context, origin.module_idx, origin.expr_idx)
                else
                    null;
                if (callee_template) |template_id| {
                    if (try resolveDemandedDirectCallFnMonotype(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        call_expr,
                    )) |desired_fn_monotype| {
                        if (!desired_fn_monotype.isNone()) {
                            var required_callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
                            defer required_callable_param_specs.deinit(driver.allocator);
                            const specs_complete = try collectDirectCallCallableParamSpecs(
                                driver,
                                visit_memo,
                                result,
                                thread,
                                thread.requireSourceContext(),
                                module_idx,
                                desired_fn_monotype.idx,
                                desired_fn_monotype.module_idx,
                                module_env.store.sliceExpr(call_expr.args),
                                &required_callable_param_specs,
                            );
                            if (specs_complete) {
                                const template_source_context = requireTemplateDemandSourceContextForExpr(
                                    driver,
                                    visit_memo,
                                    result,
                                    thread.requireSourceContext(),
                                    module_idx,
                                    call_expr.func,
                                    template_id,
                                );
                                const template = result.getCallableTemplate(template_id).*;
                                const defining_source_context = resolveTemplateDefiningSourceContext(
                                    result,
                                    template_source_context,
                                    template,
                                );
                                const callable_inst_id = try requireCallableInstWithCallableParamSpecs(
                                    driver,
                                    visit_memo,
                                    result,
                                    thread,
                                    defining_source_context,
                                    template_id,
                                    desired_fn_monotype.idx,
                                    desired_fn_monotype.module_idx,
                                    required_callable_param_specs.items,
                                );
                                try finalizeResolvedDirectCallCallableInst(
                                    driver,
                                    visit_memo,
                                    result,
                                    thread,
                                    module_idx,
                                    expr_idx,
                                    call_expr,
                                    module_env.store.getExpr(call_expr.func),
                                    callable_inst_id,
                                );
                            }
                        }
                    }
                }
                try resolveDirectCallSite(driver, visit_memo, result, thread, module_idx, expr_idx, call_expr);
                if (std.debug.runtime_safety and readExprCallSite(result, thread.requireSourceContext(), module_idx, expr_idx) == null) {
                    const debug_callee_template = if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, call_expr.func)) |template_id|
                        template_id
                    else if (result.getExprOriginExpr(thread.requireSourceContext(), module_idx, call_expr.func)) |origin|
                        result.getExprTemplateId(origin.source_context, origin.module_idx, origin.expr_idx)
                    else
                        null;
                    const desired_fn_monotype = try resolveDemandedDirectCallFnMonotype(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        call_expr,
                    );
                    const callee_callable_value = getValueExprCallableValueForThread(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        call_expr.func,
                    );
                    const callee_callable_inst = getValueExprCallableInstForThread(
                        driver,
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        call_expr.func,
                    );
                    std.debug.panic(
                        "Lambdasolved invariant violated: e_call expr {d} in module {d} under source context {s} completed direct-call scan without recording a call site; callee_expr={d} callee_tag={s} callee_template={?d} desired_fn_monotype={?any} callee_callable_value={?any} callee_callable_inst={?d}",
                        .{
                            @intFromEnum(expr_idx),
                            module_idx,
                            @tagName(thread.requireSourceContext()),
                            @intFromEnum(call_expr.func),
                            @tagName(module_env.store.getExpr(call_expr.func)),
                            if (debug_callee_template) |template_id| @intFromEnum(template_id) else null,
                            desired_fn_monotype,
                            callee_callable_value,
                            if (callee_callable_inst) |callable_inst_id| @intFromEnum(callable_inst_id) else null,
                        },
                    );
                }
            }
        },
        .e_record => |record_expr| {
            if (record_expr.ext) |ext_expr| {
                try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, ext_expr, resolve_direct_calls);
                try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, ext_expr);
            }

            try recordDemandedRecordFieldMonotypes(driver, result, thread, module_idx, expr_idx, record_expr);

            const fields = module_env.store.sliceRecordFields(record_expr.fields);
            for (fields) |field_idx| {
                const field = module_env.store.getRecordField(field_idx);
                try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, field.value, resolve_direct_calls);
                try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, field.value);
            }
        },
        .e_block => |block_expr| {
            const stmts = module_env.store.sliceStatements(block_expr.stmts);
            try preRegisterBlockCallableStmtTemplates(driver, result, thread, module_idx, stmts);
            var block_thread = try prebindBlockStmtValueBindings(
                driver,
                result,
                thread,
                module_idx,
                stmts,
            );
            block_thread = try prebindBlockRecursiveCallableBindings(
                driver,
                visit_memo,
                result,
                block_thread,
                module_idx,
                stmts,
            );
            for (stmts) |stmt_idx| {
                block_thread = try scanStmt(driver, visit_memo, result, block_thread, module_idx, stmt_idx, resolve_direct_calls);
            }
            try propagateDemandedValueResultMonotypeToChild(driver, result, block_thread, module_idx, expr_idx, block_expr.final_expr);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, block_thread, module_idx, block_expr.final_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                block_thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, block_thread.requireSourceContext(), module_idx, block_expr.final_expr),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, block_thread, module_idx, expr_idx);
        },
        .e_tag => |tag_expr| {
            const args = module_env.store.sliceExpr(tag_expr.args);
            try scanCirValueExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, args, resolve_direct_calls);
            try materializeExprSpanCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, args);
        },
        .e_nominal => |nominal_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
        },
        .e_nominal_external => |nominal_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
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
            var capture_bindings = std.ArrayListUnmanaged(ValueLexicalBinding).empty;
            defer capture_bindings.deinit(driver.allocator);
            try collectClosureCaptureValueBindings(
                driver,
                visit_memo,
                result,
                thread,
                module_idx,
                expr_idx,
                closure_expr,
                &capture_bindings,
            );
        },
        .e_lambda => {},
        .e_binop => |binop_expr| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, binop_expr.lhs, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, binop_expr.rhs, resolve_direct_calls);
            try realizeDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr);
        },
        .e_unary_minus => |unary_expr| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, unary_expr.expr, resolve_direct_calls);
        },
        .e_unary_not => |unary_expr| try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, unary_expr.expr, resolve_direct_calls),
        .e_dot_access => |dot_expr| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, dot_expr.receiver, resolve_direct_calls);
            if (dot_expr.args) |args| {
                const arg_exprs = module_env.store.sliceExpr(args);
                try scanDispatchArgsWithDirectCallResolution(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    module_idx,
                    arg_exprs,
                    resolve_direct_calls,
                    false,
                );
                if (try ensureRecordedExactDispatchSiteFromDemandedFnMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    dot_expr.field_name,
                )) |site| {
                    try bindCurrentDispatchFromFnMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        site.fn_monotype.idx,
                        site.fn_monotype.module_idx,
                    );
                }
                try scanDispatchArgsWithDirectCallResolution(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    module_idx,
                    arg_exprs,
                    resolve_direct_calls,
                    true,
                );
                if (try ensureRecordedExactDispatchSiteFromDemandedFnMonotype(
                    driver,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    dot_expr.field_name,
                )) |site| {
                    try bindCurrentDispatchFromFnMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        site.fn_monotype.idx,
                        site.fn_monotype.module_idx,
                    );
                }
                try realizeDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr);
            } else {
                const receiver_source = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dot_expr.receiver);
                const field_source = try extendExprRef(driver, result, receiver_source, .{ .field = .{
                    .module_idx = module_idx,
                    .ident = dot_expr.field_name,
                } });
                try recordExprSourceExpr(driver, result, thread.requireSourceContext(), module_idx, expr_idx, field_source);
                try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
            }
        },
        .e_tuple_access => |tuple_access| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, tuple_access.tuple, resolve_direct_calls);
            const tuple_source = exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, tuple_access.tuple);
            const elem_source = try extendExprRef(driver, result, tuple_source, .{ .tuple_elem = tuple_access.elem_index });
            try recordExprSourceExpr(driver, result, thread.requireSourceContext(), module_idx, expr_idx, elem_source);
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
        },
        .e_dbg => |dbg_expr| {
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, dbg_expr.expr);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, dbg_expr.expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dbg_expr.expr),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
        },
        .e_expect => |expect_expr| {
            try propagateDemandedValueResultMonotypeToChild(driver, result, thread, module_idx, expr_idx, expect_expr.body);
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, expect_expr.body, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, expect_expr.body),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
        },
        .e_return => |return_expr| {
            try scanCirValueExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, return_expr.expr, resolve_direct_calls);
            try recordExprSourceExpr(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, return_expr.expr),
            );
            try materializeExprCallableSemanticsIfNeeded(driver, visit_memo, result, thread, module_idx, expr_idx);
        },
        .e_type_var_dispatch => |dispatch_expr| {
            try scanCirExprSpanWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, module_env.store.sliceExpr(dispatch_expr.args), resolve_direct_calls);
            try realizeDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr);
        },
        .e_for => |for_expr| {
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, for_expr.expr, resolve_direct_calls);
            try scanCirExprWithDirectCallResolution(driver, visit_memo, result, thread, module_idx, for_expr.body, resolve_direct_calls);
        },
        .e_hosted_lambda => {},
        .e_run_low_level => |run_low_level| {
            const args = module_env.store.sliceExpr(run_low_level.args);
            try scanCirExprSpan(driver, visit_memo, result, thread, module_idx, args);
            if (run_low_level.op == .str_inspect and args.len != 0) {
                try resolveStrInspectHelperCallableInstsForTypeVar(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    module_idx,
                    ModuleEnv.varFrom(args[0]),
                );
            }
        },
    }
}

fn recordCallResultCallableValueFromCallee(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    target_source_context: SourceContext,
    target_module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    callee_callable_inst_id: CallableInstId,
    visiting: *std.AutoHashMapUnmanaged(ContextExprKey, void),
    variant_builder: anytype,
) std.mem.Allocator.Error!void {
    try ensureCallableInstRealized(driver, visit_memo, result, callee_callable_inst_id);
    const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
    const callee_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callee_callable_inst.fn_monotype)) {
        .func => |func| func,
        else => unreachable,
    };
    if (result.context_mono.monotype_store.getMonotype(callee_fn_mono.ret) != .func) return;

    const in_progress_key = CallResultCallableInstKey{
        .context_expr = @TypeOf(result.*).contextExprKey(
            target_source_context,
            target_module_idx,
            call_expr_idx,
        ),
        .callee_callable_inst_raw = @intFromEnum(callee_callable_inst_id),
    };

    const Worker = struct {
        driver_worker: @TypeOf(driver),
        visit_memo_worker: *VisitMemo,
        result_worker: @TypeOf(result),
        callee_callable_inst_id_worker: CallableInstId,
        callee_fn_ret_worker: Monotype.Idx,
        callee_fn_module_idx_worker: u32,
        visiting_worker: *std.AutoHashMapUnmanaged(ContextExprKey, void),
        variant_builder_worker: @TypeOf(variant_builder),

        fn run(worker: @This()) std.mem.Allocator.Error!void {
            try ensureCallableInstRealized(
                worker.driver_worker,
                worker.visit_memo_worker,
                worker.result_worker,
                worker.callee_callable_inst_id_worker,
            );
            const callable_def = worker.result_worker.getCallableDefForInst(worker.callee_callable_inst_id_worker).*;
            if (worker.result_worker.getExprCallableValue(
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
            ) == null) {
                const template_id = worker.result_worker.getExprTemplateId(
                    callable_def.body_expr.source_context,
                    callable_def.body_expr.module_idx,
                    callable_def.body_expr.expr_idx,
                ) orelse std.debug.panic(
                    "Lambdasolved invariant violated: function-valued body expr {d} for callable inst {d} had no executable callable value fact and no registered callable template",
                    .{
                        @intFromEnum(callable_def.body_expr.expr_idx),
                        @intFromEnum(worker.callee_callable_inst_id_worker),
                    },
                );
                const materialize_callable_inst = try introduceExprCallableValueWithKnownFnMonotype(
                    worker.driver_worker,
                    worker.visit_memo_worker,
                    worker.result_worker,
                    try threadForSourceContext(
                        worker.driver_worker,
                        worker.result_worker,
                        callable_def.body_expr.source_context,
                    ),
                    callable_def.body_expr.source_context,
                    callable_def.body_expr.module_idx,
                    callable_def.body_expr.expr_idx,
                    template_id,
                    cm.resolvedMonotype(worker.callee_fn_ret_worker, worker.callee_fn_module_idx_worker),
                );
                if (std.debug.runtime_safety and
                    worker.result_worker.getExprCallableValue(
                        callable_def.body_expr.source_context,
                        callable_def.body_expr.module_idx,
                        callable_def.body_expr.expr_idx,
                    ) == null)
                {
                    std.debug.panic(
                        "Lambdasolved invariant violated: function-valued body expr {d} for callable inst {d} failed to materialize callable value; reason={?s} template={d}",
                        .{
                            @intFromEnum(callable_def.body_expr.expr_idx),
                            @intFromEnum(worker.callee_callable_inst_id_worker),
                            if (materialize_callable_inst != null) "introduced" else null,
                            @intFromEnum(template_id),
                        },
                    );
                }
            }
            try includeExprCallableValue(
                worker.driver_worker,
                worker.visit_memo_worker,
                worker.result_worker,
                try threadForSourceContext(
                    worker.driver_worker,
                    worker.result_worker,
                    callable_def.body_expr.source_context,
                ),
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
                worker.visiting_worker,
                worker.variant_builder_worker,
            );
        }
    };

    try visit_memo.withCallResult(
        driver.allocator,
        in_progress_key,
        Worker{
            .driver_worker = driver,
            .visit_memo_worker = visit_memo,
            .result_worker = result,
            .callee_callable_inst_id_worker = callee_callable_inst_id,
            .callee_fn_ret_worker = callee_fn_mono.ret,
            .callee_fn_module_idx_worker = callee_callable_inst.fn_monotype_module_idx,
            .visiting_worker = visiting,
            .variant_builder_worker = variant_builder,
        },
    );
}

pub fn callableInstSatisfiesCallSiteRequirements(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
    desired_fn_monotype: ?cm.ResolvedMonotype,
    required_template: ?CallableTemplateId,
    required_callable_param_specs: []const CallableParamSpecEntry,
) std.mem.Allocator.Error!bool {
    const callable_inst = result.getCallableInst(callable_inst_id);

    if (required_template) |template_id| {
        if (callable_inst.template != template_id) return false;
    }

    if (desired_fn_monotype) |desired| {
        if (desired.isNone()) {
            return callableParamSpecsEqual(
                result,
                result.getCallableParamSpecEntries(result.getCallableInst(callable_inst_id).callable_param_specs),
                required_callable_param_specs,
            );
        }
        if (!try cm.monotypesStructurallyEqualAcrossModules(
            driver,
            result,
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
            desired.idx,
            desired.module_idx,
        )) return false;
    }

    return callableParamSpecsEqual(
        result,
        result.getCallableParamSpecEntries(result.getCallableInst(callable_inst_id).callable_param_specs),
        required_callable_param_specs,
    );
}

pub fn requireTemplateDemandSourceContextForExpr(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    template_id: CallableTemplateId,
) SourceContext {
    const template = result.getCallableTemplate(template_id);
    if (template.kind != .closure) {
        return source_context;
    }
    if (getValueExprCallableValueForSourceContext(driver, visit_memo, result, source_context, module_idx, expr_idx)) |callable_value| {
        switch (callable_value) {
            .direct => |callable_inst_id| {
                if (result.getCallableInst(callable_inst_id).template == template_id) {
                    return result.getCallableInst(callable_inst_id).defining_source_context;
                }
            },
            .packed_fn => {},
        }
    }

    if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
        if (switch (source.source_context) {
            .callable_inst => true,
            .root_expr, .provenance_expr, .template_expr => false,
        }) {
            return source.source_context;
        }
        if (getValueExprCallableValueForSourceContext(
            driver,
            visit_memo,
            result,
            source.source_context,
            source.module_idx,
            source.expr_idx,
        )) |callable_value| {
            switch (callable_value) {
                .direct => |callable_inst_id| {
                    if (result.getCallableInst(callable_inst_id).template == template_id) {
                        return result.getCallableInst(callable_inst_id).defining_source_context;
                    }
                },
                .packed_fn => {},
            }
        }
    }

    switch (source_context) {
        .callable_inst, .root_expr, .provenance_expr => return source_context,
        .template_expr => {},
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "Lambdasolved invariant violated: closure expr {d} in module {d} required a concrete lexical source context for template {d} but none was available in ctx={s}",
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
    return try cm.remapMonotypeBetweenModules(
        driver,
        result,
        callee_monotype.idx,
        callee_monotype.module_idx,
        module_idx,
    );
}

pub fn resolveDemandedDirectCallFnMonotype(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!?cm.ResolvedMonotype {
    _ = call_expr_idx;
    if (getValueExprCallableValueForThread(driver, visit_memo, result, thread, module_idx, call_expr.func)) |callable_value| {
        const resolved: cm.ResolvedMonotype = switch (callable_value) {
            .direct => |callable_inst_id| .{
                .idx = result.getCallableInst(callable_inst_id).fn_monotype,
                .module_idx = result.getCallableInst(callable_inst_id).fn_monotype_module_idx,
            },
            .packed_fn => |packed_fn| packed_fn.fn_monotype,
        };
        if (std.debug.runtime_safety) {
            const mono = result.context_mono.monotype_store.getMonotype(resolved.idx);
            switch (mono) {
                .func => |func| {
                    if (func.args.len != 0) {
                        const first_arg = result.context_mono.monotype_store.getIdxSpanItem(func.args, 0);
                        if (result.context_mono.monotype_store.getMonotype(first_arg) == .unit) {
                            std.debug.print(
                                "DEBUG resolveDemandedDirectCallFnMonotype callable_value: ctx={s} module={d} callee={d} fn={d}@{d} callable_value={any}\n",
                                .{
                                    @tagName(thread.requireSourceContext()),
                                    module_idx,
                                    @intFromEnum(call_expr.func),
                                    @intFromEnum(resolved.idx),
                                    resolved.module_idx,
                                    callable_value,
                                },
                            );
                        }
                    }
                },
                else => {},
            }
        }
        return resolved;
    }
    if (try cm.resolveExprExactMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        call_expr.func,
        )) |callee_mono| {
        if (cm.resolvedIfFunctionMonotype(result, callee_mono)) |fn_monotype| {
            if (std.debug.runtime_safety) {
                const mono = result.context_mono.monotype_store.getMonotype(fn_monotype.idx);
                switch (mono) {
                    .func => |func| {
                        if (func.args.len != 0) {
                            const first_arg = result.context_mono.monotype_store.getIdxSpanItem(func.args, 0);
                            if (result.context_mono.monotype_store.getMonotype(first_arg) == .unit) {
                                std.debug.print(
                                    "DEBUG resolveDemandedDirectCallFnMonotype expr_exact: ctx={s} module={d} callee={d} fn={d}@{d}\n",
                                    .{
                                        @tagName(thread.requireSourceContext()),
                                        module_idx,
                                        @intFromEnum(call_expr.func),
                                        @intFromEnum(fn_monotype.idx),
                                        fn_monotype.module_idx,
                                    },
                                );
                            }
                        }
                    },
                    else => {},
                }
            }
            return fn_monotype;
        }
    }
    return null;
}

pub fn resolveDirectCallSite(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: SemanticThread,
    module_idx: u32,
    call_expr_idx: CIR.Expr.Idx,
    call_expr: anytype,
) std.mem.Allocator.Error!void {
    const callee_expr_idx = call_expr.func;
    const module_env = driver.all_module_envs[module_idx];
    const callee_expr = module_env.store.getExpr(callee_expr_idx);
    try materializeExprCallableSemanticsIfNeeded(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        callee_expr_idx,
    );
    var resolved_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, callee_expr_idx);
    if (resolved_template_id == null) {
        if (result.getExprOriginExpr(thread.requireSourceContext(), module_idx, callee_expr_idx)) |origin| {
            resolved_template_id = result.getExprTemplateId(
                origin.source_context,
                origin.module_idx,
                origin.expr_idx,
            );
        }
    }
    if (resolved_template_id == null and callee_expr == .e_lookup_local) {
        const lookup = callee_expr.e_lookup_local;
        if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
            if (bindingSourceExprRef(source)) |source_expr_ref| {
                resolved_template_id = result.getExprTemplateId(
                    source_expr_ref.source_context,
                    source_expr_ref.module_idx,
                    source_expr_ref.expr_idx,
                );
            }
        }
    }
    const desired_fn_monotype = try resolveDemandedDirectCallFnMonotype(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
    );
    if (desired_fn_monotype != null and
        getValueExprCallableValueForThread(driver, visit_memo, result, thread, module_idx, callee_expr_idx) == null)
    {
        var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer visiting.deinit(driver.allocator);
        try propagateDemandedCallableFnMonotypeToValueExpr(
            driver,
            visit_memo,
            result,
            thread,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            desired_fn_monotype.?.idx,
            desired_fn_monotype.?.module_idx,
            &visiting,
        );
        resolved_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, callee_expr_idx);
    }
    const resolved_low_level_op = if (resolved_template_id) |template_id|
        result.getCallableTemplate(template_id).low_level_op
    else
        directCalleeLowLevelOp(driver, result, thread, module_idx, call_expr.func);
    if (resolved_low_level_op) |low_level_op| {
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        try recordExprCallSite(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            call_expr_idx,
            .{ .low_level = .{
                .op = low_level_op,
            } },
        );
        if (low_level_op == .str_inspect and arg_exprs.len != 0) {
            try resolveStrInspectHelperCallableInstsForTypeVar(
                driver,
                visit_memo,
                result,
                thread,
                module_idx,
                ModuleEnv.varFrom(arg_exprs[0]),
            );
        }
        return;
    }

    if (resolved_template_id == null) {
        if (getValueExprCallableInstForThread(driver, visit_memo, result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
            resolved_template_id = result.getCallableInst(callable_inst_id).template;
        }
    }
    if (resolved_template_id == null) {
        var callee_visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
        defer callee_visiting.deinit(driver.allocator);
        try realizeStructuredExprCallableSemantics(
            driver,
            visit_memo,
            result,
            thread,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            &callee_visiting,
        );
        if (getValueExprCallableInstForThread(driver, visit_memo, result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
            resolved_template_id = result.getCallableInst(callable_inst_id).template;
        }
    }

    var required_callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
    defer required_callable_param_specs.deinit(driver.allocator);
    if (desired_fn_monotype != null) {
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        const specs_complete = try collectDirectCallCallableParamSpecs(
            driver,
            visit_memo,
            result,
            thread,
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
        if (desired_fn_monotype) |desired| {
            if (!desired.isNone()) {
                const template_source_context = requireTemplateDemandSourceContextForExpr(
                    driver,
                    visit_memo,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    callee_expr_idx,
                    template_id,
                );
                const template = result.getCallableTemplate(template_id).*;
                const defining_source_context = resolveTemplateDefiningSourceContext(
                    result,
                    template_source_context,
                    template,
                );
                const callable_inst_id = try requireCallableInstWithCallableParamSpecs(
                    driver,
                    visit_memo,
                    result,
                    thread,
                    defining_source_context,
                    template_id,
                    desired.idx,
                    desired.module_idx,
                    required_callable_param_specs.items,
                );
                try finalizeResolvedDirectCallCallableInst(
                    driver,
                    visit_memo,
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
    }

    if (resolved_template_id) |template_id| {
        const template_source_context = requireTemplateDemandSourceContextForExpr(
            driver,
            visit_memo,
            result,
            thread.requireSourceContext(),
            module_idx,
            callee_expr_idx,
            template_id,
        );
        const exact_callable_inst = try specializeDirectCallExactCallable(
            driver,
            visit_memo,
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr,
            template_id,
            template_source_context,
        );
        if (exact_callable_inst) |callable_inst_id| {
            try finalizeResolvedDirectCallCallableInst(
                driver,
                visit_memo,
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
        if (std.debug.runtime_safety and desired_fn_monotype != null and !desired_fn_monotype.?.isNone()) {
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            var required_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
            defer required_specs.deinit(driver.allocator);
            const specs_complete = try collectDirectCallCallableParamSpecs(
                driver,
                visit_memo,
                result,
                thread,
                thread.requireSourceContext(),
                module_idx,
                desired_fn_monotype.?.idx,
                desired_fn_monotype.?.module_idx,
                arg_exprs,
                &required_specs,
            );
            std.debug.panic(
                "Lambdasolved exact direct-call specialization returned null for expr={d} module={d} callee_expr={d} template={d} template_kind={s} template_source_context={s} desired_fn_monotype={any} directCallContainsErrorType={} specs_complete={} required_specs_len={d}",
                .{
                    @intFromEnum(call_expr_idx),
                    module_idx,
                    @intFromEnum(callee_expr_idx),
                    @intFromEnum(template_id),
                    @tagName(result.getCallableTemplate(template_id).kind),
                    @tagName(template_source_context),
                    desired_fn_monotype.?,
                    try directCallContainsErrorType(driver, module_idx, call_expr_idx, call_expr),
                    specs_complete,
                    required_specs.items.len,
                },
            );
        }
        if (std.debug.runtime_safety) {
            switch (callee_expr) {
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
                    const exact_call_fn_monotype_idx = (try resolveDirectCallFnMonotype(
                        driver,
                        result,
                        thread,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                    )) orelse unreachable;
                    const exact_call_fn_monotype = cm.resolvedMonotype(
                        exact_call_fn_monotype_idx,
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
    }

    if (std.debug.runtime_safety) {
        switch (callee_expr) {
            .e_lookup_external, .e_lookup_required => {
                const current_call_site = readExprCallSite(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    call_expr_idx,
                );
                const template_source_context = if (resolved_template_id) |template_id|
                    requireTemplateDemandSourceContextForExpr(
                        driver,
                        visit_memo,
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
                            result.getIndirectCallVariants(indirect).len,
                        },
                    ),
                    .low_level => unreachable,
                }
            },
            else => {},
        }
    }

    if (getValueExprCallableInstForThread(driver, visit_memo, result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
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
                visit_memo,
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
        if (std.debug.runtime_safety) {
            const callable_inst = result.getCallableInst(callable_inst_id);
            const template = result.getCallableTemplate(callable_inst.template);
            std.debug.panic(
                "Lambdasolved rejected direct-call callable inst {d} for expr={d} module={d} callee_expr={d} tag={s}; template={d} template_kind={s} desired_fn_monotype={?any} resolved_template={?d} required_param_specs_len={d}",
                .{
                    @intFromEnum(callable_inst_id),
                    @intFromEnum(call_expr_idx),
                    module_idx,
                    @intFromEnum(callee_expr_idx),
                    @tagName(callee_expr),
                    @intFromEnum(callable_inst.template),
                    @tagName(template.kind),
                    desired_fn_monotype,
                    if (resolved_template_id) |template_id| @intFromEnum(template_id) else null,
                    required_callable_param_specs.items.len,
                },
            );
        }
        return;
    }

    if (getValueExprCallableValueForThread(driver, visit_memo, result, thread, module_idx, callee_expr_idx)) |callable_value| {
        var all_satisfy = true;
        for (result.getCallableValueVariants(callable_value)) |callable_inst_id| {
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
            try recordExprCallSite(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                call_expr_idx,
                (try call_variant_builder.finishCallSite(driver, result)).?,
            );
            for (result.getCallableValueVariants(callable_value)) |callable_inst_id| {
                try recordCallResultCallableValueFromCallee(
                    driver,
                    visit_memo,
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
                try setExprCallableValue(
                    driver,
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
                        visit_memo,
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

    if (!calleeUsesFirstClassCallableValuePath(callee_expr)) {
        if (std.debug.runtime_safety) {
            const callee_region = module_env.store.getExprRegion(callee_expr_idx);
            const module_source = module_env.getSourceAll();
            const snippet_start = @min(callee_region.start.offset, module_source.len);
            const snippet_end = @min(callee_region.end.offset, module_source.len);
            var lookup_pattern: u32 = std.math.maxInt(u32);
            var callable_hit = false;
            var value_hit = false;
            var value_source: ?[]const u8 = null;
            var bound_source_context: ?[]const u8 = null;
            var bound_source_module: ?u32 = null;
            var bound_source_expr: ?u32 = null;
            var bound_source_expr_tag: ?[]const u8 = null;
            var bound_source_template: ?u32 = null;
            var bound_source_callable: ?CallableValue = null;
            var bound_source_expr_monotype: ?ResolvedMonotype = null;
            var bound_source_template_fn_monotype: ?ResolvedMonotype = null;
            if (callee_expr == .e_lookup_local) {
                const lookup = callee_expr.e_lookup_local;
                lookup_pattern = @intFromEnum(lookup.pattern_idx);
                callable_hit = thread.lookupCallableBinding(module_idx, lookup.pattern_idx) != null;
                if (thread.lookupValueBinding(module_idx, lookup.pattern_idx)) |source| {
                    value_hit = true;
                    value_source = @tagName(source);
                    if (source == .bound_expr) {
                        const expr_ref = source.bound_expr.expr_ref;
                        bound_source_context = @tagName(expr_ref.source_context);
                        bound_source_module = expr_ref.module_idx;
                        bound_source_expr = @intFromEnum(expr_ref.expr_idx);
                        bound_source_expr_tag = @tagName(driver.all_module_envs[expr_ref.module_idx].store.getExpr(expr_ref.expr_idx));
                        bound_source_expr_monotype = try cm.resolveExprMonotypeResolved(
                            driver,
                            result,
                            SemanticThread.trackedThread(expr_ref.source_context),
                            expr_ref.module_idx,
                            expr_ref.expr_idx,
                        );
                        if (result.getExprTemplateId(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx)) |template_id| {
                            bound_source_template = @intFromEnum(template_id);
                            bound_source_template_fn_monotype = try resolveTemplateFnMonotypeForThread(
                                driver,
                                result,
                                SemanticThread.trackedThread(expr_ref.source_context),
                                template_id,
                            );
                        }
                        bound_source_callable = getValueExprCallableValueForSourceContext(
                            driver,
                            visit_memo,
                            result,
                            expr_ref.source_context,
                            expr_ref.module_idx,
                            expr_ref.expr_idx,
                        );
                    }
                }
            }
            std.debug.panic(
                "Lambdasolved invariant violated: non-first-class callee expr {d} kind={s} in module {d} under source context {s} reached direct-call specialization without a registered callable template or explicit callable value; region={any} snippet=\"{s}\" lookup_pattern={d} callable_hit={} value_hit={} value_source={?s} bound_source_context={?s} bound_source_module={?d} bound_source_expr={?d} bound_source_expr_tag={?s} bound_source_template={?d} bound_source_expr_monotype={?any} bound_source_template_fn_monotype={?any} bound_source_callable={?any}",
                .{
                    @intFromEnum(callee_expr_idx),
                    @tagName(callee_expr),
                    module_idx,
                    @tagName(thread.requireSourceContext()),
                    callee_region,
                    module_source[snippet_start..snippet_end],
                    lookup_pattern,
                    callable_hit,
                    value_hit,
                    value_source,
                    bound_source_context,
                    bound_source_module,
                    bound_source_expr,
                    bound_source_expr_tag,
                    bound_source_template,
                    bound_source_expr_monotype,
                    bound_source_template_fn_monotype,
                    bound_source_callable,
                },
            );
        }
        unreachable;
    }

    const template_id = resolved_template_id orelse {
        switch (callee_expr) {
            .e_lookup_local, .e_lookup_external, .e_lookup_required => {
                if (desired_fn_monotype) |desired| {
                    if (desired.isNone()) {
                        return;
                    }
                } else if (std.debug.runtime_safety and thread.callableInst() == null) {
                    if (exprVisitStillPendingForThread(
                        visit_memo,
                        driver,
                        thread,
                        module_idx,
                        callee_expr_idx,
                    )) {
                        const origin = result.getExprOriginExpr(
                            thread.requireSourceContext(),
                            module_idx,
                            callee_expr_idx,
                        );
                        const lookup_resolution = result.getExprLookupResolution(
                            thread.requireSourceContext(),
                            module_idx,
                            callee_expr_idx,
                        );
                        std.debug.panic(
                            "Lambdasolved missing direct-callee template is still pending for expr={d} module={d} callee_expr={d} tag={s} origin={?any} lookup={?any} desired_fn_monotype={?any}",
                            .{
                                @intFromEnum(call_expr_idx),
                                module_idx,
                                @intFromEnum(callee_expr_idx),
                                @tagName(callee_expr),
                                origin,
                                lookup_resolution,
                                desired_fn_monotype,
                            },
                        );
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
        visit_memo,
        result,
        thread.requireSourceContext(),
        module_idx,
        callee_expr_idx,
        template_id,
    );
    const callable_inst_id = try specializeDirectCallExactCallable(
        driver,
        visit_memo,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
        template_id,
        template_source_context,
    ) orelse {
        if ((desired_fn_monotype == null or desired_fn_monotype.?.isNone()) and std.debug.runtime_safety) {
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
        visit_memo,
        result,
        thread,
        module_idx,
        call_expr_idx,
        call_expr,
        callee_expr,
        callable_inst_id,
    );
}

fn realizeStructuredExprCallableSemantics(
    driver: anytype,
    visit_memo: *VisitMemo,
    result: anytype,
    thread: ?SemanticThread,
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
    const current_thread = if (thread) |active_thread|
        active_thread
    else
        try threadForSourceContext(driver, result, source_context);
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
                visit_memo,
                result,
                current_thread,
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
            if (getValueExprCallableValueForSourceContext(driver, visit_memo, result, source_context, module_idx, expr_idx)) |callable_value| {
                try setExprCallableValue(driver, result, source_context, module_idx, expr_idx, callable_value);
            }
            return;
        },
        .e_if => |if_expr| {
            for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                try includeExprCallableValue(driver, visit_memo, result, current_thread, source_context, module_idx, branch.body, visiting, &variant_builder);
            }
            try includeExprCallableValue(driver, visit_memo, result, current_thread, source_context, module_idx, if_expr.final_else, visiting, &variant_builder);
        },
        .e_match => |match_expr| {
            for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = module_env.store.getMatchBranch(branch_idx);
                try includeExprCallableValue(driver, visit_memo, result, current_thread, source_context, module_idx, branch.value, visiting, &variant_builder);
            }
        },
        .e_call => |call_expr| {
            if (readExprCallSite(result, source_context, module_idx, expr_idx)) |call_site| {
                switch (call_site) {
                    .low_level => return,
                    .direct, .indirect_call => {},
                }
            }
            if (directCalleeLowLevelOp(
                driver,
                result,
                SemanticThread.trackedThread(source_context),
                module_idx,
                call_expr.func,
            ) != null) return;

            const call_site = readExprCallSite(result, source_context, module_idx, expr_idx);
            const resolved_call_site = call_site orelse {
                const callee_expr_idx = call_expr.func;
                const callee_expr = module_env.store.getExpr(callee_expr_idx);
                const call_region = module_env.store.getExprRegion(expr_idx);
                const source = module_env.getSourceAll();
                const call_snippet_start = @min(call_region.start.offset, source.len);
                const call_snippet_end = @min(call_region.end.offset, source.len);
                const callee_value = getValueExprCallableValueForSourceContext(
                    driver,
                    visit_memo,
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
                const source_context_callable_inst = sourceContextCallableInstId(source_context);
                const callee_pattern_value = if (callee_pattern_idx) |pattern_idx|
                    if (source_context_callable_inst) |_| blk: {
                        break :blk resolveBoundBindingCallableValueForThread(
                            driver,
                            visit_memo,
                            result,
                            current_thread,
                            module_idx,
                            pattern_idx,
                        );
                    } else null
                else
                    null;
                const callable_param_spec_count: ?usize = if (source_context_callable_inst) |context_callable_inst|
                    result.getCallableParamSpecEntries(result.getCallableInst(context_callable_inst).callable_param_specs).len
                else
                    null;
                const current_callable_template = if (source_context_callable_inst) |context_callable_inst|
                    result.getCallableTemplate(result.getCallableInst(context_callable_inst).template).*
                else
                    null;
                const callee_pattern_monotype = if (callee_pattern_idx) |pattern_idx|
                    try cm.resolveTypeVarMonotypeResolved(
                        driver,
                        result,
                        current_thread,
                        module_idx,
                        ModuleEnv.varFrom(pattern_idx),
                    )
                else
                    null;
                const call_fn_monotype = try resolveDirectCallFnMonotype(
                    driver,
                    result,
                    current_thread,
                    module_idx,
                    expr_idx,
                    call_expr,
                );
                const callee_expr_monotype = try cm.requireFullyBoundExprMonotype(
                    driver,
                    result,
                    current_thread,
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
                        visit_memo,
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
                    "Lambdasolved invariant violated: callable-valued call expr {d} in module {d} had no explicit solved call-site in source context {s}; region={any} snippet=\"{s}\" call_fn_mono={?d}@{d} call_fn_kind={?s} callee_expr={d} callee_tag={s} callee_expr_mono={d}@{d} callee_expr_mono_kind={s} callee_value={?any} callee_template={?d} callee_source={?any} callee_pattern={?d} callee_pattern_value={?any} callee_pattern_monotype={?any} callee_pattern_mono_kind={?s} callee_pattern_is_arg={} context_callable_inst={?d} current_callable_fn_mono={?d}@{?d} current_callable_fn_mono_kind={?s} callable_param_specs={?d} current_callable_template_expr={?d} current_callable_binding={?d} current_callable_arg_count={?d} callee_source_tag={?s} callee_source_value={?any} callee_source_template={?d}",
                    .{
                        @intFromEnum(expr_idx),
                        module_idx,
                        @tagName(source_context),
                        call_region,
                        source[call_snippet_start..call_snippet_end],
                        if (call_fn_monotype) |monotype| @intFromEnum(monotype) else null,
                        module_idx,
                        if (call_fn_monotype) |monotype| @tagName(result.context_mono.monotype_store.getMonotype(monotype)) else null,
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

            for (result.getCallSiteVariants(resolved_call_site)) |callee_callable_inst_id| {
                try recordCallResultCallableValueFromCallee(
                    driver,
                    visit_memo,
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
            if (sourceContextsEqual(current_thread.requireSourceContext(), source_context)) {
                try materializeTemplateExprCallableValueForThread(
                    driver,
                    visit_memo,
                    result,
                    current_thread,
                    module_idx,
                    expr_idx,
                );
            }
            return;
        },
        else => {},
    }

    if (try variant_builder.finishValue(driver, result)) |callable_value| {
        switch (callable_value) {
            .packed_fn => |packed_fn| switch (expr) {
                .e_if => |if_expr| {
                    for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                        const branch = module_env.store.getIfBranch(branch_idx);
                        try recordPackedCallableShapeForExprIfCallable(
                            driver,
                            result,
                            source_context,
                            module_idx,
                            branch.body,
                            packed_fn,
                        );
                    }
                    try recordPackedCallableShapeForExprIfCallable(
                        driver,
                        result,
                        source_context,
                        module_idx,
                        if_expr.final_else,
                        packed_fn,
                    );
                },
                .e_match => |match_expr| {
                    for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                        const branch = module_env.store.getMatchBranch(branch_idx);
                        try recordPackedCallableShapeForExprIfCallable(
                            driver,
                            result,
                            source_context,
                            module_idx,
                            branch.value,
                            packed_fn,
                        );
                    }
                },
                else => {},
            },
            .direct => {},
        }
        try setExprCallableValue(
            driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_value,
        );
    }
}

fn realizeDispatchExprSemantics(
    driver: anytype,
    visit_memo: *VisitMemo,
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
            visit_memo,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(binop_expr.lhs),
            method_name,
        )) |callable_inst_id| {
            try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
                    visit_memo,
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                )) |callable_inst_id| {
                    try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
                visit_memo,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(dot_expr.receiver),
                dot_expr.field_name,
            )) |callable_inst_id| {
                try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
                        visit_memo,
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    )) |callable_inst_id| {
                        try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
            visit_memo,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            ModuleEnv.varFrom(alias_stmt.type_var_anno),
            dispatch_expr.method_name,
        )) |callable_inst_id| {
            try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
                visit_memo,
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                alias_monotype.idx,
                dispatch_expr.method_name,
            )) |callable_inst_id| {
                try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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

    const method_name = switch (expr) {
        .e_binop => |binop_expr| dispatchMethodIdentForExprBinop(module_env, binop_expr.op) orelse return,
        .e_unary_minus => module_env.idents.negate,
        .e_dot_access => |dot_expr| if (dot_expr.args != null) dot_expr.field_name else return,
        .e_type_var_dispatch => |dispatch_expr| dispatch_expr.method_name,
        else => return,
    };

    const resolved_target = if (associated_target) |target| target else switch (expr) {
        .e_binop => |binop_expr| blk: {
            break :blk try resolveBinopDispatchTarget(driver, result, thread, module_idx, expr_idx, binop_expr, method_name);
        },
        .e_unary_minus => blk: {
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
        },
        .e_dot_access => |dot_expr| blk: {
            if (dot_expr.args == null) return;
            if (dotCallUsesRuntimeReceiverExpr(module_env, dot_expr.receiver)) {
                _ = try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver);
                break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
            }
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
        },
        .e_type_var_dispatch => |_| blk: {
            break :blk try resolveDispatchTargetForExpr(driver, result, thread, module_idx, expr_idx, method_name);
        },
        else => return,
    };
    const target_def = DispatchSolved.resolveDispatchTargetToExternalDef(
        driver.all_module_envs,
        module_idx,
        resolved_target,
    );
    try recordDispatchExprTarget(
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
        if (try specializeDispatchExactCallable(driver, visit_memo, result, thread, module_idx, expr_idx, expr, template_id)) |exact_callable_inst| {
            break :blk exact_callable_inst;
        }

        const site = (try ensureRecordedExactDispatchSiteFromDemandedFnMonotype(
            driver,
            result,
            thread,
            module_idx,
            expr_idx,
            expr,
            method_name,
        )) orelse return;

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(driver.allocator);
        try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);

        var callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
        defer callable_param_specs.deinit(driver.allocator);
        const callable_param_specs_complete = try collectDirectCallCallableParamSpecs(
            driver,
            visit_memo,
            result,
            thread,
            thread.requireSourceContext(),
            module_idx,
            site.fn_monotype.idx,
            site.fn_monotype.module_idx,
            actual_args.items,
            &callable_param_specs,
        );
        if (!callable_param_specs_complete) return;
        break :blk try requireCallableInstWithCallableParamSpecs(
            driver,
            visit_memo,
            result,
            thread,
            thread.requireSourceContext(),
            template_id,
            site.fn_monotype.idx,
            site.fn_monotype.module_idx,
            callable_param_specs.items,
        );
    };
    try commitDirectDispatchExprSemantics(driver, visit_memo, result, thread, module_idx, expr_idx, expr, callable_inst_id);
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
        .str,
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
            .prim => |prim| blk: {
                if (!primitiveUsesIntrinsicToStr(prim)) break :blk false;
                try recordDispatchExprIntrinsic(
                    driver,
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    .to_str,
                );
                break :blk true;
            },
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

    const expr = module_env.store.getExpr(expr_idx);
    const fn_monotype = (try resolveDemandedDispatchFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        constraint.fn_var,
    )) orelse return false;

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
        .prim => |ret_prim| {
            if (ret_prim != .str) return false;
            try recordDispatchExprIntrinsic(
                driver,
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                .to_str,
            );
            return true;
        },
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
    try appendDispatchActualArgsFromExpr(driver, module_idx, .{ .e_type_var_dispatch = dispatch_expr }, &arg_exprs);
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

    try recordDispatchExprIntrinsic(
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
    visit_memo: *VisitMemo,
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
    return try specializeDispatchExactCallable(driver, visit_memo, result, thread, module_idx, expr_idx, expr, method_info.template_id);
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
    visit_memo: *VisitMemo,
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
    return try specializeDispatchExactCallable(driver, visit_memo, result, thread, module_idx, expr_idx, expr, method_info.template_id);
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
    if (DispatchSolved.exactDispatchSiteForExpr(driver, result, thread, module_idx, expr_idx, method_name)) |site| {
        return site;
    }
    if (receiver_monotype.isNone()) return null;
    const template = result.getCallableTemplate(method_info.template_id);
    const fn_monotype = (try resolveTemplateFnMonotypeFromExactActuals(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        template,
    )) orelse return null;
    const site: DispatchSolved.ExactDispatchSite = .{
        .method_name = method_name,
        .fn_var = method_info.type_var,
        .fn_monotype = fn_monotype,
    };
    try result.dispatch_solved.recordExactDispatchSite(
        driver.allocator,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        site,
    );
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
    const expr = module_env.store.getExpr(expr_idx);
    const site = (try ensureRecordedExactDispatchSiteFromDemandedFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        method_name,
    )) orelse {
        if (std.debug.runtime_safety) {
            const receiver_monotype: ?ResolvedMonotype = switch (expr) {
                .e_dot_access => |dot_expr| if (dot_expr.args != null)
                    try cm.resolveExprMonotypeResolved(driver, result, thread, module_idx, dot_expr.receiver)
                else
                    null,
                .e_type_var_dispatch => blk: {
                    var args = std.ArrayList(CIR.Expr.Idx).empty;
                    defer args.deinit(driver.allocator);
                    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &args);
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
            const receiver_pattern_callable_value: ?CallableValue = null;
            const receiver_pattern_monotype = if (receiver_lookup_pattern) |pattern_idx|
                try cm.resolveTypeVarMonotypeResolved(
                    driver,
                    result,
                    thread,
                    module_idx,
                    ModuleEnv.varFrom(pattern_idx),
                )
            else
                null;
            const receiver_pattern_source = if (receiver_lookup_pattern) |pattern_idx|
                if (thread.lookupValueBinding(module_idx, pattern_idx)) |source|
                    bindingSourceExprRef(source)
                else
                    null
            else
                null;
            const receiver_binding_source = if (receiver_lookup_pattern) |pattern_idx|
                thread.lookupValueBinding(module_idx, pattern_idx)
            else
                null;
            const source_context_callable_inst = sourceContextCallableInstId(thread.requireSourceContext());
            const current_callable_fn = if (source_context_callable_inst) |callable_inst_id|
                result.getCallableInst(callable_inst_id)
            else
                null;
            const current_callable_arg_patterns = if (current_callable_fn) |callable_inst|
                driver.all_module_envs[result.getCallableTemplate(callable_inst.template).module_idx].store.slicePatterns(
                    result.getCallableTemplate(callable_inst.template).arg_patterns,
                )
            else
                &.{};
            const current_callable_arg_monos = if (current_callable_fn) |callable_inst|
                switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
                    .func => |func| result.context_mono.monotype_store.getIdxSpan(func.args),
                    else => &.{},
                }
            else
                &.{};
            std.debug.panic(
                "Lambdasolved invariant violated: no exact cached dispatch site for expr={d} method='{s}' in ctx={s}; expr_tag={s} receiver={?d}@{?d} receiver_shape={?any} receiver_pattern={?d} receiver_pattern_value={?any} receiver_pattern_mono={?any} receiver_pattern_source={?any} receiver_binding_source={?any} current_callable_inst={?d} current_callable_fn={?d}@{?d} current_callable_fn_shape={?any} current_callable_template_expr={?d} current_callable_arg_patterns={any} current_callable_arg_monos={any}",
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
                    receiver_binding_source,
                    if (source_context_callable_inst) |callable_inst_id| @intFromEnum(callable_inst_id) else null,
                    if (current_callable_fn) |callable_inst| @intFromEnum(callable_inst.fn_monotype) else null,
                    if (current_callable_fn) |callable_inst| callable_inst.fn_monotype_module_idx else null,
                    if (current_callable_fn) |callable_inst| result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype) else null,
                    if (current_callable_fn) |callable_inst| @intFromEnum(result.getCallableTemplate(callable_inst.template).cir_expr) else null,
                    current_callable_arg_patterns,
                    current_callable_arg_monos,
                },
            );
        }
        unreachable;
    };

    const desired_func_monotype = (try resolveDemandedDispatchFnMonotype(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
        site.fn_var,
    )) orelse cm.resolvedMonotype(Monotype.Idx.none, module_idx);
    const resolved_target = try resolveDispatchTargetFromExactSite(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        expr,
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
    if (std.debug.runtime_safety) {
        _ = try DispatchSolved.lookupResolvedDispatchTemplate(driver, result, module_idx, resolved_target);
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
    const expr = driver.all_module_envs[module_idx].store.getExpr(expr_idx);
    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);

    if (actual_args.items.len != fn_mono.args.len) unreachable;

    for (actual_args.items, 0..) |arg_expr_idx, i| {
        const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
        if (result.context_mono.monotype_store.getMonotype(param_mono) != .func) {
            try cm.recordExprMonotypeForThread(driver, result, thread, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            try propagateDemandedValueMonotypeToValueExpr(
                driver,
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
    visit_memo: *VisitMemo,
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
        try recordDispatchExprTarget(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            target_def,
        );
    }
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    if (template.low_level_op) |low_level_op| {
        try recordExprCallSite(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            .{ .low_level = .{
                .op = low_level_op,
            } },
        );
    } else {
        try setExprDirectCallSite(
            driver,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            callable_inst_id,
        );
        try setExprDirectCallable(
            driver,
            result,
            thread.requireSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );
    }
    var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_args.deinit(driver.allocator);
    try appendDispatchActualArgsFromExpr(driver, module_idx, expr, &actual_args);
    try bindCurrentDispatchFromCallableInst(driver, result, thread, module_idx, expr_idx, callable_inst_id);
    try prepareCallableArgsForCallableInst(driver, visit_memo, result, thread, module_idx, actual_args.items, callable_inst_id);
    try realizeCallableArgSemanticsSlice(driver, visit_memo, result, thread, module_idx, actual_args.items);
}

const CallableVariantGroup = struct {
    variants: Lambdamono.CallableVariantSpan,
};

const PackedFnKey = struct {
    variant_group: Lambdamono.CallableVariantGroupId,
    module_idx: u32,
};

pub const SolvedExprSemantics = struct {
    callable: ?Lambdamono.ExprCallableSemantics = null,
    origin: Lambdamono.ValueOrigin = .self,
    call: ?CallSite = null,
    lookup: ?Lambdamono.LookupResolution = null,
    dispatch_intrinsic: ?DispatchIntrinsic = null,
};

pub const Result = struct {
    expr_semantics_by_key: std.AutoHashMapUnmanaged(ContextExprKey, SolvedExprSemantics),
    callable_insts: std.ArrayListUnmanaged(Lambdamono.CallableInst),
    callable_defs: std.ArrayListUnmanaged(Lambdamono.CallableDef),
    callable_param_spec_entries: std.ArrayListUnmanaged(Lambdamono.CallableParamSpecEntry),
    value_projection_entries: std.ArrayListUnmanaged(Lambdamono.CallableParamProjection),
    pattern_entries: std.ArrayListUnmanaged(CIR.Pattern.Idx),
    capture_binding_entries: std.ArrayListUnmanaged(ValueLexicalBinding),
    callable_inst_capture_bindings: std.ArrayListUnmanaged(ValueLexicalBindingSpan),
    capture_fields: std.ArrayListUnmanaged(Lambdamono.CaptureField),
    callable_inst_group_entries: std.ArrayListUnmanaged(CallableInstId),
    callable_variant_groups: std.ArrayListUnmanaged(CallableVariantGroup),
    direct_callable_variant_group_ids_by_callable_inst: std.AutoHashMapUnmanaged(CallableInstId, Lambdamono.CallableVariantGroupId),
    callable_variant_entries: std.ArrayListUnmanaged(CallableInstId),
    packed_fn_by_key: std.AutoHashMapUnmanaged(PackedFnKey, Lambdamono.PackedFn),

    pub fn init(allocator: std.mem.Allocator) !Result {
        _ = allocator;
        return .{
            .expr_semantics_by_key = .empty,
            .callable_insts = .empty,
            .callable_defs = .empty,
            .callable_param_spec_entries = .empty,
            .value_projection_entries = .empty,
            .pattern_entries = .empty,
            .capture_binding_entries = .empty,
            .callable_inst_capture_bindings = .empty,
            .capture_fields = .empty,
            .callable_inst_group_entries = .empty,
            .callable_variant_groups = .empty,
            .direct_callable_variant_group_ids_by_callable_inst = .empty,
            .callable_variant_entries = .empty,
            .packed_fn_by_key = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        self.expr_semantics_by_key.deinit(allocator);
        self.callable_insts.deinit(allocator);
        self.callable_defs.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.value_projection_entries.deinit(allocator);
        self.pattern_entries.deinit(allocator);
        self.capture_binding_entries.deinit(allocator);
        self.callable_inst_capture_bindings.deinit(allocator);
        self.capture_fields.deinit(allocator);
        self.callable_inst_group_entries.deinit(allocator);
        self.callable_variant_groups.deinit(allocator);
        self.direct_callable_variant_group_ids_by_callable_inst.deinit(allocator);
        self.callable_variant_entries.deinit(allocator);
        self.packed_fn_by_key.deinit(allocator);
    }

    pub fn getExprSemantics(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?SolvedExprSemantics {
        return self.expr_semantics_by_key.get(cm.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExprSemanticsPtr(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!*SolvedExprSemantics {
        const entry = try self.expr_semantics_by_key.getOrPut(allocator, cm.Result.contextExprKey(source_context, module_idx, expr_idx));
        if (!entry.found_existing) entry.value_ptr.* = .{};
        return entry.value_ptr;
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return switch (semantics.callable orelse return null) {
            .callable => |callable_value| callable_value,
            .intro => |intro| intro.callable_value,
        };
    }

    pub fn getExprIntroCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return switch (semantics.callable orelse return null) {
            .intro => |intro| intro.callable_inst,
            .callable => null,
        };
    }

    pub fn getExprOriginExpr(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprRef {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return switch (semantics.origin) {
            .self => null,
            .expr => |origin| origin,
        };
    }

    pub fn getExprCallSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.call;
    }

    pub fn getExprLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?Lambdamono.LookupResolution {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.lookup;
    }

    pub fn getExprDispatchIntrinsic(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchIntrinsic {
        const semantics = self.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.dispatch_intrinsic;
    }

    pub fn recordExprCallableValue(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_value: CallableValue,
    ) std.mem.Allocator.Error!void {
        const semantics = try self.getExprSemanticsPtr(allocator, source_context, module_idx, expr_idx);
        const owns_callable_semantics = self.getExprIntroCallableInst(source_context, module_idx, expr_idx) != null or self.getExprCallableValue(source_context, module_idx, expr_idx) != null;
        const next_callable: Lambdamono.ExprCallableSemantics = switch (callable_value) {
            .direct => |callable_inst_id| blk: {
                if (semantics.callable) |existing_callable| {
                    switch (existing_callable) {
                        .intro => |existing_intro| switch (existing_intro.callable_value) {
                            .packed_fn => break :blk .{ .intro = .{
                                .callable_value = existing_intro.callable_value,
                                .callable_inst = callable_inst_id,
                            } },
                            .direct => {},
                        },
                        .callable => {},
                    }
                }
                if (self.getExprIntroCallableInst(source_context, module_idx, expr_idx) != null) {
                    break :blk .{ .intro = .{ .callable_value = callable_value, .callable_inst = callable_inst_id } };
                }
                break :blk .{ .callable = callable_value };
            },
            .packed_fn => |packed_fn| blk: {
                if (owns_callable_semantics) {
                    switch (semantics.callable orelse break :blk .{ .callable = callable_value }) {
                        .callable => |existing_callable_value| switch (existing_callable_value) {
                            .direct => |callable_inst_id| break :blk .{ .intro = .{
                                .callable_value = .{ .packed_fn = packed_fn },
                                .callable_inst = callable_inst_id,
                            } },
                            .packed_fn => {},
                        },
                        .intro => |existing_intro| break :blk .{ .intro = .{
                            .callable_value = .{ .packed_fn = packed_fn },
                            .callable_inst = existing_intro.callable_inst,
                        } },
                    }
                }
                break :blk .{ .callable = callable_value };
            },
        };
        semantics.callable = next_callable;
    }

    pub fn recordExprOriginExpr(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr_ref: ExprRef,
    ) std.mem.Allocator.Error!void {
        const semantics = try self.getExprSemanticsPtr(allocator, source_context, module_idx, expr_idx);
        semantics.origin = .{ .expr = expr_ref };
    }

    pub fn recordExprCallSite(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        call_site: CallSite,
    ) std.mem.Allocator.Error!void {
        const semantics = try self.getExprSemanticsPtr(allocator, source_context, module_idx, expr_idx);
        semantics.call = call_site;
    }

    pub fn recordExprLookupResolution(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        lookup_resolution: Lambdamono.LookupResolution,
    ) std.mem.Allocator.Error!void {
        const semantics = try self.getExprSemanticsPtr(allocator, source_context, module_idx, expr_idx);
        semantics.lookup = lookup_resolution;
    }

    pub fn recordExprDispatchIntrinsic(
        self: *Result,
        allocator: std.mem.Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        intrinsic: DispatchIntrinsic,
    ) std.mem.Allocator.Error!void {
        const semantics = try self.getExprSemanticsPtr(allocator, source_context, module_idx, expr_idx);
        semantics.dispatch_intrinsic = intrinsic;
    }

    pub fn getCallableInst(self: *const Result, callable_inst_id: CallableInstId) *const Lambdamono.CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn getCallableInsts(self: *const Result) []const Lambdamono.CallableInst {
        return self.callable_insts.items;
    }

    pub fn getCallableDef(self: *const Result, callable_def_id: Lambdamono.CallableDefId) *const Lambdamono.CallableDef {
        return &self.callable_defs.items[@intFromEnum(callable_def_id)];
    }

    pub fn getCallableDefMut(self: *Result, callable_def_id: Lambdamono.CallableDefId) *Lambdamono.CallableDef {
        return &self.callable_defs.items[@intFromEnum(callable_def_id)];
    }

    pub fn recordCallableInstCaptureBindings(
        self: *Result,
        allocator: std.mem.Allocator,
        callable_inst_id: CallableInstId,
        capture_bindings: []const ValueLexicalBinding,
    ) std.mem.Allocator.Error!void {
        const span_ptr = &self.callable_inst_capture_bindings.items[@intFromEnum(callable_inst_id)];
        if (capture_bindings.len == 0) {
            if (span_ptr.isEmpty()) return;
            const existing = self.getCallableInstCaptureBindings(callable_inst_id);
            if (existing.len == 0) return;
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Lambdasolved invariant violated: callable inst {d} attempted to clear finalized capture bindings",
                    .{@intFromEnum(callable_inst_id)},
                );
            }
            unreachable;
        }
        if (!span_ptr.isEmpty()) {
            const existing = self.getCallableInstCaptureBindings(callable_inst_id);
            if (existing.len != capture_bindings.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdasolved invariant violated: callable inst {d} attempted to rewrite finalized capture binding count",
                        .{@intFromEnum(callable_inst_id)},
                    );
                }
                unreachable;
            }
            for (existing, capture_bindings) |lhs, rhs| {
                if (!std.meta.eql(lhs, rhs)) {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Lambdasolved invariant violated: callable inst {d} attempted to rewrite finalized capture bindings",
                            .{@intFromEnum(callable_inst_id)},
                        );
                    }
                    unreachable;
                }
            }
            return;
        }

        span_ptr.* = try self.internCaptureBindingSlice(allocator, capture_bindings);
    }

    pub fn getCallableInstCaptureBindings(
        self: *const Result,
        callable_inst_id: CallableInstId,
    ) []const ValueLexicalBinding {
        const span = self.callable_inst_capture_bindings.items[@intFromEnum(callable_inst_id)];
        if (span.len == 0) return &.{};
        return self.capture_binding_entries.items[span.start..][0..span.len];
    }

    fn internCaptureBindingSlice(
        self: *Result,
        allocator: std.mem.Allocator,
        capture_bindings: []const ValueLexicalBinding,
    ) std.mem.Allocator.Error!ValueLexicalBindingSpan {
        if (capture_bindings.len == 0) return .empty();

        if (self.captureBindingSliceSpan(capture_bindings)) |existing_span| {
            return existing_span;
        }

        const start: u32 = @intCast(self.capture_binding_entries.items.len);
        try self.capture_binding_entries.appendSlice(allocator, capture_bindings);
        return .{
            .start = start,
            .len = @intCast(capture_bindings.len),
        };
    }

    fn captureBindingSliceSpan(
        self: *const Result,
        capture_bindings: []const ValueLexicalBinding,
    ) ?ValueLexicalBindingSpan {
        if (capture_bindings.len == 0 or self.capture_binding_entries.items.len == 0) return null;

        const base_ptr = @intFromPtr(self.capture_binding_entries.items.ptr);
        const slice_ptr = @intFromPtr(capture_bindings.ptr);
        const elem_size = @sizeOf(ValueLexicalBinding);
        const byte_len = self.capture_binding_entries.items.len * elem_size;

        if (slice_ptr < base_ptr or slice_ptr >= base_ptr + byte_len) return null;
        const byte_offset = slice_ptr - base_ptr;
        if (byte_offset % elem_size != 0) return null;

        const start = byte_offset / elem_size;
        if (start + capture_bindings.len > self.capture_binding_entries.items.len) return null;
        return .{
            .start = @intCast(start),
            .len = @intCast(capture_bindings.len),
        };
    }

    pub fn appendCaptureFields(
        self: *Result,
        allocator: std.mem.Allocator,
        capture_fields: []const Lambdamono.CaptureField,
    ) std.mem.Allocator.Error!Lambdamono.CaptureFieldSpan {
        if (capture_fields.len == 0) return .empty();
        const start: u32 = @intCast(self.capture_fields.items.len);
        try self.capture_fields.appendSlice(allocator, capture_fields);
        return .{ .start = start, .len = @intCast(capture_fields.len) };
    }

    pub fn getCaptureFields(self: *const Result, span: Lambdamono.CaptureFieldSpan) []const Lambdamono.CaptureField {
        if (span.len == 0) return &.{};
        return self.capture_fields.items[span.start..][0..span.len];
    }

    pub fn recordCallableInstRecursiveGroup(
        self: *Result,
        allocator: std.mem.Allocator,
        callable_inst_ids: []const CallableInstId,
    ) std.mem.Allocator.Error!void {
        if (callable_inst_ids.len == 0) return;
        const start: u32 = @intCast(self.callable_inst_group_entries.items.len);
        try self.callable_inst_group_entries.appendSlice(allocator, callable_inst_ids);
        const span: Lambdamono.CallableInstIdSpan = .{
            .start = start,
            .len = @intCast(callable_inst_ids.len),
        };
        for (callable_inst_ids) |callable_inst_id| {
            const callable_inst = &self.callable_insts.items[@intFromEnum(callable_inst_id)];
            if (!callable_inst.recursive_group.isEmpty()) {
                const existing = self.getCallableInstGroupEntries(callable_inst.recursive_group);
                if (existing.len != callable_inst_ids.len) {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Lambdasolved invariant violated: callable inst {d} attempted to rewrite recursive group size",
                            .{@intFromEnum(callable_inst_id)},
                        );
                    }
                    unreachable;
                }
                for (existing, callable_inst_ids) |lhs, rhs| {
                    if (lhs != rhs) {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "Lambdasolved invariant violated: callable inst {d} attempted to rewrite recursive group membership",
                                .{@intFromEnum(callable_inst_id)},
                            );
                        }
                        unreachable;
                    }
                }
                continue;
            }
            callable_inst.recursive_group = span;
        }
    }

    pub fn getCallableInstGroupEntries(
        self: *const Result,
        span: Lambdamono.CallableInstIdSpan,
    ) []const CallableInstId {
        if (span.len == 0) return &.{};
        return self.callable_inst_group_entries.items[span.start..][0..span.len];
    }

    pub fn appendPatternEntries(
        self: *Result,
        allocator: std.mem.Allocator,
        pattern_ids: []const CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!Lambdamono.PatternIdSpan {
        const start: u32 = @intCast(self.pattern_entries.items.len);
        try self.pattern_entries.appendSlice(allocator, pattern_ids);
        return .{ .start = start, .len = @intCast(pattern_ids.len) };
    }

    pub fn getPatternIds(self: *const Result, span: Lambdamono.PatternIdSpan) []const CIR.Pattern.Idx {
        if (span.len == 0) return &.{};
        return self.pattern_entries.items[span.start..][0..span.len];
    }

    pub fn addCallableParamSpecEntries(
        self: *Result,
        allocator: std.mem.Allocator,
        entries: []const Lambdamono.CallableParamSpecEntry,
    ) std.mem.Allocator.Error!Lambdamono.CallableParamSpecSpan {
        if (entries.len == 0) return .empty();
        const start: u32 = @intCast(self.callable_param_spec_entries.items.len);
        try self.callable_param_spec_entries.appendSlice(allocator, entries);
        return .{ .start = start, .len = @intCast(entries.len) };
    }

    pub fn getCallableParamSpecEntries(
        self: *const Result,
        span: Lambdamono.CallableParamSpecSpan,
    ) []const Lambdamono.CallableParamSpecEntry {
        if (span.len == 0) return &.{};
        return self.callable_param_spec_entries.items[span.start..][0..span.len];
    }

    pub fn addValueProjectionEntries(
        self: *Result,
        allocator: std.mem.Allocator,
        entries: []const Lambdamono.CallableParamProjection,
    ) std.mem.Allocator.Error!Lambdamono.CallableParamProjectionSpan {
        if (entries.len == 0) return .empty();
        const start: u32 = @intCast(self.value_projection_entries.items.len);
        try self.value_projection_entries.appendSlice(allocator, entries);
        return .{ .start = start, .len = @intCast(entries.len) };
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Result,
        span: Lambdamono.CallableParamProjectionSpan,
    ) []const Lambdamono.CallableParamProjection {
        if (span.len == 0) return &.{};
        return self.value_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getValueProjectionEntries(
        self: *const Result,
        span: ValueProjection.ProjectionSpan,
    ) []const ValueProjection.Projection {
        if (span.len == 0) return &.{};
        return self.value_projection_entries.items[span.start..][0..span.len];
    }

    pub fn internCallableVariantGroup(
        self: *Result,
        allocator: std.mem.Allocator,
        variants: []const CallableInstId,
    ) std.mem.Allocator.Error!Lambdamono.CallableVariantGroupId {
        for (self.callable_variant_groups.items, 0..) |_, idx| {
            const existing_variants = self.getCallableVariantGroupVariants(@enumFromInt(idx));
            if (existing_variants.len != variants.len) continue;
            var matches = true;
            for (existing_variants, variants) |lhs, rhs| {
                if (lhs != rhs) {
                    matches = false;
                    break;
                }
            }
            if (matches) return @enumFromInt(idx);
        }

        const span: Lambdamono.CallableVariantSpan = if (variants.len == 0)
            .empty()
        else blk: {
            const start: u32 = @intCast(self.callable_variant_entries.items.len);
            try self.callable_variant_entries.appendSlice(allocator, variants);
            break :blk .{ .start = start, .len = @intCast(variants.len) };
        };

        const variant_group_id: Lambdamono.CallableVariantGroupId = @enumFromInt(self.callable_variant_groups.items.len);
        try self.callable_variant_groups.append(allocator, .{ .variants = span });
        return variant_group_id;
    }

    pub fn ensureDirectCallableVariantGroup(
        self: *Result,
        allocator: std.mem.Allocator,
        callable_inst_id: CallableInstId,
    ) std.mem.Allocator.Error!void {
        if (self.direct_callable_variant_group_ids_by_callable_inst.contains(callable_inst_id)) return;
        const variant_group_id = try self.internCallableVariantGroup(allocator, &.{callable_inst_id});
        try self.direct_callable_variant_group_ids_by_callable_inst.put(allocator, callable_inst_id, variant_group_id);
    }

    pub fn getCallableVariantGroupVariants(
        self: *const Result,
        variant_group_id: Lambdamono.CallableVariantGroupId,
    ) []const CallableInstId {
        const group = self.callable_variant_groups.items[@intFromEnum(variant_group_id)];
        if (group.variants.len == 0) return &.{};
        return self.callable_variant_entries.items[group.variants.start..][0..group.variants.len];
    }

    pub fn getDirectCallableVariants(self: *const Result, callable_inst_id: CallableInstId) []const CallableInstId {
        const variant_group_id = self.direct_callable_variant_group_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getCallableVariantGroupVariants(variant_group_id);
    }

    pub fn getPackedFnVariants(self: *const Result, packed_fn: Lambdamono.PackedFn) []const CallableInstId {
        return self.getCallableVariantGroupVariants(packed_fn.variant_group);
    }

    pub fn getIndirectCallVariants(self: *const Result, indirect_call: Lambdamono.IndirectCall) []const CallableInstId {
        return self.getCallableVariantGroupVariants(indirect_call.packed_fn.variant_group);
    }

    pub fn getCallableValueVariants(self: *const Result, callable_value: CallableValue) []const CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| self.getDirectCallableVariants(callable_inst_id),
            .packed_fn => |packed_fn| self.getPackedFnVariants(packed_fn),
        };
    }

    pub fn getCallSiteVariants(self: *const Result, call_site: CallSite) []const CallableInstId {
        return switch (call_site) {
            .direct => |callable_inst_id| self.getDirectCallableVariants(callable_inst_id),
            .indirect_call => |indirect_call| self.getIndirectCallVariants(indirect_call),
            .low_level => &.{},
        };
    }

    pub fn getCallableInstRuntimeMonotype(
        self: *const Result,
        unit_monotype: Monotype.Idx,
        callable_inst_id: CallableInstId,
    ) ResolvedMonotype {
        const callable_inst = self.getCallableInst(callable_inst_id);
        return switch (callable_inst.runtime_value) {
            .direct_lambda => .{ .idx = unit_monotype, .module_idx = callable_inst.fn_monotype_module_idx },
            .closure => |closure| closure.capture_tuple_monotype,
        };
    }

    fn packedCallableTagName(
        allocator: std.mem.Allocator,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
        callable_inst_id: CallableInstId,
    ) std.mem.Allocator.Error!Monotype.Name {
        var name_buf: [32]u8 = undefined;
        const name_text = std.fmt.bufPrint(&name_buf, "__roc_fn_{d:0>10}", .{@intFromEnum(callable_inst_id)}) catch unreachable;
        const module_env = all_module_envs[module_idx];
        const ident = module_env.common.findIdent(name_text) orelse
            try module_env.common.insertIdent(allocator, Ident.for_text(name_text));
        return .{ .module_idx = module_idx, .ident = ident };
    }

    fn requireUniformPackedCallableFnMonotype(
        self: *const Result,
        allocator: std.mem.Allocator,
        all_module_envs: []const *ModuleEnv,
        context_mono: *const cm.Result,
        callable_inst_ids: []const CallableInstId,
    ) std.mem.Allocator.Error!ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        const first_callable = self.getCallableInst(callable_inst_ids[0]);
        const first_resolved: ResolvedMonotype = .{
            .idx = first_callable.fn_monotype,
            .module_idx = first_callable.fn_monotype_module_idx,
        };
        for (callable_inst_ids[1..]) |callable_inst_id| {
            const callable_inst = self.getCallableInst(callable_inst_id);
            if (!try context_mono.monotypesStructurallyEqualAcrossModules(
                allocator,
                all_module_envs,
                first_resolved.idx,
                first_resolved.module_idx,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
            )) {
                std.debug.panic(
                    "Lambdasolved invariant violated: callable variant set contained mismatched fn monotypes between callable insts {d} and {d}",
                    .{ @intFromEnum(callable_inst_ids[0]), @intFromEnum(callable_inst_id) },
                );
            }
        }
        return first_resolved;
    }

    fn buildPackedFnRuntimeMonotype(
        self: *const Result,
        allocator: std.mem.Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *cm.Result,
        callable_inst_ids: []const CallableInstId,
    ) std.mem.Allocator.Error!ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        var tags = std.ArrayList(Monotype.Tag).empty;
        defer tags.deinit(allocator);

        for (callable_inst_ids) |callable_inst_id| {
            const payload_mono = self.getCallableInstRuntimeMonotype(context_mono.monotype_store.unit_idx, callable_inst_id);
            const payloads = if (payload_mono.isNone() or payload_mono.idx == context_mono.monotype_store.unit_idx)
                Monotype.Span.empty()
            else
                try context_mono.monotype_store.addIdxSpan(allocator, &.{payload_mono.idx});
            try tags.append(allocator, .{
                .name = try packedCallableTagName(allocator, all_module_envs, current_module_idx, callable_inst_id),
                .payloads = payloads,
            });
        }

        std.mem.sortUnstable(Monotype.Tag, tags.items, all_module_envs, Monotype.Tag.sortByNameAsc);
        const tag_span = try context_mono.monotype_store.addTags(allocator, tags.items);
        return .{
            .idx = try context_mono.monotype_store.addMonotype(allocator, .{
                .tag_union = .{ .tags = tag_span },
            }),
            .module_idx = current_module_idx,
        };
    }

    pub fn makePackedFnForCallableInsts(
        self: *Result,
        allocator: std.mem.Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *cm.Result,
        callable_inst_ids: []const CallableInstId,
    ) std.mem.Allocator.Error!Lambdamono.PackedFn {
        const variant_group = try self.internCallableVariantGroup(allocator, callable_inst_ids);
        const packed_fn_key: PackedFnKey = .{
            .variant_group = variant_group,
            .module_idx = current_module_idx,
        };
        if (self.packed_fn_by_key.get(packed_fn_key)) |packed_fn| {
            return packed_fn;
        }
        const fn_monotype = try self.requireUniformPackedCallableFnMonotype(
            allocator,
            all_module_envs,
            context_mono,
            callable_inst_ids,
        );
        const runtime_monotype = try self.buildPackedFnRuntimeMonotype(
            allocator,
            all_module_envs,
            current_module_idx,
            context_mono,
            callable_inst_ids,
        );
        const packed_fn: Lambdamono.PackedFn = .{
            .variant_group = variant_group,
            .fn_monotype = fn_monotype,
            .runtime_monotype = runtime_monotype,
        };
        try self.packed_fn_by_key.put(allocator, packed_fn_key, packed_fn);
        return packed_fn;
    }

    pub fn makeIndirectCallForCallableInsts(
        self: *Result,
        allocator: std.mem.Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *cm.Result,
        callable_inst_ids: []const CallableInstId,
    ) std.mem.Allocator.Error!Lambdamono.IndirectCall {
        return .{
            .packed_fn = try self.makePackedFnForCallableInsts(
                allocator,
                all_module_envs,
                current_module_idx,
                context_mono,
                callable_inst_ids,
            ),
        };
    }
};
