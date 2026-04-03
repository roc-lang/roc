//! Staged CoreCIR callable pipeline.
//!
//! This module coordinates ContextMono, Lambdasolved, and Lambdamono for
//! reachable roots before MIR lowering begins. `Lower` must consume the result
//! of this stage; it must not specialize exact callables itself.

const std = @import("std");
const build_options = @import("build_options");
const can = @import("can");
const types = @import("types");

const Monotype = @import("Monotype.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");
const ContextMono = @import("ContextMono.zig");
const DispatchSolved = @import("DispatchSolved.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Lambdamono = @import("Lambdamono.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

/// Identifies a semantic callable template before executable specialization.
pub const CallableTemplateId = TemplateCatalog.CallableTemplateId;

/// Identifies a canonical type-variable substitution for a callable template.
pub const TypeSubstId = ContextMono.TypeSubstId;
pub const ContextId = ContextMono.ContextId;

/// Identifies one monomorphic callable instantiation.
pub const CallableInstId = Lambdamono.CallableInstId;

const BoundTypeVarKey = ContextMono.BoundTypeVarKey;

/// A monotype plus the module whose ident space that monotype currently uses.
pub const ResolvedMonotype = ContextMono.ResolvedMonotype;
pub const SourceContext = ContextMono.SourceContext;
pub const RootExprContext = ContextMono.RootExprContext;
pub const ProvenanceExprContext = ContextMono.ProvenanceExprContext;
pub const TemplateExprContext = ContextMono.TemplateExprContext;
/// One concrete type-variable assignment inside a callable instantiation substitution.
pub const TypeSubstEntry = ContextMono.TypeSubstEntry;

/// A packed slice of substitution entries stored in the staged pipeline result.
pub const TypeSubstSpan = ContextMono.TypeSubstSpan;

/// Describes the original callable form that produced a callable template.
pub const CallableTemplateKind = TemplateCatalog.CallableTemplateKind;

/// A semantic callable body that can later be instantiated monomorphically.
pub const CallableTemplate = TemplateCatalog.CallableTemplate;
pub const ExternalDefSource = TemplateCatalog.ExternalDefSource;

/// Records a block-local polymorphic callable that is materialized on demand.

/// The canonical substitution assigned to one callable instantiation.
pub const TypeSubst = ContextMono.TypeSubst;

/// One concrete instantiation of a semantic callable template.
pub const CallableInst = Lambdamono.CallableInst;
pub const RuntimeValue = Lambdamono.RuntimeValue;

const CallableParamProjection = Lambdamono.CallableParamProjection;
const CallableParamProjectionSpan = Lambdamono.CallableParamProjectionSpan;
const CallableParamSpecEntry = Lambdamono.CallableParamSpecEntry;
const CallableParamSpecSpan = Lambdamono.CallableParamSpecSpan;
pub const ValueProjection = CallableParamProjection;
pub const ValueProjectionSpan = CallableParamProjectionSpan;

pub const CaptureValueSource = Lambdamono.CaptureValueSource;
pub const CaptureStorage = Lambdamono.CaptureStorage;
pub const CaptureField = Lambdamono.CaptureField;
pub const CaptureFieldSpan = Lambdamono.CaptureFieldSpan;
pub const CallableDef = Lambdamono.CallableDef;
pub const CallableDefId = Lambdamono.CallableDefId;

const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[pipeline] " ++ fmt ++ "\n", args);
        }
    }
};
pub const PackedFn = Lambdamono.PackedFn;
pub const IndirectCall = Lambdamono.IndirectCall;
pub const CallableValue = Lambdamono.CallableValue;
pub const CallSite = Lambdamono.CallSite;
pub const LookupResolution = Lambdamono.LookupResolution;
pub const ExprId = Lambdamono.ExprId;
pub const ExprRef = Lambdamono.ExprRef;

/// One statically resolved dispatch target definition.
pub const DispatchExprTarget = DispatchSolved.DispatchExprTarget;
pub const ExactDispatchSite = DispatchSolved.ExactDispatchSite;
pub const DispatchIntrinsic = Lambdamono.DispatchIntrinsic;
pub const DispatchSemantics = Lambdamono.DispatchSemantics;

const ContextExprKey = ContextMono.ContextExprKey;
const ContextTypeVarKey = ContextMono.ContextTypeVarKey;

fn callableValueMonotype(result: *const Result, callable_value: CallableValue) ResolvedMonotype {
    return switch (callable_value) {
        .direct => |callable_inst_id| blk: {
            const callable_inst = result.getCallableInst(callable_inst_id);
            break :blk .{
                .idx = callable_inst.fn_monotype,
                .module_idx = callable_inst.fn_monotype_module_idx,
            };
        },
        .packed_fn => |packed_fn| packed_fn.fn_monotype,
    };
}

const exprTemplateSource = TemplateCatalog.exprTemplateSource;
const localPatternTemplateSource = TemplateCatalog.localPatternTemplateSource;
const externalDefTemplateSource = TemplateCatalog.externalDefTemplateSource;

const ResolvedDispatchTarget = DispatchSolved.ResolvedDispatchTarget;

const TypeScopeContext = struct {
    module_idx: u32,
    caller_module_idx: u32,
    scope: *const types.TypeScope,
};

const ContextExprVisitKey = ContextExprKey;
const CallResultCallableInstKey = Lambdasolved.CallResultCallableInstKey;
const calleeUsesFirstClassCallableValuePath = Lambdamono.calleeUsesFirstClassCallableValuePath;

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

fn collectModuleRootExprs(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    module_idx: u32,
) Allocator.Error![]CIR.Expr.Idx {
    const module_env = all_module_envs[module_idx];
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    const root_exprs = try allocator.alloc(CIR.Expr.Idx, defs.len);
    errdefer allocator.free(root_exprs);

    for (defs, 0..) |def_idx, i| {
        root_exprs[i] = module_env.store.getDef(def_idx).expr;
    }

    return root_exprs;
}

/// Output of the staged CoreCIR/context-mono/lambda-specialization pipeline.
pub const Result = struct {
    template_catalog: TemplateCatalog.Result,
    context_mono: ContextMono.Result,
    dispatch_solved: DispatchSolved.Result,
    lambdasolved: Lambdasolved.Result,
    lambdamono: Lambdamono.Program,

    pub fn init(allocator: Allocator, all_module_envs: []const *ModuleEnv) !Result {
        return .{
            .template_catalog = try TemplateCatalog.run(allocator, all_module_envs),
            .context_mono = try ContextMono.Result.init(allocator),
            .dispatch_solved = DispatchSolved.Result.init(),
            .lambdasolved = try Lambdasolved.Result.init(allocator),
            .lambdamono = Lambdamono.Program.init(),
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.lambdamono.deinit(allocator);
        self.dispatch_solved.deinit(allocator);
        self.context_mono.deinit(allocator);
        self.lambdasolved.deinit(allocator);
        self.template_catalog.deinit(allocator);
    }

    pub fn callSiteKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
        return (@as(u64, module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    }

    pub fn contextExprKey(
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
    }

    pub fn getExprSourceExprById(self: *const Result, expr_id: Lambdamono.ExprId) CIR.Expr.Idx {
        return self.lambdamono.getExpr(expr_id).common().source_expr;
    }

    pub fn getExprChildExprsById(self: *const Result, expr_id: Lambdamono.ExprId) Lambdamono.ExprIdSpan {
        return self.lambdamono.getExpr(expr_id).common().child_exprs;
    }

    pub fn getExprCallSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        return self.lambdasolved.getExprCallSite(source_context, module_idx, expr_idx);
    }

    pub fn getExprMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.context_mono.getExprMonotype(source_context, module_idx, expr_idx);
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        return self.lambdasolved.getExprCallableValue(source_context, module_idx, expr_idx);
    }

    pub fn getExprIntroCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        return self.lambdasolved.getExprIntroCallableInst(source_context, module_idx, expr_idx);
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return self.dispatch_solved.getDispatchExprTarget(source_context, module_idx, expr_idx);
    }

    pub fn getDispatchExprIntrinsic(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchIntrinsic {
        return self.lambdasolved.getExprDispatchIntrinsic(source_context, module_idx, expr_idx);
    }

    pub fn getLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?Lambdamono.LookupResolution {
        return self.lambdasolved.getExprLookupResolution(source_context, module_idx, expr_idx);
    }

    pub fn getProgramExprForRef(self: *const Result, expr_ref: ExprRef) *const Lambdamono.Expr {
        if (!expr_ref.projections.isEmpty()) {
            std.debug.panic(
                "Pipeline invariant violated: projected expr ref cannot be treated as a base program expr (ctx={s} module={d} expr={d}, projections={d})",
                .{
                    @tagName(expr_ref.source_context),
                    expr_ref.module_idx,
                    @intFromEnum(expr_ref.expr_idx),
                    expr_ref.projections.len,
                },
            );
        }
        const expr_id = self.lambdamono.getExprId(
            expr_ref.source_context,
            expr_ref.module_idx,
            expr_ref.expr_idx,
        ) orelse std.debug.panic(
            "Pipeline invariant violated: finalized lambdamono program missing expr for source context {s} module={d} expr={d}",
            .{
                @tagName(expr_ref.source_context),
                expr_ref.module_idx,
                @intFromEnum(expr_ref.expr_idx),
            },
        );
        return self.lambdamono.getExpr(expr_id);
    }

    pub fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return self.template_catalog.getCallableTemplate(callable_template_id);
    }

    pub fn getCallableInst(self: *const Result, callable_inst_id: CallableInstId) *const CallableInst {
        return self.lambdasolved.getCallableInst(callable_inst_id);
    }

    pub fn getCallableDef(self: *const Result, callable_def_id: CallableDefId) *const CallableDef {
        return self.lambdasolved.getCallableDef(callable_def_id);
    }

    pub fn getCallableDefForInst(self: *const Result, callable_inst_id: CallableInstId) *const CallableDef {
        return self.getCallableDef(self.getCallableInst(callable_inst_id).callable_def);
    }

    pub fn getCallableValueSourceMonotype(
        self: *const Result,
        callable_value: CallableValue,
    ) ResolvedMonotype {
        return callableValueMonotype(self, callable_value);
    }

    pub fn getCallableValueRuntimeMonotype(
        self: *const Result,
        callable_value: CallableValue,
    ) ResolvedMonotype {
        return switch (callable_value) {
            .direct => |callable_inst_id| self.getCallableInstRuntimeMonotype(callable_inst_id),
            .packed_fn => |packed_fn| packed_fn.runtime_monotype,
        };
    }

    pub fn getCallableInstRuntimeMonotype(
        self: *const Result,
        callable_inst_id: CallableInstId,
    ) ResolvedMonotype {
        return self.lambdasolved.getCallableInstRuntimeMonotype(
            self.context_mono.monotype_store.unit_idx,
            callable_inst_id,
        );
    }

    pub fn getPackedFnVariants(self: *const Result, packed_fn: PackedFn) []const CallableInstId {
        return self.lambdasolved.getPackedFnVariants(packed_fn);
    }

    pub fn getIndirectCallVariants(self: *const Result, indirect_call: IndirectCall) []const CallableInstId {
        return self.lambdasolved.getIndirectCallVariants(indirect_call);
    }

    pub fn getPackedFnTagName(
        _: *const Result,
        all_module_envs: []const *const ModuleEnv,
        packed_fn: PackedFn,
        callable_inst_id: CallableInstId,
    ) Monotype.Name {
        const module_env = all_module_envs[packed_fn.runtime_monotype.module_idx];

        var name_buf: [32]u8 = undefined;
        const name_text = std.fmt.bufPrint(&name_buf, "__roc_fn_{d:0>10}", .{@intFromEnum(callable_inst_id)}) catch unreachable;
        const ident = module_env.common.findIdent(name_text) orelse std.debug.panic(
            "Pipeline invariant violated: packed fn {d} missing tag name for callable inst {d} in module {d}",
            .{ @intFromEnum(packed_fn.variant_group), @intFromEnum(callable_inst_id), packed_fn.runtime_monotype.module_idx },
        );
        return .{
            .module_idx = packed_fn.runtime_monotype.module_idx,
            .ident = ident,
        };
    }

    pub fn getCaptureFields(self: *const Result, span: CaptureFieldSpan) []const CaptureField {
        return self.lambdasolved.getCaptureFields(span);
    }

    pub fn getCallableParamSpecEntries(
        self: *const Result,
        span: CallableParamSpecSpan,
    ) []const CallableParamSpecEntry {
        return self.lambdasolved.getCallableParamSpecEntries(span);
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Result,
        span: CallableParamProjectionSpan,
    ) []const CallableParamProjection {
        return self.lambdasolved.getCallableParamProjectionEntries(span);
    }

    pub fn getValueProjectionEntries(
        self: *const Result,
        span: ValueProjectionSpan,
    ) []const CallableParamProjection {
        return self.lambdasolved.getValueProjectionEntries(span);
    }

    pub fn getPatternIds(
        self: *const Result,
        span: Lambdamono.PatternIdSpan,
    ) []const CIR.Pattern.Idx {
        return self.lambdasolved.getPatternIds(span);
    }

    pub fn getCallableValueVariants(self: *const Result, callable_value: CallableValue) []const CallableInstId {
        return self.lambdasolved.getCallableValueVariants(callable_value);
    }

    pub fn getCallSiteVariants(self: *const Result, call_site: CallSite) []const CallableInstId {
        return self.lambdasolved.getCallSiteVariants(call_site);
    }

    pub fn getTypeSubst(self: *const Result, subst_id: TypeSubstId) *const TypeSubst {
        return self.context_mono.getTypeSubst(subst_id);
    }

    pub fn getTypeSubstEntries(self: *const Result, span: TypeSubstSpan) []const TypeSubstEntry {
        return self.context_mono.getTypeSubstEntries(span);
    }

    fn getLocalCallableTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?CallableTemplateId {
        return self.template_catalog.getLocalCallableTemplate(module_idx, pattern_idx);
    }

    fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.template_catalog.getExternalCallableTemplate(module_idx, def_node_idx);
    }

    pub fn getExprOriginExpr(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprRef {
        return self.lambdasolved.getExprOriginExpr(source_context, module_idx, expr_idx);
    }

    pub fn getExprTemplateId(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableTemplateId {
        if (self.getExprIntroCallableInst(source_context, module_idx, expr_idx)) |callable_inst_id| {
            return self.getCallableInst(callable_inst_id).template;
        }
        if (self.getExprCallableValue(source_context, module_idx, expr_idx)) |callable_value| switch (callable_value) {
            .direct => |callable_inst_id| return self.getCallableInst(callable_inst_id).template,
            .packed_fn => {},
        };
        if (self.getExprLookupResolution(source_context, module_idx, expr_idx)) |lookup| switch (lookup) {
            .expr => |expr_ref| {
                if (self.getExprTemplateId(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx)) |template_id| {
                    return template_id;
                }
            },
            .def => |external_def| {
                return self.getExternalCallableTemplate(
                    external_def.module_idx,
                    @intCast(@intFromEnum(external_def.def_idx)),
                );
            },
        };
        if (self.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
            if (self.getExprTemplateId(origin.source_context, origin.module_idx, origin.expr_idx)) |template_id| {
                return template_id;
            }
        }
        if (self.template_catalog.getExprCallableTemplate(module_idx, expr_idx)) |template_id| {
            return template_id;
        }
        return null;
    }

    pub fn requireExprTemplateId(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) CallableTemplateId {
        return self.getExprTemplateId(source_context, module_idx, expr_idx) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline.Result invariant violated: expr {d} in module {d} under source context {s} had no registered callable template",
                    .{ @intFromEnum(expr_idx), module_idx, @tagName(source_context) },
                );
            }
            unreachable;
        };
    }

    pub fn getExprLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?LookupResolution {
        return self.lambdasolved.getExprLookupResolution(source_context, module_idx, expr_idx);
    }

    pub fn getExprDispatchTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return self.dispatch_solved.getDispatchExprTarget(source_context, module_idx, expr_idx);
    }

    pub fn getSourceContextTemplateId(
        self: *const Result,
        source_context: SourceContext,
    ) ?CallableTemplateId {
        return switch (source_context) {
            .callable_inst => |context_id| self.getCallableInst(@enumFromInt(@intFromEnum(context_id))).template,
            .template_expr => |template_ctx| self.getExprTemplateId(
                source_context,
                template_ctx.module_idx,
                template_ctx.expr_idx,
            ) orelse std.debug.panic(
                "Pipeline invariant violated: template source context module={d} expr={d} had no registered callable template",
                .{ template_ctx.module_idx, @intFromEnum(template_ctx.expr_idx) },
            ),
            .root_expr => |root_ctx| self.getExprTemplateId(source_context, root_ctx.module_idx, root_ctx.expr_idx),
            .provenance_expr => |provenance_ctx| self.getExprTemplateId(source_context, provenance_ctx.module_idx, provenance_ctx.expr_idx),
        };
    }

    pub fn getExprLowLevelOp(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CIR.Expr.LowLevel {
        const template_id = self.getExprTemplateId(source_context, module_idx, expr_idx) orelse return null;
        return self.getCallableTemplate(template_id).low_level_op;
    }
};

/// Pipelines callable templates into explicit callable instantiations.
pub const Pass = struct {
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,

    pub fn init(
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        app_module_idx: ?u32,
    ) Pass {
        return .{
            .allocator = allocator,
            .all_module_envs = all_module_envs,
            .current_module_idx = current_module_idx,
            .app_module_idx = app_module_idx,
        };
    }

    fn seedExplicitTypeScopeMonotypes(
        self: *Pass,
        result: *Result,
        type_scope_context: TypeScopeContext,
    ) Allocator.Error!void {
        const module_idx = type_scope_context.module_idx;
        const caller_module_idx = type_scope_context.caller_module_idx;
        const caller_types = &self.all_module_envs[caller_module_idx].types;

        for (type_scope_context.scope.scopes.items) |*scope| {
            var it = scope.iterator();
            while (it.next()) |entry| {
                const platform_var = entry.key_ptr.*;
                const caller_var = entry.value_ptr.*;
                const caller_mono = try ContextMono.monotypeFromTypeVarInStore(
                    self,
                    result,
                    caller_module_idx,
                    caller_types,
                    caller_var,
                );
                if (caller_mono.isNone()) continue;
                const normalized_mono = if (caller_module_idx == module_idx)
                    caller_mono
                else
                    try ContextMono.remapMonotypeBetweenModules(self, result,
                        caller_mono,
                        caller_module_idx,
                        module_idx,
                    );
                try ContextMono.recordTypeScopeMonotype(
                    self,
                    result,
                    module_idx,
                    platform_var,
                    normalized_mono,
                    module_idx,
                );
            }
        }
    }

    pub fn deinit(self: *Pass) void {
        _ = self;
    }
};

/// Pipeline one expression tree rooted in the given module.
pub fn runRootSourceExpr(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    var result = try Result.init(allocator, all_module_envs);
    trace.log("runRootSourceExpr start expr={d}", .{@intFromEnum(expr_idx)});
    try Lambdasolved.run(allocator, current_module_idx, &.{expr_idx}, &pass, &result);
    trace.log("assembleLambdamono start", .{});
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, &.{expr_idx});
    trace.log("assembleLambdamono done", .{});
    return result;
}

/// Pipeline one root source expression using an explicit type scope for cross-module substitutions.
pub fn runRootSourceExprWithTypeScope(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
    expr_idx: CIR.Expr.Idx,
    scope_module_idx: u32,
    explicit_type_scope: *const types.TypeScope,
    scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator, all_module_envs);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = scope_module_idx,
        .caller_module_idx = scope_caller_module_idx,
        .scope = explicit_type_scope,
    });
    trace.log("runRootSourceExprWithTypeScope start expr={d}", .{@intFromEnum(expr_idx)});
    try Lambdasolved.run(allocator, current_module_idx, &.{expr_idx}, &pass, &result);
    trace.log("assembleLambdamono start", .{});
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, &.{expr_idx});
    trace.log("assembleLambdamono done", .{});
    return result;
}

/// Pipeline an explicit set of root source expressions in the current module.
pub fn runRootSourceExprs(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    var result = try Result.init(allocator, all_module_envs);
    try Lambdasolved.run(allocator, current_module_idx, exprs, &pass, &result);
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, exprs);
    return result;
}

/// Pipeline explicit root source expressions using an explicit type scope for cross-module substitutions.
pub fn runRootSourceExprsWithTypeScope(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
    exprs: []const CIR.Expr.Idx,
    scope_module_idx: u32,
    explicit_type_scope: *const types.TypeScope,
    scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator, all_module_envs);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = scope_module_idx,
        .caller_module_idx = scope_caller_module_idx,
        .scope = explicit_type_scope,
    });
    try Lambdasolved.run(allocator, current_module_idx, exprs, &pass, &result);
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, exprs);
    return result;
}

/// Pipeline all callables rooted in the current module.
pub fn runModule(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    var result = try Result.init(allocator, all_module_envs);
    const root_exprs = try collectModuleRootExprs(allocator, all_module_envs, current_module_idx);
    defer allocator.free(root_exprs);
    try Lambdasolved.run(allocator, current_module_idx, root_exprs, &pass, &result);
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, root_exprs);
    return result;
}

/// Pipeline all callables rooted in the current module using an explicit type scope.
pub fn runModuleWithTypeScope(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    app_module_idx: ?u32,
    scope_module_idx: u32,
    explicit_type_scope: *const types.TypeScope,
    scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator, all_module_envs);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = scope_module_idx,
        .caller_module_idx = scope_caller_module_idx,
        .scope = explicit_type_scope,
    });
    const root_exprs = try collectModuleRootExprs(allocator, all_module_envs, current_module_idx);
    defer allocator.free(root_exprs);
    try Lambdasolved.run(allocator, current_module_idx, root_exprs, &pass, &result);
    try Lambdamono.run(allocator, all_module_envs, current_module_idx, &result, &pass, root_exprs);
    return result;
}
