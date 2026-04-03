//! Staged CoreCIR callable pipeline.
//!
//! This module coordinates ContextMono, Lambdasolved, and Lambdamono for
//! reachable roots before MIR lowering begins. `Lower` must consume the result
//! of this stage; it must not specialize exact callables itself.

const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Monotype = @import("Monotype.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");
const ContextMono = @import("ContextMono.zig");
const DispatchSolved = @import("DispatchSolved.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Lambdamono = @import("Lambdamono.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
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
const ContextPatternKey = ContextMono.ContextPatternKey;
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

fn callableInstHasRealizedRuntimeExpr(result: *const Result, callable_inst_id: CallableInstId) bool {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    return result.getExprCallableValue(
        callableInstSourceContext(callable_inst_id),
        template.module_idx,
        template.runtime_expr,
    ) != null;
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
const SemanticThread = Lambdasolved.SemanticThread;

const ContextExprVisitKey = ContextExprKey;
const CallResultCallableInstKey = Lambdasolved.CallResultCallableInstKey;
const calleeUsesFirstClassCallableValuePath = Lambdamono.calleeUsesFirstClassCallableValuePath;

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

/// Output of the staged CoreCIR/context-mono/lambda-specialization pipeline.
pub const Result = struct {
    template_catalog: TemplateCatalog.Result,
    context_mono: ContextMono.Result,
    dispatch_solved: DispatchSolved.Result,
    lambdasolved: Lambdasolved.Result,
    lambdamono: Lambdamono.Program,

    pub fn init(allocator: Allocator) !Result {
        return .{
            .template_catalog = TemplateCatalog.Result.init(),
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

    pub fn contextPatternKey(
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
    }

    fn getExprMonotypeById(self: *const Result, expr_id: Lambdamono.ExprId) ResolvedMonotype {
        return self.lambdamono.getExpr(expr_id).common().monotype;
    }

    fn getExprOriginById(self: *const Result, expr_id: Lambdamono.ExprId) ?ExprRef {
        return self.lambdamono.getExpr(expr_id).getOriginExpr();
    }

    fn getExprSourceExprById(self: *const Result, expr_id: Lambdamono.ExprId) CIR.Expr.Idx {
        return self.lambdamono.getExpr(expr_id).common().source_expr;
    }

    fn getExprChildExprsById(self: *const Result, expr_id: Lambdamono.ExprId) Lambdamono.ExprIdSpan {
        return self.lambdamono.getExpr(expr_id).common().child_exprs;
    }

    pub fn getExprCallSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getCall();
    }

    pub fn getExprMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        if (self.lambdamono.getExprId(source_context, module_idx, expr_idx)) |expr_id| {
            return self.getExprMonotypeById(expr_id);
        }
        return self.context_mono.getExprMonotype(source_context, module_idx, expr_idx);
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getCallableValue();
    }

    pub fn getExprIntroCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return if (self.lambdamono.getExpr(expr_id).getCallableIntro()) |intro|
            intro.callable_inst
        else
            null;
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getDispatchTarget();
    }

    pub fn getDispatchExprIntrinsic(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchIntrinsic {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getDispatchIntrinsic();
    }

    pub fn getLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?Lambdamono.LookupResolution {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getLookup();
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

    pub fn getContextPatternMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        if (self.getPatternCallableValue(source_context, module_idx, pattern_idx)) |callable_value| {
            return self.getCallableValueSourceMonotype(callable_value);
        }

        if (self.context_mono.getContextPatternMonotype(source_context, module_idx, pattern_idx)) |resolved| {
            return resolved;
        }

        const source = self.getContextPatternSourceExpr(source_context, module_idx, pattern_idx) orelse return null;
        if (!source.projections.isEmpty()) return null;
        if (self.getExprCallableValue(source.source_context, source.module_idx, source.expr_idx)) |callable_value| {
            return self.getCallableValueSourceMonotype(callable_value);
        }
        return self.getExprMonotype(source.source_context, source.module_idx, source.expr_idx);
    }

    fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return self.template_catalog.getCallableTemplate(callable_template_id);
    }

    pub fn getCallableInst(self: *const Result, callable_inst_id: CallableInstId) *const CallableInst {
        return self.lambdamono.getCallableInst(callable_inst_id);
    }

    pub fn getCallableDef(self: *const Result, callable_def_id: CallableDefId) *const CallableDef {
        return Lambdamono.getCallableDef(&self.lambdamono, callable_def_id);
    }

    pub fn getCallableDefForInst(self: *const Result, callable_inst_id: CallableInstId) *const CallableDef {
        return self.getCallableDef(self.getCallableInst(callable_inst_id).callable_def);
    }

    pub fn getPatternCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        return self.lambdamono.getPatternCallableValue(source_context, module_idx, pattern_idx);
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
        return self.lambdamono.getCallableInstRuntimeMonotype(
            self.context_mono.monotype_store.unit_idx,
            callable_inst_id,
        );
    }

    pub fn getPackedFnVariants(self: *const Result, packed_fn: PackedFn) []const CallableInstId {
        return self.lambdamono.getCallableVariantGroupVariants(packed_fn.variant_group);
    }

    pub fn getIndirectCallVariants(self: *const Result, indirect_call: IndirectCall) []const CallableInstId {
        return self.lambdamono.getCallableVariantGroupVariants(indirect_call.packed_fn.variant_group);
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
        return Lambdamono.getCaptureFields(&self.lambdamono, span);
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

    pub fn getContextPatternSourceExpr(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprRef {
        return self.lambdamono.getPatternOriginExpr(source_context, module_idx, pattern_idx);
    }

    pub fn getExprOriginExpr(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprRef {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.getExprOriginById(expr_id);
    }

    pub fn getExprTemplateId(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableTemplateId {
        if (self.lambdamono.getExprId(source_context, module_idx, expr_idx)) |expr_id| {
            const expr = self.lambdamono.getExpr(expr_id);
            if (expr.getCallableIntro()) |intro| {
                return self.getCallableInst(intro.callable_inst).template;
            }
            if (expr.getCallableValue()) |callable_value| switch (callable_value) {
                .direct => |callable_inst_id| return self.getCallableInst(callable_inst_id).template,
                .packed_fn => {},
            };
            if (expr.getLookupExpr()) |expr_ref| {
                if (self.getExprTemplateId(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx)) |template_id| {
                    return template_id;
                }
            }
            if (expr.getLookupDef()) |external_def| {
                return self.getExternalCallableTemplate(
                    external_def.module_idx,
                    @intCast(@intFromEnum(external_def.def_idx)),
                );
            }
            if (self.getExprOriginById(expr_id)) |origin| {
                if (self.getExprTemplateId(origin.source_context, origin.module_idx, origin.expr_idx)) |template_id| {
                    return template_id;
                }
            }
        }
        if (self.template_catalog.getExprCallableTemplate(module_idx, expr_idx)) |template_id| {
            return template_id;
        }
        return null;
    }

    pub fn getExprLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?LookupResolution {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getLookup();
    }

    pub fn getExprDispatchTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return self.lambdamono.getExpr(expr_id).getDispatchTarget();
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
                try result.context_mono.type_scope_monotypes.put(
                    self.allocator,
                    .{
                        .module_idx = module_idx,
                        .type_var = platform_var,
                    },
                    ContextMono.resolvedMonotype(normalized_mono, module_idx),
                );
            }
        }
    }

    fn callableInstSourceContext(context_callable_inst: CallableInstId) SourceContext {
        return .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) };
    }

    fn sourceContextHasCallableInst(source_context: SourceContext) bool {
        return switch (source_context) {
            .callable_inst => true,
            .root_expr, .provenance_expr, .template_expr => false,
        };
    }

    fn sourceContextCallableInst(source_context: SourceContext) ?CallableInstId {
        return switch (source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => null,
        };
    }

    fn requireSourceContextCallableInst(source_context: SourceContext) CallableInstId {
        return switch (source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => std.debug.panic(
                "Pipeline invariant violated: source context {s} did not carry a callable inst",
                .{@tagName(source_context)},
            ),
        };
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

    fn sameSourceContext(_: *Pass, lhs: SourceContext, rhs: SourceContext) bool {
        return sourceContextsEqual(lhs, rhs);
    }

    fn contextExprVisitKeyForSourceContext(
        _: *Pass,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprVisitKey {
        return contextExprVisitKey(source_context, module_idx, expr_idx);
    }

    fn calleeUsesFirstClassCallableValuePath(
        _: *Pass,
        expr: CIR.Expr,
    ) bool {
        return Lambdamono.calleeUsesFirstClassCallableValuePath(expr);
    }

    fn sourceContextCallableInstId(
        _: *Pass,
        source_context: SourceContext,
    ) ?CallableInstId {
        return sourceContextCallableInst(source_context);
    }

    fn findDefByPatternInModule(
        _: *Pass,
        module_env: *const ModuleEnv,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CIR.Def.Idx {
        return findDefByPattern(module_env, pattern_idx);
    }

    fn dispatchMethodIdentForExprBinop(
        _: *Pass,
        module_env: *const ModuleEnv,
        op: CIR.Expr.Binop.Op,
    ) ?Ident.Idx {
        return dispatchMethodIdentForBinop(module_env, op);
    }

    fn dotCallUsesRuntimeReceiverExpr(
        _: *Pass,
        module_env: *const ModuleEnv,
        receiver_expr_idx: CIR.Expr.Idx,
    ) bool {
        return dotCallUsesRuntimeReceiver(module_env, receiver_expr_idx);
    }

    fn initCallableVariantBuilder(_: *Pass) CallableVariantBuilder {
        return CallableVariantBuilder.init();
    }

    fn deinitCallableVariantBuilder(self: *Pass, builder: *CallableVariantBuilder) void {
        builder.deinit(self.allocator);
    }

    fn finishCallableVariantBuilderValue(
        self: *Pass,
        builder: *CallableVariantBuilder,
        result: *Result,
    ) Allocator.Error!?CallableValue {
        return builder.finishValue(self, result);
    }

    fn contextTypeVarKeyMatchesSourceContext(source_context: SourceContext, key: ContextTypeVarKey) bool {
        return switch (source_context) {
            .callable_inst => |context_id| key.source_context_kind == .callable_inst and
                key.source_context_module_idx == std.math.maxInt(u32) and
                key.source_context_raw == @intFromEnum(context_id),
            .root_expr => |root| key.source_context_kind == .root_expr and
                key.source_context_module_idx == root.module_idx and
                key.source_context_raw == @intFromEnum(root.expr_idx),
            .provenance_expr => |source| key.source_context_kind == .provenance_expr and
                key.source_context_module_idx == source.module_idx and
                key.source_context_raw == @intFromEnum(source.expr_idx),
            .template_expr => |template| key.source_context_kind == .template_expr and
                key.source_context_module_idx == template.module_idx and
                key.source_context_raw == @intFromEnum(template.expr_idx),
        };
    }

    fn activeCallableTemplateForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
    ) ?CallableTemplate {
        _ = self;
        const template_id = result.getSourceContextTemplateId(thread.requireSourceContext()) orelse return null;
        return result.getCallableTemplate(template_id).*;
    }

    fn resultExprKey(
        self: *const Pass,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return self.resultExprKeyForSourceContext(callableInstSourceContext(context_callable_inst), module_idx, expr_idx);
    }

    fn resultExprKeyForSourceContext(
        _: *const Pass,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return Result.contextExprKey(source_context, module_idx, expr_idx);
    }

    fn resultExprKeyForThread(
        self: *const Pass,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return self.resultExprKeyForSourceContext(thread.requireSourceContext(), module_idx, expr_idx);
    }

    fn resultPatternKey(
        self: *const Pass,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return self.resultPatternKeyForSourceContext(callableInstSourceContext(context_callable_inst), module_idx, pattern_idx);
    }

    fn resultPatternKeyForSourceContext(
        _: *const Pass,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return Result.contextPatternKey(source_context, module_idx, pattern_idx);
    }

    fn resultPatternKeyForThread(
        self: *const Pass,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return self.resultPatternKeyForSourceContext(thread.requireSourceContext(), module_idx, pattern_idx);
    }

    fn resultTypeVarKeyForSourceContext(
        _: *const Pass,
        source_context: SourceContext,
        module_idx: u32,
        type_var: types.Var,
    ) ContextTypeVarKey {
        return ContextMono.Result.contextTypeVarKey(source_context, module_idx, type_var);
    }

    fn resultTypeVarKeyForThread(
        self: *const Pass,
        thread: SemanticThread,
        module_idx: u32,
        type_var: types.Var,
    ) ContextTypeVarKey {
        return self.resultTypeVarKeyForSourceContext(thread.requireSourceContext(), module_idx, type_var);
    }


const MaterializeCallableValueFailure = enum {
    non_function_monotype,
    requires_owner_callable_inst,
    nested_callable_value_shape,
};

    const CallableVariantBuilder = struct {
        variants: std.ArrayList(CallableInstId),

        fn init() CallableVariantBuilder {
            return .{ .variants = .empty };
        }

        fn deinit(self: *CallableVariantBuilder, allocator: Allocator) void {
            self.variants.deinit(allocator);
        }

        fn isEmpty(self: *const CallableVariantBuilder) bool {
            return self.variants.items.len == 0;
        }

        fn includeCallableInst(
            self: *CallableVariantBuilder,
            pass: *Pass,
            callable_inst_id: CallableInstId,
        ) Allocator.Error!void {
            for (self.variants.items) |existing_callable_inst_id| {
                if (existing_callable_inst_id == callable_inst_id) return;
            }
            try self.variants.append(pass.allocator, callable_inst_id);
        }

        fn includeCallableValue(
            self: *CallableVariantBuilder,
            pass: *Pass,
            result: *const Result,
            callable_value: CallableValue,
        ) Allocator.Error!void {
            for (result.lambdamono.getCallableValueVariants(callable_value)) |callable_inst_id| {
                try self.includeCallableInst(pass, callable_inst_id);
            }
        }

        fn includeCallSite(
            self: *CallableVariantBuilder,
            pass: *Pass,
            result: *const Result,
            call_site: CallSite,
        ) Allocator.Error!void {
            for (result.lambdamono.getCallSiteVariants(call_site)) |callable_inst_id| {
                try self.includeCallableInst(pass, callable_inst_id);
            }
        }

        fn finishValue(
            self: *CallableVariantBuilder,
            pass: *Pass,
            result: *Result,
        ) Allocator.Error!?CallableValue {
            if (self.variants.items.len == 0) return null;
            std.mem.sortUnstable(CallableInstId, self.variants.items, {}, struct {
                fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan);
            if (self.variants.items.len == 1) {
                return .{ .direct = self.variants.items[0] };
            }
            return .{ .packed_fn = try pass.ensurePackedFnForVariants(result, self.variants.items) };
        }

        fn finishCallSite(
            self: *CallableVariantBuilder,
            pass: *Pass,
            result: *Result,
        ) Allocator.Error!?CallSite {
            if (self.variants.items.len == 0) return null;
            std.mem.sortUnstable(CallableInstId, self.variants.items, {}, struct {
                fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan);
            if (self.variants.items.len == 1) {
                return .{ .direct = self.variants.items[0] };
            }
            return .{ .indirect_call = try pass.ensureIndirectCallForVariants(result, self.variants.items) };
        }
    };

    fn getContextPatternMonotypeInContext(
        _: *const Pass,
        result: *const Result,
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
        _: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        return result.getContextPatternMonotype(thread.requireSourceContext(), module_idx, pattern_idx);
    }

    fn resolveBoundPatternCallableValueInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        return self.readCallableParamValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
        );
    }

    fn resolveBoundPatternCallableValueForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        return self.readCallableParamValue(
            result,
            thread.requireSourceContext(),
            module_idx,
            pattern_idx,
        );
    }

    fn resolveBoundPatternCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const callable_value = self.resolveBoundPatternCallableValueInContext(
            result,
            context_callable_inst,
            module_idx,
            pattern_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn resolveBoundPatternCallableInstForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const callable_value = self.resolveBoundPatternCallableValueForThread(
            result,
            thread,
            module_idx,
            pattern_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn getExprCallableValueInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        return self.readExprCallableValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getExprCallableValueForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        return self.readExprCallableValue(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn getExprCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.getExprCallableValueInContext(
            result,
            context_callable_inst,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn getExprCallableInstForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.getExprCallableValueForThread(
            result,
            thread,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn getCallSiteInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        return self.readExprCallSite(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getCallSiteForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        return self.readExprCallSite(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn getCallSiteForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        return self.readExprCallSite(result, source_context, module_idx, expr_idx);
    }

    fn getCallSiteCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_site = self.getCallSiteInContext(
            result,
            context_callable_inst,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromCallSite(call_site);
    }

    fn getCallSiteCallableInstForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_site = self.getCallSiteForThread(
            result,
            thread,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromCallSite(call_site);
    }

    fn getCallSiteCallableInstForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_site = self.getCallSiteForSourceContext(result, source_context, module_idx, expr_idx) orelse return null;
        return Lambdamono.exactCallableInstFromCallSite(call_site);
    }

    fn getCallableParamSpecCallableValueForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        if (!sourceContextHasCallableInst(source_context)) return null;
        const resolved_context_callable_inst = requireSourceContextCallableInst(source_context);

        var projections = std.ArrayListUnmanaged(CallableParamProjection).empty;
        defer projections.deinit(self.allocator);
        const pattern_idx = self.resolveCallableParamProjectionRef(
            module_idx,
            expr_idx,
            &projections,
        ) orelse return null;

        const context_callable = result.getCallableInst(resolved_context_callable_inst);
        const template = result.getCallableTemplate(context_callable.template);
        const param_patterns = self.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);

        const param_index = for (param_patterns, 0..) |param_pattern_idx, idx| {
            if (param_pattern_idx == pattern_idx) break idx;
        } else return null;

        for (result.lambdamono.getCallableParamSpecEntries(result.getCallableInst(resolved_context_callable_inst).callable_param_specs)) |spec| {
            if (spec.param_index != param_index) continue;
            if (!self.callableParamProjectionSeqEqual(
                result.lambdamono.getCallableParamProjectionEntries(spec.projections),
                projections.items,
            )) continue;
            return spec.callable_value;
        }

        return null;
    }

    fn getCallableParamSpecCallableValueInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        return self.getCallableParamSpecCallableValueForSourceContext(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getCallableParamSpecCallableValueForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        return self.getCallableParamSpecCallableValueForSourceContext(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn getValueExprCallableValueInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        switch (expr) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableValueInContext(result, context_callable_inst, module_idx, lookup.pattern_idx)) |callable_value| {
                    return callable_value;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableValueInContext(result, context_callable_inst, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        return self.getExprCallableValueInContext(result, context_callable_inst, module_idx, expr_idx);
    }

    fn getValueExprCallableValueForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        switch (expr) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableValueForThread(result, thread, module_idx, lookup.pattern_idx)) |callable_value| {
                    return callable_value;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableValueForThread(result, thread, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        return self.getExprCallableValueForThread(result, thread, module_idx, expr_idx);
    }

    fn getValueExprCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.getValueExprCallableValueInContext(
            result,
            context_callable_inst,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn getValueExprCallableInstForThread(
        self: *const Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.getValueExprCallableValueForThread(
            result,
            thread,
            module_idx,
            expr_idx,
        ) orelse return null;
        return Lambdamono.exactCallableInstFromValue(callable_value);
    }

    fn getValueExprCallableValueForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        if (sourceContextHasCallableInst(source_context)) {
            return self.getValueExprCallableValueInContext(
                result,
                requireSourceContextCallableInst(source_context),
                module_idx,
                expr_idx,
            );
        }

        if (self.getCallableParamSpecCallableValueForSourceContext(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |source| {
            if (source.projections.isEmpty()) {
                if (sourceContextHasCallableInst(source.source_context)) {
                    return self.getValueExprCallableValueInContext(
                        result,
                        requireSourceContextCallableInst(source.source_context),
                        source.module_idx,
                        source.expr_idx,
                    );
                }
                if (self.getCallableParamSpecCallableValueForSourceContext(
                    result,
                    source.source_context,
                    source.module_idx,
                    source.expr_idx,
                )) |callable_value| {
                    return callable_value;
                }
                if (self.readExprCallableValue(result, source.source_context, source.module_idx, source.expr_idx)) |callable_value| {
                    return callable_value;
                }
            }
        }

        return null;
    }

    fn getValueExprCallableInstForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        if (self.getValueExprCallableValueForSourceContext(result, source_context, module_idx, expr_idx)) |callable_value| {
            if (Lambdamono.exactCallableInstFromValue(callable_value)) |callable_inst_id| {
                return callable_inst_id;
            }
        }

        return null;
    }

    fn resolveCallableParamProjectionRef(
        self: *const Pass,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        out: *std.ArrayListUnmanaged(CallableParamProjection),
    ) ?CIR.Pattern.Idx {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| lookup.pattern_idx,
            .e_dot_access => |dot_expr| blk: {
                if (dot_expr.args != null) break :blk null;
                const base_pattern = self.resolveCallableParamProjectionRef(module_idx, dot_expr.receiver, out) orelse break :blk null;
                out.append(self.allocator, .{ .field = .{
                    .module_idx = module_idx,
                    .ident = dot_expr.field_name,
                } }) catch return null;
                break :blk base_pattern;
            },
            .e_tuple_access => |tuple_access| blk: {
                const base_pattern = self.resolveCallableParamProjectionRef(module_idx, tuple_access.tuple, out) orelse break :blk null;
                out.append(self.allocator, .{ .tuple_elem = tuple_access.elem_index }) catch return null;
                break :blk base_pattern;
            },
            .e_block => |block| self.resolveCallableParamProjectionRef(module_idx, block.final_expr, out),
            .e_dbg => |dbg_expr| self.resolveCallableParamProjectionRef(module_idx, dbg_expr.expr, out),
            .e_expect => |expect_expr| self.resolveCallableParamProjectionRef(module_idx, expect_expr.body, out),
            .e_return => |return_expr| self.resolveCallableParamProjectionRef(module_idx, return_expr.expr, out),
            else => null,
        };
    }

    fn callableParamProjectionSeqEqual(
        self: *const Pass,
        lhs: []const CallableParamProjection,
        rhs: []const CallableParamProjection,
    ) bool {
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_proj, rhs_proj| {
            switch (lhs_proj) {
                .field => |lhs_field| switch (rhs_proj) {
                    .field => |rhs_field| {
                        if (!lhs_field.textEqual(self.all_module_envs, rhs_field)) return false;
                    },
                    else => return false,
                },
                .tuple_elem => |lhs_index| switch (rhs_proj) {
                    .tuple_elem => |rhs_index| if (lhs_index != rhs_index) return false,
                    else => return false,
                },
                .tag_payload => |lhs_payload| switch (rhs_proj) {
                    .tag_payload => |rhs_payload| if (lhs_payload.payload_index != rhs_payload.payload_index or !lhs_payload.tag_name.textEqual(self.all_module_envs, rhs_payload.tag_name)) return false,
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

    pub fn deinit(self: *Pass) void {
        _ = self;
    }

    pub fn runRootSourceExpr(self: *Pass, expr_idx: CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        trace.log("runRootSourceExpr start expr={d}", .{@intFromEnum(expr_idx)});
        try self.runRootsInto(&result, &.{expr_idx});
        return result;
    }

    pub fn runRootSourceExprs(self: *Pass, exprs: []const CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        try self.runRootsInto(&result, exprs);
        return result;
    }

    fn runRootsInto(
        self: *Pass,
        result: *Result,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        trace.log("primeAllModules start", .{});
        try Lambdasolved.seedAllModuleDefPatternOrigins(self, result);
        try result.template_catalog.primeAllModules(self.allocator, self.all_module_envs);
        trace.log("primeAllModules done", .{});
        try Lambdasolved.analyzeRoots(
            self.allocator,
            self.current_module_idx,
            exprs,
            Lambdasolved.rootAnalysisDriver(self, result),
        );
        trace.log("assembleLambdamono start", .{});
        try Lambdamono.assembleRoots(
            self.allocator,
            self.all_module_envs,
            self.current_module_idx,
            result,
            Lambdamono.programAssemblyDriver(self),
            exprs,
        );
        trace.log("assembleLambdamono done", .{});
    }

    /// Pipeline all callables rooted in the current module.
    pub fn runModule(self: *Pass) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        trace.log("primeAllModules start", .{});
        try Lambdasolved.seedAllModuleDefPatternOrigins(self, &result);
        try result.template_catalog.primeAllModules(self.allocator, self.all_module_envs);
        trace.log("primeAllModules done", .{});
        const root_exprs = result.template_catalog.getModuleRootExprs(self.current_module_idx);
        try Lambdasolved.analyzeRoots(
            self.allocator,
            self.current_module_idx,
            root_exprs,
            Lambdasolved.rootAnalysisDriver(self, &result),
        );
        trace.log("assembleLambdamono start", .{});
        try Lambdamono.assembleRoots(
            self.allocator,
            self.all_module_envs,
            self.current_module_idx,
            &result,
            Lambdamono.programAssemblyDriver(self),
            root_exprs,
        );
        trace.log("assembleLambdamono done", .{});
        return result;
    }

    fn putIfChanged(self: *Pass, map: anytype, key: anytype, value: anytype) Allocator.Error!void {
        const gop = try map.getOrPut(self.allocator, key);
        const typed_value: @TypeOf(gop.value_ptr.*) = value;
        if (!gop.found_existing or !std.meta.eql(gop.value_ptr.*, typed_value)) {
            gop.value_ptr.* = typed_value;
        }
    }

    fn appendExact(self: *Pass, list: anytype, value: anytype) Allocator.Error!void {
        const typed_value: @TypeOf(list.items[0]) = value;
        try list.append(self.allocator, typed_value);
    }

    fn readExprCallableValue(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        _ = self;
        return result.getExprCallableValue(source_context, module_idx, expr_idx);
    }

    fn readCallableParamValue(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        _ = self;
        return result.getPatternCallableValue(source_context, module_idx, pattern_idx);
    }

    fn isTopLevelDefPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) bool {
        return findDefByPattern(module_env, pattern_idx) != null;
    }

    fn readExprCallSite(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        _ = self;
        return result.getExprCallSite(source_context, module_idx, expr_idx);
    }

    fn exprVisitKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
        return (@as(u64, module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    }

    fn contextExprVisitKey(
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprVisitKey {
        return Result.contextExprKey(source_context, module_idx, expr_idx);
    }

    fn resolveRequiredLookupTarget(
        self: *Pass,
        module_env: *const ModuleEnv,
        lookup: @TypeOf(@as(CIR.Expr, undefined).e_lookup_required),
    ) ?RequiredLookupTarget {
        const app_idx = self.app_module_idx orelse return null;
        const required_type = module_env.requires_types.get(lookup.requires_idx);
        const required_name = module_env.getIdent(required_type.ident);

        const app_env = self.all_module_envs[app_idx];
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

    fn monotypeContainsNestedCallableValueShape(
        self: *Pass,
        result: *const Result,
        monotype: ResolvedMonotype,
    ) Allocator.Error!bool {
        if (monotype.isNone()) return false;
        var visited: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visited.deinit(self.allocator);
        return self.monotypeContainsNestedCallableValueShapeRec(result, monotype.idx, monotype.module_idx, &visited);
    }

    fn monotypeContainsNestedCallableValueShapeRec(
        self: *Pass,
        result: *const Result,
        monotype_idx: Monotype.Idx,
        monotype_module_idx: u32,
        visited: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!bool {
        if (monotype_idx.isNone()) return false;
        if (result.context_mono.monotype_store.isOpaque(monotype_idx)) return false;

        const visit_key = (@as(u64, monotype_module_idx) << 32) | @as(u64, @intFromEnum(monotype_idx));
        const gop = try visited.getOrPut(self.allocator, visit_key);
        if (gop.found_existing) return false;

        return switch (result.context_mono.monotype_store.getMonotype(monotype_idx)) {
            .func => |func_mono| blk: {
                for (result.context_mono.monotype_store.getIdxSpan(func_mono.args)) |arg_mono| {
                    if (try self.monotypeContainsNestedCallableValueShapeRec(result, arg_mono, monotype_module_idx, visited)) {
                        break :blk true;
                    }
                }
                break :blk try self.monotypeContainsNestedCallableValueShapeRec(
                    result,
                    func_mono.ret,
                    monotype_module_idx,
                    visited,
                );
            },
            .box => |box_mono| self.monotypeContainsNestedCallableValueShapeRec(
                result,
                box_mono.inner,
                monotype_module_idx,
                visited,
            ),
            .list => |list_mono| self.monotypeContainsNestedCallableValueShapeRec(
                result,
                list_mono.elem,
                monotype_module_idx,
                visited,
            ),
            .tuple => |tuple_mono| blk: {
                for (result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                    if (try self.monotypeContainsNestedCallableValueShapeRec(result, elem_mono, monotype_module_idx, visited)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .record => |record_mono| blk: {
                for (result.context_mono.monotype_store.getFields(record_mono.fields)) |field| {
                    if (try self.monotypeContainsNestedCallableValueShapeRec(result, field.type_idx, monotype_module_idx, visited)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .tag_union => |tag_union_mono| blk: {
                for (result.context_mono.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                    for (result.context_mono.monotype_store.getIdxSpan(tag.payloads)) |payload_mono| {
                        if (try self.monotypeContainsNestedCallableValueShapeRec(result, payload_mono, monotype_module_idx, visited)) {
                            break :blk true;
                        }
                    }
                }
                break :blk false;
            },
            else => false,
        };
    }

    fn monotypeRequiresCallableParamSpec(
        self: *Pass,
        result: *const Result,
        monotype: Monotype.Idx,
    ) Allocator.Error!bool {
        var visited: std.AutoHashMapUnmanaged(Monotype.Idx, void) = .empty;
        defer visited.deinit(self.allocator);
        return self.monotypeRequiresCallableParamSpecRec(result, monotype, &visited);
    }

    fn monotypeRequiresCallableParamSpecRec(
        self: *Pass,
        result: *const Result,
        monotype: Monotype.Idx,
        visited: *std.AutoHashMapUnmanaged(Monotype.Idx, void),
    ) Allocator.Error!bool {
        if (monotype.isNone()) return false;
        if (result.context_mono.monotype_store.isOpaque(monotype)) return false;
        const gop = try visited.getOrPut(self.allocator, monotype);
        if (gop.found_existing) return false;

        return switch (result.context_mono.monotype_store.getMonotype(monotype)) {
            .func => true,
            .box => |box_mono| self.monotypeRequiresCallableParamSpecRec(result, box_mono.inner, visited),
            .list => |list_mono| self.monotypeRequiresCallableParamSpecRec(result, list_mono.elem, visited),
            .tuple => |tuple_mono| blk: {
                for (result.context_mono.monotype_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                    if (try self.monotypeRequiresCallableParamSpecRec(result, elem_mono, visited)) break :blk true;
                }
                break :blk false;
            },
            .record => |record_mono| blk: {
                for (result.context_mono.monotype_store.getFields(record_mono.fields)) |field| {
                    if (try self.monotypeRequiresCallableParamSpecRec(result, field.type_idx, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union_mono| blk: {
                for (result.context_mono.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                    for (result.context_mono.monotype_store.getIdxSpan(tag.payloads)) |payload_mono| {
                        if (try self.monotypeRequiresCallableParamSpecRec(result, payload_mono, visited)) break :blk true;
                    }
                }
                break :blk false;
            },
            else => false,
        };
    }

    fn templateSourceContext(template: CallableTemplate) SourceContext {
        return .{ .template_expr = .{
            .module_idx = template.module_idx,
            .expr_idx = template.runtime_expr,
        } };
    }

    fn projectResolvedMonotypeByValueProjection(
        self: *Pass,
        result: *const Result,
        source_monotype: ResolvedMonotype,
        projection: CallableParamProjection,
    ) Allocator.Error!ResolvedMonotype {
        const mono = result.context_mono.monotype_store.getMonotype(source_monotype.idx);
        return switch (projection) {
            .field => |field_name| blk: {
                const record = switch (mono) {
                    .record => |record| record,
                    else => std.debug.panic(
                        "Pipeline invariant violated: record field projection expected record monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                    if (self.identsStructurallyEqualAcrossModules(
                        field.name.module_idx,
                        field.name.ident,
                        field_name.module_idx,
                        field_name.ident,
                    )) {
                        break :blk ContextMono.resolvedMonotype(field.type_idx, source_monotype.module_idx);
                    }
                }
                std.debug.panic(
                    "Pipeline invariant violated: record field projection could not find field in monotype",
                    .{},
                );
            },
            .tuple_elem => |elem_index| blk: {
                const tuple = switch (mono) {
                    .tuple => |tuple| tuple,
                    else => std.debug.panic(
                        "Pipeline invariant violated: tuple projection expected tuple monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const elems = result.context_mono.monotype_store.getIdxSpan(tuple.elems);
                if (builtin.mode == .Debug and elem_index >= elems.len) {
                    std.debug.panic(
                        "Pipeline invariant violated: tuple projection elem_index {d} out of bounds for tuple arity {d}",
                        .{ elem_index, elems.len },
                    );
                }
                break :blk ContextMono.resolvedMonotype(elems[elem_index], source_monotype.module_idx);
            },
            .tag_payload => |payload| blk: {
                const tags = switch (mono) {
                    .tag_union => |tag_union| result.context_mono.monotype_store.getTags(tag_union.tags),
                    else => std.debug.panic(
                        "Pipeline invariant violated: tag payload projection expected tag_union monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                for (tags) |tag| {
                    if (!self.identsStructurallyEqualAcrossModules(
                        tag.name.module_idx,
                        tag.name.ident,
                        payload.tag_name.module_idx,
                        payload.tag_name.ident,
                    )) continue;
                    const payload_monos = result.context_mono.monotype_store.getIdxSpan(tag.payloads);
                    if (builtin.mode == .Debug and payload.payload_index >= payload_monos.len) {
                        std.debug.panic(
                            "Pipeline invariant violated: tag payload projection index {d} out of bounds for payload arity {d}",
                            .{ payload.payload_index, payload_monos.len },
                        );
                    }
                    break :blk ContextMono.resolvedMonotype(payload_monos[payload.payload_index], source_monotype.module_idx);
                }
                std.debug.panic(
                    "Pipeline invariant violated: tag payload projection could not find tag in monotype",
                    .{},
                );
            },
            .list_elem => |elem_index| blk: {
                _ = elem_index;
                const list = switch (mono) {
                    .list => |list| list,
                    else => std.debug.panic(
                        "Pipeline invariant violated: list elem projection expected list monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                break :blk ContextMono.resolvedMonotype(list.elem, source_monotype.module_idx);
            },
        };
    }

    fn directCallContainsErrorType(
        self: *Pass,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!bool {
        const module_env = self.all_module_envs[module_idx];
        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (try self.typeVarContainsError(&module_env.types, ModuleEnv.varFrom(call_expr_idx), &seen)) {
            return true;
        }
        if (try self.typeVarContainsError(&module_env.types, ModuleEnv.varFrom(call_expr.func), &seen)) {
            return true;
        }

        for (module_env.store.sliceExpr(call_expr.args)) |arg_expr_idx| {
            if (try self.typeVarContainsError(&module_env.types, ModuleEnv.varFrom(arg_expr_idx), &seen)) {
                return true;
            }
        }

        return false;
    }

    fn typeVarContainsError(
        self: *Pass,
        store_types: *const types.Store,
        var_: types.Var,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        const resolved = store_types.resolveVar(var_);
        if (seen.contains(resolved.var_)) return false;
        try seen.put(self.allocator, resolved.var_, {});

        return switch (resolved.desc.content) {
            .err => true,
            .flex, .rigid => false,
            .alias => |alias| self.typeVarContainsError(
                store_types,
                store_types.getAliasBackingVar(alias),
                seen,
            ),
            .structure => |flat_type| switch (flat_type) {
                .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                    for (store_types.sliceVars(func.args)) |arg_var| {
                        if (try self.typeVarContainsError(store_types, arg_var, seen)) break :blk true;
                    }
                    break :blk try self.typeVarContainsError(store_types, func.ret, seen);
                },
                .nominal_type => |nominal| blk: {
                    for (store_types.sliceNominalArgs(nominal)) |arg_var| {
                        if (try self.typeVarContainsError(store_types, arg_var, seen)) break :blk true;
                    }
                    break :blk try self.typeVarContainsError(
                        store_types,
                        store_types.getNominalBackingVar(nominal),
                        seen,
                    );
                },
                .record => |record| self.recordTypeContainsError(store_types, record, seen),
                .record_unbound => |fields_range| self.recordFieldsContainError(
                    store_types,
                    store_types.getRecordFieldsSlice(fields_range).items(.var_),
                    seen,
                ),
                .tuple => |tuple| blk: {
                    for (store_types.sliceVars(tuple.elems)) |elem_var| {
                        if (try self.typeVarContainsError(store_types, elem_var, seen)) break :blk true;
                    }
                    break :blk false;
                },
                .tag_union => |tag_union| self.tagUnionContainsError(store_types, tag_union, seen),
                .empty_record, .empty_tag_union => false,
            },
        };
    }

    fn recordFieldsContainError(
        self: *Pass,
        store_types: *const types.Store,
        field_vars: []const types.Var,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        for (field_vars) |field_var| {
            if (try self.typeVarContainsError(store_types, field_var, seen)) return true;
        }
        return false;
    }

    fn recordTypeContainsError(
        self: *Pass,
        store_types: *const types.Store,
        record: types.Record,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        var current_row = record;

        rows: while (true) {
            const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
            if (try self.recordFieldsContainError(store_types, fields_slice.items(.var_), seen)) {
                return true;
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
                        .record => |next_row| {
                            current_row = next_row;
                            continue :rows;
                        },
                        .record_unbound => |fields_range| {
                            return self.recordFieldsContainError(
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
        self: *Pass,
        store_types: *const types.Store,
        tag_union: types.TagUnion,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        var current_row = tag_union;

        rows: while (true) {
            const tags_slice = store_types.getTagsSlice(current_row.tags);
            for (tags_slice.items(.args)) |payloads_range| {
                for (store_types.sliceVars(payloads_range)) |payload_var| {
                    if (try self.typeVarContainsError(store_types, payload_var, seen)) {
                        return true;
                    }
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

    fn requireFullyBoundExprMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        const resolved = (try ContextMono.lookupRecordedExprMonotypeIfReadyForSourceContext(
            self,
            result,
            thread,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        )) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline invariant violated: missing fully-bound monotype for expr {d} ({s}) in module {d} under source context {s}/{d}/{d}",
                    .{
                        @intFromEnum(expr_idx),
                        @tagName(self.all_module_envs[module_idx].store.getExpr(expr_idx)),
                        module_idx,
                        @tagName(thread.requireSourceContext()),
                        switch (thread.requireSourceContext()) {
                            .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
                            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
                        },
                        switch (thread.requireSourceContext()) {
                            .root_expr => |root| @intFromEnum(root.expr_idx),
                            .provenance_expr => |source| @intFromEnum(source.expr_idx),
                            .template_expr => |template_ctx| @intFromEnum(template_ctx.expr_idx),
                            .callable_inst => std.math.maxInt(u32),
                        },
                    },
                );
            }
            unreachable;
        };
        return resolved;
    }

    fn bindCurrentPatternFromResolvedMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        resolved_mono: ResolvedMonotype,
    ) Allocator.Error!void {
        if (resolved_mono.isNone()) return;
        if (result.context_mono.monotype_store.getMonotype(resolved_mono.idx) == .func) return;
        try ContextMono.recordTypeVarMonotypeForThread(
            self,
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(pattern_idx),
            resolved_mono.idx,
            resolved_mono.module_idx,
        );
        try ContextMono.mergeContextPatternMonotype(
            self,
            result,
            self.resultPatternKeyForThread(thread, module_idx, pattern_idx),
            resolved_mono,
        );

        const module_env = self.all_module_envs[module_idx];
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
                try self.bindCurrentPatternFromResolvedMonotype(
                    result,
                    thread,
                    module_idx,
                    as_pat.pattern,
                    resolved_mono,
                );
            },
            .nominal => |nominal_pat| {
                try self.bindCurrentPatternFromResolvedMonotype(
                    result,
                    thread,
                    module_idx,
                    nominal_pat.backing_pattern,
                    resolved_mono,
                );
            },
            .nominal_external => |nominal_pat| {
                try self.bindCurrentPatternFromResolvedMonotype(
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
                const tag_idx = self.tagIndexByNameInSpan(
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
                    try self.bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        payload_pattern_idx,
                        ContextMono.resolvedMonotype(payload_mono, resolved_mono.module_idx),
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
                            const field_idx = self.recordFieldIndexByName(
                                module_idx,
                                destruct.label,
                                resolved_mono.module_idx,
                                mono_fields,
                            );
                            try self.bindCurrentPatternFromResolvedMonotype(
                                result,
                                thread,
                                module_idx,
                                sub_pattern_idx,
                                ContextMono.resolvedMonotype(mono_fields[field_idx].type_idx, resolved_mono.module_idx),
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
                    try self.bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        elem_pattern_idx,
                        ContextMono.resolvedMonotype(elem_mono, resolved_mono.module_idx),
                    );
                }
                if (list_pat.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindCurrentPatternFromResolvedMonotype(
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
                    try self.bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        elem_pattern_idx,
                        ContextMono.resolvedMonotype(elem_mono, resolved_mono.module_idx),
                    );
                }
            },
        }
    }

    fn bindMatchBranchPatternsFromCondMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        match_expr: @TypeOf(@as(CIR.Expr, undefined).e_match),
        cond_mono: ResolvedMonotype,
    ) Allocator.Error!void {
        if (cond_mono.isNone()) return;

        const module_env = self.all_module_envs[module_idx];
        const branches = module_env.store.sliceMatchBranches(match_expr.branches);
        for (branches) |branch_idx| {
            const branch = module_env.store.getMatchBranch(branch_idx);
            for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                try self.bindCurrentPatternFromResolvedMonotype(
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
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        match_expr: @TypeOf(@as(CIR.Expr, undefined).e_match),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
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
                try Lambdasolved.recordContextPatternSourceExpr(
                    self,
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
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const demanded = ContextMono.resolvedMonotype(monotype, monotype_module_idx);
        try self.recordExprMonotypeIfUnsetOrEqualForSourceContext(
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
                try self.propagateDemandedValueMonotypeToExprRef(
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
                        const mono_field_idx = self.recordFieldIndexByNameInSpan(
                            result,
                            module_idx,
                            field.name,
                            demanded.module_idx,
                            record_mono.fields,
                        );
                        const mono_field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, mono_field_idx);
                        try self.propagateDemandedValueMonotypeToValueExpr(
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
                            "Pipeline invariant violated: demanded tuple monotype arity {d} did not match tuple expr arity {d} for expr {d} in module {d}",
                            .{ demanded_elems.len, elems.len, @intFromEnum(expr_idx), module_idx },
                        );
                    }
                    for (elems, demanded_elems) |elem_expr_idx, elem_mono| {
                        try self.propagateDemandedValueMonotypeToValueExpr(
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
                    const tag_idx = self.tagIndexByNameInSpan(
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
                            "Pipeline invariant violated: demanded tag payload arity {d} did not match tag expr arity {d} for expr {d} in module {d}",
                            .{ payload_monos.len, payload_exprs.len, @intFromEnum(expr_idx), module_idx },
                        );
                    }
                    for (payload_exprs, payload_monos) |payload_expr_idx, payload_mono| {
                        try self.propagateDemandedValueMonotypeToValueExpr(
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
                        try self.propagateDemandedValueMonotypeToValueExpr(
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
        self: *Pass,
        result: *Result,
        expr_ref: ExprRef,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        return self.propagateDemandedValueMonotypeToExprRefProjections(
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
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        projections: []const ValueProjection,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        if (projections.len == 0) {
            return self.propagateDemandedValueMonotypeToValueExpr(
                result,
                source_context,
                module_idx,
                expr_idx,
                monotype,
                monotype_module_idx,
            );
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const rest = projections[1..];

        switch (projections[0]) {
            .field => |field_name| switch (expr) {
                .e_record => |record_expr| {
                    for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (field.name.eql(field_name.ident)) {
                            return self.propagateDemandedValueMonotypeToExprRefProjections(
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
                        "Pipeline invariant violated: field projection demand for expr {d} in module {d} could not find field {d}",
                        .{ @intFromEnum(expr_idx), module_idx, @as(u32, @bitCast(field_name.ident)) },
                    );
                },
                else => return self.requireProjectedExprDemandMatchesResolvedMonotype(
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
                            "Pipeline invariant violated: tuple projection demand out of bounds for expr {d} elem {d} in module {d}",
                            .{ @intFromEnum(expr_idx), elem_index, module_idx },
                        );
                    }
                    return self.propagateDemandedValueMonotypeToExprRefProjections(
                        result,
                        source_context,
                        module_idx,
                        elems[elem_index],
                        rest,
                        monotype,
                        monotype_module_idx,
                    );
                },
                else => return self.requireProjectedExprDemandMatchesResolvedMonotype(
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
                    if (!expr_tag_name.textEqual(self.all_module_envs, payload.tag_name)) {
                        return;
                    }
                    const args = module_env.store.sliceExpr(tag_expr.args);
                    if (payload.payload_index >= args.len) {
                        std.debug.panic(
                            "Pipeline invariant violated: tag projection demand out of bounds for expr {d} payload {d} in module {d}",
                            .{ @intFromEnum(expr_idx), payload.payload_index, module_idx },
                        );
                    }
                    return self.propagateDemandedValueMonotypeToExprRefProjections(
                        result,
                        source_context,
                        module_idx,
                        args[payload.payload_index],
                        rest,
                        monotype,
                        monotype_module_idx,
                    );
                },
                else => return self.requireProjectedExprDemandMatchesResolvedMonotype(
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
                            "Pipeline invariant violated: list projection demand out of bounds for expr {d} elem {d} in module {d}",
                            .{ @intFromEnum(expr_idx), elem_index, module_idx },
                        );
                    }
                    return self.propagateDemandedValueMonotypeToExprRefProjections(
                        result,
                        source_context,
                        module_idx,
                        elems[elem_index],
                        rest,
                        monotype,
                        monotype_module_idx,
                    );
                },
                else => return self.requireProjectedExprDemandMatchesResolvedMonotype(
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
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        projections: []const ValueProjection,
        demanded_monotype: Monotype.Idx,
        demanded_module_idx: u32,
    ) Allocator.Error!void {
        var projected = try ContextMono.requireRecordedExprMonotypeForSourceContext(
            self,
            result,
            SemanticThread.trackedThread(source_context),
            source_context,
            module_idx,
            expr_idx,
        );
        for (projections) |projection| {
            projected = try self.projectResolvedMonotypeByValueProjection(result, projected, projection);
        }
        if (!try ContextMono.monotypesStructurallyEqualAcrossModules(
            self,
            result,
            projected.idx,
            projected.module_idx,
            demanded_monotype,
            demanded_module_idx,
        )) {
            std.debug.panic(
                "Pipeline invariant violated: projected demand monotype mismatch for expr {d} in module {d} under source context {s}",
                .{
                    @intFromEnum(expr_idx),
                    module_idx,
                    @tagName(source_context),
                },
            );
        }
    }

    fn recordExprMonotypeIfUnsetOrEqualForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        const thread = SemanticThread.trackedThread(source_context);
        const existing = try ContextMono.lookupRecordedExprMonotypeIfReadyForSourceContext(
            self,
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
        );
        if (existing) |resolved_existing| {
            if (try ContextMono.monotypesStructurallyEqualAcrossModules(
                self,
                result,
                resolved_existing.idx,
                resolved_existing.module_idx,
                monotype,
                monotype_module_idx,
            )) {
                return;
            }

            const module_env = self.all_module_envs[module_idx];
            const expr = module_env.store.getExpr(expr_idx);
            const expr_region = module_env.store.getExprRegion(expr_idx);
            const source = module_env.getSourceAll();
            const snippet_start = @min(expr_region.start.offset, source.len);
            const snippet_end = @min(expr_region.end.offset, source.len);
            std.debug.panic(
                "Pipeline invariant violated: demanded exact monotype conflicted with existing exact monotype for expr {d} kind={s} region={any} snippet=\"{s}\" in module {d} ctx={s}; existing={d}@{d} existing_mono={any} demanded={d}@{d} demanded_mono={any}",
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

        try ContextMono.recordExprMonotypeForSourceContext(
            self,
            result,
            source_context,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    fn propagateDemandedCallableFnMonotypeToValueExpr(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!void {
        const visit_key = contextExprVisitKey(source_context, module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        try ContextMono.recordExprMonotypeForSourceContext(
            self,
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
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
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
                    try self.propagateDemandedCallableFnMonotypeToValueExpr(
                        result,
                        source_context,
                        module_idx,
                        branch.body,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                }
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
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
                    try self.propagateDemandedCallableFnMonotypeToValueExpr(
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

    fn resolveExprRefCallableValue(
        self: *Pass,
        result: *Result,
        expr_ref: ExprRef,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!?CallableValue {
        return self.resolveProjectedExprCallableValue(
            result,
            expr_ref.source_context,
            expr_ref.module_idx,
            expr_ref.expr_idx,
            result.lambdamono.getValueProjectionEntries(expr_ref.projections),
            visiting,
        );
    }

    fn resolveProjectedExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        projections: []const ValueProjection,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!?CallableValue {
        if (projections.len == 0) {
            try Lambdasolved.realizeStructuredExprCallableSemantics(
                self,
                result,
                source_context,
                module_idx,
                expr_idx,
                visiting,
            );
            return self.getValueExprCallableValueForSourceContext(result, source_context, module_idx, expr_idx);
        }

        if (result.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
            const combined_projections = try self.appendValueProjectionEntries(
                result,
                origin.projections,
                projections,
            );
            return self.resolveProjectedExprCallableValue(
                result,
                origin.source_context,
                origin.module_idx,
                origin.expr_idx,
                result.lambdamono.getValueProjectionEntries(combined_projections),
                visiting,
            );
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const projection = projections[0];
        const rest = projections[1..];
        switch (projection) {
            .field => |field_name| switch (expr) {
                .e_record => |record_expr| {
                    for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        if (!self.identsStructurallyEqualAcrossModules(
                            module_idx,
                            field.name,
                            field_name.module_idx,
                            field_name.ident,
                        )) continue;
                        return self.resolveProjectedExprCallableValue(
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
                            "Pipeline invariant violated: tuple projection elem_index {d} out of bounds for expr {d} in module {d}",
                            .{ elem_index, @intFromEnum(expr_idx), module_idx },
                        );
                    }
                    return self.resolveProjectedExprCallableValue(
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
                    if (!self.identsStructurallyEqualAcrossModules(
                        module_idx,
                        tag_expr.name,
                        payload.tag_name.module_idx,
                        payload.tag_name.ident,
                    )) return null;
                    const args = module_env.store.sliceExpr(tag_expr.args);
                    if (builtin.mode == .Debug and payload.payload_index >= args.len) {
                        std.debug.panic(
                            "Pipeline invariant violated: tag payload projection index {d} out of bounds for expr {d} in module {d}",
                            .{ payload.payload_index, @intFromEnum(expr_idx), module_idx },
                        );
                    }
                    return self.resolveProjectedExprCallableValue(
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
                            "Pipeline invariant violated: list projection elem_index {d} out of bounds for expr {d} in module {d}",
                            .{ elem_index, @intFromEnum(expr_idx), module_idx },
                        );
                    }
                    return self.resolveProjectedExprCallableValue(
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

    fn copyExprCallableValueFromRef(
        self: *Pass,
        result: *Result,
        target_source_context: SourceContext,
        target_module_idx: u32,
        target_expr_idx: CIR.Expr.Idx,
        maybe_pattern_idx: ?CIR.Pattern.Idx,
        source: ExprRef,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!void {
        var resolved_callable_value = try self.resolveExprRefCallableValue(result, source, visiting);
        if (resolved_callable_value == null and source.projections.isEmpty()) {
            if (result.getExprTemplateId(source.source_context, source.module_idx, source.expr_idx)) |template_id| {
                const target_fn_monotype = (try ContextMono.resolveExprMonotypeResolved(
                    self,
                    result,
                    SemanticThread.trackedThread(target_source_context),
                    target_module_idx,
                    target_expr_idx,
                )) orelse return;
                {
                    const materialize_failure = try Lambdasolved.materializeExprCallableValueWithKnownFnMonotype(
                        self,
                        result,
                        source.source_context,
                        source.module_idx,
                        source.expr_idx,
                        template_id,
                        target_fn_monotype,
                    );
                    resolved_callable_value = try self.resolveExprRefCallableValue(result, source, visiting);
                    if (resolved_callable_value == null and std.debug.runtime_safety) {
                        std.debug.panic(
                            "Pipeline invariant violated: callable source expr {d} in module {d} under source context {s} failed to materialize callable value for target expr {d} in module {d} under target context {s} using exact fn monotype {d}@{d}; reason={?s} template={d} target_tag={s}",
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
                                @tagName(self.all_module_envs[target_module_idx].store.getExpr(target_expr_idx)),
                            },
                        );
                    }
                }
            }
        }
        const callable_value = resolved_callable_value orelse return;

        try Lambdasolved.setExprCallableValue(self,
            result,
            target_source_context,
            target_module_idx,
            target_expr_idx,
            callable_value,
        );
        if (maybe_pattern_idx) |pattern_idx| {
            try Lambdasolved.setCallableParamValue(self,
                result,
                target_source_context,
                target_module_idx,
                pattern_idx,
                callable_value,
            );
        }
    }

    fn copyExprCallableValue(
        self: *Pass,
        result: *Result,
        target_source_context: SourceContext,
        target_module_idx: u32,
        target_expr_idx: CIR.Expr.Idx,
        maybe_pattern_idx: ?CIR.Pattern.Idx,
        source_context: SourceContext,
        source_module_idx: u32,
        source_expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!void {
        return self.copyExprCallableValueFromRef(
            result,
            target_source_context,
            target_module_idx,
            target_expr_idx,
            maybe_pattern_idx,
            .{
                .source_context = source_context,
                .module_idx = source_module_idx,
                .expr_idx = source_expr_idx,
                .projections = .empty(),
            },
            visiting,
        );
    }

    fn appendDispatchActualArgsFromProgram(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        actual_args: *std.ArrayList(CIR.Expr.Idx),
    ) Allocator.Error!void {
        const expr_id = try self.ensureReservedDispatchActualArgChildren(
            result,
            source_context,
            module_idx,
            expr_idx,
        );
        for (result.lambdamono.getExprChildren(result.getExprChildExprsById(expr_id))) |child_expr_id| {
            try actual_args.append(self.allocator, result.getExprSourceExprById(child_expr_id));
        }
    }

    fn ensureReservedDispatchActualArgChildren(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Lambdamono.ExprId {
        const appendChildIfPresent = struct {
            fn run(
                pass: *Pass,
                result_inner: *Result,
                child_exprs: *std.ArrayList(Lambdamono.ExprId),
                source_context_inner: SourceContext,
                module_idx_inner: u32,
                child_expr_idx: CIR.Expr.Idx,
            ) Allocator.Error!void {
                if (result_inner.getExprTemplateId(source_context_inner, module_idx_inner, child_expr_idx) != null) {
                    const monotype = try ContextMono.lookupRecordedExprMonotypeIfReadyForSourceContext(
                        pass,
                        result_inner,
                        SemanticThread.trackedThread(source_context_inner),
                        source_context_inner,
                        module_idx_inner,
                        child_expr_idx,
                    );
                    if (monotype == null) return;
                }

                const child_expr_id = result_inner.lambdamono.getExprId(
                    source_context_inner,
                    module_idx_inner,
                    child_expr_idx,
                ) orelse blk: {
                    _ = try pass.ensureProgramExpr(
                        result_inner,
                        source_context_inner,
                        module_idx_inner,
                        child_expr_idx,
                    );
                    break :blk result_inner.lambdamono.getExprId(
                        source_context_inner,
                        module_idx_inner,
                        child_expr_idx,
                    ).?;
                };
                try child_exprs.append(pass.allocator, child_expr_id);
            }
        }.run;

        const expr_id = result.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse blk: {
            _ = try Lambdamono.ensureProgramExpr(self, result, source_context, module_idx, expr_idx);
            break :blk result.lambdamono.getExprId(source_context, module_idx, expr_idx).?;
        };
        if (!result.lambdamono.getExprPtr(expr_id).common().child_exprs.isEmpty()) return expr_id;

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var child_exprs = std.ArrayList(Lambdamono.ExprId).empty;
        defer child_exprs.deinit(self.allocator);

        switch (expr) {
            .e_binop => |binop_expr| {
                try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, binop_expr.lhs);
                try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, binop_expr.rhs);
            },
            .e_unary_minus => |unary_expr| {
                try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, unary_expr.expr);
            },
            .e_dot_access => |dot_expr| {
                try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, dot_expr.receiver);
                if (dot_expr.args) |args| for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
                    try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, arg_expr_idx);
                };
            },
            .e_type_var_dispatch => |dispatch_expr| for (module_env.store.sliceExpr(dispatch_expr.args)) |arg_expr_idx| {
                try appendChildIfPresent(self, result, &child_exprs, source_context, module_idx, arg_expr_idx);
            },
            else => unreachable,
        }

        result.lambdamono.getExprPtr(expr_id).commonMut().child_exprs =
            try result.lambdamono.appendExprChildren(self.allocator, child_exprs.items);
        return expr_id;
    }

    fn specializeDispatchExactCallable(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        template_id: CallableTemplateId,
    ) Allocator.Error!?CallableInstId {
        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgsFromProgram(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            &actual_args,
        );
        const defining_source_context = Lambdasolved.requireTemplateDemandSourceContextForExpr(
            self,
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            template_id,
        );
        const resolved_target = switch (expr) {
            .e_binop => |binop_expr| try Lambdasolved.resolveDispatchTargetForExpr(self, result, thread, module_idx, expr_idx, identFromBinOp(binop_expr.op)),
            .e_dot_access => |dot_expr| try Lambdasolved.resolveDispatchTargetForExpr(self, result, thread, module_idx, expr_idx, dot_expr.field_name),
            .e_type_var_dispatch => |dispatch_expr| try Lambdasolved.resolveDispatchTargetForExpr(self, result, thread, module_idx, expr_idx, dispatch_expr.method_name),
            else => unreachable,
        };
        const resolved_template_id = try DispatchSolved.lookupResolvedDispatchTemplate(self, result, module_idx, resolved_target);
        if (resolved_template_id != template_id) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline invariant violated: dispatch expr {d} in module {d} resolved target template mismatch",
                    .{ @intFromEnum(expr_idx), module_idx },
                );
            }
            unreachable;
        }
        const exact_desired_fn_monotype = try ContextMono.resolveTypeVarMonotypeResolved(self, result, thread, module_idx,
            resolved_target.fn_var,
        );
        if (exact_desired_fn_monotype.isNone()) return null;
        const fn_monotype = exact_desired_fn_monotype.idx;
        const fn_monotype_module_idx = exact_desired_fn_monotype.module_idx;

        var callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
        defer callable_param_specs.deinit(self.allocator);
        const callable_param_specs_complete = try Lambdasolved.collectDirectCallCallableParamSpecs(
            self,
            result,
            thread.requireSourceContext(),
            module_idx,
            fn_monotype,
            fn_monotype_module_idx,
            actual_args.items,
            &callable_param_specs,
        );
        if (!callable_param_specs_complete) return null;

        const callable_inst_id = try self.requireCallableInstWithCallableParamSpecs(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            callable_param_specs.items,
        );
        return callable_inst_id;
    }

    fn bindCurrentIntrinsicDispatchSemantics(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) Allocator.Error!void {
        switch (expr) {
            .e_binop => |binop_expr| {
                if (binop_expr.op == .eq or binop_expr.op == .ne) {
                    if (try ContextMono.resolveExprMonotypeResolved(self, result, thread, module_idx, binop_expr.lhs)) |lhs_monotype| {
                        if (result.context_mono.monotype_store.getMonotype(lhs_monotype.idx) != .func) {
                            try self.bindCurrentExprTypeRoot(
                                result,
                                thread,
                                module_idx,
                                binop_expr.rhs,
                                lhs_monotype.idx,
                                lhs_monotype.module_idx,
                            );
                            try self.propagateDemandedValueMonotypeToValueExpr(
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

        const expr_monotype = try ContextMono.resolveTypeVarMonotypeResolved(self, result, thread, module_idx,
            ModuleEnv.varFrom(expr_idx),
        );
        if (expr_monotype.isNone()) {
            std.debug.panic(
                "Pipeline invariant violated: intrinsic/no-callable dispatch expr {d} kind={s} in module {d} had no exact result monotype",
                .{ @intFromEnum(expr_idx), @tagName(expr), module_idx },
            );
        }
        if (result.context_mono.monotype_store.getMonotype(expr_monotype.idx) != .func) {
            try self.bindCurrentExprTypeRoot(
                result,
                thread,
                module_idx,
                expr_idx,
                expr_monotype.idx,
                expr_monotype.module_idx,
            );
            try self.recordCurrentExprMonotype(
                result,
                thread,
                module_idx,
                expr_idx,
                expr_monotype.idx,
                expr_monotype.module_idx,
            );
        }
    }

    fn bindCurrentExprTypeRoot(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        try ContextMono.recordExprMonotypeForThread(
            self,
            result,
            thread,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    fn recordCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;
        try ContextMono.recordExprMonotypeForThread(
            self,
            result,
            thread,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    fn recordDemandedRecordFieldMonotypes(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        record_expr: @TypeOf(@as(CIR.Expr, undefined).e_record),
    ) Allocator.Error!void {
        const record_mono = (try ContextMono.resolveExprMonotypeResolved(self, result, thread, module_idx, expr_idx)) orelse return;

        const mono = result.context_mono.monotype_store.getMonotype(record_mono.idx);
        const mono_record = switch (mono) {
            .record => |record| record,
            else => return,
        };

        const module_env = self.all_module_envs[module_idx];
        for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
            const field = module_env.store.getRecordField(field_idx);
            const field_expr = module_env.store.getExpr(field.value);
            const mono_field_idx = self.recordFieldIndexByNameInSpan(
                result,
                module_idx,
                field.name,
                record_mono.module_idx,
                mono_record.fields,
            );
            const mono_field = result.context_mono.monotype_store.getFieldItem(mono_record.fields, mono_field_idx);
            if (exprMonotypeOwnedByInvocation(field_expr)) continue;
            try self.recordCurrentExprMonotype(
                result,
                thread,
                module_idx,
                field.value,
                mono_field.type_idx,
                record_mono.module_idx,
            );
        }
    }

    /// Propagate per-element monotypes from a tuple expression's resolved
    /// monotype to its element expressions. Module-level records are stored
    /// as `e_tuple` in the CIR, and their element expressions (e.g. number
    /// literals) are context-sensitive so they don't get monotypes recorded
    /// automatically during scanning. This propagates the concrete element
    /// types from the parent's monotype so that downstream lowering resolves
    /// them correctly.
    fn recordDemandedTupleElemMonotypes(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        tuple_expr: @TypeOf(@as(CIR.Expr, undefined).e_tuple),
    ) Allocator.Error!void {
        const tuple_mono = (try ContextMono.resolveExprMonotypeResolved(self, result, thread, module_idx, expr_idx)) orelse return;

        const mono = result.context_mono.monotype_store.getMonotype(tuple_mono.idx);
        const module_env = self.all_module_envs[module_idx];
        const elems = module_env.store.sliceExpr(tuple_expr.elems);

        switch (mono) {
            .record => |record| {
                const fields = result.context_mono.monotype_store.getFields(record.fields);
                if (fields.len != elems.len) return;
                for (elems, 0..) |elem_expr_idx, i| {
                    const elem_expr = module_env.store.getExpr(elem_expr_idx);
                    const field = result.context_mono.monotype_store.getFieldItem(record.fields, i);
                    if (exprMonotypeOwnedByInvocation(elem_expr)) continue;
                    try self.recordCurrentExprMonotype(
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
                    try self.recordCurrentExprMonotype(
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

    fn propagateDemandedValueResultMonotypeToChild(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        parent_expr_idx: CIR.Expr.Idx,
        child_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const parent_mono = (try ContextMono.resolveExprMonotypeResolved(self, result, thread, module_idx, parent_expr_idx)) orelse return;
        const child_expr = self.all_module_envs[module_idx].store.getExpr(child_expr_idx);
        if (exprMonotypeOwnedByInvocation(child_expr)) return;
        try self.recordCurrentExprMonotype(
            result,
            thread,
            module_idx,
            child_expr_idx,
            parent_mono.idx,
            parent_mono.module_idx,
        );
    }

    fn ensurePackedFnForVariants(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!PackedFn {
        return result.lambdamono.makePackedFnForCallableInsts(
            self.allocator,
            self.all_module_envs,
            self.current_module_idx,
            &result.context_mono,
            callable_inst_ids,
        );
    }

    fn ensureIndirectCallForVariants(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!IndirectCall {
        return result.lambdamono.makeIndirectCallForCallableInsts(
            self.allocator,
            self.all_module_envs,
            self.current_module_idx,
            &result.context_mono,
            callable_inst_ids,
        );
    }

    fn ensureBuiltinBoxUnboxCallableInst(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        box_monotype: Monotype.Idx,
        inner_monotype: Monotype.Idx,
    ) Allocator.Error!void {
        const source_env = self.all_module_envs[source_module_idx];
        const common = ModuleEnv.CommonIdents.find(&source_env.common);
        const builtin_module_idx = TemplateCatalog.findModuleForOrigin(
            self.all_module_envs,
            source_env,
            common.builtin_module,
        );
        const builtin_env = self.all_module_envs[builtin_module_idx];
        const method_name = source_env.getIdent(common.builtin_box_unbox);
        const target_ident = builtin_env.common.findIdent(method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline invariant violated: builtin box/unbox method '{s}' not found in builtin module {d}",
                    .{ method_name, builtin_module_idx },
                );
            }
            unreachable;
        };
        const node_idx = builtin_env.getExposedNodeIndexById(target_ident) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline invariant violated: builtin box/unbox method '{s}' has no exposed def node in builtin module {d}",
                    .{ method_name, builtin_module_idx },
                );
            }
            unreachable;
        };
        if (!builtin_env.store.isDefNode(node_idx)) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline invariant violated: builtin box/unbox method '{s}' exposed node {d} is not a def in builtin module {d}",
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

        const args = try result.context_mono.monotype_store.addIdxSpan(self.allocator, &.{box_monotype});
        const fn_monotype = try result.context_mono.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = args,
            .ret = inner_monotype,
            .effectful = false,
        } });

        _ = try self.requireCallableInst(result, templateSourceContext(result.getCallableTemplate(template_id).*) , template_id, fn_monotype, source_module_idx);
    }

    fn bindTypeVarMonotypesInStore(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        common_idents: ModuleEnv.CommonIdents,
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
        type_var: types.Var,
        monotype: Monotype.Idx,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        const resolved = store_types.resolveVar(type_var);
        if (bindings.get(resolved.var_)) |existing| {
            if (!(try ContextMono.monotypesStructurallyEqual(self, result, existing, monotype))) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: conflicting monotype binding for type var root {d}",
                        .{@intFromEnum(resolved.var_)},
                    );
                }
                unreachable;
            }
            return;
        }

        switch (resolved.desc.content) {
            .flex, .rigid => try bindings.put(resolved.var_, monotype),
            .alias => |alias| try self.bindTypeVarMonotypesInStore(
                result,
                module_idx,
                store_types,
                common_idents,
                bindings,
                store_types.getAliasBackingVar(alias),
                monotype,
            ),
            .structure => |flat_type| {
                try bindings.put(resolved.var_, monotype);
                try self.bindFlatTypeMonotypesInStore(
                    result,
                    module_idx,
                    store_types,
                    common_idents,
                    bindings,
                    flat_type,
                    monotype,
                );
            },
            .err => {},
        }
    }

    fn flatRecordRepresentsEmpty(store_types: *const types.Store, record: types.Record) bool {
        var current_row = record;

        rows: while (true) {
            if (store_types.getRecordFieldsSlice(current_row.fields).len != 0) return false;

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
                        .record_unbound => |fields_range| return store_types.getRecordFieldsSlice(fields_range).len == 0,
                        .empty_record => return true,
                        else => return false,
                    },
                    .flex, .rigid, .err => return false,
                }
            }
        }
    }

    fn bindFlatTypeMonotypesInStore(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        common_idents: ModuleEnv.CommonIdents,
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
        flat_type: types.FlatType,
        monotype: Monotype.Idx,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        const mono = result.context_mono.monotype_store.getMonotype(monotype);

        switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const mfunc = switch (mono) {
                    .func => |mfunc| mfunc,
                    else => unreachable,
                };

                const type_args = store_types.sliceVars(func.args);
                const mono_args = result.context_mono.monotype_store.getIdxSpan(mfunc.args);
                if (type_args.len != mono_args.len) unreachable;
                for (type_args, 0..) |arg_var, i| {
                    try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, arg_var, mono_args[i]);
                }
                try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, func.ret, mfunc.ret);
            },
            .nominal_type => |nominal| {
                const ident = nominal.ident.ident_idx;
                const origin = nominal.origin_module;

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                    const mlist = switch (mono) {
                        .list => |mlist| mlist,
                        else => unreachable,
                    };
                    const type_args = store_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, type_args[0], mlist.elem);
                    return;
                }

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                    const mbox = switch (mono) {
                        .box => |mbox| mbox,
                        else => unreachable,
                    };
                    const type_args = store_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, type_args[0], mbox.inner);
                    return;
                }

                if (origin.eql(common_idents.builtin_module) and DispatchSolved.builtinPrimForNominal(ident, common_idents) != null) {
                    switch (mono) {
                        .prim => {},
                        else => unreachable,
                    }
                    return;
                }

                try self.bindTypeVarMonotypesInStore(
                    result,
                    module_idx,
                    store_types,
                    common_idents,
                    bindings,
                    store_types.getNominalBackingVar(nominal),
                    monotype,
                );
            },
            .record => |record| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (flatRecordRepresentsEmpty(store_types, record)) return;
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "Pipeline invariant violated: non-empty store record matched unit monotype (module={d}, monotype={d})",
                        .{
                            module_idx,
                            @intFromEnum(monotype),
                        },
                    );
                        }
                        unreachable;
                    },
                    else => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "Pipeline invariant violated: expected record monotype for store record, got {s} (module={d}, monotype={d})",
                                .{
                                    @tagName(mono),
                                    module_idx,
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
                    },
                };
                const mono_fields = result.context_mono.monotype_store.getFields(mrec.fields);
                var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
                defer seen_field_indices.deinit(self.allocator);

                var current_row = record;
                rows: while (true) {
                    const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);
                    for (field_names, field_vars) |field_name, field_var| {
                        const field_idx = self.recordFieldIndexByName(module_idx, field_name, module_idx, mono_fields);
                        try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                        try self.bindTypeVarMonotypesInStore(
                            result,
                            module_idx,
                            store_types,
                            common_idents,
                            bindings,
                            field_var,
                            mono_fields[field_idx].type_idx,
                        );
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
                                    const ext_names = ext_fields.items(.name);
                                    const ext_vars = ext_fields.items(.var_);
                                    for (ext_names, ext_vars) |field_name, field_var| {
                                        const field_idx = self.recordFieldIndexByName(module_idx, field_name, module_idx, mono_fields);
                                        if (seenIndex(seen_field_indices.items, field_idx)) continue;
                                        try self.bindTypeVarMonotypesInStore(
                                            result,
                                            module_idx,
                                            store_types,
                                            common_idents,
                                            bindings,
                                            field_var,
                                            mono_fields[field_idx].type_idx,
                                        );
                                    }
                                    return;
                                },
                                .empty_record => return,
                                else => unreachable,
                            },
                            .flex, .rigid, .err => return,
                        }
                    }
                }
            },
            .record_unbound => |fields_range| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (store_types.getRecordFieldsSlice(fields_range).len == 0) return;
                        unreachable;
                    },
                    else => unreachable,
                };
                const mono_fields = result.context_mono.monotype_store.getFields(mrec.fields);
                const fields = store_types.getRecordFieldsSlice(fields_range);
                for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByName(module_idx, field_name, module_idx, mono_fields);
                    try self.bindTypeVarMonotypesInStore(
                        result,
                        module_idx,
                        store_types,
                        common_idents,
                        bindings,
                        field_var,
                        mono_fields[field_idx].type_idx,
                    );
                }
            },
            .tuple => |tuple| {
                const mtup = switch (mono) {
                    .tuple => |mtup| mtup,
                    else => unreachable,
                };
                const mono_elems = result.context_mono.monotype_store.getIdxSpan(mtup.elems);
                const elem_vars = store_types.sliceVars(tuple.elems);
                if (mono_elems.len != elem_vars.len) unreachable;
                for (elem_vars, mono_elems) |elem_var, elem_mono| {
                    try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, elem_var, elem_mono);
                }
            },
            .tag_union => |tag_union| {
                const mtag = switch (mono) {
                    .tag_union => |mtag| mtag,
                    else => unreachable,
                };
                const mono_tags = result.context_mono.monotype_store.getTags(mtag.tags);
                const tags = store_types.getTagsSlice(tag_union.tags);
                const tag_args = tags.items(.args);
                if (tag_args.len != mono_tags.len) unreachable;
                for (tag_args, mono_tags) |args_range, mono_tag| {
                    const payload_vars = store_types.sliceVars(args_range);
                    const mono_payloads = result.context_mono.monotype_store.getIdxSpan(mono_tag.payloads);
                    if (payload_vars.len != mono_payloads.len) unreachable;
                    for (payload_vars, mono_payloads) |payload_var, payload_mono| {
                        try self.bindTypeVarMonotypesInStore(result, module_idx, store_types, common_idents, bindings, payload_var, payload_mono);
                    }
                }
            },
            .empty_record => switch (mono) {
                .unit, .record => {},
                else => unreachable,
            },
            .empty_tag_union => switch (mono) {
                .tag_union => {},
                else => unreachable,
            },
        }
    }

    fn resolveStrInspectHelperCallableInstsForTypeVar(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        type_var: types.Var,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer visiting.deinit(self.allocator);
        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, type_var, &visiting);
    }

    fn resolveStrInspectHelperCallableInstsForTypeVarWithSeen(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        type_var: types.Var,
        visiting: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

        var resolved = module_env.types.resolveVar(type_var);
        while (resolved.desc.content == .alias) {
            resolved = module_env.types.resolveVar(module_env.types.getAliasBackingVar(resolved.desc.content.alias));
        }

        if (visiting.contains(resolved.var_)) return;
        try visiting.put(self.allocator, resolved.var_, {});
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
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, type_args[0], visiting);
                            }
                            return;
                        }
                        if (ident.eql(common.box)) {
                            const type_args = module_env.types.sliceNominalArgs(nominal);
                            const outer_mono = try ContextMono.resolveTypeVarMonotype(self, result, thread, module_idx, resolved.var_);
                            const outer_box = result.context_mono.monotype_store.getMonotype(outer_mono).box;
                            try self.ensureBuiltinBoxUnboxCallableInst(result, module_idx, outer_mono, outer_box.inner);
                            if (type_args.len == 1) {
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, type_args[0], visiting);
                            }
                            return;
                        }
                    }

                    if (try DispatchSolved.lookupAssociatedMethodTemplate(self, result, module_idx, nominal, module_env.idents.to_inspect)) |method_info| {
                        if (ContextMono.resolveFuncTypeInStore(&method_info.target_env.types, method_info.type_var)) |resolved_func| {
                            if (!resolved_func.effectful) {
                                const param_vars = method_info.target_env.types.sliceVars(resolved_func.func.args);
                                if (param_vars.len == 1) {
                                    var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
                                    defer bindings.deinit();
                                    var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
                                    defer ordered_entries.deinit(self.allocator);

                                    const arg_mono = try ContextMono.resolveTypeVarMonotype(self, result, thread, module_idx, resolved.var_);
                                    try self.bindTypeVarMonotypes(
                                        result,
                                        method_info.module_idx,
                                        &method_info.target_env.types,
                                        &bindings,
                                        &ordered_entries,
                                        param_vars[0],
                                        arg_mono,
                                        module_idx,
                                    );

                                    const method_func_mono = try ContextMono.resolveTypeVarMonotypeWithBindings(
                                        self,
                                        result,
                                        method_info.module_idx,
                                        &method_info.target_env.types,
                                        method_info.type_var,
                                        &bindings,
                                    );
                                    if (!method_func_mono.isNone()) {
                                        _ = try self.requireCallableInst(
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
                                        if (!(ret_mono == .prim and ret_mono.prim == .str)) {
                                            try self.resolveStrInspectHelperCallableInstsForMonotype(
                                                result,
                                                method_info.module_idx,
                                                method_func.ret,
                                            );
                                        }
                                    }
                                    return;
                                }
                            }
                        }
                    }

                    try self.resolveStrInspectHelperCallableInstsForMonotype(
                        result,
                        module_idx,
                        try ContextMono.resolveTypeVarMonotype(self, result, thread, module_idx, resolved.var_),
                    );
                    return;
                },
                .record => |record| {
                    try self.resolveStrInspectHelperCallableInstsForRecordType(result, thread, module_idx, &module_env.types, record, visiting);
                    return;
                },
                .record_unbound => |fields_range| {
                    const fields = module_env.types.getRecordFieldsSlice(fields_range);
                    for (fields.items(.var_)) |field_var| {
                        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, field_var, visiting);
                    }
                    return;
                },
                .tuple => |tuple| {
                    for (module_env.types.sliceVars(tuple.elems)) |elem_var| {
                        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, elem_var, visiting);
                    }
                    return;
                },
                .tag_union => |tag_union| {
                    try self.resolveStrInspectHelperCallableInstsForTagUnionType(result, thread, module_idx, &module_env.types, tag_union, visiting);
                    return;
                },
                .empty_record, .empty_tag_union => return,
                else => {},
            }
        }

        try self.resolveStrInspectHelperCallableInstsForMonotype(
            result,
            module_idx,
            try ContextMono.resolveTypeVarMonotype(self, result, thread, module_idx, resolved.var_),
        );
    }

    fn resolveStrInspectHelperCallableInstsForRecordType(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        store_types: *const types.Store,
        record: types.Record,
        visiting: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!void {
        var current_row = record;

        rows: while (true) {
            const fields = store_types.getRecordFieldsSlice(current_row.fields);
            for (fields.items(.var_)) |field_var| {
                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, field_var, visiting);
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
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, field_var, visiting);
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
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        store_types: *const types.Store,
        tag_union: types.TagUnion,
        visiting: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!void {
        var current_row = tag_union;

        rows: while (true) {
            const tags = store_types.getTagsSlice(current_row.tags);
            for (tags.items(.args)) |args_range| {
                for (store_types.sliceVars(args_range)) |payload_var| {
                    try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, payload_var, visiting);
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
        self: *Pass,
        result: *Result,
        module_idx: u32,
        monotype: Monotype.Idx,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        switch (result.context_mono.monotype_store.getMonotype(monotype)) {
            .unit, .prim => {},
            .list => |list_mono| try self.resolveStrInspectHelperCallableInstsForMonotype(result, module_idx, list_mono.elem),
            .box => |box_mono| {
                try self.ensureBuiltinBoxUnboxCallableInst(result, module_idx, monotype, box_mono.inner);
                try self.resolveStrInspectHelperCallableInstsForMonotype(result, module_idx, box_mono.inner);
            },
            .tuple => |tuple_mono| {
                var elem_i: usize = 0;
                while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                    const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                    try self.resolveStrInspectHelperCallableInstsForMonotype(result, module_idx, elem_mono);
                }
            },
            .func => {},
            .record => |record_mono| {
                var field_i: usize = 0;
                while (field_i < record_mono.fields.len) : (field_i += 1) {
                    const field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, field_i);
                    try self.resolveStrInspectHelperCallableInstsForMonotype(result, module_idx, field.type_idx);
                }
            },
            .tag_union => |tag_union_mono| {
                var tag_i: usize = 0;
                while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                    const tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                    var payload_i: usize = 0;
                    while (payload_i < tag.payloads.len) : (payload_i += 1) {
                        const payload_mono = result.context_mono.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                        try self.resolveStrInspectHelperCallableInstsForMonotype(result, module_idx, payload_mono);
                    }
                }
            },
            .recursive_placeholder => unreachable,
        }
    }

    fn requireCallableInst(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!CallableInstId {
        const ensured = try self.ensureCallableInstRecord(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            &.{},
        );
        try Lambdamono.ensureCallableInstRealized(self, result, ensured.id);
        return ensured.id;
    }

    fn requireCallableInstWithCallableParamSpecs(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!CallableInstId {
        const ensured = try self.ensureCallableInstRecord(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            callable_param_specs,
        );
        try Lambdamono.ensureCallableInstRealized(self, result, ensured.id);
        return ensured.id;
    }

    const EnsuredCallableInst = struct {
        id: CallableInstId,
        created: bool,
    };

    fn ensureCallableInstRecord(
        self: *Pass,
        result: *Result,
        active_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!EnsuredCallableInst {
        const template = result.getCallableTemplate(template_id);
        const defining_source_context = self.resolveTemplateDefiningSourceContext(
            result,
            active_source_context,
            template.*,
        );
        const canonical_fn_monotype = if (fn_monotype_module_idx == template.module_idx)
            fn_monotype
        else
            try ContextMono.remapMonotypeBetweenModules(self, result,
                fn_monotype,
                fn_monotype_module_idx,
                template.module_idx,
            );
        const canonical_fn_monotype_module_idx = template.module_idx;
        const subst_id = if (self.all_module_envs[template.module_idx].types.needsInstantiation(template.type_root))
            try self.ensureTypeSubst(result, template.*, canonical_fn_monotype, canonical_fn_monotype_module_idx)
        else
            result.context_mono.getEmptyTypeSubstId();

        for (result.lambdamono.callable_insts.items, 0..) |existing_callable_inst, idx| {
            if (existing_callable_inst.template != template_id) continue;
            if (existing_callable_inst.fn_monotype_module_idx != canonical_fn_monotype_module_idx) continue;
            if (!std.meta.eql(existing_callable_inst.defining_source_context, defining_source_context)) continue;
            if (!callableParamSpecsEqual(
                result,
                result.lambdamono.getCallableParamSpecEntries(existing_callable_inst.callable_param_specs),
                callable_param_specs,
            )) continue;
            const mono_equal = try ContextMono.monotypesStructurallyEqual(
                self,
                result,
                existing_callable_inst.fn_monotype,
                canonical_fn_monotype,
            );
            if (mono_equal) {
                if (existing_callable_inst.subst != subst_id) continue;
                const existing_id: CallableInstId = @enumFromInt(idx);
                try self.ensureDirectCallableVariantGroup(result, existing_id);
                return .{ .id = existing_id, .created = false };
            }
        }

        const runtime_kind: Lambdamono.CallableRuntimeKind = switch (template.kind) {
            .lambda => .lambda,
            .closure => .closure,
            .hosted_lambda => .hosted_lambda,
            .top_level_def => std.debug.panic(
                "Pipeline invariant violated: callable template {d} runtime expr {d} in module {d} had top_level_def kind at executable instantiation time",
                .{ @intFromEnum(template_id), @intFromEnum(template.runtime_expr), template.module_idx },
            ),
        };
        const callable_inst_id: CallableInstId = @enumFromInt(result.lambdamono.callable_insts.items.len);
        const callable_def_id: CallableDefId = @enumFromInt(result.lambdamono.callable_defs.items.len);
        try result.lambdamono.callable_defs.append(self.allocator, .{
            .module_idx = template.module_idx,
            .runtime_kind = runtime_kind,
            .arg_patterns = try self.appendProgramPatternEntries(
                &result.lambdamono,
                self.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns),
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
            .fn_monotype = ContextMono.resolvedMonotype(canonical_fn_monotype, canonical_fn_monotype_module_idx),
            .captures = .empty(),
            .source_region = template.source_region,
        });
        try self.appendExact(&result.lambdamono.callable_insts, CallableInst{
            .template = template_id,
            .subst = subst_id,
            .fn_monotype = canonical_fn_monotype,
            .fn_monotype_module_idx = canonical_fn_monotype_module_idx,
            .defining_source_context = defining_source_context,
            .callable_def = callable_def_id,
            .runtime_value = .direct_lambda,
            .callable_param_specs = try self.addCallableParamSpecEntries(result, callable_param_specs),
        });
        try self.ensureDirectCallableVariantGroup(result, callable_inst_id);
        return .{ .id = callable_inst_id, .created = true };
    }

    fn addCallableParamSpecEntries(
        self: *Pass,
        result: *Result,
        entries: []const CallableParamSpecEntry,
    ) Allocator.Error!CallableParamSpecSpan {
        if (entries.len == 0) return .empty();

        const start: u32 = @intCast(result.lambdamono.callable_param_spec_entries.items.len);
        try result.lambdamono.callable_param_spec_entries.appendSlice(self.allocator, entries);
        return .{
            .start = start,
            .len = @intCast(entries.len),
        };
    }

    fn appendProgramPatternEntries(
        self: *Pass,
        program: *Lambdamono.Program,
        pattern_ids: []const CIR.Pattern.Idx,
    ) Allocator.Error!Lambdamono.PatternIdSpan {
        const start: u32 = @intCast(program.pattern_entries.items.len);
        try program.pattern_entries.appendSlice(self.allocator, pattern_ids);
        return .{
            .start = start,
            .len = @intCast(pattern_ids.len),
        };
    }

    fn addCallableParamProjectionEntries(
        self: *Pass,
        result: *Result,
        entries: []const CallableParamProjection,
    ) Allocator.Error!CallableParamProjectionSpan {
        if (entries.len == 0) return .empty();

        const start: u32 = @intCast(result.lambdamono.value_projection_entries.items.len);
        try result.lambdamono.value_projection_entries.appendSlice(self.allocator, entries);
        return .{
            .start = start,
            .len = @intCast(entries.len),
        };
    }

    fn appendValueProjectionEntries(
        self: *Pass,
        result: *Result,
        existing: ValueProjectionSpan,
        appended: []const CallableParamProjection,
    ) Allocator.Error!ValueProjectionSpan {
        if (existing.isEmpty() and appended.len == 0) return .empty();

        var combined = std.ArrayListUnmanaged(CallableParamProjection).empty;
        defer combined.deinit(self.allocator);

        try combined.appendSlice(
            self.allocator,
            result.lambdamono.getValueProjectionEntries(existing),
        );
        try combined.appendSlice(self.allocator, appended);
        return try self.addCallableParamProjectionEntries(result, combined.items);
    }

    fn ensureDirectCallableVariantGroup(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        try result.lambdamono.ensureDirectCallableVariantGroup(self.allocator, callable_inst_id);
    }

    fn appendCallableParamSpecEntry(
        self: *Pass,
        result: *Result,
        out: *std.ArrayListUnmanaged(CallableParamSpecEntry),
        entry: CallableParamSpecEntry,
    ) Allocator.Error!void {
        for (out.items) |existing| {
            if (existing.param_index != entry.param_index) continue;
            if (!std.meta.eql(existing.callable_value, entry.callable_value)) continue;
            if (!callableParamProjectionsEqual(
                result.lambdamono.getCallableParamProjectionEntries(existing.projections),
                result.lambdamono.getCallableParamProjectionEntries(entry.projections),
            )) continue;
            return;
        }
        try out.append(self.allocator, entry);
    }

    fn resolveTemplateDefiningSourceContext(
        _: *Pass,
        result: *const Result,
        active_source_context: SourceContext,
        template: CallableTemplate,
    ) SourceContext {
        switch (template.kind) {
            .top_level_def => return .{ .template_expr = .{
                .module_idx = template.module_idx,
                .expr_idx = template.cir_expr,
            } },
            .lambda, .hosted_lambda, .closure => {},
        }

        switch (template.owner) {
            .root_scope => {
                var current = active_source_context;
                while (true) switch (current) {
                    .callable_inst => |context_id| {
                        current = result.getCallableInst(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))).defining_source_context;
                    },
                    .root_expr, .provenance_expr => return current,
                    .template_expr => {
                        std.debug.panic(
                            "Pipeline invariant violated: executable template expr={d} in module {d} reached callable inst creation from template_expr context",
                            .{ @intFromEnum(template.cir_expr), template.module_idx },
                        );
                    },
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
                                "Pipeline: executable template expr={d} requires lexical owner template={d} but active source context was {s}",
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
                    current = sourceContextCallableInst(current_callable_inst.defining_source_context) orelse break;
                }

                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: executable template expr={d} could not resolve lexical owner template={d} from active callable inst={d}",
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
        lhs: []const CallableParamProjection,
        rhs: []const CallableParamProjection,
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
        result: *const Result,
        lhs: []const CallableParamSpecEntry,
        rhs: []const CallableParamSpecEntry,
    ) bool {
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_entry, rhs_entry| {
            if (lhs_entry.param_index != rhs_entry.param_index) return false;
            if (!callableParamProjectionsEqual(
                result.lambdamono.getCallableParamProjectionEntries(lhs_entry.projections),
                result.lambdamono.getCallableParamProjectionEntries(rhs_entry.projections),
            )) return false;
            if (!std.meta.eql(lhs_entry.callable_value, rhs_entry.callable_value)) return false;
        }
        return true;
    }

    fn ensureTypeSubst(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!TypeSubstId {
        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            &self.all_module_envs[template.module_idx].types,
            &bindings,
            &ordered_entries,
            template.type_root,
            fn_monotype,
            fn_monotype_module_idx,
        );
        for (result.context_mono.substs.items, 0..) |existing_subst, idx| {
            if (try self.typeSubstEntriesEqual(
                result,
                result.getTypeSubstEntries(existing_subst.entries),
                ordered_entries.items,
            )) {
                return @enumFromInt(idx);
            }
        }

        const entries_span: TypeSubstSpan = if (ordered_entries.items.len == 0)
            TypeSubstSpan.empty()
        else blk: {
            const start: u32 = @intCast(result.context_mono.subst_entries.items.len);
            try result.context_mono.subst_entries.appendSlice(self.allocator, ordered_entries.items);
            break :blk TypeSubstSpan{
                .start = start,
                .len = @as(u16, @intCast(ordered_entries.items.len)),
            };
        };

        const subst_id: TypeSubstId = @enumFromInt(result.context_mono.substs.items.len);
        try self.appendExact(&result.context_mono.substs, TypeSubst{ .entries = entries_span });
        return subst_id;
    }

    fn typeSubstEntriesEqual(
        self: *Pass,
        result: *const Result,
        lhs: []const TypeSubstEntry,
        rhs: []const TypeSubstEntry,
    ) Allocator.Error!bool {
        if (lhs.len != rhs.len) return false;

        for (lhs, rhs) |lhs_entry, rhs_entry| {
            if (!std.meta.eql(lhs_entry.key, rhs_entry.key)) return false;
            if (lhs_entry.monotype.module_idx != rhs_entry.monotype.module_idx) return false;
            if (!try ContextMono.monotypesStructurallyEqual(self, result, lhs_entry.monotype.idx, rhs_entry.monotype.idx)) {
                return false;
            }
        }

        return true;
    }

    fn labelTextAcrossModules(self: *Pass, module_idx: u32, label: anytype) []const u8 {
        return ContextMono.labelTextAcrossModules(self.all_module_envs, module_idx, label);
    }

    fn identsStructurallyEqualAcrossModules(
        self: *Pass,
        lhs_module_idx: u32,
        lhs: anytype,
        rhs_module_idx: u32,
        rhs: anytype,
    ) bool {
        return ContextMono.identsStructurallyEqualAcrossModules(
            self.all_module_envs,
            lhs_module_idx,
            lhs,
            rhs_module_idx,
            rhs,
        );
    }

    fn recordFieldIndexByName(
        self: *Pass,
        template_module_idx: u32,
        field_name: base.Ident.Idx,
        mono_module_idx: u32,
        mono_fields: []const Monotype.Field,
    ) u32 {
        for (mono_fields, 0..) |mono_field, field_idx| {
            if (self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                field_name,
                mono_module_idx,
                mono_field.name,
            )) {
                return @intCast(field_idx);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
                .{ self.all_module_envs[template_module_idx].getIdent(field_name), template_module_idx, mono_module_idx },
            );
        }
        unreachable;
    }

    fn recordFieldIndexByNameInSpan(
        self: *Pass,
        result: *const Result,
        template_module_idx: u32,
        field_name: base.Ident.Idx,
        mono_module_idx: u32,
        mono_fields: Monotype.FieldSpan,
    ) u32 {
        var field_i: usize = 0;
        while (field_i < mono_fields.len) : (field_i += 1) {
            const mono_field = result.context_mono.monotype_store.getFieldItem(mono_fields, field_i);
            if (self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                field_name,
                mono_module_idx,
                mono_field.name,
            )) {
                return @intCast(field_i);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
                .{ self.all_module_envs[template_module_idx].getIdent(field_name), template_module_idx, mono_module_idx },
            );
        }
        unreachable;
    }

    fn tagIndexByNameInSpan(
        self: *Pass,
        result: *const Result,
        template_module_idx: u32,
        tag_name: base.Ident.Idx,
        mono_module_idx: u32,
        mono_tags: Monotype.TagSpan,
    ) u32 {
        var tag_i: usize = 0;
        while (tag_i < mono_tags.len) : (tag_i += 1) {
            const mono_tag = result.context_mono.monotype_store.getTagItem(mono_tags, tag_i);
            if (self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) {
                return @intCast(tag_i);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: tag '{s}' missing from monotype",
                .{self.all_module_envs[template_module_idx].getIdent(tag_name)},
            );
        }
        unreachable;
    }

    fn seenIndex(seen_indices: []const u32, idx: u32) bool {
        for (seen_indices) |seen_idx| {
            if (seen_idx == idx) return true;
        }
        return false;
    }

    fn containsU16(values: []const u16, target: u16) bool {
        for (values) |value| {
            if (value == target) return true;
        }
        return false;
    }

    fn appendUniqueU16(
        allocator: Allocator,
        values: *std.ArrayListUnmanaged(u16),
        target: u16,
    ) Allocator.Error!void {
        if (containsU16(values.items, target)) return;
        try values.append(allocator, target);
    }

    fn appendSeenIndex(
        allocator: Allocator,
        seen_indices: *std.ArrayListUnmanaged(u32),
        idx: u32,
    ) Allocator.Error!void {
        if (seenIndex(seen_indices.items, idx)) return;
        try seen_indices.append(allocator, idx);
    }

    fn remainingRecordTailMonotype(
        self: *Pass,
        result: *Result,
        mono_fields: []const Monotype.Field,
        seen_indices: []const u32,
    ) Allocator.Error!Monotype.Idx {
        var remaining_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
        defer remaining_fields.deinit(self.allocator);

        for (mono_fields, 0..) |field, field_idx| {
            if (seenIndex(seen_indices, @intCast(field_idx))) continue;
            try remaining_fields.append(self.allocator, field);
        }

        if (remaining_fields.items.len == 0) {
            return result.context_mono.monotype_store.unit_idx;
        }

        const field_span = try result.context_mono.monotype_store.addFields(self.allocator, remaining_fields.items);
        return try result.context_mono.monotype_store.addMonotype(self.allocator, .{ .record = .{ .fields = field_span } });
    }

    fn remainingTagUnionTailMonotype(
        self: *Pass,
        result: *Result,
        mono_tags: []const Monotype.Tag,
        seen_indices: []const u32,
    ) Allocator.Error!Monotype.Idx {
        var remaining_tags: std.ArrayListUnmanaged(Monotype.Tag) = .empty;
        defer remaining_tags.deinit(self.allocator);

        for (mono_tags, 0..) |tag, tag_idx| {
            if (seenIndex(seen_indices, @intCast(tag_idx))) continue;
            try remaining_tags.append(self.allocator, tag);
        }

        const tag_span = try result.context_mono.monotype_store.addTags(self.allocator, remaining_tags.items);
        return try result.context_mono.monotype_store.addMonotype(self.allocator, .{ .tag_union = .{ .tags = tag_span } });
    }

    fn bindRecordRowTail(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        ext_var: types.Var,
        mono_fields: []const Monotype.Field,
        seen_indices: []const u32,
        mono_module_idx: u32,
        comptime allow_failure: bool,
    ) Allocator.Error!bool {
        const tail_mono = try self.remainingRecordTailMonotype(result, mono_fields, seen_indices);
        return self.bindTypeVarMonotypesMode(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            ext_var,
            tail_mono,
            mono_module_idx,
            allow_failure,
        );
    }

    fn bindTagUnionRowTail(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        ext_var: types.Var,
        mono_tags: []const Monotype.Tag,
        seen_indices: []const u32,
        mono_module_idx: u32,
        comptime allow_failure: bool,
    ) Allocator.Error!bool {
        const tail_mono = try self.remainingTagUnionTailMonotype(result, mono_tags, seen_indices);
        return self.bindTypeVarMonotypesMode(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            ext_var,
            tail_mono,
            mono_module_idx,
            allow_failure,
        );
    }

    fn bindTagPayloadsByName(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        tag_name: base.Ident.Idx,
        payload_vars: []const types.Var,
        mono_tags: Monotype.TagSpan,
        mono_module_idx: u32,
        comptime allow_failure: bool,
    ) Allocator.Error!bool {
        var tag_i: usize = 0;
        while (tag_i < mono_tags.len) : (tag_i += 1) {
            const mono_tag = result.context_mono.monotype_store.getTagItem(mono_tags, tag_i);
            if (!self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) continue;

            if (payload_vars.len != mono_tag.payloads.len) {
                if (allow_failure) return false;
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: payload arity mismatch for tag '{s}'",
                        .{self.all_module_envs[template_module_idx].getIdent(tag_name)},
                    );
                }
                unreachable;
            }

            for (payload_vars, 0..) |payload_var, i| {
                const mono_payload = result.context_mono.monotype_store.getIdxSpanItem(mono_tag.payloads, i);
                if (!try self.bindTypeVarMonotypesMode(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    payload_var,
                    mono_payload,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            }
            return true;
        }

        if (allow_failure) return false;
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: tag '{s}' missing from monotype",
                .{self.all_module_envs[template_module_idx].getIdent(tag_name)},
            );
        }
        unreachable;
    }

    fn bindTypeVarMonotypes(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        type_var: types.Var,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
    ) Allocator.Error!void {
        const ok = try self.bindTypeVarMonotypesMode(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            type_var,
            monotype,
            mono_module_idx,
            false,
        );
        if (!ok) unreachable;
    }

    fn tryBindTypeVarMonotypes(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        type_var: types.Var,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
    ) Allocator.Error!bool {
        return self.bindTypeVarMonotypesMode(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            type_var,
            monotype,
            mono_module_idx,
            true,
        );
    }

    fn bindTypeVarMonotypesMode(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        type_var: types.Var,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
        comptime allow_failure: bool,
    ) Allocator.Error!bool {
        if (monotype.isNone()) return true;
        const normalized_mono = if (mono_module_idx == template_module_idx)
            monotype
        else
            try ContextMono.remapMonotypeBetweenModules(self, result, monotype, mono_module_idx, template_module_idx);
        const resolved_mono = ContextMono.resolvedMonotype(normalized_mono, template_module_idx);

        const resolved_key = boundTypeVarKey(template_module_idx, template_types, type_var);
        if (bindings.get(resolved_key)) |existing| {
            if (existing.module_idx != resolved_mono.module_idx or
                !try ContextMono.monotypesStructurallyEqual(self, result, existing.idx, resolved_mono.idx))
            {
                if (allow_failure) return false;
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: conflicting monotype binding for type var root {d} in module {d} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any}",
                        .{
                            @intFromEnum(resolved_key.type_var),
                            resolved_key.module_idx,
                            @intFromEnum(existing.idx),
                            existing.module_idx,
                            result.context_mono.monotype_store.getMonotype(existing.idx),
                            @intFromEnum(resolved_mono.idx),
                            resolved_mono.module_idx,
                            result.context_mono.monotype_store.getMonotype(resolved_mono.idx),
                        },
                    );
                }
                unreachable;
            }
            return true;
        }

        const resolved = template_types.resolveVar(type_var);

        switch (resolved.desc.content) {
            .flex, .rigid => {
                try bindings.put(resolved_key, resolved_mono);
                try ordered_entries.append(self.allocator, .{
                    .key = resolved_key,
                    .monotype = resolved_mono,
                });
            },
            .alias => |alias| {
                if (!try self.bindTypeVarMonotypesMode(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    template_types.getAliasBackingVar(alias),
                    normalized_mono,
                    template_module_idx,
                    allow_failure,
                )) return false;
            },
            .structure => |flat_type| {
                try bindings.put(resolved_key, resolved_mono);
                try ordered_entries.append(self.allocator, .{
                    .key = resolved_key,
                    .monotype = resolved_mono,
                });
                if (!try self.bindFlatTypeMonotypesMode(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    flat_type,
                    normalized_mono,
                    template_module_idx,
                    allow_failure,
                )) return false;
            },
            .err => {},
        }

        return true;
    }

    fn bindFlatTypeMonotypes(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        flat_type: types.FlatType,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
    ) Allocator.Error!void {
        const ok = try self.bindFlatTypeMonotypesMode(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            flat_type,
            monotype,
            mono_module_idx,
            false,
        );
        if (!ok) unreachable;
    }

    fn bindFlatTypeMonotypesMode(
        self: *Pass,
        result: *Result,
        template_module_idx: u32,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        flat_type: types.FlatType,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
        comptime allow_failure: bool,
    ) Allocator.Error!bool {
        if (monotype.isNone()) return true;

        const mono = result.context_mono.monotype_store.getMonotype(monotype);
        const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[template_module_idx].common);

        switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const mfunc = switch (mono) {
                    .func => |mfunc| mfunc,
                    else => {
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };

                const type_args = template_types.sliceVars(func.args);
                if (type_args.len != mfunc.args.len) unreachable;

                for (type_args, 0..) |arg_var, i| {
                    const mono_arg = result.context_mono.monotype_store.getIdxSpanItem(mfunc.args, i);
                    if (!try self.bindTypeVarMonotypesMode(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        arg_var,
                        mono_arg,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                }
                if (!try self.bindTypeVarMonotypesMode(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    func.ret,
                    mfunc.ret,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            },
            .nominal_type => |nominal| {
                const ident = nominal.ident.ident_idx;
                const origin = nominal.origin_module;

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                    const mlist = switch (mono) {
                        .list => |mlist| mlist,
                        else => {
                            return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        },
                    };
                    const type_args = template_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    if (!try self.bindTypeVarMonotypesMode(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        type_args[0],
                        mlist.elem,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                    return true;
                }

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                    const mbox = switch (mono) {
                        .box => |mbox| mbox,
                        else => {
                            return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        },
                    };
                    const type_args = template_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    if (!try self.bindTypeVarMonotypesMode(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        type_args[0],
                        mbox.inner,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                    return true;
                }

                if (origin.eql(common_idents.builtin_module) and DispatchSolved.builtinPrimForNominal(ident, common_idents) != null) {
                    switch (mono) {
                        .prim => {},
                        else => {
                            return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        },
                    }
                    return true;
                }

                if (!try self.bindTypeVarMonotypesMode(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    template_types.getNominalBackingVar(nominal),
                    monotype,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            },
            .record => |record| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (flatRecordRepresentsEmpty(template_types, record)) return true;
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                    else => {
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
                defer seen_field_indices.deinit(self.allocator);

                var current_row = record;
                rows: while (true) {
                    const fields_slice = template_types.getRecordFieldsSlice(current_row.fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);
                    for (field_names, field_vars) |field_name, field_var| {
                        const field_idx = self.recordFieldIndexByNameInSpan(
                            result,
                            template_module_idx,
                            field_name,
                            mono_module_idx,
                            mrec.fields,
                        );
                        try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                        const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                        if (!try self.bindTypeVarMonotypesMode(
                            result,
                            template_module_idx,
                            template_types,
                            bindings,
                            ordered_entries,
                            field_var,
                            mono_field.type_idx,
                            mono_module_idx,
                            allow_failure,
                        )) return false;
                    }

                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = template_types.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = template_types.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .record => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .record_unbound => |fields_range| {
                                    const ext_fields = template_types.getRecordFieldsSlice(fields_range);
                                    const ext_names = ext_fields.items(.name);
                                    const ext_vars = ext_fields.items(.var_);
                                    for (ext_names, ext_vars) |field_name, field_var| {
                                        const field_idx = self.recordFieldIndexByNameInSpan(
                                            result,
                                            template_module_idx,
                                            field_name,
                                            mono_module_idx,
                                            mrec.fields,
                                        );
                                        try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                                        const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                                        if (!try self.bindTypeVarMonotypesMode(
                                            result,
                                            template_module_idx,
                                            template_types,
                                            bindings,
                                            ordered_entries,
                                            field_var,
                                            mono_field.type_idx,
                                            mono_module_idx,
                                            allow_failure,
                                        )) return false;
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => {
                                    return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                                },
                            },
                            .flex, .rigid => {
                                if (!try self.bindRecordRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    result.context_mono.monotype_store.getFields(mrec.fields),
                                    seen_field_indices.items,
                                    mono_module_idx,
                                    allow_failure,
                                )) return false;
                                break :rows;
                            },
                            .err => {
                                return self.bindFlatTypeErrorTail(allow_failure, flat_type, template_module_idx, mono_module_idx, monotype);
                            },
                        }
                    }
                    return true;
                }
            },
            .record_unbound => |fields_range| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (template_types.getRecordFieldsSlice(fields_range).len == 0) return true;
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                    else => {
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                const fields_slice = template_types.getRecordFieldsSlice(fields_range);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);
                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByNameInSpan(
                        result,
                        template_module_idx,
                        field_name,
                        mono_module_idx,
                        mrec.fields,
                    );
                    const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                    if (!try self.bindTypeVarMonotypesMode(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        field_var,
                        mono_field.type_idx,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                }
            },
            .tuple => |tuple| {
                const mtup = switch (mono) {
                    .tuple => |mtup| mtup,
                    else => {
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                const elem_vars = template_types.sliceVars(tuple.elems);
                if (elem_vars.len != mtup.elems.len) unreachable;
                for (elem_vars, 0..) |elem_var, i| {
                    const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(mtup.elems, i);
                    if (!try self.bindTypeVarMonotypesMode(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        elem_var,
                        elem_mono,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                }
            },
            .tag_union => |tag_union| {
                const mtag = switch (mono) {
                    .tag_union => |mtag| mtag,
                    else => {
                        return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
                defer seen_tag_indices.deinit(self.allocator);

                var current_row = tag_union;
                rows: while (true) {
                    const type_tags = template_types.getTagsSlice(current_row.tags);
                    const tag_names = type_tags.items(.name);
                    const tag_args = type_tags.items(.args);
                    for (tag_names, tag_args) |tag_name, args_range| {
                        const tag_idx = self.tagIndexByNameInSpan(
                            result,
                            template_module_idx,
                            tag_name,
                            mono_module_idx,
                            mtag.tags,
                        );
                        try appendSeenIndex(self.allocator, &seen_tag_indices, tag_idx);
                        if (!try self.bindTagPayloadsByName(
                            result,
                            template_module_idx,
                            template_types,
                            bindings,
                            ordered_entries,
                            tag_name,
                            template_types.sliceVars(args_range),
                            mtag.tags,
                            mono_module_idx,
                            allow_failure,
                        )) return false;
                    }

                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = template_types.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = template_types.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .tag_union => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .empty_tag_union => break :rows,
                                else => {
                                    return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                                },
                            },
                            .flex, .rigid => {
                                if (!try self.bindTagUnionRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    result.context_mono.monotype_store.getTags(mtag.tags),
                                    seen_tag_indices.items,
                                    mono_module_idx,
                                    allow_failure,
                                )) return false;
                                break :rows;
                            },
                            .err => {
                                return self.bindFlatTypeErrorTail(allow_failure, flat_type, template_module_idx, mono_module_idx, monotype);
                            },
                        }
                    }
                }
            },
            .empty_record => switch (mono) {
                .unit, .record => {},
                else => {
                    return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            },
            .empty_tag_union => switch (mono) {
                .tag_union => {},
                else => {
                    return self.bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            },
        }

        return true;
    }

    fn bindFlatTypeMismatch(
        _: *Pass,
        comptime allow_failure: bool,
        flat_type: types.FlatType,
        mono: Monotype.Monotype,
        template_module_idx: u32,
        mono_module_idx: u32,
        monotype: Monotype.Idx,
    ) bool {
        if (allow_failure) return false;
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline bindFlatTypeMonotypes mismatch: flat_type={s} mono={s} template_module={d} mono_module={d} monotype={d}",
                .{
                    @tagName(flat_type),
                    @tagName(mono),
                    template_module_idx,
                    mono_module_idx,
                    @intFromEnum(monotype),
                },
            );
        }
        unreachable;
    }

    fn bindFlatTypeErrorTail(
        _: *Pass,
        comptime allow_failure: bool,
        flat_type: types.FlatType,
        template_module_idx: u32,
        mono_module_idx: u32,
        monotype: Monotype.Idx,
    ) bool {
        if (allow_failure) return false;
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline bindFlatTypeMonotypes hit err tail: flat_type={s} template_module={d} mono_module={d} monotype={d}",
                .{
                    @tagName(flat_type),
                    template_module_idx,
                    mono_module_idx,
                    @intFromEnum(monotype),
                },
            );
        }
        unreachable;
    }

    fn boundTypeVarKey(
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
    ) BoundTypeVarKey {
        return .{
            .module_idx = module_idx,
            .type_var = store_types.resolveVar(var_).var_,
        };
    }

    fn resolveImportedModuleIdx(self: *Pass, caller_env: *const ModuleEnv, import_idx: CIR.Import.Idx) ?u32 {
        if (caller_env.imports.getResolvedModule(import_idx)) |module_idx| {
            if (module_idx < self.all_module_envs.len) return module_idx;
        }

        const import_pos = @intFromEnum(import_idx);
        if (import_pos >= caller_env.imports.imports.len()) return null;

        const import_name = caller_env.common.getString(caller_env.imports.imports.items.items[import_pos]);
        const base_name = identLastSegment(import_name);

        for (self.all_module_envs, 0..) |module_env_entry, module_idx| {
            if (std.mem.eql(u8, module_env_entry.module_name, import_name) or
                std.mem.eql(u8, module_env_entry.module_name, base_name))
            {
                @constCast(&caller_env.imports).setResolvedModule(import_idx, @intCast(module_idx));
                return @intCast(module_idx);
            }
        }

        return null;
    }

    fn findDefByPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        for (defs) |def_idx| {
            if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
        }
        return null;
    }
};


fn dotCallUsesRuntimeReceiver(module_env: *const ModuleEnv, receiver_expr_idx: CIR.Expr.Idx) bool {
    return switch (module_env.store.getExpr(receiver_expr_idx)) {
        .e_nominal, .e_nominal_external => false,
        else => true,
    };
}

fn dispatchMethodIdentForBinop(module_env: *const ModuleEnv, op: CIR.Expr.Binop.Op) ?Ident.Idx {
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

fn identMatchesMethodName(full_name: []const u8, method_name: []const u8) bool {
    if (std.mem.eql(u8, full_name, method_name)) return true;
    if (full_name.len <= method_name.len + 1) return false;
    const suffix_start = full_name.len - method_name.len;
    return full_name[suffix_start - 1] == '.' and std.mem.eql(u8, full_name[suffix_start..], method_name);
}

fn identLastSegment(ident: []const u8) []const u8 {
    const idx = std.mem.lastIndexOfScalar(u8, ident, '.') orelse return ident;
    return ident[idx + 1 ..];
}

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
    return pass.runRootSourceExpr(expr_idx);
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
    var result = try Result.init(allocator);
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
    try pass.runRootsInto(&result, &.{expr_idx});
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
    return pass.runRootSourceExprs(exprs);
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
    var result = try Result.init(allocator);
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
    try pass.runRootsInto(&result, exprs);
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
    return pass.runModule();
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
    var result = try Result.init(allocator);
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
    trace.log("primeAllModules start", .{});
    try Lambdasolved.seedAllModuleDefPatternOrigins(&pass, &result);
    try result.template_catalog.primeAllModules(allocator, all_module_envs);
    trace.log("primeAllModules done", .{});
    const root_exprs = result.template_catalog.getModuleRootExprs(current_module_idx);
    try Lambdasolved.analyzeRoots(
        allocator,
        current_module_idx,
        root_exprs,
        Lambdasolved.rootAnalysisDriver(&pass, &result),
    );
    trace.log("assembleLambdamono start", .{});
    try Lambdamono.assembleRoots(
        allocator,
        all_module_envs,
        current_module_idx,
        &result,
        Lambdamono.programAssemblyDriver(&pass),
        root_exprs,
    );
    trace.log("assembleLambdamono done", .{});
    return result;
}
