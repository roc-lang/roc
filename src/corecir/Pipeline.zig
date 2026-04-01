//! Staged CoreCIR callable pipeline.
//!
//! This module coordinates ContextMono, Lambdasolved, and Lambdamono for
//! reachable roots before MIR lowering begins. `Lower` must consume the result
//! of this stage; it must not specialize exact callables itself.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Monotype = @import("Monotype.zig");
const ContextMono = @import("ContextMono.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Lambdamono = @import("Lambdamono.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const CallableSourceNamespace = Lambdasolved.CallableSourceNamespace;
const packCallableSourceKey = Lambdasolved.packCallableSourceKey;
const packLocalPatternSourceKey = Lambdasolved.packLocalPatternSourceKey;
const packExternalDefSourceKey = Lambdasolved.packExternalDefSourceKey;
const packExprSourceKey = Lambdasolved.packExprSourceKey;

/// Identifies a semantic callable template before executable specialization.
pub const CallableTemplateId = Lambdasolved.CallableTemplateId;

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
pub const CallableTemplateKind = Lambdasolved.CallableTemplateKind;

/// A semantic callable body that can later be instantiated monomorphically.
pub const CallableTemplate = Lambdasolved.CallableTemplate;
pub const ExternalDefSource = Lambdasolved.ExternalDefSource;

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
pub const PackedFn = Lambdamono.PackedFn;
pub const IndirectCall = Lambdamono.IndirectCall;
pub const CallableValue = Lambdamono.CallableValue;
pub const CallSite = Lambdamono.CallSite;
pub const LookupResolution = Lambdamono.LookupResolution;
pub const ExprId = Lambdamono.ExprId;
pub const ExprRef = Lambdamono.ExprRef;

/// One statically resolved dispatch target definition.
pub const DispatchExprTarget = ContextMono.DispatchExprTarget;

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

fn callableSourceNamespace(source_key: u64) CallableSourceNamespace {
    return @enumFromInt(@as(u2, @intCast(source_key >> 62)));
}

fn callableSourceModuleIdx(source_key: u64) u32 {
    return @intCast((source_key >> 31) & std.math.maxInt(u31));
}

fn callableSourceLocalId(source_key: u64) u32 {
    return @intCast(source_key & std.math.maxInt(u31));
}

fn externalDefSourceFromSourceKey(source_key: u64) ?ExternalDefSource {
    if (callableSourceNamespace(source_key) != .external_def) return null;
    const def_node_idx = callableSourceLocalId(source_key);
    if (std.debug.runtime_safety and def_node_idx > std.math.maxInt(u16)) {
        std.debug.panic(
            "Pipeline: external def source key {d} encoded node idx {d} that does not fit in u16",
            .{ source_key, def_node_idx },
        );
    }
    return .{
        .module_idx = callableSourceModuleIdx(source_key),
        .def_idx = @enumFromInt(@as(u16, @intCast(def_node_idx))),
    };
}

const MutationKind = enum(u8) {
    callable_templates,
    callable_template_aliases,
    source_value_provenance,
    context_expr_monotypes,
    context_pattern_monotypes,
    context_type_var_monotypes,
    context_dispatch_targets,
    callable_variant_groups,
    callable_insts,
    substs,
};

const mutation_kind_count = std.meta.fields(MutationKind).len;

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
};

const AssociatedMethodTemplate = struct {
    target_env: *const ModuleEnv,
    module_idx: u32,
    template_id: CallableTemplateId,
    type_var: types.Var,
    qualified_method_ident: Ident.Idx,
};

const TypeScopeContext = struct {
    module_idx: u32,
    caller_module_idx: u32,
    scope: *const types.TypeScope,
};

const SemanticThread = struct {
    source_context: SourceContext,

    fn trackedThread(source_context: SourceContext) SemanticThread {
        return .{
            .source_context = source_context,
        };
    }

    fn requireSourceContext(self: SemanticThread) SourceContext {
        return self.source_context;
    }

    fn hasCallableInst(self: SemanticThread) bool {
        return sourceContextHasCallableInst(self.source_context);
    }

    fn callableInst(self: SemanticThread) ?CallableInstId {
        return sourceContextCallableInst(self.source_context);
    }

    fn requireCallableInst(self: SemanticThread) CallableInstId {
        return requireSourceContextCallableInst(self.source_context);
    }

    fn callableInstRawForDebug(self: SemanticThread) u32 {
        return switch (self.source_context) {
            .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }

    fn rootExprRawForDebug(self: SemanticThread) u32 {
        return switch (self.source_context) {
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .callable_inst, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }

};

const ContextExprVisitKey = ContextExprKey;

const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const ScanUnit = union(enum) {
    root_expr: RootExprContext,
    callable_inst: CallableInstId,
};

const BuildStmtKey = Lambdamono.BuildStmtKey;

/// Output of the staged CoreCIR/context-mono/lambda-specialization pipeline.
pub const Result = struct {
    context_mono: ContextMono.Result,
    lambdasolved: Lambdasolved.Result,
    lambdamono: Lambdamono.Program,

    pub fn init(allocator: Allocator) !Result {
        return .{
            .context_mono = try ContextMono.Result.init(allocator),
            .lambdasolved = try Lambdasolved.Result.init(allocator),
            .lambdamono = Lambdamono.Program.init(),
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.lambdamono.deinit(allocator);
        self.context_mono.deinit(allocator);
        self.lambdasolved.deinit(allocator);
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

    pub fn getExprCallSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).call_semantics) {
            .not_call => null,
            .call => |call_site| call_site,
        };
    }

    pub fn getExprMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        const program_expr = self.lambdamono.getExpr(expr_id);
        if (!program_expr.monotype.isNone()) return program_expr.monotype;
        return null;
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).callable_semantics) {
            .ordinary => null,
            .callable => |callable_value| callable_value,
            .packed_intro => |intro| .{ .packed_fn = intro.packed_fn },
        };
    }

    pub fn getExprIntroCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).callable_semantics) {
            .packed_intro => |intro| intro.callable_inst,
            .ordinary, .callable => null,
        };
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).dispatch_semantics) {
            .not_dispatch => null,
            .dispatch => |dispatch_target| dispatch_target,
        };
    }

    pub fn getLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?Lambdamono.LookupResolution {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).lookup_semantics) {
            .not_lookup => null,
            .lookup => |lookup_resolution| lookup_resolution,
        };
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
            return self.getCallableValueMonotype(callable_value);
        }

        return self.context_mono.getContextPatternMonotype(source_context, module_idx, pattern_idx);
    }

    fn getCallableTemplate(self: *const Result, callable_template_id: CallableTemplateId) *const CallableTemplate {
        return self.lambdasolved.getCallableTemplate(callable_template_id);
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

    pub fn getCallableValueMonotype(
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
        const callable_inst = self.getCallableInst(callable_inst_id);
        return switch (callable_inst.runtime_value) {
            .direct_lambda => .{
                .idx = self.context_mono.monotype_store.unit_idx,
                .module_idx = callable_inst.fn_monotype_module_idx,
            },
            .closure => |closure| closure.capture_tuple_monotype,
        };
    }

    pub fn getPackedFnVariants(self: *const Result, packed_fn: PackedFn) []const CallableInstId {
        return self.lambdamono.getCallableVariantGroupVariants(packed_fn.variant_group);
    }

    pub fn getIndirectCallVariants(self: *const Result, indirect_call: IndirectCall) []const CallableInstId {
        return self.lambdamono.getCallableVariantGroupVariants(indirect_call.packed_fn.variant_group);
    }

    pub fn getPackedFnTagName(
        self: *const Result,
        packed_fn: PackedFn,
        callable_inst_id: CallableInstId,
    ) Monotype.Name {
        const module_env = self.all_module_envs[packed_fn.runtime_monotype.module_idx];

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
        return self.lambdasolved.getLocalCallableTemplate(module_idx, pattern_idx);
    }

    fn getExternalCallableTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?CallableTemplateId {
        return self.lambdasolved.getExternalCallableTemplate(module_idx, def_node_idx);
    }

    pub fn getContextPatternSourceExpr(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprRef {
        return self.lambdamono.getPatternValueOrigin(source_context, module_idx, pattern_idx);
    }

    pub fn getExprValueOrigin(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprRef {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).value_origin) {
            .self_value => null,
            .expr => |expr_ref| expr_ref,
        };
    }

    pub fn getExprTemplateId(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableTemplateId {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).template_semantics) {
            .not_template => null,
            .template => |template_id| template_id,
        };
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).callable_semantics) {
            .ordinary => null,
            .callable => |callable_value| callable_value,
            .packed_intro => |intro| .{ .packed_fn = intro.packed_fn },
        };
    }

    pub fn getExprCallSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallSite {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).call_semantics) {
            .not_call => null,
            .call => |call_site| call_site,
        };
    }

    pub fn getExprLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?LookupResolution {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).lookup_semantics) {
            .not_lookup => null,
            .lookup => |lookup_resolution| lookup_resolution,
        };
    }

    pub fn getExprDispatchTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const expr_id = self.lambdamono.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return switch (self.lambdamono.getExpr(expr_id).dispatch_semantics) {
            .not_dispatch => null,
            .dispatch => |dispatch_target| dispatch_target,
        };
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
};

/// Pipelines callable templates into explicit callable instantiations.
pub const Pass = struct {
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    visited_modules: std.AutoHashMapUnmanaged(u32, void),
    visited_exprs: std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    in_progress_value_defs: std.AutoHashMapUnmanaged(ContextExprKey, void),
    in_progress_call_result_callable_insts: std.AutoHashMapUnmanaged(CallResultCallableInstKey, void),
    in_progress_callable_scans: std.AutoHashMapUnmanaged(u32, void),
    completed_callable_scans: std.AutoHashMapUnmanaged(u32, void),
    active_scan_units: std.ArrayListUnmanaged(ScanUnit),
    pending_root_scans: std.ArrayListUnmanaged(RootExprContext),
    pending_root_scan_keys: std.AutoHashMapUnmanaged(u64, void),
    draining_root_scans: bool,
    pending_callable_scans: std.ArrayListUnmanaged(CallableInstId),
    pending_callable_scan_keys: std.AutoHashMapUnmanaged(u32, void),
    draining_callable_scans: bool,
    mutation_counts: [mutation_kind_count]u32,

    pub fn init(
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        types_store: *const types.Store,
        current_module_idx: u32,
        app_module_idx: ?u32,
    ) Pass {
        return .{
            .allocator = allocator,
            .all_module_envs = all_module_envs,
            .types_store = types_store,
            .current_module_idx = current_module_idx,
            .app_module_idx = app_module_idx,
            .visited_modules = .empty,
            .visited_exprs = .empty,
            .in_progress_value_defs = .empty,
            .in_progress_call_result_callable_insts = .empty,
            .in_progress_callable_scans = .empty,
            .completed_callable_scans = .empty,
            .active_scan_units = .empty,
            .pending_root_scans = .empty,
            .pending_root_scan_keys = .empty,
            .draining_root_scans = false,
            .pending_callable_scans = .empty,
            .pending_callable_scan_keys = .empty,
            .draining_callable_scans = false,
            .mutation_counts = [_]u32{0} ** mutation_kind_count,
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
                const caller_mono = try self.monotypeFromTypeVarInStore(
                    result,
                    caller_module_idx,
                    caller_types,
                    caller_var,
                );
                if (caller_mono.isNone()) continue;
                const normalized_mono = if (caller_module_idx == module_idx)
                    caller_mono
                else
                    try self.remapMonotypeBetweenModules(
                        result,
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
                    resolvedMonotype(normalized_mono, module_idx),
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


    fn buildStmtKey(
        _: *const Pass,
        source_context: SourceContext,
        module_idx: u32,
        stmt_idx: CIR.Statement.Idx,
    ) BuildStmtKey {
        return switch (source_context) {
            .callable_inst => |context_id| .{
                .source_context_kind = .callable_inst,
                .source_context_module_idx = std.math.maxInt(u32),
                .source_context_raw = @intFromEnum(context_id),
                .module_idx = module_idx,
                .stmt_raw = @intFromEnum(stmt_idx),
            },
            .root_expr => |root| .{
                .source_context_kind = .root_expr,
                .source_context_module_idx = root.module_idx,
                .source_context_raw = @intFromEnum(root.expr_idx),
                .module_idx = module_idx,
                .stmt_raw = @intFromEnum(stmt_idx),
            },
            .provenance_expr => |source| .{
                .source_context_kind = .provenance_expr,
                .source_context_module_idx = source.module_idx,
                .source_context_raw = @intFromEnum(source.expr_idx),
                .module_idx = module_idx,
                .stmt_raw = @intFromEnum(stmt_idx),
            },
            .template_expr => |template| .{
                .source_context_kind = .template_expr,
                .source_context_module_idx = template.module_idx,
                .source_context_raw = @intFromEnum(template.expr_idx),
                .module_idx = module_idx,
                .stmt_raw = @intFromEnum(stmt_idx),
            },
        };
    }

    fn exactCallableInstFromCallableValue(callable_value: CallableValue) ?CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_fn => null,
        };
    }

    fn callableAlternativesFromValue(result: *const Result, callable_value: CallableValue) []const CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| result.lambdamono.getDirectCallableVariants(callable_inst_id),
            .packed_fn => |packed_fn| result.getPackedFnVariants(packed_fn),
        };
    }

    fn exactCallableInstFromCallSite(call_site: CallSite) ?CallableInstId {
        return switch (call_site) {
            .direct => |callable_inst_id| callable_inst_id,
            .indirect_call => null,
        };
    }

    fn callableAlternativesFromCallSite(result: *const Result, call_site: CallSite) []const CallableInstId {
        return switch (call_site) {
            .direct => |callable_inst_id| result.lambdamono.getDirectCallableVariants(callable_inst_id),
            .indirect_call => |indirect_call| result.getIndirectCallVariants(indirect_call),
        };
    }

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
            for (callableAlternativesFromValue(result, callable_value)) |callable_inst_id| {
                try self.includeCallableInst(pass, callable_inst_id);
            }
        }

        fn includeCallSite(
            self: *CallableVariantBuilder,
            pass: *Pass,
            result: *const Result,
            call_site: CallSite,
        ) Allocator.Error!void {
            for (callableAlternativesFromCallSite(result, call_site)) |callable_inst_id| {
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
            try pass.ensureRecordedCallableInstsScanned(result, self.variants.items);
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
            try pass.ensureRecordedCallableInstsScanned(result, self.variants.items);
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
        _ = result;
        return self.readCallableParamValue(
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
        _ = result;
        return self.readCallableParamValue(
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
        return exactCallableInstFromCallableValue(callable_value);
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
        return exactCallableInstFromCallableValue(callable_value);
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
        return exactCallableInstFromCallableValue(callable_value);
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
        return exactCallableInstFromCallableValue(callable_value);
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
        return exactCallableInstFromCallSite(call_site);
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
        return exactCallableInstFromCallSite(call_site);
    }

    fn getCallSiteCallableInstForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_site = self.getCallSiteForSourceContext(result, source_context, module_idx, expr_idx) orelse return null;
        return exactCallableInstFromCallSite(call_site);
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
        const param_patterns = self.callableBoundaryArgPatterns(template.module_idx, template.cir_expr) orelse return null;

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
        switch (module_env.store.getExpr(expr_idx)) {
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
        switch (module_env.store.getExpr(expr_idx)) {
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
        return exactCallableInstFromCallableValue(callable_value);
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
        return exactCallableInstFromCallableValue(callable_value);
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

        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (isTopLevelDefPattern(module_env, lookup.pattern_idx)) return null;
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableValueForSourceContext(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callable_value;
        }

        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
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
            if (exactCallableInstFromCallableValue(callable_value)) |callable_inst_id| {
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
            }
        }
        return true;
    }

    pub fn deinit(self: *Pass) void {
        self.visited_modules.deinit(self.allocator);
        self.visited_exprs.deinit(self.allocator);
        self.in_progress_value_defs.deinit(self.allocator);
        self.in_progress_call_result_callable_insts.deinit(self.allocator);
        self.in_progress_callable_scans.deinit(self.allocator);
        self.completed_callable_scans.deinit(self.allocator);
        self.active_scan_units.deinit(self.allocator);
        self.pending_root_scans.deinit(self.allocator);
        self.pending_root_scan_keys.deinit(self.allocator);
        self.pending_callable_scans.deinit(self.allocator);
        self.pending_callable_scan_keys.deinit(self.allocator);
    }

    fn resetRunState(self: *Pass) void {
        self.visited_modules.clearRetainingCapacity();
        self.visited_exprs.clearRetainingCapacity();
        self.in_progress_value_defs.clearRetainingCapacity();
        self.in_progress_call_result_callable_insts.clearRetainingCapacity();
        self.in_progress_callable_scans.clearRetainingCapacity();
        self.completed_callable_scans.clearRetainingCapacity();
        self.active_scan_units.clearRetainingCapacity();
        self.pending_root_scans.clearRetainingCapacity();
        self.pending_root_scan_keys.clearRetainingCapacity();
        self.draining_root_scans = false;
        self.pending_callable_scans.clearRetainingCapacity();
        self.pending_callable_scan_keys.clearRetainingCapacity();
        self.draining_callable_scans = false;
        self.mutation_counts = [_]u32{0} ** mutation_kind_count;
    }

    pub fn runRootSourceExpr(self: *Pass, expr_idx: CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        try self.runRootSourceExprInto(&result, expr_idx);
        return result;
    }

    fn runRootSourceExprInto(
        self: *Pass,
        result: *Result,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        self.resetRunState();
        try self.primeAllModules(result);
        try self.scanSeedModules(result);
        try self.scanModule(result, self.current_module_idx);
        try self.enqueueRootScans(&.{expr_idx});
        try self.drainRootScans(result);
        try self.assembleLambdamonoProgramGraph(result, &.{expr_idx});
    }

    pub fn runRootSourceExprs(self: *Pass, exprs: []const CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        try self.runRootSourceExprsInto(&result, exprs);
        return result;
    }

    fn runRootSourceExprsInto(
        self: *Pass,
        result: *Result,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        self.resetRunState();
        try self.primeAllModules(result);
        try self.scanSeedModules(result);
        try self.scanModule(result, self.current_module_idx);
        try self.enqueueRootScans(exprs);
        try self.drainRootScans(result);
        try self.assembleLambdamonoProgramGraph(result, exprs);
    }

    /// Pipeline all callables rooted in the current module.
    pub fn runModule(self: *Pass) Allocator.Error!Result {
        var result = try Result.init(self.allocator);
        try self.runModuleInto(&result);
        return result;
    }

    fn runModuleInto(
        self: *Pass,
        result: *Result,
    ) Allocator.Error!void {
        self.resetRunState();
        try self.primeAllModules(result);
        try self.scanSeedModules(result);
        try self.scanModule(result, self.current_module_idx);

        const module_env = self.all_module_envs[self.current_module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        var root_source_exprs = std.ArrayList(CIR.Expr.Idx).empty;
        defer root_source_exprs.deinit(self.allocator);
        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try root_source_exprs.append(self.allocator, def.expr);
        }
        try self.enqueueRootScans(root_source_exprs.items);
        try self.drainRootScans(result);
        try self.assembleLambdamonoProgramGraph(result, root_source_exprs.items);
    }

    fn assembleLambdamonoProgramGraph(
        self: *Pass,
        result: *Result,
        explicit_root_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        try self.assembleCallableDefExprGraph(result);
        try self.registerExplicitProgramRoots(result, explicit_root_exprs);
        try self.registerDiscoveredProgramRoots(result);
    }

    fn registerExplicitProgramRoots(
        self: *Pass,
        result: *Result,
        explicit_root_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (explicit_root_exprs) |expr_idx| {
            try self.registerProgramRoot(
                result,
                .{ .root_expr = .{ .module_idx = self.current_module_idx, .expr_idx = expr_idx } },
                self.current_module_idx,
                expr_idx,
            );
        }
    }

    fn registerDiscoveredProgramRoots(
        self: *Pass,
        result: *Result,
    ) Allocator.Error!void {
        var iter = self.visited_exprs.iterator();
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.source_context_kind != .root_expr) continue;
            const source_context: SourceContext = .{ .root_expr = .{
                .module_idx = key.source_context_module_idx,
                .expr_idx = @enumFromInt(key.source_context_raw),
            } };
            try self.registerProgramRoot(
                result,
                source_context,
                key.module_idx,
                @enumFromInt(key.expr_raw),
            );
        }
    }

    fn assembleCallableDefExprGraph(
        self: *Pass,
        result: *Result,
    ) Allocator.Error!void {
        for (result.lambdamono.callable_insts.items, 0..) |callable_inst, raw_callable_inst_id| {
            const template = result.getCallableTemplate(callable_inst.template);
            switch (template.kind) {
                .lambda, .closure, .hosted_lambda => {},
                .top_level_def => std.debug.panic(
                    "Pipeline invariant violated: callable inst {d} runtime expr {d} in module {d} retained top_level_def callable template kind after callable registration",
                    .{ raw_callable_inst_id, @intFromEnum(template.runtime_expr), template.module_idx },
                ),
            }
            const callable_def = &result.lambdamono.callable_defs.items[@intFromEnum(callable_inst.callable_def)];
            _ = try self.assembleProgramExprNode(
                result,
                callable_def.runtime_expr.source_context,
                callable_def.runtime_expr.module_idx,
                callable_def.runtime_expr.expr_idx,
            );
            _ = try self.assembleProgramExprNode(
                result,
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
            );
            for (result.getCaptureFields(callable_def.captures)) |capture_field| {
                switch (capture_field.source) {
                    .expr => |expr_ref| try self.ensureProgramExprRefNode(result, expr_ref),
                    .lexical_pattern => {},
                }
            }
        }
    }

    fn registerProgramRoot(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const key = Result.contextExprKey(source_context, module_idx, expr_idx);
        if (result.lambdamono.root_expr_ids_by_key.contains(key)) return;
        const body_expr = try self.assembleProgramExprNode(result, source_context, module_idx, expr_idx);
        const root_expr_id: Lambdamono.RootExprId = @enumFromInt(result.lambdamono.root_exprs.items.len);
        try result.lambdamono.root_exprs.append(self.allocator, .{ .key = key, .body_expr = body_expr });
        try result.lambdamono.root_expr_ids_by_key.put(self.allocator, key, root_expr_id);
    }

    fn assembleProgramExprNode(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Lambdamono.ExprId {
        const key = Result.contextExprKey(source_context, module_idx, expr_idx);
        const expr_id = result.lambdamono.expr_ids_by_key.get(key) orelse blk: {
            _ = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
            break :blk result.lambdamono.expr_ids_by_key.get(key).?;
        };
        if (result.lambdamono.getExpr(expr_id).state == .finalized) return expr_id;

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var child_exprs = std.ArrayList(Lambdamono.ExprId).empty;
        defer child_exprs.deinit(self.allocator);
        var child_stmts = std.ArrayList(Lambdamono.StmtId).empty;
        defer child_stmts.deinit(self.allocator);

        switch (expr) {
            .e_str => |str_expr| for (module_env.store.sliceExpr(str_expr.span)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, child_expr_idx));
            },
            .e_list => |list_expr| for (module_env.store.sliceExpr(list_expr.elems)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, child_expr_idx));
            },
            .e_tuple => |tuple_expr| for (module_env.store.sliceExpr(tuple_expr.elems)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, child_expr_idx));
            },
            .e_match => |match_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, match_expr.cond));
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard_expr_idx| {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, guard_expr_idx));
                    }
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, branch.value));
                }
            },
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, branch.cond));
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, branch.body));
                }
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, if_expr.final_else));
            },
            .e_call => |call_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, call_expr.func));
                for (module_env.store.sliceExpr(call_expr.args)) |arg_expr_idx| {
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, arg_expr_idx));
                }
            },
            .e_record => |record_expr| {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, field.value));
                }
                if (record_expr.ext) |ext_expr_idx| {
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, ext_expr_idx));
                }
            },
            .e_block => |block_expr| {
                for (module_env.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    try child_stmts.append(self.allocator, try self.assembleProgramStmtNode(result, source_context, module_idx, stmt_idx));
                }
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, block_expr.final_expr));
            },
            .e_tag => |tag_expr| for (module_env.store.sliceExpr(tag_expr.args)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, child_expr_idx));
            },
            .e_nominal => |nominal_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, nominal_expr.backing_expr));
            },
            .e_nominal_external => |nominal_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, nominal_expr.backing_expr));
            },
            .e_binop => |binop_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, binop_expr.lhs));
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, binop_expr.rhs));
            },
            .e_unary_minus => |unary_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, unary_expr.expr));
            },
            .e_unary_not => |unary_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, unary_expr.expr));
            },
            .e_dot_access => |dot_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, dot_expr.receiver));
                if (dot_expr.args) |args| for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, arg_expr_idx));
                };
            },
            .e_tuple_access => |tuple_access| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, tuple_access.tuple));
            },
            .e_dbg => |dbg_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, dbg_expr.expr));
            },
            .e_expect => |expect_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, expect_expr.body));
            },
            .e_return => |return_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, return_expr.expr));
            },
            .e_type_var_dispatch => |dispatch_expr| for (module_env.store.sliceExpr(dispatch_expr.args)) |arg_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, arg_expr_idx));
            },
            .e_for => |for_expr| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, for_expr.expr));
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, for_expr.body));
            },
            .e_run_low_level => |run_low_level| for (module_env.store.sliceExpr(run_low_level.args)) |arg_expr_idx| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, arg_expr_idx));
            },
            else => {},
        }

        const child_expr_span = try self.appendProgramExprChildren(&result.lambdamono, child_exprs.items);
        const child_stmt_span = try self.appendProgramStmtChildren(&result.lambdamono, child_stmts.items);
        const callable_value = result.getExprCallableValue(source_context, module_idx, expr_idx);
        const call_site = result.getExprCallSite(source_context, module_idx, expr_idx);
        const value_origin = if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |origin|
            Lambdamono.ExprValueOrigin{ .expr = origin }
        else
            .self_value;
        const dispatch_target = result.getExprDispatchTarget(source_context, module_idx, expr_idx);
        const lookup_resolution = result.getExprLookupResolution(source_context, module_idx, expr_idx);
        var monotype = resolvedMonotype(.none, module_idx);
        if (callable_value) |expr_callable_value| {
            monotype = result.getCallableValueMonotype(expr_callable_value);
        }
        if (monotype.isNone()) {
            monotype = try self.requireProgramExprMonotype(
                result,
                source_context,
                module_idx,
                expr_idx,
            );
        }
        switch (value_origin) {
            .self_value => {},
            .expr => |expr_ref| try self.ensureProgramExprRefNode(result, expr_ref),
        }
        if (lookup_resolution) |resolved_lookup| {
            switch (resolved_lookup) {
                .expr => |expr_ref| try self.ensureProgramExprRefNode(result, expr_ref),
                .def => {},
            }
        }

        const program_expr = &result.lambdamono.exprs.items[@intFromEnum(expr_id)];
        program_expr.state = .finalized;
        program_expr.source_context = source_context;
        program_expr.module_idx = module_idx;
        program_expr.source_expr = expr_idx;
        program_expr.monotype = monotype;
        program_expr.child_exprs = child_expr_span;
        program_expr.child_stmts = child_stmt_span;
        program_expr.callable_semantics = if (callable_value) |expr_callable_value|
            .{ .callable = expr_callable_value }
        else
            .ordinary;
        program_expr.call_semantics = if (call_site) |expr_call_site|
            .{ .call = expr_call_site }
        else
            .not_call;
        program_expr.value_origin = value_origin;
        program_expr.dispatch_semantics = if (dispatch_target) |resolved_dispatch_target|
            .{ .dispatch = resolved_dispatch_target }
        else
            .not_dispatch;
        program_expr.lookup_semantics = semantics.lookup_semantics;
        return expr_id;
    }

    fn assembleProgramStmtNode(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        stmt_idx: CIR.Statement.Idx,
    ) Allocator.Error!Lambdamono.StmtId {
        const key = self.buildStmtKey(source_context, module_idx, stmt_idx);
        if (result.lambdamono.stmt_ids_by_key.get(key)) |existing| return existing;

        const module_env = self.all_module_envs[module_idx];
        const stmt = module_env.store.getStatement(stmt_idx);
        var child_exprs = std.ArrayList(Lambdamono.ExprId).empty;
        defer child_exprs.deinit(self.allocator);

        switch (stmt) {
            .s_decl => |decl| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, decl.expr)),
            .s_var => |var_decl| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, var_decl.expr)),
            .s_reassign => |reassign| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, reassign.expr)),
            .s_dbg => |dbg_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, dbg_stmt.expr)),
            .s_expr => |expr_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, expr_stmt.expr)),
            .s_expect => |expect_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, expect_stmt.body)),
            .s_for => |for_stmt| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, for_stmt.expr));
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, for_stmt.body));
            },
            .s_while => |while_stmt| {
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, while_stmt.cond));
                try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, while_stmt.body));
            },
            .s_return => |return_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(result, source_context, module_idx, return_stmt.expr)),
            else => {},
        }

        const stmt_id: Lambdamono.StmtId = @enumFromInt(result.lambdamono.stmts.items.len);
        try result.lambdamono.stmts.append(self.allocator, .{
            .module_idx = module_idx,
            .source_stmt = stmt_idx,
            .child_exprs = try self.appendProgramExprChildren(&result.lambdamono, child_exprs.items),
        });
        try result.lambdamono.stmt_ids_by_key.put(self.allocator, key, stmt_id);
        return stmt_id;
    }

    fn ensureProgramExprRefNode(
        self: *Pass,
        result: *Result,
        expr_ref: ExprRef,
    ) Allocator.Error!void {
        _ = try self.assembleProgramExprNode(
            result,
            expr_ref.source_context,
            expr_ref.module_idx,
            expr_ref.expr_idx,
        );
    }

    fn appendProgramExprChildren(
        self: *Pass,
        program: *Lambdamono.Program,
        child_exprs: []const Lambdamono.ExprId,
    ) Allocator.Error!Lambdamono.ExprIdSpan {
        const start: u32 = @intCast(program.expr_child_entries.items.len);
        try program.expr_child_entries.appendSlice(self.allocator, child_exprs);
        return .{
            .start = start,
            .len = @intCast(child_exprs.len),
        };
    }

    fn appendProgramStmtChildren(
        self: *Pass,
        program: *Lambdamono.Program,
        child_stmts: []const Lambdamono.StmtId,
    ) Allocator.Error!Lambdamono.StmtIdSpan {
        const start: u32 = @intCast(program.stmt_child_entries.items.len);
        try program.stmt_child_entries.appendSlice(self.allocator, child_stmts);
        return .{
            .start = start,
            .len = @intCast(child_stmts.len),
        };
    }

    fn ensureProgramExprSemantics(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!*Lambdamono.Expr {
        const key = Result.contextExprKey(source_context, module_idx, expr_idx);
        const expr_id = result.lambdamono.expr_ids_by_key.get(key) orelse blk: {
            const new_expr_id: Lambdamono.ExprId = @enumFromInt(result.lambdamono.exprs.items.len);
            try result.lambdamono.exprs.append(self.allocator, .{
                .state = .reserved,
                .source_context = source_context,
                .module_idx = module_idx,
                .source_expr = expr_idx,
                .monotype = resolvedMonotype(.none, module_idx),
                .child_exprs = .empty(),
                .child_stmts = .empty(),
                .template_semantics = .not_template,
                .callable_semantics = .ordinary,
                .call_semantics = .not_call,
                .value_origin = .self_value,
                .dispatch_semantics = .not_dispatch,
                .lookup_semantics = .not_lookup,
            });
            try result.lambdamono.expr_ids_by_key.put(self.allocator, key, new_expr_id);
            break :blk new_expr_id;
        };
        return &result.lambdamono.exprs.items[@intFromEnum(expr_id)];
    }

    fn ensureProgramPatternBinding(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) Allocator.Error!*Lambdamono.PatternBinding {
        const key = Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const binding_id = result.lambdamono.pattern_binding_ids_by_key.get(key) orelse blk: {
            const new_binding_id: Lambdamono.BindingId = @enumFromInt(result.lambdamono.pattern_bindings.items.len);
            try result.lambdamono.pattern_bindings.append(self.allocator, .{
                .key = key,
                .callable_semantics = .non_callable,
                .value_origin = .self_value,
            });
            try result.lambdamono.pattern_binding_ids_by_key.put(self.allocator, key, new_binding_id);
            break :blk new_binding_id;
        };
        return &result.lambdamono.pattern_bindings.items[@intFromEnum(binding_id)];
    }

    fn writeExprTemplateSemantics(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_semantics: Lambdamono.ExprTemplateSemantics = .{ .template = template_id };
        if (!std.meta.eql(semantics.template_semantics, next_semantics)) {
            semantics.template_semantics = next_semantics;
            self.noteMutation(.expr_semantics);
            try self.requeueActiveScanUnits();
        }
    }

    fn writePatternValueOrigin(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_ref: ExprRef,
    ) Allocator.Error!void {
        const binding = try self.ensureProgramPatternBinding(result, source_context, module_idx, pattern_idx);
        const next_origin: Lambdamono.PatternValueOrigin = .{ .expr = expr_ref };
        if (!std.meta.eql(binding.value_origin, next_origin)) {
            binding.value_origin = next_origin;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
    }

    fn requireProgramExprMonotype(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        const thread = SemanticThread.trackedThread(source_context);
        const resolved = blk: {
            if (self.lookupExprMonotypeForSourceContext(result, thread, source_context, module_idx, expr_idx)) |existing| {
                break :blk existing;
            }
            if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
                break :blk callableValueMonotype(result, callable_value);
            }
            break :blk resolvedMonotype(.none, module_idx);
        };
        if (!resolved.isNone()) return resolved;

        std.debug.panic(
            "Pipeline invariant violated: missing exact source monotype for expr {d} ({s}) in module {d} under source context {s}",
            .{
                @intFromEnum(expr_idx),
                @tagName(self.all_module_envs[module_idx].store.getExpr(expr_idx)),
                module_idx,
                @tagName(source_context),
            },
        );
    }

    fn primeAllModules(self: *Pass, result: *Result) Allocator.Error!void {
        for (self.all_module_envs, 0..) |_, module_idx| {
            try self.primeModuleDefs(result, @intCast(module_idx));
        }
    }

    fn rootScanKey(root: RootExprContext) u64 {
        return (@as(u64, root.module_idx) << 32) | @as(u64, @intFromEnum(root.expr_idx));
    }

    fn enqueueRootScan(self: *Pass, root: RootExprContext) Allocator.Error!void {
        const gop = try self.pending_root_scan_keys.getOrPut(self.allocator, rootScanKey(root));
        if (!gop.found_existing) {
            gop.value_ptr.* = {};
            try self.pending_root_scans.append(self.allocator, root);
        }
    }

    fn enqueueCallableScan(
        self: *Pass,
        callable_inst_id: CallableInstId,
        comptime force: bool,
    ) Allocator.Error!void {
        const raw = @intFromEnum(callable_inst_id);
        if (!force and (self.completed_callable_scans.contains(raw) or self.in_progress_callable_scans.contains(raw))) {
            return;
        }

        const gop = try self.pending_callable_scan_keys.getOrPut(self.allocator, raw);
        if (!gop.found_existing) {
            gop.value_ptr.* = {};
            try self.pending_callable_scans.append(self.allocator, callable_inst_id);
        }
    }

    fn pushActiveScanUnit(self: *Pass, unit: ScanUnit) Allocator.Error!void {
        try self.active_scan_units.append(self.allocator, unit);
    }

    fn popActiveScanUnit(self: *Pass) void {
        _ = self.active_scan_units.pop();
    }

    fn requeueActiveScanUnits(self: *Pass) Allocator.Error!void {
        for (self.active_scan_units.items) |unit| {
            switch (unit) {
                .root_expr => |root| try self.enqueueRootScan(root),
                .callable_inst => |callable_inst_id| try self.enqueueCallableScan(callable_inst_id, true),
            }
        }
    }

    fn clearScanScratch(self: *Pass) void {
        self.visited_exprs.clearRetainingCapacity();
        self.in_progress_value_defs.clearRetainingCapacity();
        self.in_progress_call_result_callable_insts.clearRetainingCapacity();
    }

    fn popPendingRootScan(self: *Pass) ?RootExprContext {
        if (self.pending_root_scans.items.len == 0) return null;
        const root = self.pending_root_scans.pop();
        _ = self.pending_root_scan_keys.remove(rootScanKey(root));
        return root;
    }

    fn popPendingCallableScan(self: *Pass) ?CallableInstId {
        if (self.pending_callable_scans.items.len == 0) return null;
        const callable_inst_id = self.pending_callable_scans.pop();
        _ = self.pending_callable_scan_keys.remove(@intFromEnum(callable_inst_id));
        return callable_inst_id;
    }

    fn enqueueRootScans(
        self: *Pass,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (exprs) |expr_idx| {
            try self.enqueueRootScan(.{
                .module_idx = self.current_module_idx,
                .expr_idx = expr_idx,
            });
        }
    }

    fn drainRootScans(self: *Pass, result: *Result) Allocator.Error!void {
        if (self.draining_root_scans) return;
        self.draining_root_scans = true;
        defer self.draining_root_scans = false;

        var scans_processed: u32 = 0;
        while (self.popPendingRootScan()) |root| {
            scans_processed += 1;
            if (std.debug.runtime_safety and scans_processed > 256) {
                std.debug.panic(
                    "Pipeline: explicit root rescan queue did not converge (module={d}, templates={d}, callable_insts={d}, program_exprs={d}, root_exprs={d}, context_monos={d}, context_pattern_monos={d}, mutation_counts={any})",
                    .{
                        self.current_module_idx,
                        result.lambdasolved.callable_templates.items.len,
                        result.lambdamono.callable_insts.items.len,
                        result.lambdamono.exprs.items.len,
                        result.lambdamono.root_exprs.items.len,
                        result.context_mono.context_expr_monotypes.count(),
                        result.context_mono.context_pattern_monotypes.count(),
                        self.mutation_counts,
                    },
                );
            }

            self.clearScanScratch();
            try self.pushActiveScanUnit(.{ .root_expr = root });
            defer self.popActiveScanUnit();
            try self.scanCirValueExpr(
                result,
                SemanticThread.trackedThread(.{ .root_expr = root }),
                root.module_idx,
                root.expr_idx,
            );
        }
    }

    fn drainCallableScans(self: *Pass, result: *Result) Allocator.Error!void {
        if (self.draining_callable_scans) return;
        self.draining_callable_scans = true;
        defer self.draining_callable_scans = false;

        var scans_processed: u32 = 0;
        while (self.popPendingCallableScan()) |callable_inst_id| {
            scans_processed += 1;
            if (std.debug.runtime_safety and scans_processed > 512) {
                std.debug.panic(
                    "Pipeline: explicit callable rescan queue did not converge for callable_inst={d} (mutation_counts={any})",
                    .{
                        @intFromEnum(callable_inst_id),
                        self.mutation_counts,
                    },
                );
            }

            _ = self.completed_callable_scans.remove(@intFromEnum(callable_inst_id));
            self.clearScanScratch();
            try self.scanCallableInstBody(result, callable_inst_id);
        }
    }

    fn noteMutation(self: *Pass, comptime kind: MutationKind) void {
        self.mutation_counts[@intFromEnum(kind)] +%= 1;
    }

    fn putTracked(self: *Pass, comptime kind: MutationKind, map: anytype, key: anytype, value: anytype) Allocator.Error!void {
        const gop = try map.getOrPut(self.allocator, key);
        const typed_value: @TypeOf(gop.value_ptr.*) = value;
        if (!gop.found_existing or !std.meta.eql(gop.value_ptr.*, typed_value)) {
            gop.value_ptr.* = typed_value;
            self.noteMutation(kind);
            try self.requeueActiveScanUnits();
        }
    }

    fn appendTracked(self: *Pass, comptime kind: MutationKind, list: anytype, value: anytype) Allocator.Error!void {
        const typed_value: @TypeOf(list.items[0]) = value;
        try list.append(self.allocator, typed_value);
        self.noteMutation(kind);
        try self.requeueActiveScanUnits();
    }

    fn writeExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_semantics: Lambdamono.ExprCallableSemantics = switch (callable_value) {
            .direct => .{ .callable = callable_value },
            .packed_fn => |packed_fn| blk: {
                if (callableKind(self.all_module_envs[module_idx].store.getExpr(expr_idx)) != null) {
                    switch (semantics.callable_semantics) {
                        .callable => |existing_callable_value| switch (existing_callable_value) {
                            .direct => |callable_inst_id| break :blk .{ .packed_intro = .{
                                .packed_fn = packed_fn,
                                .callable_inst = callable_inst_id,
                            } },
                            .packed_fn => {},
                        },
                        .ordinary, .packed_intro => {},
                    }
                }
                break :blk .{ .callable = callable_value };
            },
        };
        if (!std.meta.eql(semantics.callable_semantics, next_semantics)) {
            semantics.callable_semantics = next_semantics;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
    }

    fn writeCallableParamValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        const binding = try self.ensureProgramPatternBinding(result, source_context, module_idx, pattern_idx);
        const next_semantics: Lambdamono.PatternCallableSemantics = .{ .callable = callable_value };
        if (!std.meta.eql(binding.callable_semantics, next_semantics)) {
            binding.callable_semantics = next_semantics;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
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
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        const binding = self.lambdamono.getPatternBinding(source_context, module_idx, pattern_idx) orelse return null;
        return switch (binding.callable_semantics) {
            .non_callable => null,
            .callable => |callable_value| callable_value,
        };
    }

    fn isTopLevelDefPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) bool {
        return findDefByPattern(module_env, pattern_idx) != null;
    }

    fn writeExprCallSite(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        call_site: CallSite,
    ) Allocator.Error!void {
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_semantics: Lambdamono.ExprCallSemantics = .{ .call = call_site };
        if (!std.meta.eql(semantics.call_semantics, next_semantics)) {
            semantics.call_semantics = next_semantics;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
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

    fn writeExprValueOrigin(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr_ref: ExprRef,
    ) Allocator.Error!void {
        const canonical_ref = blk: {
            const origin = result.getExprValueOrigin(expr_ref.source_context, expr_ref.module_idx, expr_ref.expr_idx) orelse break :blk expr_ref;
            if (result.getExprValueOrigin(origin.source_context, origin.module_idx, origin.expr_idx) != null) {
                std.debug.panic(
                    "Pipeline invariant violated: expr value-origin for ctx={s} module={d} expr={d} was not canonical",
                    .{
                        @tagName(expr_ref.source_context),
                        expr_ref.module_idx,
                        @intFromEnum(expr_ref.expr_idx),
                    },
                );
            }
            break :blk .{
                .source_context = origin.source_context,
                .module_idx = origin.module_idx,
                .expr_idx = origin.expr_idx,
                .projections = try self.appendValueProjectionEntries(
                    result,
                    origin.projections,
                    result.lambdamono.getValueProjectionEntries(expr_ref.projections),
                ),
            };
        };
        const source_template_id = result.getExprTemplateId(
            canonical_ref.source_context,
            canonical_ref.module_idx,
            canonical_ref.expr_idx,
        );
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_origin: Lambdamono.ExprValueOrigin = .{ .expr = canonical_ref };
        if (!std.meta.eql(semantics.value_origin, next_origin)) {
            semantics.value_origin = next_origin;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
        if (source_template_id) |template_id| {
            const next_template_semantics: Lambdamono.ExprTemplateSemantics = .{ .template = template_id };
            if (!std.meta.eql(semantics.template_semantics, next_template_semantics)) {
                semantics.template_semantics = next_template_semantics;
                self.noteMutation(.expr_semantics);
                try self.requeueActiveScanUnits();
            }
        }
    }

    fn writeExprLookupResolution(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        lookup_resolution: LookupResolution,
    ) Allocator.Error!void {
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_semantics: Lambdamono.ExprLookupSemantics = .{ .lookup = lookup_resolution };
        if (!std.meta.eql(semantics.lookup_semantics, next_semantics)) {
            semantics.lookup_semantics = next_semantics;
            self.noteMutation(.source_value_provenance);
            try self.requeueActiveScanUnits();
        }
    }

    fn scanSeedModules(self: *Pass, result: *Result) Allocator.Error!void {
        if (self.app_module_idx) |app_module_idx| {
            if (app_module_idx != self.current_module_idx) {
                try self.scanModule(result, app_module_idx);
            }
        }
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

    fn scanModule(self: *Pass, result: *Result, module_idx: u32) Allocator.Error!void {
        try self.primeModuleDefs(result, module_idx);

        if (self.visited_modules.contains(module_idx)) return;
        try self.visited_modules.put(self.allocator, module_idx, {});
    }

    fn primeModuleDefs(self: *Pass, result: *Result, module_idx: u32) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);

        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try self.recordContextPatternSourceExpr(result, .{ .root_expr = .{
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

            const template_id = try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                def.expr,
                ModuleEnv.varFrom(def.pattern),
                def.pattern,
                packLocalPatternSourceKey(module_idx, def.pattern),
            );
            if (template_id) |resolved_template_id| {
                try self.aliasCallableTemplateSource(result, external_source_key, resolved_template_id);
            }
        }
    }

    fn registerCallableTemplate(
        self: *Pass,
        result: *Result,
        owner_source_context: ?SourceContext,
        source_key: u64,
        module_idx: u32,
        cir_expr: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        kind: CallableTemplateKind,
        source_region: Region,
    ) Allocator.Error!CallableTemplateId {
        const existing = result.lambdasolved.callable_template_ids_by_source.get(source_key);
        if (existing) |template_id| return template_id;
        const boundary = self.callableBoundaryInfo(module_idx, cir_expr) orelse std.debug.panic(
            "Pipeline invariant violated: registering callable template for non-callable boundary expr {d} in module {d}",
            .{ @intFromEnum(cir_expr), module_idx },
        );

        const owner: Lambdasolved.CallableTemplateOwner = if (kind == .closure)
            if (owner_source_context) |source_context|
                if (result.getSourceContextTemplateId(source_context)) |template_id|
                    .{ .lexical_template = template_id }
                else
                    .root_scope
            else
                .root_scope
        else
            .root_scope;

        const callable_template_id: CallableTemplateId = @enumFromInt(result.lambdasolved.callable_templates.items.len);
        try self.appendTracked(.callable_templates, &result.lambdasolved.callable_templates, CallableTemplate{
            .source_key = source_key,
            .module_idx = module_idx,
            .cir_expr = cir_expr,
            .runtime_expr = cir_expr,
            .arg_patterns = boundary.arg_patterns,
            .body_expr = boundary.body_expr,
            .type_root = type_root,
            .binding = if (binding_pattern) |pattern_idx|
                .{ .pattern = pattern_idx }
            else
                .anonymous,
            .kind = kind,
            .owner = owner,
            .source_region = source_region,
        });
        try self.putTracked(
            .callable_template_aliases,
            &result.lambdasolved.callable_template_ids_by_source,
            source_key,
            callable_template_id,
        );

        return callable_template_id;
    }

    fn recordCallableTemplateRuntimeExpr(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        runtime_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const template = &result.lambdasolved.callable_templates.items[@intFromEnum(template_id)];
        if (template.runtime_expr == runtime_expr_idx) return;
        const boundary = self.callableBoundaryInfo(template.module_idx, runtime_expr_idx) orelse std.debug.panic(
            "Pipeline invariant violated: runtime expr {d} for callable template {d} in module {d} was not callable-boundary-shaped",
            .{ @intFromEnum(runtime_expr_idx), @intFromEnum(template_id), template.module_idx },
        );
        template.runtime_expr = runtime_expr_idx;
        template.arg_patterns = boundary.arg_patterns;
        template.body_expr = boundary.body_expr;
        self.noteMutation(.callable_templates);
        try self.requeueActiveScanUnits();
    }

    fn aliasCallableTemplateSource(
        self: *Pass,
        result: *Result,
        source_key: u64,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        const existing = result.lambdasolved.callable_template_ids_by_source.get(source_key);
        if (existing) |existing_template_id| {
            if (existing_template_id != template_id) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: conflicting callable template aliases for source key {d} (existing={d}, new={d})",
                        .{ source_key, @intFromEnum(existing_template_id), @intFromEnum(template_id) },
                    );
                }
                unreachable;
            }
            return;
        }

        try self.putTracked(
            .callable_template_aliases,
            &result.lambdasolved.callable_template_ids_by_source,
            source_key,
            template_id,
        );
    }

    fn recordContextPatternSourceExpr(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source: ExprRef,
    ) Allocator.Error!void {
        try self.writePatternValueOrigin(
            result,
            source_context,
            module_idx,
            pattern_idx,
            source,
        );

        const module_env = self.all_module_envs[module_idx];
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
            .as => |as_pat| try self.recordContextPatternSourceExpr(result, source_context, module_idx, as_pat.pattern, source),
            .nominal => |nominal_pat| try self.recordContextPatternSourceExpr(result, source_context, module_idx, nominal_pat.backing_pattern, source),
            .nominal_external => |nominal_pat| try self.recordContextPatternSourceExpr(result, source_context, module_idx, nominal_pat.backing_pattern, source),
            .tuple => |tuple_pat| {
                for (module_env.store.slicePatterns(tuple_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                    const elem_source = try self.extendExprRef(result, source, .{ .tuple_elem = @intCast(elem_index) });
                    try self.recordContextPatternSourceExpr(result, source_context, module_idx, elem_pattern_idx, elem_source);
                }
            },
            .applied_tag => |tag_pat| {
                for (module_env.store.slicePatterns(tag_pat.args), 0..) |arg_pattern_idx, arg_index| {
                    const arg_source = try self.extendExprRef(result, source, .{ .tag_payload = .{
                        .tag_name = .{
                            .module_idx = module_idx,
                            .ident = tag_pat.name,
                        },
                        .payload_index = @intCast(arg_index),
                    } });
                    try self.recordContextPatternSourceExpr(result, source_context, module_idx, arg_pattern_idx, arg_source);
                }
            },
            .record_destructure => |record_pat| {
                for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    switch (destruct.kind) {
                        .Required, .SubPattern => |sub_pattern_idx| {
                            const field_source = try self.extendExprRef(result, source, .{ .field = .{
                                .module_idx = module_idx,
                                .ident = destruct.label,
                            } });
                            try self.recordContextPatternSourceExpr(result, source_context, module_idx, sub_pattern_idx, field_source);
                        },
                        .Rest => {},
                    }
                }
            },
            .list => |list_pat| {
                for (module_env.store.slicePatterns(list_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                    const elem_source = try self.extendExprRef(result, source, .{ .list_elem = @intCast(elem_index) });
                    try self.recordContextPatternSourceExpr(result, source_context, module_idx, elem_pattern_idx, elem_source);
                }
            },
        }
    }

    fn recordExprSourceExpr(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        source: ExprRef,
    ) Allocator.Error!void {
        try self.writeExprValueOrigin(
            result,
            source_context,
            module_idx,
            expr_idx,
            source,
        );
    }

    fn registerCallableDefTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        kind: CallableTemplateKind,
        source_region: Region,
        alias_source_key: u64,
    ) Allocator.Error!CallableTemplateId {
        const template_id = try self.registerCallableTemplate(
            result,
            null,
            packExprSourceKey(module_idx, expr_idx),
            module_idx,
            expr_idx,
            type_root,
            binding_pattern,
            kind,
            source_region,
        );
        try self.aliasCallableTemplateSource(result, alias_source_key, template_id);
        return template_id;
    }

    fn registerCallableBackedDefTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        alias_source_key: u64,
    ) Allocator.Error!?CallableTemplateId {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        if (callableKind(expr)) |kind| {
            return try self.registerCallableDefTemplate(
                result,
                module_idx,
                expr_idx,
                type_root,
                binding_pattern,
                kind,
                module_env.store.getExprRegion(expr_idx),
                alias_source_key,
            );
        }

        const template_id = switch (expr) {
            .e_block => |block| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                block.final_expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_dbg => |dbg_expr| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                dbg_expr.expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_expect => |expect_expr| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                expect_expr.body,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_return => |return_expr| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                return_expr.expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_nominal => |nominal_expr| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                nominal_expr.backing_expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_nominal_external => |nominal_expr| try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                nominal_expr.backing_expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            else => null,
        };

        if (template_id) |id| {
            try self.aliasCallableTemplateSource(result, packExprSourceKey(module_idx, expr_idx), id);
        }

        return template_id;
    }

    fn scanStmt(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        stmt_idx: CIR.Statement.Idx,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const stmt = module_env.store.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                try self.recordContextPatternSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    decl.pattern,
                    exprRefInContext(thread.requireSourceContext(), module_idx, decl.expr),
                );
                try self.propagatePatternDemandToExpr(result, thread, module_idx, decl.pattern, decl.expr);
                if (try self.registerCallableBackedDefTemplate(
                    result,
                    module_idx,
                    decl.expr,
                    ModuleEnv.varFrom(decl.pattern),
                    decl.pattern,
                    packLocalPatternSourceKey(module_idx, decl.pattern),
                )) |_| {
                    try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, decl.expr, resolve_direct_calls);
                    try self.bindPatternCallableValueCallableInsts(result, thread, module_idx, decl.pattern, decl.expr);
                } else {
                    try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, decl.expr, resolve_direct_calls);
                    try self.bindPatternCallableValueCallableInsts(result, thread, module_idx, decl.pattern, decl.expr);
                    const expr_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, decl.expr);
                    if (!expr_mono.isNone()) {
                        try self.bindCurrentPatternFromResolvedMonotype(
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
                try self.recordContextPatternSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    var_decl.pattern_idx,
                    exprRefInContext(thread.requireSourceContext(), module_idx, var_decl.expr),
                );
                try self.propagatePatternDemandToExpr(result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
                if (try self.registerCallableBackedDefTemplate(
                    result,
                    module_idx,
                    var_decl.expr,
                    ModuleEnv.varFrom(var_decl.pattern_idx),
                    var_decl.pattern_idx,
                    packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                )) |_| {
                    try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, var_decl.expr, resolve_direct_calls);
                    try self.bindPatternCallableValueCallableInsts(result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
                } else {
                    try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, var_decl.expr, resolve_direct_calls);
                    try self.bindPatternCallableValueCallableInsts(result, thread, module_idx, var_decl.pattern_idx, var_decl.expr);
                    const expr_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, var_decl.expr);
                    if (!expr_mono.isNone()) {
                        try self.bindCurrentPatternFromResolvedMonotype(
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
                try self.propagatePatternDemandToExpr(result, thread, module_idx, reassign.pattern_idx, reassign.expr);
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, reassign.expr, resolve_direct_calls);
                const expr_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, reassign.expr);
                if (!expr_mono.isNone()) {
                    try self.bindCurrentPatternFromResolvedMonotype(
                        result,
                        thread,
                        module_idx,
                        reassign.pattern_idx,
                        expr_mono,
                    );
                }
            },
            .s_dbg => |dbg_stmt| try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, dbg_stmt.expr, resolve_direct_calls),
            .s_expr => |expr_stmt| try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, expr_stmt.expr, resolve_direct_calls),
            .s_expect => |expect_stmt| try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, expect_stmt.body, resolve_direct_calls),
            .s_for => |for_stmt| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, for_stmt.expr, resolve_direct_calls);
                const iter_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, for_stmt.expr);
                if (!iter_mono.isNone()) {
                    const item_mono = switch (result.context_mono.monotype_store.getMonotype(iter_mono.idx)) {
                        .list => |list| resolvedMonotype(list.elem, iter_mono.module_idx),
                        else => std.debug.panic(
                            "Pipeline invariant violated: for-loop expr {d} in module {d} had non-list exact monotype",
                            .{ @intFromEnum(for_stmt.expr), module_idx },
                        ),
                    };
                    try self.bindCurrentPatternFromResolvedMonotype(result, thread, module_idx, for_stmt.patt, item_mono);
                }
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, for_stmt.body, resolve_direct_calls);
            },
            .s_while => |while_stmt| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, while_stmt.cond, resolve_direct_calls);
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, while_stmt.body, resolve_direct_calls);
            },
            .s_return => |return_stmt| try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, return_stmt.expr, resolve_direct_calls),
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

    fn propagatePatternDemandToExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const pattern_mono = result.context_mono.getContextPatternMonotype(
            thread.requireSourceContext(),
            module_idx,
            pattern_idx,
        ) orelse return;
        if (pattern_mono.isNone()) return;

        try self.propagateDemandedValueMonotypeToValueExpr(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            pattern_mono.idx,
            pattern_mono.module_idx,
        );
    }

    fn bindPatternCallableValueCallableInsts(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
            try self.materializeLookupExprCallableValue(result, thread, module_idx, expr_idx, template_id);
        } else {
            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
            defer visiting.deinit(self.allocator);
            var variant_builder = CallableVariantBuilder.init();
            defer variant_builder.deinit(self.allocator);
            try self.includeExprCallableValue(
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                &visiting,
                &variant_builder,
            );
        }

        const callable_value = self.getValueExprCallableValueForSourceContext(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        ) orelse return;

        switch (callable_value) {
            .direct => |callable_inst_id| {
                const callable_inst = result.getCallableInst(callable_inst_id);
                try self.setCallableParamDirectValue(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    pattern_idx,
                    callable_inst_id,
                );
                try self.setExprDirectCallable(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    callable_inst_id,
                );
                try self.bindCurrentPatternFromResolvedMonotype(
                    result,
                    thread,
                    module_idx,
                    pattern_idx,
                    resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
                );
                try self.recordCurrentExprMonotype(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    callable_inst.fn_monotype,
                    callable_inst.fn_monotype_module_idx,
                );
            },
            .packed_fn => {
                try self.setExprCallableValue(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    callable_value,
                );
                try self.setCallableParamValue(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    pattern_idx,
                    callable_value,
                );
            },
        }
    }

    fn scanCirExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        return self.scanCirExprWithDirectCallResolution(result, thread, module_idx, expr_idx, true);
    }

    fn scanCirValueExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        return self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, expr_idx, true);
    }

    fn scanCirExprWithDirectCallResolution(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        return self.scanCirExprInternal(result, thread, module_idx, expr_idx, false, false, resolve_direct_calls);
    }

    fn scanCirValueExprWithDirectCallResolution(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        return self.scanCirExprInternal(result, thread, module_idx, expr_idx, true, false, resolve_direct_calls);
    }

    fn scanCirExprSpanWithDirectCallResolution(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        exprs: []const CIR.Expr.Idx,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, child_expr, resolve_direct_calls);
        }
    }

    fn scanCirValueExprSpanWithDirectCallResolution(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        exprs: []const CIR.Expr.Idx,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, child_expr, resolve_direct_calls);
        }
    }

    fn scanForcedCirValueExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        return self.scanCirExprInternal(result, thread, module_idx, expr_idx, true, true, true);
    }

    fn scanCirValueExprWithoutDirectCallResolution(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        return self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, expr_idx, false);
    }

    fn scanDemandedValueDefExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const key = self.resultExprKeyForThread(thread, module_idx, expr_idx);
        if (self.in_progress_value_defs.contains(key)) return;
        try self.in_progress_value_defs.put(self.allocator, key, {});
        defer _ = self.in_progress_value_defs.remove(key);

        try self.scanForcedCirValueExpr(result, thread, module_idx, expr_idx);

        if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
            try self.materializeDemandedExprCallableInst(result, thread, module_idx, expr_idx, template_id);
            if (self.getExprCallableValueForThread(result, thread, module_idx, expr_idx)) |callable_value| {
                try self.ensureRecordedCallableInstsScanned(result, callableAlternativesFromValue(result, callable_value));
            }
            return;
        }
    }

    fn scanDemandedValueDefExprInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        try self.scanDemandedValueDefExpr(
            result,
            SemanticThread.trackedThread(source_context),
            module_idx,
            expr_idx,
        );
    }

    fn scanForcedCirValueExprInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        try self.scanForcedCirValueExpr(
            result,
            SemanticThread.trackedThread(source_context),
            module_idx,
            expr_idx,
        );
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

    fn scanCirExprInternal(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        materialize_if_callable: bool,
        force_rescan_children: bool,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        const monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        if (!monotype.isNone()) {
            try self.recordExprMonotypeForThread(
                result,
                thread,
                module_idx,
                expr_idx,
                monotype.idx,
                monotype.module_idx,
            );
        }

        if (callableKind(expr)) |kind| {
            const template_id = try self.registerCallableTemplate(
                result,
                thread.requireSourceContext(),
                packExprSourceKey(module_idx, expr_idx),
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(expr_idx),
                null,
                kind,
                module_env.store.getExprRegion(expr_idx),
            );
            try self.writeExprTemplateSemantics(
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                template_id,
            );
            if (materialize_if_callable) {
                try self.materializeDemandedExprCallableInst(result, thread, module_idx, expr_idx, template_id);
                if (!force_rescan_children and !thread.hasCallableInst()) {
                    try self.completeCurrentExprMonotype(result, thread, module_idx, expr_idx);
                    return;
                }
            }
        }

        if (force_rescan_children) {
            try self.scanCirExprChildren(result, thread, module_idx, expr_idx, expr, resolve_direct_calls);
            try self.completeCurrentExprMonotype(result, thread, module_idx, expr_idx);
            return;
        }

        const visit_key = self.resultExprKeyForThread(thread, module_idx, expr_idx);
        if (self.visited_exprs.contains(visit_key)) return;
        try self.visited_exprs.put(self.allocator, visit_key, {});

        try self.scanCirExprChildren(result, thread, module_idx, expr_idx, expr, resolve_direct_calls);

        if (materialize_if_callable and callableKind(expr) == null) {
            if (self.getValueExprCallableValueForThread(result, thread, module_idx, expr_idx) == null) {
                var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                defer visiting.deinit(self.allocator);
                try self.realizeStructuredExprCallableSemantics(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    &visiting,
                );
            }
        }

        try self.completeCurrentExprMonotype(result, thread, module_idx, expr_idx);
    }

    fn completeCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const resolved = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        if (resolved.isNone()) return;
        try self.recordExprMonotypeForThread(
            result,
            thread,
            module_idx,
            expr_idx,
            resolved.idx,
            resolved.module_idx,
        );
    }

    fn materializeDemandedExprCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        if (templateRequiresConcreteOwnerCallableInst(result, template_id) and !thread.hasCallableInst())
        {
            return;
        }

        const source_context = thread.requireSourceContext();

        const fn_monotype = try self.resolveExprMonotypeResolved(
            result,
            thread,
            module_idx,
            expr_idx,
        );
        if (fn_monotype.isNone()) return;

        const callable_inst_id = try self.requireCallableInst(result, source_context, template_id, fn_monotype.idx, fn_monotype.module_idx);
        try self.setExprDirectCallable(
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_inst_id,
        );
    }

    fn templateRequiresConcreteOwnerCallableInst(
        result: *const Result,
        template_id: CallableTemplateId,
    ) bool {
        const template = result.getCallableTemplate(template_id);
        return template.kind == .closure and template.owner == .lexical_template;
    }

    fn setExprDirectCallableInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.setExprDirectCallable(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
            callable_inst_id,
        );
    }

    fn setExprDirectCallable(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        try self.ensureRecordedCallableInstsScanned(result, &.{callable_inst_id});
        const callable_inst = result.getCallableInst(callable_inst_id).*;
        try self.writeExprTemplateSemantics(
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_inst.template,
        );
        try self.writeExprCallableValue(
            result,
            source_context,
            module_idx,
            expr_idx,
            .{ .direct = callable_inst_id },
        );
        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
        );
    }

    fn setExprCallableValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        return self.setExprCallableValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
            callable_value,
        );
    }

    fn setExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        try self.ensureRecordedCallableInstsScanned(result, callableAlternativesFromValue(result, callable_value));
        try self.writeExprCallableValue(result, source_context, module_idx, expr_idx, callable_value);
        const callable_value_mono = callableValueMonotype(result, callable_value);
        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            expr_idx,
            callable_value_mono.idx,
            callable_value_mono.module_idx,
        );
    }

    fn lookupExprMonotypeForThread(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.lookupExprMonotypeForSourceContext(result, thread, thread.requireSourceContext(), module_idx, expr_idx);
    }

    fn lookupExprMonotypeForSourceContext(
        self: *Pass,
        result: *Result,
        _: SemanticThread,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        const key = self.resultExprKeyForSourceContext(source_context, module_idx, expr_idx);
        return result.context_mono.context_expr_monotypes.get(key);
    }

    fn exprUsesContextSensitiveNumericDefault(
        self: *Pass,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) bool {
        const expr = self.all_module_envs[module_idx].store.getExpr(expr_idx);
        return switch (expr) {
            .e_num,
            .e_dec,
            .e_dec_small,
            => true,
            else => false,
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

    fn callableInstCaptureGraphReaches(
        self: *Pass,
        result: *const Result,
        from_callable_inst_id: CallableInstId,
        target_callable_inst_id: CallableInstId,
        visited: *std.AutoHashMapUnmanaged(u32, void),
    ) Allocator.Error!bool {
        if (from_callable_inst_id == target_callable_inst_id) return true;

        const from_key = @intFromEnum(from_callable_inst_id);
        const entry = try visited.getOrPut(self.allocator, from_key);
        if (entry.found_existing) return false;

        const callable_def = result.getCallableDefForInst(from_callable_inst_id);
        for (result.getCaptureFields(callable_def.captures)) |capture_field| {
            switch (capture_field.callable_binding) {
                .non_callable => {},
                .callable => |callable_value| {
                    for (callableAlternativesFromValue(result, callable_value)) |capture_callable_inst_id| {
                        if (try self.callableInstCaptureGraphReaches(
                            result,
                            capture_callable_inst_id,
                            target_callable_inst_id,
                            visited,
                        )) {
                            return true;
                        }
                    }
                },
            }
        }

        return false;
    }

    fn callableInstSharesRecursiveEnvironment(
        self: *Pass,
        result: *const Result,
        callable_inst_id: CallableInstId,
        capture_callable_inst_id: CallableInstId,
    ) Allocator.Error!bool {
        var visited: std.AutoHashMapUnmanaged(u32, void) = .empty;
        defer visited.deinit(self.allocator);
        return self.callableInstCaptureGraphReaches(
            result,
            capture_callable_inst_id,
            callable_inst_id,
            &visited,
        );
    }

    fn templateSourceContext(template: CallableTemplate) SourceContext {
        return .{ .template_expr = .{
            .module_idx = template.module_idx,
            .expr_idx = template.runtime_expr,
        } };
    }

    fn exprRefInContext(
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

    fn exprRefAliasOrSelf(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ExprRef {
        _ = self;
        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |origin| {
            return origin;
        }
        return exprRefInContext(source_context, module_idx, expr_idx);
    }

    fn extendExprRef(
        self: *Pass,
        result: *Result,
        source: ExprRef,
        projection: CallableParamProjection,
    ) Allocator.Error!ExprRef {
        _ = self;
        return .{
            .source_context = source.source_context,
            .module_idx = source.module_idx,
            .expr_idx = source.expr_idx,
            .projections = try self.appendValueProjectionEntries(
                result,
                source.projections,
                &.{projection},
            ),
        };
    }

    fn updateCallableDefRuntimeValue(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        runtime_expr_ref: ExprRef,
        body_expr_ref: ExprRef,
        capture_fields: []const CaptureField,
    ) Allocator.Error!void {
        var runtime_field_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer runtime_field_monotypes.deinit(self.allocator);
        const callable_def = &result.lambdamono.callable_defs.items[@intFromEnum(result.getCallableInst(callable_inst_id).callable_def)];

        for (capture_fields) |capture_field| {
            switch (capture_field.storage) {
                .runtime_field => |runtime_field| {
                    const field_monotype = if (runtime_field.field_monotype.module_idx == callable_def.module_idx)
                        runtime_field.field_monotype.idx
                    else
                        try self.remapMonotypeBetweenModules(
                            result,
                            runtime_field.field_monotype.idx,
                            runtime_field.field_monotype.module_idx,
                            callable_def.module_idx,
                        );
                    try runtime_field_monotypes.append(self.allocator, field_monotype);
                },
                .callable_only, .recursive_member => {},
            }
        }

        callable_def.runtime_expr = runtime_expr_ref;
        callable_def.body_expr = body_expr_ref;

        const capture_span: CaptureFieldSpan = if (capture_fields.len == 0)
            .empty()
        else blk: {
            const start: u32 = @intCast(result.lambdamono.capture_fields.items.len);
            try result.lambdamono.capture_fields.appendSlice(self.allocator, capture_fields);
            break :blk .{
                .start = start,
                .len = @intCast(capture_fields.len),
            };
        };
        callable_def.captures = capture_span;

        result.lambdamono.callable_insts.items[@intFromEnum(callable_inst_id)].runtime_value =
            if (runtime_field_monotypes.items.len == 0)
                .direct_lambda
            else blk: {
                const field_span = try result.context_mono.monotype_store.addIdxSpan(self.allocator, runtime_field_monotypes.items);
                break :blk .{ .closure = .{
                    .capture_tuple_monotype = resolvedMonotype(
                        try result.context_mono.monotype_store.addMonotype(self.allocator, .{ .tuple = .{ .elems = field_span } }),
                        callable_def.module_idx,
                    ),
                } };
            };

    }

    fn closureCaptureAnalysisCallableInst(
        enclosing_source_context: SourceContext,
        closure_callable_inst_id: CallableInstId,
    ) CallableInstId {
        return switch (enclosing_source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => closure_callable_inst_id,
        };
    }

    fn captureValueSourceForClosurePattern(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_env: *const ModuleEnv,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) Allocator.Error!?CaptureValueSource {
        if (findDefByPattern(module_env, pattern_idx) != null) {
            return null;
        }
        if (result.getContextPatternSourceExpr(source_context, module_idx, pattern_idx)) |source| {
            return .{ .expr = source };
        }
        return .{ .lexical_pattern = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        } };
    }

    fn resolveCaptureValueSourceMonotype(
        self: *Pass,
        result: *Result,
        capture_context_callable_inst: CallableInstId,
        capture_value_source: CaptureValueSource,
    ) Allocator.Error!ResolvedMonotype {
        _ = self;
        return switch (capture_value_source) {
            .expr => |capture_expr_ref| blk: {
                var resolved = result.context_mono.getExprMonotype(
                    capture_expr_ref.source_context,
                    capture_expr_ref.module_idx,
                    capture_expr_ref.expr_idx,
                ) orelse std.debug.panic(
                    "Pipeline invariant violated: missing exact contextual monotype for capture expr source context={s} module={d} expr={d}",
                    .{
                        @tagName(capture_expr_ref.source_context),
                        capture_expr_ref.module_idx,
                        @intFromEnum(capture_expr_ref.expr_idx),
                    },
                );
                for (result.lambdamono.getValueProjectionEntries(capture_expr_ref.projections)) |projection| {
                    resolved = try self.projectResolvedMonotypeByValueProjection(result, resolved, projection);
                }
                break :blk resolved;
            },
            .lexical_pattern => |lexical| result.context_mono.getContextPatternMonotype(
                callableInstSourceContext(capture_context_callable_inst),
                lexical.module_idx,
                lexical.pattern_idx,
            ) orelse std.debug.panic(
                "Pipeline invariant violated: missing exact contextual monotype for lexical capture pattern module={d} pattern={d} context_callable_inst={d}",
                .{
                    lexical.module_idx,
                    @intFromEnum(lexical.pattern_idx),
                    @intFromEnum(capture_context_callable_inst),
                },
            ),
        };
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
                        break :blk resolvedMonotype(field.type_idx, source_monotype.module_idx);
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
                break :blk resolvedMonotype(elems[elem_index], source_monotype.module_idx);
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
                    break :blk resolvedMonotype(payload_monos[payload.payload_index], source_monotype.module_idx);
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
                break :blk resolvedMonotype(list.elem, source_monotype.module_idx);
            },
        };
    }

    fn finalizeCallableDefForCallableInst(
        self: *Pass,
        result: *Result,
        enclosing_source_context: SourceContext,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        closure_expr: CIR.Expr.Closure,
        closure_callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        var capture_fields = std.ArrayList(CaptureField).empty;
        defer capture_fields.deinit(self.allocator);

        for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
            const capture = module_env.store.getCapture(capture_idx);
            const capture_value_source = (try self.captureValueSourceForClosurePattern(
                result,
                enclosing_source_context,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue;
            const capture_context_callable_inst = closureCaptureAnalysisCallableInst(
                enclosing_source_context,
                closure_callable_inst_id,
            );
            const capture_mono = try self.resolveCaptureValueSourceMonotype(
                result,
                capture_context_callable_inst,
                capture_value_source,
            );
            if (capture_mono.isNone()) {
                std.debug.panic(
                    "Pipeline invariant violated: missing exact capture monotype for closure callable_inst={d} module={d} closure_expr={d} pattern={d}",
                    .{
                        @intFromEnum(closure_callable_inst_id),
                        module_idx,
                        @intFromEnum(closure_expr_idx),
                        @intFromEnum(capture.pattern_idx),
                    },
                );
            }

            const capture_callable_value = if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func)
                self.readCallableParamValue(
                    callableInstSourceContext(capture_context_callable_inst),
                    module_idx,
                    capture.pattern_idx,
                ) orelse std.debug.panic(
                    "Pipeline invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no executable callable value fact",
                    .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
                )
            else
                null;

            const capture_storage: Lambdamono.CaptureStorage = blk: {
                if (capture_callable_value) |callable_value| {
                    switch (callable_value) {
                        .direct => |capture_callable_inst_id| {
                            const capture_template = result.getCallableTemplate(
                                result.getCallableInst(capture_callable_inst_id).template,
                            );
                            switch (capture_template.binding) {
                                .pattern => |binding_pattern| if (binding_pattern == capture.pattern_idx and
                                    try self.callableInstSharesRecursiveEnvironment(
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
                    const callable_value = capture_callable_value orelse std.debug.panic(
                        "Pipeline invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no executable callable value",
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

            try capture_fields.append(self.allocator, .{
                .pattern_idx = capture.pattern_idx,
                .local_monotype = capture_mono,
                .callable_binding = if (capture_callable_value) |callable_value|
                    .{ .callable = callable_value }
                else
                    .non_callable,
                .source = capture_value_source,
                .storage = capture_storage,
            });
        }

        try self.updateCallableDefRuntimeValue(
            result,
            closure_callable_inst_id,
            .{
                .source_context = callableInstSourceContext(closure_callable_inst_id),
                .module_idx = module_idx,
                .expr_idx = closure_expr_idx,
            },
            .{
                .source_context = callableInstSourceContext(closure_callable_inst_id),
                .module_idx = module_idx,
                .expr_idx = switch (module_env.store.getExpr(closure_expr.lambda_idx)) {
                    .e_lambda => |lambda_expr| lambda_expr.body,
                    else => closure_expr.lambda_idx,
                },
            },
            capture_fields.items,
        );
    }

    fn scanCirExprChildren(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        resolve_direct_calls: bool,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

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
            .e_lookup_local => |lookup| {
                if (result.getContextPatternSourceExpr(thread.requireSourceContext(), module_idx, lookup.pattern_idx)) |source| {
                    try self.recordExprSourceExpr(result, thread.requireSourceContext(), module_idx, expr_idx, source);
                    try self.writeExprLookupResolution(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        .{ .expr = source },
                    );
                } else if (findDefByPattern(module_env, lookup.pattern_idx)) |def_idx| {
                    try self.writeExprLookupResolution(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        .{ .def = .{
                            .module_idx = module_idx,
                            .def_idx = def_idx,
                        } },
                    );
                }
                const is_top_level_def = isTopLevelDefPattern(module_env, lookup.pattern_idx);
                if (!is_top_level_def) {
                    if (self.resolveBoundPatternCallableInstForThread(result, thread, module_idx, lookup.pattern_idx)) |callable_inst_id| {
                        try self.setExprDirectCallable(
                            result,
                            thread.requireSourceContext(),
                            module_idx,
                            expr_idx,
                            callable_inst_id,
                        );
                    }
                }
                if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, thread, module_idx, expr_idx, template_id);
                } else if (!is_top_level_def) {
                    if (result.getExprValueOrigin(thread.requireSourceContext(), module_idx, expr_idx)) |source| {
                        if (source.projections.isEmpty()) {
                            try self.scanDemandedValueDefExprInSourceContext(
                                result,
                                source.source_context,
                                source.module_idx,
                                source.expr_idx,
                            );
                            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                            defer visiting.deinit(self.allocator);
                            try self.copyExprCallableValue(
                                result,
                                thread.requireSourceContext(),
                                module_idx,
                                expr_idx,
                                lookup.pattern_idx,
                                source.source_context,
                                source.module_idx,
                                source.expr_idx,
                                &visiting,
                            );
                        }
                    }
                }
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                try self.scanModule(result, target_module_idx);
                const target_env = self.all_module_envs[target_module_idx];
                if (!target_env.store.isDefNode(lookup.target_node_idx)) return;

                const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                try self.writeExprLookupResolution(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    .{ .def = .{
                        .module_idx = target_module_idx,
                        .def_idx = def_idx,
                    } },
                );
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    .{
                        .source_context = .{ .root_expr = .{
                            .module_idx = target_module_idx,
                            .expr_idx = def.expr,
                        } },
                        .module_idx = target_module_idx,
                        .expr_idx = def.expr,
                    },
                );
                _ = try self.registerCallableBackedDefTemplate(
                    result,
                    target_module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target_module_idx, lookup.target_node_idx),
                );
                if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, thread, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExprInSourceContext(
                        result,
                        .{ .root_expr = .{
                            .module_idx = target_module_idx,
                            .expr_idx = def.expr,
                        } },
                        target_module_idx,
                        def.expr,
                    );
                    var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                    defer visiting.deinit(self.allocator);
                    try self.copyExprCallableValue(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        null,
                        .{ .root_expr = .{
                            .module_idx = target_module_idx,
                            .expr_idx = def.expr,
                        } },
                        target_module_idx,
                        def.expr,
                        &visiting,
                    );
                }
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                try self.scanModule(result, target.module_idx);
                const target_env = self.all_module_envs[target.module_idx];
                const def = target_env.store.getDef(target.def_idx);
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                try self.writeExprLookupResolution(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    .{ .def = .{
                        .module_idx = target.module_idx,
                        .def_idx = target.def_idx,
                    } },
                );
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    .{
                        .source_context = .{ .root_expr = .{
                            .module_idx = target.module_idx,
                            .expr_idx = def.expr,
                        } },
                        .module_idx = target.module_idx,
                        .expr_idx = def.expr,
                    },
                );
                _ = try self.registerCallableBackedDefTemplate(
                    result,
                    target.module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target.module_idx, target_node_idx),
                );
                if (result.getExprTemplateId(thread.requireSourceContext(), module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, thread, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExprInSourceContext(
                        result,
                        .{ .root_expr = .{
                            .module_idx = target.module_idx,
                            .expr_idx = def.expr,
                        } },
                        target.module_idx,
                        def.expr,
                    );
                    var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                    defer visiting.deinit(self.allocator);
                    try self.copyExprCallableValue(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        expr_idx,
                        null,
                        .{ .root_expr = .{
                            .module_idx = target.module_idx,
                            .expr_idx = def.expr,
                        } },
                        target.module_idx,
                        def.expr,
                        &visiting,
                    );
                }
            },
            .e_str => |str_expr| try self.scanCirValueExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(str_expr.span), resolve_direct_calls),
            .e_list => |list_expr| try self.scanCirValueExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(list_expr.elems), resolve_direct_calls),
            .e_tuple => |tuple_expr| {
                try self.recordDemandedTupleElemMonotypes(result, thread, module_idx, expr_idx, tuple_expr);
                try self.scanCirValueExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(tuple_expr.elems), resolve_direct_calls);
            },
            .e_match => |match_expr| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, match_expr.cond, resolve_direct_calls);
                const cond_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, match_expr.cond);
                if (cond_mono.isNone()) return;

                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                        const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                        try self.recordContextPatternSourceExpr(result, thread.requireSourceContext(), module_idx, branch_pattern.pattern, .{
                            .source_context = thread.requireSourceContext(),
                            .module_idx = module_idx,
                            .expr_idx = match_expr.cond,
                        });
                        try self.bindCurrentPatternFromResolvedMonotype(
                            result,
                            module_idx,
                            branch_pattern.pattern,
                            cond_mono,
                        );
                    }
                    try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, branch.value);
                    try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, branch.value, resolve_direct_calls);
                    if (branch.guard) |guard_expr| {
                        try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, guard_expr, resolve_direct_calls);
                    }
                }
            },
            .e_if => |if_expr| {
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, branch.cond, resolve_direct_calls);
                    try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, branch.body);
                    try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, branch.body, resolve_direct_calls);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, if_expr.final_else);
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, if_expr.final_else, resolve_direct_calls);
            },
            .e_call => |call_expr| {
                const arg_exprs = module_env.store.sliceExpr(call_expr.args);
                if (self.getCallLowLevelOp(module_env, call_expr.func)) |low_level_op| {
                    if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                        try self.resolveStrInspectHelperCallableInstsForTypeVar(
                            result,
                            thread,
                            module_idx,
                            ModuleEnv.varFrom(arg_exprs[0]),
                        );
                        try self.recordExprMonotypeForThread(
                            result,
                            thread,
                            module_idx,
                            expr_idx,
                            result.context_mono.monotype_store.primIdx(.str),
                            module_idx,
                        );
                    }
                }
                try self.scanCirExprSpanWithDirectCallResolution(result, thread, module_idx, arg_exprs, resolve_direct_calls);
                if (resolve_direct_calls) {
                    try self.resolveDirectCallSite(result, thread, module_idx, expr_idx, call_expr);
                    if (self.getCallSiteForThread(result, thread, module_idx, expr_idx) == null) {
                        try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, call_expr.func, resolve_direct_calls);
                        try self.resolveDirectCallSite(result, thread, module_idx, expr_idx, call_expr);
                    }
                    try self.assignCallableArgCallableInstsFromCallMonotype(result, thread, module_idx, expr_idx, call_expr);
                } else {
                    try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, call_expr.func, resolve_direct_calls);
                }
                if (resolve_direct_calls) {
                    try self.assignCallableArgCallableInstsFromCallMonotype(result, thread, module_idx, expr_idx, call_expr);
                }
                if (resolve_direct_calls and
                    self.getCallSiteCallableInstForThread(result, thread, module_idx, expr_idx) == null)
                {
                    try self.resolveDirectCallSite(result, thread, module_idx, expr_idx, call_expr);
                }
            },
            .e_record => |record_expr| {
                if (record_expr.ext) |ext_expr| {
                    try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, ext_expr, resolve_direct_calls);
                }

                try self.recordDemandedRecordFieldMonotypes(result, thread, module_idx, expr_idx, record_expr);

                const fields = module_env.store.sliceRecordFields(record_expr.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, field.value, resolve_direct_calls);
                }
            },
            .e_block => |block_expr| {
                const stmts = module_env.store.sliceStatements(block_expr.stmts);
                try self.preRegisterBlockCallableStmtTemplates(result, module_idx, stmts);
                for (stmts) |stmt_idx| {
                    try self.scanStmt(result, thread, module_idx, stmt_idx, resolve_direct_calls);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, block_expr.final_expr);
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, block_expr.final_expr, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, block_expr.final_expr),
                );
            },
            .e_tag => |tag_expr| try self.scanCirValueExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(tag_expr.args), resolve_direct_calls),
            .e_nominal => |nominal_expr| {
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
                );
            },
            .e_nominal_external => |nominal_expr| {
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, nominal_expr.backing_expr, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, nominal_expr.backing_expr),
                );
            },
            .e_closure => |closure_expr| {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    const lambda_template_id = try self.registerCallableTemplate(
                        result,
                        thread.requireSourceContext(),
                        packExprSourceKey(module_idx, closure_expr.lambda_idx),
                        module_idx,
                        closure_expr.lambda_idx,
                        ModuleEnv.varFrom(closure_expr.lambda_idx),
                        null,
                        .lambda,
                        module_env.store.getExprRegion(closure_expr.lambda_idx),
                    );
                    try self.recordCallableTemplateRuntimeExpr(result, lambda_template_id, expr_idx);
                }
                // Callable bodies are scanned exclusively from `scanCallableInst`, after
                // the callable template/inst has established its own lexical owner and
                // defining-context chain. Generic expr traversal only records the
                // closure value's capture sources in the surrounding callable.
                try self.scanClosureCaptureSources(result, thread.requireSourceContext(), module_idx, expr_idx, closure_expr);
            },
            .e_lambda => {},
            .e_binop => |binop_expr| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, binop_expr.lhs, resolve_direct_calls);
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, binop_expr.rhs, resolve_direct_calls);
                try self.resolveDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr);
            },
            .e_unary_minus => |unary_expr| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, unary_expr.expr, resolve_direct_calls);
                try self.resolveDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr);
            },
            .e_unary_not => |unary_expr| try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, unary_expr.expr, resolve_direct_calls),
            .e_dot_access => |dot_expr| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, dot_expr.receiver, resolve_direct_calls);
                if (dot_expr.args) |args| {
                    try self.scanCirExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(args), resolve_direct_calls);
                    try self.resolveDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr);
                } else {
                    const receiver_source = self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dot_expr.receiver);
                    const field_source = try self.extendExprRef(result, receiver_source, .{ .field = .{
                        .module_idx = module_idx,
                        .ident = dot_expr.field_name,
                    } });
                    try self.recordExprSourceExpr(result, thread.requireSourceContext(), module_idx, expr_idx, field_source);
                }
            },
            .e_tuple_access => |tuple_access| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, tuple_access.tuple, resolve_direct_calls);
                const tuple_source = self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, tuple_access.tuple);
                const elem_source = try self.extendExprRef(result, tuple_source, .{ .tuple_elem = tuple_access.elem_index });
                try self.recordExprSourceExpr(result, thread.requireSourceContext(), module_idx, expr_idx, elem_source);
            },
            .e_dbg => |dbg_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, dbg_expr.expr);
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, dbg_expr.expr, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, dbg_expr.expr),
                );
            },
            .e_expect => |expect_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, thread, module_idx, expr_idx, expect_expr.body);
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, expect_expr.body, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, expect_expr.body),
                );
            },
            .e_return => |return_expr| {
                try self.scanCirValueExprWithDirectCallResolution(result, thread, module_idx, return_expr.expr, resolve_direct_calls);
                try self.recordExprSourceExpr(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    expr_idx,
                    self.exprRefAliasOrSelf(result, thread.requireSourceContext(), module_idx, return_expr.expr),
                );
            },
            .e_type_var_dispatch => |dispatch_expr| {
                try self.scanCirExprSpanWithDirectCallResolution(result, thread, module_idx, module_env.store.sliceExpr(dispatch_expr.args), resolve_direct_calls);
                try self.resolveDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr);
            },
            .e_for => |for_expr| {
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, for_expr.expr, resolve_direct_calls);
                try self.scanCirExprWithDirectCallResolution(result, thread, module_idx, for_expr.body, resolve_direct_calls);
            },
            .e_hosted_lambda => {},
            .e_run_low_level => |run_low_level| {
                const args = module_env.store.sliceExpr(run_low_level.args);
                try self.scanCirExprSpan(result, thread, module_idx, args);
                if (run_low_level.op == .str_inspect and args.len != 0) {
                    try self.resolveStrInspectHelperCallableInstsForTypeVar(
                        result,
                        thread,
                        module_idx,
                        ModuleEnv.varFrom(args[0]),
                    );
                    try self.recordExprMonotypeForThread(
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        result.context_mono.monotype_store.primIdx(.str),
                        module_idx,
                    );
                }
            },
        }
    }

    fn scanCirExprSpan(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirExpr(result, thread, module_idx, child_expr);
        }
    }

    fn scanCirValueExprSpan(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirValueExpr(result, thread, module_idx, child_expr);
        }
    }

    fn seedCallableInstContextTypeVarMonotypes(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id).*;
        const subst = result.getTypeSubst(callable_inst.subst);
        for (result.getTypeSubstEntries(subst.entries)) |entry| {
            try self.recordTypeVarMonotypeForSourceContext(
                result,
                callableInstSourceContext(callable_inst_id),
                entry.key.module_idx,
                entry.key.type_var,
                entry.monotype.idx,
                entry.monotype.module_idx,
            );
        }
    }

    fn seedCallableBoundaryContextMonotypes(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        callable_expr_idx: CIR.Expr.Idx,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!void {
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => return,
        };
        const boundary = self.callableBoundaryInfo(module_idx, callable_expr_idx) orelse return;
        const boundary_arg_patterns = self.all_module_envs[module_idx].store.slicePatterns(boundary.arg_patterns);
        if (boundary_arg_patterns.len != fn_mono.args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: callable boundary arity mismatch for expr {d} (patterns={d}, monos={d})",
                    .{
                        @intFromEnum(callable_expr_idx),
                        boundary_arg_patterns.len,
                        fn_mono.args.len,
                    },
                );
            }
            unreachable;
        }

        try self.recordTypeVarMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            ModuleEnv.varFrom(callable_expr_idx),
            fn_monotype,
            fn_monotype_module_idx,
        );
        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            callable_expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
        );

        for (boundary_arg_patterns, 0..) |pattern_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.recordTypeVarMonotypeForSourceContext(
                result,
                source_context,
                module_idx,
                ModuleEnv.varFrom(pattern_idx),
                param_mono,
                fn_monotype_module_idx,
            );
            try self.mergeTrackedContextPatternMonotype(
                result,
                self.resultPatternKeyForSourceContext(source_context, module_idx, pattern_idx),
                resolvedMonotype(param_mono, fn_monotype_module_idx),
            );
        }

        try self.recordTypeVarMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            ModuleEnv.varFrom(boundary.body_expr),
            fn_mono.ret,
            fn_monotype_module_idx,
        );
        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            boundary.body_expr,
            fn_mono.ret,
            fn_monotype_module_idx,
        );
    }

    fn scanClosureCaptureSources(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        closure_expr: CIR.Expr.Closure,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

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

            const capture_source = (try self.captureValueSourceForClosurePattern(
                result,
                source_context,
                module_env,
                module_idx,
                capture.pattern_idx,
            )) orelse continue;
            switch (capture_source) {
                .expr => |expr_ref| try self.scanDemandedValueDefExprInSourceContext(
                    result,
                    expr_ref.source_context,
                    expr_ref.module_idx,
                    expr_ref.expr_idx,
                ),
                .lexical_pattern => {},
            }
        }
    }

    fn preRegisterBlockCallableStmtTemplates(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        stmts: []const CIR.Statement.Idx,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_decl => |decl| {
                    if ((try self.registerCallableBackedDefTemplate(
                        result,
                        module_idx,
                        decl.expr,
                        ModuleEnv.varFrom(decl.pattern),
                        decl.pattern,
                        packLocalPatternSourceKey(module_idx, decl.pattern),
                    )) != null) {}
                },
                .s_var => |var_decl| {
                    if ((try self.registerCallableBackedDefTemplate(
                        result,
                        module_idx,
                        var_decl.expr,
                        ModuleEnv.varFrom(var_decl.pattern_idx),
                        var_decl.pattern_idx,
                        packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                    )) != null) {}
                },
                else => {},
            }
        }
    }

    fn callableInstSatisfiesCallSiteRequirements(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        desired_fn_monotype: ResolvedMonotype,
        required_template: ?CallableTemplateId,
        required_callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!bool {
        const callable_inst = result.getCallableInst(callable_inst_id);

        if (required_template) |template_id| {
            if (callable_inst.template != template_id) return false;
        }

        if (!desired_fn_monotype.isNone()) {
            if (!try self.monotypesStructurallyEqualAcrossModules(
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

    fn resolveDirectCallSite(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!void {
        const callee_expr_idx = call_expr.func;
        const module_env = self.all_module_envs[module_idx];
        const callee_expr = module_env.store.getExpr(callee_expr_idx);
        const desired_fn_monotype = resolvedMonotype(
            try self.resolveDirectCallFnMonotype(result, thread, module_idx, call_expr_idx, call_expr),
            module_idx,
        );
        if (!desired_fn_monotype.isNone()) {
            try self.bindCurrentCallFromFnMonotype(
                result,
                module_idx,
                call_expr_idx,
                call_expr,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
            );
        }
        const resolved_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, callee_expr_idx);
        if (resolved_template_id == null and !desired_fn_monotype.isNone()) {
            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.propagateDemandedCallableFnMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                callee_expr_idx,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                &visiting,
            );
        }
        if (self.getCallLowLevelOp(module_env, call_expr.func)) |low_level_op| {
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                try self.resolveStrInspectHelperCallableInstsForTypeVar(
                    result,
                    thread,
                    module_idx,
                    ModuleEnv.varFrom(arg_exprs[0]),
                );
            }
            return;
        }
        var required_callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
        defer required_callable_param_specs.deinit(self.allocator);
        if (!desired_fn_monotype.isNone()) {
            const arg_exprs = module_env.store.sliceExpr(call_expr.args);
            const specs_complete = try self.collectDirectCallCallableParamSpecs(
                result,
                thread.requireSourceContext(),
                module_idx,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                arg_exprs,
                &required_callable_param_specs,
            );
            if (!specs_complete) return;
        }

        if (self.getValueExprCallableInstForThread(result, thread, module_idx, callee_expr_idx)) |callable_inst_id| {
            if (try self.callableInstSatisfiesCallSiteRequirements(
                result,
                callable_inst_id,
                desired_fn_monotype,
                resolved_template_id,
                required_callable_param_specs.items,
            )) {
                try self.finalizeResolvedDirectCallCallableInst(
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

        if (self.getValueExprCallableValueForThread(result, thread, module_idx, callee_expr_idx)) |callable_value| {
            var all_satisfy = true;
            for (callableAlternativesFromValue(result, callable_value)) |callable_inst_id| {
                if (!try self.callableInstSatisfiesCallSiteRequirements(
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
                var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                defer visiting.deinit(self.allocator);
                var variant_builder = CallableVariantBuilder.init();
                defer variant_builder.deinit(self.allocator);
                var call_variant_builder = CallableVariantBuilder.init();
                defer call_variant_builder.deinit(self.allocator);
                try call_variant_builder.includeCallableValue(self, result, callable_value);
                try self.writeExprCallSite(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    call_expr_idx,
                    (try call_variant_builder.finishCallSite(self, result)).?,
                );
                for (callableAlternativesFromValue(result, callable_value)) |callable_inst_id| {
                    try self.recordCallResultCallableValueFromCallee(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        call_expr_idx,
                        callable_inst_id,
                        &visiting,
                        &variant_builder,
                    );
                }
                if (try variant_builder.finishValue(self, result)) |call_result_value| {
                    try self.setExprCallableValue(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        call_expr_idx,
                        call_result_value,
                    );
                }
                if (self.readExprCallSite(result, thread.requireSourceContext(), module_idx, call_expr_idx)) |call_site| {
                    if (exactCallableInstFromCallSite(call_site)) |direct_callable_inst_id| {
                        try self.finalizeResolvedDirectCallCallableInst(
                            result,
                            thread,
                            module_idx,
                            call_expr_idx,
                            call_expr,
                            callee_expr,
                            direct_callable_inst_id,
                        );
                    }
                }
                return;
            }
        }

        if (resolved_template_id) |template_id| {
            if (!(templateRequiresConcreteOwnerCallableInst(result, template_id) and
                !thread.hasCallableInst()))
            {
                if (try self.specializeDirectCallExactCallable(result, thread, module_idx, call_expr_idx, call_expr, template_id)) |callable_inst_id| {
                    try self.finalizeResolvedDirectCallCallableInst(
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

        const template_id = resolved_template_id orelse {
            switch (callee_expr) {
                .e_lookup_local, .e_lookup_external, .e_lookup_required => {
                    if (try self.callUsesAnnotationOnlyIntrinsic(module_idx, callee_expr_idx)) return;
                    if (!desired_fn_monotype.isNone()) {
                        try self.bindCurrentCallFromFnMonotype(
                            result,
                            thread,
                            module_idx,
                            call_expr_idx,
                            call_expr,
                            desired_fn_monotype.idx,
                            desired_fn_monotype.module_idx,
                        );
                    } else if (std.debug.runtime_safety and !thread.hasCallableInst()) {
                        if (!self.visited_exprs.contains(self.resultExprKeyForThread(thread, module_idx, callee_expr_idx))) {
                            return;
                        }
                        std.debug.panic(
                            "Pipeline missing direct-callee template for expr={d} module={d} callee_expr={d} tag={s}",
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
                            "Pipeline: demanded direct call expr {d} in module {d} has direct-callable callee expr {d} ({s}) without a callable template",
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
        if (templateRequiresConcreteOwnerCallableInst(result, template_id) and
            !thread.hasCallableInst())
        {
            return;
        }
        const callable_inst_id = try self.specializeDirectCallExactCallable(
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr,
            template_id,
        ) orelse {
            if (desired_fn_monotype.isNone() and std.debug.runtime_safety) {
                const template = result.getCallableTemplate(template_id);
                const template_env = self.all_module_envs[template.module_idx];
                const binding_name = switch (template.binding) {
                    .pattern => |binding_pattern| switch (template_env.store.getPattern(binding_pattern)) {
                        .assign => |assign_pat| template_env.getIdent(assign_pat.ident),
                        else => "<non-assign>",
                    },
                    .anonymous => "<anonymous>",
                };
                std.debug.panic(
                    "Pipeline unresolved direct callee '{s}' template={d} kind={s} template_module={d} template_expr={d} call_module={d} call_expr={d} context={d} root_source_expr={d}",
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
                    },
                );
            }
            return;
        };
        try self.finalizeResolvedDirectCallCallableInst(
            result,
            thread,
            module_idx,
            call_expr_idx,
            call_expr,
            callee_expr,
            callable_inst_id,
        );
    }

    fn finalizeResolvedDirectCallCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        callee_expr: anytype,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const source_context = thread.requireSourceContext();

        try self.setExprDirectCallSite(
            result,
            source_context,
            module_idx,
            call_expr_idx,
            callable_inst_id,
        );
        try self.setExprDirectCallable(
            result,
            source_context,
            module_idx,
            call_expr.func,
            callable_inst_id,
        );
        switch (callee_expr) {
            .e_lookup_local => |lookup| {
                try self.setExprDirectCallable(
                    result,
                    source_context,
                    module_idx,
                    call_expr.func,
                    callable_inst_id,
                );
                if (sourceContextHasCallableInst(source_context)) {
                    try self.setCallableParamDirectValue(
                        result,
                        source_context,
                        module_idx,
                        lookup.pattern_idx,
                        callable_inst_id,
                    );
                }
            },
            .e_lookup_external, .e_lookup_required => try self.setExprDirectCallable(
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
        try self.setExprDirectCallableInCallableContext(
            result,
            callable_inst_id,
            callee_template.module_idx,
            callee_template.cir_expr,
            callable_inst_id,
        );
        try self.bindCurrentCallFromCallableInst(result, module_idx, call_expr_idx, call_expr, callable_inst_id);
        const callable_inst_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        try self.prepareCallableArgsForCallableInst(result, thread, module_idx, arg_exprs, callable_inst_id);
        try self.scanCallableInst(result, callable_inst_id);
        const callable_def = result.getCallableDefForInst(callable_inst_id);
        try self.recordExprSourceExpr(
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
        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        var variant_builder = CallableVariantBuilder.init();
        defer variant_builder.deinit(self.allocator);
        try self.recordCallResultCallableValueFromCallee(
            result,
            source_context,
            module_idx,
            call_expr_idx,
            callable_inst_id,
            &visiting,
            &variant_builder,
        );
        if (try variant_builder.finishValue(self, result)) |call_result_value| {
            try self.setExprCallableValue(
                result,
                source_context,
                module_idx,
                call_expr_idx,
                call_result_value,
            );
        }
        try self.ensureCallableArgCallableInstsScanned(result, thread, module_idx, call_expr.args);
        try self.recordExprMonotypeForThread(
            result,
            thread,
            module_idx,
            call_expr_idx,
            callable_inst_fn_mono.ret,
            callable_inst.fn_monotype_module_idx,
        );
    }

    fn assignCallableArgCallableInstsFromParams(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        param_monos: Monotype.Span,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        if (arg_exprs.len != param_monos.len) unreachable;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(param_monos, i);
            const maybe_template_id = result.getExprTemplateId(thread.requireSourceContext(), module_idx, arg_expr_idx);
            const template_id = maybe_template_id orelse continue;
            const template = result.getCallableTemplate(template_id);
            if (!thread.hasCallableInst() and template.kind == .closure) {
                // During template-binding completion, a closure's callable identity is not
                // meaningful until the enclosing callable instantiation exists. Defer that
                // assignment to the later concrete callable-context scan.
                continue;
            }
            const callable_inst_id = try self.registerCallableInst(result, thread.requireSourceContext(), template_id, param_mono, fn_monotype_module_idx);

            try self.setExprDirectCallable(
                result,
                thread.requireSourceContext(),
                template.module_idx,
                template.cir_expr,
                callable_inst_id,
            );

            switch (module_env.store.getExpr(arg_expr_idx)) {
                .e_lookup_local => |lookup| {
                    try self.setExprDirectCallable(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        arg_expr_idx,
                        callable_inst_id,
                    );
                    try self.setCallableParamDirectValue(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        lookup.pattern_idx,
                        callable_inst_id,
                    );
                    try self.propagateLookupSourceExprCallableValue(result, thread, module_idx, arg_expr_idx, callable_inst_id);
                },
                .e_lookup_external, .e_lookup_required => {
                    try self.setExprDirectCallable(
                        result,
                        thread.requireSourceContext(),
                        module_idx,
                        arg_expr_idx,
                        callable_inst_id,
                    );
                    try self.propagateLookupSourceExprCallableValue(result, thread, module_idx, arg_expr_idx, callable_inst_id);
                },
            .e_lambda, .e_closure, .e_hosted_lambda => try self.setExprDirectCallable(
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

    fn callableBoundaryArgPatterns(
        self: *const Pass,
        module_idx: u32,
        callable_expr_idx: CIR.Expr.Idx,
    ) ?[]const CIR.Pattern.Idx {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(callable_expr_idx)) {
            .e_lambda => |lambda_expr| module_env.store.slicePatterns(lambda_expr.args),
            .e_closure => |closure_expr| blk: {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr != .e_lambda) break :blk null;
                break :blk module_env.store.slicePatterns(lambda_expr.e_lambda.args);
            },
            .e_hosted_lambda => |hosted_expr| module_env.store.slicePatterns(hosted_expr.args),
            else => null,
        };
    }

    fn seedCallableParamValuesFromActualArgs(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        callee_callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
        const template = result.getCallableTemplate(callee_callable_inst.template);
        const arg_patterns = self.callableBoundaryArgPatterns(template.module_idx, template.cir_expr) orelse return;
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
            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.realizeStructuredExprCallableSemantics(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                &visiting,
            );
            const callable_value = self.getValueExprCallableValueForSourceContext(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
            ) orelse continue;

            switch (callable_value) {
                .direct => |callable_inst_id| try self.setCallableParamDirectValueInCallableContext(
                    result,
                    callee_callable_inst_id,
                    template.module_idx,
                    pattern_idx,
                    callable_inst_id,
                ),
                .packed_fn => try self.setCallableParamValueInCallableContext(
                    result,
                    callee_callable_inst_id,
                    template.module_idx,
                    pattern_idx,
                    callable_value,
                ),
            }
        }
    }

    fn recordCallableParamPatternFactsForCallableInst(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id);
        if (callable_inst.callable_param_specs.isEmpty()) return;

        const template = result.getCallableTemplate(callable_inst.template);
        const arg_patterns = self.callableBoundaryArgPatterns(template.module_idx, template.cir_expr) orelse return;

        for (result.lambdamono.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
            if (!spec.projections.isEmpty()) continue;
            if (spec.param_index >= arg_patterns.len) {
                std.debug.panic(
                    "Pipeline: callable param spec index {d} out of bounds for callable inst {d} (arg_patterns={d})",
                    .{ spec.param_index, @intFromEnum(callable_inst_id), arg_patterns.len },
                );
            }

            switch (spec.callable_value) {
                .direct => |variant_callable_inst| try self.setCallableParamDirectValueInCallableContext(
                    result,
                    callable_inst_id,
                    template.module_idx,
                    arg_patterns[spec.param_index],
                    variant_callable_inst,
                ),
                .packed_fn => {
                    if (callableAlternativesFromValue(result, spec.callable_value).len == 0) {
                        std.debug.panic(
                            "Pipeline: callable param spec for callable inst {d} param {d} had an empty packed callable variant set",
                            .{ @intFromEnum(callable_inst_id), spec.param_index },
                        );
                    }

                    try self.setCallableParamValueInCallableContext(
                        result,
                        callable_inst_id,
                        template.module_idx,
                        arg_patterns[spec.param_index],
                        spec.callable_value,
                    );
                },
            }
        }
    }

    fn prepareCallableArgsForCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id);
        const callable_inst_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        try self.assignCallableArgCallableInstsFromParams(
            result,
            thread,
            module_idx,
            arg_exprs,
            callable_inst_fn_mono.args,
            callable_inst.fn_monotype_module_idx,
        );
        try self.seedCallableParamValuesFromActualArgs(
            result,
            thread,
            module_idx,
            arg_exprs,
            callable_inst_id,
        );
    }

    fn assignCallableArgCallableInstsFromCallMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!void {
        if (self.getCallSiteCallableInstForThread(result, thread, module_idx, call_expr_idx) != null) return;

        const call_fn_monotype = try self.resolveDirectCallFnMonotype(result, thread, module_idx, call_expr_idx, call_expr);
        if (call_fn_monotype.isNone()) return;

        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(call_fn_monotype)) {
            .func => |func| func,
            else => return,
        };

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) return;

        try self.assignCallableArgCallableInstsFromParams(
            result,
            thread,
            module_idx,
            arg_exprs,
            fn_mono.args,
            module_idx,
        );
        try self.ensureCallableArgCallableInstsScanned(result, thread, module_idx, call_expr.args);
    }

    fn ensureCallableArgCallableInstsScanned(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        args: CIR.Expr.Span,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
            try self.scanCirValueExpr(result, thread, module_idx, arg_expr_idx);
            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.realizeStructuredExprCallableSemantics(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                &visiting,
            );
            if (self.getValueExprCallableValueForThread(result, thread, module_idx, arg_expr_idx)) |callable_value| {
                try self.ensureRecordedCallableInstsScanned(result, callableAlternativesFromValue(result, callable_value));
            }
        }
    }

    fn ensureCallableArgCallableInstsScannedSlice(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (arg_exprs) |arg_expr_idx| {
            var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.realizeStructuredExprCallableSemantics(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                &visiting,
            );
            if (self.getValueExprCallableValueForThread(result, thread, module_idx, arg_expr_idx)) |callable_value| {
                try self.ensureRecordedCallableInstsScanned(result, callableAlternativesFromValue(result, callable_value));
            }
        }
    }

    const CallableBoundaryInfo = struct {
        arg_patterns: CIR.Pattern.Span,
        body_expr: CIR.Expr.Idx,
    };

    fn callableBoundaryInfo(
        self: *Pass,
        module_idx: u32,
        callable_expr_idx: CIR.Expr.Idx,
    ) ?CallableBoundaryInfo {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(callable_expr_idx)) {
            .e_lambda => |lambda_expr| .{
                .arg_patterns = lambda_expr.args,
                .body_expr = lambda_expr.body,
            },
            .e_closure => |closure_expr| blk: {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr != .e_lambda) break :blk null;
                break :blk .{
                    .arg_patterns = lambda_expr.e_lambda.args,
                    .body_expr = lambda_expr.e_lambda.body,
                };
            },
            .e_hosted_lambda => |hosted_expr| .{
                .arg_patterns = hosted_expr.args,
                .body_expr = hosted_expr.body,
            },
            else => null,
        };
    }

    fn resolveDirectCallFnMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!Monotype.Idx {
        const module_env = self.all_module_envs[module_idx];
        const effectful = if (resolveFuncTypeInStore(&module_env.types, ModuleEnv.varFrom(call_expr.func))) |func_info|
            func_info.effectful
        else
            false;
        return self.resolveAppliedCallableFnMonotype(
            result,
            thread,
            module_idx,
            call_expr_idx,
            module_env.store.sliceExpr(call_expr.args),
            effectful,
        );
    }

    fn resolveAppliedCallableFnMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        effectful: bool,
    ) Allocator.Error!Monotype.Idx {
        const ret_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        if (ret_monotype.isNone()) return .none;

        var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer arg_monotypes.deinit(self.allocator);

        for (arg_exprs) |arg_expr_idx| {
            const arg_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, arg_expr_idx);
            if (arg_monotype.isNone()) return .none;
            try arg_monotypes.append(self.allocator, arg_monotype.idx);
        }

        const arg_span = try result.context_mono.monotype_store.addIdxSpan(self.allocator, arg_monotypes.items);
        return result.context_mono.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = arg_span,
            .ret = ret_monotype.idx,
            .effectful = effectful,
        } });
    }

    fn resolveAppliedCallableFnMonotypeIfFullyBound(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        effectful: bool,
    ) Allocator.Error!Monotype.Idx {
        const ret_monotype = try self.lookupRecordedExprMonotypeIfReady(result, thread, module_idx, expr_idx);
        if (ret_monotype.isNone()) return .none;

        var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer arg_monotypes.deinit(self.allocator);

        for (arg_exprs) |arg_expr_idx| {
            const arg_monotype = try self.lookupRecordedExprMonotypeIfReady(result, thread, module_idx, arg_expr_idx);
            if (arg_monotype.isNone()) return .none;
            try arg_monotypes.append(self.allocator, arg_monotype.idx);
        }

        const arg_span = try result.context_mono.monotype_store.addIdxSpan(self.allocator, arg_monotypes.items);
        return result.context_mono.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = arg_span,
            .ret = ret_monotype.idx,
            .effectful = effectful,
        } });
    }

    fn specializeDirectCallExactCallable(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        template_id: CallableTemplateId,
    ) Allocator.Error!?CallableInstId {
        if (try self.directCallContainsErrorType(module_idx, call_expr_idx, call_expr)) {
            return null;
        }

        const template = result.getCallableTemplate(template_id).*;
        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        const exact_desired_fn_monotype = resolvedMonotype(
            try self.resolveAppliedCallableFnMonotypeIfFullyBound(
                result,
                thread,
                module_idx,
                call_expr_idx,
                arg_exprs,
                if (resolveFuncTypeInStore(&self.all_module_envs[module_idx].types, ModuleEnv.varFrom(call_expr.func))) |func_info|
                    func_info.effectful
                else
                    false,
            ),
            module_idx,
        );
        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, thread.requireSourceContext(), template);
        if (exact_desired_fn_monotype.isNone()) return null;
        const fn_monotype = exact_desired_fn_monotype.idx;

        if (!try self.procSignatureAcceptsFnMonotype(
            result,
            template_id,
            template,
            fn_monotype,
            template.module_idx,
            defining_source_context,
        )) return null;

        var callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
        defer callable_param_specs.deinit(self.allocator);
        const callable_param_specs_complete = try self.collectDirectCallCallableParamSpecs(
            result,
            thread.requireSourceContext(),
            module_idx,
            fn_monotype,
            template.module_idx,
            arg_exprs,
            &callable_param_specs,
        );
        if (!callable_param_specs_complete) return null;

        return try self.registerCallableInstWithCallableParamSpecs(
            result,
            thread.requireSourceContext(),
            template_id,
            fn_monotype,
            template.module_idx,
            callable_param_specs.items,
        );
    }

    fn collectDirectCallCallableParamSpecs(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        caller_module_idx: u32,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        out: *std.ArrayListUnmanaged(CallableParamSpecEntry),
    ) Allocator.Error!bool {
        const func_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => return true,
        };
        if (func_mono.args.len != arg_exprs.len) unreachable;

        var projections = std.ArrayListUnmanaged(CallableParamProjection).empty;
        defer projections.deinit(self.allocator);
        for (arg_exprs, 0..) |arg_expr_idx, param_index| {
            projections.clearRetainingCapacity();
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(func_mono.args, param_index);
            if (!try self.collectCallableParamSpecsFromArgument(
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

    fn collectCallableParamSpecsFromArgument(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
        param_index: u16,
        projections: *std.ArrayListUnmanaged(CallableParamProjection),
        out: *std.ArrayListUnmanaged(CallableParamSpecEntry),
    ) Allocator.Error!bool {
        try self.propagateDemandedValueMonotypeToValueExpr(
            result,
            source_context,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );

        switch (result.context_mono.monotype_store.getMonotype(monotype)) {
            .func => {
                try self.scanCirValueExpr(
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    expr_idx,
                );
                try self.realizeStructuredExprCallableSemantics(
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    &demand_visiting,
                );

                if (self.getValueExprCallableValueForSourceContext(result, source_context, module_idx, expr_idx)) |callable_value| {
                    try self.appendCallableParamSpecEntry(result, out, .{
                        .param_index = param_index,
                        .projections = try self.addCallableParamProjectionEntries(result, projections.items),
                        .callable_value = callable_value,
                    });
                    return true;
                }

                return false;
            },
            .record => |record| {
                for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                    try projections.append(self.allocator, .{ .field = field.name });
                    defer _ = projections.pop();

                    if (!try self.collectCallableParamSpecsFromArgument(
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
                    try projections.append(self.allocator, .{ .tuple_elem = @intCast(elem_index) });
                    defer _ = projections.pop();

                    if (!try self.collectCallableParamSpecsFromArgument(
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

    fn procSignatureAcceptsFnMonotype(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        template: CallableTemplate,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        defining_source_context: SourceContext,
    ) Allocator.Error!bool {
        _ = template_id;
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => return true,
        };
        const arg_patterns = self.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
        if (arg_patterns.len != fn_mono.args.len) return false;
        try self.seedCallableBoundaryContextMonotypes(
            result,
            defining_source_context,
            template.module_idx,
            template.cir_expr,
            fn_monotype,
            fn_monotype_module_idx,
        );
        return true;
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
        const resolved = try self.lookupRecordedExprMonotypeIfReady(result, thread, module_idx, expr_idx);
        if (!resolved.isNone()) return resolved;

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

    fn bindCurrentPatternFromResolvedMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        resolved_mono: ResolvedMonotype,
    ) Allocator.Error!void {
        if (resolved_mono.isNone()) return;
        try self.recordTypeVarMonotypeForThread(
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(pattern_idx),
            resolved_mono.idx,
            resolved_mono.module_idx,
        );
        try self.mergeTrackedContextPatternMonotype(
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
                        resolvedMonotype(payload_mono, resolved_mono.module_idx),
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
                                resolvedMonotype(mono_fields[field_idx].type_idx, resolved_mono.module_idx),
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
                        resolvedMonotype(elem_mono, resolved_mono.module_idx),
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
                        resolvedMonotype(elem_mono, resolved_mono.module_idx),
                    );
                }
            },
        }
    }

    fn bindCurrentCallFromCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id);
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) unreachable;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, thread, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
            );
        }

        try self.bindCurrentExprTypeRoot(result, thread, module_idx, call_expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, thread, module_idx, call_expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
    }

    fn bindCurrentCallFromFnMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!void {
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => return,
        };

        try self.bindCurrentCallableExprTypeRoot(
            result,
            thread,
            module_idx,
            call_expr.func,
            fn_monotype,
            fn_monotype_module_idx,
        );

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) return;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, thread, module_idx, arg_expr_idx, param_mono, fn_monotype_module_idx);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                fn_monotype_module_idx,
            );
        }

        try self.bindCurrentExprTypeRoot(result, thread, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, thread, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
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
        if (expr == .e_anno_only) return;

        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );

        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
            if (source.projections.isEmpty() and
                !(source.source_context == source_context and
                source.module_idx == module_idx and
                source.expr_idx == expr_idx))
            {
                try self.recordExprMonotypeForSourceContext(
                    result,
                    source.source_context,
                    source.module_idx,
                    source.expr_idx,
                    monotype,
                    monotype_module_idx,
                );
            }
        }
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
        if (expr == .e_anno_only) return;

        try self.recordExprMonotypeForSourceContext(
            result,
            source_context,
            module_idx,
            expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
        );

        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
            if (source.projections.isEmpty() and
                !(source.source_context == source_context and
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

    fn includeExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
        variant_builder: *CallableVariantBuilder,
    ) Allocator.Error!void {
        try self.realizeStructuredExprCallableSemantics(result, source_context, module_idx, expr_idx, visiting);
        if (self.getValueExprCallableValueForSourceContext(result, source_context, module_idx, expr_idx)) |callable_value| {
            try variant_builder.includeCallableValue(self, result, callable_value);
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
        try self.realizeStructuredExprCallableSemantics(
            result,
            source_context,
            source_module_idx,
            source_expr_idx,
            visiting,
        );
        const callable_value = self.getValueExprCallableValueForSourceContext(
            result,
            source_context,
            source_module_idx,
            source_expr_idx,
        ) orelse return;

        try self.setExprCallableValue(
            result,
            target_source_context,
            target_module_idx,
            target_expr_idx,
            callable_value,
        );
        if (maybe_pattern_idx) |pattern_idx| {
            try self.setCallableParamValue(
                result,
                target_source_context,
                target_module_idx,
                pattern_idx,
                callable_value,
            );
        }
    }

    fn recordCallResultCallableValueFromCallee(
        self: *Pass,
        result: *Result,
        target_source_context: SourceContext,
        target_module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        callee_callable_inst_id: CallableInstId,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
        variant_builder: *CallableVariantBuilder,
    ) Allocator.Error!void {
        const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
        const callee_fn_mono = switch (result.context_mono.monotype_store.getMonotype(callee_callable_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };
        if (result.context_mono.monotype_store.getMonotype(callee_fn_mono.ret) != .func) return;

        const in_progress_key = CallResultCallableInstKey{
            .context_expr = Result.contextExprKey(target_source_context, target_module_idx, call_expr_idx),
            .callee_callable_inst_raw = @intFromEnum(callee_callable_inst_id),
        };
        if (self.in_progress_call_result_callable_insts.contains(in_progress_key)) return;
        try self.in_progress_call_result_callable_insts.put(self.allocator, in_progress_key, {});
        defer _ = self.in_progress_call_result_callable_insts.remove(in_progress_key);

        const callable_def = result.getCallableDefForInst(callee_callable_inst_id);
        try self.includeExprCallableValue(
            result,
            callable_def.body_expr.source_context,
            callable_def.body_expr.module_idx,
            callable_def.body_expr.expr_idx,
            visiting,
            variant_builder,
        );
    }

    fn realizeStructuredExprCallableSemantics(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!void {
        const visit_key = contextExprVisitKey(source_context, module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx) != null) {
            return;
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
            if (source.projections.isEmpty() and
                !(source.source_context == source_context and
                source.module_idx == module_idx and
                source.expr_idx == expr_idx))
            {
                const maybe_pattern_idx = switch (expr) {
                    .e_lookup_local => |lookup| lookup.pattern_idx,
                    else => null,
                };
                try self.copyExprCallableValue(
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                    maybe_pattern_idx,
                    source.source_context,
                    source.module_idx,
                    source.expr_idx,
                    visiting,
                );
                return;
            }
        }

        var variant_builder = CallableVariantBuilder.init();
        defer variant_builder.deinit(self.allocator);
        switch (expr) {
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.includeExprCallableValue(result, source_context, module_idx, branch.body, visiting, &variant_builder);
                }
                try self.includeExprCallableValue(result, source_context, module_idx, if_expr.final_else, visiting, &variant_builder);
            },
            .e_match => |match_expr| {
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.includeExprCallableValue(result, source_context, module_idx, branch.value, visiting, &variant_builder);
                }
            },
            .e_call => |call_expr| {
                if (self.getCallLowLevelOp(module_env, call_expr.func) != null) return;

                var call_site = self.getCallSiteForSourceContext(
                    result,
                    source_context,
                    module_idx,
                    expr_idx,
                );
                if (call_site == null) {
                    try self.scanForcedCirValueExprInSourceContext(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                    );
                    call_site = self.getCallSiteForSourceContext(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                    );
                }

                const resolved_call_site = call_site orelse {
                    std.debug.panic(
                        "Pipeline invariant violated: callable-valued call expr {d} in module {d} had no explicit solved call-site in source context {s}",
                        .{
                            @intFromEnum(expr_idx),
                            module_idx,
                            @tagName(source_context),
                        },
                    );
                };

                for (callableAlternativesFromCallSite(result, resolved_call_site)) |callee_callable_inst_id| {
                    try self.recordCallResultCallableValueFromCallee(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                        callee_callable_inst_id,
                        visiting,
                        &variant_builder,
                    );
                }
                break;
            },
            .e_closure, .e_lambda, .e_hosted_lambda => {
                const template_id = result.getExprTemplateId(
                    source_context,
                    module_idx,
                    expr_idx,
                ) orelse std.debug.panic(
                    "Pipeline invariant violated: callable-capable expr {d} ({s}) in module {d} reached structured callable realization without a registered callable template",
                    .{
                        @intFromEnum(expr_idx),
                        @tagName(module_env.store.getExpr(expr_idx)),
                        module_idx,
                    },
                );
                try self.materializeLookupExprCallableValue(
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

        if (try variant_builder.finishValue(self, result)) |callable_value| {
            try self.setExprCallableValue(
                result,
                source_context,
                module_idx,
                expr_idx,
                callable_value,
            );
        }
    }

    fn appendDispatchActualArgs(
        self: *Pass,
        module_idx: u32,
        expr: CIR.Expr,
        actual_args: *std.ArrayList(CIR.Expr.Idx),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        switch (expr) {
            .e_binop => |binop_expr| {
                try actual_args.append(self.allocator, binop_expr.lhs);
                try actual_args.append(self.allocator, binop_expr.rhs);
            },
            .e_unary_minus => |unary_expr| {
                try actual_args.append(self.allocator, unary_expr.expr);
            },
            .e_dot_access => |dot_expr| {
                try actual_args.append(self.allocator, dot_expr.receiver);
                if (dot_expr.args) |args| {
                    try actual_args.appendSlice(self.allocator, module_env.store.sliceExpr(args));
                }
            },
            .e_type_var_dispatch => |dispatch_expr| {
                try actual_args.appendSlice(self.allocator, module_env.store.sliceExpr(dispatch_expr.args));
            },
            else => {},
        }
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
        const template = result.getCallableTemplate(template_id).*;
        const template_env = self.all_module_envs[template.module_idx];
        const template_types = &template_env.types;
        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, thread.requireSourceContext(), template);

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);

        const exact_desired_fn_monotype = resolvedMonotype(
            try self.resolveAppliedCallableFnMonotypeIfFullyBound(
                result,
                thread,
                module_idx,
                expr_idx,
                actual_args.items,
                if (resolveFuncTypeInStore(template_types, template.type_root)) |func_info|
                    func_info.effectful
                else
                    false,
            ),
            module_idx,
        );
        if (resolveFuncTypeInStore(template_types, template.type_root)) |resolved_func| {
            const param_vars = template_types.sliceVars(resolved_func.func.args);
            if (param_vars.len != actual_args.items.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: dispatch template arity mismatch at expr {d} (params={d}, args={d})",
                        .{
                            @intFromEnum(expr_idx),
                            param_vars.len,
                            actual_args.items.len,
                        },
                    );
                }
                unreachable;
            }
        }
        if (exact_desired_fn_monotype.isNone()) return null;
        const fn_monotype = exact_desired_fn_monotype.idx;

        if (!try self.procSignatureAcceptsFnMonotype(
            result,
            template_id,
            template,
            fn_monotype,
            template.module_idx,
            defining_source_context,
        )) return null;

        const callable_inst_id = try self.registerCallableInst(result, thread.requireSourceContext(), template_id, fn_monotype, template.module_idx);
        return callable_inst_id;
    }

    fn bindCurrentDispatchFromCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id);
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);

        if (actual_args.items.len != fn_mono.args.len) unreachable;

        for (actual_args.items, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, thread, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                thread.requireSourceContext(),
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
            );
        }

        try self.bindCurrentExprTypeRoot(result, thread, module_idx, expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, thread, module_idx, expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
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
        try self.recordTypeVarMonotypeForThread(
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(expr_idx),
            monotype,
            monotype_module_idx,
        );
        try self.recordExprMonotypeForThread(
            result,
            thread,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    fn bindCurrentCallableExprTypeRoot(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                try self.recordTypeVarMonotypeForThread(
                    result,
                    thread,
                    module_idx,
                    ModuleEnv.varFrom(lookup.pattern_idx),
                    monotype,
                    monotype_module_idx,
                );
                try self.mergeTrackedContextPatternMonotype(
                    result,
                    self.resultPatternKeyForThread(thread, module_idx, lookup.pattern_idx),
                    resolvedMonotype(monotype, monotype_module_idx),
                );
            },
            else => {},
        }
        try self.recordTypeVarMonotypeForThread(
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(expr_idx),
            monotype,
            monotype_module_idx,
        );
        try self.recordExprMonotypeForThread(
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
        try self.recordExprMonotypeForThread(
            result,
            thread,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );
    }

    fn recordExprMonotypeForThread(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        return self.recordExprMonotypeResolved(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
            thread,
        );
    }

    fn recordExprMonotypeForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        return self.recordExprMonotypeResolved(
            result,
            source_context,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
            null,
        );
    }

    fn recordExprMonotypeResolved(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
        thread: ?SemanticThread,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;
        const key = self.resultExprKeyForSourceContext(source_context, module_idx, expr_idx);
        const resolved = resolvedMonotype(monotype, monotype_module_idx);
        if (self.lookupExprMonotypeForSourceContext(result, source_context, module_idx, expr_idx)) |existing| {
            if (try self.monotypesStructurallyEqualAcrossModules(
                result,
                existing.idx,
                existing.module_idx,
                resolved.idx,
                resolved.module_idx,
            )) {
                return;
            }

            if (std.debug.runtime_safety) {
                const module_env = self.all_module_envs[module_idx];
                const expr = module_env.store.getExpr(expr_idx);
                const expr_region = module_env.store.getExprRegion(expr_idx);
                const source = module_env.getSourceAll();
                const snippet_start = @min(expr_region.start.offset, source.len);
                const snippet_end = @min(expr_region.end.offset, source.len);
                const context_template = result.getSourceContextTemplateId(source_context);
                std.debug.panic(
                    "Pipeline: conflicting exact expr monotypes for ctx={s} module={d} module_name={s} expr={d} kind={s} region={any} snippet=\"{s}\" existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} template_expr={d}",
                    .{
                        @tagName(source_context),
                        module_idx,
                        module_env.module_name,
                        @intFromEnum(expr_idx),
                        @tagName(expr),
                        expr_region,
                        source[snippet_start..snippet_end],
                        @intFromEnum(existing.idx),
                        existing.module_idx,
                        result.context_mono.monotype_store.getMonotype(existing.idx),
                        @intFromEnum(resolved.idx),
                        resolved.module_idx,
                        result.context_mono.monotype_store.getMonotype(resolved.idx),
                        if (context_template) |template_id| @intFromEnum(result.getCallableTemplate(template_id).cir_expr) else std.math.maxInt(u32),
                    },
                );
            }
            unreachable;
        }

        if (thread) |active_thread| {
            try self.mergeTrackedContextExprMonotype(result, key, resolved);
            return;
        }

        try self.mergeTrackedContextExprMonotype(result, key, resolved);
    }

    fn mergeTrackedContextExprMonotype(
        self: *Pass,
        result: *Result,
        key: ContextExprKey,
        resolved: ResolvedMonotype,
    ) Allocator.Error!void {
        return self.mergeTrackedResolvedMonotypeMap(
            result,
            .context_expr_monotypes,
            &result.context_mono.context_expr_monotypes,
            key,
            resolved,
            "exact expr",
        );
    }

    fn mergeTrackedContextPatternMonotype(
        self: *Pass,
        result: *Result,
        key: ContextPatternKey,
        resolved: ResolvedMonotype,
    ) Allocator.Error!void {
        return self.mergeTrackedResolvedMonotypeMap(
            result,
            .context_pattern_monotypes,
            &result.context_mono.context_pattern_monotypes,
            key,
            resolved,
            "exact pattern",
        );
    }

    fn mergeTrackedContextTypeVarMonotype(
        self: *Pass,
        result: *Result,
        key: ContextTypeVarKey,
        resolved: ResolvedMonotype,
    ) Allocator.Error!void {
        return self.mergeTrackedResolvedMonotypeMap(
            result,
            .context_type_var_monotypes,
            &result.context_mono.context_type_var_monotypes,
            key,
            resolved,
            "exact typevar",
        );
    }

    fn recordTypeVarMonotypeForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        type_var: types.Var,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;
        const key = self.resultTypeVarKeyForSourceContext(source_context, module_idx, self.all_module_envs[module_idx].types.resolveVar(type_var).var_);
        try self.mergeTrackedContextTypeVarMonotype(
            result,
            key,
            resolvedMonotype(monotype, monotype_module_idx),
        );
    }

    fn recordTypeVarMonotypeForThread(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        type_var: types.Var,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        return self.recordTypeVarMonotypeForSourceContext(
            result,
            thread.requireSourceContext(),
            module_idx,
            type_var,
            monotype,
            monotype_module_idx,
        );
    }

    fn mergeTrackedResolvedMonotypeMap(
        self: *Pass,
        result: *Result,
        comptime kind: MutationKind,
        map: anytype,
        key: anytype,
        resolved: ResolvedMonotype,
        comptime label: []const u8,
    ) Allocator.Error!void {
        const gop = try map.getOrPut(self.allocator, key);
        if (!gop.found_existing) {
            gop.value_ptr.* = resolved;
            self.noteMutation(kind);
            try self.requeueActiveScanUnits();
            return;
        }

        const existing = gop.value_ptr.*;
        if (try self.monotypesStructurallyEqualAcrossModules(
            result,
            existing.idx,
            existing.module_idx,
            resolved.idx,
            resolved.module_idx,
        )) {
            return;
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: conflicting {s} monotypes while merging key={any} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any}",
                .{
                    label,
                    key,
                    @intFromEnum(existing.idx),
                    existing.module_idx,
                    result.context_mono.monotype_store.getMonotype(existing.idx),
                    @intFromEnum(resolved.idx),
                    resolved.module_idx,
                    result.context_mono.monotype_store.getMonotype(resolved.idx),
                },
            );
        }
        unreachable;
    }

    fn recordDemandedRecordFieldMonotypes(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        record_expr: @TypeOf(@as(CIR.Expr, undefined).e_record),
    ) Allocator.Error!void {
        const record_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        if (record_mono.idx.isNone()) return;

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
        const tuple_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        if (tuple_mono.idx.isNone()) return;

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
        const parent_mono = try self.resolveExprMonotypeResolved(result, thread, module_idx, parent_expr_idx);
        if (parent_mono.isNone()) return;
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

    fn resolveDispatchExprCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        var associated_target: ?ResolvedDispatchTarget = null;
        if (expr == .e_binop) {
            const binop_expr = expr.e_binop;
            if (try self.binopDispatchHandledWithoutCallableInst(result, thread, module_idx, expr_idx, binop_expr)) {
                return;
            }
            const method_name = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse return;
            if (try self.resolveAssociatedMethodCallableInstForTypeVar(
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            )) |callable_inst_id| {
                try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                thread,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            );
            const lhs_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, binop_expr.lhs);
            if (associated_target == null and !lhs_monotype.isNone()) {
                if (try self.resolveAssociatedMethodCallableInstForMonotype(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    lhs_monotype.idx,
                    method_name,
                );
            }
        }

        if (expr == .e_dot_access) {
            const dot_expr = expr.e_dot_access;
            if (dot_expr.args != null) {
                if (try self.dotDispatchHandledWithoutCallableInst(result, thread, module_idx, expr_idx, dot_expr)) {
                    return;
                }
                if (try self.resolveAssociatedMethodCallableInstForTypeVar(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                );
                const receiver_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, dot_expr.receiver);
                if (associated_target == null and !receiver_monotype.isNone()) {
                    if (try self.resolveAssociatedMethodCallableInstForMonotype(
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    )) |callable_inst_id| {
                        try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                        return;
                    }
                    associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                        result,
                        thread,
                        module_idx,
                        expr_idx,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    );
                }
            }
        }

        if (associated_target == null and expr == .e_type_var_dispatch) {
            const dispatch_expr = expr.e_type_var_dispatch;
            const alias_stmt = module_env.store.getStatement(dispatch_expr.type_var_alias_stmt).s_type_var_alias;
            if (try self.resolveAssociatedMethodCallableInstForTypeVar(
                result,
                thread,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            )) |callable_inst_id| {
                try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                thread,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            );
            const alias_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, ModuleEnv.varFrom(alias_stmt.type_var_anno));
            if (associated_target == null and !alias_monotype.isNone()) {
                if (try self.resolveAssociatedMethodCallableInstForMonotype(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    expr,
                    alias_monotype.idx,
                    dispatch_expr.method_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    alias_monotype.idx,
                    dispatch_expr.method_name,
                );
            }
        }

        const resolved_target = if (associated_target) |target| target else switch (expr) {
            .e_binop => |binop_expr| blk: {
                const method_name = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse return;
                break :blk try self.resolveBinopDispatchTarget(result, module_idx, expr_idx, binop_expr, method_name);
            },
            .e_unary_minus => blk: {
                break :blk try self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, module_env.idents.negate);
            },
            .e_dot_access => |dot_expr| blk: {
                if (dot_expr.args == null) return;
                if (dotCallUsesRuntimeReceiver(module_env, dot_expr.receiver)) {
                    _ = try self.resolveExprMonotypeResolved(result, thread, module_idx, dot_expr.receiver);
                    break :blk try self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, dot_expr.field_name);
                }
                break :blk try self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, dot_expr.field_name);
            },
            .e_type_var_dispatch => |dispatch_expr| blk: {
                break :blk try self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, dispatch_expr.method_name);
            },
            else => return,
        };
        const target_def = try self.resolveDispatchTargetToExternalDef(module_idx, resolved_target);
            try self.recordDispatchExprTarget(
                result,
                thread.requireSourceContext(),
                module_idx,
                expr_idx,
                .{
                .module_idx = target_def.module_idx,
                .def_idx = @enumFromInt(target_def.def_node_idx),
            },
        );
        const template_id = try self.lookupResolvedDispatchTemplate(result, module_idx, resolved_target) orelse {
            if (std.debug.runtime_safety) {
                const method_name = self.dispatchTargetMethodText(module_env, resolved_target) orelse "<unreadable>";
                std.debug.panic(
                    "Pipeline: demanded dispatch expr {d} in module {d} resolved to method '{s}' without a callable template",
                    .{ @intFromEnum(expr_idx), module_idx, method_name },
                );
            }
            unreachable;
        };
        const callable_inst_id = blk: {
            if (try self.specializeDispatchExactCallable(result, thread, module_idx, expr_idx, expr, template_id)) |exact_callable_inst| {
                break :blk exact_callable_inst;
            }

            const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, resolved_target.fn_var);
            if (!fn_monotype.isNone()) {
                break :blk try self.registerCallableInst(result, thread.requireSourceContext(), template_id, fn_monotype.idx, fn_monotype.module_idx);
            }

            return;
        };
        try self.recordDispatchExprCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
    }

    fn recordDispatchExprCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const dispatch_target = self.dispatchTargetForCallableInst(result, callable_inst_id) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: dispatch expr {d} in module {d} resolved to callable inst {d} whose template {d} has no external-def source",
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
        try self.recordDispatchExprTarget(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            dispatch_target,
        );
        try self.setExprDirectCallSite(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            callable_inst_id,
        );
        const callable_inst = result.getCallableInst(callable_inst_id);
        const template = result.getCallableTemplate(callable_inst.template);
        try self.setExprDirectCallable(
            result,
            thread.requireSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );
        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);
        try self.prepareCallableArgsForCallableInst(result, thread, module_idx, actual_args.items, callable_inst_id);
        try self.scanCallableInst(result, callable_inst_id);
        try self.ensureCallableArgCallableInstsScannedSlice(result, thread, module_idx, actual_args.items);
        try self.bindCurrentDispatchFromCallableInst(result, thread, module_idx, expr_idx, expr, callable_inst_id);
    }

    fn dispatchTargetForCallableInst(
        self: *Pass,
        result: *const Result,
        callable_inst_id: CallableInstId,
    ) ?DispatchExprTarget {
        _ = self;
        const callable_inst = result.getCallableInst(callable_inst_id);
        const template = result.getCallableTemplate(callable_inst.template);
        const external_def = externalDefSourceFromSourceKey(template.source_key) orelse return null;
        return .{
            .module_idx = external_def.module_idx,
            .def_idx = external_def.def_idx,
        };
    }

    fn recordDispatchExprTarget(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        dispatch_target: DispatchExprTarget,
    ) Allocator.Error!void {
        try self.putTracked(
            .context_dispatch_targets,
            &result.context_mono.resolved_dispatch_targets,
            self.resultExprKeyForSourceContext(source_context, module_idx, expr_idx),
            dispatch_target,
        );
        const semantics = try self.ensureProgramExprSemantics(result, source_context, module_idx, expr_idx);
        const next_semantics: Lambdamono.ExprDispatchSemantics = .{ .dispatch = dispatch_target };
        if (!std.meta.eql(semantics.dispatch_semantics, next_semantics)) {
            semantics.dispatch_semantics = next_semantics;
            self.noteMutation(.context_dispatch_targets);
            try self.requeueActiveScanUnits();
        }
    }

    fn dotDispatchHandledWithoutCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        dot_expr: @TypeOf(@as(CIR.Expr, undefined).e_dot_access),
    ) Allocator.Error!bool {
        const module_env = self.all_module_envs[module_idx];
        if (!dotCallUsesRuntimeReceiver(module_env, dot_expr.receiver)) return false;
        if (std.mem.eql(u8, module_env.getIdent(dot_expr.field_name), "to_str")) {
            const receiver_monotype = try self.resolveExprMonotype(result, thread, module_idx, dot_expr.receiver);
            if (receiver_monotype.isNone()) {
                return self.dispatchHandledAsPrimitiveToStrIntrinsic(
                    result,
                    thread,
                    module_idx,
                    expr_idx,
                    dot_expr.field_name,
                );
            }
            return switch (result.context_mono.monotype_store.getMonotype(receiver_monotype)) {
                .prim => |prim| primitiveUsesIntrinsicToStr(prim),
                else => false,
            };
        }
        if (!dot_expr.field_name.eql(module_env.idents.is_eq)) return false;

        const receiver_monotype = try self.resolveExprMonotype(result, thread, module_idx, dot_expr.receiver);
        if (receiver_monotype.isNone()) {
            const eq_constraint = try self.exactStaticDispatchConstraintForExpr(result, thread, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(result, thread, module_idx, expr_idx) == null and !constraint_resolved;
        }

        return switch (result.context_mono.monotype_store.getMonotype(receiver_monotype)) {
            .record, .tuple, .list, .unit => true,
            .tag_union => self.lookupResolvedDispatchTarget(result, thread, module_idx, expr_idx) == null,
            else => false,
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

    fn dispatchHandledAsPrimitiveToStrIntrinsic(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) Allocator.Error!bool {
        const module_env = self.all_module_envs[module_idx];
        var saw_matching_constraint = false;

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (!constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
            {
                return false;
            }

            const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);
            if (fn_monotype.isNone()) return false;

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
                .prim => |ret_prim| if (ret_prim != .str) return false,
                else => return false,
            }

            saw_matching_constraint = true;
        }

        return saw_matching_constraint;
    }

    fn binopDispatchHandledWithoutCallableInst(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        binop_expr: CIR.Expr.Binop,
    ) Allocator.Error!bool {
        if (binop_expr.op != .eq and binop_expr.op != .ne) return false;

        const module_env = self.all_module_envs[module_idx];
        const lhs_monotype = try self.resolveExprMonotype(result, thread, module_idx, binop_expr.lhs);
        if (lhs_monotype.isNone()) {
            const eq_constraint = try self.exactStaticDispatchConstraintForExpr(result, thread, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(result, thread, module_idx, expr_idx) == null and !constraint_resolved;
        }

        const lhs_mono = result.context_mono.monotype_store.getMonotype(lhs_monotype);
        return switch (lhs_mono) {
            .record, .tuple, .list, .unit, .prim => true,
            .tag_union => blk: {
                const cached_dispatch = self.lookupResolvedDispatchTarget(result, thread, module_idx, expr_idx);
                break :blk cached_dispatch == null;
            },
            else => false,
        };
    }

    fn resolveBinopDispatchTarget(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        binop_expr: CIR.Expr.Binop,
        method_name: Ident.Idx,
    ) Allocator.Error!ResolvedDispatchTarget {
        const lhs_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, binop_expr.lhs);
        if (lhs_monotype.isNone()) {
            return self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, method_name);
        }

        try self.recordTypeVarMonotypeForThread(
            result,
            thread,
            module_idx,
            ModuleEnv.varFrom(binop_expr.rhs),
            lhs_monotype.idx,
            lhs_monotype.module_idx,
        );
        return self.resolveDispatchTargetForExpr(result, thread, module_idx, expr_idx, method_name);
    }

    fn resolveAssociatedMethodCallableInstForTypeVar(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        receiver_type_var: types.Var,
        method_ident: Ident.Idx,
    ) Allocator.Error!?CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        const receiver_nominal = resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
        const method_info = try self.lookupAssociatedMethodTemplate(result, module_idx, receiver_nominal, method_ident) orelse return null;
        const receiver_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, receiver_type_var);
        _ = try self.exactAssociatedMethodDispatchConstraint(
            result,
            thread,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype.idx,
        ) orelse return null;
        return try self.specializeDispatchExactCallable(result, thread, module_idx, expr_idx, expr, method_info.template_id);
    }

    fn resolveAssociatedMethodDispatchTargetForTypeVar(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        receiver_type_var: types.Var,
        method_ident: Ident.Idx,
    ) Allocator.Error!?ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const receiver_nominal = resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
        const method_info = try self.lookupAssociatedMethodTemplate(result, module_idx, receiver_nominal, method_ident) orelse return null;
        const receiver_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, receiver_type_var);
        const constraint = try self.exactAssociatedMethodDispatchConstraint(
            result,
            thread,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype.idx,
        ) orelse return null;
        return .{
            .origin = method_info.target_env.qualified_module_ident,
            .method_ident = method_info.qualified_method_ident,
            .fn_var = constraint.fn_var,
            .module_idx = method_info.module_idx,
        };
    }

    fn resolveAssociatedMethodCallableInstForMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        receiver_monotype: Monotype.Idx,
        method_ident: Ident.Idx,
    ) Allocator.Error!?CallableInstId {
        const method_info = try self.lookupAssociatedMethodTemplateForMonotype(
            result,
            module_idx,
            receiver_monotype,
            method_ident,
        ) orelse return null;
        _ = try self.exactAssociatedMethodDispatchConstraint(
            result,
            thread,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype,
        ) orelse return null;
        return try self.specializeDispatchExactCallable(result, thread, module_idx, expr_idx, expr, method_info.template_id);
    }

    fn resolveAssociatedMethodDispatchTargetForMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        receiver_monotype: Monotype.Idx,
        method_ident: Ident.Idx,
    ) Allocator.Error!?ResolvedDispatchTarget {
        const method_info = try self.lookupAssociatedMethodTemplateForMonotype(
            result,
            module_idx,
            receiver_monotype,
            method_ident,
        ) orelse return null;
        const constraint = try self.exactAssociatedMethodDispatchConstraint(
            result,
            thread,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype,
        ) orelse return null;
        return .{
            .origin = method_info.target_env.qualified_module_ident,
            .method_ident = method_info.qualified_method_ident,
            .fn_var = constraint.fn_var,
            .module_idx = method_info.module_idx,
        };
    }

    fn lookupResolvedDispatchTarget(
        self: *Pass,
        result: *const Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return result.context_mono.getDispatchExprTarget(
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn exactStaticDispatchConstraintForExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) Allocator.Error!?types.StaticDispatchConstraint {
        const module_env = self.all_module_envs[module_idx];
        var matched: ?types.StaticDispatchConstraint = null;

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (!constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
            {
                const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module).?;
                const resolved_target = ResolvedDispatchTarget{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                    .module_idx = target_module_idx,
                };
                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);
                if (!fn_monotype.isNone() and
                    !try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, resolved_target, fn_monotype.idx))
                {
                    continue;
                }
                if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, thread, module_idx, expr_idx, resolved_target)) continue;
            } else {
                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);
                if (fn_monotype.isNone()) continue;
            }

            if (matched) |existing| {
                if (staticDispatchConstraintsEqual(existing, constraint) or
                    try self.dispatchConstraintsEquivalent(result, thread, module_idx, existing, constraint))
                {
                    matched = if (!existing.resolved_target.isNone())
                        existing
                    else
                        constraint;
                    continue;
                }

                const existing_mono = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, existing.fn_var);
                const next_mono = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);

                if (!existing_mono.isNone() and !next_mono.isNone() and
                    try self.monotypesStructurallyEqualAcrossModules(
                        result,
                        existing_mono.idx,
                        existing_mono.module_idx,
                        next_mono.idx,
                        next_mono.module_idx,
                    ))
                {
                    const existing_target_usable = !existing.resolved_target.isNone() and
                        self.resolvedTargetIsUsable(module_env, method_name, existing.resolved_target);
                    const next_target_usable = !constraint.resolved_target.isNone() and
                        self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target);

                    if (existing_target_usable and next_target_usable and
                        !existing.resolved_target.origin_module.eql(constraint.resolved_target.origin_module))
                    {
                        std.debug.panic(
                            "Pipeline invariant violated: dispatch expr={d} method='{s}' had conflicting resolved targets for the same function monotype",
                            .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                        );
                    }

                    matched = if (existing_target_usable) existing else if (next_target_usable) constraint else existing;
                    continue;
                }

                std.debug.panic(
                    "Pipeline invariant violated: dispatch expr={d} method='{s}' had multiple non-equivalent surviving static dispatch constraints (existing_var={d} existing_mono={d}@{d} existing_shape={any} existing_target={any} next_var={d} next_mono={d}@{d} next_shape={any} next_target={any})",
                    .{
                        @intFromEnum(expr_idx),
                        module_env.getIdent(method_name),
                        @intFromEnum(existing.fn_var),
                        @intFromEnum(existing_mono.idx),
                        existing_mono.module_idx,
                        result.context_mono.monotype_store.getMonotype(existing_mono.idx),
                        existing.resolved_target,
                        @intFromEnum(constraint.fn_var),
                        @intFromEnum(next_mono.idx),
                        next_mono.module_idx,
                        result.context_mono.monotype_store.getMonotype(next_mono.idx),
                        constraint.resolved_target,
                    },
                );
            } else {
                matched = constraint;
            }
        }

        return matched;
    }

    fn exactAssociatedMethodDispatchConstraint(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
        method_info: AssociatedMethodTemplate,
        receiver_monotype: Monotype.Idx,
    ) Allocator.Error!?types.StaticDispatchConstraint {
        const module_env = self.all_module_envs[module_idx];
        const target_method_text = method_info.target_env.getIdent(method_info.qualified_method_ident);
        var matched: ?types.StaticDispatchConstraint = null;

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (!constraint.resolved_target.isNone()) {
                const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module) orelse continue;
                if (target_module_idx != method_info.module_idx) continue;
                if (!std.mem.eql(u8, module_env.getIdent(constraint.resolved_target.method_ident), target_method_text)) continue;
            } else {
                if (receiver_monotype.isNone()) continue;

                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);
                if (fn_monotype.isNone()) continue;

                const mono = result.context_mono.monotype_store.getMonotype(fn_monotype.idx);
                if (mono != .func) continue;

                const fn_args = result.context_mono.monotype_store.getIdxSpan(mono.func.args);
                if (fn_args.len == 0) continue;

                if (!try self.monotypeDispatchCompatible(
                    result,
                    fn_args[0],
                    module_idx,
                    receiver_monotype,
                    fn_monotype.module_idx,
                )) continue;
            }

            if (matched) |existing| {
                if (staticDispatchConstraintsEqual(existing, constraint) or
                    try self.dispatchConstraintsEquivalent(result, thread, module_idx, existing, constraint))
                {
                    continue;
                }

                const existing_exact = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, existing.fn_var);
                const next_exact = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);

                if (!existing_exact.isNone() and !next_exact.isNone()) {
                    if (!try self.monotypesStructurallyEqualAcrossModules(
                        result,
                        existing_exact.idx,
                        existing_exact.module_idx,
                        next_exact.idx,
                        next_exact.module_idx,
                    )) {
                        std.debug.panic(
                            "Pipeline invariant violated: associated dispatch expr={d} method='{s}' had conflicting exact function monotypes",
                            .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                        );
                    }
                }
            } else {
                matched = constraint;
            }
        }

        return matched;
    }

    fn staticDispatchConstraintsEqual(
        lhs: types.StaticDispatchConstraint,
        rhs: types.StaticDispatchConstraint,
    ) bool {
        return lhs.fn_name.eql(rhs.fn_name) and
            lhs.fn_var == rhs.fn_var and
            lhs.origin == rhs.origin and
            std.meta.eql(lhs.num_literal, rhs.num_literal) and
            lhs.source_expr_idx == rhs.source_expr_idx and
            lhs.resolved_target.origin_module.eql(rhs.resolved_target.origin_module) and
            lhs.resolved_target.method_ident.eql(rhs.resolved_target.method_ident);
    }

    fn dispatchConstraintsEquivalent(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        lhs: types.StaticDispatchConstraint,
        rhs: types.StaticDispatchConstraint,
    ) Allocator.Error!bool {
        if (!lhs.fn_name.eql(rhs.fn_name)) return false;
        if (lhs.source_expr_idx != rhs.source_expr_idx) return false;
        if (!lhs.resolved_target.origin_module.eql(rhs.resolved_target.origin_module)) return false;
        if (!lhs.resolved_target.method_ident.eql(rhs.resolved_target.method_ident)) return false;

        const lhs_mono = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, lhs.fn_var);
        const rhs_mono = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, rhs.fn_var);
        if (lhs_mono.isNone() or rhs_mono.isNone()) return false;

        return self.monotypesStructurallyEqualAcrossModules(
            result,
            lhs_mono.idx,
            lhs_mono.module_idx,
            rhs_mono.idx,
            rhs_mono.module_idx,
        );
    }

    fn resolvedTargetIsUsable(
        self: *Pass,
        source_env: *const ModuleEnv,
        method_name: Ident.Idx,
        resolved_target: types.StaticDispatchConstraint.ResolvedTarget,
    ) bool {
        const method_name_text = source_env.getIdent(method_name);
        const target_method_text = self.identTextAcrossModules(source_env, resolved_target.method_ident) orelse return false;
        if (!identMatchesMethodName(target_method_text, method_name_text)) return false;
        return self.findModuleForOriginMaybe(source_env, resolved_target.origin_module) != null;
    }

    fn resolveDispatchTargetForExpr(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) Allocator.Error!ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const constraint = try self.exactStaticDispatchConstraintForExpr(result, thread, module_idx, expr_idx, method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: no static dispatch constraint for expr={d} method='{s}'",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }
            unreachable;
        };

        const desired_func_monotype = try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, constraint.fn_var);
        if (constraint.resolved_target.isNone()) {
            std.debug.panic(
                "Pipeline invariant violated: dispatch expr={d} method='{s}' reached specialization without an exact resolved target",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        if (!self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target)) {
            std.debug.panic(
                "Pipeline invariant violated: dispatch expr={d} method='{s}' resolved to an unusable target",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }

        const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module).?;
        const resolved_target = ResolvedDispatchTarget{
            .origin = constraint.resolved_target.origin_module,
            .method_ident = constraint.resolved_target.method_ident,
            .fn_var = constraint.fn_var,
            .module_idx = target_module_idx,
        };
        if (!desired_func_monotype.isNone() and
            !try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, resolved_target, desired_func_monotype.idx))
        {
            std.debug.panic(
                "Pipeline invariant violated: dispatch expr={d} method='{s}' resolved target did not match the exact function monotype",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, thread, module_idx, expr_idx, resolved_target)) {
            std.debug.panic(
                "Pipeline invariant violated: dispatch expr={d} method='{s}' resolved target did not match the invocation signature",
                .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
            );
        }
        return resolved_target;
    }

    fn monotypeDispatchCompatible(
        self: *Pass,
        result: *const Result,
        expected: Monotype.Idx,
        expected_module_idx: u32,
        actual: Monotype.Idx,
        actual_module_idx: u32,
    ) Allocator.Error!bool {
        if (expected.isNone() or actual.isNone()) return true;
        return self.monotypesStructurallyEqualAcrossModules(
            result,
            expected,
            expected_module_idx,
            actual,
            actual_module_idx,
        );
    }

    fn resolvedDispatchTargetMatchesMonotype(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        target: ResolvedDispatchTarget,
        desired_func_monotype: Monotype.Idx,
    ) Allocator.Error!bool {
        if (desired_func_monotype.isNone()) return true;

        const target_def = try self.resolveDispatchTargetToExternalDef(source_module_idx, target);
        const target_env = self.all_module_envs[target_def.module_idx];
        const def_idx: CIR.Def.Idx = @enumFromInt(target_def.def_node_idx);
        const def = target_env.store.getDef(def_idx);
        const target_mono = try self.resolveExprMonotype(result, target_def.module_idx, def.expr);
        if (target_mono.isNone()) return false;

        return self.monotypesStructurallyEqualAcrossModules(
            result,
            target_mono,
            target_def.module_idx,
            desired_func_monotype,
            source_module_idx,
        );
    }

    fn resolvedDispatchTargetMatchesInvocationSignature(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        source_module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        target: ResolvedDispatchTarget,
    ) Allocator.Error!bool {
        const source_env = self.all_module_envs[source_module_idx];
        const source_expr = source_env.store.getExpr(expr_idx);

        const target_def = try self.resolveDispatchTargetToExternalDef(source_module_idx, target);
        const target_env = self.all_module_envs[target_def.module_idx];
        const def_idx: CIR.Def.Idx = @enumFromInt(target_def.def_node_idx);
        const def = target_env.store.getDef(def_idx);
        const target_mono = try self.resolveExprMonotype(result, thread, target_def.module_idx, def.expr);
        if (target_mono.isNone()) return false;

        const target_func = switch (result.context_mono.monotype_store.getMonotype(target_mono)) {
            .func => |func| func,
            else => return false,
        };

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(source_module_idx, source_expr, &actual_args);

        if (target_func.args.len != actual_args.items.len) return false;

        for (actual_args.items, 0..) |arg_expr_idx, i| {
            const actual_mono = try self.resolveExprMonotypeResolved(result, thread, source_module_idx, arg_expr_idx);
            if (actual_mono.isNone()) continue;

            const expected_mono = result.context_mono.monotype_store.getIdxSpanItem(target_func.args, i);
            if (!try self.monotypesStructurallyEqualAcrossModules(
                result,
                expected_mono,
                target_def.module_idx,
                actual_mono.idx,
                actual_mono.module_idx,
            )) {
                return false;
            }
        }

        const actual_ret = try self.resolveExprMonotypeResolved(result, thread, source_module_idx, expr_idx);
        if (actual_ret.isNone()) return true;

        return self.monotypesStructurallyEqualAcrossModules(
            result,
            target_func.ret,
            target_def.module_idx,
            actual_ret.idx,
            actual_ret.module_idx,
        );
    }

    fn materializeLookupExprCallableValue(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const desired_fn_monotype = try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx);
        const existing_callable_inst_id = if (module_env.store.getExpr(expr_idx) == .e_lookup_local) blk: {
            const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
            if (isTopLevelDefPattern(module_env, lookup.pattern_idx)) break :blk null;
            if (self.readCallableParamValue(thread.requireSourceContext(), module_idx, lookup.pattern_idx)) |callable_value| {
                switch (callable_value) {
                    .direct => |callable_inst_id| break :blk callable_inst_id,
                    .packed_fn => {
                        try self.setExprCallableValue(
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
            try self.scanCallableInst(result, callable_inst_id);
            break :blk callable_inst_id;
        } else blk: {
            if (desired_fn_monotype.isNone()) return;
            const template = result.getCallableTemplate(template_id).*;
            const defining_source_context = self.resolveTemplateDefiningSourceContext(result, thread.requireSourceContext(), template);
            if (!try self.procSignatureAcceptsFnMonotype(
                result,
                template_id,
                template,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                defining_source_context,
            )) {
                return;
            }
            break :blk try self.requireCallableInst(result, thread.requireSourceContext(), template_id, desired_fn_monotype.idx, desired_fn_monotype.module_idx);
        };
        try self.setExprDirectCallable(
            result,
            thread.requireSourceContext(),
            module_idx,
            expr_idx,
            callable_inst_id,
        );
        if (module_env.store.getExpr(expr_idx) == .e_lookup_local) {
            const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
            if (!isTopLevelDefPattern(module_env, lookup.pattern_idx) and
                thread.hasCallableInst())
            {
                try self.setCallableParamDirectValue(
                    result,
                    thread.requireSourceContext(),
                    module_idx,
                    lookup.pattern_idx,
                    callable_inst_id,
                );
            }
        }
        const template = result.getCallableTemplate(template_id).*;
        try self.setExprDirectCallable(
            result,
            thread.requireSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );
        try self.propagateLookupSourceExprCallableValue(result, thread, module_idx, expr_idx, callable_inst_id);
    }

    fn propagateLookupSourceExprCallableValue(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        lookup_expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const source_expr = result.getExprValueOrigin(thread.requireSourceContext(), module_idx, lookup_expr_idx) orelse return;
        if (!source_expr.projections.isEmpty()) return;
        try self.setExprDirectCallable(
            result,
            thread.requireSourceContext(),
            source_expr.module_idx,
            source_expr.expr_idx,
            callable_inst_id,
        );
    }

    fn internCallableVariantGroup(
        self: *Pass,
        result: *Result,
        variants: []const CallableInstId,
    ) Allocator.Error!Lambdamono.CallableVariantGroupId {
        for (result.lambdamono.callable_variant_groups.items, 0..) |_, idx| {
            const existing_variants = result.lambdamono.getCallableVariantGroupVariants(@enumFromInt(idx));
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
            Lambdamono.CallableVariantSpan.empty()
        else blk: {
            const start: u32 = @intCast(result.lambdamono.callable_variant_entries.items.len);
            try result.lambdamono.callable_variant_entries.appendSlice(self.allocator, variants);
            break :blk .{
                .start = start,
                .len = @intCast(variants.len),
            };
        };

        const set_id: Lambdamono.CallableVariantGroupId = @enumFromInt(result.lambdamono.callable_variant_groups.items.len);
        const new_set: @TypeOf(result.lambdamono.callable_variant_groups.items[0]) = .{ .variants = span };
        try self.appendTracked(.callable_variant_groups, &result.lambdamono.callable_variant_groups, new_set);
        return set_id;
    }

    fn ensureRecordedCallableInstsScanned(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        for (callable_inst_ids) |callable_inst_id| {
            try self.scanCallableInst(result, callable_inst_id);
        }
    }

    fn setExprDirectCallSite(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        try self.ensureRecordedCallableInstsScanned(result, &.{callable_inst_id});
        return self.writeExprCallSite(
            result,
            source_context,
            module_idx,
            expr_idx,
            .{ .direct = callable_inst_id },
        );
    }

    fn setCallableParamDirectValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        try self.ensureRecordedCallableInstsScanned(result, &.{callable_inst_id});
        return self.writeCallableParamValue(
            result,
            source_context,
            module_idx,
            pattern_idx,
            .{ .direct = callable_inst_id },
        );
    }

    fn setCallableParamDirectValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.setCallableParamDirectValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
            callable_inst_id,
        );
    }

    fn setCallableParamValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        return self.setCallableParamValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
            callable_value,
        );
    }

    fn setCallableParamValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        try self.ensureRecordedCallableInstsScanned(result, callableAlternativesFromValue(result, callable_value));
        try self.writeCallableParamValue(
            result,
            source_context,
            module_idx,
            pattern_idx,
            callable_value,
        );
    }

    fn ensurePackedFnForVariants(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!PackedFn {
        const variant_group_id = try self.internCallableVariantGroup(
            result,
            callable_inst_ids,
        );
        const fn_monotype = try self.requireUniformPackedCallableFnMonotype(result, callable_inst_ids);
        const runtime_monotype = try self.buildPackedFnRuntimeMonotype(result, callable_inst_ids);
        return .{
            .variant_group = variant_group_id,
            .fn_monotype = fn_monotype,
            .runtime_monotype = runtime_monotype,
        };
    }

    fn packedCallableTagName(
        self: *Pass,
        module_idx: u32,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!Monotype.Name {
        var name_buf: [32]u8 = undefined;
        const name_text = try std.fmt.bufPrint(&name_buf, "__roc_fn_{d:0>10}", .{@intFromEnum(callable_inst_id)});
        const module_env = self.all_module_envs[module_idx];
        const ident = module_env.common.findIdent(name_text) orelse
            try module_env.common.insertIdent(self.allocator, Ident.for_text(name_text));
        return .{
            .module_idx = module_idx,
            .ident = ident,
        };
    }

    fn buildPackedFnRuntimeMonotype(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        var tags = std.ArrayList(Monotype.Tag).empty;
        defer tags.deinit(self.allocator);

        const monotype_module_idx = self.current_module_idx;
        for (callable_inst_ids) |callable_inst_id| {
            const payload_mono = result.getCallableInstRuntimeMonotype(callable_inst_id);
            const payloads = if (payload_mono.isNone() or payload_mono.idx == result.context_mono.monotype_store.unit_idx)
                Monotype.Span.empty()
            else
                try result.context_mono.monotype_store.addIdxSpan(self.allocator, &.{payload_mono.idx});
            try tags.append(self.allocator, .{
                .name = try self.packedCallableTagName(monotype_module_idx, callable_inst_id),
                .payloads = payloads,
            });
        }

        std.mem.sortUnstable(Monotype.Tag, tags.items, self.all_module_envs, Monotype.Tag.sortByNameAsc);
        const tag_span = try result.context_mono.monotype_store.addTags(self.allocator, tags.items);
        return .{
            .idx = try result.context_mono.monotype_store.addMonotype(self.allocator, .{
                .tag_union = .{ .tags = tag_span },
            }),
            .module_idx = monotype_module_idx,
        };
    }

    fn requireUniformPackedCallableFnMonotype(
        self: *Pass,
        result: *const Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        const first_callable = result.getCallableInst(callable_inst_ids[0]);
        const first_resolved: ResolvedMonotype = .{
            .idx = first_callable.fn_monotype,
            .module_idx = first_callable.fn_monotype_module_idx,
        };
        for (callable_inst_ids[1..]) |callable_inst_id| {
            const callable_inst = result.getCallableInst(callable_inst_id);
            if (!try self.monotypesStructurallyEqualAcrossModules(
                result,
                first_resolved.idx,
                first_resolved.module_idx,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
            )) {
                std.debug.panic(
                    "Pipeline invariant violated: callable variant set contained mismatched fn monotypes between callable insts {d} and {d}",
                    .{ @intFromEnum(callable_inst_ids[0]), @intFromEnum(callable_inst_id) },
                );
            }
        }
        return first_resolved;
    }

    fn ensureIndirectCallForVariants(
        self: *Pass,
        result: *Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!IndirectCall {
        return .{
            .packed_fn = try self.ensurePackedFnForVariants(result, callable_inst_ids),
        };
    }

    fn lookupResolvedDispatchTemplate(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        resolved_target: ResolvedDispatchTarget,
    ) Allocator.Error!?CallableTemplateId {
        const source_env = self.all_module_envs[source_module_idx];
        const target_module_idx = resolved_target.module_idx orelse self.findModuleForOrigin(source_env, resolved_target.origin);
        const target_env = self.all_module_envs[target_module_idx];
        const method_name = self.dispatchTargetMethodText(source_env, resolved_target) orelse return null;
        const target_ident = target_env.common.findIdent(method_name) orelse return null;
        const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse return null;
        if (!target_env.store.isDefNode(target_node_idx)) return null;

        if (result.getExternalCallableTemplate(target_module_idx, target_node_idx)) |template_id| {
            return template_id;
        }

        const def_idx: CIR.Def.Idx = @enumFromInt(target_node_idx);
        const def = target_env.store.getDef(def_idx);
        return try self.registerCallableBackedDefTemplate(
            result,
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            packExternalDefSourceKey(target_module_idx, target_node_idx),
        );
    }

    fn moduleIndexForEnv(self: *Pass, env: *const ModuleEnv) ?u32 {
        for (self.all_module_envs, 0..) |module_env_entry, idx| {
            if (module_env_entry == env) return @intCast(idx);
        }
        return null;
    }

    fn findModuleForOrigin(self: *Pass, source_env: *const ModuleEnv, origin_module: Ident.Idx) u32 {
        const source_module_idx = self.moduleIndexForEnv(source_env) orelse unreachable;
        const origin_name = source_env.getIdent(origin_module);

        for (self.all_module_envs, 0..) |module_env_entry, idx| {
            const origin_module_name = module_env_entry.getIdent(module_env_entry.qualified_module_ident);
            if (std.mem.eql(u8, origin_module_name, origin_name)) {
                return @intCast(idx);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline: could not resolve origin module '{s}' from source module {d}",
                .{ origin_name, source_module_idx },
            );
        }
        unreachable;
    }

    fn findModuleForOriginMaybe(self: *Pass, source_env: *const ModuleEnv, origin_module: Ident.Idx) ?u32 {
        const source_module_idx = self.moduleIndexForEnv(source_env) orelse return null;
        if (origin_module.eql(source_env.qualified_module_ident)) return source_module_idx;

        const origin_name = self.identTextAcrossModules(source_env, origin_module) orelse return null;
        for (self.all_module_envs, 0..) |module_env_entry, idx| {
            const origin_module_name = module_env_entry.getIdent(module_env_entry.qualified_module_ident);
            if (std.mem.eql(u8, origin_name, origin_module_name)) return @intCast(idx);
        }
        return null;
    }

    fn identTextAcrossModules(self: *Pass, starting_env: *const ModuleEnv, ident: Ident.Idx) ?[]const u8 {
        if (moduleOwnsIdent(starting_env, ident)) return getOwnedIdentText(starting_env, ident);

        for (self.all_module_envs) |module_env_entry| {
            if (module_env_entry == starting_env) continue;
            if (moduleOwnsIdent(module_env_entry, ident)) return getOwnedIdentText(module_env_entry, ident);
        }

        return null;
    }

    fn moduleOwnsIdent(env: *const ModuleEnv, ident: Ident.Idx) bool {
        const ident_store = env.getIdentStoreConst();
        const bytes = ident_store.interner.bytes.items.items;
        const start: usize = @intCast(ident.idx);
        if (start >= bytes.len) return false;

        const tail = bytes[start..];
        const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return false;
        const text = tail[0..end_rel];

        const roundtrip = ident_store.findByString(text) orelse return false;
        return roundtrip.eql(ident);
    }

    fn getOwnedIdentText(env: *const ModuleEnv, ident: Ident.Idx) []const u8 {
        if (builtin.mode == .Debug) std.debug.assert(moduleOwnsIdent(env, ident));
        return env.getIdent(ident);
    }

    fn dispatchTargetMethodText(
        self: *Pass,
        source_env: *const ModuleEnv,
        target: ResolvedDispatchTarget,
    ) ?[]const u8 {
        if (moduleOwnsIdent(source_env, target.method_ident)) return getOwnedIdentText(source_env, target.method_ident);

        if (target.module_idx) |target_module_idx| {
            const target_env = self.all_module_envs[target_module_idx];
            if (moduleOwnsIdent(target_env, target.method_ident)) return getOwnedIdentText(target_env, target.method_ident);
        }

        return null;
    }

    fn resolveDispatchTargetToExternalDef(
        self: *Pass,
        source_module_idx: u32,
        target: ResolvedDispatchTarget,
    ) Allocator.Error!struct { module_idx: u32, def_node_idx: u16 } {
        const source_env = self.all_module_envs[source_module_idx];
        const target_module_idx = target.module_idx orelse self.findModuleForOrigin(source_env, target.origin);
        const target_env = self.all_module_envs[target_module_idx];
        const method_name = self.dispatchTargetMethodText(source_env, target) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: method ident {d} not readable from source module {d} or target module {d}",
                    .{ target.method_ident.idx, source_module_idx, target_module_idx },
                );
            }
            unreachable;
        };

        const target_ident = target_env.common.findIdent(method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: method '{s}' not found in target module {d}",
                    .{ method_name, target_module_idx },
                );
            }
            unreachable;
        };
        const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: exposed node not found for method '{s}' in module {d}",
                    .{ method_name, target_module_idx },
                );
            }
            unreachable;
        };
        if (!target_env.store.isDefNode(target_node_idx)) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: exposed node for method '{s}' is not a def node (module={d}, node={d})",
                    .{ method_name, target_module_idx, target_node_idx },
                );
            }
            unreachable;
        }
        return .{
            .module_idx = target_module_idx,
            .def_node_idx = target_node_idx,
        };
    }

    fn builtinPrimForNominal(ident: Ident.Idx, common: ModuleEnv.CommonIdents) ?Monotype.Prim {
        if (ident.eql(common.str)) return .str;
        if (ident.eql(common.u8_type)) return .u8;
        if (ident.eql(common.i8_type)) return .i8;
        if (ident.eql(common.u16_type)) return .u16;
        if (ident.eql(common.i16_type)) return .i16;
        if (ident.eql(common.u32_type)) return .u32;
        if (ident.eql(common.i32_type)) return .i32;
        if (ident.eql(common.u64_type)) return .u64;
        if (ident.eql(common.i64_type)) return .i64;
        if (ident.eql(common.u128_type)) return .u128;
        if (ident.eql(common.i128_type)) return .i128;
        if (ident.eql(common.f32_type)) return .f32;
        if (ident.eql(common.f64_type)) return .f64;
        if (ident.eql(common.dec_type)) return .dec;
        return null;
    }

    fn lookupAssociatedMethodTemplate(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        nominal: types.NominalType,
        method_ident: Ident.Idx,
    ) Allocator.Error!?AssociatedMethodTemplate {
        return self.lookupAssociatedMethodTemplateByOriginIdent(
            result,
            source_module_idx,
            nominal.origin_module,
            nominal.ident.ident_idx,
            method_ident,
        );
    }

    fn lookupAssociatedMethodTemplateForMonotype(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        monotype: Monotype.Idx,
        method_ident: Ident.Idx,
    ) Allocator.Error!?AssociatedMethodTemplate {
        const source_env = self.all_module_envs[source_module_idx];
        const common = ModuleEnv.CommonIdents.find(&source_env.common);
        const mono = result.context_mono.monotype_store.getMonotype(monotype);

        const type_ident: Ident.Idx = switch (mono) {
            .prim => |prim| switch (prim) {
                .str => common.str,
                .u8 => common.u8_type,
                .i8 => common.i8_type,
                .u16 => common.u16_type,
                .i16 => common.i16_type,
                .u32 => common.u32_type,
                .i32 => common.i32_type,
                .u64 => common.u64_type,
                .i64 => common.i64_type,
                .u128 => common.u128_type,
                .i128 => common.i128_type,
                .f32 => common.f32_type,
                .f64 => common.f64_type,
                .dec => common.dec_type,
            },
            .list => common.list,
            .box => common.box,
            else => return null,
        };

        return self.lookupAssociatedMethodTemplateByOriginIdent(
            result,
            source_module_idx,
            common.builtin_module,
            type_ident,
            method_ident,
        );
    }

    fn lookupAssociatedMethodTemplateByOriginIdent(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        origin_module: Ident.Idx,
        type_ident: Ident.Idx,
        method_ident: Ident.Idx,
    ) Allocator.Error!?AssociatedMethodTemplate {
        const source_env = self.all_module_envs[source_module_idx];
        const target_module_idx = self.findModuleForOrigin(source_env, origin_module);
        const target_env = self.all_module_envs[target_module_idx];
        const qualified_method_ident = target_env.lookupMethodIdentFromEnvConst(
            source_env,
            type_ident,
            method_ident,
        ) orelse return null;
        const node_idx = target_env.getExposedNodeIndexById(qualified_method_ident) orelse return null;
        if (!target_env.store.isDefNode(node_idx)) return null;

        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
        const def = target_env.store.getDef(def_idx);
        const template_id = (try self.registerCallableBackedDefTemplate(
            result,
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            packExternalDefSourceKey(target_module_idx, node_idx),
        )) orelse return null;
        return .{
            .target_env = target_env,
            .module_idx = target_module_idx,
            .template_id = template_id,
            .type_var = ModuleEnv.varFrom(def.expr),
            .qualified_method_ident = qualified_method_ident,
        };
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
        const builtin_module_idx = self.findModuleForOrigin(source_env, common.builtin_module);
        const builtin_env = self.all_module_envs[builtin_module_idx];
        const method_name = source_env.getIdent(common.builtin_box_unbox);
        const target_ident = builtin_env.common.findIdent(method_name) orelse return;
        const node_idx = builtin_env.getExposedNodeIndexById(target_ident) orelse return;
        if (!builtin_env.store.isDefNode(node_idx)) return;

        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
        const def = builtin_env.store.getDef(def_idx);
        const template_id = (try self.registerCallableBackedDefTemplate(
            result,
            builtin_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            packExternalDefSourceKey(builtin_module_idx, node_idx),
        )) orelse return;

        const args = try result.context_mono.monotype_store.addIdxSpan(self.allocator, &.{box_monotype});
        const fn_monotype = try result.context_mono.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = args,
            .ret = inner_monotype,
            .effectful = false,
        } });

        _ = try self.requireCallableInst(result, templateSourceContext(result.getCallableTemplate(template_id).*) , template_id, fn_monotype, source_module_idx);
    }

    fn resolveTypeVarMonotypeWithBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!Monotype.Idx {
        var exact_specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer exact_specializations.deinit();

        var bindings_it = bindings.iterator();
        while (bindings_it.next()) |entry| {
            if (entry.key_ptr.module_idx != module_idx) continue;

            if (exact_specializations.get(entry.key_ptr.type_var)) |existing| {
                if (entry.value_ptr.module_idx != module_idx or
                    !try self.monotypesStructurallyEqual(result, existing, entry.value_ptr.idx))
                {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Pipeline: conflicting exact binding for type var root {d} in module {d}",
                            .{ @intFromEnum(entry.key_ptr.type_var), module_idx },
                        );
                    }
                    unreachable;
                }
                continue;
            }

            try exact_specializations.put(entry.key_ptr.type_var, entry.value_ptr.idx);
        }

        try self.seedRecordedTypeScopeSpecializations(result, module_idx, &exact_specializations);

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.module_idx = module_idx;
        scratches.all_module_envs = self.all_module_envs;
        return result.context_mono.monotype_store.fromTypeVarExact(
            self.allocator,
            store_types,
            var_,
            module_env.idents,
            &exact_specializations,
            &nominal_cycle_breakers,
            &scratches,
        );
    }

    fn remapMonotypeBetweenModules(
        self: *Pass,
        result: *Result,
        monotype: Monotype.Idx,
        from_module_idx: u32,
        to_module_idx: u32,
    ) Allocator.Error!Monotype.Idx {
        if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;

        var remapped = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(self.allocator);
        defer remapped.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();
        scratches.ident_store = self.all_module_envs[to_module_idx].getIdentStoreConst();
        scratches.module_env = self.all_module_envs[to_module_idx];
        scratches.module_idx = to_module_idx;
        scratches.all_module_envs = self.all_module_envs;

        return self.remapMonotypeBetweenModulesRec(
            result,
            monotype,
            from_module_idx,
            to_module_idx,
            &remapped,
            &scratches,
        );
    }

    fn remapMonotypeBetweenModulesRec(
        self: *Pass,
        result: *Result,
        monotype: Monotype.Idx,
        from_module_idx: u32,
        to_module_idx: u32,
        remapped: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
        scratches: *Monotype.Store.Scratches,
    ) Allocator.Error!Monotype.Idx {
        if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;
        if (remapped.get(monotype)) |existing| return existing;

        const mono = result.context_mono.monotype_store.getMonotype(monotype);
        switch (mono) {
            .unit => return result.context_mono.monotype_store.unit_idx,
            .prim => |prim| return result.context_mono.monotype_store.primIdx(prim),
            .recursive_placeholder => {
                if (builtin.mode == .Debug) {
                    std.debug.panic("remapMonotypeBetweenModules: unexpected recursive_placeholder", .{});
                }
                unreachable;
            },
            .list, .box, .tuple, .func, .record, .tag_union => {},
        }

        const placeholder = try result.context_mono.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
        try remapped.put(monotype, placeholder);

        const mapped_mono: Monotype.Monotype = switch (mono) {
            .list => |list_mono| .{ .list = .{
                .elem = try self.remapMonotypeBetweenModulesRec(
                    result,
                    list_mono.elem,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                    scratches,
                ),
            } },
            .box => |box_mono| .{ .box = .{
                .inner = try self.remapMonotypeBetweenModulesRec(
                    result,
                    box_mono.inner,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                    scratches,
                ),
            } },
            .tuple => |tuple_mono| blk: {
                const idx_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(idx_top);

                var elem_i: usize = 0;
                while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                    const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                    try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        result,
                        elem_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ));
                }

                const mapped_elems = try result.context_mono.monotype_store.addIdxSpan(
                    self.allocator,
                    scratches.idxs.sliceFromStart(idx_top),
                );
                break :blk .{ .tuple = .{ .elems = mapped_elems } };
            },
            .func => |func_mono| blk: {
                const idx_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(idx_top);

                var arg_i: usize = 0;
                while (arg_i < func_mono.args.len) : (arg_i += 1) {
                    const arg_mono = result.context_mono.monotype_store.getIdxSpanItem(func_mono.args, arg_i);
                    try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        result,
                        arg_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ));
                }
                const mapped_args = try result.context_mono.monotype_store.addIdxSpan(
                    self.allocator,
                    scratches.idxs.sliceFromStart(idx_top),
                );

                const mapped_ret = try self.remapMonotypeBetweenModulesRec(
                    result,
                    func_mono.ret,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                    scratches,
                );

                break :blk .{ .func = .{
                    .args = mapped_args,
                    .ret = mapped_ret,
                    .effectful = func_mono.effectful,
                } };
            },
            .record => |record_mono| blk: {
                const fields_top = scratches.fields.top();
                defer scratches.fields.clearFrom(fields_top);

                var field_i: usize = 0;
                while (field_i < record_mono.fields.len) : (field_i += 1) {
                    const field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, field_i);
                    try scratches.fields.append(.{
                        .name = field.name,
                        .type_idx = try self.remapMonotypeBetweenModulesRec(
                            result,
                            field.type_idx,
                            from_module_idx,
                            to_module_idx,
                            remapped,
                            scratches,
                        ),
                    });
                }

                const mapped_fields = try result.context_mono.monotype_store.addFields(
                    self.allocator,
                    scratches.fields.sliceFromStart(fields_top),
                );
                break :blk .{ .record = .{ .fields = mapped_fields } };
            },
            .tag_union => |tag_union_mono| blk: {
                const tags_top = scratches.tags.top();
                defer scratches.tags.clearFrom(tags_top);

                var tag_i: usize = 0;
                while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                    const tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                    const payload_top = scratches.idxs.top();
                    defer scratches.idxs.clearFrom(payload_top);

                    var payload_i: usize = 0;
                    while (payload_i < tag.payloads.len) : (payload_i += 1) {
                        const payload_mono = result.context_mono.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                        try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                            result,
                            payload_mono,
                            from_module_idx,
                            to_module_idx,
                            remapped,
                            scratches,
                        ));
                    }

                    const mapped_payloads = try result.context_mono.monotype_store.addIdxSpan(
                        self.allocator,
                        scratches.idxs.sliceFromStart(payload_top),
                    );
                    try scratches.tags.append(.{
                        .name = tag.name,
                        .payloads = mapped_payloads,
                    });
                }

                const mapped_tags = try result.context_mono.monotype_store.addTags(
                    self.allocator,
                    scratches.tags.sliceFromStart(tags_top),
                );
                break :blk .{ .tag_union = .{ .tags = mapped_tags } };
            },
            .unit, .prim, .recursive_placeholder => unreachable,
        };

        result.context_mono.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
        return placeholder;
    }

    fn resolveFuncTypeInStore(types_store: *const types.Store, type_var: types.Var) ?struct { func: types.Func, effectful: bool } {
        var resolved = types_store.resolveVar(type_var);
        while (resolved.desc.content == .alias) {
            resolved = types_store.resolveVar(types_store.getAliasBackingVar(resolved.desc.content.alias));
        }

        if (resolved.desc.content != .structure) return null;
        return switch (resolved.desc.content.structure) {
            .fn_pure => |func| .{ .func = func, .effectful = false },
            .fn_effectful => |func| .{ .func = func, .effectful = true },
            .fn_unbound => |func| .{ .func = func, .effectful = false },
            else => null,
        };
    }

    fn resolveNominalTypeInStore(types_store: *const types.Store, type_var: types.Var) ?types.NominalType {
        var resolved = types_store.resolveVar(type_var);
        while (resolved.desc.content == .alias) {
            resolved = types_store.resolveVar(types_store.getAliasBackingVar(resolved.desc.content.alias));
        }

        if (resolved.desc.content != .structure) return null;
        return switch (resolved.desc.content.structure) {
            .nominal_type => |nominal| nominal,
            else => null,
        };
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
            if (!(try self.monotypesStructurallyEqual(result, existing, monotype))) {
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

    fn seedRecordedTypeScopeSpecializations(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ) Allocator.Error!void {
        var it = result.context_mono.type_scope_monotypes.iterator();
        while (it.next()) |entry| {
            if (entry.key_ptr.module_idx != module_idx) continue;
            if (entry.value_ptr.module_idx != module_idx) {
                std.debug.panic(
                    "Pipeline invariant violated: type-scope monotype module mismatch key_module={d} value_module={d} type_var={d}",
                    .{
                        module_idx,
                        entry.value_ptr.module_idx,
                        @intFromEnum(entry.key_ptr.type_var),
                    },
                );
            }
            try bindings.put(entry.key_ptr.type_var, entry.value_ptr.idx);
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

    fn monotypeFromTypeVarInStore(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
    ) Allocator.Error!Monotype.Idx {
        var specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer specializations.deinit();

        try self.seedRecordedTypeScopeSpecializations(result, module_idx, &specializations);

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.module_idx = module_idx;
        scratches.all_module_envs = self.all_module_envs;
        return result.context_mono.monotype_store.fromTypeVarExact(
            self.allocator,
            store_types,
            var_,
            module_env.idents,
            &specializations,
            &nominal_cycle_breakers,
            &scratches,
        );
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

                if (origin.eql(common_idents.builtin_module) and builtinPrimForNominal(ident, common_idents) != null) {
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
                                "Pipeline invariant violated: non-empty store record matched unit monotype (module={d}, active_callable_inst={d}, monotype={d})",
                                .{
                                    module_idx,
                                    self.currentCallableInstRawForDebug(),
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
                    },
                    else => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "Pipeline invariant violated: expected record monotype for store record, got {s} (module={d}, active_callable_inst={d}, monotype={d})",
                                .{
                                    @tagName(mono),
                                    module_idx,
                                    self.currentCallableInstRawForDebug(),
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
                        if (builtinPrimForNominal(ident, common) != null) return;
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
                            const outer_mono = try self.resolveTypeVarMonotype(result, thread, module_idx, resolved.var_);
                            const outer_box = result.context_mono.monotype_store.getMonotype(outer_mono).box;
                            try self.ensureBuiltinBoxUnboxCallableInst(result, module_idx, outer_mono, outer_box.inner);
                            if (type_args.len == 1) {
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, thread, module_idx, type_args[0], visiting);
                            }
                            return;
                        }
                    }

                    if (try self.lookupAssociatedMethodTemplate(result, module_idx, nominal, module_env.idents.to_inspect)) |method_info| {
                        if (resolveFuncTypeInStore(&method_info.target_env.types, method_info.type_var)) |resolved_func| {
                            if (!resolved_func.effectful) {
                                const param_vars = method_info.target_env.types.sliceVars(resolved_func.func.args);
                                if (param_vars.len == 1) {
                                    var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
                                    defer bindings.deinit();
                                    var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
                                    defer ordered_entries.deinit(self.allocator);

                                    const arg_mono = try self.resolveTypeVarMonotype(result, thread, module_idx, resolved.var_);
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

                                    const method_func_mono = try self.resolveTypeVarMonotypeWithBindings(
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
                        try self.resolveTypeVarMonotype(result, thread, module_idx, resolved.var_),
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
            try self.resolveTypeVarMonotype(result, thread, module_idx, resolved.var_),
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

    fn callUsesAnnotationOnlyIntrinsic(
        self: *Pass,
        module_idx: u32,
        callee_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!bool {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(callee_expr_idx)) {
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk false;
                const target_env = self.all_module_envs[target_module_idx];
                if (!target_env.store.isDefNode(lookup.target_node_idx)) break :blk false;
                const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                break :blk target_env.store.getExpr(def.expr) == .e_anno_only;
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk false;
                const target_env = self.all_module_envs[target.module_idx];
                const def = target_env.store.getDef(target.def_idx);
                break :blk target_env.store.getExpr(def.expr) == .e_anno_only;
            },
            else => false,
        };
    }

    fn requireCallableInst(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!CallableInstId {
        const callable_inst_id = try self.ensureCallableInstRecord(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            &.{},
        );
        try self.scanCallableInst(result, callable_inst_id);
        return callable_inst_id;
    }

    fn registerCallableInst(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!CallableInstId {
        return self.ensureCallableInstRecord(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            &.{},
        );
    }

    fn registerCallableInstWithCallableParamSpecs(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!CallableInstId {
        return self.ensureCallableInstRecord(
            result,
            defining_source_context,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            callable_param_specs,
        );
    }

    fn ensureCallableInstRecord(
        self: *Pass,
        result: *Result,
        defining_source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!CallableInstId {
        const template = result.getCallableTemplate(template_id);
        const canonical_fn_monotype = if (fn_monotype_module_idx == template.module_idx)
            fn_monotype
        else
            try self.remapMonotypeBetweenModules(
                result,
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
            const mono_equal = try self.monotypesStructurallyEqual(
                result,
                existing_callable_inst.fn_monotype,
                canonical_fn_monotype,
            );
            if (mono_equal) {
                if (existing_callable_inst.subst != subst_id) continue;
                const existing_id: CallableInstId = @enumFromInt(idx);
                try self.ensureDirectCallableVariantGroup(result, existing_id);
                return existing_id;
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
            .fn_monotype = resolvedMonotype(canonical_fn_monotype, canonical_fn_monotype_module_idx),
            .captures = .empty(),
            .source_region = template.source_region,
        });
        try self.appendTracked(.callable_insts, &result.lambdamono.callable_insts, CallableInst{
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
        return callable_inst_id;
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
        if (result.lambdamono.direct_callable_variant_group_ids_by_callable_inst.contains(callable_inst_id)) return;
        const variant_group_id = try self.internCallableVariantGroup(result, &.{callable_inst_id});
        try result.lambdamono.direct_callable_variant_group_ids_by_callable_inst.put(self.allocator, callable_inst_id, variant_group_id);
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
        self: *Pass,
        result: *const Result,
        active_source_context: SourceContext,
        template: CallableTemplate,
    ) SourceContext {
        switch (template.kind) {
            .top_level_def, .lambda, .hosted_lambda => return templateSourceContext(template),
            .closure => {
                const lexical_owner_template = switch (template.owner) {
                    .root_scope => return active_source_context,
                    .lexical_template => |owner| owner,
                };
                if (result.getSourceContextTemplateId(active_source_context)) |active_template| {
                    if (active_template == lexical_owner_template) {
                        switch (active_source_context) {
                            .root_expr, .provenance_expr, .template_expr => return active_source_context,
                            .callable_inst => {},
                        }
                    }
                }
                var current = switch (active_source_context) {
                    .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
                    .root_expr, .provenance_expr, .template_expr => {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "Pipeline: closure template expr={d} requires lexical owner template={d} but active source context was {s}",
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
                        "Pipeline: closure template expr={d} could not resolve lexical owner template={d} from active callable inst={d}",
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

    fn scanCallableInst(self: *Pass, result: *Result, callable_inst_id: CallableInstId) Allocator.Error!void {
        try self.enqueueCallableScan(callable_inst_id, false);
        try self.drainCallableScans(result);
    }

    fn scanCallableInstBody(self: *Pass, result: *Result, callable_inst_id: CallableInstId) Allocator.Error!void {
        const callable_inst_key = @intFromEnum(callable_inst_id);
        if (self.completed_callable_scans.contains(callable_inst_key)) return;
        if (self.in_progress_callable_scans.contains(callable_inst_key)) return;
        // Snapshot these by value before scanning. `scanModule` can discover more
        // demanded callables and append to both arrays, which would invalidate pointers.
        const callable_inst = result.getCallableInst(callable_inst_id).*;
        const template = result.getCallableTemplate(callable_inst.template).*;
        const defining_source_context = callable_inst.defining_source_context;
        try self.primeModuleDefs(result, template.module_idx);
        try self.in_progress_callable_scans.put(self.allocator, callable_inst_key, {});
        defer _ = self.in_progress_callable_scans.remove(callable_inst_key);
        try self.pushActiveScanUnit(.{ .callable_inst = callable_inst_id });
        defer self.popActiveScanUnit();

        const module_env = self.all_module_envs[template.module_idx];

        try self.seedCallableInstContextTypeVarMonotypes(result, callable_inst_id);
        try self.seedCallableBoundaryContextMonotypes(
            result,
            callableInstSourceContext(callable_inst_id),
            template.module_idx,
            template.cir_expr,
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
        );
        const thread = SemanticThread{
            .source_context = callableInstSourceContext(callable_inst_id),
        };
        switch (template.binding) {
            .pattern => |binding_pattern| {
                try self.mergeTrackedContextPatternMonotype(
                    result,
                    self.resultPatternKey(callable_inst_id, template.module_idx, binding_pattern),
                    resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
                );
                try self.setCallableParamDirectValueInCallableContext(
                    result,
                    callable_inst_id,
                    template.module_idx,
                    binding_pattern,
                    callable_inst_id,
                );
            },
            .anonymous => {},
        }

        try self.recordCallableParamPatternFactsForCallableInst(result, callable_inst_id);

        try self.scanForcedCirValueExpr(result, thread, template.module_idx, template.body_expr);

        if (template.kind == .closure) {
            const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
                .e_closure => |closure| closure,
                else => unreachable,
            };
            try self.scanClosureCaptureSources(
                result,
                defining_source_context,
                template.module_idx,
                template.runtime_expr,
                closure_expr,
            );
        }

        switch (template.kind) {
            .closure => {
                const closure_expr = switch (module_env.store.getExpr(template.runtime_expr)) {
                    .e_closure => |closure| closure,
                    else => unreachable,
                };
                try self.finalizeCallableDefForCallableInst(
                    result,
                    defining_source_context,
                    template.module_idx,
                    template.runtime_expr,
                    closure_expr,
                    callable_inst_id,
                );
            },
            .lambda, .hosted_lambda => {
                try self.updateCallableDefRuntimeValue(
                    result,
                    callable_inst_id,
                    .{
                        .source_context = callableInstSourceContext(callable_inst_id),
                        .module_idx = template.module_idx,
                        .expr_idx = template.runtime_expr,
                    },
                    .{
                        .source_context = callableInstSourceContext(callable_inst_id),
                        .module_idx = template.module_idx,
                        .expr_idx = template.body_expr,
                    },
                    &.{},
                );
            },
            .top_level_def => unreachable,
        }

        try self.completed_callable_scans.put(self.allocator, callable_inst_key, {});
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
        try self.appendTracked(.substs, &result.context_mono.substs, TypeSubst{ .entries = entries_span });
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
            if (!try self.monotypesStructurallyEqual(result, lhs_entry.monotype.idx, rhs_entry.monotype.idx)) {
                return false;
            }
        }

        return true;
    }

    fn labelTextAcrossModules(self: *Pass, module_idx: u32, label: anytype) []const u8 {
        return switch (@TypeOf(label)) {
            base.Ident.Idx => self.all_module_envs[module_idx].getIdent(label),
            Monotype.Name => label.text(self.all_module_envs),
            else => @compileError("unsupported label type"),
        };
    }

    fn identsStructurallyEqualAcrossModules(
        self: *Pass,
        lhs_module_idx: u32,
        lhs: anytype,
        rhs_module_idx: u32,
        rhs: anytype,
    ) bool {
        if (@TypeOf(lhs) == base.Ident.Idx and @TypeOf(rhs) == base.Ident.Idx and lhs_module_idx == rhs_module_idx and lhs == rhs) {
            return true;
        }
        if (@TypeOf(lhs) == Monotype.Name and @TypeOf(rhs) == Monotype.Name and lhs.eql(rhs)) {
            return true;
        }

        const lhs_text = self.labelTextAcrossModules(lhs_module_idx, lhs);
        const rhs_text = self.labelTextAcrossModules(rhs_module_idx, rhs);
        return std.mem.eql(u8, lhs_text, rhs_text);
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
            try self.remapMonotypeBetweenModules(result, monotype, mono_module_idx, template_module_idx);
        const resolved_mono = resolvedMonotype(normalized_mono, template_module_idx);

        const resolved_key = boundTypeVarKey(template_module_idx, template_types, type_var);
        if (bindings.get(resolved_key)) |existing| {
            if (existing.module_idx != resolved_mono.module_idx or
                !try self.monotypesStructurallyEqual(result, existing.idx, resolved_mono.idx))
            {
                if (allow_failure) return false;
                if (std.debug.runtime_safety) {
                    const context_template = self.activeCallableTemplate(result);
                    std.debug.panic(
                        "Pipeline: conflicting monotype binding for type var root {d} in module {d} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} ctx={d} root_source_expr={d} template_expr={d} template_kind={s}",
                        .{
                            @intFromEnum(resolved_key.type_var),
                            resolved_key.module_idx,
                            @intFromEnum(existing.idx),
                            existing.module_idx,
                            result.context_mono.monotype_store.getMonotype(existing.idx),
                            @intFromEnum(resolved_mono.idx),
                            resolved_mono.module_idx,
                            result.context_mono.monotype_store.getMonotype(resolved_mono.idx),
                            self.currentCallableInstRawForDebug(),
                            self.activeRootExprRawForDebug(),
                            if (context_template) |template| @intFromEnum(template.cir_expr) else std.math.maxInt(u32),
                            if (context_template) |template| @tagName(template.kind) else "none",
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

                if (origin.eql(common_idents.builtin_module) and builtinPrimForNominal(ident, common_idents) != null) {
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
        self: *Pass,
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
                "Pipeline bindFlatTypeMonotypes mismatch: flat_type={s} mono={s} template_module={d} mono_module={d} active_callable_inst={d} monotype={d}",
                .{
                    @tagName(flat_type),
                    @tagName(mono),
                    template_module_idx,
                    mono_module_idx,
                    self.currentCallableInstRawForDebug(),
                    @intFromEnum(monotype),
                },
            );
        }
        unreachable;
    }

    fn bindFlatTypeErrorTail(
        self: *Pass,
        comptime allow_failure: bool,
        flat_type: types.FlatType,
        template_module_idx: u32,
        mono_module_idx: u32,
        monotype: Monotype.Idx,
    ) bool {
        if (allow_failure) return false;
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline bindFlatTypeMonotypes hit err tail: flat_type={s} template_module={d} mono_module={d} active_callable_inst={d} monotype={d}",
                .{
                    @tagName(flat_type),
                    template_module_idx,
                    mono_module_idx,
                    self.currentCallableInstRawForDebug(),
                    @intFromEnum(monotype),
                },
            );
        }
        unreachable;
    }

    fn resolveExprMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Monotype.Idx {
        return (try self.resolveExprMonotypeResolved(result, thread, module_idx, expr_idx)).idx;
    }

    fn resolveExprMonotypeResolved(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        const source_context = thread.requireSourceContext();
        return self.requireRecordedExprMonotypeForSourceContext(
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
        );
    }

    fn requireRecordedExprMonotypeForSourceContext(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupExprMonotypeForSourceContext(result, thread, source_context, module_idx, expr_idx)) |resolved| {
            return resolved;
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callableValueMonotype(result, callable_value);
        }

        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (result.context_mono.getContextPatternMonotype(source_context, module_idx, lookup.pattern_idx)) |pattern_mono| {
                    return pattern_mono;
                }
                if (self.lookupContextTypeVarMonotype(
                    result,
                    source_context,
                    module_idx,
                    ModuleEnv.varFrom(lookup.pattern_idx),
                )) |binding_mono| {
                    return binding_mono;
                }
            },
            else => {},
        }

        if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
            var source_mono = try self.requireRecordedExprMonotypeForSourceContext(
                result,
                thread,
                source.source_context,
                source.module_idx,
                source.expr_idx,
            );
            for (result.lambdamono.getValueProjectionEntries(source.projections)) |projection| {
                source_mono = try self.projectResolvedMonotypeByValueProjection(result, source_mono, projection);
            }
            return source_mono;
        }

        std.debug.panic(
            "Pipeline invariant violated: missing exact contextual monotype for expr {d} in module {d} under source context {s}",
            .{
                @intFromEnum(expr_idx),
                module_idx,
                @tagName(source_context),
            },
        );
    }

    fn lookupRecordedExprMonotypeIfReady(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        const source_context = thread.requireSourceContext();
        return self.lookupRecordedExprMonotypeIfReadyForSourceContext(
            result,
            thread,
            source_context,
            module_idx,
            expr_idx,
        );
    }

    fn lookupRecordedExprMonotypeIfReadyForSourceContext(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupExprMonotypeForSourceContext(result, thread, source_context, module_idx, expr_idx)) |resolved| {
            return resolved;
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callableValueMonotype(result, callable_value);
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |lookup| {
                if (self.getContextPatternMonotypeForThread(result, thread, module_idx, lookup.pattern_idx)) |pattern_mono| {
                    return pattern_mono;
                }
                if (self.lookupContextTypeVarMonotype(
                    result,
                    source_context,
                    module_idx,
                    ModuleEnv.varFrom(lookup.pattern_idx),
                )) |binding_mono| {
                    return binding_mono;
                }
                if (result.getExprValueOrigin(source_context, module_idx, expr_idx)) |source| {
                    var source_mono = try self.lookupRecordedExprMonotypeIfReadyForSourceContext(
                        result,
                        thread,
                        source.source_context,
                        source.module_idx,
                        source.expr_idx,
                    );
                    for (result.lambdamono.getValueProjectionEntries(source.projections)) |projection| {
                        if (source_mono.isNone()) break;
                        source_mono = try self.projectResolvedMonotypeByValueProjection(result, source_mono, projection);
                    }
                    if (!source_mono.isNone()) {
                        return source_mono;
                    }
                }
            },
            else => {},
        }

        if (self.exprUsesContextSensitiveNumericDefault(module_idx, expr_idx)) {
            return resolvedMonotype(.none, module_idx);
        }

        return resolvedMonotype(.none, module_idx);
    }

    fn resolveTypeVarMonotype(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!Monotype.Idx {
        return (try self.resolveTypeVarMonotypeResolved(result, thread, module_idx, var_)).idx;
    }

    fn resolveTypeVarMonotypeResolved(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupContextTypeVarMonotype(result, thread.requireSourceContext(), module_idx, var_)) |mono| {
            return mono;
        }
        const mono = try self.monotypeFromTypeVarInSourceContext(
            result,
            thread.requireSourceContext(),
            module_idx,
            &self.all_module_envs[module_idx].types,
            var_,
        );
        return resolvedMonotype(mono, module_idx);
    }

    fn resolveTypeVarMonotypeIfFullyBound(
        self: *Pass,
        result: *Result,
        thread: SemanticThread,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!ResolvedMonotype {
        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (!try self.typeVarFullyBoundInSourceContext(
            result,
            thread.requireSourceContext(),
            module_idx,
            &self.all_module_envs[module_idx].types,
            var_,
            &seen,
        )) {
            return resolvedMonotype(.none, module_idx);
        }
        return self.resolveTypeVarMonotypeResolved(result, thread, module_idx, var_);
    }

    fn lookupContextTypeVarMonotype(
        self: *Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        var_: types.Var,
    ) ?ResolvedMonotype {
        const resolved_var = self.all_module_envs[module_idx].types.resolveVar(var_).var_;
        return result.context_mono.getContextTypeVarMonotype(source_context, module_idx, resolved_var) orelse
            result.context_mono.getTypeScopeMonotype(module_idx, resolved_var);
    }

    fn seedRecordedContextTypeVarSpecializations(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        specializations: *std.AutoHashMap(types.Var, Monotype.Idx),
    ) Allocator.Error!void {
        var it = result.context_mono.context_type_var_monotypes.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.module_idx != module_idx) continue;
            if (!contextTypeVarKeyMatchesSourceContext(source_context, key)) continue;
            if (entry.value_ptr.module_idx != module_idx) continue;
            try specializations.put(key.type_var, entry.value_ptr.idx);
        }
    }

    fn monotypeFromTypeVarInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
    ) Allocator.Error!Monotype.Idx {
        var exact_specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer exact_specializations.deinit();

        try self.seedRecordedContextTypeVarSpecializations(result, source_context, module_idx, &exact_specializations);
        try self.seedRecordedTypeScopeSpecializations(result, module_idx, &exact_specializations);

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.module_idx = module_idx;
        scratches.all_module_envs = self.all_module_envs;
        return result.context_mono.monotype_store.fromTypeVarExact(
            self.allocator,
            store_types,
            var_,
            module_env.idents,
            &exact_specializations,
            &nominal_cycle_breakers,
            &scratches,
        );
    }

    fn typeVarFullyBoundInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        if (self.lookupContextTypeVarMonotype(result, source_context, module_idx, var_) != null) return true;
        const resolved = store_types.resolveVar(var_);
        if (seen.contains(resolved.var_)) return true;
        try seen.put(self.allocator, resolved.var_, {});

        return switch (resolved.desc.content) {
            .flex, .rigid => false,
            .alias => |alias| self.typeVarFullyBoundInSourceContext(
                result,
                source_context,
                module_idx,
                store_types,
                store_types.getAliasBackingVar(alias),
                seen,
            ),
            .structure => |flat_type| self.flatTypeFullyBoundInSourceContext(
                result,
                source_context,
                module_idx,
                store_types,
                flat_type,
                seen,
            ),
            .err => true,
        };
    }

    fn flatTypeFullyBoundInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        store_types: *const types.Store,
        flat_type: types.FlatType,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        return switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                for (store_types.sliceVars(func.args)) |arg_var| {
                    if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, arg_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, func.ret, seen);
            },
            .nominal_type => |nominal| blk: {
                for (store_types.sliceNominalArgs(nominal)) |arg_var| {
                    if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, arg_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .record => |record| blk: {
                var current_row = record;
                while (true) {
                    const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
                    for (fields_slice.items(.var_)) |field_var| {
                        if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, field_var, seen)) {
                            break :blk false;
                        }
                    }

                    const ext_resolved = store_types.resolveVar(current_row.ext);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            const backing = store_types.getAliasBackingVar(alias);
                            const backing_resolved = store_types.resolveVar(backing);
                            if (backing_resolved.desc.content == .structure) {
                                switch (backing_resolved.desc.content.structure) {
                                    .record => |next_row| {
                                        current_row = next_row;
                                        continue;
                                    },
                                    .record_unbound => |fields_range| {
                                        const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                        for (ext_fields.items(.var_)) |field_var| {
                                            if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, field_var, seen)) {
                                                break :blk false;
                                            }
                                        }
                                        break :blk true;
                                    },
                                    .empty_record => break :blk true,
                                    else => break :blk try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, backing, seen),
                                }
                            }
                            break :blk try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, backing, seen);
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                for (ext_fields.items(.var_)) |field_var| {
                                    if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, field_var, seen)) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            },
                            .empty_record => break :blk true,
                            else => break :blk false,
                        },
                        .flex, .rigid => break :blk true,
                        else => break :blk try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, current_row.ext, seen),
                    }
                }
            },
            .record_unbound => |fields_range| blk: {
                const fields_slice = store_types.getRecordFieldsSlice(fields_range);
                for (fields_slice.items(.var_)) |field_var| {
                    if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, field_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, elem_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const tags = store_types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |args_range| {
                    for (store_types.sliceVars(args_range)) |payload_var| {
                        if (!try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, payload_var, seen)) {
                            break :blk false;
                        }
                    }
                }

                const ext_resolved = store_types.resolveVar(tag_union.ext);
                switch (ext_resolved.desc.content) {
                    .flex, .rigid => break :blk true,
                    else => break :blk try self.typeVarFullyBoundInSourceContext(result, source_context, module_idx, store_types, tag_union.ext, seen),
                }
            },
            .empty_record, .empty_tag_union => true,
        };
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

    fn resolvedMonotype(idx: Monotype.Idx, module_idx: u32) ResolvedMonotype {
        return .{ .idx = idx, .module_idx = module_idx };
    }

    fn monotypesStructurallyEqual(
        self: *Pass,
        result: *const Result,
        lhs: Monotype.Idx,
        rhs: Monotype.Idx,
    ) Allocator.Error!bool {
        if (lhs == rhs) return true;

        var seen = std.AutoHashMap(u64, void).init(self.allocator);
        defer seen.deinit();

        return self.monotypesStructurallyEqualRec(result, lhs, rhs, &seen);
    }

    fn monotypesStructurallyEqualAcrossModules(
        self: *Pass,
        result: *const Result,
        lhs: Monotype.Idx,
        lhs_module_idx: u32,
        rhs: Monotype.Idx,
        rhs_module_idx: u32,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(u64, void).init(self.allocator);
        defer seen.deinit();

        return self.monotypesStructurallyEqualAcrossModulesRec(
            result,
            lhs,
            lhs_module_idx,
            rhs,
            rhs_module_idx,
            &seen,
        );
    }

    fn monotypesStructurallyEqualRec(
        self: *Pass,
        result: *const Result,
        lhs: Monotype.Idx,
        rhs: Monotype.Idx,
        seen: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        if (lhs == rhs) return true;

        const lhs_u32: u32 = @intFromEnum(lhs);
        const rhs_u32: u32 = @intFromEnum(rhs);
        const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);

        if (seen.contains(key)) return true;
        try seen.put(key, {});

        const lhs_mono = result.context_mono.monotype_store.getMonotype(lhs);
        const rhs_mono = result.context_mono.monotype_store.getMonotype(rhs);
        if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

        return switch (lhs_mono) {
            .recursive_placeholder => unreachable,
            .unit => true,
            .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
            .list => |lhs_list| try self.monotypesStructurallyEqualRec(result, lhs_list.elem, rhs_mono.list.elem, seen),
            .box => |lhs_box| try self.monotypesStructurallyEqualRec(result, lhs_box.inner, rhs_mono.box.inner, seen),
            .tuple => |lhs_tuple| blk: {
                const lhs_elems = result.context_mono.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = result.context_mono.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
                if (lhs_elems.len != rhs_elems.len) break :blk false;
                for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                    if (!try self.monotypesStructurallyEqualRec(result, lhs_elem, rhs_elem, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .func => |lhs_func| blk: {
                const rhs_func = rhs_mono.func;
                const lhs_args = result.context_mono.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = result.context_mono.monotype_store.getIdxSpan(rhs_func.args);
                if (lhs_func.effectful != rhs_func.effectful) break :blk false;
                if (lhs_args.len != rhs_args.len) break :blk false;
                for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                    if (!try self.monotypesStructurallyEqualRec(result, lhs_arg, rhs_arg, seen)) {
                        break :blk false;
                    }
                }
                break :blk try self.monotypesStructurallyEqualRec(result, lhs_func.ret, rhs_func.ret, seen);
            },
            .record => |lhs_record| blk: {
                const lhs_fields = result.context_mono.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = result.context_mono.monotype_store.getFields(rhs_mono.record.fields);
                if (lhs_fields.len != rhs_fields.len) break :blk false;
                for (lhs_fields, rhs_fields) |lhs_field, rhs_field| {
                    if (!lhs_field.name.textEqual(self.all_module_envs, rhs_field.name)) break :blk false;
                    if (!try self.monotypesStructurallyEqualRec(result, lhs_field.type_idx, rhs_field.type_idx, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |lhs_union| blk: {
                const lhs_tags = result.context_mono.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = result.context_mono.monotype_store.getTags(rhs_mono.tag_union.tags);
                if (lhs_tags.len != rhs_tags.len) break :blk false;
                for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                    const lhs_payloads = result.context_mono.monotype_store.getIdxSpan(lhs_tag.payloads);
                    const rhs_payloads = result.context_mono.monotype_store.getIdxSpan(rhs_tag.payloads);
                    if (!lhs_tag.name.textEqual(self.all_module_envs, rhs_tag.name)) break :blk false;
                    if (lhs_payloads.len != rhs_payloads.len) break :blk false;
                    for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                        if (!try self.monotypesStructurallyEqualRec(result, lhs_payload, rhs_payload, seen)) {
                            break :blk false;
                        }
                    }
                }
                break :blk true;
            },
        };
    }

    fn monotypesStructurallyEqualAcrossModulesRec(
        self: *Pass,
        result: *const Result,
        lhs: Monotype.Idx,
        lhs_module_idx: u32,
        rhs: Monotype.Idx,
        rhs_module_idx: u32,
        seen: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        const lhs_u32: u32 = @intFromEnum(lhs);
        const rhs_u32: u32 = @intFromEnum(rhs);
        const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);
        if (seen.contains(key)) return true;
        try seen.put(key, {});

        const lhs_mono = result.context_mono.monotype_store.getMonotype(lhs);
        const rhs_mono = result.context_mono.monotype_store.getMonotype(rhs);
        if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

        return switch (lhs_mono) {
            .recursive_placeholder => unreachable,
            .unit => true,
            .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
            .list => |lhs_list| try self.monotypesStructurallyEqualAcrossModulesRec(
                result,
                lhs_list.elem,
                lhs_module_idx,
                rhs_mono.list.elem,
                rhs_module_idx,
                seen,
            ),
            .box => |lhs_box| try self.monotypesStructurallyEqualAcrossModulesRec(
                result,
                lhs_box.inner,
                lhs_module_idx,
                rhs_mono.box.inner,
                rhs_module_idx,
                seen,
            ),
            .tuple => |lhs_tuple| blk: {
                const lhs_elems = result.context_mono.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = result.context_mono.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
                if (lhs_elems.len != rhs_elems.len) break :blk false;
                for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                    if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                        result,
                        lhs_elem,
                        lhs_module_idx,
                        rhs_elem,
                        rhs_module_idx,
                        seen,
                    )) break :blk false;
                }
                break :blk true;
            },
            .func => |lhs_func| blk: {
                const rhs_func = rhs_mono.func;
                const lhs_args = result.context_mono.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = result.context_mono.monotype_store.getIdxSpan(rhs_func.args);
                if (lhs_func.effectful != rhs_func.effectful) break :blk false;
                if (lhs_args.len != rhs_args.len) break :blk false;
                for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                    if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                        result,
                        lhs_arg,
                        lhs_module_idx,
                        rhs_arg,
                        rhs_module_idx,
                        seen,
                    )) break :blk false;
                }
                break :blk try self.monotypesStructurallyEqualAcrossModulesRec(
                    result,
                    lhs_func.ret,
                    lhs_module_idx,
                    rhs_func.ret,
                    rhs_module_idx,
                    seen,
                );
            },
            .record => |lhs_record| blk: {
                const lhs_fields = result.context_mono.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = result.context_mono.monotype_store.getFields(rhs_mono.record.fields);
                if (lhs_fields.len != rhs_fields.len) break :blk false;

                var rhs_used = std.ArrayListUnmanaged(bool){};
                defer rhs_used.deinit(self.allocator);
                try rhs_used.resize(self.allocator, rhs_fields.len);
                @memset(rhs_used.items, false);

                for (lhs_fields) |lhs_field| {
                    var matched = false;
                    for (rhs_fields, 0..) |rhs_field, rhs_i| {
                        if (rhs_used.items[rhs_i]) continue;
                        if (!self.identsStructurallyEqualAcrossModules(
                            lhs_module_idx,
                            lhs_field.name,
                            rhs_module_idx,
                            rhs_field.name,
                        )) continue;
                        if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                            result,
                            lhs_field.type_idx,
                            lhs_module_idx,
                            rhs_field.type_idx,
                            rhs_module_idx,
                            seen,
                        )) break :blk false;
                        rhs_used.items[rhs_i] = true;
                        matched = true;
                        break;
                    }
                    if (!matched) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |lhs_union| blk: {
                const lhs_tags = result.context_mono.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = result.context_mono.monotype_store.getTags(rhs_mono.tag_union.tags);
                if (lhs_tags.len != rhs_tags.len) break :blk false;

                var rhs_used = std.ArrayListUnmanaged(bool){};
                defer rhs_used.deinit(self.allocator);
                try rhs_used.resize(self.allocator, rhs_tags.len);
                @memset(rhs_used.items, false);

                for (lhs_tags) |lhs_tag| {
                    var matched = false;
                    for (rhs_tags, 0..) |rhs_tag, rhs_i| {
                        if (rhs_used.items[rhs_i]) continue;
                        if (!self.identsStructurallyEqualAcrossModules(
                            lhs_module_idx,
                            lhs_tag.name,
                            rhs_module_idx,
                            rhs_tag.name,
                        )) continue;

                        const lhs_payloads = result.context_mono.monotype_store.getIdxSpan(lhs_tag.payloads);
                        const rhs_payloads = result.context_mono.monotype_store.getIdxSpan(rhs_tag.payloads);
                        if (lhs_payloads.len != rhs_payloads.len) continue;

                        var payloads_equal = true;
                        for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                            if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                                result,
                                lhs_payload,
                                lhs_module_idx,
                                rhs_payload,
                                rhs_module_idx,
                                seen,
                            )) {
                                payloads_equal = false;
                                break;
                            }
                        }
                        if (!payloads_equal) continue;

                        rhs_used.items[rhs_i] = true;
                        matched = true;
                        break;
                    }
                    if (!matched) break :blk false;
                }
                break :blk true;
            },
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

    fn getCallLowLevelOp(self: *Pass, caller_env: *const ModuleEnv, func_expr_idx: CIR.Expr.Idx) ?CIR.Expr.LowLevel {
        return switch (caller_env.store.getExpr(func_expr_idx)) {
            .e_lookup_external => |lookup| self.getExternalLowLevelOp(caller_env, lookup),
            .e_lookup_local => |lookup| getLocalLowLevelOp(caller_env, lookup.pattern_idx),
            else => null,
        };
    }

    fn getLocalLowLevelOp(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Expr.LowLevel {
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            if (def.pattern != pattern_idx) continue;
            return exprLowLevelOp(module_env.store.getExpr(def.expr), module_env);
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

    fn getExternalLowLevelOp(
        self: *Pass,
        caller_env: *const ModuleEnv,
        lookup: @TypeOf(@as(CIR.Expr, undefined).e_lookup_external),
    ) ?CIR.Expr.LowLevel {
        const ext_module_idx = self.resolveImportedModuleIdx(caller_env, lookup.module_idx) orelse return null;
        const ext_env = self.all_module_envs[ext_module_idx];
        if (!ext_env.store.isDefNode(lookup.target_node_idx)) return null;
        const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
        const def = ext_env.store.getDef(def_idx);
        return exprLowLevelOp(ext_env.store.getExpr(def.expr), ext_env);
    }

    fn exprLowLevelOp(expr: CIR.Expr, module_env: *const ModuleEnv) ?CIR.Expr.LowLevel {
        const body_expr_idx = switch (expr) {
            .e_lambda => |lambda_expr| lambda_expr.body,
            .e_hosted_lambda => |hosted_expr| hosted_expr.body,
            else => return null,
        };
        const body_expr = module_env.store.getExpr(body_expr_idx);
        if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
        return null;
    }
};

fn callableKind(expr: CIR.Expr) ?CallableTemplateKind {
    return switch (expr) {
        .e_lambda => .lambda,
        .e_closure => .closure,
        .e_hosted_lambda => .hosted_lambda,
        else => null,
    };
}

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
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
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
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    expr_idx: CIR.Expr.Idx,
    type_scope_module_idx: u32,
    type_scope: *const types.TypeScope,
    type_scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = type_scope_module_idx,
        .caller_module_idx = type_scope_caller_module_idx,
        .scope = type_scope,
    });
    try pass.runRootSourceExprInto(&result, expr_idx);
    return result;
}

/// Pipeline an explicit set of root source expressions in the current module.
pub fn runRootSourceExprs(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
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
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    exprs: []const CIR.Expr.Idx,
    type_scope_module_idx: u32,
    type_scope: *const types.TypeScope,
    type_scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = type_scope_module_idx,
        .caller_module_idx = type_scope_caller_module_idx,
        .scope = type_scope,
    });
    try pass.runRootSourceExprsInto(&result, exprs);
    return result;
}

/// Pipeline all callables rooted in the current module.
pub fn runModule(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Result {
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
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
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    type_scope_module_idx: u32,
    type_scope: *const types.TypeScope,
    type_scope_caller_module_idx: u32,
) Allocator.Error!Result {
    var result = try Result.init(allocator);
    errdefer result.deinit(allocator);
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    defer pass.deinit();
    try pass.seedExplicitTypeScopeMonotypes(&result, .{
        .module_idx = type_scope_module_idx,
        .caller_module_idx = type_scope_caller_module_idx,
        .scope = type_scope,
    });
    try pass.runModuleInto(&result);
    return result;
}
