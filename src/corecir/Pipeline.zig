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
pub const LambdaSetMemberId = Lambdasolved.LambdaSetMemberId;

/// Records a block-local polymorphic callable that is materialized on demand.
pub const DeferredLocalCallable = Lambdasolved.DeferredLocalCallable;

/// The canonical substitution assigned to one callable instantiation.
pub const TypeSubst = ContextMono.TypeSubst;

/// One concrete instantiation of a semantic callable template.
pub const CallableInst = Lambdamono.CallableInst;
pub const RuntimeValue = Lambdamono.RuntimeValue;

/// One callable requirement originating from a higher-order parameter.
pub const CallableParamSpecEntry = Lambdamono.CallableParamSpecEntry;

/// One structural projection applied before demanding exact callables from a higher-order parameter.
pub const CallableParamProjection = Lambdamono.CallableParamProjection;

/// Span of callable-parameter projections stored in the staged pipeline store.
pub const CallableParamProjectionSpan = Lambdamono.CallableParamProjectionSpan;

/// Span of callable-parameter specification entries stored in the staged pipeline store.
pub const CallableParamSpecSpan = Lambdamono.CallableParamSpecSpan;

pub const CaptureValueSource = Lambdamono.CaptureValueSource;
pub const CaptureStorage = Lambdamono.CaptureStorage;
pub const CaptureField = Lambdamono.CaptureField;
pub const CaptureFieldSpan = Lambdamono.CaptureFieldSpan;
pub const CallableDef = Lambdamono.CallableDef;
pub const CallableDefId = Lambdamono.CallableDefId;
pub const PackedCallableId = Lambdamono.PackedCallableId;
pub const PackedCallable = Lambdamono.PackedCallable;
pub const DispatchCallId = Lambdamono.DispatchCallId;
pub const DispatchCall = Lambdamono.DispatchCall;
pub const CallableValue = Lambdamono.CallableValue;
pub const CallDispatch = Lambdamono.CallDispatch;
pub const LookupResolution = Lambdamono.LookupResolution;

/// One statically resolved dispatch target definition.
pub const DispatchExprTarget = ContextMono.DispatchExprTarget;

/// Associates a source call site with the callable instantiation chosen for it.
pub const CallSiteResolution = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    callable_inst: CallableInstId,
};

const ContextExprKey = ContextMono.ContextExprKey;

const TemplateBodyCompletionKey = struct {
    template_source_key: u64,
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_raw: u32,
};

const ContextPatternKey = ContextMono.ContextPatternKey;

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
    callable_template_ids_by_source,
    source_exprs,
    deferred_local_callables,
    program_values,
    context_expr_monotypes,
    context_pattern_monotypes,
    context_dispatch_targets,
    callable_member_sets,
    packed_callables,
    packed_callable_ids_by_member_set,
    dispatch_calls,
    dispatch_call_ids_by_member_set,
    program_calls,
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

/// A source expression in a specific module, used for semantic value provenance.
pub const ExprSource = Lambdasolved.ExprSource;

const ContextExprSource = struct {
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

const ContextExprVisitKey = ContextExprKey;

const ContextExprResolution = union(enum) {
    none,
    blocked,
    source: ContextExprSource,
};

const CallResultCallableInstKey = struct {
    context_expr: ContextExprKey,
    callee_callable_inst_raw: u32,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const BuildStmtKey = struct {
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    stmt_raw: u32,
};

const ProgramBuildState = struct {
    stmt_ids_by_key: std.AutoHashMapUnmanaged(BuildStmtKey, Lambdamono.StmtId),

    fn init() ProgramBuildState {
        return .{
            .stmt_ids_by_key = .empty,
        };
    }

    fn deinit(self: *ProgramBuildState, allocator: Allocator) void {
        self.stmt_ids_by_key.deinit(allocator);
    }
};

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

    pub fn getExprCallDispatch(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallDispatch {
        const semantics = self.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.call_dispatch;
    }

    fn getExprCallDispatchMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const call_dispatch = self.getExprCallDispatch(source_context, module_idx, expr_idx) orelse return null;
        return switch (call_dispatch) {
            .direct => |callable_inst_id| self.lambdamono.getSingletonCallableMemberSetMembers(callable_inst_id),
            .dispatch_call => |dispatch_call_id| self.getDispatchCallMembers(dispatch_call_id),
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

    fn getExprCallableInst(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.getExprCallableValue(source_context, module_idx, expr_idx) orelse return null;
        return switch (callable_value) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_callable => null,
        };
    }

    pub fn getExprCallableValue(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const semantics = self.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.callable_value;
    }

    fn getExprCallableMembers(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const callable_value = self.getExprCallableValue(source_context, module_idx, expr_idx) orelse return null;
        return switch (callable_value) {
            .direct => |callable_inst_id| self.lambdamono.getSingletonCallableMemberSetMembers(callable_inst_id),
            .packed_callable => |packed_callable_id| self.getPackedCallableMembers(packed_callable_id),
        };
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        const semantics = self.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.dispatch_target;
    }

    pub fn getLookupResolution(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?Lambdamono.LookupResolution {
        const semantics = self.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.lookup_resolution;
    }

    pub fn getProgramExpr(self: *const Result, expr_id: Lambdamono.ExprId) *const Lambdamono.Expr {
        return self.lambdamono.getExpr(expr_id);
    }

    pub fn getContextPatternMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        if (self.context_mono.getContextPatternMonotype(source_context, module_idx, pattern_idx)) |resolved| {
            return resolved;
        }

        const callable_inst_id = switch (source_context) {
            .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
            .root_expr, .provenance_expr, .template_expr => return null,
        };
        const callable_value = self.getCallableParamBindingValue(callable_inst_id, pattern_idx) orelse return null;
        return self.getCallableValueMonotype(callable_value);
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

    pub fn getCallableParamBindingValue(
        self: *const Result,
        callable_inst_id: CallableInstId,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        return self.lambdamono.getCallableParamBindingValue(self.getCallableInst(callable_inst_id).callable_def, pattern_idx);
    }

    pub fn getPackedCallable(self: *const Result, packed_callable_id: PackedCallableId) *const PackedCallable {
        return Lambdamono.getPackedCallable(&self.lambdamono, packed_callable_id);
    }

    pub fn getCallableValueMonotype(
        self: *const Result,
        callable_value: CallableValue,
    ) ResolvedMonotype {
        return switch (callable_value) {
            .direct => |callable_inst_id| blk: {
                const callable_inst = self.getCallableInst(callable_inst_id);
                break :blk .{
                    .idx = callable_inst.fn_monotype,
                    .module_idx = callable_inst.fn_monotype_module_idx,
                };
            },
            .packed_callable => |packed_callable_id| self.getPackedCallable(packed_callable_id).fn_monotype,
        };
    }

    pub fn getDispatchCall(self: *const Result, dispatch_call_id: DispatchCallId) *const DispatchCall {
        return Lambdamono.getDispatchCall(&self.lambdamono, dispatch_call_id);
    }

    pub fn getPackedCallableMembers(self: *const Result, packed_callable_id: PackedCallableId) []const CallableInstId {
        return Lambdamono.getCallableMembers(&self.lambdamono, self.getPackedCallable(packed_callable_id).members);
    }

    pub fn getDispatchCallMembers(self: *const Result, dispatch_call_id: DispatchCallId) []const CallableInstId {
        return Lambdamono.getCallableMembers(&self.lambdamono, self.getDispatchCall(dispatch_call_id).members);
    }

    pub fn getCaptureFields(self: *const Result, span: CaptureFieldSpan) []const CaptureField {
        return Lambdamono.getCaptureFields(&self.lambdamono, span);
    }

    pub fn getCallableParamSpecEntries(
        self: *const Result,
        span: CallableParamSpecSpan,
    ) []const CallableParamSpecEntry {
        return self.lambdamono.getCallableParamSpecEntries(span);
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Result,
        span: CallableParamProjectionSpan,
    ) []const CallableParamProjection {
        return self.lambdamono.getCallableParamProjectionEntries(span);
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

    fn getExprCallableTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?CallableTemplateId {
        return self.lambdasolved.getExprCallableTemplate(module_idx, expr_idx);
    }

    fn getDeferredLocalCallable(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?DeferredLocalCallable {
        return self.lambdasolved.getDeferredLocalCallable(module_idx, pattern_idx);
    }

    pub fn getPatternSourceExpr(
        self: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprSource {
        return self.lambdasolved.getPatternSourceExpr(module_idx, pattern_idx);
    }
};

/// Pipelines callable templates into explicit callable instantiations.
pub const Pass = struct {
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    type_scope: ?*const types.TypeScope,
    type_scope_module_idx: ?u32,
    type_scope_caller_module_idx: ?u32,
    visited_modules: std.AutoHashMapUnmanaged(u32, void),
    visited_exprs: std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    in_progress_value_defs: std.AutoHashMapUnmanaged(ContextExprKey, void),
    in_progress_call_result_callable_insts: std.AutoHashMapUnmanaged(CallResultCallableInstKey, void),
    in_progress_callable_scans: std.AutoHashMapUnmanaged(u32, void),
    completed_callable_scans: std.AutoHashMapUnmanaged(u32, void),
    in_progress_template_body_completions: std.AutoHashMapUnmanaged(TemplateBodyCompletionKey, void),
    program_build_state: ProgramBuildState,
    mutation_revision: u64,
    mutation_counts: [mutation_kind_count]u32,
    scratch_context_expr_monotypes_depth: u32,
    active_bindings: ?*std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    active_iteration_expr_monotypes: ?*std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    active_iteration_pattern_monotypes: ?*std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype),
    source_context_stack: std.ArrayListUnmanaged(SourceContext),
    suppress_direct_call_resolution: bool,
    binding_probe_mode: bool,
    binding_probe_failed: bool,

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
            .type_scope = null,
            .type_scope_module_idx = null,
            .type_scope_caller_module_idx = null,
            .visited_modules = .empty,
            .visited_exprs = .empty,
            .in_progress_value_defs = .empty,
            .in_progress_call_result_callable_insts = .empty,
            .in_progress_callable_scans = .empty,
            .completed_callable_scans = .empty,
            .in_progress_template_body_completions = .empty,
            .program_build_state = ProgramBuildState.init(),
            .mutation_revision = 0,
            .mutation_counts = [_]u32{0} ** mutation_kind_count,
            .scratch_context_expr_monotypes_depth = 0,
            .active_bindings = null,
            .active_iteration_expr_monotypes = null,
            .active_iteration_pattern_monotypes = null,
            .source_context_stack = .empty,
            .suppress_direct_call_resolution = false,
            .binding_probe_mode = false,
            .binding_probe_failed = false,
        };
    }

    pub fn setTypeScope(
        self: *Pass,
        module_idx: u32,
        type_scope: *const types.TypeScope,
        caller_module_idx: u32,
    ) void {
        self.type_scope = type_scope;
        self.type_scope_module_idx = module_idx;
        self.type_scope_caller_module_idx = caller_module_idx;
    }

    fn callableInstSourceContext(context_callable_inst: CallableInstId) SourceContext {
        return .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) };
    }

    fn currentSourceContext(self: *const Pass) SourceContext {
        if (self.source_context_stack.items.len == 0) {
            std.debug.panic("Pipeline invariant violated: missing active source context", .{});
        }
        return self.source_context_stack.items[self.source_context_stack.items.len - 1];
    }

    fn pushSourceContext(self: *Pass, source_context: SourceContext) Allocator.Error!void {
        try self.source_context_stack.append(self.allocator, source_context);
    }

    fn popSourceContext(self: *Pass) void {
        if (builtin.mode == .Debug and self.source_context_stack.items.len == 0) {
            std.debug.panic("Pipeline invariant violated: source context stack underflow", .{});
        }
        _ = self.source_context_stack.pop();
    }

    fn requireCurrentCallableInstContext(self: *const Pass) CallableInstId {
        return switch (self.currentSourceContext()) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => std.debug.panic(
                "Pipeline invariant violated: operation required callable-inst context but active source context was {s}",
                .{@tagName(self.currentSourceContext())},
            ),
        };
    }

    fn currentCallableInstRawForDebug(self: *const Pass) u32 {
        return switch (self.currentSourceContext()) {
            .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
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

    fn templateContextForSourceContext(
        _: *const Pass,
        result: *const Result,
        source_context: SourceContext,
    ) ?CallableTemplateId {
        return switch (source_context) {
            .callable_inst => |context_id| result.getCallableInst(@enumFromInt(@intFromEnum(context_id))).template,
            .template_expr => |template_ctx| result.getExprCallableTemplate(
                template_ctx.module_idx,
                template_ctx.expr_idx,
            ) orelse std.debug.panic(
                "Pipeline invariant violated: template source context module={d} expr={d} had no registered callable template",
                .{ template_ctx.module_idx, @intFromEnum(template_ctx.expr_idx) },
            ),
            .root_expr, .provenance_expr => null,
        };
    }

    fn currentTemplateContext(self: *const Pass, result: *const Result) ?CallableTemplateId {
        if (self.source_context_stack.items.len == 0) return null;
        return self.templateContextForSourceContext(result, self.currentSourceContext());
    }

    fn activeCallableTemplate(self: *const Pass, result: *const Result) ?CallableTemplate {
        const template_id = self.currentTemplateContext(result) orelse return null;
        return result.getCallableTemplate(template_id).*;
    }

    fn activeRootExprRawForDebug(self: *const Pass) u32 {
        if (self.source_context_stack.items.len == 0) return std.math.maxInt(u32);
        return switch (self.currentSourceContext()) {
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .callable_inst, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
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

    fn currentResultExprKey(
        self: *const Pass,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return self.resultExprKeyForSourceContext(self.currentSourceContext(), module_idx, expr_idx);
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

    fn currentResultPatternKey(
        self: *const Pass,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return self.resultPatternKeyForSourceContext(self.currentSourceContext(), module_idx, pattern_idx);
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

    fn appendCallableParamBinding(
        self: *Pass,
        result: *Result,
        callable_def: *Lambdamono.CallableDef,
        binding: Lambdamono.CallableParamBinding,
    ) Allocator.Error!void {
        const bindings = result.lambdamono.getCallableParamBindings(callable_def.param_bindings);
        for (bindings, 0..) |existing, idx| {
            if (existing.pattern_idx != binding.pattern_idx) continue;
            const existing_index = callable_def.param_bindings.start + @as(u32, @intCast(idx));
            if (!std.meta.eql(result.lambdamono.callable_param_bindings.items[existing_index], binding)) {
                result.lambdamono.callable_param_bindings.items[existing_index] = binding;
                self.noteMutation(.program_values);
            }
            return;
        }

        const start = result.lambdamono.callable_param_bindings.items.len;
        if (callable_def.param_bindings.len != 0 and
            callable_def.param_bindings.start + callable_def.param_bindings.len != start)
        {
            try result.lambdamono.callable_param_bindings.appendSlice(self.allocator, bindings);
            callable_def.param_bindings.start = @intCast(start);
        } else if (callable_def.param_bindings.len == 0) {
            callable_def.param_bindings.start = @intCast(start);
        }

        try result.lambdamono.callable_param_bindings.append(self.allocator, binding);
        callable_def.param_bindings.len += 1;
        self.noteMutation(.program_values);
    }

    fn directCallableFromCallableValue(callable_value: CallableValue) ?CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_callable => null,
        };
    }

    fn callableInstsFromCallableValue(result: *const Result, callable_value: CallableValue) []const CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| result.lambdamono.getSingletonCallableMemberSetMembers(callable_inst_id),
            .packed_callable => |packed_callable_id| result.getPackedCallableMembers(packed_callable_id),
        };
    }

    fn directCallableFromCallDispatch(call_dispatch: CallDispatch) ?CallableInstId {
        return switch (call_dispatch) {
            .direct => |callable_inst_id| callable_inst_id,
            .dispatch_call => null,
        };
    }

    fn callableInstsFromCallDispatch(result: *const Result, call_dispatch: CallDispatch) []const CallableInstId {
        return switch (call_dispatch) {
            .direct => |callable_inst_id| result.lambdamono.getSingletonCallableMemberSetMembers(callable_inst_id),
            .dispatch_call => |dispatch_call_id| result.getDispatchCallMembers(dispatch_call_id),
        };
    }

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

    fn getContextPatternMonotypeInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        return result.getContextPatternMonotype(self.currentSourceContext(), module_idx, pattern_idx);
    }

    fn resolveBoundPatternCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const callable_value = self.readCallableParamValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
        ) orelse return null;
        return directCallableFromCallableValue(callable_value);
    }

    fn resolveBoundPatternCallableInstInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableInstId {
        const callable_value = self.readCallableParamValue(
            result,
            self.currentSourceContext(),
            module_idx,
            pattern_idx,
        ) orelse return null;
        return directCallableFromCallableValue(callable_value);
    }

    fn resolveBoundPatternCallableMembersInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const CallableInstId {
        const callable_value = self.readCallableParamValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
        ) orelse return null;
        return callableInstsFromCallableValue(result, callable_value);
    }

    fn resolveBoundPatternCallableMembersInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const CallableInstId {
        const callable_value = self.readCallableParamValue(
            result,
            self.currentSourceContext(),
            module_idx,
            pattern_idx,
        ) orelse return null;
        return callableInstsFromCallableValue(result, callable_value);
    }

    fn getExprCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.readExprCallableValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        ) orelse return null;
        return directCallableFromCallableValue(callable_value);
    }

    fn getExprCallableInstInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const callable_value = self.readExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        ) orelse return null;
        return directCallableFromCallableValue(callable_value);
    }

    fn getExprCallableInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const callable_value = self.readExprCallableValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        ) orelse return null;
        return callableInstsFromCallableValue(result, callable_value);
    }

    fn getExprCallableInstsInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const callable_value = self.readExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        ) orelse return null;
        return callableInstsFromCallableValue(result, callable_value);
    }

    fn getCallSiteCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_dispatch = self.readExprCallDispatch(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        ) orelse return null;
        return directCallableFromCallDispatch(call_dispatch);
    }

    fn getCallSiteCallableInstInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_dispatch = self.readExprCallDispatch(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        ) orelse return null;
        return directCallableFromCallDispatch(call_dispatch);
    }

    fn getCallSiteCallableInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const call_dispatch = self.readExprCallDispatch(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        ) orelse return null;
        return callableInstsFromCallDispatch(result, call_dispatch);
    }

    fn getCallSiteCallableInstsInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const call_dispatch = self.readExprCallDispatch(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        ) orelse return null;
        return callableInstsFromCallDispatch(result, call_dispatch);
    }

    fn getCallSiteCallableInstForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const call_dispatch = self.readExprCallDispatch(result, source_context, module_idx, expr_idx) orelse return null;
        return directCallableFromCallDispatch(call_dispatch);
    }

    fn getCallSiteCallableInstsForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const call_dispatch = self.readExprCallDispatch(result, source_context, module_idx, expr_idx) orelse return null;
        return callableInstsFromCallDispatch(result, call_dispatch);
    }

    fn getValueExprCallableInstInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableInstInContext(result, context_callable_inst, module_idx, lookup.pattern_idx)) |callable_inst_id| {
                    return callable_inst_id;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsInContext(result, context_callable_inst, module_idx, expr_idx)) |callable_inst_ids| {
            if (callable_inst_ids.len == 1) return callable_inst_ids[0];
        }

        if (self.getExprCallableInstInContext(result, context_callable_inst, module_idx, expr_idx)) |callable_inst_id| {
            return callable_inst_id;
        }
        return null;
    }

    fn getValueExprCallableInstInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableInstInCurrentContext(result, module_idx, lookup.pattern_idx)) |callable_inst_id| {
                    return callable_inst_id;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callable_inst_ids| {
            if (callable_inst_ids.len == 1) return callable_inst_ids[0];
        }

        if (self.getExprCallableInstInCurrentContext(result, module_idx, expr_idx)) |callable_inst_id| {
            return callable_inst_id;
        }
        return null;
    }

    fn getValueExprCallableInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableMembersInContext(result, context_callable_inst, module_idx, lookup.pattern_idx)) |callable_inst_ids| {
                    return callable_inst_ids;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsInContext(result, context_callable_inst, module_idx, expr_idx)) |callable_inst_ids| {
            return callable_inst_ids;
        }

        if (self.getExprCallableInstsInContext(result, context_callable_inst, module_idx, expr_idx)) |callable_inst_ids| {
            return callable_inst_ids;
        }
        return null;
    }

    fn getValueExprCallableInstsInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.resolveBoundPatternCallableMembersInCurrentContext(result, module_idx, lookup.pattern_idx)) |callable_inst_ids| {
                    return callable_inst_ids;
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callable_inst_ids| {
            return callable_inst_ids;
        }

        if (self.getExprCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callable_inst_ids| {
            return callable_inst_ids;
        }
        return null;
    }

    fn getCallableParamSpecCallableInstsForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
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

        for (result.getCallableParamSpecEntries(context_callable.callable_param_specs)) |spec| {
            if (spec.param_index != param_index) continue;
            if (!self.callableParamProjectionSeqEqual(
                result.getCallableParamProjectionEntries(spec.projections),
                projections.items,
            )) continue;
            return result.lambdamono.getCallableMemberSetMembers(spec.callable_member_set_id);
        }

        return null;
    }

    fn getCallableParamSpecCallableInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        return self.getCallableParamSpecCallableInstsForSourceContext(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getCallableParamSpecCallableInstsInCurrentContext(
        self: *const Pass,
        result: *const Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        return self.getCallableParamSpecCallableInstsForSourceContext(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn getValueExprCallableInstForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableInstId {
        switch (source_context) {
            .callable_inst => |context_id| {
                return self.getValueExprCallableInstInContext(
                    result,
                    @enumFromInt(@intFromEnum(context_id)),
                    module_idx,
                    expr_idx,
                );
            },
            .root_expr, .provenance_expr, .template_expr => {},
        }

        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (sourceContextHasCallableInst(source_context)) {
                    if (self.readCallableParamValue(result, source_context, module_idx, lookup.pattern_idx)) |callable_value| {
                        if (directCallableFromCallableValue(callable_value)) |callable_inst_id| {
                            return callable_inst_id;
                        }
                    }
                } else if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    return self.getValueExprCallableInstForSourceContext(
                        result,
                        source_context,
                        source.module_idx,
                        source.expr_idx,
                    );
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
            if (callable_inst_ids.len == 1) return callable_inst_ids[0];
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            if (directCallableFromCallableValue(callable_value)) |callable_inst_id| {
                return callable_inst_id;
            }
        }

        return null;
    }

    fn getValueExprCallableMembersForSourceContext(
        self: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const CallableInstId {
        switch (source_context) {
            .callable_inst => |context_id| {
                return self.getValueExprCallableInstsInContext(
                    result,
                    @enumFromInt(@intFromEnum(context_id)),
                    module_idx,
                    expr_idx,
                );
            },
            .root_expr, .provenance_expr, .template_expr => {},
        }

        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (sourceContextHasCallableInst(source_context)) {
                    if (self.readCallableParamValue(result, source_context, module_idx, lookup.pattern_idx)) |callable_value| {
                        return callableInstsFromCallableValue(result, callable_value);
                    }
                } else if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    return self.getValueExprCallableMembersForSourceContext(
                        result,
                        source_context,
                        source.module_idx,
                        source.expr_idx,
                    );
                }
            },
            else => {},
        }

        if (self.getCallableParamSpecCallableInstsForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
            return callable_inst_ids;
        }

        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |callable_value| {
            return callableInstsFromCallableValue(result, callable_value);
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
        self.in_progress_template_body_completions.deinit(self.allocator);
        self.program_build_state.deinit(self.allocator);
        self.source_context_stack.deinit(self.allocator);
    }

    pub fn runRootSourceExpr(self: *Pass, expr_idx: CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);

        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);
        try self.scanRootsFixedPoint(&result, &.{expr_idx}, true);
        try self.finalizeLambdamonoProgram(&result, &.{expr_idx});

        return result;
    }

    pub fn runRootSourceExprs(self: *Pass, exprs: []const CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator);

        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);
        try self.scanRootsFixedPoint(&result, exprs, true);
        try self.finalizeLambdamonoProgram(&result, exprs);

        return result;
    }

    /// Pipeline all callables rooted in the current module.
    pub fn runModule(self: *Pass) Allocator.Error!Result {
        var result = try Result.init(self.allocator);

        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);

        const module_env = self.all_module_envs[self.current_module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        var root_source_exprs = std.ArrayList(CIR.Expr.Idx).empty;
        defer root_source_exprs.deinit(self.allocator);
        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try root_source_exprs.append(self.allocator, def.expr);
        }
        try self.scanRootsFixedPoint(&result, root_source_exprs.items, true);
        try self.finalizeLambdamonoProgram(&result, root_source_exprs.items);

        return result;
    }

    fn finalizeLambdamonoProgram(
        self: *Pass,
        result: *Result,
        explicit_root_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        try self.populateLambdamonoCallableBodies(result);
        try self.populateExplicitRootExprs(result, explicit_root_exprs);
        try self.populateVisitedRootExprs(result);
    }

    fn populateExplicitRootExprs(
        self: *Pass,
        result: *Result,
        explicit_root_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (explicit_root_exprs) |expr_idx| {
            try self.ensureProgramRootExpr(
                result,
                .{ .root_expr = .{ .module_idx = self.current_module_idx, .expr_idx = expr_idx } },
                self.current_module_idx,
                expr_idx,
            );
        }
    }

    fn populateVisitedRootExprs(
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
            try self.ensureProgramRootExpr(
                result,
                source_context,
                key.module_idx,
                @enumFromInt(key.expr_raw),
            );
        }
    }

    fn populateLambdamonoCallableBodies(
        self: *Pass,
        result: *Result,
    ) Allocator.Error!void {
        for (result.lambdamono.callable_insts.items, 0..) |callable_inst, raw_callable_inst_id| {
            const callable_inst_id: CallableInstId = @enumFromInt(raw_callable_inst_id);
            const template = result.getCallableTemplate(callable_inst.template);
            const module_env = self.all_module_envs[template.module_idx];
            const runtime_expr = module_env.store.getExpr(template.runtime_expr);
            const boundary = switch (runtime_expr) {
                .e_lambda => |lambda_expr| CallableBoundaryInfo{
                    .arg_patterns = module_env.store.slicePatterns(lambda_expr.args),
                    .body_expr = lambda_expr.body,
                },
                .e_closure => |closure_expr| blk: {
                    const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                    if (lambda_expr != .e_lambda) {
                        std.debug.panic(
                            "Pipeline invariant violated: closure runtime expr {d} for callable inst {d} in module {d} did not reference an e_lambda body",
                            .{ @intFromEnum(template.runtime_expr), raw_callable_inst_id, template.module_idx },
                        );
                    }
                    break :blk CallableBoundaryInfo{
                        .arg_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args),
                        .body_expr = lambda_expr.e_lambda.body,
                    };
                },
                .e_hosted_lambda => |hosted_expr| CallableBoundaryInfo{
                    .arg_patterns = module_env.store.slicePatterns(hosted_expr.args),
                    .body_expr = hosted_expr.body,
                },
                else => std.debug.panic(
                    "Pipeline invariant violated: callable inst {d} runtime expr {d} in module {d} was not lambda-shaped",
                    .{ raw_callable_inst_id, @intFromEnum(template.runtime_expr), template.module_idx },
                ),
            };
            const callable_def = &result.lambdamono.callable_defs.items[@intFromEnum(callable_inst.callable_def)];
            _ = try self.ensureProgramExpr(
                result,
                callableInstSourceContext(callable_inst_id),
                template.module_idx,
                boundary.body_expr,
            );
            callable_def.param_bindings = try self.collectCallableParamBindings(
                result,
                callable_inst_id,
                template.module_idx,
                boundary.arg_patterns,
            );
        }
    }

    fn collectCallableParamBindings(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        module_idx: u32,
        arg_patterns: []const CIR.Pattern.Idx,
    ) Allocator.Error!Lambdamono.CallableParamBindingSpan {
        const start: u32 = @intCast(result.lambdamono.callable_param_bindings.items.len);
        for (arg_patterns) |pattern_idx| {
            const callable_value = self.readCallableParamValue(
                result,
                callableInstSourceContext(callable_inst_id),
                module_idx,
                pattern_idx,
            ) orelse continue;
            try result.lambdamono.callable_param_bindings.append(self.allocator, .{
                .pattern_idx = pattern_idx,
                .callable_value = callable_value,
            });
        }
        return .{
            .start = start,
            .len = @intCast(result.lambdamono.callable_param_bindings.items.len - start),
        };
    }

    fn ensureProgramRootExpr(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const key = Result.contextExprKey(source_context, module_idx, expr_idx);
        if (result.lambdamono.root_expr_ids_by_key.contains(key)) return;
        const body_expr = try self.ensureProgramExpr(result, source_context, module_idx, expr_idx);
        const root_expr_id: Lambdamono.RootExprId = @enumFromInt(result.lambdamono.root_exprs.items.len);
        try result.lambdamono.root_exprs.append(self.allocator, .{ .key = key, .body_expr = body_expr });
        try result.lambdamono.root_expr_ids_by_key.put(self.allocator, key, root_expr_id);
    }

    fn ensureProgramExpr(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Lambdamono.ExprId {
        const key = Result.contextExprKey(source_context, module_idx, expr_idx);
        if (result.lambdamono.expr_ids_by_key.get(key)) |existing| {
            try self.refreshProgramExpr(result, existing, source_context, module_idx, expr_idx);
            return existing;
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var child_exprs = std.ArrayList(Lambdamono.ExprId).empty;
        defer child_exprs.deinit(self.allocator);
        var child_stmts = std.ArrayList(Lambdamono.StmtId).empty;
        defer child_stmts.deinit(self.allocator);

        switch (expr) {
            .e_str => |str_expr| for (module_env.store.sliceExpr(str_expr.span)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, child_expr_idx));
            },
            .e_list => |list_expr| for (module_env.store.sliceExpr(list_expr.elems)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, child_expr_idx));
            },
            .e_tuple => |tuple_expr| for (module_env.store.sliceExpr(tuple_expr.elems)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, child_expr_idx));
            },
            .e_match => |match_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, match_expr.cond));
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard_expr_idx| {
                        try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, guard_expr_idx));
                    }
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, branch.value));
                }
            },
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, branch.cond));
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, branch.body));
                }
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, if_expr.final_else));
            },
            .e_call => |call_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, call_expr.func));
                for (module_env.store.sliceExpr(call_expr.args)) |arg_expr_idx| {
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, arg_expr_idx));
                }
            },
            .e_record => |record_expr| {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, field.value));
                }
                if (record_expr.ext) |ext_expr_idx| {
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, ext_expr_idx));
                }
            },
            .e_block => |block_expr| {
                for (module_env.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                    try child_stmts.append(self.allocator, try self.ensureProgramStmt(result, source_context, module_idx, stmt_idx));
                }
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, block_expr.final_expr));
            },
            .e_tag => |tag_expr| for (module_env.store.sliceExpr(tag_expr.args)) |child_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, child_expr_idx));
            },
            .e_nominal => |nominal_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, nominal_expr.backing_expr));
            },
            .e_nominal_external => |nominal_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, nominal_expr.backing_expr));
            },
            .e_binop => |binop_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, binop_expr.lhs));
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, binop_expr.rhs));
            },
            .e_unary_minus => |unary_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, unary_expr.expr));
            },
            .e_unary_not => |unary_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, unary_expr.expr));
            },
            .e_dot_access => |dot_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, dot_expr.receiver));
                if (dot_expr.args) |args| for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
                    try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, arg_expr_idx));
                };
            },
            .e_tuple_access => |tuple_access| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, tuple_access.tuple));
            },
            .e_dbg => |dbg_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, dbg_expr.expr));
            },
            .e_expect => |expect_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, expect_expr.body));
            },
            .e_return => |return_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, return_expr.expr));
            },
            .e_type_var_dispatch => |dispatch_expr| for (module_env.store.sliceExpr(dispatch_expr.args)) |arg_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, arg_expr_idx));
            },
            .e_for => |for_expr| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, for_expr.expr));
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, for_expr.body));
            },
            .e_run_low_level => |run_low_level| for (module_env.store.sliceExpr(run_low_level.args)) |arg_expr_idx| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, arg_expr_idx));
            },
            else => {},
        }

        const expr_id: Lambdamono.ExprId = @enumFromInt(result.lambdamono.exprs.items.len);
        const child_expr_span = try self.appendProgramExprChildren(&result.lambdamono, child_exprs.items);
        const child_stmt_span = try self.appendProgramStmtChildren(&result.lambdamono, child_stmts.items);
        try result.lambdamono.exprs.append(self.allocator, .{
            .source_context = source_context,
            .module_idx = module_idx,
            .source_expr = expr_idx,
            .monotype = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx),
            .child_exprs = child_expr_span,
            .child_stmts = child_stmt_span,
            .semantics = try self.programExprSemantics(result, source_context, module_idx, expr_idx),
        });
        try result.lambdamono.expr_ids_by_key.put(self.allocator, key, expr_id);
        return expr_id;
    }

    fn refreshProgramExpr(
        self: *Pass,
        result: *Result,
        expr_id: Lambdamono.ExprId,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const program_expr = &result.lambdamono.exprs.items[@intFromEnum(expr_id)];
        program_expr.source_context = source_context;
        program_expr.module_idx = module_idx;
        program_expr.source_expr = expr_idx;
        program_expr.monotype = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
        program_expr.semantics = try self.programExprSemantics(result, source_context, module_idx, expr_idx);
    }

    fn ensureProgramStmt(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        stmt_idx: CIR.Statement.Idx,
    ) Allocator.Error!Lambdamono.StmtId {
        const key = self.buildStmtKey(source_context, module_idx, stmt_idx);
        if (self.program_build_state.stmt_ids_by_key.get(key)) |existing| return existing;

        const module_env = self.all_module_envs[module_idx];
        const stmt = module_env.store.getStatement(stmt_idx);
        var child_exprs = std.ArrayList(Lambdamono.ExprId).empty;
        defer child_exprs.deinit(self.allocator);

        switch (stmt) {
            .s_decl => |decl| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, decl.expr)),
            .s_var => |var_decl| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, var_decl.expr)),
            .s_reassign => |reassign| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, reassign.expr)),
            .s_dbg => |dbg_stmt| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, dbg_stmt.expr)),
            .s_expr => |expr_stmt| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, expr_stmt.expr)),
            .s_expect => |expect_stmt| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, expect_stmt.body)),
            .s_for => |for_stmt| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, for_stmt.expr));
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, for_stmt.body));
            },
            .s_while => |while_stmt| {
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, while_stmt.cond));
                try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, while_stmt.body));
            },
            .s_return => |return_stmt| try child_exprs.append(self.allocator, try self.ensureProgramExpr(result, source_context, module_idx, return_stmt.expr)),
            else => {},
        }

        const stmt_id: Lambdamono.StmtId = @enumFromInt(result.lambdamono.stmts.items.len);
        try result.lambdamono.stmts.append(self.allocator, .{
            .module_idx = module_idx,
            .source_stmt = stmt_idx,
            .child_exprs = try self.appendProgramExprChildren(&result.lambdamono, child_exprs.items),
        });
        try self.program_build_state.stmt_ids_by_key.put(self.allocator, key, stmt_id);
        return stmt_id;
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

    fn programExprSemantics(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Lambdamono.ExprSemantics {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var semantics: Lambdamono.ExprSemantics = .{};

        semantics.callable_value = self.readExprCallableValue(result, source_context, module_idx, expr_idx);
        semantics.call_dispatch = self.readExprCallDispatch(result, source_context, module_idx, expr_idx);
        semantics.dispatch_target = result.context_mono.getDispatchExprTarget(source_context, module_idx, expr_idx);
        semantics.lookup_resolution = switch (expr) {
            .e_lookup_local => |lookup| blk: {
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    break :blk .{ .expr = try self.ensureProgramExpr(
                        result,
                        source_context,
                        source.module_idx,
                        source.expr_idx,
                    ) };
                }
                if (findDefByPattern(module_env, lookup.pattern_idx)) |def_idx| {
                    break :blk .{ .def = .{
                        .module_idx = module_idx,
                        .def_idx = def_idx,
                    } };
                }
                break :blk null;
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) break :blk null;
                break :blk .{ .def = .{
                    .module_idx = target_module_idx,
                    .def_idx = @enumFromInt(lookup.target_node_idx),
                } };
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                break :blk .{ .def = .{
                    .module_idx = target.module_idx,
                    .def_idx = target.def_idx,
                } };
            },
            else => null,
        };
        return semantics;
    }

    fn primeAllModules(self: *Pass, result: *Result) Allocator.Error!void {
        for (self.all_module_envs, 0..) |_, module_idx| {
            try self.primeModuleDefs(result, @intCast(module_idx));
        }
    }

    fn scanRootsFixedPoint(
        self: *Pass,
        result: *Result,
        exprs: []const CIR.Expr.Idx,
        contextualize_roots: bool,
    ) Allocator.Error!void {
        var iterations: u32 = 0;

        while (true) {
            iterations += 1;
            if (std.debug.runtime_safety and iterations > 32) {
                std.debug.panic(
                    "Pipeline: root fixed point did not converge (module={d}, contextualize_roots={}, revision={d}, templates={d}, callable_insts={d}, program_exprs={d}, root_exprs={d}, context_monos={d}, context_pattern_monos={d}, mutation_counts={any})",
                    .{
                        self.current_module_idx,
                        contextualize_roots,
                        self.mutation_revision,
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

            self.visited_exprs.clearRetainingCapacity();
            self.in_progress_value_defs.clearRetainingCapacity();
            self.in_progress_call_result_callable_insts.clearRetainingCapacity();
            self.completed_callable_scans.clearRetainingCapacity();

            const mutation_revision_before = self.mutation_revision;
            for (exprs) |expr_idx| {
                if (contextualize_roots) {
                    {
                        try self.pushSourceContext(.{ .root_expr = .{
                            .module_idx = self.current_module_idx,
                            .expr_idx = expr_idx,
                        } });
                        defer self.popSourceContext();
                        try self.scanCirValueExpr(result, self.current_module_idx, expr_idx);
                    }
                } else {
                    try self.scanCirValueExpr(result, self.current_module_idx, expr_idx);
                }
            }

            if (self.mutation_revision == mutation_revision_before) {
                break;
            }
        }
    }

    fn noteMutation(self: *Pass, comptime kind: MutationKind) void {
        self.mutation_revision +%= 1;
        self.mutation_counts[@intFromEnum(kind)] +%= 1;
    }

    fn putTracked(self: *Pass, comptime kind: MutationKind, map: anytype, key: anytype, value: anytype) Allocator.Error!void {
        const gop = try map.getOrPut(self.allocator, key);
        const typed_value: @TypeOf(gop.value_ptr.*) = value;
        if (!gop.found_existing or !std.meta.eql(gop.value_ptr.*, typed_value)) {
            gop.value_ptr.* = typed_value;
            self.noteMutation(kind);
        }
    }

    fn removeTracked(self: *Pass, comptime kind: MutationKind, map: anytype, key: anytype) void {
        if (map.remove(key)) {
            self.noteMutation(kind);
        }
    }

    fn appendTracked(self: *Pass, comptime kind: MutationKind, list: anytype, value: anytype) Allocator.Error!void {
        const typed_value: @TypeOf(list.items[0]) = value;
        try list.append(self.allocator, typed_value);
        self.noteMutation(kind);
    }

    fn writeExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_value: CallableValue,
    ) Allocator.Error!void {
        _ = try self.ensureProgramExpr(result, source_context, module_idx, expr_idx);
        const semantics = result.lambdamono.getExprSemanticsPtr(source_context, module_idx, expr_idx).?;
        if (semantics.callable_value == null or !std.meta.eql(semantics.callable_value.?, callable_value)) {
            semantics.callable_value = callable_value;
            self.noteMutation(.program_values);
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
        _ = module_idx;
        const callable_inst_id = switch (source_context) {
            .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
            .root_expr, .provenance_expr, .template_expr => std.debug.panic(
                "Pipeline invariant violated: attempted to record callable-pattern value for non-callable source context kind {s}",
                .{@tagName(source_context)},
            ),
        };
        const callable_def = &result.lambdamono.callable_defs.items[@intFromEnum(result.getCallableInst(callable_inst_id).callable_def)];
        try self.appendCallableParamBinding(result, callable_def, .{
            .pattern_idx = pattern_idx,
            .callable_value = callable_value,
        });
    }

    fn readExprCallableValue(
        _: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallableValue {
        const semantics = result.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.callable_value;
    }

    fn readCallableParamValue(
        _: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        _ = module_idx;
        const callable_inst_id = switch (source_context) {
            .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
            .root_expr, .provenance_expr, .template_expr => return null,
        };
        const callable_def = result.getCallableDefForInst(callable_inst_id);
        for (result.lambdamono.getCallableParamBindings(callable_def.param_bindings)) |binding| {
            if (binding.pattern_idx == pattern_idx) {
                return binding.callable_value;
            }
        }
        return null;
    }

    fn writeExprCallDispatch(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        call_dispatch: CallDispatch,
    ) Allocator.Error!void {
        _ = try self.ensureProgramExpr(result, source_context, module_idx, expr_idx);
        const semantics = result.lambdamono.getExprSemanticsPtr(source_context, module_idx, expr_idx).?;
        if (semantics.call_dispatch == null or !std.meta.eql(semantics.call_dispatch.?, call_dispatch)) {
            semantics.call_dispatch = call_dispatch;
            self.noteMutation(.program_calls);
        }
    }

    fn readExprCallDispatch(
        _: *const Pass,
        result: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?CallDispatch {
        const semantics = result.lambdamono.getExprSemantics(source_context, module_idx, expr_idx) orelse return null;
        return semantics.call_dispatch;
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
            try self.recordPatternSourceExpr(result, module_idx, def.pattern, .{
                .module_idx = module_idx,
                .expr_idx = def.expr,
            });

            _ = try self.registerCallableBackedDefTemplate(
                result,
                module_idx,
                def.expr,
                ModuleEnv.varFrom(def.pattern),
                def.pattern,
                packLocalPatternSourceKey(module_idx, def.pattern),
            );
        }
    }

    fn registerCallableTemplate(
        self: *Pass,
        result: *Result,
        source_key: u64,
        module_idx: u32,
        cir_expr: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        kind: CallableTemplateKind,
        source_region: Region,
    ) Allocator.Error!CallableTemplateId {
        if (result.lambdasolved.callable_template_ids_by_source.get(source_key)) |existing| return existing;

        const lexical_owner_template: ?CallableTemplateId = if (kind == .closure)
            self.currentTemplateContext(result)
        else
            null;

        const callable_template_id: CallableTemplateId = @enumFromInt(result.lambdasolved.callable_templates.items.len);
        try self.appendTracked(.callable_templates, &result.lambdasolved.callable_templates, CallableTemplate{
            .source_key = source_key,
            .module_idx = module_idx,
            .cir_expr = cir_expr,
            .runtime_expr = cir_expr,
            .type_root = type_root,
            .binding_pattern = binding_pattern,
            .kind = kind,
            .lexical_owner_template = lexical_owner_template,
            .external_def = externalDefSourceFromSourceKey(source_key),
            .source_region = source_region,
        });
        try self.putTracked(.callable_template_ids_by_source, &result.lambdasolved.callable_template_ids_by_source, source_key, callable_template_id);

        return callable_template_id;
    }

    fn recordCallableTemplateRuntimeExpr(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        runtime_expr_idx: CIR.Expr.Idx,
    ) void {
        const template = &result.lambdasolved.callable_templates.items[@intFromEnum(template_id)];
        if (template.runtime_expr == runtime_expr_idx) return;
        template.runtime_expr = runtime_expr_idx;
        self.noteMutation(.callable_templates);
    }

    fn aliasCallableTemplateSource(
        self: *Pass,
        result: *Result,
        source_key: u64,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        if (result.lambdasolved.callable_template_ids_by_source.get(source_key)) |existing| {
            if (existing != template_id) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: conflicting callable template aliases for source key {d} (existing={d}, new={d})",
                        .{ source_key, @intFromEnum(existing), @intFromEnum(template_id) },
                    );
                }
                unreachable;
            }
            return;
        }

        if (externalDefSourceFromSourceKey(source_key)) |external_def| {
            const template = &result.lambdasolved.callable_templates.items[@intFromEnum(template_id)];
            if (template.external_def) |existing| {
                if (existing.module_idx != external_def.module_idx or existing.def_idx != external_def.def_idx) {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Pipeline: conflicting external-def aliases for callable template {d} (existing={d}:{d}, new={d}:{d})",
                            .{
                                @intFromEnum(template_id),
                                existing.module_idx,
                                @intFromEnum(existing.def_idx),
                                external_def.module_idx,
                                @intFromEnum(external_def.def_idx),
                            },
                        );
                    }
                    unreachable;
                }
            } else {
                template.external_def = external_def;
            }
        }

        try self.putTracked(.callable_template_ids_by_source, &result.lambdasolved.callable_template_ids_by_source, source_key, template_id);
    }

    fn recordSourceExpr(
        self: *Pass,
        result: *Result,
        source_key: u64,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        if (result.lambdasolved.source_exprs.get(source_key)) |existing| {
            if (existing.module_idx != module_idx or existing.expr_idx != expr_idx) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: conflicting source exprs for source key {d} (existing={d}:{d}, new={d}:{d})",
                        .{
                            source_key,
                            existing.module_idx,
                            @intFromEnum(existing.expr_idx),
                            module_idx,
                            @intFromEnum(expr_idx),
                        },
                    );
                }
                unreachable;
            }
            return;
        }

        try self.putTracked(.source_exprs, &result.lambdasolved.source_exprs, source_key, ExprSource{
            .module_idx = module_idx,
            .expr_idx = expr_idx,
        });
    }

    fn recordPatternSourceExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        source: ExprSource,
    ) Allocator.Error!void {
        try self.recordSourceExpr(
            result,
            packLocalPatternSourceKey(module_idx, pattern_idx),
            source.module_idx,
            source.expr_idx,
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
            .as => |as_pat| try self.recordPatternSourceExpr(result, module_idx, as_pat.pattern, source),
            .nominal => |nominal_pat| try self.recordPatternSourceExpr(result, module_idx, nominal_pat.backing_pattern, source),
            .nominal_external => |nominal_pat| try self.recordPatternSourceExpr(result, module_idx, nominal_pat.backing_pattern, source),
            .tuple => |tuple_pat| {
                for (module_env.store.slicePatterns(tuple_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                    const elem_source = try self.resolveTupleElemSourceExpr(
                        result,
                        source.module_idx,
                        source.expr_idx,
                        @intCast(elem_index),
                    ) orelse continue;
                    try self.recordPatternSourceExpr(result, module_idx, elem_pattern_idx, elem_source);
                }
            },
            .applied_tag => |tag_pat| {
                for (module_env.store.slicePatterns(tag_pat.args), 0..) |arg_pattern_idx, arg_index| {
                    const arg_source = try self.resolveTagPayloadSourceExpr(
                        result,
                        source.module_idx,
                        source.expr_idx,
                        module_idx,
                        tag_pat.name,
                        @intCast(arg_index),
                    ) orelse continue;
                    try self.recordPatternSourceExpr(result, module_idx, arg_pattern_idx, arg_source);
                }
            },
            .record_destructure => |record_pat| {
                for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    switch (destruct.kind) {
                        .Required, .SubPattern => |sub_pattern_idx| {
                            const field_source = try self.resolveRecordFieldSourceExpr(
                                result,
                                source.module_idx,
                                source.expr_idx,
                                module_idx,
                                destruct.label,
                            ) orelse continue;
                            try self.recordPatternSourceExpr(result, module_idx, sub_pattern_idx, field_source);
                        },
                        .Rest => {},
                    }
                }
            },
            .list => |list_pat| {
                for (module_env.store.slicePatterns(list_pat.patterns), 0..) |elem_pattern_idx, elem_index| {
                    const elem_source = try self.resolveListElemSourceExpr(
                        result,
                        source.module_idx,
                        source.expr_idx,
                        @intCast(elem_index),
                    ) orelse continue;
                    try self.recordPatternSourceExpr(result, module_idx, elem_pattern_idx, elem_source);
                }
            },
        }
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

    fn registerDeferredLocalCallable(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        if (callableKind(expr) == null) return;
        if (!module_env.types.needsInstantiation(ModuleEnv.varFrom(expr_idx))) return;

        const source_key = packLocalPatternSourceKey(module_idx, pattern_idx);
        if (result.lambdasolved.deferred_local_callables.contains(source_key)) return;

        try self.putTracked(.deferred_local_callables, &result.lambdasolved.deferred_local_callables, source_key, DeferredLocalCallable{
            .pattern_idx = pattern_idx,
            .cir_expr = expr_idx,
            .module_idx = module_idx,
            .source_key = source_key,
            .type_root = ModuleEnv.varFrom(pattern_idx),
        });
    }

    fn scanStmt(self: *Pass, result: *Result, module_idx: u32, stmt_idx: CIR.Statement.Idx) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const stmt = module_env.store.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                try self.recordPatternSourceExpr(result, module_idx, decl.pattern, .{
                    .module_idx = module_idx,
                    .expr_idx = decl.expr,
                });
                try self.propagatePatternDemandToExpr(result, module_idx, decl.pattern, decl.expr);
                if (try self.registerCallableBackedDefTemplate(
                    result,
                    module_idx,
                    decl.expr,
                    ModuleEnv.varFrom(decl.pattern),
                    decl.pattern,
                    packLocalPatternSourceKey(module_idx, decl.pattern),
                )) |_| {
                    try self.registerDeferredLocalCallable(result, module_idx, decl.pattern, decl.expr);
                    try self.scanCirValueExpr(result, module_idx, decl.expr);
                    try self.bindPatternCallableValueCallableInsts(result, module_idx, decl.pattern, decl.expr);
                } else {
                    try self.scanCirExpr(result, module_idx, decl.expr);
                    try self.bindPatternCallableValueCallableInsts(result, module_idx, decl.pattern, decl.expr);
                    const expr_mono = try self.resolveExprMonotypeResolved(result, module_idx, decl.expr);
                    if (!expr_mono.isNone()) {
                        try self.bindCurrentPatternFromResolvedMonotype(
                            result,
                            module_idx,
                            decl.pattern,
                            expr_mono,
                        );
                    }
                }
            },
            .s_var => |var_decl| {
                try self.recordPatternSourceExpr(result, module_idx, var_decl.pattern_idx, .{
                    .module_idx = module_idx,
                    .expr_idx = var_decl.expr,
                });
                try self.propagatePatternDemandToExpr(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                if (try self.registerCallableBackedDefTemplate(
                    result,
                    module_idx,
                    var_decl.expr,
                    ModuleEnv.varFrom(var_decl.pattern_idx),
                    var_decl.pattern_idx,
                    packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                )) |_| {
                    try self.registerDeferredLocalCallable(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    try self.scanCirValueExpr(result, module_idx, var_decl.expr);
                    try self.bindPatternCallableValueCallableInsts(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                } else {
                    try self.scanCirExpr(result, module_idx, var_decl.expr);
                    try self.bindPatternCallableValueCallableInsts(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    const expr_mono = try self.resolveExprMonotypeResolved(result, module_idx, var_decl.expr);
                    if (!expr_mono.isNone()) {
                        try self.bindCurrentPatternFromResolvedMonotype(
                            result,
                            module_idx,
                            var_decl.pattern_idx,
                            expr_mono,
                        );
                    }
                }
            },
            .s_reassign => |reassign| {
                try self.propagatePatternDemandToExpr(result, module_idx, reassign.pattern_idx, reassign.expr);
                try self.scanCirExpr(result, module_idx, reassign.expr);
                const expr_mono = try self.resolveExprMonotypeResolved(result, module_idx, reassign.expr);
                if (!expr_mono.isNone()) {
                    try self.bindCurrentPatternFromResolvedMonotype(
                        result,
                        module_idx,
                        reassign.pattern_idx,
                        expr_mono,
                    );
                }
            },
            .s_dbg => |dbg_stmt| try self.scanCirValueExpr(result, module_idx, dbg_stmt.expr),
            .s_expr => |expr_stmt| try self.scanCirValueExpr(result, module_idx, expr_stmt.expr),
            .s_expect => |expect_stmt| try self.scanCirValueExpr(result, module_idx, expect_stmt.body),
            .s_for => |for_stmt| {
                try self.scanCirExpr(result, module_idx, for_stmt.expr);
                const iter_mono = try self.resolveExprMonotypeResolved(result, module_idx, for_stmt.expr);
                if (!iter_mono.isNone()) {
                    const item_mono = switch (result.context_mono.monotype_store.getMonotype(iter_mono.idx)) {
                        .list => |list| resolvedMonotype(list.elem, iter_mono.module_idx),
                        else => std.debug.panic(
                            "Pipeline invariant violated: for-loop expr {d} in module {d} had non-list exact monotype",
                            .{ @intFromEnum(for_stmt.expr), module_idx },
                        ),
                    };
                    try self.bindCurrentPatternFromResolvedMonotype(result, module_idx, for_stmt.patt, item_mono);
                }
                try self.scanCirExpr(result, module_idx, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.scanCirExpr(result, module_idx, while_stmt.cond);
                try self.scanCirExpr(result, module_idx, while_stmt.body);
            },
            .s_return => |return_stmt| try self.scanCirValueExpr(result, module_idx, return_stmt.expr),
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
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const pattern_mono = try self.resolveTypeVarMonotypeResolved(
            result,
            module_idx,
            ModuleEnv.varFrom(pattern_idx),
        );
        if (pattern_mono.isNone()) return;

        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        try self.propagateDemandedValueMonotypeToValueExpr(
            result,
            module_idx,
            expr_idx,
            pattern_mono.idx,
            pattern_mono.module_idx,
            &visiting,
        );
    }

    fn bindPatternCallableValueCallableInsts(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        var callable_inst_ids = std.ArrayList(CallableInstId).empty;
        defer callable_inst_ids.deinit(self.allocator);

        try self.collectProgramExprCallableMembers(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            &visiting,
            &callable_inst_ids,
        );

        if (callable_inst_ids.items.len == 0) return;
        if (callable_inst_ids.items.len == 1) {
            const callable_inst_id = callable_inst_ids.items[0];
            const callable_inst = result.getCallableInst(callable_inst_id);
            try self.installCallableParamDirectValue(
                result,
                self.currentSourceContext(),
                module_idx,
                pattern_idx,
                callable_inst_id,
            );
            try self.installExprDirectCallable(
                result,
                self.currentSourceContext(),
                module_idx,
                expr_idx,
                callable_inst_id,
            );
            try self.bindCurrentPatternFromResolvedMonotype(
                result,
                module_idx,
                pattern_idx,
                resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
            );
            try self.recordCurrentExprMonotype(
                result,
                module_idx,
                expr_idx,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
            );
            return;
        }
        try self.installExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            callable_inst_ids.items,
        );
        try self.installCallableParamValue(
            result,
            self.currentSourceContext(),
            module_idx,
            pattern_idx,
            callable_inst_ids.items,
        );
    }

    fn scanCirExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanCirExprInternal(result, module_idx, expr_idx, false, false);
    }

    fn scanCirValueExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanCirExprInternal(result, module_idx, expr_idx, true, false);
    }

    fn scanForcedCirValueExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanCirExprInternal(result, module_idx, expr_idx, true, true);
    }

    fn scanDemandedValueDefExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const key = self.currentResultExprKey(module_idx, expr_idx);
        if (self.in_progress_value_defs.contains(key)) return;
        try self.in_progress_value_defs.put(self.allocator, key, {});
        defer _ = self.in_progress_value_defs.remove(key);

        if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
            try self.materializeDemandedExprCallableInst(result, module_idx, expr_idx, template_id);
            if (self.getExprCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callable_inst_ids| {
                for (callable_inst_ids) |callable_inst_id| {
                    try self.scanCallableInst(result, callable_inst_id);
                }
                return;
            }
            if (self.getExprCallableInstInCurrentContext(result, module_idx, expr_idx)) |callable_inst_id| {
                try self.scanCallableInst(result, callable_inst_id);
            }
            return;
        }

        try self.scanForcedCirValueExpr(result, module_idx, expr_idx);
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        materialize_if_callable: bool,
        force_rescan_children: bool,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        const monotype = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
        if (!monotype.isNone()) {
            try self.recordCurrentExprMonotype(result, module_idx, expr_idx, monotype.idx, monotype.module_idx);
        }

        if (callableKind(expr)) |kind| {
            const template_id = try self.registerCallableTemplate(
                result,
                packExprSourceKey(module_idx, expr_idx),
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(expr_idx),
                null,
                kind,
                module_env.store.getExprRegion(expr_idx),
            );
            if (materialize_if_callable) {
                try self.materializeDemandedExprCallableInst(result, module_idx, expr_idx, template_id);
                if (self.active_bindings == null and !sourceContextHasCallableInst(self.currentSourceContext())) {
                    try self.completeCurrentExprMonotype(result, module_idx, expr_idx);
                    return;
                }
            }
        }

        if (self.active_bindings != null or force_rescan_children) {
            try self.scanCirExprChildren(result, module_idx, expr_idx, expr);
            try self.completeCurrentExprMonotype(result, module_idx, expr_idx);
            return;
        }

        const visit_key = self.currentResultExprKey(module_idx, expr_idx);
        if (self.visited_exprs.contains(visit_key)) return;
        try self.visited_exprs.put(self.allocator, visit_key, {});

        try self.scanCirExprChildren(result, module_idx, expr_idx, expr);

        if (materialize_if_callable and callableKind(expr) == null) {
            if (self.getValueExprCallableInstInCurrentContext(result, module_idx, expr_idx) == null and
                self.getValueExprCallableInstsInCurrentContext(result, module_idx, expr_idx) == null)
            {
                var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                defer visiting.deinit(self.allocator);
                var callable_inst_ids = std.ArrayList(CallableInstId).empty;
                defer callable_inst_ids.deinit(self.allocator);
                try self.collectProgramExprCallableMembers(
                    result,
                    self.currentSourceContext(),
                    module_idx,
                    expr_idx,
                    &visiting,
                    &callable_inst_ids,
                );
                if (callable_inst_ids.items.len != 0) {
                    try self.installDemandedExprCallableValue(
                        result,
                        module_idx,
                        expr_idx,
                        callable_inst_ids.items,
                    );
                    switch (expr) {
                        .e_lookup_local => |lookup| {
                            const callable_inst_ids = self.getValueExprCallableInstsInCurrentContext(result, module_idx, expr_idx) orelse
                                if (self.getValueExprCallableInstInCurrentContext(result, module_idx, expr_idx)) |direct_callable_inst|
                                    &.{direct_callable_inst}
                                else
                                    &.{};
                            if (callable_inst_ids.len != 0) {
                                try self.installCallableParamValue(
                                    result,
                                    self.currentSourceContext(),
                                    module_idx,
                                    lookup.pattern_idx,
                                    callable_inst_ids,
                                );
                            }
                        },
                        else => {},
                    }
                }
            }
        }

        try self.completeCurrentExprMonotype(result, module_idx, expr_idx);
    }

    fn completeCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const resolved = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
        if (resolved.isNone()) return;
        try self.recordCurrentExprMonotype(result, module_idx, expr_idx, resolved.idx, resolved.module_idx);
        if (result.lambdamono.getExprId(self.currentSourceContext(), module_idx, expr_idx)) |expr_id| {
            result.lambdamono.exprs.items[@intFromEnum(expr_id)].monotype = resolved;
        }
    }

    fn materializeDemandedExprCallableInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        if (templateRequiresConcreteOwnerCallableInst(result, template_id) and
            !sourceContextHasCallableInst(self.currentSourceContext()))
        {
            return;
        }

        const source_context = self.currentSourceContext();

        const fn_monotype = try self.resolveExprMonotypeResolved(
            result,
            module_idx,
            expr_idx,
        );
        if (fn_monotype.isNone()) return;

        const callable_inst_id = try self.ensureCallableInst(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
        try self.installExprDirectCallable(
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
        return template.kind == .closure and template.lexical_owner_template != null;
    }

    fn installExprDirectCallableInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.installExprDirectCallable(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
            callable_inst_id,
        );
    }

    fn installExprDirectCallable(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.installExprCallableValue(
            result,
            source_context,
            module_idx,
            expr_idx,
            &.{callable_inst_id},
        );
    }

    fn installExprCallableValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        return self.installExprCallableValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            expr_idx,
            callable_inst_ids,
        );
    }

    fn installExprCallableValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        if (callable_inst_ids.len == 0) unreachable;

        var merged = std.ArrayList(CallableInstId).empty;
        defer merged.deinit(self.allocator);
        if (self.readExprCallableValue(result, source_context, module_idx, expr_idx)) |existing_callable_value| {
            try self.existingCallableValueMembers(result, existing_callable_value, &merged);
        }
        try self.appendUniqueCallableInsts(&merged, callable_inst_ids);
        std.mem.sortUnstable(CallableInstId, merged.items, {}, struct {
            fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                return @intFromEnum(lhs) < @intFromEnum(rhs);
            }
        }.lessThan);
        try self.ensureRecordedCallableInstsScanned(result, callable_inst_ids);
        if (merged.items.len == 1) {
            try self.writeExprCallableValue(result, source_context, module_idx, expr_idx, .{ .direct = merged.items[0] });
            const callable_inst = result.getCallableInst(merged.items[0]).*;
            try self.recordExprMonotypeForSourceContext(
                result,
                source_context,
                module_idx,
                expr_idx,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
            );
        } else {
            const callable_member_set_id = try self.internCallableMemberSet(result, merged.items);
            const packed_callable_id = try self.ensurePackedCallable(result, callable_member_set_id);
            try self.writeExprCallableValue(result, source_context, module_idx, expr_idx, .{ .packed_callable = packed_callable_id });
            const packed_callable = result.getPackedCallable(packed_callable_id);
            try self.recordExprMonotypeForSourceContext(
                result,
                source_context,
                module_idx,
                expr_idx,
                packed_callable.fn_monotype.idx,
                packed_callable.fn_monotype.module_idx,
            );
        }
    }

    fn lookupCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.lookupExprMonotypeForSourceContext(result, self.currentSourceContext(), module_idx, expr_idx);
    }

    fn lookupExprMonotypeForSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        const key = self.resultExprKeyForSourceContext(source_context, module_idx, expr_idx);
        if (self.active_iteration_expr_monotypes) |iteration_map| {
            if (iteration_map.get(key)) |resolved| return resolved;
            if (sourceContextHasCallableInst(source_context) and
                key.source_context_kind == .callable_inst and
                key.source_context_raw == switch (source_context) {
                    .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
                    .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
                })
            {
                return null;
            }
        }
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

    fn captureFieldExactCallableInst(field: CaptureField) ?CallableInstId {
        return field.exact_callable_inst;
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
            const capture_callable_inst_id = captureFieldExactCallableInst(capture_field) orelse continue;
            if (try self.callableInstCaptureGraphReaches(
                result,
                capture_callable_inst_id,
                target_callable_inst_id,
                visited,
            )) {
                return true;
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

    fn solvedCallableKindForRuntimeValue(
        template_kind: CallableTemplateKind,
        runtime_value: Lambdamono.RuntimeValue,
    ) Lambdasolved.SolvedCallableKind {
        if (template_kind == .hosted_lambda) return .hosted;
        return switch (runtime_value) {
            .direct_lambda => .direct,
            .closure => .closure,
        };
    }

    fn templateSourceContext(template: CallableTemplate) SourceContext {
        return .{ .template_expr = .{
            .module_idx = template.module_idx,
            .expr_idx = template.runtime_expr,
        } };
    }

    fn ensureCapturePlan(
        self: *Pass,
        result: *Result,
        entries: []const Lambdasolved.CaptureEntry,
    ) Allocator.Error!Lambdasolved.CapturePlanId {
        if (entries.len == 0) return result.lambdasolved.getEmptyCapturePlanId();

        for (result.lambdasolved.capture_plans.items, 0..) |existing_plan, idx| {
            const existing_entries = result.lambdasolved.getCaptureEntries(existing_plan.entries);
            if (existing_entries.len != entries.len) continue;
            if (std.meta.eql(existing_entries, entries)) return @enumFromInt(idx);
        }

        const start: u32 = @intCast(result.lambdasolved.capture_entries.items.len);
        try result.lambdasolved.capture_entries.appendSlice(self.allocator, entries);
        const plan_id: Lambdasolved.CapturePlanId = @enumFromInt(result.lambdasolved.capture_plans.items.len);
        try result.lambdasolved.capture_plans.append(self.allocator, .{
            .entries = .{ .start = start, .len = @intCast(entries.len) },
        });
        return plan_id;
    }

    fn ensureCallableSourceMember(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        template_id: CallableTemplateId,
        fn_monotype: ResolvedMonotype,
        capture_plan: Lambdasolved.CapturePlanId,
        kind: Lambdasolved.SolvedCallableKind,
    ) Allocator.Error!Lambdasolved.LambdaSetMemberId {
        for (result.lambdasolved.lambda_set_members.items, 0..) |existing, idx| {
            if (existing.template != template_id) continue;
            if (!std.meta.eql(existing.source_context, source_context)) continue;
            if (existing.capture_plan != capture_plan) continue;
            if (existing.kind != kind) continue;
            if (existing.fn_monotype.module_idx != fn_monotype.module_idx) continue;
            if (!try self.monotypesStructurallyEqual(
                result,
                existing.fn_monotype.idx,
                fn_monotype.idx,
            )) continue;
            return @enumFromInt(idx);
        }

        const member_id: Lambdasolved.LambdaSetMemberId = @enumFromInt(result.lambdasolved.lambda_set_members.items.len);
        try result.lambdasolved.lambda_set_members.append(self.allocator, .{
            .template = template_id,
            .source_context = source_context,
            .fn_monotype = fn_monotype,
            .capture_plan = capture_plan,
            .kind = kind,
        });
        return member_id;
    }

    fn captureSourceForField(
        self: *Pass,
        result: *const Result,
        capture_field: CaptureField,
    ) Lambdasolved.CaptureSource {
        _ = self;
        return switch (capture_field.storage) {
            .recursive_member => .{ .exact_callable = .{
                .member = result.getCallableDefForInst(capture_field.exact_callable_inst.?).source_member,
            } },
            .runtime_field, .callable_only => switch (capture_field.source) {
                .lexical_pattern => |lexical| .{ .lexical_pattern = .{
                    .module_idx = lexical.module_idx,
                    .pattern_idx = lexical.pattern_idx,
                } },
                .expr => |expr_id| .{ .source_expr = .{
                    .module_idx = result.getProgramExpr(expr_id).module_idx,
                    .expr_idx = result.getProgramExpr(expr_id).source_expr,
                } },
            },
        };
    }

    fn updateCallableSourceMember(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        capture_fields: []const CaptureField,
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id).*;
        const template = result.getCallableTemplate(callable_inst.template).*;
        const callable_def = &result.lambdamono.callable_defs.items[@intFromEnum(callable_inst.callable_def)];
        const member_source_context = callable_inst.defining_source_context;

        var capture_entries = std.ArrayList(Lambdasolved.CaptureEntry).empty;
        defer capture_entries.deinit(self.allocator);
        for (capture_fields) |capture_field| {
            try capture_entries.append(self.allocator, .{
                .source = self.captureSourceForField(result, capture_field),
                .monotype = capture_field.local_monotype,
            });
        }

        if (result.lambdamono.callable_inst_ids_by_source_member.get(callable_def.source_member)) |mapped| {
            if (mapped == callable_inst_id) {
                _ = result.lambdamono.callable_inst_ids_by_source_member.remove(callable_def.source_member);
            }
        }

        callable_def.source_member = try self.ensureCallableSourceMember(
            result,
            member_source_context,
            callable_inst.template,
            resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
            try self.ensureCapturePlan(result, capture_entries.items),
            solvedCallableKindForRuntimeValue(template.kind, callable_inst.runtime_value),
        );
        try result.lambdamono.callable_inst_ids_by_source_member.put(
            self.allocator,
            callable_def.source_member,
            callable_inst_id,
        );
    }

    fn updateCallableDefRuntimeValue(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        runtime_expr_id: Lambdamono.ExprId,
        body_expr_id: Lambdamono.ExprId,
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

        callable_def.runtime_expr = runtime_expr_id;
        callable_def.body_expr = body_expr_id;

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

        try self.updateCallableSourceMember(result, callable_inst_id, capture_fields);
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
        if (result.getPatternSourceExpr(module_idx, pattern_idx)) |source| {
            return .{ .expr = try self.ensureProgramExpr(
                result,
                source_context,
                source.module_idx,
                source.expr_idx,
            ) };
        }
        if (findDefByPattern(module_env, pattern_idx) != null) {
            return null;
        }
        return .{ .lexical_pattern = .{
            .module_idx = module_idx,
            .pattern_idx = pattern_idx,
        } };
    }

    fn getExactCallableInstForCaptureValueSource(
        self: *Pass,
        result: *const Result,
        capture_context_callable_inst: CallableInstId,
        capture_value_source: CaptureValueSource,
    ) ?CallableInstId {
        switch (capture_value_source) {
            .expr => |capture_expr_id| {
                const capture_expr = result.getProgramExpr(capture_expr_id);
                return self.getExprCallableInstInContext(
                    result,
                    capture_context_callable_inst,
                    capture_expr.module_idx,
                    capture_expr.source_expr,
                );
            },
            .lexical_pattern => |lexical| {
                return self.resolveBoundPatternCallableInstInContext(
                    result,
                    capture_context_callable_inst,
                    lexical.module_idx,
                    lexical.pattern_idx,
                );
            },
        }
    }

    fn resolveCaptureValueSourceMonotype(
        self: *Pass,
        result: *Result,
        capture_context_callable_inst: CallableInstId,
        capture_value_source: CaptureValueSource,
    ) Allocator.Error!ResolvedMonotype {
        return switch (capture_value_source) {
            .expr => |capture_expr_id| result.getProgramExpr(capture_expr_id).monotype,
            .lexical_pattern => |lexical| self.resolvePatternMonotypeInSourceContext(
                result,
                callableInstSourceContext(capture_context_callable_inst),
                lexical.module_idx,
                lexical.pattern_idx,
            ),
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

            const exact_capture_callable_inst = if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func)
                self.getExactCallableInstForCaptureValueSource(
                    result,
                    capture_context_callable_inst,
                    capture_value_source,
                ) orelse std.debug.panic(
                    "Pipeline invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no exact callable fact in specialization tables",
                    .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
                )
            else
                null;

            const capture_storage: Lambdamono.CaptureStorage = blk: {
                if (exact_capture_callable_inst) |capture_callable_inst_id| {
                    const capture_template = result.getCallableTemplate(
                        result.getCallableInst(capture_callable_inst_id).template,
                    );
                    if (capture_template.binding_pattern) |binding_pattern| {
                        if (binding_pattern == capture.pattern_idx and
                            try self.callableInstSharesRecursiveEnvironment(
                                result,
                                closure_callable_inst_id,
                                capture_callable_inst_id,
                            ))
                        {
                            break :blk .recursive_member;
                        }
                    }
                }

                if (result.context_mono.monotype_store.getMonotype(capture_mono.idx) == .func) {
                    const capture_callable_inst_id = exact_capture_callable_inst orelse std.debug.panic(
                        "Pipeline invariant violated: function-valued closure capture pattern {d} for callable inst {d} had no exact callable inst",
                        .{ @intFromEnum(capture.pattern_idx), @intFromEnum(closure_callable_inst_id) },
                    );
                    switch (result.getCallableInst(capture_callable_inst_id).runtime_value) {
                        .direct_lambda => break :blk .callable_only,
                        .closure => |closure_value| break :blk .{ .runtime_field = .{
                            .field_monotype = closure_value.capture_tuple_monotype,
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
                .exact_callable_inst = exact_capture_callable_inst,
                .source = capture_value_source,
                .storage = capture_storage,
            });
        }

        try self.updateCallableDefRuntimeValue(
            result,
            closure_callable_inst_id,
            try self.ensureProgramExpr(
                result,
                callableInstSourceContext(closure_callable_inst_id),
                module_idx,
                closure_expr_idx,
            ),
            try self.ensureProgramExpr(
                result,
                callableInstSourceContext(closure_callable_inst_id),
                module_idx,
                switch (module_env.store.getExpr(closure_expr.lambda_idx)) {
                    .e_lambda => |lambda_expr| lambda_expr.body,
                    else => closure_expr.lambda_idx,
                },
            ),
            capture_fields.items,
        );
    }

    fn scanCirExprChildren(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) Allocator.Error!void {
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
                if (self.resolveBoundPatternCallableInstInCurrentContext(result, module_idx, lookup.pattern_idx)) |callable_inst_id| {
                    try self.installExprDirectCallable(
                        result,
                        self.currentSourceContext(),
                        module_idx,
                        expr_idx,
                        callable_inst_id,
                    );
                }
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, module_idx, expr_idx, template_id);
                } else if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    if (self.resolveBoundPatternCallableMembersInCurrentContext(result, module_idx, lookup.pattern_idx)) |callable_inst_ids| {
                        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                        defer visiting.deinit(self.allocator);
                        try self.propagateDemandedCallableMembersToValueExpr(
                            result,
                            source.module_idx,
                            source.expr_idx,
                            callable_inst_ids,
                            &visiting,
                        );
                    }
                    try self.scanDemandedValueDefExpr(result, source.module_idx, source.expr_idx);
                    try self.propagateLookupSourceExprCallableSemantics(
                        result,
                        module_idx,
                        expr_idx,
                        lookup.pattern_idx,
                        source.module_idx,
                        source.expr_idx,
                    );
                }
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                try self.scanModule(result, target_module_idx);
                const target_env = self.all_module_envs[target_module_idx];
                if (!target_env.store.isDefNode(lookup.target_node_idx)) return;

                const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                try self.recordSourceExpr(
                    result,
                    packExternalDefSourceKey(target_module_idx, lookup.target_node_idx),
                    target_module_idx,
                    def.expr,
                );
                _ = try self.registerCallableBackedDefTemplate(
                    result,
                    target_module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target_module_idx, lookup.target_node_idx),
                );
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExpr(result, target_module_idx, def.expr);
                    try self.propagateLookupSourceExprCallableSemantics(
                        result,
                        module_idx,
                        expr_idx,
                        null,
                        target_module_idx,
                        def.expr,
                    );
                }
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                try self.scanModule(result, target.module_idx);
                const target_env = self.all_module_envs[target.module_idx];
                const def = target_env.store.getDef(target.def_idx);
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                try self.recordSourceExpr(
                    result,
                    packExternalDefSourceKey(target.module_idx, target_node_idx),
                    target.module_idx,
                    def.expr,
                );
                _ = try self.registerCallableBackedDefTemplate(
                    result,
                    target.module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target.module_idx, target_node_idx),
                );
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.materializeLookupExprCallableValue(result, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExpr(result, target.module_idx, def.expr);
                    try self.propagateLookupSourceExprCallableSemantics(
                        result,
                        module_idx,
                        expr_idx,
                        null,
                        target.module_idx,
                        def.expr,
                    );
                }
            },
            .e_str => |str_expr| try self.scanCirValueExprSpan(result, module_idx, module_env.store.sliceExpr(str_expr.span)),
            .e_list => |list_expr| try self.scanCirValueExprSpan(result, module_idx, module_env.store.sliceExpr(list_expr.elems)),
            .e_tuple => |tuple_expr| {
                try self.recordDemandedTupleElemMonotypes(result, module_idx, expr_idx, tuple_expr);
                try self.scanCirValueExprSpan(result, module_idx, module_env.store.sliceExpr(tuple_expr.elems));
            },
            .e_match => |match_expr| {
                try self.scanCirExpr(result, module_idx, match_expr.cond);
                const cond_mono = try self.resolveExprMonotypeResolved(result, module_idx, match_expr.cond);
                if (cond_mono.isNone()) return;

                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                        const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                        try self.recordPatternSourceExpr(result, module_idx, branch_pattern.pattern, .{
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
                    try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, branch.value);
                    try self.scanCirValueExpr(result, module_idx, branch.value);
                    if (branch.guard) |guard_expr| {
                        try self.scanCirExpr(result, module_idx, guard_expr);
                    }
                }
            },
            .e_if => |if_expr| {
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.scanCirExpr(result, module_idx, branch.cond);
                    try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, branch.body);
                    try self.scanCirValueExpr(result, module_idx, branch.body);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, if_expr.final_else);
                try self.scanCirValueExpr(result, module_idx, if_expr.final_else);
            },
            .e_call => |call_expr| {
                const arg_exprs = module_env.store.sliceExpr(call_expr.args);
                if (self.getCallLowLevelOp(module_env, call_expr.func)) |low_level_op| {
                    if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                        try self.resolveStrInspectHelperCallableInstsForTypeVar(
                            result,
                            module_idx,
                            ModuleEnv.varFrom(arg_exprs[0]),
                        );
                    }
                }
                try self.scanCirExprSpan(result, module_idx, arg_exprs);
                if (!self.suppress_direct_call_resolution) {
                    try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr);
                    if (self.getCallSiteCallableInstInCurrentContext(result, module_idx, expr_idx) == null and
                        self.getCallSiteCallableInstsInCurrentContext(result, module_idx, expr_idx) == null)
                    {
                        try self.scanCirExpr(result, module_idx, call_expr.func);
                        try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr);
                    }
                    try self.assignCallableArgCallableInstsFromCallMonotype(result, module_idx, expr_idx, call_expr);
                } else {
                    try self.scanCirExpr(result, module_idx, call_expr.func);
                }
                try self.scanCirExprSpan(result, module_idx, arg_exprs);
                if (self.active_bindings != null) {
                    if (self.getCallSiteCallableInstInCurrentContext(result, module_idx, expr_idx) == null and
                        self.getCallSiteCallableInstsInCurrentContext(result, module_idx, expr_idx) == null)
                    {
                        try self.scanCirExpr(result, module_idx, call_expr.func);
                    }
                    try self.scanCirExprSpan(result, module_idx, arg_exprs);
                }
                if (!self.suppress_direct_call_resolution) {
                    try self.assignCallableArgCallableInstsFromCallMonotype(result, module_idx, expr_idx, call_expr);
                }
                if (!self.suppress_direct_call_resolution and
                    self.getCallSiteCallableInstInCurrentContext(result, module_idx, expr_idx) == null)
                {
                    try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr);
                }
            },
            .e_record => |record_expr| {
                if (record_expr.ext) |ext_expr| {
                    try self.scanCirExpr(result, module_idx, ext_expr);
                }

                try self.recordDemandedRecordFieldMonotypes(result, module_idx, expr_idx, record_expr);

                const fields = module_env.store.sliceRecordFields(record_expr.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.scanCirValueExpr(result, module_idx, field.value);
                }
            },
            .e_block => |block_expr| {
                const stmts = module_env.store.sliceStatements(block_expr.stmts);
                try self.preRegisterBlockCallableStmtTemplates(result, module_idx, stmts);
                for (stmts) |stmt_idx| {
                    try self.scanStmt(result, module_idx, stmt_idx);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, block_expr.final_expr);
                try self.scanCirValueExpr(result, module_idx, block_expr.final_expr);
            },
            .e_tag => |tag_expr| try self.scanCirValueExprSpan(result, module_idx, module_env.store.sliceExpr(tag_expr.args)),
            .e_nominal => |nominal_expr| try self.scanCirValueExpr(result, module_idx, nominal_expr.backing_expr),
            .e_nominal_external => |nominal_expr| try self.scanCirValueExpr(result, module_idx, nominal_expr.backing_expr),
            .e_closure => |closure_expr| {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    const lambda_template_id = try self.registerCallableTemplate(
                        result,
                        packExprSourceKey(module_idx, closure_expr.lambda_idx),
                        module_idx,
                        closure_expr.lambda_idx,
                        ModuleEnv.varFrom(closure_expr.lambda_idx),
                        null,
                        .lambda,
                        module_env.store.getExprRegion(closure_expr.lambda_idx),
                    );
                    self.recordCallableTemplateRuntimeExpr(result, lambda_template_id, expr_idx);
                }
                // Callable bodies are scanned exclusively from `scanCallableInst`, after
                // the callable template/inst has established its own lexical owner and
                // defining-context chain. Generic expr traversal only records the
                // closure value's capture sources in the surrounding callable.
                try self.scanClosureCaptureSources(result, self.currentSourceContext(), module_idx, expr_idx, closure_expr);
            },
            .e_lambda => {},
            .e_binop => |binop_expr| {
                try self.scanCirExpr(result, module_idx, binop_expr.lhs);
                try self.scanCirExpr(result, module_idx, binop_expr.rhs);
                try self.resolveDispatchExprCallableInst(result, module_idx, expr_idx, expr);
                if (self.active_bindings != null) {
                    try self.scanCirExpr(result, module_idx, binop_expr.lhs);
                    try self.scanCirExpr(result, module_idx, binop_expr.rhs);
                }
            },
            .e_unary_minus => |unary_expr| {
                try self.scanCirExpr(result, module_idx, unary_expr.expr);
                try self.resolveDispatchExprCallableInst(result, module_idx, expr_idx, expr);
                if (self.active_bindings != null) {
                    try self.scanCirExpr(result, module_idx, unary_expr.expr);
                }
            },
            .e_unary_not => |unary_expr| try self.scanCirExpr(result, module_idx, unary_expr.expr),
            .e_dot_access => |dot_expr| {
                try self.scanCirExpr(result, module_idx, dot_expr.receiver);
                if (dot_expr.args) |args| {
                    try self.scanCirExprSpan(result, module_idx, module_env.store.sliceExpr(args));
                    try self.resolveDispatchExprCallableInst(result, module_idx, expr_idx, expr);
                    if (self.active_bindings != null) {
                        try self.scanCirExpr(result, module_idx, dot_expr.receiver);
                        try self.scanCirExprSpan(result, module_idx, module_env.store.sliceExpr(args));
                    }
                }
            },
            .e_tuple_access => |tuple_access| try self.scanCirExpr(result, module_idx, tuple_access.tuple),
            .e_dbg => |dbg_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, dbg_expr.expr);
                try self.scanCirValueExpr(result, module_idx, dbg_expr.expr);
            },
            .e_expect => |expect_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, expect_expr.body);
                try self.scanCirValueExpr(result, module_idx, expect_expr.body);
            },
            .e_return => |return_expr| try self.scanCirValueExpr(result, module_idx, return_expr.expr),
            .e_type_var_dispatch => |dispatch_expr| {
                try self.scanCirExprSpan(result, module_idx, module_env.store.sliceExpr(dispatch_expr.args));
                try self.resolveDispatchExprCallableInst(result, module_idx, expr_idx, expr);
                if (self.active_bindings != null) {
                    try self.scanCirExprSpan(result, module_idx, module_env.store.sliceExpr(dispatch_expr.args));
                }
            },
            .e_for => |for_expr| {
                try self.scanCirExpr(result, module_idx, for_expr.expr);
                try self.scanCirExpr(result, module_idx, for_expr.body);
            },
            .e_hosted_lambda => {},
            .e_run_low_level => |run_low_level| {
                const args = module_env.store.sliceExpr(run_low_level.args);
                try self.scanCirExprSpan(result, module_idx, args);
                if (run_low_level.op == .str_inspect and args.len != 0) {
                    try self.resolveStrInspectHelperCallableInstsForTypeVar(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(args[0]),
                    );
                }
            },
        }
    }

    fn propagateLookupSourceExprCallableSemantics(
        self: *Pass,
        result: *Result,
        lookup_module_idx: u32,
        lookup_expr_idx: CIR.Expr.Idx,
        maybe_pattern_idx: ?CIR.Pattern.Idx,
        source_module_idx: u32,
        source_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        var callable_inst_ids = std.ArrayList(CallableInstId).empty;
        defer callable_inst_ids.deinit(self.allocator);

        try self.collectProgramExprCallableMembers(
            result,
            self.currentSourceContext(),
            source_module_idx,
            source_expr_idx,
            &visiting,
            &callable_inst_ids,
        );

        if (callable_inst_ids.items.len == 0) return;
        try self.installDemandedExprCallableValue(
            result,
            lookup_module_idx,
            lookup_expr_idx,
            callable_inst_ids.items,
        );
        if (maybe_pattern_idx) |pattern_idx| {
            try self.installCallableParamValue(
                result,
                self.currentSourceContext(),
                lookup_module_idx,
                pattern_idx,
                callable_inst_ids.items,
            );
        }
    }

    fn scanCirExprSpan(self: *Pass, result: *Result, module_idx: u32, exprs: []const CIR.Expr.Idx) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirExpr(result, module_idx, child_expr);
        }
    }

    fn scanCirValueExprSpan(self: *Pass, result: *Result, module_idx: u32, exprs: []const CIR.Expr.Idx) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanCirValueExpr(result, module_idx, child_expr);
        }
    }

    fn seedBindingsForCallableInst(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const callable_inst = result.getCallableInst(callable_inst_id).*;
        const template = result.getCallableTemplate(callable_inst.template).*;
        const subst = result.getTypeSubst(callable_inst.subst);
        for (result.getTypeSubstEntries(subst.entries)) |entry| {
            try bindings.put(entry.key, entry.monotype);
        }

        try self.seedCallableBodyBindingsFromSignature(
            result,
            template.module_idx,
            template.cir_expr,
            callableInstSourceContext(callable_inst_id),
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
            bindings,
        );
    }

    fn resolvePatternMonotypeInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) Allocator.Error!ResolvedMonotype {
        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();
        const saved_bindings = self.active_bindings;
        defer self.active_bindings = saved_bindings;
        switch (source_context) {
            .callable_inst => |context_id| {
                try self.seedBindingsForCallableInst(result, @enumFromInt(@intFromEnum(context_id)), &bindings);
                self.active_bindings = &bindings;
            },
            .root_expr, .provenance_expr, .template_expr => {
                self.active_bindings = null;
            },
        }

        try self.pushSourceContext(source_context);
        defer self.popSourceContext();

        return self.resolveTypeVarMonotypeResolved(
            result,
            module_idx,
            ModuleEnv.varFrom(pattern_idx),
        );
    }

    fn resolveExprMonotypeInSourceContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();
        const saved_bindings = self.active_bindings;
        defer self.active_bindings = saved_bindings;
        switch (source_context) {
            .callable_inst => |context_id| {
                try self.seedBindingsForCallableInst(result, @enumFromInt(@intFromEnum(context_id)), &bindings);
                self.active_bindings = &bindings;
            },
            .root_expr, .provenance_expr, .template_expr => {
                self.active_bindings = null;
            },
        }

        try self.pushSourceContext(source_context);
        defer self.popSourceContext();

        return self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
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
        try self.pushSourceContext(source_context);
        defer self.popSourceContext();

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
                    template.binding_pattern != null and
                    template.binding_pattern.? == capture.pattern_idx)
                {
                    continue;
                }
            }

            const source = result.getPatternSourceExpr(module_idx, capture.pattern_idx) orelse continue;

            try self.scanDemandedValueDefExpr(result, source.module_idx, source.expr_idx);
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
                    )) != null) {
                        try self.registerDeferredLocalCallable(result, module_idx, decl.pattern, decl.expr);
                    }
                },
                .s_var => |var_decl| {
                    if ((try self.registerCallableBackedDefTemplate(
                        result,
                        module_idx,
                        var_decl.expr,
                        ModuleEnv.varFrom(var_decl.pattern_idx),
                        var_decl.pattern_idx,
                        packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                    )) != null) {
                        try self.registerDeferredLocalCallable(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    }
                },
                else => {},
            }
        }
    }

    const DirectCallCallableInstResolution = union(enum) {
        none,
        blocked,
        resolved: CallableInstId,
    };

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
            result.getCallableParamSpecEntries(callable_inst.callable_param_specs),
            required_callable_param_specs,
        );
    }

    fn resolveDirectCallSite(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!void {
        const callee_expr_idx = call_expr.func;
        const module_env = self.all_module_envs[module_idx];
        const callee_expr = module_env.store.getExpr(callee_expr_idx);
        const desired_fn_monotype = resolvedMonotype(
            try self.resolveDirectCallFnMonotype(result, module_idx, call_expr_idx, call_expr),
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
        const resolved_template_id = try self.lookupDirectCalleeTemplate(result, module_idx, callee_expr_idx);
        if (resolved_template_id == null and !desired_fn_monotype.isNone()) {
            var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.propagateDemandedCallableFnMonotypeToValueExpr(
                result,
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
                self.currentSourceContext(),
                module_idx,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                arg_exprs,
                &required_callable_param_specs,
            );
            if (!specs_complete) return;
        }

        if (self.getValueExprCallableInstInCurrentContext(result, module_idx, callee_expr_idx)) |callable_inst_id| {
            if (try self.callableInstSatisfiesCallSiteRequirements(
                result,
                callable_inst_id,
                desired_fn_monotype,
                resolved_template_id,
                required_callable_param_specs.items,
            )) {
                try self.finalizeResolvedDirectCallCallableInst(
                    result,
                    module_idx,
                    call_expr_idx,
                    call_expr,
                    callee_expr,
                    callable_inst_id,
                );
                return;
            }
        }

        if (self.getValueExprCallableInstsInCurrentContext(result, module_idx, callee_expr_idx)) |callable_inst_ids| {
            var all_satisfy = true;
            for (callable_inst_ids) |callable_inst_id| {
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
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    module_idx,
                    callee_expr_idx,
                    callable_inst_ids,
                    &visiting,
                );
                try self.installExprCallDispatch(
                    result,
                    self.currentSourceContext(),
                    module_idx,
                    call_expr_idx,
                    callable_inst_ids,
                );
                if (callable_inst_ids.len == 1) {
                    try self.finalizeResolvedDirectCallCallableInst(
                        result,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                        callee_expr,
                        callable_inst_ids[0],
                    );
                }
                return;
            }
        }

        if (resolved_template_id) |template_id| {
            if (!(templateRequiresConcreteOwnerCallableInst(result, template_id) and
                !sourceContextHasCallableInst(self.currentSourceContext())))
            {
                switch (try self.specializeDirectCallExactCallable(result, module_idx, call_expr_idx, call_expr, template_id)) {
                    .resolved => |callable_inst_id| {
                        try self.finalizeResolvedDirectCallCallableInst(
                            result,
                            module_idx,
                            call_expr_idx,
                            call_expr,
                            callee_expr,
                            callable_inst_id,
                        );
                        return;
                    },
                    .blocked => {
                        return;
                    },
                    .none => {},
                }
            }
        }

        const template_id = resolved_template_id orelse {
            switch (callee_expr) {
                .e_lookup_local, .e_lookup_external, .e_lookup_required => {
                    if (try self.callUsesAnnotationOnlyIntrinsic(module_idx, callee_expr_idx)) return;
                    if (self.active_bindings != null and !desired_fn_monotype.isNone()) {
                        try self.bindCurrentCallFromFnMonotype(
                            result,
                            module_idx,
                            call_expr_idx,
                            call_expr,
                            desired_fn_monotype.idx,
                            desired_fn_monotype.module_idx,
                        );
                    } else if (std.debug.runtime_safety and !sourceContextHasCallableInst(self.currentSourceContext())) {
                        if (!self.visited_exprs.contains(self.currentResultExprKey(module_idx, callee_expr_idx))) {
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
            !sourceContextHasCallableInst(self.currentSourceContext()))
        {
            return;
        }
        const callable_inst_id = resolved: switch (try self.specializeDirectCallExactCallable(result, module_idx, call_expr_idx, call_expr, template_id)) {
            .resolved => |callable_inst_id| break :resolved callable_inst_id,
            .blocked => {
                return;
            },
            .none => {
                if (desired_fn_monotype.isNone() and std.debug.runtime_safety) {
                    const template = result.getCallableTemplate(template_id);
                    const template_env = self.all_module_envs[template.module_idx];
                    const binding_name = if (template.binding_pattern) |binding_pattern|
                        switch (template_env.store.getPattern(binding_pattern)) {
                            .assign => |assign_pat| template_env.getIdent(assign_pat.ident),
                            else => "<non-assign>",
                        }
                    else
                        "<anonymous>";
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
                            self.currentCallableInstRawForDebug(),
                            self.activeRootExprRawForDebug(),
                        },
                    );
                }
                return;
            },
        };
        try self.finalizeResolvedDirectCallCallableInst(
            result,
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
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        callee_expr: anytype,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const source_context = self.currentSourceContext();

        try self.installExprDirectCallDispatch(
            result,
            source_context,
            module_idx,
            call_expr_idx,
            callable_inst_id,
        );
        try self.installExprDirectCallable(
            result,
            source_context,
            module_idx,
            call_expr.func,
            callable_inst_id,
        );
        switch (callee_expr) {
            .e_lookup_local => |lookup| {
                try self.installExprDirectCallable(
                    result,
                    source_context,
                    module_idx,
                    call_expr.func,
                    callable_inst_id,
                );
                try self.installCallableParamDirectValue(
                    result,
                    source_context,
                    module_idx,
                    lookup.pattern_idx,
                    callable_inst_id,
                );
            },
            .e_lookup_external, .e_lookup_required => try self.installExprDirectCallable(
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
        try self.installExprDirectCallableInCallableContext(
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
        try self.prepareCallableArgsForCallableInst(result, module_idx, arg_exprs, callable_inst_id);
        try self.scanCallableInst(result, callable_inst_id);
        try self.recordCallResultCallableInstsFromCallableInst(
            result,
            self.currentSourceContext(),
            module_idx,
            call_expr_idx,
            callable_inst_id,
        );
        try self.ensureCallableArgCallableInstsScanned(result, module_idx, call_expr.args);
        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            call_expr_idx,
            callable_inst_fn_mono.ret,
            callable_inst.fn_monotype_module_idx,
        );
    }

    fn assignCallableArgCallableInstsFromParams(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        param_monos: Monotype.Span,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        if (arg_exprs.len != param_monos.len) unreachable;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(param_monos, i);
            const maybe_template_id = try self.lookupDirectCalleeTemplate(result, module_idx, arg_expr_idx);
            const template_id = maybe_template_id orelse continue;
            const template = result.getCallableTemplate(template_id);
            if (self.active_bindings != null and !sourceContextHasCallableInst(self.currentSourceContext()) and template.kind == .closure) {
                // During template-binding completion, a closure's callable identity is not
                // meaningful until the enclosing callable instantiation exists. Defer that
                // assignment to the later concrete callable-context scan.
                continue;
            }
            const callable_inst_id = try self.ensureCallableInstUnscanned(result, template_id, param_mono, fn_monotype_module_idx);

            try self.installExprDirectCallable(
                result,
                self.currentSourceContext(),
                template.module_idx,
                template.cir_expr,
                callable_inst_id,
            );

            switch (module_env.store.getExpr(arg_expr_idx)) {
                .e_lookup_local => |lookup| {
                    try self.installExprDirectCallable(
                        result,
                        self.currentSourceContext(),
                        module_idx,
                        arg_expr_idx,
                        callable_inst_id,
                    );
                    try self.installCallableParamDirectValue(
                        result,
                        self.currentSourceContext(),
                        module_idx,
                        lookup.pattern_idx,
                        callable_inst_id,
                    );
                    try self.propagateLookupSourceExprCallableValue(result, module_idx, arg_expr_idx, callable_inst_id);
                },
                .e_lookup_external, .e_lookup_required => {
                    try self.installExprDirectCallable(
                        result,
                        self.currentSourceContext(),
                        module_idx,
                        arg_expr_idx,
                        callable_inst_id,
                    );
                    try self.propagateLookupSourceExprCallableValue(result, module_idx, arg_expr_idx, callable_inst_id);
                },
                .e_lambda, .e_closure, .e_hosted_lambda => try self.installExprDirectCallable(
                    result,
                    self.currentSourceContext(),
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

    fn seedCallableParamInstSetsFromActualArgs(
        self: *Pass,
        result: *Result,
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
            if (self.getValueExprCallableInstsInCurrentContext(result, module_idx, arg_expr_idx)) |callable_inst_ids| {
                for (callable_inst_ids) |callable_inst_id| {
                    try self.installCallableParamDirectValueInCallableContext(
                        result,
                        callee_callable_inst_id,
                        template.module_idx,
                        pattern_idx,
                        callable_inst_id,
                    );
                }
                continue;
            }

            const callable_inst_id = self.getValueExprCallableInstInCurrentContext(result, module_idx, arg_expr_idx) orelse continue;
            try self.installCallableParamDirectValueInCallableContext(
                result,
                callee_callable_inst_id,
                template.module_idx,
                pattern_idx,
                callable_inst_id,
            );
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

        for (result.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
            if (!spec.projections.isEmpty()) continue;
            if (spec.param_index >= arg_patterns.len) {
                std.debug.panic(
                    "Pipeline: callable param spec index {d} out of bounds for callable inst {d} (arg_patterns={d})",
                    .{ spec.param_index, @intFromEnum(callable_inst_id), arg_patterns.len },
                );
            }

            const members = result.lambdamono.getCallableMemberSetMembers(spec.callable_member_set_id);
            if (members.len == 0) {
                std.debug.panic(
                    "Pipeline: callable param spec for callable inst {d} param {d} had an empty member set",
                    .{ @intFromEnum(callable_inst_id), spec.param_index },
                );
            }

            try self.installCallableParamValueInCallableContext(
                result,
                callable_inst_id,
                template.module_idx,
                arg_patterns[spec.param_index],
                members,
            );
        }
    }

    fn prepareCallableArgsForCallableInst(
        self: *Pass,
        result: *Result,
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
            module_idx,
            arg_exprs,
            callable_inst_fn_mono.args,
            callable_inst.fn_monotype_module_idx,
        );
        try self.seedCallableParamInstSetsFromActualArgs(
            result,
            module_idx,
            arg_exprs,
            callable_inst_id,
        );
    }

    fn assignCallableArgCallableInstsFromCallMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!void {
        if (self.getCallSiteCallableInstInCurrentContext(result, module_idx, call_expr_idx) != null) return;

        const call_fn_monotype = try self.resolveDirectCallFnMonotype(result, module_idx, call_expr_idx, call_expr);
        if (call_fn_monotype.isNone()) return;

        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(call_fn_monotype)) {
            .func => |func| func,
            else => return,
        };

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) return;

        try self.assignCallableArgCallableInstsFromParams(
            result,
            module_idx,
            arg_exprs,
            fn_mono.args,
            module_idx,
        );
        try self.ensureCallableArgCallableInstsScanned(result, module_idx, call_expr.args);
    }

    fn ensureCallableArgCallableInstsScanned(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        args: CIR.Expr.Span,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
            if (self.getValueExprCallableInstsInCurrentContext(result, module_idx, arg_expr_idx)) |callable_inst_ids| {
                for (callable_inst_ids) |callable_inst_id| {
                    try self.scanCallableInst(result, callable_inst_id);
                }
                continue;
            }

            const callable_inst_id = self.getValueExprCallableInstInCurrentContext(result, module_idx, arg_expr_idx) orelse continue;
            try self.scanCallableInst(result, callable_inst_id);
        }
    }

    fn ensureCallableArgCallableInstsScannedSlice(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (arg_exprs) |arg_expr_idx| {
            if (self.getValueExprCallableInstsInCurrentContext(result, module_idx, arg_expr_idx)) |callable_inst_ids| {
                for (callable_inst_ids) |callable_inst_id| {
                    try self.scanCallableInst(result, callable_inst_id);
                }
                continue;
            }

            const callable_inst_id = self.getValueExprCallableInstInCurrentContext(result, module_idx, arg_expr_idx) orelse continue;
            try self.scanCallableInst(result, callable_inst_id);
        }
    }

    const CallableBoundaryInfo = struct {
        arg_patterns: []const CIR.Pattern.Idx,
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
                .arg_patterns = module_env.store.slicePatterns(lambda_expr.args),
                .body_expr = lambda_expr.body,
            },
            .e_closure => |closure_expr| blk: {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr != .e_lambda) break :blk null;
                break :blk .{
                    .arg_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args),
                    .body_expr = lambda_expr.e_lambda.body,
                };
            },
            .e_hosted_lambda => |hosted_expr| .{
                .arg_patterns = module_env.store.slicePatterns(hosted_expr.args),
                .body_expr = hosted_expr.body,
            },
            else => null,
        };
    }

    fn completeTemplateBindingsFromBody(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        source_context: SourceContext,
    ) Allocator.Error!void {
        const completion_key = TemplateBodyCompletionKey{
            .template_source_key = template.source_key,
            .source_context_kind = switch (source_context) {
                .callable_inst => .callable_inst,
                .root_expr => .root_expr,
                .provenance_expr => .provenance_expr,
                .template_expr => .template_expr,
            },
            .source_context_raw = switch (source_context) {
                .callable_inst => |callable_inst_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(callable_inst_id)))),
                .root_expr => |root| @intFromEnum(root.expr_idx),
                .provenance_expr => |source| @intFromEnum(source.expr_idx),
                .template_expr => |template_ctx| @intFromEnum(template_ctx.expr_idx),
            },
        };
        if (self.in_progress_template_body_completions.contains(completion_key)) {
            return;
        }
        try self.in_progress_template_body_completions.put(self.allocator, completion_key, {});
        defer _ = self.in_progress_template_body_completions.remove(completion_key);

        const module_env = self.all_module_envs[template.module_idx];
        const expr = module_env.store.getExpr(template.cir_expr);

        var iteration_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype) = .empty;
        defer iteration_expr_monotypes.deinit(self.allocator);
        var iteration_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype) = .empty;
        defer iteration_pattern_monotypes.deinit(self.allocator);

        const saved_bindings = self.active_bindings;
        self.active_bindings = bindings;
        defer self.active_bindings = saved_bindings;

        const saved_iteration_expr_monotypes = self.active_iteration_expr_monotypes;
        self.active_iteration_expr_monotypes = &iteration_expr_monotypes;
        defer self.active_iteration_expr_monotypes = saved_iteration_expr_monotypes;
        const saved_iteration_pattern_monotypes = self.active_iteration_pattern_monotypes;
        self.active_iteration_pattern_monotypes = &iteration_pattern_monotypes;
        defer self.active_iteration_pattern_monotypes = saved_iteration_pattern_monotypes;

        try self.pushSourceContext(source_context);
        defer self.popSourceContext();

        const saved_suppress_direct_call_resolution = self.suppress_direct_call_resolution;
        self.suppress_direct_call_resolution = true;
        defer self.suppress_direct_call_resolution = saved_suppress_direct_call_resolution;

        self.scratch_context_expr_monotypes_depth += 1;
        defer self.scratch_context_expr_monotypes_depth -= 1;

        var iterations: u32 = 0;
        while (true) {
            iterations += 1;
            if (std.debug.runtime_safety and iterations > 32) {
                std.debug.panic(
                    "Pipeline: template binding completion did not converge for template={d}",
                    .{@intFromEnum(template.cir_expr)},
                );
            }

            const bindings_before = bindings.count();
            const mutation_revision_before = self.mutation_revision;

            try self.seedTemplateBodyBindingsFromCurrentBindings(result, template, bindings);

            switch (expr) {
                .e_lambda => |lambda_expr| try self.scanCirValueExpr(result, template.module_idx, lambda_expr.body),
                .e_closure => |closure_expr| {
                    const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                    if (lambda_expr == .e_lambda) {
                        try self.scanCirValueExpr(result, template.module_idx, lambda_expr.e_lambda.body);
                    }
                    try self.scanClosureCaptureSources(
                        result,
                        source_context,
                        template.module_idx,
                        template.cir_expr,
                        closure_expr,
                    );
                },
                .e_hosted_lambda => |hosted_expr| try self.scanCirValueExpr(result, template.module_idx, hosted_expr.body),
                else => return,
            }

            try self.bindTemplateBodyResultFromExactExpr(result, template, bindings);

            if (bindings.count() == bindings_before and
                self.mutation_revision == mutation_revision_before)
            {
                break;
            }
        }
    }

    fn bindTemplateBodyResultFromExactExpr(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.callableBoundaryInfo(template.module_idx, template.cir_expr) orelse return;
        const resolved_func = resolveFuncTypeInStore(&module_env.types, template.type_root) orelse return;
        const body_mono = try self.resolveExprMonotypeResolved(result, template.module_idx, boundary.body_expr);
        if (body_mono.isNone()) return;

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            ModuleEnv.varFrom(boundary.body_expr),
            body_mono.idx,
            body_mono.module_idx,
        );
        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            resolved_func.func.ret,
            body_mono.idx,
            body_mono.module_idx,
        );
    }

    fn resolveDirectCallFnMonotype(
        self: *Pass,
        result: *Result,
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
            module_idx,
            call_expr_idx,
            module_env.store.sliceExpr(call_expr.args),
            effectful,
        );
    }

    fn resolveAppliedCallableFnMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        effectful: bool,
    ) Allocator.Error!Monotype.Idx {
        const ret_monotype = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
        if (ret_monotype.isNone()) return .none;

        var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer arg_monotypes.deinit(self.allocator);

        for (arg_exprs) |arg_expr_idx| {
            const arg_monotype = try self.resolveExprMonotypeResolved(result, module_idx, arg_expr_idx);
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        arg_exprs: []const CIR.Expr.Idx,
        effectful: bool,
    ) Allocator.Error!Monotype.Idx {
        const ret_monotype = try self.resolveExprMonotypeIfFullyBound(result, module_idx, expr_idx);
        if (ret_monotype.isNone()) return .none;

        var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer arg_monotypes.deinit(self.allocator);

        for (arg_exprs) |arg_expr_idx| {
            const arg_monotype = try self.resolveExprMonotypeIfFullyBound(result, module_idx, arg_expr_idx);
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
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        template_id: CallableTemplateId,
    ) Allocator.Error!DirectCallCallableInstResolution {
        if (try self.directCallContainsErrorType(module_idx, call_expr_idx, call_expr)) {
            return .none;
        }

        const saved_binding_probe_mode = self.binding_probe_mode;
        const saved_binding_probe_failed = self.binding_probe_failed;
        self.binding_probe_mode = true;
        self.binding_probe_failed = false;
        defer {
            self.binding_probe_mode = saved_binding_probe_mode;
            self.binding_probe_failed = saved_binding_probe_failed;
        }

        const template = result.getCallableTemplate(template_id).*;
        const template_env = self.all_module_envs[template.module_idx];
        const template_types = &template_env.types;
        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        const exact_desired_fn_monotype = resolvedMonotype(
            try self.resolveAppliedCallableFnMonotypeIfFullyBound(
                result,
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
        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, template);

        var callee_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer callee_bindings.deinit();

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        if (!exact_desired_fn_monotype.isNone()) {
            try self.seedTemplateBindingsFromFnMonotype(result, template, exact_desired_fn_monotype, &callee_bindings);
            if (self.binding_probe_failed) return .none;
        }

        if (resolveFuncTypeInStore(template_types, template.type_root)) |resolved_func| {
            const ret_mono = try self.resolveExprMonotypeResolved(result, module_idx, call_expr_idx);
            if (!ret_mono.isNone()) {
                if (self.callableBoundaryInfo(template.module_idx, template.cir_expr)) |boundary| {
                    try self.bindTypeVarMonotypes(
                        result,
                        template.module_idx,
                        template_types,
                        &callee_bindings,
                        &ordered_entries,
                        ModuleEnv.varFrom(boundary.body_expr),
                        ret_mono.idx,
                        ret_mono.module_idx,
                    );
                    if (self.binding_probe_failed) return .none;
                }
                var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
                defer seen.deinit(self.allocator);

                if (!try self.typeVarFullyBoundWithBindings(
                    result,
                    template.module_idx,
                    template_types,
                    resolved_func.func.ret,
                    &callee_bindings,
                    &seen,
                )) {
                    try self.bindTypeVarMonotypes(
                        result,
                        template.module_idx,
                        template_types,
                        &callee_bindings,
                        &ordered_entries,
                        resolved_func.func.ret,
                        ret_mono.idx,
                        ret_mono.module_idx,
                    );
                    if (self.binding_probe_failed) return .none;
                }
            }

            const param_vars = template_types.sliceVars(resolved_func.func.args);
            if (param_vars.len != arg_exprs.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Pipeline: direct call template arity mismatch at expr {d} (params={d}, args={d})",
                        .{
                            @intFromEnum(call_expr_idx),
                            param_vars.len,
                            arg_exprs.len,
                        },
                    );
                }
                unreachable;
            }

            try self.bindTemplateParamsFromActualArgs(
                result,
                module_idx,
                template,
                template_types,
                param_vars,
                arg_exprs,
                &callee_bindings,
                &ordered_entries,
                true,
            );
            if (self.binding_probe_failed) return .none;
        }

        try self.completeTemplateBindingsFromBody(
            result,
            template,
            &callee_bindings,
            defining_source_context,
        );
        if (self.binding_probe_failed) return .none;

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);
        if (!try self.typeVarFullyBoundWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
            &seen,
        )) return .none;

        const fn_monotype = try self.resolveTypeVarMonotypeWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
        );
        if (fn_monotype.isNone()) return .none;

        if (!try self.procSignatureAcceptsFnMonotype(
            result,
            template_id,
            template,
            fn_monotype,
            template.module_idx,
            sourceContextCallableInst(defining_source_context),
        )) return .none;

        var callable_param_specs = std.ArrayListUnmanaged(CallableParamSpecEntry).empty;
        defer callable_param_specs.deinit(self.allocator);
        const callable_param_specs_complete = try self.collectDirectCallCallableParamSpecs(
            result,
            self.currentSourceContext(),
            module_idx,
            fn_monotype,
            template.module_idx,
            arg_exprs,
            &callable_param_specs,
        );
        if (!callable_param_specs_complete) return .blocked;

        return .{ .resolved = try self.ensureCallableInstUnscannedWithCallableParamSpecs(
            result,
            template_id,
            fn_monotype,
            template.module_idx,
            callable_param_specs.items,
        ) };
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
        var demand_visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer demand_visiting.deinit(self.allocator);
        try self.propagateDemandedValueMonotypeToValueExpr(
            result,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
            &demand_visiting,
        );

        switch (result.context_mono.monotype_store.getMonotype(monotype)) {
            .func => {
                if (self.getValueExprCallableMembersForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
                    const callable_member_set_id = try self.internCallableMemberSet(result, callable_inst_ids);
                    try self.appendCallableParamSpecEntry(result, out, .{
                        .param_index = param_index,
                        .projections = try self.addCallableParamProjectionEntries(result, projections.items),
                        .callable_member_set_id = callable_member_set_id,
                    });
                    return true;
                }

                if (self.getValueExprCallableInstForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_id| {
                    const callable_member_set_id = try self.internCallableMemberSet(result, &.{callable_inst_id});
                    try self.appendCallableParamSpecEntry(result, out, .{
                        .param_index = param_index,
                        .projections = try self.addCallableParamProjectionEntries(result, projections.items),
                        .callable_member_set_id = callable_member_set_id,
                    });
                    return true;
                }

                // Function-valued arguments must be scanned/materialized through the same
                // value-expression pipeline the rest of staged callable resolution uses
                // before we conclude their exact callable identity is unavailable.
                try self.scanCirValueExpr(result, module_idx, expr_idx);

                if (self.getValueExprCallableMembersForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
                    const callable_member_set_id = try self.internCallableMemberSet(result, callable_inst_ids);
                    try self.appendCallableParamSpecEntry(result, out, .{
                        .param_index = param_index,
                        .projections = try self.addCallableParamProjectionEntries(result, projections.items),
                        .callable_member_set_id = callable_member_set_id,
                    });
                    return true;
                }

                if (self.getValueExprCallableInstForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_id| {
                    const callable_member_set_id = try self.internCallableMemberSet(result, &.{callable_inst_id});
                    try self.appendCallableParamSpecEntry(result, out, .{
                        .param_index = param_index,
                        .projections = try self.addCallableParamProjectionEntries(result, projections.items),
                        .callable_member_set_id = callable_member_set_id,
                    });
                    return true;
                }

                return false;
            },
            .record => |record| {
                for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                    var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                    defer visiting.deinit(self.allocator);
                    const field_source = switch (try self.resolveRecordFieldExprInContext(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                        field.name.module_idx,
                        field.name.ident,
                        &visiting,
                    )) {
                        .none => continue,
                        .blocked => return false,
                        .source => |source| source,
                    };

                    try projections.append(self.allocator, .{ .field = field.name });
                    defer _ = projections.pop();

                    if (!try self.collectCallableParamSpecsFromArgument(
                        result,
                        source_context,
                        field_source.module_idx,
                        field_source.expr_idx,
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
                    var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
                    defer visiting.deinit(self.allocator);
                    const elem_source = switch (try self.resolveTupleElemExprInContext(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                        @intCast(elem_index),
                        &visiting,
                    )) {
                        .none => continue,
                        .blocked => return false,
                        .source => |source| source,
                    };

                    try projections.append(self.allocator, .{ .tuple_elem = @intCast(elem_index) });
                    defer _ = projections.pop();

                    if (!try self.collectCallableParamSpecsFromArgument(
                        result,
                        source_context,
                        elem_source.module_idx,
                        elem_source.expr_idx,
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
        const saved_binding_probe_mode = self.binding_probe_mode;
        const saved_binding_probe_failed = self.binding_probe_failed;
        _ = template_id;
        self.binding_probe_mode = true;
        self.binding_probe_failed = false;
        defer {
            self.binding_probe_mode = saved_binding_probe_mode;
            self.binding_probe_failed = saved_binding_probe_failed;
        }

        var iteration_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype) = .empty;
        defer iteration_expr_monotypes.deinit(self.allocator);
        var iteration_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype) = .empty;
        defer iteration_pattern_monotypes.deinit(self.allocator);

        const saved_iteration_expr_monotypes = self.active_iteration_expr_monotypes;
        self.active_iteration_expr_monotypes = &iteration_expr_monotypes;
        defer self.active_iteration_expr_monotypes = saved_iteration_expr_monotypes;
        const saved_iteration_pattern_monotypes = self.active_iteration_pattern_monotypes;
        self.active_iteration_pattern_monotypes = &iteration_pattern_monotypes;
        defer self.active_iteration_pattern_monotypes = saved_iteration_pattern_monotypes;

        self.scratch_context_expr_monotypes_depth += 1;
        defer self.scratch_context_expr_monotypes_depth -= 1;

        var signature_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer signature_bindings.deinit();

        try self.seedCallableBodyBindingsFromSignature(
            result,
            template.module_idx,
            template.cir_expr,
            defining_source_context,
            fn_monotype,
            fn_monotype_module_idx,
            &signature_bindings,
        );

        return !self.binding_probe_failed;
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        const resolved = try self.resolveExprMonotypeIfFullyBound(result, module_idx, expr_idx);
        if (!resolved.isNone()) return resolved;

        std.debug.panic(
            "Pipeline invariant violated: missing fully-bound monotype for expr {d} ({s}) in module {d} under source context {s}/{d}/{d}",
            .{
                @intFromEnum(expr_idx),
                @tagName(self.all_module_envs[module_idx].store.getExpr(expr_idx)),
                module_idx,
                @tagName(self.currentSourceContext()),
                switch (self.currentSourceContext()) {
                    .callable_inst => |context_id| @intFromEnum(@as(CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
                    .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
                },
                switch (self.currentSourceContext()) {
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
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        resolved_mono: ResolvedMonotype,
    ) Allocator.Error!void {
        if (resolved_mono.isNone()) return;
        try self.mergeTrackedContextPatternMonotype(
            result,
            self.currentResultPatternKey(module_idx, pattern_idx),
            resolved_mono,
        );

        const module_env = self.all_module_envs[module_idx];
        if (self.active_bindings) |bindings| {
            var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
            defer ordered_entries.deinit(self.allocator);

            try self.bindTypeVarMonotypes(
                result,
                module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(pattern_idx),
                resolved_mono.idx,
                resolved_mono.module_idx,
            );
        }

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
                    module_idx,
                    as_pat.pattern,
                    resolved_mono,
                );
            },
            .nominal => |nominal_pat| {
                try self.bindCurrentPatternFromResolvedMonotype(
                    result,
                    module_idx,
                    nominal_pat.backing_pattern,
                    resolved_mono,
                );
            },
            .nominal_external => |nominal_pat| {
                try self.bindCurrentPatternFromResolvedMonotype(
                    result,
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
                        module_idx,
                        elem_pattern_idx,
                        resolvedMonotype(elem_mono, resolved_mono.module_idx),
                    );
                }
                if (list_pat.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern_idx| {
                        try self.bindCurrentPatternFromResolvedMonotype(
                            result,
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
                        module_idx,
                        elem_pattern_idx,
                        resolvedMonotype(elem_mono, resolved_mono.module_idx),
                    );
                }
            },
        }
    }

    fn bindTemplateParamsFromActualArgs(
        self: *Pass,
        result: *Result,
        actual_module_idx: u32,
        template: CallableTemplate,
        template_types: *const types.Store,
        param_vars: []const types.Var,
        actual_args: []const CIR.Expr.Idx,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        skip_fully_bound_params: bool,
    ) Allocator.Error!void {
        if (param_vars.len != actual_args.len) unreachable;

        var deferred_actuals = std.ArrayList(usize).empty;
        defer deferred_actuals.deinit(self.allocator);

        for (param_vars, actual_args, 0..) |param_var, arg_expr_idx, arg_i| {
            const callable_template = try self.resolveExprCallableTemplate(result, actual_module_idx, arg_expr_idx);
            if (callable_template != null) {
                if (try self.inferCallableActualFromCallerParam(
                    result,
                    actual_module_idx,
                    arg_expr_idx,
                    template,
                    template_types,
                    param_var,
                    bindings,
                )) |callable_mono| {
                    try self.bindTemplateParamActualMonotype(
                        result,
                        template,
                        template_types,
                        bindings,
                        ordered_entries,
                        param_var,
                        callable_mono,
                        skip_fully_bound_params,
                    );
                    continue;
                }

                try deferred_actuals.append(self.allocator, arg_i);
                continue;
            }

            const expected_param_mono = try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
                result,
                template.module_idx,
                template_types,
                param_var,
                bindings,
            );
            const exact_arg_mono = try self.resolveExprMonotypeResolved(result, actual_module_idx, arg_expr_idx);
            if (!exact_arg_mono.isNone()) {
                try self.bindTemplateParamActualMonotype(
                    result,
                    template,
                    template_types,
                    bindings,
                    ordered_entries,
                    param_var,
                    exact_arg_mono,
                    skip_fully_bound_params,
                );
                continue;
            }

            if (expected_param_mono) |bound_param_mono| {
                try self.bindTemplateParamActualMonotype(
                    result,
                    template,
                    template_types,
                    bindings,
                    ordered_entries,
                    param_var,
                    bound_param_mono,
                    skip_fully_bound_params,
                );
                continue;
            }

            try deferred_actuals.append(self.allocator, arg_i);
        }

        for (deferred_actuals.items) |arg_i| {
            const param_var = param_vars[arg_i];
            const arg_expr_idx = actual_args[arg_i];
            const callable_template = try self.resolveExprCallableTemplate(result, actual_module_idx, arg_expr_idx);
            if (callable_template != null) {
                if (try self.inferCallableActualFromCallerParam(
                    result,
                    actual_module_idx,
                    arg_expr_idx,
                    template,
                    template_types,
                    param_var,
                    bindings,
                )) |callable_mono| {
                    try self.bindTemplateParamActualMonotype(
                        result,
                        template,
                        template_types,
                        bindings,
                        ordered_entries,
                        param_var,
                        callable_mono,
                        skip_fully_bound_params,
                    );
                }
                continue;
            }

            const exact_arg_mono = try self.resolveExprMonotypeResolved(result, actual_module_idx, arg_expr_idx);
            const bound_param_mono = try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
                result,
                template.module_idx,
                template_types,
                param_var,
                bindings,
            );

            const arg_mono = if (bound_param_mono) |bound|
                bound
            else if (!exact_arg_mono.isNone())
                exact_arg_mono
            else
                try self.requireFullyBoundExprMonotype(result, actual_module_idx, arg_expr_idx);

            try self.bindTemplateParamActualMonotype(
                result,
                template,
                template_types,
                bindings,
                ordered_entries,
                param_var,
                arg_mono,
                skip_fully_bound_params,
            );
        }
    }

    fn inferCallableActualFromCallerParam(
        self: *Pass,
        result: *Result,
        actual_module_idx: u32,
        arg_expr_idx: CIR.Expr.Idx,
        caller_template: CallableTemplate,
        caller_template_types: *const types.Store,
        caller_param_var: types.Var,
        caller_bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!?ResolvedMonotype {
        const actual_template_id = (try self.resolveExprCallableTemplate(result, actual_module_idx, arg_expr_idx)) orelse return null;
        const actual_template = result.getCallableTemplate(actual_template_id).*;
        if (templateRequiresConcreteOwnerCallableInst(result, actual_template_id) and
            !sourceContextHasCallableInst(self.currentSourceContext()))
        {
            return null;
        }
        const actual_template_types = &self.all_module_envs[actual_template.module_idx].types;
        const actual_func = resolveFuncTypeInStore(actual_template_types, actual_template.type_root) orelse return null;
        const caller_func = resolveFuncTypeInStore(caller_template_types, caller_param_var) orelse return null;
        const caller_arg_vars = caller_template_types.sliceVars(caller_func.func.args);
        const actual_arg_vars = actual_template_types.sliceVars(actual_func.func.args);
        if (caller_arg_vars.len != actual_arg_vars.len) return null;

        var actual_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer actual_bindings.deinit();
        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        for (caller_arg_vars, actual_arg_vars) |caller_arg_var, actual_arg_var| {
            if (try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
                result,
                caller_template.module_idx,
                caller_template_types,
                caller_arg_var,
                caller_bindings,
            )) |caller_arg_mono| {
                try self.bindTypeVarMonotypes(
                    result,
                    actual_template.module_idx,
                    actual_template_types,
                    &actual_bindings,
                    &ordered_entries,
                    actual_arg_var,
                    caller_arg_mono.idx,
                    caller_arg_mono.module_idx,
                );
            }
        }

        if (try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
            result,
            caller_template.module_idx,
            caller_template_types,
            caller_func.func.ret,
            caller_bindings,
        )) |caller_ret_mono| {
            try self.bindTypeVarMonotypes(
                result,
                actual_template.module_idx,
                actual_template_types,
                &actual_bindings,
                &ordered_entries,
                actual_func.func.ret,
                caller_ret_mono.idx,
                caller_ret_mono.module_idx,
            );
        }

        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, actual_template);
        try self.completeTemplateBindingsFromBody(
            result,
            actual_template,
            &actual_bindings,
            defining_source_context,
        );

        if ((try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
            result,
            actual_template.module_idx,
            actual_template_types,
            actual_template.type_root,
            &actual_bindings,
        ))) |callable_mono| {
            return callable_mono;
        }

        return null;
    }

    fn bindTemplateParamActualMonotype(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        template_types: *const types.Store,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        param_var: types.Var,
        arg_mono: ResolvedMonotype,
        skip_fully_bound_params: bool,
    ) Allocator.Error!void {
        if (arg_mono.isNone()) return;

        if (skip_fully_bound_params) {
            var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
            defer seen.deinit(self.allocator);

            if (try self.typeVarFullyBoundWithBindings(
                result,
                template.module_idx,
                template_types,
                param_var,
                bindings,
                &seen,
            )) {
                return;
            }
        }

        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            template_types,
            bindings,
            ordered_entries,
            param_var,
            arg_mono.idx,
            arg_mono.module_idx,
        );
    }

    fn bindCurrentCallFromCallableInst(
        self: *Pass,
        result: *Result,
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
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
                &visiting,
            );
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, call_expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, call_expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
    }

    fn bindCurrentCallFromFnMonotype(
        self: *Pass,
        result: *Result,
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
            module_idx,
            call_expr.func,
            fn_monotype,
            fn_monotype_module_idx,
        );
        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            call_expr.func,
            fn_monotype,
            fn_monotype_module_idx,
        );

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) return;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, fn_monotype_module_idx);
            var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                module_idx,
                arg_expr_idx,
                param_mono,
                fn_monotype_module_idx,
                &visiting,
            );
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
    }

    fn propagateDemandedValueMonotypeToValueExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!void {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        if (expr == .e_anno_only) return;

        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
        );

        switch (expr) {
            .e_lookup_local => |lookup| {
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    try self.propagateDemandedValueMonotypeToValueExpr(
                        result,
                        source.module_idx,
                        source.expr_idx,
                        monotype,
                        monotype_module_idx,
                        visiting,
                    );
                }
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                const target_env = self.all_module_envs[target_module_idx];
                if (!target_env.store.isDefNode(lookup.target_node_idx)) return;
                const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                try self.propagateDemandedValueMonotypeToValueExpr(
                    result,
                    target_module_idx,
                    def.expr,
                    monotype,
                    monotype_module_idx,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                const target_env = self.all_module_envs[target.module_idx];
                const def = target_env.store.getDef(target.def_idx);
                try self.propagateDemandedValueMonotypeToValueExpr(
                    result,
                    target.module_idx,
                    def.expr,
                    monotype,
                    monotype_module_idx,
                    visiting,
                );
            },
            .e_nominal => |nominal_expr| {
                try self.propagateDemandedValueMonotypeToValueExpr(
                    result,
                    module_idx,
                    nominal_expr.backing_expr,
                    monotype,
                    monotype_module_idx,
                    visiting,
                );
            },
            .e_nominal_external => |nominal_expr| {
                try self.propagateDemandedValueMonotypeToValueExpr(
                    result,
                    module_idx,
                    nominal_expr.backing_expr,
                    monotype,
                    monotype_module_idx,
                    visiting,
                );
            },
            else => {},
        }
    }

    fn propagateDemandedCallableFnMonotypeToValueExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!void {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        if (expr == .e_anno_only) return;

        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
        );

        switch (expr) {
            .e_lookup_local => |lookup| {
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    try self.propagateDemandedCallableFnMonotypeToValueExpr(
                        result,
                        source.module_idx,
                        source.expr_idx,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                }
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse return;
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse return;
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.propagateDemandedCallableFnMonotypeToValueExpr(
                        result,
                        module_idx,
                        branch.body,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                }
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
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
                        module_idx,
                        branch.value,
                        fn_monotype,
                        fn_monotype_module_idx,
                        visiting,
                    );
                }
            },
            .e_block => |block_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    block_expr.final_expr,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_dbg => |dbg_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    dbg_expr.expr,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_expect => |expect_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    expect_expr.body,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_return => |return_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    return_expr.expr,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_nominal => |nominal_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    nominal_expr.backing_expr,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_nominal_external => |nominal_expr| {
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    module_idx,
                    nominal_expr.backing_expr,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_dot_access => |dot_expr| {
                if (dot_expr.args != null) return;
                const field_expr = try self.resolveRecordFieldExpr(
                    result,
                    module_idx,
                    dot_expr.receiver,
                    module_idx,
                    dot_expr.field_name,
                    visiting,
                ) orelse return;
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    field_expr.module_idx,
                    field_expr.expr_idx,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            .e_tuple_access => |tuple_access| {
                const elem_expr = try self.resolveTupleElemExpr(
                    result,
                    module_idx,
                    tuple_access.tuple,
                    tuple_access.elem_index,
                    visiting,
                ) orelse return;
                try self.propagateDemandedCallableFnMonotypeToValueExpr(
                    result,
                    elem_expr.module_idx,
                    elem_expr.expr_idx,
                    fn_monotype,
                    fn_monotype_module_idx,
                    visiting,
                );
            },
            else => {},
        }
    }

    fn recordCallResultCallableInstsFromCallableInst(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        callee_callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const in_progress_key = CallResultCallableInstKey{
            .context_expr = Result.contextExprKey(source_context, module_idx, call_expr_idx),
            .callee_callable_inst_raw = @intFromEnum(callee_callable_inst_id),
        };
        if (self.in_progress_call_result_callable_insts.contains(in_progress_key)) return;
        try self.in_progress_call_result_callable_insts.put(self.allocator, in_progress_key, {});
        defer _ = self.in_progress_call_result_callable_insts.remove(in_progress_key);

        const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
        const template = result.getCallableTemplate(callee_callable_inst.template);
        const boundary = self.callableBoundaryInfo(template.module_idx, template.cir_expr) orelse return;

        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        var callable_inst_ids = std.ArrayList(CallableInstId).empty;
        defer callable_inst_ids.deinit(self.allocator);

        try self.collectProgramExprCallableMembers(
            result,
            callableInstSourceContext(callee_callable_inst_id),
            template.module_idx,
            boundary.body_expr,
            &visiting,
            &callable_inst_ids,
        );

        try self.pushSourceContext(source_context);
        defer self.popSourceContext();
        try self.installDemandedExprCallableValue(
            result,
            module_idx,
            call_expr_idx,
            callable_inst_ids.items,
        );
    }

    fn collectProgramExprCallableMembers(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
        out: *std.ArrayList(CallableInstId),
    ) Allocator.Error!void {
        const visit_key = contextExprVisitKey(source_context, module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        if (self.getValueExprCallableMembersForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
            try self.appendUniqueCallableInsts(out, callable_inst_ids);
            return;
        }

        if (self.getValueExprCallableInstForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_id| {
            try self.appendUniqueCallableInsts(out, &.{callable_inst_id});
            return;
        }

        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    try self.collectProgramExprCallableMembers(
                        result,
                        source_context,
                        source.module_idx,
                        source.expr_idx,
                        visiting,
                        out,
                    );
                }
            },
            .e_if => |if_expr| {
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.collectProgramExprCallableMembers(result, source_context, module_idx, branch.body, visiting, out);
                }
                try self.collectProgramExprCallableMembers(result, source_context, module_idx, if_expr.final_else, visiting, out);
            },
            .e_match => |match_expr| {
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.collectProgramExprCallableMembers(result, source_context, module_idx, branch.value, visiting, out);
                }
            },
            .e_block => |block_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, block_expr.final_expr, visiting, out),
            .e_dbg => |dbg_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, dbg_expr.expr, visiting, out),
            .e_expect => |expect_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, expect_expr.body, visiting, out),
            .e_return => |return_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, return_expr.expr, visiting, out),
            .e_nominal => |nominal_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, nominal_expr.backing_expr, visiting, out),
            .e_nominal_external => |nominal_expr| try self.collectProgramExprCallableMembers(result, source_context, module_idx, nominal_expr.backing_expr, visiting, out),
            .e_dot_access => |dot_expr| {
                if (dot_expr.args != null) return;
                const field_expr = try self.resolveRecordFieldExpr(
                    result,
                    module_idx,
                    dot_expr.receiver,
                    module_idx,
                    dot_expr.field_name,
                    visiting,
                ) orelse return;
                try self.collectProgramExprCallableMembers(
                    result,
                    source_context,
                    field_expr.module_idx,
                    field_expr.expr_idx,
                    visiting,
                    out,
                );
            },
            .e_tuple_access => |tuple_access| {
                const elem_expr = try self.resolveTupleElemExpr(
                    result,
                    module_idx,
                    tuple_access.tuple,
                    tuple_access.elem_index,
                    visiting,
                ) orelse return;
                try self.collectProgramExprCallableMembers(
                    result,
                    source_context,
                    elem_expr.module_idx,
                    elem_expr.expr_idx,
                    visiting,
                    out,
                );
            },
            .e_call => |call_expr| {
                if (self.getCallLowLevelOp(module_env, call_expr.func) != null) return;

                const callee_callable_inst_ids = result.getExprCallDispatchMembers(source_context, module_idx, expr_idx) orelse {
                    std.debug.panic(
                        "Pipeline invariant violated: callable-valued call expr {d} in module {d} had no explicit lambdamono call dispatch in source context {s}",
                        .{
                            @intFromEnum(expr_idx),
                            module_idx,
                            @tagName(source_context),
                        },
                    );
                };

                for (callee_callable_inst_ids) |callee_callable_inst_id| {
                    try self.recordCallResultCallableInstsFromCallableInst(
                        result,
                        source_context,
                        module_idx,
                        expr_idx,
                        callee_callable_inst_id,
                    );
                }

                if (self.getValueExprCallableMembersForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_ids| {
                    try self.appendUniqueCallableInsts(out, callable_inst_ids);
                    return;
                }

                if (self.getValueExprCallableInstForSourceContext(result, source_context, module_idx, expr_idx)) |callable_inst_id| {
                    try self.appendUniqueCallableInsts(out, &.{callable_inst_id});
                    return;
                }

                for (callee_callable_inst_ids) |callee_callable_inst_id| {
                    const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
                    const callee_template = result.getCallableTemplate(callee_callable_inst.template);
                    const boundary = self.callableBoundaryInfo(callee_template.module_idx, callee_template.cir_expr) orelse continue;
                    try self.collectProgramExprCallableMembers(
                        result,
                        callableInstSourceContext(callee_callable_inst_id),
                        callee_template.module_idx,
                        boundary.body_expr,
                        visiting,
                        out,
                    );
                }
                return;
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse return;
                try self.collectProgramExprCallableMembers(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    visiting,
                    out,
                );
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse return;
                try self.collectProgramExprCallableMembers(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    visiting,
                    out,
                );
            },
            .e_closure, .e_lambda, .e_hosted_lambda => std.debug.panic(
                "Pipeline invariant violated: callable-capable expr {d} ({s}) in module {d} reached callable-member propagation without explicit lambdamono callable semantics",
                .{
                    @intFromEnum(expr_idx),
                    @tagName(module_env.store.getExpr(expr_idx)),
                    module_idx,
                },
            ),
            else => {},
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        template_id: CallableTemplateId,
    ) Allocator.Error!?CallableInstId {
        const saved_binding_probe_mode = self.binding_probe_mode;
        const saved_binding_probe_failed = self.binding_probe_failed;
        self.binding_probe_mode = true;
        self.binding_probe_failed = false;
        defer {
            self.binding_probe_mode = saved_binding_probe_mode;
            self.binding_probe_failed = saved_binding_probe_failed;
        }
        const template = result.getCallableTemplate(template_id).*;
        const template_env = self.all_module_envs[template.module_idx];
        const template_types = &template_env.types;
        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, template);

        var callee_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer callee_bindings.deinit();

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);

        const exact_desired_fn_monotype = resolvedMonotype(
            try self.resolveAppliedCallableFnMonotypeIfFullyBound(
                result,
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
        if (!exact_desired_fn_monotype.isNone()) {
            try self.seedTemplateBindingsFromFnMonotype(result, template, exact_desired_fn_monotype, &callee_bindings);
        }
        if (self.binding_probe_failed) return null;

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

            const ret_mono = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
            if (!ret_mono.isNone()) {
                if (self.callableBoundaryInfo(template.module_idx, template.cir_expr)) |boundary| {
                    try self.bindTypeVarMonotypes(
                        result,
                        template.module_idx,
                        template_types,
                        &callee_bindings,
                        &ordered_entries,
                        ModuleEnv.varFrom(boundary.body_expr),
                        ret_mono.idx,
                        ret_mono.module_idx,
                    );
                    if (self.binding_probe_failed) return null;
                }
                try self.bindTypeVarMonotypes(
                    result,
                    template.module_idx,
                    template_types,
                    &callee_bindings,
                    &ordered_entries,
                    resolved_func.func.ret,
                    ret_mono.idx,
                    ret_mono.module_idx,
                );
                if (self.binding_probe_failed) return null;
            }

            try self.bindTemplateParamsFromActualArgs(
                result,
                module_idx,
                template,
                template_types,
                param_vars,
                actual_args.items,
                &callee_bindings,
                &ordered_entries,
                true,
            );
            if (self.binding_probe_failed) return null;
        }

        try self.seedTemplateBoundaryBindingsFromActuals(
            result,
            module_idx,
            template,
            actual_args.items,
            try self.resolveExprMonotypeResolved(result, module_idx, expr_idx),
            &callee_bindings,
        );
        if (self.binding_probe_failed) return null;

        try self.completeTemplateBindingsFromBody(
            result,
            template,
            &callee_bindings,
            defining_source_context,
        );
        if (self.binding_probe_failed) return null;

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);
        if (!try self.typeVarFullyBoundWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
            &seen,
        )) return null;

        const fn_monotype = try self.resolveTypeVarMonotypeWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
        );
        if (fn_monotype.isNone()) return null;

        if (!try self.procSignatureAcceptsFnMonotype(
            result,
            template_id,
            template,
            fn_monotype,
            template.module_idx,
            sourceContextCallableInst(defining_source_context),
        )) return null;

        const callable_inst_id = try self.ensureCallableInstUnscanned(result, template_id, fn_monotype, template.module_idx);
        return callable_inst_id;
    }

    fn bindCurrentDispatchFromCallableInst(
        self: *Pass,
        result: *Result,
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
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, callable_inst.fn_monotype_module_idx);
            var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
            defer visiting.deinit(self.allocator);
            try self.propagateDemandedValueMonotypeToValueExpr(
                result,
                module_idx,
                arg_expr_idx,
                param_mono,
                callable_inst.fn_monotype_module_idx,
                &visiting,
            );
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, expr_idx, fn_mono.ret, callable_inst.fn_monotype_module_idx);
    }

    fn bindCurrentExprTypeRoot(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        const bindings = self.active_bindings orelse return;
        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);
        try self.bindTypeVarMonotypes(
            result,
            module_idx,
            &self.all_module_envs[module_idx].types,
            bindings,
            &ordered_entries,
            ModuleEnv.varFrom(expr_idx),
            monotype,
            monotype_module_idx,
        );
    }

    fn bindCurrentCallableExprTypeRoot(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        const bindings = self.active_bindings orelse return;
        const module_env = self.all_module_envs[module_idx];
        const type_root = switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| ModuleEnv.varFrom(lookup.pattern_idx),
            else => ModuleEnv.varFrom(expr_idx),
        };

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);
        try self.bindTypeVarMonotypes(
            result,
            module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            type_root,
            monotype,
            monotype_module_idx,
        );
    }

    fn recordCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: Monotype.Idx,
        monotype_module_idx: u32,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;
        try self.recordExprMonotypeForSourceContext(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            monotype,
            monotype_module_idx,
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
                const context_template = self.templateContextForSourceContext(result, source_context);
                std.debug.panic(
                    "Pipeline: conflicting exact expr monotypes for ctx={s} module={d} expr={d} kind={s} region={any} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} template_expr={d}",
                    .{
                        @tagName(source_context),
                        module_idx,
                        @intFromEnum(expr_idx),
                        @tagName(expr),
                        expr_region,
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

        if (self.active_iteration_expr_monotypes) |iteration_map| {
            try iteration_map.put(self.allocator, key, resolved);
        } else {
            if (self.scratch_context_expr_monotypes_depth != 0) {
                try result.context_mono.context_expr_monotypes.put(self.allocator, key, resolved);
            } else {
                try self.mergeTrackedContextExprMonotype(result, key, resolved);
            }
        }
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
        if (self.active_iteration_pattern_monotypes) |iteration_map| {
            try iteration_map.put(self.allocator, key, resolved);
            return;
        }
        return self.mergeTrackedResolvedMonotypeMap(
            result,
            .context_pattern_monotypes,
            &result.context_mono.context_pattern_monotypes,
            key,
            resolved,
            "exact pattern",
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        record_expr: @TypeOf(@as(CIR.Expr, undefined).e_record),
    ) Allocator.Error!void {
        const record_mono = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        tuple_expr: @TypeOf(@as(CIR.Expr, undefined).e_tuple),
    ) Allocator.Error!void {
        const tuple_mono = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
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
        module_idx: u32,
        parent_expr_idx: CIR.Expr.Idx,
        child_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const parent_mono = try self.resolveExprMonotypeResolved(result, module_idx, parent_expr_idx);
        if (parent_mono.isNone()) return;
        const child_expr = self.all_module_envs[module_idx].store.getExpr(child_expr_idx);
        if (exprMonotypeOwnedByInvocation(child_expr)) return;
        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            child_expr_idx,
            parent_mono.idx,
            parent_mono.module_idx,
        );
    }

    fn resolveDispatchExprCallableInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        var associated_target: ?ResolvedDispatchTarget = null;
        if (expr == .e_binop) {
            const binop_expr = expr.e_binop;
            if (try self.binopDispatchHandledWithoutCallableInst(result, module_idx, expr_idx, binop_expr)) {
                return;
            }
            const method_name = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse return;
            if (try self.resolveAssociatedMethodCallableInstForTypeVar(
                result,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            )) |callable_inst_id| {
                try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            );
            const lhs_monotype = try self.resolveExprMonotypeResolved(result, module_idx, binop_expr.lhs);
            if (associated_target == null and !lhs_monotype.isNone()) {
                if (try self.resolveAssociatedMethodCallableInstForMonotype(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                    result,
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
                if (try self.dotDispatchHandledWithoutCallableInst(result, module_idx, expr_idx, dot_expr)) {
                    return;
                }
                if (try self.resolveAssociatedMethodCallableInstForTypeVar(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                    result,
                    module_idx,
                    expr_idx,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                );
                const receiver_monotype = try self.resolveExprMonotypeResolved(result, module_idx, dot_expr.receiver);
                if (associated_target == null and !receiver_monotype.isNone()) {
                    if (try self.resolveAssociatedMethodCallableInstForMonotype(
                        result,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    )) |callable_inst_id| {
                        try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                        return;
                    }
                    associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                        result,
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
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            )) |callable_inst_id| {
                try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            );
            const alias_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, ModuleEnv.varFrom(alias_stmt.type_var_anno));
            if (associated_target == null and !alias_monotype.isNone()) {
                if (try self.resolveAssociatedMethodCallableInstForMonotype(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    alias_monotype.idx,
                    dispatch_expr.method_name,
                )) |callable_inst_id| {
                    try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForMonotype(
                    result,
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
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, module_env.idents.negate);
            },
            .e_dot_access => |dot_expr| blk: {
                if (dot_expr.args == null) return;
                if (dotCallUsesRuntimeReceiver(module_env, dot_expr.receiver)) {
                    _ = try self.resolveExprMonotypeResolved(result, module_idx, dot_expr.receiver);
                    break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, dot_expr.field_name);
                }
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, dot_expr.field_name);
            },
            .e_type_var_dispatch => |dispatch_expr| blk: {
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, dispatch_expr.method_name);
            },
            else => return,
        };
        const target_def = try self.resolveDispatchTargetToExternalDef(module_idx, resolved_target);
        try self.recordDispatchExprTarget(
            result,
            self.currentSourceContext(),
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
            if (self.active_bindings == null) {
                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, resolved_target.fn_var);
                if (!fn_monotype.isNone()) {
                    break :blk try self.ensureCallableInstUnscanned(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
                }
            }

            if (try self.specializeDispatchExactCallable(result, module_idx, expr_idx, expr, template_id)) |exact_callable_inst| {
                break :blk exact_callable_inst;
            }

            const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, resolved_target.fn_var);
            if (!fn_monotype.isNone()) {
                break :blk try self.ensureCallableInstUnscanned(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
            }

            return;
        };
        try self.recordDispatchExprCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
    }

    fn recordDispatchExprCallableInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        if (self.scratch_context_expr_monotypes_depth == 0) {
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
                self.currentSourceContext(),
                module_idx,
                expr_idx,
                dispatch_target,
            );
            const callable_inst = result.getCallableInst(callable_inst_id);
            const template = result.getCallableTemplate(callable_inst.template);
            try self.installExprDirectCallable(
                result,
                self.currentSourceContext(),
                template.module_idx,
                template.cir_expr,
                callable_inst_id,
            );
        }
        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);
        try self.prepareCallableArgsForCallableInst(result, module_idx, actual_args.items, callable_inst_id);
        try self.scanCallableInst(result, callable_inst_id);
        try self.ensureCallableArgCallableInstsScannedSlice(result, module_idx, actual_args.items);
        try self.bindCurrentDispatchFromCallableInst(result, module_idx, expr_idx, expr, callable_inst_id);
    }

    fn dispatchTargetForCallableInst(
        self: *Pass,
        result: *const Result,
        callable_inst_id: CallableInstId,
    ) ?DispatchExprTarget {
        _ = self;
        const callable_inst = result.getCallableInst(callable_inst_id);
        const template = result.getCallableTemplate(callable_inst.template);
        const external_def = template.external_def orelse return null;
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
        if (self.scratch_context_expr_monotypes_depth != 0) return;
        try self.putTracked(
            .context_dispatch_targets,
            &result.context_mono.resolved_dispatch_targets,
            self.resultExprKeyForSourceContext(source_context, module_idx, expr_idx),
            dispatch_target,
        );
    }

    fn dotDispatchHandledWithoutCallableInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        dot_expr: @TypeOf(@as(CIR.Expr, undefined).e_dot_access),
    ) Allocator.Error!bool {
        const module_env = self.all_module_envs[module_idx];
        if (!dotCallUsesRuntimeReceiver(module_env, dot_expr.receiver)) return false;
        if (!dot_expr.field_name.eql(module_env.idents.is_eq)) return false;

        const receiver_monotype = try self.resolveExprMonotype(result, module_idx, dot_expr.receiver);
        if (receiver_monotype.isNone()) {
            const eq_constraint = try self.canonicalDispatchConstraintForExpr(result, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(result, module_idx, expr_idx) == null and !constraint_resolved;
        }

        return switch (result.context_mono.monotype_store.getMonotype(receiver_monotype)) {
            .record, .tuple, .list, .unit => true,
            .tag_union => self.lookupResolvedDispatchTarget(result, module_idx, expr_idx) == null,
            else => false,
        };
    }

    fn binopDispatchHandledWithoutCallableInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        binop_expr: CIR.Expr.Binop,
    ) Allocator.Error!bool {
        if (binop_expr.op != .eq and binop_expr.op != .ne) return false;

        const module_env = self.all_module_envs[module_idx];
        const lhs_monotype = try self.resolveExprMonotype(result, module_idx, binop_expr.lhs);
        if (lhs_monotype.isNone()) {
            const eq_constraint = try self.canonicalDispatchConstraintForExpr(result, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(result, module_idx, expr_idx) == null and !constraint_resolved;
        }

        const lhs_mono = result.context_mono.monotype_store.getMonotype(lhs_monotype);
        return switch (lhs_mono) {
            .record, .tuple, .list, .unit, .prim => true,
            .tag_union => blk: {
                const cached_dispatch = self.lookupResolvedDispatchTarget(result, module_idx, expr_idx);
                break :blk cached_dispatch == null;
            },
            else => false,
        };
    }

    fn resolveBinopDispatchTarget(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        binop_expr: CIR.Expr.Binop,
        method_name: Ident.Idx,
    ) Allocator.Error!ResolvedDispatchTarget {
        const lhs_monotype = try self.resolveExprMonotypeResolved(result, module_idx, binop_expr.lhs);
        if (lhs_monotype.isNone()) {
            return self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, method_name);
        }

        var merged_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer merged_bindings.deinit();
        if (self.active_bindings) |bindings| {
            var it = bindings.iterator();
            while (it.next()) |entry| {
                try merged_bindings.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        try merged_bindings.put(
            boundTypeVarKey(module_idx, &self.all_module_envs[module_idx].types, ModuleEnv.varFrom(binop_expr.rhs)),
            lhs_monotype,
        );

        const saved_bindings = self.active_bindings;
        self.active_bindings = &merged_bindings;
        defer self.active_bindings = saved_bindings;

        return self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, method_name);
    }

    fn resolveAssociatedMethodCallableInstForTypeVar(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        receiver_type_var: types.Var,
        method_ident: Ident.Idx,
    ) Allocator.Error!?CallableInstId {
        const module_env = self.all_module_envs[module_idx];
        const receiver_nominal = resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
        const method_info = try self.lookupAssociatedMethodTemplate(result, module_idx, receiver_nominal, method_ident) orelse return null;
        const receiver_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, receiver_type_var);
        _ = try self.canonicalAssociatedMethodDispatchConstraint(
            result,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype.idx,
        ) orelse return null;
        return try self.specializeDispatchExactCallable(result, module_idx, expr_idx, expr, method_info.template_id);
    }

    fn resolveAssociatedMethodDispatchTargetForTypeVar(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        receiver_type_var: types.Var,
        method_ident: Ident.Idx,
    ) Allocator.Error!?ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const receiver_nominal = resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
        const method_info = try self.lookupAssociatedMethodTemplate(result, module_idx, receiver_nominal, method_ident) orelse return null;
        const receiver_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, receiver_type_var);
        const constraint = try self.canonicalAssociatedMethodDispatchConstraint(
            result,
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
        _ = try self.canonicalAssociatedMethodDispatchConstraint(
            result,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype,
        ) orelse return null;
        return try self.specializeDispatchExactCallable(result, module_idx, expr_idx, expr, method_info.template_id);
    }

    fn resolveAssociatedMethodDispatchTargetForMonotype(
        self: *Pass,
        result: *Result,
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
        const constraint = try self.canonicalAssociatedMethodDispatchConstraint(
            result,
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return result.context_mono.getDispatchExprTarget(
            self.currentSourceContext(),
            module_idx,
            expr_idx,
        );
    }

    fn canonicalDispatchConstraintForExpr(
        self: *Pass,
        result: *Result,
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
                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, constraint.fn_var);
                if (!fn_monotype.isNone() and
                    !try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, resolved_target, fn_monotype.idx))
                {
                    continue;
                }
                if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, module_idx, expr_idx, resolved_target)) continue;
            } else {
                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, constraint.fn_var);
                if (fn_monotype.isNone()) continue;
            }

            matched = if (matched) |existing|
                try self.mergeEquivalentDispatchConstraintForExpr(
                    result,
                    module_idx,
                    expr_idx,
                    method_name,
                    existing,
                    constraint,
                )
            else
                constraint;
        }

        return matched;
    }

    fn mergeEquivalentDispatchConstraintForExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
        existing: types.StaticDispatchConstraint,
        next_constraint: types.StaticDispatchConstraint,
    ) Allocator.Error!types.StaticDispatchConstraint {
        if (staticDispatchConstraintsEqual(existing, next_constraint) or
            try self.dispatchConstraintsEquivalent(result, module_idx, existing, next_constraint))
        {
            return if (!existing.resolved_target.isNone())
                existing
            else
                next_constraint;
        }

        const existing_mono = try self.resolveTypeVarMonotypeResolved(result, module_idx, existing.fn_var);
        const next_mono = try self.resolveTypeVarMonotypeResolved(result, module_idx, next_constraint.fn_var);

        if (!existing_mono.isNone() and !next_mono.isNone() and
            try self.monotypesStructurallyEqualAcrossModules(
                result,
                existing_mono.idx,
                existing_mono.module_idx,
                next_mono.idx,
                next_mono.module_idx,
            ))
        {
            const module_env = self.all_module_envs[module_idx];
            const existing_target_usable = !existing.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, existing.resolved_target);
            const next_target_usable = !next_constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, next_constraint.resolved_target);

            if (existing_target_usable and next_target_usable and
                !existing.resolved_target.origin_module.eql(next_constraint.resolved_target.origin_module))
            {
                std.debug.panic(
                    "Pipeline invariant violated: dispatch expr={d} method='{s}' had conflicting resolved targets for the same function monotype",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }

            if (existing_target_usable) return existing;
            if (next_target_usable) return next_constraint;
            return existing;
        }

        const module_env = self.all_module_envs[module_idx];
        std.debug.panic(
            "Pipeline invariant violated: dispatch expr={d} method='{s}' had multiple non-equivalent surviving static dispatch constraints",
            .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
        );
    }

    fn canonicalAssociatedMethodDispatchConstraint(
        self: *Pass,
        result: *Result,
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

                const fn_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, constraint.fn_var);
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

            matched = if (matched) |existing|
                try self.mergeEquivalentAssociatedMethodDispatchConstraint(
                    result,
                    module_idx,
                    expr_idx,
                    method_name,
                    existing,
                    constraint,
                )
            else
                constraint;
        }

        return matched;
    }

    fn mergeEquivalentAssociatedMethodDispatchConstraint(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
        existing: types.StaticDispatchConstraint,
        next_constraint: types.StaticDispatchConstraint,
    ) Allocator.Error!types.StaticDispatchConstraint {
        if (staticDispatchConstraintsEqual(existing, next_constraint) or
            try self.dispatchConstraintsEquivalent(result, module_idx, existing, next_constraint))
        {
            return existing;
        }

        const existing_exact = try self.resolveTypeVarMonotypeResolved(result, module_idx, existing.fn_var);
        const next_exact = try self.resolveTypeVarMonotypeResolved(result, module_idx, next_constraint.fn_var);

        if (!existing_exact.isNone() and !next_exact.isNone()) {
            if (!try self.monotypesStructurallyEqualAcrossModules(
                result,
                existing_exact.idx,
                existing_exact.module_idx,
                next_exact.idx,
                next_exact.module_idx,
            )) {
                const module_env = self.all_module_envs[module_idx];
                std.debug.panic(
                    "Pipeline invariant violated: associated dispatch expr={d} method='{s}' had conflicting exact function monotypes",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }
        }

        // After `canonicalAssociatedMethodDispatchConstraint` filtering, all
        // surviving constraints refer to this same associated method target.
        // Differences in remaining checker bookkeeping are not semantic.
        return existing;
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
        module_idx: u32,
        lhs: types.StaticDispatchConstraint,
        rhs: types.StaticDispatchConstraint,
    ) Allocator.Error!bool {
        if (!lhs.fn_name.eql(rhs.fn_name)) return false;
        if (lhs.source_expr_idx != rhs.source_expr_idx) return false;
        if (!lhs.resolved_target.origin_module.eql(rhs.resolved_target.origin_module)) return false;
        if (!lhs.resolved_target.method_ident.eql(rhs.resolved_target.method_ident)) return false;

        const lhs_mono = try self.resolveTypeVarMonotypeResolved(result, module_idx, lhs.fn_var);
        const rhs_mono = try self.resolveTypeVarMonotypeResolved(result, module_idx, rhs.fn_var);
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) Allocator.Error!ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const constraint = try self.canonicalDispatchConstraintForExpr(result, module_idx, expr_idx, method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: no static dispatch constraint for expr={d} method='{s}'",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }
            unreachable;
        };

        const desired_func_monotype = try self.resolveTypeVarMonotypeResolved(result, module_idx, constraint.fn_var);
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
        if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, module_idx, expr_idx, resolved_target)) {
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
        const target_mono = try self.resolveExprMonotype(result, target_def.module_idx, def.expr);
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
            const actual_mono = try self.resolveExprMonotypeResolved(result, source_module_idx, arg_expr_idx);
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

        const actual_ret = try self.resolveExprMonotypeResolved(result, source_module_idx, expr_idx);
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
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: CallableTemplateId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const desired_fn_monotype = try self.resolveExprMonotypeResolved(result, module_idx, expr_idx);
        const existing_callable_inst_id = if (module_env.store.getExpr(expr_idx) == .e_lookup_local) blk: {
            const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
            if (self.resolveBoundPatternCallableMembersInCurrentContext(result, module_idx, lookup.pattern_idx)) |callable_inst_ids| {
                if (callable_inst_ids.len == 0) unreachable;
                if (callable_inst_ids.len == 1) {
                    break :blk callable_inst_ids[0];
                }
                try self.installExprCallableValue(
                    result,
                    self.currentSourceContext(),
                    module_idx,
                    expr_idx,
                    callable_inst_ids,
                );
                return;
            }
            break :blk null;
        } else null;

        const callable_inst_id = if (existing_callable_inst_id) |callable_inst_id| blk: {
            if (self.scratch_context_expr_monotypes_depth == 0) {
                try self.scanCallableInst(result, callable_inst_id);
            }
            break :blk callable_inst_id;
        } else blk: {
            if (desired_fn_monotype.isNone()) return;
            const template = result.getCallableTemplate(template_id).*;
            const defining_source_context = self.resolveTemplateDefiningSourceContext(result, template);
            if (!try self.procSignatureAcceptsFnMonotype(
                result,
                template_id,
                template,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                sourceContextCallableInst(defining_source_context),
            )) {
                return;
            }
            break :blk if (self.scratch_context_expr_monotypes_depth == 0)
                try self.ensureCallableInst(result, template_id, desired_fn_monotype.idx, desired_fn_monotype.module_idx)
            else
                try self.ensureCallableInstUnscanned(result, template_id, desired_fn_monotype.idx, desired_fn_monotype.module_idx);
        };
        try self.installExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            &.{callable_inst_id},
        );
        if (module_env.store.getExpr(expr_idx) == .e_lookup_local) {
            try self.installCallableParamValue(
                result,
                self.currentSourceContext(),
                module_idx,
                module_env.store.getExpr(expr_idx).e_lookup_local.pattern_idx,
                &.{callable_inst_id},
            );
        }
        const template = result.getCallableTemplate(template_id).*;
        try self.installExprDirectCallable(
            result,
            self.currentSourceContext(),
            template.module_idx,
            template.cir_expr,
            callable_inst_id,
        );
        try self.propagateLookupSourceExprCallableValue(result, module_idx, expr_idx, callable_inst_id);
    }

    fn propagateLookupSourceExprCallableValue(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        lookup_expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const source: ?ExprSource = switch (module_env.store.getExpr(lookup_expr_idx)) {
            .e_lookup_local => |lookup| result.getPatternSourceExpr(module_idx, lookup.pattern_idx),
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                break :blk try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                break :blk try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx);
            },
            else => null,
        };
        const source_expr = source orelse return;
        try self.installExprDirectCallable(
            result,
            self.currentSourceContext(),
            source_expr.module_idx,
            source_expr.expr_idx,
            callable_inst_id,
        );
    }

    fn internCallableMemberSet(
        self: *Pass,
        result: *Result,
        members: []const CallableInstId,
    ) Allocator.Error!Lambdamono.CallableMemberSetId {
        for (result.lambdamono.callable_member_sets.items, 0..) |_, idx| {
            const existing_members = result.lambdamono.getCallableMemberSetMembers(@enumFromInt(idx));
            if (existing_members.len != members.len) continue;

            var matches = true;
            for (existing_members, members) |lhs, rhs| {
                if (lhs != rhs) {
                    matches = false;
                    break;
                }
            }
            if (matches) return @enumFromInt(idx);
        }

        const span: Lambdamono.CallableMemberSpan = if (members.len == 0)
            Lambdamono.CallableMemberSpan.empty()
        else blk: {
            const start: u32 = @intCast(result.lambdamono.callable_member_entries.items.len);
            try result.lambdamono.callable_member_entries.appendSlice(self.allocator, members);
            break :blk .{
                .start = start,
                .len = @intCast(members.len),
            };
        };

        const set_id: Lambdamono.CallableMemberSetId = @enumFromInt(result.lambdamono.callable_member_sets.items.len);
        const new_set: @TypeOf(result.lambdamono.callable_member_sets.items[0]) = .{ .members = span };
        try self.appendTracked(.callable_member_sets, &result.lambdamono.callable_member_sets, new_set);
        return set_id;
    }

    fn appendUniqueCallableInsts(
        self: *Pass,
        merged: *std.ArrayList(CallableInstId),
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        for (callable_inst_ids) |callable_inst_id| {
            for (merged.items) |existing_callable_inst_id| {
                if (existing_callable_inst_id == callable_inst_id) break;
            } else {
                try merged.append(self.allocator, callable_inst_id);
            }
        }
    }

    fn existingCallableValueMembers(
        self: *Pass,
        result: *const Result,
        callable_value: CallableValue,
        merged: *std.ArrayList(CallableInstId),
    ) Allocator.Error!void {
        switch (callable_value) {
            .direct => |callable_inst_id| try merged.append(self.allocator, callable_inst_id),
            .packed_callable => |packed_callable_id| try merged.appendSlice(self.allocator, result.getPackedCallableMembers(packed_callable_id)),
        }
    }

    fn existingCallDispatchMembers(
        self: *Pass,
        result: *const Result,
        call_dispatch: CallDispatch,
        merged: *std.ArrayList(CallableInstId),
    ) Allocator.Error!void {
        switch (call_dispatch) {
            .direct => |callable_inst_id| try merged.append(self.allocator, callable_inst_id),
            .dispatch_call => |dispatch_call_id| try merged.appendSlice(self.allocator, result.getDispatchCallMembers(dispatch_call_id)),
        }
    }

    fn installExprCallDispatch(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        if (callable_inst_ids.len == 0) unreachable;

        var merged = std.ArrayList(CallableInstId).empty;
        defer merged.deinit(self.allocator);
        if (self.readExprCallDispatch(result, source_context, module_idx, expr_idx)) |existing_call_dispatch| {
            try self.existingCallDispatchMembers(result, existing_call_dispatch, &merged);
        }
        try self.appendUniqueCallableInsts(&merged, callable_inst_ids);
        std.mem.sortUnstable(CallableInstId, merged.items, {}, struct {
            fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                return @intFromEnum(lhs) < @intFromEnum(rhs);
            }
        }.lessThan);
        try self.ensureRecordedCallableInstsScanned(result, callable_inst_ids);
        if (merged.items.len == 1) {
            try self.writeExprCallDispatch(result, source_context, module_idx, expr_idx, .{ .direct = merged.items[0] });
        } else {
            const callable_member_set_id = try self.internCallableMemberSet(result, merged.items);
            const dispatch_call_id = try self.ensureDispatchCall(result, callable_member_set_id);
            try self.writeExprCallDispatch(result, source_context, module_idx, expr_idx, .{ .dispatch_call = dispatch_call_id });
        }
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

    fn installExprDirectCallDispatch(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.installExprCallDispatch(result, source_context, module_idx, expr_idx, &.{callable_inst_id});
    }

    fn installCallableParamDirectValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.installCallableParamValue(
            result,
            source_context,
            module_idx,
            pattern_idx,
            &.{callable_inst_id},
        );
    }

    fn installCallableParamDirectValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        return self.installCallableParamDirectValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
            callable_inst_id,
        );
    }

    fn installCallableParamValueInCallableContext(
        self: *Pass,
        result: *Result,
        context_callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        return self.installCallableParamValue(
            result,
            callableInstSourceContext(context_callable_inst),
            module_idx,
            pattern_idx,
            callable_inst_ids,
        );
    }

    fn installCallableParamValue(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        if (callable_inst_ids.len == 0) unreachable;
        if (!sourceContextHasCallableInst(source_context)) return;

        var merged = std.ArrayList(CallableInstId).empty;
        defer merged.deinit(self.allocator);
        if (self.readCallableParamValue(result, source_context, module_idx, pattern_idx)) |existing_callable_value| {
            try self.existingCallableValueMembers(result, existing_callable_value, &merged);
        }
        try self.appendUniqueCallableInsts(&merged, callable_inst_ids);
        std.mem.sortUnstable(CallableInstId, merged.items, {}, struct {
            fn lessThan(_: void, lhs: CallableInstId, rhs: CallableInstId) bool {
                return @intFromEnum(lhs) < @intFromEnum(rhs);
            }
        }.lessThan);
        try self.ensureRecordedCallableInstsScanned(result, callable_inst_ids);
        if (merged.items.len == 1) {
            try self.writeCallableParamValue(result, source_context, module_idx, pattern_idx, .{ .direct = merged.items[0] });
        } else {
            const callable_member_set_id = try self.internCallableMemberSet(result, merged.items);
            const packed_callable_id = try self.ensurePackedCallable(result, callable_member_set_id);
            try self.writeCallableParamValue(result, source_context, module_idx, pattern_idx, .{ .packed_callable = packed_callable_id });
        }
    }

    fn ensurePackedCallable(
        self: *Pass,
        result: *Result,
        callable_member_set_id: Lambdamono.CallableMemberSetId,
    ) Allocator.Error!PackedCallableId {
        if (result.lambdamono.packed_callable_ids_by_member_set.get(callable_member_set_id)) |existing| return existing;

        const fn_monotype = try self.requireCallableMemberSetFnMonotype(
            result,
            result.lambdamono.getCallableMemberSetMembers(callable_member_set_id),
        );
        const packed_callable_id: PackedCallableId = @enumFromInt(result.lambdamono.packed_callables.items.len);
        const packed_callable: @TypeOf(result.lambdamono.packed_callables.items[0]) = .{
            .members = result.lambdamono.callable_member_sets.items[@intFromEnum(callable_member_set_id)].members,
            .fn_monotype = fn_monotype,
        };
        try self.appendTracked(.packed_callables, &result.lambdamono.packed_callables, packed_callable);
        try self.putTracked(
            .packed_callable_ids_by_member_set,
            &result.lambdamono.packed_callable_ids_by_member_set,
            callable_member_set_id,
            packed_callable_id,
        );
        return packed_callable_id;
    }

    fn requireCallableMemberSetFnMonotype(
        self: *Pass,
        result: *Result,
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
                    "Pipeline invariant violated: callable member set contained mismatched fn monotypes between callable insts {d} and {d}",
                    .{ @intFromEnum(callable_inst_ids[0]), @intFromEnum(callable_inst_id) },
                );
            }
        }
        return first_resolved;
    }

    fn ensureDispatchCall(
        self: *Pass,
        result: *Result,
        callable_member_set_id: Lambdamono.CallableMemberSetId,
    ) Allocator.Error!DispatchCallId {
        if (result.lambdamono.dispatch_call_ids_by_member_set.get(callable_member_set_id)) |existing| return existing;

        const dispatch_call_id: DispatchCallId = @enumFromInt(result.lambdamono.dispatch_calls.items.len);
        const dispatch_call: @TypeOf(result.lambdamono.dispatch_calls.items[0]) = .{
            .members = result.lambdamono.callable_member_sets.items[@intFromEnum(callable_member_set_id)].members,
        };
        try self.appendTracked(.dispatch_calls, &result.lambdamono.dispatch_calls, dispatch_call);
        try self.putTracked(
            .dispatch_call_ids_by_member_set,
            &result.lambdamono.dispatch_call_ids_by_member_set,
            callable_member_set_id,
            dispatch_call_id,
        );
        return dispatch_call_id;
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

        _ = try self.ensureCallableInst(result, template_id, fn_monotype, source_module_idx);
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

        try self.seedTypeScopeBindingsInStore(result, module_idx, store_types, &exact_specializations);
        try self.bindNamedTypeScopeMatchInStore(result, module_idx, store_types, store_types.resolveVar(var_), &exact_specializations);

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.module_idx = module_idx;
        scratches.all_module_envs = self.all_module_envs;
        return result.context_mono.monotype_store.fromTypeVar(
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

    fn seedTypeScopeBindingsInStore(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ) Allocator.Error!void {
        const type_scope = self.type_scope orelse return;
        const type_scope_module_idx = self.type_scope_module_idx orelse return;
        const caller_module_idx = self.type_scope_caller_module_idx orelse return;

        if (module_idx != type_scope_module_idx) return;

        const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common);
        const caller_types = &self.all_module_envs[caller_module_idx].types;

        for (type_scope.scopes.items) |*scope| {
            var it = scope.iterator();
            while (it.next()) |entry| {
                const platform_var = entry.key_ptr.*;
                const caller_var = entry.value_ptr.*;
                const caller_mono = try self.monotypeFromTypeVarInStore(result, caller_module_idx, caller_types, caller_var);
                if (caller_mono.isNone()) continue;
                const normalized_mono = if (caller_module_idx == module_idx)
                    caller_mono
                else
                    try self.remapMonotypeBetweenModules(result, caller_mono, caller_module_idx, module_idx);
                const type_scope_key: BoundTypeVarKey = .{
                    .module_idx = module_idx,
                    .type_var = platform_var,
                };
                if (result.context_mono.type_scope_monotypes.get(type_scope_key)) |existing| {
                    if (!try self.monotypesStructurallyEqualAcrossModules(
                        result,
                        existing.idx,
                        existing.module_idx,
                        normalized_mono,
                        module_idx,
                    )) {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "Pipeline: conflicting exact type-scope monotype for module={d} type_var={d}",
                                .{ module_idx, @intFromEnum(platform_var) },
                            );
                        }
                        unreachable;
                    }
                } else {
                    try result.context_mono.type_scope_monotypes.put(
                        self.allocator,
                        type_scope_key,
                        resolvedMonotype(normalized_mono, module_idx),
                    );
                }
                try self.bindTypeVarMonotypesInStore(
                    result,
                    module_idx,
                    store_types,
                    common_idents,
                    bindings,
                    platform_var,
                    normalized_mono,
                );
            }
        }
    }

    fn bindNamedTypeScopeMatchInStore(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        resolved: types.store.ResolvedVarDesc,
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ) Allocator.Error!void {
        const type_scope = self.type_scope orelse return;
        const type_scope_module_idx = self.type_scope_module_idx orelse return;
        const caller_module_idx = self.type_scope_caller_module_idx orelse return;

        if (module_idx != type_scope_module_idx) return;
        if (bindings.contains(resolved.var_)) return;

        const current_name = switch (resolved.desc.content) {
            .rigid => |rigid| rigid.name,
            .flex => |flex| flex.name orelse return,
            else => return,
        };

        const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[module_idx].common);
        const caller_types = &self.all_module_envs[caller_module_idx].types;

        for (type_scope.scopes.items) |*scope| {
            var it = scope.iterator();
            while (it.next()) |entry| {
                const platform_resolved = store_types.resolveVar(entry.key_ptr.*);
                const platform_name = switch (platform_resolved.desc.content) {
                    .rigid => |rigid| rigid.name,
                    .flex => |flex| flex.name orelse continue,
                    else => continue,
                };
                if (!platform_name.eql(current_name)) continue;

                const caller_mono = try self.monotypeFromTypeVarInStore(result, caller_module_idx, caller_types, entry.value_ptr.*);
                if (caller_mono.isNone()) continue;
                const normalized_mono = if (caller_module_idx == module_idx)
                    caller_mono
                else
                    try self.remapMonotypeBetweenModules(result, caller_mono, caller_module_idx, module_idx);

                try self.bindTypeVarMonotypesInStore(
                    result,
                    module_idx,
                    store_types,
                    common_idents,
                    bindings,
                    resolved.var_,
                    normalized_mono,
                );
                return;
            }
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

        try self.seedTypeScopeBindingsInStore(result, module_idx, store_types, &specializations);
        try self.bindNamedTypeScopeMatchInStore(result, module_idx, store_types, store_types.resolveVar(var_), &specializations);

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.module_idx = module_idx;
        scratches.all_module_envs = self.all_module_envs;

        return result.context_mono.monotype_store.fromTypeVar(
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
        module_idx: u32,
        type_var: types.Var,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer visiting.deinit(self.allocator);
        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, type_var, &visiting);
    }

    fn resolveStrInspectHelperCallableInstsForTypeVarWithSeen(
        self: *Pass,
        result: *Result,
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
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, type_args[0], visiting);
                            }
                            return;
                        }
                        if (ident.eql(common.box)) {
                            const type_args = module_env.types.sliceNominalArgs(nominal);
                            const outer_mono = try self.resolveTypeVarMonotype(result, module_idx, resolved.var_);
                            const outer_box = result.context_mono.monotype_store.getMonotype(outer_mono).box;
                            try self.ensureBuiltinBoxUnboxCallableInst(result, module_idx, outer_mono, outer_box.inner);
                            if (type_args.len == 1) {
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, type_args[0], visiting);
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

                                    const arg_mono = try self.resolveTypeVarMonotype(result, module_idx, resolved.var_);
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
                                        _ = try self.ensureCallableInst(
                                            result,
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
                        try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
                    );
                    return;
                },
                .record => |record| {
                    try self.resolveStrInspectHelperCallableInstsForRecordType(result, module_idx, &module_env.types, record, visiting);
                    return;
                },
                .record_unbound => |fields_range| {
                    const fields = module_env.types.getRecordFieldsSlice(fields_range);
                    for (fields.items(.var_)) |field_var| {
                        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
                    }
                    return;
                },
                .tuple => |tuple| {
                    for (module_env.types.sliceVars(tuple.elems)) |elem_var| {
                        try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, elem_var, visiting);
                    }
                    return;
                },
                .tag_union => |tag_union| {
                    try self.resolveStrInspectHelperCallableInstsForTagUnionType(result, module_idx, &module_env.types, tag_union, visiting);
                    return;
                },
                .empty_record, .empty_tag_union => return,
                else => {},
            }
        }

        try self.resolveStrInspectHelperCallableInstsForMonotype(
            result,
            module_idx,
            try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
        );
    }

    fn resolveStrInspectHelperCallableInstsForRecordType(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        record: types.Record,
        visiting: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!void {
        var current_row = record;

        rows: while (true) {
            const fields = store_types.getRecordFieldsSlice(current_row.fields);
            for (fields.items(.var_)) |field_var| {
                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
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
                                try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
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
                    try self.resolveStrInspectHelperCallableInstsForTypeVarWithSeen(result, module_idx, payload_var, visiting);
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

    fn resolveExprCallableTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!?CallableTemplateId {
        var visiting: std.AutoHashMapUnmanaged(ContextExprVisitKey, void) = .empty;
        defer visiting.deinit(self.allocator);
        return self.resolveExprCallableTemplateWithVisited(result, module_idx, expr_idx, &visiting);
    }

    fn resolveExternalDefSourceExpr(
        self: *Pass,
        result: *Result,
        target_module_idx: u32,
        target_node_idx: u16,
    ) Allocator.Error!?ExprSource {
        try self.scanModule(result, target_module_idx);

        const key = packExternalDefSourceKey(target_module_idx, target_node_idx);
        if (result.lambdasolved.source_exprs.get(key)) |source| return source;

        const target_env = self.all_module_envs[target_module_idx];
        if (!target_env.store.isDefNode(target_node_idx)) return null;

        const def_idx: CIR.Def.Idx = @enumFromInt(target_node_idx);
        const def = target_env.store.getDef(def_idx);
        const source: ExprSource = .{
            .module_idx = target_module_idx,
            .expr_idx = def.expr,
        };
        try self.recordSourceExpr(result, key, target_module_idx, def.expr);
        _ = try self.registerCallableBackedDefTemplate(
            result,
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            key,
        );
        return source;
    }

    fn resolveRecordFieldSourceExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        field_name_module_idx: u32,
        field_name: Ident.Idx,
    ) Allocator.Error!?ExprSource {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        return self.resolveRecordFieldExpr(
            result,
            module_idx,
            expr_idx,
            field_name_module_idx,
            field_name,
            &visiting,
        );
    }

    fn resolveTupleElemSourceExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        elem_index: u32,
    ) Allocator.Error!?ExprSource {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        return self.resolveTupleElemExpr(result, module_idx, expr_idx, elem_index, &visiting);
    }

    fn resolveTagPayloadSourceExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        tag_name_module_idx: u32,
        tag_name: Ident.Idx,
        payload_index: u32,
    ) Allocator.Error!?ExprSource {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        return self.resolveTagPayloadExpr(
            result,
            module_idx,
            expr_idx,
            tag_name_module_idx,
            tag_name,
            payload_index,
            &visiting,
        );
    }

    fn resolveListElemSourceExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        elem_index: u32,
    ) Allocator.Error!?ExprSource {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        return self.resolveListElemExpr(result, module_idx, expr_idx, elem_index, &visiting);
    }

    fn resolveRecordFieldExprInContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        field_name_module_idx: u32,
        field_name: Ident.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!ContextExprResolution {
        const visit_key = contextExprVisitKey(source_context, module_idx, expr_idx);
        if (visiting.contains(visit_key)) return .none;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_record => |record| blk: {
                for (module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (self.identsStructurallyEqualAcrossModules(module_idx, field.name, field_name_module_idx, field_name)) {
                        break :blk .{ .source = .{
                            .source_context = source_context,
                            .module_idx = module_idx,
                            .expr_idx = field.value,
                        } };
                    }
                }
                break :blk .none;
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk .none;
                break :blk try self.resolveRecordFieldExprInContext(
                    result,
                    source_context,
                    source.module_idx,
                    source.expr_idx,
                    field_name_module_idx,
                    field_name,
                    visiting,
                );
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk .none;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk .none;
                break :blk try self.resolveRecordFieldExprInContext(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    field_name_module_idx,
                    field_name,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk .none;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk .none;
                break :blk try self.resolveRecordFieldExprInContext(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    field_name_module_idx,
                    field_name,
                    visiting,
                );
            },
            .e_block => |block| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, block.final_expr, field_name_module_idx, field_name, visiting),
            .e_dbg => |dbg_expr| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, dbg_expr.expr, field_name_module_idx, field_name, visiting),
            .e_expect => |expect_expr| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, expect_expr.body, field_name_module_idx, field_name, visiting),
            .e_return => |return_expr| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, return_expr.expr, field_name_module_idx, field_name, visiting),
            .e_nominal => |nominal_expr| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, nominal_expr.backing_expr, field_name_module_idx, field_name, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveRecordFieldExprInContext(result, source_context, module_idx, nominal_expr.backing_expr, field_name_module_idx, field_name, visiting),
            .e_call => |call_expr| blk: {
                if (self.getCallLowLevelOp(module_env, call_expr.func) != null) break :blk .none;
                const callee_callable_inst_id = self.getCallSiteCallableInstForSourceContext(result, source_context, module_idx, expr_idx) orelse break :blk .blocked;
                const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
                const callee_template = result.getCallableTemplate(callee_callable_inst.template);
                const boundary = self.callableBoundaryInfo(callee_template.module_idx, callee_template.cir_expr) orelse break :blk .none;
                break :blk try self.resolveRecordFieldExprInContext(
                    result,
                    callableInstSourceContext(callee_callable_inst_id),
                    callee_template.module_idx,
                    boundary.body_expr,
                    field_name_module_idx,
                    field_name,
                    visiting,
                );
            },
            else => .none,
        };
    }

    fn resolveTupleElemExprInContext(
        self: *Pass,
        result: *Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        elem_index: u32,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!ContextExprResolution {
        const visit_key = contextExprVisitKey(source_context, module_idx, expr_idx);
        if (visiting.contains(visit_key)) return .none;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_tuple => |tuple_expr| blk: {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (elem_index >= elems.len) break :blk .none;
                break :blk .{ .source = .{
                    .source_context = source_context,
                    .module_idx = module_idx,
                    .expr_idx = elems[elem_index],
                } };
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk .none;
                break :blk try self.resolveTupleElemExprInContext(
                    result,
                    source_context,
                    source.module_idx,
                    source.expr_idx,
                    elem_index,
                    visiting,
                );
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk .none;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk .none;
                break :blk try self.resolveTupleElemExprInContext(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    elem_index,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk .none;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk .none;
                break :blk try self.resolveTupleElemExprInContext(
                    result,
                    .{ .root_expr = .{ .module_idx = source.module_idx, .expr_idx = source.expr_idx } },
                    source.module_idx,
                    source.expr_idx,
                    elem_index,
                    visiting,
                );
            },
            .e_block => |block| try self.resolveTupleElemExprInContext(result, source_context, module_idx, block.final_expr, elem_index, visiting),
            .e_dbg => |dbg_expr| try self.resolveTupleElemExprInContext(result, source_context, module_idx, dbg_expr.expr, elem_index, visiting),
            .e_expect => |expect_expr| try self.resolveTupleElemExprInContext(result, source_context, module_idx, expect_expr.body, elem_index, visiting),
            .e_return => |return_expr| try self.resolveTupleElemExprInContext(result, source_context, module_idx, return_expr.expr, elem_index, visiting),
            .e_nominal => |nominal_expr| try self.resolveTupleElemExprInContext(result, source_context, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveTupleElemExprInContext(result, source_context, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            .e_call => |call_expr| blk: {
                if (self.getCallLowLevelOp(module_env, call_expr.func) != null) break :blk .none;
                const callee_callable_inst_id = self.getCallSiteCallableInstForSourceContext(result, source_context, module_idx, expr_idx) orelse break :blk .blocked;
                const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
                const callee_template = result.getCallableTemplate(callee_callable_inst.template);
                const boundary = self.callableBoundaryInfo(callee_template.module_idx, callee_template.cir_expr) orelse break :blk .none;
                break :blk try self.resolveTupleElemExprInContext(
                    result,
                    callableInstSourceContext(callee_callable_inst_id),
                    callee_template.module_idx,
                    boundary.body_expr,
                    elem_index,
                    visiting,
                );
            },
            else => .none,
        };
    }

    fn resolveExprCallableTemplateWithVisited(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!?CallableTemplateId {
        const visit_key = contextExprVisitKey(self.currentSourceContext(), module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        if (self.getCallableParamSpecCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callable_inst_ids| {
            if (uniformTemplateFromCallableInsts(result, callable_inst_ids)) |template_id| {
                return template_id;
            }
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const resolved = switch (expr) {
            .e_lambda, .e_closure, .e_hosted_lambda => blk: {
                if (result.getExprCallableTemplate(module_idx, expr_idx)) |template_id| {
                    break :blk template_id;
                }

                const kind = callableKind(expr) orelse unreachable;
                break :blk try self.registerCallableTemplate(
                    result,
                    packExprSourceKey(module_idx, expr_idx),
                    module_idx,
                    expr_idx,
                    ModuleEnv.varFrom(expr_idx),
                    null,
                    kind,
                    module_env.store.getExprRegion(expr_idx),
                );
            },
            .e_lookup_local => |lookup| blk: {
                if (result.getLocalCallableTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                    break :blk template_id;
                }
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, source.module_idx, source.expr_idx, visiting);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                if (result.getExternalCallableTemplate(target_module_idx, lookup.target_node_idx)) |template_id| {
                    break :blk template_id;
                }
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, source.module_idx, source.expr_idx, visiting);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                if (result.getExternalCallableTemplate(target.module_idx, target_node_idx)) |template_id| {
                    break :blk template_id;
                }
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, source.module_idx, source.expr_idx, visiting);
            },
            .e_block => |block| try self.resolveExprCallableTemplateWithVisited(result, module_idx, block.final_expr, visiting),
            .e_dbg => |dbg_expr| try self.resolveExprCallableTemplateWithVisited(result, module_idx, dbg_expr.expr, visiting),
            .e_expect => |expect_expr| try self.resolveExprCallableTemplateWithVisited(result, module_idx, expect_expr.body, visiting),
            .e_return => |return_expr| try self.resolveExprCallableTemplateWithVisited(result, module_idx, return_expr.expr, visiting),
            .e_nominal => |nominal_expr| try self.resolveExprCallableTemplateWithVisited(result, module_idx, nominal_expr.backing_expr, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveExprCallableTemplateWithVisited(result, module_idx, nominal_expr.backing_expr, visiting),
            .e_dot_access => |dot_expr| blk: {
                if (dot_expr.args != null) break :blk null;
                const field_expr = try self.resolveRecordFieldExpr(
                    result,
                    module_idx,
                    dot_expr.receiver,
                    module_idx,
                    dot_expr.field_name,
                    visiting,
                ) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, field_expr.module_idx, field_expr.expr_idx, visiting);
            },
            .e_tuple_access => |tuple_access| blk: {
                const elem_expr = try self.resolveTupleElemExpr(result, module_idx, tuple_access.tuple, tuple_access.elem_index, visiting) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, elem_expr.module_idx, elem_expr.expr_idx, visiting);
            },
            else => null,
        };
        return resolved;
    }

    fn uniformTemplateFromCallableInsts(
        result: *const Result,
        callable_inst_ids: []const CallableInstId,
    ) ?CallableTemplateId {
        if (callable_inst_ids.len == 0) return null;
        const template_id = result.getCallableInst(callable_inst_ids[0]).template;
        for (callable_inst_ids[1..]) |callable_inst_id| {
            if (result.getCallableInst(callable_inst_id).template != template_id) return null;
        }
        return template_id;
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

    fn projectDemandedCallableInstsToTemplate(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        callable_inst_ids: []const CallableInstId,
        out: *std.ArrayList(CallableInstId),
    ) Allocator.Error!void {
        out.clearRetainingCapacity();
        for (callable_inst_ids) |callable_inst_id| {
            if (result.getCallableInst(callable_inst_id).template != template_id) continue;
            try self.appendUniqueCallableInsts(out, &.{callable_inst_id});
        }
    }

    fn installProjectedDemandedTemplateValue(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        maybe_pattern_idx: ?CIR.Pattern.Idx,
        template_id: CallableTemplateId,
        demanded_callable_insts: []const CallableInstId,
        projected_callable_insts: *std.ArrayList(CallableInstId),
    ) Allocator.Error!bool {
        try self.projectDemandedCallableInstsToTemplate(
            result,
            template_id,
            demanded_callable_insts,
            projected_callable_insts,
        );
        if (projected_callable_insts.items.len == 0) return false;

        try self.installExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            projected_callable_insts.items,
        );
        if (maybe_pattern_idx) |pattern_idx| {
            try self.installCallableParamValue(
                result,
                self.currentSourceContext(),
                module_idx,
                pattern_idx,
                projected_callable_insts.items,
            );
        }
        return true;
    }

    fn propagateDemandedCallableMembersToValueExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_ids: []const CallableInstId,
        visiting: *std.AutoHashMapUnmanaged(ContextExprVisitKey, void),
    ) Allocator.Error!void {
        const visit_key = contextExprVisitKey(self.currentSourceContext(), module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var projected_callable_insts = std.ArrayList(CallableInstId).empty;
        defer projected_callable_insts.deinit(self.allocator);
        switch (expr) {
            .e_lambda, .e_closure, .e_hosted_lambda => {
                const template_id = (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) orelse return;
                if (try self.installProjectedDemandedTemplateValue(
                    result,
                    module_idx,
                    expr_idx,
                    null,
                    template_id,
                    callable_inst_ids,
                    &projected_callable_insts,
                )) return;
                return;
            },
            .e_lookup_local => |lookup| {
                if (result.getLocalCallableTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                    if (try self.installProjectedDemandedTemplateValue(
                        result,
                        module_idx,
                        expr_idx,
                        lookup.pattern_idx,
                        template_id,
                        callable_inst_ids,
                        &projected_callable_insts,
                    )) return;
                }

                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse return;
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    callable_inst_ids,
                    visiting,
                );
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                if (result.getExternalCallableTemplate(target_module_idx, lookup.target_node_idx)) |template_id| {
                    if (try self.installProjectedDemandedTemplateValue(
                        result,
                        module_idx,
                        expr_idx,
                        null,
                        template_id,
                        callable_inst_ids,
                        &projected_callable_insts,
                    )) return;
                }

                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse return;
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    callable_inst_ids,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                if (result.getExternalCallableTemplate(target.module_idx, target_node_idx)) |template_id| {
                    if (try self.installProjectedDemandedTemplateValue(
                        result,
                        module_idx,
                        expr_idx,
                        null,
                        template_id,
                        callable_inst_ids,
                        &projected_callable_insts,
                    )) return;
                }

                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse return;
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    callable_inst_ids,
                    visiting,
                );
            },
            .e_if => |if_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, branch.body, callable_inst_ids, visiting);
                }
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, if_expr.final_else, callable_inst_ids, visiting);
            },
            .e_match => |match_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, branch.value, callable_inst_ids, visiting);
                }
            },
            .e_block => |block_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, block_expr.final_expr, callable_inst_ids, visiting);
            },
            .e_dbg => |dbg_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, dbg_expr.expr, callable_inst_ids, visiting);
            },
            .e_expect => |expect_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, expect_expr.body, callable_inst_ids, visiting);
            },
            .e_return => |return_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, return_expr.expr, callable_inst_ids, visiting);
            },
            .e_nominal => |nominal_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, nominal_expr.backing_expr, callable_inst_ids, visiting);
            },
            .e_nominal_external => |nominal_expr| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                try self.propagateDemandedCallableMembersToValueExpr(result, module_idx, nominal_expr.backing_expr, callable_inst_ids, visiting);
            },
            .e_call => |call_expr| {
                if (self.getCallLowLevelOp(module_env, call_expr.func) != null) return;

                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);

                if (self.getCallSiteCallableInstsInCurrentContext(result, module_idx, expr_idx)) |callee_callable_inst_ids| {
                    for (callee_callable_inst_ids) |callee_callable_inst_id| {
                        try self.recordCallResultCallableInstsFromCallableInst(
                            result,
                            self.currentSourceContext(),
                            module_idx,
                            expr_idx,
                            callee_callable_inst_id,
                        );
                        const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
                        const callee_template = result.getCallableTemplate(callee_callable_inst.template);
                        const boundary = self.callableBoundaryInfo(callee_template.module_idx, callee_template.cir_expr) orelse continue;
                        {
                            try self.pushSourceContext(callableInstSourceContext(callee_callable_inst_id));
                            defer self.popSourceContext();
                            try self.propagateDemandedCallableMembersToValueExpr(
                                result,
                                callee_template.module_idx,
                                boundary.body_expr,
                                callable_inst_ids,
                                visiting,
                            );
                        }
                    }
                    return;
                }

                if (self.getCallSiteCallableInstInCurrentContext(result, module_idx, expr_idx)) |callee_callable_inst_id| {
                    try self.recordCallResultCallableInstsFromCallableInst(
                        result,
                        self.currentSourceContext(),
                        module_idx,
                        expr_idx,
                        callee_callable_inst_id,
                    );
                    const callee_callable_inst = result.getCallableInst(callee_callable_inst_id);
                    const callee_template = result.getCallableTemplate(callee_callable_inst.template);
                    const boundary = self.callableBoundaryInfo(callee_template.module_idx, callee_template.cir_expr) orelse return;
                    {
                        try self.pushSourceContext(callableInstSourceContext(callee_callable_inst_id));
                        defer self.popSourceContext();
                        try self.propagateDemandedCallableMembersToValueExpr(
                            result,
                            callee_template.module_idx,
                            boundary.body_expr,
                            callable_inst_ids,
                            visiting,
                        );
                    }
                }
            },
            .e_dot_access => |dot_expr| {
                if (dot_expr.args != null) return;
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                const field_expr = try self.resolveRecordFieldExpr(
                    result,
                    module_idx,
                    dot_expr.receiver,
                    module_idx,
                    dot_expr.field_name,
                    visiting,
                ) orelse return;
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    field_expr.module_idx,
                    field_expr.expr_idx,
                    callable_inst_ids,
                    visiting,
                );
            },
            .e_tuple_access => |tuple_access| {
                try self.installDemandedExprCallableValue(result, module_idx, expr_idx, callable_inst_ids);
                const elem_expr = try self.resolveTupleElemExpr(
                    result,
                    module_idx,
                    tuple_access.tuple,
                    tuple_access.elem_index,
                    visiting,
                ) orelse return;
                try self.propagateDemandedCallableMembersToValueExpr(
                    result,
                    elem_expr.module_idx,
                    elem_expr.expr_idx,
                    callable_inst_ids,
                    visiting,
                );
            },
            else => {},
        }
    }

    fn installDemandedExprCallableValue(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!void {
        if (callable_inst_ids.len == 0) return;
        try self.installExprCallableValue(
            result,
            self.currentSourceContext(),
            module_idx,
            expr_idx,
            callable_inst_ids,
        );
    }

    fn resolveRecordFieldExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        field_name_module_idx: u32,
        field_name: Ident.Idx,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!?ExprSource {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_record => |record| blk: {
                for (module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (self.identsStructurallyEqualAcrossModules(module_idx, field.name, field_name_module_idx, field_name)) {
                        break :blk .{ .module_idx = module_idx, .expr_idx = field.value };
                    }
                }
                break :blk null;
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveRecordFieldExpr(result, source.module_idx, source.expr_idx, field_name_module_idx, field_name, visiting);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveRecordFieldExpr(result, source.module_idx, source.expr_idx, field_name_module_idx, field_name, visiting);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk null;
                break :blk try self.resolveRecordFieldExpr(result, source.module_idx, source.expr_idx, field_name_module_idx, field_name, visiting);
            },
            .e_block => |block| try self.resolveRecordFieldExpr(result, module_idx, block.final_expr, field_name_module_idx, field_name, visiting),
            .e_dbg => |dbg_expr| try self.resolveRecordFieldExpr(result, module_idx, dbg_expr.expr, field_name_module_idx, field_name, visiting),
            .e_expect => |expect_expr| try self.resolveRecordFieldExpr(result, module_idx, expect_expr.body, field_name_module_idx, field_name, visiting),
            .e_return => |return_expr| try self.resolveRecordFieldExpr(result, module_idx, return_expr.expr, field_name_module_idx, field_name, visiting),
            .e_nominal => |nominal_expr| try self.resolveRecordFieldExpr(result, module_idx, nominal_expr.backing_expr, field_name_module_idx, field_name, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveRecordFieldExpr(result, module_idx, nominal_expr.backing_expr, field_name_module_idx, field_name, visiting),
            else => null,
        };
    }

    fn resolveTupleElemExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        elem_index: u32,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!?ExprSource {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_tuple => |tuple_expr| blk: {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (elem_index >= elems.len) break :blk null;
                break :blk .{ .module_idx = module_idx, .expr_idx = elems[elem_index] };
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveTupleElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveTupleElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk null;
                break :blk try self.resolveTupleElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_block => |block| try self.resolveTupleElemExpr(result, module_idx, block.final_expr, elem_index, visiting),
            .e_dbg => |dbg_expr| try self.resolveTupleElemExpr(result, module_idx, dbg_expr.expr, elem_index, visiting),
            .e_expect => |expect_expr| try self.resolveTupleElemExpr(result, module_idx, expect_expr.body, elem_index, visiting),
            .e_return => |return_expr| try self.resolveTupleElemExpr(result, module_idx, return_expr.expr, elem_index, visiting),
            .e_nominal => |nominal_expr| try self.resolveTupleElemExpr(result, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveTupleElemExpr(result, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            else => null,
        };
    }

    fn resolveTagPayloadExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        tag_name_module_idx: u32,
        tag_name: Ident.Idx,
        payload_index: u32,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!?ExprSource {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_tag => |tag_expr| blk: {
                if (!self.identsStructurallyEqualAcrossModules(module_idx, tag_expr.name, tag_name_module_idx, tag_name)) {
                    break :blk null;
                }
                const payloads = module_env.store.sliceExpr(tag_expr.args);
                if (payload_index >= payloads.len) break :blk null;
                break :blk .{ .module_idx = module_idx, .expr_idx = payloads[payload_index] };
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveTagPayloadExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    tag_name_module_idx,
                    tag_name,
                    payload_index,
                    visiting,
                );
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveTagPayloadExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    tag_name_module_idx,
                    tag_name,
                    payload_index,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk null;
                break :blk try self.resolveTagPayloadExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    tag_name_module_idx,
                    tag_name,
                    payload_index,
                    visiting,
                );
            },
            .e_block => |block| try self.resolveTagPayloadExpr(result, module_idx, block.final_expr, tag_name_module_idx, tag_name, payload_index, visiting),
            .e_dbg => |dbg_expr| try self.resolveTagPayloadExpr(result, module_idx, dbg_expr.expr, tag_name_module_idx, tag_name, payload_index, visiting),
            .e_expect => |expect_expr| try self.resolveTagPayloadExpr(result, module_idx, expect_expr.body, tag_name_module_idx, tag_name, payload_index, visiting),
            .e_return => |return_expr| try self.resolveTagPayloadExpr(result, module_idx, return_expr.expr, tag_name_module_idx, tag_name, payload_index, visiting),
            .e_nominal => |nominal_expr| try self.resolveTagPayloadExpr(result, module_idx, nominal_expr.backing_expr, tag_name_module_idx, tag_name, payload_index, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveTagPayloadExpr(result, module_idx, nominal_expr.backing_expr, tag_name_module_idx, tag_name, payload_index, visiting),
            else => null,
        };
    }

    fn resolveListElemExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        elem_index: u32,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!?ExprSource {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(expr_idx)) {
            .e_list => |list_expr| blk: {
                const elems = module_env.store.sliceExpr(list_expr.elems);
                if (elem_index >= elems.len) break :blk null;
                break :blk .{ .module_idx = module_idx, .expr_idx = elems[elem_index] };
            },
            .e_lookup_local => |lookup| blk: {
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveListElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveListElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse break :blk null;
                break :blk try self.resolveListElemExpr(result, source.module_idx, source.expr_idx, elem_index, visiting);
            },
            .e_block => |block| try self.resolveListElemExpr(result, module_idx, block.final_expr, elem_index, visiting),
            .e_dbg => |dbg_expr| try self.resolveListElemExpr(result, module_idx, dbg_expr.expr, elem_index, visiting),
            .e_expect => |expect_expr| try self.resolveListElemExpr(result, module_idx, expect_expr.body, elem_index, visiting),
            .e_return => |return_expr| try self.resolveListElemExpr(result, module_idx, return_expr.expr, elem_index, visiting),
            .e_nominal => |nominal_expr| try self.resolveListElemExpr(result, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            .e_nominal_external => |nominal_expr| try self.resolveListElemExpr(result, module_idx, nominal_expr.backing_expr, elem_index, visiting),
            else => null,
        };
    }

    fn lookupDirectCalleeTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        callee_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!?CallableTemplateId {
        return self.resolveExprCallableTemplate(result, module_idx, callee_expr_idx);
    }

    fn ensureCallableInst(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!CallableInstId {
        return self.ensureCallableInstWithScan(result, template_id, fn_monotype, fn_monotype_module_idx, &.{}, true);
    }

    fn ensureCallableInstUnscanned(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!CallableInstId {
        return self.ensureCallableInstWithScan(result, template_id, fn_monotype, fn_monotype_module_idx, &.{}, false);
    }

    fn ensureCallableInstUnscannedWithCallableParamSpecs(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
    ) Allocator.Error!CallableInstId {
        return self.ensureCallableInstWithScan(
            result,
            template_id,
            fn_monotype,
            fn_monotype_module_idx,
            callable_param_specs,
            false,
        );
    }

    fn ensureCallableInstForSourceMember(
        self: *Pass,
        result: *Result,
        source_member: Lambdasolved.LambdaSetMemberId,
    ) Allocator.Error!CallableInstId {
        if (result.lambdamono.getCallableInstForSourceMember(source_member)) |existing| return existing;

        const member = result.lambdasolved.getLambdaSetMember(source_member).*;

        try self.pushSourceContext(member.source_context);
        defer self.popSourceContext();

        const callable_inst_id = try self.ensureCallableInst(
            result,
            member.template,
            member.fn_monotype.idx,
            member.fn_monotype.module_idx,
        );
        const callable_def = result.getCallableDefForInst(callable_inst_id);
        if (callable_def.source_member != source_member) {
            std.debug.panic(
                "Pipeline invariant violated: source member {d} materialized callable inst {d} with mismatched source member {d}",
                .{
                    @intFromEnum(source_member),
                    @intFromEnum(callable_inst_id),
                    @intFromEnum(callable_def.source_member),
                },
            );
        }
        return callable_inst_id;
    }

    fn ensureCallableInstWithScan(
        self: *Pass,
        result: *Result,
        template_id: CallableTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        callable_param_specs: []const CallableParamSpecEntry,
        scan_body: bool,
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
        const defining_source_context = self.resolveTemplateDefiningSourceContext(result, template.*);
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
                result.getCallableParamSpecEntries(existing_callable_inst.callable_param_specs),
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
                try self.ensureSingletonCallableMemberSet(result, existing_id);
                if (scan_body) {
                    try self.scanCallableInst(result, existing_id);
                }
                return existing_id;
            }
        }

        const callable_param_spec_span = try self.addCallableParamSpecEntries(result, callable_param_specs);
        const callable_inst_id: CallableInstId = @enumFromInt(result.lambdamono.callable_insts.items.len);
        const callable_def_id: CallableDefId = @enumFromInt(result.lambdamono.callable_defs.items.len);
        const member_source_context = defining_source_context;
        const source_member = try self.ensureCallableSourceMember(
            result,
            member_source_context,
            template_id,
            resolvedMonotype(canonical_fn_monotype, canonical_fn_monotype_module_idx),
            result.lambdasolved.getEmptyCapturePlanId(),
            solvedCallableKindForRuntimeValue(template.kind, .direct_lambda),
        );
        try result.lambdamono.callable_defs.append(self.allocator, .{
            .source_member = source_member,
            .module_idx = template.module_idx,
            .runtime_expr = try self.ensureProgramExpr(
                result,
                callableInstSourceContext(callable_inst_id),
                template.module_idx,
                template.runtime_expr,
            ),
            .body_expr = try self.ensureProgramExpr(
                result,
                callableInstSourceContext(callable_inst_id),
                template.module_idx,
                template.cir_expr,
            ),
            .fn_monotype = resolvedMonotype(canonical_fn_monotype, canonical_fn_monotype_module_idx),
            .param_bindings = .empty(),
            .captures = .empty(),
            .source_region = template.source_region,
        });
        try self.appendTracked(.callable_insts, &result.lambdamono.callable_insts, CallableInst{
            .template = template_id,
            .subst = subst_id,
            .fn_monotype = canonical_fn_monotype,
            .fn_monotype_module_idx = canonical_fn_monotype_module_idx,
            .defining_source_context = defining_source_context,
            .callable_param_specs = callable_param_spec_span,
            .callable_def = callable_def_id,
            .runtime_value = .direct_lambda,
        });
        try result.lambdamono.callable_inst_ids_by_source_member.put(
            self.allocator,
            source_member,
            callable_inst_id,
        );
        try self.ensureSingletonCallableMemberSet(result, callable_inst_id);
        if (scan_body) {
            try self.scanCallableInst(result, callable_inst_id);
        }
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

    fn addCallableParamProjectionEntries(
        self: *Pass,
        result: *Result,
        entries: []const CallableParamProjection,
    ) Allocator.Error!CallableParamProjectionSpan {
        if (entries.len == 0) return .empty();

        const start: u32 = @intCast(result.lambdamono.callable_param_projection_entries.items.len);
        try result.lambdamono.callable_param_projection_entries.appendSlice(self.allocator, entries);
        return .{
            .start = start,
            .len = @intCast(entries.len),
        };
    }

    fn ensureSingletonCallableMemberSet(
        self: *Pass,
        result: *Result,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        if (result.lambdamono.singleton_callable_member_set_ids_by_callable_inst.contains(callable_inst_id)) return;
        const callable_member_set_id = try self.internCallableMemberSet(result, &.{callable_inst_id});
        try result.lambdamono.singleton_callable_member_set_ids_by_callable_inst.put(self.allocator, callable_inst_id, callable_member_set_id);
    }

    fn appendCallableParamSpecEntry(
        self: *Pass,
        result: *Result,
        out: *std.ArrayListUnmanaged(CallableParamSpecEntry),
        entry: CallableParamSpecEntry,
    ) Allocator.Error!void {
        for (out.items) |existing| {
            if (existing.param_index != entry.param_index) continue;
            if (existing.callable_member_set_id != entry.callable_member_set_id) continue;
            if (!callableParamProjectionsEqual(
                result.getCallableParamProjectionEntries(existing.projections),
                result.getCallableParamProjectionEntries(entry.projections),
            )) continue;
            return;
        }
        try out.append(self.allocator, entry);
    }

    fn resolveTemplateDefiningSourceContext(
        self: *Pass,
        result: *const Result,
        template: CallableTemplate,
    ) SourceContext {
        switch (template.kind) {
            .top_level_def, .lambda, .hosted_lambda => return templateSourceContext(template),
            .closure => {
                const lexical_owner_template = template.lexical_owner_template orelse std.debug.panic(
                    "Pipeline: closure template expr={d} had no lexical owner template",
                    .{@intFromEnum(template.cir_expr)},
                );
                var current = switch (self.currentSourceContext()) {
                    .callable_inst => |context_id| @as(CallableInstId, @enumFromInt(@intFromEnum(context_id))),
                    .root_expr, .provenance_expr, .template_expr => {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "Pipeline: closure template expr={d} requires lexical owner template={d} but active source context was {s}",
                                .{
                                    @intFromEnum(template.cir_expr),
                                    @intFromEnum(lexical_owner_template),
                                    @tagName(self.currentSourceContext()),
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
                            self.currentCallableInstRawForDebug(),
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
                result.getCallableParamProjectionEntries(lhs_entry.projections),
                result.getCallableParamProjectionEntries(rhs_entry.projections),
            )) return false;
            if (lhs_entry.callable_member_set_id != rhs_entry.callable_member_set_id) return false;
        }
        return true;
    }

    fn scanCallableInst(self: *Pass, result: *Result, callable_inst_id: CallableInstId) Allocator.Error!void {
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

        const module_env = self.all_module_envs[template.module_idx];

        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();
        var iteration_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype) = .empty;
        defer iteration_expr_monotypes.deinit(self.allocator);
        var iteration_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype) = .empty;
        defer iteration_pattern_monotypes.deinit(self.allocator);

        try self.pushSourceContext(callableInstSourceContext(callable_inst_id));
        defer self.popSourceContext();

        try self.seedBindingsForCallableInst(result, callable_inst_id, &bindings);

        const saved_bindings = self.active_bindings;
        self.active_bindings = &bindings;
        defer self.active_bindings = saved_bindings;
        const saved_iteration_expr_monotypes = self.active_iteration_expr_monotypes;
        self.active_iteration_expr_monotypes = &iteration_expr_monotypes;
        defer self.active_iteration_expr_monotypes = saved_iteration_expr_monotypes;
        const saved_iteration_pattern_monotypes = self.active_iteration_pattern_monotypes;
        self.active_iteration_pattern_monotypes = &iteration_pattern_monotypes;
        defer self.active_iteration_pattern_monotypes = saved_iteration_pattern_monotypes;
        if (template.binding_pattern) |binding_pattern| {
            try self.mergeTrackedContextPatternMonotype(
                result,
                self.resultPatternKey(callable_inst_id, template.module_idx, binding_pattern),
                resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
            );
            try self.installCallableParamDirectValueInCallableContext(
                result,
                callable_inst_id,
                template.module_idx,
                binding_pattern,
                callable_inst_id,
            );
        }

        var iterations: u32 = 0;
        while (true) {
            iterations += 1;
            if (std.debug.runtime_safety and iterations > 32) {
                std.debug.panic(
                    "Pipeline: callable-inst binding fixed point did not converge for callable_inst={d}",
                    .{
                        @intFromEnum(callable_inst_id),
                    },
                );
            }

            const bindings_before = bindings.count();
            const mutation_revision_before = self.mutation_revision;
            if (template.binding_pattern) |binding_pattern| {
                try iteration_pattern_monotypes.put(
                    self.allocator,
                    self.resultPatternKey(callable_inst_id, template.module_idx, binding_pattern),
                    resolvedMonotype(callable_inst.fn_monotype, callable_inst.fn_monotype_module_idx),
                );
            }
            try self.recordCallableParamPatternFactsForCallableInst(result, callable_inst_id);

            switch (module_env.store.getExpr(template.cir_expr)) {
                .e_lambda => |lambda_expr| {
                    try self.scanCirValueExpr(result, template.module_idx, lambda_expr.body);
                },
                .e_closure => |closure_expr| {
                    const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                    if (lambda_expr == .e_lambda) {
                        try self.scanCirValueExpr(result, template.module_idx, lambda_expr.e_lambda.body);
                    }
                },
                .e_hosted_lambda => |hosted_expr| try self.scanCirValueExpr(result, template.module_idx, hosted_expr.body),
                else => unreachable,
            }

            switch (module_env.store.getExpr(template.runtime_expr)) {
                .e_closure => |closure_expr| {
                    try self.scanClosureCaptureSources(
                        result,
                        defining_source_context,
                        template.module_idx,
                        template.runtime_expr,
                        closure_expr,
                    );
                },
                .e_lambda, .e_hosted_lambda => {},
                else => unreachable,
            }

            if (bindings.count() == bindings_before and
                self.mutation_revision == mutation_revision_before)
            {
                break;
            }
        }

        var it = iteration_expr_monotypes.iterator();
        while (it.next()) |entry| {
            try self.mergeTrackedContextExprMonotype(result, entry.key_ptr.*, entry.value_ptr.*);
        }
        var pattern_it = iteration_pattern_monotypes.iterator();
        while (pattern_it.next()) |entry| {
            try self.mergeTrackedResolvedMonotypeMap(
                result,
                .context_pattern_monotypes,
                &result.context_mono.context_pattern_monotypes,
                entry.key_ptr.*,
                entry.value_ptr.*,
                "exact pattern",
            );
        }

        switch (module_env.store.getExpr(template.runtime_expr)) {
            .e_closure => |closure_expr| {
                try self.finalizeCallableDefForCallableInst(
                    result,
                    defining_source_context,
                    template.module_idx,
                    template.runtime_expr,
                    closure_expr,
                    callable_inst_id,
                );
            },
            .e_lambda, .e_hosted_lambda => {
                const body_expr_idx = switch (module_env.store.getExpr(template.runtime_expr)) {
                    .e_lambda => |lambda_expr| lambda_expr.body,
                    .e_hosted_lambda => |hosted_expr| hosted_expr.body,
                    else => unreachable,
                };
                try self.updateCallableDefRuntimeValue(
                    result,
                    callable_inst_id,
                    try self.ensureProgramExpr(
                        result,
                        callableInstSourceContext(callable_inst_id),
                        template.module_idx,
                        template.runtime_expr,
                    ),
                    try self.ensureProgramExpr(
                        result,
                        callableInstSourceContext(callable_inst_id),
                        template.module_idx,
                        body_expr_idx,
                    ),
                    &.{},
                );
            },
            else => unreachable,
        }

        if (self.scratch_context_expr_monotypes_depth == 0) {
            try self.completed_callable_scans.put(self.allocator, callable_inst_key, {});
        }
    }

    fn seedCallableBodyBindingsFromSignature(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        callable_expr_idx: CIR.Expr.Idx,
        source_context: SourceContext,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const callable_expr = module_env.store.getExpr(callable_expr_idx);
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype)) {
            .func => |func| func,
            else => return,
        };
        const CallableBoundary = struct {
            arg_patterns: []const CIR.Pattern.Idx,
            body_expr: CIR.Expr.Idx,
        };

        const boundary: CallableBoundary = switch (callable_expr) {
            .e_lambda => |lambda_expr| .{
                .arg_patterns = module_env.store.slicePatterns(lambda_expr.args),
                .body_expr = lambda_expr.body,
            },
            .e_closure => |closure_expr| blk: {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr != .e_lambda) return;
                break :blk .{
                    .arg_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args),
                    .body_expr = lambda_expr.e_lambda.body,
                };
            },
            .e_hosted_lambda => |hosted_expr| .{
                .arg_patterns = module_env.store.slicePatterns(hosted_expr.args),
                .body_expr = hosted_expr.body,
            },
            else => return,
        };

        if (boundary.arg_patterns.len != fn_mono.args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: callable signature arity mismatch for expr {d} (patterns={d}, monos={d})",
                    .{
                        @intFromEnum(callable_expr_idx),
                        boundary.arg_patterns.len,
                        fn_mono.args.len,
                    },
                );
            }
            unreachable;
        }

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        try self.bindTypeVarMonotypes(
            result,
            module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            ModuleEnv.varFrom(callable_expr_idx),
            fn_monotype,
            fn_monotype_module_idx,
        );

        for (boundary.arg_patterns, 0..) |pattern_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.mergeTrackedContextPatternMonotype(
                result,
                self.resultPatternKeyForSourceContext(source_context, module_idx, pattern_idx),
                resolvedMonotype(param_mono, fn_monotype_module_idx),
            );
            try self.bindTypeVarMonotypes(
                result,
                module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(pattern_idx),
                param_mono,
                fn_monotype_module_idx,
            );
        }
        try self.bindTypeVarMonotypes(
            result,
            module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            ModuleEnv.varFrom(boundary.body_expr),
            fn_mono.ret,
            fn_monotype_module_idx,
        );
    }

    fn seedTemplateBodyBindingsFromCurrentBindings(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.callableBoundaryInfo(template.module_idx, template.cir_expr) orelse return;
        const resolved_func = resolveFuncTypeInStore(&module_env.types, template.type_root) orelse return;

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        if (try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
            result,
            template.module_idx,
            &module_env.types,
            template.type_root,
            bindings,
        )) |fn_mono| {
            try self.bindTypeVarMonotypes(
                result,
                template.module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(template.cir_expr),
                fn_mono.idx,
                fn_mono.module_idx,
            );
        }

        const param_vars = module_env.types.sliceVars(resolved_func.func.args);
        if (boundary.arg_patterns.len != param_vars.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: template boundary arity mismatch for expr {d} (patterns={d}, vars={d})",
                    .{
                        @intFromEnum(template.cir_expr),
                        boundary.arg_patterns.len,
                        param_vars.len,
                    },
                );
            }
            unreachable;
        }

        for (boundary.arg_patterns, param_vars) |pattern_idx, param_var| {
            if (try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
                result,
                template.module_idx,
                &module_env.types,
                param_var,
                bindings,
            )) |param_mono| {
                try self.bindTypeVarMonotypes(
                    result,
                    template.module_idx,
                    &module_env.types,
                    bindings,
                    &ordered_entries,
                    ModuleEnv.varFrom(pattern_idx),
                    param_mono.idx,
                    param_mono.module_idx,
                );
            }
        }

        if (try self.resolveTemplateTypeVarIfFullyBoundWithBindings(
            result,
            template.module_idx,
            &module_env.types,
            resolved_func.func.ret,
            bindings,
        )) |ret_mono| {
            try self.bindTypeVarMonotypes(
                result,
                template.module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(boundary.body_expr),
                ret_mono.idx,
                ret_mono.module_idx,
            );
        }
    }

    fn resolveTemplateTypeVarIfFullyBoundWithBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        type_var: types.Var,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!?ResolvedMonotype {
        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (!try self.typeVarFullyBoundWithBindings(
            result,
            module_idx,
            store_types,
            type_var,
            bindings,
            &seen,
        )) {
            return null;
        }

        const mono = try self.resolveTypeVarMonotypeWithBindings(
            result,
            module_idx,
            store_types,
            type_var,
            bindings,
        );
        if (mono.isNone()) return null;
        return resolvedMonotype(mono, module_idx);
    }

    fn seedTemplateBoundaryBindingsFromActuals(
        self: *Pass,
        result: *Result,
        actual_module_idx: u32,
        template: CallableTemplate,
        actual_args: []const CIR.Expr.Idx,
        ret_mono: ResolvedMonotype,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.callableBoundaryInfo(template.module_idx, template.cir_expr) orelse return;

        if (boundary.arg_patterns.len != actual_args.len) return;

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        for (boundary.arg_patterns, actual_args) |pattern_idx, arg_expr_idx| {
            const arg_mono = try self.resolveExprMonotypeResolved(result, actual_module_idx, arg_expr_idx);
            if (arg_mono.isNone()) continue;

            try self.bindTypeVarMonotypes(
                result,
                template.module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(pattern_idx),
                arg_mono.idx,
                arg_mono.module_idx,
            );
        }

        if (!ret_mono.isNone()) {
            try self.bindTypeVarMonotypes(
                result,
                template.module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(boundary.body_expr),
                ret_mono.idx,
                ret_mono.module_idx,
            );
        }
    }

    fn seedTemplateBindingsFromFnMonotype(
        self: *Pass,
        result: *Result,
        template: CallableTemplate,
        fn_monotype: ResolvedMonotype,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        if (fn_monotype.isNone()) return;

        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.callableBoundaryInfo(template.module_idx, template.cir_expr) orelse return;
        const fn_mono = switch (result.context_mono.monotype_store.getMonotype(fn_monotype.idx)) {
            .func => |func| func,
            else => return,
        };

        if (boundary.arg_patterns.len != fn_mono.args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Pipeline: template fn-monotype arity mismatch for expr {d} (patterns={d}, monos={d})",
                    .{
                        @intFromEnum(template.cir_expr),
                        boundary.arg_patterns.len,
                        fn_mono.args.len,
                    },
                );
            }
            unreachable;
        }

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            template.type_root,
            fn_monotype.idx,
            fn_monotype.module_idx,
        );

        for (boundary.arg_patterns, 0..) |pattern_idx, i| {
            const param_mono = result.context_mono.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindTypeVarMonotypes(
                result,
                template.module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(pattern_idx),
                param_mono,
                fn_monotype.module_idx,
            );
        }

        try self.bindTypeVarMonotypes(
            result,
            template.module_idx,
            &module_env.types,
            bindings,
            &ordered_entries,
            ModuleEnv.varFrom(boundary.body_expr),
            fn_mono.ret,
            fn_monotype.module_idx,
        );
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
    ) Allocator.Error!void {
        const tail_mono = try self.remainingRecordTailMonotype(result, mono_fields, seen_indices);
        try self.bindTypeVarMonotypes(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            ext_var,
            tail_mono,
            mono_module_idx,
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
    ) Allocator.Error!void {
        const tail_mono = try self.remainingTagUnionTailMonotype(result, mono_tags, seen_indices);
        try self.bindTypeVarMonotypes(
            result,
            template_module_idx,
            template_types,
            bindings,
            ordered_entries,
            ext_var,
            tail_mono,
            mono_module_idx,
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
    ) Allocator.Error!void {
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
                try self.bindTypeVarMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    payload_var,
                    mono_payload,
                    mono_module_idx,
                );
            }
            return;
        }

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
        if (self.binding_probe_mode and self.binding_probe_failed) return;
        if (monotype.isNone()) return;
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
            return;
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
            .alias => |alias| try self.bindTypeVarMonotypes(
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                template_types.getAliasBackingVar(alias),
                normalized_mono,
                template_module_idx,
            ),
            .structure => |flat_type| {
                try bindings.put(resolved_key, resolved_mono);
                try ordered_entries.append(self.allocator, .{
                    .key = resolved_key,
                    .monotype = resolved_mono,
                });
                try self.bindFlatTypeMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    flat_type,
                    normalized_mono,
                    template_module_idx,
                );
            },
            .err => {},
        }
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
        if (self.binding_probe_mode and self.binding_probe_failed) return;
        if (monotype.isNone()) return;

        const mono = result.context_mono.monotype_store.getMonotype(monotype);
        const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[template_module_idx].common);

        switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const mfunc = switch (mono) {
                    .func => |mfunc| mfunc,
                    else => {
                        self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        return;
                    },
                };

                const type_args = template_types.sliceVars(func.args);
                if (type_args.len != mfunc.args.len) unreachable;

                for (type_args, 0..) |arg_var, i| {
                    const mono_arg = result.context_mono.monotype_store.getIdxSpanItem(mfunc.args, i);
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        arg_var,
                        mono_arg,
                        mono_module_idx,
                    );
                }
                try self.bindTypeVarMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    func.ret,
                    mfunc.ret,
                    mono_module_idx,
                );
            },
            .nominal_type => |nominal| {
                const ident = nominal.ident.ident_idx;
                const origin = nominal.origin_module;

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                    const mlist = switch (mono) {
                        .list => |mlist| mlist,
                        else => {
                            self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                            return;
                        },
                    };
                    const type_args = template_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        type_args[0],
                        mlist.elem,
                        mono_module_idx,
                    );
                    return;
                }

                if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                    const mbox = switch (mono) {
                        .box => |mbox| mbox,
                        else => {
                            self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                            return;
                        },
                    };
                    const type_args = template_types.sliceNominalArgs(nominal);
                    if (type_args.len != 1) unreachable;
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        type_args[0],
                        mbox.inner,
                        mono_module_idx,
                    );
                    return;
                }

                if (origin.eql(common_idents.builtin_module) and builtinPrimForNominal(ident, common_idents) != null) {
                    switch (mono) {
                        .prim => {},
                        else => {
                            self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                            return;
                        },
                    }
                    return;
                }

                try self.bindTypeVarMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    template_types.getNominalBackingVar(nominal),
                    monotype,
                    mono_module_idx,
                );
            },
            .record => |record| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (flatRecordRepresentsEmpty(template_types, record)) return;
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "Pipeline invariant violated: non-empty template record matched unit monotype (template_module={d}, mono_module={d}, active_callable_inst={d}, monotype={d})",
                                .{
                                    template_module_idx,
                                    mono_module_idx,
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
                                "Pipeline invariant violated: expected record monotype for template record, got {s} (template_module={d}, mono_module={d}, active_callable_inst={d}, monotype={d})",
                                .{
                                    @tagName(mono),
                                    template_module_idx,
                                    mono_module_idx,
                                    self.currentCallableInstRawForDebug(),
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
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
                        try self.bindTypeVarMonotypes(
                            result,
                            template_module_idx,
                            template_types,
                            bindings,
                            ordered_entries,
                            field_var,
                            mono_field.type_idx,
                            mono_module_idx,
                        );
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
                                        try self.bindTypeVarMonotypes(
                                            result,
                                            template_module_idx,
                                            template_types,
                                            bindings,
                                            ordered_entries,
                                            field_var,
                                            mono_field.type_idx,
                                            mono_module_idx,
                                        );
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => {
                                    self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                                    return;
                                },
                            },
                            .flex, .rigid => {
                                try self.bindRecordRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    result.context_mono.monotype_store.getFields(mrec.fields),
                                    seen_field_indices.items,
                                    mono_module_idx,
                                );
                                break :rows;
                            },
                            .err => {
                                self.bindFlatTypeErrorTail(flat_type, template_module_idx, mono_module_idx, monotype);
                                return;
                            },
                        }
                    }
                    return;
                }
            },
            .record_unbound => |fields_range| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    .unit => {
                        if (template_types.getRecordFieldsSlice(fields_range).len == 0) return;
                        self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        return;
                    },
                    else => {
                        self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        return;
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
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        field_var,
                        mono_field.type_idx,
                        mono_module_idx,
                    );
                }
            },
            .tuple => |tuple| {
                const mtup = switch (mono) {
                    .tuple => |mtup| mtup,
                    else => {
                        self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        return;
                    },
                };
                const elem_vars = template_types.sliceVars(tuple.elems);
                if (elem_vars.len != mtup.elems.len) unreachable;
                for (elem_vars, 0..) |elem_var, i| {
                    const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(mtup.elems, i);
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        elem_var,
                        elem_mono,
                        mono_module_idx,
                    );
                }
            },
            .tag_union => |tag_union| {
                const mtag = switch (mono) {
                    .tag_union => |mtag| mtag,
                    else => {
                        self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                        return;
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
                        try self.bindTagPayloadsByName(
                            result,
                            template_module_idx,
                            template_types,
                            bindings,
                            ordered_entries,
                            tag_name,
                            template_types.sliceVars(args_range),
                            mtag.tags,
                            mono_module_idx,
                        );
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
                                    self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                                    return;
                                },
                            },
                            .flex, .rigid => {
                                try self.bindTagUnionRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    result.context_mono.monotype_store.getTags(mtag.tags),
                                    seen_tag_indices.items,
                                    mono_module_idx,
                                );
                                break :rows;
                            },
                            .err => {
                                self.bindFlatTypeErrorTail(flat_type, template_module_idx, mono_module_idx, monotype);
                                return;
                            },
                        }
                    }
                }
            },
            .empty_record => switch (mono) {
                .unit, .record => {},
                else => {
                    self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    return;
                },
            },
            .empty_tag_union => switch (mono) {
                .tag_union => {},
                else => {
                    self.bindFlatTypeMismatch(flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    return;
                },
            },
        }
    }

    fn bindFlatTypeMismatch(
        self: *Pass,
        flat_type: types.FlatType,
        mono: Monotype.Monotype,
        template_module_idx: u32,
        mono_module_idx: u32,
        monotype: Monotype.Idx,
    ) void {
        if (self.binding_probe_mode) {
            self.binding_probe_failed = true;
            return;
        }
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline bindFlatTypeMonotypes mismatch: flat_type={s} mono={s} template_module={d} mono_module={d} active_callable_inst={d} monotype={d} probe_mode={}",
                .{
                    @tagName(flat_type),
                    @tagName(mono),
                    template_module_idx,
                    mono_module_idx,
                    self.currentCallableInstRawForDebug(),
                    @intFromEnum(monotype),
                    self.binding_probe_mode,
                },
            );
        }
        unreachable;
    }

    fn bindFlatTypeErrorTail(
        self: *Pass,
        flat_type: types.FlatType,
        template_module_idx: u32,
        mono_module_idx: u32,
        monotype: Monotype.Idx,
    ) void {
        if (self.binding_probe_mode) {
            self.binding_probe_failed = true;
            return;
        }
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Pipeline bindFlatTypeMonotypes hit err tail: flat_type={s} template_module={d} mono_module={d} active_callable_inst={d} monotype={d} probe_mode={}",
                .{
                    @tagName(flat_type),
                    template_module_idx,
                    mono_module_idx,
                    self.currentCallableInstRawForDebug(),
                    @intFromEnum(monotype),
                    self.binding_probe_mode,
                },
            );
        }
        unreachable;
    }

    fn resolveExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Monotype.Idx {
        return (try self.resolveExprMonotypeResolved(result, module_idx, expr_idx)).idx;
    }

    fn resolveExprMonotypeResolved(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupCurrentExprMonotype(result, module_idx, expr_idx)) |resolved| {
            return resolved;
        }

        if (self.active_bindings != null) {
            const module_env = self.all_module_envs[module_idx];
            switch (module_env.store.getExpr(expr_idx)) {
                .e_lookup_local => |lookup| {
                    const pattern_mono = try self.resolveTypeVarMonotypeResolved(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(lookup.pattern_idx),
                    );
                    if (!pattern_mono.isNone()) return pattern_mono;
                },
                else => {},
            }
        }

        return self.resolveTypeVarMonotypeResolved(result, module_idx, ModuleEnv.varFrom(expr_idx));
    }

    fn resolveExprMonotypeIfFullyBound(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupCurrentExprMonotype(result, module_idx, expr_idx)) |resolved| {
            return resolved;
        }

        if (self.readExprCallableValue(result, self.currentSourceContext(), module_idx, expr_idx)) |callable_value| {
            return result.getCallableValueMonotype(callable_value);
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |lookup| {
                if (self.getContextPatternMonotypeInCurrentContext(result, module_idx, lookup.pattern_idx)) |pattern_mono| {
                    return pattern_mono;
                }
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    if (self.lookupExprMonotypeForSourceContext(
                        result,
                        self.currentSourceContext(),
                        source.module_idx,
                        source.expr_idx,
                    )) |source_mono| {
                        return source_mono;
                    }
                }
                const pattern_mono = try self.resolveTypeVarMonotypeIfFullyBound(
                    result,
                    module_idx,
                    ModuleEnv.varFrom(lookup.pattern_idx),
                );
                if (!pattern_mono.isNone()) return pattern_mono;
            },
            else => {},
        }

        const exact_typevar_mono = try self.resolveTypeVarMonotypeIfFullyBound(
            result,
            module_idx,
            ModuleEnv.varFrom(expr_idx),
        );
        if (!exact_typevar_mono.isNone()) return exact_typevar_mono;

        if (self.exprUsesContextSensitiveNumericDefault(module_idx, expr_idx)) {
            return resolvedMonotype(.none, module_idx);
        }

        return resolvedMonotype(.none, module_idx);
    }

    fn resolveTypeVarMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!Monotype.Idx {
        return (try self.resolveTypeVarMonotypeResolved(result, module_idx, var_)).idx;
    }

    fn resolveTypeVarMonotypeResolved(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!ResolvedMonotype {
        if (self.active_bindings) |bindings| {
            const mono = try self.resolveTypeVarMonotypeWithBindings(
                result,
                module_idx,
                &self.all_module_envs[module_idx].types,
                var_,
                bindings,
            );
            return resolvedMonotype(mono, module_idx);
        }
        const mono = try self.monotypeFromTypeVarInStore(result, module_idx, &self.all_module_envs[module_idx].types, var_);
        return resolvedMonotype(mono, module_idx);
    }

    fn resolveTypeVarMonotypeIfFullyBound(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!ResolvedMonotype {
        const bindings = self.active_bindings orelse {
            const store_types = &self.all_module_envs[module_idx].types;
            var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
            defer seen.deinit(self.allocator);

            if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, var_, &seen)) {
                return resolvedMonotype(.none, module_idx);
            }
            return self.resolveTypeVarMonotypeResolved(result, module_idx, var_);
        };

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (!try self.typeVarFullyBoundWithBindings(
            result,
            module_idx,
            &self.all_module_envs[module_idx].types,
            var_,
            bindings,
            &seen,
        )) {
            return resolvedMonotype(.none, module_idx);
        }

        const mono = try self.resolveTypeVarMonotypeWithBindings(
            result,
            module_idx,
            &self.all_module_envs[module_idx].types,
            var_,
            bindings,
        );
        return resolvedMonotype(mono, module_idx);
    }

    fn typeVarFullyBoundWithoutBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        var empty_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer empty_bindings.deinit();
        return self.typeVarFullyBoundWithBindings(
            result,
            module_idx,
            store_types,
            var_,
            &empty_bindings,
            seen,
        );
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

    fn lookupBindingMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        bindings: *const std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        var_: types.Var,
    ) Allocator.Error!?ResolvedMonotype {
        if (bindings.get(boundTypeVarKey(module_idx, store_types, var_))) |binding| return binding;

        const type_scope = self.type_scope orelse return null;
        const type_scope_module_idx = self.type_scope_module_idx orelse return null;
        const caller_module_idx = self.type_scope_caller_module_idx orelse return null;

        if (module_idx != type_scope_module_idx) return null;

        const resolved = store_types.resolveVar(var_);
        const current_name = switch (resolved.desc.content) {
            .rigid => |rigid| rigid.name,
            .flex => |flex| flex.name orelse null,
            else => null,
        } orelse return null;

        const caller_types = &self.all_module_envs[caller_module_idx].types;

        for (type_scope.scopes.items) |*scope| {
            var it = scope.iterator();
            while (it.next()) |entry| {
                const platform_resolved = store_types.resolveVar(entry.key_ptr.*);
                const platform_name = switch (platform_resolved.desc.content) {
                    .rigid => |rigid| rigid.name,
                    .flex => |flex| flex.name orelse continue,
                    else => continue,
                };
                if (!platform_name.eql(current_name)) continue;

                const caller_mono = try self.monotypeFromTypeVarInStore(result, caller_module_idx, caller_types, entry.value_ptr.*);
                if (caller_mono.isNone()) continue;
                const normalized_mono = if (caller_module_idx == module_idx)
                    caller_mono
                else
                    try self.remapMonotypeBetweenModules(result, caller_mono, caller_module_idx, module_idx);
                return resolvedMonotype(normalized_mono, module_idx);
            }
        }

        return null;
    }

    fn typeVarFullyBoundWithBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
        bindings: *const std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        if (try lookupBindingMonotype(self, result, module_idx, store_types, bindings, var_)) |_| return true;

        const resolved = store_types.resolveVar(var_);
        if (seen.contains(resolved.var_)) return true;
        try seen.put(self.allocator, resolved.var_, {});

        return switch (resolved.desc.content) {
            .flex, .rigid => false,
            .alias => |alias| self.typeVarFullyBoundWithBindings(
                result,
                module_idx,
                store_types,
                store_types.getAliasBackingVar(alias),
                bindings,
                seen,
            ),
            .structure => |flat_type| self.flatTypeFullyBoundWithBindings(
                result,
                module_idx,
                store_types,
                flat_type,
                bindings,
                seen,
            ),
            .err => true,
        };
    }

    fn flatTypeFullyBoundWithBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        flat_type: types.FlatType,
        bindings: *const std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        return switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                for (store_types.sliceVars(func.args)) |arg_var| {
                    if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, arg_var, bindings, seen)) {
                        break :blk false;
                    }
                }
                break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, func.ret, bindings, seen);
            },
            .nominal_type => |nominal| blk: {
                for (store_types.sliceNominalArgs(nominal)) |arg_var| {
                    if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, arg_var, bindings, seen)) {
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
                        if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
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
                                            if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                                                break :blk false;
                                            }
                                        }
                                        break :blk true;
                                    },
                                    .empty_record => break :blk true,
                                    else => break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, backing, bindings, seen),
                                }
                            }
                            break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, backing, bindings, seen);
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                for (ext_fields.items(.var_)) |field_var| {
                                    if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            },
                            .empty_record => break :blk true,
                            else => break :blk false,
                        },
                        .flex, .rigid => break :blk true,
                        else => break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, current_row.ext, bindings, seen),
                    }
                }
            },
            .record_unbound => |fields_range| blk: {
                const fields_slice = store_types.getRecordFieldsSlice(fields_range);
                for (fields_slice.items(.var_)) |field_var| {
                    if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, elem_var, bindings, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const tags = store_types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |args_range| {
                    for (store_types.sliceVars(args_range)) |payload_var| {
                        if (!try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, payload_var, bindings, seen)) {
                            break :blk false;
                        }
                    }
                }

                const ext_resolved = store_types.resolveVar(tag_union.ext);
                switch (ext_resolved.desc.content) {
                    .flex, .rigid => break :blk true,
                    else => break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, tag_union.ext, bindings, seen),
                }
            },
            .empty_record, .empty_tag_union => true,
        };
    }

    fn flatTypeFullyBoundWithoutBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        flat_type: types.FlatType,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        return switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                for (store_types.sliceVars(func.args)) |arg_var| {
                    if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, arg_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, func.ret, seen);
            },
            .nominal_type => |nominal| blk: {
                for (store_types.sliceNominalArgs(nominal)) |arg_var| {
                    if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, arg_var, seen)) {
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
                        if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, field_var, seen)) {
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
                                            if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                                                break :blk false;
                                            }
                                        }
                                        break :blk true;
                                    },
                                    .empty_record => break :blk true,
                                    else => break :blk try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, backing, seen),
                                }
                            }
                            break :blk try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, backing, seen);
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                for (ext_fields.items(.var_)) |field_var| {
                                    if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            },
                            .empty_record => break :blk true,
                            else => break :blk false,
                        },
                        .flex, .rigid => break :blk true,
                        else => break :blk try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, current_row.ext, seen),
                    }
                }
            },
            .record_unbound => |fields_range| blk: {
                const fields_slice = store_types.getRecordFieldsSlice(fields_range);
                for (fields_slice.items(.var_)) |field_var| {
                    if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, elem_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const tags = store_types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |args_range| {
                    for (store_types.sliceVars(args_range)) |payload_var| {
                        if (!try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, payload_var, seen)) {
                            break :blk false;
                        }
                    }
                }

                const ext_resolved = store_types.resolveVar(tag_union.ext);
                switch (ext_resolved.desc.content) {
                    .flex, .rigid => break :blk true,
                    else => break :blk try self.typeVarFullyBoundWithoutBindings(result, module_idx, store_types, tag_union.ext, seen),
                }
            },
            .empty_record, .empty_tag_union => true,
        };
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
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    pass.setTypeScope(type_scope_module_idx, type_scope, type_scope_caller_module_idx);
    defer pass.deinit();
    return pass.runRootSourceExpr(expr_idx);
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
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    pass.setTypeScope(type_scope_module_idx, type_scope, type_scope_caller_module_idx);
    defer pass.deinit();
    return pass.runRootSourceExprs(exprs);
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
    var pass = Pass.init(
        allocator,
        all_module_envs,
        types_store,
        current_module_idx,
        app_module_idx,
    );
    pass.setTypeScope(type_scope_module_idx, type_scope, type_scope_caller_module_idx);
    defer pass.deinit();
    return pass.runModule();
}
