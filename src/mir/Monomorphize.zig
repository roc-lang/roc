//! Explicit callable monomorphization.
//!
//! This module owns the compiler phase that decides which callable instance
//! every use site means before MIR lowering begins. `Lower` must consume the
//! result of this pass; it must not infer callable instantiations itself.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const CallableSourceNamespace = enum(u2) {
    local_pattern = 0,
    external_def = 1,
    expr = 2,
};

fn packCallableSourceKey(namespace: CallableSourceNamespace, module_idx: u32, local_id: u32) u64 {
    if (std.debug.runtime_safety) {
        std.debug.assert(module_idx <= std.math.maxInt(u31));
        std.debug.assert(local_id <= std.math.maxInt(u31));
    }

    return (@as(u64, @intFromEnum(namespace)) << 62) |
        (@as(u64, module_idx) << 31) |
        @as(u64, local_id);
}

fn packLocalPatternSourceKey(module_idx: u32, pattern_idx: CIR.Pattern.Idx) u64 {
    return packCallableSourceKey(.local_pattern, module_idx, @intFromEnum(pattern_idx));
}

fn packExternalDefSourceKey(module_idx: u32, def_node_idx: u16) u64 {
    return packCallableSourceKey(.external_def, module_idx, def_node_idx);
}

fn packExprSourceKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
    return packCallableSourceKey(.expr, module_idx, @intFromEnum(expr_idx));
}

/// Identifies a semantic callable template before monomorphic instantiation.
pub const ProcTemplateId = enum(u32) {
    _,

    pub const none: ProcTemplateId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ProcTemplateId) bool {
        return self == none;
    }
};

/// Identifies a canonical type-variable substitution for a proc template.
pub const TypeSubstId = enum(u32) {
    _,

    pub const none: TypeSubstId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: TypeSubstId) bool {
        return self == none;
    }
};

/// Identifies one monomorphic proc instantiation.
pub const ProcInstId = enum(u32) {
    _,

    pub const none: ProcInstId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ProcInstId) bool {
        return self == none;
    }
};

const BoundTypeVarKey = struct {
    module_idx: u32,
    type_var: types.Var,
};

/// A monotype plus the module whose ident space that monotype currently uses.
pub const ResolvedMonotype = struct {
    idx: Monotype.Idx,
    module_idx: u32,

    pub fn isNone(self: ResolvedMonotype) bool {
        return self.idx.isNone();
    }
};

/// One concrete type-variable assignment inside a proc instantiation substitution.
pub const TypeSubstEntry = struct {
    key: BoundTypeVarKey,
    monotype: ResolvedMonotype,
};

/// A packed slice of substitution entries stored in the monomorphization result.
pub const TypeSubstSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() TypeSubstSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: TypeSubstSpan) bool {
        return self.len == 0;
    }
};

/// Describes the original callable form that produced a proc template.
pub const ProcTemplateKind = enum {
    top_level_def,
    lambda,
    closure,
    hosted_lambda,
};

/// A semantic callable body that can later be instantiated monomorphically.
pub const ProcTemplate = struct {
    source_key: u64,
    module_idx: u32,
    cir_expr: CIR.Expr.Idx,
    type_root: types.Var,
    binding_pattern: ?CIR.Pattern.Idx = null,
    kind: ProcTemplateKind = .top_level_def,
    lexical_owner_template: ProcTemplateId = .none,
    source_region: Region = Region.zero(),
};

/// Records a block-local polymorphic callable that is materialized on demand.
pub const DeferredLocalCallable = struct {
    pattern_idx: CIR.Pattern.Idx,
    cir_expr: CIR.Expr.Idx,
    module_idx: u32,
    source_key: u64,
    type_root: types.Var,
};

/// The canonical substitution assigned to one proc instantiation.
pub const TypeSubst = struct {
    entries: TypeSubstSpan,
};

/// One concrete instantiation of a semantic proc template.
pub const ProcInst = struct {
    template: ProcTemplateId,
    subst: TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_context_proc_inst: ProcInstId,
};

/// Interned identifier for a set of demanded proc instantiations.
pub const ProcInstSetId = enum(u32) {
    _,

    pub const none: ProcInstSetId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ProcInstSetId) bool {
        return self == none;
    }
};

/// Contiguous span of proc-inst-set members in the monomorphization store.
pub const ProcInstSetSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ProcInstSetSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ProcInstSetSpan) bool {
        return self.len == 0;
    }
};

/// A deduplicated set of proc instantiations associated with one callable value.
pub const ProcInstSet = struct {
    members: ProcInstSetSpan,
};

/// Associates a source call site with the proc instantiation chosen for it.
pub const CallSiteResolution = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    proc_inst: ProcInstId,
};

const ContextExprKey = struct {
    context_proc_inst_raw: u32,
    root_expr_raw: u32,
    module_idx: u32,
    expr_raw: u32,
};

const ContextCaptureKey = struct {
    closure_proc_inst_raw: u32,
    module_idx: u32,
    closure_expr_raw: u32,
    pattern_raw: u32,
};

const TemplateBodyCompletionKey = struct {
    template_source_key: u64,
    context_proc_inst_raw: u32,
};

const ContextPatternKey = struct {
    context_proc_inst_raw: u32,
    module_idx: u32,
    pattern_raw: u32,
};

const MutationKind = enum(u8) {
    proc_templates,
    proc_template_ids_by_source,
    source_exprs,
    deferred_local_callables,
    expr_proc_insts,
    closure_capture_monotypes,
    closure_capture_proc_insts,
    context_expr_monotypes,
    dispatch_expr_proc_insts,
    proc_inst_sets,
    context_pattern_proc_inst_sets,
    expr_proc_inst_sets,
    lookup_expr_proc_inst_sets,
    call_site_proc_inst_sets,
    call_site_proc_insts,
    context_pattern_proc_insts,
    lookup_expr_proc_insts,
    proc_insts,
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
    template_id: ProcTemplateId,
    type_var: types.Var,
    qualified_method_ident: Ident.Idx,
};

/// A source expression in a specific module, used for semantic value provenance.
pub const ExprSource = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

/// Output of the MIR monomorphization pass.
pub const Result = struct {
    monotype_store: Monotype.Store,
    proc_templates: std.ArrayListUnmanaged(ProcTemplate),
    proc_insts: std.ArrayListUnmanaged(ProcInst),
    proc_inst_set_entries: std.ArrayListUnmanaged(ProcInstId),
    proc_inst_sets: std.ArrayListUnmanaged(ProcInstSet),
    subst_entries: std.ArrayListUnmanaged(TypeSubstEntry),
    substs: std.ArrayListUnmanaged(TypeSubst),
    context_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    expr_proc_insts: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstId),
    expr_proc_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstSetId),
    call_site_proc_insts: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstId),
    call_site_proc_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstSetId),
    dispatch_expr_proc_insts: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstId),
    lookup_expr_proc_insts: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstId),
    lookup_expr_proc_inst_sets: std.AutoHashMapUnmanaged(ContextExprKey, ProcInstSetId),
    closure_capture_monotypes: std.AutoHashMapUnmanaged(ContextCaptureKey, ResolvedMonotype),
    closure_capture_proc_insts: std.AutoHashMapUnmanaged(ContextCaptureKey, ProcInstId),
    context_pattern_proc_insts: std.AutoHashMapUnmanaged(ContextPatternKey, ProcInstId),
    context_pattern_proc_inst_sets: std.AutoHashMapUnmanaged(ContextPatternKey, ProcInstSetId),
    source_exprs: std.AutoHashMapUnmanaged(u64, ExprSource),
    proc_template_ids_by_source: std.AutoHashMapUnmanaged(u64, ProcTemplateId),
    deferred_local_callables: std.AutoHashMapUnmanaged(u64, DeferredLocalCallable),
    root_module_idx: u32,
    root_expr_idx: ?CIR.Expr.Idx,

    pub fn init(allocator: Allocator, root_module_idx: u32, root_expr_idx: ?CIR.Expr.Idx) !Result {
        return .{
            .monotype_store = try Monotype.Store.init(allocator),
            .proc_templates = .empty,
            .proc_insts = .empty,
            .proc_inst_set_entries = .empty,
            .proc_inst_sets = .empty,
            .subst_entries = .empty,
            .substs = .empty,
            .context_expr_monotypes = .empty,
            .expr_proc_insts = .empty,
            .expr_proc_inst_sets = .empty,
            .call_site_proc_insts = .empty,
            .call_site_proc_inst_sets = .empty,
            .dispatch_expr_proc_insts = .empty,
            .lookup_expr_proc_insts = .empty,
            .lookup_expr_proc_inst_sets = .empty,
            .closure_capture_monotypes = .empty,
            .closure_capture_proc_insts = .empty,
            .context_pattern_proc_insts = .empty,
            .context_pattern_proc_inst_sets = .empty,
            .source_exprs = .empty,
            .proc_template_ids_by_source = .empty,
            .deferred_local_callables = .empty,
            .root_module_idx = root_module_idx,
            .root_expr_idx = root_expr_idx,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.monotype_store.deinit(allocator);
        self.proc_templates.deinit(allocator);
        self.proc_insts.deinit(allocator);
        self.proc_inst_set_entries.deinit(allocator);
        self.proc_inst_sets.deinit(allocator);
        self.subst_entries.deinit(allocator);
        self.substs.deinit(allocator);
        self.context_expr_monotypes.deinit(allocator);
        self.expr_proc_insts.deinit(allocator);
        self.expr_proc_inst_sets.deinit(allocator);
        self.call_site_proc_insts.deinit(allocator);
        self.call_site_proc_inst_sets.deinit(allocator);
        self.dispatch_expr_proc_insts.deinit(allocator);
        self.lookup_expr_proc_insts.deinit(allocator);
        self.lookup_expr_proc_inst_sets.deinit(allocator);
        self.closure_capture_monotypes.deinit(allocator);
        self.closure_capture_proc_insts.deinit(allocator);
        self.context_pattern_proc_insts.deinit(allocator);
        self.context_pattern_proc_inst_sets.deinit(allocator);
        self.source_exprs.deinit(allocator);
        self.proc_template_ids_by_source.deinit(allocator);
        self.deferred_local_callables.deinit(allocator);
    }

    pub fn callSiteKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
        return (@as(u64, module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    }

    pub fn contextExprKey(
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return .{
            .context_proc_inst_raw = @intFromEnum(context_proc_inst),
            .root_expr_raw = if (context_proc_inst.isNone() and root_expr_context != null)
                @intFromEnum(root_expr_context.?)
            else
                std.math.maxInt(u32),
            .module_idx = module_idx,
            .expr_raw = @intFromEnum(expr_idx),
        };
    }

    pub fn contextCaptureKey(
        closure_proc_inst: ProcInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextCaptureKey {
        return .{
            .closure_proc_inst_raw = @intFromEnum(closure_proc_inst),
            .module_idx = module_idx,
            .closure_expr_raw = @intFromEnum(closure_expr_idx),
            .pattern_raw = @intFromEnum(pattern_idx),
        };
    }

    pub fn contextPatternKey(
        context_proc_inst: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return .{
            .context_proc_inst_raw = @intFromEnum(context_proc_inst),
            .module_idx = module_idx,
            .pattern_raw = @intFromEnum(pattern_idx),
        };
    }

    pub fn getCallSiteProcInst(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        const proc_inst_id = self.call_site_proc_insts.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        if (proc_inst_id.isNone()) return null;
        return proc_inst_id;
    }

    pub fn getCallSiteProcInsts(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        const set_id = self.call_site_proc_inst_sets.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        return self.getProcInstSetMembers(self.getProcInstSet(set_id).members);
    }

    pub fn getExprMonotype(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.context_expr_monotypes.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx));
    }

    pub fn getExprProcInst(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        const proc_inst_id = self.expr_proc_insts.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        if (proc_inst_id.isNone()) return null;
        return proc_inst_id;
    }

    pub fn getExprProcInsts(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        const set_id = self.expr_proc_inst_sets.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        return self.getProcInstSetMembers(self.getProcInstSet(set_id).members);
    }

    pub fn getDispatchExprProcInst(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        return self.dispatch_expr_proc_insts.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx));
    }

    pub fn getLookupExprProcInst(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        const proc_inst_id = self.lookup_expr_proc_insts.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        if (proc_inst_id.isNone()) return null;
        return proc_inst_id;
    }

    pub fn getLookupExprProcInsts(
        self: *const Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        const set_id = self.lookup_expr_proc_inst_sets.get(contextExprKey(context_proc_inst, root_expr_context, module_idx, expr_idx)) orelse return null;
        return self.getProcInstSetMembers(self.getProcInstSet(set_id).members);
    }

    pub fn getClosureCaptureProcInst(
        self: *const Result,
        closure_proc_inst: ProcInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ProcInstId {
        return self.closure_capture_proc_insts.get(contextCaptureKey(
            closure_proc_inst,
            module_idx,
            closure_expr_idx,
            pattern_idx,
        ));
    }

    pub fn getClosureCaptureMonotype(
        self: *const Result,
        closure_proc_inst: ProcInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        return self.closure_capture_monotypes.get(contextCaptureKey(
            closure_proc_inst,
            module_idx,
            closure_expr_idx,
            pattern_idx,
        ));
    }

    pub fn getContextPatternProcInst(
        self: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ProcInstId {
        const proc_inst_id = self.context_pattern_proc_insts.get(contextPatternKey(context_proc_inst, module_idx, pattern_idx)) orelse return null;
        if (proc_inst_id.isNone()) return null;
        return proc_inst_id;
    }

    pub fn getProcInstSet(self: *const Result, proc_inst_set_id: ProcInstSetId) *const ProcInstSet {
        return &self.proc_inst_sets.items[@intFromEnum(proc_inst_set_id)];
    }

    pub fn getProcInstSetMembers(self: *const Result, span: ProcInstSetSpan) []const ProcInstId {
        if (span.len == 0) return &.{};
        return self.proc_inst_set_entries.items[span.start..][0..span.len];
    }

    pub fn getContextPatternProcInsts(
        self: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?[]const ProcInstId {
        const set_id = self.context_pattern_proc_inst_sets.get(contextPatternKey(context_proc_inst, module_idx, pattern_idx)) orelse return null;
        return self.getProcInstSetMembers(self.getProcInstSet(set_id).members);
    }

    pub fn getProcTemplate(self: *const Result, proc_template_id: ProcTemplateId) *const ProcTemplate {
        return &self.proc_templates.items[@intFromEnum(proc_template_id)];
    }

    pub fn getProcInst(self: *const Result, proc_inst_id: ProcInstId) *const ProcInst {
        return &self.proc_insts.items[@intFromEnum(proc_inst_id)];
    }

    pub fn getTypeSubst(self: *const Result, subst_id: TypeSubstId) *const TypeSubst {
        return &self.substs.items[@intFromEnum(subst_id)];
    }

    pub fn getTypeSubstEntries(self: *const Result, span: TypeSubstSpan) []const TypeSubstEntry {
        if (span.len == 0) return &.{};
        return self.subst_entries.items[span.start..][0..span.len];
    }

    pub fn getLocalProcTemplate(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?ProcTemplateId {
        return self.proc_template_ids_by_source.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getExternalProcTemplate(self: *const Result, module_idx: u32, def_node_idx: u16) ?ProcTemplateId {
        return self.proc_template_ids_by_source.get(packExternalDefSourceKey(module_idx, def_node_idx));
    }

    pub fn getExprProcTemplate(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ProcTemplateId {
        return self.proc_template_ids_by_source.get(packExprSourceKey(module_idx, expr_idx));
    }

    pub fn getDeferredLocalCallable(self: *const Result, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ?DeferredLocalCallable {
        return self.deferred_local_callables.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }

    pub fn getPatternSourceExpr(
        self: *const Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprSource {
        return self.source_exprs.get(packLocalPatternSourceKey(module_idx, pattern_idx));
    }
};

/// Monomorphizes callable templates into explicit proc instantiations.
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
    visited_exprs: std.AutoHashMapUnmanaged(u64, void),
    in_progress_value_defs: std.AutoHashMapUnmanaged(ContextExprKey, void),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedDispatchTarget),
    in_progress_proc_scans: std.AutoHashMapUnmanaged(u32, void),
    completed_proc_scans: std.AutoHashMapUnmanaged(u32, void),
    in_progress_template_body_completions: std.AutoHashMapUnmanaged(TemplateBodyCompletionKey, void),
    mutation_revision: u64,
    mutation_counts: [mutation_kind_count]u32,
    scratch_context_expr_monotypes_depth: u32,
    active_bindings: ?*std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    active_iteration_expr_monotypes: ?*std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    active_template_context: ProcTemplateId,
    active_proc_inst_context: ProcInstId,
    active_root_expr_context: ?CIR.Expr.Idx,
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
            .resolved_dispatch_targets = .empty,
            .in_progress_proc_scans = .empty,
            .completed_proc_scans = .empty,
            .in_progress_template_body_completions = .empty,
            .mutation_revision = 0,
            .mutation_counts = [_]u32{0} ** mutation_kind_count,
            .scratch_context_expr_monotypes_depth = 0,
            .active_bindings = null,
            .active_iteration_expr_monotypes = null,
            .active_template_context = .none,
            .active_proc_inst_context = .none,
            .active_root_expr_context = null,
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

    fn exprRootContext(self: *const Pass, context_proc_inst: ProcInstId) ?CIR.Expr.Idx {
        return if (context_proc_inst.isNone()) self.active_root_expr_context else null;
    }

    fn resultExprKey(
        self: *const Pass,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return self.resultExprKeyWithRoot(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn resultExprKeyWithRoot(
        _: *const Pass,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return Result.contextExprKey(
            context_proc_inst,
            root_expr_context,
            module_idx,
            expr_idx,
        );
    }

    fn staticExprKey(_: *const Pass, context_proc_inst: ProcInstId, module_idx: u32, expr_idx: CIR.Expr.Idx) ContextExprKey {
        return Result.contextExprKey(context_proc_inst, null, module_idx, expr_idx);
    }

    fn getExprProcInstInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        return result.getExprProcInst(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getExprProcInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        return result.getExprProcInsts(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getCallSiteProcInstInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        return result.getCallSiteProcInst(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getLookupExprProcInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        return result.getLookupExprProcInsts(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getLookupExprProcInstInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        return result.getLookupExprProcInst(
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
        );
    }

    fn getValueExprProcInstInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ProcInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (result.getContextPatternProcInst(context_proc_inst, module_idx, lookup.pattern_idx)) |proc_inst_id| {
                    return proc_inst_id;
                }
            },
            else => {},
        }

        if (self.getExprProcInstInContext(result, context_proc_inst, module_idx, expr_idx)) |proc_inst_id| {
            return proc_inst_id;
        }

        return self.getLookupExprProcInstInContext(result, context_proc_inst, module_idx, expr_idx);
    }

    fn getValueExprProcInstsInContext(
        self: *const Pass,
        result: *const Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?[]const ProcInstId {
        const module_env = self.all_module_envs[module_idx];
        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (result.getContextPatternProcInsts(context_proc_inst, module_idx, lookup.pattern_idx)) |proc_inst_ids| {
                    return proc_inst_ids;
                }
            },
            else => {},
        }

        if (self.getExprProcInstsInContext(result, context_proc_inst, module_idx, expr_idx)) |proc_inst_ids| {
            return proc_inst_ids;
        }

        return self.getLookupExprProcInstsInContext(result, context_proc_inst, module_idx, expr_idx);
    }

    pub fn deinit(self: *Pass) void {
        self.visited_modules.deinit(self.allocator);
        self.visited_exprs.deinit(self.allocator);
        self.in_progress_value_defs.deinit(self.allocator);
        self.resolved_dispatch_targets.deinit(self.allocator);
        self.in_progress_proc_scans.deinit(self.allocator);
        self.completed_proc_scans.deinit(self.allocator);
        self.in_progress_template_body_completions.deinit(self.allocator);
    }

    pub fn runExpr(self: *Pass, expr_idx: CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator, self.current_module_idx, expr_idx);

        try self.seedResolvedDispatchTargets();
        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);
        try self.scanRootsFixedPoint(&result, &.{expr_idx}, true);

        return result;
    }

    pub fn runRoots(self: *Pass, exprs: []const CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator, self.current_module_idx, null);

        try self.seedResolvedDispatchTargets();
        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);
        try self.scanRootsFixedPoint(&result, exprs, true);

        return result;
    }

    /// Monomorphize all callables rooted in the current module.
    pub fn runModule(self: *Pass) Allocator.Error!Result {
        var result = try Result.init(self.allocator, self.current_module_idx, null);

        try self.seedResolvedDispatchTargets();
        try self.primeAllModules(&result);
        try self.scanSeedModules(&result);
        try self.scanModule(&result, self.current_module_idx);

        const module_env = self.all_module_envs[self.current_module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);
        var root_exprs = std.ArrayList(CIR.Expr.Idx).empty;
        defer root_exprs.deinit(self.allocator);
        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            try root_exprs.append(self.allocator, def.expr);
        }
        try self.scanRootsFixedPoint(&result, root_exprs.items, true);

        return result;
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
        const saved_root_expr_context = self.active_root_expr_context;
        defer self.active_root_expr_context = saved_root_expr_context;

        while (true) {
            iterations += 1;
            if (std.debug.runtime_safety and iterations > 32) {
                std.debug.panic(
                    "Monomorphize: root fixed point did not converge (module={d}, contextualize_roots={}, revision={d}, templates={d}, proc_insts={d}, expr_proc_insts={d}, expr_proc_inst_sets={d}, call_sites={d}, call_site_sets={d}, dispatch={d}, lookups={d}, lookup_sets={d}, context_monos={d}, context_pattern_proc_insts={d}, context_pattern_sets={d}, closure_capture_monos={d}, closure_capture_proc_insts={d}, mutation_counts={any})",
                    .{
                        self.current_module_idx,
                        contextualize_roots,
                        self.mutation_revision,
                        result.proc_templates.items.len,
                        result.proc_insts.items.len,
                        result.expr_proc_insts.count(),
                        result.expr_proc_inst_sets.count(),
                        result.call_site_proc_insts.count(),
                        result.call_site_proc_inst_sets.count(),
                        result.dispatch_expr_proc_insts.count(),
                        result.lookup_expr_proc_insts.count(),
                        result.lookup_expr_proc_inst_sets.count(),
                        result.context_expr_monotypes.count(),
                        result.context_pattern_proc_insts.count(),
                        result.context_pattern_proc_inst_sets.count(),
                        result.closure_capture_monotypes.count(),
                        result.closure_capture_proc_insts.count(),
                        self.mutation_counts,
                    },
                );
            }

            self.visited_exprs.clearRetainingCapacity();
            self.in_progress_value_defs.clearRetainingCapacity();
            self.completed_proc_scans.clearRetainingCapacity();

            const mutation_revision_before = self.mutation_revision;
            for (exprs) |expr_idx| {
                self.active_root_expr_context = if (contextualize_roots) expr_idx else null;
                try self.scanValueExpr(result, self.current_module_idx, expr_idx);
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

    fn appendTracked(self: *Pass, comptime kind: MutationKind, list: anytype, value: anytype) Allocator.Error!void {
        const typed_value: @TypeOf(list.items[0]) = value;
        try list.append(self.allocator, typed_value);
        self.noteMutation(kind);
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

    fn seedResolvedDispatchTargets(self: *Pass) Allocator.Error!void {
        if (self.resolved_dispatch_targets.count() != 0) return;

        for (self.all_module_envs, 0..) |env, mod_idx| {
            const constraints = env.types.sliceAllStaticDispatchConstraints();
            for (constraints) |constraint| {
                if (constraint.source_expr_idx == types.StaticDispatchConstraint.no_source_expr) continue;
                if (constraint.resolved_target.isNone()) continue;

                const key = self.staticExprKey(.none, @intCast(mod_idx), @enumFromInt(constraint.source_expr_idx));
                try self.resolved_dispatch_targets.put(self.allocator, key, .{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                });
            }
        }
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

            _ = try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                def.expr,
                ModuleEnv.varFrom(def.pattern),
                def.pattern,
                packLocalPatternSourceKey(module_idx, def.pattern),
            );
        }
    }

    fn registerProcTemplate(
        self: *Pass,
        result: *Result,
        source_key: u64,
        module_idx: u32,
        cir_expr: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        kind: ProcTemplateKind,
        source_region: Region,
    ) Allocator.Error!ProcTemplateId {
        if (result.proc_template_ids_by_source.get(source_key)) |existing| return existing;

        const lexical_owner_template: ProcTemplateId = if (kind == .closure)
            self.active_template_context
        else
            .none;

        const proc_template_id: ProcTemplateId = @enumFromInt(result.proc_templates.items.len);
        try self.appendTracked(.proc_templates, &result.proc_templates, ProcTemplate{
            .source_key = source_key,
            .module_idx = module_idx,
            .cir_expr = cir_expr,
            .type_root = type_root,
            .binding_pattern = binding_pattern,
            .kind = kind,
            .lexical_owner_template = lexical_owner_template,
            .source_region = source_region,
        });
        try self.putTracked(.proc_template_ids_by_source, &result.proc_template_ids_by_source, source_key, proc_template_id);

        return proc_template_id;
    }

    fn aliasProcTemplateSource(
        self: *Pass,
        result: *Result,
        source_key: u64,
        template_id: ProcTemplateId,
    ) Allocator.Error!void {
        if (result.proc_template_ids_by_source.get(source_key)) |existing| {
            if (existing != template_id) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: conflicting proc template aliases for source key {d} (existing={d}, new={d})",
                        .{ source_key, @intFromEnum(existing), @intFromEnum(template_id) },
                    );
                }
                unreachable;
            }
            return;
        }

        try self.putTracked(.proc_template_ids_by_source, &result.proc_template_ids_by_source, source_key, template_id);
    }

    fn recordSourceExpr(
        self: *Pass,
        result: *Result,
        source_key: u64,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        if (result.source_exprs.get(source_key)) |existing| {
            if (existing.module_idx != module_idx or existing.expr_idx != expr_idx) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: conflicting source exprs for source key {d} (existing={d}:{d}, new={d}:{d})",
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

        try self.putTracked(.source_exprs, &result.source_exprs, source_key, ExprSource{
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
        kind: ProcTemplateKind,
        source_region: Region,
        alias_source_key: u64,
    ) Allocator.Error!ProcTemplateId {
        const template_id = try self.registerProcTemplate(
            result,
            packExprSourceKey(module_idx, expr_idx),
            module_idx,
            expr_idx,
            type_root,
            binding_pattern,
            kind,
            source_region,
        );
        try self.aliasProcTemplateSource(result, alias_source_key, template_id);
        return template_id;
    }

    fn registerProcBackedDefTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        type_root: types.Var,
        binding_pattern: ?CIR.Pattern.Idx,
        alias_source_key: u64,
    ) Allocator.Error!?ProcTemplateId {
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
            .e_block => |block| try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                block.final_expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_dbg => |dbg_expr| try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                dbg_expr.expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_expect => |expect_expr| try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                expect_expr.body,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_return => |return_expr| try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                return_expr.expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_nominal => |nominal_expr| try self.registerProcBackedDefTemplate(
                result,
                module_idx,
                nominal_expr.backing_expr,
                type_root,
                binding_pattern,
                alias_source_key,
            ),
            .e_nominal_external => |nominal_expr| try self.registerProcBackedDefTemplate(
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
            try self.aliasProcTemplateSource(result, packExprSourceKey(module_idx, expr_idx), id);
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
        if (result.deferred_local_callables.contains(source_key)) return;

        try self.putTracked(.deferred_local_callables, &result.deferred_local_callables, source_key, DeferredLocalCallable{
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
                if (try self.registerProcBackedDefTemplate(
                    result,
                    module_idx,
                    decl.expr,
                    ModuleEnv.varFrom(decl.pattern),
                    decl.pattern,
                    packLocalPatternSourceKey(module_idx, decl.pattern),
                )) |_| {
                    try self.registerDeferredLocalCallable(result, module_idx, decl.pattern, decl.expr);
                    try self.scanExpr(result, module_idx, decl.expr);
                    try self.bindPatternCallableValueProcInsts(result, module_idx, decl.pattern, decl.expr);
                } else {
                    try self.scanExpr(result, module_idx, decl.expr);
                    try self.bindPatternCallableValueProcInsts(result, module_idx, decl.pattern, decl.expr);
                    try self.bindCurrentPatternFromExprIfExact(result, module_idx, decl.pattern, decl.expr);
                }
            },
            .s_var => |var_decl| {
                try self.recordPatternSourceExpr(result, module_idx, var_decl.pattern_idx, .{
                    .module_idx = module_idx,
                    .expr_idx = var_decl.expr,
                });
                if (try self.registerProcBackedDefTemplate(
                    result,
                    module_idx,
                    var_decl.expr,
                    ModuleEnv.varFrom(var_decl.pattern_idx),
                    var_decl.pattern_idx,
                    packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                )) |_| {
                    try self.registerDeferredLocalCallable(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    try self.scanExpr(result, module_idx, var_decl.expr);
                    try self.bindPatternCallableValueProcInsts(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                } else {
                    try self.scanExpr(result, module_idx, var_decl.expr);
                    try self.bindPatternCallableValueProcInsts(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    try self.bindCurrentPatternFromExprIfExact(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                }
            },
            .s_reassign => |reassign| {
                try self.scanExpr(result, module_idx, reassign.expr);
                try self.bindCurrentPatternFromExprIfExact(result, module_idx, reassign.pattern_idx, reassign.expr);
            },
            .s_dbg => |dbg_stmt| try self.scanValueExpr(result, module_idx, dbg_stmt.expr),
            .s_expr => |expr_stmt| try self.scanValueExpr(result, module_idx, expr_stmt.expr),
            .s_expect => |expect_stmt| try self.scanValueExpr(result, module_idx, expect_stmt.body),
            .s_for => |for_stmt| {
                try self.scanExpr(result, module_idx, for_stmt.expr);
                try self.scanExpr(result, module_idx, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.scanExpr(result, module_idx, while_stmt.cond);
                try self.scanExpr(result, module_idx, while_stmt.body);
            },
            .s_return => |return_stmt| try self.scanValueExpr(result, module_idx, return_stmt.expr),
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

    fn bindPatternCallableValueProcInsts(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        if (self.getValueExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_ids| {
            for (proc_inst_ids) |proc_inst_id| {
                try self.recordContextPatternProcInst(
                    result,
                    self.active_proc_inst_context,
                    module_idx,
                    pattern_idx,
                    proc_inst_id,
                );
            }
            return;
        }

        if (self.getValueExprProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_id| {
            try self.recordContextPatternProcInst(
                result,
                self.active_proc_inst_context,
                module_idx,
                pattern_idx,
                proc_inst_id,
            );
        }
    }

    fn scanExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanExprInternal(result, module_idx, expr_idx, false, false);
    }

    fn scanValueExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanExprInternal(result, module_idx, expr_idx, true, false);
    }

    fn scanValueExprForced(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        return self.scanExprInternal(result, module_idx, expr_idx, true, true);
    }

    fn scanDemandedValueDefExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const key = self.resultExprKey(self.active_proc_inst_context, module_idx, expr_idx);
        if (self.in_progress_value_defs.contains(key)) return;
        try self.in_progress_value_defs.put(self.allocator, key, {});
        defer _ = self.in_progress_value_defs.remove(key);

        if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
            try self.materializeDemandedExprProcInst(result, module_idx, expr_idx, template_id);
            if (self.getExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_ids| {
                for (proc_inst_ids) |proc_inst_id| {
                    try self.scanProcInst(result, proc_inst_id);
                }
                return;
            }
            if (self.getExprProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_id| {
                try self.scanProcInst(result, proc_inst_id);
            }
            return;
        }

        try self.scanValueExprForced(result, module_idx, expr_idx);
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

    fn scanExprInternal(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        materialize_if_callable: bool,
        force_rescan_children: bool,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        const monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
        if (!monotype.isNone() and !exprMonotypeOwnedByInvocation(expr)) {
            try self.recordCurrentExprMonotype(result, module_idx, expr_idx, monotype.idx, monotype.module_idx);
        }

        if (callableKind(expr)) |kind| {
            const template_id = try self.registerProcTemplate(
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
                try self.materializeDemandedExprProcInst(result, module_idx, expr_idx, template_id);
                if (self.active_bindings == null and self.active_proc_inst_context.isNone()) {
                    return;
                }
            }
        }

        if (self.active_bindings != null or force_rescan_children) {
            try self.scanExprChildren(result, module_idx, expr_idx, expr);
            return;
        }

        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (self.visited_exprs.contains(visit_key)) return;
        try self.visited_exprs.put(self.allocator, visit_key, {});

        try self.scanExprChildren(result, module_idx, expr_idx, expr);

        if (materialize_if_callable and callableKind(expr) == null) {
            if (self.getValueExprProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx) == null and
                self.getValueExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, expr_idx) == null)
            {
                if (try self.materializeCallableExprProcInstInContext(
                    result,
                    self.active_proc_inst_context,
                    module_idx,
                    expr_idx,
                )) |proc_inst_id| {
                    switch (expr) {
                        .e_lookup_local => |lookup| {
                            try self.recordLookupExprProcInst(
                                result,
                                self.active_proc_inst_context,
                                module_idx,
                                expr_idx,
                                proc_inst_id,
                            );
                            try self.recordContextPatternProcInst(
                                result,
                                self.active_proc_inst_context,
                                module_idx,
                                lookup.pattern_idx,
                                proc_inst_id,
                            );
                            try self.recordLookupSourceExprProcInst(result, module_idx, expr_idx, proc_inst_id);
                        },
                        .e_lookup_external, .e_lookup_required => {
                            try self.recordLookupExprProcInst(
                                result,
                                self.active_proc_inst_context,
                                module_idx,
                                expr_idx,
                                proc_inst_id,
                            );
                            try self.recordLookupSourceExprProcInst(result, module_idx, expr_idx, proc_inst_id);
                        },
                        else => {},
                    }
                }
            }
        }
    }

    fn materializeDemandedExprProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: ProcTemplateId,
    ) Allocator.Error!void {
        if (templateRequiresConcreteOwnerProcInst(result, template_id) and
            self.active_proc_inst_context.isNone())
        {
            return;
        }

        const root_expr_context = self.exprRootContext(self.active_proc_inst_context);

        const fn_monotype = try self.resolveExprMonotypeIfExactResolved(
            result,
            module_idx,
            expr_idx,
        );
        if (fn_monotype.isNone()) return;

        const proc_inst_id = try self.ensureProcInst(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
        try self.recordExprProcInstWithRoot(
            result,
            self.active_proc_inst_context,
            root_expr_context,
            module_idx,
            expr_idx,
            proc_inst_id,
        );
    }

    fn templateRequiresConcreteOwnerProcInst(
        result: *const Result,
        template_id: ProcTemplateId,
    ) bool {
        const template = result.getProcTemplate(template_id);
        return template.kind == .closure and !template.lexical_owner_template.isNone();
    }

    fn recordExprProcInst(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        return self.recordExprProcInstWithRoot(
            result,
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
            proc_inst_id,
        );
    }

    fn recordExprProcInstWithRoot(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKeyWithRoot(context_proc_inst, root_expr_context, module_idx, expr_idx);
        if (result.expr_proc_insts.get(key)) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) {
                try self.mergeExprProcInstSetWithRoot(
                    result,
                    context_proc_inst,
                    root_expr_context,
                    module_idx,
                    expr_idx,
                    proc_inst_id,
                );
                return;
            }
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.expr_proc_insts, &result.expr_proc_insts, key, ProcInstId.none);
            }
            try self.mergeExprProcInstSetWithRoot(
                result,
                context_proc_inst,
                root_expr_context,
                module_idx,
                expr_idx,
                proc_inst_id,
            );
            return;
        }

        try self.putTracked(.expr_proc_insts, &result.expr_proc_insts, key, proc_inst_id);
        try self.mergeExprProcInstSetWithRoot(
            result,
            context_proc_inst,
            root_expr_context,
            module_idx,
            expr_idx,
            proc_inst_id,
        );
    }

    fn recordExprProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_ids: []const ProcInstId,
    ) Allocator.Error!void {
        if (proc_inst_ids.len == 0) unreachable;

        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        if (result.expr_proc_insts.get(key)) |existing_proc_inst_id| {
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.expr_proc_insts, &result.expr_proc_insts, key, ProcInstId.none);
            }
        } else {
            try self.putTracked(.expr_proc_insts, &result.expr_proc_insts, key, ProcInstId.none);
        }

        for (proc_inst_ids) |proc_inst_id| {
            try self.mergeExprProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
        }
    }

    fn resolveExprMonotypeIfMonomorphizableResolved(
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
                    const pattern_mono = try self.resolveTypeVarMonotypeIfMonomorphizableResolved(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(lookup.pattern_idx),
                    );
                    if (!pattern_mono.isNone()) return pattern_mono;
                },
                else => {},
            }
        }

        return self.resolveTypeVarMonotypeIfMonomorphizableResolved(result, module_idx, ModuleEnv.varFrom(expr_idx));
    }

    fn lookupCurrentExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        const key = self.resultExprKey(self.active_proc_inst_context, module_idx, expr_idx);
        if (self.active_iteration_expr_monotypes) |iteration_map| {
            if (iteration_map.get(key)) |resolved| return resolved;
            if (!self.active_proc_inst_context.isNone() and
                key.context_proc_inst_raw == @intFromEnum(self.active_proc_inst_context))
            {
                return null;
            }
        }
        return result.context_expr_monotypes.get(key);
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

    fn recordClosureCaptureFactsForProcInst(
        self: *Pass,
        result: *Result,
        enclosing_context_proc_inst: ProcInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        closure_expr: CIR.Expr.Closure,
        closure_proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

        for (module_env.store.sliceCaptures(closure_expr.captures)) |capture_idx| {
            const capture = module_env.store.getCapture(capture_idx);
            const key = Result.contextCaptureKey(closure_proc_inst_id, module_idx, closure_expr_idx, capture.pattern_idx);
            const source = result.getPatternSourceExpr(module_idx, capture.pattern_idx);
            const capture_context_proc_inst = if (!enclosing_context_proc_inst.isNone())
                enclosing_context_proc_inst
            else
                closure_proc_inst_id;
            const local_capture_proc_inst = result.getContextPatternProcInst(
                closure_proc_inst_id,
                module_idx,
                capture.pattern_idx,
            );
            const source_capture_proc_inst = if (source) |capture_source| blk: {
                var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
                defer visiting.deinit(self.allocator);
                var source_proc_insts = std.ArrayList(ProcInstId).empty;
                defer source_proc_insts.deinit(self.allocator);
                try self.collectValueExprProcInstsInContext(
                    result,
                    capture_context_proc_inst,
                    capture_source.module_idx,
                    capture_source.expr_idx,
                    &visiting,
                    &source_proc_insts,
                );
                break :blk if (source_proc_insts.items.len == 1) source_proc_insts.items[0] else null;
            } else null;
            const enclosing_capture_proc_inst = result.getContextPatternProcInst(
                enclosing_context_proc_inst,
                module_idx,
                capture.pattern_idx,
            );
            var capture_mono = if (local_capture_proc_inst orelse source_capture_proc_inst) |capture_proc_inst_id| blk: {
                const proc_inst = result.getProcInst(capture_proc_inst_id);
                break :blk resolvedMonotype(proc_inst.fn_monotype, proc_inst.fn_monotype_module_idx);
            } else try self.resolvePatternMonotypeInProcContext(
                result,
                capture_context_proc_inst,
                module_idx,
                capture.pattern_idx,
            );
            if (capture_mono.isNone()) {
                if (source) |capture_source| {
                    capture_mono = try self.resolveExprMonotypeInProcContext(
                        result,
                        capture_context_proc_inst,
                        capture_source.module_idx,
                        capture_source.expr_idx,
                    );
                }
            }
            if (capture_mono.isNone()) {
                if (enclosing_capture_proc_inst) |capture_proc_inst_id| {
                    const proc_inst = result.getProcInst(capture_proc_inst_id);
                    capture_mono = resolvedMonotype(proc_inst.fn_monotype, proc_inst.fn_monotype_module_idx);
                } else if (source) |capture_source| {
                    capture_mono = try self.resolveExprMonotypeInProcContext(
                        result,
                        enclosing_context_proc_inst,
                        capture_source.module_idx,
                        capture_source.expr_idx,
                    );
                }
            }

            if (!capture_mono.isNone()) {
                try self.mergeTrackedClosureCaptureMonotype(result, key, capture_mono);
            }
            if (local_capture_proc_inst orelse source_capture_proc_inst) |capture_proc_inst_id| {
                try self.putTracked(.closure_capture_proc_insts, &result.closure_capture_proc_insts, key, capture_proc_inst_id);
                continue;
            }

            if (!capture_mono.isNone()) {
                if (source) |capture_source| {
                    if (try self.resolveExprCallableTemplate(result, capture_source.module_idx, capture_source.expr_idx)) |template_id| {
                        const capture_proc_inst_id = try self.ensureProcInst(
                            result,
                            template_id,
                            capture_mono.idx,
                            capture_mono.module_idx,
                        );
                        try self.putTracked(.closure_capture_proc_insts, &result.closure_capture_proc_insts, key, capture_proc_inst_id);
                        try self.scanProcInst(result, capture_proc_inst_id);
                        continue;
                    }
                }
            }

            if (enclosing_capture_proc_inst) |capture_proc_inst_id| {
                try self.putTracked(.closure_capture_proc_insts, &result.closure_capture_proc_insts, key, capture_proc_inst_id);
                continue;
            }

            if (result.getLocalProcTemplate(module_idx, capture.pattern_idx)) |template_id| {
                if (!capture_mono.isNone()) {
                    const capture_proc_inst_id = try self.ensureProcInst(
                        result,
                        template_id,
                        capture_mono.idx,
                        capture_mono.module_idx,
                    );
                    try self.putTracked(.closure_capture_proc_insts, &result.closure_capture_proc_insts, key, capture_proc_inst_id);
                    try self.scanProcInst(result, capture_proc_inst_id);
                }
            }
        }
    }

    fn scanExprChildren(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) Allocator.Error!void {
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
                if (result.getContextPatternProcInst(self.active_proc_inst_context, module_idx, lookup.pattern_idx)) |proc_inst_id| {
                    try self.recordLookupExprProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        expr_idx,
                        proc_inst_id,
                    );
                }
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.resolveLookupExprProcInst(result, module_idx, expr_idx, template_id);
                } else if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    if (result.getContextPatternProcInsts(self.active_proc_inst_context, module_idx, lookup.pattern_idx)) |proc_inst_ids| {
                        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
                        defer visiting.deinit(self.allocator);
                        try self.propagateDemandedProcInstsToValueExpr(
                            result,
                            source.module_idx,
                            source.expr_idx,
                            proc_inst_ids,
                            &visiting,
                        );
                    }
                    try self.scanDemandedValueDefExpr(result, source.module_idx, source.expr_idx);
                    try self.recordLookupProcInstsFromSourceExpr(
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
                _ = try self.registerProcBackedDefTemplate(
                    result,
                    target_module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target_module_idx, lookup.target_node_idx),
                );
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.resolveLookupExprProcInst(result, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExpr(result, target_module_idx, def.expr);
                    try self.recordLookupProcInstsFromSourceExpr(
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
                _ = try self.registerProcBackedDefTemplate(
                    result,
                    target.module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    def.pattern,
                    packExternalDefSourceKey(target.module_idx, target_node_idx),
                );
                if (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) |template_id| {
                    try self.resolveLookupExprProcInst(result, module_idx, expr_idx, template_id);
                } else {
                    try self.scanDemandedValueDefExpr(result, target.module_idx, def.expr);
                    try self.recordLookupProcInstsFromSourceExpr(
                        result,
                        module_idx,
                        expr_idx,
                        null,
                        target.module_idx,
                        def.expr,
                    );
                }
            },
            .e_str => |str_expr| try self.scanValueExprSpan(result, module_idx, module_env.store.sliceExpr(str_expr.span)),
            .e_list => |list_expr| try self.scanValueExprSpan(result, module_idx, module_env.store.sliceExpr(list_expr.elems)),
            .e_tuple => |tuple_expr| {
                try self.recordDemandedTupleElemMonotypes(result, module_idx, expr_idx, tuple_expr);
                try self.scanValueExprSpan(result, module_idx, module_env.store.sliceExpr(tuple_expr.elems));
            },
            .e_match => |match_expr| {
                try self.scanExpr(result, module_idx, match_expr.cond);

                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    for (module_env.store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                        const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                        try self.recordPatternSourceExpr(result, module_idx, branch_pattern.pattern, .{
                            .module_idx = module_idx,
                            .expr_idx = match_expr.cond,
                        });
                        try self.bindCurrentPatternFromExprIfExact(
                            result,
                            module_idx,
                            branch_pattern.pattern,
                            match_expr.cond,
                        );
                    }
                    try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, branch.value);
                    try self.scanValueExpr(result, module_idx, branch.value);
                    if (branch.guard) |guard_expr| {
                        try self.scanExpr(result, module_idx, guard_expr);
                    }
                }
            },
            .e_if => |if_expr| {
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.scanExpr(result, module_idx, branch.cond);
                    try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, branch.body);
                    try self.scanValueExpr(result, module_idx, branch.body);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, if_expr.final_else);
                try self.scanValueExpr(result, module_idx, if_expr.final_else);
            },
            .e_call => |call_expr| {
                if (self.getCallLowLevelOp(module_env, call_expr.func)) |low_level_op| {
                    const arg_exprs = module_env.store.sliceExpr(call_expr.args);
                    if (low_level_op == .str_inspect and arg_exprs.len != 0) {
                        try self.resolveStrInspectHelperProcInstsForTypeVar(
                            result,
                            module_idx,
                            ModuleEnv.varFrom(arg_exprs[0]),
                        );
                    }
                }
                if (!self.suppress_direct_call_resolution) {
                    try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr);
                    if (self.getCallSiteProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx) == null and
                        result.getCallSiteProcInsts(
                            self.active_proc_inst_context,
                            self.exprRootContext(self.active_proc_inst_context),
                            module_idx,
                            expr_idx,
                        ) == null)
                    {
                        try self.scanExpr(result, module_idx, call_expr.func);
                        try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr);
                    }
                    try self.assignCallableArgProcInstsFromCallMonotype(result, module_idx, expr_idx, call_expr);
                } else {
                    try self.scanExpr(result, module_idx, call_expr.func);
                }
                try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(call_expr.args));
                if (self.active_bindings != null) {
                    if (self.getCallSiteProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx) == null and
                        result.getCallSiteProcInsts(
                            self.active_proc_inst_context,
                            self.exprRootContext(self.active_proc_inst_context),
                            module_idx,
                            expr_idx,
                        ) == null)
                    {
                        try self.scanExpr(result, module_idx, call_expr.func);
                    }
                    try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(call_expr.args));
                }
            },
            .e_record => |record_expr| {
                if (record_expr.ext) |ext_expr| {
                    try self.scanExpr(result, module_idx, ext_expr);
                }

                try self.recordDemandedRecordFieldMonotypes(result, module_idx, expr_idx, record_expr);

                const fields = module_env.store.sliceRecordFields(record_expr.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.scanValueExpr(result, module_idx, field.value);
                }
            },
            .e_block => |block_expr| {
                const stmts = module_env.store.sliceStatements(block_expr.stmts);
                try self.preRegisterBlockCallableStmtTemplates(result, module_idx, stmts);
                for (stmts) |stmt_idx| {
                    try self.scanStmt(result, module_idx, stmt_idx);
                }
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, block_expr.final_expr);
                try self.scanValueExpr(result, module_idx, block_expr.final_expr);
            },
            .e_tag => |tag_expr| try self.scanValueExprSpan(result, module_idx, module_env.store.sliceExpr(tag_expr.args)),
            .e_nominal => |nominal_expr| try self.scanValueExpr(result, module_idx, nominal_expr.backing_expr),
            .e_nominal_external => |nominal_expr| try self.scanValueExpr(result, module_idx, nominal_expr.backing_expr),
            .e_closure => |closure_expr| {
                // Callable bodies are scanned exclusively from `scanProcInst`, after
                // the proc template/inst has established its own lexical owner and
                // defining-context chain. Generic expr traversal only records the
                // closure value's capture sources in the surrounding proc.
                try self.scanClosureCaptureSources(result, self.active_proc_inst_context, module_idx, expr_idx, closure_expr);
            },
            .e_lambda => {},
            .e_binop => |binop_expr| {
                try self.scanExpr(result, module_idx, binop_expr.lhs);
                try self.scanExpr(result, module_idx, binop_expr.rhs);
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
                if (self.active_bindings != null) {
                    try self.scanExpr(result, module_idx, binop_expr.lhs);
                    try self.scanExpr(result, module_idx, binop_expr.rhs);
                }
            },
            .e_unary_minus => |unary_expr| {
                try self.scanExpr(result, module_idx, unary_expr.expr);
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
                if (self.active_bindings != null) {
                    try self.scanExpr(result, module_idx, unary_expr.expr);
                }
            },
            .e_unary_not => |unary_expr| try self.scanExpr(result, module_idx, unary_expr.expr),
            .e_dot_access => |dot_expr| {
                try self.scanExpr(result, module_idx, dot_expr.receiver);
                if (dot_expr.args) |args| {
                    try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(args));
                    try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
                    if (self.active_bindings != null) {
                        try self.scanExpr(result, module_idx, dot_expr.receiver);
                        try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(args));
                    }
                }
            },
            .e_tuple_access => |tuple_access| try self.scanExpr(result, module_idx, tuple_access.tuple),
            .e_dbg => |dbg_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, dbg_expr.expr);
                try self.scanValueExpr(result, module_idx, dbg_expr.expr);
            },
            .e_expect => |expect_expr| {
                try self.propagateDemandedValueResultMonotypeToChild(result, module_idx, expr_idx, expect_expr.body);
                try self.scanValueExpr(result, module_idx, expect_expr.body);
            },
            .e_return => |return_expr| try self.scanValueExpr(result, module_idx, return_expr.expr),
            .e_type_var_dispatch => |dispatch_expr| {
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
                try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(dispatch_expr.args));
                if (self.active_bindings != null) {
                    try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(dispatch_expr.args));
                }
            },
            .e_for => |for_expr| {
                try self.scanExpr(result, module_idx, for_expr.expr);
                try self.scanExpr(result, module_idx, for_expr.body);
            },
            .e_hosted_lambda => {},
            .e_run_low_level => |run_low_level| {
                const args = module_env.store.sliceExpr(run_low_level.args);
                try self.scanExprSpan(result, module_idx, args);
                if (run_low_level.op == .str_inspect and args.len != 0) {
                    try self.resolveStrInspectHelperProcInstsForTypeVar(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(args[0]),
                    );
                }
            },
        }
    }

    fn recordLookupProcInstsFromSourceExpr(
        self: *Pass,
        result: *Result,
        lookup_module_idx: u32,
        lookup_expr_idx: CIR.Expr.Idx,
        maybe_pattern_idx: ?CIR.Pattern.Idx,
        source_module_idx: u32,
        source_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        var source_proc_insts = std.ArrayList(ProcInstId).empty;
        defer source_proc_insts.deinit(self.allocator);

        try self.collectValueExprProcInstsInContext(
            result,
            self.active_proc_inst_context,
            source_module_idx,
            source_expr_idx,
            &visiting,
            &source_proc_insts,
        );

        if (source_proc_insts.items.len == 0) return;

        if (source_proc_insts.items.len == 1) {
            const proc_inst_id = source_proc_insts.items[0];
            try self.recordLookupExprProcInst(
                result,
                self.active_proc_inst_context,
                lookup_module_idx,
                lookup_expr_idx,
                proc_inst_id,
            );
            if (maybe_pattern_idx) |pattern_idx| {
                try self.recordContextPatternProcInst(
                    result,
                    self.active_proc_inst_context,
                    lookup_module_idx,
                    pattern_idx,
                    proc_inst_id,
                );
            }
            return;
        }

        try self.recordLookupExprProcInstSet(
            result,
            self.active_proc_inst_context,
            lookup_module_idx,
            lookup_expr_idx,
            source_proc_insts.items,
        );
        if (maybe_pattern_idx) |pattern_idx| {
            for (source_proc_insts.items) |proc_inst_id| {
                try self.recordContextPatternProcInst(
                    result,
                    self.active_proc_inst_context,
                    lookup_module_idx,
                    pattern_idx,
                    proc_inst_id,
                );
            }
        }
    }

    fn scanExprSpan(self: *Pass, result: *Result, module_idx: u32, exprs: []const CIR.Expr.Idx) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanExpr(result, module_idx, child_expr);
        }
    }

    fn scanValueExprSpan(self: *Pass, result: *Result, module_idx: u32, exprs: []const CIR.Expr.Idx) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanValueExpr(result, module_idx, child_expr);
        }
    }

    fn seedBindingsForProcInst(
        self: *Pass,
        result: *Result,
        proc_inst_id: ProcInstId,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const proc_inst = result.getProcInst(proc_inst_id).*;
        const template = result.getProcTemplate(proc_inst.template).*;

        if (!proc_inst.subst.isNone()) {
            const subst = result.getTypeSubst(proc_inst.subst);
            for (result.getTypeSubstEntries(subst.entries)) |entry| {
                try bindings.put(entry.key, entry.monotype);
            }
        }

        try self.seedProcBodyBindingsFromSignature(result, template.module_idx, template.cir_expr, proc_inst, bindings);
    }

    fn resolvePatternMonotypeInProcContext(
        self: *Pass,
        result: *Result,
        proc_inst_id: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) Allocator.Error!ResolvedMonotype {
        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();

        const saved_bindings = self.active_bindings;
        defer self.active_bindings = saved_bindings;
        if (proc_inst_id.isNone()) {
            self.active_bindings = null;
        } else {
            try self.seedBindingsForProcInst(result, proc_inst_id, &bindings);
            self.active_bindings = &bindings;
        }

        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = proc_inst_id;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        return self.resolveTypeVarMonotypeIfMonomorphizableResolved(
            result,
            module_idx,
            ModuleEnv.varFrom(pattern_idx),
        );
    }

    fn resolveExprMonotypeInProcContext(
        self: *Pass,
        result: *Result,
        proc_inst_id: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();

        const saved_bindings = self.active_bindings;
        defer self.active_bindings = saved_bindings;
        if (proc_inst_id.isNone()) {
            self.active_bindings = null;
        } else {
            try self.seedBindingsForProcInst(result, proc_inst_id, &bindings);
            self.active_bindings = &bindings;
        }

        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = proc_inst_id;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        return self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
    }

    fn scanClosureCaptureSources(
        self: *Pass,
        result: *Result,
        source_context_proc_inst: ProcInstId,
        module_idx: u32,
        closure_expr_idx: CIR.Expr.Idx,
        closure_expr: CIR.Expr.Closure,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = source_context_proc_inst;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        const current_template = if (!source_context_proc_inst.isNone())
            result.getProcTemplate(result.getProcInst(source_context_proc_inst).template).*
        else
            null;

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
                    if ((try self.registerProcBackedDefTemplate(
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
                    if ((try self.registerProcBackedDefTemplate(
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

    const ProcInstMatch = union(enum) {
        none,
        one: ProcInstId,
        ambiguous,
    };

    fn selectExistingProcInstForFnMonotype(
        self: *Pass,
        result: *Result,
        proc_inst_ids: []const ProcInstId,
        desired_fn_monotype: ResolvedMonotype,
        required_template: ?ProcTemplateId,
    ) Allocator.Error!ProcInstMatch {
        if (desired_fn_monotype.isNone()) return .none;

        var matched_proc_inst: ?ProcInstId = null;
        for (proc_inst_ids) |proc_inst_id| {
            const proc_inst = result.getProcInst(proc_inst_id);
            if (required_template) |template_id| {
                if (proc_inst.template != template_id) continue;
            }

            if (!try self.monotypesStructurallyEqualAcrossModules(
                result,
                proc_inst.fn_monotype,
                proc_inst.fn_monotype_module_idx,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
            )) continue;

            if (matched_proc_inst) |existing_proc_inst_id| {
                if (existing_proc_inst_id != proc_inst_id) return .ambiguous;
                continue;
            }

            matched_proc_inst = proc_inst_id;
        }

        if (matched_proc_inst) |proc_inst_id| {
            return .{ .one = proc_inst_id };
        }

        return .none;
    }

    const ExistingValueExprProcInstResolution = union(enum) {
        none,
        one: ProcInstId,
        set: []const ProcInstId,
    };

    fn selectExistingValueExprProcInstResolution(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        desired_fn_monotype: ResolvedMonotype,
        preferred_template: ?ProcTemplateId,
    ) Allocator.Error!ExistingValueExprProcInstResolution {
        if (self.getValueExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_ids| {
            switch (try self.selectExistingProcInstForFnMonotype(
                result,
                proc_inst_ids,
                desired_fn_monotype,
                preferred_template,
            )) {
                .one => |proc_inst_id| return .{ .one = proc_inst_id },
                .ambiguous => return .{ .set = proc_inst_ids },
                .none => {
                    if (proc_inst_ids.len > 1) return .{ .set = proc_inst_ids };
                    if (proc_inst_ids.len == 1 and desired_fn_monotype.isNone()) {
                        return .{ .one = proc_inst_ids[0] };
                    }
                },
            }
        }

        if (self.getValueExprProcInstInContext(result, self.active_proc_inst_context, module_idx, expr_idx)) |proc_inst_id| {
            return .{ .one = proc_inst_id };
        }

        return .none;
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
                try self.resolveStrInspectHelperProcInstsForTypeVar(
                    result,
                    module_idx,
                    ModuleEnv.varFrom(arg_exprs[0]),
                );
            }
            return;
        }
        if (resolved_template_id) |template_id| {
            if (!(templateRequiresConcreteOwnerProcInst(result, template_id) and
                self.active_proc_inst_context.isNone()))
            {
                if (try self.inferDirectCallProcInst(result, module_idx, call_expr_idx, call_expr, template_id)) |proc_inst_id| {
                    try self.finalizeResolvedDirectCallProcInst(
                        result,
                        module_idx,
                        call_expr_idx,
                        call_expr,
                        callee_expr,
                        proc_inst_id,
                    );
                    return;
                }
            }
        }
        switch (try self.selectExistingValueExprProcInstResolution(
            result,
            module_idx,
            callee_expr_idx,
            desired_fn_monotype,
            resolved_template_id,
        )) {
            .one => |proc_inst_id| {
                try self.recordCallSiteProcInst(
                    result,
                    self.active_proc_inst_context,
                    module_idx,
                    call_expr_idx,
                    proc_inst_id,
                );
                switch (callee_expr) {
                    .e_lookup_local => |lookup| try self.recordContextPatternProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        lookup.pattern_idx,
                        proc_inst_id,
                    ),
                    .e_lookup_external, .e_lookup_required => {},
                    else => {},
                }
                try self.finalizeResolvedDirectCallProcInst(
                    result,
                    module_idx,
                    call_expr_idx,
                    call_expr,
                    callee_expr,
                    proc_inst_id,
                );
                return;
            },
            .set => |proc_inst_ids| {
                var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
                defer visiting.deinit(self.allocator);
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    module_idx,
                    callee_expr_idx,
                    proc_inst_ids,
                    &visiting,
                );
                try self.recordCallSiteProcInstSet(
                    result,
                    self.active_proc_inst_context,
                    module_idx,
                    call_expr_idx,
                    proc_inst_ids,
                );
                return;
            },
            .none => {},
        }

        const template_id = resolved_template_id orelse {
            switch (callee_expr) {
                .e_lookup_local, .e_lookup_external, .e_lookup_required => {
                    if (try self.callUsesAnnotationOnlyIntrinsic(module_idx, callee_expr_idx)) {
                        return;
                    }
                    if (self.active_bindings != null and !desired_fn_monotype.isNone()) {
                        try self.bindCurrentCallFromFnMonotype(
                            result,
                            module_idx,
                            call_expr_idx,
                            call_expr,
                            desired_fn_monotype.idx,
                            desired_fn_monotype.module_idx,
                        );
                    } else if (std.debug.runtime_safety and self.active_proc_inst_context.isNone()) {
                        if (!self.visited_exprs.contains(exprVisitKey(module_idx, callee_expr_idx))) {
                            return;
                        }
                        std.debug.panic(
                            "Monomorphize missing direct-callee template for expr={d} module={d} callee_expr={d} tag={s}",
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
                            "Monomorphize: demanded direct call expr {d} in module {d} has direct-callable callee expr {d} ({s}) without a proc template",
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
        if (templateRequiresConcreteOwnerProcInst(result, template_id) and
            self.active_proc_inst_context.isNone())
        {
            return;
        }
        const proc_inst_id = if (try self.inferDirectCallProcInst(result, module_idx, call_expr_idx, call_expr, template_id)) |proc_inst_id|
            proc_inst_id
        else {
            if (desired_fn_monotype.isNone() and std.debug.runtime_safety) {
                const template = result.getProcTemplate(template_id);
                const template_env = self.all_module_envs[template.module_idx];
                const binding_name = if (template.binding_pattern) |binding_pattern|
                    switch (template_env.store.getPattern(binding_pattern)) {
                        .assign => |assign_pat| template_env.getIdent(assign_pat.ident),
                        else => "<non-assign>",
                    }
                else
                    "<anonymous>";
                std.debug.panic(
                    "Monomorphize unresolved direct callee '{s}' template={d} kind={s} template_module={d} template_expr={d} call_module={d} call_expr={d} context={d} root_expr={d}",
                    .{
                        binding_name,
                        @intFromEnum(template_id),
                        @tagName(template.kind),
                        template.module_idx,
                        @intFromEnum(template.cir_expr),
                        module_idx,
                        @intFromEnum(call_expr_idx),
                        @intFromEnum(self.active_proc_inst_context),
                        if (self.active_root_expr_context) |root_expr_idx| @intFromEnum(root_expr_idx) else std.math.maxInt(u32),
                    },
                );
            }
            return;
        };
        try self.finalizeResolvedDirectCallProcInst(
            result,
            module_idx,
            call_expr_idx,
            call_expr,
            callee_expr,
            proc_inst_id,
        );
    }

    fn finalizeResolvedDirectCallProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        callee_expr: anytype,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

        try self.recordCallSiteProcInst(
            result,
            self.active_proc_inst_context,
            module_idx,
            call_expr_idx,
            proc_inst_id,
        );
        switch (callee_expr) {
            .e_lookup_local => |lookup| try self.recordContextPatternProcInst(
                result,
                self.active_proc_inst_context,
                module_idx,
                lookup.pattern_idx,
                proc_inst_id,
            ),
            .e_lookup_external, .e_lookup_required => {},
            else => {},
        }

        const proc_inst = result.getProcInst(proc_inst_id);
        const callee_template = result.getProcTemplate(proc_inst.template);
        try self.recordExprProcInst(
            result,
            self.active_proc_inst_context,
            callee_template.module_idx,
            callee_template.cir_expr,
            proc_inst_id,
        );
        try self.bindCurrentCallFromProcInst(result, module_idx, call_expr_idx, call_expr, proc_inst_id);
        const proc_inst_fn_mono = switch (result.monotype_store.getMonotype(proc_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        try self.prepareCallableArgsForProcInst(result, module_idx, arg_exprs, proc_inst_id);
        try self.scanProcInst(result, proc_inst_id);
        try self.recordCallResultProcInstsFromProcInst(result, module_idx, call_expr_idx, proc_inst_id);
        try self.ensureCallableArgProcInstsScanned(result, module_idx, call_expr.args);
        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            call_expr_idx,
            proc_inst_fn_mono.ret,
            proc_inst.fn_monotype_module_idx,
        );
    }

    fn assignCallableArgProcInstsFromParams(
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
            const param_mono = result.monotype_store.getIdxSpanItem(param_monos, i);
            const maybe_template_id = try self.lookupDirectCalleeTemplate(result, module_idx, arg_expr_idx);
            const template_id = maybe_template_id orelse continue;
            const template = result.getProcTemplate(template_id);
            if (self.active_bindings != null and self.active_proc_inst_context.isNone() and template.kind == .closure) {
                // During template-binding completion, a closure's proc identity is not
                // meaningful until the enclosing proc instantiation exists. Defer that
                // assignment to the later concrete proc-context scan.
                continue;
            }
            const proc_inst_id = try self.ensureProcInstUnscanned(result, template_id, param_mono, fn_monotype_module_idx);

            try self.recordExprProcInst(
                result,
                self.active_proc_inst_context,
                template.module_idx,
                template.cir_expr,
                proc_inst_id,
            );

            switch (module_env.store.getExpr(arg_expr_idx)) {
                .e_lookup_local => |lookup| {
                    try self.recordLookupExprProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        arg_expr_idx,
                        proc_inst_id,
                    );
                    try self.recordContextPatternProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        lookup.pattern_idx,
                        proc_inst_id,
                    );
                    try self.recordLookupSourceExprProcInst(result, module_idx, arg_expr_idx, proc_inst_id);
                },
                .e_lookup_external, .e_lookup_required => {
                    try self.recordLookupExprProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        arg_expr_idx,
                        proc_inst_id,
                    );
                    try self.recordLookupSourceExprProcInst(result, module_idx, arg_expr_idx, proc_inst_id);
                },
                .e_lambda, .e_closure, .e_hosted_lambda => try self.recordExprProcInst(
                    result,
                    self.active_proc_inst_context,
                    module_idx,
                    arg_expr_idx,
                    proc_inst_id,
                ),
                else => {},
            }
        }
    }

    fn procBoundaryArgPatterns(
        self: *Pass,
        module_idx: u32,
        proc_expr_idx: CIR.Expr.Idx,
    ) ?[]const CIR.Pattern.Idx {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(proc_expr_idx)) {
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

    fn seedCallableParamProcSetsFromActualArgs(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        callee_proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const callee_proc_inst = result.getProcInst(callee_proc_inst_id);
        const template = result.getProcTemplate(callee_proc_inst.template);
        const arg_patterns = self.procBoundaryArgPatterns(template.module_idx, template.cir_expr) orelse return;
        if (arg_patterns.len != arg_exprs.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: callable param arity mismatch for proc inst {d} (patterns={d}, args={d})",
                    .{ @intFromEnum(callee_proc_inst_id), arg_patterns.len, arg_exprs.len },
                );
            }
            unreachable;
        }

        for (arg_patterns, arg_exprs) |pattern_idx, arg_expr_idx| {
            switch (self.all_module_envs[module_idx].store.getExpr(arg_expr_idx)) {
                .e_lookup_local => |lookup| {
                    if (result.getContextPatternProcInsts(self.active_proc_inst_context, module_idx, lookup.pattern_idx)) |proc_inst_ids| {
                        for (proc_inst_ids) |proc_inst_id| {
                            try self.recordContextPatternProcInst(
                                result,
                                callee_proc_inst_id,
                                template.module_idx,
                                pattern_idx,
                                proc_inst_id,
                            );
                        }
                        continue;
                    }
                },
                else => {},
            }

            if (self.getLookupExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx)) |proc_inst_ids| {
                for (proc_inst_ids) |proc_inst_id| {
                    try self.recordContextPatternProcInst(
                        result,
                        callee_proc_inst_id,
                        template.module_idx,
                        pattern_idx,
                        proc_inst_id,
                    );
                }
                continue;
            }

            const proc_inst_id = self.getExprProcInstInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx) orelse continue;
            try self.recordContextPatternProcInst(
                result,
                callee_proc_inst_id,
                template.module_idx,
                pattern_idx,
                proc_inst_id,
            );
        }
    }

    fn prepareCallableArgsForProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const proc_inst = result.getProcInst(proc_inst_id);
        const proc_inst_fn_mono = switch (result.monotype_store.getMonotype(proc_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        try self.assignCallableArgProcInstsFromParams(
            result,
            module_idx,
            arg_exprs,
            proc_inst_fn_mono.args,
            proc_inst.fn_monotype_module_idx,
        );
        try self.seedCallableParamProcSetsFromActualArgs(
            result,
            module_idx,
            arg_exprs,
            proc_inst_id,
        );
    }

    fn assignCallableArgProcInstsFromCallMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!void {
        if (self.getCallSiteProcInstInContext(result, self.active_proc_inst_context, module_idx, call_expr_idx) != null) return;

        const callee_monotype = try self.resolveExprMonotypeResolved(result, module_idx, call_expr.func);
        if (callee_monotype.isNone()) return;

        const fn_mono = switch (result.monotype_store.getMonotype(callee_monotype.idx)) {
            .func => |func| func,
            else => return,
        };

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) return;

        try self.assignCallableArgProcInstsFromParams(
            result,
            module_idx,
            arg_exprs,
            fn_mono.args,
            callee_monotype.module_idx,
        );
        try self.ensureCallableArgProcInstsScanned(result, module_idx, call_expr.args);
    }

    fn ensureCallableArgProcInstsScanned(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        args: CIR.Expr.Span,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
            if (self.getLookupExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx)) |proc_inst_ids| {
                for (proc_inst_ids) |proc_inst_id| {
                    try self.scanProcInst(result, proc_inst_id);
                }
                continue;
            }

            const proc_inst_id = self.getExprProcInstInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx) orelse continue;
            try self.scanProcInst(result, proc_inst_id);
        }
    }

    fn ensureCallableArgProcInstsScannedSlice(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        arg_exprs: []const CIR.Expr.Idx,
    ) Allocator.Error!void {
        for (arg_exprs) |arg_expr_idx| {
            if (self.getLookupExprProcInstsInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx)) |proc_inst_ids| {
                for (proc_inst_ids) |proc_inst_id| {
                    try self.scanProcInst(result, proc_inst_id);
                }
                continue;
            }

            const proc_inst_id = self.getExprProcInstInContext(result, self.active_proc_inst_context, module_idx, arg_expr_idx) orelse continue;
            try self.scanProcInst(result, proc_inst_id);
        }
    }

    const ProcBoundaryInfo = struct {
        arg_patterns: []const CIR.Pattern.Idx,
        body_expr: CIR.Expr.Idx,
    };

    fn procBoundaryInfo(
        self: *Pass,
        module_idx: u32,
        proc_expr_idx: CIR.Expr.Idx,
    ) ?ProcBoundaryInfo {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(proc_expr_idx)) {
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
        template: ProcTemplate,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
        proc_inst_context: ProcInstId,
    ) Allocator.Error!void {
        const completion_key = TemplateBodyCompletionKey{
            .template_source_key = template.source_key,
            .context_proc_inst_raw = @intFromEnum(proc_inst_context),
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

        const saved_bindings = self.active_bindings;
        self.active_bindings = bindings;
        defer self.active_bindings = saved_bindings;

        const saved_iteration_expr_monotypes = self.active_iteration_expr_monotypes;
        self.active_iteration_expr_monotypes = &iteration_expr_monotypes;
        defer self.active_iteration_expr_monotypes = saved_iteration_expr_monotypes;

        const saved_template_context = self.active_template_context;
        self.active_template_context = result.proc_template_ids_by_source.get(template.source_key) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: missing template id for source key {d} while completing template body",
                    .{template.source_key},
                );
            }
            unreachable;
        };
        defer self.active_template_context = saved_template_context;

        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = proc_inst_context;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        const saved_root_expr_context = self.active_root_expr_context;
        self.active_root_expr_context = if (proc_inst_context.isNone()) template.cir_expr else null;
        defer self.active_root_expr_context = saved_root_expr_context;

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
                    "Monomorphize: template binding completion did not converge for template={d}",
                    .{@intFromEnum(template.cir_expr)},
                );
            }

            const bindings_before = bindings.count();
            const mutation_revision_before = self.mutation_revision;

            try self.seedTemplateBodyBindingsFromCurrentBindings(result, template, bindings);

            switch (expr) {
                .e_lambda => |lambda_expr| try self.scanValueExpr(result, template.module_idx, lambda_expr.body),
                .e_closure => |closure_expr| {
                    const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                    if (lambda_expr == .e_lambda) {
                        try self.scanValueExpr(result, template.module_idx, lambda_expr.e_lambda.body);
                    }
                    try self.scanClosureCaptureSources(
                        result,
                        proc_inst_context,
                        template.module_idx,
                        template.cir_expr,
                        closure_expr,
                    );
                },
                .e_hosted_lambda => |hosted_expr| try self.scanValueExpr(result, template.module_idx, hosted_expr.body),
                else => return,
            }

            if (bindings.count() == bindings_before and
                self.mutation_revision == mutation_revision_before)
            {
                break;
            }
        }
    }

    fn resolveDirectCallFnMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
    ) Allocator.Error!Monotype.Idx {
        const ret_monotype = try self.resolveExprMonotype(result, module_idx, call_expr_idx);
        if (ret_monotype.isNone()) return .none;

        const module_env = self.all_module_envs[module_idx];
        const effectful = if (resolveFuncTypeInStore(&module_env.types, ModuleEnv.varFrom(call_expr.func))) |func_info|
            func_info.effectful
        else
            false;
        const arg_exprs = module_env.store.sliceExpr(call_expr.args);
        var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
        defer arg_monotypes.deinit(self.allocator);

        for (arg_exprs) |arg_expr_idx| {
            const arg_monotype = try self.resolveExprMonotype(result, module_idx, arg_expr_idx);
            if (arg_monotype.isNone()) return .none;
            try arg_monotypes.append(self.allocator, arg_monotype);
        }

        const arg_span = try result.monotype_store.addIdxSpan(self.allocator, arg_monotypes.items);
        return result.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = arg_span,
            .ret = ret_monotype,
            .effectful = effectful,
        } });
    }

    fn inferDirectCallProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        template_id: ProcTemplateId,
    ) Allocator.Error!?ProcInstId {
        if (try self.directCallContainsErrorType(module_idx, call_expr_idx, call_expr)) {
            return null;
        }

        const saved_binding_probe_mode = self.binding_probe_mode;
        const saved_binding_probe_failed = self.binding_probe_failed;
        self.binding_probe_mode = true;
        self.binding_probe_failed = false;
        defer {
            self.binding_probe_mode = saved_binding_probe_mode;
            self.binding_probe_failed = saved_binding_probe_failed;
        }

        const template = result.getProcTemplate(template_id).*;
        const template_env = self.all_module_envs[template.module_idx];
        const template_types = &template_env.types;
        const desired_fn_monotype = resolvedMonotype(
            try self.resolveDirectCallFnMonotype(result, module_idx, call_expr_idx, call_expr),
            module_idx,
        );
        const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, template);

        var callee_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer callee_bindings.deinit();

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        try self.seedTemplateBindingsFromFnMonotype(result, template, desired_fn_monotype, &callee_bindings);
        if (self.binding_probe_failed) return null;

        if (resolveFuncTypeInStore(template_types, template.type_root)) |resolved_func| {
            const ret_mono = try self.resolveExprMonotypeIfExactResolved(result, module_idx, call_expr_idx);
            if (!ret_mono.isNone()) {
                if (self.procBoundaryInfo(template.module_idx, template.cir_expr)) |boundary| {
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
                    if (self.binding_probe_failed) return null;
                }
            }

            const param_vars = template_types.sliceVars(resolved_func.func.args);
            const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
            if (param_vars.len != arg_exprs.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: direct call template arity mismatch at expr {d} (params={d}, args={d})",
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
            if (self.binding_probe_failed) return null;
        }

        try self.completeTemplateBindingsFromBody(result, template, &callee_bindings, defining_context_proc_inst);
        if (self.binding_probe_failed) return null;

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);
        if (!try self.typeVarMonomorphizableWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
            &seen,
        )) {
            return null;
        }

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
            defining_context_proc_inst,
        )) {
            return null;
        }

        return try self.ensureProcInstUnscanned(result, template_id, fn_monotype, template.module_idx);
    }

    fn procSignatureAcceptsFnMonotype(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        template: ProcTemplate,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        defining_context_proc_inst: ProcInstId,
    ) Allocator.Error!bool {
        const saved_binding_probe_mode = self.binding_probe_mode;
        const saved_binding_probe_failed = self.binding_probe_failed;
        self.binding_probe_mode = true;
        self.binding_probe_failed = false;
        defer {
            self.binding_probe_mode = saved_binding_probe_mode;
            self.binding_probe_failed = saved_binding_probe_failed;
        }

        var signature_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer signature_bindings.deinit();

        try self.seedProcBodyBindingsFromSignature(
            result,
            template.module_idx,
            template.cir_expr,
            .{
                .template = template_id,
                .subst = TypeSubstId.none,
                .fn_monotype = fn_monotype,
                .fn_monotype_module_idx = fn_monotype_module_idx,
                .defining_context_proc_inst = defining_context_proc_inst,
            },
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

    fn bindCurrentPatternFromExprIfExact(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr_mono = switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| blk: {
                const source_mono = try self.resolveTypeVarMonotypeIfExactResolved(
                    result,
                    module_idx,
                    ModuleEnv.varFrom(lookup.pattern_idx),
                );
                if (!source_mono.isNone()) break :blk source_mono;
                break :blk try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
            },
            else => try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx),
        };
        if (expr_mono.isNone()) return;

        try self.bindCurrentPatternFromResolvedMonotype(result, module_idx, pattern_idx, expr_mono);
    }

    fn bindCurrentPatternFromResolvedMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        resolved_mono: ResolvedMonotype,
    ) Allocator.Error!void {
        if (resolved_mono.isNone()) return;

        const bindings = self.active_bindings orelse return;
        const module_env = self.all_module_envs[module_idx];

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
                const tag_union_tags = switch (result.monotype_store.getMonotype(resolved_mono.idx)) {
                    .tag_union => |tag_union| tag_union.tags,
                    else => return,
                };
                const mono_tags = result.monotype_store.getTags(tag_union_tags);
                // The pattern's tag may be absent from the monotype when
                // a match covers more tags than the concrete type has
                // (e.g. matching on Try but the value is always Ok).
                const tag_idx = self.tagIndexByNameInSpan(
                    result,
                    module_idx,
                    tag_pat.name,
                    resolved_mono.module_idx,
                    tag_union_tags,
                ) orelse return;
                const mono_tag = mono_tags[tag_idx];
                const mono_payloads = result.monotype_store.getIdxSpan(mono_tag.payloads);
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
                const mono_fields = switch (result.monotype_store.getMonotype(resolved_mono.idx)) {
                    .record => |record_mono| result.monotype_store.getFields(record_mono.fields),
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
                const elem_mono = switch (result.monotype_store.getMonotype(resolved_mono.idx)) {
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
                const mono_elems = switch (result.monotype_store.getMonotype(resolved_mono.idx)) {
                    .tuple => |tuple_mono| result.monotype_store.getIdxSpan(tuple_mono.elems),
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
        template: ProcTemplate,
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
            const exact_arg_mono = try self.resolveExprMonotypeIfExactResolved(result, actual_module_idx, arg_expr_idx);
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

            const exact_arg_mono = try self.resolveExprMonotypeIfExactResolved(result, actual_module_idx, arg_expr_idx);
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
            else if (self.exprUsesContextSensitiveNumericDefault(actual_module_idx, arg_expr_idx))
                resolvedMonotype(.none, actual_module_idx)
            else blk: {
                const resolved = try self.resolveExprMonotypeResolved(result, actual_module_idx, arg_expr_idx);
                if (!resolved.isNone()) break :blk resolved;
                // When exact resolution fails (e.g., tag unions with flex extension
                // variables like [Red, ..]), fall back to monomorphizable resolution
                // which closes flex extensions to produce a concrete monotype.
                break :blk try self.resolveTypeVarMonotypeIfMonomorphizableResolved(
                    result,
                    actual_module_idx,
                    ModuleEnv.varFrom(arg_expr_idx),
                );
            };

            if (arg_mono.isNone()) continue;
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
        caller_template: ProcTemplate,
        caller_template_types: *const types.Store,
        caller_param_var: types.Var,
        caller_bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!?ResolvedMonotype {
        const actual_template_id = (try self.resolveExprCallableTemplate(result, actual_module_idx, arg_expr_idx)) orelse return null;
        const actual_template = result.getProcTemplate(actual_template_id).*;
        if (actual_template.kind == .closure and self.active_proc_inst_context.isNone()) {
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

        const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, actual_template);
        try self.completeTemplateBindingsFromBody(result, actual_template, &actual_bindings, defining_context_proc_inst);

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
        template: ProcTemplate,
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

    fn bindCurrentCallFromProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        call_expr: anytype,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const proc_inst = result.getProcInst(proc_inst_id);
        const fn_mono = switch (result.monotype_store.getMonotype(proc_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        const arg_exprs = self.all_module_envs[module_idx].store.sliceExpr(call_expr.args);
        if (arg_exprs.len != fn_mono.args.len) unreachable;

        for (arg_exprs, 0..) |arg_expr_idx, i| {
            const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
            try self.recordCurrentExprMonotype(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, call_expr_idx, fn_mono.ret, proc_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, call_expr_idx, fn_mono.ret, proc_inst.fn_monotype_module_idx);
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
        const fn_mono = switch (result.monotype_store.getMonotype(fn_monotype)) {
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
            const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, fn_monotype_module_idx);
            try self.recordCurrentExprMonotype(result, module_idx, arg_expr_idx, param_mono, fn_monotype_module_idx);
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, call_expr_idx, fn_mono.ret, fn_monotype_module_idx);
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
        if (module_env.store.getExpr(expr_idx) == .e_anno_only) return;

        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
        );

        switch (module_env.store.getExpr(expr_idx)) {
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

    fn recordCallResultProcInstsFromProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        callee_proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const callee_proc_inst = result.getProcInst(callee_proc_inst_id);
        const template = result.getProcTemplate(callee_proc_inst.template);
        const boundary = self.procBoundaryInfo(template.module_idx, template.cir_expr) orelse return;

        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
        defer visiting.deinit(self.allocator);
        var proc_inst_ids = std.ArrayList(ProcInstId).empty;
        defer proc_inst_ids.deinit(self.allocator);

        try self.collectValueExprProcInstsInContext(
            result,
            callee_proc_inst_id,
            template.module_idx,
            boundary.body_expr,
            &visiting,
            &proc_inst_ids,
        );

        if (proc_inst_ids.items.len == 0) return;
        if (proc_inst_ids.items.len == 1) {
            try self.recordExprProcInst(
                result,
                self.active_proc_inst_context,
                module_idx,
                call_expr_idx,
                proc_inst_ids.items[0],
            );
            return;
        }

        try self.recordExprProcInstSet(
            result,
            self.active_proc_inst_context,
            module_idx,
            call_expr_idx,
            proc_inst_ids.items,
        );
    }

    fn collectValueExprProcInstsInContext(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
        out: *std.ArrayList(ProcInstId),
    ) Allocator.Error!void {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        if (self.getValueExprProcInstsInContext(result, context_proc_inst, module_idx, expr_idx)) |proc_inst_ids| {
            for (proc_inst_ids) |proc_inst_id| {
                var seen = false;
                for (out.items) |existing| {
                    if (existing == proc_inst_id) {
                        seen = true;
                        break;
                    }
                }
                if (!seen) try out.append(self.allocator, proc_inst_id);
            }
            return;
        }

        if (self.getValueExprProcInstInContext(result, context_proc_inst, module_idx, expr_idx)) |proc_inst_id| {
            for (out.items) |existing| {
                if (existing == proc_inst_id) return;
            }
            try out.append(self.allocator, proc_inst_id);
            return;
        }

        const module_env = self.all_module_envs[module_idx];
        if (callableKind(module_env.store.getExpr(expr_idx)) != null) {
            if (try self.materializeCallableExprProcInstInContext(
                result,
                context_proc_inst,
                module_idx,
                expr_idx,
            )) |proc_inst_id| {
                for (out.items) |existing| {
                    if (existing == proc_inst_id) return;
                }
                try out.append(self.allocator, proc_inst_id);
                return;
            }
        }

        switch (module_env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (result.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                    try self.collectValueExprProcInstsInContext(
                        result,
                        context_proc_inst,
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
                    try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, branch.body, visiting, out);
                }
                try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, if_expr.final_else, visiting, out);
            },
            .e_match => |match_expr| {
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, branch.value, visiting, out);
                }
            },
            .e_block => |block_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, block_expr.final_expr, visiting, out),
            .e_dbg => |dbg_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, dbg_expr.expr, visiting, out),
            .e_expect => |expect_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, expect_expr.body, visiting, out),
            .e_return => |return_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, return_expr.expr, visiting, out),
            .e_nominal => |nominal_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, nominal_expr.backing_expr, visiting, out),
            .e_nominal_external => |nominal_expr| try self.collectValueExprProcInstsInContext(result, context_proc_inst, module_idx, nominal_expr.backing_expr, visiting, out),
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
                try self.collectValueExprProcInstsInContext(
                    result,
                    context_proc_inst,
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
                try self.collectValueExprProcInstsInContext(
                    result,
                    context_proc_inst,
                    elem_expr.module_idx,
                    elem_expr.expr_idx,
                    visiting,
                    out,
                );
            },
            else => {},
        }
    }

    fn materializeCallableExprProcInstInContext(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!?ProcInstId {
        const template_id = (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) orelse return null;
        if (templateRequiresConcreteOwnerProcInst(result, template_id) and context_proc_inst.isNone()) {
            return null;
        }

        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = context_proc_inst;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        const fn_monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
        if (fn_monotype.isNone()) return null;

        const template = result.getProcTemplate(template_id).*;
        const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, template);
        if (!try self.procSignatureAcceptsFnMonotype(
            result,
            template_id,
            template,
            fn_monotype.idx,
            fn_monotype.module_idx,
            defining_context_proc_inst,
        )) {
            return null;
        }

        const proc_inst_id = try self.ensureProcInst(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
        try self.recordExprProcInst(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
        return proc_inst_id;
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

    fn inferDispatchProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        template_id: ProcTemplateId,
    ) Allocator.Error!?ProcInstId {
        const template = result.getProcTemplate(template_id).*;
        const template_env = self.all_module_envs[template.module_idx];
        const template_types = &template_env.types;
        const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, template);

        var callee_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer callee_bindings.deinit();

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);

        if (resolveFuncTypeInStore(template_types, template.type_root)) |resolved_func| {
            const param_vars = template_types.sliceVars(resolved_func.func.args);
            if (param_vars.len != actual_args.items.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: dispatch template arity mismatch at expr {d} (params={d}, args={d})",
                        .{
                            @intFromEnum(expr_idx),
                            param_vars.len,
                            actual_args.items.len,
                        },
                    );
                }
                unreachable;
            }

            const ret_mono = try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
            if (!ret_mono.isNone()) {
                if (self.procBoundaryInfo(template.module_idx, template.cir_expr)) |boundary| {
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
                false,
            );
        }

        try self.seedTemplateBoundaryBindingsFromActuals(
            result,
            module_idx,
            template,
            actual_args.items,
            try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx),
            &callee_bindings,
        );

        try self.completeTemplateBindingsFromBody(result, template, &callee_bindings, defining_context_proc_inst);

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);
        if (!try self.typeVarMonomorphizableWithBindings(
            result,
            template.module_idx,
            template_types,
            template.type_root,
            &callee_bindings,
            &seen,
        )) {
            return null;
        }

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
            defining_context_proc_inst,
        )) {
            return null;
        }

        return try self.ensureProcInstUnscanned(result, template_id, fn_monotype, template.module_idx);
    }

    fn bindCurrentDispatchFromProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const proc_inst = result.getProcInst(proc_inst_id);
        const fn_mono = switch (result.monotype_store.getMonotype(proc_inst.fn_monotype)) {
            .func => |func| func,
            else => unreachable,
        };

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);

        if (actual_args.items.len != fn_mono.args.len) unreachable;

        for (actual_args.items, 0..) |arg_expr_idx, i| {
            const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
            try self.recordCurrentExprMonotype(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
        }

        try self.bindCurrentExprTypeRoot(result, module_idx, expr_idx, fn_mono.ret, proc_inst.fn_monotype_module_idx);
        try self.recordCurrentExprMonotype(result, module_idx, expr_idx, fn_mono.ret, proc_inst.fn_monotype_module_idx);
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
        const key = self.resultExprKey(self.active_proc_inst_context, module_idx, expr_idx);
        const resolved = resolvedMonotype(monotype, monotype_module_idx);
        if (self.lookupCurrentExprMonotype(result, module_idx, expr_idx)) |existing| {
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
                const context_template: ?ProcTemplate = if (!self.active_proc_inst_context.isNone())
                    result.getProcTemplate(result.getProcInst(self.active_proc_inst_context).template).*
                else
                    null;
                std.debug.panic(
                    "Monomorphize: conflicting exact expr monotypes for ctx={d} module={d} expr={d} kind={s} region={any} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} template_expr={d} template_kind={s}",
                    .{
                        @intFromEnum(self.active_proc_inst_context),
                        module_idx,
                        @intFromEnum(expr_idx),
                        @tagName(expr),
                        expr_region,
                        @intFromEnum(existing.idx),
                        existing.module_idx,
                        result.monotype_store.getMonotype(existing.idx),
                        @intFromEnum(resolved.idx),
                        resolved.module_idx,
                        result.monotype_store.getMonotype(resolved.idx),
                        if (context_template) |template| @intFromEnum(template.cir_expr) else std.math.maxInt(u32),
                        if (context_template) |template| @tagName(template.kind) else "none",
                    },
                );
            }
            unreachable;
        }

        if (self.active_iteration_expr_monotypes) |iteration_map| {
            try iteration_map.put(self.allocator, key, resolved);
        } else {
            if (self.scratch_context_expr_monotypes_depth != 0) {
                try result.context_expr_monotypes.put(self.allocator, key, resolved);
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
            &result.context_expr_monotypes,
            key,
            resolved,
            "exact expr",
        );
    }

    fn mergeTrackedClosureCaptureMonotype(
        self: *Pass,
        result: *Result,
        key: ContextCaptureKey,
        resolved: ResolvedMonotype,
    ) Allocator.Error!void {
        return self.mergeTrackedResolvedMonotypeMap(
            result,
            .closure_capture_monotypes,
            &result.closure_capture_monotypes,
            key,
            resolved,
            "closure capture",
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
                "Monomorphize: conflicting {s} monotypes while merging key={any} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any}",
                .{
                    label,
                    key,
                    @intFromEnum(existing.idx),
                    existing.module_idx,
                    result.monotype_store.getMonotype(existing.idx),
                    @intFromEnum(resolved.idx),
                    resolved.module_idx,
                    result.monotype_store.getMonotype(resolved.idx),
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

        const mono = result.monotype_store.getMonotype(record_mono.idx);
        const mono_record = switch (mono) {
            .record => |record| record,
            else => return,
        };

        const module_env = self.all_module_envs[module_idx];
        for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
            const field = module_env.store.getRecordField(field_idx);
            const mono_field_idx = self.recordFieldIndexByNameInSpan(
                result,
                module_idx,
                field.name,
                record_mono.module_idx,
                mono_record.fields,
            );
            const mono_field = result.monotype_store.getFieldItem(mono_record.fields, mono_field_idx);
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

        const mono = result.monotype_store.getMonotype(tuple_mono.idx);
        const module_env = self.all_module_envs[module_idx];
        const elems = module_env.store.sliceExpr(tuple_expr.elems);

        switch (mono) {
            .record => |record| {
                const fields = result.monotype_store.getFields(record.fields);
                if (fields.len != elems.len) return;
                for (elems, 0..) |elem_expr_idx, i| {
                    const field = result.monotype_store.getFieldItem(record.fields, i);
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
                    const elem_mono = result.monotype_store.getIdxSpanItem(tup.elems, i);
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
        const parent_mono = try self.resolveExprMonotypeIfMonomorphizableResolved(result, module_idx, parent_expr_idx);
        if (parent_mono.isNone()) return;
        try self.recordCurrentExprMonotype(
            result,
            module_idx,
            child_expr_idx,
            parent_mono.idx,
            parent_mono.module_idx,
        );
    }

    fn resolveDispatchExprProcInst(
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
            if (try self.binopDispatchHandledWithoutProcInst(result, module_idx, expr_idx, binop_expr)) {
                return;
            }
            const method_name = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse return;
            if (try self.resolveAssociatedMethodProcInstForTypeVar(
                result,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            )) |proc_inst_id| {
                try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(binop_expr.lhs),
                method_name,
            );
            const lhs_monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, binop_expr.lhs);
            if (associated_target == null and !lhs_monotype.isNone()) {
                if (try self.resolveAssociatedMethodProcInstForMonotype(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    lhs_monotype.idx,
                    method_name,
                )) |proc_inst_id| {
                    try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
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
                if (try self.dotDispatchHandledWithoutProcInst(result, module_idx, expr_idx, dot_expr)) {
                    return;
                }
                if (try self.resolveAssociatedMethodProcInstForTypeVar(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                )) |proc_inst_id| {
                    try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
                    return;
                }
                associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                    result,
                    module_idx,
                    expr_idx,
                    ModuleEnv.varFrom(dot_expr.receiver),
                    dot_expr.field_name,
                );
                const receiver_monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, dot_expr.receiver);
                if (associated_target == null and !receiver_monotype.isNone()) {
                    if (try self.resolveAssociatedMethodProcInstForMonotype(
                        result,
                        module_idx,
                        expr_idx,
                        expr,
                        receiver_monotype.idx,
                        dot_expr.field_name,
                    )) |proc_inst_id| {
                        try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
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
            if (try self.resolveAssociatedMethodProcInstForTypeVar(
                result,
                module_idx,
                expr_idx,
                expr,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            )) |proc_inst_id| {
                try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
                return;
            }
            associated_target = try self.resolveAssociatedMethodDispatchTargetForTypeVar(
                result,
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(alias_stmt.type_var_anno),
                dispatch_expr.method_name,
            );
            const alias_monotype = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, ModuleEnv.varFrom(alias_stmt.type_var_anno));
            if (associated_target == null and !alias_monotype.isNone()) {
                if (try self.resolveAssociatedMethodProcInstForMonotype(
                    result,
                    module_idx,
                    expr_idx,
                    expr,
                    alias_monotype.idx,
                    dispatch_expr.method_name,
                )) |proc_inst_id| {
                    try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
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
                    const receiver_monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, dot_expr.receiver);
                    break :blk try self.resolveDispatchTargetForDotCall(
                        result,
                        module_idx,
                        expr_idx,
                        dot_expr.field_name,
                        receiver_monotype.idx,
                    );
                }
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, dot_expr.field_name);
            },
            .e_type_var_dispatch => |dispatch_expr| blk: {
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, dispatch_expr.method_name);
            },
            else => return,
        };
        const template_id = try self.lookupResolvedDispatchTemplate(result, module_idx, resolved_target) orelse {
            if (std.debug.runtime_safety) {
                const method_name = self.dispatchTargetMethodText(module_env, resolved_target) orelse "<unreadable>";
                std.debug.panic(
                    "Monomorphize: demanded dispatch expr {d} in module {d} resolved to method '{s}' without a proc template",
                    .{ @intFromEnum(expr_idx), module_idx, method_name },
                );
            }
            unreachable;
        };
        const proc_inst_id = blk: {
            if (self.active_bindings == null) {
                const fn_monotype = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, resolved_target.fn_var);
                if (!fn_monotype.isNone()) {
                    break :blk try self.ensureProcInstUnscanned(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
                }
            }

            if (try self.inferDispatchProcInst(result, module_idx, expr_idx, expr, template_id)) |inferred| {
                break :blk inferred;
            }

            if (self.active_bindings != null) return;

            const fn_monotype = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, resolved_target.fn_var);
            if (!fn_monotype.isNone()) {
                break :blk try self.ensureProcInstUnscanned(result, template_id, fn_monotype.idx, fn_monotype.module_idx);
            }

            return;
        };
        try self.recordDispatchExprProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
    }

    fn recordDispatchExprProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        if (self.scratch_context_expr_monotypes_depth == 0) {
            const key = self.resultExprKey(self.active_proc_inst_context, module_idx, expr_idx);
            try self.putTracked(
                .dispatch_expr_proc_insts,
                &result.dispatch_expr_proc_insts,
                key,
                proc_inst_id,
            );
            const proc_inst = result.getProcInst(proc_inst_id);
            const template = result.getProcTemplate(proc_inst.template);
            try self.recordExprProcInst(
                result,
                self.active_proc_inst_context,
                template.module_idx,
                template.cir_expr,
                proc_inst_id,
            );
        }
        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(module_idx, expr, &actual_args);
        try self.prepareCallableArgsForProcInst(result, module_idx, actual_args.items, proc_inst_id);
        try self.scanProcInst(result, proc_inst_id);
        try self.ensureCallableArgProcInstsScannedSlice(result, module_idx, actual_args.items);
        try self.bindCurrentDispatchFromProcInst(result, module_idx, expr_idx, expr, proc_inst_id);
    }

    fn dotDispatchHandledWithoutProcInst(
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
            const eq_constraint = try self.lookupDispatchConstraintForExpr(result, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(module_idx, expr_idx) == null and !constraint_resolved;
        }

        return switch (result.monotype_store.getMonotype(receiver_monotype)) {
            .record, .tuple, .list, .unit => true,
            .tag_union => self.lookupResolvedDispatchTarget(module_idx, expr_idx) == null,
            else => false,
        };
    }

    fn binopDispatchHandledWithoutProcInst(
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
            const eq_constraint = try self.lookupDispatchConstraintForExpr(result, module_idx, expr_idx, module_env.idents.is_eq);
            const constraint_resolved = if (eq_constraint) |constraint|
                !constraint.resolved_target.isNone() and self.resolvedTargetIsUsable(module_env, module_env.idents.is_eq, constraint.resolved_target)
            else
                false;
            return self.lookupResolvedDispatchTarget(module_idx, expr_idx) == null and !constraint_resolved;
        }

        const lhs_mono = result.monotype_store.getMonotype(lhs_monotype);
        return switch (lhs_mono) {
            .record, .tuple, .list, .unit, .prim => true,
            .tag_union => blk: {
                const cached_dispatch = self.lookupResolvedDispatchTarget(module_idx, expr_idx);
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

    fn resolveAssociatedMethodProcInstForTypeVar(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        receiver_type_var: types.Var,
        method_ident: Ident.Idx,
    ) Allocator.Error!?ProcInstId {
        const module_env = self.all_module_envs[module_idx];
        const receiver_nominal = resolveNominalTypeInStore(&module_env.types, receiver_type_var) orelse return null;
        const method_info = try self.lookupAssociatedMethodTemplate(result, module_idx, receiver_nominal, method_ident) orelse return null;
        const receiver_monotype = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, receiver_type_var);
        _ = try self.lookupDispatchConstraintForAssociatedMethod(
            result,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype.idx,
        ) orelse return null;
        return try self.inferDispatchProcInst(result, module_idx, expr_idx, expr, method_info.template_id);
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
        const receiver_monotype = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, receiver_type_var);
        const constraint = try self.lookupDispatchConstraintForAssociatedMethod(
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

    fn resolveAssociatedMethodProcInstForMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        expr: CIR.Expr,
        receiver_monotype: Monotype.Idx,
        method_ident: Ident.Idx,
    ) Allocator.Error!?ProcInstId {
        const method_info = try self.lookupAssociatedMethodTemplateForMonotype(
            result,
            module_idx,
            receiver_monotype,
            method_ident,
        ) orelse return null;
        _ = try self.lookupDispatchConstraintForAssociatedMethod(
            result,
            module_idx,
            expr_idx,
            method_ident,
            method_info,
            receiver_monotype,
        ) orelse return null;
        return try self.inferDispatchProcInst(result, module_idx, expr_idx, expr, method_info.template_id);
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
        const constraint = try self.lookupDispatchConstraintForAssociatedMethod(
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

    fn lookupResolvedDispatchTarget(self: *Pass, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ResolvedDispatchTarget {
        return self.resolved_dispatch_targets.get(self.staticExprKey(self.active_proc_inst_context, module_idx, expr_idx));
    }

    fn lookupDispatchConstraintForExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) Allocator.Error!?types.StaticDispatchConstraint {
        const module_env = self.all_module_envs[module_idx];

        var resolved_match: ?types.StaticDispatchConstraint = null;
        var unresolved_match: ?types.StaticDispatchConstraint = null;
        var first_unresolved: ?types.StaticDispatchConstraint = null;
        var unresolved_count: u32 = 0;
        var ambiguous_resolved_targets = false;
        var unique_unresolved = std.ArrayList(types.StaticDispatchConstraint).empty;
        defer unique_unresolved.deinit(self.allocator);

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (!constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
            {
                const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module).?;
                const candidate = ResolvedDispatchTarget{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                    .module_idx = target_module_idx,
                };
                const fn_monotype = try self.resolveTypeVarMonotypeIfMonomorphizableResolved(result, module_idx, constraint.fn_var);
                if (!fn_monotype.isNone() and
                    !try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate, fn_monotype.idx))
                {
                    continue;
                }
                if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, module_idx, expr_idx, candidate)) continue;

                if (resolved_match) |existing| {
                    if (std.debug.runtime_safety and
                        (!existing.resolved_target.origin_module.eql(constraint.resolved_target.origin_module) or
                            !existing.resolved_target.method_ident.eql(constraint.resolved_target.method_ident)))
                    {
                        ambiguous_resolved_targets = true;
                        resolved_match = null;
                        continue;
                    }
                }
                if (resolved_match == null) resolved_match = constraint;
                continue;
            }

            var seen_unresolved = false;
            for (unique_unresolved.items) |existing| {
                if (staticDispatchConstraintsEqual(existing, constraint)) {
                    seen_unresolved = true;
                    break;
                }
            }
            if (!seen_unresolved) {
                if (first_unresolved == null) first_unresolved = constraint;
                unresolved_count += 1;
                try unique_unresolved.append(self.allocator, constraint);
            }

            const fn_monotype = try self.resolveTypeVarMonotypeIfMonomorphizableResolved(result, module_idx, constraint.fn_var);
            if (fn_monotype.isNone()) continue;
            if (unresolved_match == null) unresolved_match = constraint;
        }

        if (ambiguous_resolved_targets) {
            return unresolved_match orelse if (unresolved_count == 1) first_unresolved else null;
        }

        return resolved_match orelse unresolved_match orelse if (unresolved_count == 1) first_unresolved else null;
    }

    fn lookupDispatchConstraintForAssociatedMethod(
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

        var resolved_match: ?types.StaticDispatchConstraint = null;
        var unresolved_match: ?types.StaticDispatchConstraint = null;
        var first_unresolved: ?types.StaticDispatchConstraint = null;
        var unresolved_count: u32 = 0;
        var unique_unresolved = std.ArrayList(types.StaticDispatchConstraint).empty;
        defer unique_unresolved.deinit(self.allocator);

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (!constraint.resolved_target.isNone()) {
                const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module) orelse continue;
                if (target_module_idx != method_info.module_idx) continue;
                if (!std.mem.eql(u8, module_env.getIdent(constraint.resolved_target.method_ident), target_method_text)) continue;

                if (resolved_match == null) resolved_match = constraint;
                continue;
            }

            var seen_unresolved = false;
            for (unique_unresolved.items) |existing| {
                if (staticDispatchConstraintsEqual(existing, constraint)) {
                    seen_unresolved = true;
                    break;
                }
            }
            if (!seen_unresolved) {
                if (first_unresolved == null) first_unresolved = constraint;
                unresolved_count += 1;
                try unique_unresolved.append(self.allocator, constraint);
            }
            if (receiver_monotype.isNone()) {
                if (unresolved_match == null) unresolved_match = constraint;
                continue;
            }

            const fn_monotype = try self.resolveTypeVarMonotypeIfMonomorphizableResolved(result, module_idx, constraint.fn_var);
            if (fn_monotype.isNone()) continue;

            const mono = result.monotype_store.getMonotype(fn_monotype.idx);
            if (mono != .func) continue;

            const fn_args = result.monotype_store.getIdxSpan(mono.func.args);
            if (fn_args.len == 0) continue;

            if (!try self.monotypeDispatchCompatible(
                result,
                fn_args[0],
                module_idx,
                receiver_monotype,
                fn_monotype.module_idx,
            )) continue;

            if (unresolved_match == null) unresolved_match = constraint;
        }

        return resolved_match orelse unresolved_match orelse if (unresolved_count == 1) first_unresolved else null;
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
        if (self.lookupResolvedDispatchTarget(module_idx, expr_idx)) |cached| return cached;

        const module_env = self.all_module_envs[module_idx];
        const constraint = try self.lookupDispatchConstraintForExpr(result, module_idx, expr_idx, method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: no static dispatch constraint for expr={d} method='{s}'",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }
            unreachable;
        };

        const desired_func_monotype = try self.resolveTypeVarMonotypeIfMonomorphizableResolved(result, module_idx, constraint.fn_var);

        const resolved = blk: {
            if (!constraint.resolved_target.isNone() and
                self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
            {
                const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module).?;
                const candidate = ResolvedDispatchTarget{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                    .module_idx = target_module_idx,
                };
                if (try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate, desired_func_monotype.idx) and
                    try self.resolvedDispatchTargetMatchesInvocationSignature(result, module_idx, expr_idx, candidate))
                {
                    break :blk candidate;
                }
            }

            break :blk try self.resolveUnresolvedTypeVarDispatchTarget(
                result,
                module_idx,
                expr_idx,
                method_name,
                constraint,
                desired_func_monotype,
            );
        };

        try self.resolved_dispatch_targets.put(
            self.allocator,
            self.staticExprKey(self.active_proc_inst_context, module_idx, expr_idx),
            resolved,
        );
        return resolved;
    }

    fn resolveDispatchTargetForDotCall(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
        receiver_monotype: Monotype.Idx,
    ) Allocator.Error!ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];

        var first_candidate: ?ResolvedDispatchTarget = null;
        var unresolved_match: ?ResolvedDispatchTarget = null;
        var resolved_match: ?ResolvedDispatchTarget = null;

        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            const maybe_candidate: ?ResolvedDispatchTarget = blk: {
                if (!constraint.resolved_target.isNone() and
                    self.resolvedTargetIsUsable(module_env, method_name, constraint.resolved_target))
                {
                    const target_module_idx = self.findModuleForOriginMaybe(module_env, constraint.resolved_target.origin_module).?;
                    break :blk ResolvedDispatchTarget{
                        .origin = constraint.resolved_target.origin_module,
                        .method_ident = constraint.resolved_target.method_ident,
                        .fn_var = constraint.fn_var,
                        .module_idx = target_module_idx,
                    };
                }
                break :blk null;
            };
            const candidate = maybe_candidate orelse continue;

            const fn_mono = try self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, constraint.fn_var);
            if (!try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate, fn_mono.idx)) continue;

            if (first_candidate == null) first_candidate = candidate;

            if (fn_mono.isNone()) {
                if (!constraint.resolved_target.isNone()) {
                    if (resolved_match == null) resolved_match = candidate;
                } else if (unresolved_match == null) {
                    unresolved_match = candidate;
                }
                continue;
            }

            const mono = result.monotype_store.getMonotype(fn_mono.idx);
            if (mono != .func) continue;
            const fn_args = result.monotype_store.getIdxSpan(mono.func.args);
            const compatible = if (fn_args.len == 0)
                true
            else
                try self.monotypeDispatchCompatible(result, fn_args[0], module_idx, receiver_monotype, module_idx);
            if (!compatible) continue;

            if (!constraint.resolved_target.isNone()) {
                if (resolved_match == null) resolved_match = candidate;
            } else if (unresolved_match == null) {
                unresolved_match = candidate;
            }
        }

        const resolved = if (resolved_match) |target|
            target
        else if (unresolved_match) |target|
            target
        else if (first_candidate) |target|
            target
        else
            try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, method_name);

        try self.resolved_dispatch_targets.put(
            self.allocator,
            self.staticExprKey(self.active_proc_inst_context, module_idx, expr_idx),
            resolved,
        );
        return resolved;
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
        const candidate_mono = try self.resolveExprMonotype(result, target_def.module_idx, def.expr);
        if (candidate_mono.isNone()) return false;

        return self.monotypesStructurallyEqualAcrossModules(
            result,
            candidate_mono,
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
        const candidate_mono = try self.resolveExprMonotype(result, target_def.module_idx, def.expr);
        if (candidate_mono.isNone()) return false;

        const candidate_func = switch (result.monotype_store.getMonotype(candidate_mono)) {
            .func => |func| func,
            else => return false,
        };

        var actual_args = std.ArrayList(CIR.Expr.Idx).empty;
        defer actual_args.deinit(self.allocator);
        try self.appendDispatchActualArgs(source_module_idx, source_expr, &actual_args);

        if (candidate_func.args.len != actual_args.items.len) return false;

        for (actual_args.items, 0..) |arg_expr_idx, i| {
            const actual_mono = try self.resolveExprMonotypeIfMonomorphizableResolved(result, source_module_idx, arg_expr_idx);
            if (actual_mono.isNone()) continue;

            const expected_mono = result.monotype_store.getIdxSpanItem(candidate_func.args, i);
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

        const actual_ret = try self.resolveExprMonotypeIfExactResolved(result, source_module_idx, expr_idx);
        if (actual_ret.isNone()) return true;

        return self.monotypesStructurallyEqualAcrossModules(
            result,
            candidate_func.ret,
            target_def.module_idx,
            actual_ret.idx,
            actual_ret.module_idx,
        );
    }

    fn resolveUnresolvedTypeVarDispatchTarget(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
        constraint: types.StaticDispatchConstraint,
        desired_func_monotype: ResolvedMonotype,
    ) Allocator.Error!ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const method_name_text = module_env.getIdent(method_name);
        var found_target: ?ResolvedDispatchTarget = null;

        for (self.all_module_envs, 0..) |candidate_env, candidate_module_idx_usize| {
            const candidate_module_idx: u32 = @intCast(candidate_module_idx_usize);
            const defs = candidate_env.store.sliceDefs(candidate_env.all_defs);
            for (defs) |def_idx| {
                const def = candidate_env.store.getDef(def_idx);
                const pattern = candidate_env.store.getPattern(def.pattern);
                if (pattern != .assign) continue;

                const method_ident = pattern.assign.ident;
                const full_name = candidate_env.getIdent(method_ident);
                if (!identMatchesMethodName(full_name, method_name_text)) continue;
                if (candidate_env.getExposedNodeIndexById(method_ident) == null) continue;

                const candidate_expr = candidate_env.store.getExpr(def.expr);
                if (callableKind(candidate_expr) == null) continue;
                const candidate_origin_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
                const mapped_origin = module_env.common.findIdent(candidate_origin_name) orelse module_env.qualified_module_ident;

                const candidate_target = ResolvedDispatchTarget{
                    .origin = mapped_origin,
                    .method_ident = method_ident,
                    .fn_var = constraint.fn_var,
                    .module_idx = candidate_module_idx,
                };
                if (!desired_func_monotype.isNone() and
                    !try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate_target, desired_func_monotype.idx))
                {
                    continue;
                }
                if (!try self.resolvedDispatchTargetMatchesInvocationSignature(result, module_idx, expr_idx, candidate_target)) continue;

                if (found_target) |existing| {
                    const existing_method_text = self.dispatchTargetMethodText(module_env, existing) orelse {
                        if (std.debug.runtime_safety) {
                            std.debug.panic(
                                "Monomorphize: existing candidate method ident {d} unreadable",
                                .{existing.method_ident.idx},
                            );
                        }
                        unreachable;
                    };
                    if (std.debug.runtime_safety and
                        ((existing.module_idx orelse std.math.maxInt(u32)) != (candidate_target.module_idx orelse std.math.maxInt(u32)) or
                            !std.mem.eql(u8, existing_method_text, candidate_env.getIdent(method_ident))))
                    {
                        std.debug.panic(
                            "Monomorphize: ambiguous dispatch for method '{s}' expr={d}",
                            .{
                                method_name_text,
                                @intFromEnum(expr_idx),
                            },
                        );
                    }
                    continue;
                }
                found_target = candidate_target;
            }
        }

        return found_target orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: no candidate for method '{s}' expr={d}",
                    .{ method_name_text, @intFromEnum(expr_idx) },
                );
            }
            unreachable;
        };
    }

    fn resolveLookupExprProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: ProcTemplateId,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const desired_fn_monotype = try self.resolveExprMonotypeIfExactResolved(result, module_idx, expr_idx);
        const existing_proc_inst_id = if (module_env.store.getExpr(expr_idx) == .e_lookup_local) blk: {
            const lookup = module_env.store.getExpr(expr_idx).e_lookup_local;
            if (result.getContextPatternProcInsts(self.active_proc_inst_context, module_idx, lookup.pattern_idx)) |proc_inst_ids| {
                if (proc_inst_ids.len == 0) unreachable;
                switch (try self.selectExistingProcInstForFnMonotype(
                    result,
                    proc_inst_ids,
                    desired_fn_monotype,
                    template_id,
                )) {
                    .one => |proc_inst_id| break :blk proc_inst_id,
                    .ambiguous => unreachable,
                    .none => {
                        if (proc_inst_ids.len > 1 and desired_fn_monotype.isNone()) {
                            try self.recordLookupExprProcInstSet(
                                result,
                                self.active_proc_inst_context,
                                module_idx,
                                expr_idx,
                                proc_inst_ids,
                            );
                            return;
                        }
                    },
                }
            }
            break :blk null;
        } else null;

        const proc_inst_id = if (existing_proc_inst_id) |proc_inst_id| blk: {
            if (self.scratch_context_expr_monotypes_depth == 0) {
                try self.scanProcInst(result, proc_inst_id);
            }
            break :blk proc_inst_id;
        } else blk: {
            if (desired_fn_monotype.isNone()) return;
            const template = result.getProcTemplate(template_id).*;
            const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, template);
            if (!try self.procSignatureAcceptsFnMonotype(
                result,
                template_id,
                template,
                desired_fn_monotype.idx,
                desired_fn_monotype.module_idx,
                defining_context_proc_inst,
            )) {
                return;
            }
            break :blk if (self.scratch_context_expr_monotypes_depth == 0)
                try self.ensureProcInst(result, template_id, desired_fn_monotype.idx, desired_fn_monotype.module_idx)
            else
                try self.ensureProcInstUnscanned(result, template_id, desired_fn_monotype.idx, desired_fn_monotype.module_idx);
        };
        try self.recordLookupExprProcInst(
            result,
            self.active_proc_inst_context,
            module_idx,
            expr_idx,
            proc_inst_id,
        );
        if (module_env.store.getExpr(expr_idx) == .e_lookup_local) {
            try self.recordContextPatternProcInst(
                result,
                self.active_proc_inst_context,
                module_idx,
                module_env.store.getExpr(expr_idx).e_lookup_local.pattern_idx,
                proc_inst_id,
            );
        }
        const template = result.getProcTemplate(template_id).*;
        try self.recordExprProcInst(
            result,
            self.active_proc_inst_context,
            template.module_idx,
            template.cir_expr,
            proc_inst_id,
        );
        try self.recordLookupSourceExprProcInst(result, module_idx, expr_idx, proc_inst_id);
    }

    fn recordLookupSourceExprProcInst(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        lookup_expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
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
        try self.recordExprProcInst(
            result,
            self.active_proc_inst_context,
            source_expr.module_idx,
            source_expr.expr_idx,
            proc_inst_id,
        );
    }

    fn internProcInstSet(
        self: *Pass,
        result: *Result,
        members: []const ProcInstId,
    ) Allocator.Error!ProcInstSetId {
        for (result.proc_inst_sets.items, 0..) |existing_set, idx| {
            const existing_members = result.getProcInstSetMembers(existing_set.members);
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

        const span: ProcInstSetSpan = if (members.len == 0)
            ProcInstSetSpan.empty()
        else blk: {
            const start: u32 = @intCast(result.proc_inst_set_entries.items.len);
            try result.proc_inst_set_entries.appendSlice(self.allocator, members);
            break :blk .{
                .start = start,
                .len = @intCast(members.len),
            };
        };

        const set_id: ProcInstSetId = @enumFromInt(result.proc_inst_sets.items.len);
        try self.appendTracked(.proc_inst_sets, &result.proc_inst_sets, ProcInstSet{ .members = span });
        return set_id;
    }

    fn mergeContextPatternProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = Result.contextPatternKey(context_proc_inst, module_idx, pattern_idx);
        const existing_set_id = result.context_pattern_proc_inst_sets.get(key);

        var merged = std.ArrayList(ProcInstId).empty;
        defer merged.deinit(self.allocator);

        if (existing_set_id) |set_id| {
            try merged.appendSlice(self.allocator, result.getProcInstSetMembers(result.getProcInstSet(set_id).members));
        }

        for (merged.items) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) return;
        }
        try merged.append(self.allocator, proc_inst_id);
        std.mem.sortUnstable(
            ProcInstId,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: ProcInstId, rhs: ProcInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan,
        );

        const set_id = try self.internProcInstSet(result, merged.items);
        try self.putTracked(.context_pattern_proc_inst_sets, &result.context_pattern_proc_inst_sets, key, set_id);
    }

    fn mergeExprProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        return self.mergeExprProcInstSetWithRoot(
            result,
            context_proc_inst,
            self.exprRootContext(context_proc_inst),
            module_idx,
            expr_idx,
            proc_inst_id,
        );
    }

    fn mergeExprProcInstSetWithRoot(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        root_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKeyWithRoot(context_proc_inst, root_expr_context, module_idx, expr_idx);
        const existing_set_id = result.expr_proc_inst_sets.get(key);

        var merged = std.ArrayList(ProcInstId).empty;
        defer merged.deinit(self.allocator);

        if (existing_set_id) |set_id| {
            try merged.appendSlice(self.allocator, result.getProcInstSetMembers(result.getProcInstSet(set_id).members));
        }

        for (merged.items) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) return;
        }
        try merged.append(self.allocator, proc_inst_id);
        std.mem.sortUnstable(
            ProcInstId,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: ProcInstId, rhs: ProcInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan,
        );

        const set_id = try self.internProcInstSet(result, merged.items);
        try self.putTracked(.expr_proc_inst_sets, &result.expr_proc_inst_sets, key, set_id);
    }

    fn mergeLookupExprProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        const existing_set_id = result.lookup_expr_proc_inst_sets.get(key);

        var merged = std.ArrayList(ProcInstId).empty;
        defer merged.deinit(self.allocator);

        if (existing_set_id) |set_id| {
            try merged.appendSlice(self.allocator, result.getProcInstSetMembers(result.getProcInstSet(set_id).members));
        }

        for (merged.items) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) return;
        }
        try merged.append(self.allocator, proc_inst_id);
        std.mem.sortUnstable(
            ProcInstId,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: ProcInstId, rhs: ProcInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan,
        );

        const set_id = try self.internProcInstSet(result, merged.items);
        try self.putTracked(.lookup_expr_proc_inst_sets, &result.lookup_expr_proc_inst_sets, key, set_id);
    }

    fn mergeCallSiteProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        const existing_set_id = result.call_site_proc_inst_sets.get(key);

        var merged = std.ArrayList(ProcInstId).empty;
        defer merged.deinit(self.allocator);

        if (existing_set_id) |set_id| {
            try merged.appendSlice(self.allocator, result.getProcInstSetMembers(result.getProcInstSet(set_id).members));
        }

        for (merged.items) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) return;
        }
        try merged.append(self.allocator, proc_inst_id);
        std.mem.sortUnstable(
            ProcInstId,
            merged.items,
            {},
            struct {
                fn lessThan(_: void, lhs: ProcInstId, rhs: ProcInstId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan,
        );

        const set_id = try self.internProcInstSet(result, merged.items);
        try self.putTracked(.call_site_proc_inst_sets, &result.call_site_proc_inst_sets, key, set_id);
    }

    fn recordCallSiteProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_ids: []const ProcInstId,
    ) Allocator.Error!void {
        if (proc_inst_ids.len == 0) unreachable;

        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        if (result.call_site_proc_insts.get(key)) |existing_proc_inst_id| {
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.call_site_proc_insts, &result.call_site_proc_insts, key, ProcInstId.none);
            }
        } else {
            try self.putTracked(.call_site_proc_insts, &result.call_site_proc_insts, key, ProcInstId.none);
        }

        for (proc_inst_ids) |proc_inst_id| {
            try self.mergeCallSiteProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
        }
    }

    fn recordCallSiteProcInst(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        if (!context_proc_inst.isNone()) {
            try self.putTracked(.call_site_proc_insts, &result.call_site_proc_insts, key, proc_inst_id);
            const singleton_set_id = try self.internProcInstSet(result, &.{proc_inst_id});
            try self.putTracked(.call_site_proc_inst_sets, &result.call_site_proc_inst_sets, key, singleton_set_id);
            return;
        }

        if (result.call_site_proc_insts.get(key)) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) {
                try self.mergeCallSiteProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
                return;
            }
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.call_site_proc_insts, &result.call_site_proc_insts, key, ProcInstId.none);
            }
            try self.mergeCallSiteProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
            return;
        }

        try self.putTracked(.call_site_proc_insts, &result.call_site_proc_insts, key, proc_inst_id);
        try self.mergeCallSiteProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
    }

    fn recordContextPatternProcInst(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = Result.contextPatternKey(context_proc_inst, module_idx, pattern_idx);
        const existing = result.context_pattern_proc_insts.get(key);
        if (existing) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) {
                try self.mergeContextPatternProcInstSet(result, context_proc_inst, module_idx, pattern_idx, proc_inst_id);
                return;
            }
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.context_pattern_proc_insts, &result.context_pattern_proc_insts, key, ProcInstId.none);
            }
            try self.mergeContextPatternProcInstSet(result, context_proc_inst, module_idx, pattern_idx, proc_inst_id);
            return;
        }
        try self.putTracked(.context_pattern_proc_insts, &result.context_pattern_proc_insts, key, proc_inst_id);
        try self.mergeContextPatternProcInstSet(result, context_proc_inst, module_idx, pattern_idx, proc_inst_id);
    }

    fn recordLookupExprProcInst(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_id: ProcInstId,
    ) Allocator.Error!void {
        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        if (result.lookup_expr_proc_insts.get(key)) |existing_proc_inst_id| {
            if (existing_proc_inst_id == proc_inst_id) {
                try self.mergeLookupExprProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
                return;
            }
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.lookup_expr_proc_insts, &result.lookup_expr_proc_insts, key, ProcInstId.none);
            }
            try self.mergeLookupExprProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
            return;
        }

        try self.putTracked(.lookup_expr_proc_insts, &result.lookup_expr_proc_insts, key, proc_inst_id);
        try self.mergeLookupExprProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
    }

    fn recordLookupExprProcInstSet(
        self: *Pass,
        result: *Result,
        context_proc_inst: ProcInstId,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_ids: []const ProcInstId,
    ) Allocator.Error!void {
        if (proc_inst_ids.len == 0) unreachable;

        const key = self.resultExprKey(context_proc_inst, module_idx, expr_idx);
        if (result.lookup_expr_proc_insts.get(key)) |existing_proc_inst_id| {
            if (!existing_proc_inst_id.isNone()) {
                try self.putTracked(.lookup_expr_proc_insts, &result.lookup_expr_proc_insts, key, ProcInstId.none);
            }
        } else {
            try self.putTracked(.lookup_expr_proc_insts, &result.lookup_expr_proc_insts, key, ProcInstId.none);
        }

        for (proc_inst_ids) |proc_inst_id| {
            try self.mergeLookupExprProcInstSet(result, context_proc_inst, module_idx, expr_idx, proc_inst_id);
        }
    }

    fn lookupResolvedDispatchTemplate(
        self: *Pass,
        result: *Result,
        source_module_idx: u32,
        resolved_target: ResolvedDispatchTarget,
    ) Allocator.Error!?ProcTemplateId {
        const source_env = self.all_module_envs[source_module_idx];
        const target_module_idx = resolved_target.module_idx orelse self.findModuleForOrigin(source_env, resolved_target.origin);
        const target_env = self.all_module_envs[target_module_idx];
        const method_name = self.dispatchTargetMethodText(source_env, resolved_target) orelse return null;
        const target_ident = target_env.common.findIdent(method_name) orelse return null;
        const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse return null;
        if (!target_env.store.isDefNode(target_node_idx)) return null;

        if (result.getExternalProcTemplate(target_module_idx, target_node_idx)) |template_id| {
            return template_id;
        }

        const def_idx: CIR.Def.Idx = @enumFromInt(target_node_idx);
        const def = target_env.store.getDef(def_idx);
        return try self.registerProcBackedDefTemplate(
            result,
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            packExternalDefSourceKey(target_module_idx, target_node_idx),
        );
    }

    fn moduleIndexForEnv(self: *Pass, env: *const ModuleEnv) ?u32 {
        for (self.all_module_envs, 0..) |candidate, idx| {
            if (candidate == env) return @intCast(idx);
        }
        return null;
    }

    fn findModuleForOrigin(self: *Pass, source_env: *const ModuleEnv, origin_module: Ident.Idx) u32 {
        const source_module_idx = self.moduleIndexForEnv(source_env) orelse unreachable;
        const origin_name = source_env.getIdent(origin_module);

        for (self.all_module_envs, 0..) |candidate, idx| {
            const candidate_name = candidate.getIdent(candidate.qualified_module_ident);
            if (std.mem.eql(u8, candidate_name, origin_name)) {
                return @intCast(idx);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Monomorphize: could not resolve origin module '{s}' from source module {d}",
                .{ origin_name, source_module_idx },
            );
        }
        unreachable;
    }

    fn findModuleForOriginMaybe(self: *Pass, source_env: *const ModuleEnv, origin_module: Ident.Idx) ?u32 {
        const source_module_idx = self.moduleIndexForEnv(source_env) orelse return null;
        if (origin_module.eql(source_env.qualified_module_ident)) return source_module_idx;

        const origin_name = self.identTextAcrossModules(source_env, origin_module) orelse return null;
        for (self.all_module_envs, 0..) |candidate_env, idx| {
            const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
            if (std.mem.eql(u8, origin_name, candidate_name)) return @intCast(idx);
        }
        return null;
    }

    fn identTextAcrossModules(self: *Pass, preferred_env: *const ModuleEnv, ident: Ident.Idx) ?[]const u8 {
        if (moduleOwnsIdent(preferred_env, ident)) return getOwnedIdentText(preferred_env, ident);

        for (self.all_module_envs) |candidate_env| {
            if (candidate_env == preferred_env) continue;
            if (moduleOwnsIdent(candidate_env, ident)) return getOwnedIdentText(candidate_env, ident);
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
                    "Monomorphize: method ident {d} not readable from source module {d} or target module {d}",
                    .{ target.method_ident.idx, source_module_idx, target_module_idx },
                );
            }
            unreachable;
        };

        const target_ident = target_env.common.findIdent(method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: method '{s}' not found in target module {d}",
                    .{ method_name, target_module_idx },
                );
            }
            unreachable;
        };
        const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: exposed node not found for method '{s}' in module {d}",
                    .{ method_name, target_module_idx },
                );
            }
            unreachable;
        };
        if (!target_env.store.isDefNode(target_node_idx)) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: exposed node for method '{s}' is not a def node (module={d}, node={d})",
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
        const mono = result.monotype_store.getMonotype(monotype);

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
        const template_id = (try self.registerProcBackedDefTemplate(
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

    fn ensureBuiltinBoxUnboxProcInst(
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
        const template_id = (try self.registerProcBackedDefTemplate(
            result,
            builtin_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            def.pattern,
            packExternalDefSourceKey(builtin_module_idx, node_idx),
        )) orelse return;

        const args = try result.monotype_store.addIdxSpan(self.allocator, &.{box_monotype});
        const fn_monotype = try result.monotype_store.addMonotype(self.allocator, .{ .func = .{
            .args = args,
            .ret = inner_monotype,
            .effectful = false,
        } });

        _ = try self.ensureProcInst(result, template_id, fn_monotype, source_module_idx);
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
                            "Monomorphize: conflicting exact binding for type var root {d} in module {d}",
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
        return result.monotype_store.fromTypeVar(
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

        const mono = result.monotype_store.getMonotype(monotype);
        switch (mono) {
            .unit => return result.monotype_store.unit_idx,
            .prim => |prim| return result.monotype_store.primIdx(prim),
            .recursive_placeholder => {
                if (builtin.mode == .Debug) {
                    std.debug.panic("remapMonotypeBetweenModules: unexpected recursive_placeholder", .{});
                }
                unreachable;
            },
            .list, .box, .tuple, .func, .record, .tag_union => {},
        }

        const placeholder = try result.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
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
                    const elem_mono = result.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                    try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        result,
                        elem_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ));
                }

                const mapped_elems = try result.monotype_store.addIdxSpan(
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
                    const arg_mono = result.monotype_store.getIdxSpanItem(func_mono.args, arg_i);
                    try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        result,
                        arg_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ));
                }
                const mapped_args = try result.monotype_store.addIdxSpan(
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
                    const field = result.monotype_store.getFieldItem(record_mono.fields, field_i);
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

                const mapped_fields = try result.monotype_store.addFields(
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
                    const tag = result.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                    const payload_top = scratches.idxs.top();
                    defer scratches.idxs.clearFrom(payload_top);

                    var payload_i: usize = 0;
                    while (payload_i < tag.payloads.len) : (payload_i += 1) {
                        const payload_mono = result.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                        try scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                            result,
                            payload_mono,
                            from_module_idx,
                            to_module_idx,
                            remapped,
                            scratches,
                        ));
                    }

                    const mapped_payloads = try result.monotype_store.addIdxSpan(
                        self.allocator,
                        scratches.idxs.sliceFromStart(payload_top),
                    );
                    try scratches.tags.append(.{
                        .name = tag.name,
                        .payloads = mapped_payloads,
                    });
                }

                const mapped_tags = try result.monotype_store.addTags(
                    self.allocator,
                    scratches.tags.sliceFromStart(tags_top),
                );
                break :blk .{ .tag_union = .{ .tags = mapped_tags } };
            },
            .unit, .prim, .recursive_placeholder => unreachable,
        };

        result.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
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
                        "Monomorphize: conflicting monotype binding for type var root {d}",
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

        return result.monotype_store.fromTypeVar(
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

        const mono = result.monotype_store.getMonotype(monotype);

        switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const mfunc = switch (mono) {
                    .func => |mfunc| mfunc,
                    else => unreachable,
                };

                const type_args = store_types.sliceVars(func.args);
                const mono_args = result.monotype_store.getIdxSpan(mfunc.args);
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
                                "Monomorphize invariant violated: non-empty store record matched unit monotype (module={d}, active_proc_inst={d}, monotype={d})",
                                .{
                                    module_idx,
                                    @intFromEnum(self.active_proc_inst_context),
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
                    },
                    else => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "Monomorphize invariant violated: expected record monotype for store record, got {s} (module={d}, active_proc_inst={d}, monotype={d})",
                                .{
                                    @tagName(mono),
                                    module_idx,
                                    @intFromEnum(self.active_proc_inst_context),
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
                    },
                };
                const mono_fields = result.monotype_store.getFields(mrec.fields);
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
                const mono_fields = result.monotype_store.getFields(mrec.fields);
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
                const mono_elems = result.monotype_store.getIdxSpan(mtup.elems);
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
                const mono_tags = result.monotype_store.getTags(mtag.tags);
                const tags = store_types.getTagsSlice(tag_union.tags);
                const tag_args = tags.items(.args);
                if (tag_args.len != mono_tags.len) unreachable;
                for (tag_args, mono_tags) |args_range, mono_tag| {
                    const payload_vars = store_types.sliceVars(args_range);
                    const mono_payloads = result.monotype_store.getIdxSpan(mono_tag.payloads);
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

    fn resolveStrInspectHelperProcInstsForTypeVar(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        type_var: types.Var,
    ) Allocator.Error!void {
        var visiting: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer visiting.deinit(self.allocator);
        try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, type_var, &visiting);
    }

    fn resolveStrInspectHelperProcInstsForTypeVarWithSeen(
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
                                try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, type_args[0], visiting);
                            }
                            return;
                        }
                        if (ident.eql(common.box)) {
                            const type_args = module_env.types.sliceNominalArgs(nominal);
                            const outer_mono = try self.resolveTypeVarMonotype(result, module_idx, resolved.var_);
                            const outer_box = result.monotype_store.getMonotype(outer_mono).box;
                            try self.ensureBuiltinBoxUnboxProcInst(result, module_idx, outer_mono, outer_box.inner);
                            if (type_args.len == 1) {
                                try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, type_args[0], visiting);
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
                                        _ = try self.ensureProcInst(
                                            result,
                                            method_info.template_id,
                                            method_func_mono,
                                            method_info.module_idx,
                                        );

                                        const method_func = switch (result.monotype_store.getMonotype(method_func_mono)) {
                                            .func => |func| func,
                                            else => unreachable,
                                        };
                                        const ret_mono = result.monotype_store.getMonotype(method_func.ret);
                                        if (!(ret_mono == .prim and ret_mono.prim == .str)) {
                                            try self.resolveStrInspectHelperProcInstsForMonotype(
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

                    try self.resolveStrInspectHelperProcInstsForMonotype(
                        result,
                        module_idx,
                        try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
                    );
                    return;
                },
                .record => |record| {
                    try self.resolveStrInspectHelperProcInstsForRecordType(result, module_idx, &module_env.types, record, visiting);
                    return;
                },
                .record_unbound => |fields_range| {
                    const fields = module_env.types.getRecordFieldsSlice(fields_range);
                    for (fields.items(.var_)) |field_var| {
                        try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
                    }
                    return;
                },
                .tuple => |tuple| {
                    for (module_env.types.sliceVars(tuple.elems)) |elem_var| {
                        try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, elem_var, visiting);
                    }
                    return;
                },
                .tag_union => |tag_union| {
                    try self.resolveStrInspectHelperProcInstsForTagUnionType(result, module_idx, &module_env.types, tag_union, visiting);
                    return;
                },
                .empty_record, .empty_tag_union => return,
                else => {},
            }
        }

        try self.resolveStrInspectHelperProcInstsForMonotype(
            result,
            module_idx,
            try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
        );
    }

    fn resolveStrInspectHelperProcInstsForRecordType(
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
                try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
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
                                try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, field_var, visiting);
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

    fn resolveStrInspectHelperProcInstsForTagUnionType(
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
                    try self.resolveStrInspectHelperProcInstsForTypeVarWithSeen(result, module_idx, payload_var, visiting);
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

    fn resolveStrInspectHelperProcInstsForMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        monotype: Monotype.Idx,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        switch (result.monotype_store.getMonotype(monotype)) {
            .unit, .prim => {},
            .list => |list_mono| try self.resolveStrInspectHelperProcInstsForMonotype(result, module_idx, list_mono.elem),
            .box => |box_mono| {
                try self.ensureBuiltinBoxUnboxProcInst(result, module_idx, monotype, box_mono.inner);
                try self.resolveStrInspectHelperProcInstsForMonotype(result, module_idx, box_mono.inner);
            },
            .tuple => |tuple_mono| {
                var elem_i: usize = 0;
                while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                    const elem_mono = result.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                    try self.resolveStrInspectHelperProcInstsForMonotype(result, module_idx, elem_mono);
                }
            },
            .func => {},
            .record => |record_mono| {
                var field_i: usize = 0;
                while (field_i < record_mono.fields.len) : (field_i += 1) {
                    const field = result.monotype_store.getFieldItem(record_mono.fields, field_i);
                    try self.resolveStrInspectHelperProcInstsForMonotype(result, module_idx, field.type_idx);
                }
            },
            .tag_union => |tag_union_mono| {
                var tag_i: usize = 0;
                while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                    const tag = result.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                    var payload_i: usize = 0;
                    while (payload_i < tag.payloads.len) : (payload_i += 1) {
                        const payload_mono = result.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                        try self.resolveStrInspectHelperProcInstsForMonotype(result, module_idx, payload_mono);
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
    ) Allocator.Error!?ProcTemplateId {
        var visiting: std.AutoHashMapUnmanaged(u64, void) = .empty;
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
        if (result.source_exprs.get(key)) |source| return source;

        const target_env = self.all_module_envs[target_module_idx];
        if (!target_env.store.isDefNode(target_node_idx)) return null;

        const def_idx: CIR.Def.Idx = @enumFromInt(target_node_idx);
        const def = target_env.store.getDef(def_idx);
        const source: ExprSource = .{
            .module_idx = target_module_idx,
            .expr_idx = def.expr,
        };
        try self.recordSourceExpr(result, key, target_module_idx, def.expr);
        _ = try self.registerProcBackedDefTemplate(
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

    fn resolveExprCallableTemplateWithVisited(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!?ProcTemplateId {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return null;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        const resolved = switch (expr) {
            .e_lambda, .e_closure, .e_hosted_lambda => blk: {
                if (result.getExprProcTemplate(module_idx, expr_idx)) |template_id| {
                    break :blk template_id;
                }

                const kind = callableKind(expr) orelse unreachable;
                break :blk try self.registerProcTemplate(
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
                if (result.getLocalProcTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                    break :blk template_id;
                }
                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, source.module_idx, source.expr_idx, visiting);
            },
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                if (result.getExternalProcTemplate(target_module_idx, lookup.target_node_idx)) |template_id| {
                    break :blk template_id;
                }
                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse break :blk null;
                break :blk try self.resolveExprCallableTemplateWithVisited(result, source.module_idx, source.expr_idx, visiting);
            },
            .e_lookup_required => |lookup| blk: {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                if (result.getExternalProcTemplate(target.module_idx, target_node_idx)) |template_id| {
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
        if (resolved) |template_id| {
            try self.aliasProcTemplateSource(result, packExprSourceKey(module_idx, expr_idx), template_id);
        }
        return resolved;
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

    fn collectMatchingDemandedProcInstsForTemplate(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        proc_inst_ids: []const ProcInstId,
        out: *std.ArrayList(ProcInstId),
    ) Allocator.Error!void {
        out.clearRetainingCapacity();
        for (proc_inst_ids) |proc_inst_id| {
            if (result.getProcInst(proc_inst_id).template != template_id) continue;
            try out.append(self.allocator, proc_inst_id);
        }
    }

    fn selectDemandedTemplateProcInsts(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        template_id: ProcTemplateId,
        proc_inst_ids: []const ProcInstId,
        matching: *std.ArrayList(ProcInstId),
    ) Allocator.Error!?ProcInstId {
        try self.collectMatchingDemandedProcInstsForTemplate(result, template_id, proc_inst_ids, matching);
        if (matching.items.len == 0) return null;
        if (matching.items.len == 1) return matching.items[0];

        const desired_fn_monotype = try self.resolveExprMonotypeIfMonomorphizableResolved(result, module_idx, expr_idx);
        return switch (try self.selectExistingProcInstForFnMonotype(
            result,
            matching.items,
            desired_fn_monotype,
            template_id,
        )) {
            .one => |proc_inst_id| proc_inst_id,
            .none, .ambiguous => null,
        };
    }

    fn propagateDemandedProcInstsToValueExpr(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_ids: []const ProcInstId,
        visiting: *std.AutoHashMapUnmanaged(u64, void),
    ) Allocator.Error!void {
        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (visiting.contains(visit_key)) return;
        try visiting.put(self.allocator, visit_key, {});
        defer _ = visiting.remove(visit_key);

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);
        var matching_proc_insts = std.ArrayList(ProcInstId).empty;
        defer matching_proc_insts.deinit(self.allocator);
        switch (expr) {
            .e_lambda, .e_closure, .e_hosted_lambda => {
                const template_id = (try self.resolveExprCallableTemplate(result, module_idx, expr_idx)) orelse return;
                if (try self.selectDemandedTemplateProcInsts(
                    result,
                    module_idx,
                    expr_idx,
                    template_id,
                    proc_inst_ids,
                    &matching_proc_insts,
                )) |proc_inst_id| {
                    try self.recordExprProcInst(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        expr_idx,
                        proc_inst_id,
                    );
                } else if (matching_proc_insts.items.len != 0) {
                    try self.recordExprProcInstSet(
                        result,
                        self.active_proc_inst_context,
                        module_idx,
                        expr_idx,
                        matching_proc_insts.items,
                    );
                } else return;
            },
            .e_lookup_local => |lookup| {
                if (result.getLocalProcTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                    if (try self.selectDemandedTemplateProcInsts(
                        result,
                        module_idx,
                        expr_idx,
                        template_id,
                        proc_inst_ids,
                        &matching_proc_insts,
                    )) |proc_inst_id| {
                        try self.recordLookupExprProcInst(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            proc_inst_id,
                        );
                        try self.recordContextPatternProcInst(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            lookup.pattern_idx,
                            proc_inst_id,
                        );
                        return;
                    } else if (matching_proc_insts.items.len != 0) {
                        try self.recordLookupExprProcInstSet(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            matching_proc_insts.items,
                        );
                        for (matching_proc_insts.items) |proc_inst_id| {
                            try self.recordContextPatternProcInst(
                                result,
                                self.active_proc_inst_context,
                                module_idx,
                                lookup.pattern_idx,
                                proc_inst_id,
                            );
                        }
                        return;
                    }
                }

                const source = result.getPatternSourceExpr(module_idx, lookup.pattern_idx) orelse return;
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    proc_inst_ids,
                    visiting,
                );
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                if (result.getExternalProcTemplate(target_module_idx, lookup.target_node_idx)) |template_id| {
                    if (try self.selectDemandedTemplateProcInsts(
                        result,
                        module_idx,
                        expr_idx,
                        template_id,
                        proc_inst_ids,
                        &matching_proc_insts,
                    )) |proc_inst_id| {
                        try self.recordLookupExprProcInst(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            proc_inst_id,
                        );
                        return;
                    } else if (matching_proc_insts.items.len != 0) {
                        try self.recordLookupExprProcInstSet(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            matching_proc_insts.items,
                        );
                        return;
                    }
                }

                const source = try self.resolveExternalDefSourceExpr(result, target_module_idx, lookup.target_node_idx) orelse return;
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    proc_inst_ids,
                    visiting,
                );
            },
            .e_lookup_required => |lookup| {
                const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse return;
                const target_node_idx: u16 = @intCast(@intFromEnum(target.def_idx));
                if (result.getExternalProcTemplate(target.module_idx, target_node_idx)) |template_id| {
                    if (try self.selectDemandedTemplateProcInsts(
                        result,
                        module_idx,
                        expr_idx,
                        template_id,
                        proc_inst_ids,
                        &matching_proc_insts,
                    )) |proc_inst_id| {
                        try self.recordLookupExprProcInst(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            proc_inst_id,
                        );
                        return;
                    } else if (matching_proc_insts.items.len != 0) {
                        try self.recordLookupExprProcInstSet(
                            result,
                            self.active_proc_inst_context,
                            module_idx,
                            expr_idx,
                            matching_proc_insts.items,
                        );
                        return;
                    }
                }

                const source = try self.resolveExternalDefSourceExpr(result, target.module_idx, target_node_idx) orelse return;
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    source.module_idx,
                    source.expr_idx,
                    proc_inst_ids,
                    visiting,
                );
            },
            .e_if => |if_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.propagateDemandedProcInstsToValueExpr(result, module_idx, branch.body, proc_inst_ids, visiting);
                }
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, if_expr.final_else, proc_inst_ids, visiting);
            },
            .e_match => |match_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.propagateDemandedProcInstsToValueExpr(result, module_idx, branch.value, proc_inst_ids, visiting);
                }
            },
            .e_block => |block_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, block_expr.final_expr, proc_inst_ids, visiting);
            },
            .e_dbg => |dbg_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, dbg_expr.expr, proc_inst_ids, visiting);
            },
            .e_expect => |expect_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, expect_expr.body, proc_inst_ids, visiting);
            },
            .e_return => |return_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, return_expr.expr, proc_inst_ids, visiting);
            },
            .e_nominal => |nominal_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, nominal_expr.backing_expr, proc_inst_ids, visiting);
            },
            .e_nominal_external => |nominal_expr| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                try self.propagateDemandedProcInstsToValueExpr(result, module_idx, nominal_expr.backing_expr, proc_inst_ids, visiting);
            },
            .e_dot_access => |dot_expr| {
                if (dot_expr.args != null) return;
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                const field_expr = try self.resolveRecordFieldExpr(
                    result,
                    module_idx,
                    dot_expr.receiver,
                    module_idx,
                    dot_expr.field_name,
                    visiting,
                ) orelse return;
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    field_expr.module_idx,
                    field_expr.expr_idx,
                    proc_inst_ids,
                    visiting,
                );
            },
            .e_tuple_access => |tuple_access| {
                try self.recordDemandedValueExprProcInsts(result, module_idx, expr_idx, proc_inst_ids);
                const elem_expr = try self.resolveTupleElemExpr(
                    result,
                    module_idx,
                    tuple_access.tuple,
                    tuple_access.elem_index,
                    visiting,
                ) orelse return;
                try self.propagateDemandedProcInstsToValueExpr(
                    result,
                    elem_expr.module_idx,
                    elem_expr.expr_idx,
                    proc_inst_ids,
                    visiting,
                );
            },
            else => {},
        }
    }

    fn recordDemandedValueExprProcInsts(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        proc_inst_ids: []const ProcInstId,
    ) Allocator.Error!void {
        if (proc_inst_ids.len == 0) return;
        if (proc_inst_ids.len == 1) {
            try self.recordExprProcInst(
                result,
                self.active_proc_inst_context,
                module_idx,
                expr_idx,
                proc_inst_ids[0],
            );
            return;
        }

        try self.recordExprProcInstSet(
            result,
            self.active_proc_inst_context,
            module_idx,
            expr_idx,
            proc_inst_ids,
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
    ) Allocator.Error!?ProcTemplateId {
        return self.resolveExprCallableTemplate(result, module_idx, callee_expr_idx);
    }

    fn ensureProcInst(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!ProcInstId {
        return self.ensureProcInstWithScan(result, template_id, fn_monotype, fn_monotype_module_idx, true);
    }

    fn ensureProcInstUnscanned(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!ProcInstId {
        return self.ensureProcInstWithScan(result, template_id, fn_monotype, fn_monotype_module_idx, false);
    }

    fn ensureProcInstWithScan(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
        scan_body: bool,
    ) Allocator.Error!ProcInstId {
        const template = result.getProcTemplate(template_id);
        const defining_context_proc_inst = self.resolveTemplateDefiningContextProcInst(result, template.*);
        const subst_id = if (self.all_module_envs[template.module_idx].types.needsInstantiation(template.type_root))
            try self.ensureTypeSubst(result, template.*, fn_monotype, fn_monotype_module_idx)
        else
            TypeSubstId.none;

        for (result.proc_insts.items, 0..) |existing_proc_inst, idx| {
            if (existing_proc_inst.template != template_id) continue;
            if (existing_proc_inst.fn_monotype_module_idx != fn_monotype_module_idx) continue;
            if (existing_proc_inst.defining_context_proc_inst != defining_context_proc_inst) continue;
            const mono_equal = try self.monotypesStructurallyEqual(result, existing_proc_inst.fn_monotype, fn_monotype);
            if (mono_equal) {
                if (existing_proc_inst.subst != subst_id) continue;
                const existing_id: ProcInstId = @enumFromInt(idx);
                if (scan_body) {
                    try self.scanProcInst(result, existing_id);
                }
                return existing_id;
            }
        }

        const proc_inst_id: ProcInstId = @enumFromInt(result.proc_insts.items.len);
        try self.appendTracked(.proc_insts, &result.proc_insts, ProcInst{
            .template = template_id,
            .subst = subst_id,
            .fn_monotype = fn_monotype,
            .fn_monotype_module_idx = fn_monotype_module_idx,
            .defining_context_proc_inst = defining_context_proc_inst,
        });
        if (scan_body) {
            try self.scanProcInst(result, proc_inst_id);
        }
        return proc_inst_id;
    }

    fn resolveTemplateDefiningContextProcInst(
        self: *Pass,
        result: *const Result,
        template: ProcTemplate,
    ) ProcInstId {
        switch (template.kind) {
            .top_level_def, .lambda, .hosted_lambda => return .none,
            .closure => {
                if (template.lexical_owner_template.isNone()) return .none;
                if (self.active_proc_inst_context.isNone()) {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Monomorphize: closure template expr={d} requires lexical owner template={d} but no active proc inst is available",
                            .{
                                @intFromEnum(template.cir_expr),
                                @intFromEnum(template.lexical_owner_template),
                            },
                        );
                    }
                    unreachable;
                }

                var current = self.active_proc_inst_context;
                while (!current.isNone()) {
                    const current_proc_inst = result.getProcInst(current);
                    if (current_proc_inst.template == template.lexical_owner_template) {
                        return current;
                    }
                    current = current_proc_inst.defining_context_proc_inst;
                }

                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: closure template expr={d} could not resolve lexical owner template={d} from active proc inst={d}",
                        .{
                            @intFromEnum(template.cir_expr),
                            @intFromEnum(template.lexical_owner_template),
                            @intFromEnum(self.active_proc_inst_context),
                        },
                    );
                }
                unreachable;
            },
        }
    }

    fn scanProcInst(self: *Pass, result: *Result, proc_inst_id: ProcInstId) Allocator.Error!void {
        const proc_inst_key = @intFromEnum(proc_inst_id);
        if (self.completed_proc_scans.contains(proc_inst_key)) return;
        if (self.in_progress_proc_scans.contains(proc_inst_key)) return;
        // Snapshot these by value before scanning. `scanModule` can discover more
        // demanded callables and append to both arrays, which would invalidate pointers.
        const proc_inst = result.getProcInst(proc_inst_id).*;
        const template = result.getProcTemplate(proc_inst.template).*;
        const defining_context_proc_inst = proc_inst.defining_context_proc_inst;
        try self.primeModuleDefs(result, template.module_idx);
        try self.in_progress_proc_scans.put(self.allocator, proc_inst_key, {});
        defer _ = self.in_progress_proc_scans.remove(proc_inst_key);

        const module_env = self.all_module_envs[template.module_idx];

        var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
        defer bindings.deinit();
        var iteration_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype) = .empty;
        defer iteration_expr_monotypes.deinit(self.allocator);

        const saved_template_context = self.active_template_context;
        self.active_template_context = proc_inst.template;
        defer self.active_template_context = saved_template_context;
        const saved_proc_inst_context = self.active_proc_inst_context;
        self.active_proc_inst_context = proc_inst_id;
        defer self.active_proc_inst_context = saved_proc_inst_context;

        try self.seedBindingsForProcInst(result, proc_inst_id, &bindings);

        const saved_bindings = self.active_bindings;
        self.active_bindings = &bindings;
        defer self.active_bindings = saved_bindings;
        const saved_iteration_expr_monotypes = self.active_iteration_expr_monotypes;
        self.active_iteration_expr_monotypes = &iteration_expr_monotypes;
        defer self.active_iteration_expr_monotypes = saved_iteration_expr_monotypes;
        if (template.binding_pattern) |binding_pattern| {
            try self.recordContextPatternProcInst(
                result,
                proc_inst_id,
                template.module_idx,
                binding_pattern,
                proc_inst_id,
            );
        }

        var iterations: u32 = 0;
        while (true) {
            iterations += 1;
            if (std.debug.runtime_safety and iterations > 32) {
                std.debug.panic(
                    "Monomorphize: proc-inst binding fixed point did not converge for proc_inst={d}",
                    .{
                        @intFromEnum(proc_inst_id),
                    },
                );
            }

            const bindings_before = bindings.count();
            const mutation_revision_before = self.mutation_revision;
            iteration_expr_monotypes.clearRetainingCapacity();

            switch (module_env.store.getExpr(template.cir_expr)) {
                .e_lambda => |lambda_expr| try self.scanValueExpr(result, template.module_idx, lambda_expr.body),
                .e_closure => |closure_expr| {
                    const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                    if (lambda_expr == .e_lambda) {
                        try self.scanValueExpr(result, template.module_idx, lambda_expr.e_lambda.body);
                    }
                    try self.scanClosureCaptureSources(
                        result,
                        defining_context_proc_inst,
                        template.module_idx,
                        template.cir_expr,
                        closure_expr,
                    );
                    try self.recordClosureCaptureFactsForProcInst(
                        result,
                        defining_context_proc_inst,
                        template.module_idx,
                        template.cir_expr,
                        closure_expr,
                        proc_inst_id,
                    );
                },
                .e_hosted_lambda => |hosted_expr| try self.scanValueExpr(result, template.module_idx, hosted_expr.body),
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

        if (self.scratch_context_expr_monotypes_depth == 0) {
            try self.completed_proc_scans.put(self.allocator, proc_inst_key, {});
        }
    }

    fn seedProcBodyBindingsFromSignature(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        proc_expr_idx: CIR.Expr.Idx,
        proc_inst: ProcInst,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const proc_expr = module_env.store.getExpr(proc_expr_idx);
        const fn_mono = switch (result.monotype_store.getMonotype(proc_inst.fn_monotype)) {
            .func => |func| func,
            else => return,
        };
        const ProcBoundary = struct {
            arg_patterns: []const CIR.Pattern.Idx,
            body_expr: CIR.Expr.Idx,
        };

        const boundary: ProcBoundary = switch (proc_expr) {
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
                    "Monomorphize: proc signature arity mismatch for expr {d} (patterns={d}, monos={d})",
                    .{
                        @intFromEnum(proc_expr_idx),
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
            ModuleEnv.varFrom(proc_expr_idx),
            proc_inst.fn_monotype,
            proc_inst.fn_monotype_module_idx,
        );

        for (boundary.arg_patterns, 0..) |pattern_idx, i| {
            const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
            try self.bindTypeVarMonotypes(
                result,
                module_idx,
                &module_env.types,
                bindings,
                &ordered_entries,
                ModuleEnv.varFrom(pattern_idx),
                param_mono,
                proc_inst.fn_monotype_module_idx,
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
            proc_inst.fn_monotype_module_idx,
        );
    }

    fn seedTemplateBodyBindingsFromCurrentBindings(
        self: *Pass,
        result: *Result,
        template: ProcTemplate,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.procBoundaryInfo(template.module_idx, template.cir_expr) orelse return;
        const resolved_func = resolveFuncTypeInStore(&module_env.types, template.type_root) orelse return;

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        if (try self.resolveTemplateTypeVarWithBindings(
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
                    "Monomorphize: template boundary arity mismatch for expr {d} (patterns={d}, vars={d})",
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
            if (try self.resolveTemplateTypeVarWithBindings(
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

        if (try self.resolveTemplateTypeVarWithBindings(
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

    fn resolveTemplateTypeVarWithBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        type_var: types.Var,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!?ResolvedMonotype {
        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (!try self.typeVarMonomorphizableWithBindings(
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
        template: ProcTemplate,
        actual_args: []const CIR.Expr.Idx,
        ret_mono: ResolvedMonotype,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.procBoundaryInfo(template.module_idx, template.cir_expr) orelse return;

        if (boundary.arg_patterns.len != actual_args.len) return;

        var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
        defer ordered_entries.deinit(self.allocator);

        for (boundary.arg_patterns, actual_args) |pattern_idx, arg_expr_idx| {
            const arg_mono = try self.resolveExprMonotypeIfExactResolved(result, actual_module_idx, arg_expr_idx);
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
        template: ProcTemplate,
        fn_monotype: ResolvedMonotype,
        bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ) Allocator.Error!void {
        if (fn_monotype.isNone()) return;

        const module_env = self.all_module_envs[template.module_idx];
        const boundary = self.procBoundaryInfo(template.module_idx, template.cir_expr) orelse return;
        const fn_mono = switch (result.monotype_store.getMonotype(fn_monotype.idx)) {
            .func => |func| func,
            else => return,
        };

        if (boundary.arg_patterns.len != fn_mono.args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: template fn-monotype arity mismatch for expr {d} (patterns={d}, monos={d})",
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
            const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
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
        template: ProcTemplate,
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

        for (result.substs.items, 0..) |existing_subst, idx| {
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
            const start: u32 = @intCast(result.subst_entries.items.len);
            try result.subst_entries.appendSlice(self.allocator, ordered_entries.items);
            break :blk TypeSubstSpan{
                .start = start,
                .len = @as(u16, @intCast(ordered_entries.items.len)),
            };
        };

        const subst_id: TypeSubstId = @enumFromInt(result.substs.items.len);
        try self.appendTracked(.substs, &result.substs, TypeSubst{ .entries = entries_span });
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
                "Monomorphize: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
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
            const mono_field = result.monotype_store.getFieldItem(mono_fields, field_i);
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
                "Monomorphize: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
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
    ) ?u32 {
        var tag_i: usize = 0;
        while (tag_i < mono_tags.len) : (tag_i += 1) {
            const mono_tag = result.monotype_store.getTagItem(mono_tags, tag_i);
            if (self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) {
                return @intCast(tag_i);
            }
        }
        return null;
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
            return result.monotype_store.unit_idx;
        }

        const field_span = try result.monotype_store.addFields(self.allocator, remaining_fields.items);
        return try result.monotype_store.addMonotype(self.allocator, .{ .record = .{ .fields = field_span } });
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

        const tag_span = try result.monotype_store.addTags(self.allocator, remaining_tags.items);
        return try result.monotype_store.addMonotype(self.allocator, .{ .tag_union = .{ .tags = tag_span } });
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
            const mono_tag = result.monotype_store.getTagItem(mono_tags, tag_i);
            if (!self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) continue;

            if (payload_vars.len != mono_tag.payloads.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: payload arity mismatch for tag '{s}'",
                        .{self.all_module_envs[template_module_idx].getIdent(tag_name)},
                    );
                }
                unreachable;
            }

            for (payload_vars, 0..) |payload_var, i| {
                const mono_payload = result.monotype_store.getIdxSpanItem(mono_tag.payloads, i);
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

        // Tag absent from monotype — nothing to bind. This happens when a
        // polymorphic function matches on more tags than the concrete call
        // site provides (e.g. matching on Try but the value is always Ok).
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
                    const context_template: ?ProcTemplate = if (!self.active_proc_inst_context.isNone())
                        result.getProcTemplate(result.getProcInst(self.active_proc_inst_context).template).*
                    else
                        null;
                    std.debug.panic(
                        "Monomorphize: conflicting monotype binding for type var root {d} in module {d} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} ctx={d} root_expr={d} template_expr={d} template_kind={s}",
                        .{
                            @intFromEnum(resolved_key.type_var),
                            resolved_key.module_idx,
                            @intFromEnum(existing.idx),
                            existing.module_idx,
                            result.monotype_store.getMonotype(existing.idx),
                            @intFromEnum(resolved_mono.idx),
                            resolved_mono.module_idx,
                            result.monotype_store.getMonotype(resolved_mono.idx),
                            @intFromEnum(self.active_proc_inst_context),
                            if (self.active_root_expr_context) |root_expr_idx| @intFromEnum(root_expr_idx) else std.math.maxInt(u32),
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

        const mono = result.monotype_store.getMonotype(monotype);
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
                    const mono_arg = result.monotype_store.getIdxSpanItem(mfunc.args, i);
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
                                "Monomorphize invariant violated: non-empty template record matched unit monotype (template_module={d}, mono_module={d}, active_proc_inst={d}, monotype={d})",
                                .{
                                    template_module_idx,
                                    mono_module_idx,
                                    @intFromEnum(self.active_proc_inst_context),
                                    @intFromEnum(monotype),
                                },
                            );
                        }
                        unreachable;
                    },
                    else => {
                        if (builtin.mode == .Debug) {
                            std.debug.panic(
                                "Monomorphize invariant violated: expected record monotype for template record, got {s} (template_module={d}, mono_module={d}, active_proc_inst={d}, monotype={d})",
                                .{
                                    @tagName(mono),
                                    template_module_idx,
                                    mono_module_idx,
                                    @intFromEnum(self.active_proc_inst_context),
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
                        const mono_field = result.monotype_store.getFieldItem(mrec.fields, field_idx);
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
                                        const mono_field = result.monotype_store.getFieldItem(mrec.fields, field_idx);
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
                                    result.monotype_store.getFields(mrec.fields),
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
                    const mono_field = result.monotype_store.getFieldItem(mrec.fields, field_idx);
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
                    const elem_mono = result.monotype_store.getIdxSpanItem(mtup.elems, i);
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
                        // A template tag may be absent from the monotype when a
                        // polymorphic function (e.g. matching on Try(ok, err)) is
                        // called with a value whose concrete type has fewer tags
                        // (e.g. only Ok). Skip binding for the missing tag — its
                        // type variables are unused in this specialization.
                        const tag_idx = self.tagIndexByNameInSpan(
                            result,
                            template_module_idx,
                            tag_name,
                            mono_module_idx,
                            mtag.tags,
                        ) orelse continue;
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
                                    result.monotype_store.getTags(mtag.tags),
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
                "Monomorphize bindFlatTypeMonotypes mismatch: flat_type={s} mono={s} template_module={d} mono_module={d} active_proc_inst={d} monotype={d} probe_mode={}",
                .{
                    @tagName(flat_type),
                    @tagName(mono),
                    template_module_idx,
                    mono_module_idx,
                    @intFromEnum(self.active_proc_inst_context),
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
                "Monomorphize bindFlatTypeMonotypes hit err tail: flat_type={s} template_module={d} mono_module={d} active_proc_inst={d} monotype={d} probe_mode={}",
                .{
                    @tagName(flat_type),
                    template_module_idx,
                    mono_module_idx,
                    @intFromEnum(self.active_proc_inst_context),
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
                    const pattern_mono = try self.resolveTypeVarMonotypeIfExactResolved(
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

    fn resolveExprMonotypeIfExactResolved(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!ResolvedMonotype {
        if (self.lookupCurrentExprMonotype(result, module_idx, expr_idx)) |resolved| {
            return resolved;
        }

        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        // Invocation nodes get their exact monotype from explicit proc-inst
        // resolution in root/global contexts. During active binding completion,
        // the invocation's own CIR type root is part of the current demanded
        // specialization and can be read directly from the active bindings.
        if (exprMonotypeOwnedByInvocation(expr)) {
            if (self.active_bindings != null) {
                return self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, ModuleEnv.varFrom(expr_idx));
            }
            return resolvedMonotype(.none, module_idx);
        }

        if (self.exprUsesContextSensitiveNumericDefault(module_idx, expr_idx)) {
            return resolvedMonotype(.none, module_idx);
        }

        if (self.active_bindings != null) {
            switch (expr) {
                .e_lookup_local => |lookup| {
                    const pattern_mono = try self.resolveTypeVarMonotypeIfExactResolved(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(lookup.pattern_idx),
                    );
                    if (!pattern_mono.isNone()) return pattern_mono;
                },
                else => {},
            }
        }

        return self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, ModuleEnv.varFrom(expr_idx));
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
        if (self.active_bindings != null) {
            return self.resolveTypeVarMonotypeIfExactResolved(result, module_idx, var_);
        }
        const mono = try self.monotypeFromTypeVarInStore(result, module_idx, &self.all_module_envs[module_idx].types, var_);
        return resolvedMonotype(mono, module_idx);
    }

    fn resolveTypeVarMonotypeIfExactResolved(
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

    fn resolveTypeVarMonotypeIfMonomorphizableResolved(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!ResolvedMonotype {
        if (self.active_bindings != null) {
            const bindings = self.active_bindings.?;
            var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
            defer seen.deinit(self.allocator);

            if (!try self.typeVarMonomorphizableWithBindings(
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

        var seen: std.AutoHashMapUnmanaged(types.Var, void) = .empty;
        defer seen.deinit(self.allocator);

        if (!try self.typeVarMonomorphizableWithoutBindings(
            result,
            module_idx,
            &self.all_module_envs[module_idx].types,
            var_,
            &seen,
        )) {
            return resolvedMonotype(.none, module_idx);
        }

        return self.resolveTypeVarMonotypeResolved(result, module_idx, var_);
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

    fn typeVarMonomorphizableWithBindings(
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
        defer _ = seen.remove(resolved.var_);

        return switch (resolved.desc.content) {
            .flex, .rigid => true,
            .alias => |alias| self.typeVarMonomorphizableWithBindings(
                result,
                module_idx,
                store_types,
                store_types.getAliasBackingVar(alias),
                bindings,
                seen,
            ),
            .structure => |flat_type| self.flatTypeMonomorphizableWithBindings(
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

    fn typeVarMonomorphizableWithoutBindings(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        store_types: *const types.Store,
        var_: types.Var,
        seen: *std.AutoHashMapUnmanaged(types.Var, void),
    ) Allocator.Error!bool {
        {
            var empty_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(self.allocator);
            defer empty_bindings.deinit();
            if (try lookupBindingMonotype(self, result, module_idx, store_types, &empty_bindings, var_)) |_| return true;
        }

        const resolved = store_types.resolveVar(var_);
        if (seen.contains(resolved.var_)) return true;
        try seen.put(self.allocator, resolved.var_, {});
        defer _ = seen.remove(resolved.var_);

        return switch (resolved.desc.content) {
            .flex, .rigid => true,
            .alias => |alias| self.typeVarMonomorphizableWithoutBindings(
                result,
                module_idx,
                store_types,
                store_types.getAliasBackingVar(alias),
                seen,
            ),
            .structure => |flat_type| self.flatTypeMonomorphizableWithoutBindings(
                result,
                module_idx,
                store_types,
                flat_type,
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
                break :blk try self.typeVarFullyBoundWithBindings(result, module_idx, store_types, tag_union.ext, bindings, seen);
            },
            .empty_record, .empty_tag_union => true,
        };
    }

    fn flatTypeMonomorphizableWithoutBindings(
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
                    if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, arg_var, seen)) {
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
                        if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, field_var, seen)) {
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
                                            if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                                                break :blk false;
                                            }
                                        }
                                        break :blk true;
                                    },
                                    .empty_record => break :blk true,
                                    else => break :blk try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, backing, seen),
                                }
                            }
                            break :blk try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, backing, seen);
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                for (ext_fields.items(.var_)) |field_var| {
                                    if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            },
                            .empty_record => break :blk true,
                            else => break :blk false,
                        },
                        .flex, .rigid => break :blk true,
                        else => break :blk try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, current_row.ext, seen),
                    }
                }
            },
            .record_unbound => |fields_range| blk: {
                const fields_slice = store_types.getRecordFieldsSlice(fields_range);
                for (fields_slice.items(.var_)) |field_var| {
                    if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, field_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, elem_var, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const tags = store_types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |args_range| {
                    for (store_types.sliceVars(args_range)) |payload_var| {
                        if (!try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, payload_var, seen)) {
                            break :blk false;
                        }
                    }
                }

                const ext_resolved = store_types.resolveVar(tag_union.ext);
                switch (ext_resolved.desc.content) {
                    .flex, .rigid => break :blk true,
                    else => break :blk try self.typeVarMonomorphizableWithoutBindings(result, module_idx, store_types, tag_union.ext, seen),
                }
            },
            .empty_record, .empty_tag_union => true,
        };
    }

    fn flatTypeMonomorphizableWithBindings(
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
                    if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, arg_var, bindings, seen)) {
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
                        if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
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
                                            if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                                                break :blk false;
                                            }
                                        }
                                        break :blk true;
                                    },
                                    .empty_record => break :blk true,
                                    else => break :blk try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, backing, bindings, seen),
                                }
                            }
                            break :blk try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, backing, bindings, seen);
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                for (ext_fields.items(.var_)) |field_var| {
                                    if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                                        break :blk false;
                                    }
                                }
                                break :blk true;
                            },
                            .empty_record => break :blk true,
                            else => break :blk false,
                        },
                        .flex, .rigid => break :blk true,
                        else => break :blk try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, current_row.ext, bindings, seen),
                    }
                }
            },
            .record_unbound => |fields_range| blk: {
                const fields_slice = store_types.getRecordFieldsSlice(fields_range);
                for (fields_slice.items(.var_)) |field_var| {
                    if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, field_var, bindings, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tuple => |tuple| blk: {
                for (store_types.sliceVars(tuple.elems)) |elem_var| {
                    if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, elem_var, bindings, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const tags = store_types.getTagsSlice(tag_union.tags);
                for (tags.items(.args)) |args_range| {
                    for (store_types.sliceVars(args_range)) |payload_var| {
                        if (!try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, payload_var, bindings, seen)) {
                            break :blk false;
                        }
                    }
                }

                const ext_resolved = store_types.resolveVar(tag_union.ext);
                switch (ext_resolved.desc.content) {
                    .flex, .rigid => break :blk true,
                    else => break :blk try self.typeVarMonomorphizableWithBindings(result, module_idx, store_types, tag_union.ext, bindings, seen),
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

        const lhs_mono = result.monotype_store.getMonotype(lhs);
        const rhs_mono = result.monotype_store.getMonotype(rhs);
        if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

        return switch (lhs_mono) {
            .recursive_placeholder => unreachable,
            .unit => true,
            .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
            .list => |lhs_list| try self.monotypesStructurallyEqualRec(result, lhs_list.elem, rhs_mono.list.elem, seen),
            .box => |lhs_box| try self.monotypesStructurallyEqualRec(result, lhs_box.inner, rhs_mono.box.inner, seen),
            .tuple => |lhs_tuple| blk: {
                const lhs_elems = result.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = result.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
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
                const lhs_args = result.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = result.monotype_store.getIdxSpan(rhs_func.args);
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
                const lhs_fields = result.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = result.monotype_store.getFields(rhs_mono.record.fields);
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
                const lhs_tags = result.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = result.monotype_store.getTags(rhs_mono.tag_union.tags);
                if (lhs_tags.len != rhs_tags.len) break :blk false;
                for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                    const lhs_payloads = result.monotype_store.getIdxSpan(lhs_tag.payloads);
                    const rhs_payloads = result.monotype_store.getIdxSpan(rhs_tag.payloads);
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

        const lhs_mono = result.monotype_store.getMonotype(lhs);
        const rhs_mono = result.monotype_store.getMonotype(rhs);
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
                const lhs_elems = result.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = result.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
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
                const lhs_args = result.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = result.monotype_store.getIdxSpan(rhs_func.args);
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
                const lhs_fields = result.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = result.monotype_store.getFields(rhs_mono.record.fields);
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
                const lhs_tags = result.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = result.monotype_store.getTags(rhs_mono.tag_union.tags);
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

                        const lhs_payloads = result.monotype_store.getIdxSpan(lhs_tag.payloads);
                        const rhs_payloads = result.monotype_store.getIdxSpan(rhs_tag.payloads);
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

        for (self.all_module_envs, 0..) |candidate_env, module_idx| {
            if (std.mem.eql(u8, candidate_env.module_name, import_name) or
                std.mem.eql(u8, candidate_env.module_name, base_name))
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
            const def_expr = module_env.store.getExpr(def.expr);
            if (def_expr == .e_lambda) {
                const body_expr = module_env.store.getExpr(def_expr.e_lambda.body);
                if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
            }
            return null;
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
        const def_expr = ext_env.store.getExpr(def.expr);
        if (def_expr == .e_lambda) {
            const body_expr = ext_env.store.getExpr(def_expr.e_lambda.body);
            if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
        }
        return null;
    }
};

fn callableKind(expr: CIR.Expr) ?ProcTemplateKind {
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

/// Monomorphize one expression tree rooted in the given module.
pub fn runExpr(
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
    return pass.runExpr(expr_idx);
}

/// Monomorphize one root expression using an explicit type scope for cross-module substitutions.
pub fn runExprWithTypeScope(
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
    return pass.runExpr(expr_idx);
}

/// Monomorphize an explicit set of root expressions in the current module.
pub fn runRoots(
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
    return pass.runRoots(exprs);
}

/// Monomorphize explicit root expressions using an explicit type scope for cross-module substitutions.
pub fn runRootsWithTypeScope(
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
    return pass.runRoots(exprs);
}

/// Monomorphize all callables rooted in the current module.
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

/// Monomorphize all callables rooted in the current module using an explicit type scope.
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
