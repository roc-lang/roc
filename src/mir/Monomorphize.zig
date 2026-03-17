//! Explicit callable monomorphization.
//!
//! This module owns the compiler phase that decides which callable instance
//! every use site means before MIR lowering begins. `Lower` must consume the
//! result of this pass; it must not infer callable instantiations itself.

const std = @import("std");
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

/// One concrete type-variable assignment inside a proc instantiation substitution.
pub const TypeSubstEntry = struct {
    type_var: types.Var,
    monotype: Monotype.Idx,
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
    kind: ProcTemplateKind = .top_level_def,
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
};

/// Associates a source call site with the proc instantiation chosen for it.
pub const CallSiteResolution = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    proc_inst: ProcInstId,
};

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
};

/// Output of the MIR monomorphization pass.
pub const Result = struct {
    monotype_store: Monotype.Store,
    proc_templates: std.ArrayListUnmanaged(ProcTemplate),
    proc_insts: std.ArrayListUnmanaged(ProcInst),
    subst_entries: std.ArrayListUnmanaged(TypeSubstEntry),
    substs: std.ArrayListUnmanaged(TypeSubst),
    call_site_proc_insts: std.AutoHashMapUnmanaged(u64, ProcInstId),
    dispatch_expr_proc_insts: std.AutoHashMapUnmanaged(u64, ProcInstId),
    lookup_expr_proc_insts: std.AutoHashMapUnmanaged(u64, ProcInstId),
    proc_template_ids_by_source: std.AutoHashMapUnmanaged(u64, ProcTemplateId),
    deferred_local_callables: std.AutoHashMapUnmanaged(u64, DeferredLocalCallable),
    root_module_idx: u32,
    root_expr_idx: ?CIR.Expr.Idx,

    pub fn init(allocator: Allocator, root_module_idx: u32, root_expr_idx: ?CIR.Expr.Idx) !Result {
        return .{
            .monotype_store = try Monotype.Store.init(allocator),
            .proc_templates = .empty,
            .proc_insts = .empty,
            .subst_entries = .empty,
            .substs = .empty,
            .call_site_proc_insts = .empty,
            .dispatch_expr_proc_insts = .empty,
            .lookup_expr_proc_insts = .empty,
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
        self.subst_entries.deinit(allocator);
        self.substs.deinit(allocator);
        self.call_site_proc_insts.deinit(allocator);
        self.dispatch_expr_proc_insts.deinit(allocator);
        self.lookup_expr_proc_insts.deinit(allocator);
        self.proc_template_ids_by_source.deinit(allocator);
        self.deferred_local_callables.deinit(allocator);
    }

    pub fn callSiteKey(module_idx: u32, expr_idx: CIR.Expr.Idx) u64 {
        return (@as(u64, module_idx) << 32) | @as(u64, @intFromEnum(expr_idx));
    }

    pub fn getCallSiteProcInst(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ProcInstId {
        return self.call_site_proc_insts.get(callSiteKey(module_idx, expr_idx));
    }

    pub fn getDispatchExprProcInst(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ProcInstId {
        return self.dispatch_expr_proc_insts.get(callSiteKey(module_idx, expr_idx));
    }

    pub fn getLookupExprProcInst(self: *const Result, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ProcInstId {
        return self.lookup_expr_proc_insts.get(callSiteKey(module_idx, expr_idx));
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
};

/// Monomorphizes callable templates into explicit proc instantiations.
pub const Pass = struct {
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
    visited_modules: std.AutoHashMapUnmanaged(u32, void),
    visited_exprs: std.AutoHashMapUnmanaged(u64, void),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(u64, ResolvedDispatchTarget),
    scanned_proc_insts: std.AutoHashMapUnmanaged(u32, void),
    active_bindings: ?*const std.AutoHashMap(types.Var, Monotype.Idx),

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
            .resolved_dispatch_targets = .empty,
            .scanned_proc_insts = .empty,
            .active_bindings = null,
        };
    }

    pub fn deinit(self: *Pass) void {
        self.visited_modules.deinit(self.allocator);
        self.visited_exprs.deinit(self.allocator);
        self.resolved_dispatch_targets.deinit(self.allocator);
        self.scanned_proc_insts.deinit(self.allocator);
    }

    pub fn runExpr(self: *Pass, expr_idx: CIR.Expr.Idx) Allocator.Error!Result {
        var result = try Result.init(self.allocator, self.current_module_idx, expr_idx);

        try self.seedResolvedDispatchTargets();
        try self.scanModule(&result, self.current_module_idx);
        try self.scanExpr(&result, self.current_module_idx, expr_idx);

        return result;
    }

    /// Monomorphize all reachable callables in the current module.
    pub fn runModule(self: *Pass) Allocator.Error!Result {
        var result = try Result.init(self.allocator, self.current_module_idx, null);

        try self.seedResolvedDispatchTargets();
        try self.scanModule(&result, self.current_module_idx);

        return result;
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

                const key = (@as(u64, @intCast(mod_idx)) << 32) | @as(u64, constraint.source_expr_idx);
                try self.resolved_dispatch_targets.put(self.allocator, key, .{
                    .origin = constraint.resolved_target.origin_module,
                    .method_ident = constraint.resolved_target.method_ident,
                    .fn_var = constraint.fn_var,
                });
            }
        }
    }

    fn scanModule(self: *Pass, result: *Result, module_idx: u32) Allocator.Error!void {
        if (self.visited_modules.contains(module_idx)) return;
        try self.visited_modules.put(self.allocator, module_idx, {});

        const module_env = self.all_module_envs[module_idx];
        const defs = module_env.store.sliceDefs(module_env.all_defs);

        for (defs) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const expr = module_env.store.getExpr(def.expr);

            if (callableKind(expr)) |kind| {
                _ = try self.registerProcTemplate(
                    result,
                    packLocalPatternSourceKey(module_idx, def.pattern),
                    module_idx,
                    def.expr,
                    ModuleEnv.varFrom(def.pattern),
                    kind,
                    module_env.store.getExprRegion(def.expr),
                );
            } else {
                try self.scanExpr(result, module_idx, def.expr);
            }
        }
    }

    fn registerProcTemplate(
        self: *Pass,
        result: *Result,
        source_key: u64,
        module_idx: u32,
        cir_expr: CIR.Expr.Idx,
        type_root: types.Var,
        kind: ProcTemplateKind,
        source_region: Region,
    ) Allocator.Error!ProcTemplateId {
        if (result.proc_template_ids_by_source.get(source_key)) |existing| return existing;

        const proc_template_id: ProcTemplateId = @enumFromInt(result.proc_templates.items.len);
        try result.proc_templates.append(self.allocator, .{
            .source_key = source_key,
            .module_idx = module_idx,
            .cir_expr = cir_expr,
            .type_root = type_root,
            .kind = kind,
            .source_region = source_region,
        });
        try result.proc_template_ids_by_source.put(self.allocator, source_key, proc_template_id);

        return proc_template_id;
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

        try result.deferred_local_callables.put(self.allocator, source_key, .{
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
                const expr = module_env.store.getExpr(decl.expr);
                if (callableKind(expr)) |kind| {
                    _ = try self.registerProcTemplate(
                        result,
                        packLocalPatternSourceKey(module_idx, decl.pattern),
                        module_idx,
                        decl.expr,
                        ModuleEnv.varFrom(decl.pattern),
                        kind,
                        module_env.store.getExprRegion(decl.expr),
                    );
                    try self.registerDeferredLocalCallable(result, module_idx, decl.pattern, decl.expr);
                    try self.scanExpr(result, module_idx, decl.expr);
                } else {
                    try self.scanExpr(result, module_idx, decl.expr);
                }
            },
            .s_var => |var_decl| {
                const expr = module_env.store.getExpr(var_decl.expr);
                if (callableKind(expr)) |kind| {
                    _ = try self.registerProcTemplate(
                        result,
                        packLocalPatternSourceKey(module_idx, var_decl.pattern_idx),
                        module_idx,
                        var_decl.expr,
                        ModuleEnv.varFrom(var_decl.pattern_idx),
                        kind,
                        module_env.store.getExprRegion(var_decl.expr),
                    );
                    try self.registerDeferredLocalCallable(result, module_idx, var_decl.pattern_idx, var_decl.expr);
                    try self.scanExpr(result, module_idx, var_decl.expr);
                } else {
                    try self.scanExpr(result, module_idx, var_decl.expr);
                }
            },
            .s_reassign => |reassign| try self.scanExpr(result, module_idx, reassign.expr),
            .s_dbg => |dbg_stmt| try self.scanExpr(result, module_idx, dbg_stmt.expr),
            .s_expr => |expr_stmt| try self.scanExpr(result, module_idx, expr_stmt.expr),
            .s_expect => |expect_stmt| try self.scanExpr(result, module_idx, expect_stmt.body),
            .s_for => |for_stmt| {
                try self.scanExpr(result, module_idx, for_stmt.expr);
                try self.scanExpr(result, module_idx, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.scanExpr(result, module_idx, while_stmt.cond);
                try self.scanExpr(result, module_idx, while_stmt.body);
            },
            .s_return => |return_stmt| try self.scanExpr(result, module_idx, return_stmt.expr),
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

    fn scanExpr(self: *Pass, result: *Result, module_idx: u32, expr_idx: CIR.Expr.Idx) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const expr = module_env.store.getExpr(expr_idx);

        if (callableKind(expr)) |kind| {
            _ = try self.registerProcTemplate(
                result,
                packExprSourceKey(module_idx, expr_idx),
                module_idx,
                expr_idx,
                ModuleEnv.varFrom(expr_idx),
                kind,
                module_env.store.getExprRegion(expr_idx),
            );
        }

        if (self.active_bindings != null) {
            try self.scanExprChildren(result, module_idx, expr_idx, expr);
            return;
        }

        const visit_key = exprVisitKey(module_idx, expr_idx);
        if (self.visited_exprs.contains(visit_key)) return;
        try self.visited_exprs.put(self.allocator, visit_key, {});

        try self.scanExprChildren(result, module_idx, expr_idx, expr);
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
            .e_lookup_required,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            => {},
            .e_lookup_local => |lookup| {
                if (result.getLocalProcTemplate(module_idx, lookup.pattern_idx)) |template_id| {
                    try self.resolveLookupExprProcInst(result, module_idx, expr_idx, template_id);
                }
            },
            .e_lookup_external => |lookup| {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse return;
                const target_env = self.all_module_envs[target_module_idx];
                if (!target_env.store.isDefNode(lookup.target_node_idx)) return;

                const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const def = target_env.store.getDef(def_idx);
                const target_expr = target_env.store.getExpr(def.expr);
                if (callableKind(target_expr)) |kind| {
                    _ = try self.registerProcTemplate(
                        result,
                        packExternalDefSourceKey(target_module_idx, lookup.target_node_idx),
                        target_module_idx,
                        def.expr,
                        ModuleEnv.varFrom(def.pattern),
                        kind,
                        target_env.store.getExprRegion(def.expr),
                    );
                }
                if (result.getExternalProcTemplate(target_module_idx, lookup.target_node_idx)) |template_id| {
                    try self.resolveLookupExprProcInst(result, module_idx, expr_idx, template_id);
                }
                try self.scanModule(result, target_module_idx);
            },
            .e_str => |str_expr| try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(str_expr.span)),
            .e_list => |list_expr| try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(list_expr.elems)),
            .e_tuple => |tuple_expr| try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(tuple_expr.elems)),
            .e_match => |match_expr| {
                try self.scanExpr(result, module_idx, match_expr.cond);

                const branches = module_env.store.sliceMatchBranches(match_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    try self.scanExpr(result, module_idx, branch.value);
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
                    try self.scanExpr(result, module_idx, branch.body);
                }
                try self.scanExpr(result, module_idx, if_expr.final_else);
            },
            .e_call => |call_expr| {
                try self.scanExpr(result, module_idx, call_expr.func);
                try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(call_expr.args));
                try self.resolveDirectCallSite(result, module_idx, expr_idx, call_expr.func);
            },
            .e_record => |record_expr| {
                if (record_expr.ext) |ext_expr| {
                    try self.scanExpr(result, module_idx, ext_expr);
                }

                const fields = module_env.store.sliceRecordFields(record_expr.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.scanExpr(result, module_idx, field.value);
                }
            },
            .e_block => |block_expr| {
                const stmts = module_env.store.sliceStatements(block_expr.stmts);
                for (stmts) |stmt_idx| {
                    try self.scanStmt(result, module_idx, stmt_idx);
                }
                try self.scanExpr(result, module_idx, block_expr.final_expr);
            },
            .e_tag => |tag_expr| try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(tag_expr.args)),
            .e_nominal => |nominal_expr| try self.scanExpr(result, module_idx, nominal_expr.backing_expr),
            .e_nominal_external => |nominal_expr| try self.scanExpr(result, module_idx, nominal_expr.backing_expr),
            .e_closure => |closure_expr| try self.scanExpr(result, module_idx, closure_expr.lambda_idx),
            .e_lambda => {},
            .e_binop => |binop_expr| {
                try self.scanExpr(result, module_idx, binop_expr.lhs);
                try self.scanExpr(result, module_idx, binop_expr.rhs);
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
            },
            .e_unary_minus => |unary_expr| {
                try self.scanExpr(result, module_idx, unary_expr.expr);
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
            },
            .e_unary_not => |unary_expr| try self.scanExpr(result, module_idx, unary_expr.expr),
            .e_dot_access => |dot_expr| {
                try self.scanExpr(result, module_idx, dot_expr.receiver);
                if (dot_expr.args) |args| {
                    try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(args));
                    try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
                }
            },
            .e_tuple_access => |tuple_access| try self.scanExpr(result, module_idx, tuple_access.tuple),
            .e_dbg => |dbg_expr| try self.scanExpr(result, module_idx, dbg_expr.expr),
            .e_expect => |expect_expr| try self.scanExpr(result, module_idx, expect_expr.body),
            .e_return => |return_expr| try self.scanExpr(result, module_idx, return_expr.expr),
            .e_type_var_dispatch => |dispatch_expr| {
                try self.scanExprSpan(result, module_idx, module_env.store.sliceExpr(dispatch_expr.args));
                try self.resolveDispatchExprProcInst(result, module_idx, expr_idx, expr);
            },
            .e_for => |for_expr| {
                try self.scanExpr(result, module_idx, for_expr.expr);
                try self.scanExpr(result, module_idx, for_expr.body);
            },
            .e_hosted_lambda => {},
            .e_run_low_level => |run_low_level| {
                const args = module_env.store.sliceExpr(run_low_level.args);
                try self.scanExprSpan(result, module_idx, args);
                if (run_low_level.op == .str_inspekt and args.len != 0) {
                    try self.resolveStrInspektHelperProcInstsForTypeVar(
                        result,
                        module_idx,
                        ModuleEnv.varFrom(args[0]),
                    );
                }
            },
        }
    }

    fn scanExprSpan(self: *Pass, result: *Result, module_idx: u32, exprs: []const CIR.Expr.Idx) Allocator.Error!void {
        for (exprs) |child_expr| {
            try self.scanExpr(result, module_idx, child_expr);
        }
    }

    fn resolveDirectCallSite(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        call_expr_idx: CIR.Expr.Idx,
        callee_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];
        const callee_expr = module_env.store.getExpr(callee_expr_idx);
        const template_id = try self.lookupDirectCalleeTemplate(result, module_idx, callee_expr_idx) orelse {
            switch (callee_expr) {
                .e_lookup_local, .e_lookup_external, .e_lambda, .e_closure, .e_hosted_lambda => {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Monomorphize: reachable direct call expr {d} in module {d} has direct-callable callee expr {d} ({s}) without a proc template",
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
        const fn_monotype = try self.resolveExprMonotype(result, module_idx, callee_expr_idx);
        if (fn_monotype.isNone()) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: reachable direct call expr {d} in module {d} has callee expr {d} with no concrete fn monotype",
                    .{
                        @intFromEnum(call_expr_idx),
                        module_idx,
                        @intFromEnum(callee_expr_idx),
                    },
                );
            }
            unreachable;
        }

        const proc_inst_id = try self.ensureProcInst(result, template_id, fn_monotype, module_idx);
        try result.call_site_proc_insts.put(
            self.allocator,
            Result.callSiteKey(module_idx, call_expr_idx),
            proc_inst_id,
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
        const resolved_target = switch (expr) {
            .e_binop => |binop_expr| blk: {
                const method_name = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse return;
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, method_name);
            },
            .e_unary_minus => blk: {
                break :blk try self.resolveDispatchTargetForExpr(result, module_idx, expr_idx, module_env.idents.negate);
            },
            .e_dot_access => |dot_expr| blk: {
                if (dot_expr.args == null) return;
                if (dotCallUsesRuntimeReceiver(module_env, dot_expr.receiver)) {
                    const receiver_monotype = try self.resolveExprMonotype(result, module_idx, dot_expr.receiver);
                    break :blk try self.resolveDispatchTargetForDotCall(
                        result,
                        module_idx,
                        expr_idx,
                        dot_expr.field_name,
                        receiver_monotype,
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
                    "Monomorphize: reachable dispatch expr {d} in module {d} resolved to method '{s}' without a proc template",
                    .{ @intFromEnum(expr_idx), module_idx, method_name },
                );
            }
            unreachable;
        };
        const fn_monotype = try self.resolveTypeVarMonotype(result, module_idx, resolved_target.fn_var);
        if (fn_monotype.isNone()) {
            if (std.debug.runtime_safety) {
                const method_name = self.dispatchTargetMethodText(module_env, resolved_target) orelse "<unreadable>";
                std.debug.panic(
                    "Monomorphize: reachable dispatch expr {d} in module {d} resolved to method '{s}' with no concrete fn monotype",
                    .{ @intFromEnum(expr_idx), module_idx, method_name },
                );
            }
            unreachable;
        }

        const proc_inst_id = try self.ensureProcInst(result, template_id, fn_monotype, module_idx);
        try result.dispatch_expr_proc_insts.put(
            self.allocator,
            Result.callSiteKey(module_idx, expr_idx),
            proc_inst_id,
        );
    }

    fn lookupResolvedDispatchTarget(self: *Pass, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ResolvedDispatchTarget {
        return self.resolved_dispatch_targets.get(Result.callSiteKey(module_idx, expr_idx));
    }

    fn lookupDispatchConstraintForExpr(
        self: *Pass,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        method_name: Ident.Idx,
    ) ?types.StaticDispatchConstraint {
        const module_env = self.all_module_envs[module_idx];

        var found: ?types.StaticDispatchConstraint = null;
        for (module_env.types.sliceAllStaticDispatchConstraints()) |constraint| {
            if (constraint.source_expr_idx != @intFromEnum(expr_idx)) continue;
            if (!constraint.fn_name.eql(method_name)) continue;

            if (found) |existing| {
                const existing_resolved = !existing.resolved_target.isNone();
                const candidate_resolved = !constraint.resolved_target.isNone();

                if (existing_resolved and candidate_resolved) {
                    if (std.debug.runtime_safety and
                        (!existing.resolved_target.origin_module.eql(constraint.resolved_target.origin_module) or
                            !existing.resolved_target.method_ident.eql(constraint.resolved_target.method_ident)))
                    {
                        std.debug.panic(
                            "Monomorphize: conflicting resolved dispatch targets for expr={d} method={d}",
                            .{ @intFromEnum(expr_idx), method_name.idx },
                        );
                    }
                } else if (!existing_resolved and candidate_resolved) {
                    found = constraint;
                }
                continue;
            }

            found = constraint;
        }

        return found;
    }

    fn resolvedTargetIsUsable(
        self: *Pass,
        source_env: *const ModuleEnv,
        method_name: Ident.Idx,
        resolved_target: types.StaticDispatchConstraint.ResolvedTarget,
    ) bool {
        const method_name_text = source_env.getIdent(method_name);
        const target_method_text = identTextIfOwnedBy(source_env, resolved_target.method_ident) orelse return false;
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
        const constraint = self.lookupDispatchConstraintForExpr(module_idx, expr_idx, method_name) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: no static dispatch constraint for expr={d} method='{s}'",
                    .{ @intFromEnum(expr_idx), module_env.getIdent(method_name) },
                );
            }
            unreachable;
        };

        const desired_func_monotype = try self.resolveTypeVarMonotype(result, module_idx, constraint.fn_var);

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
                if (try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate, desired_func_monotype)) {
                    break :blk candidate;
                }
            }

            break :blk try self.resolveUnresolvedTypeVarDispatchTarget(result, module_idx, method_name, constraint);
        };

        try self.resolved_dispatch_targets.put(self.allocator, Result.callSiteKey(module_idx, expr_idx), resolved);
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

            const fn_mono = try self.resolveTypeVarMonotype(result, module_idx, constraint.fn_var);
            if (!try self.resolvedDispatchTargetMatchesMonotype(result, module_idx, candidate, fn_mono)) continue;

            if (first_candidate == null) first_candidate = candidate;

            if (fn_mono.isNone()) {
                if (!constraint.resolved_target.isNone()) {
                    if (resolved_match == null) resolved_match = candidate;
                } else if (unresolved_match == null) {
                    unresolved_match = candidate;
                }
                continue;
            }

            const mono = result.monotype_store.getMonotype(fn_mono);
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

        try self.resolved_dispatch_targets.put(self.allocator, Result.callSiteKey(module_idx, expr_idx), resolved);
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

    fn resolveUnresolvedTypeVarDispatchTarget(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        method_name: Ident.Idx,
        constraint: types.StaticDispatchConstraint,
    ) Allocator.Error!ResolvedDispatchTarget {
        const module_env = self.all_module_envs[module_idx];
        const desired_func_monotype = try self.resolveTypeVarMonotype(result, module_idx, constraint.fn_var);
        if (desired_func_monotype.isNone()) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monomorphize: unresolved fn_var monotype for method '{s}'",
                    .{module_env.getIdent(method_name)},
                );
            }
            unreachable;
        }

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
                const candidate_mono = try self.resolveExprMonotype(result, candidate_module_idx, def.expr);
                if (candidate_mono.isNone()) continue;
                if (!try self.monotypesStructurallyEqualAcrossModules(
                    result,
                    candidate_mono,
                    candidate_module_idx,
                    desired_func_monotype,
                    module_idx,
                )) continue;

                const candidate_origin_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
                const mapped_origin = module_env.common.findIdent(candidate_origin_name) orelse module_env.qualified_module_ident;

                const candidate_target = ResolvedDispatchTarget{
                    .origin = mapped_origin,
                    .method_ident = method_ident,
                    .fn_var = constraint.fn_var,
                    .module_idx = candidate_module_idx,
                };
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
                            "Monomorphize: ambiguous dispatch for method '{s}'",
                            .{method_name_text},
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
                    "Monomorphize: no candidate for method '{s}'",
                    .{method_name_text},
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
        const fn_monotype = try self.resolveExprMonotype(result, module_idx, expr_idx);
        if (fn_monotype.isNone()) return;

        const proc_inst_id = try self.ensureProcInst(result, template_id, fn_monotype, module_idx);
        try result.lookup_expr_proc_insts.put(
            self.allocator,
            Result.callSiteKey(module_idx, expr_idx),
            proc_inst_id,
        );
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
        const target_expr = target_env.store.getExpr(def.expr);
        const kind = callableKind(target_expr) orelse return null;
        return try self.registerProcTemplate(
            result,
            packExternalDefSourceKey(target_module_idx, target_node_idx),
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            kind,
            target_env.store.getExprRegion(def.expr),
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

        const origin_name = identTextIfOwnedBy(source_env, origin_module) orelse return null;
        for (self.all_module_envs, 0..) |candidate_env, idx| {
            const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
            if (std.mem.eql(u8, origin_name, candidate_name)) return @intCast(idx);
        }
        return null;
    }

    fn identTextIfOwnedBy(env: *const ModuleEnv, ident: Ident.Idx) ?[]const u8 {
        const ident_store = env.getIdentStoreConst();
        const bytes = ident_store.interner.bytes.items.items;
        const start: usize = @intCast(ident.idx);
        if (start >= bytes.len) return null;

        const tail = bytes[start..];
        const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return null;
        const text = tail[0..end_rel];

        const roundtrip = ident_store.findByString(text) orelse return null;
        if (!roundtrip.eql(ident)) return null;
        return text;
    }

    fn dispatchTargetMethodText(
        self: *Pass,
        source_env: *const ModuleEnv,
        target: ResolvedDispatchTarget,
    ) ?[]const u8 {
        if (identTextIfOwnedBy(source_env, target.method_ident)) |text| return text;

        if (target.module_idx) |target_module_idx| {
            const target_env = self.all_module_envs[target_module_idx];
            if (identTextIfOwnedBy(target_env, target.method_ident)) |text| return text;
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
    ) Allocator.Error!?struct {
        target_env: *const ModuleEnv,
        module_idx: u32,
        template_id: ProcTemplateId,
        type_var: types.Var,
    } {
        const source_env = self.all_module_envs[source_module_idx];
        const target_module_idx = self.findModuleForOrigin(source_env, nominal.origin_module);
        const target_env = self.all_module_envs[target_module_idx];
        const qualified_method_ident = target_env.lookupMethodIdentFromEnvConst(
            source_env,
            nominal.ident.ident_idx,
            method_ident,
        ) orelse return null;
        const node_idx = target_env.getExposedNodeIndexById(qualified_method_ident) orelse return null;
        if (!target_env.store.isDefNode(node_idx)) return null;

        const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
        const def = target_env.store.getDef(def_idx);
        const target_expr = target_env.store.getExpr(def.expr);
        const kind = callableKind(target_expr) orelse return null;
        const template_id = try self.registerProcTemplate(
            result,
            packExternalDefSourceKey(target_module_idx, node_idx),
            target_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            kind,
            target_env.store.getExprRegion(def.expr),
        );
        return .{
            .target_env = target_env,
            .module_idx = target_module_idx,
            .template_id = template_id,
            .type_var = ModuleEnv.varFrom(def.expr),
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
        const target_expr = builtin_env.store.getExpr(def.expr);
        const kind = callableKind(target_expr) orelse return;
        const template_id = try self.registerProcTemplate(
            result,
            packExternalDefSourceKey(builtin_module_idx, node_idx),
            builtin_module_idx,
            def.expr,
            ModuleEnv.varFrom(def.pattern),
            kind,
            builtin_env.store.getExprRegion(def.expr),
        );

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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    ) Allocator.Error!Monotype.Idx {
        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        const module_env = self.all_module_envs[module_idx];
        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.all_module_envs = self.all_module_envs;

        return result.monotype_store.fromTypeVar(
            self.allocator,
            store_types,
            var_,
            module_env.idents,
            bindings,
            &nominal_cycle_breakers,
            &scratches,
        );
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

    fn resolveStrInspektHelperProcInstsForTypeVar(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        type_var: types.Var,
    ) Allocator.Error!void {
        const module_env = self.all_module_envs[module_idx];

        var resolved = module_env.types.resolveVar(type_var);
        while (resolved.desc.content == .alias) {
            resolved = module_env.types.resolveVar(module_env.types.getAliasBackingVar(resolved.desc.content.alias));
        }

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
                                try self.resolveStrInspektHelperProcInstsForTypeVar(result, module_idx, type_args[0]);
                            }
                            return;
                        }
                        if (ident.eql(common.box)) {
                            const type_args = module_env.types.sliceNominalArgs(nominal);
                            const outer_mono = try self.resolveTypeVarMonotype(result, module_idx, resolved.var_);
                            const outer_box = result.monotype_store.getMonotype(outer_mono).box;
                            try self.ensureBuiltinBoxUnboxProcInst(result, module_idx, outer_mono, outer_box.inner);
                            if (type_args.len == 1) {
                                try self.resolveStrInspektHelperProcInstsForTypeVar(result, module_idx, type_args[0]);
                            }
                            return;
                        }
                    }

                    if (try self.lookupAssociatedMethodTemplate(result, module_idx, nominal, module_env.idents.to_inspect)) |method_info| {
                        if (resolveFuncTypeInStore(&method_info.target_env.types, method_info.type_var)) |resolved_func| {
                            if (!resolved_func.effectful) {
                                const param_vars = method_info.target_env.types.sliceVars(resolved_func.func.args);
                                if (param_vars.len == 1) {
                                    var bindings = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
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
                                            try self.resolveStrInspektHelperProcInstsForMonotype(
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

                    try self.resolveStrInspektHelperProcInstsForMonotype(
                        result,
                        module_idx,
                        try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
                    );
                    return;
                },
                else => {},
            }
        }

        try self.resolveStrInspektHelperProcInstsForMonotype(
            result,
            module_idx,
            try self.resolveTypeVarMonotype(result, module_idx, resolved.var_),
        );
    }

    fn resolveStrInspektHelperProcInstsForMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        monotype: Monotype.Idx,
    ) Allocator.Error!void {
        switch (result.monotype_store.getMonotype(monotype)) {
            .unit, .prim => {},
            .list => |list_mono| try self.resolveStrInspektHelperProcInstsForMonotype(result, module_idx, list_mono.elem),
            .box => |box_mono| {
                try self.ensureBuiltinBoxUnboxProcInst(result, module_idx, monotype, box_mono.inner);
                try self.resolveStrInspektHelperProcInstsForMonotype(result, module_idx, box_mono.inner);
            },
            .tuple => |tuple_mono| {
                for (result.monotype_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                    try self.resolveStrInspektHelperProcInstsForMonotype(result, module_idx, elem_mono);
                }
            },
            .func => {},
            .record => |record_mono| {
                for (result.monotype_store.getFields(record_mono.fields)) |field| {
                    try self.resolveStrInspektHelperProcInstsForMonotype(result, module_idx, field.type_idx);
                }
            },
            .tag_union => |tag_union_mono| {
                for (result.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                    for (result.monotype_store.getIdxSpan(tag.payloads)) |payload_mono| {
                        try self.resolveStrInspektHelperProcInstsForMonotype(result, module_idx, payload_mono);
                    }
                }
            },
            .recursive_placeholder => unreachable,
        }
    }

    fn lookupDirectCalleeTemplate(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        callee_expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!?ProcTemplateId {
        const module_env = self.all_module_envs[module_idx];
        return switch (module_env.store.getExpr(callee_expr_idx)) {
            .e_lookup_local => |lookup| result.getLocalProcTemplate(module_idx, lookup.pattern_idx),
            .e_lookup_external => |lookup| blk: {
                const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
                break :blk result.getExternalProcTemplate(target_module_idx, lookup.target_node_idx);
            },
            .e_lambda, .e_closure, .e_hosted_lambda => result.getExprProcTemplate(module_idx, callee_expr_idx),
            else => null,
        };
    }

    fn ensureProcInst(
        self: *Pass,
        result: *Result,
        template_id: ProcTemplateId,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!ProcInstId {
        const template = result.getProcTemplate(template_id);
        const subst_id = if (self.all_module_envs[template.module_idx].types.needsInstantiation(template.type_root))
            try self.ensureTypeSubst(result, template.*, fn_monotype, fn_monotype_module_idx)
        else
            TypeSubstId.none;

        for (result.proc_insts.items, 0..) |existing_proc_inst, idx| {
            if (existing_proc_inst.template != template_id) continue;
            if (existing_proc_inst.fn_monotype_module_idx != fn_monotype_module_idx) continue;
            if (try self.monotypesStructurallyEqual(result, existing_proc_inst.fn_monotype, fn_monotype)) {
                if (existing_proc_inst.subst != subst_id) continue;
                const existing_id: ProcInstId = @enumFromInt(idx);
                try self.scanProcInst(result, existing_id);
                return existing_id;
            }
        }

        const proc_inst_id: ProcInstId = @enumFromInt(result.proc_insts.items.len);
        try result.proc_insts.append(self.allocator, .{
            .template = template_id,
            .subst = subst_id,
            .fn_monotype = fn_monotype,
            .fn_monotype_module_idx = fn_monotype_module_idx,
        });
        try self.scanProcInst(result, proc_inst_id);
        return proc_inst_id;
    }

    fn scanProcInst(self: *Pass, result: *Result, proc_inst_id: ProcInstId) Allocator.Error!void {
        const proc_inst_key = @intFromEnum(proc_inst_id);
        if (self.scanned_proc_insts.contains(proc_inst_key)) return;
        const proc_inst = result.getProcInst(proc_inst_id);
        const template = result.getProcTemplate(proc_inst.template);
        try self.scanModule(result, template.module_idx);
        try self.scanned_proc_insts.put(self.allocator, proc_inst_key, {});

        const module_env = self.all_module_envs[template.module_idx];

        var bindings = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer bindings.deinit();

        if (!proc_inst.subst.isNone()) {
            const subst = result.getTypeSubst(proc_inst.subst);
            for (result.getTypeSubstEntries(subst.entries)) |entry| {
                try bindings.put(entry.type_var, entry.monotype);
            }
        }

        const saved_bindings = self.active_bindings;
        self.active_bindings = &bindings;
        defer self.active_bindings = saved_bindings;

        switch (module_env.store.getExpr(template.cir_expr)) {
            .e_lambda => |lambda_expr| try self.scanExpr(result, template.module_idx, lambda_expr.body),
            .e_closure => |closure_expr| {
                const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    try self.scanExpr(result, template.module_idx, lambda_expr.e_lambda.body);
                }
            },
            .e_hosted_lambda => |hosted_expr| try self.scanExpr(result, template.module_idx, hosted_expr.body),
            else => unreachable,
        }
    }

    fn ensureTypeSubst(
        self: *Pass,
        result: *Result,
        template: ProcTemplate,
        fn_monotype: Monotype.Idx,
        fn_monotype_module_idx: u32,
    ) Allocator.Error!TypeSubstId {
        var bindings = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
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
        try result.substs.append(self.allocator, .{ .entries = entries_span });
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
            if (lhs_entry.type_var != rhs_entry.type_var) return false;
            if (!try self.monotypesStructurallyEqual(result, lhs_entry.monotype, rhs_entry.monotype)) {
                return false;
            }
        }

        return true;
    }

    fn identsStructurallyEqualAcrossModules(
        self: *Pass,
        lhs_module_idx: u32,
        lhs: base.Ident.Idx,
        rhs_module_idx: u32,
        rhs: base.Ident.Idx,
    ) bool {
        if (lhs_module_idx == rhs_module_idx and lhs == rhs) return true;

        const lhs_text = self.all_module_envs[lhs_module_idx].getIdent(lhs);
        const rhs_text = self.all_module_envs[rhs_module_idx].getIdent(rhs);
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
                "Monomorphize: record field '{s}' missing from monotype",
                .{self.all_module_envs[template_module_idx].getIdent(field_name)},
            );
        }
        unreachable;
    }

    fn tagIndexByName(
        self: *Pass,
        template_module_idx: u32,
        tag_name: base.Ident.Idx,
        mono_module_idx: u32,
        mono_tags: []const Monotype.Tag,
    ) u32 {
        for (mono_tags, 0..) |mono_tag, tag_idx| {
            if (self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) {
                return @intCast(tag_idx);
            }
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Monomorphize: tag '{s}' missing from monotype",
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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        tag_name: base.Ident.Idx,
        payload_vars: []const types.Var,
        mono_tags: []const Monotype.Tag,
        mono_module_idx: u32,
    ) Allocator.Error!void {
        for (mono_tags) |mono_tag| {
            if (!self.identsStructurallyEqualAcrossModules(
                template_module_idx,
                tag_name,
                mono_module_idx,
                mono_tag.name,
            )) continue;

            const mono_payloads = result.monotype_store.getIdxSpan(mono_tag.payloads);
            if (payload_vars.len != mono_payloads.len) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monomorphize: payload arity mismatch for tag '{s}'",
                        .{self.all_module_envs[template_module_idx].getIdent(tag_name)},
                    );
                }
                unreachable;
            }

            for (payload_vars, 0..) |payload_var, i| {
                try self.bindTypeVarMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    payload_var,
                    mono_payloads[i],
                    mono_module_idx,
                );
            }
            return;
        }

        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Monomorphize: tag '{s}' missing from monotype",
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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        type_var: types.Var,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        const resolved = template_types.resolveVar(type_var);
        if (bindings.get(resolved.var_)) |existing| {
            if (!try self.monotypesStructurallyEqual(result, existing, monotype)) {
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
            .flex, .rigid => {
                try bindings.put(resolved.var_, monotype);
                try ordered_entries.append(self.allocator, .{
                    .type_var = resolved.var_,
                    .monotype = monotype,
                });
            },
            .alias => |alias| try self.bindTypeVarMonotypes(
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                template_types.getAliasBackingVar(alias),
                monotype,
                mono_module_idx,
            ),
            .structure => |flat_type| {
                try bindings.put(resolved.var_, monotype);
                try ordered_entries.append(self.allocator, .{
                    .type_var = resolved.var_,
                    .monotype = monotype,
                });
                try self.bindFlatTypeMonotypes(
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    flat_type,
                    monotype,
                    mono_module_idx,
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
        bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
        ordered_entries: *std.ArrayList(TypeSubstEntry),
        flat_type: types.FlatType,
        monotype: Monotype.Idx,
        mono_module_idx: u32,
    ) Allocator.Error!void {
        if (monotype.isNone()) return;

        const mono = result.monotype_store.getMonotype(monotype);
        const common_idents = ModuleEnv.CommonIdents.find(&self.all_module_envs[template_module_idx].common);

        switch (flat_type) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| {
                const mfunc = switch (mono) {
                    .func => |mfunc| mfunc,
                    else => unreachable,
                };

                const type_args = template_types.sliceVars(func.args);
                const mono_args = result.monotype_store.getIdxSpan(mfunc.args);
                if (type_args.len != mono_args.len) unreachable;

                for (type_args, 0..) |arg_var, i| {
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        arg_var,
                        mono_args[i],
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
                        else => unreachable,
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
                        else => unreachable,
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
                        else => unreachable,
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
                    else => unreachable,
                };
                const mono_fields = result.monotype_store.getFields(mrec.fields);
                var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
                defer seen_field_indices.deinit(self.allocator);

                var current_row = record;
                rows: while (true) {
                    const fields_slice = template_types.getRecordFieldsSlice(current_row.fields);
                    const field_names = fields_slice.items(.name);
                    const field_vars = fields_slice.items(.var_);
                    for (field_names, field_vars) |field_name, field_var| {
                        const field_idx = self.recordFieldIndexByName(
                            template_module_idx,
                            field_name,
                            mono_module_idx,
                            mono_fields,
                        );
                        try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                        try self.bindTypeVarMonotypes(
                            result,
                            template_module_idx,
                            template_types,
                            bindings,
                            ordered_entries,
                            field_var,
                            mono_fields[field_idx].type_idx,
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
                                        const field_idx = self.recordFieldIndexByName(
                                            template_module_idx,
                                            field_name,
                                            mono_module_idx,
                                            mono_fields,
                                        );
                                        try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                                        try self.bindTypeVarMonotypes(
                                            result,
                                            template_module_idx,
                                            template_types,
                                            bindings,
                                            ordered_entries,
                                            field_var,
                                            mono_fields[field_idx].type_idx,
                                            mono_module_idx,
                                        );
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => unreachable,
                            },
                            .flex, .rigid => {
                                try self.bindRecordRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    mono_fields,
                                    seen_field_indices.items,
                                    mono_module_idx,
                                );
                                break :rows;
                            },
                            .err => unreachable,
                        }
                    }
                }
            },
            .record_unbound => |fields_range| {
                const mrec = switch (mono) {
                    .record => |mrec| mrec,
                    else => unreachable,
                };
                const mono_fields = result.monotype_store.getFields(mrec.fields);
                const fields_slice = template_types.getRecordFieldsSlice(fields_range);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);
                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByName(
                        template_module_idx,
                        field_name,
                        mono_module_idx,
                        mono_fields,
                    );
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        field_var,
                        mono_fields[field_idx].type_idx,
                        mono_module_idx,
                    );
                }
            },
            .tuple => |tuple| {
                const mtup = switch (mono) {
                    .tuple => |mtup| mtup,
                    else => unreachable,
                };
                const elem_vars = template_types.sliceVars(tuple.elems);
                const elem_monos = result.monotype_store.getIdxSpan(mtup.elems);
                if (elem_vars.len != elem_monos.len) unreachable;
                for (elem_vars, 0..) |elem_var, i| {
                    try self.bindTypeVarMonotypes(
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        elem_var,
                        elem_monos[i],
                        mono_module_idx,
                    );
                }
            },
            .tag_union => |tag_union| {
                const mtag = switch (mono) {
                    .tag_union => |mtag| mtag,
                    else => unreachable,
                };
                const mono_tags = result.monotype_store.getTags(mtag.tags);
                var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
                defer seen_tag_indices.deinit(self.allocator);

                var current_row = tag_union;
                rows: while (true) {
                    const type_tags = template_types.getTagsSlice(current_row.tags);
                    const tag_names = type_tags.items(.name);
                    const tag_args = type_tags.items(.args);
                    for (tag_names, tag_args) |tag_name, args_range| {
                        const tag_idx = self.tagIndexByName(
                            template_module_idx,
                            tag_name,
                            mono_module_idx,
                            mono_tags,
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
                            mono_tags,
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
                                else => unreachable,
                            },
                            .flex, .rigid => {
                                try self.bindTagUnionRowTail(
                                    result,
                                    template_module_idx,
                                    template_types,
                                    bindings,
                                    ordered_entries,
                                    ext_var,
                                    mono_tags,
                                    seen_tag_indices.items,
                                    mono_module_idx,
                                );
                                break :rows;
                            },
                            .err => unreachable,
                        }
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

    fn resolveExprMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) Allocator.Error!Monotype.Idx {
        return self.resolveTypeVarMonotype(result, module_idx, ModuleEnv.varFrom(expr_idx));
    }

    fn resolveTypeVarMonotype(
        self: *Pass,
        result: *Result,
        module_idx: u32,
        var_: types.Var,
    ) Allocator.Error!Monotype.Idx {
        if (self.active_bindings) |bindings| {
            return self.resolveTypeVarMonotypeWithBindings(
                result,
                module_idx,
                &self.all_module_envs[module_idx].types,
                var_,
                @constCast(bindings),
            );
        }

        const module_env = self.all_module_envs[module_idx];

        var specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer specializations.deinit();

        var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
        defer nominal_cycle_breakers.deinit();

        var scratches = try Monotype.Store.Scratches.init(self.allocator);
        defer scratches.deinit();

        scratches.ident_store = module_env.getIdentStoreConst();
        scratches.module_env = module_env;
        scratches.all_module_envs = self.all_module_envs;

        return result.monotype_store.fromTypeVar(
            self.allocator,
            &module_env.types,
            var_,
            module_env.idents,
            &specializations,
            &nominal_cycle_breakers,
            &scratches,
        );
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
                    if (lhs_field.name != rhs_field.name) break :blk false;
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
                    if (lhs_tag.name != rhs_tag.name) break :blk false;
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

/// Monomorphize all reachable callables in the current module.
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
