//! Shared statement-only lowering pipeline used by eval backends.
//!
//! This module centralizes the CIR -> MIR -> LIR -> RC path for eval consumers.
//! It owns a shared layout store and exposes proc-root lowering results.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const can = @import("can");
const layout = @import("layout");
const lir = @import("lir");
const mir = @import("mir");
const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

const LirStore = lir.LirStore;
const LirProcSpecId = lir.LirProcSpecId;
const MIR = mir.MIR;

/// Comptime-gated tracing for the shared lowering pipeline.
const trace = struct {
    const enabled = if (@hasDecl(build_options, "trace_eval")) build_options.trace_eval else false;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (comptime enabled) {
            std.debug.print("[lower] " ++ fmt ++ "\n", args);
        }
    }
};

/// Find the index of a module environment in the all-module-env slice.
pub fn findModuleEnvIdx(all_module_envs: []const *ModuleEnv, module_env: *ModuleEnv) ?u32 {
    for (all_module_envs, 0..) |env, i| {
        if (env == module_env) return @intCast(i);
    }
    return null;
}

/// Build a TypeScope for platform `requires` type variables.
/// Maps platform flex vars from `requires { model : Model }` to the app's concrete types.
pub fn buildPlatformTypeScope(
    allocator: Allocator,
    platform_env: *const ModuleEnv,
    app_env: *const ModuleEnv,
    platform_to_app_idents: *const std.AutoHashMap(base.Ident.Idx, base.Ident.Idx),
) !types.TypeScope {
    var type_scope = types.TypeScope.init(allocator);
    errdefer type_scope.deinit();

    try type_scope.scopes.append(types.VarMap.init(allocator));
    const rigid_scope = &type_scope.scopes.items[0];
    const all_aliases = platform_env.for_clause_aliases.items.items;

    for (platform_env.requires_types.items.items) |required_type| {
        const aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (aliases_slice) |alias| {
            const alias_stmt = platform_env.store.getStatement(alias.alias_stmt_idx);
            std.debug.assert(alias_stmt == .s_alias_decl);
            const alias_body_var = ModuleEnv.varFrom(alias_stmt.s_alias_decl.anno);
            const alias_stmt_var = ModuleEnv.varFrom(alias.alias_stmt_idx);
            const app_alias_name = platform_to_app_idents.get(alias.alias_name) orelse continue;
            const app_var = findTypeAliasBodyVar(app_env, app_alias_name) orelse continue;
            try rigid_scope.put(alias_body_var, app_var);
            try rigid_scope.put(alias_stmt_var, app_var);
        }
    }

    return type_scope;
}

fn findTypeAliasBodyVar(module: *const ModuleEnv, name: base.Ident.Idx) ?types.Var {
    const stmts_slice = module.store.sliceStatements(module.all_statements);
    for (stmts_slice) |stmt_idx| {
        const stmt = module.store.getStatement(stmt_idx);
        switch (stmt) {
            .s_alias_decl => |alias_decl| {
                const header = module.store.getTypeHeader(alias_decl.header);
                if (header.relative_name.eql(name)) {
                    return ModuleEnv.varFrom(alias_decl.anno);
                }
            },
            else => {},
        }
    }
    return null;
}

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

pub const LirProgram = struct {
    allocator: Allocator,
    global_layout_store: ?*layout.Store = null,
    global_type_layout_resolver: ?*layout.TypeLayoutResolver = null,
    target_usize: base.target.TargetUsize,

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
        ModuleEnvNotFound,
    };

    /// Result of lowering one CIR expression to post-RC statement-only LIR.
    pub const LowerResult = struct {
        lir_store: LirStore,
        root_proc_id: LirProcSpecId,
        result_layout: layout.Idx,
        layout_store: *layout.Store,
        tuple_len: usize,

        pub fn deinit(self: *LowerResult) void {
            self.lir_store.deinit();
        }
    };

    pub fn init(allocator: Allocator, target_usize: base.target.TargetUsize) LirProgram {
        return .{
            .allocator = allocator,
            .target_usize = target_usize,
        };
    }

    pub fn deinit(self: *LirProgram) void {
        if (self.global_type_layout_resolver) |resolver| {
            resolver.deinit();
            self.allocator.destroy(resolver);
        }
        if (self.global_layout_store) |ls| {
            ls.deinit();
            self.allocator.destroy(ls);
        }
    }

    pub fn ensureGlobalLayoutStore(self: *LirProgram, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        if (self.global_layout_store) |ls| return ls;

        var builtin_str: ?base.Ident.Idx = null;
        for (all_module_envs) |env| {
            if (isBuiltinModuleEnv(env)) {
                builtin_str = env.idents.builtin_str;
                break;
            }
        }

        const ls = self.allocator.create(layout.Store) catch return error.OutOfMemory;
        ls.* = layout.Store.init(all_module_envs, builtin_str, self.allocator, self.target_usize) catch {
            self.allocator.destroy(ls);
            return error.OutOfMemory;
        };

        self.global_layout_store = ls;
        return ls;
    }

    pub fn ensureGlobalTypeLayoutResolver(self: *LirProgram, all_module_envs: []const *ModuleEnv) Error!*layout.TypeLayoutResolver {
        if (self.global_type_layout_resolver) |resolver| return resolver;

        const layout_store = try self.ensureGlobalLayoutStore(all_module_envs);
        const resolver = self.allocator.create(layout.TypeLayoutResolver) catch return error.OutOfMemory;
        resolver.* = layout.TypeLayoutResolver.init(layout_store);
        self.global_type_layout_resolver = resolver;
        return resolver;
    }

    pub fn prepareLayoutStores(self: *LirProgram, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);
        return layout_store_ptr;
    }

    pub fn lowerExpr(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
    ) Error!LowerResult {
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }
        module_env.imports.resolveImports(module_env, all_module_envs);

        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const app_module_idx = if (app_module_env) |env|
            findModuleEnvIdx(all_module_envs, env) orelse return error.ModuleEnvNotFound
        else
            null;

        const layout_store_ptr = try self.prepareLayoutStores(all_module_envs);
        return self.lowerExprWithPreparedLayout(
            module_env,
            expr_idx,
            all_module_envs,
            module_idx,
            app_module_idx,
            layout_store_ptr,
            null,
        );
    }

    pub fn lowerEntrypointExpr(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        type_scope: ?*const types.TypeScope,
    ) Error!LowerResult {
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }
        module_env.imports.resolveImports(module_env, all_module_envs);

        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const app_module_idx = if (app_module_env) |env|
            findModuleEnvIdx(all_module_envs, env) orelse return error.ModuleEnvNotFound
        else
            null;

        const layout_store_ptr = try self.prepareLayoutStores(all_module_envs);
        return self.lowerEntrypointExprWithPreparedLayout(
            module_env,
            expr_idx,
            all_module_envs,
            module_idx,
            app_module_idx,
            layout_store_ptr,
            arg_layouts,
            ret_layout,
            type_scope,
        );
    }

    fn lowerExprWithPreparedLayout(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
        app_module_idx: ?u32,
        layout_store_ptr: *layout.Store,
        maybe_type_scope: ?*const types.TypeScope,
    ) Error!LowerResult {
        trace.log("lowerExpr: expr={any} module={d}", .{ expr_idx, module_idx });

        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        errdefer mir_store.deinit(self.allocator);

        var monomorphization = if (maybe_type_scope) |type_scope|
            mir.Monomorphize.runRootSourceExprWithTypeScope(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
                module_idx,
                type_scope,
                app_module_idx orelse return error.RuntimeError,
            ) catch return error.OutOfMemory
        else
            mir.Monomorphize.runRootSourceExpr(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
            ) catch return error.OutOfMemory;
        defer monomorphization.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &monomorphization,
            all_module_envs,
            &module_env.types,
            module_idx,
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        if (maybe_type_scope) |type_scope| {
            mir_lower.setTypeScope(module_idx, type_scope, app_module_idx orelse return error.RuntimeError) catch return error.OutOfMemory;
        }

        const root_const_id = mir_lower.lowerRootConst(expr_idx) catch return error.RuntimeError;

        var mir_analyses = mir.Analyses.init(
            self.allocator,
            &mir_store,
            all_module_envs,
            module_idx,
            &.{root_const_id},
        ) catch return error.OutOfMemory;
        defer mir_analyses.deinit();

        var lir_store = LirStore.init(self.allocator);
        errdefer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &mir_analyses);
        defer mir_to_lir.deinit();

        const root_proc_id = mir_to_lir.lower(root_const_id) catch return error.RuntimeError;
        try mir_to_lir.flush();

        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        try rc_pass.insertRcOpsForAllProcs();

        const cir_expr = module_env.store.getExpr(expr_idx);
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        return .{
            .lir_store = lir_store,
            .root_proc_id = root_proc_id,
            .result_layout = lir_store.getProcSpec(root_proc_id).ret_layout,
            .layout_store = layout_store_ptr,
            .tuple_len = tuple_len,
        };
    }

    fn lowerEntrypointExprWithPreparedLayout(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
        app_module_idx: ?u32,
        layout_store_ptr: *layout.Store,
        arg_layouts: []const layout.Idx,
        ret_layout: layout.Idx,
        type_scope: ?*const types.TypeScope,
    ) Error!LowerResult {
        trace.log("lowerEntrypointExpr: expr={any} module={d}", .{ expr_idx, module_idx });

        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        errdefer mir_store.deinit(self.allocator);

        var monomorphization = if (type_scope) |ts|
            mir.Monomorphize.runRootSourceExprWithTypeScope(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
                module_idx,
                ts,
                app_module_idx orelse return error.RuntimeError,
            ) catch return error.OutOfMemory
        else
            mir.Monomorphize.runRootSourceExpr(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
            ) catch return error.OutOfMemory;
        defer monomorphization.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &monomorphization,
            all_module_envs,
            &module_env.types,
            module_idx,
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        if (type_scope) |ts| {
            mir_lower.setTypeScope(module_idx, ts, app_module_idx orelse return error.RuntimeError) catch return error.OutOfMemory;
        }

        const root_const_id = mir_lower.lowerRootConst(expr_idx) catch return error.RuntimeError;

        var mir_analyses = mir.Analyses.init(
            self.allocator,
            &mir_store,
            all_module_envs,
            module_idx,
            &.{root_const_id},
        ) catch return error.OutOfMemory;
        defer mir_analyses.deinit();

        var lir_store = LirStore.init(self.allocator);
        errdefer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, &mir_store, &lir_store, layout_store_ptr, &mir_analyses);
        defer mir_to_lir.deinit();

        const root_proc_id = mir_to_lir.lowerEntrypointProc(root_const_id, arg_layouts, ret_layout) catch return error.RuntimeError;
        try mir_to_lir.flush();

        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        try rc_pass.insertRcOpsForAllProcs();

        return .{
            .lir_store = lir_store,
            .root_proc_id = root_proc_id,
            .result_layout = ret_layout,
            .layout_store = layout_store_ptr,
            .tuple_len = 1,
        };
    }
};
