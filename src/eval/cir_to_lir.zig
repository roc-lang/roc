//! Shared LIR Lowering Pipeline
//!
//! Centralizes the CIR → MIR → LIR → RC lowering pipeline used by
//! dev_evaluator, wasm_evaluator, and the LIR interpreter.
//!
//! Manages a global layout store (shared across evaluations) and provides
//! a single `lowerExpr` entry point that produces post-RC LIR ready for
//! consumption by code generators or the interpreter.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mir = @import("mir");
const MIR = mir.MIR;
const lir = @import("lir");
const LirExprStore = lir.LirExprStore;

const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// Extract the result layout from a LIR expression.
/// This is total for value-producing expressions and unit-valued RC/loop nodes.
pub fn lirExprResultLayout(store: *const LirExprStore, expr_id: lir.LirExprId) layout.Idx {
    const LirExpr = lir.LirExpr;
    const expr: LirExpr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |b| b.result_layout,
        .if_then_else => |ite| ite.result_layout,
        .match_expr => |w| w.result_layout,
        .dbg => |d| d.result_layout,
        .expect => |e| e.result_layout,
        .call => |c| c.ret_layout,
        .low_level => |ll| ll.ret_layout,
        .early_return => |er| er.ret_layout,
        .lookup => |l| l.layout_idx,
        .cell_load => |l| l.layout_idx,
        .struct_ => |s| s.struct_layout,
        .tag => |t| t.union_layout,
        .zero_arg_tag => |z| z.union_layout,
        .struct_access => |sa| sa.field_layout,
        .nominal => |n| n.nominal_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .bool_literal => .bool,
        .dec_literal => .dec,
        .str_literal => .str,
        .i64_literal => |i| i.layout_idx,
        .i128_literal => |i| i.layout_idx,
        .list => |l| l.list_layout,
        .empty_list => |l| l.list_layout,
        .hosted_call => |hc| hc.ret_layout,
        .tag_payload_access => |tpa| tpa.payload_layout,
        .lambda => |l| l.fn_layout,
        .for_loop, .while_loop, .incref, .decref, .free => .zst,
        .crash => |c| c.ret_layout,
        .runtime_error => |re| re.ret_layout,
        .break_expr => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "LIR/eval invariant violated: lirExprResultLayout called on break_expr",
                    .{},
                );
            }
            unreachable;
        },
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        => .str,
    };
}

/// Find the index of a module environment in the all_module_envs slice.
pub fn findModuleEnvIdx(all_module_envs: []const *ModuleEnv, module_env: *ModuleEnv) ?u32 {
    for (all_module_envs, 0..) |env, i| {
        if (env == module_env) {
            return @intCast(i);
        }
    }
    return null;
}

/// Build a TypeScope for platform `requires` type variables.
/// Maps platform flex vars from `requires { model : Model }` to the app's concrete types.
pub fn buildPlatformTypeScope(
    allocator: Allocator,
    platform_env: *const ModuleEnv,
    app_env: *const ModuleEnv,
) !types.TypeScope {
    var type_scope = types.TypeScope.init(allocator);
    errdefer type_scope.deinit();

    try type_scope.scopes.append(types.VarMap.init(allocator));
    const rigid_scope = &type_scope.scopes.items[0];
    const all_aliases = platform_env.for_clause_aliases.items.items;

    for (platform_env.requires_types.items.items) |required_type| {
        const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (type_aliases_slice) |alias| {
            const alias_stmt = platform_env.store.getStatement(alias.alias_stmt_idx);
            std.debug.assert(alias_stmt == .s_alias_decl);
            const alias_body_var = ModuleEnv.varFrom(alias_stmt.s_alias_decl.anno);
            const alias_stmt_var = ModuleEnv.varFrom(alias.alias_stmt_idx);
            const app_alias_name = app_env.common.findIdent(platform_env.getIdentText(alias.alias_name)) orelse continue;
            const app_var = findTypeAliasBodyVar(app_env, app_alias_name) orelse continue;
            try rigid_scope.put(alias_body_var, app_var);
            try rigid_scope.put(alias_stmt_var, app_var);
        }
    }

    return type_scope;
}

/// Find a type alias declaration by name in a module and return the var for its underlying type.
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

/// Check if a module environment is the builtin module.
pub fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    return env.display_module_name_idx.eql(env.idents.builtin_module);
}

/// Shared LIR lowering pipeline.
///
/// Manages a global layout store (cached across evaluations) and provides
/// the full CIR → MIR → LIR → RC lowering pipeline as a single operation.
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

    /// Result of lowering a CIR expression to post-RC LIR.
    /// The consumer takes ownership of `lir_store` and must call `deinit()`.
    pub const LowerResult = struct {
        lir_store: LirExprStore,
        final_expr_id: lir.LirExprId,
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

    /// Get or create the global layout store.
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

    /// Get or create the global type layout resolver.
    pub fn ensureGlobalTypeLayoutResolver(self: *LirProgram, all_module_envs: []const *ModuleEnv) Error!*layout.TypeLayoutResolver {
        if (self.global_type_layout_resolver) |resolver| return resolver;

        const layout_store = try self.ensureGlobalLayoutStore(all_module_envs);
        const resolver = self.allocator.create(layout.TypeLayoutResolver) catch return error.OutOfMemory;
        resolver.* = layout.TypeLayoutResolver.init(layout_store);
        self.global_type_layout_resolver = resolver;
        return resolver;
    }

    /// Prepare the layout stores for a new lowering pass.
    ///
    /// Updates module envs on the layout store and resets stale type-side caches
    /// in the type layout resolver. Call this before `lowerExprInner` or before
    /// doing manual MIR lowering for entrypoint code.
    pub fn prepareLayoutStores(self: *LirProgram, all_module_envs: []const *ModuleEnv) Error!*layout.Store {
        const layout_store_ptr = try self.ensureGlobalLayoutStore(all_module_envs);
        layout_store_ptr.setModuleEnvs(all_module_envs);
        const type_layout_resolver_ptr = try self.ensureGlobalTypeLayoutResolver(all_module_envs);
        type_layout_resolver_ptr.resetModuleCache(all_module_envs);
        return layout_store_ptr;
    }

    /// Lower a CIR expression through the full pipeline: CIR → MIR → LIR → RC.
    ///
    /// Handles all pre-lowering setup:
    /// - Enables runtime inserts on all module interners
    /// - Resolves module imports
    /// - Prepares layout stores
    pub fn lowerExpr(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
    ) Error!LowerResult {
        // Enable runtime inserts on all module interners.
        // MIR lowering may need to translate structural identifiers between
        // modules (e.g. record fields in cross-module specializations).
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }

        // Resolve imports for this module ordering.
        module_env.imports.resolveImports(module_env, all_module_envs);

        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const app_module_idx = if (app_module_env) |env|
            findModuleEnvIdx(all_module_envs, env) orelse return error.ModuleEnvNotFound
        else
            null;

        const layout_store_ptr = try self.prepareLayoutStores(all_module_envs);

        return self.lowerExprInner(module_env, expr_idx, all_module_envs, module_idx, app_module_idx, layout_store_ptr);
    }

    /// Lower a CIR entrypoint expression to post-RC LIR.
    ///
    /// When `wrap_zero_arg_call` is true and the MIR expression has a function
    /// type, wraps it in a zero-arg call so the result is the function's return
    /// value, not a lambda. This is the same wrapping the dev evaluator does.
    pub fn lowerEntrypointExpr(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        app_module_env: ?*ModuleEnv,
        wrap_zero_arg_call: bool,
        type_scope: ?*const types.TypeScope,
    ) Error!LowerResult {
        // Pre-lowering setup
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

        // CIR → MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            all_module_envs,
            &module_env.types,
            module_idx,
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        // Apply platform TypeScope if provided (maps requires flex vars to app types)
        if (type_scope) |ts| {
            if (app_module_idx) |ami| {
                mir_lower.setTypeScope(module_idx, ts, ami) catch return error.OutOfMemory;
            }
        }

        var mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.RuntimeError;
        };

        // Wrap zero-arg functions in a call (same logic as dev evaluator)
        if (wrap_zero_arg_call) {
            const func_mono_idx = mir_store.typeOf(mir_expr_id);
            const resolved = mir_store.monotype_store.resolve(func_mono_idx);
            if (resolved.kind == .func) {
                const ret = mir_store.monotype_store.funcRet(resolved);
                mir_expr_id = mir_store.addExpr(self.allocator, .{ .call = .{
                    .func = mir_expr_id,
                    .args = MIR.ExprSpan.empty(),
                } }, ret, base.Region.zero()) catch return error.OutOfMemory;
            }
        }

        return self.lowerFromMir(module_env, expr_idx, all_module_envs, &mir_store, mir_expr_id, layout_store_ptr);
    }

    /// Lower a CIR expression to post-RC LIR, given already-resolved module indices
    /// and a prepared layout store. Use this when you need to do additional MIR
    /// manipulation (e.g. wrapping zero-arg entrypoints) between CIR→MIR and MIR→LIR.
    ///
    /// For the common case, use `lowerExpr` instead.
    fn lowerExprInner(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
        app_module_idx: ?u32,
        layout_store_ptr: *layout.Store,
    ) Error!LowerResult {
        // CIR → MIR
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            all_module_envs,
            &module_env.types,
            module_idx,
            app_module_idx,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        const mir_expr_id = mir_lower.lowerExpr(expr_idx) catch {
            return error.RuntimeError;
        };

        return self.lowerFromMir(module_env, expr_idx, all_module_envs, &mir_store, mir_expr_id, layout_store_ptr);
    }

    /// Complete the lowering pipeline from MIR onwards: lambda set inference → LIR → RC.
    ///
    /// Exposed so that callers who need to manipulate MIR (e.g. wrapping zero-arg
    /// entrypoints in a call) can do CIR→MIR themselves, modify the MIR expression,
    /// then call this to finish lowering.
    pub fn lowerFromMir(
        self: *LirProgram,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        all_module_envs: []const *ModuleEnv,
        mir_store: *MIR.Store,
        mir_expr_id: MIR.ExprId,
        layout_store_ptr: *layout.Store,
    ) Error!LowerResult {
        // Lambda set inference
        var lambda_set_store = mir.LambdaSet.infer(self.allocator, mir_store, all_module_envs) catch return error.OutOfMemory;
        defer lambda_set_store.deinit(self.allocator);

        // MIR → LIR
        var lir_store = LirExprStore.init(self.allocator);
        errdefer lir_store.deinit();

        var mir_to_lir = lir.MirToLir.init(self.allocator, mir_store, &lir_store, layout_store_ptr, &lambda_set_store, module_env.idents.true_tag);
        defer mir_to_lir.deinit();

        const lir_expr_id = mir_to_lir.lower(mir_expr_id) catch {
            return error.RuntimeError;
        };

        // RC insertion
        var rc_pass = lir.RcInsert.RcInsertPass.init(self.allocator, &lir_store, layout_store_ptr) catch return error.OutOfMemory;
        defer rc_pass.deinit();
        const final_expr_id = rc_pass.insertRcOps(lir_expr_id) catch lir_expr_id;

        lir.RcInsert.insertRcOpsIntoSymbolDefsBestEffort(self.allocator, &lir_store, layout_store_ptr);

        // Extract result metadata from the CIR expression
        const cir_expr = module_env.store.getExpr(expr_idx);
        const result_layout = lirExprResultLayout(&lir_store, final_expr_id);
        const tuple_len: usize = if (cir_expr == .e_tuple)
            module_env.store.exprSlice(cir_expr.e_tuple.elems).len
        else
            1;

        return LowerResult{
            .lir_store = lir_store,
            .final_expr_id = final_expr_id,
            .result_layout = result_layout,
            .layout_store = layout_store_ptr,
            .tuple_len = tuple_len,
        };
    }
};
