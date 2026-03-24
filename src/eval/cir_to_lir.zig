//! Shared LIR Lowering Pipeline
//!
//! Centralizes the CIR → MIR → LIR → RC lowering pipeline used by
//! dev_evaluator, wasm_evaluator, and the interpreter.
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
const Monomorphize = mir.Monomorphize;
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
        .proc_call => |pc| pc.ret_layout,
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

    /// Result of batch-lowering multiple module defs to post-RC LIR in a single shared pipeline.
    /// The consumer takes ownership of `lir_store` and must call `deinit()`.
    pub const BatchLowerResult = struct {
        lir_store: LirExprStore,
        layout_store: *layout.Store,
        def_lir_exprs: []DefLirExpr,
        /// The single LIR block expression that chains all def evaluations.
        block_expr_id: lir.LirExprId,
        allocator: Allocator,

        pub const DefLirExpr = struct {
            def_idx: CIR.Def.Idx,
            expr_idx: CIR.Expr.Idx,
            lir_expr_id: lir.LirExprId,
            result_layout: layout.Idx,
            /// MIR/LIR symbol for this def's binding (for extracting values from interpreter).
            symbol: MIR.Symbol,
        };

        pub fn deinit(self: *BatchLowerResult) void {
            self.allocator.free(self.def_lir_exprs);
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

        // Run monomorphization pass to discover all proc templates and instances
        var mono_result = if (type_scope) |ts|
            if (app_module_idx) |ami|
                Monomorphize.runExprWithTypeScope(
                    self.allocator,
                    all_module_envs,
                    &module_env.types,
                    module_idx,
                    app_module_idx,
                    expr_idx,
                    module_idx,
                    ts,
                    ami,
                ) catch return error.OutOfMemory
            else
                Monomorphize.runExpr(
                    self.allocator,
                    all_module_envs,
                    &module_env.types,
                    module_idx,
                    app_module_idx,
                    expr_idx,
                ) catch return error.OutOfMemory
        else
            Monomorphize.runExpr(
                self.allocator,
                all_module_envs,
                &module_env.types,
                module_idx,
                app_module_idx,
                expr_idx,
            ) catch return error.OutOfMemory;
        defer mono_result.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &mono_result,
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
            const resolved = mir_store.monotype_store.getMonotype(func_mono_idx);
            if (resolved == .func) {
                const ret = resolved.func.ret;
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

        // Run monomorphization pass to discover all proc templates and instances
        var mono_result2 = Monomorphize.runExpr(
            self.allocator,
            all_module_envs,
            &module_env.types,
            module_idx,
            app_module_idx,
            expr_idx,
        ) catch return error.OutOfMemory;
        defer mono_result2.deinit(self.allocator);

        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &mono_result2,
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

    /// Batch-lower multiple module defs through a single shared pipeline.
    ///
    /// All defs share ONE Monomorphize pass and ONE MIR Lower. After lowering
    /// each def expression to MIR, a synthetic MIR block is constructed with
    /// `decl_const` statements that bind each def's value to its symbol. This
    /// block is then converted to LIR via a single `lowerFromMir` call.
    ///
    /// The interpreter evaluates the block, executing declarations in order.
    /// Cross-def lookups resolve through the interpreter's local bindings —
    /// closures stay as live interpreter values without CIR round-tripping.
    pub fn lowerModuleDefs(
        self: *LirProgram,
        module_env: *ModuleEnv,
        def_indices: []const CIR.Def.Idx,
        all_module_envs: []const *ModuleEnv,
    ) Error!BatchLowerResult {
        // Pre-lowering setup (same as lowerExpr)
        for (all_module_envs) |env| {
            env.common.idents.interner.enableRuntimeInserts(env.gpa) catch return error.OutOfMemory;
        }
        module_env.imports.resolveImports(module_env, all_module_envs);

        const module_idx = findModuleEnvIdx(all_module_envs, module_env) orelse return error.ModuleEnvNotFound;
        const layout_store_ptr = try self.prepareLayoutStores(all_module_envs);

        // Collect all def expressions for monomorphization
        const def_exprs = self.allocator.alloc(CIR.Expr.Idx, def_indices.len) catch return error.OutOfMemory;
        defer self.allocator.free(def_exprs);
        for (def_indices, 0..) |def_idx, i| {
            def_exprs[i] = module_env.store.getDef(def_idx).expr;
        }

        // CIR → MIR: shared stores
        var mir_store = MIR.Store.init(self.allocator) catch return error.OutOfMemory;
        defer mir_store.deinit(self.allocator);

        // Monomorphize all def expressions together
        var mono_result = Monomorphize.runRoots(
            self.allocator,
            all_module_envs,
            &module_env.types,
            module_idx,
            null,
            def_exprs,
        ) catch return error.OutOfMemory;
        defer mono_result.deinit(self.allocator);

        // Create ONE MIR Lower for all defs
        var mir_lower = mir.Lower.init(
            self.allocator,
            &mir_store,
            &mono_result,
            all_module_envs,
            &module_env.types,
            module_idx,
            null,
        ) catch return error.OutOfMemory;
        defer mir_lower.deinit();

        // Lower each def expression and build MIR block statements.
        // Each def becomes a `decl_const pattern = expr` statement so the
        // interpreter creates proper local bindings for cross-def lookups.
        var stmts: std.ArrayList(MIR.Stmt) = .empty;
        defer stmts.deinit(self.allocator);
        var def_symbols: std.ArrayList(MIR.Symbol) = .empty;
        defer def_symbols.deinit(self.allocator);
        var def_mir_exprs: std.ArrayList(MIR.ExprId) = .empty;
        defer def_mir_exprs.deinit(self.allocator);
        var succeeded_indices: std.ArrayList(usize) = .empty;
        defer succeeded_indices.deinit(self.allocator);

        var last_mir_expr: ?MIR.ExprId = null;

        for (0..def_indices.len) |i| {
            const mir_expr_id = mir_lower.lowerExpr(def_exprs[i]) catch continue;

            // Get the MIR symbol for this def's pattern
            const def = module_env.store.getDef(def_indices[i]);
            const symbol = mir_lower.patternToSymbol(def.pattern) catch continue;
            const monotype = mir_store.typeOf(mir_expr_id);

            // Create a MIR bind pattern for this symbol
            const pattern_id = mir_store.addPattern(self.allocator, .{ .bind = symbol }, monotype) catch return error.OutOfMemory;

            // Register the symbol as lowered in both the Lower's cache and the
            // MIR store's value_defs. This prevents lowerExternalDefWithType from
            // re-lowering when a subsequent def references this symbol.
            mir_lower.registerLoweredSymbol(symbol, mir_expr_id) catch return error.OutOfMemory;
            if (mir_store.getValueDef(symbol) == null) {
                mir_store.registerValueDef(self.allocator, symbol, mir_expr_id) catch return error.OutOfMemory;
            }

            stmts.append(self.allocator, .{ .decl_const = .{
                .pattern = pattern_id,
                .expr = mir_expr_id,
            } }) catch return error.OutOfMemory;

            def_symbols.append(self.allocator, symbol) catch return error.OutOfMemory;
            def_mir_exprs.append(self.allocator, mir_expr_id) catch return error.OutOfMemory;
            succeeded_indices.append(self.allocator, i) catch return error.OutOfMemory;
            last_mir_expr = mir_expr_id;
        }

        if (last_mir_expr == null) return error.RuntimeError;

        // Construct a synthetic MIR block: { decl sym1 = expr1; ...; final_expr }
        const stmt_span = mir_store.addStmts(self.allocator, stmts.items) catch return error.OutOfMemory;
        const block_monotype = mir_store.typeOf(last_mir_expr.?);
        const block_expr = mir_store.addExpr(self.allocator, .{ .block = .{
            .stmts = stmt_span,
            .final_expr = last_mir_expr.?,
        } }, block_monotype, base.Region.zero()) catch return error.OutOfMemory;

        // Lower the synthetic block through MIR → LIR → RC
        const lower_result = try self.lowerFromMir(
            module_env,
            def_exprs[succeeded_indices.items[succeeded_indices.items.len - 1]],
            all_module_envs,
            &mir_store,
            block_expr,
            layout_store_ptr,
        );

        // Build per-def metadata for the caller.
        // Each def gets its own result_layout from CIR types (not the block's layout).
        const types_mod = @import("types");
        var empty_type_scope = types_mod.TypeScope.init(self.allocator);
        defer empty_type_scope.deinit();

        const def_lir_exprs = self.allocator.alloc(BatchLowerResult.DefLirExpr, succeeded_indices.items.len) catch return error.OutOfMemory;
        for (succeeded_indices.items, 0..) |orig_idx, j| {
            // Resolve this def's layout from its CIR type var
            const type_var = ModuleEnv.varFrom(def_exprs[orig_idx]);
            const def_layout = layout_store_ptr.fromTypeVar(module_idx, type_var, &empty_type_scope, null) catch lower_result.result_layout;
            def_lir_exprs[j] = .{
                .def_idx = def_indices[orig_idx],
                .expr_idx = def_exprs[orig_idx],
                .lir_expr_id = lower_result.final_expr_id,
                .result_layout = def_layout,
                .symbol = def_symbols.items[j],
            };
        }

        return BatchLowerResult{
            .lir_store = lower_result.lir_store,
            .layout_store = layout_store_ptr,
            .def_lir_exprs = def_lir_exprs,
            .block_expr_id = lower_result.final_expr_id,
            .allocator = self.allocator,
        };
    }
};
