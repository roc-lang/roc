//! Interpreter implementing the type-carrying architecture.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const trace_eval = build_options.trace_eval;
const base_pkg = @import("base");
const types = @import("types");
const import_mapping_mod = types.import_mapping;
const layout = @import("layout");
const can = @import("can");
const TypeScope = types.TypeScope;
const Content = types.Content;
const HashMap = std.hash_map.HashMap;
const unify = @import("check").unifier;
const problem_mod = @import("check").problem;
const snapshot_mod = @import("check").snapshot;
const stack = @import("stack.zig");
const StackValue = @import("StackValue.zig");
const render_helpers = @import("render_helpers.zig");
const builtins = @import("builtins");
const RocOps = builtins.host_abi.RocOps;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const RocStr = builtins.str.RocStr;
const RocDec = builtins.dec.RocDec;
const RocList = builtins.list.RocList;
const utils = builtins.utils;
const Layout = layout.Layout;
const helpers = @import("test/helpers.zig");
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const BuiltinTypes = @import("builtins.zig").BuiltinTypes;

/// Context structure for inc/dec callbacks in list operations
const RefcountContext = struct {
    layout_store: *layout.Store,
    elem_layout: Layout,
    roc_ops: *RocOps,
};

/// Increment callback for list operations - increments refcount of element via StackValue
fn listElementInc(context_opaque: ?*anyopaque, elem_ptr: ?[*]u8) callconv(.c) void {
    const context: *RefcountContext = @ptrCast(@alignCast(context_opaque.?));
    const elem_value = StackValue{
        .layout = context.elem_layout,
        .ptr = @ptrCast(elem_ptr),
        .is_initialized = true,
    };
    elem_value.incref();
}

/// Decrement callback for list operations - decrements refcount of element via StackValue
fn listElementDec(context_opaque: ?*anyopaque, elem_ptr: ?[*]u8) callconv(.c) void {
    const context: *RefcountContext = @ptrCast(@alignCast(context_opaque.?));
    const elem_value = StackValue{
        .layout = context.elem_layout,
        .ptr = @ptrCast(elem_ptr),
        .is_initialized = true,
    };
    elem_value.decref(context.layout_store, context.roc_ops);
}

/// Compare two layouts for equality
/// For lists, this compares the element layout index, so two lists with
/// different element types (e.g., List(Dec) vs List(generic_num)) will be different.
fn layoutsEqual(a: Layout, b: Layout) bool {
    if (a.tag != b.tag) return false;
    return switch (a.tag) {
        .scalar => std.meta.eql(a.data.scalar, b.data.scalar),
        .list => a.data.list == b.data.list,
        .list_of_zst => true,
        .box => a.data.box == b.data.box,
        .box_of_zst => true,
        .record => std.meta.eql(a.data.record, b.data.record),
        .tuple => std.meta.eql(a.data.tuple, b.data.tuple),
        .closure => std.meta.eql(a.data.closure, b.data.closure),
        .zst => true,
    };
}

fn interpreterLookupModuleEnv(
    ctx: ?*const anyopaque,
    module_ident: base_pkg.Ident.Idx,
) ?*const can.ModuleEnv {
    const map_ptr = ctx orelse return null;
    const map: *const std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv) =
        @ptrCast(@alignCast(map_ptr));

    if (map.*.get(module_ident)) |entry| {
        return entry;
    }
    return null;
}

/// Interpreter that evaluates canonical Roc expressions against runtime types/layouts.
pub const Interpreter = struct {
    pub const Error = error{
        Crash,
        DivisionByZero,
        EarlyReturn,
        IntegerOverflow,
        InvalidMethodReceiver,
        InvalidNumExt,
        InvalidTagExt,
        ListIndexOutOfBounds,
        MethodLookupFailed,
        MethodNotFound,
        NoSpaceLeft,
        NotNumeric,
        NullStackPointer,
        RecordIndexOutOfBounds,
        StringOrderingNotSupported,
        StackOverflow,
        TupleIndexOutOfBounds,
        TypeMismatch,
        ZeroSizedType,
    } || std.mem.Allocator.Error || layout.LayoutError;
    const PolyKey = struct {
        module_id: u32,
        func_id: u32,
        args_len: u32,
        args_ptr: [*]const types.Var,

        fn slice(self: PolyKey) []const types.Var {
            if (self.args_len == 0) return &.{};
            return self.args_ptr[0..self.args_len];
        }

        fn init(module_id: u32, func_id: u32, args: []const types.Var) PolyKey {
            return .{
                .module_id = module_id,
                .func_id = func_id,
                .args_len = @intCast(args.len),
                .args_ptr = if (args.len == 0) undefined else args.ptr,
            };
        }
    };

    const PolyEntry = struct {
        return_var: types.Var,
        return_layout_slot: u32,
        args: []const types.Var,
    };

    const PolyKeyCtx = struct {
        pub fn hash(_: PolyKeyCtx, k: PolyKey) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&k.module_id));
            h.update(std.mem.asBytes(&k.func_id));
            h.update(std.mem.asBytes(&k.args_len));
            if (k.args_len > 0) {
                var i: usize = 0;
                while (i < k.args_len) : (i += 1) {
                    const v_int: u32 = @intFromEnum(k.args_ptr[i]);
                    h.update(std.mem.asBytes(&v_int));
                }
            }
            return h.final();
        }
        pub fn eql(_: PolyKeyCtx, a: PolyKey, b: PolyKey) bool {
            if (a.module_id != b.module_id or a.func_id != b.func_id or a.args_len != b.args_len) return false;
            // Compare type variable indices element-wise
            for (0..a.args_len) |i| {
                if (a.args_ptr[i] != b.args_ptr[i]) return false;
            }
            return true;
        }
    };
    const Binding = struct {
        pattern_idx: can.CIR.Pattern.Idx,
        value: StackValue,
        expr_idx: can.CIR.Expr.Idx,
        /// The source module environment where this binding was created.
        /// Used to distinguish bindings from different modules with the same pattern_idx.
        source_env: *const can.ModuleEnv,
    };
    const DefInProgress = struct {
        pattern_idx: can.CIR.Pattern.Idx,
        expr_idx: can.CIR.Expr.Idx,
        value: ?StackValue,
    };
    allocator: std.mem.Allocator,
    runtime_types: *types.store.Store,
    runtime_layout_store: layout.Store,
    // O(1) Var -> Layout slot cache (0 = unset, else layout_idx + 1)
    var_to_layout_slot: std.array_list.Managed(u32),
    // Empty scope used when converting runtime vars to layouts
    empty_scope: TypeScope,
    // Translation cache: (env_ptr, compile_var) -> runtime_var
    translate_cache: std.AutoHashMap(u64, types.Var),
    // Rigid variable substitution context for generic function instantiation
    // Maps rigid type variables to their concrete instantiations
    rigid_subst: std.AutoHashMap(types.Var, types.Var),

    // Polymorphic instantiation cache

    poly_cache: HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80),

    // Runtime unification context
    env: *can.ModuleEnv,
    /// Root module used for method idents (is_lt, is_eq, etc.) - never changes during execution
    root_env: *can.ModuleEnv,
    builtin_module_env: ?*const can.ModuleEnv,
    /// Array of all module environments, indexed by resolved module index
    /// Used to resolve imports via pre-resolved indices in env.imports.resolved_modules
    all_module_envs: []const *const can.ModuleEnv,
    module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
    module_ids: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32),
    import_envs: std.AutoHashMapUnmanaged(can.CIR.Import.Idx, *const can.ModuleEnv),
    current_module_id: u32,
    next_module_id: u32,
    problems: problem_mod.Store,
    snapshots: snapshot_mod.Store,
    import_mapping: *const import_mapping_mod.ImportMapping,
    unify_scratch: unify.Scratch,

    // Minimal eval support
    stack_memory: stack.Stack,
    bindings: std.array_list.Managed(Binding),
    // Track active closures during calls (for capture lookup)
    active_closures: std.array_list.Managed(StackValue),
    canonical_bool_rt_var: ?types.Var,
    // Used to unwrap extensible tags
    scratch_tags: std.array_list.Managed(types.Tag),
    /// Builtin types required by the interpreter (Bool, Try, etc.)
    builtins: BuiltinTypes,
    def_stack: std.array_list.Managed(DefInProgress),
    /// Target type for num_from_numeral (set by callLowLevelBuiltinWithTargetType)
    num_literal_target_type: ?types.Var,
    /// Last error message from num_from_numeral when payload area is too small
    last_error_message: ?[]const u8,
    /// Value being returned early from a function (set by s_return, consumed at function boundaries)
    early_return_value: ?StackValue,

    pub fn init(allocator: std.mem.Allocator, env: *can.ModuleEnv, builtin_types: BuiltinTypes, builtin_module_env: ?*const can.ModuleEnv, other_envs: []const *const can.ModuleEnv, import_mapping: *const import_mapping_mod.ImportMapping) !Interpreter {
        // Build maps from Ident.Idx to ModuleEnv and module ID
        var module_envs = std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv){};
        errdefer module_envs.deinit(allocator);
        var module_ids = std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32){};
        errdefer module_ids.deinit(allocator);
        var import_envs = std.AutoHashMapUnmanaged(can.CIR.Import.Idx, *const can.ModuleEnv){};
        errdefer import_envs.deinit(allocator);

        var next_id: u32 = 1; // Start at 1, reserve 0 for current module

        // Safely access import count
        const import_count = if (env.imports.imports.items.items.len > 0)
            env.imports.imports.items.items.len
        else
            0;

        if (other_envs.len > 0 and import_count > 0) {
            // Allocate capacity for all imports (even if some are duplicates)
            try module_envs.ensureTotalCapacity(allocator, @intCast(other_envs.len));
            try module_ids.ensureTotalCapacity(allocator, @intCast(other_envs.len));
            try import_envs.ensureTotalCapacity(allocator, @intCast(import_count));

            // Process ALL imports using pre-resolved module indices
            // Note: Some imports may be unresolved (e.g., platform modules in test context).
            // We skip unresolved imports here - errors will occur at point-of-use if the
            // code actually tries to access an unresolved import.
            for (0..import_count) |i| {
                const import_idx: can.CIR.Import.Idx = @enumFromInt(i);

                // Use pre-resolved module index - skip if not resolved
                const resolved_idx = env.imports.getResolvedModule(import_idx) orelse continue;

                if (resolved_idx >= other_envs.len) continue;

                const module_env = other_envs[resolved_idx];

                // Store in import_envs (always, for every import)
                // This is the critical mapping that e_lookup_external needs!
                import_envs.putAssumeCapacity(import_idx, module_env);

                // Also add to module_envs/module_ids for module lookups (optional, only if ident exists)
                // Use pre-stored ident index instead of string lookup
                if (env.imports.getIdentIdx(import_idx)) |idx| {
                    // Only add to module_envs/module_ids if not already present (to avoid duplicates)
                    if (!module_envs.contains(idx)) {
                        module_envs.putAssumeCapacity(idx, module_env);
                        module_ids.putAssumeCapacity(idx, next_id);
                        next_id += 1;
                    }
                }
            }
        }

        return initWithModuleEnvs(allocator, env, other_envs, module_envs, module_ids, import_envs, next_id, builtin_types, builtin_module_env, import_mapping);
    }

    /// Deinit the interpreter and also free the module maps if they were allocated by init()
    pub fn deinitAndFreeOtherEnvs(self: *Interpreter) void {
        self.deinit();
    }

    pub fn initWithModuleEnvs(
        allocator: std.mem.Allocator,
        env: *can.ModuleEnv,
        all_module_envs: []const *const can.ModuleEnv,
        module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
        module_ids: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32),
        import_envs: std.AutoHashMapUnmanaged(can.CIR.Import.Idx, *const can.ModuleEnv),
        next_module_id: u32,
        builtin_types: BuiltinTypes,
        builtin_module_env: ?*const can.ModuleEnv,
        import_mapping: *const import_mapping_mod.ImportMapping,
    ) !Interpreter {
        const rt_types_ptr = try allocator.create(types.store.Store);
        rt_types_ptr.* = try types.store.Store.initCapacity(allocator, 1024, 512);
        var slots = try std.array_list.Managed(u32).initCapacity(allocator, 1024);
        slots.appendNTimesAssumeCapacity(0, 1024);
        const scope = TypeScope.init(allocator);
        var result = Interpreter{
            .allocator = allocator,
            .runtime_types = rt_types_ptr,
            .runtime_layout_store = undefined, // set below to point at result.runtime_types
            .var_to_layout_slot = slots,
            .empty_scope = scope,
            .translate_cache = std.AutoHashMap(u64, types.Var).init(allocator),
            .rigid_subst = std.AutoHashMap(types.Var, types.Var).init(allocator),
            .poly_cache = HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80).init(allocator),
            .env = env,
            .root_env = env, // Root env is the original env passed to init - used for method idents
            .builtin_module_env = builtin_module_env,
            .all_module_envs = all_module_envs,
            .module_envs = module_envs,
            .module_ids = module_ids,
            .import_envs = import_envs,
            .current_module_id = 0, // Current module always gets ID 0
            .next_module_id = next_module_id,
            .problems = try problem_mod.Store.initCapacity(allocator, 64),
            .snapshots = try snapshot_mod.Store.initCapacity(allocator, 256),
            .import_mapping = import_mapping,
            .unify_scratch = try unify.Scratch.init(allocator),
            .stack_memory = try stack.Stack.initCapacity(allocator, 8 * 1024 * 1024), // 8MB stack
            .bindings = try std.array_list.Managed(Binding).initCapacity(allocator, 8),
            .active_closures = try std.array_list.Managed(StackValue).initCapacity(allocator, 4),
            .canonical_bool_rt_var = null,
            .scratch_tags = try std.array_list.Managed(types.Tag).initCapacity(allocator, 8),
            .builtins = builtin_types,
            .def_stack = try std.array_list.Managed(DefInProgress).initCapacity(allocator, 4),
            .num_literal_target_type = null,
            .last_error_message = null,
            .early_return_value = null,
        };

        // Use the pre-interned "Builtin.Str" identifier from the module env
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types, env.idents.builtin_str);

        return result;
    }

    // Minimal evaluator for subset: string literals, lambdas without captures, and lambda calls
    pub fn evalMinimal(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) Error!StackValue {
        return try self.evalExprMinimal(expr_idx, roc_ops, null);
    }

    pub fn registerDefValue(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, value: StackValue) void {
        if (self.def_stack.items.len == 0) return;
        var top = &self.def_stack.items[self.def_stack.items.len - 1];
        if (top.expr_idx == expr_idx and top.value == null) {
            top.value = value;
        }
    }

    pub fn startTrace(self: *Interpreter) void {
        _ = self;
    }

    pub fn endTrace(self: *Interpreter) void {
        _ = self;
    }

    pub fn evaluateExpression(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        ret_ptr: *anyopaque,
        roc_ops: *RocOps,
        arg_ptr: ?*anyopaque,
    ) Error!void {
        if (arg_ptr) |args_ptr| {
            const func_val = try self.evalMinimal(expr_idx, roc_ops);
            defer func_val.decref(&self.runtime_layout_store, roc_ops);

            if (func_val.layout.tag != .closure) {
                self.triggerCrash("evalEntry: expected closure layout, got something else", false, roc_ops);
                return error.Crash;
            }

            const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));

            // Switch to the closure's source module for correct expression evaluation.
            // This is critical because pattern indices and expression indices in the closure
            // are relative to the source module where the closure was defined, not the
            // current module. Without this switch, bindings created in the closure body
            // would have the wrong source_env and lookups would fail.
            const saved_env = self.env;
            self.env = @constCast(header.source_env);
            defer self.env = saved_env;

            const params = self.env.store.slicePatterns(header.params);

            try self.active_closures.append(func_val);
            defer _ = self.active_closures.pop();

            const base_binding_len = self.bindings.items.len;

            var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, params.len);
            defer {
                self.trimBindingList(&temp_binds, 0, roc_ops);
                temp_binds.deinit();
            }

            var param_rt_vars = try self.allocator.alloc(types.Var, params.len);
            defer self.allocator.free(param_rt_vars);

            var param_layouts: []layout.Layout = &.{};
            if (params.len > 0) {
                param_layouts = try self.allocator.alloc(layout.Layout, params.len);
            }
            defer if (param_layouts.len > 0) self.allocator.free(param_layouts);

            var args_tuple_value: StackValue = undefined;
            var args_accessor: StackValue.TupleAccessor = undefined;
            if (params.len > 0) {
                var i: usize = 0;
                while (i < params.len) : (i += 1) {
                    const param_idx = params[i];
                    const param_var = can.ModuleEnv.varFrom(param_idx);
                    const rt_var = try self.translateTypeVar(self.env, param_var);
                    param_rt_vars[i] = rt_var;
                    param_layouts[i] = try self.getRuntimeLayout(rt_var);
                }

                const tuple_idx = try self.runtime_layout_store.putTuple(param_layouts);
                const tuple_layout = self.runtime_layout_store.getLayout(tuple_idx);
                args_tuple_value = StackValue{ .layout = tuple_layout, .ptr = args_ptr, .is_initialized = true };
                args_accessor = try args_tuple_value.asTuple(&self.runtime_layout_store);

                var j: usize = 0;
                while (j < params.len) : (j += 1) {
                    // getElement expects original index and converts to sorted internally
                    const arg_value = try args_accessor.getElement(j);
                    const matched = try self.patternMatchesBind(params[j], arg_value, param_rt_vars[j], roc_ops, &temp_binds, @enumFromInt(0));
                    if (!matched) return error.TypeMismatch;
                }
            }

            if (params.len == 0) {
                // Nothing to bind for zero-argument functions
            } else {
                for (temp_binds.items) |binding| {
                    try self.bindings.append(binding);
                }
                temp_binds.items.len = 0;
            }

            defer self.trimBindingList(&self.bindings, base_binding_len, roc_ops);

            // Evaluate body, handling early returns at function boundary
            const result_value = self.evalExprMinimal(header.body_idx, roc_ops, null) catch |err| {
                if (err == error.EarlyReturn) {
                    const return_val = self.early_return_value orelse return error.Crash;
                    self.early_return_value = null;
                    defer return_val.decref(&self.runtime_layout_store, roc_ops);
                    if (try self.shouldCopyResult(return_val, ret_ptr, roc_ops)) {
                        try return_val.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
                    }
                    return;
                }
                return err;
            };
            defer result_value.decref(&self.runtime_layout_store, roc_ops);

            // Only copy result if the result type is compatible with ret_ptr
            if (try self.shouldCopyResult(result_value, ret_ptr, roc_ops)) {
                try result_value.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
            }
            return;
        }

        const result = try self.evalMinimal(expr_idx, roc_ops);
        defer result.decref(&self.runtime_layout_store, roc_ops);

        // Only copy result if the result type is compatible with ret_ptr
        if (try self.shouldCopyResult(result, ret_ptr, roc_ops)) {
            try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
        }
    }

    /// Check if the result should be copied to ret_ptr based on the result's layout.
    /// Returns false for zero-sized types (nothing to copy).
    /// Validates that ret_ptr is properly aligned for the result type.
    fn shouldCopyResult(self: *Interpreter, result: StackValue, ret_ptr: *anyopaque, _: *RocOps) !bool {
        const result_size = self.runtime_layout_store.layoutSize(result.layout);
        if (result_size == 0) {
            // Zero-sized types don't need copying
            return false;
        }

        // Validate alignment: ret_ptr must be properly aligned for the result type.
        // A mismatch here indicates a type error between what the platform expects
        // and what the Roc code returns. This should have been caught at compile
        // time, but if the type checking didn't enforce the constraint, we catch
        // it here at runtime.
        const required_alignment = result.layout.alignment(self.runtime_layout_store.targetUsize());
        const ret_addr = @intFromPtr(ret_ptr);
        if (ret_addr % required_alignment.toByteUnits() != 0) {
            return error.TypeMismatch;
        }

        return true;
    }

    fn evalExprMinimal(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        roc_ops: *RocOps,
        expected_rt_var: ?types.Var,
    ) Error!StackValue {
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            .e_block => |blk| {
                // New scope for bindings
                const original_len = self.bindings.items.len;
                defer self.trimBindingList(&self.bindings, original_len, roc_ops);

                const stmts = self.env.store.sliceStatements(blk.stmts);

                // First pass: add placeholders for all decl/var lambdas/closures (mutual recursion support)
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    const Placeholder = struct {
                        fn exists(self_interp: *Interpreter, start: usize, pattern_idx: can.CIR.Pattern.Idx) bool {
                            var i: usize = self_interp.bindings.items.len;
                            while (i > start) {
                                i -= 1;
                                if (self_interp.bindings.items[i].pattern_idx == pattern_idx) return true;
                            }
                            return false;
                        }
                        fn add(self_interp: *Interpreter, patt_idx: can.CIR.Pattern.Idx, rhs_expr: can.CIR.Expr.Idx) !void {
                            const patt_ct_var = can.ModuleEnv.varFrom(patt_idx);
                            const patt_rt_var = try self_interp.translateTypeVar(self_interp.env, patt_ct_var);
                            const closure_layout = try self_interp.getRuntimeLayout(patt_rt_var);
                            if (closure_layout.tag != .closure) return; // only closures get placeholders
                            const lam_or = self_interp.env.store.getExpr(rhs_expr);
                            var body_idx: can.CIR.Expr.Idx = rhs_expr;
                            var params: can.CIR.Pattern.Span = .{ .span = .{ .start = 0, .len = 0 } };
                            if (lam_or == .e_lambda) {
                                body_idx = lam_or.e_lambda.body;
                                params = lam_or.e_lambda.args;
                            } else if (lam_or == .e_closure) {
                                const lam_expr = self_interp.env.store.getExpr(lam_or.e_closure.lambda_idx);
                                if (lam_expr == .e_lambda) {
                                    body_idx = lam_expr.e_lambda.body;
                                    params = lam_expr.e_lambda.args;
                                }
                            } else return;
                            const ph = try self_interp.pushRaw(closure_layout, 0);
                            if (ph.ptr) |ptr| {
                                const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                                header.* = .{
                                    .body_idx = body_idx,
                                    .params = params,
                                    .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                                    .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                                    .lambda_expr_idx = rhs_expr,
                                    .source_env = self_interp.env,
                                };
                            }
                            try self_interp.bindings.append(.{ .pattern_idx = patt_idx, .value = ph, .expr_idx = rhs_expr, .source_env = self_interp.env });
                        }
                    };
                    switch (stmt) {
                        .s_decl => |d| {
                            const patt = self.env.store.getPattern(d.pattern);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(d.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, d.pattern)) {
                                try Placeholder.add(self, d.pattern, d.expr);
                            }
                        },
                        .s_decl_gen => |d| {
                            const patt = self.env.store.getPattern(d.pattern);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(d.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, d.pattern)) {
                                try Placeholder.add(self, d.pattern, d.expr);
                            }
                        },
                        .s_var => |v| {
                            const patt = self.env.store.getPattern(v.pattern_idx);
                            if (patt != .assign) continue;
                            const rhs = self.env.store.getExpr(v.expr);
                            if ((rhs == .e_lambda or rhs == .e_closure) and !Placeholder.exists(self, original_len, v.pattern_idx)) {
                                try Placeholder.add(self, v.pattern_idx, v.expr);
                            }
                        },
                        else => {},
                    }
                }

                // Second pass: evaluate statements, updating placeholders
                for (stmts) |stmt_idx| {
                    const stmt = self.env.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |d| {
                            const expr_ct_var = can.ModuleEnv.varFrom(d.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                            defer {
                                self.trimBindingList(&temp_binds, 0, roc_ops);
                                temp_binds.deinit();
                            }

                            const val = try self.evalExprMinimal(d.expr, roc_ops, expr_rt_var);
                            defer val.decref(&self.runtime_layout_store, roc_ops);

                            if (!try self.patternMatchesBind(d.pattern, val, expr_rt_var, roc_ops, &temp_binds, d.expr)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_decl_gen => |d| {
                            const expr_ct_var = can.ModuleEnv.varFrom(d.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                            defer {
                                self.trimBindingList(&temp_binds, 0, roc_ops);
                                temp_binds.deinit();
                            }

                            const val = try self.evalExprMinimal(d.expr, roc_ops, expr_rt_var);
                            defer val.decref(&self.runtime_layout_store, roc_ops);

                            if (!try self.patternMatchesBind(d.pattern, val, expr_rt_var, roc_ops, &temp_binds, d.expr)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_var => |v| {
                            const expr_ct_var = can.ModuleEnv.varFrom(v.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                            defer {
                                self.trimBindingList(&temp_binds, 0, roc_ops);
                                temp_binds.deinit();
                            }

                            const val = try self.evalExprMinimal(v.expr, roc_ops, expr_rt_var);
                            defer val.decref(&self.runtime_layout_store, roc_ops);

                            if (!try self.patternMatchesBind(v.pattern_idx, val, expr_rt_var, roc_ops, &temp_binds, v.expr)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_reassign => |r| {
                            const patt = self.env.store.getPattern(r.pattern_idx);
                            if (patt != .assign) {
                                self.triggerCrash("s_reassign: pattern is not an assign pattern", false, roc_ops);
                                return error.Crash;
                            }
                            const new_val = try self.evalExprMinimal(r.expr, roc_ops, null);
                            // Search through all bindings, not just current block scope
                            // This allows reassigning variables from outer scopes (e.g., in for loops)
                            var j: usize = self.bindings.items.len;
                            while (j > 0) {
                                j -= 1;
                                if (self.bindings.items[j].pattern_idx == r.pattern_idx) {
                                    self.bindings.items[j].value.decref(&self.runtime_layout_store, roc_ops);
                                    self.bindings.items[j].value = new_val;
                                    break;
                                }
                            }
                        },
                        .s_crash => |c| {
                            const msg = self.env.getString(c.msg);
                            self.triggerCrash(msg, false, roc_ops);
                            return error.Crash;
                        },
                        .s_expect => |expect_stmt| {
                            const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
                            const cond_val = try self.evalExprMinimal(expect_stmt.body, roc_ops, bool_rt_var);
                            const is_true = boolValueEquals(true, cond_val);
                            if (!is_true) {
                                self.handleExpectFailure(expect_stmt.body, roc_ops);
                                return error.Crash;
                            }
                        },
                        .s_expr => |sx| {
                            // Evaluate the expression - propagate early returns
                            const expr_result = self.evalExprMinimal(sx.expr, roc_ops, null) catch |err| {
                                return err; // Propagate EarlyReturn or other errors
                            };
                            expr_result.decref(&self.runtime_layout_store, roc_ops);
                        },
                        .s_dbg => |dbg_stmt| {
                            const inner_ct_var = can.ModuleEnv.varFrom(dbg_stmt.expr);
                            const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                            const value = try self.evalExprMinimal(dbg_stmt.expr, roc_ops, inner_rt_var);
                            defer value.decref(&self.runtime_layout_store, roc_ops);
                            const rendered = try self.renderValueRocWithType(value, inner_rt_var);
                            defer self.allocator.free(rendered);
                            roc_ops.dbg(rendered);
                        },
                        .s_for => |for_stmt| {
                            // Evaluate the list expression
                            const expr_ct_var = can.ModuleEnv.varFrom(for_stmt.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            const list_value = try self.evalExprMinimal(for_stmt.expr, roc_ops, expr_rt_var);
                            defer list_value.decref(&self.runtime_layout_store, roc_ops);

                            // Get the list layout
                            if (list_value.layout.tag != .list) {
                                return error.TypeMismatch;
                            }
                            const elem_layout_idx = list_value.layout.data.list;
                            const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                            const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));

                            // Get the RocList header
                            const list_header: *const RocList = @ptrCast(@alignCast(list_value.ptr.?));
                            const list_len = list_header.len();

                            // Get the element type for binding
                            const patt_ct_var = can.ModuleEnv.varFrom(for_stmt.patt);
                            const patt_rt_var = try self.translateTypeVar(self.env, patt_ct_var);

                            // Iterate over each element
                            var i: usize = 0;
                            while (i < list_len) : (i += 1) {
                                // Get pointer to element
                                const elem_ptr = if (list_header.bytes) |buffer|
                                    buffer + i * elem_size
                                else
                                    return error.TypeMismatch;

                                // Create a StackValue from the element
                                var elem_value = StackValue{
                                    .ptr = elem_ptr,
                                    .layout = elem_layout,
                                    .is_initialized = true,
                                };

                                // Increment refcount since we're creating a new reference
                                elem_value.incref();

                                // Bind the pattern to the element value
                                var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                                defer {
                                    self.trimBindingList(&temp_binds, 0, roc_ops);
                                    temp_binds.deinit();
                                }

                                if (!try self.patternMatchesBind(for_stmt.patt, elem_value, patt_rt_var, roc_ops, &temp_binds, @enumFromInt(0))) {
                                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                                    return error.TypeMismatch;
                                }

                                // Add bindings to the environment
                                const loop_bindings_start = self.bindings.items.len;
                                for (temp_binds.items) |binding| {
                                    try self.bindings.append(binding);
                                }

                                // Evaluate the body - handle early returns
                                const body_result = self.evalExprMinimal(for_stmt.body, roc_ops, null) catch |err| {
                                    // Clean up before propagating error
                                    self.trimBindingList(&self.bindings, loop_bindings_start, roc_ops);
                                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                                    return err; // Propagate EarlyReturn or other errors
                                };
                                body_result.decref(&self.runtime_layout_store, roc_ops);

                                // Clean up bindings for this iteration
                                self.trimBindingList(&self.bindings, loop_bindings_start, roc_ops);

                                // Decrement the element reference (it was incremented above)
                                elem_value.decref(&self.runtime_layout_store, roc_ops);
                            }
                        },
                        .s_while => |while_stmt| {
                            // Loop until condition becomes false
                            while (true) {
                                // 1. EVALUATE CONDITION
                                const cond_ct_var = can.ModuleEnv.varFrom(while_stmt.cond);
                                const cond_rt_var = try self.translateTypeVar(self.env, cond_ct_var);
                                const cond_value = try self.evalExprMinimal(while_stmt.cond, roc_ops, cond_rt_var);

                                // 2. CHECK IF CONDITION IS TRUE
                                const cond_is_true = boolValueEquals(true, cond_value);

                                // 3. EXIT LOOP IF CONDITION IS FALSE
                                if (!cond_is_true) {
                                    break;
                                }

                                // 4. EVALUATE BODY - propagate early returns
                                const body_result = self.evalExprMinimal(while_stmt.body, roc_ops, null) catch |err| {
                                    return err; // Propagate EarlyReturn or other errors
                                };
                                body_result.decref(&self.runtime_layout_store, roc_ops);

                                // Body result is {} (empty record), so nothing to do with it
                                // Loop continues to next iteration
                            }

                            // While loop completes and returns {} (implicitly)
                        },
                        .s_return => |ret| {
                            // Early return: evaluate expression, store value, signal return
                            const expr_ct_var = can.ModuleEnv.varFrom(ret.expr);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            const return_value = try self.evalExprMinimal(ret.expr, roc_ops, expr_rt_var);
                            // Store the return value for the caller to consume
                            self.early_return_value = return_value;
                            return error.EarlyReturn;
                        },
                        else => {
                            self.triggerCrash("e_block: unhandled statement type", false, roc_ops);
                            return error.Crash;
                        },
                    }
                }

                return try self.evalExprMinimal(blk.final_expr, roc_ops, null);
            },
            .e_num => |num_lit| {
                // Use runtime type to choose layout
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                var value = try self.pushRaw(layout_val, 0);
                // Write integer as i128 respecting precision via StackValue
                value.is_initialized = false;
                switch (layout_val.tag) {
                    .scalar => switch (layout_val.data.scalar.tag) {
                        .int => try value.setIntFromBytes(num_lit.value.bytes, num_lit.value.kind == .u128),
                        .frac => switch (layout_val.data.scalar.data.frac) {
                            .f32 => {
                                const ptr = @as(*f32, @ptrCast(@alignCast(value.ptr.?)));
                                // For u128 values, convert from the raw bytes
                                if (num_lit.value.kind == .u128) {
                                    const u128_val: u128 = @bitCast(num_lit.value.bytes);
                                    ptr.* = @floatFromInt(u128_val);
                                } else {
                                    ptr.* = @floatFromInt(num_lit.value.toI128());
                                }
                            },
                            .f64 => {
                                const ptr = @as(*f64, @ptrCast(@alignCast(value.ptr.?)));
                                if (num_lit.value.kind == .u128) {
                                    const u128_val: u128 = @bitCast(num_lit.value.bytes);
                                    ptr.* = @floatFromInt(u128_val);
                                } else {
                                    ptr.* = @floatFromInt(num_lit.value.toI128());
                                }
                            },
                            .dec => {
                                const ptr = @as(*RocDec, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = .{ .num = num_lit.value.toI128() * RocDec.one_point_zero_i128 };
                            },
                        },
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
                }
                value.is_initialized = true;
                return value;
            },
            .e_unary_minus => |unary_minus| {
                // Desugar `-a` to `a.negate()`
                // Use root_env.idents because method idents must be consistent across all modules
                return try self.dispatchUnaryOpMethod(self.root_env.idents.negate, unary_minus.expr, roc_ops);
            },
            .e_unary_not => |unary_not| {
                // Desugar `!a` to `a.not()`
                // Use root_env.idents because method idents must be consistent across all modules
                return try self.dispatchUnaryOpMethod(self.root_env.idents.not, unary_not.expr, roc_ops);
            },
            .e_binop => |binop| {
                // Use root_env.idents for all method dispatches because method idents must be
                // consistent across all modules. Different modules may have different ident
                // indices for the same strings, so we always use the root module's indices.
                switch (binop.op) {
                    .add => {
                        // Desugar `a + b` to `a.plus(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.plus, binop.lhs, binop.rhs, roc_ops);
                    },
                    .sub => {
                        // Desugar `a - b` to `a.minus(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.minus, binop.lhs, binop.rhs, roc_ops);
                    },
                    .mul => {
                        // Desugar `a * b` to `a.times(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.times, binop.lhs, binop.rhs, roc_ops);
                    },
                    .div => {
                        // Desugar `a / b` to `a.div_by(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.div_by, binop.lhs, binop.rhs, roc_ops);
                    },
                    .div_trunc => {
                        // Desugar `a // b` to `a.div_trunc_by(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.div_trunc_by, binop.lhs, binop.rhs, roc_ops);
                    },
                    .rem => {
                        // Desugar `a % b` to `a.rem_by(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.rem_by, binop.lhs, binop.rhs, roc_ops);
                    },
                    .lt => {
                        // Desugar `a < b` to `a.is_lt(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.is_lt, binop.lhs, binop.rhs, roc_ops);
                    },
                    .le => {
                        // Desugar `a <= b` to `a.is_lte(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.is_lte, binop.lhs, binop.rhs, roc_ops);
                    },
                    .gt => {
                        // Desugar `a > b` to `a.is_gt(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.is_gt, binop.lhs, binop.rhs, roc_ops);
                    },
                    .ge => {
                        // Desugar `a >= b` to `a.is_gte(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.is_gte, binop.lhs, binop.rhs, roc_ops);
                    },
                    .eq => {
                        // Desugar `a == b` to `a.is_eq(b)`
                        return try self.dispatchBinaryOpMethod(self.root_env.idents.is_eq, binop.lhs, binop.rhs, roc_ops);
                    },
                    .ne => {
                        // Desugar `a != b` to `!(a.is_eq(b))` - negate the result of is_eq
                        // This matches the type checker which desugars to `a.is_eq(b).not()`
                        const eq_result = try self.dispatchBinaryOpMethod(self.root_env.idents.is_eq, binop.lhs, binop.rhs, roc_ops);
                        defer eq_result.decref(&self.runtime_layout_store, roc_ops);
                        // Negate the boolean result
                        const is_eq = boolValueEquals(true, eq_result);
                        return try self.makeBoolValue(!is_eq);
                    },
                    .@"or" => {
                        var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                        defer lhs.decref(&self.runtime_layout_store, roc_ops);
                        if (boolValueEquals(true, lhs)) {
                            return try self.makeBoolValue(true);
                        }

                        var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                        defer rhs.decref(&self.runtime_layout_store, roc_ops);
                        return try self.makeBoolValue(boolValueEquals(true, rhs));
                    },
                    .@"and" => {
                        var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                        defer lhs.decref(&self.runtime_layout_store, roc_ops);
                        if (boolValueEquals(false, lhs)) {
                            return try self.makeBoolValue(false);
                        }

                        var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                        defer rhs.decref(&self.runtime_layout_store, roc_ops);
                        return try self.makeBoolValue(boolValueEquals(true, rhs));
                    },
                }
            },
            .e_if => |if_expr| {
                const branches = self.env.store.sliceIfBranches(if_expr.branches);
                // Evaluate branches in order; pick first true condition
                var i: usize = 0;
                while (i < branches.len) : (i += 1) {
                    const br = self.env.store.getIfBranch(branches[i]);
                    const cond_val = try self.evalExprMinimal(br.cond, roc_ops, null);
                    const cond_is_true = boolValueEquals(true, cond_val);
                    if (cond_is_true) {
                        return try self.evalExprMinimal(br.body, roc_ops, null);
                    }
                }
                // No condition matched; evaluate final else
                return try self.evalExprMinimal(if_expr.final_else, roc_ops, null);
            },
            .e_str => |str_expr| {
                const segments = self.env.store.sliceExpr(str_expr.span);
                if (segments.len == 0) {
                    const value = try self.pushStr("");
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                    roc_str_ptr.* = RocStr.empty();
                    return value;
                }

                var segment_strings = std.array_list.AlignedManaged(RocStr, null).init(self.allocator);
                defer {
                    for (segment_strings.items) |segment_str| {
                        segment_str.decref(roc_ops);
                    }
                    segment_strings.deinit();
                }

                var total_len: usize = 0;
                for (segments) |seg_idx| {
                    const seg_expr = self.env.store.getExpr(seg_idx);
                    if (seg_expr == .e_str_segment) {
                        const content = self.env.getString(seg_expr.e_str_segment.literal);
                        var literal_str = RocStr.fromSlice(content, roc_ops);
                        total_len += literal_str.asSlice().len;
                        try segment_strings.append(literal_str);
                        continue;
                    }

                    const seg_ct_var = can.ModuleEnv.varFrom(seg_idx);
                    const seg_rt_var = try self.translateTypeVar(self.env, seg_ct_var);
                    const seg_value = try self.evalExprMinimal(seg_idx, roc_ops, seg_rt_var);
                    const segment_str = try self.stackValueToRocStr(seg_value, seg_rt_var, roc_ops);
                    seg_value.decref(&self.runtime_layout_store, roc_ops);
                    total_len += segment_str.asSlice().len;
                    try segment_strings.append(segment_str);
                }

                const result_str: RocStr = if (total_len == 0)
                    RocStr.empty()
                else blk: {
                    const buffer = try self.allocator.alloc(u8, total_len);
                    defer self.allocator.free(buffer);
                    var offset: usize = 0;
                    for (segment_strings.items) |segment_str| {
                        const slice = segment_str.asSlice();
                        std.mem.copyForwards(u8, buffer[offset .. offset + slice.len], slice);
                        offset += slice.len;
                    }
                    break :blk RocStr.fromSlice(buffer, roc_ops);
                };

                const value = try self.pushStr("");
                const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str_ptr.* = result_str;
                return value;
            },
            .e_str_segment => |seg| {
                const content = self.env.getString(seg.literal);
                const value = try self.pushStr(content);
                const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str.* = RocStr.fromSlice(content, roc_ops);
                return value;
            },
            .e_frac_f32 => |lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *f32 = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = lit.value;
                }
                return value;
            },
            .e_frac_f64 => |lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *f64 = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = lit.value;
                }
                return value;
            },
            .e_dec => |dec_lit| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
                    typed_ptr.* = dec_lit.value;
                }
                return value;
            },
            .e_dec_small => |small| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(layout_val, 0);
                if (value.ptr) |ptr| {
                    const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
                    const scale_factor = std.math.pow(i128, 10, RocDec.decimal_places - small.value.denominator_power_of_ten);
                    const scaled = @as(i128, small.value.numerator) * scale_factor;
                    typed_ptr.* = RocDec{ .num = scaled };
                }
                return value;
            },
            .e_tuple => |tup| {
                // Evaluate all elements first to drive runtime unification
                const elems = self.env.store.sliceExpr(tup.elems);
                var values = try std.array_list.AlignedManaged(StackValue, null).initCapacity(self.allocator, elems.len);
                defer values.deinit();
                for (elems) |e_idx| {
                    const v = try self.evalExprMinimal(e_idx, roc_ops, null);
                    try values.append(v);
                }

                // Compute tuple layout from concrete element value layouts
                var elem_layouts = try self.allocator.alloc(Layout, values.items.len);
                defer self.allocator.free(elem_layouts);
                for (values.items, 0..) |v, ii| elem_layouts[ii] = v.layout;
                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                var dest = try self.pushRaw(tuple_layout, 0);
                var accessor = try dest.asTuple(&self.runtime_layout_store);

                if (values.items.len != accessor.getElementCount()) return error.TypeMismatch;
                var i: usize = 0;
                while (i < values.items.len) : (i += 1) {
                    // Pass the original index - setElement->getElement will convert to sorted internally
                    try accessor.setElement(i, values.items[i], roc_ops);
                }
                return dest;
            },
            .e_list => |list_expr| {
                const elem_indices = self.env.store.sliceExpr(list_expr.elems);
                const list_rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };

                // Get the first element's variables, which is representative of all the element vars
                const elems = self.env.store.sliceExpr(list_expr.elems);
                std.debug.assert(elems.len > 0);
                const first_elem_var: types.Var = @enumFromInt(@intFromEnum(elems[0]));

                const elem_rt_var = try self.translateTypeVar(self.env, first_elem_var);

                var values = try std.array_list.AlignedManaged(StackValue, null).initCapacity(self.allocator, elem_indices.len);
                defer {
                    // Decref temporary element values after they've been copied into the list.
                    // copyToPtr increfs refcounted values, so we need to release the temporary references.
                    for (values.items) |val| {
                        val.decref(&self.runtime_layout_store, roc_ops);
                    }
                    values.deinit();
                }

                for (elem_indices) |elem_idx| {
                    const val = try self.evalExprMinimal(elem_idx, roc_ops, elem_rt_var);
                    try values.append(val);
                }

                // Handle empty list case first
                if (values.items.len == 0) {
                    const list_layout = try self.getRuntimeLayout(list_rt_var);
                    const dest = try self.pushRaw(list_layout, 0);
                    if (dest.ptr == null) return dest;
                    const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));
                    header.* = RocList.empty();
                    return dest;
                }

                // Use the actual layout from the first evaluated element, not the type-variable-derived layout.
                // This is critical because type variables may be flex vars that get defaulted to Dec,
                // but the actual elements (e.g., strings) have their concrete layout from evaluation.
                const actual_elem_layout = values.items[0].layout;

                // Create the list layout with the correct element layout
                const correct_elem_idx = try self.runtime_layout_store.insertLayout(actual_elem_layout);
                const actual_list_layout = Layout{ .tag = .list, .data = .{ .list = correct_elem_idx } };

                const dest = try self.pushRaw(actual_list_layout, 0);
                if (dest.ptr == null) return dest;
                const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));
                const elem_alignment = actual_elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);
                const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(actual_elem_layout));
                const elements_refcounted = actual_elem_layout.isRefcounted();

                var runtime_list = RocList.allocateExact(
                    elem_alignment_u32,
                    values.items.len,
                    elem_size,
                    elements_refcounted,
                    roc_ops,
                );

                if (elem_size > 0) {
                    if (runtime_list.bytes) |buffer| {
                        var i: usize = 0;
                        while (i < values.items.len) : (i += 1) {
                            const dest_ptr = buffer + i * elem_size;
                            try values.items[i].copyToPtr(&self.runtime_layout_store, dest_ptr, roc_ops);
                        }
                    }
                }

                markListElementCount(&runtime_list, elements_refcounted);
                header.* = runtime_list;
                return dest;
            },
            .e_record => |rec| {
                // Allocate record and fill fields
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);

                var union_names = std.array_list.AlignedManaged(base_pkg.Ident.Idx, null).init(self.allocator);
                defer union_names.deinit();
                var union_layouts = std.array_list.AlignedManaged(layout.Layout, null).init(self.allocator);
                defer union_layouts.deinit();
                var union_indices = std.AutoHashMap(u32, usize).init(self.allocator);
                defer union_indices.deinit();

                var field_values = std.array_list.AlignedManaged(StackValue, null).init(self.allocator);
                defer {
                    for (field_values.items) |val| {
                        val.decref(&self.runtime_layout_store, roc_ops);
                    }
                    field_values.deinit();
                }

                const upsert = struct {
                    fn go(
                        names: *std.array_list.AlignedManaged(base_pkg.Ident.Idx, null),
                        layouts: *std.array_list.AlignedManaged(layout.Layout, null),
                        indices: *std.AutoHashMap(u32, usize),
                        name: base_pkg.Ident.Idx,
                        layout_value: layout.Layout,
                    ) !void {
                        const key: u32 = @bitCast(name);
                        if (indices.get(key)) |idx_ptr| {
                            layouts.items[idx_ptr] = layout_value;
                            names.items[idx_ptr] = name;
                        } else {
                            try layouts.append(layout_value);
                            try names.append(name);
                            try indices.put(key, layouts.items.len - 1);
                        }
                    }
                }.go;

                var base_accessor_opt: ?StackValue.RecordAccessor = null;

                if (rec.ext) |ext_idx| {
                    const ext_ct_var = can.ModuleEnv.varFrom(ext_idx);
                    const ext_rt_var = try self.translateTypeVar(self.env, ext_ct_var);
                    var base_value = try self.evalExprMinimal(ext_idx, roc_ops, ext_rt_var);
                    if (base_value.layout.tag != .record) {
                        base_value.decref(&self.runtime_layout_store, roc_ops);
                        return error.TypeMismatch;
                    }
                    defer base_value.decref(&self.runtime_layout_store, roc_ops);
                    var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                    base_accessor_opt = base_accessor;

                    var idx: usize = 0;
                    while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                        const info = base_accessor.field_layouts.get(idx);
                        const field_layout = self.runtime_layout_store.getLayout(info.layout);
                        try upsert(&union_names, &union_layouts, &union_indices, info.name, field_layout);
                    }
                }

                const fields = self.env.store.sliceRecordFields(rec.fields);
                try field_values.ensureTotalCapacity(fields.len);
                var field_list_index: usize = 0;
                while (field_list_index < fields.len) : (field_list_index += 1) {
                    const field_idx_val = fields[field_list_index];
                    const f = self.env.store.getRecordField(field_idx_val);
                    const field_ct_var = can.ModuleEnv.varFrom(f.value);
                    const field_rt_var = try self.translateTypeVar(self.env, field_ct_var);
                    const val = try self.evalExprMinimal(f.value, roc_ops, field_rt_var);
                    try field_values.append(val);
                    const field_layout = val.layout;
                    try upsert(&union_names, &union_layouts, &union_indices, f.name, field_layout);
                }
                const record_layout_idx = try self.runtime_layout_store.putRecord(self.env, union_layouts.items, union_names.items);
                const rec_layout = self.runtime_layout_store.getLayout(record_layout_idx);

                const resolved_rt = self.runtime_types.resolveVar(rt_var);
                const root_idx: usize = @intFromEnum(resolved_rt.var_);
                try self.ensureVarLayoutCapacity(root_idx + 1);
                self.var_to_layout_slot.items[root_idx] = @intFromEnum(record_layout_idx) + 1;

                var dest = try self.pushRaw(rec_layout, 0);
                var accessor = try dest.asRecord(&self.runtime_layout_store);

                if (base_accessor_opt) |base_accessor| {
                    var idx: usize = 0;
                    while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                        const info = base_accessor.field_layouts.get(idx);
                        const dest_field_idx = accessor.findFieldIndex(info.name) orelse return error.TypeMismatch;
                        const base_field_value = try base_accessor.getFieldByIndex(idx);
                        try accessor.setFieldByIndex(dest_field_idx, base_field_value, roc_ops);
                    }
                }

                for (fields, 0..) |field_idx_enum, explicit_index| {
                    const f = self.env.store.getRecordField(field_idx_enum);
                    const dest_field_idx = accessor.findFieldIndex(f.name) orelse return error.TypeMismatch;
                    const val = field_values.items[explicit_index];

                    if (base_accessor_opt) |base_accessor| {
                        if (base_accessor.findFieldIndex(f.name) != null) {
                            const existing = try accessor.getFieldByIndex(dest_field_idx);
                            existing.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    try accessor.setFieldByIndex(dest_field_idx, val, roc_ops);
                }

                return dest;
            },
            .e_empty_record => {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const rec_layout = try self.getRuntimeLayout(rt_var);
                return try self.pushRaw(rec_layout, 0);
            },
            .e_empty_list => {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const derived_layout = try self.getRuntimeLayout(rt_var);

                // Ensure we have a proper list layout even if the type variable defaulted to Dec.
                // For empty lists, if the layout isn't already a list, create one with a default element layout.
                const list_layout = if (derived_layout.tag == .list or derived_layout.tag == .list_of_zst)
                    derived_layout
                else blk: {
                    // Default to list of Dec for empty lists when type can't be determined
                    const default_elem_layout = Layout.frac(types.Frac.Precision.dec);
                    const elem_layout_idx = try self.runtime_layout_store.insertLayout(default_elem_layout);
                    break :blk Layout{ .tag = .list, .data = .{ .list = elem_layout_idx } };
                };

                const dest = try self.pushRaw(list_layout, 0);
                if (dest.ptr) |ptr| {
                    const header: *RocList = @ptrCast(@alignCast(ptr));
                    header.* = RocList.empty();
                }
                return dest;
            },
            // no zero-argument tag handling in minimal evaluator
            .e_nominal => |nom| {
                // Evaluate backing expression using minimal evaluator
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                // Check if this is Bool by comparing against the dynamic bool_stmt
                const backing_rt_var = if (nom.nominal_type_decl == self.builtins.bool_stmt)
                    try self.getCanonicalBoolRuntimeVar()
                else switch (nominal_resolved.desc.content) {
                    .structure => |st| switch (st) {
                        .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                        else => nominal_rt_var,
                    },
                    else => nominal_rt_var,
                };
                return try self.evalExprMinimal(nom.backing_expr, roc_ops, backing_rt_var);
            },
            .e_nominal_external => |nom| {
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                    const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                    const backing_rt_var = switch (nominal_resolved.desc.content) {
                        .structure => |st| switch (st) {
                            .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                            else => nominal_rt_var,
                        },
                        else => nominal_rt_var,
                    };
                    break :blk backing_rt_var;
                };
                return try self.evalExprMinimal(nom.backing_expr, roc_ops, rt_var);
            },
            .e_zero_argument_tag => |zero| {
                // Construct a tag union value with no payload
                // Determine discriminant index by consulting the runtime tag union type
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                // Use resolveBaseVar to unwrap nominal types (like Bool := [False, True])
                // to get to the underlying tag union
                const resolved = self.resolveBaseVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) {
                    self.triggerCrash("e_zero_argument_tag: expected tag_union structure type", false, roc_ops);
                    return error.Crash;
                }
                const tu = resolved.desc.content.structure.tag_union;
                const tags = self.runtime_types.getTagsSlice(tu.tags);
                // Find tag index by translating the source ident to the runtime store and comparing indices
                const tag_index = try self.findTagIndexByIdent(self.env, zero.name, tags) orelse {
                    const name_text = self.env.getIdent(zero.name);
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                };
                const layout_val = try self.getRuntimeLayout(rt_var);
                // If layout is scalar (int), write discriminant directly
                if (layout_val.tag == .scalar) {
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        try out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        out.rt_var = rt_var; // Set rt_var for proper rendering
                        return out;
                    }
                    self.triggerCrash("e_zero_argument_tag: scalar layout is not int", false, roc_ops);
                    return error.Crash;
                } else if (layout_val.tag == .record) {
                    // Record { tag: Discriminant, payload: ZST }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                        self.triggerCrash("DEBUG: e_zero_argument_tag tag field not found", false, roc_ops);
                        return error.Crash;
                    };
                    const tag_field = try acc.getFieldByIndex(tag_idx);
                    // write tag as int
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        try tmp.setInt(@intCast(tag_index));
                    } else {
                        self.triggerCrash("e_zero_argument_tag: record tag field is not scalar int", false, roc_ops);
                        return error.Crash;
                    }
                    dest.rt_var = rt_var; // Set rt_var for proper rendering
                    return dest;
                } else if (layout_val.tag == .tuple) {
                    // Tuple (payload, tag) - tag unions are now represented as tuples
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asTuple(&self.runtime_layout_store);
                    // Element 1 is the tag discriminant - getElement takes original index directly
                    const tag_field = try acc.getElement(1);
                    // write tag as int
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        try tmp.setInt(@intCast(tag_index));
                    } else {
                        self.triggerCrash("e_zero_argument_tag: tuple tag field is not scalar int", false, roc_ops);
                        return error.Crash;
                    }
                    dest.rt_var = rt_var; // Set rt_var for proper rendering
                    return dest;
                }
                self.triggerCrash("e_zero_argument_tag: unexpected layout type", false, roc_ops);
                return error.Crash;
            },
            .e_tag => |tag| {
                // Construct a tag union value with payloads
                var rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                // Unwrap nominal types and aliases to get the base tag union
                var resolved = self.resolveBaseVar(rt_var);
                // If the type is still flex and this is a True/False tag, use Bool
                if (resolved.desc.content == .flex) {
                    if (tag.name == self.env.idents.true_tag or tag.name == self.env.idents.false_tag) {
                        rt_var = try self.getCanonicalBoolRuntimeVar();
                        resolved = self.resolveBaseVar(rt_var);
                    }
                }
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) {
                    self.triggerCrash("e_tag: expected tag_union structure type", false, roc_ops);
                    return error.Crash;
                }
                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(rt_var, &tag_list);
                // Find tag index by translating the source ident to the runtime store and comparing indices
                const tag_index = try self.findTagIndexByIdentInList(self.env, tag.name, tag_list.items) orelse {
                    const name_text = self.env.getIdent(tag.name);
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                };

                const layout_val = try self.getRuntimeLayout(rt_var);

                if (layout_val.tag == .scalar) {
                    // No payload union
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        try out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        out.rt_var = rt_var; // Set rt_var for proper rendering
                        return out;
                    }
                    self.triggerCrash("e_tag: scalar layout is not int", false, roc_ops);
                    return error.Crash;
                } else if (layout_val.tag == .record) {
                    // Has payload: record { tag, payload }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                        self.triggerCrash("DEBUG: e_tag tag field not found", false, roc_ops);
                        return error.Crash;
                    };
                    const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse {
                        self.triggerCrash("DEBUG: e_tag payload field not found", false, roc_ops);
                        return error.Crash;
                    };
                    // write tag discriminant
                    const tag_field = try acc.getFieldByIndex(tag_field_idx);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        try tmp.setInt(@intCast(tag_index));
                    } else {
                        self.triggerCrash("e_tag: record tag field is not scalar int", false, roc_ops);
                        return error.Crash;
                    }

                    const args_exprs = self.env.store.sliceExpr(tag.args);
                    const arg_vars_range = tag_list.items[tag_index].args;
                    const arg_rt_vars = self.runtime_types.sliceVars(arg_vars_range);
                    if (args_exprs.len != arg_rt_vars.len) return error.TypeMismatch;
                    const payload_field = try acc.getFieldByIndex(payload_field_idx);

                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    if (args_exprs.len == 0) {
                        dest.rt_var = rt_var;
                        return dest;
                    } else if (args_exprs.len == 1) {
                        const arg_rt_var = arg_rt_vars[0];
                        const arg_val = try self.evalExprMinimal(args_exprs[0], roc_ops, arg_rt_var);
                        defer arg_val.decref(&self.runtime_layout_store, roc_ops);
                        if (payload_field.ptr) |payload_ptr| {
                            try arg_val.copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                        }
                        dest.rt_var = rt_var;
                        return dest;
                    } else {
                        const arg_count = args_exprs.len;
                        var elem_layouts = try self.allocator.alloc(Layout, arg_count);
                        defer self.allocator.free(elem_layouts);
                        var elem_values = try self.allocator.alloc(StackValue, arg_count);
                        defer {
                            for (elem_values[0..arg_count]) |val| {
                                val.decref(&self.runtime_layout_store, roc_ops);
                            }
                            self.allocator.free(elem_values);
                        }

                        var j: usize = 0;
                        while (j < arg_count) : (j += 1) {
                            const arg_rt_var = arg_rt_vars[j];
                            const val = try self.evalExprMinimal(args_exprs[j], roc_ops, arg_rt_var);
                            elem_values[j] = val;
                            // Use actual value layout, not type system layout, to avoid mismatch
                            elem_layouts[j] = val.layout;
                        }

                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);

                        if (payload_field.ptr) |payload_ptr| {
                            var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                            var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                            j = 0;
                            while (j < elem_values.len) : (j += 1) {
                                try tup_acc.setElement(j, elem_values[j], roc_ops);
                            }
                        }

                        dest.rt_var = rt_var;
                        return dest;
                    }
                } else if (layout_val.tag == .tuple) {
                    // Tuple (payload, tag) - tag unions now represented as tuples
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asTuple(&self.runtime_layout_store);

                    // Element 0 is payload, Element 1 is tag discriminant
                    // getElement takes original index directly - it does the mapping internally

                    // Write tag discriminant (element 1)
                    const tag_field = try acc.getElement(1);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        try tmp.setInt(@intCast(tag_index));
                    } else {
                        self.triggerCrash("e_tag: tuple tag field is not scalar int", false, roc_ops);
                        return error.Crash;
                    }

                    const args_exprs = self.env.store.sliceExpr(tag.args);
                    const arg_vars_range = tag_list.items[tag_index].args;
                    const arg_rt_vars = self.runtime_types.sliceVars(arg_vars_range);
                    if (args_exprs.len != arg_rt_vars.len) return error.TypeMismatch;
                    const payload_field = try acc.getElement(0);

                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    if (args_exprs.len == 0) {
                        dest.rt_var = rt_var;
                        return dest;
                    } else if (args_exprs.len == 1) {
                        const arg_rt_var = arg_rt_vars[0];
                        const arg_val = try self.evalExprMinimal(args_exprs[0], roc_ops, arg_rt_var);
                        defer arg_val.decref(&self.runtime_layout_store, roc_ops);

                        // The tuple layout may be wrong (too small) for the actual argument.
                        // Create a properly-sized result using the argument's actual layout.
                        const arg_size = self.runtime_layout_store.layoutSize(arg_val.layout);
                        const payload_size = self.runtime_layout_store.layoutSize(payload_field.layout);

                        // Check if layout differs (size or list element type mismatch)
                        // For lists, the size is the same but element layout may differ
                        const layouts_differ = arg_size > payload_size or !layoutsEqual(arg_val.layout, payload_field.layout);

                        if (layouts_differ) {
                            // The tuple layout differs - create a new properly-typed tuple
                            // with (payload, tag) elements using the actual argument layout
                            var elem_layouts_fixed = [2]Layout{ arg_val.layout, tag_field.layout };
                            const proper_tuple_idx = try self.runtime_layout_store.putTuple(&elem_layouts_fixed);
                            const proper_tuple_layout = self.runtime_layout_store.getLayout(proper_tuple_idx);
                            var proper_dest = try self.pushRaw(proper_tuple_layout, 0);
                            var proper_acc = try proper_dest.asTuple(&self.runtime_layout_store);

                            // Write tag discriminant to the proper location (element 1)
                            const proper_tag_field = try proper_acc.getElement(1);
                            if (proper_tag_field.layout.tag == .scalar and proper_tag_field.layout.data.scalar.tag == .int) {
                                var tmp = proper_tag_field;
                                tmp.is_initialized = false;
                                try tmp.setInt(@intCast(tag_index));
                            }

                            // Write payload to the proper location (element 0)
                            const proper_payload_field = try proper_acc.getElement(0);
                            if (proper_payload_field.ptr) |payload_ptr| {
                                try arg_val.copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                            }

                            proper_dest.rt_var = rt_var;
                            return proper_dest;
                        }

                        // Normal case: sizes and layouts match, use original destination
                        if (payload_field.ptr) |payload_ptr| {
                            try arg_val.copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                        }
                        dest.rt_var = rt_var;
                        return dest;
                    } else {
                        const arg_count = args_exprs.len;
                        var elem_layouts = try self.allocator.alloc(Layout, arg_count);
                        defer self.allocator.free(elem_layouts);
                        var elem_values = try self.allocator.alloc(StackValue, arg_count);
                        defer {
                            for (elem_values[0..arg_count]) |val| {
                                val.decref(&self.runtime_layout_store, roc_ops);
                            }
                            self.allocator.free(elem_values);
                        }

                        var j: usize = 0;
                        while (j < arg_count) : (j += 1) {
                            const arg_rt_var = arg_rt_vars[j];
                            const val = try self.evalExprMinimal(args_exprs[j], roc_ops, arg_rt_var);
                            elem_values[j] = val;
                            // Use actual value layout, not type system layout, to avoid mismatch
                            // (e.g., List(Dec) actual vs List(generic_num) from type system)
                            elem_layouts[j] = val.layout;
                        }

                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);

                        if (payload_field.ptr) |payload_ptr| {
                            var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                            var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                            j = 0;
                            while (j < elem_values.len) : (j += 1) {
                                try tup_acc.setElement(j, elem_values[j], roc_ops);
                            }
                        }

                        // Check if the payload layout differs from what dest expects
                        // If so, create a new outer tuple with the correct layout
                        const layouts_differ = !layoutsEqual(tuple_layout, payload_field.layout);
                        if (layouts_differ) {
                            // Create properly-typed outer tuple (payload, tag)
                            var outer_elem_layouts = [2]Layout{ tuple_layout, tag_field.layout };
                            const proper_outer_idx = try self.runtime_layout_store.putTuple(&outer_elem_layouts);
                            const proper_outer_layout = self.runtime_layout_store.getLayout(proper_outer_idx);
                            var proper_dest = try self.pushRaw(proper_outer_layout, 0);
                            var proper_acc = try proper_dest.asTuple(&self.runtime_layout_store);

                            // Write tag discriminant
                            const proper_tag_field = try proper_acc.getElement(1);
                            if (proper_tag_field.layout.tag == .scalar and proper_tag_field.layout.data.scalar.tag == .int) {
                                var tmp = proper_tag_field;
                                tmp.is_initialized = false;
                                try tmp.setInt(@intCast(tag_index));
                            }

                            // Copy payload tuple data
                            const proper_payload_field = try proper_acc.getElement(0);
                            if (proper_payload_field.ptr) |proper_payload_ptr| {
                                // Copy the tuple data we already wrote
                                const payload_size = self.runtime_layout_store.layoutSize(tuple_layout);
                                const src_bytes = @as([*]const u8, @ptrCast(payload_field.ptr.?))[0..payload_size];
                                const dst_bytes = @as([*]u8, @ptrCast(proper_payload_ptr))[0..payload_size];
                                @memcpy(dst_bytes, src_bytes);
                            }

                            proper_dest.rt_var = rt_var;
                            return proper_dest;
                        }

                        dest.rt_var = rt_var;
                        return dest;
                    }
                }
                self.triggerCrash("e_tag: unexpected layout type", false, roc_ops);
                return error.Crash;
            },
            .e_match => |m| {
                // Evaluate scrutinee once and protect from stack corruption
                // Use pushCopy to allocate a new stack location for the scrutinee header,
                // preventing it from being corrupted by pattern match bindings
                const scrutinee_temp = try self.evalExprMinimal(m.cond, roc_ops, null);
                const scrutinee = try self.pushCopy(scrutinee_temp, roc_ops);
                // Decref the original since pushCopy increfd the underlying data.
                // The copy now owns the reference, and we're transferring ownership.
                scrutinee_temp.decref(&self.runtime_layout_store, roc_ops);
                defer scrutinee.decref(&self.runtime_layout_store, roc_ops);

                const scrutinee_ct_var = can.ModuleEnv.varFrom(m.cond);
                const scrutinee_rt_var = try self.translateTypeVar(self.env, scrutinee_ct_var);
                const match_result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const match_result_rt_var = try self.translateTypeVar(self.env, match_result_ct_var);
                // Iterate branches and find first matching pattern set
                const branches = self.env.store.matchBranchSlice(m.branches);
                for (branches) |br_idx| {
                    const br = self.env.store.getMatchBranch(br_idx);
                    const patterns = self.env.store.sliceMatchBranchPatterns(br.patterns);
                    var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                    defer {
                        self.trimBindingList(&temp_binds, 0, roc_ops);
                        temp_binds.deinit();
                    }

                    for (patterns) |bp_idx| {
                        self.trimBindingList(&temp_binds, 0, roc_ops);
                        if (!try self.patternMatchesBind(self.env.store.getMatchBranchPattern(bp_idx).pattern, scrutinee, scrutinee_rt_var, roc_ops, &temp_binds, @enumFromInt(0))) {
                            self.trimBindingList(&temp_binds, 0, roc_ops);
                            continue;
                        }

                        const start_len = self.bindings.items.len;
                        try self.bindings.appendSlice(temp_binds.items);
                        temp_binds.items.len = 0;

                        var guard_pass = true;
                        if (br.guard) |guard_idx| {
                            const guard_ct_var = can.ModuleEnv.varFrom(guard_idx);
                            const guard_rt_var = try self.translateTypeVar(self.env, guard_ct_var);
                            const guard_val = try self.evalExprMinimal(guard_idx, roc_ops, guard_rt_var);
                            defer guard_val.decref(&self.runtime_layout_store, roc_ops);
                            guard_pass = boolValueEquals(true, guard_val);
                        }

                        if (!guard_pass) {
                            self.trimBindingList(&self.bindings, start_len, roc_ops);
                            continue;
                        }

                        const result = try self.evalExprMinimal(br.value, roc_ops, match_result_rt_var);
                        self.trimBindingList(&self.bindings, start_len, roc_ops);
                        return result;
                    }
                }
                self.triggerCrash("non-exhaustive match", false, roc_ops);
                return error.Crash;
            },
            .e_crash => |crash_expr| {
                const msg = self.env.getString(crash_expr.msg);
                self.triggerCrash(msg, false, roc_ops);
                return error.Crash;
            },
            .e_expect => |expect_expr| {
                const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
                const cond_val = try self.evalExprMinimal(expect_expr.body, roc_ops, bool_rt_var);
                const succeeded = boolValueEquals(true, cond_val);
                if (succeeded) {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const layout_val = try self.getRuntimeLayout(rt_var);
                    return try self.pushRaw(layout_val, 0);
                }
                self.handleExpectFailure(expect_expr.body, roc_ops);
                return error.Crash;
            },
            .e_dbg => |dbg_expr| {
                // Evaluate and print the inner expression
                const inner_ct_var = can.ModuleEnv.varFrom(dbg_expr.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                const value = try self.evalExprMinimal(dbg_expr.expr, roc_ops, inner_rt_var);
                defer value.decref(&self.runtime_layout_store, roc_ops);
                const rendered = try self.renderValueRocWithType(value, inner_rt_var);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                // dbg returns {} (empty record) - use same pattern as e_expect
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const layout_val = try self.getRuntimeLayout(rt_var);
                return try self.pushRaw(layout_val, 0);
            },
            // no tag handling in minimal evaluator
            .e_lambda => |lam| {
                // Build a closure value with empty captures using the runtime layout for the lambda's type
                // Use provided expected_rt_var if available (for cross-module instantiated functions),
                // otherwise translate from compile-time types
                const rt_var = if (expected_rt_var) |provided_var|
                    provided_var
                else blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const closure_layout = try self.getRuntimeLayout(rt_var);
                // Expect a closure layout from type-to-layout translation
                if (closure_layout.tag != .closure) {
                    self.triggerCrash("e_lambda: expected closure layout", false, roc_ops);
                    return error.Crash;
                }
                const value = try self.pushRaw(closure_layout, 0);
                self.registerDefValue(expr_idx, value);
                // Initialize the closure header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // no captures in minimal path
                        .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                        .lambda_expr_idx = expr_idx,
                        .source_env = self.env,
                    };
                }
                return value;
            },
            .e_anno_only => |_| {
                // This represents a value that only has a type annotation, no implementation.
                // Crash immediately when accessed or called, regardless of type.
                self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
                return error.Crash;
            },
            .e_return => |ret| {
                // Early return expression - evaluate inner expr, store value, signal return
                // Get the expected type from the INNER expression's type variable,
                // since e_return's type may have been unified with {} or another type
                const inner_ct_var = can.ModuleEnv.varFrom(ret.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                const return_value = try self.evalExprMinimal(ret.expr, roc_ops, inner_rt_var);
                // Store the return value for the caller to consume at function boundary
                self.early_return_value = return_value;
                return error.EarlyReturn;
            },
            .e_low_level_lambda => |lam| {
                // Build a closure for a low-level builtin function
                // Use provided expected_rt_var if available (for cross-module instantiated functions),
                // otherwise translate from compile-time types
                const rt_var = if (expected_rt_var) |provided_var|
                    provided_var
                else blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                const closure_layout = try self.getRuntimeLayout(rt_var);
                const value = try self.pushRaw(closure_layout, 0);
                self.registerDefValue(expr_idx, value);

                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                        .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                        .lambda_expr_idx = expr_idx,
                        .source_env = self.env,
                    };
                }
                return value;
            },
            .e_hosted_lambda => |hosted| {
                // Build a closure for a hosted function that will dispatch to the host via RocOps
                // We MUST create a closure layout manually since the type might be flex/unknown

                // Manually create a closure layout instead of using getRuntimeLayout
                // because hosted functions might have flex types
                const closure_layout = Layout{
                    .tag = .closure,
                    .data = .{
                        .closure = .{
                            .captures_layout_idx = @enumFromInt(0), // No captures for hosted functions
                        },
                    },
                };
                const value = try self.pushRaw(closure_layout, 0);
                self.registerDefValue(expr_idx, value);

                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = hosted.body,
                        .params = hosted.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                        .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                        .lambda_expr_idx = expr_idx,
                        .source_env = self.env,
                    };
                }
                return value;
            },
            .e_closure => |cls| {
                // Build a closure value with concrete captures. The closure references a lambda.
                const lam_expr = self.env.store.getExpr(cls.lambda_idx);
                if (lam_expr != .e_lambda) {
                    self.triggerCrash("e_closure: lambda_idx does not point to e_lambda", false, roc_ops);
                    return error.Crash;
                }
                const lam = lam_expr.e_lambda;

                // Collect capture layouts and names from current bindings
                const caps = self.env.store.sliceCaptures(cls.captures);
                var field_layouts = try self.allocator.alloc(Layout, caps.len);
                defer self.allocator.free(field_layouts);
                var field_names = try self.allocator.alloc(@import("base").Ident.Idx, caps.len);
                defer self.allocator.free(field_names);

                // Helper: resolve a capture value (from local bindings, active closure captures, or top-level defs)
                const resolveCapture = struct {
                    fn go(self_interp: *Interpreter, cap: can.CIR.Expr.Capture, ops: *RocOps) ?StackValue {
                        // First try local bindings by pattern idx
                        var i: usize = self_interp.bindings.items.len;
                        while (i > 0) {
                            i -= 1;
                            const b = self_interp.bindings.items[i];
                            if (b.pattern_idx == cap.pattern_idx) return b.value;
                        }
                        // Next try ALL active closure captures in reverse order (innermost to outermost)
                        // This is critical for deeply nested lambdas where inner closures need to
                        // capture values from outer closures
                        if (self_interp.active_closures.items.len > 0) {
                            var closure_idx: usize = self_interp.active_closures.items.len;
                            while (closure_idx > 0) {
                                closure_idx -= 1;
                                const cls_val = self_interp.active_closures.items[closure_idx];
                                if (cls_val.layout.tag == .closure and cls_val.ptr != null) {
                                    const captures_layout = self_interp.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                                    const header_sz = @sizeOf(layout.Closure);
                                    const cap_align = captures_layout.alignment(self_interp.runtime_layout_store.targetUsize());
                                    const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                                    const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                                    var rec_acc = (rec_val.asRecord(&self_interp.runtime_layout_store)) catch continue;
                                    if (rec_acc.findFieldIndex(cap.name)) |fidx| {
                                        if (rec_acc.getFieldByIndex(fidx) catch null) |field_val| {
                                            return field_val;
                                        }
                                    }
                                }
                            }
                        }
                        // Finally try top-level defs by pattern idx
                        const all_defs = self_interp.env.store.sliceDefs(self_interp.env.all_defs);
                        for (all_defs) |def_idx| {
                            const def = self_interp.env.store.getDef(def_idx);
                            if (def.pattern == cap.pattern_idx) {
                                var k: usize = self_interp.def_stack.items.len;
                                while (k > 0) {
                                    k -= 1;
                                    const entry = self_interp.def_stack.items[k];
                                    if (entry.pattern_idx == cap.pattern_idx) {
                                        if (entry.value) |val| {
                                            return val;
                                        }
                                    }
                                }
                                // Found the def! Evaluate it to get the captured value
                                const new_entry = DefInProgress{
                                    .pattern_idx = def.pattern,
                                    .expr_idx = def.expr,
                                    .value = null,
                                };
                                self_interp.def_stack.append(new_entry) catch return null;
                                defer _ = self_interp.def_stack.pop();
                                return self_interp.evalMinimal(def.expr, ops) catch null;
                            }
                        }
                        return null;
                    }
                }.go;

                // First, resolve all capture values and collect their actual layouts.
                // This is critical because type variables may default to Dec, but the actual
                // captured values (e.g., strings) have their concrete layouts from evaluation.
                var capture_values = try self.allocator.alloc(StackValue, caps.len);
                defer self.allocator.free(capture_values);

                for (caps, 0..) |cap_idx, i| {
                    const cap = self.env.store.getCapture(cap_idx);
                    // Translate ident from current env to runtime layout store's env
                    // This is necessary for cross-module closures (e.g., builtin functions)
                    const name_text = self.env.getIdent(cap.name);
                    field_names[i] = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(name_text));

                    // Resolve the capture value first to get its actual layout
                    const cap_val = resolveCapture(self, cap, roc_ops) orelse {
                        self.triggerCrash("e_closure: failed to resolve capture value", false, roc_ops);
                        return error.Crash;
                    };
                    capture_values[i] = cap_val;
                    // Use the actual evaluated value's layout, not the type-variable-derived layout
                    field_layouts[i] = cap_val.layout;
                }

                const captures_layout_idx = try self.runtime_layout_store.putRecord(self.runtime_layout_store.env, field_layouts, field_names);
                const captures_layout = self.runtime_layout_store.getLayout(captures_layout_idx);
                const closure_layout = Layout.closure(captures_layout_idx);
                const value = try self.pushRaw(closure_layout, 0);
                self.registerDefValue(expr_idx, value);

                // Initialize header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // not used in minimal path
                        .captures_layout_idx = captures_layout_idx,
                        // Store e_closure expr_idx (not inner e_lambda) so has_real_captures check works in e_lookup_local
                        .lambda_expr_idx = expr_idx,
                        .source_env = self.env,
                    };
                    // Copy captures into record area following header (aligned)
                    const header_size = @sizeOf(layout.Closure);
                    const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                    const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
                    const base: [*]u8 = @ptrCast(@alignCast(ptr));
                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                    var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                    for (caps, 0..) |_, cap_i| {
                        const cap_val = capture_values[cap_i];
                        const translated_name = field_names[cap_i];
                        const idx_opt = accessor.findFieldIndex(translated_name) orelse {
                            self.triggerCrash("e_closure: capture field not found in record", false, roc_ops);
                            return error.Crash;
                        };
                        try accessor.setFieldByIndex(idx_opt, cap_val, roc_ops);
                    }
                }
                return value;
            },
            .e_call => |call| {
                const all = self.env.store.sliceExpr(call.args);
                const func_idx = call.func;
                const arg_indices = all[0..];

                // Check if the function is an anno-only lookup that will crash
                // If so, skip type translation and crash immediately
                const func_expr_for_anno_check = self.env.store.getExpr(func_idx);
                if (func_expr_for_anno_check == .e_lookup_local) {
                    const lookup = func_expr_for_anno_check.e_lookup_local;
                    const all_defs = self.env.store.sliceDefs(self.env.all_defs);
                    for (all_defs) |def_idx| {
                        const def = self.env.store.getDef(def_idx);
                        if (def.pattern == lookup.pattern_idx) {
                            const def_expr = self.env.store.getExpr(def.expr);
                            if (def_expr == .e_anno_only) {
                                // Calling an anno-only function - crash immediately
                                const msg = "This function has only a type annotation - no implementation was provided";
                                const crashed = RocCrashed{
                                    .utf8_bytes = @ptrCast(@constCast(msg.ptr)),
                                    .len = msg.len,
                                };
                                roc_ops.roc_crashed(&crashed, roc_ops.env);
                                return error.Crash;
                            }
                        }
                    }
                }

                // Runtime unification for call: constrain return type from arg types
                const func_expr = self.env.store.getExpr(func_idx);
                const func_ct_var = can.ModuleEnv.varFrom(func_idx);
                const func_rt_var_orig = try self.translateTypeVar(self.env, func_ct_var);

                // Only instantiate if we have an actual function type (not a flex variable)
                // This is needed for cross-module calls with rigid type parameters
                const func_rt_orig_resolved = self.runtime_types.resolveVar(func_rt_var_orig);
                const should_instantiate = func_rt_orig_resolved.desc.content == .structure and
                    (func_rt_orig_resolved.desc.content.structure == .fn_pure or
                        func_rt_orig_resolved.desc.content.structure == .fn_effectful or
                        func_rt_orig_resolved.desc.content.structure == .fn_unbound);

                var subst_map = std.AutoHashMap(types.Var, types.Var).init(self.allocator);
                defer subst_map.deinit();
                const func_rt_var = if (should_instantiate)
                    try self.instantiateType(func_rt_var_orig, &subst_map)
                else
                    func_rt_var_orig;

                // Save current rigid substitution context and merge in the new substitutions (only if we instantiated)
                // This will be used during function body evaluation
                const saved_subst = if (should_instantiate) try self.rigid_subst.clone() else null;
                defer {
                    if (saved_subst) |saved| {
                        // Restore the previous substitution context after the call
                        self.rigid_subst.deinit();
                        self.rigid_subst = saved;
                    }
                }

                if (should_instantiate) {
                    var subst_iter = subst_map.iterator();
                    while (subst_iter.next()) |entry| {
                        try self.rigid_subst.put(entry.key_ptr.*, entry.value_ptr.*);
                    }

                    // Clear the layout cache so layouts are recomputed with substitutions
                    @memset(self.var_to_layout_slot.items, 0);
                }

                var arg_rt_buf = try self.allocator.alloc(types.Var, arg_indices.len);
                defer self.allocator.free(arg_rt_buf);
                var i: usize = 0;
                while (i < arg_indices.len) : (i += 1) {
                    const arg_ct_var = can.ModuleEnv.varFrom(arg_indices[i]);
                    const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);

                    // Apply substitution if this argument is a rigid variable that was instantiated
                    if (should_instantiate) {
                        const arg_resolved = self.runtime_types.resolveVar(arg_rt_var);
                        if (arg_resolved.desc.content == .rigid) {
                            if (self.rigid_subst.get(arg_resolved.var_)) |substituted_arg| {
                                arg_rt_buf[i] = substituted_arg;
                            } else {
                                arg_rt_buf[i] = arg_rt_var;
                            }
                        } else {
                            arg_rt_buf[i] = arg_rt_var;
                        }
                    } else {
                        arg_rt_buf[i] = arg_rt_var;
                    }
                }

                // Check if this is an error expression that shouldn't be called
                // These should return TypeMismatch immediately
                if (func_expr == .e_runtime_error or func_expr == .e_anno_only or func_expr == .e_crash) {
                    return error.TypeMismatch;
                }

                // Prepare polymorphic call entry
                // For flex types this may return null if the function type isn't resolved yet
                const poly_entry: ?PolyEntry = self.prepareCallWithFuncVar(0, @intCast(@intFromEnum(func_idx)), func_rt_var, arg_rt_buf) catch |err| blk: {
                    // If we got TypeMismatch from prepareCallWithFuncVar, allow null
                    // The function value will be evaluated and closures will be handled
                    if (err == error.TypeMismatch) {
                        break :blk null;
                    }
                    break :blk null;
                };
                // Get call expression's return type for low-level builtins
                const call_ret_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const call_ret_rt_var = try self.translateTypeVar(self.env, call_ret_ct_var);

                // Unify this call expression's return var with the function's constrained return var
                // Only do this if we have a polymorphic call entry (concrete function type)
                if (poly_entry) |entry| {
                    _ = try unify.unifyWithConf(
                        self.env,
                        self.runtime_types,
                        &self.problems,
                        &self.snapshots,
                        &self.unify_scratch,
                        &self.unify_scratch.occurs_scratch,
                        unify.ModuleEnvLookup{
                            .interpreter_lookup_ctx = @ptrCast(&self.module_envs),
                            .interpreter_lookup_fn = interpreterLookupModuleEnv,
                        },
                        call_ret_rt_var,
                        entry.return_var,
                        unify.Conf{ .ctx = .anon, .constraint_origin_var = null },
                    );
                }

                // Pass the instantiated function type so cross-module generic functions work correctly
                const func_val = try self.evalExprMinimal(func_idx, roc_ops, func_rt_var);

                var arg_values = try self.allocator.alloc(StackValue, arg_indices.len);
                defer self.allocator.free(arg_values);
                var j: usize = 0;
                while (j < arg_indices.len) : (j += 1) {
                    arg_values[j] = try self.evalExprMinimal(arg_indices[j], roc_ops, if (arg_rt_buf.len == 0) null else arg_rt_buf[j]);
                }
                // Support calling closures produced by evaluating expressions (including nested calls)
                if (func_val.layout.tag == .closure) {
                    const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));

                    // Switch to the closure's source module for correct expression evaluation
                    const saved_env = self.env;
                    const saved_bindings_len = self.bindings.items.len;
                    self.env = @constCast(header.source_env);
                    defer {
                        self.env = saved_env;
                        self.bindings.shrinkRetainingCapacity(saved_bindings_len);
                    }

                    // Check if this is an annotation-only function (body points to e_anno_only)
                    const body_expr = self.env.store.getExpr(header.body_idx);
                    if (body_expr == .e_anno_only) {
                        self.triggerCrash("This function has no implementation. It is only a type annotation for now.", false, roc_ops);
                        return error.Crash;
                    }

                    // Check if this is a low-level lambda - if so, dispatch to builtin implementation
                    const lambda_expr = self.env.store.getExpr(header.lambda_expr_idx);
                    if (lambda_expr == .e_low_level_lambda) {
                        const low_level = lambda_expr.e_low_level_lambda;
                        const result = try self.callLowLevelBuiltin(low_level.op, arg_values, roc_ops, call_ret_rt_var);

                        // Decref args that aren't consumed by the builtin.
                        // list_concat consumes its input lists (handles refcounting internally),
                        // so we must not decref them again here to avoid double-free.
                        if (low_level.op != .list_concat) {
                            for (arg_values) |arg| {
                                arg.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }

                        return result;
                    }

                    // Check if this is a hosted lambda - if so, dispatch to host function via RocOps
                    if (lambda_expr == .e_hosted_lambda) {
                        const hosted = lambda_expr.e_hosted_lambda;
                        // Get the return type from the hosted function's type annotation
                        // The function type should be stored in the lambda expression's type variable
                        const hosted_lambda_ct_var = can.ModuleEnv.varFrom(header.lambda_expr_idx);
                        const hosted_lambda_rt_var = try self.translateTypeVar(self.env, hosted_lambda_ct_var);
                        const resolved_func = self.runtime_types.resolveVar(hosted_lambda_rt_var);

                        // Extract the return type from the function type
                        const ret_rt_var = if (resolved_func.desc.content.unwrapFunc()) |func| blk: {
                            // Function type has a return type
                            break :blk func.ret;
                        } else call_ret_rt_var;

                        const result = try self.callHostedFunction(hosted.index, arg_values, roc_ops, ret_rt_var);

                        // Decref all args
                        for (arg_values) |arg| {
                            arg.decref(&self.runtime_layout_store, roc_ops);
                        }

                        return result;
                    }

                    const params = self.env.store.slicePatterns(header.params);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    // Provide closure context for capture lookup during body eval
                    try self.active_closures.append(func_val);
                    defer _ = self.active_closures.pop();
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count], .expr_idx = @enumFromInt(0), .source_env = self.env });
                    }
                    defer {
                        var k = params.len;
                        while (k > 0) {
                            k -= 1;
                            if (self.bindings.pop()) |binding| {
                                binding.value.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }
                    }
                    // Evaluate body, handling early returns at function boundary
                    return self.evalExprMinimal(header.body_idx, roc_ops, call_ret_rt_var) catch |err| {
                        if (err == error.EarlyReturn) {
                            // Consume early return value as function result
                            const return_val = self.early_return_value orelse return error.Crash;
                            self.early_return_value = null;
                            return return_val;
                        }
                        return err;
                    };
                }

                // Fallback: direct lambda expression (legacy minimal path)
                // (func_expr was already declared above for external lookup handling)
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const params = self.env.store.slicePatterns(lambda.args);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count], .expr_idx = @enumFromInt(0), .source_env = self.env });
                    }
                    defer {
                        var k = params.len;
                        while (k > 0) {
                            k -= 1;
                            if (self.bindings.pop()) |binding| {
                                binding.value.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }
                    }
                    // Evaluate body, handling early returns at function boundary
                    return self.evalExprMinimal(lambda.body, roc_ops, call_ret_rt_var) catch |err| {
                        if (err == error.EarlyReturn) {
                            // Consume early return value as function result
                            const return_val = self.early_return_value orelse return error.Crash;
                            self.early_return_value = null;
                            return return_val;
                        }
                        return err;
                    };
                }

                self.triggerCrash("e_call: func is neither closure nor lambda", false, roc_ops);
                return error.Crash;
            },
            .e_dot_access => |dot_access| {
                const receiver_ct_var = can.ModuleEnv.varFrom(dot_access.receiver);

                const receiver_rt_var = try self.translateTypeVar(self.env, receiver_ct_var);
                var receiver_value = try self.evalExprMinimal(dot_access.receiver, roc_ops, receiver_rt_var);
                defer receiver_value.decref(&self.runtime_layout_store, roc_ops);

                const method_args = dot_access.args;

                // Field access vs method call
                if (method_args == null) {
                    // This is field access on a record, not a method call
                    if (receiver_value.layout.tag != .record) return error.TypeMismatch;
                    // Records can have zero-sized fields
                    const rec_data = self.runtime_layout_store.getRecordData(receiver_value.layout.data.record.idx);
                    if (rec_data.fields.count == 0) return error.TypeMismatch; // No fields to access
                    var accessor = try receiver_value.asRecord(&self.runtime_layout_store);
                    const field_idx = accessor.findFieldIndex(dot_access.field_name) orelse return error.TypeMismatch;
                    const field_value = try accessor.getFieldByIndex(field_idx);
                    return try self.pushCopy(field_value, roc_ops);
                }

                // This is a method call - resolve receiver type for dispatch
                const resolved_receiver = self.resolveBaseVar(receiver_rt_var);

                const arg_count = if (method_args) |span| span.span.len else 0;
                var arg_values: []StackValue = &.{};
                if (arg_count > 0) {
                    arg_values = try self.allocator.alloc(StackValue, arg_count);
                }
                defer if (arg_values.len > 0) self.allocator.free(arg_values);

                if (method_args) |span| {
                    // Use sliceExpr to properly get argument indices instead of computing them directly
                    const arg_indices = self.env.store.sliceExpr(span);
                    var i: usize = 0;
                    while (i < arg_values.len) : (i += 1) {
                        const arg_expr_idx = arg_indices[i];
                        const arg_ct_var = can.ModuleEnv.varFrom(arg_expr_idx);
                        const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);
                        arg_values[i] = try self.evalExprMinimal(arg_expr_idx, roc_ops, arg_rt_var);
                    }
                }

                const base_content = resolved_receiver.desc.content;
                if (base_content == .structure) {
                    switch (base_content.structure) {
                        .nominal_type => |nominal| {
                            // Check if this is Box using ident comparison
                            if (nominal.ident.ident_idx == self.env.idents.box) {
                                if (dot_access.field_name == self.env.idents.box_method) {
                                    if (arg_values.len != 1) return error.TypeMismatch;
                                    const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                    const result_layout = try self.getRuntimeLayout(result_rt_var);
                                    return try self.makeBoxValueFromLayout(result_layout, arg_values[0], roc_ops);
                                } else if (dot_access.field_name == self.env.idents.unbox_method) {
                                    if (arg_values.len != 1) return error.TypeMismatch;
                                    const box_value = arg_values[0];
                                    const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                    const result_layout = try self.getRuntimeLayout(result_rt_var);

                                    if (box_value.layout.tag == .box_of_zst) {
                                        var out = try self.pushRaw(result_layout, 0);
                                        out.is_initialized = true;
                                        return out;
                                    }

                                    if (box_value.layout.tag != .box) return error.TypeMismatch;

                                    const elem_layout = self.runtime_layout_store.getLayout(box_value.layout.data.box);
                                    const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                                    const data_ptr = box_value.boxDataPointer() orelse return error.NullStackPointer;
                                    var payload_ptr_any: ?*anyopaque = null;
                                    if (elem_size > 0) {
                                        payload_ptr_any = @as(*anyopaque, @ptrFromInt(@intFromPtr(data_ptr)));
                                    }

                                    const payload_value = StackValue{
                                        .layout = elem_layout,
                                        .ptr = payload_ptr_any,
                                        .is_initialized = true,
                                    };

                                    if (!std.meta.eql(elem_layout, result_layout)) {
                                        var out = try self.pushRaw(result_layout, 0);
                                        if (self.runtime_layout_store.layoutSize(result_layout) > 0 and out.ptr != null and payload_ptr_any != null) {
                                            try payload_value.copyToPtr(&self.runtime_layout_store, out.ptr.?, roc_ops);
                                        }
                                        out.is_initialized = true;
                                        return out;
                                    }

                                    return try self.pushCopy(payload_value, roc_ops);
                                }
                            }
                        },
                        else => {},
                    }
                }

                // Try static dispatch for nominal types with method constraints
                const method_ident = dot_access.field_name;

                // Find the nominal type's origin module from the receiver type
                var receiver_resolved = self.runtime_types.resolveVar(receiver_rt_var);

                // If the type is still a flex/rigid var, default to Dec
                // (Unsuffixed numeric literals default to Dec in Roc)
                if (receiver_resolved.desc.content == .flex or receiver_resolved.desc.content == .rigid) {
                    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                    const dec_var = try self.runtime_types.freshFromContent(dec_content);
                    receiver_resolved = self.runtime_types.resolveVar(dec_var);
                }

                const nominal_info = blk: {
                    switch (receiver_resolved.desc.content) {
                        .structure => |s| switch (s) {
                            .nominal_type => |nom| break :blk .{
                                .origin = nom.origin_module,
                                .ident = nom.ident.ident_idx,
                            },
                            else => return error.InvalidMethodReceiver,
                        },
                        else => return error.InvalidMethodReceiver,
                    }
                };

                // Resolve and evaluate the method function
                // method_ident comes from the CIR (self.env), not root_env
                const method_func = self.resolveMethodFunction(
                    nominal_info.origin,
                    nominal_info.ident,
                    method_ident,
                    roc_ops,
                ) catch |err| {
                    if (err == error.MethodLookupFailed) {
                        // Get type and method names for a helpful crash message
                        // nominal_info.ident is from runtime_layout_store.env (translated during translateTypeVar)
                        const layout_env = self.runtime_layout_store.env;
                        const type_name = import_mapping_mod.getDisplayName(
                            self.import_mapping,
                            layout_env.common.getIdentStore(),
                            nominal_info.ident,
                        );
                        // method_ident is from self.env (current CIR module)
                        const method_name = self.env.getIdent(dot_access.field_name);
                        const crash_msg = std.fmt.allocPrint(self.allocator, "{s} does not implement {s}", .{ type_name, method_name }) catch {
                            self.triggerCrash("Method not found", false, roc_ops);
                            return error.Crash;
                        };
                        self.triggerCrash(crash_msg, true, roc_ops);
                        return error.Crash;
                    }
                    return err;
                };
                defer method_func.decref(&self.runtime_layout_store, roc_ops);

                // Prepare arguments: receiver + explicit args
                const total_args = 1 + arg_values.len;
                var all_args = try self.allocator.alloc(StackValue, total_args);
                defer self.allocator.free(all_args);

                // First argument is the receiver
                all_args[0] = receiver_value;

                // Remaining arguments
                for (arg_values, 0..) |arg, i| {
                    all_args[i + 1] = arg;
                }

                // Call the method closure
                if (method_func.layout.tag != .closure) {
                    // Decref all args before returning error
                    for (all_args) |arg| {
                        arg.decref(&self.runtime_layout_store, roc_ops);
                    }
                    return error.TypeMismatch;
                }

                const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                // Switch to the closure's source module for correct expression evaluation
                const saved_env = self.env;
                const saved_bindings_len = self.bindings.items.len;
                self.env = @constCast(closure_header.source_env);
                defer {
                    self.env = saved_env;
                    self.bindings.shrinkRetainingCapacity(saved_bindings_len);
                }

                const params = self.env.store.slicePatterns(closure_header.params);
                if (params.len != all_args.len) {
                    // Decref all args before returning error
                    for (all_args) |arg| {
                        arg.decref(&self.runtime_layout_store, roc_ops);
                    }
                    return error.TypeMismatch;
                }

                // Provide closure context for capture lookup during body eval
                try self.active_closures.append(method_func);
                defer _ = self.active_closures.pop();

                // Check if this is a low-level lambda - if so, dispatch to builtin implementation
                const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                if (lambda_expr == .e_low_level_lambda) {
                    const low_level = lambda_expr.e_low_level_lambda;

                    // Get return type for low-level builtin
                    const method_call_ret_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));

                    // Dispatch to actual low-level builtin implementation
                    const result = try self.callLowLevelBuiltin(low_level.op, all_args, roc_ops, method_call_ret_rt_var);

                    // Decref args that aren't consumed by the builtin.
                    // list_concat consumes its input lists (handles refcounting internally).
                    if (low_level.op != .list_concat) {
                        for (all_args) |arg| {
                            arg.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    return result;
                }

                var bind_count: usize = 0;
                while (bind_count < params.len) : (bind_count += 1) {
                    try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = all_args[bind_count], .expr_idx = @enumFromInt(0), .source_env = self.env });
                }
                defer {
                    var k = params.len;
                    while (k > 0) {
                        k -= 1;
                        if (self.bindings.pop()) |binding| {
                            binding.value.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }
                }

                // Evaluate body, handling early returns at function boundary
                return self.evalExprMinimal(closure_header.body_idx, roc_ops, null) catch |err| {
                    if (err == error.EarlyReturn) {
                        const return_val = self.early_return_value orelse return error.Crash;
                        self.early_return_value = null;
                        return return_val;
                    }
                    return err;
                };
            },
            .e_lookup_local => |lookup| {
                // Search bindings in reverse
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    // Check both pattern_idx AND source module to avoid cross-module collisions.
                    // Pattern indices are module-local, so the same pattern_idx can exist in
                    // multiple modules. We must match the binding from the correct module.
                    // Note: Compare by module_name_idx (interned identifier), not pointer, because
                    // the same module can have multiple ModuleEnv instances (e.g., when closures
                    // reference their source env).
                    const same_module = (b.source_env == self.env) or
                        (b.source_env.module_name_idx == self.env.module_name_idx);
                    if (b.pattern_idx == lookup.pattern_idx and same_module) {
                        // Check if this binding came from an e_anno_only expression
                        // Skip check for expr_idx == 0 (sentinel for non-def bindings like parameters)
                        const expr_idx_int: u32 = @intFromEnum(b.expr_idx);
                        if (expr_idx_int != 0) {
                            const binding_expr = self.env.store.getExpr(b.expr_idx);
                            if (binding_expr == .e_anno_only and b.value.layout.tag != .closure) {
                                // This is a non-function annotation-only value being looked up
                                self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
                                return error.Crash;
                            }
                            // e_low_level_lambda is always a closure, so no special check needed
                        }
                        const copy_result = try self.pushCopy(b.value, roc_ops);
                        return copy_result;
                    }
                }
                // If not found, try active closure captures by variable name
                // IMPORTANT: Only check captures if the closure header indicates it actually has captures
                // The captures layout from the type system might not match what's actually captured
                // Search ALL active closures in reverse order (innermost to outermost) for nested lambdas
                if (self.active_closures.items.len > 0) {
                    const pat2 = self.env.store.getPattern(lookup.pattern_idx);
                    if (pat2 == .assign) {
                        const var_ident = pat2.assign.ident;
                        // Search from innermost to outermost closure
                        var closure_idx: usize = self.active_closures.items.len;
                        while (closure_idx > 0) {
                            closure_idx -= 1;
                            const cls_val = self.active_closures.items[closure_idx];
                            if (cls_val.layout.tag == .closure and cls_val.ptr != null) {
                                const header: *const layout.Closure = @ptrCast(@alignCast(cls_val.ptr.?));
                                // Check if this closure was created with actual captures (e_closure)
                                // vs. a plain lambda (e_lambda) or low-level lambda (e_low_level_lambda)
                                // Only e_closure creates real capture values; others have uninitialized captures area
                                const lambda_expr = header.source_env.store.getExpr(header.lambda_expr_idx);
                                const has_real_captures = (lambda_expr == .e_closure);
                                if (has_real_captures) {
                                    const captures_layout = self.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                                    const header_sz = @sizeOf(layout.Closure);
                                    const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                                    const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                                    const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                                    var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                                    if (accessor.findFieldIndex(var_ident)) |fidx| {
                                        const field_val = try accessor.getFieldByIndex(fidx);
                                        return try self.pushCopy(field_val, roc_ops);
                                    }
                                }
                            }
                        }
                    }
                }

                // Check if this pattern corresponds to a top-level def that wasn't evaluated yet
                const all_defs = self.env.store.sliceDefs(self.env.all_defs);
                for (all_defs) |def_idx| {
                    const def = self.env.store.getDef(def_idx);
                    if (def.pattern == lookup.pattern_idx) {
                        // Evaluate the definition on demand and cache the result in bindings
                        const result = try self.evalExprMinimal(def.expr, roc_ops, null);
                        try self.bindings.append(.{
                            .pattern_idx = def.pattern,
                            .value = result,
                            .expr_idx = def.expr,
                            .source_env = self.env,
                        });
                        return result;
                    }
                }

                self.triggerCrash("e_lookup_local: definition not found in current scope", false, roc_ops);
                return error.Crash;
            },
            .e_lookup_external => |lookup| {
                // Cross-module reference - look up in imported module
                const other_env = self.import_envs.get(lookup.module_idx) orelse {
                    self.triggerCrash("e_lookup_external: import_envs missing entry for module", false, roc_ops);
                    return error.Crash;
                };

                // The target_node_idx is a Def.Idx in the other module
                const target_def_idx: can.CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
                const target_def = other_env.store.getDef(target_def_idx);

                // Save both env and bindings state
                const saved_env = self.env;
                const saved_bindings_len = self.bindings.items.len;
                self.env = @constCast(other_env);
                defer {
                    self.env = saved_env;
                    self.bindings.shrinkRetainingCapacity(saved_bindings_len);
                }

                // Evaluate the definition's expression in the other module's context
                // If this is being called as a function, pass through the instantiated type
                // from the call site (via expected_rt_var) to avoid re-translating generic types
                const result = try self.evalExprMinimal(target_def.expr, roc_ops, expected_rt_var);

                return result;
            },
            .e_runtime_error => |rt_err| {
                _ = rt_err;
                self.triggerCrash("runtime error", false, roc_ops);
                return error.Crash;
            },
            .e_lookup_required => {
                // Required lookups reference values from the app that provides values to the
                // platform's `requires` clause. These are not available during compile-time
                // evaluation - they will be linked at runtime. Return TypeMismatch to signal
                // that this expression cannot be evaluated at compile time.
                return error.TypeMismatch;
            },
            // no if handling in minimal evaluator
            // no second e_binop case; handled above
            else => {
                self.triggerCrash("evalExprMinimal: unhandled expression type", false, roc_ops);
                return error.Crash;
            },
        }
    }

    fn pushStr(self: *Interpreter, content: []const u8) !StackValue {
        _ = content; // size computed below but content copied via RocStr
        const layout_val = Layout.str();
        const size: u32 = self.runtime_layout_store.layoutSize(layout_val);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = false };
        }
        const alignment = layout_val.alignment(self.runtime_layout_store.targetUsize());
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    fn stackValueToRocStr(
        self: *Interpreter,
        value: StackValue,
        value_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) !RocStr {
        if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .str) {
            if (value.ptr) |ptr| {
                const existing: *const RocStr = @ptrCast(@alignCast(ptr));
                var copy = existing.*;
                copy.incref(1);
                return copy;
            } else {
                return RocStr.empty();
            }
        }

        const rendered = blk: {
            if (value_rt_var) |rt_var| {
                break :blk try self.renderValueRocWithType(value, rt_var);
            } else {
                break :blk try self.renderValueRoc(value);
            }
        };
        defer self.allocator.free(rendered);

        return RocStr.fromSlice(rendered, roc_ops);
    }

    pub fn pushRaw(self: *Interpreter, layout_val: Layout, initial_size: usize) !StackValue {
        const size: u32 = if (initial_size == 0) self.runtime_layout_store.layoutSize(layout_val) else @intCast(initial_size);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = true };
        }
        const target_usize = self.runtime_layout_store.targetUsize();
        var alignment = layout_val.alignment(target_usize);
        if (layout_val.tag == .closure) {
            const captures_layout = self.runtime_layout_store.getLayout(layout_val.data.closure.captures_layout_idx);
            alignment = alignment.max(captures_layout.alignment(target_usize));
        }
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true };
    }

    pub fn pushCopy(self: *Interpreter, src: StackValue, roc_ops: *RocOps) !StackValue {
        const size: u32 = if (src.layout.tag == .closure) src.getTotalSize(&self.runtime_layout_store) else self.runtime_layout_store.layoutSize(src.layout);
        const target_usize = self.runtime_layout_store.targetUsize();
        var alignment = src.layout.alignment(target_usize);
        if (src.layout.tag == .closure) {
            const captures_layout = self.runtime_layout_store.getLayout(src.layout.data.closure.captures_layout_idx);
            alignment = alignment.max(captures_layout.alignment(target_usize));
        }
        const ptr = if (size > 0) try self.stack_memory.alloca(size, alignment) else null;
        // Preserve rt_var for constant folding
        const dest = StackValue{ .layout = src.layout, .ptr = ptr, .is_initialized = true, .rt_var = src.rt_var };
        if (size > 0 and src.ptr != null and ptr != null) {
            try src.copyToPtr(&self.runtime_layout_store, ptr.?, roc_ops);
        }
        return dest;
    }

    /// Call a hosted function via RocOps.hosted_fns array
    /// This marshals arguments to the host, invokes the function pointer, and marshals the result back
    fn callHostedFunction(
        self: *Interpreter,
        hosted_fn_index: u32,
        args: []StackValue,
        roc_ops: *RocOps,
        _: types.Var, // return_rt_var - currently unused, type inferred from args.len
    ) !StackValue {
        // Validate index is within bounds
        if (hosted_fn_index >= roc_ops.hosted_fns.count) {
            self.triggerCrash("Hosted function index out of bounds", false, roc_ops);
            return error.Crash;
        }

        // Get the hosted function pointer from RocOps
        const hosted_fn = roc_ops.hosted_fns.fns[hosted_fn_index];

        // Allocate space for the return value
        // Hosted lambda types aren't properly propagated from annotations, so we infer the
        // return type based on the argument type:
        // - If no args OR single ZST arg (like () in Stdin.line!), return type is Str
        // - If arg is non-zero-sized (like Str in Stdout.line!), return type is {}
        const has_zst_or_no_args = args.len == 0 or (args.len == 1 and self.runtime_layout_store.isZeroSized(args[0].layout));
        const return_layout = if (has_zst_or_no_args) blk: {
            // Functions taking unit () or no args return Str (e.g., Stdin.line!)
            break :blk layout.Layout.str();
        } else blk: {
            // Functions taking Str return {} (e.g., Stdout.line!, Stderr.line!)
            const empty_idx = try self.runtime_layout_store.ensureEmptyRecordLayout();
            break :blk self.runtime_layout_store.getLayout(empty_idx);
        };
        const result_value = try self.pushRaw(return_layout, 0);

        // Allocate stack space for marshalled arguments
        // The host now uses the same RocStr as builtins, so no conversion needed
        const ArgsStruct = extern struct { str: RocStr };
        var args_struct: ArgsStruct = undefined;

        // Marshal arguments into a contiguous struct matching the RocCall ABI
        // For now, we support zero-argument and single-argument functions
        if (args.len == 0) {
            // Zero argument case - pass dummy pointer for args
            const ret_ptr = if (result_value.ptr) |p| p else blk: {
                // Zero-sized return - pass stack address
                break :blk @as(*anyopaque, @ptrFromInt(@intFromPtr(&result_value)));
            };

            // For zero-argument functions, we still need to pass a valid args pointer
            // Use the address of args_struct even though it won't be read
            const arg_ptr = @as(*anyopaque, @ptrCast(&args_struct));

            // Invoke the hosted function following RocCall ABI: (ops, ret_ptr, args_ptr)
            hosted_fn(roc_ops, ret_ptr, arg_ptr);
        } else if (args.len == 1) {
            // Single argument case - we need to marshal it properly
            // For strings, we need to pass a RocStr struct wrapped in Args
            const arg_ptr = blk: {
                // For strings, we need to pass a RocStr struct
                // Try to determine if this is a string by checking if it contains a RocStr
                // For now, we assume it's a string if it has a pointer (TODO: better type checking)
                if (args[0].ptr) |str_ptr| {
                    const roc_str: *const RocStr = @ptrCast(@alignCast(str_ptr));
                    // Host and builtin now use the same RocStr, so just copy it
                    args_struct.str = roc_str.*;
                    break :blk @as(*anyopaque, @ptrCast(&args_struct));
                } else {
                    // Empty or zero-sized argument - create empty small string
                    args_struct.str = RocStr.empty();
                    break :blk @as(*anyopaque, @ptrCast(&args_struct));
                }
            };

            const ret_ptr = if (result_value.ptr) |p| p else blk: {
                // Zero-sized return - pass stack address
                break :blk @as(*anyopaque, @ptrFromInt(@intFromPtr(&result_value)));
            };

            // Invoke the hosted function following RocCall ABI: (ops, ret_ptr, args_ptr)
            hosted_fn(roc_ops, ret_ptr, arg_ptr);
        } else {
            // Multi-argument case - pack arguments into a struct
            // TODO: implement multi-argument marshalling
            self.triggerCrash("Multi-argument hosted functions not yet implemented in interpreter", false, roc_ops);
            return error.Crash;
        }

        return result_value;
    }

    /// Version of callLowLevelBuiltin that also accepts a target type for operations like num_from_numeral
    pub fn callLowLevelBuiltinWithTargetType(self: *Interpreter, op: can.CIR.Expr.LowLevel, args: []StackValue, roc_ops: *RocOps, return_rt_var: ?types.Var, target_type_var: ?types.Var) !StackValue {
        // For num_from_numeral, we need to pass the target type through a different mechanism
        // since the standard handler extracts it from the return type which has a generic parameter.
        // Store the target type temporarily so the handler can use it.
        const saved_target = self.num_literal_target_type;
        self.num_literal_target_type = target_type_var;
        defer self.num_literal_target_type = saved_target;
        return self.callLowLevelBuiltin(op, args, roc_ops, return_rt_var);
    }

    pub fn callLowLevelBuiltin(self: *Interpreter, op: can.CIR.Expr.LowLevel, args: []StackValue, roc_ops: *RocOps, return_rt_var: ?types.Var) !StackValue {
        switch (op) {
            .str_is_empty => {
                // Str.is_empty : Str -> Bool
                std.debug.assert(args.len == 1); // low-level .str_is_empty expects 1 argument

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null); // low-level .str_is_empty expects non-null string pointer

                const roc_str: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                return try self.makeBoolValue(builtins.str.isEmpty(roc_str.*));
            },
            .str_is_eq => {
                // Str.is_eq : Str, Str -> Bool
                std.debug.assert(args.len == 2); // low-level .str_is_eq expects 2 arguments

                const str_a = args[0];
                const str_b = args[1];
                std.debug.assert(str_a.ptr != null); // low-level .str_is_eq expects non-null string pointer
                std.debug.assert(str_b.ptr != null); // low-level .str_is_eq expects non-null string pointer

                const roc_str_a: *const RocStr = @ptrCast(@alignCast(str_a.ptr.?));
                const roc_str_b: *const RocStr = @ptrCast(@alignCast(str_b.ptr.?));

                return try self.makeBoolValue(roc_str_a.eql(roc_str_b.*));
            },
            .str_concat => {
                // Str.concat : Str, Str -> Str
                std.debug.assert(args.len == 2);

                const str_a_arg = args[0];
                const str_b_arg = args[1];

                std.debug.assert(str_a_arg.ptr != null);
                std.debug.assert(str_b_arg.ptr != null);

                const str_a: *const RocStr = @ptrCast(@alignCast(str_a_arg.ptr.?));
                const str_b: *const RocStr = @ptrCast(@alignCast(str_b_arg.ptr.?));

                // Call strConcat to concatenate the strings
                const result_str = builtins.str.strConcat(str_a.*, str_b.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_a_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_contains => {
                // Str.contains : Str, Str -> Bool
                std.debug.assert(args.len == 2);

                const haystack_arg = args[0];
                const needle_arg = args[1];

                std.debug.assert(haystack_arg.ptr != null);
                std.debug.assert(needle_arg.ptr != null);

                const haystack: *const RocStr = @ptrCast(@alignCast(haystack_arg.ptr.?));
                const needle: *const RocStr = @ptrCast(@alignCast(needle_arg.ptr.?));

                const result = builtins.str.strContains(haystack.*, needle.*);

                return try self.makeBoolValue(result);
            },
            .str_trim => {
                // Str.trim : Str -> Str
                std.debug.assert(args.len == 1);

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);

                const roc_str_arg: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_str = builtins.str.strTrim(roc_str_arg.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_trim_start => {
                // Str.trim_start : Str -> Str
                std.debug.assert(args.len == 1);

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);

                const roc_str_arg: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_str = builtins.str.strTrimStart(roc_str_arg.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_trim_end => {
                // Str.trim_end : Str -> Str
                std.debug.assert(args.len == 1);

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);

                const roc_str_arg: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_str = builtins.str.strTrimEnd(roc_str_arg.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_caseless_ascii_equals => {
                // Str.caseless_ascii_equals : Str, Str -> Bool
                std.debug.assert(args.len == 2);

                const str_a_arg = args[0];
                const str_b_arg = args[1];

                std.debug.assert(str_a_arg.ptr != null);
                std.debug.assert(str_b_arg.ptr != null);

                const str_a: *const RocStr = @ptrCast(@alignCast(str_a_arg.ptr.?));
                const str_b: *const RocStr = @ptrCast(@alignCast(str_b_arg.ptr.?));

                // Call strConcat to concatenate the strings
                const result = builtins.str.strCaselessAsciiEquals(str_a.*, str_b.*);

                return try self.makeBoolValue(result);
            },
            .str_with_ascii_lowercased => {
                // Str.with_ascii_lowercased : Str -> Str
                std.debug.assert(args.len == 1);

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);

                const roc_str_arg: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_str = builtins.str.strWithAsciiLowercased(roc_str_arg.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_with_ascii_uppercased => {
                // Str.with_ascii_uppercased : Str -> Str
                std.debug.assert(args.len == 1);

                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);

                const roc_str_arg: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_str = builtins.str.strWithAsciiUppercased(roc_str_arg.*, roc_ops);

                // Allocate space for the result string
                const result_layout = str_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_starts_with => {
                // Str.starts_with : Str, Str -> Bool
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const prefix_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(prefix_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const prefix: *const RocStr = @ptrCast(@alignCast(prefix_arg.ptr.?));

                return try self.makeBoolValue(builtins.str.startsWith(string.*, prefix.*));
            },
            .str_ends_with => {
                // Str.ends_with : Str, Str -> Bool
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const suffix_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(suffix_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const suffix: *const RocStr = @ptrCast(@alignCast(suffix_arg.ptr.?));

                return try self.makeBoolValue(builtins.str.endsWith(string.*, suffix.*));
            },
            .str_repeat => {
                // Str.repeat : Str, U64 -> Str
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const count_arg = args[1];

                std.debug.assert(string_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const count_value = try self.extractNumericValue(count_arg);
                const count: u64 = switch (count_value) {
                    .int => |v| @intCast(v),
                    .f32 => |v| @intFromFloat(v),
                    .f64 => |v| @intFromFloat(v),
                    .dec => |v| @intCast(@divTrunc(v.num, RocDec.one_point_zero.num)),
                };

                // Call repeatC to repeat the string
                const result_str = builtins.str.repeatC(string.*, count, roc_ops);

                // Allocate space for the result string
                const result_layout = string_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_with_prefix => {
                // Str.with_prefix : Str, Str -> Str (prefix ++ string)
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const prefix_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(prefix_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const prefix: *const RocStr = @ptrCast(@alignCast(prefix_arg.ptr.?));

                // with_prefix is just concat with args swapped: prefix ++ string
                const result_str = builtins.str.strConcat(prefix.*, string.*, roc_ops);

                // Allocate space for the result string
                const result_layout = string_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_drop_prefix => {
                // Str.drop_prefix : Str, Str -> Str
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const prefix_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(prefix_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const prefix: *const RocStr = @ptrCast(@alignCast(prefix_arg.ptr.?));

                const result_str = builtins.str.strDropPrefix(string.*, prefix.*, roc_ops);

                // Allocate space for the result string
                const result_layout = string_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_drop_suffix => {
                // Str.drop_suffix : Str, Str -> Str
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const suffix_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(suffix_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const suffix: *const RocStr = @ptrCast(@alignCast(suffix_arg.ptr.?));

                const result_str = builtins.str.strDropSuffix(string.*, suffix.*, roc_ops);

                // Allocate space for the result string
                const result_layout = string_arg.layout; // Str layout
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result string structure to the output
                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_count_utf8_bytes => {
                // Str.count_utf8_bytes : Str -> U64
                std.debug.assert(args.len == 1);

                const string_arg = args[0];
                std.debug.assert(string_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const byte_count = builtins.str.countUtf8Bytes(string.*);

                const result_layout = layout.Layout.int(.u64);
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;
                try out.setInt(@intCast(byte_count));
                out.is_initialized = true;
                return out;
            },
            .str_with_capacity => {
                // Str.with_capacity : U64 -> Str
                std.debug.assert(args.len == 1);

                const capacity_arg = args[0];
                const capacity_value = try self.extractNumericValue(capacity_arg);
                const capacity: u64 = @intCast(capacity_value.int);

                const result_str = builtins.str.withCapacityC(capacity, roc_ops);

                const result_layout = layout.Layout.str();
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_reserve => {
                // Str.reserve : Str, U64 -> Str
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const spare_arg = args[1];

                std.debug.assert(string_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const spare_value = try self.extractNumericValue(spare_arg);
                const spare: u64 = @intCast(spare_value.int);

                const result_str = builtins.str.reserveC(string.*, spare, roc_ops);

                const result_layout = string_arg.layout;
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_release_excess_capacity => {
                // Str.release_excess_capacity : Str -> Str
                std.debug.assert(args.len == 1);

                const string_arg = args[0];
                std.debug.assert(string_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const result_str = builtins.str.strReleaseExcessCapacity(roc_ops, string.*);

                const result_layout = string_arg.layout;
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_to_utf8 => {
                // Str.to_utf8 : Str -> List(U8)
                std.debug.assert(args.len == 1);

                const string_arg = args[0];
                std.debug.assert(string_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const result_list = builtins.str.strToUtf8C(string.*, roc_ops);

                const result_rt_var = return_rt_var orelse {
                    self.triggerCrash("str_to_utf8 requires return type info", false, roc_ops);
                    return error.Crash;
                };
                const result_layout = try self.getRuntimeLayout(result_rt_var);

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            .str_from_utf8_lossy => {
                // Str.from_utf8_lossy : List(U8) -> Str
                std.debug.assert(args.len == 1);

                const list_arg = args[0];
                std.debug.assert(list_arg.ptr != null);

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const result_str = builtins.str.fromUtf8Lossy(roc_list.*, roc_ops);

                const result_layout = layout.Layout.str();
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_split_on => {
                // Str.split_on : Str, Str -> List(Str)
                std.debug.assert(args.len == 2);

                const string_arg = args[0];
                const delimiter_arg = args[1];

                std.debug.assert(string_arg.ptr != null);
                std.debug.assert(delimiter_arg.ptr != null);

                const string: *const RocStr = @ptrCast(@alignCast(string_arg.ptr.?));
                const delimiter: *const RocStr = @ptrCast(@alignCast(delimiter_arg.ptr.?));

                const result_list = builtins.str.strSplitOn(string.*, delimiter.*, roc_ops);

                // str_split_on has a fixed return type of List(Str).
                // Prefer the caller's return_rt_var when it matches that shape, but fall back
                // to the known layout if type information is missing or incorrect.
                const result_layout = blk: {
                    const expected_idx = try self.runtime_layout_store.insertList(layout.Idx.str);
                    const expected_layout = self.runtime_layout_store.getLayout(expected_idx);

                    if (return_rt_var) |rt_var| {
                        const candidate = self.getRuntimeLayout(rt_var) catch expected_layout;
                        if (candidate.tag == .list) {
                            const elem_layout = self.runtime_layout_store.getLayout(candidate.data.list);
                            if (elem_layout.tag == .scalar and elem_layout.data.scalar.tag == .str) {
                                break :blk candidate;
                            }
                        }
                    }

                    break :blk expected_layout;
                };

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            .str_join_with => {
                // Str.join_with : List(Str), Str -> Str
                std.debug.assert(args.len == 2);

                const list_arg = args[0];
                const separator_arg = args[1];

                std.debug.assert(list_arg.ptr != null);
                std.debug.assert(separator_arg.ptr != null);

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const separator: *const RocStr = @ptrCast(@alignCast(separator_arg.ptr.?));

                const result_str = builtins.str.strJoinWithC(roc_list.*, separator.*, roc_ops);

                const result_layout = layout.Layout.str();
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .list_len => {
                // List.len : List(a) -> U64
                // Note: listLen returns usize, but List.len always returns U64.
                // We need to cast usize -> u64 for 32-bit targets (e.g. wasm32).
                std.debug.assert(args.len == 1); // low-level .list_len expects 1 argument

                const list_arg = args[0];
                std.debug.assert(list_arg.ptr != null); // low-level .list_len expects non-null list pointer

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const len_usize = builtins.list.listLen(roc_list.*);

                const len_u64: u64 = @intCast(len_usize);

                const result_layout = layout.Layout.int(.u64);
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;
                try out.setInt(@intCast(len_u64));
                out.is_initialized = true;
                return out;
            },
            .list_is_empty => {
                // List.is_empty : List(a) -> Bool
                std.debug.assert(args.len == 1); // low-level .list_is_empty expects 1 argument

                const list_arg = args[0];
                std.debug.assert(list_arg.ptr != null); // low-level .list_is_empty expects non-null list pointer

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const result = builtins.list.listIsEmpty(roc_list.*);

                return try self.makeBoolValue(result);
            },
            .list_get_unsafe => {
                // Internal operation: Get element at index without bounds checking
                // Args: List(a), U64 (index)
                // Returns: a (the element)
                std.debug.assert(args.len == 2); // low-level .list_get_unsafe expects 2 arguments

                const list_arg = args[0];
                const index_arg = args[1];

                std.debug.assert(list_arg.ptr != null); // low-level .list_get_unsafe expects non-null list pointer

                // Extract element layout from List(a)
                std.debug.assert(list_arg.layout.tag == .list or list_arg.layout.tag == .list_of_zst); // low-level .list_get_unsafe expects list layout

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const index = index_arg.asI128(); // U64 stored as i128

                // Get element layout
                const elem_layout_idx = list_arg.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);

                if (elem_size == 0) {
                    // ZST element - return zero-sized value
                    return StackValue{
                        .layout = elem_layout,
                        .ptr = null,
                        .is_initialized = true,
                    };
                }

                // Get pointer to element (no bounds checking!)
                const elem_ptr = builtins.list.listGetUnsafe(roc_list.*, @intCast(index), elem_size);
                // Null pointer from list_get_unsafe is a compiler bug - bounds should have been checked
                std.debug.assert(elem_ptr != null);

                // Create StackValue pointing to the element
                const elem_value = StackValue{
                    .layout = elem_layout,
                    .ptr = @ptrCast(elem_ptr.?),
                    .is_initialized = true,
                };

                // Copy to new location and increment refcount
                return try self.pushCopy(elem_value, roc_ops);
            },
            .list_concat => {
                // List.concat : List(a), List(a) -> List(a)
                std.debug.assert(args.len == 2);

                const list_a_arg = args[0];
                const list_b_arg = args[1];

                std.debug.assert(list_a_arg.ptr != null);
                std.debug.assert(list_b_arg.ptr != null);

                // Extract element layout from List(a)
                std.debug.assert(list_a_arg.layout.tag == .list or list_a_arg.layout.tag == .list_of_zst);
                std.debug.assert(list_b_arg.layout.tag == .list or list_b_arg.layout.tag == .list_of_zst);

                const list_a: *const builtins.list.RocList = @ptrCast(@alignCast(list_a_arg.ptr.?));
                const list_b: *const builtins.list.RocList = @ptrCast(@alignCast(list_b_arg.ptr.?));

                // Get element layout
                const elem_layout_idx = list_a_arg.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Set up context for refcount callbacks
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
                    .roc_ops = roc_ops,
                };

                // Call listConcat with proper inc/dec callbacks.
                // If elements are refcounted, pass callbacks that will inc/dec each element.
                // Otherwise, pass no-op callbacks.
                const result_list = builtins.list.listConcat(
                    list_a.*,
                    list_b.*,
                    elem_alignment_u32,
                    elem_size,
                    elements_refcounted,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementInc else &builtins.list.rcNone,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementDec else &builtins.list.rcNone,
                    roc_ops,
                );

                // Allocate space for the result list
                const result_layout = list_a_arg.layout; // Same layout as input
                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                // Copy the result list structure to the output
                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            .set_is_empty => {
                // TODO: implement Set.is_empty
                self.triggerCrash("Set.is_empty not yet implemented", false, roc_ops);
                return error.Crash;
            },

            // Bool operations
            .bool_is_eq => {
                // Bool.is_eq : Bool, Bool -> Bool
                std.debug.assert(args.len == 2); // low-level .bool_is_eq expects 2 arguments
                const lhs = args[0].asBool();
                const rhs = args[1].asBool();
                const result = lhs == rhs;
                return try self.makeBoolValue(result);
            },
            // Numeric type checking operations
            .num_is_zero => {
                // num.is_zero : num -> Bool
                std.debug.assert(args.len == 1); // low-level .num_is_zero expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result = switch (num_val) {
                    .int => |i| i == 0,
                    .f32 => |f| f == 0.0,
                    .f64 => |f| f == 0.0,
                    .dec => |d| d.num == 0,
                };
                return try self.makeBoolValue(result);
            },
            .num_is_negative => {
                // num.is_negative : num -> Bool (signed types only)
                std.debug.assert(args.len == 1); // low-level .num_is_negative expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result = switch (num_val) {
                    .int => |i| i < 0,
                    .f32 => |f| f < 0.0,
                    .f64 => |f| f < 0.0,
                    .dec => |d| d.num < 0,
                };
                return try self.makeBoolValue(result);
            },
            .num_is_positive => {
                // num.is_positive : num -> Bool (signed types only)
                std.debug.assert(args.len == 1); // low-level .num_is_positive expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result = switch (num_val) {
                    .int => |i| i > 0,
                    .f32 => |f| f > 0.0,
                    .f64 => |f| f > 0.0,
                    .dec => |d| d.num > 0,
                };
                return try self.makeBoolValue(result);
            },

            // Numeric comparison operations
            .num_is_eq => {
                // num.is_eq : num, num -> Bool (all integer types + Dec, NOT F32/F64)
                std.debug.assert(args.len == 2); // low-level .num_is_eq expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result = switch (lhs) {
                    .int => |l| l == rhs.int,
                    .dec => |l| l.num == rhs.dec.num,
                    .f32, .f64 => {
                        self.triggerCrash("Equality comparison not supported for F32/F64 due to floating point imprecision", false, roc_ops);
                        return error.Crash;
                    },
                };
                return try self.makeBoolValue(result);
            },
            .num_is_gt => {
                // num.is_gt : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_gt expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result = switch (lhs) {
                    .int => |l| l > rhs.int,
                    .f32 => |l| l > rhs.f32,
                    .f64 => |l| l > rhs.f64,
                    .dec => |l| l.num > rhs.dec.num,
                };
                return try self.makeBoolValue(result);
            },
            .num_is_gte => {
                // num.is_gte : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_gte expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result = switch (lhs) {
                    .int => |l| l >= rhs.int,
                    .f32 => |l| l >= rhs.f32,
                    .f64 => |l| l >= rhs.f64,
                    .dec => |l| l.num >= rhs.dec.num,
                };
                return try self.makeBoolValue(result);
            },
            .num_is_lt => {
                // num.is_lt : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_lt expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result = switch (lhs) {
                    .int => |l| l < rhs.int,
                    .f32 => |l| l < rhs.f32,
                    .f64 => |l| l < rhs.f64,
                    .dec => |l| l.num < rhs.dec.num,
                };
                return try self.makeBoolValue(result);
            },
            .num_is_lte => {
                // num.is_lte : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_lte expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result = switch (lhs) {
                    .int => |l| l <= rhs.int,
                    .f32 => |l| l <= rhs.f32,
                    .f64 => |l| l <= rhs.f64,
                    .dec => |l| l.num <= rhs.dec.num,
                };
                return try self.makeBoolValue(result);
            },

            // Numeric arithmetic operations
            .num_negate => {
                // num.negate : num -> num (signed types only)
                std.debug.assert(args.len == 1); // low-level .num_negate expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (num_val) {
                    .int => |i| try out.setInt(-i),
                    .f32 => |f| out.setF32(-f),
                    .f64 => |f| out.setF64(-f),
                    .dec => |d| out.setDec(RocDec{ .num = -d.num }),
                }
                out.is_initialized = true;
                return out;
            },
            .num_plus => {
                std.debug.assert(args.len == 2); // low-level .num_plus expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| try out.setInt(l + rhs.int),
                    .f32 => |l| out.setF32(l + rhs.f32),
                    .f64 => |l| out.setF64(l + rhs.f64),
                    .dec => |l| out.setDec(RocDec.add(l, rhs.dec, roc_ops)),
                }
                out.is_initialized = true;
                return out;
            },
            .num_minus => {
                std.debug.assert(args.len == 2); // low-level .num_minus expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| try out.setInt(l - rhs.int),
                    .f32 => |l| out.setF32(l - rhs.f32),
                    .f64 => |l| out.setF64(l - rhs.f64),
                    .dec => |l| out.setDec(RocDec.sub(l, rhs.dec, roc_ops)),
                }
                out.is_initialized = true;
                return out;
            },
            .num_times => {
                std.debug.assert(args.len == 2); // low-level .num_times expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| try out.setInt(l * rhs.int),
                    .f32 => |l| out.setF32(l * rhs.f32),
                    .f64 => |l| out.setF64(l * rhs.f64),
                    .dec => |l| out.setDec(RocDec.mul(l, rhs.dec, roc_ops)),
                }
                out.is_initialized = true;
                return out;
            },
            .num_div_by => {
                std.debug.assert(args.len == 2); // low-level .num_div_by expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| {
                        if (rhs.int == 0) return error.DivisionByZero;
                        try out.setInt(@divTrunc(l, rhs.int));
                    },
                    .f32 => |l| {
                        if (rhs.f32 == 0) return error.DivisionByZero;
                        out.setF32(l / rhs.f32);
                    },
                    .f64 => |l| {
                        if (rhs.f64 == 0) return error.DivisionByZero;
                        out.setF64(l / rhs.f64);
                    },
                    .dec => |l| {
                        if (rhs.dec.num == 0) return error.DivisionByZero;
                        out.setDec(RocDec.div(l, rhs.dec, roc_ops));
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_div_trunc_by => {
                std.debug.assert(args.len == 2); // low-level .num_div_trunc_by expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| {
                        if (rhs.int == 0) return error.DivisionByZero;
                        try out.setInt(@divTrunc(l, rhs.int));
                    },
                    .f32 => |l| {
                        if (rhs.f32 == 0) return error.DivisionByZero;
                        out.setF32(@trunc(l / rhs.f32));
                    },
                    .f64 => |l| {
                        if (rhs.f64 == 0) return error.DivisionByZero;
                        out.setF64(@trunc(l / rhs.f64));
                    },
                    .dec => |l| {
                        // For Dec, div and div_trunc are the same since it's already integer-like
                        if (rhs.dec.num == 0) return error.DivisionByZero;
                        out.setDec(RocDec.div(l, rhs.dec, roc_ops));
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_rem_by => {
                std.debug.assert(args.len == 2); // low-level .num_rem_by expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| {
                        if (rhs.int == 0) return error.DivisionByZero;
                        try out.setInt(@rem(l, rhs.int));
                    },
                    .f32 => |l| {
                        if (rhs.f32 == 0) return error.DivisionByZero;
                        out.setF32(@rem(l, rhs.f32));
                    },
                    .f64 => |l| {
                        if (rhs.f64 == 0) return error.DivisionByZero;
                        out.setF64(@rem(l, rhs.f64));
                    },
                    .dec => |l| {
                        if (rhs.dec.num == 0) return error.DivisionByZero;
                        out.setDec(RocDec.rem(l, rhs.dec, roc_ops));
                    },
                }
                out.is_initialized = true;
                return out;
            },

            // Numeric parsing operations
            .num_from_int_digits => {
                // num.from_int_digits : List(U8) -> Try(num, [OutOfRange])
                std.debug.assert(args.len == 1); // expects 1 argument: List(U8)

                // Return type info is required - missing it is a compiler bug
                const result_rt_var = return_rt_var orelse unreachable;

                // Get the result layout (Try tag union)
                const result_layout = try self.getRuntimeLayout(result_rt_var);

                // Extract base-256 digits from List(U8)
                const list_arg = args[0];
                std.debug.assert(list_arg.ptr != null);
                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const list_len = roc_list.len();
                const digits_ptr = roc_list.elements(u8);
                const digits: []const u8 = if (digits_ptr) |ptr| ptr[0..list_len] else &[_]u8{};

                // Convert base-256 digits to u128 (max intermediate precision)
                var value: u128 = 0;
                var overflow = false;
                for (digits) |digit| {
                    const new_value = @mulWithOverflow(value, 256);
                    if (new_value[1] != 0) {
                        overflow = true;
                        break;
                    }
                    const add_result = @addWithOverflow(new_value[0], digit);
                    if (add_result[1] != 0) {
                        overflow = true;
                        break;
                    }
                    value = add_result[0];
                }

                // Resolve the Try type to get Ok's payload type (the numeric type)
                const resolved = self.resolveBaseVar(result_rt_var);
                // Type system should guarantee this is a tag union - if not, it's a compiler bug
                std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .tag_union);

                // Find tag indices for Ok and Err
                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(result_rt_var, &tag_list);

                var ok_index: ?usize = null;
                var err_index: ?usize = null;
                var ok_payload_var: ?types.Var = null;

                // Use precomputed idents from the module env for direct comparison instead of string matching
                const ok_ident = self.env.idents.ok;
                const err_ident = self.env.idents.err;

                for (tag_list.items, 0..) |tag_info, i| {
                    if (tag_info.name == ok_ident) {
                        ok_index = i;
                        const arg_vars = self.runtime_types.sliceVars(tag_info.args);
                        if (arg_vars.len >= 1) {
                            ok_payload_var = arg_vars[0];
                        }
                    } else if (tag_info.name == err_ident) {
                        err_index = i;
                    }
                }

                // Determine target numeric type and check range
                var in_range = !overflow;
                if (in_range and ok_payload_var != null) {
                    const num_layout = try self.getRuntimeLayout(ok_payload_var.?);
                    if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .int) {
                        // Check if value fits in target integer type
                        const int_type = num_layout.data.scalar.data.int;
                        in_range = switch (int_type) {
                            .u8 => value <= std.math.maxInt(u8),
                            .i8 => value <= std.math.maxInt(i8),
                            .u16 => value <= std.math.maxInt(u16),
                            .i16 => value <= std.math.maxInt(i16),
                            .u32 => value <= std.math.maxInt(u32),
                            .i32 => value <= std.math.maxInt(i32),
                            .u64 => value <= std.math.maxInt(u64),
                            .i64 => value <= std.math.maxInt(i64),
                            .u128, .i128 => true, // u128 fits, i128 needs sign check
                        };
                    }
                }

                // Construct the result tag union
                if (result_layout.tag == .scalar) {
                    // Simple tag with no payload (shouldn't happen for Try)
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try out.setInt(@intCast(tag_idx));
                    out.is_initialized = true;
                    return out;
                } else if (result_layout.tag == .record) {
                    // Record { tag, payload }
                    var dest = try self.pushRaw(result_layout, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
                    const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse unreachable;
                    const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse unreachable;

                    // Write tag discriminant
                    const tag_field = try acc.getFieldByIndex(tag_field_idx);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area
                    const payload_field = try acc.getFieldByIndex(payload_field_idx);
                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    // Write payload
                    if (in_range and ok_payload_var != null) {
                        // Write the numeric value as Ok payload
                        const num_layout = try self.getRuntimeLayout(ok_payload_var.?);
                        if (payload_field.ptr) |payload_ptr| {
                            if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .int) {
                                // Write integer value directly to payload
                                const int_type = num_layout.data.scalar.data.int;
                                switch (int_type) {
                                    .u8 => @as(*u8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .i8 => @as(*i8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .u16 => @as(*u16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .i16 => @as(*i16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .u32 => @as(*u32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .i32 => @as(*i32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .u64 => @as(*u64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .i64 => @as(*i64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    .u128 => @as(*u128, @ptrCast(@alignCast(payload_ptr))).* = value,
                                    .i128 => @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                }
                            }
                        }
                    }
                    // For Err case, payload is OutOfRange which is a zero-arg tag (already zeroed)

                    return dest;
                }

                // Unsupported result layout is a compiler bug
                unreachable;
            },
            .num_from_dec_digits => {
                // num.from_dec_digits : (List(U8), List(U8)) -> Try(num, [OutOfRange])
                self.triggerCrash("num_from_dec_digits not yet implemented", false, roc_ops);
                return error.Crash;
            },
            .num_from_numeral => {
                // num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)])
                // Numeral is { is_negative: Bool, digits_before_pt: List(U8), digits_after_pt: List(U8) }
                std.debug.assert(args.len == 1); // expects 1 argument: Numeral record

                // Return type info is required - missing it is a compiler bug
                const result_rt_var = return_rt_var orelse unreachable;

                // Get the result layout (Try tag union)
                const result_layout = try self.getRuntimeLayout(result_rt_var);

                // Extract fields from Numeral record
                const num_literal_arg = args[0];
                // Null argument is a compiler bug - the compiler should never produce code with null args
                std.debug.assert(num_literal_arg.ptr != null);

                // Argument should be a record - if not, it's a compiler bug
                var acc = num_literal_arg.asRecord(&self.runtime_layout_store) catch unreachable;

                // Get is_negative field
                // Use runtime_layout_store.env for field lookups since the record was built with that env's idents
                const layout_env = self.runtime_layout_store.env;
                // Field lookups should succeed - missing fields is a compiler bug
                const is_neg_idx = acc.findFieldIndex(layout_env.idents.is_negative) orelse unreachable;
                const is_neg_field = acc.getFieldByIndex(is_neg_idx) catch unreachable;
                const is_negative = getRuntimeU8(is_neg_field) != 0;

                // Get digits_before_pt field (List(U8))
                const before_idx = acc.findFieldIndex(layout_env.idents.digits_before_pt) orelse unreachable;
                const before_field = acc.getFieldByIndex(before_idx) catch unreachable;

                // Get digits_after_pt field (List(U8))
                const after_idx = acc.findFieldIndex(layout_env.idents.digits_after_pt) orelse unreachable;
                const after_field = acc.getFieldByIndex(after_idx) catch unreachable;

                // Extract list data from digits_before_pt
                const before_list: *const builtins.list.RocList = @ptrCast(@alignCast(before_field.ptr.?));
                const before_len = before_list.len();
                const before_ptr = before_list.elements(u8);
                const digits_before: []const u8 = if (before_ptr) |ptr| ptr[0..before_len] else &[_]u8{};

                // Extract list data from digits_after_pt
                const after_list: *const builtins.list.RocList = @ptrCast(@alignCast(after_field.ptr.?));
                const after_len = after_list.len();
                const after_ptr = after_list.elements(u8);
                const digits_after: []const u8 = if (after_ptr) |ptr| ptr[0..after_len] else &[_]u8{};

                // Convert base-256 digits to u128
                var value: u128 = 0;
                var overflow = false;
                for (digits_before) |digit| {
                    const new_value = @mulWithOverflow(value, 256);
                    if (new_value[1] != 0) {
                        overflow = true;
                        break;
                    }
                    const add_result = @addWithOverflow(new_value[0], digit);
                    if (add_result[1] != 0) {
                        overflow = true;
                        break;
                    }
                    value = add_result[0];
                }

                // Resolve the Try type to get Ok's payload type
                const resolved = self.resolveBaseVar(result_rt_var);
                // Type system should guarantee this is a tag union - if not, it's a compiler bug
                std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .tag_union);

                // Find tag indices for Ok and Err
                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(result_rt_var, &tag_list);

                var ok_index: ?usize = null;
                var err_index: ?usize = null;
                var ok_payload_var: ?types.Var = null;
                var err_payload_var: ?types.Var = null;

                // Use precomputed idents from the module env for direct comparison instead of string matching
                const ok_ident = self.env.idents.ok;
                const err_ident = self.env.idents.err;

                for (tag_list.items, 0..) |tag_info, i| {
                    if (tag_info.name == ok_ident) {
                        ok_index = i;
                        const arg_vars = self.runtime_types.sliceVars(tag_info.args);
                        if (arg_vars.len >= 1) {
                            ok_payload_var = arg_vars[0];
                        }
                    } else if (tag_info.name == err_ident) {
                        err_index = i;
                        const arg_vars = self.runtime_types.sliceVars(tag_info.args);
                        if (arg_vars.len >= 1) {
                            err_payload_var = arg_vars[0];
                        }
                    }
                }

                // Determine target numeric type and check range
                var in_range = !overflow;
                var rejection_reason: enum { none, overflow, negative_unsigned, fractional_integer, out_of_range } = .none;
                if (overflow) rejection_reason = .overflow;

                // Track target type info for error messages
                var type_name: []const u8 = "number";
                var min_value_str: []const u8 = "";
                var max_value_str: []const u8 = "";

                // Use the explicit target type if provided, otherwise fall back to ok_payload_var
                const target_type_var = self.num_literal_target_type orelse ok_payload_var;

                if (in_range and target_type_var != null) {
                    // Use the target type var directly - getRuntimeLayout handles nominal types properly
                    // (Don't use resolveBaseVar here as it strips away nominal type info needed for layout)
                    const num_layout = try self.getRuntimeLayout(target_type_var.?);
                    if (num_layout.tag == .scalar) {
                        if (num_layout.data.scalar.tag == .int) {
                            // Integer type - check range and sign
                            const int_type = num_layout.data.scalar.data.int;

                            // Set type info for error messages
                            switch (int_type) {
                                .u8 => {
                                    type_name = "U8";
                                    min_value_str = "0";
                                    max_value_str = "255";
                                },
                                .i8 => {
                                    type_name = "I8";
                                    min_value_str = "-128";
                                    max_value_str = "127";
                                },
                                .u16 => {
                                    type_name = "U16";
                                    min_value_str = "0";
                                    max_value_str = "65535";
                                },
                                .i16 => {
                                    type_name = "I16";
                                    min_value_str = "-32768";
                                    max_value_str = "32767";
                                },
                                .u32 => {
                                    type_name = "U32";
                                    min_value_str = "0";
                                    max_value_str = "4294967295";
                                },
                                .i32 => {
                                    type_name = "I32";
                                    min_value_str = "-2147483648";
                                    max_value_str = "2147483647";
                                },
                                .u64 => {
                                    type_name = "U64";
                                    min_value_str = "0";
                                    max_value_str = "18446744073709551615";
                                },
                                .i64 => {
                                    type_name = "I64";
                                    min_value_str = "-9223372036854775808";
                                    max_value_str = "9223372036854775807";
                                },
                                .u128 => {
                                    type_name = "U128";
                                    min_value_str = "0";
                                    max_value_str = "340282366920938463463374607431768211455";
                                },
                                .i128 => {
                                    type_name = "I128";
                                    min_value_str = "-170141183460469231731687303715884105728";
                                    max_value_str = "170141183460469231731687303715884105727";
                                },
                            }

                            // Check sign for unsigned types
                            if (is_negative) {
                                switch (int_type) {
                                    .u8, .u16, .u32, .u64, .u128 => {
                                        in_range = false;
                                        rejection_reason = .negative_unsigned;
                                    },
                                    else => {},
                                }
                            }

                            // Check value range
                            if (in_range) {
                                const value_in_range = switch (int_type) {
                                    .u8 => value <= std.math.maxInt(u8),
                                    .i8 => if (is_negative) value <= @as(u128, @abs(@as(i128, std.math.minInt(i8)))) else value <= std.math.maxInt(i8),
                                    .u16 => value <= std.math.maxInt(u16),
                                    .i16 => if (is_negative) value <= @as(u128, @abs(@as(i128, std.math.minInt(i16)))) else value <= std.math.maxInt(i16),
                                    .u32 => value <= std.math.maxInt(u32),
                                    .i32 => if (is_negative) value <= @as(u128, @abs(@as(i128, std.math.minInt(i32)))) else value <= std.math.maxInt(i32),
                                    .u64 => value <= std.math.maxInt(u64),
                                    .i64 => if (is_negative) value <= @as(u128, @abs(@as(i128, std.math.minInt(i64)))) else value <= std.math.maxInt(i64),
                                    .u128 => true,
                                    .i128 => true,
                                };
                                if (!value_in_range) {
                                    in_range = false;
                                    rejection_reason = .out_of_range;
                                }
                            }

                            // Fractional part not allowed for integers
                            if (in_range and digits_after.len > 0) {
                                var has_fractional = false;
                                for (digits_after) |d| {
                                    if (d != 0) {
                                        has_fractional = true;
                                        break;
                                    }
                                }
                                if (has_fractional) {
                                    in_range = false;
                                    rejection_reason = .fractional_integer;
                                }
                            }
                        } else if (num_layout.data.scalar.tag == .frac) {
                            const frac_type = num_layout.data.scalar.data.frac;
                            switch (frac_type) {
                                .f32 => type_name = "F32",
                                .f64 => type_name = "F64",
                                .dec => type_name = "Dec",
                            }
                        }
                    }
                }

                // Construct the result tag union
                if (result_layout.tag == .scalar) {
                    // Simple tag with no payload
                    var out = try self.pushRaw(result_layout, 0);
                    out.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try out.setInt(@intCast(tag_idx));
                    out.is_initialized = true;
                    return out;
                } else if (result_layout.tag == .record) {
                    // Record { tag, payload }
                    var dest = try self.pushRaw(result_layout, 0);
                    var result_acc = try dest.asRecord(&self.runtime_layout_store);
                    // Use layout_env for field lookups since record fields use layout store's env idents
                    // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
                    const tag_field_idx = result_acc.findFieldIndex(layout_env.idents.tag) orelse unreachable;
                    const payload_field_idx = result_acc.findFieldIndex(layout_env.idents.payload) orelse unreachable;

                    // Write tag discriminant
                    const tag_field = try result_acc.getFieldByIndex(tag_field_idx);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area
                    const payload_field = try result_acc.getFieldByIndex(payload_field_idx);
                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    // Write payload for Ok case
                    if (in_range and ok_payload_var != null) {
                        const num_layout = try self.getRuntimeLayout(ok_payload_var.?);
                        if (payload_field.ptr) |payload_ptr| {
                            if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .int) {
                                const int_type = num_layout.data.scalar.data.int;
                                if (is_negative) {
                                    // Write negative value
                                    // For i128, we need special handling because the minimum value's absolute
                                    // value (2^127) doesn't fit in i128 (max is 2^127-1). Use wrapping negation.
                                    switch (int_type) {
                                        .i8 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i16 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i32 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i64 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i128 => {
                                            // For i128, we need special handling because the minimum value's absolute
                                            // value (2^127) doesn't fit in i128 (max is 2^127-1).
                                            // We interpret the u128 value as an i128 and negate using wrapping arithmetic.
                                            // This correctly handles i128 min value: -(2^127) wraps to itself.
                                            const as_signed: i128 = @bitCast(value);
                                            const neg_value: i128 = -%as_signed;
                                            @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = neg_value;
                                        },
                                        else => {}, // Unsigned types already rejected above
                                    }
                                } else {
                                    // Write positive value
                                    switch (int_type) {
                                        .u8 => @as(*u8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i8 => @as(*i8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u16 => @as(*u16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i16 => @as(*i16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u32 => @as(*u32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i32 => @as(*i32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u64 => @as(*u64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i64 => @as(*i64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u128 => @as(*u128, @ptrCast(@alignCast(payload_ptr))).* = value,
                                        .i128 => @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    }
                                }
                            } else if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .frac) {
                                // Floating-point and Dec types
                                const frac_precision = num_layout.data.scalar.data.frac;
                                const float_value: f64 = if (is_negative)
                                    -@as(f64, @floatFromInt(value))
                                else
                                    @as(f64, @floatFromInt(value));

                                // Handle fractional part for floats
                                var final_value = float_value;
                                if (digits_after.len > 0) {
                                    var frac_value: f64 = 0;
                                    var frac_mult: f64 = 1.0 / 256.0;
                                    for (digits_after) |digit| {
                                        frac_value += @as(f64, @floatFromInt(digit)) * frac_mult;
                                        frac_mult /= 256.0;
                                    }
                                    if (is_negative) {
                                        final_value -= frac_value;
                                    } else {
                                        final_value += frac_value;
                                    }
                                }

                                switch (frac_precision) {
                                    .f32 => @as(*f32, @ptrCast(@alignCast(payload_ptr))).* = @floatCast(final_value),
                                    .f64 => @as(*f64, @ptrCast(@alignCast(payload_ptr))).* = final_value,
                                    .dec => {
                                        // Dec type - RocDec has i128 internal representation
                                        const dec_value: i128 = if (is_negative)
                                            -@as(i128, @intCast(value)) * builtins.dec.RocDec.one_point_zero_i128
                                        else
                                            @as(i128, @intCast(value)) * builtins.dec.RocDec.one_point_zero_i128;
                                        @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = dec_value;
                                    },
                                }
                            }
                        }
                    } else if (!in_range and err_payload_var != null) {
                        // For Err case, construct InvalidNumeral(Str) with descriptive message
                        // Format the number that was rejected
                        var num_str_buf: [128]u8 = undefined;
                        const num_str = can.CIR.formatBase256ToDecimal(is_negative, digits_before, digits_after, &num_str_buf);

                        // Create descriptive error message
                        const error_msg = switch (rejection_reason) {
                            .negative_unsigned => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. {s} values cannot be negative.",
                                .{ num_str, type_name, type_name },
                            ) catch null,
                            .fractional_integer => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. {s} values must be whole numbers, not fractions.",
                                .{ num_str, type_name, type_name },
                            ) catch null,
                            .out_of_range, .overflow => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. Valid {s} values are integers between {s} and {s}.",
                                .{ num_str, type_name, type_name, min_value_str, max_value_str },
                            ) catch null,
                            .none => null,
                        };

                        if (error_msg) |msg| {
                            // Get the Err payload layout (which is [InvalidNumeral(Str)])
                            const err_payload_layout = try self.getRuntimeLayout(err_payload_var.?);
                            const payload_field_size = self.runtime_layout_store.layoutSize(payload_field.layout);

                            // Check if payload area has enough space for RocStr (24 bytes on 64-bit)
                            // The layout computation may be wrong for error types, so check against actual RocStr size
                            const roc_str_size = @sizeOf(RocStr);
                            if (payload_field_size >= roc_str_size and payload_field.ptr != null) {
                                defer self.allocator.free(msg);
                                const outer_payload_ptr = payload_field.ptr.?;
                                // Create the RocStr for the error message
                                const roc_str = RocStr.fromSlice(msg, roc_ops);

                                if (err_payload_layout.tag == .record) {
                                    // InvalidNumeral tag union is a record { tag, payload }
                                    // Set is_initialized to true since we're about to write to this memory
                                    var err_inner = StackValue{
                                        .ptr = outer_payload_ptr,
                                        .layout = err_payload_layout,
                                        .is_initialized = true,
                                    };
                                    var err_acc = try err_inner.asRecord(&self.runtime_layout_store);

                                    // Set the tag to InvalidNumeral (index 0, assuming it's the first/only tag)
                                    // Use layout store's env for field lookup to match comptime_evaluator
                                    if (err_acc.findFieldIndex(layout_env.idents.tag)) |inner_tag_idx| {
                                        const inner_tag_field = try err_acc.getFieldByIndex(inner_tag_idx);
                                        if (inner_tag_field.layout.tag == .scalar and inner_tag_field.layout.data.scalar.tag == .int) {
                                            var inner_tmp = inner_tag_field;
                                            inner_tmp.is_initialized = false;
                                            try inner_tmp.setInt(0); // InvalidNumeral tag index
                                        }
                                    }

                                    // Set the payload to the Str
                                    if (err_acc.findFieldIndex(layout_env.idents.payload)) |inner_payload_idx| {
                                        const inner_payload_field = try err_acc.getFieldByIndex(inner_payload_idx);
                                        if (inner_payload_field.ptr) |str_ptr| {
                                            const str_dest: *RocStr = @ptrCast(@alignCast(str_ptr));
                                            str_dest.* = roc_str;
                                        }
                                    }
                                } else if (err_payload_layout.tag == .scalar and err_payload_layout.data.scalar.tag == .str) {
                                    // Direct Str payload (single-tag union optimized away)
                                    const str_dest: *RocStr = @ptrCast(@alignCast(outer_payload_ptr));
                                    str_dest.* = roc_str;
                                }
                            } else {
                                // Payload area is too small for RocStr - store the error message in the interpreter
                                // for retrieval by the caller. This happens when layout optimization doesn't
                                // allocate enough space for the Err payload.
                                // Note: Do NOT free msg here - it will be used and freed by the caller
                                self.last_error_message = msg;
                            }
                        }
                    }

                    return dest;
                } else if (result_layout.tag == .tuple) {
                    // Tuple (payload, tag) - tag unions are now represented as tuples
                    var dest = try self.pushRaw(result_layout, 0);
                    var result_acc = try dest.asTuple(&self.runtime_layout_store);

                    // Element 0 is payload, Element 1 is tag discriminant
                    // getElement takes original index directly

                    // Write tag discriminant (element 1)
                    const tag_field = try result_acc.getElement(1);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area (element 0)
                    const payload_field = try result_acc.getElement(0);
                    if (payload_field.ptr) |payload_ptr| {
                        const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                        if (payload_bytes_len > 0) {
                            const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                            @memset(bytes, 0);
                        }
                    }

                    // Write payload for Ok case
                    if (in_range and ok_payload_var != null) {
                        const num_layout = try self.getRuntimeLayout(ok_payload_var.?);
                        if (payload_field.ptr) |payload_ptr| {
                            if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .int) {
                                const int_type = num_layout.data.scalar.data.int;
                                if (is_negative) {
                                    // Write negative value
                                    switch (int_type) {
                                        .i8 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i16 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i32 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i64 => {
                                            const neg_value: i128 = -@as(i128, @intCast(value));
                                            @as(*i64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(neg_value);
                                        },
                                        .i128 => {
                                            const as_signed: i128 = @bitCast(value);
                                            const neg_value: i128 = -%as_signed;
                                            @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = neg_value;
                                        },
                                        else => {}, // Unsigned types already rejected above
                                    }
                                } else {
                                    // Write positive value
                                    switch (int_type) {
                                        .u8 => @as(*u8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i8 => @as(*i8, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u16 => @as(*u16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i16 => @as(*i16, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u32 => @as(*u32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i32 => @as(*i32, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u64 => @as(*u64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .i64 => @as(*i64, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                        .u128 => @as(*u128, @ptrCast(@alignCast(payload_ptr))).* = value,
                                        .i128 => @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = @intCast(value),
                                    }
                                }
                            } else if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .frac) {
                                // Floating-point and Dec types
                                const frac_precision = num_layout.data.scalar.data.frac;
                                const float_value: f64 = if (is_negative)
                                    -@as(f64, @floatFromInt(value))
                                else
                                    @as(f64, @floatFromInt(value));

                                // Handle fractional part for floats
                                var frac_part: f64 = 0;
                                if (digits_after.len > 0) {
                                    var mult: f64 = 1.0 / 256.0;
                                    for (digits_after) |digit| {
                                        frac_part += @as(f64, @floatFromInt(digit)) * mult;
                                        mult /= 256.0;
                                    }
                                }
                                const full_value = if (is_negative) float_value - frac_part else float_value + frac_part;

                                switch (frac_precision) {
                                    .f32 => @as(*f32, @ptrCast(@alignCast(payload_ptr))).* = @floatCast(full_value),
                                    .f64 => @as(*f64, @ptrCast(@alignCast(payload_ptr))).* = full_value,
                                    .dec => {
                                        const dec_value: i128 = if (is_negative)
                                            -@as(i128, @intCast(value)) * builtins.dec.RocDec.one_point_zero_i128
                                        else
                                            @as(i128, @intCast(value)) * builtins.dec.RocDec.one_point_zero_i128;
                                        @as(*i128, @ptrCast(@alignCast(payload_ptr))).* = dec_value;
                                    },
                                }
                            }
                        }
                    } else if (!in_range and err_payload_var != null) {
                        // For Err case, construct InvalidNumeral(Str) with descriptive message
                        var num_str_buf: [128]u8 = undefined;
                        const num_str = can.CIR.formatBase256ToDecimal(is_negative, digits_before, digits_after, &num_str_buf);

                        const error_msg = switch (rejection_reason) {
                            .negative_unsigned => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. {s} values cannot be negative.",
                                .{ num_str, type_name, type_name },
                            ) catch null,
                            .fractional_integer => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. {s} values must be whole numbers, not fractions.",
                                .{ num_str, type_name, type_name },
                            ) catch null,
                            .out_of_range, .overflow => std.fmt.allocPrint(
                                self.allocator,
                                "The number {s} is not a valid {s}. Valid {s} values are integers between {s} and {s}.",
                                .{ num_str, type_name, type_name, min_value_str, max_value_str },
                            ) catch null,
                            .none => null,
                        };

                        if (error_msg) |msg| {
                            // Store error message for retrieval by caller
                            // Note: Do NOT free msg here - it will be used and freed by the caller
                            self.last_error_message = msg;
                        }
                    }

                    return dest;
                }

                // Unsupported result layout is a compiler bug
                unreachable;
            },
            .dec_to_str => {
                // Dec.to_str : Dec -> Str
                std.debug.assert(args.len == 1); // expects 1 argument: Dec

                const dec_arg = args[0];
                // Null argument is a compiler bug - the compiler should never produce code with null args
                std.debug.assert(dec_arg.ptr != null);

                const roc_dec: *const RocDec = @ptrCast(@alignCast(dec_arg.ptr.?));
                const result_str = builtins.dec.to_str(roc_dec.*, roc_ops);

                const value = try self.pushStr("");
                const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                roc_str_ptr.* = result_str;
                return value;
            },
            .u8_to_str => return self.intToStr(u8, args, roc_ops),
            .i8_to_str => return self.intToStr(i8, args, roc_ops),
            .u16_to_str => return self.intToStr(u16, args, roc_ops),
            .i16_to_str => return self.intToStr(i16, args, roc_ops),
            .u32_to_str => return self.intToStr(u32, args, roc_ops),
            .i32_to_str => return self.intToStr(i32, args, roc_ops),
            .u64_to_str => return self.intToStr(u64, args, roc_ops),
            .i64_to_str => return self.intToStr(i64, args, roc_ops),
            .u128_to_str => return self.intToStr(u128, args, roc_ops),
            .i128_to_str => return self.intToStr(i128, args, roc_ops),
            .f32_to_str => return self.floatToStr(f32, args, roc_ops),
            .f64_to_str => return self.floatToStr(f64, args, roc_ops),

            // U8 conversion operations
            .u8_to_i8_wrap => return self.intConvertWrap(u8, i8, args, roc_ops),
            .u8_to_i8_try => return self.intConvertTry(u8, i8, args, roc_ops, return_rt_var),
            .u8_to_i16 => return self.intConvert(u8, i16, args, roc_ops),
            .u8_to_i32 => return self.intConvert(u8, i32, args, roc_ops),
            .u8_to_i64 => return self.intConvert(u8, i64, args, roc_ops),
            .u8_to_i128 => return self.intConvert(u8, i128, args, roc_ops),
            .u8_to_u16 => return self.intConvert(u8, u16, args, roc_ops),
            .u8_to_u32 => return self.intConvert(u8, u32, args, roc_ops),
            .u8_to_u64 => return self.intConvert(u8, u64, args, roc_ops),
            .u8_to_u128 => return self.intConvert(u8, u128, args, roc_ops),
            .u8_to_f32 => return self.intToFloat(u8, f32, args, roc_ops),
            .u8_to_f64 => return self.intToFloat(u8, f64, args, roc_ops),
            .u8_to_dec => return self.intToDec(u8, args, roc_ops),

            // I8 conversion operations
            .i8_to_i16 => return self.intConvert(i8, i16, args, roc_ops),
            .i8_to_i32 => return self.intConvert(i8, i32, args, roc_ops),
            .i8_to_i64 => return self.intConvert(i8, i64, args, roc_ops),
            .i8_to_i128 => return self.intConvert(i8, i128, args, roc_ops),
            .i8_to_u8_wrap => return self.intConvertWrap(i8, u8, args, roc_ops),
            .i8_to_u8_try => return self.intConvertTry(i8, u8, args, roc_ops, return_rt_var),
            .i8_to_u16_wrap => return self.intConvertWrap(i8, u16, args, roc_ops),
            .i8_to_u16_try => return self.intConvertTry(i8, u16, args, roc_ops, return_rt_var),
            .i8_to_u32_wrap => return self.intConvertWrap(i8, u32, args, roc_ops),
            .i8_to_u32_try => return self.intConvertTry(i8, u32, args, roc_ops, return_rt_var),
            .i8_to_u64_wrap => return self.intConvertWrap(i8, u64, args, roc_ops),
            .i8_to_u64_try => return self.intConvertTry(i8, u64, args, roc_ops, return_rt_var),
            .i8_to_u128_wrap => return self.intConvertWrap(i8, u128, args, roc_ops),
            .i8_to_u128_try => return self.intConvertTry(i8, u128, args, roc_ops, return_rt_var),
            .i8_to_f32 => return self.intToFloat(i8, f32, args, roc_ops),
            .i8_to_f64 => return self.intToFloat(i8, f64, args, roc_ops),
            .i8_to_dec => return self.intToDec(i8, args, roc_ops),
        }
    }

    /// Helper to create a simple boolean StackValue (for low-level builtins)
    fn makeBoolValue(self: *Interpreter, value: bool) !StackValue {
        const bool_layout = Layout.int(.u8);
        var bool_value = try self.pushRaw(bool_layout, 0);
        bool_value.is_initialized = false;
        try bool_value.setInt(@intFromBool(value));
        bool_value.is_initialized = true;
        // Store the Bool runtime type variable for constant folding
        bool_value.rt_var = try self.getCanonicalBoolRuntimeVar();
        return bool_value;
    }

    /// Helper for integer to_str operations
    fn intToStr(self: *Interpreter, comptime T: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const int_value: T = @as(*const T, @ptrCast(@alignCast(int_arg.ptr.?))).*;

        // Use std.fmt to format the integer
        var buf: [40]u8 = undefined; // 40 is enough for i128
        const result = std.fmt.bufPrint(&buf, "{}", .{int_value}) catch unreachable;

        const value = try self.pushStr("");
        const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
        roc_str_ptr.* = RocStr.init(&buf, result.len, roc_ops);
        return value;
    }

    /// Helper for float to_str operations
    fn floatToStr(self: *Interpreter, comptime T: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(float_arg.ptr != null);

        const float_value: T = @as(*const T, @ptrCast(@alignCast(float_arg.ptr.?))).*;

        // Use std.fmt to format the float
        var buf: [400]u8 = undefined;
        const result = std.fmt.bufPrint(&buf, "{d}", .{float_value}) catch unreachable;

        const value = try self.pushStr("");
        const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
        roc_str_ptr.* = RocStr.init(&buf, result.len, roc_ops);
        return value;
    }

    /// Helper for safe integer conversions (widening)
    fn intConvert(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        _ = roc_ops;
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const to_value: To = @intCast(from_value);

        const to_layout = Layout.int(comptime intTypeFromZigType(To));
        var out = try self.pushRaw(to_layout, 0);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for wrapping integer conversions (potentially lossy)
    fn intConvertWrap(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        _ = roc_ops;
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        // For wrapping conversion:
        // - Same size: bitCast (reinterpret bits)
        // - Narrowing: truncate then bitCast
        // - Widening signed to unsigned: sign-extend to wider signed first, then bitCast to unsigned (so -1i8 -> -1i16 -> 65535u16)
        // - Widening unsigned to any: zero-extend
        const to_value: To = if (@bitSizeOf(From) == @bitSizeOf(To))
            @bitCast(from_value)
        else if (@bitSizeOf(From) > @bitSizeOf(To))
            // Narrowing: truncate bits
            @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(To)), @truncate(@as(std.meta.Int(.unsigned, @bitSizeOf(From)), @bitCast(from_value)))))
        else if (@typeInfo(From).int.signedness == .signed and @typeInfo(To).int.signedness == .unsigned)
            // Widening from signed to unsigned: sign-extend to wider signed first, then bitCast to unsigned
            // e.g., -1i8 -> -1i16 -> 65535u16
            @bitCast(@as(std.meta.Int(.signed, @bitSizeOf(To)), from_value))
        else
            // Widening (signed to signed, or unsigned to any): use standard int cast
            @intCast(from_value);

        const to_layout = Layout.int(comptime intTypeFromZigType(To));
        var out = try self.pushRaw(to_layout, 0);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for try integer conversions (returns Try(To, [OutOfRange]))
    fn intConvertTry(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue, roc_ops: *RocOps, return_rt_var: ?types.Var) !StackValue {
        _ = roc_ops;
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        // Return type info is required - missing it is a compiler bug
        const result_rt_var = return_rt_var orelse unreachable;

        const result_layout = try self.getRuntimeLayout(result_rt_var);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;

        // Check if conversion is in range
        const in_range = std.math.cast(To, from_value) != null;

        // Resolve the Try type to get Ok's payload type
        const resolved = self.resolveBaseVar(result_rt_var);
        // Type system should guarantee this is a tag union - if not, it's a compiler bug
        std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .tag_union);

        // Find tag indices for Ok and Err
        var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.appendUnionTags(result_rt_var, &tag_list);

        var ok_index: ?usize = null;
        var err_index: ?usize = null;

        const ok_ident = self.env.idents.ok;
        const err_ident = self.env.idents.err;

        for (tag_list.items, 0..) |tag_info, i| {
            if (tag_info.name == ok_ident) {
                ok_index = i;
            } else if (tag_info.name == err_ident) {
                err_index = i;
            }
        }

        // Construct the result tag union
        if (result_layout.tag == .scalar) {
            // Simple tag with no payload (shouldn't happen for Try with payload)
            var out = try self.pushRaw(result_layout, 0);
            out.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try out.setInt(@intCast(tag_idx));
            out.is_initialized = true;
            return out;
        } else if (result_layout.tag == .record) {
            // Record { tag, payload }
            var dest = try self.pushRaw(result_layout, 0);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
            const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse unreachable;
            const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse unreachable;

            // Write tag discriminant
            const tag_field = try acc.getFieldByIndex(tag_field_idx);
            // Tag field should be scalar int - if not, it's a compiler bug
            std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
            var tmp = tag_field;
            tmp.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try tmp.setInt(@intCast(tag_idx));

            // Clear payload area
            const payload_field = try acc.getFieldByIndex(payload_field_idx);
            if (payload_field.ptr) |payload_ptr| {
                const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                if (payload_bytes_len > 0) {
                    const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                    @memset(bytes, 0);
                }
            }

            // Write payload for Ok case
            if (in_range) {
                const to_value: To = @intCast(from_value);
                if (payload_field.ptr) |payload_ptr| {
                    @as(*To, @ptrCast(@alignCast(payload_ptr))).* = to_value;
                }
            }
            // For Err case, payload is OutOfRange which is a zero-arg tag (already zeroed)

            return dest;
        } else if (result_layout.tag == .tuple) {
            // Tuple (payload, tag) - tag unions are now represented as tuples
            var dest = try self.pushRaw(result_layout, 0);
            var result_acc = try dest.asTuple(&self.runtime_layout_store);

            // Element 0 is payload, Element 1 is tag discriminant

            // Write tag discriminant (element 1)
            const tag_field = try result_acc.getElement(1);
            // Tag field should be scalar int - if not, it's a compiler bug
            std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
            var tmp = tag_field;
            tmp.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try tmp.setInt(@intCast(tag_idx));

            // Clear payload area (element 0)
            const payload_field = try result_acc.getElement(0);
            if (payload_field.ptr) |payload_ptr| {
                const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                if (payload_bytes_len > 0) {
                    const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                    @memset(bytes, 0);
                }
            }

            // Write payload for Ok case
            if (in_range) {
                const to_value: To = @intCast(from_value);
                if (payload_field.ptr) |payload_ptr| {
                    @as(*To, @ptrCast(@alignCast(payload_ptr))).* = to_value;
                }
            }
            // For Err case, payload is OutOfRange which is a zero-arg tag (already zeroed)

            return dest;
        }

        // Unsupported result layout is a compiler bug
        unreachable;
    }

    /// Helper for integer to float conversions
    fn intToFloat(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        _ = roc_ops;
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const to_value: To = @floatFromInt(from_value);

        const to_layout = Layout.frac(comptime fracTypeFromZigType(To));
        var out = try self.pushRaw(to_layout, 0);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for integer to Dec conversions
    fn intToDec(self: *Interpreter, comptime From: type, args: []const StackValue, roc_ops: *RocOps) !StackValue {
        _ = roc_ops;
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const dec_value = RocDec{ .num = @as(i128, from_value) * RocDec.one_point_zero_i128 };

        const dec_layout = Layout.frac(.dec);
        var out = try self.pushRaw(dec_layout, 0);
        out.is_initialized = false;
        @as(*RocDec, @ptrCast(@alignCast(out.ptr.?))).* = dec_value;
        out.is_initialized = true;
        return out;
    }

    /// Convert Zig integer type to types.Int.Precision
    fn intTypeFromZigType(comptime T: type) types.Int.Precision {
        return switch (T) {
            u8 => .u8,
            i8 => .i8,
            u16 => .u16,
            i16 => .i16,
            u32 => .u32,
            i32 => .i32,
            u64 => .u64,
            i64 => .i64,
            u128 => .u128,
            i128 => .i128,
            else => @compileError("Unsupported integer type"),
        };
    }

    /// Convert Zig float type to types.Frac.Precision
    fn fracTypeFromZigType(comptime T: type) types.Frac.Precision {
        return switch (T) {
            f32 => .f32,
            f64 => .f64,
            else => @compileError("Unsupported float type"),
        };
    }

    fn triggerCrash(self: *Interpreter, message: []const u8, owned: bool, roc_ops: *RocOps) void {
        defer if (owned) self.allocator.free(@constCast(message));
        roc_ops.crash(message);
    }

    fn handleExpectFailure(self: *Interpreter, snippet_expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) void {
        const region = self.env.store.getExprRegion(snippet_expr_idx);
        const source_bytes = self.env.getSource(region);

        // Pass raw source bytes to the host - let the host handle trimming and formatting
        const expect_args = RocExpectFailed{
            .utf8_bytes = @constCast(source_bytes.ptr),
            .len = source_bytes.len,
        };
        roc_ops.roc_expect_failed(&expect_args, roc_ops.env);

        // Also pass raw source bytes to crash - host handles formatting
        roc_ops.crash(source_bytes);
    }

    fn getRuntimeU8(value: StackValue) u8 {
        std.debug.assert(value.layout.tag == .scalar);
        std.debug.assert(value.layout.data.scalar.tag == .int);
        std.debug.assert(value.layout.data.scalar.data.int == .u8);

        const ptr = value.ptr orelse unreachable;
        const b: *const u8 = @ptrCast(@alignCast(ptr));

        return b.*;
    }

    fn boolValueEquals(equals: bool, value: StackValue) bool {
        // Bools are u8 scalars
        std.debug.assert(value.layout.tag == .scalar);
        std.debug.assert(value.layout.data.scalar.tag == .int);
        std.debug.assert(value.layout.data.scalar.data.int == .u8);
        const ptr = value.ptr orelse unreachable;
        const bool_byte = @as(*const u8, @ptrCast(@alignCast(ptr))).*;
        return (bool_byte != 0) == equals;
    }

    const NumericKind = enum { int, dec, f32, f64 };

    fn numericKindFromLayout(layout_val: Layout) ?NumericKind {
        if (layout_val.tag != .scalar) return null;
        return switch (layout_val.data.scalar.tag) {
            .int => .int,
            .frac => switch (layout_val.data.scalar.data.frac) {
                .dec => .dec,
                .f32 => .f32,
                .f64 => .f64,
            },
            else => null,
        };
    }

    fn unifyNumericKinds(lhs: NumericKind, rhs: NumericKind) ?NumericKind {
        if (lhs == rhs) return lhs;
        if (lhs == .int) return rhs;
        if (rhs == .int) return lhs;
        return null;
    }

    fn layoutMatchesKind(self: *Interpreter, layout_val: Layout, kind: NumericKind) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return switch (kind) {
            .int => layout_val.data.scalar.tag == .int,
            .dec => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .dec,
            .f32 => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .f32,
            .f64 => layout_val.data.scalar.tag == .frac and layout_val.data.scalar.data.frac == .f64,
        };
    }

    fn invalidateRuntimeLayoutCache(self: *Interpreter, type_var: types.Var) void {
        const slot_idx = @intFromEnum(type_var);
        if (slot_idx < self.var_to_layout_slot.items.len) {
            self.var_to_layout_slot.items[slot_idx] = 0;
        }

        const resolved = self.runtime_types.resolveVar(type_var);
        const map_idx = @intFromEnum(resolved.var_);
        if (map_idx < self.runtime_layout_store.layouts_by_var.entries.len) {
            self.runtime_layout_store.layouts_by_var.entries[map_idx] = layout.Idx.none;
        }
    }

    fn adjustNumericResultLayout(
        self: *Interpreter,
        result_rt_var: types.Var,
        current_layout: Layout,
        lhs: StackValue,
        lhs_rt_var: types.Var,
        rhs: StackValue,
        rhs_rt_var: types.Var,
    ) !Layout {
        const lhs_kind_opt = numericKindFromLayout(lhs.layout);
        const rhs_kind_opt = numericKindFromLayout(rhs.layout);
        if (lhs_kind_opt == null or rhs_kind_opt == null) return current_layout;

        const desired_kind = unifyNumericKinds(lhs_kind_opt.?, rhs_kind_opt.?) orelse return error.TypeMismatch;

        if (self.layoutMatchesKind(current_layout, desired_kind)) return current_layout;

        const source = blk: {
            if (self.layoutMatchesKind(lhs.layout, desired_kind)) break :blk lhs_rt_var;
            if (self.layoutMatchesKind(rhs.layout, desired_kind)) break :blk rhs_rt_var;
            return error.TypeMismatch;
        };

        const source_resolved = self.runtime_types.resolveVar(source);
        try self.runtime_types.setVarContent(result_rt_var, source_resolved.desc.content);
        self.invalidateRuntimeLayoutCache(result_rt_var);

        const updated_layout = try self.getRuntimeLayout(result_rt_var);
        if (!self.layoutMatchesKind(updated_layout, desired_kind)) return error.TypeMismatch;
        return updated_layout;
    }

    fn evalDecBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
        roc_ops: *RocOps,
    ) !StackValue {
        const lhs_dec = try self.stackValueToDecimal(lhs);
        const rhs_dec = try self.stackValueToDecimal(rhs);

        const result_dec: RocDec = switch (op) {
            .add => RocDec.add(lhs_dec, rhs_dec, roc_ops),
            .sub => RocDec.sub(lhs_dec, rhs_dec, roc_ops),
            .mul => RocDec.mul(lhs_dec, rhs_dec, roc_ops),
            .div, .div_trunc => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                break :blk RocDec.div(lhs_dec, rhs_dec, roc_ops);
            },
            .rem => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                break :blk RocDec.rem(lhs_dec, rhs_dec, roc_ops);
            },
            else => @panic("evalDecBinop: unhandled decimal operation"),
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;
        if (out.ptr) |ptr| {
            const dest: *RocDec = @ptrCast(@alignCast(ptr));
            dest.* = result_dec;
        }
        return out;
    }

    fn evalFloatBinop(
        self: *Interpreter,
        comptime FloatT: type,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        const lhs_float = try self.stackValueToFloat(FloatT, lhs);
        const rhs_float = try self.stackValueToFloat(FloatT, rhs);

        const result_float: FloatT = switch (op) {
            .add => lhs_float + rhs_float,
            .sub => lhs_float - rhs_float,
            .mul => lhs_float * rhs_float,
            .div => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                break :blk lhs_float / rhs_float;
            },
            .div_trunc => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                const quotient = lhs_float / rhs_float;
                break :blk std.math.trunc(quotient);
            },
            .rem => blk: {
                if (rhs_float == 0) return error.DivisionByZero;
                break :blk @rem(lhs_float, rhs_float);
            },
            else => @panic("evalFloatBinop: unhandled float operation"),
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;
        if (out.ptr) |ptr| {
            const dest: *FloatT = @ptrCast(@alignCast(ptr));
            dest.* = result_float;
        }
        return out;
    }

    fn stackValueToDecimal(self: *Interpreter, value: StackValue) !RocDec {
        _ = self;
        if (value.layout.tag != .scalar) return error.TypeMismatch;
        switch (value.layout.data.scalar.tag) {
            .frac => switch (value.layout.data.scalar.data.frac) {
                .dec => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const dec_ptr: *const RocDec = @ptrCast(@alignCast(ptr));
                    return dec_ptr.*;
                },
                else => return error.TypeMismatch,
            },
            .int => {
                return RocDec{ .num = value.asI128() * RocDec.one_point_zero_i128 };
            },
            else => return error.TypeMismatch,
        }
    }

    fn stackValueToFloat(self: *Interpreter, comptime FloatT: type, value: StackValue) !FloatT {
        _ = self;
        if (value.layout.tag != .scalar) return error.TypeMismatch;
        switch (value.layout.data.scalar.tag) {
            .int => {
                return @floatFromInt(value.asI128());
            },
            .frac => switch (value.layout.data.scalar.data.frac) {
                .f32 => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const val_ptr: *const f32 = @ptrCast(@alignCast(ptr));
                    if (FloatT == f32) {
                        return val_ptr.*;
                    }
                    return @floatCast(val_ptr.*);
                },
                .f64 => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const val_ptr: *const f64 = @ptrCast(@alignCast(ptr));
                    if (FloatT == f64) {
                        return val_ptr.*;
                    }
                    return @floatCast(val_ptr.*);
                },
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        }
    }

    fn debugAssertIsBoolLayout(layout_val: Layout) void {
        std.debug.assert(layout_val.tag == .scalar);
        std.debug.assert(layout_val.data.scalar.tag == .int);
        std.debug.assert(layout_val.data.scalar.data.int == .u8);
    }

    const NumericValue = union(enum) {
        int: i128,
        f32: f32,
        f64: f64,
        dec: RocDec,
    };

    fn isNumericScalar(self: *Interpreter, layout_val: Layout) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return switch (layout_val.data.scalar.tag) {
            .int, .frac => true,
            else => false,
        };
    }

    fn extractNumericValue(self: *Interpreter, value: StackValue) !NumericValue {
        _ = self;
        if (value.layout.tag != .scalar) return error.NotNumeric;
        const scalar = value.layout.data.scalar;
        return switch (scalar.tag) {
            .int => NumericValue{ .int = value.asI128() },
            .frac => switch (scalar.data.frac) {
                .f32 => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const f32, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .f32 = ptr.* };
                },
                .f64 => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const f64, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .f64 = ptr.* };
                },
                .dec => {
                    const raw_ptr = value.ptr orelse return error.TypeMismatch;
                    const ptr = @as(*const RocDec, @ptrCast(@alignCast(raw_ptr)));
                    return NumericValue{ .dec = ptr.* };
                },
            },
            else => error.NotNumeric,
        };
    }

    fn compareNumericScalars(self: *Interpreter, lhs: StackValue, rhs: StackValue) !std.math.Order {
        const lhs_value = try self.extractNumericValue(lhs);
        const rhs_value = try self.extractNumericValue(rhs);
        return self.orderNumericValues(lhs_value, rhs_value);
    }

    fn orderNumericValues(self: *Interpreter, lhs: NumericValue, rhs: NumericValue) !std.math.Order {
        return switch (lhs) {
            .int => self.orderInt(lhs.int, rhs),
            .f32 => self.orderF32(lhs.f32, rhs),
            .f64 => self.orderF64(lhs.f64, rhs),
            .dec => self.orderDec(lhs.dec, rhs),
        };
    }

    fn orderInt(self: *Interpreter, lhs: i128, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => std.math.order(lhs, rhs.int),
            .f32 => {
                const lhs_f: f32 = @floatFromInt(lhs);
                return std.math.order(lhs_f, rhs.f32);
            },
            .f64 => {
                const lhs_f: f64 = @floatFromInt(lhs);
                return std.math.order(lhs_f, rhs.f64);
            },
            .dec => {
                const lhs_dec = lhs * RocDec.one_point_zero_i128;
                return std.math.order(lhs_dec, rhs.dec.num);
            },
        };
    }

    fn orderF32(self: *Interpreter, lhs: f32, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_f: f32 = @floatFromInt(rhs.int);
                return std.math.order(lhs, rhs_f);
            },
            .f32 => std.math.order(lhs, rhs.f32),
            .f64 => {
                const lhs_f64: f64 = @as(f64, @floatCast(lhs));
                return std.math.order(lhs_f64, rhs.f64);
            },
            .dec => return error.TypeMismatch,
        };
    }

    fn orderF64(self: *Interpreter, lhs: f64, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_f: f64 = @floatFromInt(rhs.int);
                return std.math.order(lhs, rhs_f);
            },
            .f32 => {
                const rhs_f64: f64 = @as(f64, @floatCast(rhs.f32));
                return std.math.order(lhs, rhs_f64);
            },
            .f64 => std.math.order(lhs, rhs.f64),
            .dec => return error.TypeMismatch,
        };
    }

    fn orderDec(self: *Interpreter, lhs: RocDec, rhs: NumericValue) !std.math.Order {
        _ = self;
        return switch (rhs) {
            .int => {
                const rhs_dec = rhs.int * RocDec.one_point_zero_i128;
                return std.math.order(lhs.num, rhs_dec);
            },
            .dec => std.math.order(lhs.num, rhs.dec.num),
            else => return error.TypeMismatch,
        };
    }

    const StructuralEqError = Error;

    fn valuesStructurallyEqual(
        self: *Interpreter,
        lhs: StackValue,
        lhs_var: types.Var,
        rhs: StackValue,
        _: types.Var, // rhs_var unused
    ) StructuralEqError!bool {
        // Handle scalar comparisons (numbers, strings) directly.
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const lhs_scalar = lhs.layout.data.scalar;
            const rhs_scalar = rhs.layout.data.scalar;
            if (lhs_scalar.tag != rhs_scalar.tag) return error.TypeMismatch;

            switch (lhs_scalar.tag) {
                .int, .frac => {
                    const order = try self.compareNumericScalars(lhs, rhs);
                    return order == .eq;
                },
                .str => {
                    if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;
                    const lhs_str: *const RocStr = @ptrCast(@alignCast(lhs.ptr.?));
                    const rhs_str: *const RocStr = @ptrCast(@alignCast(rhs.ptr.?));
                    return lhs_str.eql(rhs_str.*);
                },
                else => @panic("valuesStructurallyEqual: unhandled scalar type"),
            }
        }

        // Ensure runtime vars resolve to the same descriptor before structural comparison.
        const lhs_resolved = self.resolveBaseVar(lhs_var);
        const lhs_content = lhs_resolved.desc.content;
        if (lhs_content != .structure) @panic("valuesStructurallyEqual: lhs is not a structure type");

        return switch (lhs_content.structure) {
            .tuple => |tuple| {
                const elem_vars = self.runtime_types.sliceVars(tuple.elems);
                return try self.structuralEqualTuple(lhs, rhs, elem_vars);
            },
            .record => |record| {
                return try self.structuralEqualRecord(lhs, rhs, record);
            },
            .tag_union => {
                return try self.structuralEqualTag(lhs, rhs, lhs_var);
            },
            .empty_record => true,
            .empty_tag_union => true,
            .nominal_type => |nom| {
                // For nominal types, dispatch to their is_eq method
                return try self.dispatchNominalIsEq(lhs, rhs, nom, lhs_var);
            },
            .record_unbound, .fn_pure, .fn_effectful, .fn_unbound => @panic("valuesStructurallyEqual: cannot compare functions or unbound records"),
        };
    }

    fn structuralEqualTuple(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        elem_vars: []const types.Var,
    ) StructuralEqError!bool {
        if (lhs.layout.tag != .tuple or rhs.layout.tag != .tuple) return error.TypeMismatch;
        if (elem_vars.len == 0) return true;

        const lhs_size = self.runtime_layout_store.layoutSize(lhs.layout);
        const rhs_size = self.runtime_layout_store.layoutSize(rhs.layout);
        if (lhs_size == 0 and rhs_size == 0) return true;
        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        var lhs_acc = try lhs.asTuple(&self.runtime_layout_store);
        var rhs_acc = try rhs.asTuple(&self.runtime_layout_store);
        if (lhs_acc.getElementCount() != elem_vars.len or rhs_acc.getElementCount() != elem_vars.len) {
            return error.TypeMismatch;
        }

        var index: usize = 0;
        while (index < elem_vars.len) : (index += 1) {
            // getElement expects original index and converts to sorted internally
            const lhs_elem = try lhs_acc.getElement(index);
            const rhs_elem = try rhs_acc.getElement(index);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_vars[index], rhs_elem, elem_vars[index]);
            if (!elems_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualRecord(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        record: types.Record,
    ) StructuralEqError!bool {
        if (lhs.layout.tag != .record or rhs.layout.tag != .record) return error.TypeMismatch;

        if (@intFromEnum(record.ext) != 0) {
            const ext_resolved = self.resolveBaseVar(record.ext);
            if (ext_resolved.desc.content != .structure or ext_resolved.desc.content.structure != .empty_record) {
                @panic("structuralEqualRecord: record extension is not empty_record");
            }
        }

        const field_count = record.fields.len();
        if (field_count == 0) return true;

        const field_slice = self.runtime_types.getRecordFieldsSlice(record.fields);

        const lhs_size = self.runtime_layout_store.layoutSize(lhs.layout);
        const rhs_size = self.runtime_layout_store.layoutSize(rhs.layout);
        if ((lhs_size == 0 or lhs.ptr == null) and (rhs_size == 0 or rhs.ptr == null)) {
            var idx: usize = 0;
            while (idx < field_count) : (idx += 1) {
                const field_var = field_slice.items(.var_)[idx];
                const field_layout = try self.getRuntimeLayout(field_var);
                if (self.runtime_layout_store.layoutSize(field_layout) != 0) return error.TypeMismatch;
            }
            return true;
        }

        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        var lhs_rec = try lhs.asRecord(&self.runtime_layout_store);
        var rhs_rec = try rhs.asRecord(&self.runtime_layout_store);
        if (lhs_rec.getFieldCount() != field_count or rhs_rec.getFieldCount() != field_count) {
            return error.TypeMismatch;
        }

        var idx: usize = 0;
        while (idx < field_count) : (idx += 1) {
            const lhs_field = try lhs_rec.getFieldByIndex(idx);
            const rhs_field = try rhs_rec.getFieldByIndex(idx);
            const field_var = field_slice.items(.var_)[idx];
            const fields_equal = try self.valuesStructurallyEqual(lhs_field, field_var, rhs_field, field_var);
            if (!fields_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualList(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        elem_var: types.Var,
    ) StructuralEqError!bool {
        const lhs_is_list = lhs.layout.tag == .list or lhs.layout.tag == .list_of_zst;
        const rhs_is_list = rhs.layout.tag == .list or rhs.layout.tag == .list_of_zst;
        if (!lhs_is_list or !rhs_is_list) return error.TypeMismatch;
        if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;

        const lhs_header = @as(*const RocList, @ptrCast(@alignCast(lhs.ptr.?))).*;
        const rhs_header = @as(*const RocList, @ptrCast(@alignCast(rhs.ptr.?))).*;
        if (lhs_header.len() != rhs_header.len()) return false;

        const elem_layout = try self.getRuntimeLayout(elem_var);
        const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
        if (elem_size == 0 or lhs_header.len() == 0) {
            return true;
        }

        var lhs_acc = try lhs.asList(&self.runtime_layout_store, elem_layout);
        var rhs_acc = try rhs.asList(&self.runtime_layout_store, elem_layout);

        var index: usize = 0;
        while (index < lhs_header.len()) : (index += 1) {
            const lhs_elem = try lhs_acc.getElement(index);
            const rhs_elem = try rhs_acc.getElement(index);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_var, rhs_elem, elem_var);
            if (!elems_equal) {
                return false;
            }
        }

        return true;
    }

    fn structuralEqualTag(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        union_var: types.Var,
    ) StructuralEqError!bool {
        var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.appendUnionTags(union_var, &tag_list);

        const lhs_data = try self.extractTagValue(lhs, union_var);
        const rhs_data = try self.extractTagValue(rhs, union_var);

        if (lhs_data.index >= tag_list.items.len or rhs_data.index >= tag_list.items.len) {
            return error.TypeMismatch;
        }

        if (lhs_data.index != rhs_data.index) return false;

        const tag_info = tag_list.items[lhs_data.index];
        const arg_vars = self.runtime_types.sliceVars(tag_info.args);
        if (arg_vars.len == 0) return true;

        if (arg_vars.len == 1) {
            const lhs_payload = lhs_data.payload orelse return error.TypeMismatch;
            const rhs_payload = rhs_data.payload orelse return error.TypeMismatch;
            return try self.valuesStructurallyEqual(lhs_payload, arg_vars[0], rhs_payload, arg_vars[0]);
        }

        const lhs_payload = lhs_data.payload orelse return error.TypeMismatch;
        const rhs_payload = rhs_data.payload orelse return error.TypeMismatch;
        if (lhs_payload.layout.tag != .tuple or rhs_payload.layout.tag != .tuple) return error.TypeMismatch;

        var lhs_tuple = try lhs_payload.asTuple(&self.runtime_layout_store);
        var rhs_tuple = try rhs_payload.asTuple(&self.runtime_layout_store);
        if (lhs_tuple.getElementCount() != arg_vars.len or rhs_tuple.getElementCount() != arg_vars.len) {
            return error.TypeMismatch;
        }

        var idx: usize = 0;
        while (idx < arg_vars.len) : (idx += 1) {
            // getElement expects original index and converts to sorted internally
            const lhs_elem = try lhs_tuple.getElement(idx);
            const rhs_elem = try rhs_tuple.getElement(idx);
            const args_equal = try self.valuesStructurallyEqual(lhs_elem, arg_vars[idx], rhs_elem, arg_vars[idx]);
            if (!args_equal) {
                return false;
            }
        }

        return true;
    }

    /// Dispatch is_eq method call for a nominal type
    fn dispatchNominalIsEq(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        nom: types.NominalType,
        lhs_var: types.Var,
    ) StructuralEqError!bool {
        _ = lhs_var;

        // Check if this is a simple scalar comparison (numbers, bools represented as scalars)
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const lhs_scalar = lhs.layout.data.scalar;
            const rhs_scalar = rhs.layout.data.scalar;
            if (lhs_scalar.tag != rhs_scalar.tag) {
                // Different scalar types can't be equal
                return false;
            }
            return switch (lhs_scalar.tag) {
                .int, .frac => blk: {
                    const order = self.compareNumericScalars(lhs, rhs) catch @panic("dispatchNominalIsEq: failed to compare scalars");
                    break :blk order == .eq;
                },
                .str => blk: {
                    if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;
                    const lhs_str: *const RocStr = @ptrCast(@alignCast(lhs.ptr.?));
                    const rhs_str: *const RocStr = @ptrCast(@alignCast(rhs.ptr.?));
                    break :blk lhs_str.eql(rhs_str.*);
                },
                .opaque_ptr => blk: {
                    // Opaque pointer comparison - compare the pointer values directly
                    if (lhs.ptr == null and rhs.ptr == null) break :blk true;
                    if (lhs.ptr == null or rhs.ptr == null) break :blk false;
                    break :blk lhs.ptr.? == rhs.ptr.?;
                },
            };
        }

        // Structural comparison of the backing type
        // This handles nominal types like Try that wrap tag unions
        const backing_var = self.runtime_types.getNominalBackingVar(nom);
        const backing_resolved = self.runtime_types.resolveVar(backing_var);

        if (backing_resolved.desc.content == .structure) {
            return self.valuesStructurallyEqual(lhs, backing_var, rhs, backing_var);
        }

        // For other cases, fall back to attempting scalar comparison
        // This handles cases like Bool which wraps a tag union but is represented as a scalar
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const order = self.compareNumericScalars(lhs, rhs) catch @panic("dispatchNominalIsEq: failed to compare scalars");
            return order == .eq;
        }

        // Can't compare - likely a user-defined nominal type that needs is_eq dispatch
        // TODO: Implement proper method dispatch by looking up is_eq in the nominal type's module
        _ = lhs_var;
        @panic("dispatchNominalIsEq: cannot compare non-scalar nominal types without is_eq method");
    }

    pub fn getCanonicalBoolRuntimeVar(self: *Interpreter) !types.Var {
        if (self.canonical_bool_rt_var) |cached| return cached;
        // Use the dynamic bool_stmt index (from the Bool module)
        const bool_decl_idx = self.builtins.bool_stmt;

        // Get the statement from the Bool module
        const bool_stmt = self.builtins.bool_env.store.getStatement(bool_decl_idx);

        // For nominal type declarations, we need to get the backing type, not the nominal wrapper
        const ct_var = switch (bool_stmt) {
            .s_nominal_decl => blk: {
                // The type of the declaration is the nominal type, but we want its backing
                const nom_var = can.ModuleEnv.varFrom(bool_decl_idx);
                const nom_resolved = self.builtins.bool_env.types.resolveVar(nom_var);
                if (nom_resolved.desc.content == .structure) {
                    if (nom_resolved.desc.content.structure == .nominal_type) {
                        const nt = nom_resolved.desc.content.structure.nominal_type;
                        const backing_var = self.builtins.bool_env.types.getNominalBackingVar(nt);
                        break :blk backing_var;
                    }
                }
                break :blk nom_var;
            },
            else => can.ModuleEnv.varFrom(bool_decl_idx),
        };

        // Use bool_env to translate since bool_stmt is from the Bool module
        // Cast away const - translateTypeVar doesn't actually mutate the module
        const nominal_rt_var = try self.translateTypeVar(@constCast(self.builtins.bool_env), ct_var);
        const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
        const backing_rt_var = switch (nominal_resolved.desc.content) {
            .structure => |st| switch (st) {
                .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                .tag_union => nominal_rt_var,
                else => nominal_rt_var,
            },
            else => nominal_rt_var,
        };
        self.canonical_bool_rt_var = backing_rt_var;
        return backing_rt_var;
    }

    fn resolveBaseVar(self: *Interpreter, runtime_var: types.Var) types.store.ResolvedVarDesc {
        var current = self.runtime_types.resolveVar(runtime_var);
        while (true) {
            switch (current.desc.content) {
                .alias => |al| {
                    const backing = self.runtime_types.getAliasBackingVar(al);
                    current = self.runtime_types.resolveVar(backing);
                },
                .structure => |st| switch (st) {
                    .nominal_type => |nom| {
                        const backing = self.runtime_types.getNominalBackingVar(nom);
                        current = self.runtime_types.resolveVar(backing);
                    },
                    else => return current,
                },
                else => return current,
            }
        }
    }

    pub fn appendUnionTags(self: *Interpreter, runtime_var: types.Var, list: *std.array_list.AlignedManaged(types.Tag, null)) !void {
        var var_stack = try std.array_list.AlignedManaged(types.Var, null).initCapacity(self.allocator, 4);
        defer var_stack.deinit();
        try var_stack.append(runtime_var);

        while (var_stack.items.len > 0) {
            const current_var = var_stack.pop().?;
            var resolved = self.runtime_types.resolveVar(current_var);
            expand: while (true) {
                switch (resolved.desc.content) {
                    .alias => |al| {
                        const backing = self.runtime_types.getAliasBackingVar(al);
                        resolved = self.runtime_types.resolveVar(backing);
                        continue :expand;
                    },
                    .structure => |flat| switch (flat) {
                        .nominal_type => |nom| {
                            const backing = self.runtime_types.getNominalBackingVar(nom);
                            resolved = self.runtime_types.resolveVar(backing);
                            continue :expand;
                        },
                        .tag_union => |tu| {
                            const tags_slice = self.runtime_types.getTagsSlice(tu.tags);
                            for (tags_slice.items(.name), tags_slice.items(.args)) |name_idx, args_range| {
                                try list.append(.{ .name = name_idx, .args = args_range });
                            }
                            const ext_var = tu.ext;
                            if (@intFromEnum(ext_var) != 0) {
                                const ext_resolved = self.runtime_types.resolveVar(ext_var);
                                if (!(ext_resolved.desc.content == .structure and ext_resolved.desc.content.structure == .empty_tag_union)) {
                                    try var_stack.append(ext_var);
                                }
                            }
                        },
                        .empty_tag_union => {},
                        else => {},
                    },
                    else => {},
                }
                break :expand;
            }
        }

        // Sort the tags alphabetically to match gatherTags and layout store ordering
        // This ensures tag discriminants are consistent between evaluation and rendering
        // Use runtime_layout_store.env since runtime type tag names are translated to that env
        std.mem.sort(types.Tag, list.items, self.runtime_layout_store.env.common.getIdentStore(), comptime types.Tag.sortByNameAsc);
    }

    /// Find the index of a tag in a runtime tag union by translating the source tag name ident.
    /// This avoids string comparison by translating the source ident to the runtime layout store's
    /// ident store and comparing ident indices directly.
    ///
    /// Parameters:
    /// - source_env: The module environment containing the source tag name ident
    /// - source_tag_ident: The tag name ident from the source module
    /// - runtime_tags: MultiArrayList slice of tags from the runtime tag union type
    ///
    /// Returns the tag index if found, or null if not found.
    pub fn findTagIndexByIdent(
        self: *Interpreter,
        source_env: *const can.ModuleEnv,
        source_tag_ident: base_pkg.Ident.Idx,
        runtime_tags: anytype,
    ) !?usize {
        // Translate the source tag name to the runtime layout store's ident store
        const source_name_str = source_env.getIdent(source_tag_ident);
        const rt_tag_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));

        // Compare ident indices directly (O(1) per comparison instead of string comparison)
        for (runtime_tags.items(.name), 0..) |tag_name_ident, i| {
            if (tag_name_ident == rt_tag_ident) {
                return i;
            }
        }
        return null;
    }

    /// Find the index of a tag in a list of runtime tags by translating the source tag name ident.
    /// This is the list-based variant of findTagIndexByIdent, used when tags come from appendUnionTags.
    pub fn findTagIndexByIdentInList(
        self: *Interpreter,
        source_env: *const can.ModuleEnv,
        source_tag_ident: base_pkg.Ident.Idx,
        tag_list: []const types.Tag,
    ) !?usize {
        // Translate the source tag name to the runtime layout store's ident store
        const source_name_str = source_env.getIdent(source_tag_ident);
        const rt_tag_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));

        // Compare ident indices directly (O(1) per comparison instead of string comparison)
        for (tag_list, 0..) |tag_info, i| {
            if (tag_info.name == rt_tag_ident) {
                return i;
            }
        }
        return null;
    }

    const TagValue = struct {
        index: usize,
        payload: ?StackValue,
    };

    fn extractTagValue(self: *Interpreter, value: StackValue, union_rt_var: types.Var) !TagValue {
        switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .int => {
                    return .{ .index = @intCast(value.asI128()), .payload = null };
                },
                else => return error.TypeMismatch,
            },
            .record => {
                var acc = try value.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse return error.TypeMismatch;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                    tag_index = @intCast(tmp.asI128());
                } else return error.TypeMismatch;

                var payload_value: ?StackValue = null;
                if (acc.findFieldIndex(self.env.idents.payload)) |payload_idx| {
                    payload_value = try acc.getFieldByIndex(payload_idx);
                    if (payload_value) |field_value| {
                        var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                        defer tag_list.deinit();
                        try self.appendUnionTags(union_rt_var, &tag_list);
                        if (tag_index >= tag_list.items.len) return error.TypeMismatch;
                        const tag_info = tag_list.items[tag_index];
                        const arg_vars = self.runtime_types.sliceVars(tag_info.args);

                        if (arg_vars.len == 0) {
                            payload_value = null;
                        } else if (arg_vars.len == 1) {
                            // Use the layout from the record's stored field, not from the type system.
                            // This ensures we preserve the actual element layout (e.g., List(Dec))
                            // rather than the type system's generic layout.
                            payload_value = StackValue{
                                .layout = field_value.layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        } else {
                            // For multiple args, use the layout from the stored field
                            payload_value = StackValue{
                                .layout = field_value.layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        }
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            .tuple => {
                // Tag unions are now represented as tuples (payload, tag)
                var acc = try value.asTuple(&self.runtime_layout_store);

                // Element 1 is the tag discriminant - getElement takes original index directly
                const tag_field = try acc.getElement(1);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                    tag_index = @intCast(tmp.asI128());
                } else return error.TypeMismatch;

                // Element 0 is the payload - getElement takes original index directly
                var payload_value: ?StackValue = null;
                const payload_field = acc.getElement(0) catch null;
                if (payload_field) |field_value| {
                    var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                    defer tag_list.deinit();
                    try self.appendUnionTags(union_rt_var, &tag_list);
                    if (tag_index >= tag_list.items.len) return error.TypeMismatch;
                    const tag_info = tag_list.items[tag_index];
                    const arg_vars = self.runtime_types.sliceVars(tag_info.args);

                    if (arg_vars.len == 0) {
                        payload_value = null;
                    } else if (arg_vars.len == 1) {
                        // Use the layout from the tuple's stored field, not from the type system.
                        // This ensures we preserve the actual element layout (e.g., List(Dec))
                        // rather than the type system's generic layout (e.g., List(opaque_ptr)).
                        payload_value = StackValue{
                            .layout = field_value.layout,
                            .ptr = field_value.ptr,
                            .is_initialized = field_value.is_initialized,
                        };
                    } else {
                        // For multiple args, use the layout from the stored field
                        // which already has the correct tuple layout
                        payload_value = StackValue{
                            .layout = field_value.layout,
                            .ptr = field_value.ptr,
                            .is_initialized = field_value.is_initialized,
                        };
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            else => return error.TypeMismatch,
        }
    }

    fn makeBoxValueFromLayout(self: *Interpreter, result_layout: Layout, payload: StackValue, roc_ops: *RocOps) !StackValue {
        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = true;

        switch (result_layout.tag) {
            .box_of_zst => {
                if (out.ptr) |ptr| {
                    const slot: *usize = @ptrCast(@alignCast(ptr));
                    slot.* = 0;
                }
                return out;
            },
            .box => {
                const elem_layout = self.runtime_layout_store.getLayout(result_layout.data.box);

                if (!std.meta.eql(elem_layout, payload.layout)) {
                    return error.TypeMismatch;
                }

                const target_usize = self.runtime_layout_store.targetUsize();
                const elem_alignment = elem_layout.alignment(target_usize).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const data_ptr = utils.allocateWithRefcount(elem_size, elem_alignment_u32, false, roc_ops);

                if (elem_size > 0 and payload.ptr != null) {
                    try payload.copyToPtr(&self.runtime_layout_store, data_ptr, roc_ops);
                }

                if (out.ptr) |ptr| {
                    const slot: *usize = @ptrCast(@alignCast(ptr));
                    slot.* = @intFromPtr(data_ptr);
                }
                return out;
            },
            else => return error.TypeMismatch,
        }
    }

    fn makeRenderCtx(self: *Interpreter) render_helpers.RenderCtx {
        return .{
            .allocator = self.allocator,
            .env = self.env,
            .runtime_types = self.runtime_types,
            .layout_store = &self.runtime_layout_store,
            .type_scope = &self.empty_scope,
        };
    }

    pub fn renderValueRoc(self: *Interpreter, value: StackValue) Error![]u8 {
        var ctx = self.makeRenderCtx();
        return render_helpers.renderValueRoc(&ctx, value);
    }

    // removed duplicate

    // Helper for REPL and tests: render a value given its runtime type var
    pub fn renderValueRocWithType(self: *Interpreter, value: StackValue, rt_var: types.Var) Error![]u8 {
        var ctx = self.makeRenderCtx();
        return render_helpers.renderValueRocWithType(&ctx, value, rt_var);
    }

    fn makeListSliceValue(
        self: *Interpreter,
        list_layout: Layout,
        elem_layout: Layout,
        source: RocList,
        start: usize,
        count: usize,
    ) !StackValue {
        // Apply layout correction if needed.
        // This handles cases where the type system's layout doesn't match the actual
        // element layout after runtime defaulting (e.g., numeric literals defaulting to Dec).
        const actual_list_layout = if (list_layout.tag == .list) blk: {
            const stored_elem_layout_idx = list_layout.data.list;
            const stored_elem_layout = self.runtime_layout_store.getLayout(stored_elem_layout_idx);

            const layouts_match = std.meta.eql(stored_elem_layout, elem_layout);
            if (!layouts_match) {
                const correct_elem_idx = try self.runtime_layout_store.insertLayout(elem_layout);
                break :blk Layout{ .tag = .list, .data = .{ .list = correct_elem_idx } };
            } else {
                break :blk list_layout;
            }
        } else list_layout;

        const dest = try self.pushRaw(actual_list_layout, 0);
        if (dest.ptr == null) return dest;
        const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));

        if (count == 0) {
            header.* = RocList.empty();
            return dest;
        }

        const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));
        const elements_refcounted = elem_layout.isRefcounted();

        if (elements_refcounted and source.isUnique()) {
            var source_copy = source;
            markListElementCount(&source_copy, true);
        }

        const src_bytes = source.bytes orelse return error.NullStackPointer;

        var slice = RocList{
            .bytes = src_bytes + start * elem_size,
            .length = count,
            .capacity_or_alloc_ptr = blk: {
                const list_alloc_ptr = (@intFromPtr(src_bytes) >> 1) | builtins.list.SEAMLESS_SLICE_BIT;
                const slice_alloc_ptr = source.capacity_or_alloc_ptr;
                const slice_mask = source.seamlessSliceMask();
                break :blk (list_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
            },
        };

        source.incref(1, elements_refcounted);
        markListElementCount(&slice, elements_refcounted);
        header.* = slice;
        return dest;
    }

    fn markListElementCount(list: *RocList, elements_refcounted: bool) void {
        if (elements_refcounted and !list.isSeamlessSlice()) {
            if (list.getAllocationDataPtr()) |source| {
                const ptr = @as([*]usize, @ptrCast(@alignCast(source))) - 2;
                ptr[0] = list.length;
            }
        }
    }

    fn upsertBinding(
        self: *Interpreter,
        binding: Binding,
        search_start: usize,
        roc_ops: *RocOps,
    ) !void {
        var idx = self.bindings.items.len;
        while (idx > search_start) {
            idx -= 1;
            if (self.bindings.items[idx].pattern_idx == binding.pattern_idx) {
                self.bindings.items[idx].value.decref(&self.runtime_layout_store, roc_ops);
                self.bindings.items[idx] = binding;
                return;
            }
        }

        try self.bindings.append(binding);
    }

    fn trimBindingList(
        self: *Interpreter,
        list: *std.array_list.AlignedManaged(Binding, null),
        new_len: usize,
        roc_ops: *RocOps,
    ) void {
        var idx = list.items.len;
        while (idx > new_len) {
            idx -= 1;
            list.items[idx].value.decref(&self.runtime_layout_store, roc_ops);
        }
        list.items.len = new_len;
    }

    fn patternMatchesBind(
        self: *Interpreter,
        pattern_idx: can.CIR.Pattern.Idx,
        value: StackValue,
        value_rt_var: types.Var,
        roc_ops: *RocOps,
        out_binds: *std.array_list.AlignedManaged(Binding, null),
        expr_idx: can.CIR.Expr.Idx,
    ) !bool {
        const pat = self.env.store.getPattern(pattern_idx);
        switch (pat) {
            .assign => |_| {
                // Bind entire value to this pattern
                const copied = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = copied, .expr_idx = expr_idx, .source_env = self.env });
                return true;
            },
            .as => |as_pat| {
                const before = out_binds.items.len;
                if (!try self.patternMatchesBind(as_pat.pattern, value, value_rt_var, roc_ops, out_binds, expr_idx)) {
                    self.trimBindingList(out_binds, before, roc_ops);
                    return false;
                }

                const alias_value = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = alias_value, .expr_idx = expr_idx, .source_env = self.env });
                return true;
            },
            .underscore => return true,
            .num_literal => |il| {
                if (value.layout.tag != .scalar) return false;
                const lit = il.value.toI128();

                // Handle both int and Dec (frac) layouts for numeric literals
                return switch (value.layout.data.scalar.tag) {
                    .int => value.asI128() == lit,
                    .frac => blk: {
                        // For Dec type, extract the value and compare
                        if (value.layout.data.scalar.data.frac != .dec) break :blk false;
                        const dec_value = value.asDec();
                        // Dec stores values scaled by 10^18, so we need to compare scaled values
                        // For integer literals, we scale the literal value
                        const scaled_lit = lit * RocDec.one_point_zero_i128;
                        break :blk dec_value.num == scaled_lit;
                    },
                    else => false,
                };
            },
            .str_literal => |sl| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .str)) return false;
                const lit = self.env.getString(sl.literal);
                const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
                return rs.eqlSlice(lit);
            },
            .nominal => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds, expr_idx);
            },
            .nominal_external => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds, expr_idx);
            },
            .tuple => |tuple_pat| {
                if (value.layout.tag != .tuple) return false;
                var accessor = try value.asTuple(&self.runtime_layout_store);
                const pat_ids = self.env.store.slicePatterns(tuple_pat.patterns);
                if (pat_ids.len != accessor.getElementCount()) return false;

                const tuple_resolved = self.resolveBaseVar(value_rt_var);
                if (tuple_resolved.desc.content != .structure or tuple_resolved.desc.content.structure != .tuple) return false;
                const elem_vars = self.runtime_types.sliceVars(tuple_resolved.desc.content.structure.tuple.elems);
                if (elem_vars.len != pat_ids.len) return false;

                var idx: usize = 0;
                while (idx < pat_ids.len) : (idx += 1) {
                    if (idx >= accessor.getElementCount()) return false;
                    // getElement expects original index and converts to sorted internally
                    const elem_value = try accessor.getElement(idx);
                    const before = out_binds.items.len;
                    const matched = try self.patternMatchesBind(pat_ids[idx], elem_value, elem_vars[idx], roc_ops, out_binds, expr_idx);
                    if (!matched) {
                        self.trimBindingList(out_binds, before, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            .list => |list_pat| {
                if (value.layout.tag != .list and value.layout.tag != .list_of_zst) return false;

                // Use the layout from the StackValue instead of re-querying the type system.
                // The StackValue has the correct layout that was used to allocate the list,
                // which may differ from the type system's layout if runtime defaulting occurred.
                const list_layout = value.layout;

                const list_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(pattern_idx));
                const list_rt_content = self.runtime_types.resolveVar(list_rt_var).desc.content;
                std.debug.assert(list_rt_content == .structure);
                std.debug.assert(list_rt_content.structure == .nominal_type);

                // Extract the element type variable from the List type
                // Note: nominal.vars contains [backing_var, elem_var] for List types
                // where backing_var is the ProvidedByCompiler tag union, and elem_var is the element type
                const nominal = list_rt_content.structure.nominal_type;
                const vars = self.runtime_types.sliceVars(nominal.vars.nonempty);
                std.debug.assert(vars.len == 2); // List has backing var + elem var
                const elem_rt_var = vars[1];

                // Get element layout from the actual list layout, not from the type system.
                // The list's runtime layout may differ from the type system's expectation
                // due to numeric literal defaulting.
                const elem_layout = if (list_layout.tag == .list)
                    self.runtime_layout_store.getLayout(list_layout.data.list)
                else
                    Layout.zst(); // list_of_zst has zero-sized elements

                var accessor = try value.asList(&self.runtime_layout_store, elem_layout);
                const total_len = accessor.len();
                const non_rest_patterns = self.env.store.slicePatterns(list_pat.patterns);

                if (list_pat.rest_info) |rest_info| {
                    const prefix_len: usize = @intCast(rest_info.index);
                    if (prefix_len > non_rest_patterns.len) return false;
                    const suffix_len: usize = non_rest_patterns.len - prefix_len;
                    if (total_len < prefix_len + suffix_len) return false;

                    var idx: usize = 0;
                    while (idx < prefix_len) : (idx += 1) {
                        const elem_value = try accessor.getElement(idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds, expr_idx);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    var suffix_idx: usize = 0;
                    while (suffix_idx < suffix_len) : (suffix_idx += 1) {
                        const suffix_pattern_idx = non_rest_patterns[prefix_len + suffix_idx];
                        const element_idx = total_len - suffix_len + suffix_idx;
                        const elem_value = try accessor.getElement(element_idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(suffix_pattern_idx, elem_value, elem_rt_var, roc_ops, out_binds, expr_idx);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    if (rest_info.pattern) |rest_pat_idx| {
                        const rest_len = total_len - prefix_len - suffix_len;
                        const rest_value = try self.makeListSliceValue(list_layout, elem_layout, accessor.list, prefix_len, rest_len);
                        defer rest_value.decref(&self.runtime_layout_store, roc_ops);
                        const before = out_binds.items.len;
                        if (!try self.patternMatchesBind(rest_pat_idx, rest_value, value_rt_var, roc_ops, out_binds, expr_idx)) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    return true;
                } else {
                    if (total_len != non_rest_patterns.len) return false;
                    var idx: usize = 0;
                    while (idx < non_rest_patterns.len) : (idx += 1) {
                        const elem_value = try accessor.getElement(idx);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds, expr_idx);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }
                    return true;
                }
            },
            .record_destructure => |rec_pat| {
                if (value.layout.tag != .record) return false;
                var accessor = try value.asRecord(&self.runtime_layout_store);

                const destructs = self.env.store.sliceRecordDestructs(rec_pat.destructs);
                for (destructs) |destruct_idx| {
                    const destruct = self.env.store.getRecordDestruct(destruct_idx);

                    const field_index = accessor.findFieldIndex(destruct.label) orelse return false;
                    const field_value = try accessor.getFieldByIndex(field_index);
                    const field_ct_var = can.ModuleEnv.varFrom(destruct_idx);
                    const field_var = try self.translateTypeVar(self.env, field_ct_var);

                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p_idx| p_idx,
                        .SubPattern => |p_idx| p_idx,
                    };

                    const before = out_binds.items.len;
                    if (!try self.patternMatchesBind(inner_pattern_idx, field_value, field_var, roc_ops, out_binds, expr_idx)) {
                        self.trimBindingList(out_binds, before, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            .applied_tag => |tag_pat| {
                const union_resolved = self.resolveBaseVar(value_rt_var);
                if (union_resolved.desc.content != .structure or union_resolved.desc.content.structure != .tag_union) return false;

                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(value_rt_var, &tag_list);

                const tag_data = try self.extractTagValue(value, value_rt_var);
                if (tag_data.index >= tag_list.items.len) return false;

                // Find the expected tag's index in the tag list by matching the name
                // Pattern tag names are from self.env, tag_list names are in runtime_layout_store.env
                // Translate pattern's tag ident to runtime env for direct comparison
                const expected_name_str = self.env.getIdent(tag_pat.name);
                const expected_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(expected_name_str));
                var expected_index: ?usize = null;
                for (tag_list.items, 0..) |tag_info, i| {
                    if (tag_info.name == expected_ident) {
                        expected_index = i;
                        break;
                    }
                }

                // If the pattern's tag doesn't exist in the union, the match fails
                if (expected_index == null) return false;

                // Compare runtime tag discriminant with expected tag discriminant
                if (tag_data.index != expected_index.?) return false;

                const arg_patterns = self.env.store.slicePatterns(tag_pat.args);
                const arg_vars_range = tag_list.items[expected_index.?].args;
                const arg_vars = self.runtime_types.sliceVars(arg_vars_range);
                if (arg_patterns.len != arg_vars.len) return false;

                if (arg_patterns.len == 0) {
                    return true;
                }

                const start_len = out_binds.items.len;

                const payload_value = tag_data.payload orelse {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                };

                if (arg_patterns.len == 1) {
                    if (!try self.patternMatchesBind(arg_patterns[0], payload_value, arg_vars[0], roc_ops, out_binds, expr_idx)) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                    return true;
                }

                if (payload_value.layout.tag != .tuple) {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                }

                var payload_tuple = try payload_value.asTuple(&self.runtime_layout_store);
                if (payload_tuple.getElementCount() != arg_patterns.len) {
                    self.trimBindingList(out_binds, start_len, roc_ops);
                    return false;
                }

                var j: usize = 0;
                while (j < arg_patterns.len) : (j += 1) {
                    if (j >= payload_tuple.getElementCount()) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                    // getElement expects original index and converts to sorted internally
                    const elem_val = try payload_tuple.getElement(j);
                    if (!try self.patternMatchesBind(arg_patterns[j], elem_val, arg_vars[j], roc_ops, out_binds, expr_idx)) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            else => return false,
        }
    }

    pub fn deinit(self: *Interpreter) void {
        self.empty_scope.deinit();
        self.translate_cache.deinit();
        self.rigid_subst.deinit();
        var it = self.poly_cache.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.args.len > 0) {
                self.allocator.free(@constCast(entry.value_ptr.args));
            }
        }
        self.poly_cache.deinit();
        self.module_envs.deinit(self.allocator);
        self.module_ids.deinit(self.allocator);
        self.import_envs.deinit(self.allocator);
        self.var_to_layout_slot.deinit();
        self.runtime_layout_store.deinit();
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);
        self.snapshots.deinit();
        self.problems.deinit(self.allocator);
        // Note: import_mapping is borrowed, not owned - don't deinit it
        self.unify_scratch.deinit();
        self.stack_memory.deinit();
        self.bindings.deinit();
        self.active_closures.deinit();
        self.def_stack.deinit();
        self.scratch_tags.deinit();
    }

    /// Get the module environment for a given origin module identifier.
    /// Returns the current module's env if the identifier matches, otherwise looks it up in the module map.
    fn getModuleEnvForOrigin(self: *const Interpreter, origin_module: base_pkg.Ident.Idx) ?*const can.ModuleEnv {
        // Check if it's the Builtin module
        // Use root_env.idents for consistent module comparison across all contexts
        if (origin_module == self.root_env.idents.builtin_module) {
            // In shim context, builtins are embedded in the main module env
            // (builtin_module_env is null), so fall back to self.env
            return self.builtin_module_env orelse self.env;
        }
        // Check if it's the current module
        if (self.env.module_name_idx == origin_module) {
            return self.env;
        }
        // Look up in imported modules
        return self.module_envs.get(origin_module);
    }

    /// Get the numeric module ID for a given origin module identifier.
    /// Returns current_module_id (always 0) for the current module, otherwise looks it up in the module ID map.
    fn getModuleIdForOrigin(self: *const Interpreter, origin_module: base_pkg.Ident.Idx) u32 {
        // Check if it's the current module
        if (self.env.module_name_idx == origin_module) {
            return self.current_module_id;
        }
        // Look up in imported modules (should always exist if getModuleEnvForOrigin succeeded)
        return self.module_ids.get(origin_module) orelse self.current_module_id;
    }

    /// Extract the static dispatch constraint for a given method name from a resolved receiver type variable.
    /// Returns the constraint if found, or MethodNotFound if the receiver doesn't expose the method.
    fn getStaticDispatchConstraint(
        self: *const Interpreter,
        receiver_var: types.Var,
        method_name: base_pkg.Ident.Idx,
    ) Error!types.StaticDispatchConstraint {
        const resolved = self.runtime_types.resolveVar(receiver_var);

        // Get constraints from flex or rigid vars
        const constraints: []const types.StaticDispatchConstraint = switch (resolved.desc.content) {
            .flex => |flex| self.runtime_types.sliceStaticDispatchConstraints(flex.constraints),
            .rigid => |rigid| self.runtime_types.sliceStaticDispatchConstraints(rigid.constraints),
            else => return error.MethodNotFound,
        };

        // Linear search for the matching method name (constraints are typically few)
        for (constraints) |constraint| {
            if (constraint.fn_name == method_name) {
                return constraint;
            }
        }

        return error.MethodNotFound;
    }

    /// Dispatch a binary operator to its corresponding method.
    /// Handles the full method dispatch including:
    /// - Type resolution with Dec default for flex/rigid vars
    /// - Operand evaluation
    /// - Method lookup and invocation
    /// Returns the result of the method call.
    fn dispatchBinaryOpMethod(
        self: *Interpreter,
        method_ident: base_pkg.Ident.Idx,
        lhs_expr: can.CIR.Expr.Idx,
        rhs_expr: can.CIR.Expr.Idx,
        roc_ops: *RocOps,
    ) Error!StackValue {
        const lhs_ct_var = can.ModuleEnv.varFrom(lhs_expr);
        const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);

        const rhs_ct_var = can.ModuleEnv.varFrom(rhs_expr);
        const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

        // Resolve the lhs type to get nominal type information
        var lhs_resolved = self.runtime_types.resolveVar(lhs_rt_var);

        // If the type is still a flex/rigid var, default to Dec
        // (Unsuffixed numeric literals default to Dec in Roc)
        if (lhs_resolved.desc.content == .flex or lhs_resolved.desc.content == .rigid) {
            // Create Dec nominal type content
            const dec_content = try self.mkNumberTypeContentRuntime("Dec");
            const dec_var = try self.runtime_types.freshFromContent(dec_content);
            lhs_resolved = self.runtime_types.resolveVar(dec_var);
        }

        // Evaluate both operands
        var lhs = try self.evalExprMinimal(lhs_expr, roc_ops, lhs_rt_var);
        defer lhs.decref(&self.runtime_layout_store, roc_ops);

        var rhs = try self.evalExprMinimal(rhs_expr, roc_ops, rhs_rt_var);
        defer rhs.decref(&self.runtime_layout_store, roc_ops);

        // Get the nominal type information from lhs, or handle anonymous structural types
        const nominal_info: ?struct { origin: base_pkg.Ident.Idx, ident: base_pkg.Ident.Idx } = switch (lhs_resolved.desc.content) {
            .structure => |s| blk2: {
                break :blk2 switch (s) {
                    .nominal_type => |nom| .{
                        .origin = nom.origin_module,
                        .ident = nom.ident.ident_idx,
                    },
                    .record, .tuple, .tag_union, .empty_record, .empty_tag_union => blk: {
                        // Anonymous structural types have implicit is_eq
                        // Use root_env.idents for consistency with method dispatch
                        if (method_ident == self.root_env.idents.is_eq) {
                            const result = self.valuesStructurallyEqual(lhs, lhs_rt_var, rhs, rhs_rt_var) catch |err| {
                                // If structural equality is not implemented for this type, return false
                                if (err == error.NotImplemented) {
                                    self.triggerCrash("DEBUG: dispatchBinaryOpMethod NotImplemented", false, roc_ops);
                                    return error.Crash;
                                }
                                return err;
                            };
                            return try self.makeBoolValue(result);
                        }
                        break :blk null;
                    },
                    else => null,
                };
            },
            else => null,
        };

        if (nominal_info == null) {
            return error.InvalidMethodReceiver;
        }

        // Resolve the method function
        // method_ident comes from root_env.idents (is_lt, is_eq, etc.)
        const method_func = try self.resolveMethodFunction(
            nominal_info.?.origin,
            nominal_info.?.ident,
            method_ident,
            roc_ops,
        );
        defer method_func.decref(&self.runtime_layout_store, roc_ops);

        // Prepare arguments: lhs (receiver) + rhs
        var args = [2]StackValue{ lhs, rhs };

        // Call the method closure
        if (method_func.layout.tag != .closure) {
            return error.TypeMismatch;
        }

        const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

        // Switch to the closure's source module
        const saved_env = self.env;
        const saved_bindings_len = self.bindings.items.len;
        self.env = @constCast(closure_header.source_env);
        defer {
            self.env = saved_env;
            self.bindings.shrinkRetainingCapacity(saved_bindings_len);
        }

        const params = self.env.store.slicePatterns(closure_header.params);
        if (params.len != args.len) {
            return error.TypeMismatch;
        }

        // Provide closure context for capture lookup
        try self.active_closures.append(method_func);
        defer _ = self.active_closures.pop();

        // Check if this is a low-level lambda - if so, dispatch to builtin
        const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
        if (lambda_expr == .e_low_level_lambda) {
            const low_level = lambda_expr.e_low_level_lambda;

            // Dispatch to actual low-level builtin implementation
            // Binary ops don't need return type info (not num_from_int_digits etc)
            return try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);
        }

        // Bind parameters
        for (params, 0..) |param, i| {
            try self.bindings.append(.{
                .pattern_idx = param,
                .value = args[i],
                .expr_idx = @enumFromInt(0),
                .source_env = self.env,
            });
        }

        // Evaluate the method body, handling early returns at function boundary
        const result = self.evalExprMinimal(closure_header.body_idx, roc_ops, null) catch |err| {
            // Clean up bindings before returning
            var k = params.len;
            while (k > 0) {
                k -= 1;
                _ = self.bindings.pop();
            }
            if (err == error.EarlyReturn) {
                const return_val = self.early_return_value orelse return error.Crash;
                self.early_return_value = null;
                return return_val;
            }
            return err;
        };

        // Clean up bindings
        var k = params.len;
        while (k > 0) {
            k -= 1;
            _ = self.bindings.pop();
        }

        return result;
    }

    /// Dispatch a unary operator to a method call.
    /// For example, `!a` desugars to `a.not()` and `-a` desugars to `a.negate()`.
    fn dispatchUnaryOpMethod(
        self: *Interpreter,
        method_ident: base_pkg.Ident.Idx,
        operand_expr: can.CIR.Expr.Idx,
        roc_ops: *RocOps,
    ) Error!StackValue {
        const operand_ct_var = can.ModuleEnv.varFrom(operand_expr);
        const operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);

        // Resolve the operand type to get nominal type information
        var operand_resolved = self.runtime_types.resolveVar(operand_rt_var);

        // If the type is still a flex/rigid var, default to Dec for numeric operations
        // (Unsuffixed numeric literals default to Dec in Roc)
        if (operand_resolved.desc.content == .flex or operand_resolved.desc.content == .rigid) {
            // Create Dec nominal type content
            const dec_content = try self.mkNumberTypeContentRuntime("Dec");
            const dec_var = try self.runtime_types.freshFromContent(dec_content);
            operand_resolved = self.runtime_types.resolveVar(dec_var);
        }

        // Evaluate the operand
        var operand = try self.evalExprMinimal(operand_expr, roc_ops, operand_rt_var);
        defer operand.decref(&self.runtime_layout_store, roc_ops);

        // Get the nominal type information from operand
        const nominal_info = switch (operand_resolved.desc.content) {
            .structure => |s| switch (s) {
                .nominal_type => |nom| .{
                    .origin = nom.origin_module,
                    .ident = nom.ident.ident_idx,
                },
                else => return error.InvalidMethodReceiver,
            },
            else => return error.InvalidMethodReceiver,
        };

        // Resolve the method function
        // method_ident comes from root_env.idents (negate, not, etc.)
        const method_func = try self.resolveMethodFunction(
            nominal_info.origin,
            nominal_info.ident,
            method_ident,
            roc_ops,
        );
        defer method_func.decref(&self.runtime_layout_store, roc_ops);

        // Prepare arguments: just the operand (receiver)
        var args = [1]StackValue{operand};

        // Call the method closure
        if (method_func.layout.tag != .closure) {
            return error.TypeMismatch;
        }

        const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

        // Switch to the closure's source module
        const saved_env = self.env;
        const saved_bindings_len = self.bindings.items.len;
        self.env = @constCast(closure_header.source_env);
        defer {
            self.env = saved_env;
            self.bindings.shrinkRetainingCapacity(saved_bindings_len);
        }

        const params = self.env.store.slicePatterns(closure_header.params);
        if (params.len != args.len) {
            return error.TypeMismatch;
        }

        // Provide closure context for capture lookup
        try self.active_closures.append(method_func);
        defer _ = self.active_closures.pop();

        // Check if this is a low-level lambda - if so, dispatch to builtin
        const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
        if (lambda_expr == .e_low_level_lambda) {
            const low_level = lambda_expr.e_low_level_lambda;
            // Dispatch to actual low-level builtin implementation
            // Binary ops don't need return type info (not num_from_int_digits etc)
            return try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);
        }

        // Bind parameters
        for (params, 0..) |param, i| {
            try self.bindings.append(.{
                .pattern_idx = param,
                .value = args[i],
                .expr_idx = @enumFromInt(0),
                .source_env = self.env,
            });
        }

        // Evaluate the method body, handling early returns at function boundary
        const result = self.evalExprMinimal(closure_header.body_idx, roc_ops, null) catch |err| {
            // Clean up bindings before returning
            var k = params.len;
            while (k > 0) {
                k -= 1;
                _ = self.bindings.pop();
            }
            if (err == error.EarlyReturn) {
                const return_val = self.early_return_value orelse return error.Crash;
                self.early_return_value = null;
                return return_val;
            }
            return err;
        };

        // Clean up bindings
        var k = params.len;
        while (k > 0) {
            k -= 1;
            _ = self.bindings.pop();
        }

        return result;
    }

    /// Resolve and evaluate a method function from its origin module.
    /// Returns a StackValue representing the method function.
    /// The caller is responsible for decref'ing the returned value.
    fn resolveMethodFunction(
        self: *Interpreter,
        origin_module: base_pkg.Ident.Idx,
        nominal_ident: base_pkg.Ident.Idx,
        method_name_ident: base_pkg.Ident.Idx,
        roc_ops: *RocOps,
    ) Error!StackValue {
        // Get the module environment for this type's origin
        const origin_env = self.getModuleEnvForOrigin(origin_module) orelse {
            return error.MethodLookupFailed;
        };

        // Get type and method name strings from their respective stores.
        // nominal_ident comes from runtime types - always in runtime_layout_store.env
        // (see translateTypeVar's nominal handling and mkNumberTypeContentRuntime)
        // method_name_ident comes from the CIR - in self.env
        const type_name = self.runtime_layout_store.env.getIdent(nominal_ident);
        const method_name_str = self.env.getIdent(method_name_ident);

        // Use getMethodIdent which handles the qualified name construction properly
        const method_ident = origin_env.getMethodIdent(type_name, method_name_str) orelse {
            return error.MethodLookupFailed;
        };

        const node_idx = origin_env.getExposedNodeIndexById(method_ident) orelse exposed_blk: {
            // Fallback: search all definitions for the method
            const all_defs = origin_env.store.sliceDefs(origin_env.all_defs);
            for (all_defs) |def_idx| {
                const def = origin_env.store.getDef(def_idx);
                const pat = origin_env.store.getPattern(def.pattern);
                if (pat == .assign and pat.assign.ident == method_ident) {
                    break :exposed_blk @as(u16, @intCast(@intFromEnum(def_idx)));
                }
            }
            return error.MethodLookupFailed;
        };

        // The node should be a Def
        const target_def_idx: can.CIR.Def.Idx = @enumFromInt(node_idx);
        const target_def = origin_env.store.getDef(target_def_idx);

        // Save current environment and bindings
        const saved_env = self.env;
        const saved_bindings_len = self.bindings.items.len;
        self.env = @constCast(origin_env);
        defer {
            self.env = saved_env;
            // Restore bindings
            self.bindings.items.len = saved_bindings_len;
        }

        // Translate the def's type var to runtime
        const def_var = can.ModuleEnv.varFrom(target_def_idx);
        const rt_def_var = try self.translateTypeVar(@constCast(origin_env), def_var);

        // Evaluate the method's expression
        const method_value = try self.evalExprMinimal(target_def.expr, roc_ops, rt_def_var);

        return method_value;
    }

    /// Ensure the slot array can index at least `min_len` entries; zero-fill new entries.
    pub fn ensureVarLayoutCapacity(self: *Interpreter, min_len: usize) !void {
        if (self.var_to_layout_slot.items.len >= min_len) return;
        const old_len = self.var_to_layout_slot.items.len;
        try self.var_to_layout_slot.ensureTotalCapacityPrecise(min_len);
        // Set new length and zero-fill
        self.var_to_layout_slot.items.len = min_len;
        @memset(self.var_to_layout_slot.items[old_len..], 0);
    }

    /// Create nominal number type content for runtime types (e.g., Dec, I64, F64)
    fn mkNumberTypeContentRuntime(self: *Interpreter, type_name: []const u8) !types.Content {
        // Use root_env.idents for consistent module reference
        const origin_module_id = self.root_env.idents.builtin_module;

        // Use fully-qualified type name "Builtin.Num.U8" etc.
        // This allows method lookup to work correctly.
        // Insert into runtime_layout_store.env to be consistent with translateTypeVar's nominal handling.
        const qualified_type_name = try std.fmt.allocPrint(self.allocator, "Builtin.Num.{s}", .{type_name});
        defer self.allocator.free(qualified_type_name);
        const type_name_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(qualified_type_name));
        const type_ident = types.TypeIdent{
            .ident_idx = type_name_ident,
        };

        // Number types backing is [] (empty tag union with closed extension)
        const empty_tag_union_content = types.Content{ .structure = .empty_tag_union };
        const ext_var = try self.runtime_types.freshFromContent(empty_tag_union_content);
        const empty_tag_union = types.TagUnion{
            .tags = types.Tag.SafeMultiList.Range.empty(),
            .ext = ext_var,
        };
        const backing_content = types.Content{ .structure = .{ .tag_union = empty_tag_union } };
        const backing_var = try self.runtime_types.freshFromContent(backing_content);

        // Number types have no type arguments
        const no_type_args: []const types.Var = &.{};

        return try self.runtime_types.mkNominal(
            type_ident,
            backing_var,
            no_type_args,
            origin_module_id,
        );
    }

    /// Get the layout for a runtime type var using the O(1) biased slot array.
    pub fn getRuntimeLayout(self: *Interpreter, type_var: types.Var) !layout.Layout {
        var resolved = self.runtime_types.resolveVar(type_var);

        // Apply rigid variable substitution if this is a rigid variable
        // Follow the substitution chain until we reach a non-rigid variable or run out of substitutions
        // Note: Cycles are prevented by unification, so this chain must terminate
        while (resolved.desc.content == .rigid) {
            if (self.rigid_subst.get(resolved.var_)) |substituted_var| {
                resolved = self.runtime_types.resolveVar(substituted_var);
            } else {
                break;
            }
        }

        const idx: usize = @intFromEnum(resolved.var_);
        try self.ensureVarLayoutCapacity(idx + 1);
        const slot_ptr = &self.var_to_layout_slot.items[idx];

        // If we have a flex var, default it to Dec
        // This is the interpreter-time defaulting for numeric literals
        if (resolved.desc.content == .flex) {
            // Directly return Dec's scalar layout
            const dec_layout = layout.Layout.frac(types.Frac.Precision.dec);
            const dec_layout_idx = try self.runtime_layout_store.insertLayout(dec_layout);
            slot_ptr.* = @intFromEnum(dec_layout_idx) + 1;
            return dec_layout;
        }
        if (slot_ptr.* != 0) {
            const layout_idx_plus_one = slot_ptr.*;
            const layout_idx: layout.Idx = @enumFromInt(layout_idx_plus_one - 1);
            return self.runtime_layout_store.getLayout(layout_idx);
        }

        const layout_idx = switch (resolved.desc.content) {
            .structure => |st| switch (st) {
                .empty_record => try self.runtime_layout_store.ensureEmptyRecordLayout(),
                else => try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope),
            },
            else => try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope),
        };
        slot_ptr.* = @intFromEnum(layout_idx) + 1;
        return self.runtime_layout_store.getLayout(layout_idx);
    }

    const FieldAccumulator = struct {
        fields: std.array_list.AlignedManaged(types.RecordField, null),
        name_to_index: std.AutoHashMap(u32, usize),

        fn init(allocator: std.mem.Allocator) !FieldAccumulator {
            return FieldAccumulator{
                .fields = std.array_list.Managed(types.RecordField).init(allocator),
                .name_to_index = std.AutoHashMap(u32, usize).init(allocator),
            };
        }

        fn deinit(self: *FieldAccumulator) void {
            self.fields.deinit();
            self.name_to_index.deinit();
        }

        fn put(self: *FieldAccumulator, name: base_pkg.Ident.Idx, var_: types.Var) !void {
            const key: u32 = @bitCast(name);
            if (self.name_to_index.get(key)) |idx_ptr| {
                self.fields.items[idx_ptr] = .{ .name = name, .var_ = var_ };
            } else {
                try self.fields.append(.{ .name = name, .var_ = var_ });
                try self.name_to_index.put(key, self.fields.items.len - 1);
            }
        }
    };

    fn collectRecordFieldsFromVar(
        self: *Interpreter,
        module: *can.ModuleEnv,
        ct_var: types.Var,
        acc: *FieldAccumulator,
        visited: *std.AutoHashMap(types.Var, void),
    ) !void {
        if (visited.contains(ct_var)) return;
        try visited.put(ct_var, {});

        const resolved = module.types.resolveVar(ct_var);
        switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .record => |rec| {
                    const ct_fields = module.types.getRecordFieldsSlice(rec.fields);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }
                    try self.collectRecordFieldsFromVar(module, rec.ext, acc, visited);
                },
                .record_unbound => |fields_range| {
                    const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                    var i: usize = 0;
                    while (i < ct_fields.len) : (i += 1) {
                        const f = ct_fields.get(i);
                        try acc.put(f.name, f.var_);
                    }
                },
                .nominal_type => |nom| {
                    const backing = module.types.getNominalBackingVar(nom);
                    try self.collectRecordFieldsFromVar(module, backing, acc, visited);
                },
                .empty_record => {},
                else => {},
            },
            .alias => |alias| {
                const backing = module.types.getAliasBackingVar(alias);
                try self.collectRecordFieldsFromVar(module, backing, acc, visited);
            },
            else => {},
        }
    }

    /// Minimal translate implementation (scaffolding): handles .str only for now
    pub fn translateTypeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var) Error!types.Var {
        const resolved = module.types.resolveVar(compile_var);

        // DEBUG: Print content type for debugging method dispatch issues
        // std.debug.print("translateTypeVar: content = {s}\n", .{@tagName(resolved.desc.content)});
        // if (resolved.desc.content == .structure) {
        //     std.debug.print("  structure = {s}\n", .{@tagName(resolved.desc.content.structure)});
        // }

        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(resolved.var_));
        if (self.translate_cache.get(key)) |found| {
            return found;
        }

        // Insert a placeholder to break cycles during recursive type translation.
        // If we recurse back to this type, we'll return the placeholder instead of infinite looping.
        const placeholder = try self.runtime_types.freshFromContent(.{ .flex = types.Flex.init() });
        try self.translate_cache.put(key, placeholder);

        const out_var = blk: {
            switch (resolved.desc.content) {
                .structure => |flat| {
                    switch (flat) {
                        .tag_union => |tu| {
                            var rt_tag_args = try std.ArrayList(types.Var).initCapacity(self.allocator, 8);
                            defer rt_tag_args.deinit(self.allocator);

                            var rt_tags = try self.gatherTags(module, tu);
                            defer rt_tags.deinit(self.allocator);

                            for (rt_tags.items) |*tag| {
                                rt_tag_args.clearRetainingCapacity();
                                const ct_args = module.types.sliceVars(tag.args);
                                for (ct_args) |ct_arg_var| {
                                    try rt_tag_args.append(self.allocator, try self.translateTypeVar(module, ct_arg_var));
                                }
                                const rt_args_range = try self.runtime_types.appendVars(rt_tag_args.items);
                                // Keep the original tag name - it should already exist in the module's ident store
                                tag.* = .{
                                    .name = tag.name,
                                    .args = rt_args_range,
                                };
                            }

                            const rt_ext = try self.runtime_types.register(.{ .content = .{ .structure = .empty_tag_union }, .rank = types.Rank.top_level, .mark = types.Mark.none });
                            const content = try self.runtime_types.mkTagUnion(rt_tags.items, rt_ext);
                            break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                        },
                        .empty_tag_union => {
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .empty_tag_union });
                        },
                        .tuple => |t| {
                            const ct_elems = module.types.sliceVars(t.elems);
                            var buf = try self.allocator.alloc(types.Var, ct_elems.len);
                            defer self.allocator.free(buf);
                            for (ct_elems, 0..) |ct_elem, i| {
                                buf[i] = try self.translateTypeVar(module, ct_elem);
                            }
                            const range = try self.runtime_types.appendVars(buf);
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = range } } });
                        },
                        .record => |rec| {
                            var acc = try FieldAccumulator.init(self.allocator);
                            defer acc.deinit();
                            var visited = std.AutoHashMap(types.Var, void).init(self.allocator);
                            defer visited.deinit();

                            try self.collectRecordFieldsFromVar(module, rec.ext, &acc, &visited);

                            const ct_fields = module.types.getRecordFieldsSlice(rec.fields);
                            var i: usize = 0;
                            while (i < ct_fields.len) : (i += 1) {
                                const f = ct_fields.get(i);
                                try acc.put(f.name, f.var_);
                            }

                            const rt_ext = try self.translateTypeVar(module, rec.ext);
                            var runtime_fields = try self.allocator.alloc(types.RecordField, acc.fields.items.len);
                            defer self.allocator.free(runtime_fields);
                            var j: usize = 0;
                            while (j < acc.fields.items.len) : (j += 1) {
                                const ct_field = acc.fields.items[j];
                                runtime_fields[j] = .{
                                    .name = ct_field.name,
                                    .var_ = try self.translateTypeVar(module, ct_field.var_),
                                };
                            }
                            const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = rt_ext } } });
                        },
                        .record_unbound => |fields_range| {
                            // TODO: Recursively unwrap record fields via ext, like tag unions
                            const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                            var runtime_fields = try self.allocator.alloc(types.RecordField, ct_fields.len);
                            defer self.allocator.free(runtime_fields);
                            var i: usize = 0;
                            while (i < ct_fields.len) : (i += 1) {
                                const f = ct_fields.get(i);
                                runtime_fields[i] = .{
                                    .name = f.name,
                                    .var_ = try self.translateTypeVar(module, f.var_),
                                };
                            }
                            const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                            const ext_empty = try self.runtime_types.freshFromContent(.{ .structure = .empty_record });
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = ext_empty } } });
                        },
                        .empty_record => {
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .empty_record });
                        },
                        .fn_pure => |f| {
                            const ct_args = module.types.sliceVars(f.args);
                            var buf = try self.allocator.alloc(types.Var, ct_args.len);
                            defer self.allocator.free(buf);
                            for (ct_args, 0..) |ct_arg, i| {
                                buf[i] = try self.translateTypeVar(module, ct_arg);
                            }
                            const rt_ret = try self.translateTypeVar(module, f.ret);
                            const content = try self.runtime_types.mkFuncPure(buf, rt_ret);
                            break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                        },
                        .fn_effectful => |f| {
                            const ct_args = module.types.sliceVars(f.args);
                            var buf = try self.allocator.alloc(types.Var, ct_args.len);
                            defer self.allocator.free(buf);
                            for (ct_args, 0..) |ct_arg, i| {
                                buf[i] = try self.translateTypeVar(module, ct_arg);
                            }
                            const rt_ret = try self.translateTypeVar(module, f.ret);
                            const content = try self.runtime_types.mkFuncEffectful(buf, rt_ret);
                            break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                        },
                        .fn_unbound => |f| {
                            const ct_args = module.types.sliceVars(f.args);
                            var buf = try self.allocator.alloc(types.Var, ct_args.len);
                            defer self.allocator.free(buf);
                            for (ct_args, 0..) |ct_arg, i| {
                                buf[i] = try self.translateTypeVar(module, ct_arg);
                            }
                            const rt_ret = try self.translateTypeVar(module, f.ret);
                            const content = try self.runtime_types.mkFuncUnbound(buf, rt_ret);
                            break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                        },
                        .nominal_type => |nom| {
                            const ct_backing = module.types.getNominalBackingVar(nom);
                            const rt_backing = try self.translateTypeVar(module, ct_backing);
                            const ct_args = module.types.sliceNominalArgs(nom);
                            var buf = try self.allocator.alloc(types.Var, ct_args.len);
                            defer self.allocator.free(buf);
                            for (ct_args, 0..) |ct_arg, i| {
                                buf[i] = try self.translateTypeVar(module, ct_arg);
                            }
                            // Always translate idents to the runtime_layout_store's env's ident store.
                            // This is critical because the layout store was initialized with that env,
                            // and ident comparisons in the layout store use that env's ident indices.
                            // Note: self.env may be temporarily switched during from_numeral evaluation,
                            // so we MUST use runtime_layout_store.env which remains constant.
                            const layout_env = self.runtime_layout_store.env;
                            // Compare the underlying interner pointers to detect different ident stores
                            const needs_translation = @intFromPtr(&module.common.idents.interner) != @intFromPtr(&layout_env.common.idents.interner);
                            const translated_ident = if (needs_translation) ident_blk: {
                                const type_name_str = module.getIdent(nom.ident.ident_idx);
                                break :ident_blk types.TypeIdent{ .ident_idx = try layout_env.insertIdent(base_pkg.Ident.for_text(type_name_str)) };
                            } else nom.ident;
                            const translated_origin = if (needs_translation) origin_blk: {
                                const origin_str = module.getIdent(nom.origin_module);
                                break :origin_blk try layout_env.insertIdent(base_pkg.Ident.for_text(origin_str));
                            } else nom.origin_module;
                            const content = try self.runtime_types.mkNominal(translated_ident, rt_backing, buf, translated_origin);
                            break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                        },
                    }
                },
                .alias => |alias| {
                    const ct_backing = module.types.getAliasBackingVar(alias);
                    const rt_backing = try self.translateTypeVar(module, ct_backing);
                    const ct_args = module.types.sliceAliasArgs(alias);
                    var buf = try self.allocator.alloc(types.Var, ct_args.len);
                    defer self.allocator.free(buf);
                    for (ct_args, 0..) |ct_arg, i| {
                        buf[i] = try self.translateTypeVar(module, ct_arg);
                    }
                    const content = try self.runtime_types.mkAlias(alias.ident, rt_backing, buf);
                    break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .recursion_var => |rec_var| {
                    // Translate the structure variable that the recursion var points to
                    const rt_structure = try self.translateTypeVar(module, rec_var.structure);
                    const content: types.Content = .{ .recursion_var = .{
                        .structure = rt_structure,
                        .name = rec_var.name,
                    } };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .flex => |flex| {
                    // Translate static dispatch constraints if present
                    const rt_flex = if (flex.constraints.len() > 0) blk_flex: {
                        const ct_constraints = module.types.sliceStaticDispatchConstraints(flex.constraints);
                        var rt_constraints = try std.ArrayList(types.StaticDispatchConstraint).initCapacity(self.allocator, ct_constraints.len);
                        defer rt_constraints.deinit(self.allocator);

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            try rt_constraints.append(self.allocator, .{
                                .fn_name = ct_constraint.fn_name,
                                .fn_var = rt_fn_var,
                                .origin = ct_constraint.origin,
                            });
                        }

                        const rt_constraints_range = try self.runtime_types.appendStaticDispatchConstraints(rt_constraints.items);
                        break :blk_flex flex.withConstraints(rt_constraints_range);
                    } else flex;

                    const content: types.Content = .{ .flex = rt_flex };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .rigid => |rigid| {
                    // Translate static dispatch constraints if present
                    const rt_rigid = if (rigid.constraints.len() > 0) blk_rigid: {
                        const ct_constraints = module.types.sliceStaticDispatchConstraints(rigid.constraints);
                        var rt_constraints = try std.ArrayList(types.StaticDispatchConstraint).initCapacity(self.allocator, ct_constraints.len);
                        defer rt_constraints.deinit(self.allocator);

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            try rt_constraints.append(self.allocator, .{
                                .fn_name = ct_constraint.fn_name,
                                .fn_var = rt_fn_var,
                                .origin = ct_constraint.origin,
                            });
                        }

                        const rt_constraints_range = try self.runtime_types.appendStaticDispatchConstraints(rt_constraints.items);
                        break :blk_rigid rigid.withConstraints(rt_constraints_range);
                    } else rigid;

                    const content: types.Content = .{ .rigid = rt_rigid };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .err => {
                    // Handle generic type parameters from compiled builtin modules.
                    // When a generic type variable (like `item` or `state` in List.fold) is
                    // serialized in the compiled Builtin module, it may have .err content
                    // because no concrete type was known at compile time.
                    // Create a fresh unbound variable to represent this generic parameter.
                    // This will be properly instantiated/unified when the function is called.
                    break :blk try self.runtime_types.fresh();
                },
            }
        };

        // Check if this variable has a substitution active (for generic function instantiation)
        const final_var = if (self.rigid_subst.get(out_var)) |substituted| blk: {
            // Recursively check if the substituted variable also has a substitution
            var current = substituted;
            while (self.rigid_subst.get(current)) |next_subst| {
                current = next_subst;
            }
            break :blk current;
        } else out_var;

        // Update the cache with the final var
        try self.translate_cache.put(key, final_var);

        // Redirect the placeholder to the final var so any code that grabbed the placeholder
        // during recursion will now resolve to the correct type
        if (@intFromEnum(placeholder) != @intFromEnum(final_var)) {
            try self.runtime_types.setVarRedirect(placeholder, final_var);
        }

        return final_var;
    }

    /// Instantiate a type by replacing rigid variables with fresh flex variables.
    /// This is used when calling generic functions - it allows rigid type parameters
    /// to be unified with concrete argument types.
    fn instantiateType(self: *Interpreter, type_var: types.Var, subst_map: *std.AutoHashMap(types.Var, types.Var)) Error!types.Var {
        const resolved = self.runtime_types.resolveVar(type_var);

        // Check if we've already instantiated this variable
        if (subst_map.get(resolved.var_)) |instantiated| {
            return instantiated;
        }

        const instantiated = switch (resolved.desc.content) {
            .rigid => blk: {
                // Replace rigid with fresh flex that can be unified
                const fresh = try self.runtime_types.fresh();
                try subst_map.put(resolved.var_, fresh);
                break :blk fresh;
            },
            .structure => |st| blk_struct: {
                // Recursively instantiate type arguments in structures
                const new_var = switch (st) {
                    .fn_pure => |f| blk_fn: {
                        const arg_vars = self.runtime_types.sliceVars(f.args);
                        var new_args = try self.allocator.alloc(types.Var, arg_vars.len);
                        defer self.allocator.free(new_args);
                        for (arg_vars, 0..) |arg_var, i| {
                            new_args[i] = try self.instantiateType(arg_var, subst_map);
                        }
                        const new_ret = try self.instantiateType(f.ret, subst_map);
                        const content = try self.runtime_types.mkFuncPure(new_args, new_ret);
                        break :blk_fn try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                    },
                    .fn_effectful => |f| blk_fn: {
                        const arg_vars = self.runtime_types.sliceVars(f.args);
                        var new_args = try self.allocator.alloc(types.Var, arg_vars.len);
                        defer self.allocator.free(new_args);
                        for (arg_vars, 0..) |arg_var, i| {
                            new_args[i] = try self.instantiateType(arg_var, subst_map);
                        }
                        const new_ret = try self.instantiateType(f.ret, subst_map);
                        const content = try self.runtime_types.mkFuncEffectful(new_args, new_ret);
                        break :blk_fn try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                    },
                    .fn_unbound => |f| blk_fn: {
                        const arg_vars = self.runtime_types.sliceVars(f.args);
                        var new_args = try self.allocator.alloc(types.Var, arg_vars.len);
                        defer self.allocator.free(new_args);
                        for (arg_vars, 0..) |arg_var, i| {
                            new_args[i] = try self.instantiateType(arg_var, subst_map);
                        }
                        const new_ret = try self.instantiateType(f.ret, subst_map);
                        const content = try self.runtime_types.mkFuncUnbound(new_args, new_ret);
                        break :blk_fn try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                    },
                    .tuple => |tuple| blk_tuple: {
                        // Recursively instantiate tuple element types
                        const elem_vars = self.runtime_types.sliceVars(tuple.elems);
                        var new_elems = try self.allocator.alloc(types.Var, elem_vars.len);
                        defer self.allocator.free(new_elems);
                        for (elem_vars, 0..) |elem_var, i| {
                            new_elems[i] = try self.instantiateType(elem_var, subst_map);
                        }
                        const new_elems_range = try self.runtime_types.appendVars(new_elems);
                        const content = types.Content{ .structure = .{ .tuple = .{ .elems = new_elems_range } } };
                        break :blk_tuple try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                    },
                    .record => |record| blk_record: {
                        // Recursively instantiate record field types
                        const fields = self.runtime_types.record_fields.sliceRange(record.fields);
                        var new_fields = try self.allocator.alloc(types.RecordField, fields.len);
                        defer self.allocator.free(new_fields);
                        var i: usize = 0;
                        while (i < fields.len) : (i += 1) {
                            const field = fields.get(i);
                            new_fields[i] = .{
                                .name = field.name,
                                .var_ = try self.instantiateType(field.var_, subst_map),
                            };
                        }
                        const new_fields_range = try self.runtime_types.appendRecordFields(new_fields);
                        const new_ext = try self.instantiateType(record.ext, subst_map);
                        const content = types.Content{ .structure = .{ .record = .{ .fields = new_fields_range, .ext = new_ext } } };
                        break :blk_record try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                    },
                    // For other structures (str, num, empty_record, etc.), return as-is
                    else => type_var,
                };
                try subst_map.put(resolved.var_, new_var);
                break :blk_struct new_var;
            },
            // For other content types, return as-is
            else => type_var,
        };

        return instantiated;
    }

    /// Recursively expand a tag union's tags, returning an array list
    /// Caller owns the returned memory
    fn gatherTags(
        ctx: *const Interpreter,
        module: *can.ModuleEnv,
        tag_union: types.TagUnion,
    ) std.mem.Allocator.Error!std.ArrayList(types.Tag) {
        var scratch_tags = try std.ArrayList(types.Tag).initCapacity(ctx.allocator, 8);

        const tag_slice = module.types.getTagsSlice(tag_union.tags);
        for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
            _ = try scratch_tags.append(ctx.allocator, .{ .name = name, .args = args });
        }

        var current_ext = tag_union.ext;
        while (true) {
            const resolved_ext = module.types.resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| {
                    switch (ext_flat_type) {
                        .empty_tag_union => break,
                        .empty_record => break,
                        .tag_union => |ext_tag_union| {
                            if (ext_tag_union.tags.len() > 0) {
                                const ext_tag_slice = module.types.getTagsSlice(ext_tag_union.tags);
                                for (ext_tag_slice.items(.name), ext_tag_slice.items(.args)) |name, args| {
                                    _ = try scratch_tags.append(ctx.allocator, .{ .name = name, .args = args });
                                }
                                current_ext = ext_tag_union.ext;
                            } else {
                                break;
                            }
                        },
                        .nominal_type => |nom| {
                            // Nominal types (like numeric types) act as their backing type
                            current_ext = module.types.getNominalBackingVar(nom);
                        },
                        else => {
                            // TODO: Don't use unreachable here
                            unreachable;
                        },
                    }
                },
                .alias => |alias| {
                    current_ext = module.types.getAliasBackingVar(alias);
                },
                .flex => break,
                .rigid => break,
                else => {
                    // TODO: Don't use unreachable here
                    unreachable;
                },
            }
        }

        // Sort the tags alphabetically
        std.mem.sort(types.Tag, scratch_tags.items, module.common.getIdentStore(), comptime types.Tag.sortByNameAsc);

        return scratch_tags;
    }

    pub fn makePolyKey(self: *Interpreter, module_id: u32, func_id: u32, args: []const types.Var) PolyKey {
        _ = self;
        return PolyKey.init(module_id, func_id, args);
    }

    fn polyLookup(self: *Interpreter, module_id: u32, func_id: u32, args: []const types.Var) ?PolyEntry {
        const key = self.makePolyKey(module_id, func_id, args);
        return self.poly_cache.get(key);
    }

    fn polyInsert(self: *Interpreter, module_id: u32, func_id: u32, entry: PolyEntry) !void {
        const key = PolyKey.init(module_id, func_id, entry.args);
        try self.poly_cache.put(key, entry);
    }

    /// Prepare a call: return cached instantiation entry if present; on miss, insert using return_var_hint if provided.
    pub fn prepareCall(self: *Interpreter, module_id: u32, func_id: u32, args: []const types.Var, return_var_hint: ?types.Var) !?PolyEntry {
        if (self.polyLookup(module_id, func_id, args)) |found| return found;

        if (return_var_hint) |ret| {
            _ = try self.getRuntimeLayout(ret);
            const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret).var_);
            try self.ensureVarLayoutCapacity(root_idx + 1);
            const slot = self.var_to_layout_slot.items[root_idx];
            const args_copy_mut = try self.allocator.alloc(types.Var, args.len);
            errdefer self.allocator.free(args_copy_mut);
            std.mem.copyForwards(types.Var, args_copy_mut, args);
            const entry = PolyEntry{ .return_var = ret, .return_layout_slot = slot, .args = args_copy_mut };
            try self.polyInsert(module_id, func_id, entry);
            return entry;
        }

        return null;
    }

    /// Prepare a call using a known runtime function type var.
    /// Builds and inserts a cache entry on miss using the function's declared return var.
    pub fn prepareCallWithFuncVar(self: *Interpreter, module_id: u32, func_id: u32, func_type_var: types.Var, args: []const types.Var) !PolyEntry {
        if (self.polyLookup(module_id, func_id, args)) |found| return found;

        const func_resolved = self.runtime_types.resolveVar(func_type_var);

        const ret_var: types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| f.ret,
                .fn_effectful => |f| f.ret,
                .fn_unbound => |f| f.ret,
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        };

        // Attempt simple runtime unification of parameters with arguments.
        const params: []types.Var = switch (func_resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .fn_pure => |f| self.runtime_types.sliceVars(f.args),
                .fn_effectful => |f| self.runtime_types.sliceVars(f.args),
                .fn_unbound => |f| self.runtime_types.sliceVars(f.args),
                else => &[_]types.Var{},
            },
            else => &[_]types.Var{},
        };
        if (params.len != args.len) return error.TypeMismatch;

        var i: usize = 0;
        while (i < params.len) : (i += 1) {
            _ = try unify.unifyWithConf(
                self.env,
                self.runtime_types,
                &self.problems,
                &self.snapshots,
                &self.unify_scratch,
                &self.unify_scratch.occurs_scratch,
                unify.ModuleEnvLookup{
                    .interpreter_lookup_ctx = @ptrCast(&self.module_envs),
                    .interpreter_lookup_fn = interpreterLookupModuleEnv,
                },
                params[i],
                args[i],
                unify.Conf{ .ctx = .anon, .constraint_origin_var = null },
            );
        }
        // ret_var may now be constrained

        // Apply rigid substitutions to ret_var if needed
        // Follow the substitution chain until we reach a non-rigid variable or run out of substitutions
        // Note: Cycles are prevented by unification, so this chain must terminate
        var resolved_ret = self.runtime_types.resolveVar(ret_var);
        var substituted_ret = ret_var;
        while (resolved_ret.desc.content == .rigid) {
            if (self.rigid_subst.get(resolved_ret.var_)) |subst_var| {
                substituted_ret = subst_var;
                resolved_ret = self.runtime_types.resolveVar(subst_var);
            } else {
                break;
            }
        }

        // Ensure layout slot for return var
        _ = try self.getRuntimeLayout(substituted_ret);
        const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(substituted_ret).var_);
        try self.ensureVarLayoutCapacity(root_idx + 1);
        const slot = self.var_to_layout_slot.items[root_idx];
        const args_copy_mut = try self.allocator.alloc(types.Var, args.len);
        errdefer self.allocator.free(args_copy_mut);
        std.mem.copyForwards(types.Var, args_copy_mut, args);

        const entry = PolyEntry{ .return_var = substituted_ret, .return_layout_slot = slot, .args = args_copy_mut };
        try self.polyInsert(module_id, func_id, entry);
        return entry;
    }

    /// Initial a TypeWriter from an interpreter. Useful when debugging
    fn initTypeWriter(self: *const Interpreter) std.mem.Allocator.Error!types.TypeWriter {
        return try types.TypeWriter.initFromParts(self.allocator, self.runtime_types, self.env.common.getIdentStore(), null);
    }
};

fn add(a: i32, b: i32) i32 {
    return a + b;
}

// GREEN step: basic test to confirm the module's tests run
test "interpreter: wiring works" {
    try std.testing.expectEqual(@as(i32, 3), add(1, 2));
}

// Empty import mapping for tests that don't need type name resolution
var empty_import_mapping = import_mapping_mod.ImportMapping.init(std.testing.allocator);

// RED: expect Var->Layout slot to work (will fail until implemented)

// RED: translating a compile-time str var should produce a runtime str var
test "interpreter: translateTypeVar for str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    // Get the actual Str type from the Builtin module using the str_stmt index
    const ct_str = can.ModuleEnv.varFrom(builtin_indices.str_type);
    const rt_var = try interp.translateTypeVar(str_module.env, ct_str);

    // The runtime var should be a nominal Str type
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .nominal_type);
}

// RED: translating a compile-time concrete int64 should produce a runtime int64
// RED: translating a compile-time tuple (Str, I64) should produce a runtime tuple with same element shapes

// RED: translating a compile-time record { first: Str, second: I64 } should produce equivalent runtime record

// RED: translating a compile-time alias should produce equivalent runtime alias
test "interpreter: translateTypeVar for alias of Str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    const alias_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("MyAlias"));
    const type_ident = types.TypeIdent{ .ident_idx = alias_name };

    // Create nominal Str type
    const str_ident = try env.insertIdent(base_pkg.Ident.for_text("Str"));
    const builtin_ident = try env.insertIdent(base_pkg.Ident.for_text("Builtin"));
    const str_backing_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const str_vars = [_]types.Var{str_backing_var};
    const str_vars_range = try env.types.appendVars(&str_vars);
    const str_nominal = types.NominalType{
        .ident = types.TypeIdent{ .ident_idx = str_ident },
        .vars = .{ .nonempty = str_vars_range },
        .origin_module = builtin_ident,
    };
    const ct_str = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });

    const ct_alias_content = try env.types.mkAlias(type_ident, ct_str, &.{});
    const ct_alias_var = try env.types.register(.{ .content = ct_alias_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_alias_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .alias);
    const rt_alias = resolved.desc.content.alias;
    try std.testing.expectEqual(alias_name, rt_alias.ident.ident_idx);
    const rt_backing = interp.runtime_types.getAliasBackingVar(rt_alias);
    const backing_resolved = interp.runtime_types.resolveVar(rt_backing);
    try std.testing.expect(backing_resolved.desc.content == .structure);
    try std.testing.expect(backing_resolved.desc.content.structure == .nominal_type);
}

// RED: translating a compile-time nominal type should produce equivalent runtime nominal
test "interpreter: translateTypeVar for nominal Point(Str)" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    const name_nominal = try env.common.idents.insert(gpa, @import("base").Ident.for_text("Point"));
    const type_ident = types.TypeIdent{ .ident_idx = name_nominal };

    // Create nominal Str type
    const str_ident = try env.insertIdent(base_pkg.Ident.for_text("Str"));
    const builtin_ident = try env.insertIdent(base_pkg.Ident.for_text("Builtin"));
    const str_backing_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const str_vars = [_]types.Var{str_backing_var};
    const str_vars_range = try env.types.appendVars(&str_vars);
    const str_nominal = types.NominalType{
        .ident = types.TypeIdent{ .ident_idx = str_ident },
        .vars = .{ .nonempty = str_vars_range },
        .origin_module = builtin_ident,
    };
    const ct_str = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });

    // backing type is Str for simplicity
    const ct_nominal_content = try env.types.mkNominal(type_ident, ct_str, &.{}, name_nominal);
    const ct_nominal_var = try env.types.register(.{ .content = ct_nominal_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    const rt_var = try interp.translateTypeVar(&env, ct_nominal_var);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .nominal_type => |nom| {
            try std.testing.expectEqual(name_nominal, nom.ident.ident_idx);
            const backing = interp.runtime_types.getNominalBackingVar(nom);
            const b_resolved = interp.runtime_types.resolveVar(backing);
            try std.testing.expect(b_resolved.desc.content == .structure);
            try std.testing.expect(b_resolved.desc.content.structure == .nominal_type);
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time flex var should produce a runtime flex var
test "interpreter: translateTypeVar for flex var" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    const ct_flex = try env.types.freshFromContent(.{ .flex = types.Flex.init() });
    const rt_var = try interp.translateTypeVar(&env, ct_flex);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .flex);
}

// RED: translating a compile-time rigid var should produce a runtime rigid var with same ident
test "interpreter: translateTypeVar for rigid var" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    const name_a = try env.common.idents.insert(gpa, @import("base").Ident.for_text("A"));
    const ct_rigid = try env.types.freshFromContent(.{ .rigid = types.Rigid.init(name_a) });
    const rt_var = try interp.translateTypeVar(&env, ct_rigid);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .rigid);
    try std.testing.expectEqual(name_a, resolved.desc.content.rigid.name);
}

// RED: translating a flex var with static dispatch constraints should preserve constraints

// Test multiple constraints on a single flex var

// Test rigid var with static dispatch constraints

// Test getStaticDispatchConstraint helper with flex var

// Test getStaticDispatchConstraint with non-constrained type
test "interpreter: getStaticDispatchConstraint returns error for non-constrained types" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    // Create nominal Str type (no constraints)
    const str_ident = try env.insertIdent(base_pkg.Ident.for_text("Str"));
    const builtin_ident = try env.insertIdent(base_pkg.Ident.for_text("Builtin"));
    const str_backing_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const str_vars = [_]types.Var{str_backing_var};
    const str_vars_range = try env.types.appendVars(&str_vars);
    const str_nominal = types.NominalType{
        .ident = types.TypeIdent{ .ident_idx = str_ident },
        .vars = .{ .nonempty = str_vars_range },
        .origin_module = builtin_ident,
    };
    const ct_str = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });
    const rt_var = try interp.translateTypeVar(&env, ct_str);

    // Try to get a constraint from a non-flex/rigid type
    const method_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("someMethod"));
    const result = interp.getStaticDispatchConstraint(rt_var, method_name);
    try std.testing.expectError(error.MethodNotFound, result);
}

// RED: poly cache miss then hit

// RED: prepareCall should miss without hint, then hit after inserting with hint

// RED: prepareCallWithFuncVar populates cache based on function type

// RED: unification constrains return type for polymorphic (a -> a), when called with Str
test "interpreter: unification constrains (a->a) with Str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    const func_id: u32 = 42;
    // runtime flex var 'a'
    const a = try interp.runtime_types.freshFromContent(.{ .flex = types.Flex.init() });
    const func_content = try interp.runtime_types.mkFuncPure(&.{a}, a);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Call with Str
    // Get the real Str type from the loaded builtin module and translate to runtime
    const ct_str = can.ModuleEnv.varFrom(builtin_indices.str_type);
    const rt_str = try interp.translateTypeVar(str_module.env, ct_str);
    const entry = try interp.prepareCallWithFuncVar(0, func_id, func_var, &.{rt_str});

    // After unification, return var should resolve to str (nominal type)
    const resolved_ret = interp.runtime_types.resolveVar(entry.return_var);
    try std.testing.expect(resolved_ret.desc.content == .structure);
    try std.testing.expect(resolved_ret.desc.content.structure == .nominal_type);
    try std.testing.expect(entry.return_layout_slot != 0);
}

test "interpreter: cross-module method resolution should find methods in origin module" {
    const gpa = std.testing.allocator;

    const module_a_name = "ModuleA";
    const module_b_name = "ModuleB";

    // Set up Module A (the imported module where the type and method are defined)
    var module_a = try can.ModuleEnv.init(gpa, module_a_name);
    defer module_a.deinit();
    try module_a.initCIRFields(gpa, module_a_name);

    // Set up Module B (the current module that imports Module A)
    var module_b = try can.ModuleEnv.init(gpa, module_b_name);
    defer module_b.deinit();
    try module_b.initCIRFields(gpa, module_b_name);

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    var interp = try Interpreter.init(gpa, &module_b, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    // Register module A as an imported module
    const module_a_ident = try module_b.common.idents.insert(gpa, @import("base").Ident.for_text(module_a_name));
    try interp.module_envs.put(interp.allocator, module_a_ident, &module_a);
    const module_a_id: u32 = 1;
    try interp.module_ids.put(interp.allocator, module_a_ident, module_a_id);

    // Create an Import.Idx for module A
    const import_idx: can.CIR.Import.Idx = @enumFromInt(0);
    try interp.import_envs.put(interp.allocator, import_idx, &module_a);

    // Verify we can retrieve module A's environment
    const found_env = interp.getModuleEnvForOrigin(module_a_ident);
    try std.testing.expect(found_env != null);
    try std.testing.expectEqual(module_a.module_name_idx, found_env.?.module_name_idx);

    // Verify we can retrieve module A's ID
    const found_id = interp.getModuleIdForOrigin(module_a_ident);
    try std.testing.expectEqual(module_a_id, found_id);
}

test "interpreter: transitive module method resolution (A imports B imports C)" {
    const gpa = std.testing.allocator;

    const module_a_name = "ModuleA";
    const module_b_name = "ModuleB";
    const module_c_name = "ModuleC";

    // Set up three modules: A (current) imports B, B imports C
    var module_a = try can.ModuleEnv.init(gpa, module_a_name);
    defer module_a.deinit();
    try module_a.initCIRFields(gpa, module_a_name);

    var module_b = try can.ModuleEnv.init(gpa, module_b_name);
    defer module_b.deinit();
    try module_b.initCIRFields(gpa, module_b_name);

    var module_c = try can.ModuleEnv.init(gpa, module_c_name);
    defer module_c.deinit();
    try module_c.initCIRFields(gpa, module_c_name);

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Try(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Try", result_source);
    defer result_module.deinit();
    const str_source = compiled_builtins.builtin_source;
    var str_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Str", str_source);
    defer str_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env, str_module.env);
    // Use module_a as the current module
    var interp = try Interpreter.init(gpa, &module_a, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping);
    defer interp.deinit();

    // Register module B
    const module_b_ident = try module_a.common.idents.insert(gpa, @import("base").Ident.for_text(module_b_name));
    try interp.module_envs.put(interp.allocator, module_b_ident, &module_b);
    const module_b_id: u32 = 1;
    try interp.module_ids.put(interp.allocator, module_b_ident, module_b_id);

    // Register module C
    const module_c_ident = try module_a.common.idents.insert(gpa, @import("base").Ident.for_text(module_c_name));
    try interp.module_envs.put(interp.allocator, module_c_ident, &module_c);
    const module_c_id: u32 = 2;
    try interp.module_ids.put(interp.allocator, module_c_ident, module_c_id);

    // Create Import.Idx entries for both modules
    const import_b_idx: can.CIR.Import.Idx = @enumFromInt(0);
    const import_c_idx: can.CIR.Import.Idx = @enumFromInt(1);
    try interp.import_envs.put(interp.allocator, import_b_idx, &module_b);
    try interp.import_envs.put(interp.allocator, import_c_idx, &module_c);

    // Verify we can retrieve all module environments
    try std.testing.expectEqual(module_b.module_name_idx, interp.getModuleEnvForOrigin(module_b_ident).?.module_name_idx);
    try std.testing.expectEqual(module_c.module_name_idx, interp.getModuleEnvForOrigin(module_c_ident).?.module_name_idx);

    // Verify we can retrieve all module IDs
    try std.testing.expectEqual(module_b_id, interp.getModuleIdForOrigin(module_b_ident));
    try std.testing.expectEqual(module_c_id, interp.getModuleIdForOrigin(module_c_ident));
}
