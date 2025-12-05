//! Interpreter implementing the type-carrying architecture.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

/// Stack size for the interpreter. WASM targets use a smaller stack to avoid
/// memory pressure from repeated allocations that can't be efficiently coalesced.
const stack_size: u32 = if (builtin.cpu.arch == .wasm32) 4 * 1024 * 1024 else 64 * 1024 * 1024;
const trace_eval = build_options.trace_eval;
const trace_refcount = if (@hasDecl(build_options, "trace_refcount")) build_options.trace_refcount else false;
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
    elem_rt_var: types.Var,
    roc_ops: *RocOps,
};

/// Increment callback for list operations - increments refcount of element via StackValue
fn listElementInc(context_opaque: ?*anyopaque, elem_ptr: ?[*]u8) callconv(.c) void {
    const context: *RefcountContext = @ptrCast(@alignCast(context_opaque.?));
    const elem_value = StackValue{
        .layout = context.elem_layout,
        .ptr = @ptrCast(elem_ptr),
        .is_initialized = true,
        .rt_var = context.elem_rt_var,
    };
    elem_value.incref(context.layout_store);
}

/// Decrement callback for list operations - decrements refcount of element via StackValue
fn listElementDec(context_opaque: ?*anyopaque, elem_ptr: ?[*]u8) callconv(.c) void {
    const context: *RefcountContext = @ptrCast(@alignCast(context_opaque.?));
    const elem_value = StackValue{
        .layout = context.elem_layout,
        .ptr = @ptrCast(elem_ptr),
        .is_initialized = true,
        .rt_var = context.elem_rt_var,
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
        .tag_union => std.meta.eql(a.data.tag_union, b.data.tag_union),
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
        NotImplemented,
        NotNumeric,
        NullStackPointer,
        RecordIndexOutOfBounds,
        StringOrderingNotSupported,
        StackOverflow,
        TupleIndexOutOfBounds,
        TypeMismatch,
        ZeroSizedType,
    } || std.mem.Allocator.Error || layout.LayoutError;

    /// Key for caching type translations, combining module identity with type variable.
    const ModuleVarKey = struct {
        module: *can.ModuleEnv,
        var_: types.Var,
    };

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
        /// Optional expression index. Null for bindings that don't have an associated
        /// expression (e.g., function parameters, method parameters, etc. where the
        /// binding comes from a pattern match rather than a def expression).
        expr_idx: ?can.CIR.Expr.Idx,
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
    // Translation cache: (module, resolved_var) -> runtime_var
    translate_cache: std.AutoHashMap(ModuleVarKey, types.Var),
    // Rigid variable substitution context for generic function instantiation
    // Maps rigid type variables to their concrete instantiations
    rigid_subst: std.AutoHashMap(types.Var, types.Var),
    // Compile-time rigid substitution for nominal type backing translation
    // Maps CT rigid vars in backing type to CT type arg vars
    translate_rigid_subst: std.AutoHashMap(types.Var, types.Var),

    // Flex type context for polymorphic parameter type propagation.
    // This allows numeric literals inside polymorphic functions to get the correct
    // concrete type when the function is called with a specific type context.
    flex_type_context: std.AutoHashMap(ModuleVarKey, types.Var),

    // Polymorphic instantiation cache

    poly_cache: HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80),

    // Runtime unification context
    env: *can.ModuleEnv,
    /// Root module used for method idents (is_lt, is_eq, etc.) - never changes during execution
    root_env: *can.ModuleEnv,
    builtin_module_env: ?*const can.ModuleEnv,
    /// App module for resolving e_lookup_required (platform requires clause)
    /// When the primary env is the platform, this points to the app that provides required values.
    app_env: ?*can.ModuleEnv,
    /// Array of all module environments, indexed by resolved module index
    /// Used to resolve imports via pre-resolved indices in env.imports.resolved_modules
    all_module_envs: []const *const can.ModuleEnv,
    module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
    /// Module envs keyed by translated idents (in runtime_layout_store.env's ident space)
    /// Used for method lookup on nominal types whose origin_module was translated
    translated_module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
    /// Pre-translated module name idents for comparison in getModuleEnvForOrigin
    /// These are in runtime_layout_store.env's ident space
    translated_builtin_module: base_pkg.Ident.Idx,
    translated_env_module: base_pkg.Ident.Idx,
    translated_app_module: base_pkg.Ident.Idx,
    module_ids: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32),
    import_envs: std.AutoHashMapUnmanaged(can.CIR.Import.Idx, *const can.ModuleEnv),
    current_module_id: u32,
    next_module_id: u32,
    problems: problem_mod.Store,
    snapshots: snapshot_mod.Store,
    import_mapping: *const import_mapping_mod.ImportMapping,
    unify_scratch: unify.Scratch,
    type_writer: types.TypeWriter,

    // Minimal eval support
    stack_memory: stack.Stack,
    bindings: std.array_list.Managed(Binding),
    // Track active closures during calls (for capture lookup)
    active_closures: std.array_list.Managed(StackValue),
    canonical_bool_rt_var: ?types.Var,
    canonical_str_rt_var: ?types.Var,
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

    /// Arena allocator for constant/static strings. These are allocated once and never freed
    /// individually - the entire arena is freed when the interpreter is deinitialized.
    /// This avoids leak detection false positives for intentionally-immortal string literals.
    constant_strings_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, env: *can.ModuleEnv, builtin_types: BuiltinTypes, builtin_module_env: ?*const can.ModuleEnv, other_envs: []const *const can.ModuleEnv, import_mapping: *const import_mapping_mod.ImportMapping, app_env: ?*can.ModuleEnv) !Interpreter {
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

        // Calculate total import count including app imports
        const app_import_count: usize = if (app_env) |a_env| a_env.imports.imports.items.items.len else 0;
        const total_import_count = import_count + app_import_count;

        if (other_envs.len > 0 and total_import_count > 0) {
            // Allocate capacity for all imports (even if some are duplicates)
            try module_envs.ensureTotalCapacity(allocator, @intCast(other_envs.len));
            try module_ids.ensureTotalCapacity(allocator, @intCast(other_envs.len));
            try import_envs.ensureTotalCapacity(allocator, @intCast(total_import_count));

            // Process ALL imports from primary env using pre-resolved module indices
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

            // Also process app env imports if app_env is different from primary env
            // This is needed when the platform calls the app's main! via e_lookup_required
            if (app_env) |a_env| {
                if (a_env != env) {
                    for (0..app_import_count) |i| {
                        const import_idx: can.CIR.Import.Idx = @enumFromInt(i);

                        // Use pre-resolved module index - skip if not resolved
                        const resolved_idx = a_env.imports.getResolvedModule(import_idx) orelse continue;

                        if (resolved_idx >= other_envs.len) continue;

                        const module_env = other_envs[resolved_idx];

                        // Store in import_envs for app's imports
                        // Use put instead of putAssumeCapacity since we may have overlapping indices
                        try import_envs.put(allocator, import_idx, module_env);
                    }
                }
            }
        }

        return initWithModuleEnvs(allocator, env, other_envs, module_envs, module_ids, import_envs, next_id, builtin_types, builtin_module_env, import_mapping, app_env);
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
        app_env: ?*can.ModuleEnv,
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
            .translate_cache = std.AutoHashMap(ModuleVarKey, types.Var).init(allocator),
            .rigid_subst = std.AutoHashMap(types.Var, types.Var).init(allocator),
            .translate_rigid_subst = std.AutoHashMap(types.Var, types.Var).init(allocator),
            .flex_type_context = std.AutoHashMap(ModuleVarKey, types.Var).init(allocator),
            .poly_cache = HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80).init(allocator),
            .env = env,
            .root_env = env, // Root env is the original env passed to init - used for method idents
            .builtin_module_env = builtin_module_env,
            .app_env = app_env,
            .all_module_envs = all_module_envs,
            .module_envs = module_envs,
            .translated_module_envs = undefined, // Set after runtime_layout_store init
            .translated_builtin_module = base_pkg.Ident.Idx.NONE,
            .translated_env_module = base_pkg.Ident.Idx.NONE,
            .translated_app_module = base_pkg.Ident.Idx.NONE,
            .module_ids = module_ids,
            .import_envs = import_envs,
            .current_module_id = 0, // Current module always gets ID 0
            .next_module_id = next_module_id,
            .problems = try problem_mod.Store.initCapacity(allocator, 64),
            .snapshots = try snapshot_mod.Store.initCapacity(allocator, 256),
            .import_mapping = import_mapping,
            .unify_scratch = try unify.Scratch.init(allocator),
            .type_writer = try types.TypeWriter.initFromParts(allocator, rt_types_ptr, env.common.getIdentStore(), null),
            .stack_memory = try stack.Stack.initCapacity(allocator, stack_size),
            .bindings = try std.array_list.Managed(Binding).initCapacity(allocator, 8),
            .active_closures = try std.array_list.Managed(StackValue).initCapacity(allocator, 4),
            .canonical_bool_rt_var = null,
            .canonical_str_rt_var = null,
            .scratch_tags = try std.array_list.Managed(types.Tag).initCapacity(allocator, 8),
            .builtins = builtin_types,
            .def_stack = try std.array_list.Managed(DefInProgress).initCapacity(allocator, 4),
            .num_literal_target_type = null,
            .last_error_message = null,
            .early_return_value = null,
            .constant_strings_arena = std.heap.ArenaAllocator.init(allocator),
        };

        // Use the pre-interned "Builtin.Str" identifier from the module env
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types, env.idents.builtin_str);

        // Build translated_module_envs for runtime method lookups
        // This maps module names in runtime_layout_store.env's ident space to their ModuleEnvs
        var translated_module_envs = std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv){};
        errdefer translated_module_envs.deinit(allocator);
        const layout_env = result.runtime_layout_store.env;

        // Helper to check if a module has a valid module_name_idx
        // (handles both unset NONE and corrupted undefined values from deserialized data)
        const hasValidModuleName = struct {
            fn check(mod_env: *const can.ModuleEnv) bool {
                // Check for NONE sentinel
                if (mod_env.module_name_idx.isNone()) return false;
                // Bounds check - module_name_idx.idx must be within the ident store
                const ident_store_size = mod_env.common.idents.interner.bytes.items.items.len;
                return mod_env.module_name_idx.idx < ident_store_size;
            }
        }.check;

        // Add current/root module (skip if module_name_idx is unset, e.g., in tests)
        if (hasValidModuleName(env)) {
            const current_name_str = env.getIdent(env.module_name_idx);
            const translated_current = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(current_name_str));
            try translated_module_envs.put(allocator, translated_current, env);
        }

        // Add app module if different from env
        if (app_env) |a_env| {
            if (a_env != env and hasValidModuleName(a_env)) {
                const app_name_str = a_env.getIdent(a_env.module_name_idx);
                const translated_app = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(app_name_str));
                try translated_module_envs.put(allocator, translated_app, a_env);
            }
        }

        // Add builtin module
        if (builtin_module_env) |bme| {
            if (hasValidModuleName(bme)) {
                const builtin_name_str = bme.getIdent(bme.module_name_idx);
                const translated_builtin = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(builtin_name_str));
                try translated_module_envs.put(allocator, translated_builtin, bme);
            }
        }

        // Add all other modules
        for (all_module_envs) |mod_env| {
            if (hasValidModuleName(mod_env)) {
                const mod_name_str = mod_env.getIdent(mod_env.module_name_idx);
                const translated_mod = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(mod_name_str));
                // Use put to handle potential duplicates (same module might be in multiple places)
                try translated_module_envs.put(allocator, translated_mod, mod_env);
            }
        }

        result.translated_module_envs = translated_module_envs;

        // Pre-translate module names for comparison in getModuleEnvForOrigin
        // All translated idents are in runtime_layout_store.env's ident space
        result.translated_builtin_module = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text("Builtin"));

        // Translate env's module name
        if (hasValidModuleName(env)) {
            const env_name_str = env.getIdent(env.module_name_idx);
            result.translated_env_module = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(env_name_str));
        }

        // Translate app's module name
        if (app_env) |a_env| {
            if (a_env != env and hasValidModuleName(a_env)) {
                const app_name_str = a_env.getIdent(a_env.module_name_idx);
                result.translated_app_module = try @constCast(layout_env).insertIdent(base_pkg.Ident.for_text(app_name_str));
            }
        }

        return result;
    }

    /// Evaluates a Roc expression and returns the result.
    pub fn eval(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) Error!StackValue {
        // Clear flex_type_context at the start of each top-level evaluation.
        // This prevents stale type mappings from previous evaluations from
        // interfering with polymorphic function instantiation.
        self.flex_type_context.clearRetainingCapacity();
        return try self.evalWithExpectedType(expr_idx, roc_ops, null);
    }

    pub fn registerDefValue(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, value: StackValue) void {
        if (self.def_stack.items.len == 0) return;
        var top = &self.def_stack.items[self.def_stack.items.len - 1];
        if (top.expr_idx == expr_idx and top.value == null) {
            top.value = value;
        }
    }

    pub fn startTrace(_: *Interpreter) void {}

    pub fn endTrace(_: *Interpreter) void {}

    pub fn evaluateExpression(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        ret_ptr: *anyopaque,
        roc_ops: *RocOps,
        arg_ptr: ?*anyopaque,
    ) Error!void {
        if (arg_ptr) |args_ptr| {
            const func_val = try self.eval(expr_idx, roc_ops);
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
                // Use first element's rt_var as placeholder - this tuple is internal-only,
                // elements get their own rt_vars when extracted via getElement
                args_tuple_value = StackValue{ .layout = tuple_layout, .ptr = args_ptr, .is_initialized = true, .rt_var = param_rt_vars[0] };
                args_accessor = try args_tuple_value.asTuple(&self.runtime_layout_store);

                var j: usize = 0;
                while (j < params.len) : (j += 1) {
                    // getElement expects original index and converts to sorted internally
                    const arg_value = try args_accessor.getElement(j, param_rt_vars[j]);
                    // expr_idx not used in this context - binding happens during function call setup
                    const matched = try self.patternMatchesBind(params[j], arg_value, param_rt_vars[j], roc_ops, &temp_binds, null);
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

            // Decref args after body evaluation (caller transfers ownership)
            defer if (params.len > 0) args_tuple_value.decref(&self.runtime_layout_store, roc_ops);

            defer self.trimBindingList(&self.bindings, base_binding_len, roc_ops);

            // Evaluate body, handling early returns at function boundary
            const result_value = self.evalWithExpectedType(header.body_idx, roc_ops, null) catch |err| {
                if (err == error.EarlyReturn) {
                    const return_val = self.early_return_value orelse return error.Crash;
                    self.early_return_value = null;
                    defer return_val.decref(&self.runtime_layout_store, roc_ops);
                    if (try self.shouldCopyResult(return_val, ret_ptr, roc_ops)) {
                        try return_val.copyToPtr(&self.runtime_layout_store, ret_ptr);
                    }
                    return;
                }
                return err;
            };
            defer result_value.decref(&self.runtime_layout_store, roc_ops);

            // Only copy result if the result type is compatible with ret_ptr
            if (try self.shouldCopyResult(result_value, ret_ptr, roc_ops)) {
                try result_value.copyToPtr(&self.runtime_layout_store, ret_ptr);
            }
            return;
        }

        const result = try self.eval(expr_idx, roc_ops);
        defer result.decref(&self.runtime_layout_store, roc_ops);

        // Only copy result if the result type is compatible with ret_ptr
        if (try self.shouldCopyResult(result, ret_ptr, roc_ops)) {
            try result.copyToPtr(&self.runtime_layout_store, ret_ptr);
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

    fn pushStr(self: *Interpreter, rt_var: types.Var) !StackValue {
        const layout_val = Layout.str();
        const size: u32 = self.runtime_layout_store.layoutSize(layout_val);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = false, .rt_var = rt_var };
        }
        const alignment = layout_val.alignment(self.runtime_layout_store.targetUsize());
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true, .rt_var = rt_var };
    }

    /// Create a constant/static string using the arena allocator.
    /// The string data is allocated from the constant_strings_arena and will be
    /// freed wholesale when the interpreter is deinitialized.
    /// Returns a RocStr that can be assigned to a StackValue.
    fn createConstantStr(self: *Interpreter, content: []const u8) !RocStr {
        // Small strings are stored inline - no heap allocation needed
        if (RocStr.fitsInSmallStr(content.len)) {
            return RocStr.fromSliceSmall(content);
        }

        // Big string - allocate from arena with space for refcount
        const ptr_width = @sizeOf(usize);
        const extra_bytes = ptr_width; // Space for refcount
        const total_size = extra_bytes + content.len;

        const arena_alloc = self.constant_strings_arena.allocator();
        // Alignment must match usize for refcount storage (portable across 32/64-bit and wasm)
        const alignment = comptime std.mem.Alignment.fromByteUnits(@alignOf(usize));
        const buffer = try arena_alloc.alignedAlloc(u8, alignment, total_size);

        // Set refcount to REFCOUNT_STATIC_DATA (0) - this string is immortal
        const refcount_ptr: *usize = @ptrCast(@alignCast(buffer.ptr));
        refcount_ptr.* = 0; // REFCOUNT_STATIC_DATA

        // Copy string content after refcount
        const data_ptr = buffer.ptr + extra_bytes;
        @memcpy(data_ptr[0..content.len], content);

        return RocStr{
            .bytes = data_ptr,
            .length = content.len,
            .capacity_or_alloc_ptr = content.len,
        };
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
                break :blk try self.renderValueRocWithType(value, rt_var, roc_ops);
            } else {
                break :blk try self.renderValueRoc(value);
            }
        };
        defer self.allocator.free(rendered);

        return RocStr.fromSlice(rendered, roc_ops);
    }

    pub fn pushRaw(self: *Interpreter, layout_val: Layout, initial_size: usize, rt_var: types.Var) !StackValue {
        const size: u32 = if (initial_size == 0) self.runtime_layout_store.layoutSize(layout_val) else @intCast(initial_size);
        if (size == 0) {
            return StackValue{ .layout = layout_val, .ptr = null, .is_initialized = true, .rt_var = rt_var };
        }
        const target_usize = self.runtime_layout_store.targetUsize();
        var alignment = layout_val.alignment(target_usize);
        if (layout_val.tag == .closure) {
            const captures_layout = self.runtime_layout_store.getLayout(layout_val.data.closure.captures_layout_idx);
            alignment = alignment.max(captures_layout.alignment(target_usize));
        }
        const ptr = try self.stack_memory.alloca(size, alignment);
        return StackValue{ .layout = layout_val, .ptr = ptr, .is_initialized = true, .rt_var = rt_var };
    }

    /// Push raw bytes with a specific size and alignment (for building records/tuples)
    pub fn pushRawBytes(self: *Interpreter, size: usize, alignment: usize, rt_var: types.Var) !StackValue {
        if (size == 0) {
            return StackValue{ .layout = .{ .tag = .zst, .data = undefined }, .ptr = null, .is_initialized = true, .rt_var = rt_var };
        }
        const align_enum: std.mem.Alignment = switch (alignment) {
            1 => .@"1",
            2 => .@"2",
            4 => .@"4",
            8 => .@"8",
            16 => .@"16",
            else => .@"1",
        };
        const ptr = try self.stack_memory.alloca(@intCast(size), align_enum);
        return StackValue{ .layout = .{ .tag = .zst, .data = undefined }, .ptr = ptr, .is_initialized = true, .rt_var = rt_var };
    }

    pub fn pushCopy(self: *Interpreter, src: StackValue) !StackValue {
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
            try src.copyToPtr(&self.runtime_layout_store, ptr.?);
        }
        return dest;
    }

    /// Result from setupSortWith helper
    const SortWithResult = union(enum) {
        /// List has < 2 elements, already sorted. Caller should decref compare_fn and push list_value.
        already_sorted: StackValue,
        /// Sorting continuation has been set up. Caller should return true.
        sorting_started,
    };

    /// Helper to set up list_sort_with continuation-based evaluation.
    /// Shared between call_invoke_closure and dot_access_collect_args paths.
    fn setupSortWith(
        self: *Interpreter,
        list_arg: StackValue,
        compare_fn: StackValue,
        call_ret_rt_var: ?types.Var,
        saved_rigid_subst_in: ?std.AutoHashMap(types.Var, types.Var),
        roc_ops: *RocOps,
        work_stack: *WorkStack,
    ) !SortWithResult {
        var saved_rigid_subst = saved_rigid_subst_in;

        std.debug.assert(list_arg.layout.tag == .list or list_arg.layout.tag == .list_of_zst);

        const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
        const list_len = roc_list.len();

        // If list has 0 or 1 elements, it's already sorted
        if (list_len < 2) {
            // Free saved_rigid_subst since we won't pass it to continuation
            if (saved_rigid_subst) |*saved| saved.deinit();
            return .{ .already_sorted = list_arg };
        }

        // Get element layout
        const elem_layout_idx = list_arg.layout.data.list;
        const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
        const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
        const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
        const elem_alignment_u32: u32 = @intCast(elem_alignment);

        // Make a unique copy of the list for sorting
        const elements_refcounted = elem_layout.isRefcounted();
        const elem_rt_var = try self.runtime_types.fresh();
        var refcount_context = RefcountContext{
            .layout_store = &self.runtime_layout_store,
            .elem_layout = elem_layout,
            .elem_rt_var = elem_rt_var,
            .roc_ops = roc_ops,
        };

        const working_list = roc_list.makeUnique(
            elem_alignment_u32,
            elem_size,
            elements_refcounted,
            if (elements_refcounted) @ptrCast(&refcount_context) else null,
            if (elements_refcounted) &listElementInc else &builtins.list.rcNone,
            if (elements_refcounted) @ptrCast(&refcount_context) else null,
            if (elements_refcounted) &listElementDec else &builtins.list.rcNone,
            roc_ops,
        );

        // Write the result of makeUnique back into the list arg
        const list_arg_ptr: *builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
        list_arg_ptr.* = working_list;

        // Update rt_var if provided
        var result_list = list_arg;
        if (call_ret_rt_var) |rt_var| {
            result_list.rt_var = rt_var;
        }

        // Start insertion sort at index 1
        // Get elements at indices 0 and 1 for first comparison
        const elem0_ptr = working_list.bytes.? + 0 * elem_size;
        const elem1_ptr = working_list.bytes.? + 1 * elem_size;

        // elem_rt_var already declared above for RefcountContext
        const elem0_value = StackValue{
            .layout = elem_layout,
            .ptr = @ptrCast(elem0_ptr),
            .is_initialized = true,
            .rt_var = elem_rt_var,
        };
        const elem1_value = StackValue{
            .layout = elem_layout,
            .ptr = @ptrCast(elem1_ptr),
            .is_initialized = true,
            .rt_var = elem_rt_var,
        };

        // Copy elements for comparison (compare_fn will consume them)
        const arg0 = try self.pushCopy(elem1_value); // element being inserted
        const arg1 = try self.pushCopy(elem0_value); // element to compare against

        // Push continuation to handle comparison result
        try work_stack.push(.{ .apply_continuation = .{ .sort_compare_result = .{
            .list_value = result_list,
            .compare_fn = compare_fn,
            .call_ret_rt_var = call_ret_rt_var,
            .saved_rigid_subst = saved_rigid_subst,
            .outer_index = 1,
            .inner_index = 0,
            .list_len = list_len,
            .elem_size = elem_size,
            .elem_layout = elem_layout,
            .elem_rt_var = elem_rt_var,
        } } });
        saved_rigid_subst = null; // Ownership transferred to continuation

        // Invoke comparison function with (elem_at_outer, elem_at_inner)
        const cmp_header: *const layout.Closure = @ptrCast(@alignCast(compare_fn.ptr.?));
        const cmp_saved_env = self.env;
        self.env = @constCast(cmp_header.source_env);

        const cmp_params = self.env.store.slicePatterns(cmp_header.params);
        if (cmp_params.len != 2) {
            self.env = cmp_saved_env;
            return error.TypeMismatch;
        }

        try self.active_closures.append(compare_fn);

        // Bind parameters
        try self.bindings.append(.{
            .pattern_idx = cmp_params[0],
            .value = arg0,
            .expr_idx = null, // expr_idx not used for comparison function parameter bindings
            .source_env = self.env,
        });
        try self.bindings.append(.{
            .pattern_idx = cmp_params[1],
            .value = arg1,
            .expr_idx = null, // expr_idx not used for comparison function parameter bindings
            .source_env = self.env,
        });

        // Push cleanup and evaluate body
        const bindings_start = self.bindings.items.len - 2;
        try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
            .saved_env = cmp_saved_env,
            .saved_bindings_len = bindings_start,
            .param_count = 2,
            .has_active_closure = true,
            .did_instantiate = false,
            .call_ret_rt_var = null,
            .saved_rigid_subst = null,
            .saved_flex_type_context = null,
            .arg_rt_vars_to_free = null,
        } } });
        try work_stack.push(.{ .eval_expr = .{
            .expr_idx = cmp_header.body_idx,
            .expected_rt_var = null,
        } });

        return .sorting_started;
    }

    /// Call a hosted function via RocOps.hosted_fns array
    /// This marshals arguments to the host, invokes the function pointer, and marshals the result back
    fn callHostedFunction(
        self: *Interpreter,
        hosted_fn_index: u32,
        args: []StackValue,
        roc_ops: *RocOps,
        return_rt_var: types.Var,
    ) !StackValue {
        // Validate index is within bounds
        if (hosted_fn_index >= roc_ops.hosted_fns.count) {
            self.triggerCrash("Hosted function index out of bounds", false, roc_ops);
            return error.Crash;
        }

        // Get the hosted function pointer from RocOps
        const hosted_fn = roc_ops.hosted_fns.fns[hosted_fn_index];

        // Allocate space for the return value using the actual return type
        const return_layout = try self.getRuntimeLayout(return_rt_var);
        const result_value = try self.pushRaw(return_layout, 0, return_rt_var);

        // Get return pointer (for ZST returns, use a dummy stack address)
        const ret_ptr = if (result_value.ptr) |p| p else @as(*anyopaque, @ptrFromInt(@intFromPtr(&result_value)));

        // Calculate total size needed for packed arguments
        var total_args_size: usize = 0;
        var max_alignment: std.mem.Alignment = .@"1";
        for (args) |arg| {
            const arg_size: usize = self.runtime_layout_store.layoutSize(arg.layout);
            const arg_align = arg.layout.alignment(self.runtime_layout_store.targetUsize());
            max_alignment = max_alignment.max(arg_align);
            // Align to the argument's alignment
            total_args_size = std.mem.alignForward(usize, total_args_size, arg_align.toByteUnits());
            total_args_size += arg_size;
        }

        if (args.len == 0) {
            // Zero argument case - pass dummy pointer
            var dummy: u8 = 0;
            hosted_fn(roc_ops, ret_ptr, @ptrCast(&dummy));
        } else {
            // Allocate buffer for packed arguments
            const args_buffer = try self.stack_memory.alloca(@intCast(total_args_size), max_alignment);

            // Pack each argument into the buffer
            var offset: usize = 0;
            for (args) |arg| {
                const arg_size: usize = self.runtime_layout_store.layoutSize(arg.layout);
                const arg_align = arg.layout.alignment(self.runtime_layout_store.targetUsize());

                // Align offset
                offset = std.mem.alignForward(usize, offset, arg_align.toByteUnits());

                // Copy argument data
                if (arg_size > 0) {
                    if (arg.ptr) |src_ptr| {
                        const dest_ptr = @as([*]u8, @ptrCast(args_buffer)) + offset;
                        @memcpy(dest_ptr[0..arg_size], @as([*]const u8, @ptrCast(src_ptr))[0..arg_size]);
                    }
                }
                offset += arg_size;
            }

            // Invoke the hosted function following RocCall ABI: (ops, ret_ptr, args_ptr)
            hosted_fn(roc_ops, ret_ptr, args_buffer);
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
                var out = try self.pushRaw(result_layout, 0, str_a_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, str_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, str_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, str_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, str_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, str_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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

                const result_rt_var = return_rt_var orelse unreachable;
                const result_layout = layout.Layout.int(.u64);
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
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

                const result_rt_var = return_rt_var orelse try self.getCanonicalStrRuntimeVar();
                const result_layout = layout.Layout.str();
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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
                var out = try self.pushRaw(result_layout, 0, string_arg.rt_var);
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

                // Get the result layout - should be List(U8).
                // If return_rt_var is a flex that would default to a scalar,
                // we need to ensure we get a proper list layout for correct refcounting.
                const result_rt_var = return_rt_var orelse {
                    self.triggerCrash("str_to_utf8 requires return type info", false, roc_ops);
                    return error.Crash;
                };
                const result_layout = blk: {
                    const maybe_layout = try self.getRuntimeLayout(result_rt_var);
                    // If the layout is a list, use it
                    if (maybe_layout.tag == .list or maybe_layout.tag == .list_of_zst) {
                        break :blk maybe_layout;
                    }
                    // Fallback: create a proper List(U8) layout
                    const u8_layout_idx = try self.runtime_layout_store.insertLayout(Layout.int(.u8));
                    break :blk Layout.list(u8_layout_idx);
                };

                var out = try self.pushRaw(result_layout, 0, result_rt_var);
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

                const result_rt_var = return_rt_var orelse try self.getCanonicalStrRuntimeVar();
                const result_layout = layout.Layout.str();
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
                out.is_initialized = false;

                const result_ptr: *RocStr = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_str;

                out.is_initialized = true;
                return out;
            },
            .str_from_utf8 => {
                // Str.from_utf8 : List(U8) -> Try(Str, [BadUtf8({ problem: Utf8Problem, index: U64 })])
                std.debug.assert(args.len == 1);

                const list_arg = args[0];
                std.debug.assert(list_arg.ptr != null);

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));
                const result = builtins.str.fromUtf8C(roc_list.*, .Immutable, roc_ops);

                // Get the return layout from the caller - it should be a Try tag union
                const result_rt_var = return_rt_var orelse {
                    self.triggerCrash("str_from_utf8 requires return type info", false, roc_ops);
                    return error.Crash;
                };
                const result_layout = try self.getRuntimeLayout(result_rt_var);

                // Resolve the Try type to get tag indices
                const resolved = self.resolveBaseVar(result_rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) {
                    self.triggerCrash("str_from_utf8: expected tag union return type", false, roc_ops);
                    return error.Crash;
                }

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

                if (result.is_ok) {
                    // Return Ok(string)
                    if (result_layout.tag == .tuple) {
                        // Tuple (payload, tag)
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        var acc = try dest.asTuple(&self.runtime_layout_store);

                        // Create fresh vars for element access (payload is Str, discriminant is int)
                        const str_rt_var = try self.getCanonicalStrRuntimeVar();
                        const disc_rt_var = try self.runtime_types.fresh();

                        // Element 0 is the payload - clear it first since it's a union
                        const payload_field = try acc.getElement(0, str_rt_var);
                        if (payload_field.ptr) |payload_ptr| {
                            const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                            if (payload_bytes_len > 0) {
                                const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                                @memset(bytes, 0);
                            }
                            // Write Str to the payload area
                            const str_ptr: *RocStr = @ptrCast(@alignCast(payload_ptr));
                            str_ptr.* = result.string;
                        }

                        // Element 1 is the tag discriminant
                        const tag_field = try acc.getElement(1, disc_rt_var);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(ok_index orelse 0));
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else if (result_layout.tag == .record) {
                        // Record { tag, payload }
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        var acc = try dest.asRecord(&self.runtime_layout_store);

                        const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                            self.triggerCrash("str_from_utf8: tag field not found", false, roc_ops);
                            return error.Crash;
                        };
                        const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse {
                            self.triggerCrash("str_from_utf8: payload field not found", false, roc_ops);
                            return error.Crash;
                        };

                        // Create fresh vars for field access (payload is Str, discriminant is int)
                        const str_rt_var = try self.getCanonicalStrRuntimeVar();
                        const disc_rt_var = try self.runtime_types.fresh();

                        // Write tag discriminant
                        const tag_field = try acc.getFieldByIndex(tag_field_idx, disc_rt_var);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(ok_index orelse 0));
                        }

                        // Clear payload area first since it's a union
                        const payload_field = try acc.getFieldByIndex(payload_field_idx, str_rt_var);
                        if (payload_field.ptr) |payload_ptr| {
                            const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                            if (payload_bytes_len > 0) {
                                const bytes = @as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len];
                                @memset(bytes, 0);
                            }
                            // Write Str to the payload area
                            const str_ptr: *RocStr = @ptrCast(@alignCast(payload_ptr));
                            str_ptr.* = result.string;
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else if (result_layout.tag == .tag_union) {
                        // Tag union layout with proper variant info
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        const tu_data = self.runtime_layout_store.getTagUnionData(result_layout.data.tag_union.idx);

                        if (dest.ptr) |base_ptr| {
                            const ptr_u8 = @as([*]u8, @ptrCast(base_ptr));

                            // Clear the entire payload area first
                            const total_size = self.runtime_layout_store.layoutSize(result_layout);
                            if (total_size > 0) {
                                @memset(ptr_u8[0..total_size], 0);
                            }

                            // Write discriminant at discriminant_offset
                            const disc_ptr = ptr_u8 + tu_data.discriminant_offset;
                            const disc_value: u32 = @intCast(ok_index orelse 0);
                            switch (tu_data.discriminant_size) {
                                1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(disc_value),
                                2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(disc_value),
                                4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = disc_value,
                                else => {},
                            }

                            // Write Str payload at offset 0
                            const str_ptr: *RocStr = @ptrCast(@alignCast(base_ptr));
                            str_ptr.* = result.string;
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else {
                        self.triggerCrash("str_from_utf8: unexpected result layout", false, roc_ops);
                        return error.Crash;
                    }
                } else {
                    // Return Err(BadUtf8({ problem: Utf8Problem, index: U64 }))
                    if (result_layout.tag == .tuple) {
                        // Tuple (payload, tag)
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        var acc = try dest.asTuple(&self.runtime_layout_store);

                        // Element 1 is the tag discriminant
                        const disc_rt_var = try self.runtime_types.fresh();
                        const tag_field = try acc.getElement(1, disc_rt_var);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(err_index orelse 1));
                        }

                        // Element 0 is the payload - need to construct BadUtf8 record
                        const payload_rt_var = try self.runtime_types.fresh();
                        const payload_field = try acc.getElement(0, payload_rt_var);
                        if (payload_field.layout.tag == .tuple) {
                            // BadUtf8 is represented as a tuple containing the error record
                            var err_tuple = try payload_field.asTuple(&self.runtime_layout_store);
                            // First element should be the record { problem, index }
                            const inner_rt_var = try self.runtime_types.fresh();
                            const inner_payload = try err_tuple.getElement(0, inner_rt_var);
                            if (inner_payload.layout.tag == .record) {
                                var inner_acc = try inner_payload.asRecord(&self.runtime_layout_store);
                                // Set problem field (tag union represented as u8)
                                if (inner_acc.findFieldIndex(self.env.idents.problem)) |problem_idx| {
                                    const problem_rt = try self.runtime_types.fresh();
                                    const problem_field = try inner_acc.getFieldByIndex(problem_idx, problem_rt);
                                    if (problem_field.ptr) |ptr| {
                                        const typed_ptr: *u8 = @ptrCast(@alignCast(ptr));
                                        typed_ptr.* = @intFromEnum(result.problem_code);
                                    }
                                }
                                // Set index field (U64)
                                if (inner_acc.findFieldIndex(self.env.idents.index)) |index_idx| {
                                    const index_rt = try self.runtime_types.fresh();
                                    const index_field = try inner_acc.getFieldByIndex(index_idx, index_rt);
                                    if (index_field.ptr) |ptr| {
                                        const typed_ptr: *u64 = @ptrCast(@alignCast(ptr));
                                        typed_ptr.* = result.byte_index;
                                    }
                                }
                            }
                            // Set BadUtf8 tag discriminant (index 0 since it's the only variant)
                            const inner_disc_rt_var = try self.runtime_types.fresh();
                            const err_tag = try err_tuple.getElement(1, inner_disc_rt_var);
                            if (err_tag.layout.tag == .scalar and err_tag.layout.data.scalar.tag == .int) {
                                var tmp = err_tag;
                                tmp.is_initialized = false;
                                try tmp.setInt(0);
                            }
                        } else if (payload_field.layout.tag == .record) {
                            // Payload is a record with tag and payload for BadUtf8
                            var err_rec = try payload_field.asRecord(&self.runtime_layout_store);
                            if (err_rec.findFieldIndex(self.env.idents.tag)) |tag_idx| {
                                const field_rt = try self.runtime_types.fresh();
                                const inner_tag = try err_rec.getFieldByIndex(tag_idx, field_rt);
                                if (inner_tag.layout.tag == .scalar and inner_tag.layout.data.scalar.tag == .int) {
                                    var tmp = inner_tag;
                                    tmp.is_initialized = false;
                                    try tmp.setInt(0); // BadUtf8 is index 0
                                }
                            }
                            if (err_rec.findFieldIndex(self.env.idents.payload)) |inner_payload_idx| {
                                const field_rt = try self.runtime_types.fresh();
                                const inner_payload = try err_rec.getFieldByIndex(inner_payload_idx, field_rt);
                                if (inner_payload.layout.tag == .record) {
                                    var inner_acc = try inner_payload.asRecord(&self.runtime_layout_store);
                                    if (inner_acc.findFieldIndex(self.env.idents.problem)) |problem_idx| {
                                        const field_rt2 = try self.runtime_types.fresh();
                                        const problem_field = try inner_acc.getFieldByIndex(problem_idx, field_rt2);
                                        if (problem_field.ptr) |ptr| {
                                            const typed_ptr: *u8 = @ptrCast(@alignCast(ptr));
                                            typed_ptr.* = @intFromEnum(result.problem_code);
                                        }
                                    }
                                    if (inner_acc.findFieldIndex(self.env.idents.index)) |index_idx| {
                                        const field_rt2 = try self.runtime_types.fresh();
                                        const index_field = try inner_acc.getFieldByIndex(index_idx, field_rt2);
                                        if (index_field.ptr) |ptr| {
                                            const typed_ptr: *u64 = @ptrCast(@alignCast(ptr));
                                            typed_ptr.* = result.byte_index;
                                        }
                                    }
                                }
                            }
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else if (result_layout.tag == .record) {
                        // Record { tag, payload }
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        var acc = try dest.asRecord(&self.runtime_layout_store);

                        const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                            self.triggerCrash("str_from_utf8: tag field not found", false, roc_ops);
                            return error.Crash;
                        };
                        const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse {
                            self.triggerCrash("str_from_utf8: payload field not found", false, roc_ops);
                            return error.Crash;
                        };

                        // Write tag discriminant for Err
                        const field_rt = try self.runtime_types.fresh();
                        const tag_field = try acc.getFieldByIndex(tag_field_idx, field_rt);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(err_index orelse 1));
                        }

                        // Write error payload - need to construct BadUtf8({ problem, index })
                        const payload_rt = try self.runtime_types.fresh();
                        const outer_payload = try acc.getFieldByIndex(payload_field_idx, payload_rt);
                        if (outer_payload.layout.tag == .tuple) {
                            var err_tuple = try outer_payload.asTuple(&self.runtime_layout_store);
                            const inner_rt_var = try self.runtime_types.fresh();
                            const inner_payload = try err_tuple.getElement(0, inner_rt_var);
                            if (inner_payload.layout.tag == .record) {
                                var inner_acc = try inner_payload.asRecord(&self.runtime_layout_store);
                                if (inner_acc.findFieldIndex(self.env.idents.problem)) |problem_idx| {
                                    const field_rt2 = try self.runtime_types.fresh();
                                    const problem_field = try inner_acc.getFieldByIndex(problem_idx, field_rt2);
                                    if (problem_field.ptr) |ptr| {
                                        const typed_ptr: *u8 = @ptrCast(@alignCast(ptr));
                                        typed_ptr.* = @intFromEnum(result.problem_code);
                                    }
                                }
                                if (inner_acc.findFieldIndex(self.env.idents.index)) |index_idx| {
                                    const field_rt2 = try self.runtime_types.fresh();
                                    const index_field = try inner_acc.getFieldByIndex(index_idx, field_rt2);
                                    if (index_field.ptr) |ptr| {
                                        const typed_ptr: *u64 = @ptrCast(@alignCast(ptr));
                                        typed_ptr.* = result.byte_index;
                                    }
                                }
                            }
                            const err_disc_rt_var = try self.runtime_types.fresh();
                            const err_tag = try err_tuple.getElement(1, err_disc_rt_var);
                            if (err_tag.layout.tag == .scalar and err_tag.layout.data.scalar.tag == .int) {
                                var tmp = err_tag;
                                tmp.is_initialized = false;
                                try tmp.setInt(0);
                            }
                        } else if (outer_payload.layout.tag == .record) {
                            var err_rec = try outer_payload.asRecord(&self.runtime_layout_store);
                            if (err_rec.findFieldIndex(self.env.idents.tag)) |inner_tag_idx| {
                                const field_rt2 = try self.runtime_types.fresh();
                                const inner_tag = try err_rec.getFieldByIndex(inner_tag_idx, field_rt2);
                                if (inner_tag.layout.tag == .scalar and inner_tag.layout.data.scalar.tag == .int) {
                                    var tmp = inner_tag;
                                    tmp.is_initialized = false;
                                    try tmp.setInt(0);
                                }
                            }
                            if (err_rec.findFieldIndex(self.env.idents.payload)) |inner_payload_idx| {
                                const field_rt2 = try self.runtime_types.fresh();
                                const inner_payload = try err_rec.getFieldByIndex(inner_payload_idx, field_rt2);
                                if (inner_payload.layout.tag == .record) {
                                    var inner_acc = try inner_payload.asRecord(&self.runtime_layout_store);
                                    if (inner_acc.findFieldIndex(self.env.idents.problem)) |problem_idx| {
                                        const field_rt3 = try self.runtime_types.fresh();
                                        const problem_field = try inner_acc.getFieldByIndex(problem_idx, field_rt3);
                                        if (problem_field.ptr) |ptr| {
                                            const typed_ptr: *u8 = @ptrCast(@alignCast(ptr));
                                            typed_ptr.* = @intFromEnum(result.problem_code);
                                        }
                                    }
                                    if (inner_acc.findFieldIndex(self.env.idents.index)) |index_idx| {
                                        const field_rt3 = try self.runtime_types.fresh();
                                        const index_field = try inner_acc.getFieldByIndex(index_idx, field_rt3);
                                        if (index_field.ptr) |ptr| {
                                            const typed_ptr: *u64 = @ptrCast(@alignCast(ptr));
                                            typed_ptr.* = result.byte_index;
                                        }
                                    }
                                }
                            }
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else if (result_layout.tag == .tag_union) {
                        // Tag union layout with proper variant info for Err case
                        var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                        const tu_data = self.runtime_layout_store.getTagUnionData(result_layout.data.tag_union.idx);

                        if (dest.ptr) |base_ptr| {
                            const ptr_u8 = @as([*]u8, @ptrCast(base_ptr));

                            // Clear the entire area first
                            const total_size = self.runtime_layout_store.layoutSize(result_layout);
                            if (total_size > 0) {
                                @memset(ptr_u8[0..total_size], 0);
                            }

                            // Write outer discriminant (Err) at discriminant_offset
                            const disc_ptr = ptr_u8 + tu_data.discriminant_offset;
                            const disc_value: u32 = @intCast(err_index orelse 1);
                            switch (tu_data.discriminant_size) {
                                1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(disc_value),
                                2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(disc_value),
                                4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = disc_value,
                                else => {},
                            }

                            // Get Err variant's payload layout (BadUtf8 - also a tag_union)
                            const variants = self.runtime_layout_store.getTagUnionVariants(tu_data);
                            const err_variant_layout = self.runtime_layout_store.getLayout(variants.get(@intCast(err_index orelse 1)).payload_layout);

                            // BadUtf8 is a tag_union with record { problem, index } as its payload
                            if (err_variant_layout.tag == .tag_union) {
                                const inner_tu_data = self.runtime_layout_store.getTagUnionData(err_variant_layout.data.tag_union.idx);

                                // Write inner discriminant (BadUtf8 is index 0)
                                const inner_disc_ptr = ptr_u8 + inner_tu_data.discriminant_offset;
                                switch (inner_tu_data.discriminant_size) {
                                    1 => @as(*u8, @ptrCast(inner_disc_ptr)).* = 0,
                                    2 => @as(*u16, @ptrCast(@alignCast(inner_disc_ptr))).* = 0,
                                    4 => @as(*u32, @ptrCast(@alignCast(inner_disc_ptr))).* = 0,
                                    else => {},
                                }

                                // Get BadUtf8's payload layout (should be record { problem, index })
                                const inner_variants = self.runtime_layout_store.getTagUnionVariants(inner_tu_data);
                                const record_layout = self.runtime_layout_store.getLayout(inner_variants.get(0).payload_layout);

                                if (record_layout.tag == .record) {
                                    // Write problem field
                                    if (self.runtime_layout_store.getRecordFieldOffsetByName(
                                        record_layout.data.record.idx,
                                        self.env.idents.problem,
                                    )) |problem_offset| {
                                        const problem_ptr: *u8 = @ptrCast(@alignCast(ptr_u8 + problem_offset));
                                        problem_ptr.* = @intFromEnum(result.problem_code);
                                    }

                                    // Write index field
                                    if (self.runtime_layout_store.getRecordFieldOffsetByName(
                                        record_layout.data.record.idx,
                                        self.env.idents.index,
                                    )) |index_offset| {
                                        const index_ptr: *u64 = @ptrCast(@alignCast(ptr_u8 + index_offset));
                                        index_ptr.* = result.byte_index;
                                    }
                                }
                            }
                        }

                        dest.is_initialized = true;
                        return dest;
                    } else {
                        self.triggerCrash("str_from_utf8: unexpected result layout for Err", false, roc_ops);
                        return error.Crash;
                    }
                }
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

                // Get the proper List(Str) type for rt_var
                const list_str_rt_var = try self.mkListStrTypeRuntime();
                var out = try self.pushRaw(result_layout, 0, list_str_rt_var);
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
                const str_rt_var = try self.getCanonicalStrRuntimeVar();
                var out = try self.pushRaw(result_layout, 0, str_rt_var);
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
                const result_rt_var = try self.runtime_types.fresh();
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
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
            .list_with_capacity => {
                // List.with_capacity : U64 -> List(a)
                // Creates an empty list with preallocated capacity
                std.debug.assert(args.len == 1); // low-level .list_with_capacity expects 1 argument

                const capacity_arg = args[0];
                const capacity: u64 = @intCast(capacity_arg.asI128());

                // Get the return type to determine element layout
                const result_rt_var = return_rt_var orelse unreachable;
                const result_layout = try self.getRuntimeLayout(result_rt_var);

                // Handle ZST lists specially - they don't actually allocate
                if (result_layout.tag == .list_of_zst) {
                    // For ZST lists, capacity doesn't matter - just return an empty list
                    var out = try self.pushRaw(result_layout, 0, result_rt_var);
                    out.is_initialized = false;
                    const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                    result_ptr.* = builtins.list.RocList.empty();
                    out.is_initialized = true;
                    return out;
                }

                // Get element layout from the list layout
                std.debug.assert(result_layout.tag == .list);
                const elem_layout_idx = result_layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Set up context for refcount callbacks
                const elem_rt_var = try self.runtime_types.fresh();
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
                    .elem_rt_var = elem_rt_var,
                    .roc_ops = roc_ops,
                };

                // Create empty list with capacity
                const result_list = builtins.list.listWithCapacity(
                    capacity,
                    elem_alignment_u32,
                    elem_size,
                    elements_refcounted,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementInc else &builtins.list.rcNone,
                    roc_ops,
                );

                // Allocate space for the result list
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
                out.is_initialized = false;

                // Copy the result list structure to the output
                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
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
                    const elem_rt_var = return_rt_var orelse try self.runtime_types.fresh();
                    return StackValue{
                        .layout = elem_layout,
                        .ptr = null,
                        .is_initialized = true,
                        .rt_var = elem_rt_var,
                    };
                }

                // Get pointer to element (no bounds checking!)
                const elem_ptr = builtins.list.listGetUnsafe(roc_list.*, @intCast(index), elem_size);
                // Null pointer from list_get_unsafe is a compiler bug - bounds should have been checked
                std.debug.assert(elem_ptr != null);

                // Get element runtime type from the list's attached type.
                // Priority: extract from list's concrete type first, as it has actual type info.
                // Only fall back to return_rt_var if it's concrete and list type is polymorphic.
                const elem_rt_var: types.Var = blk: {
                    // First try extracting from the list's attached type - this has concrete type info
                    const list_resolved = self.runtime_types.resolveVar(list_arg.rt_var);
                    if (list_resolved.desc.content == .structure) {
                        if (list_resolved.desc.content.structure == .nominal_type) {
                            const nom = list_resolved.desc.content.structure.nominal_type;
                            const vars = self.runtime_types.sliceVars(nom.vars.nonempty);
                            // For List(elem), vars[0] is backing, vars[1] is element type
                            if (vars.len == 2) {
                                const elem_var = vars[1];
                                // Follow aliases to check if underlying type is concrete
                                var elem_resolved = self.runtime_types.resolveVar(elem_var);
                                var unwrap_count: u32 = 0;
                                while (elem_resolved.desc.content == .alias and unwrap_count < 100) : (unwrap_count += 1) {
                                    const backing = self.runtime_types.getAliasBackingVar(elem_resolved.desc.content.alias);
                                    elem_resolved = self.runtime_types.resolveVar(backing);
                                }
                                // If element type is concrete (structure or alias to structure), create a fresh copy
                                // to avoid corruption from later unifications during equality checking
                                if (elem_resolved.desc.content == .structure) {
                                    const fresh_var = try self.runtime_types.freshFromContent(elem_resolved.desc.content);
                                    break :blk fresh_var;
                                }
                                // If element type got corrupted (content is .err), skip to fallbacks
                                // instead of using the corrupted type
                                if (elem_resolved.desc.content != .err) {
                                    // If element type is a flex var, try flex_type_context for mapped type
                                    if (elem_resolved.desc.content == .flex and self.flex_type_context.count() > 0) {
                                        var it = self.flex_type_context.iterator();
                                        while (it.next()) |entry| {
                                            const mapped_var = entry.value_ptr.*;
                                            const mapped_resolved = self.runtime_types.resolveVar(mapped_var);
                                            if (mapped_resolved.desc.content == .structure) {
                                                const fresh_var = try self.runtime_types.freshFromContent(mapped_resolved.desc.content);
                                                break :blk fresh_var;
                                            }
                                        }
                                    }
                                    // Element type is not concrete but we have it from the list
                                    // Still create a fresh copy to avoid corruption
                                    const fresh_var = try self.runtime_types.freshFromContent(elem_resolved.desc.content);
                                    break :blk fresh_var;
                                }
                                // Element type is corrupted (.err) - fall through to other fallbacks
                            }
                        }
                    }
                    // List came from polymorphic context - try return_rt_var if it's concrete
                    if (return_rt_var) |rv| {
                        var rv_resolved = self.runtime_types.resolveVar(rv);
                        var unwrap_count: u32 = 0;
                        while (rv_resolved.desc.content == .alias and unwrap_count < 100) : (unwrap_count += 1) {
                            const backing = self.runtime_types.getAliasBackingVar(rv_resolved.desc.content.alias);
                            rv_resolved = self.runtime_types.resolveVar(backing);
                        }
                        if (rv_resolved.desc.content == .structure) {
                            break :blk rv;
                        }
                    }
                    // Check flex_type_context for concrete type
                    if ((list_resolved.desc.content == .flex or list_resolved.desc.content == .rigid) and
                        self.flex_type_context.count() > 0)
                    {
                        var it = self.flex_type_context.iterator();
                        while (it.next()) |entry| {
                            const mapped_var = entry.value_ptr.*;
                            const mapped_resolved = self.runtime_types.resolveVar(mapped_var);
                            if (mapped_resolved.desc.content == .structure and
                                mapped_resolved.desc.content.structure == .nominal_type)
                            {
                                const nom = mapped_resolved.desc.content.structure.nominal_type;
                                const vars = self.runtime_types.sliceVars(nom.vars.nonempty);
                                if (vars.len == 2) {
                                    break :blk vars[1];
                                }
                            }
                        }
                    }
                    // Final fallback: create type from layout (handles corrupted types)
                    break :blk try self.createTypeFromLayout(elem_layout);
                };

                // Create StackValue pointing to the element
                const elem_value = StackValue{
                    .layout = elem_layout,
                    .ptr = @ptrCast(elem_ptr.?),
                    .is_initialized = true,
                    .rt_var = elem_rt_var,
                };

                // Copy to new location and increment refcount
                return try self.pushCopy(elem_value);
            },
            .list_sort_with => {
                // list_sort_with is handled specially in call_invoke_closure continuation
                // because it requires continuation-based evaluation for the comparison function
                self.triggerCrash("list_sort_with should be handled in call_invoke_closure, not callLowLevelBuiltin", false, roc_ops);
                return error.Crash;
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

                // Get element layout - handle list_of_zst by checking both lists for a proper element layout.
                // When concatenating a list_of_zst (e.g., empty list []) with a regular list,
                // we need to use the element layout from the regular list.
                const elem_layout_result: struct { elem_layout: Layout, result_layout: Layout } = blk: {
                    // Try to get element layout from list_a first
                    if (list_a_arg.layout.tag == .list) {
                        const elem_idx = list_a_arg.layout.data.list;
                        const elem_lay = self.runtime_layout_store.getLayout(elem_idx);
                        // Check if this is actually a non-ZST element
                        if (self.runtime_layout_store.layoutSize(elem_lay) > 0) {
                            break :blk .{ .elem_layout = elem_lay, .result_layout = list_a_arg.layout };
                        }
                    }
                    // Try list_b
                    if (list_b_arg.layout.tag == .list) {
                        const elem_idx = list_b_arg.layout.data.list;
                        const elem_lay = self.runtime_layout_store.getLayout(elem_idx);
                        if (self.runtime_layout_store.layoutSize(elem_lay) > 0) {
                            break :blk .{ .elem_layout = elem_lay, .result_layout = list_b_arg.layout };
                        }
                    }
                    // Both are ZST - use ZST layout
                    break :blk .{ .elem_layout = Layout.zst(), .result_layout = list_a_arg.layout };
                };
                const elem_layout = elem_layout_result.elem_layout;
                const result_layout = elem_layout_result.result_layout;
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // If either list is empty, just return a copy of the other (avoid allocation)
                // Since ownership is consume, we must decref the empty list.
                if (list_a.len() == 0) {
                    list_a_arg.decref(&self.runtime_layout_store, roc_ops);
                    // list_b ownership is transferred to the result (pushCopy increfs)
                    const result = try self.pushCopy(list_b_arg);
                    list_b_arg.decref(&self.runtime_layout_store, roc_ops);
                    return result;
                }
                if (list_b.len() == 0) {
                    list_b_arg.decref(&self.runtime_layout_store, roc_ops);
                    // list_a ownership is transferred to the result (pushCopy increfs)
                    const result = try self.pushCopy(list_a_arg);
                    list_a_arg.decref(&self.runtime_layout_store, roc_ops);
                    return result;
                }

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Create a fresh list by allocating and copying elements.
                // We can't use the builtin listConcat here because it consumes its input lists
                // (handles refcounting internally), but we're working with StackValues that
                // have their own lifetime management - the caller will decref the args.
                const total_count = list_a.len() + list_b.len();
                const result_rt_var = return_rt_var orelse list_a_arg.rt_var;
                var out = try self.pushRaw(result_layout, 0, result_rt_var);
                out.is_initialized = false;
                const header: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));

                const runtime_list = builtins.list.RocList.allocateExact(
                    elem_alignment_u32,
                    total_count,
                    elem_size,
                    elements_refcounted,
                    roc_ops,
                );

                if (elem_size > 0) {
                    if (runtime_list.bytes) |buffer| {
                        // Copy elements from list_a
                        if (list_a.bytes) |src_a| {
                            @memcpy(buffer[0 .. list_a.len() * elem_size], src_a[0 .. list_a.len() * elem_size]);
                        }
                        // Copy elements from list_b
                        if (list_b.bytes) |src_b| {
                            const offset = list_a.len() * elem_size;
                            @memcpy(buffer[offset .. offset + list_b.len() * elem_size], src_b[0 .. list_b.len() * elem_size]);
                        }
                    }
                }

                header.* = runtime_list;
                out.is_initialized = true;

                // Handle refcounting for copied elements - increment refcount for each element
                // since we copied them (the elements are now shared with the original lists)
                if (elements_refcounted) {
                    const elem_rt_var = try self.runtime_types.fresh();
                    var refcount_context = RefcountContext{
                        .layout_store = &self.runtime_layout_store,
                        .elem_layout = elem_layout,
                        .elem_rt_var = elem_rt_var,
                        .roc_ops = roc_ops,
                    };
                    if (runtime_list.bytes) |buffer| {
                        var i: usize = 0;
                        while (i < total_count) : (i += 1) {
                            listElementInc(@ptrCast(&refcount_context), buffer + i * elem_size);
                        }
                    }
                }

                // list_concat has consume ownership, so we must decref the input lists.
                // The elements were already increffed above, and decref on the lists
                // will decref their elements (if they're unique), resulting in net-zero
                // refcount change for shared elements.
                list_a_arg.decref(&self.runtime_layout_store, roc_ops);
                list_b_arg.decref(&self.runtime_layout_store, roc_ops);

                return out;
            },
            .list_append => {
                // List.append: List(a), a -> List(a)
                std.debug.assert(args.len == 2); // low-level .list_append expects 2 arguments

                const roc_list_arg = args[0];
                const elt_arg = args[1];

                std.debug.assert(roc_list_arg.ptr != null); // low-level .list_append expects non-null list pointer
                std.debug.assert(elt_arg.ptr != null); // low-level .list_append expects non-null 2nd argument

                // Extract element layout from List(a)
                std.debug.assert(roc_list_arg.layout.tag == .list or roc_list_arg.layout.tag == .list_of_zst); // low-level .list_append expects list layout

                // Format arguments into proper types
                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(roc_list_arg.ptr.?));
                const non_null_bytes: [*]u8 = @ptrCast(elt_arg.ptr.?);
                const append_elt: builtins.list.Opaque = non_null_bytes;

                // Get element layout
                const elem_layout_idx = roc_list_arg.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size: u32 = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Determine if list can be mutated in place
                const update_mode = if (roc_list.isUnique()) builtins.utils.UpdateMode.InPlace else builtins.utils.UpdateMode.Immutable;

                // Set up context for refcount callbacks
                const elem_rt_var = try self.runtime_types.fresh();
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
                    .elem_rt_var = elem_rt_var,
                    .roc_ops = roc_ops,
                };

                const copy_fn: builtins.list.CopyFallbackFn = copy: switch (elem_layout.tag) {
                    .scalar => {
                        switch (elem_layout.data.scalar.tag) {
                            .str => break :copy &builtins.list.copy_str,
                            .int => {
                                switch (elem_layout.data.scalar.data.int) {
                                    .u8 => break :copy &builtins.list.copy_u8,
                                    .u16 => break :copy &builtins.list.copy_u16,
                                    .u32 => break :copy &builtins.list.copy_u32,
                                    .u64 => break :copy &builtins.list.copy_u64,
                                    .u128 => break :copy &builtins.list.copy_u128,
                                    .i8 => break :copy &builtins.list.copy_i8,
                                    .i16 => break :copy &builtins.list.copy_i16,
                                    .i32 => break :copy &builtins.list.copy_i32,
                                    .i64 => break :copy &builtins.list.copy_i64,
                                    .i128 => break :copy &builtins.list.copy_i128,
                                }
                            },
                            else => break :copy &builtins.list.copy_fallback,
                        }
                    },
                    .box => break :copy &builtins.list.copy_box,
                    .box_of_zst => break :copy &builtins.list.copy_box_zst,
                    .list => break :copy &builtins.list.copy_list,
                    .list_of_zst => break :copy &builtins.list.copy_list_zst,
                    else => break :copy &builtins.list.copy_fallback,
                };

                const result_list = builtins.list.listAppend(roc_list.*, elem_alignment_u32, append_elt, elem_size, elements_refcounted, if (elements_refcounted) @ptrCast(&refcount_context) else null, if (elements_refcounted) &listElementInc else &builtins.list.rcNone, update_mode, copy_fn, roc_ops);

                // Allocate space for the result list
                const result_layout = roc_list_arg.layout; // Same layout as input
                var out = try self.pushRaw(result_layout, 0, roc_list_arg.rt_var);
                out.is_initialized = false;

                // Copy the result list structure to the output
                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            .list_drop_at => {
                // List.drop_at : List(a), U64 -> List(a)
                std.debug.assert(args.len == 2); // low-level .list_drop_at expects 2 argument

                const list_arg = args[0];
                const drop_index_arg = args[1];
                const drop_index: u64 = @intCast(drop_index_arg.asI128());

                std.debug.assert(list_arg.layout.tag == .list or list_arg.layout.tag == .list_of_zst);

                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));

                // Get element layout from the list layout
                const elem_layout_idx = list_arg.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Set up context for refcount callbacks
                const elem_rt_var = try self.runtime_types.fresh();
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
                    .elem_rt_var = elem_rt_var,
                    .roc_ops = roc_ops,
                };

                // Return list with element at index dropped
                const result_list = builtins.list.listDropAt(
                    roc_list.*,
                    elem_alignment_u32,
                    elem_size,
                    elements_refcounted,
                    drop_index,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementInc else &builtins.list.rcNone,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementDec else &builtins.list.rcNone,
                    roc_ops,
                );

                // Allocate space for the result list
                const result_layout = list_arg.layout;
                var out = try self.pushRaw(result_layout, 0, list_arg.rt_var);
                out.is_initialized = false;

                // Copy the result list structure to the output
                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            .list_sublist => {
                // List.sublist : List(a), {start : U64, len : U64} -> List(a)
                std.debug.assert(args.len == 2); // low-level .list_sublist expects 2 argument

                // Check and extract first element as a typed RocList
                const list_arg = args[0];
                std.debug.assert(list_arg.layout.tag == .list or list_arg.layout.tag == .list_of_zst);
                const roc_list: *const builtins.list.RocList = @ptrCast(@alignCast(list_arg.ptr.?));

                // Access second argument as a record and extract its specific fields
                const sublist_config = args[1].asRecord(&self.runtime_layout_store) catch unreachable;
                // When fields are alphabetically sorted, 0 will be `len` and 1 will be `start`
                const field_rt = try self.runtime_types.fresh();
                const sublist_start_stack = sublist_config.getFieldByIndex(1, field_rt) catch unreachable;
                const field_rt2 = try self.runtime_types.fresh();
                const sublist_len_stack = sublist_config.getFieldByIndex(0, field_rt2) catch unreachable;
                const sublist_start: u64 = @intCast(sublist_start_stack.asI128());
                const sublist_len: u64 = @intCast(sublist_len_stack.asI128());

                // Get element layout from the list layout
                const elem_layout_idx = list_arg.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size = self.runtime_layout_store.layoutSize(elem_layout);
                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);

                // Determine if elements are refcounted
                const elements_refcounted = elem_layout.isRefcounted();

                // Set up context for refcount callbacks
                const elem_rt_var = try self.runtime_types.fresh();
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
                    .elem_rt_var = elem_rt_var,
                    .roc_ops = roc_ops,
                };

                // Return list with element at index dropped
                const result_list = builtins.list.listSublist(
                    roc_list.*,
                    elem_alignment_u32,
                    elem_size,
                    elements_refcounted,
                    sublist_start,
                    sublist_len,
                    if (elements_refcounted) @ptrCast(&refcount_context) else null,
                    if (elements_refcounted) &listElementDec else &builtins.list.rcNone,
                    roc_ops,
                );

                // Allocate space for the result list
                const result_layout = list_arg.layout;
                var out = try self.pushRaw(result_layout, 0, list_arg.rt_var);
                out.is_initialized = false;

                // Copy the result list structure to the output
                const result_ptr: *builtins.list.RocList = @ptrCast(@alignCast(out.ptr.?));
                result_ptr.* = result_list;

                out.is_initialized = true;
                return out;
            },
            // .set_is_empty => {
            //     // TODO: implement Set.is_empty
            //     self.triggerCrash("Set.is_empty not yet implemented", false, roc_ops);
            //     return error.Crash;
            // },
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
                const result: bool = switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| l == r,
                        .dec => |r| l == @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| l.num == r.num,
                        .int => |r| l.num == @as(i128, r) * RocDec.one_point_zero_i128,
                        else => return error.TypeMismatch,
                    },
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
                const result: bool = switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| l > r,
                        // Int vs Dec: convert Dec to Int for comparison
                        .dec => |r| l > @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| l > r,
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| l > r,
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| l.num > r.num,
                        // Dec vs Int: convert Int to Dec for comparison
                        .int => |r| l.num > @as(i128, r) * RocDec.one_point_zero_i128,
                        else => return error.TypeMismatch,
                    },
                };
                return try self.makeBoolValue(result);
            },
            .num_is_gte => {
                // num.is_gte : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_gte expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result: bool = switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| l >= r,
                        .dec => |r| l >= @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| l >= r,
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| l >= r,
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| l.num >= r.num,
                        .int => |r| l.num >= @as(i128, r) * RocDec.one_point_zero_i128,
                        else => return error.TypeMismatch,
                    },
                };
                return try self.makeBoolValue(result);
            },
            .num_is_lt => {
                // num.is_lt : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_lt expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result: bool = switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| l < r,
                        .dec => |r| l < @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| l < r,
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| l < r,
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| l.num < r.num,
                        .int => |r| l.num < @as(i128, r) * RocDec.one_point_zero_i128,
                        else => return error.TypeMismatch,
                    },
                };
                return try self.makeBoolValue(result);
            },
            .num_is_lte => {
                // num.is_lte : num, num -> Bool
                std.debug.assert(args.len == 2); // low-level .num_is_lte expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result: bool = switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| l <= r,
                        .dec => |r| l <= @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| l <= r,
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| l <= r,
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| l.num <= r.num,
                        .int => |r| l.num <= @as(i128, r) * RocDec.one_point_zero_i128,
                        else => return error.TypeMismatch,
                    },
                };
                return try self.makeBoolValue(result);
            },

            // Numeric arithmetic operations
            .num_negate => {
                // num.negate : num -> num (signed types only)
                std.debug.assert(args.len == 1); // low-level .num_negate expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
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
            .num_abs => {
                // num.abs : num -> num (signed types only)
                std.debug.assert(args.len == 1); // low-level .num_abs expects 1 argument
                const num_val = try self.extractNumericValue(args[0]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (num_val) {
                    .int => |i| try out.setInt(if (i < 0) -i else i),
                    .f32 => |f| out.setF32(@abs(f)),
                    .f64 => |f| out.setF64(@abs(f)),
                    .dec => |d| out.setDec(RocDec{ .num = if (d.num < 0) -d.num else d.num }),
                }
                out.is_initialized = true;
                return out;
            },
            .num_abs_diff => {
                // num.abs_diff : num, num -> num (all numeric types)
                // For signed types, returns unsigned counterpart
                std.debug.assert(args.len == 2); // low-level .num_abs_diff expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| {
                            const diff = if (l > r) l - r else r - l;
                            try out.setInt(diff);
                        },
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| out.setF32(@abs(l - r)),
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| out.setF64(@abs(l - r)),
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| {
                            const diff = l.num - r.num;
                            out.setDec(RocDec{ .num = if (diff < 0) -diff else diff });
                        },
                        else => return error.TypeMismatch,
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_plus => {
                std.debug.assert(args.len == 2); // low-level .num_plus expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| try out.setInt(l + r),
                        .dec => |r| try out.setInt(l + @divTrunc(r.num, RocDec.one_point_zero_i128)),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| out.setF32(l + r),
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| out.setF64(l + r),
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| out.setDec(RocDec.add(l, r, roc_ops)),
                        .int => |r| out.setDec(RocDec.add(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                        else => return error.TypeMismatch,
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_minus => {
                std.debug.assert(args.len == 2); // low-level .num_minus expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| try out.setInt(l - r),
                        .dec => |r| try out.setInt(l - @divTrunc(r.num, RocDec.one_point_zero_i128)),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| out.setF32(l - r),
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| out.setF64(l - r),
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| out.setDec(RocDec.sub(l, r, roc_ops)),
                        .int => |r| out.setDec(RocDec.sub(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                        else => return error.TypeMismatch,
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_times => {
                std.debug.assert(args.len == 2); // low-level .num_times expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| try out.setInt(l * r),
                        .dec => |r| try out.setInt(l * @divTrunc(r.num, RocDec.one_point_zero_i128)),
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| out.setF32(l * r),
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| out.setF64(l * r),
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| out.setDec(RocDec.mul(l, r, roc_ops)),
                        .int => |r| out.setDec(RocDec.mul(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                        else => return error.TypeMismatch,
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_div_by => {
                std.debug.assert(args.len == 2); // low-level .num_div_by expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            try out.setInt(@divTrunc(l, r));
                        },
                        .dec => |r| {
                            const r_int = @divTrunc(r.num, RocDec.one_point_zero_i128);
                            if (r_int == 0) return error.DivisionByZero;
                            try out.setInt(@divTrunc(l, r_int));
                        },
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF32(l / r);
                        },
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF64(l / r);
                        },
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| {
                            if (r.num == 0) return error.DivisionByZero;
                            out.setDec(RocDec.div(l, r, roc_ops));
                        },
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            const r_dec = RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 };
                            out.setDec(RocDec.div(l, r_dec, roc_ops));
                        },
                        else => return error.TypeMismatch,
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

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            try out.setInt(@divTrunc(l, r));
                        },
                        .dec => |r| {
                            const r_int = @divTrunc(r.num, RocDec.one_point_zero_i128);
                            if (r_int == 0) return error.DivisionByZero;
                            try out.setInt(@divTrunc(l, r_int));
                        },
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF32(@trunc(l / r));
                        },
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF64(@trunc(l / r));
                        },
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| {
                            // For Dec, div and div_trunc are the same since it's already integer-like
                            if (r.num == 0) return error.DivisionByZero;
                            out.setDec(RocDec.div(l, r, roc_ops));
                        },
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            const r_dec = RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 };
                            out.setDec(RocDec.div(l, r_dec, roc_ops));
                        },
                        else => return error.TypeMismatch,
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

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            try out.setInt(@rem(l, r));
                        },
                        .dec => |r| {
                            const r_int = @divTrunc(r.num, RocDec.one_point_zero_i128);
                            if (r_int == 0) return error.DivisionByZero;
                            try out.setInt(@rem(l, r_int));
                        },
                        else => return error.TypeMismatch,
                    },
                    .f32 => |l| switch (rhs) {
                        .f32 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF32(@rem(l, r));
                        },
                        else => return error.TypeMismatch,
                    },
                    .f64 => |l| switch (rhs) {
                        .f64 => |r| {
                            if (r == 0) return error.DivisionByZero;
                            out.setF64(@rem(l, r));
                        },
                        else => return error.TypeMismatch,
                    },
                    .dec => |l| switch (rhs) {
                        .dec => |r| {
                            if (r.num == 0) return error.DivisionByZero;
                            out.setDec(RocDec.rem(l, r, roc_ops));
                        },
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            const r_dec = RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 };
                            out.setDec(RocDec.rem(l, r_dec, roc_ops));
                        },
                        else => return error.TypeMismatch,
                    },
                }
                out.is_initialized = true;
                return out;
            },
            .num_mod_by => {
                std.debug.assert(args.len == 2); // low-level .num_mod_by expects 2 arguments
                const lhs = try self.extractNumericValue(args[0]);
                const rhs = try self.extractNumericValue(args[1]);
                const result_layout = args[0].layout;

                var out = try self.pushRaw(result_layout, 0, args[0].rt_var);
                out.is_initialized = false;

                switch (lhs) {
                    .int => |l| switch (rhs) {
                        .int => |r| {
                            if (r == 0) return error.DivisionByZero;
                            try out.setInt(@mod(l, r));
                        },
                        else => return error.TypeMismatch,
                    },
                    else => return error.TypeMismatch,
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
                    var out = try self.pushRaw(result_layout, 0, result_rt_var);
                    out.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try out.setInt(@intCast(tag_idx));
                    out.is_initialized = true;
                    return out;
                } else if (result_layout.tag == .record) {
                    // Record { tag, payload }
                    var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
                    const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse unreachable;
                    const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse unreachable;

                    // Write tag discriminant
                    const field_rt = try self.runtime_types.fresh();
                    const tag_field = try acc.getFieldByIndex(tag_field_idx, field_rt);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area
                    const field_rt2 = try self.runtime_types.fresh();
                    const payload_field = try acc.getFieldByIndex(payload_field_idx, field_rt2);
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
                const field_rt = try self.runtime_types.fresh();
                const is_neg_field = acc.getFieldByIndex(is_neg_idx, field_rt) catch unreachable;
                const is_negative = getRuntimeU8(is_neg_field) != 0;

                // Get digits_before_pt field (List(U8))
                const before_idx = acc.findFieldIndex(layout_env.idents.digits_before_pt) orelse unreachable;
                const field_rt2 = try self.runtime_types.fresh();
                const before_field = acc.getFieldByIndex(before_idx, field_rt2) catch unreachable;

                // Get digits_after_pt field (List(U8))
                const after_idx = acc.findFieldIndex(layout_env.idents.digits_after_pt) orelse unreachable;
                const field_rt3 = try self.runtime_types.fresh();
                const after_field = acc.getFieldByIndex(after_idx, field_rt3) catch unreachable;

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
                    var out = try self.pushRaw(result_layout, 0, result_rt_var);
                    out.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try out.setInt(@intCast(tag_idx));
                    out.is_initialized = true;
                    return out;
                } else if (result_layout.tag == .record) {
                    // Record { tag, payload }
                    var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                    var result_acc = try dest.asRecord(&self.runtime_layout_store);
                    // Use layout_env for field lookups since record fields use layout store's env idents
                    // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
                    const tag_field_idx = result_acc.findFieldIndex(layout_env.idents.tag) orelse unreachable;
                    const payload_field_idx = result_acc.findFieldIndex(layout_env.idents.payload) orelse unreachable;

                    // Write tag discriminant
                    const tag_rt = try self.runtime_types.fresh();
                    const tag_field = try result_acc.getFieldByIndex(tag_field_idx, tag_rt);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area
                    const payload_rt = try self.runtime_types.fresh();
                    const payload_field = try result_acc.getFieldByIndex(payload_field_idx, payload_rt);
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
                                        .rt_var = err_payload_var.?,
                                    };
                                    var err_acc = try err_inner.asRecord(&self.runtime_layout_store);

                                    // Set the tag to InvalidNumeral (index 0, assuming it's the first/only tag)
                                    // Use layout store's env for field lookup to match comptime_evaluator
                                    if (err_acc.findFieldIndex(layout_env.idents.tag)) |inner_tag_idx| {
                                        const inner_tag_rt = try self.runtime_types.fresh();
                                        const inner_tag_field = try err_acc.getFieldByIndex(inner_tag_idx, inner_tag_rt);
                                        if (inner_tag_field.layout.tag == .scalar and inner_tag_field.layout.data.scalar.tag == .int) {
                                            var inner_tmp = inner_tag_field;
                                            inner_tmp.is_initialized = false;
                                            try inner_tmp.setInt(0); // InvalidNumeral tag index
                                        }
                                    }

                                    // Set the payload to the Str
                                    if (err_acc.findFieldIndex(layout_env.idents.payload)) |inner_payload_idx| {
                                        const inner_payload_rt = try self.runtime_types.fresh();
                                        const inner_payload_field = try err_acc.getFieldByIndex(inner_payload_idx, inner_payload_rt);
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
                    var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                    var result_acc = try dest.asTuple(&self.runtime_layout_store);

                    // Element 0 is payload, Element 1 is tag discriminant
                    // getElement takes original index directly

                    // Write tag discriminant (element 1)
                    const tag_elem_rt_var = try self.runtime_types.fresh();
                    const tag_field = try result_acc.getElement(1, tag_elem_rt_var);
                    // Tag field should be scalar int - if not, it's a compiler bug
                    std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
                    var tmp = tag_field;
                    tmp.is_initialized = false;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    try tmp.setInt(@intCast(tag_idx));

                    // Clear payload area (element 0)
                    const payload_elem_rt_var = try self.runtime_types.fresh();
                    const payload_field = try result_acc.getElement(0, payload_elem_rt_var);
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
                } else if (result_layout.tag == .tag_union) {
                    // Tag union layout: payload at offset 0, discriminant at discriminant_offset
                    var dest = try self.pushRaw(result_layout, 0, result_rt_var);
                    const tu_data = self.runtime_layout_store.getTagUnionData(result_layout.data.tag_union.idx);

                    // Write tag discriminant at discriminant_offset
                    const base_ptr: [*]u8 = @ptrCast(dest.ptr.?);
                    const disc_ptr = base_ptr + tu_data.discriminant_offset;
                    const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
                    switch (tu_data.discriminant_size) {
                        1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(tag_idx),
                        2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                        4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                        8 => @as(*u64, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                        else => {},
                    }

                    // Clear payload area (at offset 0)
                    const payload_size = tu_data.discriminant_offset; // Payload spans from 0 to discriminant_offset
                    if (payload_size > 0) {
                        @memset(base_ptr[0..payload_size], 0);
                    }

                    // Write payload for Ok case
                    if (in_range and ok_payload_var != null) {
                        const num_layout = try self.getRuntimeLayout(ok_payload_var.?);
                        const payload_ptr: *anyopaque = @ptrCast(base_ptr);
                        if (num_layout.tag == .scalar and num_layout.data.scalar.tag == .int) {
                            const int_type = num_layout.data.scalar.data.int;
                            if (is_negative) {
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
                                    else => {},
                                }
                            } else {
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
                            const frac_precision = num_layout.data.scalar.data.frac;
                            const float_value: f64 = if (is_negative)
                                -@as(f64, @floatFromInt(value))
                            else
                                @as(f64, @floatFromInt(value));

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

                    // Store error message for Err case (same as tuple branch)
                    if (!in_range) {
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
                            self.last_error_message = msg;
                        }
                    }

                    dest.is_initialized = true;
                    return dest;
                }

                // Unsupported result layout is a compiler bug
                unreachable;
            },
            .num_from_str => {
                // num.from_str : Str -> Try(num, [BadNumStr])
                // Dispatch to type-specific parsing using comptime generics
                std.debug.assert(args.len == 1);
                const str_arg = args[0];
                std.debug.assert(str_arg.ptr != null);
                const roc_str: *const RocStr = @ptrCast(@alignCast(str_arg.ptr.?));

                const result_rt_var = return_rt_var orelse unreachable;
                const ok_payload_var = try self.getTryOkPayloadVar(result_rt_var);

                if (ok_payload_var) |payload_var| {
                    const num_layout = try self.getRuntimeLayout(payload_var);
                    if (num_layout.tag == .scalar) {
                        if (num_layout.data.scalar.tag == .int) {
                            return switch (num_layout.data.scalar.data.int) {
                                .u8 => self.numFromStrInt(u8, roc_str, result_rt_var),
                                .i8 => self.numFromStrInt(i8, roc_str, result_rt_var),
                                .u16 => self.numFromStrInt(u16, roc_str, result_rt_var),
                                .i16 => self.numFromStrInt(i16, roc_str, result_rt_var),
                                .u32 => self.numFromStrInt(u32, roc_str, result_rt_var),
                                .i32 => self.numFromStrInt(i32, roc_str, result_rt_var),
                                .u64 => self.numFromStrInt(u64, roc_str, result_rt_var),
                                .i64 => self.numFromStrInt(i64, roc_str, result_rt_var),
                                .u128 => self.numFromStrInt(u128, roc_str, result_rt_var),
                                .i128 => self.numFromStrInt(i128, roc_str, result_rt_var),
                            };
                        } else if (num_layout.data.scalar.tag == .frac) {
                            return switch (num_layout.data.scalar.data.frac) {
                                .f32 => self.numFromStrFloat(f32, roc_str, result_rt_var),
                                .f64 => self.numFromStrFloat(f64, roc_str, result_rt_var),
                                .dec => self.numFromStrDec(roc_str, result_rt_var),
                            };
                        }
                    }
                }
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

                const str_rt_var = try self.getCanonicalStrRuntimeVar();
                const value = try self.pushStr(str_rt_var);
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
            .u8_to_i8_wrap => return self.intConvertWrap(u8, i8, args),
            .u8_to_i8_try => return self.intConvertTry(u8, i8, args, return_rt_var),
            .u8_to_i16 => return self.intConvert(u8, i16, args),
            .u8_to_i32 => return self.intConvert(u8, i32, args),
            .u8_to_i64 => return self.intConvert(u8, i64, args),
            .u8_to_i128 => return self.intConvert(u8, i128, args),
            .u8_to_u16 => return self.intConvert(u8, u16, args),
            .u8_to_u32 => return self.intConvert(u8, u32, args),
            .u8_to_u64 => return self.intConvert(u8, u64, args),
            .u8_to_u128 => return self.intConvert(u8, u128, args),
            .u8_to_f32 => return self.intToFloat(u8, f32, args),
            .u8_to_f64 => return self.intToFloat(u8, f64, args),
            .u8_to_dec => return self.intToDec(u8, args),

            // I8 conversion operations
            .i8_to_i16 => return self.intConvert(i8, i16, args),
            .i8_to_i32 => return self.intConvert(i8, i32, args),
            .i8_to_i64 => return self.intConvert(i8, i64, args),
            .i8_to_i128 => return self.intConvert(i8, i128, args),
            .i8_to_u8_wrap => return self.intConvertWrap(i8, u8, args),
            .i8_to_u8_try => return self.intConvertTry(i8, u8, args, return_rt_var),
            .i8_to_u16_wrap => return self.intConvertWrap(i8, u16, args),
            .i8_to_u16_try => return self.intConvertTry(i8, u16, args, return_rt_var),
            .i8_to_u32_wrap => return self.intConvertWrap(i8, u32, args),
            .i8_to_u32_try => return self.intConvertTry(i8, u32, args, return_rt_var),
            .i8_to_u64_wrap => return self.intConvertWrap(i8, u64, args),
            .i8_to_u64_try => return self.intConvertTry(i8, u64, args, return_rt_var),
            .i8_to_u128_wrap => return self.intConvertWrap(i8, u128, args),
            .i8_to_u128_try => return self.intConvertTry(i8, u128, args, return_rt_var),
            .i8_to_f32 => return self.intToFloat(i8, f32, args),
            .i8_to_f64 => return self.intToFloat(i8, f64, args),
            .i8_to_dec => return self.intToDec(i8, args),

            // U16 conversion operations
            .u16_to_i8_wrap => return self.intConvertWrap(u16, i8, args),
            .u16_to_i8_try => return self.intConvertTry(u16, i8, args, return_rt_var),
            .u16_to_i16_wrap => return self.intConvertWrap(u16, i16, args),
            .u16_to_i16_try => return self.intConvertTry(u16, i16, args, return_rt_var),
            .u16_to_i32 => return self.intConvert(u16, i32, args),
            .u16_to_i64 => return self.intConvert(u16, i64, args),
            .u16_to_i128 => return self.intConvert(u16, i128, args),
            .u16_to_u8_wrap => return self.intConvertWrap(u16, u8, args),
            .u16_to_u8_try => return self.intConvertTry(u16, u8, args, return_rt_var),
            .u16_to_u32 => return self.intConvert(u16, u32, args),
            .u16_to_u64 => return self.intConvert(u16, u64, args),
            .u16_to_u128 => return self.intConvert(u16, u128, args),
            .u16_to_f32 => return self.intToFloat(u16, f32, args),
            .u16_to_f64 => return self.intToFloat(u16, f64, args),
            .u16_to_dec => return self.intToDec(u16, args),

            // I16 conversion operations
            .i16_to_i8_wrap => return self.intConvertWrap(i16, i8, args),
            .i16_to_i8_try => return self.intConvertTry(i16, i8, args, return_rt_var),
            .i16_to_i32 => return self.intConvert(i16, i32, args),
            .i16_to_i64 => return self.intConvert(i16, i64, args),
            .i16_to_i128 => return self.intConvert(i16, i128, args),
            .i16_to_u8_wrap => return self.intConvertWrap(i16, u8, args),
            .i16_to_u8_try => return self.intConvertTry(i16, u8, args, return_rt_var),
            .i16_to_u16_wrap => return self.intConvertWrap(i16, u16, args),
            .i16_to_u16_try => return self.intConvertTry(i16, u16, args, return_rt_var),
            .i16_to_u32_wrap => return self.intConvertWrap(i16, u32, args),
            .i16_to_u32_try => return self.intConvertTry(i16, u32, args, return_rt_var),
            .i16_to_u64_wrap => return self.intConvertWrap(i16, u64, args),
            .i16_to_u64_try => return self.intConvertTry(i16, u64, args, return_rt_var),
            .i16_to_u128_wrap => return self.intConvertWrap(i16, u128, args),
            .i16_to_u128_try => return self.intConvertTry(i16, u128, args, return_rt_var),
            .i16_to_f32 => return self.intToFloat(i16, f32, args),
            .i16_to_f64 => return self.intToFloat(i16, f64, args),
            .i16_to_dec => return self.intToDec(i16, args),

            // U32 conversion operations
            .u32_to_i8_wrap => return self.intConvertWrap(u32, i8, args),
            .u32_to_i8_try => return self.intConvertTry(u32, i8, args, return_rt_var),
            .u32_to_i16_wrap => return self.intConvertWrap(u32, i16, args),
            .u32_to_i16_try => return self.intConvertTry(u32, i16, args, return_rt_var),
            .u32_to_i32_wrap => return self.intConvertWrap(u32, i32, args),
            .u32_to_i32_try => return self.intConvertTry(u32, i32, args, return_rt_var),
            .u32_to_i64 => return self.intConvert(u32, i64, args),
            .u32_to_i128 => return self.intConvert(u32, i128, args),
            .u32_to_u8_wrap => return self.intConvertWrap(u32, u8, args),
            .u32_to_u8_try => return self.intConvertTry(u32, u8, args, return_rt_var),
            .u32_to_u16_wrap => return self.intConvertWrap(u32, u16, args),
            .u32_to_u16_try => return self.intConvertTry(u32, u16, args, return_rt_var),
            .u32_to_u64 => return self.intConvert(u32, u64, args),
            .u32_to_u128 => return self.intConvert(u32, u128, args),
            .u32_to_f32 => return self.intToFloat(u32, f32, args),
            .u32_to_f64 => return self.intToFloat(u32, f64, args),
            .u32_to_dec => return self.intToDec(u32, args),

            // I32 conversion operations
            .i32_to_i8_wrap => return self.intConvertWrap(i32, i8, args),
            .i32_to_i8_try => return self.intConvertTry(i32, i8, args, return_rt_var),
            .i32_to_i16_wrap => return self.intConvertWrap(i32, i16, args),
            .i32_to_i16_try => return self.intConvertTry(i32, i16, args, return_rt_var),
            .i32_to_i64 => return self.intConvert(i32, i64, args),
            .i32_to_i128 => return self.intConvert(i32, i128, args),
            .i32_to_u8_wrap => return self.intConvertWrap(i32, u8, args),
            .i32_to_u8_try => return self.intConvertTry(i32, u8, args, return_rt_var),
            .i32_to_u16_wrap => return self.intConvertWrap(i32, u16, args),
            .i32_to_u16_try => return self.intConvertTry(i32, u16, args, return_rt_var),
            .i32_to_u32_wrap => return self.intConvertWrap(i32, u32, args),
            .i32_to_u32_try => return self.intConvertTry(i32, u32, args, return_rt_var),
            .i32_to_u64_wrap => return self.intConvertWrap(i32, u64, args),
            .i32_to_u64_try => return self.intConvertTry(i32, u64, args, return_rt_var),
            .i32_to_u128_wrap => return self.intConvertWrap(i32, u128, args),
            .i32_to_u128_try => return self.intConvertTry(i32, u128, args, return_rt_var),
            .i32_to_f32 => return self.intToFloat(i32, f32, args),
            .i32_to_f64 => return self.intToFloat(i32, f64, args),
            .i32_to_dec => return self.intToDec(i32, args),

            // U64 conversion operations
            .u64_to_i8_wrap => return self.intConvertWrap(u64, i8, args),
            .u64_to_i8_try => return self.intConvertTry(u64, i8, args, return_rt_var),
            .u64_to_i16_wrap => return self.intConvertWrap(u64, i16, args),
            .u64_to_i16_try => return self.intConvertTry(u64, i16, args, return_rt_var),
            .u64_to_i32_wrap => return self.intConvertWrap(u64, i32, args),
            .u64_to_i32_try => return self.intConvertTry(u64, i32, args, return_rt_var),
            .u64_to_i64_wrap => return self.intConvertWrap(u64, i64, args),
            .u64_to_i64_try => return self.intConvertTry(u64, i64, args, return_rt_var),
            .u64_to_i128 => return self.intConvert(u64, i128, args),
            .u64_to_u8_wrap => return self.intConvertWrap(u64, u8, args),
            .u64_to_u8_try => return self.intConvertTry(u64, u8, args, return_rt_var),
            .u64_to_u16_wrap => return self.intConvertWrap(u64, u16, args),
            .u64_to_u16_try => return self.intConvertTry(u64, u16, args, return_rt_var),
            .u64_to_u32_wrap => return self.intConvertWrap(u64, u32, args),
            .u64_to_u32_try => return self.intConvertTry(u64, u32, args, return_rt_var),
            .u64_to_u128 => return self.intConvert(u64, u128, args),
            .u64_to_f32 => return self.intToFloat(u64, f32, args),
            .u64_to_f64 => return self.intToFloat(u64, f64, args),
            .u64_to_dec => return self.intToDec(u64, args),

            // I64 conversion operations
            .i64_to_i8_wrap => return self.intConvertWrap(i64, i8, args),
            .i64_to_i8_try => return self.intConvertTry(i64, i8, args, return_rt_var),
            .i64_to_i16_wrap => return self.intConvertWrap(i64, i16, args),
            .i64_to_i16_try => return self.intConvertTry(i64, i16, args, return_rt_var),
            .i64_to_i32_wrap => return self.intConvertWrap(i64, i32, args),
            .i64_to_i32_try => return self.intConvertTry(i64, i32, args, return_rt_var),
            .i64_to_i128 => return self.intConvert(i64, i128, args),
            .i64_to_u8_wrap => return self.intConvertWrap(i64, u8, args),
            .i64_to_u8_try => return self.intConvertTry(i64, u8, args, return_rt_var),
            .i64_to_u16_wrap => return self.intConvertWrap(i64, u16, args),
            .i64_to_u16_try => return self.intConvertTry(i64, u16, args, return_rt_var),
            .i64_to_u32_wrap => return self.intConvertWrap(i64, u32, args),
            .i64_to_u32_try => return self.intConvertTry(i64, u32, args, return_rt_var),
            .i64_to_u64_wrap => return self.intConvertWrap(i64, u64, args),
            .i64_to_u64_try => return self.intConvertTry(i64, u64, args, return_rt_var),
            .i64_to_u128_wrap => return self.intConvertWrap(i64, u128, args),
            .i64_to_u128_try => return self.intConvertTry(i64, u128, args, return_rt_var),
            .i64_to_f32 => return self.intToFloat(i64, f32, args),
            .i64_to_f64 => return self.intToFloat(i64, f64, args),
            .i64_to_dec => return self.intToDec(i64, args),

            // U128 conversion operations
            .u128_to_i8_wrap => return self.intConvertWrap(u128, i8, args),
            .u128_to_i8_try => return self.intConvertTry(u128, i8, args, return_rt_var),
            .u128_to_i16_wrap => return self.intConvertWrap(u128, i16, args),
            .u128_to_i16_try => return self.intConvertTry(u128, i16, args, return_rt_var),
            .u128_to_i32_wrap => return self.intConvertWrap(u128, i32, args),
            .u128_to_i32_try => return self.intConvertTry(u128, i32, args, return_rt_var),
            .u128_to_i64_wrap => return self.intConvertWrap(u128, i64, args),
            .u128_to_i64_try => return self.intConvertTry(u128, i64, args, return_rt_var),
            .u128_to_i128_wrap => return self.intConvertWrap(u128, i128, args),
            .u128_to_i128_try => return self.intConvertTry(u128, i128, args, return_rt_var),
            .u128_to_u8_wrap => return self.intConvertWrap(u128, u8, args),
            .u128_to_u8_try => return self.intConvertTry(u128, u8, args, return_rt_var),
            .u128_to_u16_wrap => return self.intConvertWrap(u128, u16, args),
            .u128_to_u16_try => return self.intConvertTry(u128, u16, args, return_rt_var),
            .u128_to_u32_wrap => return self.intConvertWrap(u128, u32, args),
            .u128_to_u32_try => return self.intConvertTry(u128, u32, args, return_rt_var),
            .u128_to_u64_wrap => return self.intConvertWrap(u128, u64, args),
            .u128_to_u64_try => return self.intConvertTry(u128, u64, args, return_rt_var),
            .u128_to_f32 => return self.intToFloat(u128, f32, args),
            .u128_to_f64 => return self.intToFloat(u128, f64, args),

            // I128 conversion operations
            .i128_to_i8_wrap => return self.intConvertWrap(i128, i8, args),
            .i128_to_i8_try => return self.intConvertTry(i128, i8, args, return_rt_var),
            .i128_to_i16_wrap => return self.intConvertWrap(i128, i16, args),
            .i128_to_i16_try => return self.intConvertTry(i128, i16, args, return_rt_var),
            .i128_to_i32_wrap => return self.intConvertWrap(i128, i32, args),
            .i128_to_i32_try => return self.intConvertTry(i128, i32, args, return_rt_var),
            .i128_to_i64_wrap => return self.intConvertWrap(i128, i64, args),
            .i128_to_i64_try => return self.intConvertTry(i128, i64, args, return_rt_var),
            .i128_to_u8_wrap => return self.intConvertWrap(i128, u8, args),
            .i128_to_u8_try => return self.intConvertTry(i128, u8, args, return_rt_var),
            .i128_to_u16_wrap => return self.intConvertWrap(i128, u16, args),
            .i128_to_u16_try => return self.intConvertTry(i128, u16, args, return_rt_var),
            .i128_to_u32_wrap => return self.intConvertWrap(i128, u32, args),
            .i128_to_u32_try => return self.intConvertTry(i128, u32, args, return_rt_var),
            .i128_to_u64_wrap => return self.intConvertWrap(i128, u64, args),
            .i128_to_u64_try => return self.intConvertTry(i128, u64, args, return_rt_var),
            .i128_to_u128_wrap => return self.intConvertWrap(i128, u128, args),
            .i128_to_u128_try => return self.intConvertTry(i128, u128, args, return_rt_var),
            .i128_to_f32 => return self.intToFloat(i128, f32, args),
            .i128_to_f64 => return self.intToFloat(i128, f64, args),

            // U128 to Dec (try_unsafe - can overflow Dec's range)
            .u128_to_dec_try_unsafe => return self.intToDecTryUnsafe(u128, args),
            // I128 to Dec (try_unsafe - can overflow Dec's range)
            .i128_to_dec_try_unsafe => return self.intToDecTryUnsafe(i128, args),

            // F32 conversion operations
            .f32_to_i8_trunc => return self.floatToIntTrunc(f32, i8, args),
            .f32_to_i8_try_unsafe => return self.floatToIntTryUnsafe(f32, i8, args),
            .f32_to_i16_trunc => return self.floatToIntTrunc(f32, i16, args),
            .f32_to_i16_try_unsafe => return self.floatToIntTryUnsafe(f32, i16, args),
            .f32_to_i32_trunc => return self.floatToIntTrunc(f32, i32, args),
            .f32_to_i32_try_unsafe => return self.floatToIntTryUnsafe(f32, i32, args),
            .f32_to_i64_trunc => return self.floatToIntTrunc(f32, i64, args),
            .f32_to_i64_try_unsafe => return self.floatToIntTryUnsafe(f32, i64, args),
            .f32_to_i128_trunc => return self.floatToIntTrunc(f32, i128, args),
            .f32_to_i128_try_unsafe => return self.floatToIntTryUnsafe(f32, i128, args),
            .f32_to_u8_trunc => return self.floatToIntTrunc(f32, u8, args),
            .f32_to_u8_try_unsafe => return self.floatToIntTryUnsafe(f32, u8, args),
            .f32_to_u16_trunc => return self.floatToIntTrunc(f32, u16, args),
            .f32_to_u16_try_unsafe => return self.floatToIntTryUnsafe(f32, u16, args),
            .f32_to_u32_trunc => return self.floatToIntTrunc(f32, u32, args),
            .f32_to_u32_try_unsafe => return self.floatToIntTryUnsafe(f32, u32, args),
            .f32_to_u64_trunc => return self.floatToIntTrunc(f32, u64, args),
            .f32_to_u64_try_unsafe => return self.floatToIntTryUnsafe(f32, u64, args),
            .f32_to_u128_trunc => return self.floatToIntTrunc(f32, u128, args),
            .f32_to_u128_try_unsafe => return self.floatToIntTryUnsafe(f32, u128, args),
            .f32_to_f64 => return self.floatWiden(f32, f64, args),

            // F64 conversion operations
            .f64_to_i8_trunc => return self.floatToIntTrunc(f64, i8, args),
            .f64_to_i8_try_unsafe => return self.floatToIntTryUnsafe(f64, i8, args),
            .f64_to_i16_trunc => return self.floatToIntTrunc(f64, i16, args),
            .f64_to_i16_try_unsafe => return self.floatToIntTryUnsafe(f64, i16, args),
            .f64_to_i32_trunc => return self.floatToIntTrunc(f64, i32, args),
            .f64_to_i32_try_unsafe => return self.floatToIntTryUnsafe(f64, i32, args),
            .f64_to_i64_trunc => return self.floatToIntTrunc(f64, i64, args),
            .f64_to_i64_try_unsafe => return self.floatToIntTryUnsafe(f64, i64, args),
            .f64_to_i128_trunc => return self.floatToIntTrunc(f64, i128, args),
            .f64_to_i128_try_unsafe => return self.floatToIntTryUnsafe(f64, i128, args),
            .f64_to_u8_trunc => return self.floatToIntTrunc(f64, u8, args),
            .f64_to_u8_try_unsafe => return self.floatToIntTryUnsafe(f64, u8, args),
            .f64_to_u16_trunc => return self.floatToIntTrunc(f64, u16, args),
            .f64_to_u16_try_unsafe => return self.floatToIntTryUnsafe(f64, u16, args),
            .f64_to_u32_trunc => return self.floatToIntTrunc(f64, u32, args),
            .f64_to_u32_try_unsafe => return self.floatToIntTryUnsafe(f64, u32, args),
            .f64_to_u64_trunc => return self.floatToIntTrunc(f64, u64, args),
            .f64_to_u64_try_unsafe => return self.floatToIntTryUnsafe(f64, u64, args),
            .f64_to_u128_trunc => return self.floatToIntTrunc(f64, u128, args),
            .f64_to_u128_try_unsafe => return self.floatToIntTryUnsafe(f64, u128, args),
            .f64_to_f32_wrap => return self.floatNarrow(f64, f32, args),
            .f64_to_f32_try_unsafe => return self.floatNarrowTryUnsafe(f64, f32, args),

            // Dec conversion operations
            .dec_to_i8_trunc => return self.decToIntTrunc(i8, args),
            .dec_to_i8_try_unsafe => return self.decToIntTryUnsafe(i8, args),
            .dec_to_i16_trunc => return self.decToIntTrunc(i16, args),
            .dec_to_i16_try_unsafe => return self.decToIntTryUnsafe(i16, args),
            .dec_to_i32_trunc => return self.decToIntTrunc(i32, args),
            .dec_to_i32_try_unsafe => return self.decToIntTryUnsafe(i32, args),
            .dec_to_i64_trunc => return self.decToIntTrunc(i64, args),
            .dec_to_i64_try_unsafe => return self.decToIntTryUnsafe(i64, args),
            .dec_to_i128_trunc => return self.decToIntTrunc(i128, args),
            .dec_to_i128_try_unsafe => return self.decToI128TryUnsafe(args),
            .dec_to_u8_trunc => return self.decToIntTrunc(u8, args),
            .dec_to_u8_try_unsafe => return self.decToIntTryUnsafe(u8, args),
            .dec_to_u16_trunc => return self.decToIntTrunc(u16, args),
            .dec_to_u16_try_unsafe => return self.decToIntTryUnsafe(u16, args),
            .dec_to_u32_trunc => return self.decToIntTrunc(u32, args),
            .dec_to_u32_try_unsafe => return self.decToIntTryUnsafe(u32, args),
            .dec_to_u64_trunc => return self.decToIntTrunc(u64, args),
            .dec_to_u64_try_unsafe => return self.decToIntTryUnsafe(u64, args),
            .dec_to_u128_trunc => return self.decToIntTrunc(u128, args),
            .dec_to_u128_try_unsafe => return self.decToIntTryUnsafe(u128, args),
            .dec_to_f32_wrap => return self.decToF32Wrap(args),
            .dec_to_f32_try_unsafe => return self.decToF32TryUnsafe(args),
            .dec_to_f64 => return self.decToF64(args),
        }
    }

    /// Helper to create a simple boolean StackValue (for low-level builtins)
    fn makeBoolValue(self: *Interpreter, value: bool) !StackValue {
        const bool_layout = Layout.int(.u8);
        const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
        var bool_value = try self.pushRaw(bool_layout, 0, bool_rt_var);
        bool_value.is_initialized = false;
        try bool_value.setInt(@intFromBool(value));
        bool_value.is_initialized = true;
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

        const str_rt_var = try self.getCanonicalStrRuntimeVar();
        const value = try self.pushStr(str_rt_var);
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

        const str_rt_var = try self.getCanonicalStrRuntimeVar();
        const value = try self.pushStr(str_rt_var);
        const roc_str_ptr: *RocStr = @ptrCast(@alignCast(value.ptr.?));
        roc_str_ptr.* = RocStr.init(&buf, result.len, roc_ops);
        return value;
    }

    /// Helper for safe integer conversions (widening)
    fn intConvert(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const to_value: To = @intCast(from_value);

        const to_layout = Layout.int(comptime intTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for wrapping integer conversions (potentially lossy)
    fn intConvertWrap(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
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
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for try integer conversions (returns Try(To, [OutOfRange]))
    fn intConvertTry(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue, return_rt_var: ?types.Var) !StackValue {
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
            var out = try self.pushRaw(result_layout, 0, result_rt_var);
            out.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try out.setInt(@intCast(tag_idx));
            out.is_initialized = true;
            return out;
        } else if (result_layout.tag == .record) {
            // Record { tag, payload }
            var dest = try self.pushRaw(result_layout, 0, result_rt_var);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            // Layout should guarantee tag and payload fields exist - if not, it's a compiler bug
            const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse unreachable;
            const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse unreachable;

            // Write tag discriminant
            const field_rt = try self.runtime_types.fresh();
            const tag_field = try acc.getFieldByIndex(tag_field_idx, field_rt);
            // Tag field should be scalar int - if not, it's a compiler bug
            std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
            var tmp = tag_field;
            tmp.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try tmp.setInt(@intCast(tag_idx));

            // Clear payload area
            const field_rt2 = try self.runtime_types.fresh();
            const payload_field = try acc.getFieldByIndex(payload_field_idx, field_rt2);
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
            var dest = try self.pushRaw(result_layout, 0, result_rt_var);
            var result_acc = try dest.asTuple(&self.runtime_layout_store);

            // Element 0 is payload, Element 1 is tag discriminant

            // Write tag discriminant (element 1)
            const tag_elem_rt_var = try self.runtime_types.fresh();
            const tag_field = try result_acc.getElement(1, tag_elem_rt_var);
            // Tag field should be scalar int - if not, it's a compiler bug
            std.debug.assert(tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int);
            var tmp = tag_field;
            tmp.is_initialized = false;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            try tmp.setInt(@intCast(tag_idx));

            // Clear payload area (element 0)
            const payload_elem_rt_var = try self.runtime_types.fresh();
            const payload_field = try result_acc.getElement(0, payload_elem_rt_var);
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
        } else if (result_layout.tag == .tag_union) {
            // Tag union layout: payload at offset 0, discriminant at discriminant_offset
            const dest = try self.pushRaw(result_layout, 0, result_rt_var);
            const tu_data = self.runtime_layout_store.getTagUnionData(result_layout.data.tag_union.idx);

            // Write tag discriminant at discriminant_offset
            const base_ptr: [*]u8 = @ptrCast(dest.ptr.?);
            const disc_ptr = base_ptr + tu_data.discriminant_offset;
            const tag_idx: usize = if (in_range) ok_index orelse 0 else err_index orelse 1;
            switch (tu_data.discriminant_size) {
                1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(tag_idx),
                2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                8 => @as(*u64, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                else => {},
            }

            // Clear payload area (at offset 0)
            const payload_size = tu_data.discriminant_offset; // Payload spans from 0 to discriminant_offset
            if (payload_size > 0) {
                @memset(base_ptr[0..payload_size], 0);
            }

            // Write payload for Ok case
            if (in_range) {
                const to_value: To = @intCast(from_value);
                const payload_ptr: *To = @ptrCast(@alignCast(base_ptr));
                payload_ptr.* = to_value;
            }
            // For Err case, payload is OutOfRange which is a zero-arg tag (already zeroed)

            return dest;
        }

        // Unsupported result layout is a compiler bug
        unreachable;
    }

    /// Helper for integer to float conversions
    fn intToFloat(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const to_value: To = @floatFromInt(from_value);

        const to_layout = Layout.frac(comptime fracTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for integer to Dec conversions
    fn intToDec(self: *Interpreter, comptime From: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        // Null argument is a compiler bug - the compiler should never produce code with null args
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;
        const dec_value = RocDec{ .num = @as(i128, from_value) * RocDec.one_point_zero_i128 };

        const dec_layout = Layout.frac(.dec);
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(dec_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*RocDec, @ptrCast(@alignCast(out.ptr.?))).* = dec_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for integer to Dec try_unsafe conversions (for u128/i128 which can overflow)
    /// Returns { success: Bool, val_or_memory_garbage: Dec }
    fn intToDecTryUnsafe(self: *Interpreter, comptime From: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const int_arg = args[0];
        std.debug.assert(int_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(int_arg.ptr.?))).*;

        // Dec's max whole number is ~1.7×10^20, which is less than u128's max (~3.4×10^38)
        // Dec is stored as i128 * 10^18, so max safe value is i128.max / 10^18
        const dec_max_whole: i128 = @divFloor(std.math.maxInt(i128), RocDec.one_point_zero_i128);
        const dec_min_whole: i128 = @divFloor(std.math.minInt(i128), RocDec.one_point_zero_i128);

        // Check if conversion is safe
        const success = if (From == u128)
            from_value <= @as(u128, @intCast(dec_max_whole))
        else if (From == i128)
            from_value >= dec_min_whole and from_value <= dec_max_whole
        else
            @compileError("intToDecTryUnsafe only supports u128 and i128");

        // Build the result record: { success: Bool, val_or_memory_garbage: Dec }
        return try self.buildSuccessValRecord(success, if (success) RocDec{ .num = @as(i128, @intCast(from_value)) * RocDec.one_point_zero_i128 } else RocDec{ .num = 0 });
    }

    /// Helper for float to int truncating conversions
    fn floatToIntTrunc(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        std.debug.assert(float_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(float_arg.ptr.?))).*;

        // Truncate float to integer (clamping to range and truncating fractional part)
        const to_value: To = floatToIntSaturating(From, To, from_value);

        const to_layout = Layout.int(comptime intTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for float to int try_unsafe conversions
    /// Returns { is_int: Bool, in_range: Bool, val_or_memory_garbage: To }
    fn floatToIntTryUnsafe(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        std.debug.assert(float_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(float_arg.ptr.?))).*;

        // Check if it's an integer (no fractional part) and not NaN/Inf
        const is_int = !std.math.isNan(from_value) and !std.math.isInf(from_value) and @trunc(from_value) == from_value;

        // Check if in range for target type
        const min_val: From = @floatFromInt(std.math.minInt(To));
        const max_val: From = @floatFromInt(std.math.maxInt(To));
        const in_range = from_value >= min_val and from_value <= max_val;

        const val: To = if (is_int and in_range) @intFromFloat(from_value) else 0;

        // Build the result record: { is_int: Bool, in_range: Bool, val_or_memory_garbage: To }
        return try self.buildIsIntInRangeValRecord(is_int, in_range, To, val);
    }

    /// Helper for float widening (F32 -> F64)
    fn floatWiden(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        std.debug.assert(float_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(float_arg.ptr.?))).*;
        const to_value: To = @floatCast(from_value);

        const to_layout = Layout.frac(comptime fracTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for float narrowing (F64 -> F32)
    fn floatNarrow(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        std.debug.assert(float_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(float_arg.ptr.?))).*;
        const to_value: To = @floatCast(from_value);

        const to_layout = Layout.frac(comptime fracTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for float narrowing try_unsafe (F64 -> F32)
    /// Returns { success: Bool, val_or_memory_garbage: F32 }
    fn floatNarrowTryUnsafe(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const float_arg = args[0];
        std.debug.assert(float_arg.ptr != null);

        const from_value: From = @as(*const From, @ptrCast(@alignCast(float_arg.ptr.?))).*;
        const to_value: To = @floatCast(from_value);

        // Check if the conversion is lossless (converting back gives the same value)
        // Also check for infinity which indicates overflow
        const success = !std.math.isInf(to_value) or std.math.isInf(from_value);
        const back: From = @floatCast(to_value);
        const lossless = from_value == back or (std.math.isNan(from_value) and std.math.isNan(back));

        return try self.buildSuccessValRecordF32(success and lossless, to_value);
    }

    /// Helper for Dec to int truncating conversions
    fn decToIntTrunc(self: *Interpreter, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;

        // Get the whole number part by dividing by one_point_zero
        const whole_part = @divTrunc(dec_value.num, RocDec.one_point_zero_i128);

        // Saturate to target range
        const to_value: To = std.math.cast(To, whole_part) orelse if (whole_part < 0) std.math.minInt(To) else std.math.maxInt(To);

        const to_layout = Layout.int(comptime intTypeFromZigType(To));
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = to_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for Dec to int try_unsafe conversions
    /// Returns { is_int: Bool, in_range: Bool, val_or_memory_garbage: To }
    fn decToIntTryUnsafe(self: *Interpreter, comptime To: type, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;

        // Check if it's an integer (no fractional part)
        const remainder = @rem(dec_value.num, RocDec.one_point_zero_i128);
        const is_int = remainder == 0;

        // Get the whole number part
        const whole_part = @divTrunc(dec_value.num, RocDec.one_point_zero_i128);

        // Check if in range for target type
        const in_range = std.math.cast(To, whole_part) != null;

        const val: To = if (is_int and in_range) @intCast(whole_part) else 0;

        return try self.buildIsIntInRangeValRecord(is_int, in_range, To, val);
    }

    /// Helper for Dec to i128 try_unsafe conversions (special case - always in range)
    /// Returns { is_int: Bool, val_or_memory_garbage: I128 }
    fn decToI128TryUnsafe(self: *Interpreter, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;

        // Check if it's an integer (no fractional part)
        const remainder = @rem(dec_value.num, RocDec.one_point_zero_i128);
        const is_int = remainder == 0;

        // Get the whole number part - always fits in i128
        const whole_part = @divTrunc(dec_value.num, RocDec.one_point_zero_i128);

        return try self.buildIsIntValRecord(is_int, whole_part);
    }

    /// Helper for Dec to F32 wrapping conversion
    fn decToF32Wrap(self: *Interpreter, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;
        const f64_value = dec_value.toF64();
        const f32_value: f32 = @floatCast(f64_value);

        const to_layout = Layout.frac(.f32);
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*f32, @ptrCast(@alignCast(out.ptr.?))).* = f32_value;
        out.is_initialized = true;
        return out;
    }

    /// Helper for Dec to F32 try_unsafe conversion
    /// Returns { success: Bool, val_or_memory_garbage: F32 }
    fn decToF32TryUnsafe(self: *Interpreter, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;
        const f64_value = dec_value.toF64();
        const f32_value: f32 = @floatCast(f64_value);

        // Check if conversion is lossless by converting back
        const back_f64: f64 = @floatCast(f32_value);
        const back_dec = RocDec.fromF64(back_f64);
        const success = back_dec != null and back_dec.?.num == dec_value.num;

        return try self.buildSuccessValRecordF32(success, f32_value);
    }

    /// Helper for Dec to F64 conversion
    fn decToF64(self: *Interpreter, args: []const StackValue) !StackValue {
        std.debug.assert(args.len == 1);
        const dec_arg = args[0];
        std.debug.assert(dec_arg.ptr != null);

        const dec_value: RocDec = @as(*const RocDec, @ptrCast(@alignCast(dec_arg.ptr.?))).*;
        const f64_value = dec_value.toF64();

        const to_layout = Layout.frac(.f64);
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRaw(to_layout, 0, result_rt_var);
        out.is_initialized = false;
        @as(*f64, @ptrCast(@alignCast(out.ptr.?))).* = f64_value;
        out.is_initialized = true;
        return out;
    }

    /// Build a record { success: Bool, val_or_memory_garbage: Dec }
    fn buildSuccessValRecord(self: *Interpreter, success: bool, val: RocDec) !StackValue {
        // Layout: tuple (Dec, Bool) where element 0 is Dec (16 bytes) and element 1 is Bool (1 byte)
        // Total size with alignment: 24 bytes (16 for Dec + 8 for alignment of Bool field)

        // We need to create a tuple layout for the result
        // For now, allocate raw bytes and set them directly
        // The tuple is (val_or_memory_garbage: Dec, success: Bool)
        const tuple_size: usize = 24; // 16 bytes Dec + padding + 1 byte bool
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRawBytes(tuple_size, 16, result_rt_var);
        out.is_initialized = false;

        // Write Dec at offset 0
        @as(*RocDec, @ptrCast(@alignCast(out.ptr.?))).* = val;

        // Write Bool at offset 16
        const bool_ptr: *u8 = @ptrFromInt(@intFromPtr(out.ptr.?) + 16);
        bool_ptr.* = @intFromBool(success);

        out.is_initialized = true;
        // Layout is set by pushRawBytes as .zst since we're working with raw bytes
        return out;
    }

    /// Build a record { success: Bool, val_or_memory_garbage: F32 }
    fn buildSuccessValRecordF32(self: *Interpreter, success: bool, val: f32) !StackValue {
        // Layout: tuple (F32, Bool) where element 0 is F32 (4 bytes) and element 1 is Bool (1 byte)
        const tuple_size: usize = 8; // 4 bytes F32 + padding + 1 byte bool
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRawBytes(tuple_size, 4, result_rt_var);
        out.is_initialized = false;

        // Write F32 at offset 0
        @as(*f32, @ptrCast(@alignCast(out.ptr.?))).* = val;

        // Write Bool at offset 4
        const bool_ptr: *u8 = @ptrFromInt(@intFromPtr(out.ptr.?) + 4);
        bool_ptr.* = @intFromBool(success);

        out.is_initialized = true;
        // Layout is set by pushRawBytes as .zst since we're working with raw bytes
        return out;
    }

    /// Build a record { is_int: Bool, in_range: Bool, val_or_memory_garbage: To }
    fn buildIsIntInRangeValRecord(self: *Interpreter, is_int: bool, in_range: bool, comptime To: type, val: To) !StackValue {
        // Layout depends on To's size
        const val_size = @sizeOf(To);
        const val_align = @alignOf(To);
        // Structure: (val, is_int, in_range) with proper alignment
        const tuple_size: usize = val_size + 2; // val + 2 bools
        const padded_size = (tuple_size + val_align - 1) / val_align * val_align;

        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRawBytes(padded_size, val_align, result_rt_var);
        out.is_initialized = false;

        // Write val at offset 0
        @as(*To, @ptrCast(@alignCast(out.ptr.?))).* = val;

        // Write is_int at offset val_size
        const is_int_ptr: *u8 = @ptrFromInt(@intFromPtr(out.ptr.?) + val_size);
        is_int_ptr.* = @intFromBool(is_int);

        // Write in_range at offset val_size + 1
        const in_range_ptr: *u8 = @ptrFromInt(@intFromPtr(out.ptr.?) + val_size + 1);
        in_range_ptr.* = @intFromBool(in_range);

        out.is_initialized = true;
        // Layout is set by pushRawBytes as .zst since we're working with raw bytes
        return out;
    }

    /// Build a record { is_int: Bool, val_or_memory_garbage: I128 } (for dec_to_i128 which is always in range)
    fn buildIsIntValRecord(self: *Interpreter, is_int: bool, val: i128) !StackValue {
        // Layout: tuple (I128, Bool)
        const tuple_size: usize = 24; // 16 bytes I128 + padding + 1 byte bool
        const result_rt_var = try self.runtime_types.fresh();
        var out = try self.pushRawBytes(tuple_size, 16, result_rt_var);
        out.is_initialized = false;

        // Write I128 at offset 0
        @as(*i128, @ptrCast(@alignCast(out.ptr.?))).* = val;

        // Write Bool at offset 16
        const bool_ptr: *u8 = @ptrFromInt(@intFromPtr(out.ptr.?) + 16);
        bool_ptr.* = @intFromBool(is_int);

        out.is_initialized = true;
        // Layout is set by pushRawBytes as .zst since we're working with raw bytes
        return out;
    }

    /// Helper to convert float to int with saturation (for trunc operations)
    fn floatToIntSaturating(comptime From: type, comptime To: type, value: From) To {
        if (std.math.isNan(value)) return 0;

        const min_val: From = @floatFromInt(std.math.minInt(To));
        const max_val: From = @floatFromInt(std.math.maxInt(To));

        if (value <= min_val) return std.math.minInt(To);
        if (value >= max_val) return std.math.maxInt(To);

        return @intFromFloat(value);
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

    /// Get the Ok payload type variable from a Try type
    fn getTryOkPayloadVar(self: *Interpreter, result_rt_var: types.Var) !?types.Var {
        const resolved = self.resolveBaseVar(result_rt_var);
        std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .tag_union);

        var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.appendUnionTags(result_rt_var, &tag_list);

        const ok_ident = self.env.idents.ok;
        for (tag_list.items) |tag_info| {
            if (tag_info.name == ok_ident) {
                const arg_vars = self.runtime_types.sliceVars(tag_info.args);
                if (arg_vars.len >= 1) {
                    return arg_vars[0];
                }
            }
        }
        return null;
    }

    /// Get Ok and Err tag indices from a Try type
    fn getTryTagIndices(self: *Interpreter, result_rt_var: types.Var) !struct { ok: ?usize, err: ?usize } {
        const resolved = self.resolveBaseVar(result_rt_var);
        std.debug.assert(resolved.desc.content == .structure and resolved.desc.content.structure == .tag_union);

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
        return .{ .ok = ok_index, .err = err_index };
    }

    /// Helper for parsing integer from string (Str -> Try(T, [BadNumStr]))
    fn numFromStrInt(self: *Interpreter, comptime T: type, roc_str: *const RocStr, result_rt_var: types.Var) !StackValue {
        const str_slice = roc_str.asSlice();

        // Parse integer using base-10 radix only
        const parsed: ?T = std.fmt.parseInt(T, str_slice, 10) catch null;
        const success = parsed != null;

        const result_layout = try self.getRuntimeLayout(result_rt_var);
        const tag_indices = try self.getTryTagIndices(result_rt_var);

        return self.buildTryResultWithValue(T, result_layout, tag_indices.ok, tag_indices.err, success, parsed orelse 0, result_rt_var);
    }

    /// Helper for parsing float from string (Str -> Try(T, [BadNumStr]))
    fn numFromStrFloat(self: *Interpreter, comptime T: type, roc_str: *const RocStr, result_rt_var: types.Var) !StackValue {
        const str_slice = roc_str.asSlice();

        // Parse float
        const parsed: ?T = std.fmt.parseFloat(T, str_slice) catch null;
        const success = parsed != null;

        const result_layout = try self.getRuntimeLayout(result_rt_var);
        const tag_indices = try self.getTryTagIndices(result_rt_var);

        return self.buildTryResultWithValue(T, result_layout, tag_indices.ok, tag_indices.err, success, parsed orelse 0, result_rt_var);
    }

    /// Helper for parsing Dec from string (Str -> Try(Dec, [BadNumStr]))
    fn numFromStrDec(self: *Interpreter, roc_str: *const RocStr, result_rt_var: types.Var) !StackValue {
        // Use RocDec's fromStr implementation
        const parsed = builtins.dec.RocDec.fromStr(roc_str.*);
        const success = parsed != null;

        const result_layout = try self.getRuntimeLayout(result_rt_var);
        const tag_indices = try self.getTryTagIndices(result_rt_var);

        // Dec is stored as i128 internally
        const dec_val: i128 = if (parsed) |dec| dec.num else 0;
        return self.buildTryResultWithValue(i128, result_layout, tag_indices.ok, tag_indices.err, success, dec_val, result_rt_var);
    }

    /// Build a Try result with a value payload
    fn buildTryResultWithValue(
        self: *Interpreter,
        comptime T: type,
        result_layout: Layout,
        ok_index: ?usize,
        err_index: ?usize,
        success: bool,
        value: T,
        result_rt_var: types.Var,
    ) !StackValue {
        const tag_idx: usize = if (success) ok_index orelse 0 else err_index orelse 1;

        if (result_layout.tag == .record) {
            var dest = try self.pushRaw(result_layout, 0, result_rt_var);
            var result_acc = try dest.asRecord(&self.runtime_layout_store);
            const layout_env = self.runtime_layout_store.env;
            const tag_field_idx = result_acc.findFieldIndex(layout_env.idents.tag) orelse unreachable;
            const payload_field_idx = result_acc.findFieldIndex(layout_env.idents.payload) orelse unreachable;

            // Write tag discriminant
            const field_rt = try self.runtime_types.fresh();
            const tag_field = try result_acc.getFieldByIndex(tag_field_idx, field_rt);
            var tmp = tag_field;
            tmp.is_initialized = false;
            try tmp.setInt(@intCast(tag_idx));

            // Clear and write payload
            const field_rt2 = try self.runtime_types.fresh();
            const payload_field = try result_acc.getFieldByIndex(payload_field_idx, field_rt2);
            if (payload_field.ptr) |payload_ptr| {
                const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                if (payload_bytes_len > 0) {
                    @memset(@as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len], 0);
                }
                if (success) {
                    @as(*T, @ptrCast(@alignCast(payload_ptr))).* = value;
                }
            }
            return dest;
        } else if (result_layout.tag == .tuple) {
            var dest = try self.pushRaw(result_layout, 0, result_rt_var);
            var result_acc = try dest.asTuple(&self.runtime_layout_store);

            // Write tag discriminant (element 1)
            const tag_elem_rt_var = try self.runtime_types.fresh();
            const tag_field = try result_acc.getElement(1, tag_elem_rt_var);
            var tmp = tag_field;
            tmp.is_initialized = false;
            try tmp.setInt(@intCast(tag_idx));

            // Clear and write payload (element 0)
            const payload_elem_rt_var = try self.runtime_types.fresh();
            const payload_field = try result_acc.getElement(0, payload_elem_rt_var);
            if (payload_field.ptr) |payload_ptr| {
                const payload_bytes_len = self.runtime_layout_store.layoutSize(payload_field.layout);
                if (payload_bytes_len > 0) {
                    @memset(@as([*]u8, @ptrCast(payload_ptr))[0..payload_bytes_len], 0);
                }
                if (success) {
                    @as(*T, @ptrCast(@alignCast(payload_ptr))).* = value;
                }
            }
            return dest;
        } else if (result_layout.tag == .tag_union) {
            var dest = try self.pushRaw(result_layout, 0, result_rt_var);
            const tu_data = self.runtime_layout_store.getTagUnionData(result_layout.data.tag_union.idx);

            const base_ptr: [*]u8 = @ptrCast(dest.ptr.?);
            const disc_ptr = base_ptr + tu_data.discriminant_offset;

            // Write discriminant
            switch (tu_data.discriminant_size) {
                1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(tag_idx),
                2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                8 => @as(*u64, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_idx),
                else => {},
            }

            // Clear and write payload
            const payload_size = tu_data.discriminant_offset;
            if (payload_size > 0) {
                @memset(base_ptr[0..payload_size], 0);
            }
            if (success) {
                @as(*T, @ptrCast(@alignCast(base_ptr))).* = value;
            }

            dest.is_initialized = true;
            return dest;
        }

        unreachable;
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

    /// Handle completion of a for loop/expression.
    /// For statements: continue with remaining statements or final expression.
    /// For expressions: push empty record {} as result.
    fn handleForLoopComplete(
        self: *Interpreter,
        work_stack: *WorkStack,
        value_stack: *ValueStack,
        stmt_context: ?Continuation.ForIterate.StatementContext,
        bindings_start: usize,
        roc_ops: *RocOps,
    ) Error!void {
        if (stmt_context) |ctx| {
            // For statement: continue with remaining statements
            if (ctx.remaining_stmts.len == 0) {
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = ctx.final_expr,
                    .expected_rt_var = null,
                } });
            } else {
                const next_stmt = self.env.store.getStatement(ctx.remaining_stmts[0]);
                try self.scheduleNextStatement(work_stack, next_stmt, ctx.remaining_stmts[1..], ctx.final_expr, bindings_start, null, roc_ops);
            }
        } else {
            // For expression: push empty record {} as result
            const empty_record_layout_idx = try self.runtime_layout_store.ensureEmptyRecordLayout();
            const empty_record_layout = self.runtime_layout_store.getLayout(empty_record_layout_idx);
            const empty_record_rt_var = try self.runtime_types.fresh();
            const empty_record_value = try self.pushRaw(empty_record_layout, 0, empty_record_rt_var);
            try value_stack.push(empty_record_value);
        }
    }

    fn getRuntimeU8(value: StackValue) u8 {
        std.debug.assert(value.layout.tag == .scalar);
        std.debug.assert(value.layout.data.scalar.tag == .int);
        std.debug.assert(value.layout.data.scalar.data.int == .u8);

        const ptr = value.ptr orelse unreachable;
        const b: *const u8 = @ptrCast(@alignCast(ptr));

        return b.*;
    }

    fn boolValueEquals(self: *Interpreter, equals: bool, value: StackValue) bool {
        const ptr = value.ptr orelse unreachable;

        // Bool can be either a scalar (u8) or a tag_union layout
        // For tag_union: False=0, True=1 (alphabetically sorted)
        if (value.layout.tag == .scalar) {
            std.debug.assert(value.layout.data.scalar.tag == .int);
            std.debug.assert(value.layout.data.scalar.data.int == .u8);
            const bool_byte = @as(*const u8, @ptrCast(@alignCast(ptr))).*;
            // Debug removed
            return (bool_byte != 0) == equals;
        } else if (value.layout.tag == .tag_union) {
            // Tag union Bool: read discriminant at the correct offset
            const tu_data = self.runtime_layout_store.getTagUnionData(value.layout.data.tag_union.idx);
            const base_ptr: [*]u8 = @ptrCast(ptr);
            const disc_ptr = base_ptr + tu_data.discriminant_offset;
            const bool_byte = disc_ptr[0];
            // Debug removed
            // discriminant 1 = True, discriminant 0 = False
            return (bool_byte == 1) == equals;
        } else {
            std.debug.panic("boolValueEquals: unexpected layout tag {s}", .{@tagName(value.layout.tag)});
        }
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

    fn layoutMatchesKind(_: *Interpreter, layout_val: Layout, kind: NumericKind) bool {
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
        _: Layout, // Ignored - we always use Dec layout for proper alignment
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

        // Use proper Dec layout to ensure 16-byte alignment for RocDec
        const dec_layout = Layout.frac(.dec);
        var out = try self.pushRaw(dec_layout, 0, lhs.rt_var);
        out.is_initialized = true;
        if (out.ptr) |ptr| {
            const dest: *RocDec = @ptrCast(@alignCast(ptr));
            dest.* = result_dec;
        }
        return out;
    }

    /// Evaluate a binary operation on numeric values (int, f32, f64, or dec)
    /// This function dispatches to the appropriate type-specific operation.
    fn evalNumericBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        lhs: StackValue,
        rhs: StackValue,
        roc_ops: *RocOps,
    ) !StackValue {
        const lhs_val = try self.extractNumericValue(lhs);
        const rhs_val = try self.extractNumericValue(rhs);
        const result_layout = lhs.layout;

        var out = try self.pushRaw(result_layout, 0, lhs.rt_var);
        out.is_initialized = false;

        switch (op) {
            .add => switch (lhs_val) {
                .int => |l| switch (rhs_val) {
                    .int => |r| try out.setInt(l + r),
                    .dec => |r| try out.setInt(l + @divTrunc(r.num, RocDec.one_point_zero_i128)),
                    else => return error.TypeMismatch,
                },
                .f32 => |l| switch (rhs_val) {
                    .f32 => |r| out.setF32(l + r),
                    else => return error.TypeMismatch,
                },
                .f64 => |l| switch (rhs_val) {
                    .f64 => |r| out.setF64(l + r),
                    else => return error.TypeMismatch,
                },
                .dec => |l| switch (rhs_val) {
                    .dec => |r| out.setDec(RocDec.add(l, r, roc_ops)),
                    .int => |r| out.setDec(RocDec.add(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                    else => return error.TypeMismatch,
                },
            },
            .sub => switch (lhs_val) {
                .int => |l| switch (rhs_val) {
                    .int => |r| try out.setInt(l - r),
                    .dec => |r| try out.setInt(l - @divTrunc(r.num, RocDec.one_point_zero_i128)),
                    else => return error.TypeMismatch,
                },
                .f32 => |l| switch (rhs_val) {
                    .f32 => |r| out.setF32(l - r),
                    else => return error.TypeMismatch,
                },
                .f64 => |l| switch (rhs_val) {
                    .f64 => |r| out.setF64(l - r),
                    else => return error.TypeMismatch,
                },
                .dec => |l| switch (rhs_val) {
                    .dec => |r| out.setDec(RocDec.sub(l, r, roc_ops)),
                    .int => |r| out.setDec(RocDec.sub(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                    else => return error.TypeMismatch,
                },
            },
            .mul => switch (lhs_val) {
                .int => |l| switch (rhs_val) {
                    .int => |r| try out.setInt(l * r),
                    .dec => |r| try out.setInt(l * @divTrunc(r.num, RocDec.one_point_zero_i128)),
                    else => return error.TypeMismatch,
                },
                .f32 => |l| switch (rhs_val) {
                    .f32 => |r| out.setF32(l * r),
                    else => return error.TypeMismatch,
                },
                .f64 => |l| switch (rhs_val) {
                    .f64 => |r| out.setF64(l * r),
                    else => return error.TypeMismatch,
                },
                .dec => |l| switch (rhs_val) {
                    .dec => |r| out.setDec(RocDec.mul(l, r, roc_ops)),
                    .int => |r| out.setDec(RocDec.mul(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops)),
                    else => return error.TypeMismatch,
                },
            },
            .div, .div_trunc => switch (lhs_val) {
                .int => |l| switch (rhs_val) {
                    .int => |r| {
                        if (r == 0) return error.DivisionByZero;
                        try out.setInt(@divTrunc(l, r));
                    },
                    else => return error.TypeMismatch,
                },
                .f32 => |l| switch (rhs_val) {
                    .f32 => |r| {
                        if (r == 0) return error.DivisionByZero;
                        if (op == .div_trunc) {
                            out.setF32(std.math.trunc(l / r));
                        } else {
                            out.setF32(l / r);
                        }
                    },
                    else => return error.TypeMismatch,
                },
                .f64 => |l| switch (rhs_val) {
                    .f64 => |r| {
                        if (r == 0) return error.DivisionByZero;
                        if (op == .div_trunc) {
                            out.setF64(std.math.trunc(l / r));
                        } else {
                            out.setF64(l / r);
                        }
                    },
                    else => return error.TypeMismatch,
                },
                .dec => |l| switch (rhs_val) {
                    .dec => |r| {
                        if (r.num == 0) return error.DivisionByZero;
                        out.setDec(RocDec.div(l, r, roc_ops));
                    },
                    .int => |r| {
                        if (r == 0) return error.DivisionByZero;
                        out.setDec(RocDec.div(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops));
                    },
                    else => return error.TypeMismatch,
                },
            },
            .rem => switch (lhs_val) {
                .int => |l| switch (rhs_val) {
                    .int => |r| {
                        if (r == 0) return error.DivisionByZero;
                        try out.setInt(@rem(l, r));
                    },
                    else => return error.TypeMismatch,
                },
                .f32 => |l| switch (rhs_val) {
                    .f32 => |r| {
                        if (r == 0) return error.DivisionByZero;
                        out.setF32(@rem(l, r));
                    },
                    else => return error.TypeMismatch,
                },
                .f64 => |l| switch (rhs_val) {
                    .f64 => |r| {
                        if (r == 0) return error.DivisionByZero;
                        out.setF64(@rem(l, r));
                    },
                    else => return error.TypeMismatch,
                },
                .dec => |l| switch (rhs_val) {
                    .dec => |r| {
                        if (r.num == 0) return error.DivisionByZero;
                        out.setDec(RocDec.rem(l, r, roc_ops));
                    },
                    .int => |r| {
                        if (r == 0) return error.DivisionByZero;
                        out.setDec(RocDec.rem(l, RocDec{ .num = @as(i128, r) * RocDec.one_point_zero_i128 }, roc_ops));
                    },
                    else => return error.TypeMismatch,
                },
            },
            else => return error.TypeMismatch,
        }
        out.is_initialized = true;
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

    fn stackValueToDecimal(_: *Interpreter, value: StackValue) !RocDec {
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

    fn stackValueToFloat(_: *Interpreter, comptime FloatT: type, value: StackValue) !FloatT {
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

    fn isNumericScalar(_: *Interpreter, layout_val: Layout) bool {
        if (layout_val.tag != .scalar) return false;
        return switch (layout_val.data.scalar.tag) {
            .int, .frac => true,
            else => false,
        };
    }

    fn extractNumericValue(_: *Interpreter, value: StackValue) !NumericValue {
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

    const CompareOp = enum { gt, gte, lt, lte, eq };

    /// Compare two numeric values using the specified comparison operation
    fn compareNumericValues(self: *Interpreter, lhs: StackValue, rhs: StackValue, op: CompareOp) !bool {
        const order = try self.compareNumericScalars(lhs, rhs);
        return switch (op) {
            .gt => order == .gt,
            .gte => order == .gt or order == .eq,
            .lt => order == .lt,
            .lte => order == .lt or order == .eq,
            .eq => order == .eq,
        };
    }

    fn orderNumericValues(self: *Interpreter, lhs: NumericValue, rhs: NumericValue) !std.math.Order {
        return switch (lhs) {
            .int => self.orderInt(lhs.int, rhs),
            .f32 => self.orderF32(lhs.f32, rhs),
            .f64 => self.orderF64(lhs.f64, rhs),
            .dec => self.orderDec(lhs.dec, rhs),
        };
    }

    fn orderInt(_: *Interpreter, lhs: i128, rhs: NumericValue) !std.math.Order {
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

    fn orderF32(_: *Interpreter, lhs: f32, rhs: NumericValue) !std.math.Order {
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

    fn orderF64(_: *Interpreter, lhs: f64, rhs: NumericValue) !std.math.Order {
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

    fn orderDec(_: *Interpreter, lhs: RocDec, rhs: NumericValue) !std.math.Order {
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
        roc_ops: *RocOps,
    ) StructuralEqError!bool {
        // Handle scalar comparisons (numbers, strings) directly.
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const lhs_scalar = lhs.layout.data.scalar;
            const rhs_scalar = rhs.layout.data.scalar;

            // Handle numeric type mismatches (Int vs Dec)
            const lhs_is_numeric = lhs_scalar.tag == .int or lhs_scalar.tag == .frac;
            const rhs_is_numeric = rhs_scalar.tag == .int or rhs_scalar.tag == .frac;
            if (lhs_is_numeric and rhs_is_numeric) {
                // Allow comparing Int with Dec by converting
                const lhs_num = self.extractNumericValue(lhs) catch return error.TypeMismatch;
                const rhs_num = self.extractNumericValue(rhs) catch return error.TypeMismatch;
                return switch (lhs_num) {
                    .int => |l| switch (rhs_num) {
                        .int => |r| l == r,
                        .dec => |r| l == @divTrunc(r.num, RocDec.one_point_zero_i128),
                        else => false,
                    },
                    .dec => |l| switch (rhs_num) {
                        .dec => |r| l.num == r.num,
                        .int => |r| l.num == @as(i128, r) * RocDec.one_point_zero_i128,
                        else => false,
                    },
                    .f32 => |l| switch (rhs_num) {
                        .f32 => |r| l == r,
                        else => false,
                    },
                    .f64 => |l| switch (rhs_num) {
                        .f64 => |r| l == r,
                        else => false,
                    },
                };
            }

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

        // Check for nominal types FIRST (before resolveBaseVar) to dispatch to their is_eq method.
        // This is critical because resolveBaseVar follows nominal types to their backing var,
        // but we need to dispatch to the nominal type's is_eq method instead.
        const direct_resolved = self.runtime_types.resolveVar(lhs_var);
        if (direct_resolved.desc.content == .structure) {
            if (direct_resolved.desc.content.structure == .nominal_type) {
                const nom = direct_resolved.desc.content.structure.nominal_type;
                return try self.dispatchNominalIsEq(lhs, rhs, nom, roc_ops);
            }
        }

        // Now use resolveBaseVar for non-nominal structural types
        const lhs_resolved = self.resolveBaseVar(lhs_var);
        const lhs_content = lhs_resolved.desc.content;
        if (lhs_content != .structure) @panic("valuesStructurallyEqual: lhs is not a structure type");

        return switch (lhs_content.structure) {
            .nominal_type => |nom| try self.dispatchNominalIsEq(lhs, rhs, nom, roc_ops),
            .tuple => |tuple| {
                const elem_vars = self.runtime_types.sliceVars(tuple.elems);
                return try self.structuralEqualTuple(lhs, rhs, elem_vars, roc_ops);
            },
            .record => |record| {
                return try self.structuralEqualRecord(lhs, rhs, record, roc_ops);
            },
            .tag_union => {
                return try self.structuralEqualTag(lhs, rhs, lhs_var, roc_ops);
            },
            .empty_record => true,
            .empty_tag_union => true,
            .record_unbound, .fn_pure, .fn_effectful, .fn_unbound => @panic("valuesStructurallyEqual: cannot compare functions or unbound records"),
        };
    }

    fn structuralEqualTuple(
        self: *Interpreter,
        lhs: StackValue,
        rhs: StackValue,
        elem_vars: []const types.Var,
        roc_ops: *RocOps,
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
            const elem_rt_var = elem_vars[index];
            const lhs_elem = try lhs_acc.getElement(index, elem_rt_var);
            const rhs_elem = try rhs_acc.getElement(index, elem_rt_var);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_rt_var, rhs_elem, elem_rt_var, roc_ops);
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
        roc_ops: *RocOps,
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
            const field_var = field_slice.items(.var_)[idx];
            const lhs_field = try lhs_rec.getFieldByIndex(idx, field_var);
            const rhs_field = try rhs_rec.getFieldByIndex(idx, field_var);
            const fields_equal = try self.valuesStructurallyEqual(lhs_field, field_var, rhs_field, field_var, roc_ops);
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
        roc_ops: *RocOps,
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
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_var, rhs_elem, elem_var, roc_ops);
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
        roc_ops: *RocOps,
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
            return try self.valuesStructurallyEqual(lhs_payload, arg_vars[0], rhs_payload, arg_vars[0], roc_ops);
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
            const arg_rt_var = arg_vars[idx];
            const lhs_elem = try lhs_tuple.getElement(idx, arg_rt_var);
            const rhs_elem = try rhs_tuple.getElement(idx, arg_rt_var);
            const args_equal = try self.valuesStructurallyEqual(lhs_elem, arg_rt_var, rhs_elem, arg_rt_var, roc_ops);
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
        roc_ops: *RocOps,
    ) StructuralEqError!bool {
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

        // For scalar types, fall back to attempting scalar comparison
        // This handles cases like Bool which wraps a tag union but is represented as a scalar
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const order = self.compareNumericScalars(lhs, rhs) catch @panic("dispatchNominalIsEq: failed to compare scalars");
            return order == .eq;
        }

        // Look up and call the is_eq method on the nominal type
        const method_func = self.resolveMethodFunction(
            nom.origin_module,
            nom.ident.ident_idx,
            self.root_env.idents.is_eq,
            roc_ops,
            lhs.rt_var,
        ) catch |err| {
            // If method lookup fails, we can't compare this type
            if (err == error.MethodLookupFailed) {
                return error.NotImplemented;
            }
            return err;
        };
        defer method_func.decref(&self.runtime_layout_store, roc_ops);

        // Call the is_eq method with lhs and rhs as arguments
        if (method_func.layout.tag != .closure) {
            return error.TypeMismatch;
        }

        const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

        // All is_eq methods are low-level lambdas (builtins). If a type doesn't have
        // is_eq, the type-checker catches it as a missing method error before we get here.
        const lambda_expr = closure_header.source_env.store.getExpr(closure_header.lambda_expr_idx);
        if (lambda_expr != .e_low_level_lambda) {
            unreachable; // is_eq methods are always low-level builtins
        }
        const low_level = lambda_expr.e_low_level_lambda;
        var args = [2]StackValue{ lhs, rhs };
        const result = self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null) catch {
            return error.NotImplemented;
        };
        defer result.decref(&self.runtime_layout_store, roc_ops);
        return self.boolValueEquals(true, result);
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

    pub fn getCanonicalStrRuntimeVar(self: *Interpreter) !types.Var {
        if (self.canonical_str_rt_var) |cached| return cached;
        // Use the dynamic str_stmt index (from the Str module)
        // We need the nominal type itself (not the backing type) so that method dispatch
        // can look up methods like split_on, drop_prefix, etc.
        const ct_var = can.ModuleEnv.varFrom(self.builtins.str_stmt);

        // Use str_env to translate since str_stmt is from the Str module
        // Cast away const - translateTypeVar doesn't actually mutate the module
        const nominal_rt_var = try self.translateTypeVar(@constCast(self.builtins.str_env), ct_var);
        // Return the nominal type, not the backing type - method dispatch needs the nominal
        // type to look up methods like split_on, drop_prefix, etc.
        self.canonical_str_rt_var = nominal_rt_var;
        return nominal_rt_var;
    }

    fn resolveBaseVar(self: *Interpreter, runtime_var: types.Var) types.store.ResolvedVarDesc {
        var current = self.runtime_types.resolveVar(runtime_var);
        var guard = types.debug.IterationGuard.init("resolveBaseVar");
        while (true) {
            guard.tick();
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

        var outer_guard = types.debug.IterationGuard.init("appendUnionTags.outer");
        while (var_stack.items.len > 0) {
            outer_guard.tick();
            const current_var = var_stack.pop().?;
            var resolved = self.runtime_types.resolveVar(current_var);
            var inner_guard = types.debug.IterationGuard.init("appendUnionTags.expand");
            expand: while (true) {
                inner_guard.tick();
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
                const disc_rt_var = try self.runtime_types.fresh();
                const tag_field = try acc.getFieldByIndex(tag_field_idx, disc_rt_var);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true, .rt_var = tag_field.rt_var };
                    tag_index = @intCast(tmp.asI128());
                } else return error.TypeMismatch;

                var payload_value: ?StackValue = null;
                if (acc.findFieldIndex(self.env.idents.payload)) |payload_idx| {
                    const payload_rt_var = try self.runtime_types.fresh();
                    payload_value = try acc.getFieldByIndex(payload_idx, payload_rt_var);
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
                            // For heterogeneous tag unions (like Try(Str, ErrorRecord)), the payload
                            // union in memory is sized for the largest variant. When extracting a
                            // specific variant's payload, we need the correct layout for that variant.
                            //
                            // Check if the arg var has a rigid substitution (from polymorphic method
                            // instantiation). If so, use the substituted type's layout.
                            const arg_var = arg_vars[0];
                            const arg_resolved = self.runtime_types.resolveVar(arg_var);
                            const effective_layout = if (arg_resolved.desc.content == .rigid) blk: {
                                if (self.rigid_subst.get(arg_resolved.var_)) |subst_var| {
                                    // Use the substituted concrete type's layout
                                    break :blk self.getRuntimeLayout(subst_var) catch field_value.layout;
                                }
                                break :blk field_value.layout;
                            } else field_value.layout;

                            payload_value = StackValue{
                                .layout = effective_layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                                .rt_var = field_value.rt_var,
                            };
                        } else {
                            // For multiple args, use the layout from the stored field
                            payload_value = StackValue{
                                .layout = field_value.layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                                .rt_var = field_value.rt_var,
                            };
                        }
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            .tuple => {
                // Tag unions are now represented as tuples (payload, tag)
                var acc = try value.asTuple(&self.runtime_layout_store);

                // Get tuple element rt_vars if available from value's type
                const tuple_elem_vars: ?[]const types.Var = blk: {
                    const resolved = self.runtime_types.resolveVar(value.rt_var);
                    if (resolved.desc.content == .structure) {
                        if (resolved.desc.content.structure == .tuple) {
                            break :blk self.runtime_types.sliceVars(resolved.desc.content.structure.tuple.elems);
                        }
                    }
                    break :blk null;
                };

                // Element 1 is the tag discriminant - getElement takes original index directly
                const discrim_rt_var = if (tuple_elem_vars) |vars| (if (vars.len > 1) vars[1] else value.rt_var) else value.rt_var;
                const tag_field = try acc.getElement(1, discrim_rt_var);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true, .rt_var = tag_field.rt_var };
                    tag_index = @intCast(tmp.asI128());
                } else return error.TypeMismatch;

                // Element 0 is the payload - getElement takes original index directly
                var payload_value: ?StackValue = null;
                const payload_rt_var = if (tuple_elem_vars) |vars| (if (vars.len > 0) vars[0] else value.rt_var) else value.rt_var;
                const payload_field = acc.getElement(0, payload_rt_var) catch null;
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
                        // For heterogeneous tag unions (like Try(Str, ErrorRecord)), the payload
                        // union in memory is sized for the largest variant. When extracting a
                        // specific variant's payload, we need the correct layout for that variant.
                        //
                        // Check if the arg var has a rigid substitution (from polymorphic method
                        // instantiation). If so, use the substituted type's layout.
                        const arg_var = arg_vars[0];
                        const arg_resolved = self.runtime_types.resolveVar(arg_var);
                        const effective_layout = if (arg_resolved.desc.content == .rigid) blk: {
                            if (self.rigid_subst.get(arg_resolved.var_)) |subst_var| {
                                // Use the substituted concrete type's layout
                                break :blk self.getRuntimeLayout(subst_var) catch field_value.layout;
                            }
                            break :blk field_value.layout;
                        } else field_value.layout;

                        payload_value = StackValue{
                            .layout = effective_layout,
                            .ptr = field_value.ptr,
                            .is_initialized = field_value.is_initialized,
                            .rt_var = arg_var,
                        };
                    } else {
                        // For multiple args, use the layout from the stored field
                        // which already has the correct tuple layout
                        payload_value = StackValue{
                            .layout = field_value.layout,
                            .ptr = field_value.ptr,
                            .is_initialized = field_value.is_initialized,
                            .rt_var = field_value.rt_var,
                        };
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            .tag_union => {
                // New proper tag_union layout: payload at offset 0, discriminant at discriminant_offset
                var acc = try value.asTagUnion(&self.runtime_layout_store);
                const tag_index = acc.getDiscriminant();

                var payload_value: ?StackValue = null;
                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(union_rt_var, &tag_list);
                if (tag_index >= tag_list.items.len) return error.TypeMismatch;
                const tag_info = tag_list.items[tag_index];
                const arg_vars = self.runtime_types.sliceVars(tag_info.args);

                if (arg_vars.len == 0) {
                    payload_value = null;
                } else if (arg_vars.len == 1) {
                    // Get the payload layout from the variant
                    const variant_layout = acc.getVariantLayout(tag_index);
                    const arg_var = arg_vars[0];
                    const arg_resolved = self.runtime_types.resolveVar(arg_var);
                    const effective_layout = if (arg_resolved.desc.content == .rigid) blk: {
                        if (self.rigid_subst.get(arg_resolved.var_)) |subst_var| {
                            break :blk self.getRuntimeLayout(subst_var) catch variant_layout;
                        }
                        break :blk variant_layout;
                    } else variant_layout;

                    payload_value = StackValue{
                        .layout = effective_layout,
                        .ptr = value.ptr, // Payload is at offset 0
                        .is_initialized = true,
                        .rt_var = arg_var,
                    };
                } else {
                    // Multiple args: the payload is a tuple at offset 0
                    const variant_layout = acc.getVariantLayout(tag_index);
                    // For multiple args, we need a tuple type - use value's rt_var as fallback
                    // since the exact tuple type construction is complex
                    payload_value = StackValue{
                        .layout = variant_layout,
                        .ptr = value.ptr,
                        .is_initialized = true,
                        .rt_var = value.rt_var,
                    };
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
                    try payload.copyToPtr(&self.runtime_layout_store, data_ptr);
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
            .env = self.root_env, // Use root_env for consistent identifier lookups
            .runtime_types = self.runtime_types,
            .layout_store = &self.runtime_layout_store,
            .type_scope = &self.empty_scope,
        };
    }

    /// Context for the to_inspect callback containing both interpreter and RocOps.
    const ToInspectCallbackContext = struct {
        interpreter: *Interpreter,
        roc_ops: *RocOps,
    };

    /// Make a render context with to_inspect callback enabled for recursive method calls.
    /// This version is used when rendering values that may contain nested nominal types
    /// with custom to_inspect methods (e.g., inside records).
    fn makeRenderCtxWithCallback(self: *Interpreter, callback_ctx: *ToInspectCallbackContext) render_helpers.RenderCtx {
        return .{
            .allocator = self.allocator,
            .env = self.root_env,
            .runtime_types = self.runtime_types,
            .layout_store = &self.runtime_layout_store,
            .type_scope = &self.empty_scope,
            .to_inspect_callback = toInspectCallback,
            .callback_ctx = callback_ctx,
        };
    }

    /// Callback for render_helpers to handle nominal types with custom to_inspect methods.
    /// Returns the rendered string if the type has a to_inspect method, null otherwise.
    fn toInspectCallback(ctx: *anyopaque, value: StackValue, rt_var: types.Var) ?[]u8 {
        const cb_ctx: *ToInspectCallbackContext = @ptrCast(@alignCast(ctx));
        const self = cb_ctx.interpreter;
        const roc_ops = cb_ctx.roc_ops;

        // Check if this is a nominal type with to_inspect
        const resolved = self.runtime_types.resolveVar(rt_var);
        if (resolved.desc.content != .structure) return null;
        const nom = switch (resolved.desc.content.structure) {
            .nominal_type => |n| n,
            else => return null,
        };

        const maybe_method = self.tryResolveMethodByIdent(
            nom.origin_module,
            nom.ident.ident_idx,
            self.env.idents.to_inspect,
            roc_ops,
            rt_var,
        ) catch return null;

        const method_func = maybe_method orelse return null;
        defer method_func.decref(&self.runtime_layout_store, roc_ops);

        // Found to_inspect - call it synchronously
        if (method_func.layout.tag != .closure) return null;

        const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));
        const params = self.env.store.slicePatterns(closure_header.params);
        if (params.len != 1) return null;

        // Save state before calling to_inspect
        const saved_env = self.env;
        const saved_bindings_len = self.bindings.items.len;
        self.env = @constCast(closure_header.source_env);

        defer {
            self.env = saved_env;
            self.bindings.shrinkRetainingCapacity(saved_bindings_len);
        }

        // Copy the value to pass to the method
        // Important: use the correct rt_var (from the type system) not value.rt_var
        // (which may be a fresh variable from record field access)
        var copied_value = self.pushCopy(value) catch return null;
        copied_value.rt_var = rt_var;

        // Bind the parameter
        self.bindings.append(.{
            .pattern_idx = params[0],
            .value = copied_value,
            .expr_idx = null, // expr_idx not used for inspect method parameter bindings
            .source_env = self.env,
        }) catch return null;

        // Evaluate the method body
        const result = self.eval(closure_header.body_idx, roc_ops) catch return null;
        defer result.decref(&self.runtime_layout_store, roc_ops);

        // The result should be a Str
        if (result.layout.tag != .scalar) return null;
        if (result.layout.data.scalar.tag != .str) return null;

        const rs: *const builtins.str.RocStr = @ptrCast(@alignCast(result.ptr.?));
        const s = rs.asSlice();

        // Return a copy of the string
        return self.allocator.dupe(u8, s) catch return null;
    }

    pub fn renderValueRoc(self: *Interpreter, value: StackValue) Error![]u8 {
        var ctx = self.makeRenderCtx();
        return render_helpers.renderValueRoc(&ctx, value);
    }

    // Helper for REPL and tests: render a value given its runtime type var.
    // Uses callback-enabled context for recursive to_inspect handling on nested nominal types.
    pub fn renderValueRocWithType(self: *Interpreter, value: StackValue, rt_var: types.Var, roc_ops: *RocOps) Error![]u8 {
        var cb_ctx = ToInspectCallbackContext{
            .interpreter = self,
            .roc_ops = roc_ops,
        };
        var ctx = self.makeRenderCtxWithCallback(&cb_ctx);
        return render_helpers.renderValueRocWithType(&ctx, value, rt_var);
    }

    fn makeListSliceValue(
        self: *Interpreter,
        list_layout: Layout,
        elem_layout: Layout,
        source: RocList,
        start: usize,
        count: usize,
        rt_var: types.Var,
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

        const dest = try self.pushRaw(actual_list_layout, 0, rt_var);
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
            if (comptime trace_refcount and builtin.os.tag != .freestanding) {
                const stderr_file: std.fs.File = .stderr();
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "[INTERP] trimBindingList decref binding idx={} ptr=0x{x}\n", .{
                    idx,
                    @intFromPtr(list.items[idx].value.ptr),
                }) catch "[INTERP] trimBindingList decref\n";
                stderr_file.writeAll(msg) catch {};
            }
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
        expr_idx: ?can.CIR.Expr.Idx,
    ) !bool {
        const pat = self.env.store.getPattern(pattern_idx);
        switch (pat) {
            .assign => |_| {
                // Bind entire value to this pattern
                const copied = try self.pushCopy(value);
                // pushCopy preserves rt_var from value
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = copied, .expr_idx = expr_idx, .source_env = self.env });
                return true;
            },
            .as => |as_pat| {
                const before = out_binds.items.len;
                if (!try self.patternMatchesBind(as_pat.pattern, value, value_rt_var, roc_ops, out_binds, expr_idx)) {
                    self.trimBindingList(out_binds, before, roc_ops);
                    return false;
                }

                const alias_value = try self.pushCopy(value);
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
                    const elem_value = try accessor.getElement(idx, elem_vars[idx]);
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

                // Check if the list value itself is polymorphic (from a polymorphic function)
                const value_rt_resolved = self.runtime_types.resolveVar(value_rt_var);
                const list_is_polymorphic = value_rt_resolved.desc.content == .flex or
                    value_rt_resolved.desc.content == .rigid;

                // Get element type from the list value's type if available, otherwise from the pattern
                // Using the value's type preserves proper method bindings through polymorphic calls
                const elem_rt_var: types.Var = if (list_is_polymorphic) blk: {
                    // List came from polymorphic context - create a fresh flex variable for elements
                    // so they maintain their polymorphic nature
                    break :blk try self.runtime_types.fresh();
                } else if (value_rt_resolved.desc.content == .structure and
                    value_rt_resolved.desc.content.structure == .nominal_type)
                blk: {
                    // Use the element type from the list value's actual type
                    // This preserves method bindings through polymorphic function calls
                    const nominal = value_rt_resolved.desc.content.structure.nominal_type;
                    const vars = self.runtime_types.sliceVars(nominal.vars.nonempty);
                    if (vars.len == 2) {
                        break :blk vars[1]; // element type is second var
                    }
                    // Fallback to pattern translation if structure is unexpected
                    const list_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(pattern_idx));
                    const list_rt_content = self.runtime_types.resolveVar(list_rt_var).desc.content;
                    std.debug.assert(list_rt_content == .structure);
                    std.debug.assert(list_rt_content.structure == .nominal_type);
                    const nom = list_rt_content.structure.nominal_type;
                    const pattern_vars = self.runtime_types.sliceVars(nom.vars.nonempty);
                    std.debug.assert(pattern_vars.len == 2);
                    break :blk pattern_vars[1];
                } else blk: {
                    // Value's type is not a nominal List type - extract from pattern
                    const list_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(pattern_idx));
                    const list_rt_content = self.runtime_types.resolveVar(list_rt_var).desc.content;
                    std.debug.assert(list_rt_content == .structure);
                    std.debug.assert(list_rt_content.structure == .nominal_type);
                    const nominal = list_rt_content.structure.nominal_type;
                    const vars = self.runtime_types.sliceVars(nominal.vars.nonempty);
                    std.debug.assert(vars.len == 2);
                    break :blk vars[1];
                };

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
                        const elem_value = try accessor.getElement(idx, elem_rt_var);
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
                        const elem_value = try accessor.getElement(element_idx, elem_rt_var);
                        const before = out_binds.items.len;
                        const matched = try self.patternMatchesBind(suffix_pattern_idx, elem_value, elem_rt_var, roc_ops, out_binds, expr_idx);
                        if (!matched) {
                            self.trimBindingList(out_binds, before, roc_ops);
                            return false;
                        }
                    }

                    if (rest_info.pattern) |rest_pat_idx| {
                        const rest_len = total_len - prefix_len - suffix_len;
                        const rest_value = try self.makeListSliceValue(list_layout, elem_layout, accessor.list, prefix_len, rest_len, value_rt_var);
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
                        const elem_value = try accessor.getElement(idx, elem_rt_var);
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
                const destructs = self.env.store.sliceRecordDestructs(rec_pat.destructs);

                // Empty record pattern {} matches zero-sized types
                if (destructs.len == 0) {
                    // No fields to destructure - matches any empty record (including zst)
                    return value.layout.tag == .record or value.layout.tag == .zst;
                }

                if (value.layout.tag != .record) return false;
                var accessor = try value.asRecord(&self.runtime_layout_store);

                for (destructs) |destruct_idx| {
                    const destruct = self.env.store.getRecordDestruct(destruct_idx);

                    const field_index = accessor.findFieldIndex(destruct.label) orelse return false;
                    const field_ct_var = can.ModuleEnv.varFrom(destruct_idx);
                    const field_var = try self.translateTypeVar(self.env, field_ct_var);
                    const field_value = try accessor.getFieldByIndex(field_index, field_var);

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

                // Build tag list from value's original rt_var.
                // This is critical when a value was created with a narrower type (e.g., [Ok])
                // and is later matched against a wider type (e.g., Try = [Err, Ok]).
                // The discriminant stored in the value is based on the original type's ordering,
                // so we need the original type's tag list to translate it to a tag name.
                var value_tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer value_tag_list.deinit();
                try self.appendUnionTags(value.rt_var, &value_tag_list);

                const tag_data = try self.extractTagValue(value, value_rt_var);

                // Translate pattern's tag ident to runtime env for direct comparison
                const expected_name_str = self.env.getIdent(tag_pat.name);
                const expected_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(expected_name_str));

                // Get the actual tag name from the value by looking up its discriminant
                // in the appropriate tag list (value's original type if available, else expected type)
                const lookup_list = if (value_tag_list.items.len > 0) value_tag_list.items else tag_list.items;
                if (tag_data.index >= lookup_list.len) return false;
                const actual_tag_name = lookup_list[tag_data.index].name;

                // Compare tag names directly instead of comparing discriminant indices.
                // This handles the case where a value's discriminant was set based on a narrower
                // type and needs to match a pattern from a wider type.
                if (actual_tag_name != expected_ident) return false;

                // Find the expected tag's index in the expected type's tag list for payload access
                var expected_index: ?usize = null;
                for (tag_list.items, 0..) |tag_info, i| {
                    if (tag_info.name == expected_ident) {
                        expected_index = i;
                        break;
                    }
                }

                // If the pattern's tag doesn't exist in the union, the match fails
                if (expected_index == null) return false;

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
                    const elem_val = try payload_tuple.getElement(j, arg_vars[j]);
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
        self.translate_rigid_subst.deinit();
        self.flex_type_context.deinit();
        var it = self.poly_cache.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.args.len > 0) {
                self.allocator.free(@constCast(entry.value_ptr.args));
            }
        }
        self.poly_cache.deinit();
        self.module_envs.deinit(self.allocator);
        self.translated_module_envs.deinit(self.allocator);
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
        self.type_writer.deinit();
        self.stack_memory.deinit();
        self.bindings.deinit();
        self.active_closures.deinit();
        self.def_stack.deinit();
        self.scratch_tags.deinit();
        // Free all constant/static strings at once - they were never freed individually
        self.constant_strings_arena.deinit();
    }

    /// Get the module environment for a given origin module identifier.
    /// Returns the current module's env if the identifier matches, otherwise looks it up in the module map.
    /// Note: origin_module may be in runtime_layout_store.env's ident space (after translateTypeVar),
    /// or in the original ident space (for direct lookups), so we check both maps.
    fn getModuleEnvForOrigin(self: *const Interpreter, origin_module: base_pkg.Ident.Idx) ?*const can.ModuleEnv {
        // Check if it's the Builtin module (using pre-translated ident for runtime-translated case)
        if (origin_module.idx == self.translated_builtin_module.idx) {
            // In shim context, builtins are embedded in the main module env
            // (builtin_module_env is null), so fall back to self.env
            return self.builtin_module_env orelse self.env;
        }
        // Also check original builtin ident for non-translated case
        if (origin_module == self.root_env.idents.builtin_module) {
            return self.builtin_module_env orelse self.env;
        }

        // Check if it's the current module (both translated and original idents)
        if (!self.translated_env_module.isNone() and origin_module.idx == self.translated_env_module.idx) {
            return self.env;
        }
        if (self.env.module_name_idx == origin_module) {
            return self.env;
        }

        // Check if it's the app module (both translated and original idents)
        if (self.app_env) |a_env| {
            if (!self.translated_app_module.isNone() and origin_module.idx == self.translated_app_module.idx) {
                return a_env;
            }
            if (a_env.module_name_idx == origin_module) {
                return a_env;
            }
        }

        // Look up in imported modules (original idents)
        if (self.module_envs.get(origin_module)) |env| {
            return env;
        }

        // Look up in translated module envs (for runtime-translated idents)
        // This handles the case where origin_module comes from runtime_layout_store.env's ident space
        return self.translated_module_envs.get(origin_module);
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

    fn resolveMethodFunction(
        self: *Interpreter,
        origin_module: base_pkg.Ident.Idx,
        nominal_ident: base_pkg.Ident.Idx,
        method_name_ident: base_pkg.Ident.Idx,
        roc_ops: *RocOps,
        receiver_rt_var: ?types.Var,
    ) Error!StackValue {
        // Get the module environment for this type's origin
        const origin_env = self.getModuleEnvForOrigin(origin_module) orelse {
            return error.MethodLookupFailed;
        };

        // Use index-based lookup to find the qualified method ident.
        // nominal_ident comes from runtime types - always in runtime_layout_store.env
        // method_name_ident comes from the CIR - in self.env
        const method_ident = origin_env.lookupMethodIdentFromTwoEnvsConst(
            self.runtime_layout_store.env,
            nominal_ident,
            self.env,
            method_name_ident,
        ) orelse {
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

        // Propagate receiver type to flex_type_context BEFORE translating the method's type.
        // This ensures that polymorphic methods like `to` have their type parameters mapped
        // to the correct concrete type (e.g., U8) before the closure is created.
        if (receiver_rt_var) |recv_rt_var| {
            const def_ct_var = can.ModuleEnv.varFrom(target_def_idx);
            const def_resolved = origin_env.types.resolveVar(def_ct_var);

            // If the method has a function type, extract its first parameter type
            // and propagate mappings from the receiver type to it
            if (def_resolved.desc.content == .structure) {
                const flat = def_resolved.desc.content.structure;
                switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => |fn_type| {
                        const param_vars = origin_env.types.sliceVars(fn_type.args);
                        if (param_vars.len > 0) {
                            // The first parameter is the receiver type (e.g., Num a)
                            // Propagate mappings from the concrete receiver to this type
                            try self.propagateFlexMappings(@constCast(origin_env), param_vars[0], recv_rt_var);
                        }
                    },
                    else => {},
                }
            }
        }

        // Translate the def's type var to runtime
        const def_var = can.ModuleEnv.varFrom(target_def_idx);
        const rt_def_var = try self.translateTypeVar(@constCast(origin_env), def_var);

        // Evaluate the method's expression
        const method_value = try self.evalWithExpectedType(target_def.expr, roc_ops, rt_def_var);

        return method_value;
    }

    /// Try to resolve a method by ident. Returns null if method not found.
    /// Used for special methods like `to_inspect` where we need to look up by ident.
    fn tryResolveMethodByIdent(
        self: *Interpreter,
        origin_module: base_pkg.Ident.Idx,
        nominal_ident: base_pkg.Ident.Idx,
        method_name_ident: base_pkg.Ident.Idx,
        roc_ops: *RocOps,
        receiver_rt_var: ?types.Var,
    ) Error!?StackValue {
        // Get the module environment for this type's origin
        const origin_env = self.getModuleEnvForOrigin(origin_module) orelse {
            return null;
        };

        // Use index-based method lookup - the method_name_ident is in self.env's ident space,
        // nominal_ident is in runtime_layout_store.env's ident space
        const method_ident = origin_env.lookupMethodIdentFromTwoEnvsConst(
            self.runtime_layout_store.env,
            nominal_ident,
            self.env,
            method_name_ident,
        ) orelse {
            return null;
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
            return null;
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

        // Propagate receiver type to flex_type_context BEFORE translating the method's type.
        // This ensures that polymorphic methods have their type parameters mapped
        // to the correct concrete type before the closure is created.
        if (receiver_rt_var) |recv_rt_var| {
            const def_ct_var = can.ModuleEnv.varFrom(target_def_idx);
            const def_resolved = origin_env.types.resolveVar(def_ct_var);

            // If the method has a function type, extract its first parameter type
            // and propagate mappings from the receiver type to it
            if (def_resolved.desc.content == .structure) {
                const flat = def_resolved.desc.content.structure;
                switch (flat) {
                    .fn_pure, .fn_effectful, .fn_unbound => |fn_type| {
                        const param_vars = origin_env.types.sliceVars(fn_type.args);
                        if (param_vars.len > 0) {
                            // The first parameter is the receiver type (e.g., Num a)
                            // Propagate mappings from the concrete receiver to this type
                            try self.propagateFlexMappings(@constCast(origin_env), param_vars[0], recv_rt_var);
                        }
                    },
                    else => {},
                }
            }
        }

        // Translate the def's type var to runtime
        const def_var = can.ModuleEnv.varFrom(target_def_idx);
        const rt_def_var = try self.translateTypeVar(@constCast(origin_env), def_var);

        // Evaluate the method's expression
        const method_value = try self.evalWithExpectedType(target_def.expr, roc_ops, rt_def_var);

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

    /// Create List(Str) type for runtime type propagation
    fn mkListStrTypeRuntime(self: *Interpreter) !types.Var {
        const origin_module_id = self.root_env.idents.builtin_module;

        // Create Builtin.Str type for the element
        const str_type_name = "Builtin.Str";
        const str_type_name_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(str_type_name));
        const str_type_ident = types.TypeIdent{ .ident_idx = str_type_name_ident };

        const empty_tag_union_content = types.Content{ .structure = .empty_tag_union };
        const ext_var = try self.runtime_types.freshFromContent(empty_tag_union_content);
        const empty_tag_union = types.TagUnion{
            .tags = types.Tag.SafeMultiList.Range.empty(),
            .ext = ext_var,
        };
        const str_backing_content = types.Content{ .structure = .{ .tag_union = empty_tag_union } };
        const str_backing_var = try self.runtime_types.freshFromContent(str_backing_content);
        const no_type_args: []const types.Var = &.{};
        const str_content = try self.runtime_types.mkNominal(str_type_ident, str_backing_var, no_type_args, origin_module_id, false);
        const str_var = try self.runtime_types.freshFromContent(str_content);

        // Create Builtin.List type with Str as element type
        const list_type_name = "Builtin.List";
        const list_type_name_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(list_type_name));
        const list_type_ident = types.TypeIdent{ .ident_idx = list_type_name_ident };

        const ext_var2 = try self.runtime_types.freshFromContent(empty_tag_union_content);
        const empty_tag_union2 = types.TagUnion{
            .tags = types.Tag.SafeMultiList.Range.empty(),
            .ext = ext_var2,
        };
        const list_backing_content = types.Content{ .structure = .{ .tag_union = empty_tag_union2 } };
        const list_backing_var = try self.runtime_types.freshFromContent(list_backing_content);

        // List has one type argument (element type)
        // Use stack-allocated array - mkNominal copies via appendVars so no heap allocation needed
        const type_args: [1]types.Var = .{str_var};
        const list_content = try self.runtime_types.mkNominal(list_type_ident, list_backing_var, &type_args, origin_module_id, false);
        return try self.runtime_types.freshFromContent(list_content);
    }

    /// Create List(element_type) for runtime type propagation.
    /// Used when a list's type variable resolved to flex and we need a proper nominal type.
    fn createListTypeWithElement(self: *Interpreter, element_rt_var: types.Var) !types.Var {
        const origin_module_id = self.root_env.idents.builtin_module;

        // Create Builtin.List type with the given element type
        const list_type_name = "Builtin.List";
        const list_type_name_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(list_type_name));
        const list_type_ident = types.TypeIdent{ .ident_idx = list_type_name_ident };

        const empty_tag_union_content = types.Content{ .structure = .empty_tag_union };
        const ext_var = try self.runtime_types.freshFromContent(empty_tag_union_content);
        const empty_tag_union = types.TagUnion{
            .tags = types.Tag.SafeMultiList.Range.empty(),
            .ext = ext_var,
        };
        const list_backing_content = types.Content{ .structure = .{ .tag_union = empty_tag_union } };
        const list_backing_var = try self.runtime_types.freshFromContent(list_backing_content);

        // Create a fresh copy of the element type to avoid corruption from later unifications.
        // If we use the original element_rt_var directly, it can be unified with other types
        // during evaluation (e.g., during equality checking), corrupting this list type.
        const elem_resolved = self.runtime_types.resolveVar(element_rt_var);
        const fresh_elem_var = try self.runtime_types.freshFromContent(elem_resolved.desc.content);

        // List has one type argument (element type)
        const type_args: [1]types.Var = .{fresh_elem_var};
        const list_content = try self.runtime_types.mkNominal(list_type_ident, list_backing_var, &type_args, origin_module_id, false);
        return try self.runtime_types.freshFromContent(list_content);
    }

    /// Create a type variable from a layout. Used as a fallback when type info is corrupted.
    /// Recursively handles nested types (e.g., List(List(Dec))).
    fn createTypeFromLayout(self: *Interpreter, lay: layout.Layout) !types.Var {
        return switch (lay.tag) {
            .list, .list_of_zst => blk: {
                // Get element layout and recursively create element type
                const elem_layout = self.runtime_layout_store.getLayout(lay.data.list);
                const elem_type = try self.createTypeFromLayout(elem_layout);
                // Create List type with element type
                break :blk try self.createListTypeWithElement(elem_type);
            },
            .scalar => blk: {
                const scalar = lay.data.scalar;
                switch (scalar.tag) {
                    .int => {
                        const type_name = switch (scalar.data.int) {
                            .i8 => "I8",
                            .i16 => "I16",
                            .i32 => "I32",
                            .i64 => "I64",
                            .i128 => "I128",
                            .u8 => "U8",
                            .u16 => "U16",
                            .u32 => "U32",
                            .u64 => "U64",
                            .u128 => "U128",
                        };
                        const content = try self.mkNumberTypeContentRuntime(type_name);
                        break :blk try self.runtime_types.freshFromContent(content);
                    },
                    .frac => {
                        const type_name = switch (scalar.data.frac) {
                            .dec => "Dec",
                            .f32 => "F32",
                            .f64 => "F64",
                        };
                        const content = try self.mkNumberTypeContentRuntime(type_name);
                        break :blk try self.runtime_types.freshFromContent(content);
                    },
                    .str => {
                        // Create Str type
                        const origin_module_id = self.root_env.idents.builtin_module;
                        const str_type_name = "Builtin.Str";
                        const str_type_name_ident = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(str_type_name));
                        const str_type_ident = types.TypeIdent{ .ident_idx = str_type_name_ident };
                        const empty_tag_union_content = types.Content{ .structure = .empty_tag_union };
                        const ext_var = try self.runtime_types.freshFromContent(empty_tag_union_content);
                        const empty_tag_union = types.TagUnion{
                            .tags = types.Tag.SafeMultiList.Range.empty(),
                            .ext = ext_var,
                        };
                        const str_backing_content = types.Content{ .structure = .{ .tag_union = empty_tag_union } };
                        const str_backing_var = try self.runtime_types.freshFromContent(str_backing_content);
                        const no_type_args: []const types.Var = &.{};
                        const str_content = try self.runtime_types.mkNominal(str_type_ident, str_backing_var, no_type_args, origin_module_id, false);
                        break :blk try self.runtime_types.freshFromContent(str_content);
                    },
                    else => {
                        // Default to fresh var for unknown scalar types
                        break :blk try self.runtime_types.fresh();
                    },
                }
            },
            else => {
                // For other layouts, create a fresh var (fallback)
                return try self.runtime_types.fresh();
            },
        };
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
            true, // Number types are opaque
        );
    }

    /// Get the layout for a runtime type var using the O(1) biased slot array.
    pub fn getRuntimeLayout(self: *Interpreter, type_var: types.Var) !layout.Layout {
        var resolved = self.runtime_types.resolveVar(type_var);

        // Apply rigid variable substitution if this is a rigid variable
        // Follow the substitution chain until we reach a non-rigid variable or run out of substitutions
        // Use a counter to prevent infinite loops from cyclic substitutions
        var count: u32 = 0;
        while (resolved.desc.content == .rigid) {
            if (self.rigid_subst.get(resolved.var_)) |substituted_var| {
                count += 1;
                if (count > 1000) break; // Prevent infinite loops
                resolved = self.runtime_types.resolveVar(substituted_var);
            } else {
                break;
            }
        }

        const idx: usize = @intFromEnum(resolved.var_);
        try self.ensureVarLayoutCapacity(idx + 1);
        const slot_ptr = &self.var_to_layout_slot.items[idx];

        // If we have a flex var, default to Dec.
        // Note: flex_type_context mappings are handled in translateTypeVar, not here.
        // This function receives runtime type vars that should already be resolved.
        if (resolved.desc.content == .flex) {
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
                .nominal_type => try self.runtime_layout_store.addTypeVar(resolved.var_, &self.empty_scope),
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

    /// Collect all rigid vars from a type, traversing the structure recursively.
    /// Used to map rigids in nominal type backings to their corresponding type args.
    fn collectRigidsFromType(
        allocator: std.mem.Allocator,
        module: *can.ModuleEnv,
        var_: types.Var,
        rigids: *std.ArrayList(types.Var),
        visited: *std.AutoHashMap(types.Var, void),
    ) error{OutOfMemory}!void {
        const resolved = module.types.resolveVar(var_);
        if (visited.contains(resolved.var_)) return;
        try visited.put(resolved.var_, {});

        switch (resolved.desc.content) {
            .rigid => {
                // Found a rigid - add if not already present
                for (rigids.items) |r| {
                    if (@intFromEnum(r) == @intFromEnum(resolved.var_)) return;
                }
                try rigids.append(allocator, resolved.var_);
            },
            .structure => |flat| switch (flat) {
                .tag_union => |tu| {
                    const tags = module.types.getTagsSlice(tu.tags);
                    for (tags.items(.args)) |tag_args| {
                        for (module.types.sliceVars(tag_args)) |arg| {
                            try collectRigidsFromType(allocator, module, arg, rigids, visited);
                        }
                    }
                    // Also traverse extension
                    try collectRigidsFromType(allocator, module, tu.ext, rigids, visited);
                },
                .tuple => |t| {
                    for (module.types.sliceVars(t.elems)) |elem| {
                        try collectRigidsFromType(allocator, module, elem, rigids, visited);
                    }
                },
                .record => |rec| {
                    const fields = module.types.getRecordFieldsSlice(rec.fields);
                    for (fields.items(.var_)) |field_var| {
                        try collectRigidsFromType(allocator, module, field_var, rigids, visited);
                    }
                    // Also traverse extension
                    try collectRigidsFromType(allocator, module, rec.ext, rigids, visited);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |f| {
                    for (module.types.sliceVars(f.args)) |arg| {
                        try collectRigidsFromType(allocator, module, arg, rigids, visited);
                    }
                    try collectRigidsFromType(allocator, module, f.ret, rigids, visited);
                },
                else => {},
            },
            .alias => |alias| {
                try collectRigidsFromType(allocator, module, module.types.getAliasBackingVar(alias), rigids, visited);
            },
            .flex => {},
            .err => {},
            .recursion_var => {},
        }
    }

    /// Collect all rigid vars from a RUNTIME type, traversing the structure.
    /// Similar to collectRigidsFromType but works on the runtime type store.
    fn collectRigidsFromRuntimeType(
        self: *Interpreter,
        allocator: std.mem.Allocator,
        var_: types.Var,
        rigids: *std.ArrayListUnmanaged(types.Var),
        visited: *std.AutoHashMap(types.Var, void),
    ) error{OutOfMemory}!void {
        const resolved = self.runtime_types.resolveVar(var_);
        if (visited.contains(resolved.var_)) return;
        try visited.put(resolved.var_, {});

        switch (resolved.desc.content) {
            .rigid => {
                // Found a rigid - add if not already present
                for (rigids.items) |r| {
                    if (@intFromEnum(r) == @intFromEnum(resolved.var_)) return;
                }
                try rigids.append(allocator, resolved.var_);
            },
            .structure => |flat| switch (flat) {
                .tag_union => |tu| {
                    const tags = self.runtime_types.getTagsSlice(tu.tags);
                    for (tags.items(.args)) |tag_args| {
                        for (self.runtime_types.sliceVars(tag_args)) |arg| {
                            try self.collectRigidsFromRuntimeType(allocator, arg, rigids, visited);
                        }
                    }
                    // Also traverse extension
                    try self.collectRigidsFromRuntimeType(allocator, tu.ext, rigids, visited);
                },
                .tuple => |t| {
                    for (self.runtime_types.sliceVars(t.elems)) |elem| {
                        try self.collectRigidsFromRuntimeType(allocator, elem, rigids, visited);
                    }
                },
                .record => |rec| {
                    const fields = self.runtime_types.getRecordFieldsSlice(rec.fields);
                    for (fields.items(.var_)) |field_var| {
                        try self.collectRigidsFromRuntimeType(allocator, field_var, rigids, visited);
                    }
                    // Also traverse extension
                    try self.collectRigidsFromRuntimeType(allocator, rec.ext, rigids, visited);
                },
                .fn_pure, .fn_effectful, .fn_unbound => |f| {
                    for (self.runtime_types.sliceVars(f.args)) |arg| {
                        try self.collectRigidsFromRuntimeType(allocator, arg, rigids, visited);
                    }
                    try self.collectRigidsFromRuntimeType(allocator, f.ret, rigids, visited);
                },
                else => {},
            },
            .alias => |alias| {
                try self.collectRigidsFromRuntimeType(allocator, self.runtime_types.getAliasBackingVar(alias), rigids, visited);
            },
            .flex => {},
            .err => {},
            .recursion_var => {},
        }
    }

    /// Add rigid -> type_arg mappings to empty_scope for layout computation.
    /// The layout store uses TypeScope.lookup() when it encounters rigids,
    /// so this ensures nested rigids in nominal types get properly substituted.
    fn addRigidMappingsToScope(
        self: *Interpreter,
        rigids: []const types.Var,
        type_args: []const types.Var,
    ) !void {
        // Ensure we have at least one scope level
        if (self.empty_scope.scopes.items.len == 0) {
            try self.empty_scope.scopes.append(types.VarMap.init(self.allocator));
        }

        // Add mappings to the first scope
        const scope = &self.empty_scope.scopes.items[0];
        const num_mappings = @min(rigids.len, type_args.len);
        for (0..num_mappings) |i| {
            // Resolve the type_arg - if it's a rigid that we already have a mapping for,
            // follow the chain to get the concrete type
            var resolved_type_arg = type_args[i];
            const type_arg_resolved = self.runtime_types.resolveVar(type_args[i]);
            if (type_arg_resolved.desc.content == .rigid) {
                // Type arg is itself a rigid - look it up in empty_scope or rigid_subst
                if (self.empty_scope.lookup(type_args[i])) |mapped| {
                    resolved_type_arg = mapped;
                } else if (self.rigid_subst.get(type_args[i])) |mapped| {
                    resolved_type_arg = mapped;
                }
            }

            // Skip if we'd be mapping rigid -> same rigid (useless)
            if (rigids[i] == resolved_type_arg) {
                continue;
            }

            try scope.put(rigids[i], resolved_type_arg);
        }
    }

    /// Propagate flex type context mappings by walking compile-time and runtime types in parallel.
    /// This is used when entering polymorphic functions to map flex vars in the function's type
    /// to their concrete runtime types based on the arguments.
    ///
    /// For example, if CT type is `Num a` and RT type is `U8`, we need to extract `a` and map it to U8.
    /// This ensures that when we later encounter just `a` (e.g., in `List a` for an empty list),
    /// we can find the mapping.
    fn propagateFlexMappings(self: *Interpreter, module: *can.ModuleEnv, ct_var: types.Var, rt_var: types.Var) Error!void {
        const ct_resolved = module.types.resolveVar(ct_var);
        const rt_resolved = self.runtime_types.resolveVar(rt_var);

        // If the CT type is a flex var, add the mapping directly
        if (ct_resolved.desc.content == .flex) {
            const flex_key = ModuleVarKey{ .module = module, .var_ = ct_resolved.var_ };
            try self.flex_type_context.put(flex_key, rt_var);
            return;
        }

        // If the CT type is a rigid var, also add to flex_type_context.
        // This is needed because: in polymorphic functions, the parameter type might be rigid
        // (from the function signature), but flex vars inside the function body were unified
        // with this rigid var at compile time. After serialization, these unifications might
        // not be preserved, so we need to map both the rigid var and any flex vars that might
        // be looking for it.
        if (ct_resolved.desc.content == .rigid) {
            const flex_key = ModuleVarKey{ .module = module, .var_ = ct_resolved.var_ };
            try self.flex_type_context.put(flex_key, rt_var);
            return;
        }

        // If the CT type is a structure, walk its children and propagate recursively
        if (ct_resolved.desc.content == .structure) {
            const ct_flat = ct_resolved.desc.content.structure;

            switch (ct_flat) {
                .nominal_type => |ct_nom| {
                    // For nominal types like `Num a`, extract the type args and map them
                    const ct_args = module.types.sliceNominalArgs(ct_nom);

                    // If the RT type is also a nominal type, try to match up the args
                    if (rt_resolved.desc.content == .structure) {
                        if (rt_resolved.desc.content.structure == .nominal_type) {
                            const rt_nom = rt_resolved.desc.content.structure.nominal_type;
                            const rt_args = self.runtime_types.sliceNominalArgs(rt_nom);

                            const min_args = @min(ct_args.len, rt_args.len);
                            for (0..min_args) |i| {
                                try self.propagateFlexMappings(module, ct_args[i], rt_args[i]);
                            }

                            // If CT has more args than RT (common case: CT is `Num a` but RT is `U8` with no args),
                            // we need to map those CT args to the RT type itself.
                            // This handles the case where `Num a` in CT should map `a` to U8.
                            if (ct_args.len > rt_args.len) {
                                for (rt_args.len..ct_args.len) |i| {
                                    try self.propagateFlexMappings(module, ct_args[i], rt_var);
                                }
                            }
                        }
                    }
                },
                .tuple => |ct_tuple| {
                    if (rt_resolved.desc.content == .structure and rt_resolved.desc.content.structure == .tuple) {
                        const ct_elems = module.types.sliceVars(ct_tuple.elems);
                        const rt_tuple = rt_resolved.desc.content.structure.tuple;
                        const rt_elems = self.runtime_types.sliceVars(rt_tuple.elems);

                        const min_elems = @min(ct_elems.len, rt_elems.len);
                        for (0..min_elems) |i| {
                            try self.propagateFlexMappings(module, ct_elems[i], rt_elems[i]);
                        }
                    }
                },
                .fn_pure, .fn_effectful, .fn_unbound => {
                    // Function type propagation is complex - skip for now
                    // The main use case we need is nominal types like `Num a`
                },
                .tag_union => {
                    // Tag union propagation is complex - skip for now
                    // This case is less common for the numeric range use case we're fixing
                },
                .record => {
                    // Record propagation is complex - skip for now
                    // This case is less common for the numeric range use case we're fixing
                },
                else => {
                    // For other structure types, no recursive propagation needed
                },
            }
        }

        // Also add a mapping for the outer type itself (in case it's referenced directly)
        if (ct_resolved.desc.content == .flex or ct_resolved.desc.content == .rigid) {
            const flex_key = ModuleVarKey{ .module = module, .var_ = ct_resolved.var_ };
            try self.flex_type_context.put(flex_key, rt_var);
        }
    }

    /// Translate a compile-time type variable from a module's type store to the runtime type store.
    /// Handles most structural types: tag unions, tuples, records, functions, and nominal types.
    /// Uses caching to handle recursive types and avoid duplicate work.
    pub fn translateTypeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var) Error!types.Var {
        const resolved = module.types.resolveVar(compile_var);

        const key = ModuleVarKey{ .module = module, .var_ = resolved.var_ };

        // Check flex_type_context BEFORE translate_cache for flex and rigid types.
        // This is critical for polymorphic functions: the same compile-time flex/rigid var
        // may need to translate to different runtime types depending on calling context.
        // For example, `sum = |num| 0 + num` called as U64.to_str(sum(2400)) needs
        // the literal 0 to become U64, not the cached Dec default.
        if (resolved.desc.content == .flex or resolved.desc.content == .rigid) {
            if (self.flex_type_context.get(key)) |context_rt_var| {
                return context_rt_var;
            }
        }

        // Skip translate_cache for flex/rigid vars when inside a polymorphic function.
        // The cache may have stale mappings from a different calling context where the
        // flex var defaulted to Dec, but we now have a concrete type from flex_type_context.
        // We check if flex_type_context has ANY entries as a proxy for "inside polymorphic call".
        const in_polymorphic_context = self.flex_type_context.count() > 0;
        const skip_cache_for_this_var = in_polymorphic_context and
            (resolved.desc.content == .flex or resolved.desc.content == .rigid);

        if (!skip_cache_for_this_var) {
            if (self.translate_cache.get(key)) |found| {
                return found;
            }
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
                                // Translate tag name from source module's ident store to runtime_layout_store's ident store
                                const source_name_str = module.getIdent(tag.name);
                                const rt_tag_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));
                                tag.* = .{
                                    .name = rt_tag_name,
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
                                // Translate field name from source module's ident store to runtime ident store
                                const source_field_name_str = module.getIdent(ct_field.name);
                                const rt_field_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_field_name_str));
                                runtime_fields[j] = .{
                                    .name = rt_field_name,
                                    .var_ = try self.translateTypeVar(module, ct_field.var_),
                                };
                            }
                            const rt_fields = try self.runtime_types.appendRecordFields(runtime_fields);
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .record = .{ .fields = rt_fields, .ext = rt_ext } } });
                        },
                        .record_unbound => |fields_range| {
                            // record_unbound has no extension - it's a complete set of fields
                            const ct_fields = module.types.getRecordFieldsSlice(fields_range);
                            var runtime_fields = try self.allocator.alloc(types.RecordField, ct_fields.len);
                            defer self.allocator.free(runtime_fields);
                            var i: usize = 0;
                            while (i < ct_fields.len) : (i += 1) {
                                const f = ct_fields.get(i);
                                // Translate field name from source module's ident store to runtime ident store
                                const source_field_name_str = module.getIdent(f.name);
                                const rt_field_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_field_name_str));
                                runtime_fields[i] = .{
                                    .name = rt_field_name,
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
                            const ct_args = module.types.sliceNominalArgs(nom);

                            // Build rigid → type arg substitution map before translating backing
                            if (ct_args.len > 0) {
                                // Collect rigids from backing type
                                var rigids = try std.ArrayList(types.Var).initCapacity(self.allocator, 8);
                                defer rigids.deinit(self.allocator);
                                var visited = std.AutoHashMap(types.Var, void).init(self.allocator);
                                defer visited.deinit();

                                collectRigidsFromType(self.allocator, module, ct_backing, &rigids, &visited) catch |e| switch (e) {
                                    error.OutOfMemory => return error.OutOfMemory,
                                };

                                // Sort by var ID for positional correspondence with type args
                                std.mem.sort(types.Var, rigids.items, {}, struct {
                                    fn lessThan(_: void, a: types.Var, b: types.Var) bool {
                                        return @intFromEnum(a) < @intFromEnum(b);
                                    }
                                }.lessThan);

                                // Map rigids to type args positionally
                                const num_mappings = @min(rigids.items.len, ct_args.len);
                                for (0..num_mappings) |i| {
                                    try self.translate_rigid_subst.put(rigids.items[i], ct_args[i]);
                                }
                            }

                            // Translate backing (rigids will be substituted via translate_rigid_subst)
                            const rt_backing = try self.translateTypeVar(module, ct_backing);

                            // Clear substitution map for next nominal type
                            self.translate_rigid_subst.clearRetainingCapacity();
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
                            const content = try self.runtime_types.mkNominal(translated_ident, rt_backing, buf, translated_origin, nom.is_opaque);
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
                    // Translate the alias's ident from source module's ident store to runtime ident store
                    const source_alias_str = module.getIdent(alias.ident.ident_idx);
                    const rt_alias_ident_idx = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_alias_str));
                    const rt_alias_ident = types.TypeIdent{ .ident_idx = rt_alias_ident_idx };
                    const content = try self.runtime_types.mkAlias(rt_alias_ident, rt_backing, buf);
                    break :blk try self.runtime_types.register(.{ .content = content, .rank = types.Rank.top_level, .mark = types.Mark.none });
                },
                .recursion_var => |rec_var| {
                    // Translate the structure variable that the recursion var points to
                    const rt_structure = try self.translateTypeVar(module, rec_var.structure);
                    // Translate the recursion var's name (if present) from source module's ident store
                    const rt_name: ?base_pkg.Ident.Idx = if (rec_var.name) |name| blk_name: {
                        const source_name_str = module.getIdent(name);
                        break :blk_name try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));
                    } else null;
                    const content: types.Content = .{ .recursion_var = .{
                        .structure = rt_structure,
                        .name = rt_name,
                    } };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .flex => |flex| {
                    // Note: flex_type_context is checked at the top of translateTypeVar,
                    // before the translate_cache lookup. If we reach here, there was no
                    // contextual override.
                    //
                    // However, if we're in a polymorphic function context (flex_type_context is non-empty)
                    // and there's exactly one mapping, we should use it. This handles the case where
                    // a flex var inside a function body (e.g., the element type of an empty list)
                    // was unified with the function's type parameter at compile time, but the
                    // union-find structure wasn't preserved during serialization.
                    //
                    // For example, in `range_to = |current, end| { var answer = [] ... }`:
                    // - The function has type `Num a, Num a -> List (Num a)` with rigid `a`
                    // - The empty list `[]` has element type `Num flex_b` where `flex_b` was unified with `a`
                    // - After serialization, `flex_b` and `a` are different vars
                    // - If we mapped `a -> U8` from the call arguments, we should use U8 for `flex_b` too
                    //
                    // Check if all entries in flex_type_context map to the same runtime type.
                    // This handles the case where multiple var entries exist (e.g., from parameters
                    // and internal type vars) but they all represent the same type parameter.
                    const ctx_count = self.flex_type_context.count();
                    if (ctx_count > 0) {
                        var it = self.flex_type_context.iterator();
                        var first_rt_var: ?types.Var = null;
                        var all_same = true;
                        while (it.next()) |entry| {
                            const rt_var = entry.value_ptr.*;
                            if (first_rt_var) |first| {
                                // Check if this entry maps to the same runtime type
                                // by comparing the resolved root var
                                const first_resolved = self.runtime_types.resolveVar(first);
                                const this_resolved = self.runtime_types.resolveVar(rt_var);
                                // If they resolve to the same root var, they're the same type
                                if (first_resolved.var_ != this_resolved.var_) {
                                    all_same = false;
                                    break;
                                }
                            } else {
                                first_rt_var = rt_var;
                            }
                        }
                        if (all_same) {
                            if (first_rt_var) |rt_var| {
                                break :blk rt_var;
                            }
                        }
                    }

                    // Translate the flex's name from source module's ident store to runtime ident store (if present)
                    const rt_name: ?base_pkg.Ident.Idx = if (flex.name) |name| blk_name: {
                        const source_name_str = module.getIdent(name);
                        break :blk_name try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));
                    } else null;

                    // Translate static dispatch constraints if present
                    const rt_flex = if (flex.constraints.len() > 0) blk_flex: {
                        const ct_constraints = module.types.sliceStaticDispatchConstraints(flex.constraints);
                        var rt_constraints = try std.ArrayList(types.StaticDispatchConstraint).initCapacity(self.allocator, ct_constraints.len);
                        defer rt_constraints.deinit(self.allocator);

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            // Translate the constraint's fn_name from source module's ident store
                            const ct_fn_name_str = module.getIdent(ct_constraint.fn_name);
                            const rt_fn_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(ct_fn_name_str));
                            try rt_constraints.append(self.allocator, .{
                                .fn_name = rt_fn_name,
                                .fn_var = rt_fn_var,
                                .origin = ct_constraint.origin,
                            });
                        }

                        const rt_constraints_range = try self.runtime_types.appendStaticDispatchConstraints(rt_constraints.items);
                        break :blk_flex types.Flex{
                            .name = rt_name,
                            .constraints = rt_constraints_range,
                        };
                    } else types.Flex{
                        .name = rt_name,
                        .constraints = types.StaticDispatchConstraint.SafeList.Range.empty(),
                    };

                    const content: types.Content = .{ .flex = rt_flex };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .rigid => |rigid| {
                    // Check if this rigid should be substituted (during nominal type backing translation)
                    if (self.translate_rigid_subst.get(resolved.var_)) |substitute_var| {
                        // Translate the substitute type instead of the rigid
                        break :blk try self.translateTypeVar(module, substitute_var);
                    }

                    // Translate the rigid's name from source module's ident store to runtime ident store
                    const source_name_str = module.getIdent(rigid.name);
                    const rt_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(source_name_str));

                    // Translate static dispatch constraints if present
                    const rt_rigid = if (rigid.constraints.len() > 0) blk_rigid: {
                        const ct_constraints = module.types.sliceStaticDispatchConstraints(rigid.constraints);
                        var rt_constraints = try std.ArrayList(types.StaticDispatchConstraint).initCapacity(self.allocator, ct_constraints.len);
                        defer rt_constraints.deinit(self.allocator);

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            // Translate the constraint's fn_name from source module's ident store
                            const ct_fn_name_str = module.getIdent(ct_constraint.fn_name);
                            const rt_fn_name = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(ct_fn_name_str));
                            try rt_constraints.append(self.allocator, .{
                                .fn_name = rt_fn_name,
                                .fn_var = rt_fn_var,
                                .origin = ct_constraint.origin,
                            });
                        }

                        const rt_constraints_range = try self.runtime_types.appendStaticDispatchConstraints(rt_constraints.items);
                        break :blk_rigid types.Rigid{
                            .name = rt_name,
                            .constraints = rt_constraints_range,
                        };
                    } else types.Rigid{
                        .name = rt_name,
                        .constraints = types.StaticDispatchConstraint.SafeList.Range.empty(),
                    };

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
            // Follow the substitution chain to find the final variable
            // Use a counter to prevent infinite loops from cyclic substitutions
            var current = substituted;
            var count: u32 = 0;
            while (self.rigid_subst.get(current)) |next_subst| {
                count += 1;
                if (count > 1000) break; // Prevent infinite loops
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
            .rigid => |rigid| blk: {
                // Replace rigid with fresh flex that can be unified
                // IMPORTANT: Copy the rigid's constraints so numeric constraints are preserved
                const fresh = try self.runtime_types.freshFromContent(.{
                    .flex = .{ .name = rigid.name, .constraints = rigid.constraints },
                });
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
                    .nominal_type => |nt| blk_nominal: {
                        // Add placeholder to prevent infinite recursion on recursive types
                        try subst_map.put(resolved.var_, type_var);

                        // Recursively process type args to find and map any rigids
                        const type_args = self.runtime_types.sliceNominalArgs(nt);
                        for (type_args) |arg_var| {
                            _ = try self.instantiateType(arg_var, subst_map);
                        }

                        // Also process the backing type to find any rigids there
                        const backing = self.runtime_types.getNominalBackingVar(nt);
                        _ = try self.instantiateType(backing, subst_map);

                        // Return original - substitution handled via rigid_subst during layout
                        break :blk_nominal type_var;
                    },
                    .tag_union => |tu| blk_tag_union: {
                        // Add placeholder to prevent infinite recursion
                        try subst_map.put(resolved.var_, type_var);

                        // Recursively process each tag's argument types to find rigids
                        const tags_slice = self.runtime_types.getTagsSlice(tu.tags);
                        for (tags_slice.items(.args)) |args_range| {
                            const arg_vars = self.runtime_types.sliceVars(args_range);
                            for (arg_vars) |arg_var| {
                                _ = try self.instantiateType(arg_var, subst_map);
                            }
                        }

                        // Also process the extension
                        _ = try self.instantiateType(tu.ext, subst_map);

                        // Return original - substitution handled via rigid_subst during layout
                        break :blk_tag_union type_var;
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
        var guard = types.debug.IterationGuard.init("interpreter.gatherTags");
        while (true) {
            guard.tick();
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

    fn polyLookup(self: *Interpreter, module_id: u32, func_id: u32, args: []const types.Var) ?PolyEntry {
        const key = PolyKey.init(module_id, func_id, args);
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
                &self.type_writer,
                &self.unify_scratch,
                &self.unify_scratch.occurs_scratch,
                params[i],
                args[i],
                unify.Conf{ .ctx = .anon, .constraint_origin_var = null },
            );
        }
        // ret_var may now be constrained

        // Apply rigid substitutions to ret_var if needed
        // Follow the substitution chain until we reach a non-rigid variable or run out of substitutions
        // Use a counter to prevent infinite loops from cyclic substitutions
        var resolved_ret = self.runtime_types.resolveVar(ret_var);
        var substituted_ret = ret_var;
        var ret_count: u32 = 0;
        while (resolved_ret.desc.content == .rigid) {
            if (self.rigid_subst.get(resolved_ret.var_)) |subst_var| {
                ret_count += 1;
                if (ret_count > 1000) break; // Prevent infinite loops
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

    // ============================================================================
    // Stack-Safe Interpreter Infrastructure
    // ============================================================================
    //
    // The following types and functions implement a stack-safe interpreter that
    // uses explicit work and value stacks instead of recursive calls. This avoids
    // stack overflow errors on deeply nested programs.

    /// Represents a unit of work to be executed by the stack-safe interpreter.
    pub const WorkItem = union(enum) {
        /// Evaluate an expression and push result to value stack
        eval_expr: EvalExpr,

        /// Apply a continuation to consume values from the value stack
        apply_continuation: Continuation,

        pub const EvalExpr = struct {
            expr_idx: can.CIR.Expr.Idx,
            expected_rt_var: ?types.Var,
        };
    };

    /// Continuations represent "what to do next" after evaluating sub-expressions.
    /// This is the core of continuation-passing style - each continuation captures
    /// exactly what's needed to proceed after a sub-expression completes.
    pub const Continuation = union(enum) {
        /// Return the top value on the stack as the final result.
        /// When this continuation is applied, the main loop will exit and
        /// return the top value from the value stack.
        return_result: void,

        /// Decrement reference count of a value after use.
        /// This is used for cleanup when intermediate values are no longer needed.
        decref_value: DecrefValue,

        /// Restore bindings to a previous length.
        /// Used when exiting a scope to clean up local bindings.
        trim_bindings: TrimBindings,

        /// Short-circuit AND: after evaluating LHS, check if false (short-circuit)
        /// or evaluate RHS.
        and_short_circuit: AndShortCircuit,

        /// Short-circuit OR: after evaluating LHS, check if true (short-circuit)
        /// or evaluate RHS.
        or_short_circuit: OrShortCircuit,

        /// If branch: after evaluating condition, either evaluate body or try next branch.
        if_branch: IfBranch,

        /// Block continuation: process remaining statements in a block.
        block_continue: BlockContinue,

        /// Bind a declaration pattern to the evaluated value.
        bind_decl: BindDecl,

        /// Collect tuple elements: after evaluating an element, either continue
        /// collecting more elements or finalize the tuple.
        tuple_collect: TupleCollect,

        /// Collect list elements: after evaluating an element, either continue
        /// collecting more elements or finalize the list.
        list_collect: ListCollect,

        /// Collect record fields: first evaluate extension (if any), then fields.
        record_collect: RecordCollect,

        /// Handle early return - pop value from stack and signal early return.
        early_return: EarlyReturn,

        /// Collect tag payload arguments and finalize the tag union value.
        tag_collect: TagCollect,

        /// Match expression - try branches after scrutinee is evaluated.
        match_branches: MatchBranches,

        /// Match guard - check guard result and evaluate body or try next branch.
        match_guard: MatchGuard,

        /// Match cleanup - trim bindings after branch body evaluation.
        match_cleanup: MatchCleanup,

        /// Expect check - verify condition is true after evaluation.
        expect_check: ExpectCheck,

        /// Dbg print - print evaluated value and return {}.
        dbg_print: DbgPrint,

        /// Inspect render - render value to Str and push result.
        inspect_render: InspectRender,

        /// String interpolation - collect segment strings.
        str_collect: StrCollect,

        /// Function call - collect arguments after function value is evaluated.
        call_collect_args: CallCollectArgs,

        /// Function call - invoke the closure after all arguments are collected.
        call_invoke_closure: CallInvokeClosure,

        /// Function call - cleanup after function body is evaluated.
        call_cleanup: CallCleanup,

        /// Unary operation - apply method after operand is evaluated.
        unary_op_apply: UnaryOpApply,

        /// Binary operation - evaluate RHS after LHS is evaluated.
        binop_eval_rhs: BinopEvalRhs,

        /// Binary operation - apply method after both operands are evaluated.
        binop_apply: BinopApply,

        /// Dot access - resolve field or method after receiver is evaluated.
        dot_access_resolve: DotAccessResolve,

        /// Dot access method call - collect arguments after receiver is evaluated.
        dot_access_collect_args: DotAccessCollectArgs,

        /// For loop/expression - iterate over list elements after list is evaluated.
        for_iterate: ForIterate,

        /// For loop/expression - process body result and continue to next iteration.
        for_body_done: ForBodyDone,

        /// While loop - check condition and decide whether to continue.
        while_loop_check: WhileLoopCheck,

        /// While loop - process body result and continue to next iteration.
        while_loop_body_done: WhileLoopBodyDone,

        /// Expect statement - check condition after evaluation.
        expect_check_stmt: ExpectCheckStmt,

        /// Reassign statement - update binding after expression evaluation.
        reassign_value: ReassignValue,

        /// Dbg statement - print value after evaluation.
        dbg_print_stmt: DbgPrintStmt,

        /// Inspect statement - render value to Str after evaluation.
        inspect_render_stmt: InspectRenderStmt,

        /// Cleanup after to_inspect method call in statement context.
        inspect_stmt_cleanup: InspectStmtCleanup,

        /// Sort - process comparison result and continue insertion sort.
        sort_compare_result: SortCompareResult,

        /// Negate boolean result on value stack (for != operator).
        negate_bool: void,

        pub const DecrefValue = struct {
            value: StackValue,
        };

        pub const TrimBindings = struct {
            target_len: usize,
        };

        /// Sort compare result - process comparison and continue insertion sort.
        /// Uses insertion sort algorithm which works well with continuation-based evaluation.
        pub const SortCompareResult = struct {
            /// The list being sorted (working copy, will be modified in place)
            list_value: StackValue,
            /// The comparison function closure
            compare_fn: StackValue,
            /// Return type variable for the sort call (for rendering result)
            call_ret_rt_var: ?types.Var,
            /// Saved rigid_subst to restore after sort completes
            saved_rigid_subst: ?std.AutoHashMap(types.Var, types.Var),
            /// Current outer index (element being inserted)
            outer_index: usize,
            /// Current inner index (position being compared)
            inner_index: usize,
            /// Total number of elements
            list_len: usize,
            /// Element size in bytes
            elem_size: usize,
            /// Element layout
            elem_layout: layout.Layout,
            /// Element runtime type variable
            elem_rt_var: types.Var,
        };

        pub const AndShortCircuit = struct {
            rhs_expr: can.CIR.Expr.Idx,
        };

        pub const OrShortCircuit = struct {
            rhs_expr: can.CIR.Expr.Idx,
        };

        pub const IfBranch = struct {
            /// The body to evaluate if condition is true
            body: can.CIR.Expr.Idx,
            /// Remaining branches to try (slice indices into store)
            remaining_branches: []const can.CIR.Expr.IfBranch.Idx,
            /// The final else expression
            final_else: can.CIR.Expr.Idx,
            /// Expected runtime type for the result (propagated from caller)
            expected_rt_var: ?types.Var = null,
        };

        pub const BlockContinue = struct {
            /// Remaining statements to process
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// The final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
            /// True if this block_continue was scheduled after an s_expr statement,
            /// meaning we should pop and discard the expression's result value
            should_discard_value: bool = false,
            /// Expected runtime type for the final expression (propagated from caller)
            expected_rt_var: ?types.Var = null,
        };

        pub const BindDecl = struct {
            /// The pattern to bind
            pattern: can.CIR.Pattern.Idx,
            /// The expression that was evaluated (for expr_idx in binding)
            expr_idx: can.CIR.Expr.Idx,
            /// Remaining statements to process
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// The final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
            /// Expected runtime type for the final expression (propagated from caller)
            expected_rt_var: ?types.Var = null,
        };

        pub const TupleCollect = struct {
            /// Number of collected values on the value stack (collected so far)
            collected_count: usize,
            /// Remaining element expressions to evaluate
            remaining_elems: []const can.CIR.Expr.Idx,
        };

        pub const ListCollect = struct {
            /// Number of collected values on the value stack (collected so far)
            collected_count: usize,
            /// Remaining element expressions to evaluate
            remaining_elems: []const can.CIR.Expr.Idx,
            /// Element runtime type variable (for type-consistent evaluation)
            elem_rt_var: types.Var,
            /// List runtime type variable (for layout computation)
            list_rt_var: types.Var,
        };

        pub const RecordCollect = struct {
            /// Number of collected field values on the value stack (plus base record if any)
            collected_count: usize,
            /// Remaining field expressions to evaluate
            remaining_fields: []const can.CIR.RecordField.Idx,
            /// Record runtime type variable (for layout computation)
            rt_var: types.Var,
            /// Expression idx for caching
            expr_idx: can.CIR.Expr.Idx,
            /// Whether this record has an extension base (the first value on stack will be the base)
            has_extension: bool,
            /// All fields in the record (for name lookup during finalization)
            all_fields: []const can.CIR.RecordField.Idx,
        };

        /// Return the value on the stack as an early return.
        pub const EarlyReturn = struct {};

        pub const TagCollect = struct {
            /// Number of collected payload values on the value stack
            collected_count: usize,
            /// Remaining payload expressions to evaluate
            remaining_args: []const can.CIR.Expr.Idx,
            /// Argument runtime type variables
            arg_rt_vars: []const types.Var,
            /// Tag expression index (for type info)
            expr_idx: can.CIR.Expr.Idx,
            /// Runtime type variable for the tag union
            rt_var: types.Var,
            /// Tag index (discriminant)
            tag_index: usize,
            /// Layout type: 0=record, 1=tuple
            layout_type: u8,
        };

        /// Match continuation - after scrutinee is evaluated, try branches
        pub const MatchBranches = struct {
            /// Match expression index (for result type)
            expr_idx: can.CIR.Expr.Idx,
            /// Scrutinee runtime type variable
            scrutinee_rt_var: types.Var,
            /// Result runtime type variable
            result_rt_var: types.Var,
            /// All branches to try
            branches: []const can.CIR.Expr.Match.Branch.Idx,
            /// Current branch index being tried
            current_branch: usize,
        };

        /// Match guard continuation - after guard is evaluated, check result
        pub const MatchGuard = struct {
            /// Branch body to evaluate if guard passes
            branch_body: can.CIR.Expr.Idx,
            /// Result runtime type variable
            result_rt_var: types.Var,
            /// Bindings start index (to trim on failure)
            bindings_start: usize,
            /// Remaining branches if guard fails
            remaining_branches: []const can.CIR.Expr.Match.Branch.Idx,
            /// Match expression index
            expr_idx: can.CIR.Expr.Idx,
            /// Scrutinee value (kept on stack)
            scrutinee_rt_var: types.Var,
        };

        /// Match cleanup continuation - trim bindings after branch body evaluation
        pub const MatchCleanup = struct {
            /// Bindings start index to trim to
            bindings_start: usize,
        };

        /// Expect continuation - after condition is evaluated, check if true
        pub const ExpectCheck = struct {
            /// Original expect expression index (for failure reporting)
            expr_idx: can.CIR.Expr.Idx,
            /// Body expression index (for failure reporting)
            body_expr: can.CIR.Expr.Idx,
        };

        /// Dbg continuation - after expression is evaluated, print and return {}
        pub const DbgPrint = struct {
            /// Original dbg expression index (for type info)
            expr_idx: can.CIR.Expr.Idx,
            /// Inner expression runtime type variable
            inner_rt_var: types.Var,
        };

        /// Inspect continuation - after expression is evaluated, render to Str and push result
        pub const InspectRender = struct {
            /// Original inspect expression index (for type info)
            expr_idx: can.CIR.Expr.Idx,
            /// Inner expression runtime type variable
            inner_rt_var: types.Var,
        };

        /// String interpolation continuation - collect segment strings
        pub const StrCollect = struct {
            /// Number of segments already collected (as strings on value stack)
            collected_count: usize,
            /// Total number of segments
            total_count: usize,
            /// Remaining segment expressions to evaluate
            remaining_segments: []const can.CIR.Expr.Idx,
            /// Whether we need to convert the top value to a string (just evaluated an expr)
            needs_conversion: bool,
        };

        /// Function call - collect arguments after function value is evaluated
        pub const CallCollectArgs = struct {
            /// Number of arguments already collected on the value stack
            collected_count: usize,
            /// Remaining argument expression indices
            remaining_args: []const can.CIR.Expr.Idx,
            /// Runtime type variables for all arguments (for type-consistent evaluation)
            arg_rt_vars: []const types.Var,
            /// Return type variable for the call
            call_ret_rt_var: types.Var,
            /// Whether type instantiation was performed (need to restore rigid_subst)
            did_instantiate: bool,
        };

        /// Function call - invoke the closure after all arguments are collected
        pub const CallInvokeClosure = struct {
            /// Number of arguments on value stack (plus function value)
            arg_count: usize,
            /// Return type variable for the call
            call_ret_rt_var: types.Var,
            /// Whether type instantiation was performed
            did_instantiate: bool,
            /// Saved rigid_subst to restore after the call completes
            saved_rigid_subst: ?std.AutoHashMap(types.Var, types.Var),
            /// Allocated arg_rt_vars slice to free after call completes
            arg_rt_vars_to_free: ?[]const types.Var,
        };

        /// Function call - cleanup after function body is evaluated
        pub const CallCleanup = struct {
            /// Environment to restore
            saved_env: *can.ModuleEnv,
            /// Bindings length to restore to
            saved_bindings_len: usize,
            /// Number of parameter bindings that were added
            param_count: usize,
            /// Whether to pop an active closure
            has_active_closure: bool,
            /// Whether type instantiation was performed
            did_instantiate: bool,
            /// Return type variable for the call (used for rendering results)
            call_ret_rt_var: ?types.Var,
            /// Saved rigid_subst to restore after method call (for polymorphic dispatch)
            saved_rigid_subst: ?std.AutoHashMap(types.Var, types.Var),
            /// Saved flex_type_context to restore after call (for polymorphic parameter types)
            saved_flex_type_context: ?std.AutoHashMap(ModuleVarKey, types.Var),
            /// Allocated arg_rt_vars slice to free (null if none)
            arg_rt_vars_to_free: ?[]const types.Var,
        };

        /// Unary operation - apply method after operand is evaluated
        pub const UnaryOpApply = struct {
            /// Method identifier (negate or not)
            method_ident: base_pkg.Ident.Idx,
            /// Runtime type of the operand (for method resolution)
            operand_rt_var: types.Var,
        };

        /// Binary operation - evaluate RHS after LHS is evaluated
        pub const BinopEvalRhs = struct {
            /// Right operand expression index
            rhs_expr: can.CIR.Expr.Idx,
            /// Method identifier (plus, minus, times, etc.)
            method_ident: base_pkg.Ident.Idx,
            /// LHS runtime type variable (for method resolution)
            lhs_rt_var: types.Var,
            /// RHS runtime type variable
            rhs_rt_var: types.Var,
            /// Whether to negate the result (for != operator)
            negate_result: bool,
        };

        /// Binary operation - apply method after both operands are evaluated
        pub const BinopApply = struct {
            /// Method identifier
            method_ident: base_pkg.Ident.Idx,
            /// Receiver type (LHS) for method resolution
            receiver_rt_var: types.Var,
            /// RHS runtime type variable (for structural equality)
            rhs_rt_var: types.Var,
            /// Whether to negate the result (for != operator)
            negate_result: bool,
        };

        /// Dot access - resolve field or method after receiver is evaluated
        pub const DotAccessResolve = struct {
            /// Field/method name
            field_name: base_pkg.Ident.Idx,
            /// Optional method arguments (null for field access)
            method_args: ?can.CIR.Expr.Span,
            /// Receiver runtime type variable
            receiver_rt_var: types.Var,
            /// Expression index (for return type)
            expr_idx: can.CIR.Expr.Idx,
        };

        /// Dot access method call - collect arguments after receiver is evaluated
        pub const DotAccessCollectArgs = struct {
            /// Method name
            method_name: base_pkg.Ident.Idx,
            /// Number of arguments already collected on the value stack
            collected_count: usize,
            /// Remaining argument expression indices
            remaining_args: []const can.CIR.Expr.Idx,
            /// Receiver runtime type variable (for method resolution)
            receiver_rt_var: types.Var,
            /// Expression index (for return type)
            expr_idx: can.CIR.Expr.Idx,
        };

        /// For loop/expression - iterate over list elements
        pub const ForIterate = struct {
            /// The list value being iterated (stored to access elements)
            list_value: StackValue,
            /// Current iteration index
            current_index: usize,
            /// Total number of elements in the list
            list_len: usize,
            /// Element size in bytes
            elem_size: usize,
            /// Element layout
            elem_layout: layout.Layout,
            /// Pattern to bind each element to
            pattern: can.CIR.Pattern.Idx,
            /// Pattern runtime type variable
            patt_rt_var: types.Var,
            /// Body expression to evaluate for each element
            body: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
            /// Statement context for for-statements (null for for-expressions)
            stmt_context: ?StatementContext,

            pub const StatementContext = struct {
                /// Remaining statements after the for loop
                remaining_stmts: []const can.CIR.Statement.Idx,
                /// Final expression to evaluate after all statements
                final_expr: can.CIR.Expr.Idx,
            };
        };

        /// For loop/expression - cleanup after body evaluation
        pub const ForBodyDone = struct {
            /// The list value being iterated
            list_value: StackValue,
            /// Current iteration index (just completed)
            current_index: usize,
            /// Total number of elements in the list
            list_len: usize,
            /// Element size in bytes
            elem_size: usize,
            /// Element layout
            elem_layout: layout.Layout,
            /// Pattern to bind each element to
            pattern: can.CIR.Pattern.Idx,
            /// Pattern runtime type variable
            patt_rt_var: types.Var,
            /// Body expression to evaluate for each element
            body: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
            /// Bindings length at iteration start (for per-iteration cleanup)
            loop_bindings_start: usize,
            /// Statement context for for-statements (null for for-expressions)
            stmt_context: ?ForIterate.StatementContext,
        };

        /// While loop - check condition
        pub const WhileLoopCheck = struct {
            /// Condition expression
            cond: can.CIR.Expr.Idx,
            /// Body expression
            body: can.CIR.Expr.Idx,
            /// Remaining statements after the while loop
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// While loop - cleanup after body evaluation
        pub const WhileLoopBodyDone = struct {
            /// Condition expression
            cond: can.CIR.Expr.Idx,
            /// Body expression
            body: can.CIR.Expr.Idx,
            /// Remaining statements after the while loop
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// Expect statement - check condition
        pub const ExpectCheckStmt = struct {
            /// The expression being checked (for error reporting)
            body_expr: can.CIR.Expr.Idx,
            /// Remaining statements after expect
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// Reassign statement - update binding
        pub const ReassignValue = struct {
            /// The pattern to reassign
            pattern_idx: can.CIR.Pattern.Idx,
            /// Remaining statements after reassign
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// Dbg statement - print value
        pub const DbgPrintStmt = struct {
            /// Runtime type for rendering
            rt_var: types.Var,
            /// Remaining statements after dbg
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// Inspect statement - render value to Str (value is discarded unless final)
        pub const InspectRenderStmt = struct {
            /// Runtime type for rendering
            rt_var: types.Var,
            /// Remaining statements after inspect
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// Cleanup after to_inspect method call in statement context
        /// Discards the result and continues with remaining statements
        pub const InspectStmtCleanup = struct {
            /// Remaining statements after inspect
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };
    };

    /// Work stack for the stack-safe interpreter.
    /// Contains pending operations (eval expressions or apply continuations).
    pub const WorkStack = struct {
        items: std.array_list.AlignedManaged(WorkItem, null),

        pub fn init(allocator: std.mem.Allocator) !WorkStack {
            return .{ .items = try std.array_list.AlignedManaged(WorkItem, null).initCapacity(allocator, 64) };
        }

        pub fn deinit(self: *WorkStack) void {
            self.items.deinit();
        }

        pub fn push(self: *WorkStack, item: WorkItem) !void {
            try self.items.append(item);
        }

        pub fn pop(self: *WorkStack) ?WorkItem {
            return self.items.pop();
        }

        /// Push multiple items in reverse order so they execute in forward order.
        /// For example, if you push [A, B, C], they will be executed as A, B, C.
        pub fn pushMultipleReverse(self: *WorkStack, items: []const WorkItem) !void {
            var i = items.len;
            while (i > 0) {
                i -= 1;
                try self.items.append(items[i]);
            }
        }
    };

    /// Value stack for the stack-safe interpreter.
    /// Contains intermediate results from evaluated expressions.
    pub const ValueStack = struct {
        items: std.array_list.AlignedManaged(StackValue, null),

        pub fn init(allocator: std.mem.Allocator) !ValueStack {
            return .{ .items = try std.array_list.AlignedManaged(StackValue, null).initCapacity(allocator, 64) };
        }

        pub fn deinit(self: *ValueStack) void {
            self.items.deinit();
        }

        pub fn push(self: *ValueStack, value: StackValue) !void {
            try self.items.append(value);
        }

        pub fn pop(self: *ValueStack) ?StackValue {
            return self.items.pop();
        }

        /// Peek at the top value without removing it.
        pub fn peek(self: *const ValueStack) ?StackValue {
            if (self.items.items.len == 0) return null;
            return self.items.items[self.items.items.len - 1];
        }
    };

    /// Stack-safe evaluation entry point.
    /// This function evaluates expressions using explicit work and value stacks
    /// instead of recursive calls, preventing stack overflow on deeply nested programs.
    pub fn evalWithExpectedType(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        roc_ops: *RocOps,
        expected_rt_var: ?types.Var,
    ) Error!StackValue {
        var work_stack = try WorkStack.init(self.allocator);
        defer work_stack.deinit();

        // On error, clean up any pending allocations in continuations
        errdefer self.cleanupPendingWorkStack(&work_stack, roc_ops);

        var value_stack = try ValueStack.init(self.allocator);
        defer value_stack.deinit();

        // Initial work: evaluate the root expression, then return result
        // Push in reverse order: return_result first (will be executed last),
        // then eval_expr (will be executed first)
        try work_stack.push(.{ .apply_continuation = .{ .return_result = {} } });
        try work_stack.push(.{ .eval_expr = .{
            .expr_idx = expr_idx,
            .expected_rt_var = expected_rt_var,
        } });

        while (work_stack.pop()) |work_item| {
            switch (work_item) {
                .eval_expr => |eval_item| {
                    try self.scheduleExprEval(&work_stack, &value_stack, eval_item.expr_idx, eval_item.expected_rt_var, roc_ops);
                },
                .apply_continuation => |cont| {
                    const should_continue = try self.applyContinuation(&work_stack, &value_stack, cont, roc_ops);
                    if (!should_continue) {
                        // return_result continuation signals completion
                        if (value_stack.pop()) |val| {
                            return val;
                        } else {
                            self.triggerCrash("eval: value_stack empty after return_result", false, roc_ops);
                            return error.Crash;
                        }
                    }
                },
            }
        }

        // Should never reach here - return_result should have exited the loop
        self.triggerCrash("eval: should never reach here - return_result should have exited the loop", false, roc_ops);
        return error.Crash;
    }

    /// Find a re-evaluable numeric expression that a variable or expression ultimately points to.
    /// This follows lookup chains to find numeric literals or numeric operations (like binop),
    /// enabling polymorphic re-evaluation for cases like `sum = 5 + 10; I64.to_str(sum)`.
    fn findRootNumericLiteral(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        source_env: *const can.ModuleEnv,
    ) ?can.CIR.Expr.Idx {
        const expr = source_env.store.getExpr(expr_idx);

        // If this is a numeric literal or numeric operation, return it
        switch (expr) {
            .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small => return expr_idx,
            .e_binop => |binop| {
                // Binary operations on numbers can be re-evaluated with expected type
                // Only return binop if it's a numeric operation (not boolean and/or)
                switch (binop.op) {
                    .add, .sub, .mul, .div, .div_trunc, .rem => return expr_idx,
                    else => return null,
                }
            },
            .e_lookup_local => |lookup| {
                // Follow the lookup to see what it points to
                // Search bindings from most recent to oldest
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    if (b.pattern_idx == lookup.pattern_idx) {
                        // Found the binding - recursively check what it points to
                        if (b.expr_idx) |binding_expr_idx| {
                            return self.findRootNumericLiteral(binding_expr_idx, b.source_env);
                        }
                        return null;
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Set up flex_type_context entries for flex vars in a numeric expression.
    /// This enables re-evaluation with a specific expected type by ensuring
    /// translateTypeVar returns the expected type for any flex vars.
    fn setupFlexContextForNumericExpr(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        source_env: *const can.ModuleEnv,
        target_rt_var: types.Var,
    ) Error!void {
        const expr = source_env.store.getExpr(expr_idx);
        switch (expr) {
            .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small => {
                // For numeric literals, map the expression's type var to target
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const resolved = source_env.types.resolveVar(ct_var);
                if (resolved.desc.content == .flex or resolved.desc.content == .rigid) {
                    const key = ModuleVarKey{ .module = @constCast(source_env), .var_ = resolved.var_ };
                    try self.flex_type_context.put(key, target_rt_var);
                }
            },
            .e_binop => |binop| {
                // For binops, recursively set up context for operands
                try self.setupFlexContextForNumericExpr(binop.lhs, source_env, target_rt_var);
                try self.setupFlexContextForNumericExpr(binop.rhs, source_env, target_rt_var);
            },
            .e_lookup_local => |lookup| {
                // Also map the lookup expression's type var itself
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const resolved = source_env.types.resolveVar(ct_var);
                if (resolved.desc.content == .flex or resolved.desc.content == .rigid) {
                    const key = ModuleVarKey{ .module = @constCast(source_env), .var_ = resolved.var_ };
                    try self.flex_type_context.put(key, target_rt_var);
                }
                // For lookups, find the binding and recursively set up context
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    if (b.pattern_idx == lookup.pattern_idx) {
                        if (b.expr_idx) |binding_expr_idx| {
                            try self.setupFlexContextForNumericExpr(binding_expr_idx, b.source_env, target_rt_var);
                        }
                        return;
                    }
                }
            },
            else => {},
        }
    }

    /// Clean up any pending allocations in the work stack when an error occurs.
    /// This prevents memory leaks when evaluation fails partway through.
    fn cleanupPendingWorkStack(self: *Interpreter, work_stack: *WorkStack, roc_ops: *RocOps) void {
        while (work_stack.pop()) |work_item| {
            switch (work_item) {
                .apply_continuation => |cont| {
                    switch (cont) {
                        .call_invoke_closure => |ci| {
                            if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                            if (ci.saved_rigid_subst) |saved| {
                                var saved_copy = saved;
                                saved_copy.deinit();
                            }
                        },
                        .call_cleanup => |cc| {
                            if (cc.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                            if (cc.saved_rigid_subst) |saved| {
                                var saved_copy = saved;
                                saved_copy.deinit();
                            }
                            if (cc.saved_flex_type_context) |saved| {
                                var saved_copy = saved;
                                saved_copy.deinit();
                            }
                        },
                        .for_iterate => |fl| {
                            // Decref the list value
                            fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                        },
                        .for_body_done => |fl| {
                            // Decref the list value
                            fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                        },
                        .sort_compare_result => |sc| {
                            // Decref the list and compare function
                            sc.list_value.decref(&self.runtime_layout_store, roc_ops);
                            sc.compare_fn.decref(&self.runtime_layout_store, roc_ops);
                            if (sc.saved_rigid_subst) |saved| {
                                var saved_copy = saved;
                                saved_copy.deinit();
                            }
                        },
                        else => {},
                    }
                },
                .eval_expr => {},
            }
        }
    }

    /// Schedule evaluation of an expression by examining it and pushing appropriate work items.
    /// Instead of recursing, this pushes work items onto the stack to be processed by the main loop.
    fn scheduleExprEval(
        self: *Interpreter,
        work_stack: *WorkStack,
        value_stack: *ValueStack,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) Error!void {
        const expr = self.env.store.getExpr(expr_idx);

        switch (expr) {
            // ================================================================
            // Immediate values - no sub-expressions to evaluate
            // ================================================================

            .e_num => |num_lit| {
                const value = try self.evalNum(expr_idx, expected_rt_var, num_lit);
                try value_stack.push(value);
            },

            .e_frac_f32 => |lit| {
                const value = try self.evalFracF32(expr_idx, expected_rt_var, lit);
                try value_stack.push(value);
            },

            .e_frac_f64 => |lit| {
                const value = try self.evalFracF64(expr_idx, expected_rt_var, lit);
                try value_stack.push(value);
            },

            .e_dec => |dec_lit| {
                const value = try self.evalDec(expr_idx, expected_rt_var, dec_lit);
                try value_stack.push(value);
            },

            .e_dec_small => |small| {
                const value = try self.evalDecSmall(expr_idx, expected_rt_var, small);
                try value_stack.push(value);
            },

            .e_str_segment => |seg| {
                const value = try self.evalStrSegment(seg, roc_ops);
                try value_stack.push(value);
            },

            .e_str => |str_expr| {
                const segments = self.env.store.sliceExpr(str_expr.span);
                if (segments.len == 0) {
                    // Empty string - return immediately
                    const str_rt_var = try self.getCanonicalStrRuntimeVar();
                    const value = try self.pushStr(str_rt_var);
                    const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
                    roc_str.* = RocStr.empty();
                    try value_stack.push(value);
                } else {
                    // Schedule collection of segments
                    // Push continuation to handle all segments, starting with none collected
                    try work_stack.push(.{
                        .apply_continuation = .{
                            .str_collect = .{
                                .collected_count = 0,
                                .total_count = segments.len,
                                .remaining_segments = segments,
                                .needs_conversion = false, // No value to convert yet
                            },
                        },
                    });
                }
            },

            .e_empty_record => {
                const value = try self.evalEmptyRecord(expr_idx, expected_rt_var);
                try value_stack.push(value);
            },

            .e_empty_list => {
                const value = try self.evalEmptyList(expr_idx, expected_rt_var);
                try value_stack.push(value);
            },

            .e_zero_argument_tag => |zero| {
                const value = try self.evalZeroArgumentTag(expr_idx, expected_rt_var, zero, roc_ops);
                try value_stack.push(value);
            },

            // ================================================================
            // Lambda/Closure creation
            // ================================================================

            .e_lambda => |lam| {
                const value = try self.evalLambda(expr_idx, expected_rt_var, lam, roc_ops);
                try value_stack.push(value);
            },

            .e_low_level_lambda => |lam| {
                const value = try self.evalLowLevelLambda(expr_idx, expected_rt_var, lam);
                try value_stack.push(value);
            },

            .e_hosted_lambda => |hosted| {
                const value = try self.evalHostedLambda(expr_idx, hosted);
                try value_stack.push(value);
            },

            .e_closure => |cls| {
                const value = try self.evalClosure(expr_idx, cls, roc_ops);
                try value_stack.push(value);
            },

            // ================================================================
            // Variable lookups
            // ================================================================

            .e_lookup_local => |lookup| {
                const value = try self.evalLookupLocal(lookup, expected_rt_var, roc_ops);
                try value_stack.push(value);
            },

            .e_lookup_external => |lookup| {
                const value = try self.evalLookupExternal(lookup, expected_rt_var, roc_ops);
                try value_stack.push(value);
            },

            .e_lookup_required => |lookup| {
                // Required lookups reference values from the app that provides values to the
                // platform's `requires` clause.
                if (self.app_env) |app_env| {
                    // Get the required type info from the platform's requires_types
                    const requires_items = self.env.requires_types.items.items;
                    const requires_idx_val = @intFromEnum(lookup.requires_idx);
                    if (requires_idx_val >= requires_items.len) {
                        return error.TypeMismatch;
                    }
                    const required_type = requires_items[requires_idx_val];
                    // Translate the required ident from platform's store to app's store (once, outside loop)
                    const required_ident_str = self.env.getIdent(required_type.ident);
                    const app_required_ident = try @constCast(app_env).insertIdent(base_pkg.Ident.for_text(required_ident_str));

                    // Find the matching export in the app
                    const exports = app_env.store.sliceDefs(app_env.exports);
                    var found_expr: ?can.CIR.Expr.Idx = null;
                    for (exports) |def_idx| {
                        const def = app_env.store.getDef(def_idx);
                        // Get the def's identifier from its pattern
                        const pattern = app_env.store.getPattern(def.pattern);
                        if (pattern == .assign) {
                            // Compare ident indices directly (O(1) instead of string comparison)
                            if (pattern.assign.ident == app_required_ident) {
                                found_expr = def.expr;
                                break;
                            }
                        }
                    }

                    if (found_expr) |app_expr_idx| {
                        // Switch to app env for evaluation (like evalLookupExternal)
                        const saved_env = self.env;
                        const saved_bindings_len = self.bindings.items.len;
                        self.env = @constCast(app_env);
                        defer {
                            self.env = saved_env;
                            self.bindings.shrinkRetainingCapacity(saved_bindings_len);
                        }

                        // Evaluate the app's exported expression synchronously
                        const result = try self.evalWithExpectedType(app_expr_idx, roc_ops, expected_rt_var);
                        try value_stack.push(result);
                    } else {
                        return error.TypeMismatch;
                    }
                } else {
                    // No app_env - can't resolve required lookups
                    return error.TypeMismatch;
                }
            },

            .e_runtime_error => {
                self.triggerCrash("runtime error", false, roc_ops);
                return error.Crash;
            },

            // ================================================================
            // Binary operations
            // ================================================================

            .e_binop => |binop| {
                switch (binop.op) {
                    .@"and" => {
                        // Short-circuit AND: evaluate LHS first, then check
                        // Push continuation first (will be executed after LHS)
                        try work_stack.push(.{ .apply_continuation = .{ .and_short_circuit = .{
                            .rhs_expr = binop.rhs,
                        } } });
                        // Push LHS evaluation (will be executed first)
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = binop.lhs,
                            .expected_rt_var = null,
                        } });
                    },
                    .@"or" => {
                        // Short-circuit OR: evaluate LHS first, then check
                        // Push continuation first (will be executed after LHS)
                        try work_stack.push(.{ .apply_continuation = .{ .or_short_circuit = .{
                            .rhs_expr = binop.rhs,
                        } } });
                        // Push LHS evaluation (will be executed first)
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = binop.lhs,
                            .expected_rt_var = null,
                        } });
                    },
                    else => {
                        // Arithmetic and comparison operations: desugar to method calls
                        const method_ident: base_pkg.Ident.Idx = switch (binop.op) {
                            .add => self.root_env.idents.plus,
                            .sub => self.root_env.idents.minus,
                            .mul => self.root_env.idents.times,
                            .div => self.root_env.idents.div_by,
                            .div_trunc => self.root_env.idents.div_trunc_by,
                            .rem => self.root_env.idents.rem_by,
                            .lt => self.root_env.idents.is_lt,
                            .le => self.root_env.idents.is_lte,
                            .gt => self.root_env.idents.is_gt,
                            .ge => self.root_env.idents.is_gte,
                            .eq, .ne => self.root_env.idents.is_eq,
                            .@"and", .@"or" => unreachable, // handled above
                        };

                        // Get LHS and RHS type info
                        // Note: Both operands should be unified to the same type by the type checker
                        const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                        const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                        const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                        const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

                        // Ensure both operands have the same numeric type.
                        // Strategy:
                        // - If one operand is concrete (not flex/rigid), unify the other with it
                        // - If both are unresolved (flex/rigid), default both to Dec
                        const lhs_resolved = self.runtime_types.resolveVar(lhs_rt_var);
                        const rhs_resolved = self.runtime_types.resolveVar(rhs_rt_var);
                        const lhs_is_flex = lhs_resolved.desc.content == .flex or lhs_resolved.desc.content == .rigid;
                        const rhs_is_flex = rhs_resolved.desc.content == .flex or rhs_resolved.desc.content == .rigid;

                        if (lhs_is_flex and rhs_is_flex) {
                            // Both unresolved - for arithmetic ops, use expected type if available and concrete,
                            // otherwise default to Dec. For comparison ops, always default to Dec since
                            // expected_rt_var would be Bool (the result type), not the operand type.
                            const is_arithmetic = switch (binop.op) {
                                .add, .sub, .mul, .div, .div_trunc, .rem => true,
                                else => false,
                            };
                            const target_var = blk: {
                                if (is_arithmetic) {
                                    if (expected_rt_var) |exp_var| {
                                        const exp_resolved = self.runtime_types.resolveVar(exp_var);
                                        const exp_is_concrete = exp_resolved.desc.content != .flex and exp_resolved.desc.content != .rigid;
                                        if (exp_is_concrete) {
                                            break :blk exp_var;
                                        }
                                    }
                                }
                                // No expected type, expected is flex, or comparison op - default to Dec
                                const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                                break :blk try self.runtime_types.freshFromContent(dec_content);
                            };
                            const dec_var = target_var;
                            _ = try unify.unify(
                                self.env,
                                self.runtime_types,
                                &self.problems,
                                &self.snapshots,
                                &self.type_writer,
                                &self.unify_scratch,
                                &self.unify_scratch.occurs_scratch,
                                lhs_rt_var,
                                dec_var,
                            );
                            _ = try unify.unify(
                                self.env,
                                self.runtime_types,
                                &self.problems,
                                &self.snapshots,
                                &self.type_writer,
                                &self.unify_scratch,
                                &self.unify_scratch.occurs_scratch,
                                rhs_rt_var,
                                dec_var,
                            );
                        } else if (lhs_is_flex and !rhs_is_flex) {
                            // LHS is flex, RHS is concrete - unify LHS with RHS
                            _ = try unify.unify(
                                self.env,
                                self.runtime_types,
                                &self.problems,
                                &self.snapshots,
                                &self.type_writer,
                                &self.unify_scratch,
                                &self.unify_scratch.occurs_scratch,
                                lhs_rt_var,
                                rhs_rt_var,
                            );
                        } else if (!lhs_is_flex and rhs_is_flex) {
                            // RHS is flex, LHS is concrete - unify RHS with LHS
                            _ = try unify.unify(
                                self.env,
                                self.runtime_types,
                                &self.problems,
                                &self.snapshots,
                                &self.type_writer,
                                &self.unify_scratch,
                                &self.unify_scratch.occurs_scratch,
                                rhs_rt_var,
                                lhs_rt_var,
                            );
                        }
                        // If both are concrete, they should already match (type checker ensures this)

                        // For != we need to negate the result of is_eq
                        const negate_result = binop.op == .ne;

                        // Schedule: first evaluate LHS, then evaluate RHS, then apply method
                        try work_stack.push(.{ .apply_continuation = .{ .binop_eval_rhs = .{
                            .rhs_expr = binop.rhs,
                            .method_ident = method_ident,
                            .lhs_rt_var = lhs_rt_var,
                            .rhs_rt_var = rhs_rt_var,
                            .negate_result = negate_result,
                        } } });
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = binop.lhs,
                            .expected_rt_var = lhs_rt_var,
                        } });
                    },
                }
            },

            // ================================================================
            // Conditionals
            // ================================================================

            .e_if => |if_expr| {
                const branches = self.env.store.sliceIfBranches(if_expr.branches);
                if (branches.len > 0) {
                    // Get first branch
                    const first_branch = self.env.store.getIfBranch(branches[0]);
                    // Push if_branch continuation (to be executed after condition evaluation)
                    try work_stack.push(.{ .apply_continuation = .{ .if_branch = .{
                        .body = first_branch.body,
                        .remaining_branches = branches[1..],
                        .final_else = if_expr.final_else,
                        .expected_rt_var = expected_rt_var,
                    } } });
                    // Push condition evaluation (to be executed first)
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = first_branch.cond,
                        .expected_rt_var = null,
                    } });
                } else {
                    // No branches, just evaluate final else
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = if_expr.final_else,
                        .expected_rt_var = expected_rt_var,
                    } });
                }
            },

            // ================================================================
            // Blocks
            // ================================================================

            .e_block => |blk| {
                const stmts = self.env.store.sliceStatements(blk.stmts);
                const bindings_start = self.bindings.items.len;

                // First pass: add placeholders for all decl/var lambdas/closures (mutual recursion support)
                try self.addClosurePlaceholders(stmts, bindings_start);

                if (stmts.len == 0) {
                    // No statements, just evaluate final expression
                    // Push trim_bindings to clean up after evaluation
                    try work_stack.push(.{ .apply_continuation = .{ .trim_bindings = .{
                        .target_len = bindings_start,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = blk.final_expr,
                        .expected_rt_var = expected_rt_var,
                    } });
                } else {
                    // Schedule processing of statements
                    // Push trim_bindings first (executed last)
                    try work_stack.push(.{ .apply_continuation = .{ .trim_bindings = .{
                        .target_len = bindings_start,
                    } } });
                    // Push block_continue to process statements
                    try work_stack.push(.{ .apply_continuation = .{ .block_continue = .{
                        .remaining_stmts = stmts,
                        .final_expr = blk.final_expr,
                        .bindings_start = bindings_start,
                        .expected_rt_var = expected_rt_var,
                    } } });
                }
            },

            // ================================================================
            // Tuples
            // ================================================================

            .e_tuple => |tup| {
                const elems = self.env.store.sliceExpr(tup.elems);
                if (elems.len == 0) {
                    // Empty tuple - create immediately
                    // Compute tuple layout with no elements
                    const tuple_layout_idx = try self.runtime_layout_store.putTuple(&[0]Layout{});
                    const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                    const tuple_rt_var = expected_rt_var orelse blk: {
                        const ct_var = can.ModuleEnv.varFrom(expr_idx);
                        break :blk try self.translateTypeVar(self.env, ct_var);
                    };
                    const value = try self.pushRaw(tuple_layout, 0, tuple_rt_var);
                    try value_stack.push(value);
                } else {
                    // Schedule collection of elements
                    // Push tuple_collect continuation (to be executed after first element)
                    try work_stack.push(.{ .apply_continuation = .{ .tuple_collect = .{
                        .collected_count = 0,
                        .remaining_elems = elems,
                    } } });
                }
            },

            // ================================================================
            // Lists
            // ================================================================

            .e_list => |list_expr| {
                const elems = self.env.store.sliceExpr(list_expr.elems);

                // Get list type variable
                const list_rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };

                if (elems.len == 0) {
                    // Empty list - create immediately
                    const list_layout = try self.getRuntimeLayout(list_rt_var);
                    const dest = try self.pushRaw(list_layout, 0, list_rt_var);
                    if (dest.ptr != null) {
                        const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));
                        header.* = RocList.empty();
                    }
                    try value_stack.push(dest);
                } else {
                    // Get element type variable from first element
                    const first_elem_var: types.Var = @enumFromInt(@intFromEnum(elems[0]));
                    const elem_rt_var = try self.translateTypeVar(self.env, first_elem_var);

                    // Schedule collection of elements
                    try work_stack.push(.{ .apply_continuation = .{ .list_collect = .{
                        .collected_count = 0,
                        .remaining_elems = elems,
                        .elem_rt_var = elem_rt_var,
                        .list_rt_var = list_rt_var,
                    } } });
                }
            },

            // ================================================================
            // Records
            // ================================================================

            .e_record => |rec| {
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const fields = self.env.store.sliceRecordFields(rec.fields);

                if (rec.ext) |ext_idx| {
                    // Has extension record - schedule extension evaluation first
                    try work_stack.push(.{ .apply_continuation = .{ .record_collect = .{
                        .collected_count = 0,
                        .remaining_fields = fields,
                        .rt_var = rt_var,
                        .expr_idx = expr_idx,
                        .has_extension = true,
                        .all_fields = fields,
                    } } });
                    // Evaluate extension first - it will be the first value on stack
                    const ext_ct_var = can.ModuleEnv.varFrom(ext_idx);
                    const ext_rt_var = try self.translateTypeVar(self.env, ext_ct_var);
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ext_idx,
                        .expected_rt_var = ext_rt_var,
                    } });
                } else if (fields.len == 0) {
                    // Empty record with no extension - create immediately
                    const rec_layout = try self.getRuntimeLayout(rt_var);
                    const dest = try self.pushRaw(rec_layout, 0, rt_var);
                    try value_stack.push(dest);
                } else {
                    // Non-empty record without extension
                    try work_stack.push(.{ .apply_continuation = .{ .record_collect = .{
                        .collected_count = 0,
                        .remaining_fields = fields,
                        .rt_var = rt_var,
                        .expr_idx = expr_idx,
                        .has_extension = false,
                        .all_fields = fields,
                    } } });
                }
            },

            // ================================================================
            // Nominal types - evaluate backing expression
            // ================================================================

            .e_nominal => |nom| {
                // Compute the backing type variable for the nominal.
                // Use expected_rt_var if available - this carries the correctly instantiated type
                // from the call site (with concrete type args), avoiding re-translation from
                // the builtins module which would have rigid type args.
                const backing_rt_var = if (nom.nominal_type_decl == self.builtins.bool_stmt)
                    try self.getCanonicalBoolRuntimeVar()
                else if (expected_rt_var) |expected| blk: {
                    // Use the expected type's backing - but we need to set up rigid substitution
                    // because the backing may still have rigids that need to map to concrete type args
                    const expected_resolved = self.runtime_types.resolveVar(expected);
                    switch (expected_resolved.desc.content) {
                        .structure => |st| switch (st) {
                            .nominal_type => |nt| {
                                const backing = self.runtime_types.getNominalBackingVar(nt);
                                const rt_type_args = self.runtime_types.sliceNominalArgs(nt);

                                // Set up rigid_subst: map rigids in backing to concrete type args
                                if (rt_type_args.len > 0) {
                                    // Collect rigids from the backing type
                                    var rigids: std.ArrayListUnmanaged(types.Var) = .empty;
                                    defer rigids.deinit(self.allocator);
                                    var visited = std.AutoHashMap(types.Var, void).init(self.allocator);
                                    defer visited.deinit();
                                    try self.collectRigidsFromRuntimeType(self.allocator, backing, &rigids, &visited);

                                    // Sort by var ID for positional correspondence
                                    std.mem.sort(types.Var, rigids.items, {}, struct {
                                        fn lessThan(_: void, a: types.Var, b: types.Var) bool {
                                            return @intFromEnum(a) < @intFromEnum(b);
                                        }
                                    }.lessThan);

                                    // Add mappings to empty_scope so layout store finds them via TypeScope.lookup()
                                    try self.addRigidMappingsToScope(rigids.items, rt_type_args);

                                    // Also add to rigid_subst for backwards compatibility
                                    const num_mappings = @min(rigids.items.len, rt_type_args.len);
                                    for (0..num_mappings) |i| {
                                        const arg_resolved = self.runtime_types.resolveVar(rt_type_args[i]);
                                        // If the type arg is itself a rigid, look it up in rigid_subst
                                        // to get the concrete type from an outer context
                                        const concrete_type = switch (arg_resolved.desc.content) {
                                            .rigid => if (self.rigid_subst.get(arg_resolved.var_)) |outer_concrete|
                                                outer_concrete
                                            else
                                                rt_type_args[i],
                                            else => rt_type_args[i],
                                        };
                                        try self.rigid_subst.put(rigids.items[i], concrete_type);
                                    }
                                }
                                break :blk backing;
                            },
                            else => break :blk expected,
                        },
                        else => break :blk expected,
                    }
                } else blk: {
                    // Fall back to translating from current env
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                    const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                    break :blk switch (nominal_resolved.desc.content) {
                        .structure => |st| switch (st) {
                            .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                            else => nominal_rt_var,
                        },
                        else => nominal_rt_var,
                    };
                };
                // Schedule evaluation of the backing expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = nom.backing_expr,
                    .expected_rt_var = backing_rt_var,
                } });
            },

            .e_nominal_external => |nom| {
                // Compute the backing type variable for the external nominal
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
                // Schedule evaluation of the backing expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = nom.backing_expr,
                    .expected_rt_var = rt_var,
                } });
            },

            // ================================================================
            // Simple error/crash expressions
            // ================================================================

            .e_crash => |crash_expr| {
                // Get the crash message string and trigger crash
                const msg = self.env.getString(crash_expr.msg);
                self.triggerCrash(msg, false, roc_ops);
                return error.Crash;
            },

            .e_anno_only => {
                self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
                return error.Crash;
            },

            .e_ellipsis => {
                self.triggerCrash("This expression uses `...` as a placeholder. Implementation is required.", false, roc_ops);
                return error.Crash;
            },

            .e_return => |ret| {
                // Schedule the early return continuation after evaluating the inner expression
                const inner_ct_var = can.ModuleEnv.varFrom(ret.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                try work_stack.push(.{ .apply_continuation = .{ .early_return = .{} } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = ret.expr,
                    .expected_rt_var = inner_rt_var,
                } });
            },

            // ================================================================
            // Tag unions with payloads
            // ================================================================

            .e_tag => |tag| {
                // Determine runtime type and tag index.
                // Use expected_rt_var if it's resolved to something concrete (structure or alias).
                // If expected_rt_var is flex (unresolved), fall back to ct_var translation.
                // This handles the case where the app's main! return type hasn't been fully
                // unified with the platform's expected type - the expected_rt_var may be
                // passed but still be flex, while ct_var correctly resolves to the concrete type.
                var rt_var = blk: {
                    if (expected_rt_var) |expected| {
                        const expected_resolved = self.runtime_types.resolveVar(expected);
                        // Use expected only if it's concrete (not flex)
                        if (expected_resolved.desc.content == .structure or
                            expected_resolved.desc.content == .alias)
                        {
                            break :blk expected;
                        }
                    }
                    // Fall back to translating from compile-time type
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                var resolved = self.resolveBaseVar(rt_var);
                // Handle flex types for True/False
                // Note: We also need to handle non-flex Bool types that might come from
                // type inference (e.g., in `if True then ...` the condition has Bool type)
                const is_bool_tag = tag.name == self.env.idents.true_tag or tag.name == self.env.idents.false_tag;
                if (is_bool_tag) {
                    // Always use canonical Bool for True/False to ensure consistent layout
                    rt_var = try self.getCanonicalBoolRuntimeVar();
                    resolved = self.resolveBaseVar(rt_var);
                }
                // Unwrap nominal types (like Try) to get to the underlying tag_union
                if (resolved.desc.content == .structure and resolved.desc.content.structure == .nominal_type) {
                    const nom = resolved.desc.content.structure.nominal_type;
                    const backing = self.runtime_types.getNominalBackingVar(nom);
                    resolved = self.runtime_types.resolveVar(backing);
                }
                // Also handle aliases that wrap tag unions
                if (resolved.desc.content == .alias) {
                    const backing = self.runtime_types.getAliasBackingVar(resolved.desc.content.alias);
                    resolved = self.runtime_types.resolveVar(backing);
                }
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) {
                    const content_tag = @tagName(resolved.desc.content);
                    const struct_tag = if (resolved.desc.content == .structure) @tagName(resolved.desc.content.structure) else "n/a";
                    const tag_name_str = self.env.getIdent(tag.name);
                    // Also show what the ct_var resolves to for debugging
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const ct_resolved = self.env.types.resolveVar(ct_var);
                    const ct_content_tag = @tagName(ct_resolved.desc.content);
                    const has_expected = expected_rt_var != null;
                    const msg = std.fmt.allocPrint(self.allocator, "e_tag: expected tag_union but got rt={s}:{s} ct={s} has_expected={} for tag `{s}`", .{ content_tag, struct_tag, ct_content_tag, has_expected, tag_name_str }) catch "e_tag: expected tag_union structure type";
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                }

                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(rt_var, &tag_list);

                // Find tag in the type's tag list
                const tag_index_opt = try self.findTagIndexByIdentInList(self.env, tag.name, tag_list.items);

                const tag_index = tag_index_opt orelse {
                    const name_text = self.env.getIdent(tag.name);
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                };
                const layout_val = try self.getRuntimeLayout(rt_var);

                if (layout_val.tag == .scalar) {
                    // No payload union - just set discriminant
                    var out = try self.pushRaw(layout_val, 0, rt_var);
                    if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        try out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        try value_stack.push(out);
                    } else {
                        self.triggerCrash("e_tag: scalar layout is not int", false, roc_ops);
                        return error.Crash;
                    }
                } else if (layout_val.tag == .record or layout_val.tag == .tuple or layout_val.tag == .tag_union) {
                    const args_exprs = self.env.store.sliceExpr(tag.args);
                    const arg_vars_range = tag_list.items[tag_index].args;
                    const arg_rt_vars = self.runtime_types.sliceVars(arg_vars_range);

                    if (args_exprs.len == 0) {
                        // No payload args - finalize immediately
                        const value = try self.finalizeTagNoPayload(rt_var, tag_index, layout_val, roc_ops);
                        try value_stack.push(value);
                    } else {
                        // Has payload args - schedule collection
                        // layout_type: 0=record, 1=tuple, 2=tag_union
                        const layout_type: u8 = if (layout_val.tag == .record) 0 else if (layout_val.tag == .tuple) 1 else 2;
                        try work_stack.push(.{ .apply_continuation = .{ .tag_collect = .{
                            .collected_count = 0,
                            .remaining_args = args_exprs,
                            .arg_rt_vars = arg_rt_vars,
                            .expr_idx = expr_idx,
                            .rt_var = rt_var,
                            .tag_index = tag_index,
                            .layout_type = layout_type,
                        } } });
                    }
                } else {
                    self.triggerCrash("e_tag: unexpected layout type", false, roc_ops);
                    return error.Crash;
                }
            },

            // ================================================================
            // Pattern matching
            // ================================================================

            .e_match => |m| {
                // Get type info for scrutinee and result
                const scrutinee_ct_var = can.ModuleEnv.varFrom(m.cond);
                const scrutinee_rt_var = try self.translateTypeVar(self.env, scrutinee_ct_var);
                const match_result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const match_result_rt_var = try self.translateTypeVar(self.env, match_result_ct_var);

                const branches = self.env.store.matchBranchSlice(m.branches);

                // Schedule: first evaluate scrutinee, then try branches
                try work_stack.push(.{ .apply_continuation = .{ .match_branches = .{
                    .expr_idx = expr_idx,
                    .scrutinee_rt_var = scrutinee_rt_var,
                    .result_rt_var = match_result_rt_var,
                    .branches = branches,
                    .current_branch = 0,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = m.cond,
                    .expected_rt_var = null,
                } });
            },

            // ================================================================
            // Debugging and assertions
            // ================================================================

            .e_expect => |expect_expr| {
                const bool_rt_var = try self.getCanonicalBoolRuntimeVar();
                // Schedule: first evaluate condition, then check result
                try work_stack.push(.{ .apply_continuation = .{ .expect_check = .{
                    .expr_idx = expr_idx,
                    .body_expr = expect_expr.body,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = expect_expr.body,
                    .expected_rt_var = bool_rt_var,
                } });
            },

            .e_dbg => |dbg_expr| {
                const inner_ct_var = can.ModuleEnv.varFrom(dbg_expr.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                // Schedule: first evaluate inner expression, then print
                try work_stack.push(.{ .apply_continuation = .{ .dbg_print = .{
                    .expr_idx = expr_idx,
                    .inner_rt_var = inner_rt_var,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = dbg_expr.expr,
                    .expected_rt_var = inner_rt_var,
                } });
            },

            .e_inspect => |inspect_expr| {
                const inner_ct_var = can.ModuleEnv.varFrom(inspect_expr.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                // Schedule: first evaluate inner expression, then render to Str
                try work_stack.push(.{ .apply_continuation = .{ .inspect_render = .{
                    .expr_idx = expr_idx,
                    .inner_rt_var = inner_rt_var,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = inspect_expr.expr,
                    .expected_rt_var = inner_rt_var,
                } });
            },

            .e_for => |for_expr| {
                // For expression: first evaluate the list, then set up iteration
                const expr_ct_var = can.ModuleEnv.varFrom(for_expr.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);

                // Get the element type for binding
                const patt_ct_var = can.ModuleEnv.varFrom(for_expr.patt);
                const patt_rt_var = try self.translateTypeVar(self.env, patt_ct_var);

                // Push for_iterate continuation (will be executed after list is evaluated)
                // stmt_context is null for for-expressions
                try work_stack.push(.{
                    .apply_continuation = .{
                        .for_iterate = .{
                            .list_value = undefined, // Will be set when list is evaluated
                            .current_index = 0,
                            .list_len = 0, // Will be set when list is evaluated
                            .elem_size = 0, // Will be set when list is evaluated
                            .elem_layout = undefined, // Will be set when list is evaluated
                            .pattern = for_expr.patt,
                            .patt_rt_var = patt_rt_var,
                            .body = for_expr.body,
                            .bindings_start = self.bindings.items.len,
                            .stmt_context = null,
                        },
                    },
                });

                // Evaluate the list expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = for_expr.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },

            // ================================================================
            // Function calls
            // ================================================================

            .e_call => |call| {
                const func_idx = call.func;
                const arg_indices = self.env.store.sliceExpr(call.args);

                // Check if the function is an anno-only lookup that will crash
                const func_expr_check = self.env.store.getExpr(func_idx);
                if (func_expr_check == .e_lookup_local) {
                    const lookup = func_expr_check.e_lookup_local;
                    const all_defs = self.env.store.sliceDefs(self.env.all_defs);
                    for (all_defs) |def_idx| {
                        const def = self.env.store.getDef(def_idx);
                        if (def.pattern == lookup.pattern_idx) {
                            const def_expr = self.env.store.getExpr(def.expr);
                            if (def_expr == .e_anno_only) {
                                self.triggerCrash("This function has only a type annotation - no implementation was provided", false, roc_ops);
                                return error.Crash;
                            }
                        }
                    }
                }

                // Check if this is an error expression that shouldn't be called
                if (func_expr_check == .e_runtime_error or func_expr_check == .e_anno_only or func_expr_check == .e_crash) {
                    return error.TypeMismatch;
                }

                // Get function type and potentially instantiate
                const func_ct_var = can.ModuleEnv.varFrom(func_idx);
                const func_rt_var_orig = try self.translateTypeVar(self.env, func_ct_var);

                // Only instantiate if we have an actual function type (not a flex variable)
                const func_rt_orig_resolved = self.runtime_types.resolveVar(func_rt_var_orig);
                const should_instantiate = func_rt_orig_resolved.desc.content == .structure and
                    (func_rt_orig_resolved.desc.content.structure == .fn_pure or
                        func_rt_orig_resolved.desc.content.structure == .fn_effectful or
                        func_rt_orig_resolved.desc.content.structure == .fn_unbound);

                var saved_rigid_subst: ?std.AutoHashMap(types.Var, types.Var) = null;
                if (should_instantiate) {
                    saved_rigid_subst = try self.rigid_subst.clone();
                }
                errdefer {
                    if (saved_rigid_subst) |*saved| saved.deinit();
                }

                var subst_map = std.AutoHashMap(types.Var, types.Var).init(self.allocator);
                defer subst_map.deinit();
                const func_rt_var = if (should_instantiate)
                    try self.instantiateType(func_rt_var_orig, &subst_map)
                else
                    func_rt_var_orig;

                // If we instantiated, update rigid_subst and empty_scope (will be restored in cleanup)
                if (should_instantiate) {
                    // Ensure we have at least one scope level
                    if (self.empty_scope.scopes.items.len == 0) {
                        try self.empty_scope.scopes.append(types.VarMap.init(self.allocator));
                    }
                    const scope = &self.empty_scope.scopes.items[0];

                    var subst_iter = subst_map.iterator();
                    while (subst_iter.next()) |entry| {
                        try self.rigid_subst.put(entry.key_ptr.*, entry.value_ptr.*);
                        // Also add to empty_scope so layout store finds the mapping
                        try scope.put(entry.key_ptr.*, entry.value_ptr.*);
                    }
                    // Clear the layout cache so layouts are recomputed with substitutions
                    @memset(self.var_to_layout_slot.items, 0);
                }

                // Compute argument runtime type variables
                var arg_rt_vars = try self.allocator.alloc(types.Var, arg_indices.len);
                for (arg_indices, 0..) |arg_idx, i| {
                    const arg_ct_var = can.ModuleEnv.varFrom(arg_idx);
                    const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);

                    // Apply substitution if this argument is a rigid variable that was instantiated
                    if (should_instantiate) {
                        const arg_resolved = self.runtime_types.resolveVar(arg_rt_var);
                        if (arg_resolved.desc.content == .rigid) {
                            if (self.rigid_subst.get(arg_resolved.var_)) |substituted_arg| {
                                arg_rt_vars[i] = substituted_arg;
                            } else {
                                arg_rt_vars[i] = arg_rt_var;
                            }
                        } else {
                            arg_rt_vars[i] = arg_rt_var;
                        }
                    } else {
                        arg_rt_vars[i] = arg_rt_var;
                    }
                }

                // Get call expression's return type
                const call_ret_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const call_ret_rt_var = try self.translateTypeVar(self.env, call_ret_ct_var);

                // Prepare polymorphic call entry for unification
                const poly_entry: ?PolyEntry = self.prepareCallWithFuncVar(0, @intCast(@intFromEnum(func_idx)), func_rt_var, arg_rt_vars) catch |err| blk: {
                    if (err == error.TypeMismatch) {
                        break :blk null;
                    }
                    break :blk null;
                };

                // Unify call return type with function's return type
                // Use the function's return var (from instantiated function) instead of
                // call_ret_rt_var (fresh translation) because the function's return var
                // has concrete type args while call_ret_rt_var may have rigid type args.
                const effective_ret_var = if (poly_entry) |entry| blk: {
                    _ = try unify.unifyWithConf(
                        self.env,
                        self.runtime_types,
                        &self.problems,
                        &self.snapshots,
                        &self.type_writer,
                        &self.unify_scratch,
                        &self.unify_scratch.occurs_scratch,
                        call_ret_rt_var,
                        entry.return_var,
                        unify.Conf{ .ctx = .anon, .constraint_origin_var = null },
                    );
                    // Use the function's return type - it has properly instantiated type args
                    break :blk entry.return_var;
                } else call_ret_rt_var;

                // Schedule: first evaluate function, then collect args, then invoke
                // Push invoke continuation (to be executed after all args collected)
                try work_stack.push(.{ .apply_continuation = .{ .call_invoke_closure = .{
                    .arg_count = arg_indices.len,
                    .call_ret_rt_var = effective_ret_var,
                    .did_instantiate = should_instantiate,
                    .saved_rigid_subst = saved_rigid_subst,
                    .arg_rt_vars_to_free = arg_rt_vars,
                } } });
                saved_rigid_subst = null;

                // Push arg collection continuation (to be executed after function is evaluated)
                try work_stack.push(.{ .apply_continuation = .{ .call_collect_args = .{
                    .collected_count = 0,
                    .remaining_args = arg_indices,
                    .arg_rt_vars = arg_rt_vars,
                    .call_ret_rt_var = effective_ret_var,
                    .did_instantiate = should_instantiate,
                } } });

                // Evaluate the function expression first
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = func_idx,
                    .expected_rt_var = func_rt_var,
                } });
            },

            // ================================================================
            // Unary operations
            // ================================================================

            .e_unary_minus => |unary_minus| {
                // Desugar `-a` to `a.negate()`
                const operand_ct_var = can.ModuleEnv.varFrom(unary_minus.expr);
                var operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);

                // Resolve the operand type
                const operand_resolved = self.runtime_types.resolveVar(operand_rt_var);

                // If the type is still a flex/rigid var, default to Dec
                if (operand_resolved.desc.content == .flex or operand_resolved.desc.content == .rigid) {
                    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                    const dec_var = try self.runtime_types.freshFromContent(dec_content);
                    operand_rt_var = dec_var;
                }

                // Schedule: first evaluate operand, then apply method
                try work_stack.push(.{ .apply_continuation = .{ .unary_op_apply = .{
                    .method_ident = self.root_env.idents.negate,
                    .operand_rt_var = operand_rt_var,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = unary_minus.expr,
                    .expected_rt_var = operand_rt_var,
                } });
            },

            .e_unary_not => |unary_not| {
                // Desugar `!a` to `a.not()`
                const operand_ct_var = can.ModuleEnv.varFrom(unary_not.expr);
                var operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);

                // Resolve the operand type
                const operand_resolved = self.runtime_types.resolveVar(operand_rt_var);

                // If the type is still a flex/rigid var, default to Bool (shouldn't happen for bool, but be safe)
                if (operand_resolved.desc.content == .flex or operand_resolved.desc.content == .rigid) {
                    operand_rt_var = try self.getCanonicalBoolRuntimeVar();
                }

                // Schedule: first evaluate operand, then apply method
                try work_stack.push(.{ .apply_continuation = .{ .unary_op_apply = .{
                    .method_ident = self.root_env.idents.not,
                    .operand_rt_var = operand_rt_var,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = unary_not.expr,
                    .expected_rt_var = operand_rt_var,
                } });
            },

            // ================================================================
            // Dot access (field access and method calls)
            // ================================================================

            .e_dot_access => |dot_access| {
                const receiver_ct_var = can.ModuleEnv.varFrom(dot_access.receiver);
                var receiver_rt_var = try self.translateTypeVar(self.env, receiver_ct_var);

                // If the receiver type is a flex/rigid var, default to Dec
                // (Unsuffixed numeric literals default to Dec in Roc)
                const receiver_resolved = self.runtime_types.resolveVar(receiver_rt_var);
                if (receiver_resolved.desc.content == .flex or receiver_resolved.desc.content == .rigid) {
                    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                    const dec_var = try self.runtime_types.freshFromContent(dec_content);
                    receiver_rt_var = dec_var;
                }

                // Schedule: first evaluate receiver, then resolve field/method
                try work_stack.push(.{ .apply_continuation = .{ .dot_access_resolve = .{
                    .field_name = dot_access.field_name,
                    .method_args = dot_access.args,
                    .receiver_rt_var = receiver_rt_var,
                    .expr_idx = expr_idx,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = dot_access.receiver,
                    .expected_rt_var = receiver_rt_var,
                } });
            },

            // If we reach here, there's a new expression type that hasn't been added.
            // else => unreachable,
        }
    }

    // ========================================================================
    // Helper functions for evaluating immediate values (no sub-expressions)
    // ========================================================================

    /// Evaluate a numeric literal (e_num)
    fn evalNum(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        num_lit: @TypeOf(@as(can.CIR.Expr, undefined).e_num),
    ) Error!StackValue {
        // Get the layout type variable - use expected_rt_var if provided for layout determination
        const layout_rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };

        var layout_val = try self.getRuntimeLayout(layout_rt_var);

        // If the layout isn't a numeric type (e.g., ZST from unconstrained flex/rigid),
        // default to Dec since we're evaluating a numeric literal
        const is_numeric_layout = layout_val.tag == .scalar and
            (layout_val.data.scalar.tag == .int or layout_val.data.scalar.tag == .frac);
        if (!is_numeric_layout) {
            layout_val = layout.Layout.frac(types.Frac.Precision.dec);
        }

        var value = try self.pushRaw(layout_val, 0, layout_rt_var);
        value.is_initialized = false;
        switch (layout_val.tag) {
            .scalar => switch (layout_val.data.scalar.tag) {
                .int => try value.setIntFromBytes(num_lit.value.bytes, num_lit.value.kind == .u128),
                .frac => switch (layout_val.data.scalar.data.frac) {
                    .f32 => {
                        const ptr = @as(*f32, @ptrCast(@alignCast(value.ptr.?)));
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

        // If the rt_var is still flex but we evaluated to a numeric type,
        // update the rt_var to a concrete numeric type for method dispatch.
        // This is needed because getRuntimeLayout defaults flex vars to Dec layout
        // but doesn't update the rt_var itself.
        const rt_resolved = self.runtime_types.resolveVar(value.rt_var);
        if (rt_resolved.desc.content == .flex) {
            // Create concrete type based on the layout we used
            const concrete_rt_var = switch (layout_val.tag) {
                .scalar => switch (layout_val.data.scalar.tag) {
                    .int => switch (layout_val.data.scalar.data.int) {
                        .i8 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("I8")),
                        .i16 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("I16")),
                        .i32 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("I32")),
                        .i64 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("I64")),
                        .i128 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("I128")),
                        .u8 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("U8")),
                        .u16 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("U16")),
                        .u32 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("U32")),
                        .u64 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("U64")),
                        .u128 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("U128")),
                    },
                    .frac => switch (layout_val.data.scalar.data.frac) {
                        .f32 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("F32")),
                        .f64 => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("F64")),
                        .dec => try self.runtime_types.freshFromContent(try self.mkNumberTypeContentRuntime("Dec")),
                    },
                    else => value.rt_var,
                },
                else => value.rt_var,
            };
            value.rt_var = concrete_rt_var;
        }

        return value;
    }

    /// Evaluate a f32 fractional literal (e_frac_f32)
    fn evalFracF32(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lit: @TypeOf(@as(can.CIR.Expr, undefined).e_frac_f32),
    ) Error!StackValue {
        const layout_rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const layout_val = try self.getRuntimeLayout(layout_rt_var);

        const value = try self.pushRaw(layout_val, 0, layout_rt_var);
        if (value.ptr) |ptr| {
            const typed_ptr: *f32 = @ptrCast(@alignCast(ptr));
            typed_ptr.* = lit.value;
        }
        return value;
    }

    /// Evaluate a f64 fractional literal (e_frac_f64)
    fn evalFracF64(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lit: @TypeOf(@as(can.CIR.Expr, undefined).e_frac_f64),
    ) Error!StackValue {
        const layout_rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const layout_val = try self.getRuntimeLayout(layout_rt_var);

        const value = try self.pushRaw(layout_val, 0, layout_rt_var);
        if (value.ptr) |ptr| {
            const typed_ptr: *f64 = @ptrCast(@alignCast(ptr));
            typed_ptr.* = lit.value;
        }
        return value;
    }

    /// Evaluate a decimal literal (e_dec)
    fn evalDec(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        dec_lit: @TypeOf(@as(can.CIR.Expr, undefined).e_dec),
    ) Error!StackValue {
        const layout_rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const layout_val = try self.getRuntimeLayout(layout_rt_var);

        const value = try self.pushRaw(layout_val, 0, layout_rt_var);
        if (value.ptr) |ptr| {
            const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
            typed_ptr.* = dec_lit.value;
        }
        return value;
    }

    /// Evaluate a small decimal literal (e_dec_small)
    fn evalDecSmall(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        small: @TypeOf(@as(can.CIR.Expr, undefined).e_dec_small),
    ) Error!StackValue {
        const layout_rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const layout_val = try self.getRuntimeLayout(layout_rt_var);

        // Dec literals require Dec-compatible layout. If we reach here with a different layout
        // (e.g., U8 integer), it means validation should have caught this and skipped evaluation.
        std.debug.assert(layout_val.tag == .scalar and
            layout_val.data.scalar.tag == .frac and
            layout_val.data.scalar.data.frac == .dec);

        const value = try self.pushRaw(layout_val, 0, layout_rt_var);
        if (value.ptr) |ptr| {
            const typed_ptr: *RocDec = @ptrCast(@alignCast(ptr));
            const scale_factor = std.math.pow(i128, 10, RocDec.decimal_places - small.value.denominator_power_of_ten);
            const scaled = @as(i128, small.value.numerator) * scale_factor;
            typed_ptr.* = RocDec{ .num = scaled };
        }
        return value;
    }

    /// Evaluate a string segment literal (e_str_segment)
    fn evalStrSegment(
        self: *Interpreter,
        seg: @TypeOf(@as(can.CIR.Expr, undefined).e_str_segment),
        _: *RocOps,
    ) Error!StackValue {
        const content = self.env.getString(seg.literal);
        const str_rt_var = try self.getCanonicalStrRuntimeVar();
        const value = try self.pushStr(str_rt_var);
        const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
        // Use arena allocator for string literals - freed wholesale at interpreter deinit
        roc_str.* = try self.createConstantStr(content);
        return value;
    }

    /// Evaluate an empty record literal (e_empty_record)
    fn evalEmptyRecord(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
    ) Error!StackValue {
        const rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const rec_layout = try self.getRuntimeLayout(rt_var);
        return try self.pushRaw(rec_layout, 0, rt_var);
    }

    /// Evaluate an empty list literal (e_empty_list)
    fn evalEmptyList(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
    ) Error!StackValue {
        const rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };

        // Get the element type from the list type and use flex_type_context for it
        const list_resolved = self.runtime_types.resolveVar(rt_var);
        var final_rt_var = rt_var;
        if (list_resolved.desc.content == .structure) {
            if (list_resolved.desc.content.structure == .nominal_type) {
                const list_nom = list_resolved.desc.content.structure.nominal_type;
                const list_args = self.runtime_types.sliceNominalArgs(list_nom);
                if (list_args.len > 0) {
                    const elem_var = list_args[0];
                    const elem_resolved = self.runtime_types.resolveVar(elem_var);
                    // If element type is a flex var and we have mappings, use the mapped type
                    if (elem_resolved.desc.content == .flex and self.flex_type_context.count() > 0) {
                        var it = self.flex_type_context.iterator();
                        var first_concrete: ?types.Var = null;
                        var all_same = true;
                        while (it.next()) |entry| {
                            const mapped_var = entry.value_ptr.*;
                            const mapped_resolved = self.runtime_types.resolveVar(mapped_var);
                            if (mapped_resolved.desc.content != .flex) {
                                if (first_concrete) |first| {
                                    const first_resolved = self.runtime_types.resolveVar(first);
                                    if (first_resolved.var_ != mapped_resolved.var_) {
                                        all_same = false;
                                        break;
                                    }
                                } else {
                                    first_concrete = mapped_var;
                                }
                            }
                        }
                        if (all_same) {
                            if (first_concrete) |concrete_elem_var| {
                                // Create a new List type with the concrete element type
                                // Get the backing var from the original list type
                                const backing_var = self.runtime_types.getNominalBackingVar(list_nom);
                                // Create new nominal content
                                const args = [_]types.Var{concrete_elem_var};
                                const new_list_content = self.runtime_types.mkNominal(
                                    list_nom.ident,
                                    backing_var,
                                    &args,
                                    list_nom.origin_module,
                                    list_nom.is_opaque,
                                ) catch unreachable;
                                // Create a new Var from that content
                                final_rt_var = self.runtime_types.freshFromContent(new_list_content) catch unreachable;
                            }
                        }
                    }
                }
            }
        }

        const derived_layout = try self.getRuntimeLayout(final_rt_var);

        // Ensure we have a proper list layout even if the type variable defaulted to Dec.
        const list_layout = if (derived_layout.tag == .list or derived_layout.tag == .list_of_zst)
            derived_layout
        else blk: {
            // Default to list of Dec for empty lists when type can't be determined
            const default_elem_layout = Layout.frac(types.Frac.Precision.dec);
            const elem_layout_idx = try self.runtime_layout_store.insertLayout(default_elem_layout);
            break :blk Layout{ .tag = .list, .data = .{ .list = elem_layout_idx } };
        };

        const dest = try self.pushRaw(list_layout, 0, final_rt_var);
        if (dest.ptr) |ptr| {
            const header: *RocList = @ptrCast(@alignCast(ptr));
            header.* = RocList.empty();
        }
        return dest;
    }

    /// Evaluate a zero-argument tag (e_zero_argument_tag)
    fn evalZeroArgumentTag(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        zero: @TypeOf(@as(can.CIR.Expr, undefined).e_zero_argument_tag),
        roc_ops: *RocOps,
    ) Error!StackValue {
        const rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        // Use resolveBaseVar to unwrap nominal types (like Bool := [False, True])
        const resolved = self.resolveBaseVar(rt_var);
        if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) {
            self.triggerCrash("e_zero_argument_tag: expected tag_union structure type", false, roc_ops);
            return error.Crash;
        }
        // Use appendUnionTags to properly handle tag union extensions
        var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.appendUnionTags(rt_var, &tag_list);
        // Find tag index by translating the source ident to the runtime store
        const tag_index = try self.findTagIndexByIdentInList(self.env, zero.name, tag_list.items) orelse {
            const name_text = self.env.getIdent(zero.name);
            const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
            self.triggerCrash(msg, true, roc_ops);
            return error.Crash;
        };
        const layout_val = try self.getRuntimeLayout(rt_var);

        // Handle different layout representations
        if (layout_val.tag == .scalar) {
            var out = try self.pushRaw(layout_val, 0, rt_var);
            if (layout_val.data.scalar.tag == .int) {
                out.is_initialized = false;
                try out.setInt(@intCast(tag_index));
                out.is_initialized = true;
                return out;
            }
            self.triggerCrash("e_zero_argument_tag: scalar layout is not int", false, roc_ops);
            return error.Crash;
        } else if (layout_val.tag == .record) {
            // Record { tag: Discriminant, payload: ZST }
            var dest = try self.pushRaw(layout_val, 0, rt_var);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            const tag_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                self.triggerCrash("e_zero_argument_tag: tag field not found", false, roc_ops);
                return error.Crash;
            };
            // Get rt_var for the tag field from the record type
            const record_resolved = self.runtime_types.resolveVar(rt_var);
            const tag_rt_var = blk: {
                if (record_resolved.desc.content == .structure) {
                    const flat = record_resolved.desc.content.structure;
                    const fields_range = switch (flat) {
                        .record => |rec| rec.fields,
                        .record_unbound => |fields| fields,
                        else => break :blk try self.runtime_types.fresh(),
                    };
                    const fields = self.runtime_types.getRecordFieldsSlice(fields_range);
                    var i: usize = 0;
                    while (i < fields.len) : (i += 1) {
                        const f = fields.get(i);
                        if (f.name == self.env.idents.tag) {
                            break :blk f.var_;
                        }
                    }
                }
                break :blk try self.runtime_types.fresh();
            };
            const tag_field = try acc.getFieldByIndex(tag_idx, tag_rt_var);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            } else {
                self.triggerCrash("e_zero_argument_tag: record tag field is not scalar int", false, roc_ops);
                return error.Crash;
            }
            return dest;
        } else if (layout_val.tag == .tuple) {
            // Tuple (payload, tag) - tag unions are now represented as tuples
            var dest = try self.pushRaw(layout_val, 0, rt_var);
            var acc = try dest.asTuple(&self.runtime_layout_store);
            // Element 1 is the tag discriminant - get its rt_var from the tuple type
            const tuple_resolved = self.runtime_types.resolveVar(rt_var);
            const elem_rt_var = if (tuple_resolved.desc.content == .structure and tuple_resolved.desc.content.structure == .tuple) blk: {
                const elem_vars = self.runtime_types.sliceVars(tuple_resolved.desc.content.structure.tuple.elems);
                break :blk if (elem_vars.len > 1) elem_vars[1] else rt_var;
            } else rt_var;
            const tag_field = try acc.getElement(1, elem_rt_var);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            } else {
                self.triggerCrash("e_zero_argument_tag: tuple tag field is not scalar int", false, roc_ops);
                return error.Crash;
            }
            return dest;
        }
        self.triggerCrash("e_zero_argument_tag: unexpected layout type", false, roc_ops);
        return error.Crash;
    }

    /// Finalize a tag with no payload arguments (but may still have record/tuple layout)
    fn finalizeTagNoPayload(
        self: *Interpreter,
        rt_var: types.Var,
        tag_index: usize,
        layout_val: Layout,
        roc_ops: *RocOps,
    ) Error!StackValue {
        if (layout_val.tag == .record) {
            var dest = try self.pushRaw(layout_val, 0, rt_var);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                self.triggerCrash("e_tag: tag field not found", false, roc_ops);
                return error.Crash;
            };
            // Get rt_var for the tag field from the record type
            const record_resolved = self.runtime_types.resolveVar(rt_var);
            const tag_rt_var = blk: {
                if (record_resolved.desc.content == .structure) {
                    const flat = record_resolved.desc.content.structure;
                    const fields_range = switch (flat) {
                        .record => |rec| rec.fields,
                        .record_unbound => |fields| fields,
                        else => break :blk try self.runtime_types.fresh(),
                    };
                    const fields = self.runtime_types.getRecordFieldsSlice(fields_range);
                    var i: usize = 0;
                    while (i < fields.len) : (i += 1) {
                        const f = fields.get(i);
                        if (f.name == self.env.idents.tag) {
                            break :blk f.var_;
                        }
                    }
                }
                break :blk try self.runtime_types.fresh();
            };
            const tag_field = try acc.getFieldByIndex(tag_field_idx, tag_rt_var);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            }
            return dest;
        } else if (layout_val.tag == .tuple) {
            var dest = try self.pushRaw(layout_val, 0, rt_var);
            var acc = try dest.asTuple(&self.runtime_layout_store);
            // Get element rt_var from tuple type
            const tuple_resolved = self.runtime_types.resolveVar(rt_var);
            const elem_rt_var = if (tuple_resolved.desc.content == .structure and tuple_resolved.desc.content.structure == .tuple) blk: {
                const elem_vars = self.runtime_types.sliceVars(tuple_resolved.desc.content.structure.tuple.elems);
                break :blk if (elem_vars.len > 1) elem_vars[1] else rt_var;
            } else rt_var;
            const tag_field = try acc.getElement(1, elem_rt_var);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            }
            return dest;
        } else if (layout_val.tag == .tag_union) {
            var dest = try self.pushRaw(layout_val, 0, rt_var);
            // Write discriminant at discriminant_offset
            const tu_data = self.runtime_layout_store.getTagUnionData(layout_val.data.tag_union.idx);
            const base_ptr: [*]u8 = @ptrCast(dest.ptr.?);
            const disc_ptr = base_ptr + tu_data.discriminant_offset;
            switch (tu_data.discriminant_size) {
                1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(tag_index),
                2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_index),
                4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_index),
                8 => @as(*u64, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tag_index),
                else => {},
            }
            dest.is_initialized = true;
            return dest;
        }
        self.triggerCrash("e_tag: unexpected layout in finalizeTagNoPayload", false, roc_ops);
        return error.Crash;
    }

    // ========================================================================
    // Helper functions for lambda/closure creation
    // ========================================================================

    /// Evaluate a lambda expression (e_lambda) - creates a closure value with empty captures
    fn evalLambda(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lam: @TypeOf(@as(can.CIR.Expr, undefined).e_lambda),
        roc_ops: *RocOps,
    ) Error!StackValue {
        // Build a closure value with empty captures using the runtime layout for the lambda's type
        const rt_var = if (expected_rt_var) |provided_var|
            provided_var
        else blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const closure_layout = try self.getRuntimeLayout(rt_var);
        if (closure_layout.tag != .closure) {
            self.triggerCrash("e_lambda: expected closure layout", false, roc_ops);
            return error.Crash;
        }
        const value = try self.pushRaw(closure_layout, 0, rt_var);
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
    }

    /// Evaluate a low-level lambda expression (e_low_level_lambda) - creates a closure for builtins
    fn evalLowLevelLambda(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lam: @TypeOf(@as(can.CIR.Expr, undefined).e_low_level_lambda),
    ) Error!StackValue {
        const rt_var = if (expected_rt_var) |provided_var|
            provided_var
        else blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const closure_layout = try self.getRuntimeLayout(rt_var);
        const value = try self.pushRaw(closure_layout, 0, rt_var);
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
    }

    /// Evaluate a hosted lambda expression (e_hosted_lambda) - creates a closure for host dispatch
    fn evalHostedLambda(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        hosted: @TypeOf(@as(can.CIR.Expr, undefined).e_hosted_lambda),
    ) Error!StackValue {
        // Get the rt_var from the expression's type
        const ct_var = can.ModuleEnv.varFrom(expr_idx);
        const rt_var = try self.translateTypeVar(self.env, ct_var);

        // Get a ZST layout for hosted functions (they have no captures)
        const zst_idx = try self.runtime_layout_store.ensureZstLayout();
        const closure_layout = Layout{
            .tag = .closure,
            .data = .{
                .closure = .{
                    .captures_layout_idx = zst_idx,
                },
            },
        };
        const value = try self.pushRaw(closure_layout, 0, rt_var);
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
    }

    /// Evaluate a closure expression (e_closure) - creates a closure with captured values
    fn evalClosure(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        cls: @TypeOf(@as(can.CIR.Expr, undefined).e_closure),
        roc_ops: *RocOps,
    ) Error!StackValue {
        const lam_expr = self.env.store.getExpr(cls.lambda_idx);
        if (lam_expr != .e_lambda) {
            self.triggerCrash("e_closure: lambda_idx does not point to e_lambda", false, roc_ops);
            return error.Crash;
        }
        const lam = lam_expr.e_lambda;

        const caps = self.env.store.sliceCaptures(cls.captures);
        var field_layouts = try self.allocator.alloc(Layout, caps.len);
        defer self.allocator.free(field_layouts);
        var field_names = try self.allocator.alloc(base_pkg.Ident.Idx, caps.len);
        defer self.allocator.free(field_names);

        // Resolve all capture values
        var capture_values = try self.allocator.alloc(StackValue, caps.len);
        defer self.allocator.free(capture_values);

        for (caps, 0..) |cap_idx, i| {
            const cap = self.env.store.getCapture(cap_idx);
            const name_text = self.env.getIdent(cap.name);
            field_names[i] = try self.runtime_layout_store.env.insertIdent(base_pkg.Ident.for_text(name_text));

            const cap_val = self.resolveCapture(cap, roc_ops) orelse {
                self.triggerCrash("e_closure: failed to resolve capture value", false, roc_ops);
                return error.Crash;
            };
            capture_values[i] = cap_val;
            field_layouts[i] = cap_val.layout;
        }

        const captures_layout_idx = try self.runtime_layout_store.putRecord(self.runtime_layout_store.env, field_layouts, field_names);
        const captures_layout = self.runtime_layout_store.getLayout(captures_layout_idx);
        const closure_layout = Layout.closure(captures_layout_idx);
        // Get rt_var for the closure
        const ct_var = can.ModuleEnv.varFrom(expr_idx);
        const closure_rt_var = try self.translateTypeVar(self.env, ct_var);
        const value = try self.pushRaw(closure_layout, 0, closure_rt_var);
        self.registerDefValue(expr_idx, value);

        if (value.ptr) |ptr| {
            const header: *layout.Closure = @ptrCast(@alignCast(ptr));
            header.* = .{
                .body_idx = lam.body,
                .params = lam.args,
                .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                .captures_layout_idx = captures_layout_idx,
                .lambda_expr_idx = expr_idx,
                .source_env = self.env,
            };
            // Copy captures into record area following header
            const header_size = @sizeOf(layout.Closure);
            const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
            const aligned_off = std.mem.alignForward(usize, header_size, @intCast(cap_align.toByteUnits()));
            const base: [*]u8 = @ptrCast(@alignCast(ptr));
            const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
            const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true, .rt_var = closure_rt_var };
            var accessor = try rec_val.asRecord(&self.runtime_layout_store);
            for (caps, 0..) |_, cap_i| {
                const cap_val = capture_values[cap_i];
                const translated_name = field_names[cap_i];
                const idx_opt = accessor.findFieldIndex(translated_name) orelse {
                    self.triggerCrash("e_closure: capture field not found in record", false, roc_ops);
                    return error.Crash;
                };
                try accessor.setFieldByIndex(idx_opt, cap_val);
            }
        }
        return value;
    }

    /// Helper to resolve a capture value from bindings, active closures, or top-level defs
    fn resolveCapture(self: *Interpreter, cap: can.CIR.Expr.Capture, roc_ops: *RocOps) ?StackValue {
        // First try local bindings by pattern idx
        var i: usize = self.bindings.items.len;
        while (i > 0) {
            i -= 1;
            const b = self.bindings.items[i];
            if (b.pattern_idx == cap.pattern_idx) return b.value;
        }
        // Next try ALL active closure captures in reverse order
        if (self.active_closures.items.len > 0) {
            var closure_idx: usize = self.active_closures.items.len;
            while (closure_idx > 0) {
                closure_idx -= 1;
                const cls_val = self.active_closures.items[closure_idx];
                if (cls_val.layout.tag == .closure and cls_val.ptr != null) {
                    const captures_layout = self.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                    const header_sz = @sizeOf(layout.Closure);
                    const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                    const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                    const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                    const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                    // Use the closure's rt_var for the captures record
                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true, .rt_var = cls_val.rt_var };
                    var rec_acc = (rec_val.asRecord(&self.runtime_layout_store)) catch continue;
                    if (rec_acc.findFieldIndex(cap.name)) |fidx| {
                        const field_rt_var = self.runtime_types.fresh() catch continue;
                        if (rec_acc.getFieldByIndex(fidx, field_rt_var) catch null) |field_val| {
                            return field_val;
                        }
                    }
                }
            }
        }
        // Finally try top-level defs by pattern idx
        const all_defs = self.env.store.sliceDefs(self.env.all_defs);
        for (all_defs) |def_idx| {
            const def = self.env.store.getDef(def_idx);
            if (def.pattern == cap.pattern_idx) {
                var k: usize = self.def_stack.items.len;
                while (k > 0) {
                    k -= 1;
                    const entry = self.def_stack.items[k];
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
                self.def_stack.append(new_entry) catch return null;
                defer _ = self.def_stack.pop();
                return self.eval(def.expr, roc_ops) catch null;
            }
        }
        return null;
    }

    // ========================================================================
    // Helper functions for variable lookups
    // ========================================================================

    /// Evaluate a local variable lookup (e_lookup_local)
    /// Searches bindings in reverse order, checks closure captures, and handles
    /// lazy evaluation of top-level definitions.
    fn evalLookupLocal(
        self: *Interpreter,
        lookup: @TypeOf(@as(can.CIR.Expr, undefined).e_lookup_local),
        expected_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) Error!StackValue {
        // Search bindings in reverse
        var i: usize = self.bindings.items.len;
        while (i > 0) {
            i -= 1;
            const b = self.bindings.items[i];
            // Check both pattern_idx AND source module to avoid cross-module collisions.
            const same_module = (b.source_env == self.env) or
                (b.source_env.module_name_idx == self.env.module_name_idx);
            if (b.pattern_idx == lookup.pattern_idx and same_module) {
                // Check if this binding came from an e_anno_only expression
                if (b.expr_idx) |expr_idx| {
                    const binding_expr = self.env.store.getExpr(expr_idx);
                    if (binding_expr == .e_anno_only and b.value.layout.tag != .closure) {
                        self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
                        return error.Crash;
                    }

                    // For polymorphic numeric literals: if the expected type is a concrete
                    // numeric type that differs from the cached value's layout, re-evaluate
                    // the literal with the expected type. This enables true polymorphism for
                    // numeric literals like `x = 42; I64.to_str(x)`.
                    if (expected_rt_var) |exp_var| {
                        // Check if expected type is a concrete numeric type
                        const expected_layout = try self.getRuntimeLayout(exp_var);
                        const is_expected_numeric = expected_layout.tag == .scalar;
                        if (is_expected_numeric) {
                            // Check if cached value's layout differs from expected.
                            // Use Layout.eql instead of std.meta.eql to avoid comparing
                            // uninitialized union bytes which triggers Valgrind warnings.
                            const cached_layout = b.value.layout;
                            const layouts_differ = !cached_layout.eql(expected_layout);
                            if (layouts_differ) {
                                // Check if the binding expression is a numeric literal (direct or via lookup)
                                const root_numeric_expr = self.findRootNumericLiteral(expr_idx, b.source_env);
                                if (root_numeric_expr) |root_expr_idx| {
                                    // Re-evaluate the numeric expression with the expected type.
                                    // Set up flex_type_context so flex vars in the expression
                                    // translate to the expected type instead of defaulting to Dec.
                                    // Note: We no longer save/restore flex_type_context here because
                                    // the type mappings need to persist across the call chain for
                                    // polymorphic functions from pre-compiled modules like Builtin.
                                    try self.setupFlexContextForNumericExpr(root_expr_idx, b.source_env, exp_var);

                                    const result = try self.evalWithExpectedType(root_expr_idx, roc_ops, exp_var);
                                    return result;
                                }
                            }
                        }
                    }
                }
                const copy_result = try self.pushCopy(b.value);
                return copy_result;
            }
        }

        // If not found, try active closure captures by variable name
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
                        const lambda_expr = header.source_env.store.getExpr(header.lambda_expr_idx);
                        const has_real_captures = (lambda_expr == .e_closure);
                        if (has_real_captures) {
                            const captures_layout = self.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                            const header_sz = @sizeOf(layout.Closure);
                            const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                            const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                            const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                            const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                            const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true, .rt_var = cls_val.rt_var };
                            var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                            if (accessor.findFieldIndex(var_ident)) |fidx| {
                                const field_rt = try self.runtime_types.fresh();
                                const field_val = try accessor.getFieldByIndex(fidx, field_rt);
                                return try self.pushCopy(field_val);
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
                const result = try self.evalWithExpectedType(def.expr, roc_ops, null);
                try self.bindings.append(.{
                    .pattern_idx = def.pattern,
                    .value = result,
                    .expr_idx = def.expr,
                    .source_env = self.env,
                });
                // Return a copy to give the caller ownership while the binding retains ownership too.
                // This is consistent with the pushCopy call above for already-bound values.
                return try self.pushCopy(result);
            }
        }

        self.triggerCrash("e_lookup_local: definition not found in current scope", false, roc_ops);
        return error.Crash;
    }

    /// Evaluate an external variable lookup (e_lookup_external)
    /// Handles cross-module references by switching to the imported module's context.
    fn evalLookupExternal(
        self: *Interpreter,
        lookup: @TypeOf(@as(can.CIR.Expr, undefined).e_lookup_external),
        expected_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) Error!StackValue {
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
        const result = try self.evalWithExpectedType(target_def.expr, roc_ops, expected_rt_var);

        return result;
    }

    // ========================================================================
    // Helper functions for block evaluation
    // ========================================================================

    /// Add closure placeholders for mutual recursion support.
    /// This is the first pass over statements that creates bindings for closures
    /// before their actual evaluation, enabling mutual recursion.
    fn addClosurePlaceholders(
        self: *Interpreter,
        stmts: []const can.CIR.Statement.Idx,
        bindings_start: usize,
    ) Error!void {
        for (stmts) |stmt_idx| {
            const stmt = self.env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_decl => |d| {
                    const patt = self.env.store.getPattern(d.pattern);
                    if (patt != .assign) continue;
                    const rhs = self.env.store.getExpr(d.expr);
                    if ((rhs == .e_lambda or rhs == .e_closure) and !self.placeholderExists(bindings_start, d.pattern)) {
                        try self.addClosurePlaceholder(d.pattern, d.expr);
                    }
                },
                .s_decl_gen => |d| {
                    const patt = self.env.store.getPattern(d.pattern);
                    if (patt != .assign) continue;
                    const rhs = self.env.store.getExpr(d.expr);
                    if ((rhs == .e_lambda or rhs == .e_closure) and !self.placeholderExists(bindings_start, d.pattern)) {
                        try self.addClosurePlaceholder(d.pattern, d.expr);
                    }
                },
                .s_var => |v| {
                    const patt = self.env.store.getPattern(v.pattern_idx);
                    if (patt != .assign) continue;
                    const rhs = self.env.store.getExpr(v.expr);
                    if ((rhs == .e_lambda or rhs == .e_closure) and !self.placeholderExists(bindings_start, v.pattern_idx)) {
                        try self.addClosurePlaceholder(v.pattern_idx, v.expr);
                    }
                },
                else => {},
            }
        }
    }

    /// Check if a placeholder binding already exists for a pattern.
    fn placeholderExists(self: *Interpreter, start: usize, pattern_idx: can.CIR.Pattern.Idx) bool {
        var i: usize = self.bindings.items.len;
        while (i > start) {
            i -= 1;
            if (self.bindings.items[i].pattern_idx == pattern_idx) return true;
        }
        return false;
    }

    /// Add a closure placeholder binding for mutual recursion.
    fn addClosurePlaceholder(
        self: *Interpreter,
        patt_idx: can.CIR.Pattern.Idx,
        rhs_expr: can.CIR.Expr.Idx,
    ) Error!void {
        const patt_ct_var = can.ModuleEnv.varFrom(patt_idx);
        const patt_rt_var = try self.translateTypeVar(self.env, patt_ct_var);
        const closure_layout = try self.getRuntimeLayout(patt_rt_var);
        if (closure_layout.tag != .closure) return; // only closures get placeholders
        const lam_or = self.env.store.getExpr(rhs_expr);
        var body_idx: can.CIR.Expr.Idx = rhs_expr;
        var params: can.CIR.Pattern.Span = .{ .span = .{ .start = 0, .len = 0 } };
        if (lam_or == .e_lambda) {
            body_idx = lam_or.e_lambda.body;
            params = lam_or.e_lambda.args;
        } else if (lam_or == .e_closure) {
            const lam_expr = self.env.store.getExpr(lam_or.e_closure.lambda_idx);
            if (lam_expr == .e_lambda) {
                body_idx = lam_expr.e_lambda.body;
                params = lam_expr.e_lambda.args;
            }
        } else return;
        const ph = try self.pushRaw(closure_layout, 0, patt_rt_var);
        if (ph.ptr) |ptr| {
            const header: *layout.Closure = @ptrCast(@alignCast(ptr));
            header.* = .{
                .body_idx = body_idx,
                .params = params,
                .captures_pattern_idx = @enumFromInt(@as(u32, 0)),
                .captures_layout_idx = closure_layout.data.closure.captures_layout_idx,
                .lambda_expr_idx = rhs_expr,
                .source_env = self.env,
            };
        }
        try self.bindings.append(.{ .pattern_idx = patt_idx, .value = ph, .expr_idx = rhs_expr, .source_env = self.env });
    }

    /// Schedule processing of the next statement in a block.
    fn scheduleNextStatement(
        self: *Interpreter,
        work_stack: *WorkStack,
        stmt: can.CIR.Statement,
        remaining_stmts: []const can.CIR.Statement.Idx,
        final_expr: can.CIR.Expr.Idx,
        bindings_start: usize,
        expected_rt_var: ?types.Var,
        roc_ops: *RocOps,
    ) Error!void {
        switch (stmt) {
            .s_decl => |d| {
                // Schedule: evaluate expression, then bind the pattern
                try work_stack.push(.{ .apply_continuation = .{ .bind_decl = .{
                    .pattern = d.pattern,
                    .expr_idx = d.expr,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                    .expected_rt_var = expected_rt_var,
                } } });
                // Push expression evaluation
                const expr_ct_var = can.ModuleEnv.varFrom(d.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = d.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },
            .s_decl_gen => |d| {
                // Same as s_decl
                try work_stack.push(.{ .apply_continuation = .{ .bind_decl = .{
                    .pattern = d.pattern,
                    .expr_idx = d.expr,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                    .expected_rt_var = expected_rt_var,
                } } });
                const expr_ct_var = can.ModuleEnv.varFrom(d.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = d.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },
            .s_var => |v| {
                // Same as s_decl but uses pattern_idx
                try work_stack.push(.{ .apply_continuation = .{ .bind_decl = .{
                    .pattern = v.pattern_idx,
                    .expr_idx = v.expr,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                    .expected_rt_var = expected_rt_var,
                } } });
                const expr_ct_var = can.ModuleEnv.varFrom(v.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = v.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },
            .s_expr => |sx| {
                // Evaluate expression, discard result, continue with remaining
                // Push block_continue for remaining statements (with should_discard_value=true)
                try work_stack.push(.{
                    .apply_continuation = .{
                        .block_continue = .{
                            .remaining_stmts = remaining_stmts,
                            .final_expr = final_expr,
                            .bindings_start = bindings_start,
                            .should_discard_value = true, // s_expr result should be discarded
                            .expected_rt_var = expected_rt_var,
                        },
                    },
                });
                // Evaluate the expression; block_continue will discard its result
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = sx.expr,
                    .expected_rt_var = null,
                } });
            },
            .s_crash => |c| {
                const msg = self.env.getString(c.msg);
                self.triggerCrash(msg, false, roc_ops);
                return error.Crash;
            },
            .s_expect => |expect_stmt| {
                // Evaluate condition, then check
                const bool_rt_var = try self.getCanonicalBoolRuntimeVar();

                // Push expect_check_stmt continuation
                try work_stack.push(.{ .apply_continuation = .{ .expect_check_stmt = .{
                    .body_expr = expect_stmt.body,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });

                // Evaluate condition
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = expect_stmt.body,
                    .expected_rt_var = bool_rt_var,
                } });
            },
            .s_reassign => |r| {
                // Evaluate expression, then reassign

                // Push reassign_value continuation
                try work_stack.push(.{ .apply_continuation = .{ .reassign_value = .{
                    .pattern_idx = r.pattern_idx,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });

                // Evaluate the new value
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = r.expr,
                    .expected_rt_var = null,
                } });
            },
            .s_dbg => |dbg_stmt| {
                // Evaluate expression, then print
                const inner_ct_var = can.ModuleEnv.varFrom(dbg_stmt.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);

                // Push dbg_print_stmt continuation
                try work_stack.push(.{ .apply_continuation = .{ .dbg_print_stmt = .{
                    .rt_var = inner_rt_var,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });

                // Evaluate the expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = dbg_stmt.expr,
                    .expected_rt_var = inner_rt_var,
                } });
            },
            .s_inspect => |inspect_stmt| {
                // Evaluate expression, then render to Str (value is discarded in stmt context)
                const inner_ct_var = can.ModuleEnv.varFrom(inspect_stmt.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);

                // Push inspect_render_stmt continuation
                try work_stack.push(.{ .apply_continuation = .{ .inspect_render_stmt = .{
                    .rt_var = inner_rt_var,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });

                // Evaluate the expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = inspect_stmt.expr,
                    .expected_rt_var = inner_rt_var,
                } });
            },
            .s_return => |ret| {
                // Early return: evaluate expression, then use early_return continuation
                const expr_ct_var = can.ModuleEnv.varFrom(ret.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);

                // Push early_return continuation
                try work_stack.push(.{ .apply_continuation = .{ .early_return = .{} } });

                // Evaluate the return expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = ret.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },
            .s_for => |for_stmt| {
                // For loop: first evaluate the list, then set up iteration
                const expr_ct_var = can.ModuleEnv.varFrom(for_stmt.expr);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);

                // Get the element type for binding
                const patt_ct_var = can.ModuleEnv.varFrom(for_stmt.patt);
                const patt_rt_var = try self.translateTypeVar(self.env, patt_ct_var);

                // Push for_iterate continuation (will be executed after list is evaluated)
                try work_stack.push(.{
                    .apply_continuation = .{
                        .for_iterate = .{
                            .list_value = undefined, // Will be set when list is evaluated
                            .current_index = 0,
                            .list_len = 0, // Will be set when list is evaluated
                            .elem_size = 0, // Will be set when list is evaluated
                            .elem_layout = undefined, // Will be set when list is evaluated
                            .pattern = for_stmt.patt,
                            .patt_rt_var = patt_rt_var,
                            .body = for_stmt.body,
                            .bindings_start = bindings_start,
                            .stmt_context = .{
                                .remaining_stmts = remaining_stmts,
                                .final_expr = final_expr,
                            },
                        },
                    },
                });

                // Evaluate the list expression
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = for_stmt.expr,
                    .expected_rt_var = expr_rt_var,
                } });
            },
            .s_while => |while_stmt| {
                // While loop: first evaluate condition, then decide
                // Push while_loop_check continuation
                try work_stack.push(.{ .apply_continuation = .{ .while_loop_check = .{
                    .cond = while_stmt.cond,
                    .body = while_stmt.body,
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });

                // Evaluate the condition
                const cond_ct_var = can.ModuleEnv.varFrom(while_stmt.cond);
                const cond_rt_var = try self.translateTypeVar(self.env, cond_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = while_stmt.cond,
                    .expected_rt_var = cond_rt_var,
                } });
            },
            else => {
                @panic("statement type not yet implemented");
            },
        }
    }

    /// Apply a continuation to consume values from the value stack.
    /// Returns true to continue execution, false to exit the main loop.
    fn applyContinuation(
        self: *Interpreter,
        work_stack: *WorkStack,
        value_stack: *ValueStack,
        cont: Continuation,
        roc_ops: *RocOps,
    ) Error!bool {
        switch (cont) {
            .return_result => {
                // Signal to exit the main loop - the result is on the value stack
                return false;
            },
            .decref_value => |dv| {
                // Decrement reference count of the value
                dv.value.decref(&self.runtime_layout_store, roc_ops);
                return true;
            },
            .trim_bindings => |tb| {
                // Restore bindings to a previous length
                self.trimBindingList(&self.bindings, tb.target_len, roc_ops);
                return true;
            },
            .and_short_circuit => |sc| {
                // Pop LHS value from stack
                const lhs = value_stack.pop() orelse return error.Crash;
                defer lhs.decref(&self.runtime_layout_store, roc_ops);

                if (self.boolValueEquals(false, lhs)) {
                    // Short-circuit: LHS is false, so result is false
                    const result = try self.makeBoolValue(false);
                    try value_stack.push(result);
                } else {
                    // LHS is true, need to evaluate RHS
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = sc.rhs_expr,
                        .expected_rt_var = null,
                    } });
                }
                return true;
            },
            .or_short_circuit => |sc| {
                // Pop LHS value from stack
                const lhs = value_stack.pop() orelse return error.Crash;
                defer lhs.decref(&self.runtime_layout_store, roc_ops);

                if (self.boolValueEquals(true, lhs)) {
                    // Short-circuit: LHS is true, so result is true
                    const result = try self.makeBoolValue(true);
                    try value_stack.push(result);
                } else {
                    // LHS is false, need to evaluate RHS
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = sc.rhs_expr,
                        .expected_rt_var = null,
                    } });
                }
                return true;
            },
            .if_branch => |ib| {
                // Pop condition value from stack
                const cond = value_stack.pop() orelse return error.Crash;
                defer cond.decref(&self.runtime_layout_store, roc_ops);

                const is_true = self.boolValueEquals(true, cond);

                if (is_true) {
                    // Condition is true, evaluate the body
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ib.body,
                        .expected_rt_var = ib.expected_rt_var,
                    } });
                } else if (ib.remaining_branches.len > 0) {
                    // Try next branch
                    const next_branch = self.env.store.getIfBranch(ib.remaining_branches[0]);
                    // Push continuation for next branch
                    try work_stack.push(.{ .apply_continuation = .{ .if_branch = .{
                        .body = next_branch.body,
                        .remaining_branches = ib.remaining_branches[1..],
                        .final_else = ib.final_else,
                        .expected_rt_var = ib.expected_rt_var,
                    } } });
                    // Push condition evaluation
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = next_branch.cond,
                        .expected_rt_var = null,
                    } });
                } else {
                    // No more branches, evaluate final else
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ib.final_else,
                        .expected_rt_var = ib.expected_rt_var,
                    } });
                }
                return true;
            },
            .block_continue => |bc| {
                // For s_expr statements, we need to pop and discard the value
                // Only pop if should_discard_value is set (meaning this was scheduled after an s_expr)
                if (bc.should_discard_value) {
                    const val = value_stack.pop() orelse return error.Crash;
                    val.decref(&self.runtime_layout_store, roc_ops);
                }

                if (bc.remaining_stmts.len == 0) {
                    // No more statements, evaluate final expression
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = bc.final_expr,
                        .expected_rt_var = bc.expected_rt_var,
                    } });
                } else {
                    // Process next statement
                    const next_stmt = self.env.store.getStatement(bc.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, bc.remaining_stmts[1..], bc.final_expr, bc.bindings_start, bc.expected_rt_var, roc_ops);
                }
                return true;
            },
            .bind_decl => |bd| {
                // Pop evaluated value from stack
                const val = value_stack.pop() orelse return error.Crash;
                if (comptime trace_refcount and builtin.os.tag != .freestanding) {
                    const stderr_file: std.fs.File = .stderr();
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "[INTERP] bind_decl popped val ptr=0x{x} (will defer decref)\n", .{
                        @intFromPtr(val.ptr),
                    }) catch "[INTERP] bind_decl popped val\n";
                    stderr_file.writeAll(msg) catch {};
                }
                defer {
                    if (comptime trace_refcount and builtin.os.tag != .freestanding) {
                        const stderr_file: std.fs.File = .stderr();
                        var buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "[INTERP] bind_decl defer decref val ptr=0x{x}\n", .{
                            @intFromPtr(val.ptr),
                        }) catch "[INTERP] bind_decl defer decref\n";
                        stderr_file.writeAll(msg) catch {};
                    }
                    val.decref(&self.runtime_layout_store, roc_ops);
                }

                // Get the runtime type for pattern matching
                const expr_ct_var = can.ModuleEnv.varFrom(bd.expr_idx);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);

                // Bind the pattern
                var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                defer temp_binds.deinit();

                if (!try self.patternMatchesBind(bd.pattern, val, expr_rt_var, roc_ops, &temp_binds, bd.expr_idx)) {
                    // Pattern match failed - decref any bindings that were created
                    self.trimBindingList(&temp_binds, 0, roc_ops);
                    return error.TypeMismatch;
                }

                // Add bindings using upsertBinding to handle closure placeholders.
                // After upsertBinding, ownership of the binding's value is transferred
                // to self.bindings, so we must NOT decref temp_binds afterwards.
                for (temp_binds.items) |binding| {
                    if (comptime trace_refcount and builtin.os.tag != .freestanding) {
                        const stderr_file: std.fs.File = .stderr();
                        var buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "[INTERP] upsertBinding from temp_binds ptr=0x{x}\n", .{
                            @intFromPtr(binding.value.ptr),
                        }) catch "[INTERP] upsertBinding\n";
                        stderr_file.writeAll(msg) catch {};
                    }
                    try self.upsertBinding(binding, bd.bindings_start, roc_ops);
                }
                // Clear temp_binds without decref - ownership was transferred to self.bindings
                temp_binds.clearRetainingCapacity();

                // Continue with remaining statements
                if (bd.remaining_stmts.len == 0) {
                    // No more statements, evaluate final expression
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = bd.final_expr,
                        .expected_rt_var = bd.expected_rt_var,
                    } });
                } else {
                    // Process next statement
                    const next_stmt = self.env.store.getStatement(bd.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, bd.remaining_stmts[1..], bd.final_expr, bd.bindings_start, bd.expected_rt_var, roc_ops);
                }
                return true;
            },
            .tuple_collect => |tc| {
                // Tuple collection works by evaluating elements one at a time
                // and tracking how many we've collected
                if (tc.remaining_elems.len > 0) {
                    // More elements to evaluate - schedule next one
                    try work_stack.push(.{ .apply_continuation = .{ .tuple_collect = .{
                        .collected_count = tc.collected_count + 1,
                        .remaining_elems = tc.remaining_elems[1..],
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = tc.remaining_elems[0],
                        .expected_rt_var = null,
                    } });
                } else {
                    // All elements evaluated - finalize the tuple
                    // Pop all collected values from the value stack
                    const total_count = tc.collected_count;

                    if (total_count == 0) {
                        // Empty tuple (shouldn't happen as it's handled directly)
                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(&[0]Layout{});
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                        // Create empty tuple type var
                        const empty_range = try self.runtime_types.appendVars(&[0]types.Var{});
                        const empty_tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = empty_range } } };
                        const empty_tuple_rt_var = try self.runtime_types.freshFromContent(empty_tuple_content);
                        const tuple_val = try self.pushRaw(tuple_layout, 0, empty_tuple_rt_var);
                        try value_stack.push(tuple_val);
                    } else {
                        // Gather layouts and values
                        var elem_layouts = try self.allocator.alloc(Layout, total_count);
                        defer self.allocator.free(elem_layouts);

                        // Values are in reverse order on stack (first element pushed first, so it's at the bottom)
                        // We need to pop them and store in correct order
                        var values = try self.allocator.alloc(StackValue, total_count);
                        defer self.allocator.free(values);

                        // Collect element rt_vars for constructing tuple type
                        var elem_rt_vars = try self.allocator.alloc(types.Var, total_count);
                        defer self.allocator.free(elem_rt_vars);

                        // Pop values in reverse order (last evaluated is on top)
                        var i: usize = total_count;
                        while (i > 0) {
                            i -= 1;
                            values[i] = value_stack.pop() orelse return error.Crash;
                            elem_layouts[i] = values[i].layout;
                            elem_rt_vars[i] = values[i].rt_var;
                        }

                        // Create tuple type from element types
                        const elem_vars_range = try self.runtime_types.appendVars(elem_rt_vars);
                        const tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
                        const tuple_rt_var = try self.runtime_types.freshFromContent(tuple_content);

                        // Create tuple layout
                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                        var dest = try self.pushRaw(tuple_layout, 0, tuple_rt_var);
                        var accessor = try dest.asTuple(&self.runtime_layout_store);

                        if (total_count != accessor.getElementCount()) return error.TypeMismatch;

                        // Set all elements
                        for (0..total_count) |idx| {
                            try accessor.setElement(idx, values[idx]);
                        }

                        // Decref temporary values after they've been copied into the tuple
                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }

                        try value_stack.push(dest);
                    }
                }
                return true;
            },
            .list_collect => |lc| {
                // List collection works by evaluating elements one at a time
                // and tracking how many we've collected
                if (lc.remaining_elems.len > 0) {
                    // More elements to evaluate - schedule next one
                    try work_stack.push(.{ .apply_continuation = .{ .list_collect = .{
                        .collected_count = lc.collected_count + 1,
                        .remaining_elems = lc.remaining_elems[1..],
                        .elem_rt_var = lc.elem_rt_var,
                        .list_rt_var = lc.list_rt_var,
                    } } });
                    // Only pass expected_rt_var if it's concrete (not flex/rigid).
                    // This ensures nested lists compute their own concrete types
                    // instead of inheriting a polymorphic type from the outer list.
                    const elem_expected_rt_var: ?types.Var = blk: {
                        const elem_resolved = self.runtime_types.resolveVar(lc.elem_rt_var);
                        if (elem_resolved.desc.content == .flex or elem_resolved.desc.content == .rigid) {
                            break :blk null;
                        }
                        break :blk lc.elem_rt_var;
                    };
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = lc.remaining_elems[0],
                        .expected_rt_var = elem_expected_rt_var,
                    } });
                } else {
                    // All elements evaluated - finalize the list
                    const total_count = lc.collected_count;

                    if (total_count == 0) {
                        // Empty list (shouldn't happen as it's handled directly)
                        const list_layout = try self.getRuntimeLayout(lc.list_rt_var);
                        var dest = try self.pushRaw(list_layout, 0, lc.list_rt_var);
                        dest.rt_var = lc.list_rt_var;
                        if (dest.ptr != null) {
                            const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));
                            header.* = RocList.empty();
                        }
                        try value_stack.push(dest);
                    } else {
                        // Pop all collected values from the value stack
                        var values = try self.allocator.alloc(StackValue, total_count);
                        defer self.allocator.free(values);

                        // Pop values in reverse order (last evaluated is on top)
                        var i: usize = total_count;
                        while (i > 0) {
                            i -= 1;
                            values[i] = value_stack.pop() orelse return error.Crash;
                        }

                        // Use the actual layout from the first evaluated element
                        const actual_elem_layout = values[0].layout;

                        // Create the list layout with the correct element layout
                        const correct_elem_idx = try self.runtime_layout_store.insertLayout(actual_elem_layout);
                        const actual_list_layout = Layout{ .tag = .list, .data = .{ .list = correct_elem_idx } };

                        var dest = try self.pushRaw(actual_list_layout, 0, lc.list_rt_var);
                        dest.rt_var = lc.list_rt_var;
                        if (dest.ptr == null) {
                            // Decref all values before returning
                            for (values) |val| {
                                val.decref(&self.runtime_layout_store, roc_ops);
                            }
                            try value_stack.push(dest);
                            return true;
                        }

                        const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));
                        const elem_alignment = actual_elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                        const elem_alignment_u32: u32 = @intCast(elem_alignment);
                        const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(actual_elem_layout));
                        const elements_refcounted = actual_elem_layout.isRefcounted();

                        var runtime_list = RocList.allocateExact(
                            elem_alignment_u32,
                            total_count,
                            elem_size,
                            elements_refcounted,
                            roc_ops,
                        );

                        if (elem_size > 0) {
                            if (runtime_list.bytes) |buffer| {
                                for (values, 0..) |val, idx| {
                                    const dest_ptr = buffer + idx * elem_size;
                                    try val.copyToPtr(&self.runtime_layout_store, dest_ptr);
                                }
                            }
                        }

                        markListElementCount(&runtime_list, elements_refcounted);
                        header.* = runtime_list;

                        // Decref temporary values after they've been copied into the list
                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }

                        // Set the runtime type variable so method dispatch works correctly.
                        // Always use the actual element's rt_var to construct the list type,
                        // since it reflects the concrete types from evaluation.
                        var final_list_rt_var = lc.list_rt_var;
                        const first_elem_rt_resolved = self.runtime_types.resolveVar(values[0].rt_var);

                        // If actual element has a concrete type (not flex), create a new List type
                        // with the concrete element type. Always use createListTypeWithElement to
                        // ensure fresh backing vars are created (reusing backing vars causes corruption).
                        if (first_elem_rt_resolved.desc.content != .flex) {
                            final_list_rt_var = try self.createListTypeWithElement(values[0].rt_var);
                        }

                        var result = dest;
                        result.rt_var = final_list_rt_var;
                        try value_stack.push(result);
                    }
                }
                return true;
            },
            .record_collect => |rc| {
                // Record collection: evaluate extension (if any), then fields in order
                if (rc.remaining_fields.len > 0) {
                    // More fields to evaluate - schedule next one
                    const next_field_idx = rc.remaining_fields[0];
                    const f = self.env.store.getRecordField(next_field_idx);
                    const field_ct_var = can.ModuleEnv.varFrom(f.value);
                    const field_rt_var = try self.translateTypeVar(self.env, field_ct_var);

                    try work_stack.push(.{ .apply_continuation = .{ .record_collect = .{
                        .collected_count = rc.collected_count + 1,
                        .remaining_fields = rc.remaining_fields[1..],
                        .rt_var = rc.rt_var,
                        .expr_idx = rc.expr_idx,
                        .has_extension = rc.has_extension,
                        .all_fields = rc.all_fields,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = f.value,
                        .expected_rt_var = field_rt_var,
                    } });
                } else {
                    // All values collected - finalize the record
                    const total_field_values = rc.collected_count;

                    // Build layout info from collected values
                    var union_names = std.array_list.AlignedManaged(base_pkg.Ident.Idx, null).init(self.allocator);
                    defer union_names.deinit();
                    var union_layouts = std.array_list.AlignedManaged(layout.Layout, null).init(self.allocator);
                    defer union_layouts.deinit();
                    var union_indices = std.AutoHashMap(u32, usize).init(self.allocator);
                    defer union_indices.deinit();

                    // Pop field values from stack (in reverse order since last evaluated is on top)
                    var field_values = try self.allocator.alloc(StackValue, total_field_values);
                    defer self.allocator.free(field_values);

                    var i: usize = total_field_values;
                    while (i > 0) {
                        i -= 1;
                        field_values[i] = value_stack.pop() orelse return error.Crash;
                    }

                    // Handle base record if extension exists
                    var base_value_opt: ?StackValue = null;
                    if (rc.has_extension) {
                        base_value_opt = value_stack.pop() orelse return error.Crash;
                        const base_value = base_value_opt.?;
                        if (base_value.layout.tag != .record) {
                            base_value.decref(&self.runtime_layout_store, roc_ops);
                            for (field_values) |fv| fv.decref(&self.runtime_layout_store, roc_ops);
                            return error.TypeMismatch;
                        }
                        var base_accessor = try base_value.asRecord(&self.runtime_layout_store);

                        // Add base record fields to union
                        var idx: usize = 0;
                        while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                            const info = base_accessor.field_layouts.get(idx);
                            const field_layout = self.runtime_layout_store.getLayout(info.layout);
                            const key: u32 = @bitCast(info.name);
                            if (union_indices.get(key)) |idx_ptr| {
                                union_layouts.items[idx_ptr] = field_layout;
                                union_names.items[idx_ptr] = info.name;
                            } else {
                                try union_layouts.append(field_layout);
                                try union_names.append(info.name);
                                try union_indices.put(key, union_layouts.items.len - 1);
                            }
                        }
                    }

                    // Add explicit field layouts to union
                    // Translate field names from self.env's identifier store to runtime_layout_store.env's
                    // identifier store. This is necessary because field names may come from different modules
                    // (e.g., app module), but rendering uses root_env (same as runtime_layout_store.env).
                    for (rc.all_fields, 0..) |field_idx_enum, idx| {
                        const f = self.env.store.getRecordField(field_idx_enum);
                        const field_layout = field_values[idx].layout;
                        // Translate field name to runtime layout store's identifier space
                        const field_name_str = self.env.getIdent(f.name);
                        const translated_name = try @constCast(self.runtime_layout_store.env).insertIdent(base_pkg.Ident.for_text(field_name_str));
                        const key: u32 = @bitCast(translated_name);
                        if (union_indices.get(key)) |idx_ptr| {
                            union_layouts.items[idx_ptr] = field_layout;
                            union_names.items[idx_ptr] = translated_name;
                        } else {
                            try union_layouts.append(field_layout);
                            try union_names.append(translated_name);
                            try union_indices.put(key, union_layouts.items.len - 1);
                        }
                    }

                    // Create record layout using runtime_layout_store.env for field name lookups
                    const record_layout_idx = try self.runtime_layout_store.putRecord(self.runtime_layout_store.env, union_layouts.items, union_names.items);
                    const rec_layout = self.runtime_layout_store.getLayout(record_layout_idx);

                    // Cache the layout for this var
                    const resolved_rt = self.runtime_types.resolveVar(rc.rt_var);
                    const root_idx: usize = @intFromEnum(resolved_rt.var_);
                    try self.ensureVarLayoutCapacity(root_idx + 1);
                    self.var_to_layout_slot.items[root_idx] = @intFromEnum(record_layout_idx) + 1;

                    var dest = try self.pushRaw(rec_layout, 0, rc.rt_var);
                    var accessor = try dest.asRecord(&self.runtime_layout_store);

                    // Copy base record fields first
                    if (base_value_opt) |base_value| {
                        var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                        var idx: usize = 0;
                        while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                            const info = base_accessor.field_layouts.get(idx);
                            const dest_field_idx = accessor.findFieldIndex(info.name) orelse return error.TypeMismatch;
                            const field_rt = try self.runtime_types.fresh();
                            const base_field_value = try base_accessor.getFieldByIndex(idx, field_rt);
                            try accessor.setFieldByIndex(dest_field_idx, base_field_value);
                        }
                    }

                    // Set explicit field values (overwriting base values if needed)
                    for (rc.all_fields, 0..) |field_idx_enum, explicit_index| {
                        const f = self.env.store.getRecordField(field_idx_enum);
                        // Translate field name to runtime layout store's identifier space for lookup
                        const field_name_str = self.env.getIdent(f.name);
                        const translated_name = try @constCast(self.runtime_layout_store.env).insertIdent(base_pkg.Ident.for_text(field_name_str));
                        const dest_field_idx = accessor.findFieldIndex(translated_name) orelse return error.TypeMismatch;
                        const val = field_values[explicit_index];

                        // If overwriting a base field, decref the existing value
                        if (base_value_opt) |base_value| {
                            var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                            if (base_accessor.findFieldIndex(translated_name) != null) {
                                const field_rt = try self.runtime_types.fresh();
                                const existing = try accessor.getFieldByIndex(dest_field_idx, field_rt);
                                existing.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }

                        try accessor.setFieldByIndex(dest_field_idx, val);
                    }

                    // Decref base value and field values after they've been copied
                    if (base_value_opt) |base_value| {
                        base_value.decref(&self.runtime_layout_store, roc_ops);
                    }
                    for (field_values) |val| {
                        val.decref(&self.runtime_layout_store, roc_ops);
                    }

                    try value_stack.push(dest);
                }
                return true;
            },
            .early_return => {
                // Pop the evaluated value and signal early return
                const return_value = value_stack.pop() orelse return error.Crash;
                self.early_return_value = return_value;

                // Drain work stack until we find call_cleanup (function boundary)
                // This skips any remaining work items for the current function body
                while (work_stack.pop()) |pending_item| {
                    switch (pending_item) {
                        .apply_continuation => |pending_cont| {
                            switch (pending_cont) {
                                .call_cleanup => {
                                    // Found function boundary - put it back and continue normal processing
                                    try work_stack.push(pending_item);
                                    break;
                                },
                                .call_invoke_closure => |ci| {
                                    // Free arg_rt_vars if we're skipping a pending call invocation
                                    if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                                },
                                .for_iterate => |fl| {
                                    // Decref the list value when skipping a for loop
                                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                                },
                                .for_body_done => |fl| {
                                    // Decref the list value and clean up bindings
                                    self.trimBindingList(&self.bindings, fl.loop_bindings_start, roc_ops);
                                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                                },
                                else => {
                                    // Skip this continuation - it's part of the function body being early-returned from
                                },
                            }
                        },
                        .eval_expr => {
                            // Skip pending expression evaluations in the function body
                        },
                    }
                }
                return true;
            },
            .tag_collect => |tc| {
                // Tag payload collection: evaluate each argument, then finalize tag
                if (tc.remaining_args.len > 0) {
                    // More arguments to evaluate
                    const arg_idx = tc.collected_count;
                    const arg_rt_var = if (arg_idx < tc.arg_rt_vars.len) tc.arg_rt_vars[arg_idx] else null;
                    try work_stack.push(.{ .apply_continuation = .{ .tag_collect = .{
                        .collected_count = tc.collected_count + 1,
                        .remaining_args = tc.remaining_args[1..],
                        .arg_rt_vars = tc.arg_rt_vars,
                        .expr_idx = tc.expr_idx,
                        .rt_var = tc.rt_var,
                        .tag_index = tc.tag_index,
                        .layout_type = tc.layout_type,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = tc.remaining_args[0],
                        .expected_rt_var = arg_rt_var,
                    } });
                } else {
                    // All arguments collected - finalize the tag
                    const total_count = tc.collected_count;
                    const layout_val = try self.getRuntimeLayout(tc.rt_var);

                    // Pop all collected values
                    var values = try self.allocator.alloc(StackValue, total_count);
                    defer self.allocator.free(values);
                    var i: usize = total_count;
                    while (i > 0) {
                        i -= 1;
                        values[i] = value_stack.pop() orelse return error.Crash;
                    }

                    if (tc.layout_type == 0) {
                        // Record layout { tag, payload }
                        var dest = try self.pushRaw(layout_val, 0, tc.rt_var);
                        var acc = try dest.asRecord(&self.runtime_layout_store);
                        const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                            for (values) |v| v.decref(&self.runtime_layout_store, roc_ops);
                            self.triggerCrash("e_tag: tag field not found", false, roc_ops);
                            return error.Crash;
                        };
                        const payload_field_idx = acc.findFieldIndex(self.env.idents.payload) orelse {
                            for (values) |v| v.decref(&self.runtime_layout_store, roc_ops);
                            self.triggerCrash("e_tag: payload field not found", false, roc_ops);
                            return error.Crash;
                        };

                        // Write tag discriminant
                        const field_rt = try self.runtime_types.fresh();
                        const tag_field = try acc.getFieldByIndex(tag_field_idx, field_rt);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(tc.tag_index));
                        }

                        // Write payload
                        const field_rt2 = try self.runtime_types.fresh();
                        const payload_field = try acc.getFieldByIndex(payload_field_idx, field_rt2);
                        if (payload_field.ptr) |payload_ptr| {
                            if (total_count == 1) {
                                try values[0].copyToPtr(&self.runtime_layout_store, payload_ptr);
                            } else {
                                // Multiple args - create tuple payload
                                var elem_layouts = try self.allocator.alloc(Layout, total_count);
                                defer self.allocator.free(elem_layouts);
                                var elem_rt_vars = try self.allocator.alloc(types.Var, total_count);
                                defer self.allocator.free(elem_rt_vars);
                                for (values, 0..) |val, idx| {
                                    elem_layouts[idx] = val.layout;
                                    elem_rt_vars[idx] = val.rt_var;
                                }
                                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                                // Create tuple type from element types
                                const elem_vars_range = try self.runtime_types.appendVars(elem_rt_vars);
                                const tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
                                const tuple_rt_var = try self.runtime_types.freshFromContent(tuple_content);
                                var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true, .rt_var = tuple_rt_var };
                                var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                                for (values, 0..) |val, idx| {
                                    try tup_acc.setElement(idx, val);
                                }
                            }
                        }

                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }
                        try value_stack.push(dest);
                    } else if (tc.layout_type == 1) {
                        // Tuple layout (payload, tag)
                        var dest = try self.pushRaw(layout_val, 0, tc.rt_var);
                        var acc = try dest.asTuple(&self.runtime_layout_store);

                        // Compute element rt_vars for tuple access
                        // Element 0 = payload, Element 1 = discriminant (int)
                        const discriminant_rt_var = try self.runtime_types.fresh();
                        const payload_rt_var: types.Var = if (total_count == 1)
                            tc.arg_rt_vars[0]
                        else if (total_count > 0) blk: {
                            const elem_vars_range = try self.runtime_types.appendVars(tc.arg_rt_vars);
                            const tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
                            break :blk try self.runtime_types.freshFromContent(tuple_content);
                        } else try self.runtime_types.fresh();

                        // Write tag discriminant (element 1)
                        const tag_field = try acc.getElement(1, discriminant_rt_var);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(tc.tag_index));
                        }

                        // Write payload (element 0)
                        const payload_field = try acc.getElement(0, payload_rt_var);
                        if (payload_field.ptr) |payload_ptr| {
                            if (total_count == 1) {
                                // Check for layout mismatch and handle it
                                const arg_size = self.runtime_layout_store.layoutSize(values[0].layout);
                                const payload_size = self.runtime_layout_store.layoutSize(payload_field.layout);
                                const layouts_differ = arg_size > payload_size or !layoutsEqual(values[0].layout, payload_field.layout);

                                if (layouts_differ) {
                                    // Create properly-typed tuple with actual arg layout
                                    var elem_layouts_fixed = [2]Layout{ values[0].layout, tag_field.layout };
                                    const proper_tuple_idx = try self.runtime_layout_store.putTuple(&elem_layouts_fixed);
                                    const proper_tuple_layout = self.runtime_layout_store.getLayout(proper_tuple_idx);
                                    var proper_dest = try self.pushRaw(proper_tuple_layout, 0, tc.rt_var);
                                    var proper_acc = try proper_dest.asTuple(&self.runtime_layout_store);

                                    // Write tag
                                    const proper_tag_field = try proper_acc.getElement(1, discriminant_rt_var);
                                    if (proper_tag_field.layout.tag == .scalar and proper_tag_field.layout.data.scalar.tag == .int) {
                                        var tmp = proper_tag_field;
                                        tmp.is_initialized = false;
                                        try tmp.setInt(@intCast(tc.tag_index));
                                    }

                                    // Write payload
                                    const proper_payload_field = try proper_acc.getElement(0, values[0].rt_var);
                                    if (proper_payload_field.ptr) |proper_ptr| {
                                        try values[0].copyToPtr(&self.runtime_layout_store, proper_ptr);
                                    }

                                    for (values) |val| {
                                        val.decref(&self.runtime_layout_store, roc_ops);
                                    }
                                    proper_dest.rt_var = tc.rt_var;
                                    try value_stack.push(proper_dest);
                                    return true;
                                }

                                try values[0].copyToPtr(&self.runtime_layout_store, payload_ptr);
                            } else {
                                // Multiple args - create tuple payload
                                var elem_layouts = try self.allocator.alloc(Layout, total_count);
                                defer self.allocator.free(elem_layouts);
                                var elem_rt_vars = try self.allocator.alloc(types.Var, total_count);
                                defer self.allocator.free(elem_rt_vars);
                                for (values, 0..) |val, idx| {
                                    elem_layouts[idx] = val.layout;
                                    elem_rt_vars[idx] = val.rt_var;
                                }
                                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                                // Create tuple type from element types
                                const elem_vars_range = try self.runtime_types.appendVars(elem_rt_vars);
                                const tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
                                const tuple_rt_var = try self.runtime_types.freshFromContent(tuple_content);
                                var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true, .rt_var = tuple_rt_var };
                                var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                                for (values, 0..) |val, idx| {
                                    try tup_acc.setElement(idx, val);
                                }
                            }
                        }

                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }
                        try value_stack.push(dest);
                    } else if (tc.layout_type == 2) {
                        // Tag union layout: payload at offset 0, discriminant at discriminant_offset
                        const tu_data = self.runtime_layout_store.getTagUnionData(layout_val.data.tag_union.idx);

                        // Check for layout mismatch - if the expected payload is smaller than actual
                        // we need to use a properly-sized tuple layout to avoid corruption.
                        // This happens with polymorphic types like Result where the type param
                        // is a disconnected flex var that defaults to ZST layout.
                        if (total_count == 1) {
                            const arg_size = self.runtime_layout_store.layoutSize(values[0].layout);
                            const expected_payload_size = tu_data.discriminant_offset; // payload is before discriminant
                            // Apply fix when expected payload is very small but actual is larger
                            const needs_fix = expected_payload_size <= 1 and arg_size > expected_payload_size;
                            if (needs_fix) {
                                // Layout mismatch - create a tuple layout [payload, discriminant]
                                // This is the same approach as layout_type == 1
                                const disc_precision: types.Int.Precision = switch (tu_data.discriminant_size) {
                                    1 => .u8,
                                    2 => .u16,
                                    4 => .u32,
                                    8 => .u64,
                                    else => .u8,
                                };
                                const disc_layout = Layout{
                                    .tag = .scalar,
                                    .data = .{ .scalar = .{ .tag = .int, .data = .{ .int = disc_precision } } },
                                };
                                var elem_layouts_fixed = [2]Layout{ values[0].layout, disc_layout };
                                const proper_tuple_idx = try self.runtime_layout_store.putTuple(&elem_layouts_fixed);
                                const proper_tuple_layout = self.runtime_layout_store.getLayout(proper_tuple_idx);
                                var proper_dest = try self.pushRaw(proper_tuple_layout, 0, tc.rt_var);
                                var proper_acc = try proper_dest.asTuple(&self.runtime_layout_store);

                                // Create fresh vars for tuple element access
                                const disc_rt_var = try self.runtime_types.fresh();

                                // Write tag discriminant (element 1)
                                const proper_tag_field = try proper_acc.getElement(1, disc_rt_var);
                                if (proper_tag_field.layout.tag == .scalar and proper_tag_field.layout.data.scalar.tag == .int) {
                                    var tmp = proper_tag_field;
                                    tmp.is_initialized = false;
                                    try tmp.setInt(@intCast(tc.tag_index));
                                }

                                // Write payload (element 0)
                                const proper_payload_field = try proper_acc.getElement(0, values[0].rt_var);
                                if (proper_payload_field.ptr) |proper_ptr| {
                                    try values[0].copyToPtr(&self.runtime_layout_store, proper_ptr);
                                }

                                for (values) |val| {
                                    val.decref(&self.runtime_layout_store, roc_ops);
                                }
                                try value_stack.push(proper_dest);
                                return true;
                            }
                        }

                        var dest = try self.pushRaw(layout_val, 0, tc.rt_var);

                        // Write discriminant
                        const base_ptr: [*]u8 = @ptrCast(dest.ptr.?);
                        const disc_ptr = base_ptr + tu_data.discriminant_offset;
                        switch (tu_data.discriminant_size) {
                            1 => @as(*u8, @ptrCast(disc_ptr)).* = @intCast(tc.tag_index),
                            2 => @as(*u16, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tc.tag_index),
                            4 => @as(*u32, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tc.tag_index),
                            8 => @as(*u64, @ptrCast(@alignCast(disc_ptr))).* = @intCast(tc.tag_index),
                            else => {},
                        }

                        // Write payload at offset 0
                        const payload_ptr: *anyopaque = @ptrCast(base_ptr);
                        if (total_count == 1) {
                            try values[0].copyToPtr(&self.runtime_layout_store, payload_ptr);
                        } else {
                            // Multiple args - create tuple payload at offset 0
                            var elem_layouts = try self.allocator.alloc(Layout, total_count);
                            defer self.allocator.free(elem_layouts);
                            var elem_rt_vars = try self.allocator.alloc(types.Var, total_count);
                            defer self.allocator.free(elem_rt_vars);
                            for (values, 0..) |val, idx| {
                                elem_layouts[idx] = val.layout;
                                elem_rt_vars[idx] = val.rt_var;
                            }
                            const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                            const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                            // Create tuple type from element types
                            const elem_vars_range = try self.runtime_types.appendVars(elem_rt_vars);
                            const tuple_content = types.Content{ .structure = .{ .tuple = .{ .elems = elem_vars_range } } };
                            const tuple_rt_var = try self.runtime_types.freshFromContent(tuple_content);
                            var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true, .rt_var = tuple_rt_var };
                            var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                            for (values, 0..) |val, idx| {
                                try tup_acc.setElement(idx, val);
                            }
                        }

                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }
                        dest.is_initialized = true;
                        dest.rt_var = tc.rt_var;
                        try value_stack.push(dest);
                    }
                }
                return true;
            },
            .match_branches => |mb| {
                // Scrutinee is on value stack - get it but keep it there for potential later use
                const scrutinee_temp = value_stack.pop() orelse return error.Crash;
                // Make a copy to protect from corruption
                const scrutinee = try self.pushCopy(scrutinee_temp);
                scrutinee_temp.decref(&self.runtime_layout_store, roc_ops);

                // Use the scrutinee's own rt_var (preserves type through polymorphic calls)
                const effective_scrutinee_rt_var = scrutinee.rt_var;

                // Try branches starting from current_branch
                var branch_idx = mb.current_branch;
                while (branch_idx < mb.branches.len) : (branch_idx += 1) {
                    const br = self.env.store.getMatchBranch(mb.branches[branch_idx]);
                    const patterns = self.env.store.sliceMatchBranchPatterns(br.patterns);

                    for (patterns) |bp_idx| {
                        var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                        defer {
                            self.trimBindingList(&temp_binds, 0, roc_ops);
                            temp_binds.deinit();
                        }

                        // expr_idx not used for match pattern bindings
                        if (!try self.patternMatchesBind(
                            self.env.store.getMatchBranchPattern(bp_idx).pattern,
                            scrutinee,
                            effective_scrutinee_rt_var,
                            roc_ops,
                            &temp_binds,
                            null,
                        )) {
                            continue;
                        }

                        // Pattern matched! Add bindings
                        const start_len = self.bindings.items.len;
                        try self.bindings.appendSlice(temp_binds.items);
                        temp_binds.items.len = 0;

                        if (br.guard) |guard_idx| {
                            // Has guard - need to evaluate it
                            // Keep scrutinee on stack for potential next branch
                            try value_stack.push(scrutinee);

                            const guard_ct_var = can.ModuleEnv.varFrom(guard_idx);
                            const guard_rt_var = try self.translateTypeVar(self.env, guard_ct_var);

                            try work_stack.push(.{ .apply_continuation = .{ .match_guard = .{
                                .branch_body = br.value,
                                .result_rt_var = mb.result_rt_var,
                                .bindings_start = start_len,
                                .remaining_branches = mb.branches[branch_idx + 1 ..],
                                .expr_idx = mb.expr_idx,
                                .scrutinee_rt_var = mb.scrutinee_rt_var,
                            } } });
                            try work_stack.push(.{ .eval_expr = .{
                                .expr_idx = guard_idx,
                                .expected_rt_var = guard_rt_var,
                            } });
                            return true;
                        }

                        // No guard - evaluate body directly
                        scrutinee.decref(&self.runtime_layout_store, roc_ops);

                        try work_stack.push(.{ .apply_continuation = .{ .match_cleanup = .{
                            .bindings_start = start_len,
                        } } });
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = br.value,
                            .expected_rt_var = mb.result_rt_var,
                        } });
                        return true;
                    }
                }

                // No branch matched
                scrutinee.decref(&self.runtime_layout_store, roc_ops);
                self.triggerCrash("non-exhaustive match", false, roc_ops);
                return error.Crash;
            },
            .match_guard => |mg| {
                // Guard result is on value stack
                const guard_val = value_stack.pop() orelse return error.Crash;
                defer guard_val.decref(&self.runtime_layout_store, roc_ops);

                const guard_pass = self.boolValueEquals(true, guard_val);

                if (guard_pass) {
                    // Guard passed - evaluate body
                    // Scrutinee is still on value stack - pop and decref it
                    const scrutinee = value_stack.pop() orelse return error.Crash;
                    scrutinee.decref(&self.runtime_layout_store, roc_ops);

                    try work_stack.push(.{ .apply_continuation = .{ .match_cleanup = .{
                        .bindings_start = mg.bindings_start,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = mg.branch_body,
                        .expected_rt_var = mg.result_rt_var,
                    } });
                } else {
                    // Guard failed - try remaining branches
                    self.trimBindingList(&self.bindings, mg.bindings_start, roc_ops);

                    if (mg.remaining_branches.len == 0) {
                        // No more branches
                        const scrutinee = value_stack.pop() orelse return error.Crash;
                        scrutinee.decref(&self.runtime_layout_store, roc_ops);
                        self.triggerCrash("non-exhaustive match", false, roc_ops);
                        return error.Crash;
                    }

                    // Continue with remaining branches
                    try work_stack.push(.{ .apply_continuation = .{ .match_branches = .{
                        .expr_idx = mg.expr_idx,
                        .scrutinee_rt_var = mg.scrutinee_rt_var,
                        .result_rt_var = mg.result_rt_var,
                        .branches = mg.remaining_branches,
                        .current_branch = 0,
                    } } });
                }
                return true;
            },
            .match_cleanup => |mc| {
                // Result is on value stack - leave it there, just trim bindings
                self.trimBindingList(&self.bindings, mc.bindings_start, roc_ops);
                return true;
            },
            .expect_check => |ec| {
                // Pop condition value from stack
                const cond_val = value_stack.pop() orelse return error.Crash;
                const succeeded = self.boolValueEquals(true, cond_val);
                if (succeeded) {
                    // Return {} (empty record)
                    const ct_var = can.ModuleEnv.varFrom(ec.expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const layout_val = try self.getRuntimeLayout(rt_var);
                    const result = try self.pushRaw(layout_val, 0, rt_var);
                    try value_stack.push(result);
                    return true;
                }
                // Expect failed - trigger error
                self.handleExpectFailure(ec.body_expr, roc_ops);
                return error.Crash;
            },
            .dbg_print => |dp| {
                // Pop evaluated value from stack
                const value = value_stack.pop() orelse return error.Crash;
                defer value.decref(&self.runtime_layout_store, roc_ops);
                const rendered = try self.renderValueRocWithType(value, dp.inner_rt_var, roc_ops);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                // Return {} (empty record) - dbg always returns unit like expect
                const ct_var = can.ModuleEnv.varFrom(dp.expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const layout_val = try self.getRuntimeLayout(rt_var);
                const result = try self.pushRaw(layout_val, 0, rt_var);
                try value_stack.push(result);
                return true;
            },
            .inspect_render => |ir| {
                // Pop evaluated value from stack
                const value = value_stack.pop() orelse return error.Crash;

                // Check if the type is nominal and has a to_inspect method
                const resolved = self.runtime_types.resolveVar(ir.inner_rt_var);
                const maybe_to_inspect: ?StackValue = if (resolved.desc.content == .structure)
                    switch (resolved.desc.content.structure) {
                        .nominal_type => |nom| try self.tryResolveMethodByIdent(
                            nom.origin_module,
                            nom.ident.ident_idx,
                            self.env.idents.to_inspect,
                            roc_ops,
                            ir.inner_rt_var,
                        ),
                        else => null,
                    }
                else
                    null;

                if (maybe_to_inspect) |method_func| {
                    // Found to_inspect method - call it with the value
                    if (method_func.layout.tag != .closure) {
                        value.decref(&self.runtime_layout_store, roc_ops);
                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        return error.TypeMismatch;
                    }

                    const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                    const saved_env = self.env;
                    const saved_bindings_len = self.bindings.items.len;
                    self.env = @constCast(closure_header.source_env);

                    // Check if low-level lambda (unlikely for user-defined to_inspect)
                    const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                    if (lambda_expr == .e_low_level_lambda) {
                        const low_level = lambda_expr.e_low_level_lambda;
                        var args = [1]StackValue{value};
                        const result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);

                        // Decref based on ownership semantics
                        const arg_ownership = low_level.op.getArgOwnership();
                        if (arg_ownership.len > 0 and arg_ownership[0] == .borrow) {
                            value.decref(&self.runtime_layout_store, roc_ops);
                        }

                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        self.env = saved_env;
                        try value_stack.push(result);
                        return true;
                    }

                    const params = self.env.store.slicePatterns(closure_header.params);
                    if (params.len != 1) {
                        // to_inspect must take exactly one argument
                        self.env = saved_env;
                        value.decref(&self.runtime_layout_store, roc_ops);
                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        // Fall back to default rendering
                        const rendered = try self.renderValueRocWithType(value, ir.inner_rt_var, roc_ops);
                        defer self.allocator.free(rendered);
                        const str_rt_var = try self.getCanonicalStrRuntimeVar();
                        const str_value = try self.pushStr(str_rt_var);
                        const roc_str_ptr: *RocStr = @ptrCast(@alignCast(str_value.ptr.?));
                        roc_str_ptr.* = RocStr.fromSlice(rendered, roc_ops);
                        try value_stack.push(str_value);
                        return true;
                    }

                    try self.active_closures.append(method_func);
                    try self.bindings.append(.{
                        .pattern_idx = params[0],
                        .value = value,
                        .expr_idx = null, // expr_idx not used for inspect method parameter bindings
                        .source_env = self.env,
                    });

                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = 1,
                        .has_active_closure = true,
                        .did_instantiate = false,
                        .call_ret_rt_var = null,
                        .saved_rigid_subst = null,
                        .saved_flex_type_context = null,
                        .arg_rt_vars_to_free = null,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = closure_header.body_idx,
                        .expected_rt_var = null,
                    } });
                    return true;
                }

                // No to_inspect method found - use default rendering
                defer value.decref(&self.runtime_layout_store, roc_ops);

                // Check if this is an opaque or nominal type (without to_inspect)
                const rendered: []const u8 = if (resolved.desc.content == .structure and
                    resolved.desc.content.structure == .nominal_type)
                blk: {
                    const nom = resolved.desc.content.structure.nominal_type;
                    if (nom.is_opaque) {
                        // Opaque types without to_inspect render as "<opaque>"
                        break :blk try self.allocator.dupe(u8, "<opaque>");
                    } else {
                        // Nominal types render as "TypeName.InnerValue"
                        // Use the original type var - render_helpers will unwrap the nominal type
                        const type_name = self.root_env.getIdent(nom.ident.ident_idx);
                        const inner_rendered = try self.renderValueRocWithType(value, ir.inner_rt_var, roc_ops);
                        defer self.allocator.free(inner_rendered);
                        break :blk try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ type_name, inner_rendered });
                    }
                } else blk: {
                    // Non-nominal types use default rendering
                    break :blk try self.renderValueRocWithType(value, ir.inner_rt_var, roc_ops);
                };
                defer self.allocator.free(rendered);

                // Create a RocStr from the rendered bytes and push it
                const str_rt_var = try self.getCanonicalStrRuntimeVar();
                const str_value = try self.pushStr(str_rt_var);
                const roc_str_ptr: *RocStr = @ptrCast(@alignCast(str_value.ptr.?));
                roc_str_ptr.* = RocStr.fromSlice(rendered, roc_ops);
                try value_stack.push(str_value);
                return true;
            },
            .str_collect => |sc| {
                // State machine for string interpolation:
                // 1. If needs_conversion, convert top of value stack to string
                // 2. If remaining segments, process next one
                // 3. If no remaining segments, concatenate all collected strings

                var collected_count = sc.collected_count;
                var remaining = sc.remaining_segments;

                // Step 1: If we just evaluated an expression, convert it to string
                if (sc.needs_conversion) {
                    const seg_value = value_stack.pop() orelse return error.Crash;

                    // Convert to RocStr
                    const segment_str = try self.stackValueToRocStr(seg_value, seg_value.rt_var, roc_ops);
                    seg_value.decref(&self.runtime_layout_store, roc_ops);

                    // Push as string value
                    const str_rt_var = try self.getCanonicalStrRuntimeVar();
                    const str_value = try self.pushStr(str_rt_var);
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(str_value.ptr.?));
                    roc_str_ptr.* = segment_str;
                    try value_stack.push(str_value);
                    collected_count += 1;
                    remaining = remaining[1..]; // Move past the segment we just converted
                }

                // Step 2: Process remaining segments
                if (remaining.len == 0) {
                    // Step 3: All segments collected - concatenate them
                    // Fast path for single-segment strings: return directly without copying
                    if (sc.total_count == 1) {
                        // Single segment - just return it directly, transferring ownership
                        // No incref/decref needed since we're not copying, just passing through
                        const str_val = value_stack.pop() orelse return error.Crash;
                        try value_stack.push(str_val);
                        return true;
                    }

                    var segment_strings = try std.array_list.AlignedManaged(RocStr, null).initCapacity(self.allocator, sc.total_count);
                    defer {
                        for (segment_strings.items) |s| {
                            var str_copy = s;
                            str_copy.decref(roc_ops);
                        }
                        segment_strings.deinit();
                    }

                    // Pop values in reverse order (stack is LIFO)
                    var i: usize = 0;
                    while (i < sc.total_count) : (i += 1) {
                        const str_val = value_stack.pop() orelse return error.Crash;
                        if (str_val.ptr) |ptr| {
                            const roc_str: *RocStr = @ptrCast(@alignCast(ptr));
                            try segment_strings.append(roc_str.*);
                        } else {
                            try segment_strings.append(RocStr.empty());
                        }
                    }

                    // Reverse to get correct order
                    std.mem.reverse(RocStr, segment_strings.items);

                    // Calculate total length
                    var total_len: usize = 0;
                    for (segment_strings.items) |s| {
                        total_len += s.asSlice().len;
                    }

                    // Concatenate
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

                    const str_rt_var = try self.getCanonicalStrRuntimeVar();
                    const result = try self.pushStr(str_rt_var);
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(result.ptr.?));
                    roc_str_ptr.* = result_str;
                    try value_stack.push(result);
                    return true;
                }

                // Process next segment
                const next_seg = remaining[0];
                const next_seg_expr = self.env.store.getExpr(next_seg);

                if (next_seg_expr == .e_str_segment) {
                    // Literal segment - push directly as string value
                    // Use arena allocator for string literals - freed wholesale at interpreter deinit
                    const content = self.env.getString(next_seg_expr.e_str_segment.literal);
                    const seg_str = try self.createConstantStr(content);
                    const str_rt_var = try self.getCanonicalStrRuntimeVar();
                    const seg_value = try self.pushStr(str_rt_var);
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(seg_value.ptr.?));
                    roc_str_ptr.* = seg_str;
                    try value_stack.push(seg_value);

                    // Schedule continuation for remaining (no conversion needed)
                    try work_stack.push(.{ .apply_continuation = .{ .str_collect = .{
                        .collected_count = collected_count + 1,
                        .total_count = sc.total_count,
                        .remaining_segments = remaining[1..],
                        .needs_conversion = false,
                    } } });
                } else {
                    // Expression segment - evaluate it, then convert
                    const seg_ct_var = can.ModuleEnv.varFrom(next_seg);
                    const seg_rt_var = try self.translateTypeVar(self.env, seg_ct_var);
                    // Schedule continuation with needs_conversion = true
                    try work_stack.push(.{
                        .apply_continuation = .{
                            .str_collect = .{
                                .collected_count = collected_count,
                                .total_count = sc.total_count,
                                .remaining_segments = remaining, // Don't advance - we'll do it after conversion
                                .needs_conversion = true,
                            },
                        },
                    });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = next_seg,
                        .expected_rt_var = seg_rt_var,
                    } });
                }
                return true;
            },
            .call_collect_args => |cc| {
                // Function call: collect arguments one by one
                if (cc.remaining_args.len > 0) {
                    // More arguments to evaluate
                    const arg_idx = cc.collected_count;
                    const arg_rt_var = if (arg_idx < cc.arg_rt_vars.len) cc.arg_rt_vars[arg_idx] else null;

                    try work_stack.push(.{ .apply_continuation = .{ .call_collect_args = .{
                        .collected_count = cc.collected_count + 1,
                        .remaining_args = cc.remaining_args[1..],
                        .arg_rt_vars = cc.arg_rt_vars,
                        .call_ret_rt_var = cc.call_ret_rt_var,
                        .did_instantiate = cc.did_instantiate,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = cc.remaining_args[0],
                        .expected_rt_var = arg_rt_var,
                    } });
                }
                // If no more args, the call_invoke_closure continuation handles the rest
                return true;
            },
            .call_invoke_closure => |ci| {
                // All arguments collected - pop them and the function, then invoke
                // Stack state: [func_val, arg0, arg1, ...] (func at bottom, args on top)
                var saved_rigid_subst = ci.saved_rigid_subst;
                defer {
                    if (saved_rigid_subst) |saved| {
                        self.rigid_subst.deinit();
                        self.rigid_subst = saved;
                    }
                }

                const arg_count = ci.arg_count;

                // Pop all arguments (in reverse order)
                var arg_values = try self.allocator.alloc(StackValue, arg_count);
                defer self.allocator.free(arg_values);
                var i: usize = arg_count;
                while (i > 0) {
                    i -= 1;
                    arg_values[i] = value_stack.pop() orelse {
                        self.triggerCrash("call_invoke_closure: value_stack empty when popping arguments", false, roc_ops);
                        return error.Crash;
                    };
                }

                // Pop function value
                const func_val = value_stack.pop() orelse {
                    self.triggerCrash("call_invoke_closure: value_stack empty when popping function", false, roc_ops);
                    return error.Crash;
                };

                // Handle closure invocation
                if (func_val.layout.tag == .closure) {
                    const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));

                    // Switch to the closure's source module
                    const saved_env = self.env;
                    const saved_bindings_len = self.bindings.items.len;
                    self.env = @constCast(header.source_env);

                    // Check if this is an annotation-only function
                    const body_expr = self.env.store.getExpr(header.body_idx);
                    if (body_expr == .e_anno_only) {
                        self.env = saved_env;
                        func_val.decref(&self.runtime_layout_store, roc_ops);
                        for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                        if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                        self.triggerCrash("This function has no implementation. It is only a type annotation for now.", false, roc_ops);
                        return error.Crash;
                    }

                    // Check if this is a low-level lambda
                    const lambda_expr = self.env.store.getExpr(header.lambda_expr_idx);
                    if (lambda_expr == .e_low_level_lambda) {
                        const low_level = lambda_expr.e_low_level_lambda;

                        // Special handling for list_sort_with which requires continuation-based evaluation
                        if (low_level.op == .list_sort_with) {
                            std.debug.assert(arg_values.len == 2);
                            const list_arg = arg_values[0];
                            const compare_fn = arg_values[1];

                            // Restore environment before setting up sort (helper saves env for comparison cleanup)
                            self.env = saved_env;
                            func_val.decref(&self.runtime_layout_store, roc_ops);
                            if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);

                            switch (try self.setupSortWith(list_arg, compare_fn, ci.call_ret_rt_var, saved_rigid_subst, roc_ops, work_stack)) {
                                .already_sorted => |result_list| {
                                    compare_fn.decref(&self.runtime_layout_store, roc_ops);
                                    try value_stack.push(result_list);
                                },
                                .sorting_started => {},
                            }
                            saved_rigid_subst = null; // Ownership transferred to helper
                            return true;
                        }

                        // Call the builtin
                        const result = try self.callLowLevelBuiltin(low_level.op, arg_values, roc_ops, ci.call_ret_rt_var);

                        // Decref arguments based on ownership semantics.
                        // See src/builtins/OWNERSHIP.md for detailed documentation.
                        //
                        // Simple rule:
                        // - Borrow: decref (we release our copy, builtin didn't take ownership)
                        // - Consume: don't decref (ownership transferred to builtin)
                        const arg_ownership = low_level.op.getArgOwnership();
                        for (arg_values, 0..) |arg, arg_idx| {
                            // Only decref borrowed arguments. Consumed arguments have ownership
                            // transferred to the builtin (it handles cleanup or returns the value).
                            const ownership = if (arg_idx < arg_ownership.len) arg_ownership[arg_idx] else .borrow;
                            if (ownership == .borrow) {
                                arg.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }

                        // Restore environment and free arg_rt_vars
                        self.env = saved_env;
                        func_val.decref(&self.runtime_layout_store, roc_ops);
                        if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                        // rt_var is set by the builtin - builtins like list_get_unsafe set rt_var
                        // to the element's concrete type, which is more specific than the call site's
                        // polymorphic type and needed for correct method dispatch on the result.
                        try value_stack.push(result);
                        return true;
                    }

                    // Check if this is a hosted lambda
                    if (lambda_expr == .e_hosted_lambda) {
                        const hosted = lambda_expr.e_hosted_lambda;
                        const hosted_lambda_ct_var = can.ModuleEnv.varFrom(header.lambda_expr_idx);
                        const hosted_lambda_rt_var = try self.translateTypeVar(self.env, hosted_lambda_ct_var);
                        const resolved_func = self.runtime_types.resolveVar(hosted_lambda_rt_var);

                        const ret_rt_var = if (resolved_func.desc.content.unwrapFunc()) |func| func.ret else ci.call_ret_rt_var;
                        const result = try self.callHostedFunction(hosted.index, arg_values, roc_ops, ret_rt_var);

                        // Decref all args
                        for (arg_values) |arg| {
                            arg.decref(&self.runtime_layout_store, roc_ops);
                        }

                        // Restore environment and free arg_rt_vars
                        self.env = saved_env;
                        func_val.decref(&self.runtime_layout_store, roc_ops);
                        if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                        // rt_var is already set by callHostedFunction
                        try value_stack.push(result);
                        return true;
                    }

                    // Regular closure - bind parameters and evaluate body
                    const params = self.env.store.slicePatterns(header.params);
                    if (params.len != arg_count) {
                        self.env = saved_env;
                        func_val.decref(&self.runtime_layout_store, roc_ops);
                        for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                        if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                        return error.TypeMismatch;
                    }

                    // Provide closure context for capture lookup
                    try self.active_closures.append(func_val);

                    // Save the current flex_type_context before adding parameter mappings
                    // This will be restored in call_cleanup
                    var saved_flex_type_context = try self.flex_type_context.clone();
                    errdefer saved_flex_type_context.deinit();

                    // Bind parameters using pattern matching to handle destructuring
                    for (params, 0..) |param, idx| {
                        // Get the runtime type for this parameter
                        const param_rt_var = if (ci.arg_rt_vars_to_free) |vars|
                            (if (idx < vars.len) vars[idx] else try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(param)))
                        else
                            try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(param));

                        // Add the parameter's CT type to RT type mapping for polymorphic type propagation.
                        // This allows numeric literals inside the function body that were unified with
                        // this parameter's type at compile time to get the correct concrete type.
                        // IMPORTANT: Only add mappings for concrete (structure) types, not flex/rigid types.
                        // If the arg type is still flex/rigid, the default Dec fallback should apply.
                        if (ci.arg_rt_vars_to_free) |vars| {
                            if (idx < vars.len) {
                                const arg_rt_resolved = self.runtime_types.resolveVar(vars[idx]);
                                // Only add mapping if the argument has a concrete type (structure)
                                if (arg_rt_resolved.desc.content == .structure) {
                                    const param_ct_var = can.ModuleEnv.varFrom(param);
                                    // Propagate flex mappings from the compile-time type to runtime type.
                                    // This walks both types in parallel and maps any flex vars found in CT to their RT counterparts.
                                    try self.propagateFlexMappings(self.env, param_ct_var, vars[idx]);
                                }
                            }
                        }

                        // Use patternMatchesBind to properly handle complex patterns (e.g., list destructuring)
                        // patternMatchesBind borrows the value and creates copies for bindings, so we need to
                        // decref the original arg_value after successful binding
                        // expr_idx not used for function parameter bindings
                        if (!try self.patternMatchesBind(param, arg_values[idx], param_rt_var, roc_ops, &self.bindings, null)) {
                            // Pattern match failed - cleanup and error
                            self.env = saved_env;
                            _ = self.active_closures.pop();
                            func_val.decref(&self.runtime_layout_store, roc_ops);
                            for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                            if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                            // Restore flex_type_context on error
                            self.flex_type_context.deinit();
                            self.flex_type_context = saved_flex_type_context;
                            return error.TypeMismatch;
                        }
                        // Decref the original argument value since patternMatchesBind made copies
                        arg_values[idx].decref(&self.runtime_layout_store, roc_ops);
                    }

                    // Push cleanup continuation, then evaluate body
                    const cleanup_saved_rigid_subst = saved_rigid_subst;
                    saved_rigid_subst = null;
                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = params.len,
                        .has_active_closure = true,
                        .did_instantiate = ci.did_instantiate,
                        .call_ret_rt_var = ci.call_ret_rt_var,
                        .saved_rigid_subst = cleanup_saved_rigid_subst,
                        .saved_flex_type_context = saved_flex_type_context,
                        .arg_rt_vars_to_free = ci.arg_rt_vars_to_free,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = header.body_idx,
                        .expected_rt_var = ci.call_ret_rt_var,
                    } });
                    return true;
                }

                // Not a closure - check if it's a direct lambda expression
                func_val.decref(&self.runtime_layout_store, roc_ops);
                for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                self.triggerCrash("e_call: func is neither closure nor lambda", false, roc_ops);
                return error.Crash;
            },
            .call_cleanup => |cleanup| {
                // Function body evaluated - cleanup and return result
                // Check for early return
                if (self.early_return_value) |return_val_in| {
                    // Body triggered early return - use that value
                    self.early_return_value = null;
                    const return_val = return_val_in;

                    // rt_var is already set by the return value's creation

                    // Pop active closure if needed
                    if (cleanup.has_active_closure) {
                        if (self.active_closures.pop()) |closure_val| {
                            closure_val.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    // Restore rigid_subst if we did polymorphic instantiation
                    if (cleanup.saved_rigid_subst) |saved| {
                        self.rigid_subst.deinit();
                        self.rigid_subst = saved;
                    }

                    // Note: Don't restore flex_type_context (same rationale as normal return case)
                    if (cleanup.saved_flex_type_context) |saved| {
                        var saved_copy = saved;
                        saved_copy.deinit();
                    }

                    // Restore environment and cleanup bindings
                    // Use trimBindingList to properly decref all bindings created by pattern matching
                    // (which may be more than param_count due to destructuring)
                    self.env = cleanup.saved_env;
                    self.trimBindingList(&self.bindings, cleanup.saved_bindings_len, roc_ops);
                    if (cleanup.arg_rt_vars_to_free) |vars| self.allocator.free(vars);

                    try value_stack.push(return_val);
                    return true;
                }

                // Normal return - result is on value stack
                const result = value_stack.pop() orelse return error.Crash;

                // Pop active closure if needed
                if (cleanup.has_active_closure) {
                    if (self.active_closures.pop()) |closure_val| {
                        closure_val.decref(&self.runtime_layout_store, roc_ops);
                    }
                }

                // Restore rigid_subst if we did polymorphic instantiation
                if (cleanup.saved_rigid_subst) |saved| {
                    self.rigid_subst.deinit();
                    self.rigid_subst = saved;
                }

                // Note: We intentionally do NOT restore flex_type_context here.
                // The type mappings need to persist across the call chain for polymorphic
                // functions from pre-compiled modules like Builtin. When a function returns
                // a value that is used in subsequent calls (e.g., method dispatch returning
                // a closure that is then invoked), those later calls need the type mappings
                // from the original call arguments.
                //
                // The mappings are keyed by compile-time type vars, so mappings from different
                // call sites with different type vars won't conflict. For the same polymorphic
                // function called multiple times with different concrete types, the later call
                // will overwrite the mapping with the new concrete type, which is correct.
                if (cleanup.saved_flex_type_context) |saved| {
                    // Just free the saved context, don't restore it
                    var saved_copy = saved;
                    saved_copy.deinit();
                }

                // Restore environment and cleanup bindings
                // Use trimBindingList to properly decref all bindings created by pattern matching
                // (which may be more than param_count due to destructuring)
                self.env = cleanup.saved_env;
                self.trimBindingList(&self.bindings, cleanup.saved_bindings_len, roc_ops);
                if (cleanup.arg_rt_vars_to_free) |vars| self.allocator.free(vars);

                // rt_var is already set by the function's return value creation
                try value_stack.push(result);
                return true;
            },
            .unary_op_apply => |ua| {
                // Unary operation: operand is on stack, apply method
                const operand = value_stack.pop() orelse return error.Crash;
                defer operand.decref(&self.runtime_layout_store, roc_ops);

                // Resolve the operand type
                const operand_resolved = self.runtime_types.resolveVar(ua.operand_rt_var);

                // Get nominal type info
                const nominal_info = switch (operand_resolved.desc.content) {
                    .structure => |s| switch (s) {
                        .nominal_type => |nom| .{
                            .origin = nom.origin_module,
                            .ident = nom.ident.ident_idx,
                        },
                        else => {
                            return error.InvalidMethodReceiver;
                        },
                    },
                    else => {
                        return error.InvalidMethodReceiver;
                    },
                };

                // Resolve the method function
                const method_func = try self.resolveMethodFunction(
                    nominal_info.origin,
                    nominal_info.ident,
                    ua.method_ident,
                    roc_ops,
                    ua.operand_rt_var,
                );
                defer method_func.decref(&self.runtime_layout_store, roc_ops);

                // Call the method closure
                if (method_func.layout.tag != .closure) {
                    return error.TypeMismatch;
                }

                const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                // Switch to the closure's source module
                const saved_env = self.env;
                const saved_bindings_len = self.bindings.items.len;
                self.env = @constCast(closure_header.source_env);

                // Check if this is a low-level lambda
                const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                if (lambda_expr == .e_low_level_lambda) {
                    const low_level = lambda_expr.e_low_level_lambda;
                    var args = [1]StackValue{operand};
                    const result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);

                    // Decref operand based on ownership semantics
                    const arg_ownership = low_level.op.getArgOwnership();
                    if (arg_ownership.len > 0 and arg_ownership[0] == .borrow) {
                        operand.decref(&self.runtime_layout_store, roc_ops);
                    }

                    self.env = saved_env;
                    try value_stack.push(result);
                    return true;
                }

                // Regular closure invocation
                const params = self.env.store.slicePatterns(closure_header.params);
                if (params.len != 1) {
                    self.env = saved_env;
                    return error.TypeMismatch;
                }

                // Provide closure context
                try self.active_closures.append(method_func);

                // Bind parameter
                try self.bindings.append(.{
                    .pattern_idx = params[0],
                    .value = operand,
                    .expr_idx = null, // expr_idx not used for unary operator method parameter bindings
                    .source_env = self.env,
                });

                // Push cleanup and evaluate body
                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = params.len,
                    .has_active_closure = true,
                    .did_instantiate = false,
                    .call_ret_rt_var = null,
                    .saved_rigid_subst = null,
                    .saved_flex_type_context = null,
                    .arg_rt_vars_to_free = null,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = closure_header.body_idx,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .binop_eval_rhs => |be| {
                // Binary operation: LHS is on stack, now evaluate RHS
                // We keep LHS on stack, push continuation to apply method after RHS is evaluated
                try work_stack.push(.{ .apply_continuation = .{ .binop_apply = .{
                    .method_ident = be.method_ident,
                    .receiver_rt_var = be.lhs_rt_var,
                    .rhs_rt_var = be.rhs_rt_var,
                    .negate_result = be.negate_result,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = be.rhs_expr,
                    .expected_rt_var = be.rhs_rt_var,
                } });
                return true;
            },
            .binop_apply => |ba| {
                // Binary operation: both operands on stack, apply method
                // Stack: [lhs, rhs] - RHS on top
                const rhs = value_stack.pop() orelse return error.Crash;
                defer rhs.decref(&self.runtime_layout_store, roc_ops);
                const lhs = value_stack.pop() orelse return error.Crash;
                defer lhs.decref(&self.runtime_layout_store, roc_ops);

                // Prefer the runtime type from the evaluated value if it's more concrete
                // (i.e., has a structure type rather than flex/rigid from polymorphic calls)
                // Track if the value came from a polymorphic context (flex/rigid rt_var)
                var effective_receiver_rt_var = ba.receiver_rt_var;
                var value_is_polymorphic = false;
                const val_rt_var = lhs.rt_var;
                const val_resolved = self.runtime_types.resolveVar(val_rt_var);
                // Only use the value's type if it's concrete (has structure/alias)
                if (val_resolved.desc.content == .structure or val_resolved.desc.content == .alias) {
                    effective_receiver_rt_var = val_rt_var;
                } else if (val_resolved.desc.content == .flex or val_resolved.desc.content == .rigid) {
                    // The value came from a polymorphic context
                    value_is_polymorphic = true;
                }

                // Check if effective type is still flex/rigid after trying value's rt_var
                // Track whether we had to default to Dec so we know to use direct numeric handling
                var defaulted_to_dec = false;
                const resolved_check = self.runtime_types.resolveVar(effective_receiver_rt_var);
                if (resolved_check.desc.content == .flex or resolved_check.desc.content == .rigid) {
                    // No concrete type info available, default to Dec for numeric operations
                    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                    const dec_var = try self.runtime_types.freshFromContent(dec_content);
                    effective_receiver_rt_var = dec_var;
                    defaulted_to_dec = true;
                } else if (value_is_polymorphic) {
                    // The value is polymorphic but we have a concrete type from CIR - mark as polymorphic
                    // so we use direct numeric handling instead of method dispatch
                    defaulted_to_dec = true;
                }

                // Resolve the lhs type
                const lhs_resolved = self.runtime_types.resolveVar(effective_receiver_rt_var);

                // Get nominal type info, or handle anonymous structural types
                // Follow aliases to get to the underlying type
                var current_var = effective_receiver_rt_var;
                var current_resolved = lhs_resolved;
                var alias_count: u32 = 0;
                while (current_resolved.desc.content == .alias) {
                    alias_count += 1;
                    if (alias_count > 1000) break; // Prevent infinite loops
                    const alias = current_resolved.desc.content.alias;
                    current_var = self.runtime_types.getAliasBackingVar(alias);
                    current_resolved = self.runtime_types.resolveVar(current_var);
                }

                // Check if we can use low-level numeric comparison based on layout
                // This handles cases where method dispatch would fail (e.g., polymorphic values)
                // Only use direct handling when we had to default to Dec due to flex/rigid types
                const is_numeric_layout = lhs.layout.tag == .scalar and
                    (lhs.layout.data.scalar.tag == .int or lhs.layout.data.scalar.tag == .frac);
                if (is_numeric_layout and defaulted_to_dec) {
                    // Handle numeric comparisons directly via low-level ops
                    if (ba.method_ident == self.root_env.idents.is_gt) {
                        const result = try self.compareNumericValues(lhs, rhs, .gt);
                        const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                        try value_stack.push(result_val);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.is_gte) {
                        const result = try self.compareNumericValues(lhs, rhs, .gte);
                        const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                        try value_stack.push(result_val);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.is_lt) {
                        const result = try self.compareNumericValues(lhs, rhs, .lt);
                        const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                        try value_stack.push(result_val);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.is_lte) {
                        const result = try self.compareNumericValues(lhs, rhs, .lte);
                        const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                        try value_stack.push(result_val);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.is_eq) {
                        const result = try self.compareNumericValues(lhs, rhs, .eq);
                        const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                        try value_stack.push(result_val);
                        return true;
                    }
                    // Handle numeric arithmetic via type-aware evalNumericBinop
                    if (ba.method_ident == self.root_env.idents.plus) {
                        const result = try self.evalNumericBinop(.add, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.minus) {
                        const result = try self.evalNumericBinop(.sub, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.times) {
                        const result = try self.evalNumericBinop(.mul, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.div_by) {
                        const result = try self.evalNumericBinop(.div, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.div_trunc_by) {
                        const result = try self.evalNumericBinop(.div_trunc, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    } else if (ba.method_ident == self.root_env.idents.rem_by) {
                        const result = try self.evalNumericBinop(.rem, lhs, rhs, roc_ops);
                        try value_stack.push(result);
                        return true;
                    }
                }

                const nominal_info: ?struct { origin: base_pkg.Ident.Idx, ident: base_pkg.Ident.Idx } = switch (current_resolved.desc.content) {
                    .structure => |s| switch (s) {
                        .nominal_type => |nom| .{
                            .origin = nom.origin_module,
                            .ident = nom.ident.ident_idx,
                        },
                        .record, .tuple, .tag_union, .empty_record, .empty_tag_union => blk: {
                            // Anonymous structural types have implicit is_eq
                            if (ba.method_ident == self.root_env.idents.is_eq) {
                                var result = self.valuesStructurallyEqual(lhs, effective_receiver_rt_var, rhs, ba.rhs_rt_var, roc_ops) catch |err| {
                                    if (err == error.NotImplemented) {
                                        self.triggerCrash("Structural equality not implemented for this type", false, roc_ops);
                                        return error.Crash;
                                    }
                                    return err;
                                };
                                // For != operator, negate the result
                                if (ba.negate_result) result = !result;
                                const result_val = try self.makeBoolValue(result);
                                try value_stack.push(result_val);
                                return true;
                            }
                            break :blk null;
                        },
                        else => null,
                    },
                    // Flex, rigid, and error vars are unresolved type variables (e.g., numeric literals defaulting to Dec,
                    // or type parameters in generic functions). For is_eq, prefer a numeric scalar fast-path when we can
                    // prove the scalar is numeric; otherwise fall back to structural equality when the type is structural.
                    // Error types can occur during generic instantiation when types couldn't be resolved.
                    .flex, .rigid, .err => blk: {
                        if (ba.method_ident == self.root_env.idents.is_eq) {
                            // Numeric scalar fast-path:
                            // Only use layout-based scalar comparison when both sides are scalar *and*
                            // the scalar tag is numeric (int/frac). This keeps the optimization
                            // for numeric flex vars while avoiding crashes for non-numeric scalars
                            // like strings.
                            if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
                                const lhs_tag = lhs.layout.data.scalar.tag;
                                const rhs_tag = rhs.layout.data.scalar.tag;

                                const lhs_is_numeric = lhs_tag == .int or lhs_tag == .frac;
                                const rhs_is_numeric = rhs_tag == .int or rhs_tag == .frac;

                                if (lhs_is_numeric and rhs_is_numeric) {
                                    const order = self.compareNumericScalars(lhs, rhs) catch {
                                        self.triggerCrash("Failed to compare numeric scalars (flex/rigid is_eq numeric scalar fast-path)", false, roc_ops);
                                        return error.Crash;
                                    };
                                    var result = (order == .eq);
                                    if (ba.negate_result) result = !result;
                                    const result_val = try self.makeBoolValue(result);
                                    try value_stack.push(result_val);
                                    return true;
                                }
                            }

                            // For non-scalar types, we need rt_var to dispatch to the type's is_eq method.
                            // Values must have rt_var set by the code that created them.
                            const resolved = self.runtime_types.resolveVar(lhs.rt_var);
                            if (resolved.desc.content == .structure) {
                                if (resolved.desc.content.structure == .nominal_type) {
                                    const nom = resolved.desc.content.structure.nominal_type;
                                    break :blk .{
                                        .origin = nom.origin_module,
                                        .ident = nom.ident.ident_idx,
                                    };
                                }
                            }

                            // Structural equality using effective_receiver_rt_var for proper type tracking
                            var result = self.valuesStructurallyEqual(lhs, effective_receiver_rt_var, rhs, ba.rhs_rt_var, roc_ops) catch |err| {
                                if (err == error.NotImplemented) {
                                    self.triggerCrash("Structural equality not implemented for this type", false, roc_ops);
                                    return error.Crash;
                                }
                                return err;
                            };
                            // For != operator, negate the result
                            if (ba.negate_result) result = !result;
                            const result_val = try self.makeBoolValue(result);
                            try value_stack.push(result_val);
                            return true;
                        }

                        // For non-is_eq binary ops on flex types, we cannot dispatch without
                        // a concrete type. The binary op setup code (e_binop handling) should have
                        // already unified flex vars with Dec before reaching here.
                        break :blk null;
                    },
                    else => null,
                };

                if (nominal_info == null) {
                    // Before failing, check if this is a numeric operation we can handle directly
                    if (is_numeric_layout) {
                        // Handle numeric arithmetic via type-aware evalNumericBinop as fallback
                        if (ba.method_ident == self.root_env.idents.plus) {
                            const result = try self.evalNumericBinop(.add, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.minus) {
                            const result = try self.evalNumericBinop(.sub, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.times) {
                            const result = try self.evalNumericBinop(.mul, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.div_by) {
                            const result = try self.evalNumericBinop(.div, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.div_trunc_by) {
                            const result = try self.evalNumericBinop(.div_trunc, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.rem_by) {
                            const result = try self.evalNumericBinop(.rem, lhs, rhs, roc_ops);
                            try value_stack.push(result);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.is_gt) {
                            const result = try self.compareNumericValues(lhs, rhs, .gt);
                            const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                            try value_stack.push(result_val);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.is_gte) {
                            const result = try self.compareNumericValues(lhs, rhs, .gte);
                            const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                            try value_stack.push(result_val);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.is_lt) {
                            const result = try self.compareNumericValues(lhs, rhs, .lt);
                            const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                            try value_stack.push(result_val);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.is_lte) {
                            const result = try self.compareNumericValues(lhs, rhs, .lte);
                            const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                            try value_stack.push(result_val);
                            return true;
                        } else if (ba.method_ident == self.root_env.idents.is_eq) {
                            const result = try self.compareNumericValues(lhs, rhs, .eq);
                            const result_val = try self.makeBoolValue(if (ba.negate_result) !result else result);
                            try value_stack.push(result_val);
                            return true;
                        }
                    }
                    return error.InvalidMethodReceiver;
                }

                // Resolve the method function
                const method_func = try self.resolveMethodFunction(
                    nominal_info.?.origin,
                    nominal_info.?.ident,
                    ba.method_ident,
                    roc_ops,
                    effective_receiver_rt_var,
                );
                defer method_func.decref(&self.runtime_layout_store, roc_ops);

                // Call the method closure
                if (method_func.layout.tag != .closure) {
                    return error.TypeMismatch;
                }

                const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                // Switch to the closure's source module
                const saved_env = self.env;
                const saved_bindings_len = self.bindings.items.len;
                self.env = @constCast(closure_header.source_env);

                // Check if this is a low-level lambda
                const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                if (lambda_expr == .e_low_level_lambda) {
                    const low_level = lambda_expr.e_low_level_lambda;
                    var args = [2]StackValue{ lhs, rhs };
                    var result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);

                    // Decref arguments based on ownership semantics
                    const arg_ownership = low_level.op.getArgOwnership();
                    for (args, 0..) |arg, arg_idx| {
                        const ownership = if (arg_idx < arg_ownership.len) arg_ownership[arg_idx] else .borrow;
                        if (ownership == .borrow) {
                            arg.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    self.env = saved_env;

                    // For != operator, negate boolean result
                    if (ba.negate_result) {
                        const is_eq_result = self.boolValueEquals(true, result);
                        result.decref(&self.runtime_layout_store, roc_ops);
                        result = try self.makeBoolValue(!is_eq_result);
                    }

                    try value_stack.push(result);
                    return true;
                }

                // Regular closure invocation
                const params = self.env.store.slicePatterns(closure_header.params);
                if (params.len != 2) {
                    self.env = saved_env;
                    return error.TypeMismatch;
                }

                // Provide closure context
                try self.active_closures.append(method_func);

                // Save the current flex_type_context before adding parameter mappings.
                // This will be restored in call_cleanup.
                var saved_flex_type_context = try self.flex_type_context.clone();
                errdefer saved_flex_type_context.deinit();

                // Set up flex_type_context for polymorphic type propagation.
                // This is critical for generic methods like List.is_eq where the element
                // type parameter needs to be mapped to the concrete type of the arguments.
                // We need to map both the parameter type AND any type parameters within it.
                // Use effective_receiver_rt_var computed earlier, rhs.rt_var is always set
                const arg_rt_vars = [2]types.Var{ effective_receiver_rt_var, rhs.rt_var };
                for (params, 0..) |param, idx| {
                    const arg_rt_resolved = self.runtime_types.resolveVar(arg_rt_vars[idx]);
                    // Only add mapping if the argument has a concrete type (structure)
                    if (arg_rt_resolved.desc.content == .structure) {
                        const param_ct_var = can.ModuleEnv.varFrom(param);
                        const param_resolved = self.env.types.resolveVar(param_ct_var);
                        const flex_key = ModuleVarKey{ .module = self.env, .var_ = param_resolved.var_ };
                        try self.flex_type_context.put(flex_key, arg_rt_vars[idx]);

                        // For nominal types (like List), also map the type parameters.
                        // E.g., for List(item) called with List(List(Dec)), map item → List(Dec)
                        if (arg_rt_resolved.desc.content.structure == .nominal_type) {
                            const rt_nom = arg_rt_resolved.desc.content.structure.nominal_type;
                            const rt_vars = self.runtime_types.sliceVars(rt_nom.vars.nonempty);

                            // Get compile-time type parameters
                            if (param_resolved.desc.content == .structure) {
                                if (param_resolved.desc.content.structure == .nominal_type) {
                                    const ct_nom = param_resolved.desc.content.structure.nominal_type;
                                    const ct_vars = self.env.types.sliceVars(ct_nom.vars.nonempty);

                                    // Map each CT type parameter to its corresponding RT type
                                    // vars[0] is the backing var, vars[1..] are the type params
                                    var i: usize = 1;
                                    while (i < ct_vars.len and i < rt_vars.len) : (i += 1) {
                                        const ct_param_resolved = self.env.types.resolveVar(ct_vars[i]);
                                        const ct_param_key = ModuleVarKey{ .module = self.env, .var_ = ct_param_resolved.var_ };
                                        try self.flex_type_context.put(ct_param_key, rt_vars[i]);
                                    }
                                }
                            }
                        }
                    }
                }

                // Bind parameters using patternMatchesBind to properly handle ownership.
                // patternMatchesBind creates copies via pushCopy, so the deferred decrefs
                // of lhs/rhs at the function start will correctly free the originals while
                // the bindings retain their own references.
                // Use effective rt_vars from values if available.
                // expr_idx not used for binary operator method parameter bindings
                if (!try self.patternMatchesBind(params[0], lhs, effective_receiver_rt_var, roc_ops, &self.bindings, null)) {
                    self.flex_type_context.deinit();
                    self.flex_type_context = saved_flex_type_context;
                    self.env = saved_env;
                    _ = self.active_closures.pop();
                    return error.TypeMismatch;
                }
                if (!try self.patternMatchesBind(params[1], rhs, rhs.rt_var, roc_ops, &self.bindings, null)) {
                    // Clean up the first binding we added
                    self.trimBindingList(&self.bindings, saved_bindings_len, roc_ops);
                    self.flex_type_context.deinit();
                    self.flex_type_context = saved_flex_type_context;
                    self.env = saved_env;
                    _ = self.active_closures.pop();
                    return error.TypeMismatch;
                }

                // Push cleanup and evaluate body
                // Push negate_bool first (executed last) if this is != operator
                if (ba.negate_result) {
                    try work_stack.push(.{ .apply_continuation = .{ .negate_bool = {} } });
                }
                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = 2,
                    .has_active_closure = true,
                    .did_instantiate = false,
                    .call_ret_rt_var = null,
                    .saved_rigid_subst = null,
                    .saved_flex_type_context = saved_flex_type_context,
                    .arg_rt_vars_to_free = null,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = closure_header.body_idx,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .dot_access_resolve => |da| {
                // Dot access: receiver is on stack, resolve field or method
                const receiver_value = value_stack.pop() orelse return error.Crash;

                if (da.method_args == null) {
                    // Field access on a record
                    defer receiver_value.decref(&self.runtime_layout_store, roc_ops);

                    if (receiver_value.layout.tag != .record) {
                        return error.TypeMismatch;
                    }

                    const rec_data = self.runtime_layout_store.getRecordData(receiver_value.layout.data.record.idx);
                    if (rec_data.fields.count == 0) {
                        return error.TypeMismatch;
                    }

                    var accessor = try receiver_value.asRecord(&self.runtime_layout_store);
                    const field_idx = accessor.findFieldIndex(da.field_name) orelse return error.TypeMismatch;

                    // Get the field's rt_var from the receiver's record type
                    const receiver_resolved = self.runtime_types.resolveVar(receiver_value.rt_var);
                    const field_rt_var = blk: {
                        if (receiver_resolved.desc.content == .structure) {
                            const flat = receiver_resolved.desc.content.structure;
                            const fields_range = switch (flat) {
                                .record => |rec| rec.fields,
                                .record_unbound => |fields| fields,
                                else => break :blk try self.runtime_types.fresh(),
                            };
                            const fields = self.runtime_types.getRecordFieldsSlice(fields_range);
                            var i: usize = 0;
                            while (i < fields.len) : (i += 1) {
                                const f = fields.get(i);
                                if (f.name == da.field_name) {
                                    break :blk f.var_;
                                }
                            }
                        }
                        break :blk try self.runtime_types.fresh();
                    };

                    const field_value = try accessor.getFieldByIndex(field_idx, field_rt_var);
                    const result = try self.pushCopy(field_value);
                    try value_stack.push(result);
                    return true;
                }

                // Method call - resolve receiver type for dispatch
                // Always prefer the runtime type from the evaluated value,
                // as it's more accurate than the compile-time type (which may be incorrectly inferred)
                const effective_receiver_rt_var = receiver_value.rt_var;

                // Don't use resolveBaseVar here - we need to keep the nominal type
                // for method dispatch (resolveBaseVar unwraps nominal types to their backing)
                const resolved_receiver = self.runtime_types.resolveVar(effective_receiver_rt_var);

                const method_args = da.method_args.?;
                const arg_exprs = self.env.store.sliceExpr(method_args);

                // Get nominal type info
                const nominal_info = switch (resolved_receiver.desc.content) {
                    .structure => |s| switch (s) {
                        .nominal_type => |nom| .{
                            .origin = nom.origin_module,
                            .ident = nom.ident.ident_idx,
                        },
                        else => {
                            receiver_value.decref(&self.runtime_layout_store, roc_ops);
                            return error.InvalidMethodReceiver;
                        },
                    },
                    else => {
                        receiver_value.decref(&self.runtime_layout_store, roc_ops);
                        return error.InvalidMethodReceiver;
                    },
                };

                // Resolve the method function
                const method_func = self.resolveMethodFunction(
                    nominal_info.origin,
                    nominal_info.ident,
                    da.field_name,
                    roc_ops,
                    effective_receiver_rt_var,
                ) catch |err| {
                    receiver_value.decref(&self.runtime_layout_store, roc_ops);
                    if (err == error.MethodLookupFailed) {
                        const layout_env = self.runtime_layout_store.env;
                        const type_name = import_mapping_mod.getDisplayName(
                            self.import_mapping,
                            layout_env.common.getIdentStore(),
                            nominal_info.ident,
                        );
                        const method_name = self.env.getIdent(da.field_name);
                        const crash_msg = std.fmt.allocPrint(self.allocator, "{s} does not implement {s}", .{ type_name, method_name }) catch {
                            self.triggerCrash("Method not found", false, roc_ops);
                            return error.Crash;
                        };
                        self.triggerCrash(crash_msg, true, roc_ops);
                        return error.Crash;
                    }
                    return err;
                };

                if (method_func.layout.tag != .closure) {
                    receiver_value.decref(&self.runtime_layout_store, roc_ops);
                    method_func.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }

                // If no additional args, invoke method directly with receiver
                if (arg_exprs.len == 0) {
                    const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                    const saved_env = self.env;
                    const saved_bindings_len = self.bindings.items.len;
                    self.env = @constCast(closure_header.source_env);

                    // Check if low-level lambda
                    const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                    if (lambda_expr == .e_low_level_lambda) {
                        const low_level = lambda_expr.e_low_level_lambda;
                        var args = [1]StackValue{receiver_value};
                        // Get return type from the dot access expression for low-level builtins that need it
                        const return_ct_var = can.ModuleEnv.varFrom(da.expr_idx);
                        const return_rt_var = try self.translateTypeVar(self.env, return_ct_var);
                        const result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, return_rt_var);

                        // Decref based on ownership semantics
                        const arg_ownership = low_level.op.getArgOwnership();
                        if (arg_ownership.len > 0 and arg_ownership[0] == .borrow) {
                            receiver_value.decref(&self.runtime_layout_store, roc_ops);
                        }

                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        self.env = saved_env;
                        try value_stack.push(result);
                        return true;
                    }

                    const params = self.env.store.slicePatterns(closure_header.params);
                    if (params.len != 1) {
                        self.env = saved_env;
                        receiver_value.decref(&self.runtime_layout_store, roc_ops);
                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        return error.TypeMismatch;
                    }

                    try self.active_closures.append(method_func);
                    try self.bindings.append(.{
                        .pattern_idx = params[0],
                        .value = receiver_value,
                        .expr_idx = null, // expr_idx not used for field access method parameter bindings
                        .source_env = self.env,
                    });

                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = 1,
                        .has_active_closure = true,
                        .did_instantiate = false,
                        .call_ret_rt_var = null,
                        .saved_rigid_subst = null,
                        .saved_flex_type_context = null,
                        .arg_rt_vars_to_free = null,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = closure_header.body_idx,
                        .expected_rt_var = null,
                    } });
                    return true;
                }

                // Has additional args - need to collect them first
                // Push receiver back on stack, then method function, then collect args
                try value_stack.push(receiver_value);
                try value_stack.push(method_func);

                try work_stack.push(.{ .apply_continuation = .{ .dot_access_collect_args = .{
                    .method_name = da.field_name,
                    .collected_count = 0,
                    .remaining_args = arg_exprs,
                    .receiver_rt_var = da.receiver_rt_var,
                    .expr_idx = da.expr_idx,
                } } });

                // Start evaluating first arg
                // For static dispatch methods like I64.to_str(x), use the receiver type
                // as the expected type for the first argument. This enables proper type
                // inference for polymorphic numeric literals.
                // Note: This assumes methods take their receiver type as first arg, which
                // is true for common patterns like I64.to_str. For multi-arg methods,
                // only the first arg gets this treatment.
                const first_arg_ct_var = can.ModuleEnv.varFrom(arg_exprs[0]);
                const first_arg_rt_var = try self.translateTypeVar(self.env, first_arg_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = arg_exprs[0],
                    .expected_rt_var = first_arg_rt_var,
                } });
                return true;
            },
            .dot_access_collect_args => |dac| {
                // Dot access method call: collecting arguments
                // Stack: [receiver, method_func, arg0, arg1, ...]
                if (dac.remaining_args.len > 1) {
                    // More arguments to evaluate
                    try work_stack.push(.{ .apply_continuation = .{ .dot_access_collect_args = .{
                        .method_name = dac.method_name,
                        .collected_count = dac.collected_count + 1,
                        .remaining_args = dac.remaining_args[1..],
                        .receiver_rt_var = dac.receiver_rt_var,
                        .expr_idx = dac.expr_idx,
                    } } });

                    // Translate argument type
                    const next_arg_ct_var = can.ModuleEnv.varFrom(dac.remaining_args[1]);
                    const next_arg_rt_var = try self.translateTypeVar(self.env, next_arg_ct_var);
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = dac.remaining_args[1],
                        .expected_rt_var = next_arg_rt_var,
                    } });
                    return true;
                }

                // All arguments collected - invoke method
                const total_args = dac.collected_count + 1; // +1 for the last arg we just got

                // Pop arguments (last evaluated on top)
                var arg_values = try self.allocator.alloc(StackValue, total_args);
                defer self.allocator.free(arg_values);
                var i: usize = total_args;
                while (i > 0) {
                    i -= 1;
                    arg_values[i] = value_stack.pop() orelse return error.Crash;
                }

                // Pop method function and receiver
                const method_func = value_stack.pop() orelse return error.Crash;
                const receiver_value = value_stack.pop() orelse return error.Crash;

                const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                const saved_env = self.env;
                const saved_bindings_len = self.bindings.items.len;
                self.env = @constCast(closure_header.source_env);

                // Check if low-level lambda
                const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                if (lambda_expr == .e_low_level_lambda) {
                    const low_level = lambda_expr.e_low_level_lambda;

                    // Special handling for list_sort_with which requires continuation-based evaluation
                    if (low_level.op == .list_sort_with) {
                        std.debug.assert(total_args == 1);
                        const list_arg = receiver_value;
                        const compare_fn = arg_values[0];

                        // Restore environment before setting up sort (helper saves env for comparison cleanup)
                        self.env = saved_env;
                        method_func.decref(&self.runtime_layout_store, roc_ops);

                        switch (try self.setupSortWith(list_arg, compare_fn, null, null, roc_ops, work_stack)) {
                            .already_sorted => |result_list| {
                                compare_fn.decref(&self.runtime_layout_store, roc_ops);
                                try value_stack.push(result_list);
                            },
                            .sorting_started => {},
                        }
                        return true;
                    }

                    // Build args array: receiver + explicit args
                    var all_args = try self.allocator.alloc(StackValue, 1 + total_args);
                    defer self.allocator.free(all_args);
                    all_args[0] = receiver_value;
                    for (arg_values, 0..) |arg, idx| {
                        all_args[1 + idx] = arg;
                    }

                    // Get return type from the dot access expression for low-level builtins that need it
                    const return_ct_var = can.ModuleEnv.varFrom(dac.expr_idx);
                    const return_rt_var = try self.translateTypeVar(self.env, return_ct_var);
                    const result = try self.callLowLevelBuiltin(low_level.op, all_args, roc_ops, return_rt_var);

                    // Decref arguments based on ownership semantics
                    const arg_ownership = low_level.op.getArgOwnership();
                    for (all_args, 0..) |arg, arg_idx| {
                        const ownership = if (arg_idx < arg_ownership.len) arg_ownership[arg_idx] else .borrow;
                        if (ownership == .borrow) {
                            arg.decref(&self.runtime_layout_store, roc_ops);
                        }
                    }

                    method_func.decref(&self.runtime_layout_store, roc_ops);
                    self.env = saved_env;
                    try value_stack.push(result);
                    return true;
                }

                // Regular closure invocation
                const params = self.env.store.slicePatterns(closure_header.params);
                const expected_params = 1 + total_args;
                if (params.len != expected_params) {
                    self.env = saved_env;
                    receiver_value.decref(&self.runtime_layout_store, roc_ops);
                    for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                    method_func.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }

                // Instantiate the method's type parameters for polymorphic dispatch.
                // This is necessary so that when pattern matching extracts payloads from
                // generic types like Try(ok, err), the rigid type variables (ok, err) are
                // properly substituted with the concrete types from the call site.
                const lambda_ct_var = can.ModuleEnv.varFrom(closure_header.lambda_expr_idx);
                const lambda_rt_var = try self.translateTypeVar(self.env, lambda_ct_var);
                const lambda_resolved = self.runtime_types.resolveVar(lambda_rt_var);

                const should_instantiate_method = lambda_resolved.desc.content == .structure and
                    (lambda_resolved.desc.content.structure == .fn_pure or
                        lambda_resolved.desc.content.structure == .fn_effectful or
                        lambda_resolved.desc.content.structure == .fn_unbound);

                var method_subst_map = std.AutoHashMap(types.Var, types.Var).init(self.allocator);
                defer method_subst_map.deinit();

                var saved_rigid_subst: ?std.AutoHashMap(types.Var, types.Var) = null;
                var did_instantiate = false;

                if (should_instantiate_method) {
                    // Instantiate the method type (replaces rigid vars with fresh flex vars)
                    _ = try self.instantiateType(lambda_rt_var, &method_subst_map);

                    // Map the fresh flex vars to concrete types from the receiver.
                    const recv_type_resolved = self.runtime_types.resolveVar(dac.receiver_rt_var);
                    if (recv_type_resolved.desc.content == .structure and
                        recv_type_resolved.desc.content.structure == .nominal_type)
                    {
                        const receiver_nom = recv_type_resolved.desc.content.structure.nominal_type;
                        const receiver_args = self.runtime_types.sliceNominalArgs(receiver_nom);

                        const fn_args = switch (lambda_resolved.desc.content.structure) {
                            .fn_pure => |f| self.runtime_types.sliceVars(f.args),
                            .fn_effectful => |f| self.runtime_types.sliceVars(f.args),
                            .fn_unbound => |f| self.runtime_types.sliceVars(f.args),
                            else => &[_]types.Var{},
                        };

                        if (fn_args.len > 0) {
                            const first_param_resolved = self.runtime_types.resolveVar(fn_args[0]);
                            if (first_param_resolved.desc.content == .structure and
                                first_param_resolved.desc.content.structure == .nominal_type)
                            {
                                const param_nom = first_param_resolved.desc.content.structure.nominal_type;
                                const param_args = self.runtime_types.sliceNominalArgs(param_nom);

                                const min_args = @min(param_args.len, receiver_args.len);
                                for (0..min_args) |arg_idx| {
                                    const param_arg_resolved = self.runtime_types.resolveVar(param_args[arg_idx]);
                                    if (param_arg_resolved.desc.content == .rigid) {
                                        try method_subst_map.put(param_arg_resolved.var_, receiver_args[arg_idx]);
                                    }
                                }
                            }
                        }
                    }

                    // Save and update rigid_subst
                    saved_rigid_subst = try self.rigid_subst.clone();
                    var subst_iter = method_subst_map.iterator();
                    while (subst_iter.next()) |entry| {
                        try self.rigid_subst.put(entry.key_ptr.*, entry.value_ptr.*);
                    }
                    @memset(self.var_to_layout_slot.items, 0);
                    did_instantiate = true;
                }

                try self.active_closures.append(method_func);

                // Bind receiver first
                try self.bindings.append(.{
                    .pattern_idx = params[0],
                    .value = receiver_value,
                    .expr_idx = null, // expr_idx not used for method call parameter bindings
                    .source_env = self.env,
                });

                // Bind explicit arguments
                for (arg_values, 0..) |arg, idx| {
                    try self.bindings.append(.{
                        .pattern_idx = params[1 + idx],
                        .value = arg,
                        .expr_idx = null, // expr_idx not used for method call parameter bindings
                        .source_env = self.env,
                    });
                }

                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = expected_params,
                    .has_active_closure = true,
                    .did_instantiate = did_instantiate,
                    .call_ret_rt_var = null,
                    .saved_rigid_subst = saved_rigid_subst,
                    .saved_flex_type_context = null,
                    .arg_rt_vars_to_free = null,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = closure_header.body_idx,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .for_iterate => |fl_in| {
                // For loop/expression iteration: list has been evaluated, start iterating
                const list_value = value_stack.pop() orelse {
                    self.triggerCrash("for_iterate: value_stack empty", false, roc_ops);
                    return error.Crash;
                };

                // Get the list layout
                if (list_value.layout.tag != .list) {
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }
                const elem_layout_idx = list_value.layout.data.list;
                const elem_layout = self.runtime_layout_store.getLayout(elem_layout_idx);
                const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));

                // Get the RocList header
                const list_header: *const RocList = @ptrCast(@alignCast(list_value.ptr.?));
                const list_len = list_header.len();

                // Create the proper for_iterate with list info filled in
                var fl = fl_in;
                fl.list_value = list_value;
                fl.list_len = list_len;
                fl.elem_size = elem_size;
                fl.elem_layout = elem_layout;

                // If list is empty, handle completion
                if (list_len == 0) {
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    try self.handleForLoopComplete(work_stack, value_stack, fl.stmt_context, fl.bindings_start, roc_ops);
                    return true;
                }

                // Process first element
                const elem_ptr = if (list_header.bytes) |buffer|
                    buffer
                else {
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                };

                var elem_value = StackValue{
                    .ptr = elem_ptr,
                    .layout = elem_layout,
                    .is_initialized = true,
                    .rt_var = fl.patt_rt_var,
                };
                elem_value.incref(&self.runtime_layout_store);

                // Bind the pattern
                const loop_bindings_start = self.bindings.items.len;
                // expr_idx not used for for-loop pattern bindings
                if (!try self.patternMatchesBind(fl.pattern, elem_value, fl.patt_rt_var, roc_ops, &self.bindings, null)) {
                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }
                elem_value.decref(&self.runtime_layout_store, roc_ops);

                // Push body_done continuation
                try work_stack.push(.{ .apply_continuation = .{ .for_body_done = .{
                    .list_value = fl.list_value,
                    .current_index = 0,
                    .list_len = fl.list_len,
                    .elem_size = fl.elem_size,
                    .elem_layout = fl.elem_layout,
                    .pattern = fl.pattern,
                    .patt_rt_var = fl.patt_rt_var,
                    .body = fl.body,
                    .bindings_start = fl.bindings_start,
                    .loop_bindings_start = loop_bindings_start,
                    .stmt_context = fl.stmt_context,
                } } });

                // Evaluate body
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = fl.body,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .for_body_done => |fl| {
                // For loop/expression body completed, clean up and continue to next iteration
                const body_result = value_stack.pop() orelse {
                    self.triggerCrash("for_body_done: value_stack empty", false, roc_ops);
                    return error.Crash;
                };
                body_result.decref(&self.runtime_layout_store, roc_ops);

                // Clean up bindings for this iteration
                self.trimBindingList(&self.bindings, fl.loop_bindings_start, roc_ops);

                // Move to next element
                const next_index = fl.current_index + 1;
                if (next_index >= fl.list_len) {
                    // Loop complete
                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                    try self.handleForLoopComplete(work_stack, value_stack, fl.stmt_context, fl.bindings_start, roc_ops);
                    return true;
                }

                // Get next element
                const list_header: *const RocList = @ptrCast(@alignCast(fl.list_value.ptr.?));
                const elem_ptr = if (list_header.bytes) |buffer|
                    buffer + next_index * fl.elem_size
                else {
                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                };

                var elem_value = StackValue{
                    .ptr = elem_ptr,
                    .layout = fl.elem_layout,
                    .is_initialized = true,
                    .rt_var = fl.patt_rt_var,
                };
                elem_value.incref(&self.runtime_layout_store);

                // Bind the pattern
                const new_loop_bindings_start = self.bindings.items.len;
                // expr_idx not used for for-loop pattern bindings
                if (!try self.patternMatchesBind(fl.pattern, elem_value, fl.patt_rt_var, roc_ops, &self.bindings, null)) {
                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }
                elem_value.decref(&self.runtime_layout_store, roc_ops);

                // Push body_done continuation for next iteration
                try work_stack.push(.{ .apply_continuation = .{ .for_body_done = .{
                    .list_value = fl.list_value,
                    .current_index = next_index,
                    .list_len = fl.list_len,
                    .elem_size = fl.elem_size,
                    .elem_layout = fl.elem_layout,
                    .pattern = fl.pattern,
                    .patt_rt_var = fl.patt_rt_var,
                    .body = fl.body,
                    .bindings_start = fl.bindings_start,
                    .loop_bindings_start = new_loop_bindings_start,
                    .stmt_context = fl.stmt_context,
                } } });

                // Evaluate body
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = fl.body,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .while_loop_check => |wl| {
                // While loop: condition has been evaluated
                const cond_value = value_stack.pop() orelse return error.Crash;
                const cond_is_true = self.boolValueEquals(true, cond_value);

                if (!cond_is_true) {
                    // Loop complete, continue with remaining statements
                    if (wl.remaining_stmts.len == 0) {
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = wl.final_expr,
                            .expected_rt_var = null,
                        } });
                    } else {
                        const next_stmt = self.env.store.getStatement(wl.remaining_stmts[0]);
                        try self.scheduleNextStatement(work_stack, next_stmt, wl.remaining_stmts[1..], wl.final_expr, wl.bindings_start, null, roc_ops);
                    }
                    return true;
                }

                // Push body_done continuation
                try work_stack.push(.{ .apply_continuation = .{ .while_loop_body_done = .{
                    .cond = wl.cond,
                    .body = wl.body,
                    .remaining_stmts = wl.remaining_stmts,
                    .final_expr = wl.final_expr,
                    .bindings_start = wl.bindings_start,
                } } });

                // Evaluate body
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = wl.body,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .while_loop_body_done => |wl| {
                // While loop body completed, check condition again
                const body_result = value_stack.pop() orelse return error.Crash;
                body_result.decref(&self.runtime_layout_store, roc_ops);

                // Push check continuation for next iteration
                try work_stack.push(.{ .apply_continuation = .{ .while_loop_check = .{
                    .cond = wl.cond,
                    .body = wl.body,
                    .remaining_stmts = wl.remaining_stmts,
                    .final_expr = wl.final_expr,
                    .bindings_start = wl.bindings_start,
                } } });

                // Evaluate condition
                const cond_ct_var = can.ModuleEnv.varFrom(wl.cond);
                const cond_rt_var = try self.translateTypeVar(self.env, cond_ct_var);
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = wl.cond,
                    .expected_rt_var = cond_rt_var,
                } });
                return true;
            },
            .expect_check_stmt => |ec| {
                // Expect statement: check condition result
                const cond_val = value_stack.pop() orelse return error.Crash;
                const is_true = self.boolValueEquals(true, cond_val);
                if (!is_true) {
                    self.handleExpectFailure(ec.body_expr, roc_ops);
                    return error.Crash;
                }
                // Continue with remaining statements
                if (ec.remaining_stmts.len == 0) {
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ec.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    const next_stmt = self.env.store.getStatement(ec.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, ec.remaining_stmts[1..], ec.final_expr, ec.bindings_start, null, roc_ops);
                }
                return true;
            },
            .reassign_value => |rv| {
                // Reassign statement: update binding
                const new_val = value_stack.pop() orelse {
                    self.triggerCrash("reassign_value: value_stack empty", false, roc_ops);
                    return error.Crash;
                };
                // Search through all bindings and reassign
                var j: usize = self.bindings.items.len;
                while (j > 0) {
                    j -= 1;
                    if (self.bindings.items[j].pattern_idx == rv.pattern_idx) {
                        self.bindings.items[j].value.decref(&self.runtime_layout_store, roc_ops);
                        self.bindings.items[j].value = new_val;
                        break;
                    }
                }
                // Continue with remaining statements
                if (rv.remaining_stmts.len == 0) {
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = rv.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    const next_stmt = self.env.store.getStatement(rv.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, rv.remaining_stmts[1..], rv.final_expr, rv.bindings_start, null, roc_ops);
                }
                return true;
            },
            .dbg_print_stmt => |dp| {
                // Dbg statement: print value
                const value = value_stack.pop() orelse return error.Crash;
                defer value.decref(&self.runtime_layout_store, roc_ops);
                const rendered = try self.renderValueRocWithType(value, dp.rt_var, roc_ops);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                // Continue with remaining statements
                if (dp.remaining_stmts.len == 0) {
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = dp.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    const next_stmt = self.env.store.getStatement(dp.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, dp.remaining_stmts[1..], dp.final_expr, dp.bindings_start, null, roc_ops);
                }
                return true;
            },
            .inspect_render_stmt => |ir| {
                // Inspect statement: render value to Str (discarded in statement context)
                const value = value_stack.pop() orelse return error.Crash;

                // Check if the type is nominal and has a to_inspect method
                const resolved = self.runtime_types.resolveVar(ir.rt_var);
                const maybe_to_inspect: ?StackValue = if (resolved.desc.content == .structure)
                    switch (resolved.desc.content.structure) {
                        .nominal_type => |nom| try self.tryResolveMethodByIdent(
                            nom.origin_module,
                            nom.ident.ident_idx,
                            self.env.idents.to_inspect,
                            roc_ops,
                            ir.rt_var,
                        ),
                        else => null,
                    }
                else
                    null;

                if (maybe_to_inspect) |method_func| {
                    // Found to_inspect method - call it with the value
                    if (method_func.layout.tag != .closure) {
                        value.decref(&self.runtime_layout_store, roc_ops);
                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        // Fall back to default rendering
                        const rendered = try self.renderValueRocWithType(value, ir.rt_var, roc_ops);
                        self.allocator.free(rendered);
                        if (ir.remaining_stmts.len == 0) {
                            try work_stack.push(.{ .eval_expr = .{
                                .expr_idx = ir.final_expr,
                                .expected_rt_var = null,
                            } });
                        } else {
                            const next_stmt = self.env.store.getStatement(ir.remaining_stmts[0]);
                            try self.scheduleNextStatement(work_stack, next_stmt, ir.remaining_stmts[1..], ir.final_expr, ir.bindings_start, null, roc_ops);
                        }
                        return true;
                    }

                    const closure_header: *const layout.Closure = @ptrCast(@alignCast(method_func.ptr.?));

                    const saved_env = self.env;
                    const saved_bindings_len = self.bindings.items.len;
                    self.env = @constCast(closure_header.source_env);

                    // Check if low-level lambda
                    const lambda_expr = self.env.store.getExpr(closure_header.lambda_expr_idx);
                    if (lambda_expr == .e_low_level_lambda) {
                        const low_level = lambda_expr.e_low_level_lambda;
                        var args = [1]StackValue{value};
                        const result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);

                        const arg_ownership = low_level.op.getArgOwnership();
                        if (arg_ownership.len > 0 and arg_ownership[0] == .borrow) {
                            value.decref(&self.runtime_layout_store, roc_ops);
                        }

                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        result.decref(&self.runtime_layout_store, roc_ops); // Discard result
                        self.env = saved_env;

                        if (ir.remaining_stmts.len == 0) {
                            try work_stack.push(.{ .eval_expr = .{
                                .expr_idx = ir.final_expr,
                                .expected_rt_var = null,
                            } });
                        } else {
                            const next_stmt = self.env.store.getStatement(ir.remaining_stmts[0]);
                            try self.scheduleNextStatement(work_stack, next_stmt, ir.remaining_stmts[1..], ir.final_expr, ir.bindings_start, null, roc_ops);
                        }
                        return true;
                    }

                    const params = self.env.store.slicePatterns(closure_header.params);
                    if (params.len != 1) {
                        // to_inspect must take exactly one argument - fall back to default
                        self.env = saved_env;
                        value.decref(&self.runtime_layout_store, roc_ops);
                        method_func.decref(&self.runtime_layout_store, roc_ops);
                        const rendered = try self.renderValueRocWithType(value, ir.rt_var, roc_ops);
                        self.allocator.free(rendered);
                        if (ir.remaining_stmts.len == 0) {
                            try work_stack.push(.{ .eval_expr = .{
                                .expr_idx = ir.final_expr,
                                .expected_rt_var = null,
                            } });
                        } else {
                            const next_stmt = self.env.store.getStatement(ir.remaining_stmts[0]);
                            try self.scheduleNextStatement(work_stack, next_stmt, ir.remaining_stmts[1..], ir.final_expr, ir.bindings_start, null, roc_ops);
                        }
                        return true;
                    }

                    try self.active_closures.append(method_func);
                    try self.bindings.append(.{
                        .pattern_idx = params[0],
                        .value = value,
                        .expr_idx = null, // expr_idx not used for inspect method parameter bindings
                        .source_env = self.env,
                    });

                    // Schedule cleanup to discard result and continue after method call
                    try work_stack.push(.{ .apply_continuation = .{ .inspect_stmt_cleanup = .{
                        .remaining_stmts = ir.remaining_stmts,
                        .final_expr = ir.final_expr,
                        .bindings_start = ir.bindings_start,
                    } } });
                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = 1,
                        .has_active_closure = true,
                        .did_instantiate = false,
                        .call_ret_rt_var = null,
                        .saved_rigid_subst = null,
                        .saved_flex_type_context = null,
                        .arg_rt_vars_to_free = null,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = closure_header.body_idx,
                        .expected_rt_var = null,
                    } });
                    return true;
                }

                // No to_inspect method - use default rendering
                defer value.decref(&self.runtime_layout_store, roc_ops);
                const rendered = try self.renderValueRocWithType(value, ir.rt_var, roc_ops);
                self.allocator.free(rendered);
                // Continue with remaining statements
                if (ir.remaining_stmts.len == 0) {
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ir.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    const next_stmt = self.env.store.getStatement(ir.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, ir.remaining_stmts[1..], ir.final_expr, ir.bindings_start, null, roc_ops);
                }
                return true;
            },
            .inspect_stmt_cleanup => |isc| {
                // Discard the result from to_inspect method call and continue with statements
                const result = value_stack.pop() orelse return error.Crash;
                result.decref(&self.runtime_layout_store, roc_ops);

                // Continue with remaining statements
                if (isc.remaining_stmts.len == 0) {
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = isc.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    const next_stmt = self.env.store.getStatement(isc.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, isc.remaining_stmts[1..], isc.final_expr, isc.bindings_start, null, roc_ops);
                }
                return true;
            },
            .sort_compare_result => |sc_in| {
                var sc = sc_in;
                var saved_rigid_subst = sc.saved_rigid_subst;
                defer {
                    if (saved_rigid_subst) |saved| {
                        self.rigid_subst.deinit();
                        self.rigid_subst = saved;
                    }
                }

                // Process comparison result for insertion sort
                const cmp_result = value_stack.pop() orelse return error.Crash;
                defer cmp_result.decref(&self.runtime_layout_store, roc_ops);

                // Extract the comparison result (LT, EQ, GT tag)
                // LT = 0, EQ = 1, GT = 2 (alphabetical order)
                const is_less_than = blk: {
                    if (cmp_result.layout.tag == .scalar) {
                        // Tag union represented as a scalar (discriminant only)
                        const discriminant = cmp_result.asI128();
                        // Tag order is alphabetical: EQ=0, GT=1, LT=2
                        break :blk discriminant == 2; // LT
                    } else if (cmp_result.layout.tag == .tag_union) {
                        // Get discriminant from tag_union layout
                        const tu_data = self.runtime_layout_store.getTagUnionData(cmp_result.layout.data.tag_union.idx);
                        if (cmp_result.ptr) |ptr| {
                            const base_ptr: [*]u8 = @ptrCast(ptr);
                            const discriminant_ptr = base_ptr + tu_data.discriminant_offset;
                            const discriminant: u8 = discriminant_ptr[0];
                            // Tag order is alphabetical: EQ=0, GT=1, LT=2
                            break :blk discriminant == 2; // LT
                        }
                        break :blk false;
                    } else {
                        // Comparison result should always be .scalar or .tag_union
                        std.debug.panic("sort_compare_result: unexpected layout tag {s} for comparison result", .{@tagName(cmp_result.layout.tag)});
                    }
                };

                const working_list_ptr: *builtins.list.RocList = @ptrCast(@alignCast(sc.list_value.ptr.?));

                if (is_less_than) {
                    // Current element is less than compared element - swap them
                    const outer_ptr = working_list_ptr.bytes.? + sc.outer_index * sc.elem_size;
                    const inner_ptr = working_list_ptr.bytes.? + sc.inner_index * sc.elem_size;

                    // Swap elements
                    var temp_buffer: [256]u8 = undefined;
                    if (sc.elem_size <= 256) {
                        @memcpy(temp_buffer[0..sc.elem_size], outer_ptr[0..sc.elem_size]);
                        @memcpy(outer_ptr[0..sc.elem_size], inner_ptr[0..sc.elem_size]);
                        @memcpy(inner_ptr[0..sc.elem_size], temp_buffer[0..sc.elem_size]);
                    } else {
                        // For larger elements, allocate temp buffer
                        const temp = try self.allocator.alloc(u8, sc.elem_size);
                        defer self.allocator.free(temp);
                        @memcpy(temp, outer_ptr[0..sc.elem_size]);
                        @memcpy(outer_ptr[0..sc.elem_size], inner_ptr[0..sc.elem_size]);
                        @memcpy(inner_ptr[0..sc.elem_size], temp);
                    }

                    // Continue comparing at inner_index - 1 if possible
                    if (sc.inner_index > 0) {
                        const new_inner = sc.inner_index - 1;
                        const elem_at_inner = working_list_ptr.bytes.? + new_inner * sc.elem_size;
                        const elem_at_current = working_list_ptr.bytes.? + sc.inner_index * sc.elem_size;

                        const elem_inner_value = StackValue{
                            .layout = sc.elem_layout,
                            .ptr = @ptrCast(elem_at_inner),
                            .is_initialized = true,
                            .rt_var = sc.elem_rt_var,
                        };
                        const elem_current_value = StackValue{
                            .layout = sc.elem_layout,
                            .ptr = @ptrCast(elem_at_current),
                            .is_initialized = true,
                            .rt_var = sc.elem_rt_var,
                        };

                        // Copy elements for comparison
                        const arg0 = try self.pushCopy(elem_current_value);
                        const arg1 = try self.pushCopy(elem_inner_value);

                        // Push continuation for next comparison
                        // After swap, the element we're inserting is now at sc.inner_index
                        // so we track that as our new "outer" position
                        try work_stack.push(.{ .apply_continuation = .{ .sort_compare_result = .{
                            .list_value = sc.list_value,
                            .compare_fn = sc.compare_fn,
                            .call_ret_rt_var = sc.call_ret_rt_var,
                            .saved_rigid_subst = saved_rigid_subst,
                            .outer_index = sc.inner_index,
                            .inner_index = new_inner,
                            .list_len = sc.list_len,
                            .elem_size = sc.elem_size,
                            .elem_layout = sc.elem_layout,
                            .elem_rt_var = sc.elem_rt_var,
                        } } });
                        saved_rigid_subst = null;

                        // Invoke comparison function
                        const cmp_header: *const layout.Closure = @ptrCast(@alignCast(sc.compare_fn.ptr.?));
                        const cmp_saved_env = self.env;
                        self.env = @constCast(cmp_header.source_env);

                        const cmp_params = self.env.store.slicePatterns(cmp_header.params);

                        try self.active_closures.append(sc.compare_fn);

                        try self.bindings.append(.{
                            .pattern_idx = cmp_params[0],
                            .value = arg0,
                            .expr_idx = null, // expr_idx not used for comparison function parameter bindings
                            .source_env = self.env,
                        });
                        try self.bindings.append(.{
                            .pattern_idx = cmp_params[1],
                            .value = arg1,
                            .expr_idx = null, // expr_idx not used for comparison function parameter bindings
                            .source_env = self.env,
                        });

                        const bindings_start = self.bindings.items.len - 2;
                        try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                            .saved_env = cmp_saved_env,
                            .saved_bindings_len = bindings_start,
                            .param_count = 2,
                            .has_active_closure = true,
                            .did_instantiate = false,
                            .call_ret_rt_var = null,
                            .saved_rigid_subst = null,
                            .saved_flex_type_context = null,
                            .arg_rt_vars_to_free = null,
                        } } });
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = cmp_header.body_idx,
                            .expected_rt_var = null,
                        } });

                        return true;
                    }
                }

                // Element is in correct position or at start - move to next outer element
                const next_outer = sc.outer_index + 1;
                if (next_outer < sc.list_len) {
                    // Start comparing next element
                    const elem_at_outer = working_list_ptr.bytes.? + next_outer * sc.elem_size;
                    const elem_at_prev = working_list_ptr.bytes.? + (next_outer - 1) * sc.elem_size;

                    const elem_outer_value = StackValue{
                        .layout = sc.elem_layout,
                        .ptr = @ptrCast(elem_at_outer),
                        .is_initialized = true,
                        .rt_var = sc.elem_rt_var,
                    };
                    const elem_prev_value = StackValue{
                        .layout = sc.elem_layout,
                        .ptr = @ptrCast(elem_at_prev),
                        .is_initialized = true,
                        .rt_var = sc.elem_rt_var,
                    };

                    // Copy elements for comparison
                    const arg0 = try self.pushCopy(elem_outer_value);
                    const arg1 = try self.pushCopy(elem_prev_value);

                    // Push continuation for next comparison
                    try work_stack.push(.{ .apply_continuation = .{ .sort_compare_result = .{
                        .list_value = sc.list_value,
                        .compare_fn = sc.compare_fn,
                        .call_ret_rt_var = sc.call_ret_rt_var,
                        .saved_rigid_subst = saved_rigid_subst,
                        .outer_index = next_outer,
                        .inner_index = next_outer - 1,
                        .list_len = sc.list_len,
                        .elem_size = sc.elem_size,
                        .elem_layout = sc.elem_layout,
                        .elem_rt_var = sc.elem_rt_var,
                    } } });
                    saved_rigid_subst = null;

                    // Invoke comparison function
                    const cmp_header: *const layout.Closure = @ptrCast(@alignCast(sc.compare_fn.ptr.?));
                    const cmp_saved_env = self.env;
                    self.env = @constCast(cmp_header.source_env);

                    const cmp_params = self.env.store.slicePatterns(cmp_header.params);

                    try self.active_closures.append(sc.compare_fn);

                    try self.bindings.append(.{
                        .pattern_idx = cmp_params[0],
                        .value = arg0,
                        .expr_idx = null, // expr_idx not used for comparison function parameter bindings
                        .source_env = self.env,
                    });
                    try self.bindings.append(.{
                        .pattern_idx = cmp_params[1],
                        .value = arg1,
                        .expr_idx = null, // expr_idx not used for comparison function parameter bindings
                        .source_env = self.env,
                    });

                    const bindings_start = self.bindings.items.len - 2;
                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = cmp_saved_env,
                        .saved_bindings_len = bindings_start,
                        .param_count = 2,
                        .has_active_closure = true,
                        .did_instantiate = false,
                        .call_ret_rt_var = null,
                        .saved_rigid_subst = null,
                        .saved_flex_type_context = null,
                        .arg_rt_vars_to_free = null,
                    } } });
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = cmp_header.body_idx,
                        .expected_rt_var = null,
                    } });

                    return true;
                }

                // Sorting complete - return the sorted list
                sc.compare_fn.decref(&self.runtime_layout_store, roc_ops);
                if (saved_rigid_subst) |saved| {
                    self.rigid_subst.deinit();
                    self.rigid_subst = saved;
                    saved_rigid_subst = null;
                }
                if (sc.call_ret_rt_var) |rt_var| {
                    sc.list_value.rt_var = rt_var;
                }
                try value_stack.push(sc.list_value);
                return true;
            },
            .negate_bool => {
                // Negate the boolean result on top of value stack (for != operator)
                var result = value_stack.pop() orelse {
                    self.triggerCrash("negate_bool: expected value on stack", false, roc_ops);
                    return error.Crash;
                };
                const is_true = self.boolValueEquals(true, result);
                result.decref(&self.runtime_layout_store, roc_ops);
                const negated = try self.makeBoolValue(!is_true);
                try value_stack.push(negated);
                return true;
            },
        }
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
        .is_opaque = false,
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
        .is_opaque = false,
    };
    const ct_str = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });

    // backing type is Str for simplicity
    const ct_nominal_content = try env.types.mkNominal(type_ident, ct_str, &.{}, name_nominal, false);
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
        .is_opaque = false,
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
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
    try module_a.initCIRFields(module_a_name);

    // Set up Module B (the current module that imports Module A)
    var module_b = try can.ModuleEnv.init(gpa, module_b_name);
    defer module_b.deinit();
    try module_b.initCIRFields(module_b_name);

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
    var interp = try Interpreter.init(gpa, &module_b, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
    defer interp.deinit();

    // Register module A as an imported module
    const module_a_ident = try module_b.common.idents.insert(gpa, @import("base").Ident.for_text(module_a_name));
    try interp.module_envs.put(interp.allocator, module_a_ident, &module_a);
    const module_a_id: u32 = 1;
    try interp.module_ids.put(interp.allocator, module_a_ident, module_a_id);

    // Create an Import.Idx for module A
    // Using first import index for test purposes
    const first_import_idx: can.CIR.Import.Idx = .first;
    try interp.import_envs.put(interp.allocator, first_import_idx, &module_a);

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
    try module_a.initCIRFields(module_a_name);

    var module_b = try can.ModuleEnv.init(gpa, module_b_name);
    defer module_b.deinit();
    try module_b.initCIRFields(module_b_name);

    var module_c = try can.ModuleEnv.init(gpa, module_c_name);
    defer module_c.deinit();
    try module_c.initCIRFields(module_c_name);

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
    var interp = try Interpreter.init(gpa, &module_a, builtin_types_test, null, &[_]*const can.ModuleEnv{}, &empty_import_mapping, null);
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
    // Using sequential import indices for test purposes
    const first_import_idx: can.CIR.Import.Idx = .first;
    const second_import_idx: can.CIR.Import.Idx = @enumFromInt(1);
    try interp.import_envs.put(interp.allocator, first_import_idx, &module_b);
    try interp.import_envs.put(interp.allocator, second_import_idx, &module_c);

    // Verify we can retrieve all module environments
    try std.testing.expectEqual(module_b.module_name_idx, interp.getModuleEnvForOrigin(module_b_ident).?.module_name_idx);
    try std.testing.expectEqual(module_c.module_name_idx, interp.getModuleEnvForOrigin(module_c_ident).?.module_name_idx);

    // Verify we can retrieve all module IDs
    try std.testing.expectEqual(module_b_id, interp.getModuleIdForOrigin(module_b_ident));
    try std.testing.expectEqual(module_c_id, interp.getModuleIdForOrigin(module_c_ident));
}
