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

    /// Evaluates a Roc expression and returns the result.
    pub fn eval(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) Error!StackValue {
        return try self.evalWithExpectedType(expr_idx, roc_ops, null);
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
            const result_value = self.evalWithExpectedType(header.body_idx, roc_ops, null) catch |err| {
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

        const result = try self.eval(expr_idx, roc_ops);
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

    fn pushStr(self: *Interpreter) !StackValue {
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
        const result_value = try self.pushRaw(return_layout, 0);

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
                    var out = try self.pushRaw(result_layout, 0);
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
                var refcount_context = RefcountContext{
                    .layout_store = &self.runtime_layout_store,
                    .elem_layout = elem_layout,
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
                var out = try self.pushRaw(result_layout, 0);
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

                const value = try self.pushStr();
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

        const value = try self.pushStr();
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

        const value = try self.pushStr();
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
        var out = try self.pushRaw(to_layout, 0);
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
        var out = try self.pushRaw(to_layout, 0);
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
    fn intToFloat(self: *Interpreter, comptime From: type, comptime To: type, args: []const StackValue) !StackValue {
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
    fn intToDec(self: *Interpreter, comptime From: type, args: []const StackValue) !StackValue {
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
        roc_ops: *RocOps,
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
            .nominal_type => |nom| {
                // For nominal types, dispatch to their is_eq method
                return try self.dispatchNominalIsEq(lhs, rhs, nom, roc_ops);
            },
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
            const lhs_elem = try lhs_acc.getElement(index);
            const rhs_elem = try rhs_acc.getElement(index);
            const elems_equal = try self.valuesStructurallyEqual(lhs_elem, elem_vars[index], rhs_elem, elem_vars[index], roc_ops);
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
            const lhs_field = try lhs_rec.getFieldByIndex(idx);
            const rhs_field = try rhs_rec.getFieldByIndex(idx);
            const field_var = field_slice.items(.var_)[idx];
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
            const lhs_elem = try lhs_tuple.getElement(idx);
            const rhs_elem = try rhs_tuple.getElement(idx);
            const args_equal = try self.valuesStructurallyEqual(lhs_elem, arg_vars[idx], rhs_elem, arg_vars[idx], roc_ops);
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

        // Structural comparison of the backing type
        // This handles nominal types like Try that wrap tag unions
        const backing_var = self.runtime_types.getNominalBackingVar(nom);
        const backing_resolved = self.runtime_types.resolveVar(backing_var);

        if (backing_resolved.desc.content == .structure) {
            return self.valuesStructurallyEqual(lhs, backing_var, rhs, backing_var, roc_ops);
        }

        // For other cases, fall back to attempting scalar comparison
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
        return boolValueEquals(true, result);
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

    /// Translate a compile-time type variable from a module's type store to the runtime type store.
    /// Handles most structural types: tag unions, tuples, records, functions, and nominal types.
    /// Uses caching to handle recursive types and avoid duplicate work.
    pub fn translateTypeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var) Error!types.Var {
        const resolved = module.types.resolveVar(compile_var);

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
                            // record_unbound has no extension - it's a complete set of fields
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

        /// For loop - iterate over list elements after list is evaluated.
        for_loop_iterate: ForLoopIterate,

        /// For loop - process body result and continue to next iteration.
        for_loop_body_done: ForLoopBodyDone,

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

        pub const DecrefValue = struct {
            value: StackValue,
        };

        pub const TrimBindings = struct {
            target_len: usize,
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
        };

        pub const BlockContinue = struct {
            /// Remaining statements to process
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// The final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
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

        /// For loop - iterate over list elements
        pub const ForLoopIterate = struct {
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
            /// Remaining statements after the for loop
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
        };

        /// For loop - cleanup after body evaluation
        pub const ForLoopBodyDone = struct {
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
            /// Remaining statements after the for loop
            remaining_stmts: []const can.CIR.Statement.Idx,
            /// Final expression to evaluate after all statements
            final_expr: can.CIR.Expr.Idx,
            /// Bindings length at block start (for cleanup)
            bindings_start: usize,
            /// Bindings length at iteration start (for per-iteration cleanup)
            loop_bindings_start: usize,
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
                        return value_stack.pop() orelse return error.Crash;
                    }
                },
            }
        }

        // Should never reach here - return_result should have exited the loop
        return error.Crash;
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
                        },
                        .call_cleanup => |cc| {
                            if (cc.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                        },
                        .for_loop_iterate => |fl| {
                            // Decref the list value
                            fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                        },
                        .for_loop_body_done => |fl| {
                            // Decref the list value
                            fl.list_value.decref(&self.runtime_layout_store, roc_ops);
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
                    const value = try self.pushStr();
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
                const value = try self.evalLookupLocal(lookup, roc_ops);
                try value_stack.push(value);
            },

            .e_lookup_external => |lookup| {
                const value = try self.evalLookupExternal(lookup, expected_rt_var, roc_ops);
                try value_stack.push(value);
            },

            .e_lookup_required => {
                // Required lookups reference values from the app that provides values to the
                // platform's `requires` clause. These are not available during compile-time
                // evaluation.
                return error.TypeMismatch;
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
                        const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                        var lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                        const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                        const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

                        // Resolve the lhs type - if flex/rigid, default to Dec
                        const lhs_resolved = self.runtime_types.resolveVar(lhs_rt_var);
                        if (lhs_resolved.desc.content == .flex or lhs_resolved.desc.content == .rigid) {
                            const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                            const dec_var = try self.runtime_types.freshFromContent(dec_content);
                            lhs_rt_var = dec_var;
                        }

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
                    const value = try self.pushRaw(tuple_layout, 0);
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
                    const dest = try self.pushRaw(list_layout, 0);
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
                    const dest = try self.pushRaw(rec_layout, 0);
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
                // Compute the backing type variable for the nominal
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const nominal_rt_var = try self.translateTypeVar(self.env, ct_var);
                const nominal_resolved = self.runtime_types.resolveVar(nominal_rt_var);
                const backing_rt_var = if (nom.nominal_type_decl == self.builtins.bool_stmt)
                    try self.getCanonicalBoolRuntimeVar()
                else switch (nominal_resolved.desc.content) {
                    .structure => |st| switch (st) {
                        .nominal_type => |nt| self.runtime_types.getNominalBackingVar(nt),
                        else => nominal_rt_var,
                    },
                    else => nominal_rt_var,
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
                // Determine runtime type and tag index
                var rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                var resolved = self.resolveBaseVar(rt_var);
                // Handle flex types for True/False
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

                const tag_index = try self.findTagIndexByIdentInList(self.env, tag.name, tag_list.items) orelse {
                    const name_text = self.env.getIdent(tag.name);
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                };

                const layout_val = try self.getRuntimeLayout(rt_var);

                if (layout_val.tag == .scalar) {
                    // No payload union - just set discriminant
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        try out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        out.rt_var = rt_var;
                        try value_stack.push(out);
                    } else {
                        self.triggerCrash("e_tag: scalar layout is not int", false, roc_ops);
                        return error.Crash;
                    }
                } else if (layout_val.tag == .record or layout_val.tag == .tuple) {
                    const args_exprs = self.env.store.sliceExpr(tag.args);
                    const arg_vars_range = tag_list.items[tag_index].args;
                    const arg_rt_vars = self.runtime_types.sliceVars(arg_vars_range);

                    if (args_exprs.len == 0) {
                        // No payload args - finalize immediately
                        const value = try self.finalizeTagNoPayload(rt_var, tag_index, layout_val, roc_ops);
                        try value_stack.push(value);
                    } else {
                        // Has payload args - schedule collection
                        try work_stack.push(.{ .apply_continuation = .{ .tag_collect = .{
                            .collected_count = 0,
                            .remaining_args = args_exprs,
                            .arg_rt_vars = arg_rt_vars,
                            .expr_idx = expr_idx,
                            .rt_var = rt_var,
                            .tag_index = tag_index,
                            .layout_type = if (layout_val.tag == .record) 0 else 1,
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

                var subst_map = std.AutoHashMap(types.Var, types.Var).init(self.allocator);
                defer subst_map.deinit();
                const func_rt_var = if (should_instantiate)
                    try self.instantiateType(func_rt_var_orig, &subst_map)
                else
                    func_rt_var_orig;

                // If we instantiated, update rigid_subst (will be restored in cleanup)
                if (should_instantiate) {
                    var subst_iter = subst_map.iterator();
                    while (subst_iter.next()) |entry| {
                        try self.rigid_subst.put(entry.key_ptr.*, entry.value_ptr.*);
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

                // Schedule: first evaluate function, then collect args, then invoke
                // Push invoke continuation (to be executed after all args collected)
                try work_stack.push(.{ .apply_continuation = .{ .call_invoke_closure = .{
                    .arg_count = arg_indices.len,
                    .call_ret_rt_var = call_ret_rt_var,
                    .did_instantiate = should_instantiate,
                    .arg_rt_vars_to_free = arg_rt_vars,
                } } });

                // Push arg collection continuation (to be executed after function is evaluated)
                try work_stack.push(.{ .apply_continuation = .{ .call_collect_args = .{
                    .collected_count = 0,
                    .remaining_args = arg_indices,
                    .arg_rt_vars = arg_rt_vars,
                    .call_ret_rt_var = call_ret_rt_var,
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
        const rt_var = expected_rt_var orelse blk: {
            const ct_var = can.ModuleEnv.varFrom(expr_idx);
            break :blk try self.translateTypeVar(self.env, ct_var);
        };
        const layout_val = try self.getRuntimeLayout(rt_var);
        var value = try self.pushRaw(layout_val, 0);
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
        return value;
    }

    /// Evaluate a f32 fractional literal (e_frac_f32)
    fn evalFracF32(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lit: @TypeOf(@as(can.CIR.Expr, undefined).e_frac_f32),
    ) Error!StackValue {
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
    }

    /// Evaluate a f64 fractional literal (e_frac_f64)
    fn evalFracF64(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        lit: @TypeOf(@as(can.CIR.Expr, undefined).e_frac_f64),
    ) Error!StackValue {
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
    }

    /// Evaluate a decimal literal (e_dec)
    fn evalDec(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        dec_lit: @TypeOf(@as(can.CIR.Expr, undefined).e_dec),
    ) Error!StackValue {
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
    }

    /// Evaluate a small decimal literal (e_dec_small)
    fn evalDecSmall(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        expected_rt_var: ?types.Var,
        small: @TypeOf(@as(can.CIR.Expr, undefined).e_dec_small),
    ) Error!StackValue {
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
    }

    /// Evaluate a string segment literal (e_str_segment)
    fn evalStrSegment(
        self: *Interpreter,
        seg: @TypeOf(@as(can.CIR.Expr, undefined).e_str_segment),
        roc_ops: *RocOps,
    ) Error!StackValue {
        const content = self.env.getString(seg.literal);
        const value = try self.pushStr();
        const roc_str: *RocStr = @ptrCast(@alignCast(value.ptr.?));
        roc_str.* = RocStr.fromSlice(content, roc_ops);
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
        return try self.pushRaw(rec_layout, 0);
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
        const derived_layout = try self.getRuntimeLayout(rt_var);

        // Ensure we have a proper list layout even if the type variable defaulted to Dec.
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
        const tu = resolved.desc.content.structure.tag_union;
        const tags = self.runtime_types.getTagsSlice(tu.tags);
        // Find tag index by translating the source ident to the runtime store
        const tag_index = try self.findTagIndexByIdent(self.env, zero.name, tags) orelse {
            const name_text = self.env.getIdent(zero.name);
            const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
            self.triggerCrash(msg, true, roc_ops);
            return error.Crash;
        };
        const layout_val = try self.getRuntimeLayout(rt_var);

        // Handle different layout representations
        if (layout_val.tag == .scalar) {
            var out = try self.pushRaw(layout_val, 0);
            if (layout_val.data.scalar.tag == .int) {
                out.is_initialized = false;
                try out.setInt(@intCast(tag_index));
                out.is_initialized = true;
                out.rt_var = rt_var;
                return out;
            }
            self.triggerCrash("e_zero_argument_tag: scalar layout is not int", false, roc_ops);
            return error.Crash;
        } else if (layout_val.tag == .record) {
            // Record { tag: Discriminant, payload: ZST }
            var dest = try self.pushRaw(layout_val, 0);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            const tag_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                self.triggerCrash("e_zero_argument_tag: tag field not found", false, roc_ops);
                return error.Crash;
            };
            const tag_field = try acc.getFieldByIndex(tag_idx);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            } else {
                self.triggerCrash("e_zero_argument_tag: record tag field is not scalar int", false, roc_ops);
                return error.Crash;
            }
            dest.rt_var = rt_var;
            return dest;
        } else if (layout_val.tag == .tuple) {
            // Tuple (payload, tag) - tag unions are now represented as tuples
            var dest = try self.pushRaw(layout_val, 0);
            var acc = try dest.asTuple(&self.runtime_layout_store);
            // Element 1 is the tag discriminant
            const tag_field = try acc.getElement(1);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            } else {
                self.triggerCrash("e_zero_argument_tag: tuple tag field is not scalar int", false, roc_ops);
                return error.Crash;
            }
            dest.rt_var = rt_var;
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
            var dest = try self.pushRaw(layout_val, 0);
            var acc = try dest.asRecord(&self.runtime_layout_store);
            const tag_field_idx = acc.findFieldIndex(self.env.idents.tag) orelse {
                self.triggerCrash("e_tag: tag field not found", false, roc_ops);
                return error.Crash;
            };
            const tag_field = try acc.getFieldByIndex(tag_field_idx);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            }
            dest.rt_var = rt_var;
            return dest;
        } else if (layout_val.tag == .tuple) {
            var dest = try self.pushRaw(layout_val, 0);
            var acc = try dest.asTuple(&self.runtime_layout_store);
            const tag_field = try acc.getElement(1);
            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                var tmp = tag_field;
                tmp.is_initialized = false;
                try tmp.setInt(@intCast(tag_index));
            }
            dest.rt_var = rt_var;
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
    }

    /// Evaluate a hosted lambda expression (e_hosted_lambda) - creates a closure for host dispatch
    fn evalHostedLambda(
        self: *Interpreter,
        expr_idx: can.CIR.Expr.Idx,
        hosted: @TypeOf(@as(can.CIR.Expr, undefined).e_hosted_lambda),
    ) Error!StackValue {
        // Manually create a closure layout since hosted functions might have flex types
        const closure_layout = Layout{
            .tag = .closure,
            .data = .{
                .closure = .{
                    .captures_layout_idx = @enumFromInt(0),
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
        const value = try self.pushRaw(closure_layout, 0);
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
                    const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                    var rec_acc = (rec_val.asRecord(&self.runtime_layout_store)) catch continue;
                    if (rec_acc.findFieldIndex(cap.name)) |fidx| {
                        if (rec_acc.getFieldByIndex(fidx) catch null) |field_val| {
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
                const expr_idx_int: u32 = @intFromEnum(b.expr_idx);
                if (expr_idx_int != 0) {
                    const binding_expr = self.env.store.getExpr(b.expr_idx);
                    if (binding_expr == .e_anno_only and b.value.layout.tag != .closure) {
                        self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
                        return error.Crash;
                    }
                }
                const copy_result = try self.pushCopy(b.value, roc_ops);
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
                const result = try self.evalWithExpectedType(def.expr, roc_ops, null);
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
        const ph = try self.pushRaw(closure_layout, 0);
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
                // Push block_continue for remaining statements
                try work_stack.push(.{ .apply_continuation = .{ .block_continue = .{
                    .remaining_stmts = remaining_stmts,
                    .final_expr = final_expr,
                    .bindings_start = bindings_start,
                } } });
                // Push decref to clean up the expression result
                // We'll handle this by pushing a special continuation or just evaluating and discarding
                // For now, we'll just evaluate and let the block_continue handle cleanup
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

                // Push for_loop_iterate continuation (will be executed after list is evaluated)
                // We'll fill in list_value, list_len, etc. in the continuation handler
                try work_stack.push(.{
                    .apply_continuation = .{
                        .for_loop_iterate = .{
                            .list_value = undefined, // Will be set when list is evaluated
                            .current_index = 0,
                            .list_len = 0, // Will be set when list is evaluated
                            .elem_size = 0, // Will be set when list is evaluated
                            .elem_layout = undefined, // Will be set when list is evaluated
                            .pattern = for_stmt.patt,
                            .patt_rt_var = patt_rt_var,
                            .body = for_stmt.body,
                            .remaining_stmts = remaining_stmts,
                            .final_expr = final_expr,
                            .bindings_start = bindings_start,
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

                if (boolValueEquals(false, lhs)) {
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

                if (boolValueEquals(true, lhs)) {
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

                if (boolValueEquals(true, cond)) {
                    // Condition is true, evaluate the body
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = ib.body,
                        .expected_rt_var = null,
                    } });
                } else if (ib.remaining_branches.len > 0) {
                    // Try next branch
                    const next_branch = self.env.store.getIfBranch(ib.remaining_branches[0]);
                    // Push continuation for next branch
                    try work_stack.push(.{ .apply_continuation = .{ .if_branch = .{
                        .body = next_branch.body,
                        .remaining_branches = ib.remaining_branches[1..],
                        .final_else = ib.final_else,
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
                        .expected_rt_var = null,
                    } });
                }
                return true;
            },
            .block_continue => |bc| {
                // For s_expr statements, we need to pop and discard the value
                // Check if there's a value to discard (from s_expr)
                if (value_stack.items.items.len > 0) {
                    // Pop and discard any value left from s_expr
                    const val = value_stack.pop().?;
                    val.decref(&self.runtime_layout_store, roc_ops);
                }

                if (bc.remaining_stmts.len == 0) {
                    // No more statements, evaluate final expression
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = bc.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    // Process next statement
                    const next_stmt = self.env.store.getStatement(bc.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, bc.remaining_stmts[1..], bc.final_expr, bc.bindings_start, roc_ops);
                }
                return true;
            },
            .bind_decl => |bd| {
                // Pop evaluated value from stack
                const val = value_stack.pop() orelse return error.Crash;
                defer val.decref(&self.runtime_layout_store, roc_ops);

                // Get the runtime type for pattern matching
                const expr_ct_var = can.ModuleEnv.varFrom(bd.expr_idx);
                const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);

                // Bind the pattern
                var temp_binds = try std.array_list.AlignedManaged(Binding, null).initCapacity(self.allocator, 4);
                defer {
                    self.trimBindingList(&temp_binds, 0, roc_ops);
                    temp_binds.deinit();
                }

                if (!try self.patternMatchesBind(bd.pattern, val, expr_rt_var, roc_ops, &temp_binds, bd.expr_idx)) {
                    return error.TypeMismatch;
                }

                // Add bindings using upsertBinding to handle closure placeholders
                for (temp_binds.items) |binding| {
                    try self.upsertBinding(binding, bd.bindings_start, roc_ops);
                }

                // Continue with remaining statements
                if (bd.remaining_stmts.len == 0) {
                    // No more statements, evaluate final expression
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = bd.final_expr,
                        .expected_rt_var = null,
                    } });
                } else {
                    // Process next statement
                    const next_stmt = self.env.store.getStatement(bd.remaining_stmts[0]);
                    try self.scheduleNextStatement(work_stack, next_stmt, bd.remaining_stmts[1..], bd.final_expr, bd.bindings_start, roc_ops);
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
                        const tuple_val = try self.pushRaw(tuple_layout, 0);
                        try value_stack.push(tuple_val);
                    } else {
                        // Gather layouts and values
                        var elem_layouts = try self.allocator.alloc(Layout, total_count);
                        defer self.allocator.free(elem_layouts);

                        // Values are in reverse order on stack (first element pushed first, so it's at the bottom)
                        // We need to pop them and store in correct order
                        var values = try self.allocator.alloc(StackValue, total_count);
                        defer self.allocator.free(values);

                        // Pop values in reverse order (last evaluated is on top)
                        var i: usize = total_count;
                        while (i > 0) {
                            i -= 1;
                            values[i] = value_stack.pop() orelse return error.Crash;
                            elem_layouts[i] = values[i].layout;
                        }

                        // Create tuple layout
                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                        var dest = try self.pushRaw(tuple_layout, 0);
                        var accessor = try dest.asTuple(&self.runtime_layout_store);

                        if (total_count != accessor.getElementCount()) return error.TypeMismatch;

                        // Set all elements
                        for (0..total_count) |idx| {
                            try accessor.setElement(idx, values[idx], roc_ops);
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
                    try work_stack.push(.{ .eval_expr = .{
                        .expr_idx = lc.remaining_elems[0],
                        .expected_rt_var = lc.elem_rt_var,
                    } });
                } else {
                    // All elements evaluated - finalize the list
                    const total_count = lc.collected_count;

                    if (total_count == 0) {
                        // Empty list (shouldn't happen as it's handled directly)
                        const list_layout = try self.getRuntimeLayout(lc.list_rt_var);
                        const dest = try self.pushRaw(list_layout, 0);
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

                        const dest = try self.pushRaw(actual_list_layout, 0);
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
                                    try val.copyToPtr(&self.runtime_layout_store, dest_ptr, roc_ops);
                                }
                            }
                        }

                        markListElementCount(&runtime_list, elements_refcounted);
                        header.* = runtime_list;

                        // Decref temporary values after they've been copied into the list
                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }

                        try value_stack.push(dest);
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
                    for (rc.all_fields, 0..) |field_idx_enum, idx| {
                        const f = self.env.store.getRecordField(field_idx_enum);
                        const field_layout = field_values[idx].layout;
                        const key: u32 = @bitCast(f.name);
                        if (union_indices.get(key)) |idx_ptr| {
                            union_layouts.items[idx_ptr] = field_layout;
                            union_names.items[idx_ptr] = f.name;
                        } else {
                            try union_layouts.append(field_layout);
                            try union_names.append(f.name);
                            try union_indices.put(key, union_layouts.items.len - 1);
                        }
                    }

                    // Create record layout
                    const record_layout_idx = try self.runtime_layout_store.putRecord(self.env, union_layouts.items, union_names.items);
                    const rec_layout = self.runtime_layout_store.getLayout(record_layout_idx);

                    // Cache the layout for this var
                    const resolved_rt = self.runtime_types.resolveVar(rc.rt_var);
                    const root_idx: usize = @intFromEnum(resolved_rt.var_);
                    try self.ensureVarLayoutCapacity(root_idx + 1);
                    self.var_to_layout_slot.items[root_idx] = @intFromEnum(record_layout_idx) + 1;

                    var dest = try self.pushRaw(rec_layout, 0);
                    var accessor = try dest.asRecord(&self.runtime_layout_store);

                    // Copy base record fields first
                    if (base_value_opt) |base_value| {
                        var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                        var idx: usize = 0;
                        while (idx < base_accessor.getFieldCount()) : (idx += 1) {
                            const info = base_accessor.field_layouts.get(idx);
                            const dest_field_idx = accessor.findFieldIndex(info.name) orelse return error.TypeMismatch;
                            const base_field_value = try base_accessor.getFieldByIndex(idx);
                            try accessor.setFieldByIndex(dest_field_idx, base_field_value, roc_ops);
                        }
                    }

                    // Set explicit field values (overwriting base values if needed)
                    for (rc.all_fields, 0..) |field_idx_enum, explicit_index| {
                        const f = self.env.store.getRecordField(field_idx_enum);
                        const dest_field_idx = accessor.findFieldIndex(f.name) orelse return error.TypeMismatch;
                        const val = field_values[explicit_index];

                        // If overwriting a base field, decref the existing value
                        if (base_value_opt) |base_value| {
                            var base_accessor = try base_value.asRecord(&self.runtime_layout_store);
                            if (base_accessor.findFieldIndex(f.name) != null) {
                                const existing = try accessor.getFieldByIndex(dest_field_idx);
                                existing.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }

                        try accessor.setFieldByIndex(dest_field_idx, val, roc_ops);
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
                                .for_loop_iterate => |fl| {
                                    // Decref the list value when skipping a for loop
                                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                                },
                                .for_loop_body_done => |fl| {
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
                        var dest = try self.pushRaw(layout_val, 0);
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
                        const tag_field = try acc.getFieldByIndex(tag_field_idx);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(tc.tag_index));
                        }

                        // Write payload
                        const payload_field = try acc.getFieldByIndex(payload_field_idx);
                        if (payload_field.ptr) |payload_ptr| {
                            if (total_count == 1) {
                                try values[0].copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                            } else {
                                // Multiple args - create tuple payload
                                var elem_layouts = try self.allocator.alloc(Layout, total_count);
                                defer self.allocator.free(elem_layouts);
                                for (values, 0..) |val, idx| {
                                    elem_layouts[idx] = val.layout;
                                }
                                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                                var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                                var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                                for (values, 0..) |val, idx| {
                                    try tup_acc.setElement(idx, val, roc_ops);
                                }
                            }
                        }

                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }
                        dest.rt_var = tc.rt_var;
                        try value_stack.push(dest);
                    } else {
                        // Tuple layout (payload, tag)
                        var dest = try self.pushRaw(layout_val, 0);
                        var acc = try dest.asTuple(&self.runtime_layout_store);

                        // Write tag discriminant (element 1)
                        const tag_field = try acc.getElement(1);
                        if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                            var tmp = tag_field;
                            tmp.is_initialized = false;
                            try tmp.setInt(@intCast(tc.tag_index));
                        }

                        // Write payload (element 0)
                        const payload_field = try acc.getElement(0);
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
                                    var proper_dest = try self.pushRaw(proper_tuple_layout, 0);
                                    var proper_acc = try proper_dest.asTuple(&self.runtime_layout_store);

                                    // Write tag
                                    const proper_tag_field = try proper_acc.getElement(1);
                                    if (proper_tag_field.layout.tag == .scalar and proper_tag_field.layout.data.scalar.tag == .int) {
                                        var tmp = proper_tag_field;
                                        tmp.is_initialized = false;
                                        try tmp.setInt(@intCast(tc.tag_index));
                                    }

                                    // Write payload
                                    const proper_payload_field = try proper_acc.getElement(0);
                                    if (proper_payload_field.ptr) |proper_ptr| {
                                        try values[0].copyToPtr(&self.runtime_layout_store, proper_ptr, roc_ops);
                                    }

                                    for (values) |val| {
                                        val.decref(&self.runtime_layout_store, roc_ops);
                                    }
                                    proper_dest.rt_var = tc.rt_var;
                                    try value_stack.push(proper_dest);
                                    return true;
                                }

                                try values[0].copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                            } else {
                                // Multiple args - create tuple payload
                                var elem_layouts = try self.allocator.alloc(Layout, total_count);
                                defer self.allocator.free(elem_layouts);
                                for (values, 0..) |val, idx| {
                                    elem_layouts[idx] = val.layout;
                                }
                                const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                                const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                                var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                                var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                                for (values, 0..) |val, idx| {
                                    try tup_acc.setElement(idx, val, roc_ops);
                                }
                            }
                        }

                        for (values) |val| {
                            val.decref(&self.runtime_layout_store, roc_ops);
                        }
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
                const scrutinee = try self.pushCopy(scrutinee_temp, roc_ops);
                scrutinee_temp.decref(&self.runtime_layout_store, roc_ops);

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

                        if (!try self.patternMatchesBind(
                            self.env.store.getMatchBranchPattern(bp_idx).pattern,
                            scrutinee,
                            mb.scrutinee_rt_var,
                            roc_ops,
                            &temp_binds,
                            @enumFromInt(0),
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

                const guard_pass = boolValueEquals(true, guard_val);

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
                const succeeded = boolValueEquals(true, cond_val);
                if (succeeded) {
                    // Return {} (empty record)
                    const ct_var = can.ModuleEnv.varFrom(ec.expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const layout_val = try self.getRuntimeLayout(rt_var);
                    const result = try self.pushRaw(layout_val, 0);
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
                const rendered = try self.renderValueRocWithType(value, dp.inner_rt_var);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                // Return {} (empty record)
                const ct_var = can.ModuleEnv.varFrom(dp.expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const layout_val = try self.getRuntimeLayout(rt_var);
                const result = try self.pushRaw(layout_val, 0);
                try value_stack.push(result);
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
                    const str_value = try self.pushStr();
                    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(str_value.ptr.?));
                    roc_str_ptr.* = segment_str;
                    try value_stack.push(str_value);
                    collected_count += 1;
                    remaining = remaining[1..]; // Move past the segment we just converted
                }

                // Step 2: Process remaining segments
                if (remaining.len == 0) {
                    // Step 3: All segments collected - concatenate them
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

                    const result = try self.pushStr();
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
                    const content = self.env.getString(next_seg_expr.e_str_segment.literal);
                    const seg_str = RocStr.fromSlice(content, roc_ops);
                    const seg_value = try self.pushStr();
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
                const arg_count = ci.arg_count;

                // Pop all arguments (in reverse order)
                var arg_values = try self.allocator.alloc(StackValue, arg_count);
                defer self.allocator.free(arg_values);
                var i: usize = arg_count;
                while (i > 0) {
                    i -= 1;
                    arg_values[i] = value_stack.pop() orelse return error.Crash;
                }

                // Pop function value
                const func_val = value_stack.pop() orelse return error.Crash;

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
                        const result = try self.callLowLevelBuiltin(low_level.op, arg_values, roc_ops, ci.call_ret_rt_var);

                        // Decref args (except for list_concat which handles its own refcounting)
                        if (low_level.op != .list_concat) {
                            for (arg_values) |arg| {
                                arg.decref(&self.runtime_layout_store, roc_ops);
                            }
                        }

                        // Restore environment and free arg_rt_vars
                        self.env = saved_env;
                        func_val.decref(&self.runtime_layout_store, roc_ops);
                        if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
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

                    // Bind parameters using pattern matching to handle destructuring
                    for (params, 0..) |param, idx| {
                        // Get the runtime type for this parameter
                        const param_rt_var = if (ci.arg_rt_vars_to_free) |vars|
                            (if (idx < vars.len) vars[idx] else try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(param)))
                        else
                            try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(param));

                        // Use patternMatchesBind to properly handle complex patterns (e.g., list destructuring)
                        // patternMatchesBind borrows the value and creates copies for bindings, so we need to
                        // decref the original arg_value after successful binding
                        if (!try self.patternMatchesBind(param, arg_values[idx], param_rt_var, roc_ops, &self.bindings, @enumFromInt(0))) {
                            // Pattern match failed - cleanup and error
                            self.env = saved_env;
                            _ = self.active_closures.pop();
                            func_val.decref(&self.runtime_layout_store, roc_ops);
                            for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
                            if (ci.arg_rt_vars_to_free) |vars| self.allocator.free(vars);
                            return error.TypeMismatch;
                        }
                        // Decref the original argument value since patternMatchesBind made copies
                        arg_values[idx].decref(&self.runtime_layout_store, roc_ops);
                    }

                    // Push cleanup continuation, then evaluate body
                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = params.len,
                        .has_active_closure = true,
                        .did_instantiate = ci.did_instantiate,
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
                if (self.early_return_value) |return_val| {
                    // Body triggered early return - use that value
                    self.early_return_value = null;

                    // Pop active closure if needed
                    if (cleanup.has_active_closure) {
                        if (self.active_closures.pop()) |closure_val| {
                            closure_val.decref(&self.runtime_layout_store, roc_ops);
                        }
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

                // Restore environment and cleanup bindings
                // Use trimBindingList to properly decref all bindings created by pattern matching
                // (which may be more than param_count due to destructuring)
                self.env = cleanup.saved_env;
                self.trimBindingList(&self.bindings, cleanup.saved_bindings_len, roc_ops);
                if (cleanup.arg_rt_vars_to_free) |vars| self.allocator.free(vars);

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
                    .expr_idx = @enumFromInt(0),
                    .source_env = self.env,
                });

                // Push cleanup and evaluate body
                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = 1,
                    .has_active_closure = true,
                    .did_instantiate = false,
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

                // Resolve the lhs type
                const lhs_resolved = self.runtime_types.resolveVar(ba.receiver_rt_var);

                // Get nominal type info, or handle anonymous structural types
                const nominal_info: ?struct { origin: base_pkg.Ident.Idx, ident: base_pkg.Ident.Idx } = switch (lhs_resolved.desc.content) {
                    .structure => |s| switch (s) {
                        .nominal_type => |nom| .{
                            .origin = nom.origin_module,
                            .ident = nom.ident.ident_idx,
                        },
                        .record, .tuple, .tag_union, .empty_record, .empty_tag_union => blk: {
                            // Anonymous structural types have implicit is_eq
                            if (ba.method_ident == self.root_env.idents.is_eq) {
                                var result = self.valuesStructurallyEqual(lhs, ba.receiver_rt_var, rhs, ba.rhs_rt_var, roc_ops) catch |err| {
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
                    else => null,
                };

                if (nominal_info == null) {
                    return error.InvalidMethodReceiver;
                }

                // Resolve the method function
                const method_func = try self.resolveMethodFunction(
                    nominal_info.?.origin,
                    nominal_info.?.ident,
                    ba.method_ident,
                    roc_ops,
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
                    self.env = saved_env;

                    // For != operator, negate boolean result
                    if (ba.negate_result) {
                        const is_eq_result = boolValueEquals(true, result);
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

                // Bind parameters
                try self.bindings.append(.{
                    .pattern_idx = params[0],
                    .value = lhs,
                    .expr_idx = @enumFromInt(0),
                    .source_env = self.env,
                });
                try self.bindings.append(.{
                    .pattern_idx = params[1],
                    .value = rhs,
                    .expr_idx = @enumFromInt(0),
                    .source_env = self.env,
                });

                // Push cleanup and evaluate body
                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = 2,
                    .has_active_closure = true,
                    .did_instantiate = false,
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
                    const field_value = try accessor.getFieldByIndex(field_idx);
                    const result = try self.pushCopy(field_value, roc_ops);
                    try value_stack.push(result);
                    return true;
                }

                // Method call - resolve receiver type for dispatch
                // First check if the type is still a flex/rigid var, default to Dec
                var effective_receiver_rt_var = da.receiver_rt_var;
                const receiver_resolved_check = self.runtime_types.resolveVar(da.receiver_rt_var);
                if (receiver_resolved_check.desc.content == .flex or receiver_resolved_check.desc.content == .rigid) {
                    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
                    const dec_var = try self.runtime_types.freshFromContent(dec_content);
                    effective_receiver_rt_var = dec_var;
                }

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
                        const result = try self.callLowLevelBuiltin(low_level.op, &args, roc_ops, null);
                        receiver_value.decref(&self.runtime_layout_store, roc_ops);
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
                        .expr_idx = @enumFromInt(0),
                        .source_env = self.env,
                    });

                    try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                        .saved_env = saved_env,
                        .saved_bindings_len = saved_bindings_len,
                        .param_count = 1,
                        .has_active_closure = true,
                        .did_instantiate = false,
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

                    // Build args array: receiver + explicit args
                    var all_args = try self.allocator.alloc(StackValue, 1 + total_args);
                    defer self.allocator.free(all_args);
                    all_args[0] = receiver_value;
                    for (arg_values, 0..) |arg, idx| {
                        all_args[1 + idx] = arg;
                    }

                    const result = try self.callLowLevelBuiltin(low_level.op, all_args, roc_ops, null);

                    receiver_value.decref(&self.runtime_layout_store, roc_ops);
                    for (arg_values) |arg| arg.decref(&self.runtime_layout_store, roc_ops);
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

                try self.active_closures.append(method_func);

                // Bind receiver first
                try self.bindings.append(.{
                    .pattern_idx = params[0],
                    .value = receiver_value,
                    .expr_idx = @enumFromInt(0),
                    .source_env = self.env,
                });

                // Bind explicit arguments
                for (arg_values, 0..) |arg, idx| {
                    try self.bindings.append(.{
                        .pattern_idx = params[1 + idx],
                        .value = arg,
                        .expr_idx = @enumFromInt(0),
                        .source_env = self.env,
                    });
                }

                try work_stack.push(.{ .apply_continuation = .{ .call_cleanup = .{
                    .saved_env = saved_env,
                    .saved_bindings_len = saved_bindings_len,
                    .param_count = expected_params,
                    .has_active_closure = true,
                    .did_instantiate = false,
                    .arg_rt_vars_to_free = null,
                } } });
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = closure_header.body_idx,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .for_loop_iterate => |fl_in| {
                // For loop iteration: list has been evaluated, start iterating
                const list_value = value_stack.pop() orelse return error.Crash;

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

                // Create the proper for_loop_iterate with list info filled in
                var fl = fl_in;
                fl.list_value = list_value;
                fl.list_len = list_len;
                fl.elem_size = elem_size;
                fl.elem_layout = elem_layout;

                // If list is empty, skip to remaining statements
                if (list_len == 0) {
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    if (fl.remaining_stmts.len == 0) {
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = fl.final_expr,
                            .expected_rt_var = null,
                        } });
                    } else {
                        const next_stmt = self.env.store.getStatement(fl.remaining_stmts[0]);
                        try self.scheduleNextStatement(work_stack, next_stmt, fl.remaining_stmts[1..], fl.final_expr, fl.bindings_start, roc_ops);
                    }
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
                };
                elem_value.incref();

                // Bind the pattern
                const loop_bindings_start = self.bindings.items.len;
                if (!try self.patternMatchesBind(fl.pattern, elem_value, fl.patt_rt_var, roc_ops, &self.bindings, @enumFromInt(0))) {
                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                    list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }

                // Push body_done continuation
                try work_stack.push(.{ .apply_continuation = .{ .for_loop_body_done = .{
                    .list_value = fl.list_value,
                    .current_index = 0,
                    .list_len = fl.list_len,
                    .elem_size = fl.elem_size,
                    .elem_layout = fl.elem_layout,
                    .pattern = fl.pattern,
                    .patt_rt_var = fl.patt_rt_var,
                    .body = fl.body,
                    .remaining_stmts = fl.remaining_stmts,
                    .final_expr = fl.final_expr,
                    .bindings_start = fl.bindings_start,
                    .loop_bindings_start = loop_bindings_start,
                } } });

                // Evaluate body
                try work_stack.push(.{ .eval_expr = .{
                    .expr_idx = fl.body,
                    .expected_rt_var = null,
                } });
                return true;
            },
            .for_loop_body_done => |fl| {
                // For loop body completed, clean up and continue to next iteration
                const body_result = value_stack.pop() orelse return error.Crash;
                body_result.decref(&self.runtime_layout_store, roc_ops);

                // Clean up bindings for this iteration
                self.trimBindingList(&self.bindings, fl.loop_bindings_start, roc_ops);

                // Move to next element
                const next_index = fl.current_index + 1;
                if (next_index >= fl.list_len) {
                    // Loop complete, decref list and continue with remaining statements
                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                    if (fl.remaining_stmts.len == 0) {
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = fl.final_expr,
                            .expected_rt_var = null,
                        } });
                    } else {
                        const next_stmt = self.env.store.getStatement(fl.remaining_stmts[0]);
                        try self.scheduleNextStatement(work_stack, next_stmt, fl.remaining_stmts[1..], fl.final_expr, fl.bindings_start, roc_ops);
                    }
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
                };
                elem_value.incref();

                // Bind the pattern
                const new_loop_bindings_start = self.bindings.items.len;
                if (!try self.patternMatchesBind(fl.pattern, elem_value, fl.patt_rt_var, roc_ops, &self.bindings, @enumFromInt(0))) {
                    elem_value.decref(&self.runtime_layout_store, roc_ops);
                    fl.list_value.decref(&self.runtime_layout_store, roc_ops);
                    return error.TypeMismatch;
                }

                // Push body_done continuation for next iteration
                try work_stack.push(.{ .apply_continuation = .{ .for_loop_body_done = .{
                    .list_value = fl.list_value,
                    .current_index = next_index,
                    .list_len = fl.list_len,
                    .elem_size = fl.elem_size,
                    .elem_layout = fl.elem_layout,
                    .pattern = fl.pattern,
                    .patt_rt_var = fl.patt_rt_var,
                    .body = fl.body,
                    .remaining_stmts = fl.remaining_stmts,
                    .final_expr = fl.final_expr,
                    .bindings_start = fl.bindings_start,
                    .loop_bindings_start = new_loop_bindings_start,
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
                const cond_is_true = boolValueEquals(true, cond_value);

                if (!cond_is_true) {
                    // Loop complete, continue with remaining statements
                    if (wl.remaining_stmts.len == 0) {
                        try work_stack.push(.{ .eval_expr = .{
                            .expr_idx = wl.final_expr,
                            .expected_rt_var = null,
                        } });
                    } else {
                        const next_stmt = self.env.store.getStatement(wl.remaining_stmts[0]);
                        try self.scheduleNextStatement(work_stack, next_stmt, wl.remaining_stmts[1..], wl.final_expr, wl.bindings_start, roc_ops);
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
                const is_true = boolValueEquals(true, cond_val);
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
                    try self.scheduleNextStatement(work_stack, next_stmt, ec.remaining_stmts[1..], ec.final_expr, ec.bindings_start, roc_ops);
                }
                return true;
            },
            .reassign_value => |rv| {
                // Reassign statement: update binding
                const new_val = value_stack.pop() orelse return error.Crash;
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
                    try self.scheduleNextStatement(work_stack, next_stmt, rv.remaining_stmts[1..], rv.final_expr, rv.bindings_start, roc_ops);
                }
                return true;
            },
            .dbg_print_stmt => |dp| {
                // Dbg statement: print value
                const value = value_stack.pop() orelse return error.Crash;
                defer value.decref(&self.runtime_layout_store, roc_ops);
                const rendered = try self.renderValueRocWithType(value, dp.rt_var);
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
                    try self.scheduleNextStatement(work_stack, next_stmt, dp.remaining_stmts[1..], dp.final_expr, dp.bindings_start, roc_ops);
                }
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
