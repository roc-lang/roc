//! Interpreter implementing the type-carrying architecture.

const std = @import("std");
const builtin = @import("builtin");
const base_pkg = @import("base");
const types = @import("types");
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
const RocStr = builtins.str.RocStr;
const RocDec = builtins.dec.RocDec;
const RocList = builtins.list.RocList;
const utils = builtins.utils;
const Layout = layout.Layout;
const helpers = @import("test/helpers.zig");
const builtin_loading = @import("builtin_loading.zig");
const compiled_builtins = @import("compiled_builtins");
const BuiltinTypes = @import("builtins.zig").BuiltinTypes;

/// Interpreter that evaluates canonical Roc expressions against runtime types/layouts.
pub const Interpreter = struct {
    pub const Error = error{
        Crash,
        DivisionByZero,
        InvalidMethodReceiver,
        InvalidNumExt,
        InvalidTagExt,
        ListIndexOutOfBounds,
        MethodLookupFailed,
        MethodNotFound,
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
            if (a.args_len == 0) return true;
            return std.mem.eql(types.Var, a.args_ptr[0..a.args_len], b.args_ptr[0..b.args_len]);
        }
    };
    const Binding = struct { pattern_idx: can.CIR.Pattern.Idx, value: StackValue };
    allocator: std.mem.Allocator,
    runtime_types: *types.store.Store,
    runtime_layout_store: layout.Store,
    // O(1) Var -> Layout slot cache (0 = unset, else layout_idx + 1)
    var_to_layout_slot: std.array_list.Managed(u32),
    // Empty scope used when converting runtime vars to layouts
    empty_scope: TypeScope,
    // Translation cache: (env_ptr, compile_var) -> runtime_var
    translate_cache: std.AutoHashMap(u64, types.Var),

    // Polymorphic instantiation cache

    poly_cache: HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80),

    // Runtime unification context
    env: *can.ModuleEnv,
    module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
    module_ids: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32),
    current_module_id: u32,
    next_module_id: u32,
    // For backward compatibility with e_lookup_external (uses Import.Idx indexing)
    other_envs: []const *const can.ModuleEnv,
    problems: problem_mod.Store,
    snapshots: snapshot_mod.Store,
    unify_scratch: unify.Scratch,

    // Minimal eval support
    stack_memory: stack.Stack,
    bindings: std.array_list.Managed(Binding),
    // Track active closures during calls (for capture lookup)
    active_closures: std.array_list.Managed(StackValue),
    bool_false_index: u8,
    bool_true_index: u8,
    canonical_bool_rt_var: ?types.Var,
    // Used to unwrap extensible tags
    scratch_tags: std.array_list.Managed(types.Tag),
    /// Builtin types required by the interpreter (Bool, Result, etc.)
    builtins: BuiltinTypes,
    /// Map from module name to ModuleEnv for resolving e_lookup_external expressions
    imported_modules: std.StringHashMap(*const can.ModuleEnv),

    pub fn init(allocator: std.mem.Allocator, env: *can.ModuleEnv, builtin_types: BuiltinTypes, imported_modules_map: ?*const std.AutoHashMap(base_pkg.Ident.Idx, can.Can.AutoImportedType)) !Interpreter {
        // Build maps from Ident.Idx to ModuleEnv and module ID
        var module_envs = std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv){};
        errdefer module_envs.deinit(allocator);
        var module_ids = std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32){};
        errdefer module_ids.deinit(allocator);

        // Also build other_envs slice for backward compatibility with e_lookup_external
        var other_envs_list = std.array_list.Managed(*const can.ModuleEnv).init(allocator);
        errdefer other_envs_list.deinit();

        var next_id: u32 = 1; // Start at 1, reserve 0 for current module

        if (imported_modules_map) |modules_map| {
            try module_envs.ensureTotalCapacity(allocator, modules_map.count());
            try module_ids.ensureTotalCapacity(allocator, modules_map.count());

            var iter = modules_map.iterator();
            while (iter.next()) |entry| {
                const ident_idx = entry.key_ptr.*;
                const module_env = entry.value_ptr.env;
                module_envs.putAssumeCapacity(ident_idx, module_env);
                module_ids.putAssumeCapacity(ident_idx, next_id);
                try other_envs_list.append(module_env);
                next_id += 1;
            }
        }

        const other_envs = try other_envs_list.toOwnedSlice();
        return initWithModuleEnvs(allocator, env, module_envs, module_ids, other_envs, next_id, builtin_types);
    }

    /// Deinit the interpreter and also free the module maps if they were allocated by init()
    pub fn deinitAndFreeOtherEnvs(self: *Interpreter) void {
        const other_envs = self.other_envs;
        const allocator = self.allocator;
        self.deinit();
        if (other_envs.len > 0) {
            allocator.free(other_envs);
        }
    }

    pub fn initWithModuleEnvs(
        allocator: std.mem.Allocator,
        env: *can.ModuleEnv,
        module_envs: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, *const can.ModuleEnv),
        module_ids: std.AutoHashMapUnmanaged(base_pkg.Ident.Idx, u32),
        other_envs: []const *const can.ModuleEnv,
        next_module_id: u32,
        builtin_types: BuiltinTypes,
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
            .poly_cache = HashMap(PolyKey, PolyEntry, PolyKeyCtx, 80).init(allocator),
            .env = env,
            .module_envs = module_envs,
            .module_ids = module_ids,
            .current_module_id = 0, // Current module always gets ID 0
            .next_module_id = next_module_id,
            .other_envs = other_envs,
            .problems = try problem_mod.Store.initCapacity(allocator, 64),
            .snapshots = try snapshot_mod.Store.initCapacity(allocator, 256),
            .unify_scratch = try unify.Scratch.init(allocator),
            .stack_memory = try stack.Stack.initCapacity(allocator, 4096),
            .bindings = try std.array_list.Managed(Binding).initCapacity(allocator, 8),
            .active_closures = try std.array_list.Managed(StackValue).initCapacity(allocator, 4),
            .bool_false_index = 0,
            .bool_true_index = 1,
            .canonical_bool_rt_var = null,
            .scratch_tags = try std.array_list.Managed(types.Tag).initCapacity(allocator, 8),
            .builtins = builtin_types,
            .imported_modules = std.StringHashMap(*const can.ModuleEnv).init(allocator),
        };
        result.runtime_layout_store = try layout.Store.init(env, result.runtime_types);

        return result;
    }

    // Minimal evaluator for subset: string literals, lambdas without captures, and lambda calls
    pub fn evalMinimal(self: *Interpreter, expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) Error!StackValue {
        return try self.evalExprMinimal(expr_idx, roc_ops, null);
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
                return error.NotImplemented;
            }

            const header: *const layout.Closure = @ptrCast(@alignCast(func_val.ptr.?));
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
                    const sorted_idx = args_accessor.findElementIndexByOriginal(j) orelse j;
                    const arg_value = try args_accessor.getElement(sorted_idx);
                    const matched = try self.patternMatchesBind(params[j], arg_value, param_rt_vars[j], roc_ops, &temp_binds);
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

            const result_value = try self.evalExprMinimal(header.body_idx, roc_ops, null);
            defer result_value.decref(&self.runtime_layout_store, roc_ops);

            try result_value.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
            return;
        }

        const result = try self.evalMinimal(expr_idx, roc_ops);
        defer result.decref(&self.runtime_layout_store, roc_ops);

        try result.copyToPtr(&self.runtime_layout_store, ret_ptr, roc_ops);
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
                            try self_interp.bindings.append(.{ .pattern_idx = patt_idx, .value = ph });
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

                            if (!try self.patternMatchesBind(d.pattern, val, expr_rt_var, roc_ops, &temp_binds)) {
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

                            if (!try self.patternMatchesBind(v.pattern_idx, val, expr_rt_var, roc_ops, &temp_binds)) {
                                return error.TypeMismatch;
                            }

                            for (temp_binds.items) |binding| {
                                try self.upsertBinding(binding, original_len, roc_ops);
                            }
                            temp_binds.items.len = 0;
                        },
                        .s_reassign => |r| {
                            const patt = self.env.store.getPattern(r.pattern_idx);
                            if (patt != .assign) return error.NotImplemented;
                            const new_val = try self.evalExprMinimal(r.expr, roc_ops, null);
                            var j: usize = self.bindings.items.len;
                            while (j > original_len) {
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
                            // Get the actual type of the expression
                            const expr_ct_var = can.ModuleEnv.varFrom(expect_stmt.body);
                            const expr_rt_var = try self.translateTypeVar(self.env, expr_ct_var);
                            const cond_val = try self.evalExprMinimal(expect_stmt.body, roc_ops, bool_rt_var);
                            // Try using the expression's actual type first, then fall back to canonical Bool type
                            const is_true = self.boolValueIsTrue(cond_val, expr_rt_var) catch blk: {
                                break :blk try self.boolValueIsTrue(cond_val, bool_rt_var);
                            };
                            if (!is_true) {
                                try self.handleExpectFailure(expect_stmt.body, roc_ops);
                                return error.Crash;
                            }
                        },
                        .s_expr => |sx| {
                            _ = try self.evalExprMinimal(sx.expr, roc_ops, null);
                        },
                        else => return error.NotImplemented,
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
                        .int => value.setInt(num_lit.value.toI128()),
                        .frac => switch (layout_val.data.scalar.data.frac) {
                            .f32 => {
                                const ptr = @as(*f32, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = @floatFromInt(num_lit.value.toI128());
                            },
                            .f64 => {
                                const ptr = @as(*f64, @ptrCast(@alignCast(value.ptr.?)));
                                ptr.* = @floatFromInt(num_lit.value.toI128());
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
            .e_binop => |binop| {
                if (binop.op == .add or binop.op == .sub or binop.op == .mul or binop.op == .div or binop.op == .div_trunc or binop.op == .rem) {
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

                    const lhs = try self.evalExprMinimal(binop.lhs, roc_ops, lhs_rt_var);
                    const rhs = try self.evalExprMinimal(binop.rhs, roc_ops, rhs_rt_var);

                    return try self.evalArithmeticBinop(binop.op, expr_idx, lhs, rhs, lhs_rt_var, rhs_rt_var);
                } else if (binop.op == .eq or binop.op == .ne or binop.op == .lt or binop.op == .le or binop.op == .gt or binop.op == .ge) {
                    // Comparison operators - evaluate both sides and compare
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);

                    var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, lhs_rt_var);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, rhs_rt_var);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);

                    // Compare the values
                    const comparison_result = try self.compareValues(lhs, rhs, binop.op);
                    return try self.makeBoolValue(result_rt_var, comparison_result);
                } else if (binop.op == .@"or") {
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    if (try self.boolValueIsTrue(lhs, lhs_rt_var)) {
                        return try self.makeBoolValue(result_rt_var, true);
                    }

                    var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);
                    const rhs_truthy = try self.boolValueIsTrue(rhs, rhs_rt_var);
                    return try self.makeBoolValue(result_rt_var, rhs_truthy);
                } else if (binop.op == .@"and") {
                    const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                    var result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                    result_rt_var = try self.ensureBoolRuntimeVar(self.env, result_ct_var, result_rt_var);

                    var lhs = try self.evalExprMinimal(binop.lhs, roc_ops, null);
                    defer lhs.decref(&self.runtime_layout_store, roc_ops);
                    const lhs_ct_var = can.ModuleEnv.varFrom(binop.lhs);
                    const lhs_rt_var = try self.translateTypeVar(self.env, lhs_ct_var);
                    if (!try self.boolValueIsTrue(lhs, lhs_rt_var)) {
                        return try self.makeBoolValue(result_rt_var, false);
                    }

                    var rhs = try self.evalExprMinimal(binop.rhs, roc_ops, null);
                    defer rhs.decref(&self.runtime_layout_store, roc_ops);
                    const rhs_ct_var = can.ModuleEnv.varFrom(binop.rhs);
                    const rhs_rt_var = try self.translateTypeVar(self.env, rhs_ct_var);
                    const rhs_truthy = try self.boolValueIsTrue(rhs, rhs_rt_var);
                    return try self.makeBoolValue(result_rt_var, rhs_truthy);
                }
                return error.NotImplemented;
            },
            .e_if => |if_expr| {
                const branches = self.env.store.sliceIfBranches(if_expr.branches);
                // Evaluate branches in order; pick first true condition
                var i: usize = 0;
                while (i < branches.len) : (i += 1) {
                    const br = self.env.store.getIfBranch(branches[i]);
                    const cond_val = try self.evalExprMinimal(br.cond, roc_ops, null);
                    const cond_ct_var = can.ModuleEnv.varFrom(br.cond);
                    const cond_rt_var = try self.translateTypeVar(self.env, cond_ct_var);
                    if (try self.boolValueIsTrue(cond_val, cond_rt_var)) {
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
                    const sorted_idx = accessor.findElementIndexByOriginal(i) orelse return error.TypeMismatch;
                    try accessor.setElement(sorted_idx, values.items[i], roc_ops);
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
                const elem_layout = try self.getRuntimeLayout(elem_rt_var);

                var values = try std.array_list.AlignedManaged(StackValue, null).initCapacity(self.allocator, elem_indices.len);
                defer values.deinit();

                for (elem_indices) |elem_idx| {
                    const val = try self.evalExprMinimal(elem_idx, roc_ops, elem_rt_var);
                    try values.append(val);
                }

                const list_layout = try self.getRuntimeLayout(list_rt_var);
                const dest = try self.pushRaw(list_layout, 0);
                if (dest.ptr == null) return dest;
                const header: *RocList = @ptrCast(@alignCast(dest.ptr.?));

                if (values.items.len == 0) {
                    header.* = RocList.empty();
                    return dest;
                }

                const elem_alignment = elem_layout.alignment(self.runtime_layout_store.targetUsize()).toByteUnits();
                const elem_alignment_u32: u32 = @intCast(elem_alignment);
                const elem_size: usize = @intCast(self.runtime_layout_store.layoutSize(elem_layout));
                const elements_refcounted = elem_layout.isRefcounted();

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
                const record_layout_idx = try self.runtime_layout_store.putRecord(union_layouts.items, union_names.items);
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
                        const field_name = self.env.getIdent(info.name);
                        const dest_field_idx = accessor.findFieldIndex(self.env, field_name) orelse return error.TypeMismatch;
                        const base_field_value = try base_accessor.getFieldByIndex(idx);
                        try accessor.setFieldByIndex(dest_field_idx, base_field_value, roc_ops);
                    }
                }

                for (fields, 0..) |field_idx_enum, explicit_index| {
                    const f = self.env.store.getRecordField(field_idx_enum);
                    const name_text = self.env.getIdent(f.name);
                    const dest_field_idx = accessor.findFieldIndex(self.env, name_text) orelse return error.TypeMismatch;
                    const val = field_values.items[explicit_index];

                    if (base_accessor_opt) |base_accessor| {
                        if (base_accessor.findFieldIndex(self.env, name_text) != null) {
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
                const list_layout = try self.getRuntimeLayout(rt_var);
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
                const resolved = self.runtime_types.resolveVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const tu = resolved.desc.content.structure.tag_union;
                const tags = self.runtime_types.getTagsSlice(tu.tags);
                // Find index by name
                var tag_index: usize = 0;
                var found = false;
                const name_text = self.env.getIdent(zero.name);
                var i: usize = 0;
                while (i < tags.len) : (i += 1) {
                    if (std.mem.eql(u8, self.env.getIdent(tags.items(.name)[i]), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                }
                const layout_val = try self.getRuntimeLayout(rt_var);
                if (self.isBoolLayout(layout_val) and (std.mem.eql(u8, name_text, "True") or std.mem.eql(u8, name_text, "False"))) {
                    try self.prepareBoolIndices(rt_var);
                    return try self.makeBoolValue(rt_var, std.mem.eql(u8, name_text, "True"));
                }
                // If layout is scalar (bool/uint), write discriminant directly
                if (layout_val.tag == .scalar) {
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = @intCast(tag_index);
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Record { tag: Discriminant, payload: ZST }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const tag_field = try acc.getFieldByIndex(tag_idx);
                    // write tag as int/byte
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = @intCast(tag_index);
                    } else return error.NotImplemented;
                    return dest;
                }
                return error.NotImplemented;
            },
            .e_tag => |tag| {
                // Construct a tag union value with payloads
                const rt_var = expected_rt_var orelse blk: {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    break :blk try self.translateTypeVar(self.env, ct_var);
                };
                // Unwrap nominal types and aliases to get the base tag union
                const resolved = self.resolveBaseVar(rt_var);
                if (resolved.desc.content != .structure or resolved.desc.content.structure != .tag_union) return error.NotImplemented;
                const name_text = self.env.getIdent(tag.name);
                var tag_list = std.array_list.AlignedManaged(types.Tag, null).init(self.allocator);
                defer tag_list.deinit();
                try self.appendUnionTags(rt_var, &tag_list);
                var tag_index: usize = 0;
                var found = false;
                for (tag_list.items, 0..) |tag_info, i| {
                    if (std.mem.eql(u8, self.env.getIdent(tag_info.name), name_text)) {
                        tag_index = i;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Invalid tag `{s}`", .{name_text});
                    self.triggerCrash(msg, true, roc_ops);
                    return error.Crash;
                }

                const layout_val = try self.getRuntimeLayout(rt_var);
                if (self.isBoolLayout(layout_val) and (std.mem.eql(u8, name_text, "True") or std.mem.eql(u8, name_text, "False"))) {
                    try self.prepareBoolIndices(rt_var);
                    return try self.makeBoolValue(rt_var, std.mem.eql(u8, name_text, "True"));
                }
                if (layout_val.tag == .scalar) {
                    // No payload union
                    var out = try self.pushRaw(layout_val, 0);
                    if (layout_val.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(out.ptr.?));
                        p.* = @intCast(tag_index);
                        return out;
                    } else if (layout_val.data.scalar.tag == .int) {
                        out.is_initialized = false;
                        out.setInt(@intCast(tag_index));
                        out.is_initialized = true;
                        return out;
                    }
                    return error.NotImplemented;
                } else if (layout_val.tag == .record) {
                    // Has payload: record { tag, payload }
                    var dest = try self.pushRaw(layout_val, 0);
                    var acc = try dest.asRecord(&self.runtime_layout_store);
                    const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                    const payload_field_idx = acc.findFieldIndex(self.env, "payload") orelse return error.NotImplemented;
                    // write tag discriminant
                    const tag_field = try acc.getFieldByIndex(tag_field_idx);
                    if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                        var tmp = tag_field;
                        tmp.is_initialized = false;
                        tmp.setInt(@intCast(tag_index));
                    } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                        const p: *u8 = @ptrCast(@alignCast(tag_field.ptr.?));
                        p.* = @intCast(tag_index);
                    } else return error.NotImplemented;

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
                        return dest;
                    } else if (args_exprs.len == 1) {
                        const arg_rt_var = arg_rt_vars[0];
                        const arg_val = try self.evalExprMinimal(args_exprs[0], roc_ops, arg_rt_var);
                        defer arg_val.decref(&self.runtime_layout_store, roc_ops);
                        if (payload_field.ptr) |payload_ptr| {
                            try arg_val.copyToPtr(&self.runtime_layout_store, payload_ptr, roc_ops);
                        }
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
                            elem_layouts[j] = try self.getRuntimeLayout(arg_rt_var);
                        }

                        const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                        const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);

                        if (payload_field.ptr) |payload_ptr| {
                            var tuple_dest = StackValue{ .layout = tuple_layout, .ptr = payload_ptr, .is_initialized = true };
                            var tup_acc = try tuple_dest.asTuple(&self.runtime_layout_store);
                            j = 0;
                            while (j < elem_values.len) : (j += 1) {
                                const sorted_idx = tup_acc.findElementIndexByOriginal(j) orelse return error.TypeMismatch;
                                try tup_acc.setElement(sorted_idx, elem_values[j], roc_ops);
                            }
                        }

                        return dest;
                    }
                }
                return error.NotImplemented;
            },
            .e_match => |m| {
                // Evaluate scrutinee once
                const scrutinee = try self.evalExprMinimal(m.cond, roc_ops, null);
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
                        if (!try self.patternMatchesBind(self.env.store.getMatchBranchPattern(bp_idx).pattern, scrutinee, scrutinee_rt_var, roc_ops, &temp_binds)) {
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
                            guard_pass = try self.boolValueIsTrue(guard_val, guard_rt_var);
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
                const succeeded = try self.boolValueIsTrue(cond_val, bool_rt_var);
                if (succeeded) {
                    const ct_var = can.ModuleEnv.varFrom(expr_idx);
                    const rt_var = try self.translateTypeVar(self.env, ct_var);
                    const layout_val = try self.getRuntimeLayout(rt_var);
                    return try self.pushRaw(layout_val, 0);
                }
                try self.handleExpectFailure(expect_expr.body, roc_ops);
                return error.Crash;
            },
            .e_dbg => |dbg_expr| {
                const inner_ct_var = can.ModuleEnv.varFrom(dbg_expr.expr);
                const inner_rt_var = try self.translateTypeVar(self.env, inner_ct_var);
                const value = try self.evalExprMinimal(dbg_expr.expr, roc_ops, inner_rt_var);
                const rendered = try self.renderValueRocWithType(value, inner_rt_var);
                defer self.allocator.free(rendered);
                roc_ops.dbg(rendered);
                return value;
            },
            // no tag handling in minimal evaluator
            .e_lambda => |lam| {
                // Build a closure value with empty captures using the runtime layout for the lambda's type
                const ct_var = can.ModuleEnv.varFrom(expr_idx);
                const rt_var = try self.translateTypeVar(self.env, ct_var);
                const closure_layout = try self.getRuntimeLayout(rt_var);
                // Expect a closure layout from type-to-layout translation
                if (closure_layout.tag != .closure) return error.NotImplemented;
                const value = try self.pushRaw(closure_layout, 0);
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
            .e_closure => |cls| {
                // Build a closure value with concrete captures. The closure references a lambda.
                const lam_expr = self.env.store.getExpr(cls.lambda_idx);
                if (lam_expr != .e_lambda) return error.NotImplemented;
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
                        // Next try active closure captures (top-most only) by name
                        if (self_interp.active_closures.items.len > 0) {
                            const top = self_interp.active_closures.items[self_interp.active_closures.items.len - 1];
                            if (top.layout.tag == .closure and top.ptr != null) {
                                const captures_layout = self_interp.runtime_layout_store.getLayout(top.layout.data.closure.captures_layout_idx);
                                const header_sz = @sizeOf(layout.Closure);
                                const cap_align = captures_layout.alignment(self_interp.runtime_layout_store.targetUsize());
                                const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                                const base: [*]u8 = @ptrCast(@alignCast(top.ptr.?));
                                const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                                const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                                var accessor = self_interp.runtime_layout_store; // just for type
                                _ = &accessor;
                                var rec_acc = (try rec_val.asRecord(&self_interp.runtime_layout_store));
                                const name_text = self_interp.env.getIdent(cap.name);
                                if (rec_acc.findFieldIndex(self_interp.env, name_text)) |fidx| {
                                    return rec_acc.getFieldByIndex(fidx) catch null;
                                }
                            }
                        }
                        // Finally try top-level defs by pattern idx
                        const all_defs = self_interp.env.store.sliceDefs(self_interp.env.all_defs);
                        for (all_defs) |def_idx| {
                            const def = self_interp.env.store.getDef(def_idx);
                            if (def.pattern == cap.pattern_idx) {
                                // Found the def! Evaluate it to get the captured value
                                return self_interp.evalMinimal(def.expr, ops) catch null;
                            }
                        }
                        return null;
                    }
                }.go;

                for (caps, 0..) |cap_idx, i| {
                    const cap = self.env.store.getCapture(cap_idx);
                    field_names[i] = cap.name;
                    const captured_val = resolveCapture(self, cap, roc_ops) orelse return error.NotImplemented;
                    field_layouts[i] = captured_val.layout;
                }

                const captures_layout_idx = try self.runtime_layout_store.putRecord(field_layouts, field_names);
                const captures_layout = self.runtime_layout_store.getLayout(captures_layout_idx);
                const closure_layout = Layout.closure(captures_layout_idx);
                const value = try self.pushRaw(closure_layout, 0);

                // Initialize header
                if (value.ptr) |ptr| {
                    const header: *layout.Closure = @ptrCast(@alignCast(ptr));
                    header.* = .{
                        .body_idx = lam.body,
                        .params = lam.args,
                        .captures_pattern_idx = @enumFromInt(@as(u32, 0)), // not used in minimal path
                        .captures_layout_idx = captures_layout_idx,
                        .lambda_expr_idx = cls.lambda_idx,
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
                    for (caps) |cap_idx2| {
                        const cap2 = self.env.store.getCapture(cap_idx2);
                        const cap_val2 = resolveCapture(self, cap2, roc_ops) orelse return error.NotImplemented;
                        const idx_opt = accessor.findFieldIndex(self.env, self.env.getIdent(cap2.name)) orelse return error.NotImplemented;
                        try accessor.setFieldByIndex(idx_opt, cap_val2, roc_ops);
                    }
                }
                return value;
            },
            .e_call => |call| {
                const all = self.env.store.sliceExpr(call.args);
                if (all.len == 0) return error.TypeMismatch;
                const func_idx = call.func;
                const arg_indices = all[0..];

                // Runtime unification for call: constrain return type from arg types
                const func_ct_var = can.ModuleEnv.varFrom(func_idx);
                const func_rt_var = try self.translateTypeVar(self.env, func_ct_var);
                var arg_rt_buf = try self.allocator.alloc(types.Var, arg_indices.len);
                defer self.allocator.free(arg_rt_buf);
                var i: usize = 0;
                while (i < arg_indices.len) : (i += 1) {
                    const arg_ct_var = can.ModuleEnv.varFrom(arg_indices[i]);
                    arg_rt_buf[i] = try self.translateTypeVar(self.env, arg_ct_var);
                }
                const poly_entry = try self.prepareCallWithFuncVar(0, @intCast(@intFromEnum(func_idx)), func_rt_var, arg_rt_buf);
                // Unify this call expression's return var with the function's constrained return var
                const call_ret_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const call_ret_rt_var = try self.translateTypeVar(self.env, call_ret_ct_var);
                _ = try unify.unifyWithContext(
                    self.env,
                    self.runtime_types,
                    &self.problems,
                    &self.snapshots,
                    &self.unify_scratch,
                    &self.unify_scratch.occurs_scratch,
                    call_ret_rt_var,
                    poly_entry.return_var,
                    false,
                );

                const func_val = try self.evalExprMinimal(func_idx, roc_ops, null);

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

                    const params = self.env.store.slicePatterns(header.params);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    // Provide closure context for capture lookup during body eval
                    try self.active_closures.append(func_val);
                    defer _ = self.active_closures.pop();
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count] });
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
                    return try self.evalExprMinimal(header.body_idx, roc_ops, null);
                }

                // Fallback: direct lambda expression (legacy minimal path)
                const func_expr = self.env.store.getExpr(func_idx);
                if (func_expr == .e_lambda) {
                    const lambda = func_expr.e_lambda;
                    const params = self.env.store.slicePatterns(lambda.args);
                    if (params.len != arg_indices.len) return error.TypeMismatch;
                    var bind_count: usize = 0;
                    while (bind_count < params.len) : (bind_count += 1) {
                        try self.bindings.append(.{ .pattern_idx = params[bind_count], .value = arg_values[bind_count] });
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
                    return try self.evalExprMinimal(lambda.body, roc_ops, null);
                }

                return error.NotImplemented;
            },
            .e_dot_access => |dot_access| {
                const receiver_ct_var = can.ModuleEnv.varFrom(dot_access.receiver);
                const receiver_rt_var = try self.translateTypeVar(self.env, receiver_ct_var);
                var receiver_value = try self.evalExprMinimal(dot_access.receiver, roc_ops, receiver_rt_var);
                defer receiver_value.decref(&self.runtime_layout_store, roc_ops);

                const method_args = dot_access.args;
                const field_name = self.env.getIdent(dot_access.field_name);
                const resolved_receiver = self.resolveBaseVar(receiver_rt_var);
                const is_list_receiver = resolved_receiver.desc.content == .structure and switch (resolved_receiver.desc.content.structure) {
                    .list, .list_unbound => true,
                    else => false,
                };
                const is_list_method = is_list_receiver and (std.mem.eql(u8, field_name, "len") or std.mem.eql(u8, field_name, "isEmpty"));
                const treat_as_method = method_args != null or is_list_method;

                if (!treat_as_method) {
                    if (receiver_value.layout.tag != .record) return error.TypeMismatch;
                    if (receiver_value.ptr == null) return error.ZeroSizedType;
                    const rec_data = self.runtime_layout_store.getRecordData(receiver_value.layout.data.record.idx);
                    if (rec_data.fields.count == 0) return error.ZeroSizedType;
                    var accessor = try receiver_value.asRecord(&self.runtime_layout_store);
                    const field_idx = accessor.findFieldIndex(self.env, field_name) orelse return error.TypeMismatch;
                    const field_value = try accessor.getFieldByIndex(field_idx);
                    return try self.pushCopy(field_value, roc_ops);
                }

                const arg_count = if (method_args) |span| span.span.len else 0;
                var arg_values: []StackValue = &.{};
                if (arg_count > 0) {
                    arg_values = try self.allocator.alloc(StackValue, arg_count);
                }
                defer {
                    if (arg_values.len > 0) {
                        var idx: usize = 0;
                        while (idx < arg_values.len) : (idx += 1) {
                            arg_values[idx].decref(&self.runtime_layout_store, roc_ops);
                        }
                        self.allocator.free(arg_values);
                    }
                }
                if (method_args) |span| {
                    var i: usize = 0;
                    while (i < arg_values.len) : (i += 1) {
                        const arg_expr_idx: can.CIR.Expr.Idx = @enumFromInt(span.span.start + i);
                        const arg_ct_var = can.ModuleEnv.varFrom(arg_expr_idx);
                        const arg_rt_var = try self.translateTypeVar(self.env, arg_ct_var);
                        arg_values[i] = try self.evalExprMinimal(arg_expr_idx, roc_ops, arg_rt_var);
                    }
                }

                const base_content = resolved_receiver.desc.content;
                if (base_content == .structure) {
                    switch (base_content.structure) {
                        .list, .list_unbound => {
                            if (std.mem.eql(u8, field_name, "len")) {
                                const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                const result_layout = try self.getRuntimeLayout(result_rt_var);
                                const length: usize = if (receiver_value.ptr) |ptr| blk: {
                                    const header: *const RocList = @ptrCast(@alignCast(ptr));
                                    break :blk header.len();
                                } else 0;
                                var out = try self.pushRaw(result_layout, 0);
                                out.is_initialized = false;
                                out.setInt(@intCast(length));
                                out.is_initialized = true;
                                return out;
                            }

                            if (std.mem.eql(u8, field_name, "isEmpty")) {
                                const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                const length: usize = if (receiver_value.ptr) |ptr| blk: {
                                    const header: *const RocList = @ptrCast(@alignCast(ptr));
                                    break :blk header.len();
                                } else 0;
                                return try self.makeBoolValue(result_rt_var, length == 0);
                            }
                        },
                        .nominal_type => |nominal| {
                            const nominal_name = self.env.getIdent(nominal.ident.ident_idx);
                            if (std.mem.eql(u8, nominal_name, "Box")) {
                                if (std.mem.eql(u8, field_name, "box")) {
                                    if (arg_values.len != 1) return error.TypeMismatch;
                                    const result_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(expr_idx));
                                    const result_layout = try self.getRuntimeLayout(result_rt_var);
                                    return try self.makeBoxValueFromLayout(result_layout, arg_values[0], roc_ops);
                                } else if (std.mem.eql(u8, field_name, "unbox")) {
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

                return error.NotImplemented;
            },
            .e_lookup_local => |lookup| {
                // Search bindings in reverse
                var i: usize = self.bindings.items.len;
                while (i > 0) {
                    i -= 1;
                    const b = self.bindings.items[i];
                    if (b.pattern_idx == lookup.pattern_idx) {
                        return try self.pushCopy(b.value, roc_ops);
                    }
                }
                // If not found, try active closure captures by variable name
                if (self.active_closures.items.len > 0) {
                    const pat = self.env.store.getPattern(lookup.pattern_idx);
                    if (pat == .assign) {
                        const var_name = self.env.getIdent(pat.assign.ident);
                        const cls_val = self.active_closures.items[self.active_closures.items.len - 1];
                        if (cls_val.layout.tag == .closure and cls_val.ptr != null) {
                            const captures_layout = self.runtime_layout_store.getLayout(cls_val.layout.data.closure.captures_layout_idx);
                            const header_sz = @sizeOf(layout.Closure);
                            const cap_align = captures_layout.alignment(self.runtime_layout_store.targetUsize());
                            const aligned_off = std.mem.alignForward(usize, header_sz, @intCast(cap_align.toByteUnits()));
                            const base: [*]u8 = @ptrCast(@alignCast(cls_val.ptr.?));
                            const rec_ptr: *anyopaque = @ptrCast(base + aligned_off);
                            const rec_val = StackValue{ .layout = captures_layout, .ptr = rec_ptr, .is_initialized = true };
                            var accessor = try rec_val.asRecord(&self.runtime_layout_store);
                            if (accessor.findFieldIndex(self.env, var_name)) |fidx| {
                                const field_val = try accessor.getFieldByIndex(fidx);
                                return try self.pushCopy(field_val, roc_ops);
                            }
                        }
                    }
                }

                if (builtin.mode == .Debug) {
                    // In debug builds, check if this pattern corresponds to a top-level def
                    // If we find it, that means it should have been in bindings - this is a compiler bug
                    const all_defs = self.env.store.sliceDefs(self.env.all_defs);
                    for (all_defs) |def_idx| {
                        const def = self.env.store.getDef(def_idx);
                        if (def.pattern == lookup.pattern_idx) {
                            const pat = self.env.store.getPattern(lookup.pattern_idx);
                            const var_name = switch (pat) {
                                .assign => |a| self.env.getIdent(a.ident),
                                else => "(non-assign pattern)",
                            };
                            std.debug.panic(
                                "Bug in compiler: top-level definition '{s}' (pattern_idx={}) should have been added to bindings but wasn't found there",
                                .{ var_name, lookup.pattern_idx },
                            );
                        }
                    }
                }

                return error.NotImplemented;
            },
            .e_lookup_external => |lookup| {
                // Cross-module reference - look up in imported module
                const import_idx: usize = @intFromEnum(lookup.module_idx);
                if (import_idx >= self.other_envs.len) {
                    return error.NotImplemented;
                }
                const other_env = self.other_envs[import_idx];

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
                const target_ct_var = can.ModuleEnv.varFrom(target_def.expr);
                const target_rt_var = try self.translateTypeVar(self.env, target_ct_var);
                const result = try self.evalExprMinimal(target_def.expr, roc_ops, target_rt_var);

                return result;
            },
            .e_unary_minus => |unary| {
                const operand_ct_var = can.ModuleEnv.varFrom(unary.expr);
                const operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);
                const operand = try self.evalExprMinimal(unary.expr, roc_ops, operand_rt_var);
                defer operand.decref(&self.runtime_layout_store, roc_ops);

                const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                const result_layout = try self.getRuntimeLayout(result_rt_var);
                if (result_layout.tag != .scalar) return error.TypeMismatch;

                return switch (result_layout.data.scalar.tag) {
                    .int => {
                        if (!(operand.layout.tag == .scalar and operand.layout.data.scalar.tag == .int)) {
                            return error.TypeMismatch;
                        }
                        const value = operand.asI128();
                        var out = try self.pushRaw(result_layout, 0);
                        out.is_initialized = false;
                        out.setInt(-value);
                        out.is_initialized = true;
                        return out;
                    },
                    .frac => switch (result_layout.data.scalar.data.frac) {
                        .dec => {
                            const operand_dec = try self.stackValueToDecimal(operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *RocDec = @ptrCast(@alignCast(ptr));
                                dest.* = RocDec{ .num = -operand_dec.num };
                            }
                            return out;
                        },
                        .f32 => {
                            const operand_float = try self.stackValueToFloat(f32, operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *f32 = @ptrCast(@alignCast(ptr));
                                dest.* = -operand_float;
                            }
                            return out;
                        },
                        .f64 => {
                            const operand_float = try self.stackValueToFloat(f64, operand);
                            const out = try self.pushRaw(result_layout, 0);
                            if (out.ptr) |ptr| {
                                const dest: *f64 = @ptrCast(@alignCast(ptr));
                                dest.* = -operand_float;
                            }
                            return out;
                        },
                    },
                    else => error.TypeMismatch,
                };
            },
            .e_unary_not => |unary| {
                const operand_ct_var = can.ModuleEnv.varFrom(unary.expr);
                const operand_rt_var = try self.translateTypeVar(self.env, operand_ct_var);

                const operand = try self.evalExprMinimal(unary.expr, roc_ops, operand_rt_var);
                defer operand.decref(&self.runtime_layout_store, roc_ops);

                const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
                const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
                const truthy = try self.boolValueIsTrue(operand, operand_rt_var);
                return try self.makeBoolValue(result_rt_var, !truthy);
            },
            .e_runtime_error => |rt_err| {
                _ = rt_err;
                self.triggerCrash("runtime error", false, roc_ops);
                return error.Crash;
            },
            // no if handling in minimal evaluator
            // no second e_binop case; handled above
            else => return error.NotImplemented,
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

    fn pushRaw(self: *Interpreter, layout_val: Layout, initial_size: usize) !StackValue {
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
        const dest = StackValue{ .layout = src.layout, .ptr = ptr, .is_initialized = true };
        if (size > 0 and src.ptr != null and ptr != null) {
            try src.copyToPtr(&self.runtime_layout_store, ptr.?, roc_ops);
        }
        return dest;
    }

    fn triggerCrash(self: *Interpreter, message: []const u8, owned: bool, roc_ops: *RocOps) void {
        defer if (owned) self.allocator.free(@constCast(message));
        roc_ops.crash(message);
    }

    fn handleExpectFailure(self: *Interpreter, snippet_expr_idx: can.CIR.Expr.Idx, roc_ops: *RocOps) !void {
        const region = self.env.store.getExprRegion(snippet_expr_idx);
        const slice = self.env.getSource(region);
        const trimmed = std.mem.trim(u8, slice, " \t\n\r");
        const message = try std.fmt.allocPrint(self.allocator, "Expect failed: {s}", .{trimmed});
        defer self.allocator.free(message);

        const expect_args = RocExpectFailed{
            .utf8_bytes = @constCast(message.ptr),
            .len = message.len,
        };
        roc_ops.roc_expect_failed(&expect_args, roc_ops.env);
        roc_ops.crash(message);
    }

    fn extractBoolTagIndex(self: *Interpreter, value: StackValue, bool_var: ?types.Var) !usize {
        const raw_index: usize = switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .bool => {
                    const ptr = value.ptr orelse return error.TypeMismatch;
                    const b: *const u8 = @ptrCast(@alignCast(ptr));
                    return @intCast(b.*);
                },
                .int => {
                    return @intCast(value.asI128());
                },
                else => return error.TypeMismatch,
            },
            .record => {
                var acc = try value.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.TypeMismatch;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                const tag_value = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                return try self.extractBoolTagIndex(tag_value, bool_var);
            },
            else => return error.TypeMismatch,
        };

        if (bool_var) |_var| {
            try self.prepareBoolIndices(_var);
            if (raw_index == self.bool_false_index or raw_index == self.bool_true_index) {
                return raw_index;
            }
            return error.TypeMismatch;
        }

        return raw_index;
    }

    fn boolValueIsTrue(self: *Interpreter, value: StackValue, rt_var: types.Var) !bool {
        // Check that the layout is compatible with Bool
        if (!self.isBoolLayout(value.layout)) return error.TypeMismatch;

        // Try to verify this is actually a Bool type
        try self.prepareBoolIndices(rt_var);
        if (!try self.runtimeVarIsBool(rt_var)) return error.TypeMismatch;

        // Bool values are ALWAYS stored with canonical indices: 0=False, 1=True
        const idx = try self.extractBoolTagIndex(value, rt_var);
        return idx == self.bool_true_index;
    }

    const NumericKind = enum { int, dec, f32, f64 };

    fn numericKindFromLayout(self: *Interpreter, layout_val: Layout) ?NumericKind {
        _ = self;
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
            self.runtime_layout_store.layouts_by_var.entries[map_idx] = std.mem.zeroes(layout.Idx);
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
        const lhs_kind_opt = self.numericKindFromLayout(lhs.layout);
        const rhs_kind_opt = self.numericKindFromLayout(rhs.layout);
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

    fn evalArithmeticBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        expr_idx: can.CIR.Expr.Idx,
        lhs: StackValue,
        rhs: StackValue,
        lhs_rt_var: types.Var,
        rhs_rt_var: types.Var,
    ) !StackValue {
        const result_ct_var = can.ModuleEnv.varFrom(expr_idx);
        const result_rt_var = try self.translateTypeVar(self.env, result_ct_var);
        var result_layout = try self.getRuntimeLayout(result_rt_var);

        result_layout = try self.adjustNumericResultLayout(result_rt_var, result_layout, lhs, lhs_rt_var, rhs, rhs_rt_var);

        if (result_layout.tag != .scalar) return error.TypeMismatch;
        return switch (result_layout.data.scalar.tag) {
            .int => try self.evalIntBinop(op, result_layout, lhs, rhs),
            .frac => switch (result_layout.data.scalar.data.frac) {
                .dec => try self.evalDecBinop(op, result_layout, lhs, rhs),
                .f32 => try self.evalFloatBinop(f32, op, result_layout, lhs, rhs),
                .f64 => try self.evalFloatBinop(f64, op, result_layout, lhs, rhs),
            },
            else => error.TypeMismatch,
        };
    }

    fn evalIntBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        if (!(lhs.layout.tag == .scalar and lhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;
        if (!(rhs.layout.tag == .scalar and rhs.layout.data.scalar.tag == .int)) return error.TypeMismatch;

        const lhs_val = lhs.asI128();
        const rhs_val = rhs.asI128();

        const result_val: i128 = switch (op) {
            .add => lhs_val + rhs_val,
            .sub => lhs_val - rhs_val,
            .mul => lhs_val * rhs_val,
            .div, .div_trunc => blk: {
                if (rhs_val == 0) return error.DivisionByZero;
                break :blk @divTrunc(lhs_val, rhs_val);
            },
            .rem => blk: {
                if (rhs_val == 0) return error.DivisionByZero;
                break :blk @rem(lhs_val, rhs_val);
            },
            else => return error.NotImplemented,
        };

        var out = try self.pushRaw(result_layout, 0);
        out.is_initialized = false;
        out.setInt(result_val);
        out.is_initialized = true;
        return out;
    }

    fn evalDecBinop(
        self: *Interpreter,
        op: can.CIR.Expr.Binop.Op,
        result_layout: Layout,
        lhs: StackValue,
        rhs: StackValue,
    ) !StackValue {
        const lhs_dec = try self.stackValueToDecimal(lhs);
        const rhs_dec = try self.stackValueToDecimal(rhs);

        const result_dec: RocDec = switch (op) {
            .add => RocDec{ .num = lhs_dec.num + rhs_dec.num },
            .sub => RocDec{ .num = lhs_dec.num - rhs_dec.num },
            .mul => RocDec{ .num = @divTrunc(lhs_dec.num * rhs_dec.num, RocDec.one_point_zero_i128) },
            .div, .div_trunc => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                const scaled_lhs = lhs_dec.num * RocDec.one_point_zero_i128;
                break :blk RocDec{ .num = @divTrunc(scaled_lhs, rhs_dec.num) };
            },
            .rem => blk: {
                if (rhs_dec.num == 0) return error.DivisionByZero;
                break :blk RocDec{ .num = @rem(lhs_dec.num, rhs_dec.num) };
            },
            else => return error.NotImplemented,
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
            else => return error.NotImplemented,
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

    fn isBoolLayout(self: *Interpreter, layout_val: Layout) bool {
        _ = self;
        if (layout_val.tag != .scalar) return false;
        return layout_val.data.scalar.tag == .bool or layout_val.data.scalar.tag == .int;
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
        rhs_var: types.Var,
    ) StructuralEqError!bool {
        // Handle scalar comparisons (bool, numbers, strings) directly.
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar) {
            const lhs_scalar = lhs.layout.data.scalar;
            const rhs_scalar = rhs.layout.data.scalar;
            if (lhs_scalar.tag != rhs_scalar.tag) return error.TypeMismatch;

            switch (lhs_scalar.tag) {
                .bool => {
                    const lhs_bool = try self.boolValueIsTrue(lhs, lhs_var);
                    const rhs_bool = try self.boolValueIsTrue(rhs, rhs_var);
                    return lhs_bool == rhs_bool;
                },
                .int, .frac => {
                    const order = try self.compareNumericScalars(lhs, rhs);
                    return order == .eq;
                },
                .str => {
                    if (lhs.ptr == null or rhs.ptr == null) return error.TypeMismatch;
                    const lhs_str: *const RocStr = @ptrCast(@alignCast(lhs.ptr.?));
                    const rhs_str: *const RocStr = @ptrCast(@alignCast(rhs.ptr.?));
                    return std.mem.eql(u8, lhs_str.asSlice(), rhs_str.asSlice());
                },
                else => return error.NotImplemented,
            }
        }

        // Ensure runtime vars resolve to the same descriptor before structural comparison.
        const lhs_resolved = self.resolveBaseVar(lhs_var);
        const lhs_content = lhs_resolved.desc.content;
        if (lhs_content != .structure) return error.NotImplemented;

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
            .list => |elem_var| {
                return try self.structuralEqualList(lhs, rhs, elem_var);
            },
            .empty_record => true,
            .list_unbound, .record_unbound, .fn_pure, .fn_effectful, .fn_unbound, .nominal_type, .empty_tag_union, .box => error.NotImplemented,
            .str => error.NotImplemented,
            .num => error.NotImplemented,
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
            const lhs_sorted = lhs_acc.findElementIndexByOriginal(index) orelse index;
            const rhs_sorted = rhs_acc.findElementIndexByOriginal(index) orelse index;
            const lhs_elem = try lhs_acc.getElement(lhs_sorted);
            const rhs_elem = try rhs_acc.getElement(rhs_sorted);
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
                return error.NotImplemented;
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
            const lhs_sorted = lhs_tuple.findElementIndexByOriginal(idx) orelse idx;
            const rhs_sorted = rhs_tuple.findElementIndexByOriginal(idx) orelse idx;
            const lhs_elem = try lhs_tuple.getElement(lhs_sorted);
            const rhs_elem = try rhs_tuple.getElement(rhs_sorted);
            const args_equal = try self.valuesStructurallyEqual(lhs_elem, arg_vars[idx], rhs_elem, arg_vars[idx]);
            if (!args_equal) {
                return false;
            }
        }

        return true;
    }

    fn runtimeVarIsBool(self: *Interpreter, rt_var: types.Var) !bool {
        var resolved = self.runtime_types.resolveVar(rt_var);

        unwrap: while (true) {
            switch (resolved.desc.content) {
                .alias => |al| {
                    const backing = self.runtime_types.getAliasBackingVar(al);
                    resolved = self.runtime_types.resolveVar(backing);
                },
                .structure => |st| switch (st) {
                    .nominal_type => |nom| {
                        const backing = self.runtime_types.getNominalBackingVar(nom);
                        resolved = self.runtime_types.resolveVar(backing);
                    },
                    else => {
                        break :unwrap;
                    },
                },
                else => {
                    break :unwrap;
                },
            }
        }

        if (resolved.desc.content != .structure) {
            return false;
        }
        const structure = resolved.desc.content.structure;
        if (structure != .tag_union) {
            return false;
        }
        const tu = structure.tag_union;

        const scratch_tags_top = self.scratch_tags.items.len;
        defer self.scratch_tags.shrinkRetainingCapacity(scratch_tags_top);

        const tag_slice = self.runtime_types.getTagsSlice(tu.tags);
        for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
            _ = try self.scratch_tags.append(.{ .name = name, .args = args });
        }

        var current_ext = tu.ext;
        while (true) {
            const resolved_ext = self.runtime_types.resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| switch (ext_flat_type) {
                    .empty_tag_union => break,
                    .tag_union => |ext_tag_union| {
                        if (ext_tag_union.tags.len() > 0) {
                            const ext_tag_slice = self.runtime_types.getTagsSlice(ext_tag_union.tags);
                            for (ext_tag_slice.items(.name), ext_tag_slice.items(.args)) |name, args| {
                                _ = try self.scratch_tags.append(.{ .name = name, .args = args });
                            }
                            current_ext = ext_tag_union.ext;
                        } else {
                            break;
                        }
                    },
                    else => return Error.InvalidTagExt,
                },
                .alias => |alias| {
                    current_ext = self.runtime_types.getAliasBackingVar(alias);
                },
                else => return Error.InvalidTagExt,
            }
        }

        const tags = self.scratch_tags.items[scratch_tags_top..];
        if (tags.len == 0 or tags.len > 2) {
            return false;
        }

        var false_idx: ?usize = null;
        var true_idx: ?usize = null;
        for (tags, 0..) |tag, i| {
            // Use env to look up tag names - works for both Bool module and copied Bool types
            const name_text = self.env.getIdent(tag.name);
            if (std.mem.eql(u8, name_text, "False")) {
                false_idx = i;
            } else if (std.mem.eql(u8, name_text, "True")) {
                true_idx = i;
            } else {
                return false;
            }
        }

        // Accept types that have True OR False (for anonymous tags like [True]_others)
        // Not just full Bool types with both tags
        if (false_idx == null and true_idx == null) {
            return false;
        }
        // IMPORTANT: Bool values are ALWAYS stored with canonical indices: False=0, True=1
        // This is true regardless of the tag order in the type.
        // The tag list indices (false_idx, true_idx) tell us which tag is which,
        // but the actual runtime values always use canonical indices.
        self.bool_false_index = 0; // False is always 0
        self.bool_true_index = 1; // True is always 1
        return true;
    }

    fn getCanonicalBoolRuntimeVar(self: *Interpreter) !types.Var {
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

    fn prepareBoolIndices(self: *Interpreter, rt_var: types.Var) !void {
        if (try self.runtimeVarIsBool(rt_var)) return;
        const canonical = try self.getCanonicalBoolRuntimeVar();
        _ = try self.runtimeVarIsBool(canonical);
    }

    fn ensureBoolRuntimeVar(self: *Interpreter, module: *can.ModuleEnv, compile_var: types.Var, runtime_var: types.Var) !types.Var {
        if (try self.runtimeVarIsBool(runtime_var)) return runtime_var;

        const canonical = try self.getCanonicalBoolRuntimeVar();
        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(compile_var));
        try self.translate_cache.put(key, canonical);
        return canonical;
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

    fn appendUnionTags(self: *Interpreter, runtime_var: types.Var, list: *std.array_list.AlignedManaged(types.Tag, null)) !void {
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
    }

    const TagValue = struct {
        index: usize,
        payload: ?StackValue,
    };

    fn extractTagValue(self: *Interpreter, value: StackValue, union_rt_var: types.Var) !TagValue {
        switch (value.layout.tag) {
            .scalar => switch (value.layout.data.scalar.tag) {
                .bool => {
                    const idx = try self.extractBoolTagIndex(value, null);
                    return .{ .index = idx, .payload = null };
                },
                .int => {
                    return .{ .index = @intCast(value.asI128()), .payload = null };
                },
                else => return error.TypeMismatch,
            },
            .record => {
                var acc = try value.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.TypeMismatch;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                var tag_index: usize = undefined;
                if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                    var tmp = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                    tag_index = @intCast(tmp.asI128());
                } else if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .bool) {
                    const ptr = tag_field.ptr orelse return error.TypeMismatch;
                    const b: *const u8 = @ptrCast(@alignCast(ptr));
                    tag_index = b.*;
                } else return error.TypeMismatch;

                var payload_value: ?StackValue = null;
                if (acc.findFieldIndex(self.env, "payload")) |payload_idx| {
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
                            const arg_layout = try self.getRuntimeLayout(arg_vars[0]);
                            payload_value = StackValue{
                                .layout = arg_layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        } else {
                            var elem_layouts = try self.allocator.alloc(Layout, arg_vars.len);
                            defer self.allocator.free(elem_layouts);
                            for (arg_vars, 0..) |arg_var, i| {
                                elem_layouts[i] = try self.getRuntimeLayout(arg_var);
                            }
                            const tuple_layout_idx = try self.runtime_layout_store.putTuple(elem_layouts);
                            const tuple_layout = self.runtime_layout_store.getLayout(tuple_layout_idx);
                            payload_value = StackValue{
                                .layout = tuple_layout,
                                .ptr = field_value.ptr,
                                .is_initialized = field_value.is_initialized,
                            };
                        }
                    }
                }

                return .{ .index = tag_index, .payload = payload_value };
            },
            else => return error.TypeMismatch,
        }
    }

    fn makeBoolValue(self: *Interpreter, target_rt_var: types.Var, truthy: bool) !StackValue {
        const layout_val = try self.getRuntimeLayout(target_rt_var);
        if (!self.isBoolLayout(layout_val)) return error.NotImplemented;
        try self.prepareBoolIndices(target_rt_var);
        const chosen_index: u8 = if (truthy) self.bool_true_index else self.bool_false_index;
        var out = try self.pushRaw(layout_val, 0);

        switch (layout_val.tag) {
            .scalar => switch (layout_val.data.scalar.tag) {
                .bool => {
                    const ptr = out.ptr orelse return error.NotImplemented;
                    const p: *u8 = @ptrCast(@alignCast(ptr));
                    p.* = chosen_index;
                    return out;
                },
                .int => {
                    out.is_initialized = false;
                    out.setInt(chosen_index);
                    out.is_initialized = true;
                    return out;
                },
                else => return error.NotImplemented,
            },
            .record => {
                var acc = try out.asRecord(&self.runtime_layout_store);
                const tag_field_idx = acc.findFieldIndex(self.env, "tag") orelse return error.NotImplemented;
                const tag_field = try acc.getFieldByIndex(tag_field_idx);
                var tag_slot = StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true };
                switch (tag_slot.layout.tag) {
                    .scalar => switch (tag_slot.layout.data.scalar.tag) {
                        .bool => {
                            const ptr = tag_slot.ptr orelse return error.NotImplemented;
                            const p: *u8 = @ptrCast(@alignCast(ptr));
                            p.* = chosen_index;
                        },
                        .int => {
                            tag_slot.is_initialized = false;
                            tag_slot.setInt(chosen_index);
                            tag_slot.is_initialized = true;
                        },
                        else => return error.NotImplemented,
                    },
                    else => return error.NotImplemented,
                }

                if (acc.findFieldIndex(self.env, "payload")) |payload_idx| {
                    const payload_field = try acc.getFieldByIndex(payload_idx);
                    const payload_size = self.runtime_layout_store.layoutSize(payload_field.layout);
                    if (payload_size > 0 and payload_field.ptr != null) {
                        @memset(@as([*]u8, @ptrCast(payload_field.ptr.?))[0..payload_size], 0);
                    }
                }

                return out;
            },
            else => return error.NotImplemented,
        }
    }

    fn compareValues(self: *Interpreter, lhs: StackValue, rhs: StackValue, op: can.CIR.Expr.Binop.Op) !bool {
        // Handle numeric comparisons
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar and
            lhs.layout.data.scalar.tag == .int and rhs.layout.data.scalar.tag == .int)
        {
            const lhs_val = lhs.asI128();
            const rhs_val = rhs.asI128();

            return switch (op) {
                .eq => lhs_val == rhs_val,
                .ne => lhs_val != rhs_val,
                .lt => lhs_val < rhs_val,
                .le => lhs_val <= rhs_val,
                .gt => lhs_val > rhs_val,
                .ge => lhs_val >= rhs_val,
                else => error.NotImplemented,
            };
        }

        // Handle bool comparisons (like True == False)
        if (lhs.layout.tag == .scalar and rhs.layout.tag == .scalar and
            lhs.layout.data.scalar.tag == .bool and rhs.layout.data.scalar.tag == .bool)
        {
            const lhs_val = lhs.asBool();
            const rhs_val = rhs.asBool();

            return switch (op) {
                .eq => lhs_val == rhs_val,
                .ne => lhs_val != rhs_val,
                else => error.NotImplemented,
            };
        }

        // For now, only numeric and bool comparisons are supported
        _ = self;
        return error.NotImplemented;
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
        const dest = try self.pushRaw(list_layout, 0);
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
    ) !bool {
        const pat = self.env.store.getPattern(pattern_idx);
        switch (pat) {
            .assign => |_| {
                // Bind entire value to this pattern
                const copied = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = copied });
                return true;
            },
            .as => |as_pat| {
                const before = out_binds.items.len;
                if (!try self.patternMatchesBind(as_pat.pattern, value, value_rt_var, roc_ops, out_binds)) {
                    self.trimBindingList(out_binds, before, roc_ops);
                    return false;
                }

                const alias_value = try self.pushCopy(value, roc_ops);
                try out_binds.append(.{ .pattern_idx = pattern_idx, .value = alias_value });
                return true;
            },
            .underscore => return true,
            .num_literal => |il| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .int)) return false;
                const lit = il.value.toI128();
                return value.asI128() == lit;
            },
            .str_literal => |sl| {
                if (!(value.layout.tag == .scalar and value.layout.data.scalar.tag == .str)) return false;
                const lit = self.env.getString(sl.literal);
                const rs: *const RocStr = @ptrCast(@alignCast(value.ptr.?));
                return std.mem.eql(u8, rs.asSlice(), lit);
            },
            .nominal => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds);
            },
            .nominal_external => |n| {
                const underlying = self.resolveBaseVar(value_rt_var);
                return try self.patternMatchesBind(n.backing_pattern, value, underlying.var_, roc_ops, out_binds);
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
                    const sorted_idx = accessor.findElementIndexByOriginal(idx) orelse idx;
                    if (sorted_idx >= accessor.getElementCount()) return false;
                    const elem_value = try accessor.getElement(sorted_idx);
                    const before = out_binds.items.len;
                    const matched = try self.patternMatchesBind(pat_ids[idx], elem_value, elem_vars[idx], roc_ops, out_binds);
                    if (!matched) {
                        self.trimBindingList(out_binds, before, roc_ops);
                        return false;
                    }
                }

                return true;
            },
            .list => |list_pat| {
                if (value.layout.tag != .list and value.layout.tag != .list_of_zst) return false;

                const list_layout = try self.getRuntimeLayout(value_rt_var);

                const list_rt_var = try self.translateTypeVar(self.env, can.ModuleEnv.varFrom(pattern_idx));
                const list_rt_content = self.runtime_types.resolveVar(list_rt_var).desc.content;
                std.debug.assert(list_rt_content == .structure);
                std.debug.assert(list_rt_content.structure == .list);

                const elem_rt_var = list_rt_content.structure.list;
                const elem_layout = try self.getRuntimeLayout(elem_rt_var);

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
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds);
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
                        const matched = try self.patternMatchesBind(suffix_pattern_idx, elem_value, elem_rt_var, roc_ops, out_binds);
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
                        if (!try self.patternMatchesBind(rest_pat_idx, rest_value, value_rt_var, roc_ops, out_binds)) {
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
                        const matched = try self.patternMatchesBind(non_rest_patterns[idx], elem_value, elem_rt_var, roc_ops, out_binds);
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
                    const field_name = self.env.getIdent(destruct.label);

                    const field_index = accessor.findFieldIndex(self.env, field_name) orelse return false;
                    const field_value = try accessor.getFieldByIndex(field_index);
                    const field_ct_var = can.ModuleEnv.varFrom(destruct_idx);
                    const field_var = try self.translateTypeVar(self.env, field_ct_var);

                    const inner_pattern_idx = switch (destruct.kind) {
                        .Required => |p_idx| p_idx,
                        .SubPattern => |p_idx| p_idx,
                    };

                    const before = out_binds.items.len;
                    if (!try self.patternMatchesBind(inner_pattern_idx, field_value, field_var, roc_ops, out_binds)) {
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

                const expected_name = self.env.getIdent(tag_pat.name);
                if (try self.runtimeVarIsBool(value_rt_var)) {
                    const actual_name = if (tag_data.index == self.bool_true_index) "True" else "False";
                    if (!std.mem.eql(u8, expected_name, actual_name)) return false;
                } else {
                    try self.prepareBoolIndices(value_rt_var);
                    const actual_name = self.env.getIdent(tag_list.items[tag_data.index].name);
                    if (!std.mem.eql(u8, expected_name, actual_name)) return false;
                }

                const arg_patterns = self.env.store.slicePatterns(tag_pat.args);
                const arg_vars_range = tag_list.items[tag_data.index].args;
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
                    if (!try self.patternMatchesBind(arg_patterns[0], payload_value, arg_vars[0], roc_ops, out_binds)) {
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
                    const sorted_idx = payload_tuple.findElementIndexByOriginal(j) orelse j;
                    if (sorted_idx >= payload_tuple.getElementCount()) {
                        self.trimBindingList(out_binds, start_len, roc_ops);
                        return false;
                    }
                    const elem_val = try payload_tuple.getElement(sorted_idx);
                    if (!try self.patternMatchesBind(arg_patterns[j], elem_val, arg_vars[j], roc_ops, out_binds)) {
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
        var it = self.poly_cache.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.args.len > 0) {
                self.allocator.free(@constCast(entry.value_ptr.args));
            }
        }
        self.poly_cache.deinit();
        self.module_envs.deinit(self.allocator);
        self.module_ids.deinit(self.allocator);
        self.var_to_layout_slot.deinit();
        self.runtime_layout_store.deinit();
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);
        self.snapshots.deinit();
        self.problems.deinit(self.allocator);
        self.unify_scratch.deinit();
        self.stack_memory.deinit();
        self.bindings.deinit();
        self.active_closures.deinit();
        self.scratch_tags.deinit();
        self.imported_modules.deinit();
    }

    /// Get the module environment for a given origin module identifier.
    /// Returns the current module's env if the identifier matches, otherwise looks it up in the module map.
    fn getModuleEnvForOrigin(self: *const Interpreter, origin_module: base_pkg.Ident.Idx) ?*const can.ModuleEnv {
        // Check if it's the current module
        if (self.env.module_name_idx) |current_name| {
            if (current_name == origin_module) {
                return self.env;
            }
        }
        // Look up in imported modules
        return self.module_envs.get(origin_module);
    }

    /// Get the numeric module ID for a given origin module identifier.
    /// Returns 0 for the current module, otherwise looks it up in the module ID map.
    fn getModuleIdForOrigin(self: *const Interpreter, origin_module: base_pkg.Ident.Idx) u32 {
        // Check if it's the current module
        if (self.env.module_name_idx) |current_name| {
            if (current_name == origin_module) {
                return self.current_module_id;
            }
        }
        // Look up in imported modules (should always exist if getModuleEnvForOrigin succeeded)
        return self.module_ids.get(origin_module) orelse self.current_module_id;
    }

    /// Build a fully-qualified method identifier in the form "TypeName.method".
    fn getMethodQualifiedIdent(self: *const Interpreter, nominal_ident: base_pkg.Ident.Idx, method_name: base_pkg.Ident.Idx, buf: []u8) ![]const u8 {
        const ident_store = self.env.common.getIdentStore();
        const type_name = ident_store.getSlice(nominal_ident);
        const method_name_str = ident_store.getSlice(method_name);
        return std.fmt.bufPrint(buf, "{s}.{s}", .{ type_name, method_name_str });
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

    /// Get the layout for a runtime type var using the O(1) biased slot array.
    pub fn getRuntimeLayout(self: *Interpreter, type_var: types.Var) !layout.Layout {
        const resolved = self.runtime_types.resolveVar(type_var);
        const idx: usize = @intFromEnum(resolved.var_);
        try self.ensureVarLayoutCapacity(idx + 1);
        const slot_ptr = &self.var_to_layout_slot.items[idx];
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

        const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(resolved.var_));
        if (self.translate_cache.get(key)) |found| return found;

        const out_var = blk: {
            switch (resolved.desc.content) {
                .structure => |flat| {
                    switch (flat) {
                        .str => {
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .str });
                        },
                        .num => |initial_num| {
                            const compact_num: types.Num.Compact = prec: {
                                var num = initial_num;
                                while (true) {
                                    switch (num) {
                                        .num_compact => |compact| break :prec compact,
                                        .int_precision => |precision| break :prec .{ .int = precision },
                                        .frac_precision => |precision| break :prec .{ .frac = precision },
                                        // For polymorphic types, use default precision
                                        .num_unbound => |_| {
                                            // TODO: Should we consider requirements here?
                                            break :prec .{ .int = types.Num.Int.Precision.default };
                                        },
                                        .int_unbound => {
                                            // TODO: Should we consider requirements here?
                                            break :prec .{ .int = types.Num.Int.Precision.default };
                                        },
                                        .frac_unbound => {
                                            // TODO: Should we consider requirements here?
                                            break :prec .{ .frac = types.Num.Frac.Precision.default };
                                        },
                                        .num_poly => |var_| {
                                            const next_type = module.types.resolveVar(var_).desc.content;
                                            if (next_type == .structure and next_type.structure == .num) {
                                                num = next_type.structure.num;
                                            } else if (next_type == .flex) {
                                                break :prec .{ .int = types.Num.Int.Precision.default };
                                            } else {
                                                return Error.InvalidNumExt;
                                            }
                                        },
                                        .int_poly => |var_| {
                                            const next_type = module.types.resolveVar(var_).desc.content;
                                            if (next_type == .structure and next_type.structure == .num) {
                                                num = next_type.structure.num;
                                            } else if (next_type == .flex) {
                                                break :prec .{ .int = types.Num.Int.Precision.default };
                                            } else {
                                                return Error.InvalidNumExt;
                                            }
                                        },
                                        .frac_poly => |var_| {
                                            const next_type = module.types.resolveVar(var_).desc.content;
                                            if (next_type == .structure and next_type.structure == .num) {
                                                num = next_type.structure.num;
                                            } else if (next_type == .flex) {
                                                break :prec .{ .frac = types.Num.Frac.Precision.default };
                                            } else {
                                                return Error.InvalidNumExt;
                                            }
                                        },
                                    }
                                }
                            };
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = compact_num } } });
                        },
                        .tag_union => |tu| {
                            var rt_tag_args = try std.ArrayListUnmanaged(types.Var).initCapacity(self.allocator, 8);
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
                        .box => |elem_var| {
                            const rt_elem = try self.translateTypeVar(module, elem_var);
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .box = rt_elem } });
                        },
                        .list => |elem_var| {
                            const rt_elem = try self.translateTypeVar(module, elem_var);
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .list = rt_elem } });
                        },
                        .list_unbound => {
                            const elem_var = try self.runtime_types.freshFromContent(.{ .flex = types.Flex.init() });
                            break :blk try self.runtime_types.freshFromContent(.{ .structure = .{ .list = elem_var } });
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
                            const content = try self.runtime_types.mkNominal(nom.ident, rt_backing, buf, nom.origin_module);
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
                .flex => |flex| {
                    // Translate static dispatch constraints if present
                    const rt_flex = if (flex.constraints.len() > 0) blk_flex: {
                        const ct_constraints = module.types.sliceStaticDispatchConstraints(flex.constraints);
                        var rt_constraints = try std.ArrayList(types.StaticDispatchConstraint).initCapacity(self.allocator, ct_constraints.len);
                        defer rt_constraints.deinit();

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            try rt_constraints.append(.{
                                .fn_name = ct_constraint.fn_name,
                                .fn_var = rt_fn_var,
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
                        defer rt_constraints.deinit();

                        for (ct_constraints) |ct_constraint| {
                            // Translate the constraint's fn_var recursively
                            const rt_fn_var = try self.translateTypeVar(module, ct_constraint.fn_var);
                            try rt_constraints.append(.{
                                .fn_name = ct_constraint.fn_name,
                                .fn_var = rt_fn_var,
                            });
                        }

                        const rt_constraints_range = try self.runtime_types.appendStaticDispatchConstraints(rt_constraints.items);
                        break :blk_rigid rigid.withConstraints(rt_constraints_range);
                    } else rigid;

                    const content: types.Content = .{ .rigid = rt_rigid };
                    break :blk try self.runtime_types.freshFromContent(content);
                },
                .err => {
                    return error.TypeMismatch;
                },
            }
        };

        try self.translate_cache.put(key, out_var);
        return out_var;
    }

    /// Recursively expand a tag union's tags, returning an array list
    /// Caller owns the returned memory
    fn gatherTags(
        ctx: *const Interpreter,
        module: *can.ModuleEnv,
        tag_union: types.TagUnion,
    ) std.mem.Allocator.Error!std.ArrayListUnmanaged(types.Tag) {
        var scratch_tags = try std.ArrayListUnmanaged(types.Tag).initCapacity(ctx.allocator, 8);

        const tag_slice = module.types.getTagsSlice(tag_union.tags);
        for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
            _ = try scratch_tags.append(ctx.allocator, .{ .name = name, .args = args });
        }

        var current_ext = tag_union.ext;
        while (true) {
            const resolved_ext = module.types.resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| switch (ext_flat_type) {
                    .empty_tag_union => break,
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
                    else => {
                        // TODO: Don't use unreachable here
                        unreachable;
                    },
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
            _ = try unify.unifyWithContext(
                self.env,
                self.runtime_types,
                &self.problems,
                &self.snapshots,
                &self.unify_scratch,
                &self.unify_scratch.occurs_scratch,
                params[i],
                args[i],
                false,
            );
        }
        // ret_var may now be constrained

        // Ensure layout slot for return var
        _ = try self.getRuntimeLayout(ret_var);
        const root_idx: usize = @intFromEnum(self.runtime_types.resolveVar(ret_var).var_);
        try self.ensureVarLayoutCapacity(root_idx + 1);
        const slot = self.var_to_layout_slot.items[root_idx];
        const args_copy_mut = try self.allocator.alloc(types.Var, args.len);
        errdefer self.allocator.free(args_copy_mut);
        std.mem.copyForwards(types.Var, args_copy_mut, args);

        const entry = PolyEntry{ .return_var = ret_var, .return_layout_slot = slot, .args = args_copy_mut };
        try self.polyInsert(module_id, func_id, entry);
        return entry;
    }

    /// Initial a TypeWriter from an interpreter. Useful when debugging
    fn initTypeWriter(self: *const Interpreter) std.mem.Allocator.Error!types.TypeWriter {
        return try types.TypeWriter.initFromParts(self.allocator, self.runtime_types, self.env.common.getIdentStore());
    }
};

fn add(a: i32, b: i32) i32 {
    return a + b;
}

// GREEN step: basic test to confirm the module’s tests run
test "interpreter: wiring works" {
    try std.testing.expectEqual(@as(i32, 3), add(1, 2));
}

// RED: expect Var->Layout slot to work (will fail until implemented)
test "interpreter: Var->Layout slot caches computed layout" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    // Create a concrete runtime type: Str
    const str_var = try interp.runtime_types.freshFromContent(.{ .structure = .str });

    // Initially, slot is either absent or zero; ensure capacity then check
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(str_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    try std.testing.expectEqual(@as(u32, 0), interp.var_to_layout_slot.items[root_idx]);

    // Retrieve layout and expect scalar.str; slot becomes non-zero
    const layout_value = try interp.getRuntimeLayout(str_var);
    try std.testing.expect(layout_value.tag == .scalar);
    try std.testing.expect(layout_value.data.scalar.tag == .str);
    try std.testing.expect(interp.var_to_layout_slot.items[root_idx] != 0);
}

// RED: translating a compile-time str var should produce a runtime str var
test "interpreter: translateTypeVar for str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const rt_var = try interp.translateTypeVar(&env, ct_str);

    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    try std.testing.expect(resolved.desc.content.structure == .str);
}

// RED: translating a compile-time concrete int64 should produce a runtime int64
test "interpreter: translateTypeVar for int64" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const ct_int = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_int);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time concrete f64 should produce a runtime f64
test "interpreter: translateTypeVar for f64" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const ct_frac = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f64 } } } });
    const rt_var = try interp.translateTypeVar(&env, ct_frac);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .num => |n| switch (n) {
            .num_compact => |c| switch (c) {
                .frac => |p| try std.testing.expectEqual(types.Num.Frac.Precision.f64, p),
                else => return error.TestUnexpectedResult,
            },
            else => return error.TestUnexpectedResult,
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time tuple (Str, I64) should produce a runtime tuple with same element shapes
test "interpreter: translateTypeVar for tuple(Str, I64)" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const elems = [_]types.Var{ ct_str, ct_i64 };
    const ct_tuple = try env.types.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = try env.types.appendVars(&elems) } } });

    const rt_var = try interp.translateTypeVar(&env, ct_tuple);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .tuple => |t| {
            const rt_elems = interp.runtime_types.sliceVars(t.elems);
            try std.testing.expectEqual(@as(usize, 2), rt_elems.len);
            // elem 0: str
            const e0 = interp.runtime_types.resolveVar(rt_elems[0]);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // elem 1: i64
            const e1 = interp.runtime_types.resolveVar(rt_elems[1]);
            try std.testing.expect(e1.desc.content == .structure);
            switch (e1.desc.content.structure) {
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time record { first: Str, second: I64 } should produce equivalent runtime record
test "interpreter: translateTypeVar for record {first: Str, second: I64}" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    // Build compile-time record content
    const name_first = try env.common.idents.insert(gpa, @import("base").Ident.for_text("first"));
    const name_second = try env.common.idents.insert(gpa, @import("base").Ident.for_text("second"));
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    var ct_fields = [_]types.RecordField{
        .{ .name = name_first, .var_ = ct_str },
        .{ .name = name_second, .var_ = ct_i64 },
    };
    const ct_fields_range = try env.types.appendRecordFields(&ct_fields);
    const ct_ext_empty = try env.types.freshFromContent(.{ .structure = .empty_record });
    const ct_record = try env.types.freshFromContent(.{ .structure = .{ .record = .{ .fields = ct_fields_range, .ext = ct_ext_empty } } });

    // Translate
    const rt_var = try interp.translateTypeVar(&env, ct_record);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .structure);
    switch (resolved.desc.content.structure) {
        .record => |rec| {
            const rt_fields = interp.runtime_types.getRecordFieldsSlice(rec.fields);
            try std.testing.expectEqual(@as(u32, 2), rt_fields.len);
            const f0 = rt_fields.get(0);
            const f1 = rt_fields.get(1);
            // Field names are preserved
            try std.testing.expectEqual(name_first, f0.name);
            try std.testing.expectEqual(name_second, f1.name);
            // Field 0 type is Str
            const e0 = interp.runtime_types.resolveVar(f0.var_);
            try std.testing.expect(e0.desc.content == .structure);
            try std.testing.expect(e0.desc.content.structure == .str);
            // Field 1 type is I64
            const e1 = interp.runtime_types.resolveVar(f1.var_);
            try std.testing.expect(e1.desc.content == .structure);
            switch (e1.desc.content.structure) {
                .num => |n| switch (n) {
                    .num_compact => |c| switch (c) {
                        .int => |p| try std.testing.expectEqual(types.Num.Int.Precision.i64, p),
                        else => return error.TestUnexpectedResult,
                    },
                    else => return error.TestUnexpectedResult,
                },
                else => return error.TestUnexpectedResult,
            }
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: translating a compile-time alias should produce equivalent runtime alias
test "interpreter: translateTypeVar for alias of Str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const alias_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("MyAlias"));
    const type_ident = types.TypeIdent{ .ident_idx = alias_name };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
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
    try std.testing.expect(backing_resolved.desc.content.structure == .str);
}

// RED: translating a compile-time nominal type should produce equivalent runtime nominal
test "interpreter: translateTypeVar for nominal Point(Str)" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const name_nominal = try env.common.idents.insert(gpa, @import("base").Ident.for_text("Point"));
    const type_ident = types.TypeIdent{ .ident_idx = name_nominal };
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
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
            try std.testing.expect(b_resolved.desc.content.structure == .str);
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
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
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
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const name_a = try env.common.idents.insert(gpa, @import("base").Ident.for_text("A"));
    const ct_rigid = try env.types.freshFromContent(.{ .rigid = types.Rigid.init(name_a) });
    const rt_var = try interp.translateTypeVar(&env, ct_rigid);
    const resolved = interp.runtime_types.resolveVar(rt_var);
    try std.testing.expect(resolved.desc.content == .rigid);
    try std.testing.expectEqual(name_a, resolved.desc.content.rigid.name);
}

// RED: translating a flex var with static dispatch constraints should preserve constraints
test "interpreter: translateTypeVar for flex var with static dispatch constraint" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    // Create a method function type: Str -> I64
    const ct_str = try env.types.freshFromContent(.{ .structure = .str });
    const ct_i64 = try env.types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const ct_fn_args = [_]types.Var{ct_str};
    const ct_fn_content = try env.types.mkFuncPure(&ct_fn_args, ct_i64);
    const ct_fn_var = try env.types.register(.{ .content = ct_fn_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Create a static dispatch constraint
    const method_name = try env.common.idents.insert(gpa, @import("base").Ident.for_text("len"));
    const ct_constraint = types.StaticDispatchConstraint{
        .fn_name = method_name,
        .fn_var = ct_fn_var,
    };
    const ct_constraints = [_]types.StaticDispatchConstraint{ct_constraint};
    const ct_constraints_range = try env.types.appendStaticDispatchConstraints(&ct_constraints);

    // Create a flex var with the constraint
    const ct_flex = try env.types.freshFromContent(.{ .flex = types.Flex.init().withConstraints(ct_constraints_range) });

    // Translate to runtime
    const rt_var = try interp.translateTypeVar(&env, ct_flex);
    const resolved = interp.runtime_types.resolveVar(rt_var);

    // Verify it's still a flex var
    try std.testing.expect(resolved.desc.content == .flex);
    const rt_flex = resolved.desc.content.flex;

    // Verify constraints were translated
    const rt_constraints = interp.runtime_types.sliceStaticDispatchConstraints(rt_flex.constraints);
    try std.testing.expectEqual(@as(usize, 1), rt_constraints.len);

    // Verify the method name is preserved
    try std.testing.expectEqual(method_name, rt_constraints[0].fn_name);

    // Verify the fn_var was translated (should resolve in runtime store)
    const rt_fn_resolved = interp.runtime_types.resolveVar(rt_constraints[0].fn_var);
    try std.testing.expect(rt_fn_resolved.desc.content == .structure);
    switch (rt_fn_resolved.desc.content.structure) {
        .fn_pure => |f| {
            const rt_fn_args = interp.runtime_types.sliceVars(f.args);
            try std.testing.expectEqual(@as(usize, 1), rt_fn_args.len);
            // Arg should be Str
            const rt_arg_resolved = interp.runtime_types.resolveVar(rt_fn_args[0]);
            try std.testing.expect(rt_arg_resolved.desc.content == .structure);
            try std.testing.expect(rt_arg_resolved.desc.content.structure == .str);
            // Return should be I64
            const rt_ret_resolved = interp.runtime_types.resolveVar(f.ret);
            try std.testing.expect(rt_ret_resolved.desc.content == .structure);
        },
        else => return error.TestUnexpectedResult,
    }
}

// RED: poly cache miss then hit
test "interpreter: poly cache insert and lookup" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const f_id: u32 = 12345;
    // Create runtime args: (Str, I64)
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    try std.testing.expect(interp.polyLookup(0, f_id, &args) == null);

    // For testing, say return type is Str
    const ret_var = rt_str;
    // Precompute layout slot for return type
    _ = try interp.getRuntimeLayout(ret_var);
    const root_idx: usize = @intFromEnum(interp.runtime_types.resolveVar(ret_var).var_);
    try interp.ensureVarLayoutCapacity(root_idx + 1);
    const slot = interp.var_to_layout_slot.items[root_idx];
    try std.testing.expect(slot != 0);

    const args_copy = try interp.allocator.alloc(types.Var, args.len);
    std.mem.copyForwards(types.Var, args_copy, &args);
    try interp.polyInsert(0, f_id, .{ .return_var = ret_var, .return_layout_slot = slot, .args = args_copy });
    const found = interp.polyLookup(0, f_id, &args) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(ret_var, found.return_var);
    try std.testing.expectEqual(slot, found.return_layout_slot);
    try std.testing.expect(std.mem.eql(types.Var, found.args, &args));
}

// RED: prepareCall should miss without hint, then hit after inserting with hint
test "interpreter: prepareCall miss then hit" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const func_id: u32 = 7777;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // miss without hint
    const miss = try interp.prepareCall(0, func_id, &args, null);
    try std.testing.expect(miss == null);

    // insert with hint
    const entry = (try interp.prepareCall(0, func_id, &args, rt_str)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // subsequent call should hit without hint
    const hit = (try interp.prepareCall(0, func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: prepareCallWithFuncVar populates cache based on function type
test "interpreter: prepareCallWithFuncVar populates cache" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const func_id: u32 = 9999;
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const rt_i64 = try interp.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .num_compact = .{ .int = .i64 } } } });
    const args = [_]types.Var{ rt_str, rt_i64 };

    // Build a runtime function type: (Str, I64) -> Str
    const func_content = try interp.runtime_types.mkFuncPure(&args, rt_str);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Should populate cache
    const entry = try interp.prepareCallWithFuncVar(0, func_id, func_var, &args);
    try std.testing.expectEqual(rt_str, entry.return_var);
    try std.testing.expect(entry.return_layout_slot != 0);

    // Now a plain prepareCall without hint should hit
    const hit = (try interp.prepareCall(0, func_id, &args, null)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(rt_str, hit.return_var);
    try std.testing.expectEqual(entry.return_layout_slot, hit.return_layout_slot);
}

// RED: unification constrains return type for polymorphic (a -> a), when called with Str
test "interpreter: unification constrains (a->a) with Str" {
    const gpa = std.testing.allocator;

    var env = try can.ModuleEnv.init(gpa, "");
    defer env.deinit();

    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const bool_source = "Bool := [True, False].{}\n";
    var bool_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.bool_bin, "Bool", bool_source);
    defer bool_module.deinit();
    const result_source = "Result(ok, err) := [Ok(ok), Err(err)].{}\n";
    var result_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.result_bin, "Result", result_source);
    defer result_module.deinit();

    const builtin_types_test = BuiltinTypes.init(builtin_indices, bool_module.env, result_module.env);
    var interp = try Interpreter.init(gpa, &env, builtin_types_test, null);
    defer interp.deinit();

    const func_id: u32 = 42;
    // runtime flex var 'a'
    const a = try interp.runtime_types.freshFromContent(.{ .flex = types.Flex.init() });
    const func_content = try interp.runtime_types.mkFuncPure(&.{a}, a);
    const func_var = try interp.runtime_types.register(.{ .content = func_content, .rank = types.Rank.top_level, .mark = types.Mark.none });

    // Call with Str
    const rt_str = try interp.runtime_types.freshFromContent(.{ .structure = .str });
    const entry = try interp.prepareCallWithFuncVar(0, func_id, func_var, &.{rt_str});

    // After unification, return var should resolve to str
    const resolved_ret = interp.runtime_types.resolveVar(entry.return_var);
    try std.testing.expect(resolved_ret.desc.content == .structure);
    try std.testing.expect(resolved_ret.desc.content.structure == .str);
    try std.testing.expect(entry.return_layout_slot != 0);
}
