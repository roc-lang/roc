//! Evaluates top-level declarations at compile time
//!
//! This module evaluates all top-level declarations after type checking,
//! converting any crashes into diagnostics that are reported normally.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check_mod = @import("check");
const types_mod = @import("types");
const import_mapping_mod = types_mod.import_mapping;
const Interpreter = @import("interpreter.zig").Interpreter;
const eval_mod = @import("mod.zig");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Problem = check_mod.problem.Problem;
const ProblemStore = check_mod.problem.Store;

const EvalError = Interpreter.Error;
const CrashContext = eval_mod.CrashContext;
const BuiltinTypes = eval_mod.BuiltinTypes;
const layout_mod = @import("layout");

fn comptimeRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));

    // Use arena allocator - all memory freed at once when evaluation completes
    const allocation = evaluator.roc_arena.allocator().rawAlloc(alloc_args.length, align_enum, @returnAddress());
    const base_ptr = allocation orelse {
        const msg = "Out of memory during compile-time evaluation (alloc)";
        const crashed = RocCrashed{
            .utf8_bytes = @ptrCast(@constCast(msg.ptr)),
            .len = msg.len,
        };
        comptimeRocCrashed(&crashed, env);
        evaluator.halted = true;
        return;
    };

    // Track allocation size for realloc
    evaluator.roc_alloc_sizes.put(@intFromPtr(base_ptr), alloc_args.length) catch {};

    alloc_args.answer = base_ptr;
}

fn comptimeRocDealloc(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
    // No-op: arena allocator frees all memory at once when evaluation completes
}

fn comptimeRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const arena = evaluator.roc_arena.allocator();
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(realloc_args.alignment)));

    // Arena doesn't support true realloc, so allocate new memory and copy
    const new_ptr = arena.rawAlloc(realloc_args.new_length, align_enum, @returnAddress()) orelse {
        const msg = "Out of memory during compile-time evaluation (realloc)";
        const crashed = RocCrashed{
            .utf8_bytes = @ptrCast(@constCast(msg.ptr)),
            .len = msg.len,
        };
        comptimeRocCrashed(&crashed, env);
        evaluator.halted = true;
        return;
    };

    // Copy old data to new location
    const old_ptr_addr = @intFromPtr(realloc_args.answer);
    const old_size = evaluator.roc_alloc_sizes.get(old_ptr_addr) orelse 0;
    const copy_len = @min(old_size, realloc_args.new_length);
    if (copy_len > 0) {
        const old_ptr: [*]const u8 = @ptrCast(realloc_args.answer);
        @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);
    }

    // Update tracking with new pointer and size
    _ = evaluator.roc_alloc_sizes.remove(old_ptr_addr);
    evaluator.roc_alloc_sizes.put(@intFromPtr(new_ptr), realloc_args.new_length) catch {};

    realloc_args.answer = new_ptr;
}

fn comptimeRocDbg(dbg_args: *const RocDbg, _: *anyopaque) callconv(.c) void {
    var stderr_buffer: [256]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    const msg_slice = dbg_args.utf8_bytes[0..dbg_args.len];
    stderr.print("[dbg] {s}\n", .{msg_slice}) catch {};
    stderr.flush() catch {};
}

fn comptimeRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    // Record the raw source bytes - the diagnostics machinery will handle formatting
    // via buildComptimeExpectFailedReport which shows the source region with ANSI colors
    evaluator.expect.recordCrash(source_bytes) catch {
        // If we can't record the expect failure, halt evaluation
        // This is the only case where expect should halt
        evaluator.halted = true;
        return;
    };
    // expect never halts execution - it only records the failure
}

fn comptimeRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    // Try to record the crash message, but if we can't, just continue
    // Either way, we halt evaluation
    evaluator.crash.recordCrash(msg_slice) catch {};
    evaluator.halted = true;
}

/// Result of evaluating a single declaration
const EvalResult = union(enum) {
    success: ?eval_mod.StackValue, // Optional value to add to bindings (null for lambdas)
    crash: struct {
        message: []const u8,
        region: base.Region,
    },
    expect_failed: struct {
        message: []const u8,
        region: base.Region,
    },
    error_eval: struct {
        err: EvalError,
        region: base.Region,
    },
};

/// Summary of compile-time evaluation
pub const EvalSummary = struct {
    evaluated: u32,
    crashed: u32,
};

/// Evaluates top-level declarations at compile time
pub const ComptimeEvaluator = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    interpreter: Interpreter,
    crash: CrashContext,
    expect: CrashContext, // Reuse CrashContext for expect failures
    roc_ops: ?RocOps,
    problems: *ProblemStore,
    /// Track crash messages we've allocated so we can free them
    crash_messages: std.array_list.Managed([]const u8),
    /// Track expect failure messages we've allocated so we can free them
    expect_messages: std.array_list.Managed([]const u8),
    /// Track error names we've allocated so we can free them
    error_names: std.array_list.Managed([]const u8),
    /// Track expressions that failed numeric literal validation (to skip evaluation)
    failed_literal_exprs: std.AutoHashMap(CIR.Expr.Idx, void),
    /// Flag to indicate if evaluation has been halted due to a crash
    halted: bool,
    /// Track the current expression being evaluated (for stack traces)
    current_expr_region: ?base.Region,
    /// Arena allocator for Roc runtime allocations - freed all at once when evaluation completes
    roc_arena: std.heap.ArenaAllocator,
    /// Track allocation sizes for realloc (maps ptr -> size)
    roc_alloc_sizes: std.AutoHashMap(usize, usize),

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_envs: []const *const ModuleEnv,
        problems: *ProblemStore,
        builtin_types: BuiltinTypes,
        builtin_module_env: ?*const ModuleEnv,
        import_mapping: *const import_mapping_mod.ImportMapping,
    ) !ComptimeEvaluator {
        const interp = try Interpreter.init(allocator, cir, builtin_types, builtin_module_env, other_envs, import_mapping, null);

        return ComptimeEvaluator{
            .allocator = allocator,
            .env = cir,
            .interpreter = interp,
            .crash = CrashContext.init(allocator),
            .expect = CrashContext.init(allocator),
            .roc_ops = null,
            .problems = problems,
            .crash_messages = std.array_list.Managed([]const u8).init(allocator),
            .expect_messages = std.array_list.Managed([]const u8).init(allocator),
            .error_names = std.array_list.Managed([]const u8).init(allocator),
            .failed_literal_exprs = std.AutoHashMap(CIR.Expr.Idx, void).init(allocator),
            .halted = false,
            .current_expr_region = null,
            .roc_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .roc_alloc_sizes = std.AutoHashMap(usize, usize).init(allocator),
        };
    }

    pub fn deinit(self: *ComptimeEvaluator) void {
        // Free all crash messages we allocated
        for (self.crash_messages.items) |msg| {
            self.allocator.free(msg);
        }
        self.crash_messages.deinit();

        // Free all expect failure messages we allocated
        for (self.expect_messages.items) |msg| {
            self.allocator.free(msg);
        }
        self.expect_messages.deinit();

        // Free all error names we allocated
        for (self.error_names.items) |name| {
            self.allocator.free(name);
        }
        self.error_names.deinit();
        self.failed_literal_exprs.deinit();

        // Free all Roc runtime allocations at once
        self.roc_arena.deinit();
        self.roc_alloc_sizes.deinit();

        self.interpreter.deinit();
        self.crash.deinit();
        self.expect.deinit();
    }

    pub fn get_ops(self: *ComptimeEvaluator) *RocOps {
        if (self.roc_ops == null) {
            self.roc_ops = RocOps{
                .env = @ptrCast(self),
                .roc_alloc = comptimeRocAlloc,
                .roc_dealloc = comptimeRocDealloc,
                .roc_realloc = comptimeRocRealloc,
                .roc_dbg = comptimeRocDbg,
                .roc_expect_failed = comptimeRocExpectFailed,
                .roc_crashed = comptimeRocCrashed,
                .hosted_fns = undefined, // Not used in compile-time eval
            };
        }
        self.crash.reset();
        self.expect.reset();
        return &(self.roc_ops.?);
    }

    /// Evaluates a single declaration
    fn evalDecl(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx) !EvalResult {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;
        const region = self.env.store.getExprRegion(expr_idx);

        const expr = self.env.store.getExpr(expr_idx);

        const is_lambda = switch (expr) {
            .e_lambda, .e_closure, .e_low_level_lambda => true,
            .e_runtime_error => return EvalResult{
                .crash = .{
                    .message = "Runtime error in expression",
                    .region = region,
                },
            },
            // Nothing to evaluate at the declaration site for these;
            // by design, they cause crashes when lookups happen on them
            .e_anno_only => return EvalResult{ .success = null },
            // Required lookups reference values from the app's `main` that provides
            // values to the platform's `requires` clause. These values are not available
            // during compile-time evaluation of the platform - they will be linked at runtime.
            .e_lookup_required => return EvalResult{ .success = null },
            else => false,
        };

        // Reset halted flag at the start of each def - crashes only halt within a single def
        self.halted = false;

        // Track the current expression region for stack traces
        self.current_expr_region = region;
        defer self.current_expr_region = null;

        const ops = self.get_ops();

        const result = self.interpreter.eval(expr_idx, ops) catch |err| {
            // If this is a lambda/closure and it failed to evaluate, just skip it
            // Top-level function definitions can fail for various reasons and that's ok
            // The interpreter will evaluate them on-demand when they're called
            // IMPORTANT: We do NOT skip blocks - blocks can have side effects like crash/expect
            if (is_lambda) {
                // Lambdas that fail to evaluate won't be added to bindings
                // They'll be re-evaluated on-demand when called
                return EvalResult{ .success = null };
            }

            if (err == error.Crash) {
                if (self.expect.crashMessage()) |msg| {
                    return EvalResult{
                        .expect_failed = .{
                            .message = msg,
                            .region = region,
                        },
                    };
                }
                const msg = self.crash.crashMessage() orelse unreachable;
                return EvalResult{
                    .crash = .{
                        .message = msg,
                        .region = region,
                    },
                };
            }
            return EvalResult{
                .error_eval = .{
                    .err = err,
                    .region = region,
                },
            };
        };

        // Try to fold the result to a constant expression (only for non-lambdas)
        if (!is_lambda) {
            self.tryFoldConstant(def_idx, result) catch {
                // If folding fails, just continue - the original expression is still valid
                // NotImplemented is expected for non-foldable types
            };
        }

        // Return the result value so it can be stored in bindings
        // Note: We don't decref here because the value needs to stay alive in bindings
        return EvalResult{ .success = result };
    }

    /// Try to fold a successfully evaluated constant into a constant expression
    /// This replaces the expression in-place so future references see the constant value
    fn tryFoldConstant(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx, stack_value: eval_mod.StackValue) !void {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;

        // Don't fold if the expression is already a constant
        const old_expr = self.env.store.getExpr(expr_idx);
        if (old_expr == .e_num or old_expr == .e_zero_argument_tag) {
            return; // Already folded, nothing to do
        }

        // Convert StackValue to CIR expression based on layout
        const layout = stack_value.layout;

        // Get the runtime type variable from the StackValue
        const rt_var = stack_value.rt_var;
        const resolved = self.interpreter.runtime_types.resolveVar(rt_var);

        // Check if it's a tag union type
        const is_tag_union = resolved.desc.content == .structure and
            resolved.desc.content.structure == .tag_union;

        // Special case for Bool type: u8 scalar with value 0 or 1
        // This handles nominal Bool types that aren't properly tracked through rt_var
        if (layout.tag == .scalar and layout.data.scalar.tag == .int and
            layout.data.scalar.data.int == .u8)
        {
            const val = stack_value.asI128();
            if (val == 0 or val == 1) {
                // This is a Bool value - fold it directly
                try self.foldBoolScalar(expr_idx, val == 1);
                return;
            }
        }

        if (is_tag_union) {
            // Tag unions can be scalars (no payload) or tuples (with payload)
            switch (layout.tag) {
                .scalar => try self.foldTagUnionScalar(def_idx, expr_idx, stack_value),
                .tuple => try self.foldTagUnionTuple(def_idx, expr_idx, stack_value),
                else => return error.NotImplemented,
            }
        } else {
            // Not a tag union - must be a scalar numeric type
            switch (layout.tag) {
                .scalar => try self.foldScalar(expr_idx, stack_value, layout),
                else => return error.NotImplemented,
            }
        }
    }

    /// Fold a scalar value (int, frac) to an e_num expression
    fn foldScalar(self: *ComptimeEvaluator, expr_idx: CIR.Expr.Idx, stack_value: eval_mod.StackValue, layout: layout_mod.Layout) !void {
        const scalar_tag = layout.data.scalar.tag;
        switch (scalar_tag) {
            .int => {
                // Extract integer value
                const value = stack_value.asI128();
                const precision = layout.data.scalar.data.int;

                // Map precision to NumKind
                const num_kind: CIR.NumKind = switch (precision) {
                    .i8 => .i8,
                    .i16 => .i16,
                    .i32 => .i32,
                    .i64 => .i64,
                    .i128 => .i128,
                    .u8 => .u8,
                    .u16 => .u16,
                    .u32 => .u32,
                    .u64 => .u64,
                    .u128 => .u128,
                };

                // Create IntValue
                const int_value = CIR.IntValue{
                    .bytes = @bitCast(value),
                    .kind = switch (precision) {
                        .u8, .u16, .u32, .u64, .u128 => .u128,
                        .i8, .i16, .i32, .i64, .i128 => .i128,
                    },
                };

                // Replace the expression with e_num in-place
                try self.env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
            },
            .frac => {
                // Handle fractional/decimal types (Dec, F32, F64)
                const frac_precision = layout.data.scalar.data.frac;

                // For Dec type, extract the i128 value and fold as Dec
                if (frac_precision == .dec) {
                    // Dec is stored as RocDec struct with .num field of type i128
                    // The value is scaled by 10^18, so we need to unscale it to get the literal value
                    const dec_value = stack_value.asDec();
                    const scaled_value = dec_value.num;

                    // Unscale by dividing by 10^18 to get the original literal value
                    const unscaled_value = @divTrunc(scaled_value, builtins.dec.RocDec.one_point_zero_i128);

                    // Create IntValue and fold as Dec
                    const int_value = CIR.IntValue{
                        .bytes = @bitCast(unscaled_value),
                        .kind = .i128,
                    };

                    try self.env.store.replaceExprWithNum(expr_idx, int_value, .dec);
                } else {
                    // For F32/F64, we don't fold yet
                    return error.NotImplemented;
                }
            },
            else => return error.NotImplemented,
        }
    }

    /// Fold a Bool value to an e_zero_argument_tag expression (True or False)
    fn foldBoolScalar(self: *ComptimeEvaluator, expr_idx: CIR.Expr.Idx, is_true: bool) !void {
        // Bool tags: 0 = False, 1 = True
        // Get the canonical Bool type variable from builtins
        const bool_rt_var = try self.interpreter.getCanonicalBoolRuntimeVar();
        const resolved = self.interpreter.runtime_types.resolveVar(bool_rt_var);

        // For Bool, we need to find the correct tag name
        const tag_name_str = if (is_true) "True" else "False";
        const tag_name_ident = try self.env.insertIdent(base.Ident.for_text(tag_name_str));

        // Get variant_var and ext_var
        const variant_var: types_mod.Var = bool_rt_var;
        // ext_var will be set if this is a tag_union type
        var ext_var: types_mod.Var = undefined;

        if (resolved.desc.content == .structure) {
            if (resolved.desc.content.structure == .tag_union) {
                ext_var = resolved.desc.content.structure.tag_union.ext;
            }
        }

        // Replace the expression with e_zero_argument_tag
        try self.env.store.replaceExprWithZeroArgumentTag(
            expr_idx,
            tag_name_ident, // closure_name
            variant_var,
            ext_var,
            tag_name_ident,
        );
    }

    /// Fold a tag union (represented as scalar, like Bool) to an e_zero_argument_tag expression
    fn foldTagUnionScalar(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx, expr_idx: CIR.Expr.Idx, stack_value: eval_mod.StackValue) !void {
        _ = def_idx; // unused now that we get rt_var from stack_value
        // The value is the tag index directly (scalar integer).
        // The caller already verified layout.tag == .scalar, and scalar tag unions are always ints.
        std.debug.assert(stack_value.layout.tag == .scalar and stack_value.layout.data.scalar.tag == .int);
        const tag_index: usize = @intCast(stack_value.asI128());

        // Get the runtime type variable from the StackValue
        const rt_var = stack_value.rt_var;

        // Get the list of tags for this union type
        var tag_list = std.array_list.AlignedManaged(types_mod.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.interpreter.appendUnionTags(rt_var, &tag_list);

        // Tag index from the value must be valid
        std.debug.assert(tag_index < tag_list.items.len);

        const tag_info = tag_list.items[tag_index];
        const arg_vars = self.interpreter.runtime_types.sliceVars(tag_info.args);

        // Scalar tag unions don't have payloads, so arg_vars must be empty
        std.debug.assert(arg_vars.len == 0);

        // Get variant_var and ext_var from type information
        const resolved = self.interpreter.runtime_types.resolveVar(rt_var);
        const variant_var: types_mod.Var = rt_var;
        // ext_var will be set if this is a tag_union type
        var ext_var: types_mod.Var = undefined;

        if (resolved.desc.content == .structure) {
            if (resolved.desc.content.structure == .tag_union) {
                ext_var = resolved.desc.content.structure.tag_union.ext;
            }
        }

        // Replace the expression with e_zero_argument_tag
        try self.env.store.replaceExprWithZeroArgumentTag(
            expr_idx,
            tag_info.name, // closure_name
            variant_var,
            ext_var,
            tag_info.name,
        );
    }

    /// Fold a tag union (represented as tuple) to an e_zero_argument_tag expression
    fn foldTagUnionTuple(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx, expr_idx: CIR.Expr.Idx, stack_value: eval_mod.StackValue) !void {
        _ = def_idx; // unused now that we get rt_var from stack_value
        // Tag unions are now represented as tuples (payload, tag)
        var acc = try stack_value.asTuple(&self.interpreter.runtime_layout_store);

        // Element 1 is the tag discriminant - getElement takes original index directly
        const tag_elem_rt_var = try self.interpreter.runtime_types.fresh();
        const tag_field = try acc.getElement(1, tag_elem_rt_var);

        // Extract tag index
        if (tag_field.layout.tag != .scalar or tag_field.layout.data.scalar.tag != .int) {
            return error.NotImplemented;
        }
        const tmp_sv = eval_mod.StackValue{ .layout = tag_field.layout, .ptr = tag_field.ptr, .is_initialized = true, .rt_var = tag_elem_rt_var };
        const tag_index: usize = @intCast(tmp_sv.asI128());

        // Get the runtime type variable from the StackValue
        const rt_var = stack_value.rt_var;

        // Get the list of tags for this union type
        var tag_list = std.array_list.AlignedManaged(types_mod.Tag, null).init(self.allocator);
        defer tag_list.deinit();
        try self.interpreter.appendUnionTags(rt_var, &tag_list);

        if (tag_index >= tag_list.items.len) {
            return error.NotImplemented;
        }

        const tag_info = tag_list.items[tag_index];
        const arg_vars = self.interpreter.runtime_types.sliceVars(tag_info.args);

        // Only fold zero-argument tags (like True, False, Ok with no payload variant, etc.)
        if (arg_vars.len != 0) {
            return error.NotImplemented; // Has payload, can't fold to e_zero_argument_tag
        }

        // Get variant_var and ext_var from type information
        const resolved = self.interpreter.runtime_types.resolveVar(rt_var);
        const variant_var: types_mod.Var = rt_var;
        // ext_var will be set if this is a tag_union type
        var ext_var: types_mod.Var = undefined;

        if (resolved.desc.content == .structure) {
            if (resolved.desc.content.structure == .tag_union) {
                ext_var = resolved.desc.content.structure.tag_union.ext;
            }
        }

        // Get closure name - use an empty ident for now (we don't need it for folded constants)
        const closure_name = tag_info.name; // Reuse tag name as closure name

        // Replace the expression with e_zero_argument_tag
        try self.env.store.replaceExprWithZeroArgumentTag(
            expr_idx,
            closure_name,
            variant_var,
            ext_var,
            tag_info.name,
        );
    }

    /// Helper to report a problem and track allocated message
    fn reportProblem(
        self: *ComptimeEvaluator,
        message: []const u8,
        region: base.Region,
        problem_type: enum { crash, expect_failed, error_eval },
    ) !void {
        // Allocate and track the message
        const owned_message = try self.allocator.dupe(u8, message);

        switch (problem_type) {
            .crash => {
                try self.crash_messages.append(owned_message);
                const problem = Problem{
                    .comptime_crash = .{
                        .message = owned_message,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
            },
            .expect_failed => {
                try self.expect_messages.append(owned_message);
                const problem = Problem{
                    .comptime_expect_failed = .{
                        .message = owned_message,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
            },
            .error_eval => {
                try self.error_names.append(owned_message);
                const problem = Problem{
                    .comptime_eval_error = .{
                        .error_name = owned_message,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
            },
        }
    }

    /// Validates all deferred numeric literals by invoking their from_numeral constraints
    ///
    /// This function is called at the beginning of compile-time evaluation, after type checking
    /// has completed. Each deferred literal contains:
    /// - expr_idx: The CIR expression index
    /// - type_var: The type variable the literal unified with (now concrete after unification)
    /// - constraint: The from_numeral StaticDispatchConstraint with:
    ///   - fn_name: "from_numeral" identifier
    ///   - fn_var: Type variable for the function
    ///   - num_literal: NumeralInfo with value, is_negative, is_fractional
    /// - region: Source location for error reporting
    ///
    /// Implementation steps (to be completed):
    /// 1. Resolve type_var to get the concrete nominal type (e.g., I64, U32, custom type)
    /// 2. Look up the from_numeral definition for that type:
    ///    - For built-in types: find in Num module (e.g., I64.from_numeral)
    ///    - For user types: find in the type's origin module
    /// 3. Build a Numeral value: [Self(is_negative: Bool)]
    ///    - This is a tag union with tag "Self" and Bool payload
    ///    - Can create synthetically or use interpreter to evaluate an e_tag expression
    /// 4. Invoke from_numeral via interpreter:
    ///    - Create a function call expression or use eval
    ///    - Pass the Numeral value as argument
    /// 5. Handle the Try result:
    ///    - Pattern match on Ok/Err tags
    ///    - For Ok: validation succeeded
    ///    - For Err: extract error message string and report via self.reportProblem()
    ///
    /// For now, validation is skipped - literals are allowed without validation.
    /// This preserves current behavior while the infrastructure is in place.
    fn validateDeferredNumericLiterals(self: *ComptimeEvaluator) !void {
        const literals = self.env.deferred_numeric_literals.items.items;

        for (literals) |literal| {
            // Step 1: Resolve the type variable to get the concrete type
            const resolved = self.env.types.resolveVar(literal.type_var);
            const content = resolved.desc.content;

            // Extract the nominal type if this is a structure
            const nominal_type = switch (content) {
                .structure => |flat_type| switch (flat_type) {
                    .nominal_type => |nom| nom,
                    else => {
                        // Non-nominal types (e.g., records, tuples, functions) don't have from_numeral
                        // This is a type error - numeric literal can't be used as this type
                        const error_msg = try std.fmt.allocPrint(
                            self.allocator,
                            "Numeric literal cannot be used as this type (type doesn't support from_numeral)",
                            .{},
                        );
                        try self.error_names.append(error_msg);
                        const problem = Problem{
                            .comptime_eval_error = .{
                                .error_name = error_msg,
                                .region = literal.region,
                            },
                        };
                        _ = try self.problems.appendProblem(self.allocator, problem);
                        continue;
                    },
                },
                else => {
                    // Non-structure types (flex, rigid, alias, etc.)
                    // If still flex, type checking didn't fully resolve it - this is OK, may resolve later
                    // If rigid/alias, it doesn't support from_numeral
                    if (content != .flex) {
                        const error_msg = try std.fmt.allocPrint(
                            self.allocator,
                            "Numeric literal cannot be used as this type (type doesn't support from_numeral)",
                            .{},
                        );
                        try self.error_names.append(error_msg);
                        const problem = Problem{
                            .comptime_eval_error = .{
                                .error_name = error_msg,
                                .region = literal.region,
                            },
                        };
                        _ = try self.problems.appendProblem(self.allocator, problem);
                    }
                    continue;
                },
            };

            // Step 2: Look up the from_numeral method for this nominal type
            // Get the module where the type is defined
            const origin_module_ident = nominal_type.origin_module;
            const is_builtin = origin_module_ident == self.env.idents.builtin_module;

            const origin_env: *const ModuleEnv = if (is_builtin) blk: {
                break :blk self.interpreter.builtin_module_env orelse {
                    // No builtin module available (shouldn't happen in normal compilation)
                    continue;
                };
            } else blk: {
                // For user-defined types, use interpreter's module lookup
                break :blk self.interpreter.module_envs.get(origin_module_ident) orelse {
                    // Module not found - might be current module
                    if (origin_module_ident == self.env.module_name_idx) {
                        break :blk self.env;
                    }
                    // Unknown module - skip for now
                    continue;
                };
            };

            // Look up the method using ident indices directly via the method_idents map
            // Pass self.env as the source since that's where the idents are from
            const ident_in_origin = origin_env.lookupMethodIdentFromEnvConst(
                self.env,
                nominal_type.ident.ident_idx,
                literal.constraint.fn_name,
            ) orelse {
                // Method not found - the type doesn't have a from_numeral method
                // Use import mapping to get the user-facing display name
                const short_type_name = import_mapping_mod.getDisplayName(
                    self.interpreter.import_mapping,
                    self.env.common.getIdentStore(),
                    nominal_type.ident.ident_idx,
                );
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "Type {s} does not have a from_numeral method",
                    .{short_type_name},
                );
                try self.error_names.append(error_msg);
                const problem = Problem{
                    .comptime_eval_error = .{
                        .error_name = error_msg,
                        .region = literal.region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
                continue;
            };

            // Get the definition index
            const node_idx_in_origin = origin_env.getExposedNodeIndexById(ident_in_origin) orelse {
                // Definition not exposed - this is also an error
                // Use import mapping to get the user-facing display name
                const short_type_name = import_mapping_mod.getDisplayName(
                    self.interpreter.import_mapping,
                    self.env.common.getIdentStore(),
                    nominal_type.ident.ident_idx,
                );
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "Type {s} does not have an accessible from_numeral method",
                    .{short_type_name},
                );
                try self.error_names.append(error_msg);
                const problem = Problem{
                    .comptime_eval_error = .{
                        .error_name = error_msg,
                        .region = literal.region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
                continue;
            };

            const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(node_idx_in_origin)));

            // Get num_lit_info for validation
            const num_lit_info = literal.constraint.num_literal orelse {
                // No NumeralInfo means this isn't a from_numeral constraint
                continue;
            };

            // Step 3: Validate the literal by invoking from_numeral
            // All types (builtin and user-defined) use the same unified path
            const is_valid = try self.invokeFromNumeral(
                origin_env,
                def_idx,
                num_lit_info,
                literal.region,
                literal.type_var,
            );

            if (!is_valid) {
                // Error already reported by invokeFromNumeral
                // Mark this expression as failed so we skip evaluating it
                try self.failed_literal_exprs.put(literal.expr_idx, {});
                continue;
            }

            // Validation passed - rewrite the expression for builtin types
            if (is_builtin) {
                try self.rewriteNumericLiteralExpr(literal.expr_idx, nominal_type.ident.ident_idx, num_lit_info);
            }
            // For user-defined types, keep the original expression
        }
    }

    /// Rewrite a numeric literal expression to match the inferred type
    /// Converts e_dec/e_dec_small to e_num, e_frac_f32, or e_frac_f64 based on the target type
    fn rewriteNumericLiteralExpr(
        self: *ComptimeEvaluator,
        expr_idx: CIR.Expr.Idx,
        type_ident: base.Ident.Idx,
        num_lit_info: types_mod.NumeralInfo,
    ) !void {
        const builtin_indices = self.interpreter.builtins.indices;

        // Use direct ident comparison to determine NumKind
        const num_kind = builtin_indices.numKindFromIdent(type_ident) orelse {
            // Unknown type - nothing to rewrite
            return;
        };

        const current_expr = self.env.store.getExpr(expr_idx);

        // Extract the f64 value from the current expression (needed for float types)
        const f64_value: f64 = switch (current_expr) {
            .e_dec => |dec| blk: {
                // Dec is stored as i128 scaled by 10^18
                const scaled = @as(f64, @floatFromInt(dec.value.num));
                break :blk scaled / 1e18;
            },
            .e_dec_small => |small| blk: {
                // Small dec has numerator and denominator_power_of_ten
                const numerator = @as(f64, @floatFromInt(small.value.numerator));
                const power: u8 = small.value.denominator_power_of_ten;
                var divisor: f64 = 1.0;
                var i: u8 = 0;
                while (i < power) : (i += 1) {
                    divisor *= 10.0;
                }
                break :blk numerator / divisor;
            },
            else => {
                // Not a dec literal - nothing to rewrite
                return;
            },
        };

        // Determine the target expression type based on num_kind
        switch (num_kind) {
            .f32 => {
                // Rewrite to e_frac_f32
                const f32_value: f32 = @floatCast(f64_value);
                const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                self.env.store.nodes.set(node_idx, .{
                    .tag = .expr_frac_f32,
                    .data_1 = @bitCast(f32_value),
                    .data_2 = 1, // has_suffix = true to mark as explicitly typed
                    .data_3 = 0,
                });
            },
            .f64 => {
                // Rewrite to e_frac_f64
                const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const f64_bits: u64 = @bitCast(f64_value);
                const low: u32 = @truncate(f64_bits);
                const high: u32 = @truncate(f64_bits >> 32);
                self.env.store.nodes.set(node_idx, .{
                    .tag = .expr_frac_f64,
                    .data_1 = low,
                    .data_2 = high,
                    .data_3 = 1, // has_suffix = true to mark as explicitly typed
                });
            },
            .dec => {
                // For Dec type, keep the original e_dec/e_dec_small expression
            },
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => {
                // Integer type - rewrite to e_num
                if (!num_lit_info.is_fractional) {
                    const int_value = CIR.IntValue{
                        .bytes = num_lit_info.bytes,
                        .kind = if (num_lit_info.is_u128) .u128 else .i128,
                    };
                    try self.env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
                }
            },
            .num_unbound, .int_unbound => {
                // Nothing to rewrite for unbound types
            },
        }
    }

    /// Invoke a user-defined from_numeral function and check the result.
    /// Returns true if validation passed (Ok), false if it failed (Err).
    fn invokeFromNumeral(
        self: *ComptimeEvaluator,
        origin_env: *const ModuleEnv,
        def_idx: CIR.Def.Idx,
        num_lit_info: types_mod.NumeralInfo,
        region: base.Region,
        target_ct_type_var: types_mod.Var, // The compile-time type variable the literal is being converted to
    ) !bool {
        const roc_ops = self.get_ops();

        // Look up the from_numeral function
        const target_def = origin_env.store.getDef(def_idx);

        // Save current environment and switch to origin_env BEFORE building the record
        // This is critical because the record's field names (ident indices) must come from
        // the same ident store that will be used when the interpreter reads them
        const saved_env = self.interpreter.env;
        const saved_bindings_len = self.interpreter.bindings.items.len;
        self.interpreter.env = @constCast(origin_env);
        defer {
            self.interpreter.env = saved_env;
            self.interpreter.bindings.items.len = saved_bindings_len;
        }

        // Build Numeral record: { is_negative: Bool, digits_before_pt: List(U8), digits_after_pt: List(U8) }
        // Must be built AFTER switching to origin_env so ident indices are from the correct store

        // Convert the numeric value to base-256 digits
        // Use @abs to safely handle minimum i128 value without overflow
        var base256_buf_before: [16]u8 = undefined;
        var base256_buf_after: [16]u8 = undefined;

        var digits_before: []const u8 = undefined;
        var digits_after: []const u8 = undefined;

        if (num_lit_info.is_fractional) {
            // For fractional literals, value is scaled by 10^18 (Dec representation)
            // Extract integer and fractional parts
            const scale: u128 = 1_000_000_000_000_000_000; // 10^18
            const abs_value: u128 = if (num_lit_info.is_u128) num_lit_info.toU128() else @abs(num_lit_info.toI128());
            const integer_part = abs_value / scale;
            const fractional_part = abs_value % scale;

            digits_before = toBase256(integer_part, &base256_buf_before);

            // Convert fractional part to base-256
            // The fractional part is already in decimal scaled form (0 to 10^18-1)
            // We need to convert it to base-256 fractional representation
            if (fractional_part > 0) {
                // Convert decimal fractional to binary fractional
                // frac = fractional_part / 10^18
                // We multiply by 256 repeatedly to get base-256 digits
                var frac_num: u128 = fractional_part;
                var frac_digits: usize = 0;
                const max_frac_digits = 8; // Enough precision for most cases
                while (frac_num > 0 and frac_digits < max_frac_digits) {
                    frac_num *= 256;
                    base256_buf_after[frac_digits] = @truncate(frac_num / scale);
                    frac_num = frac_num % scale;
                    frac_digits += 1;
                }
                digits_after = base256_buf_after[0..frac_digits];
            } else {
                digits_after = &[_]u8{};
            }
        } else {
            // Integer literal - no fractional part
            const abs_value: u128 = if (num_lit_info.is_u128) num_lit_info.toU128() else @abs(num_lit_info.toI128());
            digits_before = toBase256(abs_value, &base256_buf_before);
            digits_after = &[_]u8{};
        }

        // Build is_negative Bool
        const bool_rt_var = try self.interpreter.getCanonicalBoolRuntimeVar();
        const is_neg_value = try self.interpreter.pushRaw(layout_mod.Layout.int(.u8), 0, bool_rt_var);
        if (is_neg_value.ptr) |ptr| {
            @as(*u8, @ptrCast(@alignCast(ptr))).* = @intFromBool(num_lit_info.is_negative);
        }

        // Build digits_before_pt List(U8)
        const before_list = try self.buildU8List(digits_before, roc_ops);
        // Note: Don't decref these lists - ownership is transferred to the record below

        // Build digits_after_pt List(U8)
        const after_list = try self.buildU8List(digits_after, roc_ops);
        // Note: Don't decref these lists - ownership is transferred to the record below

        // Build the Numeral record
        // Ownership of before_list and after_list is transferred to this record
        const num_literal_record = try self.buildNumeralRecord(is_neg_value, before_list, after_list);
        defer num_literal_record.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Evaluate the from_numeral function to get a closure
        const func_value = self.interpreter.eval(target_def.expr, roc_ops) catch |err| {
            const error_msg = try std.fmt.allocPrint(
                self.allocator,
                "Failed to evaluate from_numeral function: {s}",
                .{@errorName(err)},
            );
            try self.error_names.append(error_msg);
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        };
        defer func_value.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Check if func_value is a closure
        if (func_value.layout.tag != .closure) {
            const error_msg = try std.fmt.allocPrint(
                self.allocator,
                "from_numeral is not a function",
                .{},
            );
            try self.error_names.append(error_msg);
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        }

        const closure_header: *const layout_mod.Closure = @ptrCast(@alignCast(func_value.ptr.?));

        // Get the parameters
        const params = origin_env.store.slicePatterns(closure_header.params);
        if (params.len != 1) {
            const error_msg = try std.fmt.allocPrint(
                self.allocator,
                "from_numeral has wrong number of parameters (expected 1, got {d})",
                .{params.len},
            );
            try self.error_names.append(error_msg);
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        }

        // Check if this is a low-level lambda (builtin type) or a user-defined function
        const lambda_expr = origin_env.store.getExpr(closure_header.lambda_expr_idx);

        var result: eval_mod.StackValue = undefined;
        if (lambda_expr == .e_low_level_lambda) {
            // Builtin type: dispatch directly to low-level implementation
            const low_level = lambda_expr.e_low_level_lambda;

            // Get return type for low-level builtin
            // We need to translate the type variable for the result type
            const ct_var = can.ModuleEnv.varFrom(def_idx);
            const rt_var = try self.interpreter.translateTypeVar(@constCast(origin_env), ct_var);

            // Get the return type from the function type
            const resolved = self.interpreter.runtime_types.resolveVar(rt_var);
            const return_rt_var = blk: {
                if (resolved.desc.content == .structure) {
                    const struct_content = resolved.desc.content.structure;
                    if (struct_content == .fn_pure or struct_content == .fn_effectful or struct_content == .fn_unbound) {
                        const func = switch (struct_content) {
                            .fn_pure => |f| f,
                            .fn_effectful => |f| f,
                            .fn_unbound => |f| f,
                            else => unreachable,
                        };
                        break :blk func.ret;
                    }
                }
                break :blk rt_var;
            };

            // Translate the target type variable (e.g., U8) to runtime
            // This tells the interpreter what type the literal is being converted to
            const target_rt_var = try self.interpreter.translateTypeVar(self.env, target_ct_type_var);

            // Call the low-level builtin with our Numeral argument and target type
            var args = [_]eval_mod.StackValue{num_literal_record};
            result = self.interpreter.callLowLevelBuiltinWithTargetType(low_level.op, &args, roc_ops, return_rt_var, target_rt_var) catch |err| {
                // Include crash message if available for better debugging
                const crash_msg = self.crash.crashMessage() orelse "no crash message";
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "from_numeral builtin failed: {s} ({s})",
                    .{ @errorName(err), crash_msg },
                );
                try self.error_names.append(error_msg);
                const problem = Problem{
                    .comptime_eval_error = .{
                        .error_name = error_msg,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
                return false;
            };
        } else {
            // User-defined type: bind argument and evaluate body
            try self.interpreter.bindings.append(.{
                .pattern_idx = params[0],
                .value = num_literal_record,
                .expr_idx = null, // No source expression for synthetic binding
                .source_env = origin_env,
            });
            defer _ = self.interpreter.bindings.pop();

            // Provide closure context
            try self.interpreter.active_closures.append(func_value);
            defer _ = self.interpreter.active_closures.pop();

            // Call the function body
            result = self.interpreter.eval(closure_header.body_idx, roc_ops) catch |err| {
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "from_numeral evaluation failed: {s}",
                    .{@errorName(err)},
                );
                try self.error_names.append(error_msg);
                const problem = Problem{
                    .comptime_eval_error = .{
                        .error_name = error_msg,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
                return false;
            };
        }
        defer result.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Check the Try result
        return try self.checkTryResult(result, region);
    }

    /// Convert a u128 value to base-256 representation (big-endian)
    /// Returns slice of the buffer containing the digits (without leading zeros)
    fn toBase256(value: u128, buf: *[16]u8) []const u8 {
        if (value == 0) {
            buf[0] = 0;
            return buf[0..1];
        }

        var v = value;
        var i: usize = 16;
        while (v > 0) {
            i -= 1;
            buf[i] = @intCast(v & 0xFF);
            v >>= 8;
        }
        return buf[i..16];
    }

    /// Build a List(U8) StackValue from a slice of bytes
    fn buildU8List(
        self: *ComptimeEvaluator,
        bytes: []const u8,
        roc_ops: *RocOps,
    ) !eval_mod.StackValue {
        const list_layout_idx = try self.interpreter.runtime_layout_store.insertList(layout_mod.Idx.u8);
        const list_layout = self.interpreter.runtime_layout_store.getLayout(list_layout_idx);

        // rt_var not needed for List(U8) construction - only layout matters
        const dest = try self.interpreter.pushRaw(list_layout, 0, undefined);
        if (dest.ptr == null) return dest;

        const header: *builtins.list.RocList = @ptrCast(@alignCast(dest.ptr.?));

        if (bytes.len == 0) {
            header.* = builtins.list.RocList.empty();
            return dest;
        }

        var runtime_list = builtins.list.RocList.allocateExact(
            1, // alignment for u8
            bytes.len,
            1, // element size for u8
            false, // u8 is not refcounted
            roc_ops,
        );

        if (runtime_list.elements(u8)) |elems| {
            @memcpy(elems[0..bytes.len], bytes);
        }

        header.* = runtime_list;
        return dest;
    }

    /// Build a Numeral record from its components
    /// Uses self.env for layout store operations (since layout store was initialized with user's env)
    /// but uses self.interpreter.env for field index lookups during value setting
    fn buildNumeralRecord(
        self: *ComptimeEvaluator,
        is_negative: eval_mod.StackValue,
        digits_before_pt: eval_mod.StackValue,
        digits_after_pt: eval_mod.StackValue,
    ) !eval_mod.StackValue {
        // Use precomputed idents from self.env for field names
        const field_layouts = [_]layout_mod.Layout{
            is_negative.layout,
            digits_before_pt.layout,
            digits_after_pt.layout,
        };
        const field_names = [_]base.Ident.Idx{
            self.env.idents.is_negative,
            self.env.idents.digits_before_pt,
            self.env.idents.digits_after_pt,
        };

        const record_layout_idx = try self.interpreter.runtime_layout_store.putRecord(self.env, &field_layouts, &field_names);
        const record_layout = self.interpreter.runtime_layout_store.getLayout(record_layout_idx);

        // rt_var not needed for Numeral record construction - only layout matters
        var dest = try self.interpreter.pushRaw(record_layout, 0, undefined);
        var accessor = try dest.asRecord(&self.interpreter.runtime_layout_store);

        // Use self.env for field lookups since the record was built with self.env's idents
        const is_neg_idx = accessor.findFieldIndex(self.env.idents.is_negative) orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(is_neg_idx, is_negative);

        const before_pt_idx = accessor.findFieldIndex(self.env.idents.digits_before_pt) orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(before_pt_idx, digits_before_pt);

        const after_pt_idx = accessor.findFieldIndex(self.env.idents.digits_after_pt) orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(after_pt_idx, digits_after_pt);

        return dest;
    }

    /// Check a Try result value - returns true if Ok, false if Err
    /// For Err case, extracts the InvalidNumeral(Str) message if present
    fn checkTryResult(
        self: *ComptimeEvaluator,
        result: eval_mod.StackValue,
        region: base.Region,
    ) !bool {
        // First check if the interpreter stored an error message directly
        // (happens when payload area is too small for RocStr)
        if (self.interpreter.last_error_message) |msg| {
            // Copy the message to our allocator
            const error_msg = try self.allocator.dupe(u8, msg);
            // Free the original message from the interpreter's allocator
            self.interpreter.allocator.free(msg);
            try self.error_names.append(error_msg);
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            // Clear the message for next call
            self.interpreter.last_error_message = null;
            return false;
        }

        // Try is a tag union [Ok(val), Err(err)]
        if (result.layout.tag == .scalar) {
            if (result.layout.data.scalar.tag == .int) {
                const tag_value = result.asI128();
                // "Err" < "Ok" alphabetically, so Err = 0, Ok = 1
                if (tag_value == 0) {
                    // Err with no payload - generic error
                    const error_msg = try std.fmt.allocPrint(
                        self.allocator,
                        "Numeric literal validation failed",
                        .{},
                    );
                    try self.error_names.append(error_msg);
                    const problem = Problem{
                        .comptime_eval_error = .{
                            .error_name = error_msg,
                            .region = region,
                        },
                    };
                    _ = try self.problems.appendProblem(self.allocator, problem);
                    return false;
                }
                return tag_value == 1;
            }
            return true; // Unknown format, optimistically allow
        } else if (result.layout.tag == .record) {
            var accessor = result.asRecord(&self.interpreter.runtime_layout_store) catch return true;
            // Use layout store's env for field lookups since records use that env's idents
            const layout_env = self.interpreter.runtime_layout_store.env;
            const tag_idx = accessor.findFieldIndex(layout_env.idents.tag) orelse return true;
            const tag_rt_var = self.interpreter.runtime_types.fresh() catch return true;
            const tag_field = accessor.getFieldByIndex(tag_idx, tag_rt_var) catch return true;

            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                const tag_value = tag_field.asI128();
                if (tag_value == 0) {
                    // This is an Err - try to extract InvalidNumeral(Str) message
                    const error_msg = try self.extractInvalidNumeralMessage(accessor, region);
                    try self.error_names.append(error_msg);
                    const problem = Problem{
                        .comptime_eval_error = .{
                            .error_name = error_msg,
                            .region = region,
                        },
                    };
                    _ = try self.problems.appendProblem(self.allocator, problem);
                    return false;
                }
                return true; // Ok
            }
            return true; // Unknown format, optimistically allow
        } else if (result.layout.tag == .tuple) {
            // Tuple layout (payload, tag) - newer representation for tag unions
            // For tuple layouts, the interpreter stores error messages in last_error_message
            // (which was already checked at the start of this function).
            // If we get here, we just need to check if it was an Err and return false.
            var accessor = result.asTuple(&self.interpreter.runtime_layout_store) catch return true;

            // Element 1 is tag discriminant - getElement takes original index directly
            const tag_elem_rt_var = self.interpreter.runtime_types.fresh() catch return true;
            const tag_field = accessor.getElement(1, tag_elem_rt_var) catch return true;

            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                const tag_value = tag_field.asI128();
                // Tag indices: Ok and Err are ordered alphabetically, so Err=0 and Ok=1
                // The interpreter writes ok_index for in_range, err_index for !in_range
                // Looking at appendUnionTags sorting, "Err" < "Ok" alphabetically
                if (tag_value == 0) {
                    // This is an Err - the detailed message should have been in last_error_message
                    // If we get here, something went wrong but we know it's an error
                    const error_msg = try std.fmt.allocPrint(
                        self.allocator,
                        "Numeric literal validation failed",
                        .{},
                    );
                    try self.error_names.append(error_msg);
                    const problem = Problem{
                        .comptime_eval_error = .{
                            .error_name = error_msg,
                            .region = region,
                        },
                    };
                    _ = try self.problems.appendProblem(self.allocator, problem);
                    return false;
                }
                return true; // Ok
            }
            return true; // Unknown format, optimistically allow
        } else if (result.layout.tag == .tag_union) {
            // Tag union layout: payload at offset 0, discriminant at discriminant_offset
            // For Try types from num.from_numeral, the interpreter should have stored
            // the error message in last_error_message, which was already checked above.
            // If we reach here without a last_error_message, assume it's Ok.
            return true;
        }

        return true; // Unknown format, optimistically allow
    }

    /// Extract the error message from an Err(InvalidNumeral(Str)) payload
    fn extractInvalidNumeralMessage(
        self: *ComptimeEvaluator,
        try_accessor: eval_mod.StackValue.RecordAccessor,
        _: base.Region,
    ) ![]const u8 {

        // Get the payload field from the Try record
        // Use layout store's env for field lookups
        const layout_env = self.interpreter.runtime_layout_store.env;
        const payload_idx = try_accessor.findFieldIndex(layout_env.idents.payload) orelse {
            // This should never happen - Try type must have a payload field
            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral returned malformed Try value (missing payload field)", .{});
        };
        const payload_rt_var = self.interpreter.runtime_types.fresh() catch {
            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral returned malformed Try value (could not create rt_var)", .{});
        };
        const payload_field = try_accessor.getFieldByIndex(payload_idx, payload_rt_var) catch {
            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral returned malformed Try value (could not access payload)", .{});
        };

        // The payload for Err is the error type: [InvalidNumeral(Str), ...]
        // This is itself a tag union which may be a record { tag, payload } or just a scalar
        if (payload_field.layout.tag == .record) {
            // Tag union with payload - look for InvalidNumeral tag
            var err_accessor = payload_field.asRecord(&self.interpreter.runtime_layout_store) catch {
                return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral error payload is not a valid record", .{});
            };

            // Check if this has a payload field (for the Str)
            // Single-tag unions might not have a "tag" field, so we look for payload first
            if (err_accessor.findFieldIndex(layout_env.idents.payload)) |err_payload_idx| {
                const err_payload_rt_var = self.interpreter.runtime_types.fresh() catch {
                    return try std.fmt.allocPrint(self.allocator, "Internal error: could not create rt_var for InvalidNumeral payload", .{});
                };
                const err_payload = err_accessor.getFieldByIndex(err_payload_idx, err_payload_rt_var) catch {
                    return try std.fmt.allocPrint(self.allocator, "Internal error: could not access InvalidNumeral payload", .{});
                };
                return try self.extractStrFromValue(err_payload);
            }

            // If no payload field, try to find a Str field directly (might be named differently)
            // Iterate through fields looking for a Str
            var field_idx: usize = 0;
            while (true) : (field_idx += 1) {
                const iter_field_rt_var = self.interpreter.runtime_types.fresh() catch break;
                const field = err_accessor.getFieldByIndex(field_idx, iter_field_rt_var) catch break;
                if (field.layout.tag == .scalar and field.layout.data.scalar.tag == .str) {
                    return try self.extractStrFromValue(field);
                }
            }

            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral error has no string message in InvalidNumeral", .{});
        } else if (payload_field.layout.tag == .scalar and payload_field.layout.data.scalar.tag == .str) {
            // Direct Str payload (single-tag union optimized to just the payload)
            return try self.extractStrFromValue(payload_field);
        }

        return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral returned unexpected error type (expected InvalidNumeral with Str payload)", .{});
    }

    /// Extract a Str value from a StackValue
    fn extractStrFromValue(self: *ComptimeEvaluator, value: eval_mod.StackValue) ![]const u8 {
        if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .str) {
            if (value.ptr) |ptr| {
                const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(ptr));
                const str_bytes = roc_str.asSlice();
                if (str_bytes.len > 0) {
                    // Copy the string to our allocator so we own it
                    return try self.allocator.dupe(u8, str_bytes);
                }
                return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral returned empty error message", .{});
            }
            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral error string has null pointer", .{});
        }
        if (value.layout.tag == .scalar) {
            return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral error payload is not a string (layout tag: scalar.{s})", .{@tagName(value.layout.data.scalar.tag)});
        }
        return try std.fmt.allocPrint(self.allocator, "Internal error: from_numeral error payload is not a string (layout tag: {s})", .{@tagName(value.layout.tag)});
    }

    /// Evaluates all top-level declarations in the module
    pub fn evalAll(self: *ComptimeEvaluator) !EvalSummary {
        var evaluated: u32 = 0;
        var crashed: u32 = 0;

        // Validate all deferred numeric literals first
        try self.validateDeferredNumericLiterals();

        // evaluation_order must be set after successful canonicalization
        const eval_order = self.env.evaluation_order.?;

        // Evaluate SCCs in topological order (dependencies before dependents)
        for (eval_order.sccs) |scc| {
            for (scc.defs) |def_idx| {
                // Skip declarations whose expression failed numeric literal validation
                const def = self.env.store.getDef(def_idx);
                if (self.failed_literal_exprs.contains(def.expr)) {
                    // Skip evaluation but count it as evaluated (error already reported)
                    evaluated += 1;
                    continue;
                }

                evaluated += 1;

                const eval_result = self.evalDecl(def_idx) catch |err| {
                    // If we get an allocation error, propagate it
                    return err;
                };

                switch (eval_result) {
                    .success => |maybe_value| {
                        // Declaration evaluated successfully
                        // If we got a value, add it to bindings so later defs can reference it
                        if (maybe_value) |value| {
                            const def_info = self.env.store.getDef(def_idx);
                            try self.interpreter.bindings.append(.{
                                .pattern_idx = def_info.pattern,
                                .value = value,
                                .expr_idx = def_info.expr,
                                .source_env = self.env,
                            });
                        }
                    },
                    .crash => |crash_info| {
                        crashed += 1;
                        try self.reportProblem(crash_info.message, crash_info.region, .crash);
                    },
                    .expect_failed => |expect_info| {
                        try self.reportProblem(expect_info.message, expect_info.region, .expect_failed);
                    },
                    .error_eval => |error_info| {
                        const error_name = @errorName(error_info.err);
                        try self.reportProblem(error_name, error_info.region, .error_eval);
                    },
                }
            }
        }

        return EvalSummary{
            .evaluated = evaluated,
            .crashed = crashed,
        };
    }
};
