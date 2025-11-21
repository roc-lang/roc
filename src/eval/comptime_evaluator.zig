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
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = evaluator.allocator.rawAlloc(total_size, align_enum, @returnAddress());
    const base_ptr = result orelse {
        const msg = "Out of memory during compile-time evaluation (alloc)";
        const crashed = RocCrashed{
            .utf8_bytes = @ptrCast(@constCast(msg.ptr)),
            .len = msg.len,
        };
        comptimeRocCrashed(&crashed, env);
        evaluator.halted = true;
        // Return an invalid pointer - the evaluator is already halted
        // The value doesn't matter since evaluation will stop
        return;
    };
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn comptimeRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    evaluator.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn comptimeRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
    const new_total_size = realloc_args.new_length + size_storage_bytes;
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = evaluator.allocator.realloc(old_slice, new_total_size) catch {
        const msg = "Out of memory during compile-time evaluation (realloc)";
        const crashed = RocCrashed{
            .utf8_bytes = @ptrCast(@constCast(msg.ptr)),
            .len = msg.len,
        };
        comptimeRocCrashed(&crashed, env);
        evaluator.halted = true;
        // Leave answer unchanged - the interpreter will catch this as error.Crash
        return;
    };
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn comptimeRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    var stderr_buffer: [256]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    const msg_slice = dbg_args.utf8_bytes[0..dbg_args.len];
    stderr.print("[dbg] {s}\n", .{msg_slice}) catch {};
    stderr.flush() catch {};
}

fn comptimeRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = expect_args.utf8_bytes[0..expect_args.len];
    evaluator.expect.recordCrash(msg_slice) catch {
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
    /// Flag to indicate if evaluation has been halted due to a crash
    halted: bool,
    /// Track the current expression being evaluated (for stack traces)
    current_expr_region: ?base.Region,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_envs: []const *const ModuleEnv,
        problems: *ProblemStore,
        builtin_types: BuiltinTypes,
        builtin_module_env: ?*const ModuleEnv,
    ) !ComptimeEvaluator {
        const interp = try Interpreter.init(allocator, cir, builtin_types, builtin_module_env, other_envs);

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
            .halted = false,
            .current_expr_region = null,
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

        self.interpreter.deinit();
        self.crash.deinit();
        self.expect.deinit();
    }

    fn get_ops(self: *ComptimeEvaluator) *RocOps {
        if (self.roc_ops == null) {
            self.roc_ops = RocOps{
                .env = @ptrCast(self),
                .roc_alloc = comptimeRocAlloc,
                .roc_dealloc = comptimeRocDealloc,
                .roc_realloc = comptimeRocRealloc,
                .roc_dbg = comptimeRocDbg,
                .roc_expect_failed = comptimeRocExpectFailed,
                .roc_crashed = comptimeRocCrashed,
                .host_fns = undefined, // Not used in compile-time eval
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
            else => false,
        };

        // Reset halted flag at the start of each def - crashes only halt within a single def
        self.halted = false;

        // Track the current expression region for stack traces
        self.current_expr_region = region;
        defer self.current_expr_region = null;

        const ops = self.get_ops();

        const result = self.interpreter.evalMinimal(expr_idx, ops) catch |err| {
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

    /// Try to fold a successfully evaluated constant into an e_num expression
    /// This replaces the expression in-place so future references see the constant value
    fn tryFoldConstant(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx, stack_value: eval_mod.StackValue) !void {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;

        // Don't fold if the expression is already e_num (already a constant)
        const old_expr = self.env.store.getExpr(expr_idx);
        if (old_expr == .e_num) {
            return; // Already folded, nothing to do
        }

        // Convert StackValue to CIR expression based on layout
        const layout = stack_value.layout;

        // Check if this is a scalar type (including integers)
        if (layout.tag != .scalar) {
            return error.NotImplemented; // Don't fold non-scalar types yet
        }

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
            else => return error.NotImplemented, // Don't fold other scalar types yet
        }
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

    /// Validates all deferred numeric literals by invoking their from_num_literal constraints
    ///
    /// This function is called at the beginning of compile-time evaluation, after type checking
    /// has completed. Each deferred literal contains:
    /// - expr_idx: The CIR expression index
    /// - type_var: The type variable the literal unified with (now concrete after unification)
    /// - constraint: The from_num_literal StaticDispatchConstraint with:
    ///   - fn_name: "from_num_literal" identifier
    ///   - fn_var: Type variable for the function
    ///   - num_literal: NumLiteralInfo with value, is_negative, is_fractional
    /// - region: Source location for error reporting
    ///
    /// Implementation steps (to be completed):
    /// 1. Resolve type_var to get the concrete nominal type (e.g., I64, U32, custom type)
    /// 2. Look up the from_num_literal definition for that type:
    ///    - For built-in types: find in Num module (e.g., I64.from_num_literal)
    ///    - For user types: find in the type's origin module
    /// 3. Build a NumLiteral value: [Self(is_negative: Bool)]
    ///    - This is a tag union with tag "Self" and Bool payload
    ///    - Can create synthetically or use interpreter to evaluate an e_tag expression
    /// 4. Invoke from_num_literal via interpreter:
    ///    - Create a function call expression or use evalMinimal
    ///    - Pass the NumLiteral value as argument
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
                        // Non-nominal types (e.g., records, tuples, functions) don't have from_num_literal
                        // This is a type error - numeric literal can't be used as this type
                        const error_msg = try std.fmt.allocPrint(
                            self.allocator,
                            "Numeric literal cannot be used as this type (type doesn't support from_num_literal)",
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
                    // If rigid/alias, it doesn't support from_num_literal
                    if (content != .flex) {
                        const error_msg = try std.fmt.allocPrint(
                            self.allocator,
                            "Numeric literal cannot be used as this type (type doesn't support from_num_literal)",
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

            // Step 2: Look up the from_num_literal method for this nominal type
            // Get the module where the type is defined
            const origin_module_ident = nominal_type.origin_module;
            const is_builtin = origin_module_ident == self.env.builtin_module_ident;

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

            // Build the qualified method name: ModuleName.TypeName.from_num_literal
            const type_name_bytes = self.env.getIdent(nominal_type.ident.ident_idx);
            const method_name_bytes = self.env.getIdent(literal.constraint.fn_name);

            // Extract short type name (e.g., "I64" from "Num.I64")
            const short_type_name = if (std.mem.lastIndexOf(u8, type_name_bytes, ".")) |dot_idx|
                type_name_bytes[dot_idx + 1 ..]
            else
                type_name_bytes;

            // Build qualified name: Builtin.TypeName.from_num_literal
            var qualified_name_buf: [256]u8 = undefined;
            const qualified_name = try std.fmt.bufPrint(
                &qualified_name_buf,
                "{s}.{s}.{s}",
                .{ origin_env.module_name, short_type_name, method_name_bytes },
            );

            // Look up the identifier in the origin module
            const ident_in_origin = origin_env.getIdentStoreConst().findByString(qualified_name) orelse {
                // Method not found - the type doesn't have a from_num_literal method
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "Type {s} does not have a from_num_literal method",
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
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "Type {s} does not have an accessible from_num_literal method",
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
                // No NumLiteralInfo means this isn't a from_num_literal constraint
                continue;
            };

            // Step 3: Validate the literal by invoking from_num_literal
            // All types (builtin and user-defined) use the same unified path
            const is_valid = try self.invokeFromNumLiteral(
                origin_env,
                def_idx,
                num_lit_info,
                literal.region,
            );

            if (!is_valid) {
                // Error already reported by invokeFromNumLiteral
                continue;
            }

            // Validation passed - rewrite the expression for builtin types
            if (is_builtin) {
                try self.rewriteNumericLiteralExpr(literal.expr_idx, short_type_name, num_lit_info);
            }
            // For user-defined types, keep the original expression
        }
    }

    /// Rewrite a numeric literal expression to match the inferred type
    /// Converts e_dec/e_dec_small to e_num, e_frac_f32, or e_frac_f64 based on the target type
    fn rewriteNumericLiteralExpr(
        self: *ComptimeEvaluator,
        expr_idx: CIR.Expr.Idx,
        type_name: []const u8,
        num_lit_info: types_mod.NumLiteralInfo,
    ) !void {
        const current_expr = self.env.store.getExpr(expr_idx);

        // Extract the f64 value from the current expression
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

        // Determine the target expression type based on type_name
        if (std.mem.eql(u8, type_name, "F32")) {
            // Rewrite to e_frac_f32
            const f32_value: f32 = @floatCast(f64_value);
            const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
            self.env.store.nodes.set(node_idx, .{
                .tag = .expr_frac_f32,
                .data_1 = @bitCast(f32_value),
                .data_2 = 1, // has_suffix = true to mark as explicitly typed
                .data_3 = 0,
            });
        } else if (std.mem.eql(u8, type_name, "F64")) {
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
        } else if (!num_lit_info.is_fractional) {
            // Integer type - rewrite to e_num
            const num_kind: CIR.NumKind = blk: {
                if (std.mem.eql(u8, type_name, "I8")) break :blk .i8;
                if (std.mem.eql(u8, type_name, "U8")) break :blk .u8;
                if (std.mem.eql(u8, type_name, "I16")) break :blk .i16;
                if (std.mem.eql(u8, type_name, "U16")) break :blk .u16;
                if (std.mem.eql(u8, type_name, "I32")) break :blk .i32;
                if (std.mem.eql(u8, type_name, "U32")) break :blk .u32;
                if (std.mem.eql(u8, type_name, "I64")) break :blk .i64;
                if (std.mem.eql(u8, type_name, "U64")) break :blk .u64;
                if (std.mem.eql(u8, type_name, "I128")) break :blk .i128;
                if (std.mem.eql(u8, type_name, "U128")) break :blk .u128;
                break :blk .int_unbound; // Fallback
            };

            const int_value = CIR.IntValue{
                .bytes = @bitCast(num_lit_info.value),
                .kind = .i128,
            };
            try self.env.store.replaceExprWithNum(expr_idx, int_value, num_kind);
        }
        // For Dec type, keep the original e_dec/e_dec_small expression
    }

    /// Invoke a user-defined from_num_literal function and check the result.
    /// Returns true if validation passed (Ok), false if it failed (Err).
    fn invokeFromNumLiteral(
        self: *ComptimeEvaluator,
        origin_env: *const ModuleEnv,
        def_idx: CIR.Def.Idx,
        num_lit_info: types_mod.NumLiteralInfo,
        region: base.Region,
    ) !bool {
        const roc_ops = self.get_ops();

        // Build NumLiteral record: { is_negative: Bool, digits_before_pt: List(U8), digits_after_pt: List(U8) }

        // Convert the numeric value to base-256 digits
        var base256_buf: [16]u8 = undefined;
        const abs_value: u128 = if (num_lit_info.value < 0)
            @intCast(-num_lit_info.value)
        else
            @intCast(num_lit_info.value);
        const digits_before = toBase256(abs_value, &base256_buf);

        // Build is_negative Bool
        const is_neg_value = try self.interpreter.pushRaw(layout_mod.Layout.int(.u8), 0);
        if (is_neg_value.ptr) |ptr| {
            @as(*u8, @ptrCast(@alignCast(ptr))).* = @intFromBool(num_lit_info.value < 0);
        }

        // Build digits_before_pt List(U8)
        const before_list = try self.buildU8List(digits_before, roc_ops);
        defer before_list.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Build digits_after_pt List(U8) - empty for non-fractional, compute for fractional
        const empty_digits: []const u8 = &[_]u8{};
        const after_list = try self.buildU8List(empty_digits, roc_ops);
        defer after_list.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Build the NumLiteral record
        const num_literal_record = try self.buildNumLiteralRecord(is_neg_value, before_list, after_list, roc_ops);
        defer num_literal_record.decref(&self.interpreter.runtime_layout_store, roc_ops);

        // Look up the from_num_literal function
        const target_def = origin_env.store.getDef(def_idx);

        // Save current environment
        const saved_env = self.interpreter.env;
        const saved_bindings_len = self.interpreter.bindings.items.len;
        self.interpreter.env = @constCast(origin_env);
        defer {
            self.interpreter.env = saved_env;
            self.interpreter.bindings.items.len = saved_bindings_len;
        }

        // Evaluate the from_num_literal function to get a closure
        const func_value = self.interpreter.evalMinimal(target_def.expr, roc_ops) catch |err| {
            const error_msg = try std.fmt.allocPrint(
                self.allocator,
                "Failed to evaluate from_num_literal function: {s}",
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
                "from_num_literal is not a function",
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
                "from_num_literal has wrong number of parameters (expected 1, got {d})",
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

        // Bind the NumLiteral argument
        try self.interpreter.bindings.append(.{
            .pattern_idx = params[0],
            .value = num_literal_record,
            .expr_idx = @enumFromInt(0),
        });
        defer _ = self.interpreter.bindings.pop();

        // Provide closure context
        try self.interpreter.active_closures.append(func_value);
        defer _ = self.interpreter.active_closures.pop();

        // Call the function body
        const result = self.interpreter.evalMinimal(closure_header.body_idx, roc_ops) catch |err| {
            const error_msg = try std.fmt.allocPrint(
                self.allocator,
                "from_num_literal evaluation failed: {s}",
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

        const dest = try self.interpreter.pushRaw(list_layout, 0);
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

    /// Build a NumLiteral record from its components
    fn buildNumLiteralRecord(
        self: *ComptimeEvaluator,
        is_negative: eval_mod.StackValue,
        digits_before_pt: eval_mod.StackValue,
        digits_after_pt: eval_mod.StackValue,
        roc_ops: *RocOps,
    ) !eval_mod.StackValue {
        const is_negative_ident = self.env.common.findIdent("is_negative") orelse
            return error.OutOfMemory;
        const digits_before_pt_ident = self.env.common.findIdent("digits_before_pt") orelse
            return error.OutOfMemory;
        const digits_after_pt_ident = self.env.common.findIdent("digits_after_pt") orelse
            return error.OutOfMemory;

        const field_layouts = [_]layout_mod.Layout{
            is_negative.layout,
            digits_before_pt.layout,
            digits_after_pt.layout,
        };
        const field_names = [_]base.Ident.Idx{
            is_negative_ident,
            digits_before_pt_ident,
            digits_after_pt_ident,
        };

        const record_layout_idx = try self.interpreter.runtime_layout_store.putRecord(&field_layouts, &field_names);
        const record_layout = self.interpreter.runtime_layout_store.getLayout(record_layout_idx);

        var dest = try self.interpreter.pushRaw(record_layout, 0);
        var accessor = try dest.asRecord(&self.interpreter.runtime_layout_store);

        const is_neg_idx = accessor.findFieldIndex(self.env, "is_negative") orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(is_neg_idx, is_negative, roc_ops);

        const before_pt_idx = accessor.findFieldIndex(self.env, "digits_before_pt") orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(before_pt_idx, digits_before_pt, roc_ops);

        const after_pt_idx = accessor.findFieldIndex(self.env, "digits_after_pt") orelse return error.OutOfMemory;
        try accessor.setFieldByIndex(after_pt_idx, digits_after_pt, roc_ops);

        return dest;
    }

    /// Check a Try result value - returns true if Ok, false if Err
    /// For Err case, extracts the InvalidNumLiteral(Str) message if present
    fn checkTryResult(
        self: *ComptimeEvaluator,
        result: eval_mod.StackValue,
        region: base.Region,
    ) !bool {
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
            const tag_idx = accessor.findFieldIndex(self.env, "tag") orelse return true;
            const tag_field = accessor.getFieldByIndex(tag_idx) catch return true;

            if (tag_field.layout.tag == .scalar and tag_field.layout.data.scalar.tag == .int) {
                const tag_value = tag_field.asI128();
                if (tag_value == 0) {
                    // This is an Err - try to extract InvalidNumLiteral(Str) message
                    const error_msg = try self.extractInvalidNumLiteralMessage(accessor, region);
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
        }

        return true; // Unknown format, optimistically allow
    }

    /// Extract the error message from an Err(InvalidNumLiteral(Str)) payload
    fn extractInvalidNumLiteralMessage(
        self: *ComptimeEvaluator,
        try_accessor: eval_mod.StackValue.RecordAccessor,
        region: base.Region,
    ) ![]const u8 {
        _ = region;

        // Get the payload field from the Try record
        const payload_idx = try_accessor.findFieldIndex(self.env, "payload") orelse {
            return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
        };
        const payload_field = try_accessor.getFieldByIndex(payload_idx) catch {
            return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
        };

        // The payload for Err is the error type: [InvalidNumLiteral(Str), ...]
        // This is itself a tag union which may be a record { tag, payload } or just a scalar
        if (payload_field.layout.tag == .record) {
            // Tag union with payload - look for InvalidNumLiteral tag
            var err_accessor = payload_field.asRecord(&self.interpreter.runtime_layout_store) catch {
                return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
            };

            // Check if this is a tag union with tag and payload fields
            const err_tag_idx = err_accessor.findFieldIndex(self.env, "tag") orelse {
                return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
            };
            const err_tag_field = err_accessor.getFieldByIndex(err_tag_idx) catch {
                return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
            };

            // Get tag value to verify it's InvalidNumLiteral
            // Note: We don't need to check the tag name because we're looking for ANY Str payload
            // that represents the error message (InvalidNumLiteral is the only tag with Str payload)
            _ = err_tag_field;

            // Get the inner payload which should be the Str
            const err_payload_idx = err_accessor.findFieldIndex(self.env, "payload") orelse {
                return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
            };
            const err_payload = err_accessor.getFieldByIndex(err_payload_idx) catch {
                return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
            };

            // Extract the Str from the payload
            return try self.extractStrFromValue(err_payload);
        } else if (payload_field.layout.tag == .scalar and payload_field.layout.data.scalar.tag == .str) {
            // Direct Str payload
            return try self.extractStrFromValue(payload_field);
        }

        return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
    }

    /// Extract a Str value from a StackValue
    fn extractStrFromValue(self: *ComptimeEvaluator, value: eval_mod.StackValue) ![]const u8 {
        if (value.layout.tag == .scalar and value.layout.data.scalar.tag == .str) {
            if (value.ptr) |ptr| {
                const roc_str: *const builtins.str.RocStr = @ptrCast(@alignCast(ptr));
                const str_bytes = roc_str.asSlice();
                // Copy the string to our allocator so we own it
                return try self.allocator.dupe(u8, str_bytes);
            }
        }
        return try std.fmt.allocPrint(self.allocator, "Numeric literal validation failed", .{});
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
                            const def = self.env.store.getDef(def_idx);
                            try self.interpreter.bindings.append(.{
                                .pattern_idx = def.pattern,
                                .value = value,
                                .expr_idx = def.expr,
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
