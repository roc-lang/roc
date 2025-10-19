//! Evaluates top-level declarations at compile time
//!
//! This module evaluates all top-level declarations after type checking,
//! converting any crashes into diagnostics that are reported normally.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check_mod = @import("check");
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

fn comptimeRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = evaluator.allocator.rawAlloc(total_size, align_enum, @returnAddress());
    const base_ptr = result orelse {
        const msg = "Out of memory during compile-time evaluation (alloc)";
        const crashed = RocCrashed{
            .utf8_bytes = @constCast(@ptrCast(msg.ptr)),
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

fn comptimeRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
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

fn comptimeRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
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
            .utf8_bytes = @constCast(@ptrCast(msg.ptr)),
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

fn comptimeRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = env;
    const stderr = std.io.getStdErr().writer();
    const msg_slice = dbg_args.utf8_bytes[0..dbg_args.len];
    stderr.print("[dbg] {s}\n", .{msg_slice}) catch {};
}

fn comptimeRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
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

fn comptimeRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    // Try to record the crash message, but if we can't, just continue
    // Either way, we halt evaluation
    evaluator.crash.recordCrash(msg_slice) catch {};
    evaluator.halted = true;
}

/// Result of evaluating a single declaration
const EvalResult = union(enum) {
    success,
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
    crash_messages: std.ArrayList([]const u8),
    /// Track expect failure messages we've allocated so we can free them
    expect_messages: std.ArrayList([]const u8),
    /// Track error names we've allocated so we can free them
    error_names: std.ArrayList([]const u8),
    /// Flag to indicate if evaluation has been halted due to a crash
    halted: bool,
    /// Track the current expression being evaluated (for stack traces)
    current_expr_region: ?base.Region,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_envs: []const *const ModuleEnv,
        problems: *ProblemStore,
    ) !ComptimeEvaluator {
        return ComptimeEvaluator{
            .allocator = allocator,
            .env = cir,
            .interpreter = try Interpreter.initWithOtherEnvs(allocator, cir, other_envs),
            .crash = CrashContext.init(allocator),
            .expect = CrashContext.init(allocator),
            .roc_ops = null,
            .problems = problems,
            .crash_messages = std.ArrayList([]const u8).init(allocator),
            .expect_messages = std.ArrayList([]const u8).init(allocator),
            .error_names = std.ArrayList([]const u8).init(allocator),
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
            .e_lambda, .e_closure => true,
            else => false,
        };

        // Skip e_runtime_error (compile error already reported)
        if (expr == .e_runtime_error) {
            return EvalResult.success;
        }

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
                return EvalResult.success;
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
                if (self.crash.crashMessage()) |msg| {
                    return EvalResult{
                        .crash = .{
                            .message = msg,
                            .region = region,
                        },
                    };
                }
                // If we got error.Crash but no message was recorded, still return crash with a default message
                return EvalResult{
                    .crash = .{
                        .message = "Crash occurred but no message was recorded",
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

        const layout_cache = &self.interpreter.runtime_layout_store;
        defer result.decref(layout_cache, ops);

        // TODO: Re-enable constant folding after fixing type system integration
        // Try to fold the result to a constant expression
        // self.tryFoldConstant(def_idx, result) catch {
        //     // If folding fails, just continue - the original expression is still valid
        //     // NotImplemented is expected for non-foldable types
        // };

        return EvalResult.success;
    }

    /// Attempts to replace an expression with its constant-folded result
    fn tryFoldConstant(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx, stack_value: eval_mod.StackValue) !void {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;
        const region = self.env.store.getExprRegion(expr_idx);

        // Convert StackValue to CIR expression based on layout
        const layout = stack_value.layout;

        // Check if this is a scalar type (including integers)
        if (layout.tag != .scalar) {
            return error.NotImplemented; // Don't fold non-scalar types yet
        }

        const scalar_tag = layout.data.scalar.tag;
        const new_expr_idx = switch (scalar_tag) {
            .int => blk: {
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

                // Create e_num expression
                const folded_expr = CIR.Expr{
                    .e_num = .{
                        .value = int_value,
                        .kind = num_kind,
                    },
                };

                // Add the new expression to the store
                break :blk try self.env.store.addExpr(folded_expr, region);
            },
            else => return error.NotImplemented, // Don't fold other scalar types yet
        };

        // Don't fold if the expression is already e_num (already a constant)
        const old_expr = self.env.store.getExpr(expr_idx);
        if (old_expr == .e_num) {
            return; // Already folded, nothing to do
        }

        // Replace the expr field in the Def using the safe helper function
        self.env.store.setDefExpr(def_idx, new_expr_idx);

        // We need to maintain type information for the new expression.
        // We're adding a new CIR node AFTER type checking has completed.
        // The type system expects a 1-to-1 mapping between CIR Expr indices and type variables.
        // By calling freshRedirect, we create a new type variable for the new expression
        // that redirects to the original expression's type.
        const original_type_var = ModuleEnv.varFrom(expr_idx);
        _ = try self.env.types.freshRedirect(original_type_var);
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

    /// Evaluates all top-level declarations in the module
    pub fn evalAll(self: *ComptimeEvaluator) !EvalSummary {
        var evaluated: u32 = 0;
        var crashed: u32 = 0;

        const defs = self.env.store.sliceDefs(self.env.all_defs);
        for (defs) |def_idx| {
            evaluated += 1;

            const eval_result = self.evalDecl(def_idx) catch |err| {
                // If we get an allocation error, propagate it
                return err;
            };

            switch (eval_result) {
                .success => {
                    // Declaration evaluated successfully, nothing to report
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

        return EvalSummary{
            .evaluated = evaluated,
            .crashed = crashed,
        };
    }
};
