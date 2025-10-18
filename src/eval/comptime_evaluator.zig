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
        std.debug.panic("Out of memory during comptimeRocAlloc", .{});
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
        std.debug.panic("Out of memory during comptimeRocRealloc", .{});
    };
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn comptimeRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = dbg_args;
    _ = env;
    // For compile-time evaluation, dbg is a no-op (or could be logged)
}

fn comptimeRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = expect_args;
    _ = env;
    // For compile-time evaluation, expects are a no-op
}

fn comptimeRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    evaluator.crash.recordCrash(msg_slice) catch |err| {
        std.debug.panic("failed to record crash message for comptime evaluator: {}", .{err});
    };
}

/// Result of evaluating a single declaration
const EvalResult = union(enum) {
    success,
    crash: struct {
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
    roc_ops: ?RocOps,
    problems: *ProblemStore,
    /// Track crash messages we've allocated so we can free them
    crash_messages: std.ArrayList([]const u8),

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        problems: *ProblemStore,
    ) !ComptimeEvaluator {
        return ComptimeEvaluator{
            .allocator = allocator,
            .env = cir,
            .interpreter = try Interpreter.init(allocator, cir),
            .crash = CrashContext.init(allocator),
            .roc_ops = null,
            .problems = problems,
            .crash_messages = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *ComptimeEvaluator) void {
        // Free all crash messages we allocated
        for (self.crash_messages.items) |msg| {
            self.allocator.free(msg);
        }
        self.crash_messages.deinit();

        self.interpreter.deinit();
        self.crash.deinit();
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
        return &(self.roc_ops.?);
    }

    /// Check if an expression contains references to external modules
    fn containsExternalReferences(self: *ComptimeEvaluator, expr_idx: CIR.Expr.Idx) bool {
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            // These expressions directly reference external modules
            .e_lookup_external, .e_nominal_external => return true,

            // Recursively check composite expressions
            .e_binop => |e| {
                if (self.containsExternalReferences(e.lhs)) return true;
                if (self.containsExternalReferences(e.rhs)) return true;
            },
            .e_unary_minus => |e| {
                if (self.containsExternalReferences(e.expr)) return true;
            },
            .e_unary_not => |e| {
                if (self.containsExternalReferences(e.expr)) return true;
            },
            .e_if => |e| {
                const branches = self.env.store.sliceIfBranches(e.branches);
                for (branches) |branch_idx| {
                    const branch = self.env.store.getIfBranch(branch_idx);
                    if (self.containsExternalReferences(branch.cond)) return true;
                    if (self.containsExternalReferences(branch.body)) return true;
                }
                if (self.containsExternalReferences(e.final_else)) return true;
            },
            .e_block => |e| {
                if (self.containsExternalReferences(e.final_expr)) return true;
            },
            .e_call => |e| {
                if (self.containsExternalReferences(e.func)) return true;
                const args = self.env.store.sliceExpr(e.args);
                for (args) |arg_idx| {
                    if (self.containsExternalReferences(arg_idx)) return true;
                }
            },
            .e_list => |e| {
                const elems = self.env.store.sliceExpr(e.elems);
                for (elems) |elem_idx| {
                    if (self.containsExternalReferences(elem_idx)) return true;
                }
            },
            .e_tuple => |e| {
                const elems = self.env.store.sliceExpr(e.elems);
                for (elems) |elem_idx| {
                    if (self.containsExternalReferences(elem_idx)) return true;
                }
            },
            .e_tag => |e| {
                const args = self.env.store.sliceExpr(e.args);
                for (args) |arg_idx| {
                    if (self.containsExternalReferences(arg_idx)) return true;
                }
            },
            .e_nominal => |e| {
                if (self.containsExternalReferences(e.backing_expr)) return true;
            },
            .e_record => |e| {
                if (e.ext) |ext_idx| {
                    if (self.containsExternalReferences(ext_idx)) return true;
                }
                const fields = self.env.store.sliceRecordFields(e.fields);
                for (fields) |field_idx| {
                    const field = self.env.store.getRecordField(field_idx);
                    if (self.containsExternalReferences(field.value)) return true;
                }
            },
            .e_str => |e| {
                const segments = self.env.store.sliceExpr(e.span);
                for (segments) |segment_idx| {
                    if (self.containsExternalReferences(segment_idx)) return true;
                }
            },
            .e_dbg => |e| {
                if (self.containsExternalReferences(e.expr)) return true;
            },
            .e_expect => |e| {
                if (self.containsExternalReferences(e.body)) return true;
            },
            .e_dot_access => |e| {
                if (self.containsExternalReferences(e.receiver)) return true;
                if (e.args) |args| {
                    const arg_exprs = self.env.store.sliceExpr(args);
                    for (arg_exprs) |arg_idx| {
                        if (self.containsExternalReferences(arg_idx)) return true;
                    }
                }
            },
            .e_match => |e| {
                if (self.containsExternalReferences(e.cond)) return true;
                const branches = self.env.store.matchBranchSlice(e.branches);
                for (branches) |branch_idx| {
                    const branch = self.env.store.getMatchBranch(branch_idx);
                    if (self.containsExternalReferences(branch.value)) return true;
                    if (branch.guard) |guard_idx| {
                        if (self.containsExternalReferences(guard_idx)) return true;
                    }
                }
            },
            .e_closure => |e| {
                if (self.containsExternalReferences(e.lambda_idx)) return true;
            },
            .e_lambda => |e| {
                if (self.containsExternalReferences(e.body)) return true;
            },

            // Leaf expressions that don't contain subexpressions
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_str_segment,
            .e_lookup_local,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            => {},
        }
        return false;
    }

    /// Evaluates a single declaration
    fn evalDecl(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx) !EvalResult {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;
        const region = self.env.store.getExprRegion(expr_idx);

        // Skip function definitions (lambdas/closures) - they can't be evaluated at compile time
        const expr = self.env.store.getExpr(expr_idx);
        switch (expr) {
            .e_lambda, .e_closure => return EvalResult.success,
            else => {},
        }

        // Skip expressions that reference external modules
        // Cross-module evaluation requires access to multiple ModuleEnvs which the interpreter doesn't support
        if (self.containsExternalReferences(expr_idx)) {
            return EvalResult.success;
        }

        const ops = self.get_ops();
        const result = self.interpreter.evalMinimal(expr_idx, ops) catch |err| {
            // Check if this was a crash
            if (err == error.Crash) {
                if (self.crash.crashMessage()) |msg| {
                    // Allocate a copy of the crash message that will persist
                    const owned_msg = try self.allocator.dupe(u8, msg);
                    return EvalResult{
                        .crash = .{
                            .message = owned_msg,
                            .region = region,
                        },
                    };
                }
            }
            // For all other errors (like trying to evaluate unevaluatable expressions),
            // we silently skip them - the type checker has already validated the code
            return EvalResult.success;
        };

        const layout_cache = &self.interpreter.runtime_layout_store;
        defer result.decref(layout_cache, ops);

        return EvalResult.success;
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
                    // Track the crash message so we can free it later
                    try self.crash_messages.append(crash_info.message);
                    // Create a crash problem and add it to the problem store
                    const problem = Problem{
                        .comptime_crash = .{
                            .message = crash_info.message,
                            .region = crash_info.region,
                        },
                    };
                    _ = try self.problems.appendProblem(self.allocator, problem);
                },
                .error_eval => {
                    // For non-crash errors, we could also report them, but for now
                    // we just silently continue (type checker should have caught most issues)
                },
            }
        }

        return EvalSummary{
            .evaluated = evaluated,
            .crashed = crashed,
        };
    }
};
