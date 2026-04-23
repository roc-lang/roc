//! Evaluates top-level declarations at compile time through the shared LIR path.
//!
//! This module no longer interprets checked CIR directly.
//!
//! It lowers the checked module through the normal `typed_cir -> ... -> LIR`
//! pipeline, executes proc roots with the statement-only LIR interpreter, and
//! reifies runtime bytes into explicit compile-time constants.
//!
const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const Io = @import("io").Io;
const can = @import("can");
const check_mod = @import("check");
const lir = @import("lir");
const import_mapping_mod = @import("types").import_mapping;
const eval_mod = @import("mod.zig");
const pipeline = @import("pipeline.zig");
const comptime_value = @import("comptime_value.zig");
const roc_target = @import("roc_target");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const ProblemStore = check_mod.problem.Store;

const CrashContext = eval_mod.CrashContext;
const BuiltinTypes = eval_mod.BuiltinTypes;

fn comptimeRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const allocation = evaluator.roc_arena.allocator().rawAlloc(alloc_args.length, align_enum, @returnAddress()) orelse
        @panic("Out of memory during compile-time evaluation");
    evaluator.roc_alloc_sizes.put(@intFromPtr(allocation.ptr), alloc_args.length) catch
        @panic("failed to track compile-time allocation");
    alloc_args.answer = allocation.ptr;
}

fn comptimeRocDealloc(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
    // Arena-backed. Freed wholesale when the evaluator deinitializes.
}

fn comptimeRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const arena = evaluator.roc_arena.allocator();
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(realloc_args.alignment)));
    const new_ptr = arena.rawAlloc(realloc_args.new_length, align_enum, @returnAddress()) orelse
        @panic("Out of memory during compile-time reallocation");

    const old_ptr_addr = @intFromPtr(realloc_args.answer);
    const old_size = evaluator.roc_alloc_sizes.get(old_ptr_addr) orelse 0;
    const copy_len = @min(old_size, realloc_args.new_length);
    if (copy_len > 0) {
        const old_ptr: [*]const u8 = @ptrCast(realloc_args.answer);
        @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);
    }

    _ = evaluator.roc_alloc_sizes.remove(old_ptr_addr);
    evaluator.roc_alloc_sizes.put(@intFromPtr(new_ptr.ptr), realloc_args.new_length) catch
        @panic("failed to track compile-time reallocation");
    realloc_args.answer = new_ptr.ptr;
}

fn comptimeRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = dbg_args.utf8_bytes[0..dbg_args.len];
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "[dbg] {s}\n", .{msg_slice}) catch "[dbg] (message too long)\n";
    evaluator.io.writeStderr(msg) catch {};
}

fn comptimeRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    evaluator.expect.recordCrash(source_bytes) catch @panic("failed to record expect failure");
}

fn comptimeRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const evaluator: *ComptimeEvaluator = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    evaluator.crash.recordCrash(msg_slice) catch @panic("failed to record comptime crash");
}

/// Summary of compile-time top-level evaluation.
pub const EvalSummary = struct {
    evaluated: u32,
    crashed: u32,
};

/// Drives schema-based compile-time evaluation through the LIR interpreter.
pub const ComptimeEvaluator = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    semantic_program: pipeline.SemanticEvalProgram,
    interpreter: eval_mod.Interpreter,
    values: comptime_value.Store,
    crash: CrashContext,
    expect: CrashContext,
    roc_ops: ?RocOps,
    problems: *ProblemStore,
    roc_arena: std.heap.ArenaAllocator,
    roc_alloc_sizes: std.AutoHashMap(usize, usize),
    io: Io,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_envs: []const *const ModuleEnv,
        problems: *ProblemStore,
        _: BuiltinTypes,
        _: ?*const ModuleEnv,
        _: *const import_mapping_mod.ImportMapping,
        target: roc_target.RocTarget,
        io: ?Io,
    ) !ComptimeEvaluator {
        const all_module_envs = try collectSemanticEvalModuleEnvs(allocator, cir, other_envs);
        defer allocator.free(all_module_envs);

        const source_modules = try allocator.alloc(check_mod.TypedCIR.Modules.SourceModule, all_module_envs.len);
        defer allocator.free(source_modules);
        for (all_module_envs, 0..) |env, i| {
            source_modules[i] = .{ .precompiled = @constCast(env) };
        }

        var typed_cir_modules = try check_mod.TypedCIR.Modules.init(allocator, source_modules);
        defer typed_cir_modules.deinit();

        var semantic_program = try pipeline.lowerTypedCIRToSemanticEvalProgramForTarget(
            allocator,
            &typed_cir_modules,
            all_module_envs,
            target.targetUsize(),
        );
        errdefer semantic_program.deinit();

        var evaluator = ComptimeEvaluator{
            .allocator = allocator,
            .env = cir,
            .semantic_program = semantic_program,
            .interpreter = undefined,
            .values = comptime_value.Store.init(allocator),
            .crash = CrashContext.init(allocator),
            .expect = CrashContext.init(allocator),
            .roc_ops = null,
            .problems = problems,
            .roc_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .roc_alloc_sizes = std.AutoHashMap(usize, usize).init(allocator),
            .io = io orelse Io.default(),
        };
        errdefer evaluator.values.deinit();
        errdefer evaluator.crash.deinit();
        errdefer evaluator.expect.deinit();
        errdefer evaluator.roc_arena.deinit();
        errdefer evaluator.roc_alloc_sizes.deinit();

        evaluator.interpreter = try eval_mod.Interpreter.init(
            allocator,
            &evaluator.semantic_program.lowered.lir_result.store,
            &evaluator.semantic_program.lowered.lir_result.layouts,
            evaluator.get_ops(),
        );

        const schemas = evaluator.semantic_program.schemas;
        evaluator.semantic_program.schemas = comptime_value.SchemaStore.init(allocator);
        evaluator.values.schemas = schemas;
        return evaluator;
    }

    pub fn deinit(self: *ComptimeEvaluator) void {
        self.interpreter.deinit();
        self.roc_arena.deinit();
        self.roc_alloc_sizes.deinit();
        self.values.deinit();
        self.semantic_program.deinit();
        self.crash.deinit();
        self.expect.deinit();
    }

    pub fn takeValues(self: *ComptimeEvaluator) comptime_value.Store {
        const values = self.values;
        self.values = comptime_value.Store.init(self.allocator);
        return values;
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
                .hosted_fns = undefined,
            };
        }
        self.crash.reset();
        self.expect.reset();
        return &(self.roc_ops.?);
    }

    fn reportProblem(
        self: *ComptimeEvaluator,
        message: []const u8,
        region: base.Region,
        problem_type: enum { crash, expect_failed, error_eval },
    ) !void {
        const owned_message = try self.problems.putExtraString(message);
        switch (problem_type) {
            .crash => {
                _ = try self.problems.appendProblem(self.allocator, .{
                    .comptime_crash = .{
                        .message = owned_message,
                        .region = region,
                    },
                });
            },
            .expect_failed => {
                _ = try self.problems.appendProblem(self.allocator, .{
                    .comptime_expect_failed = .{
                        .message = owned_message,
                        .region = region,
                    },
                });
            },
            .error_eval => {
                _ = try self.problems.appendProblem(self.allocator, .{
                    .comptime_eval_error = .{
                        .error_name = owned_message,
                        .region = region,
                    },
                });
            },
        }
    }

    fn rootForProcId(
        self: *const ComptimeEvaluator,
        proc_id: lir.LIR.LirProcSpecId,
    ) ?pipeline.SemanticEvalTopLevelRoot {
        for (self.semantic_program.top_level_roots) |root| {
            if (root.proc_id == proc_id) return root;
        }
        return null;
    }

    fn failedTopLevelRoot(self: *const ComptimeEvaluator) ?pipeline.SemanticEvalTopLevelRoot {
        const failed_stack = self.interpreter.getFailedCallStack();
        var i = failed_stack.len;
        while (i > 0) {
            i -= 1;
            if (self.rootForProcId(failed_stack[i])) |root| return root;
        }
        return null;
    }

    fn runnableRootCount(self: *const ComptimeEvaluator) u32 {
        var count: u32 = 0;
        for (self.semantic_program.top_level_roots) |root| {
            if (root.init_eval_index != null) count += 1;
        }
        return count;
    }

    pub fn evalAll(self: *ComptimeEvaluator) !EvalSummary {
        const init_proc_id = self.semantic_program.comptime_init_proc orelse return .{
            .evaluated = 0,
            .crashed = 0,
        };
        const init_schema_id = self.semantic_program.comptime_init_schema_id orelse
            std.debug.panic("eval.comptime_evaluator missing comptime init schema for comptime init proc", .{});
        const init_proc = self.semantic_program.lowered.lir_result.store.getProcSpec(init_proc_id);

        _ = self.get_ops();
        const eval_result = self.interpreter.eval(.{
            .proc_id = init_proc_id,
            .ret_layout = init_proc.ret_layout,
        }) catch |err| {
            const failed_root = self.failedTopLevelRoot();
            const region = if (failed_root) |root|
                self.env.store.getExprRegion(root.expr_idx)
            else
                base.Region.zero();
            const evaluated = if (failed_root) |root|
                (root.init_eval_index orelse 0) + 1
            else
                self.runnableRootCount();

            switch (err) {
                error.Crash => {
                    if (self.expect.crashMessage()) |msg| {
                        try self.reportProblem(msg, region, .expect_failed);
                    } else {
                        try self.reportProblem(
                            self.crash.crashMessage() orelse self.interpreter.getCrashMessage() orelse "compile-time crash",
                            region,
                            .crash,
                        );
                    }
                    return .{
                        .evaluated = evaluated,
                        .crashed = 1,
                    };
                },
                else => {
                    const message = switch (err) {
                        error.DivisionByZero => "Division by zero",
                        else => @errorName(err),
                    };
                    try self.reportProblem(message, region, .error_eval);
                    return .{
                        .evaluated = evaluated,
                        .crashed = 0,
                    };
                },
            }
        };

        const init_value = switch (eval_result) {
            .value => |runtime_result| runtime_result,
        };
        const aggregate_value_id = try self.values.reify(
            init_schema_id,
            init_value,
            &self.semantic_program.lowered.lir_result.layouts,
            init_proc.ret_layout,
            self.get_ops(),
        );

        for (self.semantic_program.top_level_roots) |root| {
            const tuple_index = root.init_tuple_index orelse continue;
            const binding = self.values.tupleElementBinding(
                init_schema_id,
                aggregate_value_id,
                tuple_index,
            ) orelse std.debug.panic(
                "eval.comptime_evaluator missing tuple binding for top-level root {} at tuple index {}",
                .{ @intFromEnum(root.def_idx), tuple_index },
            );
            try self.values.bind(@intFromEnum(root.pattern_idx), binding);
        }

        return .{
            .evaluated = self.runnableRootCount(),
            .crashed = 0,
        };
    }
};

fn isBuiltinModuleEnv(env: *const ModuleEnv) bool {
    if (!env.display_module_name_idx.isNone()) {
        return env.idents.builtin_module.eql(env.display_module_name_idx);
    }
    return std.mem.eql(u8, env.module_name, "Builtin");
}

fn collectSemanticEvalModuleEnvs(
    allocator: std.mem.Allocator,
    root_env: *const ModuleEnv,
    other_envs: []const *const ModuleEnv,
) ![]const *const ModuleEnv {
    var seen = std.AutoHashMap(usize, void).init(allocator);
    defer seen.deinit();

    var builtin_env: ?*const ModuleEnv = null;
    var extras = std.ArrayList(*const ModuleEnv).empty;
    defer extras.deinit(allocator);

    try seen.put(@intFromPtr(root_env), {});
    for (other_envs) |env| {
        if (seen.contains(@intFromPtr(env))) continue;
        try seen.put(@intFromPtr(env), {});
        if (isBuiltinModuleEnv(env)) {
            builtin_env = env;
        } else {
            try extras.append(allocator, env);
        }
    }

    const total_len: usize = 1 + @intFromBool(builtin_env != null) + extras.items.len;
    const all_envs = try allocator.alloc(*const ModuleEnv, total_len);
    all_envs[0] = root_env;
    var next: usize = 1;
    if (builtin_env) |builtin| {
        all_envs[next] = builtin;
        next += 1;
    }
    for (extras.items) |env| {
        all_envs[next] = env;
        next += 1;
    }
    return all_envs;
}
