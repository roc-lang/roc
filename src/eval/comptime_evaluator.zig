//! Evaluates top-level declarations at compile time
//!
//! This module evaluates all top-level declarations after type checking,
//! converting any crashes into diagnostics that are reported normally.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const Io = @import("io").Io;
const can = @import("can");
const check_mod = @import("check");
const types_mod = @import("types");
const import_mapping_mod = types_mod.import_mapping;
const eval_mod = @import("mod.zig");
const fold_type = @import("fold_type.zig");
const value_to_cir = @import("value_to_cir.zig");

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

const LirProgram = eval_mod.LirProgram;
const LirInterpreter = eval_mod.LirInterpreter;
const Value = eval_mod.Value;
const LayoutHelper = eval_mod.value.LayoutHelper;
const CrashContext = eval_mod.CrashContext;
const BuiltinTypes = eval_mod.BuiltinTypes;
const layout_mod = @import("layout");
const RocList = builtins.list.RocList;

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

const TypeWalkState = enum {
    visiting,
    done,
};

const TypeWalkResult = enum {
    ok,
    unresolved_rigid,
    recursive_cycle,
};

fn shouldSkipComptimeEvalForType(allocator: Allocator, types_store: *const types_mod.Store, var_: types_mod.Var) bool {
    var visited = std.AutoHashMap(types_mod.Var, TypeWalkState).init(allocator);
    defer visited.deinit();
    return containsUnresolvedRigidVarInner(types_store, var_, &visited) != .ok;
}

fn containsUnresolvedRigidVarInner(
    types_store: *const types_mod.Store,
    var_: types_mod.Var,
    visited: *std.AutoHashMap(types_mod.Var, TypeWalkState),
) TypeWalkResult {
    const gop = visited.getOrPut(var_) catch return .unresolved_rigid;
    if (gop.found_existing) {
        return switch (gop.value_ptr.*) {
            .visiting => .recursive_cycle,
            .done => .ok,
        };
    }
    gop.value_ptr.* = .visiting;

    const resolved = types_store.resolveVar(var_);
    const result = containsUnresolvedRigidContent(types_store, resolved.desc.content, visited);
    visited.put(var_, .done) catch {};
    return result;
}

fn containsUnresolvedRigidContent(
    types_store: *const types_mod.Store,
    content: types_mod.Content,
    visited: *std.AutoHashMap(types_mod.Var, TypeWalkState),
) TypeWalkResult {
    return switch (content) {
        .rigid => .unresolved_rigid,
        .flex, .err => .ok,
        .alias => |alias| blk: {
            var saw_cycle = false;
            for (types_store.sliceVars(alias.vars.nonempty)) |arg_var| {
                switch (containsUnresolvedRigidVarInner(types_store, arg_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .structure => |flat_type| containsUnresolvedRigidFlatType(types_store, flat_type, visited),
    };
}

fn containsUnresolvedRigidFlatType(
    types_store: *const types_mod.Store,
    flat_type: types_mod.FlatType,
    visited: *std.AutoHashMap(types_mod.Var, TypeWalkState),
) TypeWalkResult {
    return switch (flat_type) {
        .tuple => |tuple| blk: {
            var saw_cycle = false;
            for (types_store.sliceVars(tuple.elems)) |elem_var| {
                switch (containsUnresolvedRigidVarInner(types_store, elem_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .nominal_type => |nominal| blk: {
            var saw_cycle = false;
            for (types_store.sliceVars(nominal.vars.nonempty)) |arg_var| {
                switch (containsUnresolvedRigidVarInner(types_store, arg_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
            var saw_cycle = false;
            for (types_store.sliceVars(func.args)) |arg_var| {
                switch (containsUnresolvedRigidVarInner(types_store, arg_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            switch (containsUnresolvedRigidVarInner(types_store, func.ret, visited)) {
                .unresolved_rigid => break :blk .unresolved_rigid,
                .recursive_cycle => saw_cycle = true,
                .ok => {},
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .record => |record| blk: {
            const fields = types_store.getRecordFieldsSlice(record.fields);
            var saw_cycle = false;
            for (fields.items(.var_)) |field_var| {
                switch (containsUnresolvedRigidVarInner(types_store, field_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            switch (containsUnresolvedRigidVarInner(types_store, record.ext, visited)) {
                .unresolved_rigid => break :blk .unresolved_rigid,
                .recursive_cycle => saw_cycle = true,
                .ok => {},
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .record_unbound => |fields| blk: {
            const slice = types_store.getRecordFieldsSlice(fields);
            var saw_cycle = false;
            for (slice.items(.var_)) |field_var| {
                switch (containsUnresolvedRigidVarInner(types_store, field_var, visited)) {
                    .unresolved_rigid => break :blk .unresolved_rigid,
                    .recursive_cycle => saw_cycle = true,
                    .ok => {},
                }
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .tag_union => |tag_union| blk: {
            const tags = types_store.getTagsSlice(tag_union.tags);
            var saw_cycle = false;
            for (tags.items(.args)) |tag_args| {
                for (types_store.sliceVars(tag_args)) |arg_var| {
                    switch (containsUnresolvedRigidVarInner(types_store, arg_var, visited)) {
                        .unresolved_rigid => break :blk .unresolved_rigid,
                        .recursive_cycle => saw_cycle = true,
                        .ok => {},
                    }
                }
            }
            switch (containsUnresolvedRigidVarInner(types_store, tag_union.ext, visited)) {
                .unresolved_rigid => break :blk .unresolved_rigid,
                .recursive_cycle => saw_cycle = true,
                .ok => {},
            }
            break :blk if (saw_cycle) .recursive_cycle else .ok;
        },
        .empty_record, .empty_tag_union => .ok,
    };
}

const BuiltinIntValidation = struct {
    type_name: []const u8,
    min_value: []const u8,
    max_value: []const u8,
    positive_limit: u128,
    negative_limit: u128,
    is_unsigned: bool,
};

const try_suffix_type_error_crash_message =
    "The ? operator was used on a value that is not a Try type. The ? operator expects a value of type [Ok(a), Err(e)].";

const builtin_num_kind_map = std.StaticStringMap(CIR.NumKind).initComptime(.{
    .{ "U8", .u8 },
    .{ "I8", .i8 },
    .{ "U16", .u16 },
    .{ "I16", .i16 },
    .{ "U32", .u32 },
    .{ "I32", .i32 },
    .{ "U64", .u64 },
    .{ "I64", .i64 },
    .{ "U128", .u128 },
    .{ "I128", .i128 },
    .{ "F32", .f32 },
    .{ "F64", .f64 },
    .{ "Dec", .dec },
});

fn builtinNumKindFromDisplayName(type_name: []const u8) ?CIR.NumKind {
    return builtin_num_kind_map.get(type_name);
}

fn numeralAbsValue(num_lit_info: types_mod.NumeralInfo) u128 {
    if (num_lit_info.is_u128) return num_lit_info.toU128();

    const signed_value = num_lit_info.toI128();
    if (signed_value >= 0) return @intCast(signed_value);

    const magnitude: i128 = -%signed_value;
    return @bitCast(magnitude);
}

fn builtinIntValidationForKind(num_kind: CIR.NumKind) ?BuiltinIntValidation {
    return switch (num_kind) {
        .u8 => .{
            .type_name = "U8",
            .min_value = "0",
            .max_value = "255",
            .positive_limit = std.math.maxInt(u8),
            .negative_limit = 0,
            .is_unsigned = true,
        },
        .i8 => .{
            .type_name = "I8",
            .min_value = "-128",
            .max_value = "127",
            .positive_limit = std.math.maxInt(i8),
            .negative_limit = 128,
            .is_unsigned = false,
        },
        .u16 => .{
            .type_name = "U16",
            .min_value = "0",
            .max_value = "65535",
            .positive_limit = std.math.maxInt(u16),
            .negative_limit = 0,
            .is_unsigned = true,
        },
        .i16 => .{
            .type_name = "I16",
            .min_value = "-32768",
            .max_value = "32767",
            .positive_limit = std.math.maxInt(i16),
            .negative_limit = 32768,
            .is_unsigned = false,
        },
        .u32 => .{
            .type_name = "U32",
            .min_value = "0",
            .max_value = "4294967295",
            .positive_limit = std.math.maxInt(u32),
            .negative_limit = 0,
            .is_unsigned = true,
        },
        .i32 => .{
            .type_name = "I32",
            .min_value = "-2147483648",
            .max_value = "2147483647",
            .positive_limit = std.math.maxInt(i32),
            .negative_limit = 2147483648,
            .is_unsigned = false,
        },
        .u64 => .{
            .type_name = "U64",
            .min_value = "0",
            .max_value = "18446744073709551615",
            .positive_limit = std.math.maxInt(u64),
            .negative_limit = 0,
            .is_unsigned = true,
        },
        .i64 => .{
            .type_name = "I64",
            .min_value = "-9223372036854775808",
            .max_value = "9223372036854775807",
            .positive_limit = std.math.maxInt(i64),
            .negative_limit = 9223372036854775808,
            .is_unsigned = false,
        },
        .u128 => .{
            .type_name = "U128",
            .min_value = "0",
            .max_value = "340282366920938463463374607431768211455",
            .positive_limit = std.math.maxInt(u128),
            .negative_limit = 0,
            .is_unsigned = true,
        },
        .i128 => .{
            .type_name = "I128",
            .min_value = "-170141183460469231731687303715884105728",
            .max_value = "170141183460469231731687303715884105727",
            .positive_limit = @intCast(std.math.maxInt(i128)),
            .negative_limit = @as(u128, 1) << 127,
            .is_unsigned = false,
        },
        else => null,
    };
}

/// Result of evaluating a single declaration
const EvalResult = union(enum) {
    success: void, // Declaration evaluated successfully
    crash: struct {
        message: []const u8,
        region: base.Region,
    },
    expect_failed: struct {
        message: []const u8,
        region: base.Region,
    },
    error_eval: struct {
        message: []const u8,
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
    lir_program: LirProgram,
    all_module_envs: []const *ModuleEnv,
    crash: CrashContext,
    expect: CrashContext, // Reuse CrashContext for expect failures
    roc_ops: ?RocOps,
    problems: *ProblemStore,
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
    /// Io context for routing [dbg] output
    io: Io,
    /// Builtin module environment (for from_numeral validation)
    builtin_module_env: ?*const ModuleEnv,
    /// Other module environments (for from_numeral lookup)
    other_envs: []const *const ModuleEnv,
    /// Import mapping (for display names in diagnostics)
    import_mapping: *const import_mapping_mod.ImportMapping,
    /// Builtin types (for numKindFromIdent)
    builtin_types: BuiltinTypes,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_envs: []const *const ModuleEnv,
        problems: *ProblemStore,
        builtin_types: BuiltinTypes,
        builtin_module_env: ?*const ModuleEnv,
        import_mapping: *const import_mapping_mod.ImportMapping,
        io: ?Io,
    ) !ComptimeEvaluator {
        const target_usize: base.target.TargetUsize = if (@import("builtin").cpu.arch == .wasm32) .u32 else .u64;

        // Build all_module_envs slice including the current env
        var envs = try std.ArrayList(*ModuleEnv).initCapacity(allocator, other_envs.len + 1);
        defer envs.deinit(allocator);
        envs.appendAssumeCapacity(cir);
        for (other_envs) |other_env| {
            envs.appendAssumeCapacity(@constCast(other_env));
        }
        const all_module_envs = try allocator.dupe(*ModuleEnv, envs.items);

        return ComptimeEvaluator{
            .allocator = allocator,
            .env = cir,
            .lir_program = LirProgram.init(allocator, target_usize),
            .all_module_envs = @ptrCast(all_module_envs),
            .crash = CrashContext.init(allocator),
            .expect = CrashContext.init(allocator),
            .roc_ops = null,
            .problems = problems,
            .failed_literal_exprs = std.AutoHashMap(CIR.Expr.Idx, void).init(allocator),
            .halted = false,
            .current_expr_region = null,
            .roc_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .roc_alloc_sizes = std.AutoHashMap(usize, usize).init(allocator),
            .io = io orelse Io.default(),
            .builtin_module_env = builtin_module_env,
            .other_envs = other_envs,
            .import_mapping = import_mapping,
            .builtin_types = builtin_types,
        };
    }

    pub fn deinit(self: *ComptimeEvaluator) void {
        self.failed_literal_exprs.deinit();

        // Free all Roc runtime allocations at once
        self.roc_arena.deinit();
        self.roc_alloc_sizes.deinit();

        self.lir_program.deinit();
        // Free the all_module_envs slice we allocated
        const mutable_envs: []*ModuleEnv = @constCast(self.all_module_envs);
        self.allocator.free(mutable_envs);
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

    /// Evaluates a single declaration via LIR interpreter
    fn evalDecl(self: *ComptimeEvaluator, def_idx: CIR.Def.Idx) !EvalResult {
        const def = self.env.store.getDef(def_idx);
        const expr_idx = def.expr;
        const region = self.env.store.getExprRegion(expr_idx);

        const expr = self.env.store.getExpr(expr_idx);

        const is_lambda = switch (expr) {
            .e_lambda, .e_closure, .e_hosted_lambda => true,
            .e_runtime_error => return EvalResult{
                .crash = .{
                    .message = "Runtime error in expression",
                    .region = region,
                },
            },
            // Nothing to evaluate at the declaration site for these;
            // by design, they cause crashes when lookups happen on them
            .e_anno_only => return EvalResult{ .success = {} },
            // Required lookups reference values from the app's `main` that provides
            // values to the platform's `requires` clause. These values are not available
            // during compile-time evaluation of the platform - they will be linked at runtime.
            .e_lookup_required => return EvalResult{ .success = {} },
            else => false,
        };

        // Skip lambdas - they don't need to be evaluated at the top level
        if (is_lambda) return EvalResult{ .success = {} };

        // Skip defs whose types still contain unresolved rigid vars (e.g. platform
        // module defs that reference a `requires` clause type parameter like `model`).
        // Ordinary polymorphic constants such as numeric literals should still evaluate.
        const type_var = ModuleEnv.varFrom(expr_idx);
        if (shouldSkipComptimeEvalForType(self.allocator, &self.env.types, type_var)) {
            return EvalResult{ .success = {} };
        }

        // Reset halted flag at the start of each def - crashes only halt within a single def
        self.halted = false;

        // Track the current expression region for stack traces
        self.current_expr_region = region;
        defer self.current_expr_region = null;

        // Lower CIR → MIR → LIR → RC
        var lower_result = self.lir_program.lowerExpr(
            self.env,
            expr_idx,
            self.all_module_envs,
            null,
        ) catch {
            if (expr == .e_match and expr.e_match.is_try_suffix) {
                return EvalResult{
                    .crash = .{
                        .message = try_suffix_type_error_crash_message,
                        .region = region,
                    },
                };
            }
            return EvalResult{
                .error_eval = .{
                    .message = @errorName(error.RuntimeError),
                    .region = region,
                },
            };
        };
        defer lower_result.deinit();

        // Evaluate via LIR interpreter
        var interp = try LirInterpreter.init(self.allocator, &lower_result.lir_store, lower_result.layout_store, self.io);
        interp.detect_infinite_while_loops = true;
        defer interp.deinit();

        const eval_result = interp.eval(lower_result.final_expr_id) catch |err| {
            switch (err) {
                error.Crash => {
                    // Dupe the message: it's owned by the interpreter and freed by defer interp.deinit()
                    const msg = self.allocator.dupe(u8, interp.getCrashMessage() orelse "crash during compile-time evaluation") catch "crash during compile-time evaluation";
                    return EvalResult{
                        .crash = .{
                            .message = msg,
                            .region = region,
                        },
                    };
                },
                else => {
                    if (expr == .e_match and expr.e_match.is_try_suffix) {
                        return EvalResult{
                            .crash = .{
                                .message = try_suffix_type_error_crash_message,
                                .region = region,
                            },
                        };
                    }
                    return EvalResult{
                        .error_eval = .{
                            .message = interp.getRuntimeErrorMessage() orelse @errorName(err),
                            .region = region,
                        },
                    };
                },
            }
        };

        // Extract the value from the eval result
        const result_value = switch (eval_result) {
            .value => |v| v,
            .early_return => |v| v,
            .break_expr => return EvalResult{ .success = {} },
        };

        // Try to fold the result to a constant expression
        self.tryFoldExprFromValue(expr_idx, result_value, lower_result.result_layout, lower_result.layout_store) catch {
            // If folding fails, just continue - the original expression is still valid
        };

        if (interp.getExpectMessage()) |_| {
            return EvalResult{
                .expect_failed = .{
                    .message = "expect failed",
                    .region = region,
                },
            };
        }

        return EvalResult{ .success = {} };
    }

    /// Fold an expression to a constant value using FoldType + value_to_cir.
    fn tryFoldExprFromValue(
        self: *ComptimeEvaluator,
        expr_idx: CIR.Expr.Idx,
        value: Value,
        layout_idx: layout_mod.Idx,
        layout_store: *const layout_mod.Store,
    ) !void {
        // Don't fold if the expression is already a constant
        const old_expr = self.env.store.getExpr(expr_idx);
        if (old_expr == .e_num or old_expr == .e_zero_argument_tag) {
            return; // Already folded, nothing to do
        }

        // Build the fold type descriptor from checked types + layout
        const fold_ty = fold_type.fromExpr(
            self.allocator,
            self.env,
            expr_idx,
            layout_idx,
            layout_store,
        ) catch return;
        defer fold_ty.deinit(self.allocator);

        // Reconstruct the CIR constant expression
        _ = value_to_cir.replaceExpr(
            self.allocator,
            value,
            layout_idx,
            fold_ty,
            layout_store,
            self.env,
            expr_idx,
        ) catch return;
    }

    /// Helper to report a problem and track allocated message
    fn reportProblem(
        self: *ComptimeEvaluator,
        message: []const u8,
        region: base.Region,
        problem_type: enum { crash, expect_failed, error_eval },
    ) !void {
        // Put error str into problems store
        const owned_message = try self.problems.putExtraString(message);
        switch (problem_type) {
            .crash => {
                const problem = Problem{
                    .comptime_crash = .{
                        .message = owned_message,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
            },
            .expect_failed => {
                const problem = Problem{
                    .comptime_expect_failed = .{
                        .message = owned_message,
                        .region = region,
                    },
                };
                _ = try self.problems.appendProblem(self.allocator, problem);
            },
            .error_eval => {
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

    fn builtinNumKindFromTypeIdent(self: *const ComptimeEvaluator, type_ident: base.Ident.Idx) ?CIR.NumKind {
        if (self.builtin_types.indices.numKindFromIdent(type_ident)) |num_kind| {
            return num_kind;
        }

        const display_name = import_mapping_mod.getDisplayName(
            self.import_mapping,
            self.env.common.getIdentStore(),
            type_ident,
        );

        return builtinNumKindFromDisplayName(display_name);
    }

    fn validateBuiltinNumericLiteral(
        self: *ComptimeEvaluator,
        type_ident: base.Ident.Idx,
        num_lit_info: types_mod.NumeralInfo,
        region: base.Region,
    ) !bool {
        const num_kind = self.builtinNumKindFromTypeIdent(type_ident) orelse return true;
        if (num_kind == .f32 or num_kind == .f64 or num_kind == .dec) {
            return true;
        }

        const int_info = builtinIntValidationForKind(num_kind) orelse return true;
        const source_text = self.env.common.source[region.start.offset..region.end.offset];
        const abs_value = numeralAbsValue(num_lit_info);
        const dec_scale: u128 = 1_000_000_000_000_000_000;
        const integer_value = if (num_lit_info.is_fractional) abs_value / dec_scale else abs_value;
        const has_fractional_part = num_lit_info.is_fractional and (abs_value % dec_scale != 0);

        if (int_info.is_unsigned and num_lit_info.is_negative) {
            const message = try std.fmt.allocPrint(
                self.allocator,
                "The number {s} is not a valid {s}. {s} values cannot be negative.",
                .{ source_text, int_info.type_name, int_info.type_name },
            );
            defer self.allocator.free(message);
            try self.reportProblem(message, region, .error_eval);
            return false;
        }

        if (has_fractional_part) {
            const message = try std.fmt.allocPrint(
                self.allocator,
                "The number {s} is not a valid {s}. {s} values must be whole numbers, not fractions.",
                .{ source_text, int_info.type_name, int_info.type_name },
            );
            defer self.allocator.free(message);
            try self.reportProblem(message, region, .error_eval);
            return false;
        }

        const limit = if (num_lit_info.is_negative and !int_info.is_unsigned)
            int_info.negative_limit
        else
            int_info.positive_limit;

        if (integer_value > limit) {
            const message = try std.fmt.allocPrint(
                self.allocator,
                "The number {s} is not a valid {s}. Valid {s} values are integers between {s} and {s}.",
                .{
                    source_text,
                    int_info.type_name,
                    int_info.type_name,
                    int_info.min_value,
                    int_info.max_value,
                },
            );
            defer self.allocator.free(message);
            try self.reportProblem(message, region, .error_eval);
            return false;
        }

        return true;
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
                        const error_msg = try self.problems.putExtraString(
                            "Numeric literal cannot be used as this type (type doesn't support from_numeral)",
                        );
                        const problem = Problem{
                            .comptime_eval_error = .{
                                .error_name = error_msg,
                                .region = literal.region,
                            },
                        };
                        _ = try self.problems.appendProblem(self.allocator, problem);
                        // Mark this expression as failed so we skip evaluating it
                        try self.failed_literal_exprs.put(literal.expr_idx, {});
                        continue;
                    },
                },
                else => {
                    // Non-structure types (flex, rigid, alias, etc.)
                    // If still flex, type checking didn't fully resolve it - this is OK, may resolve later
                    // If rigid/alias, it doesn't support from_numeral
                    if (content != .flex) {
                        const error_msg = try self.problems.putExtraString(
                            "Numeric literal cannot be used as this type (type doesn't support from_numeral)",
                        );
                        const problem = Problem{
                            .comptime_eval_error = .{
                                .error_name = error_msg,
                                .region = literal.region,
                            },
                        };
                        _ = try self.problems.appendProblem(self.allocator, problem);
                        // Mark this expression as failed so we skip evaluating it
                        try self.failed_literal_exprs.put(literal.expr_idx, {});
                    }
                    continue;
                },
            };

            // Step 2: Look up the from_numeral method for this nominal type
            // Get the module where the type is defined
            const origin_module_ident = nominal_type.origin_module;
            const is_builtin = origin_module_ident.eql(self.env.idents.builtin_module);

            const num_lit_info = literal.constraint.num_literal orelse {
                // No NumeralInfo means this isn't a from_numeral constraint
                continue;
            };

            if (is_builtin) {
                const is_valid = try self.validateBuiltinNumericLiteral(
                    nominal_type.ident.ident_idx,
                    num_lit_info,
                    literal.region,
                );

                if (!is_valid) {
                    try self.failed_literal_exprs.put(literal.expr_idx, {});
                    continue;
                }

                try self.rewriteNumericLiteralExpr(literal.expr_idx, nominal_type.ident.ident_idx, num_lit_info);
                continue;
            }

            const origin_env: *const ModuleEnv = if (is_builtin) blk: {
                break :blk self.builtin_module_env orelse {
                    // No builtin module available (shouldn't happen in normal compilation)
                    continue;
                };
            } else blk: {
                // For user-defined types, search through other module envs
                break :blk self.findModuleEnvByIdent(origin_module_ident) orelse {
                    // Module not found - might be current module
                    if (origin_module_ident.eql(self.env.qualified_module_ident)) {
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
                    self.import_mapping,
                    self.env.common.getIdentStore(),
                    nominal_type.ident.ident_idx,
                );
                const error_msg = try self.problems.putFmtExtraString(
                    "Type {s} does not have a from_numeral method",
                    .{short_type_name},
                );
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
                    self.import_mapping,
                    self.env.common.getIdentStore(),
                    nominal_type.ident.ident_idx,
                );
                const error_msg = try self.problems.putFmtExtraString(
                    "Type {s} does not have an accessible from_numeral method",
                    .{short_type_name},
                );
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
        const num_kind = self.builtinNumKindFromTypeIdent(type_ident) orelse {
            // Unknown type - nothing to rewrite
            return;
        };

        const current_expr = self.env.store.getExpr(expr_idx);

        // Extract the f64 value from the current expression (needed for float types)
        const f64_value: f64 = switch (current_expr) {
            .e_dec => |dec| blk: {
                // Dec is stored as i128 scaled by 10^18
                const scaled = builtins.compiler_rt_128.i128_to_f64(dec.value.num);
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
                var node = CIR.Node.init(.expr_frac_f32);
                node.setPayload(.{ .expr_frac_f32 = .{
                    .value = @bitCast(f32_value),
                    .has_suffix = true,
                } });
                self.env.store.nodes.set(node_idx, node);
            },
            .f64 => {
                // Rewrite to e_frac_f64
                const node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
                const f64_bits: u64 = @bitCast(f64_value);
                const low: u32 = @truncate(f64_bits);
                const high: u32 = @truncate(f64_bits >> 32);
                var node = CIR.Node.init(.expr_frac_f64);
                node.setPayload(.{ .expr_frac_f64 = .{
                    .value_lo = low,
                    .value_hi = high,
                    .has_suffix = true,
                } });
                self.env.store.nodes.set(node_idx, node);
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

    /// Invoke a from_numeral function via LIR and check the result.
    /// Returns true if validation passed (Ok), false if it failed (Err).
    fn invokeFromNumeral(
        self: *ComptimeEvaluator,
        origin_env: *const ModuleEnv,
        def_idx: CIR.Def.Idx,
        num_lit_info: types_mod.NumeralInfo,
        region: base.Region,
        _: types_mod.Var, // target_ct_type_var — not needed for LIR path
    ) !bool {
        const target_def = origin_env.store.getDef(def_idx);

        // Lower the from_numeral function to LIR
        var lower_result = self.lir_program.lowerExpr(
            @constCast(origin_env),
            target_def.expr,
            self.all_module_envs,
            null,
        ) catch {
            const error_msg = try self.problems.putExtraString("Failed to lower from_numeral function");
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        };
        defer lower_result.deinit();

        // For a function, the result_layout is the function's layout, not the arg layout.
        // We need to get the arg layout from the function type.
        const expr_type_var = ModuleEnv.varFrom(target_def.expr);
        const resolved_type = origin_env.types.resolveVar(expr_type_var);
        const maybe_func = resolved_type.desc.content.unwrapFunc();
        if (maybe_func == null) {
            const error_msg = try self.problems.putExtraString("from_numeral is not a function");
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        }
        const func = maybe_func.?;
        const arg_vars = origin_env.types.sliceVars(func.args);
        if (arg_vars.len != 1) {
            const error_msg = try self.problems.putFmtExtraString(
                "from_numeral has wrong number of parameters (expected 1, got {d})",
                .{arg_vars.len},
            );
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        }

        // Get module index for layout resolution
        const module_idx: u32 = for (self.all_module_envs, 0..) |env, i| {
            if (env == origin_env) break @intCast(i);
        } else {
            return false;
        };

        var type_scope = types_mod.TypeScope.init(self.allocator);
        defer type_scope.deinit();

        const param_layout_idx = lower_result.layout_store.fromTypeVar(module_idx, arg_vars[0], &type_scope, null) catch {
            return false;
        };
        const ret_layout_idx = lower_result.layout_store.fromTypeVar(module_idx, func.ret, &type_scope, null) catch {
            return false;
        };

        const param_size = lower_result.layout_store.layoutSize(lower_result.layout_store.getLayout(param_layout_idx));
        const ret_size = lower_result.layout_store.layoutSize(lower_result.layout_store.getLayout(ret_layout_idx));

        // Allocate buffers for argument and result
        const arena_alloc = self.roc_arena.allocator();
        const arg_buf = arena_alloc.alloc(u8, param_size) catch return false;
        @memset(arg_buf, 0);
        const ret_buf = arena_alloc.alloc(u8, if (ret_size > 0) ret_size else 1) catch return false;
        @memset(ret_buf, 0);

        // Build the Numeral record in arg_buf
        self.writeNumeralToBuffer(arg_buf, param_layout_idx, lower_result.layout_store, num_lit_info) catch {
            return false;
        };

        // Evaluate via LIR interpreter
        var interp = try LirInterpreter.init(self.allocator, &lower_result.lir_store, lower_result.layout_store, self.io);
        interp.detect_infinite_while_loops = true;
        defer interp.deinit();

        const arg_layouts = [_]layout_mod.Idx{param_layout_idx};
        interp.evalEntrypoint(
            lower_result.final_expr_id,
            &arg_layouts,
            ret_layout_idx,
            self.get_ops(),
            @ptrCast(arg_buf.ptr),
            @ptrCast(ret_buf.ptr),
        ) catch |err| {
            const crash_msg = interp.getCrashMessage() orelse @errorName(err);
            const error_msg = try self.problems.putFmtExtraString(
                "from_numeral evaluation failed: {s}",
                .{crash_msg},
            );
            const problem = Problem{
                .comptime_eval_error = .{
                    .error_name = error_msg,
                    .region = region,
                },
            };
            _ = try self.problems.appendProblem(self.allocator, problem);
            return false;
        };

        // Check the Try result
        const result_value = Value{ .ptr = ret_buf.ptr };
        return try self.checkTryResult(result_value, ret_layout_idx, lower_result.layout_store, region);
    }

    /// Write a Numeral record into a pre-allocated buffer using layout offsets.
    ///
    /// Numeral record: { digits_after_pt: List(U8), digits_before_pt: List(U8), is_negative: Bool }
    /// Fields are in alphabetical order: digits_after_pt(0), digits_before_pt(1), is_negative(2)
    fn writeNumeralToBuffer(
        self: *ComptimeEvaluator,
        buf: []u8,
        numeral_layout_idx: layout_mod.Idx,
        layout_store: *const layout_mod.Store,
        num_lit_info: types_mod.NumeralInfo,
    ) !void {
        const layout = layout_store.getLayout(numeral_layout_idx);
        if (layout.tag != .struct_) return error.OutOfMemory;
        const struct_idx = layout.data.struct_.idx;

        // Alphabetical field order: digits_after_pt(0), digits_before_pt(1), is_negative(2)
        const after_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
        const before_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 1);
        const neg_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, 2);

        // Convert the numeric value to base-256 digits
        var base256_buf_before: [16]u8 = undefined;
        var base256_buf_after: [16]u8 = undefined;
        var digits_before: []const u8 = undefined;
        var digits_after: []const u8 = undefined;

        if (num_lit_info.is_fractional) {
            const scale: u128 = 1_000_000_000_000_000_000; // 10^18
            const abs_value: u128 = if (num_lit_info.is_u128) num_lit_info.toU128() else @abs(num_lit_info.toI128());
            const integer_part = abs_value / scale;
            const fractional_part = abs_value % scale;

            digits_before = toBase256(integer_part, &base256_buf_before);

            if (fractional_part > 0) {
                var frac_num: u128 = fractional_part;
                var frac_digits: usize = 0;
                const max_frac_digits = 8;
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
            const abs_value: u128 = if (num_lit_info.is_u128) num_lit_info.toU128() else @abs(num_lit_info.toI128());
            digits_before = toBase256(abs_value, &base256_buf_before);
            digits_after = &[_]u8{};
        }

        // Write is_negative
        buf[neg_offset] = @intFromBool(num_lit_info.is_negative);

        // Build and write digits_before_pt List(U8)
        const roc_ops = self.get_ops();
        const before_list = self.buildRocU8List(digits_before, roc_ops);
        @memcpy(buf[before_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&before_list));

        // Build and write digits_after_pt List(U8)
        const after_list = self.buildRocU8List(digits_after, roc_ops);
        @memcpy(buf[after_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&after_list));
    }

    /// Build a RocList(U8) from a slice of bytes using the comptime evaluator's arena.
    fn buildRocU8List(_: *ComptimeEvaluator, bytes: []const u8, roc_ops: *RocOps) RocList {
        if (bytes.len == 0) return RocList.empty();

        var list = RocList.allocateExact(
            1, // alignment for u8
            bytes.len,
            1, // element size for u8
            false, // u8 is not refcounted
            roc_ops,
        );

        if (list.elements(u8)) |elems| {
            @memcpy(elems[0..bytes.len], bytes);
        }

        return list;
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

    /// Check a Try result value using layout-based reading.
    /// Returns true if Ok (discriminant 1), false if Err (discriminant 0).
    /// "Err" < "Ok" alphabetically, so Err = 0, Ok = 1.
    fn checkTryResult(
        self: *ComptimeEvaluator,
        result_value: Value,
        result_layout_idx: layout_mod.Idx,
        layout_store: *const layout_mod.Store,
        region: base.Region,
    ) !bool {
        const result_layout = layout_store.getLayout(result_layout_idx);

        switch (result_layout.tag) {
            .scalar => {
                // Scalar tag union: value IS the discriminant
                // For Try with no payloads, Err = 0, Ok = 1
                const disc = result_value.read(u8);
                if (disc == 0) {
                    const error_msg = try self.problems.putExtraString("Numeric literal validation failed");
                    const problem = Problem{
                        .comptime_eval_error = .{
                            .error_name = error_msg,
                            .region = region,
                        },
                    };
                    _ = try self.problems.appendProblem(self.allocator, problem);
                    return false;
                }
                return disc == 1;
            },
            .tag_union => {
                // Full tag union layout: read discriminant via helper
                const helper = LayoutHelper.init(layout_store);
                const disc = helper.readTagDiscriminant(result_value, result_layout_idx);
                if (disc == 0) {
                    // Err case - try to extract error message from payload
                    const tu_data = layout_store.getTagUnionData(result_layout.data.tag_union.idx);
                    const variants = layout_store.getTagUnionVariants(tu_data);
                    const err_variant = variants.get(0); // Err is at discriminant 0
                    const err_payload_layout = layout_store.getLayout(err_variant.payload_layout);
                    const err_msg = tryExtractErrorMessage(err_payload_layout);
                    const error_msg = try self.problems.putExtraString(err_msg);
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
            },
            .struct_ => {
                // Struct-represented tag union: discriminant is the last field
                const struct_idx = result_layout.data.struct_.idx;
                const sd = layout_store.getStructData(struct_idx);
                const num_fields = sd.fields.count;
                const disc_field_idx: u32 = @intCast(num_fields - 1);
                const disc_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, disc_field_idx);
                const disc_value = result_value.offset(disc_offset);
                const disc = disc_value.read(u8);
                if (disc == 0) {
                    const error_msg = try self.problems.putExtraString("Numeric literal validation failed");
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
            },
            else => return true, // Unknown format, optimistically allow
        }
    }

    /// Try to extract a string error message from an Err payload.
    /// Returns a human-readable error message.
    fn tryExtractErrorMessage(payload_layout: layout_mod.Layout) []const u8 {
        // The Err payload is [InvalidNumeral Str, ...]
        // For now, return a generic message. Full string extraction from RocStr
        // would require reading the RocStr struct and its bytes.
        if (payload_layout.tag == .scalar) {
            return "Numeric literal validation failed";
        }
        return "Numeric literal validation failed";
    }

    /// Evaluates all top-level declarations in the module
    /// Evaluates all top-level declarations in the module
    pub fn evalAll(self: *ComptimeEvaluator) !EvalSummary {
        var evaluated: u32 = 0;
        var crashed: u32 = 0;
        var had_error_eval = false;

        // Validate all deferred numeric literals first
        try self.validateDeferredNumericLiterals();

        // evaluation_order must be set after successful canonicalization
        const eval_order = self.env.evaluation_order.?;

        // Collect deferred error_eval problems — these might be resolved by
        // the batch retry (cross-def closure evaluation). Only report them
        // if the batch retry doesn't fix the underlying expression.
        const DeferredError = struct { expr_idx: CIR.Expr.Idx, message: []const u8, region: base.Region };
        var deferred_errors: std.ArrayList(DeferredError) = .empty;
        defer deferred_errors.deinit(self.allocator);

        // Phase 1: Per-def evaluation (preserves per-def error/expect reporting)
        for (eval_order.sccs) |scc| {
            for (scc.defs) |def_idx| {
                const def = self.env.store.getDef(def_idx);
                if (self.failed_literal_exprs.contains(def.expr)) {
                    evaluated += 1;
                    continue;
                }

                evaluated += 1;

                const eval_result = self.evalDecl(def_idx) catch |err| {
                    return err;
                };

                switch (eval_result) {
                    .success => {},
                    .crash => |crash_info| {
                        crashed += 1;
                        try self.reportProblem(crash_info.message, crash_info.region, .crash);
                    },
                    .expect_failed => |expect_info| {
                        try self.reportProblem(expect_info.message, expect_info.region, .expect_failed);
                    },
                    .error_eval => |error_info| {
                        had_error_eval = true;
                        // Defer reporting — batch retry might fix this
                        try deferred_errors.append(self.allocator, .{
                            .expr_idx = def.expr,
                            .message = error_info.message,
                            .region = error_info.region,
                        });
                    },
                }
            }
        }

        // Phase 2: If any defs had error_eval (often cross-def closure failures),
        // retry ALL evaluable defs through batch lowering.
        if (had_error_eval) {
            self.retryWithBatchEval() catch {};
        }

        // Report any error_eval problems that weren't resolved by batch retry.
        // A def is "resolved" if its CIR expression was folded to a constant.
        for (deferred_errors.items) |err_info| {
            const expr = self.env.store.getExpr(err_info.expr_idx);
            // If the batch retry folded this expression, don't report the error
            if (expr == .e_num or expr == .e_zero_argument_tag) continue;
            try self.reportProblem(err_info.message, err_info.region, .error_eval);
        }

        return EvalSummary{
            .evaluated = evaluated,
            .crashed = crashed,
        };
    }

    /// Retry failed defs via batch evaluation. Lowers all evaluable defs
    /// through a single shared pipeline and re-folds any that succeed.
    /// Already-folded defs are skipped by tryFoldExprFromValue.
    fn retryWithBatchEval(self: *ComptimeEvaluator) !void {
        const eval_order = self.env.evaluation_order orelse return;

        // Collect evaluable defs
        var evaluable_defs: std.ArrayList(CIR.Def.Idx) = .empty;
        defer evaluable_defs.deinit(self.allocator);

        for (eval_order.sccs) |scc| {
            for (scc.defs) |def_idx| {
                const def = self.env.store.getDef(def_idx);
                if (self.failed_literal_exprs.contains(def.expr)) continue;

                const expr = self.env.store.getExpr(def.expr);
                switch (expr) {
                    .e_lambda, .e_closure, .e_hosted_lambda => continue,
                    .e_runtime_error, .e_anno_only, .e_lookup_required => continue,
                    else => {},
                }

                const type_var = ModuleEnv.varFrom(def.expr);
                if (shouldSkipComptimeEvalForType(self.allocator, &self.env.types, type_var)) continue;

                try evaluable_defs.append(self.allocator, def_idx);
            }
        }

        if (evaluable_defs.items.len < 2) return;

        // Batch-lower all defs through one shared pipeline
        var batch_result = self.lir_program.lowerModuleDefs(
            self.env,
            evaluable_defs.items,
            self.all_module_envs,
        ) catch return;
        defer batch_result.deinit();

        // Evaluate the synthetic block (all defs chained with decl_const statements)
        var interp = try LirInterpreter.init(
            self.allocator,
            &batch_result.lir_store,
            batch_result.layout_store,
            self.io,
        );
        interp.detect_infinite_while_loops = true;
        defer interp.deinit();

        _ = interp.eval(batch_result.block_expr_id) catch return;

        // Extract per-def values from bindings and fold to CIR.
        // Already-folded defs (from per-def pass) are skipped by tryFoldExprFromValue.
        for (batch_result.def_lir_exprs) |def_entry| {
            const binding = interp.bindings.get(def_entry.symbol.raw()) orelse
                (interp.top_level_cache.get(def_entry.symbol.raw()) orelse continue);

            self.tryFoldExprFromValue(
                def_entry.expr_idx,
                binding.val,
                def_entry.result_layout,
                batch_result.layout_store,
            ) catch {};
        }
    }

    /// Evaluate and fold a standalone expression (not part of a def).
    /// This is used for mono tests where we have a single expression to evaluate.
    /// Returns true if the expression was successfully evaluated and folded.
    pub fn evalAndFoldExpr(self: *ComptimeEvaluator, expr_idx: CIR.Expr.Idx) !bool {
        // Lower CIR → LIR
        var lower_result = self.lir_program.lowerExpr(
            self.env,
            expr_idx,
            self.all_module_envs,
            null,
        ) catch return false;
        defer lower_result.deinit();

        // Evaluate via LIR interpreter
        var interp = try LirInterpreter.init(self.allocator, &lower_result.lir_store, lower_result.layout_store, self.io);
        interp.detect_infinite_while_loops = true;
        defer interp.deinit();

        const eval_result = interp.eval(lower_result.final_expr_id) catch return false;
        const result_value = switch (eval_result) {
            .value => |v| v,
            .early_return => |v| v,
            .break_expr => return false,
        };

        // Fold the result into a constant
        self.tryFoldExprFromValue(expr_idx, result_value, lower_result.result_layout, lower_result.layout_store) catch return false;

        return true;
    }

    /// Find a module environment by its qualified ident.
    fn findModuleEnvByIdent(self: *const ComptimeEvaluator, module_ident: base.Ident.Idx) ?*const ModuleEnv {
        for (self.other_envs) |env| {
            if (env.qualified_module_ident.eql(module_ident)) return env;
        }
        return null;
    }
};
