//! Compile-Time Constant Evaluation
//!
//! This module evaluates constants at compile time by:
//! 1. Generating machine code for each constant's expression
//! 2. Executing that code with compile-time-safe RocOps
//! 3. Extracting the resulting value
//! 4. Binding the value to the constant's symbol in the scope
//!
//! This enables constant folding: when generating code for the main expression,
//! lookups to constants resolve to pre-computed values instead of re-evaluating at runtime.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mono = @import("mono");
const backend = @import("backend");
const builtins = @import("builtins");
const crash_context = @import("crash_context.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

const MonoExprCodeGen = backend.MonoExprCodeGen;
const MonoScope = backend.MonoScope;
const ExecutableMemory = backend.ExecutableMemory;

const MonoIR = mono.MonoIR;
const MonoExprStore = mono.MonoExprStore;
const MonoLower = mono.Lower;
const MonoExprId = MonoIR.MonoExprId;
const MonoSymbol = MonoIR.MonoSymbol;

const LayoutIdx = layout.Idx;

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const CrashContext = crash_context.CrashContext;
const CrashState = crash_context.CrashState;

/// Context for compile-time constant evaluation.
///
/// Provides RocOps callbacks that are safe for compile-time evaluation:
/// - Arena-based allocation (dealloc is a no-op)
/// - Crash capture (doesn't terminate, records message for diagnostics)
pub const ConstantEvalContext = struct {
    allocator: Allocator,
    crash: CrashContext,
    roc_ops: RocOps,

    pub fn init(allocator: Allocator) ConstantEvalContext {
        var self = ConstantEvalContext{
            .allocator = allocator,
            .crash = CrashContext.init(allocator),
            .roc_ops = undefined,
        };

        // Set up RocOps with compile-time-safe callbacks
        self.roc_ops = RocOps{
            .env = @ptrCast(&self),
            .roc_alloc = &rocAllocFn,
            .roc_dealloc = &rocDeallocFn,
            .roc_realloc = &rocReallocFn,
            .roc_dbg = &rocDbgFn,
            .roc_expect_failed = &rocExpectFailedFn,
            .roc_crashed = &rocCrashedFn,
            .hosted_fns = .{ .count = 0, .fns = undefined },
        };

        return self;
    }

    pub fn deinit(self: *ConstantEvalContext) void {
        self.crash.deinit();
    }

    /// Reset crash state for evaluating the next constant.
    pub fn reset(self: *ConstantEvalContext) void {
        self.crash.reset();
    }

    /// Get the crash message if the evaluation crashed.
    pub fn crashMessage(self: *ConstantEvalContext) ?[]const u8 {
        return self.crash.crashMessage();
    }

    // RocOps callbacks for compile-time evaluation

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *ConstantEvalContext = @ptrCast(@alignCast(env));

        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_alloc.length),
            else => {
                std.debug.print("ConstantEvalContext: Unsupported alignment {d}\n", .{roc_alloc.alignment});
                unreachable;
            },
        } catch {
            std.debug.print("ConstantEvalContext: Allocation failed\n", .{});
            unreachable;
        };

        roc_alloc.answer = @ptrCast(ptr.ptr);
    }

    fn rocDeallocFn(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
        // No-op: arena manages deallocation
    }

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *ConstantEvalContext = @ptrCast(@alignCast(env));

        const new_ptr = switch (roc_realloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_realloc.new_length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_realloc.new_length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_realloc.new_length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_realloc.new_length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_realloc.new_length),
            else => {
                std.debug.print("ConstantEvalContext: Unsupported alignment {d}\n", .{roc_realloc.alignment});
                unreachable;
            },
        } catch {
            std.debug.print("ConstantEvalContext: Reallocation failed\n", .{});
            unreachable;
        };

        // Copy old data
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        @memcpy(new_ptr[0..roc_realloc.new_length], old_ptr[0..roc_realloc.new_length]);

        roc_realloc.answer = @ptrCast(new_ptr.ptr);
    }

    fn rocDbgFn(roc_dbg: *const RocDbg, _: *anyopaque) callconv(.c) void {
        const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
        std.debug.print("[compile-time dbg] {s}\n", .{msg});
    }

    fn rocExpectFailedFn(expect_failed: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
        const self: *ConstantEvalContext = @ptrCast(@alignCast(env));
        const msg = expect_failed.utf8_bytes[0..expect_failed.len];
        self.crash.recordCrash(msg) catch {};
    }

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *ConstantEvalContext = @ptrCast(@alignCast(env));
        const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
        // Record the crash message instead of terminating
        self.crash.recordCrash(msg) catch {};
    }
};

/// Value types that can be computed at compile time.
pub const ConstantValue = union(enum) {
    i64_val: i64,
    i128_val: i128,
    f64_val: f64,
    eval_failed: void, // Constant couldn't be evaluated at compile time
};

/// Result of evaluating a constant.
pub const EvaluatedConstant = struct {
    symbol: MonoSymbol,
    value: ConstantValue,
};

/// Evaluate a single constant expression and return its value.
///
/// This generates machine code for the constant, executes it, and extracts
/// the resulting value based on the layout.
pub fn evaluateConstant(
    ctx: *ConstantEvalContext,
    codegen: *MonoExprCodeGen,
    mono_expr_id: MonoExprId,
    symbol: MonoSymbol,
    result_layout: LayoutIdx,
    scope: *MonoScope,
) !EvaluatedConstant {
    // Generate machine code for the constant expression
    const code = codegen.generateCodeForExpr(mono_expr_id, result_layout, scope) catch {
        return EvaluatedConstant{
            .symbol = symbol,
            .value = .eval_failed,
        };
    };
    defer codegen.allocator.free(code);

    if (code.len == 0) {
        return EvaluatedConstant{
            .symbol = symbol,
            .value = .eval_failed,
        };
    }

    // Make the code executable
    var executable = ExecutableMemory.init(code) catch {
        return EvaluatedConstant{
            .symbol = symbol,
            .value = .eval_failed,
        };
    };
    defer executable.deinit();

    // Execute and extract value based on layout
    const value: ConstantValue = switch (result_layout) {
        .i64, .i8, .i16, .i32, .u64, .u8, .u16, .u32, .bool => blk: {
            var result: i64 = undefined;
            executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&ctx.roc_ops));

            // Check if evaluation crashed
            if (ctx.crashMessage() != null) {
                break :blk ConstantValue{ .eval_failed = {} };
            }

            break :blk ConstantValue{ .i64_val = result };
        },
        .i128, .u128, .dec => blk: {
            var result: i128 = undefined;
            executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&ctx.roc_ops));

            if (ctx.crashMessage() != null) {
                break :blk ConstantValue{ .eval_failed = {} };
            }

            break :blk ConstantValue{ .i128_val = result };
        },
        .f64, .f32 => blk: {
            var result: f64 = undefined;
            executable.callWithResultPtrAndRocOps(@ptrCast(&result), @constCast(&ctx.roc_ops));

            if (ctx.crashMessage() != null) {
                break :blk ConstantValue{ .eval_failed = {} };
            }

            break :blk ConstantValue{ .f64_val = result };
        },
        else => ConstantValue{ .eval_failed = {} },
    };

    return EvaluatedConstant{
        .symbol = symbol,
        .value = value,
    };
}

/// Get the MonoSymbol for a lowered constant.
///
/// A constant's symbol is determined by its definition's pattern. For assign
/// patterns, this is the identifier being bound.
pub fn loweredConstantToSymbol(
    all_module_envs: []const *ModuleEnv,
    lc: MonoLower.LoweredConstant,
) ?MonoSymbol {
    if (lc.module_idx >= all_module_envs.len) return null;

    const module_env = all_module_envs[lc.module_idx];
    const def = module_env.store.getDef(lc.def_idx);
    const pattern = module_env.store.getPattern(def.pattern);

    return switch (pattern) {
        .assign => |a| MonoSymbol{
            .module_idx = lc.module_idx,
            .ident_idx = a.ident,
        },
        else => null,
    };
}

/// Get the layout for a constant based on its expression.
///
/// This examines the Mono IR expression to determine the appropriate layout.
pub fn getConstantLayout(
    mono_store: *const MonoExprStore,
    mono_expr_id: MonoExprId,
) LayoutIdx {
    if (mono_expr_id.isNone()) return .i64;

    const expr = mono_store.getExpr(mono_expr_id);

    return switch (expr) {
        .i64_literal => .i64,
        .i128_literal => .i128,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .dec_literal => .dec,
        .bool_literal => .bool,
        .str_literal => .str,
        .lookup => |lookup| lookup.layout_idx,
        else => .i64,
    };
}

/// Evaluate all constants and bind their values to the scope.
///
/// This processes constants in the order provided by LoweredConstants (which
/// is dependency order), evaluates each one, and binds the result to the scope.
/// When code is later generated for the main expression, lookups to these
/// constants will resolve to pre-computed values.
pub fn evaluateAndBindConstants(
    allocator: Allocator,
    codegen: *MonoExprCodeGen,
    lowered_constants: MonoLower.LoweredConstants,
    all_module_envs: []const *ModuleEnv,
    scope: *MonoScope,
) !void {
    var ctx = ConstantEvalContext.init(allocator);
    defer ctx.deinit();

    for (lowered_constants.constants) |lc| {
        // Get the symbol for this constant
        const symbol = loweredConstantToSymbol(all_module_envs, lc) orelse continue;

        // Determine layout from the Mono expression
        const result_layout = getConstantLayout(codegen.mono_store, lc.mono_expr_id);

        // Reset crash state for this constant
        ctx.reset();

        // Create a child scope for evaluation (isolate from main scope bindings during eval)
        var eval_scope = scope.child();
        defer eval_scope.deinit();

        // Evaluate the constant
        const result = evaluateConstant(
            &ctx,
            codegen,
            lc.mono_expr_id,
            symbol,
            result_layout,
            &eval_scope,
        ) catch {
            // On error, bind the expression for runtime evaluation
            try scope.bindExpr(symbol, lc.mono_expr_id);
            continue;
        };

        // Bind the evaluated value (or fall back to expression)
        switch (result.value) {
            .i64_val => |v| try scope.bind(result.symbol, v),
            .i128_val => |v| try scope.bindI128(result.symbol, v),
            .f64_val => |v| try scope.bindF64(result.symbol, v),
            .eval_failed => {
                // Fall back to expr_ref - will be evaluated at runtime
                try scope.bindExpr(result.symbol, lc.mono_expr_id);
            },
        }

        // Log crash if one occurred (for future diagnostics)
        if (ctx.crashMessage()) |_| {
            // TODO: Convert to compile-time diagnostic
            // For now, the constant is bound as eval_failed above
        }
    }
}

// Tests

test "ConstantEvalContext initialization" {
    var ctx = ConstantEvalContext.init(std.testing.allocator);
    defer ctx.deinit();

    try std.testing.expect(ctx.crashMessage() == null);
}

test "ConstantEvalContext crash capture" {
    var ctx = ConstantEvalContext.init(std.testing.allocator);
    defer ctx.deinit();

    // Simulate a crash
    const crash_msg = "test crash message";
    ctx.crash.recordCrash(crash_msg) catch unreachable;

    const msg = ctx.crashMessage();
    try std.testing.expect(msg != null);
    try std.testing.expectEqualStrings("test crash message", msg.?);
}

test "ConstantEvalContext reset" {
    var ctx = ConstantEvalContext.init(std.testing.allocator);
    defer ctx.deinit();

    ctx.crash.recordCrash("first crash") catch unreachable;
    try std.testing.expect(ctx.crashMessage() != null);

    ctx.reset();
    try std.testing.expect(ctx.crashMessage() == null);
}
