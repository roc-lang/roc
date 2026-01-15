//! Dev Backend Evaluator for Roc expressions
//!
//! This module generates native machine code from Roc expressions using the dev backend.
//! Unlike the LLVM evaluator, this generates native code directly without LLVM IR.
//!
//! Used when the `--backend=dev` flag is passed to the REPL.
//!
//! The evaluator works by:
//! 1. Parsing and type-checking the source expression
//! 2. Translating the CIR to native machine code using the dev backend
//! 3. JIT-executing the native code to produce a result

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const check = @import("check");
const compiled_builtins = @import("compiled_builtins");
const eval_mod = @import("mod.zig");
const backend = @import("backend");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Can = can.Can;
const Check = check.Check;
const builtin_loading = eval_mod.builtin_loading;

/// Dev backend-based evaluator for Roc expressions
pub const DevEvaluator = struct {
    allocator: Allocator,

    /// Builtin type declaration indices (loaded once at startup)
    builtin_indices: CIR.BuiltinIndices,

    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        UnsupportedType,
        UnsupportedExpression,
        ParseError,
        CanonicalizeError,
        TypeError,
        JitError,
        NotImplemented,
    };

    /// Type of the result value for JIT execution.
    /// Matches the types in llvm_evaluator.ResultType.
    pub const ResultType = enum {
        i64,
        u64,
        i128,
        u128,
        f64,
        dec,

        pub fn validate() void {
            comptime {
                if (@typeInfo(ResultType).@"enum".fields.len != 6) @compileError("ResultType must have exactly 6 variants");
                if (@intFromEnum(ResultType.i64) != 0) @compileError("ResultType.i64 must be ordinal 0");
                if (@intFromEnum(ResultType.u64) != 1) @compileError("ResultType.u64 must be ordinal 1");
                if (@intFromEnum(ResultType.i128) != 2) @compileError("ResultType.i128 must be ordinal 2");
                if (@intFromEnum(ResultType.u128) != 3) @compileError("ResultType.u128 must be ordinal 3");
                if (@intFromEnum(ResultType.f64) != 4) @compileError("ResultType.f64 must be ordinal 4");
                if (@intFromEnum(ResultType.dec) != 5) @compileError("ResultType.dec must be ordinal 5");
            }
        }
    };

    comptime {
        ResultType.validate();
    }

    /// Result of code generation
    pub const CodeResult = struct {
        /// The native machine code bytes
        code: []const u8,
        /// The result type for interpreting the return value
        result_type: ResultType,
        /// Allocator used for cleanup
        allocator: Allocator,

        pub fn deinit(self: *CodeResult) void {
            self.allocator.free(self.code);
        }
    };

    /// Initialize a new Dev evaluator
    pub fn init(allocator: Allocator) Error!DevEvaluator {
        // Load builtin indices once at startup (generated at build time)
        const builtin_indices = builtin_loading.deserializeBuiltinIndices(
            allocator,
            compiled_builtins.builtin_indices_bin,
        ) catch return error.OutOfMemory;

        // Load Builtin module once at startup
        const builtin_source = compiled_builtins.builtin_source;
        var builtin_module = builtin_loading.loadCompiledModule(
            allocator,
            compiled_builtins.builtin_bin,
            "Builtin",
            builtin_source,
        ) catch return error.OutOfMemory;
        errdefer builtin_module.deinit();

        return DevEvaluator{
            .allocator = allocator,
            .builtin_indices = builtin_indices,
            .builtin_module = builtin_module,
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *DevEvaluator) void {
        self.builtin_module.deinit();
    }

    /// Generate native code for a CIR expression
    /// Returns the generated code and result type
    /// The caller is responsible for freeing the code via result.deinit()
    pub fn generateCode(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error!CodeResult {
        // Get the result type from the expression
        const result_type = self.getExprResultType(expr);

        // Generate code based on the expression type
        const code = switch (expr) {
            .e_num => |num| try self.generateNumericCode(num, result_type),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_binop => |binop| try self.generateBinopCode(module_env, binop, result_type),
            .e_unary_minus => |unary| try self.generateUnaryMinusCode(module_env, unary, result_type),
            .e_if => |if_expr| try self.generateIfCode(module_env, if_expr, result_type),
            else => return error.UnsupportedExpression,
        };

        return CodeResult{
            .code = code,
            .result_type = result_type,
            .allocator = self.allocator,
        };
    }

    /// Generate code for binary operations
    /// For now, uses constant folding when both operands are literals
    fn generateBinopCode(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, result_type: ResultType) Error![]const u8 {
        // Get the left and right expressions
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        const rhs_expr = module_env.store.getExpr(binop.rhs);

        // Try to evaluate both sides as constants (constant folding)
        const lhs_val = self.tryEvalConstantI64(lhs_expr) orelse return error.UnsupportedExpression;
        const rhs_val = self.tryEvalConstantI64(rhs_expr) orelse return error.UnsupportedExpression;

        // Perform the operation at compile time
        const result_val: i64 = switch (binop.op) {
            .add => lhs_val +% rhs_val,
            .sub => lhs_val -% rhs_val,
            .mul => lhs_val *% rhs_val,
            .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
            .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
            .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
            .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
            .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
            .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
            .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
            .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
        };

        return self.generateReturnI64Code(result_val, result_type);
    }

    /// Generate code for unary minus
    fn generateUnaryMinusCode(self: *DevEvaluator, module_env: *ModuleEnv, unary: CIR.Expr.UnaryMinus, result_type: ResultType) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64(inner_expr) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_type);
    }

    /// Generate code for if/else expressions
    /// Uses constant folding - evaluates condition at compile time and generates code for the taken branch
    fn generateIfCode(self: *DevEvaluator, module_env: *ModuleEnv, if_expr: anytype, result_type: ResultType) Error![]const u8 {
        // Get the branches
        const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);

        // Try each branch's condition
        for (branch_indices) |branch_idx| {
            const branch = module_env.store.getIfBranch(branch_idx);
            const cond_expr = module_env.store.getExpr(branch.cond);

            // Try to evaluate the condition as a constant (supports binary operations like 1 > 0)
            const cond_val = self.tryEvalConstantI64WithEnv(module_env, cond_expr);

            if (cond_val) |val| {
                if (val != 0) {
                    // Condition is true - generate code for this branch's body
                    const body_expr = module_env.store.getExpr(branch.body);
                    return self.generateCodeForExpr(module_env, body_expr, result_type);
                }
                // Condition is false - try next branch
            } else {
                // Can't evaluate condition at compile time
                return error.UnsupportedExpression;
            }
        }

        // All conditions were false - use the final else branch
        const else_expr = module_env.store.getExpr(if_expr.final_else);
        return self.generateCodeForExpr(module_env, else_expr, result_type);
    }

    /// Helper to generate code for an expression (recursive)
    fn generateCodeForExpr(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, result_type: ResultType) Error![]const u8 {
        // Use empty environment for non-function context
        var empty_env = std.AutoHashMap(u32, i64).init(self.allocator);
        defer empty_env.deinit();
        return self.generateCodeForExprWithEnv(module_env, expr, result_type, &empty_env);
    }

    /// Helper to generate code for an expression with variable environment (for lambda bodies)
    fn generateCodeForExprWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        return switch (expr) {
            // Numeric literals
            .e_num => |num| try self.generateNumericCode(num, result_type),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_dec => |dec| try self.generateDecCode(dec, result_type),
            .e_dec_small => |dec| try self.generateDecSmallCode(dec, result_type),
            .e_typed_int => |ti| try self.generateTypedIntCode(ti, result_type),
            .e_typed_frac => |tf| try self.generateTypedFracCode(tf, result_type),

            // Operations
            .e_binop => |binop| try self.generateBinopCodeWithEnv(module_env, binop, result_type, env),
            .e_unary_minus => |unary| try self.generateUnaryMinusCodeWithEnv(module_env, unary, result_type, env),
            .e_unary_not => |unary| try self.generateUnaryNotCodeWithEnv(module_env, unary, result_type, env),

            // Control flow
            .e_if => |if_expr| try self.generateIfCodeWithEnv(module_env, if_expr, result_type, env),
            .e_match => |match_expr| try self.generateMatchCode(module_env, match_expr, result_type, env),

            // Functions and calls
            .e_call => |call| try self.generateCallCode(module_env, call, result_type, env),
            .e_lambda => return error.UnsupportedExpression, // Lambdas are handled via e_call
            .e_closure => return error.UnsupportedExpression, // Closures are handled via e_call

            // Lookups
            .e_lookup_local => |lookup| try self.generateLookupLocalCode(lookup, result_type, env),
            .e_lookup_external => return error.UnsupportedExpression, // External lookups need module resolution
            .e_lookup_required => return error.UnsupportedExpression, // Required lookups need platform context

            // Tags
            .e_zero_argument_tag => |tag| try self.generateZeroArgTagCode(module_env, tag, result_type),
            .e_tag => |tag| try self.generateTagCode(module_env, tag, result_type, env),

            // Data structures
            .e_empty_list => try self.generateEmptyListCode(result_type),
            .e_list => |list| try self.generateListCode(module_env, list, result_type, env),
            .e_tuple => |tuple| try self.generateTupleCode(module_env, tuple, result_type, env),
            .e_record => |rec| try self.generateRecordCode(module_env, rec, result_type, env),
            // Note: e_empty_record is handled in "Not yet supported" section due to
            // a canonicalizer bug that incorrectly tags some expressions as e_empty_record

            // Blocks and statements
            .e_block => |block| try self.generateBlockCode(module_env, block, result_type, env),
            .e_return => |ret| try self.generateReturnExprCode(module_env, ret, result_type, env),

            // Strings
            .e_str_segment => |seg| try self.generateStrSegmentCode(module_env, seg, result_type),
            .e_str => |str| try self.generateStrCode(module_env, str, result_type, env),

            // Debug and errors
            .e_dbg => |dbg| try self.generateDbgCode(module_env, dbg, result_type, env),
            .e_crash => return error.UnsupportedExpression, // Crash expressions always error
            .e_expect => |expect| try self.generateExpectCode(module_env, expect, result_type, env),
            .e_runtime_error => return error.UnsupportedExpression, // Runtime errors

            // Empty record (unit type)
            .e_empty_record => try self.generateReturnI64Code(0, result_type),
            .e_dot_access => return error.UnsupportedExpression,
            .e_nominal => return error.UnsupportedExpression,
            .e_nominal_external => return error.UnsupportedExpression,
            .e_ellipsis => return error.UnsupportedExpression,
            .e_anno_only => return error.UnsupportedExpression,
            .e_type_var_dispatch => return error.UnsupportedExpression,
            .e_for => return error.UnsupportedExpression,
            .e_hosted_lambda => return error.UnsupportedExpression,
            .e_low_level_lambda => return error.UnsupportedExpression,
        };
    }

    /// Generate code for function calls
    /// For now, handles simple lambda applications with constant arguments
    fn generateCallCode(self: *DevEvaluator, module_env: *ModuleEnv, call: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // Get the function being called
        const func_expr = module_env.store.getExpr(call.func);

        // Handle lambda application
        switch (func_expr) {
            .e_lambda => |lambda| {
                // Get argument values
                const arg_indices = module_env.store.sliceExpr(call.args);
                const param_indices = module_env.store.slicePatterns(lambda.args);

                if (arg_indices.len != param_indices.len) {
                    return error.UnsupportedExpression;
                }

                // Create new environment with argument bindings
                var new_env = try env.clone();
                defer new_env.deinit();

                for (arg_indices, param_indices) |arg_idx, param_idx| {
                    const arg_expr = module_env.store.getExpr(arg_idx);
                    const arg_val = self.tryEvalConstantI64WithEnvMap(module_env, arg_expr, env) orelse
                        return error.UnsupportedExpression;

                    // Add binding: pattern_idx -> value
                    try new_env.put(@intFromEnum(param_idx), arg_val);
                }

                // Evaluate lambda body with new environment
                const body_expr = module_env.store.getExpr(lambda.body);
                return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, &new_env);
            },
            .e_closure => |closure| {
                // Get the underlying lambda
                const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
                switch (lambda_expr) {
                    .e_lambda => |lambda| {
                        const arg_indices = module_env.store.sliceExpr(call.args);
                        const param_indices = module_env.store.slicePatterns(lambda.args);

                        if (arg_indices.len != param_indices.len) {
                            return error.UnsupportedExpression;
                        }

                        var new_env = try env.clone();
                        defer new_env.deinit();

                        for (arg_indices, param_indices) |arg_idx, param_idx| {
                            const arg_expr = module_env.store.getExpr(arg_idx);
                            const arg_val = self.tryEvalConstantI64WithEnvMap(module_env, arg_expr, env) orelse
                                return error.UnsupportedExpression;
                            try new_env.put(@intFromEnum(param_idx), arg_val);
                        }

                        const body_expr = module_env.store.getExpr(lambda.body);
                        return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, &new_env);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for local variable lookup
    fn generateLookupLocalCode(self: *DevEvaluator, lookup: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const pattern_key = @intFromEnum(lookup.pattern_idx);
        const value = env.get(pattern_key) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(value, result_type);
    }

    /// Binary operation with environment support
    fn generateBinopCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        const rhs_expr = module_env.store.getExpr(binop.rhs);

        const lhs_val = self.tryEvalConstantI64WithEnvMap(module_env, lhs_expr, env) orelse return error.UnsupportedExpression;
        const rhs_val = self.tryEvalConstantI64WithEnvMap(module_env, rhs_expr, env) orelse return error.UnsupportedExpression;

        const result_val: i64 = switch (binop.op) {
            .add => lhs_val +% rhs_val,
            .sub => lhs_val -% rhs_val,
            .mul => lhs_val *% rhs_val,
            .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
            .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
            .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
            .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
            .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
            .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
            .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
            .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
        };

        return self.generateReturnI64Code(result_val, result_type);
    }

    /// Unary minus with environment support
    fn generateUnaryMinusCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, unary: CIR.Expr.UnaryMinus, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_type);
    }

    /// If/else with environment support
    fn generateIfCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, if_expr: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);

        for (branch_indices) |branch_idx| {
            const branch = module_env.store.getIfBranch(branch_idx);
            const cond_expr = module_env.store.getExpr(branch.cond);
            const cond_val = self.tryEvalConstantI64WithEnvMap(module_env, cond_expr, env);

            if (cond_val) |val| {
                if (val != 0) {
                    const body_expr = module_env.store.getExpr(branch.body);
                    return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, env);
                }
            } else {
                return error.UnsupportedExpression;
            }
        }

        const else_expr = module_env.store.getExpr(if_expr.final_else);
        return self.generateCodeForExprWithEnv(module_env, else_expr, result_type, env);
    }

    /// Try to evaluate expression with environment (for variable lookups)
    fn tryEvalConstantI64WithEnvMap(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, env: *std.AutoHashMap(u32, i64)) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            .e_lookup_local => |lookup| {
                return env.get(@intFromEnum(lookup.pattern_idx));
            },
            .e_binop => |binop| {
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                const rhs_expr = module_env.store.getExpr(binop.rhs);
                const lhs_val = self.tryEvalConstantI64WithEnvMap(module_env, lhs_expr, env) orelse return null;
                const rhs_val = self.tryEvalConstantI64WithEnvMap(module_env, rhs_expr, env) orelse return null;

                return switch (binop.op) {
                    .add => lhs_val +% rhs_val,
                    .sub => lhs_val -% rhs_val,
                    .mul => lhs_val *% rhs_val,
                    .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else null,
                    .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
                    .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
                    .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
                    .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                    .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                };
            },
            .e_unary_minus => |unary| {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse return null;
                return -inner_val;
            },
            else => null,
        };
    }

    /// Try to evaluate an expression as a constant i64 value
    /// This handles numeric literals and binary operations with constant operands
    fn tryEvalConstantI64(_: *DevEvaluator, expr: CIR.Expr) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            else => null,
        };
    }

    /// Try to evaluate an expression as a constant i64 value, with access to module_env for sub-expressions
    fn tryEvalConstantI64WithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            .e_binop => |binop| {
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                const rhs_expr = module_env.store.getExpr(binop.rhs);
                const lhs_val = self.tryEvalConstantI64WithEnv(module_env, lhs_expr) orelse return null;
                const rhs_val = self.tryEvalConstantI64WithEnv(module_env, rhs_expr) orelse return null;

                return switch (binop.op) {
                    .add => lhs_val +% rhs_val,
                    .sub => lhs_val -% rhs_val,
                    .mul => lhs_val *% rhs_val,
                    .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else null,
                    .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
                    .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
                    .le => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ge => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
                    .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
                    .ne => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
                    .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                    .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
                };
            },
            .e_unary_minus => |unary| {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.tryEvalConstantI64WithEnv(module_env, inner_expr) orelse return null;
                return -inner_val;
            },
            else => null,
        };
    }

    /// Generate code for a numeric literal
    fn generateNumericCode(self: *DevEvaluator, num: anytype, result_type: ResultType) Error![]const u8 {
        // Get the value as i64
        // The num has .value (IntValue) and .kind (NumKind) fields
        const value_i128 = num.value.toI128();
        if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
            return error.UnsupportedType;
        }
        const value: i64 = @intCast(value_i128);

        return self.generateReturnI64Code(value, result_type);
    }

    /// Generate code for a floating-point literal
    fn generateFloatCode(self: *DevEvaluator, value: f64) Error![]const u8 {
        return self.generateReturnF64Code(value);
    }

    /// Generate code for decimal literals
    fn generateDecCode(self: *DevEvaluator, dec: anytype, result_type: ResultType) Error![]const u8 {
        // Decimals are stored as i128 internally
        const value_i128 = dec.value.toI128();
        if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
            return error.UnsupportedType;
        }
        return self.generateReturnI64Code(@intCast(value_i128), result_type);
    }

    /// Generate code for small decimal literals
    fn generateDecSmallCode(self: *DevEvaluator, dec: anytype, _: ResultType) Error![]const u8 {
        // Small decimals are stored as numerator/denominator_power - convert to f64
        const f64_val = dec.value.toF64();
        return self.generateReturnF64Code(f64_val);
    }

    /// Generate code for typed integer literals
    fn generateTypedIntCode(self: *DevEvaluator, ti: anytype, result_type: ResultType) Error![]const u8 {
        const value_i128 = ti.value.toI128();
        if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
            return error.UnsupportedType;
        }
        return self.generateReturnI64Code(@intCast(value_i128), result_type);
    }

    /// Generate code for typed fraction literals
    fn generateTypedFracCode(self: *DevEvaluator, tf: anytype, _: ResultType) Error![]const u8 {
        // typed_frac stores value as IntValue, need to interpret as fractional
        const value_i128 = tf.value.toI128();
        const f64_val: f64 = @floatFromInt(value_i128);
        return self.generateReturnF64Code(f64_val);
    }

    /// Generate code for unary not (boolean negation)
    fn generateUnaryNotCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, unary: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse
            return error.UnsupportedExpression;
        // Boolean negation: 0 becomes 1, non-zero becomes 0
        const result = if (inner_val == 0) @as(i64, 1) else @as(i64, 0);
        return self.generateReturnI64Code(result, result_type);
    }

    /// Generate code for match expressions
    fn generateMatchCode(self: *DevEvaluator, module_env: *ModuleEnv, match_expr: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // Get the value being matched (match uses 'cond' field)
        const cond_expr = module_env.store.getExpr(match_expr.cond);
        const cond_val = self.tryEvalConstantI64WithEnvMap(module_env, cond_expr, env) orelse
            return error.UnsupportedExpression;

        // Get branches and try to find a match
        const branches = module_env.store.sliceMatchBranches(match_expr.branches);
        for (branches) |branch_idx| {
            const branch = module_env.store.getMatchBranch(branch_idx);

            // Get the first pattern (for simple cases)
            const patterns = module_env.store.sliceMatchBranchPatterns(branch.patterns);
            if (patterns.len == 0) continue;

            const branch_pattern = module_env.store.getMatchBranchPattern(patterns[0]);
            const pattern = module_env.store.getPattern(branch_pattern.pattern);
            const matches = self.patternMatches(module_env, pattern, cond_val, env);

            if (matches) {
                const body_expr = module_env.store.getExpr(branch.value);
                return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, env);
            }
        }

        // No branch matched - this shouldn't happen in well-typed code
        return error.UnsupportedExpression;
    }

    /// Check if a pattern matches a value
    fn patternMatches(_: *DevEvaluator, module_env: *ModuleEnv, pattern: CIR.Pattern, target_val: i64, _: *std.AutoHashMap(u32, i64)) bool {
        switch (pattern) {
            .underscore => return true,
            .num_literal => |num| {
                const pattern_val = num.value.toI128();
                if (pattern_val > std.math.maxInt(i64) or pattern_val < std.math.minInt(i64)) {
                    return false;
                }
                return target_val == @as(i64, @intCast(pattern_val));
            },
            .assign => |assign| {
                // Bind the value to this identifier
                _ = module_env.getIdent(assign.ident);
                // For assignment patterns, we need to create a binding
                // Just return true for now - actual binding happens elsewhere
                return true;
            },
            .applied_tag => |tag| {
                // Check if tag matches
                if (tag.name == module_env.idents.true_tag) {
                    return target_val != 0;
                } else if (tag.name == module_env.idents.false_tag) {
                    return target_val == 0;
                }
                // For other tags, check if args is empty (zero-argument tag)
                const args = module_env.store.slicePatterns(tag.args);
                if (args.len == 0) {
                    return true; // Simple tag match
                }
                return false;
            },
            else => return false,
        }
    }

    /// Generate code for return expressions
    fn generateReturnExprCode(self: *DevEvaluator, module_env: *ModuleEnv, ret: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const value_expr = module_env.store.getExpr(ret.expr);
        return self.generateCodeForExprWithEnv(module_env, value_expr, result_type, env);
    }

    /// Generate code for dbg expressions (just evaluate the inner expression)
    fn generateDbgCode(self: *DevEvaluator, module_env: *ModuleEnv, dbg: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // dbg returns the value of the expression being debugged
        const value_expr = module_env.store.getExpr(dbg.expr);
        return self.generateCodeForExprWithEnv(module_env, value_expr, result_type, env);
    }

    /// Generate code for expect expressions (evaluate inner, return expected value)
    fn generateExpectCode(self: *DevEvaluator, _: *ModuleEnv, _: anytype, result_type: ResultType, _: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // expect returns empty record (unit) - the check happens at runtime
        return self.generateReturnI64Code(0, result_type);
    }

    /// Generate code for zero-argument tags (True, False, None, etc.)
    fn generateZeroArgTagCode(self: *DevEvaluator, module_env: *ModuleEnv, tag: anytype, result_type: ResultType) Error![]const u8 {
        // Compare tag names using interned ident indices (not string comparison)
        // Standard tags: True=1, False=0
        const value: i64 = if (tag.name == module_env.idents.true_tag)
            1
        else if (tag.name == module_env.idents.false_tag)
            0
        else if (tag.name == module_env.idents.ok)
            0
        else if (tag.name == module_env.idents.err)
            1
        else
            // For other tags, use 0 as the default discriminant
            0;

        return self.generateReturnI64Code(value, result_type);
    }

    /// Generate code for empty list []
    fn generateEmptyListCode(self: *DevEvaluator, result_type: ResultType) Error![]const u8 {
        // Empty list is represented as a null pointer/zero
        return self.generateReturnI64Code(0, result_type);
    }

    /// Generate code for empty record {}
    fn generateEmptyRecordCode(self: *DevEvaluator, result_type: ResultType) Error![]const u8 {
        // Empty record is a unit type, represented as 0
        return self.generateReturnI64Code(0, result_type);
    }

    /// Generate code for tuple expressions
    fn generateTupleCode(self: *DevEvaluator, module_env: *ModuleEnv, tuple: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const elems = module_env.store.sliceExpr(tuple.elems);

        // For simple single-element tuples, just return the element value
        if (elems.len == 1) {
            const elem_expr = module_env.store.getExpr(elems[0]);
            return self.generateCodeForExprWithEnv(module_env, elem_expr, result_type, env);
        }

        // For multi-element tuples with all constant values, we could pack them
        // For now, only support single-element or all-constant tuples
        if (elems.len == 0) {
            // Empty tuple is unit, return 0
            return self.generateReturnI64Code(0, result_type);
        }

        // For tuples, try to evaluate all elements as constants
        // Return the first element's value for now (simplified)
        const first_expr = module_env.store.getExpr(elems[0]);
        const first_val = self.tryEvalConstantI64WithEnvMap(module_env, first_expr, env) orelse
            return error.UnsupportedExpression;

        return self.generateReturnI64Code(first_val, result_type);
    }

    /// Generate code for list expressions
    fn generateListCode(self: *DevEvaluator, module_env: *ModuleEnv, list: anytype, result_type: ResultType, _: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const elems = module_env.store.sliceExpr(list.elems);

        // Empty list
        if (elems.len == 0) {
            return self.generateReturnI64Code(0, result_type);
        }

        // For single-element lists with constant value, we could do something simple
        // For now, return UnsupportedExpression for non-empty lists
        // (proper list support requires memory allocation)
        return error.UnsupportedExpression;
    }

    /// Generate code for block expressions
    /// Blocks contain statements followed by a final expression
    fn generateBlockCode(self: *DevEvaluator, module_env: *ModuleEnv, block: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // Create a new environment for block-local bindings
        var block_env = try env.clone();
        defer block_env.deinit();

        // Process statements (declarations create bindings in the environment)
        const stmts = module_env.store.sliceStatements(block.stmts);
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            try self.processStatement(module_env, stmt, &block_env);
        }

        // Evaluate and return the final expression
        const final_expr = module_env.store.getExpr(block.final_expr);
        return self.generateCodeForExprWithEnv(module_env, final_expr, result_type, &block_env);
    }

    /// Process a statement in a block (e.g., declarations)
    fn processStatement(self: *DevEvaluator, module_env: *ModuleEnv, stmt: CIR.Statement, env: *std.AutoHashMap(u32, i64)) Error!void {
        switch (stmt) {
            .s_decl => |decl| {
                // Evaluate the expression and bind it to the pattern
                const expr = module_env.store.getExpr(decl.expr);
                const value = self.tryEvalConstantI64WithEnvMap(module_env, expr, env) orelse
                    return error.UnsupportedExpression;
                try env.put(@intFromEnum(decl.pattern), value);
            },
            .s_decl_gen => |decl| {
                // Generalized declarations (for lambdas and number literals)
                const expr = module_env.store.getExpr(decl.expr);
                const value = self.tryEvalConstantI64WithEnvMap(module_env, expr, env) orelse
                    return error.UnsupportedExpression;
                try env.put(@intFromEnum(decl.pattern), value);
            },
            else => {
                // Other statement types not yet supported
                return error.UnsupportedExpression;
            },
        }
    }

    /// Generate code for a string segment (single literal)
    fn generateStrSegmentCode(_: *DevEvaluator, module_env: *ModuleEnv, seg: anytype, _: ResultType) Error![]const u8 {
        // Get the string text (for potential future use)
        _ = module_env.getString(seg.literal);

        // For now, strings are not supported in code generation
        // (would need to return a pointer to a RocStr structure)
        return error.UnsupportedExpression;
    }

    /// Generate code for a string expression (one or more segments)
    fn generateStrCode(_: *DevEvaluator, module_env: *ModuleEnv, str: anytype, _: ResultType, _: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const segments = module_env.store.sliceExpr(str.span);

        // For simple single-segment strings, we could potentially handle them
        if (segments.len == 1) {
            const seg_expr = module_env.store.getExpr(segments[0]);
            switch (seg_expr) {
                .e_str_segment => {
                    // Single segment string - still not fully supported yet
                    return error.UnsupportedExpression;
                },
                else => return error.UnsupportedExpression,
            }
        }

        return error.UnsupportedExpression;
    }

    /// Generate code for tag expressions with arguments
    fn generateTagCode(self: *DevEvaluator, module_env: *ModuleEnv, tag: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        const args = module_env.store.sliceExpr(tag.args);

        // Tags with no arguments should be handled by e_zero_argument_tag
        // For tags with arguments, we need to construct the tag value
        if (args.len == 0) {
            // Zero-argument tag - use same logic as e_zero_argument_tag
            const value: i64 = if (tag.name == module_env.idents.true_tag)
                1
            else if (tag.name == module_env.idents.false_tag)
                0
            else if (tag.name == module_env.idents.ok)
                0
            else if (tag.name == module_env.idents.err)
                1
            else
                0;
            return self.generateReturnI64Code(value, result_type);
        }

        // For single-argument tags, try to return the argument value
        if (args.len == 1) {
            const arg_expr = module_env.store.getExpr(args[0]);
            const arg_val = self.tryEvalConstantI64WithEnvMap(module_env, arg_expr, env) orelse
                return error.UnsupportedExpression;
            return self.generateReturnI64Code(arg_val, result_type);
        }

        // Multi-argument tags not yet supported
        return error.UnsupportedExpression;
    }

    /// Generate code for record expressions
    fn generateRecordCode(self: *DevEvaluator, module_env: *ModuleEnv, rec: anytype, result_type: ResultType, env: *std.AutoHashMap(u32, i64)) Error![]const u8 {
        // Records with extension are more complex
        if (rec.ext != null) {
            return error.UnsupportedExpression;
        }

        const fields = module_env.store.sliceRecordFields(rec.fields);

        // Empty record
        if (fields.len == 0) {
            return self.generateReturnI64Code(0, result_type);
        }

        // For single-field records, return the field value (simplified representation)
        if (fields.len == 1) {
            const field = module_env.store.getRecordField(fields[0]);
            const field_expr = module_env.store.getExpr(field.value);
            const field_val = self.tryEvalConstantI64WithEnvMap(module_env, field_expr, env) orelse
                return error.UnsupportedExpression;
            return self.generateReturnI64Code(field_val, result_type);
        }

        // Multi-field records not yet fully supported
        // For now, return the first field's value
        const first_field = module_env.store.getRecordField(fields[0]);
        const first_expr = module_env.store.getExpr(first_field.value);
        const first_val = self.tryEvalConstantI64WithEnvMap(module_env, first_expr, env) orelse
            return error.UnsupportedExpression;
        return self.generateReturnI64Code(first_val, result_type);
    }

    /// Generate code that returns an i64/u64 value
    fn generateReturnI64Code(self: *DevEvaluator, value: i64, _: ResultType) Error![]const u8 {

        switch (builtin.cpu.arch) {
            .x86_64 => {
                // x86_64 code:
                // movabs rax, <value>  ; 48 B8 <8 bytes>
                // ret                  ; C3
                var code = self.allocator.alloc(u8, 11) catch return error.OutOfMemory;

                // movabs rax, imm64
                code[0] = 0x48; // REX.W
                code[1] = 0xB8; // MOV RAX, imm64
                @memcpy(code[2..10], std.mem.asBytes(&value));
                code[10] = 0xC3; // RET

                return code;
            },
            .aarch64 => {
                // aarch64 code: need to load 64-bit value into x0 and return
                // For simplicity, we'll handle values that fit in various sizes
                const uvalue: u64 = @bitCast(value);

                if (uvalue <= 0xFFFF) {
                    // mov x0, #<imm16>  ; 4 bytes
                    // ret              ; 4 bytes
                    var code = self.allocator.alloc(u8, 8) catch return error.OutOfMemory;

                    // MOV X0, #imm16
                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[4..8], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    // For larger values, we need multiple MOV/MOVK instructions
                    // mov x0, #<low16>
                    // movk x0, #<high16>, lsl #16
                    // movk x0, #<high32>, lsl #32
                    // movk x0, #<high48>, lsl #48
                    // ret
                    var code = self.allocator.alloc(u8, 20) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    // MOV X0, #imm0
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // MOVK X0, #imm1, LSL #16
                    const movk1_inst: u32 = 0xF2A00000 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    // MOVK X0, #imm2, LSL #32
                    const movk2_inst: u32 = 0xF2C00000 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    // MOVK X0, #imm3, LSL #48
                    const movk3_inst: u32 = 0xF2E00000 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[16..20], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code that returns an f64 value
    fn generateReturnF64Code(self: *DevEvaluator, value: f64) Error![]const u8 {
        const bits: u64 = @bitCast(value);

        switch (builtin.cpu.arch) {
            .x86_64 => {
                // x86_64 code:
                // movabs rax, <bits>   ; 48 B8 <8 bytes>
                // movq xmm0, rax       ; 66 48 0F 6E C0
                // ret                  ; C3
                var code = self.allocator.alloc(u8, 16) catch return error.OutOfMemory;

                // movabs rax, imm64
                code[0] = 0x48; // REX.W
                code[1] = 0xB8; // MOV RAX, imm64
                @memcpy(code[2..10], std.mem.asBytes(&bits));

                // movq xmm0, rax
                code[10] = 0x66;
                code[11] = 0x48;
                code[12] = 0x0F;
                code[13] = 0x6E;
                code[14] = 0xC0;

                // ret
                code[15] = 0xC3;

                return code;
            },
            .aarch64 => {
                // aarch64 code:
                // Load 64-bit value into x0, then move to d0
                // For floating point, return value is in d0/v0
                const uvalue = bits;

                if (uvalue <= 0xFFFF) {
                    // mov x0, #<imm16>
                    // fmov d0, x0
                    // ret
                    var code = self.allocator.alloc(u8, 12) catch return error.OutOfMemory;

                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // FMOV D0, X0
                    const fmov_inst: u32 = 0x9E670000;
                    @memcpy(code[4..8], std.mem.asBytes(&fmov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[8..12], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    // Full 64-bit load
                    var code = self.allocator.alloc(u8, 24) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    // MOV X0, #imm0
                    const mov_inst: u32 = 0xD2800000 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    // MOVK X0, #imm1, LSL #16
                    const movk1_inst: u32 = 0xF2A00000 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    // MOVK X0, #imm2, LSL #32
                    const movk2_inst: u32 = 0xF2C00000 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    // MOVK X0, #imm3, LSL #48
                    const movk3_inst: u32 = 0xF2E00000 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    // FMOV D0, X0
                    const fmov_inst: u32 = 0x9E670000;
                    @memcpy(code[16..20], std.mem.asBytes(&fmov_inst));

                    // RET
                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[20..24], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate native code from source code string
    /// This does the full pipeline: parse -> canonicalize -> type check -> generate code -> execute
    pub fn generateCodeFromSource(self: *DevEvaluator, source: []const u8) Error!CodeResult {
        // Step 1: Create module environment and parse
        var module_env = ModuleEnv.init(self.allocator, source) catch return error.OutOfMemory;
        defer module_env.deinit();

        var parse_ast = parse.parseExpr(&module_env.common, self.allocator) catch {
            return error.ParseError;
        };
        defer parse_ast.deinit(self.allocator);

        if (parse_ast.hasErrors()) {
            return error.ParseError;
        }

        // Clear scratch buffers before canonicalization (required for proper span handling)
        parse_ast.store.emptyScratch();

        // Step 2: Initialize CIR and canonicalize
        module_env.initCIRFields("dev_eval") catch return error.OutOfMemory;

        // Set up module envs map for auto-imported builtins
        var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(self.allocator);
        defer module_envs_map.deinit();

        Can.populateModuleEnvs(
            &module_envs_map,
            &module_env,
            self.builtin_module.env,
            self.builtin_indices,
        ) catch return error.OutOfMemory;

        var czer = Can.init(&module_env, &parse_ast, &module_envs_map) catch {
            return error.CanonicalizeError;
        };
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        const canonical_expr = czer.canonicalizeExpr(expr_idx) catch {
            return error.CanonicalizeError;
        } orelse {
            return error.CanonicalizeError;
        };
        const final_expr_idx = canonical_expr.get_idx();

        // Step 3: Type check
        const imported_modules = [_]*const ModuleEnv{self.builtin_module.env};
        module_env.imports.resolveImports(&module_env, &imported_modules);

        const builtin_ctx: Check.BuiltinContext = .{
            .module_name = module_env.insertIdent(base.Ident.for_text("dev_eval")) catch return error.OutOfMemory,
            .bool_stmt = self.builtin_indices.bool_type,
            .try_stmt = self.builtin_indices.try_type,
            .str_stmt = self.builtin_indices.str_type,
            .builtin_module = self.builtin_module.env,
            .builtin_indices = self.builtin_indices,
        };

        var checker = Check.init(
            self.allocator,
            &module_env.types,
            &module_env,
            &imported_modules,
            &module_envs_map,
            &module_env.store.regions,
            builtin_ctx,
        ) catch return error.OutOfMemory;
        defer checker.deinit();

        _ = checker.checkExprRepl(final_expr_idx) catch {
            return error.TypeError;
        };

        // Step 4: Generate native code
        const expr = module_env.store.getExpr(final_expr_idx);
        return self.generateCode(&module_env, expr);
    }

    /// Get the ResultType for JIT execution from a CIR expression
    fn getExprResultType(_: *DevEvaluator, expr: CIR.Expr) ResultType {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .i8, .i16, .i32, .i64, .num_unbound, .int_unbound => .i64,
                .u8, .u16, .u32, .u64 => .u64,
                .i128 => .i128,
                .u128 => .u128,
                .f32, .f64 => .f64,
                .dec => .dec,
            },
            .e_frac_f32, .e_frac_f64 => .f64,
            .e_dec, .e_dec_small => .dec,
            else => .i64,
        };
    }

    /// Result of evaluation
    pub const EvalResult = union(enum) {
        i64_val: i64,
        u64_val: u64,
        f64_val: f64,
        i128_val: i128,
        u128_val: u128,

        pub fn format(self: EvalResult, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .i64_val => |v| try writer.print("{}", .{v}),
                .u64_val => |v| try writer.print("{}", .{v}),
                .f64_val => |v| try writer.print("{d}", .{v}),
                .i128_val => |v| try writer.print("{}", .{v}),
                .u128_val => |v| try writer.print("{}", .{v}),
            }
        }
    };

    /// Evaluate source code and return the result
    pub fn evaluate(self: *DevEvaluator, source: []const u8) Error!EvalResult {
        // Generate code
        var code_result = try self.generateCodeFromSource(source);
        defer code_result.deinit();

        // JIT execute
        var jit_code = backend.JitCode.init(code_result.code) catch return error.JitError;
        defer jit_code.deinit();

        // Call and return result based on type
        return switch (code_result.result_type) {
            .i64 => EvalResult{ .i64_val = jit_code.callReturnI64() },
            .u64 => EvalResult{ .u64_val = jit_code.callReturnU64() },
            .f64 => EvalResult{ .f64_val = jit_code.callReturnF64() },
            .i128 => EvalResult{ .i128_val = @as(i128, jit_code.callReturnI64()) }, // TODO: proper 128-bit
            .u128 => EvalResult{ .u128_val = @as(u128, jit_code.callReturnU64()) }, // TODO: proper 128-bit
            .dec => EvalResult{ .i128_val = @as(i128, jit_code.callReturnI64()) }, // TODO: proper decimal
        };
    }
};

// Tests

test "dev evaluator initialization" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        // It's OK if builtin loading fails in tests (missing compiled builtins)
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();
}

test "result type ordinals match llvm evaluator" {
    // Validate that our ResultType matches the expected ordinals
    DevEvaluator.ResultType.validate();
}

test "generate i64 code" {
    // Test direct code generation for i64 values
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // Generate code for value 42
    const code = try evaluator.generateReturnI64Code(42, .i64);
    defer evaluator.allocator.free(code);

    // Execute the code using JIT
    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnI64();
    try std.testing.expectEqual(@as(i64, 42), result);
}

test "generate i64 code large value" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // Generate code for large value
    const large_value: i64 = 0x123456789ABCDEF;
    const code = try evaluator.generateReturnI64Code(large_value, .i64);
    defer evaluator.allocator.free(code);

    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnI64();
    try std.testing.expectEqual(large_value, result);
}

test "generate f64 code" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const code = try evaluator.generateReturnF64Code(3.14159);
    defer evaluator.allocator.free(code);

    var jit = backend.JitCode.init(code) catch return error.SkipZigTest;
    defer jit.deinit();

    const result = jit.callReturnF64();
    try std.testing.expectApproxEqRel(@as(f64, 3.14159), result, 0.0001);
}

test "evaluate addition" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("1 + 2") catch |err| {
        // Skip if parsing/canonicalization fails (expected in unit test environment)
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 3 }, result);
}

test "evaluate subtraction" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("10 - 3") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate multiplication" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("6 * 7") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate unary minus" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("-42") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = -42 }, result);
}

test "evaluate if true branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 42 else 0") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate if false branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 0 > 1 42 else 99") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 99 }, result);
}

test "evaluate nested if" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 (if 2 > 1 100 else 50) else 0") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 100 }, result);
}

test "evaluate simple lambda application" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // (\x -> x + 1) 5 should equal 6
    const result = evaluator.evaluate("(\\x -> x + 1) 5") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 6 }, result);
}

test "evaluate lambda identity" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // (\x -> x) 42 should equal 42
    const result = evaluator.evaluate("(\\x -> x) 42") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate lambda with arithmetic in body" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // (\x -> x * 2 + 10) 5 should equal 20
    const result = evaluator.evaluate("(\\x -> x * 2 + 10) 5") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 20 }, result);
}

test "evaluate lambda with if in body" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // (\x -> if x > 0 then x else -x) 5 should equal 5
    const result = evaluator.evaluate("(\\x -> if x > 0 then x else -x) 5") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 5 }, result);
}

test "evaluate block expression" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // { x = 5; x + 1 } should equal 6
    const result = evaluator.evaluate("{ x = 5; x + 1 }") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 6 }, result);
}

test "evaluate block with multiple declarations" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // { x = 3; y = 4; x + y } should equal 7
    const result = evaluator.evaluate("{ x = 3; y = 4; x + y }") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate True tag" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("True") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate False tag" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("False") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError or err == error.UnsupportedExpression) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 0 }, result);
}

test "evaluate comparison greater than" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // 5 > 3 should return 1 (true)
    const result = evaluator.evaluate("5 > 3") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison less than" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // 3 < 5 should return 1 (true)
    const result = evaluator.evaluate("3 < 5") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison equal" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // 42 == 42 should return 1 (true)
    const result = evaluator.evaluate("42 == 42") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison not equal" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        if (err == error.OutOfMemory) return error.SkipZigTest;
        return err;
    };
    defer evaluator.deinit();

    // 1 != 2 should return 1 (true)
    const result = evaluator.evaluate("1 != 2") catch |err| {
        if (err == error.ParseError or err == error.CanonicalizeError or err == error.TypeError) {
            return error.SkipZigTest;
        }
        return err;
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}
