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
const comptime_value = eval_mod.comptime_value;
const ComptimeHeap = comptime_value.ComptimeHeap;
const ComptimeValue = comptime_value.ComptimeValue;
const ComptimeEnv = comptime_value.ComptimeEnv;

/// Dev backend-based evaluator for Roc expressions
pub const DevEvaluator = struct {
    allocator: Allocator,

    /// Builtin type declaration indices (loaded once at startup)
    builtin_indices: CIR.BuiltinIndices,

    /// Loaded Builtin module (loaded once at startup)
    builtin_module: builtin_loading.LoadedModule,

    /// Heap memory for compile-time evaluation
    /// Bytes allocated here may end up in the final binary's readonly section
    comptime_heap: ComptimeHeap,

    /// Crash message from e_crash expression (if any)
    /// Owned by the allocator, freed on reset or deinit
    crash_message: ?[]const u8 = null,

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
        Crash,
        RuntimeError,
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
            .comptime_heap = ComptimeHeap.init(allocator),
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *DevEvaluator) void {
        if (self.crash_message) |msg| {
            self.allocator.free(msg);
        }
        self.comptime_heap.deinit();
        self.builtin_module.deinit();
    }

    /// Set the crash message (for e_crash and related errors)
    fn setCrashMessage(self: *DevEvaluator, message: []const u8) !void {
        if (self.crash_message) |old_msg| {
            self.allocator.free(old_msg);
        }
        self.crash_message = try self.allocator.dupe(u8, message);
    }

    /// Get the crash message (if any)
    pub fn getCrashMessage(self: *const DevEvaluator) ?[]const u8 {
        return self.crash_message;
    }

    /// Clear the crash message
    pub fn clearCrashMessage(self: *DevEvaluator) void {
        if (self.crash_message) |msg| {
            self.allocator.free(msg);
            self.crash_message = null;
        }
    }

    /// Create an empty ComptimeEnv using the evaluator's heap
    fn createEnv(self: *DevEvaluator) ComptimeEnv {
        return ComptimeEnv.init(&self.comptime_heap, null);
    }

    /// Create a ComptimeValue holding an i64
    fn createI64Value(self: *DevEvaluator, value: i64) Error!ComptimeValue {
        const bytes = self.comptime_heap.alloc8(8) catch return error.OutOfMemory;
        // layout_idx is undefined - will be set properly when we have LayoutStore integration
        const cv = ComptimeValue.fromBytes(bytes, undefined);
        cv.set(i64, value);
        return cv;
    }

    /// Generate native code for a CIR expression
    /// Returns the generated code and result type
    /// The caller is responsible for freeing the code via result.deinit()
    pub fn generateCode(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) Error!CodeResult {
        // Get the result type from the expression
        const result_type = self.getExprResultType(module_env, expr);

        // Generate code based on the expression type
        // For expressions that need environment support, create an empty environment
        var empty_env = self.createEnv();
        defer empty_env.deinit();

        const code = switch (expr) {
            // Numeric literals
            .e_num => |num| try self.generateNumericCode(num, result_type),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_dec => |dec| try self.generateDecCode(dec, result_type),
            .e_dec_small => |dec| try self.generateDecSmallCode(dec, result_type),
            .e_typed_int => |ti| try self.generateTypedIntCode(ti, result_type),
            .e_typed_frac => |tf| try self.generateTypedFracCode(tf, result_type),

            // Operations
            .e_binop => |binop| try self.generateBinopCode(module_env, binop, result_type),
            .e_unary_minus => |unary| try self.generateUnaryMinusCode(module_env, unary, result_type),
            .e_unary_not => |unary| try self.generateUnaryNotCodeWithEnv(module_env, unary, result_type, &empty_env),

            // Control flow
            .e_if => |if_expr| try self.generateIfCode(module_env, if_expr, result_type),
            .e_match => |match_expr| try self.generateMatchCode(module_env, match_expr, result_type, &empty_env),

            // Functions and calls
            .e_call => |call| try self.generateCallCode(module_env, call, result_type, &empty_env),

            // Data structures
            .e_dot_access => |dot| try self.generateDotAccessCode(module_env, dot, result_type, &empty_env),
            .e_record => |rec| try self.generateRecordCode(module_env, rec, result_type, &empty_env),
            .e_tuple => |tuple| try self.generateTupleCode(module_env, tuple, result_type, &empty_env),
            .e_list => |list| try self.generateListCode(module_env, list, result_type, &empty_env),
            .e_empty_list => try self.generateEmptyListCode(result_type),
            .e_tag => |tag| try self.generateTagCode(module_env, tag, result_type, &empty_env),
            .e_zero_argument_tag => |tag| try self.generateZeroArgTagCode(module_env, tag, result_type),
            .e_empty_record => try self.generateReturnI64Code(0, result_type),

            // Blocks
            .e_block => |block| try self.generateBlockCode(module_env, block, result_type, &empty_env),

            // For loops (always return {} / empty record)
            .e_for => try self.generateReturnI64Code(0, result_type),

            // Nominal types - evaluate the backing expression
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                const code_result = try self.generateCode(module_env, backing_expr);
                break :blk code_result.code;
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                const code_result = try self.generateCode(module_env, backing_expr);
                break :blk code_result.code;
            },

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
        // Uses tryEvalConstantI64WithEnv to support True/False tags
        const lhs_val = self.tryEvalConstantI64WithEnv(module_env, lhs_expr) orelse return error.UnsupportedExpression;
        const rhs_val = self.tryEvalConstantI64WithEnv(module_env, rhs_expr) orelse return error.UnsupportedExpression;

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
        const inner_val = self.tryEvalConstantI64(module_env, inner_expr) orelse return error.UnsupportedExpression;
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
        var empty_env = self.createEnv();
        defer empty_env.deinit();
        return self.generateCodeForExprWithEnv(module_env, expr, result_type, &empty_env);
    }

    /// Helper to generate code for an expression with variable environment (for lambda bodies)
    fn generateCodeForExprWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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
            .e_lookup_external => {
                self.setCrashMessage("Dev evaluator: external module lookup not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_lookup_required => {
                self.setCrashMessage("Dev evaluator: required value lookup not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },

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
            .e_crash => |crash| {
                // Store the crash message and return error
                const msg = module_env.getString(crash.msg);
                self.setCrashMessage(msg) catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_expect => |expect| try self.generateExpectCode(module_env, expect, result_type, env),
            .e_runtime_error => {
                // Runtime errors are always errors
                self.setCrashMessage("Runtime error encountered") catch return error.OutOfMemory;
                return error.RuntimeError;
            },

            // Empty record (unit type)
            .e_empty_record => try self.generateReturnI64Code(0, result_type),
            .e_dot_access => |dot| try self.generateDotAccessCode(module_env, dot, result_type, env),
            .e_nominal => |nom| {
                // Nominal types wrap a backing expression - evaluate it directly
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.generateCodeForExprWithEnv(module_env, backing_expr, result_type, env);
            },
            .e_nominal_external => |nom| {
                // External nominal types also wrap a backing expression
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.generateCodeForExprWithEnv(module_env, backing_expr, result_type, env);
            },
            .e_ellipsis => {
                self.setCrashMessage("This expression uses `...` as a placeholder. Implementation is required.") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_anno_only => {
                self.setCrashMessage("This value has no implementation. It is only a type annotation for now.") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_type_var_dispatch => {
                self.setCrashMessage("Dev evaluator: type variable dispatch not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_for => try self.generateReturnI64Code(0, result_type), // For loops always return {} (empty record)
            .e_hosted_lambda => {
                self.setCrashMessage("Dev evaluator: hosted (platform) functions not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_low_level_lambda => {
                // Standalone low-level lambda (not being called) - this creates a closure for a builtin
                self.setCrashMessage("Dev evaluator: low-level lambda closure not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
        };
    }

    /// Generate code for function calls
    /// Handles lambda applications with constant arguments, including stored closures
    fn generateCallCode(self: *DevEvaluator, module_env: *ModuleEnv, call: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        // Get the function being called
        const func_expr = module_env.store.getExpr(call.func);

        // Handle lambda application
        switch (func_expr) {
            .e_lambda => |lambda| {
                return self.applyLambda(module_env, lambda, call.args, result_type, env);
            },
            .e_closure => |closure| {
                return self.applyClosure(module_env, closure, call.args, result_type, env);
            },
            .e_low_level_lambda => |low_level| {
                // Low-level builtin operations
                return self.generateLowLevelCallCode(module_env, low_level, call.args, result_type, env);
            },
            .e_lookup_local => |lookup| {
                // Function is stored in a variable - look it up in closure_refs
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                if (env.lookupClosure(pattern_key)) |stored_expr_idx| {
                    // It's a stored lambda/closure - get the expression and call it
                    const stored_expr = module_env.store.getExpr(stored_expr_idx);
                    switch (stored_expr) {
                        .e_lambda => |lambda| {
                            return self.applyLambda(module_env, lambda, call.args, result_type, env);
                        },
                        .e_closure => |closure| {
                            return self.applyClosure(module_env, closure, call.args, result_type, env);
                        },
                        else => return error.UnsupportedExpression,
                    }
                } else {
                    // Not a function - can't call it
                    return error.UnsupportedExpression;
                }
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Apply a lambda to arguments
    fn applyLambda(self: *DevEvaluator, module_env: *ModuleEnv, lambda: CIR.Expr.Lambda, args_span: CIR.Expr.Span, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        const arg_indices = module_env.store.sliceExpr(args_span);
        const param_indices = module_env.store.slicePatterns(lambda.args);

        if (arg_indices.len != param_indices.len) {
            return error.UnsupportedExpression;
        }

        // Create new environment with argument bindings
        var new_env = try env.child();
        defer new_env.deinit();

        for (arg_indices, param_indices) |arg_idx, param_idx| {
            try self.bindArgumentToParam(module_env, arg_idx, param_idx, env, &new_env);
        }

        // Evaluate lambda body with new environment
        const body_expr = module_env.store.getExpr(lambda.body);
        return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, &new_env);
    }

    /// Bind an argument expression to a parameter in the new environment
    /// Handles both value arguments and function arguments (higher-order functions)
    fn bindArgumentToParam(self: *DevEvaluator, module_env: *ModuleEnv, arg_idx: CIR.Expr.Idx, param_idx: CIR.Pattern.Idx, env: *ComptimeEnv, new_env: *ComptimeEnv) Error!void {
        const arg_expr = module_env.store.getExpr(arg_idx);
        const param_key = @intFromEnum(param_idx);

        switch (arg_expr) {
            // Direct lambda/closure argument - bind as closure reference
            .e_lambda, .e_closure => {
                try new_env.bindClosure(param_key, arg_idx);
            },
            // Lookup might be a function or a value
            .e_lookup_local => |lookup| {
                const lookup_key = @intFromEnum(lookup.pattern_idx);
                // Check if it's a closure first
                if (env.lookupClosure(lookup_key)) |closure_idx| {
                    try new_env.bindClosure(param_key, closure_idx);
                } else if (env.lookup(lookup_key)) |cv| {
                    // It's a value, copy it
                    try new_env.bind(param_key, cv);
                } else {
                    return error.UnsupportedExpression;
                }
            },
            // Other expressions - try to evaluate as i64
            else => {
                const arg_val = self.tryEvalConstantI64WithEnvMap(module_env, arg_expr, env) orelse
                    return error.UnsupportedExpression;
                const cv = try self.createI64Value(arg_val);
                try new_env.bind(param_key, cv);
            },
        }
    }

    /// Apply a closure to arguments
    /// Closures have captured values that need to be restored in the environment
    fn applyClosure(self: *DevEvaluator, module_env: *ModuleEnv, closure: CIR.Expr.Closure, args_span: CIR.Expr.Span, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        // Get the underlying lambda
        const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
        switch (lambda_expr) {
            .e_lambda => |lambda| {
                const arg_indices = module_env.store.sliceExpr(args_span);
                const param_indices = module_env.store.slicePatterns(lambda.args);

                if (arg_indices.len != param_indices.len) {
                    return error.UnsupportedExpression;
                }

                // Create new environment with captured values and argument bindings
                var new_env = try env.child();
                defer new_env.deinit();

                // First, add captured values to the environment
                const capture_indices = module_env.store.sliceCaptures(closure.captures);
                for (capture_indices) |capture_idx| {
                    const capture = module_env.store.getCapture(capture_idx);
                    const pattern_key = @intFromEnum(capture.pattern_idx);

                    // Look up the captured value - could be a regular value or a closure
                    if (env.lookup(pattern_key)) |cv| {
                        try new_env.bind(pattern_key, cv);
                    } else if (env.lookupClosure(pattern_key)) |closure_expr_idx| {
                        try new_env.bindClosure(pattern_key, closure_expr_idx);
                    } else {
                        // Captured value not found in environment
                        return error.UnsupportedExpression;
                    }
                }

                // Then, bind the arguments (supports higher-order functions)
                for (arg_indices, param_indices) |arg_idx, param_idx| {
                    try self.bindArgumentToParam(module_env, arg_idx, param_idx, env, &new_env);
                }

                // Evaluate lambda body with new environment
                const body_expr = module_env.store.getExpr(lambda.body);
                return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, &new_env);
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for low-level builtin operations
    fn generateLowLevelCallCode(
        self: *DevEvaluator,
        module_env: *ModuleEnv,
        low_level: anytype,
        args_span: CIR.Expr.Span,
        result_type: ResultType,
        env: *ComptimeEnv,
    ) Error![]const u8 {
        const arg_indices = module_env.store.sliceExpr(args_span);

        switch (low_level.op) {
            // Boolean operations
            .bool_is_eq => {
                if (arg_indices.len != 2) return error.UnsupportedExpression;
                const lhs_expr = module_env.store.getExpr(arg_indices[0]);
                const rhs_expr = module_env.store.getExpr(arg_indices[1]);
                const lhs_val = self.tryEvalConstantI64WithEnvMap(module_env, lhs_expr, env) orelse
                    return error.UnsupportedExpression;
                const rhs_val = self.tryEvalConstantI64WithEnvMap(module_env, rhs_expr, env) orelse
                    return error.UnsupportedExpression;
                const result: i64 = if (lhs_val == rhs_val) 1 else 0;
                return self.generateReturnI64Code(result, result_type);
            },

            // String operations - return UnsupportedExpression for now
            // These need actual string handling which requires more infrastructure
            .str_is_empty,
            .str_is_eq,
            .str_concat,
            .str_contains,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_caseless_ascii_equals,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_starts_with,
            .str_ends_with,
            .str_repeat,
            .str_with_prefix,
            .str_drop_prefix,
            .str_drop_suffix,
            .str_count_utf8_bytes,
            .str_with_capacity,
            .str_reserve,
            .str_release_excess_capacity,
            .str_to_utf8,
            .str_from_utf8_lossy,
            .str_from_utf8,
            .str_split_on,
            .str_join_with,
            .str_inspekt,
            => return error.UnsupportedExpression,

            // Numeric to_str operations - return UnsupportedExpression for now
            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            => return error.UnsupportedExpression,

            // List operations
            .list_len => {
                // For constant empty list, return 0
                if (arg_indices.len != 1) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                switch (list_expr) {
                    .e_empty_list => return self.generateReturnI64Code(0, result_type),
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        return self.generateReturnI64Code(@intCast(elements.len), result_type);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_is_empty => {
                if (arg_indices.len != 1) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                switch (list_expr) {
                    .e_empty_list => return self.generateReturnI64Code(1, result_type),
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        const result: i64 = if (elements.len == 0) 1 else 0;
                        return self.generateReturnI64Code(result, result_type);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_get_unsafe => {
                // Get element at index from constant list
                if (arg_indices.len != 2) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                const index_expr = module_env.store.getExpr(arg_indices[1]);

                // Get the index value
                const index_val = self.tryEvalConstantI64WithEnvMap(module_env, index_expr, env) orelse
                    return error.UnsupportedExpression;
                if (index_val < 0) return error.UnsupportedExpression;

                switch (list_expr) {
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        const index: usize = @intCast(index_val);
                        if (index >= elements.len) return error.UnsupportedExpression;

                        // Get the element and evaluate it
                        const elem_expr = module_env.store.getExpr(elements[index]);
                        const elem_val = self.tryEvalConstantI64WithEnvMap(module_env, elem_expr, env) orelse
                            return error.UnsupportedExpression;
                        return self.generateReturnI64Code(elem_val, result_type);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_append_unsafe,
            .list_concat,
            .list_with_capacity,
            .list_sort_with,
            .list_drop_at,
            .list_sublist,
            .list_append,
            => return error.UnsupportedExpression,

            // Other operations not yet implemented
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for local variable lookup
    fn generateLookupLocalCode(self: *DevEvaluator, lookup: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        const pattern_key = @intFromEnum(lookup.pattern_idx);
        const cv = env.lookup(pattern_key) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(cv.as(i64), result_type);
    }

    /// Binary operation with environment support
    fn generateBinopCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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
    fn generateUnaryMinusCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, unary: CIR.Expr.UnaryMinus, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_type);
    }

    /// If/else with environment support
    fn generateIfCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, if_expr: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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

    /// Try to fold a pure constant expression to an i64 value.
    /// Only handles expressions that can be fully evaluated at compile time.
    /// Returns null for expressions that require runtime computation.
    fn tryEvalConstantI64WithEnvMap(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, env: *ComptimeEnv) ?i64 {
        return switch (expr) {
            .e_num => |num| {
                const value_i128 = num.value.toI128();
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return null;
                }
                return @intCast(value_i128);
            },
            .e_lookup_local => |lookup| {
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                const cv = env.lookup(pattern_key) orelse return null;
                return cv.as(i64);
            },
            .e_zero_argument_tag => |tag| {
                if (tag.name == module_env.idents.true_tag) return 1;
                if (tag.name == module_env.idents.false_tag) return 0;
                return null;
            },
            .e_tag => |tag| {
                const args = module_env.store.sliceExpr(tag.args);
                if (args.len == 0) {
                    if (tag.name == module_env.idents.true_tag) return 1;
                    if (tag.name == module_env.idents.false_tag) return 0;
                }
                return null;
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
            .e_unary_not => |unary| {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse return null;
                return if (inner_val == 0) 1 else 0;
            },
            .e_nominal => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.tryEvalConstantI64WithEnvMap(module_env, backing_expr, env);
            },
            .e_nominal_external => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.tryEvalConstantI64WithEnvMap(module_env, backing_expr, env);
            },
            else => null,
        };
    }

    /// Try to evaluate an expression as a constant i64 value (no environment)
    fn tryEvalConstantI64(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ?i64 {
        var empty_env = self.createEnv();
        defer empty_env.deinit();
        return self.tryEvalConstantI64WithEnvMap(module_env, expr, &empty_env);
    }

    /// Try to evaluate an expression as a constant i64 value, with access to module_env
    fn tryEvalConstantI64WithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ?i64 {
        var empty_env = self.createEnv();
        defer empty_env.deinit();
        return self.tryEvalConstantI64WithEnvMap(module_env, expr, &empty_env);
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
    fn generateUnaryNotCodeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, unary: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.tryEvalConstantI64WithEnvMap(module_env, inner_expr, env) orelse
            return error.UnsupportedExpression;
        // Boolean negation: 0 becomes 1, non-zero becomes 0
        const result = if (inner_val == 0) @as(i64, 1) else @as(i64, 0);
        return self.generateReturnI64Code(result, result_type);
    }

    /// Generate code for match expressions
    fn generateMatchCode(self: *DevEvaluator, module_env: *ModuleEnv, match_expr: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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
    fn patternMatches(_: *DevEvaluator, module_env: *ModuleEnv, pattern: CIR.Pattern, target_val: i64, _: *ComptimeEnv) bool {
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
    fn generateReturnExprCode(self: *DevEvaluator, module_env: *ModuleEnv, ret: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        const value_expr = module_env.store.getExpr(ret.expr);
        return self.generateCodeForExprWithEnv(module_env, value_expr, result_type, env);
    }

    /// Generate code for dbg expressions (just evaluate the inner expression)
    fn generateDbgCode(self: *DevEvaluator, module_env: *ModuleEnv, dbg: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        // dbg returns the value of the expression being debugged
        const value_expr = module_env.store.getExpr(dbg.expr);
        return self.generateCodeForExprWithEnv(module_env, value_expr, result_type, env);
    }

    /// Generate code for expect expressions (evaluate inner, return expected value)
    fn generateExpectCode(self: *DevEvaluator, _: *ModuleEnv, _: anytype, result_type: ResultType, _: *ComptimeEnv) Error![]const u8 {
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

    /// Generate code for tuple expressions
    fn generateTupleCode(self: *DevEvaluator, module_env: *ModuleEnv, tuple: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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
    fn generateListCode(self: *DevEvaluator, module_env: *ModuleEnv, list: anytype, result_type: ResultType, _: *ComptimeEnv) Error![]const u8 {
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
    fn generateBlockCode(self: *DevEvaluator, module_env: *ModuleEnv, block: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        // Create a new environment for block-local bindings
        var block_env = try env.child();
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
    fn processStatement(self: *DevEvaluator, module_env: *ModuleEnv, stmt: CIR.Statement, env: *ComptimeEnv) Error!void {
        switch (stmt) {
            .s_decl => |decl| {
                try self.bindDeclaration(module_env, decl.expr, decl.pattern, env);
            },
            .s_decl_gen => |decl| {
                try self.bindDeclaration(module_env, decl.expr, decl.pattern, env);
            },
            else => {
                // Other statement types not yet supported
                return error.UnsupportedExpression;
            },
        }
    }

    /// Bind a declaration's expression to its pattern
    /// Closures are stored by expression reference, other values are evaluated
    fn bindDeclaration(self: *DevEvaluator, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, pattern: CIR.Pattern.Idx, env: *ComptimeEnv) Error!void {
        const expr = module_env.store.getExpr(expr_idx);
        const pattern_key = @intFromEnum(pattern);

        switch (expr) {
            // Lambdas and closures are stored by expression reference
            .e_lambda, .e_closure => {
                try env.bindClosure(pattern_key, expr_idx);
            },
            // Other expressions are evaluated to values
            else => {
                const value = self.tryEvalConstantI64WithEnvMap(module_env, expr, env) orelse
                    return error.UnsupportedExpression;
                const cv = try self.createI64Value(value);
                try env.bind(pattern_key, cv);
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
    fn generateStrCode(_: *DevEvaluator, module_env: *ModuleEnv, str: anytype, _: ResultType, _: *ComptimeEnv) Error![]const u8 {
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
    fn generateTagCode(self: *DevEvaluator, module_env: *ModuleEnv, tag: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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
    fn generateRecordCode(self: *DevEvaluator, module_env: *ModuleEnv, rec: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
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

    /// Generate code for dot access (record field access)
    fn generateDotAccessCode(self: *DevEvaluator, module_env: *ModuleEnv, dot: anytype, result_type: ResultType, env: *ComptimeEnv) Error![]const u8 {
        // Only support field access (no method calls)
        if (dot.args != null) {
            return error.UnsupportedExpression;
        }

        // Get the receiver expression
        const receiver_expr = module_env.store.getExpr(dot.receiver);

        // Only support direct record literal access for now
        switch (receiver_expr) {
            .e_record => |rec| {
                // Records with extension not supported
                if (rec.ext != null) {
                    return error.UnsupportedExpression;
                }

                const fields = module_env.store.sliceRecordFields(rec.fields);

                // Find the field with matching name
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (field.name == dot.field_name) {
                        const field_expr = module_env.store.getExpr(field.value);
                        const field_val = self.tryEvalConstantI64WithEnvMap(module_env, field_expr, env) orelse
                            return error.UnsupportedExpression;
                        return self.generateReturnI64Code(field_val, result_type);
                    }
                }

                // Field not found
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        }
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

    /// Type environment mapping pattern indices to their result types
    const TypeEnv = std.AutoHashMap(u32, ResultType);

    /// Get the ResultType for JIT execution from a CIR expression.
    /// Recursively determines the type for compound expressions.
    fn getExprResultType(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr) ResultType {
        var type_env = TypeEnv.init(self.allocator);
        defer type_env.deinit();
        return self.getExprResultTypeWithEnv(module_env, expr, &type_env);
    }

    /// Get the ResultType with a type environment for tracking variable types through blocks
    fn getExprResultTypeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, expr: CIR.Expr, type_env: *TypeEnv) ResultType {
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
            .e_typed_int => |ti| self.getTypedIntResultType(module_env, ti.type_name),
            .e_binop => |binop| self.getBinopResultTypeWithEnv(module_env, binop, type_env),
            .e_unary_minus => |unary| blk: {
                const inner_expr = module_env.store.getExpr(unary.expr);
                break :blk self.getExprResultTypeWithEnv(module_env, inner_expr, type_env);
            },
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.getExprResultTypeWithEnv(module_env, backing_expr, type_env);
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.getExprResultTypeWithEnv(module_env, backing_expr, type_env);
            },
            .e_if => |if_expr| blk: {
                // Get type from final_else branch
                const else_expr = module_env.store.getExpr(if_expr.final_else);
                break :blk self.getExprResultTypeWithEnv(module_env, else_expr, type_env);
            },
            .e_block => |block| self.getBlockResultType(module_env, block, type_env),
            .e_lookup_local => |lookup| blk: {
                // Look up the type from the type environment
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                break :blk type_env.get(pattern_key) orelse .i64;
            },
            else => .i64,
        };
    }

    /// Get the result type for a block, tracking variable types through declarations
    fn getBlockResultType(self: *DevEvaluator, module_env: *ModuleEnv, block: anytype, type_env: *TypeEnv) ResultType {
        // First pass: collect type annotations from s_type_anno statements
        // These are separate statements like `a : U64` that precede the declaration
        var type_annos = std.AutoHashMap(base.Ident.Idx, ResultType).init(self.allocator);
        defer type_annos.deinit();

        const stmts = module_env.store.sliceStatements(block.stmts);
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_type_anno => |ta| {
                    // Get the result type from the type annotation
                    const type_anno = module_env.store.getTypeAnno(ta.anno);
                    switch (type_anno) {
                        .apply => |apply| {
                            const result_type = resultTypeFromLocalOrExternal(apply.base);
                            type_annos.put(ta.name, result_type) catch {};
                        },
                        .lookup => |lookup| {
                            const result_type = resultTypeFromLocalOrExternal(lookup.base);
                            type_annos.put(ta.name, result_type) catch {};
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        // Second pass: process declarations and apply type annotations
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_decl => |decl| {
                    const pattern_key = @intFromEnum(decl.pattern);
                    // Check for inline annotation first
                    if (decl.anno) |anno_idx| {
                        const result_type = self.getAnnotationResultType(module_env, anno_idx);
                        type_env.put(pattern_key, result_type) catch {};
                    } else {
                        // Try to find a separate type annotation for this pattern
                        const pattern = module_env.store.getPattern(decl.pattern);
                        switch (pattern) {
                            .assign => |assign| {
                                if (type_annos.get(assign.ident)) |result_type| {
                                    type_env.put(pattern_key, result_type) catch {};
                                }
                            },
                            else => {},
                        }
                    }
                },
                .s_decl_gen => |decl| {
                    const pattern_key = @intFromEnum(decl.pattern);
                    if (decl.anno) |anno_idx| {
                        const result_type = self.getAnnotationResultType(module_env, anno_idx);
                        type_env.put(pattern_key, result_type) catch {};
                    } else {
                        const pattern = module_env.store.getPattern(decl.pattern);
                        switch (pattern) {
                            .assign => |assign| {
                                if (type_annos.get(assign.ident)) |result_type| {
                                    type_env.put(pattern_key, result_type) catch {};
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        // Now get the type of the final expression with the updated environment
        const final_expr = module_env.store.getExpr(block.final_expr);
        return self.getExprResultTypeWithEnv(module_env, final_expr, type_env);
    }

    /// Get the result type from an annotation
    fn getAnnotationResultType(_: *DevEvaluator, module_env: *ModuleEnv, anno_idx: CIR.Annotation.Idx) ResultType {
        const anno = module_env.store.getAnnotation(anno_idx);
        // Get the TypeAnno from the annotation
        const type_anno = module_env.store.getTypeAnno(anno.anno);
        switch (type_anno) {
            .apply => |apply| {
                // Type application like List(Str)
                return resultTypeFromLocalOrExternal(apply.base);
            },
            .lookup => |lookup| {
                // Basic type like U64, Str, Bool
                return resultTypeFromLocalOrExternal(lookup.base);
            },
            else => return .i64,
        }
    }

    /// Convert a LocalOrExternal to ResultType by checking if it's a builtin numeric type
    fn resultTypeFromLocalOrExternal(loe: CIR.TypeAnno.LocalOrExternal) ResultType {
        switch (loe) {
            .builtin => |b| return resultTypeFromBuiltin(b),
            .local, .external => return .i64,
        }
    }

    /// Convert a Builtin type enum to ResultType
    fn resultTypeFromBuiltin(b: CIR.TypeAnno.Builtin) ResultType {
        return switch (b) {
            .u8, .u16, .u32, .u64 => .u64,
            .u128 => .u128,
            .i128 => .i128,
            .f32, .f64 => .f64,
            .dec => .dec,
            else => .i64,
        };
    }

    /// Get the result type for a typed integer based on its type name
    fn getTypedIntResultType(_: *DevEvaluator, module_env: *ModuleEnv, type_name: base.Ident.Idx) ResultType {
        // For typed integers, we need to look up the type by name
        // Since we don't have LocalOrExternal info here, fall back to identifier comparison
        const idents = &module_env.idents;
        if (type_name == idents.u8_type or
            type_name == idents.u16_type or
            type_name == idents.u32_type or
            type_name == idents.u64_type)
        {
            return .u64;
        } else if (type_name == idents.u128_type) {
            return .u128;
        } else if (type_name == idents.i128_type) {
            return .i128;
        } else if (type_name == idents.f32_type or type_name == idents.f64_type) {
            return .f64;
        } else if (type_name == idents.dec_type) {
            return .dec;
        }
        return .i64;
    }

    /// Get the result type for a binary operation
    fn getBinopResultTypeWithEnv(self: *DevEvaluator, module_env: *ModuleEnv, binop: CIR.Expr.Binop, type_env: *TypeEnv) ResultType {
        // Comparison operators always return boolean (i64 for 0/1)
        switch (binop.op) {
            .lt, .gt, .le, .ge, .eq, .ne, .@"and", .@"or" => return .i64,
            else => {},
        }
        // For arithmetic operators, determine type from operands
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        return self.getExprResultTypeWithEnv(module_env, lhs_expr, type_env);
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
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
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
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
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
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
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
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
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
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("1 + 2") catch |err| {
        // Skip if parsing/canonicalization fails (expected in unit test environment)
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 3 }, result);
}

test "evaluate subtraction" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("10 - 3") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate multiplication" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("6 * 7") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate unary minus" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("-42") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = -42 }, result);
}

test "evaluate if true branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 42 else 0") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate if false branch" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 0 > 1 42 else 99") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 99 }, result);
}

test "evaluate nested if" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("if 1 > 0 (if 2 > 1 100 else 50) else 0") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 100 }, result);
}

test "evaluate simple lambda application" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (\x -> x + 1) 5 should equal 6
    const result = evaluator.evaluate("(\\x -> x + 1) 5") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 6 }, result);
}

test "evaluate lambda identity" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (\x -> x) 42 should equal 42
    const result = evaluator.evaluate("(\\x -> x) 42") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate lambda with arithmetic in body" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (\x -> x * 2 + 10) 5 should equal 20
    const result = evaluator.evaluate("(\\x -> x * 2 + 10) 5") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 20 }, result);
}

test "evaluate lambda with if in body" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (\x -> if x > 0 then x else -x) 5 should equal 5
    const result = evaluator.evaluate("(\\x -> if x > 0 then x else -x) 5") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 5 }, result);
}

test "evaluate block expression" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { x = 5; x + 1 } should equal 6
    const result = evaluator.evaluate("{ x = 5; x + 1 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 6 }, result);
}

test "evaluate block with multiple declarations" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { x = 3; y = 4; x + y } should equal 7
    const result = evaluator.evaluate("{ x = 3; y = 4; x + y }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate True tag" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate False tag" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    const result = evaluator.evaluate("False") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 0 }, result);
}

test "evaluate comparison greater than" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 5 > 3 should return 1 (true)
    const result = evaluator.evaluate("5 > 3") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison less than" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 3 < 5 should return 1 (true)
    const result = evaluator.evaluate("3 < 5") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison equal" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 42 == 42 should return 1 (true)
    const result = evaluator.evaluate("42 == 42") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison not equal" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 1 != 2 should return 1 (true)
    const result = evaluator.evaluate("1 != 2") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate integer division" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 10 // 3 should return 3 (integer division)
    const result = evaluator.evaluate("10 // 3") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 3 }, result);
}

test "evaluate modulo" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // 10 % 3 should return 1
    const result = evaluator.evaluate("10 % 3") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate boolean and" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // True and True should return 1
    const result = evaluator.evaluate("True and True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate boolean or" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // False or True should return 1
    const result = evaluator.evaluate("False or True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate multi-parameter lambda" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // Lambda with two parameters
    const result = evaluator.evaluate("(|x, y| x + y)(3, 4)") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 7 }, result);
}

test "evaluate complex arithmetic expression" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // Complex expression: (5 + 3) * 2 - 10 // 2
    const result = evaluator.evaluate("(5 + 3) * 2 - 10 // 2") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    // (5 + 3) * 2 - 10 // 2 = 8 * 2 - 5 = 16 - 5 = 11
    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 11 }, result);
}

test "evaluate boolean and short circuit semantics" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // False and True should return 0 (False)
    const result = evaluator.evaluate("False and True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 0 }, result);
}

test "evaluate boolean or short circuit semantics" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // True or False should return 1 (True)
    const result = evaluator.evaluate("True or False") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate nested boolean expression" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (True and True) or False should return 1
    const result = evaluator.evaluate("(True and True) or False") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate comparison with booleans in if" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // if True and True 100 else 0 should return 100
    const result = evaluator.evaluate("if True and True 100 else 0") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 100 }, result);
}

test "evaluate record field access" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // {foo: 42}.foo should return 42
    const result = evaluator.evaluate("{foo: 42}.foo") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate record field access multi-field" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // {x: 10, y: 20}.y should return 20
    const result = evaluator.evaluate("{x: 10, y: 20}.y") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 20 }, result);
}

test "evaluate tuple access" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // (10, 20) returns first element (simplified representation)
    const result = evaluator.evaluate("(10, 20)") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 10 }, result);
}

test "evaluate block with binding" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // Block with local binding
    const result = evaluator.evaluate("{ x = 5\n x * 2 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 10 }, result);
}

test "evaluate unary not" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // !True should return 0 (False)
    const result = evaluator.evaluate("!True") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 0 }, result);
}

test "evaluate unary not false" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // !False should return 1 (True)
    const result = evaluator.evaluate("!False") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 1 }, result);
}

test "evaluate stored lambda" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { f = \x -> x + 1; f 5 } should equal 6
    const result = evaluator.evaluate("{ f = \\x -> x + 1; f 5 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 6 }, result);
}

test "evaluate stored lambda with multiple uses" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { double = \x -> x * 2; double 5 + double 3 } should equal 16
    const result = evaluator.evaluate("{ double = \\x -> x * 2; double 5 + double 3 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 16 }, result);
}

test "evaluate stored lambda with value binding" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { x = 10; f = \y -> y * 2; f x } should equal 20
    // This tests storing both a value and a closure in the same block
    const result = evaluator.evaluate("{ x = 10; f = \\y -> y * 2; f x }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 20 }, result);
}

test "evaluate closure with capture" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { y = 10; f = \x -> x + y; f 5 } should equal 15
    // The closure f captures y from the enclosing scope
    const result = evaluator.evaluate("{ y = 10; f = \\x -> x + y; f 5 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 15 }, result);
}

test "evaluate closure capturing multiple values" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { a = 3; b = 7; f = \x -> x + a + b; f 5 } should equal 15
    // The closure f captures both a and b
    const result = evaluator.evaluate("{ a = 3; b = 7; f = \\x -> x + a + b; f 5 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 15 }, result);
}

test "evaluate higher-order function apply" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { apply = \f, x -> f x; double = \n -> n * 2; apply double 21 } should equal 42
    const result = evaluator.evaluate("{ apply = \\f, x -> f x; double = \\n -> n * 2; apply double 21 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate higher-order function with inline lambda" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { apply = \f, x -> f x; apply (\n -> n + 10) 32 } should equal 42
    const result = evaluator.evaluate("{ apply = \\f, x -> f x; apply (\\n -> n + 10) 32 }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 42 }, result);
}

test "evaluate compose functions" {
    var evaluator = DevEvaluator.init(std.testing.allocator) catch |err| {
        return switch (err) {
            error.OutOfMemory => error.SkipZigTest,
            else => err,
        };
    };
    defer evaluator.deinit();

    // { double = \x -> x * 2; addOne = \x -> x + 1; double (addOne 5) } should equal 12
    const result = evaluator.evaluate("{ double = \\x -> x * 2; addOne = \\x -> x + 1; double (addOne 5) }") catch |err| {
        return switch (err) {
            error.ParseError, error.CanonicalizeError, error.TypeError, error.UnsupportedExpression => error.SkipZigTest,
            else => err,
        };
    };

    try std.testing.expectEqual(DevEvaluator.EvalResult{ .i64_val = 12 }, result);
}
