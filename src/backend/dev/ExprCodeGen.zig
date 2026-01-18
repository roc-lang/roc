//! Expression Code Generator for the Dev Backend
//!
//! This module generates native machine code (x86_64/aarch64) directly from CIR expressions.
//! It is used by the dev backend JIT evaluator for the REPL and tests.
//!
//! Architecture:
//! - ExprCodeGen holds state during code generation (allocator, crash messages, etc.)
//! - Scope tracks variable bindings for block expressions and closures
//! - BindingValue represents different types of bound values
//!
//! All generated code uses a result-pointer calling convention:
//! - First argument (rdi/x0) is a pointer to write the result
//! - Functions write their result to this pointer and return

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const types = @import("types");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// Binding value that can hold different types during code generation
pub const BindingValue = union(enum) {
    i64_val: i64,
    i128_val: i128,
    f64_val: f64,
    expr_ref: CIR.Expr.Idx, // For deferred evaluation (expressions not yet evaluated)

    pub fn asI64(self: BindingValue) ?i64 {
        return switch (self) {
            .i64_val => |v| v,
            .i128_val => |v| if (v >= std.math.minInt(i64) and v <= std.math.maxInt(i64)) @as(i64, @intCast(v)) else null,
            else => null,
        };
    }

    pub fn asI128(self: BindingValue) ?i128 {
        return switch (self) {
            .i64_val => |v| @as(i128, v),
            .i128_val => |v| v,
            else => null,
        };
    }

    pub fn asF64(self: BindingValue) ?f64 {
        return switch (self) {
            .f64_val => |v| v,
            else => null,
        };
    }
};

/// Scope for tracking variable bindings during code generation.
/// Uses parent pointer for O(1) child creation instead of copying all bindings.
pub const Scope = struct {
    parent: ?*const Scope,
    bindings: std.AutoHashMap(u32, BindingValue),
    closure_refs: std.AutoHashMap(u32, CIR.Expr.Idx),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Scope {
        return .{
            .parent = null,
            .bindings = std.AutoHashMap(u32, BindingValue).init(allocator),
            .closure_refs = std.AutoHashMap(u32, CIR.Expr.Idx).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn child(self: *const Scope) Scope {
        return .{
            .parent = self,
            .bindings = std.AutoHashMap(u32, BindingValue).init(self.allocator),
            .closure_refs = std.AutoHashMap(u32, CIR.Expr.Idx).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    pub fn bind(self: *Scope, pattern_idx: u32, value: i64) !void {
        try self.bindings.put(pattern_idx, BindingValue{ .i64_val = value });
    }

    pub fn bindI128(self: *Scope, pattern_idx: u32, value: i128) !void {
        try self.bindings.put(pattern_idx, BindingValue{ .i128_val = value });
    }

    pub fn bindF64(self: *Scope, pattern_idx: u32, value: f64) !void {
        try self.bindings.put(pattern_idx, BindingValue{ .f64_val = value });
    }

    pub fn bindExpr(self: *Scope, pattern_idx: u32, expr_idx: CIR.Expr.Idx) !void {
        try self.bindings.put(pattern_idx, BindingValue{ .expr_ref = expr_idx });
    }

    pub fn bindClosure(self: *Scope, pattern_idx: u32, expr_idx: CIR.Expr.Idx) !void {
        try self.closure_refs.put(pattern_idx, expr_idx);
    }

    pub fn lookup(self: *const Scope, pattern_idx: u32) ?i64 {
        if (self.bindings.get(pattern_idx)) |v| return v.asI64();
        if (self.parent) |p| return p.lookup(pattern_idx);
        return null;
    }

    pub fn lookupI128(self: *const Scope, pattern_idx: u32) ?i128 {
        if (self.bindings.get(pattern_idx)) |v| return v.asI128();
        if (self.parent) |p| return p.lookupI128(pattern_idx);
        return null;
    }

    pub fn lookupF64(self: *const Scope, pattern_idx: u32) ?f64 {
        if (self.bindings.get(pattern_idx)) |v| return v.asF64();
        if (self.parent) |p| return p.lookupF64(pattern_idx);
        return null;
    }

    pub fn lookupBinding(self: *const Scope, pattern_idx: u32) ?BindingValue {
        if (self.bindings.get(pattern_idx)) |v| return v;
        if (self.parent) |p| return p.lookupBinding(pattern_idx);
        return null;
    }

    pub fn lookupClosure(self: *const Scope, pattern_idx: u32) ?CIR.Expr.Idx {
        if (self.closure_refs.get(pattern_idx)) |v| return v;
        if (self.parent) |p| return p.lookupClosure(pattern_idx);
        return null;
    }

    pub fn deinit(self: *Scope) void {
        self.bindings.deinit();
        self.closure_refs.deinit();
    }
};

/// Layout index for result type
pub const LayoutIdx = layout.Idx;

/// Expression code generator for the dev backend
pub const ExprCodeGen = struct {
    allocator: Allocator,

    /// Crash message from e_crash expression (if any)
    crash_message: ?[]const u8 = null,

    pub const Error = error{
        OutOfMemory,
        UnsupportedType,
        UnsupportedExpression,
        Crash,
        RuntimeError,
    };

    /// Initialize a new ExprCodeGen
    pub fn init(allocator: Allocator) ExprCodeGen {
        return ExprCodeGen{
            .allocator = allocator,
        };
    }

    /// Clean up
    pub fn deinit(self: *ExprCodeGen) void {
        if (self.crash_message) |msg| {
            self.allocator.free(msg);
        }
    }

    /// Set the crash message (for e_crash and related errors)
    pub fn setCrashMessage(self: *ExprCodeGen, message: []const u8) !void {
        if (self.crash_message) |old_msg| {
            self.allocator.free(old_msg);
        }
        self.crash_message = try self.allocator.dupe(u8, message);
    }

    /// Get the crash message (if any)
    pub fn getCrashMessage(self: *const ExprCodeGen) ?[]const u8 {
        return self.crash_message;
    }

    /// Clear the crash message
    pub fn clearCrashMessage(self: *ExprCodeGen) void {
        if (self.crash_message) |msg| {
            self.allocator.free(msg);
            self.crash_message = null;
        }
    }

    /// Create an empty Scope for code generation
    pub fn createScope(self: *ExprCodeGen) Scope {
        return Scope.init(self.allocator);
    }

    /// Generate code for an expression with variable environment
    pub fn generateCodeForExpr(self: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        return switch (expr) {
            // Numeric literals
            .e_num => |num| try self.generateNumericCode(num, result_layout),
            .e_frac_f64 => |frac| try self.generateFloatCode(frac.value),
            .e_frac_f32 => |frac| try self.generateFloatCode(@floatCast(frac.value)),
            .e_dec => |dec| try self.generateDecCode(dec, result_layout),
            .e_dec_small => |dec| try self.generateDecSmallCode(dec, result_layout),
            .e_typed_int => |ti| try self.generateTypedIntCode(ti, result_layout),
            .e_typed_frac => |tf| try self.generateTypedFracCode(tf, result_layout),

            // Operations
            .e_binop => |binop| try self.generateBinopCode(module_env, binop, result_layout, env),
            .e_unary_minus => |unary| try self.generateUnaryMinusCode(module_env, unary, result_layout, env),
            .e_unary_not => |unary| try self.generateUnaryNotCode(module_env, unary, result_layout, env),

            // Control flow
            .e_if => |if_expr| try self.generateIfCode(module_env, if_expr, result_layout, env),
            .e_match => |match_expr| try self.generateMatchCode(module_env, match_expr, result_layout, env),

            // Functions and calls
            .e_call => |call| try self.generateCallCode(module_env, call, result_layout, env),
            .e_lambda => return error.UnsupportedExpression, // Lambdas are handled via e_call
            .e_closure => return error.UnsupportedExpression, // Closures are handled via e_call

            // Lookups
            .e_lookup_local => |lookup| try self.generateLookupLocalCode(module_env, lookup, result_layout, env),
            .e_lookup_external => {
                self.setCrashMessage("Dev evaluator: external module lookup not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_lookup_required => {
                self.setCrashMessage("Dev evaluator: required value lookup not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },

            // Tags
            .e_zero_argument_tag => |tag| try self.generateZeroArgTagCode(module_env, tag, result_layout),
            .e_tag => |tag| try self.generateTagCode(module_env, tag, result_layout, env),

            // Data structures
            .e_empty_list => try self.generateEmptyListCode(result_layout),
            .e_list => |list| try self.generateListCode(module_env, list, result_layout, env),
            .e_tuple => |tuple| try self.generateTupleCode(module_env, tuple, result_layout, env),
            .e_record => |rec| try self.generateRecordCode(module_env, rec, result_layout, env),

            // Blocks and statements
            .e_block => |block| try self.generateBlockCode(module_env, block, result_layout, env),
            .e_return => |ret| try self.generateReturnExprCode(module_env, ret, result_layout, env),

            // Strings
            .e_str_segment => |seg| try self.generateStrSegmentCode(module_env, seg, result_layout),
            .e_str => |str| try self.generateStrCode(module_env, str, result_layout, env),

            // Debug and errors
            .e_dbg => |dbg| try self.generateDbgCode(module_env, dbg, result_layout, env),
            .e_crash => |crash| {
                const msg = module_env.getString(crash.msg);
                self.setCrashMessage(msg) catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_expect => |expect| try self.generateExpectCode(module_env, expect, result_layout, env),
            .e_runtime_error => {
                self.setCrashMessage("Runtime error encountered") catch return error.OutOfMemory;
                return error.RuntimeError;
            },

            // Empty record (unit type)
            .e_empty_record => try self.generateReturnI64Code(0, result_layout),
            .e_dot_access => |dot| try self.generateDotAccessCode(module_env, dot, result_layout, env),
            .e_nominal => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.generateCodeForExpr(module_env, backing_expr, result_layout, env);
            },
            .e_nominal_external => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.generateCodeForExpr(module_env, backing_expr, result_layout, env);
            },
            .e_ellipsis => {
                self.setCrashMessage("This expression uses `...` as a placeholder. Implementation is required.") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_anno_only => {
                self.setCrashMessage("This value has no implementation. It is only a type annotation for now.") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_type_var_dispatch => |tvd| {
                // Resolve type variable dispatch by:
                // 1. Getting the type annotation from the alias statement
                // 2. Resolving it to a concrete type
                // 3. Looking up the method on that type
                // 4. Generating a call to the resolved method

                // Get the type var alias statement
                const alias_stmt = module_env.store.getStatement(tvd.type_var_alias_stmt);
                const alias_data = alias_stmt.s_type_var_alias;

                // Get the type variable and resolve it
                const type_var_anno = alias_data.type_var_anno;
                const ct_var = ModuleEnv.varFrom(type_var_anno);
                const resolved = module_env.types.resolveVar(ct_var);

                // Get the type identifier from the resolved type
                const type_ident: ?base.Ident.Idx = switch (resolved.desc.content) {
                    .structure => |flat| switch (flat) {
                        .nominal_type => |nom| nom.ident.ident_idx,
                        .record, .record_unbound => module_env.common.findIdent("Record"),
                        .tag_union, .empty_tag_union => module_env.common.findIdent("Tag"),
                        .tuple => module_env.common.findIdent("Tuple"),
                        .fn_pure, .fn_effectful, .fn_unbound => module_env.common.findIdent("Fn"),
                        .empty_record => module_env.common.findIdent("Record"),
                    },
                    .alias => |alias| alias.ident.ident_idx,
                    // Flex/rigid vars - check for from_numeral constraint and default to Dec
                    .flex => |flex| blk: {
                        if (!flex.constraints.isEmpty()) {
                            for (module_env.types.sliceStaticDispatchConstraints(flex.constraints)) |constraint| {
                                if (constraint.origin == .from_numeral) {
                                    break :blk module_env.common.findIdent("Dec");
                                }
                            }
                        }
                        break :blk null;
                    },
                    .rigid => |rigid| blk: {
                        if (!rigid.constraints.isEmpty()) {
                            for (module_env.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                                if (constraint.origin == .from_numeral) {
                                    break :blk module_env.common.findIdent("Dec");
                                }
                            }
                        }
                        break :blk null;
                    },
                    .err => null,
                };

                const resolved_type_ident = type_ident orelse {
                    self.setCrashMessage("Type variable dispatch: could not resolve type") catch return error.OutOfMemory;
                    return error.Crash;
                };

                // Look up the method implementation
                const qualified_method = module_env.lookupMethodIdent(
                    resolved_type_ident,
                    tvd.method_name,
                ) orelse {
                    self.setCrashMessage("Type variable dispatch: method not found on type") catch return error.OutOfMemory;
                    return error.Crash;
                };

                // Get the method's definition expression
                const node_idx = module_env.getExposedNodeIndexById(qualified_method) orelse {
                    self.setCrashMessage("Type variable dispatch: method definition not found") catch return error.OutOfMemory;
                    return error.Crash;
                };

                const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                const def = module_env.store.getDef(def_idx);
                const method_expr = module_env.store.getExpr(def.expr);

                // Generate a call to the method with the dispatch arguments
                switch (method_expr) {
                    .e_lambda => |lambda| {
                        return self.applyLambda(module_env, lambda, tvd.args, result_layout, env);
                    },
                    .e_closure => |closure| {
                        return self.applyClosure(module_env, closure, tvd.args, result_layout, env);
                    },
                    .e_low_level_lambda => |low_level| {
                        return self.generateLowLevelCallCode(module_env, low_level, tvd.args, result_layout, env);
                    },
                    else => {
                        self.setCrashMessage("Type variable dispatch: unsupported method expression type") catch return error.OutOfMemory;
                        return error.Crash;
                    },
                }
            },
            .e_for => |for_expr| {
                // Execute the for loop, then return unit
                try self.executeForLoop(module_env, for_expr.patt, for_expr.expr, for_expr.body, env);
                return self.generateReturnI64Code(0, result_layout); // For expressions return unit
            },
            .e_hosted_lambda => {
                self.setCrashMessage("Dev evaluator: hosted (platform) functions not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },
            .e_low_level_lambda => {
                self.setCrashMessage("Dev evaluator: low-level lambda closure not yet supported") catch return error.OutOfMemory;
                return error.Crash;
            },

            // RC expressions - no-ops in dev backend (acceptable memory leaks for REPL)
            .e_incref, .e_decref, .e_free => {
                return self.generateReturnI64Code(0, result_layout);
            },
        };
    }

    /// Generate code for function calls
    fn generateCallCode(self: *ExprCodeGen, module_env: *ModuleEnv, call: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const func_expr = module_env.store.getExpr(call.func);

        switch (func_expr) {
            .e_lambda => |lambda| {
                return self.applyLambda(module_env, lambda, call.args, result_layout, env);
            },
            .e_closure => |closure| {
                return self.applyClosure(module_env, closure, call.args, result_layout, env);
            },
            .e_low_level_lambda => |low_level| {
                return self.generateLowLevelCallCode(module_env, low_level, call.args, result_layout, env);
            },
            .e_lookup_local => |lookup| {
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                if (env.lookupClosure(pattern_key)) |stored_expr_idx| {
                    const stored_expr = module_env.store.getExpr(stored_expr_idx);
                    switch (stored_expr) {
                        .e_lambda => |lambda| {
                            return self.applyLambda(module_env, lambda, call.args, result_layout, env);
                        },
                        .e_closure => |closure| {
                            return self.applyClosure(module_env, closure, call.args, result_layout, env);
                        },
                        else => return error.UnsupportedExpression,
                    }
                } else {
                    // Try to look up the function by its identifier in definitions
                    const pattern = module_env.store.getPattern(lookup.pattern_idx);
                    switch (pattern) {
                        .assign => |assign| {
                            // Look up by identifier
                            if (module_env.getExposedNodeIndexById(assign.ident)) |node_idx| {
                                const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
                                const def = module_env.store.getDef(def_idx);
                                const func_def_expr = module_env.store.getExpr(def.expr);
                                switch (func_def_expr) {
                                    .e_lambda => |lambda| {
                                        return self.applyLambda(module_env, lambda, call.args, result_layout, env);
                                    },
                                    .e_closure => |closure| {
                                        return self.applyClosure(module_env, closure, call.args, result_layout, env);
                                    },
                                    .e_low_level_lambda => |low_level| {
                                        return self.generateLowLevelCallCode(module_env, low_level, call.args, result_layout, env);
                                    },
                                    else => return error.UnsupportedExpression,
                                }
                            } else {
                                return error.UnsupportedExpression;
                            }
                        },
                        else => return error.UnsupportedExpression,
                    }
                }
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Apply a lambda to arguments
    fn applyLambda(self: *ExprCodeGen, module_env: *ModuleEnv, lambda: CIR.Expr.Lambda, args_span: CIR.Expr.Span, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const arg_indices = module_env.store.sliceExpr(args_span);
        const param_indices = module_env.store.slicePatterns(lambda.args);

        if (arg_indices.len != param_indices.len) {
            return error.UnsupportedExpression;
        }

        var new_env = env.child();
        defer new_env.deinit();

        for (arg_indices, param_indices) |arg_idx, param_idx| {
            try self.bindArgumentToParam(module_env, arg_idx, param_idx, env, &new_env);
        }

        const body_expr = module_env.store.getExpr(lambda.body);
        return self.generateCodeForExpr(module_env, body_expr, result_layout, &new_env);
    }

    /// Bind an argument expression to a parameter in the new environment
    fn bindArgumentToParam(self: *ExprCodeGen, module_env: *ModuleEnv, arg_idx: CIR.Expr.Idx, param_idx: CIR.Pattern.Idx, env: *Scope, new_env: *Scope) Error!void {
        const arg_expr = module_env.store.getExpr(arg_idx);
        const param_key = @intFromEnum(param_idx);

        switch (arg_expr) {
            .e_lambda, .e_closure => {
                try new_env.bindClosure(param_key, arg_idx);
            },
            .e_lookup_local => |lookup| {
                const lookup_key = @intFromEnum(lookup.pattern_idx);
                if (env.lookupClosure(lookup_key)) |closure_idx| {
                    try new_env.bindClosure(param_key, closure_idx);
                } else if (env.lookup(lookup_key)) |cv| {
                    try new_env.bind(param_key, cv);
                } else {
                    return error.UnsupportedExpression;
                }
            },
            else => {
                if (self.evalConstantI64(module_env, arg_expr, env)) |arg_val| {
                    try new_env.bind(param_key, arg_val);
                    return;
                }
                if (self.evalConstantI128(module_env, arg_expr, env)) |arg_val| {
                    try new_env.bindI128(param_key, arg_val);
                    return;
                }
                try new_env.bindExpr(param_key, arg_idx);
            },
        }
    }

    /// Apply a closure to arguments
    fn applyClosure(self: *ExprCodeGen, module_env: *ModuleEnv, closure: CIR.Expr.Closure, args_span: CIR.Expr.Span, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
        switch (lambda_expr) {
            .e_lambda => |lambda| {
                const arg_indices = module_env.store.sliceExpr(args_span);
                const param_indices = module_env.store.slicePatterns(lambda.args);

                if (arg_indices.len != param_indices.len) {
                    return error.UnsupportedExpression;
                }

                var new_env = env.child();
                defer new_env.deinit();

                const capture_indices = module_env.store.sliceCaptures(closure.captures);
                for (capture_indices) |capture_idx| {
                    const capture = module_env.store.getCapture(capture_idx);
                    const pattern_key = @intFromEnum(capture.pattern_idx);

                    if (env.lookup(pattern_key)) |cv| {
                        try new_env.bind(pattern_key, cv);
                    } else if (env.lookupClosure(pattern_key)) |closure_expr_idx| {
                        try new_env.bindClosure(pattern_key, closure_expr_idx);
                    } else {
                        return error.UnsupportedExpression;
                    }
                }

                for (arg_indices, param_indices) |arg_idx, param_idx| {
                    try self.bindArgumentToParam(module_env, arg_idx, param_idx, env, &new_env);
                }

                const body_expr = module_env.store.getExpr(lambda.body);
                return self.generateCodeForExpr(module_env, body_expr, result_layout, &new_env);
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for low-level builtin operations
    fn generateLowLevelCallCode(
        self: *ExprCodeGen,
        module_env: *ModuleEnv,
        low_level: anytype,
        args_span: CIR.Expr.Span,
        result_layout: LayoutIdx,
        env: *Scope,
    ) Error![]const u8 {
        const arg_indices = module_env.store.sliceExpr(args_span);

        switch (low_level.op) {
            .bool_is_eq => {
                if (arg_indices.len != 2) return error.UnsupportedExpression;
                const lhs_expr = module_env.store.getExpr(arg_indices[0]);
                const rhs_expr = module_env.store.getExpr(arg_indices[1]);
                const lhs_val = self.evalConstantI64(module_env, lhs_expr, env) orelse
                    return error.UnsupportedExpression;
                const rhs_val = self.evalConstantI64(module_env, rhs_expr, env) orelse
                    return error.UnsupportedExpression;
                const result: i64 = if (lhs_val == rhs_val) 1 else 0;
                return self.generateReturnI64Code(result, result_layout);
            },

            // String operations - not yet supported
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

            // Numeric to_str operations - not yet supported
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
                if (arg_indices.len != 1) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                switch (list_expr) {
                    .e_empty_list => return self.generateReturnI64Code(0, result_layout),
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        return self.generateReturnI64Code(@intCast(elements.len), result_layout);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_is_empty => {
                if (arg_indices.len != 1) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                switch (list_expr) {
                    .e_empty_list => return self.generateReturnI64Code(1, result_layout),
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        const result: i64 = if (elements.len == 0) 1 else 0;
                        return self.generateReturnI64Code(result, result_layout);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_get_unsafe => {
                if (arg_indices.len != 2) return error.UnsupportedExpression;
                const list_expr = module_env.store.getExpr(arg_indices[0]);
                const index_expr = module_env.store.getExpr(arg_indices[1]);

                const index_val = self.evalConstantI64(module_env, index_expr, env) orelse
                    return error.UnsupportedExpression;
                if (index_val < 0) return error.UnsupportedExpression;

                switch (list_expr) {
                    .e_list => |list| {
                        const elements = module_env.store.sliceExpr(list.elems);
                        const index: usize = @intCast(index_val);
                        if (index >= elements.len) return error.UnsupportedExpression;

                        const elem_expr = module_env.store.getExpr(elements[index]);
                        const elem_val = self.evalConstantI64(module_env, elem_expr, env) orelse
                            return error.UnsupportedExpression;
                        return self.generateReturnI64Code(elem_val, result_layout);
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

            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for local variable lookup
    fn generateLookupLocalCode(self: *ExprCodeGen, module_env: *ModuleEnv, lookup: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const pattern_key = @intFromEnum(lookup.pattern_idx);

        if (env.lookupBinding(pattern_key)) |binding| {
            switch (binding) {
                .expr_ref => |expr_idx| {
                    const expr = module_env.store.getExpr(expr_idx);
                    return self.generateCodeForExpr(module_env, expr, result_layout, env);
                },
                .i64_val => |value| {
                    return self.generateReturnI64Code(value, result_layout);
                },
                .i128_val => |value| {
                    return self.generateReturnI128Code(value);
                },
                .f64_val => |value| {
                    return self.generateReturnF64Code(value);
                },
            }
        }

        return error.UnsupportedExpression;
    }

    /// Binary operation with environment support
    fn generateBinopCode(self: *ExprCodeGen, module_env: *ModuleEnv, binop: CIR.Expr.Binop, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const lhs_expr = module_env.store.getExpr(binop.lhs);
        const rhs_expr = module_env.store.getExpr(binop.rhs);

        // Special handling for record/tuple equality comparison
        if (binop.op == .eq or binop.op == .ne) {
            const are_equal = self.compareExprsForEquality(module_env, lhs_expr, rhs_expr, env);
            if (are_equal) |eq| {
                const result_val: i64 = if (binop.op == .eq)
                    (if (eq) @as(i64, 1) else @as(i64, 0))
                else
                    (if (eq) @as(i64, 0) else @as(i64, 1));
                return self.generateReturnI64Code(result_val, result_layout);
            }
        }

        // Check if this is a 128-bit operation
        if (result_layout == .i128 or result_layout == .u128 or result_layout == .dec) {
            const lhs_val = self.evalConstantI128(module_env, lhs_expr, env) orelse return error.UnsupportedExpression;
            const rhs_val = self.evalConstantI128(module_env, rhs_expr, env) orelse return error.UnsupportedExpression;

            const dec_scale: i128 = 1_000_000_000_000_000_000;

            const result_val: i128 = switch (binop.op) {
                .add => lhs_val +% rhs_val,
                .sub => lhs_val -% rhs_val,
                .mul => blk: {
                    if (result_layout == .dec) {
                        break :blk @divTrunc(lhs_val *% rhs_val, dec_scale);
                    } else {
                        break :blk lhs_val *% rhs_val;
                    }
                },
                .div => blk: {
                    if (rhs_val == 0) return error.UnsupportedExpression;
                    if (result_layout == .dec) {
                        break :blk @divTrunc(lhs_val *% dec_scale, rhs_val);
                    } else {
                        break :blk @divTrunc(lhs_val, rhs_val);
                    }
                },
                .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
                .div_trunc => blk: {
                    if (rhs_val == 0) return error.UnsupportedExpression;
                    if (result_layout == .dec) {
                        break :blk @divTrunc(lhs_val *% dec_scale, rhs_val);
                    } else {
                        break :blk @divTrunc(lhs_val, rhs_val);
                    }
                },
                .lt => if (lhs_val < rhs_val) @as(i128, 1) else @as(i128, 0),
                .gt => if (lhs_val > rhs_val) @as(i128, 1) else @as(i128, 0),
                .le => if (lhs_val <= rhs_val) @as(i128, 1) else @as(i128, 0),
                .ge => if (lhs_val >= rhs_val) @as(i128, 1) else @as(i128, 0),
                .eq => if (lhs_val == rhs_val) @as(i128, 1) else @as(i128, 0),
                .ne => if (lhs_val != rhs_val) @as(i128, 1) else @as(i128, 0),
                .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i128, 1) else @as(i128, 0),
                .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i128, 1) else @as(i128, 0),
            };

            return self.generateReturnI128Code(result_val);
        }

        // Fall back to i64 scalar operations
        const lhs_val = self.evalConstantI64(module_env, lhs_expr, env) orelse return error.UnsupportedExpression;
        const rhs_val = self.evalConstantI64(module_env, rhs_expr, env) orelse return error.UnsupportedExpression;

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

        return self.generateReturnI64Code(result_val, result_layout);
    }

    /// Compare two expressions for structural equality
    fn compareExprsForEquality(self: *ExprCodeGen, module_env: *ModuleEnv, lhs: CIR.Expr, rhs: CIR.Expr, env: *Scope) ?bool {
        const lhs_val = self.evalConstantI64(module_env, lhs, env);
        const rhs_val = self.evalConstantI64(module_env, rhs, env);
        if (lhs_val != null and rhs_val != null) {
            return lhs_val.? == rhs_val.?;
        }

        switch (lhs) {
            .e_record => |lhs_rec| {
                switch (rhs) {
                    .e_record => |rhs_rec| {
                        return self.compareRecords(module_env, lhs_rec, rhs_rec, env);
                    },
                    else => return null,
                }
            },
            .e_tuple => |lhs_tuple| {
                switch (rhs) {
                    .e_tuple => |rhs_tuple| {
                        return self.compareTuples(module_env, lhs_tuple, rhs_tuple, env);
                    },
                    else => return null,
                }
            },
            .e_empty_record => {
                return rhs == .e_empty_record;
            },
            .e_str_segment => |lhs_seg| {
                switch (rhs) {
                    .e_str_segment => |rhs_seg| {
                        return lhs_seg.literal == rhs_seg.literal;
                    },
                    else => return null,
                }
            },
            .e_str => |lhs_str_expr| {
                switch (rhs) {
                    .e_str => |rhs_str_expr| {
                        return compareStrings(module_env, lhs_str_expr, rhs_str_expr);
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Compare two string expressions
    fn compareStrings(module_env: *ModuleEnv, lhs: anytype, rhs: anytype) ?bool {
        const lhs_segments = module_env.store.sliceExpr(lhs.span);
        const rhs_segments = module_env.store.sliceExpr(rhs.span);

        if (lhs_segments.len != rhs_segments.len) return false;

        for (lhs_segments, rhs_segments) |lhs_seg_idx, rhs_seg_idx| {
            const lhs_seg = module_env.store.getExpr(lhs_seg_idx);
            const rhs_seg = module_env.store.getExpr(rhs_seg_idx);

            switch (lhs_seg) {
                .e_str_segment => |lhs_lit| {
                    switch (rhs_seg) {
                        .e_str_segment => |rhs_lit| {
                            if (lhs_lit.literal != rhs_lit.literal) return false;
                        },
                        else => return null,
                    }
                },
                else => return null,
            }
        }

        return true;
    }

    /// Compare two records for equality
    fn compareRecords(self: *ExprCodeGen, module_env: *ModuleEnv, lhs: anytype, rhs: anytype, env: *Scope) ?bool {
        if (lhs.ext != null or rhs.ext != null) return null;

        const lhs_fields = module_env.store.sliceRecordFields(lhs.fields);
        const rhs_fields = module_env.store.sliceRecordFields(rhs.fields);

        if (lhs_fields.len != rhs_fields.len) return false;

        for (lhs_fields) |lhs_field_idx| {
            const lhs_field = module_env.store.getRecordField(lhs_field_idx);
            const lhs_field_expr = module_env.store.getExpr(lhs_field.value);

            var found_match = false;
            for (rhs_fields) |rhs_field_idx| {
                const rhs_field = module_env.store.getRecordField(rhs_field_idx);
                if (lhs_field.name == rhs_field.name) {
                    const rhs_field_expr = module_env.store.getExpr(rhs_field.value);
                    const fields_equal = self.compareExprsForEquality(module_env, lhs_field_expr, rhs_field_expr, env) orelse return null;
                    if (!fields_equal) return false;
                    found_match = true;
                    break;
                }
            }
            if (!found_match) return false;
        }

        return true;
    }

    /// Compare two tuples for equality
    fn compareTuples(self: *ExprCodeGen, module_env: *ModuleEnv, lhs: anytype, rhs: anytype, env: *Scope) ?bool {
        const lhs_elems = module_env.store.sliceExpr(lhs.elems);
        const rhs_elems = module_env.store.sliceExpr(rhs.elems);

        if (lhs_elems.len != rhs_elems.len) return false;

        for (lhs_elems, rhs_elems) |lhs_elem_idx, rhs_elem_idx| {
            const lhs_elem = module_env.store.getExpr(lhs_elem_idx);
            const rhs_elem = module_env.store.getExpr(rhs_elem_idx);
            const elems_equal = self.compareExprsForEquality(module_env, lhs_elem, rhs_elem, env) orelse return null;
            if (!elems_equal) return false;
        }

        return true;
    }

    /// Unary minus
    fn generateUnaryMinusCode(self: *ExprCodeGen, module_env: *ModuleEnv, unary: CIR.Expr.UnaryMinus, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);

        if (result_layout == .i128 or result_layout == .u128 or result_layout == .dec) {
            const inner_val = self.evalConstantI128(module_env, inner_expr, env) orelse return error.UnsupportedExpression;
            return self.generateReturnI128Code(-inner_val);
        }

        const inner_val = self.evalConstantI64(module_env, inner_expr, env) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_layout);
    }

    /// If/else
    fn generateIfCode(self: *ExprCodeGen, module_env: *ModuleEnv, if_expr: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const branch_indices = module_env.store.sliceIfBranches(if_expr.branches);

        for (branch_indices) |branch_idx| {
            const branch = module_env.store.getIfBranch(branch_idx);
            const cond_expr = module_env.store.getExpr(branch.cond);
            const cond_val = self.evalConstantI64(module_env, cond_expr, env);

            if (cond_val) |val| {
                if (val != 0) {
                    const body_expr = module_env.store.getExpr(branch.body);
                    return self.generateCodeForExpr(module_env, body_expr, result_layout, env);
                }
            } else {
                return error.UnsupportedExpression;
            }
        }

        const else_expr = module_env.store.getExpr(if_expr.final_else);
        return self.generateCodeForExpr(module_env, else_expr, result_layout, env);
    }

    /// Evaluate constant i64
    pub fn evalConstantI64(self: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr, env: *Scope) ?i64 {
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
                if (env.lookupBinding(pattern_key)) |binding| {
                    switch (binding) {
                        .expr_ref => |expr_idx| {
                            const deferred_expr = module_env.store.getExpr(expr_idx);
                            return self.evalConstantI64(module_env, deferred_expr, env);
                        },
                        .i64_val => |val| return val,
                        .i128_val => |val| {
                            if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                                return @intCast(val);
                            }
                            return null;
                        },
                        .f64_val => return null,
                    }
                }
                return null;
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
                const lhs_val = self.evalConstantI64(module_env, lhs_expr, env) orelse return null;
                const rhs_val = self.evalConstantI64(module_env, rhs_expr, env) orelse return null;
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
                const inner_val = self.evalConstantI64(module_env, inner_expr, env) orelse return null;
                return -inner_val;
            },
            .e_unary_not => |unary| {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.evalConstantI64(module_env, inner_expr, env) orelse return null;
                return if (inner_val == 0) 1 else 0;
            },
            .e_nominal => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.evalConstantI64(module_env, backing_expr, env);
            },
            .e_nominal_external => |nom| {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                return self.evalConstantI64(module_env, backing_expr, env);
            },
            .e_dot_access => |dot| {
                if (dot.args != null) return null;
                const target_expr = self.resolveDotAccess(module_env, dot) orelse return null;
                return self.evalConstantI64(module_env, target_expr, env);
            },
            .e_call => |call| {
                const func_expr = module_env.store.getExpr(call.func);
                switch (func_expr) {
                    .e_lambda => |lambda| {
                        const arg_indices = module_env.store.sliceExpr(call.args);
                        const param_indices = module_env.store.slicePatterns(lambda.args);
                        if (arg_indices.len != param_indices.len) return null;

                        var temp_env = env.child();
                        defer temp_env.deinit();

                        for (arg_indices, param_indices) |arg_idx, param_idx| {
                            const arg_expr = module_env.store.getExpr(arg_idx);
                            const arg_val = self.evalConstantI64(module_env, arg_expr, env) orelse return null;
                            temp_env.bind(@intFromEnum(param_idx), arg_val) catch return null;
                        }

                        const body_expr = module_env.store.getExpr(lambda.body);
                        return self.evalConstantI64(module_env, body_expr, &temp_env);
                    },
                    .e_lookup_local => |lookup| {
                        const pattern_key = @intFromEnum(lookup.pattern_idx);
                        if (env.lookupClosure(pattern_key)) |stored_expr_idx| {
                            const stored_expr = module_env.store.getExpr(stored_expr_idx);
                            switch (stored_expr) {
                                .e_lambda => |lambda| {
                                    const arg_indices = module_env.store.sliceExpr(call.args);
                                    const param_indices = module_env.store.slicePatterns(lambda.args);
                                    if (arg_indices.len != param_indices.len) return null;

                                    var temp_env = env.child();
                                    defer temp_env.deinit();

                                    for (arg_indices, param_indices) |arg_idx, param_idx| {
                                        const arg_expr = module_env.store.getExpr(arg_idx);
                                        const arg_val = self.evalConstantI64(module_env, arg_expr, env) orelse return null;
                                        temp_env.bind(@intFromEnum(param_idx), arg_val) catch return null;
                                    }

                                    const body_expr = module_env.store.getExpr(lambda.body);
                                    return self.evalConstantI64(module_env, body_expr, &temp_env);
                                },
                                else => return null,
                            }
                        }
                        return null;
                    },
                    else => return null,
                }
            },
            else => null,
        };
    }

    /// Resolve a chain of dot accesses to the final field expression
    pub fn resolveDotAccess(self: *ExprCodeGen, module_env: *ModuleEnv, dot: anytype) ?CIR.Expr {
        if (dot.args != null) return null;

        const receiver_expr = module_env.store.getExpr(dot.receiver);
        const record_expr = self.resolveToRecord(module_env, receiver_expr) orelse return null;

        switch (record_expr) {
            .e_record => |rec| {
                if (rec.ext != null) return null;
                const fields = module_env.store.sliceRecordFields(rec.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (field.name == dot.field_name) {
                        return module_env.store.getExpr(field.value);
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Resolve an expression to a record expression
    fn resolveToRecord(self: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr) ?CIR.Expr {
        return switch (expr) {
            .e_record => expr,
            .e_dot_access => |dot| {
                const field_expr = self.resolveDotAccess(module_env, dot) orelse return null;
                return self.resolveToRecord(module_env, field_expr);
            },
            .e_nominal => |nom| {
                const backing = module_env.store.getExpr(nom.backing_expr);
                return self.resolveToRecord(module_env, backing);
            },
            .e_nominal_external => |nom| {
                const backing = module_env.store.getExpr(nom.backing_expr);
                return self.resolveToRecord(module_env, backing);
            },
            else => null,
        };
    }

    /// Generate code for a numeric literal
    fn generateNumericCode(self: *ExprCodeGen, num: anytype, result_layout: LayoutIdx) Error![]const u8 {
        const value_i128 = num.value.toI128();

        switch (num.kind) {
            .i128, .u128 => {
                return self.generateReturnI128Code(value_i128);
            },
            else => {
                if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
                    return error.UnsupportedType;
                }
                const value: i64 = @intCast(value_i128);
                return self.generateReturnI64Code(value, result_layout);
            },
        }
    }

    /// Generate code for a floating-point literal
    fn generateFloatCode(self: *ExprCodeGen, value: f64) Error![]const u8 {
        return self.generateReturnF64Code(value);
    }

    /// Generate code for decimal literals
    fn generateDecCode(self: *ExprCodeGen, dec: anytype, _: LayoutIdx) Error![]const u8 {
        const value_i128 = dec.value.toI128();
        return self.generateReturnI128Code(value_i128);
    }

    /// Generate code for small decimal literals
    fn generateDecSmallCode(self: *ExprCodeGen, dec: anytype, _: LayoutIdx) Error![]const u8 {
        const decimal_places: u8 = 18;
        const numerator: i128 = dec.value.numerator;
        const denominator_power: u8 = dec.value.denominator_power_of_ten;

        const scale_power: u8 = if (denominator_power >= decimal_places)
            0
        else
            decimal_places - denominator_power;

        var scale: i128 = 1;
        for (0..scale_power) |_| {
            scale *= 10;
        }

        const scaled_value = numerator * scale;
        return self.generateReturnI128Code(scaled_value);
    }

    /// Generate code for typed integer literals
    fn generateTypedIntCode(self: *ExprCodeGen, ti: anytype, result_layout: LayoutIdx) Error![]const u8 {
        const value_i128 = ti.value.toI128();

        if (result_layout == .i128 or result_layout == .u128) {
            return self.generateReturnI128Code(value_i128);
        }

        if (value_i128 > std.math.maxInt(i64) or value_i128 < std.math.minInt(i64)) {
            return error.UnsupportedType;
        }
        return self.generateReturnI64Code(@intCast(value_i128), result_layout);
    }

    /// Generate code for typed fraction literals
    fn generateTypedFracCode(self: *ExprCodeGen, tf: anytype, _: LayoutIdx) Error![]const u8 {
        const value_i128 = tf.value.toI128();
        const f64_val: f64 = @floatFromInt(value_i128);
        return self.generateReturnF64Code(f64_val);
    }

    /// Generate code for unary not (boolean negation)
    fn generateUnaryNotCode(self: *ExprCodeGen, module_env: *ModuleEnv, unary: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const inner_expr = module_env.store.getExpr(unary.expr);
        const inner_val = self.evalConstantI64(module_env, inner_expr, env) orelse
            return error.UnsupportedExpression;
        const result = if (inner_val == 0) @as(i64, 1) else @as(i64, 0);
        return self.generateReturnI64Code(result, result_layout);
    }

    /// Generate code for match expressions
    fn generateMatchCode(self: *ExprCodeGen, module_env: *ModuleEnv, match_expr: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const cond_expr = module_env.store.getExpr(match_expr.cond);
        const cond_val = self.evalConstantI64(module_env, cond_expr, env) orelse
            return error.UnsupportedExpression;

        const branches = module_env.store.sliceMatchBranches(match_expr.branches);
        for (branches) |branch_idx| {
            const branch = module_env.store.getMatchBranch(branch_idx);

            const patterns = module_env.store.sliceMatchBranchPatterns(branch.patterns);
            if (patterns.len == 0) continue;

            const branch_pattern = module_env.store.getMatchBranchPattern(patterns[0]);
            const pattern = module_env.store.getPattern(branch_pattern.pattern);
            const matches = self.patternMatches(module_env, pattern, cond_val, env);

            if (matches) {
                const body_expr = module_env.store.getExpr(branch.value);
                return self.generateCodeForExpr(module_env, body_expr, result_layout, env);
            }
        }

        return error.UnsupportedExpression;
    }

    /// Check if a pattern matches a value
    fn patternMatches(_: *ExprCodeGen, module_env: *ModuleEnv, pattern: CIR.Pattern, target_val: i64, _: *Scope) bool {
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
                _ = module_env.getIdent(assign.ident);
                return true;
            },
            .applied_tag => |tag| {
                if (tag.name == module_env.idents.true_tag) {
                    return target_val != 0;
                } else if (tag.name == module_env.idents.false_tag) {
                    return target_val == 0;
                }
                const args = module_env.store.slicePatterns(tag.args);
                if (args.len == 0) {
                    return true;
                }
                return false;
            },
            else => return false,
        }
    }

    /// Generate code for return expressions
    fn generateReturnExprCode(self: *ExprCodeGen, module_env: *ModuleEnv, ret: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const value_expr = module_env.store.getExpr(ret.expr);
        return self.generateCodeForExpr(module_env, value_expr, result_layout, env);
    }

    /// Generate code for dbg expressions
    fn generateDbgCode(self: *ExprCodeGen, module_env: *ModuleEnv, dbg: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const value_expr = module_env.store.getExpr(dbg.expr);
        return self.generateCodeForExpr(module_env, value_expr, result_layout, env);
    }

    /// Generate code for expect expressions
    fn generateExpectCode(self: *ExprCodeGen, _: *ModuleEnv, _: anytype, result_layout: LayoutIdx, _: *Scope) Error![]const u8 {
        return self.generateReturnI64Code(0, result_layout);
    }

    /// Generate code for zero-argument tags
    fn generateZeroArgTagCode(self: *ExprCodeGen, module_env: *ModuleEnv, tag: anytype, result_layout: LayoutIdx) Error![]const u8 {
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

        return self.generateReturnI64Code(value, result_layout);
    }

    /// Generate code for empty list []
    /// RocList: { bytes: null (0), length: 0, capacity: 0 } = 24 bytes of zeros
    fn generateEmptyListCode(self: *ExprCodeGen, _: LayoutIdx) Error![]const u8 {
        // Write 24 bytes of zeros to result pointer (RocList with null ptr, 0 len, 0 cap)
        switch (builtin.cpu.arch) {
            .x86_64 => {
                // xor rax, rax          ; 48 31 C0
                // mov [rdi], rax        ; 48 89 07
                // mov [rdi+8], rax      ; 48 89 47 08
                // mov [rdi+16], rax     ; 48 89 47 10
                // ret                   ; C3
                var code = self.allocator.alloc(u8, 18) catch return error.OutOfMemory;
                const result_reg: u8 = if (builtin.os.tag == .windows) 0x01 else 0x07; // rcx vs rdi

                code[0] = 0x48;
                code[1] = 0x31;
                code[2] = 0xC0; // xor rax, rax

                code[3] = 0x48;
                code[4] = 0x89;
                code[5] = result_reg; // mov [rdi/rcx], rax

                code[6] = 0x48;
                code[7] = 0x89;
                code[8] = result_reg | 0x40; // mov [rdi/rcx+8], rax
                code[9] = 0x08;

                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = result_reg | 0x40; // mov [rdi/rcx+16], rax
                code[13] = 0x10;

                code[14] = 0xC3; // ret

                // Pad to 18 bytes
                code[15] = 0x90;
                code[16] = 0x90;
                code[17] = 0x90;

                return code;
            },
            .aarch64 => {
                // mov x1, #0            ; D2 80 00 01
                // str x1, [x0]          ; F9 00 00 01
                // str x1, [x0, #8]      ; F9 00 04 01
                // str x1, [x0, #16]     ; F9 00 08 01
                // ret                   ; D6 5F 03 C0
                var code = self.allocator.alloc(u8, 20) catch return error.OutOfMemory;

                // mov x1, #0
                const mov_zero: u32 = 0xD2800001;
                @memcpy(code[0..4], std.mem.asBytes(&mov_zero));

                // str x1, [x0]
                const str_0: u32 = 0xF9000001;
                @memcpy(code[4..8], std.mem.asBytes(&str_0));

                // str x1, [x0, #8]
                const str_8: u32 = 0xF9000401;
                @memcpy(code[8..12], std.mem.asBytes(&str_8));

                // str x1, [x0, #16]
                const str_16: u32 = 0xF9000801;
                @memcpy(code[12..16], std.mem.asBytes(&str_16));

                // ret
                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[16..20], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code for tuple expressions
    fn generateTupleCode(self: *ExprCodeGen, module_env: *ModuleEnv, tuple: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const elems = module_env.store.sliceExpr(tuple.elems);

        if (elems.len == 0) {
            return self.generateReturnI64Code(0, result_layout);
        }

        if (elems.len == 1) {
            const elem_expr = module_env.store.getExpr(elems[0]);
            return self.generateCodeForExpr(module_env, elem_expr, result_layout, env);
        }

        var elem_values = std.array_list.Managed(i64).initCapacity(self.allocator, elems.len) catch
            return error.OutOfMemory;
        defer elem_values.deinit();

        for (elems) |elem_idx| {
            const elem_expr = module_env.store.getExpr(elem_idx);
            const elem_val = self.evalConstantI64(module_env, elem_expr, env) orelse
                return error.UnsupportedExpression;
            elem_values.append(elem_val) catch return error.OutOfMemory;
        }

        return self.generateReturnTupleCode(elem_values.items);
    }

    /// Generate code that returns a tuple of i64 values
    fn generateReturnTupleCode(self: *ExprCodeGen, values: []const i64) Error![]const u8 {
        switch (builtin.cpu.arch) {
            .x86_64 => {
                const per_elem: usize = 14;
                const code_size = values.len * per_elem + 1;
                var code = self.allocator.alloc(u8, code_size) catch return error.OutOfMemory;

                var pos: usize = 0;
                for (values, 0..) |value, idx| {
                    code[pos] = 0x48;
                    code[pos + 1] = 0xB8;
                    @memcpy(code[pos + 2 .. pos + 10], std.mem.asBytes(&value));

                    const offset: u8 = @intCast(idx * 8);
                    if (offset == 0) {
                        code[pos + 10] = 0x48;
                        code[pos + 11] = 0x89;
                        code[pos + 12] = if (builtin.os.tag == .windows) 0x01 else 0x07;
                        code[pos + 13] = 0x90;
                    } else {
                        code[pos + 10] = 0x48;
                        code[pos + 11] = 0x89;
                        code[pos + 12] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                        code[pos + 13] = offset;
                    }
                    pos += per_elem;
                }

                code[pos] = 0xC3;
                return code;
            },
            .aarch64 => {
                const per_elem: usize = 20;
                const code_size = values.len * per_elem + 4;
                var code = self.allocator.alloc(u8, code_size) catch return error.OutOfMemory;

                var pos: usize = 0;
                for (values, 0..) |value, idx| {
                    const uvalue: u64 = @bitCast(value);
                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    var mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[pos .. pos + 4], std.mem.asBytes(&mov_inst));
                    pos += 4;

                    var movk1: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[pos .. pos + 4], std.mem.asBytes(&movk1));
                    pos += 4;

                    var movk2: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[pos .. pos + 4], std.mem.asBytes(&movk2));
                    pos += 4;

                    var movk3: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[pos .. pos + 4], std.mem.asBytes(&movk3));
                    pos += 4;

                    const offset: u12 = @intCast(idx * 8);
                    const str_inst: u32 = 0xF9000001 | (@as(u32, offset / 8) << 10);
                    @memcpy(code[pos .. pos + 4], std.mem.asBytes(&str_inst));
                    pos += 4;
                }

                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[pos .. pos + 4], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }

    /// Element type classification for list code generation
    const ElementType = enum {
        i64_type,
        i128_type,
        f64_type,
        bool_type,
        str_type,
        unknown,

        fn size(self: ElementType) usize {
            return switch (self) {
                .bool_type => 1,
                .i64_type => 8,
                .i128_type => 16,
                .f64_type => 8,
                .str_type => 24, // RocStr structure
                .unknown => 8, // Default to i64
            };
        }
    };

    /// Determine the element type from a CIR expression
    fn getElementType(module_env: *ModuleEnv, expr: CIR.Expr) ElementType {
        return switch (expr) {
            .e_num => |num| switch (num.kind) {
                .i128, .u128 => .i128_type,
                .dec => .i128_type,
                .f32, .f64 => .f64_type,
                else => .i64_type,
            },
            .e_frac_f32, .e_frac_f64 => .f64_type,
            .e_dec, .e_dec_small => .i128_type,
            .e_typed_int => |ti| {
                const type_str = module_env.getIdent(ti.type_name);
                if (std.mem.eql(u8, type_str, "I128") or std.mem.eql(u8, type_str, "U128") or std.mem.eql(u8, type_str, "Dec")) {
                    return .i128_type;
                }
                if (std.mem.eql(u8, type_str, "F32") or std.mem.eql(u8, type_str, "F64")) {
                    return .f64_type;
                }
                return .i64_type;
            },
            .e_zero_argument_tag => .bool_type,
            .e_tag => |tag| {
                const args = module_env.store.sliceExpr(tag.args);
                if (args.len == 0) return .bool_type;
                return .unknown;
            },
            .e_str, .e_str_segment => .str_type,
            .e_lookup_local => .unknown, // Would need type info
            .e_binop => |binop| {
                // Get type from operands
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                return getElementType(module_env, lhs_expr);
            },
            .e_nominal => |nom| {
                const backing = module_env.store.getExpr(nom.backing_expr);
                return getElementType(module_env, backing);
            },
            .e_nominal_external => |nom| {
                const backing = module_env.store.getExpr(nom.backing_expr);
                return getElementType(module_env, backing);
            },
            else => .unknown,
        };
    }

    /// Generate code for list expressions
    /// For non-empty lists, allocates memory and creates a RocList structure.
    /// Supports i64, i128, Dec, f64, bool, and string elements.
    fn generateListCode(self: *ExprCodeGen, module_env: *ModuleEnv, list: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const elems = module_env.store.sliceExpr(list.elems);

        if (elems.len == 0) {
            return self.generateEmptyListCode(result_layout);
        }

        // Determine element type from first element
        const first_elem = module_env.store.getExpr(elems[0]);
        const elem_type = getElementType(module_env, first_elem);
        const elem_size = elem_type.size();

        // Allocate memory for the list data
        const data_size = elems.len * elem_size;
        const list_data = self.allocator.alloc(u8, data_size) catch return error.OutOfMemory;

        // Evaluate and copy each element into the allocated memory
        for (elems, 0..) |elem_idx, i| {
            const elem_expr = module_env.store.getExpr(elem_idx);
            const offset = i * elem_size;

            switch (elem_type) {
                .bool_type => {
                    const val = self.evalConstantI64(module_env, elem_expr, env) orelse
                        return error.UnsupportedExpression;
                    list_data[offset] = if (val != 0) 1 else 0;
                },
                .i64_type => {
                    const val = self.evalConstantI64(module_env, elem_expr, env) orelse
                        return error.UnsupportedExpression;
                    const bytes: [8]u8 = @bitCast(val);
                    @memcpy(list_data[offset..][0..8], &bytes);
                },
                .i128_type => {
                    const val = self.evalConstantI128(module_env, elem_expr, env) orelse
                        return error.UnsupportedExpression;
                    const bytes: [16]u8 = @bitCast(val);
                    @memcpy(list_data[offset..][0..16], &bytes);
                },
                .f64_type => {
                    const val = self.evalConstantF64(module_env, elem_expr, env) orelse
                        return error.UnsupportedExpression;
                    const bytes: [8]u8 = @bitCast(val);
                    @memcpy(list_data[offset..][0..8], &bytes);
                },
                .str_type => {
                    // For strings, create inline RocStr and copy it
                    const str_val = self.evalConstantStr(module_env, elem_expr) orelse
                        return error.UnsupportedExpression;
                    const roc_str = self.createRocStr(str_val) catch return error.OutOfMemory;
                    const str_bytes: [24]u8 = @bitCast(roc_str);
                    @memcpy(list_data[offset..][0..24], &str_bytes);
                },
                .unknown => {
                    // Try i64 first, then i128
                    if (self.evalConstantI64(module_env, elem_expr, env)) |val| {
                        const bytes: [8]u8 = @bitCast(val);
                        @memcpy(list_data[offset..][0..8], &bytes);
                    } else if (self.evalConstantI128(module_env, elem_expr, env)) |val| {
                        // Element is actually i128, but we allocated for i64
                        // This is a type mismatch - fall back to error
                        _ = val;
                        return error.UnsupportedExpression;
                    } else {
                        return error.UnsupportedExpression;
                    }
                },
            }
        }

        // Generate code that writes RocList { ptr, len, cap } to result pointer
        const data_ptr: u64 = @intFromPtr(list_data.ptr);
        const list_len: u64 = @intCast(elems.len);

        return self.generateReturnRocListCode(data_ptr, list_len, list_len);
    }

    /// Generate code that writes a RocList structure to the result pointer
    fn generateReturnRocListCode(self: *ExprCodeGen, ptr: u64, len: u64, cap: u64) Error![]const u8 {
        switch (builtin.cpu.arch) {
            .x86_64 => {
                // mov rax, ptr          ; 48 B8 <8 bytes>
                // mov [rdi], rax        ; 48 89 07
                // mov rax, len          ; 48 B8 <8 bytes>
                // mov [rdi+8], rax      ; 48 89 47 08
                // mov rax, cap          ; 48 B8 <8 bytes>
                // mov [rdi+16], rax     ; 48 89 47 10
                // ret                   ; C3
                var code = self.allocator.alloc(u8, 43) catch return error.OutOfMemory;
                const result_reg: u8 = if (builtin.os.tag == .windows) 0x01 else 0x07;

                var pos: usize = 0;

                // mov rax, ptr
                code[pos] = 0x48;
                code[pos + 1] = 0xB8;
                @memcpy(code[pos + 2 .. pos + 10], std.mem.asBytes(&ptr));
                pos += 10;

                // mov [rdi], rax
                code[pos] = 0x48;
                code[pos + 1] = 0x89;
                code[pos + 2] = result_reg;
                pos += 3;

                // mov rax, len
                code[pos] = 0x48;
                code[pos + 1] = 0xB8;
                @memcpy(code[pos + 2 .. pos + 10], std.mem.asBytes(&len));
                pos += 10;

                // mov [rdi+8], rax
                code[pos] = 0x48;
                code[pos + 1] = 0x89;
                code[pos + 2] = result_reg | 0x40;
                code[pos + 3] = 0x08;
                pos += 4;

                // mov rax, cap
                code[pos] = 0x48;
                code[pos + 1] = 0xB8;
                @memcpy(code[pos + 2 .. pos + 10], std.mem.asBytes(&cap));
                pos += 10;

                // mov [rdi+16], rax
                code[pos] = 0x48;
                code[pos + 1] = 0x89;
                code[pos + 2] = result_reg | 0x40;
                code[pos + 3] = 0x10;
                pos += 4;

                // ret
                code[pos] = 0xC3;
                pos += 1;

                // Pad to 43 bytes
                code[pos] = 0x90;

                return code;
            },
            .aarch64 => {
                // For each of ptr, len, cap:
                //   mov x1, imm  (4 instructions for 64-bit)
                //   str x1, [x0, offset]
                // Then ret
                const per_field: usize = 20; // 4 mov instructions + 1 str = 20 bytes
                const code_size = per_field * 3 + 4; // 3 fields + ret
                var code = self.allocator.alloc(u8, code_size) catch return error.OutOfMemory;

                var pos: usize = 0;
                const values = [_]u64{ ptr, len, cap };
                const offsets = [_]u12{ 0, 8, 16 };

                for (values, offsets) |value, offset| {
                    const imm0: u16 = @truncate(value);
                    const imm1: u16 = @truncate(value >> 16);
                    const imm2: u16 = @truncate(value >> 32);
                    const imm3: u16 = @truncate(value >> 48);

                    var mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&mov_inst));
                    pos += 4;

                    var movk1: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk1));
                    pos += 4;

                    var movk2: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk2));
                    pos += 4;

                    var movk3: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk3));
                    pos += 4;

                    const str_inst: u32 = 0xF9000001 | (@as(u32, offset / 8) << 10);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&str_inst));
                    pos += 4;
                }

                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[pos..][0..4], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code for block expressions
    fn generateBlockCode(self: *ExprCodeGen, module_env: *ModuleEnv, block: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        var block_env = env.child();
        defer block_env.deinit();

        const stmts = module_env.store.sliceStatements(block.stmts);
        for (stmts) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            try self.processStatement(module_env, stmt, &block_env);
        }

        const final_expr = module_env.store.getExpr(block.final_expr);
        return self.generateCodeForExpr(module_env, final_expr, result_layout, &block_env);
    }

    /// Process a statement in a block
    fn processStatement(self: *ExprCodeGen, module_env: *ModuleEnv, stmt: CIR.Statement, env: *Scope) Error!void {
        switch (stmt) {
            .s_decl => |decl| {
                try self.bindDeclaration(module_env, decl.expr, decl.pattern, env);
            },
            .s_decl_gen => |decl| {
                try self.bindDeclaration(module_env, decl.expr, decl.pattern, env);
            },
            .s_var => |var_decl| {
                // Mutable variable declaration - bind like a regular declaration
                try self.bindDeclaration(module_env, var_decl.expr, var_decl.pattern_idx, env);
            },
            .s_reassign => |reassign| {
                // Reassignment to mutable variable - update the binding
                try self.bindDeclaration(module_env, reassign.expr, reassign.pattern_idx, env);
            },
            .s_for => |for_stmt| {
                // For loop - iterate through the list and execute body for each element
                try self.executeForLoop(module_env, for_stmt.patt, for_stmt.expr, for_stmt.body, env);
            },
            .s_while => |while_stmt| {
                // While loop - execute while condition is true
                try self.executeWhileLoop(module_env, while_stmt.cond, while_stmt.body, env);
            },
            else => {
                return error.UnsupportedExpression;
            },
        }
    }

    /// Execute a for loop over a list
    fn executeForLoop(
        self: *ExprCodeGen,
        module_env: *ModuleEnv,
        pattern: CIR.Pattern.Idx,
        list_expr_idx: CIR.Expr.Idx,
        body_expr_idx: CIR.Expr.Idx,
        env: *Scope,
    ) Error!void {
        const list_expr = module_env.store.getExpr(list_expr_idx);

        // Get the list elements
        const elements: []const CIR.Expr.Idx = switch (list_expr) {
            .e_empty_list => &[_]CIR.Expr.Idx{},
            .e_list => |list| module_env.store.sliceExpr(list.elems),
            .e_lookup_local => |lookup| blk: {
                // Look up the list from a binding
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                if (env.lookupBinding(pattern_key)) |binding| {
                    switch (binding) {
                        .expr_ref => |expr_idx| {
                            const bound_expr = module_env.store.getExpr(expr_idx);
                            switch (bound_expr) {
                                .e_empty_list => break :blk &[_]CIR.Expr.Idx{},
                                .e_list => |list| break :blk module_env.store.sliceExpr(list.elems),
                                else => return error.UnsupportedExpression,
                            }
                        },
                        else => return error.UnsupportedExpression,
                    }
                }
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        };

        const pattern_key = @intFromEnum(pattern);

        // Iterate through each element
        for (elements) |elem_idx| {
            // Bind the element to the pattern
            try env.bindExpr(pattern_key, elem_idx);

            // Execute the body (ignore result - for loops return unit)
            const body_expr = module_env.store.getExpr(body_expr_idx);
            _ = self.generateCodeForExpr(module_env, body_expr, .i64, env) catch |err| {
                // For loops may call closures that reassign state - continue on errors
                if (err == error.UnsupportedExpression) continue;
                return err;
            };
        }
    }

    /// Execute a while loop
    fn executeWhileLoop(
        self: *ExprCodeGen,
        module_env: *ModuleEnv,
        cond_expr_idx: CIR.Expr.Idx,
        body_expr_idx: CIR.Expr.Idx,
        env: *Scope,
    ) Error!void {
        // Execute while condition is true (limited iterations for safety)
        const max_iterations: usize = 10000;
        var iteration: usize = 0;

        while (iteration < max_iterations) : (iteration += 1) {
            // Evaluate condition
            const cond_expr = module_env.store.getExpr(cond_expr_idx);
            const cond_val = self.evalConstantI64(module_env, cond_expr, env) orelse
                return error.UnsupportedExpression;

            // Check if condition is false (0 = false, non-0 = true)
            if (cond_val == 0) break;

            // Execute body
            const body_expr = module_env.store.getExpr(body_expr_idx);
            _ = self.generateCodeForExpr(module_env, body_expr, .i64, env) catch |err| {
                if (err == error.UnsupportedExpression) continue;
                return err;
            };
        }
    }

    /// Bind a declaration's expression to its pattern
    fn bindDeclaration(self: *ExprCodeGen, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, pattern: CIR.Pattern.Idx, env: *Scope) Error!void {
        const expr = module_env.store.getExpr(expr_idx);
        const pattern_key = @intFromEnum(pattern);

        switch (expr) {
            .e_lambda, .e_closure => {
                try env.bindClosure(pattern_key, expr_idx);
            },
            .e_num => |num| {
                switch (num.kind) {
                    .i128, .u128 => {
                        const value = num.value.toI128();
                        try env.bindI128(pattern_key, value);
                    },
                    else => {
                        if (self.evalConstantI64(module_env, expr, env)) |value| {
                            try env.bind(pattern_key, value);
                        } else {
                            const value = num.value.toI128();
                            try env.bindI128(pattern_key, value);
                        }
                    },
                }
            },
            .e_typed_int => |ti| {
                const value_i128 = ti.value.toI128();
                if (value_i128 >= std.math.minInt(i64) and value_i128 <= std.math.maxInt(i64)) {
                    try env.bind(pattern_key, @intCast(value_i128));
                } else {
                    try env.bindI128(pattern_key, value_i128);
                }
            },
            .e_dec => |dec| {
                const value_i128 = dec.value.toI128();
                try env.bindI128(pattern_key, value_i128);
            },
            .e_dec_small => |dec| {
                const decimal_places: u8 = 18;
                const numerator: i128 = dec.value.numerator;
                const denominator_power: u8 = dec.value.denominator_power_of_ten;
                const scale_power: u8 = if (denominator_power >= decimal_places)
                    0
                else
                    decimal_places - denominator_power;
                var scale: i128 = 1;
                for (0..scale_power) |_| {
                    scale *= 10;
                }
                const scaled_value = numerator * scale;
                try env.bindI128(pattern_key, scaled_value);
            },
            .e_frac_f64 => |frac| {
                try env.bindF64(pattern_key, frac.value);
            },
            .e_frac_f32 => |frac| {
                try env.bindF64(pattern_key, @floatCast(frac.value));
            },
            .e_binop => |binop| {
                if (self.evalConstantI128(module_env, expr, env)) |value_i128| {
                    if (value_i128 >= std.math.minInt(i64) and value_i128 <= std.math.maxInt(i64)) {
                        try env.bind(pattern_key, @intCast(value_i128));
                    } else {
                        try env.bindI128(pattern_key, value_i128);
                    }
                } else {
                    _ = binop;
                    return error.UnsupportedExpression;
                }
            },
            else => {
                if (self.evalConstantI64(module_env, expr, env)) |value| {
                    try env.bind(pattern_key, value);
                } else if (self.evalConstantI128(module_env, expr, env)) |value| {
                    try env.bindI128(pattern_key, value);
                } else {
                    try env.bindExpr(pattern_key, expr_idx);
                }
            },
        }
    }

    /// Try to fold a pure constant expression to an i128 value
    pub fn evalConstantI128(self: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr, env: *Scope) ?i128 {
        return switch (expr) {
            .e_num => |num| num.value.toI128(),
            .e_typed_int => |ti| ti.value.toI128(),
            .e_dec => |dec| dec.value.toI128(),
            .e_dec_small => |dec| blk: {
                const decimal_places: u8 = 18;
                const numerator: i128 = dec.value.numerator;
                const denominator_power: u8 = dec.value.denominator_power_of_ten;
                const scale_power: u8 = if (denominator_power >= decimal_places)
                    0
                else
                    decimal_places - denominator_power;
                var scale: i128 = 1;
                for (0..scale_power) |_| {
                    scale *= 10;
                }
                break :blk numerator * scale;
            },
            .e_lookup_local => |lookup| blk: {
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                break :blk env.lookupI128(pattern_key);
            },
            .e_binop => |binop| blk: {
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                const rhs_expr = module_env.store.getExpr(binop.rhs);
                const lhs_val = self.evalConstantI128(module_env, lhs_expr, env) orelse break :blk null;
                const rhs_val = self.evalConstantI128(module_env, rhs_expr, env) orelse break :blk null;
                break :blk switch (binop.op) {
                    .add => lhs_val +% rhs_val,
                    .sub => lhs_val -% rhs_val,
                    .mul => lhs_val *% rhs_val,
                    .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .rem => if (rhs_val != 0) @rem(lhs_val, rhs_val) else null,
                    .div_trunc => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else null,
                    .lt => if (lhs_val < rhs_val) @as(i128, 1) else @as(i128, 0),
                    .gt => if (lhs_val > rhs_val) @as(i128, 1) else @as(i128, 0),
                    .le => if (lhs_val <= rhs_val) @as(i128, 1) else @as(i128, 0),
                    .ge => if (lhs_val >= rhs_val) @as(i128, 1) else @as(i128, 0),
                    .eq => if (lhs_val == rhs_val) @as(i128, 1) else @as(i128, 0),
                    .ne => if (lhs_val != rhs_val) @as(i128, 1) else @as(i128, 0),
                    .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i128, 1) else @as(i128, 0),
                    .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i128, 1) else @as(i128, 0),
                };
            },
            .e_unary_minus => |unary| blk: {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.evalConstantI128(module_env, inner_expr, env) orelse break :blk null;
                break :blk -inner_val;
            },
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.evalConstantI128(module_env, backing_expr, env);
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.evalConstantI128(module_env, backing_expr, env);
            },
            else => null,
        };
    }

    /// Try to fold a pure constant expression to an f64 value
    pub fn evalConstantF64(self: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr, env: *Scope) ?f64 {
        return switch (expr) {
            .e_frac_f64 => |frac| frac.value,
            .e_frac_f32 => |frac| @floatCast(frac.value),
            .e_num => |num| @floatFromInt(num.value.toI128()),
            .e_lookup_local => |lookup| blk: {
                const pattern_key = @intFromEnum(lookup.pattern_idx);
                break :blk env.lookupF64(pattern_key);
            },
            .e_binop => |binop| blk: {
                const lhs_expr = module_env.store.getExpr(binop.lhs);
                const rhs_expr = module_env.store.getExpr(binop.rhs);
                const lhs_val = self.evalConstantF64(module_env, lhs_expr, env) orelse break :blk null;
                const rhs_val = self.evalConstantF64(module_env, rhs_expr, env) orelse break :blk null;
                break :blk switch (binop.op) {
                    .add => lhs_val + rhs_val,
                    .sub => lhs_val - rhs_val,
                    .mul => lhs_val * rhs_val,
                    .div => if (rhs_val != 0) lhs_val / rhs_val else null,
                    .lt => if (lhs_val < rhs_val) @as(f64, 1) else @as(f64, 0),
                    .gt => if (lhs_val > rhs_val) @as(f64, 1) else @as(f64, 0),
                    .le => if (lhs_val <= rhs_val) @as(f64, 1) else @as(f64, 0),
                    .ge => if (lhs_val >= rhs_val) @as(f64, 1) else @as(f64, 0),
                    .eq => if (lhs_val == rhs_val) @as(f64, 1) else @as(f64, 0),
                    .ne => if (lhs_val != rhs_val) @as(f64, 1) else @as(f64, 0),
                    else => null,
                };
            },
            .e_unary_minus => |unary| blk: {
                const inner_expr = module_env.store.getExpr(unary.expr);
                const inner_val = self.evalConstantF64(module_env, inner_expr, env) orelse break :blk null;
                break :blk -inner_val;
            },
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.evalConstantF64(module_env, backing_expr, env);
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk self.evalConstantF64(module_env, backing_expr, env);
            },
            else => null,
        };
    }

    /// Try to extract a constant string value from an expression
    pub fn evalConstantStr(_: *ExprCodeGen, module_env: *ModuleEnv, expr: CIR.Expr) ?[]const u8 {
        return switch (expr) {
            .e_str_segment => |seg| module_env.common.getString(seg.literal),
            .e_str => |str| blk: {
                const segments = module_env.store.sliceExpr(str.span);
                if (segments.len == 1) {
                    const seg_expr = module_env.store.getExpr(segments[0]);
                    switch (seg_expr) {
                        .e_str_segment => |seg| break :blk module_env.common.getString(seg.literal),
                        else => break :blk null,
                    }
                }
                break :blk null;
            },
            .e_nominal => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk evalConstantStr(undefined, module_env, backing_expr);
            },
            .e_nominal_external => |nom| blk: {
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk evalConstantStr(undefined, module_env, backing_expr);
            },
            else => null,
        };
    }

    /// Create a RocStr from a string slice
    /// Returns a 24-byte RocStr structure
    pub fn createRocStr(self: *ExprCodeGen, text: []const u8) !RocStr {
        const SMALL_STRING_SIZE: usize = 24;
        if (text.len < SMALL_STRING_SIZE) {
            // Small string - inline storage
            var roc_str: RocStr = undefined;
            @memset(std.mem.asBytes(&roc_str), 0);
            @memcpy(std.mem.asBytes(&roc_str)[0..text.len], text);
            std.mem.asBytes(&roc_str)[SMALL_STRING_SIZE - 1] = @as(u8, @intCast(text.len)) | 0x80;
            return roc_str;
        } else {
            // Large string - heap allocated
            const data = self.allocator.alloc(u8, text.len) catch return error.OutOfMemory;
            @memcpy(data, text);
            return RocStr{
                .bytes = data.ptr,
                .len = @intCast(text.len),
                .cap = @intCast(text.len),
            };
        }
    }

    /// RocStr structure for list of strings
    const RocStr = extern struct {
        bytes: ?[*]u8,
        len: u64,
        cap: u64,
    };

    /// Generate code for a string segment
    fn generateStrSegmentCode(_: *ExprCodeGen, module_env: *ModuleEnv, seg: anytype, _: LayoutIdx) Error![]const u8 {
        _ = module_env.getString(seg.literal);
        return error.UnsupportedExpression;
    }

    /// Generate code for a string expression
    fn generateStrCode(self: *ExprCodeGen, module_env: *ModuleEnv, str: anytype, _: LayoutIdx, _: *Scope) Error![]const u8 {
        const segments = module_env.store.sliceExpr(str.span);

        if (segments.len == 1) {
            const seg_expr = module_env.store.getExpr(segments[0]);
            switch (seg_expr) {
                .e_str_segment => |seg| {
                    const text = module_env.common.getString(seg.literal);
                    return self.generateStringCode(text);
                },
                else => return error.UnsupportedExpression,
            }
        }

        var total_len: usize = 0;
        for (segments) |seg_idx| {
            const seg_expr = module_env.store.getExpr(seg_idx);
            switch (seg_expr) {
                .e_str_segment => |seg| {
                    const text = module_env.common.getString(seg.literal);
                    total_len += text.len;
                },
                else => return error.UnsupportedExpression,
            }
        }

        const combined = self.allocator.alloc(u8, total_len) catch return error.OutOfMemory;
        defer self.allocator.free(combined);

        var pos: usize = 0;
        for (segments) |seg_idx| {
            const seg_expr = module_env.store.getExpr(seg_idx);
            switch (seg_expr) {
                .e_str_segment => |seg| {
                    const text = module_env.common.getString(seg.literal);
                    @memcpy(combined[pos..][0..text.len], text);
                    pos += text.len;
                },
                else => {},
            }
        }

        return self.generateStringCode(combined[0..pos]);
    }

    /// Generate code that returns a string
    fn generateStringCode(self: *ExprCodeGen, text: []const u8) Error![]const u8 {
        const SMALL_STRING_SIZE: usize = 24;
        if (text.len >= SMALL_STRING_SIZE) {
            return self.generateLargeStringCode(text);
        }

        var roc_str: [SMALL_STRING_SIZE]u8 = undefined;
        @memset(&roc_str, 0);
        @memcpy(roc_str[0..text.len], text);
        roc_str[SMALL_STRING_SIZE - 1] = @as(u8, @intCast(text.len)) | 0x80;

        switch (builtin.cpu.arch) {
            .x86_64 => {
                var code = self.allocator.alloc(u8, 36) catch return error.OutOfMemory;

                const val0: u64 = @bitCast(roc_str[0..8].*);
                code[0] = 0x48;
                code[1] = 0xB8;
                @memcpy(code[2..10], std.mem.asBytes(&val0));
                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = if (builtin.os.tag == .windows) 0x01 else 0x07;

                const val1: u64 = @bitCast(roc_str[8..16].*);
                code[13] = 0x48;
                code[14] = 0xB8;
                @memcpy(code[15..23], std.mem.asBytes(&val1));
                code[23] = 0x48;
                code[24] = 0x89;
                code[25] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                code[26] = 0x08;

                const val2: u64 = @bitCast(roc_str[16..24].*);
                code[27] = 0x48;
                code[28] = 0xB8;
                @memcpy(code[29..37], std.mem.asBytes(&val2));

                code = self.allocator.realloc(code, 42) catch return error.OutOfMemory;
                code[37] = 0x48;
                code[38] = 0x89;
                code[39] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                code[40] = 0x10;
                code[41] = 0xC3;

                return code;
            },
            .aarch64 => {
                var code = self.allocator.alloc(u8, 64) catch return error.OutOfMemory;
                var pos: usize = 0;

                for (0..3) |chunk_idx| {
                    const chunk_val: u64 = @bitCast(roc_str[chunk_idx * 8 ..][0..8].*);
                    const imm0: u16 = @truncate(chunk_val);
                    const imm1: u16 = @truncate(chunk_val >> 16);
                    const imm2: u16 = @truncate(chunk_val >> 32);
                    const imm3: u16 = @truncate(chunk_val >> 48);

                    var mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&mov_inst));
                    pos += 4;

                    var movk1: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk1));
                    pos += 4;

                    var movk2: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk2));
                    pos += 4;

                    var movk3: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk3));
                    pos += 4;

                    const offset: u12 = @intCast(chunk_idx * 8);
                    const str_inst: u32 = 0xF9000001 | (@as(u32, offset / 8) << 10);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&str_inst));
                    pos += 4;
                }

                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[pos..][0..4], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code for large strings (24+ bytes)
    fn generateLargeStringCode(self: *ExprCodeGen, text: []const u8) Error![]const u8 {
        const str_data = self.allocator.alloc(u8, text.len) catch return error.OutOfMemory;
        @memcpy(str_data, text);

        const str_ptr: u64 = @intFromPtr(str_data.ptr);
        const str_len: u64 = @intCast(text.len);

        switch (builtin.cpu.arch) {
            .x86_64 => {
                var code = self.allocator.alloc(u8, 42) catch return error.OutOfMemory;

                code[0] = 0x48;
                code[1] = 0xB8;
                @memcpy(code[2..10], std.mem.asBytes(&str_ptr));
                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = if (builtin.os.tag == .windows) 0x01 else 0x07;

                code[13] = 0x48;
                code[14] = 0xB8;
                @memcpy(code[15..23], std.mem.asBytes(&str_len));
                code[23] = 0x48;
                code[24] = 0x89;
                code[25] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                code[26] = 0x08;

                code[27] = 0x48;
                code[28] = 0xB8;
                @memcpy(code[29..37], std.mem.asBytes(&str_len));
                code[37] = 0x48;
                code[38] = 0x89;
                code[39] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                code[40] = 0x10;
                code[41] = 0xC3;

                return code;
            },
            .aarch64 => {
                var code = self.allocator.alloc(u8, 64) catch return error.OutOfMemory;
                var pos: usize = 0;

                const values = [_]u64{ str_ptr, str_len, str_len };

                for (values, 0..) |val, chunk_idx| {
                    const imm0: u16 = @truncate(val);
                    const imm1: u16 = @truncate(val >> 16);
                    const imm2: u16 = @truncate(val >> 32);
                    const imm3: u16 = @truncate(val >> 48);

                    var mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&mov_inst));
                    pos += 4;

                    var movk1: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk1));
                    pos += 4;

                    var movk2: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk2));
                    pos += 4;

                    var movk3: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&movk3));
                    pos += 4;

                    const offset: u12 = @intCast(chunk_idx * 8);
                    const str_inst: u32 = 0xF9000001 | (@as(u32, offset / 8) << 10);
                    @memcpy(code[pos..][0..4], std.mem.asBytes(&str_inst));
                    pos += 4;
                }

                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[pos..][0..4], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code for tag expressions with arguments
    fn generateTagCode(self: *ExprCodeGen, module_env: *ModuleEnv, tag: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        const args = module_env.store.sliceExpr(tag.args);

        if (args.len == 0) {
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
            return self.generateReturnI64Code(value, result_layout);
        }

        if (args.len == 1) {
            const arg_expr = module_env.store.getExpr(args[0]);
            const arg_val = self.evalConstantI64(module_env, arg_expr, env) orelse
                return error.UnsupportedExpression;
            return self.generateReturnI64Code(arg_val, result_layout);
        }

        return error.UnsupportedExpression;
    }

    /// Generate code for record expressions
    fn generateRecordCode(self: *ExprCodeGen, module_env: *ModuleEnv, rec: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        if (rec.ext != null) {
            return error.UnsupportedExpression;
        }

        const fields = module_env.store.sliceRecordFields(rec.fields);

        if (fields.len == 0) {
            return self.generateReturnI64Code(0, result_layout);
        }

        if (fields.len == 1) {
            const field = module_env.store.getRecordField(fields[0]);
            const field_expr = module_env.store.getExpr(field.value);
            const field_val = self.evalConstantI64(module_env, field_expr, env) orelse
                return error.UnsupportedExpression;
            return self.generateReturnI64Code(field_val, result_layout);
        }

        const first_field = module_env.store.getRecordField(fields[0]);
        const first_expr = module_env.store.getExpr(first_field.value);
        const first_val = self.evalConstantI64(module_env, first_expr, env) orelse
            return error.UnsupportedExpression;
        return self.generateReturnI64Code(first_val, result_layout);
    }

    /// Generate code for dot access
    fn generateDotAccessCode(self: *ExprCodeGen, module_env: *ModuleEnv, dot: anytype, result_layout: LayoutIdx, env: *Scope) Error![]const u8 {
        if (dot.args != null) {
            return error.UnsupportedExpression;
        }

        const target_expr = self.resolveDotAccess(module_env, dot) orelse return error.UnsupportedExpression;

        if (result_layout == .str) {
            return self.generateCodeForExpr(module_env, target_expr, result_layout, env);
        }

        if (result_layout == .i128 or result_layout == .u128 or result_layout == .dec) {
            const field_val = self.evalConstantI128(module_env, target_expr, env) orelse
                return error.UnsupportedExpression;
            return self.generateReturnI128Code(field_val);
        }

        const field_val = self.evalConstantI64(module_env, target_expr, env) orelse
            return error.UnsupportedExpression;

        return self.generateReturnI64Code(field_val, result_layout);
    }

    /// Generate code that returns an i64/u64 value
    pub fn generateReturnI64Code(self: *ExprCodeGen, value: i64, _: LayoutIdx) Error![]const u8 {
        switch (builtin.cpu.arch) {
            .x86_64 => {
                var code = self.allocator.alloc(u8, 14) catch return error.OutOfMemory;

                code[0] = 0x48;
                code[1] = 0xB8;
                @memcpy(code[2..10], std.mem.asBytes(&value));

                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = if (builtin.os.tag == .windows) 0x01 else 0x07;

                code[13] = 0xC3;

                return code;
            },
            .aarch64 => {
                const uvalue: u64 = @bitCast(value);

                if (uvalue <= 0xFFFF) {
                    var code = self.allocator.alloc(u8, 16) catch return error.OutOfMemory;

                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800001 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    const str_inst: u32 = 0xF9000001;
                    @memcpy(code[4..8], std.mem.asBytes(&str_inst));

                    const mov_x0_x1: u32 = 0xAA0103E0;
                    @memcpy(code[8..12], std.mem.asBytes(&mov_x0_x1));

                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[12..16], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    var code = self.allocator.alloc(u8, 28) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    const mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    const movk1_inst: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    const movk2_inst: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    const movk3_inst: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    const str_inst: u32 = 0xF9000001;
                    @memcpy(code[16..20], std.mem.asBytes(&str_inst));

                    const mov_x0_x1: u32 = 0xAA0103E0;
                    @memcpy(code[20..24], std.mem.asBytes(&mov_x0_x1));

                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[24..28], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code that returns an f64 value
    pub fn generateReturnF64Code(self: *ExprCodeGen, value: f64) Error![]const u8 {
        const bits: u64 = @bitCast(value);

        switch (builtin.cpu.arch) {
            .x86_64 => {
                var code = self.allocator.alloc(u8, 19) catch return error.OutOfMemory;

                code[0] = 0x48;
                code[1] = 0xB8;
                @memcpy(code[2..10], std.mem.asBytes(&bits));

                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = if (builtin.os.tag == .windows) 0x01 else 0x07;

                code[13] = 0x66;
                code[14] = 0x48;
                code[15] = 0x0F;
                code[16] = 0x6E;
                code[17] = 0xC0;

                code[18] = 0xC3;

                return code;
            },
            .aarch64 => {
                const uvalue = bits;

                if (uvalue <= 0xFFFF) {
                    var code = self.allocator.alloc(u8, 16) catch return error.OutOfMemory;

                    const imm16: u16 = @truncate(uvalue);
                    const mov_inst: u32 = 0xD2800001 | (@as(u32, imm16) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    const str_inst: u32 = 0xF9000001;
                    @memcpy(code[4..8], std.mem.asBytes(&str_inst));

                    const fmov_inst: u32 = 0x9E670020;
                    @memcpy(code[8..12], std.mem.asBytes(&fmov_inst));

                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[12..16], std.mem.asBytes(&ret_inst));

                    return code;
                } else {
                    var code = self.allocator.alloc(u8, 28) catch return error.OutOfMemory;

                    const imm0: u16 = @truncate(uvalue);
                    const imm1: u16 = @truncate(uvalue >> 16);
                    const imm2: u16 = @truncate(uvalue >> 32);
                    const imm3: u16 = @truncate(uvalue >> 48);

                    const mov_inst: u32 = 0xD2800001 | (@as(u32, imm0) << 5);
                    @memcpy(code[0..4], std.mem.asBytes(&mov_inst));

                    const movk1_inst: u32 = 0xF2A00001 | (@as(u32, imm1) << 5);
                    @memcpy(code[4..8], std.mem.asBytes(&movk1_inst));

                    const movk2_inst: u32 = 0xF2C00001 | (@as(u32, imm2) << 5);
                    @memcpy(code[8..12], std.mem.asBytes(&movk2_inst));

                    const movk3_inst: u32 = 0xF2E00001 | (@as(u32, imm3) << 5);
                    @memcpy(code[12..16], std.mem.asBytes(&movk3_inst));

                    const str_inst: u32 = 0xF9000001;
                    @memcpy(code[16..20], std.mem.asBytes(&str_inst));

                    const fmov_inst: u32 = 0x9E670020;
                    @memcpy(code[20..24], std.mem.asBytes(&fmov_inst));

                    const ret_inst: u32 = 0xD65F03C0;
                    @memcpy(code[24..28], std.mem.asBytes(&ret_inst));

                    return code;
                }
            },
            else => return error.UnsupportedType,
        }
    }

    /// Generate code that returns an i128 value
    pub fn generateReturnI128Code(self: *ExprCodeGen, value: i128) Error![]const u8 {
        const bits: u128 = @bitCast(value);
        const low: u64 = @truncate(bits);
        const high: u64 = @truncate(bits >> 64);

        switch (builtin.cpu.arch) {
            .x86_64 => {
                var code = self.allocator.alloc(u8, 32) catch return error.OutOfMemory;

                code[0] = 0x48;
                code[1] = 0xB8;
                @memcpy(code[2..10], std.mem.asBytes(&low));

                code[10] = 0x48;
                code[11] = 0x89;
                code[12] = if (builtin.os.tag == .windows) 0x01 else 0x07;

                code[13] = 0x48;
                code[14] = 0xB8;
                @memcpy(code[15..23], std.mem.asBytes(&high));

                code[23] = 0x48;
                code[24] = 0x89;
                code[25] = if (builtin.os.tag == .windows) 0x41 else 0x47;
                code[26] = 0x08;

                code[27] = 0xC3;

                code[28] = 0x90;
                code[29] = 0x90;
                code[30] = 0x90;
                code[31] = 0x90;

                return code;
            },
            .aarch64 => {
                var code = self.allocator.alloc(u8, 44) catch return error.OutOfMemory;

                const imm0_lo: u16 = @truncate(low);
                const imm1_lo: u16 = @truncate(low >> 16);
                const imm2_lo: u16 = @truncate(low >> 32);
                const imm3_lo: u16 = @truncate(low >> 48);

                var mov_lo: u32 = 0xD2800001 | (@as(u32, imm0_lo) << 5);
                @memcpy(code[0..4], std.mem.asBytes(&mov_lo));

                var movk1_lo: u32 = 0xF2A00001 | (@as(u32, imm1_lo) << 5);
                @memcpy(code[4..8], std.mem.asBytes(&movk1_lo));

                var movk2_lo: u32 = 0xF2C00001 | (@as(u32, imm2_lo) << 5);
                @memcpy(code[8..12], std.mem.asBytes(&movk2_lo));

                var movk3_lo: u32 = 0xF2E00001 | (@as(u32, imm3_lo) << 5);
                @memcpy(code[12..16], std.mem.asBytes(&movk3_lo));

                const str_lo: u32 = 0xF9000001;
                @memcpy(code[16..20], std.mem.asBytes(&str_lo));

                const imm0_hi: u16 = @truncate(high);
                const imm1_hi: u16 = @truncate(high >> 16);
                const imm2_hi: u16 = @truncate(high >> 32);
                const imm3_hi: u16 = @truncate(high >> 48);

                var mov_hi: u32 = 0xD2800002 | (@as(u32, imm0_hi) << 5);
                @memcpy(code[20..24], std.mem.asBytes(&mov_hi));

                var movk1_hi: u32 = 0xF2A00002 | (@as(u32, imm1_hi) << 5);
                @memcpy(code[24..28], std.mem.asBytes(&movk1_hi));

                var movk2_hi: u32 = 0xF2C00002 | (@as(u32, imm2_hi) << 5);
                @memcpy(code[28..32], std.mem.asBytes(&movk2_hi));

                var movk3_hi: u32 = 0xF2E00002 | (@as(u32, imm3_hi) << 5);
                @memcpy(code[32..36], std.mem.asBytes(&movk3_hi));

                const str_hi: u32 = 0xF9000402;
                @memcpy(code[36..40], std.mem.asBytes(&str_hi));

                const ret_inst: u32 = 0xD65F03C0;
                @memcpy(code[40..44], std.mem.asBytes(&ret_inst));

                return code;
            },
            else => return error.UnsupportedType,
        }
    }
};

test "ExprCodeGen initialization" {
    var codegen = ExprCodeGen.init(std.testing.allocator);
    defer codegen.deinit();
}
