//! Mono IR Expression Code Generator for the Dev Backend
//!
//! This module generates native machine code (x86_64/aarch64) from Mono IR expressions.
//! Unlike ExprCodeGen which works with CIR, this works with the lowered Mono IR where:
//! - All symbol references are globally unique (MonoSymbol = module_idx + ident_idx)
//! - All types are concrete layouts (layout.Idx)
//! - No module-local index collisions
//!
//! Architecture:
//! - MonoExprCodeGen holds state during code generation
//! - MonoScope tracks variable bindings using globally unique MonoSymbol keys
//! - BindingValue represents different types of bound values

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const layout = @import("layout");
const mono = @import("mono");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;

const MonoIR = mono.MonoIR;
const MonoExprStore = mono.MonoExprStore;
const MonoExpr = MonoIR.MonoExpr;
const MonoPattern = MonoIR.MonoPattern;
const MonoExprId = MonoIR.MonoExprId;
const MonoPatternId = MonoIR.MonoPatternId;
const MonoExprSpan = MonoIR.MonoExprSpan;
const MonoPatternSpan = MonoIR.MonoPatternSpan;
const MonoSymbol = MonoIR.MonoSymbol;
const MonoCapture = MonoIR.MonoCapture;

/// Layout index for result type
pub const LayoutIdx = layout.Idx;

/// Binding value that can hold different types during code generation
pub const BindingValue = union(enum) {
    i64_val: i64,
    i128_val: i128,
    f64_val: f64,
    expr_ref: MonoExprId, // For deferred evaluation (expressions not yet evaluated)

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
/// Uses MonoSymbol as keys - globally unique across all modules.
pub const MonoScope = struct {
    parent: ?*const MonoScope,
    /// Bindings keyed by MonoSymbol (packed u48, used as u64 for hashmap)
    bindings: std.AutoHashMap(u64, BindingValue),
    /// Closure references - stores the MonoExprId for closures/lambdas
    closure_refs: std.AutoHashMap(u64, MonoExprId),
    allocator: Allocator,

    fn symbolKey(symbol: MonoSymbol) u64 {
        // MonoSymbol is a packed u48, convert to u64 for hashmap key
        return @as(u64, @as(u48, @bitCast(symbol)));
    }

    pub fn init(allocator: Allocator) MonoScope {
        return .{
            .parent = null,
            .bindings = std.AutoHashMap(u64, BindingValue).init(allocator),
            .closure_refs = std.AutoHashMap(u64, MonoExprId).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn child(self: *const MonoScope) MonoScope {
        return .{
            .parent = self,
            .bindings = std.AutoHashMap(u64, BindingValue).init(self.allocator),
            .closure_refs = std.AutoHashMap(u64, MonoExprId).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    pub fn bind(self: *MonoScope, symbol: MonoSymbol, value: i64) !void {
        try self.bindings.put(symbolKey(symbol), BindingValue{ .i64_val = value });
    }

    pub fn bindI128(self: *MonoScope, symbol: MonoSymbol, value: i128) !void {
        try self.bindings.put(symbolKey(symbol), BindingValue{ .i128_val = value });
    }

    pub fn bindF64(self: *MonoScope, symbol: MonoSymbol, value: f64) !void {
        try self.bindings.put(symbolKey(symbol), BindingValue{ .f64_val = value });
    }

    pub fn bindExpr(self: *MonoScope, symbol: MonoSymbol, expr_id: MonoExprId) !void {
        try self.bindings.put(symbolKey(symbol), BindingValue{ .expr_ref = expr_id });
    }

    pub fn bindClosure(self: *MonoScope, symbol: MonoSymbol, expr_id: MonoExprId) !void {
        try self.closure_refs.put(symbolKey(symbol), expr_id);
    }

    pub fn lookup(self: *const MonoScope, symbol: MonoSymbol) ?i64 {
        if (self.bindings.get(symbolKey(symbol))) |v| return v.asI64();
        if (self.parent) |p| return p.lookup(symbol);
        return null;
    }

    pub fn lookupI128(self: *const MonoScope, symbol: MonoSymbol) ?i128 {
        if (self.bindings.get(symbolKey(symbol))) |v| return v.asI128();
        if (self.parent) |p| return p.lookupI128(symbol);
        return null;
    }

    pub fn lookupF64(self: *const MonoScope, symbol: MonoSymbol) ?f64 {
        if (self.bindings.get(symbolKey(symbol))) |v| return v.asF64();
        if (self.parent) |p| return p.lookupF64(symbol);
        return null;
    }

    pub fn lookupBinding(self: *const MonoScope, symbol: MonoSymbol) ?BindingValue {
        if (self.bindings.get(symbolKey(symbol))) |v| return v;
        if (self.parent) |p| return p.lookupBinding(symbol);
        return null;
    }

    pub fn lookupClosure(self: *const MonoScope, symbol: MonoSymbol) ?MonoExprId {
        if (self.closure_refs.get(symbolKey(symbol))) |v| return v;
        if (self.parent) |p| return p.lookupClosure(symbol);
        return null;
    }

    pub fn deinit(self: *MonoScope) void {
        self.bindings.deinit();
        self.closure_refs.deinit();
    }
};

/// Expression code generator for Mono IR
pub const MonoExprCodeGen = struct {
    allocator: Allocator,

    /// The Mono IR store containing all expressions
    mono_store: *const MonoExprStore,

    /// All module environments for string/ident lookups
    all_module_envs: []const *ModuleEnv,

    /// Crash message from crash expression (if any)
    crash_message: ?[]const u8 = null,

    pub const Error = error{
        OutOfMemory,
        UnsupportedType,
        UnsupportedExpression,
        Crash,
        RuntimeError,
    };

    /// Initialize a new MonoExprCodeGen
    pub fn init(
        allocator: Allocator,
        mono_store: *const MonoExprStore,
        all_module_envs: []const *ModuleEnv,
    ) MonoExprCodeGen {
        return MonoExprCodeGen{
            .allocator = allocator,
            .mono_store = mono_store,
            .all_module_envs = all_module_envs,
        };
    }

    /// Clean up
    pub fn deinit(self: *MonoExprCodeGen) void {
        if (self.crash_message) |msg| {
            self.allocator.free(msg);
        }
    }

    /// Set the crash message
    pub fn setCrashMessage(self: *MonoExprCodeGen, message: []const u8) !void {
        if (self.crash_message) |old_msg| {
            self.allocator.free(old_msg);
        }
        self.crash_message = try self.allocator.dupe(u8, message);
    }

    /// Get the crash message (if any)
    pub fn getCrashMessage(self: *const MonoExprCodeGen) ?[]const u8 {
        return self.crash_message;
    }

    /// Create an empty Scope for code generation
    pub fn createScope(self: *MonoExprCodeGen) MonoScope {
        return MonoScope.init(self.allocator);
    }

    /// Generate code for a Mono IR expression
    pub fn generateCodeForExpr(
        self: *MonoExprCodeGen,
        expr_id: MonoExprId,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        if (expr_id.isNone()) {
            return error.UnsupportedExpression;
        }

        const expr = self.mono_store.getExpr(expr_id);

        return switch (expr) {
            // Literals
            .i64_literal => |val| try self.generateReturnI64Code(val, result_layout),
            .i128_literal => |val| try self.generateReturnI128Code(val),
            .f64_literal => |val| try self.generateReturnF64Code(val),
            .f32_literal => |val| try self.generateReturnF64Code(@as(f64, val)),
            .dec_literal => |val| try self.generateReturnI128Code(val),
            .bool_literal => |val| try self.generateReturnI64Code(if (val) 1 else 0, result_layout),

            // String literals
            .str_literal => |lit_idx| try self.generateStrLiteralCode(lit_idx),

            // Lookups - globally unique symbol
            .lookup => |lookup| try self.generateLookupCode(lookup.symbol, lookup.layout_idx, env),

            // Function calls
            .call => |call| try self.generateCallCode(call, result_layout, env),

            // Lambda and closure
            .lambda => return error.UnsupportedExpression, // Lambdas handled via call
            .closure => return error.UnsupportedExpression, // Closures handled via call

            // Data structures
            .empty_list => try self.generateReturnI64Code(0, result_layout),
            .list => |list| try self.generateListCode(list, result_layout, env),
            .empty_record => try self.generateReturnI64Code(0, result_layout),
            .record => |rec| try self.generateRecordCode(rec, result_layout, env),
            .tuple => |tuple| try self.generateTupleCode(tuple, result_layout, env),

            // Field access
            .field_access => |fa| try self.generateFieldAccessCode(fa, env),
            .tuple_access => |ta| try self.generateTupleAccessCode(ta, env),

            // Tags
            .zero_arg_tag => |tag| try self.generateZeroArgTagCode(tag, result_layout),
            .tag => |tag| try self.generateTagCode(tag, result_layout, env),

            // Control flow
            .if_then_else => |ite| try self.generateIfCode(ite, result_layout, env),
            .when => |when| try self.generateWhenCode(when, result_layout, env),

            // Blocks
            .block => |block| try self.generateBlockCode(block, result_layout, env),
            .early_return => |ret| try self.generateCodeForExpr(ret.expr, ret.ret_layout, env),

            // Binary operations
            .binop => |binop| try self.generateBinopCode(binop, result_layout, env),
            .unary_minus => |um| try self.generateUnaryMinusCode(um, result_layout, env),
            .unary_not => |un| try self.generateUnaryNotCode(un, result_layout, env),

            // Low-level operations
            .low_level => |ll| try self.generateLowLevelCode(ll, result_layout, env),

            // Debugging/errors
            .dbg => |dbg| try self.generateCodeForExpr(dbg.expr, dbg.result_layout, env),
            .expect => try self.generateReturnI64Code(0, result_layout),
            .crash => |crash| {
                const msg = self.getStringLiteral(crash.msg);
                self.setCrashMessage(msg) catch return error.OutOfMemory;
                return error.Crash;
            },
            .runtime_error => {
                self.setCrashMessage("Runtime error encountered") catch return error.OutOfMemory;
                return error.RuntimeError;
            },

            // Nominal (transparent wrapper)
            .nominal => |nom| try self.generateCodeForExpr(nom.backing_expr, nom.nominal_layout, env),
        };
    }

    /// Generate code for lookup
    fn generateLookupCode(
        self: *MonoExprCodeGen,
        symbol: MonoSymbol,
        lookup_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        if (env.lookupBinding(symbol)) |binding| {
            switch (binding) {
                .expr_ref => |expr_id| {
                    return self.generateCodeForExpr(expr_id, lookup_layout, env);
                },
                .i64_val => |value| {
                    return self.generateReturnI64Code(value, lookup_layout);
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

    /// Generate code for function calls
    fn generateCallCode(
        self: *MonoExprCodeGen,
        call: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const fn_expr = self.mono_store.getExpr(call.fn_expr);

        switch (fn_expr) {
            .lambda => |lambda| {
                return self.applyLambda(lambda, call.args, result_layout, env);
            },
            .closure => |closure| {
                return self.applyClosure(closure, call.args, result_layout, env);
            },
            .lookup => |lookup| {
                // Look up the closure in scope
                if (env.lookupClosure(lookup.symbol)) |closure_id| {
                    const closure_expr = self.mono_store.getExpr(closure_id);
                    switch (closure_expr) {
                        .lambda => |lambda| {
                            return self.applyLambda(lambda, call.args, result_layout, env);
                        },
                        .closure => |closure| {
                            return self.applyClosure(closure, call.args, result_layout, env);
                        },
                        else => return error.UnsupportedExpression,
                    }
                }
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Apply a lambda to arguments
    fn applyLambda(
        self: *MonoExprCodeGen,
        lambda: anytype,
        args_span: MonoExprSpan,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const arg_ids = self.mono_store.getExprSpan(args_span);
        const param_ids = self.mono_store.getPatternSpan(lambda.params);

        if (arg_ids.len != param_ids.len) {
            return error.UnsupportedExpression;
        }

        var new_env = env.child();
        defer new_env.deinit();

        for (arg_ids, param_ids) |arg_id, param_id| {
            try self.bindArgumentToParam(arg_id, param_id, env, &new_env);
        }

        return self.generateCodeForExpr(lambda.body, result_layout, &new_env);
    }

    /// Apply a closure to arguments
    fn applyClosure(
        self: *MonoExprCodeGen,
        closure: anytype,
        args_span: MonoExprSpan,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const lambda_expr = self.mono_store.getExpr(closure.lambda);
        switch (lambda_expr) {
            .lambda => |lambda| {
                const arg_ids = self.mono_store.getExprSpan(args_span);
                const param_ids = self.mono_store.getPatternSpan(lambda.params);

                if (arg_ids.len != param_ids.len) {
                    return error.UnsupportedExpression;
                }

                var new_env = env.child();
                defer new_env.deinit();

                // Bind captures first
                const captures = self.mono_store.getCaptures(closure.captures);
                for (captures) |capture| {
                    if (env.lookup(capture.symbol)) |cv| {
                        try new_env.bind(capture.symbol, cv);
                    } else if (env.lookupClosure(capture.symbol)) |closure_id| {
                        try new_env.bindClosure(capture.symbol, closure_id);
                    } else {
                        return error.UnsupportedExpression;
                    }
                }

                // Then bind arguments
                for (arg_ids, param_ids) |arg_id, param_id| {
                    try self.bindArgumentToParam(arg_id, param_id, env, &new_env);
                }

                return self.generateCodeForExpr(lambda.body, result_layout, &new_env);
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Bind an argument expression to a parameter pattern
    fn bindArgumentToParam(
        self: *MonoExprCodeGen,
        arg_id: MonoExprId,
        param_id: MonoPatternId,
        env: *MonoScope,
        new_env: *MonoScope,
    ) Error!void {
        const arg_expr = self.mono_store.getExpr(arg_id);
        const param_pattern = self.mono_store.getPattern(param_id);

        const param_symbol: MonoSymbol = switch (param_pattern) {
            .bind => |b| b.symbol,
            .wildcard => return, // No binding needed
            else => return error.UnsupportedExpression,
        };

        switch (arg_expr) {
            .lambda, .closure => {
                try new_env.bindClosure(param_symbol, arg_id);
            },
            .lookup => |lookup| {
                if (env.lookupClosure(lookup.symbol)) |closure_id| {
                    try new_env.bindClosure(param_symbol, closure_id);
                } else if (env.lookup(lookup.symbol)) |cv| {
                    try new_env.bind(param_symbol, cv);
                } else {
                    return error.UnsupportedExpression;
                }
            },
            else => {
                if (self.evalConstantI64(arg_id, env)) |arg_val| {
                    try new_env.bind(param_symbol, arg_val);
                    return;
                }
                if (self.evalConstantI128(arg_id, env)) |arg_val| {
                    try new_env.bindI128(param_symbol, arg_val);
                    return;
                }
                try new_env.bindExpr(param_symbol, arg_id);
            },
        }
    }

    /// Evaluate constant i64
    pub fn evalConstantI64(self: *MonoExprCodeGen, expr_id: MonoExprId, env: *MonoScope) ?i64 {
        if (expr_id.isNone()) return null;
        const expr = self.mono_store.getExpr(expr_id);

        return switch (expr) {
            .i64_literal => |val| val,
            .i128_literal => |val| blk: {
                if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                    break :blk @intCast(val);
                }
                break :blk null;
            },
            .bool_literal => |val| if (val) 1 else 0,
            .lookup => |lookup| env.lookup(lookup.symbol),
            .binop => |binop| self.evalBinopI64(binop, env),
            .unary_minus => |um| blk: {
                const inner = self.evalConstantI64(um.expr, env) orelse break :blk null;
                break :blk -inner;
            },
            .unary_not => |un| blk: {
                const inner = self.evalConstantI64(un.expr, env) orelse break :blk null;
                break :blk if (inner == 0) 1 else 0;
            },
            .zero_arg_tag => |tag| blk: {
                // Boolean tags: discriminant 0 = False, 1 = True
                break :blk @as(i64, tag.discriminant);
            },
            else => null,
        };
    }

    /// Evaluate constant i128
    pub fn evalConstantI128(self: *MonoExprCodeGen, expr_id: MonoExprId, env: *MonoScope) ?i128 {
        if (expr_id.isNone()) return null;
        const expr = self.mono_store.getExpr(expr_id);

        return switch (expr) {
            .i64_literal => |val| @as(i128, val),
            .i128_literal => |val| val,
            .dec_literal => |val| val,
            .lookup => |lookup| env.lookupI128(lookup.symbol),
            else => null,
        };
    }

    /// Evaluate a binop for i64
    fn evalBinopI64(self: *MonoExprCodeGen, binop: anytype, env: *MonoScope) ?i64 {
        const lhs = self.evalConstantI64(binop.lhs, env) orelse return null;
        const rhs = self.evalConstantI64(binop.rhs, env) orelse return null;

        return switch (binop.op) {
            .add => lhs +% rhs,
            .sub => lhs -% rhs,
            .mul => lhs *% rhs,
            .div => if (rhs != 0) @divTrunc(lhs, rhs) else null,
            .mod => if (rhs != 0) @rem(lhs, rhs) else null,
            .lt => if (lhs < rhs) @as(i64, 1) else @as(i64, 0),
            .gt => if (lhs > rhs) @as(i64, 1) else @as(i64, 0),
            .lte => if (lhs <= rhs) @as(i64, 1) else @as(i64, 0),
            .gte => if (lhs >= rhs) @as(i64, 1) else @as(i64, 0),
            .eq => if (lhs == rhs) @as(i64, 1) else @as(i64, 0),
            .neq => if (lhs != rhs) @as(i64, 1) else @as(i64, 0),
            .@"and" => if (lhs != 0 and rhs != 0) @as(i64, 1) else @as(i64, 0),
            .@"or" => if (lhs != 0 or rhs != 0) @as(i64, 1) else @as(i64, 0),
        };
    }

    /// Generate code for binary operations
    fn generateBinopCode(
        self: *MonoExprCodeGen,
        binop: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        // Check if this is a 128-bit operation
        if (result_layout == .i128 or result_layout == .u128 or result_layout == .dec) {
            const lhs_val = self.evalConstantI128(binop.lhs, env) orelse return error.UnsupportedExpression;
            const rhs_val = self.evalConstantI128(binop.rhs, env) orelse return error.UnsupportedExpression;

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
                .mod => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
                .lt => if (lhs_val < rhs_val) @as(i128, 1) else @as(i128, 0),
                .gt => if (lhs_val > rhs_val) @as(i128, 1) else @as(i128, 0),
                .lte => if (lhs_val <= rhs_val) @as(i128, 1) else @as(i128, 0),
                .gte => if (lhs_val >= rhs_val) @as(i128, 1) else @as(i128, 0),
                .eq => if (lhs_val == rhs_val) @as(i128, 1) else @as(i128, 0),
                .neq => if (lhs_val != rhs_val) @as(i128, 1) else @as(i128, 0),
                .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i128, 1) else @as(i128, 0),
                .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i128, 1) else @as(i128, 0),
            };

            return self.generateReturnI128Code(result_val);
        }

        // Fall back to i64 operations
        const lhs_val = self.evalConstantI64(binop.lhs, env) orelse return error.UnsupportedExpression;
        const rhs_val = self.evalConstantI64(binop.rhs, env) orelse return error.UnsupportedExpression;

        const result_val: i64 = switch (binop.op) {
            .add => lhs_val +% rhs_val,
            .sub => lhs_val -% rhs_val,
            .mul => lhs_val *% rhs_val,
            .div => if (rhs_val != 0) @divTrunc(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .mod => if (rhs_val != 0) @rem(lhs_val, rhs_val) else return error.UnsupportedExpression,
            .lt => if (lhs_val < rhs_val) @as(i64, 1) else @as(i64, 0),
            .gt => if (lhs_val > rhs_val) @as(i64, 1) else @as(i64, 0),
            .lte => if (lhs_val <= rhs_val) @as(i64, 1) else @as(i64, 0),
            .gte => if (lhs_val >= rhs_val) @as(i64, 1) else @as(i64, 0),
            .eq => if (lhs_val == rhs_val) @as(i64, 1) else @as(i64, 0),
            .neq => if (lhs_val != rhs_val) @as(i64, 1) else @as(i64, 0),
            .@"and" => if (lhs_val != 0 and rhs_val != 0) @as(i64, 1) else @as(i64, 0),
            .@"or" => if (lhs_val != 0 or rhs_val != 0) @as(i64, 1) else @as(i64, 0),
        };

        return self.generateReturnI64Code(result_val, result_layout);
    }

    /// Generate code for unary minus
    fn generateUnaryMinusCode(
        self: *MonoExprCodeGen,
        unary: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        if (result_layout == .i128 or result_layout == .u128 or result_layout == .dec) {
            const inner_val = self.evalConstantI128(unary.expr, env) orelse return error.UnsupportedExpression;
            return self.generateReturnI128Code(-inner_val);
        }

        const inner_val = self.evalConstantI64(unary.expr, env) orelse return error.UnsupportedExpression;
        return self.generateReturnI64Code(-inner_val, result_layout);
    }

    /// Generate code for unary not
    fn generateUnaryNotCode(
        self: *MonoExprCodeGen,
        unary: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const inner_val = self.evalConstantI64(unary.expr, env) orelse return error.UnsupportedExpression;
        const result = if (inner_val == 0) @as(i64, 1) else @as(i64, 0);
        return self.generateReturnI64Code(result, result_layout);
    }

    /// Generate code for if-then-else
    fn generateIfCode(
        self: *MonoExprCodeGen,
        ite: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const branches = self.mono_store.getIfBranches(ite.branches);

        for (branches) |branch| {
            const cond_val = self.evalConstantI64(branch.cond, env);
            if (cond_val) |val| {
                if (val != 0) {
                    return self.generateCodeForExpr(branch.body, result_layout, env);
                }
            } else {
                return error.UnsupportedExpression;
            }
        }

        return self.generateCodeForExpr(ite.final_else, result_layout, env);
    }

    /// Generate code for when/match
    fn generateWhenCode(
        self: *MonoExprCodeGen,
        when: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const value = self.evalConstantI64(when.value, env) orelse return error.UnsupportedExpression;
        const branches = self.mono_store.getWhenBranches(when.branches);

        for (branches) |branch| {
            if (self.patternMatches(branch.pattern, value)) {
                return self.generateCodeForExpr(branch.body, result_layout, env);
            }
        }

        return error.UnsupportedExpression;
    }

    /// Check if a pattern matches a value
    fn patternMatches(self: *MonoExprCodeGen, pattern_id: MonoPatternId, value: i64) bool {
        if (pattern_id.isNone()) return false;
        const pattern = self.mono_store.getPattern(pattern_id);

        return switch (pattern) {
            .wildcard => true,
            .bind => true, // Bind always matches
            .int_literal => |lit| blk: {
                if (lit.value > std.math.maxInt(i64) or lit.value < std.math.minInt(i64)) {
                    break :blk false;
                }
                break :blk value == @as(i64, @intCast(lit.value));
            },
            .tag => |tag| blk: {
                // For boolean tags, discriminant matches value
                break :blk value == tag.discriminant;
            },
            else => false,
        };
    }

    /// Generate code for blocks
    fn generateBlockCode(
        self: *MonoExprCodeGen,
        block: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        var block_env = env.child();
        defer block_env.deinit();

        const stmts = self.mono_store.getStmts(block.stmts);

        for (stmts) |stmt| {
            const pattern = self.mono_store.getPattern(stmt.pattern);
            const symbol: MonoSymbol = switch (pattern) {
                .bind => |b| b.symbol,
                .wildcard => continue, // No binding needed
                else => continue,
            };

            const stmt_expr = self.mono_store.getExpr(stmt.expr);
            switch (stmt_expr) {
                .lambda, .closure => {
                    try block_env.bindClosure(symbol, stmt.expr);
                },
                else => {
                    if (self.evalConstantI64(stmt.expr, &block_env)) |val| {
                        try block_env.bind(symbol, val);
                    } else if (self.evalConstantI128(stmt.expr, &block_env)) |val| {
                        try block_env.bindI128(symbol, val);
                    } else {
                        try block_env.bindExpr(symbol, stmt.expr);
                    }
                },
            }
        }

        return self.generateCodeForExpr(block.final_expr, result_layout, &block_env);
    }

    /// Generate code for zero-arg tags
    fn generateZeroArgTagCode(
        self: *MonoExprCodeGen,
        tag: anytype,
        result_layout: LayoutIdx,
    ) Error![]const u8 {
        return self.generateReturnI64Code(tag.discriminant, result_layout);
    }

    /// Generate code for tags with args
    fn generateTagCode(
        self: *MonoExprCodeGen,
        tag: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        _ = self;
        _ = tag;
        _ = result_layout;
        _ = env;
        // Tags with payloads not yet supported
        return error.UnsupportedExpression;
    }

    /// Generate code for lists
    fn generateListCode(
        self: *MonoExprCodeGen,
        list: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        _ = self;
        _ = list;
        _ = result_layout;
        _ = env;
        return error.UnsupportedExpression;
    }

    /// Generate code for records
    fn generateRecordCode(
        self: *MonoExprCodeGen,
        rec: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        _ = self;
        _ = rec;
        _ = result_layout;
        _ = env;
        return error.UnsupportedExpression;
    }

    /// Generate code for tuples
    fn generateTupleCode(
        self: *MonoExprCodeGen,
        tuple: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        _ = self;
        _ = tuple;
        _ = result_layout;
        _ = env;
        return error.UnsupportedExpression;
    }

    /// Generate code for field access
    fn generateFieldAccessCode(
        _: *MonoExprCodeGen,
        fa: anytype,
        _: *MonoScope,
    ) Error![]const u8 {
        _ = fa;
        // Field access not fully supported yet
        return error.UnsupportedExpression;
    }

    /// Generate code for tuple access
    fn generateTupleAccessCode(
        _: *MonoExprCodeGen,
        ta: anytype,
        _: *MonoScope,
    ) Error![]const u8 {
        _ = ta;
        // Tuple access not fully supported yet
        return error.UnsupportedExpression;
    }

    /// Generate code for low-level operations
    fn generateLowLevelCode(
        self: *MonoExprCodeGen,
        ll: anytype,
        result_layout: LayoutIdx,
        _: *MonoScope,
    ) Error![]const u8 {
        const args = self.mono_store.getExprSpan(ll.args);

        switch (ll.op) {
            .list_len => {
                if (args.len != 1) return error.UnsupportedExpression;
                const list_expr = self.mono_store.getExpr(args[0]);
                switch (list_expr) {
                    .empty_list => return self.generateReturnI64Code(0, result_layout),
                    .list => |list| {
                        const elems = self.mono_store.getExprSpan(list.elems);
                        return self.generateReturnI64Code(@intCast(elems.len), result_layout);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            .list_is_empty => {
                if (args.len != 1) return error.UnsupportedExpression;
                const list_expr = self.mono_store.getExpr(args[0]);
                switch (list_expr) {
                    .empty_list => return self.generateReturnI64Code(1, result_layout),
                    .list => |list| {
                        const elems = self.mono_store.getExprSpan(list.elems);
                        const result: i64 = if (elems.len == 0) 1 else 0;
                        return self.generateReturnI64Code(result, result_layout);
                    },
                    else => return error.UnsupportedExpression,
                }
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate code for string literal
    fn generateStrLiteralCode(self: *MonoExprCodeGen, lit_idx: base.StringLiteral.Idx) Error![]const u8 {
        _ = self;
        _ = lit_idx;
        // String literals not fully supported yet
        return error.UnsupportedExpression;
    }

    /// Get a string literal from any module
    fn getStringLiteral(self: *MonoExprCodeGen, lit_idx: base.StringLiteral.Idx) []const u8 {
        // Try each module to find the string
        for (self.all_module_envs) |module| {
            const str = module.common.getString(lit_idx);
            if (str.len > 0) {
                return str;
            }
        }
        return "<unknown>";
    }

    // ========== Machine Code Generation ==========

    /// Generate machine code that returns an i64 value
    pub fn generateReturnI64Code(self: *MonoExprCodeGen, value: i64, _: LayoutIdx) Error![]const u8 {
        // Architecture-specific code generation
        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64ReturnI64(value),
            .aarch64 => self.generateArm64ReturnI64(value),
            else => error.UnsupportedExpression,
        };
    }

    /// Generate machine code that returns an f64 value
    pub fn generateReturnF64Code(self: *MonoExprCodeGen, value: f64) Error![]const u8 {
        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64ReturnF64(value),
            .aarch64 => self.generateArm64ReturnF64(value),
            else => error.UnsupportedExpression,
        };
    }

    /// Generate machine code that returns an i128 value
    pub fn generateReturnI128Code(self: *MonoExprCodeGen, value: i128) Error![]const u8 {
        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64ReturnI128(value),
            .aarch64 => self.generateArm64ReturnI128(value),
            else => error.UnsupportedExpression,
        };
    }

    // ========== x86_64 Code Generation ==========

    fn generateX64ReturnI64(self: *MonoExprCodeGen, value: i64) Error![]const u8 {
        // mov qword [rdi], value
        // ret
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // REX.W prefix + mov r/m64, imm32 or movabs for large values
        if (value >= std.math.minInt(i32) and value <= std.math.maxInt(i32)) {
            // mov qword [rdi], imm32 (sign-extended)
            try code.appendSlice(&[_]u8{ 0x48, 0xC7, 0x07 }); // REX.W mov [rdi], imm32
            const val32: i32 = @intCast(value);
            try code.appendSlice(&@as([4]u8, @bitCast(val32)));
        } else {
            // movabs rax, imm64; mov [rdi], rax
            try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
            try code.appendSlice(&@as([8]u8, @bitCast(value)));
            try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x07 }); // mov [rdi], rax
        }

        try code.append(0xC3); // ret
        return code.toOwnedSlice();
    }

    fn generateX64ReturnF64(self: *MonoExprCodeGen, value: f64) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // movabs rax, imm64; mov [rdi], rax
        const bits = @as(u64, @bitCast(value));
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(bits)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x07 }); // mov [rdi], rax
        try code.append(0xC3); // ret

        return code.toOwnedSlice();
    }

    fn generateX64ReturnI128(self: *MonoExprCodeGen, value: i128) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        const bits = @as(u128, @bitCast(value));
        const low: u64 = @truncate(bits);
        const high: u64 = @truncate(bits >> 64);

        // movabs rax, low; mov [rdi], rax
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 });
        try code.appendSlice(&@as([8]u8, @bitCast(low)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x07 });

        // movabs rax, high; mov [rdi+8], rax
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 });
        try code.appendSlice(&@as([8]u8, @bitCast(high)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x47, 0x08 });

        try code.append(0xC3); // ret

        return code.toOwnedSlice();
    }

    // ========== ARM64 Code Generation ==========

    fn generateArm64ReturnI64(self: *MonoExprCodeGen, value: i64) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        const uvalue = @as(u64, @bitCast(value));

        // Load value into x1 using movz/movk sequence
        try self.generateArm64LoadImm64(&code, 1, uvalue);

        // str x1, [x0]
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000001))));

        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    fn generateArm64ReturnF64(self: *MonoExprCodeGen, value: f64) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        const bits = @as(u64, @bitCast(value));

        // Load bits into x1
        try self.generateArm64LoadImm64(&code, 1, bits);

        // str x1, [x0]
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000001))));

        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    fn generateArm64ReturnI128(self: *MonoExprCodeGen, value: i128) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        const bits = @as(u128, @bitCast(value));
        const low: u64 = @truncate(bits);
        const high: u64 = @truncate(bits >> 64);

        // Load low into x1, store to [x0]
        try self.generateArm64LoadImm64(&code, 1, low);
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000001))));

        // Load high into x1, store to [x0, #8]
        try self.generateArm64LoadImm64(&code, 1, high);
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000401))));

        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    fn generateArm64LoadImm64(self: *MonoExprCodeGen, code: *std.array_list.Managed(u8), reg: u5, value: u64) !void {
        _ = self;
        // movz/movk sequence to load 64-bit immediate
        const chunks = [4]u16{
            @truncate(value),
            @truncate(value >> 16),
            @truncate(value >> 32),
            @truncate(value >> 48),
        };

        // movz xN, #imm, lsl #0
        const movz: u32 = 0xD2800000 | (@as(u32, chunks[0]) << 5) | reg;
        try code.appendSlice(&@as([4]u8, @bitCast(movz)));

        // movk for remaining chunks if non-zero
        if (chunks[1] != 0) {
            const movk1: u32 = 0xF2A00000 | (@as(u32, chunks[1]) << 5) | reg;
            try code.appendSlice(&@as([4]u8, @bitCast(movk1)));
        }
        if (chunks[2] != 0) {
            const movk2: u32 = 0xF2C00000 | (@as(u32, chunks[2]) << 5) | reg;
            try code.appendSlice(&@as([4]u8, @bitCast(movk2)));
        }
        if (chunks[3] != 0) {
            const movk3: u32 = 0xF2E00000 | (@as(u32, chunks[3]) << 5) | reg;
            try code.appendSlice(&@as([4]u8, @bitCast(movk3)));
        }
    }
};

// ============ Tests ============

test "MonoScope bind and lookup" {
    const allocator = std.testing.allocator;
    var scope = MonoScope.init(allocator);
    defer scope.deinit();

    const ident1 = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const symbol = MonoSymbol{ .module_idx = 0, .ident_idx = ident1 };

    try scope.bind(symbol, 123);
    try std.testing.expectEqual(@as(?i64, 123), scope.lookup(symbol));
}

test "MonoScope child scope" {
    const allocator = std.testing.allocator;
    var parent = MonoScope.init(allocator);
    defer parent.deinit();

    const ident1 = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const symbol1 = MonoSymbol{ .module_idx = 0, .ident_idx = ident1 };
    try parent.bind(symbol1, 100);

    var child = parent.child();
    defer child.deinit();

    const ident2 = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const symbol2 = MonoSymbol{ .module_idx = 0, .ident_idx = ident2 };
    try child.bind(symbol2, 200);

    // Child can see parent binding
    try std.testing.expectEqual(@as(?i64, 100), child.lookup(symbol1));
    // Child can see own binding
    try std.testing.expectEqual(@as(?i64, 200), child.lookup(symbol2));
    // Parent cannot see child binding
    try std.testing.expectEqual(@as(?i64, null), parent.lookup(symbol2));
}

test "MonoScope global symbol uniqueness" {
    const allocator = std.testing.allocator;
    var scope = MonoScope.init(allocator);
    defer scope.deinit();

    // Same ident index, different modules = different symbols
    const ident = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const symbol_mod0 = MonoSymbol{ .module_idx = 0, .ident_idx = ident };
    const symbol_mod1 = MonoSymbol{ .module_idx = 1, .ident_idx = ident };

    try scope.bind(symbol_mod0, 100);
    try scope.bind(symbol_mod1, 200);

    // Different modules, same ident = different values
    try std.testing.expectEqual(@as(?i64, 100), scope.lookup(symbol_mod0));
    try std.testing.expectEqual(@as(?i64, 200), scope.lookup(symbol_mod1));
}
