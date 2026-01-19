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
const StaticDataInterner = @import("StaticDataInterner.zig");

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

    /// Global static data interner for string literals and other static data.
    /// When set, big strings are interned here instead of heap-allocated.
    /// This enables deduplication and proper static storage.
    static_interner: ?*StaticDataInterner = null,

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
            .static_interner = null,
        };
    }

    /// Initialize with a static data interner for proper string literal handling
    pub fn initWithInterner(
        allocator: Allocator,
        mono_store: *const MonoExprStore,
        all_module_envs: []const *ModuleEnv,
        static_interner: *StaticDataInterner,
    ) MonoExprCodeGen {
        return MonoExprCodeGen{
            .allocator = allocator,
            .mono_store = mono_store,
            .all_module_envs = all_module_envs,
            .static_interner = static_interner,
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

            // Inspect operations (str_inspekt support)
            .str_concat => |parts| try self.generateStrConcatCode(parts, result_layout, env),
            .int_to_str => |info| try self.generateIntToStrCode(info, result_layout, env),
            .float_to_str => |info| try self.generateFloatToStrCode(info, result_layout, env),
            .dec_to_str => |value_expr| try self.generateDecToStrCode(value_expr, result_layout, env),
            .str_escape_and_quote => |inner| try self.generateStrEscapeAndQuoteCode(inner, result_layout, env),
            .discriminant_switch => |sw| try self.generateDiscriminantSwitchCode(sw, result_layout, env),

            // Reference counting operations
            .incref => |rc| try self.generateIncrefCode(rc, env),
            .decref => |rc| try self.generateDecrefCode(rc, env),
            // free is a no-op - memory is managed by arenas in compile-time evaluation
            .free => try self.generateNoOpCode(),
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
            .field_access => |fa| self.evalFieldAccessI64(fa, env),
            .if_then_else => |ite| self.evalIfThenElseI64(ite, env),
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

    /// Evaluate field access for i64 - used for compile-time field access evaluation
    fn evalFieldAccessI64(self: *MonoExprCodeGen, fa: anytype, env: *MonoScope) ?i64 {
        // Get the record expression
        const record_expr = self.mono_store.getExpr(fa.record_expr);

        switch (record_expr) {
            .record => |rec| {
                // Record literal - look up the field by name
                const fields = self.mono_store.getExprSpan(rec.fields);
                const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                // Find the field index by matching the field name
                const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;

                if (field_idx >= fields.len) return null;
                const field_expr_id = fields[field_idx];
                return self.evalConstantI64(field_expr_id, env);
            },
            .field_access => |inner_fa| {
                // Nested field access - first resolve the inner access to get the record
                const inner_record_expr_id = self.resolveFieldAccessToRecord(inner_fa, env) orelse return null;
                const inner_record = self.mono_store.getExpr(inner_record_expr_id);
                if (inner_record == .record) {
                    const rec = inner_record.record;
                    const fields = self.mono_store.getExprSpan(rec.fields);
                    const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                    const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;
                    if (field_idx >= fields.len) return null;
                    const field_expr_id = fields[field_idx];
                    return self.evalConstantI64(field_expr_id, env);
                }
                return null;
            },
            .lookup => |lookup| {
                // Lookup - check if it's bound to a record expression
                if (env.lookupBinding(lookup.symbol)) |binding| {
                    if (binding == .expr_ref) {
                        const bound_expr = self.mono_store.getExpr(binding.expr_ref);
                        if (bound_expr == .record) {
                            const rec = bound_expr.record;
                            const fields = self.mono_store.getExprSpan(rec.fields);
                            const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                            const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;
                            if (field_idx >= fields.len) return null;
                            const field_expr_id = fields[field_idx];
                            return self.evalConstantI64(field_expr_id, env);
                        }
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Resolve a field access expression to the underlying record expression ID
    fn resolveFieldAccessToRecord(self: *MonoExprCodeGen, fa: anytype, env: *MonoScope) ?MonoExprId {
        const record_expr = self.mono_store.getExpr(fa.record_expr);

        switch (record_expr) {
            .record => |rec| {
                // Get the field value from this record
                const fields = self.mono_store.getExprSpan(rec.fields);
                const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;
                if (field_idx >= fields.len) return null;
                return fields[field_idx];
            },
            .field_access => |inner_fa| {
                // Recursively resolve nested field access
                const inner_record_expr_id = self.resolveFieldAccessToRecord(inner_fa, env) orelse return null;
                const inner_record = self.mono_store.getExpr(inner_record_expr_id);
                if (inner_record == .record) {
                    const rec = inner_record.record;
                    const fields = self.mono_store.getExprSpan(rec.fields);
                    const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                    const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;
                    if (field_idx >= fields.len) return null;
                    return fields[field_idx];
                }
                return null;
            },
            .lookup => |lookup| {
                // Lookup - check if it's bound to a record expression
                if (env.lookupBinding(lookup.symbol)) |binding| {
                    if (binding == .expr_ref) {
                        const bound_expr = self.mono_store.getExpr(binding.expr_ref);
                        if (bound_expr == .record) {
                            const rec = bound_expr.record;
                            const fields = self.mono_store.getExprSpan(rec.fields);
                            const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                            const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse return null;
                            if (field_idx >= fields.len) return null;
                            return fields[field_idx];
                        }
                    }
                }
                return null;
            },
            else => return null,
        }
    }

    /// Evaluate if-then-else for i64 - used for compile-time conditional evaluation
    fn evalIfThenElseI64(self: *MonoExprCodeGen, ite: anytype, env: *MonoScope) ?i64 {
        const branches = self.mono_store.getIfBranches(ite.branches);

        for (branches) |branch| {
            const cond_val = self.evalConstantI64(branch.cond, env) orelse return null;
            if (cond_val != 0) {
                return self.evalConstantI64(branch.body, env);
            }
        }

        return self.evalConstantI64(ite.final_else, env);
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
        self: *MonoExprCodeGen,
        fa: anytype,
        env: *MonoScope,
    ) Error![]const u8 {
        // Get the record expression
        const record_expr = self.mono_store.getExpr(fa.record_expr);

        switch (record_expr) {
            .record => |rec| {
                // Record literal - look up the field by name
                const fields = self.mono_store.getExprSpan(rec.fields);
                const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                // Find the field index by matching the field name
                const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse {
                    return error.UnsupportedExpression;
                };

                if (field_idx >= fields.len) {
                    return error.UnsupportedExpression;
                }
                const field_expr_id = fields[field_idx];
                // Infer the actual layout from the field expression
                const actual_layout = self.inferExprLayout(field_expr_id, fa.field_layout);
                return self.generateCodeForExpr(field_expr_id, actual_layout, env);
            },
            .empty_record => {
                // Empty record - no fields to access
                return error.UnsupportedExpression;
            },
            .lookup => |lookup| {
                // Lookup - check if it's bound to a record expression
                if (env.lookupBinding(lookup.symbol)) |binding| {
                    if (binding == .expr_ref) {
                        const bound_expr = self.mono_store.getExpr(binding.expr_ref);
                        if (bound_expr == .record) {
                            const fields = self.mono_store.getExprSpan(bound_expr.record.fields);
                            const field_names = self.mono_store.getFieldNameSpan(bound_expr.record.field_names);

                            // Find the field index by matching the field name
                            const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse {
                                return error.UnsupportedExpression;
                            };

                            if (field_idx >= fields.len) {
                                return error.UnsupportedExpression;
                            }
                            const field_expr_id = fields[field_idx];
                            // Infer the actual layout from the field expression
                            const actual_layout = self.inferExprLayout(field_expr_id, fa.field_layout);
                            return self.generateCodeForExpr(field_expr_id, actual_layout, env);
                        }
                    }
                }
                return error.UnsupportedExpression;
            },
            .field_access => |inner_fa| {
                // Nested field access - first resolve the inner access to get the record
                const inner_record_expr_id = self.resolveFieldAccessToRecord(inner_fa, env) orelse {
                    return error.UnsupportedExpression;
                };
                const inner_record = self.mono_store.getExpr(inner_record_expr_id);
                if (inner_record == .record) {
                    const rec = inner_record.record;
                    const fields = self.mono_store.getExprSpan(rec.fields);
                    const field_names = self.mono_store.getFieldNameSpan(rec.field_names);

                    const field_idx = self.findFieldIndexByName(field_names, fa.field_name) orelse {
                        return error.UnsupportedExpression;
                    };
                    if (field_idx >= fields.len) {
                        return error.UnsupportedExpression;
                    }
                    const field_expr_id = fields[field_idx];
                    const actual_layout = self.inferExprLayout(field_expr_id, fa.field_layout);
                    return self.generateCodeForExpr(field_expr_id, actual_layout, env);
                }
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Infer the layout of an expression from its type
    fn inferExprLayout(self: *MonoExprCodeGen, expr_id: MonoExprId, fallback: LayoutIdx) LayoutIdx {
        const expr = self.mono_store.getExpr(expr_id);
        return switch (expr) {
            .str_literal => .str,
            .i64_literal => .i64,
            .i128_literal => .i128,
            .f64_literal => .f64,
            .f32_literal => .f32,
            .dec_literal => .dec,
            .bool_literal => .bool,
            .empty_list => |el| el.elem_layout,
            .list => |l| l.elem_layout,
            else => fallback,
        };
    }

    /// Find field index by matching field name
    fn findFieldIndexByName(self: *MonoExprCodeGen, field_names: []const base.Ident.Idx, target_name: base.Ident.Idx) ?usize {
        _ = self;
        for (field_names, 0..) |name, idx| {
            // Compare the packed u32 representation of Ident.Idx
            if (@as(u32, @bitCast(name)) == @as(u32, @bitCast(target_name))) {
                return idx;
            }
        }
        return null;
    }

    /// Generate code for tuple access
    fn generateTupleAccessCode(
        self: *MonoExprCodeGen,
        ta: anytype,
        env: *MonoScope,
    ) Error![]const u8 {
        // Get the tuple expression
        const tuple_expr = self.mono_store.getExpr(ta.tuple_expr);

        switch (tuple_expr) {
            .tuple => |tup| {
                // Tuple literal - get the element directly from the elems span
                const elems = self.mono_store.getExprSpan(tup.elems);
                if (ta.elem_idx >= elems.len) {
                    return error.UnsupportedExpression;
                }
                const elem_expr_id = elems[ta.elem_idx];
                return self.generateCodeForExpr(elem_expr_id, ta.elem_layout, env);
            },
            .lookup => |lookup| {
                // Lookup - check if it's bound to a tuple expression
                if (env.lookupBinding(lookup.symbol)) |binding| {
                    if (binding == .expr_ref) {
                        const bound_expr = self.mono_store.getExpr(binding.expr_ref);
                        if (bound_expr == .tuple) {
                            const elems = self.mono_store.getExprSpan(bound_expr.tuple.elems);
                            if (ta.elem_idx >= elems.len) {
                                return error.UnsupportedExpression;
                            }
                            const elem_expr_id = elems[ta.elem_idx];
                            return self.generateCodeForExpr(elem_expr_id, ta.elem_layout, env);
                        }
                    }
                }
                return error.UnsupportedExpression;
            },
            else => return error.UnsupportedExpression,
        }
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

    /// RocStr constants for small string optimization
    const ROCSTR_SIZE: usize = 24;
    const SMALL_STR_CAPACITY: usize = ROCSTR_SIZE - 1; // 23 bytes
    const SMALL_STR_MASK: u8 = 0x80;

    /// Generate code for string literal
    fn generateStrLiteralCode(self: *MonoExprCodeGen, lit_idx: base.StringLiteral.Idx) Error![]const u8 {
        const str_text = self.getStringLiteral(lit_idx);
        return self.generateRocStrCode(str_text);
    }

    /// Generate code that creates a RocStr from the given text
    fn generateRocStrCode(self: *MonoExprCodeGen, str_text: []const u8) Error![]const u8 {
        if (str_text.len <= SMALL_STR_CAPACITY) {
            return self.generateSmallStrCode(str_text);
        } else {
            return self.generateBigStrCode(str_text);
        }
    }

    /// Refcount value indicating static data that should never be freed.
    /// This MUST match utils.REFCOUNT_STATIC_DATA = 0.
    /// When a refcount equals this value, it indicates static/constant data that
    /// should never be decremented or freed.
    /// The value 0 is chosen because:
    /// - It's clearly distinct from normal refcounts (which start at 1)
    /// - It makes the "constant" check very efficient
    /// - It's safe since normal refcounts should never reach 0 while still being referenced
    const REFCOUNT_STATIC: usize = 0;

    /// Generate code for a big string (>23 bytes)
    /// Uses static interner when available for proper static storage.
    /// Falls back to heap allocation with static refcount for compatibility.
    /// Layout: [refcount: 8 bytes][string data: N bytes]
    /// RocStr: { bytes: ptr to data, length: N, capacity: N }
    fn generateBigStrCode(self: *MonoExprCodeGen, str_text: []const u8) Error![]const u8 {
        const length = str_text.len;

        // Get the data pointer - either from interner or heap allocation
        const data_ptr: [*]const u8 = blk: {
            if (self.static_interner) |interner| {
                // Use the static data interner for proper static storage
                // String literals are interned and deduplicated
                const interned = interner.internString(str_text) orelse return error.OutOfMemory;
                break :blk interned.ptr;
            } else {
                // Fallback: heap allocation with static refcount (legacy behavior)
                // This path is used when no interner is configured
                const alloc_size = @sizeOf(usize) + length;
                const allocation = self.allocator.alloc(u8, alloc_size) catch return error.OutOfMemory;
                // Note: This memory is intentionally NOT freed here - it will be used by the returned RocStr
                // The refcount is set to static so Roc runtime won't try to free it either

                // Write static refcount at the start
                const refcount_ptr: *usize = @ptrCast(@alignCast(allocation.ptr));
                refcount_ptr.* = REFCOUNT_STATIC;

                // Copy string data after refcount
                const heap_data_ptr = allocation.ptr + @sizeOf(usize);
                @memcpy(heap_data_ptr[0..length], str_text);
                break :blk heap_data_ptr;
            }
        };

        // Build the RocStr structure:
        // bytes (8 bytes): pointer to data
        // length (8 bytes): string length
        // capacity_or_alloc_ptr (8 bytes): capacity (same as length for exact allocation)
        var roc_str_bytes: [ROCSTR_SIZE]u8 = undefined;
        const bytes_field: *u64 = @ptrCast(@alignCast(&roc_str_bytes[0]));
        const length_field: *u64 = @ptrCast(@alignCast(&roc_str_bytes[8]));
        const capacity_field: *u64 = @ptrCast(@alignCast(&roc_str_bytes[16]));

        bytes_field.* = @intFromPtr(data_ptr);
        length_field.* = length;
        capacity_field.* = length;

        // Generate code that writes these 24 bytes to the result pointer
        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64WriteBytes(&roc_str_bytes),
            .aarch64 => self.generateArm64WriteBytes(&roc_str_bytes),
            else => error.UnsupportedExpression,
        };
    }

    /// Generate code for a small string (≤23 bytes)
    /// Layout: [23 bytes of data][1 byte: len | 0x80]
    fn generateSmallStrCode(self: *MonoExprCodeGen, str_text: []const u8) Error![]const u8 {
        // Build the 24-byte small string representation
        var roc_str_bytes: [ROCSTR_SIZE]u8 = [_]u8{0} ** ROCSTR_SIZE;

        // Copy string data to bytes[0..len]
        @memcpy(roc_str_bytes[0..str_text.len], str_text);

        // Set length byte with high bit set to indicate small string
        roc_str_bytes[ROCSTR_SIZE - 1] = @as(u8, @intCast(str_text.len)) | SMALL_STR_MASK;

        // Generate code that writes these 24 bytes to the result pointer
        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64WriteBytes(&roc_str_bytes),
            .aarch64 => self.generateArm64WriteBytes(&roc_str_bytes),
            else => error.UnsupportedExpression,
        };
    }

    /// Generate x86_64 code to write 24 bytes to [rdi]
    fn generateX64WriteBytes(self: *MonoExprCodeGen, bytes: *const [ROCSTR_SIZE]u8) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Write 3 qwords (24 bytes) to [rdi], [rdi+8], [rdi+16]
        // movabs rax, qword0; mov [rdi], rax
        const qword0: u64 = @bitCast(bytes[0..8].*);
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(qword0)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x07 }); // mov [rdi], rax

        // movabs rax, qword1; mov [rdi+8], rax
        const qword1: u64 = @bitCast(bytes[8..16].*);
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(qword1)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x47, 0x08 }); // mov [rdi+8], rax

        // movabs rax, qword2; mov [rdi+16], rax
        const qword2: u64 = @bitCast(bytes[16..24].*);
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(qword2)));
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x47, 0x10 }); // mov [rdi+16], rax

        try code.append(0xC3); // ret
        return code.toOwnedSlice();
    }

    /// Generate ARM64 code to write 24 bytes to [x0]
    fn generateArm64WriteBytes(self: *MonoExprCodeGen, bytes: *const [ROCSTR_SIZE]u8) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Write 3 qwords to [x0], [x0+8], [x0+16]
        const qword0: u64 = @bitCast(bytes[0..8].*);
        const qword1: u64 = @bitCast(bytes[8..16].*);
        const qword2: u64 = @bitCast(bytes[16..24].*);

        // Load qword0 into x1, store to [x0]
        try self.generateArm64LoadImm64(&code, 1, qword0);
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000001)))); // str x1, [x0]

        // Load qword1 into x1, store to [x0, #8]
        try self.generateArm64LoadImm64(&code, 1, qword1);
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000401)))); // str x1, [x0, #8]

        // Load qword2 into x1, store to [x0, #16]
        try self.generateArm64LoadImm64(&code, 1, qword2);
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF9000801)))); // str x1, [x0, #16]

        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    /// Get a string literal from the mono store
    /// All string literals are copied to the mono store during lowering
    fn getStringLiteral(self: *MonoExprCodeGen, lit_idx: base.StringLiteral.Idx) []const u8 {
        return self.mono_store.getString(lit_idx);
    }

    // ========== str_inspekt Code Generation ==========
    //
    // These functions generate code for str_inspekt-related expressions.
    // Currently, they return UnsupportedExpression because generating actual
    // machine code for string operations requires runtime support (RocStr allocation,
    // concatenation, etc.) that isn't available in the dev backend yet.
    //
    // The str_inspekt lowering creates a tree of MonoExprs with all field names,
    // tag names, and structure baked in as string literals. Full code generation
    // would involve emitting calls to RocStr runtime functions.

    /// Generate code for string concatenation (compile-time)
    fn generateStrConcatCode(
        self: *MonoExprCodeGen,
        parts_span: MonoExprSpan,
        _: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const parts = self.mono_store.getExprSpan(parts_span);

        // Collect all string parts at compile time
        var total_len: usize = 0;
        var string_parts: std.ArrayListUnmanaged([]const u8) = .empty;
        defer string_parts.deinit(self.allocator);

        for (parts) |part_id| {
            const part_str = self.evalStringExpr(part_id, env) orelse return error.UnsupportedExpression;
            try string_parts.append(self.allocator, part_str);
            total_len += part_str.len;
        }

        // Concatenate all parts into a single string
        var result = try self.allocator.alloc(u8, total_len);
        defer self.allocator.free(result);

        var offset: usize = 0;
        for (string_parts.items) |part| {
            @memcpy(result[offset..][0..part.len], part);
            offset += part.len;
        }

        // Generate code for the concatenated string
        return self.generateRocStrCode(result);
    }

    /// Evaluate a string expression at compile time
    /// Returns null if the expression can't be evaluated to a string at compile time
    fn evalStringExpr(self: *MonoExprCodeGen, expr_id: MonoExprId, env: *MonoScope) ?[]const u8 {
        if (expr_id.isNone()) return null;
        const expr = self.mono_store.getExpr(expr_id);

        return switch (expr) {
            .str_literal => |lit_idx| self.getStringLiteral(lit_idx),
            .str_concat => |parts_span| blk: {
                const parts = self.mono_store.getExprSpan(parts_span);

                var total_len: usize = 0;
                var string_parts: std.ArrayListUnmanaged([]const u8) = .empty;
                defer string_parts.deinit(self.allocator);

                for (parts) |part_id| {
                    const part_str = self.evalStringExpr(part_id, env) orelse break :blk null;
                    string_parts.append(self.allocator, part_str) catch break :blk null;
                    total_len += part_str.len;
                }

                var result = self.allocator.alloc(u8, total_len) catch break :blk null;
                // Note: this leaks memory, but for compile-time evaluation it's acceptable
                var offset: usize = 0;
                for (string_parts.items) |part| {
                    @memcpy(result[offset..][0..part.len], part);
                    offset += part.len;
                }
                break :blk result;
            },
            .int_to_str => |info| blk: {
                // Evaluate the integer at compile time and format it
                const int_val = self.evalConstantI64(info.value, env) orelse break :blk null;
                // Format the integer as a string
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{int_val}) catch break :blk null;
                const result = self.allocator.dupe(u8, formatted) catch break :blk null;
                break :blk result;
            },
            .bool_literal => |val| blk: {
                // Format boolean as "Bool.true" or "Bool.false"
                break :blk if (val) "Bool.true" else "Bool.false";
            },
            .str_escape_and_quote => |inner_id| blk: {
                const inner_str = self.evalStringExpr(inner_id, env) orelse break :blk null;
                // Escape and quote the string
                const escaped = self.escapeAndQuoteString(inner_str) catch break :blk null;
                break :blk escaped;
            },
            .discriminant_switch => |sw| blk: {
                // Evaluate the discriminant and pick the right branch
                const disc = self.evalConstantI64(sw.value, env) orelse break :blk null;
                const branches = self.mono_store.getExprSpan(sw.branches);
                if (disc < 0 or @as(usize, @intCast(disc)) >= branches.len) break :blk null;
                const branch_id = branches[@intCast(disc)];
                break :blk self.evalStringExpr(branch_id, env);
            },
            else => null,
        };
    }

    /// Escape special characters in a string and wrap in quotes
    fn escapeAndQuoteString(self: *MonoExprCodeGen, str: []const u8) ![]const u8 {
        // Calculate the required buffer size
        var needed: usize = 2; // for the quotes
        for (str) |c| {
            needed += switch (c) {
                '\\', '"', '\n', '\r', '\t' => 2,
                else => 1,
            };
        }

        var result = try self.allocator.alloc(u8, needed);
        var i: usize = 0;
        result[i] = '"';
        i += 1;

        for (str) |c| {
            switch (c) {
                '\\' => {
                    result[i] = '\\';
                    result[i + 1] = '\\';
                    i += 2;
                },
                '"' => {
                    result[i] = '\\';
                    result[i + 1] = '"';
                    i += 2;
                },
                '\n' => {
                    result[i] = '\\';
                    result[i + 1] = 'n';
                    i += 2;
                },
                '\r' => {
                    result[i] = '\\';
                    result[i + 1] = 'r';
                    i += 2;
                },
                '\t' => {
                    result[i] = '\\';
                    result[i + 1] = 't';
                    i += 2;
                },
                else => {
                    result[i] = c;
                    i += 1;
                },
            }
        }

        result[i] = '"';
        return result;
    }

    /// Generate code for integer to string conversion (compile-time)
    fn generateIntToStrCode(
        self: *MonoExprCodeGen,
        info: anytype,
        _: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const int_val = self.evalConstantI64(info.value, env) orelse return error.UnsupportedExpression;
        var buf: [32]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf, "{d}", .{int_val}) catch return error.UnsupportedExpression;
        return self.generateRocStrCode(formatted);
    }

    /// Generate code for float to string conversion (compile-time)
    fn generateFloatToStrCode(
        self: *MonoExprCodeGen,
        info: anytype,
        _: LayoutIdx,
        _: *MonoScope,
    ) Error![]const u8 {
        // Try to evaluate the float at compile time
        if (info.value.isNone()) return error.UnsupportedExpression;
        const expr = self.mono_store.getExpr(info.value);

        const float_val: f64 = switch (expr) {
            .f64_literal => |v| v,
            .f32_literal => |v| @as(f64, v),
            else => return error.UnsupportedExpression,
        };

        var buf: [64]u8 = undefined;
        const formatted = std.fmt.bufPrint(&buf, "{d}", .{float_val}) catch return error.UnsupportedExpression;
        return self.generateRocStrCode(formatted);
    }

    /// Generate code for Dec to string conversion (compile-time)
    fn generateDecToStrCode(
        self: *MonoExprCodeGen,
        value_expr: MonoExprId,
        _: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const dec_val = self.evalConstantI128(value_expr, env) orelse return error.UnsupportedExpression;

        // Dec is stored as i128 with 18 decimal places
        const dec_scale: i128 = 1_000_000_000_000_000_000;

        // Format the decimal value
        var buf: [64]u8 = undefined;
        const sign: []const u8 = if (dec_val < 0) "-" else "";
        const abs_val: u128 = if (dec_val < 0) @intCast(-dec_val) else @intCast(dec_val);
        const whole = abs_val / @as(u128, @intCast(dec_scale));
        const frac = abs_val % @as(u128, @intCast(dec_scale));

        const formatted = if (frac == 0)
            std.fmt.bufPrint(&buf, "{s}{d}", .{ sign, whole }) catch return error.UnsupportedExpression
        else
            std.fmt.bufPrint(&buf, "{s}{d}.{d:0>18}", .{ sign, whole, frac }) catch return error.UnsupportedExpression;

        // Trim trailing zeros after decimal point
        var trimmed = formatted;
        if (std.mem.indexOf(u8, trimmed, ".")) |_| {
            while (trimmed.len > 0 and trimmed[trimmed.len - 1] == '0') {
                trimmed = trimmed[0 .. trimmed.len - 1];
            }
            if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '.') {
                trimmed = trimmed[0 .. trimmed.len - 1];
            }
        }

        return self.generateRocStrCode(trimmed);
    }

    /// Generate code for string escaping and quoting (compile-time)
    fn generateStrEscapeAndQuoteCode(
        self: *MonoExprCodeGen,
        inner_expr: MonoExprId,
        _: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        const inner_str = self.evalStringExpr(inner_expr, env) orelse return error.UnsupportedExpression;
        const escaped = self.escapeAndQuoteString(inner_str) catch return error.OutOfMemory;
        return self.generateRocStrCode(escaped);
    }

    /// Generate code for discriminant switch
    fn generateDiscriminantSwitchCode(
        self: *MonoExprCodeGen,
        sw: anytype,
        result_layout: LayoutIdx,
        env: *MonoScope,
    ) Error![]const u8 {
        // Try to evaluate the discriminant value at compile time
        const discriminant = self.evalConstantI64(sw.value, env) orelse return error.UnsupportedExpression;

        // Get the appropriate branch
        const branches = self.mono_store.getExprSpan(sw.branches);
        if (discriminant < 0 or @as(usize, @intCast(discriminant)) >= branches.len) {
            return error.UnsupportedExpression;
        }

        const branch_id = branches[@intCast(discriminant)];
        return self.generateCodeForExpr(branch_id, result_layout, env);
    }

    // ========== Reference Counting Operations ==========
    //
    // RC operations (incref/decref) MUST work properly during codegen because:
    // - Roc's opportunistic in-place mutation relies on checking runtime reference counts
    // - If refcount == 1, the value is unique and can be mutated in place
    // - If refcount > 1, the value is shared and must be copied before mutation
    //
    // Only free() should be a no-op (arena manages actual deallocation).
    //
    // Implementation:
    // - Generated code receives result_ptr in rdi/x0 and roc_ops in rsi/x1
    // - For RC operations, we evaluate the value expression to get the data pointer
    // - Then call the appropriate builtin RC function with the roc_ops
    //
    // Memory layout for refcounted values:
    // [REFCOUNT: isize] [DATA...]
    //        ^              ^
    //        |              +-- data pointer points here
    //        +-- refcount at (data_ptr - sizeof(usize))
    //
    // REFCOUNT_STATIC_DATA (0) marks static data that RC ops skip.

    /// Generate code for incref operation.
    /// Increments the reference count of a refcounted value.
    /// Skips values with REFCOUNT_STATIC_DATA (0) as they are static.
    ///
    /// Note: Falls back to no-op if:
    /// - The builtin incref function can't be resolved
    /// - The value expression can't be generated (unsupported expression type)
    /// This is acceptable because:
    /// - Static data has REFCOUNT_STATIC_DATA (0) and RC ops are no-ops for it
    /// - If we can't generate the value expression, we'd fail elsewhere anyway
    fn generateIncrefCode(
        self: *MonoExprCodeGen,
        rc: anytype,
        env: *MonoScope,
    ) Error![]const u8 {
        const dev = @import("mod.zig");

        // Get the address of the builtin incref function
        const incref_addr = dev.resolveBuiltinFunction("incref_data_ptr") orelse {
            // No builtin available - fall back to no-op
            return self.generateNoOpCode();
        };

        // Generate code for the value expression (writes to temp location)
        const value_code = self.generateCodeForExpr(rc.value, rc.layout_idx, env) catch {
            // Can't generate value expression - fall back to no-op
            // This typically means the expression type isn't supported yet
            return self.generateNoOpCode();
        };
        defer self.allocator.free(value_code);

        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64IncrefCall(value_code, rc.count, incref_addr),
            .aarch64 => self.generateArm64IncrefCall(value_code, rc.count, incref_addr),
            else => self.generateNoOpCode(),
        };
    }

    /// Generate code for decref operation.
    /// Decrements the reference count and calls free when it reaches 0.
    /// Skips values with REFCOUNT_STATIC_DATA (0) as they are static.
    ///
    /// Note: Falls back to no-op if:
    /// - The builtin decref function can't be resolved
    /// - The value expression can't be generated (unsupported expression type)
    fn generateDecrefCode(
        self: *MonoExprCodeGen,
        rc: anytype,
        env: *MonoScope,
    ) Error![]const u8 {
        const dev = @import("mod.zig");

        // Get the address of the builtin decref function
        const decref_addr = dev.resolveBuiltinFunction("decref_data_ptr") orelse {
            // No builtin available - fall back to no-op
            return self.generateNoOpCode();
        };

        // Generate code for the value expression (writes to temp location)
        const value_code = self.generateCodeForExpr(rc.value, rc.layout_idx, env) catch {
            // Can't generate value expression - fall back to no-op
            return self.generateNoOpCode();
        };
        defer self.allocator.free(value_code);

        // For decref, we need alignment and elements_refcounted from layout
        // Default to 8-byte alignment and non-refcounted elements for now
        const alignment: u32 = 8;
        const elements_refcounted: bool = false;

        return switch (builtin.cpu.arch) {
            .x86_64 => self.generateX64DecrefCall(value_code, alignment, elements_refcounted, decref_addr),
            .aarch64 => self.generateArm64DecrefCall(value_code, alignment, elements_refcounted, decref_addr),
            else => self.generateNoOpCode(),
        };
    }

    /// Generate x86_64 code to call increfDataPtrC.
    /// The generated function receives: rdi=result_ptr, rsi=roc_ops
    /// increfDataPtrC signature: (bytes_or_null: ?[*]u8, inc_amount: isize, roc_ops: *RocOps)
    fn generateX64IncrefCall(
        self: *MonoExprCodeGen,
        value_code: []const u8,
        count: u32,
        incref_addr: usize,
    ) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Prologue: set up stack frame and save registers
        try code.appendSlice(&[_]u8{ 0x55 }); // push rbp
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0xE5 }); // mov rbp, rsp
        try code.appendSlice(&[_]u8{ 0x48, 0x83, 0xEC, 0x40 }); // sub rsp, 64 (maintain 16-byte alignment)
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x7D, 0xF8 }); // mov [rbp-8], rdi (save result_ptr)
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x75, 0xF0 }); // mov [rbp-16], rsi (save roc_ops)

        // Set rdi to stack location for temp storage of value expression result
        try code.appendSlice(&[_]u8{ 0x48, 0x8D, 0x7D, 0xD0 }); // lea rdi, [rbp-48]

        // Inline the value code (strip trailing ret if present)
        if (value_code.len > 0 and value_code[value_code.len - 1] == 0xC3) {
            try code.appendSlice(value_code[0 .. value_code.len - 1]);
        } else {
            try code.appendSlice(value_code);
        }

        // Now [rbp-48] contains the data structure (e.g., RocStr)
        // The data pointer (bytes field for RocStr) is at offset 0
        // Call increfDataPtrC(data_ptr, inc_amount, roc_ops)

        // arg1: data_ptr - load the bytes pointer from the first qword
        try code.appendSlice(&[_]u8{ 0x48, 0x8B, 0x7D, 0xD0 }); // mov rdi, [rbp-48]

        // arg2: inc_amount (as isize)
        try code.appendSlice(&[_]u8{ 0xBE }); // mov esi, imm32 (zero-extends to rsi)
        try code.appendSlice(&@as([4]u8, @bitCast(count)));

        // arg3: roc_ops
        try code.appendSlice(&[_]u8{ 0x48, 0x8B, 0x55, 0xF0 }); // mov rdx, [rbp-16]

        // Load function address and call
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(@as(u64, incref_addr))));
        try code.appendSlice(&[_]u8{ 0xFF, 0xD0 }); // call rax

        // Epilogue
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0xEC }); // mov rsp, rbp
        try code.appendSlice(&[_]u8{ 0x5D }); // pop rbp
        try code.appendSlice(&[_]u8{ 0xC3 }); // ret

        return code.toOwnedSlice();
    }

    /// Generate x86_64 code to call decrefDataPtrC.
    /// The generated function receives: rdi=result_ptr, rsi=roc_ops
    /// decrefDataPtrC signature: (bytes_or_null: ?[*]u8, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps)
    fn generateX64DecrefCall(
        self: *MonoExprCodeGen,
        value_code: []const u8,
        alignment: u32,
        elements_refcounted: bool,
        decref_addr: usize,
    ) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Prologue: set up stack frame and save registers
        try code.appendSlice(&[_]u8{ 0x55 }); // push rbp
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0xE5 }); // mov rbp, rsp
        try code.appendSlice(&[_]u8{ 0x48, 0x83, 0xEC, 0x40 }); // sub rsp, 64
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x7D, 0xF8 }); // mov [rbp-8], rdi (save result_ptr)
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0x75, 0xF0 }); // mov [rbp-16], rsi (save roc_ops)

        // Set rdi to stack location for temp storage
        try code.appendSlice(&[_]u8{ 0x48, 0x8D, 0x7D, 0xD0 }); // lea rdi, [rbp-48]

        // Inline the value code (strip trailing ret if present)
        if (value_code.len > 0 and value_code[value_code.len - 1] == 0xC3) {
            try code.appendSlice(value_code[0 .. value_code.len - 1]);
        } else {
            try code.appendSlice(value_code);
        }

        // Call decrefDataPtrC(data_ptr, alignment, elements_refcounted, roc_ops)

        // arg1: data_ptr
        try code.appendSlice(&[_]u8{ 0x48, 0x8B, 0x7D, 0xD0 }); // mov rdi, [rbp-48]

        // arg2: alignment
        try code.appendSlice(&[_]u8{ 0xBE }); // mov esi, imm32
        try code.appendSlice(&@as([4]u8, @bitCast(alignment)));

        // arg3: elements_refcounted (bool as u8)
        const elem_refcounted_byte: u8 = if (elements_refcounted) 1 else 0;
        try code.appendSlice(&[_]u8{ 0xB2, elem_refcounted_byte }); // mov dl, imm8

        // arg4: roc_ops
        try code.appendSlice(&[_]u8{ 0x48, 0x8B, 0x4D, 0xF0 }); // mov rcx, [rbp-16]

        // Load function address and call
        try code.appendSlice(&[_]u8{ 0x48, 0xB8 }); // movabs rax, imm64
        try code.appendSlice(&@as([8]u8, @bitCast(@as(u64, decref_addr))));
        try code.appendSlice(&[_]u8{ 0xFF, 0xD0 }); // call rax

        // Epilogue
        try code.appendSlice(&[_]u8{ 0x48, 0x89, 0xEC }); // mov rsp, rbp
        try code.appendSlice(&[_]u8{ 0x5D }); // pop rbp
        try code.appendSlice(&[_]u8{ 0xC3 }); // ret

        return code.toOwnedSlice();
    }

    /// Generate ARM64 code to call increfDataPtrC.
    /// The generated function receives: x0=result_ptr, x1=roc_ops
    fn generateArm64IncrefCall(
        self: *MonoExprCodeGen,
        value_code: []const u8,
        count: u32,
        incref_addr: usize,
    ) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Prologue: save frame pointer, link register, and callee-saved registers
        // stp x29, x30, [sp, #-16]!
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA9BF7BFD))));
        // mov x29, sp
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910003FD))));
        // stp x19, x20, [sp, #-16]!
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA9BF53F3))));
        // mov x19, x0 (save result_ptr)
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA0003F3))));
        // mov x20, x1 (save roc_ops)
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA0103F4))));
        // sub sp, sp, #64
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD10103FF))));

        // Set x0 to stack location for temp storage
        // mov x0, sp
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910003E0))));

        // Inline the value code (strip trailing ret if present)
        // ARM64 ret is 0xD65F03C0
        const ret_instr: [4]u8 = @bitCast(@as(u32, 0xD65F03C0));
        if (value_code.len >= 4 and std.mem.eql(u8, value_code[value_code.len - 4 ..], &ret_instr)) {
            try code.appendSlice(value_code[0 .. value_code.len - 4]);
        } else {
            try code.appendSlice(value_code);
        }

        // Call increfDataPtrC(data_ptr, inc_amount, roc_ops)

        // arg1 (x0): data_ptr - load from stack
        // ldr x0, [sp]
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF94003E0))));

        // arg2 (x1): inc_amount
        try self.generateArm64LoadImm64(&code, 1, @as(u64, count));

        // arg3 (x2): roc_ops
        // mov x2, x20
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA1403E2))));

        // Load function address and call
        try self.generateArm64LoadImm64(&code, 8, incref_addr);
        // blr x8
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD63F0100))));

        // Epilogue
        // add sp, sp, #64
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910103FF))));
        // ldp x19, x20, [sp], #16
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA8C153F3))));
        // ldp x29, x30, [sp], #16
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA8C17BFD))));
        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    /// Generate ARM64 code to call decrefDataPtrC.
    /// The generated function receives: x0=result_ptr, x1=roc_ops
    fn generateArm64DecrefCall(
        self: *MonoExprCodeGen,
        value_code: []const u8,
        alignment: u32,
        elements_refcounted: bool,
        decref_addr: usize,
    ) Error![]const u8 {
        var code = std.array_list.Managed(u8).init(self.allocator);
        errdefer code.deinit();

        // Prologue
        // stp x29, x30, [sp, #-16]!
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA9BF7BFD))));
        // mov x29, sp
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910003FD))));
        // stp x19, x20, [sp, #-16]!
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA9BF53F3))));
        // mov x19, x0 (save result_ptr)
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA0003F3))));
        // mov x20, x1 (save roc_ops)
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA0103F4))));
        // sub sp, sp, #64
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD10103FF))));

        // Set x0 to stack location
        // mov x0, sp
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910003E0))));

        // Inline the value code
        const ret_instr: [4]u8 = @bitCast(@as(u32, 0xD65F03C0));
        if (value_code.len >= 4 and std.mem.eql(u8, value_code[value_code.len - 4 ..], &ret_instr)) {
            try code.appendSlice(value_code[0 .. value_code.len - 4]);
        } else {
            try code.appendSlice(value_code);
        }

        // Call decrefDataPtrC(data_ptr, alignment, elements_refcounted, roc_ops)

        // arg1 (x0): data_ptr
        // ldr x0, [sp]
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xF94003E0))));

        // arg2 (w1): alignment
        try self.generateArm64LoadImm64(&code, 1, @as(u64, alignment));

        // arg3 (w2): elements_refcounted
        const elem_refcounted_val: u64 = if (elements_refcounted) 1 else 0;
        try self.generateArm64LoadImm64(&code, 2, elem_refcounted_val);

        // arg4 (x3): roc_ops
        // mov x3, x20
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xAA1403E3))));

        // Load function address and call
        try self.generateArm64LoadImm64(&code, 8, decref_addr);
        // blr x8
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD63F0100))));

        // Epilogue
        // add sp, sp, #64
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0x910103FF))));
        // ldp x19, x20, [sp], #16
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA8C153F3))));
        // ldp x29, x30, [sp], #16
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xA8C17BFD))));
        // ret
        try code.appendSlice(&@as([4]u8, @bitCast(@as(u32, 0xD65F03C0))));

        return code.toOwnedSlice();
    }

    /// Generate machine code that does nothing (just returns).
    /// Used for no-op expressions like RC operations in compile-time evaluation.
    fn generateNoOpCode(self: *MonoExprCodeGen) Error![]const u8 {
        return switch (builtin.cpu.arch) {
            .x86_64 => blk: {
                // Just return immediately - x86_64: ret
                const code = try self.allocator.alloc(u8, 1);
                code[0] = 0xC3; // ret
                break :blk code;
            },
            .aarch64 => blk: {
                // Just return immediately - ARM64: ret
                const code = try self.allocator.alloc(u8, 4);
                const ret_instr: u32 = 0xD65F03C0;
                @as(*[4]u8, @ptrCast(code.ptr)).* = @bitCast(ret_instr);
                break :blk code;
            },
            else => error.UnsupportedExpression,
        };
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
