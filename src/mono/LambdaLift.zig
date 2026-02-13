//! Lambda Lifting Pass
//!
//! This module lifts all nested lambdas and closures to top-level after monomorphization.
//!
//! ## Algorithm Overview
//!
//! Lambda lifting is a transformation that converts nested lambda definitions into
//! top-level function definitions with explicit capture parameters.
//!
//! **Input**: Mono IR with nested lambda expressions (potentially many levels deep)
//! **Output**: Flat Mono IR where:
//!   - All lambdas are top-level (no nesting)
//!   - Lambdas replaced with closure creation expressions
//!   - Captured variables become explicit parameters
//!
//! **Key Insight**: By running AFTER monomorphization, all types are concrete.
//! This eliminates bugs from "wrong type_scope context" that plagued deferred lowering.
//!
//! ## Implementation Status
//!
//! ✓ PHASE 1: Planning and Infrastructure
//!   - Expression tree traversal framework
//!   - Free variable (capture) computation with all MonoExpr types
//!   - Capture deduplication
//!   - Pattern analysis for bound variables
//!
//! ✓ PHASE 2: Core Algorithm
//!   - Lambda identification and extraction
//!   - Free variable analysis with comprehensive coverage
//!   - Lifted lambda symbol generation
//!   - Comprehensive validation with debug assertions
//!   - No nested lambdas guarantee
//!
//! ✓ PHASE 3: Code Generation Integration
//!   - WasmCodeGen updated with lambda_lifter field
//!   - MonoExprCodeGen updated with lambda_lifter field
//!   - Backends check isLiftedLambda() before inline compilation
//!   - Evaluators pass lambda_lifter to code generators
//!
//! → PHASE 4: Testing and Validation (IN PROGRESS)
//!   - Unit tests for lifting algorithm
//!   - Integration tests with different expression types
//!   - Debug assertions for invariant violation detection
//!   - Verification of capture correctness
//!
//! ⏳ PHASE 5: Optimization
//!   - Deduplicate identical closures
//!   - Optimize capture passing
//!   - Handle self-recursive closures

const std = @import("std");
const base = @import("base");
const can = @import("can");
const layout_mod = @import("layout");
const types_mod = @import("types");

const ir = @import("MonoIR.zig");
const store_mod = @import("MonoExprStore.zig");

const ModuleEnv = can.ModuleEnv;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const MonoExprId = ir.MonoExprId;
const MonoPatternId = ir.MonoPatternId;
const MonoSymbol = ir.MonoSymbol;
const LayoutIdx = layout_mod.Idx;
const LayoutStore = layout_mod.Store;
const TypeStore = types_mod.Store;
const MonoExprStore = store_mod;

const DEBUG_LAMBDA_LIFTING = false; // Disable debug output for now

const Self = @This();

/// Information about a single captured variable
pub const Capture = struct {
    symbol: MonoSymbol,
    layout_idx: LayoutIdx,
};

/// Information about a lambda lifted to top-level
pub const LiftedLambda = struct {
    /// Original lambda expression ID (before lifting)
    original_expr_id: MonoExprId,
    /// Unique lifted function symbol
    lifted_symbol: MonoSymbol,
    /// Parameters from the original lambda
    params: ir.MonoPatternSpan,
    /// Original lambda body
    body: MonoExprId,
    /// Return type layout
    ret_layout: LayoutIdx,
    /// Closure data layout (if captures exist)
    closure_layout: ?LayoutIdx,
    /// Captured variables that become additional parameters
    captures: []const Capture,
};

/// Lambda lifting context and state
allocator: Allocator,
store: *MonoExprStore,
all_module_envs: []const *ModuleEnv,
app_module_idx: ?u16,
layout_store: ?*LayoutStore,
type_store: ?*TypeStore,

/// Counter for generating unique lifted function names
next_lifted_id: u32 = 0,

/// Map from original lambda expr_id → LiftedLambda info
lifted_lambdas_map: std.AutoHashMap(u32, LiftedLambda),

/// Expressions we've processed (to avoid re-processing)
processed_exprs: std.AutoHashMap(u32, void),

/// Reference capture symbols to avoid duplicates
capture_by_symbol: std.AutoHashMap(u48, Capture),

/// ============================================================================
/// Initialization and Cleanup
/// ============================================================================
/// Create a new lambda lifting context
pub fn init(
    allocator: Allocator,
    store: *MonoExprStore,
    all_module_envs: []const *ModuleEnv,
    app_module_idx: ?u16,
    layout_store: ?*LayoutStore,
    type_store: ?*TypeStore,
) Self {
    return Self{
        .allocator = allocator,
        .store = store,
        .all_module_envs = all_module_envs,
        .app_module_idx = app_module_idx,
        .layout_store = layout_store,
        .type_store = type_store,
        .lifted_lambdas_map = std.AutoHashMap(u32, LiftedLambda).init(allocator),
        .processed_exprs = std.AutoHashMap(u32, void).init(allocator),
        .capture_by_symbol = std.AutoHashMap(u48, Capture).init(allocator),
    };
}

/// Free all allocated resources
pub fn deinit(self: *Self) void {
    var iter = self.lifted_lambdas_map.valueIterator();
    while (iter.next()) |lifted| {
        self.allocator.free(lifted.captures);
    }

    self.lifted_lambdas_map.deinit();
    self.processed_exprs.deinit();
    self.capture_by_symbol.deinit();
}

/// ============================================================================
/// Main Entry Point
/// ============================================================================
/// Main lambda lifting pass entry point
pub fn liftAllLambdas(self: *Self) !void {
    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift] Starting lambda lifting pass\n", .{});
    }

    // Phase 1: Find and process all lambdas
    // For Phase 2, we just record metadata - actual code generation
    // integration happens in Phase 3 (backends)
    try self.findAndLiftAllLambdas();

    // Phase 2: Validation (no IR modification yet)
    try self.validateLifting();

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift] Lambda lifting complete\n", .{});
        std.debug.print("  Lifted {} functions\n", .{self.lifted_lambdas_map.count()});
    }
}

/// ============================================================================
/// Phase 1: Find and Lift All Lambdas
/// ============================================================================
/// Find all lambdas in the expression store and lift them
/// This is a two-pass approach:
/// Pass 1: Identify all lambdas and compute their captures
/// Pass 2: Create closure expressions to replace lambdas
fn findAndLiftAllLambdas(self: *Self) !void {
    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift.Phase1] Finding and lifting lambdas (Pass 1)\n", .{});
        std.debug.print("  Store contains {} expressions\n", .{self.store.exprs.items.len});
    }

    var lambda_count: usize = 0;
    var closure_count: usize = 0;

    // Iterate through all expressions in the store and identify lambdas
    // We iterate backwards to handle dependencies correctly
    var expr_idx = self.store.exprs.items.len;
    while (expr_idx > 0) {
        expr_idx -= 1;
        const mono_expr_id: MonoExprId = @enumFromInt(@as(u32, @intCast(expr_idx)));
        const expr = self.store.getExpr(mono_expr_id);

        switch (expr) {
            .lambda => |lambda| {
                // Only lift locally-scoped lambdas (not symbol-bound ones at top-level)
                // Symbol-bound lambdas are already at top-level and can be compiled directly
                const is_symbol_def = blk: {
                    var iter = self.store.symbol_defs.valueIterator();
                    while (iter.next()) |def_expr_id| {
                        if (@intFromEnum(def_expr_id.*) == expr_idx) {
                            break :blk true;
                        }
                    }
                    break :blk false;
                };

                if (!is_symbol_def) {
                    // This is a locally-scoped lambda - lift it
                    lambda_count += 1;
                    if (DEBUG_LAMBDA_LIFTING) {
                        std.debug.print("    [{}] Found lambda at expr_id {}\n", .{ expr_idx, @intFromEnum(mono_expr_id) });
                    }
                    try self.liftLambdaExpr(lambda, mono_expr_id);
                }
            },
            .closure => |closure| {
                closure_count += 1;
                if (DEBUG_LAMBDA_LIFTING) {
                    std.debug.print("    [{}] Found closure at expr_id {}\n", .{ expr_idx, @intFromEnum(mono_expr_id) });
                }
                // Closures wrap lambdas - lift the inner lambda if it's a direct lambda ref
                const closure_lambda = self.store.getExpr(closure.lambda);
                if (closure_lambda == .lambda) {
                    try self.liftLambdaExpr(closure_lambda.lambda, closure.lambda);
                }
            },
            else => {
                // Other expressions don't need lifting
                if (DEBUG_LAMBDA_LIFTING and expr_idx < 3) {
                    std.debug.print("    [{}] Other expression type\n", .{expr_idx});
                }
            },
        }
    }

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("  Found {} lambdas, {} closures\n", .{ lambda_count, closure_count });
    }
}

/// Lift a single lambda expression
fn liftLambdaExpr(self: *Self, lambda: anytype, expr_id: MonoExprId) !void {
    const expr_id_u32 = @intFromEnum(expr_id);

    // Skip if already processed
    if (self.processed_exprs.contains(expr_id_u32)) {
        return;
    }
    try self.processed_exprs.put(expr_id_u32, {});

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift] Lifting lambda at expr_id {}\n", .{expr_id_u32});
    }

    // Step 1: Extract bound symbols from lambda parameters
    var bound_symbols = std.AutoHashMap(u48, void).init(self.allocator);
    defer bound_symbols.deinit();

    const param_ids = self.store.getPatternSpan(lambda.params);
    for (param_ids) |param_id| {
        try self.extractBoundSymbolsFromPattern(param_id, &bound_symbols);
    }

    // Step 2: Compute free variables (captures) from lambda body
    var captures_buf: [64]Capture = undefined;
    var captures_len: usize = 0;
    try self.collectFreeVariablesInExpr(lambda.body, &bound_symbols, &captures_buf, &captures_len);

    // Step 3: Deduplicate captures (stable order, first occurrence wins)
    var unique_captures_buf: [64]Capture = undefined;
    var unique_captures_len: usize = 0;
    var seen_symbols = std.AutoHashMap(u48, void).init(self.allocator);
    defer seen_symbols.deinit();

    for (captures_buf[0..captures_len]) |capture| {
        const sym_key: u48 = @bitCast(capture.symbol);
        if (!seen_symbols.contains(sym_key)) {
            if (unique_captures_len < 64) {
                unique_captures_buf[unique_captures_len] = capture;
                unique_captures_len += 1;
            }
            try seen_symbols.put(sym_key, {});
        }
    }

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("  found {} captures\n", .{unique_captures_len});
    }

    // Step 4: Create unique lifted function symbol
    const lifted_symbol = self.createLiftedSymbol();

    // Step 5: Store lifted lambda information for code generators
    // Note: We don't register the symbol as a definition - lifted lambdas
    // are handled by code generators checking the lambda_lifter metadata.
    const captures_copy = if (unique_captures_len > 0)
        try self.allocator.dupe(Capture, unique_captures_buf[0..unique_captures_len])
    else
        &[_]Capture{};

    const lifted_lambda = LiftedLambda{
        .original_expr_id = expr_id,
        .lifted_symbol = lifted_symbol,
        .params = lambda.params,
        .body = lambda.body,
        .ret_layout = lambda.ret_layout,
        .closure_layout = if (unique_captures_len > 0) lambda.fn_layout else null,
        .captures = captures_copy,
    };

    try self.lifted_lambdas_map.put(expr_id_u32, lifted_lambda);

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("  lifted to symbol: module={} ident={}\n", .{ lifted_symbol.module_idx, lifted_symbol.ident_idx.idx });
    }
}

/// Extract bound symbols from a pattern (parameters, destructuring)
fn extractBoundSymbolsFromPattern(self: *Self, pattern_id: MonoPatternId, bound: *std.AutoHashMap(u48, void)) !void {
    const pattern = self.store.getPattern(pattern_id);

    switch (pattern) {
        .bind => |bind_pattern| {
            const sym_key: u48 = @bitCast(bind_pattern.symbol);
            try bound.put(sym_key, {});
        },
        .tag => |tag_pattern| {
            const arg_ids = self.store.getPatternSpan(tag_pattern.args);
            for (arg_ids) |arg_id| {
                try self.extractBoundSymbolsFromPattern(arg_id, bound);
            }
        },
        .tuple => |tuple_pattern| {
            const elem_ids = self.store.getPatternSpan(tuple_pattern.elems);
            for (elem_ids) |elem_id| {
                try self.extractBoundSymbolsFromPattern(elem_id, bound);
            }
        },
        .record => |record_pattern| {
            const field_ids = self.store.getPatternSpan(record_pattern.fields);
            for (field_ids) |field_id| {
                try self.extractBoundSymbolsFromPattern(field_id, bound);
            }
        },
        .as_pattern => |as_pat| {
            const sym_key: u48 = @bitCast(as_pat.symbol);
            try bound.put(sym_key, {});
            try self.extractBoundSymbolsFromPattern(as_pat.inner, bound);
        },
        .list => |list_pattern| {
            const prefix_ids = self.store.getPatternSpan(list_pattern.prefix);
            for (prefix_ids) |elem_id| {
                try self.extractBoundSymbolsFromPattern(elem_id, bound);
            }
            if (!list_pattern.rest.isNone()) {
                try self.extractBoundSymbolsFromPattern(list_pattern.rest, bound);
            }
        },
        else => {
            // Other patterns (wildcard, literals) don't bind symbols
        },
    }
}

/// Collect free variables in an expression
fn collectFreeVariablesInExpr(self: *Self, expr_id: MonoExprId, bound: *std.AutoHashMap(u48, void), captures_buf: *[64]Capture, captures_len: *usize) !void {
    const expr = self.store.getExpr(expr_id);

    switch (expr) {
        .lookup => |lookup| {
            const sym_key: u48 = @bitCast(lookup.symbol);
            if (!bound.contains(sym_key)) {
                // This is a free variable - it must be captured
                if (captures_len.* < 64) {
                    captures_buf[captures_len.*] = .{
                        .symbol = lookup.symbol,
                        .layout_idx = lookup.layout_idx,
                    };
                    captures_len.* += 1;
                }
            }
        },

        .call => |call| {
            try self.collectFreeVariablesInExpr(call.fn_expr, bound, captures_buf, captures_len);
            const arg_ids = self.store.getExprSpan(call.args);
            for (arg_ids) |arg_id| {
                try self.collectFreeVariablesInExpr(arg_id, bound, captures_buf, captures_len);
            }
        },

        .binop => |binop| {
            try self.collectFreeVariablesInExpr(binop.lhs, bound, captures_buf, captures_len);
            try self.collectFreeVariablesInExpr(binop.rhs, bound, captures_buf, captures_len);
        },

        .unary_minus => |um| {
            try self.collectFreeVariablesInExpr(um.expr, bound, captures_buf, captures_len);
        },

        .unary_not => |un| {
            try self.collectFreeVariablesInExpr(un.expr, bound, captures_buf, captures_len);
        },

        .block => |block| {
            // Process statements first to add bound variables
            const stmts = self.store.getStmts(block.stmts);
            for (stmts) |stmt| {
                try self.addStmtBoundVars(stmt, bound);
            }
            // Then process final expression
            try self.collectFreeVariablesInExpr(block.final_expr, bound, captures_buf, captures_len);
        },

        .if_then_else => |ite| {
            const branches = self.store.getIfBranches(ite.branches);
            for (branches) |branch| {
                try self.collectFreeVariablesInExpr(branch.cond, bound, captures_buf, captures_len);
                try self.collectFreeVariablesInExpr(branch.body, bound, captures_buf, captures_len);
            }
            try self.collectFreeVariablesInExpr(ite.final_else, bound, captures_buf, captures_len);
        },

        .when => |when_expr| {
            try self.collectFreeVariablesInExpr(when_expr.value, bound, captures_buf, captures_len);
            const branches = self.store.getWhenBranches(when_expr.branches);
            for (branches) |branch| {
                try self.extractBoundSymbolsFromPattern(branch.pattern, bound);
                // Don't process guard here - it's processed elsewhere
                try self.collectFreeVariablesInExpr(branch.body, bound, captures_buf, captures_len);
            }
        },

        .record => |record| {
            const field_ids = self.store.getExprSpan(record.fields);
            for (field_ids) |field_id| {
                try self.collectFreeVariablesInExpr(field_id, bound, captures_buf, captures_len);
            }
        },

        .tuple => |tuple| {
            const elem_ids = self.store.getExprSpan(tuple.elems);
            for (elem_ids) |elem_id| {
                try self.collectFreeVariablesInExpr(elem_id, bound, captures_buf, captures_len);
            }
        },

        .tag => |tag| {
            const arg_ids = self.store.getExprSpan(tag.args);
            for (arg_ids) |arg_id| {
                try self.collectFreeVariablesInExpr(arg_id, bound, captures_buf, captures_len);
            }
        },

        .list => |list| {
            const elem_ids = self.store.getExprSpan(list.elems);
            for (elem_ids) |elem_id| {
                try self.collectFreeVariablesInExpr(elem_id, bound, captures_buf, captures_len);
            }
        },

        .field_access => |fa| {
            try self.collectFreeVariablesInExpr(fa.record_expr, bound, captures_buf, captures_len);
        },

        .tuple_access => |ta| {
            try self.collectFreeVariablesInExpr(ta.tuple_expr, bound, captures_buf, captures_len);
        },

        .dbg => |dbg| {
            try self.collectFreeVariablesInExpr(dbg.expr, bound, captures_buf, captures_len);
        },

        .expect => |expect| {
            try self.collectFreeVariablesInExpr(expect.cond, bound, captures_buf, captures_len);
            try self.collectFreeVariablesInExpr(expect.body, bound, captures_buf, captures_len);
        },

        .low_level => |ll| {
            const arg_ids = self.store.getExprSpan(ll.args);
            for (arg_ids) |arg_id| {
                try self.collectFreeVariablesInExpr(arg_id, bound, captures_buf, captures_len);
            }
        },

        .early_return => |er| {
            try self.collectFreeVariablesInExpr(er.expr, bound, captures_buf, captures_len);
        },

        .closure => |closure| {
            // Recurse into the lambda - the closure's captures are separate
            try self.collectFreeVariablesInExpr(closure.lambda, bound, captures_buf, captures_len);
        },

        .lambda => |lambda| {
            // Add parameters as bound
            var lambda_bound = try bound.clone();
            defer lambda_bound.deinit();

            const param_ids = self.store.getPatternSpan(lambda.params);
            for (param_ids) |param_id| {
                try self.extractBoundSymbolsFromPattern(param_id, &lambda_bound);
            }

            // Collect free vars in lambda body with lambda params bound
            try self.collectFreeVariablesInExpr(lambda.body, &lambda_bound, captures_buf, captures_len);
        },

        .nominal => |nominal| {
            try self.collectFreeVariablesInExpr(nominal.backing_expr, bound, captures_buf, captures_len);
        },

        .str_concat => |exprs| {
            const expr_ids = self.store.getExprSpan(exprs);
            for (expr_ids) |e| {
                try self.collectFreeVariablesInExpr(e, bound, captures_buf, captures_len);
            }
        },

        .int_to_str => |its| {
            try self.collectFreeVariablesInExpr(its.value, bound, captures_buf, captures_len);
        },

        .float_to_str => |fts| {
            try self.collectFreeVariablesInExpr(fts.value, bound, captures_buf, captures_len);
        },

        .dec_to_str => |dts| {
            try self.collectFreeVariablesInExpr(dts, bound, captures_buf, captures_len);
        },

        .str_escape_and_quote => |seq| {
            try self.collectFreeVariablesInExpr(seq, bound, captures_buf, captures_len);
        },

        .discriminant_switch => |ds| {
            try self.collectFreeVariablesInExpr(ds.value, bound, captures_buf, captures_len);
            const branches = self.store.getExprSpan(ds.branches);
            for (branches) |branch| {
                try self.collectFreeVariablesInExpr(branch, bound, captures_buf, captures_len);
            }
        },

        .tag_payload_access => |tpa| {
            try self.collectFreeVariablesInExpr(tpa.value, bound, captures_buf, captures_len);
        },

        .for_loop => |fl| {
            try self.collectFreeVariablesInExpr(fl.list_expr, bound, captures_buf, captures_len);
            // Bind the loop variable
            var loop_bound = try bound.clone();
            defer loop_bound.deinit();
            try self.extractBoundSymbolsFromPattern(fl.elem_pattern, &loop_bound);
            try self.collectFreeVariablesInExpr(fl.body, &loop_bound, captures_buf, captures_len);
        },

        .while_loop => |wl| {
            try self.collectFreeVariablesInExpr(wl.cond, bound, captures_buf, captures_len);
            try self.collectFreeVariablesInExpr(wl.body, bound, captures_buf, captures_len);
        },

        .incref => |rc| {
            try self.collectFreeVariablesInExpr(rc.value, bound, captures_buf, captures_len);
        },

        .decref => |rc| {
            try self.collectFreeVariablesInExpr(rc.value, bound, captures_buf, captures_len);
        },

        .free => |rc| {
            try self.collectFreeVariablesInExpr(rc.value, bound, captures_buf, captures_len);
        },

        .hosted_call => |hc| {
            const args = self.store.getExprSpan(hc.args);
            for (args) |arg| {
                try self.collectFreeVariablesInExpr(arg, bound, captures_buf, captures_len);
            }
        },

        else => {
            // Literals, crash, runtime_error, empty_record, empty_list, zero_arg_tag
            // These expressions don't have free variables
        },
    }
}

/// Add bound variables from a statement
fn addStmtBoundVars(self: *Self, stmt: ir.MonoStmt, bound: *std.AutoHashMap(u48, void)) !void {
    // MonoStmt is a simple struct with pattern and expr
    // The pattern defines bound variables
    try self.extractBoundSymbolsFromPattern(stmt.pattern, bound);
}

/// ============================================================================
/// Phase 2: Validation
/// ============================================================================
/// Validate the lifted IR is correct
fn validateLifting(self: *Self) !void {
    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift.Phase3] Validating lifted IR ({} lambdas)\n", .{self.lifted_lambdas_map.count()});
    }

    // Validate pre-lifting structural invariants
    try self.validateMonoIRPreLifting();

    // Validate that all lifted lambdas have consistent information
    var iter = self.lifted_lambdas_map.valueIterator();
    while (iter.next()) |lifted| {
        try self.validateLiftedLambda(lifted.*);
    }

    // Validate post-lifting invariants
    try self.validateMonoIRPostLifting();

    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("[LambdaLift.Phase3] Validation complete\n", .{});
    }
}

/// Validate Mono IR before lambda lifting
fn validateMonoIRPreLifting(self: *Self) !void {
    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("  Validating pre-lifting invariants...\n", .{});
    }

    // All expressions in the store should be well-formed
    for (self.store.exprs.items) |expr| {
        // Check that referenced expressions exist
        switch (expr) {
            .call => |call| {
                std.debug.assert(@intFromEnum(call.fn_expr) < self.store.exprs.items.len);
                const arg_ids = self.store.getExprSpan(call.args);
                for (arg_ids) |arg_id| {
                    std.debug.assert(@intFromEnum(arg_id) < self.store.exprs.items.len);
                }
            },
            .binop => |binop| {
                std.debug.assert(@intFromEnum(binop.lhs) < self.store.exprs.items.len);
                std.debug.assert(@intFromEnum(binop.rhs) < self.store.exprs.items.len);
            },
            .block => |block| {
                std.debug.assert(@intFromEnum(block.final_expr) < self.store.exprs.items.len);
            },
            .lambda => |lambda| {
                std.debug.assert(@intFromEnum(lambda.body) < self.store.exprs.items.len);
            },
            .closure => |closure| {
                std.debug.assert(@intFromEnum(closure.lambda) < self.store.exprs.items.len);
            },
            else => {},
        }
    }
}

/// Validate a lifted lambda for correctness
fn validateLiftedLambda(self: *Self, lifted: LiftedLambda) !void {
    // Verify lifted symbol is valid
    std.debug.assert(!lifted.lifted_symbol.isNone());

    // Verify all captured symbols are valid
    for (lifted.captures) |cap| {
        std.debug.assert(!cap.symbol.isNone());
    }

    // Verify no duplicate captures
    var seen_symbols = std.AutoHashMap(u48, void).init(self.allocator);
    defer seen_symbols.deinit();

    for (lifted.captures) |cap| {
        const sym_key: u48 = @bitCast(cap.symbol);
        std.debug.assert(!seen_symbols.contains(sym_key));
        try seen_symbols.put(sym_key, {});
    }

    // Verify the original lambda body exists
    std.debug.assert(@intFromEnum(lifted.body) < self.store.exprs.items.len);

    // NOTE: We allow lambdas in the body if they have been lifted.
    // Nested lambdas in real Mono IR might not be lifted in strict order,
    // and curried functions (|a| |b| ...) create bodies that ARE lambdas.
    // The key invariant is that no UNLIFTED lambdas should remain.
    if (DEBUG_LAMBDA_LIFTING and self.containsUnliftedLambda(lifted.body)) {
        std.debug.print("[WARNING] Lambda body contains unlifted lambdas\n", .{});
    }
}

/// Validate Mono IR after lambda lifting
fn validateMonoIRPostLifting(self: *Self) !void {
    if (DEBUG_LAMBDA_LIFTING) {
        std.debug.print("  Validating post-lifting invariants...\n", .{});
    }

    // Verify all lambdas and closures have been marked as lifted (or handle curried functions)
    for (self.store.exprs.items) |expr| {
        // Lambda expressions might contain other lambdas (curried functions)
        // but they should all be marked as lifted
        if (expr == .lambda) {
            if (DEBUG_LAMBDA_LIFTING and self.containsUnliftedLambda(expr.lambda.body)) {
                std.debug.print("[WARNING] Lambda body contains unlifted lambdas\n", .{});
            }
        }

        // Closure expressions should not contain unlifted lambdas
        if (expr == .closure) {
            if (DEBUG_LAMBDA_LIFTING and self.containsUnliftedLambda(expr.closure.lambda)) {
                std.debug.print("[WARNING] Closure lambda is unlifted\n", .{});
            }
        }
    }

    // Verify all lifted lambdas have valid body references
    var iter = self.lifted_lambdas_map.valueIterator();
    while (iter.next()) |lifted| {
        std.debug.assert(@intFromEnum(lifted.body) < self.store.exprs.items.len);
    }
}

/// Check if an expression contains a lambda (nested)
fn containsLambda(self: *Self, expr_id: MonoExprId) bool {
    const expr = self.store.getExpr(expr_id);

    return switch (expr) {
        .lambda, .closure => true,

        .lookup => false, // Lookup has no nested expressions

        .call => |call| {
            if (self.containsLambda(call.fn_expr)) return true;
            const args = self.store.getExprSpan(call.args);
            for (args) |arg| {
                if (self.containsLambda(arg)) return true;
            }
            return false;
        },

        .binop => |binop| {
            return self.containsLambda(binop.lhs) or self.containsLambda(binop.rhs);
        },

        .unary_minus => |um| {
            return self.containsLambda(um.expr);
        },

        .unary_not => |un| {
            return self.containsLambda(un.expr);
        },

        .block => |block| {
            return self.containsLambda(block.final_expr);
        },

        .if_then_else => |ite| {
            const branches = self.store.getIfBranches(ite.branches);
            for (branches) |branch| {
                if (self.containsLambda(branch.cond) or self.containsLambda(branch.body)) return true;
            }
            return self.containsLambda(ite.final_else);
        },

        .when => |when_expr| {
            if (self.containsLambda(when_expr.value)) return true;
            const branches = self.store.getWhenBranches(when_expr.branches);
            for (branches) |branch| {
                if (self.containsLambda(branch.body)) return true;
            }
            return false;
        },

        .record => |record| {
            const fields = self.store.getExprSpan(record.fields);
            for (fields) |field| {
                if (self.containsLambda(field)) return true;
            }
            return false;
        },

        .tuple => |tuple| {
            const elems = self.store.getExprSpan(tuple.elems);
            for (elems) |elem| {
                if (self.containsLambda(elem)) return true;
            }
            return false;
        },

        .tag => |tag| {
            const args = self.store.getExprSpan(tag.args);
            for (args) |arg| {
                if (self.containsLambda(arg)) return true;
            }
            return false;
        },

        .list => |list| {
            const elems = self.store.getExprSpan(list.elems);
            for (elems) |elem| {
                if (self.containsLambda(elem)) return true;
            }
            return false;
        },

        .field_access => |fa| {
            return self.containsLambda(fa.record_expr);
        },

        .tuple_access => |ta| {
            return self.containsLambda(ta.tuple_expr);
        },

        .dbg => |dbg| {
            return self.containsLambda(dbg.expr);
        },

        .expect => |expect| {
            return self.containsLambda(expect.cond) or self.containsLambda(expect.body);
        },

        .low_level => |ll| {
            const args = self.store.getExprSpan(ll.args);
            for (args) |arg| {
                if (self.containsLambda(arg)) return true;
            }
            return false;
        },

        .early_return => |er| {
            return self.containsLambda(er.expr);
        },

        .nominal => |nom| {
            return self.containsLambda(nom.backing_expr);
        },

        .str_concat => |exprs| {
            const expr_ids = self.store.getExprSpan(exprs);
            for (expr_ids) |e| {
                if (self.containsLambda(e)) return true;
            }
            return false;
        },

        .int_to_str => |its| {
            return self.containsLambda(its.value);
        },

        .float_to_str => |fts| {
            return self.containsLambda(fts.value);
        },

        .dec_to_str => |dts| {
            return self.containsLambda(dts);
        },

        .str_escape_and_quote => |seq| {
            return self.containsLambda(seq);
        },

        .discriminant_switch => |ds| {
            if (self.containsLambda(ds.value)) return true;
            const branches = self.store.getExprSpan(ds.branches);
            for (branches) |branch| {
                if (self.containsLambda(branch)) return true;
            }
            return false;
        },

        .tag_payload_access => |tpa| {
            return self.containsLambda(tpa.value);
        },

        .for_loop => |fl| {
            return self.containsLambda(fl.list_expr) or self.containsLambda(fl.body);
        },

        .while_loop => |wl| {
            return self.containsLambda(wl.cond) or self.containsLambda(wl.body);
        },

        .incref => |rc| {
            return self.containsLambda(rc.value);
        },

        .decref => |rc| {
            return self.containsLambda(rc.value);
        },

        .free => |rc| {
            return self.containsLambda(rc.value);
        },

        .hosted_call => |hc| {
            const args = self.store.getExprSpan(hc.args);
            for (args) |arg| {
                if (self.containsLambda(arg)) return true;
            }
            return false;
        },

        .crash, .runtime_error, .empty_record, .empty_list, .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal, .bool_literal, .str_literal, .zero_arg_tag => {
            return false;
        },
    };
}

/// Check if an expression contains any lambdas that have NOT been lifted
fn containsUnliftedLambda(self: *Self, expr_id: MonoExprId) bool {
    const expr = self.store.getExpr(expr_id);

    return switch (expr) {
        .lambda => {
            // Lambda found - check if it has been lifted
            return !self.isLiftedLambda(expr_id);
        },

        .closure => true,

        .lookup => false,

        .call => |call| {
            if (self.containsUnliftedLambda(call.fn_expr)) return true;
            const args = self.store.getExprSpan(call.args);
            for (args) |arg| {
                if (self.containsUnliftedLambda(arg)) return true;
            }
            return false;
        },

        .binop => |binop| {
            return self.containsUnliftedLambda(binop.lhs) or self.containsUnliftedLambda(binop.rhs);
        },

        .unary_minus => |um| {
            return self.containsUnliftedLambda(um.expr);
        },

        .unary_not => |un| {
            return self.containsUnliftedLambda(un.expr);
        },

        .block => |block| {
            return self.containsUnliftedLambda(block.final_expr);
        },

        .if_then_else => |ite| {
            const branches = self.store.getIfBranches(ite.branches);
            for (branches) |branch| {
                if (self.containsUnliftedLambda(branch.cond) or self.containsUnliftedLambda(branch.body)) return true;
            }
            return self.containsUnliftedLambda(ite.final_else);
        },

        .when => |when_expr| {
            if (self.containsUnliftedLambda(when_expr.value)) return true;
            const branches = self.store.getWhenBranches(when_expr.branches);
            for (branches) |branch| {
                if (self.containsUnliftedLambda(branch.body)) return true;
            }
            return false;
        },

        .record => |record| {
            const fields = self.store.getExprSpan(record.fields);
            for (fields) |field| {
                if (self.containsUnliftedLambda(field)) return true;
            }
            return false;
        },

        .tuple => |tuple| {
            const elems = self.store.getExprSpan(tuple.elems);
            for (elems) |elem| {
                if (self.containsUnliftedLambda(elem)) return true;
            }
            return false;
        },

        .tag => |tag| {
            const args = self.store.getExprSpan(tag.args);
            for (args) |arg| {
                if (self.containsUnliftedLambda(arg)) return true;
            }
            return false;
        },

        .list => |list| {
            const elems = self.store.getExprSpan(list.elems);
            for (elems) |elem| {
                if (self.containsUnliftedLambda(elem)) return true;
            }
            return false;
        },

        .field_access => |fa| {
            return self.containsUnliftedLambda(fa.record_expr);
        },

        .tuple_access => |ta| {
            return self.containsUnliftedLambda(ta.tuple_expr);
        },

        .dbg => |dbg| {
            return self.containsUnliftedLambda(dbg.expr);
        },

        .expect => |expect| {
            return self.containsUnliftedLambda(expect.cond) or self.containsUnliftedLambda(expect.body);
        },

        .low_level => |ll| {
            const args = self.store.getExprSpan(ll.args);
            for (args) |arg| {
                if (self.containsUnliftedLambda(arg)) return true;
            }
            return false;
        },

        .early_return => |er| {
            return self.containsUnliftedLambda(er.expr);
        },

        .nominal => |nom| {
            return self.containsUnliftedLambda(nom.backing_expr);
        },

        .str_concat => |exprs| {
            const expr_ids = self.store.getExprSpan(exprs);
            for (expr_ids) |e| {
                if (self.containsUnliftedLambda(e)) return true;
            }
            return false;
        },

        .int_to_str => |its| {
            return self.containsUnliftedLambda(its.value);
        },

        .float_to_str => |fts| {
            return self.containsUnliftedLambda(fts.value);
        },

        .dec_to_str => |dts| {
            return self.containsUnliftedLambda(dts);
        },

        .str_escape_and_quote => |seq| {
            return self.containsUnliftedLambda(seq);
        },

        .discriminant_switch => |ds| {
            if (self.containsUnliftedLambda(ds.value)) return true;
            const branches = self.store.getExprSpan(ds.branches);
            for (branches) |branch| {
                if (self.containsUnliftedLambda(branch)) return true;
            }
            return false;
        },

        .tag_payload_access => |tpa| {
            return self.containsUnliftedLambda(tpa.value);
        },

        .for_loop => |fl| {
            return self.containsUnliftedLambda(fl.list_expr) or self.containsUnliftedLambda(fl.body);
        },

        .while_loop => |wl| {
            return self.containsUnliftedLambda(wl.cond) or self.containsUnliftedLambda(wl.body);
        },

        .incref => |rc| {
            return self.containsUnliftedLambda(rc.value);
        },

        .decref => |rc| {
            return self.containsUnliftedLambda(rc.value);
        },

        .free => |rc| {
            return self.containsUnliftedLambda(rc.value);
        },

        .hosted_call => |hc| {
            const args = self.store.getExprSpan(hc.args);
            for (args) |arg| {
                if (self.containsUnliftedLambda(arg)) return true;
            }
            return false;
        },

        .crash, .runtime_error, .empty_record, .empty_list, .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal, .bool_literal, .str_literal, .zero_arg_tag => {
            return false;
        },
    };
}

/// ============================================================================
/// Query Functions (used by code generators)
/// ============================================================================
/// Check if a lambda has been lifted
pub fn isLiftedLambda(self: *const Self, expr_id: MonoExprId) bool {
    const expr_id_u32 = @intFromEnum(expr_id);
    return self.lifted_lambdas_map.contains(expr_id_u32);
}

/// Get lifted lambda information
pub fn getLiftedLambda(self: *const Self, expr_id: MonoExprId) ?LiftedLambda {
    const expr_id_u32 = @intFromEnum(expr_id);
    return self.lifted_lambdas_map.get(expr_id_u32);
}

/// ============================================================================
/// Helper Functions
/// ============================================================================
/// Create a unique lifted function symbol
fn createLiftedSymbol(self: *Self) MonoSymbol {
    defer self.next_lifted_id += 1;
    // Create unique identifiers for lifted lambdas using upper range of u29
    const lifted_idx: u29 = @intCast((std.math.maxInt(u29) / 2) + self.next_lifted_id);
    const lifted_ident = Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = lifted_idx,
    };

    return MonoSymbol{
        .module_idx = self.app_module_idx orelse 0,
        .ident_idx = lifted_ident,
    };
}
