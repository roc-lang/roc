//! Reference Counting Insertion Pass
//!
//! This module implements the RC insertion pass that transforms CIR expressions
//! to include explicit reference counting operations (e_incref, e_decref, e_free).
//!
//! ## Overview
//!
//! The pass walks the CIR and inserts RC operations at appropriate points:
//! - `e_incref`: Before non-last uses of a refcounted value
//! - `e_decref`: At scope exits for owned values that weren't consumed
//! - `e_free`: When we know refcount is already 0 (optimization)
//!
//! ## Ownership Model
//!
//! Each symbol has an ownership state:
//! - **Owned**: The current scope owns the value and is responsible for decref
//! - **Borrowed**: The value is borrowed; no decref needed
//!
//! ## Usage Tracking
//!
//! The pass counts how many times each symbol is used:
//! - First N-1 uses: Insert incref before the use
//! - Last use: Consumes ownership, no incref needed
//! - Scope exit: If owned and not consumed, insert decref
//!
//! ## Integration
//!
//! This pass should be called after layout computation and before code generation.
//! All backends (interpreter, dev backend, LLVM) then emit code for the
//! explicit RC operations in the IR.

const std = @import("std");
const can = @import("can");
const layout_mod = @import("layout");
const base = @import("base");
const types = @import("types");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const NodeStore = can.NodeStore;
const Layout = layout_mod.Layout;
const LayoutStore = layout_mod.Store;
const Region = base.Region;
const Var = types.Var;
const TypeScope = types.TypeScope;

/// Ownership state of a symbol
pub const Ownership = enum {
    /// This scope owns the value and is responsible for releasing it
    owned,
    /// The value is borrowed; do not release
    borrowed,
};

/// Information about a symbol's RC state
pub const SymbolState = struct {
    /// The pattern that introduced this symbol
    pattern_idx: CIR.Pattern.Idx,
    /// Layout of this symbol's value (for checking if refcounted)
    layout_idx: ?layout_mod.Idx,
    /// Whether this scope owns the value
    ownership: Ownership,
    /// How many times this symbol is used in remaining code
    remaining_uses: u32,
    /// Whether ownership has been transferred away
    consumed: bool,
};

/// Snapshot of a symbol's state for branch reconciliation
pub const SymbolStateSnapshot = struct {
    pattern_idx: CIR.Pattern.Idx,
    remaining_uses: u32,
    consumed: bool,
};

/// Tracks patterns introduced in a scope
pub const ScopeInfo = struct {
    /// Patterns introduced in this scope
    introduced_patterns: std.array_list.Managed(CIR.Pattern.Idx),
    /// Parent scope for nested lookup
    parent: ?*ScopeInfo,

    fn init(allocator: std.mem.Allocator) ScopeInfo {
        return .{
            .introduced_patterns = std.array_list.Managed(CIR.Pattern.Idx).init(allocator),
            .parent = null,
        };
    }

    fn deinit(self: *ScopeInfo) void {
        self.introduced_patterns.deinit();
    }
};

/// The RC insertion pass
pub const InsertPass = struct {
    const Self = @This();

    /// The module environment containing the CIR
    module_env: *ModuleEnv,
    /// Layout store for computing and checking if types are refcounted
    /// Owned by this pass, created using the module's compile-time types
    layout_store: LayoutStore,
    /// Empty type scope for layout computation (no flex/rigid mappings needed)
    type_scope: TypeScope,
    /// Map from pattern index to symbol state
    symbol_states: std.AutoHashMap(CIR.Pattern.Idx, SymbolState),
    /// Allocator for internal data structures
    allocator: std.mem.Allocator,
    /// Current scope for tracking pattern introductions
    current_scope: ?*ScopeInfo,

    /// Initialize the RC insertion pass
    /// Creates a layout store using the module's compile-time types for layout computation.
    pub fn init(
        allocator: std.mem.Allocator,
        module_env: *ModuleEnv,
    ) std.mem.Allocator.Error!Self {
        return .{
            .module_env = module_env,
            .layout_store = try LayoutStore.init(module_env, &module_env.types, module_env.idents.builtin_str),
            .type_scope = TypeScope.init(allocator),
            .symbol_states = std.AutoHashMap(CIR.Pattern.Idx, SymbolState).init(allocator),
            .allocator = allocator,
            .current_scope = null,
        };
    }

    /// Deinitialize and free resources
    pub fn deinit(self: *Self) void {
        self.symbol_states.deinit();
        self.type_scope.deinit();
        self.layout_store.deinit();
    }

    /// Run the RC insertion pass on the module
    pub fn run(self: *Self) !void {
        // Phase 1: Count usages of each symbol
        try self.countUsages();

        // Phase 2: Insert RC operations based on usage counts
        try self.insertRcOperations();
    }

    /// Run the RC insertion pass on a specific expression (for REPL use).
    /// This processes a single expression that may not be in module_env.all_defs.
    /// Returns the transformed expression index.
    pub fn runOnExpr(self: *Self, expr_idx: CIR.Expr.Idx) !CIR.Expr.Idx {
        // Phase 1: Count usages in this expression
        try self.countUsagesInExpr(expr_idx);

        // Phase 2: Transform the expression with RC operations
        return try self.transformExpr(expr_idx);
    }

    /// Count how many times each symbol is used
    fn countUsages(self: *Self) !void {
        // Walk all top-level definitions
        const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = self.module_env.store.getDef(def_idx);
            try self.countUsagesInExpr(def.expr);
        }
    }

    /// Count usages in a single expression
    fn countUsagesInExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
        const expr = self.module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |local| {
                // This is a use of a symbol
                if (self.symbol_states.getPtr(local.pattern_idx)) |state| {
                    state.remaining_uses += 1;
                }
            },
            .e_block => |block| {
                // Walk statements
                const stmts = self.module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    try self.countUsagesInStatement(stmt_idx);
                }
                // Walk final expression
                try self.countUsagesInExpr(block.final_expr);
            },
            .e_if => |if_expr| {
                // Walk all branches
                const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = self.module_env.store.getIfBranch(branch_idx);
                    try self.countUsagesInExpr(branch.cond);
                    try self.countUsagesInExpr(branch.body);
                }
                // Walk else branch
                try self.countUsagesInExpr(if_expr.final_else);
            },
            .e_lambda => |lambda| {
                // Register each parameter pattern with borrowed ownership
                // Parameters are borrowed because the caller owns the arguments
                const params = self.module_env.store.slicePatterns(lambda.args);
                for (params) |param_pattern| {
                    try self.registerPattern(param_pattern, .borrowed, null);
                }
                // Walk lambda body
                try self.countUsagesInExpr(lambda.body);
            },
            .e_closure => |closure| {
                // Walk captures - these are uses of the captured variables
                const capture_idxs = self.module_env.store.sliceCaptures(closure.captures);
                for (capture_idxs) |capture_idx| {
                    const capture = self.module_env.store.getCapture(capture_idx);
                    if (self.symbol_states.getPtr(capture.pattern_idx)) |state| {
                        state.remaining_uses += 1;
                    }
                }
                // Walk the lambda body
                const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    try self.countUsagesInExpr(lambda_expr.e_lambda.body);
                }
            },
            .e_call => |call| {
                // Count usages in callee and arguments
                try self.countUsagesInExpr(call.func);
                const args = self.module_env.store.sliceExpr(call.args);
                for (args) |arg_idx| {
                    try self.countUsagesInExpr(arg_idx);
                }
            },
            .e_list => |list| {
                const elems = self.module_env.store.sliceExpr(list.elems);
                for (elems) |elem_idx| {
                    try self.countUsagesInExpr(elem_idx);
                }
            },
            .e_tuple => |tuple| {
                const elems = self.module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem_idx| {
                    try self.countUsagesInExpr(elem_idx);
                }
            },
            .e_record => |record| {
                const field_idxs = self.module_env.store.sliceRecordFields(record.fields);
                for (field_idxs) |field_idx| {
                    const field = self.module_env.store.getRecordField(field_idx);
                    try self.countUsagesInExpr(field.value);
                }
                // Record update syntax: { ..base, field: value }
                if (record.ext) |ext_expr| {
                    try self.countUsagesInExpr(ext_expr);
                }
            },
            .e_match => |match| {
                try self.countUsagesInExpr(match.cond);
                const branches = self.module_env.store.sliceMatchBranches(match.branches);
                for (branches) |branch_idx| {
                    const branch = self.module_env.store.getMatchBranch(branch_idx);
                    // Register bindings introduced by branch patterns
                    const branch_patterns = self.module_env.store.sliceMatchBranchPatterns(branch.patterns);
                    for (branch_patterns) |branch_pattern_idx| {
                        const branch_pattern = self.module_env.store.getMatchBranchPattern(branch_pattern_idx);
                        try self.registerBindingsFromPattern(branch_pattern.pattern, .owned);
                    }
                    // Count usages in guard if present
                    if (branch.guard) |guard| {
                        try self.countUsagesInExpr(guard);
                    }
                    // Count usages in branch body
                    try self.countUsagesInExpr(branch.value);
                }
            },
            .e_binop => |binop| {
                try self.countUsagesInExpr(binop.lhs);
                try self.countUsagesInExpr(binop.rhs);
            },
            .e_unary_minus => |unary| {
                try self.countUsagesInExpr(unary.expr);
            },
            .e_unary_not => |unary| {
                try self.countUsagesInExpr(unary.expr);
            },
            .e_tag => |tag| {
                const args = self.module_env.store.sliceExpr(tag.args);
                for (args) |arg_idx| {
                    try self.countUsagesInExpr(arg_idx);
                }
            },
            .e_dbg => |dbg| {
                try self.countUsagesInExpr(dbg.expr);
            },
            .e_expect => |expect| {
                try self.countUsagesInExpr(expect.body);
            },
            .e_for => |for_expr| {
                // Register the pattern for the loop variable
                // Note: loop variable type comes from iterating over expr, not expr itself
                try self.registerPattern(for_expr.patt, .borrowed, null);
                try self.countUsagesInExpr(for_expr.expr);
                try self.countUsagesInExpr(for_expr.body);
            },
            .e_return => |ret| {
                try self.countUsagesInExpr(ret.expr);
            },
            .e_dot_access => |dot| {
                try self.countUsagesInExpr(dot.receiver);
                if (dot.args) |args| {
                    const arg_exprs = self.module_env.store.sliceExpr(args);
                    for (arg_exprs) |arg_idx| {
                        try self.countUsagesInExpr(arg_idx);
                    }
                }
            },
            .e_nominal => |nom| {
                try self.countUsagesInExpr(nom.backing_expr);
            },
            .e_nominal_external => |nom| {
                try self.countUsagesInExpr(nom.backing_expr);
            },
            // Literals and other expressions that don't contain nested expressions
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_str,
            .e_str_segment,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_ellipsis,
            .e_lookup_external,
            .e_lookup_required,
            .e_runtime_error,
            .e_crash,
            .e_low_level_lambda,
            .e_hosted_lambda,
            .e_type_var_dispatch,
            .e_anno_only,
            => {},
            // RC operations - already explicit, don't recurse
            .e_incref, .e_decref, .e_free => {},
        }
    }

    /// Count usages in a statement
    fn countUsagesInStatement(self: *Self, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
        const stmt = self.module_env.store.getStatement(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                // Register this pattern as a bound symbol
                // Pass the expression so we can get layout from it (needed for synthetic patterns)
                try self.registerPattern(decl.pattern, .owned, decl.expr);
                try self.countUsagesInExpr(decl.expr);
            },
            .s_decl_gen => |decl| {
                try self.registerPattern(decl.pattern, .owned, decl.expr);
                try self.countUsagesInExpr(decl.expr);
            },
            .s_var => |var_decl| {
                try self.registerPattern(var_decl.pattern_idx, .owned, var_decl.expr);
                try self.countUsagesInExpr(var_decl.expr);
            },
            .s_reassign => |reassign| {
                try self.countUsagesInExpr(reassign.expr);
            },
            .s_expr => |expr| {
                try self.countUsagesInExpr(expr.expr);
            },
            .s_for => |for_stmt| {
                try self.registerPattern(for_stmt.patt, .borrowed, null);
                try self.countUsagesInExpr(for_stmt.expr);
                try self.countUsagesInExpr(for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.countUsagesInExpr(while_stmt.cond);
                try self.countUsagesInExpr(while_stmt.body);
            },
            .s_return => |ret| {
                try self.countUsagesInExpr(ret.expr);
            },
            .s_expect => |expect| {
                try self.countUsagesInExpr(expect.body);
            },
            .s_dbg => |dbg| {
                try self.countUsagesInExpr(dbg.expr);
            },
            .s_crash => {},
            .s_break => {},
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => {},
        }
    }

    /// Register a pattern and its introduced symbols.
    /// When expr_idx is provided (for declarations), the layout is computed from the expression's type.
    /// This handles synthetic patterns created by the Monomorphizer that don't have direct type mappings.
    fn registerPattern(self: *Self, pattern_idx: CIR.Pattern.Idx, ownership: Ownership, expr_idx: ?CIR.Expr.Idx) !void {
        // Add to current scope if we have one
        if (self.current_scope) |scope| {
            try scope.introduced_patterns.append(pattern_idx);
        }

        // Compute layout - prefer expression type (works for synthetic patterns), fall back to pattern type
        const layout_idx = if (expr_idx) |eidx|
            self.getLayoutForExpr(eidx) orelse self.getLayoutForPattern(pattern_idx)
        else
            self.getLayoutForPattern(pattern_idx);

        // Register the symbol state
        try self.symbol_states.put(pattern_idx, SymbolState{
            .pattern_idx = pattern_idx,
            .layout_idx = layout_idx,
            .ownership = ownership,
            .remaining_uses = 0,
            .consumed = false,
        });
    }

    /// Recursively register all bindings introduced by a pattern.
    /// This handles nested patterns like `Ok(val)`, `(a, b)`, `{ x, y }`, etc.
    fn registerBindingsFromPattern(self: *Self, pattern_idx: CIR.Pattern.Idx, ownership: Ownership) !void {
        const pattern = self.module_env.store.getPattern(pattern_idx);

        switch (pattern) {
            .assign => {
                // This is a direct binding - register it
                // No associated expression, so pass null for layout computation
                try self.registerPattern(pattern_idx, ownership, null);
            },
            .as => |as_pat| {
                // Register the alias and recurse into the nested pattern
                try self.registerPattern(pattern_idx, ownership, null);
                try self.registerBindingsFromPattern(as_pat.pattern, ownership);
            },
            .applied_tag => |tag| {
                // Recurse into argument patterns
                const args = self.module_env.store.slicePatterns(tag.args);
                for (args) |arg_pattern| {
                    try self.registerBindingsFromPattern(arg_pattern, ownership);
                }
            },
            .nominal => |nom| {
                // Recurse into backing pattern
                try self.registerBindingsFromPattern(nom.backing_pattern, ownership);
            },
            .nominal_external => |nom| {
                // Recurse into backing pattern
                try self.registerBindingsFromPattern(nom.backing_pattern, ownership);
            },
            .record_destructure => |record| {
                // Recurse into each destruct pattern
                const destructs = self.module_env.store.sliceRecordDestructs(record.destructs);
                for (destructs) |destruct_idx| {
                    const destruct = self.module_env.store.getRecordDestruct(destruct_idx);
                    // The pattern is in the kind field - could be Required or SubPattern
                    const pattern_from_kind = destruct.kind.toPatternIdx();
                    try self.registerBindingsFromPattern(pattern_from_kind, ownership);
                }
            },
            .list => |list| {
                // Recurse into element patterns
                const patterns = self.module_env.store.slicePatterns(list.patterns);
                for (patterns) |elem_pattern| {
                    try self.registerBindingsFromPattern(elem_pattern, ownership);
                }
                // Handle rest pattern if present
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern| {
                        try self.registerBindingsFromPattern(rest_pattern, ownership);
                    }
                }
            },
            .tuple => |tuple| {
                // Recurse into element patterns
                const patterns = self.module_env.store.slicePatterns(tuple.patterns);
                for (patterns) |elem_pattern| {
                    try self.registerBindingsFromPattern(elem_pattern, ownership);
                }
            },
            // Patterns that don't introduce bindings
            .underscore,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .runtime_error,
            => {},
        }
    }

    /// Get the layout index for a pattern by computing it from the type system.
    /// Returns null if the layout cannot be computed (e.g., for patterns with unresolved types).
    fn getLayoutForPattern(self: *Self, pattern_idx: CIR.Pattern.Idx) ?layout_mod.Idx {
        // Pattern indices map directly to type variables
        const type_var: Var = @enumFromInt(@intFromEnum(pattern_idx));

        // First check the cache
        if (self.layout_store.layouts_by_var.get(type_var)) |cached_idx| {
            return cached_idx;
        }

        // Compute the layout from the type variable
        // This will cache the result in layouts_by_var
        // Layout computation can fail for various reasons (unresolved types, etc.)
        // In those cases, we conservatively assume no RC is needed
        return self.layout_store.addTypeVar(type_var, &self.type_scope) catch null;
    }

    /// Get the layout index for an expression by computing it from the expression's type.
    /// This is useful for synthetic patterns where the pattern itself doesn't have type info,
    /// but the expression being assigned does.
    fn getLayoutForExpr(self: *Self, expr_idx: CIR.Expr.Idx) ?layout_mod.Idx {
        // Expression indices map to type variables the same way as patterns
        const type_var: Var = @enumFromInt(@intFromEnum(expr_idx));

        // First check the cache
        if (self.layout_store.layouts_by_var.get(type_var)) |cached_idx| {
            return cached_idx;
        }

        // Compute the layout from the type variable
        return self.layout_store.addTypeVar(type_var, &self.type_scope) catch null;
    }

    // ============================================================================
    // Phase 2: Transform IR to insert RC operations
    // ============================================================================

    /// Insert RC operations into the IR by walking and transforming expressions
    fn insertRcOperations(self: *Self) !void {
        // Walk all top-level definitions and transform their expressions
        const defs_slice = self.module_env.store.sliceDefs(self.module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = self.module_env.store.getDef(def_idx);
            const new_expr = try self.transformExpr(def.expr);
            // If the expression changed, update the def
            if (new_expr != def.expr) {
                self.module_env.store.setDefExpr(def_idx, new_expr);
            }
        }
    }

    /// Transform an expression, inserting RC operations as needed.
    /// Returns the (possibly new) expression index.
    fn transformExpr(self: *Self, expr_idx: CIR.Expr.Idx) std.mem.Allocator.Error!CIR.Expr.Idx {
        const expr = self.module_env.store.getExpr(expr_idx);

        return switch (expr) {
            .e_lookup_local => |local| try self.transformLookup(expr_idx, local.pattern_idx),
            .e_block => |block| try self.transformBlock(expr_idx, block),
            .e_if => |if_expr| try self.transformIf(expr_idx, if_expr),
            .e_lambda => |lambda| try self.transformLambda(expr_idx, lambda),
            .e_closure => |closure| try self.transformClosure(expr_idx, closure),
            .e_call => |call| try self.transformCall(expr_idx, call),
            .e_match => |match| try self.transformMatch(expr_idx, match),
            .e_binop => |binop| try self.transformBinop(expr_idx, binop),
            .e_unary_minus => |unary| try self.transformUnaryMinus(expr_idx, unary),
            .e_unary_not => |unary| try self.transformUnaryNot(expr_idx, unary),
            .e_list => |list| try self.transformList(expr_idx, list),
            .e_tuple => |tuple| try self.transformTuple(expr_idx, tuple),
            .e_record => |record| try self.transformRecord(expr_idx, record),
            .e_tag => |tag| try self.transformTag(expr_idx, tag),
            .e_nominal => |nom| try self.transformNominal(expr_idx, nom),
            .e_nominal_external => |nom| try self.transformNominalExternal(expr_idx, nom),
            .e_dot_access => |dot| try self.transformDotAccess(expr_idx, dot),
            .e_dbg => |dbg| try self.transformDbg(expr_idx, dbg),
            .e_expect => |exp| try self.transformExpect(expr_idx, exp),
            .e_for => |for_expr| try self.transformFor(expr_idx, for_expr),
            .e_return => |ret| try self.transformReturn(expr_idx, ret),
            // Expressions that don't contain nested expressions or need transformation
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_str,
            .e_str_segment,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_ellipsis,
            .e_lookup_external,
            .e_lookup_required,
            .e_runtime_error,
            .e_crash,
            .e_low_level_lambda,
            .e_hosted_lambda,
            .e_type_var_dispatch,
            .e_anno_only,
            .e_incref,
            .e_decref,
            .e_free,
            => expr_idx,
        };
    }

    /// Transform a local variable lookup, potentially inserting incref
    fn transformLookup(self: *Self, expr_idx: CIR.Expr.Idx, pattern_idx: CIR.Pattern.Idx) !CIR.Expr.Idx {
        if (self.symbol_states.getPtr(pattern_idx)) |state| {
            // Check if this is NOT the last use
            if (state.remaining_uses > 1) {
                // Check if the layout needs RC (if we have layout info)
                const needs_rc = if (state.layout_idx) |layout_idx|
                    self.layoutNeedsRc(layout_idx)
                else
                    false;

                if (needs_rc) {
                    // Decrement remaining uses
                    state.remaining_uses -= 1;

                    // Create e_incref expression
                    const region = self.module_env.store.getExprRegion(expr_idx);
                    const incref_expr = try self.module_env.store.addExpr(
                        .{ .e_incref = .{
                            .pattern_idx = pattern_idx,
                            .count = 1,
                        } },
                        region,
                    );

                    // Create a block that does: { s_expr(incref); original_lookup }
                    // First add incref as a statement to scratch
                    const stmt_start = self.module_env.store.scratchTop("statements");
                    const incref_stmt = try self.module_env.store.addStatement(
                        .{ .s_expr = .{ .expr = incref_expr } },
                        region,
                    );
                    try self.module_env.store.addScratchStatement(incref_stmt);

                    // Build statement span
                    const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

                    // Create block with incref statement and original lookup as final_expr
                    return try self.module_env.store.addExpr(
                        .{ .e_block = .{
                            .stmts = stmts_span,
                            .final_expr = expr_idx,
                        } },
                        region,
                    );
                } else {
                    // Not refcounted, just decrement uses
                    state.remaining_uses -= 1;
                }
            } else if (state.remaining_uses == 1) {
                // Last use - just decrement remaining_uses.
                // Don't mark as consumed here - a lookup doesn't transfer ownership.
                // Only consuming contexts (return, passing to owned parameter) mark consumed.
                // Without borrow inference, we assume all uses are borrowed and let
                // scope exit handle decrefs. See Rust's inc_dec.rs for the model.
                state.remaining_uses = 0;
            }
        }
        return expr_idx;
    }

    /// Transform a block, inserting decrefs at scope exit
    fn transformBlock(self: *Self, expr_idx: CIR.Expr.Idx, block: anytype) !CIR.Expr.Idx {
        // Create a new scope for this block
        var scope = ScopeInfo.init(self.allocator);
        scope.parent = self.current_scope;
        self.current_scope = &scope;
        defer {
            self.current_scope = scope.parent;
            scope.deinit();
        }

        // Transform all statements
        const stmts = self.module_env.store.sliceStatements(block.stmts);
        var any_changed = false;
        const stmt_start = self.module_env.store.scratchTop("statements");

        for (stmts) |stmt_idx| {
            const new_stmt = try self.transformStatement(stmt_idx);
            try self.module_env.store.addScratchStatement(new_stmt);
            if (new_stmt != stmt_idx) any_changed = true;
        }

        // Transform final expression
        const new_final = try self.transformExpr(block.final_expr);
        if (new_final != block.final_expr) any_changed = true;

        // The final expression is the "return value" of the block.
        // If it's a lookup, that pattern's ownership transfers to the caller,
        // so mark it as consumed (don't decref it at scope exit).
        const final_expr_val = self.module_env.store.getExpr(block.final_expr);
        if (final_expr_val == .e_lookup_local) {
            if (self.symbol_states.getPtr(final_expr_val.e_lookup_local.pattern_idx)) |state| {
                state.consumed = true;
            }
        }

        // Insert decrefs for symbols going out of scope that weren't consumed
        for (scope.introduced_patterns.items) |pattern_idx| {
            if (self.symbol_states.get(pattern_idx)) |state| {
                if (state.ownership == .owned and !state.consumed) {
                    // Check if layout needs RC
                    const needs_rc = if (state.layout_idx) |layout_idx|
                        self.layoutNeedsRc(layout_idx)
                    else
                        true;  // Assume RC needed if layout unknown

                    if (needs_rc) {
                        any_changed = true;
                        const region = self.module_env.store.getExprRegion(expr_idx);
                        const decref_expr = try self.module_env.store.addExpr(
                            .{ .e_decref = .{ .pattern_idx = pattern_idx } },
                            region,
                        );
                        const decref_stmt = try self.module_env.store.addStatement(
                            .{ .s_expr = .{ .expr = decref_expr } },
                            region,
                        );
                        try self.module_env.store.addScratchStatement(decref_stmt);
                    }
                }
            }
        }

        // If nothing changed, return original
        if (!any_changed) {
            // Clear scratch statements we added
            self.module_env.store.clearScratchFrom("statements", stmt_start);
            return expr_idx;
        }

        // Build new statement span
        const new_stmts = try self.module_env.store.statementSpanFrom(stmt_start);

        // Create new block expression
        return try self.module_env.store.addExpr(
            .{ .e_block = .{
                .stmts = new_stmts,
                .final_expr = new_final,
            } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    /// Transform a statement
    fn transformStatement(self: *Self, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!CIR.Statement.Idx {
        const stmt = self.module_env.store.getStatement(stmt_idx);

        return switch (stmt) {
            .s_decl => |decl| blk: {
                // Register pattern in current scope
                if (self.current_scope) |scope| {
                    try scope.introduced_patterns.append(decl.pattern);
                }
                // Transform the expression
                const new_expr = try self.transformExpr(decl.expr);
                if (new_expr != decl.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_decl = .{
                            .pattern = decl.pattern,
                            .expr = new_expr,
                            .anno = decl.anno,
                        } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_decl_gen => |decl| blk: {
                if (self.current_scope) |scope| {
                    try scope.introduced_patterns.append(decl.pattern);
                }
                const new_expr = try self.transformExpr(decl.expr);
                if (new_expr != decl.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_decl_gen = .{
                            .pattern = decl.pattern,
                            .expr = new_expr,
                            .anno = decl.anno,
                        } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_var => |var_decl| blk: {
                if (self.current_scope) |scope| {
                    try scope.introduced_patterns.append(var_decl.pattern_idx);
                }
                const new_expr = try self.transformExpr(var_decl.expr);
                if (new_expr != var_decl.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_var = .{
                            .pattern_idx = var_decl.pattern_idx,
                            .expr = new_expr,
                            .anno = var_decl.anno,
                        } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_reassign => |reassign| blk: {
                const new_expr = try self.transformExpr(reassign.expr);
                if (new_expr != reassign.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_reassign = .{
                            .pattern_idx = reassign.pattern_idx,
                            .expr = new_expr,
                        } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_expr => |expr| blk: {
                const new_expr = try self.transformExpr(expr.expr);
                if (new_expr != expr.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_expr = .{ .expr = new_expr } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_return => |ret| blk: {
                const new_expr = try self.transformExpr(ret.expr);
                if (new_expr != ret.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_return = .{ .expr = new_expr, .lambda = ret.lambda } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_expect => |exp| blk: {
                const new_body = try self.transformExpr(exp.body);
                if (new_body != exp.body) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_expect = .{ .body = new_body } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_dbg => |dbg| blk: {
                const new_expr = try self.transformExpr(dbg.expr);
                if (new_expr != dbg.expr) {
                    break :blk try self.module_env.store.addStatement(
                        .{ .s_dbg = .{ .expr = new_expr } },
                        self.module_env.store.getStatementRegion(stmt_idx),
                    );
                }
                break :blk stmt_idx;
            },
            .s_for => |for_stmt| blk: {
                const new_stmt = try self.transformForStatement(stmt_idx, for_stmt);
                break :blk new_stmt;
            },
            .s_while => |while_stmt| blk: {
                const new_stmt = try self.transformWhileStatement(stmt_idx, while_stmt);
                break :blk new_stmt;
            },
            // These don't need transformation
            .s_crash, .s_break, .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias, .s_runtime_error => stmt_idx,
        };
    }

    // ============================================================================
    // Expression transformers - recursively transform sub-expressions
    // ============================================================================

    fn transformIf(self: *Self, expr_idx: CIR.Expr.Idx, if_expr: anytype) !CIR.Expr.Idx {
        // Phase 3B: Handle divergent consumption in branches
        //
        // Different branches may consume different symbols. For example:
        //   x = [1, 2, 3]
        //   if condition then x else []
        // The 'then' branch consumes x, but 'else' doesn't.
        // We need to insert a decref for x in the 'else' branch.
        //
        // Strategy:
        // 1. Snapshot state before branches
        // 2. Transform each branch, recording which symbols were consumed
        // 3. Compute union of consumed symbols across all branches
        // 4. For branches that didn't consume a symbol that others did, insert decref

        const branches = self.module_env.store.sliceIfBranches(if_expr.branches);
        const region = self.module_env.store.getExprRegion(expr_idx);

        // Snapshot state before processing branches
        const snapshot = try self.snapshotSymbolStates();
        defer self.allocator.free(snapshot);

        // Store transformed bodies and their consumed symbol sets
        const total_branches = branches.len + 1; // +1 for else branch
        var transformed_bodies = try self.allocator.alloc(CIR.Expr.Idx, total_branches);
        defer self.allocator.free(transformed_bodies);
        var transformed_conds = try self.allocator.alloc(CIR.Expr.Idx, branches.len);
        defer self.allocator.free(transformed_conds);
        var consumed_sets = try self.allocator.alloc(std.AutoHashMap(CIR.Pattern.Idx, void), total_branches);
        defer {
            for (consumed_sets) |*set| set.deinit();
            self.allocator.free(consumed_sets);
        }

        // Transform each if-then branch
        for (branches, 0..) |branch_idx, i| {
            const branch = self.module_env.store.getIfBranch(branch_idx);

            // Transform condition (doesn't affect symbol consumption for the body)
            transformed_conds[i] = try self.transformExpr(branch.cond);

            // Restore snapshot before transforming body
            self.restoreSymbolStates(snapshot);

            // Transform body
            transformed_bodies[i] = try self.transformExpr(branch.body);

            // Record which symbols were consumed
            consumed_sets[i] = try self.getConsumedPatterns();
        }

        // Transform else branch
        self.restoreSymbolStates(snapshot);
        transformed_bodies[branches.len] = try self.transformExpr(if_expr.final_else);
        consumed_sets[branches.len] = try self.getConsumedPatterns();

        // Compute union of all consumed symbols
        var all_consumed = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
        defer all_consumed.deinit();
        for (consumed_sets) |set| {
            var iter = set.keyIterator();
            while (iter.next()) |pattern_idx| {
                try all_consumed.put(pattern_idx.*, {});
            }
        }

        // For each branch, insert decrefs for symbols consumed by other branches but not this one
        var any_changed = false;
        const branch_start = self.module_env.store.scratchTop("if_branches");

        for (branches, 0..) |branch_idx, i| {
            const branch = self.module_env.store.getIfBranch(branch_idx);
            var body = transformed_bodies[i];

            // Check for symbols consumed elsewhere but not in this branch
            var decrefs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
            defer decrefs_needed.deinit();

            var all_iter = all_consumed.keyIterator();
            while (all_iter.next()) |pattern_idx| {
                if (!consumed_sets[i].contains(pattern_idx.*)) {
                    // This branch didn't consume this symbol but another did
                    // Check if it needs RC
                    if (self.symbol_states.get(pattern_idx.*)) |state| {
                        if (state.ownership == .owned) {
                            const needs_rc = if (state.layout_idx) |layout_idx|
                                self.layoutNeedsRc(layout_idx)
                            else
                                true;
                            if (needs_rc) {
                                try decrefs_needed.append(pattern_idx.*);
                            }
                        }
                    }
                }
            }

            // If we need decrefs, wrap body in a block with decref statements
            if (decrefs_needed.items.len > 0) {
                any_changed = true;
                body = try self.wrapWithDecrefs(body, decrefs_needed.items, region);
            }

            // Check if branch changed
            if (transformed_conds[i] != branch.cond or body != branch.body) {
                any_changed = true;
            }

            const new_branch = try self.module_env.store.addIfBranch(.{
                .cond = transformed_conds[i],
                .body = body,
            }, region);
            try self.module_env.store.addScratchIfBranch(new_branch);
        }

        // Handle else branch
        var else_body = transformed_bodies[branches.len];
        {
            var decrefs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
            defer decrefs_needed.deinit();

            var all_iter = all_consumed.keyIterator();
            while (all_iter.next()) |pattern_idx| {
                if (!consumed_sets[branches.len].contains(pattern_idx.*)) {
                    if (self.symbol_states.get(pattern_idx.*)) |state| {
                        if (state.ownership == .owned) {
                            const needs_rc = if (state.layout_idx) |layout_idx|
                                self.layoutNeedsRc(layout_idx)
                            else
                                true;
                            if (needs_rc) {
                                try decrefs_needed.append(pattern_idx.*);
                            }
                        }
                    }
                }
            }

            if (decrefs_needed.items.len > 0) {
                any_changed = true;
                else_body = try self.wrapWithDecrefs(else_body, decrefs_needed.items, region);
            }
        }

        if (else_body != if_expr.final_else) any_changed = true;

        // Merge consumption from all branches - a symbol is consumed if ANY branch consumed it
        // (since exactly one branch will execute, and that branch might consume it)
        var final_iter = all_consumed.keyIterator();
        while (final_iter.next()) |pattern_idx| {
            if (self.symbol_states.getPtr(pattern_idx.*)) |state| {
                state.consumed = true;
            }
        }

        if (!any_changed) {
            self.module_env.store.clearScratchFrom("if_branches", branch_start);
            return expr_idx;
        }

        const new_branches = try self.module_env.store.ifBranchSpanFrom(branch_start);
        return try self.module_env.store.addExpr(
            .{ .e_if = .{
                .branches = new_branches,
                .final_else = else_body,
            } },
            region,
        );
    }

    /// Wrap an expression in a block with decref statements at the end
    fn wrapWithDecrefs(self: *Self, expr_idx: CIR.Expr.Idx, patterns: []const CIR.Pattern.Idx, region: Region) !CIR.Expr.Idx {
        const stmt_start = self.module_env.store.scratchTop("statements");

        // Add decref statements for each pattern
        for (patterns) |pattern_idx| {
            const decref_expr = try self.module_env.store.addExpr(
                .{ .e_decref = .{ .pattern_idx = pattern_idx } },
                region,
            );
            const decref_stmt = try self.module_env.store.addStatement(
                .{ .s_expr = .{ .expr = decref_expr } },
                region,
            );
            try self.module_env.store.addScratchStatement(decref_stmt);
        }

        const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

        // Create block: { decref1; decref2; ...; original_expr }
        return try self.module_env.store.addExpr(
            .{ .e_block = .{
                .stmts = stmts_span,
                .final_expr = expr_idx,
            } },
            region,
        );
    }

    fn transformLambda(self: *Self, expr_idx: CIR.Expr.Idx, lambda: anytype) !CIR.Expr.Idx {
        const region = self.module_env.store.getExprRegion(expr_idx);

        // Create a new scope for the lambda body
        var scope = ScopeInfo.init(self.allocator);
        scope.parent = self.current_scope;
        self.current_scope = &scope;
        defer {
            self.current_scope = scope.parent;
            scope.deinit();
        }

        // Register parameter patterns in this scope
        const params = self.module_env.store.slicePatterns(lambda.args);
        for (params) |param_pattern| {
            try scope.introduced_patterns.append(param_pattern);
        }

        // Transform the body
        const new_body = try self.transformExpr(lambda.body);

        // The lambda body is the return value of the lambda.
        // If it's a lookup, that pattern's ownership transfers to the caller.
        const body_expr = self.module_env.store.getExpr(lambda.body);
        if (body_expr == .e_lookup_local) {
            if (self.symbol_states.getPtr(body_expr.e_lookup_local.pattern_idx)) |state| {
                state.consumed = true;
            }
        }

        // Collect decrefs needed for unconsumed refcounted parameters
        var decrefs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
        defer decrefs_needed.deinit();

        for (scope.introduced_patterns.items) |pattern_idx| {
            if (self.symbol_states.get(pattern_idx)) |state| {
                // Parameters are borrowed, but if they're refcounted and not consumed,
                // we still might need cleanup in some cases (e.g., if the parameter
                // is assigned to a local that goes out of scope)
                // For now, we only decref owned values that weren't consumed
                if (state.ownership == .owned and !state.consumed) {
                    const needs_rc = if (state.layout_idx) |layout_idx|
                        self.layoutNeedsRc(layout_idx)
                    else
                        true;
                    if (needs_rc) {
                        try decrefs_needed.append(pattern_idx);
                    }
                }
            }
        }

        // If we need decrefs, wrap the body
        var final_body = new_body;
        if (decrefs_needed.items.len > 0) {
            final_body = try self.wrapWithDecrefs(new_body, decrefs_needed.items, region);
        }

        if (final_body != lambda.body) {
            return try self.module_env.store.addExpr(
                .{ .e_lambda = .{
                    .args = lambda.args,
                    .body = final_body,
                } },
                region,
            );
        }
        return expr_idx;
    }

    fn transformClosure(self: *Self, expr_idx: CIR.Expr.Idx, closure: anytype) !CIR.Expr.Idx {
        const region = self.module_env.store.getExprRegion(expr_idx);

        // Transform the lambda
        const new_lambda = try self.transformExpr(closure.lambda_idx);

        // Check if any captures need incref
        // When a closure captures a variable, it creates an additional reference
        const capture_idxs = self.module_env.store.sliceCaptures(closure.captures);
        var increfs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
        defer increfs_needed.deinit();

        for (capture_idxs) |capture_idx| {
            const capture = self.module_env.store.getCapture(capture_idx);
            if (self.symbol_states.get(capture.pattern_idx)) |state| {
                // Check if the captured variable needs RC
                const needs_rc = if (state.layout_idx) |layout_idx|
                    self.layoutNeedsRc(layout_idx)
                else
                    false;
                if (needs_rc) {
                    try increfs_needed.append(capture.pattern_idx);
                }
            }
        }

        // Build the closure expression (possibly with new lambda)
        var closure_expr_idx = expr_idx;
        if (new_lambda != closure.lambda_idx) {
            closure_expr_idx = try self.module_env.store.addExpr(
                .{ .e_closure = .{
                    .lambda_idx = new_lambda,
                    .captures = closure.captures,
                    .tag_name = closure.tag_name,
                } },
                region,
            );
        }

        // If we need increfs, wrap the closure in a block with incref statements
        if (increfs_needed.items.len > 0) {
            const stmt_start = self.module_env.store.scratchTop("statements");

            // Add incref statements for each captured variable
            for (increfs_needed.items) |pattern_idx| {
                const incref_expr = try self.module_env.store.addExpr(
                    .{ .e_incref = .{
                        .pattern_idx = pattern_idx,
                        .count = 1,
                    } },
                    region,
                );
                const incref_stmt = try self.module_env.store.addStatement(
                    .{ .s_expr = .{ .expr = incref_expr } },
                    region,
                );
                try self.module_env.store.addScratchStatement(incref_stmt);
            }

            const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

            // Create block: { incref1; incref2; ...; closure }
            return try self.module_env.store.addExpr(
                .{ .e_block = .{
                    .stmts = stmts_span,
                    .final_expr = closure_expr_idx,
                } },
                region,
            );
        }

        return closure_expr_idx;
    }

    /// Transform a for loop statement to properly handle RC for the loop variable.
    /// The loop variable is rebound each iteration and needs to be decref'd at the end.
    fn transformForStatement(self: *Self, stmt_idx: CIR.Statement.Idx, for_stmt: anytype) !CIR.Statement.Idx {
        const region = self.module_env.store.getStatementRegion(stmt_idx);

        // Create a scope for the loop body
        var scope = ScopeInfo.init(self.allocator);
        scope.parent = self.current_scope;
        self.current_scope = &scope;
        defer {
            self.current_scope = scope.parent;
            scope.deinit();
        }

        // Register the loop variable pattern
        try self.registerBindingsFromPattern(for_stmt.patt, .owned);
        try scope.introduced_patterns.append(for_stmt.patt);

        // Transform the iterable expression
        const new_expr = try self.transformExpr(for_stmt.expr);

        // Transform the body
        const new_body = try self.transformExpr(for_stmt.body);

        // Check if loop variable needs decref at end of each iteration
        var needs_decref = false;
        if (self.symbol_states.get(for_stmt.patt)) |state| {
            if (!state.consumed) {
                const needs_rc = if (state.layout_idx) |layout_idx|
                    self.layoutNeedsRc(layout_idx)
                else
                    false;
                needs_decref = needs_rc;
            }
        }

        // If we need decref, wrap the body in a block that decrefs at the end
        var final_body = new_body;
        if (needs_decref) {
            final_body = try self.wrapWithDecrefs(new_body, &[_]CIR.Pattern.Idx{for_stmt.patt}, region);
        }

        // Return new statement if anything changed
        if (new_expr != for_stmt.expr or final_body != for_stmt.body) {
            return try self.module_env.store.addStatement(
                .{ .s_for = .{
                    .patt = for_stmt.patt,
                    .expr = new_expr,
                    .body = final_body,
                } },
                region,
            );
        }
        return stmt_idx;
    }

    /// Transform a while loop statement.
    fn transformWhileStatement(self: *Self, stmt_idx: CIR.Statement.Idx, while_stmt: anytype) !CIR.Statement.Idx {
        const region = self.module_env.store.getStatementRegion(stmt_idx);

        // Transform the condition
        const new_cond = try self.transformExpr(while_stmt.cond);

        // Transform the body
        const new_body = try self.transformExpr(while_stmt.body);

        // Return new statement if anything changed
        if (new_cond != while_stmt.cond or new_body != while_stmt.body) {
            return try self.module_env.store.addStatement(
                .{ .s_while = .{
                    .cond = new_cond,
                    .body = new_body,
                } },
                region,
            );
        }
        return stmt_idx;
    }

    fn transformCall(self: *Self, expr_idx: CIR.Expr.Idx, call: anytype) !CIR.Expr.Idx {
        // Arguments are already normalized to simple lookups by the Monomorphizer.
        // Just transform the callee and arguments recursively.
        var any_changed = false;
        const new_callee = try self.transformExpr(call.func);
        if (new_callee != call.func) any_changed = true;

        const args = self.module_env.store.sliceExpr(call.args);
        const arg_start = self.module_env.store.scratchExprTop();
        for (args) |arg_idx| {
            const new_arg = try self.transformExpr(arg_idx);
            if (new_arg != arg_idx) any_changed = true;
            try self.module_env.store.addScratchExpr(new_arg);
        }

        if (!any_changed) {
            self.module_env.store.clearScratchExprsFrom(arg_start);
            return expr_idx;
        }

        const new_args = try self.module_env.store.exprSpanFrom(arg_start);
        return try self.module_env.store.addExpr(
            .{ .e_call = .{
                .func = new_callee,
                .args = new_args,
                .called_via = call.called_via,
            } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }


    fn transformMatch(self: *Self, expr_idx: CIR.Expr.Idx, match: anytype) !CIR.Expr.Idx {
        // Phase 3B: Handle divergent consumption in match branches
        // Similar to if/then/else, different match branches may consume different symbols.

        const region = self.module_env.store.getExprRegion(expr_idx);

        // Transform the matched expression first
        const new_matched = try self.transformExpr(match.cond);

        const branches = self.module_env.store.sliceMatchBranches(match.branches);
        if (branches.len == 0) {
            // Empty match - just return with potentially new matched expr
            if (new_matched != match.cond) {
                return try self.module_env.store.addExpr(
                    .{ .e_match = .{
                        .cond = new_matched,
                        .branches = match.branches,
                        .exhaustive = match.exhaustive,
                        .is_try_suffix = match.is_try_suffix,
                    } },
                    region,
                );
            }
            return expr_idx;
        }

        // Snapshot state before processing branches
        const snapshot = try self.snapshotSymbolStates();
        defer self.allocator.free(snapshot);

        // Store transformed bodies and their consumed symbol sets
        var transformed_bodies = try self.allocator.alloc(CIR.Expr.Idx, branches.len);
        defer self.allocator.free(transformed_bodies);
        var consumed_sets = try self.allocator.alloc(std.AutoHashMap(CIR.Pattern.Idx, void), branches.len);
        defer {
            for (consumed_sets) |*set| set.deinit();
            self.allocator.free(consumed_sets);
        }

        // Transform each match branch
        for (branches, 0..) |branch_idx, i| {
            const branch = self.module_env.store.getMatchBranch(branch_idx);

            // Restore snapshot before transforming body
            self.restoreSymbolStates(snapshot);

            // Transform body
            transformed_bodies[i] = try self.transformExpr(branch.value);

            // Record which symbols were consumed
            consumed_sets[i] = try self.getConsumedPatterns();
        }

        // Compute union of all consumed symbols
        var all_consumed = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
        defer all_consumed.deinit();
        for (consumed_sets) |set| {
            var iter = set.keyIterator();
            while (iter.next()) |pattern_idx| {
                try all_consumed.put(pattern_idx.*, {});
            }
        }

        // For each branch, insert decrefs for symbols consumed by other branches but not this one
        var any_changed = new_matched != match.cond;
        const branch_start = self.module_env.store.scratchTop("match_branches");

        for (branches, 0..) |branch_idx, i| {
            const branch = self.module_env.store.getMatchBranch(branch_idx);
            var body = transformed_bodies[i];

            // Check for symbols consumed elsewhere but not in this branch
            var decrefs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
            defer decrefs_needed.deinit();

            var all_iter = all_consumed.keyIterator();
            while (all_iter.next()) |pattern_idx| {
                if (!consumed_sets[i].contains(pattern_idx.*)) {
                    // This branch didn't consume this symbol but another did
                    if (self.symbol_states.get(pattern_idx.*)) |state| {
                        if (state.ownership == .owned) {
                            const needs_rc = if (state.layout_idx) |layout_idx|
                                self.layoutNeedsRc(layout_idx)
                            else
                                true;
                            if (needs_rc) {
                                try decrefs_needed.append(pattern_idx.*);
                            }
                        }
                    }
                }
            }

            // If we need decrefs, wrap body in a block with decref statements
            if (decrefs_needed.items.len > 0) {
                any_changed = true;
                body = try self.wrapWithDecrefs(body, decrefs_needed.items, region);
            }

            // Check if branch changed
            if (body != branch.value) {
                any_changed = true;
            }

            const new_branch = try self.module_env.store.addMatchBranch(.{
                .patterns = branch.patterns,
                .value = body,
                .guard = branch.guard,
                .redundant = branch.redundant,
            }, region);
            try self.module_env.store.addScratchMatchBranch(new_branch);
        }

        // Merge consumption from all branches
        var final_iter = all_consumed.keyIterator();
        while (final_iter.next()) |pattern_idx| {
            if (self.symbol_states.getPtr(pattern_idx.*)) |state| {
                state.consumed = true;
            }
        }

        if (!any_changed) {
            self.module_env.store.clearScratchFrom("match_branches", branch_start);
            return expr_idx;
        }

        const new_branches = try self.module_env.store.matchBranchSpanFrom(branch_start);
        return try self.module_env.store.addExpr(
            .{ .e_match = .{
                .cond = new_matched,
                .branches = new_branches,
                .exhaustive = match.exhaustive,
                .is_try_suffix = match.is_try_suffix,
            } },
            region,
        );
    }

    fn transformBinop(self: *Self, expr_idx: CIR.Expr.Idx, binop: anytype) !CIR.Expr.Idx {
        const new_lhs = try self.transformExpr(binop.lhs);
        const new_rhs = try self.transformExpr(binop.rhs);
        if (new_lhs != binop.lhs or new_rhs != binop.rhs) {
            return try self.module_env.store.addExpr(
                .{ .e_binop = .{
                    .op = binop.op,
                    .lhs = new_lhs,
                    .rhs = new_rhs,
                } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformUnaryMinus(self: *Self, expr_idx: CIR.Expr.Idx, unary: anytype) !CIR.Expr.Idx {
        const new_expr = try self.transformExpr(unary.expr);
        if (new_expr != unary.expr) {
            return try self.module_env.store.addExpr(
                .{ .e_unary_minus = .{ .expr = new_expr } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformUnaryNot(self: *Self, expr_idx: CIR.Expr.Idx, unary: anytype) !CIR.Expr.Idx {
        const new_expr = try self.transformExpr(unary.expr);
        if (new_expr != unary.expr) {
            return try self.module_env.store.addExpr(
                .{ .e_unary_not = .{ .expr = new_expr } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformList(self: *Self, expr_idx: CIR.Expr.Idx, list: anytype) !CIR.Expr.Idx {
        const elems = self.module_env.store.sliceExpr(list.elems);
        var any_changed = false;
        const elem_start = self.module_env.store.scratchExprTop();
        for (elems) |elem_idx| {
            const new_elem = try self.transformExpr(elem_idx);
            if (new_elem != elem_idx) any_changed = true;
            try self.module_env.store.addScratchExpr(new_elem);
        }
        if (!any_changed) {
            self.module_env.store.clearScratchExprsFrom(elem_start);
            return expr_idx;
        }
        const new_elems = try self.module_env.store.exprSpanFrom(elem_start);
        return try self.module_env.store.addExpr(
            .{ .e_list = .{ .elems = new_elems } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    fn transformTuple(self: *Self, expr_idx: CIR.Expr.Idx, tuple: anytype) !CIR.Expr.Idx {
        const elems = self.module_env.store.sliceExpr(tuple.elems);
        var any_changed = false;
        const elem_start = self.module_env.store.scratchExprTop();
        for (elems) |elem_idx| {
            const new_elem = try self.transformExpr(elem_idx);
            if (new_elem != elem_idx) any_changed = true;
            try self.module_env.store.addScratchExpr(new_elem);
        }
        if (!any_changed) {
            self.module_env.store.clearScratchExprsFrom(elem_start);
            return expr_idx;
        }
        const new_elems = try self.module_env.store.exprSpanFrom(elem_start);
        return try self.module_env.store.addExpr(
            .{ .e_tuple = .{ .elems = new_elems } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    fn transformRecord(self: *Self, expr_idx: CIR.Expr.Idx, record: anytype) !CIR.Expr.Idx {
        const field_idxs = self.module_env.store.sliceRecordFields(record.fields);
        var any_changed = false;
        const field_start = self.module_env.store.scratchTop("record_fields");
        for (field_idxs) |field_idx| {
            const field = self.module_env.store.getRecordField(field_idx);
            const new_value = try self.transformExpr(field.value);
            if (new_value != field.value) any_changed = true;
            const new_field = try self.module_env.store.addRecordField(.{
                .name = field.name,
                .value = new_value,
            }, self.module_env.store.getExprRegion(expr_idx));
            try self.module_env.store.addScratch("record_fields", new_field);
        }

        // Handle record update syntax: { ..base, field: value }
        var new_ext: ?CIR.Expr.Idx = record.ext;
        if (record.ext) |ext_expr| {
            const transformed_ext = try self.transformExpr(ext_expr);
            if (transformed_ext != ext_expr) {
                any_changed = true;
                new_ext = transformed_ext;
            }
        }

        if (!any_changed) {
            self.module_env.store.clearScratchFrom("record_fields", field_start);
            return expr_idx;
        }
        const new_fields = try self.module_env.store.recordFieldSpanFrom(field_start);
        return try self.module_env.store.addExpr(
            .{ .e_record = .{ .fields = new_fields, .ext = new_ext } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    fn transformTag(self: *Self, expr_idx: CIR.Expr.Idx, tag: anytype) !CIR.Expr.Idx {
        const args = self.module_env.store.sliceExpr(tag.args);
        var any_changed = false;
        const arg_start = self.module_env.store.scratchExprTop();
        for (args) |arg_idx| {
            const new_arg = try self.transformExpr(arg_idx);
            if (new_arg != arg_idx) any_changed = true;
            try self.module_env.store.addScratchExpr(new_arg);
        }
        if (!any_changed) {
            self.module_env.store.clearScratchExprsFrom(arg_start);
            return expr_idx;
        }
        const new_args = try self.module_env.store.exprSpanFrom(arg_start);
        return try self.module_env.store.addExpr(
            .{ .e_tag = .{
                .name = tag.name,
                .args = new_args,
            } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    fn transformNominal(self: *Self, expr_idx: CIR.Expr.Idx, nom: anytype) !CIR.Expr.Idx {
        const new_backing = try self.transformExpr(nom.backing_expr);
        if (new_backing != nom.backing_expr) {
            return try self.module_env.store.addExpr(
                .{ .e_nominal = .{
                    .nominal_type_decl = nom.nominal_type_decl,
                    .backing_expr = new_backing,
                    .backing_type = nom.backing_type,
                } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformNominalExternal(self: *Self, expr_idx: CIR.Expr.Idx, nom: anytype) !CIR.Expr.Idx {
        const new_backing = try self.transformExpr(nom.backing_expr);
        if (new_backing != nom.backing_expr) {
            return try self.module_env.store.addExpr(
                .{ .e_nominal_external = .{
                    .module_idx = nom.module_idx,
                    .target_node_idx = nom.target_node_idx,
                    .backing_expr = new_backing,
                    .backing_type = nom.backing_type,
                } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformDotAccess(self: *Self, expr_idx: CIR.Expr.Idx, dot: anytype) !CIR.Expr.Idx {
        var any_changed = false;
        const new_receiver = try self.transformExpr(dot.receiver);
        if (new_receiver != dot.receiver) any_changed = true;

        var new_args: ?CIR.Expr.Span = dot.args;
        if (dot.args) |args| {
            const arg_exprs = self.module_env.store.sliceExpr(args);
            const arg_start = self.module_env.store.scratchExprTop();
            for (arg_exprs) |arg_idx| {
                const new_arg = try self.transformExpr(arg_idx);
                if (new_arg != arg_idx) any_changed = true;
                try self.module_env.store.addScratchExpr(new_arg);
            }
            if (any_changed) {
                new_args = try self.module_env.store.exprSpanFrom(arg_start);
            } else {
                self.module_env.store.clearScratchExprsFrom(arg_start);
            }
        }

        if (!any_changed) return expr_idx;

        return try self.module_env.store.addExpr(
            .{ .e_dot_access = .{
                .receiver = new_receiver,
                .field_name = dot.field_name,
                .field_name_region = dot.field_name_region,
                .args = new_args,
            } },
            self.module_env.store.getExprRegion(expr_idx),
        );
    }

    fn transformDbg(self: *Self, expr_idx: CIR.Expr.Idx, dbg: anytype) !CIR.Expr.Idx {
        const new_expr = try self.transformExpr(dbg.expr);
        if (new_expr != dbg.expr) {
            return try self.module_env.store.addExpr(
                .{ .e_dbg = .{ .expr = new_expr } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformExpect(self: *Self, expr_idx: CIR.Expr.Idx, exp: anytype) !CIR.Expr.Idx {
        const new_body = try self.transformExpr(exp.body);
        if (new_body != exp.body) {
            return try self.module_env.store.addExpr(
                .{ .e_expect = .{ .body = new_body } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformFor(self: *Self, expr_idx: CIR.Expr.Idx, for_expr: anytype) !CIR.Expr.Idx {
        const new_iter = try self.transformExpr(for_expr.expr);
        const new_body = try self.transformExpr(for_expr.body);
        if (new_iter != for_expr.expr or new_body != for_expr.body) {
            return try self.module_env.store.addExpr(
                .{ .e_for = .{
                    .patt = for_expr.patt,
                    .expr = new_iter,
                    .body = new_body,
                } },
                self.module_env.store.getExprRegion(expr_idx),
            );
        }
        return expr_idx;
    }

    fn transformReturn(self: *Self, expr_idx: CIR.Expr.Idx, ret: anytype) !CIR.Expr.Idx {
        const region = self.module_env.store.getExprRegion(expr_idx);
        const new_expr = try self.transformExpr(ret.expr);

        // Return CONSUMES the returned value - ownership transfers to caller.
        // If the return expression is a lookup, mark that pattern as consumed
        // so we don't decref it (the caller takes ownership).
        // See Rust's inc_dec.rs Stmt::Ret handling.
        const original_expr = self.module_env.store.getExpr(ret.expr);
        if (original_expr == .e_lookup_local) {
            if (self.symbol_states.getPtr(original_expr.e_lookup_local.pattern_idx)) |state| {
                state.consumed = true;
            }
        }

        // Collect all owned, unconsumed patterns that need decref from all enclosing scopes
        var decrefs_needed = std.array_list.Managed(CIR.Pattern.Idx).init(self.allocator);
        defer decrefs_needed.deinit();

        // Walk up the scope chain
        var scope = self.current_scope;
        while (scope) |s| {
            for (s.introduced_patterns.items) |pattern_idx| {
                if (self.symbol_states.get(pattern_idx)) |state| {
                    if (state.ownership == .owned and !state.consumed) {
                        const needs_rc = if (state.layout_idx) |layout_idx|
                            self.layoutNeedsRc(layout_idx)
                        else
                            true;
                        if (needs_rc) {
                            try decrefs_needed.append(pattern_idx);
                        }
                    }
                }
            }
            scope = s.parent;
        }

        // If we need decrefs, wrap the return:
        // { decref x; decref y; return expr }
        if (decrefs_needed.items.len > 0) {
            const stmt_start = self.module_env.store.scratchTop("statements");

            // Add decref statements for each pattern
            for (decrefs_needed.items) |pattern_idx| {
                const decref_expr = try self.module_env.store.addExpr(
                    .{ .e_decref = .{
                        .pattern_idx = pattern_idx,
                    } },
                    region,
                );
                const decref_stmt = try self.module_env.store.addStatement(
                    .{ .s_expr = .{ .expr = decref_expr } },
                    region,
                );
                try self.module_env.store.addScratchStatement(decref_stmt);
            }

            const stmts_span = try self.module_env.store.statementSpanFrom(stmt_start);

            // Create the return expression with possibly transformed inner expression
            const return_expr = try self.module_env.store.addExpr(
                .{ .e_return = .{ .expr = new_expr } },
                region,
            );

            // Create block: { decref1; decref2; ...; return expr }
            return try self.module_env.store.addExpr(
                .{ .e_block = .{
                    .stmts = stmts_span,
                    .final_expr = return_expr,
                } },
                region,
            );
        }

        // No decrefs needed, just transform the return expression if needed
        if (new_expr != ret.expr) {
            return try self.module_env.store.addExpr(
                .{ .e_return = .{ .expr = new_expr } },
                region,
            );
        }
        return expr_idx;
    }

    // ============================================================================
    // Branch Reconciliation: Snapshot and Restore Symbol States
    // ============================================================================

    /// Snapshot the current state of all symbols for branch reconciliation.
    /// Returns a list of snapshots that can be used to restore/reconcile after branches.
    fn snapshotSymbolStates(self: *const Self) std.mem.Allocator.Error![]SymbolStateSnapshot {
        var snapshots = std.array_list.Managed(SymbolStateSnapshot).init(self.allocator);
        errdefer snapshots.deinit();

        var iter = self.symbol_states.iterator();
        while (iter.next()) |entry| {
            const state = entry.value_ptr.*;
            try snapshots.append(.{
                .pattern_idx = state.pattern_idx,
                .remaining_uses = state.remaining_uses,
                .consumed = state.consumed,
            });
        }
        return snapshots.toOwnedSlice();
    }

    /// Restore symbol states from a snapshot.
    /// Used before processing each branch to start from the same state.
    fn restoreSymbolStates(self: *Self, snapshots: []const SymbolStateSnapshot) void {
        for (snapshots) |snapshot| {
            if (self.symbol_states.getPtr(snapshot.pattern_idx)) |state| {
                state.remaining_uses = snapshot.remaining_uses;
                state.consumed = snapshot.consumed;
            }
        }
    }

    /// Get the set of patterns that have been consumed (consumed = true)
    fn getConsumedPatterns(self: *const Self) std.mem.Allocator.Error!std.AutoHashMap(CIR.Pattern.Idx, void) {
        var consumed = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
        errdefer consumed.deinit();

        var iter = self.symbol_states.iterator();
        while (iter.next()) |entry| {
            const state = entry.value_ptr.*;
            if (state.consumed) {
                try consumed.put(state.pattern_idx, {});
            }
        }
        return consumed;
    }

    // ============================================================================
    // Helper methods
    // ============================================================================

    /// Check if a layout needs reference counting
    pub fn layoutNeedsRc(self: *const Self, layout_idx: layout_mod.Idx) bool {
        const layout = self.layout_store.getLayout(layout_idx);
        return self.layout_store.layoutContainsRefcounted(layout);
    }

    /// Get statistics about symbol usage for debugging
    pub fn getStats(self: *const Self) Stats {
        var total_symbols: u32 = 0;
        var owned_symbols: u32 = 0;
        var consumed_symbols: u32 = 0;
        var multi_use_symbols: u32 = 0;

        var iter = self.symbol_states.iterator();
        while (iter.next()) |entry| {
            const state = entry.value_ptr.*;
            total_symbols += 1;
            if (state.ownership == .owned) owned_symbols += 1;
            if (state.consumed) consumed_symbols += 1;
            if (state.remaining_uses > 1) multi_use_symbols += 1;
        }

        return Stats{
            .total_symbols = total_symbols,
            .owned_symbols = owned_symbols,
            .consumed_symbols = consumed_symbols,
            .multi_use_symbols = multi_use_symbols,
        };
    }

    pub const Stats = struct {
        total_symbols: u32,
        owned_symbols: u32,
        consumed_symbols: u32,
        multi_use_symbols: u32,
    };
};

// Tests
test "InsertPass types compile" {
    // Verify the types compile correctly.
    // Full integration tests require ModuleEnv and LayoutStore which are
    // complex to set up in isolation.
    try std.testing.expect(@sizeOf(InsertPass) > 0);
    try std.testing.expect(@sizeOf(SymbolState) > 0);
    try std.testing.expect(@sizeOf(Ownership) == 1);
    try std.testing.expect(@sizeOf(ScopeInfo) > 0);
}
