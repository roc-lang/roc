//! Scope reconstruction for LSP code completion.
//!
//! Scopes are not persisted after canonicalization - they exist only during the
//! canonicalization pass. To provide completions for local variables in scope at
//! a cursor position, we need to reconstruct scope information by traversing the CIR.
//!
//! This module provides `ScopeMap` which maps regions to the bindings they introduce,
//! allowing queries like "what variables are visible at offset X?"

const std = @import("std");
const base = @import("base");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

/// A single binding introduced in a scope.
pub const Binding = struct {
    /// The identifier name
    ident: Ident.Idx,
    /// The pattern that introduced this binding
    pattern_idx: CIR.Pattern.Idx,
    /// The region where this binding is visible (from definition to end of scope)
    visible_from: u32,
    /// The region where this binding's scope ends
    visible_to: u32,
    /// Whether this is a function parameter
    is_parameter: bool,
};

/// Reconstructed scope information from CIR.
/// Allows querying which bindings are visible at a given offset.
pub const ScopeMap = struct {
    /// All bindings found, sorted by visible_from offset
    bindings: std.ArrayList(Binding),
    allocator: Allocator,

    pub fn init(allocator: Allocator) ScopeMap {
        return .{
            .bindings = std.ArrayList(Binding){},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ScopeMap) void {
        self.bindings.deinit(self.allocator);
    }

    /// Build scope map by traversing all CIR structures in the module.
    pub fn build(self: *ScopeMap, module_env: *ModuleEnv) !void {
        // Process top-level definitions
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            // Top-level defs are visible from their definition to the end of the module
            try self.extractBindingsFromPattern(module_env, def.pattern, 0, std.math.maxInt(u32), false, 0);

            // Also traverse the expression to find nested scopes
            try self.traverseExpr(module_env, def.expr, std.math.maxInt(u32), 0);
        }

        // Process top-level statements
        const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (statements_slice) |stmt_idx| {
            try self.processStatement(module_env, stmt_idx, std.math.maxInt(u32), 0);
        }

        // Sort bindings by visible_from offset for efficient querying
        std.mem.sort(Binding, self.bindings.items, {}, struct {
            fn lessThan(_: void, a: Binding, b: Binding) bool {
                return a.visible_from < b.visible_from;
            }
        }.lessThan);
    }

    /// Check if a binding is visible at the given offset
    pub fn isVisibleAt(binding: Binding, offset: u32) bool {
        return binding.visible_from <= offset and offset < binding.visible_to;
    }

    /// Process a statement and extract any bindings it introduces.
    fn processStatement(self: *ScopeMap, module_env: *ModuleEnv, stmt_idx: CIR.Statement.Idx, scope_end: u32, depth: usize) Allocator.Error!void {
        if (depth > 128) return;

        const stmt = module_env.store.getStatement(stmt_idx);
        const stmt_region = module_env.store.getStatementRegion(stmt_idx);

        switch (stmt) {
            .s_decl => |decl| {
                // Variable binding - visible from definition to end of scope
                try self.extractBindingsFromPattern(module_env, decl.pattern, stmt_region.start.offset, scope_end, false, depth + 1);
                // Traverse the expression for nested scopes
                try self.traverseExpr(module_env, decl.expr, scope_end, depth + 1);
            },

            .s_var => |var_decl| {
                // Mutable variable binding
                try self.extractBindingsFromPattern(module_env, var_decl.pattern_idx, stmt_region.start.offset, scope_end, false, depth + 1);
                try self.traverseExpr(module_env, var_decl.expr, scope_end, depth + 1);
            },
            .s_reassign => |reassign| {
                // Reassignment doesn't introduce new bindings, but traverse the expr
                try self.traverseExpr(module_env, reassign.expr, scope_end, depth + 1);
            },
            .s_for => |for_stmt| {
                // For loop - pattern is visible within the body
                const body_region = module_env.store.getExprRegion(for_stmt.body);
                try self.extractBindingsFromPattern(module_env, for_stmt.patt, body_region.start.offset, body_region.end.offset, false, depth + 1);
                // Traverse the iterable expression and body
                try self.traverseExpr(module_env, for_stmt.expr, scope_end, depth + 1);
                try self.traverseExpr(module_env, for_stmt.body, body_region.end.offset, depth + 1);
            },
            .s_while => |while_stmt| {
                try self.traverseExpr(module_env, while_stmt.cond, scope_end, depth + 1);
                try self.traverseExpr(module_env, while_stmt.body, scope_end, depth + 1);
            },
            .s_expr => |expr_stmt| {
                try self.traverseExpr(module_env, expr_stmt.expr, scope_end, depth + 1);
            },
            .s_expect => |expect_stmt| {
                try self.traverseExpr(module_env, expect_stmt.body, scope_end, depth + 1);
            },
            .s_dbg => |dbg_stmt| {
                try self.traverseExpr(module_env, dbg_stmt.expr, scope_end, depth + 1);
            },
            .s_return => |return_stmt| {
                try self.traverseExpr(module_env, return_stmt.expr, scope_end, depth + 1);
            },
            // Type declarations, imports, etc. don't introduce variable bindings
            .s_import, .s_alias_decl, .s_nominal_decl, .s_crash, .s_break, .s_type_anno, .s_type_var_alias, .s_runtime_error => {},
        }
    }

    /// Traverse an expression and extract bindings from nested scopes.
    fn traverseExpr(self: *ScopeMap, module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx, scope_end: u32, depth: usize) Allocator.Error!void {
        if (depth > 128) return;

        const expr = module_env.store.getExpr(expr_idx);

        switch (expr) {
            .e_block => |block| {
                // Block introduces a new scope
                const block_region = module_env.store.getExprRegion(expr_idx);
                const block_end = block_region.end.offset;

                // Process statements in order - each binding is visible to subsequent statements
                const stmts = module_env.store.sliceStatements(block.stmts);
                for (stmts) |stmt_idx| {
                    try self.processStatement(module_env, stmt_idx, block_end, depth + 1);
                }

                // Traverse the final expression
                try self.traverseExpr(module_env, block.final_expr, block_end, depth + 1);
            },
            .e_lambda => |lambda| {
                // Lambda parameters are visible within the body
                const body_region = module_env.store.getExprRegion(lambda.body);
                const args = module_env.store.slicePatterns(lambda.args);
                for (args) |arg_pattern| {
                    try self.extractBindingsFromPattern(module_env, arg_pattern, body_region.start.offset, body_region.end.offset, true, depth + 1);
                }
                // Traverse the body
                try self.traverseExpr(module_env, lambda.body, body_region.end.offset, depth + 1);
            },
            .e_closure => |closure| {
                // Closure wraps a lambda - get the lambda and process it
                const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
                if (lambda_expr == .e_lambda) {
                    const lambda = lambda_expr.e_lambda;
                    const body_region = module_env.store.getExprRegion(lambda.body);
                    const args = module_env.store.slicePatterns(lambda.args);
                    for (args) |arg_pattern| {
                        try self.extractBindingsFromPattern(module_env, arg_pattern, body_region.start.offset, body_region.end.offset, true, depth + 1);
                    }
                    try self.traverseExpr(module_env, lambda.body, body_region.end.offset, depth + 1);
                }
            },
            .e_match => |match| {
                // Traverse the condition
                try self.traverseExpr(module_env, match.cond, scope_end, depth + 1);

                // Each branch's patterns introduce bindings visible in that branch's body
                const branches = module_env.store.sliceMatchBranches(match.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getMatchBranch(branch_idx);
                    const body_region = module_env.store.getExprRegion(branch.value);

                    // Extract bindings from all patterns in this branch
                    const patterns = module_env.store.sliceMatchBranchPatterns(branch.patterns);
                    for (patterns) |branch_pattern_idx| {
                        const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
                        try self.extractBindingsFromPattern(module_env, branch_pattern.pattern, body_region.start.offset, body_region.end.offset, false, depth + 1);
                    }

                    // Traverse the guard if present
                    if (branch.guard) |guard_idx| {
                        try self.traverseExpr(module_env, guard_idx, body_region.end.offset, depth + 1);
                    }

                    // Traverse the branch body
                    try self.traverseExpr(module_env, branch.value, body_region.end.offset, depth + 1);
                }
            },
            .e_if => |if_expr| {
                // Traverse all branches
                const branches = module_env.store.sliceIfBranches(if_expr.branches);
                for (branches) |branch_idx| {
                    const branch = module_env.store.getIfBranch(branch_idx);
                    try self.traverseExpr(module_env, branch.cond, scope_end, depth + 1);
                    try self.traverseExpr(module_env, branch.body, scope_end, depth + 1);
                }
                // Traverse the else branch
                try self.traverseExpr(module_env, if_expr.final_else, scope_end, depth + 1);
            },
            .e_call => |call| {
                try self.traverseExpr(module_env, call.func, scope_end, depth + 1);
                const args = module_env.store.sliceExpr(call.args);
                for (args) |arg_idx| {
                    try self.traverseExpr(module_env, arg_idx, scope_end, depth + 1);
                }
            },
            .e_binop => |binop| {
                try self.traverseExpr(module_env, binop.lhs, scope_end, depth + 1);
                try self.traverseExpr(module_env, binop.rhs, scope_end, depth + 1);
            },
            .e_unary_minus => |unary| {
                try self.traverseExpr(module_env, unary.expr, scope_end, depth + 1);
            },
            .e_unary_not => |unary| {
                try self.traverseExpr(module_env, unary.expr, scope_end, depth + 1);
            },
            .e_list => |list| {
                const elems = module_env.store.sliceExpr(list.elems);
                for (elems) |elem_idx| {
                    try self.traverseExpr(module_env, elem_idx, scope_end, depth + 1);
                }
            },
            .e_tuple => |tuple| {
                const elems = module_env.store.sliceExpr(tuple.elems);
                for (elems) |elem_idx| {
                    try self.traverseExpr(module_env, elem_idx, scope_end, depth + 1);
                }
            },
            .e_record => |record| {
                const fields = module_env.store.sliceRecordFields(record.fields);
                for (fields) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    try self.traverseExpr(module_env, field.value, scope_end, depth + 1);
                }
            },
            .e_dot_access => |dot| {
                try self.traverseExpr(module_env, dot.receiver, scope_end, depth + 1);
            },
            .e_tuple_access => |ta| {
                try self.traverseExpr(module_env, ta.tuple, scope_end, depth + 1);
            },
            .e_str => |str| {
                const segments = module_env.store.sliceExpr(str.span);
                for (segments) |seg_idx| {
                    try self.traverseExpr(module_env, seg_idx, scope_end, depth + 1);
                }
            },
            .e_tag => |tag| {
                const args = module_env.store.sliceExpr(tag.args);
                for (args) |arg_idx| {
                    try self.traverseExpr(module_env, arg_idx, scope_end, depth + 1);
                }
            },
            .e_nominal => |nominal| {
                try self.traverseExpr(module_env, nominal.backing_expr, scope_end, depth + 1);
            },
            .e_nominal_external => |nominal| {
                try self.traverseExpr(module_env, nominal.backing_expr, scope_end, depth + 1);
            },
            .e_hosted_lambda => |hosted| {
                // Hosted lambda parameters are visible within the body
                const body_region = module_env.store.getExprRegion(hosted.body);
                const args = module_env.store.slicePatterns(hosted.args);
                for (args) |arg_pattern| {
                    try self.extractBindingsFromPattern(module_env, arg_pattern, body_region.start.offset, body_region.end.offset, true, depth + 1);
                }
                try self.traverseExpr(module_env, hosted.body, body_region.end.offset, depth + 1);
            },
            .e_for => |for_expr| {
                // For loop variable is visible within the body
                const body_region = module_env.store.getExprRegion(for_expr.body);
                try self.extractBindingsFromPattern(module_env, for_expr.patt, body_region.start.offset, body_region.end.offset, false, depth + 1);
                try self.traverseExpr(module_env, for_expr.expr, scope_end, depth + 1);
                try self.traverseExpr(module_env, for_expr.body, body_region.end.offset, depth + 1);
            },
            .e_dbg => |dbg_expr| {
                try self.traverseExpr(module_env, dbg_expr.expr, scope_end, depth + 1);
            },
            .e_expect => |expect_expr| {
                try self.traverseExpr(module_env, expect_expr.body, scope_end, depth + 1);
            },
            .e_return => |ret| {
                try self.traverseExpr(module_env, ret.expr, scope_end, depth + 1);
            },
            .e_run_low_level => |low_level| {
                const args = module_env.store.sliceExpr(low_level.args);
                for (args) |arg_idx| {
                    try self.traverseExpr(module_env, arg_idx, scope_end, depth + 1);
                }
            },
            // Leaf expressions - no nested scopes
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_typed_int,
            .e_typed_frac,
            .e_str_segment,
            .e_lookup_local,
            .e_lookup_external,
            .e_lookup_required,
            .e_lookup_pending,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            .e_type_var_dispatch,
            => {},
        }
    }

    /// Extract bindings from a pattern (handles destructuring, as-patterns, etc.)
    fn extractBindingsFromPattern(
        self: *ScopeMap,
        module_env: *ModuleEnv,
        pattern_idx: CIR.Pattern.Idx,
        visible_from: u32,
        visible_to: u32,
        is_parameter: bool,
        depth: usize,
    ) Allocator.Error!void {
        if (depth > 128) return;

        const pattern = module_env.store.getPattern(pattern_idx);

        switch (pattern) {
            .assign => |p| {
                try self.bindings.append(self.allocator, .{
                    .ident = p.ident,
                    .pattern_idx = pattern_idx,
                    .visible_from = visible_from,
                    .visible_to = visible_to,
                    .is_parameter = is_parameter,
                });
            },
            .as => |p| {
                // The identifier is bound
                try self.bindings.append(self.allocator, .{
                    .ident = p.ident,
                    .pattern_idx = pattern_idx,
                    .visible_from = visible_from,
                    .visible_to = visible_to,
                    .is_parameter = is_parameter,
                });
                // Also extract from the nested pattern
                try self.extractBindingsFromPattern(module_env, p.pattern, visible_from, visible_to, is_parameter, depth + 1);
            },
            .applied_tag => |p| {
                // Extract bindings from tag arguments
                const args = module_env.store.slicePatterns(p.args);
                for (args) |arg_pattern| {
                    try self.extractBindingsFromPattern(module_env, arg_pattern, visible_from, visible_to, is_parameter, depth + 1);
                }
            },
            .nominal => |p| {
                // Extract bindings from the backing pattern
                try self.extractBindingsFromPattern(module_env, p.backing_pattern, visible_from, visible_to, is_parameter, depth + 1);
            },
            .nominal_external => |p| {
                try self.extractBindingsFromPattern(module_env, p.backing_pattern, visible_from, visible_to, is_parameter, depth + 1);
            },
            .record_destructure => |p| {
                const destructs = module_env.store.sliceRecordDestructs(p.destructs);
                for (destructs) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    // The field identifier is bound
                    try self.bindings.append(self.allocator, .{
                        .ident = destruct.ident,
                        .pattern_idx = pattern_idx,
                        .visible_from = visible_from,
                        .visible_to = visible_to,
                        .is_parameter = is_parameter,
                    });

                    switch (destruct.kind) {
                        .SubPattern => |nested_pattern| {
                            try self.extractBindingsFromPattern(module_env, nested_pattern, visible_from, visible_to, is_parameter, depth + 1);
                        },
                        .Required => {},
                    }
                }
            },
            .list => |p| {
                const patterns = module_env.store.slicePatterns(p.patterns);
                for (patterns) |elem_pattern| {
                    try self.extractBindingsFromPattern(module_env, elem_pattern, visible_from, visible_to, is_parameter, depth + 1);
                }
                // Handle rest pattern if present (e.g., [first, .. as rest])
                if (p.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern| {
                        try self.extractBindingsFromPattern(module_env, rest_pattern, visible_from, visible_to, is_parameter, depth + 1);
                    }
                }
            },
            .tuple => |p| {
                const patterns = module_env.store.slicePatterns(p.patterns);
                for (patterns) |elem_pattern| {
                    try self.extractBindingsFromPattern(module_env, elem_pattern, visible_from, visible_to, is_parameter, depth + 1);
                }
            },
            // Literal and other patterns don't introduce bindings
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }
};
