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
const CirVisitor = @import("cir_visitor.zig").CirVisitor;
const VisitAction = @import("cir_visitor.zig").VisitAction;
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
            .bindings = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ScopeMap) void {
        self.bindings.deinit(self.allocator);
    }

    /// Build scope map by traversing all CIR structures in the module.
    pub fn build(self: *ScopeMap, module_env: *ModuleEnv) Allocator.Error!void {
        var walk = ScopeWalk{ .scope_map = self, .module_env = module_env };
        var visitor = CirVisitor(ScopeWalk).init(&walk, .{
            .visit_expr_pre = ScopeWalk.visitExpr,
            .visit_stmt_pre = ScopeWalk.visitStmt,
        });
        const store = &module_env.store;

        // Top-level defs are visible from their definition to the end of the module
        for (store.sliceDefs(module_env.all_defs)) |def_idx| {
            const def = store.getDef(def_idx);
            try self.extractBindingsFromPattern(module_env, def.pattern, 0, std.math.maxInt(u32), false);
            visitor.walkExpr(store, def.expr);
            if (walk.oom) |err| return err;
        }

        for (store.sliceStatements(module_env.all_statements)) |stmt_idx| {
            try self.extractStatementBindings(module_env, stmt_idx, std.math.maxInt(u32));
            visitor.walkStatement(store, stmt_idx);
            if (walk.oom) |err| return err;
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

    /// Visitor context that records the bindings each scope-introducing node
    /// adds as the CIR walk reaches it.
    const ScopeWalk = struct {
        scope_map: *ScopeMap,
        module_env: *ModuleEnv,
        oom: ?Allocator.Error = null,

        fn visitExpr(walk: *ScopeWalk, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) VisitAction {
            walk.recordExprBindings(expr_idx, expr) catch |err| {
                walk.oom = err;
                return .stop;
            };
            return .continue_traversal;
        }

        fn visitStmt(walk: *ScopeWalk, _: CIR.Statement.Idx, stmt: CIR.Statement) VisitAction {
            switch (stmt) {
                .s_for => |for_stmt| walk.bindInBody(for_stmt.patt, for_stmt.body, false) catch |err| {
                    walk.oom = err;
                    return .stop;
                },
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_expr,
                .s_expect,
                .s_dbg,
                .s_return,
                .s_import,
                .s_alias_decl,
                .s_nominal_decl,
                .s_where_alias_decl,
                .s_crash,
                .s_break,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => {},
            }
            return .continue_traversal;
        }

        /// Records `pattern`'s bindings as visible throughout `body`.
        fn bindInBody(walk: *ScopeWalk, pattern: CIR.Pattern.Idx, body: CIR.Expr.Idx, is_parameter: bool) Allocator.Error!void {
            const body_region = walk.module_env.store.getExprRegion(body);
            try walk.scope_map.extractBindingsFromPattern(walk.module_env, pattern, body_region.start.offset, body_region.end.offset, is_parameter);
        }

        fn recordExprBindings(walk: *ScopeWalk, expr_idx: CIR.Expr.Idx, expr: CIR.Expr) Allocator.Error!void {
            const store = &walk.module_env.store;
            switch (expr) {
                .e_block => |block| {
                    // Each statement binding is visible from its statement to the end of the block
                    const block_end = store.getExprRegion(expr_idx).end.offset;
                    for (store.sliceStatements(block.stmts)) |stmt_idx| {
                        try walk.scope_map.extractStatementBindings(walk.module_env, stmt_idx, block_end);
                    }
                },
                .e_lambda => |lambda| {
                    for (store.slicePatterns(lambda.args)) |arg_pattern| {
                        try walk.bindInBody(arg_pattern, lambda.body, true);
                    }
                },
                .e_match => |match| {
                    // Each branch's patterns introduce bindings visible in that branch's body
                    for (store.sliceMatchBranches(match.branches)) |branch_idx| {
                        const branch = store.getMatchBranch(branch_idx);
                        for (store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                            try walk.bindInBody(store.getMatchBranchPattern(branch_pattern_idx).pattern, branch.value, false);
                        }
                    }
                },
                .e_for => |for_expr| try walk.bindInBody(for_expr.patt, for_expr.body, false),
                .e_closure,
                .e_if,
                .e_call,
                .e_binop,
                .e_unary_minus,
                .e_list,
                .e_tuple,
                .e_record,
                .e_field_access,
                .e_method_call,
                .e_dispatch_call,
                .e_interpolation,
                .e_structural_eq,
                .e_structural_hash,
                .e_method_eq,
                .e_type_method_call,
                .e_type_dispatch_call,
                .e_tuple_access,
                .e_str,
                .e_tag,
                .e_nominal,
                .e_nominal_external,
                .e_hosted_lambda,
                .e_dbg,
                .e_expect_err,
                .e_expect,
                .e_return,
                .e_run_low_level,
                .e_num,
                .e_frac_f32,
                .e_frac_f64,
                .e_dec,
                .e_dec_small,
                .e_num_from_numeral,
                .e_typed_int,
                .e_typed_frac,
                .e_typed_num_from_numeral,
                .e_str_segment,
                .e_lookup_local,
                .e_lookup_external,
                .e_lookup_associated_local,
                .e_lookup_associated,
                .e_lookup_associated_resolved,
                .e_lookup_required,
                .e_empty_list,
                .e_empty_record,
                .e_zero_argument_tag,
                .e_runtime_error,
                .e_crash,
                .e_ellipsis,
                .e_anno_only,
                .e_derived_method,
                .e_break,
                .e_bytes_literal,
                => {},
                .e_deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference reached a stage that runs after import resolution", .{}),
            }
        }
    };

    /// Record the bindings a declaration statement introduces, visible from the
    /// statement to `scope_end`.
    fn extractStatementBindings(self: *ScopeMap, module_env: *ModuleEnv, stmt_idx: CIR.Statement.Idx, scope_end: u32) Allocator.Error!void {
        const pattern = switch (module_env.store.getSourceStatement(stmt_idx)) {
            .s_decl => |decl| decl.pattern,
            inline .s_var, .s_var_uninitialized => |var_decl| var_decl.pattern_idx,
            .s_reassign,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_expr,
            .s_expect,
            .s_dbg,
            .s_return,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_where_alias_decl,
            .s_crash,
            .s_break,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => return,
        };
        const visible_from = module_env.store.getStatementRegion(stmt_idx).start.offset;
        try self.extractBindingsFromPattern(module_env, pattern, visible_from, scope_end, false);
    }

    /// Extract bindings from a pattern (handles destructuring, as-patterns, etc.)
    fn extractBindingsFromPattern(
        self: *ScopeMap,
        module_env: *ModuleEnv,
        pattern_idx: CIR.Pattern.Idx,
        visible_from: u32,
        visible_to: u32,
        is_parameter: bool,
    ) Allocator.Error!void {
        const pattern = module_env.store.getSourcePattern(pattern_idx);

        switch (pattern) {
            inline .assign, .var_assign => |p| {
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
                try self.extractBindingsFromPattern(module_env, p.pattern, visible_from, visible_to, is_parameter);
            },
            .applied_tag => |p| {
                // Extract bindings from tag arguments
                const args = module_env.store.slicePatterns(p.args);
                for (args) |arg_pattern| {
                    try self.extractBindingsFromPattern(module_env, arg_pattern, visible_from, visible_to, is_parameter);
                }
            },
            .nominal => |p| {
                // Extract bindings from the backing pattern
                try self.extractBindingsFromPattern(module_env, p.backing_pattern, visible_from, visible_to, is_parameter);
            },
            .nominal_external => |p| {
                try self.extractBindingsFromPattern(module_env, p.backing_pattern, visible_from, visible_to, is_parameter);
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
                            try self.extractBindingsFromPattern(module_env, nested_pattern, visible_from, visible_to, is_parameter);
                        },
                        .Rest => |nested_pattern| {
                            try self.extractBindingsFromPattern(module_env, nested_pattern, visible_from, visible_to, is_parameter);
                        },
                        .Required => {},
                    }
                }
            },
            .list => |p| {
                const patterns = module_env.store.slicePatterns(p.patterns);
                for (patterns) |elem_pattern| {
                    try self.extractBindingsFromPattern(module_env, elem_pattern, visible_from, visible_to, is_parameter);
                }
                // Handle rest pattern if present (e.g., [first, .. as rest])
                if (p.rest_info) |rest_info| {
                    if (rest_info.pattern) |rest_pattern| {
                        try self.extractBindingsFromPattern(module_env, rest_pattern, visible_from, visible_to, is_parameter);
                    }
                }
            },
            .tuple => |p| {
                const patterns = module_env.store.slicePatterns(p.patterns);
                for (patterns) |elem_pattern| {
                    try self.extractBindingsFromPattern(module_env, elem_pattern, visible_from, visible_to, is_parameter);
                }
            },
            .str_interpolation => |p| {
                var i: u32 = 0;
                while (i < p.steps.span.len) : (i += 1) {
                    const step = module_env.store.getStrPatternStep(p.steps, i);
                    if (step.capture) |capture| {
                        try self.extractBindingsFromPattern(module_env, capture, visible_from, visible_to, is_parameter);
                    }
                }
            },
            // Literal and other patterns don't introduce bindings
            .num_literal,
            .num_from_numeral_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
            .deferred_import_ref => base.invariant("compiler invariant violated: deferred import reference pattern reached a stage that runs after import resolution", .{}),
        }
    }
};

test "ScopeMap traverses a flattened field access receiver" {
    const allocator = std.testing.allocator;
    var module_env = try ModuleEnv.init(allocator, "");
    defer module_env.deinit();
    try module_env.initCIRFields("Test");

    const parameter_name = try module_env.insertIdent(Ident.for_text("parameter"));
    const parameter = try module_env.addPattern(.{ .assign = .{ .ident = parameter_name } }, base.Region.from_raw_offsets(10, 19));
    const args_start = module_env.store.scratchPatternTop();
    try module_env.store.addScratchPattern(parameter);
    const args = try module_env.store.patternSpanFrom(args_start);
    const body = try module_env.addExpr(.{ .e_empty_record = .{} }, base.Region.from_raw_offsets(20, 30));
    const receiver = try module_env.addExpr(.{ .e_lambda = .{
        .args = args,
        .body = body,
    } }, base.Region.from_raw_offsets(9, 30));

    const path = try module_env.store.startFieldAccessPath(1);
    _ = module_env.store.appendFieldAccessPathSegmentAssumeCapacity(path, .{
        .name = parameter_name,
        .mode = .optional,
    }, base.Region.from_raw_offsets(31, 38));
    const access = try module_env.addExpr(.{ .e_field_access = .{
        .receiver = receiver,
        .segments = module_env.store.finishFieldAccessPath(path),
    } }, base.Region.from_raw_offsets(9, 38));

    var scope_map = ScopeMap.init(allocator);
    defer scope_map.deinit();
    var walk = ScopeMap.ScopeWalk{ .scope_map = &scope_map, .module_env = &module_env };
    var visitor = CirVisitor(ScopeMap.ScopeWalk).init(&walk, .{ .visit_expr_pre = ScopeMap.ScopeWalk.visitExpr });
    visitor.walkExpr(&module_env.store, access);
    try std.testing.expectEqual(@as(?Allocator.Error, null), walk.oom);

    try std.testing.expectEqual(@as(usize, 1), scope_map.bindings.items.len);
    const binding = scope_map.bindings.items[0];
    try std.testing.expect(binding.ident.eql(parameter_name));
    try std.testing.expectEqual(parameter, binding.pattern_idx);
    try std.testing.expectEqual(@as(u32, 20), binding.visible_from);
    try std.testing.expectEqual(@as(u32, 30), binding.visible_to);
    try std.testing.expect(binding.is_parameter);
}
