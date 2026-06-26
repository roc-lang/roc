//! Dependency Graph and SCC computation for top-level definitions
//!
//! This module provides dependency analysis for top-level definitions to enable
//! proper evaluation ordering. It computes Strongly Connected Components (SCCs)
//! using Tarjan's algorithm and provides a topologically sorted evaluation order.
//!
//! NOTE: This handles ALL top-level definitions including:
//! - Regular top-level definitions (e.g., `foo = 42`)
//! - Associated items (e.g., `TypeName.item_name = 5` from `TypeName := T.{ item_name = 5 }`)
//!
//! Associated items are definitions nested under nominal type declarations and have
//! qualified names. They are stored in `all_defs` alongside regular top-level defs.

const std = @import("std");
const base = @import("base");
const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");

/// Represents a directed graph of dependencies between top-level definitions.
/// Edges point from dependent to dependency (A -> B means A depends on B).
pub const DependencyGraph = struct {
    /// Map from def_idx to list of def_idx it depends on
    edges: std.AutoHashMapUnmanaged(CIR.Def.Idx, std.ArrayList(CIR.Def.Idx)),

    /// All defs in the graph
    nodes: []const CIR.Def.Idx,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, defs: []const CIR.Def.Idx) DependencyGraph {
        return DependencyGraph{
            .edges = .{},
            .nodes = defs,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DependencyGraph) void {
        var iter = self.edges.valueIterator();
        while (iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.edges.deinit(self.allocator);
    }

    /// Add an edge: from_def depends on to_def
    pub fn addEdge(self: *DependencyGraph, from_def: CIR.Def.Idx, to_def: CIR.Def.Idx) std.mem.Allocator.Error!void {
        const gop = try self.edges.getOrPut(self.allocator, from_def);
        if (!gop.found_existing) {
            gop.value_ptr.* = .empty;
        }
        try gop.value_ptr.append(self.allocator, to_def);
    }

    /// Get dependencies of a def
    pub fn getDependencies(self: *const DependencyGraph, def: CIR.Def.Idx) []const CIR.Def.Idx {
        const list = self.edges.get(def) orelse return &.{};
        return list.items;
    }
};

/// A Strongly Connected Component (SCC) in the dependency graph.
/// Contains one or more definitions that may be mutually recursive.
pub const SCC = struct {
    /// Definitions in this SCC
    defs: []CIR.Def.Idx,

    /// True if this SCC contains recursion (size > 1 or has self-loop)
    is_recursive: bool,

    pub const Idx = enum(u32) { _ };
};

/// The computed evaluation order for all definitions in a module.
/// SCCs are arranged in topological order (dependencies come before dependents).
pub const EvaluationOrder = struct {
    /// SCCs in topologically sorted order
    /// (dependencies come before dependents)
    sccs: []SCC,

    allocator: std.mem.Allocator,

    pub fn clone(self: *const EvaluationOrder, allocator: std.mem.Allocator) std.mem.Allocator.Error!EvaluationOrder {
        const sccs = try allocator.alloc(SCC, self.sccs.len);
        errdefer allocator.free(sccs);

        var built: usize = 0;
        errdefer {
            for (sccs[0..built]) |scc| allocator.free(scc.defs);
        }

        for (self.sccs, 0..) |scc, i| {
            sccs[i] = .{
                .defs = try allocator.dupe(CIR.Def.Idx, scc.defs),
                .is_recursive = scc.is_recursive,
            };
            built += 1;
        }

        return .{
            .sccs = sccs,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *EvaluationOrder) void {
        for (self.sccs) |scc| {
            self.allocator.free(scc.defs);
        }
        self.allocator.free(self.sccs);
    }
};

const DemandSummary = struct {
    deps: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{},
    called_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void) = .{},

    fn deinit(self: *DemandSummary, allocator: std.mem.Allocator) void {
        self.deps.deinit(allocator);
        self.called_patterns.deinit(allocator);
    }

    fn addDep(self: *DemandSummary, allocator: std.mem.Allocator, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!bool {
        const gop = try self.deps.getOrPut(allocator, def_idx);
        if (gop.found_existing) return false;
        gop.value_ptr.* = {};
        return true;
    }

    fn addCalledPattern(self: *DemandSummary, allocator: std.mem.Allocator, pattern_idx: CIR.Pattern.Idx) std.mem.Allocator.Error!bool {
        const gop = try self.called_patterns.getOrPut(allocator, pattern_idx);
        if (gop.found_existing) return false;
        gop.value_ptr.* = {};
        return true;
    }

    fn mergeFrom(self: *DemandSummary, allocator: std.mem.Allocator, other: *const DemandSummary) std.mem.Allocator.Error!bool {
        var changed = false;

        var dep_iter = other.deps.keyIterator();
        while (dep_iter.next()) |def_idx| {
            if (try self.addDep(allocator, def_idx.*)) changed = true;
        }

        var called_iter = other.called_patterns.keyIterator();
        while (called_iter.next()) |pattern_idx| {
            if (try self.addCalledPattern(allocator, pattern_idx.*)) changed = true;
        }

        return changed;
    }
};

const LocalCallables = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, CIR.Expr.Idx);

const DemandAnalyzer = struct {
    cir: *const ModuleEnv,
    allocator: std.mem.Allocator,
    summary_defs: []const CIR.Def.Idx,
    graph_def_set: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{},
    pattern_to_def: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, CIR.Def.Idx) = .{},
    summaries: std.AutoHashMapUnmanaged(CIR.Expr.Idx, DemandSummary) = .{},
    active_lambdas: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},

    fn init(
        cir: *const ModuleEnv,
        summary_defs: []const CIR.Def.Idx,
        graph_defs: []const CIR.Def.Idx,
        allocator: std.mem.Allocator,
    ) std.mem.Allocator.Error!DemandAnalyzer {
        var analyzer = DemandAnalyzer{
            .cir = cir,
            .allocator = allocator,
            .summary_defs = summary_defs,
        };
        errdefer analyzer.deinit();

        for (graph_defs) |def_idx| {
            try analyzer.graph_def_set.put(allocator, def_idx, {});
        }

        for (summary_defs) |def_idx| {
            const def = cir.store.getDef(def_idx);
            try analyzer.pattern_to_def.put(allocator, def.pattern, def_idx);
        }

        return analyzer;
    }

    fn deinit(self: *DemandAnalyzer) void {
        var summary_iter = self.summaries.valueIterator();
        while (summary_iter.next()) |summary| {
            summary.deinit(self.allocator);
        }
        self.summaries.deinit(self.allocator);
        self.pattern_to_def.deinit(self.allocator);
        self.graph_def_set.deinit(self.allocator);
        self.active_lambdas.deinit(self.allocator);
    }

    fn computeSummaries(self: *DemandAnalyzer) std.mem.Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.summary_defs) |def_idx| {
                const lambda_idx = self.lambdaFromDef(def_idx) orelse continue;

                var local_callables = LocalCallables{};
                defer local_callables.deinit(self.allocator);

                var computed = DemandSummary{};
                defer computed.deinit(self.allocator);

                try self.collectLambdaExecution(lambda_idx, &computed, &local_callables);

                const gop = try self.summaries.getOrPut(self.allocator, lambda_idx);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                if (try gop.value_ptr.mergeFrom(self.allocator, &computed)) {
                    changed = true;
                }
            }
        }
    }

    fn collectDefDependencies(self: *DemandAnalyzer, def_idx: CIR.Def.Idx, out: *DemandSummary) std.mem.Allocator.Error!void {
        var local_callables = LocalCallables{};
        defer local_callables.deinit(self.allocator);

        const def = self.cir.store.getDef(def_idx);
        try self.collectExprConstruction(def.expr, out, &local_callables);
    }

    fn addGraphDep(self: *DemandAnalyzer, out: *DemandSummary, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
        if (self.graph_def_set.contains(def_idx)) {
            _ = try out.addDep(self.allocator, def_idx);
        }
    }

    fn lambdaFromDef(self: *const DemandAnalyzer, def_idx: CIR.Def.Idx) ?CIR.Expr.Idx {
        const def = self.cir.store.getDef(def_idx);
        return self.lambdaFromExprWithLocals(def.expr, null);
    }

    fn lambdaFromExprWithLocals(
        self: *const DemandAnalyzer,
        expr_idx: CIR.Expr.Idx,
        local_callables: ?*const LocalCallables,
    ) ?CIR.Expr.Idx {
        return switch (self.cir.store.getExpr(expr_idx)) {
            .e_lambda => expr_idx,
            .e_closure => |closure| closure.lambda_idx,
            .e_lookup_local => |lookup| blk: {
                if (local_callables) |locals| {
                    if (locals.get(lookup.pattern_idx)) |lambda_idx| break :blk lambda_idx;
                }
                const def_idx = self.pattern_to_def.get(lookup.pattern_idx) orelse break :blk null;
                break :blk self.lambdaFromDef(def_idx);
            },
            else => null,
        };
    }

    fn rememberLocalCallable(
        self: *DemandAnalyzer,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        if (self.lambdaFromExprWithLocals(expr_idx, local_callables)) |lambda_idx| {
            try local_callables.put(self.allocator, pattern_idx, lambda_idx);
        }
    }

    fn collectLambdaExecution(
        self: *DemandAnalyzer,
        lambda_idx: CIR.Expr.Idx,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        if (self.active_lambdas.contains(lambda_idx)) return;
        try self.active_lambdas.put(self.allocator, lambda_idx, {});
        defer _ = self.active_lambdas.remove(lambda_idx);

        const expr = self.cir.store.getExpr(lambda_idx);
        if (expr != .e_lambda) return;
        try self.collectExprConstruction(expr.e_lambda.body, out, local_callables);
    }

    fn collectExprSpan(
        self: *DemandAnalyzer,
        span: CIR.Expr.Span,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        for (self.cir.store.sliceExpr(span)) |expr_idx| {
            try self.collectExprConstruction(expr_idx, out, local_callables);
        }
    }

    fn collectCall(
        self: *DemandAnalyzer,
        call_func: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        try self.collectExprConstruction(call_func, out, local_callables);
        try self.collectExprSpan(call_args, out, local_callables);
        try self.applyCallTarget(call_func, call_args, out, local_callables);
    }

    fn applyCallTarget(
        self: *DemandAnalyzer,
        call_func: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        switch (self.cir.store.getExpr(call_func)) {
            .e_lookup_local => |lookup| {
                if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                    try self.applyLambdaSummary(lambda_idx, call_args, out, local_callables);
                    return;
                }
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                        try self.applyLambdaSummary(lambda_idx, call_args, out, local_callables);
                    }
                    return;
                }
                _ = try out.addCalledPattern(self.allocator, lookup.pattern_idx);
            },
            .e_lambda => try self.applyLambdaSummary(call_func, call_args, out, local_callables),
            .e_closure => |closure| try self.applyLambdaSummary(closure.lambda_idx, call_args, out, local_callables),
            else => {},
        }
    }

    fn applyLambdaSummary(
        self: *DemandAnalyzer,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        var computed = DemandSummary{};
        defer computed.deinit(self.allocator);

        if (self.summaries.getPtr(lambda_idx)) |summary| {
            _ = try computed.mergeFrom(self.allocator, summary);
        } else {
            try self.collectLambdaExecution(lambda_idx, &computed, local_callables);
        }

        var dep_iter = computed.deps.keyIterator();
        while (dep_iter.next()) |def_idx| {
            try self.addGraphDep(out, def_idx.*);
        }

        var called_iter = computed.called_patterns.keyIterator();
        while (called_iter.next()) |pattern_idx| {
            if (self.callArgForPattern(lambda_idx, call_args, pattern_idx.*)) |arg_expr| {
                try self.collectCalledValue(arg_expr, out, local_callables);
            } else {
                _ = try out.addCalledPattern(self.allocator, pattern_idx.*);
            }
        }
    }

    fn collectCalledValue(
        self: *DemandAnalyzer,
        expr_idx: CIR.Expr.Idx,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        try self.collectExprConstruction(expr_idx, out, local_callables);
        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                    const empty_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                    try self.applyLambdaSummary(lambda_idx, empty_args, out, local_callables);
                    return;
                }
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                        const empty_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                        try self.applyLambdaSummary(lambda_idx, empty_args, out, local_callables);
                    }
                    return;
                }
                _ = try out.addCalledPattern(self.allocator, lookup.pattern_idx);
            },
            .e_lambda => {
                const empty_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                try self.applyLambdaSummary(expr_idx, empty_args, out, local_callables);
            },
            .e_closure => |closure| {
                const empty_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                try self.applyLambdaSummary(closure.lambda_idx, empty_args, out, local_callables);
            },
            else => {},
        }
    }

    fn callArgForPattern(
        self: *DemandAnalyzer,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CIR.Expr.Idx {
        const lambda_expr = self.cir.store.getExpr(lambda_idx);
        if (lambda_expr != .e_lambda) return null;

        const args = self.cir.store.slicePatterns(lambda_expr.e_lambda.args);
        const call_arg_exprs = self.cir.store.sliceExpr(call_args);
        const arg_count = @min(args.len, call_arg_exprs.len);
        for (args[0..arg_count], call_arg_exprs[0..arg_count]) |arg_pattern, arg_expr| {
            if (self.patternBinds(arg_pattern, pattern_idx)) return arg_expr;
        }
        return null;
    }

    fn patternBinds(self: *DemandAnalyzer, root: CIR.Pattern.Idx, needle: CIR.Pattern.Idx) bool {
        if (root == needle) return true;

        var stack_allocator_state = std.heap.stackFallback(2048, self.allocator);
        const stack_allocator = stack_allocator_state.get();
        var pending: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer pending.deinit(stack_allocator);

        pending.append(stack_allocator, root) catch return false;
        while (pending.pop()) |current| {
            if (current == needle) return true;
            switch (self.cir.store.getPattern(current)) {
                .as => |as_pattern| pending.append(stack_allocator, as_pattern.pattern) catch return false,
                .applied_tag => |tag| {
                    for (self.cir.store.slicePatterns(tag.args)) |arg| {
                        pending.append(stack_allocator, arg) catch return false;
                    }
                },
                .nominal => |nominal| pending.append(stack_allocator, nominal.backing_pattern) catch return false,
                .nominal_external => |nominal| pending.append(stack_allocator, nominal.backing_pattern) catch return false,
                .record_destructure => |record| {
                    for (self.cir.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                        const destruct = self.cir.store.getRecordDestruct(destruct_idx);
                        pending.append(stack_allocator, destruct.kind.toPatternIdx()) catch return false;
                    }
                },
                .list => |list| {
                    for (self.cir.store.slicePatterns(list.patterns)) |item| {
                        pending.append(stack_allocator, item) catch return false;
                    }
                    if (list.rest_info) |rest| {
                        if (rest.pattern) |rest_pattern| {
                            pending.append(stack_allocator, rest_pattern) catch return false;
                        }
                    }
                },
                .tuple => |tuple| {
                    for (self.cir.store.slicePatterns(tuple.patterns)) |item| {
                        pending.append(stack_allocator, item) catch return false;
                    }
                },
                .str_interpolation => |str| {
                    for (0..str.steps.span.len) |offset| {
                        const step = self.cir.store.getStrPatternStep(str.steps, @intCast(offset));
                        if (step.capture) |capture| {
                            pending.append(stack_allocator, capture) catch return false;
                        }
                    }
                },
                .assign,
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

        return false;
    }

    fn collectExprConstruction(
        self: *DemandAnalyzer,
        expr_idx: CIR.Expr.Idx,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try self.addGraphDep(out, def_idx);
                }
            },
            .e_call => |call| try self.collectCall(call.func, call.args, out, local_callables),
            .e_lambda => {},
            .e_closure => |closure| {
                for (self.cir.store.sliceCaptures(closure.captures)) |capture_idx| {
                    const capture = self.cir.store.getCapture(capture_idx);
                    if (self.pattern_to_def.get(capture.pattern_idx)) |def_idx| {
                        try self.addGraphDep(out, def_idx);
                    }
                }
            },
            .e_if => |if_expr| {
                try self.collectExprConstruction(if_expr.final_else, out, local_callables);
                for (self.cir.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = self.cir.store.getIfBranch(branch_idx);
                    try self.collectExprConstruction(branch.body, out, local_callables);
                    try self.collectExprConstruction(branch.cond, out, local_callables);
                }
            },
            .e_match => |match_expr| {
                for (self.cir.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = self.cir.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard_idx| {
                        try self.collectExprConstruction(guard_idx, out, local_callables);
                    }
                    try self.collectExprConstruction(branch.value, out, local_callables);
                }
                try self.collectExprConstruction(match_expr.cond, out, local_callables);
            },
            .e_list => |list| try self.collectExprSpan(list.elems, out, local_callables),
            .e_record => |record| {
                if (record.ext) |ext_idx| {
                    try self.collectExprConstruction(ext_idx, out, local_callables);
                }
                for (self.cir.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = self.cir.store.getRecordField(field_idx);
                    try self.collectExprConstruction(field.value, out, local_callables);
                }
            },
            .e_field_access => |access| try self.collectExprConstruction(access.receiver, out, local_callables),
            .e_method_call => |call| {
                try self.collectExprConstruction(call.receiver, out, local_callables);
                try self.collectExprSpan(call.args, out, local_callables);
            },
            .e_dispatch_call => |call| {
                try self.collectExprConstruction(call.receiver, out, local_callables);
                try self.collectExprSpan(call.args, out, local_callables);
            },
            .e_interpolation => |interpolation| {
                try self.collectExprSpan(interpolation.parts, out, local_callables);
                try self.collectExprConstruction(interpolation.first, out, local_callables);
            },
            .e_structural_eq => |eq| {
                try self.collectExprConstruction(eq.rhs, out, local_callables);
                try self.collectExprConstruction(eq.lhs, out, local_callables);
            },
            .e_structural_hash => |h| {
                try self.collectExprConstruction(h.hasher, out, local_callables);
                try self.collectExprConstruction(h.value, out, local_callables);
            },
            .e_method_eq => |eq| {
                try self.collectExprConstruction(eq.rhs, out, local_callables);
                try self.collectExprConstruction(eq.lhs, out, local_callables);
            },
            .e_type_method_call => |call| try self.collectExprSpan(call.args, out, local_callables),
            .e_type_dispatch_call => |call| try self.collectExprSpan(call.args, out, local_callables),
            .e_tuple_access => |tuple_access| try self.collectExprConstruction(tuple_access.tuple, out, local_callables),
            .e_tuple => |tuple| try self.collectExprSpan(tuple.elems, out, local_callables),
            .e_binop => |binop| {
                try self.collectExprConstruction(binop.rhs, out, local_callables);
                try self.collectExprConstruction(binop.lhs, out, local_callables);
            },
            .e_unary_minus => |unop| try self.collectExprConstruction(unop.expr, out, local_callables),
            .e_unary_not => |unop| try self.collectExprConstruction(unop.expr, out, local_callables),
            .e_block => |block| {
                for (self.cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                    const stmt = self.cir.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |decl| {
                            try self.collectExprConstruction(decl.expr, out, local_callables);
                            try self.rememberLocalCallable(decl.pattern, decl.expr, local_callables);
                        },
                        .s_var => |var_stmt| {
                            try self.collectExprConstruction(var_stmt.expr, out, local_callables);
                            try self.rememberLocalCallable(var_stmt.pattern_idx, var_stmt.expr, local_callables);
                        },
                        .s_var_uninitialized => {},
                        .s_reassign => |reassign| try self.collectExprConstruction(reassign.expr, out, local_callables),
                        .s_dbg => |dbg| try self.collectExprConstruction(dbg.expr, out, local_callables),
                        .s_expr => |expr_stmt| try self.collectExprConstruction(expr_stmt.expr, out, local_callables),
                        .s_expect => |expect| try self.collectExprConstruction(expect.body, out, local_callables),
                        .s_for => |for_stmt| try self.collectExprConstruction(for_stmt.expr, out, local_callables),
                        .s_while => |while_stmt| {
                            try self.collectExprConstruction(while_stmt.body, out, local_callables);
                            try self.collectExprConstruction(while_stmt.cond, out, local_callables);
                        },
                        .s_infinite_loop => |loop_stmt| {
                            try self.collectExprConstruction(loop_stmt.body, out, local_callables);
                            try self.collectExprConstruction(loop_stmt.cond, out, local_callables);
                        },
                        .s_breakable_loop => |loop_stmt| {
                            try self.collectExprConstruction(loop_stmt.body, out, local_callables);
                            try self.collectExprConstruction(loop_stmt.cond, out, local_callables);
                        },
                        .s_return => |ret| try self.collectExprConstruction(ret.expr, out, local_callables),
                        .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_runtime_error, .s_break => {},
                    }
                }
                try self.collectExprConstruction(block.final_expr, out, local_callables);
            },
            .e_tag => |tag| try self.collectExprSpan(tag.args, out, local_callables),
            .e_nominal => |nominal| try self.collectExprConstruction(nominal.backing_expr, out, local_callables),
            .e_run_low_level => |run_ll| try self.collectExprSpan(run_ll.args, out, local_callables),
            .e_nominal_external => |nominal| try self.collectExprConstruction(nominal.backing_expr, out, local_callables),
            .e_dbg => |dbg| try self.collectExprConstruction(dbg.expr, out, local_callables),
            .e_expect_err => |expect_err| try self.collectExprConstruction(expect_err.expr, out, local_callables),
            .e_expect => |expect| try self.collectExprConstruction(expect.body, out, local_callables),
            .e_return => |ret| try self.collectExprConstruction(ret.expr, out, local_callables),
            .e_break => {},
            .e_for => |for_expr| {
                try self.collectExprConstruction(for_expr.body, out, local_callables);
                try self.collectExprConstruction(for_expr.expr, out, local_callables);
            },
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str,
            .e_str_segment,
            .e_bytes_literal,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_ellipsis,
            .e_anno_only,
            .e_hosted_lambda,
            .e_lookup_external,
            .e_lookup_required,
            .e_crash,
            .e_runtime_error,
            => {},
        }
    }
};

/// Build a dependency graph for all definitions
pub fn buildDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!DependencyGraph {
    const defs_slice = cir.store.sliceDefs(all_defs);
    var graph = DependencyGraph.init(allocator, defs_slice);
    errdefer graph.deinit();

    var analyzer = try DemandAnalyzer.init(cir, defs_slice, defs_slice, allocator);
    defer analyzer.deinit();

    try analyzer.computeSummaries();

    for (defs_slice) |def_idx| {
        var deps = DemandSummary{};
        defer deps.deinit(allocator);

        try analyzer.collectDefDependencies(def_idx, &deps);

        var dep_iter = deps.deps.keyIterator();
        while (dep_iter.next()) |dep_def_idx| {
            try graph.addEdge(def_idx, dep_def_idx.*);
        }
    }

    return graph;
}

/// Tarjan's algorithm for finding strongly connected components
pub fn computeSCCs(
    graph: *const DependencyGraph,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!EvaluationOrder {
    var state = TarjanState.init(allocator);
    defer state.deinit();

    // Run DFS from each unvisited node
    for (graph.nodes) |node| {
        if (!state.visited.contains(node)) {
            try state.strongConnect(graph, node);
        }
    }

    // Note: state.sccs is already in topological order (dependencies before dependents)
    // because Tarjan's algorithm adds SCCs in post-order of DFS traversal.
    // When we follow edges from A to B (A depends on B), B finishes first,
    // so B's SCC is added before A's SCC.

    return EvaluationOrder{
        .sccs = try state.sccs.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

/// Returns indices of all top-level constants (definitions that are not functions).
///
/// This is used to identify definitions that should be evaluated at compile time,
/// as opposed to functions which are only evaluated when called.
pub fn getTopLevelConstants(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error![]const CIR.Def.Idx {
    const defs_slice = cir.store.sliceDefs(all_defs);

    var constants: std.ArrayList(CIR.Def.Idx) = .empty;
    errdefer constants.deinit(allocator);

    for (defs_slice) |def_idx| {
        const def = cir.store.getDef(def_idx);
        const expr = cir.store.getExpr(def.expr);

        const is_constant = switch (expr) {
            .e_lambda, .e_closure, .e_anno_only, .e_hosted_lambda => false,
            else => true,
        };

        if (is_constant) {
            try constants.append(allocator, def_idx);
        }
    }

    return constants.toOwnedSlice(allocator);
}

/// Returns constants in dependency order (dependencies first).
///
/// This computes the strongly connected components (SCCs) for only the constant
/// definitions, returning them in topological order so that each constant can
/// be evaluated after all its dependencies have been evaluated.
pub fn getConstantsInDependencyOrder(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!EvaluationOrder {
    // Get only the constant definitions
    const constants = try getTopLevelConstants(cir, all_defs, allocator);
    defer allocator.free(constants);

    if (constants.len == 0) {
        return EvaluationOrder{
            .sccs = &[_]SCC{},
            .allocator = allocator,
        };
    }

    // Build a dependency graph for just the constants
    var graph = DependencyGraph.init(allocator, constants);
    errdefer graph.deinit();

    const defs_slice = cir.store.sliceDefs(all_defs);
    var analyzer = try DemandAnalyzer.init(cir, defs_slice, constants, allocator);
    defer analyzer.deinit();

    try analyzer.computeSummaries();

    for (constants) |def_idx| {
        var deps = DemandSummary{};
        defer deps.deinit(allocator);

        try analyzer.collectDefDependencies(def_idx, &deps);

        var dep_iter = deps.deps.keyIterator();
        while (dep_iter.next()) |dep_def_idx| {
            try graph.addEdge(def_idx, dep_def_idx.*);
        }
    }

    // Compute SCCs using Tarjan's algorithm
    const result = try computeSCCs(&graph, allocator);
    graph.deinit();
    return result;
}

const TarjanState = struct {
    /// Current DFS index
    index: u32,

    /// Map from node to its DFS index
    indices: std.AutoHashMapUnmanaged(CIR.Def.Idx, u32),

    /// Map from node to its lowlink value
    lowlinks: std.AutoHashMapUnmanaged(CIR.Def.Idx, u32),

    /// Set of visited nodes
    visited: std.AutoHashMapUnmanaged(CIR.Def.Idx, void),

    /// Stack for Tarjan's algorithm
    stack: std.ArrayList(CIR.Def.Idx),

    /// Set of nodes currently on stack
    on_stack: std.AutoHashMapUnmanaged(CIR.Def.Idx, void),

    /// Resulting SCCs (in reverse topological order during construction)
    sccs: std.ArrayList(SCC),

    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TarjanState {
        return .{
            .index = 0,
            .indices = .{},
            .lowlinks = .{},
            .visited = .{},
            .stack = .empty,
            .on_stack = .{},
            .sccs = .empty,
            .allocator = allocator,
        };
    }

    fn deinit(self: *TarjanState) void {
        self.indices.deinit(self.allocator);
        self.lowlinks.deinit(self.allocator);
        self.visited.deinit(self.allocator);
        self.stack.deinit(self.allocator);
        self.on_stack.deinit(self.allocator);
        // Note: sccs ownership transferred to EvaluationOrder, don't free here
        self.sccs.deinit(self.allocator);
    }

    fn beginNode(self: *TarjanState, v: CIR.Def.Idx) std.mem.Allocator.Error!void {
        try self.indices.put(self.allocator, v, self.index);
        try self.lowlinks.put(self.allocator, v, self.index);
        try self.visited.put(self.allocator, v, {});
        self.index += 1;

        try self.stack.append(self.allocator, v);
        try self.on_stack.put(self.allocator, v, {});
    }

    fn finishNode(
        self: *TarjanState,
        graph: *const DependencyGraph,
        v: CIR.Def.Idx,
    ) std.mem.Allocator.Error!void {
        const v_lowlink = self.lowlinks.get(v).?;
        const v_index = self.indices.get(v).?;
        if (v_lowlink != v_index) return;

        var scc_defs: std.ArrayList(CIR.Def.Idx) = .empty;

        while (true) {
            const w = self.stack.pop() orelse unreachable; // Stack should not be empty
            std.debug.assert(self.on_stack.remove(w));
            try scc_defs.append(self.allocator, w);

            if (@intFromEnum(w) == @intFromEnum(v)) break;
        }

        // Check if this SCC is recursive
        const is_recursive = scc_defs.items.len > 1 or blk: {
            // Check for self-loop
            if (scc_defs.items.len == 1) {
                const node = scc_defs.items[0];
                const deps = graph.getDependencies(node);
                for (deps) |dep| {
                    if (@intFromEnum(dep) == @intFromEnum(node)) break :blk true;
                }
            }
            break :blk false;
        };

        try self.sccs.append(self.allocator, .{
            .defs = try scc_defs.toOwnedSlice(self.allocator),
            .is_recursive = is_recursive,
        });
    }

    fn strongConnect(
        self: *TarjanState,
        graph: *const DependencyGraph,
        v: CIR.Def.Idx,
    ) std.mem.Allocator.Error!void {
        const DfsFrame = struct {
            node: CIR.Def.Idx,
            next_dependency: usize,
        };

        var stack_allocator_state = std.heap.stackFallback(4096, self.allocator);
        const stack_allocator = stack_allocator_state.get();
        var dfs_stack: std.ArrayList(DfsFrame) = .empty;
        defer dfs_stack.deinit(stack_allocator);

        try self.beginNode(v);
        try dfs_stack.append(stack_allocator, .{
            .node = v,
            .next_dependency = 0,
        });

        while (dfs_stack.items.len > 0) {
            const top = &dfs_stack.items[dfs_stack.items.len - 1];
            const dependencies = graph.getDependencies(top.node);

            if (top.next_dependency < dependencies.len) {
                const w = dependencies[top.next_dependency];
                top.next_dependency += 1;

                if (!self.visited.contains(w)) {
                    try self.beginNode(w);
                    try dfs_stack.append(stack_allocator, .{
                        .node = w,
                        .next_dependency = 0,
                    });
                } else if (self.on_stack.contains(w)) {
                    const v_lowlink = self.lowlinks.get(top.node).?;
                    const w_index = self.indices.get(w).?;
                    try self.lowlinks.put(self.allocator, top.node, @min(v_lowlink, w_index));
                }
                continue;
            }

            const finished = top.node;
            try self.finishNode(graph, finished);
            _ = dfs_stack.pop() orelse unreachable;

            if (dfs_stack.items.len > 0) {
                const parent = dfs_stack.items[dfs_stack.items.len - 1].node;
                const parent_lowlink = self.lowlinks.get(parent).?;
                const finished_lowlink = self.lowlinks.get(finished).?;
                try self.lowlinks.put(self.allocator, parent, @min(parent_lowlink, finished_lowlink));
            }
        }
    }
};
