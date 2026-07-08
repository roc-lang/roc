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

                // Walk the lambda's execution with the lambda marked active,
                // exactly as an inline execution frame would.
                const lambda_expr = self.cir.store.getExpr(lambda_idx);
                if (lambda_expr == .e_lambda and !self.active_lambdas.contains(lambda_idx)) {
                    try self.active_lambdas.put(self.allocator, lambda_idx, {});
                    defer _ = self.active_lambdas.remove(lambda_idx);
                    try self.walkDemand(lambda_expr.e_lambda.body, &computed, &local_callables);
                }

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
        try self.walkDemand(def.expr, out, &local_callables);
    }

    fn addGraphDep(self: *DemandAnalyzer, out: *DemandSummary, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
        if (self.graph_def_set.contains(def_idx)) {
            _ = try out.addDep(self.allocator, def_idx);
        }
    }

    /// Resolve the lambda a def or expression evaluates to, following local
    /// callable bindings and def-to-def alias chains (`f = g`) iteratively.
    /// A cyclic alias chain (`a = b` / `b = a`) terminates as "no lambda".
    fn lambdaFromDef(self: *const DemandAnalyzer, def_idx: CIR.Def.Idx) ?CIR.Expr.Idx {
        const def = self.cir.store.getDef(def_idx);
        return self.lambdaFromExprWithLocals(def.expr, null);
    }

    fn lambdaFromExprWithLocals(
        self: *const DemandAnalyzer,
        expr_idx: CIR.Expr.Idx,
        local_callables: ?*const LocalCallables,
    ) ?CIR.Expr.Idx {
        var current = expr_idx;
        // A def-alias chain is at most one hop per distinct def; anything
        // longer than the number of defs is a cycle.
        var hops_remaining: usize = self.summary_defs.len + 1;
        while (hops_remaining > 0) : (hops_remaining -= 1) {
            switch (self.cir.store.getExpr(current)) {
                .e_lambda => return current,
                .e_closure => |closure| return closure.lambda_idx,
                .e_lookup_local => |lookup| {
                    if (local_callables) |locals| {
                        if (locals.get(lookup.pattern_idx)) |lambda_idx| return lambda_idx;
                    }
                    const def_idx = self.pattern_to_def.get(lookup.pattern_idx) orelse return null;
                    current = self.cir.store.getDef(def_idx).expr;
                },
                else => return null,
            }
        }
        return null;
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

    /// One unit of work for the demand walk. The walk is an explicit worklist
    /// plus a stack of lambda-execution frames (zero-recursion policy): every
    /// item executes against the innermost open frame's summary, or the
    /// walk's root output when no frame is open.
    const WalkItem = union(enum) {
        /// Pre-order construction visit of one expression node.
        visit: CIR.Expr.Idx,
        /// Process one block statement; statements are pushed in reverse so
        /// they execute in source order (local callables register in order).
        visit_stmt: CIR.Statement.Idx,
        /// Register a local binding as callable AFTER its RHS subtree has
        /// been walked, matching the recursive walk's ordering (a binding is
        /// never callable inside its own RHS walk).
        remember_local: struct { pattern: CIR.Pattern.Idx, expr: CIR.Expr.Idx },
        /// Resolve a call's target after its operands were visited: apply
        /// the called lambda's demand summary, or record an unresolved
        /// called pattern.
        apply_call_target: struct { func: CIR.Expr.Idx, args: CIR.Expr.Span },
        /// The summary-application half of a called-pattern argument (the
        /// argument's construction visit is pushed separately).
        apply_called_value: CIR.Expr.Idx,
        /// Close an inline lambda-execution frame: fold its computed summary
        /// into the parent (graph deps directly; called patterns matched
        /// against the call's arguments) and deactivate the lambda.
        finish_lambda: struct { lambda: CIR.Expr.Idx, args: CIR.Expr.Span },
    };

    const Walk = struct {
        work: std.ArrayList(WalkItem) = .empty,
        frames: std.ArrayList(DemandSummary) = .empty,

        fn deinit(walk: *Walk, allocator: std.mem.Allocator) void {
            walk.work.deinit(allocator);
            for (walk.frames.items) |*frame| frame.deinit(allocator);
            walk.frames.deinit(allocator);
        }

        fn push(walk: *Walk, allocator: std.mem.Allocator, item: WalkItem) std.mem.Allocator.Error!void {
            try walk.work.append(allocator, item);
        }
    };

    /// Run the demand walk from `root_expr`, accumulating into `out`.
    fn walkDemand(
        self: *DemandAnalyzer,
        root_expr: CIR.Expr.Idx,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        var walk = Walk{};
        defer walk.deinit(self.allocator);
        try walk.push(self.allocator, .{ .visit = root_expr });

        while (walk.work.pop()) |item| {
            const current: *DemandSummary = if (walk.frames.items.len > 0)
                &walk.frames.items[walk.frames.items.len - 1]
            else
                out;
            switch (item) {
                .visit => |expr_idx| try self.visitExpr(&walk, current, expr_idx),
                .visit_stmt => |stmt_idx| try self.visitStmt(&walk, stmt_idx),
                .remember_local => |bind| try self.rememberLocalCallable(bind.pattern, bind.expr, local_callables),
                .apply_call_target => |call| try self.applyCallTarget(&walk, current, call.func, call.args, local_callables),
                .apply_called_value => |expr_idx| try self.applyCalledValue(&walk, current, expr_idx, local_callables),
                .finish_lambda => |fin| {
                    var computed = walk.frames.pop().?;
                    defer computed.deinit(self.allocator);
                    _ = self.active_lambdas.remove(fin.lambda);
                    const parent: *DemandSummary = if (walk.frames.items.len > 0)
                        &walk.frames.items[walk.frames.items.len - 1]
                    else
                        out;
                    try self.foldLambdaSummary(&walk, parent, &computed, fin.lambda, fin.args);
                },
            }
        }
    }

    /// Apply a called lambda's demand summary at a call site. A cached
    /// summary folds immediately; an uncached lambda's body is walked inline
    /// in its own frame (guarded by `active_lambdas` against recursion), and
    /// folded when the frame closes.
    fn beginApplyLambdaSummary(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
    ) std.mem.Allocator.Error!void {
        if (self.summaries.getPtr(lambda_idx)) |summary| {
            try self.foldLambdaSummary(walk, current, summary, lambda_idx, call_args);
            return;
        }
        if (self.active_lambdas.contains(lambda_idx)) return;
        const expr = self.cir.store.getExpr(lambda_idx);
        if (expr != .e_lambda) return;

        try self.active_lambdas.put(self.allocator, lambda_idx, {});
        try walk.frames.append(self.allocator, DemandSummary{});
        // The finish item pops first-in-last-out: the body's whole walk runs
        // inside the frame, then the frame folds into its parent.
        try walk.push(self.allocator, .{ .finish_lambda = .{ .lambda = lambda_idx, .args = call_args } });
        try walk.push(self.allocator, .{ .visit = expr.e_lambda.body });
    }

    /// Fold one lambda's demand summary into `into` for a call with
    /// `call_args`: graph deps transfer directly; a called pattern that
    /// matches a passed argument walks that argument as a called value,
    /// otherwise it propagates unresolved.
    fn foldLambdaSummary(
        self: *DemandAnalyzer,
        walk: *Walk,
        into: *DemandSummary,
        summary: *const DemandSummary,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
    ) std.mem.Allocator.Error!void {
        var dep_iter = summary.deps.keyIterator();
        while (dep_iter.next()) |def_idx| {
            try self.addGraphDep(into, def_idx.*);
        }

        var called_iter = summary.called_patterns.keyIterator();
        while (called_iter.next()) |pattern_idx| {
            if (self.callArgForPattern(lambda_idx, call_args, pattern_idx.*)) |arg_expr| {
                try walk.push(self.allocator, .{ .apply_called_value = arg_expr });
                try walk.push(self.allocator, .{ .visit = arg_expr });
            } else {
                _ = try into.addCalledPattern(self.allocator, pattern_idx.*);
            }
        }
    }

    fn applyCallTarget(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        call_func: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        switch (self.cir.store.getExpr(call_func)) {
            .e_lookup_local => |lookup| {
                if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, call_args);
                    return;
                }
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                        try self.beginApplyLambdaSummary(walk, current, lambda_idx, call_args);
                    }
                    return;
                }
                _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
            },
            .e_lambda => try self.beginApplyLambdaSummary(walk, current, call_func, call_args),
            .e_closure => |closure| try self.beginApplyLambdaSummary(walk, current, closure.lambda_idx, call_args),
            else => {},
        }
    }

    fn applyCalledValue(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        expr_idx: CIR.Expr.Idx,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        const empty_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, empty_args);
                    return;
                }
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                        try self.beginApplyLambdaSummary(walk, current, lambda_idx, empty_args);
                    }
                    return;
                }
                _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
            },
            .e_lambda => try self.beginApplyLambdaSummary(walk, current, expr_idx, empty_args),
            .e_closure => |closure| try self.beginApplyLambdaSummary(walk, current, closure.lambda_idx, empty_args),
            else => {},
        }
    }

    /// Push every child of a span in reverse, so pops visit in source order.
    fn pushExprSpanReversed(self: *DemandAnalyzer, walk: *Walk, span: CIR.Expr.Span) std.mem.Allocator.Error!void {
        const exprs = self.cir.store.sliceExpr(span);
        var i = exprs.len;
        while (i > 0) {
            i -= 1;
            try walk.push(self.allocator, .{ .visit = exprs[i] });
        }
    }

    fn visitStmt(self: *DemandAnalyzer, walk: *Walk, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
        switch (self.cir.store.getStatement(stmt_idx)) {
            .s_decl => |decl| {
                // Register the callable only after its RHS subtree walked.
                try walk.push(self.allocator, .{ .remember_local = .{ .pattern = decl.pattern, .expr = decl.expr } });
                try walk.push(self.allocator, .{ .visit = decl.expr });
            },
            .s_var => |var_stmt| {
                try walk.push(self.allocator, .{ .remember_local = .{ .pattern = var_stmt.pattern_idx, .expr = var_stmt.expr } });
                try walk.push(self.allocator, .{ .visit = var_stmt.expr });
            },
            .s_var_uninitialized => {},
            .s_reassign => |reassign| try walk.push(self.allocator, .{ .visit = reassign.expr }),
            .s_dbg => |dbg| try walk.push(self.allocator, .{ .visit = dbg.expr }),
            .s_expr => |expr_stmt| try walk.push(self.allocator, .{ .visit = expr_stmt.expr }),
            .s_expect => |expect| try walk.push(self.allocator, .{ .visit = expect.body }),
            .s_for => |for_stmt| {
                try walk.push(self.allocator, .{ .visit = for_stmt.body });
                try walk.push(self.allocator, .{ .visit = for_stmt.expr });
            },
            .s_while => |while_stmt| {
                try walk.push(self.allocator, .{ .visit = while_stmt.body });
                try walk.push(self.allocator, .{ .visit = while_stmt.cond });
            },
            .s_infinite_loop => |loop_stmt| {
                try walk.push(self.allocator, .{ .visit = loop_stmt.body });
                try walk.push(self.allocator, .{ .visit = loop_stmt.cond });
            },
            .s_breakable_loop => |loop_stmt| {
                try walk.push(self.allocator, .{ .visit = loop_stmt.body });
                try walk.push(self.allocator, .{ .visit = loop_stmt.cond });
            },
            .s_return => |ret| try walk.push(self.allocator, .{ .visit = ret.expr }),
            .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_runtime_error, .s_break => {},
        }
    }

    fn visitExpr(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try self.addGraphDep(current, def_idx);
                }
            },
            .e_call => |call| {
                // Operands first, then the target's summary applies —
                // pushed in reverse so pops run func, args, apply.
                try walk.push(self.allocator, .{ .apply_call_target = .{ .func = call.func, .args = call.args } });
                try self.pushExprSpanReversed(walk, call.args);
                try walk.push(self.allocator, .{ .visit = call.func });
            },
            .e_lambda => {},
            .e_closure => |closure| {
                for (self.cir.store.sliceCaptures(closure.captures)) |capture_idx| {
                    const capture = self.cir.store.getCapture(capture_idx);
                    if (self.pattern_to_def.get(capture.pattern_idx)) |def_idx| {
                        try self.addGraphDep(current, def_idx);
                    }
                }
            },
            .e_if => |if_expr| {
                const branches = self.cir.store.sliceIfBranches(if_expr.branches);
                var i = branches.len;
                while (i > 0) {
                    i -= 1;
                    const branch = self.cir.store.getIfBranch(branches[i]);
                    try walk.push(self.allocator, .{ .visit = branch.cond });
                    try walk.push(self.allocator, .{ .visit = branch.body });
                }
                try walk.push(self.allocator, .{ .visit = if_expr.final_else });
            },
            .e_match => |match_expr| {
                try walk.push(self.allocator, .{ .visit = match_expr.cond });
                const branches = self.cir.store.sliceMatchBranches(match_expr.branches);
                var i = branches.len;
                while (i > 0) {
                    i -= 1;
                    const branch = self.cir.store.getMatchBranch(branches[i]);
                    try walk.push(self.allocator, .{ .visit = branch.value });
                    if (branch.guard) |guard_idx| {
                        try walk.push(self.allocator, .{ .visit = guard_idx });
                    }
                }
            },
            .e_list => |list| try self.pushExprSpanReversed(walk, list.elems),
            .e_record => |record| {
                const fields = self.cir.store.sliceRecordFields(record.fields);
                var i = fields.len;
                while (i > 0) {
                    i -= 1;
                    const field = self.cir.store.getRecordField(fields[i]);
                    try walk.push(self.allocator, .{ .visit = field.value });
                }
                if (record.ext) |ext_idx| try walk.push(self.allocator, .{ .visit = ext_idx });
            },
            .e_field_access => |access| try walk.push(self.allocator, .{ .visit = access.receiver }),
            .e_method_call => |call| {
                try self.pushExprSpanReversed(walk, call.args);
                try walk.push(self.allocator, .{ .visit = call.receiver });
            },
            .e_dispatch_call => |call| {
                try self.pushExprSpanReversed(walk, call.args);
                try walk.push(self.allocator, .{ .visit = call.receiver });
            },
            .e_interpolation => |interpolation| {
                try walk.push(self.allocator, .{ .visit = interpolation.first });
                try self.pushExprSpanReversed(walk, interpolation.parts);
            },
            .e_structural_eq => |eq| {
                try walk.push(self.allocator, .{ .visit = eq.lhs });
                try walk.push(self.allocator, .{ .visit = eq.rhs });
            },
            .e_structural_hash => |h| {
                try walk.push(self.allocator, .{ .visit = h.value });
                try walk.push(self.allocator, .{ .visit = h.hasher });
            },
            .e_method_eq => |eq| {
                try walk.push(self.allocator, .{ .visit = eq.lhs });
                try walk.push(self.allocator, .{ .visit = eq.rhs });
            },
            .e_type_method_call => |call| try self.pushExprSpanReversed(walk, call.args),
            .e_type_dispatch_call => |call| try self.pushExprSpanReversed(walk, call.args),
            .e_tuple_access => |tuple_access| try walk.push(self.allocator, .{ .visit = tuple_access.tuple }),
            .e_tuple => |tuple| try self.pushExprSpanReversed(walk, tuple.elems),
            .e_binop => |binop| {
                try walk.push(self.allocator, .{ .visit = binop.lhs });
                try walk.push(self.allocator, .{ .visit = binop.rhs });
            },
            .e_unary_minus => |unop| try walk.push(self.allocator, .{ .visit = unop.expr }),
            .e_unary_not => |unop| try walk.push(self.allocator, .{ .visit = unop.expr }),
            .e_block => |block| {
                try walk.push(self.allocator, .{ .visit = block.final_expr });
                const stmts = self.cir.store.sliceStatements(block.stmts);
                var i = stmts.len;
                while (i > 0) {
                    i -= 1;
                    try walk.push(self.allocator, .{ .visit_stmt = stmts[i] });
                }
            },
            .e_tag => |tag| try self.pushExprSpanReversed(walk, tag.args),
            .e_nominal => |nominal| try walk.push(self.allocator, .{ .visit = nominal.backing_expr }),
            .e_run_low_level => |run_ll| try self.pushExprSpanReversed(walk, run_ll.args),
            .e_nominal_external => |nominal| try walk.push(self.allocator, .{ .visit = nominal.backing_expr }),
            .e_dbg => |dbg| try walk.push(self.allocator, .{ .visit = dbg.expr }),
            .e_expect_err => |expect_err| try walk.push(self.allocator, .{ .visit = expect_err.expr }),
            .e_expect => |expect| try walk.push(self.allocator, .{ .visit = expect.body }),
            .e_return => |ret| try walk.push(self.allocator, .{ .visit = ret.expr }),
            .e_break => {},
            .e_for => |for_expr| {
                try walk.push(self.allocator, .{ .visit = for_expr.body });
                try walk.push(self.allocator, .{ .visit = for_expr.expr });
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

/// Collect every top-level def referenced anywhere in `root_expr`'s expression
/// tree — including nested lambda bodies and blocks — into `out` (deduplicated).
///
/// This is the *name-reference* relation used to order type checking, not the
/// demand relation used to order compile-time constant evaluation: a reference
/// under an uncalled lambda still creates an edge here, because checking the
/// def's body needs the referenced def's type regardless of whether evaluation
/// would ever demand its value.
///
/// Type-qualified method calls (`U.method(..)`) resolve their owner statement
/// during canonicalization, so their target defs are statically known and
/// contribute edges too. Value-receiver dispatch (`u.method()`) is inherently
/// type-directed and contributes no edge; the checker discovers those
/// dependencies during inference and resolves them at group boundaries.
///
/// The walk is an explicit worklist (zero-recursion policy).
fn collectNameReferences(
    cir: *const ModuleEnv,
    pattern_to_def: *const std.AutoHashMapUnmanaged(CIR.Pattern.Idx, CIR.Def.Idx),
    root_expr: CIR.Expr.Idx,
    out: *std.AutoHashMapUnmanaged(CIR.Def.Idx, void),
    scratch_stack: *std.ArrayList(CIR.Expr.Idx),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    scratch_stack.clearRetainingCapacity();
    try scratch_stack.append(allocator, root_expr);

    while (scratch_stack.pop()) |expr_idx| {
        switch (cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try out.put(allocator, def_idx, {});
                }
            },
            .e_type_method_call => |call| {
                if (cir.lookupMethodBindingForOwnerConst(call.type_dispatch_stmt, call.method_name)) |binding| {
                    try out.put(allocator, binding.def_idx, {});
                }
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_type_dispatch_call => |call| {
                if (cir.lookupMethodBindingForOwnerConst(call.type_dispatch_stmt, call.method_name)) |binding| {
                    try out.put(allocator, binding.def_idx, {});
                }
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_lambda => |lambda| try scratch_stack.append(allocator, lambda.body),
            .e_closure => |closure| {
                try scratch_stack.append(allocator, closure.lambda_idx);
                for (cir.store.sliceCaptures(closure.captures)) |capture_idx| {
                    const capture = cir.store.getCapture(capture_idx);
                    if (pattern_to_def.get(capture.pattern_idx)) |def_idx| {
                        try out.put(allocator, def_idx, {});
                    }
                }
            },
            .e_call => |call| {
                try scratch_stack.append(allocator, call.func);
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_if => |if_expr| {
                try scratch_stack.append(allocator, if_expr.final_else);
                for (cir.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = cir.store.getIfBranch(branch_idx);
                    try scratch_stack.append(allocator, branch.cond);
                    try scratch_stack.append(allocator, branch.body);
                }
            },
            .e_match => |match_expr| {
                try scratch_stack.append(allocator, match_expr.cond);
                for (cir.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = cir.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard_idx| try scratch_stack.append(allocator, guard_idx);
                    try scratch_stack.append(allocator, branch.value);
                }
            },
            .e_list => |list| {
                for (cir.store.sliceExpr(list.elems)) |elem| try scratch_stack.append(allocator, elem);
            },
            .e_record => |record| {
                if (record.ext) |ext_idx| try scratch_stack.append(allocator, ext_idx);
                for (cir.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = cir.store.getRecordField(field_idx);
                    try scratch_stack.append(allocator, field.value);
                }
            },
            .e_field_access => |access| try scratch_stack.append(allocator, access.receiver),
            .e_method_call => |call| {
                try scratch_stack.append(allocator, call.receiver);
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_dispatch_call => |call| {
                try scratch_stack.append(allocator, call.receiver);
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_interpolation => |interpolation| {
                try scratch_stack.append(allocator, interpolation.first);
                for (cir.store.sliceExpr(interpolation.parts)) |part| try scratch_stack.append(allocator, part);
            },
            .e_structural_eq => |eq| {
                try scratch_stack.append(allocator, eq.lhs);
                try scratch_stack.append(allocator, eq.rhs);
            },
            .e_structural_hash => |h| {
                try scratch_stack.append(allocator, h.hasher);
                try scratch_stack.append(allocator, h.value);
            },
            .e_method_eq => |eq| {
                try scratch_stack.append(allocator, eq.lhs);
                try scratch_stack.append(allocator, eq.rhs);
            },
            .e_tuple_access => |tuple_access| try scratch_stack.append(allocator, tuple_access.tuple),
            .e_tuple => |tuple| {
                for (cir.store.sliceExpr(tuple.elems)) |elem| try scratch_stack.append(allocator, elem);
            },
            .e_binop => |binop| {
                try scratch_stack.append(allocator, binop.lhs);
                try scratch_stack.append(allocator, binop.rhs);
            },
            .e_unary_minus => |unop| try scratch_stack.append(allocator, unop.expr),
            .e_unary_not => |unop| try scratch_stack.append(allocator, unop.expr),
            .e_block => |block| {
                for (cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                    switch (cir.store.getStatement(stmt_idx)) {
                        .s_decl => |decl| try scratch_stack.append(allocator, decl.expr),
                        .s_var => |var_stmt| try scratch_stack.append(allocator, var_stmt.expr),
                        .s_var_uninitialized => {},
                        .s_reassign => |reassign| try scratch_stack.append(allocator, reassign.expr),
                        .s_dbg => |dbg| try scratch_stack.append(allocator, dbg.expr),
                        .s_expr => |expr_stmt| try scratch_stack.append(allocator, expr_stmt.expr),
                        .s_expect => |expect| try scratch_stack.append(allocator, expect.body),
                        .s_for => |for_stmt| {
                            try scratch_stack.append(allocator, for_stmt.expr);
                            try scratch_stack.append(allocator, for_stmt.body);
                        },
                        .s_while => |while_stmt| {
                            try scratch_stack.append(allocator, while_stmt.cond);
                            try scratch_stack.append(allocator, while_stmt.body);
                        },
                        .s_infinite_loop => |loop_stmt| {
                            try scratch_stack.append(allocator, loop_stmt.cond);
                            try scratch_stack.append(allocator, loop_stmt.body);
                        },
                        .s_breakable_loop => |loop_stmt| {
                            try scratch_stack.append(allocator, loop_stmt.cond);
                            try scratch_stack.append(allocator, loop_stmt.body);
                        },
                        .s_return => |ret| try scratch_stack.append(allocator, ret.expr),
                        .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_runtime_error, .s_break => {},
                    }
                }
                try scratch_stack.append(allocator, block.final_expr);
            },
            .e_tag => |tag| {
                for (cir.store.sliceExpr(tag.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_nominal => |nominal| try scratch_stack.append(allocator, nominal.backing_expr),
            .e_nominal_external => |nominal| try scratch_stack.append(allocator, nominal.backing_expr),
            .e_run_low_level => |run_ll| {
                for (cir.store.sliceExpr(run_ll.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_dbg => |dbg| try scratch_stack.append(allocator, dbg.expr),
            .e_expect_err => |expect_err| try scratch_stack.append(allocator, expect_err.expr),
            .e_expect => |expect| try scratch_stack.append(allocator, expect.body),
            // NOTE: `ret.lambda` is a back-reference to the enclosing lambda,
            // not a child; walking it would loop.
            .e_return => |ret| try scratch_stack.append(allocator, ret.expr),
            .e_for => |for_expr| {
                try scratch_stack.append(allocator, for_expr.expr);
                try scratch_stack.append(allocator, for_expr.body);
            },
            .e_break,
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
}

/// Compute the order in which type checking processes top-level defs: the SCC
/// condensation of the name-reference graph, in deterministic topological
/// order.
///
/// Determinism rule: among groups whose dependencies are all emitted, the
/// group whose first member appears earliest in source order goes next;
/// members within a group are in source order. This keeps diagnostic order as
/// close to source order as the dependency relation allows.
///
/// The result is a transient checking artifact: Check consumes it for the
/// duration of one module's checking and frees it; it is never part of the
/// checked module (see design.md, "Checked module boundaries").
pub fn computeCheckOrder(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!EvaluationOrder {
    const defs_slice = cir.store.sliceDefs(all_defs);

    var pattern_to_def: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, CIR.Def.Idx) = .{};
    defer pattern_to_def.deinit(allocator);
    // Source position of each def, for the deterministic tie-break.
    var def_position: std.AutoHashMapUnmanaged(CIR.Def.Idx, u32) = .{};
    defer def_position.deinit(allocator);
    for (defs_slice, 0..) |def_idx, position| {
        const def = cir.store.getDef(def_idx);
        try pattern_to_def.put(allocator, def.pattern, def_idx);
        try def_position.put(allocator, def_idx, @intCast(position));
    }

    var graph = DependencyGraph.init(allocator, defs_slice);
    defer graph.deinit();

    var refs: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{};
    defer refs.deinit(allocator);
    var scratch_stack: std.ArrayList(CIR.Expr.Idx) = .empty;
    defer scratch_stack.deinit(allocator);

    for (defs_slice) |def_idx| {
        refs.clearRetainingCapacity();
        const def = cir.store.getDef(def_idx);
        try collectNameReferences(cir, &pattern_to_def, def.expr, &refs, &scratch_stack, allocator);
        var ref_iter = refs.keyIterator();
        while (ref_iter.next()) |ref_def_idx| {
            try graph.addEdge(def_idx, ref_def_idx.*);
        }
    }

    var tarjan_order = try computeSCCs(&graph, allocator);
    defer tarjan_order.deinit();

    // Sort members within each group by source position.
    const MemberSort = struct {
        fn lessThan(ctx: *const std.AutoHashMapUnmanaged(CIR.Def.Idx, u32), a: CIR.Def.Idx, b: CIR.Def.Idx) bool {
            return ctx.get(a).? < ctx.get(b).?;
        }
    };
    for (tarjan_order.sccs) |scc| {
        std.mem.sort(CIR.Def.Idx, scc.defs, &def_position, MemberSort.lessThan);
    }

    // Deterministic topological order over the condensation: Kahn's algorithm,
    // always emitting the ready group with the earliest first member.
    const group_count = tarjan_order.sccs.len;
    var def_to_group: std.AutoHashMapUnmanaged(CIR.Def.Idx, u32) = .{};
    defer def_to_group.deinit(allocator);
    for (tarjan_order.sccs, 0..) |scc, group_index| {
        for (scc.defs) |def_idx| {
            try def_to_group.put(allocator, def_idx, @intCast(group_index));
        }
    }

    var successors = try allocator.alloc(std.ArrayList(u32), group_count);
    defer {
        for (successors) |*list| list.deinit(allocator);
        allocator.free(successors);
    }
    for (successors) |*list| list.* = .empty;
    const indegree = try allocator.alloc(u32, group_count);
    defer allocator.free(indegree);
    @memset(indegree, 0);

    // Dedup cross-group edges so indegrees count each predecessor group once.
    var seen_edges: std.AutoHashMapUnmanaged(u64, void) = .{};
    defer seen_edges.deinit(allocator);
    for (tarjan_order.sccs, 0..) |scc, group_index| {
        for (scc.defs) |def_idx| {
            for (graph.getDependencies(def_idx)) |dep_def_idx| {
                const dep_group = def_to_group.get(dep_def_idx).?;
                if (dep_group == group_index) continue;
                // Group depends on dep_group: dep_group must be emitted first.
                const edge_key = (@as(u64, dep_group) << 32) | @as(u64, @intCast(group_index));
                const gop = try seen_edges.getOrPut(allocator, edge_key);
                if (gop.found_existing) continue;
                try successors[dep_group].append(allocator, @intCast(group_index));
                indegree[group_index] += 1;
            }
        }
    }

    // Priority = source position of the group's first (earliest) member.
    const group_priority = try allocator.alloc(u32, group_count);
    defer allocator.free(group_priority);
    for (tarjan_order.sccs, 0..) |scc, group_index| {
        group_priority[group_index] = def_position.get(scc.defs[0]).?;
    }
    const ReadyContext = struct {
        priorities: []const u32,
        fn compare(ctx: @This(), a: u32, b: u32) std.math.Order {
            return std.math.order(ctx.priorities[a], ctx.priorities[b]);
        }
    };
    var ready = std.PriorityQueue(u32, ReadyContext, ReadyContext.compare).initContext(.{ .priorities = group_priority });
    defer ready.deinit(allocator);
    for (0..group_count) |group_index| {
        if (indegree[group_index] == 0) try ready.push(allocator, @intCast(group_index));
    }

    var ordered_sccs: std.ArrayList(SCC) = .empty;
    errdefer {
        for (ordered_sccs.items) |scc| allocator.free(scc.defs);
        ordered_sccs.deinit(allocator);
    }
    try ordered_sccs.ensureTotalCapacityPrecise(allocator, group_count);

    while (ready.pop()) |group_index| {
        const scc = tarjan_order.sccs[group_index];
        ordered_sccs.appendAssumeCapacity(.{
            .defs = try allocator.dupe(CIR.Def.Idx, scc.defs),
            .is_recursive = scc.is_recursive,
        });
        for (successors[group_index].items) |successor| {
            indegree[successor] -= 1;
            if (indegree[successor] == 0) try ready.push(allocator, successor);
        }
    }
    // Tarjan produced an acyclic condensation, so Kahn must emit every group.
    std.debug.assert(ordered_sccs.items.len == group_count);

    return EvaluationOrder{
        .sccs = try ordered_sccs.toOwnedSlice(allocator),
        .allocator = allocator,
    };
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
