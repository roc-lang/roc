//! End-of-module default-cycle pass (design.md "Defaulted Fields").
//!
//! A `??` default is materialized at every construction site that omits its
//! field, so a default whose own materialization reaches itself again has no
//! value to start from. Post the nominal-only restriction, both edge kinds of
//! that dependency are name-resolvable at canonicalization:
//!
//! - REFERENCE edges: the default (or a def it transitively references)
//!   mentions a same-module top-level def, whose body is walked in turn.
//! - OMISSION edges: a LOCAL nominal construction (`Cfg.{ ... }`) omits a
//!   declared defaulted field, so materializing the surrounding expression
//!   materializes that field's default.
//!
//! Value references only point down the import DAG, so a path that returns
//! to its origin default can never leave the declaring module: this pass is
//! module-local without losing any name-resolvable cycle. Edges that only
//! exist after type checking—method/dispatch calls, calls through function
//! parameters, and FOREIGN nominal constructions' omissions—terminate here
//! by principle and are the checker's residue
//! (`defaultMaterializationIsRecursive` in src/check/Check.zig).
//!
//! Detection is one Tarjan SCC run over a graph whose nodes are the module's
//! defaults and top-level defs. Each cyclic SCC containing at least one
//! default is reported ONCE, on its first default in source order (per-origin
//! walks would report a mutual cycle once per participant), and EVERY default
//! in the SCC is dropped—each is unmaterializable—so check and lowering
//! never see a cyclic default. The walk is conservative by reachability: an
//! omission edge on a dead branch still counts, exactly like the checker's
//! aggregate walk before it. The one deliberate exception is function
//! VALUES: materializing a default that is (or references) a lambda only
//! creates the closure—the body is not evaluated—so a lambda reached as a
//! value does not walk its body (its captures still do), while a lambda in
//! direct callee position of a walked call (immediately invoked) does.

const std = @import("std");
const base = @import("base");

const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");
const default_omissions = @import("default_omissions.zig");
const Diagnostic = CIR.Diagnostic;

const Expr = CIR.Expr;
const Allocator = std.mem.Allocator;

/// One declared default: where it lives (for dropping and reporting) and its
/// expression (the walk root).
const DefaultInfo = struct {
    decl_stmt: CIR.Statement.Idx,
    field_idx: CIR.TypeAnno.RecordField.Idx,
    field_name: base.Ident.Idx,
    default_expr: Expr.Idx,
};

/// Run the pass: build the default/def dependency graph, find cyclic SCCs
/// containing a default, report each once, and drop every participating
/// default (the field degrades to plain required, the established recovery).
pub fn checkDefaultCycles(env: *ModuleEnv, gpa: Allocator) Allocator.Error!void {
    var pass = Pass{
        .env = env,
        .gpa = gpa,
    };
    defer pass.deinit();

    try pass.collectDefaults();
    if (pass.defaults.items.len == 0) return;
    try pass.collectDefs();
    try pass.buildEdges();
    try pass.findAndReportCycles();
}

const Pass = struct {
    env: *ModuleEnv,
    gpa: Allocator,

    defaults: std.ArrayListUnmanaged(DefaultInfo) = .empty,
    /// Defaulted-field anno node -> graph node id of that default. Keyed by
    /// the field's anno node so the shared omission enumeration
    /// (`default_omissions.zig`) maps directly to graph nodes.
    default_node_by_field: std.AutoHashMapUnmanaged(CIR.TypeAnno.RecordField.Idx, u32) = .{},
    /// Top-level binder pattern -> graph node id of the def.
    pattern_to_node: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, u32) = .{},
    /// Per-def walk roots, indexed by node id - defaults.len.
    def_exprs: std.ArrayListUnmanaged(Expr.Idx) = .empty,
    /// CSR adjacency: edges of node i are edges[edge_starts[i]..edge_starts[i+1]].
    edges: std.ArrayListUnmanaged(u32) = .empty,
    edge_starts: std.ArrayListUnmanaged(u32) = .empty,
    /// Expression worklist for one node's walk.
    walk: std.ArrayListUnmanaged(Expr.Idx) = .empty,
    /// Dedup of edge targets within one node's walk (also guards re-walking).
    seen_targets: std.AutoHashMapUnmanaged(u32, void) = .{},
    /// Dedup of visited expressions within one node's walk.
    seen_exprs: std.AutoHashMapUnmanaged(Expr.Idx, void) = .{},

    fn deinit(self: *Pass) void {
        self.defaults.deinit(self.gpa);
        self.default_node_by_field.deinit(self.gpa);
        self.pattern_to_node.deinit(self.gpa);
        self.def_exprs.deinit(self.gpa);
        self.edges.deinit(self.gpa);
        self.edge_starts.deinit(self.gpa);
        self.walk.deinit(self.gpa);
        self.seen_targets.deinit(self.gpa);
        self.seen_exprs.deinit(self.gpa);
    }

    fn nodeCount(self: *const Pass) u32 {
        return @intCast(self.defaults.items.len + self.def_exprs.items.len);
    }

    /// Every defaulted field of every nominal declaration's backing record
    /// (resolved via `default_omissions.backingRecordFields`, which handles
    /// parenthesized backings—`Foo := ({ ... })`). `forward_type_decls`
    /// re-lists forward-prepared statements also present in `type_decls`;
    /// `default_node_by_field` keys by the field's anno node, so a duplicate
    /// listing is skipped rather than double-registered.
    fn collectDefaults(self: *Pass) Allocator.Error!void {
        const spans = [_]CIR.Statement.Span{ self.env.type_decls, self.env.forward_type_decls };
        for (spans) |span| {
            for (self.env.store.sliceStatements(span)) |stmt_idx| {
                const declared = default_omissions.backingRecordFields(self.env, stmt_idx) orelse continue;
                for (self.env.store.sliceAnnoRecordFields(declared)) |field_idx| {
                    const field = self.env.store.getAnnoRecordField(field_idx);
                    const default_expr = field.default_value orelse continue;
                    const entry = try self.default_node_by_field.getOrPut(self.gpa, field_idx);
                    if (entry.found_existing) continue;
                    entry.value_ptr.* = @intCast(self.defaults.items.len);
                    try self.defaults.append(self.gpa, .{
                        .decl_stmt = stmt_idx,
                        .field_idx = field_idx,
                        .field_name = field.name,
                        .default_expr = default_expr,
                    });
                }
            }
        }
    }

    /// Top-level defs become graph nodes after the defaults; a lookup of a
    /// def's binder pattern is a reference edge to that def's body.
    fn collectDefs(self: *Pass) Allocator.Error!void {
        const default_count: u32 = @intCast(self.defaults.items.len);
        for (self.env.store.sliceDefs(self.env.all_defs)) |def_idx| {
            const def = self.env.store.getDef(def_idx);
            const node_id: u32 = default_count + @as(u32, @intCast(self.def_exprs.items.len));
            try self.def_exprs.append(self.gpa, def.expr);
            try self.pattern_to_node.put(self.gpa, def.pattern, node_id);
        }
    }

    fn buildEdges(self: *Pass) Allocator.Error!void {
        const count = self.nodeCount();
        try self.edge_starts.ensureTotalCapacity(self.gpa, count + 1);
        var node: u32 = 0;
        while (node < count) : (node += 1) {
            self.edge_starts.appendAssumeCapacity(@intCast(self.edges.items.len));
            const root = if (node < self.defaults.items.len)
                self.defaults.items[node].default_expr
            else
                self.def_exprs.items[node - self.defaults.items.len];
            try self.walkNode(root);
        }
        self.edge_starts.appendAssumeCapacity(@intCast(self.edges.items.len));
    }

    fn addEdge(self: *Pass, target: u32) Allocator.Error!void {
        const entry = try self.seen_targets.getOrPut(self.gpa, target);
        if (entry.found_existing) return;
        try self.edges.append(self.gpa, target);
    }

    /// Structurally walk one node's expression, recording reference and
    /// omission edges. Explicit worklist (zero-recursion policy); every
    /// expression form descends into all child expressions—edges that need
    /// type information (dispatch, parameter calls, foreign lookups,
    /// foreign constructions) terminate here by principle.
    fn walkNode(self: *Pass, root: Expr.Idx) Allocator.Error!void {
        self.seen_targets.clearRetainingCapacity();
        self.seen_exprs.clearRetainingCapacity();
        self.walk.clearRetainingCapacity();
        try self.walk.append(self.gpa, root);
        while (self.walk.pop()) |expr_idx| {
            const seen = try self.seen_exprs.getOrPut(self.gpa, expr_idx);
            if (seen.found_existing) continue;
            switch (self.env.store.getExpr(expr_idx)) {
                .e_lookup_local => |lookup| {
                    if (self.pattern_to_node.get(lookup.pattern_idx)) |node| {
                        try self.addEdge(node);
                    }
                },
                .e_lookup_associated_resolved => |lookup| {
                    // Resolved associated items name their target def
                    // directly; a same-module target is an ordinary
                    // reference edge.
                    if (lookup.module_identity == self.env.selfModuleIdentity()) {
                        const def = self.env.store.getDef(lookup.target_def_idx);
                        if (self.pattern_to_node.get(def.pattern)) |node| {
                            try self.addEdge(node);
                        }
                    }
                },
                .e_nominal => |nominal| {
                    try self.appendOmissionEdges(nominal.nominal_type_decl, nominal.backing_expr);
                    try self.walk.append(self.gpa, nominal.backing_expr);
                },
                .e_nominal_external => |nominal| {
                    // A foreign construction's omitted defaults are not
                    // visible at canonicalization; the checker's residue
                    // walk owns that edge. The supplied values still walk.
                    try self.walk.append(self.gpa, nominal.backing_expr);
                },
                // A closure reached as a VALUE materializes only its
                // captured values—creating the function value does not run
                // its body; the body runs on a later application, which is
                // beyond name resolution (calls through values/parameters
                // are the checker's residue). Captures ARE evaluated at
                // materialization, so their reference edges stay.
                .e_closure => |closure| {
                    for (self.env.store.sliceCaptures(closure.captures)) |capture_idx| {
                        const capture = self.env.store.getCapture(capture_idx);
                        if (self.pattern_to_node.get(capture.pattern_idx)) |node| {
                            try self.addEdge(node);
                        }
                    }
                },
                // A lambda reached as a VALUE only creates the function
                // value; its body is not evaluated at materialization, so
                // the body is not walked. The one place a body IS evaluated
                // at materialization—an immediately-invoked lambda—is
                // handled in `.e_call` below.
                .e_lambda => {},
                .e_call => |call| {
                    // A lambda/closure in DIRECT callee position is
                    // immediately invoked: its body IS evaluated at
                    // materialization, so walk it (a closure callee's
                    // captures walk via the `.e_closure` arm). Every other
                    // callee walks as a value.
                    switch (self.env.store.getExpr(call.func)) {
                        .e_lambda => |lambda| try self.walk.append(self.gpa, lambda.body),
                        .e_closure => |closure| {
                            try self.walk.append(self.gpa, call.func);
                            // `lambda_idx` always names an `e_lambda` (see
                            // CIR.Expr.Closure).
                            const lambda = self.env.store.getExpr(closure.lambda_idx);
                            try self.walk.append(self.gpa, lambda.e_lambda.body);
                        },
                        else => try self.walk.append(self.gpa, call.func),
                    }
                    try self.appendSpan(call.args);
                },
                .e_if => |if_expr| {
                    for (self.env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                        const branch = self.env.store.getIfBranch(branch_idx);
                        try self.walk.append(self.gpa, branch.cond);
                        try self.walk.append(self.gpa, branch.body);
                    }
                    try self.walk.append(self.gpa, if_expr.final_else);
                },
                .e_match => |match_expr| {
                    try self.walk.append(self.gpa, match_expr.cond);
                    for (self.env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                        const branch = self.env.store.getMatchBranch(branch_idx);
                        try self.walk.append(self.gpa, branch.value);
                        if (branch.guard) |guard_idx| try self.walk.append(self.gpa, guard_idx);
                    }
                },
                .e_list => |list| try self.appendSpan(list.elems),
                .e_tuple => |tuple| try self.appendSpan(tuple.elems),
                .e_record => |record| {
                    for (self.env.store.sliceRecordFields(record.fields)) |field_idx| {
                        const field = self.env.store.getRecordField(field_idx);
                        try self.walk.append(self.gpa, field.value);
                    }
                    if (record.ext) |ext_idx| try self.walk.append(self.gpa, ext_idx);
                },
                .e_field_access => |access| try self.walk.append(self.gpa, access.receiver),
                .e_tuple_access => |access| try self.walk.append(self.gpa, access.tuple),
                .e_method_call => |call| {
                    try self.walk.append(self.gpa, call.receiver);
                    try self.appendSpan(call.args);
                },
                .e_dispatch_call => |call| {
                    try self.walk.append(self.gpa, call.receiver);
                    try self.appendSpan(call.args);
                },
                .e_type_method_call => |call| try self.appendSpan(call.args),
                .e_type_dispatch_call => |call| try self.appendSpan(call.args),
                .e_interpolation => |interpolation| {
                    try self.walk.append(self.gpa, interpolation.first);
                    try self.appendSpan(interpolation.parts);
                },
                .e_structural_eq => |eq| {
                    try self.walk.append(self.gpa, eq.lhs);
                    try self.walk.append(self.gpa, eq.rhs);
                },
                .e_structural_hash => |h| {
                    try self.walk.append(self.gpa, h.value);
                    try self.walk.append(self.gpa, h.hasher);
                },
                .e_method_eq => |eq| {
                    try self.walk.append(self.gpa, eq.lhs);
                    try self.walk.append(self.gpa, eq.rhs);
                },
                .e_binop => |binop| {
                    try self.walk.append(self.gpa, binop.lhs);
                    try self.walk.append(self.gpa, binop.rhs);
                },
                .e_unary_minus => |unop| try self.walk.append(self.gpa, unop.expr),
                .e_unary_not => |unop| try self.walk.append(self.gpa, unop.expr),
                .e_block => |block| {
                    for (self.env.store.sliceStatements(block.stmts)) |stmt_idx| {
                        try self.appendStmtExprs(stmt_idx);
                    }
                    try self.walk.append(self.gpa, block.final_expr);
                },
                .e_tag => |tag| try self.appendSpan(tag.args),
                .e_str => |str| try self.appendSpan(str.span),
                .e_run_low_level => |run_ll| try self.appendSpan(run_ll.args),
                .e_dbg => |dbg| try self.walk.append(self.gpa, dbg.expr),
                .e_expect_err => |expect_err| try self.walk.append(self.gpa, expect_err.expr),
                .e_expect => |expect| try self.walk.append(self.gpa, expect.body),
                .e_return => |ret| try self.walk.append(self.gpa, ret.expr),
                .e_for => |for_expr| {
                    try self.walk.append(self.gpa, for_expr.expr);
                    try self.walk.append(self.gpa, for_expr.body);
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
                .e_str_segment,
                .e_bytes_literal,
                .e_empty_list,
                .e_empty_record,
                .e_zero_argument_tag,
                .e_ellipsis,
                .e_anno_only,
                .e_derived_method,
                .e_hosted_lambda,
                .e_lookup_external,
                .e_lookup_associated_local,
                .e_lookup_associated,
                .e_lookup_required,
                .e_crash,
                .e_runtime_error,
                => {},
            }
        }
    }

    fn appendSpan(self: *Pass, span: Expr.Span) Allocator.Error!void {
        for (self.env.store.sliceExpr(span)) |child| {
            try self.walk.append(self.gpa, child);
        }
    }

    fn appendStmtExprs(self: *Pass, stmt_idx: CIR.Statement.Idx) Allocator.Error!void {
        switch (self.env.store.getStatement(stmt_idx)) {
            .s_decl => |decl| try self.walk.append(self.gpa, decl.expr),
            .s_var => |var_stmt| try self.walk.append(self.gpa, var_stmt.expr),
            .s_reassign => |reassign| try self.walk.append(self.gpa, reassign.expr),
            .s_dbg => |dbg| try self.walk.append(self.gpa, dbg.expr),
            .s_expr => |expr_stmt| try self.walk.append(self.gpa, expr_stmt.expr),
            .s_expect => |expect| try self.walk.append(self.gpa, expect.body),
            .s_for => |for_stmt| {
                try self.walk.append(self.gpa, for_stmt.expr);
                try self.walk.append(self.gpa, for_stmt.body);
            },
            .s_while => |while_stmt| {
                try self.walk.append(self.gpa, while_stmt.cond);
                try self.walk.append(self.gpa, while_stmt.body);
            },
            .s_infinite_loop => |loop_stmt| try self.walk.append(self.gpa, loop_stmt.body),
            .s_breakable_loop => |loop_stmt| try self.walk.append(self.gpa, loop_stmt.body),
            .s_return => |ret| try self.walk.append(self.gpa, ret.expr),
            .s_var_uninitialized,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_crash,
            .s_runtime_error,
            .s_break,
            => {},
        }
    }

    /// A local nominal construction over a record literal: every declared
    /// defaulted field the literal does not mention is an omission edge to
    /// that field's default. The enumeration (update/unset/backing-shape
    /// rules included) is shared with `DependencyGraph.zig` via
    /// `default_omissions.zig`, so the cycle graph and the demand graph
    /// agree on what "omitted" means.
    fn appendOmissionEdges(self: *Pass, decl_stmt: CIR.Statement.Idx, backing_expr: Expr.Idx) Allocator.Error!void {
        var omissions = default_omissions.omittedDefaults(self.env, decl_stmt, backing_expr);
        while (omissions.next()) |omitted| {
            // `collectDefaults` registered the defaults of `type_decls` and
            // `forward_type_decls`; a construction naming a declaration
            // outside those spans has no graph node (exactly the statements
            // the old per-decl map skipped).
            const default_node = self.default_node_by_field.get(omitted.field_idx) orelse continue;
            try self.addEdge(default_node);
        }
    }

    /// Iterative Tarjan SCC over the finished graph. Each cyclic SCC (size
    /// > 1, or a single node with a self-edge) containing at least one
    /// default reports once—on the SCC's first default in source order—and
    /// drops every default in the SCC.
    fn findAndReportCycles(self: *Pass) Allocator.Error!void {
        const count = self.nodeCount();
        const unvisited = std.math.maxInt(u32);

        var state = TarjanState{
            .index_of = try self.gpa.alloc(u32, count),
            .lowlink_of = try self.gpa.alloc(u32, count),
            .on_stack = try self.gpa.alloc(bool, count),
        };
        defer state.deinit(self.gpa);
        @memset(state.index_of, unvisited);
        @memset(state.on_stack, false);

        var start: u32 = 0;
        while (start < count) : (start += 1) {
            if (state.index_of[start] != unvisited) continue;
            try state.frames.append(self.gpa, .{ .node = start, .edge_cursor = self.edge_starts.items[start] });
            while (state.frames.items.len > 0) {
                const frame = &state.frames.items[state.frames.items.len - 1];
                const node = frame.node;
                if (state.index_of[node] == unvisited) {
                    state.index_of[node] = state.next_index;
                    state.lowlink_of[node] = state.next_index;
                    state.next_index += 1;
                    try state.stack.append(self.gpa, node);
                    state.on_stack[node] = true;
                }
                if (frame.edge_cursor < self.edge_starts.items[node + 1]) {
                    const target = self.edges.items[frame.edge_cursor];
                    frame.edge_cursor += 1;
                    if (state.index_of[target] == unvisited) {
                        try state.frames.append(self.gpa, .{ .node = target, .edge_cursor = self.edge_starts.items[target] });
                    } else if (state.on_stack[target]) {
                        state.lowlink_of[node] = @min(state.lowlink_of[node], state.index_of[target]);
                    }
                    continue;
                }
                // Node finished: pop the frame, fold the lowlink into the
                // parent, and emit an SCC when this node is its root.
                _ = state.frames.pop();
                if (state.frames.items.len > 0) {
                    const parent = state.frames.items[state.frames.items.len - 1].node;
                    state.lowlink_of[parent] = @min(state.lowlink_of[parent], state.lowlink_of[node]);
                }
                if (state.lowlink_of[node] != state.index_of[node]) continue;
                const scc_start = std.mem.findScalarLast(u32, state.stack.items, node) orelse unreachable;
                const scc = state.stack.items[scc_start..];
                const cyclic = scc.len > 1 or self.hasSelfEdge(node);
                if (cyclic) try self.reportScc(scc);
                for (scc) |member| state.on_stack[member] = false;
                state.stack.shrinkRetainingCapacity(scc_start);
            }
        }
    }

    fn hasSelfEdge(self: *const Pass, node: u32) bool {
        const start = self.edge_starts.items[node];
        const end = self.edge_starts.items[node + 1];
        return std.mem.findScalar(u32, self.edges.items[start..end], node) != null;
    }

    /// Report a cyclic SCC once and drop every default in it. The report
    /// names the SCC's first default in source order (defaults were
    /// collected in declaration order, so the smallest node id is it).
    fn reportScc(self: *Pass, scc: []const u32) Allocator.Error!void {
        var representative: ?u32 = null;
        for (scc) |member| {
            if (member >= self.defaults.items.len) continue;
            if (representative == null or member < representative.?) representative = member;
        }
        const rep = representative orelse return; // def-only cycle: recursion, not our concern
        const rep_info = self.defaults.items[rep];
        try self.env.pushDiagnostic(Diagnostic{ .record_default_reference_cycle = .{
            .field_name = rep_info.field_name,
            .region = self.env.store.getExprRegion(rep_info.default_expr),
        } });
        for (scc) |member| {
            if (member >= self.defaults.items.len) continue;
            self.env.store.dropAnnoRecordFieldDefault(self.defaults.items[member].field_idx);
        }
    }

    const TarjanState = struct {
        index_of: []u32,
        lowlink_of: []u32,
        on_stack: []bool,
        next_index: u32 = 0,
        stack: std.ArrayListUnmanaged(u32) = .empty,
        frames: std.ArrayListUnmanaged(Frame) = .empty,

        const Frame = struct { node: u32, edge_cursor: u32 };

        fn deinit(state: *TarjanState, gpa: Allocator) void {
            gpa.free(state.index_of);
            gpa.free(state.lowlink_of);
            gpa.free(state.on_stack);
            state.stack.deinit(gpa);
            state.frames.deinit(gpa);
        }
    };
};
