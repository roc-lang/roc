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

/// Collects all definition dependencies from an expression
/// Returns a list of Ident.Idx that this expression references
fn collectExprDependencies(
    cir: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    dependencies: *std.AutoHashMapUnmanaged(base.Ident.Idx, void),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    var stack_allocator_state = std.heap.stackFallback(4096, allocator);
    const stack_allocator = stack_allocator_state.get();
    var pending: std.ArrayList(CIR.Expr.Idx) = .empty;
    defer pending.deinit(stack_allocator);

    try pending.append(stack_allocator, expr_idx);
    while (pending.pop()) |current_idx| {
        const expr = cir.store.getExpr(current_idx);
        switch (expr) {
            .e_lookup_local => |lookup| {
                const pattern = cir.store.getPattern(lookup.pattern_idx);
                if (pattern == .assign) {
                    try dependencies.put(allocator, pattern.assign.ident, {});
                }
            },
            .e_call => |call| {
                for (cir.store.sliceExpr(call.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
                try pending.append(stack_allocator, call.func);
            },
            .e_lambda => |lambda| {
                try pending.append(stack_allocator, lambda.body);
            },
            .e_closure => |closure| {
                try pending.append(stack_allocator, closure.lambda_idx);
            },
            .e_if => |if_expr| {
                try pending.append(stack_allocator, if_expr.final_else);
                for (cir.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = cir.store.getIfBranch(branch_idx);
                    try pending.append(stack_allocator, branch.body);
                    try pending.append(stack_allocator, branch.cond);
                }
            },
            .e_match => |match_expr| {
                for (cir.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = cir.store.getMatchBranch(branch_idx);
                    if (branch.guard) |guard_idx| {
                        try pending.append(stack_allocator, guard_idx);
                    }
                    try pending.append(stack_allocator, branch.value);
                }
                try pending.append(stack_allocator, match_expr.cond);
            },
            .e_list => |list| {
                for (cir.store.sliceExpr(list.elems)) |elem_idx| {
                    try pending.append(stack_allocator, elem_idx);
                }
            },
            .e_record => |record| {
                if (record.ext) |ext_idx| {
                    try pending.append(stack_allocator, ext_idx);
                }
                for (cir.store.sliceRecordFields(record.fields)) |field_idx| {
                    const field = cir.store.getRecordField(field_idx);
                    try pending.append(stack_allocator, field.value);
                }
            },
            .e_field_access => |access| {
                try pending.append(stack_allocator, access.receiver);
            },
            .e_method_call => |call| {
                for (cir.store.sliceExpr(call.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
                try pending.append(stack_allocator, call.receiver);
            },
            .e_dispatch_call => |call| {
                for (cir.store.sliceExpr(call.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
                try pending.append(stack_allocator, call.receiver);
            },
            .e_interpolation => |interpolation| {
                for (cir.store.sliceExpr(interpolation.parts)) |part_idx| {
                    try pending.append(stack_allocator, part_idx);
                }
                try pending.append(stack_allocator, interpolation.first);
            },
            .e_structural_eq => |eq| {
                try pending.append(stack_allocator, eq.rhs);
                try pending.append(stack_allocator, eq.lhs);
            },
            .e_method_eq => |eq| {
                try pending.append(stack_allocator, eq.rhs);
                try pending.append(stack_allocator, eq.lhs);
            },
            .e_type_method_call => |call| {
                for (cir.store.sliceExpr(call.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
            },
            .e_type_dispatch_call => |call| {
                for (cir.store.sliceExpr(call.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
            },
            .e_tuple_access => |tuple_access| {
                try pending.append(stack_allocator, tuple_access.tuple);
            },
            .e_tuple => |tuple| {
                for (cir.store.sliceExpr(tuple.elems)) |elem_idx| {
                    try pending.append(stack_allocator, elem_idx);
                }
            },
            .e_binop => |binop| {
                try pending.append(stack_allocator, binop.rhs);
                try pending.append(stack_allocator, binop.lhs);
            },
            .e_unary_minus => |unop| {
                try pending.append(stack_allocator, unop.expr);
            },
            .e_unary_not => |unop| {
                try pending.append(stack_allocator, unop.expr);
            },
            .e_block => |block| {
                try pending.append(stack_allocator, block.final_expr);
                for (cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                    const stmt = cir.store.getStatement(stmt_idx);
                    switch (stmt) {
                        .s_decl => |decl| try pending.append(stack_allocator, decl.expr),
                        .s_var => |var_stmt| try pending.append(stack_allocator, var_stmt.expr),
                        .s_reassign => |reassign| try pending.append(stack_allocator, reassign.expr),
                        .s_dbg => |dbg| try pending.append(stack_allocator, dbg.expr),
                        .s_expr => |expr_stmt| try pending.append(stack_allocator, expr_stmt.expr),
                        .s_expect => |expect| try pending.append(stack_allocator, expect.body),
                        .s_for => |for_stmt| try pending.append(stack_allocator, for_stmt.expr),
                        .s_while => |while_stmt| {
                            try pending.append(stack_allocator, while_stmt.body);
                            try pending.append(stack_allocator, while_stmt.cond);
                        },
                        .s_infinite_loop => |loop_stmt| {
                            try pending.append(stack_allocator, loop_stmt.body);
                            try pending.append(stack_allocator, loop_stmt.cond);
                        },
                        .s_breakable_loop => |loop_stmt| {
                            try pending.append(stack_allocator, loop_stmt.body);
                            try pending.append(stack_allocator, loop_stmt.cond);
                        },
                        .s_return => |ret| try pending.append(stack_allocator, ret.expr),
                        .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_runtime_error, .s_break => {},
                    }
                }
            },
            .e_tag => |tag| {
                for (cir.store.sliceExpr(tag.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
            },
            .e_nominal => |nominal| {
                try pending.append(stack_allocator, nominal.backing_expr);
            },
            .e_run_low_level => |run_ll| {
                for (cir.store.sliceExpr(run_ll.args)) |arg_idx| {
                    try pending.append(stack_allocator, arg_idx);
                }
            },
            .e_nominal_external => |nominal| {
                try pending.append(stack_allocator, nominal.backing_expr);
            },
            .e_dbg => |dbg| {
                try pending.append(stack_allocator, dbg.expr);
            },
            .e_expect_err => |expect_err| {
                try pending.append(stack_allocator, expect_err.expr);
            },
            .e_expect => |expect| {
                try pending.append(stack_allocator, expect.body);
            },
            .e_return => |ret| {
                try pending.append(stack_allocator, ret.expr);
            },
            .e_break => {},
            .e_for => |for_expr| {
                try pending.append(stack_allocator, for_expr.body);
                try pending.append(stack_allocator, for_expr.expr);
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
}

/// Build a dependency graph for all definitions
pub fn buildDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!DependencyGraph {
    const defs_slice = cir.store.sliceDefs(all_defs);
    var graph = DependencyGraph.init(allocator, defs_slice);
    errdefer graph.deinit();

    // Map from Ident.Idx to Def.Idx for resolving references
    var ident_to_def = std.AutoHashMapUnmanaged(base.Ident.Idx, CIR.Def.Idx){};
    defer ident_to_def.deinit(allocator);

    // First pass: build ident -> def mapping
    for (defs_slice) |def_idx| {
        const def = cir.store.getDef(def_idx);
        const pattern = cir.store.getPattern(def.pattern);

        if (pattern == .assign) {
            try ident_to_def.put(allocator, pattern.assign.ident, def_idx);
        }
    }

    // Second pass: collect dependencies and build graph
    for (defs_slice) |def_idx| {
        const def = cir.store.getDef(def_idx);

        // Collect all identifiers this def's expression references
        var deps = std.AutoHashMapUnmanaged(base.Ident.Idx, void){};
        defer deps.deinit(allocator);

        try collectExprDependencies(cir, def.expr, &deps, allocator);

        // Convert ident dependencies to def dependencies
        var dep_iter = deps.keyIterator();
        while (dep_iter.next()) |ident_idx| {
            if (ident_to_def.get(ident_idx.*)) |dep_def_idx| {
                try graph.addEdge(def_idx, dep_def_idx);
            }
            // If ident not found in ident_to_def, it's either:
            // - A builtin function
            // - An external module reference
            // - A parameter/local variable
            // In all cases, we don't need to track it for top-level evaluation order
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

    // Map from Ident.Idx to Def.Idx for resolving references (only for constants)
    var ident_to_def = std.AutoHashMapUnmanaged(base.Ident.Idx, CIR.Def.Idx){};
    defer ident_to_def.deinit(allocator);

    // First pass: build ident -> def mapping for constants only
    for (constants) |def_idx| {
        const def = cir.store.getDef(def_idx);
        const pattern = cir.store.getPattern(def.pattern);

        if (pattern == .assign) {
            try ident_to_def.put(allocator, pattern.assign.ident, def_idx);
        }
    }

    // Second pass: collect dependencies and build graph
    for (constants) |def_idx| {
        const def = cir.store.getDef(def_idx);

        // Collect all identifiers this def's expression references
        var deps = std.AutoHashMapUnmanaged(base.Ident.Idx, void){};
        defer deps.deinit(allocator);

        try collectExprDependencies(cir, def.expr, &deps, allocator);

        // Convert ident dependencies to def dependencies
        var dep_iter = deps.keyIterator();
        while (dep_iter.next()) |ident_idx| {
            if (ident_to_def.get(ident_idx.*)) |dep_def_idx| {
                try graph.addEdge(def_idx, dep_def_idx);
            }
            // If ident not found in ident_to_def, it's either:
            // - A function (not a constant)
            // - A builtin function
            // - An external module reference
            // - A parameter/local variable
            // In all cases, we don't need to track it for constant evaluation order
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
