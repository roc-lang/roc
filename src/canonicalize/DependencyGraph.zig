//! Dependency Graph and SCC computation for top-level definitions
//!
//! This module provides dependency analysis for top-level definitions to enable
//! proper evaluation ordering. It computes Strongly Connected Components (SCCs)
//! using Tarjan's algorithm and provides a topologically sorted evaluation order.
//!
//! NOTE: This handles ALL top-level definitions including:
//! - Regular top-level definitions (e.g., `foo = 42`)
//! - Associated items (e.g., `TypeName.itemName = 5` from `TypeName := T.{ itemName = 5 }`)
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
    edges: std.AutoHashMapUnmanaged(CIR.Def.Idx, std.ArrayListUnmanaged(CIR.Def.Idx)),

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
    pub fn addEdge(self: *DependencyGraph, from_def: CIR.Def.Idx, to_def: CIR.Def.Idx) !void {
        const gop = try self.edges.getOrPut(self.allocator, from_def);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
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
) !void {
    const expr = cir.store.getExpr(expr_idx);

    switch (expr) {
        .e_lookup_local => |lookup| {
            // This is a variable reference - add to dependencies
            const pattern = cir.store.getPattern(lookup.pattern_idx);
            if (pattern == .assign) {
                try dependencies.put(allocator, pattern.assign.ident, {});
            }
        },

        .e_call => |call| {
            // Recurse into function and arguments
            try collectExprDependencies(cir, call.func, dependencies, allocator);
            for (cir.store.sliceExpr(call.args)) |arg_idx| {
                try collectExprDependencies(cir, arg_idx, dependencies, allocator);
            }
        },

        .e_lambda => |lambda| {
            // Recurse into lambda body
            // Note: We should skip parameters in the lambda's scope
            try collectExprDependencies(cir, lambda.body, dependencies, allocator);
        },

        .e_closure => |closure| {
            // Recurse into the lambda expression
            try collectExprDependencies(cir, closure.lambda_idx, dependencies, allocator);
        },

        .e_if => |if_expr| {
            for (cir.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = cir.store.getIfBranch(branch_idx);
                try collectExprDependencies(cir, branch.cond, dependencies, allocator);
                try collectExprDependencies(cir, branch.body, dependencies, allocator);
            }
            try collectExprDependencies(cir, if_expr.final_else, dependencies, allocator);
        },

        .e_match => |match_expr| {
            try collectExprDependencies(cir, match_expr.cond, dependencies, allocator);
            for (cir.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = cir.store.getMatchBranch(branch_idx);
                try collectExprDependencies(cir, branch.value, dependencies, allocator);
                if (branch.guard) |guard_idx| {
                    try collectExprDependencies(cir, guard_idx, dependencies, allocator);
                }
            }
        },

        .e_list => |list| {
            for (cir.store.sliceExpr(list.elems)) |elem_idx| {
                try collectExprDependencies(cir, elem_idx, dependencies, allocator);
            }
        },

        .e_record => |record| {
            for (cir.store.sliceRecordFields(record.fields)) |field_idx| {
                const field = cir.store.getRecordField(field_idx);
                try collectExprDependencies(cir, field.value, dependencies, allocator);
            }
            // Handle record update syntax: { ..base, field: value }
            if (record.ext) |ext_idx| {
                try collectExprDependencies(cir, ext_idx, dependencies, allocator);
            }
        },

        .e_dot_access => |access| {
            try collectExprDependencies(cir, access.receiver, dependencies, allocator);
            if (access.args) |args_span| {
                for (cir.store.sliceExpr(args_span)) |arg_idx| {
                    try collectExprDependencies(cir, arg_idx, dependencies, allocator);
                }
            }
        },

        .e_tuple => |tuple| {
            for (cir.store.sliceExpr(tuple.elems)) |elem_idx| {
                try collectExprDependencies(cir, elem_idx, dependencies, allocator);
            }
        },

        .e_binop => |binop| {
            try collectExprDependencies(cir, binop.lhs, dependencies, allocator);
            try collectExprDependencies(cir, binop.rhs, dependencies, allocator);
        },

        .e_unary_minus => |unop| {
            try collectExprDependencies(cir, unop.expr, dependencies, allocator);
        },

        .e_unary_not => |unop| {
            try collectExprDependencies(cir, unop.expr, dependencies, allocator);
        },

        .e_block => |block| {
            // Recurse into the block's statements
            for (cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                const stmt = cir.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        try collectExprDependencies(cir, decl.expr, dependencies, allocator);
                    },
                    .s_var => |var_stmt| {
                        try collectExprDependencies(cir, var_stmt.expr, dependencies, allocator);
                    },
                    .s_reassign => |reassign| {
                        try collectExprDependencies(cir, reassign.expr, dependencies, allocator);
                    },
                    .s_dbg => |dbg| {
                        try collectExprDependencies(cir, dbg.expr, dependencies, allocator);
                    },
                    .s_expr => |expr_stmt| {
                        try collectExprDependencies(cir, expr_stmt.expr, dependencies, allocator);
                    },
                    .s_expect => |expect| {
                        try collectExprDependencies(cir, expect.body, dependencies, allocator);
                    },
                    .s_for => |for_stmt| {
                        try collectExprDependencies(cir, for_stmt.expr, dependencies, allocator);
                    },
                    .s_return => |ret| {
                        try collectExprDependencies(cir, ret.expr, dependencies, allocator);
                    },
                    .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno, .s_crash, .s_runtime_error => {},
                }
            }
            // Recurse into the final expression
            try collectExprDependencies(cir, block.final_expr, dependencies, allocator);
        },

        .e_tag => |tag| {
            for (cir.store.sliceExpr(tag.args)) |arg_idx| {
                try collectExprDependencies(cir, arg_idx, dependencies, allocator);
            }
        },

        .e_nominal => |nominal| {
            try collectExprDependencies(cir, nominal.backing_expr, dependencies, allocator);
        },

        // Literals have no dependencies
        .e_num, .e_frac_f32, .e_frac_f64, .e_dec, .e_dec_small, .e_str, .e_str_segment, .e_empty_list, .e_empty_record, .e_zero_argument_tag, .e_ellipsis => {},

        // External lookups reference other modules - skip for now
        .e_lookup_external => {},

        .e_nominal_external => |nominal| {
            try collectExprDependencies(cir, nominal.backing_expr, dependencies, allocator);
        },

        // Crash has a string literal message (no dependencies)
        .e_crash => {},

        .e_dbg => |dbg| {
            try collectExprDependencies(cir, dbg.expr, dependencies, allocator);
        },

        .e_expect => |expect| {
            try collectExprDependencies(cir, expect.body, dependencies, allocator);
        },

        .e_runtime_error => {},
    }
}

/// Build a dependency graph for all definitions
pub fn buildDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) !DependencyGraph {
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
) !EvaluationOrder {
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
    stack: std.ArrayListUnmanaged(CIR.Def.Idx),

    /// Set of nodes currently on stack
    on_stack: std.AutoHashMapUnmanaged(CIR.Def.Idx, void),

    /// Resulting SCCs (in reverse topological order during construction)
    sccs: std.ArrayListUnmanaged(SCC),

    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TarjanState {
        return .{
            .index = 0,
            .indices = .{},
            .lowlinks = .{},
            .visited = .{},
            .stack = .{},
            .on_stack = .{},
            .sccs = .{},
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

    fn strongConnect(
        self: *TarjanState,
        graph: *const DependencyGraph,
        v: CIR.Def.Idx,
    ) !void {
        // Set the depth index for v
        try self.indices.put(self.allocator, v, self.index);
        try self.lowlinks.put(self.allocator, v, self.index);
        try self.visited.put(self.allocator, v, {});
        self.index += 1;

        try self.stack.append(self.allocator, v);
        try self.on_stack.put(self.allocator, v, {});

        // Consider successors of v
        const dependencies = graph.getDependencies(v);
        for (dependencies) |w| {
            if (!self.visited.contains(w)) {
                // Successor w has not yet been visited; recurse on it
                try self.strongConnect(graph, w);
                const v_lowlink = self.lowlinks.get(v).?;
                const w_lowlink = self.lowlinks.get(w).?;
                try self.lowlinks.put(self.allocator, v, @min(v_lowlink, w_lowlink));
            } else if (self.on_stack.contains(w)) {
                // Successor w is on stack, hence in the current SCC
                const v_lowlink = self.lowlinks.get(v).?;
                const w_index = self.indices.get(w).?;
                try self.lowlinks.put(self.allocator, v, @min(v_lowlink, w_index));
            }
        }

        // If v is a root node, pop the stack and create an SCC
        const v_lowlink = self.lowlinks.get(v).?;
        const v_index = self.indices.get(v).?;
        if (v_lowlink == v_index) {
            var scc_defs = std.ArrayListUnmanaged(CIR.Def.Idx){};

            while (true) {
                const w = self.stack.pop() orelse unreachable; // Stack should not be empty
                _ = self.on_stack.remove(w);
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
    }
};
