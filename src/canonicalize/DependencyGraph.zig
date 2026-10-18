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
const collections = @import("collections");
const types = @import("types");
const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");
const default_omissions = @import("default_omissions.zig");
const Var = types.Var;

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

/// One exact edge from the dependency analysis that produced an evaluation
/// order. Edges point from dependent to dependency.
pub const Dependency = struct {
    dependent: CIR.Def.Idx,
    dependency: CIR.Def.Idx,

    pub const SafeList = collections.SafeList(@This());
};

/// One dispatch whose method target checking selected, available to
/// strict-demand analysis. `fn_var` is the exact raw callable variable for
/// this dispatch edge; scheme-use pairs map generalized source constraints to
/// these selected instantiations at their forcing call sites.
pub const ResolvedDispatchTarget = struct {
    fn_var: Var,
    /// The dispatch this one was derived for (a component of a derived
    /// method, comparison, or hash), whose demand includes this one's.
    parent_fn_var: ?Var,
    /// The expression that introduced the dispatch. A structural comparison
    /// or hash carries no callable variable of its own, so its component
    /// dispatches are found through the expression they serve.
    intro_expr: ?CIR.Expr.Idx,
    target: Target,

    pub const Target = union(enum) {
        /// A method defined in this module.
        local: CIR.Def.Idx,
        /// A method defined in another module. Imports are acyclic, so its
        /// body cannot demand this module's values; `invokes` is its module's
        /// record of whether calling it can invoke a function value it does
        /// not name.
        foreign: struct { invokes: bool },
    };
};

/// A lookup naming a definition of another module.
pub const ForeignCallee = union(enum) {
    external: @FieldType(CIR.Expr, "e_lookup_external"),
    associated: @FieldType(CIR.Expr, "e_lookup_associated_resolved"),
};

/// What checking knows about functions defined in other modules.
pub const ForeignCallees = struct {
    context: *const anyopaque,
    /// Whether calling the foreign function `callee` names can invoke a
    /// function value it does not name. A lookup that names a value rather
    /// than a function is a call of whatever function that value holds.
    invokes: *const fn (context: *const anyopaque, callee: ForeignCallee) std.mem.Allocator.Error!bool,
};

/// The checked strict-demand relation, and the module's functions whose
/// calls can invoke a function value they do not name (design.md "Function
/// Values In Strict Demand"), in ascending order.
pub const CheckedDemand = struct {
    graph: DependencyGraph,
    invoking_defs: std.ArrayList(CIR.Def.Idx),

    pub fn deinit(self: *CheckedDemand, allocator: std.mem.Allocator) void {
        self.graph.deinit();
        self.invoking_defs.deinit(allocator);
    }
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

/// The order in which type checking processes a module's top-level defs,
/// together with each group's direct name dependencies. Groups are indexed by
/// their position in `sccs`; every dependency index is smaller than the index
/// of the group that depends on it.
pub const CheckOrder = struct {
    /// Binding groups in deterministic topological order
    /// (dependencies come before dependents).
    sccs: []SCC,

    /// Offsets into `group_dependencies`: the direct name dependencies of
    /// group `g` are `group_dependencies[group_dependency_starts[g]..group_dependency_starts[g + 1]]`.
    group_dependency_starts: []u32,

    /// Every group's direct name dependencies (other groups only, each listed
    /// once), in ascending group order within each group's range.
    group_dependencies: []u32,

    allocator: std.mem.Allocator,

    /// The groups that `group_index` names directly, in ascending order.
    pub fn dependenciesOf(self: *const CheckOrder, group_index: u32) []const u32 {
        return self.group_dependencies[self.group_dependency_starts[group_index]..self.group_dependency_starts[group_index + 1]];
    }

    pub fn deinit(self: *CheckOrder) void {
        for (self.sccs) |scc| {
            self.allocator.free(scc.defs);
        }
        self.allocator.free(self.sccs);
        self.allocator.free(self.group_dependency_starts);
        self.allocator.free(self.group_dependencies);
    }
};

/// Collect the graph's exact edges in deterministic order for downstream
/// consumers that need the strict top-level demand relation.
pub fn collectDependencies(
    graph: *const DependencyGraph,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Dependency.SafeList {
    var dependencies = try Dependency.SafeList.initCapacity(allocator, graph.nodes.len);
    errdefer dependencies.deinit(allocator);

    for (graph.nodes) |dependent| {
        for (graph.getDependencies(dependent)) |dependency| {
            _ = try dependencies.append(allocator, .{
                .dependent = dependent,
                .dependency = dependency,
            });
        }
    }

    const DependencySort = struct {
        fn lessThan(_: void, a: Dependency, b: Dependency) bool {
            const a_dependent = @backingInt(a.dependent);
            const b_dependent = @backingInt(b.dependent);
            if (a_dependent != b_dependent) return a_dependent < b_dependent;
            return @backingInt(a.dependency) < @backingInt(b.dependency);
        }
    };
    std.mem.sort(Dependency, dependencies.items.items, {}, DependencySort.lessThan);

    var unique_len: usize = 0;
    for (dependencies.items.items) |dependency| {
        if (unique_len != 0) {
            const previous = dependencies.items.items[unique_len - 1];
            if (previous.dependent == dependency.dependent and previous.dependency == dependency.dependency) continue;
        }
        dependencies.items.items[unique_len] = dependency;
        unique_len += 1;
    }
    dependencies.items.items.len = unique_len;

    return dependencies;
}

/// Whether a sorted exact dependency set contains one edge.
pub fn hasDependency(
    dependencies: []const Dependency,
    dependent: CIR.Def.Idx,
    dependency: CIR.Def.Idx,
) bool {
    const dependent_raw = @backingInt(dependent);
    const dependency_raw = @backingInt(dependency);
    var low: usize = 0;
    var high: usize = dependencies.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        const candidate = dependencies[mid];
        const candidate_dependent = @backingInt(candidate.dependent);
        const candidate_dependency = @backingInt(candidate.dependency);
        if (candidate_dependent < dependent_raw or
            (candidate_dependent == dependent_raw and candidate_dependency < dependency_raw))
        {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    if (low == dependencies.len) return false;
    const candidate = dependencies[low];
    return candidate.dependent == dependent and candidate.dependency == dependency;
}

const DemandSummary = struct {
    deps: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{},
    /// Lambdas whose summarized graph deps this demand also has. A callee's
    /// deps are reached through its summary rather than copied into every
    /// caller's, so a call chain's summaries stay the size of each body's
    /// direct demands; `DemandAnalyzer.expandDeps` collects the transitive set
    /// once per def that needs its edges.
    deps_from: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},
    called_patterns: std.AutoHashMapUnmanaged(CIR.Pattern.Idx, void) = .{},
    /// Dispatch callable vars whose target remains polymorphic in this
    /// summary: literal conversions and method dispatches alike.
    dispatch_requirements: std.AutoHashMapUnmanaged(Var, void) = .{},
    /// Lambdas this demand references as values, each with its own stored
    /// summary. They join the demand when it can invoke a function value it
    /// does not name (`invokes`).
    fn_values: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},
    /// Whether this demand calls a function value it does not name: a call
    /// of a computed value, of a value-holding definition, or of an imported
    /// function that can itself do so.
    invokes: bool = false,

    fn deinit(self: *DemandSummary, allocator: std.mem.Allocator) void {
        self.deps.deinit(allocator);
        self.deps_from.deinit(allocator);
        self.called_patterns.deinit(allocator);
        self.dispatch_requirements.deinit(allocator);
        self.fn_values.deinit(allocator);
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

    fn addDispatchRequirement(self: *DemandSummary, allocator: std.mem.Allocator, fn_var: Var) std.mem.Allocator.Error!bool {
        const gop = try self.dispatch_requirements.getOrPut(allocator, fn_var);
        if (gop.found_existing) return false;
        gop.value_ptr.* = {};
        return true;
    }

    fn addFnValue(self: *DemandSummary, allocator: std.mem.Allocator, lambda_idx: CIR.Expr.Idx) std.mem.Allocator.Error!bool {
        const gop = try self.fn_values.getOrPut(allocator, lambda_idx);
        return !gop.found_existing;
    }

    fn mergeFrom(self: *DemandSummary, allocator: std.mem.Allocator, other: *const DemandSummary) std.mem.Allocator.Error!bool {
        var changed = false;

        var dep_iter = other.deps.keyIterator();
        while (dep_iter.next()) |def_idx| {
            if (try self.addDep(allocator, def_idx.*)) changed = true;
        }

        var from_iter = other.deps_from.keyIterator();
        while (from_iter.next()) |lambda_idx| {
            const gop = try self.deps_from.getOrPut(allocator, lambda_idx.*);
            if (!gop.found_existing) changed = true;
        }

        var called_iter = other.called_patterns.keyIterator();
        while (called_iter.next()) |pattern_idx| {
            if (try self.addCalledPattern(allocator, pattern_idx.*)) changed = true;
        }

        var requirement_iter = other.dispatch_requirements.keyIterator();
        while (requirement_iter.next()) |fn_var| {
            if (try self.addDispatchRequirement(allocator, fn_var.*)) changed = true;
        }

        var value_iter = other.fn_values.keyIterator();
        while (value_iter.next()) |lambda_idx| {
            if (try self.addFnValue(allocator, lambda_idx.*)) changed = true;
        }

        if (other.invokes and !self.invokes) {
            self.invokes = true;
            changed = true;
        }

        return changed;
    }
};

const LocalCallables = std.AutoHashMapUnmanaged(CIR.Pattern.Idx, CIR.Expr.Idx);

/// The selected targets of one dispatch callable root.
const DispatchResolution = struct {
    locals: std.ArrayListUnmanaged(CIR.Def.Idx) = .empty,
    /// Some foreign target can invoke a function value it does not name.
    foreign_invokes: bool = false,
    /// Dispatch roots derived for this one, whose demand is part of it.
    children: std.ArrayListUnmanaged(Var) = .empty,
    /// Whether any edge selected a target for this root.
    resolved: bool = false,

    fn deinit(self: *DispatchResolution, allocator: std.mem.Allocator) void {
        self.locals.deinit(allocator);
        self.children.deinit(allocator);
    }
};

const DemandAnalyzer = struct {
    cir: *const ModuleEnv,
    allocator: std.mem.Allocator,
    summary_defs: []const CIR.Def.Idx,
    graph_def_set: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{},
    pattern_to_def: collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    resolved_dispatch: std.AutoHashMapUnmanaged(Var, DispatchResolution) = .{},
    /// Dispatch roots by the expression that introduced them, for the
    /// structural comparisons and hashes whose nodes carry no callable.
    dispatch_by_intro: std.AutoHashMapUnmanaged(CIR.Expr.Idx, std.ArrayListUnmanaged(Var)) = .{},
    /// Present once checking has run: function values then demand their
    /// bodies whenever their holder can invoke them (design.md "Function
    /// Values In Strict Demand"). Canonicalization's initial relation has no
    /// such knowledge of imported functions and leaves function values out.
    foreign: ?ForeignCallees = null,
    scheme_use_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, u32) = .{},
    /// The `iter` and `next` dispatch callables of each `for` loop, by node.
    for_loop_dispatch: std.AutoHashMapUnmanaged(u32, [2]Var) = .{},
    summaries: std.AutoHashMapUnmanaged(CIR.Expr.Idx, DemandSummary) = .{},
    active_lambdas: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},
    /// The lambdas of `summary_defs`, whose summaries `computeSummaries`
    /// computes as jobs of their own.
    summary_lambdas: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},
    /// Lambdas used as values whose bodies this pass has walked. A pass
    /// walks each once: its stored summary only grows, and the next pass
    /// walks it again with whatever the summaries it read have gained.
    walked_value_lambdas: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},
    /// Set when a summary job's walk needs a summary lambda's summary that
    /// is not computed yet: that lambda's job runs first, then the walk
    /// resumes.
    needed_summary: ?CIR.Expr.Idx = null,

    fn init(
        cir: *const ModuleEnv,
        summary_defs: []const CIR.Def.Idx,
        graph_defs: []const CIR.Def.Idx,
        resolved_dispatch_targets: []const ResolvedDispatchTarget,
        foreign: ?ForeignCallees,
        allocator: std.mem.Allocator,
    ) std.mem.Allocator.Error!DemandAnalyzer {
        var analyzer = DemandAnalyzer{
            .cir = cir,
            .allocator = allocator,
            .summary_defs = summary_defs,
            .pattern_to_def = .init(allocator),
            .foreign = foreign,
        };
        errdefer analyzer.deinit();

        for (graph_defs) |def_idx| {
            try analyzer.graph_def_set.put(allocator, def_idx, {});
        }

        var binders: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer binders.deinit(allocator);
        var binder_scratch: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer binder_scratch.deinit(allocator);
        for (summary_defs) |def_idx| {
            try mapDefBinderPatterns(cir, &analyzer.pattern_to_def, def_idx, &binders, &binder_scratch, allocator);
        }

        for (resolved_dispatch_targets) |target| {
            const fn_root = cir.types.resolveVar(target.fn_var).var_;
            const entry = try analyzer.resolved_dispatch.getOrPut(allocator, fn_root);
            if (!entry.found_existing) entry.value_ptr.* = .{};
            const resolution = entry.value_ptr;
            resolution.resolved = true;
            switch (target.target) {
                .local => |target_def| for (resolution.locals.items) |existing| {
                    if (existing == target_def) break;
                } else {
                    try resolution.locals.append(allocator, target_def);
                },
                .foreign => |foreign_target| resolution.foreign_invokes = resolution.foreign_invokes or foreign_target.invokes,
            }
            if (target.parent_fn_var) |parent| {
                const parent_root = cir.types.resolveVar(parent).var_;
                if (parent_root != fn_root) {
                    const parent_entry = try analyzer.resolved_dispatch.getOrPut(allocator, parent_root);
                    if (!parent_entry.found_existing) parent_entry.value_ptr.* = .{};
                    for (parent_entry.value_ptr.children.items) |existing| {
                        if (existing == fn_root) break;
                    } else {
                        try parent_entry.value_ptr.children.append(allocator, fn_root);
                    }
                }
            }
            if (target.intro_expr) |intro| {
                const intro_entry = try analyzer.dispatch_by_intro.getOrPut(allocator, intro);
                if (!intro_entry.found_existing) intro_entry.value_ptr.* = .empty;
                for (intro_entry.value_ptr.items) |existing| {
                    if (existing == fn_root) break;
                } else {
                    try intro_entry.value_ptr.append(allocator, fn_root);
                }
            }
        }

        for (cir.scheme_uses.items.items, 0..) |record, record_index| {
            switch (@as(ModuleEnv.SchemeUseRecord.Slot, @fromBackingInt(@intCast(record.slot_kind)))) {
                .value_use, .shared_value_use => {
                    const entry = try analyzer.scheme_use_by_node.getOrPut(allocator, @fromBackingInt(@intCast(record.node_idx)));
                    if (!entry.found_existing) entry.value_ptr.* = @intCast(record_index);
                },
                .nested_function_use, .dispatch_target, .recursive_dispatch_target, .recursive_reference, .recursive_value_reference, .where_method_use => {},
            }
        }

        for (cir.for_loop_dispatch_plans.items.items) |plan| {
            try analyzer.for_loop_dispatch.put(allocator, plan.node_idx, .{
                @fromBackingInt(@intCast(plan.iter_fn_var)),
                @fromBackingInt(@intCast(plan.next_fn_var)),
            });
        }

        return analyzer;
    }

    fn deinit(self: *DemandAnalyzer) void {
        self.for_loop_dispatch.deinit(self.allocator);
        var summary_iter = self.summaries.valueIterator();
        while (summary_iter.next()) |summary| {
            summary.deinit(self.allocator);
        }
        self.summaries.deinit(self.allocator);
        self.pattern_to_def.deinit();
        self.graph_def_set.deinit(self.allocator);
        var resolved_iter = self.resolved_dispatch.valueIterator();
        while (resolved_iter.next()) |resolution| resolution.deinit(self.allocator);
        self.resolved_dispatch.deinit(self.allocator);
        var intro_iter = self.dispatch_by_intro.valueIterator();
        while (intro_iter.next()) |roots| roots.deinit(self.allocator);
        self.dispatch_by_intro.deinit(self.allocator);
        self.scheme_use_by_node.deinit(self.allocator);
        self.active_lambdas.deinit(self.allocator);
        self.summary_lambdas.deinit(self.allocator);
        self.walked_value_lambdas.deinit(self.allocator);
    }

    /// The summary of one lambda being computed: its argument patterns and
    /// then its body walk with the lambda marked active, exactly as an
    /// inline execution frame would.
    const SummaryJob = struct {
        lambda: CIR.Expr.Idx,
        computed: DemandSummary = .{},
        local_callables: LocalCallables = .{},
        walk: Walk = .{},
        /// Argument patterns walked so far; one more once the body walk has
        /// started.
        next_part: usize = 0,

        fn deinit(job: *SummaryJob, allocator: std.mem.Allocator) void {
            job.walk.deinit(allocator);
            job.local_callables.deinit(allocator);
            job.computed.deinit(allocator);
        }
    };

    /// Summaries grow monotonically to their least fixpoint. Within each
    /// round, a walk that needs a summary lambda's summary before it exists
    /// suspends while that lambda's own job runs, so callee summaries are
    /// computed before their callers fold them rather than being re-walked
    /// inline by every caller.
    fn computeSummaries(self: *DemandAnalyzer) std.mem.Allocator.Error!void {
        for (self.summary_defs) |def_idx| {
            const lambda_idx = self.lambdaFromDef(def_idx) orelse continue;
            if (self.cir.store.getExpr(lambda_idx) == .e_lambda) try self.summary_lambdas.put(self.allocator, lambda_idx, {});
        }
        defer self.summary_lambdas.clearRetainingCapacity();

        var jobs = std.ArrayList(SummaryJob).empty;
        defer {
            for (jobs.items) |*job| {
                _ = self.active_lambdas.remove(job.lambda);
                job.deinit(self.allocator);
            }
            jobs.deinit(self.allocator);
        }
        var changed = true;
        while (changed) {
            changed = false;
            self.walked_value_lambdas.clearRetainingCapacity();
            for (self.summary_defs) |def_idx| {
                const lambda_idx = self.lambdaFromDef(def_idx) orelse continue;
                if (self.cir.store.getExpr(lambda_idx) != .e_lambda or self.active_lambdas.contains(lambda_idx)) {
                    // A lambda already being walked contributes nothing new
                    // on this pass; its summary entry still exists.
                    const nothing = DemandSummary{};
                    if (try self.mergeSummary(lambda_idx, &nothing)) changed = true;
                    continue;
                }
                try self.startSummaryJob(&jobs, lambda_idx);
                while (jobs.items.len != 0) {
                    const job = &jobs.items[jobs.items.len - 1];
                    if (job.walk.work.items.len != 0) {
                        try self.drainWalk(&job.walk, &job.computed, &job.local_callables);
                        if (self.needed_summary) |needed| {
                            self.needed_summary = null;
                            try self.startSummaryJob(&jobs, needed);
                        }
                        continue;
                    }
                    const args = self.cir.store.slicePatterns(self.cir.store.getExpr(job.lambda).e_lambda.args);
                    if (job.next_part <= args.len) {
                        // Each argument pattern and the body is a walk of its own.
                        job.walk.deinit(self.allocator);
                        job.walk = .{};
                        if (job.next_part < args.len) {
                            try job.walk.push(self.allocator, .{ .visit_pattern = args[job.next_part] });
                        } else {
                            try job.walk.push(self.allocator, .{ .visit = self.cir.store.getExpr(job.lambda).e_lambda.body });
                        }
                        job.next_part += 1;
                        continue;
                    }
                    var finished = jobs.pop().?;
                    defer finished.deinit(self.allocator);
                    _ = self.active_lambdas.remove(finished.lambda);
                    if (try self.mergeSummary(finished.lambda, &finished.computed)) changed = true;
                }
            }
        }
    }

    fn startSummaryJob(self: *DemandAnalyzer, jobs: *std.ArrayList(SummaryJob), lambda_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
        try jobs.ensureUnusedCapacity(self.allocator, 1);
        try self.active_lambdas.put(self.allocator, lambda_idx, {});
        jobs.appendAssumeCapacity(.{ .lambda = lambda_idx });
    }

    /// Merge a computed summary into the lambda's stored summary; true when
    /// the stored summary grew or was created.
    fn mergeSummary(self: *DemandAnalyzer, lambda_idx: CIR.Expr.Idx, computed: *const DemandSummary) std.mem.Allocator.Error!bool {
        const gop = try self.summaries.getOrPut(self.allocator, lambda_idx);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        return try gop.value_ptr.mergeFrom(self.allocator, computed);
    }

    fn collectDefDependencies(self: *DemandAnalyzer, def_idx: CIR.Def.Idx, out: *DemandSummary) std.mem.Allocator.Error!void {
        var local_callables = LocalCallables{};
        defer local_callables.deinit(self.allocator);

        const def = self.cir.store.getDef(def_idx);
        try self.walkDemand(def.expr, out, &local_callables);
        try self.walkPatternDemand(def.pattern, out, &local_callables);
    }

    /// Every graph dep a def's demand reaches: its own, and each summarized
    /// lambda's reached through `deps_from`, visiting each lambda once. Once
    /// checking has run, the function values the demand reaches join it too
    /// when it can invoke a function value it does not name. A call through
    /// a binder that no call site's argument resolved is such an invocation.
    fn expandDeps(self: *DemandAnalyzer, demand: *DemandSummary) std.mem.Allocator.Error!void {
        var visited = std.AutoHashMapUnmanaged(CIR.Expr.Idx, void).empty;
        defer visited.deinit(self.allocator);
        var pending = std.ArrayList(CIR.Expr.Idx).empty;
        defer pending.deinit(self.allocator);
        var roots = demand.deps_from.keyIterator();
        while (roots.next()) |lambda_idx| try pending.append(self.allocator, lambda_idx.*);
        if (self.foreign != null and demand.called_patterns.count() != 0) demand.invokes = true;
        while (true) {
            while (pending.pop()) |lambda_idx| {
                if ((try visited.getOrPut(self.allocator, lambda_idx)).found_existing) continue;
                const summary = self.summaries.getPtr(lambda_idx) orelse continue;
                var deps = summary.deps.keyIterator();
                while (deps.next()) |def_idx| _ = try demand.addDep(self.allocator, def_idx.*);
                var callees = summary.deps_from.keyIterator();
                while (callees.next()) |callee| try pending.append(self.allocator, callee.*);
                if (self.foreign == null) continue;
                var values = summary.fn_values.keyIterator();
                while (values.next()) |value| _ = try demand.addFnValue(self.allocator, value.*);
                if (summary.invokes) demand.invokes = true;
            }
            if (!try self.invokeFnValues(demand, &visited, &pending)) return;
        }
    }

    /// When `demand` can invoke a function value it does not name, queue
    /// every function value it reaches that `visited` has not expanded; true
    /// when any was queued.
    fn invokeFnValues(
        self: *DemandAnalyzer,
        demand: *const DemandSummary,
        visited: *const std.AutoHashMapUnmanaged(CIR.Expr.Idx, void),
        pending: *std.ArrayList(CIR.Expr.Idx),
    ) std.mem.Allocator.Error!bool {
        if (!demand.invokes) return false;
        var queued = false;
        var values = demand.fn_values.keyIterator();
        while (values.next()) |value| {
            if (visited.contains(value.*)) continue;
            try pending.append(self.allocator, value.*);
            queued = true;
        }
        return queued;
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
            const expr = self.cir.store.getExpr(current);
            const tag = std.meta.activeTag(expr);
            if (tag == .e_lambda) return current;
            if (tag == .e_closure) return expr.e_closure.lambda_idx;
            if (tag != .e_lookup_local) return null;

            const lookup = expr.e_lookup_local;
            if (local_callables) |locals| {
                if (locals.get(lookup.pattern_idx)) |lambda_idx| return lambda_idx;
            }
            const def_idx = self.pattern_to_def.get(lookup.pattern_idx) orelse return null;
            current = self.cir.store.getDef(def_idx).expr;
        }
        return null;
    }

    /// Record a lambda used as a value. Once checking has run, its body's
    /// demand is summarized under its own identity, so whatever holds the
    /// value reaches it when that holder can invoke it.
    fn noteFnValue(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        lambda_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        if (self.foreign == null) return;
        _ = try current.addFnValue(self.allocator, lambda_idx);
        // Definitions' lambdas are summarized as jobs of their own.
        if (self.summary_lambdas.contains(lambda_idx) or self.active_lambdas.contains(lambda_idx)) return;
        const expr = self.cir.store.getExpr(lambda_idx);
        if (expr != .e_lambda) return;
        if ((try self.walked_value_lambdas.getOrPut(self.allocator, lambda_idx)).found_existing) return;
        try self.active_lambdas.put(self.allocator, lambda_idx, {});
        try walk.frames.ensureUnusedCapacity(self.allocator, 1);
        const frame = try self.allocator.create(DemandSummary);
        frame.* = .{};
        walk.frames.appendAssumeCapacity(frame);
        try walk.push(self.allocator, .{ .finish_value_lambda = lambda_idx });
        try walk.push(self.allocator, .{ .visit = expr.e_lambda.body });
        try self.pushPatternSpanReversed(walk, expr.e_lambda.args);
    }

    /// The local definition an associated lookup checking resolved names,
    /// when it names one of this module's own.
    fn associatedLocalDef(self: *const DemandAnalyzer, resolved: anytype) ?CIR.Def.Idx {
        if (resolved.module_identity != self.cir.self_module_identity) return null;
        return resolved.target_def_idx;
    }

    /// A statically selected method is called like the function it names.
    fn applyMethodBinding(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        def_idx: CIR.Def.Idx,
    ) std.mem.Allocator.Error!void {
        const lambda_idx = self.lambdaFromDef(def_idx) orelse return;
        const no_source_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
        try self.beginApplyLambdaSummary(walk, current, lambda_idx, no_source_args, null, null);
    }

    /// A `for` loop dispatches its iteration methods on every pass.
    fn applyForLoopDispatch(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        node_idx: CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        const fn_vars = self.for_loop_dispatch.get(@backingInt(node_idx)) orelse return;
        for (fn_vars) |fn_var| try self.applyDispatchRequirement(walk, current, fn_var, null);
    }

    /// A call whose target this module's demand cannot name invokes some
    /// function value; checking alone knows which imported functions do.
    fn noteInvocation(self: *const DemandAnalyzer, current: *DemandSummary) void {
        if (self.foreign != null) current.invokes = true;
    }

    fn noteForeignCall(self: *const DemandAnalyzer, current: *DemandSummary, callee: ForeignCallee) std.mem.Allocator.Error!void {
        const foreign = self.foreign orelse return;
        if (try foreign.invokes(foreign.context, callee)) current.invokes = true;
    }

    /// Whether calling `def_idx` can invoke a function value it does not
    /// name: a function whose body can, or a definition holding a value,
    /// whose call invokes whatever function it holds.
    fn defInvokes(self: *DemandAnalyzer, def_idx: CIR.Def.Idx) std.mem.Allocator.Error!bool {
        const lambda_idx = self.lambdaFromDef(def_idx) orelse return true;
        const summary = self.summaries.getPtr(lambda_idx) orelse return false;
        // A called parameter invokes whatever the caller passes.
        if (summary.called_patterns.count() != 0) return true;
        var demand = DemandSummary{};
        defer demand.deinit(self.allocator);
        try demand.deps_from.put(self.allocator, lambda_idx, {});
        try self.expandDeps(&demand);
        return demand.invokes;
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
        /// Pre-order execution visit of one pattern node.
        visit_pattern: CIR.Pattern.Idx,
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
        finish_lambda: struct { lambda: CIR.Expr.Idx, args: CIR.Expr.Span, demand_node: ?CIR.Node.Idx },
        /// Close the frame of a lambda walked because it is used as a value:
        /// its summary is stored as its own rather than folded into the
        /// enclosing demand, which reaches it through `fn_values`.
        finish_value_lambda: CIR.Expr.Idx,
    };

    const Walk = struct {
        work: std.ArrayList(WalkItem) = .empty,
        /// Summaries of the lambdas whose bodies are being walked inline.
        /// Each is boxed so a summary being folded into stays in place while
        /// the fold starts further lambda walks.
        frames: std.ArrayList(*DemandSummary) = .empty,
        /// Dedups omitted-default expression pushes per walk, mirroring
        /// `collectNameReferences`'s `scratch_seen_defaults` (see its doc
        /// comment): a default expression is shared across construction
        /// sites, and a chain of defaults that each construct omitting more
        /// defaults would otherwise re-walk with product growth. Demand
        /// contributions are set-based and frames fold into their parents,
        /// so one visit per walk reaches the walk's output.
        seen_defaults: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{},

        fn deinit(walk: *Walk, allocator: std.mem.Allocator) void {
            walk.seen_defaults.deinit(allocator);
            walk.work.deinit(allocator);
            for (walk.frames.items) |frame| destroyFrame(allocator, frame);
            walk.frames.deinit(allocator);
        }

        fn destroyFrame(allocator: std.mem.Allocator, frame: *DemandSummary) void {
            frame.deinit(allocator);
            allocator.destroy(frame);
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

        try self.drainWalk(&walk, out, local_callables);
    }

    fn drainWalk(
        self: *DemandAnalyzer,
        walk: *Walk,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        while (self.needed_summary == null) {
            const item = walk.work.pop() orelse return;
            const current: *DemandSummary = if (walk.frames.items.len > 0)
                walk.frames.items[walk.frames.items.len - 1]
            else
                out;
            switch (item) {
                .visit => |expr_idx| try self.visitExpr(walk, current, expr_idx, local_callables),
                .visit_pattern => |pattern_idx| try self.visitPattern(walk, current, pattern_idx),
                .visit_stmt => |stmt_idx| try self.visitStmt(walk, current, stmt_idx),
                .remember_local => |bind| try self.rememberLocalCallable(bind.pattern, bind.expr, local_callables),
                .apply_call_target => |call| try self.applyCallTarget(walk, current, call.func, call.args, local_callables),
                .apply_called_value => |expr_idx| try self.applyCalledValue(walk, current, expr_idx, local_callables),
                .finish_lambda => |fin| {
                    const computed = walk.frames.pop().?;
                    defer Walk.destroyFrame(self.allocator, computed);
                    _ = self.active_lambdas.remove(fin.lambda);
                    const parent: *DemandSummary = if (walk.frames.items.len > 0)
                        walk.frames.items[walk.frames.items.len - 1]
                    else
                        out;
                    try self.foldLambdaSummary(walk, parent, computed, fin.lambda, fin.args, fin.demand_node);
                },
                .finish_value_lambda => |lambda_idx| {
                    const computed = walk.frames.pop().?;
                    defer Walk.destroyFrame(self.allocator, computed);
                    _ = self.active_lambdas.remove(lambda_idx);
                    _ = try self.mergeSummary(lambda_idx, computed);
                },
            }
        }
    }

    /// Run the demand walk from `root_pattern`, accumulating into `out`.
    fn walkPatternDemand(
        self: *DemandAnalyzer,
        root_pattern: CIR.Pattern.Idx,
        out: *DemandSummary,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        var walk = Walk{};
        defer walk.deinit(self.allocator);
        try walk.push(self.allocator, .{ .visit_pattern = root_pattern });

        try self.drainWalk(&walk, out, local_callables);
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
        demand_node: ?CIR.Node.Idx,
        retry: ?WalkItem,
    ) std.mem.Allocator.Error!void {
        if (self.summaries.getPtr(lambda_idx)) |summary| {
            try self.foldLambdaSummary(walk, current, summary, lambda_idx, call_args, demand_node);
            return;
        }
        if (retry) |item| {
            if (self.summary_lambdas.contains(lambda_idx) and !self.active_lambdas.contains(lambda_idx)) {
                // Compute the lambda's own summary first, then apply it.
                try walk.push(self.allocator, item);
                self.needed_summary = lambda_idx;
                return;
            }
        }
        try self.beginLambdaWalk(walk, lambda_idx, call_args, demand_node);
    }

    /// Walk an uncached lambda's body inline in its own frame, guarded by
    /// `active_lambdas` against recursion; the frame folds when it closes.
    fn beginLambdaWalk(
        self: *DemandAnalyzer,
        walk: *Walk,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        demand_node: ?CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        if (self.active_lambdas.contains(lambda_idx)) return;
        const expr = self.cir.store.getExpr(lambda_idx);
        if (expr != .e_lambda) return;

        try self.active_lambdas.put(self.allocator, lambda_idx, {});
        try walk.frames.ensureUnusedCapacity(self.allocator, 1);
        const frame = try self.allocator.create(DemandSummary);
        frame.* = .{};
        walk.frames.appendAssumeCapacity(frame);
        // The finish item pops first-in-last-out: argument patterns and the
        // body run inside the frame, then the frame folds into its parent.
        try walk.push(self.allocator, .{ .finish_lambda = .{ .lambda = lambda_idx, .args = call_args, .demand_node = demand_node } });
        try walk.push(self.allocator, .{ .visit = expr.e_lambda.body });
        try self.pushPatternSpanReversed(walk, expr.e_lambda.args);
    }

    /// A folded summary whose dispatch requirements are still being applied.
    const FoldCursor = struct {
        requirements: std.AutoHashMapUnmanaged(Var, void).KeyIterator,
        demand_node: ?CIR.Node.Idx,
        /// Target definitions of the requirement being applied.
        targets: std.ArrayListUnmanaged(CIR.Def.Idx) = .empty,
        next_target: usize = 0,
    };

    /// Fold one lambda's demand summary into `into` for a call with
    /// `call_args`: graph deps transfer directly; called patterns flow through
    /// the matching argument; and polymorphic dispatch requirements translate
    /// through the exact scheme-use pairs recorded at `demand_node`. A
    /// requirement with resolved targets applies each target method's own
    /// summary in turn, so cursors over the summaries being applied are kept
    /// on an explicit stack.
    fn foldLambdaSummary(
        self: *DemandAnalyzer,
        walk: *Walk,
        into: *DemandSummary,
        summary: *const DemandSummary,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
        demand_node: ?CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        var cursors = std.ArrayList(FoldCursor).empty;
        defer {
            for (cursors.items) |*cursor| cursor.targets.deinit(self.allocator);
            cursors.deinit(self.allocator);
        }
        try self.foldSummaryEdges(walk, into, summary, lambda_idx, call_args);
        try cursors.append(self.allocator, .{ .requirements = summary.dispatch_requirements.keyIterator(), .demand_node = demand_node });
        while (cursors.items.len > 0) {
            const cursor = &cursors.items[cursors.items.len - 1];
            if (cursor.next_target < cursor.targets.items.len) {
                const target_def = cursor.targets.items[cursor.next_target];
                cursor.next_target += 1;
                const target_lambda = self.lambdaFromDef(target_def) orelse continue;
                // A selected method's parameters are not resolved against
                // the dispatch's operands: a called parameter stays an
                // unresolved call, which invokes whatever it is given.
                const no_source_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                if (self.summaries.getPtr(target_lambda)) |target_summary| {
                    try self.foldSummaryEdges(walk, into, target_summary, target_lambda, no_source_args);
                    try cursors.append(self.allocator, .{ .requirements = target_summary.dispatch_requirements.keyIterator(), .demand_node = null });
                } else {
                    try self.beginLambdaWalk(walk, target_lambda, no_source_args, null);
                }
                continue;
            }
            const requirement = cursor.requirements.next() orelse {
                var finished = cursors.pop().?;
                finished.targets.deinit(self.allocator);
                continue;
            };
            const requirement_var = requirement.*;
            const requirement_node = cursor.demand_node;
            cursor.targets.clearRetainingCapacity();
            cursor.next_target = 0;
            switch (try self.dispatchRequirementTarget(requirement_var, requirement_node, &cursor.targets)) {
                .targets => |foreign_invokes| {
                    if (foreign_invokes) into.invokes = true;
                },
                .requirement => |fn_var| _ = try into.addDispatchRequirement(self.allocator, fn_var),
            }
        }
    }

    /// The graph deps and called patterns of a folded summary.
    fn foldSummaryEdges(
        self: *DemandAnalyzer,
        walk: *Walk,
        into: *DemandSummary,
        summary: *const DemandSummary,
        lambda_idx: CIR.Expr.Idx,
        call_args: CIR.Expr.Span,
    ) std.mem.Allocator.Error!void {
        if (self.summaries.getPtr(lambda_idx) == summary) {
            try into.deps_from.put(self.allocator, lambda_idx, {});
        } else {
            // An inline frame's demand has no stored summary to reach later.
            var dep_iter = summary.deps.keyIterator();
            while (dep_iter.next()) |def_idx| try self.addGraphDep(into, def_idx.*);
            var from_iter = summary.deps_from.keyIterator();
            while (from_iter.next()) |callee| try into.deps_from.put(self.allocator, callee.*, {});
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
        const expr = self.cir.store.getExpr(call_func);
        const tag = std.meta.activeTag(expr);
        if (tag == .e_lambda) {
            try self.beginApplyLambdaSummary(walk, current, call_func, call_args, ModuleEnv.nodeIdxFrom(call_func), .{ .apply_call_target = .{ .func = call_func, .args = call_args } });
        } else if (tag == .e_closure) {
            try self.beginApplyLambdaSummary(walk, current, expr.e_closure.lambda_idx, call_args, ModuleEnv.nodeIdxFrom(call_func), .{ .apply_call_target = .{ .func = call_func, .args = call_args } });
        } else if (tag == .e_lookup_local) {
            const lookup = expr.e_lookup_local;
            if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                try self.beginApplyLambdaSummary(walk, current, lambda_idx, call_args, ModuleEnv.nodeIdxFrom(call_func), .{ .apply_call_target = .{ .func = call_func, .args = call_args } });
                return;
            }
            if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, call_args, ModuleEnv.nodeIdxFrom(call_func), .{ .apply_call_target = .{ .func = call_func, .args = call_args } });
                } else {
                    // A definition holding a function value computed some
                    // other way: whatever it holds is invoked.
                    self.noteInvocation(current);
                }
                return;
            }
            _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
        } else if (tag == .e_lookup_associated_resolved) {
            if (self.associatedLocalDef(expr.e_lookup_associated_resolved)) |def_idx| {
                if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, call_args, ModuleEnv.nodeIdxFrom(call_func), .{ .apply_call_target = .{ .func = call_func, .args = call_args } });
                } else {
                    self.noteInvocation(current);
                }
                return;
            }
            try self.noteForeignCall(current, .{ .associated = expr.e_lookup_associated_resolved });
        } else if (tag == .e_lookup_external) {
            try self.noteForeignCall(current, .{ .external = expr.e_lookup_external });
        } else if (tag == .e_hosted_lambda or tag == .e_runtime_error or tag == .e_crash) {
            // A hosted procedure never runs at compile time, and a checked
            // error or crash calls nothing.
        } else {
            self.noteInvocation(current);
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
        const expr = self.cir.store.getExpr(expr_idx);
        const tag = std.meta.activeTag(expr);
        if (tag == .e_lambda) {
            try self.beginApplyLambdaSummary(walk, current, expr_idx, empty_args, ModuleEnv.nodeIdxFrom(expr_idx), .{ .apply_called_value = expr_idx });
        } else if (tag == .e_closure) {
            try self.beginApplyLambdaSummary(walk, current, expr.e_closure.lambda_idx, empty_args, ModuleEnv.nodeIdxFrom(expr_idx), .{ .apply_called_value = expr_idx });
        } else if (tag == .e_lookup_local) {
            const lookup = expr.e_lookup_local;
            if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                try self.beginApplyLambdaSummary(walk, current, lambda_idx, empty_args, ModuleEnv.nodeIdxFrom(expr_idx), .{ .apply_called_value = expr_idx });
                return;
            }
            if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, empty_args, ModuleEnv.nodeIdxFrom(expr_idx), .{ .apply_called_value = expr_idx });
                } else {
                    self.noteInvocation(current);
                }
                return;
            }
            _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
        } else if (tag == .e_lookup_associated_resolved) {
            if (self.associatedLocalDef(expr.e_lookup_associated_resolved)) |def_idx| {
                if (self.lambdaFromDef(def_idx)) |lambda_idx| {
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, empty_args, ModuleEnv.nodeIdxFrom(expr_idx), .{ .apply_called_value = expr_idx });
                } else {
                    self.noteInvocation(current);
                }
            }
        } else if (tag == .e_lookup_external or tag == .e_hosted_lambda) {
            // Imports are acyclic: another module's function cannot demand
            // this module's values.
        } else {
            self.noteInvocation(current);
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

    fn visitStmt(self: *DemandAnalyzer, walk: *Walk, current: *DemandSummary, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
        switch (self.cir.store.getStatement(stmt_idx)) {
            .s_decl => |decl| {
                // Register the callable only after its RHS subtree walked.
                try walk.push(self.allocator, .{ .remember_local = .{ .pattern = decl.pattern, .expr = decl.expr } });
                try walk.push(self.allocator, .{ .visit_pattern = decl.pattern });
                try walk.push(self.allocator, .{ .visit = decl.expr });
            },
            .s_var => |var_stmt| {
                try walk.push(self.allocator, .{ .remember_local = .{ .pattern = var_stmt.pattern_idx, .expr = var_stmt.expr } });
                try walk.push(self.allocator, .{ .visit_pattern = var_stmt.pattern_idx });
                try walk.push(self.allocator, .{ .visit = var_stmt.expr });
            },
            .s_var_uninitialized => {},
            .s_reassign => |reassign| {
                try walk.push(self.allocator, .{ .visit_pattern = reassign.pattern_idx });
                try walk.push(self.allocator, .{ .visit = reassign.expr });
            },
            .s_dbg => |dbg| try walk.push(self.allocator, .{ .visit = dbg.expr }),
            .s_expr => |expr_stmt| try walk.push(self.allocator, .{ .visit = expr_stmt.expr }),
            .s_expect => |expect| try walk.push(self.allocator, .{ .visit = expect.body }),
            .s_for => |for_stmt| {
                try self.applyForLoopDispatch(walk, current, ModuleEnv.nodeIdxFrom(stmt_idx));
                try walk.push(self.allocator, .{ .visit = for_stmt.body });
                try walk.push(self.allocator, .{ .visit_pattern = for_stmt.patt });
                try walk.push(self.allocator, .{ .visit = for_stmt.expr });
            },
            inline .s_while, .s_infinite_loop, .s_breakable_loop => |while_stmt| {
                try walk.push(self.allocator, .{ .visit = while_stmt.body });
                try walk.push(self.allocator, .{ .visit = while_stmt.cond });
            },
            .s_return => |ret| try walk.push(self.allocator, .{ .visit = ret.expr }),
            .s_runtime_error => |runtime_error| try self.pushExprSpanReversed(walk, runtime_error.evaluated),
            .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_break => {},
        }
    }

    fn visitExpr(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        expr_idx: CIR.Expr.Idx,
        local_callables: *const LocalCallables,
    ) std.mem.Allocator.Error!void {
        try self.visitLiteralNode(walk, current, ModuleEnv.nodeIdxFrom(expr_idx));

        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (local_callables.get(lookup.pattern_idx)) |lambda_idx| {
                    try self.noteFnValue(walk, current, lambda_idx);
                } else if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try self.addGraphDep(current, def_idx);
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| try self.noteFnValue(walk, current, lambda_idx);
                }
            },
            .e_lookup_associated_resolved => |resolved| {
                if (self.associatedLocalDef(resolved)) |def_idx| {
                    try self.addGraphDep(current, def_idx);
                    if (self.lambdaFromDef(def_idx)) |lambda_idx| try self.noteFnValue(walk, current, lambda_idx);
                }
            },
            .e_call => |call| {
                // Operands first, then the target's summary applies—
                // pushed in reverse so pops run func, args, apply.
                try walk.push(self.allocator, .{ .apply_call_target = .{ .func = call.func, .args = call.args } });
                try self.pushExprSpanReversed(walk, call.args);
                try walk.push(self.allocator, .{ .visit = call.func });
            },
            .e_lambda => try self.noteFnValue(walk, current, expr_idx),
            .e_closure => |closure| {
                for (self.cir.store.sliceCaptures(closure.captures)) |capture_idx| {
                    const capture = self.cir.store.getCapture(capture_idx);
                    if (self.pattern_to_def.get(capture.pattern_idx)) |def_idx| {
                        try self.addGraphDep(current, def_idx);
                    }
                }
                try self.noteFnValue(walk, current, closure.lambda_idx);
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
                    const branch_patterns = self.cir.store.sliceMatchBranchPatterns(branch.patterns);
                    var pattern_i = branch_patterns.len;
                    while (pattern_i > 0) {
                        pattern_i -= 1;
                        const branch_pattern = self.cir.store.getMatchBranchPattern(branch_patterns[pattern_i]);
                        try walk.push(self.allocator, .{ .visit_pattern = branch_pattern.pattern });
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
                try self.applyDispatchRequirement(walk, current, call.constraint_fn_var, null);
                try self.pushExprSpanReversed(walk, call.args);
                try walk.push(self.allocator, .{ .visit = call.receiver });
            },
            .e_interpolation => |interpolation| {
                if (interpolation.constraint_fn_var) |fn_var| try self.applyDispatchRequirement(walk, current, fn_var, null);
                if (interpolation.assembler_fn_var) |fn_var| try self.applyDispatchRequirement(walk, current, fn_var, null);
                try walk.push(self.allocator, .{ .visit = interpolation.first });
                try self.pushExprSpanReversed(walk, interpolation.parts);
            },
            .e_structural_eq => |eq| {
                try self.applyIntroducedDispatches(walk, current, expr_idx);
                try walk.push(self.allocator, .{ .visit = eq.lhs });
                try walk.push(self.allocator, .{ .visit = eq.rhs });
            },
            .e_structural_hash => |h| {
                try self.applyIntroducedDispatches(walk, current, expr_idx);
                try walk.push(self.allocator, .{ .visit = h.value });
                try walk.push(self.allocator, .{ .visit = h.hasher });
            },
            .e_method_eq => |eq| {
                try self.applyDispatchRequirement(walk, current, eq.constraint_fn_var, null);
                try walk.push(self.allocator, .{ .visit = eq.lhs });
                try walk.push(self.allocator, .{ .visit = eq.rhs });
            },
            .e_type_method_call => |call| {
                // Method bindings are final once checking has run.
                if (self.foreign != null) {
                    if (self.cir.lookupMethodBindingForOwnerConst(call.type_dispatch_stmt, call.method_name)) |binding| {
                        try self.applyMethodBinding(walk, current, binding.def_idx);
                    }
                }
                try self.pushExprSpanReversed(walk, call.args);
            },
            .e_type_dispatch_call => |call| {
                try self.applyDispatchRequirement(walk, current, call.constraint_fn_var, null);
                try self.pushExprSpanReversed(walk, call.args);
            },
            .e_tuple_access => |tuple_access| try walk.push(self.allocator, .{ .visit = tuple_access.tuple }),
            .e_tuple => |tuple| try self.pushExprSpanReversed(walk, tuple.elems),
            .e_binop => |binop| {
                try walk.push(self.allocator, .{ .visit = binop.lhs });
                try walk.push(self.allocator, .{ .visit = binop.rhs });
            },
            .e_unary_minus => |unop| try walk.push(self.allocator, .{ .visit = unop.expr }),
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
            .e_nominal => |nominal| {
                // A local construction that omits declared defaulted fields
                // materializes those defaults here, so the omitted defaults'
                // expressions contribute this walk's demands: explicit
                // omission edges, mirroring DefaultCycles' cycle graph via
                // the shared enumeration (design.md "Defaulted Fields").
                // DefaultCycles runs before any demand graph is built and
                // drops every name-resolvable cyclic default, so the
                // omission relation walked here is acyclic and the walk
                // terminates; `walk.seen_defaults` dedups the pushes so
                // shared defaults don't re-walk with product growth.
                var omissions = default_omissions.omittedDefaults(self.cir, nominal.nominal_type_decl, nominal.backing_expr);
                while (omissions.next()) |omitted| {
                    const entry = try walk.seen_defaults.getOrPut(self.allocator, omitted.default_expr);
                    if (entry.found_existing) continue;
                    try walk.push(self.allocator, .{ .visit = omitted.default_expr });
                }
                try walk.push(self.allocator, .{ .visit = nominal.backing_expr });
            },
            .e_run_low_level => |run_ll| try self.pushExprSpanReversed(walk, run_ll.args),
            // A foreign construction's omitted defaults live in the foreign
            // module and are that module's own compile-time roots; only the
            // supplied values contribute local demands.
            .e_nominal_external => |nominal| try walk.push(self.allocator, .{ .visit = nominal.backing_expr }),
            .e_deferred_import_ref => |deferred| {
                if (deferred.backing) |backing| try walk.push(self.allocator, .{ .visit = backing.expr });
            },
            .e_dbg => |dbg| try walk.push(self.allocator, .{ .visit = dbg.expr }),
            .e_expect_err => |expect_err| try walk.push(self.allocator, .{ .visit = expect_err.expr }),
            .e_expect => |expect| try walk.push(self.allocator, .{ .visit = expect.body }),
            .e_return => |ret| try walk.push(self.allocator, .{ .visit = ret.expr }),
            .e_break => {},
            .e_for => |for_expr| {
                try self.applyForLoopDispatch(walk, current, ModuleEnv.nodeIdxFrom(expr_idx));
                try walk.push(self.allocator, .{ .visit = for_expr.body });
                try walk.push(self.allocator, .{ .visit_pattern = for_expr.patt });
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
            .e_derived_method,
            .e_hosted_lambda,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_required,
            .e_crash,
            => {},
            .e_runtime_error => |runtime_error| try self.pushExprSpanReversed(walk, runtime_error.evaluated),
        }
    }

    fn pushPatternSpanReversed(self: *DemandAnalyzer, walk: *Walk, span: CIR.Pattern.Span) std.mem.Allocator.Error!void {
        const patterns = self.cir.store.slicePatterns(span);
        var i = patterns.len;
        while (i > 0) {
            i -= 1;
            try walk.push(self.allocator, .{ .visit_pattern = patterns[i] });
        }
    }

    fn visitPattern(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        pattern_idx: CIR.Pattern.Idx,
    ) std.mem.Allocator.Error!void {
        try self.visitLiteralNode(walk, current, ModuleEnv.nodeIdxFrom(pattern_idx));

        switch (self.cir.store.getPattern(pattern_idx)) {
            .as => |as_pattern| try walk.push(self.allocator, .{ .visit_pattern = as_pattern.pattern }),
            .applied_tag => |tag| try self.pushPatternSpanReversed(walk, tag.args),
            .nominal => |nominal| try walk.push(self.allocator, .{ .visit_pattern = nominal.backing_pattern }),
            .nominal_external => |nominal| try walk.push(self.allocator, .{ .visit_pattern = nominal.backing_pattern }),
            .deferred_import_ref => |deferred| try walk.push(self.allocator, .{ .visit_pattern = deferred.backing_pattern }),
            .record_destructure => |record| {
                const destructs = self.cir.store.sliceRecordDestructs(record.destructs);
                var i = destructs.len;
                while (i > 0) {
                    i -= 1;
                    const destruct = self.cir.store.getRecordDestruct(destructs[i]);
                    try walk.push(self.allocator, .{ .visit_pattern = destruct.kind.toPatternIdx() });
                }
            },
            .list => |list| {
                if (list.rest_info) |rest| {
                    if (rest.pattern) |rest_pattern| {
                        try walk.push(self.allocator, .{ .visit_pattern = rest_pattern });
                    }
                }
                try self.pushPatternSpanReversed(walk, list.patterns);
            },
            .tuple => |tuple| try self.pushPatternSpanReversed(walk, tuple.patterns),
            .str_interpolation => |str| {
                for (0..str.steps.span.len) |offset| {
                    const step = self.cir.store.getStrPatternStep(str.steps, @intCast(offset));
                    if (step.capture) |capture| {
                        try walk.push(self.allocator, .{ .visit_pattern = capture });
                    }
                }
            },
            .assign,
            .var_assign,
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
        }
    }

    fn visitLiteralNode(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        node_idx: CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        const plan = self.cir.store.literalDispatchPlanForNode(node_idx) orelse return;
        switch (plan.dispatchResolution()) {
            .custom_dispatch, .specialization_dispatch => {},
            .unresolved, .builtin_direct, .checked_error => return,
        }
        try self.applyDispatchRequirement(walk, current, plan.fnVar().?, null);
    }

    fn applyDispatchRequirement(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        source_fn_var: Var,
        demand_node: ?CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        var targets = std.ArrayListUnmanaged(CIR.Def.Idx).empty;
        defer targets.deinit(self.allocator);
        switch (try self.dispatchRequirementTarget(source_fn_var, demand_node, &targets)) {
            .targets => |foreign_invokes| {
                if (foreign_invokes) current.invokes = true;
                for (targets.items) |target_def| {
                    if (self.lambdaFromDef(target_def)) |lambda_idx| {
                        // A selected method's parameters are not resolved
                        // against the dispatch's operands: a called parameter
                        // stays an unresolved call, which invokes whatever it
                        // is given.
                        const no_source_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                        try self.beginApplyLambdaSummary(walk, current, lambda_idx, no_source_args, null, null);
                    }
                }
            },
            .requirement => |fn_var| _ = try current.addDispatchRequirement(self.allocator, fn_var),
        }
    }

    /// Apply every dispatch derived for a structural comparison or hash: the
    /// node itself has no callable, so its components are found through the
    /// expression they serve.
    fn applyIntroducedDispatches(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        expr_idx: CIR.Expr.Idx,
    ) std.mem.Allocator.Error!void {
        const roots = self.dispatch_by_intro.get(expr_idx) orelse return;
        for (roots.items) |root| try self.applyDispatchRequirement(walk, current, root, null);
    }

    const DispatchTarget = union(enum) {
        /// The requirement resolved; its local targets were appended, and
        /// this is whether some foreign target can invoke a function value
        /// it does not name.
        targets: bool,
        /// The dispatch's callable var, still polymorphic here.
        requirement: Var,
    };

    /// Resolve one dispatch requirement at `demand_node` into the local
    /// definitions its selected targets and their derived dispatches name.
    fn dispatchRequirementTarget(
        self: *const DemandAnalyzer,
        source_fn_var: Var,
        demand_node: ?CIR.Node.Idx,
        targets: *std.ArrayListUnmanaged(CIR.Def.Idx),
    ) std.mem.Allocator.Error!DispatchTarget {
        const fn_var = if (demand_node) |node|
            self.translateDispatchRequirement(source_fn_var, node)
        else
            source_fn_var;
        const fn_root = self.cir.types.resolveVar(fn_var).var_;
        const root_resolution = self.resolved_dispatch.get(fn_root) orelse return .{ .requirement = fn_var };
        if (!root_resolution.resolved) return .{ .requirement = fn_var };
        var foreign_invokes = false;
        var pending = std.ArrayListUnmanaged(Var).empty;
        defer pending.deinit(self.allocator);
        var seen = std.AutoHashMapUnmanaged(Var, void).empty;
        defer seen.deinit(self.allocator);
        try pending.append(self.allocator, fn_root);
        while (pending.pop()) |root| {
            if ((try seen.getOrPut(self.allocator, root)).found_existing) continue;
            const resolution = self.resolved_dispatch.get(root) orelse continue;
            if (resolution.foreign_invokes) foreign_invokes = true;
            for (resolution.locals.items) |target_def| {
                for (targets.items) |existing| {
                    if (existing == target_def) break;
                } else {
                    try targets.append(self.allocator, target_def);
                }
            }
            try pending.appendSlice(self.allocator, resolution.children.items);
        }
        return .{ .targets = foreign_invokes };
    }

    fn translateDispatchRequirement(
        self: *const DemandAnalyzer,
        fn_var: Var,
        demand_node: CIR.Node.Idx,
    ) Var {
        const record_index = self.scheme_use_by_node.get(demand_node) orelse return fn_var;
        const record = self.cir.scheme_uses.items.items[record_index];
        const pairs = self.cir.scheme_use_pairs.items.items[record.pairs_start .. record.pairs_start + record.pairs_len];
        const old_root = self.cir.types.resolveVar(fn_var).var_;
        for (pairs) |pair| {
            if (self.cir.types.resolveVar(@fromBackingInt(@intCast(pair.old_var))).var_ == old_root) {
                return @fromBackingInt(@intCast(pair.fresh_var));
            }
        }
        return fn_var;
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

        var stack_allocator_state_buffer: [2048]u8 align(@alignOf(usize)) = undefined;
        var stack_allocator_state = std.heap.BufferFirstAllocator.init(&stack_allocator_state_buffer, self.allocator);
        const stack_allocator = stack_allocator_state.allocator();
        var pending: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer pending.deinit(stack_allocator);

        pending.append(stack_allocator, root) catch return false;
        while (pending.pop()) |current| {
            if (current == needle) return true;
            appendChildPatterns(self.cir, current, &pending, stack_allocator) catch return false;
        }

        return false;
    }
};

/// Append the patterns nested directly inside `pattern_idx`.
fn appendChildPatterns(
    cir: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    out: *std.ArrayList(CIR.Pattern.Idx),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    switch (cir.store.getPattern(pattern_idx)) {
        .as => |as_pattern| try out.append(allocator, as_pattern.pattern),
        .applied_tag => |tag| {
            for (cir.store.slicePatterns(tag.args)) |arg| {
                try out.append(allocator, arg);
            }
        },
        inline .nominal, .nominal_external, .deferred_import_ref => |nominal| try out.append(allocator, nominal.backing_pattern),
        .record_destructure => |record| {
            for (cir.store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                const destruct = cir.store.getRecordDestruct(destruct_idx);
                try out.append(allocator, destruct.kind.toPatternIdx());
            }
        },
        .list => |list| {
            for (cir.store.slicePatterns(list.patterns)) |item| {
                try out.append(allocator, item);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pattern| {
                    try out.append(allocator, rest_pattern);
                }
            }
        },
        .tuple => |tuple| {
            for (cir.store.slicePatterns(tuple.patterns)) |item| {
                try out.append(allocator, item);
            }
        },
        .str_interpolation => |str| {
            for (0..str.steps.span.len) |offset| {
                const step = cir.store.getStrPatternStep(str.steps, @intCast(offset));
                if (step.capture) |capture| {
                    try out.append(allocator, capture);
                }
            }
        },
        .assign,
        .var_assign,
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
    }
}

/// Append every binder pattern (`assign` or `as`) inside `root`, `root`
/// itself included, in a deterministic order.
///
/// The walk is an explicit worklist (zero-recursion policy).
pub fn appendPatternBinders(
    cir: *const ModuleEnv,
    root: CIR.Pattern.Idx,
    out: *std.ArrayList(CIR.Pattern.Idx),
    scratch: *std.ArrayList(CIR.Pattern.Idx),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    scratch.clearRetainingCapacity();
    try scratch.append(allocator, root);
    while (scratch.pop()) |pattern_idx| {
        if (patternBindsName(cir.store.getPattern(pattern_idx))) {
            try out.append(allocator, pattern_idx);
        }
        try appendChildPatterns(cir, pattern_idx, scratch, allocator);
    }
}

/// Whether a pattern node itself binds a name (nested binders aside).
fn patternBindsName(pattern: CIR.Pattern) bool {
    return switch (pattern) {
        .assign, .var_assign, .as => true,
        .applied_tag,
        .nominal,
        .nominal_external,
        .deferred_import_ref,
        .record_destructure,
        .list,
        .tuple,
        .str_interpolation,
        .num_literal,
        .num_from_numeral_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => false,
    };
}

/// Map every name a def's pattern binds to that def. A def whose pattern
/// destructures its right-hand side binds several names, and a reference to
/// any of them is a reference to the def that computes the whole value, so
/// each one must resolve to the def for the name-reference graph to see the
/// dependency (and any cycle) it creates.
fn mapDefBinderPatterns(
    cir: *const ModuleEnv,
    pattern_to_def: *collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    def_idx: CIR.Def.Idx,
    binders: *std.ArrayList(CIR.Pattern.Idx),
    scratch: *std.ArrayList(CIR.Pattern.Idx),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    binders.clearRetainingCapacity();
    try appendPatternBinders(cir, cir.store.getDef(def_idx).pattern, binders, scratch, allocator);
    for (binders.items) |binder| {
        try pattern_to_def.put(binder, def_idx);
    }
}

/// Build canonicalization's initial strict-demand graph for all definitions.
/// It has no knowledge of imported functions or selected dispatch targets;
/// checking replaces it (`buildCheckedDependencyGraph`).
pub fn buildDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!DependencyGraph {
    const defs_slice = cir.store.sliceDefs(all_defs);
    var graph = DependencyGraph.init(allocator, defs_slice);
    errdefer graph.deinit();

    var analyzer = try DemandAnalyzer.init(cir, defs_slice, defs_slice, &.{}, null, allocator);
    defer analyzer.deinit();

    try analyzer.computeSummaries();
    analyzer.walked_value_lambdas.clearRetainingCapacity();

    for (defs_slice) |def_idx| {
        var deps = DemandSummary{};
        defer deps.deinit(allocator);

        try analyzer.collectDefDependencies(def_idx, &deps);
        try analyzer.expandDeps(&deps);

        var dep_iter = deps.deps.keyIterator();
        while (dep_iter.next()) |dep_def_idx| {
            try graph.addEdge(def_idx, dep_def_idx.*);
        }
    }

    return graph;
}

/// Build the strict-demand graph after checking. This is the same demand
/// analysis canonicalization uses, with checker-produced inputs: the selected
/// dispatch targets, and what imported functions can invoke. With them,
/// function values demand their bodies wherever their holder can invoke a
/// function value it does not name (design.md "Function Values In Strict
/// Demand"), and a value that reads another value holds every function value
/// the read one holds.
pub fn buildCheckedDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    resolved_dispatch_targets: []const ResolvedDispatchTarget,
    foreign: ForeignCallees,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!CheckedDemand {
    const defs_slice = cir.store.sliceDefs(all_defs);
    var graph = DependencyGraph.init(allocator, defs_slice);
    errdefer graph.deinit();
    var invoking_defs = std.ArrayList(CIR.Def.Idx).empty;
    errdefer invoking_defs.deinit(allocator);

    var analyzer = try DemandAnalyzer.init(cir, defs_slice, defs_slice, resolved_dispatch_targets, foreign, allocator);
    defer analyzer.deinit();

    try analyzer.computeSummaries();
    analyzer.walked_value_lambdas.clearRetainingCapacity();

    const demands = try allocator.alloc(DemandSummary, defs_slice.len);
    for (demands) |*demand| demand.* = .{};
    defer {
        for (demands) |*demand| demand.deinit(allocator);
        allocator.free(demands);
    }
    var position = std.AutoHashMapUnmanaged(CIR.Def.Idx, u32).empty;
    defer position.deinit(allocator);
    for (defs_slice, demands, 0..) |def_idx, *demand, index| {
        try position.put(allocator, def_idx, @intCast(index));
        try analyzer.collectDefDependencies(def_idx, demand);
        try analyzer.expandDeps(demand);
    }

    // A value read by another value hands it every function value it holds,
    // and a value that can invoke a function value it does not name reaches
    // the bodies of all it holds. Both only grow, so iterate to the fixpoint.
    var reads = std.ArrayList(CIR.Def.Idx).empty;
    defer reads.deinit(allocator);
    var changed = true;
    while (changed) {
        changed = false;
        for (defs_slice, demands) |def_idx, *demand| {
            if (analyzer.lambdaFromDef(def_idx) != null) continue;
            const before = .{ demand.deps.count(), demand.fn_values.count(), demand.invokes };
            reads.clearRetainingCapacity();
            var dep_iter = demand.deps.keyIterator();
            while (dep_iter.next()) |dep| try reads.append(allocator, dep.*);
            for (reads.items) |dep| {
                if (dep == def_idx or analyzer.lambdaFromDef(dep) != null) continue;
                const read = &demands[position.get(dep).?];
                var values = read.fn_values.keyIterator();
                while (values.next()) |value| _ = try demand.addFnValue(allocator, value.*);
            }
            try analyzer.expandDeps(demand);
            const after = .{ demand.deps.count(), demand.fn_values.count(), demand.invokes };
            if (!std.meta.eql(before, after)) changed = true;
        }
    }

    for (defs_slice, demands) |def_idx, *demand| {
        var dep_iter = demand.deps.keyIterator();
        while (dep_iter.next()) |dep_def_idx| {
            try graph.addEdge(def_idx, dep_def_idx.*);
        }
        if (try analyzer.defInvokes(def_idx)) try invoking_defs.append(allocator, def_idx);
    }
    std.mem.sort(CIR.Def.Idx, invoking_defs.items, {}, struct {
        fn lessThan(_: void, a: CIR.Def.Idx, b: CIR.Def.Idx) bool {
            return @backingInt(a) < @backingInt(b);
        }
    }.lessThan);

    return .{ .graph = graph, .invoking_defs = invoking_defs };
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

    const sccs = try state.sccs.toOwnedSlice(allocator);
    errdefer {
        for (sccs) |scc| allocator.free(scc.defs);
        allocator.free(sccs);
    }

    return EvaluationOrder{
        .sccs = sccs,
        .allocator = allocator,
    };
}

/// Collect every top-level def referenced anywhere in `root_expr`'s expression
/// tree—including nested lambda bodies and blocks—into `out` (deduplicated).
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
/// A local nominal construction that omits declared defaulted fields also
/// walks those defaults' expressions (the checker materializes the default at
/// the construction site, so checking this def needs the default's referenced
/// defs checked first). `scratch_seen_defaults` dedups those pushes per call:
/// unlike the tree structure, a default expression is shared across
/// construction sites—and because this walk enters lambda bodies, an
/// un-deduped push could revisit a construction of the same declaration
/// through a lambda-valued default (a shape DefaultCycles deliberately keeps:
/// materializing it only creates the closure).
///
/// The walk is an explicit worklist (zero-recursion policy).
pub fn collectNameReferences(
    cir: *const ModuleEnv,
    pattern_to_def: *const collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    root_expr: CIR.Expr.Idx,
    out: *std.AutoHashMapUnmanaged(CIR.Def.Idx, void),
    scratch_stack: *std.ArrayList(CIR.Expr.Idx),
    scratch_seen_defaults: *std.AutoHashMapUnmanaged(CIR.Expr.Idx, void),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    scratch_stack.clearRetainingCapacity();
    scratch_seen_defaults.clearRetainingCapacity();
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
                switch (call.owner) {
                    .statement => |stmt| if (cir.lookupMethodBindingForOwnerConst(stmt, call.method_name)) |binding| {
                        try out.put(allocator, binding.def_idx, {});
                    },
                    .dispatcher => {},
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
            inline .e_method_call, .e_dispatch_call => |call| {
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
            .e_block => |block| {
                for (cir.store.sliceStatements(block.stmts)) |stmt_idx| {
                    switch (cir.store.getStatement(stmt_idx)) {
                        .s_decl => |decl| try scratch_stack.append(allocator, decl.expr),
                        .s_var => |var_stmt| try scratch_stack.append(allocator, var_stmt.expr),
                        .s_var_uninitialized => {},
                        inline .s_reassign, .s_dbg, .s_expr => |reassign| try scratch_stack.append(allocator, reassign.expr),
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
                        .s_runtime_error => |runtime_error| try scratch_stack.appendSlice(allocator, cir.store.sliceExpr(runtime_error.evaluated)),
                        .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_break => {},
                    }
                }
                try scratch_stack.append(allocator, block.final_expr);
            },
            .e_tag => |tag| {
                for (cir.store.sliceExpr(tag.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_nominal => |nominal| {
                // Omitted defaulted fields materialize at this construction,
                // so their expressions' references are this def's references
                // (shared enumeration with DefaultCycles; see the doc
                // comment for why the pushes dedup).
                var omissions = default_omissions.omittedDefaults(cir, nominal.nominal_type_decl, nominal.backing_expr);
                while (omissions.next()) |omitted| {
                    const entry = try scratch_seen_defaults.getOrPut(allocator, omitted.default_expr);
                    if (entry.found_existing) continue;
                    try scratch_stack.append(allocator, omitted.default_expr);
                }
                try scratch_stack.append(allocator, nominal.backing_expr);
            },
            // Foreign defaults are the foreign module's concern; only the
            // supplied values contribute local references.
            .e_nominal_external => |nominal| try scratch_stack.append(allocator, nominal.backing_expr),
            .e_deferred_import_ref => |deferred| {
                if (deferred.backing) |backing| try scratch_stack.append(allocator, backing.expr);
            },
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
            .e_derived_method,
            .e_hosted_lambda,
            .e_lookup_external,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_associated_resolved,
            .e_lookup_required,
            .e_crash,
            => {},
            .e_runtime_error => |runtime_error| try scratch_stack.appendSlice(allocator, cir.store.sliceExpr(runtime_error.evaluated)),
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
) std.mem.Allocator.Error!CheckOrder {
    const defs_slice = cir.store.sliceDefs(all_defs);

    var pattern_to_def = collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx).init(allocator);
    defer pattern_to_def.deinit();
    // Source position of each def, for the deterministic tie-break.
    var def_position = collections.DenseMap(CIR.Def.Idx, u32).init(allocator);
    defer def_position.deinit();
    var binders: std.ArrayList(CIR.Pattern.Idx) = .empty;
    defer binders.deinit(allocator);
    var binder_scratch: std.ArrayList(CIR.Pattern.Idx) = .empty;
    defer binder_scratch.deinit(allocator);
    for (defs_slice, 0..) |def_idx, position| {
        try mapDefBinderPatterns(cir, &pattern_to_def, def_idx, &binders, &binder_scratch, allocator);
        try def_position.put(def_idx, @intCast(position));
    }

    var graph = DependencyGraph.init(allocator, defs_slice);
    defer graph.deinit();

    var refs: std.AutoHashMapUnmanaged(CIR.Def.Idx, void) = .{};
    defer refs.deinit(allocator);
    var scratch_stack: std.ArrayList(CIR.Expr.Idx) = .empty;
    defer scratch_stack.deinit(allocator);
    var scratch_seen_defaults: std.AutoHashMapUnmanaged(CIR.Expr.Idx, void) = .{};
    defer scratch_seen_defaults.deinit(allocator);

    for (defs_slice) |def_idx| {
        refs.clearRetainingCapacity();
        const def = cir.store.getDef(def_idx);
        try collectNameReferences(cir, &pattern_to_def, def.expr, &refs, &scratch_stack, &scratch_seen_defaults, allocator);
        var ref_iter = refs.keyIterator();
        while (ref_iter.next()) |ref_def_idx| {
            try graph.addEdge(def_idx, ref_def_idx.*);
        }
    }

    var tarjan_order = try computeSCCs(&graph, allocator);
    defer tarjan_order.deinit();

    // Sort members within each group by source position.
    const MemberSort = struct {
        fn lessThan(ctx: *const collections.DenseMap(CIR.Def.Idx, u32), a: CIR.Def.Idx, b: CIR.Def.Idx) bool {
            return ctx.get(a).? < ctx.get(b).?;
        }
    };
    for (tarjan_order.sccs) |scc| {
        std.mem.sort(CIR.Def.Idx, scc.defs, &def_position, MemberSort.lessThan);
    }

    // Deterministic topological order over the condensation: Kahn's algorithm,
    // always emitting the ready group with the earliest first member.
    const group_count = tarjan_order.sccs.len;
    var def_to_group = collections.DenseMap(CIR.Def.Idx, u32).init(allocator);
    defer def_to_group.deinit();
    for (tarjan_order.sccs, 0..) |scc, group_index| {
        for (scc.defs) |def_idx| {
            try def_to_group.put(def_idx, @intCast(group_index));
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

    // Each Tarjan group's position in the emitted order.
    const emitted_index = try allocator.alloc(u32, group_count);
    defer allocator.free(emitted_index);

    while (ready.pop()) |group_index| {
        const scc = tarjan_order.sccs[group_index];
        emitted_index[group_index] = @intCast(ordered_sccs.items.len);
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

    // Each emitted group's direct dependencies, flattened. A successor edge
    // `dep -> dependent` lists `dep` among `dependent`'s dependencies.
    const group_dependency_starts = try allocator.alloc(u32, group_count + 1);
    errdefer allocator.free(group_dependency_starts);
    @memset(group_dependency_starts, 0);
    for (successors) |list| {
        for (list.items) |dependent| {
            group_dependency_starts[emitted_index[dependent] + 1] += 1;
        }
    }
    for (1..group_count + 1) |i| {
        group_dependency_starts[i] += group_dependency_starts[i - 1];
    }
    const group_dependencies = try allocator.alloc(u32, group_dependency_starts[group_count]);
    errdefer allocator.free(group_dependencies);
    const fill = try allocator.dupe(u32, group_dependency_starts[0..group_count]);
    defer allocator.free(fill);
    for (successors, 0..) |list, dependency| {
        for (list.items) |dependent| {
            const dependent_index = emitted_index[dependent];
            group_dependencies[fill[dependent_index]] = emitted_index[dependency];
            fill[dependent_index] += 1;
        }
    }
    for (0..group_count) |group_index| {
        const deps = group_dependencies[group_dependency_starts[group_index]..group_dependency_starts[group_index + 1]];
        std.mem.sort(u32, deps, {}, std.sort.asc(u32));
    }

    const sccs = try ordered_sccs.toOwnedSlice(allocator);
    errdefer {
        for (sccs) |scc| allocator.free(scc.defs);
        allocator.free(sccs);
    }

    return CheckOrder{
        .sccs = sccs,
        .group_dependency_starts = group_dependency_starts,
        .group_dependencies = group_dependencies,
        .allocator = allocator,
    };
}

const TarjanState = struct {
    /// Current DFS index
    index: u32,

    /// Map from node to its DFS index
    indices: collections.DenseMap(CIR.Def.Idx, u32),

    /// Map from node to its lowlink value
    lowlinks: collections.DenseMap(CIR.Def.Idx, u32),

    /// Set of visited nodes
    visited: collections.DenseMap(CIR.Def.Idx, void),

    /// Stack for Tarjan's algorithm
    stack: std.ArrayList(CIR.Def.Idx),

    /// Set of nodes currently on stack
    on_stack: collections.DenseMap(CIR.Def.Idx, void),

    /// Resulting SCCs (in reverse topological order during construction)
    sccs: std.ArrayList(SCC),

    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) TarjanState {
        return .{
            .index = 0,
            .indices = .init(allocator),
            .lowlinks = .init(allocator),
            .visited = .init(allocator),
            .stack = .empty,
            .on_stack = .init(allocator),
            .sccs = .empty,
            .allocator = allocator,
        };
    }

    fn deinit(self: *TarjanState) void {
        self.indices.deinit();
        self.lowlinks.deinit();
        self.visited.deinit();
        self.stack.deinit(self.allocator);
        self.on_stack.deinit();
        // Note: sccs ownership transferred to EvaluationOrder, don't free here
        self.sccs.deinit(self.allocator);
    }

    fn beginNode(self: *TarjanState, v: CIR.Def.Idx) std.mem.Allocator.Error!void {
        try self.indices.put(v, self.index);
        try self.lowlinks.put(v, self.index);
        try self.visited.put(v, {});
        self.index += 1;

        try self.stack.append(self.allocator, v);
        try self.on_stack.put(v, {});
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

            if (@backingInt(w) == @backingInt(v)) break;
        }

        // Check if this SCC is recursive
        const is_recursive = scc_defs.items.len > 1 or blk: {
            // Check for self-loop
            if (scc_defs.items.len == 1) {
                const node = scc_defs.items[0];
                const deps = graph.getDependencies(node);
                for (deps) |dep| {
                    if (@backingInt(dep) == @backingInt(node)) break :blk true;
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

        var stack_allocator_state_buffer: [4096]u8 align(@alignOf(usize)) = undefined;
        var stack_allocator_state = std.heap.BufferFirstAllocator.init(&stack_allocator_state_buffer, self.allocator);
        const stack_allocator = stack_allocator_state.allocator();
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
                    try self.lowlinks.put(top.node, @min(v_lowlink, w_index));
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
                try self.lowlinks.put(parent, @min(parent_lowlink, finished_lowlink));
            }
        }
    }
};
