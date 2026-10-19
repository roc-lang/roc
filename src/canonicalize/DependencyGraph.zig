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
    /// All defs in the graph
    nodes: []const CIR.Def.Idx,
    /// Each def's first position in `nodes`.
    positions: collections.DenseMap(CIR.Def.Idx, u32),
    /// Edges in the order they were added, until `finish` groups them.
    added: std.ArrayList(AddedEdge),
    /// After `finish`, the dependencies of `nodes[i]` are
    /// `dependencies[dependency_starts[i]..dependency_starts[i + 1]]`, in the
    /// order they were added.
    dependency_starts: []u32,
    dependencies: []CIR.Def.Idx,

    allocator: std.mem.Allocator,

    const AddedEdge = struct {
        from_position: u32,
        to: CIR.Def.Idx,
    };

    pub fn init(allocator: std.mem.Allocator, defs: []const CIR.Def.Idx) std.mem.Allocator.Error!DependencyGraph {
        var positions = collections.DenseMap(CIR.Def.Idx, u32).init(allocator);
        errdefer positions.deinit();
        for (defs, 0..) |def_idx, position| {
            const gop = try positions.getOrPut(def_idx);
            if (!gop.found_existing) gop.value_ptr.* = std.math.cast(u32, position) orelse return error.OutOfMemory;
        }
        return DependencyGraph{
            .nodes = defs,
            .positions = positions,
            .added = .empty,
            .dependency_starts = &.{},
            .dependencies = &.{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DependencyGraph) void {
        self.positions.deinit();
        self.added.deinit(self.allocator);
        self.allocator.free(self.dependency_starts);
        self.allocator.free(self.dependencies);
    }

    /// Add an edge: from_def depends on to_def
    pub fn addEdge(self: *DependencyGraph, from_def: CIR.Def.Idx, to_def: CIR.Def.Idx) std.mem.Allocator.Error!void {
        const from_position = self.positions.get(from_def) orelse
            base.invariant("dependency graph edge from a def outside the graph", .{});
        try self.added.append(self.allocator, .{ .from_position = from_position, .to = to_def });
    }

    /// Group the added edges by dependent. Dependencies are read only after this.
    pub fn finish(self: *DependencyGraph) std.mem.Allocator.Error!void {
        const starts = try self.allocator.alloc(u32, self.nodes.len + 1);
        errdefer self.allocator.free(starts);
        const dependencies = try self.allocator.alloc(CIR.Def.Idx, self.added.items.len);
        errdefer self.allocator.free(dependencies);
        if (self.added.items.len > std.math.maxInt(u32)) return error.OutOfMemory;

        @memset(starts, 0);
        for (self.added.items) |edge| starts[edge.from_position + 1] += 1;
        for (1..starts.len) |position| starts[position] += starts[position - 1];
        for (self.added.items) |edge| {
            dependencies[starts[edge.from_position]] = edge.to;
            starts[edge.from_position] += 1;
        }
        // Each start advanced to the next node's; shift them back.
        var position = self.nodes.len;
        while (position > 0) : (position -= 1) starts[position] = starts[position - 1];
        starts[0] = 0;

        self.allocator.free(self.dependency_starts);
        self.allocator.free(self.dependencies);
        self.dependency_starts = starts;
        self.dependencies = dependencies;
        self.added.clearAndFree(self.allocator);
    }

    /// Get dependencies of a def
    pub fn getDependencies(self: *const DependencyGraph, def: CIR.Def.Idx) []const CIR.Def.Idx {
        std.debug.assert(self.added.items.len == 0);
        const position = self.positions.get(def) orelse return &.{};
        if (self.dependency_starts.len == 0) return &.{};
        return self.dependencies[self.dependency_starts[position]..self.dependency_starts[position + 1]];
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

/// One literal constraint whose checker-selected same-module method target is
/// now available to strict-demand analysis. `fn_var` is the exact raw callable
/// variable for this dispatch edge; scheme-use pairs map generalized source
/// constraints to these selected instantiations at their forcing call sites.
pub const ResolvedLiteralTarget = struct {
    fn_var: Var,
    target_def: CIR.Def.Idx,
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
    /// Literal callable vars whose target remains polymorphic in this summary.
    literal_requirements: std.AutoHashMapUnmanaged(Var, void) = .{},

    fn deinit(self: *DemandSummary, allocator: std.mem.Allocator) void {
        self.deps.deinit(allocator);
        self.deps_from.deinit(allocator);
        self.called_patterns.deinit(allocator);
        self.literal_requirements.deinit(allocator);
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

    fn addLiteralRequirement(self: *DemandSummary, allocator: std.mem.Allocator, fn_var: Var) std.mem.Allocator.Error!bool {
        const gop = try self.literal_requirements.getOrPut(allocator, fn_var);
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

        var from_iter = other.deps_from.keyIterator();
        while (from_iter.next()) |lambda_idx| {
            const gop = try self.deps_from.getOrPut(allocator, lambda_idx.*);
            if (!gop.found_existing) changed = true;
        }

        var called_iter = other.called_patterns.keyIterator();
        while (called_iter.next()) |pattern_idx| {
            if (try self.addCalledPattern(allocator, pattern_idx.*)) changed = true;
        }

        var literal_iter = other.literal_requirements.keyIterator();
        while (literal_iter.next()) |fn_var| {
            if (try self.addLiteralRequirement(allocator, fn_var.*)) changed = true;
        }

        return changed;
    }
};

const LocalCallables = collections.DenseMap(CIR.Pattern.Idx, CIR.Expr.Idx);

const DemandAnalyzer = struct {
    cir: *const ModuleEnv,
    allocator: std.mem.Allocator,
    summary_defs: []const CIR.Def.Idx,
    graph_def_set: collections.DenseMap(CIR.Def.Idx, void),
    pattern_to_def: collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    /// The distinct checker-selected targets of each literal callable root,
    /// in first-selected order: `resolved_literal_target_defs[run.start..][0..run.len]`.
    resolved_literal_targets: collections.DenseMap(Var, TargetRun),
    resolved_literal_target_defs: std.ArrayListUnmanaged(CIR.Def.Idx) = .empty,
    scheme_use_by_node: collections.DenseMap(CIR.Node.Idx, u32),
    summaries: collections.DenseMap(CIR.Expr.Idx, DemandSummary),
    active_lambdas: collections.DenseMap(CIR.Expr.Idx, void),
    /// The lambdas of `summary_defs`, whose summaries `computeSummaries`
    /// computes as jobs of their own.
    summary_lambdas: collections.DenseMap(CIR.Expr.Idx, void),
    /// Inline lambda frames not in use by a walk. Frames are boxed so a
    /// summary being folded into stays in place while the fold starts
    /// further lambda walks; a finished frame's box is reused.
    free_frames: std.ArrayListUnmanaged(*DemandSummary) = .empty,
    /// How many frame boxes exist, in use or free.
    frame_count: usize = 0,
    /// Set when a summary job's walk needs a summary lambda's summary that
    /// is not computed yet: that lambda's job runs first, then the walk
    /// resumes.
    needed_summary: ?CIR.Expr.Idx = null,

    fn init(
        cir: *const ModuleEnv,
        summary_defs: []const CIR.Def.Idx,
        graph_defs: []const CIR.Def.Idx,
        resolved_literal_targets: []const ResolvedLiteralTarget,
        allocator: std.mem.Allocator,
    ) std.mem.Allocator.Error!DemandAnalyzer {
        var analyzer = DemandAnalyzer{
            .cir = cir,
            .allocator = allocator,
            .summary_defs = summary_defs,
            .graph_def_set = .init(allocator),
            .pattern_to_def = .init(allocator),
            .resolved_literal_targets = .init(allocator),
            .scheme_use_by_node = .init(allocator),
            .summaries = .init(allocator),
            .active_lambdas = .init(allocator),
            .summary_lambdas = .init(allocator),
        };
        errdefer analyzer.deinit();

        for (graph_defs) |def_idx| {
            try analyzer.graph_def_set.put(def_idx, {});
        }

        var binders: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer binders.deinit(allocator);
        var binder_scratch: std.ArrayList(CIR.Pattern.Idx) = .empty;
        defer binder_scratch.deinit(allocator);
        for (summary_defs) |def_idx| {
            try mapDefBinderPatterns(cir, &analyzer.pattern_to_def, def_idx, &binders, &binder_scratch, allocator);
        }

        try analyzer.indexResolvedLiteralTargets(resolved_literal_targets);

        for (cir.scheme_uses.items.items, 0..) |record, record_index| {
            switch (@as(ModuleEnv.SchemeUseRecord.Slot, @fromBackingInt(@intCast(record.slot_kind)))) {
                .value_use, .shared_value_use => {
                    const entry = try analyzer.scheme_use_by_node.getOrPut(@fromBackingInt(@intCast(record.node_idx)));
                    if (!entry.found_existing) entry.value_ptr.* = @intCast(record_index);
                },
                .nested_function_use, .dispatch_target, .recursive_dispatch_target, .recursive_reference, .recursive_value_reference, .where_method_use => {},
            }
        }

        return analyzer;
    }

    fn deinit(self: *DemandAnalyzer) void {
        var summary_iter = self.summaries.valueIterator();
        while (summary_iter.next()) |summary| {
            summary.deinit(self.allocator);
        }
        self.summaries.deinit();
        self.pattern_to_def.deinit();
        self.graph_def_set.deinit();
        self.resolved_literal_targets.deinit();
        self.resolved_literal_target_defs.deinit(self.allocator);
        self.scheme_use_by_node.deinit();
        self.active_lambdas.deinit();
        self.summary_lambdas.deinit();
        for (self.free_frames.items) |frame| self.allocator.destroy(frame);
        self.free_frames.deinit(self.allocator);
    }

    const TargetRun = struct { start: u32, len: u32 };

    /// Group the checker-selected targets by their callable's root, each
    /// root's distinct targets in the order they were first selected.
    fn indexResolvedLiteralTargets(self: *DemandAnalyzer, targets: []const ResolvedLiteralTarget) std.mem.Allocator.Error!void {
        const Selected = struct { root: Var, def: CIR.Def.Idx, seq: u32 };
        const selected = try self.allocator.alloc(Selected, targets.len);
        defer self.allocator.free(selected);
        for (targets, selected, 0..) |target, *entry, seq| {
            entry.* = .{
                .root = self.cir.types.resolveVar(target.fn_var).var_,
                .def = target.target_def,
                .seq = std.math.cast(u32, seq) orelse return error.OutOfMemory,
            };
        }
        std.sort.pdq(Selected, selected, {}, struct {
            fn lessThan(_: void, a: Selected, b: Selected) bool {
                if (a.root != b.root) return @backingInt(a.root) < @backingInt(b.root);
                return a.seq < b.seq;
            }
        }.lessThan);

        try self.resolved_literal_target_defs.ensureTotalCapacity(self.allocator, selected.len);
        var i: usize = 0;
        while (i < selected.len) {
            const root = selected[i].root;
            const start = self.resolved_literal_target_defs.items.len;
            while (i < selected.len and selected[i].root == root) : (i += 1) {
                const run = self.resolved_literal_target_defs.items[start..];
                if (std.mem.findScalar(CIR.Def.Idx, run, selected[i].def) != null) continue;
                self.resolved_literal_target_defs.appendAssumeCapacity(selected[i].def);
            }
            try self.resolved_literal_targets.put(root, .{
                .start = @intCast(start),
                .len = @intCast(self.resolved_literal_target_defs.items.len - start),
            });
        }
    }

    fn takeFrame(self: *DemandAnalyzer) std.mem.Allocator.Error!*DemandSummary {
        const frame = self.free_frames.pop() orelse frame: {
            // Reserve the slot the frame returns to, so giving it back cannot fail.
            try self.free_frames.ensureTotalCapacity(self.allocator, self.frame_count + 1);
            const fresh = try self.allocator.create(DemandSummary);
            self.frame_count += 1;
            break :frame fresh;
        };
        frame.* = .{};
        return frame;
    }

    /// Release a frame's summary and keep its box for the next frame.
    fn releaseFrame(self: *DemandAnalyzer, frame: *DemandSummary) void {
        frame.deinit(self.allocator);
        self.free_frames.appendAssumeCapacity(frame);
    }

    /// The summary of one lambda being computed: its argument patterns and
    /// then its body walk with the lambda marked active, exactly as an
    /// inline execution frame would.
    const SummaryJob = struct {
        lambda: CIR.Expr.Idx,
        computed: DemandSummary = .{},
        local_callables: LocalCallables,
        walk: Walk,
        /// Argument patterns walked so far; one more once the body walk has
        /// started.
        next_part: usize = 0,

        fn deinit(job: *SummaryJob, analyzer: *DemandAnalyzer) void {
            job.walk.deinit(analyzer);
            job.local_callables.deinit();
            job.computed.deinit(analyzer.allocator);
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
            if (self.cir.store.getExpr(lambda_idx) == .e_lambda) try self.summary_lambdas.put(lambda_idx, {});
        }
        defer self.summary_lambdas.clearRetainingCapacity();

        var jobs = std.ArrayList(SummaryJob).empty;
        defer {
            for (jobs.items) |*job| {
                _ = self.active_lambdas.remove(job.lambda);
                job.deinit(self);
            }
            jobs.deinit(self.allocator);
        }
        var changed = true;
        while (changed) {
            changed = false;
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
                        job.walk.deinit(self);
                        job.walk = .init(self.allocator);
                        if (job.next_part < args.len) {
                            try job.walk.push(self.allocator, .{ .visit_pattern = args[job.next_part] });
                        } else {
                            try job.walk.push(self.allocator, .{ .visit = self.cir.store.getExpr(job.lambda).e_lambda.body });
                        }
                        job.next_part += 1;
                        continue;
                    }
                    var finished = jobs.pop().?;
                    defer finished.deinit(self);
                    _ = self.active_lambdas.remove(finished.lambda);
                    if (try self.mergeSummary(finished.lambda, &finished.computed)) changed = true;
                }
            }
        }
    }

    fn startSummaryJob(self: *DemandAnalyzer, jobs: *std.ArrayList(SummaryJob), lambda_idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
        try jobs.ensureUnusedCapacity(self.allocator, 1);
        try self.active_lambdas.put(lambda_idx, {});
        jobs.appendAssumeCapacity(.{
            .lambda = lambda_idx,
            .local_callables = .init(self.allocator),
            .walk = .init(self.allocator),
        });
    }

    /// Merge a computed summary into the lambda's stored summary; true when
    /// the stored summary grew or was created.
    fn mergeSummary(self: *DemandAnalyzer, lambda_idx: CIR.Expr.Idx, computed: *const DemandSummary) std.mem.Allocator.Error!bool {
        const gop = try self.summaries.getOrPut(lambda_idx);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        return try gop.value_ptr.mergeFrom(self.allocator, computed);
    }

    fn collectDefDependencies(self: *DemandAnalyzer, def_idx: CIR.Def.Idx, out: *DemandSummary) std.mem.Allocator.Error!void {
        var local_callables = LocalCallables.init(self.allocator);
        defer local_callables.deinit();

        const def = self.cir.store.getDef(def_idx);
        try self.walkDemand(def.expr, out, &local_callables);
        try self.walkPatternDemand(def.pattern, out, &local_callables);
    }

    /// Every graph dep a def's demand reaches: its own, and each summarized
    /// lambda's reached through `deps_from`, visiting each lambda once.
    fn expandDeps(self: *DemandAnalyzer, demand: *DemandSummary) std.mem.Allocator.Error!void {
        var visited = collections.DenseMap(CIR.Expr.Idx, void).init(self.allocator);
        defer visited.deinit();
        var pending = std.ArrayList(CIR.Expr.Idx).empty;
        defer pending.deinit(self.allocator);
        var roots = demand.deps_from.keyIterator();
        while (roots.next()) |lambda_idx| try pending.append(self.allocator, lambda_idx.*);
        while (pending.pop()) |lambda_idx| {
            if ((try visited.getOrPut(lambda_idx)).found_existing) continue;
            const summary = self.summaries.getPtr(lambda_idx) orelse continue;
            var deps = summary.deps.keyIterator();
            while (deps.next()) |def_idx| _ = try demand.addDep(self.allocator, def_idx.*);
            var callees = summary.deps_from.keyIterator();
            while (callees.next()) |callee| try pending.append(self.allocator, callee.*);
        }
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

    fn rememberLocalCallable(
        self: *DemandAnalyzer,
        pattern_idx: CIR.Pattern.Idx,
        expr_idx: CIR.Expr.Idx,
        local_callables: *LocalCallables,
    ) std.mem.Allocator.Error!void {
        if (self.lambdaFromExprWithLocals(expr_idx, local_callables)) |lambda_idx| {
            try local_callables.put(pattern_idx, lambda_idx);
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
    };

    const Walk = struct {
        work: std.ArrayList(WalkItem) = .empty,
        /// Summaries of the lambdas whose bodies are being walked inline,
        /// in boxes from `DemandAnalyzer.takeFrame`.
        frames: std.ArrayList(*DemandSummary) = .empty,
        /// Dedups omitted-default expression pushes per walk, mirroring
        /// `collectNameReferences`'s `scratch_seen_defaults` (see its doc
        /// comment): a default expression is shared across construction
        /// sites, and a chain of defaults that each construct omitting more
        /// defaults would otherwise re-walk with product growth. Demand
        /// contributions are set-based and frames fold into their parents,
        /// so one visit per walk reaches the walk's output.
        seen_defaults: collections.DenseMap(CIR.Expr.Idx, void),

        fn init(allocator: std.mem.Allocator) Walk {
            return .{ .seen_defaults = .init(allocator) };
        }

        fn deinit(walk: *Walk, analyzer: *DemandAnalyzer) void {
            walk.seen_defaults.deinit();
            walk.work.deinit(analyzer.allocator);
            for (walk.frames.items) |frame| analyzer.releaseFrame(frame);
            walk.frames.deinit(analyzer.allocator);
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
        var walk = Walk.init(self.allocator);
        defer walk.deinit(self);
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
                .visit => |expr_idx| try self.visitExpr(walk, current, expr_idx),
                .visit_pattern => |pattern_idx| try self.visitPattern(walk, current, pattern_idx),
                .visit_stmt => |stmt_idx| try self.visitStmt(walk, stmt_idx),
                .remember_local => |bind| try self.rememberLocalCallable(bind.pattern, bind.expr, local_callables),
                .apply_call_target => |call| try self.applyCallTarget(walk, current, call.func, call.args, local_callables),
                .apply_called_value => |expr_idx| try self.applyCalledValue(walk, current, expr_idx, local_callables),
                .finish_lambda => |fin| {
                    const computed = walk.frames.pop().?;
                    defer self.releaseFrame(computed);
                    _ = self.active_lambdas.remove(fin.lambda);
                    const parent: *DemandSummary = if (walk.frames.items.len > 0)
                        walk.frames.items[walk.frames.items.len - 1]
                    else
                        out;
                    try self.foldLambdaSummary(walk, parent, computed, fin.lambda, fin.args, fin.demand_node);
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
        var walk = Walk.init(self.allocator);
        defer walk.deinit(self);
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

        try self.active_lambdas.put(lambda_idx, {});
        try walk.frames.ensureUnusedCapacity(self.allocator, 1);
        walk.frames.appendAssumeCapacity(try self.takeFrame());
        // The finish item pops first-in-last-out: argument patterns and the
        // body run inside the frame, then the frame folds into its parent.
        try walk.push(self.allocator, .{ .finish_lambda = .{ .lambda = lambda_idx, .args = call_args, .demand_node = demand_node } });
        try walk.push(self.allocator, .{ .visit = expr.e_lambda.body });
        try self.pushPatternSpanReversed(walk, expr.e_lambda.args);
    }

    /// A folded summary whose literal requirements are still being applied.
    const FoldCursor = struct {
        literals: std.AutoHashMapUnmanaged(Var, void).KeyIterator,
        demand_node: ?CIR.Node.Idx,
        /// Target definitions of the literal requirement being applied.
        targets: []const CIR.Def.Idx = &.{},
        next_target: usize = 0,
    };

    /// Fold one lambda's demand summary into `into` for a call with
    /// `call_args`: graph deps transfer directly; called patterns flow through
    /// the matching argument; and polymorphic literal requirements translate
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
        defer cursors.deinit(self.allocator);
        try self.foldSummaryEdges(walk, into, summary, lambda_idx, call_args);
        try cursors.append(self.allocator, .{ .literals = summary.literal_requirements.keyIterator(), .demand_node = demand_node });
        while (cursors.items.len > 0) {
            const cursor = &cursors.items[cursors.items.len - 1];
            if (cursor.next_target < cursor.targets.len) {
                const target_def = cursor.targets[cursor.next_target];
                cursor.next_target += 1;
                const target_lambda = self.lambdaFromDef(target_def) orelse continue;
                // Literal conversion arguments contain no callable value
                // supplied by source, so only each selected method body's
                // explicit top-level demands can flow outward.
                const no_source_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                if (self.summaries.getPtr(target_lambda)) |target_summary| {
                    try self.foldSummaryEdges(walk, into, target_summary, target_lambda, no_source_args);
                    try cursors.append(self.allocator, .{ .literals = target_summary.literal_requirements.keyIterator(), .demand_node = null });
                } else {
                    try self.beginLambdaWalk(walk, target_lambda, no_source_args, null);
                }
                continue;
            }
            const literal = cursor.literals.next() orelse {
                _ = cursors.pop();
                continue;
            };
            switch (self.literalRequirementTarget(literal.*, cursor.demand_node)) {
                .targets => |targets| {
                    cursor.targets = targets;
                    cursor.next_target = 0;
                },
                .requirement => |fn_var| _ = try into.addLiteralRequirement(self.allocator, fn_var),
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
                }
                return;
            }
            _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
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
                }
                return;
            }
            _ = try current.addCalledPattern(self.allocator, lookup.pattern_idx);
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
    ) std.mem.Allocator.Error!void {
        try self.visitLiteralNode(walk, current, ModuleEnv.nodeIdxFrom(expr_idx));

        switch (self.cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (self.pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try self.addGraphDep(current, def_idx);
                }
            },
            .e_call => |call| {
                // Operands first, then the target's summary applies—
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
            inline .e_method_call, .e_dispatch_call => |call| {
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
                    const entry = try walk.seen_defaults.getOrPut(omitted.default_expr);
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
            .e_lookup_associated_resolved,
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
        try self.applyLiteralRequirement(walk, current, plan.fnVar().?, null);
    }

    fn applyLiteralRequirement(
        self: *DemandAnalyzer,
        walk: *Walk,
        current: *DemandSummary,
        source_fn_var: Var,
        demand_node: ?CIR.Node.Idx,
    ) std.mem.Allocator.Error!void {
        switch (self.literalRequirementTarget(source_fn_var, demand_node)) {
            .targets => |targets| for (targets) |target_def| {
                if (self.lambdaFromDef(target_def)) |lambda_idx| {
                    // Literal conversion arguments contain no callable value
                    // supplied by source, so only each selected method body's
                    // explicit top-level demands can flow outward.
                    const no_source_args = CIR.Expr.Span{ .span = base.DataSpan.empty() };
                    try self.beginApplyLambdaSummary(walk, current, lambda_idx, no_source_args, null, null);
                }
            },
            .requirement => |fn_var| _ = try current.addLiteralRequirement(self.allocator, fn_var),
        }
    }

    const LiteralTarget = union(enum) {
        /// The definitions whose methods the literal resolved to.
        targets: []const CIR.Def.Idx,
        /// The literal's callable var, still polymorphic here.
        requirement: Var,
    };

    fn literalRequirementTarget(self: *const DemandAnalyzer, source_fn_var: Var, demand_node: ?CIR.Node.Idx) LiteralTarget {
        const fn_var = if (demand_node) |node|
            self.translateLiteralRequirement(source_fn_var, node)
        else
            source_fn_var;
        const fn_root = self.cir.types.resolveVar(fn_var).var_;
        if (self.resolved_literal_targets.get(fn_root)) |run| return .{ .targets = self.resolved_literal_target_defs.items[run.start..][0..run.len] };
        return .{ .requirement = fn_var };
    }

    fn translateLiteralRequirement(
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

/// Build a dependency graph for all definitions
pub fn buildDependencyGraph(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!DependencyGraph {
    return buildDependencyGraphWithResolvedLiteralTargets(cir, all_defs, &.{}, allocator);
}

/// Build the strict-demand graph after checking, applying the exact local
/// method targets selected for literal conversions. This is the same demand
/// analysis canonicalization uses; the only additional input is explicit
/// checker-produced dispatch data.
pub fn buildDependencyGraphWithResolvedLiteralTargets(
    cir: *const ModuleEnv,
    all_defs: CIR.Def.Span,
    resolved_literal_targets: []const ResolvedLiteralTarget,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!DependencyGraph {
    const defs_slice = cir.store.sliceDefs(all_defs);
    var graph = try DependencyGraph.init(allocator, defs_slice);
    errdefer graph.deinit();

    var analyzer = try DemandAnalyzer.init(cir, defs_slice, defs_slice, resolved_literal_targets, allocator);
    defer analyzer.deinit();

    try analyzer.computeSummaries();

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

    try graph.finish();
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
    out: *collections.DenseMap(CIR.Def.Idx, void),
    scratch_stack: *std.ArrayList(CIR.Expr.Idx),
    scratch_seen_defaults: *collections.DenseMap(CIR.Expr.Idx, void),
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    scratch_stack.clearRetainingCapacity();
    scratch_seen_defaults.clearRetainingCapacity();
    try scratch_stack.append(allocator, root_expr);

    while (scratch_stack.pop()) |expr_idx| {
        switch (cir.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (pattern_to_def.get(lookup.pattern_idx)) |def_idx| {
                    try out.put(def_idx, {});
                }
            },
            .e_type_method_call => |call| {
                if (cir.lookupMethodBindingForOwnerConst(call.type_dispatch_stmt, call.method_name)) |binding| {
                    try out.put(binding.def_idx, {});
                }
                for (cir.store.sliceExpr(call.args)) |arg| try scratch_stack.append(allocator, arg);
            },
            .e_type_dispatch_call => |call| {
                switch (call.owner) {
                    .statement => |stmt| if (cir.lookupMethodBindingForOwnerConst(stmt, call.method_name)) |binding| {
                        try out.put(binding.def_idx, {});
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
                        try out.put(def_idx, {});
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
                    const entry = try scratch_seen_defaults.getOrPut(omitted.default_expr);
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

    var graph = try DependencyGraph.init(allocator, defs_slice);
    defer graph.deinit();

    // The check order depends on the set of references only: groups are
    // emitted by source position and their dependency lists are sorted.
    var refs = collections.DenseMap(CIR.Def.Idx, void).init(allocator);
    defer refs.deinit();
    var scratch_stack: std.ArrayList(CIR.Expr.Idx) = .empty;
    defer scratch_stack.deinit(allocator);
    var scratch_seen_defaults = collections.DenseMap(CIR.Expr.Idx, void).init(allocator);
    defer scratch_seen_defaults.deinit();

    for (defs_slice) |def_idx| {
        refs.clearRetainingCapacity();
        const def = cir.store.getDef(def_idx);
        try collectNameReferences(cir, &pattern_to_def, def.expr, &refs, &scratch_stack, &scratch_seen_defaults, allocator);
        var ref_iter = refs.keyIterator();
        while (ref_iter.next()) |ref_def_idx| {
            try graph.addEdge(def_idx, ref_def_idx.*);
        }
    }
    try graph.finish();

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

    // Cross-group edges (predecessor group, successor group) in discovery order.
    const GroupEdge = struct { from: u32, to: u32 };
    var group_edges: std.ArrayList(GroupEdge) = .empty;
    defer group_edges.deinit(allocator);
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
                try group_edges.append(allocator, .{ .from = dep_group, .to = @intCast(group_index) });
                indegree[group_index] += 1;
            }
        }
    }

    // Each group's successors, in discovery order:
    // `successors[successor_starts[g]..successor_starts[g + 1]]`.
    const successor_starts = try allocator.alloc(u32, group_count + 1);
    defer allocator.free(successor_starts);
    const successors = try allocator.alloc(u32, group_edges.items.len);
    defer allocator.free(successors);
    if (group_edges.items.len > std.math.maxInt(u32)) return error.OutOfMemory;
    @memset(successor_starts, 0);
    for (group_edges.items) |edge| successor_starts[edge.from + 1] += 1;
    for (1..successor_starts.len) |group| successor_starts[group] += successor_starts[group - 1];
    for (group_edges.items) |edge| {
        successors[successor_starts[edge.from]] = edge.to;
        successor_starts[edge.from] += 1;
    }
    // Each start advanced to the next group's; shift them back.
    var start_group = group_count;
    while (start_group > 0) : (start_group -= 1) successor_starts[start_group] = successor_starts[start_group - 1];
    successor_starts[0] = 0;

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
        for (successors[successor_starts[group_index]..successor_starts[group_index + 1]]) |successor| {
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
    for (successors) |dependent| {
        group_dependency_starts[emitted_index[dependent] + 1] += 1;
    }
    for (1..group_count + 1) |i| {
        group_dependency_starts[i] += group_dependency_starts[i - 1];
    }
    const group_dependencies = try allocator.alloc(u32, group_dependency_starts[group_count]);
    errdefer allocator.free(group_dependencies);
    const fill = try allocator.dupe(u32, group_dependency_starts[0..group_count]);
    defer allocator.free(fill);
    for (0..group_count) |dependency| {
        for (successors[successor_starts[dependency]..successor_starts[dependency + 1]]) |dependent| {
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
