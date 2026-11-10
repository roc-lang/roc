//! The compile-time producers each compile-time root's evaluation reads,
//! computed from the checked modules alone (design.md "Dependency-Ordered
//! Compile-Time Evaluation"), for an evaluator that lowers no Solved program
//! before it schedules the roots: the `.boxy` compile-time evaluator.
//!
//! A root's evaluation runs its wrapper template and every checked template
//! the wrapper reaches: through its value references, through the method
//! targets its dispatch plans and its uses' evidence name, nested evidence
//! included, and through the bodies of constants holding function values,
//! which a program restores by lowering them inline, in any module of the
//! program. The compile-time values those templates read, and the literal
//! argument roots their edges demand, name the roots the evaluation depends
//! on. A reference inside a function value the evaluation never calls is
//! still walked, so the graph over-approximates what any one evaluation
//! reads, as the Solved program's graph does.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;

/// One compile-time root an evaluation completes: its owning module's
/// position in the finalization's module list, and its checked identity.
pub const Root = struct {
    module: u32,
    root: checked.ComptimeRootId,
    request: checked.RootRequest,
};

/// For each root, the roots its evaluation reads, in ascending order.
pub const Graph = struct {
    deps: []const []const u32,
    /// A reached value outside this batch lacks its Boxy representation.
    /// Visibility does not grant the batch permission to evaluate that value.
    unavailable_boxy_reads: []const bool,

    pub fn deinit(self: *Graph, allocator: Allocator) void {
        for (self.deps) |deps| allocator.free(deps);
        allocator.free(self.deps);
        allocator.free(self.unavailable_boxy_reads);
        self.* = undefined;
    }
};

const RootKey = struct { module: [32]u8, root: checked.ComptimeRootId };

/// A template of one listed module.
pub const TemplateKey = struct { module: u32, template: canonical.CheckedProcedureTemplateId };

/// Where a walk of the templates starts: a checked template, or a listed
/// module's top-level procedure binding.
pub const Start = union(enum) {
    template: canonical.ProcedureTemplateRef,
    binding: struct { module: u32, binding: checked.TopLevelProcedureBindingRef },
};

/// Every template `starts` reach over `modules` through the references a
/// root's evaluation follows (`build`), each once, in no particular order.
/// Like a root's dependencies, the set over-approximates what a run of the
/// program calls: a reference inside a function value the program never
/// calls is still followed.
pub fn reachTemplates(allocator: Allocator, modules: []const *const checked.CheckedModuleArtifact, starts: []const Start) Allocator.Error![]TemplateKey {
    var walker = try Walker.init(allocator, modules, &.{});
    defer walker.deinit();
    walker.visit += 1;
    for (starts) |start| switch (start) {
        .template => |ref| try walker.pushTemplate(ref),
        .binding => |binding| try walker.topLevelBinding(binding.module, binding.binding),
    };
    while (walker.pending.pop()) |next| try walker.walkTemplate(next);
    var out = std.ArrayList(TemplateKey).empty;
    errdefer out.deinit(allocator);
    for (walker.visited, 0..) |stamps, module| for (stamps, 0..) |stamp, index| {
        if (stamp == walker.visit) try out.append(allocator, .{ .module = @intCast(module), .template = @fromBackingInt(@as(u32, @intCast(index))) });
    };
    return try out.toOwnedSlice(allocator);
}

/// Build the graph of `roots` over `modules`, the artifacts the finalization
/// completes. A value a root reads that no listed root produces was completed
/// earlier (an import finalized before this program, or a root this
/// finalization does not evaluate), so it depends on nothing here.
pub fn build(allocator: Allocator, modules: []const *const checked.CheckedModuleArtifact, roots: []const Root) Allocator.Error!Graph {
    var walker = try Walker.init(allocator, modules, roots);
    defer walker.deinit();
    return try buildGraph(allocator, &walker, roots);
}

/// Before Solved evaluation, follow only requested roots' reachable checked
/// references, including imported procedures. A completed LSS value need not
/// have a Boxy representation, so unlisted reads are not assumed Boxy-ready.
pub fn buildForBoxySelection(allocator: Allocator, modules: []const *const checked.CheckedModuleArtifact, program: lir.CheckedPipeline.CheckedModuleSet, roots: []const Root) Allocator.Error!Graph {
    var views = std.ArrayList(checked.ImportedModuleView).empty;
    defer views.deinit(allocator);
    var seen = std.AutoHashMapUnmanaged([32]u8, void).empty;
    defer seen.deinit(allocator);
    // Preserve the completion-list positions that Root.module indexes.
    for (modules) |module| {
        try views.append(allocator, checked.importedView(module));
        try seen.put(allocator, module.key.digest().bytes, {});
    }
    const root_view = checked.importedView(program.root.module);
    for ([_][]const checked.ImportedModuleView{ &.{root_view}, program.root.relation_modules, program.imports }) |group| {
        for (group) |view| {
            if ((try seen.getOrPut(allocator, view.key.digest().bytes)).found_existing) continue;
            try views.append(allocator, view);
        }
    }
    var walker = try Walker.initViews(allocator, views.items, roots);
    defer walker.deinit();
    walker.check_unlisted_readiness = true;
    return try buildGraph(allocator, &walker, roots);
}

fn buildGraph(allocator: Allocator, walker: *Walker, roots: []const Root) Allocator.Error!Graph {
    const deps = try allocator.alloc([]const u32, roots.len);
    errdefer allocator.free(deps);
    const unavailable = try allocator.alloc(bool, roots.len);
    errdefer allocator.free(unavailable);
    var built: usize = 0;
    errdefer {
        for (deps[0..built]) |list| allocator.free(list);
    }
    for (roots, deps, 0..) |root, *out, ordinal| {
        out.* = try walker.dependencies(root, @intCast(ordinal));
        unavailable[ordinal] = walker.unavailable_boxy_read;
        built += 1;
    }
    return .{ .deps = deps, .unavailable_boxy_reads = unavailable };
}

/// Direct references decoded once per reached template during this graph build.
/// Ranges share two append-only arrays; no transitive dependency sets are cached.
const DirectDependencies = struct {
    templates_start: u32,
    templates_len: u32,
    reads_start: u32,
    reads_len: u32,
    unavailable_boxy_read: bool,
};

const Walker = struct {
    allocator: Allocator,
    modules: []const checked.ImportedModuleView,
    owns_module_views: bool = false,
    check_unlisted_readiness: bool = false,
    unavailable_boxy_read: bool = false,
    /// The first listed module of each checked identity.
    by_key: std.AutoHashMapUnmanaged([32]u8, u32),
    root_ordinals: std.AutoHashMapUnmanaged(RootKey, u32),
    /// Per module, the walk that last reached each of its templates.
    visited: [][]u32,
    /// Per module, the summary that last reached each of its evidence nodes.
    visited_nodes: [][]u32,
    /// Per module, the summary that last reached each of its generated codec
    /// derivations.
    visited_derivations: [][]u32,
    visit: u32 = 0,
    /// Evidence stamps belong to one summary, not to a root: two templates
    /// can share evidence, and each summary must include all its references.
    evidence_visit: u32 = 0,
    summaries: std.AutoHashMapUnmanaged(TemplateKey, DirectDependencies) = .empty,
    summary_templates: std.ArrayList(TemplateKey) = .empty,
    summary_reads: std.ArrayList(u32) = .empty,
    pending: std.ArrayList(TemplateKey) = .empty,
    reads: std.ArrayList(u32) = .empty,
    /// The evidence nodes of one module still to walk (`drainNodes`).
    nodes: std.ArrayList(static_dispatch.EvidenceNodeId) = .empty,
    derivations: std.ArrayList(static_dispatch.GeneratedCodecDerivationId) = .empty,

    fn init(allocator: Allocator, modules: []const *const checked.CheckedModuleArtifact, roots: []const Root) Allocator.Error!Walker {
        const views = try allocator.alloc(checked.ImportedModuleView, modules.len);
        errdefer allocator.free(views);
        for (modules, views) |module, *view| view.* = checked.importedView(module);
        var walker = try initViews(allocator, views, roots);
        walker.owns_module_views = true;
        return walker;
    }

    fn initViews(allocator: Allocator, modules: []const checked.ImportedModuleView, roots: []const Root) Allocator.Error!Walker {
        var by_key = std.AutoHashMapUnmanaged([32]u8, u32).empty;
        errdefer by_key.deinit(allocator);
        for (modules, 0..) |module, index| {
            const entry = try by_key.getOrPut(allocator, module.key.digest().bytes);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(index);
        }
        var root_ordinals = std.AutoHashMapUnmanaged(RootKey, u32).empty;
        errdefer root_ordinals.deinit(allocator);
        for (roots, 0..) |root, ordinal| {
            const entry = try root_ordinals.getOrPut(allocator, .{ .module = modules[root.module].key.digest().bytes, .root = root.root });
            if (!entry.found_existing) entry.value_ptr.* = @intCast(ordinal);
        }
        const visited = try walkStamps(allocator, modules, struct {
            fn count(module: checked.ImportedModuleView) usize {
                return module.checked_procedure_templates.templates.items.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited);
        const visited_nodes = try walkStamps(allocator, modules, struct {
            fn count(module: checked.ImportedModuleView) usize {
                return module.static_dispatch_plans.evidence_nodes.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_nodes);
        const visited_derivations = try walkStamps(allocator, modules, struct {
            fn count(module: checked.ImportedModuleView) usize {
                return module.static_dispatch_plans.generated_codec_derivations.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_derivations);
        return .{ .allocator = allocator, .modules = modules, .by_key = by_key, .root_ordinals = root_ordinals, .visited = visited, .visited_nodes = visited_nodes, .visited_derivations = visited_derivations };
    }

    /// Per module, a zeroed walk stamp per item `count` counts.
    fn walkStamps(allocator: Allocator, modules: []const checked.ImportedModuleView, count: fn (checked.ImportedModuleView) usize) Allocator.Error![][]u32 {
        const out = try allocator.alloc([]u32, modules.len);
        var allocated: usize = 0;
        errdefer {
            for (out[0..allocated]) |items| allocator.free(items);
            allocator.free(out);
        }
        for (modules, out) |module, *items| {
            items.* = try allocator.alloc(u32, count(module));
            @memset(items.*, 0);
            allocated += 1;
        }
        return out;
    }

    fn freeStamps(allocator: Allocator, out: [][]u32) void {
        for (out) |items| allocator.free(items);
        allocator.free(out);
    }

    fn deinit(self: *Walker) void {
        if (self.owns_module_views) self.allocator.free(self.modules);
        freeStamps(self.allocator, self.visited);
        freeStamps(self.allocator, self.visited_nodes);
        freeStamps(self.allocator, self.visited_derivations);
        self.summaries.deinit(self.allocator);
        self.summary_templates.deinit(self.allocator);
        self.summary_reads.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.derivations.deinit(self.allocator);
        self.root_ordinals.deinit(self.allocator);
        self.by_key.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.reads.deinit(self.allocator);
    }

    /// The roots `root`'s evaluation reads, in ascending order, without
    /// itself.
    fn dependencies(self: *Walker, root: Root, ordinal: u32) Allocator.Error![]const u32 {
        self.visit += 1;
        self.unavailable_boxy_read = false;
        self.reads.clearRetainingCapacity();
        self.pending.clearRetainingCapacity();
        const wrapper = root.request.procedure_template orelse
            base.invariant("compile-time finalization invariant violated: a compile-time root has no checked wrapper template", .{});
        try self.pushTemplate(wrapper);
        if (self.modules[root.module].compile_time_roots.root(root.root).literal_argument) |argument| try self.walkLiteralEdge(root.module, argument.edge);
        while (self.pending.pop()) |next| try self.walkTemplate(next);
        std.sort.pdq(u32, self.reads.items, {}, std.sort.asc(u32));
        var out = std.ArrayList(u32).empty;
        errdefer out.deinit(self.allocator);
        for (self.reads.items) |read| {
            if (read == ordinal) continue;
            if (out.items.len != 0 and out.items[out.items.len - 1] == read) continue;
            try out.append(self.allocator, read);
        }
        return try out.toOwnedSlice(self.allocator);
    }

    fn moduleOf(self: *const Walker, key: canonical.CheckedModuleDigest) ?u32 {
        return self.by_key.get(key.bytes);
    }

    /// A dispatch to a method bound to a computed callable reads the root
    /// that evaluates the callable.
    fn readCallableBinding(self: *Walker, binding: static_dispatch.CallableBindingTarget) Allocator.Error!void {
        const module = self.moduleOf(binding.module) orelse return;
        try self.readRoot(module, self.modules[module].callable_eval_templates.templates[@backingInt(binding.template)].root);
    }

    fn pushTemplate(self: *Walker, ref: canonical.ProcedureTemplateRef) Allocator.Error!void {
        const module = self.moduleOf(ref.artifact) orelse return;
        try self.pending.append(self.allocator, .{ .module = module, .template = ref.template });
    }

    /// Decode a template's direct references once, then replay that summary
    /// for each root reaching it. Per-root stamps still terminate cycles.
    fn walkTemplate(self: *Walker, key: TemplateKey) Allocator.Error!void {
        const stamps = self.visited[key.module];
        const index = @backingInt(key.template);
        if (index >= stamps.len) base.invariant("compile-time finalization invariant violated: a compile-time dependency named an unknown procedure template", .{});
        if (stamps[index] == self.visit) return;
        stamps[index] = self.visit;
        if (self.summaries.get(key)) |summary| {
            try self.pending.appendSlice(self.allocator, self.summary_templates.items[summary.templates_start..][0..summary.templates_len]);
            try self.reads.appendSlice(self.allocator, self.summary_reads.items[summary.reads_start..][0..summary.reads_len]);
            self.unavailable_boxy_read = self.unavailable_boxy_read or summary.unavailable_boxy_read;
            return;
        }
        const previous_unavailable = self.unavailable_boxy_read;
        self.unavailable_boxy_read = false;
        defer self.unavailable_boxy_read = self.unavailable_boxy_read or previous_unavailable;
        const templates_start = self.pending.items.len;
        const reads_start = self.reads.items.len;
        self.evidence_visit += 1;
        const module = self.modules[key.module];
        const template = module.checked_procedure_templates.get(key.template);
        const refs = template.resolved_value_refs;
        const end = refs.start + refs.len;
        if (end > module.resolved_value_refs.template_refs.len) base.invariant("compile-time finalization invariant violated: a template's value reference span is outside its checked table", .{});
        for (module.resolved_value_refs.template_refs[refs.start..end]) |ref_id| {
            const raw = @backingInt(ref_id);
            if (raw >= module.resolved_value_refs.records.len) base.invariant("compile-time finalization invariant violated: a value reference id is outside its checked table", .{});
            const record = module.resolved_value_refs.records[raw];
            try self.walkRef(record.ref);
            if (module.static_dispatch_plans.siteEvidence(record.expr)) |evidence| {
                try self.walkEntries(key.module, evidence);
                try self.drainNodes(key.module);
            }
        }
        const plans = module.static_dispatch_plans;
        const plan_span = template.static_dispatch_plans;
        if (plan_span.start + plan_span.len > plans.template_refs.len) base.invariant("compile-time finalization invariant violated: a template's dispatch plan span is outside its checked table", .{});
        for (plans.template_refs[plan_span.start..][0..plan_span.len]) |plan_id| {
            const plan = plans.plans[@backingInt(plan_id)];
            switch (plan.resolution) {
                .direct_closed, .direct_parametric => |direct| try self.nodes.append(self.allocator, direct.evidence),
                .structural => if (plan.generated_codec_derivation.get()) |id| try self.derivations.append(self.allocator, id),
                .evidence_dependent, .checked_error, .@"unreachable", .direct_pending => {},
            }
            try self.drainNodes(key.module);
        }
        // The literal argument roots the template's edges read (design.md
        // "Literal Arguments").
        for (module.checked_procedure_templates.templateLiteralArgumentDemand(&template)) |root| try self.readRoot(key.module, root);
        const summary: DirectDependencies = .{
            .templates_start = @intCast(self.summary_templates.items.len),
            .templates_len = @intCast(self.pending.items.len - templates_start),
            .reads_start = @intCast(self.summary_reads.items.len),
            .reads_len = @intCast(self.reads.items.len - reads_start),
            .unavailable_boxy_read = self.unavailable_boxy_read,
        };
        try self.summary_templates.appendSlice(self.allocator, self.pending.items[templates_start..]);
        try self.summary_reads.appendSlice(self.allocator, self.reads.items[reads_start..]);
        try self.summaries.put(self.allocator, key, summary);
    }

    /// Walk the evidence nodes queued for `module` (`nodes`) and the
    /// generated codec derivations queued for it (`derivations`): each
    /// procedure target is a template the evaluation reaches, and each
    /// node's nested evidence names further targets.
    fn drainNodes(self: *Walker, module: u32) Allocator.Error!void {
        const plans = self.modules[module].static_dispatch_plans;
        while (self.nodes.items.len != 0 or self.derivations.items.len != 0) {
            if (self.derivations.pop()) |id| {
                const stamps_of = self.visited_derivations[module];
                if (stamps_of[@backingInt(id)] == self.evidence_visit) continue;
                stamps_of[@backingInt(id)] = self.evidence_visit;
                const derivation = plans.generated_codec_derivations[@backingInt(id)];
                for (derivation.callsSlice(plans)) |call| switch (call.resolution) {
                    .callable => |node| try self.nodes.append(self.allocator, node),
                    .structural => |nested| try self.derivations.append(self.allocator, nested),
                    .pending, .checked_error => {},
                };
                continue;
            }
            const id = self.nodes.pop().?;
            const stamps_of = self.visited_nodes[module];
            if (stamps_of[@backingInt(id)] == self.evidence_visit) continue;
            stamps_of[@backingInt(id)] = self.evidence_visit;
            const node = plans.evidenceNode(id);
            switch (node.target.kind) {
                .procedure => |procedure| try self.pushTemplate(procedure.template),
                .callable_binding => |binding| try self.readCallableBinding(binding),
                .local_proc, .structural => {},
            }
            switch (node.nested) {
                .resolved => try self.walkEntries(module, plans.nestedEvidence(node)),
                .through_alias => try self.walkEntries(module, plans.aliasScopeEvidence(node)),
                .from_callable => {},
            }
        }
    }

    /// Queue the evidence nodes and derivations `evidence` resolves to.
    fn walkEntries(self: *Walker, module: u32, evidence: []const static_dispatch.CheckedEvidence) Allocator.Error!void {
        const plans = self.modules[module].static_dispatch_plans;
        for (evidence) |entry| {
            try self.walkResolution(entry.resolution);
            const contracts = entry.callable_contracts;
            for (plans.evidence_refs[contracts.start..][0..contracts.len]) |contract| try self.walkResolution(contract.resolution);
        }
    }

    fn walkResolution(self: *Walker, resolution: static_dispatch.CheckedEvidence.Resolution) Allocator.Error!void {
        switch (resolution) {
            .direct => |nested| try self.nodes.append(self.allocator, nested),
            .structural => |structural| if (structural.generated_codec_derivation) |id| try self.derivations.append(self.allocator, id),
            .constraint, .from_callable, .from_scheme, .checked_error, .unreachable_value => {},
        }
    }

    /// Walk the scheme a literal argument root's edge instantiates: the
    /// root's body converts the literal in that scheme's frames.
    fn walkLiteralEdge(self: *Walker, module: u32, edge: checked.LiteralEdge) Allocator.Error!void {
        const artifact = self.modules[module];
        switch (edge) {
            .literal => {},
            .node => |id| switch (artifact.static_dispatch_plans.evidenceNode(id).target.kind) {
                .procedure => |procedure| try self.pushTemplate(procedure.template),
                .callable_binding => |binding| try self.readCallableBinding(binding),
                .local_proc, .structural => {},
            },
            .use => |expr| {
                const ref_id = artifact.resolved_value_refs.lookupIdByExpr(expr) orelse return;
                try self.walkRef(artifact.resolved_value_refs.records[@backingInt(ref_id)].ref);
            },
        }
    }

    fn walkRef(self: *Walker, ref: checked.ResolvedValueRef) Allocator.Error!void {
        switch (ref) {
            .top_level_const, .imported_const => |const_use| try self.constUse(const_use),
            .selected_hoisted_const => |selected| try self.constUse(selected.const_use),
            .platform_required_const => |required| try self.constUse(required.const_use),
            .top_level_proc, .imported_proc, .hosted_proc, .promoted_top_level_proc => |proc_use| try self.procedureUse(proc_use),
            .platform_required_proc => |required| try self.procedureUse(required.procedure),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .platform_required_declaration,
            .platform_required_checked_error,
            => {},
        }
    }

    fn constUse(self: *Walker, const_use: checked.ConstUseTemplate) Allocator.Error!void {
        const const_ref = const_use.const_ref;
        const module = self.moduleOf(const_ref.artifact) orelse return;
        const artifact = self.modules[module];
        switch (artifact.const_templates.get(const_ref).state) {
            // A declaration with no implementation evaluates nothing.
            .unimplemented => return,
            // A value holding function values is restored by lowering its
            // body inline, so a reader reads everything that body reads,
            // its literal arguments included.
            .eval_template => |eval| try self.pushTemplate(eval.entry_template),
            .stored_const, .reserved => {},
        }
        const root: checked.ComptimeRootId = switch (const_ref.owner) {
            .top_level_binding => |top_level| artifact.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse return,
            .hoisted_root => |hoisted| (artifact.hoisted_constants.lookupByRoot(hoisted.root) orelse return).root,
        };
        try self.readRoot(module, root);
    }

    fn procedureUse(self: *Walker, proc_use: checked.ProcedureUseTemplate) Allocator.Error!void {
        switch (proc_use.binding) {
            .top_level => |top_level| try self.topLevelBinding(self.moduleOf(top_level.artifact) orelse return, top_level.binding),
            .imported => |imported| {
                const module = self.moduleOf(imported.artifact) orelse return;
                const entry = self.modules[module].top_level_values.lookupByDef(imported.def) orelse return;
                switch (entry.value) {
                    .procedure_binding => |procedure_binding| try self.topLevelBinding(module, procedure_binding),
                    .const_ref => |const_ref| try self.constUse(.{ .const_ref = const_ref, .requested_source_ty_payload = proc_use.source_fn_ty_payload }),
                }
            },
            .hosted => {},
            // The app's top-level procedure binding the requirement is
            // bound to, in the app artifact the reference names: what the
            // lowering calls for the requirement.
            .platform_required => |required| try self.topLevelBinding(self.moduleOf(required.artifact) orelse return, required.procedure_binding),
        }
    }

    fn topLevelBinding(self: *Walker, module: u32, binding: checked.TopLevelProcedureBindingRef) Allocator.Error!void {
        const artifact = self.modules[module];
        switch (artifact.top_level_procedure_bindings.get(binding).body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| try self.pushTemplate(template),
                .lifted, .synthetic => base.invariant("compile-time finalization invariant violated: a checked procedure dependency named a post-check template", .{}),
            },
            .callable_eval_template => |template_id| try self.readRoot(module, artifact.callable_eval_templates.templates[@backingInt(template_id)].root),
            .checked_error => {},
        }
    }

    /// Record a read of `module`'s root `root`, when a listed root produces
    /// it. Every read is a dependency, a reference inside a function value
    /// the evaluation may never call included: the root then runs after the
    /// value completes, or, when the two read each other, in one group with
    /// it.
    fn readRoot(self: *Walker, module: u32, root: checked.ComptimeRootId) Allocator.Error!void {
        const artifact = self.modules[module];
        const ordinal = self.root_ordinals.get(.{ .module = artifact.key.digest().bytes, .root = root }) orelse {
            if (self.check_unlisted_readiness and artifact.compile_time_roots.root(root).needsEvaluation(.boxy)) {
                self.unavailable_boxy_read = true;
            }
            return;
        };
        try self.reads.append(self.allocator, ordinal);
    }
};

/// Whether a root's value holds a function value anywhere in its type: in
/// a component, a list item, a box payload, a nominal's argument or
/// backing. Such a root has no compile-time value slot, and a `.boxy`
/// program storing it keeps what reading function values takes (design.md
/// "Stored Function Values"). `modules` lists the artifacts `root.module`
/// indexes; `program` is every module of the program the root is evaluated
/// in, which holds each nominal's declaring module.
pub fn rootHoldsFunction(
    allocator: Allocator,
    modules: []const *const checked.CheckedModuleArtifact,
    program: lir.CheckedPipeline.CheckedModuleSet,
    root: Root,
) Allocator.Error!bool {
    const artifact = modules[root.module];
    if (artifact.compile_time_roots.root(root.root).kind == .callable_binding) return true;
    const Pending = struct { key: [32]u8, types: checked.CheckedTypeStoreView, ty: checked.CheckedTypeId };
    const Seen = struct { key: [32]u8, ty: checked.CheckedTypeId };
    var seen = std.AutoHashMapUnmanaged(Seen, void).empty;
    defer seen.deinit(allocator);
    var pending = std.ArrayList(Pending).empty;
    defer pending.deinit(allocator);
    const own_types = artifact.typesView();
    const own_key = artifact.key.digest().bytes;
    // The root's checked type is its wrapper's function type; its value is
    // the wrapper's result.
    switch (own_types.payload(root.request.checked_type)) {
        .function => |function| try pending.append(allocator, .{ .key = own_key, .types = own_types, .ty = function.ret }),
        .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => base.invariant("compile-time finalization invariant violated: a compile-time root's checked type is not a function type", .{}),
    }
    while (pending.pop()) |item| {
        const seen_entry = try seen.getOrPut(allocator, .{ .key = item.key, .ty = item.ty });
        if (seen_entry.found_existing) continue;
        switch (item.types.payload(item.ty)) {
            .function => return true,
            .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
            .alias => |alias| {
                try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = alias.backing });
                for (alias.args) |arg| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = arg });
            },
            .record => |record| {
                for (0..record.fields.len()) |field_index| {
                    const field = record.fields.get(field_index);
                    try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = field.ty });
                }
                try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = record.ext });
            },
            .tuple => |items| for (items) |elem| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = elem }),
            .tag_union => |tag_union| {
                for (0..tag_union.tags.len()) |tag_index| {
                    const tag = tag_union.tags.get(tag_index);
                    for (tag.args) |arg| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = arg });
                }
                try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = tag_union.ext });
            },
            .nominal => |nominal| {
                for (nominal.args) |arg| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = arg });
                // The declaration's backing, in the declaring module's own
                // store: its formals are walked as the arguments above.
                switch (nominal.representation) {
                    .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                        // A tag-union parse spec is encoded as a function.
                        .parse_tag_union_spec => return true,
                        .primitive, .bool_tag_union, .try_nominal, .list, .box, .dict, .set, .fields, .field, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => {},
                        .iterator => {
                            const declaration = item.types.builtinNominalDeclaration(builtin_nominal) orelse
                                base.invariant("compile-time finalization invariant violated: a module has no declaration for the builtin iterator", .{});
                            try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = declaration.backing });
                        },
                    },
                    .local_declaration => |declaration_id| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = item.types.nominalDeclarationById(declaration_id).backing }),
                    .imported_declaration => |imported| {
                        const declaring = program.moduleByKey(imported.artifact) orelse
                            base.invariant("compile-time finalization invariant violated: a nominal type's declaring module is not in the program", .{});
                        try pending.append(allocator, .{ .key = imported.artifact.bytes, .types = declaring.checked_types, .ty = declaring.checked_types.nominalDeclarationById(imported.declaration).backing });
                    },
                    // A nominal published as a box payload capability is
                    // declared in its module like any other: its value is
                    // its backing's.
                    .local_box_payload_capability => {
                        const declaration = item.types.nominalDeclarationForPayload(nominal) orelse
                            base.invariant("compile-time finalization invariant violated: a box payload capability names a nominal its module does not declare", .{});
                        try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = declaration.backing });
                    },
                    // The capability names its nominal in the declaring
                    // module's own names.
                    .imported_box_payload_capability => |imported| {
                        const declaring = program.moduleByKey(imported.artifact) orelse
                            base.invariant("compile-time finalization invariant violated: a nominal type's declaring module is not in the program", .{});
                        const declaration = declaring.checked_types.nominalDeclaration(declaring.interface_capabilities.boxPayloadCapability(imported.capability).nominal) orelse
                            base.invariant("compile-time finalization invariant violated: a box payload capability names a nominal its module does not declare", .{});
                        try pending.append(allocator, .{ .key = imported.artifact.bytes, .types = declaring.checked_types, .ty = declaration.backing });
                    },
                    .opaque_without_backing => {},
                }
            },
        }
    }
    return false;
}

test "Boxy selection retains imported representation readiness and shares cyclic summaries" {
    const allocator = std.testing.allocator;
    var parsed = try @import("inspected.zig").parseProgram(allocator, .module,
        \\module [first, second, third]
        \\import Helpers
        \\first = Helpers.read(2.I64)
        \\second = Helpers.read(3.I64)
        \\third = Helpers.clean(4.I64)
    , &.{
        .{ .name = "Helpers", .source =
        \\module [read, clean]
        \\make = |n| |x| x + n
        \\stored = make(9.I64)
        \\read : I64 -> I64
        \\read = |n| if n == 0 stored(0.I64) else again(n - 1)
        \\again : I64 -> I64
        \\again = |n| read(n)
        \\clean : I64 -> I64
        \\clean = |n| n + 1
        },
        .{ .name = "Unused", .source =
        \\module [unused]
        \\unused = |x| x
        },
    }, .{ .io = std.testing.io, .roots = .published_only });
    defer parsed.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
    const consumer = &parsed.checked_artifact;
    const views = try allocator.alloc(checked.ImportedModuleView, parsed.import_artifacts.len + 1);
    defer allocator.free(views);
    views[0] = checked.importedView(consumer);
    for (parsed.import_artifacts, views[1..]) |*artifact, *view| view.* = checked.importedView(artifact);
    var has_lss_only_value = false;
    for (views[1..]) |view| for (view.compile_time_roots.roots) |root| {
        if (!root.needsEvaluation(.lss) and root.needsEvaluation(.boxy)) has_lss_only_value = true;
    };
    try std.testing.expect(has_lss_only_value);
    try std.testing.expectEqual(@as(usize, 3), consumer.root_requests.compile_time_requests.len);
    var roots: [3]Root = undefined;
    for (&roots, consumer.root_requests.compile_time_requests) |*root, request| {
        root.* = .{ .module = 0, .root = request.compile_time_root.?, .request = request };
    }
    var graph = try buildForBoxySelection(allocator, &.{consumer}, .{ .root = checked.loweringView(consumer), .imports = views[1..] }, &roots);
    defer graph.deinit(allocator);
    try std.testing.expectEqualSlices(bool, &.{ true, true, false }, graph.unavailable_boxy_reads);
    // The imported values are visible, not roots this batch owns or schedules.
    for (graph.deps) |deps| try std.testing.expectEqual(@as(usize, 0), deps.len);

    // Construct cold summaries in both orders, not only replay them backwards.
    for ([_]bool{ false, true }) |reverse| {
        var walker = try Walker.initViews(allocator, views, &roots);
        defer walker.deinit();
        walker.check_unlisted_readiness = true;
        for (0..roots.len) |position| {
            const ordinal = if (reverse) roots.len - 1 - position else position;
            const deps = try walker.dependencies(roots[ordinal], @intCast(ordinal));
            allocator.free(deps);
            try std.testing.expectEqual(graph.unavailable_boxy_reads[ordinal], walker.unavailable_boxy_read);
        }
        const decoded = walker.summaries.count();
        const edges = walker.summary_templates.items.len;
        try std.testing.expect(decoded > 0);
        for (0..32) |round| {
            for (0..roots.len) |position| {
                const ordinal = if (round % 2 == 0) position else roots.len - 1 - position;
                const deps = try walker.dependencies(roots[ordinal], @intCast(ordinal));
                allocator.free(deps);
                try std.testing.expectEqual(graph.unavailable_boxy_reads[ordinal], walker.unavailable_boxy_read);
            }
        }
        try std.testing.expectEqual(decoded, walker.summaries.count());
        try std.testing.expectEqual(edges, walker.summary_templates.items.len);
        // Unused is visible but its template bodies are never decoded.
        for (walker.visited[views.len - 1]) |stamp| try std.testing.expectEqual(@as(u32, 0), stamp);
    }

    const FailureCase = struct {
        fn run(a: Allocator, module: *checked.CheckedModuleArtifact, imports: []const checked.ImportedModuleView, requested: []const Root) Allocator.Error!void {
            var candidate = try buildForBoxySelection(a, &.{module}, .{ .root = checked.loweringView(module), .imports = imports }, requested);
            defer candidate.deinit(a);
        }
    };
    try std.testing.checkAllAllocationFailures(allocator, FailureCase.run, .{ consumer, views[1..], @as([]const Root, &roots) });
}

test "CTFE root classification resolves nominal backings throughout the checked program" {
    const allocator = std.testing.allocator;
    for ([_]struct { source: []const u8, holds_function: bool }{
        .{ .source =
        \\Holder := { value : I64 }.{
        \\    make : I64 -> Holder
        \\    make = |n| { value: n }
        \\}
        , .holds_function = false },
        .{ .source =
        \\Holder := { call : I64 -> I64 }.{
        \\    make : I64 -> Holder
        \\    make = |n| { call: |x| x + n }
        \\}
        , .holds_function = true },
    }) |case| {
        var parsed = try @import("inspected.zig").parseProgram(allocator, .module,
            \\module [value]
            \\import Holder
            \\value : Holder
            \\value = Holder.make(7)
        , &.{.{ .name = "Holder", .source = case.source }}, .{ .io = std.testing.io, .roots = .published_only });
        defer parsed.deinit(allocator);
        try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
        const consumer = &parsed.checked_artifact;
        // The import artifacts list the builtin module, then the program's
        // imports in order; `Holder` is the program's only import.
        try std.testing.expectEqual(@as(usize, 2), parsed.import_artifacts.len);
        const holder = &parsed.import_artifacts[1];
        try std.testing.expectEqual(@as(usize, 1), consumer.root_requests.compile_time_requests.len);
        const request = consumer.root_requests.compile_time_requests[0];
        // A nominal can be supplied by the ownership anchor, a platform
        // relation, or an ordinary import. None needs a pending CTFE root.
        const root: Root = .{ .module = 0, .root = request.compile_time_root.?, .request = request };
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &.{consumer}, .{ .root = checked.loweringView(holder), .imports = &.{checked.importedView(consumer)} }, root));
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &.{consumer}, .{ .root = checked.loweringViewWithRelations(consumer, &.{checked.importedView(holder)}) }, root));
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &.{consumer}, .{ .root = checked.loweringView(consumer), .imports = &.{checked.importedView(holder)} }, root));
    }
}

fn expectSharedDependencies(source: []const u8, imports: []const @import("inspected.zig").ModuleSource) (@import("inspected.zig").Error || Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    const allocator = std.testing.allocator;
    var parsed = try @import("inspected.zig").parseProgram(allocator, .module, source, imports, .{ .io = std.testing.io, .roots = .published_only });
    defer parsed.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
    var modules = std.ArrayList(*checked.CheckedModuleArtifact).empty;
    defer modules.deinit(allocator);
    try modules.append(allocator, &parsed.checked_artifact);
    for (parsed.import_artifacts) |*artifact| try modules.append(allocator, artifact);
    var roots = std.ArrayList(Root).empty;
    defer roots.deinit(allocator);
    for (modules.items, 0..) |module, index| {
        for (module.root_requests.compile_time_requests) |request| {
            try roots.append(allocator, .{ .module = @intCast(index), .root = request.compile_time_root.?, .request = request });
        }
    }
    try std.testing.expect(roots.items.len >= 3);
    // Evidence can be shared between templates. Each summary must decode
    // it independently even when both templates belong to one root walk.
    var evidence_probe = try Walker.init(allocator, modules.items, roots.items);
    defer evidence_probe.deinit();
    evidence_probe.visit = 1;
    var probed_evidence = false;
    evidence: for (modules.items, 0..) |module, module_index| {
        for (module.static_dispatch_plans.evidence_nodes, 0..) |node, node_index| {
            if (node.target.kind != .procedure or evidence_probe.moduleOf(node.target.kind.procedure.template.artifact) == null) continue;
            const id: static_dispatch.EvidenceNodeId = @fromBackingInt(@intCast(node_index));
            evidence_probe.evidence_visit = 1;
            try evidence_probe.nodes.append(allocator, id);
            try evidence_probe.drainNodes(@intCast(module_index));
            try std.testing.expect(evidence_probe.pending.items.len > 0);
            const targets = try allocator.dupe(TemplateKey, evidence_probe.pending.items);
            defer allocator.free(targets);
            const reads = try allocator.dupe(u32, evidence_probe.reads.items);
            defer allocator.free(reads);
            evidence_probe.pending.clearRetainingCapacity();
            evidence_probe.reads.clearRetainingCapacity();
            evidence_probe.evidence_visit = 2;
            try evidence_probe.nodes.append(allocator, id);
            try evidence_probe.drainNodes(@intCast(module_index));
            try std.testing.expectEqualSlices(TemplateKey, targets, evidence_probe.pending.items);
            try std.testing.expectEqualSlices(u32, reads, evidence_probe.reads.items);
            probed_evidence = true;
            break :evidence;
        }
    }
    try std.testing.expect(probed_evidence);
    var shared = try Walker.init(allocator, modules.items, roots.items);
    defer shared.deinit();
    var expected = std.ArrayList([]const u32).empty;
    defer {
        for (expected.items) |deps| allocator.free(deps);
        expected.deinit(allocator);
    }
    var total_reads: usize = 0;
    for (roots.items, 0..) |root, ordinal| {
        var isolated = try Walker.init(allocator, modules.items, roots.items);
        defer isolated.deinit();
        const deps = try isolated.dependencies(root, @intCast(ordinal));
        expected.append(allocator, deps) catch |err| {
            allocator.free(deps);
            return err;
        };
        total_reads += deps.len;
        const actual = try shared.dependencies(root, @intCast(ordinal));
        defer allocator.free(actual);
        try std.testing.expectEqualSlices(u32, deps, actual);
        for (actual) |dependency| try std.testing.expect(dependency != ordinal);
    }
    try std.testing.expect(total_reads > 0);
    const decoded = shared.summaries.count();
    const templates_len = shared.summary_templates.items.len;
    const reads_len = shared.summary_reads.items.len;
    try std.testing.expect(decoded > 0);
    // A second traversal in reverse root order must reuse complete summaries,
    // including evidence that another template happened to visit first.
    var ordinal = roots.items.len;
    while (ordinal > 0) {
        ordinal -= 1;
        const actual = try shared.dependencies(roots.items[ordinal], @intCast(ordinal));
        defer allocator.free(actual);
        try std.testing.expectEqualSlices(u32, expected.items[ordinal], actual);
    }
    try std.testing.expectEqual(decoded, shared.summaries.count());
    try std.testing.expectEqual(templates_len, shared.summary_templates.items.len);
    try std.testing.expectEqual(reads_len, shared.summary_reads.items.len);
}

test "checked dependency summaries preserve shared calls, evidence and literal conversions" {
    try expectSharedDependencies(
        \\module [first, second]
        \\base = 37.I64
        \\shared : I64 -> I64
        \\shared = |x| base + x
        \\left = |x| shared(x)
        \\right = |x| shared(x)
        \\first = left(1.I64)
        \\second = right(2.I64)
    , &.{});
}

test "checked dependency summaries terminate mutual recursion and preserve imported reads" {
    try expectSharedDependencies(
        \\module [first, second]
        \\import Helpers
        \\first = Helpers.down(3.I64)
        \\second = Helpers.down(4.I64)
    , &.{.{ .name = "Helpers", .source =
        \\module [down]
        \\base = 37.I64
        \\down : I64 -> I64
        \\down = |n| if n == 0 base else bounce(n - 1)
        \\bounce : I64 -> I64
        \\bounce = |n| down(n)
    }});
}

test "checked dependency summaries preserve function-valued constant reads" {
    try expectSharedDependencies(
        \\module [first, second]
        \\base = 37.I64
        \\add : I64 -> I64
        \\add = |n| base + n
        \\sub : I64 -> I64
        \\sub = |n| base - n
        \\choose : Bool -> (I64 -> I64)
        \\choose = |pick_add| if pick_add add else sub
        \\chosen = choose(Bool.True)
        \\first = chosen(1.I64)
        \\second = chosen(2.I64)
    , &.{});
}
