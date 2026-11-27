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

    pub fn deinit(self: *Graph, allocator: Allocator) void {
        for (self.deps) |deps| allocator.free(deps);
        allocator.free(self.deps);
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

fn buildGraph(allocator: Allocator, walker: *Walker, roots: []const Root) Allocator.Error!Graph {
    const deps = try allocator.alloc([]const u32, roots.len);
    errdefer allocator.free(deps);
    var built: usize = 0;
    errdefer {
        for (deps[0..built]) |list| allocator.free(list);
    }
    for (roots, deps, 0..) |root, *out, ordinal| {
        out.* = try walker.dependencies(root, @intCast(ordinal));
        built += 1;
    }
    return .{ .deps = deps };
}

/// Direct references decoded once per reached template during this graph build.
/// Ranges share two append-only arrays; no transitive dependency sets are cached.
const DirectDependencies = struct {
    templates_start: u32,
    templates_len: u32,
    reads_start: u32,
    reads_len: u32,
};

/// Walks the checked templates compile-time roots reach and records the
/// roots each evaluation reads, summarizing each template's direct
/// references once (`DirectDependencies`).
pub const Walker = struct {
    allocator: Allocator,
    modules: []const checked.ImportedModuleView,
    owns_module_views: bool = false,
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

    /// A walker over `modules` for `roots`, owning the views it makes of
    /// them.
    pub fn init(allocator: Allocator, modules: []const *const checked.CheckedModuleArtifact, roots: []const Root) Allocator.Error!Walker {
        const views = try allocator.alloc(checked.ImportedModuleView, modules.len);
        errdefer allocator.free(views);
        for (modules, views) |module, *view| view.* = checked.importedView(module);
        var walker = try initViews(allocator, views, roots);
        walker.owns_module_views = true;
        return walker;
    }

    /// A walker over the caller's module views, which must outlive it, for
    /// `roots`.
    pub fn initViews(allocator: Allocator, modules: []const checked.ImportedModuleView, roots: []const Root) Allocator.Error!Walker {
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
            fn count(module: *const checked.ImportedModuleView) usize {
                return module.checked_procedure_templates.templates.items().len;
            }
        }.count);
        errdefer freeStamps(allocator, visited);
        const visited_nodes = try walkStamps(allocator, modules, struct {
            fn count(module: *const checked.ImportedModuleView) usize {
                return module.static_dispatch_plans.evidence_nodes.items().len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_nodes);
        const visited_derivations = try walkStamps(allocator, modules, struct {
            fn count(module: *const checked.ImportedModuleView) usize {
                return module.static_dispatch_plans.generated_codec_derivations.items().len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_derivations);
        return .{ .allocator = allocator, .modules = modules, .by_key = by_key, .root_ordinals = root_ordinals, .visited = visited, .visited_nodes = visited_nodes, .visited_derivations = visited_derivations };
    }

    /// Per module, a zeroed walk stamp per item `count` counts.
    fn walkStamps(allocator: Allocator, modules: []const checked.ImportedModuleView, count: fn (*const checked.ImportedModuleView) usize) Allocator.Error![][]u32 {
        const out = try allocator.alloc([]u32, modules.len);
        var allocated: usize = 0;
        errdefer {
            for (out[0..allocated]) |items| allocator.free(items);
            allocator.free(out);
        }
        for (modules, out) |*module, *items| {
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

    /// Frees what the walker owns.
    pub fn deinit(self: *Walker) void {
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
    pub fn dependencies(self: *Walker, root: Root, ordinal: u32) Allocator.Error![]const u32 {
        self.visit += 1;
        self.reads.clearRetainingCapacity();
        self.pending.clearRetainingCapacity();
        const wrapper = root.request.procedure_template orelse
            base.invariant("compile-time finalization invariant violated: a compile-time root has no checked wrapper template", .{});
        try self.pushTemplate(wrapper);
        if (self.modules[root.module].compile_time_roots.root(root.root).literal_argument) |argument| {
            try self.walkLiteralEdge(root.module, argument.edge);
            if (argument.site.kind == .constant_instance) try self.walkConstantInstance(root.module, argument);
        }
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

    /// The first listed module with checked identity `key`, if listed.
    pub fn moduleOf(self: *const Walker, key: canonical.CheckedModuleDigest) ?u32 {
        return self.by_key.get(key.bytes);
    }

    /// A dispatch to a method bound to a computed callable reads the root
    /// that evaluates the callable.
    /// A polymorphic binding is read as the instance the dispatch's edge
    /// names (design.md "Polymorphic Constants"); its own root is never
    /// evaluated.
    fn readCallableBinding(self: *Walker, binding: static_dispatch.CallableBindingTarget) Allocator.Error!void {
        const module = self.moduleOf(binding.module) orelse return;
        const root = self.modules[module].callable_eval_templates.templates[@backingInt(binding.template)].root;
        if (self.modules[module].entry_wrappers.lookupByRoot(root)) |wrapper| if (self.modules[module].checked_procedure_templates.isPolymorphicConstantWrapper(wrapper.template.template)) return;
        try self.readRoot(module, root);
    }

    /// A polymorphic constant's instance root `argument` of `module`: its
    /// evaluation runs the constant's entry wrapper in the frame of the
    /// scheme its edge instantiates. When that scheme is the constant's own,
    /// the wrapper's body reads the other instances the edge supplies, the
    /// ones it forwards (`mk(empty)` for `wrap = mk(empty)`), so the root
    /// reads every other root the edge names. An instance the edge supplies
    /// to a scheme it is forwarded through reads none of its siblings.
    fn walkConstantInstance(self: *Walker, module: u32, argument: checked.LiteralArgumentRoot) Allocator.Error!void {
        const site_module = self.moduleOf(argument.site.module) orelse return;
        const wrapper = self.rootWrapper(site_module, argument.site.constantRoot()) orelse return;
        try self.pending.append(self.allocator, wrapper);
        const callee = self.edgeCallee(module, argument.edge) orelse return;
        if (callee.module != wrapper.module or callee.template != wrapper.template) return;
        for (self.modules[module].checked_procedure_templates.literalEdgeArgs(argument.edge)) |arg| switch (arg) {
            .root => |id| try self.readRoot(module, id),
            .forwarded, .unbound => {},
        };
    }

    /// The template of a listed module that `ref` names.
    fn templateKey(self: *const Walker, ref: canonical.ProcedureTemplateRef) ?TemplateKey {
        return .{ .module = self.moduleOf(ref.artifact) orelse return null, .template = ref.template };
    }

    /// The entry wrapper of `module`'s compile-time root `root`.
    fn rootWrapper(self: *const Walker, module: u32, root: checked.ComptimeRootId) ?TemplateKey {
        const wrapper = self.modules[module].entry_wrappers.lookupByRoot(root) orelse return null;
        return self.templateKey(wrapper.template);
    }

    /// The template whose scheme literal edge `edge` of `module`
    /// instantiates, when a listed module holds it.
    fn edgeCallee(self: *const Walker, module: u32, edge: checked.LiteralEdge) ?TemplateKey {
        const artifact = self.modules[module];
        return switch (edge) {
            .literal => null,
            .canonical => |template| .{ .module = module, .template = template },
            .requirement => |requirement| .{ .module = module, .template = requirement.template },
            .node => |id| switch (artifact.static_dispatch_plans.evidenceNode(id).target.kind) {
                .procedure => |procedure| self.templateKey(procedure.template),
                .callable_binding => |binding| blk: {
                    const binding_module = self.moduleOf(binding.module) orelse break :blk null;
                    break :blk self.rootWrapper(binding_module, self.modules[binding_module].callable_eval_templates.templates[@backingInt(binding.template)].root);
                },
                .local_proc, .structural => null,
            },
            .use => |expr| blk: {
                const ref_id = artifact.resolved_value_refs.lookupIdByExpr(expr) orelse break :blk null;
                break :blk switch (artifact.resolved_value_refs.ref(ref_id)) {
                    .top_level_const, .imported_const => |const_use| self.constWrapper(const_use),
                    .selected_hoisted_const => |selected| self.constWrapper(selected.const_use),
                    .platform_required_const => |required| self.constWrapper(required.const_use),
                    .top_level_proc, .imported_proc, .promoted_top_level_proc => |proc_use| self.procedureCallee(proc_use),
                    .platform_required_proc => |required| self.procedureCallee(required.procedure),
                    .hosted_proc, .local_param, .local_value, .local_mutable_version, .pattern_binder, .local_proc, .platform_required_declaration, .platform_required_checked_error => null,
                };
            },
        };
    }

    /// The entry wrapper of the constant `const_use` reads.
    fn constWrapper(self: *const Walker, const_use: checked.ConstUseTemplate) ?TemplateKey {
        const module = self.moduleOf(const_use.const_ref.artifact) orelse return null;
        const artifact = self.modules[module];
        const root: checked.ComptimeRootId = switch (const_use.const_ref.owner) {
            .top_level_binding => |top_level| artifact.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse return null,
            .hoisted_root => |hoisted| (artifact.hoisted_constants.lookupByRoot(hoisted.root) orelse return null).root,
        };
        return self.rootWrapper(module, root);
    }

    /// The template a use of a procedure binding instantiates: the
    /// procedure, or a computed callable's entry wrapper.
    fn procedureCallee(self: *const Walker, proc_use: checked.ProcedureUseTemplate) ?TemplateKey {
        const module: u32, const binding: checked.TopLevelProcedureBindingRef = switch (proc_use.binding) {
            .top_level => |top_level| .{ self.moduleOf(top_level.artifact) orelse return null, top_level.binding },
            .imported => |imported| blk: {
                const module = self.moduleOf(imported.artifact) orelse return null;
                const entry = self.modules[module].top_level_values.lookupByDef(imported.def) orelse return null;
                break :blk switch (entry.value) {
                    .procedure_binding => |procedure_binding| .{ module, procedure_binding },
                    .const_ref => |const_ref| return self.constWrapper(.{ .const_ref = const_ref, .requested_source_ty_payload = proc_use.source_fn_ty_payload }),
                };
            },
            .platform_required => |required| .{ self.moduleOf(required.artifact) orelse return null, required.procedure_binding },
            .hosted => return null,
        };
        return switch (self.modules[module].top_level_procedure_bindings.get(binding).body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| self.templateKey(template),
                .lifted, .synthetic => null,
            },
            .callable_eval_template => |template_id| self.rootWrapper(module, self.modules[module].callable_eval_templates.templates[@backingInt(template_id)].root),
            .checked_error => null,
        };
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
            return;
        }
        const templates_start = self.pending.items.len;
        const reads_start = self.reads.items.len;
        self.evidence_visit += 1;
        const module = self.modules[key.module];
        const template = module.checked_procedure_templates.get(key.template);
        const refs = template.resolved_value_refs;
        const end = refs.start + refs.len;
        if (end > module.resolved_value_refs.template_refs.items().len) base.invariant("compile-time finalization invariant violated: a template's value reference span is outside its checked table", .{});
        for (module.resolved_value_refs.template_refs.items()[refs.start..end]) |ref_id| {
            const raw = @backingInt(ref_id);
            if (raw >= module.resolved_value_refs.len()) base.invariant("compile-time finalization invariant violated: a value reference id is outside its checked table", .{});
            const record = module.resolved_value_refs.get(@fromBackingInt(@intCast(raw)));
            try self.walkRef(record.ref);
            if (module.static_dispatch_plans.siteEvidence(record.expr)) |evidence| {
                try self.walkEntries(key.module, evidence);
                try self.drainNodes(key.module);
            }
        }
        const plans = module.static_dispatch_plans;
        const plan_span = template.static_dispatch_plans;
        if (plan_span.start + plan_span.len > plans.template_refs.items().len) base.invariant("compile-time finalization invariant violated: a template's dispatch plan span is outside its checked table", .{});
        for (plans.template_refs.items()[plan_span.start..][0..plan_span.len]) |plan_id| {
            const plan = plans.plans.items()[@backingInt(plan_id)];
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
        };
        try self.summary_templates.appendSlice(self.allocator, self.pending.items[templates_start..]);
        try self.summary_reads.appendSlice(self.allocator, self.reads.items[reads_start..]);
        try self.summaries.put(self.allocator, key, summary);
    }

    /// Walk the evidence nodes queued for `module` (`nodes`) and the
    /// generated codec derivations queued for it (`derivations`): each
    /// procedure target is a template the evaluation reaches, and each
    /// node's nested evidence names further targets.
    pub fn drainNodes(self: *Walker, module: u32) Allocator.Error!void {
        const plans = self.modules[module].static_dispatch_plans;
        while (self.nodes.items.len != 0 or self.derivations.items.len != 0) {
            if (self.derivations.pop()) |id| {
                const stamps_of = self.visited_derivations[module];
                if (stamps_of[@backingInt(id)] == self.evidence_visit) continue;
                stamps_of[@backingInt(id)] = self.evidence_visit;
                const derivation = plans.generated_codec_derivations.items()[@backingInt(id)];
                for (derivation.callsSlice(plans)) |call| switch (call.resolution) {
                    .callable => |node| try self.nodes.append(self.allocator, node),
                    .structural => |nested| try self.derivations.append(self.allocator, nested),
                    // The enclosing definition's evidence supplies the target,
                    // and that evidence is reached from its own sites.
                    .pending, .checked_error, .forwarded => {},
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
            for (plans.evidence_refs.items()[contracts.start..][0..contracts.len]) |contract| try self.walkResolution(contract.resolution);
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
                try self.walkRef(artifact.resolved_value_refs.ref(ref_id));
            },
            .requirement => |requirement| try self.pushTemplate(artifact.checked_procedure_templates.templateRef(artifact.key.digest(), requirement.template)),
            .canonical => |template| try self.pushTemplate(artifact.checked_procedure_templates.templateRef(artifact.key.digest(), template)),
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
            // A polymorphic constant is read as the instance its use's edge
            // names, a root the reader's template demands (design.md
            // "Polymorphic Constants"); its own root is never evaluated.
            .eval_template => |eval| if (artifact.checked_procedure_templates.isPolymorphicConstantWrapper(eval.entry_template.template)) return else try self.pushTemplate(eval.entry_template),
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
            .callable_eval_template => |template_id| {
                const root = artifact.callable_eval_templates.templates[@backingInt(template_id)].root;
                // A polymorphic binding is read as the instance its use's
                // edge names (design.md "Polymorphic Constants").
                if (artifact.entry_wrappers.lookupByRoot(root)) |wrapper| if (artifact.checked_procedure_templates.isPolymorphicConstantWrapper(wrapper.template.template)) return;
                try self.readRoot(module, root);
            },
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
        const ordinal = self.root_ordinals.get(.{ .module = artifact.key.digest().bytes, .root = root }) orelse return;
        try self.reads.append(self.allocator, ordinal);
    }
};

/// Whether a root's value holds a function value anywhere in its type: in
/// a component, a list item, a box payload, a nominal's argument or
/// backing. Such a root has no compile-time value slot, and a `.boxy`
/// program storing it keeps what reading function values takes (design.md
/// "Stored Function Values"). `modules` lists the artifacts `root.module`
/// indexes; `program` is every module of the program the root is evaluated
/// in, which holds each nominal's declaring module. `memo` remembers the
/// answer for each value type across the roots of one program.
pub fn rootHoldsFunction(
    allocator: Allocator,
    memo: *FunctionTypeMemo,
    modules: []const *const checked.CheckedModuleArtifact,
    program: lir.CheckedPipeline.CheckedModuleSet,
    root: Root,
) Allocator.Error!bool {
    const artifact = modules[root.module];
    if (artifact.compile_time_roots.root(root.root).kind == .callable_binding) return true;
    const own_types = artifact.typesView();
    // The root's checked type is its wrapper's function type; its value is
    // the wrapper's result.
    const value_type = switch (own_types.payload(root.request.checked_type)) {
        .function => |function| function.ret,
        .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => base.invariant("compile-time finalization invariant violated: a compile-time root's checked type is not a function type", .{}),
    };
    const key: FunctionTypeMemo.Key = .{ .key = artifact.key.digest().bytes, .ty = value_type };
    const entry = try memo.answers.getOrPut(allocator, key);
    if (!entry.found_existing) {
        errdefer memo.answers.removeByPtr(entry.key_ptr);
        entry.value_ptr.* = try typeHoldsFunction(allocator, program, key.key, &own_types, value_type);
    }
    return entry.value_ptr.*;
}

/// `rootHoldsFunction`'s answers by value type. A module key names one
/// checked type store, which does not change once checked, so an answer
/// holds for every root of a program whose value has that type.
pub const FunctionTypeMemo = struct {
    answers: std.AutoHashMapUnmanaged(Key, bool) = .empty,

    const Key = struct { key: [32]u8, ty: checked.CheckedTypeId };

    pub fn deinit(self: *FunctionTypeMemo, allocator: Allocator) void {
        self.answers.deinit(allocator);
        self.* = undefined;
    }
};

fn typeHoldsFunction(
    allocator: Allocator,
    program: lir.CheckedPipeline.CheckedModuleSet,
    start_key: [32]u8,
    start_types: *const checked.CheckedTypeStoreView,
    start: checked.CheckedTypeId,
) Allocator.Error!bool {
    const Pending = struct { key: [32]u8, types: checked.CheckedTypeStoreView, ty: checked.CheckedTypeId };
    const Seen = struct { key: [32]u8, ty: checked.CheckedTypeId };
    var seen = std.AutoHashMapUnmanaged(Seen, void).empty;
    defer seen.deinit(allocator);
    var pending = std.ArrayList(Pending).empty;
    defer pending.deinit(allocator);
    try pending.append(allocator, .{ .key = start_key, .types = start_types.*, .ty = start });
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
                        .primitive, .bool_tag_union, .try_nominal, .list, .box, .fields, .field => {},
                        .declared_backing => {
                            const declaration = item.types.builtinNominalDeclaration(builtin_nominal) orelse
                                base.invariant("compile-time finalization invariant violated: a module has no declaration for a builtin with a declared backing", .{});
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
