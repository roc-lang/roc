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

const TemplateKey = struct { module: u32, template: canonical.CheckedProcedureTemplateId };

/// Build the graph of `roots` over `modules`, the artifacts the finalization
/// completes. A value a root reads that no listed root produces was completed
/// earlier (an import finalized before this program, or a root this
/// finalization does not evaluate), so it depends on nothing here.
pub fn build(allocator: Allocator, modules: []const *checked.CheckedModuleArtifact, roots: []const Root) Allocator.Error!Graph {
    var walker = try Walker.init(allocator, modules, roots);
    defer walker.deinit();
    const deps = try allocator.alloc([]const u32, roots.len);
    var built: usize = 0;
    errdefer {
        for (deps[0..built]) |list| allocator.free(list);
        allocator.free(deps);
    }
    for (roots, deps, 0..) |root, *out, ordinal| {
        out.* = try walker.dependencies(root, @intCast(ordinal));
        built += 1;
    }
    return .{ .deps = deps };
}

const Walker = struct {
    allocator: Allocator,
    modules: []const *checked.CheckedModuleArtifact,
    /// The first listed module of each checked identity.
    by_key: std.AutoHashMapUnmanaged([32]u8, u32),
    root_ordinals: std.AutoHashMapUnmanaged(RootKey, u32),
    /// Per module, the walk that last reached each of its templates.
    visited: [][]u32,
    /// Per module, the walk that last reached each of its evidence nodes.
    visited_nodes: [][]u32,
    /// Per module, the walk that last reached each of its generated codec
    /// derivations.
    visited_derivations: [][]u32,
    visit: u32 = 0,
    pending: std.ArrayList(TemplateKey) = .empty,
    reads: std.ArrayList(u32) = .empty,
    /// The evidence nodes of one module still to walk (`drainNodes`).
    nodes: std.ArrayList(static_dispatch.EvidenceNodeId) = .empty,
    derivations: std.ArrayList(static_dispatch.GeneratedCodecDerivationId) = .empty,
    /// The root whose dependencies are being collected.
    current: Root = undefined,

    fn init(allocator: Allocator, modules: []const *checked.CheckedModuleArtifact, roots: []const Root) Allocator.Error!Walker {
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
            fn count(module: *const checked.CheckedModuleArtifact) usize {
                return module.checked_procedure_templates.templates.items.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited);
        const visited_nodes = try walkStamps(allocator, modules, struct {
            fn count(module: *const checked.CheckedModuleArtifact) usize {
                return module.static_dispatch_plans.evidence_nodes.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_nodes);
        const visited_derivations = try walkStamps(allocator, modules, struct {
            fn count(module: *const checked.CheckedModuleArtifact) usize {
                return module.static_dispatch_plans.generated_codec_derivations.len;
            }
        }.count);
        errdefer freeStamps(allocator, visited_derivations);
        return .{ .allocator = allocator, .modules = modules, .by_key = by_key, .root_ordinals = root_ordinals, .visited = visited, .visited_nodes = visited_nodes, .visited_derivations = visited_derivations };
    }

    /// Per module, a zeroed walk stamp per item `count` counts.
    fn walkStamps(allocator: Allocator, modules: []const *checked.CheckedModuleArtifact, count: fn (*const checked.CheckedModuleArtifact) usize) Allocator.Error![][]u32 {
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
        freeStamps(self.allocator, self.visited);
        freeStamps(self.allocator, self.visited_nodes);
        freeStamps(self.allocator, self.visited_derivations);
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
        self.current = root;
        self.visit += 1;
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

    fn pushTemplate(self: *Walker, ref: canonical.ProcedureTemplateRef) Allocator.Error!void {
        const module = self.moduleOf(ref.artifact) orelse return;
        try self.pending.append(self.allocator, .{ .module = module, .template = ref.template });
    }

    /// Walk one template, once per root: its value references and the
    /// evidence their sites record, the method targets its dispatch plans
    /// name, and the literal argument roots its edges demand.
    fn walkTemplate(self: *Walker, key: TemplateKey) Allocator.Error!void {
        const stamps = self.visited[key.module];
        const index = @backingInt(key.template);
        if (index >= stamps.len) base.invariant("compile-time finalization invariant violated: a compile-time dependency named an unknown procedure template", .{});
        if (stamps[index] == self.visit) return;
        stamps[index] = self.visit;
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
        const plans = &module.static_dispatch_plans;
        const plan_span = template.static_dispatch_plans;
        if (plan_span.start + plan_span.len > plans.template_refs.len) base.invariant("compile-time finalization invariant violated: a template's dispatch plan span is outside its checked table", .{});
        for (plans.template_refs[plan_span.start..][0..plan_span.len]) |plan_id| {
            const plan = plans.plans[@backingInt(plan_id)];
            switch (plan.resolution) {
                .direct_closed, .direct_parametric => |direct| try self.nodes.append(self.allocator, direct.evidence),
                .structural => if (plan.generated_codec_derivation) |id| try self.derivations.append(self.allocator, id),
                .evidence_dependent, .checked_error, .@"unreachable", .direct_pending => {},
            }
            try self.drainNodes(key.module);
        }
        // The literal argument roots the template's edges read (design.md
        // "Literal Arguments").
        for (module.checked_procedure_templates.templateLiteralArgumentDemand(&template)) |root| try self.readRoot(key.module, root);
    }

    /// Walk the evidence nodes queued for `module` (`nodes`) and the
    /// generated codec derivations queued for it (`derivations`): each
    /// procedure target is a template the evaluation reaches, and each
    /// node's nested evidence names further targets.
    fn drainNodes(self: *Walker, module: u32) Allocator.Error!void {
        const plans = &self.modules[module].static_dispatch_plans;
        while (self.nodes.items.len != 0 or self.derivations.items.len != 0) {
            if (self.derivations.pop()) |id| {
                const stamps_of = self.visited_derivations[module];
                if (stamps_of[@backingInt(id)] == self.visit) continue;
                stamps_of[@backingInt(id)] = self.visit;
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
            if (stamps_of[@backingInt(id)] == self.visit) continue;
            stamps_of[@backingInt(id)] = self.visit;
            const node = plans.evidenceNode(id);
            switch (node.target.kind) {
                .procedure => |procedure| try self.pushTemplate(procedure.template),
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
        const plans = &self.modules[module].static_dispatch_plans;
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
                .local_proc, .structural => {},
            },
            .use => |expr| {
                const raw = @backingInt(expr);
                if (raw >= artifact.resolved_value_refs.by_checked_expr.len) return;
                const ref_id = artifact.resolved_value_refs.by_checked_expr[raw].get() orelse return;
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
            .hoisted_expr => |hoisted| (artifact.hoisted_constants.lookupByExpr(hoisted.expr) orelse return).root,
        };
        try self.readRoot(module, root);
    }

    fn procedureUse(self: *Walker, proc_use: checked.ProcedureUseTemplate) Allocator.Error!void {
        var binding = proc_use.binding;
        while (true) switch (binding) {
            .top_level => |top_level| return try self.topLevelBinding(self.moduleOf(top_level.artifact) orelse return, top_level.binding),
            .imported => |imported| {
                const module = self.moduleOf(imported.artifact) orelse return;
                const entry = self.modules[module].top_level_values.lookupByDef(imported.def) orelse return;
                return switch (entry.value) {
                    .procedure_binding => |procedure_binding| try self.topLevelBinding(module, procedure_binding),
                    .const_ref => |const_ref| try self.constUse(.{ .const_ref = const_ref, .requested_source_ty_template = proc_use.source_fn_ty_template }),
                };
            },
            .hosted => return,
            .platform_required => |required| {
                const module = self.moduleOf(required.artifact) orelse return;
                const required_binding = self.modules[module].platform_required_bindings.lookupByBindingId(@backingInt(required.procedure_binding)) orelse
                    base.invariant("compile-time finalization invariant violated: a platform-required procedure dependency named a missing binding", .{});
                switch (required_binding.value_use) {
                    .procedure_value => |procedure_use| binding = procedure_use.procedure.binding,
                    .const_value => |const_use| return try self.constUse(const_use.const_use),
                }
            },
        };
    }

    fn topLevelBinding(self: *Walker, module: u32, binding: checked.TopLevelProcedureBindingRef) Allocator.Error!void {
        const artifact = self.modules[module];
        switch (artifact.top_level_procedure_bindings.get(binding).body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| try self.pushTemplate(template),
                .lifted, .synthetic => base.invariant("compile-time finalization invariant violated: a checked procedure dependency named a post-check template", .{}),
            },
            .callable_eval_template => |template_id| try self.readRoot(module, artifact.callable_eval_templates.get(template_id).root),
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
/// backing. Such a root is evaluated specialized, which records the
/// evidence a stored function value restores with; a `.boxy` program's
/// function values carry none (design.md "Caching Under `.boxy`").
pub fn rootHoldsFunction(
    allocator: Allocator,
    modules: []const *checked.CheckedModuleArtifact,
    imports: []const checked.ImportedModuleView,
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
    const own_types = artifact.checked_types.view();
    const own_key = artifact.key.digest().bytes;
    // The root's checked type is its wrapper's function type; its value is
    // the wrapper's result.
    switch (own_types.payload(root.request.checked_type)) {
        .function => |function| try pending.append(allocator, .{ .key = own_key, .types = own_types, .ty = function.ret }),
        .pending, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => base.invariant("compile-time finalization invariant violated: a compile-time root's checked type is not a function type", .{}),
    }
    while (pending.pop()) |item| {
        const seen_entry = try seen.getOrPut(allocator, .{ .key = item.key, .ty = item.ty });
        if (seen_entry.found_existing) continue;
        switch (item.types.payload(item.ty)) {
            .function => return true,
            .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
            .alias => |alias| {
                try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = alias.backing });
                for (alias.args) |arg| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = arg });
            },
            .record => |record| {
                for (record.fields) |field| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = field.ty });
                try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = record.ext });
            },
            .tuple => |items| for (items) |elem| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = elem }),
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| for (tag.argsSlice(item.types)) |arg| try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = arg });
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
                        const declaring = typesOf(modules, imports, imported.artifact) orelse
                            base.invariant("compile-time finalization invariant violated: a nominal type's declaring module is not in the program", .{});
                        try pending.append(allocator, .{ .key = imported.artifact.bytes, .types = declaring, .ty = declaring.nominalDeclarationById(imported.declaration).backing });
                    },
                    // A nominal published as a box payload capability is
                    // declared in its module like any other: its value is
                    // its backing's.
                    .local_box_payload_capability => {
                        const declaration = item.types.nominalDeclarationForPayload(nominal) orelse
                            base.invariant("compile-time finalization invariant violated: a box payload capability names a nominal its module does not declare", .{});
                        try pending.append(allocator, .{ .key = item.key, .types = item.types, .ty = declaration.backing });
                    },
                    .imported_box_payload_capability => |imported| {
                        const declaring = typesOf(modules, imports, imported.artifact) orelse
                            base.invariant("compile-time finalization invariant violated: a nominal type's declaring module is not in the program", .{});
                        const declaration = declaring.nominalDeclarationForPayload(nominal) orelse
                            base.invariant("compile-time finalization invariant violated: a box payload capability names a nominal its module does not declare", .{});
                        try pending.append(allocator, .{ .key = imported.artifact.bytes, .types = declaring, .ty = declaration.backing });
                    },
                    .opaque_without_backing => {},
                }
            },
        }
    }
    return false;
}

fn typesOf(modules: []const *checked.CheckedModuleArtifact, imports: []const checked.ImportedModuleView, key: canonical.CheckedModuleDigest) ?checked.CheckedTypeStoreView {
    for (modules) |module| {
        if (module.key.digest().eql(key)) return module.checked_types.view();
    }
    for (imports) |import| {
        if (import.key.digest().eql(key)) return import.checked_types;
    }
    return null;
}
