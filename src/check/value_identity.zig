//! Value identities of a checked module's compile-time roots (design.md
//! "Compile-Time Value Reuse Across Edits").
//!
//! A root's value identity is a digest of every input its evaluation reads,
//! computed from explicit checked data when the module is published. Two
//! roots with equal value identities compute equal values, so a later
//! version of the module reuses a stored value whose identity is unchanged.
//! A root's site names its position within the module independently of node
//! ids, so the roots a reused value read can be found in the later version.
//!
//! A definition identity is the per-definition analogue of a module's content
//! identity: a Merkle digest over the module's name-reference graph, one
//! component of mutually referencing definitions at a time.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const checked_artifact = @import("checked_artifact.zig");
const canonical = @import("canonical_names.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const DependencyGraph = can.DependencyGraph;
const Sha256 = base.Sha256;

const CheckedTypeId = checked_artifact.CheckedTypeId;
const CheckedTypeStoreView = checked_artifact.CheckedTypeStoreView;
const CompileTimeRoot = checked_artifact.CompileTimeRoot;

/// The value identity and site of one compile-time root.
pub const CompileTimeRootIdentity = extern struct {
    /// Digest of everything that decides the root's value.
    value: [32]u8,
    /// The root's position-stable name within its module.
    site: [32]u8,
};

/// A producer-authored report site. Identity names its declaration's bytes and
/// the site's relative span; region is this checked version's reporting location.
pub const TestReportSite = extern struct {
    identity: [32]u8,
    start: u32,
    end: u32,

    /// The site's absolute region in this checked source version.
    pub fn region(self: TestReportSite) base.Region {
        return base.Region.from_raw_offsets(self.start, self.end);
    }
};

/// What the identities of one module's roots are computed from.
pub const Inputs = struct {
    own_key: checked_artifact.CheckedModuleArtifactKey,
    env: *const ModuleEnv,
    names: *const canonical.CanonicalNameStore,
    types: CheckedTypeStoreView,
    roots: []const CompileTimeRoot,
    bodies: *const checked_artifact.CheckedBodyStore,
    top_level_values: *const checked_artifact.TopLevelValueTable,
    /// The static dispatch plans of the module's checked bodies: the methods
    /// a definition's or root's evaluation can select.
    plans: *const static_dispatch.StaticDispatchPlanTable,
    /// The module's index among the modules checking resolved, by which a
    /// dispatch target names its module.
    module_idx: u32,
    /// The checked artifact key's inputs outside the module's own source:
    /// the checking context and the direct imports' keys.
    context: ContextHashes,
    /// The module's own content identity, which names its own declarations.
    own_identity: [32]u8,
    /// Test summaries use published import contexts and exact referenced
    /// implementations instead of whole imported artifacts.
    test_dependencies: bool = false,
    imports: []const checked_artifact.PublishImportArtifact = &.{},
    available: []const checked_artifact.ImportedModuleView = &.{},
    relations: []const checked_artifact.ImportedModuleView = &.{},
};

/// The artifact-key inputs a module's value context hashes.
pub const ContextHashes = struct {
    checking_context: [32]u8,
    direct_import_keys: [32]u8,
};

const domain = "roc.compile_time_value_identity.v2";

/// The definition identity of a definition the module exports, by the
/// digest of its name.
pub const ExportIdentity = extern struct {
    name: [32]u8,
    value: [32]u8,
};

/// Stable declaration provenance, indexed by the current checked source node.
pub const DeclarationIdentity = extern struct {
    statement: CIR.Statement.Idx,
    value: [32]u8,
};

/// A module's value identities: its value context, the identities of its
/// exports, the platform requirements its methods reach, and every root's
/// identity.
pub const DefinitionIdentity = extern struct {
    def: CIR.Def.Idx,
    value: [32]u8,
};

/// The module's value context, dependency summaries, root identities and report sites.
pub const ModuleIdentities = struct {
    /// Digest of everything outside its plain definitions that dispatch on
    /// its types can select or depend on: the module context and the type
    /// world. A per-program literal root's value identity names a module no
    /// other module imports by this digest (design.md "Compile-Time Value
    /// Reuse Across Edits").
    context: [32]u8,
    /// Every exported definition's identity, sorted by name digest. Owned.
    exports: []ExportIdentity,
    /// The digests of the names of the platform requirements code reachable
    /// from the module's methods looks up, sorted. Owned.
    method_requires: [][32]u8,
    /// The same for the module's exported definitions. Owned.
    export_requires: [][32]u8,
    /// Every root's identity, in root order. Owned.
    roots: []CompileTimeRootIdentity,
    definitions: []DefinitionIdentity,
    report_sites: []TestReportSite,
    declarations: []DeclarationIdentity,
    reuse_types: []ReuseTypeIdentity,
};

/// Exact checked type identity for relocating stored nominal occurrences.
pub const ReuseTypeIdentity = extern struct {
    ty: CheckedTypeId,
    identity: [32]u8,
};

/// Hash the checked producer's actual operand, including generated literals.
/// Imported artifact versions and expression numbering are absent.
fn literalIdentity(env: *const ModuleEnv, bodies: checked_artifact.CheckedBodyStoreView, expr: checked_artifact.CheckedExprId) [32]u8 {
    var hash = Sha256.init(.{});
    hashTag(&hash, "roc.literal-operand.v1");
    hashBytes(&hash, env.module_name);
    const literal = bodies.expr(expr);
    hashBytes(&hash, @tagName(literal.data));
    switch (literal.data) {
        .numeral => |number| {
            hashBytes(&hash, env.numeralDigitsBefore(number.literal));
            hashBytes(&hash, env.numeralDigitsAfter(number.literal));
            hashValue(&hash, number.literal.flags);
            hashValue(&hash, number.literal.after_decimal_digit_count);
        },
        .str_from_quote => |quote| hashBytes(&hash, bodies.stringLiteral(quote.literal)),
        .interpolation => |interpolation| {
            hashU32(&hash, @intCast(interpolation.segments.len));
            for (interpolation.segments) |segment| {
                const data = bodies.expr(segment).data;
                hashBytes(&hash, bodies.stringLiteral(data.str_segment));
            }
        },
        .pending, .str_segment, .str, .bytes_literal, .lookup_local, .lookup_external, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .unary_not, .field_access, .dispatch_call, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .row_coerce, .for_, .hosted_lambda, .run_low_level => base.invariant("literal identity producer received a non-literal", .{}),
    }
    return hash.finalResult();
}

/// Compute the module's value context and every root's identity.
pub fn compute(allocator: Allocator, inputs: Inputs) Allocator.Error!ModuleIdentities {
    const identities = try allocator.alloc(CompileTimeRootIdentity, inputs.roots.len);
    errdefer allocator.free(identities);

    var state = try State.init(allocator, inputs);
    defer state.deinit();
    try state.mapExpressionOwners();
    const declarations = try state.declarationIdentities();
    errdefer allocator.free(declarations);
    state.type_walk.declarations = declarations;
    try state.computeDefinitionIdentities();
    const context = state.moduleContext();
    const type_world = try state.typeWorld();
    const value_context = valueContext(context, type_world);
    const exports = try state.exportIdentities();
    errdefer allocator.free(exports);
    const method_requires = try state.methodRequires();
    errdefer allocator.free(method_requires);
    const export_requires = try state.exportRequires();
    errdefer allocator.free(export_requires);

    var definitions = std.ArrayList(DefinitionIdentity).empty;
    errdefer definitions.deinit(allocator);
    if (inputs.test_dependencies) {
        var seen = collections.DenseMap(CIR.Def.Idx, void).init(allocator);
        defer seen.deinit();
        for (inputs.env.store.sliceDefs(inputs.env.all_defs)) |def| {
            try seen.put(def, {});
            try definitions.append(allocator, .{ .def = def, .value = try state.methodIdentity(def) });
        }
        for (inputs.env.store.sliceDefs(inputs.env.exports)) |def| {
            if (seen.contains(def)) continue;
            try seen.put(def, {});
            try definitions.append(allocator, .{ .def = def, .value = try state.methodIdentity(def) });
        }
        for (inputs.env.method_defs.entries.items) |entry| {
            const def = entry.value.def_idx;
            if (seen.contains(def)) continue;
            try seen.put(def, {});
            try definitions.append(allocator, .{ .def = def, .value = try state.methodIdentity(def) });
        }
        // An importer names a value by its definition's node or, for a name
        // a destructure binds, by its binder's node, which shares the
        // definitions' node numbering. Each binder carries the identity of
        // the definition that destructures, rejected ones included.
        var binders = std.ArrayList(CIR.Pattern.Idx).empty;
        defer binders.deinit(allocator);
        var pending = std.ArrayList(CIR.Pattern.Idx).empty;
        defer pending.deinit(allocator);
        for (inputs.env.store.sliceDefs(inputs.env.global_value_defs)) |def| {
            const value = try state.methodIdentity(def);
            if (!seen.contains(def)) {
                try seen.put(def, {});
                try definitions.append(allocator, .{ .def = def, .value = value });
            }
            binders.clearRetainingCapacity();
            try DependencyGraph.appendPatternBinders(inputs.env, inputs.env.store.getDef(def).pattern, &binders, &pending, allocator);
            for (binders.items) |binder| {
                const node: CIR.Def.Idx = @fromBackingInt(@backingInt(binder));
                if (seen.contains(node)) continue;
                try seen.put(node, {});
                try definitions.append(allocator, .{ .def = node, .value = value });
            }
        }
    }
    std.mem.sort(DefinitionIdentity, definitions.items, {}, struct {
        fn lessThan(_: void, a: DefinitionIdentity, b: DefinitionIdentity) bool {
            return @backingInt(a.def) < @backingInt(b.def);
        }
    }.lessThan);
    var ordinals = std.AutoHashMapUnmanaged([32]u8, u32).empty;
    defer ordinals.deinit(allocator);
    for (inputs.roots, identities) |root, *identity| {
        identity.* = try state.rootIdentity(root, context, type_world, &ordinals);
    }
    const report_sites = if (inputs.test_dependencies) try state.reportSites() else try allocator.alloc(TestReportSite, 0);
    errdefer allocator.free(report_sites);
    var reuse_types = std.ArrayList(ReuseTypeIdentity).empty;
    errdefer reuse_types.deinit(allocator);
    if (inputs.test_dependencies) {
        for (0..inputs.types.payloadCount()) |index| {
            const ty: CheckedTypeId = @fromBackingInt(@as(u32, @intCast(index)));
            switch (inputs.types.payload(ty)) {
                .nominal, .alias => {
                    var hash = Sha256.init(.{});
                    hashTag(&hash, "roc.relocatable-type.v1");
                    hash.update(&value_context);
                    hash.update(&try state.type_walk.digest(ty));
                    try reuse_types.append(allocator, .{ .ty = ty, .identity = hash.finalResult() });
                },
                .pending, .err, .flex, .rigid, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => {},
            }
        }
    }
    const owned_reuse_types = try reuse_types.toOwnedSlice(allocator);
    errdefer allocator.free(owned_reuse_types);
    const owned_definitions = try definitions.toOwnedSlice(allocator);
    return .{
        .reuse_types = owned_reuse_types,
        .report_sites = report_sites,
        .declarations = declarations,
        .context = value_context,
        .exports = exports,
        .method_requires = method_requires,
        .export_requires = export_requires,
        .roots = identities,
        .definitions = owned_definitions,
    };
}

fn valueContext(context: [32]u8, type_world: [32]u8) [32]u8 {
    var hasher = Sha256.init(.{});
    hashTag(&hasher, domain);
    hashTag(&hasher, "value-context");
    hasher.update(&context);
    hasher.update(&type_world);
    return hasher.finalResult();
}

/// The digest naming a definition or platform requirement called `name`.
pub fn nameDigest(name: []const u8) [32]u8 {
    return digestBytes("name", name);
}

const Owner = union(enum) {
    def: CIR.Def.Idx,
    expect: CIR.Statement.Idx,
    type_default: CIR.Statement.Idx,
};

/// The module's definitions and method bindings in strongly connected
/// components of the graph whose edges are the definitions a body names and
/// the methods its dispatch sites resolve to, dependencies before dependents.
const Order = struct {
    allocator: Allocator,
    /// Each component's definitions.
    components: [][]CIR.Def.Idx,
    /// Each component's direct dependencies, as component indices.
    dependencies: [][]u32,

    const empty: Order = .{ .allocator = undefined, .components = &.{}, .dependencies = &.{} };

    fn deinit(self: *Order) void {
        for (self.components) |defs| self.allocator.free(defs);
        self.allocator.free(self.components);
        for (self.dependencies) |deps| self.allocator.free(deps);
        self.allocator.free(self.dependencies);
    }
};

/// The method names lowering looks up on a type by name rather than through
/// a dispatch plan: a structural derivation uses a type's own override.
fn isOverrideMethodName(name: []const u8) bool {
    return std.mem.eql(u8, name, "is_eq") or std.mem.eql(u8, name, "to_hash") or std.mem.eql(u8, name, "to_inspect");
}

const State = struct {
    allocator: Allocator,
    inputs: Inputs,
    order: Order,
    /// The definition graph's nodes: the module's definitions and method
    /// bindings (`graphNodes`).
    nodes: collections.DenseMap(CIR.Def.Idx, void),
    pattern_to_def: collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx),
    definition: collections.DenseMap(CIR.Def.Idx, [32]u8),
    owners: collections.DenseMap(CIR.Expr.Idx, Owner),
    refs: collections.DenseMap(CIR.Def.Idx, void),
    expr_stack: std.ArrayList(CIR.Expr.Idx) = .empty,
    seen_defaults: collections.DenseMap(CIR.Expr.Idx, void),
    digests: std.ArrayList([32]u8) = .empty,
    type_walk: TypeDigest,

    fn init(allocator: Allocator, inputs: Inputs) Allocator.Error!State {
        const env = inputs.env;
        var state = State{
            .allocator = allocator,
            .inputs = inputs,
            .order = .empty,
            .nodes = collections.DenseMap(CIR.Def.Idx, void).init(allocator),
            .pattern_to_def = collections.DenseMap(CIR.Pattern.Idx, CIR.Def.Idx).init(allocator),
            .definition = collections.DenseMap(CIR.Def.Idx, [32]u8).init(allocator),
            .owners = collections.DenseMap(CIR.Expr.Idx, Owner).init(allocator),
            .refs = collections.DenseMap(CIR.Def.Idx, void).init(allocator),
            .seen_defaults = collections.DenseMap(CIR.Expr.Idx, void).init(allocator),
            .type_walk = TypeDigest.init(allocator, inputs),
        };
        errdefer state.deinitMaps();
        var binders = std.ArrayList(CIR.Pattern.Idx).empty;
        defer binders.deinit(allocator);
        var scratch = std.ArrayList(CIR.Pattern.Idx).empty;
        defer scratch.deinit(allocator);
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            binders.clearRetainingCapacity();
            try DependencyGraph.appendPatternBinders(env, env.store.getDef(def_idx).pattern, &binders, &scratch, allocator);
            for (binders.items) |binder| try state.pattern_to_def.put(binder, def_idx);
        }
        // `all_statements` also holds block-local type declarations, and
        // `type_decls` holds the types nested in a block-local type's
        // associated block, so only a declaration in both is module-level.
        var statements = collections.DenseMap(CIR.Statement.Idx, void).init(allocator);
        defer statements.deinit();
        for (env.store.sliceStatements(env.all_statements)) |statement| try statements.put(statement, {});
        for (env.store.sliceStatements(env.type_decls)) |statement| {
            if (statements.contains(statement)) try state.type_walk.top_level.put(statement, {});
        }
        state.order = try state.computeOrder();
        return state;
    }

    /// The definitions and method bindings of the module, each once, in
    /// source order with the method bindings outside the definitions last;
    /// `nodes` holds the same set.
    fn graphNodes(self: *State) Allocator.Error![]CIR.Def.Idx {
        const env = self.inputs.env;
        var nodes = std.ArrayList(CIR.Def.Idx).empty;
        errdefer nodes.deinit(self.allocator);
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            if ((try self.nodes.getOrPut(def_idx)).found_existing) continue;
            try nodes.append(self.allocator, def_idx);
        }
        for (env.method_defs.entries.items) |entry| {
            if ((try self.nodes.getOrPut(entry.value.def_idx)).found_existing) continue;
            try nodes.append(self.allocator, entry.value.def_idx);
        }
        return nodes.toOwnedSlice(self.allocator);
    }

    /// Strongly connected components of the graph over `graphNodes` whose
    /// edges are the definitions each body names and the methods its
    /// dispatch sites resolve to, in an order that puts every component after
    /// the components it depends on.
    fn computeOrder(self: *State) Allocator.Error!Order {
        const allocator = self.allocator;
        const env = self.inputs.env;
        const nodes = try self.graphNodes();
        defer allocator.free(nodes);
        var index_of = collections.DenseMap(CIR.Def.Idx, u32).init(allocator);
        defer index_of.deinit();
        for (nodes, 0..) |def_idx, index| try index_of.put(def_idx, @intCast(index));

        const edge_starts = try allocator.alloc(u32, nodes.len + 1);
        defer allocator.free(edge_starts);
        var edges = std.ArrayList(u32).empty;
        defer edges.deinit(allocator);
        var reached = std.ArrayList(CIR.Def.Idx).empty;
        defer reached.deinit(allocator);
        for (nodes, 0..) |def_idx, index| {
            edge_starts[index] = @intCast(edges.items.len);
            reached.clearRetainingCapacity();
            try self.collectReached(env.store.getDef(def_idx).expr, &reached, null);
            for (reached.items) |target| {
                const target_index = index_of.get(target) orelse
                    base.invariant("compile-time value identity reached a definition outside the module's definitions and method bindings", .{});
                try edges.append(allocator, target_index);
            }
        }
        edge_starts[nodes.len] = @intCast(edges.items.len);

        // Tarjan's algorithm with an explicit stack; components complete in
        // dependency order, since a component finishes only after everything
        // it reaches.
        const unvisited = std.math.maxInt(u32);
        const index = try allocator.alloc(u32, nodes.len);
        defer allocator.free(index);
        @memset(index, unvisited);
        const lowlink = try allocator.alloc(u32, nodes.len);
        defer allocator.free(lowlink);
        const on_stack = try allocator.alloc(bool, nodes.len);
        defer allocator.free(on_stack);
        @memset(on_stack, false);
        const component_of = try allocator.alloc(u32, nodes.len);
        defer allocator.free(component_of);
        var stack = std.ArrayList(u32).empty;
        defer stack.deinit(allocator);
        const Frame = struct { node: u32, next_edge: u32 };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(allocator);
        var components = std.ArrayList([]CIR.Def.Idx).empty;
        errdefer {
            for (components.items) |defs| allocator.free(defs);
            components.deinit(allocator);
        }
        var next_index: u32 = 0;
        for (0..nodes.len) |root_node| {
            if (index[root_node] != unvisited) continue;
            try frames.append(allocator, .{ .node = @intCast(root_node), .next_edge = edge_starts[root_node] });
            index[root_node] = next_index;
            lowlink[root_node] = next_index;
            next_index += 1;
            try stack.append(allocator, @intCast(root_node));
            on_stack[root_node] = true;
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                if (frame.next_edge < edge_starts[node + 1]) {
                    const target = edges.items[frame.next_edge];
                    frame.next_edge += 1;
                    if (index[target] == unvisited) {
                        index[target] = next_index;
                        lowlink[target] = next_index;
                        next_index += 1;
                        try stack.append(allocator, target);
                        on_stack[target] = true;
                        try frames.append(allocator, .{ .node = target, .next_edge = edge_starts[target] });
                    } else if (on_stack[target]) {
                        lowlink[node] = @min(lowlink[node], index[target]);
                    }
                    continue;
                }
                if (lowlink[node] == index[node]) {
                    var members = std.ArrayList(CIR.Def.Idx).empty;
                    errdefer members.deinit(allocator);
                    while (true) {
                        const member = stack.pop().?;
                        on_stack[member] = false;
                        component_of[member] = @intCast(components.items.len);
                        try members.append(allocator, nodes[member]);
                        if (member == node) break;
                    }
                    try components.append(allocator, try members.toOwnedSlice(allocator));
                }
                frames.items.len -= 1;
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    lowlink[parent] = @min(lowlink[parent], lowlink[node]);
                }
            }
        }

        var dependencies = std.ArrayList([]u32).empty;
        errdefer {
            for (dependencies.items) |deps| allocator.free(deps);
            dependencies.deinit(allocator);
        }
        var deps = std.ArrayList(u32).empty;
        defer deps.deinit(allocator);
        var seen_deps = std.AutoHashMapUnmanaged(u32, void).empty;
        defer seen_deps.deinit(allocator);
        for (components.items, 0..) |defs, component| {
            deps.clearRetainingCapacity();
            seen_deps.clearRetainingCapacity();
            for (defs) |def_idx| {
                const node = index_of.get(def_idx).?;
                for (edges.items[edge_starts[node]..edge_starts[node + 1]]) |target| {
                    const target_component = component_of[target];
                    if (target_component == component) continue;
                    if ((try seen_deps.getOrPut(allocator, target_component)).found_existing) continue;
                    try deps.append(allocator, target_component);
                }
            }
            std.mem.sort(u32, deps.items, {}, std.sort.asc(u32));
            try dependencies.append(allocator, try allocator.dupe(u32, deps.items));
        }
        return .{
            .allocator = allocator,
            .components = try components.toOwnedSlice(allocator),
            .dependencies = try dependencies.toOwnedSlice(allocator),
        };
    }

    /// Append to `reached` the definitions of this module that `root` names
    /// and that the dispatch sites in `root` resolve to, each once, and to
    /// `imported` the identities of the dispatch targets in other modules.
    fn collectReached(self: *State, root: CIR.Expr.Idx, reached: *std.ArrayList(CIR.Def.Idx), imported: ?*std.ArrayList([32]u8)) Allocator.Error!void {
        self.refs.clearRetainingCapacity();
        try DependencyGraph.collectNameReferences(self.inputs.env, &self.pattern_to_def, root, &self.refs, &self.expr_stack, &self.seen_defaults, self.allocator);
        try self.collectDispatchTargets(root, &self.refs, imported);
        var it = self.refs.keyIterator();
        while (it.next()) |def_idx| try reached.append(self.allocator, def_idx.*);
    }

    /// Add to `own` the definitions of this module that the dispatch sites
    /// in `root` resolve to, through every nested obligation each target's
    /// instantiation resolves, and to `imported` the identities of the
    /// targets in other modules.
    fn collectDispatchTargets(self: *State, root: CIR.Expr.Idx, own: *collections.DenseMap(CIR.Def.Idx, void), imported: ?*std.ArrayList([32]u8)) Allocator.Error!void {
        const env = self.inputs.env;
        const plans = self.inputs.plans;
        var stack = std.ArrayList(CIR.Expr.Idx).empty;
        defer stack.deinit(self.allocator);
        var seen = collections.DenseMap(CIR.Expr.Idx, void).init(self.allocator);
        defer seen.deinit();
        var seen_nodes = std.AutoHashMapUnmanaged(u32, void).empty;
        defer seen_nodes.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |expr| {
            const entry = try seen.getOrPut(expr);
            if (entry.found_existing) continue;
            const node = @backingInt(expr);
            // A call's instantiation edge records the evidence of the
            // callee's requirements.
            if (self.inputs.bodies.checkedExprAtSourceNode(node)) |checked_expr| {
                if (plans.siteEvidence(checked_expr)) |evidence_list| for (evidence_list) |evidence| switch (evidence.resolution) {
                    .direct => |id| try self.collectEvidenceTargets(id, &seen_nodes, own, imported),
                    .constraint, .structural, .from_callable, .from_scheme, .checked_error, .unreachable_value => {},
                };
            }
            for ([_]?static_dispatch.StaticDispatchPlanId{ plans.lookupByExpr(expr), plans.lookupNumeralByNode(node), plans.lookupQuoteByNode(node) }) |found| {
                const plan_id = found orelse continue;
                switch (plans.plans[@backingInt(plan_id)].resolution) {
                    .direct_closed, .direct_parametric => |call| try self.collectEvidenceTargets(call.evidence, &seen_nodes, own, imported),
                    .direct_pending => base.invariant("compile-time value identity read a dispatch plan whose target was still pending", .{}),
                    // A site dispatching on a scheme variable takes its target
                    // from the evidence of the instantiating call, which that
                    // call's plan resolves; a structural derivation and a
                    // rejected or unreachable site select no method.
                    .evidence_dependent, .structural, .checked_error, .@"unreachable" => {},
                }
            }
            try appendExprChildren(env, expr, &stack, self.allocator);
        }
    }

    /// The identities of the methods the conversion of the literal at source
    /// node `node` runs: its `from_numeral` or `from_quote` and the evidence
    /// of that conversion's requirements, which the literal's plan resolves.
    /// The literal may be a pattern, which no expression walk reaches.
    fn literalTargetsDigest(self: *State, node: u32) Allocator.Error![32]u8 {
        const plans = self.inputs.plans;
        var own = collections.DenseMap(CIR.Def.Idx, void).init(self.allocator);
        defer own.deinit();
        var imported = std.ArrayList([32]u8).empty;
        defer imported.deinit(self.allocator);
        var seen_nodes = std.AutoHashMapUnmanaged(u32, void).empty;
        defer seen_nodes.deinit(self.allocator);
        for ([_]?static_dispatch.StaticDispatchPlanId{ plans.lookupNumeralByNode(node), plans.lookupQuoteByNode(node) }) |found| {
            const plan_id = found orelse continue;
            switch (plans.plans[@backingInt(plan_id)].resolution) {
                .direct_closed, .direct_parametric => |call| try self.collectEvidenceTargets(call.evidence, &seen_nodes, &own, &imported),
                .direct_pending => base.invariant("compile-time value identity read a literal plan whose target was still pending", .{}),
                .evidence_dependent, .structural, .checked_error, .@"unreachable" => {},
            }
        }
        var digests = std.ArrayList([32]u8).empty;
        defer digests.deinit(self.allocator);
        var it = own.keyIterator();
        while (it.next()) |def_idx| try digests.append(self.allocator, self.definitionIdentity(def_idx.*));
        try digests.appendSlice(self.allocator, imported.items);
        sortDigests(digests.items);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "literal-targets");
        hashDigests(&hasher, digests.items);
        return hasher.finalResult();
    }

    /// The identities of every method of every nominal type `root` mentions,
    /// sorted: a literal converted at that type runs its `from_numeral` or
    /// `from_quote` with the type's methods as the evidence of the
    /// conversion's requirements. An own type's methods are named by
    /// identity, an imported type's by the identities its module published
    /// in a module-independent identity and by the module's version
    /// otherwise.
    fn typeMethodsDigest(self: *State, root: CheckedTypeId) Allocator.Error![32]u8 {
        const env = self.inputs.env;
        var pending = std.ArrayList(CheckedTypeId).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();
        var digests = std.ArrayList([32]u8).empty;
        defer digests.deinit(self.allocator);
        try pending.append(self.allocator, root);
        while (pending.pop()) |ty| {
            const entry = try seen.getOrPut(ty);
            if (entry.found_existing) continue;
            var scratch = Sha256.init(.{});
            try self.type_walk.writeNode(&scratch, ty, &pending);
            const nominal = switch (self.inputs.types.payload(ty)) {
                .nominal => |nominal| nominal,
                .pending, .err, .flex, .rigid, .alias, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => continue,
            };
            if (self.type_walk.isOwn(nominal.origin_module)) {
                const statement: CIR.Statement.Idx = @fromBackingInt(nominal.source_decl);
                for (env.method_defs.entries.items) |method| {
                    if (method.key.owner != statement) continue;
                    try digests.append(self.allocator, try self.methodIdentity(method.value.def_idx));
                }
                continue;
            }
            switch (nominal.representation) {
                .imported_declaration => |ref| {
                    if (!self.inputs.test_dependencies) {
                        try digests.append(self.allocator, digestBytes("imported-type-version", &ref.artifact.bytes));
                        continue;
                    }
                    const owner = self.type_walk.importedView(ref.artifact.bytes, true);
                    const declaration = owner.checked_types.nominal_declarations[@backingInt(ref.declaration)];
                    const statement: CIR.Statement.Idx = @fromBackingInt(declaration.source_statement);
                    for (owner.module_env.method_defs.entries.items) |method| {
                        if (method.key.owner != statement) continue;
                        try digests.append(self.allocator, importedDefinition(owner, method.value.def_idx));
                    }
                },
                .imported_box_payload_capability => |ref| try digests.append(self.allocator, digestBytes("imported-type-version", &ref.artifact.bytes)),
                .builtin, .local_declaration, .local_box_payload_capability, .opaque_without_backing => {},
            }
        }
        sortDigests(digests.items);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "type-methods");
        hashDigests(&hasher, digests.items);
        return hasher.finalResult();
    }

    /// Add the target of evidence node `id` and of every obligation nested
    /// under it.
    fn collectEvidenceTargets(self: *State, id: static_dispatch.EvidenceNodeId, seen_nodes: *std.AutoHashMapUnmanaged(u32, void), own: *collections.DenseMap(CIR.Def.Idx, void), imported: ?*std.ArrayList([32]u8)) Allocator.Error!void {
        if ((try seen_nodes.getOrPut(self.allocator, @backingInt(id))).found_existing) return;
        const plans = self.inputs.plans;
        const node = plans.evidenceNode(id);
        try self.collectTarget(node.target, own, imported);
        switch (node.nested) {
            .resolved => for (plans.nestedEvidence(node)) |evidence| switch (evidence.resolution) {
                .direct => |nested| try self.collectEvidenceTargets(nested, seen_nodes, own, imported),
                // A constraint names an obligation of the enclosing callable,
                // which its own instantiating call resolves; a structural
                // derivation selects no method; the rest supply no target.
                .constraint, .structural, .from_callable, .from_scheme, .checked_error, .unreachable_value => {},
            },
            // The targets of these obligations are derived from the concrete
            // callable when the edge is lowered, so the identity covers every
            // implementation the edge could select: the module's own, and its
            // imports by version.
            .from_callable, .through_alias => try self.collectEveryMethod(own, imported),
        }
    }

    /// Add every method binding of the module, and the version of every
    /// direct import, for an edge whose nested targets are derived at
    /// lowering.
    fn collectEveryMethod(self: *State, own: *collections.DenseMap(CIR.Def.Idx, void), imported: ?*std.ArrayList([32]u8)) Allocator.Error!void {
        for (self.inputs.env.method_defs.entries.items) |entry| try own.put(entry.value.def_idx, {});
        const list = imported orelse return;
        for (self.inputs.imports) |entry| try list.append(self.allocator, digestBytes("imported-version", &entry.key.bytes));
    }

    /// Add `target`: this module's definition it reaches, or the identity of
    /// an imported module's.
    fn collectTarget(self: *State, target: static_dispatch.MethodTarget, own: *collections.DenseMap(CIR.Def.Idx, void), imported: ?*std.ArrayList([32]u8)) Allocator.Error!void {
        switch (target.kind) {
            .procedure => |procedure| {
                if (target.module_idx == self.inputs.module_idx) {
                    // A method of a block-local type is lexically inside a
                    // definition of the graph, whose bytes cover it, and a
                    // value of that type reaches other code only through
                    // that definition.
                    if (self.nodes.contains(target.def_idx)) try own.put(target.def_idx, {});
                } else if (imported) |list| {
                    try list.append(self.allocator, self.importedTargetIdentity(procedure.template, target.def_idx));
                }
            },
            // A block-local procedure is lexically inside the code that
            // reaches it, whose bytes the identity already covers.
            .local_proc => {},
            // A structural derivation selects no method of its own; a type's
            // override it uses is in the type world.
            .structural => {},
        }
    }

    /// The identity of the definition `def` of the module `template` names,
    /// or the module's version when the compilation published no identities
    /// for it (the builtin module's never change within a compiler).
    fn importedTargetIdentity(self: *const State, template: canonical.ProcedureTemplateRef, def: CIR.Def.Idx) [32]u8 {
        for (self.inputs.imports) |entry| if (std.meta.eql(entry.key.bytes, template.artifact.bytes)) return importedDefinition(entry.view, def);
        for (self.inputs.available) |view| if (std.meta.eql(view.key.bytes, template.artifact.bytes)) return importedDefinition(view, def);
        for (self.inputs.relations) |view| if (std.meta.eql(view.key.bytes, template.artifact.bytes)) return importedDefinition(view, def);
        var hash = Sha256.init(.{});
        hashTag(&hash, "foreign-target");
        hash.update(&template.artifact.bytes);
        hashU32(&hash, @backingInt(def));
        return hash.finalResult();
    }

    fn deinitMaps(self: *State) void {
        self.nodes.deinit();
        self.pattern_to_def.deinit();
        self.definition.deinit();
        self.owners.deinit();
        self.refs.deinit();
        self.expr_stack.deinit(self.allocator);
        self.seen_defaults.deinit();
        self.digests.deinit(self.allocator);
        self.type_walk.deinit();
    }

    fn deinit(self: *State) void {
        self.deinitMaps();
        self.order.deinit();
    }

    fn source(self: *const State) []const u8 {
        return self.inputs.env.getSourceAll();
    }

    fn regionText(self: *const State, region: base.Region) []const u8 {
        return self.source()[region.start.offset..region.end.offset];
    }

    /// One definition's own contribution: its source bytes and the type of
    /// its published scheme.
    fn localDigest(self: *State, def_idx: CIR.Def.Idx) Allocator.Error![32]u8 {
        const env = self.inputs.env;
        const def = env.store.getDef(def_idx);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "def");
        hashBytes(&hasher, self.regionText(env.store.getPatternRegion(def.pattern)));
        if (def.annotation) |annotation| {
            hashTag(&hasher, "annotation");
            hashBytes(&hasher, self.regionText(env.store.getAnnotationRegion(annotation)));
        } else hashTag(&hasher, "no-annotation");
        hashBytes(&hasher, self.regionText(env.store.getExprRegion(def.expr)));
        const scheme_root: ?CheckedTypeId = if (self.inputs.top_level_values.lookupByDef(def_idx)) |entry|
            if (self.inputs.types.schemeForKey(entry.source_scheme)) |scheme| scheme.root else null
        else
            null;
        if (scheme_root) |root| {
            hashTag(&hasher, "scheme");
            hasher.update(&try self.type_walk.digest(root));
        } else hashTag(&hasher, "no-scheme");
        if (self.inputs.test_dependencies) {
            // Failed-expect detail includes the complete mentioned binding,
            // including whitespace between its pattern and expression.
            const expr_tag = std.meta.activeTag(env.store.getExpr(def.expr));
            if (expr_tag != .e_lambda and expr_tag != .e_closure and expr_tag != .e_hosted_lambda) {
                const start = env.store.getPatternRegion(def.pattern).start.offset;
                const end = env.store.getExprRegion(def.expr).end.offset;
                // This is the reporter's exact excerpt eligibility rule.
                if (start < end and end <= self.source().len) hashBytes(&hasher, self.source()[start..end]);
            }
            hasher.update(&try self.importedReferences(def.expr));
        }
        return hasher.finalResult();
    }

    /// Definition identities, one strongly connected component at a time in
    /// dependency order: each folds its members' own contributions and the
    /// identities of the components they name.
    fn computeDefinitionIdentities(self: *State) Allocator.Error!void {
        const group_identities = try self.allocator.alloc([32]u8, self.order.components.len);
        defer self.allocator.free(group_identities);
        var locals = std.ArrayList([32]u8).empty;
        defer locals.deinit(self.allocator);
        var deps = std.ArrayList([32]u8).empty;
        defer deps.deinit(self.allocator);
        for (self.order.components, 0..) |defs, group_index| {
            locals.clearRetainingCapacity();
            for (defs) |def_idx| try locals.append(self.allocator, try self.localDigest(def_idx));
            const member_locals = try self.allocator.dupe([32]u8, locals.items);
            defer self.allocator.free(member_locals);
            sortDigests(locals.items);
            deps.clearRetainingCapacity();
            for (self.order.dependencies[group_index]) |dependency| try deps.append(self.allocator, group_identities[dependency]);
            sortDigests(deps.items);
            var hasher = Sha256.init(.{});
            hashTag(&hasher, "component");
            hashDigests(&hasher, locals.items);
            hashDigests(&hasher, deps.items);
            group_identities[group_index] = hasher.finalResult();
            for (defs, member_locals) |def_idx, local| {
                var member = Sha256.init(.{});
                hashTag(&member, "definition");
                member.update(&local);
                member.update(&group_identities[group_index]);
                try self.definition.put(def_idx, member.finalResult());
            }
        }
    }

    fn definitionIdentity(self: *const State, def_idx: CIR.Def.Idx) [32]u8 {
        return self.definition.get(def_idx) orelse
            base.invariant("compile-time value identity asked for a definition outside the module's definitions", .{});
    }

    /// A method's or exported definition's identity: every method binding
    /// is a node of the definition graph.
    fn methodIdentity(self: *State, def_idx: CIR.Def.Idx) Allocator.Error![32]u8 {
        return self.definitionIdentity(def_idx);
    }

    /// The sorted identities of the definitions `expr` names and of the
    /// methods of this module its dispatch sites resolve to.
    fn referencedIdentities(self: *State, expr: CIR.Expr.Idx) Allocator.Error![]const [32]u8 {
        var reached = std.ArrayList(CIR.Def.Idx).empty;
        defer reached.deinit(self.allocator);
        try self.collectReached(expr, &reached, null);
        self.digests.clearRetainingCapacity();
        for (reached.items) |def_idx| try self.digests.append(self.allocator, self.definitionIdentity(def_idx));
        sortDigests(self.digests.items);
        return self.digests.items;
    }

    /// Map every expression of every definition and top-level `expect` to
    /// the declaration that contains it.
    fn mapExpressionOwners(self: *State) Allocator.Error!void {
        const env = self.inputs.env;
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            try self.mapOwner(env.store.getDef(def_idx).expr, .{ .def = def_idx });
        }
        for (env.store.sliceStatements(env.all_statements)) |statement_idx| {
            switch (env.store.getStatement(statement_idx)) {
                .s_expect => |expect| try self.mapOwner(expect.body, .{ .expect = statement_idx }),
                .s_decl,
                .s_var,
                .s_var_uninitialized,
                .s_reassign,
                .s_crash,
                .s_dbg,
                .s_expr,
                .s_for,
                .s_while,
                .s_infinite_loop,
                .s_breakable_loop,
                .s_break,
                .s_return,
                .s_import,
                .s_alias_decl,
                .s_nominal_decl,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                .s_runtime_error,
                => {},
            }
        }
        if (!self.inputs.test_dependencies) return;
        for (env.store.sliceStatements(env.type_decls)) |statement| {
            const fields = can.default_omissions.backingRecordFields(env, statement) orelse continue;
            for (env.store.sliceAnnoRecordFields(fields)) |field| {
                const expr = env.store.getAnnoRecordField(field).default_value orelse continue;
                try self.mapOwner(expr, .{ .type_default = statement });
            }
        }
    }

    fn mapOwner(self: *State, root: CIR.Expr.Idx, owner: Owner) Allocator.Error!void {
        var stack = std.ArrayList(CIR.Expr.Idx).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, root);
        while (stack.pop()) |expr| {
            const entry = try self.owners.getOrPut(expr);
            if (entry.found_existing) continue;
            entry.value_ptr.* = owner;
            try appendExprChildren(self.inputs.env, expr, &stack, self.allocator);
        }
    }

    fn declarationIdentities(self: *State) Allocator.Error![]DeclarationIdentity {
        var rows = std.ArrayList(DeclarationIdentity).empty;
        errdefer rows.deinit(self.allocator);
        if (!self.inputs.test_dependencies) return rows.toOwnedSlice(self.allocator);
        const env = self.inputs.env;
        var owners = collections.DenseMap(CIR.Statement.Idx, Owner).init(self.allocator);
        defer owners.deinit();
        // A checked type can name any declaration the module holds, including
        // one in code that canonicalization or checking replaced with a
        // runtime error, which no expression reaches. So the explicit
        // declaration lists are enumerated here, and blocks add ownership.
        var declarations = std.ArrayList(CIR.Statement.Idx).empty;
        defer declarations.deinit(self.allocator);
        try declarations.appendSlice(self.allocator, env.store.sliceStatements(env.type_decls));
        try declarations.appendSlice(self.allocator, env.store.sliceStatements(env.forward_type_decls));
        for (env.store.sliceStatements(env.all_statements)) |statement| {
            if (declaresType(env.store.getStatement(statement))) try declarations.append(self.allocator, statement);
        }
        for (env.for_clause_aliases.items.items) |alias| try declarations.append(self.allocator, alias.alias_stmt_idx);
        var expressions = self.owners.keyIterator();
        while (expressions.next()) |expr| {
            if (env.store.getExpr(expr.*) == .e_block) {
                for (env.store.sliceStatements(env.store.getExpr(expr.*).e_block.stmts)) |statement| {
                    try owners.put(statement, self.owners.get(expr.*).?);
                    if (declaresType(env.store.getStatement(statement))) try declarations.append(self.allocator, statement);
                }
            }
        }
        var seen = collections.DenseMap(CIR.Statement.Idx, void).init(self.allocator);
        defer seen.deinit();
        var owner_digests = std.AutoHashMapUnmanaged(Owner, [32]u8).empty;
        defer owner_digests.deinit(self.allocator);
        for (declarations.items) |statement| {
            const entry = try seen.getOrPut(statement);
            if (entry.found_existing) continue;
            var hash = Sha256.init(.{});
            hashTag(&hash, "roc.test-declaration.v1");
            hashBytes(&hash, env.module_name);
            const region = env.store.getStatementRegion(statement);
            hashBytes(&hash, self.regionText(region));
            if (owners.get(statement)) |owner| {
                const envelope = switch (owner) {
                    .def => |def| env.store.getExprRegion(env.store.getDef(def).expr),
                    .expect, .type_default => |root| env.store.getStatementRegion(root),
                };
                const owner_digest = try owner_digests.getOrPut(self.allocator, owner);
                if (!owner_digest.found_existing) {
                    var owner_hash = Sha256.init(.{});
                    owner_hash.update(&self.ownerKey(owner));
                    hashBytes(&owner_hash, self.regionText(envelope));
                    owner_digest.value_ptr.* = owner_hash.finalResult();
                }
                hash.update(owner_digest.value_ptr);
                hashU32(&hash, region.start.offset - envelope.start.offset);
                hashU32(&hash, region.end.offset - envelope.start.offset);
            } else if (!self.type_walk.top_level.contains(statement)) {
                // A declaration outside the expression ownership graph retains
                // its exact version provenance; no lexical owner is invented.
                hash.update(&self.inputs.own_identity);
                hashU32(&hash, @backingInt(statement));
            }
            try rows.append(self.allocator, .{ .statement = statement, .value = hash.finalResult() });
        }
        std.mem.sort(DeclarationIdentity, rows.items, {}, struct {
            fn less(_: void, a: DeclarationIdentity, b: DeclarationIdentity) bool {
                return @backingInt(a.statement) < @backingInt(b.statement);
            }
        }.less);
        return rows.toOwnedSlice(self.allocator);
    }

    fn reportSites(self: *State) Allocator.Error![]TestReportSite {
        var sites = std.ArrayList(TestReportSite).empty;
        errdefer sites.deinit(self.allocator);
        const env = self.inputs.env;
        const Anchor = struct { region: base.Region, identity: [32]u8 };
        var anchors = std.AutoHashMapUnmanaged(Owner, Anchor).empty;
        defer anchors.deinit(self.allocator);
        var it = self.owners.keyIterator();
        while (it.next()) |expr| {
            const owner = self.owners.get(expr.*).?;
            const anchor = try anchors.getOrPut(self.allocator, owner);
            if (!anchor.found_existing) {
                const region = switch (owner) {
                    .def => |def| env.store.getExprRegion(env.store.getDef(def).expr),
                    .expect, .type_default => |statement| env.store.getStatementRegion(statement),
                };
                var hash = Sha256.init(.{});
                hashTag(&hash, "roc.test-report-owner.v1");
                hash.update(&self.ownerKey(owner));
                hashBytes(&hash, self.regionText(region));
                anchor.value_ptr.* = .{ .region = region, .identity = hash.finalResult() };
            }
            const owner_region = anchor.value_ptr.region;
            const identity = anchor.value_ptr.identity;
            try self.appendReportSite(&sites, identity, owner_region, env.store.getExprRegion(expr.*));
            if (env.store.getExpr(expr.*) == .e_block) {
                for (env.store.sliceStatements(env.store.getExpr(expr.*).e_block.stmts)) |statement| {
                    try self.appendReportSite(&sites, identity, owner_region, env.store.getStatementRegion(statement));
                }
            }
        }
        return sites.toOwnedSlice(self.allocator);
    }

    fn appendReportSite(self: *State, sites: *std.ArrayList(TestReportSite), owner: [32]u8, envelope: base.Region, region: base.Region) Allocator.Error!void {
        // Generated expressions without a source span have no report site.
        if (region.start.offset < envelope.start.offset or region.end.offset > envelope.end.offset) return;
        var hash = Sha256.init(.{});
        hash.update(&owner);
        hashU32(&hash, region.start.offset - envelope.start.offset);
        hashU32(&hash, region.end.offset - envelope.start.offset);
        try sites.append(self.allocator, .{ .identity = hash.finalResult(), .start = region.start.offset, .end = region.end.offset });
    }

    fn importedDefinition(view: checked_artifact.ImportedModuleView, def: CIR.Def.Idx) [32]u8 {
        const summary = view.compile_time_root_identities;
        var lo: usize = 0;
        var hi = summary.test_definitions.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const entry = summary.test_definitions[mid];
            if (@backingInt(entry.def) < @backingInt(def)) {
                lo = mid + 1;
            } else if (@backingInt(entry.def) > @backingInt(def)) {
                hi = mid;
            } else {
                var hash = Sha256.init(.{});
                hashTag(&hash, "imported-test-definition");
                hash.update(&summary.test_context[0]);
                hash.update(&entry.value);
                return hash.finalResult();
            }
        }
        base.invariant("checked imported reference has no published implementation identity", .{});
    }

    fn importedByIndex(self: *const State, index: CIR.Import.Idx) checked_artifact.ImportedModuleView {
        // External lookups use the resolved import scope supplied to checked
        // publication. The drain's identity table need not contain imports
        // introduced by checking (including implicit builtin imports).
        const module_idx = self.inputs.env.imports.getResolvedModule(index) orelse
            base.invariant("checked external lookup has no resolved module", .{});
        for (self.inputs.imports) |entry| {
            if (entry.module_idx == module_idx) return entry.view;
        }
        base.invariant("checked external lookup has no published imported artifact", .{});
    }

    fn importedByIdentity(self: *const State, identity: [32]u8) checked_artifact.ImportedModuleView {
        for (self.inputs.imports) |entry| if (std.meta.eql(entry.view.module_identity.stable_hash, identity)) return entry.view;
        for (self.inputs.available) |view| if (std.meta.eql(view.module_identity.stable_hash, identity)) return view;
        for (self.inputs.relations) |view| if (std.meta.eql(view.module_identity.stable_hash, identity)) return view;
        base.invariant("checked associated lookup has no declaring module", .{});
    }

    /// Traverse explicit checked name references on every path, including
    /// nested closure bodies. Local recursion is handled by the SCC summaries.
    fn importedReferences(self: *State, root: CIR.Expr.Idx) Allocator.Error![32]u8 {
        var stack = std.ArrayList(CIR.Expr.Idx).empty;
        defer stack.deinit(self.allocator);
        var seen = collections.DenseMap(CIR.Expr.Idx, void).init(self.allocator);
        defer seen.deinit();
        var values = std.ArrayList([32]u8).empty;
        defer values.deinit(self.allocator);
        var own_targets = collections.DenseMap(CIR.Def.Idx, void).init(self.allocator);
        defer own_targets.deinit();
        try self.collectDispatchTargets(root, &own_targets, &values);
        try stack.append(self.allocator, root);
        while (stack.pop()) |expr| {
            const entry = try seen.getOrPut(expr);
            if (entry.found_existing) continue;
            switch (self.inputs.env.store.getExpr(expr)) {
                .e_lookup_external => |lookup| {
                    const view = self.importedByIndex(lookup.module_idx);
                    const target: CIR.Def.Idx = @fromBackingInt(lookup.target_node_idx);
                    try values.append(self.allocator, importedDefinition(view, target));
                },
                .e_lookup_associated_resolved => |lookup| {
                    const identity = self.inputs.env.moduleIdentityHash(lookup.module_identity);
                    if (!std.meta.eql(identity.*, self.inputs.own_identity)) {
                        try values.append(self.allocator, importedDefinition(self.importedByIdentity(identity.*), lookup.target_def_idx));
                    }
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
                .e_str_segment,
                .e_str,
                .e_bytes_literal,
                .e_lookup_local,
                .e_deferred_import_ref,
                .e_lookup_associated_local,
                .e_lookup_associated,
                .e_lookup_required,
                .e_list,
                .e_empty_list,
                .e_tuple,
                .e_match,
                .e_if,
                .e_call,
                .e_record,
                .e_empty_record,
                .e_block,
                .e_tag,
                .e_nominal,
                .e_nominal_external,
                .e_zero_argument_tag,
                .e_closure,
                .e_lambda,
                .e_binop,
                .e_unary_minus,
                .e_field_access,
                .e_method_call,
                .e_dispatch_call,
                .e_interpolation,
                .e_structural_eq,
                .e_structural_hash,
                .e_method_eq,
                .e_type_method_call,
                .e_type_dispatch_call,
                .e_tuple_access,
                .e_runtime_error,
                .e_crash,
                .e_dbg,
                .e_expect_err,
                .e_expect,
                .e_ellipsis,
                .e_anno_only,
                .e_derived_method,
                .e_return,
                .e_break,
                .e_for,
                .e_hosted_lambda,
                .e_run_low_level,
                => {},
            }
            try appendExprChildren(self.inputs.env, expr, &stack, self.allocator);
        }
        sortDigests(values.items);
        var hash = Sha256.init(.{});
        hashTag(&hash, "checked-import-references");
        hashDigests(&hash, values.items);
        return hash.finalResult();
    }

    /// Everything outside any one root that its value can depend on: the
    /// module's checking context and imports, its name and kind, and the
    /// files it imports.
    fn moduleContext(self: *const State) [32]u8 {
        const env = self.inputs.env;
        const context = self.inputs.context;
        var hasher = Sha256.init(.{});
        hashTag(&hasher, domain);
        hashTag(&hasher, "context");
        hasher.update(&context.checking_context);
        if (self.inputs.test_dependencies) {
            for (self.inputs.imports) |imported| {
                const summary = imported.view.compile_time_root_identities;
                if (summary.test_context.len != 1) base.invariant("imported checked module has no test dependency context", .{});
                hasher.update(&summary.test_context[0]);
            }
        } else hasher.update(&context.direct_import_keys);
        // A type module's main type is named after the module, so the name
        // covers it; a type module whose declaration is missing names no
        // main type at all.
        hashBytes(&hasher, env.module_name);
        hashBytes(&hasher, @tagName(env.module_kind));
        const deps = env.file_dependencies.items.items;
        hashU32(&hasher, @intCast(deps.len));
        for (deps) |dep| {
            hashBytes(&hasher, env.fileDependencyRelativePath(dep));
            hashBytes(&hasher, @tagName(dep.state));
            switch (dep.state) {
                .present => hasher.update(&dep.content_hash),
                .missing, .unreadable => {},
                .pending => base.invariant("compile-time value identity read an unsettled file import", .{}),
            }
        }
        return hasher.finalResult();
    }

    /// The module's type world: the source of every top-level statement that
    /// is not a value or `expect`, every type declaration, and the identity
    /// of every method the module registers.
    fn typeWorld(self: *State) Allocator.Error![32]u8 {
        const env = self.inputs.env;
        var texts = std.ArrayList([32]u8).empty;
        defer texts.deinit(self.allocator);
        var seen = collections.DenseMap(CIR.Statement.Idx, void).init(self.allocator);
        defer seen.deinit();
        for ([_][]const CIR.Statement.Idx{ env.store.sliceStatements(env.all_statements), env.store.sliceStatements(env.type_decls) }) |statements| {
            for (statements) |statement_idx| {
                const entry = try seen.getOrPut(statement_idx);
                if (entry.found_existing) continue;
                const include = switch (env.store.getStatement(statement_idx)) {
                    .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_var_alias, .s_runtime_error => true,
                    .s_decl,
                    .s_var,
                    .s_var_uninitialized,
                    .s_reassign,
                    .s_crash,
                    .s_dbg,
                    .s_expr,
                    .s_expect,
                    .s_for,
                    .s_while,
                    .s_infinite_loop,
                    .s_breakable_loop,
                    .s_break,
                    .s_return,
                    .s_type_anno,
                    => false,
                };
                if (!include) continue;
                try texts.append(self.allocator, digestBytes("statement", self.regionText(env.store.getStatementRegion(statement_idx))));
            }
        }
        if (self.inputs.test_dependencies) {
            // Foreign constructions materialize these defaults without a local
            // value-reference edge. Publish their complete dependencies as part
            // of the declaring module's semantic type environment.
            for (env.store.sliceStatements(env.type_decls)) |statement| {
                const fields = can.default_omissions.backingRecordFields(env, statement) orelse continue;
                for (env.store.sliceAnnoRecordFields(fields)) |field| {
                    const expr = env.store.getAnnoRecordField(field).default_value orelse continue;
                    var hash = Sha256.init(.{});
                    hashTag(&hash, "declared-default-dependencies");
                    hashDigests(&hash, try self.referencedIdentities(expr));
                    hash.update(&try self.importedReferences(expr));
                    try texts.append(self.allocator, hash.finalResult());
                }
            }
        }
        sortDigests(texts.items);
        // A dispatch site's targets are part of the identity of the code
        // that reaches it; only the overrides a structural derivation looks
        // up by name belong to the type world.
        var methods = std.ArrayList([32]u8).empty;
        defer methods.deinit(self.allocator);
        for (env.method_defs.entries.items) |entry| {
            const ident: base.Ident.Idx = @bitCast(entry.key.method_ident_bits);
            if (!isOverrideMethodName(env.getIdentText(ident))) continue;
            try methods.append(self.allocator, try self.methodIdentity(entry.value.def_idx));
        }
        sortDigests(methods.items);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "type-world");
        hashDigests(&hasher, texts.items);
        hashDigests(&hasher, methods.items);
        return hasher.finalResult();
    }

    /// The identity of every definition the module exports, sorted by name.
    fn exportIdentities(self: *State) Allocator.Error![]ExportIdentity {
        const env = self.inputs.env;
        const defs = env.store.sliceDefs(env.exports);
        const exports = try self.allocator.alloc(ExportIdentity, defs.len);
        errdefer self.allocator.free(exports);
        for (defs, exports) |def_idx, *entry| entry.* = .{ .name = nameDigest(self.defName(def_idx)), .value = try self.methodIdentity(def_idx) };
        std.mem.sort(ExportIdentity, exports, {}, struct {
            fn lessThan(_: void, a: ExportIdentity, b: ExportIdentity) bool {
                return std.mem.order(u8, &a.name, &b.name) == .lt;
            }
        }.lessThan);
        return exports;
    }

    /// The platform requirements the code of `starts`, and of every
    /// definition that code names, looks up, by sorted name digest.
    fn requiresReachedFrom(self: *State, starts: []const CIR.Def.Idx) Allocator.Error![][32]u8 {
        const env = self.inputs.env;
        var names = std.AutoArrayHashMapUnmanaged([32]u8, void).empty;
        defer names.deinit(self.allocator);
        const requires = env.requires_types.items.items;
        if (requires.len == 0) return try self.allocator.alloc([32]u8, 0);
        var visited = collections.DenseMap(CIR.Def.Idx, void).init(self.allocator);
        defer visited.deinit();
        var pending = std.ArrayList(CIR.Def.Idx).empty;
        defer pending.deinit(self.allocator);
        try pending.appendSlice(self.allocator, starts);
        var exprs = std.ArrayList(CIR.Expr.Idx).empty;
        defer exprs.deinit(self.allocator);
        while (pending.pop()) |def_idx| {
            const seen = try visited.getOrPut(def_idx);
            if (seen.found_existing) continue;
            const body = env.store.getDef(def_idx).expr;
            exprs.clearRetainingCapacity();
            try exprs.append(self.allocator, body);
            while (exprs.pop()) |expr| {
                const data = env.store.getExpr(expr);
                if (data == .e_lookup_required) {
                    const ident = requires[data.e_lookup_required.requires_idx.toU32()].ident;
                    try names.put(self.allocator, nameDigest(env.getIdent(ident)), {});
                }
                try appendExprChildren(env, expr, &exprs, self.allocator);
            }
            self.refs.clearRetainingCapacity();
            try DependencyGraph.collectNameReferences(env, &self.pattern_to_def, body, &self.refs, &self.expr_stack, &self.seen_defaults, self.allocator);
            var it = self.refs.keyIterator();
            while (it.next()) |referenced| try pending.append(self.allocator, referenced.*);
        }
        const result = try self.allocator.dupe([32]u8, names.keys());
        sortDigests(result);
        return result;
    }

    /// The platform requirements the module's methods reach: dispatch on its
    /// types can run them from any module.
    fn methodRequires(self: *State) Allocator.Error![][32]u8 {
        const env = self.inputs.env;
        var starts = std.ArrayList(CIR.Def.Idx).empty;
        defer starts.deinit(self.allocator);
        for (env.method_defs.entries.items) |entry| try starts.append(self.allocator, entry.value.def_idx);
        return self.requiresReachedFrom(starts.items);
    }

    /// The platform requirements the module's exported definitions reach:
    /// a module importing this one can run them.
    fn exportRequires(self: *State) Allocator.Error![][32]u8 {
        const env = self.inputs.env;
        return self.requiresReachedFrom(env.store.sliceDefs(env.exports));
    }

    fn defName(self: *const State, def_idx: CIR.Def.Idx) []const u8 {
        const env = self.inputs.env;
        const pattern = env.store.getDef(def_idx).pattern;
        const data = env.store.getPattern(pattern);
        if (data == .assign) return env.getIdent(data.assign.ident);
        return self.regionText(env.store.getPatternRegion(pattern));
    }

    fn ownerKey(self: *const State, owner: ?Owner) [32]u8 {
        const env = self.inputs.env;
        const resolved = owner orelse return digestBytes("owner.none", "");
        return switch (resolved) {
            .def => |def_idx| digestBytes("owner.def", self.defName(def_idx)),
            .expect => |statement| digestBytes("owner.expect", self.regionText(env.store.getStatementRegion(statement))),
            .type_default => |statement| digestBytes("owner.default", self.regionText(env.store.getStatementRegion(statement))),
        };
    }

    /// The next ordinal among the roots of one class: roots whose sites
    /// would otherwise coincide. A class names everything its roots' value
    /// identities cover beyond the module context, so roots that share a class
    /// have equal value identities and their order among themselves never
    /// decides which value a site names.
    fn nextOrdinal(self: *State, ordinals: *std.AutoHashMapUnmanaged([32]u8, u32), class: [32]u8) Allocator.Error!u32 {
        const entry = try ordinals.getOrPut(self.allocator, class);
        if (!entry.found_existing) entry.value_ptr.* = 0;
        const ordinal = entry.value_ptr.*;
        entry.value_ptr.* += 1;
        return ordinal;
    }

    /// A hoisted expression is typed independently of its owner's unrelated
    /// expressions. Retain the checked types of its complete expression graph,
    /// including sharing between those types. A read of a lexical binding
    /// additionally names its enclosing declaration, which supplies that binding.
    fn hoistedTypeContext(self: *State, root: CIR.Expr.Idx, hash: *Sha256) Allocator.Error!bool {
        var pending = std.ArrayList(CIR.Expr.Idx).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(CIR.Expr.Idx, void).init(self.allocator);
        defer seen.deinit();
        var types = std.ArrayList(CheckedTypeId).empty;
        defer types.deinit(self.allocator);
        var lexical = false;
        try pending.append(self.allocator, root);
        while (pending.pop()) |expr| {
            if ((try seen.getOrPut(expr)).found_existing) continue;
            const expression = self.inputs.env.store.getExpr(expr);
            const id = self.inputs.bodies.exprIdForSource(expr) orelse
                base.invariant("hoisted expression has no published checked body", .{});
            const body = self.inputs.bodies.expr(id);
            hashBytes(hash, @tagName(expression));
            hashBytes(hash, @tagName(body.data));
            try types.append(self.allocator, body.ty);
            switch (expression) {
                .e_lookup_local => |lookup| if (!self.pattern_to_def.contains(lookup.pattern_idx)) {
                    lexical = true;
                },
                .e_closure => |closure| for (self.inputs.env.store.sliceCaptures(closure.captures)) |capture_id| {
                    const capture = self.inputs.env.store.getCapture(capture_id);
                    if (!self.pattern_to_def.contains(capture.pattern_idx)) lexical = true;
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
                .e_str_segment,
                .e_str,
                .e_bytes_literal,
                .e_lookup_external,
                .e_deferred_import_ref,
                .e_lookup_associated_local,
                .e_lookup_associated,
                .e_lookup_associated_resolved,
                .e_lookup_required,
                .e_list,
                .e_empty_list,
                .e_tuple,
                .e_match,
                .e_if,
                .e_call,
                .e_record,
                .e_empty_record,
                .e_block,
                .e_tag,
                .e_nominal,
                .e_nominal_external,
                .e_zero_argument_tag,
                .e_lambda,
                .e_binop,
                .e_unary_minus,
                .e_field_access,
                .e_method_call,
                .e_dispatch_call,
                .e_interpolation,
                .e_structural_eq,
                .e_structural_hash,
                .e_method_eq,
                .e_type_method_call,
                .e_type_dispatch_call,
                .e_tuple_access,
                .e_runtime_error,
                .e_crash,
                .e_dbg,
                .e_expect_err,
                .e_expect,
                .e_ellipsis,
                .e_anno_only,
                .e_derived_method,
                .e_return,
                .e_break,
                .e_for,
                .e_hosted_lambda,
                .e_run_low_level,
                => {},
            }
            try appendExprChildren(self.inputs.env, expr, &pending, self.allocator);
        }
        hash.update(&try self.type_walk.digestRoots(types.items));
        return lexical;
    }

    fn hashHoistedPattern(self: *const State, hash: *Sha256, pattern: CIR.Pattern.Idx) void {
        hashBytes(hash, self.regionText(self.inputs.env.store.getPatternRegion(pattern)));
    }

    fn rootIdentity(
        self: *State,
        root: CompileTimeRoot,
        context: [32]u8,
        type_world: [32]u8,
        ordinals: *std.AutoHashMapUnmanaged([32]u8, u32),
    ) Allocator.Error!CompileTimeRootIdentity {
        const env = self.inputs.env;
        const type_digest = try self.type_walk.digest(root.checked_type);
        var value = Sha256.init(.{});
        hashTag(&value, domain);
        value.update(&context);
        value.update(&type_world);
        hashBytes(&value, @tagName(root.kind));
        var site = Sha256.init(.{});
        hashTag(&site, "roc.compile_time_root_site.v1");
        hashBytes(&site, @tagName(root.kind));
        switch (root.source) {
            .def => |def_idx| {
                value.update(&self.definitionIdentity(def_idx));
                hashBytes(&site, self.defName(def_idx));
            },
            .statement => |statement| {
                const text = self.regionText(env.store.getStatementRegion(statement));
                hashBytes(&value, text);
                hashDigests(&value, try self.referencedIdentities(expectBody(env, statement)));
                if (self.inputs.test_dependencies) value.update(&try self.importedReferences(expectBody(env, statement)));
                hashBytes(&site, text);
            },
            .hoisted => |hoisted| {
                const owner = self.owners.get(hoisted.expr);
                var class = Sha256.init(.{});
                hashTag(&class, "hoisted");
                class.update(&self.ownerKey(owner));
                hashBytes(&class, @tagName(root.kind));
                const lexical = try self.hoistedTypeContext(hoisted.expr, &class);
                if (lexical) {
                    const resolved = owner orelse base.invariant("a hoisted lexical dependency has no enclosing declaration", .{});
                    switch (resolved) {
                        .def => |def_idx| class.update(&self.definitionIdentity(def_idx)),
                        .expect => |statement| {
                            hashBytes(&class, self.regionText(env.store.getStatementRegion(statement)));
                            hashDigests(&class, try self.referencedIdentities(expectBody(env, statement)));
                            if (self.inputs.test_dependencies) class.update(&try self.importedReferences(expectBody(env, statement)));
                        },
                        .type_default => |statement| hashBytes(&class, self.regionText(env.store.getStatementRegion(statement))),
                    }
                }
                hashBytes(&class, if (root.hoisted_body) |body| @tagName(body) else "no-body");
                if (root.hoisted_body) |body| switch (body) {
                    .expr => {},
                    .pattern_extraction => |extraction| {
                        self.hashHoistedPattern(&class, extraction.scrutinee_pattern);
                        self.hashHoistedPattern(&class, extraction.result_pattern);
                    },
                    .pattern_validation => |validation| self.hashHoistedPattern(&class, validation.scrutinee_pattern),
                    .pattern_error, .valueless_binding => |pattern| self.hashHoistedPattern(&class, pattern),
                };
                hashBytes(&class, self.regionText(env.store.getExprRegion(hoisted.expr)));
                hashDigests(&class, try self.referencedIdentities(hoisted.expr));
                class.update(&try self.importedReferences(hoisted.expr));
                class.update(&type_digest);
                const class_digest = class.finalResult();
                value.update(&class_digest);
                site.update(&class_digest);
                hashU32(&site, try self.nextOrdinal(ordinals, class_digest));
            },
            .expr => |expr| switch (root.kind) {
                // A literal argument is the literal of another module's
                // generic function at a type this module fixes: its
                // identity is the literal's and the type's.
                .literal_argument => {
                    const argument = root.literal_argument orelse base.invariant("value identity invariant violated: a literal argument root names no literal", .{});
                    var class = Sha256.init(.{});
                    hashTag(&class, "literal-argument");
                    if (self.inputs.test_dependencies) {
                        const operand = if (std.meta.eql(argument.site.module.bytes, self.inputs.own_key.bytes))
                            literalIdentity(env, self.inputs.bodies.view(), argument.site.expr)
                        else imported: {
                            const owner = self.type_walk.importedView(argument.site.module.bytes, true);
                            break :imported literalIdentity(owner.module_env, owner.checked_bodies, argument.site.expr);
                        };
                        class.update(&operand);
                    } else {
                        class.update(&argument.site.module.bytes);
                        hashU32(&class, @backingInt(argument.site.expr));
                    }
                    hashBytes(&class, @tagName(argument.site.kind));
                    class.update(&type_digest);
                    const class_digest = class.finalResult();
                    value.update(&class_digest);
                    value.update(&try self.typeMethodsDigest(root.checked_type));
                    site.update(&class_digest);
                    hashU32(&site, try self.nextOrdinal(ordinals, class_digest));
                },
                .numeral_conversion, .quote_conversion => {
                    const text = self.regionText(env.store.getExprRegion(expr));
                    hashBytes(&value, text);
                    value.update(&try self.literalTargetsDigest(@backingInt(expr)));
                    var class = Sha256.init(.{});
                    hashTag(&class, "literal");
                    class.update(&self.ownerKey(self.owners.get(expr)));
                    hashBytes(&class, @tagName(root.kind));
                    hashBytes(&class, text);
                    class.update(&type_digest);
                    const class_digest = class.finalResult();
                    site.update(&class_digest);
                    hashU32(&site, try self.nextOrdinal(ordinals, class_digest));
                },
                .repl_expr => {
                    hashBytes(&value, self.regionText(env.store.getExprRegion(expr)));
                    hashU32(&site, @backingInt(expr));
                },
                .constant, .hoisted_constant, .hoisted_validation, .callable_binding, .expect => base.invariant("compile-time root kind does not take an expression source", .{}),
            },
            .required_binding => |binding| {
                hashU32(&value, binding);
                hashU32(&site, binding);
            },
        }
        hashTag(&value, "type");
        value.update(&type_digest);
        return .{ .value = value.finalResult(), .site = site.finalResult() };
    }
};

/// Digests a checked type graph in one canonical pre-order: each node is
/// written on first visit with its child count, and a revisit writes the
/// node's first-visit position, so cyclic and shared graphs digest exactly.
/// The module's own nominal types are named by declaration rather than by
/// the module's content identity.
const TypeDigest = struct {
    allocator: Allocator,
    inputs: Inputs,
    visited: collections.DenseMap(CheckedTypeId, u32),
    /// The module's top-level type declarations, which the type world covers
    /// by source. A declaration in a block depends on its lexical context, so
    /// it is not one of them.
    top_level: collections.DenseMap(CIR.Statement.Idx, void),
    /// The digest of each top-level declaration's source text, computed the
    /// first time a type names that declaration. A declaration's text can be
    /// most of the module (every builtin type is nested in one), and every
    /// type that names it would otherwise hash all of it again.
    declaration_digests: collections.DenseMap(CIR.Statement.Idx, [32]u8),
    stack: std.ArrayList(CheckedTypeId) = .empty,
    declarations: []const DeclarationIdentity = &.{},

    fn init(allocator: Allocator, inputs: Inputs) TypeDigest {
        return .{
            .allocator = allocator,
            .inputs = inputs,
            .visited = collections.DenseMap(CheckedTypeId, u32).init(allocator),
            .top_level = collections.DenseMap(CIR.Statement.Idx, void).init(allocator),
            .declaration_digests = collections.DenseMap(CIR.Statement.Idx, [32]u8).init(allocator),
        };
    }

    fn deinit(self: *TypeDigest) void {
        self.visited.deinit();
        self.top_level.deinit();
        self.declaration_digests.deinit();
        self.stack.deinit(self.allocator);
    }

    fn digest(self: *TypeDigest, root: CheckedTypeId) Allocator.Error![32]u8 {
        return self.digestRoots(&.{root});
    }

    fn digestRoots(self: *TypeDigest, roots: []const CheckedTypeId) Allocator.Error![32]u8 {
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "checked-type");
        hashU32(&hasher, @intCast(roots.len));
        var root_index = roots.len;
        while (root_index > 0) {
            root_index -= 1;
            try self.stack.append(self.allocator, roots[root_index]);
        }
        var next_position: u32 = 0;
        // Children are pushed in reverse so they pop, and are written, in order.
        var children = std.ArrayList(CheckedTypeId).empty;
        defer children.deinit(self.allocator);
        while (self.stack.pop()) |ty| {
            if (self.visited.get(ty)) |position| {
                hashTag(&hasher, "seen");
                hashU32(&hasher, position);
                continue;
            }
            try self.visited.put(ty, next_position);
            next_position += 1;
            children.clearRetainingCapacity();
            try self.writeNode(&hasher, ty, &children);
            hashU32(&hasher, @intCast(children.items.len));
            var index = children.items.len;
            while (index > 0) {
                index -= 1;
                try self.stack.append(self.allocator, children.items[index]);
            }
        }
        return hasher.finalResult();
    }

    fn isOwn(self: *const TypeDigest, origin: canonical.ModuleIdentityId) bool {
        return std.meta.eql(self.inputs.names.moduleIdentityBytes(origin).*, self.inputs.own_identity);
    }

    fn importedView(self: *const TypeDigest, identity: [32]u8, comptime by_artifact: bool) checked_artifact.ImportedModuleView {
        for (self.inputs.imports) |entry| {
            const key = if (by_artifact) entry.view.key.bytes else entry.view.module_identity.stable_hash;
            if (std.meta.eql(key, identity)) return entry.view;
        }
        for (self.inputs.available) |view| {
            const key = if (by_artifact) view.key.bytes else view.module_identity.stable_hash;
            if (std.meta.eql(key, identity)) return view;
        }
        for (self.inputs.relations) |view| {
            const key = if (by_artifact) view.key.bytes else view.module_identity.stable_hash;
            if (std.meta.eql(key, identity)) return view;
        }
        base.invariant("checked test type has no declaring imported module", .{});
    }

    fn writeImportedDeclaration(_: *const TypeDigest, hasher: *Sha256, view: checked_artifact.ImportedModuleView, declaration: u32) void {
        const summary = view.compile_time_root_identities;
        hashTag(hasher, "imported-test-type");
        hasher.update(&summary.test_context[0]);
        hasher.update(&declarationIdentity(summary.test_declarations, declaration));
    }

    /// Names a declaration of the module: a top-level one by its name and
    /// source, any other by version-specific data.
    fn writeOwnDeclaration(self: *TypeDigest, hasher: *Sha256, name: []const u8, source_decl: ?u32) Allocator.Error!void {
        if (source_decl) |raw| {
            if (self.inputs.test_dependencies) {
                hashTag(hasher, "own-test-type");
                hashBytes(hasher, name);
                hasher.update(&declarationIdentity(self.declarations, raw));
                return;
            }
            const statement: CIR.Statement.Idx = @fromBackingInt(raw);
            if (self.top_level.contains(statement)) {
                hashTag(hasher, "own-top-level");
                hashBytes(hasher, name);
                hasher.update(&try self.declarationDigest(statement));
                return;
            }
        }
        hashTag(hasher, "own-version");
        hasher.update(&self.inputs.own_identity);
        hashBytes(hasher, name);
        hashU32(hasher, source_decl orelse std.math.maxInt(u32));
    }

    /// The digest of a top-level declaration's source text.
    fn declarationDigest(self: *TypeDigest, statement: CIR.Statement.Idx) Allocator.Error![32]u8 {
        const entry = try self.declaration_digests.getOrPut(statement);
        if (entry.found_existing) return entry.value_ptr.*;
        const env = self.inputs.env;
        const region = env.store.getStatementRegion(statement);
        var hasher = Sha256.init(.{});
        hashTag(&hasher, "declaration-source");
        hashBytes(&hasher, env.getSourceAll()[region.start.offset..region.end.offset]);
        entry.value_ptr.* = hasher.finalResult();
        return entry.value_ptr.*;
    }

    fn writeNode(self: *TypeDigest, hasher: *Sha256, ty: CheckedTypeId, children: *std.ArrayList(CheckedTypeId)) Allocator.Error!void {
        const names = self.inputs.names;
        switch (self.inputs.types.payload(ty)) {
            .pending => hashTag(hasher, "pending"),
            .err => hashTag(hasher, "err"),
            .empty_record => hashTag(hasher, "empty-record"),
            .empty_tag_union => hashTag(hasher, "empty-tag-union"),
            inline .flex, .rigid => |variable, tag| {
                hashTag(hasher, @tagName(tag));
                hashBytes(hasher, variable.name orelse "");
                hashValue(hasher, variable.numeric_default_phase);
                hashValue(hasher, variable.row_default);
                hashValue(hasher, variable.row_default_discharges_constraints);
                hashValue(hasher, variable.bounded_row_ext);
                hashU32(hasher, @intCast(variable.constraints.len));
                for (variable.constraints) |constraint| {
                    hashBytes(hasher, names.methodNameText(constraint.fn_name));
                    hashValue(hasher, constraint.origin);
                    try children.append(self.allocator, constraint.fn_ty);
                }
            },
            .alias => |alias| {
                hashTag(hasher, "alias");
                const name = names.typeNameText(alias.name);
                if (self.isOwn(alias.origin_module)) {
                    try self.writeOwnDeclaration(hasher, name, alias.source_decl);
                } else if (self.inputs.test_dependencies and alias.source_decl != null) {
                    hashBytes(hasher, name);
                    self.writeImportedDeclaration(hasher, self.importedView(names.moduleIdentityBytes(alias.origin_module).*, false), alias.source_decl.?);
                } else {
                    hasher.update(names.moduleIdentityBytes(alias.origin_module));
                    hashBytes(hasher, name);
                    hashU32(hasher, alias.source_decl orelse std.math.maxInt(u32));
                }
                hashValue(hasher, alias.builtin_origin);
                try children.appendSlice(self.allocator, alias.args);
                try children.append(self.allocator, alias.backing);
            },
            .nominal => |nominal| {
                hashTag(hasher, "nominal");
                const name = names.typeNameText(nominal.name);
                if (self.isOwn(nominal.origin_module)) {
                    try self.writeOwnDeclaration(hasher, name, nominal.source_decl);
                } else if (self.inputs.test_dependencies) {
                    hashBytes(hasher, name);
                    self.writeImportedDeclaration(hasher, self.importedView(names.moduleIdentityBytes(nominal.origin_module).*, false), nominal.source_decl);
                } else {
                    hasher.update(names.moduleIdentityBytes(nominal.origin_module));
                    hashBytes(hasher, name);
                    hashU32(hasher, nominal.source_decl);
                }
                hashValue(hasher, nominal.builtin);
                hashValue(hasher, nominal.is_opaque);
                hashBytes(hasher, @tagName(nominal.representation));
                switch (nominal.representation) {
                    .builtin => |builtin_nominal| hashBytes(hasher, @tagName(builtin_nominal)),
                    .imported_declaration => |ref| {
                        if (self.inputs.test_dependencies) {
                            const owner = self.importedView(ref.artifact.bytes, true);
                            const declaration = owner.checked_types.nominal_declarations[@backingInt(ref.declaration)];
                            self.writeImportedDeclaration(hasher, owner, declaration.source_statement);
                        } else {
                            hasher.update(&ref.artifact.bytes);
                            hashU32(hasher, @backingInt(ref.declaration));
                        }
                    },
                    .imported_box_payload_capability => |ref| {
                        hasher.update(&ref.artifact.bytes);
                        hashU32(hasher, @backingInt(ref.capability));
                    },
                    // Own declarations are named above by their source.
                    .local_declaration, .local_box_payload_capability, .opaque_without_backing => {},
                }
                hashU32(hasher, @intCast(nominal.declared_fields.len));
                for (nominal.declared_fields) |field| switch (field) {
                    .named => |label| {
                        hashTag(hasher, "named");
                        hashBytes(hasher, names.recordFieldLabelText(label));
                    },
                    .padding => |slot| {
                        hashTag(hasher, "padding");
                        hashU32(hasher, slot);
                    },
                };
                hashU32(hasher, @intCast(nominal.args.len));
                try children.appendSlice(self.allocator, nominal.args);
                try children.appendSlice(self.allocator, nominal.padding_field_types);
            },
            .record => |record| {
                hashTag(hasher, "record");
                hashU32(hasher, @intCast(record.fields.len));
                for (record.fields) |field| {
                    hashBytes(hasher, names.recordFieldLabelText(field.name));
                    hashBytes(hasher, @tagName(field.kind.tag));
                    if (field.kind.defaultIdentity()) |default| {
                        if (default.origin()) |origin| {
                            if (self.isOwn(origin)) {
                                hashTag(hasher, "own-default");
                                hasher.update(&self.inputs.own_identity);
                            } else {
                                hasher.update(names.moduleIdentityBytes(origin));
                            }
                        }
                        hashU32(hasher, default.expr_node);
                    }
                    try children.append(self.allocator, field.ty);
                    if (field.kind.undeterminedVariable()) |variable| try children.append(self.allocator, variable);
                }
                try children.append(self.allocator, record.ext);
            },
            .tuple => |items| {
                hashTag(hasher, "tuple");
                try children.appendSlice(self.allocator, items);
            },
            .function => |function| {
                hashTag(hasher, "function");
                hashBytes(hasher, @tagName(function.kind));
                hashU32(hasher, @intCast(function.args.len));
                try children.appendSlice(self.allocator, function.args);
                try children.append(self.allocator, function.ret);
            },
            .tag_union => |tag_union| {
                hashTag(hasher, "tag-union");
                hashU32(hasher, @intCast(tag_union.tags.len));
                for (tag_union.tags) |tag| {
                    const args = tag.argsSlice(self.inputs.types);
                    hashBytes(hasher, names.tagLabelText(tag.name));
                    hashU32(hasher, @intCast(args.len));
                    try children.appendSlice(self.allocator, args);
                }
                try children.append(self.allocator, tag_union.ext);
            },
        }
    }
};

/// Whether `statement` declares a named type.
fn declaresType(statement: CIR.Statement) bool {
    return switch (statement) {
        .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_var_alias => true,
        .s_decl, .s_var, .s_var_uninitialized, .s_reassign, .s_crash, .s_dbg, .s_expr, .s_expect, .s_for, .s_while, .s_infinite_loop, .s_breakable_loop, .s_break, .s_return, .s_import, .s_type_anno, .s_runtime_error => false,
    };
}

fn declarationIdentity(rows: []const DeclarationIdentity, raw: u32) [32]u8 {
    var lo: usize = 0;
    var hi = rows.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        const node = @backingInt(rows[mid].statement);
        if (node < raw) lo = mid + 1 else if (node > raw) hi = mid else return rows[mid].value;
    }
    base.invariant("checked test type has no producer-authored declaration identity", .{});
}

fn expectBody(env: *const ModuleEnv, statement: CIR.Statement.Idx) CIR.Expr.Idx {
    const data = env.store.getStatement(statement);
    if (data != .s_expect) base.invariant("compile-time statement root was not an expect", .{});
    return data.s_expect.body;
}

/// Push every direct subexpression of `expr`.
fn appendExprChildren(env: *const ModuleEnv, expr_idx: CIR.Expr.Idx, stack: *std.ArrayList(CIR.Expr.Idx), allocator: Allocator) Allocator.Error!void {
    const store = &env.store;
    switch (store.getExpr(expr_idx)) {
        .e_type_method_call => |call| try stack.appendSlice(allocator, store.sliceExpr(call.args)),
        .e_type_dispatch_call => |call| try stack.appendSlice(allocator, store.sliceExpr(call.args)),
        .e_lambda => |lambda| try stack.append(allocator, lambda.body),
        .e_closure => |closure| try stack.append(allocator, closure.lambda_idx),
        .e_call => |call| {
            try stack.append(allocator, call.func);
            try stack.appendSlice(allocator, store.sliceExpr(call.args));
        },
        .e_if => |if_expr| {
            try stack.append(allocator, if_expr.final_else);
            for (store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                const branch = store.getIfBranch(branch_idx);
                try stack.append(allocator, branch.cond);
                try stack.append(allocator, branch.body);
            }
        },
        .e_match => |match_expr| {
            try stack.append(allocator, match_expr.cond);
            for (store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                const branch = store.getMatchBranch(branch_idx);
                if (branch.guard) |guard_idx| try stack.append(allocator, guard_idx);
                try stack.append(allocator, branch.value);
            }
        },
        .e_list => |list| try stack.appendSlice(allocator, store.sliceExpr(list.elems)),
        .e_record => |record| {
            if (record.ext) |ext_idx| try stack.append(allocator, ext_idx);
            for (store.sliceRecordFields(record.fields)) |field_idx| try stack.append(allocator, store.getRecordField(field_idx).value);
        },
        .e_field_access => |access| try stack.append(allocator, access.receiver),
        inline .e_method_call, .e_dispatch_call => |call| {
            try stack.append(allocator, call.receiver);
            try stack.appendSlice(allocator, store.sliceExpr(call.args));
        },
        .e_interpolation => |interpolation| {
            try stack.append(allocator, interpolation.first);
            try stack.appendSlice(allocator, store.sliceExpr(interpolation.parts));
        },
        .e_structural_eq => |eq| {
            try stack.append(allocator, eq.lhs);
            try stack.append(allocator, eq.rhs);
        },
        .e_structural_hash => |h| {
            try stack.append(allocator, h.hasher);
            try stack.append(allocator, h.value);
        },
        .e_method_eq => |eq| {
            try stack.append(allocator, eq.lhs);
            try stack.append(allocator, eq.rhs);
        },
        .e_tuple_access => |tuple_access| try stack.append(allocator, tuple_access.tuple),
        .e_tuple => |tuple| try stack.appendSlice(allocator, store.sliceExpr(tuple.elems)),
        .e_binop => |binop| {
            try stack.append(allocator, binop.lhs);
            try stack.append(allocator, binop.rhs);
        },
        .e_unary_minus => |unop| try stack.append(allocator, unop.expr),
        .e_block => |block| {
            for (store.sliceStatements(block.stmts)) |stmt_idx| {
                switch (store.getStatement(stmt_idx)) {
                    .s_decl => |decl| try stack.append(allocator, decl.expr),
                    .s_var => |var_stmt| try stack.append(allocator, var_stmt.expr),
                    .s_var_uninitialized => {},
                    inline .s_reassign, .s_dbg, .s_expr => |statement| try stack.append(allocator, statement.expr),
                    .s_expect => |expect| try stack.append(allocator, expect.body),
                    .s_for => |for_stmt| {
                        try stack.append(allocator, for_stmt.expr);
                        try stack.append(allocator, for_stmt.body);
                    },
                    .s_while => |while_stmt| {
                        try stack.append(allocator, while_stmt.cond);
                        try stack.append(allocator, while_stmt.body);
                    },
                    .s_infinite_loop => |loop_stmt| {
                        try stack.append(allocator, loop_stmt.cond);
                        try stack.append(allocator, loop_stmt.body);
                    },
                    .s_breakable_loop => |loop_stmt| {
                        try stack.append(allocator, loop_stmt.cond);
                        try stack.append(allocator, loop_stmt.body);
                    },
                    .s_return => |ret| try stack.append(allocator, ret.expr),
                    .s_runtime_error => |runtime_error| try stack.appendSlice(allocator, store.sliceExpr(runtime_error.evaluated)),
                    .s_import, .s_alias_decl, .s_nominal_decl, .s_where_alias_decl, .s_type_anno, .s_type_var_alias, .s_crash, .s_break => {},
                }
            }
            try stack.append(allocator, block.final_expr);
        },
        .e_tag => |tag| try stack.appendSlice(allocator, store.sliceExpr(tag.args)),
        .e_nominal => |nominal| try stack.append(allocator, nominal.backing_expr),
        .e_nominal_external => |nominal| try stack.append(allocator, nominal.backing_expr),
        .e_deferred_import_ref => |deferred| {
            if (deferred.backing) |backing| try stack.append(allocator, backing.expr);
        },
        .e_run_low_level => |run_ll| try stack.appendSlice(allocator, store.sliceExpr(run_ll.args)),
        .e_dbg => |dbg| try stack.append(allocator, dbg.expr),
        .e_expect_err => |expect_err| try stack.append(allocator, expect_err.expr),
        .e_expect => |expect| try stack.append(allocator, expect.body),
        // `ret.lambda` refers back to the enclosing lambda; it is no child.
        .e_return => |ret| try stack.append(allocator, ret.expr),
        .e_for => |for_expr| {
            try stack.append(allocator, for_expr.expr);
            try stack.append(allocator, for_expr.body);
        },
        .e_runtime_error => |runtime_error| try stack.appendSlice(allocator, store.sliceExpr(runtime_error.evaluated)),
        .e_lookup_local,
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
    }
}

fn hashTag(hasher: *Sha256, tag: []const u8) void {
    hashBytes(hasher, tag);
}

fn hashU32(hasher: *Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

fn hashBytes(hasher: *Sha256, bytes: []const u8) void {
    hashU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn hashDigests(hasher: *Sha256, digests: []const [32]u8) void {
    hashU32(hasher, @intCast(digests.len));
    for (digests) |digest| hasher.update(&digest);
}

fn digestBytes(tag: []const u8, bytes: []const u8) [32]u8 {
    var hasher = Sha256.init(.{});
    hashTag(&hasher, tag);
    hashBytes(&hasher, bytes);
    return hasher.finalResult();
}

fn sortDigests(digests: [][32]u8) void {
    std.mem.sort([32]u8, digests, {}, struct {
        fn lessThan(_: void, a: [32]u8, b: [32]u8) bool {
            return std.mem.order(u8, &a, &b) == .lt;
        }
    }.lessThan);
}

/// Hash a plain value by its logical contents, field by field, so padding
/// never reaches the digest. Source regions are positions, not identity.
fn hashValue(hasher: *Sha256, value: anytype) void {
    const T = @TypeOf(value);
    if (T == base.Region) return;
    switch (@typeInfo(T)) {
        .bool => hasher.update(&.{@intFromBool(value)}),
        .int => |info| {
            const Wide = @Int(.unsigned, ((info.bits + 7) / 8) * 8);
            var bytes: [@sizeOf(Wide)]u8 = undefined;
            std.mem.writeInt(Wide, &bytes, @bitCast(@as(@Int(info.signedness, @bitSizeOf(Wide)), value)), .little);
            hasher.update(&bytes);
        },
        .@"enum" => hashBytes(hasher, @tagName(value)),
        .array => for (value) |item| hashValue(hasher, item),
        .optional => if (value) |inner| {
            hasher.update(&.{1});
            hashValue(hasher, inner);
        } else hasher.update(&.{0}),
        .@"struct" => |info| {
            if (info.backing_integer) |Backing| {
                hashValue(hasher, @as(Backing, @bitCast(value)));
                return;
            }
            inline for (info.field_names) |name| hashValue(hasher, @field(value, name));
        },
        .@"union" => |info| {
            hashBytes(hasher, @tagName(value));
            inline for (info.field_names) |name| {
                if (std.meta.activeTag(value) == @field(std.meta.Tag(@TypeOf(value)), name)) hashValue(hasher, @field(value, name));
            }
        },
        .void => {},
        .type,
        .noreturn,
        .float,
        .pointer,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("value identity cannot hash " ++ @typeName(T)),
    }
}
