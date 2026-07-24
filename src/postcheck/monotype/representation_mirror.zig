//! Graph-driven shadow of the representation closure engine (reunify.md section
//! 10, Slice 7 Stage B).
//!
//! The Monotype instantiation graph owns production representation decisions:
//! iterator tier joins, the sanctioned nominal-backing relations, and the
//! generated-evidence selections. This module makes the graph SHADOW-DRIVE the
//! separate `representation_closure` engine on the live corpus: wherever the
//! graph applies one of those decisions, the same relation is mirrored into
//! engine slots; when the graph's specialization seals, the engine's sealed
//! representation descriptor (tier/kind/depth/owner) is compared against the
//! graph-sealed node's representation content and the agreement is censused.
//!
//! This is a verifier that cannot select compiler behavior. It is compiled out
//! unless `census.enabled` and turned on only by `ROC_REUNIFY_SHADOW`. It owns
//! its own engine, slot map, and logical-token table; it never writes to the
//! type store, name store, or any authoritative registry, and it never
//! allocates a store id — descriptor comparison reads live graph node content
//! and engine slot state only. Every internal failure (allocation, a slot shape
//! the engine cannot model, a logically-unequal operand) is caught and measured
//! or disables the mirror; nothing here ever aborts lowering.

const std = @import("std");
const Allocator = std.mem.Allocator;

const check = @import("check");
const Type = @import("type.zig");
const census = @import("census.zig");
const solve = @import("solve.zig");
const closure = @import("../representation_closure.zig");
const policy = @import("../representation_policy.zig");
const reunify_shadow = @import("../reunify_shadow/shadow.zig");

const static_dispatch = check.StaticDispatchRegistry;
const names = check.CheckedNames;
const InstGraph = solve.InstGraph;
const NodeId = solve.NodeId;
const InstNamed = solve.InstNamed;

/// The maximum depth `slotForNode` builds representation slots before treating a
/// node as an opaque leaf. Representation-carrying spines (iterators, evidence,
/// nominal wrappers, box/list) are shallow; this only bounds pathological input.
const max_slot_depth: u32 = 64;

/// The rule a mirrored relation cited, remembered per participating node so the
/// seal comparison can tag its match/mismatch by rule.
const SiteRule = enum {
    iterator_public_minted,
    iterator_forced_dynamic,
    iterator_minted_join,
    generated_evidence_selection,
    nominal_backing,
};

/// One reserved argument/result interface position for the in-flight
/// specialization (reunify.md section 11.1). `tier` is the representation tier
/// the requested type declared before body lowering; `gained` records whether
/// the live position moved to a higher tier during body discovery.
const InterfacePosition = struct {
    tier: Type.IteratorRepresentation,
    node: NodeId,
};

/// Mirrors the instantiation graph's representation decisions into the closure
/// engine so the engine's sealed descriptors can be compared against the
/// graph's. The graph stays the authority; this sidecar only observes.
pub const RepresentationMirror = struct {
    allocator: Allocator,
    graph: *InstGraph,
    engine: closure.Engine,
    /// Root graph node -> the engine slot modelling its representation. Keyed by
    /// the union-find root so one equivalence class maps to one slot; when the
    /// graph later merges two classes the merged relation is mirrored into the
    /// engine at the decision site.
    node_slots: std.AutoHashMapUnmanaged(NodeId, closure.RepresentationSlotId),
    /// Root graph nodes currently being built into a slot, to break backing
    /// cycles (the issue-10170 recursive `rest` shape) with an opaque leaf.
    in_progress: std.AutoHashMapUnmanaged(NodeId, void),
    /// The rule that related each graph-decision node, for the seal comparison.
    site_rules: std.AutoHashMapUnmanaged(NodeId, SiteRule),
    /// Interned logical-skeleton digests -> dense token. The engine refuses to
    /// relate two slots with unequal tokens, so a token distinguishes logical
    /// identity while erasing representation.
    logical_tokens: std.AutoHashMapUnmanaged([32]u8, u64),
    next_token: u64,
    next_producer: u32,
    /// Reserved interface positions for the specialization currently lowering.
    interface: std.ArrayListUnmanaged(InterfacePosition),
    interface_recursive: bool,
    /// Any internal error disables the mirror so it can never affect lowering.
    disabled: bool,

    /// Create a mirror for `graph` when the shadow is compiled in and enabled,
    /// otherwise null. The caller stores the pointer on the graph.
    pub fn maybeCreate(graph: *InstGraph) ?*RepresentationMirror {
        if (comptime !census.enabled) return null;
        if (!reunify_shadow.shouldRun()) return null;
        return create(graph) catch null;
    }

    /// Build a mirror unconditionally. `maybeCreate` gates this behind the
    /// compile-in and env checks; tests drive it directly.
    pub fn create(graph: *InstGraph) Allocator.Error!*RepresentationMirror {
        const self = try graph.allocator.create(RepresentationMirror);
        self.* = .{
            .allocator = graph.allocator,
            .graph = graph,
            .engine = closure.Engine.init(graph.allocator),
            .node_slots = .empty,
            .in_progress = .empty,
            .site_rules = .empty,
            .logical_tokens = .empty,
            .next_token = 1,
            .next_producer = 1,
            .interface = .empty,
            .interface_recursive = false,
            .disabled = false,
        };
        return self;
    }

    /// The engine slot modelling a node's representation, if one exists. For
    /// tests and the seal comparison.
    pub fn slotOf(self: *RepresentationMirror, node: NodeId) ?closure.RepresentationSlotId {
        return self.node_slots.get(self.graph.rootOf(node));
    }

    pub fn destroy(self: *RepresentationMirror) void {
        self.engine.deinit();
        self.node_slots.deinit(self.allocator);
        self.in_progress.deinit(self.allocator);
        self.site_rules.deinit(self.allocator);
        self.logical_tokens.deinit(self.allocator);
        self.interface.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    // --- Decision-site hooks (reunify.md section 10.3) ---

    /// Mirror an iterator tier join the graph classified (`applyIteratorJoin`).
    /// Builds an iterator slot for each operand and relates them under the rule
    /// matching the policy's tier decision.
    pub fn mirrorIteratorJoin(
        self: *RepresentationMirror,
        left: NodeId,
        right: NodeId,
        join: policy.IteratorJoin,
    ) void {
        if (self.disabled) return;
        const rule: closure.RepresentationRule = switch (join.relation) {
            .public_minted => .iterator_public_minted,
            .forced_dynamic => .iterator_forced_dynamic,
            .minted_join => .iterator_minted_join,
            .ordinary => return,
        };
        const site: SiteRule = switch (join.relation) {
            .public_minted => .iterator_public_minted,
            .forced_dynamic => .iterator_forced_dynamic,
            .minted_join => .iterator_minted_join,
            .ordinary => return,
        };
        self.relateNodes(left, right, rule, site);
    }

    /// Mirror the sanctioned nominal-backing relation: two equal-identity
    /// nominals whose backings the graph relates (reunify.md section 10.3,
    /// distinct from the dying generic head-mismatch path). Both operands build
    /// into nominal wrapper slots; a component-equality relation joins the two
    /// wrappers, which relates their backing children.
    pub fn nominalBackingRelated(self: *RepresentationMirror, left: NodeId, right: NodeId) void {
        if (self.disabled) return;
        census.bump("representation_mirror_nominal_backing_related");
        self.relateNodes(left, right, .component_equality, .nominal_backing);
    }

    /// Mirror a generated-evidence selection: two equal-identity generated
    /// evidence owners whose backing the graph selects rather than relates.
    pub fn evidenceSelection(self: *RepresentationMirror, left: NodeId, right: NodeId) void {
        if (self.disabled) return;
        self.relateNodes(left, right, .generated_evidence_selection, .generated_evidence_selection);
    }

    fn relateNodes(
        self: *RepresentationMirror,
        left: NodeId,
        right: NodeId,
        rule: closure.RepresentationRule,
        site: SiteRule,
    ) void {
        const left_slot = self.slotForNode(left, 0) orelse return self.fail();
        const right_slot = self.slotForNode(right, 0) orelse return self.fail();
        self.site_rules.put(self.allocator, self.graph.rootOf(left), site) catch return self.fail();
        self.site_rules.put(self.allocator, self.graph.rootOf(right), site) catch return self.fail();
        self.engine.relate(left_slot, right_slot, rule) catch |err| switch (err) {
            error.LogicallyUnequal => census.bump("representation_mirror_relate_rejected"),
            else => self.fail(),
        };
    }

    // --- Interface reservation trial (reunify.md section 11.1) ---

    /// Reserve argument and result representation slots for the specialization
    /// about to lower `root_fn`, before its body is discovered. Records each
    /// position's declared representation tier so the post-body measurement can
    /// see which reserved positions gained information.
    pub fn reserveInterface(self: *RepresentationMirror, root_fn: NodeId) void {
        if (self.disabled) return;
        self.interface.clearRetainingCapacity();
        self.interface_recursive = false;
        const content = self.graph.content(root_fn);
        const func = switch (content) {
            .func => |func| func,
            else => return,
        };
        for (func.args) |arg| self.reservePosition(arg);
        self.reservePosition(func.ret);
    }

    fn reservePosition(self: *RepresentationMirror, node: NodeId) void {
        // Reserving a slot exercises the reservation API and gives the count a
        // slot, even for representation-neutral positions.
        _ = self.slotForNode(node, 0) orelse return self.fail();
        self.interface.append(self.allocator, .{
            .tier = self.tierOf(node),
            .node = node,
        }) catch return self.fail();
        census.bump("interface_slots_reserved");
    }

    /// Note that the specialization currently lowering made a recursive
    /// self-request, so its interface gains are not the non-recursive openness
    /// the section 11 measurement isolates.
    pub fn markInterfaceRecursive(self: *RepresentationMirror) void {
        if (self.disabled) return;
        self.interface_recursive = true;
    }

    /// After body discovery, measure which reserved interface positions moved to
    /// a higher representation tier. Reads live graph node content only.
    pub fn measureInterfaceGain(self: *RepresentationMirror) void {
        if (self.disabled) return;
        for (self.interface.items) |position| {
            const live = self.tierOf(position.node);
            if (tierRank(live) > tierRank(position.tier)) {
                census.bump("interface_slots_gained_info");
                if (!self.interface_recursive) census.bump("gained_info_nonrecursive");
            }
        }
        self.interface.clearRetainingCapacity();
    }

    fn tierOf(self: *RepresentationMirror, node: NodeId) Type.IteratorRepresentation {
        return switch (self.graph.content(node)) {
            .named => |named| named.def.iterator_representation,
            else => .none,
        };
    }

    // --- Seal and compare (reunify.md section 10.6) ---

    /// Seal the mirrored component and compare, for every graph-decision node,
    /// the engine's sealed representation descriptor against the graph node's
    /// final representation content. Called once the graph's work is done.
    pub fn sealAndCompare(self: *RepresentationMirror) void {
        if (self.disabled) return;
        var compared: std.AutoHashMapUnmanaged(closure.RepresentationSlotId, void) = .empty;
        defer compared.deinit(self.allocator);

        var it = self.site_rules.iterator();
        while (it.next()) |entry| {
            const node = entry.key_ptr.*;
            const site = entry.value_ptr.*;
            const slot = self.node_slots.get(self.graph.rootOf(node)) orelse continue;
            const representative = self.engine.find(slot);
            const gop = compared.getOrPut(self.allocator, representative) catch return self.fail();
            if (gop.found_existing) continue;
            self.compareOne(node, site);
        }
    }

    fn compareOne(self: *RepresentationMirror, node: NodeId, site: SiteRule) void {
        switch (site) {
            .iterator_public_minted, .iterator_forced_dynamic, .iterator_minted_join => {
                self.compareIterator(node, site);
            },
            // A component-equality relation on nominal wrappers agrees by having
            // been applied; there is no tier descriptor on a wrapper slot.
            .nominal_backing => {
                census.bump("representation_mirror_match");
            },
            // Evidence selection agrees by having been applied; the graph keeps
            // one backing rather than joining a tier, so there is no descriptor.
            .generated_evidence_selection => {
                census.bump("representation_mirror_match");
                census.bump("representation_mirror_match_evidence");
            },
        }
    }

    fn compareIterator(self: *RepresentationMirror, node: NodeId, site: SiteRule) void {
        const root = self.graph.rootOf(node);
        const slot = self.node_slots.get(root) orelse return;
        const engine_shape = self.engine.shapeOf(slot);
        const engine_descriptor = switch (engine_shape) {
            .iterator => |iter| iter.descriptor,
            else => {
                self.bumpMismatch(site);
                return;
            },
        };
        const graph_named = switch (self.graph.content(root)) {
            .named => |named| named,
            else => {
                self.bumpMismatch(site);
                return;
            },
        };
        if (descriptorsAgree(graph_named, engine_descriptor)) {
            self.bumpMatch(site);
        } else {
            self.bumpMismatch(site);
        }
    }

    fn bumpMatch(_: *RepresentationMirror, site: SiteRule) void {
        census.bump("representation_mirror_match");
        switch (site) {
            .iterator_public_minted => census.bump("representation_mirror_match_public_minted"),
            .iterator_forced_dynamic => census.bump("representation_mirror_match_forced_dynamic"),
            .iterator_minted_join => census.bump("representation_mirror_match_minted_join"),
            .generated_evidence_selection => census.bump("representation_mirror_match_evidence"),
            .nominal_backing => {},
        }
    }

    fn bumpMismatch(_: *RepresentationMirror, site: SiteRule) void {
        census.bump("representation_mirror_mismatch");
        switch (site) {
            .iterator_public_minted => census.bump("representation_mirror_mismatch_public_minted"),
            .iterator_forced_dynamic => census.bump("representation_mirror_mismatch_forced_dynamic"),
            .iterator_minted_join => census.bump("representation_mirror_mismatch_minted_join"),
            .generated_evidence_selection => census.bump("representation_mirror_mismatch_evidence"),
            .nominal_backing => {},
        }
    }

    // --- Slot construction ---

    /// Build (memoized) the engine slot for a graph node's representation, keyed
    /// by its union-find root. Returns null on allocation failure so the caller
    /// disables the mirror. A backing cycle returns a fresh opaque leaf.
    fn slotForNode(self: *RepresentationMirror, node: NodeId, depth: u32) ?closure.RepresentationSlotId {
        const root = self.graph.rootOf(node);
        if (self.node_slots.get(root)) |existing| return existing;
        if (depth >= max_slot_depth or self.in_progress.contains(root)) {
            return self.freshLeaf(root);
        }
        self.in_progress.put(self.allocator, root, {}) catch return null;
        defer _ = self.in_progress.remove(root);

        const shape = self.buildShape(root, depth) orelse return null;
        const token = self.tokenForNode(root) orelse return null;
        const producer = self.freshProducer();
        const slot = self.engine.createSlot(token, producer, shape) catch return null;
        self.node_slots.put(self.allocator, root, slot) catch return null;
        return slot;
    }

    fn buildShape(self: *RepresentationMirror, root: NodeId, depth: u32) ?closure.SlotShape {
        switch (self.graph.content(root)) {
            .named => |named| return self.buildNamedShape(root, named, depth),
            .box, .list => |elem| {
                const child = self.slotForNode(elem, depth + 1) orelse return null;
                return .{ .wrapper = child };
            },
            else => {
                const token = self.tokenForNode(root) orelse return null;
                return .{ .leaf = @intFromEnum(token) };
            },
        }
    }

    fn buildNamedShape(
        self: *RepresentationMirror,
        root: NodeId,
        named: InstNamed,
        depth: u32,
    ) ?closure.SlotShape {
        const owner = named.builtin_owner;
        if (owner != null and static_dispatch.isIteratorOwner(owner.?) and named.args.len >= 1) {
            const item = self.slotForNode(named.args[0], depth + 1) orelse return null;
            // A public iterator carries no explicit backing node; model it with a
            // placeholder leaf so the operand still builds as an iterator slot and
            // the engine runs the tier relation instead of refusing the join.
            const backing = if (named.backing) |backing_node|
                (self.slotForNode(backing_node.node, depth + 1) orelse return null)
            else
                (self.placeholderBacking() orelse return null);
            return .{ .iterator = .{
                .descriptor = descriptorOf(named),
                .item = item,
                .backing = backing,
            } };
        }
        if (owner != null and owner.? == .fields and named.backing != null) {
            // The graph selects one `FieldNames` backing rather than relating the
            // two (its equal-def `.fields` skip); an evidence slot models that.
            // Other score-selection owners (`FieldName`, `ParseTagUnionSpec`) fall
            // through: the graph relates their backings, so they build as wrappers
            // and the mirror replays that relation. The graph carries no score and
            // Slice 0 measured no equal-score tie, so a single declared score
            // suffices for the engine's selection rule.
            return .{ .evidence = .{ .score = 0 } };
        }
        if (named.backing) |backing| {
            const child = self.slotForNode(backing.node, depth + 1) orelse return null;
            return .{ .wrapper = child };
        }
        const token = self.tokenForNode(root) orelse return null;
        return .{ .leaf = @intFromEnum(token) };
    }

    fn freshLeaf(self: *RepresentationMirror, root: NodeId) ?closure.RepresentationSlotId {
        const token = self.tokenForNode(root) orelse return null;
        return self.engine.createSlot(token, self.freshProducer(), .{ .leaf = @intFromEnum(token) }) catch return null;
    }

    /// A shared stand-in backing leaf for a backing-less iterator. It carries
    /// the engine's `stand_in` token, which `tokenForNode` (numbering from 1)
    /// never mints, so all stand-in backings share one token: a minted-join backing
    /// relation between two backing-less iterators still relates them, while
    /// public-and-minted keeps them separate as its rule already dictates.
    fn placeholderBacking(self: *RepresentationMirror) ?closure.RepresentationSlotId {
        return self.engine.createSlot(.stand_in, self.freshProducer(), .{ .leaf = 0 }) catch null;
    }

    fn freshProducer(self: *RepresentationMirror) closure.ProducerAtom {
        const atom: closure.ProducerAtom = @enumFromInt(self.next_producer);
        self.next_producer +%= 1;
        return atom;
    }

    fn tokenForNode(self: *RepresentationMirror, root: NodeId) ?closure.LogicalToken {
        const digest = self.logicalDigest(root);
        const gop = self.logical_tokens.getOrPut(self.allocator, digest) catch return null;
        if (!gop.found_existing) {
            gop.value_ptr.* = self.next_token;
            self.next_token +%= 1;
        }
        return @enumFromInt(gop.value_ptr.*);
    }

    /// A 32-byte digest of a node's head identity, erasing representation
    /// content. It captures only the node's own shape and declared identity, not
    /// its children: the engine refuses to relate two slots with unequal tokens,
    /// and the graph relates two nodes at these sites precisely when they are
    /// logically equal — but their children are unified separately and are not
    /// yet structurally identical at relate time, so recursing into them would
    /// spuriously distinguish operands the graph is joining. The head identity is
    /// stable under child unification, so it is equal for exactly the operands
    /// the graph relates; the descriptor comparison, not this token, is the
    /// verifier.
    fn logicalDigest(self: *RepresentationMirror, root: NodeId) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        const content = self.graph.content(root);
        const tag: u8 = @intFromEnum(std.meta.activeTag(content));
        hasher.update(&.{tag});
        switch (content) {
            .named => |named| {
                // Declared identity and kind only; the representation fields
                // (tier, iterator kind, mint depth, generated owner) are erased so
                // representationally distinct instances of one declared type share
                // a token, and arguments are omitted so an argument still being
                // unified does not distinguish two equal-identity operands.
                hasher.update(std.mem.asBytes(&@intFromEnum(named.def.module)));
                hasher.update(std.mem.asBytes(&@intFromEnum(named.def.type_name)));
                if (named.def.source_decl) |decl| hasher.update(std.mem.asBytes(&decl));
                hasher.update(&.{@intFromEnum(named.kind)});
            },
            .primitive => |prim| hasher.update(&.{@intFromEnum(prim)}),
            .erased => |digest| hasher.update(&digest.bytes),
            // Every other shape contributes only its tag: two nodes of the same
            // shape the graph relates are logically equal once their children
            // unify, which the shape tag already captures.
            else => {},
        }
        var out: [32]u8 = undefined;
        hasher.final(&out);
        return out;
    }

    fn fail(self: *RepresentationMirror) void {
        self.disabled = true;
    }
};

/// The immutable descriptor the shared policy reads, copied out of a graph named
/// node exactly as the graph's own adapter does.
fn descriptorOf(named: InstNamed) policy.NamedDescriptor {
    return .{
        .kind = named.kind,
        .def = named.def,
        .builtin_owner = named.builtin_owner,
        .arg_count = named.args.len,
        .backing_use = if (named.backing) |backing| backing.use else null,
    };
}

/// Whether the engine's sealed iterator descriptor agrees with the graph node's
/// final representation content across every representation field the flip must
/// preserve: tier, iterator kind, mint depth, owner, and named kind.
fn descriptorsAgree(graph_named: InstNamed, engine_descriptor: policy.NamedDescriptor) bool {
    return graph_named.def.iterator_representation == engine_descriptor.def.iterator_representation and
        graph_named.def.iterator_kind == engine_descriptor.def.iterator_kind and
        graph_named.def.iterator_depth == engine_descriptor.def.iterator_depth and
        graph_named.builtin_owner == engine_descriptor.builtin_owner and
        graph_named.kind == engine_descriptor.kind;
}

fn tierRank(tier: Type.IteratorRepresentation) u8 {
    return switch (tier) {
        .none => 0,
        .minted => 1,
        .forced_dynamic => 2,
    };
}

// --- Tests: graph decisions drive the engine; sealed descriptors agree ---

const testing = std.testing;

const MirrorFixture = struct {
    type_store: Type.Store,
    name_store: names.NameStore,
    unsolved_monos: std.AutoHashMap(Type.TypeId, void),
    graph: *InstGraph,

    fn init(allocator: Allocator) Allocator.Error!MirrorFixture {
        var fixture: MirrorFixture = .{
            .type_store = Type.Store.init(allocator),
            .name_store = names.NameStore.init(allocator),
            .unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(allocator),
            .graph = undefined,
        };
        fixture.graph = try InstGraph.create(
            allocator,
            &fixture.type_store,
            &fixture.name_store,
            &fixture.unsolved_monos,
        );
        return fixture;
    }

    fn attachMirror(self: *MirrorFixture) Allocator.Error!void {
        self.graph.mirror = try RepresentationMirror.create(self.graph);
    }

    fn deinit(self: *MirrorFixture) void {
        self.graph.destroy();
        self.unsolved_monos.deinit();
        self.name_store.deinit();
        self.type_store.deinit();
    }
};

const IterNodes = struct {
    public_iter: NodeId,
    minted_iter: NodeId,
    public_backing: NodeId,
    minted_backing: NodeId,
};

/// Build a public iterator and a distinct minted iterator over the same
/// declaration and item type, mirroring the solve.zig iterator-join fixture.
fn buildPublicAndMinted(fixture: *MirrorFixture) Allocator.Error!IterNodes {
    const graph = fixture.graph;
    const module_identity = try fixture.name_store.internModuleIdentity(&([_]u8{0xC1} ** 32));
    const type_name = try fixture.name_store.internTypeName("Builtin.Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = @enumFromInt(5) };
    const public_def: Type.TypeDef = .{
        .module = module_identity,
        .type_name = type_name,
        .source_decl = 62,
    };
    var minted_def = public_def;
    minted_def.generated = .{ .bytes = [_]u8{0x5A} ** 32 };
    minted_def.iterator_representation = .minted;
    minted_def.iterator_kind = .list;
    minted_def.iterator_depth = 1;

    const item = try graph.newNode(.{ .primitive = .u64 });
    const public_backing = try graph.newNode(.empty_record);
    const minted_backing = try graph.newNode(.empty_record);
    const public_args = try graph.arena().alloc(NodeId, 1);
    public_args[0] = item;
    const minted_args = try graph.arena().alloc(NodeId, 1);
    minted_args[0] = item;

    const public_iter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = public_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = public_args,
        .backing = .{ .node = public_backing, .use = .inspectable },
    } });
    const minted_iter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = minted_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = minted_args,
        .backing = .{ .node = minted_backing, .use = .inspectable },
    } });
    return .{
        .public_iter = public_iter,
        .minted_iter = minted_iter,
        .public_backing = public_backing,
        .minted_backing = minted_backing,
    };
}

test "graph iterator join drives the mirror engine to the same class" {
    const allocator = testing.allocator;
    var fixture = try MirrorFixture.init(allocator);
    defer fixture.deinit();
    try fixture.attachMirror();

    const iters = try buildPublicAndMinted(&fixture);
    try fixture.graph.unify(iters.public_iter, iters.minted_iter);

    const mirror = fixture.graph.mirror.?;
    try testing.expect(!mirror.disabled);

    // The graph's join fired the mirror hook, which created a slot per operand
    // and related them into one engine class.
    const public_slot = mirror.slotOf(iters.public_iter) orelse return error.TestUnexpectedResult;
    const minted_slot = mirror.slotOf(iters.minted_iter) orelse return error.TestUnexpectedResult;
    try testing.expect(mirror.engine.related(public_slot, minted_slot));

    // The engine's representative keeps the minted representation, matching the
    // graph's minted-stands outcome.
    const representative = mirror.engine.shapeOf(public_slot);
    try testing.expectEqual(
        Type.IteratorRepresentation.minted,
        representative.iterator.descriptor.def.iterator_representation,
    );
}

test "sealed engine descriptor agrees with the graph node on a hand-built join" {
    const allocator = testing.allocator;
    var fixture = try MirrorFixture.init(allocator);
    defer fixture.deinit();
    try fixture.attachMirror();

    const iters = try buildPublicAndMinted(&fixture);
    try fixture.graph.unify(iters.public_iter, iters.minted_iter);

    const mirror = fixture.graph.mirror.?;

    // The seal comparison reads the graph node's final content (the minted
    // winner) and the engine's representative descriptor; they must agree.
    const root = fixture.graph.rootOf(iters.public_iter);
    const graph_named = switch (fixture.graph.content(root)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    const slot = mirror.slotOf(root) orelse return error.TestUnexpectedResult;
    const engine_descriptor = mirror.engine.shapeOf(slot).iterator.descriptor;
    try testing.expect(descriptorsAgree(graph_named, engine_descriptor));
}

test "interface reservation reserves argument and result positions" {
    const allocator = testing.allocator;
    var fixture = try MirrorFixture.init(allocator);
    defer fixture.deinit();
    try fixture.attachMirror();

    const graph = fixture.graph;
    const arg = try graph.newNode(.{ .primitive = .u64 });
    const ret = try graph.newNode(.{ .primitive = .str });
    const args = try graph.arena().alloc(NodeId, 1);
    args[0] = arg;
    const root_fn = try graph.newNode(.{ .func = .{ .args = args, .ret = ret } });

    const mirror = graph.mirror.?;
    mirror.reserveInterface(root_fn);
    // One argument slot plus the result slot were reserved.
    try testing.expectEqual(@as(usize, 2), mirror.interface.items.len);
    try testing.expect(mirror.slotOf(arg) != null);
    try testing.expect(mirror.slotOf(ret) != null);
    // A representation-neutral body moves no reserved position up a tier.
    mirror.measureInterfaceGain();
    try testing.expect(!mirror.disabled);
}
