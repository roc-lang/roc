//! Per-specialization type solver for Monotype lowering.
//!
//! Checked types instantiate into union-find nodes with explicit row
//! extension links; constraints unify nodes order-independently; Monotypes
//! use immutable read-only snapshots of fully resolved nodes when Type-shaped
//! inspection is required. Cross-specialization edges import finished Monotypes as
//! snapshots, so a specialization that needs more than its requested type is
//! a unification conflict rather than a silent rewrite of another
//! specialization's final type.

const std = @import("std");
const check = @import("check");
const base = @import("base");
const collections = @import("collections");

const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const Ident = base.Ident;

/// A compile-time entry root qualified by the checked module that owns it.
/// `ComptimeRootId`s are module-local, so a root that travels across template
/// requests must carry its owning module to stay comparable: the same integer
/// id names unrelated roots in different modules.
pub const EntryRoot = struct {
    module: checked.ModuleId,
    root: checked.ComptimeRootId,
};

/// Identity of a node in a specialization's instantiation graph.
pub const NodeId = enum(u32) { _ };

/// Tag variant inside an instantiation-graph row. Names are program NameStore
/// ids translated at instantiation so rows from different checked modules
/// compare uniformly.
pub const InstTag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: []NodeId,
};

/// Record field inside an instantiation-graph row.
pub const InstField = struct {
    name: names.RecordFieldNameId,
    ty: NodeId,
};

/// Source of an unresolved instantiation-graph node. Sealing may default a
/// checked variable, but a compiler-owned placeholder that survives to sealing
/// means an earlier instantiation step failed to write explicit data.
pub const InstVariableOrigin = enum(u8) {
    checked_variable,
    row_extension,
    placeholder,
};

/// Defaulting evidence carried by an unresolved instantiation-graph node until
/// unification resolves it or materialization applies the default.
pub const InstVariable = struct {
    origin: InstVariableOrigin,
    numeric_default_phase: ?checked.NumericDefaultPhase = null,
    row_default: ?checked.RowDefault = null,

    pub fn checkedVariable(
        numeric_default_phase: ?checked.NumericDefaultPhase,
        row_default: ?checked.RowDefault,
    ) InstVariable {
        return .{
            .origin = .checked_variable,
            .numeric_default_phase = numeric_default_phase,
            .row_default = row_default,
        };
    }

    pub fn row(default: checked.RowDefault) InstVariable {
        return .{
            .origin = .row_extension,
            .row_default = default,
        };
    }

    pub fn placeholder() InstVariable {
        return .{ .origin = .placeholder };
    }
};

/// Backing of a named instantiation-graph node.
pub const InstBacking = struct {
    node: NodeId,
    use: Type.BackingUse,
    authority: Type.BackingAuthority = .checked_public,
};

/// Declared field order while a named type is still in the instantiation graph.
pub const InstDeclaredField = union(enum(u8)) {
    named: names.RecordFieldNameId,
    padding: NodeId,
};

/// Named (alias/nominal/opaque) instantiation-graph node.
const InstNamed = struct {
    named_type: Type.NamedType,
    def: Type.TypeDef,
    kind: Type.NamedKind,
    builtin_owner: ?static_dispatch.BuiltinOwner,
    args: []NodeId,
    backing: ?InstBacking,
    /// Graph-owned provenance for an iterator representation minted while
    /// relations are still being produced. Its durable `generated` digest is
    /// computed only when this graph is sealed, from the final component
    /// types. Imported finished Monotypes have this null and already carry
    /// their producer digest in `def.generated`.
    generated_iterator: ?InstGeneratedIterator = null,
    /// Declared field order for a nominal/opaque record backing (empty
    /// otherwise). Padding field types are graph nodes so sealing maps them to
    /// immutable type ids with the rest of the named type.
    declared_order: []const InstDeclaredField = &.{},
};

/// Graph-owned data for a private iterator representation before sealing.
pub const InstGeneratedIterator = struct {
    callable_evidence: ?names.TypeDigest,
    /// Exact producer inputs used while relations are active. These are not
    /// nominal type arguments: the durable generated definition is identified
    /// by their final digests, and its runtime backing is already explicit.
    components: []const NodeId = &.{},
    public_source: InstIteratorPublicSource,
};

/// Exact checked public iterator definition refined by a generated iterator.
pub const InstIteratorPublicSource = struct {
    named_type: Type.NamedType,
    def: Type.TypeDef,
    kind: Type.NamedKind,
    builtin_owner: static_dispatch.BuiltinOwner,
    backing: InstBacking,
    declared_order: []const InstDeclaredField,
};

/// Content of an instantiation-graph node. Rows carry explicit extension
/// links; `redirect` is the union-find edge.
pub const InstNode = union(enum) {
    redirect: NodeId,
    unresolved: InstVariable,
    primitive: Type.Primitive,
    list: NodeId,
    box: NodeId,
    tuple: []NodeId,
    func: struct {
        args: []NodeId,
        ret: NodeId,
    },
    tag_union: struct {
        tags: []InstTag,
        ext: NodeId,
    },
    record: struct {
        fields: []InstField,
        ext: NodeId,
    },
    empty_tag_union,
    empty_record,
    named: InstNamed,
    erased: names.TypeDigest,
    zst,
};

/// Graph-native function shape. These nodes remain live until their owning
/// specialization graph is sealed.
pub const FunctionNodes = struct {
    args: []const NodeId,
    ret: NodeId,
};

/// Immutable alpha-normalized bytes for one open function interface, scoped to
/// the producing instantiation graph. The digest selects lookup candidates;
/// exact bytes remain the collision authority after body relations mutate the
/// live request nodes.
pub const OpenFunctionInterfaceShape = struct {
    digest: names.TypeDigest,
    bytes: []const u8,
    resolved: bool,
};

/// Deterministic operation counts for diagnosing Monotype graph workloads.
/// `InstGraph.diagnostics` remains null unless detailed diagnostics were
/// requested, so ordinary lowering does not count hot-path operations.
pub const GraphDiagnostics = struct {
    nodes_created: u64 = 0,
    unify_requests: u64 = 0,
    class_unions: u64 = 0,
    active_type_requests: u64 = 0,
    active_type_imported_hits: u64 = 0,
    active_snapshot_cache_hits: u64 = 0,
    active_snapshot_cache_misses: u64 = 0,
    active_snapshot_nodes_materialized: u64 = 0,
    active_snapshot_invalidations: u64 = 0,
    active_snapshot_entries_invalidated: u64 = 0,
    mono_import_requests: u64 = 0,
    mono_import_hits: u64 = 0,
    mono_import_misses: u64 = 0,
    produced_type_requests: u64 = 0,
    produced_type_cycle_hits: u64 = 0,
    produced_type_pairs_visited: u64 = 0,
    produced_type_joins: u64 = 0,
    function_request_builds: u64 = 0,
    function_request_pairs_visited: u64 = 0,
    function_request_replacements: u64 = 0,
    function_request_nodes_materialized: u64 = 0,
    generated_representation_roots_finalized: u64 = 0,
    generated_identity_roots_finalized: u64 = 0,
    generated_identity_roots_coalesced: u64 = 0,
    generated_identity_nodes_hashed: u64 = 0,
    generated_identity_cache_hits: u64 = 0,
    generated_type_store_hits: u64 = 0,
    generated_type_store_misses: u64 = 0,
};

/// Graph-native named-type cells.
pub const NamedNodes = struct {
    kind: Type.NamedKind,
    args: []const NodeId,
    backing: ?InstBacking,
};

/// Graph-native record fields in their exact flattened row order.
pub const RecordNodes = struct {
    fields: []const InstField,
};

/// Graph-native flattened tag-row variants. The extension remains internal:
/// callers consume the explicit labels and payload cells rather than
/// reconstructing or mutating row openness.
pub const TagRowNodes = struct {
    tags: []const InstTag,
};

/// Graph-native tag row used while constructing an exact produced value.
pub const TagConstructionRow = struct {
    /// Exact structural backing beneath any explicit nominal constructor.
    root: NodeId,
    tags: []const InstTag,
    ext: NodeId,
};

const NodePair = struct {
    left: NodeId,
    right: NodeId,
};

const FunctionRequestMaterializationMode = enum(u8) {
    request,
    produced_value,
    produced_callable,
    body_abi,
    reassigned_storage,
};

const materialization_mode_count = std.meta.fields(FunctionRequestMaterializationMode).len;

const FunctionRequestMaterialization = struct {
    pair: NodePair,
    mode: FunctionRequestMaterializationMode,
};

const MaterializedNode = struct {
    node: NodeId,
    /// A replacement or function copy selected this node. A provisional
    /// recursive self-edge can differ from its source without changing this.
    changed: bool,
};

const MaterializedNodes = struct {
    nodes: ?[]NodeId,
    changed: bool,
};

const FunctionRequestSubstitution = struct {
    replacements: collections.DenseMap(NodeId, NodeId),
    materialized: std.AutoHashMap(FunctionRequestMaterialization, MaterializedNode),
    /// Reserved named roots currently being built, separated by copy purpose.
    /// This closes recursive edges without merging completed sibling copies.
    active_materialized: [materialization_mode_count]collections.DenseMap(NodeId, NodeId),
    compared: std.AutoHashMap(NodePair, void),
    /// Whether collecting produced inputs added or changed a replacement
    /// after an existing request span was seeded.
    changed_after_seed: bool = false,

    fn init(allocator: Allocator) FunctionRequestSubstitution {
        return .{
            .replacements = collections.DenseMap(NodeId, NodeId).init(allocator),
            .materialized = std.AutoHashMap(FunctionRequestMaterialization, MaterializedNode).init(allocator),
            .active_materialized = .{collections.DenseMap(NodeId, NodeId).init(allocator)} ** materialization_mode_count,
            .compared = std.AutoHashMap(NodePair, void).init(allocator),
        };
    }

    fn deinit(self: *FunctionRequestSubstitution) void {
        self.replacements.deinit();
        self.materialized.deinit();
        for (&self.active_materialized) |*active| active.deinit();
        self.compared.deinit();
    }
};

/// One checker-authored source cell and the produced cell selected for it by a
/// completed function request.
pub const RequestSubstitution = struct {
    checked: NodeId,
    produced: NodeId,
};

/// Result of hashing one complete generated-iterator construction request.
/// A vacant result carries the digest into registration so a cache miss never
/// hashes the same inputs twice.
pub const GeneratedIteratorLookup = struct {
    existing: ?NodeId,
    digest: names.TypeDigest,
};

const RequestSubstitutionSpan = struct {
    start: u32,
    len: u32,

    const uninitialized_len = std.math.maxInt(u32);
    const uninitialized: RequestSubstitutionSpan = .{ .start = 0, .len = uninitialized_len };

    fn isInitialized(self: RequestSubstitutionSpan) bool {
        return self.len != uninitialized_len;
    }
};

const ProducedJoinMemo = union(enum) {
    visiting,
    cycle: NodeId,
    done: NodeId,
};

const RelationStamp = struct {
    left: NodeId,
    left_version: u32,
    right: NodeId,
    right_version: u32,
};

const ProducedRelationStamp = struct {
    request: NodeId,
    request_version: u32,
    produced: NodeId,
    produced_version: u32,
};

const TypeApplicationKind = enum {
    exact_producer,
    checked_mapping,
    selected_substitutions,
};

const NominalBackingDeclaration = struct {
    module_bytes: [32]u8,
    declaration_id: u32,
};

const NominalBackingCacheContext = struct {
    pub fn hash(_: NominalBackingCacheContext, key: NominalBackingDeclaration) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(&key.module_bytes);
        var declaration_id = std.mem.nativeToLittle(u32, key.declaration_id);
        hasher.update(std.mem.asBytes(&declaration_id));
        return hasher.final();
    }

    pub fn eql(_: NominalBackingCacheContext, left: NominalBackingDeclaration, right: NominalBackingDeclaration) bool {
        return std.mem.eql(u8, left.module_bytes[0..], right.module_bytes[0..]) and
            left.declaration_id == right.declaration_id;
    }
};

/// One instantiated backing of a declaration. Argument identity follows the
/// union-find classes rather than raw node ids, which can redirect as producer
/// evidence is applied.
const NominalBackingInstance = struct {
    args: []NodeId,
    node: NodeId,
};

const GeneratedIteratorInternContext = struct {
    pub fn hash(_: @This(), key: names.TypeDigest) u64 {
        return std.hash.Wyhash.hash(0, &key.bytes);
    }

    pub fn eql(_: @This(), left: names.TypeDigest, right: names.TypeDigest) bool {
        return std.mem.eql(u8, &left.bytes, &right.bytes);
    }
};

const RelationState = enum {
    producing,
    frozen,
};

const GeneratedIteratorDepthRule = union(enum) {
    fixed: u8,
    children: struct {
        count: usize,
        increment: u8,
    },
};

const GeneratedIteratorDepthFrame = struct {
    node: NodeId,
    next_child: usize,
    child_count: usize,
    max_child_depth: u8,
    increment: u8,
};

/// Per-specialization type solver. Checked types instantiate into union-find
/// nodes with explicit row extension links; constraints unify nodes
/// order-independently. Type-shaped inspection receives immutable snapshots of
/// resolved graph nodes, invalidated rather than rewritten when relations
/// change. Cross-specialization edges import final Monotypes as closed
/// structure, so a specialization that tries to exceed its requested type is a
/// unification conflict, not a silent divergence.
pub const InstGraph = struct {
    allocator: Allocator,
    relation_state: RelationState,
    types: *Type.Store,
    name_store: *const names.NameStore,
    diagnostics: ?*GraphDiagnostics,
    arena_impl: std.heap.ArenaAllocator,
    nodes: std.ArrayList(InstNode),
    versions: std.ArrayList(u32),
    /// Intrusive chain of permanent node ids in each live union class. Draft
    /// request lookup indexes an open function under one permanent interface
    /// node and probes the current class members, so later unions never stale
    /// the key. Roots own the head/tail; every node owns one next link.
    class_member_next: std.ArrayList(?NodeId),
    class_member_head: std.ArrayList(NodeId),
    class_member_tail: std.ArrayList(NodeId),
    processed_relations: std.AutoHashMap(RelationStamp, void),
    processed_produced_relations: std.AutoHashMap(ProducedRelationStamp, void),
    produced_type_pending: std.ArrayList(NodePair),
    applying_produced_type: bool,
    produced_join_memo: std.AutoHashMap(NodePair, ProducedJoinMemo),
    joining_produced_types: bool,
    /// Immutable Type-shaped snapshots by permanent node id. Old snapshots
    /// retain their original provenance while `find` resolves that node to its
    /// current class root; unions therefore never move or reindex snapshots.
    node_snapshots: collections.DenseMap(NodeId, std.ArrayList(Type.TypeId)),
    /// Latest immutable snapshot for a root. Any relation mutation clears this
    /// cache; a subsequent inspection materializes a fresh snapshot.
    current_snapshots: collections.DenseMap(NodeId, Type.TypeId),
    /// Relation mutations only mark the snapshot cache stale. The next read
    /// performs one exact global invalidation, coalescing mutation bursts that
    /// do not inspect an intermediate graph state.
    current_snapshots_dirty: bool,
    /// Reverse active-snapshot links, also the import memo: a Monotype already
    /// connected to this graph reuses its node instead of being copied.
    linked_type_nodes: collections.DenseMap(Type.TypeId, NodeId),
    /// Exact immutable Monotype snapshot imported at each permanent node.
    /// Unlike `node_snapshots`, these are producer-owned representation
    /// witnesses. Keeping the direct node association lets consumers of an
    /// imported request use the exact finished TypeId rather than reconstructing
    /// an equivalent public shape.
    imported_monos: collections.DenseMap(NodeId, Type.TypeId),
    /// Finished generated roots encountered explicitly while importing a
    /// Monotype. These need authoritative descendant binding even when this graph
    /// does not own a corresponding representation producer.
    imported_generated_iterator_nodes: std.ArrayList(NodeId),
    /// Authoritative TypeIds imported from the generated-representation interner.
    /// During binding these are keyed by permanent import nodes; after all
    /// authoritative relations are complete they are reindexed once by final
    /// union-class root, giving sealing a direct lookup for every backing child.
    generated_authoritative_monos: collections.DenseMap(NodeId, Type.TypeId),
    /// Current extension root for each row root. This is the authority for
    /// maintaining `row_parents`; stale extension edges are removed when row
    /// content changes.
    row_exts: std.ArrayList(?NodeId),
    /// Row nodes by the extension node they currently chain through.
    row_parents: collections.DenseMap(NodeId, std.ArrayList(NodeId)),
    /// Declaration-backed nominal backings already instantiated in this graph,
    /// bucketed by source declaration. Entries compare argument union-find
    /// classes, so a backing instance keeps one identity after evidence merges
    /// or redirects its original argument nodes.
    nominal_backings: std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingCacheContext, 80),
    /// Generated iterator candidates keyed by all exact construction inputs.
    /// The digest is graph-local: durable identities are sealed separately
    /// after every input type is final.
    generated_iterator_intern: std.HashMap(names.TypeDigest, std.ArrayList(NodeId), GeneratedIteratorInternContext, 80),
    /// Permanent item-node index used when a later union changes the current
    /// roots that contributed to an older graph-local digest. Walking the
    /// dense union class reaches only candidates that can actually match.
    generated_iterators_by_item: collections.DenseMap(NodeId, std.ArrayList(NodeId)),
    /// Permanent roots recorded by the producer at construction time. The two
    /// generated-representation finalizers consume only this registry instead
    /// of searching every unrelated node in the graph.
    generated_iterator_nodes: std.ArrayList(NodeId),
    /// Exact checked source function node from which a distinct produced
    /// function request was constructed. This is explicit substitution input;
    /// consumers read this field instead of deriving it from the produced
    /// function's type shape.
    request_checked_sources: std.ArrayList(?NodeId),
    /// Complete substitutions already discovered while each function request
    /// was constructed. Request refinement and body lowering consume this
    /// explicit output instead of traversing the previous interface again.
    request_substitution_spans: std.ArrayList(RequestSubstitutionSpan),
    request_substitutions: std.ArrayList(RequestSubstitution),
    /// Minted iterator roots whose relation graph proved that retaining the
    /// minted tier would create a recursive component identity. The raw node
    /// remains valid across later unions; finalization resolves it to the live
    /// class and constructs the single forced-dynamic fixed point.
    forced_dynamic_iterator_roots: collections.DenseMap(NodeId, void),
    /// Permanent value-slot nodes that differ from the corresponding source
    /// slot on an explicit recursive edge. Function recursion and loop
    /// feedback both mark this dense set; a later minted join touching one of
    /// these slots proves that recursion grows the representation rather than
    /// merely recurring over a fixed iterator.
    recursive_argument_slots: collections.DenseMap(NodeId, void),
    pub fn create(
        allocator: Allocator,
        types: *Type.Store,
        name_store: *const names.NameStore,
    ) Allocator.Error!*InstGraph {
        const graph = try allocator.create(InstGraph);
        graph.* = .{
            .allocator = allocator,
            .relation_state = .producing,
            .types = types,
            .name_store = name_store,
            .diagnostics = null,
            .arena_impl = std.heap.ArenaAllocator.init(allocator),
            .nodes = .empty,
            .versions = .empty,
            .class_member_next = .empty,
            .class_member_head = .empty,
            .class_member_tail = .empty,
            .processed_relations = std.AutoHashMap(RelationStamp, void).init(allocator),
            .processed_produced_relations = std.AutoHashMap(ProducedRelationStamp, void).init(allocator),
            .produced_type_pending = .empty,
            .applying_produced_type = false,
            .produced_join_memo = std.AutoHashMap(NodePair, ProducedJoinMemo).init(allocator),
            .joining_produced_types = false,
            .node_snapshots = collections.DenseMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .current_snapshots = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .current_snapshots_dirty = false,
            .linked_type_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .imported_monos = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .imported_generated_iterator_nodes = .empty,
            .generated_authoritative_monos = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .row_exts = .empty,
            .row_parents = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .nominal_backings = std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingCacheContext, 80).init(allocator),
            .generated_iterator_intern = std.HashMap(names.TypeDigest, std.ArrayList(NodeId), GeneratedIteratorInternContext, 80).init(allocator),
            .generated_iterators_by_item = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .generated_iterator_nodes = .empty,
            .request_checked_sources = .empty,
            .request_substitution_spans = .empty,
            .request_substitutions = .empty,
            .forced_dynamic_iterator_roots = collections.DenseMap(NodeId, void).init(allocator),
            .recursive_argument_slots = collections.DenseMap(NodeId, void).init(allocator),
        };
        return graph;
    }

    pub fn setDiagnostics(self: *InstGraph, diagnostics: *GraphDiagnostics) void {
        self.diagnostics = diagnostics;
    }

    fn countDiagnostic(self: *InstGraph, comptime field: []const u8) void {
        if (self.diagnostics) |diagnostics| {
            @field(diagnostics, field) += 1;
        }
    }

    fn countDiagnosticBy(self: *InstGraph, comptime field: []const u8, amount: usize) void {
        if (self.diagnostics) |diagnostics| {
            @field(diagnostics, field) += @intCast(amount);
        }
    }

    pub fn destroy(self: *InstGraph) void {
        const allocator = self.allocator;
        var views = self.node_snapshots.valueIterator();
        while (views.next()) |list| {
            list.deinit(allocator);
        }
        self.node_snapshots.deinit();
        self.current_snapshots.deinit();
        var parents = self.row_parents.valueIterator();
        while (parents.next()) |list| {
            list.deinit(allocator);
        }
        var backing_buckets = self.nominal_backings.valueIterator();
        while (backing_buckets.next()) |bucket| {
            bucket.deinit(allocator);
        }
        self.nominal_backings.deinit();
        var generated_buckets = self.generated_iterator_intern.valueIterator();
        while (generated_buckets.next()) |bucket| bucket.deinit(allocator);
        self.generated_iterator_intern.deinit();
        var generated_item_buckets = self.generated_iterators_by_item.valueIterator();
        while (generated_item_buckets.next()) |bucket| bucket.deinit(allocator);
        self.generated_iterators_by_item.deinit();
        self.generated_iterator_nodes.deinit(allocator);
        self.request_substitutions.deinit(allocator);
        self.request_substitution_spans.deinit(allocator);
        self.request_checked_sources.deinit(allocator);
        self.forced_dynamic_iterator_roots.deinit();
        self.recursive_argument_slots.deinit();
        self.row_parents.deinit();
        self.row_exts.deinit(allocator);
        self.imported_generated_iterator_nodes.deinit(allocator);
        self.imported_monos.deinit();
        self.generated_authoritative_monos.deinit();
        self.linked_type_nodes.deinit();
        self.produced_join_memo.deinit();
        self.produced_type_pending.deinit(allocator);
        self.processed_produced_relations.deinit();
        self.processed_relations.deinit();
        self.class_member_tail.deinit(allocator);
        self.class_member_head.deinit(allocator);
        self.class_member_next.deinit(allocator);
        self.versions.deinit(allocator);
        self.nodes.deinit(allocator);
        self.arena_impl.deinit();
        allocator.destroy(self);
    }

    pub fn arena(self: *InstGraph) Allocator {
        return self.arena_impl.allocator();
    }

    pub fn registerRequestCheckedSource(
        self: *InstGraph,
        request_fn: NodeId,
        source_fn: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const entry = &self.request_checked_sources.items[@intFromEnum(request_fn)];
        if (entry.*) |existing| {
            if (self.find(existing) != self.find(source_fn)) {
                Common.invariant("exact function request was registered with two checked sources");
            }
        } else {
            entry.* = source_fn;
        }
    }

    pub fn requestCheckedSource(self: *InstGraph, request_fn: NodeId) ?NodeId {
        const source_fn = self.request_checked_sources.items[@intFromEnum(request_fn)] orelse return null;
        return self.find(source_fn);
    }

    pub fn requestSubstitutions(self: *const InstGraph, request_fn: NodeId) []const RequestSubstitution {
        const span = self.request_substitution_spans.items[@intFromEnum(request_fn)];
        if (!span.isInitialized()) return &.{};
        const start: usize = span.start;
        return self.request_substitutions.items[start .. start + span.len];
    }

    pub fn nodeIsGeneratedPrivateRoot(self: *InstGraph, node: NodeId) bool {
        return isGeneratedPrivateRootContent(self.nodes.items[@intFromEnum(self.find(node))]);
    }

    pub fn findGeneratedIterator(
        self: *InstGraph,
        public_node: NodeId,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) Allocator.Error!?NodeId {
        const public_named = switch (self.content(public_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => return null,
        };
        return (try self.lookupGeneratedIteratorFromNamed(
            public_named,
            kind,
            components,
            callable_evidence,
        )).existing;
    }

    /// Look up an equal generated iterator and retain the computed digest for
    /// allocation-free registration when the lookup is vacant.
    pub fn lookupGeneratedIterator(
        self: *InstGraph,
        public_node: NodeId,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) Allocator.Error!GeneratedIteratorLookup {
        const public_named = switch (self.content(public_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator lookup received a non-named public type"),
        };
        return self.lookupGeneratedIteratorFromNamed(
            public_named,
            kind,
            components,
            callable_evidence,
        );
    }

    pub fn lookupGeneratedIteratorFromNamed(
        self: *InstGraph,
        public_named: InstNamed,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) Allocator.Error!GeneratedIteratorLookup {
        if (public_named.args.len == 0) {
            Common.invariant("generated iterator lookup received no public item argument");
        }
        const digest = try self.generatedIteratorInternDigest(public_named, kind, components, callable_evidence);
        if (self.generated_iterator_intern.get(digest)) |candidates| {
            for (candidates.items) |candidate| {
                if (try self.generatedIteratorMatches(candidate, public_named, kind, components, callable_evidence)) {
                    return .{ .existing = self.find(candidate), .digest = digest };
                }
            }
        }

        // A union can change a key's current node ids after insertion. Each
        // generated iterator is also indexed under its permanent item node, so
        // inspect only the now-equal item class instead of scanning the graph.
        var item_members = self.classMemberIterator(public_named.args[0]);
        while (item_members.next()) |member| {
            const candidates = self.generated_iterators_by_item.get(member) orelse continue;
            for (candidates.items) |candidate| {
                if (try self.generatedIteratorMatches(candidate, public_named, kind, components, callable_evidence)) {
                    return .{ .existing = self.find(candidate), .digest = digest };
                }
            }
        }
        return .{ .existing = null, .digest = digest };
    }

    pub fn registerGeneratedIterator(self: *InstGraph, raw_node: NodeId) Allocator.Error!void {
        self.requireRelationProduction();
        try self.generated_iterator_nodes.append(self.allocator, raw_node);
        try self.indexGeneratedIterator(raw_node);
    }

    /// Register a newly built iterator under the digest already computed by
    /// its immediately preceding vacant lookup.
    pub fn registerGeneratedIteratorAtDigest(
        self: *InstGraph,
        raw_node: NodeId,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        try self.generated_iterator_nodes.append(self.allocator, raw_node);
        try self.indexGeneratedIteratorAtDigest(raw_node, digest);
    }

    fn indexGeneratedIterator(self: *InstGraph, raw_node: NodeId) Allocator.Error!void {
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator interner received a non-named node"),
        };
        const provenance = named.generated_iterator orelse
            Common.invariant("generated iterator interner received a node without producer provenance");
        if (named.args.len == 0) {
            Common.invariant("generated iterator interner received no item argument");
        }
        const digest = try self.generatedIteratorInternDigest(
            named,
            named.def.iterator_kind,
            provenance.components,
            provenance.callable_evidence,
        );
        try self.indexGeneratedIteratorAtDigest(node, digest);
    }

    fn indexGeneratedIteratorAtDigest(
        self: *InstGraph,
        raw_node: NodeId,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator interner received a non-named node"),
        };
        if (named.args.len == 0) {
            Common.invariant("generated iterator interner received no item argument");
        }
        const digest_entry = try self.generated_iterator_intern.getOrPut(digest);
        if (!digest_entry.found_existing) digest_entry.value_ptr.* = .empty;
        for (digest_entry.value_ptr.items) |existing| {
            if (self.find(existing) == node) break;
        } else try digest_entry.value_ptr.append(self.allocator, node);

        const item_entry = try self.generated_iterators_by_item.getOrPut(named.args[0]);
        if (!item_entry.found_existing) item_entry.value_ptr.* = .empty;
        for (item_entry.value_ptr.items) |existing| {
            if (self.find(existing) == node) return;
        }
        try item_entry.value_ptr.append(self.allocator, node);
    }

    fn generatedIteratorMatches(
        self: *InstGraph,
        raw_candidate: NodeId,
        public_named: InstNamed,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) Allocator.Error!bool {
        const candidate = switch (self.content(raw_candidate)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => return false,
        };
        const provenance = candidate.generated_iterator orelse return false;
        if (candidate.def.iterator_kind != kind or
            !optionalInstDigestEql(provenance.callable_evidence, callable_evidence) or
            candidate.kind != public_named.kind or
            candidate.def.module != public_named.def.module or
            candidate.def.type_name != public_named.def.type_name or
            candidate.def.source_decl != public_named.def.source_decl or
            candidate.args.len != 1 or
            provenance.components.len != components.len or
            public_named.args.len != 1)
        {
            return false;
        }
        if (!try self.sameGeneratedIteratorIdentityInput(candidate.args[0], public_named.args[0])) return false;
        for (components, provenance.components) |component, stored| {
            if (!try self.sameGeneratedIteratorIdentityInput(component, stored)) return false;
        }
        return true;
    }

    fn generatedIteratorInternDigest(
        self: *InstGraph,
        public_named: InstNamed,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) Allocator.Error!names.TypeDigest {
        var writer = OpenFunctionInterfaceShapeWriter.initGeneratedLookup(self);
        defer writer.deinit();
        writer.writeBytes("roc.generated_iterator.graph_identity.v2");
        writer.writeBytes(self.name_store.moduleIdentityBytes(public_named.def.module));
        writer.writeOptionalU32(public_named.def.source_decl);
        if (public_named.def.source_decl == null) {
            writer.writeBytes(self.name_store.typeNameText(public_named.def.type_name));
        }
        writer.writeBytes(@tagName(public_named.kind));
        writer.writeBytes(@tagName(kind));
        try writer.writeNode(public_named.args[0]);
        writer.writeU32(@intCast(components.len));
        for (components) |component| try writer.writeNode(component);
        if (callable_evidence) |evidence| {
            writer.writeU8(1);
            writer.writeBytes(&evidence.bytes);
        } else {
            writer.writeU8(0);
        }
        return .{ .bytes = writer.hasher.finalResult() };
    }

    /// Exact collision authority for the early generated-representation
    /// interner. Distinct unresolved cells are deliberately not equal: their
    /// later solutions may differ. Fully produced structure is compared by
    /// content, so separately allocated copies of (for example) `List I64`
    /// select one iterator node before either backing is lowered again.
    fn sameGeneratedIteratorIdentityInput(
        self: *InstGraph,
        left_raw: NodeId,
        right_raw: NodeId,
    ) Allocator.Error!bool {
        var pending = std.ArrayList(NodePair).empty;
        defer pending.deinit(self.allocator);
        var compared = std.AutoHashMap(NodePair, void).init(self.allocator);
        defer compared.deinit();
        try pending.append(self.allocator, .{ .left = left_raw, .right = right_raw });

        while (pending.pop()) |raw_pair| {
            const left = self.find(raw_pair.left);
            const right = self.find(raw_pair.right);
            if (left == right) continue;
            const pair = NodePair{ .left = left, .right = right };
            const seen = try compared.getOrPut(pair);
            if (seen.found_existing) continue;

            const left_content = self.nodes.items[@intFromEnum(left)];
            const right_content = self.nodes.items[@intFromEnum(right)];

            if (left_content == .named and left_content.named.kind == .alias) {
                const backing = left_content.named.backing orelse
                    Common.invariant("generated identity comparison found an alias without backing");
                try pending.append(self.allocator, .{ .left = backing.node, .right = right });
                continue;
            }
            if (right_content == .named and right_content.named.kind == .alias) {
                const backing = right_content.named.backing orelse
                    Common.invariant("generated identity comparison found an alias without backing");
                try pending.append(self.allocator, .{ .left = left, .right = backing.node });
                continue;
            }
            if (std.meta.activeTag(left_content) != std.meta.activeTag(right_content)) return false;

            switch (left_content) {
                .redirect => unreachable,
                // A shared unresolved cell returned above. Independent cells
                // are different construction inputs even when their checked
                // variable metadata happens to match.
                .unresolved => return false,
                .primitive => |primitive| if (right_content.primitive != primitive) return false,
                .list => |elem| try pending.append(self.allocator, .{ .left = elem, .right = right_content.list }),
                .box => |elem| try pending.append(self.allocator, .{ .left = elem, .right = right_content.box }),
                .tuple => |items| {
                    if (items.len != right_content.tuple.len) return false;
                    for (items, right_content.tuple) |left_item, right_item| {
                        try pending.append(self.allocator, .{ .left = left_item, .right = right_item });
                    }
                },
                .func => |function| {
                    if (function.args.len != right_content.func.args.len) return false;
                    for (function.args, right_content.func.args) |left_arg, right_arg| {
                        try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                    }
                    try pending.append(self.allocator, .{ .left = function.ret, .right = right_content.func.ret });
                },
                .tag_union => |row| {
                    const right_row = right_content.tag_union;
                    if (row.tags.len != right_row.tags.len) return false;
                    for (row.tags, right_row.tags) |left_tag, right_tag| {
                        if (!self.name_store.tagLabelTextEql(left_tag.name, right_tag.name) or
                            !self.name_store.tagLabelTextEql(left_tag.checked_name, right_tag.checked_name) or
                            left_tag.payloads.len != right_tag.payloads.len)
                        {
                            return false;
                        }
                        for (left_tag.payloads, right_tag.payloads) |left_payload, right_payload| {
                            try pending.append(self.allocator, .{ .left = left_payload, .right = right_payload });
                        }
                    }
                    try pending.append(self.allocator, .{ .left = row.ext, .right = right_row.ext });
                },
                .record => |row| {
                    const right_row = right_content.record;
                    if (row.fields.len != right_row.fields.len) return false;
                    for (row.fields, right_row.fields) |left_field, right_field| {
                        if (!self.name_store.recordFieldLabelTextEql(left_field.name, right_field.name)) return false;
                        try pending.append(self.allocator, .{ .left = left_field.ty, .right = right_field.ty });
                    }
                    try pending.append(self.allocator, .{ .left = row.ext, .right = right_row.ext });
                },
                .empty_tag_union, .empty_record, .zst => {},
                .named => |left_named| {
                    const right_named = right_content.named;
                    const left_generated = left_named.generated_iterator;
                    const right_generated = right_named.generated_iterator;
                    if (left_generated != null or right_generated != null) {
                        if (left_generated == null or right_generated == null) return false;
                        if (!sameGeneratedIteratorPublicIdentity(
                            self.name_store,
                            left_generated.?.public_source,
                            right_generated.?.public_source,
                        ) or
                            left_named.def.iterator_representation != right_named.def.iterator_representation or
                            left_named.def.iterator_kind != right_named.def.iterator_kind or
                            !optionalInstDigestEql(left_generated.?.callable_evidence, right_generated.?.callable_evidence) or
                            left_named.args.len != 1 or right_named.args.len != 1 or
                            left_generated.?.components.len != right_generated.?.components.len)
                        {
                            return false;
                        }
                        try pending.append(self.allocator, .{ .left = left_named.args[0], .right = right_named.args[0] });
                        for (left_generated.?.components, right_generated.?.components) |left_component, right_component| {
                            try pending.append(self.allocator, .{ .left = left_component, .right = right_component });
                        }
                        continue;
                    }

                    const left_sealed_generated = sealedGeneratedIteratorDigest(left_named);
                    const right_sealed_generated = sealedGeneratedIteratorDigest(right_named);
                    if (left_sealed_generated != null or right_sealed_generated != null) {
                        if (!optionalInstDigestEql(left_sealed_generated, right_sealed_generated)) return false;
                        continue;
                    }

                    if (!std.mem.eql(u8, &left_named.named_type.module.bytes, &right_named.named_type.module.bytes) or
                        !std.meta.eql(left_named.def, right_named.def) or
                        left_named.kind != right_named.kind or
                        left_named.builtin_owner != right_named.builtin_owner or
                        left_named.args.len != right_named.args.len or
                        left_named.declared_order.len != right_named.declared_order.len)
                    {
                        return false;
                    }
                    for (left_named.args, right_named.args) |left_arg, right_arg| {
                        try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                    }
                    if ((left_named.backing == null) != (right_named.backing == null)) return false;
                    if (left_named.backing) |left_backing| {
                        const right_backing = right_named.backing.?;
                        if (left_backing.use != right_backing.use or
                            left_backing.authority != right_backing.authority)
                        {
                            return false;
                        }
                        try pending.append(self.allocator, .{ .left = left_backing.node, .right = right_backing.node });
                    }
                    for (left_named.declared_order, right_named.declared_order) |left_field, right_field| {
                        if (std.meta.activeTag(left_field) != std.meta.activeTag(right_field)) return false;
                        switch (left_field) {
                            .named => |left_name| if (!self.name_store.recordFieldLabelTextEql(left_name, right_field.named)) return false,
                            .padding => |left_padding| try pending.append(self.allocator, .{ .left = left_padding, .right = right_field.padding }),
                        }
                    }
                },
                .erased => |digest| if (!std.mem.eql(u8, &digest.bytes, &right_content.erased.bytes)) return false,
            }
        }
        return true;
    }

    fn sealedGeneratedIteratorDigest(named: InstNamed) ?names.TypeDigest {
        if (named.generated_iterator != null or named.def.iterator_representation == .none) return null;
        const owner = named.builtin_owner orelse return null;
        if (!static_dispatch.isIteratorOwner(owner)) return null;
        return named.def.generated;
    }

    fn sameGeneratedIteratorPublicIdentity(
        name_store: *const names.NameStore,
        left: InstIteratorPublicSource,
        right: InstIteratorPublicSource,
    ) bool {
        if (!std.mem.eql(
            u8,
            name_store.moduleIdentityBytes(left.def.module),
            name_store.moduleIdentityBytes(right.def.module),
        ) or left.def.source_decl != right.def.source_decl) {
            return false;
        }
        return left.def.source_decl != null or
            std.mem.eql(
                u8,
                name_store.typeNameText(left.def.type_name),
                name_store.typeNameText(right.def.type_name),
            );
    }

    fn acceptsRelationMutation(self: *const InstGraph) bool {
        return self.relation_state == .producing;
    }

    fn requireRelationProduction(self: *const InstGraph) void {
        if (!self.acceptsRelationMutation()) {
            Common.invariant("instantiation graph relation changed after final relation production");
        }
    }

    fn requireFrozenRelations(self: *const InstGraph) void {
        if (self.acceptsRelationMutation()) {
            Common.invariant("instantiation graph finalized before relation production was frozen");
        }
    }

    /// Complete pending active-view updates and prevent any later relation
    /// production. Final type sealing remains available after this transition.
    pub fn freezeRelations(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        self.relation_state = .frozen;
    }

    pub const ArgumentClassSnapshot = struct {
        members: []const NodeId,

        fn contains(self: ArgumentClassSnapshot, node: NodeId) bool {
            for (self.members) |member| {
                if (member == node) return true;
            }
            return false;
        }
    };

    /// Snapshot every permanent member of each ordered argument class before
    /// a specialization body can add recursive relations. Root choice is not
    /// stable under union, so recursive growth must compare permanent identity
    /// against the whole initial class rather than one representative node.
    pub fn snapshotFunctionArgumentClasses(
        self: *InstGraph,
        fn_node: NodeId,
    ) Allocator.Error![]const ArgumentClassSnapshot {
        const args = (try self.functionNodes(fn_node)).args;
        const snapshots = try self.arena().alloc(ArgumentClassSnapshot, args.len);
        for (args, snapshots) |arg, *snapshot| {
            var count: usize = 0;
            var counting = self.classMemberIterator(arg);
            while (counting.next() != null) count += 1;

            const members = try self.arena().alloc(NodeId, count);
            var filling = self.classMemberIterator(arg);
            var index: usize = 0;
            while (filling.next()) |member| : (index += 1) members[index] = member;
            snapshot.* = .{ .members = members };
        }
        return snapshots;
    }

    pub fn joinRecursiveFunctionInterface(
        self: *InstGraph,
        active_fn: NodeId,
        initial_active_arg_classes: []const ArgumentClassSnapshot,
        recursive_request: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const active = try self.functionNodes(active_fn);
        const request = try self.functionNodes(recursive_request);
        if (active.args.len != request.args.len or
            initial_active_arg_classes.len != request.args.len)
        {
            Common.invariant("recursive function interface changed argument arity");
        }
        const joined_args = try self.arena().alloc(NodeId, request.args.len);
        for (active.args, initial_active_arg_classes, request.args, joined_args) |active_arg, initial_class, request_arg, *joined_arg| {
            joined_arg.* = try self.joinProducedTypeRepresentations(active_arg, request_arg);
            if (!initial_class.contains(request_arg)) {
                try self.recursive_argument_slots.put(joined_arg.*, {});
            }
        }
        const joined_ret = try self.joinProducedTypeRepresentations(active.ret, request.ret);
        const joined_content: InstNode = .{ .func = .{
            .args = joined_args,
            .ret = joined_ret,
        } };
        const active_root = self.find(active_fn);
        const request_root = self.find(recursive_request);
        try self.setContent(active_root, joined_content);
        if (request_root != active_root) try self.setContent(request_root, joined_content);
    }

    pub fn markRecursiveValueSlot(self: *InstGraph, slot: NodeId) Allocator.Error!void {
        self.requireRelationProduction();
        try self.recursive_argument_slots.put(slot, {});
    }

    const generated_iterator_mint_depth_limit: u8 = std.math.maxInt(u8) - 1;
    const generated_iterator_forced_depth: u8 = generated_iterator_mint_depth_limit + 1;

    /// Decide every graph-owned iterator representation after relation
    /// production has supplied its complete component graph, but before any
    /// durable Monotype is sealed. The exact memoized graph walk follows values
    /// only: function bodies and named backings cannot store an iterator value
    /// and therefore do not contribute depth. A value cycle selects the finite
    /// forced-dynamic fixed point.
    pub fn finalizeGeneratedIteratorRepresentations(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();

        const Pending = struct {
            node: NodeId,
            depth: u8,
            force_dynamic: bool,
        };
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        var depths = collections.DenseMap(NodeId, u8).init(self.allocator);
        defer depths.deinit();
        var active = collections.DenseMap(NodeId, void).init(self.allocator);
        defer active.deinit();

        // Recursive representation growth is an explicit proof that this
        // class has the dynamic fixed-point tier. Seed that proof into the
        // depth graph before visiting any producer so every adapter containing
        // the class observes the dynamic sentinel as its child depth.
        var forced_roots = self.forced_dynamic_iterator_roots.keyIterator();
        while (forced_roots.next()) |forced_root| {
            try depths.put(self.find(forced_root.*), generated_iterator_forced_depth);
        }

        for (self.generated_iterator_nodes.items) |registered_node| {
            const node = self.find(registered_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("registered generated iterator stopped being named"),
            };
            if (named.generated_iterator == null) {
                Common.invariant("registered generated iterator lost its producer provenance");
            }
            self.countDiagnostic("generated_representation_roots_finalized");
            const depth = try self.generatedIteratorDepth(node, &depths, &active);
            try pending.append(self.allocator, .{
                .node = node,
                .depth = depth,
                .force_dynamic = self.iteratorRootRequiresForcedDynamic(node),
            });
        }

        for (pending.items) |item| {
            const node = self.find(item.node);
            var named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator representation target stopped being named"),
            };
            named.def.generated = null;
            if (item.force_dynamic or item.depth > generated_iterator_mint_depth_limit) {
                if (named.args.len == 0) {
                    Common.invariant("generated iterator representation had no item argument");
                }
                const lookup = try self.lookupGeneratedIterator(node, .forced_dynamic, &.{}, null);
                if (lookup.existing) |existing| {
                    if (self.find(existing) != node) {
                        try self.unify(node, existing);
                        continue;
                    }
                }
                try self.rewriteGeneratedIteratorAsForcedDynamic(node, named, lookup.digest);
            } else {
                if (item.depth == 0) {
                    Common.invariant("minted iterator representation had zero producer depth");
                }
                named.def.iterator_representation = .minted;
                named.def.iterator_depth = item.depth;
                try self.setContent(node, .{ .named = named });
            }
        }
    }

    fn iteratorRootRequiresForcedDynamic(self: *InstGraph, node: NodeId) bool {
        return self.classContainsMarkedNode(node, &self.forced_dynamic_iterator_roots);
    }

    fn classContainsMarkedNode(
        self: *InstGraph,
        node: NodeId,
        marked: *const collections.DenseMap(NodeId, void),
    ) bool {
        var members = self.classMemberIterator(node);
        while (members.next()) |member| {
            if (marked.contains(member)) return true;
        }
        return false;
    }

    fn rewriteGeneratedIteratorAsForcedDynamic(
        self: *InstGraph,
        node: NodeId,
        source_named: InstNamed,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        const provenance = source_named.generated_iterator orelse
            Common.invariant("forced-dynamic iterator rewrite lacked producer provenance");
        if (source_named.args.len == 0) {
            Common.invariant("forced-dynamic iterator rewrite had no item argument");
        }
        const topology = source_named.def.iterator_topology orelse
            Common.invariant("forced-dynamic iterator rewrite lacked producer topology");
        const item_node = source_named.args[0];
        const dynamic_args = try self.arena().alloc(NodeId, 1);
        dynamic_args[0] = item_node;
        var def = provenance.public_source.def;
        def.generated = null;
        def.iterator_representation = .forced_dynamic;
        def.iterator_kind = .forced_dynamic;
        def.iterator_depth = 0;
        def.iterator_topology = topology;
        try self.setContent(node, .{ .named = .{
            .named_type = provenance.public_source.named_type,
            .def = def,
            .kind = provenance.public_source.kind,
            .builtin_owner = provenance.public_source.builtin_owner,
            .args = dynamic_args,
            .backing = .{
                .node = try self.forcedDynamicIteratorBackingNode(
                    provenance.public_source.backing.node,
                    node,
                    item_node,
                    topology,
                ),
                .use = provenance.public_source.backing.use,
                .authority = .generated_private,
            },
            .generated_iterator = .{
                .callable_evidence = null,
                .components = &.{},
                .public_source = provenance.public_source,
            },
            .declared_order = provenance.public_source.declared_order,
        } });
        try self.indexGeneratedIteratorAtDigest(node, digest);
    }

    fn forcedDynamicIteratorBackingNode(
        self: *InstGraph,
        public_backing: NodeId,
        self_node: NodeId,
        item_node: NodeId,
        topology: Type.IteratorTopology,
    ) Allocator.Error!NodeId {
        const public_fields = (try self.recordNodes(public_backing)).fields;
        const fields = try self.arena().alloc(InstField, public_fields.len);
        for (public_fields, fields) |field, *out| {
            out.* = .{
                .name = field.name,
                .ty = if (self.name_store.recordFieldLabelTextEql(field.name, topology.step_field))
                    try self.forcedDynamicIteratorStepFunctionNode(field.ty, self_node, item_node, topology)
                else
                    field.ty,
            };
        }
        return try self.newNode(.{ .record = .{
            .fields = fields,
            .ext = try self.newNode(.empty_record),
        } });
    }

    fn forcedDynamicIteratorStepFunctionNode(
        self: *InstGraph,
        public_step: NodeId,
        self_node: NodeId,
        item_node: NodeId,
        topology: Type.IteratorTopology,
    ) Allocator.Error!NodeId {
        const step = try self.functionNodes(public_step);
        return try self.newNode(.{ .func = .{
            .args = try self.arena().dupe(NodeId, step.args),
            .ret = try self.forcedDynamicIteratorStepResultNode(step.ret, self_node, item_node, topology),
        } });
    }

    fn forcedDynamicIteratorStepResultNode(
        self: *InstGraph,
        public_result: NodeId,
        self_node: NodeId,
        item_node: NodeId,
        topology: Type.IteratorTopology,
    ) Allocator.Error!NodeId {
        const public_tags = (try self.tagRowNodes(public_result)).tags;
        const tags = try self.arena().alloc(InstTag, public_tags.len);
        for (public_tags, tags) |tag, *out| {
            const payloads = try self.arena().alloc(NodeId, tag.payloads.len);
            for (tag.payloads, payloads) |payload, *payload_out| {
                payload_out.* = try self.forcedDynamicIteratorStepPayloadNode(
                    tag.name,
                    payload,
                    self_node,
                    item_node,
                    topology,
                );
            }
            out.* = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = payloads,
            };
        }
        return try self.newNode(.{ .tag_union = .{
            .tags = tags,
            .ext = try self.newNode(.empty_tag_union),
        } });
    }

    fn forcedDynamicIteratorStepPayloadNode(
        self: *InstGraph,
        tag_name: names.TagNameId,
        public_payload: NodeId,
        self_node: NodeId,
        item_node: NodeId,
        topology: Type.IteratorTopology,
    ) Allocator.Error!NodeId {
        if (!self.name_store.tagLabelTextEql(tag_name, topology.one_tag) and
            !self.name_store.tagLabelTextEql(tag_name, topology.skip_tag)) return public_payload;
        const public_fields = (try self.recordNodes(public_payload)).fields;
        const fields = try self.arena().alloc(InstField, public_fields.len);
        for (public_fields, fields) |field, *out| {
            out.* = .{
                .name = field.name,
                .ty = if (self.name_store.recordFieldLabelTextEql(field.name, topology.rest_field))
                    self_node
                else if (self.name_store.recordFieldLabelTextEql(field.name, topology.item_field))
                    item_node
                else
                    field.ty,
            };
        }
        return try self.newNode(.{ .record = .{
            .fields = fields,
            .ext = try self.newNode(.empty_record),
        } });
    }

    fn generatedIteratorDepth(
        self: *InstGraph,
        raw_node: NodeId,
        depths: *collections.DenseMap(NodeId, u8),
        active: *collections.DenseMap(NodeId, void),
    ) Allocator.Error!u8 {
        const root = self.find(raw_node);
        if (depths.get(root)) |depth| return depth;
        if (active.count() != 0) {
            Common.invariant("generated iterator depth walk retained active nodes between roots");
        }

        var stack = std.ArrayList(GeneratedIteratorDepthFrame).empty;
        defer stack.deinit(self.allocator);
        try self.pushGeneratedIteratorDepthFrame(root, depths, active, &stack);

        while (stack.items.len != 0) {
            const frame_index = stack.items.len - 1;
            if (stack.items[frame_index].max_child_depth <= generated_iterator_mint_depth_limit and
                stack.items[frame_index].next_child < stack.items[frame_index].child_count)
            {
                const child_index = stack.items[frame_index].next_child;
                stack.items[frame_index].next_child += 1;
                const child = self.find(self.generatedIteratorDepthChild(
                    stack.items[frame_index].node,
                    child_index,
                ));
                if (depths.get(child)) |depth| {
                    stack.items[frame_index].max_child_depth = @max(
                        stack.items[frame_index].max_child_depth,
                        depth,
                    );
                } else if (active.contains(child)) {
                    stack.items[frame_index].max_child_depth = generated_iterator_forced_depth;
                } else {
                    try self.pushGeneratedIteratorDepthFrame(child, depths, active, &stack);
                }
                continue;
            }

            const frame = stack.pop().?;
            _ = active.remove(frame.node);
            const depth = if (frame.max_child_depth > generated_iterator_mint_depth_limit or
                (frame.increment != 0 and frame.max_child_depth >= generated_iterator_mint_depth_limit))
                generated_iterator_forced_depth
            else
                frame.max_child_depth + frame.increment;
            try depths.put(frame.node, depth);
            if (stack.items.len == 0) return depth;
            stack.items[stack.items.len - 1].max_child_depth = @max(
                stack.items[stack.items.len - 1].max_child_depth,
                depth,
            );
        }
        unreachable;
    }

    fn pushGeneratedIteratorDepthFrame(
        self: *InstGraph,
        node: NodeId,
        depths: *collections.DenseMap(NodeId, u8),
        active: *collections.DenseMap(NodeId, void),
        stack: *std.ArrayList(GeneratedIteratorDepthFrame),
    ) Allocator.Error!void {
        switch (self.generatedIteratorDepthRule(node)) {
            .fixed => |depth| {
                try depths.put(node, depth);
                if (stack.items.len == 0) {
                    try stack.append(self.allocator, .{
                        .node = node,
                        .next_child = 0,
                        .child_count = 0,
                        .max_child_depth = depth,
                        .increment = 0,
                    });
                    try active.put(node, {});
                } else {
                    stack.items[stack.items.len - 1].max_child_depth = @max(
                        stack.items[stack.items.len - 1].max_child_depth,
                        depth,
                    );
                }
            },
            .children => |children| {
                try active.put(node, {});
                try stack.append(self.allocator, .{
                    .node = node,
                    .next_child = 0,
                    .child_count = children.count,
                    .max_child_depth = 0,
                    .increment = children.increment,
                });
            },
        }
    }

    fn generatedIteratorDepthRule(self: *InstGraph, node: NodeId) GeneratedIteratorDepthRule {
        return switch (self.nodes.items[@intFromEnum(node)]) {
            .redirect => unreachable,
            .unresolved => |variable| switch (variable.origin) {
                .checked_variable, .row_extension => .{ .fixed = 0 },
                .placeholder => Common.invariant("placeholder reached generated iterator representation finalization"),
            },
            .primitive, .empty_tag_union, .empty_record, .erased, .zst, .func => .{ .fixed = 0 },
            .list, .box => .{ .children = .{ .count = 1, .increment = 0 } },
            .tuple => |items| .{ .children = .{ .count = items.len, .increment = 0 } },
            .tag_union => |row| .{ .children = .{
                .count = 1 + tagPayloadCount(row.tags),
                .increment = 0,
            } },
            .record => |row| .{ .children = .{ .count = 1 + row.fields.len, .increment = 0 } },
            .named => |named| blk: {
                if (named.generated_iterator != null) {
                    break :blk switch (named.def.iterator_kind) {
                        .custom,
                        .list,
                        .str,
                        .single,
                        .range_exclusive,
                        .range_inclusive,
                        => .{ .fixed = 1 },
                        .map,
                        .keep_if,
                        .drop_if,
                        .take_first,
                        .drop_first,
                        .concat,
                        .append,
                        => adapter: {
                            const provenance = named.generated_iterator orelse unreachable;
                            if (named.args.len != 1) {
                                Common.invariant("generated iterator adapter had no item argument");
                            }
                            break :adapter .{ .children = .{
                                .count = provenance.components.len,
                                .increment = 1,
                            } };
                        },
                        .join => join: {
                            const provenance = named.generated_iterator orelse unreachable;
                            if (provenance.components.len != 2) {
                                Common.invariant("generated iterator join did not retain two exact inputs");
                            }
                            break :join .{ .children = .{
                                .count = provenance.components.len,
                                .increment = 0,
                            } };
                        },
                        .forced_dynamic => .{ .fixed = generated_iterator_forced_depth },
                        .none => Common.invariant("generated iterator had no producer kind"),
                    };
                }
                break :blk switch (named.def.iterator_representation) {
                    .forced_dynamic => .{ .fixed = generated_iterator_forced_depth },
                    .minted => if (named.def.iterator_depth == 0)
                        Common.invariant("finished minted iterator had zero producer depth")
                    else
                        .{ .fixed = named.def.iterator_depth },
                    .none => .{ .children = .{ .count = named.args.len, .increment = 0 } },
                };
            },
        };
    }

    fn generatedIteratorDepthChild(
        self: *InstGraph,
        node: NodeId,
        child_index: usize,
    ) NodeId {
        return switch (self.nodes.items[@intFromEnum(node)]) {
            .list, .box => |child| child,
            .tuple => |items| items[child_index],
            .tag_union => |row| if (child_index == 0)
                row.ext
            else
                tagPayloadAt(row.tags, child_index - 1),
            .record => |row| if (child_index == 0)
                row.ext
            else
                row.fields[child_index - 1].ty,
            .named => |named| if (named.generated_iterator) |provenance|
                provenance.components[child_index]
            else
                named.args[child_index],
            .redirect, .unresolved, .primitive, .func, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator depth frame had no structural child"),
        };
    }

    fn generatedIteratorIdentityInputDigest(
        self: *InstGraph,
        finalizer: *GeneratedIteratorIdentityFinalizer,
        node: NodeId,
    ) Allocator.Error!names.TypeDigest {
        var writer = OpenFunctionInterfaceShapeWriter.initGeneratedIdentity(self, finalizer);
        defer writer.deinit();
        writer.writeBytes("roc.monotype.generated_iterator.identity_input.v2");
        try writer.writeNode(node);
        return .{ .bytes = writer.hasher.finalResult() };
    }

    /// Seal producer identities for graph-owned iterator representations only
    /// after all type relations and representation decisions have been
    /// applied. Nested generated iterators contribute their memoized stable
    /// identity instead of recursively expanding their implementation graph.
    /// All digests are computed before any node is stamped. Equal identities
    /// are coalesced at this relation-finalization barrier, before Monotype
    /// sealing or specialization discovery can observe or process duplicate
    /// private backings. Construction-time interning already handles inputs
    /// whose equality is known early; this pass handles only inputs that stayed
    /// independently unresolved until their language defaults became final.
    pub fn finalizeGeneratedIteratorIdentities(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        const Pending = struct { node: NodeId, digest: names.TypeDigest };
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        var finalizer = GeneratedIteratorIdentityFinalizer.init(self);
        defer finalizer.deinit();

        for (self.generated_iterator_nodes.items) |registered_node| {
            const node = self.find(registered_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("registered generated iterator stopped being named"),
            };
            if (named.generated_iterator == null) {
                Common.invariant("registered generated iterator lost its producer provenance");
            }
            self.countDiagnostic("generated_identity_roots_finalized");
            try pending.append(self.allocator, .{
                .node = node,
                .digest = try finalizer.digestFor(node),
            });
        }

        var authoritative_by_identity = std.HashMap(
            names.TypeDigest,
            NodeId,
            GeneratedIteratorInternContext,
            80,
        ).init(self.allocator);
        defer authoritative_by_identity.deinit();
        for (pending.items) |item| {
            const entry = try authoritative_by_identity.getOrPut(item.digest);
            if (!entry.found_existing) {
                entry.value_ptr.* = item.node;
                continue;
            }
            const authoritative = self.find(entry.value_ptr.*);
            const duplicate = self.find(item.node);
            if (authoritative == duplicate) continue;
            try self.unify(authoritative, duplicate);
            entry.value_ptr.* = self.find(authoritative);
            self.countDiagnostic("generated_identity_roots_coalesced");
        }

        seen.clearRetainingCapacity();
        for (pending.items) |item| {
            const node = self.find(item.node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            var named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator identity target stopped being named"),
            };
            named.def.generated = item.digest;
            try self.setContent(node, .{ .named = named });
        }
    }

    /// Relate every finalized graph-owned generated iterator whose durable
    /// identity already exists to that authoritative Monotype before relations are
    /// frozen. Importing and unifying the complete type preserves the exact
    /// backing correspondence; a root-only sealing shortcut would leave
    /// independently requested backing descendants free to acquire duplicate
    /// TypeIds.
    pub fn bindGeneratedIteratorAuthoritativeTypes(
        self: *InstGraph,
        generated_types_by_identity: *std.AutoHashMap(names.TypeDigest, Type.TypeId),
    ) Allocator.Error!void {
        self.requireRelationProduction();

        // A finished imported generated type is already a valid durable
        // authoritative tree. Record it before binding graph-owned producers so a
        // graph that first encounters an identity through an import and also
        // produces that identity still chooses one tree before freezing.
        for (self.imported_generated_iterator_nodes.items) |registered_node| {
            const imported_ty = self.imported_monos.get(registered_node) orelse
                Common.invariant("imported generated iterator lost its durable Monotype");
            const imported_named = switch (self.types.get(imported_ty)) {
                .named => |named| named,
                .primitive, .list, .box, .tuple, .func, .tag_union, .record, .erased, .zst => Common.invariant("imported generated iterator TypeId stopped being named"),
            };
            const identity = imported_named.def.generated orelse
                Common.invariant("imported generated iterator had no durable identity");
            const authoritative = try generated_types_by_identity.getOrPut(identity);
            if (!authoritative.found_existing) {
                authoritative.value_ptr.* = imported_ty;
            } else if (authoritative.value_ptr.* != imported_ty and
                !try self.types.typeEql(self.name_store, authoritative.value_ptr.*, imported_ty))
            {
                Common.invariant("one generated identity named different durable Monotypes");
            }
        }

        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();

        for (self.generated_iterator_nodes.items) |registered_node| {
            const node = self.find(registered_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("registered generated iterator stopped being named"),
            };
            const identity = named.def.generated orelse
                Common.invariant("generated iterator reached authoritative binding without a finalized identity");
            const authoritative_ty = generated_types_by_identity.get(identity) orelse continue;
            const authoritative_node = try self.importGeneratedAuthoritativeMono(authoritative_ty);
            try self.unify(node, authoritative_node);
            self.countDiagnostic("generated_type_store_hits");
        }
        const imported_count = self.imported_generated_iterator_nodes.items.len;
        var imported_index: usize = 0;
        while (imported_index < imported_count) : (imported_index += 1) {
            const registered_node = self.imported_generated_iterator_nodes.items[imported_index];
            const node = self.find(registered_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("imported generated iterator stopped being named"),
            };
            const identity = named.def.generated orelse
                Common.invariant("imported generated iterator had no durable identity");
            const authoritative_ty = generated_types_by_identity.get(identity) orelse continue;
            const authoritative_node = try self.importGeneratedAuthoritativeMono(authoritative_ty);
            try self.unify(node, authoritative_node);
        }
        try self.indexGeneratedAuthoritativeMonosByRoot();
    }

    fn indexGeneratedAuthoritativeMonosByRoot(self: *InstGraph) Allocator.Error!void {
        var by_root = collections.DenseMap(NodeId, Type.TypeId).init(self.allocator);
        errdefer by_root.deinit();
        var imported = self.generated_authoritative_monos.iterator();
        while (imported.next()) |entry| {
            const root = self.find(entry.key_ptr.*);
            const authoritative = try by_root.getOrPut(root);
            if (!authoritative.found_existing) {
                authoritative.value_ptr.* = entry.value_ptr.*;
            } else if (authoritative.value_ptr.* != entry.value_ptr.*) {
                if (!try self.types.typeEql(self.name_store, authoritative.value_ptr.*, entry.value_ptr.*)) {
                    Common.invariant("one finalized graph class retained different authoritative Monotypes");
                }
                if (@intFromEnum(entry.value_ptr.*) < @intFromEnum(authoritative.value_ptr.*)) {
                    authoritative.value_ptr.* = entry.value_ptr.*;
                }
            }
        }
        self.generated_authoritative_monos.deinit();
        self.generated_authoritative_monos = by_root;
    }

    pub fn finalizesAsClosedEmptyTagUnion(self: *InstGraph, raw_node: NodeId) bool {
        var node = self.find(raw_node);
        var remaining = self.nodes.items.len;
        while (remaining > 0) : (remaining -= 1) {
            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .empty_tag_union => return true,
                .unresolved => |variable| {
                    if (variable.numeric_default_phase != null) return false;
                    if (variable.row_default) |row_default| return row_default == .empty_tag_union;
                    return switch (variable.origin) {
                        .checked_variable => true,
                        .row_extension => Common.invariant("row extension reached final demand validation without row default"),
                        .placeholder => Common.invariant("instantiation placeholder reached final demand validation"),
                    };
                },
                .named => |named| {
                    const backing = named.backing orelse return false;
                    if (backing.use != .inspectable) return false;
                    node = self.find(backing.node);
                },
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .tag_union,
                .record,
                .empty_record,
                .erased,
                .zst,
                => return false,
            }
        }
        Common.invariant("named Monotype backing cycle reached final demand validation");
    }

    /// Whether an inhabitance proof over this node could still hold under any
    /// future relations—the monotone counterpart of
    /// `finalizesAsUninhabited`. A class that carries a numeric default
    /// finalizes numeric and inhabited-only content (primitives, lists,
    /// functions, empty records) is permanent, so `false` here is stable for
    /// the rest of this graph's lifetime. Anything still unresolved (or a
    /// named type whose backing has not been recorded) answers `true`
    /// conservatively.
    pub fn mayFinalizeAsUninhabited(self: *InstGraph, raw_node: NodeId) Allocator.Error!bool {
        var visiting = collections.DenseMap(NodeId, void).init(self.allocator);
        defer visiting.deinit();
        return try self.mayFinalizeAsUninhabitedInner(self.find(raw_node), &visiting);
    }

    fn mayFinalizeAsUninhabitedInner(
        self: *InstGraph,
        raw_node: NodeId,
        visiting: *collections.DenseMap(NodeId, void),
    ) Allocator.Error!bool {
        const node = self.find(raw_node);
        const entry = try visiting.getOrPut(node);
        if (entry.found_existing) return false;
        defer _ = visiting.remove(node);

        return switch (self.nodes.items[@intFromEnum(node)]) {
            .redirect => unreachable,
            .empty_tag_union => true,
            .unresolved => |variable| variable.numeric_default_phase == null,
            .named => |named| if (named.backing) |backing|
                try self.mayFinalizeAsUninhabitedInner(backing.node, visiting)
            else
                true,
            .box => |payload| try self.mayFinalizeAsUninhabitedInner(payload, visiting),
            .tuple => |items| blk: {
                for (items) |item| {
                    if (try self.mayFinalizeAsUninhabitedInner(item, visiting)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.mayFinalizeAsUninhabitedInner(field.ty, visiting)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                if (!try self.mayFinalizeAsUninhabitedInner(tag_union.ext, visiting)) break :blk false;
                for (tag_union.tags) |tag| {
                    var tag_may_be_uninhabited = false;
                    for (tag.payloads) |payload| {
                        if (try self.mayFinalizeAsUninhabitedInner(payload, visiting)) {
                            tag_may_be_uninhabited = true;
                            break;
                        }
                    }
                    if (!tag_may_be_uninhabited) break :blk false;
                }
                break :blk true;
            },
            .primitive,
            .list,
            .func,
            .empty_record,
            .erased,
            .zst,
            => false,
        };
    }

    /// Whether frozen graph structure proves that no runtime value can inhabit
    /// this node after unresolved checked variables apply their recorded final
    /// defaults. This is used only for explicit reachability guards captured
    /// while lowering a branch; it never manufactures a durable type view.
    pub fn finalizesAsUninhabited(self: *InstGraph, raw_node: NodeId) Allocator.Error!bool {
        self.requireFrozenRelations();
        var visiting = collections.DenseMap(NodeId, void).init(self.allocator);
        defer visiting.deinit();
        return try self.finalizesAsUninhabitedInner(self.find(raw_node), &visiting);
    }

    fn finalizesAsUninhabitedInner(
        self: *InstGraph,
        raw_node: NodeId,
        visiting: *collections.DenseMap(NodeId, void),
    ) Allocator.Error!bool {
        const node = self.find(raw_node);
        const entry = try visiting.getOrPut(node);
        if (entry.found_existing) return false;
        defer _ = visiting.remove(node);

        return switch (self.nodes.items[@intFromEnum(node)]) {
            .redirect => unreachable,
            .empty_tag_union => true,
            .unresolved => |variable| blk: {
                if (variable.numeric_default_phase != null) break :blk false;
                if (variable.row_default) |row_default| break :blk row_default == .empty_tag_union;
                break :blk switch (variable.origin) {
                    .checked_variable => true,
                    .row_extension => Common.invariant("row extension reached final inhabitance validation without row default"),
                    .placeholder => Common.invariant("instantiation placeholder reached final inhabitance validation"),
                };
            },
            .named => |named| if (named.backing) |backing|
                if (backing.use == .inspectable)
                    self.finalizesAsUninhabitedInner(backing.node, visiting)
                else
                    false
            else
                false,
            .box => |payload| self.finalizesAsUninhabitedInner(payload, visiting),
            .tuple => |items| blk: {
                for (items) |item| {
                    if (try self.finalizesAsUninhabitedInner(item, visiting)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.finalizesAsUninhabitedInner(field.ty, visiting)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                if (!try self.finalizesAsUninhabitedInner(tag_union.ext, visiting)) break :blk false;
                for (tag_union.tags) |tag| {
                    var tag_is_inhabited = true;
                    for (tag.payloads) |payload| {
                        if (try self.finalizesAsUninhabitedInner(payload, visiting)) {
                            tag_is_inhabited = false;
                            break;
                        }
                    }
                    if (tag_is_inhabited) break :blk false;
                }
                break :blk true;
            },
            .primitive,
            .list,
            .func,
            .empty_record,
            .erased,
            .zst,
            => false,
        };
    }

    pub fn newNode(self: *InstGraph, node_content: InstNode) Allocator.Error!NodeId {
        self.requireRelationProduction();
        const id: NodeId = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        try self.nodes.append(self.allocator, node_content);
        try self.versions.append(self.allocator, 0);
        try self.class_member_next.append(self.allocator, null);
        try self.class_member_head.append(self.allocator, id);
        try self.class_member_tail.append(self.allocator, id);
        try self.row_exts.append(self.allocator, null);
        try self.request_checked_sources.append(self.allocator, null);
        try self.request_substitution_spans.append(self.allocator, .uninitialized);
        try self.registerRowParent(id, node_content);
        self.countDiagnostic("nodes_created");
        return id;
    }

    /// Reserve a graph node before constructing content that recursively
    /// refers to it. The placeholder is graph-only and must be filled before
    /// relation production can complete.
    pub fn addRecursiveNode(
        self: *InstGraph,
        context: anytype,
        comptime fill: fn (@TypeOf(context), NodeId) Allocator.Error!InstNode,
    ) Allocator.Error!NodeId {
        const reserved = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        try self.setContent(reserved, try fill(context, reserved));
        return reserved;
    }

    pub fn nominalBackingNode(
        self: *InstGraph,
        module_bytes: [32]u8,
        declaration_id: u32,
        args: []const NodeId,
    ) ?NodeId {
        const bucket = self.nominal_backings.getPtr(.{
            .module_bytes = module_bytes,
            .declaration_id = declaration_id,
        }) orelse return null;
        instances: for (bucket.items) |*instance| {
            if (instance.args.len != args.len) continue;
            for (instance.args, args) |*stored, wanted| {
                stored.* = self.find(stored.*);
                if (stored.* != self.find(wanted)) continue :instances;
            }
            return instance.node;
        }
        return null;
    }

    pub fn putNominalBackingNode(
        self: *InstGraph,
        module_bytes: [32]u8,
        declaration_id: u32,
        args: []const NodeId,
        node: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const stored_args = try self.arena().alloc(NodeId, args.len);
        for (stored_args, args) |*stored, arg| {
            stored.* = self.find(arg);
        }
        const bucket = try self.nominal_backings.getOrPut(.{
            .module_bytes = module_bytes,
            .declaration_id = declaration_id,
        });
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        try bucket.value_ptr.append(self.allocator, .{ .args = stored_args, .node = node });
    }

    fn registerRowParent(self: *InstGraph, row: NodeId, node_content: InstNode) Allocator.Error!void {
        const row_root = self.find(row);
        const maybe_ext = if (node_content == .tag_union)
            node_content.tag_union.ext
        else if (node_content == .record)
            node_content.record.ext
        else
            null;
        const ext = if (maybe_ext) |raw_ext| self.find(raw_ext) else {
            try self.unregisterRowParent(row_root);
            return;
        };

        const row_ext = &self.row_exts.items[@intFromEnum(row_root)];
        if (row_ext.*) |old_ext| {
            if (old_ext == ext) {
                try self.addRowParent(ext, row_root);
                return;
            }
            self.removeRowParent(old_ext, row_root);
        }
        row_ext.* = ext;
        try self.addRowParent(ext, row_root);
    }

    fn unregisterRowParent(self: *InstGraph, row: NodeId) Allocator.Error!void {
        const row_root = self.find(row);
        const row_ext = &self.row_exts.items[@intFromEnum(row_root)];
        if (row_ext.*) |old| {
            row_ext.* = null;
            self.removeRowParent(old, row_root);
        }
    }

    fn addRowParent(self: *InstGraph, ext: NodeId, row: NodeId) Allocator.Error!void {
        const entry = try self.row_parents.getOrPut(self.find(ext));
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        const row_root = self.find(row);
        for (entry.value_ptr.items) |existing| {
            if (self.find(existing) == row_root) return;
        }
        try entry.value_ptr.append(self.allocator, row_root);
    }

    fn removeRowParent(self: *InstGraph, ext: NodeId, row: NodeId) void {
        const ext_root = self.find(ext);
        const parents = self.row_parents.getPtr(ext_root) orelse return;
        const row_root = self.find(row);
        var index: usize = 0;
        while (index < parents.items.len) {
            if (self.find(parents.items[index]) == row_root) {
                _ = parents.swapRemove(index);
                continue;
            }
            index += 1;
        }
        if (parents.items.len == 0) {
            var removed = self.row_parents.fetchRemove(ext_root).?;
            removed.value.deinit(self.allocator);
        }
    }

    fn find(self: *InstGraph, id: NodeId) NodeId {
        var current = id;
        while (true) {
            const node = self.nodes.items[@intFromEnum(current)];
            if (node == .redirect) current = node.redirect else break;
        }
        // Path compression: repoint every redirect on the chain at the root.
        var walk = id;
        while (walk != current) {
            const redirect = self.nodes.items[@intFromEnum(walk)];
            if (redirect != .redirect) unreachable;
            const next = redirect.redirect;
            self.nodes.items[@intFromEnum(walk)] = .{ .redirect = current };
            walk = next;
        }
        return current;
    }

    pub fn content(self: *InstGraph, id: NodeId) InstNode {
        return self.nodes.items[@intFromEnum(self.find(id))];
    }

    /// Current root for the node's union-find class.
    pub fn rootNode(self: *InstGraph, id: NodeId) NodeId {
        return self.find(id);
    }

    /// Whether two live cells already belong to the same union-find class.
    pub fn sameClass(self: *InstGraph, left: NodeId, right: NodeId) bool {
        return self.find(left) == self.find(right);
    }

    pub const ClassMemberIterator = struct {
        graph: *const InstGraph,
        current: ?NodeId,

        pub fn next(self: *ClassMemberIterator) ?NodeId {
            const member = self.current orelse return null;
            self.current = self.graph.class_member_next.items[@intFromEnum(member)];
            return member;
        }
    };

    /// Permanent node ids currently joined to the requested node by explicit
    /// graph relations. Open draft lookup probes these stable aliases directly.
    pub fn classMemberIterator(self: *InstGraph, node: NodeId) ClassMemberIterator {
        const root = self.find(node);
        return .{ .graph = self, .current = self.class_member_head.items[@intFromEnum(root)] };
    }

    /// Collision authority for open function-interface lookup buckets.
    pub fn sameFunctionInterface(self: *InstGraph, left: NodeId, right: NodeId) bool {
        const left_content = self.content(left);
        if (left_content != .func) Common.invariant("draft function interface comparison received a non-function left request");
        const left_fn = left_content.func;
        const right_content = self.content(right);
        if (right_content != .func) Common.invariant("draft function interface comparison received a non-function right request");
        const right_fn = right_content.func;
        if (left_fn.args.len != right_fn.args.len) return false;
        for (left_fn.args, right_fn.args) |left_arg, right_arg| {
            if (!self.sameClass(left_arg, right_arg)) return false;
        }
        return self.sameClass(left_fn.ret, right_fn.ret);
    }

    /// Alpha-normalized shape of an open function interface. This is a
    /// graph-local lookup key for unresolved draft requests: concrete
    /// structure is written directly, while unresolved union-find classes are
    /// numbered by first occurrence so interface aliasing is preserved without
    /// depending on fresh node ids. Producer-owned source-interface and
    /// recursive-representation evidence participate because they can change
    /// how an otherwise identical open shape finalizes.
    /// Capture the exact open-interface shape before a callee body can refine
    /// its live graph nodes. The bytes are graph-arena owned and must not escape
    /// draft specialization lookup.
    pub fn openFunctionInterfaceShape(self: *InstGraph, node: NodeId) Allocator.Error!OpenFunctionInterfaceShape {
        var sizing_finalizer = GeneratedIteratorIdentityFinalizer.init(self);
        defer sizing_finalizer.deinit();
        var sizing = OpenFunctionInterfaceShapeWriter.init(self);
        defer sizing.deinit();
        sizing.generated_identity_finalizer = &sizing_finalizer;
        try sizing.writeFunctionInterface(node);
        const digest: names.TypeDigest = .{ .bytes = sizing.hasher.finalResult() };

        const bytes = try self.arena().alloc(u8, sizing.output_len);
        var output_finalizer = GeneratedIteratorIdentityFinalizer.init(self);
        defer output_finalizer.deinit();
        var writer = OpenFunctionInterfaceShapeWriter.initWithOutput(self, bytes);
        defer writer.deinit();
        writer.generated_identity_finalizer = &output_finalizer;
        try writer.writeFunctionInterface(node);
        if (writer.output_len != bytes.len) {
            Common.invariant("open function-interface shape changed while being captured");
        }
        const written_digest: names.TypeDigest = .{ .bytes = writer.hasher.finalResult() };
        if (!std.mem.eql(u8, &digest.bytes, &written_digest.bytes)) {
            Common.invariant("open function-interface shape digest differed from its exact bytes");
        }
        if (sizing.primary_resolved != writer.primary_resolved) {
            Common.invariant("open function-interface resolution changed while being captured");
        }
        return .{ .digest = digest, .bytes = bytes, .resolved = sizing.primary_resolved };
    }

    /// Whether a live graph type is already closed and can be snapshotted
    /// without applying any unresolved-variable or row default. Draft
    /// specialization lookup uses closed snapshots as its direct key; open
    /// requests remain graph-local until explicit recursive-edge identity or
    /// final body sealing resolves them.
    pub fn typeIsResolved(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try pending.append(self.allocator, root);
        while (pending.pop()) |raw_node| {
            const node = self.find(raw_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .unresolved => return false,
                .primitive, .empty_tag_union, .empty_record, .erased, .zst => {},
                .list, .box => |child| try pending.append(self.allocator, child),
                .tuple => |items| try pending.appendSlice(self.allocator, items),
                .func => |function| {
                    try pending.appendSlice(self.allocator, function.args);
                    try pending.append(self.allocator, function.ret);
                },
                .tag_union => |row| {
                    if (!try self.rowExtensionChainResolved(node, .tag_union)) return false;
                    for (row.tags) |tag| try pending.appendSlice(self.allocator, tag.payloads);
                    try pending.append(self.allocator, row.ext);
                },
                .record => |row| {
                    if (!try self.rowExtensionChainResolved(node, .record)) return false;
                    for (row.fields) |field| try pending.append(self.allocator, field.ty);
                    try pending.append(self.allocator, row.ext);
                },
                .named => |named| {
                    try pending.appendSlice(self.allocator, named.args);
                    if (named.backing) |backing| try pending.append(self.allocator, backing.node);
                    for (named.declared_order) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding| try pending.append(self.allocator, padding),
                    };
                },
            }
        }
        return true;
    }

    /// Materialize the checked literal default recorded on an otherwise-open
    /// instantiation node. Literal lowering calls this only when runtime demand
    /// reaches an unpinned literal leaf; custom specializations have already
    /// related the node to their concrete target before that point.
    pub fn materializeLiteralDefault(self: *InstGraph, raw_node: NodeId) Allocator.Error!void {
        self.requireRelationProduction();
        const node = self.find(raw_node);
        const node_content = self.nodes.items[@intFromEnum(node)];
        if (node_content != .unresolved) Common.invariant("literal default materialization received a non-variable node");
        const variable = node_content.unresolved;
        const phase = variable.numeric_default_phase orelse
            Common.invariant("unresolved literal leaf had no checked default phase");
        const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
            Common.invariant("checking-finalized literal variable reached Monotype unresolved");
        try self.setContent(node, switch (target) {
            .dec => .{ .primitive = .dec },
            .str => .{ .primitive = .str },
        });
    }

    /// Whether evidence finalization has explicit producer provenance for every
    /// node in this live type. Numeric and row defaults are direct closure
    /// evidence. A plain checked variable is provisionally sealable as the
    /// language's truly-unconstrained empty union only when the caller proves,
    /// by comparing after all dependent lowering, that no later relation
    /// refined it. Row extensions and compiler placeholders require their own
    /// explicit data and can never use that rule.
    pub fn typeCanSealFromExplicitEvidence(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try pending.append(self.allocator, root);
        while (pending.pop()) |raw_node| {
            const node = self.find(raw_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .unresolved => |variable| {
                    const numeric_default = if (variable.numeric_default_phase) |phase|
                        checked.literal_defaulting.defaultTargetForPhase(phase) != null
                    else
                        false;
                    if (!numeric_default and variable.row_default == null and variable.origin != .checked_variable) return false;
                },
                .primitive, .empty_tag_union, .empty_record, .erased, .zst => {},
                .list, .box => |child| try pending.append(self.allocator, child),
                .tuple => |items| try pending.appendSlice(self.allocator, items),
                .func => |function| {
                    try pending.appendSlice(self.allocator, function.args);
                    try pending.append(self.allocator, function.ret);
                },
                .tag_union => |row| {
                    for (row.tags) |tag| try pending.appendSlice(self.allocator, tag.payloads);
                    try pending.append(self.allocator, row.ext);
                },
                .record => |row| {
                    for (row.fields) |field| try pending.append(self.allocator, field.ty);
                    try pending.append(self.allocator, row.ext);
                },
                .named => |named| {
                    try pending.appendSlice(self.allocator, named.args);
                    if (named.backing) |backing| try pending.append(self.allocator, backing.node);
                    for (named.declared_order) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding| try pending.append(self.allocator, padding),
                    };
                },
            }
        }
        return true;
    }

    fn rowExtensionChainResolved(self: *InstGraph, raw_root: NodeId, kind: RowKind) Allocator.Error!bool {
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        var current = self.find(raw_root);
        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) return false;
            switch (self.nodes.items[@intFromEnum(current)]) {
                .tag_union => |row| {
                    if (kind != .tag_union) return false;
                    current = self.find(row.ext);
                },
                .record => |row| {
                    if (kind != .record) return false;
                    current = self.find(row.ext);
                },
                .empty_tag_union => return kind == .tag_union,
                .empty_record => return kind == .record,
                .unresolved => return false,
                .redirect,
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .named,
                .erased,
                .zst,
                => return false,
            }
        }
    }
    /// Apply one exact produced type to an already-checked specialization
    /// request. This is directed substitution, not type checking and not
    /// symmetric equality. Matching compound structure is visited once so
    /// checked variables receive their produced arguments. A generated nominal
    /// is handled only when that exact node is reached, and its produced root is
    /// never merged into its public nominal.
    pub fn applyProducedTypeToRequest(self: *InstGraph, request_node: NodeId, produced_node: NodeId) Allocator.Error!NodeId {
        return try self.applyTypeToRequest(request_node, produced_node, .exact_producer);
    }

    /// Apply a checker-authored type scheme or view to one exact Monotype
    /// instantiation. Unlike an exact producer relation, this may cross from a
    /// named checked view into the definition-private structural view recorded
    /// by checking. It still preserves the exact node as the authority.
    pub fn applyCheckedTypeMapping(self: *InstGraph, checked_node: NodeId, exact_node: NodeId) Allocator.Error!NodeId {
        return try self.applyTypeToRequest(checked_node, exact_node, .checked_mapping);
    }

    /// Copy substitutions that this specialization has already selected into
    /// a fresh occurrence of the same checked type. An unresolved source is
    /// not a substitution, so it deliberately leaves the fresh occurrence
    /// independent. This preserves producer-owned exact roots while carrying
    /// ordinary choices such as a generic numeric slot specialized to U16.
    pub fn applySelectedCheckedSubstitutions(
        self: *InstGraph,
        fresh_checked_node: NodeId,
        current_checked_node: NodeId,
    ) Allocator.Error!NodeId {
        return try self.applyTypeToRequest(
            fresh_checked_node,
            current_checked_node,
            .selected_substitutions,
        );
    }

    fn applyTypeToRequest(
        self: *InstGraph,
        request_node: NodeId,
        produced_node: NodeId,
        kind: TypeApplicationKind,
    ) Allocator.Error!NodeId {
        self.requireRelationProduction();
        self.countDiagnostic("produced_type_requests");
        if (self.find(request_node) == self.find(produced_node)) return self.find(produced_node);
        if (self.applying_produced_type) {
            Common.invariant("produced-type substitution reentered its graph-owned worklist");
        }
        self.applying_produced_type = true;
        defer self.applying_produced_type = false;
        // A compound relation depends on every descendant pair it visits, not
        // only on the versions of its two roots. Retain the allocation across
        // requests, but keep the entries local to one traversal so child
        // mutations in later requests cannot make a cached parent pair stale.
        self.processed_produced_relations.clearRetainingCapacity();
        defer self.processed_produced_relations.clearRetainingCapacity();
        self.produced_type_pending.clearRetainingCapacity();
        defer self.produced_type_pending.clearRetainingCapacity();
        try self.produced_type_pending.append(self.allocator, .{ .left = request_node, .right = produced_node });
        while (self.produced_type_pending.pop()) |pair| {
            try self.applyProducedTypePair(pair.left, pair.right, kind, &self.produced_type_pending);
        }
        return self.find(produced_node);
    }

    fn applyProducedTypePair(
        self: *InstGraph,
        raw_public: NodeId,
        raw_private: NodeId,
        kind: TypeApplicationKind,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        const public_node = self.find(raw_public);
        const private_node = self.find(raw_private);
        if (public_node == private_node) return;
        const relation = ProducedRelationStamp{
            .request = public_node,
            .request_version = self.versions.items[@intFromEnum(public_node)],
            .produced = private_node,
            .produced_version = self.versions.items[@intFromEnum(private_node)],
        };
        if (self.processed_produced_relations.contains(relation)) {
            self.countDiagnostic("produced_type_cycle_hits");
            return;
        }
        try self.processed_produced_relations.put(relation, {});
        self.countDiagnostic("produced_type_pairs_visited");

        const public_content = self.nodes.items[@intFromEnum(public_node)];
        const private_content = self.nodes.items[@intFromEnum(private_node)];

        // A checked occurrence can already have consumed this specialization's
        // exact nominal selection before a later interface constraint creates
        // a fresh public view. Normalize that checked-mapping pair here: the
        // exact nominal remains authoritative, and the public node contributes
        // only its checker-proved interface relation.
        if (kind == .checked_mapping and
            isGeneratedPrivateRootContent(public_content) and
            private_content == .named and
            private_content.named.backing != null and
            private_content.named.backing.?.authority == .checked_public and
            sameTypeDef(public_content.named.def, private_content.named.def))
        {
            try self.relateGeneratedOpaquePair(private_content, public_content.named, pending);
            return;
        }

        // A still-open cell contains no selected substitution to copy. Do not
        // merge it with the fresh occurrence: either side may later receive a
        // different producer-owned exact representation of the same checked
        // type variable.
        if (kind == .selected_substitutions and private_content == .unresolved) return;

        // Aliases are not runtime representation boundaries. Preserve the
        // exact produced root while applying the request through either
        // side's checker-authored transparent backing.
        if (public_content == .named and public_content.named.kind == .alias) {
            const backing = public_content.named.backing orelse
                Common.invariant("produced-type substitution found an alias request without backing");
            try self.relateOpaqueChild(backing.node, private_node, pending);
            return;
        }
        if (private_content == .named and private_content.named.kind == .alias) {
            const backing = private_content.named.backing orelse
                Common.invariant("produced-type substitution found an exact alias without backing");
            try self.relateOpaqueChild(public_node, backing.node, pending);
            return;
        }

        if (public_content == .named and private_content != .named) {
            const backing = public_content.named.backing orelse
                Common.invariant("checked type mapping found a named view without backing");
            if (kind == .checked_mapping or backing.authority == .checked_public) {
                // Ordinary nominal requests are handled at the exact point
                // where the traversal encounters them. The IR boundary emits
                // the explicit nominal constructor; this relation only maps
                // the checker-authorized backing to the produced structure.
                try self.relateOpaqueChild(backing.node, private_node, pending);
                return;
            }
        }

        if (isGeneratedPrivateRootContent(public_content) and isGeneratedPrivateRootContent(private_content)) {
            if (self.sameExactGeneratedPrivateIdentity(public_content.named, private_content.named)) {
                // Equal content-addressed private identities are already the
                // same runtime type. Keep the current producer's complete
                // node as authority without recursively relating a stale
                // request backing that may still contain public cells.
                try self.union_(private_node, public_node);
            } else {
                // A destination request can itself carry an earlier exact
                // identity. The current producer remains authoritative: bind
                // only the public checked arguments and keep the two private
                // roots distinct. Choosing a common runtime representation is
                // reserved for an explicit control-flow or recursion join.
                if (public_content.named.args.len != private_content.named.args.len) {
                    Common.invariant("generated-private request and producer had different public argument arities");
                }
                for (public_content.named.args, private_content.named.args) |public_arg, private_arg| {
                    try self.relateOpaqueChild(public_arg, private_arg, pending);
                }
            }
            return;
        }
        switch (private_content) {
            .named => |private_named| if (private_named.backing) |backing| {
                if (backing.authority == .generated_private) {
                    // A structural request names the representation expected
                    // below an explicit constructor layer. Generated nominals
                    // obey the same rule as every other nominal: relate that
                    // request directly to the produced backing. Only a public
                    // nominal interface needs the generated/public argument
                    // relation below.
                    if (public_content != .named and public_content != .unresolved) {
                        try self.relateOpaqueChild(public_node, backing.node, pending);
                        return;
                    }
                    switch (public_content) {
                        .unresolved => |public_var| {
                            if (public_var.numeric_default_phase != null or public_var.row_default != null) {
                                Common.invariant("generated-private substitution received a defaultable request variable");
                            }
                            // A generic checked slot has no public nominal
                            // contract to preserve. Select the exact produced
                            // nominal directly; only an already-materialized
                            // public nominal takes the interface path below.
                            try self.selectNamedOverUnresolved(private_node, public_node);
                            return;
                        },
                        .redirect, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => {},
                    }
                    try self.relateGeneratedOpaquePair(public_content, private_named, pending);
                    return;
                }
            },
            .unresolved => {
                try self.unify(public_node, private_node);
                return;
            },
            .redirect, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => {},
        }

        // An exact produced nominal is an explicit constructor layer. A
        // structural request maps to its backing without discarding that
        // produced root.
        if (public_content != .named and public_content != .unresolved and private_content == .named) {
            const backing = private_content.named.backing orelse
                Common.invariant("produced-type substitution found an exact nominal without backing");
            try self.relateOpaqueChild(public_node, backing.node, pending);
            return;
        }

        switch (public_content) {
            .redirect => unreachable,
            .unresolved => {
                // A checked type variable is a substitution slot. Bind it to
                // the exact produced type while keeping the produced node as
                // the class authority.
                if (private_content == .named) {
                    try self.selectNamedOverUnresolved(private_node, public_node);
                } else {
                    try self.union_(private_node, public_node);
                }
                return;
            },
            .primitive => |public_primitive| {
                if (private_content != .primitive) Common.invariant("opaque interface relation received different type structure");
                if (public_primitive != private_content.primitive) {
                    Common.invariant("opaque interface relation received different primitive types");
                }
            },
            .list => |public_elem| {
                if (private_content != .list) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueChild(public_elem, private_content.list, pending);
            },
            .box => |public_elem| {
                if (private_content != .box) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueChild(public_elem, private_content.box, pending);
            },
            .tuple => |public_items| {
                if (private_content != .tuple) Common.invariant("opaque interface relation received different type structure");
                const private_items = private_content.tuple;
                if (public_items.len != private_items.len) {
                    Common.invariant("opaque interface relation received tuples of different arity");
                }
                for (public_items, private_items) |public_item, private_item| {
                    try self.relateOpaqueChild(public_item, private_item, pending);
                }
            },
            .func => |public_fn| {
                if (private_content != .func) Common.invariant("opaque interface relation received different type structure");
                const private_fn = private_content.func;
                if (public_fn.args.len != private_fn.args.len) {
                    Common.invariant("opaque interface relation received functions of different arity");
                }
                for (public_fn.args, private_fn.args) |public_arg, private_arg| {
                    try self.relateOpaqueChild(public_arg, private_arg, pending);
                }
                try self.relateOpaqueChild(public_fn.ret, private_fn.ret, pending);
            },
            .tag_union => {
                if (private_content != .tag_union) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueTagRows(public_node, private_node, kind, pending);
            },
            .record => {
                if (private_content != .record) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueRecordRows(public_node, private_node, kind, pending);
            },
            .empty_tag_union => if (private_content != .empty_tag_union)
                Common.invariant("opaque interface relation received different type structure"),
            .empty_record => if (private_content != .empty_record)
                Common.invariant("opaque interface relation received different type structure"),
            .named => |public_named| {
                if (private_content != .named) Common.invariant("opaque interface relation received different type structure");
                try self.relatePublicNamedOpaquePair(public_named, private_content.named, pending);
            },
            .erased => |public_digest| {
                if (private_content != .erased) Common.invariant("opaque interface relation received different type structure");
                if (!std.mem.eql(u8, public_digest.bytes[0..], private_content.erased.bytes[0..])) {
                    Common.invariant("opaque interface relation received different erased types");
                }
            },
            .zst => if (private_content != .zst)
                Common.invariant("opaque interface relation received different type structure"),
        }
    }

    /// Relate row-polymorphic tag structure without merging a row that carries
    /// generated-private evidence into its checked-public counterpart. Labels
    /// present on only one side are ordinary checker-produced row widening;
    /// their payloads must not themselves introduce private evidence.
    fn relateOpaqueTagRows(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        kind: TypeApplicationKind,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        const flat_public = try self.flattenTagRow(public_node);
        const flat_private = try self.flattenTagRow(private_node);
        var only_public = std.ArrayList(InstTag).empty;
        defer only_public.deinit(self.allocator);
        var only_private = std.ArrayList(InstTag).empty;
        defer only_private.deinit(self.allocator);

        for (flat_public.tags) |public_tag| {
            const wanted = self.tagLabelText(public_tag.name);
            var matched: ?InstTag = null;
            for (flat_private.tags) |private_tag| {
                if (!Ident.textEql(wanted, self.tagLabelText(private_tag.name))) continue;
                if (matched != null) Common.invariant("opaque interface relation received duplicate private tag labels");
                matched = private_tag;
            }
            if (matched) |private_tag| {
                if (public_tag.payloads.len != private_tag.payloads.len) {
                    Common.invariant("opaque interface relation received one tag at two payload arities");
                }
                for (public_tag.payloads, private_tag.payloads) |public_payload, private_payload| {
                    try self.relateOpaqueChild(public_payload, private_payload, pending);
                }
            } else {
                try only_public.append(self.allocator, public_tag);
            }
        }
        for (flat_private.tags) |private_tag| {
            const wanted = self.tagLabelText(private_tag.name);
            var shared = false;
            for (flat_public.tags) |public_tag| {
                if (Ident.textEql(wanted, self.tagLabelText(public_tag.name))) {
                    shared = true;
                    break;
                }
            }
            if (!shared) {
                try only_private.append(self.allocator, private_tag);
            }
        }

        const public_conflicts = self.rowAdditionConflicts(flat_public.ext, only_private.items.len, .tag_union);
        const private_conflicts = self.rowAdditionConflicts(flat_private.ext, only_public.items.len, .tag_union);
        if (kind == .checked_mapping and only_public.items.len == 0 and public_conflicts) {
            // Checking may coerce a closed narrow tag row into a wider
            // contextual row. The exact contextual cell already owns that
            // wider runtime representation, so mapping the shared payloads is
            // complete; do not try to mutate the checked closed row.
            return;
        }
        if (public_conflicts or private_conflicts) {
            Common.invariant("opaque interface relation widened a closed tag union");
        }
        if (only_public.items.len == 0 and only_private.items.len == 0) {
            try self.relateOpaqueChild(flat_public.ext, flat_private.ext, pending);
        } else if (only_public.items.len == 0) {
            try self.writeOrQueueTagRest(flat_public.ext, only_private.items, flat_private.ext, pending);
        } else if (only_private.items.len == 0) {
            try self.writeOrQueueTagRest(flat_private.ext, only_public.items, flat_public.ext, pending);
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
            if (self.find(flat_public.ext) == self.find(flat_private.ext)) {
                var rest = std.ArrayList(InstTag).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_public.items);
                try rest.appendSlice(self.allocator, only_private.items);
                try self.writeOrQueueTagRest(flat_public.ext, rest.items, new_ext, pending);
            } else {
                try self.writeOrQueueTagRest(flat_public.ext, only_private.items, new_ext, pending);
                try self.writeOrQueueTagRest(flat_private.ext, only_public.items, new_ext, pending);
            }
        }
    }

    /// Record-row counterpart of `relateOpaqueTagRows`.
    fn relateOpaqueRecordRows(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        kind: TypeApplicationKind,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        const flat_public = try self.flattenRecordRow(public_node);
        const flat_private = try self.flattenRecordRow(private_node);
        var only_public = std.ArrayList(InstField).empty;
        defer only_public.deinit(self.allocator);
        var only_private = std.ArrayList(InstField).empty;
        defer only_private.deinit(self.allocator);

        for (flat_public.fields) |public_field| {
            const wanted = self.fieldLabelText(public_field.name);
            var matched: ?InstField = null;
            for (flat_private.fields) |private_field| {
                if (!Ident.textEql(wanted, self.fieldLabelText(private_field.name))) continue;
                if (matched != null) Common.invariant("opaque interface relation received duplicate private record labels");
                matched = private_field;
            }
            if (matched) |private_field| {
                try self.relateOpaqueChild(public_field.ty, private_field.ty, pending);
            } else {
                try only_public.append(self.allocator, public_field);
            }
        }
        for (flat_private.fields) |private_field| {
            const wanted = self.fieldLabelText(private_field.name);
            var shared = false;
            for (flat_public.fields) |public_field| {
                if (Ident.textEql(wanted, self.fieldLabelText(public_field.name))) {
                    shared = true;
                    break;
                }
            }
            if (!shared) {
                try only_private.append(self.allocator, private_field);
            }
        }

        const public_conflicts = self.rowAdditionConflicts(flat_public.ext, only_private.items.len, .record);
        const private_conflicts = self.rowAdditionConflicts(flat_private.ext, only_public.items.len, .record);
        if (kind == .checked_mapping and only_public.items.len == 0 and public_conflicts) {
            // Record width coercion has the same directional contract as tag
            // rows: the wider exact contextual cell is the representation;
            // the narrower checked interface contributes only shared fields.
            return;
        }
        if (public_conflicts or private_conflicts) {
            Common.invariant("opaque interface relation widened a closed record");
        }
        if (only_public.items.len == 0 and only_private.items.len == 0) {
            try self.relateOpaqueChild(flat_public.ext, flat_private.ext, pending);
        } else if (only_public.items.len == 0) {
            try self.writeOrQueueRecordRest(flat_public.ext, only_private.items, flat_private.ext, pending);
        } else if (only_private.items.len == 0) {
            try self.writeOrQueueRecordRest(flat_private.ext, only_public.items, flat_public.ext, pending);
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
            if (self.find(flat_public.ext) == self.find(flat_private.ext)) {
                var rest = std.ArrayList(InstField).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_public.items);
                try rest.appendSlice(self.allocator, only_private.items);
                try self.writeOrQueueRecordRest(flat_public.ext, rest.items, new_ext, pending);
            } else {
                try self.writeOrQueueRecordRest(flat_public.ext, only_private.items, new_ext, pending);
                try self.writeOrQueueRecordRest(flat_private.ext, only_public.items, new_ext, pending);
            }
        }
    }

    fn relateOpaqueChild(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        try pending.append(self.allocator, .{ .left = public_node, .right = private_node });
    }

    fn relateGeneratedOpaquePair(
        self: *InstGraph,
        public_content: InstNode,
        private_named: InstNamed,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        if (public_content != .named) Common.invariant("opaque public interface relation received a non-named public node");
        const public_named = public_content.named;
        const iterator_relation = Type.iteratorRelation(public_named, private_named);
        if (iterator_relation == .public_minted or iterator_relation == .forced_dynamic) {
            if (public_named.def.iterator_representation != .none or
                (private_named.def.iterator_representation != .minted and
                    private_named.def.iterator_representation != .forced_dynamic))
            {
                Common.invariant("iterator interface relation did not receive a checked-public/private pair");
            }
            const public_backing = public_named.backing orelse
                Common.invariant("iterator interface relation received a public type without backing");
            const private_backing = private_named.backing orelse
                Common.invariant("iterator interface relation received a private type without backing");
            if (public_backing.authority != .checked_public or private_backing.authority != .generated_private) {
                Common.invariant("iterator interface relation received incorrect backing authority");
            }
            if (public_named.args.len == 0 or private_named.args.len == 0) {
                Common.invariant("iterator interface relation received no public item argument");
            }
            try self.relateOpaqueChild(public_named.args[0], private_named.args[0], pending);
            return;
        }
        if (public_named.kind != .@"opaque" or private_named.kind != .@"opaque" or
            !std.meta.eql(public_named.def, private_named.def))
        {
            Common.invariant("opaque public interface relation received different opaque definitions");
        }
        const public_backing = public_named.backing orelse
            Common.invariant("opaque public interface relation received a public type without backing");
        const private_backing = private_named.backing orelse
            Common.invariant("opaque public interface relation received a private type without backing");
        if (public_backing.authority != .checked_public or private_backing.authority != .generated_private) {
            Common.invariant("opaque public interface relation received incorrect backing authority");
        }
        if (public_named.args.len != private_named.args.len) {
            Common.invariant("opaque public interface relation received different type-argument arities");
        }
        for (public_named.args, private_named.args) |public_arg, private_arg| {
            try self.relateOpaqueChild(public_arg, private_arg, pending);
        }
    }

    fn relatePublicNamedOpaquePair(
        self: *InstGraph,
        public_named: InstNamed,
        private_named: InstNamed,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        // The definition's private view can be nominal where its public view
        // is opaque. They are two checked views of the same declared type, so
        // apply the produced backing directionally while retaining the
        // produced wrapper.
        if (!sameTypeDef(public_named.def, private_named.def)) {
            Common.invariant("opaque interface relation received different named types");
        }
        if (!std.meta.eql(public_named.def, private_named.def)) {
            Common.invariant("opaque interface relation received different instances of one named declaration");
        }
        if (public_named.args.len != private_named.args.len) {
            Common.invariant("opaque interface relation received different named type-argument arities");
        }
        for (public_named.args, private_named.args) |public_arg, private_arg| {
            try self.relateOpaqueChild(public_arg, private_arg, pending);
        }
        if (public_named.backing) |public_backing| {
            const private_backing = private_named.backing orelse
                Common.invariant("opaque interface relation received different named backing presence");
            if (public_backing.authority != private_backing.authority) {
                Common.invariant("opaque interface relation received unmatched backing authority");
            }
            try self.relateOpaqueChild(public_backing.node, private_backing.node, pending);
        } else if (private_named.backing != null) {
            Common.invariant("opaque interface relation received different named backing presence");
        }
    }

    const BackingAccess = enum { inspectable, runtime_layout };

    fn backingAllowsAccess(use: Type.BackingUse, access: BackingAccess) bool {
        return use == .inspectable or access == .runtime_layout;
    }

    fn shapeRoot(
        self: *InstGraph,
        raw_node: NodeId,
        comptime noun: []const u8,
        access: BackingAccess,
    ) Allocator.Error!NodeId {
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();

        var node = self.find(raw_node);
        while (true) {
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) {
                Common.invariant("instantiation " ++ noun ++ " read encountered a recursive named backing");
            }
            const node_content = self.nodes.items[@intFromEnum(node)];
            if (node_content == .named) {
                const named = node_content.named;
                const backing = named.backing orelse
                    Common.invariant("instantiation " ++ noun ++ " read reached a named type without backing");
                if (!backingAllowsAccess(backing.use, access)) {
                    Common.invariant("instantiation " ++ noun ++ " read inspected a runtime-layout-only backing");
                }
                node = self.find((try self.structuralBackingNode(backing.node, named)).node);
            } else {
                return node;
            }
        }
    }

    /// Structural root of a function-shaped request node. Callable request
    /// identity is structural: a transparent named wrapper names the same
    /// function interface as its backing, so requests resolve to the backing
    /// before they become specialization keys or sealed function types.
    pub fn functionRequestRoot(self: *InstGraph, node: NodeId) Allocator.Error!NodeId {
        return self.shapeRoot(node, "function request", .inspectable);
    }

    /// Project a function-shaped live node without materializing a Monotype.
    pub fn functionNodes(self: *InstGraph, node: NodeId) Allocator.Error!FunctionNodes {
        const node_content = self.content(try self.shapeRoot(node, "function", .inspectable));
        if (node_content != .func) Common.invariant("instantiation function read had a non-function node");
        return .{ .args = node_content.func.args, .ret = self.find(node_content.func.ret) };
    }

    /// Construct one complete specialization interface from a checked callable
    /// and the exact values at this call site. Each checked polymorphic cell is
    /// substituted everywhere it occurs, while independent concrete argument
    /// positions retain the distinct exact values they produced. This completes
    /// the callable before body lowering begins through one directed
    /// request-to-produced traversal, not a containment scan followed by a
    /// whole-root merge.
    pub fn functionRequestFromProducedArguments(
        self: *InstGraph,
        checked_fn_node: NodeId,
        current_request_fn_node: NodeId,
        produced_args: []const NodeId,
    ) Allocator.Error!NodeId {
        return (try self.functionRequestFromProducedArgumentsAndComponents(
            checked_fn_node,
            current_request_fn_node,
            produced_args,
            &.{},
        )).request;
    }

    pub const MaterializedFunctionRequest = struct {
        request: NodeId,
        components: []const NodeId,
    };

    /// Build a function request and, through the exact same substitution,
    /// materialize checker-authored component roots such as a type-only method
    /// dispatcher. Returning those roots preserves the request's produced
    /// structure without re-reading the independent checked instantiation.
    pub fn functionRequestFromProducedArgumentsAndComponents(
        self: *InstGraph,
        checked_fn_node: NodeId,
        current_request_fn_node: NodeId,
        produced_args: []const NodeId,
        checked_components: []const NodeId,
    ) Allocator.Error!MaterializedFunctionRequest {
        self.countDiagnostic("function_request_builds");
        const checked_fn = try self.functionNodes(checked_fn_node);
        const current_request_fn = switch (self.content(try self.functionRequestRoot(current_request_fn_node))) {
            .func => |function| function,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => Common.invariant("function request had a non-function current interface"),
        };
        if (checked_fn.args.len != current_request_fn.args.len or
            checked_fn.args.len != produced_args.len)
        {
            Common.invariant("exact function request had different arity from its checked source");
        }

        var substitution = FunctionRequestSubstitution.init(self.allocator);
        defer substitution.deinit();

        const checked_source_root = try self.functionRequestRoot(checked_fn_node);
        const current_source = self.requestCheckedSource(current_request_fn_node);
        const source_changed = if (current_source) |source|
            !self.sameClass(try self.functionRequestRoot(source), checked_source_root)
        else
            false;
        const stored = self.request_substitution_spans.items[@intFromEnum(current_request_fn_node)];
        if (stored.isInitialized() and !source_changed) {
            for (self.requestSubstitutions(current_request_fn_node)) |selection| {
                try self.seedFunctionRequestReplacement(
                    self.find(selection.checked),
                    self.find(selection.produced),
                    &substitution,
                );
            }
        } else {
            for (checked_fn.args, current_request_fn.args) |checked_arg, current_arg| {
                try self.collectFunctionRequestSubstitutions(
                    checked_arg,
                    current_arg,
                    &substitution,
                );
            }
            try self.collectFunctionRequestSubstitutions(
                checked_fn.ret,
                current_request_fn.ret,
                &substitution,
            );
            substitution.compared.clearRetainingCapacity();
        }
        for (checked_fn.args, produced_args) |checked_arg, produced_arg| {
            try self.collectFunctionRequestSubstitutions(
                checked_arg,
                produced_arg,
                &substitution,
            );
        }

        const changed_args = try self.materializeFunctionRequestArgumentNodes(
            checked_fn.args,
            current_request_fn.args,
            produced_args,
            &substitution,
        );
        // The return cell is the caller's exact destination, not a fresh
        // checked-public occurrence. Preserve a definition-private structural
        // destination just as nested callable outputs preserve their produced
        // representation.
        const request_ret = try self.materializeFunctionRequestNodeMode(
            checked_fn.ret,
            current_request_fn.ret,
            &substitution,
            .produced_callable,
        );
        const ret_changed = !self.sameClass(current_request_fn.ret, request_ret);

        const request_fn = if (changed_args != null or ret_changed or source_changed)
            try self.newNode(.{ .func = .{
                .args = changed_args orelse current_request_fn.args,
                .ret = request_ret,
            } })
        else
            current_request_fn_node;
        try self.registerRequestCheckedSource(request_fn, checked_source_root);
        if (request_fn != current_request_fn_node or
            !stored.isInitialized() or
            substitution.changed_after_seed)
        {
            try self.recordRequestSubstitutions(request_fn, &substitution);
        }
        const components = try self.arena().alloc(NodeId, checked_components.len);
        for (checked_components, components) |checked_component, *component| {
            component.* = try self.materializeFunctionRequestNode(
                checked_component,
                checked_component,
                &substitution,
            );
        }
        return .{ .request = request_fn, .components = components };
    }

    fn recordRequestSubstitutions(
        self: *InstGraph,
        request_fn: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        const start = self.request_substitutions.items.len;
        var replacements = substitution.replacements.iterator();
        while (replacements.next()) |entry| {
            try self.request_substitutions.append(self.allocator, .{
                .checked = self.find(entry.key_ptr.*),
                .produced = self.find(entry.value_ptr.*),
            });
        }
        self.request_substitution_spans.items[@intFromEnum(request_fn)] = .{
            .start = @intCast(start),
            .len = @intCast(self.request_substitutions.items.len - start),
        };
    }

    fn collectFunctionRequestSubstitutions(
        self: *InstGraph,
        raw_checked: NodeId,
        raw_produced: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        const checked_node = self.find(raw_checked);
        const produced_node = self.find(raw_produced);
        if (checked_node == produced_node) return;

        const pair = NodePair{ .left = checked_node, .right = produced_node };
        const compared = try substitution.compared.getOrPut(pair);
        if (compared.found_existing) return;
        self.countDiagnostic("function_request_pairs_visited");

        const checked_content = self.nodes.items[@intFromEnum(checked_node)];
        const produced_content = self.nodes.items[@intFromEnum(produced_node)];

        if (checked_content == .unresolved) {
            if (checked_node == produced_node) return;
            try self.recordFunctionRequestReplacement(checked_node, produced_node, substitution);
            return;
        }

        if (isGeneratedPrivateRootContent(produced_content) and checked_content == .named and
            sameTypeDef(checked_content.named.def, produced_content.named.def))
        {
            // Checked-node identity is the checker's explicit relation between
            // occurrences. Replace this complete occurrence everywhere that
            // same node recurs; independent concrete positions have distinct
            // checked nodes even when their public shapes are identical.
            try self.recordFunctionRequestReplacement(checked_node, produced_node, substitution);
            return;
        }

        if (checked_content == .named and produced_content != .named) {
            const backing = checked_content.named.backing orelse
                Common.invariant("function substitution found a checked named view without backing");
            return self.collectFunctionRequestSubstitutions(backing.node, produced_node, substitution);
        }
        if (produced_content == .named and checked_content != .named) {
            const backing = produced_content.named.backing orelse
                Common.invariant("function substitution found an exact named view without backing");
            return self.collectFunctionRequestSubstitutions(checked_node, backing.node, substitution);
        }

        switch (checked_content) {
            .redirect => unreachable,
            .unresolved => unreachable,
            .primitive => |primitive| {
                if (produced_content != .primitive or produced_content.primitive != primitive) {
                    Common.invariant("function substitution received different primitive types");
                }
            },
            .list => |checked_elem| {
                if (produced_content != .list) Common.invariant("function substitution received different type structure");
                try self.collectFunctionRequestSubstitutions(checked_elem, produced_content.list, substitution);
            },
            .box => |checked_elem| {
                if (produced_content != .box) Common.invariant("function substitution received different type structure");
                try self.collectFunctionRequestSubstitutions(checked_elem, produced_content.box, substitution);
            },
            .tuple => |checked_items| {
                if (produced_content != .tuple or checked_items.len != produced_content.tuple.len) {
                    Common.invariant("function substitution received tuples of different arity");
                }
                for (checked_items, produced_content.tuple) |checked_item, produced_item| {
                    try self.collectFunctionRequestSubstitutions(checked_item, produced_item, substitution);
                }
            },
            .func => |checked_function| {
                if (produced_content != .func or checked_function.args.len != produced_content.func.args.len) {
                    Common.invariant("function substitution received functions of different arity");
                }
                for (checked_function.args, produced_content.func.args) |checked_arg, produced_arg| {
                    try self.collectFunctionRequestSubstitutions(checked_arg, produced_arg, substitution);
                }
                try self.collectFunctionRequestSubstitutions(checked_function.ret, produced_content.func.ret, substitution);
            },
            .tag_union => try self.collectFunctionRequestTagSubstitutions(
                checked_node,
                produced_node,
                substitution,
            ),
            .record => try self.collectFunctionRequestRecordSubstitutions(
                checked_node,
                produced_node,
                substitution,
            ),
            .empty_tag_union => if (produced_content != .empty_tag_union)
                Common.invariant("function substitution received different type structure"),
            .empty_record => if (produced_content != .empty_record)
                Common.invariant("function substitution received different type structure"),
            .named => |checked_named| {
                if (produced_content != .named) {
                    Common.invariant("function substitution received a named and non-named pair");
                }
                if (!sameTypeDef(checked_named.def, produced_content.named.def) or
                    checked_named.args.len != produced_content.named.args.len)
                {
                    Common.invariant("function substitution received different named types");
                }
                for (checked_named.args, produced_content.named.args) |checked_arg, produced_arg| {
                    try self.collectFunctionRequestSubstitutions(checked_arg, produced_arg, substitution);
                }
            },
            .erased => |digest| {
                if (produced_content != .erased or
                    !std.mem.eql(u8, &digest.bytes, &produced_content.erased.bytes))
                {
                    Common.invariant("function substitution received different erased types");
                }
            },
            .zst => if (produced_content != .zst)
                Common.invariant("function substitution received different type structure"),
        }
    }

    fn collectFunctionRequestTagSubstitutions(
        self: *InstGraph,
        checked_node: NodeId,
        produced_node: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        if (self.content(produced_node) != .tag_union) {
            Common.invariant("function substitution received different type structure");
        }
        const checked_row = try self.flattenTagRow(checked_node);
        const produced_row = try self.flattenTagRow(produced_node);
        for (checked_row.tags) |checked_tag| {
            for (produced_row.tags) |produced_tag| {
                if (!self.name_store.tagLabelTextEql(checked_tag.name, produced_tag.name)) continue;
                if (checked_tag.payloads.len != produced_tag.payloads.len) {
                    Common.invariant("function substitution received one tag at two payload arities");
                }
                for (checked_tag.payloads, produced_tag.payloads) |checked_payload, produced_payload| {
                    try self.collectFunctionRequestSubstitutions(checked_payload, produced_payload, substitution);
                }
                break;
            }
        }
        if (!self.sameClass(checked_row.ext, produced_row.ext)) {
            try self.collectFunctionRequestSubstitutions(checked_row.ext, produced_row.ext, substitution);
        }
    }

    fn collectFunctionRequestRecordSubstitutions(
        self: *InstGraph,
        checked_node: NodeId,
        produced_node: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        if (self.content(produced_node) != .record) {
            Common.invariant("function substitution received different type structure");
        }
        const checked_row = try self.flattenRecordRow(checked_node);
        const produced_row = try self.flattenRecordRow(produced_node);
        for (checked_row.fields) |checked_field| {
            for (produced_row.fields) |produced_field| {
                if (!self.name_store.recordFieldLabelTextEql(checked_field.name, produced_field.name)) continue;
                try self.collectFunctionRequestSubstitutions(checked_field.ty, produced_field.ty, substitution);
                break;
            }
        }
        if (!self.sameClass(checked_row.ext, produced_row.ext)) {
            try self.collectFunctionRequestSubstitutions(checked_row.ext, produced_row.ext, substitution);
        }
    }

    fn recordFunctionRequestReplacement(
        self: *InstGraph,
        checked_node: NodeId,
        produced_node: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        return self.putFunctionRequestReplacement(checked_node, produced_node, substitution, true);
    }

    fn seedFunctionRequestReplacement(
        self: *InstGraph,
        checked_node: NodeId,
        produced_node: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!void {
        return self.putFunctionRequestReplacement(checked_node, produced_node, substitution, false);
    }

    fn putFunctionRequestReplacement(
        self: *InstGraph,
        checked_node: NodeId,
        produced_node: NodeId,
        substitution: *FunctionRequestSubstitution,
        count_discovery: bool,
    ) Allocator.Error!void {
        const entry = try substitution.replacements.getOrPut(checked_node);
        if (!entry.found_existing) {
            entry.value_ptr.* = produced_node;
            if (count_discovery) {
                substitution.changed_after_seed = true;
                self.countDiagnostic("function_request_replacements");
            }
            return;
        }
        if (self.sameClass(entry.value_ptr.*, produced_node)) return;
        const previous = entry.value_ptr.*;
        entry.value_ptr.* = try self.joinProducedTypeRepresentations(previous, produced_node);
        if (count_discovery and !self.sameClass(previous, entry.value_ptr.*)) {
            substitution.changed_after_seed = true;
        }
    }

    fn materializeFunctionRequestNode(
        self: *InstGraph,
        raw_checked: NodeId,
        raw_node: NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!NodeId {
        return self.materializeFunctionRequestNodeMode(
            raw_checked,
            raw_node,
            substitution,
            .request,
        );
    }

    /// Give one function body stable constructor roots for its ABI while
    /// sharing all value-only structure. Function nodes are copied, and
    /// compound paths are copied only when they lead to a copied function.
    /// Later checked-source relations can then refine their own request roots
    /// without changing argument cells the body has already emitted.
    pub fn isolateFunctionAbi(self: *InstGraph, fn_node: NodeId) Allocator.Error!NodeId {
        var substitution = FunctionRequestSubstitution.init(self.allocator);
        defer substitution.deinit();
        const isolated = try self.materializeFunctionRequestNodeMode(
            fn_node,
            fn_node,
            &substitution,
            .body_abi,
        );
        self.inheritRequestSubstitutions(fn_node, isolated);
        return isolated;
    }

    /// Share a completed request's immutable substitution span with a generated
    /// callable wrapper or isolated body ABI that gets its own function node.
    pub fn inheritRequestSubstitutions(
        self: *InstGraph,
        source_fn: NodeId,
        destination_fn: NodeId,
    ) void {
        if (source_fn == destination_fn) return;
        const source = self.request_substitution_spans.items[@intFromEnum(source_fn)];
        self.request_substitution_spans.items[@intFromEnum(destination_fn)] = source;
    }

    /// Build one detached request whose explicitly selected descendant cells
    /// use the representations already stored in mutable locals. The old local
    /// cells remain immutable: only compound paths from `request_root` to a
    /// selected cell are copied. Multiple selections for the same request cell
    /// meet at that assignment boundary and choose one common representation.
    pub fn materializeReassignedStorageRequest(
        self: *InstGraph,
        request_root: NodeId,
        request_nodes: []const NodeId,
        stored_nodes: []const NodeId,
    ) Allocator.Error!NodeId {
        if (request_nodes.len != stored_nodes.len) {
            Common.invariant("reassigned storage request received different selection lengths");
        }
        var substitution = FunctionRequestSubstitution.init(self.allocator);
        defer substitution.deinit();
        for (request_nodes, stored_nodes) |request, stored| {
            try self.recordFunctionRequestReplacement(
                self.find(request),
                self.find(stored),
                &substitution,
            );
        }
        return try self.materializeFunctionRequestNodeMode(
            request_root,
            request_root,
            &substitution,
            .reassigned_storage,
        );
    }

    fn materializeFunctionRequestNodeMode(
        self: *InstGraph,
        raw_checked: NodeId,
        raw_node: NodeId,
        substitution: *FunctionRequestSubstitution,
        mode: FunctionRequestMaterializationMode,
    ) Allocator.Error!NodeId {
        return (try self.materializeFunctionRequestNodeResult(
            raw_checked,
            raw_node,
            substitution,
            mode,
        )).node;
    }

    fn materializeFunctionRequestNodeResult(
        self: *InstGraph,
        raw_checked: NodeId,
        raw_node: NodeId,
        substitution: *FunctionRequestSubstitution,
        mode: FunctionRequestMaterializationMode,
    ) Allocator.Error!MaterializedNode {
        const checked_node = self.find(raw_checked);
        const node = self.find(raw_node);
        const node_content = self.nodes.items[@intFromEnum(node)];
        if (substitution.replacements.get(checked_node)) |raw_replacement| {
            const replacement = self.find(raw_replacement);
            return .{ .node = replacement, .changed = !self.sameClass(node, replacement) };
        }
        if (isGeneratedPrivateRootContent(node_content)) return .{ .node = node, .changed = false };
        const pair = NodePair{ .left = checked_node, .right = node };
        const materialization_key = FunctionRequestMaterialization{ .pair = pair, .mode = mode };
        if (substitution.materialized.get(materialization_key)) |materialized| return .{
            .node = self.find(materialized.node),
            .changed = materialized.changed,
        };
        const active_materialized = &substitution.active_materialized[@intFromEnum(mode)];
        if (active_materialized.get(node)) |materialized| return .{
            .node = self.find(materialized),
            .changed = false,
        };

        const checked_content = self.nodes.items[@intFromEnum(checked_node)];
        if (checked_content == .named and node_content != .named) {
            const backing = checked_content.named.backing orelse
                Common.invariant("function request materialization found a checked named view without backing");
            if (checked_content.named.kind == .alias or mode == .produced_callable) {
                return self.materializeFunctionRequestNodeResult(backing.node, node, substitution, mode);
            }

            // A definition-private call-site view may expose an ordinary
            // nominal's structural backing. Preserve the checked constructor
            // root and substitute beneath it; only an exact generated-private
            // nominal (handled above) may replace a nominal request root.
            const reserved = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
            try substitution.materialized.put(materialization_key, .{ .node = reserved, .changed = false });
            const args_result = try self.materializeFunctionRequestNodesResult(
                checked_content.named.args,
                checked_content.named.args,
                substitution,
                mode,
            );
            const materialized_backing = try self.materializeFunctionRequestNodeResult(
                backing.node,
                node,
                substitution,
                mode,
            );
            var declared_order = checked_content.named.declared_order;
            var changed_declared: ?[]InstDeclaredField = null;
            for (checked_content.named.declared_order, 0..) |declared, index| {
                const next_result = switch (declared) {
                    .named => MaterializedNode{ .node = undefined, .changed = false },
                    .padding => |padding| try self.materializeFunctionRequestNodeResult(
                        padding,
                        padding,
                        substitution,
                        mode,
                    ),
                };
                const next = switch (declared) {
                    .named => |field| InstDeclaredField{ .named = field },
                    .padding => InstDeclaredField{ .padding = next_result.node },
                };
                if (changed_declared) |fields| {
                    fields[index] = next;
                } else if (!std.meta.eql(declared, next)) {
                    const fields = try self.arena().alloc(InstDeclaredField, declared_order.len);
                    @memcpy(fields[0..index], declared_order[0..index]);
                    fields[index] = next;
                    changed_declared = fields;
                }
            }
            if (changed_declared) |fields| declared_order = fields;
            try self.setContent(reserved, .{ .named = .{
                .named_type = checked_content.named.named_type,
                .def = checked_content.named.def,
                .kind = checked_content.named.kind,
                .builtin_owner = checked_content.named.builtin_owner,
                .args = args_result.nodes orelse checked_content.named.args,
                .backing = .{
                    .node = materialized_backing.node,
                    .use = backing.use,
                    .authority = backing.authority,
                },
                .generated_iterator = checked_content.named.generated_iterator,
                .declared_order = declared_order,
            } });
            const materialized = MaterializedNode{ .node = reserved, .changed = true };
            try substitution.materialized.put(materialization_key, materialized);
            self.countDiagnostic("function_request_nodes_materialized");
            return materialized;
        }
        if (checked_content == .unresolved) return .{ .node = node, .changed = false };

        const materialized: MaterializedNode = switch (node_content) {
            .redirect => unreachable,
            .unresolved, .primitive, .empty_tag_union, .empty_record, .erased, .zst => .{
                .node = node,
                .changed = false,
            },
            .list => |elem| blk: {
                if (checked_content != .list) Common.invariant("function request materialization expected a checked list");
                const next = try self.materializeFunctionRequestNodeResult(checked_content.list, elem, substitution, mode);
                if (self.sameClass(elem, next.node)) break :blk .{ .node = node, .changed = next.changed };
                break :blk .{ .node = try self.newNode(.{ .list = next.node }), .changed = next.changed };
            },
            .box => |elem| blk: {
                if (checked_content != .box) Common.invariant("function request materialization expected a checked box");
                const next = try self.materializeFunctionRequestNodeResult(checked_content.box, elem, substitution, mode);
                if (self.sameClass(elem, next.node)) break :blk .{ .node = node, .changed = next.changed };
                break :blk .{ .node = try self.newNode(.{ .box = next.node }), .changed = next.changed };
            },
            .tuple => |items| blk: {
                if (checked_content != .tuple or checked_content.tuple.len != items.len) {
                    Common.invariant("function request materialization received tuples of different arity");
                }
                const next = try self.materializeFunctionRequestNodesResult(checked_content.tuple, items, substitution, mode);
                const next_items = next.nodes orelse break :blk .{ .node = node, .changed = next.changed };
                break :blk .{ .node = try self.newNode(.{ .tuple = next_items }), .changed = next.changed };
            },
            .func => |function| blk: {
                if (checked_content != .func or checked_content.func.args.len != function.args.len) {
                    Common.invariant("function request materialization received functions of different arity");
                }
                const callable_mode: FunctionRequestMaterializationMode = switch (mode) {
                    .request => .request,
                    .produced_value, .produced_callable => .produced_callable,
                    .body_abi => .body_abi,
                    .reassigned_storage => .reassigned_storage,
                };
                const changed_args = try self.materializeFunctionRequestNodesResult(
                    checked_content.func.args,
                    function.args,
                    substitution,
                    callable_mode,
                );
                const ret = try self.materializeFunctionRequestNodeResult(
                    checked_content.func.ret,
                    function.ret,
                    substitution,
                    callable_mode,
                );
                const changed = mode == .body_abi or changed_args.changed or ret.changed;
                if (!changed and changed_args.nodes == null and self.sameClass(function.ret, ret.node)) {
                    break :blk .{ .node = node, .changed = false };
                }
                break :blk .{ .node = try self.newNode(.{ .func = .{
                    .args = changed_args.nodes orelse function.args,
                    .ret = ret.node,
                } }), .changed = changed };
            },
            .tag_union => |row| blk: {
                if (checked_content != .tag_union) Common.invariant("function request materialization expected a checked tag union");
                const checked_row: FlatTagRow = if (checked_node == node)
                    .{ .tags = row.tags, .ext = row.ext }
                else
                    try self.flattenTagRow(checked_node);
                var changed_tags: ?[]InstTag = null;
                var changed = false;
                for (row.tags, 0..) |tag, index| {
                    var checked_tag: ?InstTag = null;
                    for (checked_row.tags) |candidate| {
                        if (!Ident.textEql(self.tagLabelText(candidate.name), self.tagLabelText(tag.name))) continue;
                        checked_tag = candidate;
                        break;
                    }
                    const payloads_result = if (checked_tag) |source| payloads: {
                        if (source.payloads.len != tag.payloads.len) {
                            Common.invariant("function request materialization received one tag at two payload arities");
                        }
                        break :payloads try self.materializeFunctionRequestNodesResult(source.payloads, tag.payloads, substitution, mode);
                    } else MaterializedNodes{ .nodes = null, .changed = false };
                    changed = changed or payloads_result.changed;
                    if (changed_tags) |tags| {
                        tags[index] = .{
                            .name = tag.name,
                            .checked_name = tag.checked_name,
                            .payloads = payloads_result.nodes orelse tag.payloads,
                        };
                    } else if (payloads_result.nodes) |payloads| {
                        const tags = try self.arena().alloc(InstTag, row.tags.len);
                        @memcpy(tags[0..index], row.tags[0..index]);
                        tags[index] = .{
                            .name = tag.name,
                            .checked_name = tag.checked_name,
                            .payloads = payloads,
                        };
                        changed_tags = tags;
                    }
                }
                const ext = try self.materializeFunctionRequestNodeResult(checked_row.ext, row.ext, substitution, mode);
                changed = changed or ext.changed;
                if (changed_tags == null and self.sameClass(row.ext, ext.node)) break :blk .{ .node = node, .changed = changed };
                break :blk .{ .node = try self.newNode(.{ .tag_union = .{
                    .tags = changed_tags orelse row.tags,
                    .ext = ext.node,
                } }), .changed = changed };
            },
            .record => |row| blk: {
                if (checked_content != .record) Common.invariant("function request materialization expected a checked record");
                const checked_row: FlatRecordRow = if (checked_node == node)
                    .{ .fields = row.fields, .ext = row.ext }
                else
                    try self.flattenRecordRow(checked_node);
                var changed_fields: ?[]InstField = null;
                var changed = false;
                for (row.fields, 0..) |field, index| {
                    var checked_field: ?InstField = null;
                    for (checked_row.fields) |candidate| {
                        if (!Ident.textEql(self.fieldLabelText(candidate.name), self.fieldLabelText(field.name))) continue;
                        checked_field = candidate;
                        break;
                    }
                    const ty_result = if (checked_field) |source|
                        try self.materializeFunctionRequestNodeResult(source.ty, field.ty, substitution, mode)
                    else
                        MaterializedNode{ .node = field.ty, .changed = false };
                    changed = changed or ty_result.changed;
                    if (changed_fields) |fields| {
                        fields[index] = .{ .name = field.name, .ty = ty_result.node };
                    } else if (!self.sameClass(field.ty, ty_result.node)) {
                        const fields = try self.arena().alloc(InstField, row.fields.len);
                        @memcpy(fields[0..index], row.fields[0..index]);
                        fields[index] = .{ .name = field.name, .ty = ty_result.node };
                        changed_fields = fields;
                    }
                }
                const ext = try self.materializeFunctionRequestNodeResult(checked_row.ext, row.ext, substitution, mode);
                changed = changed or ext.changed;
                if (changed_fields == null and self.sameClass(row.ext, ext.node)) break :blk .{ .node = node, .changed = changed };
                break :blk .{ .node = try self.newNode(.{ .record = .{
                    .fields = changed_fields orelse row.fields,
                    .ext = ext.node,
                } }), .changed = changed };
            },
            .named => |named| blk: {
                if (checked_content != .named or
                    !sameTypeDef(checked_content.named.def, named.def) or
                    checked_content.named.args.len != named.args.len)
                {
                    break :blk .{ .node = node, .changed = false };
                }
                const changed_args = try self.materializeFunctionRequestNodesResult(
                    checked_content.named.args,
                    named.args,
                    substitution,
                    mode,
                );
                if (!changed_args.changed and changed_args.nodes == null and
                    mode != .body_abi and mode != .reassigned_storage)
                {
                    break :blk .{ .node = node, .changed = false };
                }

                const reserved = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
                try substitution.materialized.put(materialization_key, .{ .node = reserved, .changed = false });
                try active_materialized.putNoClobber(node, reserved);
                defer _ = active_materialized.remove(node);
                var backing_changed = false;
                const backing = if (named.backing) |backing| backing: {
                    const backing_result = if (checked_content.named.backing) |checked_backing|
                        try self.materializeFunctionRequestNodeResult(checked_backing.node, backing.node, substitution, mode)
                    else
                        MaterializedNode{ .node = backing.node, .changed = false };
                    backing_changed = backing_result.changed;
                    break :backing InstBacking{
                        .node = backing_result.node,
                        .use = backing.use,
                        .authority = backing.authority,
                    };
                } else null;
                var changed_declared: ?[]InstDeclaredField = null;
                var declared_changed = false;
                for (named.declared_order, 0..) |declared, index| {
                    const next_result = switch (declared) {
                        .named => MaterializedNode{ .node = undefined, .changed = false },
                        .padding => |padding| padding: {
                            if (index >= checked_content.named.declared_order.len or
                                checked_content.named.declared_order[index] != .padding)
                            {
                                break :padding MaterializedNode{ .node = padding, .changed = false };
                            }
                            break :padding try self.materializeFunctionRequestNodeResult(
                                checked_content.named.declared_order[index].padding,
                                padding,
                                substitution,
                                mode,
                            );
                        },
                    };
                    declared_changed = declared_changed or next_result.changed;
                    const next = switch (declared) {
                        .named => |field| InstDeclaredField{ .named = field },
                        .padding => InstDeclaredField{ .padding = next_result.node },
                    };
                    if (changed_declared) |fields| {
                        fields[index] = next;
                    } else if (!std.meta.eql(declared, next)) {
                        const fields = try self.arena().alloc(InstDeclaredField, named.declared_order.len);
                        @memcpy(fields[0..index], named.declared_order[0..index]);
                        fields[index] = next;
                        changed_declared = fields;
                    }
                }
                const changed = changed_args.changed or backing_changed or declared_changed;
                if (!changed) {
                    const unchanged = MaterializedNode{ .node = node, .changed = false };
                    try substitution.materialized.put(materialization_key, unchanged);
                    break :blk unchanged;
                }
                try self.setContent(reserved, .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = changed_args.nodes orelse named.args,
                    .backing = backing,
                    .generated_iterator = named.generated_iterator,
                    .declared_order = changed_declared orelse named.declared_order,
                } });
                break :blk .{ .node = reserved, .changed = true };
            },
        };
        try substitution.materialized.put(materialization_key, materialized);
        if (materialized.changed) {
            self.countDiagnostic("function_request_nodes_materialized");
        }
        return .{ .node = self.find(materialized.node), .changed = materialized.changed };
    }

    fn materializeFunctionRequestNodesResult(
        self: *InstGraph,
        checked_nodes: []const NodeId,
        nodes: []const NodeId,
        substitution: *FunctionRequestSubstitution,
        mode: FunctionRequestMaterializationMode,
    ) Allocator.Error!MaterializedNodes {
        if (checked_nodes.len != nodes.len) {
            Common.invariant("function request materialization received different node span lengths");
        }
        var changed_nodes: ?[]NodeId = null;
        var changed = false;
        for (checked_nodes, nodes, 0..) |checked_node, node, index| {
            const materialized = try self.materializeFunctionRequestNodeResult(checked_node, node, substitution, mode);
            changed = changed or materialized.changed;
            if (changed_nodes) |out| {
                out[index] = materialized.node;
            } else if (!self.sameClass(node, materialized.node)) {
                const out = try self.arena().alloc(NodeId, nodes.len);
                @memcpy(out[0..index], nodes[0..index]);
                out[index] = materialized.node;
                changed_nodes = out;
            }
        }
        return .{ .nodes = changed_nodes, .changed = changed };
    }

    fn materializeFunctionRequestArgumentNodes(
        self: *InstGraph,
        checked_nodes: []const NodeId,
        current_nodes: []const NodeId,
        produced_nodes: []const NodeId,
        substitution: *FunctionRequestSubstitution,
    ) Allocator.Error!?[]NodeId {
        if (checked_nodes.len != current_nodes.len or current_nodes.len != produced_nodes.len) {
            Common.invariant("function request argument materialization received different arities");
        }
        var changed_nodes: ?[]NodeId = null;
        for (checked_nodes, current_nodes, produced_nodes, 0..) |checked_node, current, produced, index| {
            const materialized = try self.materializeFunctionRequestNodeMode(
                checked_node,
                produced,
                substitution,
                .produced_value,
            );
            if (changed_nodes) |out| {
                out[index] = materialized;
            } else if (!self.sameClass(current, materialized)) {
                const out = try self.arena().alloc(NodeId, current_nodes.len);
                @memcpy(out[0..index], current_nodes[0..index]);
                out[index] = materialized;
                changed_nodes = out;
            }
        }
        return changed_nodes;
    }

    pub const FunctionInterfaceIterator = struct {
        function: FunctionNodes,
        index: usize = 0,

        pub fn next(self: *FunctionInterfaceIterator) ?NodeId {
            if (self.index < self.function.args.len) {
                defer self.index += 1;
                return self.function.args[self.index];
            }
            if (self.index == self.function.args.len) {
                self.index += 1;
                return self.function.ret;
            }
            return null;
        }
    };

    /// Every permanent cell in a function request's explicit interface.
    /// Open draft lookup indexes and probes all of these cells so recursive
    /// requests remain discoverable when any subset of the interface is joined.
    pub fn functionInterfaceIterator(self: *InstGraph, node: NodeId) Allocator.Error!FunctionInterfaceIterator {
        return .{ .function = try self.functionNodes(node) };
    }

    /// Project tuple item cells without materializing a Monotype.
    pub fn tupleItemNodes(self: *InstGraph, node: NodeId) Allocator.Error![]const NodeId {
        const node_content = self.content(try self.shapeRoot(node, "tuple", .inspectable));
        if (node_content != .tuple) Common.invariant("instantiation tuple read had a non-tuple node");
        return node_content.tuple;
    }

    /// Project a list element cell without materializing a Monotype.
    pub fn listElementNode(self: *InstGraph, node: NodeId) Allocator.Error!NodeId {
        const node_content = self.content(try self.shapeRoot(node, "list", .inspectable));
        if (node_content != .list) Common.invariant("instantiation list read had a non-list node");
        return self.find(node_content.list);
    }

    /// Project a box element cell without materializing a Monotype.
    pub fn boxElementNode(self: *InstGraph, node: NodeId) Allocator.Error!NodeId {
        const node_content = self.content(try self.shapeRoot(node, "box", .inspectable));
        if (node_content != .box) Common.invariant("instantiation box read had a non-box node");
        return self.find(node_content.box);
    }

    /// Project one exact tag payload cell from a live tag-union row.
    pub fn tagPayloadNode(
        self: *InstGraph,
        node: NodeId,
        name: names.TagNameId,
        payload_index: usize,
    ) Allocator.Error!NodeId {
        return self.tagPayloadNodeWithAccess(node, name, payload_index, .inspectable);
    }

    /// Project one exact tag payload cell for runtime construction.
    pub fn tagConstructionPayloadNode(
        self: *InstGraph,
        node: NodeId,
        name: names.TagNameId,
        payload_index: usize,
    ) Allocator.Error!NodeId {
        return self.tagPayloadNodeWithAccess(node, name, payload_index, .runtime_layout);
    }

    /// Project the flattened runtime row used to construct a tag value. The
    /// returned nodes remain graph-owned exact cells.
    pub fn tagConstructionRow(self: *InstGraph, raw_row: NodeId) Allocator.Error!TagConstructionRow {
        const structural = try self.shapeRoot(raw_row, "tag constructor", .runtime_layout);
        return switch (self.content(structural)) {
            .tag_union => blk: {
                const row = try self.flattenTagRow(structural);
                break :blk .{ .root = structural, .tags = row.tags, .ext = row.ext };
            },
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => Common.invariant("instantiation tag constructor had a non-tag-union runtime backing"),
        };
    }

    /// Read every explicit tag and payload cell when the node is tag-row
    /// shaped, without materializing a Monotype or exposing its row extension.
    pub fn tagRowNodesOrNull(self: *InstGraph, raw_row: NodeId) Allocator.Error!?TagRowNodes {
        const structural = try self.shapeRoot(raw_row, "tag row", .inspectable);
        const structural_content = self.content(structural);
        if (structural_content == .tag_union) return .{ .tags = (try self.flattenTagRow(structural)).tags };
        if (structural_content == .empty_tag_union) return .{ .tags = &.{} };
        return null;
    }

    /// Read every explicit tag and payload cell from a tag-union-shaped node
    /// without materializing a Monotype or exposing its row extension.
    pub fn tagRowNodes(self: *InstGraph, raw_row: NodeId) Allocator.Error!TagRowNodes {
        return try self.tagRowNodesOrNull(raw_row) orelse
            Common.invariant("instantiation tag-row read had a non-tag-union node");
    }

    /// Return whether a tag row's explicit extension is proven closed. This
    /// preserves the extension as graph-owned evidence while allowing callers
    /// to distinguish a closed marker union from an open row with the same
    /// currently known tags.
    pub fn tagRowIsClosed(self: *InstGraph, raw_row: NodeId) Allocator.Error!bool {
        const structural = try self.shapeRoot(raw_row, "tag row closure", .inspectable);
        const structural_content = self.content(structural);
        if (structural_content == .empty_tag_union) return true;
        if (structural_content != .tag_union) Common.invariant("instantiation tag-row closure read had a non-tag-union node");
        const ext = self.content((try self.flattenTagRow(structural)).ext);
        if (ext == .empty_tag_union) return true;
        if (ext == .unresolved) return false;
        Common.invariant("flattened tag row had an invalid extension");
    }

    fn tagPayloadNodeWithAccess(
        self: *InstGraph,
        node: NodeId,
        name: names.TagNameId,
        payload_index: usize,
        access: BackingAccess,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(node, "tag payload", access);
        if (self.content(structural) != .tag_union) Common.invariant("instantiation tag payload read had a non-tag-union node");
        const row = try self.flattenTagRow(structural);
        const wanted = self.tagLabelText(name);
        for (row.tags) |tag| {
            if (!Ident.textEql(wanted, self.tagLabelText(tag.name))) continue;
            if (payload_index >= tag.payloads.len) {
                Common.invariant("instantiation tag payload read index exceeded the checked arity");
            }
            return self.find(tag.payloads[payload_index]);
        }
        Common.invariant("instantiation tag payload read requested an absent checked tag");
    }

    /// Project the explicit arguments and backing of a named live node.
    pub fn namedNodes(self: *InstGraph, node: NodeId) NamedNodes {
        const node_content = self.content(node);
        if (node_content != .named) Common.invariant("instantiation named read had a non-named node");
        const named = node_content.named;
        return .{
            .kind = named.kind,
            .args = named.args,
            .backing = if (named.backing) |backing| .{
                .node = self.find(backing.node),
                .use = backing.use,
                .authority = backing.authority,
            } else null,
        };
    }

    /// Return the graph node for one field of a record-shaped node. Field
    /// access is a type relation, so callers use this node directly instead of
    /// selecting a field from a temporary Monotype view and losing later row
    /// evidence.
    pub fn recordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .inspectable, "record field access");
    }

    /// Select a private backing field for
    /// `CheckedFieldBackingAccess.opaque_definition_private`.
    pub fn opaqueDefinitionFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .runtime_layout, "opaque-definition-private record field access");
    }

    /// Return one backing field cell while lowering a checked record
    /// constructor. The explicit API name is the capability to cross a
    /// runtime-layout-only named backing; ordinary field selection must use
    /// `recordFieldNode` and cannot inspect such a backing.
    pub fn recordConstructionFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .runtime_layout, "record constructor");
    }

    fn recordFieldNodeWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_record, noun, access);
        if (self.content(structural) != .record) Common.invariant("instantiation " ++ noun ++ " had a non-record receiver type");
        const row = try self.flattenRecordRow(structural);
        const wanted = self.fieldLabelText(name);
        for (row.fields) |field| {
            if (Ident.textEql(wanted, self.fieldLabelText(field.name))) {
                return self.find(field.ty);
            }
        }
        Common.invariant("instantiation " ++ noun ++ " requested an absent field");
    }

    /// Read every field cell from a record-shaped live node without creating
    /// a temporary Monotype view.
    pub fn recordNodes(self: *InstGraph, raw_record: NodeId) Allocator.Error!RecordNodes {
        const structural = try self.shapeRoot(raw_record, "record", .inspectable);
        const structural_content = self.content(structural);
        if (structural_content == .record) return .{ .fields = (try self.flattenRecordRow(structural)).fields };
        if (structural_content == .empty_record) return .{ .fields = &.{} };
        Common.invariant("instantiation record read had a non-record node");
    }

    /// Project the explicit runtime backing fields needed to emit a checked
    /// record constructor. This is construction-layout access, not structural
    /// type inspection.
    pub fn recordConstructionNodes(self: *InstGraph, raw_record: NodeId) Allocator.Error!RecordNodes {
        const structural = try self.shapeRoot(raw_record, "record constructor", .runtime_layout);
        const structural_content = self.content(structural);
        if (structural_content == .record) return .{ .fields = structural_content.record.fields };
        if (structural_content == .empty_record) return .{ .fields = &.{} };
        Common.invariant("instantiation record constructor had a non-record runtime backing");
    }

    /// A relation mutation invalidates every cached Type-shaped snapshot.
    /// Snapshots may contain the changed node at any structural depth, so
    /// global invalidation is the exact dependency rule. Mutation bursts are
    /// coalesced: the next inspection clears the cache once before reading it.
    /// Observed snapshots remain immutable and valid as historical values.
    fn invalidateActiveSnapshots(self: *InstGraph, _: NodeId) void {
        self.countDiagnostic("active_snapshot_invalidations");
        if (!self.current_snapshots_dirty) {
            self.countDiagnosticBy("active_snapshot_entries_invalidated", self.current_snapshots.count());
            self.current_snapshots_dirty = true;
        }
    }

    fn refreshActiveSnapshots(self: *InstGraph) void {
        if (!self.current_snapshots_dirty) return;
        self.current_snapshots.clearRetainingCapacity();
        self.current_snapshots_dirty = false;
    }

    /// Redirect `loser` into `winner`, moving row back references and
    /// invalidating the current snapshot cache. Immutable snapshot provenance
    /// remains attached to permanent node ids and resolves through `find`.
    fn union_(self: *InstGraph, raw_winner: NodeId, raw_loser: NodeId) Allocator.Error!void {
        const winner = self.find(raw_winner);
        const loser = self.find(raw_loser);
        if (winner == loser) return;
        try self.unregisterRowParent(loser);
        const winner_tail = self.class_member_tail.items[@intFromEnum(winner)];
        const loser_head = self.class_member_head.items[@intFromEnum(loser)];
        self.class_member_next.items[@intFromEnum(winner_tail)] = loser_head;
        self.class_member_tail.items[@intFromEnum(winner)] = self.class_member_tail.items[@intFromEnum(loser)];
        self.nodes.items[@intFromEnum(loser)] = .{ .redirect = winner };
        self.versions.items[@intFromEnum(winner)] +%= 1;
        if (self.row_parents.fetchRemove(loser)) |moved| {
            var moved_list = moved.value;
            for (moved_list.items) |parent| {
                const parent_root = self.find(parent);
                self.row_exts.items[@intFromEnum(parent_root)] = winner;
                try self.addRowParent(winner, parent_root);
            }
            moved_list.deinit(self.allocator);
        }
        self.countDiagnostic("class_unions");
        self.invalidateActiveSnapshots(winner);
    }

    /// Select a named exact type over an unresolved request cell without ever
    /// making the named node its own backing. A checked variable can also be
    /// the declaration-instantiated backing slot of the named node; preserve
    /// that distinct structural role before redirecting the request root.
    fn selectNamedOverUnresolved(
        self: *InstGraph,
        raw_named: NodeId,
        raw_unresolved: NodeId,
    ) Allocator.Error!void {
        const named_node = self.find(raw_named);
        const unresolved_node = self.find(raw_unresolved);
        if (named_node == unresolved_node) return;
        const named_content = self.nodes.items[@intFromEnum(named_node)];
        if (named_content != .named) Common.invariant("named selection received a non-named exact node");
        if (self.nodes.items[@intFromEnum(unresolved_node)] != .unresolved) {
            Common.invariant("named selection received a resolved request node");
        }
        const named = named_content.named;
        if (named.backing) |backing| {
            if (self.find(backing.node) == unresolved_node) {
                const moved = try self.newNode(self.nodes.items[@intFromEnum(unresolved_node)]);
                var rewired = named;
                rewired.backing = .{
                    .node = moved,
                    .use = backing.use,
                    .authority = backing.authority,
                };
                try self.setContent(named_node, .{ .named = rewired });
            }
        }
        try self.union_(named_node, unresolved_node);
    }

    /// Replace a root's content with an observationally equivalent compressed
    /// form without invalidating immutable Type-shaped snapshots. Returns
    /// whether the stored graph content changed.
    fn replaceContentWithoutSnapshotInvalidation(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!bool {
        const root = self.find(raw_root);
        if (instNodeEql(self.nodes.items[@intFromEnum(root)], new_content)) return false;
        self.nodes.items[@intFromEnum(root)] = new_content;
        self.versions.items[@intFromEnum(root)] +%= 1;
        try self.registerRowParent(root, new_content);
        return true;
    }

    /// Replace a root's type content and invalidate every cached snapshot.
    fn setContent(self: *InstGraph, root: NodeId, new_content: InstNode) Allocator.Error!void {
        if (!try self.replaceContentWithoutSnapshotInvalidation(root, new_content)) return;
        self.invalidateActiveSnapshots(root);
    }

    pub fn unify(self: *InstGraph, a: NodeId, b: NodeId) Allocator.Error!void {
        try self.unifyRootsTransitively(a, b);
    }

    /// Select the deterministic common runtime representation for two exact
    /// produced values that meet at one control-flow or storage boundary.
    /// Matching structure is rebuilt from the joined child cells. In
    /// particular, a checked-public generated nominal is a request and the
    /// exact nominal remains the child authority; the two nominal roots are
    /// never merged.
    pub fn joinProducedTypeRepresentations(self: *InstGraph, a: NodeId, b: NodeId) Allocator.Error!NodeId {
        self.countDiagnostic("produced_type_joins");
        if (self.joining_produced_types) {
            Common.invariant("produced representation join reentered its graph-owned memo");
        }
        self.joining_produced_types = true;
        defer self.joining_produced_types = false;
        self.produced_join_memo.clearRetainingCapacity();
        defer self.produced_join_memo.clearRetainingCapacity();
        return try self.joinProducedTypeNodes(a, b, &self.produced_join_memo);
    }

    /// Select one common representation for two stable structural cells at a
    /// storage boundary. Nominal roots are immutable content identities and
    /// must instead use the directed request-to-produced relation.
    pub fn applyCompoundStorageRepresentation(
        self: *InstGraph,
        request_node: NodeId,
        stored_node: NodeId,
    ) Allocator.Error!void {
        const request_root = self.find(request_node);
        const stored_root = self.find(stored_node);
        if (self.content(request_root) == .named or self.content(stored_root) == .named) {
            Common.invariant("compound storage representation selection received a named root");
        }

        const joined = try self.joinProducedTypeRepresentations(request_root, stored_root);
        if (!self.sameClass(request_root, joined)) {
            try self.writeProducedTypeSelection(request_root, joined);
            try self.union_(joined, request_root);
        }
        if (!self.sameClass(stored_root, joined)) {
            try self.writeProducedTypeSelection(stored_root, joined);
            try self.union_(joined, stored_root);
        }
    }

    /// Write the current common representation into one stable control-flow
    /// selection cell. Branch expressions retain this cell while its explicit
    /// child structure is refined by later branches.
    pub fn writeProducedTypeSelection(
        self: *InstGraph,
        selection: NodeId,
        produced: NodeId,
    ) Allocator.Error!void {
        const selection_root = self.find(selection);
        const produced_root = self.find(produced);
        if (selection_root == produced_root) return;
        try self.setContent(selection_root, self.nodes.items[@intFromEnum(produced_root)]);
    }

    fn orderedNodePair(left: NodeId, right: NodeId) NodePair {
        return if (@intFromEnum(left) <= @intFromEnum(right))
            .{ .left = left, .right = right }
        else
            .{ .left = right, .right = left };
    }

    fn joinProducedTypeNodes(
        self: *InstGraph,
        raw_left: NodeId,
        raw_right: NodeId,
        joined: *std.AutoHashMap(NodePair, ProducedJoinMemo),
    ) Allocator.Error!NodeId {
        const left = self.find(raw_left);
        const right = self.find(raw_right);
        if (left == right) return left;
        const pair = orderedNodePair(left, right);
        if (joined.getPtr(pair)) |entry| return switch (entry.*) {
            .done => |existing| self.find(existing),
            .cycle => |placeholder| placeholder,
            .visiting => blk: {
                // Allocate an indirection only for a real recursive re-entry;
                // ordinary joins do not create a node merely for memoization.
                const placeholder = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
                entry.* = .{ .cycle = placeholder };
                break :blk placeholder;
            },
        };

        try joined.put(pair, .visiting);
        const built = try self.joinProducedTypeNodeContents(left, right, joined);
        const entry = joined.getPtr(pair) orelse
            Common.invariant("produced representation join lost its memo entry");
        return switch (entry.*) {
            .visiting => blk: {
                entry.* = .{ .done = built };
                break :blk self.find(built);
            },
            .cycle => |placeholder| blk: {
                try self.writeProducedTypeSelection(placeholder, built);
                if (isGeneratedPrivateRootContent(self.content(placeholder)) and
                    self.content(placeholder).named.generated_iterator != null)
                {
                    try self.registerGeneratedIterator(placeholder);
                }
                entry.* = .{ .done = placeholder };
                break :blk self.find(placeholder);
            },
            .done => Common.invariant("produced representation join completed one pair twice"),
        };
    }

    fn joinProducedTypeNodeContents(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        joined: *std.AutoHashMap(NodePair, ProducedJoinMemo),
    ) Allocator.Error!NodeId {
        const left_content = self.content(left);
        const right_content = self.content(right);
        const left_generated = isGeneratedPrivateRootContent(left_content);
        const right_generated = isGeneratedPrivateRootContent(right_content);
        if (left_generated or right_generated) {
            if (left_generated and right_generated) {
                if (self.sameExactGeneratedPrivateIdentity(left_content.named, right_content.named)) {
                    const pair = orderedNodePair(left, right);
                    try self.union_(pair.left, pair.right);
                    return self.find(left);
                }
                return try self.joinGeneratedIteratorRepresentations(left, right, joined);
            }
            const exact = if (left_generated) left else right;
            const public = if (left_generated) right else left;
            _ = try self.applyCheckedTypeMapping(public, exact);
            return self.find(exact);
        }

        if (left_content == .named and left_content.named.kind == .alias) {
            const backing = left_content.named.backing orelse
                Common.invariant("produced representation join found an alias without backing");
            return try self.joinProducedTypeNodes(backing.node, right, joined);
        }
        if (right_content == .named and right_content.named.kind == .alias) {
            const backing = right_content.named.backing orelse
                Common.invariant("produced representation join found an alias without backing");
            return try self.joinProducedTypeNodes(left, backing.node, joined);
        }

        if (left_content == .unresolved) {
            try self.unify(left, right);
            return self.find(right);
        }
        if (right_content == .unresolved) {
            try self.unify(left, right);
            return self.find(left);
        }
        if (right_content == .named and left_content != .named) {
            return try self.joinProducedTypeNodeContents(right, left, joined);
        }

        return switch (left_content) {
            .redirect, .unresolved => unreachable,
            .primitive => |primitive| blk: {
                if (right_content != .primitive or right_content.primitive != primitive) {
                    Common.invariant("produced representation join received different primitive types");
                }
                break :blk left;
            },
            .list => |left_element| blk: {
                if (right_content != .list) Common.invariant("produced representation join received different type structure");
                const element = try self.joinProducedTypeNodes(
                    left_element,
                    right_content.list,
                    joined,
                );
                if (self.sameClass(element, left_element)) break :blk left;
                if (self.sameClass(element, right_content.list)) break :blk right;
                break :blk try self.newNode(.{ .list = element });
            },
            .box => |left_element| blk: {
                if (right_content != .box) Common.invariant("produced representation join received different type structure");
                const element = try self.joinProducedTypeNodes(
                    left_element,
                    right_content.box,
                    joined,
                );
                if (self.sameClass(element, left_element)) break :blk left;
                if (self.sameClass(element, right_content.box)) break :blk right;
                break :blk try self.newNode(.{ .box = element });
            },
            .tuple => |left_items| blk: {
                if (right_content != .tuple or right_content.tuple.len != left_items.len) {
                    Common.invariant("produced representation join received tuples of different arity");
                }
                const items = try self.allocator.alloc(NodeId, left_items.len);
                defer self.allocator.free(items);
                var all_left = true;
                var all_right = true;
                for (left_items, right_content.tuple, items) |left_item, right_item, *item| {
                    item.* = try self.joinProducedTypeNodes(left_item, right_item, joined);
                    all_left = all_left and self.sameClass(item.*, left_item);
                    all_right = all_right and self.sameClass(item.*, right_item);
                }
                if (all_left) break :blk left;
                if (all_right) break :blk right;
                break :blk try self.newNode(.{ .tuple = try self.arena().dupe(NodeId, items) });
            },
            .func => |left_fn| blk: {
                if (right_content != .func or right_content.func.args.len != left_fn.args.len) {
                    Common.invariant("produced representation join received functions of different arity");
                }
                const args = try self.allocator.alloc(NodeId, left_fn.args.len);
                defer self.allocator.free(args);
                var all_left = true;
                var all_right = true;
                for (left_fn.args, right_content.func.args, args) |left_arg, right_arg, *arg| {
                    arg.* = try self.joinProducedTypeNodes(left_arg, right_arg, joined);
                    all_left = all_left and self.sameClass(arg.*, left_arg);
                    all_right = all_right and self.sameClass(arg.*, right_arg);
                }
                const ret = try self.joinProducedTypeNodes(left_fn.ret, right_content.func.ret, joined);
                all_left = all_left and self.sameClass(ret, left_fn.ret);
                all_right = all_right and self.sameClass(ret, right_content.func.ret);
                if (all_left) break :blk left;
                if (all_right) break :blk right;
                break :blk try self.newNode(.{ .func = .{
                    .args = try self.arena().dupe(NodeId, args),
                    .ret = ret,
                } });
            },
            .tag_union => blk: {
                if (right_content != .tag_union and right_content != .empty_tag_union) {
                    Common.invariant("produced representation join received different type structure");
                }
                break :blk try self.joinProducedTagRows(left, right, joined);
            },
            .empty_tag_union => switch (right_content) {
                .empty_tag_union => left,
                .tag_union => try self.joinProducedTagRows(left, right, joined),
                .redirect,
                .unresolved,
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .record,
                .empty_record,
                .named,
                .erased,
                .zst,
                => Common.invariant("produced representation join received different type structure"),
            },
            .record => blk: {
                if (right_content != .record and right_content != .empty_record) {
                    Common.invariant("produced representation join received different type structure");
                }
                break :blk try self.joinProducedRecordRows(left, right, joined);
            },
            .empty_record => switch (right_content) {
                .empty_record => left,
                .record => try self.joinProducedRecordRows(left, right, joined),
                .redirect,
                .unresolved,
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .tag_union,
                .empty_tag_union,
                .named,
                .erased,
                .zst,
                => Common.invariant("produced representation join received different type structure"),
            },
            .named => |left_named| blk: {
                if (right_content != .named) {
                    const backing = left_named.backing orelse
                        Common.invariant("produced representation join received a nominal without backing");
                    const joined_backing = try self.joinProducedTypeNodes(backing.node, right, joined);
                    var result = left_named;
                    result.backing = .{ .node = joined_backing, .use = backing.use, .authority = backing.authority };
                    break :blk try self.newNode(.{ .named = result });
                }
                const right_named = right_content.named;
                if (!std.meta.eql(left_named.def, right_named.def) or left_named.args.len != right_named.args.len) {
                    Common.invariant("produced representation join received different nominal types");
                }
                const args = try self.allocator.alloc(NodeId, left_named.args.len);
                defer self.allocator.free(args);
                var all_left = true;
                var all_right = true;
                for (left_named.args, right_named.args, args) |left_arg, right_arg, *arg| {
                    arg.* = try self.joinProducedTypeNodes(left_arg, right_arg, joined);
                    all_left = all_left and self.sameClass(arg.*, left_arg);
                    all_right = all_right and self.sameClass(arg.*, right_arg);
                }
                var result = left_named;
                if (left_named.backing) |left_backing| {
                    const right_backing = right_named.backing orelse
                        Common.invariant("produced representation join found backing on only one nominal");
                    if (left_backing.authority != right_backing.authority) {
                        Common.invariant("produced representation join found different nominal backing contracts");
                    }
                    const backing = try self.joinProducedTypeNodes(left_backing.node, right_backing.node, joined);
                    all_left = all_left and self.sameClass(backing, left_backing.node);
                    all_right = all_right and self.sameClass(backing, right_backing.node);
                    // Backing visibility is a lowering capability, not part of
                    // the runtime representation. A join cannot grant the
                    // definition-private inspectable view to a public use, so
                    // retain the more restrictive capability when they meet.
                    const backing_use: Type.BackingUse = if (left_backing.use == .runtime_layout_only or
                        right_backing.use == .runtime_layout_only)
                        .runtime_layout_only
                    else
                        .inspectable;
                    all_left = all_left and left_backing.use == backing_use;
                    all_right = all_right and right_backing.use == backing_use;
                    result.backing = .{
                        .node = backing,
                        .use = backing_use,
                        .authority = left_backing.authority,
                    };
                } else if (right_named.backing != null) {
                    Common.invariant("produced representation join found backing on only one nominal");
                }
                if (all_left) break :blk left;
                if (all_right) break :blk right;
                result.args = try self.arena().dupe(NodeId, args);
                break :blk try self.newNode(.{ .named = result });
            },
            .erased => |left_digest| blk: {
                if (right_content != .erased or !std.mem.eql(u8, &left_digest.bytes, &right_content.erased.bytes)) {
                    Common.invariant("produced representation join received different erased types");
                }
                break :blk left;
            },
            .zst => if (right_content == .zst)
                left
            else
                Common.invariant("produced representation join received different type structure"),
        };
    }

    fn joinGeneratedIteratorRepresentations(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        joined: *std.AutoHashMap(NodePair, ProducedJoinMemo),
    ) Allocator.Error!NodeId {
        const left_named = self.content(left).named;
        const right_named = self.content(right).named;
        if (left_named.def.iterator_representation == .none or
            right_named.def.iterator_representation == .none or
            !sameTypeDef(left_named.def, right_named.def) or
            left_named.kind != right_named.kind or
            left_named.builtin_owner == null or
            right_named.builtin_owner == null or
            !static_dispatch.isIteratorOwner(left_named.builtin_owner.?) or
            left_named.builtin_owner.? != right_named.builtin_owner.? or
            left_named.args.len != 1 or
            right_named.args.len != 1)
        {
            Common.invariant("generated representation join received incompatible exact nominals");
        }
        const left_backing = left_named.backing orelse
            Common.invariant("generated iterator join found no left backing");
        const right_backing = right_named.backing orelse
            Common.invariant("generated iterator join found no right backing");
        if (left_backing.authority != .generated_private or
            right_backing.authority != .generated_private)
        {
            Common.invariant("generated iterator join received a non-private backing");
        }

        // The forced-dynamic representation is already the declared common
        // representation for every minted peer with the same public item.
        // Select it before descending into either backing: its Monotype
        // backing shape is universal, and Lambda Solved later joins the
        // concrete callable members carried by the two occurrences.
        if (left_named.def.iterator_representation == .forced_dynamic or
            right_named.def.iterator_representation == .forced_dynamic)
        {
            const dynamic_node = if (left_named.def.iterator_representation == .forced_dynamic) left else right;
            const dynamic = self.content(dynamic_node).named;
            const item = try self.joinProducedTypeNodes(left_named.args[0], right_named.args[0], joined);
            if (!self.sameClass(item, dynamic.args[0])) {
                Common.invariant("forced-dynamic iterator did not absorb an exact minted item join");
            }
            return dynamic_node;
        }

        const item = try self.joinProducedTypeNodes(left_named.args[0], right_named.args[0], joined);
        const backing = try self.joinProducedTypeNodes(left_backing.node, right_backing.node, joined);

        const ordered_inputs = try self.orderedGeneratedIteratorJoinInputs(left, right);
        const source = self.generatedIteratorSourceFromNamed(self.content(ordered_inputs[0]).named);
        var public_named = InstNamed{
            .named_type = source.named_type,
            .def = source.def,
            .kind = source.kind,
            .builtin_owner = source.builtin_owner,
            .args = try self.arena().dupe(NodeId, &.{item}),
            .backing = source.backing,
            .declared_order = source.declared_order,
        };
        public_named.def.generated = null;
        public_named.def.iterator_representation = .none;
        public_named.def.iterator_kind = .none;
        public_named.def.iterator_depth = 0;

        const lookup = try self.lookupGeneratedIteratorFromNamed(
            public_named,
            .join,
            &ordered_inputs,
            null,
        );
        if (lookup.existing) |existing| return existing;

        var def = public_named.def;
        def.iterator_representation = .minted;
        def.iterator_kind = .join;
        def.iterator_depth = @max(left_named.def.iterator_depth, right_named.def.iterator_depth);
        if (def.iterator_depth == 0) {
            Common.invariant("generated iterator join received an unfinished input depth");
        }
        const result = try self.newNode(.{ .named = .{
            .named_type = source.named_type,
            .def = def,
            .kind = source.kind,
            .builtin_owner = source.builtin_owner,
            .args = try self.arena().dupe(NodeId, &.{item}),
            .backing = .{
                .node = backing,
                .use = if (left_backing.use == .runtime_layout_only or right_backing.use == .runtime_layout_only)
                    .runtime_layout_only
                else
                    .inspectable,
                .authority = .generated_private,
            },
            .generated_iterator = .{
                .callable_evidence = null,
                .components = try self.arena().dupe(NodeId, &ordered_inputs),
                .public_source = source,
            },
            .declared_order = source.declared_order,
        } });
        try self.registerGeneratedIteratorAtDigest(result, lookup.digest);
        return result;
    }

    fn orderedGeneratedIteratorJoinInputs(self: *InstGraph, left: NodeId, right: NodeId) Allocator.Error![2]NodeId {
        const left_digest = try self.generatedIteratorJoinOrderDigest(left);
        const right_digest = try self.generatedIteratorJoinOrderDigest(right);
        return if (std.mem.order(u8, &left_digest.bytes, &right_digest.bytes) == .gt)
            .{ right, left }
        else
            .{ left, right };
    }

    fn generatedIteratorJoinOrderDigest(self: *InstGraph, node: NodeId) Allocator.Error!names.TypeDigest {
        const named = self.content(node).named;
        if (named.generated_iterator == null) {
            return named.def.generated orelse
                Common.invariant("finished generated iterator join input had no stable identity");
        }
        var finalizer = GeneratedIteratorIdentityFinalizer.init(self);
        defer finalizer.deinit();
        return try finalizer.digestFor(node);
    }

    pub fn generatedIteratorPublicSource(self: *InstGraph, raw_node: NodeId) InstIteratorPublicSource {
        const named = switch (self.content(raw_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("iterator public source requested from a non-named node"),
        };
        return self.generatedIteratorSourceFromNamed(named);
    }

    fn generatedIteratorSourceFromNamed(_: *InstGraph, named: InstNamed) InstIteratorPublicSource {
        if (named.generated_iterator) |generated| return generated.public_source;
        const backing = named.backing orelse
            Common.invariant("iterator representation source had no backing");
        const owner = named.builtin_owner orelse
            Common.invariant("iterator representation source had no builtin owner");
        if (!static_dispatch.isIteratorOwner(owner)) {
            Common.invariant("iterator representation source had a non-iterator owner");
        }
        if (named.def.iterator_representation == .none and backing.authority != .checked_public) {
            Common.invariant("public iterator representation source had private backing authority");
        }
        if (named.def.iterator_representation != .none and backing.authority != .generated_private) {
            Common.invariant("finished iterator representation source had public backing authority");
        }
        var public_def = named.def;
        public_def.generated = null;
        public_def.iterator_representation = .none;
        public_def.iterator_kind = .none;
        public_def.iterator_depth = 0;
        return .{
            .named_type = named.named_type,
            .def = public_def,
            .kind = named.kind,
            .builtin_owner = owner,
            // Finished generated iterator backings retain the checker-declared
            // outer iterator topology. The explicit topology IDs—not
            // field-name inference—authorize the exact backing as the
            // structural source for another adapter or a forced-dynamic join.
            .backing = .{
                .node = backing.node,
                .use = backing.use,
                .authority = .checked_public,
            },
            .declared_order = named.declared_order,
        };
    }

    fn instTagLessThan(graph: *InstGraph, left: InstTag, right: InstTag) bool {
        return std.mem.order(u8, graph.tagLabelText(left.name), graph.tagLabelText(right.name)) == .lt;
    }

    fn instFieldLessThan(graph: *InstGraph, left: InstField, right: InstField) bool {
        return std.mem.order(u8, graph.fieldLabelText(left.name), graph.fieldLabelText(right.name)) == .lt;
    }

    fn joinProducedTagRows(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        joined: *std.AutoHashMap(NodePair, ProducedJoinMemo),
    ) Allocator.Error!NodeId {
        const left_row: FlatTagRow = if (self.content(left) == .empty_tag_union)
            .{ .tags = &.{}, .ext = left }
        else
            try self.flattenTagRow(left);
        const right_row: FlatTagRow = if (self.content(right) == .empty_tag_union)
            .{ .tags = &.{}, .ext = right }
        else
            try self.flattenTagRow(right);
        var tags = std.ArrayList(InstTag).empty;
        defer tags.deinit(self.allocator);
        var owned_payloads = std.ArrayList([]NodeId).empty;
        defer {
            for (owned_payloads.items) |payloads| self.allocator.free(payloads);
            owned_payloads.deinit(self.allocator);
        }
        var all_left = true;
        var all_right = true;
        for (left_row.tags) |left_tag| {
            var matched: ?InstTag = null;
            for (right_row.tags) |right_tag| {
                if (!Ident.textEql(self.tagLabelText(left_tag.name), self.tagLabelText(right_tag.name))) continue;
                matched = right_tag;
                break;
            }
            if (matched) |right_tag| {
                if (left_tag.payloads.len != right_tag.payloads.len) {
                    Common.invariant("produced representation join received one tag at two payload arities");
                }
                const payloads = try self.allocator.alloc(NodeId, left_tag.payloads.len);
                try owned_payloads.append(self.allocator, payloads);
                for (left_tag.payloads, right_tag.payloads, payloads) |left_payload, right_payload, *payload| {
                    payload.* = try self.joinProducedTypeNodes(left_payload, right_payload, joined);
                    all_left = all_left and self.sameClass(payload.*, left_payload);
                    all_right = all_right and self.sameClass(payload.*, right_payload);
                }
                try tags.append(self.allocator, .{
                    .name = left_tag.name,
                    .checked_name = left_tag.checked_name,
                    .payloads = payloads,
                });
            } else {
                all_right = false;
                try tags.append(self.allocator, left_tag);
            }
        }
        for (right_row.tags) |right_tag| {
            for (left_row.tags) |left_tag| {
                if (Ident.textEql(self.tagLabelText(left_tag.name), self.tagLabelText(right_tag.name))) break;
            } else {
                all_left = false;
                try tags.append(self.allocator, right_tag);
            }
        }
        std.mem.sort(InstTag, tags.items, self, instTagLessThan);
        const ext = try self.joinProducedTypeNodes(left_row.ext, right_row.ext, joined);
        all_left = all_left and self.sameClass(ext, left_row.ext);
        all_right = all_right and self.sameClass(ext, right_row.ext);
        if (all_left) return left;
        if (all_right) return right;
        if (tags.items.len == 0) return ext;
        const stored_tags = try self.arena().alloc(InstTag, tags.items.len);
        for (tags.items, stored_tags) |tag, *stored| {
            stored.* = tag;
            stored.payloads = try self.arena().dupe(NodeId, tag.payloads);
        }
        return try self.newNode(.{ .tag_union = .{
            .tags = stored_tags,
            .ext = ext,
        } });
    }

    fn joinProducedRecordRows(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        joined: *std.AutoHashMap(NodePair, ProducedJoinMemo),
    ) Allocator.Error!NodeId {
        const left_row: FlatRecordRow = if (self.content(left) == .empty_record)
            .{ .fields = &.{}, .ext = left }
        else
            try self.flattenRecordRow(left);
        const right_row: FlatRecordRow = if (self.content(right) == .empty_record)
            .{ .fields = &.{}, .ext = right }
        else
            try self.flattenRecordRow(right);
        var fields = std.ArrayList(InstField).empty;
        defer fields.deinit(self.allocator);
        var all_left = true;
        var all_right = true;
        for (left_row.fields) |left_field| {
            var matched: ?InstField = null;
            for (right_row.fields) |right_field| {
                if (!Ident.textEql(self.fieldLabelText(left_field.name), self.fieldLabelText(right_field.name))) continue;
                matched = right_field;
                break;
            }
            if (matched) |right_field| {
                const field_ty = try self.joinProducedTypeNodes(left_field.ty, right_field.ty, joined);
                all_left = all_left and self.sameClass(field_ty, left_field.ty);
                all_right = all_right and self.sameClass(field_ty, right_field.ty);
                try fields.append(self.allocator, .{ .name = left_field.name, .ty = field_ty });
            } else {
                all_right = false;
                try fields.append(self.allocator, left_field);
            }
        }
        for (right_row.fields) |right_field| {
            for (left_row.fields) |left_field| {
                if (Ident.textEql(self.fieldLabelText(left_field.name), self.fieldLabelText(right_field.name))) break;
            } else {
                all_left = false;
                try fields.append(self.allocator, right_field);
            }
        }
        std.mem.sort(InstField, fields.items, self, instFieldLessThan);
        const ext = try self.joinProducedTypeNodes(left_row.ext, right_row.ext, joined);
        all_left = all_left and self.sameClass(ext, left_row.ext);
        all_right = all_right and self.sameClass(ext, right_row.ext);
        if (all_left) return left;
        if (all_right) return right;
        if (fields.items.len == 0) return ext;
        return try self.newNode(.{ .record = .{
            .fields = try self.arena().dupe(InstField, fields.items),
            .ext = ext,
        } });
    }

    fn unifyRootsTransitively(
        self: *InstGraph,
        a: NodeId,
        b: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        self.countDiagnostic("unify_requests");
        var pending = std.ArrayList(NodePair).empty;
        defer pending.deinit(self.allocator);
        var related = std.AutoHashMap(NodePair, void).init(self.allocator);
        defer related.deinit();
        try pending.append(self.allocator, .{ .left = a, .right = b });
        while (pending.pop()) |pair| {
            try self.unifyRoots(pair.left, pair.right, &pending, &related);
        }
    }

    fn unifyRoots(
        self: *InstGraph,
        raw_left: NodeId,
        raw_right: NodeId,
        pending: *std.ArrayList(NodePair),
        related: *std.AutoHashMap(NodePair, void),
    ) Allocator.Error!void {
        const left = self.find(raw_left);
        const right = self.find(raw_right);
        if (left == right) return;
        const pair = NodePair{ .left = left, .right = right };
        if (related.contains(pair)) return;
        try related.put(pair, {});
        const relation = self.relationStamp(left, right);
        if (self.processed_relations.contains(relation)) return;
        try self.processed_relations.put(relation, {});

        const left_content = self.nodes.items[@intFromEnum(left)];
        const right_content = self.nodes.items[@intFromEnum(right)];
        const left_generated_private = left_content == .named and
            (if (left_content.named.backing) |backing| backing.authority == .generated_private else false);
        const right_generated_private = right_content == .named and
            (if (right_content.named.backing) |backing| backing.authority == .generated_private else false);
        if (left_generated_private != right_generated_private) {
            Common.invariant("generated-private representation reached ordinary public/private graph unification");
        }

        if (left_content == .redirect) unreachable;
        if (left_content == .unresolved) {
            if (right_content == .unresolved) {
                try self.setContent(right, .{ .unresolved = mergeVariables(left_content.unresolved, right_content.unresolved) });
                try self.union_(right, left);
            } else if (right_content == .named and right_content.named.kind == .alias) {
                try self.unifyThroughBacking(right, right_content, left, pending);
            } else if (right_content == .named) {
                try self.selectNamedOverUnresolved(right, left);
            } else {
                try self.union_(right, left);
            }
        } else if (right_content == .unresolved) {
            if (left_content == .named and left_content.named.kind == .alias) {
                try self.unifyThroughBacking(left, left_content, right, pending);
            } else if (left_content == .named) {
                try self.selectNamedOverUnresolved(left, right);
            } else {
                try self.union_(left, right);
            }
        } else {
            try self.unifyConcrete(left, left_content, right, right_content, pending);
        }
    }

    fn relationStamp(self: *InstGraph, left: NodeId, right: NodeId) RelationStamp {
        const left_raw = @intFromEnum(left);
        const right_raw = @intFromEnum(right);
        if (left_raw <= right_raw) {
            return .{
                .left = left,
                .left_version = self.versions.items[left_raw],
                .right = right,
                .right_version = self.versions.items[right_raw],
            };
        }
        return .{
            .left = right,
            .left_version = self.versions.items[right_raw],
            .right = left,
            .right_version = self.versions.items[left_raw],
        };
    }

    fn mergeVariables(a: InstVariable, b: InstVariable) InstVariable {
        return .{
            .origin = mergeVariableOrigin(a.origin, b.origin),
            .numeric_default_phase = a.numeric_default_phase orelse b.numeric_default_phase,
            .row_default = a.row_default orelse b.row_default,
        };
    }

    fn mergeVariableOrigin(a: InstVariableOrigin, b: InstVariableOrigin) InstVariableOrigin {
        if (a == .checked_variable or b == .checked_variable) return .checked_variable;
        if (a == .row_extension or b == .row_extension) return .row_extension;
        return .placeholder;
    }

    fn unifyConcrete(
        self: *InstGraph,
        left: NodeId,
        left_content: InstNode,
        right: NodeId,
        right_content: InstNode,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        switch (left_content) {
            .redirect, .unresolved => unreachable,
            .primitive => |left_prim| {
                if (right_content == .primitive) {
                    if (left_prim != right_content.primitive) Common.invariant("instantiation unified two different primitive types");
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a primitive type with a non-primitive type");
                }
            },
            .list => |left_elem| {
                if (right_content == .list) {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_content.list });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a List with a non-List type");
                }
            },
            .box => |left_elem| {
                if (right_content == .box) {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_content.box });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a Box with a non-Box type");
                }
            },
            .tuple => |left_items| {
                if (right_content == .tuple) {
                    const right_items = right_content.tuple;
                    if (left_items.len != right_items.len) Common.invariant("instantiation unified tuples of different arity");
                    for (left_items, right_items) |left_item, right_item| {
                        try pending.append(self.allocator, .{ .left = left_item, .right = right_item });
                    }
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a tuple with a non-tuple type");
                }
            },
            .func => |left_fn| {
                if (right_content == .func) {
                    const right_fn = right_content.func;
                    if (left_fn.args.len != right_fn.args.len) Common.invariant("instantiation unified functions of different arity");
                    for (left_fn.args, right_fn.args) |left_arg, right_arg| {
                        try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                    }
                    try pending.append(self.allocator, .{ .left = left_fn.ret, .right = right_fn.ret });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a function with a non-function type");
                }
            },
            .tag_union => {
                if (right_content == .tag_union) {
                    try self.unifyTagRows(left, right, pending);
                } else if (right_content == .empty_tag_union) {
                    try self.unifyRowWithEmpty(left, right, .tag_union);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a tag union with a non-tag-union type");
                }
            },
            .empty_tag_union => {
                if (right_content == .empty_tag_union) {
                    try self.union_(left, right);
                } else if (right_content == .tag_union) {
                    try self.unifyRowWithEmpty(right, left, .tag_union);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified an empty tag union with an incompatible type");
                }
            },
            .record => {
                if (right_content == .record) {
                    try self.unifyRecordRows(left, right, pending);
                } else if (right_content == .empty_record) {
                    try self.unifyRowWithEmpty(left, right, .record);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a record with a non-record type");
                }
            },
            .empty_record => {
                if (right_content == .empty_record) {
                    try self.union_(left, right);
                } else if (right_content == .record) {
                    try self.unifyRowWithEmpty(right, left, .record);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified an empty record with an incompatible type");
                }
            },
            .named => |left_named| {
                if (right_content == .named) {
                    const right_named = right_content.named;
                    if (left_named.kind == .alias) {
                        try self.unifyThroughBacking(left, left_content, right, pending);
                        return;
                    }
                    if (right_named.kind == .alias) {
                        try self.unifyThroughBacking(right, right_content, left, pending);
                        return;
                    }
                    switch (self.iteratorRelation(left_named, right_named)) {
                        .ordinary => {},
                        .public_minted => {
                            if (left_named.args.len == 0 or right_named.args.len == 0) {
                                Common.invariant("minted/public iterator pair reached Monotype instantiation without a public item argument");
                            }
                            try pending.append(self.allocator, .{
                                .left = left_named.args[0],
                                .right = right_named.args[0],
                            });
                            if (left_named.def.iterator_representation == .minted) {
                                try self.union_(left, right);
                            } else {
                                try self.union_(right, left);
                            }
                            return;
                        },
                        .forced_dynamic => {
                            if (left_named.args.len == 0 or right_named.args.len == 0) {
                                Common.invariant("forced-dynamic iterator reached Monotype instantiation without a public item argument");
                            }
                            try pending.append(self.allocator, .{
                                .left = left_named.args[0],
                                .right = right_named.args[0],
                            });
                            if (left_named.def.iterator_representation == .forced_dynamic) {
                                try self.union_(left, right);
                            } else {
                                try self.union_(right, left);
                            }
                            return;
                        },
                        .minted_join => {
                            if (left_named.args.len == 0 or right_named.args.len == 0) {
                                Common.invariant("minted iterator join reached Monotype instantiation without a public item argument");
                            }
                            try pending.append(self.allocator, .{
                                .left = left_named.args[0],
                                .right = right_named.args[0],
                            });
                            if (left_named.backing) |left_backing| {
                                const right_backing = right_named.backing orelse
                                    Common.invariant("minted iterator join found backing on only one side");
                                if (left_backing.use != right_backing.use) {
                                    Common.invariant("minted iterator join found different backing uses");
                                }
                                if (left_backing.authority != right_backing.authority) {
                                    Common.invariant("minted iterator join found different backing authorities");
                                }
                                try pending.append(self.allocator, .{
                                    .left = left_backing.node,
                                    .right = right_backing.node,
                                });
                            } else if (right_named.backing != null) {
                                Common.invariant("minted iterator join found backing on only one side");
                            }

                            if (self.classContainsMarkedNode(left, &self.recursive_argument_slots) or
                                self.classContainsMarkedNode(right, &self.recursive_argument_slots))
                            {
                                try self.forced_dynamic_iterator_roots.put(left, {});
                            }

                            // A graph-owned producer still has the public-source
                            // provenance required to finalize a newly joined
                            // representation; an imported finished Monotype
                            // deliberately does not. Preserve that explicit
                            // authority when only one side owns it. This is
                            // especially important for recursive joins, whose
                            // selected root must still be rewritable to the
                            // forced-dynamic fixed point below.
                            const left_owns_provenance = left_named.generated_iterator != null;
                            const right_owns_provenance = right_named.generated_iterator != null;
                            if (left_owns_provenance != right_owns_provenance) {
                                if (left_owns_provenance) {
                                    try self.union_(left, right);
                                } else {
                                    try self.union_(right, left);
                                }
                            } else if (left_named.builtin_owner) |left_owner| {
                                // Close recursive `rest` references before the
                                // backing pair is drained. Otherwise each
                                // nominal unwrap creates another fresh
                                // structural node.
                                if (!static_dispatch.isIteratorOwner(left_owner)) unreachable;
                                try self.union_(left, right);
                            } else {
                                try self.union_(right, left);
                            }
                            return;
                        },
                    }
                    if (std.meta.eql(left_named.def, right_named.def) and left_named.args.len == right_named.args.len) {
                        for (left_named.args, right_named.args) |left_arg, right_arg| {
                            try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                        }
                        if (left_named.backing) |left_backing| {
                            if (right_named.backing) |right_backing| {
                                if (left_backing.authority == right_backing.authority) {
                                    const left_wraps_right = self.find(left_backing.node) == right;
                                    const right_wraps_left = self.find(right_backing.node) == left;
                                    if (left_wraps_right and right_wraps_left) {
                                        Common.invariant("equivalent named nodes formed a two-node backing cycle");
                                    }
                                    if (left_wraps_right) {
                                        // The left node is an extra view of the
                                        // same nominal around the right node.
                                        // Compress that wrapper before the
                                        // right root redirects into the left.
                                        var compressed = left_named;
                                        compressed.backing = .{
                                            .node = right_backing.node,
                                            .use = left_backing.use,
                                            .authority = left_backing.authority,
                                        };
                                        try self.setContent(left, .{ .named = compressed });
                                    } else if (!right_wraps_left) {
                                        try pending.append(self.allocator, .{ .left = left_backing.node, .right = right_backing.node });
                                    }
                                } else {
                                    const private_is_left = left_backing.authority == .generated_private;
                                    const private_is_right = right_backing.authority == .generated_private;
                                    if (private_is_left == private_is_right) {
                                        Common.invariant("instantiation named backing authorities were incompatible");
                                    }
                                    if (private_is_left) {
                                        try self.union_(left, right);
                                    } else {
                                        try self.union_(right, left);
                                    }
                                    return;
                                }
                            } else {
                                Common.invariant("instantiation named type backing presence differed");
                            }
                        } else if (right_named.backing != null) {
                            Common.invariant("instantiation named type backing presence differed");
                        }
                        try self.union_(left, right);
                        return;
                    }
                    try self.unifyThroughBacking(left, left_content, right, pending);
                } else {
                    try self.unifyThroughBacking(left, left_content, right, pending);
                }
            },
            .erased => |left_digest| {
                if (right_content == .erased) {
                    if (!std.mem.eql(u8, left_digest.bytes[0..], right_content.erased.bytes[0..])) {
                        Common.invariant("instantiation unified two different erased types");
                    }
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified an erased type with an incompatible type");
                }
            },
            .zst => {
                if (right_content == .zst) {
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, pending);
                } else {
                    Common.invariant("instantiation unified a zero-sized type with an incompatible type");
                }
            },
        }
    }

    fn iteratorRelation(self: *InstGraph, left: InstNamed, right: InstNamed) Type.IteratorRelation {
        const base_relation = Type.iteratorRelation(left, right);
        if (base_relation != .ordinary) return base_relation;
        if (left.def.iterator_representation != .minted or right.def.iterator_representation != .minted) {
            return .ordinary;
        }
        if (left.kind != right.kind or
            left.def.module != right.def.module or
            left.def.type_name != right.def.type_name or
            left.def.source_decl != right.def.source_decl or
            !instIteratorOwnerPair(left.builtin_owner, right.builtin_owner))
        {
            return .ordinary;
        }
        if (left.generated_iterator != null or right.generated_iterator != null) {
            if (left.generated_iterator == null or right.generated_iterator == null) return .minted_join;
            if (!optionalInstDigestEql(
                left.generated_iterator.?.callable_evidence,
                right.generated_iterator.?.callable_evidence,
            )) return .minted_join;
            if (left.def.iterator_kind != right.def.iterator_kind or
                !self.sameNamedArgs(left.args, right.args)) return .minted_join;
            return .ordinary;
        }
        return Type.iteratorRelation(left, right);
    }

    /// Directed request-to-produced substitution may deduplicate two copies of
    /// one exact private identity, but it is not a representation join. Graph-
    /// local producers compare their complete construction inputs; imported
    /// producers compare the stable identity already sealed into their type
    /// definitions. A local unfinished identity cannot equal an imported
    /// finished identity inside the active graph.
    fn sameExactGeneratedPrivateIdentity(self: *InstGraph, left: InstNamed, right: InstNamed) bool {
        const left_is_iterator = left.def.iterator_representation != .none;
        const right_is_iterator = right.def.iterator_representation != .none;
        if (left.kind != right.kind or
            left.def.module != right.def.module or
            left.def.type_name != right.def.type_name or
            left.def.source_decl != right.def.source_decl or
            left.def.iterator_representation != right.def.iterator_representation or
            left.def.iterator_kind != right.def.iterator_kind or
            !self.sameNamedArgs(left.args, right.args))
        {
            return false;
        }
        if (!left_is_iterator and !right_is_iterator) {
            return optionalInstDigestEql(left.def.generated, right.def.generated);
        }
        if (left_is_iterator != right_is_iterator) return false;

        if (left.generated_iterator) |left_generated| {
            const right_generated = right.generated_iterator orelse return false;
            if (!optionalInstDigestEql(left_generated.callable_evidence, right_generated.callable_evidence) or
                left_generated.components.len != right_generated.components.len)
            {
                return false;
            }
            for (left_generated.components, right_generated.components) |left_component, right_component| {
                if (self.find(left_component) != self.find(right_component)) return false;
            }
            return true;
        }
        if (right.generated_iterator != null) return false;
        return optionalInstDigestEql(left.def.generated, right.def.generated);
    }

    /// A named type met a structurally different type. Aliases are transparent
    /// downstream, so an alias relates through its backing without merging
    /// roots. A nominal becomes the single node both sides resolve to: the
    /// other side's structure moves to a fresh node that unifies with the
    /// nominal's backing, so every Monotype view of either side carries the
    /// named wrapper, exactly as later stages expect.
    fn unifyThroughBacking(
        self: *InstGraph,
        named_node: NodeId,
        named_content: InstNode,
        other: NodeId,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        if (named_content != .named) unreachable;
        const named = named_content.named;
        const declared_backing = named.backing orelse
            Common.invariant("instantiation unified an opaque type without backing against a structural type");
        const backing = try self.structuralBackingNode(declared_backing.node, named);
        const backing_node = backing.node;
        if (backing.recursive) {
            if (named.kind == .alias) {
                Common.invariant("alias backing cycle reached Monotype instantiation");
            }
            if (self.nodes.items[@intFromEnum(other)] == .named) {
                Common.invariant("recursive nominal backing met a different named type");
            }
            try self.union_(named_node, other);
            return;
        }
        if (declared_backing.node != backing_node) {
            var compressed = named;
            compressed.backing = .{ .node = backing_node, .use = declared_backing.use, .authority = declared_backing.authority };
            try self.setContent(named_node, .{ .named = compressed });
        }
        if (named.kind == .alias) {
            try pending.append(self.allocator, .{ .left = backing_node, .right = other });
            return;
        }
        if (self.nodes.items[@intFromEnum(other)] == .named) {
            try pending.append(self.allocator, .{ .left = backing_node, .right = other });
            return;
        }
        // If `other` is this nominal's declared backing, redirecting it first
        // would make the nominal point to its own union-find class. Preserve a
        // structural copy as the backing before selecting the nominal root.
        if (self.find(backing_node) == self.find(other)) {
            const moved = try self.newNode(self.nodes.items[@intFromEnum(other)]);
            var rewired = named;
            rewired.backing = .{
                .node = moved,
                .use = declared_backing.use,
                .authority = declared_backing.authority,
            };
            try self.setContent(named_node, .{ .named = rewired });
            try self.union_(named_node, other);
            return;
        }
        const moved = try self.newNode(self.nodes.items[@intFromEnum(other)]);
        try self.union_(named_node, other);
        try pending.append(self.allocator, .{ .left = backing_node, .right = moved });
    }

    const StructuralBacking = struct {
        node: NodeId,
        recursive: bool,
    };

    fn structuralBackingNode(self: *InstGraph, raw: NodeId, owner: InstNamed) Allocator.Error!StructuralBacking {
        const result = try self.findStructuralBackingNode(raw, owner);
        if (!result.recursive) {
            try self.compressStructuralBacking(raw, owner, result.node);
        }
        return result;
    }

    fn findStructuralBackingNode(self: *InstGraph, raw: NodeId, owner: InstNamed) Allocator.Error!StructuralBacking {
        var seen = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.nodes.items.len);
        defer seen.deinit(self.allocator);
        var current = self.find(raw);
        while (true) {
            const index = @intFromEnum(current);
            if (seen.isSet(index)) return .{ .node = current, .recursive = true };
            seen.set(index);
            const next = self.structuralBackingNext(current, owner) orelse return .{ .node = current, .recursive = false };
            current = next;
        }
    }

    fn compressStructuralBacking(self: *InstGraph, raw: NodeId, owner: InstNamed, result: NodeId) Allocator.Error!void {
        var current = self.find(raw);
        while (current != result) {
            const node_content = self.nodes.items[@intFromEnum(current)];
            if (node_content != .named) Common.invariant("named backing compression reached a structural node before its result");
            const named = node_content.named;
            if (named.kind != .alias and !self.sameNamedInstance(named, owner)) {
                Common.invariant("named backing compression reached a non-transparent named type");
            }
            const backing = named.backing orelse
                Common.invariant("named backing compression reached a named type without backing");
            const next = self.find(backing.node);
            if (backing.node != result) {
                var compressed = named;
                compressed.backing = .{ .node = result, .use = backing.use, .authority = backing.authority };
                try self.setContent(current, .{ .named = compressed });
            }
            current = next;
        }
    }

    fn structuralBackingNext(self: *InstGraph, raw: NodeId, owner: InstNamed) ?NodeId {
        const current = self.find(raw);
        const node_content = self.nodes.items[@intFromEnum(current)];
        if (node_content != .named) return null;
        const named = node_content.named;
        if (named.kind != .alias and !self.sameNamedInstance(named, owner)) return null;
        const backing = named.backing orelse
            Common.invariant("named backing chain reached a named type without backing");
        return self.find(backing.node);
    }

    fn sameNamedInstance(self: *InstGraph, left: InstNamed, right: InstNamed) bool {
        return left.kind == right.kind and
            sameTypeDef(left.def, right.def) and
            left.builtin_owner == right.builtin_owner and
            self.sameNamedArgs(left.args, right.args);
    }

    fn sameNamedArgs(self: *InstGraph, left: []const NodeId, right: []const NodeId) bool {
        if (left.len != right.len) return false;
        for (left, right) |left_arg, right_arg| {
            if (self.find(left_arg) != self.find(right_arg)) return false;
        }
        return true;
    }

    fn sameTypeDef(left: Type.TypeDef, right: Type.TypeDef) bool {
        return left.module == right.module and
            left.type_name == right.type_name and
            left.source_decl == right.source_decl;
    }

    const RowKind = enum {
        tag_union,
        record,
    };

    fn rowAdditionConflicts(
        self: *InstGraph,
        raw_ext: NodeId,
        addition_count: usize,
        kind: RowKind,
    ) bool {
        if (addition_count == 0) return false;
        return switch (self.nodes.items[@intFromEnum(self.find(raw_ext))]) {
            .unresolved => false,
            .empty_tag_union => switch (kind) {
                .tag_union => true,
                .record => Common.invariant("record row terminated in an empty tag-union extension"),
            },
            .empty_record => switch (kind) {
                .record => true,
                .tag_union => Common.invariant("tag row terminated in an empty record extension"),
            },
            .redirect,
            .primitive,
            .list,
            .box,
            .tuple,
            .func,
            .tag_union,
            .record,
            .named,
            .erased,
            .zst,
            => Common.invariant("flattened row did not terminate in an unresolved or empty extension"),
        };
    }

    /// A row with a head met an empty row: the head must be empty too, and the
    /// row's extension must also be empty.
    fn unifyRowWithEmpty(self: *InstGraph, row: NodeId, empty: NodeId, kind: RowKind) Allocator.Error!void {
        switch (kind) {
            .tag_union => {
                const flat = try self.flattenTagRow(row);
                if (flat.tags.len != 0) Common.invariant("instantiation unified a non-empty tag union with an empty tag union");
                try self.unify(flat.ext, empty);
                try self.setContent(row, .empty_tag_union);
                try self.union_(empty, row);
            },
            .record => {
                const flat = try self.flattenRecordRow(row);
                if (flat.fields.len != 0) Common.invariant("instantiation unified a non-empty record with an empty record");
                try self.unify(flat.ext, empty);
                try self.setContent(row, .empty_record);
                try self.union_(empty, row);
            },
        }
    }

    const FlatTagRow = struct {
        tags: []InstTag,
        ext: NodeId,
    };

    const FlatRecordRow = struct {
        fields: []InstField,
        ext: NodeId,
    };

    /// Chase a tag row's extension chain and rewrite the root to a single
    /// flattened row. The returned extension is unresolved (open), an empty tag
    /// union (closed), or compressed out.
    fn flattenTagRow(self: *InstGraph, raw_root: NodeId) Allocator.Error!FlatTagRow {
        const root = self.find(raw_root);
        const root_content = self.nodes.items[@intFromEnum(root)];
        if (root_content != .tag_union) Common.invariant("instantiation flattened a non-tag-union row");
        const row = root_content.tag_union;
        var tags = std.ArrayList(InstTag).empty;
        defer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, row.tags);

        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try seen.put(root, {});

        var ext = self.find(row.ext);
        const ext_content = self.nodes.items[@intFromEnum(ext)];
        if (ext_content == .unresolved or ext_content == .empty_tag_union) {
            if (row.ext != ext) {
                const flattened: InstNode = .{ .tag_union = .{ .tags = row.tags, .ext = ext } };
                _ = try self.replaceContentWithoutSnapshotInvalidation(root, flattened);
            }
            return .{ .tags = row.tags, .ext = ext };
        }

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further tags—every
                // tag on the cycle is already collected—but the row remains
                // extensible, so the chain terminates open.
                ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
                break;
            }
            try seen.put(ext, {});
            switch (self.nodes.items[@intFromEnum(ext)]) {
                .tag_union => |tail| {
                    try tags.appendSlice(self.allocator, tail.tags);
                    ext = self.find(tail.ext);
                },
                .unresolved, .empty_tag_union => break,
                .redirect,
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .record,
                .empty_record,
                .named,
                .erased,
                .zst,
                => Common.invariant("instantiation tag row extended into a non-tag-union type"),
            }
        }

        const flat_tags = try self.arena().dupe(InstTag, tags.items);
        const flattened: InstNode = .{ .tag_union = .{ .tags = flat_tags, .ext = ext } };
        _ = try self.replaceContentWithoutSnapshotInvalidation(root, flattened);
        return .{ .tags = flat_tags, .ext = ext };
    }

    fn flattenRecordRow(self: *InstGraph, raw_root: NodeId) Allocator.Error!FlatRecordRow {
        const root = self.find(raw_root);
        const root_content = self.nodes.items[@intFromEnum(root)];
        if (root_content != .record) Common.invariant("instantiation flattened a non-record row");
        const row = root_content.record;
        var fields = std.ArrayList(InstField).empty;
        defer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, row.fields);

        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try seen.put(root, {});

        var ext = self.find(row.ext);
        const ext_content = self.nodes.items[@intFromEnum(ext)];
        if (ext_content == .unresolved or ext_content == .empty_record) {
            if (row.ext != ext) {
                const flattened: InstNode = .{ .record = .{ .fields = row.fields, .ext = ext } };
                _ = try self.replaceContentWithoutSnapshotInvalidation(root, flattened);
            }
            return .{ .fields = row.fields, .ext = ext };
        }

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further fields—
                // every field on the cycle is already collected—but the row
                // remains extensible, so the chain terminates open.
                ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
                break;
            }
            try seen.put(ext, {});
            switch (self.nodes.items[@intFromEnum(ext)]) {
                .record => |tail| {
                    try fields.appendSlice(self.allocator, tail.fields);
                    ext = self.find(tail.ext);
                },
                .unresolved, .empty_record => break,
                .redirect,
                .primitive,
                .list,
                .box,
                .tuple,
                .func,
                .tag_union,
                .empty_tag_union,
                .named,
                .erased,
                .zst,
                => Common.invariant("instantiation record row extended into a non-record type"),
            }
        }

        const flat_fields = try self.arena().dupe(InstField, fields.items);
        const flattened: InstNode = .{ .record = .{ .fields = flat_fields, .ext = ext } };
        _ = try self.replaceContentWithoutSnapshotInvalidation(root, flattened);
        return .{ .fields = flat_fields, .ext = ext };
    }

    fn tagLabelText(self: *InstGraph, name: names.TagNameId) []const u8 {
        return self.name_store.tagLabelText(name);
    }

    fn fieldLabelText(self: *InstGraph, name: names.RecordFieldNameId) []const u8 {
        return self.name_store.recordFieldLabelText(name);
    }

    fn unifyTagRows(self: *InstGraph, left: NodeId, right: NodeId, pending: *std.ArrayList(NodePair)) Allocator.Error!void {
        const flat_left = try self.flattenTagRow(left);
        const flat_right = try self.flattenTagRow(right);

        var merged = std.ArrayList(InstTag).empty;
        defer merged.deinit(self.allocator);
        var only_left = std.ArrayList(InstTag).empty;
        defer only_left.deinit(self.allocator);
        var only_right = std.ArrayList(InstTag).empty;
        defer only_right.deinit(self.allocator);

        for (flat_left.tags) |left_tag| {
            const wanted = self.tagLabelText(left_tag.name);
            var shared = false;
            for (flat_right.tags) |right_tag| {
                if (!Ident.textEql(wanted, self.tagLabelText(right_tag.name))) continue;
                if (left_tag.payloads.len != right_tag.payloads.len) {
                    Common.invariant("instantiation unified one tag at two different payload arities");
                }
                for (left_tag.payloads, right_tag.payloads) |left_payload, right_payload| {
                    try pending.append(self.allocator, .{ .left = left_payload, .right = right_payload });
                }
                shared = true;
                break;
            }
            try merged.append(self.allocator, left_tag);
            if (!shared) try only_left.append(self.allocator, left_tag);
        }
        for (flat_right.tags) |right_tag| {
            const wanted = self.tagLabelText(right_tag.name);
            var shared = false;
            for (flat_left.tags) |left_tag| {
                if (Ident.textEql(wanted, self.tagLabelText(left_tag.name))) {
                    shared = true;
                    break;
                }
            }
            if (!shared) {
                try merged.append(self.allocator, right_tag);
                try only_right.append(self.allocator, right_tag);
            }
        }

        if (self.rowAdditionConflicts(flat_left.ext, only_right.items.len, .tag_union) or
            self.rowAdditionConflicts(flat_right.ext, only_left.items.len, .tag_union))
        {
            Common.invariant("instantiation widened a closed tag union");
        }

        var merged_ext = flat_left.ext;
        if (only_left.items.len == 0 and only_right.items.len == 0) {
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = flat_right.ext });
        } else if (only_left.items.len == 0) {
            // Left lacks tags: its extension absorbs the right-only tags.
            try self.writeOrQueueTagRest(flat_left.ext, only_right.items, flat_right.ext, pending);
            merged_ext = flat_right.ext;
        } else if (only_right.items.len == 0) {
            try self.writeOrQueueTagRest(flat_right.ext, only_left.items, flat_left.ext, pending);
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
            if (self.find(flat_left.ext) == self.find(flat_right.ext)) {
                var rest = std.ArrayList(InstTag).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_left.items);
                try rest.appendSlice(self.allocator, only_right.items);
                try self.writeOrQueueTagRest(flat_left.ext, rest.items, new_ext, pending);
            } else {
                try self.writeOrQueueTagRest(flat_left.ext, only_right.items, new_ext, pending);
                try self.writeOrQueueTagRest(flat_right.ext, only_left.items, new_ext, pending);
            }
            merged_ext = new_ext;
        }

        try self.setContent(left, .{ .tag_union = .{
            .tags = try self.arena().dupe(InstTag, merged.items),
            .ext = merged_ext,
        } });
        try self.union_(left, right);
    }

    fn writeOrQueueTagRest(
        self: *InstGraph,
        ext: NodeId,
        tags: []const InstTag,
        tail_ext: NodeId,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        const ext_root = self.find(ext);
        const ext_content = self.nodes.items[@intFromEnum(ext_root)];
        if (ext_content == .unresolved) {
            const variable = ext_content.unresolved;
            if (variable.numeric_default_phase != null) {
                Common.invariant("instantiation tried to write a tag row into a numeric variable");
            }
            if (variable.row_default) |default| {
                if (default != .empty_tag_union) {
                    Common.invariant("instantiation tried to write a tag row into a record row variable");
                }
            }
            try self.setContent(ext_root, .{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, tags),
                .ext = tail_ext,
            } });
        } else {
            const rest = try self.newNode(.{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, tags),
                .ext = tail_ext,
            } });
            try pending.append(self.allocator, .{ .left = ext_root, .right = rest });
        }
    }

    fn unifyRecordRows(self: *InstGraph, left: NodeId, right: NodeId, pending: *std.ArrayList(NodePair)) Allocator.Error!void {
        const flat_left = try self.flattenRecordRow(left);
        const flat_right = try self.flattenRecordRow(right);

        var merged = std.ArrayList(InstField).empty;
        defer merged.deinit(self.allocator);
        var only_left = std.ArrayList(InstField).empty;
        defer only_left.deinit(self.allocator);
        var only_right = std.ArrayList(InstField).empty;
        defer only_right.deinit(self.allocator);

        for (flat_left.fields) |left_field| {
            const wanted = self.fieldLabelText(left_field.name);
            var shared = false;
            for (flat_right.fields) |right_field| {
                if (!Ident.textEql(wanted, self.fieldLabelText(right_field.name))) continue;
                try pending.append(self.allocator, .{ .left = left_field.ty, .right = right_field.ty });
                shared = true;
                break;
            }
            try merged.append(self.allocator, left_field);
            if (!shared) try only_left.append(self.allocator, left_field);
        }
        for (flat_right.fields) |right_field| {
            const wanted = self.fieldLabelText(right_field.name);
            var shared = false;
            for (flat_left.fields) |left_field| {
                if (Ident.textEql(wanted, self.fieldLabelText(left_field.name))) {
                    shared = true;
                    break;
                }
            }
            if (!shared) {
                try merged.append(self.allocator, right_field);
                try only_right.append(self.allocator, right_field);
            }
        }

        if (self.rowAdditionConflicts(flat_left.ext, only_right.items.len, .record) or
            self.rowAdditionConflicts(flat_right.ext, only_left.items.len, .record))
        {
            Common.invariant("instantiation widened a closed record");
        }

        var merged_ext = flat_left.ext;
        if (only_left.items.len == 0 and only_right.items.len == 0) {
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = flat_right.ext });
        } else if (only_left.items.len == 0) {
            try self.writeOrQueueRecordRest(flat_left.ext, only_right.items, flat_right.ext, pending);
            merged_ext = flat_right.ext;
        } else if (only_right.items.len == 0) {
            try self.writeOrQueueRecordRest(flat_right.ext, only_left.items, flat_left.ext, pending);
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
            if (self.find(flat_left.ext) == self.find(flat_right.ext)) {
                var rest = std.ArrayList(InstField).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_left.items);
                try rest.appendSlice(self.allocator, only_right.items);
                try self.writeOrQueueRecordRest(flat_left.ext, rest.items, new_ext, pending);
            } else {
                try self.writeOrQueueRecordRest(flat_left.ext, only_right.items, new_ext, pending);
                try self.writeOrQueueRecordRest(flat_right.ext, only_left.items, new_ext, pending);
            }
            merged_ext = new_ext;
        }

        try self.setContent(left, .{ .record = .{
            .fields = try self.arena().dupe(InstField, merged.items),
            .ext = merged_ext,
        } });
        try self.union_(left, right);
    }

    fn writeOrQueueRecordRest(
        self: *InstGraph,
        ext: NodeId,
        fields: []const InstField,
        tail_ext: NodeId,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        const ext_root = self.find(ext);
        const ext_content = self.nodes.items[@intFromEnum(ext_root)];
        if (ext_content == .unresolved) {
            const variable = ext_content.unresolved;
            if (variable.numeric_default_phase != null) {
                Common.invariant("instantiation tried to write a record row into a numeric variable");
            }
            if (variable.row_default) |default| {
                if (default != .empty_record) {
                    Common.invariant("instantiation tried to write a record row into a tag row variable");
                }
            }
            try self.setContent(ext_root, .{ .record = .{
                .fields = try self.arena().dupe(InstField, fields),
                .ext = tail_ext,
            } });
        } else {
            const rest = try self.newNode(.{ .record = .{
                .fields = try self.arena().dupe(InstField, fields),
                .ext = tail_ext,
            } });
            try pending.append(self.allocator, .{ .left = ext_root, .right = rest });
        }
    }

    /// Import a Monotype into the graph. A Monotype already linked to a node
    /// reconnects to it; an unlinked one copies in as closed structure, so a
    /// later attempt to widen it is a unification conflict rather than a silent
    /// mutation of another specialization's final type.
    pub fn importMono(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        return try self.importMonoInner(ty, null);
    }

    fn importGeneratedAuthoritativeMono(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        var seen = collections.DenseMap(Type.TypeId, void).init(self.allocator);
        defer seen.deinit();
        return try self.importMonoInner(ty, &seen);
    }

    fn importMonoInner(
        self: *InstGraph,
        ty: Type.TypeId,
        authoritative_seen: ?*collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!NodeId {
        self.countDiagnostic("mono_import_requests");
        if (authoritative_seen) |seen| {
            const entry = try seen.getOrPut(ty);
            if (entry.found_existing) {
                const existing = self.linked_type_nodes.get(ty) orelse
                    Common.invariant("recursive authoritative Monotype child had not been linked");
                return self.find(existing);
            }
        }
        if (self.linked_type_nodes.get(ty)) |existing| {
            self.countDiagnostic("mono_import_hits");
            if (authoritative_seen) |seen| {
                try self.recordGeneratedAuthoritativeMono(existing, ty);
                try self.visitExistingGeneratedAuthoritativeMonoChildren(ty, seen);
            }
            return self.find(existing);
        }
        self.countDiagnostic("mono_import_misses");
        const node = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        // One-way memo: every import is a finished Monotype from outside this
        // graph (ids materialized here hit the memo above), so it enters as a
        // snapshot. Registering a view would let this specialization's
        // evidence rewrite another specialization's final type, destabilizing
        // every digest taken from it.
        try self.linked_type_nodes.put(ty, node);
        try self.imported_monos.put(node, ty);
        if (authoritative_seen != null) try self.recordGeneratedAuthoritativeMono(node, ty);

        const types = self.types;
        const imported: InstNode = switch (types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.importMonoInner(elem, authoritative_seen) },
            .box => |elem| .{ .box = try self.importMonoInner(elem, authoritative_seen) },
            .tuple => |items| .{ .tuple = try self.importMonoSliceInner(types.span(items), authoritative_seen) },
            .func => |func| .{ .func = .{
                .args = try self.importMonoSliceInner(types.span(func.args), authoritative_seen),
                .ret = try self.importMonoInner(func.ret, authoritative_seen),
            } },
            .tag_union => |tags| blk: {
                const span = types.tagSpan(tags);
                if (span.len == 0) {
                    break :blk .empty_tag_union;
                }
                const inst_tags = try self.arena().alloc(InstTag, span.len);
                for (0..span.len) |index| {
                    const tag = GuardedList.at(span, index);
                    inst_tags[index] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.importMonoSliceInner(types.span(tag.payloads), authoritative_seen),
                    };
                }
                break :blk .{ .tag_union = .{
                    .tags = inst_tags,
                    .ext = try self.newNode(.empty_tag_union),
                } };
            },
            .record => |fields| blk: {
                const span = types.fieldSpan(fields);
                if (span.len == 0) break :blk .empty_record;
                const inst_fields = try self.arena().alloc(InstField, span.len);
                for (0..span.len) |index| {
                    const field = GuardedList.at(span, index);
                    inst_fields[index] = .{
                        .name = field.name,
                        .ty = try self.importMonoInner(field.ty, authoritative_seen),
                    };
                }
                break :blk .{ .record = .{
                    .fields = inst_fields,
                    .ext = try self.newNode(.empty_record),
                } };
            },
            .named => |named| .{ .named = .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try self.importMonoSliceInner(types.span(named.args), authoritative_seen),
                .backing = if (named.backing) |backing| .{
                    .node = try self.importMonoInner(backing.ty, authoritative_seen),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.importDeclaredFieldsInner(named.declared_order, authoritative_seen),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentWithoutSnapshotInvalidation(node, imported);
        switch (types.get(ty)) {
            .named => |named| if (named.def.iterator_representation != .none) {
                if (named.def.generated == null) {
                    Common.invariant("imported generated iterator had no durable identity");
                }
                try self.imported_generated_iterator_nodes.append(self.allocator, node);
            },
            .primitive, .list, .box, .tuple, .func, .tag_union, .record, .erased, .zst => {},
        }
        return node;
    }

    fn recordGeneratedAuthoritativeMono(self: *InstGraph, raw_node: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const entry = try self.generated_authoritative_monos.getOrPut(raw_node);
        if (!entry.found_existing) {
            entry.value_ptr.* = ty;
        } else if (entry.value_ptr.* != ty and
            !try self.types.typeEql(self.name_store, entry.value_ptr.*, ty))
        {
            Common.invariant("one generated authoritative graph node represented different Monotypes");
        }
    }

    fn visitExistingGeneratedAuthoritativeMonoChildren(
        self: *InstGraph,
        ty: Type.TypeId,
        seen: *collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!void {
        switch (self.types.get(ty)) {
            .primitive, .erased, .zst => {},
            .list => |elem| _ = try self.importMonoInner(elem, seen),
            .box => |elem| _ = try self.importMonoInner(elem, seen),
            .tuple => |items| try self.visitExistingGeneratedAuthoritativeMonoSpan(items, seen),
            .func => |func| {
                try self.visitExistingGeneratedAuthoritativeMonoSpan(func.args, seen);
                _ = try self.importMonoInner(func.ret, seen);
            },
            .record => |fields| {
                const field_span = self.types.fieldSpan(fields);
                for (0..field_span.len) |index| {
                    _ = try self.importMonoInner(GuardedList.at(field_span, index).ty, seen);
                }
            },
            .tag_union => |tags| {
                const tag_span = self.types.tagSpan(tags);
                for (0..tag_span.len) |index| {
                    try self.visitExistingGeneratedAuthoritativeMonoSpan(GuardedList.at(tag_span, index).payloads, seen);
                }
            },
            .named => |named| {
                try self.visitExistingGeneratedAuthoritativeMonoSpan(named.args, seen);
                if (named.backing) |backing| {
                    _ = try self.importMonoInner(backing.ty, seen);
                }
                const declared_fields = self.types.declaredFieldSpan(named.declared_order);
                for (0..declared_fields.len) |index| {
                    switch (GuardedList.at(declared_fields, index)) {
                        .named => {},
                        .padding => |padding| _ = try self.importMonoInner(padding, seen),
                    }
                }
            },
        }
    }

    fn visitExistingGeneratedAuthoritativeMonoSpan(
        self: *InstGraph,
        span: Type.Span,
        seen: *collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!void {
        const children = self.types.span(span);
        for (0..children.len) |index| {
            _ = try self.importMonoInner(GuardedList.at(children, index), seen);
        }
    }

    fn importMonoSliceInner(
        self: *InstGraph,
        tys: anytype,
        authoritative_seen: ?*collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (0..tys.len) |index| {
            const ty = GuardedList.at(tys, index);
            out[index] = try self.importMonoInner(ty, authoritative_seen);
        }
        return out;
    }

    fn importDeclaredFieldsInner(
        self: *InstGraph,
        span: Type.Span,
        authoritative_seen: ?*collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error![]const InstDeclaredField {
        const fields = self.types.declaredFieldSpan(span);
        if (fields.len == 0) return &.{};
        const out = try self.arena().alloc(InstDeclaredField, fields.len);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.importMonoInner(ty, authoritative_seen) },
            };
        }
        return out;
    }

    /// Materialize a read-only Monotype-shaped view of a fully resolved graph
    /// node. Open rows and unresolved checked variables have no TypeId view:
    /// callers must continue to use their graph nodes until explicit evidence
    /// closes them. The returned TypeId is graph-owned scratch state and must
    /// not be written to completed Monotype output.
    pub fn activeTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        self.requireRelationProduction();
        self.countDiagnostic("active_type_requests");
        if (self.imported_monos.get(node)) |imported| {
            self.countDiagnostic("active_type_imported_hits");
            return imported;
        }
        if (!try self.typeIsResolved(node)) {
            Common.invariant("active Monotype TypeId requested for an unresolved instantiation graph node");
        }
        return try self.monoFor(node);
    }

    fn monoFor(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        const root = self.find(node);
        if (!try self.typeIsResolved(root)) {
            Common.invariant("immutable Monotype snapshot requested for an unresolved instantiation graph node");
        }
        self.refreshActiveSnapshots();
        if (self.current_snapshots.get(root)) |current| {
            self.countDiagnostic("active_snapshot_cache_hits");
            return current;
        }
        self.countDiagnostic("active_snapshot_cache_misses");

        var snapshot = GraphTypeFinals.initActiveSnapshot(self);
        defer snapshot.deinit();
        const ty = try snapshot.sealNode(root);
        self.countDiagnosticBy("active_snapshot_nodes_materialized", snapshot.sealed.count());

        var materialized = snapshot.sealed.iterator();
        while (materialized.next()) |item| {
            const snapshot_node = self.find(item.key_ptr.*);
            const snapshot_ty = item.value_ptr.*;
            const entry = try self.node_snapshots.getOrPut(snapshot_node);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.append(self.allocator, snapshot_ty);
            try self.linked_type_nodes.put(snapshot_ty, snapshot_node);
            try self.current_snapshots.put(snapshot_node, snapshot_ty);
        }
        return ty;
    }

    /// Materialize a graph node directly into a final TypeId without first
    /// exposing or copying an active Type-shaped snapshot.
    pub fn sealNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        var sealer = GraphTypeFinals.init(self);
        defer sealer.deinit();
        return try sealer.sealNode(node);
    }

    /// Materialize a TypeId into a final copy. If the TypeId is an active
    /// snapshot, seal its current solved node instead of reusing the snapshot.
    pub fn sealType(self: *InstGraph, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        var sealer = GraphTypeFinals.init(self);
        defer sealer.deinit();
        return try sealer.sealType(ty);
    }

    pub fn assertTypeHasNoActiveSnapshots(self: *InstGraph, ty: Type.TypeId) Allocator.Error!void {
        if (try self.typeHasActiveSnapshots(ty)) {
            Common.invariant("Monotype body draft retained an active type snapshot after sealing");
        }
    }

    pub fn typeHasActiveSnapshots(self: *InstGraph, ty: Type.TypeId) Allocator.Error!bool {
        var seen = collections.DenseMap(Type.TypeId, void).init(self.allocator);
        defer seen.deinit();
        return try self.typeContainsActiveSnapshot(ty, &seen);
    }

    fn typeContainsActiveSnapshot(
        self: *InstGraph,
        ty: Type.TypeId,
        seen: *collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!bool {
        if (self.isActiveSnapshotType(ty)) return true;
        const seen_entry = try seen.getOrPut(ty);
        if (seen_entry.found_existing) return false;
        return switch (self.types.get(ty)) {
            .primitive, .erased, .zst => false,
            .list => |elem| try self.typeContainsActiveSnapshot(elem, seen),
            .box => |elem| try self.typeContainsActiveSnapshot(elem, seen),
            .tuple => |items| try self.typeSpanContainsActiveSnapshot(items, seen),
            .func => |func| blk: {
                if (try self.typeSpanContainsActiveSnapshot(func.args, seen)) break :blk true;
                break :blk try self.typeContainsActiveSnapshot(func.ret, seen);
            },
            .record => |fields| blk: {
                const field_span = self.types.fieldSpan(fields);
                for (0..field_span.len) |index| {
                    const field = GuardedList.at(field_span, index);
                    if (try self.typeContainsActiveSnapshot(field.ty, seen)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tags| blk: {
                const tag_span = self.types.tagSpan(tags);
                for (0..tag_span.len) |index| {
                    const tag = GuardedList.at(tag_span, index);
                    if (try self.typeSpanContainsActiveSnapshot(tag.payloads, seen)) break :blk true;
                }
                break :blk false;
            },
            .named => |named| blk: {
                if (try self.typeSpanContainsActiveSnapshot(named.args, seen)) break :blk true;
                if (named.backing) |backing| {
                    if (try self.typeContainsActiveSnapshot(backing.ty, seen)) break :blk true;
                }
                const declared_fields = self.types.declaredFieldSpan(named.declared_order);
                for (0..declared_fields.len) |index| {
                    const field = GuardedList.at(declared_fields, index);
                    switch (field) {
                        .named => {},
                        .padding => |padding| if (try self.typeContainsActiveSnapshot(padding, seen)) break :blk true,
                    }
                }
                break :blk false;
            },
        };
    }

    fn typeSpanContainsActiveSnapshot(
        self: *InstGraph,
        span: Type.Span,
        seen: *collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!bool {
        const children = self.types.span(span);
        for (0..children.len) |index| {
            const child = GuardedList.at(children, index);
            if (try self.typeContainsActiveSnapshot(child, seen)) return true;
        }
        return false;
    }

    fn isGeneratedPrivateRootContent(node_content: InstNode) bool {
        if (node_content != .named) return false;
        return if (node_content.named.backing) |backing|
            backing.authority == .generated_private
        else
            false;
    }

    fn isActiveSnapshotType(self: *InstGraph, ty: Type.TypeId) bool {
        const raw_node = self.linked_type_nodes.get(ty) orelse return false;
        const views = self.node_snapshots.get(raw_node) orelse return false;
        for (views.items) |view| {
            if (view == ty) return true;
        }
        return false;
    }

    /// Return the current root node for a TypeId that is one of this graph's
    /// immutable active snapshots. Closed imported TypeIds return null.
    pub fn activeSnapshotNode(self: *InstGraph, ty: Type.TypeId) ?NodeId {
        const raw_node = self.linked_type_nodes.get(ty) orelse return null;
        const views = self.node_snapshots.get(raw_node) orelse return null;
        for (views.items) |view| {
            if (view == ty) return self.find(raw_node);
        }
        return null;
    }
};

/// Shared finalization state for materializing graph nodes into immutable
/// Monotype type ids.
pub const GraphTypeFinals = struct {
    graph: *InstGraph,
    sealed: collections.DenseMap(NodeId, Type.TypeId),
    sealed_types: collections.DenseMap(Type.TypeId, Type.TypeId),
    generated_types_by_identity: ?*std.AutoHashMap(names.TypeDigest, Type.TypeId),

    pub fn init(graph: *InstGraph) GraphTypeFinals {
        graph.requireFrozenRelations();
        return initUnchecked(graph);
    }

    pub fn initWithGeneratedTypeInterner(
        graph: *InstGraph,
        generated_types_by_identity: *std.AutoHashMap(names.TypeDigest, Type.TypeId),
    ) GraphTypeFinals {
        graph.requireFrozenRelations();
        var finals = initUnchecked(graph);
        finals.generated_types_by_identity = generated_types_by_identity;
        return finals;
    }

    fn initActiveSnapshot(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph);
    }

    fn initUnchecked(graph: *InstGraph) GraphTypeFinals {
        return .{
            .graph = graph,
            .sealed = collections.DenseMap(NodeId, Type.TypeId).init(graph.allocator),
            .sealed_types = collections.DenseMap(Type.TypeId, Type.TypeId).init(graph.allocator),
            .generated_types_by_identity = null,
        };
    }

    pub fn deinit(self: *GraphTypeFinals) void {
        self.sealed_types.deinit();
        self.sealed.deinit();
    }

    pub fn sealType(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (self.graph.linked_type_nodes.get(ty)) |raw_node| {
            if (self.graph.node_snapshots.get(raw_node)) |views| {
                for (views.items) |view| {
                    if (view == ty) return try self.sealNode(raw_node);
                }
            }
        }
        if (try self.typeHasActiveSnapshots(ty)) return try self.sealStoreType(ty);
        return ty;
    }

    pub fn sealNode(self: *GraphTypeFinals, raw_node: NodeId) Allocator.Error!Type.TypeId {
        const node = self.graph.find(raw_node);
        if (self.sealed.get(node)) |existing| return existing;

        const node_content = self.graph.nodes.items[@intFromEnum(node)];
        const generated_identity = switch (node_content) {
            .named => |named| if (named.def.iterator_representation != .none) named.def.generated else null,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => null,
        };
        if (node_content == .named and node_content.named.def.iterator_representation != .none) {
            if (node_content.named.generated_iterator != null and self.generated_types_by_identity == null) {
                Common.invariant("active Monotype snapshot traversed a graph-owned generated iterator");
            }
        }
        if (self.graph.generated_authoritative_monos.get(node)) |authoritative| {
            if (generated_identity) |identity| {
                const interner = self.generated_types_by_identity orelse
                    Common.invariant("generated authoritative root reached sealing without its interner");
                const expected = interner.get(identity) orelse
                    Common.invariant("generated authoritative root was absent from its interner");
                if (expected != authoritative) {
                    Common.invariant("generated authoritative root retained the wrong durable TypeId");
                }
            }
            try self.sealed.put(node, authoritative);
            return authoritative;
        }
        if (generated_identity) |identity| {
            if (self.generated_types_by_identity) |interner| {
                if (interner.contains(identity)) {
                    Common.invariant("generated Monotype identity was not bound before relation freezing");
                }
            }
        }

        const Context = struct {
            sealer: *GraphTypeFinals,
            node: NodeId,

            fn fill(context: @This(), reserved: Type.TypeId) Allocator.Error!Type.Content {
                try context.sealer.sealed.put(context.node, reserved);
                return try context.sealer.sealContent(context.node);
            }
        };
        const sealed = try self.graph.types.addRecursive(Context{ .sealer = self, .node = node }, Context.fill);
        if (generated_identity) |identity| {
            if (self.generated_types_by_identity) |interner| {
                const entry = try interner.getOrPut(identity);
                if (entry.found_existing) {
                    Common.invariant("generated Monotype identity was sealed twice before interning");
                }
                entry.value_ptr.* = sealed;
                self.graph.countDiagnostic("generated_type_store_misses");
            }
        }
        return sealed;
    }

    /// Commit every graph-owned generated identity before sealing work can
    /// recursively lower another specialization. Without this barrier a
    /// nested specialization can commit the same identity first, after this
    /// graph's relations have already frozen and can no longer bind to it.
    pub fn commitGeneratedIteratorRoots(self: *GraphTypeFinals) Allocator.Error!void {
        var seen = collections.DenseMap(NodeId, void).init(self.graph.allocator);
        defer seen.deinit();
        for (self.graph.generated_iterator_nodes.items) |registered| {
            const node = self.graph.find(registered);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            _ = try self.sealNode(node);
        }
    }

    fn sealContent(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.Content {
        return switch (self.graph.nodes.items[@intFromEnum(node)]) {
            .redirect => unreachable,
            .unresolved => |variable| materializeUnresolved(variable),
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.sealNode(elem) },
            .box => |elem| .{ .box = try self.sealNode(elem) },
            .tuple => |items| .{ .tuple = try self.sealNodeSpan(items) },
            .func => |func| .{ .func = .{
                .args = try self.sealNodeSpan(func.args),
                .ret = try self.sealNode(func.ret),
            } },
            .empty_tag_union => .{ .tag_union = Type.Span.empty() },
            .empty_record => .{ .record = Type.Span.empty() },
            .tag_union => .{ .tag_union = try self.sealTagRow(node) },
            .record => .{ .record = try self.sealRecordRow(node) },
            .named => |named| .{ .named = .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try self.sealNodeSpan(named.args),
                .backing = if (named.backing) |raw_backing| backing: {
                    const structural = try self.graph.structuralBackingNode(raw_backing.node, named);
                    break :backing .{
                        .ty = try self.sealNode(structural.node),
                        .use = raw_backing.use,
                        .authority = raw_backing.authority,
                    };
                } else null,
                .declared_order = try self.sealDeclaredFieldSpan(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
    }

    fn typeHasActiveSnapshots(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!bool {
        var seen = collections.DenseMap(Type.TypeId, void).init(self.graph.allocator);
        defer seen.deinit();
        return try self.graph.typeContainsActiveSnapshot(ty, &seen);
    }

    fn sealStoreType(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (self.sealed_types.get(ty)) |existing| return existing;

        const Context = struct {
            sealer: *GraphTypeFinals,
            ty: Type.TypeId,

            fn fill(context: @This(), reserved: Type.TypeId) Allocator.Error!Type.Content {
                try context.sealer.sealed_types.put(context.ty, reserved);
                return try context.sealer.sealStoreContent(context.ty);
            }
        };
        return try self.graph.types.addRecursive(Context{ .sealer = self, .ty = ty }, Context.fill);
    }

    fn sealStoreContent(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.Content {
        return switch (self.graph.types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.sealType(elem) },
            .box => |elem| .{ .box = try self.sealType(elem) },
            .tuple => |items| .{ .tuple = try self.sealTypeSpan(items) },
            .func => |func| .{ .func = .{
                .args = try self.sealTypeSpan(func.args),
                .ret = try self.sealType(func.ret),
            } },
            .tag_union => |tags| .{ .tag_union = try self.sealStoredTagSpan(tags) },
            .record => |fields| .{ .record = try self.sealStoredFieldSpan(fields) },
            .named => |named| .{ .named = .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try self.sealTypeSpan(named.args),
                .backing = if (named.backing) |backing| .{
                    .ty = try self.sealType(backing.ty),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.sealStoredDeclaredFieldSpan(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
    }

    fn sealNodeSpan(self: *GraphTypeFinals, nodes: []const NodeId) Allocator.Error!Type.Span {
        if (nodes.len == 0) return .empty();
        const sealed_nodes = try self.graph.allocator.alloc(Type.TypeId, nodes.len);
        defer self.graph.allocator.free(sealed_nodes);
        for (nodes, 0..) |node, index| {
            sealed_nodes[index] = try self.sealNode(node);
        }
        return try self.graph.types.addSpan(sealed_nodes);
    }

    fn sealTypeSpan(self: *GraphTypeFinals, span: Type.Span) Allocator.Error!Type.Span {
        const sealed = try GuardedList.dupe(self.graph.allocator, Type.TypeId, self.graph.types.span(span));
        defer self.graph.allocator.free(sealed);
        if (sealed.len == 0) return .empty();
        for (sealed) |*ty| {
            ty.* = try self.sealType(ty.*);
        }
        return try self.graph.types.addSpan(sealed);
    }

    fn sealRecordRow(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.Span {
        const flat = try self.graph.flattenRecordRow(node);
        if (flat.fields.len == 0) return .empty();
        const fields = try self.graph.allocator.alloc(Type.Field, flat.fields.len);
        defer self.graph.allocator.free(fields);
        for (flat.fields, 0..) |field, index| {
            fields[index] = .{
                .name = field.name,
                .ty = try self.sealNode(field.ty),
            };
        }
        return try self.graph.types.addRecordFields(self.graph.name_store, fields);
    }

    fn sealStoredFieldSpan(self: *GraphTypeFinals, span: Type.Span) Allocator.Error!Type.Span {
        const fields = try GuardedList.dupe(self.graph.allocator, Type.Field, self.graph.types.fieldSpan(span));
        defer self.graph.allocator.free(fields);
        if (fields.len == 0) return .empty();
        for (fields) |*field| {
            field.ty = try self.sealType(field.ty);
        }
        return try self.graph.types.addRecordFields(self.graph.name_store, fields);
    }

    fn sealTagRow(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.Span {
        const flat = try self.graph.flattenTagRow(node);
        if (flat.tags.len == 0) return .empty();
        const tags = try self.graph.allocator.alloc(Type.Tag, flat.tags.len);
        defer self.graph.allocator.free(tags);
        for (flat.tags, 0..) |tag, index| {
            tags[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.sealNodeSpan(tag.payloads),
            };
        }
        return try self.graph.types.addTagVariants(self.graph.name_store, tags);
    }

    fn sealStoredTagSpan(self: *GraphTypeFinals, span: Type.Span) Allocator.Error!Type.Span {
        const tags = try GuardedList.dupe(self.graph.allocator, Type.Tag, self.graph.types.tagSpan(span));
        defer self.graph.allocator.free(tags);
        if (tags.len == 0) return .empty();
        for (tags) |*tag| {
            tag.payloads = try self.sealTypeSpan(tag.payloads);
        }
        return try self.graph.types.addTagVariants(self.graph.name_store, tags);
    }

    fn sealDeclaredFieldSpan(self: *GraphTypeFinals, fields: []const InstDeclaredField) Allocator.Error!Type.Span {
        if (fields.len == 0) return .empty();
        const sealed = try self.graph.allocator.alloc(Type.DeclaredField, fields.len);
        defer self.graph.allocator.free(sealed);
        for (fields, 0..) |field, index| {
            sealed[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |node| .{ .padding = try self.sealNode(node) },
            };
        }
        return try self.graph.types.addDeclaredFields(sealed);
    }

    fn sealStoredDeclaredFieldSpan(self: *GraphTypeFinals, span: Type.Span) Allocator.Error!Type.Span {
        const sealed = try GuardedList.dupe(self.graph.allocator, Type.DeclaredField, self.graph.types.declaredFieldSpan(span));
        defer self.graph.allocator.free(sealed);
        if (sealed.len == 0) return .empty();
        for (sealed) |*field| {
            switch (field.*) {
                .named => {},
                .padding => |ty| field.* = .{ .padding = try self.sealType(ty) },
            }
        }
        return try self.graph.types.addDeclaredFields(sealed);
    }
};

fn instIteratorOwnerPair(
    left: ?static_dispatch.BuiltinOwner,
    right: ?static_dispatch.BuiltinOwner,
) bool {
    const owner = left orelse right orelse return false;
    if (!static_dispatch.isIteratorOwner(owner)) return false;
    if (left) |left_owner| if (left_owner != owner) return false;
    if (right) |right_owner| if (right_owner != owner) return false;
    return true;
}

fn optionalInstDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left) |left_digest| {
        const right_digest = right orelse return false;
        return std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes);
    }
    return right == null;
}

fn updateGeneratedIteratorInternU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

/// Computes each graph-owned generated iterator identity once. A generated
/// iterator nested in another identity input is represented by this digest,
/// so later producers do not re-walk the nested producer's complete graph.
const GeneratedIteratorIdentityFinalizer = struct {
    graph: *InstGraph,
    digests: collections.DenseMap(NodeId, names.TypeDigest),
    active: collections.DenseMap(NodeId, void),
    inputs_resolved: bool = true,

    fn init(graph: *InstGraph) GeneratedIteratorIdentityFinalizer {
        return .{
            .graph = graph,
            .digests = collections.DenseMap(NodeId, names.TypeDigest).init(graph.allocator),
            .active = collections.DenseMap(NodeId, void).init(graph.allocator),
        };
    }

    fn deinit(self: *GeneratedIteratorIdentityFinalizer) void {
        self.active.deinit();
        self.digests.deinit();
    }

    fn digestFor(
        self: *GeneratedIteratorIdentityFinalizer,
        raw_node: NodeId,
    ) Allocator.Error!names.TypeDigest {
        const node = self.graph.find(raw_node);
        if (self.digests.get(node)) |digest| {
            self.graph.countDiagnostic("generated_identity_cache_hits");
            return digest;
        }
        if (self.active.contains(node)) {
            Common.invariant("generated iterator durable identity contained a recursive producer dependency");
        }
        try self.active.put(node, {});
        defer _ = self.active.remove(node);

        const named = switch (self.graph.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator identity target was not named"),
        };
        const provenance = named.generated_iterator orelse
            Common.invariant("graph-owned generated iterator identity lacked producer provenance");
        if (named.args.len != 1) {
            Common.invariant("generated iterator identity did not have exactly one item argument");
        }

        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        const item_digest = try self.graph.generatedIteratorIdentityInputDigest(self, named.args[0]);
        hasher.update("public_source");
        hasher.update(self.graph.name_store.moduleIdentityBytes(provenance.public_source.def.module));
        if (provenance.public_source.def.source_decl) |source_decl| {
            hasher.update(&.{1});
            updateGeneratedIteratorInternU32(&hasher, source_decl);
        } else {
            hasher.update(&.{0});
            hasher.update(self.graph.name_store.typeNameText(provenance.public_source.def.type_name));
        }
        if (named.def.iterator_representation == .forced_dynamic) {
            if (provenance.components.len != 0 or provenance.callable_evidence != null) {
                Common.invariant("forced-dynamic iterator retained minted producer identity inputs");
            }
            hasher.update("roc.generated_iterator.forced_dynamic_identity.v2");
            hasher.update(&item_digest.bytes);
        } else {
            hasher.update("roc.generated_iterator.producer_identity.v3");
            hasher.update(&.{@intFromEnum(named.def.iterator_kind)});
            hasher.update(&item_digest.bytes);
            updateGeneratedIteratorInternU32(&hasher, @intCast(provenance.components.len));
            if (named.def.iterator_kind == .join) {
                if (provenance.components.len != 2) {
                    Common.invariant("generated iterator join identity did not retain two exact inputs");
                }
                var component_digests = [2]names.TypeDigest{
                    try self.graph.generatedIteratorIdentityInputDigest(self, provenance.components[0]),
                    try self.graph.generatedIteratorIdentityInputDigest(self, provenance.components[1]),
                };
                if (std.mem.order(u8, &component_digests[0].bytes, &component_digests[1].bytes) == .gt) {
                    std.mem.swap(names.TypeDigest, &component_digests[0], &component_digests[1]);
                }
                for (component_digests) |component_digest| hasher.update(&component_digest.bytes);
            } else {
                for (provenance.components) |component| {
                    const component_digest = try self.graph.generatedIteratorIdentityInputDigest(self, component);
                    hasher.update(&component_digest.bytes);
                }
            }
            if (provenance.callable_evidence) |evidence| {
                hasher.update("callable_evidence");
                hasher.update(&evidence.bytes);
            }
        }
        const digest: names.TypeDigest = .{ .bytes = hasher.finalResult() };
        try self.digests.put(node, digest);
        return digest;
    }
};

const OpenFunctionInterfaceShapeWriter = struct {
    const Mode = enum {
        open_interface,
        generated_lookup,
        generated_identity,
    };

    graph: *InstGraph,
    hasher: std.crypto.hash.sha2.Sha256,
    unresolved_ids: collections.DenseMap(NodeId, u32),
    generated_ids: collections.DenseMap(NodeId, u32),
    recursive_value_slot_classes: collections.DenseMap(NodeId, bool),
    forced_dynamic_iterator_classes: collections.DenseMap(NodeId, bool),
    visiting: std.ArrayList(NodeId),
    mode: Mode,
    generated_identity_finalizer: ?*GeneratedIteratorIdentityFinalizer = null,
    next_unresolved: u32 = 0,
    next_generated: u32 = 0,
    output: ?[]u8 = null,
    output_len: usize = 0,
    primary_resolved: bool = true,

    fn init(graph: *InstGraph) OpenFunctionInterfaceShapeWriter {
        return .{
            .graph = graph,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .unresolved_ids = collections.DenseMap(NodeId, u32).init(graph.allocator),
            .generated_ids = collections.DenseMap(NodeId, u32).init(graph.allocator),
            .recursive_value_slot_classes = collections.DenseMap(NodeId, bool).init(graph.allocator),
            .forced_dynamic_iterator_classes = collections.DenseMap(NodeId, bool).init(graph.allocator),
            .visiting = .empty,
            .mode = .open_interface,
        };
    }

    fn initGeneratedIdentity(
        graph: *InstGraph,
        finalizer: *GeneratedIteratorIdentityFinalizer,
    ) OpenFunctionInterfaceShapeWriter {
        var writer = init(graph);
        writer.mode = .generated_identity;
        writer.generated_identity_finalizer = finalizer;
        return writer;
    }

    fn initGeneratedLookup(graph: *InstGraph) OpenFunctionInterfaceShapeWriter {
        var writer = init(graph);
        writer.mode = .generated_lookup;
        return writer;
    }

    fn initWithOutput(graph: *InstGraph, output: []u8) OpenFunctionInterfaceShapeWriter {
        var writer = init(graph);
        writer.output = output;
        return writer;
    }

    fn deinit(self: *OpenFunctionInterfaceShapeWriter) void {
        self.visiting.deinit(self.graph.allocator);
        self.forced_dynamic_iterator_classes.deinit();
        self.recursive_value_slot_classes.deinit();
        self.generated_ids.deinit();
        self.unresolved_ids.deinit();
    }

    fn writeFunctionInterface(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) Allocator.Error!void {
        self.writeBytes("roc.monotype.open_function_interface_shape.v4");
        try self.writeFunctionNodes(try self.graph.functionNodes(node));
        self.primary_resolved = self.next_unresolved == 0 and
            (if (self.generated_identity_finalizer) |finalizer| finalizer.inputs_resolved else true);
        if (self.graph.requestCheckedSource(node)) |source| {
            self.writeBytes("source-interface");
            try self.writeFunctionNodes(try self.graph.functionNodes(source));
        } else {
            self.writeBytes("no-source-interface");
        }
    }

    fn writeFunctionNodes(self: *OpenFunctionInterfaceShapeWriter, function: FunctionNodes) Allocator.Error!void {
        self.writeU32(@intCast(function.args.len));
        for (function.args) |arg| try self.writeNode(arg);
        try self.writeNode(function.ret);
    }

    fn writeNode(self: *OpenFunctionInterfaceShapeWriter, raw_node: NodeId) Allocator.Error!void {
        const node = self.graph.find(raw_node);
        const content = self.graph.nodes.items[@intFromEnum(node)];
        if (self.mode == .open_interface and content == .named) {
            const named = content.named;
            const generated_digest = if (named.generated_iterator != null)
                try (self.generated_identity_finalizer orelse
                    Common.invariant("open generated nominal shape lacked an identity finalizer")).digestFor(node)
            else
                InstGraph.sealedGeneratedIteratorDigest(named);
            if (generated_digest) |digest| {
                self.writeU8(if (try self.hasRecursiveValueSlot(node)) 1 else 0);
                self.writeU8(if (try self.hasForcedDynamicIteratorRoot(node)) 1 else 0);
                self.writeBytes("generated-iterator-nominal");
                self.writeBytes(&digest.bytes);
                return;
            }
        }
        if (self.mode == .generated_lookup and
            content == .named and
            content.named.generated_iterator != null)
        {
            const entry = try self.generated_ids.getOrPut(node);
            if (entry.found_existing) {
                self.writeBytes("generated-node-ref");
                self.writeU32(entry.value_ptr.*);
                return;
            }
            entry.value_ptr.* = self.next_generated;
            self.next_generated += 1;
            self.writeBytes("generated-node-def");
            self.writeU32(entry.value_ptr.*);
        }
        if (self.mode == .generated_lookup and content == .named) {
            const named = content.named;
            if (named.generated_iterator) |generated| {
                self.writeBytes("generated_iterator_lookup_identity");
                self.writeBytes(self.graph.name_store.moduleIdentityBytes(generated.public_source.def.module));
                self.writeOptionalU32(generated.public_source.def.source_decl);
                if (generated.public_source.def.source_decl == null) {
                    self.writeBytes(self.graph.name_store.typeNameText(generated.public_source.def.type_name));
                }
                self.writeBytes(@tagName(named.def.iterator_representation));
                self.writeBytes(@tagName(named.def.iterator_kind));
                if (named.args.len != 1) {
                    Common.invariant("generated iterator lookup identity did not have exactly one item argument");
                }
                try self.writeNode(named.args[0]);
                try self.writeNodeSpan(generated.components);
                self.writeOptionalDigest(generated.callable_evidence);
                return;
            }
            if (InstGraph.sealedGeneratedIteratorDigest(named)) |digest| {
                self.writeBytes("sealed_generated_iterator_lookup_identity");
                self.writeBytes(&digest.bytes);
                return;
            }
        }
        if (self.mode == .generated_identity) {
            self.graph.countDiagnostic("generated_identity_nodes_hashed");
            if (content == .named) {
                const named = content.named;
                const generated_digest = if (named.generated_iterator != null)
                    try (self.generated_identity_finalizer orelse
                        Common.invariant("generated identity writer lacked its finalizer")).digestFor(node)
                else if (named.def.generated) |digest|
                    if (named.def.iterator_representation != .none and
                        (named.builtin_owner != null and static_dispatch.isIteratorOwner(named.builtin_owner.?)))
                        digest
                    else
                        null
                else
                    null;
                if (generated_digest) |digest| {
                    self.writeBytes("generated_iterator_identity");
                    self.writeBytes(&digest.bytes);
                    return;
                }
            }
        }
        if (self.mode == .open_interface) {
            self.writeU8(if (try self.hasRecursiveValueSlot(node)) 1 else 0);
            self.writeU8(if (try self.hasForcedDynamicIteratorRoot(node)) 1 else 0);
        }
        if (content == .redirect) unreachable;
        if (content == .unresolved) {
            if (self.mode == .generated_identity) {
                (self.generated_identity_finalizer orelse
                    Common.invariant("generated identity writer lacked its finalizer")).inputs_resolved = false;
                self.writeMaterializedVariable(content.unresolved);
                return;
            }
            if (self.mode == .generated_lookup) {
                self.writeBytes("unresolved-graph-node");
                self.writeU32(@intFromEnum(node));
                self.writeVariable(content.unresolved);
                return;
            }
            const entry = try self.unresolved_ids.getOrPut(node);
            if (!entry.found_existing) {
                entry.value_ptr.* = self.next_unresolved;
                self.next_unresolved += 1;
                self.writeBytes("unresolved-new");
                self.writeU32(entry.value_ptr.*);
                self.writeVariable(content.unresolved);
            } else {
                self.writeBytes("unresolved-ref");
                self.writeU32(entry.value_ptr.*);
            }
            return;
        }

        for (self.visiting.items, 0..) |open_node, position| {
            if (open_node == node) {
                self.writeBytes("cycle");
                self.writeU32(@intCast(position));
                return;
            }
        }
        try self.visiting.append(self.graph.allocator, node);
        defer _ = self.visiting.pop();

        switch (content) {
            .redirect, .unresolved => unreachable,
            .primitive => |primitive| {
                self.writeBytes("primitive");
                self.writeBytes(@tagName(primitive));
            },
            .list => |elem| {
                self.writeBytes("list");
                try self.writeNode(elem);
            },
            .box => |elem| {
                self.writeBytes("box");
                try self.writeNode(elem);
            },
            .tuple => |items| {
                self.writeBytes("tuple");
                try self.writeNodeSpan(items);
            },
            .func => |function| {
                self.writeBytes("func");
                try self.writeNodeSpan(function.args);
                try self.writeNode(function.ret);
            },
            .tag_union => |row| {
                self.writeBytes("tag_union");
                self.writeU32(@intCast(row.tags.len));
                for (row.tags) |tag| {
                    self.writeBytes(self.graph.name_store.tagLabelText(tag.name));
                    self.writeBytes(self.graph.name_store.tagLabelText(tag.checked_name));
                    try self.writeNodeSpan(tag.payloads);
                }
                try self.writeNode(row.ext);
            },
            .record => |row| {
                self.writeBytes("record");
                self.writeU32(@intCast(row.fields.len));
                for (row.fields) |field| {
                    self.writeBytes(self.graph.name_store.recordFieldLabelText(field.name));
                    try self.writeNode(field.ty);
                }
                try self.writeNode(row.ext);
            },
            .empty_tag_union => self.writeBytes("empty_tag_union"),
            .empty_record => self.writeBytes("empty_record"),
            .named => |named| {
                if (named.kind == .alias) {
                    const backing = named.backing orelse {
                        self.writeBytes("alias-without-backing");
                        return;
                    };
                    try self.writeNode(backing.node);
                    return;
                }

                self.writeBytes("named");
                self.writeBytes(&named.named_type.module.bytes);
                self.writeTypeDef(named.def);
                self.writeBytes(@tagName(named.kind));
                self.writeOptionalBuiltinOwner(named.builtin_owner);
                try self.writeNodeSpan(named.args);
                try self.writeOptionalBacking(named.backing);
                try self.writeDeclaredFieldSpan(named.declared_order);
                try self.writeOptionalGeneratedIterator(named.generated_iterator);
            },
            .erased => |digest| {
                self.writeBytes("erased");
                self.writeBytes(&digest.bytes);
            },
            .zst => self.writeBytes("zst"),
        }
    }

    fn hasRecursiveValueSlot(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) Allocator.Error!bool {
        return try self.classContainsMarkedNodeCached(
            node,
            &self.graph.recursive_argument_slots,
            &self.recursive_value_slot_classes,
        );
    }

    fn hasForcedDynamicIteratorRoot(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) Allocator.Error!bool {
        return try self.classContainsMarkedNodeCached(
            node,
            &self.graph.forced_dynamic_iterator_roots,
            &self.forced_dynamic_iterator_classes,
        );
    }

    fn classContainsMarkedNodeCached(
        self: *OpenFunctionInterfaceShapeWriter,
        raw_node: NodeId,
        marked: *const collections.DenseMap(NodeId, void),
        cache: *collections.DenseMap(NodeId, bool),
    ) Allocator.Error!bool {
        const node = self.graph.find(raw_node);
        const entry = try cache.getOrPut(node);
        if (entry.found_existing) return entry.value_ptr.*;
        entry.value_ptr.* = self.graph.classContainsMarkedNode(node, marked);
        return entry.value_ptr.*;
    }

    fn writeNodeSpan(self: *OpenFunctionInterfaceShapeWriter, nodes: []const NodeId) Allocator.Error!void {
        self.writeU32(@intCast(nodes.len));
        for (nodes) |node| try self.writeNode(node);
    }

    fn writeVariable(self: *OpenFunctionInterfaceShapeWriter, variable: InstVariable) void {
        self.writeBytes(@tagName(variable.origin));
        self.writeOptionalNumericDefaultPhase(variable.numeric_default_phase);
        self.writeOptionalRowDefault(variable.row_default);
    }

    fn writeMaterializedVariable(self: *OpenFunctionInterfaceShapeWriter, variable: InstVariable) void {
        if (variable.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                Common.invariant("checking-finalized numeric variable reached generated iterator identity");
            self.writeBytes("primitive");
            self.writeBytes(switch (target) {
                .dec => @tagName(Type.Primitive.dec),
                .str => @tagName(Type.Primitive.str),
            });
            return;
        }
        if (variable.row_default) |row_default| {
            self.writeBytes(switch (row_default) {
                .empty_record => "empty_record",
                .empty_tag_union => "empty_tag_union",
            });
            return;
        }
        switch (variable.origin) {
            .checked_variable => self.writeBytes("empty_tag_union"),
            .row_extension => Common.invariant("row extension reached generated iterator identity without row default"),
            .placeholder => Common.invariant("instantiation placeholder reached generated iterator identity"),
        }
    }

    fn writeTypeDef(self: *OpenFunctionInterfaceShapeWriter, def: Type.TypeDef) void {
        self.writeBytes(self.graph.name_store.moduleIdentityBytes(def.module));
        self.writeOptionalU32(def.source_decl);
        if (def.source_decl == null) {
            self.writeBytes(self.graph.name_store.typeNameText(def.type_name));
        }
        self.writeOptionalDigest(def.generated);
        self.writeBytes(@tagName(def.iterator_representation));
        self.writeBytes(@tagName(def.iterator_kind));
        self.writeU8(def.iterator_depth);
        self.writeOptionalIteratorTopology(def.iterator_topology);
    }

    fn writeOptionalBacking(self: *OpenFunctionInterfaceShapeWriter, backing: ?InstBacking) Allocator.Error!void {
        if (backing) |actual| {
            self.writeU8(1);
            try self.writeBacking(actual);
        } else {
            self.writeU8(0);
        }
    }

    fn writeBacking(self: *OpenFunctionInterfaceShapeWriter, backing: InstBacking) Allocator.Error!void {
        self.writeBytes(@tagName(backing.use));
        self.writeBytes(@tagName(backing.authority));
        try self.writeNode(backing.node);
    }

    fn writeDeclaredFieldSpan(
        self: *OpenFunctionInterfaceShapeWriter,
        declared_order: []const InstDeclaredField,
    ) Allocator.Error!void {
        self.writeU32(@intCast(declared_order.len));
        for (declared_order) |entry| {
            switch (entry) {
                .named => |field_name| {
                    self.writeBytes("named");
                    self.writeBytes(self.graph.name_store.recordFieldLabelText(field_name));
                },
                .padding => |padding| {
                    self.writeBytes("padding");
                    try self.writeNode(padding);
                },
            }
        }
    }

    fn writeOptionalGeneratedIterator(
        self: *OpenFunctionInterfaceShapeWriter,
        generated_iterator: ?InstGeneratedIterator,
    ) Allocator.Error!void {
        const generated = generated_iterator orelse {
            self.writeU8(0);
            return;
        };
        self.writeU8(1);
        self.writeOptionalDigest(generated.callable_evidence);
        try self.writeNodeSpan(generated.components);
        self.writeBytes(&generated.public_source.named_type.module.bytes);
        self.writeTypeDef(generated.public_source.def);
        self.writeBytes(@tagName(generated.public_source.kind));
        self.writeBytes(@tagName(generated.public_source.builtin_owner));
        try self.writeBacking(generated.public_source.backing);
        try self.writeDeclaredFieldSpan(generated.public_source.declared_order);
    }

    fn writeOptionalIteratorTopology(
        self: *OpenFunctionInterfaceShapeWriter,
        topology: ?Type.IteratorTopology,
    ) void {
        const value = topology orelse {
            self.writeU8(0);
            return;
        };
        self.writeU8(1);
        self.writeBytes(self.graph.name_store.recordFieldLabelText(value.len_field));
        self.writeBytes(self.graph.name_store.recordFieldLabelText(value.step_field));
        self.writeBytes(self.graph.name_store.tagLabelText(value.known_tag));
        self.writeBytes(self.graph.name_store.tagLabelText(value.unknown_tag));
        self.writeBytes(self.graph.name_store.tagLabelText(value.done_tag));
        self.writeBytes(self.graph.name_store.tagLabelText(value.one_tag));
        self.writeBytes(self.graph.name_store.tagLabelText(value.skip_tag));
        self.writeBytes(self.graph.name_store.recordFieldLabelText(value.item_field));
        self.writeBytes(self.graph.name_store.recordFieldLabelText(value.rest_field));
    }

    fn writeOptionalBuiltinOwner(
        self: *OpenFunctionInterfaceShapeWriter,
        owner: ?static_dispatch.BuiltinOwner,
    ) void {
        if (owner) |actual| {
            self.writeU8(1);
            self.writeBytes(@tagName(actual));
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalNumericDefaultPhase(
        self: *OpenFunctionInterfaceShapeWriter,
        phase: ?checked.NumericDefaultPhase,
    ) void {
        if (phase) |actual| {
            self.writeU8(1);
            self.writeBytes(@tagName(actual));
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalRowDefault(
        self: *OpenFunctionInterfaceShapeWriter,
        row_default: ?checked.RowDefault,
    ) void {
        if (row_default) |actual| {
            self.writeU8(1);
            self.writeBytes(@tagName(actual));
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalDigest(self: *OpenFunctionInterfaceShapeWriter, digest: ?names.TypeDigest) void {
        if (digest) |actual| {
            self.writeU8(1);
            self.writeBytes(&actual.bytes);
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalU32(self: *OpenFunctionInterfaceShapeWriter, value: ?u32) void {
        if (value) |actual| {
            self.writeU8(1);
            self.writeU32(actual);
        } else {
            self.writeU8(0);
        }
    }

    fn writeBytes(self: *OpenFunctionInterfaceShapeWriter, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.writeRawBytes(bytes);
    }

    fn writeU8(self: *OpenFunctionInterfaceShapeWriter, value: u8) void {
        self.writeRawBytes(&.{value});
    }

    fn writeU32(self: *OpenFunctionInterfaceShapeWriter, value: u32) void {
        var little = std.mem.nativeToLittle(u32, value);
        self.writeRawBytes(std.mem.asBytes(&little));
    }

    fn writeRawBytes(self: *OpenFunctionInterfaceShapeWriter, bytes: []const u8) void {
        self.hasher.update(bytes);
        if (self.output) |output| {
            if (self.output_len > output.len or bytes.len > output.len - self.output_len) {
                Common.invariant("open function-interface shape exceeded its measured byte count");
            }
            @memcpy(output[self.output_len..][0..bytes.len], bytes);
        }
        self.output_len += bytes.len;
    }
};

fn materializeUnresolved(variable: InstVariable) Type.Content {
    if (variable.numeric_default_phase) |phase| {
        const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
            Common.invariant("checking-finalized numeric variable reached Monotype unresolved");
        return switch (target) {
            .dec => .{ .primitive = .dec },
            .str => .{ .primitive = .str },
        };
    }
    if (variable.row_default) |row_default| switch (row_default) {
        .empty_record => return .{ .record = Type.Span.empty() },
        .empty_tag_union => return .{ .tag_union = Type.Span.empty() },
    };
    return switch (variable.origin) {
        .checked_variable => .{ .tag_union = Type.Span.empty() },
        .row_extension => Common.invariant("row extension reached Monotype materialization without row default"),
        .placeholder => Common.invariant("instantiation placeholder reached Monotype materialization"),
    };
}

/// Orders record fields by label text for layout-stable sorting.
pub fn recordFieldLessThan(name_store: *const names.NameStore, lhs: Type.Field, rhs: Type.Field) bool {
    return name_store.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

/// Orders tag union tags by label text for layout-stable sorting.
pub fn tagLessThan(name_store: *const names.NameStore, lhs: Type.Tag, rhs: Type.Tag) bool {
    return name_store.tagLabelTextLessThan(lhs.name, rhs.name);
}

fn tagPayloadCount(tags: []const InstTag) usize {
    var count: usize = 0;
    for (tags) |tag| count += tag.payloads.len;
    return count;
}

fn tagPayloadAt(tags: []const InstTag, raw_index: usize) NodeId {
    var index = raw_index;
    for (tags) |tag| {
        if (index < tag.payloads.len) return tag.payloads[index];
        index -= tag.payloads.len;
    }
    Common.invariant("generated iterator depth tag payload index was out of bounds");
}

/// Panics with the given message if a sorted field list contains a repeated label.
pub fn assertNoDuplicateRecordFields(name_store: *const names.NameStore, fields: []const Type.Field, comptime message: []const u8) void {
    if (fields.len < 2) return;
    for (fields[1..], 1..) |field, i| {
        if (name_store.recordFieldLabelTextEql(fields[i - 1].name, field.name)) {
            Common.invariant(message);
        }
    }
}

/// Panics with the given message if a sorted tag list contains a repeated label.
pub fn assertNoDuplicateTags(name_store: *const names.NameStore, tags: []const Type.Tag, comptime message: []const u8) void {
    if (tags.len < 2) return;
    for (tags[1..], 1..) |tag, i| {
        if (name_store.tagLabelTextEql(tags[i - 1].name, tag.name)) {
            Common.invariant(message);
        }
    }
}

fn instNodeEql(left: InstNode, right: InstNode) bool {
    return switch (left) {
        .redirect => |left_next| right == .redirect and left_next == right.redirect,
        .unresolved => |left_var| right == .unresolved and std.meta.eql(left_var, right.unresolved),
        .primitive => |left_primitive| right == .primitive and left_primitive == right.primitive,
        .list => |left_elem| right == .list and left_elem == right.list,
        .box => |left_elem| right == .box and left_elem == right.box,
        .tuple => |left_items| right == .tuple and nodeSliceEql(left_items, right.tuple),
        .func => |left_fn| right == .func and nodeSliceEql(left_fn.args, right.func.args) and left_fn.ret == right.func.ret,
        .tag_union => |left_row| right == .tag_union and left_row.ext == right.tag_union.ext and instTagSliceEql(left_row.tags, right.tag_union.tags),
        .record => |left_row| right == .record and left_row.ext == right.record.ext and instFieldSliceEql(left_row.fields, right.record.fields),
        .empty_tag_union => right == .empty_tag_union,
        .empty_record => right == .empty_record,
        .named => |left_named| right == .named and instNamedEql(left_named, right.named),
        .erased => |left_digest| right == .erased and std.mem.eql(u8, left_digest.bytes[0..], right.erased.bytes[0..]),
        .zst => right == .zst,
    };
}

fn nodeSliceEql(left: []const NodeId, right: []const NodeId) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_node, right_node| {
        if (left_node != right_node) return false;
    }
    return true;
}

fn instTagSliceEql(left: []const InstTag, right: []const InstTag) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_tag, right_tag| {
        if (left_tag.name != right_tag.name or left_tag.checked_name != right_tag.checked_name) return false;
        if (!nodeSliceEql(left_tag.payloads, right_tag.payloads)) return false;
    }
    return true;
}

fn instFieldSliceEql(left: []const InstField, right: []const InstField) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_field, right_field| {
        if (left_field.name != right_field.name or left_field.ty != right_field.ty) return false;
    }
    return true;
}

fn instNamedEql(left: InstNamed, right: InstNamed) bool {
    return std.meta.eql(left.named_type, right.named_type) and
        std.meta.eql(left.def, right.def) and
        left.kind == right.kind and
        std.meta.eql(left.builtin_owner, right.builtin_owner) and
        nodeSliceEql(left.args, right.args) and
        backingEql(left.backing, right.backing) and
        instDeclaredFieldSliceEql(left.declared_order, right.declared_order);
}

fn instDeclaredFieldSliceEql(left: []const InstDeclaredField, right: []const InstDeclaredField) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_field, right_field| {
        switch (left_field) {
            .named => |left_name| switch (right_field) {
                .named => |right_name| if (left_name != right_name) return false,
                .padding => return false,
            },
            .padding => |left_node| switch (right_field) {
                .named => return false,
                .padding => |right_node| if (left_node != right_node) return false,
            },
        }
    }
    return true;
}

fn backingEql(left: ?InstBacking, right: ?InstBacking) bool {
    if (left) |left_backing| {
        const right_backing = right orelse return false;
        return left_backing.node == right_backing.node and
            left_backing.use == right_backing.use and
            left_backing.authority == right_backing.authority;
    }
    return right == null;
}

fn testCheckedTypeId(comptime value: u32) checked.CheckedTypeId {
    comptime std.debug.assert(value != 0);
    return @enumFromInt(value);
}

test "monotype solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "graph diagnostics count authoritative operations" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    const boolean = try graph.newNode(.{ .primitive = .bool });
    _ = try graph.activeTypeViewForNode(boolean);
    _ = try graph.activeTypeViewForNode(boolean);

    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const str = try graph.newNode(.{ .primitive = .str });
    try graph.unify(unresolved, str);

    try std.testing.expectEqual(@as(u64, 3), diagnostics.nodes_created);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.unify_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.class_unions);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.active_type_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_cache_hits);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_cache_misses);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_nodes_materialized);
    try std.testing.expect(diagnostics.active_snapshot_invalidations >= 1);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_entries_invalidated);
}

test "completed monotype program view does not expose instantiation graph nodes" {
    @setEvalBranchQuota(10_000);
    comptime assertNoNodeId(Ast.ProgramView, "Ast.ProgramView");
}

test "resolved graph type detection does not default open cells" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const open_list = try graph.newNode(.{ .list = unresolved });
    try std.testing.expect(!try graph.typeIsResolved(open_list));
    try std.testing.expect(try graph.typeCanSealFromExplicitEvidence(open_list));

    const unmarked = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    try std.testing.expect(try graph.typeCanSealFromExplicitEvidence(unmarked));
    const placeholder = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try std.testing.expect(!try graph.typeCanSealFromExplicitEvidence(placeholder));

    const str = try graph.newNode(.{ .primitive = .str });
    try graph.unify(unresolved, str);
    try std.testing.expect(try graph.typeIsResolved(open_list));
    try std.testing.expect(try graph.typeCanSealFromExplicitEvidence(open_list));
}

test "open draft function interfaces use related graph classes directly" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const ret = try graph.newNode(.{ .primitive = .bool });
    const left = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{arg}), .ret = ret } });
    const right = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{arg}), .ret = ret } });
    try std.testing.expect(graph.sameFunctionInterface(left, right));
    var interface = try graph.functionInterfaceIterator(left);
    try std.testing.expectEqual(arg, interface.next().?);
    try std.testing.expectEqual(ret, interface.next().?);
    try std.testing.expectEqual(null, interface.next());

    const older_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    try graph.unify(arg, older_arg);
    var aliases = graph.classMemberIterator(arg);
    var saw_arg = false;
    var saw_older_arg = false;
    var alias_count: usize = 0;
    while (aliases.next()) |alias| {
        alias_count += 1;
        saw_arg = saw_arg or alias == arg;
        saw_older_arg = saw_older_arg or alias == older_arg;
    }
    try std.testing.expectEqual(@as(usize, 2), alias_count);
    try std.testing.expect(saw_arg);
    try std.testing.expect(saw_older_arg);

    const other_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const other = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{other_arg}), .ret = ret } });
    try std.testing.expect(!graph.sameFunctionInterface(left, other));
}

test "open function interface shape snapshot alpha-normalizes variables and survives refinement" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const left_var = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const left = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ left_var, left_var }),
        .ret = left_var,
    } });
    const equivalent_var = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const equivalent = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ equivalent_var, equivalent_var }),
        .ret = equivalent_var,
    } });

    const left_shape = try graph.openFunctionInterfaceShape(left);
    const equivalent_shape = try graph.openFunctionInterfaceShape(equivalent);
    try std.testing.expectEqualSlices(u8, &left_shape.digest.bytes, &equivalent_shape.digest.bytes);
    try std.testing.expectEqualSlices(u8, left_shape.bytes, equivalent_shape.bytes);
    var exact_bytes_digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(left_shape.bytes, &exact_bytes_digest, .{});
    try std.testing.expectEqualSlices(u8, &left_shape.digest.bytes, &exact_bytes_digest);

    const distinct_first = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const distinct_second = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const distinct = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ distinct_first, distinct_second }),
        .ret = distinct_first,
    } });
    const distinct_shape = try graph.openFunctionInterfaceShape(distinct);
    try std.testing.expect(!std.mem.eql(u8, &left_shape.digest.bytes, &distinct_shape.digest.bytes));
    try std.testing.expect(!std.mem.eql(u8, left_shape.bytes, distinct_shape.bytes));

    const stored_equivalent_bytes = equivalent_shape.bytes;
    const str = try graph.newNode(.{ .primitive = .str });
    try graph.unify(equivalent_var, str);
    const refined_shape = try graph.openFunctionInterfaceShape(equivalent);
    try std.testing.expect(!std.mem.eql(u8, stored_equivalent_bytes, refined_shape.bytes));
    try std.testing.expectEqualSlices(u8, left_shape.bytes, stored_equivalent_bytes);
}

test "open function interface shape preserves defaults and recursive structure" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const ret = try graph.newNode(.{ .primitive = .bool });
    const record_default = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_record) });
    const record_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{record_default}),
        .ret = ret,
    } });
    const tag_default = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const tag_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{tag_default}),
        .ret = ret,
    } });
    const record_shape = try graph.openFunctionInterfaceShape(record_fn);
    const tag_shape = try graph.openFunctionInterfaceShape(tag_fn);
    try std.testing.expect(!std.mem.eql(u8, record_shape.bytes, tag_shape.bytes));

    const left_cycle = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(left_cycle, .{ .tuple = try graph.arena().dupe(NodeId, &.{left_cycle}) });
    const left_recursive = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{left_cycle}),
        .ret = ret,
    } });
    const right_cycle = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(right_cycle, .{ .tuple = try graph.arena().dupe(NodeId, &.{right_cycle}) });
    const right_recursive = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{right_cycle}),
        .ret = ret,
    } });
    const left_recursive_shape = try graph.openFunctionInterfaceShape(left_recursive);
    const right_recursive_shape = try graph.openFunctionInterfaceShape(right_recursive);
    try std.testing.expectEqualSlices(u8, left_recursive_shape.bytes, right_recursive_shape.bytes);
}

test "open function interface shape includes producer-owned graph evidence" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const ret = try graph.newNode(.{ .primitive = .bool });
    const left_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const left = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{left_arg}),
        .ret = ret,
    } });
    const right_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const right = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{right_arg}),
        .ret = ret,
    } });
    const initial_left_shape = try graph.openFunctionInterfaceShape(left);
    const initial_right_shape = try graph.openFunctionInterfaceShape(right);
    try std.testing.expectEqualSlices(u8, initial_left_shape.bytes, initial_right_shape.bytes);

    try graph.markRecursiveValueSlot(left_arg);
    const recursive_left_shape = try graph.openFunctionInterfaceShape(left);
    const unmarked_right_shape = try graph.openFunctionInterfaceShape(right);
    try std.testing.expect(!std.mem.eql(u8, recursive_left_shape.bytes, unmarked_right_shape.bytes));

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xA7} ** 32));
    const type_name = try name_store.internTypeName("PrivateShape");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const private_left = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .inspectable,
            .authority = .generated_private,
        },
    } });
    const private_right = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .inspectable,
            .authority = .generated_private,
        },
    } });
    const private_left_request = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{private_left}),
        .ret = ret,
    } });
    const private_right_request = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{private_right}),
        .ret = ret,
    } });
    const source_bool = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ret}),
        .ret = ret,
    } });
    const source_str_arg = try graph.newNode(.{ .primitive = .str });
    const source_str = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{source_str_arg}),
        .ret = ret,
    } });
    try graph.registerRequestCheckedSource(private_left_request, source_bool);
    try graph.registerRequestCheckedSource(private_right_request, source_str);

    const private_left_shape = try graph.openFunctionInterfaceShape(private_left_request);
    const private_right_shape = try graph.openFunctionInterfaceShape(private_right_request);
    try std.testing.expect(!std.mem.eql(u8, private_left_shape.bytes, private_right_shape.bytes));
}

test "cyclic row extension is not a resolved graph type" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const row = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(row, .{ .tag_union = .{ .tags = &.{}, .ext = row } });
    try std.testing.expect(!try graph.typeIsResolved(row));
}

fn assertNoNodeId(comptime T: type, comptime path: []const u8) void {
    if (T == NodeId) @compileError(path ++ " exposes instantiation graph NodeId");

    const info = @typeInfo(T);
    if (info == .array) {
        assertNoNodeId(info.array.child, path ++ "[]");
    } else if (info == .optional) {
        assertNoNodeId(info.optional.child, path ++ "?");
    } else if (info == .pointer) {
        switch (info.pointer.size) {
            .slice => assertNoNodeId(info.pointer.child, path ++ "[]"),
            .one, .many, .c => {},
        }
    } else if (info == .@"struct") {
        inline for (info.@"struct".fields) |field| {
            assertNoNodeId(field.type, path ++ "." ++ field.name);
        }
    } else if (info == .@"union") {
        inline for (info.@"union".fields) |field| {
            assertNoNodeId(field.type, path ++ "." ++ field.name);
        }
    }
}

test "active Monotype snapshots are immutable across graph mutations" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const node = try graph.newNode(.{ .primitive = .u64 });
    const first = try graph.activeTypeViewForNode(node);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(first));

    try graph.setContent(node, .{ .primitive = .str });
    const second = try graph.activeTypeViewForNode(node);

    try std.testing.expect(first != second);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(first));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(second));
}

test "record field node carries contextual row evidence into receiver" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("shout!");
    const emit_failed = try name_store.internTagLabel("EmitFailed");
    const exit = try name_store.internTagLabel("Exit");

    const narrow_tags = try graph.arena().alloc(InstTag, 1);
    narrow_tags[0] = .{ .name = emit_failed, .checked_name = emit_failed, .payloads = try graph.arena().alloc(NodeId, 0) };
    const narrow_ret = try graph.newNode(.{ .tag_union = .{
        .tags = narrow_tags,
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) }),
    } });
    const field_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().alloc(NodeId, 0),
        .ret = narrow_ret,
    } });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = field_name, .ty = field_fn };
    const record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });

    const contextual_tags = try graph.arena().alloc(InstTag, 2);
    contextual_tags[0] = .{ .name = emit_failed, .checked_name = emit_failed, .payloads = try graph.arena().alloc(NodeId, 0) };
    contextual_tags[1] = .{ .name = exit, .checked_name = exit, .payloads = try graph.arena().alloc(NodeId, 0) };
    const contextual_ret = try graph.newNode(.{ .tag_union = .{
        .tags = contextual_tags,
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) }),
    } });
    const contextual_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().alloc(NodeId, 0),
        .ret = contextual_ret,
    } });

    const selected_field = try graph.recordFieldNode(record, field_name);
    try graph.unify(selected_field, contextual_fn);

    try graph.freezeRelations();
    const sealed_record = try graph.sealNode(record);
    const sealed_fields = type_store.fieldSpan(type_store.get(sealed_record).record);
    try std.testing.expectEqual(@as(usize, 1), sealed_fields.len);
    const sealed_fn = type_store.get(GuardedList.at(sealed_fields, 0).ty).func;
    const sealed_tags = type_store.tagSpan(type_store.get(sealed_fn.ret).tag_union);
    try std.testing.expectEqual(@as(usize, 2), sealed_tags.len);
    try std.testing.expectEqual(emit_failed, GuardedList.at(sealed_tags, 0).name);
    try std.testing.expectEqual(exit, GuardedList.at(sealed_tags, 1).name);
}

test "graph-native child reads retain live relations until final sealing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const unresolved_ret = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    const function = try graph.newNode(.{ .func = .{
        .args = try graph.arena().alloc(NodeId, 0),
        .ret = unresolved_ret,
    } });
    const function_nodes = try graph.functionNodes(function);
    try std.testing.expectEqual(@as(usize, 0), function_nodes.args.len);
    try std.testing.expect(graph.sameClass(unresolved_ret, function_nodes.ret));

    const field_name = try name_store.internRecordFieldLabel("run");
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = field_name, .ty = function };
    const record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });
    const selected = try graph.recordFieldNode(record, field_name);
    try std.testing.expect(graph.sameClass(function, selected));

    const error_tag = try name_store.internTagLabel("Failed");
    const tags = try graph.arena().alloc(InstTag, 1);
    tags[0] = .{
        .name = error_tag,
        .checked_name = error_tag,
        .payloads = try graph.arena().alloc(NodeId, 0),
    };
    const solved_ret = try graph.newNode(.{ .tag_union = .{
        .tags = tags,
        .ext = try graph.newNode(.empty_tag_union),
    } });
    try graph.unify(function_nodes.ret, solved_ret);

    const element = try graph.newNode(.{ .primitive = .u8 });
    const list = try graph.newNode(.{ .list = element });
    const box = try graph.newNode(.{ .box = element });
    const tuple_items = try graph.arena().alloc(NodeId, 1);
    tuple_items[0] = element;
    const tuple = try graph.newNode(.{ .tuple = tuple_items });
    try std.testing.expect(graph.sameClass(element, try graph.listElementNode(list)));
    try std.testing.expect(graph.sameClass(element, try graph.boxElementNode(box)));
    try std.testing.expect(graph.sameClass(element, (try graph.tupleItemNodes(tuple))[0]));

    const payloads = try graph.arena().alloc(NodeId, 1);
    payloads[0] = element;
    const payload_tags = try graph.arena().alloc(InstTag, 1);
    payload_tags[0] = .{ .name = error_tag, .checked_name = error_tag, .payloads = payloads };
    const tagged = try graph.newNode(.{ .tag_union = .{
        .tags = payload_tags,
        .ext = try graph.newNode(.empty_tag_union),
    } });
    try std.testing.expect(graph.sameClass(element, try graph.tagPayloadNode(tagged, error_tag, 0)));

    try graph.freezeRelations();
    const sealed = try graph.sealNode(record);
    const sealed_fields = type_store.fieldSpan(type_store.get(sealed).record);
    const sealed_function = type_store.get(GuardedList.at(sealed_fields, 0).ty).func;
    const sealed_tags = type_store.tagSpan(type_store.get(sealed_function.ret).tag_union);
    try std.testing.expectEqual(@as(usize, 1), sealed_tags.len);
    try std.testing.expectEqual(error_tag, GuardedList.at(sealed_tags, 0).name);
}

test "record field graph access distinguishes inspection from runtime construction" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    try std.testing.expect(!InstGraph.backingAllowsAccess(.runtime_layout_only, .inspectable));
    try std.testing.expect(InstGraph.backingAllowsAccess(.runtime_layout_only, .runtime_layout));

    const field_name = try name_store.internRecordFieldLabel("private");
    const field_ty = try graph.newNode(.{ .primitive = .u8 });
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = field_name, .ty = field_ty };
    const backing = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });
    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xB1} ** 32));
    const type_name = try name_store.internTypeName("PrivateRecord");
    const named = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(11) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    } });

    const selected = try graph.recordConstructionFieldNode(named, field_name);
    try std.testing.expect(graph.sameClass(field_ty, selected));
    const definition_private = try graph.opaqueDefinitionFieldNode(named, field_name);
    try std.testing.expect(graph.sameClass(field_ty, definition_private));
}

test "active Monotype snapshots keep different roots distinct" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const old_root = try graph.newNode(.{ .primitive = .u64 });
    const new_root = try graph.newNode(.{ .primitive = .str });
    const old_view = try graph.activeTypeViewForNode(old_root);
    const new_view = try graph.activeTypeViewForNode(new_root);

    try std.testing.expect(old_view != new_view);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(old_view));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(new_view));
}

test "union resolves immutable snapshot provenance without reindexing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const winner = try graph.newNode(.{ .primitive = .u64 });
    const snapshot_count = 128;
    var snapshots: [snapshot_count]Type.TypeId = undefined;
    var owners: [snapshot_count]NodeId = undefined;

    for (&snapshots, &owners) |*snapshot, *owner| {
        const node = try graph.newNode(.{ .primitive = .u64 });
        snapshot.* = try graph.activeTypeViewForNode(node);
        owner.* = graph.linked_type_nodes.get(snapshot.*).?;
        try graph.union_(winner, node);
    }

    for (snapshots, owners) |snapshot, owner| {
        // The reverse index remains stable instead of rewriting every prior
        // snapshot on each union. Root resolution happens only when queried.
        try std.testing.expectEqual(owner, graph.linked_type_nodes.get(snapshot).?);
        try std.testing.expectEqual(winner, graph.activeSnapshotNode(snapshot).?);
    }
}

test "alias unification does not make the alias its own backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const backing = try graph.newNode(.{ .primitive = .u64 });
    const alias = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32)), .type_name = @enumFromInt(1) },
        .kind = .alias,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .inspectable },
    } });

    try graph.unify(alias, backing);
    try std.testing.expect(graph.find(alias) != graph.find(backing));

    const alias_ty = try graph.activeTypeViewForNode(alias);
    const alias_content = type_store.get(alias_ty);
    if (alias_content != .named) return error.TestExpectedEqual;
    const named = alias_content.named;
    const named_backing = named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(named_backing.ty != alias_ty);
}

test "nominal unification with its own backing preserves a distinct backing node" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const backing = try graph.newNode(.{ .primitive = .u64 });
    const nominal = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(2) },
        .def = .{ .module = try name_store.internModuleIdentity(&([_]u8{0xAC} ** 32)), .type_name = @enumFromInt(2) },
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .inspectable },
    } });

    try graph.unify(nominal, backing);
    try std.testing.expect(graph.sameClass(nominal, backing));

    const named = graph.content(nominal).named;
    const retained_backing = named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(!graph.sameClass(nominal, retained_backing.node));
    try std.testing.expectEqual(InstNode{ .primitive = .u64 }, graph.content(retained_backing.node));
    try std.testing.expect(!graph.finalizesAsClosedEmptyTagUnion(nominal));
}

test "nominal selection over its unresolved backing preserves a distinct backing node" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const backing = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const nominal = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(3) },
        .def = .{ .module = try name_store.internModuleIdentity(&([_]u8{0xAD} ** 32)), .type_name = @enumFromInt(3) },
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = backing, .use = .inspectable },
    } });

    try graph.unify(nominal, backing);
    try std.testing.expect(graph.sameClass(nominal, backing));

    const retained_backing = graph.content(nominal).named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(!graph.sameClass(nominal, retained_backing.node));
    try std.testing.expectEqual(
        InstVariableOrigin.checked_variable,
        graph.content(retained_backing.node).unresolved.origin,
    );
}

test "equivalent nominal wrapper unification preserves the structural backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAE} ** 32));
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = @enumFromInt(4) };
    const structural = try graph.newNode(.{ .primitive = .u64 });
    const inner = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(4) },
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = structural, .use = .inspectable },
    } });
    const outer = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(4) },
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = inner, .use = .inspectable },
    } });

    try graph.unify(outer, inner);
    try std.testing.expect(graph.sameClass(outer, inner));

    const retained_backing = graph.content(outer).named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(!graph.sameClass(outer, retained_backing.node));
    try std.testing.expectEqual(structural, graph.find(retained_backing.node));
}

test "checked type mapping crosses a nominal view without changing exact root authority" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("value");
    const checked_field = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const exact_field = try graph.newNode(.{ .primitive = .u64 });
    const checked_backing = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = field_name, .ty = checked_field }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const checked_nominal = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(3) },
        .def = .{
            .module = try name_store.internModuleIdentity(&([_]u8{0xAD} ** 32)),
            .type_name = try name_store.internTypeName("Wrapper"),
        },
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = checked_backing, .use = .inspectable },
    } });
    const exact_structural = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = field_name, .ty = exact_field }}),
        .ext = try graph.newNode(.empty_record),
    } });

    const selected = try graph.applyCheckedTypeMapping(checked_nominal, exact_structural);

    try std.testing.expectEqual(graph.find(exact_structural), selected);
    try std.testing.expect(!graph.sameClass(checked_nominal, exact_structural));
    try std.testing.expect(graph.sameClass(checked_field, exact_field));
}

test "final sealing does not mutate an earlier active snapshot" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });

    const snapshot = try graph.activeTypeViewForNode(row);
    const snapshot_field = GuardedList.at(type_store.fieldSpan(type_store.get(snapshot).record), 0);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(snapshot_field.ty));

    try graph.setContent(a_ty, .{ .primitive = .str });

    try graph.freezeRelations();
    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed = try finals.sealType(snapshot);

    try std.testing.expect(sealed != snapshot);
    const still_snapshot_field = GuardedList.at(type_store.fieldSpan(type_store.get(snapshot).record), 0);
    const sealed_field = GuardedList.at(type_store.fieldSpan(type_store.get(sealed).record), 0);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(still_snapshot_field.ty));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(sealed_field.ty));
}

test "final graph function recursively replaces active snapshots" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });

    const args = try graph.arena().alloc(NodeId, 1);
    args[0] = row;
    const fn_node = try graph.newNode(.{ .func = .{
        .args = args,
        .ret = row,
    } });
    const draft_fn = try graph.activeTypeViewForNode(fn_node);
    try graph.setContent(a_ty, .{ .primitive = .str });

    try graph.freezeRelations();
    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed_fn = try finals.sealType(draft_fn);
    try std.testing.expect(sealed_fn != draft_fn);
    const sealed_arg = GuardedList.at(type_store.span(type_store.get(sealed_fn).func.args), 0);

    const draft_arg = GuardedList.at(type_store.span(type_store.get(draft_fn).func.args), 0);
    const draft_field = GuardedList.at(type_store.fieldSpan(type_store.get(draft_arg).record), 0);
    const sealed_field = GuardedList.at(type_store.fieldSpan(type_store.get(sealed_arg).record), 0);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(draft_field.ty));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(sealed_field.ty));
}

test "final sealed graph node does not allocate an active snapshot" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = ext,
    } });

    try graph.freezeRelations();
    const sealed = try graph.sealNode(row);
    try std.testing.expectEqual(@as(usize, 0), graph.node_snapshots.count());
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);
}

test "active view of an imported recursive type preserves its exact immutable representation" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const node_tag = try name_store.internTagLabel("Node");
    const Context = struct {
        types: *Type.Store,
        names: *names.NameStore,
        tag: names.TagNameId,

        fn fill(self: @This(), reserved: Type.TypeId) std.mem.Allocator.Error!Type.Content {
            const payloads = try self.types.addSpan(&.{reserved});
            const tags = try self.types.addTagVariants(self.names, &.{.{
                .name = self.tag,
                .checked_name = self.tag,
                .payloads = payloads,
            }});
            return .{ .tag_union = tags };
        }
    };
    const exact = try type_store.addRecursive(Context{
        .types = &type_store,
        .names = &name_store,
        .tag = node_tag,
    }, Context.fill);

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const imported = try graph.importMono(exact);
    try std.testing.expectEqual(exact, try graph.activeTypeViewForNode(imported));
}

test "unresolved row graph node seals to closed empty tag union only at finalization" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const node = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    try graph.freezeRelations();
    const sealed = try graph.sealNode(node);
    const content = type_store.get(sealed);

    try std.testing.expectEqual(Type.Span.empty(), content.tag_union);
}

test "relation mutation invalidates active snapshots before freezing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const resolved = try graph.newNode(.{ .primitive = .u64 });
    const before_mutation = try graph.activeTypeViewForNode(resolved);
    try std.testing.expect(graph.current_snapshots.count() != 0);
    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    try graph.unify(resolved, unresolved);

    try std.testing.expect(graph.acceptsRelationMutation());
    try std.testing.expect(graph.current_snapshots_dirty);

    const after_mutation = try graph.activeTypeViewForNode(resolved);
    try std.testing.expect(!graph.current_snapshots_dirty);
    try std.testing.expect(graph.current_snapshots.count() != 0);
    try std.testing.expect(before_mutation != after_mutation);

    try graph.freezeRelations();

    try std.testing.expectEqual(RelationState.frozen, graph.relation_state);
    try std.testing.expect(!graph.acceptsRelationMutation());
}

test "final type sealing remains allowed after instantiation relations freeze" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const node = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    try graph.freezeRelations();

    const sealed = try graph.sealNode(node);
    const content = type_store.get(sealed);
    try std.testing.expectEqual(Type.Span.empty(), content.tag_union);
}

test "imported closed tag row rejects additional evidence without mutating shared import" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const a = try name_store.internTagLabel("A");
    const b = try name_store.internTagLabel("B");
    const no_payloads = try type_store.addSpan(&.{});
    const requested_tags = try type_store.addTags(&.{.{
        .name = a,
        .checked_name = a,
        .payloads = no_payloads,
    }});
    const requested = try type_store.add(.{ .tag_union = requested_tags });

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const request_node = try graph.importMono(requested);
    const shared_request_node = try graph.importMono(requested);
    try std.testing.expectEqual(request_node, shared_request_node);

    const imported = graph.content(request_node).tag_union;
    const additional_tags = [_]InstTag{.{ .name = b, .checked_name = b, .payloads = &.{} }};
    try std.testing.expect(graph.rowAdditionConflicts(imported.ext, additional_tags.len, .tag_union));
    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(imported.ext));

    const retained = graph.content(shared_request_node).tag_union;
    try std.testing.expectEqual(@as(usize, 1), retained.tags.len);
    try std.testing.expectEqual(a, retained.tags[0].name);
    try std.testing.expect(retained.tags[0].name != b);
    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(retained.ext));
}

test "imported closed record row rejects additional evidence without mutating shared import" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const value = try name_store.internRecordFieldLabel("value");
    const extra = try name_store.internRecordFieldLabel("extra");
    const u64_ty = try type_store.add(.{ .primitive = .u64 });
    const requested_fields = try type_store.addRecordFields(&name_store, &.{.{
        .name = value,
        .ty = u64_ty,
    }});
    const requested = try type_store.add(.{ .record = requested_fields });

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const request_node = try graph.importMono(requested);
    const shared_request_node = try graph.importMono(requested);
    try std.testing.expectEqual(request_node, shared_request_node);

    const imported = graph.content(request_node).record;
    const additional_fields = [_]InstField{.{
        .name = extra,
        .ty = try graph.newNode(.{ .primitive = .u64 }),
    }};
    try std.testing.expect(graph.rowAdditionConflicts(imported.ext, additional_fields.len, .record));
    try std.testing.expectEqual(InstNode.empty_record, graph.content(imported.ext));

    const retained = graph.content(shared_request_node).record;
    try std.testing.expectEqual(@as(usize, 1), retained.fields.len);
    try std.testing.expectEqual(value, retained.fields[0].name);
    try std.testing.expect(retained.fields[0].name != extra);
    try std.testing.expectEqual(InstNode.empty_record, graph.content(retained.ext));
}

test "explicit empty tag union imports as closed uninhabited row" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const explicit_empty = try type_store.add(.{ .tag_union = Type.Span.empty() });
    const imported = try graph.importMono(explicit_empty);

    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(imported));
}

test "opaque interface relation preserves distinct public and generated-private backing authority" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAD} ** 32));
    const type_name = try name_store.internTypeName("FieldNames");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const public_args = try graph.arena().alloc(NodeId, 1);
    public_args[0] = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const private_args = try graph.arena().alloc(NodeId, 1);
    private_args[0] = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public_backing = try graph.newNode(.empty_record);
    const private_backing = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().alloc(InstField, 0),
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) }),
    } });
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = public_args,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = private_args,
        .backing = .{
            .node = private_backing,
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    _ = try graph.applyProducedTypeToRequest(public, private);

    const public_content = graph.content(public);
    if (public_content != .named) return error.TestUnexpectedResult;
    const retained_public = public_content.named;
    const private_content = graph.content(private);
    if (private_content != .named) return error.TestUnexpectedResult;
    const retained_private = private_content.named;
    try std.testing.expect(!graph.sameClass(public, private));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, retained_private.backing.?.authority);
    try std.testing.expect(!graph.sameClass(public_backing, private_backing));
    try std.testing.expect(graph.sameClass(public_args[0], private_args[0]));
    try std.testing.expectEqual(@as(usize, 0), (try graph.recordConstructionNodes(public)).fields.len);

    const field_name = try name_store.internRecordFieldLabel("value");
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = field_name, .ty = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) }) };
    const structural_record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) }),
    } });
    const projected = try graph.recordNodes(structural_record);
    try std.testing.expectEqual(@as(usize, 1), projected.fields.len);
    try std.testing.expectEqual(field_name, projected.fields[0].name);
}

test "function request follows checked occurrence identity and keeps independent slots distinct" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xCF} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(12) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const public_backing = try graph.newNode(.empty_record);
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    const exact = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const checked_list = try graph.newNode(.{ .list = slot });
    const checked_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ checked_list, slot }),
        .ret = checked_list,
    } });
    const public_list = try graph.newNode(.{ .list = public });
    const current_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ public_list, public }),
        .ret = public_list,
    } });
    const produced_list = try graph.newNode(.{ .list = public });

    const request = try graph.functionRequestFromProducedArguments(
        checked_fn,
        current_fn,
        &.{ produced_list, exact },
    );
    const request_fn = try graph.functionNodes(request);

    try std.testing.expect(!graph.sameClass(request, current_fn));
    try std.testing.expect(graph.sameClass(try graph.listElementNode(request_fn.args[0]), exact));
    try std.testing.expect(graph.sameClass(request_fn.args[1], exact));
    try std.testing.expect(graph.sameClass(try graph.listElementNode(request_fn.ret), exact));
    try std.testing.expect(!graph.sameClass(public, exact));
    const request_selections = graph.requestSubstitutions(request);
    try std.testing.expectEqual(@as(usize, 1), request_selections.len);
    try std.testing.expect(graph.sameClass(request_selections[0].checked, slot));
    try std.testing.expect(graph.sameClass(request_selections[0].produced, exact));

    const isolated_request = try graph.isolateFunctionAbi(request);
    const isolated_request_fn = try graph.functionNodes(isolated_request);
    try std.testing.expect(!graph.sameClass(isolated_request, request));
    try std.testing.expect(graph.sameClass(isolated_request_fn.args[0], request_fn.args[0]));
    try std.testing.expect(graph.sameClass(isolated_request_fn.args[1], request_fn.args[1]));
    try std.testing.expect(graph.sameClass(isolated_request_fn.ret, request_fn.ret));
    const isolated_selections = graph.requestSubstitutions(isolated_request);
    try std.testing.expectEqual(@as(usize, 1), isolated_selections.len);
    try std.testing.expect(graph.sameClass(isolated_selections[0].checked, slot));
    try std.testing.expect(graph.sameClass(isolated_selections[0].produced, exact));

    const recursive_type_name = try name_store.internTypeName("Node");
    const recursive = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const recursive_backing = try graph.newNode(.{ .list = recursive });
    try graph.setContent(recursive, .{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(13) },
        .def = .{ .module = module_identity, .type_name = recursive_type_name },
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = recursive_backing, .use = .inspectable },
    } });
    const recursive_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{recursive}),
        .ret = item,
    } });
    const isolated_recursive_fn = try graph.isolateFunctionAbi(recursive_fn);
    const isolated_recursive = try graph.functionNodes(isolated_recursive_fn);
    try std.testing.expect(!graph.sameClass(isolated_recursive_fn, recursive_fn));
    try std.testing.expect(graph.sameClass(isolated_recursive.args[0], recursive));

    var refinement_diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&refinement_diagnostics);
    const refined_request = try graph.functionRequestFromProducedArguments(
        checked_fn,
        request,
        &.{ produced_list, exact },
    );
    const refined_fn = try graph.functionNodes(refined_request);
    try std.testing.expect(graph.sameClass(try graph.listElementNode(refined_fn.args[0]), exact));
    try std.testing.expect(graph.sameClass(refined_fn.args[1], exact));
    // Refinement seeds the existing replacement from request metadata. A
    // previous-interface walk would rediscover and count that replacement.
    try std.testing.expectEqual(@as(u64, 0), refinement_diagnostics.function_request_replacements);

    const checked_view_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{public}),
        .ret = public,
    } });
    const structural_view_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{public_backing}),
        .ret = public_backing,
    } });
    const structural_request = try graph.functionRequestFromProducedArguments(
        checked_view_fn,
        structural_view_fn,
        &.{public_backing},
    );
    const structural_request_fn = try graph.functionNodes(structural_request);
    try std.testing.expect(!graph.sameClass(structural_request, structural_view_fn));
    try std.testing.expect(graph.content(structural_request_fn.args[0]) == .named);
    const structural_arg = graph.namedNodes(structural_request_fn.args[0]);
    try std.testing.expect(structural_arg.backing != null);
    const structural_arg_backing = structural_arg.backing.?;
    try std.testing.expect(graph.sameClass(structural_arg_backing.node, public_backing));
    try std.testing.expect(graph.content(structural_request_fn.ret) == .empty_record);

    const unique_concrete_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{public}),
        .ret = public,
    } });
    const unique_concrete_request = try graph.functionRequestFromProducedArguments(
        unique_concrete_fn,
        unique_concrete_fn,
        &.{exact},
    );
    const unique_concrete_request_fn = try graph.functionNodes(unique_concrete_request);
    try std.testing.expect(graph.sameClass(unique_concrete_request_fn.args[0], exact));
    try std.testing.expect(graph.sameClass(unique_concrete_request_fn.ret, exact));
    const substitutions_before_noop = graph.request_substitutions.items.len;
    const repeated_unique_request = try graph.functionRequestFromProducedArguments(
        unique_concrete_fn,
        unique_concrete_request,
        &.{exact},
    );
    try std.testing.expectEqual(unique_concrete_request, repeated_unique_request);
    try std.testing.expectEqual(substitutions_before_noop, graph.request_substitutions.items.len);

    const independent_first = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const independent_second = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const independent_checked_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ independent_first, independent_second }),
        .ret = independent_second,
    } });
    const shared_public_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ public, public }),
        .ret = public,
    } });
    const independent_request = try graph.functionRequestFromProducedArguments(
        independent_checked_fn,
        shared_public_fn,
        &.{ exact, public },
    );
    const independent_request_fn = try graph.functionNodes(independent_request);
    try std.testing.expect(graph.sameClass(independent_request_fn.args[0], exact));
    try std.testing.expect(graph.sameClass(independent_request_fn.args[1], public));
    try std.testing.expect(graph.sameClass(independent_request_fn.ret, public));

    const mixed_concrete_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ public, public }),
        .ret = public,
    } });
    const mixed_concrete_request = try graph.functionRequestFromProducedArguments(
        mixed_concrete_fn,
        mixed_concrete_fn,
        &.{ public, exact },
    );
    const mixed_concrete_request_fn = try graph.functionNodes(mixed_concrete_request);
    try std.testing.expect(graph.sameClass(mixed_concrete_request_fn.args[0], exact));
    try std.testing.expect(graph.sameClass(mixed_concrete_request_fn.args[1], exact));
    try std.testing.expect(graph.sameClass(mixed_concrete_request_fn.ret, exact));

    var second_def = def;
    second_def.generated = .{ .bytes = [_]u8{0xA2} ** 32 };
    const second_exact = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = second_def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const second_public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    } });
    const return_public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    } });
    const concrete_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ public, second_public }),
        .ret = return_public,
    } });
    const distinct_request = try graph.functionRequestFromProducedArguments(
        concrete_fn,
        concrete_fn,
        &.{ exact, second_exact },
    );
    const distinct_request_fn = try graph.functionNodes(distinct_request);
    try std.testing.expect(graph.sameClass(distinct_request_fn.args[0], exact));
    try std.testing.expect(graph.sameClass(distinct_request_fn.args[1], second_exact));
    try std.testing.expect(!graph.sameClass(distinct_request_fn.args[0], distinct_request_fn.args[1]));
    try std.testing.expect(graph.sameClass(distinct_request_fn.ret, return_public));
    const distinct_selections = graph.requestSubstitutions(distinct_request);
    try std.testing.expectEqual(@as(usize, 2), distinct_selections.len);
    var found_first = false;
    var found_second = false;
    for (distinct_selections) |selection| {
        if (graph.sameClass(selection.checked, public) and graph.sameClass(selection.produced, exact)) {
            found_first = true;
        }
        if (graph.sameClass(selection.checked, second_public) and graph.sameClass(selection.produced, second_exact)) {
            found_second = true;
        }
    }
    try std.testing.expect(found_first);
    try std.testing.expect(found_second);

    const stored_public_list = try graph.newNode(.{ .list = public });
    const requested_exact_list = try graph.newNode(.{ .list = exact });
    try graph.applyCompoundStorageRepresentation(requested_exact_list, stored_public_list);
    try std.testing.expect(graph.sameClass(requested_exact_list, stored_public_list));
    try std.testing.expect(graph.sameClass(try graph.listElementNode(stored_public_list), exact));
    try std.testing.expect(!graph.sameClass(public, exact));

    const recursive_active = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{public}),
        .ret = public,
    } });
    const initial_classes = try graph.snapshotFunctionArgumentClasses(recursive_active);
    const recursive_request = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{exact}),
        .ret = exact,
    } });
    try graph.joinRecursiveFunctionInterface(recursive_active, initial_classes, recursive_request);
    const joined_active = try graph.functionNodes(recursive_active);
    const joined_recursive = try graph.functionNodes(recursive_request);
    try std.testing.expect(graph.sameClass(joined_active.args[0], exact));
    try std.testing.expect(graph.sameClass(joined_active.ret, exact));
    try std.testing.expect(graph.sameClass(joined_active.args[0], joined_recursive.args[0]));
    try std.testing.expect(graph.sameClass(joined_active.ret, joined_recursive.ret));
    try std.testing.expect(!graph.sameClass(recursive_active, recursive_request));

    const first_tag = try name_store.internTagLabel("First");
    const second_tag = try name_store.internTagLabel("Second");
    const row_slot = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const checked_row_tail = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = second_tag,
            .checked_name = second_tag,
            .payloads = try graph.arena().dupe(NodeId, &.{row_slot}),
        }}),
        .ext = try graph.newNode(.empty_tag_union),
    } });
    const checked_row = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = first_tag,
            .checked_name = first_tag,
            .payloads = try graph.arena().dupe(NodeId, &.{row_slot}),
        }}),
        .ext = checked_row_tail,
    } });
    const completed_row = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{
            .{ .name = first_tag, .checked_name = first_tag, .payloads = try graph.arena().dupe(NodeId, &.{exact}) },
            .{ .name = second_tag, .checked_name = second_tag, .payloads = try graph.arena().dupe(NodeId, &.{exact}) },
        }),
        .ext = try graph.newNode(.empty_tag_union),
    } });
    const checked_row_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().alloc(NodeId, 0),
        .ret = checked_row,
    } });
    const completed_row_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().alloc(NodeId, 0),
        .ret = completed_row,
    } });
    const row_request = try graph.functionRequestFromProducedArguments(
        checked_row_fn,
        completed_row_fn,
        &.{},
    );
    const flattened_request = try graph.flattenTagRow((try graph.functionNodes(row_request)).ret);
    try std.testing.expectEqual(@as(usize, 2), flattened_request.tags.len);
    for (flattened_request.tags) |tag| {
        try std.testing.expectEqual(@as(usize, 1), tag.payloads.len);
        try std.testing.expect(graph.sameClass(tag.payloads[0], exact));
    }
}

test "function ABI isolation preserves nested row extensions" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const first_tag = try name_store.internTagLabel("First");
    const second_tag = try name_store.internTagLabel("Second");
    const tag_tail = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = second_tag,
            .checked_name = second_tag,
            .payloads = &.{},
        }}),
        .ext = try graph.newNode(.empty_tag_union),
    } });
    const tag_root = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = first_tag,
            .checked_name = first_tag,
            .payloads = &.{},
        }}),
        .ext = tag_tail,
    } });

    const first_field = try name_store.internRecordFieldLabel("first");
    const second_field = try name_store.internRecordFieldLabel("second");
    const value = try graph.newNode(.{ .primitive = .u64 });
    const record_tail = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = second_field, .ty = value }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const record_root = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = first_field, .ty = value }}),
        .ext = record_tail,
    } });

    const fn_node = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ tag_root, record_root }),
        .ret = value,
    } });
    const isolated = try graph.isolateFunctionAbi(fn_node);
    const isolated_fn = try graph.functionNodes(isolated);

    try std.testing.expect(!graph.sameClass(isolated, fn_node));
    try std.testing.expect(graph.sameClass(isolated_fn.args[0], tag_root));
    try std.testing.expect(graph.sameClass(isolated_fn.args[1], record_root));
    try std.testing.expectEqual(@as(usize, 2), (try graph.flattenTagRow(tag_root)).tags.len);
    try std.testing.expectEqual(@as(usize, 2), (try graph.flattenRecordRow(record_root)).fields.len);
}

test "checked type mapping preserves forced-dynamic iterator identity" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xFE} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(4) };
    const public_item = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const private_item = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public_backing = try graph.newNode(.empty_record);
    const private_backing = try graph.newNode(.empty_record);
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{public_item}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_representation = .forced_dynamic,
            .iterator_kind = .forced_dynamic,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{private_item}),
        .backing = .{
            .node = private_backing,
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    _ = try graph.applyCheckedTypeMapping(public, private);

    try std.testing.expect(!graph.sameClass(public, private));
    try std.testing.expect(graph.sameClass(public_item, private_item));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, graph.content(public).named.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(private).named.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.none, graph.content(public).named.def.iterator_representation);
    try std.testing.expectEqual(Type.IteratorRepresentation.forced_dynamic, graph.content(private).named.def.iterator_representation);
}

test "opaque iterator relation selects generated-private nominal over unresolved request" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x51} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(9) };
    const public_backing = try graph.newNode(.empty_record);
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const public = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const private_item = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{private_item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{},
            .public_source = public_source,
        },
    } });

    _ = try graph.applyProducedTypeToRequest(public, private);

    const selected = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, private));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, selected.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, selected.def.iterator_representation);
    try std.testing.expectEqual(@as(usize, 1), selected.args.len);
    try std.testing.expect(graph.sameClass(selected.args[0], private_item));
    try std.testing.expect(selected.generated_iterator != null);
}

test "opaque iterator relation resolves unresolved public variable to imported generated iterator" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x71} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(12) };
    const public = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const item = try graph.newNode(.{ .primitive = .u64 });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .generated = .{ .bytes = [_]u8{0x72} ** 32 },
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    _ = try graph.applyProducedTypeToRequest(public, private);

    const retained = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, private));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, retained.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, retained.def.iterator_representation);
    try std.testing.expectEqual(@as(usize, 1), retained.args.len);
    try std.testing.expect(graph.sameClass(retained.args[0], item));
}

test "opaque interface relation deduplicates only identical generated-private iterator requests" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x73} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(13) };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const left_iter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .generated = .{ .bytes = [_]u8{0x74} ** 32 },
            .iterator_representation = .minted,
            .iterator_kind = .concat,
            .iterator_depth = 2,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const right_iter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .generated = .{ .bytes = [_]u8{0x74} ** 32 },
            .iterator_representation = .minted,
            .iterator_kind = .concat,
            .iterator_depth = 2,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const public_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{left_iter}),
        .ret = try graph.newNode(.empty_record),
    } });
    const private_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{right_iter}),
        .ret = try graph.newNode(.empty_record),
    } });

    _ = try graph.applyProducedTypeToRequest(public_fn, private_fn);

    try std.testing.expect(graph.sameClass(left_iter, right_iter));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(left_iter).named.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, graph.content(left_iter).named.def.iterator_representation);

    const distinct_item = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const distinct_iter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .generated = .{ .bytes = [_]u8{0x75} ** 32 },
            .iterator_representation = .minted,
            .iterator_kind = .concat,
            .iterator_depth = 2,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{distinct_item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    _ = try graph.applyProducedTypeToRequest(left_iter, distinct_iter);

    try std.testing.expect(!graph.sameClass(left_iter, distinct_iter));
    try std.testing.expect(graph.sameClass(item, distinct_item));
}

test "opaque relation materializes unresolved public named shell from request" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x52} ** 32));
    const shell_type_name = try name_store.internTypeName("ShellEvidence");
    const iter_type_name = try name_store.internTypeName("Iter");
    const shell_named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(10) };
    const iter_named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(11) };
    const shell_def: Type.TypeDef = .{ .module = module_identity, .type_name = shell_type_name };
    const iter_def: Type.TypeDef = .{ .module = module_identity, .type_name = iter_type_name };
    const public = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public_source: InstIteratorPublicSource = .{
        .named_type = iter_named_type,
        .def = iter_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = try graph.newNode(.empty_record), .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const private_arg = try graph.newNode(.{ .named = .{
        .named_type = iter_named_type,
        .def = .{
            .module = module_identity,
            .type_name = iter_type_name,
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{},
            .public_source = public_source,
        },
    } });
    const request = try graph.newNode(.{ .named = .{
        .named_type = shell_named_type,
        .def = shell_def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{private_arg}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    } });

    _ = try graph.applyProducedTypeToRequest(public, request);

    const retained_public = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, request));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(@as(usize, 1), retained_public.args.len);
    try std.testing.expect(graph.sameClass(retained_public.args[0], private_arg));
}

test "generated iterator interning happens before backing work and survives later unions" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x63} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(12) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const item_at_construction = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const item_at_lookup = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const component_at_construction = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const component_at_lookup = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public_backing = try graph.newNode(.empty_record);
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const public_at_construction = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item_at_construction}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    const generated = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_representation = .minted,
            .iterator_kind = .map,
            .iterator_depth = 2,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item_at_construction}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{component_at_construction},
            .public_source = public_source,
        },
    } });
    try graph.registerGeneratedIterator(generated);
    try std.testing.expectEqual(
        generated,
        (try graph.findGeneratedIterator(public_at_construction, .map, &.{component_at_construction}, null)).?,
    );

    // Current roots change after the intern key was recorded. The permanent
    // item-class index still finds the same exact generated identity without a
    // graph-wide search or repeated backing construction.
    try graph.unify(item_at_construction, item_at_lookup);
    try graph.unify(component_at_construction, component_at_lookup);
    const public_at_lookup = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item_at_lookup}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    try std.testing.expectEqual(
        generated,
        (try graph.findGeneratedIterator(public_at_lookup, .map, &.{component_at_lookup}, null)).?,
    );

    // Separately allocated closed inputs with the same exact structure are
    // one construction identity. The lookup must find the existing node
    // before a caller constructs and lowers a duplicate private backing.
    const closed_item_at_construction = try graph.newNode(.{ .primitive = .i64 });
    const closed_item_at_lookup = try graph.newNode(.{ .primitive = .i64 });
    const closed_component_at_construction = try graph.newNode(.{ .list = closed_item_at_construction });
    const closed_component_at_lookup = try graph.newNode(.{ .list = closed_item_at_lookup });
    const closed_generated = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_representation = .minted,
            .iterator_kind = .list,
            .iterator_depth = 1,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{closed_item_at_construction}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{closed_component_at_construction},
            .public_source = public_source,
        },
    } });
    try graph.registerGeneratedIterator(closed_generated);
    const closed_public_at_lookup = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{closed_item_at_lookup}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    try std.testing.expectEqual(
        closed_generated,
        (try graph.findGeneratedIterator(closed_public_at_lookup, .list, &.{closed_component_at_lookup}, null)).?,
    );

    // Independent open cells cannot be equated safely during construction,
    // but checked variables that remain open have one explicit final meaning:
    // the empty tag union. Coalesce equal content addresses at the relation
    // finalization barrier, before either backing reaches a later IR.
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    const open_item_a = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const open_item_b = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const open_component_a = try graph.newNode(.{ .list = open_item_a });
    const open_component_b = try graph.newNode(.{ .list = open_item_b });
    const open_nodes = try graph.arena().alloc(NodeId, 2);
    for (open_nodes, [_]NodeId{ open_item_a, open_item_b }, [_]NodeId{ open_component_a, open_component_b }) |*node, open_item, open_component| {
        node.* = try graph.newNode(.{ .named = .{
            .named_type = named_type,
            .def = .{
                .module = module_identity,
                .type_name = type_name,
                .iterator_representation = .minted,
                .iterator_kind = .list,
                .iterator_depth = 1,
            },
            .kind = .@"opaque",
            .builtin_owner = .iter,
            .args = try graph.arena().dupe(NodeId, &.{open_item}),
            .backing = .{
                .node = try graph.newNode(.empty_record),
                .use = .runtime_layout_only,
                .authority = .generated_private,
            },
            .generated_iterator = .{
                .callable_evidence = null,
                .components = try graph.arena().dupe(NodeId, &.{open_component}),
                .public_source = public_source,
            },
        } });
        try graph.registerGeneratedIterator(node.*);
    }
    try std.testing.expect(!graph.sameClass(open_nodes[0], open_nodes[1]));
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expect(graph.sameClass(open_nodes[0], open_nodes[1]));
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_identity_roots_coalesced);
}

test "generated iterator store interning preserves authoritative backing TypeIds across graphs" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    var generated_types_by_identity = std.AutoHashMap(names.TypeDigest, Type.TypeId).init(gpa);
    defer generated_types_by_identity.deinit();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x68} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(17) };
    const public_def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };

    const Built = struct { root: NodeId, backing: NodeId };
    const Build = struct {
        fn inGraph(
            graph: *InstGraph,
            source_named_type: Type.NamedType,
            source_def: Type.TypeDef,
        ) Allocator.Error!Built {
            const item = try graph.newNode(.{ .primitive = .u64 });
            const backing = try graph.newNode(.{ .func = .{
                .args = try graph.arena().dupe(NodeId, &.{item}),
                .ret = item,
            } });
            const public_source: InstIteratorPublicSource = .{
                .named_type = source_named_type,
                .def = source_def,
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .backing = .{
                    .node = try graph.newNode(.empty_record),
                    .use = .runtime_layout_only,
                },
                .declared_order = &.{},
            };
            var generated_def = source_def;
            generated_def.iterator_kind = .list;
            const root = try graph.newNode(.{ .named = .{
                .named_type = source_named_type,
                .def = generated_def,
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{item}),
                .backing = .{
                    .node = backing,
                    .use = .runtime_layout_only,
                    .authority = .generated_private,
                },
                .generated_iterator = .{
                    .callable_evidence = null,
                    .components = try graph.arena().dupe(NodeId, &.{try graph.newNode(.{ .list = item })}),
                    .public_source = public_source,
                },
            } });
            try graph.registerGeneratedIterator(root);
            return .{ .root = root, .backing = backing };
        }
    };

    var authoritative_root: Type.TypeId = undefined;
    var authoritative_backing: Type.TypeId = undefined;
    {
        const graph = try InstGraph.create(gpa, &type_store, &name_store);
        defer graph.destroy();
        const built = try Build.inGraph(graph, named_type, public_def);
        try graph.finalizeGeneratedIteratorRepresentations();
        try graph.finalizeGeneratedIteratorIdentities();
        try graph.bindGeneratedIteratorAuthoritativeTypes(&generated_types_by_identity);
        try graph.freezeRelations();
        var sealer = GraphTypeFinals.initWithGeneratedTypeInterner(graph, &generated_types_by_identity);
        defer sealer.deinit();
        authoritative_root = try sealer.sealNode(built.root);
        authoritative_backing = try sealer.sealNode(built.backing);
    }

    {
        const graph = try InstGraph.create(gpa, &type_store, &name_store);
        defer graph.destroy();
        var diagnostics: GraphDiagnostics = .{};
        graph.setDiagnostics(&diagnostics);
        const built = try Build.inGraph(graph, named_type, public_def);
        try graph.finalizeGeneratedIteratorRepresentations();
        try graph.finalizeGeneratedIteratorIdentities();
        try graph.bindGeneratedIteratorAuthoritativeTypes(&generated_types_by_identity);
        try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_type_store_hits);
        try graph.freezeRelations();
        var sealer = GraphTypeFinals.initWithGeneratedTypeInterner(graph, &generated_types_by_identity);
        defer sealer.deinit();
        try std.testing.expectEqual(authoritative_root, try sealer.sealNode(built.root));
        try std.testing.expectEqual(authoritative_backing, try sealer.sealNode(built.backing));
    }

    // A durable generated type can enter a fresh lowering run through an
    // imported compound before this in-session identity table has seen it.
    // That import seeds the table, and an equal graph-owned producer binds to
    // the complete imported tree before either root is sealed.
    generated_types_by_identity.clearRetainingCapacity();
    {
        const graph = try InstGraph.create(gpa, &type_store, &name_store);
        defer graph.destroy();
        const imported_root = try graph.importMono(authoritative_root);
        const built = try Build.inGraph(graph, named_type, public_def);
        try graph.finalizeGeneratedIteratorRepresentations();
        try graph.finalizeGeneratedIteratorIdentities();
        try graph.bindGeneratedIteratorAuthoritativeTypes(&generated_types_by_identity);
        try std.testing.expect(graph.sameClass(imported_root, built.root));
        try graph.freezeRelations();
        var sealer = GraphTypeFinals.initWithGeneratedTypeInterner(graph, &generated_types_by_identity);
        defer sealer.deinit();
        try std.testing.expectEqual(authoritative_root, try sealer.sealNode(built.root));
        try std.testing.expectEqual(authoritative_backing, try sealer.sealNode(built.backing));
    }
}

test "generated iterator identity includes nested graph-local producer evidence" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x66} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(14) };
    const public_def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = public_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
        .declared_order = &.{},
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const source = try graph.newNode(.{ .list = item });

    const GeneratedNodeBuilder = struct {
        fn add(
            active_graph: *InstGraph,
            source_contract: InstIteratorPublicSource,
            item_node: NodeId,
            components: []const NodeId,
            evidence_byte: u8,
        ) Allocator.Error!NodeId {
            var def = source_contract.def;
            def.iterator_kind = .map;
            var evidence: names.TypeDigest = undefined;
            @memset(&evidence.bytes, evidence_byte);
            return try active_graph.newNode(.{ .named = .{
                .named_type = source_contract.named_type,
                .def = def,
                .kind = source_contract.kind,
                .builtin_owner = source_contract.builtin_owner,
                .args = try active_graph.arena().dupe(NodeId, &.{item_node}),
                .backing = .{
                    .node = try active_graph.newNode(.empty_record),
                    .use = .runtime_layout_only,
                    .authority = .generated_private,
                },
                .generated_iterator = .{
                    .callable_evidence = evidence,
                    .components = try active_graph.arena().dupe(NodeId, components),
                    .public_source = source_contract,
                },
            } });
        }
    };

    const equivalent_item = try graph.newNode(.{ .primitive = .u64 });
    const equivalent_source = try graph.newNode(.{ .list = equivalent_item });
    const inner_a = try GeneratedNodeBuilder.add(graph, public_source, item, &.{source}, 0xA1);
    const inner_b = try GeneratedNodeBuilder.add(graph, public_source, item, &.{source}, 0xB2);
    const inner_equivalent = try GeneratedNodeBuilder.add(graph, public_source, equivalent_item, &.{equivalent_source}, 0xA1);
    const outer_a = try GeneratedNodeBuilder.add(graph, public_source, item, &.{inner_a}, 0xC3);
    const outer_b = try GeneratedNodeBuilder.add(graph, public_source, item, &.{inner_b}, 0xC3);
    const outer_equivalent = try GeneratedNodeBuilder.add(graph, public_source, equivalent_item, &.{inner_equivalent}, 0xC3);
    for ([_]NodeId{ inner_a, inner_b, inner_equivalent, outer_a, outer_b, outer_equivalent }) |node| {
        try graph.registerGeneratedIterator(node);
    }

    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();

    const inner_a_named = graph.content(inner_a).named;
    const inner_b_named = graph.content(inner_b).named;
    const inner_equivalent_named = graph.content(inner_equivalent).named;
    const outer_a_named = graph.content(outer_a).named;
    const outer_b_named = graph.content(outer_b).named;
    const outer_equivalent_named = graph.content(outer_equivalent).named;
    try std.testing.expect(!optionalInstDigestEql(inner_a_named.def.generated, inner_b_named.def.generated));
    try std.testing.expect(optionalInstDigestEql(inner_a_named.def.generated, inner_equivalent_named.def.generated));
    try std.testing.expect(!optionalInstDigestEql(outer_a_named.def.generated, outer_b_named.def.generated));
    try std.testing.expect(optionalInstDigestEql(outer_a_named.def.generated, outer_equivalent_named.def.generated));
    try std.testing.expectEqual(@as(usize, 1), outer_a_named.args.len);
    try std.testing.expectEqual(@as(usize, 1), outer_b_named.args.len);
}

test "generated iterator identity is independent of dense module interning order" {
    const Build = struct {
        fn digest(allocator: Allocator, intern_dummy_first: bool) Allocator.Error!names.TypeDigest {
            var type_store = Type.Store.init(allocator);
            defer type_store.deinit();
            var name_store = names.NameStore.init(allocator);
            defer name_store.deinit();
            const graph = try InstGraph.create(allocator, &type_store, &name_store);
            defer graph.destroy();

            if (intern_dummy_first) {
                _ = try name_store.internModuleIdentity(&([_]u8{0x12} ** 32));
            }
            const module_identity = try name_store.internModuleIdentity(&([_]u8{0x34} ** 32));
            const type_name = try name_store.internTypeName("Iter");
            const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(16) };
            const public_def: Type.TypeDef = .{
                .module = module_identity,
                .type_name = type_name,
                .source_decl = 17,
            };
            const public_source: InstIteratorPublicSource = .{
                .named_type = named_type,
                .def = public_def,
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .backing = .{
                    .node = try graph.newNode(.empty_record),
                    .use = .runtime_layout_only,
                },
                .declared_order = &.{},
            };
            const item = try graph.newNode(.{ .primitive = .u64 });
            var generated_def = public_def;
            generated_def.iterator_kind = .list;
            const generated = try graph.newNode(.{ .named = .{
                .named_type = named_type,
                .def = generated_def,
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{item}),
                .backing = .{
                    .node = try graph.newNode(.empty_record),
                    .use = .runtime_layout_only,
                    .authority = .generated_private,
                },
                .generated_iterator = .{
                    .callable_evidence = null,
                    .components = try graph.arena().dupe(NodeId, &.{try graph.newNode(.{ .list = item })}),
                    .public_source = public_source,
                },
            } });
            try graph.registerGeneratedIterator(generated);
            try graph.finalizeGeneratedIteratorRepresentations();
            try graph.finalizeGeneratedIteratorIdentities();
            return graph.content(generated).named.def.generated orelse
                Common.invariant("generated iterator test identity was not finalized");
        }
    };

    const direct = try Build.digest(std.testing.allocator, false);
    const shifted = try Build.digest(std.testing.allocator, true);
    try std.testing.expect(optionalInstDigestEql(direct, shifted));
}

test "generated iterator identity hashes a nested producer chain linearly" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x67} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(15) };
    const public_def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = public_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
        .declared_order = &.{},
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    var previous = try graph.newNode(.{ .list = item });
    const chain_len = 12;
    var chain: [chain_len]NodeId = undefined;
    for (&chain) |*slot| {
        var def = public_def;
        def.iterator_kind = .map;
        slot.* = try graph.newNode(.{ .named = .{
            .named_type = named_type,
            .def = def,
            .kind = .@"opaque",
            .builtin_owner = .iter,
            .args = try graph.arena().dupe(NodeId, &.{item}),
            .backing = .{
                .node = try graph.newNode(.empty_record),
                .use = .runtime_layout_only,
                .authority = .generated_private,
            },
            .generated_iterator = .{
                .callable_evidence = null,
                .components = try graph.arena().dupe(NodeId, &.{previous}),
                .public_source = public_source,
            },
        } });
        try graph.registerGeneratedIterator(slot.*);
        previous = slot.*;
    }

    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();

    for (chain) |node| try std.testing.expect(graph.content(node).named.def.generated != null);
    try std.testing.expect(diagnostics.generated_identity_nodes_hashed <= chain_len * 3);
    try std.testing.expect(diagnostics.generated_identity_cache_hits >= chain_len - 1);

    const request = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{chain[chain.len - 1]}),
        .ret = item,
    } });
    const shape = try graph.openFunctionInterfaceShape(request);
    try std.testing.expect(shape.resolved);
    try std.testing.expect(shape.bytes.len < 256);
}

test "generated iterator depth visits wide graphs without a size cutoff" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x64} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(5) };
    const public_backing = try graph.newNode(.empty_record);
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const source = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_kind = .single,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{},
            .public_source = public_source,
        },
    } });

    // Put the only iterator-bearing child after the former 64-node walk
    // budget. Graph width must not change the adapter's representation.
    const wide_children = try graph.arena().alloc(NodeId, 65);
    for (wide_children[0..64]) |*child| {
        child.* = try graph.newNode(.{ .primitive = .u64 });
    }
    wide_children[64] = source;
    const wide_component = try graph.newNode(.{ .tuple = wide_children });
    const adapter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = .{
            .module = module_identity,
            .type_name = type_name,
            .iterator_kind = .concat,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{wide_component},
            .public_source = public_source,
        },
    } });
    try graph.registerGeneratedIterator(source);
    try graph.registerGeneratedIterator(adapter);

    try graph.finalizeGeneratedIteratorRepresentations();

    const finalized = graph.content(adapter).named.def;
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, finalized.iterator_representation);
    try std.testing.expectEqual(@as(u8, 2), finalized.iterator_depth);
}

test "recursive join keeps graph-owned iterator provenance over a finished Monotype" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x65} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(6) };
    const public_def: Type.TypeDef = .{
        .module = module_identity,
        .type_name = type_name,
        .iterator_topology = .{
            .len_field = try name_store.internRecordFieldLabel("len"),
            .step_field = try name_store.internRecordFieldLabel("step"),
            .known_tag = try name_store.internTagLabel("Known"),
            .unknown_tag = try name_store.internTagLabel("Unknown"),
            .done_tag = try name_store.internTagLabel("Done"),
            .one_tag = try name_store.internTagLabel("One"),
            .skip_tag = try name_store.internTagLabel("Skip"),
            .item_field = try name_store.internRecordFieldLabel("item"),
            .rest_field = try name_store.internRecordFieldLabel("rest"),
        },
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const public_backing = try graph.newNode(.empty_record);
    const public_source: InstIteratorPublicSource = .{
        .named_type = named_type,
        .def = public_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
        .declared_order = &.{},
    };

    var finished_def = public_def;
    finished_def.generated = .{ .bytes = [_]u8{0xA5} ** 32 };
    finished_def.iterator_representation = .minted;
    finished_def.iterator_kind = .list;
    finished_def.iterator_depth = 1;
    const finished = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = finished_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    var owned_def = public_def;
    owned_def.iterator_representation = .minted;
    owned_def.iterator_kind = .list;
    owned_def.iterator_depth = 1;
    const owned = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = owned_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = &.{},
            .public_source = public_source,
        },
    } });

    var adapter_def = public_def;
    adapter_def.iterator_representation = .minted;
    adapter_def.iterator_kind = .map;
    adapter_def.iterator_depth = 2;
    const adapter = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = adapter_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .components = try graph.arena().dupe(NodeId, &.{owned}),
            .public_source = public_source,
        },
    } });

    // This order is significant: the finished type is the left side of the
    // recursive join, but only the graph-owned side can author the final
    // forced-dynamic representation.
    try graph.registerGeneratedIterator(owned);
    try graph.registerGeneratedIterator(adapter);
    try graph.markRecursiveValueSlot(finished);
    try graph.unify(finished, owned);
    try graph.finalizeGeneratedIteratorRepresentations();

    const finalized = graph.content(finished).named;
    try std.testing.expect(finalized.generated_iterator != null);
    try std.testing.expectEqual(Type.IteratorRepresentation.forced_dynamic, finalized.def.iterator_representation);
    try std.testing.expectEqual(Type.IteratorKind.forced_dynamic, finalized.def.iterator_kind);
    const finalized_adapter = graph.content(adapter).named;
    try std.testing.expectEqual(Type.IteratorRepresentation.forced_dynamic, finalized_adapter.def.iterator_representation);
    try std.testing.expectEqual(Type.IteratorKind.forced_dynamic, finalized_adapter.def.iterator_kind);
}

test "opaque interface relation preserves nested generated-private backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xBC} ** 32));
    const type_name = try name_store.internTypeName("NestedEvidence");
    const inner_type_name = try name_store.internTypeName("InnerEvidence");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(2) };
    const inner_named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(3) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const inner_def: Type.TypeDef = .{ .module = module_identity, .type_name = inner_type_name };
    const public_inner_backing = try graph.newNode(.empty_record);
    const private_inner_backing = try graph.newNode(.empty_record);
    const public_arg = try graph.newNode(.{ .named = .{
        .named_type = inner_named_type,
        .def = inner_def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = public_inner_backing, .use = .runtime_layout_only },
    } });
    const private_arg = try graph.newNode(.{ .named = .{
        .named_type = inner_named_type,
        .def = inner_def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{
            .node = private_inner_backing,
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const public_backing = try graph.newNode(.empty_record);
    const private_backing = try graph.newNode(.empty_record);
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{public_arg}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    } });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{private_arg}),
        .backing = .{
            .node = private_backing,
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });

    const public_list = try graph.newNode(.{ .list = public });
    const private_list = try graph.newNode(.{ .list = private });
    const public_tuple = try graph.newNode(.{ .tuple = try graph.arena().dupe(NodeId, &.{public}) });
    const private_tuple = try graph.newNode(.{ .tuple = try graph.arena().dupe(NodeId, &.{private}) });
    const public_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{public_list}),
        .ret = public_tuple,
    } });
    const private_fn = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{private_list}),
        .ret = private_tuple,
    } });

    _ = try graph.applyProducedTypeToRequest(public_fn, private_fn);

    try std.testing.expect(!graph.sameClass(public_fn, private_fn));
    try std.testing.expect(!graph.sameClass(public_list, private_list));
    try std.testing.expect(!graph.sameClass(public_tuple, private_tuple));
    try std.testing.expect(graph.sameClass(try graph.listElementNode(public_list), public));
    try std.testing.expect(graph.sameClass(try graph.listElementNode(private_list), private));
    try std.testing.expect(!graph.sameClass(public, private));
    try std.testing.expect(!graph.sameClass(public_backing, private_backing));
    try std.testing.expect(!graph.sameClass(public_arg, private_arg));
    try std.testing.expect(!graph.sameClass(public_inner_backing, private_inner_backing));
    const retained_public = graph.content(public).named;
    const retained_private = graph.content(private).named;
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, retained_private.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, graph.content(public_arg).named.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(private_arg).named.backing.?.authority);
}

test "produced representation join keeps exact children at their compound positions" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xC1} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(20) };
    const public_def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    var private_def = public_def;
    private_def.generated = .{ .bytes = [_]u8{0xC2} ** 32 };
    private_def.iterator_representation = .minted;
    private_def.iterator_kind = .list;
    private_def.iterator_depth = 1;
    const item = try graph.newNode(.{ .primitive = .u64 });

    const Pair = struct {
        public: NodeId,
        private: NodeId,

        fn add(
            active_graph: *InstGraph,
            public_type_def: Type.TypeDef,
            private_type_def: Type.TypeDef,
            type_ref: Type.NamedType,
            item_node: NodeId,
        ) Allocator.Error!@This() {
            return .{
                .public = try active_graph.newNode(.{ .named = .{
                    .named_type = type_ref,
                    .def = public_type_def,
                    .kind = .@"opaque",
                    .builtin_owner = .iter,
                    .args = try active_graph.arena().dupe(NodeId, &.{item_node}),
                    .backing = .{
                        .node = try active_graph.newNode(.empty_record),
                        .use = .runtime_layout_only,
                    },
                } }),
                .private = try active_graph.newNode(.{ .named = .{
                    .named_type = type_ref,
                    .def = private_type_def,
                    .kind = .@"opaque",
                    .builtin_owner = .iter,
                    .args = try active_graph.arena().dupe(NodeId, &.{item_node}),
                    .backing = .{
                        .node = try active_graph.newNode(.empty_record),
                        .use = .runtime_layout_only,
                        .authority = .generated_private,
                    },
                } }),
            };
        }
    };

    const a = try Pair.add(graph, public_def, private_def, named_type, item);
    const b = try Pair.add(graph, public_def, private_def, named_type, item);
    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");
    const left = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{
            .{ .name = a_name, .ty = a.private },
            .{ .name = b_name, .ty = b.public },
        }),
        .ext = try graph.newNode(.empty_record),
    } });
    const right = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{
            .{ .name = a_name, .ty = a.public },
            .{ .name = b_name, .ty = b.private },
        }),
        .ext = try graph.newNode(.empty_record),
    } });

    const joined = try graph.joinProducedTypeRepresentations(left, right);
    const joined_fields = (try graph.recordConstructionNodes(joined)).fields;

    try std.testing.expect(!graph.sameClass(joined, left));
    try std.testing.expect(!graph.sameClass(joined, right));
    try std.testing.expectEqual(@as(usize, 2), joined_fields.len);
    try std.testing.expectEqual(a_name, joined_fields[0].name);
    try std.testing.expectEqual(b_name, joined_fields[1].name);
    try std.testing.expect(graph.sameClass(joined_fields[0].ty, a.private));
    try std.testing.expect(graph.sameClass(joined_fields[1].ty, b.private));
    try std.testing.expect(!graph.sameClass(a.public, a.private));
    try std.testing.expect(!graph.sameClass(b.public, b.private));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, graph.content(a.public).named.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(a.private).named.backing.?.authority);

    const left_cycle = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(left_cycle, .{ .tuple = try graph.arena().dupe(NodeId, &.{ a.private, left_cycle }) });
    const right_cycle = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(right_cycle, .{ .tuple = try graph.arena().dupe(NodeId, &.{ a.public, right_cycle }) });

    const joined_cycle = try graph.joinProducedTypeRepresentations(left_cycle, right_cycle);
    const joined_items = try graph.tupleItemNodes(joined_cycle);
    try std.testing.expectEqual(@as(usize, 2), joined_items.len);
    try std.testing.expect(graph.sameClass(joined_items[0], a.private));
    try std.testing.expect(graph.sameClass(joined_items[1], joined_cycle));
}

test "produced representation join retains restrictive nominal backing visibility" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xD1} ** 32));
    const type_name = try name_store.internTypeName("Date");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(21) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const backing = try graph.newNode(.{ .primitive = .i64 });

    const inspectable = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = backing, .use = .inspectable },
    } });
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    } });

    const joined = try graph.joinProducedTypeRepresentations(inspectable, public);
    const joined_named = graph.content(joined).named;
    try std.testing.expectEqual(Type.BackingUse.runtime_layout_only, joined_named.backing.?.use);
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, joined_named.backing.?.authority);
    try std.testing.expect(graph.sameClass(joined_named.backing.?.node, backing));
}

test "checked mapping places narrow closed rows in wider exact representations" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const first_tag = try name_store.internTagLabel("First");
    const second_tag = try name_store.internTagLabel("Second");
    const narrow_tags = try graph.arena().dupe(InstTag, &.{.{
        .name = first_tag,
        .checked_name = first_tag,
        .payloads = &.{},
    }});
    const wide_tags = try graph.arena().dupe(InstTag, &.{
        .{ .name = first_tag, .checked_name = first_tag, .payloads = &.{} },
        .{ .name = second_tag, .checked_name = second_tag, .payloads = &.{} },
    });
    const narrow_tags_node = try graph.newNode(.{ .tag_union = .{
        .tags = narrow_tags,
        .ext = try graph.newNode(.empty_tag_union),
    } });
    const wide_tags_node = try graph.newNode(.{ .tag_union = .{
        .tags = wide_tags,
        .ext = try graph.newNode(.empty_tag_union),
    } });
    const narrow_fn = try graph.newNode(.{ .func = .{
        .args = &.{},
        .ret = narrow_tags_node,
    } });
    const wide_fn = try graph.newNode(.{ .func = .{
        .args = &.{},
        .ret = wide_tags_node,
    } });

    _ = try graph.applyCheckedTypeMapping(narrow_fn, wide_fn);
    try std.testing.expect(!graph.sameClass(narrow_tags_node, wide_tags_node));
    try std.testing.expectEqual(@as(usize, 1), (try graph.tagRowNodes(narrow_tags_node)).tags.len);
    try std.testing.expectEqual(@as(usize, 2), (try graph.tagRowNodes(wide_tags_node)).tags.len);

    // Storing the narrower value in the wider row is an explicit common-
    // representation boundary. Both live cells adopt the wide layout so the
    // already-emitted local definition and its constructor use agree.
    try graph.applyCompoundStorageRepresentation(wide_tags_node, narrow_tags_node);
    try std.testing.expect(graph.sameClass(narrow_tags_node, wide_tags_node));
    try std.testing.expectEqual(@as(usize, 2), (try graph.tagRowNodes(narrow_tags_node)).tags.len);

    const first_field = try name_store.internRecordFieldLabel("first");
    const second_field = try name_store.internRecordFieldLabel("second");
    const value = try graph.newNode(.{ .primitive = .u64 });
    const narrow_record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = first_field, .ty = value }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const wide_record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{
            .{ .name = first_field, .ty = value },
            .{ .name = second_field, .ty = value },
        }),
        .ext = try graph.newNode(.empty_record),
    } });

    _ = try graph.applyCheckedTypeMapping(narrow_record, wide_record);
    try std.testing.expect(!graph.sameClass(narrow_record, wide_record));
    try std.testing.expectEqual(@as(usize, 1), (try graph.recordNodes(narrow_record)).fields.len);
    try std.testing.expectEqual(@as(usize, 2), (try graph.recordNodes(wide_record)).fields.len);
}

test "issue 9647: unresolved tag row extension absorbs rest without allocating a rest node" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const shared_name = try name_store.internTagLabel("Shared");
    const extra_name = try name_store.internTagLabel("Extra");

    const left_ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    const right_ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });

    const left_tags = try graph.arena().alloc(InstTag, 1);
    left_tags[0] = .{ .name = shared_name, .checked_name = shared_name, .payloads = try graph.arena().alloc(NodeId, 0) };

    const right_tags = try graph.arena().alloc(InstTag, 2);
    right_tags[0] = .{ .name = shared_name, .checked_name = shared_name, .payloads = try graph.arena().alloc(NodeId, 0) };
    right_tags[1] = .{ .name = extra_name, .checked_name = extra_name, .payloads = try graph.arena().alloc(NodeId, 0) };

    const left = try graph.newNode(.{ .tag_union = .{ .tags = left_tags, .ext = left_ext } });
    const right = try graph.newNode(.{ .tag_union = .{ .tags = right_tags, .ext = right_ext } });
    const before_nodes = graph.nodes.items.len;

    try graph.unify(left, right);

    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
    const left_ext_content = graph.content(left_ext);
    if (left_ext_content != .tag_union) return error.TestUnexpectedResult;
    const rest = left_ext_content.tag_union;
    try std.testing.expectEqual(@as(usize, 1), rest.tags.len);
    try std.testing.expectEqual(extra_name, rest.tags[0].name);
    try std.testing.expectEqual(graph.find(right_ext), graph.find(rest.ext));
}

test "issue 9647: same nominal backing wrapper resolves to structural backing once" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Role");
    const tag_name = try name_store.internTagLabel("Tile");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const empty_args = try graph.arena().alloc(NodeId, 0);

    const empty = try graph.newNode(.empty_tag_union);
    const backing_tags = try graph.arena().alloc(InstTag, 1);
    backing_tags[0] = .{ .name = tag_name, .checked_name = tag_name, .payloads = try graph.arena().alloc(NodeId, 0) };
    const structural_backing = try graph.newNode(.{ .tag_union = .{ .tags = backing_tags, .ext = empty } });

    const inner_named = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = empty_args,
        .backing = .{ .node = structural_backing, .use = .inspectable },
    } });
    const outer_named = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = empty_args,
        .backing = .{ .node = inner_named, .use = .inspectable },
    } });

    const other_tags = try graph.arena().alloc(InstTag, 1);
    other_tags[0] = .{ .name = tag_name, .checked_name = tag_name, .payloads = try graph.arena().alloc(NodeId, 0) };
    const other = try graph.newNode(.{ .tag_union = .{ .tags = other_tags, .ext = empty } });
    const before_nodes = graph.nodes.items.len;

    try graph.unify(outer_named, other);

    try std.testing.expectEqual(before_nodes + 1, graph.nodes.items.len);
    const outer_content = graph.content(outer_named);
    if (outer_content != .named) return error.TestUnexpectedResult;
    const compressed = outer_content.named;
    try std.testing.expectEqual(structural_backing, compressed.backing.?.node);
}

test "issue 9647: recursive nominal backing cycle is not chased as structural backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const type_name = try name_store.internTypeName("Recursive");
    const tag_name = try name_store.internTagLabel("Wrap");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(2) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };

    const nominal = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(nominal, .{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    } });

    const empty = try graph.newNode(.empty_tag_union);
    const tags = try graph.arena().alloc(InstTag, 1);
    tags[0] = .{ .name = tag_name, .checked_name = tag_name, .payloads = try graph.arena().alloc(NodeId, 0) };
    const structural = try graph.newNode(.{ .tag_union = .{ .tags = tags, .ext = empty } });
    const before_nodes = graph.nodes.items.len;

    try graph.unify(nominal, structural);

    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
    try std.testing.expectEqual(graph.find(nominal), graph.find(structural));
}

test "recursive nominal backing can meet an alias to that nominal" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const nominal_name = try name_store.internTypeName("Role");
    const alias_name = try name_store.internTypeName("Wrapper.Role");
    const nominal_type: Type.NamedType = .{ .module = .{}, .ty = @enumFromInt(3) };
    const alias_type: Type.NamedType = .{ .module = .{}, .ty = @enumFromInt(4) };
    const nominal_def: Type.TypeDef = .{ .module = module_identity, .type_name = nominal_name };
    const alias_def: Type.TypeDef = .{ .module = module_identity, .type_name = alias_name };

    const nominal = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.setContent(nominal, .{ .named = .{
        .named_type = nominal_type,
        .def = nominal_def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    } });

    const alias = try graph.newNode(.{ .named = .{
        .named_type = alias_type,
        .def = alias_def,
        .kind = .alias,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    } });

    const before_nodes = graph.nodes.items.len;
    try graph.unify(nominal, alias);

    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
    try std.testing.expectEqual(nominal, graph.find(nominal));
    try std.testing.expectEqual(alias, graph.find(alias));
}
