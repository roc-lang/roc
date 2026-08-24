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

/// Union-find identity for one instantiated checked field-presence variable.
pub const FieldKindId = enum(u32) { _ };

/// Checked field-kind evidence carried independently from its runtime slot.
/// `sealed` is reserved for already-materialized Monotypes and generated
/// records whose kind has already been consumed into `ty`.
pub const InstFieldKind = union(enum) {
    sealed,
    required,
    optional,
    defaulted: Type.FieldDefault,
    undetermined: FieldKindId,
};

/// Concrete field-presence evidence selected during one specialization.
pub const ResolvedFieldKind = union(enum) {
    required,
    optional,
    defaulted: Type.FieldDefault,

    pub fn defaultIdentity(self: ResolvedFieldKind) ?Type.FieldDefault {
        return switch (self) {
            .defaulted => |default| default,
            .required, .optional => null,
        };
    }
};

const FieldKindNode = struct {
    parent: FieldKindId,
    rank: u8 = 0,
    resolved: ?ResolvedFieldKind = null,
    cells: ?FieldKindCells = null,
};

/// The exact cells whose representation is selected by one generalized field
/// kind. Producers register these when they instantiate the field; relation
/// freeze consumes them if no earlier specialization evidence selected a kind.
const FieldKindCells = struct {
    slot: NodeId,
    value: NodeId,
};

/// Record field inside an instantiation-graph row. `default` carries the
/// monotype `??` default identity through instantiation unchanged: rows
/// disagreeing about defaults are distinct monotypes, so a graph merge of
/// two rows that both name a field always sees identical defaults.
pub const InstField = struct {
    name: names.RecordFieldNameId,
    /// Final runtime slot node. Optional fields use the tagged slot; required
    /// and defaulted fields use the inline value node.
    ty: NodeId,
    /// Source value type before the field kind is consumed into `ty`. Present
    /// on checked optional and undetermined fields; absent means `ty` itself.
    value_ty: ?NodeId = null,
    kind: InstFieldKind = .sealed,
    default: ?Type.FieldDefault,
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

/// Declared nominal fields while a named record is still in the instantiation
/// graph.
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
    /// Declared fields for a nominal/opaque record backing (empty otherwise).
    /// Padding field types are graph nodes so sealing maps them to immutable
    /// type ids with the rest of the named type.
    declared_order: []const InstDeclaredField = &.{},
};

/// Graph-owned data for a private iterator representation before sealing.
pub const InstGeneratedIterator = struct {
    callable_evidence: ?names.TypeDigest,
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
    iterator_interface_scans: u64 = 0,
    iterator_interface_cache_hits: u64 = 0,
    iterator_interface_nodes_visited: u64 = 0,
    generated_private_scans: u64 = 0,
    generated_private_cache_hits: u64 = 0,
    generated_private_nodes_visited: u64 = 0,
    finished_mono_scans: u64 = 0,
    finished_mono_nodes_visited: u64 = 0,
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

/// The explicit runtime-slot and source-value cells selected by one optional
/// checked field-access segment.
pub const OptionalFieldAccessNodes = struct {
    slot: NodeId,
    value: NodeId,
};

/// Graph-native flattened tag-row variants. The extension remains internal:
/// callers consume the explicit labels and payload cells rather than
/// reconstructing or mutating row openness.
pub const TagRowNodes = struct {
    tags: []const InstTag,
};

/// Whether one graph relation may replay checker-approved record-construction
/// width absorption or must preserve exact closed-row width.
pub const RowWidthRelation = enum(u8) {
    exact,
    construction,
};

const NodePair = struct {
    left: NodeId,
    right: NodeId,
    row_width: RowWidthRelation = .exact,
};

const RelationStamp = struct {
    left: NodeId,
    left_version: u32,
    right: NodeId,
    right_version: u32,
    row_width: RowWidthRelation,
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

const ContainmentDependency = struct {
    node: NodeId,
    root: NodeId,
    version: u32,
};

/// One containment query's memoized answer for one graph root, with the exact
/// nodes that answer depends on. Each query keeps its own dependency list: the
/// two queries stop descending at different nodes, so sharing one list would
/// make each answer's validity scan cover the other's nodes and let a version
/// bump on nodes only one query visited invalidate both.
const ContainmentQueryCache = struct {
    valid: bool = false,
    result: bool = false,
    dependencies: std.ArrayList(ContainmentDependency) = .empty,
};

const ContainmentCacheEntry = struct {
    generated_private: ContainmentQueryCache = .{},
    iterator_interface: ContainmentQueryCache = .{},

    fn forQuery(self: *ContainmentCacheEntry, comptime query: ContainmentQuery) *ContainmentQueryCache {
        return switch (query) {
            .generated_private => &self.generated_private,
            .iterator_interface => &self.iterator_interface,
        };
    }

    fn deinit(self: *ContainmentCacheEntry, allocator: Allocator) void {
        self.generated_private.dependencies.deinit(allocator);
        self.iterator_interface.dependencies.deinit(allocator);
    }
};

const ContainmentQuery = enum {
    generated_private,
    iterator_interface,
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
    field_kinds: std.ArrayList(FieldKindNode),
    versions: std.ArrayList(u32),
    /// Intrusive chain of permanent node ids in each live union class. Draft
    /// request lookup indexes an open function under one permanent interface
    /// node and probes the current class members, so later unions never stale
    /// the key. Roots own the head/tail; every node owns one next link.
    class_member_next: std.ArrayList(?NodeId),
    class_member_head: std.ArrayList(NodeId),
    class_member_tail: std.ArrayList(NodeId),
    processed_relations: std.AutoHashMap(RelationStamp, void),
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
    /// Exact source function node from which each generated-private request
    /// function was constructed. A generic source interface may itself carry
    /// upstream generated-private arguments; retaining that producer node is
    /// what lets the callee instantiate those relations without reconstruction.
    request_source_interfaces: std.ArrayList(?NodeId),
    /// Minted iterator roots whose relation graph proved that retaining the
    /// minted tier would create a recursive component identity. The raw node
    /// remains valid across later unions; finalization resolves it to the live
    /// class and constructs the single forced-dynamic fixed point.
    forced_dynamic_iterator_roots: std.ArrayList(NodeId),
    /// Permanent value-slot nodes that differ from the corresponding source
    /// slot on an explicit recursive edge. Function recursion and loop
    /// feedback both append here; a later minted join touching one of these
    /// slots proves that recursion grows the representation rather than merely
    /// recurring over a fixed iterator.
    recursive_argument_slots: std.ArrayList(NodeId),
    /// Shared allocation-free scratch and cache for exact structural
    /// containment. The two queries share one conservative dependency list,
    /// while each walk can stop as soon as its requested property is found.
    containment_pending: std.ArrayList(NodeId),
    containment_visit_epochs: std.ArrayList(u32),
    containment_visit_epoch: u32,
    containment_cache: collections.DenseMap(NodeId, ContainmentCacheEntry),
    /// Scratch epoch marks for `compactRowParents`, indexed by node id. A
    /// slot carrying the current epoch means its class root is already kept
    /// in the list being compacted.
    row_parent_seen_epochs: std.ArrayList(u64),
    row_parent_seen_epoch: u64,
    /// Pools for the visited sets the graph's walks create per query. Fresh
    /// maps re-allocate and re-zero sparse chunks across the node/type ID
    /// domains on every walk; pooled maps keep their chunks.
    node_set_pool: collections.DenseMapPool(NodeId, void),
    type_set_pool: collections.DenseMapPool(Type.TypeId, void),
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
            .field_kinds = .empty,
            .versions = .empty,
            .class_member_next = .empty,
            .class_member_head = .empty,
            .class_member_tail = .empty,
            .processed_relations = std.AutoHashMap(RelationStamp, void).init(allocator),
            .node_snapshots = collections.DenseMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .current_snapshots = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .current_snapshots_dirty = false,
            .linked_type_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .imported_monos = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .row_exts = .empty,
            .row_parents = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .nominal_backings = std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingCacheContext, 80).init(allocator),
            .request_source_interfaces = .empty,
            .forced_dynamic_iterator_roots = .empty,
            .recursive_argument_slots = .empty,
            .containment_pending = .empty,
            .containment_visit_epochs = .empty,
            .containment_visit_epoch = 0,
            .containment_cache = collections.DenseMap(NodeId, ContainmentCacheEntry).init(allocator),
            .row_parent_seen_epochs = .empty,
            .row_parent_seen_epoch = 0,
            .node_set_pool = collections.DenseMapPool(NodeId, void).init(allocator),
            .type_set_pool = collections.DenseMapPool(Type.TypeId, void).init(allocator),
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
        self.request_source_interfaces.deinit(allocator);
        self.forced_dynamic_iterator_roots.deinit(allocator);
        self.recursive_argument_slots.deinit(allocator);
        self.containment_pending.deinit(allocator);
        self.containment_visit_epochs.deinit(allocator);
        self.row_parent_seen_epochs.deinit(allocator);
        self.node_set_pool.deinit();
        self.type_set_pool.deinit();
        var containment_entries = self.containment_cache.valueIterator();
        while (containment_entries.next()) |entry| {
            entry.deinit(allocator);
        }
        self.containment_cache.deinit();
        self.row_parents.deinit();
        self.row_exts.deinit(allocator);
        self.imported_monos.deinit();
        self.linked_type_nodes.deinit();
        self.processed_relations.deinit();
        self.class_member_tail.deinit(allocator);
        self.class_member_head.deinit(allocator);
        self.class_member_next.deinit(allocator);
        self.versions.deinit(allocator);
        self.nodes.deinit(allocator);
        self.field_kinds.deinit(allocator);
        self.arena_impl.deinit();
        allocator.destroy(self);
    }

    pub fn arena(self: *InstGraph) Allocator {
        return self.arena_impl.allocator();
    }

    pub fn newUndeterminedFieldKind(self: *InstGraph) Allocator.Error!FieldKindId {
        const id: FieldKindId = @enumFromInt(self.field_kinds.items.len);
        try self.field_kinds.append(self.allocator, .{ .parent = id });
        return id;
    }

    pub fn registerUndeterminedFieldKindCells(
        self: *InstGraph,
        raw: FieldKindId,
        slot: NodeId,
        value: NodeId,
    ) void {
        self.requireRelationProduction();
        const root = self.findFieldKind(raw);
        const node = &self.field_kinds.items[@intFromEnum(root)];
        if (node.cells != null) {
            Common.invariant("instantiation field kind registered its representation cells more than once");
        }
        node.cells = .{ .slot = slot, .value = value };
    }

    fn findFieldKind(self: *InstGraph, raw: FieldKindId) FieldKindId {
        var root = raw;
        while (self.field_kinds.items[@intFromEnum(root)].parent != root) {
            root = self.field_kinds.items[@intFromEnum(root)].parent;
        }
        var current = raw;
        while (current != root) {
            const next = self.field_kinds.items[@intFromEnum(current)].parent;
            self.field_kinds.items[@intFromEnum(current)].parent = root;
            current = next;
        }
        return root;
    }

    fn mergeResolvedFieldKinds(left: ResolvedFieldKind, right: ResolvedFieldKind) ResolvedFieldKind {
        return switch (left) {
            .required => switch (right) {
                .required => .required,
                .defaulted => |default| .{ .defaulted = default },
                .optional => Common.invariant("instantiation unified required and optional record field kinds"),
            },
            .optional => switch (right) {
                .optional => .optional,
                .required, .defaulted => Common.invariant("instantiation unified optional and inline record field kinds"),
            },
            .defaulted => |left_default| switch (right) {
                .required => .{ .defaulted = left_default },
                .optional => Common.invariant("instantiation unified defaulted and optional record field kinds"),
                .defaulted => |right_default| if (instFieldDefaultEql(left_default, right_default))
                    ResolvedFieldKind{ .defaulted = left_default }
                else
                    Common.invariant("instantiation unified record field kinds with different defaults"),
            },
        };
    }

    fn constrainUndeterminedFieldKind(self: *InstGraph, raw: FieldKindId, resolved: ResolvedFieldKind) void {
        const root = self.findFieldKind(raw);
        const node = &self.field_kinds.items[@intFromEnum(root)];
        node.resolved = if (node.resolved) |existing|
            mergeResolvedFieldKinds(existing, resolved)
        else
            resolved;
    }

    fn unionUndeterminedFieldKinds(self: *InstGraph, left_raw: FieldKindId, right_raw: FieldKindId) FieldKindId {
        var left = self.findFieldKind(left_raw);
        var right = self.findFieldKind(right_raw);
        if (left == right) return left;
        if (self.field_kinds.items[@intFromEnum(left)].rank < self.field_kinds.items[@intFromEnum(right)].rank) {
            const temp = left;
            left = right;
            right = temp;
        }
        const right_state = self.field_kinds.items[@intFromEnum(right)].resolved;
        const right_cells = self.field_kinds.items[@intFromEnum(right)].cells;
        self.field_kinds.items[@intFromEnum(right)].parent = left;
        if (self.field_kinds.items[@intFromEnum(left)].rank == self.field_kinds.items[@intFromEnum(right)].rank) {
            self.field_kinds.items[@intFromEnum(left)].rank += 1;
        }
        if (self.field_kinds.items[@intFromEnum(left)].cells == null) {
            self.field_kinds.items[@intFromEnum(left)].cells = right_cells;
        }
        if (right_state) |resolved| self.constrainUndeterminedFieldKind(left, resolved);
        return self.findFieldKind(left);
    }

    pub fn resolvedFieldKind(self: *InstGraph, kind: InstFieldKind) ?ResolvedFieldKind {
        return switch (kind) {
            .sealed => null,
            .required => .required,
            .optional => .optional,
            .defaulted => |default| .{ .defaulted = default },
            .undetermined => |id| self.field_kinds.items[@intFromEnum(self.findFieldKind(id))].resolved,
        };
    }

    fn unifyFieldKinds(
        self: *InstGraph,
        left: InstFieldKind,
        left_default: ?Type.FieldDefault,
        right: InstFieldKind,
        right_default: ?Type.FieldDefault,
    ) InstFieldKind {
        if (left == .sealed and right == .sealed) {
            if (!instFieldDefaultEql(left_default, right_default)) {
                Common.invariant("instantiation unified sealed record fields with different defaults");
            }
            return .sealed;
        }

        const left_resolved = self.resolvedFieldKind(left) orelse if (left_default) |default|
            ResolvedFieldKind{ .defaulted = default }
        else
            null;
        const right_resolved = self.resolvedFieldKind(right) orelse if (right_default) |default|
            ResolvedFieldKind{ .defaulted = default }
        else
            null;

        switch (left) {
            .undetermined => |left_id| switch (right) {
                .undetermined => |right_id| {
                    const root = self.unionUndeterminedFieldKinds(left_id, right_id);
                    if (left_resolved) |resolved| self.constrainUndeterminedFieldKind(root, resolved);
                    if (right_resolved) |resolved| self.constrainUndeterminedFieldKind(root, resolved);
                    return .{ .undetermined = root };
                },
                .sealed => {
                    if (right_resolved) |resolved| self.constrainUndeterminedFieldKind(left_id, resolved);
                    return left;
                },
                .required, .optional, .defaulted => {
                    self.constrainUndeterminedFieldKind(left_id, right_resolved.?);
                    return left;
                },
            },
            .sealed => return right,
            .required, .optional, .defaulted => switch (right) {
                .undetermined => |right_id| {
                    self.constrainUndeterminedFieldKind(right_id, left_resolved.?);
                    return right;
                },
                .sealed => return left,
                .required, .optional, .defaulted => return switch (mergeResolvedFieldKinds(left_resolved.?, right_resolved.?)) {
                    .required => .required,
                    .optional => .optional,
                    .defaulted => |default| .{ .defaulted = default },
                },
            },
        }
    }

    /// Apply the explicit field-presence relation between two matched record
    /// fields without otherwise joining their rows. Deferred template
    /// interface replay uses this alongside the separate source-value and
    /// runtime-slot constructor relations.
    pub fn relateRecordFieldKind(
        self: *InstGraph,
        left: InstField,
        right: InstField,
    ) void {
        self.requireRelationProduction();
        _ = self.unifyFieldKinds(left.kind, left.default, right.kind, right.default);
    }

    pub fn registerRequestSourceInterface(
        self: *InstGraph,
        request_fn: NodeId,
        source_fn: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        if (!try self.containsGeneratedPrivate(request_fn)) {
            Common.invariant("registered private request interface contained no generated-private evidence");
        }
        const entry = &self.request_source_interfaces.items[@intFromEnum(request_fn)];
        if (entry.*) |existing| {
            if (self.find(existing) != self.find(source_fn)) {
                Common.invariant("generated-private request was registered with two source interfaces");
            }
        } else {
            entry.* = source_fn;
        }
    }

    pub fn requestSourceInterface(self: *InstGraph, request_fn: NodeId) ?NodeId {
        const source_fn = self.request_source_interfaces.items[@intFromEnum(request_fn)] orelse return null;
        return self.find(source_fn);
    }

    pub fn findGeneratedIterator(
        self: *InstGraph,
        public_node: NodeId,
        kind: Type.IteratorKind,
        components: []const NodeId,
        callable_evidence: ?names.TypeDigest,
    ) ?NodeId {
        const public_named = switch (self.content(public_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => return null,
        };
        if (public_named.args.len == 0) return null;
        for (self.nodes.items, 0..) |node_content, raw_index| {
            const candidate = switch (node_content) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => continue,
            };
            const provenance = candidate.generated_iterator orelse continue;
            if (candidate.def.iterator_kind != kind or
                !optionalInstDigestEql(provenance.callable_evidence, callable_evidence) or
                candidate.kind != public_named.kind or
                candidate.def.module != public_named.def.module or
                candidate.def.type_name != public_named.def.type_name or
                candidate.def.source_decl != public_named.def.source_decl or
                candidate.args.len != components.len + 1 or
                self.find(candidate.args[0]) != self.find(public_named.args[0]))
            {
                continue;
            }
            var matches = true;
            for (components, candidate.args[1..]) |component, stored| {
                if (self.find(component) != self.find(stored)) {
                    matches = false;
                    break;
                }
            }
            if (matches) return self.find(@enumFromInt(@as(u32, @intCast(raw_index))));
        }
        return null;
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

    /// Commit every generalized field kind that received no concrete
    /// specialization evidence. The field-kind producer recorded the exact
    /// runtime-slot/source-value pair, so required defaulting relates those
    /// cells directly without inspecting or reconstructing a row shape.
    fn finalizeUndeterminedFieldKinds(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        for (0..self.field_kinds.items.len) |raw_index| {
            const id: FieldKindId = @enumFromInt(raw_index);
            const root = self.findFieldKind(id);
            if (root != id) continue;
            const node = &self.field_kinds.items[raw_index];
            if (node.resolved != null) continue;
            const cells = node.cells orelse
                Common.invariant("unresolved instantiation field kind had no registered representation cells");
            node.resolved = .required;
            try self.unify(cells.slot, cells.value);
        }
    }

    /// Complete pending specialization defaults and prevent any later
    /// relation production. Final type sealing remains available after this
    /// transition.
    pub fn freezeRelations(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        try self.finalizeUndeterminedFieldKinds();
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

    pub fn unifyRecursiveFunctionInterface(
        self: *InstGraph,
        active_fn: NodeId,
        initial_active_arg_classes: []const ArgumentClassSnapshot,
        recursive_request: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const request = try self.functionNodes(recursive_request);
        if (initial_active_arg_classes.len != request.args.len) {
            Common.invariant("recursive function interface changed argument arity");
        }
        for (initial_active_arg_classes, request.args) |initial_class, request_arg| {
            if (!initial_class.contains(request_arg)) {
                try self.recursive_argument_slots.append(self.allocator, request_arg);
            }
        }
        try self.unify(active_fn, recursive_request);
    }

    pub fn markRecursiveValueSlot(self: *InstGraph, slot: NodeId) Allocator.Error!void {
        self.requireRelationProduction();
        try self.recursive_argument_slots.append(self.allocator, slot);
    }

    const generated_iterator_mint_depth_limit: u8 = 16;
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
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        var depths = collections.DenseMap(NodeId, u8).init(self.allocator);
        defer depths.deinit();
        var active = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&active);

        for (self.nodes.items, 0..) |_, raw_index| {
            const node = self.find(@enumFromInt(@as(u32, @intCast(raw_index))));
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => continue,
            };
            if (named.generated_iterator == null) continue;

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
                if (self.findGeneratedIterator(node, .forced_dynamic, &.{}, null)) |existing| {
                    if (self.find(existing) != node) {
                        try self.unify(node, existing);
                        continue;
                    }
                }
                try self.rewriteGeneratedIteratorAsForcedDynamic(node, named);
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
        const root = self.find(node);
        for (self.forced_dynamic_iterator_roots.items) |candidate| {
            if (self.find(candidate) == root) return true;
        }
        return false;
    }

    fn rewriteGeneratedIteratorAsForcedDynamic(
        self: *InstGraph,
        node: NodeId,
        source_named: InstNamed,
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
                .public_source = provenance.public_source,
            },
            .declared_order = provenance.public_source.declared_order,
        } });
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
                .ty = if (field.name == topology.step_field)
                    try self.forcedDynamicIteratorStepFunctionNode(field.ty, self_node, item_node, topology)
                else
                    field.ty,
                .value_ty = field.value_ty,
                .kind = field.kind,
                .default = field.default,
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
        if (tag_name != topology.one_tag and tag_name != topology.skip_tag) return public_payload;
        const public_fields = (try self.recordNodes(public_payload)).fields;
        const fields = try self.arena().alloc(InstField, public_fields.len);
        for (public_fields, fields) |field, *out| {
            out.* = .{
                .name = field.name,
                .ty = if (field.name == topology.rest_field)
                    self_node
                else if (field.name == topology.item_field)
                    item_node
                else
                    field.ty,
                .value_ty = field.value_ty,
                .kind = field.kind,
                .default = field.default,
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
                    if (named.def.iterator_kind == .forced_dynamic) {
                        break :blk .{ .fixed = generated_iterator_forced_depth };
                    }
                    const topology = named.def.iterator_kind.componentTopology() orelse
                        Common.invariant("generated iterator had no producer kind");
                    break :blk switch (topology) {
                        .source_without_components, .source_with_components => .{ .fixed = 1 },
                        .adapter => adapter: {
                            if (named.args.len == 0) {
                                Common.invariant("generated iterator adapter had no item argument");
                            }
                            break :adapter .{ .children = .{
                                .count = named.args.len - 1,
                                .increment = 1,
                            } };
                        },
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
            .named => |named| if (named.generated_iterator != null)
                named.args[child_index + 1]
            else
                named.args[child_index],
            .redirect, .unresolved, .primitive, .func, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator depth frame had no structural child"),
        };
    }

    /// Generated-iterator identities are representation-level: a type
    /// nickname over the same item type must produce the same generated
    /// iterator machinery, or an alias could duplicate that machinery and
    /// alter unification behavior. Type digests treat aliases as opaque
    /// nodes, so peel aliases to their backing before digesting here.
    fn peelAliasBacking(types: *const Type.Store, ty: Type.TypeId) Type.TypeId {
        var current = ty;
        // Alias chains in checked output are finite, so this terminates.
        while (true) {
            const node = types.get(current);
            if (node != .named) return current;
            const named = node.named;
            if (named.kind != .alias) return current;
            const backing = named.backing orelse return current;
            current = backing.ty;
        }
    }

    /// Seal producer identities for graph-owned iterator representations only
    /// after all type relations and representation decisions have been
    /// applied. Immutable Type-shaped snapshots of resolved nodes are used
    /// here; they remain graph-owned and never enter completed Monotype output.
    /// All digests are computed before any node is stamped, so dependency
    /// order cannot affect identity.
    pub fn finalizeGeneratedIteratorIdentities(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        const Pending = struct { node: NodeId, digest: names.TypeDigest };
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);

        for (self.nodes.items, 0..) |_, raw_index| {
            const node = self.find(@enumFromInt(@as(u32, @intCast(raw_index))));
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => continue,
            };
            const provenance = named.generated_iterator orelse continue;
            var hasher = std.crypto.hash.sha2.Sha256.init(.{});
            if (named.def.iterator_representation == .forced_dynamic) {
                if (named.args.len != 1) {
                    Common.invariant("forced-dynamic iterator identity did not have exactly one item argument");
                }
                const item = try self.provisionalTypeViewForNode(named.args[0]);
                const item_digest = self.types.typeDigest(self.name_store, peelAliasBacking(self.types, item));
                hasher.update("roc.generated_iterator.forced_dynamic_identity");
                hasher.update(&item_digest.bytes);
            } else {
                const final = try self.provisionalTypeViewForNode(node);
                const shape = self.types.typeDigest(self.name_store, peelAliasBacking(self.types, final));
                hasher.update("roc.generated_iterator.final_identity");
                hasher.update(&shape.bytes);
                if (provenance.callable_evidence) |evidence| {
                    hasher.update("callable_evidence");
                    hasher.update(&evidence.bytes);
                }
            }
            try pending.append(self.allocator, .{
                .node = node,
                .digest = .{ .bytes = hasher.finalResult() },
            });
        }
        for (pending.items) |item| {
            var named = switch (self.content(item.node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator identity target stopped being named"),
            };
            named.def.generated = item.digest;
            try self.setContent(item.node, .{ .named = named });
        }
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
        var visiting = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&visiting);
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
        var visiting = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&visiting);
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
        try self.containment_visit_epochs.append(self.allocator, 0);
        try self.row_exts.append(self.allocator, null);
        try self.request_source_interfaces.append(self.allocator, null);
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

    /// Short parent lists de-duplicate exactly on insert; above this length
    /// the insert-time scan (with a `find` per element) would make merging
    /// two classes quadratic in their parent counts, so longer lists append
    /// unconditionally and compact when they would otherwise grow.
    const row_parent_exact_scan_max = 16;

    fn addRowParent(self: *InstGraph, ext: NodeId, row: NodeId) Allocator.Error!void {
        const entry = try self.row_parents.getOrPut(self.find(ext));
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        const list = entry.value_ptr;
        const row_root = self.find(row);
        if (list.items.len <= row_parent_exact_scan_max) {
            for (list.items) |existing| {
                if (self.find(existing) == row_root) return;
            }
        } else if (list.items.len == list.capacity) {
            // Duplicate class entries are tolerated by every reader (each
            // resolves entries through `find`), so de-duplication can wait
            // until the list is about to grow. Capacity doubles between
            // compactions, keeping the total compaction work linear in the
            // number of inserts.
            try self.compactRowParents(list);
            // Guarantee at least as many appends as surviving entries before
            // the next compaction, so compaction cost amortizes to O(1) per
            // insert even when it reclaims almost nothing.
            try list.ensureUnusedCapacity(self.allocator, @max(list.items.len, row_parent_exact_scan_max));
            for (list.items) |existing| {
                if (existing == row_root) return;
            }
        }
        try list.append(self.allocator, row_root);
    }

    /// Rewrite every entry to its current class root and drop duplicate
    /// classes, preserving order of first occurrence.
    fn compactRowParents(self: *InstGraph, list: *std.ArrayList(NodeId)) Allocator.Error!void {
        const epochs_len = self.nodes.items.len;
        if (self.row_parent_seen_epochs.items.len < epochs_len) {
            const old_len = self.row_parent_seen_epochs.items.len;
            try self.row_parent_seen_epochs.resize(self.allocator, epochs_len);
            @memset(self.row_parent_seen_epochs.items[old_len..], 0);
        }
        self.row_parent_seen_epoch += 1;
        const epoch = self.row_parent_seen_epoch;
        var write: usize = 0;
        for (list.items) |existing| {
            const existing_root = self.find(existing);
            const slot = &self.row_parent_seen_epochs.items[@intFromEnum(existing_root)];
            if (slot.* == epoch) continue;
            slot.* = epoch;
            list.items[write] = existing_root;
            write += 1;
        }
        list.shrinkRetainingCapacity(write);
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
        var sizing = OpenFunctionInterfaceShapeWriter.init(self);
        defer sizing.deinit();
        try sizing.writeFunctionInterface(node);
        const digest: names.TypeDigest = .{ .bytes = sizing.hasher.finalResult() };

        const bytes = try self.arena().alloc(u8, sizing.output_len);
        var writer = OpenFunctionInterfaceShapeWriter.initWithOutput(self, bytes);
        defer writer.deinit();
        try writer.writeFunctionInterface(node);
        if (writer.output_len != bytes.len) {
            Common.invariant("open function-interface shape changed while being captured");
        }
        const written_digest: names.TypeDigest = .{ .bytes = writer.hasher.finalResult() };
        if (!std.mem.eql(u8, &digest.bytes, &written_digest.bytes)) {
            Common.invariant("open function-interface shape digest differed from its exact bytes");
        }
        return .{ .digest = digest, .bytes = bytes };
    }

    /// Whether a live graph type is already closed and can be snapshotted
    /// without applying any unresolved-variable or row default. Draft
    /// specialization lookup uses closed snapshots as its direct key; open
    /// requests remain graph-local until explicit recursive-edge identity or
    /// final body sealing resolves them.
    pub fn typeIsResolved(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        return try self.typeIsResolvedInner(root, false);
    }

    /// Whether every unresolved part of a specialization request is an
    /// undetermined field-kind cell. Those cells have the language-defined
    /// required default at relation freeze, so specialization lookup may take
    /// a read-only view with that default without mutating the live graph.
    pub fn typeIsSpecializationDefaultable(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        return try self.typeIsResolvedInner(root, true);
    }

    fn typeIsResolvedInner(
        self: *InstGraph,
        root: NodeId,
        allow_field_kind_defaults: bool,
    ) Allocator.Error!bool {
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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
                    for (row.fields) |field| {
                        if (field.kind == .undetermined and self.resolvedFieldKind(field.kind) == null) {
                            if (!allow_field_kind_defaults) return false;
                            try pending.append(
                                self.allocator,
                                field.value_ty orelse
                                    Common.invariant("undetermined field kind carried no source value cell"),
                            );
                            continue;
                        }
                        try pending.append(self.allocator, field.ty);
                        if (field.value_ty) |value_ty| try pending.append(self.allocator, value_ty);
                    }
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
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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

    /// Whether this exact graph type contains the public iterator interface at
    /// any structural depth. This consumes the checker-authored builtin owner
    /// carried by each named node; callers do not derive iterator intent from
    /// a backing shape.
    ///
    /// `Type.Store.containsIteratorInterface` answers the same question for
    /// immutable Monotypes and must stay in step with this walk; see its doc
    /// comment and the correspondence test at the bottom of this file.
    pub fn containsIteratorInterface(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        self.countDiagnostic("iterator_interface_scans");
        return try self.containmentResult(
            root,
            .iterator_interface,
            "iterator_interface_nodes_visited",
            "iterator_interface_cache_hits",
        );
    }

    /// Whether this exact graph type contains compiler-generated private
    /// opaque evidence at any structural depth.
    pub fn containsGeneratedPrivate(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        self.countDiagnostic("generated_private_scans");
        return try self.containmentResult(
            root,
            .generated_private,
            "generated_private_nodes_visited",
            "generated_private_cache_hits",
        );
    }

    fn containmentResult(
        self: *InstGraph,
        root: NodeId,
        comptime query: ContainmentQuery,
        comptime nodes_visited_field: []const u8,
        comptime cache_hits_field: []const u8,
    ) Allocator.Error!bool {
        const query_root = self.find(root);
        const cache = try self.containment_cache.getOrPut(query_root);
        if (!cache.found_existing) cache.value_ptr.* = .{};
        const entry = cache.value_ptr.forQuery(query);
        if (entry.valid and !self.containmentQueryCacheValid(entry)) {
            entry.valid = false;
            entry.result = false;
            entry.dependencies.clearRetainingCapacity();
        }
        if (entry.valid) {
            self.countDiagnostic(cache_hits_field);
            return entry.result;
        }
        self.containment_pending.clearRetainingCapacity();
        defer self.containment_pending.clearRetainingCapacity();
        if (self.containment_visit_epoch == std.math.maxInt(u32)) {
            @memset(self.containment_visit_epochs.items, 0);
            self.containment_visit_epoch = 1;
        } else {
            self.containment_visit_epoch += 1;
        }
        const visit_epoch = self.containment_visit_epoch;

        try self.containment_pending.append(self.allocator, query_root);
        while (self.containment_pending.pop()) |raw_node| {
            const node = self.find(raw_node);
            const node_index = @intFromEnum(node);
            if (self.containment_visit_epochs.items[node_index] == visit_epoch) continue;
            self.containment_visit_epochs.items[node_index] = visit_epoch;
            try entry.dependencies.append(self.allocator, .{
                .node = raw_node,
                .root = node,
                .version = self.versions.items[node_index],
            });
            self.countDiagnostic(nodes_visited_field);

            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .unresolved, .primitive, .empty_tag_union, .empty_record, .erased, .zst => {},
                .list, .box => |child| try self.containment_pending.append(self.allocator, child),
                .tuple => |items| try self.containment_pending.appendSlice(self.allocator, items),
                .func => |function| {
                    try self.containment_pending.appendSlice(self.allocator, function.args);
                    try self.containment_pending.append(self.allocator, function.ret);
                },
                .tag_union => |row| {
                    for (row.tags) |tag| try self.containment_pending.appendSlice(self.allocator, tag.payloads);
                    try self.containment_pending.append(self.allocator, row.ext);
                },
                .record => |row| {
                    for (row.fields) |field| {
                        try self.containment_pending.append(self.allocator, field.ty);
                        if (field.value_ty) |value_ty| {
                            try self.containment_pending.append(self.allocator, value_ty);
                        }
                    }
                    try self.containment_pending.append(self.allocator, row.ext);
                },
                .named => |named| {
                    const found = switch (query) {
                        .iterator_interface => if (named.builtin_owner) |owner|
                            static_dispatch.isIteratorOwner(owner)
                        else
                            false,
                        .generated_private => if (named.backing) |backing|
                            backing.authority == .generated_private
                        else
                            false,
                    };
                    if (found) {
                        entry.result = true;
                        entry.valid = true;
                        return true;
                    }
                    if (named.backing) |backing| {
                        try self.containment_pending.append(self.allocator, backing.node);
                    }
                    try self.containment_pending.appendSlice(self.allocator, named.args);
                    for (named.declared_order) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding| try self.containment_pending.append(self.allocator, padding),
                    };
                },
            }
        }
        entry.valid = true;
        return false;
    }

    fn containmentQueryCacheValid(
        self: *InstGraph,
        entry: *const ContainmentQueryCache,
    ) bool {
        for (entry.dependencies.items) |dependency| {
            if (self.find(dependency.node) != dependency.root or
                self.versions.items[@intFromEnum(dependency.root)] != dependency.version)
            {
                return false;
            }
        }
        return true;
    }

    /// Whether this exact graph type contains a node imported from a finished
    /// Monotype at any structural depth. Finished snapshots may be related to
    /// producer evidence, but no enclosing representation-selection operation
    /// may mutate one of their descendant classes.
    pub fn containsFinishedMono(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        self.countDiagnostic("finished_mono_scans");
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        try pending.append(self.allocator, root);
        while (pending.pop()) |raw_node| {
            const node = self.find(raw_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            self.countDiagnostic("finished_mono_nodes_visited");
            var members = self.classMemberIterator(node);
            while (members.next()) |member| {
                if (self.imported_monos.contains(member)) return true;
            }
            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .unresolved, .primitive, .empty_tag_union, .empty_record, .erased, .zst => {},
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
                    if (named.backing) |backing| try pending.append(self.allocator, backing.node);
                    try pending.appendSlice(self.allocator, named.args);
                    for (named.declared_order) |declared| switch (declared) {
                        .named => {},
                        .padding => |padding| try pending.append(self.allocator, padding),
                    };
                },
            }
        }
        return false;
    }

    /// Relate a checked public interface to a generated private specialization
    /// without merging a generated-private opaque node or any composite that
    /// contains it with its public counterpart. Matching composite structure is
    /// traversed explicitly so both roots keep a path to their respective
    /// opaque backing at every structural depth.
    /// `public_node` drives checked relations. A generated-private
    /// specialization lowers its callable body against `private_node`, which
    /// also supplies the specialization identity.
    pub fn relateOpaqueInterface(self: *InstGraph, public_node: NodeId, private_node: NodeId) Allocator.Error!void {
        try self.relateOpaqueInterfaceAtWidth(public_node, private_node, .exact);
    }

    /// Construction-width counterpart of `relateOpaqueInterface`. Generated-
    /// private representation boundaries remain distinct, while ordinary
    /// descendants replay the checker's explicit construction-width judgment.
    pub fn relateOpaqueConstructionInterface(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
    ) Allocator.Error!void {
        try self.relateOpaqueInterfaceAtWidth(public_node, private_node, .construction);
    }

    fn relateOpaqueInterfaceAtWidth(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        row_width: RowWidthRelation,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        var pending = std.ArrayList(NodePair).empty;
        defer pending.deinit(self.allocator);
        var related = std.AutoHashMap(NodePair, void).init(self.allocator);
        defer related.deinit();
        try pending.append(self.allocator, .{ .left = public_node, .right = private_node, .row_width = row_width });
        while (pending.pop()) |pair| {
            try self.relateOpaqueInterfacePair(pair, &pending, &related);
        }
    }

    /// Select producer-authored generated-private evidence as the runtime
    /// representation of a live checked-public draft. This capability exists
    /// only while the instantiation graph is producing relations; imported
    /// finished Monotypes can never participate. Ordinary `unify` rejects the
    /// same public/private edge structurally.
    pub fn selectGeneratedPrivateRepresentation(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
    ) Allocator.Error!void {
        try self.selectGeneratedPrivateRepresentationAtWidth(public_node, private_node, .exact);
    }

    /// Select generated-private evidence while replaying checker-approved
    /// construction width through the surrounding live graph.
    pub fn selectGeneratedPrivateConstructionRepresentation(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
    ) Allocator.Error!void {
        try self.selectGeneratedPrivateRepresentationAtWidth(public_node, private_node, .construction);
    }

    fn selectGeneratedPrivateRepresentationAtWidth(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        row_width: RowWidthRelation,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        if (try self.containsGeneratedPrivate(public_node) or !try self.containsGeneratedPrivate(private_node)) {
            Common.invariant("generated-private representation selection received incorrect public/private direction");
        }
        if (try self.containsFinishedMono(public_node) or try self.containsFinishedMono(private_node)) {
            Common.invariant("finished Monotype reached generated-private representation selection");
        }
        try self.unifyRootsTransitively(public_node, private_node, true, row_width);
    }

    fn relateOpaqueInterfacePair(
        self: *InstGraph,
        raw_pair: NodePair,
        pending: *std.ArrayList(NodePair),
        related: *std.AutoHashMap(NodePair, void),
    ) Allocator.Error!void {
        const public_node = self.find(raw_pair.left);
        const private_node = self.find(raw_pair.right);
        if (public_node == private_node) return;
        const pair = NodePair{
            .left = public_node,
            .right = private_node,
            .row_width = raw_pair.row_width,
        };
        if (related.contains(pair)) return;
        try related.put(pair, {});

        const public_content = self.nodes.items[@intFromEnum(public_node)];
        const private_content = self.nodes.items[@intFromEnum(private_node)];
        if (isGeneratedPrivateRootContent(public_content) and isGeneratedPrivateRootContent(private_content)) {
            try self.unifyAtRowWidth(public_node, private_node, pair.row_width);
            return;
        }
        const private_contains_generated = try self.containsGeneratedPrivate(private_node);
        if (private_content == .named) {
            const private_named = private_content.named;
            if (private_named.backing) |backing| {
                if (backing.authority == .generated_private) {
                    if (public_content == .unresolved) {
                        const public_var = public_content.unresolved;
                        if (private_named.generated_iterator != null) {
                            try self.materializeGeneratedIteratorPublicInterface(public_node, public_var, private_named);
                            try self.relateGeneratedOpaquePair(
                                self.nodes.items[@intFromEnum(self.find(public_node))],
                                private_named,
                                pair.row_width,
                                pending,
                            );
                            return;
                        }
                        if (try self.resolvePublicVariableToImportedGeneratedIterator(public_node, public_var, private_node, private_named)) {
                            return;
                        }
                    }
                    try self.relateGeneratedOpaquePair(public_content, private_named, pair.row_width, pending);
                    return;
                }
            }
        } else if (private_content == .unresolved) {
            try self.unifyAtRowWidth(public_node, private_node, pair.row_width);
            return;
        }

        switch (public_content) {
            .redirect => unreachable,
            .unresolved => |public_var| {
                if (private_contains_generated) {
                    if (private_content == .named) {
                        const private_named = private_content.named;
                        const public_named = try self.materializeNamedRequestPublicInterface(
                            public_node,
                            public_var,
                            private_named,
                        );
                        try self.relatePublicNamedOpaquePair(public_named, private_named, pair.row_width, pending);
                        try self.union_(private_node, public_node);
                        return;
                    }
                    if (try self.materializeStructuralRequestPublicInterface(
                        public_node,
                        public_var,
                        private_content,
                        pair.row_width,
                        pending,
                    )) {
                        return;
                    }
                    Common.invariant("opaque interface relation received unresolved checked structure for generated evidence");
                }
                try self.unifyAtRowWidth(public_node, private_node, pair.row_width);
            },
            .primitive => |public_primitive| {
                if (private_content != .primitive) Common.invariant("opaque interface relation received different type structure");
                if (public_primitive != private_content.primitive) {
                    Common.invariant("opaque interface relation received different primitive types");
                }
            },
            .list => |public_elem| {
                if (private_content != .list) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueChild(public_elem, private_content.list, pair.row_width, pending);
            },
            .box => |public_elem| {
                if (private_content != .box) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueChild(public_elem, private_content.box, pair.row_width, pending);
            },
            .tuple => |public_items| {
                if (private_content != .tuple) Common.invariant("opaque interface relation received different type structure");
                const private_items = private_content.tuple;
                if (public_items.len != private_items.len) {
                    Common.invariant("opaque interface relation received tuples of different arity");
                }
                for (public_items, private_items) |public_item, private_item| {
                    try self.relateOpaqueChild(public_item, private_item, pair.row_width, pending);
                }
            },
            .func => |public_fn| {
                if (private_content != .func) Common.invariant("opaque interface relation received different type structure");
                const private_fn = private_content.func;
                if (public_fn.args.len != private_fn.args.len) {
                    Common.invariant("opaque interface relation received functions of different arity");
                }
                for (public_fn.args, private_fn.args) |public_arg, private_arg| {
                    try self.relateOpaqueChild(public_arg, private_arg, pair.row_width, pending);
                }
                try self.relateOpaqueChild(public_fn.ret, private_fn.ret, pair.row_width, pending);
            },
            .tag_union => {
                if (private_content != .tag_union) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueTagRows(public_node, private_node, pair.row_width, pending);
            },
            .record => {
                if (private_content != .record) Common.invariant("opaque interface relation received different type structure");
                try self.relateOpaqueRecordRows(public_node, private_node, pair.row_width, pending);
            },
            .empty_tag_union => if (private_content != .empty_tag_union)
                Common.invariant("opaque interface relation received different type structure"),
            .empty_record => if (private_content != .empty_record)
                Common.invariant("opaque interface relation received different type structure"),
            .named => |public_named| {
                if (private_content != .named) Common.invariant("opaque interface relation received different type structure");
                try self.relatePublicNamedOpaquePair(public_named, private_content.named, pair.row_width, pending);
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
        if (!private_contains_generated) {
            try self.union_(public_node, private_node);
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
        row_width: RowWidthRelation,
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
                    try self.relateOpaqueChild(public_payload, private_payload, row_width, pending);
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
                for (private_tag.payloads) |payload| {
                    if (try self.containsGeneratedPrivate(payload)) {
                        Common.invariant("opaque interface row widening introduced unmatched generated-private tag payload");
                    }
                }
                try only_private.append(self.allocator, private_tag);
            }
        }

        if (self.rowAdditionConflicts(flat_public.ext, only_private.items.len, .tag_union) or
            self.rowAdditionConflicts(flat_private.ext, only_public.items.len, .tag_union))
        {
            Common.invariant("opaque interface relation widened a closed tag union");
        }
        if (only_public.items.len == 0 and only_private.items.len == 0) {
            try self.relateOpaqueChild(flat_public.ext, flat_private.ext, row_width, pending);
        } else if (only_public.items.len == 0) {
            try self.writeOrQueueTagRest(flat_public.ext, only_private.items, flat_private.ext, row_width, pending);
        } else if (only_private.items.len == 0) {
            try self.writeOrQueueTagRest(flat_private.ext, only_public.items, flat_public.ext, row_width, pending);
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
            if (self.find(flat_public.ext) == self.find(flat_private.ext)) {
                var rest = std.ArrayList(InstTag).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_public.items);
                try rest.appendSlice(self.allocator, only_private.items);
                try self.writeOrQueueTagRest(flat_public.ext, rest.items, new_ext, row_width, pending);
            } else {
                try self.writeOrQueueTagRest(flat_public.ext, only_private.items, new_ext, row_width, pending);
                try self.writeOrQueueTagRest(flat_private.ext, only_public.items, new_ext, row_width, pending);
            }
        }
    }

    /// Record-row counterpart of `relateOpaqueTagRows`.
    fn relateOpaqueRecordRows(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        row_width: RowWidthRelation,
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
                _ = self.unifyFieldKinds(
                    public_field.kind,
                    public_field.default,
                    private_field.kind,
                    private_field.default,
                );
                try self.relateOpaqueChild(
                    public_field.value_ty orelse public_field.ty,
                    private_field.value_ty orelse private_field.ty,
                    row_width,
                    pending,
                );
                try self.relateOpaqueChild(public_field.ty, private_field.ty, row_width, pending);
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
                if (try self.containsGeneratedPrivate(private_field.ty)) {
                    Common.invariant("opaque interface row widening introduced unmatched generated-private record field");
                }
                try only_private.append(self.allocator, private_field);
            }
        }

        const public_absorbs_private = self.closedRecordAbsorbsFields(flat_public.ext, only_private.items, row_width);
        const private_absorbs_public = self.closedRecordAbsorbsFields(flat_private.ext, only_public.items, row_width);
        if ((!public_absorbs_private and self.rowAdditionConflicts(flat_public.ext, only_private.items.len, .record)) or
            (!private_absorbs_public and self.rowAdditionConflicts(flat_private.ext, only_public.items.len, .record)))
        {
            Common.invariant("opaque interface relation widened a closed record");
        }

        const add_to_public = if (public_absorbs_private) &.{} else only_private.items;
        const add_to_private = if (private_absorbs_public) &.{} else only_public.items;
        if (add_to_public.len == 0 and add_to_private.len == 0) {
            try self.relateOpaqueChild(flat_public.ext, flat_private.ext, row_width, pending);
        } else if (add_to_private.len == 0) {
            try self.writeOrQueueRecordRest(flat_public.ext, add_to_public, flat_private.ext, row_width, pending);
        } else if (add_to_public.len == 0) {
            try self.writeOrQueueRecordRest(flat_private.ext, add_to_private, flat_public.ext, row_width, pending);
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
            if (self.find(flat_public.ext) == self.find(flat_private.ext)) {
                var rest = std.ArrayList(InstField).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, add_to_private);
                try rest.appendSlice(self.allocator, add_to_public);
                try self.writeOrQueueRecordRest(flat_public.ext, rest.items, new_ext, row_width, pending);
            } else {
                try self.writeOrQueueRecordRest(flat_public.ext, add_to_public, new_ext, row_width, pending);
                try self.writeOrQueueRecordRest(flat_private.ext, add_to_private, new_ext, row_width, pending);
            }
        }
    }

    fn relateOpaqueChild(
        self: *InstGraph,
        public_node: NodeId,
        private_node: NodeId,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        if (try self.containsGeneratedPrivate(private_node)) {
            try pending.append(self.allocator, .{ .left = public_node, .right = private_node, .row_width = row_width });
        } else {
            try self.unifyAtRowWidth(public_node, private_node, row_width);
        }
    }

    fn relateGeneratedOpaquePair(
        self: *InstGraph,
        public_content: InstNode,
        private_named: InstNamed,
        row_width: RowWidthRelation,
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
            try self.relateOpaqueChild(public_named.args[0], private_named.args[0], row_width, pending);
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
            try self.relateOpaqueChild(public_arg, private_arg, row_width, pending);
        }
    }

    fn relatePublicNamedOpaquePair(
        self: *InstGraph,
        public_named: InstNamed,
        private_named: InstNamed,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        if (public_named.kind != private_named.kind or
            !std.meta.eql(public_named.def, private_named.def) or
            public_named.args.len != private_named.args.len)
        {
            Common.invariant("opaque interface relation received different named types");
        }
        for (public_named.args, private_named.args) |public_arg, private_arg| {
            try self.relateOpaqueChild(public_arg, private_arg, row_width, pending);
        }
        if (public_named.backing) |public_backing| {
            const private_backing = private_named.backing orelse
                Common.invariant("opaque interface relation received different named backing presence");
            if (public_backing.authority != private_backing.authority) {
                Common.invariant("opaque interface relation received unmatched backing authority");
            }
            try self.relateOpaqueChild(public_backing.node, private_backing.node, row_width, pending);
        } else if (private_named.backing != null) {
            Common.invariant("opaque interface relation received different named backing presence");
        }
    }

    fn resolvePublicVariableToImportedGeneratedIterator(
        self: *InstGraph,
        public_node: NodeId,
        public_var: InstVariable,
        private_node: NodeId,
        private_named: InstNamed,
    ) Allocator.Error!bool {
        if (private_named.generated_iterator != null) return false;
        if (private_named.def.generated == null) return false;
        switch (private_named.def.iterator_representation) {
            .minted, .forced_dynamic => {},
            .none => return false,
        }
        const owner = private_named.builtin_owner orelse return false;
        if (!static_dispatch.isIteratorOwner(owner)) return false;
        if (private_named.args.len == 0) {
            Common.invariant("imported generated iterator relation received no item argument");
        }
        if (public_var.numeric_default_phase != null or public_var.row_default != null) {
            Common.invariant("imported generated iterator relation received a defaultable public variable");
        }
        try self.union_(private_node, public_node);
        return true;
    }

    fn materializeGeneratedIteratorPublicInterface(
        self: *InstGraph,
        public_node: NodeId,
        public_var: InstVariable,
        private_named: InstNamed,
    ) Allocator.Error!void {
        if (public_var.numeric_default_phase != null or public_var.row_default != null) {
            Common.invariant("generated iterator interface relation received a defaultable public variable");
        }
        if (private_named.args.len == 0) {
            Common.invariant("generated iterator interface relation received no private item argument");
        }
        const generated = private_named.generated_iterator orelse
            Common.invariant("generated iterator interface relation lacked producer provenance");
        const public_source = generated.public_source;
        if (!static_dispatch.isIteratorOwner(public_source.builtin_owner) or
            public_source.def.iterator_representation != .none or
            public_source.def.iterator_kind != .none)
        {
            Common.invariant("generated iterator interface relation received an invalid public iterator source");
        }
        if (public_source.backing.authority != .checked_public) {
            Common.invariant("generated iterator interface relation received a non-public source backing");
        }

        const args = try self.arena().alloc(NodeId, 1);
        args[0] = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
        try self.setContent(public_node, .{ .named = .{
            .named_type = public_source.named_type,
            .def = public_source.def,
            .kind = public_source.kind,
            .builtin_owner = public_source.builtin_owner,
            .args = args,
            .backing = public_source.backing,
            .generated_iterator = null,
            .declared_order = public_source.declared_order,
        } });
    }

    fn materializeNamedRequestPublicInterface(
        self: *InstGraph,
        public_node: NodeId,
        public_var: InstVariable,
        private_named: InstNamed,
    ) Allocator.Error!InstNamed {
        if (public_var.numeric_default_phase != null or public_var.row_default != null) {
            Common.invariant("named request interface relation received a defaultable public variable");
        }
        if (private_named.generated_iterator != null) {
            Common.invariant("named request interface relation received generated iterator provenance");
        }
        if (private_named.backing) |backing| {
            if (backing.authority != .checked_public) {
                Common.invariant("named request interface relation received a private root backing");
            }
        }

        const args = try self.arena().dupe(NodeId, private_named.args);
        try self.setContent(public_node, .{ .named = .{
            .named_type = private_named.named_type,
            .def = private_named.def,
            .kind = private_named.kind,
            .builtin_owner = private_named.builtin_owner,
            .args = args,
            .backing = private_named.backing,
            .generated_iterator = null,
            .declared_order = private_named.declared_order,
        } });
        return self.nodes.items[@intFromEnum(self.find(public_node))].named;
    }

    /// A generated-private witness can sit behind a structural container—a
    /// list literal element, a tuple slot, a record field, a tag payload—while
    /// the checked request position is still an unstructured variable. The
    /// public side then adopts the container shape with a fresh checked
    /// variable in each child slot, and every generated-carrying child keeps
    /// descending through the opaque relation. Keep the accepted set here
    /// equal to the structural containers a constructor can mint a witness
    /// for; private content this relation cannot structure returns false and
    /// leaves the caller's invariant to report it.
    fn materializeStructuralRequestPublicInterface(
        self: *InstGraph,
        public_node: NodeId,
        public_var: InstVariable,
        private_content: InstNode,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!bool {
        switch (private_content) {
            .list, .box, .tuple, .record, .tag_union => {},
            .redirect, .unresolved, .primitive, .named, .func, .empty_tag_union, .empty_record, .erased, .zst => return false,
        }
        if (public_var.numeric_default_phase != null or public_var.row_default != null) {
            Common.invariant("structural request interface relation received a defaultable public variable");
        }
        switch (private_content) {
            .list => |private_elem| {
                const elem = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                try self.setContent(public_node, .{ .list = elem });
                try self.relateOpaqueChild(elem, private_elem, row_width, pending);
            },
            .box => |private_elem| {
                const elem = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                try self.setContent(public_node, .{ .box = elem });
                try self.relateOpaqueChild(elem, private_elem, row_width, pending);
            },
            .tuple => |private_items| {
                const items = try self.arena().alloc(NodeId, private_items.len);
                for (items) |*item| {
                    item.* = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                }
                try self.setContent(public_node, .{ .tuple = items });
                for (items, private_items) |item, private_item| {
                    try self.relateOpaqueChild(item, private_item, row_width, pending);
                }
            },
            .record => |private_row| {
                const fields = try self.arena().alloc(InstField, private_row.fields.len);
                for (fields, private_row.fields) |*field, private_field| {
                    field.* = .{
                        .name = private_field.name,
                        .ty = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) }),
                        .value_ty = private_field.value_ty,
                        .kind = private_field.kind,
                        .default = private_field.default,
                    };
                }
                const ext = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                try self.setContent(public_node, .{ .record = .{ .fields = fields, .ext = ext } });
                for (fields, private_row.fields) |field, private_field| {
                    try self.relateOpaqueChild(field.ty, private_field.ty, row_width, pending);
                }
                try self.relateOpaqueChild(ext, private_row.ext, row_width, pending);
            },
            .tag_union => |private_row| {
                const tags = try self.arena().alloc(InstTag, private_row.tags.len);
                for (tags, private_row.tags) |*tag, private_tag| {
                    const payloads = try self.arena().alloc(NodeId, private_tag.payloads.len);
                    for (payloads) |*payload| {
                        payload.* = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                    }
                    tag.* = .{
                        .name = private_tag.name,
                        .checked_name = private_tag.checked_name,
                        .payloads = payloads,
                    };
                }
                const ext = try self.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
                try self.setContent(public_node, .{ .tag_union = .{ .tags = tags, .ext = ext } });
                for (tags, private_row.tags) |tag, private_tag| {
                    for (tag.payloads, private_tag.payloads) |payload, private_payload| {
                        try self.relateOpaqueChild(payload, private_payload, row_width, pending);
                    }
                }
                try self.relateOpaqueChild(ext, private_row.ext, row_width, pending);
            },
            .redirect, .unresolved, .primitive, .named, .func, .empty_tag_union, .empty_record, .erased, .zst => unreachable,
        }
        return true;
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
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);

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

    /// Build an exact value witness for one constructed tag while preserving
    /// every other checked tag and the row extension. The caller supplies the
    /// payload representations emitted by the constructor's children.
    pub fn tagValueNodeWithPayloads(
        self: *InstGraph,
        raw_row: NodeId,
        name: names.TagNameId,
        payloads: []const NodeId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_row, "tag value", .runtime_layout);
        const flat = try self.flattenTagRow(structural);
        const tags = try self.arena().alloc(InstTag, flat.tags.len);
        var found = false;
        for (flat.tags, tags) |tag, *out| {
            if (tag.name == name) {
                if (found) Common.invariant("tag value witness found duplicate tag labels");
                if (tag.payloads.len != payloads.len) {
                    Common.invariant("tag value witness payload arity differed from its checked tag");
                }
                found = true;
                out.* = .{
                    .name = tag.name,
                    .checked_name = tag.checked_name,
                    .payloads = try self.arena().dupe(NodeId, payloads),
                };
            } else {
                out.* = tag;
            }
        }
        if (!found) Common.invariant("tag value witness did not find its checked tag");
        return try self.newNode(.{ .tag_union = .{
            .tags = tags,
            .ext = self.find(flat.ext),
        } });
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

    /// Create a named value witness with an exact produced backing while
    /// preserving the checked nominal identity and backing capabilities.
    pub fn namedValueNodeWithBacking(
        self: *InstGraph,
        raw_named: NodeId,
        backing_node: NodeId,
    ) Allocator.Error!NodeId {
        const named_content = self.content(raw_named);
        if (named_content != .named) Common.invariant("named value witness had a non-named checked node");
        var named = named_content.named;
        const backing = named.backing orelse
            Common.invariant("named value witness had no checked backing");
        named.backing = .{
            .node = backing_node,
            .use = backing.use,
            .authority = backing.authority,
        };
        return self.newNode(.{ .named = named });
    }

    /// Return the graph node for one field of a record-shaped node. Field
    /// access is a type relation, so callers use this node directly instead of
    /// selecting a field from a temporary Monotype view and losing later row
    /// evidence.
    pub fn recordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .inspectable, "record field access");
    }

    /// Apply one checked required-access judgment to the field-kind cell and
    /// return the inline value slot selected by that judgment.
    pub fn requiredRecordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.requiredRecordFieldNodeWithAccess(raw_record, name, .inspectable, "required record field access");
    }

    /// Select a private backing field for
    /// `CheckedFieldBackingAccess.opaque_definition_private`.
    pub fn opaqueDefinitionFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .runtime_layout, "opaque-definition-private record field access");
    }

    pub fn requiredOpaqueDefinitionFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.requiredRecordFieldNodeWithAccess(raw_record, name, .runtime_layout, "required opaque-definition-private record field access");
    }

    /// Apply one checked optional-access judgment and return the distinct
    /// source-value/runtime-slot cells whose relationship the caller records.
    pub fn optionalRecordFieldNodes(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!OptionalFieldAccessNodes {
        return self.optionalRecordFieldNodesWithAccess(raw_record, name, .inspectable, "optional record field access");
    }

    pub fn optionalOpaqueDefinitionFieldNodes(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!OptionalFieldAccessNodes {
        return self.optionalRecordFieldNodesWithAccess(raw_record, name, .runtime_layout, "optional opaque-definition-private record field access");
    }

    /// Return one backing field cell while lowering a checked record
    /// constructor. The explicit API name is the capability to cross a
    /// runtime-layout-only named backing; ordinary field selection must use
    /// `recordFieldNode` and cannot inspect such a backing.
    pub fn recordConstructionFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .runtime_layout, "record constructor");
    }

    /// Return the source value cell of a construction field. Optional and
    /// generalized fields keep this distinct from their runtime slot so child
    /// relations connect source-value cells before checked field-kind evidence
    /// commits a runtime slot.
    pub fn recordConstructionFieldValueNode(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_record, "record constructor", .runtime_layout);
        if (self.content(structural) != .record) {
            Common.invariant("instantiation record constructor had a non-record receiver type");
        }
        const row = try self.flattenRecordRow(structural);
        const wanted = self.fieldLabelText(name);
        for (row.fields) |field| {
            if (Ident.textEql(wanted, self.fieldLabelText(field.name))) {
                return self.find(field.value_ty orelse field.ty);
            }
        }
        Common.invariant("instantiation record constructor requested an absent field value");
    }

    /// Return checker-originated field-kind evidence after specialization has
    /// resolved any generalized presence variable. Construction consumes this
    /// instead of re-reading the generalized checked scheme.
    pub fn recordConstructionFieldKind(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
    ) Allocator.Error!ResolvedFieldKind {
        const structural = try self.shapeRoot(raw_record, "record constructor", .runtime_layout);
        if (self.content(structural) != .record) {
            Common.invariant("instantiation record constructor had a non-record receiver type");
        }
        const row = try self.flattenRecordRow(structural);
        const wanted = self.fieldLabelText(name);
        for (row.fields) |field| {
            if (!Ident.textEql(wanted, self.fieldLabelText(field.name))) continue;
            if (self.resolvedFieldKind(field.kind)) |resolved| return resolved;
            switch (field.kind) {
                .undetermined => |id| {
                    // A checked literal/update introduces a required field.
                    // Optional/defaulted caller evidence, when present, has
                    // already constrained this same identity; otherwise the
                    // construction itself is the explicit required evidence.
                    // Required means the runtime slot is exactly the source
                    // value cell, so commit that relation together with the
                    // kind instead of leaving an unrelated placeholder slot.
                    self.constrainUndeterminedFieldKind(id, .required);
                    try self.unify(
                        field.ty,
                        field.value_ty orelse
                            Common.invariant("undetermined constructor field carried no source value type"),
                    );
                    return .required;
                },
                .sealed, .required, .optional, .defaulted => Common.invariant("record constructor field kind carried no specialization evidence"),
            }
        }
        Common.invariant("instantiation record constructor requested an absent field kind");
    }

    /// Return the already-selected kind for a field omitted by a record
    /// constructor. Unlike `recordConstructionFieldKind`, omission is not
    /// evidence that an undetermined field is required: the checker or the
    /// specialization relation must already have selected optional/defaulted.
    pub fn recordOmittedFieldKind(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
    ) Allocator.Error!ResolvedFieldKind {
        const structural = try self.shapeRoot(raw_record, "record constructor", .runtime_layout);
        if (self.content(structural) != .record) {
            Common.invariant("instantiation record constructor had a non-record receiver type");
        }
        const row = try self.flattenRecordRow(structural);
        const wanted = self.fieldLabelText(name);
        for (row.fields) |field| {
            if (!Ident.textEql(wanted, self.fieldLabelText(field.name))) continue;
            if (self.resolvedFieldKind(field.kind)) |resolved| return resolved;
            return switch (field.kind) {
                .sealed => if (field.default) |default|
                    .{ .defaulted = default }
                else if (field.value_ty != null)
                    .optional
                else
                    .required,
                .undetermined => Common.invariant("omitted record constructor field kind remained undetermined"),
                .required, .optional, .defaulted => unreachable,
            };
        }
        Common.invariant("instantiation record constructor requested an absent omitted-field kind");
    }

    fn recordFieldNodeWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!NodeId {
        return self.find((try self.recordFieldWithAccess(raw_record, name, access, noun)).ty);
    }

    fn recordFieldWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!InstField {
        const structural = try self.shapeRoot(raw_record, noun, access);
        if (self.content(structural) != .record) Common.invariant("instantiation " ++ noun ++ " had a non-record receiver type");
        const row = try self.flattenRecordRow(structural);
        const wanted = self.fieldLabelText(name);
        for (row.fields) |field| {
            if (Ident.textEql(wanted, self.fieldLabelText(field.name))) {
                return field;
            }
        }
        Common.invariant("instantiation " ++ noun ++ " requested an absent field");
    }

    fn requiredRecordFieldNodeWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!NodeId {
        const field = try self.recordFieldWithAccess(raw_record, name, access, noun);
        switch (field.kind) {
            .sealed => {
                if (field.value_ty != null) {
                    Common.invariant("required access reached a sealed optional record field");
                }
            },
            .required, .defaulted => {},
            .optional => Common.invariant("required access reached an optional record field"),
            .undetermined => |id| if (self.resolvedFieldKind(field.kind)) |resolved| switch (resolved) {
                .required, .defaulted => {},
                .optional => Common.invariant("required access resolved an optional record field kind"),
            } else {
                self.constrainUndeterminedFieldKind(id, .required);
            },
        }
        const value = field.value_ty orelse field.ty;
        try self.unify(field.ty, value);
        return self.find(field.ty);
    }

    fn optionalRecordFieldNodesWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!OptionalFieldAccessNodes {
        const field = try self.recordFieldWithAccess(raw_record, name, access, noun);
        _ = self.unifyFieldKinds(field.kind, field.default, .optional, null);
        return .{
            .slot = self.find(field.ty),
            .value = self.find(field.value_ty orelse
                Common.invariant("optional field access had no source value cell")),
        };
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
        if (structural_content == .record) return .{ .fields = (try self.flattenRecordRow(structural)).fields };
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
        try self.unifyRootsTransitively(a, b, false, .exact);
    }

    /// Replay a checker-approved construction relation. Unlike ordinary graph
    /// equality, a closed record may absorb unmatched fields whose explicit
    /// kind is optional or defaulted; required and unresolved fields remain an
    /// invariant violation.
    pub fn unifyConstruction(self: *InstGraph, a: NodeId, b: NodeId) Allocator.Error!void {
        try self.unifyRootsTransitively(a, b, false, .construction);
    }

    fn unifyAtRowWidth(
        self: *InstGraph,
        a: NodeId,
        b: NodeId,
        row_width: RowWidthRelation,
    ) Allocator.Error!void {
        switch (row_width) {
            .exact => try self.unify(a, b),
            .construction => try self.unifyConstruction(a, b),
        }
    }

    /// Join two matching structural request containers after their components
    /// have already been related with public/private-aware edges. The request
    /// node remains the class representative so later body lowering continues
    /// to see the producer-owned representation at the container boundary.
    pub fn joinRelatedRequestContainer(
        self: *InstGraph,
        public_node: NodeId,
        request_node: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const public_root = self.find(public_node);
        const request_root = self.find(request_node);
        if (public_root == request_root) return;
        const public_content = self.nodes.items[@intFromEnum(public_root)];
        const request_content = self.nodes.items[@intFromEnum(request_root)];
        switch (public_content) {
            .list => {
                if (request_content != .list) Common.invariant("request container join received different type structure");
            },
            .box => {
                if (request_content != .box) Common.invariant("request container join received different type structure");
            },
            .tuple => |public_items| {
                if (request_content != .tuple) Common.invariant("request container join received different type structure");
                if (public_items.len != request_content.tuple.len) {
                    Common.invariant("request container join received tuples of different arity");
                }
            },
            .func => |public_fn| {
                if (request_content != .func) Common.invariant("request container join received different type structure");
                if (public_fn.args.len != request_content.func.args.len) {
                    Common.invariant("request container join received functions of different arity");
                }
            },
            .record => {
                if (request_content != .record) Common.invariant("request container join received different type structure");
                const public_row = try self.flattenRecordRow(public_root);
                const request_row = try self.flattenRecordRow(request_root);
                if (public_row.fields.len != request_row.fields.len) {
                    Common.invariant("request container join received records with different field counts");
                }
                for (public_row.fields) |public_field| {
                    const wanted = self.fieldLabelText(public_field.name);
                    var found = false;
                    for (request_row.fields) |request_field| {
                        if (Ident.textEql(wanted, self.fieldLabelText(request_field.name))) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        Common.invariant("request container join received records with different fields");
                    }
                }
            },
            .tag_union => {
                if (request_content != .tag_union) Common.invariant("request container join received different type structure");
                const public_row = try self.flattenTagRow(public_root);
                const request_row = try self.flattenTagRow(request_root);
                if (public_row.tags.len != request_row.tags.len) {
                    Common.invariant("request container join received tag unions with different tag counts");
                }
                for (public_row.tags) |public_tag| {
                    const wanted = self.tagLabelText(public_tag.name);
                    var found = false;
                    for (request_row.tags) |request_tag| {
                        if (Ident.textEql(wanted, self.tagLabelText(request_tag.name)) and
                            public_tag.payloads.len == request_tag.payloads.len)
                        {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        Common.invariant("request container join received tag unions with different tags");
                    }
                }
            },
            .redirect,
            .unresolved,
            .primitive,
            .empty_tag_union,
            .empty_record,
            .named,
            .erased,
            .zst,
            => Common.invariant("request container join received a non-container public type"),
        }
        try self.union_(request_root, public_root);
    }

    fn unifyRootsTransitively(
        self: *InstGraph,
        a: NodeId,
        b: NodeId,
        allow_private_selection: bool,
        row_width: RowWidthRelation,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        self.countDiagnostic("unify_requests");
        var pending = std.ArrayList(NodePair).empty;
        defer pending.deinit(self.allocator);
        var related = std.AutoHashMap(NodePair, void).init(self.allocator);
        defer related.deinit();
        try pending.append(self.allocator, .{ .left = a, .right = b, .row_width = row_width });
        while (pending.pop()) |pair| {
            try self.unifyRoots(pair.left, pair.right, pair.row_width, &pending, &related, allow_private_selection);
        }
    }

    fn unifyRoots(
        self: *InstGraph,
        raw_left: NodeId,
        raw_right: NodeId,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
        related: *std.AutoHashMap(NodePair, void),
        allow_private_selection: bool,
    ) Allocator.Error!void {
        const left = self.find(raw_left);
        const right = self.find(raw_right);
        if (left == right) return;
        const pair = NodePair{ .left = left, .right = right, .row_width = row_width };
        if (related.contains(pair)) return;
        try related.put(pair, {});
        const relation = self.relationStamp(left, right, row_width);
        if (self.processed_relations.contains(relation)) return;
        try self.processed_relations.put(relation, {});

        const left_content = self.nodes.items[@intFromEnum(left)];
        const right_content = self.nodes.items[@intFromEnum(right)];
        const left_generated_private = left_content == .named and
            (if (left_content.named.backing) |backing| backing.authority == .generated_private else false);
        const right_generated_private = right_content == .named and
            (if (right_content.named.backing) |backing| backing.authority == .generated_private else false);
        if (left_generated_private != right_generated_private and
            !allow_private_selection and
            !self.isIteratorRepresentationTierRelation(left_content, right_content))
        {
            Common.invariant("generated-private representation reached ordinary public/private graph unification");
        }

        if (left_content == .redirect) unreachable;
        if (left_content == .unresolved) {
            if (right_content == .unresolved) {
                try self.setContent(right, .{ .unresolved = mergeVariables(left_content.unresolved, right_content.unresolved) });
                try self.union_(right, left);
            } else if (right_content == .named and right_content.named.kind == .alias) {
                try self.unifyThroughBacking(right, right_content, left, row_width, pending);
            } else {
                try self.union_(right, left);
            }
        } else if (right_content == .unresolved) {
            if (left_content == .named and left_content.named.kind == .alias) {
                try self.unifyThroughBacking(left, left_content, right, row_width, pending);
            } else {
                try self.union_(left, right);
            }
        } else {
            try self.unifyConcrete(left, left_content, right, right_content, row_width, pending);
        }
    }

    fn relationStamp(self: *InstGraph, left: NodeId, right: NodeId, row_width: RowWidthRelation) RelationStamp {
        const left_raw = @intFromEnum(left);
        const right_raw = @intFromEnum(right);
        if (left_raw <= right_raw) {
            return .{
                .left = left,
                .left_version = self.versions.items[left_raw],
                .right = right,
                .right_version = self.versions.items[right_raw],
                .row_width = row_width,
            };
        }
        return .{
            .left = right,
            .left_version = self.versions.items[right_raw],
            .right = left,
            .right_version = self.versions.items[left_raw],
            .row_width = row_width,
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
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
        switch (left_content) {
            .redirect, .unresolved => unreachable,
            .primitive => |left_prim| {
                if (right_content == .primitive) {
                    if (left_prim != right_content.primitive) Common.invariant("instantiation unified two different primitive types");
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a primitive type with a non-primitive type");
                }
            },
            .list => |left_elem| {
                if (right_content == .list) {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_content.list, .row_width = row_width });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a List with a non-List type");
                }
            },
            .box => |left_elem| {
                if (right_content == .box) {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_content.box, .row_width = row_width });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a Box with a non-Box type");
                }
            },
            .tuple => |left_items| {
                if (right_content == .tuple) {
                    const right_items = right_content.tuple;
                    if (left_items.len != right_items.len) Common.invariant("instantiation unified tuples of different arity");
                    for (left_items, right_items) |left_item, right_item| {
                        try pending.append(self.allocator, .{ .left = left_item, .right = right_item, .row_width = row_width });
                    }
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a tuple with a non-tuple type");
                }
            },
            .func => |left_fn| {
                if (right_content == .func) {
                    const right_fn = right_content.func;
                    if (left_fn.args.len != right_fn.args.len) Common.invariant("instantiation unified functions of different arity");
                    for (left_fn.args, right_fn.args) |left_arg, right_arg| {
                        try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg, .row_width = row_width });
                    }
                    try pending.append(self.allocator, .{ .left = left_fn.ret, .right = right_fn.ret, .row_width = row_width });
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a function with a non-function type");
                }
            },
            .tag_union => {
                if (right_content == .tag_union) {
                    try self.unifyTagRows(left, right, row_width, pending);
                } else if (right_content == .empty_tag_union) {
                    try self.unifyRowWithEmpty(left, right, .tag_union, row_width);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a tag union with a non-tag-union type");
                }
            },
            .empty_tag_union => {
                if (right_content == .empty_tag_union) {
                    try self.union_(left, right);
                } else if (right_content == .tag_union) {
                    try self.unifyRowWithEmpty(right, left, .tag_union, row_width);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified an empty tag union with an incompatible type");
                }
            },
            .record => {
                if (right_content == .record) {
                    try self.unifyRecordRows(left, right, row_width, pending);
                } else if (right_content == .empty_record) {
                    try self.unifyRowWithEmpty(left, right, .record, row_width);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a record with a non-record type");
                }
            },
            .empty_record => {
                if (right_content == .empty_record) {
                    try self.union_(left, right);
                } else if (right_content == .record) {
                    try self.unifyRowWithEmpty(right, left, .record, row_width);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified an empty record with an incompatible type");
                }
            },
            .named => |left_named| {
                if (right_content == .named) {
                    const right_named = right_content.named;
                    if (left_named.kind == .alias) {
                        try self.unifyThroughBacking(left, left_content, right, row_width, pending);
                        return;
                    }
                    if (right_named.kind == .alias) {
                        try self.unifyThroughBacking(right, right_content, left, row_width, pending);
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
                                .row_width = row_width,
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
                                .row_width = row_width,
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
                                .row_width = row_width,
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
                                    .row_width = row_width,
                                });
                            } else if (right_named.backing != null) {
                                Common.invariant("minted iterator join found backing on only one side");
                            }

                            for (self.recursive_argument_slots.items) |slot| {
                                const slot_root = self.find(slot);
                                if (slot_root == left or slot_root == right) {
                                    try self.forced_dynamic_iterator_roots.append(self.allocator, left);
                                }
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
                            try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg, .row_width = row_width });
                        }
                        if (left_named.backing) |left_backing| {
                            if (right_named.backing) |right_backing| {
                                if (left_backing.authority == right_backing.authority) {
                                    try pending.append(self.allocator, .{ .left = left_backing.node, .right = right_backing.node, .row_width = row_width });
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
                    try self.unifyThroughBacking(left, left_content, right, row_width, pending);
                } else {
                    try self.unifyThroughBacking(left, left_content, right, row_width, pending);
                }
            },
            .erased => |left_digest| {
                if (right_content == .erased) {
                    if (!std.mem.eql(u8, left_digest.bytes[0..], right_content.erased.bytes[0..])) {
                        Common.invariant("instantiation unified two different erased types");
                    }
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified an erased type with an incompatible type");
                }
            },
            .zst => {
                if (right_content == .zst) {
                    try self.union_(left, right);
                } else if (right_content == .named) {
                    try self.unifyThroughBacking(right, right_content, left, row_width, pending);
                } else {
                    Common.invariant("instantiation unified a zero-sized type with an incompatible type");
                }
            },
        }
    }

    fn isIteratorRepresentationTierRelation(self: *InstGraph, left: InstNode, right: InstNode) bool {
        if (left != .named or right != .named) return false;
        const left_named = left.named;
        const right_named = right.named;
        return switch (self.iteratorRelation(left_named, right_named)) {
            .public_minted, .forced_dynamic => true,
            .ordinary, .minted_join => false,
        };
    }

    fn iteratorRelation(self: *InstGraph, left: InstNamed, right: InstNamed) Type.IteratorRelation {
        const base_relation = Type.iteratorRelation(left, right);
        if (base_relation != .ordinary) return base_relation;
        if (left.def.iterator_representation == .forced_dynamic and
            right.def.iterator_representation == .forced_dynamic and
            !optionalInstDigestEql(left.def.generated, right.def.generated))
        {
            return .forced_dynamic;
        }
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
        row_width: RowWidthRelation,
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
        // The named node already owns this exact structural backing. This
        // relation arises when a checked function interface names the wrapper
        // while its constructor pattern names the backing. Redirecting the
        // backing into its owner would destroy the explicit backing edge and
        // leave a non-recursive named type pointing to itself.
        if (backing_node == other) return;
        if (named.kind == .alias) {
            try pending.append(self.allocator, .{ .left = backing_node, .right = other, .row_width = row_width });
            return;
        }
        if (self.nodes.items[@intFromEnum(other)] == .named) {
            try pending.append(self.allocator, .{ .left = backing_node, .right = other, .row_width = row_width });
            return;
        }
        const moved = try self.newNode(self.nodes.items[@intFromEnum(other)]);
        try self.union_(named_node, other);
        try pending.append(self.allocator, .{ .left = backing_node, .right = moved, .row_width = row_width });
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
            left.type_name == right.type_name;
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

    fn recordFieldsAreAbsorbable(self: *InstGraph, fields: []const InstField) bool {
        for (fields) |field| {
            const kind = self.resolvedFieldKind(field.kind) orelse return false;
            switch (kind) {
                .optional, .defaulted => {},
                .required => return false,
            }
        }
        return true;
    }

    fn closedRecordAbsorbsFields(
        self: *InstGraph,
        raw_ext: NodeId,
        fields: []const InstField,
        row_width: RowWidthRelation,
    ) bool {
        if (row_width != .construction or fields.len == 0) return false;
        if (self.nodes.items[@intFromEnum(self.find(raw_ext))] != .empty_record) return false;
        return self.recordFieldsAreAbsorbable(fields);
    }

    /// A row with a head met an empty row: the head must be empty too, and the
    /// row's extension must also be empty.
    fn unifyRowWithEmpty(
        self: *InstGraph,
        row: NodeId,
        empty: NodeId,
        kind: RowKind,
        row_width: RowWidthRelation,
    ) Allocator.Error!void {
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
                if (flat.fields.len != 0 and
                    (row_width != .construction or !self.recordFieldsAreAbsorbable(flat.fields)))
                {
                    Common.invariant("instantiation unified a non-absorbable record with an empty record");
                }
                try self.unify(flat.ext, empty);
                if (flat.fields.len == 0) {
                    try self.setContent(row, .empty_record);
                    try self.union_(empty, row);
                } else {
                    // The empty construction adopts the explicit optional or
                    // defaulted slots; its lowering materializes Missing tags
                    // or defaults from the now-shared record class.
                    try self.union_(row, empty);
                }
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

        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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

        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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
                .named => |named| {
                    const declared_backing = named.backing orelse
                        Common.invariant("instantiation record row extended into a named type without backing");
                    if (declared_backing.use != .inspectable) {
                        Common.invariant("instantiation record row extended into a non-inspectable named type");
                    }
                    const backing = try self.structuralBackingNode(declared_backing.node, named);
                    if (backing.recursive) {
                        Common.invariant("instantiation record row extended into a recursive named type");
                    }
                    ext = self.find(backing.node);
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

    fn unifyTagRows(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
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
                    try pending.append(self.allocator, .{ .left = left_payload, .right = right_payload, .row_width = row_width });
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
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = flat_right.ext, .row_width = row_width });
        } else if (only_left.items.len == 0) {
            // Left lacks tags: its extension absorbs the right-only tags.
            try self.writeOrQueueTagRest(flat_left.ext, only_right.items, flat_right.ext, row_width, pending);
            merged_ext = flat_right.ext;
        } else if (only_right.items.len == 0) {
            try self.writeOrQueueTagRest(flat_right.ext, only_left.items, flat_left.ext, row_width, pending);
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
            if (self.find(flat_left.ext) == self.find(flat_right.ext)) {
                var rest = std.ArrayList(InstTag).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, only_left.items);
                try rest.appendSlice(self.allocator, only_right.items);
                try self.writeOrQueueTagRest(flat_left.ext, rest.items, new_ext, row_width, pending);
            } else {
                try self.writeOrQueueTagRest(flat_left.ext, only_right.items, new_ext, row_width, pending);
                try self.writeOrQueueTagRest(flat_right.ext, only_left.items, new_ext, row_width, pending);
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
        row_width: RowWidthRelation,
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
            try pending.append(self.allocator, .{ .left = ext_root, .right = rest, .row_width = row_width });
        }
    }

    fn unifyRecordRows(
        self: *InstGraph,
        left: NodeId,
        right: NodeId,
        row_width: RowWidthRelation,
        pending: *std.ArrayList(NodePair),
    ) Allocator.Error!void {
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
                const merged_kind = self.unifyFieldKinds(
                    left_field.kind,
                    left_field.default,
                    right_field.kind,
                    right_field.default,
                );
                try pending.append(self.allocator, .{
                    .left = left_field.value_ty orelse left_field.ty,
                    .right = right_field.value_ty orelse right_field.ty,
                    .row_width = row_width,
                });
                try pending.append(self.allocator, .{ .left = left_field.ty, .right = right_field.ty, .row_width = row_width });
                const resolved_default = if (self.resolvedFieldKind(merged_kind)) |resolved|
                    resolved.defaultIdentity()
                else
                    left_field.default orelse right_field.default;
                try merged.append(self.allocator, .{
                    .name = left_field.name,
                    .ty = left_field.ty,
                    .value_ty = left_field.value_ty orelse right_field.value_ty,
                    .kind = merged_kind,
                    .default = resolved_default,
                });
                shared = true;
                break;
            }
            if (!shared) {
                try merged.append(self.allocator, left_field);
                try only_left.append(self.allocator, left_field);
            }
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

        const left_absorbs_right = self.closedRecordAbsorbsFields(flat_left.ext, only_right.items, row_width);
        const right_absorbs_left = self.closedRecordAbsorbsFields(flat_right.ext, only_left.items, row_width);
        if ((!left_absorbs_right and self.rowAdditionConflicts(flat_left.ext, only_right.items.len, .record)) or
            (!right_absorbs_left and self.rowAdditionConflicts(flat_right.ext, only_left.items.len, .record)))
        {
            Common.invariant("instantiation widened a closed record");
        }

        const add_to_left = if (left_absorbs_right) &.{} else only_right.items;
        const add_to_right = if (right_absorbs_left) &.{} else only_left.items;
        var merged_ext = flat_left.ext;
        if (add_to_left.len == 0 and add_to_right.len == 0) {
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = flat_right.ext, .row_width = row_width });
        } else if (add_to_right.len == 0) {
            try self.writeOrQueueRecordRest(flat_left.ext, add_to_left, flat_right.ext, row_width, pending);
            merged_ext = flat_right.ext;
        } else if (add_to_left.len == 0) {
            try self.writeOrQueueRecordRest(flat_right.ext, add_to_right, flat_left.ext, row_width, pending);
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
            if (self.find(flat_left.ext) == self.find(flat_right.ext)) {
                var rest = std.ArrayList(InstField).empty;
                defer rest.deinit(self.allocator);
                try rest.appendSlice(self.allocator, add_to_left);
                try rest.appendSlice(self.allocator, add_to_right);
                try self.writeOrQueueRecordRest(flat_left.ext, rest.items, new_ext, row_width, pending);
            } else {
                try self.writeOrQueueRecordRest(flat_left.ext, add_to_left, new_ext, row_width, pending);
                try self.writeOrQueueRecordRest(flat_right.ext, add_to_right, new_ext, row_width, pending);
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
        row_width: RowWidthRelation,
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
            try pending.append(self.allocator, .{ .left = ext_root, .right = rest, .row_width = row_width });
        }
    }

    /// Import a Monotype into the graph. A Monotype already linked to a node
    /// reconnects to it; an unlinked one copies in as closed structure, so a
    /// later attempt to widen it is a unification conflict rather than a silent
    /// mutation of another specialization's final type.
    pub fn importMono(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        self.countDiagnostic("mono_import_requests");
        if (self.linked_type_nodes.get(ty)) |existing| {
            self.countDiagnostic("mono_import_hits");
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

        const types = self.types;
        const imported: InstNode = switch (types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.importMono(elem) },
            .box => |elem| .{ .box = try self.importMono(elem) },
            .tuple => |items| .{ .tuple = try self.importMonoSlice(types.span(items)) },
            .func => |func| .{ .func = .{
                .args = try self.importMonoSlice(types.span(func.args)),
                .ret = try self.importMono(func.ret),
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
                        .payloads = try self.importMonoSlice(types.span(tag.payloads)),
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
                    if (field.kind_state == .undetermined) {
                        Common.invariant("finished Monotype import received a provisional record field");
                    }
                    const value_ty = if (field.value_ty) |value_ty|
                        try self.importMono(value_ty)
                    else
                        null;
                    inst_fields[index] = .{
                        .name = field.name,
                        .ty = try self.importMono(field.ty),
                        .value_ty = value_ty,
                        .kind = if (value_ty != null)
                            .optional
                        else if (field.default) |default|
                            .{ .defaulted = default }
                        else
                            .required,
                        .default = field.default,
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
                .args = try self.importMonoSlice(types.span(named.args)),
                .backing = if (named.backing) |backing| .{
                    .node = try self.importMono(backing.ty),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.importDeclaredFields(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentWithoutSnapshotInvalidation(node, imported);
        return node;
    }

    /// Instantiate one immutable provisional interface-replay view as fresh
    /// graph cells. Unlike `importMono`, this deliberately creates an
    /// independent copy on every call: an undetermined field-kind cell remains
    /// open evidence for the duplicate request and must not become an immutable
    /// cross-request witness or share later constraints with another duplicate.
    pub fn instantiateProvisionalTypeView(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        var imported = collections.DenseMap(Type.TypeId, NodeId).init(self.allocator);
        defer imported.deinit();
        return try self.instantiateProvisionalTypeViewInner(ty, &imported);
    }

    fn instantiateProvisionalTypeViewInner(
        self: *InstGraph,
        ty: Type.TypeId,
        imported: *collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error!NodeId {
        if (imported.get(ty)) |existing| return existing;

        const node = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        try imported.put(ty, node);
        const types = self.types;
        const imported_node: InstNode = switch (types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.instantiateProvisionalTypeViewInner(elem, imported) },
            .box => |elem| .{ .box = try self.instantiateProvisionalTypeViewInner(elem, imported) },
            .tuple => |items| .{ .tuple = try self.instantiateProvisionalTypeSlice(types.span(items), imported) },
            .func => |func| .{ .func = .{
                .args = try self.instantiateProvisionalTypeSlice(types.span(func.args), imported),
                .ret = try self.instantiateProvisionalTypeViewInner(func.ret, imported),
            } },
            .tag_union => |tags| blk: {
                const span = types.tagSpan(tags);
                if (span.len == 0) break :blk .empty_tag_union;
                const inst_tags = try self.arena().alloc(InstTag, span.len);
                for (0..span.len) |index| {
                    const tag = GuardedList.at(span, index);
                    inst_tags[index] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.instantiateProvisionalTypeSlice(types.span(tag.payloads), imported),
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
                    inst_fields[index] = switch (field.kind_state) {
                        .undetermined => undetermined: {
                            if (field.default != null) {
                                Common.invariant("provisional undetermined field carried a default identity");
                            }
                            const source_ty = field.value_ty orelse
                                Common.invariant("provisional undetermined field carried no source value type");
                            const value_node = try self.instantiateProvisionalTypeViewInner(source_ty, imported);
                            const slot_node = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
                            const kind = try self.newUndeterminedFieldKind();
                            self.registerUndeterminedFieldKindCells(kind, slot_node, value_node);
                            break :undetermined .{
                                .name = field.name,
                                .ty = slot_node,
                                .value_ty = value_node,
                                .kind = .{ .undetermined = kind },
                                .default = null,
                            };
                        },
                        .resolved => resolved: {
                            const value_ty = if (field.value_ty) |value_ty|
                                try self.instantiateProvisionalTypeViewInner(value_ty, imported)
                            else
                                null;
                            break :resolved .{
                                .name = field.name,
                                .ty = try self.instantiateProvisionalTypeViewInner(field.ty, imported),
                                .value_ty = value_ty,
                                .kind = if (value_ty != null)
                                    .optional
                                else if (field.default) |default|
                                    .{ .defaulted = default }
                                else
                                    .required,
                                .default = field.default,
                            };
                        },
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
                .args = try self.instantiateProvisionalTypeSlice(types.span(named.args), imported),
                .backing = if (named.backing) |backing| .{
                    .node = try self.instantiateProvisionalTypeViewInner(backing.ty, imported),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.instantiateProvisionalDeclaredFields(named.declared_order, imported),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentWithoutSnapshotInvalidation(node, imported_node);
        return node;
    }

    fn instantiateProvisionalTypeSlice(
        self: *InstGraph,
        tys: anytype,
        imported: *collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (0..tys.len) |index| {
            out[index] = try self.instantiateProvisionalTypeViewInner(GuardedList.at(tys, index), imported);
        }
        return out;
    }

    fn instantiateProvisionalDeclaredFields(
        self: *InstGraph,
        span: Type.Span,
        imported: *collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error![]const InstDeclaredField {
        const fields = self.types.declaredFieldSpan(span);
        if (fields.len == 0) return &.{};
        const out = try self.arena().alloc(InstDeclaredField, fields.len);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |padding_ty| .{
                    .padding = try self.instantiateProvisionalTypeViewInner(padding_ty, imported),
                },
            };
        }
        return out;
    }

    fn importMonoSlice(self: *InstGraph, tys: anytype) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (0..tys.len) |index| {
            const ty = GuardedList.at(tys, index);
            out[index] = try self.importMono(ty);
        }
        return out;
    }

    fn importDeclaredFields(self: *InstGraph, span: Type.Span) Allocator.Error![]const InstDeclaredField {
        const fields = self.types.declaredFieldSpan(span);
        if (fields.len == 0) return &.{};
        const out = try self.arena().alloc(InstDeclaredField, fields.len);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.importMono(ty) },
            };
        }
        return out;
    }

    /// Materialize an immutable Monotype-shaped view of a node under the
    /// relations produced so far, applying defaults to unresolved leaves in
    /// the view only. The live graph is unchanged. This is collision authority
    /// for provisional relation-replay memos and finalization probes; the
    /// returned graph-owned scratch TypeId must not be emitted as output.
    pub fn provisionalTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        self.requireRelationProduction();
        if (self.types.hasSpeculativeConstruction()) {
            Common.compilerBug("provisional Monotype snapshot requested inside a type transaction");
        }
        if (self.imported_monos.get(node)) |imported| return imported;
        if (try self.typeIsResolved(node)) return try self.monoFor(node);
        var snapshot = GraphTypeFinals.initProvisionalSnapshot(self);
        defer snapshot.deinit();
        return try snapshot.sealNode(self.find(node));
    }

    /// Materialize a read-only specialization-key view of a graph type whose
    /// only open evidence is generalized field presence. Undetermined fields
    /// take their relation-freeze default (`required`) in this view, while the
    /// live field-kind cells remain open for subsequent graph relations.
    pub fn specializationTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        self.requireRelationProduction();
        if (self.types.hasSpeculativeConstruction()) {
            Common.compilerBug("specialization Monotype snapshot requested inside a type transaction");
        }
        if (try self.typeIsResolved(node)) return try self.monoFor(node);
        if (!try self.typeIsSpecializationDefaultable(node)) {
            Common.invariant("specialization type view requested for a graph type with non-field-kind unresolved evidence");
        }
        var snapshot = GraphTypeFinals.initSpecializationSnapshot(self);
        defer snapshot.deinit();
        return try snapshot.sealNode(self.find(node));
    }

    /// Materialize a read-only Monotype-shaped view of a fully resolved graph
    /// node. Open rows and unresolved checked variables have no TypeId view:
    /// callers must continue to use their graph nodes until explicit evidence
    /// closes them. The returned TypeId is graph-owned scratch state and must
    /// not be written to completed Monotype output.
    pub fn activeTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        self.requireRelationProduction();
        if (self.types.hasSpeculativeConstruction()) {
            Common.compilerBug("active Monotype snapshot requested inside a type transaction");
        }
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
        var seen = self.type_set_pool.acquire();
        defer self.type_set_pool.release(&seen);
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
                    if (field.value_ty) |value_ty| {
                        if (try self.typeContainsActiveSnapshot(value_ty, seen)) break :blk true;
                    }
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
    const Mode = enum {
        final,
        active_snapshot,
        provisional_snapshot,
        specialization_snapshot,
    };

    graph: *InstGraph,
    mode: Mode,
    sealed: collections.DenseMap(NodeId, Type.TypeId),
    sealed_types: collections.DenseMap(Type.TypeId, Type.TypeId),
    active_transaction: ?Type.Store.Transaction,
    /// Keys inserted into `sealed`/`sealed_types` while `active_transaction`
    /// is open. Commit remaps exactly these entries and a failed commit
    /// evicts exactly these, so neither path scales with everything this
    /// sealer has sealed before.
    transaction_sealed_nodes: std.ArrayList(NodeId),
    transaction_sealed_types: std.ArrayList(Type.TypeId),

    pub fn init(graph: *InstGraph) GraphTypeFinals {
        graph.requireFrozenRelations();
        return initUnchecked(graph, .final);
    }

    fn initActiveSnapshot(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph, .active_snapshot);
    }

    fn initProvisionalSnapshot(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph, .provisional_snapshot);
    }

    fn initSpecializationSnapshot(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph, .specialization_snapshot);
    }

    fn initUnchecked(graph: *InstGraph, mode: Mode) GraphTypeFinals {
        return .{
            .graph = graph,
            .mode = mode,
            .sealed = collections.DenseMap(NodeId, Type.TypeId).init(graph.allocator),
            .sealed_types = collections.DenseMap(Type.TypeId, Type.TypeId).init(graph.allocator),
            .active_transaction = null,
            .transaction_sealed_nodes = .empty,
            .transaction_sealed_types = .empty,
        };
    }

    pub fn deinit(self: *GraphTypeFinals) void {
        self.transaction_sealed_types.deinit(self.graph.allocator);
        self.transaction_sealed_nodes.deinit(self.graph.allocator);
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
        if (!try self.graph.types.isInterned(self.graph.name_store, ty)) return try self.sealStoreType(ty);
        return ty;
    }

    pub fn sealNode(self: *GraphTypeFinals, raw_node: NodeId) Allocator.Error!Type.TypeId {
        const node = self.graph.find(raw_node);
        if (self.sealed.get(node)) |existing| return existing;
        if (self.mode != .final) return try self.sealNodeSpeculative(node);
        if (self.active_transaction != null) return try self.sealNodeSpeculative(node);
        if (self.graph.types.hasSpeculativeConstruction()) return try self.sealNodeSpeculative(node);

        const transaction = self.graph.types.beginTransaction();
        self.active_transaction = transaction;
        defer self.active_transaction = null;
        errdefer {
            transaction.abort(self.graph.types);
            self.evictTransactionSealed();
        }

        const speculative = try self.sealNodeSpeculative(node);
        var result = try self.graph.types.commitTransaction(self.graph.name_store, transaction, speculative);
        defer result.deinit();
        self.remapSealedTypes(result);
        return result.root;
    }

    fn sealNodeSpeculative(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.TypeId {
        if (self.sealed.get(node)) |existing| return existing;
        const Context = struct {
            sealer: *GraphTypeFinals,
            node: NodeId,

            fn fill(context: @This(), reserved: Type.TypeId) Allocator.Error!Type.Content {
                // Recorded before the put so a failed put leaves at worst a
                // recorded key with no map entry, which eviction tolerates
                // and commit never sees; the reverse order could strand a
                // speculative id in the map past a failed commit. Snapshot
                // modes commit nothing, so they record nothing.
                if (context.sealer.active_transaction != null) {
                    try context.sealer.transaction_sealed_nodes.append(context.sealer.graph.allocator, context.node);
                }
                try context.sealer.sealed.put(context.node, reserved);
                return try context.sealer.sealContent(context.node);
            }
        };
        return try self.graph.types.addRecursive(Context{ .sealer = self, .node = node }, Context.fill);
    }

    fn remapSealedTypes(self: *GraphTypeFinals, result: Type.Store.TransactionResult) void {
        for (self.transaction_sealed_nodes.items) |node| {
            const entry = self.sealed.getPtr(node) orelse
                Common.compilerBug("transaction-sealed node was missing from the sealed map at commit");
            entry.* = result.remapType(entry.*);
        }
        self.transaction_sealed_nodes.clearRetainingCapacity();
        for (self.transaction_sealed_types.items) |ty| {
            const entry = self.sealed_types.getPtr(ty) orelse
                Common.compilerBug("transaction-sealed type was missing from the sealed-types map at commit");
            entry.* = result.remapType(entry.*);
        }
        self.transaction_sealed_types.clearRetainingCapacity();
    }

    /// Drop map entries created inside a failed transaction: their sealed ids
    /// were truncated with the speculative suffix, so retaining them would
    /// hand out dangling ids if this sealer were used again.
    fn evictTransactionSealed(self: *GraphTypeFinals) void {
        for (self.transaction_sealed_nodes.items) |node| {
            _ = self.sealed.remove(node);
        }
        self.transaction_sealed_nodes.clearRetainingCapacity();
        for (self.transaction_sealed_types.items) |ty| {
            _ = self.sealed_types.remove(ty);
        }
        self.transaction_sealed_types.clearRetainingCapacity();
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
        var seen = self.graph.type_set_pool.acquire();
        defer self.graph.type_set_pool.release(&seen);
        return try self.graph.typeContainsActiveSnapshot(ty, &seen);
    }

    fn sealStoreType(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (self.sealed_types.get(ty)) |existing| return existing;
        if (self.mode != .final) return try self.sealStoreTypeSpeculative(ty);
        if (self.active_transaction != null) return try self.sealStoreTypeSpeculative(ty);
        if (self.graph.types.hasSpeculativeConstruction()) return try self.sealStoreTypeSpeculative(ty);

        const transaction = self.graph.types.beginTransaction();
        self.active_transaction = transaction;
        defer self.active_transaction = null;
        errdefer {
            transaction.abort(self.graph.types);
            self.evictTransactionSealed();
        }

        const speculative = try self.sealStoreTypeSpeculative(ty);
        var result = try self.graph.types.commitTransaction(self.graph.name_store, transaction, speculative);
        defer result.deinit();
        self.remapSealedTypes(result);
        return result.root;
    }

    fn sealStoreTypeSpeculative(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (self.sealed_types.get(ty)) |existing| return existing;
        const Context = struct {
            sealer: *GraphTypeFinals,
            ty: Type.TypeId,

            fn fill(context: @This(), reserved: Type.TypeId) Allocator.Error!Type.Content {
                // See `sealNodeSpeculative` for the record-before-put order.
                if (context.sealer.active_transaction != null) {
                    try context.sealer.transaction_sealed_types.append(context.sealer.graph.allocator, context.ty);
                }
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
            if (field.kind == .undetermined and self.graph.resolvedFieldKind(field.kind) == null) {
                const source_node = field.value_ty orelse
                    Common.invariant("undetermined graph field carried no source value type");
                const source_ty = try self.sealNode(source_node);
                fields[index] = switch (self.mode) {
                    .provisional_snapshot => .{
                        .name = field.name,
                        // No runtime slot exists yet. The explicit kind state
                        // makes this a provisional structural cell, so mirror
                        // the source type instead of materializing the
                        // unresolved slot node.
                        .ty = source_ty,
                        .value_ty = source_ty,
                        .kind_state = .undetermined,
                        .default = null,
                    },
                    .specialization_snapshot => .{
                        .name = field.name,
                        .ty = source_ty,
                        .value_ty = null,
                        .kind_state = .resolved,
                        .default = null,
                    },
                    .final, .active_snapshot => Common.invariant("unresolved record field kind reached Monotype sealing"),
                };
                continue;
            }
            fields[index] = .{
                .name = field.name,
                .ty = try self.sealNode(field.ty),
                .value_ty = if (self.graph.resolvedFieldKind(field.kind)) |kind| switch (kind) {
                    .optional => try self.sealNode(field.value_ty orelse
                        Common.invariant("optional graph field carried no source value type")),
                    .required, .defaulted => null,
                } else if (field.value_ty) |value_ty|
                    try self.sealNode(value_ty)
                else
                    null,
                .kind_state = .resolved,
                .default = field.default,
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
            if (field.value_ty) |value_ty| field.value_ty = try self.sealType(value_ty);
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

const OpenFunctionInterfaceShapeWriter = struct {
    graph: *InstGraph,
    hasher: std.crypto.hash.sha2.Sha256,
    unresolved_ids: collections.DenseMap(NodeId, u32),
    visiting: std.ArrayList(NodeId),
    next_unresolved: u32 = 0,
    output: ?[]u8 = null,
    output_len: usize = 0,

    fn init(graph: *InstGraph) OpenFunctionInterfaceShapeWriter {
        return .{
            .graph = graph,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .unresolved_ids = collections.DenseMap(NodeId, u32).init(graph.allocator),
            .visiting = .empty,
        };
    }

    fn initWithOutput(graph: *InstGraph, output: []u8) OpenFunctionInterfaceShapeWriter {
        var writer = init(graph);
        writer.output = output;
        return writer;
    }

    fn deinit(self: *OpenFunctionInterfaceShapeWriter) void {
        self.visiting.deinit(self.graph.allocator);
        self.unresolved_ids.deinit();
    }

    fn writeFunctionInterface(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) Allocator.Error!void {
        self.writeBytes("roc.monotype.open_function_interface_shape.v2");
        try self.writeFunctionNodes(try self.graph.functionNodes(node));
        if (self.graph.requestSourceInterface(node)) |source| {
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
        self.writeU8(if (self.hasRecursiveValueSlot(node)) 1 else 0);
        self.writeU8(if (self.hasForcedDynamicIteratorRoot(node)) 1 else 0);
        if (content == .redirect) unreachable;
        if (content == .unresolved) {
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

    fn hasRecursiveValueSlot(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) bool {
        for (self.graph.recursive_argument_slots.items) |slot| {
            if (self.graph.find(slot) == node) return true;
        }
        return false;
    }

    fn hasForcedDynamicIteratorRoot(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) bool {
        for (self.graph.forced_dynamic_iterator_roots.items) |root| {
            if (self.graph.find(root) == node) return true;
        }
        return false;
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
        if (left_field.name != right_field.name or left_field.ty != right_field.ty or
            left_field.value_ty != right_field.value_ty or !std.meta.eql(left_field.kind, right_field.kind)) return false;
        if (!instFieldDefaultEql(left_field.default, right_field.default)) return false;
    }
    return true;
}

// The program name store interns module identities by full 256-bit value, so
// id equality is identity equality within one graph.
fn instFieldDefaultEql(left: ?Type.FieldDefault, right: ?Type.FieldDefault) bool {
    const left_default = left orelse return right == null;
    const right_default = right orelse return false;
    return left_default.module == right_default.module and left_default.expr_node == right_default.expr_node;
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
    try std.testing.expect(!try graph.containsGeneratedPrivate(boolean));
    try std.testing.expect(!try graph.containsFinishedMono(boolean));

    try std.testing.expectEqual(@as(u64, 3), diagnostics.nodes_created);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.unify_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.class_unions);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.active_type_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_cache_hits);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_cache_misses);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_nodes_materialized);
    try std.testing.expect(diagnostics.active_snapshot_invalidations >= 1);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.active_snapshot_entries_invalidated);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_scans);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_nodes_visited);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.finished_mono_scans);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.finished_mono_nodes_visited);
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
    try graph.registerRequestSourceInterface(private_left_request, source_bool);
    try graph.registerRequestSourceInterface(private_right_request, source_str);

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

test "provisional Monotype view preserves an undetermined record field" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("value");
    const value_ty = try graph.newNode(.{ .primitive = .u64 });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const kind = try graph.newUndeterminedFieldKind();
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{
        .name = field_name,
        .ty = slot,
        .value_ty = value_ty,
        .kind = .{ .undetermined = kind },
        .default = null,
    };
    const record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });

    const provisional = try graph.provisionalTypeViewForNode(record);
    const provisional_fields = type_store.fieldSpan(type_store.get(provisional).record);
    try std.testing.expectEqual(@as(usize, 1), provisional_fields.len);
    const provisional_value_ty = GuardedList.at(provisional_fields, 0).value_ty orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(provisional_value_ty));
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
    fields[0] = .{ .name = field_name, .ty = field_fn, .default = null };
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
    fields[0] = .{ .name = field_name, .ty = function, .default = null };
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
    fields[0] = .{ .name = field_name, .ty = field_ty, .default = null };
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
    fields[0] = .{ .name = a_name, .ty = a_ty, .default = null };
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

test "final sealing follows active snapshots stored only in field value types" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("optional");
    const value_node = try graph.newNode(.{ .primitive = .u64 });
    const value_snapshot = try graph.activeTypeViewForNode(value_node);
    const slot_ty = try type_store.add(.zst);
    const wrapper = try type_store.add(.{ .record = try type_store.addRecordFields(&name_store, &.{
        .{
            .name = field_name,
            .ty = slot_ty,
            .value_ty = value_snapshot,
            .kind_state = .resolved,
            .default = null,
        },
    }) });

    try std.testing.expect(try graph.typeHasActiveSnapshots(wrapper));
    try graph.setContent(value_node, .{ .primitive = .str });

    try graph.freezeRelations();
    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed = try finals.sealType(wrapper);

    try std.testing.expect(sealed != wrapper);
    const original_field = GuardedList.at(type_store.fieldSpan(type_store.get(wrapper).record), 0);
    const sealed_field = GuardedList.at(type_store.fieldSpan(type_store.get(sealed).record), 0);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(original_field.value_ty.?));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(sealed_field.value_ty.?));
    try std.testing.expect(!(try graph.typeHasActiveSnapshots(sealed)));
}

test "final sealing interns raw types without active snapshots" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const unit = try type_store.internZst(&name_store);
    const raw_list = try type_store.add(.{ .list = unit });
    try std.testing.expect(!(try type_store.isInterned(&name_store, raw_list)));

    try graph.freezeRelations();
    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed = try finals.sealType(raw_list);

    try std.testing.expect(try type_store.isInterned(&name_store, sealed));
    const types_len = type_store.view().types.len;
    try std.testing.expectEqual(sealed, try type_store.internList(&name_store, unit));
    try std.testing.expectEqual(types_len, type_store.view().types.len);
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
    fields[0] = .{ .name = a_name, .ty = a_ty, .default = null };
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
    fields[0] = .{ .name = a_name, .ty = a_ty, .default = null };
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

test "generated-private traversal scratch handles cycles and epoch rollover" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    const Context = struct {
        fn fill(_: @This(), reserved: NodeId) Allocator.Error!InstNode {
            return .{ .box = reserved };
        }
    };
    const recursive = try graph.addRecursiveNode(Context{}, Context.fill);
    graph.containment_visit_epoch = std.math.maxInt(u32);
    @memset(graph.containment_visit_epochs.items, std.math.maxInt(u32));

    try std.testing.expect(!try graph.containsGeneratedPrivate(recursive));
    try std.testing.expectEqual(@as(u32, 1), graph.containment_visit_epoch);

    try std.testing.expect(!try graph.containsGeneratedPrivate(recursive));
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_cache_hits);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_nodes_visited);

    const unrelated = try graph.newNode(.{ .primitive = .u64 });
    try graph.setContent(unrelated, .{ .primitive = .str });
    try std.testing.expect(!try graph.containsGeneratedPrivate(recursive));
    try std.testing.expectEqual(@as(u64, 2), diagnostics.generated_private_cache_hits);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_nodes_visited);

    try graph.setContent(recursive, .zst);
    try std.testing.expect(!try graph.containsGeneratedPrivate(recursive));
    try std.testing.expectEqual(@as(u64, 2), diagnostics.generated_private_cache_hits);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.generated_private_nodes_visited);
}

test "generated-private containment follows optional field value types" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x47} ** 32));
    const type_name = try name_store.internTypeName("PrivateValue");
    const private_value = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(31) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .inspectable,
            .authority = .generated_private,
        },
    } });
    const slot = try graph.newNode(.zst);
    const field_name = try name_store.internRecordFieldLabel("optional");
    const fields = try graph.arena().dupe(InstField, &.{.{
        .name = field_name,
        .ty = slot,
        .value_ty = private_value,
        .kind = .optional,
        .default = null,
    }});
    const record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });

    try std.testing.expect(try graph.containsGeneratedPrivate(record));
}

test "iterator-interface containment caches exact graph dependencies" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    const child = try graph.newNode(.{ .primitive = .u64 });
    const root = try graph.newNode(.{ .box = child });
    graph.containment_visit_epoch = std.math.maxInt(u32);
    @memset(graph.containment_visit_epochs.items, std.math.maxInt(u32));

    try std.testing.expect(!try graph.containsIteratorInterface(root));
    try std.testing.expectEqual(@as(u32, 1), graph.containment_visit_epoch);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.iterator_interface_nodes_visited);

    try std.testing.expect(!try graph.containsIteratorInterface(root));
    try std.testing.expectEqual(@as(u64, 1), diagnostics.iterator_interface_cache_hits);

    const unrelated = try graph.newNode(.{ .primitive = .u64 });
    try graph.setContent(unrelated, .{ .primitive = .str });
    try std.testing.expect(!try graph.containsIteratorInterface(root));
    try std.testing.expectEqual(@as(u64, 2), diagnostics.iterator_interface_cache_hits);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.iterator_interface_nodes_visited);

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x42} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    try graph.setContent(child, .{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(14) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = &.{},
        .backing = null,
    } });
    try std.testing.expect(try graph.containsIteratorInterface(root));
    try std.testing.expectEqual(@as(u64, 2), diagnostics.iterator_interface_cache_hits);
    try std.testing.expectEqual(@as(u64, 4), diagnostics.iterator_interface_nodes_visited);
}

test "iterator-interface containment agrees between Monotype and graph" {
    // `Type.Store.containsIteratorInterface` and
    // `InstGraph.containsIteratorInterface` are separate walks over separate
    // representations, and the Monotype walk gates skipping graph
    // construction entirely. A structural position one descends into and the
    // other does not would silently drop a producer's minted representation,
    // so every container position is checked in both, with an iterator leaf
    // and a plain leaf.
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x51} ** 32));
    const iter_name = try name_store.internTypeName("Iter");
    const wrapper_name = try name_store.internTypeName("Wrapper");
    const field_name = try name_store.internRecordFieldLabel("it");
    const tag_name = try name_store.internTagLabel("Holds");

    const Shapes = struct {
        graph: *InstGraph,
        module_identity: names.ModuleIdentityId,
        wrapper_name: names.TypeNameId,
        field_name: names.RecordFieldNameId,
        tag_name: names.TagNameId,

        fn wrap(self: @This(), leaf: NodeId, position: usize) Allocator.Error!NodeId {
            const u64_node = try self.graph.newNode(.{ .primitive = .u64 });
            return switch (position) {
                0 => leaf,
                1 => try self.graph.newNode(.{ .list = leaf }),
                2 => try self.graph.newNode(.{ .box = leaf }),
                3 => try self.graph.newNode(.{ .tuple = try self.graph.arena().dupe(NodeId, &.{ u64_node, leaf }) }),
                4 => blk: {
                    const fields = try self.graph.arena().dupe(InstField, &.{.{ .name = self.field_name, .ty = leaf, .default = null }});
                    break :blk try self.graph.newNode(.{ .record = .{
                        .fields = fields,
                        .ext = try self.graph.newNode(.empty_record),
                    } });
                },
                5 => blk: {
                    const payloads = try self.graph.arena().dupe(NodeId, &.{leaf});
                    const tags = try self.graph.arena().dupe(InstTag, &.{.{
                        .name = self.tag_name,
                        .checked_name = self.tag_name,
                        .payloads = payloads,
                    }});
                    break :blk try self.graph.newNode(.{ .tag_union = .{
                        .tags = tags,
                        .ext = try self.graph.newNode(.empty_tag_union),
                    } });
                },
                6 => try self.graph.newNode(.{ .func = .{
                    .args = try self.graph.arena().dupe(NodeId, &.{leaf}),
                    .ret = u64_node,
                } }),
                // A nominal wrapper reaching the leaf through its backing.
                7 => try self.graph.newNode(.{ .named = .{
                    .named_type = .{ .module = .{}, .ty = testCheckedTypeId(21) },
                    .def = .{ .module = self.module_identity, .type_name = self.wrapper_name },
                    .kind = .nominal,
                    .builtin_owner = null,
                    .args = try self.graph.arena().dupe(NodeId, &.{}),
                    .backing = .{ .node = leaf, .use = .inspectable },
                } }),
                // A nominal wrapper reaching the leaf through a type argument.
                8 => try self.graph.newNode(.{ .named = .{
                    .named_type = .{ .module = .{}, .ty = testCheckedTypeId(22) },
                    .def = .{ .module = self.module_identity, .type_name = self.wrapper_name },
                    .kind = .nominal,
                    .builtin_owner = null,
                    .args = try self.graph.arena().dupe(NodeId, &.{leaf}),
                    .backing = .{ .node = u64_node, .use = .inspectable },
                } }),
                // An optional record slot reaching the leaf only through its
                // retained source value type.
                9 => blk: {
                    const fields = try self.graph.arena().dupe(InstField, &.{.{
                        .name = self.field_name,
                        .ty = u64_node,
                        .value_ty = leaf,
                        .kind = .optional,
                        .default = null,
                    }});
                    break :blk try self.graph.newNode(.{ .record = .{
                        .fields = fields,
                        .ext = try self.graph.newNode(.empty_record),
                    } });
                },
                else => unreachable,
            };
        }
    };
    const shapes = Shapes{
        .graph = graph,
        .module_identity = module_identity,
        .wrapper_name = wrapper_name,
        .field_name = field_name,
        .tag_name = tag_name,
    };

    const position_count = 10;
    const case_count = position_count * 2;
    var roots: [case_count]NodeId = undefined;
    var graph_answers: [case_count]bool = undefined;

    for (0..position_count) |position| {
        for ([_]bool{ true, false }, 0..) |iterator_leaf, leaf_index| {
            const leaf = if (iterator_leaf) try graph.newNode(.{ .named = .{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(20) },
                .def = .{ .module = module_identity, .type_name = iter_name },
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{try graph.newNode(.{ .primitive = .u64 })}),
                .backing = .{
                    .node = try graph.newNode(.{ .primitive = .u64 }),
                    .use = .runtime_layout_only,
                },
            } }) else try graph.newNode(.{ .primitive = .str });

            const case_index = position * 2 + leaf_index;
            roots[case_index] = try shapes.wrap(leaf, position);
            graph_answers[case_index] = try graph.containsIteratorInterface(roots[case_index]);
        }
    }

    // Sealing is only allowed once relation production has finished, so every
    // shape is built and asked of the graph first, then compared.
    try graph.freezeRelations();
    for (roots, graph_answers, 0..) |root, graph_answer, case_index| {
        const iterator_leaf = case_index % 2 == 0;
        const sealed = try graph.sealNode(root);
        const mono_answer = try type_store.containsIteratorInterface(sealed);
        if (graph_answer != mono_answer) {
            std.debug.print(
                "position {d} iterator_leaf={} graph={} mono={}\n",
                .{ case_index / 2, iterator_leaf, graph_answer, mono_answer },
            );
        }
        try std.testing.expectEqual(graph_answer, mono_answer);
        try std.testing.expectEqual(iterator_leaf, graph_answer);
    }
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

test "construction row relation absorbs only explicit optional or defaulted fields into closed records" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const a = try name_store.internRecordFieldLabel("a");
    const b = try name_store.internRecordFieldLabel("b");
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const u64_node = try graph.newNode(.{ .primitive = .u64 });
    const left_ext = try graph.newNode(.empty_record);
    const right_ext = try graph.newNode(.empty_record);
    const left = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = a,
            .ty = u64_node,
            .kind = .required,
            .default = null,
        }}),
        .ext = left_ext,
    } });
    const right_only = [_]InstField{.{
        .name = b,
        .ty = u64_node,
        .value_ty = u64_node,
        .kind = .optional,
        .default = null,
    }};
    const right = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{
            .{
                .name = a,
                .ty = u64_node,
                .kind = .required,
                .default = null,
            },
            right_only[0],
        }),
        .ext = right_ext,
    } });

    try std.testing.expect(!graph.closedRecordAbsorbsFields(left_ext, &right_only, .exact));
    try std.testing.expect(graph.closedRecordAbsorbsFields(left_ext, &right_only, .construction));
    const default_identity: Type.FieldDefault = .{
        .module = try name_store.internModuleIdentity(&([_]u8{0xA5} ** 32)),
        .expr_node = 1,
    };
    const defaulted_only = [_]InstField{.{
        .name = b,
        .ty = u64_node,
        .kind = .{ .defaulted = default_identity },
        .default = default_identity,
    }};
    try std.testing.expect(graph.closedRecordAbsorbsFields(left_ext, &defaulted_only, .construction));
    const required_only = [_]InstField{.{
        .name = b,
        .ty = u64_node,
        .kind = .required,
        .default = null,
    }};
    try std.testing.expect(!graph.closedRecordAbsorbsFields(left_ext, &required_only, .construction));

    try graph.unifyConstruction(left, right);
    try std.testing.expect(graph.sameClass(left, right));
    const merged = try graph.flattenRecordRow(left);
    try std.testing.expectEqual(@as(usize, 2), merged.fields.len);

    const empty = try graph.newNode(.empty_record);
    const optional_only = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &right_only),
        .ext = try graph.newNode(.empty_record),
    } });
    try graph.unifyConstruction(empty, optional_only);
    try std.testing.expect(graph.sameClass(empty, optional_only));
    const absorbed_empty = try graph.flattenRecordRow(empty);
    try std.testing.expectEqual(@as(usize, 1), absorbed_empty.fields.len);
    try std.testing.expectEqual(b, absorbed_empty.fields[0].name);
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
        .default = null,
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
        .default = null,
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

test "finished Monotype detection includes imported structural descendants" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const imported_item_ty = try type_store.add(.{ .primitive = .u64 });
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const imported_item = try graph.importMono(imported_item_ty);
    const fresh_list = try graph.newNode(.{ .list = imported_item });
    const fresh_tuple = try graph.newNode(.{ .tuple = try graph.arena().dupe(NodeId, &.{fresh_list}) });
    try std.testing.expect(try graph.containsFinishedMono(fresh_tuple));

    const fresh_item = try graph.newNode(.{ .primitive = .u64 });
    const entirely_fresh = try graph.newNode(.{ .list = fresh_item });
    try std.testing.expect(!try graph.containsFinishedMono(entirely_fresh));
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

    try graph.relateOpaqueInterface(public, private);

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
    fields[0] = .{ .name = field_name, .ty = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) }), .default = null };
    const structural_record = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) }),
    } });
    const projected = try graph.recordNodes(structural_record);
    try std.testing.expectEqual(@as(usize, 1), projected.fields.len);
    try std.testing.expectEqual(field_name, projected.fields[0].name);
}

test "construction selection preserves private evidence while absorbing optional width" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xA7} ** 32));
    const type_name = try name_store.internTypeName("PrivateEvidence");
    const evidence_field = try name_store.internRecordFieldLabel("evidence");
    const optional_field = try name_store.internRecordFieldLabel("optional");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(18) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const public_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const private_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public_evidence = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{public_arg}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    } });
    const private_evidence = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{private_arg}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } });
    const optional_ty = try graph.newNode(.{ .primitive = .u64 });
    const public_record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{
            .{
                .name = evidence_field,
                .ty = public_evidence,
                .kind = .required,
                .default = null,
            },
            .{
                .name = optional_field,
                .ty = optional_ty,
                .value_ty = optional_ty,
                .kind = .optional,
                .default = null,
            },
        }),
        .ext = try graph.newNode(.empty_record),
    } });
    const private_record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = evidence_field,
            .ty = private_evidence,
            .kind = .required,
            .default = null,
        }}),
        .ext = try graph.newNode(.empty_record),
    } });

    try graph.selectGeneratedPrivateConstructionRepresentation(public_record, private_record);

    try std.testing.expect(graph.sameClass(public_record, private_record));
    try std.testing.expect(graph.sameClass(public_evidence, private_evidence));
    try std.testing.expect(graph.sameClass(public_arg, private_arg));
    try std.testing.expectEqual(@as(usize, 2), (try graph.flattenRecordRow(public_record)).fields.len);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(public_evidence).named.backing.?.authority);
}

test "named type relation to its own backing preserves the backing edge" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x17} ** 32));
    const type_name = try name_store.internTypeName("State");
    const field_name = try name_store.internRecordFieldLabel("value");
    const field_ty = try graph.newNode(.{ .primitive = .u64 });
    const fields = try graph.arena().dupe(InstField, &.{.{ .name = field_name, .ty = field_ty, .default = null }});
    const backing = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = try graph.newNode(.empty_record),
    } });
    const named = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    } });

    try graph.unify(named, backing);

    try std.testing.expect(!graph.sameClass(named, backing));
    const retained = graph.content(named).named.backing.?;
    try std.testing.expectEqual(backing, retained.node);
    try std.testing.expectEqual(Type.BackingUse.runtime_layout_only, retained.use);
    try std.testing.expectEqual(@as(usize, 1), (try graph.recordConstructionNodes(named)).fields.len);
}

test "record row follows an inspectable nominal record extension" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x29} ** 32));
    const type_name = try name_store.internTypeName("Vec2");
    const x = try name_store.internRecordFieldLabel("x");
    const y = try name_store.internRecordFieldLabel("y");
    const z = try name_store.internRecordFieldLabel("z");
    const f32_node = try graph.newNode(.{ .primitive = .f32 });
    const empty = try graph.newNode(.empty_record);
    const backing_fields = try graph.arena().dupe(InstField, &.{
        .{ .name = x, .ty = f32_node, .default = null },
        .{ .name = y, .ty = f32_node, .default = null },
    });
    const backing = try graph.newNode(.{ .record = .{ .fields = backing_fields, .ext = empty } });
    const nominal = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .inspectable },
    } });
    const outer_fields = try graph.arena().dupe(InstField, &.{.{ .name = z, .ty = f32_node, .default = null }});
    const outer = try graph.newNode(.{ .record = .{ .fields = outer_fields, .ext = nominal } });

    const flattened = try graph.flattenRecordRow(outer);

    try std.testing.expectEqual(@as(usize, 3), flattened.fields.len);
    try std.testing.expectEqual(empty, flattened.ext);
}

test "opaque interface relation preserves forced-dynamic iterator identity" {
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

    try graph.relateOpaqueInterface(public, private);

    try std.testing.expect(!graph.sameClass(public, private));
    try std.testing.expect(graph.sameClass(public_item, private_item));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, graph.content(public).named.backing.?.authority);
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(private).named.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.none, graph.content(public).named.def.iterator_representation);
    try std.testing.expectEqual(Type.IteratorRepresentation.forced_dynamic, graph.content(private).named.def.iterator_representation);
}

test "opaque iterator relation materializes unresolved public interface from provenance" {
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
        .args = try graph.arena().dupe(NodeId, &.{ private_item, try graph.newNode(.empty_record) }),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .public_source = public_source,
        },
    } });

    try graph.relateOpaqueInterface(public, private);

    const retained_public = graph.content(public).named;
    try std.testing.expect(!graph.sameClass(public, private));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.none, retained_public.def.iterator_representation);
    try std.testing.expectEqual(@as(usize, 1), retained_public.args.len);
    try std.testing.expect(graph.sameClass(retained_public.args[0], private_item));
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

    try graph.relateOpaqueInterface(public, private);

    const retained = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, private));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, retained.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, retained.def.iterator_representation);
    try std.testing.expectEqual(@as(usize, 1), retained.args.len);
    try std.testing.expect(graph.sameClass(retained.args[0], item));
}

test "opaque interface relation delegates nested private iterator requests to unification" {
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
    const left_component = try graph.newNode(.empty_record);
    const right_component = try graph.newNode(.empty_record);
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
        .args = try graph.arena().dupe(NodeId, &.{ item, left_component }),
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
            .generated = .{ .bytes = [_]u8{0x75} ** 32 },
            .iterator_representation = .minted,
            .iterator_kind = .concat,
            .iterator_depth = 2,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{ item, right_component }),
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

    try graph.relateOpaqueInterface(public_fn, private_fn);

    try std.testing.expect(graph.sameClass(left_iter, right_iter));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, graph.content(left_iter).named.backing.?.authority);
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, graph.content(left_iter).named.def.iterator_representation);
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
        .args = try graph.arena().dupe(NodeId, &.{ item, try graph.newNode(.empty_record) }),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
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

    try graph.relateOpaqueInterface(public, request);

    const retained_public = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, request));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(@as(usize, 1), retained_public.args.len);
    try std.testing.expect(graph.sameClass(retained_public.args[0], private_arg));
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
        .args = try graph.arena().dupe(NodeId, &.{ item, wide_component }),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
        .generated_iterator = .{
            .callable_evidence = null,
            .public_source = public_source,
        },
    } });

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
            .public_source = public_source,
        },
    } });

    // This order is significant: the finished type is the left side of the
    // recursive join, but only the graph-owned side can author the final
    // forced-dynamic representation.
    try graph.markRecursiveValueSlot(finished);
    try graph.unify(finished, owned);
    try graph.finalizeGeneratedIteratorRepresentations();

    const finalized = graph.content(finished).named;
    try std.testing.expect(finalized.generated_iterator != null);
    try std.testing.expectEqual(Type.IteratorRepresentation.forced_dynamic, finalized.def.iterator_representation);
    try std.testing.expectEqual(Type.IteratorKind.forced_dynamic, finalized.def.iterator_kind);
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

    try graph.relateOpaqueInterface(public_fn, private_fn);

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
