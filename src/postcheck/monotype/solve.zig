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
    /// Stable checked identity for a polymorphic substitution slot. This is
    /// absent on graph-only variables created by tests and compiler-owned
    /// structural work.
    checked_key: ?[32]u8 = null,

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

    pub fn checkedVariableAtKey(
        numeric_default_phase: ?checked.NumericDefaultPhase,
        row_default: ?checked.RowDefault,
        checked_key: [32]u8,
    ) InstVariable {
        return .{
            .origin = .checked_variable,
            .numeric_default_phase = numeric_default_phase,
            .row_default = row_default,
            .checked_key = checked_key,
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
pub const InstNamed = struct {
    named_type: Type.NamedType,
    def: Type.TypeDef,
    kind: Type.NamedKind,
    builtin_owner: ?static_dispatch.BuiltinOwner,
    args: []NodeId,
    backing: ?InstBacking,
    /// Declared field order for a nominal/opaque record backing (empty
    /// otherwise). Padding field types are graph nodes so sealing maps them to
    /// immutable type ids with the rest of the named type.
    declared_order: []const InstDeclaredField = &.{},
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
const InstFunction = struct {
    args: []NodeId,
    ret: NodeId,
};

pub const InstNode = union(enum) {
    redirect: NodeId,
    unresolved: InstVariable,
    primitive: Type.Primitive,
    list: NodeId,
    box: NodeId,
    tuple: []NodeId,
    func: InstFunction,
    tag_union: InstTagUnion,
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

const InstTagUnion = struct {
    tags: []InstTag,
    ext: NodeId,
};

/// Graph-native function shape. These nodes remain live until their owning
/// specialization graph is sealed.
pub const FunctionNodes = struct {
    args: []const NodeId,
    ret: NodeId,
};

/// Authority for a function request's result edge. An exact destination is
/// supplied by an enclosing storage or control-flow boundary. A produced
/// result has no destination yet; the callee body owns the exact node that
/// will complete its reserved forward cell.
pub const FunctionResultRelation = enum(u8) {
    exact_destination,
    produced,
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
    generated_identity_input_nodes_hashed: u64 = 0,
    generated_identity_intern_hits: u64 = 0,
    generated_identity_intern_misses: u64 = 0,
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

/// Stable identity of one node in an immutable checked-type base. Checked type
/// ids are dense only within one module artifact, so the module identity is an
/// inseparable part of every specialization-substitution key.
pub const CheckedBaseKey = struct {
    module_bytes: [32]u8,
    checked: checked.CheckedTypeId,
};

/// One checker-published call slot and the one exact node produced for it.
/// Selection is single-assignment: another producer must name the same graph
/// class rather than competing by priority.
pub const DirectRequestSelection = struct {
    base: CheckedBaseKey,
    produced: NodeId,
};

const DirectRequestSelectionSpan = struct {
    start: u32,
    len: u32,

    const uninitialized_len = std.math.maxInt(u32);
    const uninitialized: DirectRequestSelectionSpan = .{ .start = 0, .len = uninitialized_len };

    fn isInitialized(self: DirectRequestSelectionSpan) bool {
        return self.len != uninitialized_len;
    }
};

/// Result of hashing one complete generated-nominal construction request.
/// A vacant result carries the digest into registration so a cache miss never
/// hashes the same inputs twice.
pub const GeneratedNominalLookup = struct {
    existing: ?NodeId,
    digest: names.TypeDigest,
};

pub const GeneratedIteratorLookup = GeneratedNominalLookup;

const RelationStamp = struct {
    left: NodeId,
    left_version: u32,
    right: NodeId,
    right_version: u32,
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

const GeneratedNominalInternContext = struct {
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
    /// Ordinary primitive types are atomic identities. Reusing one node per
    /// primitive keeps independently encountered exact values identical
    /// without hashing or comparing any enclosing type graph.
    primitive_nodes: [std.meta.fields(Type.Primitive).len]?NodeId,
    empty_tag_union_node: ?NodeId,
    empty_record_node: ?NodeId,
    zst_node: ?NodeId,
    /// Produced tag unions are immutable compounds of exact child nodes.
    /// Hash-consing their one-level shape prevents independently encountered
    /// values with the same children from creating distinct specialization
    /// identities. Checked-base construction remains scope-local and is not
    /// interned here.
    produced_tag_unions: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    checked_base_nodes: std.ArrayList(bool),
    checked_base_construction_depth: usize,
    versions: std.ArrayList(u32),
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
    /// Generated nominals keyed by the final content digest assigned by their
    /// producer. Identity, arguments, and backing are complete before entry.
    generated_nominal_intern: std.HashMap(names.TypeDigest, NodeId, GeneratedNominalInternContext, 80),
    /// Canonical graph node for each completed nominal identity. A nominal's
    /// definition and exact argument nodes determine its type; the backing is
    /// implementation data owned by that identity, not another type axis.
    named_nodes_by_identity_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Canonical compound nodes keyed only by their immediate exact children.
    /// Producers therefore share an already-built type without traversing any
    /// descendant graph.
    list_nodes_by_element: collections.DenseMap(NodeId, NodeId),
    box_nodes_by_element: collections.DenseMap(NodeId, NodeId),
    tuple_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Completed function values keyed by their exact immediate argument and
    /// result nodes. Open requests remain distinct until their producer fills
    /// the result edge; completion canonicalizes the one finished shape once.
    function_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    record_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Fast producer lookup by the already-completed dense item node. Buckets
    /// distinguish declarations without re-hashing the item type graph.
    generated_iterators_by_item: collections.DenseMap(NodeId, std.ArrayList(NodeId)),
    /// Permanent roots recorded by the producer so sealing can commit each
    /// completed nominal to the cross-specialization TypeId interner.
    generated_nominal_nodes: std.ArrayList(NodeId),
    /// Exact checked source function node from which a distinct produced
    /// function request was constructed. This is explicit substitution input;
    /// consumers read this field instead of deriving it from the produced
    /// function's type shape.
    request_checked_sources: std.ArrayList(?NodeId),
    /// Explicit result authority for function requests. This is a dense
    /// NodeId column because request identity is graph-local and every node
    /// already has a stable dense ordinal.
    function_result_relations: std.ArrayList(?FunctionResultRelation),
    /// Immutable flat substitutions selected by the checker's projection
    /// program for each function request.
    direct_request_selection_spans: std.ArrayList(DirectRequestSelectionSpan),
    direct_request_selections: std.ArrayList(DirectRequestSelection),
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
            .primitive_nodes = @splat(null),
            .empty_tag_union_node = null,
            .empty_record_node = null,
            .zst_node = null,
            .produced_tag_unions = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .checked_base_nodes = .empty,
            .checked_base_construction_depth = 0,
            .versions = .empty,
            .processed_relations = std.AutoHashMap(RelationStamp, void).init(allocator),
            .node_snapshots = collections.DenseMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .current_snapshots = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .current_snapshots_dirty = false,
            .linked_type_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .imported_monos = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .row_exts = .empty,
            .row_parents = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .nominal_backings = std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingCacheContext, 80).init(allocator),
            .generated_nominal_intern = std.HashMap(names.TypeDigest, NodeId, GeneratedNominalInternContext, 80).init(allocator),
            .named_nodes_by_identity_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .list_nodes_by_element = collections.DenseMap(NodeId, NodeId).init(allocator),
            .box_nodes_by_element = collections.DenseMap(NodeId, NodeId).init(allocator),
            .tuple_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .function_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .record_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .generated_iterators_by_item = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .generated_nominal_nodes = .empty,
            .request_checked_sources = .empty,
            .function_result_relations = .empty,
            .direct_request_selection_spans = .empty,
            .direct_request_selections = .empty,
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
        var generated_item_buckets = self.generated_iterators_by_item.valueIterator();
        while (generated_item_buckets.next()) |bucket| bucket.deinit(allocator);
        self.generated_iterators_by_item.deinit();
        self.generated_nominal_intern.deinit();
        var named_identity_buckets = self.named_nodes_by_identity_hash.valueIterator();
        while (named_identity_buckets.next()) |bucket| bucket.deinit(allocator);
        self.named_nodes_by_identity_hash.deinit();
        self.list_nodes_by_element.deinit();
        self.box_nodes_by_element.deinit();
        var tuple_shape_buckets = self.tuple_nodes_by_shape_hash.valueIterator();
        while (tuple_shape_buckets.next()) |bucket| bucket.deinit(allocator);
        self.tuple_nodes_by_shape_hash.deinit();
        var function_shape_buckets = self.function_nodes_by_shape_hash.valueIterator();
        while (function_shape_buckets.next()) |bucket| bucket.deinit(allocator);
        self.function_nodes_by_shape_hash.deinit();
        var record_shape_buckets = self.record_nodes_by_shape_hash.valueIterator();
        while (record_shape_buckets.next()) |bucket| bucket.deinit(allocator);
        self.record_nodes_by_shape_hash.deinit();
        self.generated_nominal_nodes.deinit(allocator);
        self.direct_request_selections.deinit(allocator);
        self.direct_request_selection_spans.deinit(allocator);
        self.request_checked_sources.deinit(allocator);
        self.function_result_relations.deinit(allocator);
        self.row_parents.deinit();
        self.row_exts.deinit(allocator);
        self.imported_monos.deinit();
        self.linked_type_nodes.deinit();
        self.processed_relations.deinit();
        var produced_tag_buckets = self.produced_tag_unions.valueIterator();
        while (produced_tag_buckets.next()) |bucket| bucket.deinit(allocator);
        self.produced_tag_unions.deinit();
        self.versions.deinit(allocator);
        self.checked_base_nodes.deinit(allocator);
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

    pub fn registerFunctionResultRelation(
        self: *InstGraph,
        request_fn: NodeId,
        relation: FunctionResultRelation,
    ) void {
        self.requireRelationProduction();
        const entry = &self.function_result_relations.items[@intFromEnum(request_fn)];
        if (entry.*) |existing| {
            if (existing != relation) {
                Common.invariant("function request was registered with two result authorities");
            }
        } else {
            entry.* = relation;
        }
    }

    pub fn functionResultRelation(self: *const InstGraph, request_fn: NodeId) ?FunctionResultRelation {
        return self.function_result_relations.items[@intFromEnum(request_fn)];
    }

    pub fn inheritFunctionResultRelation(self: *InstGraph, source_fn: NodeId, destination_fn: NodeId) void {
        const relation = self.functionResultRelation(source_fn) orelse return;
        self.registerFunctionResultRelation(destination_fn, relation);
    }

    pub fn directRequestSelections(self: *const InstGraph, request_fn: NodeId) []const DirectRequestSelection {
        const span = self.direct_request_selection_spans.items[@intFromEnum(request_fn)];
        if (!span.isInitialized()) return &.{};
        const start: usize = span.start;
        return self.direct_request_selections.items[start .. start + span.len];
    }

    pub fn recordDirectRequestSelections(
        self: *InstGraph,
        request_fn: NodeId,
        selections: []const DirectRequestSelection,
    ) Allocator.Error!void {
        if (selections.len != 0 and self.direct_request_selections.items.len != 0) {
            const pool_start = @intFromPtr(self.direct_request_selections.items.ptr);
            const pool_end = pool_start + self.direct_request_selections.items.len * @sizeOf(DirectRequestSelection);
            const selection_start = @intFromPtr(selections.ptr);
            const selection_end = selection_start + selections.len * @sizeOf(DirectRequestSelection);
            if (selection_start >= pool_start and selection_end <= pool_end) {
                // Materialized callable nodes share the immutable flat span
                // owned by the request they came from. Besides avoiding a
                // redundant copy, this prevents an append reallocation from
                // invalidating its own source slice.
                self.direct_request_selection_spans.items[@intFromEnum(request_fn)] = .{
                    .start = @intCast((selection_start - pool_start) / @sizeOf(DirectRequestSelection)),
                    .len = @intCast(selections.len),
                };
                return;
            }
        }
        const start: u32 = @intCast(self.direct_request_selections.items.len);
        try self.direct_request_selections.appendSlice(self.allocator, selections);
        self.direct_request_selection_spans.items[@intFromEnum(request_fn)] = .{
            .start = start,
            .len = @intCast(selections.len),
        };
    }

    pub fn inheritDirectRequestSelections(self: *InstGraph, source_fn: NodeId, destination_fn: NodeId) void {
        if (source_fn == destination_fn) return;
        const source = self.direct_request_selection_spans.items[@intFromEnum(source_fn)];
        if (!source.isInitialized()) return;
        const destination = &self.direct_request_selection_spans.items[@intFromEnum(destination_fn)];
        if (destination.isInitialized() and !std.meta.eql(destination.*, source)) {
            Common.invariant("function request inherited two different direct-selection spans");
        }
        destination.* = source;
    }

    /// Whether two requests select the same exact runtime nodes for every
    /// checker-published identity slot. Callers compare the immutable checked
    /// callable base separately; this span is the complete variable part of a
    /// specialization request.
    pub fn sameDirectRequestSelections(self: *InstGraph, left_fn: NodeId, right_fn: NodeId) bool {
        const left = self.directRequestSelections(left_fn);
        const right = self.directRequestSelections(right_fn);
        if (left.len != right.len) return false;
        for (left) |left_selection| {
            const right_selection = for (right) |candidate| {
                if (std.meta.eql(candidate.base, left_selection.base)) break candidate;
            } else return false;
            if (!self.sameClass(left_selection.produced, right_selection.produced)) return false;
        }
        return true;
    }

    /// Resolve the stable result cell held by callers and recursive references
    /// to the exact node returned by the completed function body. The result
    /// cell is a producer-owned forward edge, not a checked type and not a
    /// second graph to compare or merge.
    pub fn completeFunctionResult(
        self: *InstGraph,
        raw_fn_node: NodeId,
        produced_ret: NodeId,
    ) Allocator.Error!void {
        const fn_node = self.find(raw_fn_node);
        const function = switch (self.nodes.items[@intFromEnum(fn_node)]) {
            .func => |function| function,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => Common.invariant("function result completion received a non-function node"),
        };
        const result_cell = self.find(function.ret);
        const exact_produced = self.find(produced_ret);
        if (result_cell != exact_produced) {
            if (self.nodes.items[@intFromEnum(result_cell)] != .unresolved) {
                Common.invariant("function result completion reached an already-produced result cell");
            }
            try self.setContent(result_cell, .{ .redirect = exact_produced });
        }
        _ = try self.canonicalizeCompletedFunction(fn_node);
    }

    /// Resolve one explicit control-flow result cell to the exact node chosen
    /// from its producer branches. Retaining the producer node identity also
    /// retains any queued specialization responsible for completing it.
    pub fn completeProducedSelection(
        self: *InstGraph,
        raw_selection: NodeId,
        raw_produced: NodeId,
    ) Allocator.Error!void {
        const selection = self.find(raw_selection);
        const produced = self.find(raw_produced);
        if (selection == produced) return;
        if (self.nodes.items[@intFromEnum(selection)] != .unresolved) {
            Common.invariant("produced selection completed a non-placeholder cell");
        }
        try self.setContent(selection, .{ .redirect = produced });
    }

    /// Complete the one open tag-row extension owned by a generated producer.
    /// The caller supplies the exact new tags and tail; no enclosing row is
    /// compared, copied, or reconciled.
    pub fn completeOpenTagRowExtension(
        self: *InstGraph,
        raw_extension: NodeId,
        tags: []InstTag,
        tail: NodeId,
    ) Allocator.Error!void {
        const extension = self.find(raw_extension);
        const content_ = self.nodes.items[@intFromEnum(extension)];
        if (content_ != .unresolved or content_.unresolved.numeric_default_phase != null) {
            Common.invariant("tag-row producer attempted to complete a non-row extension");
        }
        if (content_.unresolved.row_default) |default| {
            if (default != .empty_tag_union) {
                Common.invariant("tag-row producer attempted to complete a record-row extension");
            }
        }
        try self.setContent(extension, .{ .tag_union = .{
            .tags = tags,
            .ext = self.find(tail),
        } });
    }

    pub fn lookupGeneratedIteratorFromNamed(
        self: *InstGraph,
        public_named: InstNamed,
    ) Allocator.Error!GeneratedIteratorLookup {
        if (public_named.args.len == 0) {
            Common.invariant("generated iterator lookup received no public item argument");
        }
        return self.lookupGeneratedIterator(public_named.def, public_named.args[0]);
    }

    /// Resolve the content-addressed identity for the exact iterator nominal
    /// at the point where that nominal is instantiated. The declaration and
    /// item node are the complete identity input; no public iterator graph is
    /// built as an intermediate request.
    pub fn lookupGeneratedIterator(
        self: *InstGraph,
        public_def: Type.TypeDef,
        item_node: NodeId,
    ) Allocator.Error!GeneratedIteratorLookup {
        const item_root = self.find(item_node);
        if (self.generated_iterators_by_item.getPtr(item_root)) |bucket| {
            for (bucket.items) |raw_existing| {
                const existing = self.find(raw_existing);
                const existing_named = switch (self.content(existing)) {
                    .named => |named| named,
                    .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator item index contained a non-named node"),
                };
                if (!sameTypeDef(public_def, existing_named.def)) continue;
                if (existing_named.args.len != 1 or self.find(existing_named.args[0]) != item_root) {
                    Common.invariant("generated iterator item index contained a mismatched item node");
                }
                const digest = existing_named.def.generated orelse
                    Common.invariant("generated iterator item index contained an unstamped node");
                self.countDiagnostic("generated_identity_intern_hits");
                return .{ .existing = existing, .digest = digest };
            }
        }
        const digest = try self.generatedIteratorInternDigest(public_def, item_root);
        if (self.generated_nominal_intern.get(digest)) |node| {
            self.countDiagnostic("generated_identity_intern_hits");
            return .{ .existing = self.find(node), .digest = digest };
        }
        self.countDiagnostic("generated_identity_intern_misses");
        return .{ .existing = null, .digest = digest };
    }

    /// Reserve, fill, and publish one recursive generated iterator nominal.
    /// Registering the reservation before `fill` lets recursive occurrences
    /// with the same content identity resolve directly to this atomic node.
    pub fn addRecursiveGeneratedIterator(
        self: *InstGraph,
        digest: names.TypeDigest,
        context: anytype,
        comptime fill: fn (@TypeOf(context), NodeId) Allocator.Error!InstNode,
    ) Allocator.Error!NodeId {
        self.requireRelationProduction();
        const reserved = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        const entry = try self.generated_nominal_intern.getOrPut(digest);
        if (entry.found_existing) {
            Common.invariant("generated iterator was reserved after its identity had already been interned");
        }
        entry.value_ptr.* = reserved;
        try self.setContent(reserved, try fill(context, reserved));
        try self.finishGeneratedIteratorAtDigest(reserved, digest);
        return reserved;
    }

    pub fn registerGeneratedIterator(self: *InstGraph, raw_node: NodeId) Allocator.Error!void {
        self.requireRelationProduction();
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator interner received a non-named node"),
        };
        const digest = named.def.generated orelse
            Common.invariant("generated iterator reached registration without its producer identity");
        try self.registerGeneratedIteratorAtDigest(node, digest);
    }

    /// Register a newly built iterator under the digest already computed by
    /// its immediately preceding vacant lookup.
    pub fn registerGeneratedIteratorAtDigest(
        self: *InstGraph,
        raw_node: NodeId,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator interner received a non-named node"),
        };
        const stamped = named.def.generated orelse
            Common.invariant("generated iterator reached registration without its producer identity");
        if (!std.mem.eql(u8, &stamped.bytes, &digest.bytes)) {
            Common.invariant("generated iterator registration disagreed with its producer identity");
        }
        const entry = try self.generated_nominal_intern.getOrPut(digest);
        if (entry.found_existing and self.find(entry.value_ptr.*) != node) {
            Common.invariant("one generated identity was constructed twice in one body");
        }
        entry.value_ptr.* = node;
        try self.finishGeneratedIteratorAtDigest(node, digest);
    }

    fn finishGeneratedIteratorAtDigest(
        self: *InstGraph,
        node: NodeId,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        const interned = self.generated_nominal_intern.get(digest) orelse
            Common.invariant("generated iterator completed without an identity reservation");
        if (self.find(interned) != self.find(node)) {
            Common.invariant("generated iterator completion disagreed with its identity reservation");
        }
        try self.indexGeneratedIteratorByItem(node);
        try self.registerGeneratedNominalAtDigest(node, digest);
    }

    fn indexGeneratedIteratorByItem(self: *InstGraph, raw_node: NodeId) Allocator.Error!void {
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator item index received a non-named node"),
        };
        if (named.args.len != 1) {
            Common.invariant("generated iterator registration had an unexpected item arity");
        }
        const item_root = self.find(named.args[0]);
        const item_bucket = try self.generated_iterators_by_item.getOrPut(item_root);
        if (!item_bucket.found_existing) item_bucket.value_ptr.* = .empty;
        for (item_bucket.value_ptr.items) |existing| {
            if (self.find(existing) == node) return;
        }
        try item_bucket.value_ptr.append(self.allocator, node);
    }

    fn generatedIteratorInternDigest(
        self: *InstGraph,
        public_def: Type.TypeDef,
        item_node: NodeId,
    ) Allocator.Error!names.TypeDigest {
        var writer = GeneratedIdentityWriter.init(self);
        defer writer.deinit();
        // Every iterator producer implements the same runtime interface: one
        // length value and one zero-argument step closure returning the shared
        // iterator step protocol. The closure's callable set records which
        // implementations and captures may inhabit that slot; it does not
        // change the surrounding iterator's Monotype. Consequently the exact
        // generated nominal is identified only by that interface's declared
        // nominal and item type, not by the syntax/operator that produced one
        // value or by another iterator value stored in its captures.
        writer.writeBytes("roc.generated_iterator.runtime_interface.v6");
        writer.writeBytes(self.name_store.moduleIdentityBytes(public_def.module));
        writer.writeOptionalU32(public_def.source_decl);
        if (public_def.source_decl == null) {
            writer.writeBytes(self.name_store.typeNameText(public_def.type_name));
        }
        const item_digest = try self.generatedIdentityInputDigest(item_node);
        writer.writeBytes(&item_digest.bytes);
        return .{ .bytes = writer.hasher.finalResult() };
    }

    fn generatedIdentityInputDigest(self: *InstGraph, node: NodeId) Allocator.Error!names.TypeDigest {
        var writer = GeneratedIdentityWriter.init(self);
        defer writer.deinit();
        writer.writeBytes("roc.generated_private.identity_input.v4");
        try writer.writeNode(node);
        return .{ .bytes = writer.hasher.finalResult() };
    }

    /// Identify a non-recursive generated nominal from the complete inputs
    /// that determine its runtime representation. Generated children are
    /// atomic digest leaves, so producing a parent never inspects beneath a
    /// child generated nominal.
    pub fn lookupGeneratedNominal(
        self: *InstGraph,
        source_def: Type.TypeDef,
        implementation_args: []const NodeId,
        backing: NodeId,
    ) Allocator.Error!GeneratedNominalLookup {
        const digest: names.TypeDigest = source_def.generated orelse digest: {
            var public_def = source_def;
            public_def.generated = null;
            var writer = GeneratedIdentityWriter.init(self);
            defer writer.deinit();
            writer.writeBytes("roc.generated_nominal.runtime_implementation.v2");
            writer.writeTypeDef(public_def);
            // These inputs are consumed once to mint the atomic identity. They
            // are intentionally absent from the resulting named node, so no
            // downstream operation can traverse or reinterpret them.
            try writer.writeNodeSpan(implementation_args);
            try writer.writeNode(backing);
            break :digest .{ .bytes = writer.hasher.finalResult() };
        };
        if (self.generated_nominal_intern.get(digest)) |existing| {
            self.countDiagnostic("generated_identity_intern_hits");
            return .{ .existing = self.find(existing), .digest = digest };
        }
        self.countDiagnostic("generated_identity_intern_misses");
        return .{ .existing = null, .digest = digest };
    }

    /// Publish a completed generated nominal at the identity assigned by its
    /// producer. This is the only registry used by consumers and sealing.
    pub fn registerGeneratedNominalAtDigest(
        self: *InstGraph,
        raw_node: NodeId,
        digest: names.TypeDigest,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const node = self.find(raw_node);
        const named = switch (self.content(node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated nominal registry received a non-named node"),
        };
        const backing = named.backing orelse
            Common.invariant("generated nominal registry received a nominal without backing");
        if (backing.authority != .generated_private) {
            Common.invariant("generated nominal registry received a non-generated backing");
        }
        const stamped = named.def.generated orelse
            Common.invariant("generated nominal reached registration without its producer identity");
        if (!std.mem.eql(u8, &stamped.bytes, &digest.bytes)) {
            Common.invariant("generated nominal registration disagreed with its producer identity");
        }
        const entry = try self.generated_nominal_intern.getOrPut(digest);
        if (entry.found_existing and self.find(entry.value_ptr.*) != node) {
            Common.invariant("one generated identity was constructed twice in one body");
        }
        entry.value_ptr.* = node;
        for (self.generated_nominal_nodes.items) |registered| {
            if (self.find(registered) == node) return;
        }
        try self.generated_nominal_nodes.append(self.allocator, node);
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
                        .checked_variable => Common.invariant("checked variable reached final demand validation without an explicit default"),
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
                    .checked_variable => Common.invariant("checked variable reached final inhabitance validation without an explicit default"),
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

    fn producedTagUnionHash(self: *InstGraph, tag_union: InstTagUnion) u64 {
        var hasher = std.hash.Wyhash.init(0);
        var ext = std.mem.nativeToLittle(u32, @intFromEnum(self.find(tag_union.ext)));
        hasher.update(std.mem.asBytes(&ext));
        for (tag_union.tags) |tag| {
            var name = std.mem.nativeToLittle(u32, @intFromEnum(tag.name));
            hasher.update(std.mem.asBytes(&name));
            var checked_name = std.mem.nativeToLittle(u32, @intFromEnum(tag.checked_name));
            hasher.update(std.mem.asBytes(&checked_name));
            var len = std.mem.nativeToLittle(u32, @intCast(tag.payloads.len));
            hasher.update(std.mem.asBytes(&len));
            for (tag.payloads) |payload| {
                var child = std.mem.nativeToLittle(u32, @intFromEnum(self.find(payload)));
                hasher.update(std.mem.asBytes(&child));
            }
        }
        return hasher.final();
    }

    fn producedTagUnionEql(self: *InstGraph, left: InstTagUnion, right: InstTagUnion) bool {
        if (left.tags.len != right.tags.len or self.find(left.ext) != self.find(right.ext)) return false;
        for (left.tags, right.tags) |left_tag, right_tag| {
            if (left_tag.name != right_tag.name or
                left_tag.checked_name != right_tag.checked_name or
                left_tag.payloads.len != right_tag.payloads.len) return false;
            for (left_tag.payloads, right_tag.payloads) |left_payload, right_payload| {
                if (self.find(left_payload) != self.find(right_payload)) return false;
            }
        }
        return true;
    }

    fn existingTagUnionShape(self: *InstGraph, tag_union: InstTagUnion) ?NodeId {
        const bucket = self.produced_tag_unions.get(self.producedTagUnionHash(tag_union)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (candidate_content == .tag_union and self.producedTagUnionEql(candidate_content.tag_union, tag_union)) return root;
        }
        return null;
    }

    fn registerTagUnionShape(self: *InstGraph, raw_node: NodeId, tag_union: InstTagUnion) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.produced_tag_unions.getOrPut(self.producedTagUnionHash(tag_union));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| if (self.find(candidate) == node) return;
        try bucket.value_ptr.append(self.allocator, node);
    }

    fn nodeSpanShapeHash(self: *InstGraph, nodes: []const NodeId) u64 {
        var hasher = std.hash.Wyhash.init(0);
        var len = std.mem.nativeToLittle(u32, @intCast(nodes.len));
        hasher.update(std.mem.asBytes(&len));
        for (nodes) |node| {
            var child = std.mem.nativeToLittle(u32, @intFromEnum(self.find(node)));
            hasher.update(std.mem.asBytes(&child));
        }
        return hasher.final();
    }

    fn sameNodeSpanShape(self: *InstGraph, left: []const NodeId, right: []const NodeId) bool {
        if (left.len != right.len) return false;
        for (left, right) |left_node, right_node| {
            if (self.find(left_node) != self.find(right_node)) return false;
        }
        return true;
    }

    fn existingListElement(self: *InstGraph, raw_element: NodeId) ?NodeId {
        const element = self.find(raw_element);
        const candidate = self.find(self.list_nodes_by_element.get(element) orelse return null);
        const candidate_content = self.nodes.items[@intFromEnum(candidate)];
        return if (candidate_content == .list and self.find(candidate_content.list) == element) candidate else null;
    }

    fn existingBoxElement(self: *InstGraph, raw_element: NodeId) ?NodeId {
        const element = self.find(raw_element);
        const candidate = self.find(self.box_nodes_by_element.get(element) orelse return null);
        const candidate_content = self.nodes.items[@intFromEnum(candidate)];
        return if (candidate_content == .box and self.find(candidate_content.box) == element) candidate else null;
    }

    fn existingTupleShape(self: *InstGraph, items: []const NodeId) ?NodeId {
        const bucket = self.tuple_nodes_by_shape_hash.get(self.nodeSpanShapeHash(items)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (candidate_content == .tuple and self.sameNodeSpanShape(candidate_content.tuple, items)) return root;
        }
        return null;
    }

    fn registerTupleShape(self: *InstGraph, raw_node: NodeId, items: []const NodeId) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.tuple_nodes_by_shape_hash.getOrPut(self.nodeSpanShapeHash(items));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| if (self.find(candidate) == node) return;
        try bucket.value_ptr.append(self.allocator, node);
    }

    fn functionShapeHash(self: *InstGraph, function: InstFunction) u64 {
        var hasher = std.hash.Wyhash.init(self.nodeSpanShapeHash(function.args));
        var ret = std.mem.nativeToLittle(u32, @intFromEnum(self.find(function.ret)));
        hasher.update(std.mem.asBytes(&ret));
        return hasher.final();
    }

    fn sameFunctionShape(self: *InstGraph, left: InstFunction, right: InstFunction) bool {
        return self.sameNodeSpanShape(left.args, right.args) and
            self.find(left.ret) == self.find(right.ret);
    }

    fn existingFunctionShape(self: *InstGraph, function: InstFunction) ?NodeId {
        const bucket = self.function_nodes_by_shape_hash.get(self.functionShapeHash(function)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (candidate_content == .func and self.sameFunctionShape(candidate_content.func, function)) return root;
        }
        return null;
    }

    fn registerFunctionShape(self: *InstGraph, raw_node: NodeId, function: InstFunction) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.function_nodes_by_shape_hash.getOrPut(self.functionShapeHash(function));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| if (self.find(candidate) == node) return;
        try bucket.value_ptr.append(self.allocator, node);
    }

    /// Canonicalize a function only when its producer has completed the exact
    /// result edge. Open requests retain distinct forward cells; completed
    /// values with identical immediate children become one runtime type node.
    fn canonicalizeCompletedFunction(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        const node = self.find(raw_node);
        const function = switch (self.nodes.items[@intFromEnum(node)]) {
            .func => |function| function,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => Common.invariant("completed function canonicalization received a non-function node"),
        };
        if (self.existingFunctionShape(function)) |existing| {
            if (existing != node) {
                try self.union_(existing, node);
                return existing;
            }
            return node;
        }
        try self.registerFunctionShape(node, function);
        return node;
    }

    fn recordShapeHash(self: *InstGraph, record: InstNode) u64 {
        const row = record.record;
        var hasher = std.hash.Wyhash.init(0);
        var ext = std.mem.nativeToLittle(u32, @intFromEnum(self.find(row.ext)));
        hasher.update(std.mem.asBytes(&ext));
        var len = std.mem.nativeToLittle(u32, @intCast(row.fields.len));
        hasher.update(std.mem.asBytes(&len));
        for (row.fields) |field| {
            var name = std.mem.nativeToLittle(u32, @intFromEnum(field.name));
            hasher.update(std.mem.asBytes(&name));
            var child = std.mem.nativeToLittle(u32, @intFromEnum(self.find(field.ty)));
            hasher.update(std.mem.asBytes(&child));
        }
        return hasher.final();
    }

    fn sameRecordShape(self: *InstGraph, left: InstNode, right: InstNode) bool {
        const left_row = left.record;
        const right_row = right.record;
        if (left_row.fields.len != right_row.fields.len or self.find(left_row.ext) != self.find(right_row.ext)) return false;
        for (left_row.fields, right_row.fields) |left_field, right_field| {
            if (left_field.name != right_field.name or self.find(left_field.ty) != self.find(right_field.ty)) return false;
        }
        return true;
    }

    fn existingRecordShape(self: *InstGraph, record: InstNode) ?NodeId {
        const bucket = self.record_nodes_by_shape_hash.get(self.recordShapeHash(record)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (candidate_content == .record and self.sameRecordShape(candidate_content, record)) return root;
        }
        return null;
    }

    fn registerRecordShape(self: *InstGraph, raw_node: NodeId, record: InstNode) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.record_nodes_by_shape_hash.getOrPut(self.recordShapeHash(record));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| if (self.find(candidate) == node) return;
        try bucket.value_ptr.append(self.allocator, node);
    }

    /// Return the canonical identity node for one already-built immediate
    /// child. Row extensions are normalized once here, when a parent records
    /// that child, rather than being rediscovered by later call consumers.
    fn canonicalImmediateChild(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        const node = self.find(raw_node);
        return switch (self.nodes.items[@intFromEnum(node)]) {
            .tag_union => blk: {
                const row = try self.flattenTagRow(node);
                break :blk try self.newNode(.{ .tag_union = .{ .tags = row.tags, .ext = row.ext } });
            },
            .record => blk: {
                const row = try self.flattenRecordRow(node);
                break :blk try self.newNode(.{ .record = .{ .fields = row.fields, .ext = row.ext } });
            },
            .redirect => unreachable,
            .unresolved, .primitive, .list, .box, .tuple, .func, .empty_tag_union, .empty_record, .named, .erased, .zst => node,
        };
    }

    fn canonicalizeNamedArguments(self: *InstGraph, named: InstNamed) Allocator.Error!?InstNamed {
        var canonical_args: ?[]NodeId = null;
        for (named.args, 0..) |arg, index| {
            const canonical = try self.canonicalImmediateChild(arg);
            if (canonical_args) |args| {
                args[index] = canonical;
            } else if (self.find(arg) != canonical) {
                const args = try self.arena().alloc(NodeId, named.args.len);
                @memcpy(args[0..index], named.args[0..index]);
                args[index] = canonical;
                canonical_args = args;
            }
        }
        if (canonical_args) |args| {
            var canonical = named;
            canonical.args = args;
            return canonical;
        }
        return null;
    }

    fn namedIdentityHash(self: *InstGraph, named: InstNamed) u64 {
        var hasher = std.hash.Wyhash.init(0);
        var module = std.mem.nativeToLittle(u32, @intFromEnum(named.def.module));
        hasher.update(std.mem.asBytes(&module));
        var type_name = std.mem.nativeToLittle(u32, @intFromEnum(named.def.type_name));
        hasher.update(std.mem.asBytes(&type_name));
        var source_decl = std.mem.nativeToLittle(u32, named.def.source_decl orelse std.math.maxInt(u32));
        hasher.update(std.mem.asBytes(&source_decl));
        if (named.def.generated) |generated| hasher.update(&generated.bytes);
        hasher.update(&.{@intFromBool(named.kind == .alias)});
        hasher.update(&.{if (named.builtin_owner) |owner| @intFromEnum(owner) else std.math.maxInt(u8)});
        for (named.args) |arg| {
            var child = std.mem.nativeToLittle(u32, @intFromEnum(self.find(arg)));
            hasher.update(std.mem.asBytes(&child));
        }
        return hasher.final();
    }

    fn sameNamedIdentity(self: *InstGraph, left: InstNamed, right: InstNamed) bool {
        return (left.kind == .alias) == (right.kind == .alias) and
            std.meta.eql(left.def, right.def) and
            left.builtin_owner == right.builtin_owner and
            self.sameNamedArgs(left.args, right.args);
    }

    fn existingNamedIdentity(self: *InstGraph, named: InstNamed) ?NodeId {
        const bucket = self.named_nodes_by_identity_hash.get(self.namedIdentityHash(named)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (candidate_content == .named and self.sameNamedIdentity(candidate_content.named, named)) return root;
        }
        return null;
    }

    fn registerNamedIdentity(self: *InstGraph, raw_node: NodeId, named: InstNamed) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.named_nodes_by_identity_hash.getOrPut(self.namedIdentityHash(named));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| {
            if (self.find(candidate) == node) return;
        }
        try bucket.value_ptr.append(self.allocator, node);
    }

    pub fn newNode(self: *InstGraph, node_content: InstNode) Allocator.Error!NodeId {
        self.requireRelationProduction();
        if (node_content == .named) {
            if (try self.canonicalizeNamedArguments(node_content.named)) |canonical| {
                return try self.newNode(.{ .named = canonical });
            }
        }
        switch (node_content) {
            .primitive => |primitive| if (self.primitive_nodes[@intFromEnum(primitive)]) |existing| return self.find(existing),
            .empty_tag_union => if (self.empty_tag_union_node) |existing| return self.find(existing),
            .empty_record => if (self.empty_record_node) |existing| return self.find(existing),
            .zst => if (self.zst_node) |existing| return self.find(existing),
            .list => |element| if (self.existingListElement(element)) |existing| return existing,
            .box => |element| if (self.existingBoxElement(element)) |existing| return existing,
            .tuple => |items| if (self.existingTupleShape(items)) |existing| return existing,
            .record => if (self.existingRecordShape(node_content)) |existing| return existing,
            .named => |named| if (self.existingNamedIdentity(named)) |existing| return existing,
            .redirect, .unresolved, .func, .tag_union, .erased => {},
        }
        const produced_tag_hash: ?u64 = if (node_content == .tag_union)
            self.producedTagUnionHash(node_content.tag_union)
        else
            null;
        if (produced_tag_hash != null) {
            if (self.existingTagUnionShape(node_content.tag_union)) |existing| return existing;
        }
        const id = try self.appendDistinctNode(node_content);
        switch (node_content) {
            .primitive => |primitive| self.primitive_nodes[@intFromEnum(primitive)] = id,
            .empty_tag_union => self.empty_tag_union_node = id,
            .empty_record => self.empty_record_node = id,
            .zst => self.zst_node = id,
            .list => |element| try self.list_nodes_by_element.put(self.find(element), id),
            .box => |element| try self.box_nodes_by_element.put(self.find(element), id),
            .tuple => |items| try self.registerTupleShape(id, items),
            .record => try self.registerRecordShape(id, node_content),
            .named => |named| try self.registerNamedIdentity(id, named),
            .redirect, .unresolved, .func, .tag_union, .erased => {},
        }
        if (produced_tag_hash != null) {
            try self.registerTagUnionShape(id, node_content.tag_union);
        }
        return id;
    }

    /// Allocate an internal graph cell whose distinct identity is required to
    /// keep a nominal wrapper separate from the structural node it replaces.
    /// This cell is deliberately absent from the exact-node intern tables.
    fn appendDistinctNode(self: *InstGraph, node_content: InstNode) Allocator.Error!NodeId {
        const id: NodeId = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        try self.nodes.append(self.allocator, node_content);
        try self.checked_base_nodes.append(self.allocator, false);
        try self.versions.append(self.allocator, 0);
        try self.row_exts.append(self.allocator, null);
        try self.request_checked_sources.append(self.allocator, null);
        try self.function_result_relations.append(self.allocator, null);
        try self.direct_request_selection_spans.append(self.allocator, .uninitialized);
        try self.registerRowParent(id, node_content);
        self.countDiagnostic("nodes_created");
        return id;
    }

    pub fn markCheckedBase(self: *InstGraph, raw_node: NodeId) void {
        self.checked_base_nodes.items[@intFromEnum(self.find(raw_node))] = true;
    }

    pub fn beginCheckedBaseConstruction(self: *InstGraph) void {
        self.checked_base_construction_depth += 1;
    }

    pub fn endCheckedBaseConstruction(self: *InstGraph) void {
        if (self.checked_base_construction_depth == 0) {
            Common.invariant("checked base construction scope underflowed");
        }
        self.checked_base_construction_depth -= 1;
    }

    pub fn nodeIsCheckedBase(self: *InstGraph, raw_node: NodeId) bool {
        return self.checked_base_nodes.items[@intFromEnum(self.find(raw_node))];
    }

    /// A second checked id may be a transparent reference to an already-built
    /// immutable base node. Redirect only the new placeholder; never merge the
    /// existing base into it.
    pub fn attachCheckedBaseAlias(self: *InstGraph, placeholder: NodeId, checked_base: NodeId) Allocator.Error!void {
        const base_root = self.find(checked_base);
        if (!self.checked_base_nodes.items[@intFromEnum(base_root)]) {
            Common.invariant("checked base alias target was not immutable");
        }
        self.checked_base_nodes.items[@intFromEnum(base_root)] = false;
        defer self.checked_base_nodes.items[@intFromEnum(self.find(base_root))] = true;
        try self.union_(base_root, placeholder);
    }

    /// Complete a placeholder reserved by one explicit producer traversal.
    /// This exists for recursive structural producers whose self-edge is
    /// encountered before the enclosing node has been built.
    pub fn completeReservedProducedNode(
        self: *InstGraph,
        reserved: NodeId,
        raw_content: InstNode,
    ) Allocator.Error!void {
        const root = self.find(reserved);
        const existing = self.nodes.items[@intFromEnum(root)];
        if (existing != .unresolved or existing.unresolved.origin != .placeholder) {
            Common.invariant("produced node reservation was completed more than once");
        }
        const completed_content = if (raw_content == .named)
            if (try self.canonicalizeNamedArguments(raw_content.named)) |canonical|
                InstNode{ .named = canonical }
            else
                raw_content
        else
            raw_content;
        const canonical: ?NodeId = switch (completed_content) {
            .primitive => |primitive| if (self.primitive_nodes[@intFromEnum(primitive)]) |node| self.find(node) else null,
            .empty_tag_union => if (self.empty_tag_union_node) |node| self.find(node) else null,
            .empty_record => if (self.empty_record_node) |node| self.find(node) else null,
            .zst => if (self.zst_node) |node| self.find(node) else null,
            .list => |element| self.existingListElement(element),
            .box => |element| self.existingBoxElement(element),
            .tuple => |items| self.existingTupleShape(items),
            .record => self.existingRecordShape(completed_content),
            .tag_union => |tag_union| self.existingTagUnionShape(tag_union),
            .named => |named| self.existingNamedIdentity(named),
            .redirect, .unresolved, .func, .erased => null,
        };
        if (canonical) |node| {
            try self.redirectRoot(node, root, false);
            return;
        }
        _ = try self.replaceContentWithoutSnapshotInvalidation(root, completed_content);
        switch (completed_content) {
            .primitive => |primitive| self.primitive_nodes[@intFromEnum(primitive)] = root,
            .empty_tag_union => self.empty_tag_union_node = root,
            .empty_record => self.empty_record_node = root,
            .zst => self.zst_node = root,
            .list => |element| try self.list_nodes_by_element.put(self.find(element), root),
            .box => |element| try self.box_nodes_by_element.put(self.find(element), root),
            .tuple => |items| try self.registerTupleShape(root, items),
            .record => try self.registerRecordShape(root, completed_content),
            .tag_union => |tag_union| try self.registerTagUnionShape(root, tag_union),
            .named => |named| try self.registerNamedIdentity(root, named),
            .redirect, .unresolved, .func, .erased => {},
        }
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

    /// Whether two callable roots carry the same exact immediate runtime
    /// edges and the same checker-authored substitution span. This is a
    /// constant-depth recursive-binding key, not a structural graph match.
    pub fn sameExactFunctionRequest(self: *InstGraph, left: NodeId, right: NodeId) bool {
        return self.sameFunctionInterface(left, right) and
            self.sameDirectRequestSelections(left, right);
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

    /// Return an exact node for an explicit checker-published default on an identity slot.
    /// Call dependency planning uses this for pathless numeric and open-row
    /// identities before lowering a contextual consumer. Returns false when
    /// the node has no checked default; callers must then wait for another
    /// published producer edge.
    pub fn checkedDefaultNode(self: *InstGraph, raw_node: NodeId) Allocator.Error!?NodeId {
        self.requireRelationProduction();
        const node = self.find(raw_node);
        const node_content = self.nodes.items[@intFromEnum(node)];
        if (node_content != .unresolved) return node;
        const variable = node_content.unresolved;
        if (variable.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                Common.invariant("checking-finalized numeric variable reached Monotype unresolved");
            return try self.newNode(switch (target) {
                .dec => .{ .primitive = .dec },
                .str => .{ .primitive = .str },
            });
        }
        if (variable.row_default) |row_default| {
            return try self.newNode(switch (row_default) {
                .empty_record => .empty_record,
                .empty_tag_union => .empty_tag_union,
            });
        }
        return null;
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

    /// Project a primitive leaf through explicit inspectable backing edges,
    /// without materializing an immutable Monotype snapshot.
    pub fn primitiveAtNode(self: *InstGraph, node: NodeId) Allocator.Error!?Type.Primitive {
        return switch (self.content(try self.shapeRoot(node, "primitive", .inspectable))) {
            .primitive => |primitive| primitive,
            .redirect, .unresolved, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => null,
        };
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

    /// Project the semantic open-row remainder after the checker-published
    /// fields. Row extension topology is intentionally irrelevant: checked
    /// publication names the fields owned by the enclosing row, and this edge
    /// returns exactly the remaining flattened row.
    pub fn recordRemainderNode(
        self: *InstGraph,
        raw_record: NodeId,
        excluded: []const names.RecordFieldNameId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_record, "record remainder", .inspectable);
        if (self.content(structural) != .record) {
            Common.invariant("instantiation record-remainder edge had a non-record node");
        }
        if (excluded.len == 0) return structural;

        const row = try self.flattenRecordRow(structural);
        var retained = std.ArrayList(InstField).empty;
        defer retained.deinit(self.allocator);
        try retained.ensureTotalCapacity(self.allocator, row.fields.len);
        var matched = try self.allocator.alloc(bool, excluded.len);
        defer self.allocator.free(matched);
        @memset(matched, false);
        for (row.fields) |field| {
            var remove = false;
            for (excluded, 0..) |name, index| {
                if (field.name != name) continue;
                matched[index] = true;
                remove = true;
                break;
            }
            if (!remove) retained.appendAssumeCapacity(field);
        }
        for (matched) |present| if (!present) {
            Common.invariant("instantiation record-remainder edge named an absent field");
        };
        if (retained.items.len == 0) return self.find(row.ext);
        return try self.newNode(.{ .record = .{
            .fields = try self.arena().dupe(InstField, retained.items),
            .ext = row.ext,
        } });
    }

    /// Project the semantic open-tag-row remainder after the checker-published
    /// tags. The returned node contains only tags not owned by the enclosing
    /// checked row and preserves the exact produced tail node.
    pub fn tagRemainderNode(
        self: *InstGraph,
        raw_union: NodeId,
        excluded: []const names.TagNameId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_union, "tag remainder", .inspectable);
        if (self.content(structural) != .tag_union) {
            Common.invariant("instantiation tag-remainder edge had a non-tag-union node");
        }
        if (excluded.len == 0) return structural;

        const row = try self.flattenTagRow(structural);
        var retained = std.ArrayList(InstTag).empty;
        defer retained.deinit(self.allocator);
        try retained.ensureTotalCapacity(self.allocator, row.tags.len);
        var matched = try self.allocator.alloc(bool, excluded.len);
        defer self.allocator.free(matched);
        @memset(matched, false);
        for (row.tags) |tag| {
            var remove = false;
            for (excluded, 0..) |name, index| {
                if (tag.name != name) continue;
                matched[index] = true;
                remove = true;
                break;
            }
            if (!remove) retained.appendAssumeCapacity(tag);
        }
        for (matched) |present| if (!present) {
            Common.invariant("instantiation tag-remainder edge named an absent tag");
        };
        if (retained.items.len == 0) return self.find(row.ext);
        return try self.newNode(.{ .tag_union = .{
            .tags = try self.arena().dupe(InstTag, retained.items),
            .ext = row.ext,
        } });
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
        if (self.checked_base_construction_depth == 0 and
            (self.checked_base_nodes.items[@intFromEnum(winner)] or
                self.checked_base_nodes.items[@intFromEnum(loser)]))
        {
            Common.invariant("exact lowering attempted to merge an immutable checked base node");
        }
        try self.redirectRoot(winner, loser, true);
    }

    /// Redirect one reserved or related root while preserving every exact row
    /// back-reference to its canonical target.
    fn redirectRoot(
        self: *InstGraph,
        winner: NodeId,
        loser: NodeId,
        invalidate_snapshots: bool,
    ) Allocator.Error!void {
        if (winner == loser) return;
        try self.unregisterRowParent(loser);
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
        if (invalidate_snapshots) self.invalidateActiveSnapshots(winner);
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
                const moved = try self.appendDistinctNode(self.nodes.items[@intFromEnum(unresolved_node)]);
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
    fn setContent(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!void {
        const root = self.find(raw_root);
        if (self.checked_base_construction_depth == 0 and
            self.checked_base_nodes.items[@intFromEnum(root)])
        {
            Common.invariant("exact lowering attempted to rewrite an immutable checked base node");
        }
        if (new_content == .named) {
            if (try self.canonicalizeNamedArguments(new_content.named)) |canonical| {
                return try self.setContent(root, .{ .named = canonical });
            }
        }
        const old_content = self.nodes.items[@intFromEnum(root)];
        if (old_content == .primitive and
            self.primitive_nodes[@intFromEnum(old_content.primitive)] == root)
        {
            self.primitive_nodes[@intFromEnum(old_content.primitive)] = null;
        }
        if (new_content == .primitive) {
            const primitive = new_content.primitive;
            if (self.primitive_nodes[@intFromEnum(primitive)]) |raw_existing| {
                const existing = self.find(raw_existing);
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
            self.primitive_nodes[@intFromEnum(primitive)] = root;
        }
        if (new_content == .list) {
            if (self.existingListElement(new_content.list)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (new_content == .box) {
            if (self.existingBoxElement(new_content.box)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (new_content == .tuple) {
            if (self.existingTupleShape(new_content.tuple)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (new_content == .record) {
            if (self.existingRecordShape(new_content)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (new_content == .tag_union) {
            if (self.existingTagUnionShape(new_content.tag_union)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (new_content == .named) {
            const named = new_content.named;
            if (self.existingNamedIdentity(named)) |existing| {
                if (existing != root) {
                    if (!try self.replaceContentWithoutSnapshotInvalidation(root, .{ .redirect = existing })) return;
                    self.invalidateActiveSnapshots(root);
                    return;
                }
            }
        }
        if (!try self.replaceContentWithoutSnapshotInvalidation(root, new_content)) return;
        switch (new_content) {
            .list => |element| try self.list_nodes_by_element.put(self.find(element), root),
            .box => |element| try self.box_nodes_by_element.put(self.find(element), root),
            .tuple => |items| try self.registerTupleShape(root, items),
            .record => try self.registerRecordShape(root, new_content),
            .tag_union => |tag_union| try self.registerTagUnionShape(root, tag_union),
            .named => |named| try self.registerNamedIdentity(root, named),
            .redirect, .unresolved, .primitive, .func, .empty_tag_union, .empty_record, .erased, .zst => {},
        }
        self.invalidateActiveSnapshots(root);
    }

    pub fn unify(self: *InstGraph, a: NodeId, b: NodeId) Allocator.Error!void {
        try self.unifyRootsTransitively(a, b);
    }

    pub fn generatedIteratorPublicSource(self: *InstGraph, raw_node: NodeId) InstIteratorPublicSource {
        const named = switch (self.content(raw_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("iterator public source requested from a non-named node"),
        };
        return generatedIteratorSourceFromNamed(named);
    }

    fn generatedIteratorSourceFromNamed(named: InstNamed) InstIteratorPublicSource {
        const backing = named.backing orelse
            Common.invariant("iterator representation source had no backing");
        const owner = named.builtin_owner orelse
            Common.invariant("iterator representation source had no builtin owner");
        if (!static_dispatch.isIteratorOwner(owner)) {
            Common.invariant("iterator representation source had a non-iterator owner");
        }
        if (named.def.generated == null and backing.authority != .checked_public) {
            Common.invariant("public iterator representation source had private backing authority");
        }
        if (named.def.generated != null and backing.authority != .generated_private) {
            Common.invariant("finished iterator representation source had public backing authority");
        }
        var public_def = named.def;
        public_def.generated = null;
        return .{
            .named_type = named.named_type,
            .def = public_def,
            .kind = named.kind,
            .builtin_owner = owner,
            .backing = .{
                .node = backing.node,
                .use = backing.use,
                .authority = .checked_public,
            },
            .declared_order = named.declared_order,
        };
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
        // An unresolved exact-result cell is completed by its producer's
        // generated nominal in the ordinary way. What is forbidden is
        // relating an already-built public nominal graph to a private one.
        if (left_generated_private != right_generated_private and
            left_content != .unresolved and right_content != .unresolved)
        {
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
            .checked_key = mergeCheckedVariableKey(a.checked_key, b.checked_key),
        };
    }

    fn mergeCheckedVariableKey(a: ?[32]u8, b: ?[32]u8) ?[32]u8 {
        if (a == null) return b;
        if (b == null) return a;
        if (!std.meta.eql(a.?, b.?)) {
            Common.invariant("one checked substitution class carried two different stable identities");
        }
        return a;
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
                    const left_generated = isGeneratedPrivateRootContent(left_content);
                    const right_generated = isGeneratedPrivateRootContent(right_content);
                    if (left_generated or right_generated) {
                        if (left_generated and right_generated and
                            self.sameExactGeneratedPrivateIdentity(left_named, right_named))
                        {
                            try self.union_(left, right);
                            return;
                        }
                        Common.invariant("generated-private nominal reached ordinary unification instead of an explicit producer mapping or control-flow join");
                    }
                    if (left_named.kind == .alias) {
                        try self.unifyThroughBacking(left, left_content, right, pending);
                        return;
                    }
                    if (right_named.kind == .alias) {
                        try self.unifyThroughBacking(right, right_content, left, pending);
                        return;
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

    /// Generated-private nominals are complete, atomic producer results.
    /// Neither their arguments nor their backings participate in downstream
    /// relation work; the producer's content digest is their exact identity.
    fn sameExactGeneratedPrivateIdentity(self: *InstGraph, left: InstNamed, right: InstNamed) bool {
        _ = self;
        if (left.kind != right.kind or
            left.def.module != right.def.module or
            left.def.type_name != right.def.type_name or
            left.def.source_decl != right.def.source_decl)
        {
            return false;
        }
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
            const moved = try self.appendDistinctNode(self.nodes.items[@intFromEnum(other)]);
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
        const moved = try self.appendDistinctNode(self.nodes.items[@intFromEnum(other)]);
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
        return (left.kind == .alias) == (right.kind == .alias) and
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

    /// Read one tag row as a flat list without rewriting its graph-owned root.
    /// Checked-base nodes are immutable, and produced compounds already store
    /// their exact immediate children; flattening is therefore a projection,
    /// never a graph mutation.
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
        return try self.importMonoInner(ty);
    }

    fn importMonoInner(
        self: *InstGraph,
        ty: Type.TypeId,
    ) Allocator.Error!NodeId {
        self.countDiagnostic("mono_import_requests");
        if (self.linked_type_nodes.get(ty)) |existing| {
            self.countDiagnostic("mono_import_hits");
            return self.find(existing);
        }
        const imported_generated_identity = switch (self.types.get(ty)) {
            .named => |named| if (named.backing) |backing|
                if (backing.authority == .generated_private)
                    named.def.generated orelse
                        Common.invariant("imported generated-private nominal lacked its producer identity")
                else
                    null
            else
                null,
            .primitive, .list, .box, .tuple, .func, .tag_union, .record, .erased, .zst => null,
        };
        if (imported_generated_identity) |identity| {
            if (self.generated_nominal_intern.get(identity)) |existing| {
                const node = self.find(existing);
                try self.linked_type_nodes.put(ty, node);
                self.countDiagnostic("mono_import_hits");
                return node;
            }
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
        if (imported_generated_identity) |identity| {
            const entry = try self.generated_nominal_intern.getOrPut(identity);
            if (entry.found_existing) {
                Common.invariant("generated-private import interning changed during one import");
            }
            entry.value_ptr.* = node;
        }
        const types = self.types;
        const imported: InstNode = switch (types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.importMonoInner(elem) },
            .box => |elem| .{ .box = try self.importMonoInner(elem) },
            .tuple => |items| .{ .tuple = try self.importMonoSliceInner(types.span(items)) },
            .func => |func| .{ .func = .{
                .args = try self.importMonoSliceInner(types.span(func.args)),
                .ret = try self.importMonoInner(func.ret),
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
                        .payloads = try self.importMonoSliceInner(types.span(tag.payloads)),
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
                        .ty = try self.importMonoInner(field.ty),
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
                .args = try self.importMonoSliceInner(types.span(named.args)),
                .backing = if (named.backing) |backing| .{
                    .node = try self.importMonoInner(backing.ty),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.importDeclaredFieldsInner(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        try self.setContent(node, imported);
        return node;
    }

    fn importMonoSliceInner(
        self: *InstGraph,
        tys: anytype,
    ) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (0..tys.len) |index| {
            const ty = GuardedList.at(tys, index);
            out[index] = try self.importMonoInner(ty);
        }
        return out;
    }

    fn importDeclaredFieldsInner(
        self: *InstGraph,
        span: Type.Span,
    ) Allocator.Error![]const InstDeclaredField {
        const fields = self.types.declaredFieldSpan(span);
        if (fields.len == 0) return &.{};
        const out = try self.arena().alloc(InstDeclaredField, fields.len);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.importMonoInner(ty) },
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
        return try self.activeIdentityViewForNode(node) orelse
            Common.invariant("active Monotype TypeId requested before generated identity inputs resolved");
    }

    /// Return an immutable Type-shaped identity only when the ordinary type
    /// graph and every graph-owned generated representation input are resolved.
    /// The generated inputs are checked while the sealer encounters their
    /// exact roots; there is no preliminary containment scan.
    pub fn activeIdentityViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!?Type.TypeId {
        self.requireRelationProduction();
        self.countDiagnostic("active_type_requests");
        if (self.imported_monos.get(node)) |imported| {
            self.countDiagnostic("active_type_imported_hits");
            return imported;
        }
        if (!try self.typeIsResolved(node)) {
            return null;
        }
        return try self.monoFor(node);
    }

    fn monoFor(self: *InstGraph, node: NodeId) Allocator.Error!?Type.TypeId {
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

    pub fn nodeIsGeneratedNominal(self: *InstGraph, node: NodeId) bool {
        return isGeneratedPrivateRootContent(self.content(node));
    }

    /// Whether `generated_node` is the content-addressed runtime identity that
    /// replaces this exact public nominal occurrence. This compares only the
    /// atomic definition and immediate argument identities; no backing or
    /// descendant graph is inspected.
    pub fn generatedNominalReplacesPublic(
        self: *InstGraph,
        public_node: NodeId,
        generated_node: NodeId,
    ) bool {
        const public_content = self.content(public_node);
        const generated_content = self.content(generated_node);
        if (public_content != .named or generated_content != .named) return false;
        const public = public_content.named;
        const generated = generated_content.named;
        return public.def.generated == null and
            generated.def.generated != null and
            sameTypeDef(public.def, generated.def) and
            (public.kind == .alias) == (generated.kind == .alias) and
            public.builtin_owner == generated.builtin_owner and
            self.sameNamedArgs(public.args, generated.args);
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
        const generated_identity = if (InstGraph.isGeneratedPrivateRootContent(node_content))
            node_content.named.def.generated orelse
                Common.invariant("generated-private nominal reached sealing without its producer identity")
        else
            null;
        if (generated_identity) |identity| {
            if (self.generated_types_by_identity) |interner| {
                if (interner.get(identity)) |existing| {
                    try self.sealed.put(node, existing);
                    self.graph.countDiagnostic("generated_type_store_hits");
                    return existing;
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
                    Common.invariant("generated Monotype identity raced during single-threaded sealing");
                }
                entry.value_ptr.* = sealed;
                self.graph.countDiagnostic("generated_type_store_misses");
            }
        }
        return sealed;
    }

    /// Commit every completed producer-owned nominal before nested
    /// specialization sealing can request the same content identity.
    pub fn commitGeneratedNominalRoots(self: *GraphTypeFinals) Allocator.Error!void {
        var seen = collections.DenseMap(NodeId, void).init(self.graph.allocator);
        defer seen.deinit();
        for (self.graph.generated_nominal_nodes.items) |registered| {
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
            .named => |named| named_content: {
                break :named_content .{ .named = .{
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
                } };
            },
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

fn optionalInstDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left) |left_digest| {
        const right_digest = right orelse return false;
        return std.mem.eql(u8, &left_digest.bytes, &right_digest.bytes);
    }
    return right == null;
}

/// Writes the content-addressed content identity of a generated producer input.
/// Generated-private nominals are atomic digest leaves.
const GeneratedIdentityWriter = struct {
    graph: *InstGraph,
    hasher: std.crypto.hash.sha2.Sha256,
    visiting: std.ArrayList(NodeId),

    fn init(graph: *InstGraph) GeneratedIdentityWriter {
        return .{
            .graph = graph,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .visiting = .empty,
        };
    }

    fn deinit(self: *GeneratedIdentityWriter) void {
        self.visiting.deinit(self.graph.allocator);
    }

    fn writeNode(self: *GeneratedIdentityWriter, raw_node: NodeId) Allocator.Error!void {
        self.graph.countDiagnostic("generated_identity_input_nodes_hashed");
        const node = self.graph.find(raw_node);
        const content = self.graph.nodes.items[@intFromEnum(node)];
        if (InstGraph.isGeneratedPrivateRootContent(content)) {
            const digest = content.named.def.generated orelse
                Common.invariant("generated-private identity writer encountered an unstamped nominal");
            self.writeBytes("generated-private-nominal");
            self.writeBytes(&digest.bytes);
            return;
        }
        if (content == .redirect) unreachable;
        if (content == .unresolved) {
            // This is the final producer boundary for the identity input.
            // Contextual substitutions have already run; if the checker
            // deliberately left a leaf polymorphic (for example the item
            // of an entirely empty iterator), commit its declared language
            // default at the exact leaf encountered by this hash traversal.
            // There is no separate probe or graph scan, and the node cannot
            // be refined after it becomes part of a generated identity.
            const completed: InstNode = if (content.unresolved.numeric_default_phase) |phase| blk: {
                const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                    Common.invariant("checking-finalized numeric variable reached generated identity production");
                break :blk switch (target) {
                    .dec => .{ .primitive = .dec },
                    .str => .{ .primitive = .str },
                };
            } else if (content.unresolved.row_default) |row_default| switch (row_default) {
                .empty_record => .empty_record,
                .empty_tag_union => .empty_tag_union,
            } else switch (content.unresolved.origin) {
                .checked_variable => Common.invariant("generated identity input contained a checked variable without an explicit default"),
                .row_extension => Common.invariant("generated identity input contained a row extension without its checked default"),
                .placeholder => Common.invariant("generated identity input contained an incomplete producer placeholder"),
            };
            try self.graph.setContent(node, completed);
            return self.writeNode(node);
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
                self.writeOptionalBuiltinOwner(named.builtin_owner);
                try self.writeNodeSpan(named.args);
            },
            .erased => |digest| {
                self.writeBytes("erased");
                self.writeBytes(&digest.bytes);
            },
            .zst => self.writeBytes("zst"),
        }
    }

    fn writeNodeSpan(self: *GeneratedIdentityWriter, nodes: []const NodeId) Allocator.Error!void {
        self.writeU32(@intCast(nodes.len));
        for (nodes) |node| try self.writeNode(node);
    }

    fn writeTypeDef(self: *GeneratedIdentityWriter, def: Type.TypeDef) void {
        self.writeBytes(self.graph.name_store.moduleIdentityBytes(def.module));
        self.writeOptionalU32(def.source_decl);
        if (def.source_decl == null) {
            self.writeBytes(self.graph.name_store.typeNameText(def.type_name));
        }
        self.writeOptionalDigest(def.generated);
        self.writeOptionalIteratorTopology(def.iterator_topology);
    }

    fn writeOptionalIteratorTopology(
        self: *GeneratedIdentityWriter,
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
        self: *GeneratedIdentityWriter,
        owner: ?static_dispatch.BuiltinOwner,
    ) void {
        if (owner) |actual| {
            self.writeU8(1);
            self.writeBytes(@tagName(actual));
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalDigest(self: *GeneratedIdentityWriter, digest: ?names.TypeDigest) void {
        if (digest) |actual| {
            self.writeU8(1);
            self.writeBytes(&actual.bytes);
        } else {
            self.writeU8(0);
        }
    }

    fn writeOptionalU32(self: *GeneratedIdentityWriter, value: ?u32) void {
        if (value) |actual| {
            self.writeU8(1);
            self.writeU32(actual);
        } else {
            self.writeU8(0);
        }
    }

    fn writeBytes(self: *GeneratedIdentityWriter, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.writeRawBytes(bytes);
    }

    fn writeU8(self: *GeneratedIdentityWriter, value: u8) void {
        self.writeRawBytes(&.{value});
    }

    fn writeU32(self: *GeneratedIdentityWriter, value: u32) void {
        var little = std.mem.nativeToLittle(u32, value);
        self.writeRawBytes(std.mem.asBytes(&little));
    }

    fn writeRawBytes(self: *GeneratedIdentityWriter, bytes: []const u8) void {
        self.hasher.update(bytes);
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
        .checked_variable => Common.invariant("checked variable reached Monotype materialization without an explicit default"),
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

test "resolved graph type detection does not default explicit open cells" {
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

    const str = try graph.newNode(.{ .primitive = .str });
    try graph.unify(unresolved, str);
    try std.testing.expect(try graph.typeIsResolved(open_list));
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

    const older_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    try graph.unify(arg, older_arg);
    try std.testing.expect(graph.sameFunctionInterface(left, right));

    const other_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const other = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{other_arg}), .ret = ret } });
    try std.testing.expect(!graph.sameFunctionInterface(left, other));
}

test "completed functions with the same exact children share one runtime node" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const arg = try graph.newNode(.{ .primitive = .u64 });
    const produced_ret = try graph.newNode(.{ .primitive = .bool });
    const left_ret = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const right_ret = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const left = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{arg}),
        .ret = left_ret,
    } });
    const right = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{arg}),
        .ret = right_ret,
    } });

    try graph.completeFunctionResult(left, produced_ret);
    try graph.completeFunctionResult(right, produced_ret);

    try std.testing.expect(graph.sameClass(left, right));
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

test "generated identity treats public opaque and private nominal views as one type" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xD1} ** 32));
    const type_name = try name_store.internTypeName("Date");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(15) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name, .source_decl = 4 };
    const backing = try graph.newNode(.{ .primitive = .i32 });
    const public = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    } });
    const private = try graph.newNode(.{ .named = .{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = backing, .use = .inspectable },
    } });

    const public_digest = try graph.generatedIdentityInputDigest(public);
    const private_digest = try graph.generatedIdentityInputDigest(private);
    try std.testing.expectEqualSlices(u8, &public_digest.bytes, &private_digest.bytes);
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

    try std.testing.expect(graph.sameClass(inner_named, outer_named));
    try std.testing.expectEqual(structural_backing, other);
    try std.testing.expectEqual(before_nodes + 1, graph.nodes.items.len);
    const outer_content = graph.content(outer_named);
    if (outer_content != .named) return error.TestUnexpectedResult;
    const compressed = outer_content.named;
    try std.testing.expect(!graph.sameClass(outer_named, compressed.backing.?.node));
    try std.testing.expect(graph.content(compressed.backing.?.node) == .tag_union);
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
