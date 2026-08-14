//! Per-specialization type solver for Monotype lowering.
//!
//! Checked types instantiate into union-find nodes with explicit row
//! extension links and constraints unify nodes order-independently. Producing
//! graphs never expose TypeId views: lowering consumes exact nodes until one
//! final materialization after relation freeze. Cross-specialization edges
//! copy finished Monotypes into closed graph structure, so a specialization
//! that needs more than its requested type conflicts instead of rewriting
//! another specialization's final type.

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
};

/// The exact cells whose representation is selected by one generalized field
/// kind. Producers register these when they instantiate the field; relation
/// freeze consumes them if no earlier specialization evidence selected a kind.
const FieldKindCells = struct {
    kind: FieldKindId,
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
    /// Recursive forward cell owned and filled by one type-construction
    /// traversal. Consumers may retain it as explicit request authority.
    construction_placeholder,
    /// Exact output cell owned by a value producer. Consumers must use that
    /// producer's separately registered representation request until it
    /// completes the output.
    producer_placeholder,
};

/// Defaulting evidence carried by an unresolved instantiation-graph node until
/// unification resolves it or materialization applies the default.
pub const InstVariable = struct {
    origin: InstVariableOrigin,
    numeric_default_phase: ?checked.NumericDefaultPhase = null,
    row_default: ?checked.RowDefault = null,
    specialization_default: ?checked.SpecializationDefault = null,
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
        specialization_default: ?checked.SpecializationDefault,
        checked_key: [32]u8,
    ) InstVariable {
        return .{
            .origin = .checked_variable,
            .numeric_default_phase = numeric_default_phase,
            .row_default = row_default,
            .specialization_default = specialization_default,
            .checked_key = checked_key,
        };
    }

    pub fn row(default: checked.RowDefault) InstVariable {
        return .{
            .origin = .row_extension,
            .row_default = default,
        };
    }

    pub fn constructionPlaceholder() InstVariable {
        return .{ .origin = .construction_placeholder };
    }

    pub fn producerPlaceholder() InstVariable {
        return .{ .origin = .producer_placeholder };
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

/// One node in the exact Monotype construction graph.
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

/// Input authority for lowering a function body. An exact destination is
/// supplied by an enclosing storage or control-flow boundary and guides the
/// body's contextual producers. A produced result has no destination yet.
/// In both cases the function value owns a separate forward result cell that
/// the body completes with the exact node it returns.
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
    mono_import_requests: u64 = 0,
    mono_import_hits: u64 = 0,
    mono_import_misses: u64 = 0,
    generated_identity_input_nodes_hashed: u64 = 0,
    generated_identity_intern_hits: u64 = 0,
    generated_identity_intern_misses: u64 = 0,
    generated_type_store_hits: u64 = 0,
    generated_type_store_misses: u64 = 0,
    permanent_inhabitedness_requests: u64 = 0,
    permanent_inhabitedness_hits: u64 = 0,
    closed_empty_finalization_requests: u64 = 0,
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
/// ids are dense only within one CheckedModule, so the module identity is an
/// inseparable part of every specialization-substitution key.
pub const CheckedBaseKey = struct {
    module_bytes: [32]u8,
    checked: checked.CheckedTypeId,
};

/// Whether a call slot contains request input or a completed value node.
pub const DirectRequestSelectionAuthority = enum(u8) {
    /// Context used to lower a value before its exact result exists. The
    /// value's producer replaces this seed when it completes.
    request,
    /// Exact node returned by a runtime value edge or constructed from an
    /// immutable substitution-free checked recipe.
    produced,
};

/// One checker-recorded call slot and its current exact node. Producer
/// selections are single-assignment; a producer may replace only an earlier
/// request seed for the same slot.
pub const DirectRequestSelection = struct {
    base: CheckedBaseKey,
    produced: NodeId,
    authority: DirectRequestSelectionAuthority = .produced,
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

/// Generated nominal lookup used by iterator construction.
pub const GeneratedIteratorLookup = GeneratedNominalLookup;

const RelationStamp = struct {
    left: NodeId,
    left_version: u32,
    right: NodeId,
    right_version: u32,
};

/// Result of reserving one ordinary nominal identity before evaluating its
/// deterministic declaration backing.
pub const OrdinaryNamedReservation = union(enum) {
    existing: NodeId,
    vacant: struct {
        named: NodeId,
        backing: NodeId,
    },
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

const InternedCompoundByAuthority = struct {
    checked_base: ?NodeId = null,
    produced: ?NodeId = null,
};

/// Per-specialization type solver. Checked types instantiate into union-find
/// nodes with explicit row extension links; constraints unify nodes
/// order-independently. Type-shaped inspection stays graph-native until final
/// sealing. Cross-specialization edges import final Monotypes as closed
/// structure, so a specialization that tries to exceed its requested type is
/// a unification conflict, not a silent divergence.
pub const InstGraph = struct {
    allocator: Allocator,
    relation_state: RelationState,
    types: *Type.Store,
    name_store: *names.NameStore,
    diagnostics: ?*GraphDiagnostics,
    arena_impl: std.heap.ArenaAllocator,
    nodes: std.ArrayList(InstNode),
    field_kinds: std.ArrayList(FieldKindNode),
    field_kind_cells: std.ArrayList(FieldKindCells),
    /// Ordinary primitive types are atomic identities. Reusing one node per
    /// primitive keeps independently encountered exact values identical
    /// without hashing or comparing any enclosing type graph.
    primitive_nodes: [std.meta.fields(Type.Primitive).len]?NodeId,
    empty_tag_union_node: ?NodeId,
    empty_record_node: ?NodeId,
    zst_node: ?NodeId,
    /// Compound interning keeps immutable checked construction recipes separate
    /// from exact produced values. They may have the same immediate node shape,
    /// but only the latter is runtime identity.
    tag_unions_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    checked_base_nodes: std.ArrayList(bool),
    checked_base_construction_depth: usize,
    versions: std.ArrayList(u32),
    processed_relations: std.AutoHashMap(RelationStamp, void),
    /// One-way memo for finished immutable Monotypes imported from outside
    /// this graph. Graph evidence may relate the copied nodes, but can never
    /// rewrite the source Monotype.
    imported_type_nodes: collections.DenseMap(Type.TypeId, NodeId),
    /// Current extension root for each row root. This is the authority for
    /// maintaining `row_parents`; stale extension edges are removed when row
    /// content changes.
    row_exts: std.ArrayList(?NodeId),
    /// Row nodes by the extension node they currently chain through.
    row_parents: collections.DenseMap(NodeId, std.ArrayList(NodeId)),
    /// Roots already proven unable to finalize as uninhabited. This entry is
    /// monotone: only permanent inhabited content can produce it. A root that
    /// later redirects is looked up by its new root and therefore recomputed.
    permanently_inhabited_nodes: collections.DenseMap(NodeId, void),
    /// Generated nominals keyed by the final content digest assigned by their
    /// producer. Identity, arguments, and backing are complete before entry.
    generated_nominal_intern: std.HashMap(names.TypeDigest, NodeId, GeneratedNominalInternContext, 80),
    /// Interned graph node for each completed nominal identity. A nominal's
    /// definition and exact argument nodes determine its type; the backing is
    /// implementation data owned by that identity, not another type axis.
    named_nodes_by_identity_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Interned compound nodes keyed only by their immediate exact children.
    /// Producers therefore share an already-built type without traversing any
    /// descendant graph.
    list_nodes_by_element: collections.DenseMap(NodeId, InternedCompoundByAuthority),
    box_nodes_by_element: collections.DenseMap(NodeId, InternedCompoundByAuthority),
    tuple_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Completed function values keyed by their exact immediate argument and
    /// result nodes. Open requests remain distinct until their producer fills
    /// the result edge; completion interns the one finished shape once.
    function_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    record_nodes_by_shape_hash: std.AutoHashMap(u64, std.ArrayList(NodeId)),
    /// Produced equivalent of an immutable checked structural recipe. A
    /// concrete recipe is converted bottom-up only when an exact producer
    /// actually stores it as a child; subsequent producers reuse this dense
    /// result directly.
    checked_base_produced_equivalents: collections.DenseMap(NodeId, NodeId),
    /// Fast producer lookup by the already-completed dense item node. Buckets
    /// distinguish declarations without re-hashing the item type graph.
    generated_iterators_by_item: collections.DenseMap(NodeId, std.ArrayList(NodeId)),
    /// Original item-index key for the small set of recursive reservations
    /// whose item is still a forward cell. Completion removes that exact old
    /// bucket entry before indexing the finished atomic identity.
    recursive_generated_iterator_item_keys: collections.DenseMap(NodeId, NodeId),
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
    /// Explicit positional authority for every argument of a function
    /// request. This cannot be keyed by checked type: two positions with the
    /// same checked type can carry different exact runtime callable sets.
    function_argument_authority_spans: std.ArrayList(DirectRequestSelectionSpan),
    function_argument_authorities: std.ArrayList(DirectRequestSelectionAuthority),
    /// Immutable flat substitutions selected by the checker's selection-edge
    /// program for each function request.
    direct_request_selection_spans: std.ArrayList(DirectRequestSelectionSpan),
    direct_request_selections: std.ArrayList(DirectRequestSelection),
    /// Scratch epoch marks for `compactRowParents`, indexed by node id. A
    /// slot carrying the current epoch means its class root is already kept
    /// in the list being compacted.
    row_parent_seen_epochs: std.ArrayList(u64),
    row_parent_seen_epoch: u64,
    /// Pool for the visited sets the graph's remaining exact walks create per
    /// query. Fresh maps re-allocate and re-zero sparse chunks across the node
    /// ID domain on every walk; pooled maps keep their chunks.
    node_set_pool: collections.DenseMapPool(NodeId, void),
    pub fn create(
        allocator: Allocator,
        types: *Type.Store,
        name_store: *names.NameStore,
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
            .field_kind_cells = .empty,
            .primitive_nodes = @splat(null),
            .empty_tag_union_node = null,
            .empty_record_node = null,
            .zst_node = null,
            .tag_unions_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .checked_base_nodes = .empty,
            .checked_base_construction_depth = 0,
            .versions = .empty,
            .processed_relations = std.AutoHashMap(RelationStamp, void).init(allocator),
            .imported_type_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .row_exts = .empty,
            .row_parents = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .permanently_inhabited_nodes = collections.DenseMap(NodeId, void).init(allocator),
            .generated_nominal_intern = std.HashMap(names.TypeDigest, NodeId, GeneratedNominalInternContext, 80).init(allocator),
            .named_nodes_by_identity_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .list_nodes_by_element = collections.DenseMap(NodeId, InternedCompoundByAuthority).init(allocator),
            .box_nodes_by_element = collections.DenseMap(NodeId, InternedCompoundByAuthority).init(allocator),
            .tuple_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .function_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .record_nodes_by_shape_hash = std.AutoHashMap(u64, std.ArrayList(NodeId)).init(allocator),
            .checked_base_produced_equivalents = collections.DenseMap(NodeId, NodeId).init(allocator),
            .generated_iterators_by_item = collections.DenseMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .recursive_generated_iterator_item_keys = collections.DenseMap(NodeId, NodeId).init(allocator),
            .generated_nominal_nodes = .empty,
            .request_checked_sources = .empty,
            .function_result_relations = .empty,
            .function_argument_authority_spans = .empty,
            .function_argument_authorities = .empty,
            .direct_request_selection_spans = .empty,
            .direct_request_selections = .empty,
            .row_parent_seen_epochs = .empty,
            .row_parent_seen_epoch = 0,
            .node_set_pool = collections.DenseMapPool(NodeId, void).init(allocator),
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

    pub fn destroy(self: *InstGraph) void {
        const allocator = self.allocator;
        var parents = self.row_parents.valueIterator();
        while (parents.next()) |list| {
            list.deinit(allocator);
        }
        var generated_item_buckets = self.generated_iterators_by_item.valueIterator();
        while (generated_item_buckets.next()) |bucket| bucket.deinit(allocator);
        self.generated_iterators_by_item.deinit();
        self.recursive_generated_iterator_item_keys.deinit();
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
        self.checked_base_produced_equivalents.deinit();
        self.generated_nominal_nodes.deinit(allocator);
        self.direct_request_selections.deinit(allocator);
        self.direct_request_selection_spans.deinit(allocator);
        self.request_checked_sources.deinit(allocator);
        self.function_result_relations.deinit(allocator);
        self.function_argument_authority_spans.deinit(allocator);
        self.function_argument_authorities.deinit(allocator);
        self.row_parent_seen_epochs.deinit(allocator);
        self.node_set_pool.deinit();
        self.row_parents.deinit();
        self.permanently_inhabited_nodes.deinit();
        self.row_exts.deinit(allocator);
        self.imported_type_nodes.deinit();
        self.processed_relations.deinit();
        var tag_union_buckets = self.tag_unions_by_shape_hash.valueIterator();
        while (tag_union_buckets.next()) |bucket| bucket.deinit(allocator);
        self.tag_unions_by_shape_hash.deinit();
        self.versions.deinit(allocator);
        self.checked_base_nodes.deinit(allocator);
        self.nodes.deinit(allocator);
        self.field_kind_cells.deinit(allocator);
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
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const root = self.findFieldKind(raw);
        try self.field_kind_cells.append(self.allocator, .{
            .kind = root,
            .slot = slot,
            .value = value,
        });
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
            std.mem.swap(FieldKindId, &left, &right);
        }
        const right_state = self.field_kinds.items[@intFromEnum(right)].resolved;
        self.field_kinds.items[@intFromEnum(right)].parent = left;
        if (self.field_kinds.items[@intFromEnum(left)].rank == self.field_kinds.items[@intFromEnum(right)].rank) {
            self.field_kinds.items[@intFromEnum(left)].rank += 1;
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

    pub fn relateRecordFieldKind(self: *InstGraph, left: InstField, right: InstField) void {
        self.requireRelationProduction();
        _ = self.unifyFieldKinds(left.kind, left.default, right.kind, right.default);
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

    pub fn functionArgumentAuthorities(
        self: *const InstGraph,
        request_fn: NodeId,
    ) ?[]const DirectRequestSelectionAuthority {
        const span = self.function_argument_authority_spans.items[@intFromEnum(request_fn)];
        if (!span.isInitialized()) return null;
        const start: usize = span.start;
        return self.function_argument_authorities.items[start .. start + span.len];
    }

    pub fn registerFunctionArgumentAuthorities(
        self: *InstGraph,
        request_fn: NodeId,
        authorities: []const DirectRequestSelectionAuthority,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const entry = &self.function_argument_authority_spans.items[@intFromEnum(request_fn)];
        if (entry.isInitialized()) {
            const existing = self.functionArgumentAuthorities(request_fn).?;
            if (!std.mem.eql(DirectRequestSelectionAuthority, existing, authorities)) {
                Common.invariant("function request was registered with two argument-authority vectors");
            }
            return;
        }
        const start: u32 = @intCast(self.function_argument_authorities.items.len);
        try self.function_argument_authorities.appendSlice(self.allocator, authorities);
        entry.* = .{ .start = start, .len = @intCast(authorities.len) };
    }

    pub fn registerUniformFunctionArgumentAuthority(
        self: *InstGraph,
        request_fn: NodeId,
        arity: usize,
        authority: DirectRequestSelectionAuthority,
    ) Allocator.Error!void {
        const authorities = try self.arena().alloc(DirectRequestSelectionAuthority, arity);
        @memset(authorities, authority);
        return self.registerFunctionArgumentAuthorities(request_fn, authorities);
    }

    pub fn inheritFunctionArgumentAuthorities(
        self: *InstGraph,
        source_fn: NodeId,
        destination_fn: NodeId,
    ) Allocator.Error!void {
        const authorities = self.functionArgumentAuthorities(source_fn) orelse
            Common.invariant("function request rebasing had no argument-authority vector");
        return self.registerFunctionArgumentAuthorities(destination_fn, authorities);
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

    /// Whether two requests carry the same node and authority for every
    /// checker-recorded identity slot. A request seed and a produced edge are
    /// not interchangeable even when they currently name the same graph node.
    /// Callers compare the immutable checked callable base separately.
    pub fn sameDirectRequestSelections(self: *InstGraph, left_fn: NodeId, right_fn: NodeId) bool {
        const left = self.directRequestSelections(left_fn);
        const right = self.directRequestSelections(right_fn);
        if (left.len != right.len) return false;
        for (left) |left_selection| {
            const right_selection = for (right) |candidate| {
                if (candidate.base.checked == left_selection.base.checked and
                    std.mem.eql(u8, &candidate.base.module_bytes, &left_selection.base.module_bytes)) break candidate;
            } else return false;
            if (left_selection.authority != right_selection.authority) return false;
            if (!self.sameClass(left_selection.produced, right_selection.produced)) return false;
        }
        return true;
    }

    /// Whether two callable requests have the same complete producer inputs.
    /// A produced result is output and is therefore excluded; an exact result
    /// destination is immutable contextual input and participates directly.
    pub fn sameFunctionRequestInputs(self: *InstGraph, left_fn: NodeId, right_fn: NodeId) Allocator.Error!bool {
        const left = try self.functionNodes(left_fn);
        const right = try self.functionNodes(right_fn);
        const left_result_relation = self.functionResultRelation(left_fn) orelse
            Common.invariant("function request comparison received a left request without result authority");
        const right_result_relation = self.functionResultRelation(right_fn) orelse
            Common.invariant("function request comparison received a right request without result authority");
        if (left_result_relation != right_result_relation) return false;
        if (left_result_relation == .exact_destination and
            !self.sameClass(left.ret, right.ret)) return false;
        if (left.args.len != right.args.len) return false;
        for (left.args, right.args) |left_arg, right_arg| {
            if (!self.sameClass(left_arg, right_arg)) return false;
        }
        return self.sameDirectRequestSelections(left_fn, right_fn);
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
        _ = try self.internCompletedFunction(fn_node);
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
                if (existing_named.args.len != 1) {
                    Common.invariant("generated iterator item index contained a mismatched item arity");
                }
                return try self.finishGeneratedIteratorReservation(
                    existing,
                    public_def,
                    item_root,
                );
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

    /// Complete one producer-owned recursive reservation immediately after the
    /// callback that owns its item cell returns. No graph-wide finalization
    /// pass is allowed to rediscover or finish this producer operation later.
    pub fn completeRecursiveGeneratedIterator(
        self: *InstGraph,
        reservation: NodeId,
        public_def: Type.TypeDef,
        item_node: NodeId,
    ) Allocator.Error!NodeId {
        self.requireRelationProduction();
        const completed = try self.finishGeneratedIteratorReservation(
            reservation,
            public_def,
            item_node,
        );
        return completed.existing orelse
            Common.invariant("recursive generated iterator completion produced no atomic identity");
    }

    /// Stamp one already-built recursive iterator reservation from its exact
    /// item cell. The caller names the reservation directly, so completion is
    /// independent of the item index entry created before a forward cell was
    /// filled. Equal completed inputs redirect to the earlier content address;
    /// no backing or enclosing compound is inspected.
    fn finishGeneratedIteratorReservation(
        self: *InstGraph,
        raw_existing: NodeId,
        public_def: Type.TypeDef,
        item_node: NodeId,
    ) Allocator.Error!GeneratedIteratorLookup {
        const existing = self.find(raw_existing);
        const existing_named = switch (self.content(existing)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator reservation completion received a non-named node"),
        };
        if (!sameTypeDef(public_def, existing_named.def) or existing_named.args.len != 1) {
            Common.invariant("generated iterator reservation completion received a different declaration or arity");
        }
        if (existing_named.def.generated) |digest| {
            self.countDiagnostic("generated_identity_intern_hits");
            return .{ .existing = existing, .digest = digest };
        }

        const item_root = self.find(item_node);
        if (self.find(existing_named.args[0]) != item_root) {
            Common.invariant("generated iterator reservation completed from a different item cell");
        }
        self.removeRecursiveGeneratedIteratorItemIndex(existing);
        const digest = try self.generatedIteratorInternDigest(public_def, item_root);
        if (self.generated_nominal_intern.get(digest)) |raw_interned| {
            const interned = self.find(raw_interned);
            if (interned != existing) try self.redirectRoot(interned, existing);
            self.countDiagnostic("generated_identity_intern_hits");
            return .{ .existing = interned, .digest = digest };
        }

        var stamped = existing_named;
        stamped.def.generated = digest;
        try self.setContent(existing, .{ .named = stamped });
        const entry = try self.generated_nominal_intern.getOrPut(digest);
        if (entry.found_existing) {
            Common.invariant("generated iterator identity appeared while stamping one reservation");
        }
        entry.value_ptr.* = existing;
        try self.finishGeneratedIteratorAtDigest(existing, digest);
        return .{ .existing = existing, .digest = digest };
    }

    /// Reserve one recursive generated iterator around an exact forward item
    /// cell without hashing or defaulting that cell. The producer that owns
    /// the cell completes it before the producer stamps the final content
    /// address. Repeated reservations for the same declaration and cell return
    /// the same construction node.
    pub fn reserveRecursiveGeneratedIterator(
        self: *InstGraph,
        public_def: Type.TypeDef,
        item_node: NodeId,
        context: anytype,
        comptime fill: fn (@TypeOf(context), NodeId) Allocator.Error!InstNode,
    ) Allocator.Error!NodeId {
        self.requireRelationProduction();
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
                    Common.invariant("recursive generated iterator item index contained a mismatched item");
                }
                self.countDiagnostic("generated_identity_intern_hits");
                return existing;
            }
        }

        const reserved = try self.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
        const node_content = try fill(context, reserved);
        if (!isGeneratedPrivateRootContent(node_content) or node_content.named.def.generated != null) {
            Common.invariant("recursive iterator reservation did not produce one unstamped private nominal");
        }
        try self.setContent(reserved, node_content);
        if (self.find(reserved) != reserved or
            !isGeneratedPrivateRootContent(self.content(reserved)) or
            self.content(reserved).named.def.generated != null)
        {
            Common.invariant("recursive iterator reservation collided with an ordinary named identity");
        }
        try self.indexGeneratedIteratorByItem(reserved);
        try self.recursive_generated_iterator_item_keys.putNoClobber(reserved, item_root);
        self.countDiagnostic("generated_identity_intern_misses");
        return reserved;
    }

    /// Reserve, fill, and record one recursive generated iterator nominal.
    /// Registering the reservation before `fill` lets recursive occurrences
    /// with the same content identity resolve directly to this atomic node.
    pub fn addRecursiveGeneratedIterator(
        self: *InstGraph,
        digest: names.TypeDigest,
        context: anytype,
        comptime fill: fn (@TypeOf(context), NodeId) Allocator.Error!InstNode,
    ) Allocator.Error!NodeId {
        self.requireRelationProduction();
        const reserved = try self.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
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

    fn removeRecursiveGeneratedIteratorItemIndex(self: *InstGraph, reservation: NodeId) void {
        const indexed_item = self.recursive_generated_iterator_item_keys.fetchRemove(reservation) orelse
            Common.invariant("unstamped recursive iterator reservation had no original item index");
        const bucket = self.generated_iterators_by_item.getPtr(indexed_item.value) orelse
            Common.invariant("recursive iterator reservation lost its original item bucket");
        for (bucket.items, 0..) |candidate, index| {
            if (candidate != reservation) continue;
            _ = bucket.swapRemove(index);
            if (bucket.items.len == 0) {
                var removed = self.generated_iterators_by_item.fetchRemove(indexed_item.value) orelse
                    Common.invariant("empty recursive iterator item bucket disappeared during removal");
                removed.value.deinit(self.allocator);
            }
            return;
        }
        Common.invariant("recursive iterator reservation was absent from its original item bucket");
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
    ) Allocator.Error!GeneratedNominalLookup {
        const digest: names.TypeDigest = source_def.generated orelse digest: {
            var public_def = source_def;
            public_def.generated = null;
            var writer = GeneratedIdentityWriter.init(self);
            defer writer.deinit();
            writer.writeBytes("roc.generated_nominal.runtime_implementation.v3");
            writer.writeTypeDef(public_def);
            // The compiler-defined backing recipe is uniquely owned by the
            // generated declaration. Its exact public arguments are therefore
            // the complete variable inputs to the runtime implementation.
            // Lookup happens before the recipe is evaluated, so a hit avoids
            // constructing or hashing the private backing altogether.
            try writer.writeNodeSpan(implementation_args);
            break :digest .{ .bytes = writer.hasher.finalResult() };
        };
        if (self.generated_nominal_intern.get(digest)) |existing| {
            self.countDiagnostic("generated_identity_intern_hits");
            return .{ .existing = self.find(existing), .digest = digest };
        }
        self.countDiagnostic("generated_identity_intern_misses");
        return .{ .existing = null, .digest = digest };
    }

    /// Record a completed generated nominal at the identity assigned by its
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

    fn optionalFieldSlotNode(self: *InstGraph, value: NodeId) Allocator.Error!NodeId {
        const missing = try self.name_store.internTagLabel("#Missing");
        const present = try self.name_store.internTagLabel("#Present");
        const tags = try self.arena().alloc(InstTag, 2);
        tags[0] = .{ .name = missing, .checked_name = missing, .payloads = &.{} };
        tags[1] = .{
            .name = present,
            .checked_name = present,
            .payloads = try self.arena().dupe(NodeId, &.{value}),
        };
        return try self.newNode(.{
            .tag_union = .{
                .tags = tags,
                .ext = try self.newNode(.empty_tag_union),
            },
        });
    }

    /// Commit every generalized field kind and complete each occurrence's
    /// private forward slot from its own exact value node. Kind identity is
    /// shared; runtime value identity is not. No checked-base node is merged
    /// and no record graph is traversed.
    fn finalizeUndeterminedFieldKinds(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        for (0..self.field_kinds.items.len) |raw_index| {
            const id: FieldKindId = @enumFromInt(raw_index);
            const root = self.findFieldKind(id);
            if (root != id) continue;
            const node = &self.field_kinds.items[raw_index];
            _ = node.resolved orelse resolved: {
                node.resolved = .required;
                break :resolved ResolvedFieldKind.required;
            };
        }
        for (self.field_kind_cells.items) |cells| {
            const resolved = self.resolvedFieldKind(.{ .undetermined = cells.kind }) orelse
                Common.invariant("field-kind occurrence remained unresolved after defaulting");
            const representation = switch (resolved) {
                .required, .defaulted => cells.value,
                .optional => try self.optionalFieldSlotNode(cells.value),
            };
            try self.completeProducedSelection(cells.slot, representation);
        }
    }

    /// Complete pending specialization defaults and prevent any later
    /// relation production. Final type sealing remains available after this
    /// transition.
    pub fn freezeRelations(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        try self.finalizeUndeterminedFieldKinds();
        if (self.recursive_generated_iterator_item_keys.count() != 0) {
            Common.invariant("recursive generated iterator producer reached relation freeze before callback completion");
        }
        self.relation_state = .frozen;
    }

    pub fn finalizesAsClosedEmptyTagUnion(self: *InstGraph, raw_node: NodeId) bool {
        self.requireFrozenRelations();
        self.countDiagnostic("closed_empty_finalization_requests");
        var node = self.find(raw_node);
        var remaining = self.nodes.items.len;
        while (remaining > 0) : (remaining -= 1) {
            switch (self.nodes.items[@intFromEnum(node)]) {
                .redirect => unreachable,
                .empty_tag_union => return true,
                .unresolved => |variable| {
                    if (variable.numeric_default_phase != null) return false;
                    if (variable.row_default) |row_default| return row_default == .empty_tag_union;
                    if (variable.specialization_default) |default| return default == .empty_tag_union;
                    return switch (variable.origin) {
                        .checked_variable => Common.invariant("checked variable reached final demand validation without an explicit default"),
                        .row_extension => Common.invariant("row extension reached final demand validation without row default"),
                        .construction_placeholder => Common.invariant("instantiation construction placeholder reached final demand validation"),
                        .producer_placeholder => Common.invariant("instantiation producer placeholder reached final demand validation"),
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
        self.countDiagnostic("permanent_inhabitedness_requests");
        const root = self.find(raw_node);
        if (self.permanently_inhabited_nodes.contains(root)) {
            self.countDiagnostic("permanent_inhabitedness_hits");
            return false;
        }
        var visiting = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&visiting);
        const may_finalize = try self.mayFinalizeAsUninhabitedInner(root, &visiting);
        if (!may_finalize) try self.permanently_inhabited_nodes.put(root, {});
        return may_finalize;
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
                if (variable.specialization_default) |default| break :blk default == .empty_tag_union;
                break :blk switch (variable.origin) {
                    .checked_variable => Common.invariant("checked variable reached final inhabitance validation without an explicit default"),
                    .row_extension => Common.invariant("row extension reached final inhabitance validation without row default"),
                    .construction_placeholder => Common.invariant("instantiation construction placeholder reached final inhabitance validation"),
                    .producer_placeholder => Common.invariant("instantiation producer placeholder reached final inhabitance validation"),
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
        const bucket = self.tag_unions_by_shape_hash.get(self.producedTagUnionHash(tag_union)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (self.checked_base_nodes.items[@intFromEnum(root)] == (self.checked_base_construction_depth != 0) and
                candidate_content == .tag_union and
                self.producedTagUnionEql(candidate_content.tag_union, tag_union)) return root;
        }
        return null;
    }

    fn registerTagUnionShape(self: *InstGraph, raw_node: NodeId, tag_union: InstTagUnion) Allocator.Error!void {
        const node = self.find(raw_node);
        const bucket = try self.tag_unions_by_shape_hash.getOrPut(self.producedTagUnionHash(tag_union));
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        for (bucket.value_ptr.items) |candidate| if (self.find(candidate) == node) return;
        try bucket.value_ptr.append(self.allocator, node);
    }

    /// Construct one completed producer-owned tag row. Row chaining is a
    /// checked construction recipe, not runtime identity: consume the chain,
    /// sort the variants, and intern the exact immediate children once at
    /// the producer boundary.
    pub fn newProducedTagUnion(
        self: *InstGraph,
        initial_tags: []const InstTag,
        raw_ext: NodeId,
    ) Allocator.Error!NodeId {
        return try self.newProducedTagUnionInner(initial_tags, raw_ext, true);
    }

    fn newProducedTagUnionInner(
        self: *InstGraph,
        initial_tags: []const InstTag,
        raw_ext: NodeId,
        intern_children: bool,
    ) Allocator.Error!NodeId {
        var tags = std.ArrayList(InstTag).empty;
        defer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, initial_tags);

        var ext = self.find(raw_ext);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        while (self.nodes.items[@intFromEnum(ext)] == .tag_union) {
            const entry = try seen.getOrPut(ext);
            if (entry.found_existing) {
                Common.invariant("completed produced tag row contained an extension cycle");
            }
            const tail = self.nodes.items[@intFromEnum(ext)].tag_union;
            try tags.appendSlice(self.allocator, tail.tags);
            ext = self.find(tail.ext);
        }
        switch (self.nodes.items[@intFromEnum(ext)]) {
            .unresolved, .empty_tag_union => {},
            .redirect => unreachable,
            .primitive, .list, .box, .tuple, .func, .record, .empty_record, .named, .erased, .zst => Common.invariant("completed produced tag row had a non-tag extension"),
            .tag_union => unreachable,
        }
        ext = try self.internImmediateChild(ext);
        std.mem.sort(InstTag, tags.items, self.name_store, instTagLessThan);
        const interned_tags = try self.arena().dupe(InstTag, tags.items);
        for (interned_tags) |*tag| {
            if (!intern_children) continue;
            var interned_payloads: ?[]NodeId = null;
            for (tag.payloads, 0..) |payload, index| {
                const interned = try self.internImmediateChild(payload);
                if (interned_payloads) |payloads| {
                    payloads[index] = interned;
                } else if (self.find(payload) != interned) {
                    const payloads = try self.arena().alloc(NodeId, tag.payloads.len);
                    @memcpy(payloads[0..index], tag.payloads[0..index]);
                    payloads[index] = interned;
                    interned_payloads = payloads;
                }
            }
            if (interned_payloads) |payloads| tag.payloads = payloads;
        }
        return try self.newNode(.{ .tag_union = .{
            .tags = interned_tags,
            .ext = ext,
        } });
    }

    /// Construct one completed producer-owned record row. Checked row
    /// extension topology is not runtime identity: consume the chain, sort
    /// the fields, and intern only the exact immediate field nodes.
    pub fn newProducedRecord(
        self: *InstGraph,
        initial_fields: []const InstField,
        raw_ext: NodeId,
    ) Allocator.Error!NodeId {
        return try self.newProducedRecordInner(initial_fields, raw_ext, true);
    }

    fn newProducedRecordInner(
        self: *InstGraph,
        initial_fields: []const InstField,
        raw_ext: NodeId,
        intern_children: bool,
    ) Allocator.Error!NodeId {
        var fields = std.ArrayList(InstField).empty;
        defer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, initial_fields);

        var ext = self.find(raw_ext);
        var seen = collections.DenseMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        while (self.nodes.items[@intFromEnum(ext)] == .record) {
            const entry = try seen.getOrPut(ext);
            if (entry.found_existing) {
                Common.invariant("completed produced record row contained an extension cycle");
            }
            const tail = self.nodes.items[@intFromEnum(ext)].record;
            try fields.appendSlice(self.allocator, tail.fields);
            ext = self.find(tail.ext);
        }
        switch (self.nodes.items[@intFromEnum(ext)]) {
            .unresolved, .empty_record => {},
            .redirect => unreachable,
            .primitive, .list, .box, .tuple, .func, .tag_union, .empty_tag_union, .named, .erased, .zst => Common.invariant("completed produced record row had a non-record extension"),
            .record => unreachable,
        }
        ext = try self.internImmediateChild(ext);
        std.mem.sort(InstField, fields.items, self.name_store, instFieldLessThan);
        const interned_fields = try self.arena().dupe(InstField, fields.items);
        if (intern_children) {
            for (interned_fields) |*field| {
                field.ty = try self.internImmediateChild(field.ty);
                if (field.value_ty) |value_ty| {
                    field.value_ty = try self.internImmediateChild(value_ty);
                }
            }
        }
        return try self.newNode(.{ .record = .{
            .fields = interned_fields,
            .ext = ext,
        } });
    }

    /// Construct a completed producer-owned list from its one exact immediate
    /// element node. This consumes only that child; it never walks descendants.
    pub fn newProducedList(self: *InstGraph, element: NodeId) Allocator.Error!NodeId {
        return try self.newNode(.{ .list = try self.internImmediateChild(element) });
    }

    /// Construct a completed producer-owned box from its one exact immediate
    /// element node. This consumes only that child; it never walks descendants.
    pub fn newProducedBox(self: *InstGraph, element: NodeId) Allocator.Error!NodeId {
        return try self.newNode(.{ .box = try self.internImmediateChild(element) });
    }

    /// Construct a completed producer-owned tuple from exact immediate item
    /// nodes. Each item is normalized at this boundary and is not traversed.
    pub fn newProducedTuple(self: *InstGraph, items: []const NodeId) Allocator.Error!NodeId {
        const interned_items = try self.arena().alloc(NodeId, items.len);
        for (items, interned_items) |item, *interned| {
            interned.* = try self.internImmediateChild(item);
        }
        return try self.newNode(.{ .tuple = interned_items });
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
        const entry = self.list_nodes_by_element.get(element) orelse return null;
        const candidate = self.find(if (self.checked_base_construction_depth != 0)
            entry.checked_base orelse return null
        else
            entry.produced orelse return null);
        const candidate_content = self.nodes.items[@intFromEnum(candidate)];
        return if (candidate_content == .list and self.find(candidate_content.list) == element) candidate else null;
    }

    fn existingBoxElement(self: *InstGraph, raw_element: NodeId) ?NodeId {
        const element = self.find(raw_element);
        const entry = self.box_nodes_by_element.get(element) orelse return null;
        const candidate = self.find(if (self.checked_base_construction_depth != 0)
            entry.checked_base orelse return null
        else
            entry.produced orelse return null);
        const candidate_content = self.nodes.items[@intFromEnum(candidate)];
        return if (candidate_content == .box and self.find(candidate_content.box) == element) candidate else null;
    }

    fn registerListElement(self: *InstGraph, raw_element: NodeId, node: NodeId) Allocator.Error!void {
        const entry = try self.list_nodes_by_element.getOrPut(self.find(raw_element));
        if (!entry.found_existing) entry.value_ptr.* = .{};
        if (self.checked_base_construction_depth != 0) {
            entry.value_ptr.checked_base = node;
        } else {
            entry.value_ptr.produced = node;
        }
    }

    fn registerBoxElement(self: *InstGraph, raw_element: NodeId, node: NodeId) Allocator.Error!void {
        const entry = try self.box_nodes_by_element.getOrPut(self.find(raw_element));
        if (!entry.found_existing) entry.value_ptr.* = .{};
        if (self.checked_base_construction_depth != 0) {
            entry.value_ptr.checked_base = node;
        } else {
            entry.value_ptr.produced = node;
        }
    }

    fn existingTupleShape(self: *InstGraph, items: []const NodeId) ?NodeId {
        const bucket = self.tuple_nodes_by_shape_hash.get(self.nodeSpanShapeHash(items)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (self.checked_base_nodes.items[@intFromEnum(root)] == (self.checked_base_construction_depth != 0) and
                candidate_content == .tuple and
                self.sameNodeSpanShape(candidate_content.tuple, items)) return root;
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

    /// Intern a function only when its producer has completed the exact
    /// result edge. Open requests retain distinct forward cells; completed
    /// values with identical immediate children become one runtime type node.
    fn internCompletedFunction(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        const node = self.find(raw_node);
        const function = switch (self.nodes.items[@intFromEnum(node)]) {
            .func => |function| function,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .tag_union, .record, .empty_tag_union, .empty_record, .named, .erased, .zst => Common.invariant("completed function interning received a non-function node"),
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

    /// Construct and intern one function whose argument and result nodes are
    /// already exact. Open call requests must continue to use `newNode`
    /// directly so their forward result cells remain independently owned.
    pub fn newProducedFunction(
        self: *InstGraph,
        args: []const NodeId,
        ret: NodeId,
    ) Allocator.Error!NodeId {
        const stored_args = try self.arena().alloc(NodeId, args.len);
        for (args, stored_args) |arg, *stored| {
            stored.* = try self.internImmediateChild(arg);
        }
        const node = try self.newNode(.{ .func = .{
            .args = stored_args,
            .ret = try self.internImmediateChild(ret),
        } });
        return try self.internCompletedFunction(node);
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
            hasher.update(&.{@intFromBool(field.value_ty != null)});
            if (field.value_ty) |value_ty| {
                var value_child = std.mem.nativeToLittle(u32, @intFromEnum(self.find(value_ty)));
                hasher.update(std.mem.asBytes(&value_child));
            }
            hasher.update(&.{@intFromEnum(std.meta.activeTag(field.kind))});
            switch (field.kind) {
                .undetermined => |kind| {
                    var kind_id = std.mem.nativeToLittle(u32, @intFromEnum(kind));
                    hasher.update(std.mem.asBytes(&kind_id));
                },
                .defaulted => |default| updateFieldDefaultHash(&hasher, default),
                .sealed, .required, .optional => {},
            }
            hasher.update(&.{@intFromBool(field.default != null)});
            if (field.default) |default| updateFieldDefaultHash(&hasher, default);
        }
        return hasher.final();
    }

    fn sameRecordShape(self: *InstGraph, left: InstNode, right: InstNode) bool {
        const left_row = left.record;
        const right_row = right.record;
        if (left_row.fields.len != right_row.fields.len or self.find(left_row.ext) != self.find(right_row.ext)) return false;
        for (left_row.fields, right_row.fields) |left_field, right_field| {
            if (left_field.name != right_field.name or self.find(left_field.ty) != self.find(right_field.ty)) return false;
            if (left_field.value_ty != null and right_field.value_ty != null) {
                if (self.find(left_field.value_ty.?) != self.find(right_field.value_ty.?)) return false;
            } else if (left_field.value_ty != right_field.value_ty) return false;
            if (!std.meta.eql(left_field.kind, right_field.kind)) return false;
            if (!instFieldDefaultEql(left_field.default, right_field.default)) return false;
        }
        return true;
    }

    fn existingRecordShape(self: *InstGraph, record: InstNode) ?NodeId {
        const bucket = self.record_nodes_by_shape_hash.get(self.recordShapeHash(record)) orelse return null;
        for (bucket.items) |candidate| {
            const root = self.find(candidate);
            const candidate_content = self.nodes.items[@intFromEnum(root)];
            if (self.checked_base_nodes.items[@intFromEnum(root)] == (self.checked_base_construction_depth != 0) and
                candidate_content == .record and
                self.sameRecordShape(candidate_content, record)) return root;
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

    /// Materialize only defaults explicitly recorded by checking. Such a
    /// variable is a concrete immutable recipe at an exact producer boundary;
    /// a variable without one remains open and may not become a produced
    /// child.
    fn explicitInstVariableDefault(variable: InstVariable) ?InstNode {
        if (variable.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                Common.invariant("checking-finalized numeric variable reached exact producer interning");
            return switch (target) {
                .dec => .{ .primitive = .dec },
                .str => .{ .primitive = .str },
            };
        }
        if (variable.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => .empty_record,
                .empty_tag_union => .empty_tag_union,
            };
        }
        if (variable.specialization_default) |default| {
            return switch (default) {
                .empty_tag_union => .empty_tag_union,
            };
        }
        return null;
    }

    /// Convert one immutable concrete checked recipe into the produced
    /// interner domain. This follows the recipe once, at the producer that
    /// actually needs it, and memoizes every structural node by dense ID.
    /// Nominals and primitives are atomic exact identities; an unresolved
    /// checked recipe is not concrete and therefore cannot cross this edge.
    fn producedEquivalentOfCheckedBase(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        const node = self.find(raw_node);
        if (!self.checked_base_nodes.items[@intFromEnum(node)]) return node;
        if (self.checked_base_produced_equivalents.get(node)) |existing| return self.find(existing);

        const produced = switch (self.nodes.items[@intFromEnum(node)]) {
            .redirect => unreachable,
            .unresolved => |variable| if (explicitInstVariableDefault(variable)) |default|
                try self.newNode(default)
            else
                Common.invariant("an unresolved checked recipe reached an exact producer child"),
            .primitive, .empty_tag_union, .empty_record, .erased, .zst => node,
            .named => |named| if (named.def.generated != null)
                node
            else
                try self.newNode(.{ .named = named }),
            .list => |element| try self.newProducedList(
                try self.producedEquivalentOfCheckedBase(element),
            ),
            .box => |element| try self.newProducedBox(
                try self.producedEquivalentOfCheckedBase(element),
            ),
            .tuple => |items| blk: {
                const produced_items = try self.arena().alloc(NodeId, items.len);
                for (items, produced_items) |item, *produced_item| {
                    produced_item.* = try self.producedEquivalentOfCheckedBase(item);
                }
                break :blk try self.newProducedTuple(produced_items);
            },
            .func => |function| blk: {
                const produced_args = try self.arena().alloc(NodeId, function.args.len);
                for (function.args, produced_args) |arg, *produced_arg| {
                    produced_arg.* = try self.producedEquivalentOfCheckedBase(arg);
                }
                break :blk try self.newProducedFunction(
                    produced_args,
                    try self.producedEquivalentOfCheckedBase(function.ret),
                );
            },
            .record => |record| blk: {
                const row = try self.flattenRecordRow(node);
                const produced_fields = try self.arena().dupe(InstField, row.fields);
                for (produced_fields) |*field| {
                    field.ty = try self.producedEquivalentOfCheckedBase(field.ty);
                    if (field.value_ty) |value_ty| {
                        field.value_ty = try self.producedEquivalentOfCheckedBase(value_ty);
                    }
                }
                _ = record;
                break :blk try self.newProducedRecord(produced_fields, row.ext);
            },
            .tag_union => |tag_union| blk: {
                const row = try self.flattenTagRow(node);
                const produced_tags = try self.arena().dupe(InstTag, row.tags);
                for (produced_tags) |*tag| {
                    const payloads = try self.arena().alloc(NodeId, tag.payloads.len);
                    for (tag.payloads, payloads) |payload, *produced_payload| {
                        produced_payload.* = try self.producedEquivalentOfCheckedBase(payload);
                    }
                    tag.payloads = payloads;
                }
                _ = tag_union;
                break :blk try self.newProducedTagUnion(produced_tags, row.ext);
            },
        };
        try self.checked_base_produced_equivalents.put(node, produced);
        return self.find(produced);
    }

    /// Return the interned identity node for one already-built immediate
    /// child. Row extensions and concrete checked recipes are normalized once
    /// here, when a producer records that child, rather than being
    /// rediscovered by later call consumers.
    fn internImmediateChild(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        var node = self.find(raw_node);
        var remaining = self.nodes.items.len;
        while (self.nodes.items[@intFromEnum(node)] == .named and
            self.nodes.items[@intFromEnum(node)].named.kind == .alias)
        {
            if (remaining == 0) Common.invariant("transparent alias chain contained a cycle");
            remaining -= 1;
            node = self.find((self.nodes.items[@intFromEnum(node)].named.backing orelse
                Common.invariant("transparent alias child had no backing")).node);
        }
        if (self.nodes.items[@intFromEnum(node)] == .unresolved) {
            if (explicitInstVariableDefault(self.nodes.items[@intFromEnum(node)].unresolved)) |defaulted| {
                node = try self.newNode(defaulted);
            }
        }
        if (self.checked_base_construction_depth == 0 and
            self.checked_base_nodes.items[@intFromEnum(node)])
        {
            return try self.producedEquivalentOfCheckedBase(node);
        }
        return switch (self.nodes.items[@intFromEnum(node)]) {
            .tag_union => blk: {
                const row = try self.flattenTagRow(node);
                break :blk try self.newProducedTagUnionInner(row.tags, row.ext, false);
            },
            .record => blk: {
                const row = try self.flattenRecordRow(node);
                break :blk try self.newProducedRecordInner(row.fields, row.ext, false);
            },
            .named => |named| blk: {
                // A nominal may have been reserved before one of its exact
                // argument cells was selected. Its original interner bucket
                // is then keyed by the old argument roots. Re-register the
                // finished identity under its current immediate arguments so
                // later compound producers consume one canonical child.
                if (self.existingNamedIdentity(named)) |existing| break :blk existing;
                try self.registerNamedIdentity(node, named);
                break :blk node;
            },
            .redirect => unreachable,
            .unresolved, .primitive, .list, .box, .tuple, .func, .empty_tag_union, .empty_record, .erased, .zst => node,
        };
    }

    /// Intern one exact producer root at the moment it becomes a
    /// specialization identity. This is the root counterpart of recording an
    /// immediate produced child: it consumes only transparent aliases and the
    /// root's row-extension chain, then reuses the existing immediate-shape
    /// interner. It never traverses through a generated nominal.
    pub fn internProducedIdentity(self: *InstGraph, raw_node: NodeId) Allocator.Error!NodeId {
        const node = try self.internImmediateChild(raw_node);
        return switch (self.nodes.items[@intFromEnum(node)]) {
            .list => |element| try self.newProducedList(element),
            .box => |element| try self.newProducedBox(element),
            .tuple => |items| try self.newProducedTuple(items),
            .named => |named| if (try self.internNamedArguments(named)) |interned|
                try self.newNode(.{ .named = interned })
            else
                node,
            .redirect, .unresolved, .primitive, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => node,
        };
    }

    fn internNamedArguments(self: *InstGraph, named: InstNamed) Allocator.Error!?InstNamed {
        var interned_args: ?[]NodeId = null;
        for (named.args, 0..) |arg, index| {
            const interned = try self.internImmediateChild(arg);
            if (interned_args) |args| {
                args[index] = interned;
            } else if (self.find(arg) != interned) {
                const args = try self.arena().alloc(NodeId, named.args.len);
                @memcpy(args[0..index], named.args[0..index]);
                args[index] = interned;
                interned_args = args;
            }
        }
        if (interned_args) |args| {
            var interned = named;
            interned.args = args;
            return interned;
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
            if (self.checked_base_nodes.items[@intFromEnum(root)] == (self.checked_base_construction_depth != 0) and
                candidate_content == .named and
                self.sameNamedIdentity(candidate_content.named, named)) return root;
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
        if (node_content == .named and self.checked_base_construction_depth == 0) {
            if (try self.internNamedArguments(node_content.named)) |interned| {
                return try self.newNode(.{ .named = interned });
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
            .list => |element| try self.registerListElement(element, id),
            .box => |element| try self.registerBoxElement(element, id),
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
        try self.function_argument_authority_spans.append(self.allocator, .uninitialized);
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
    pub fn completeReservedConstructionNode(
        self: *InstGraph,
        reserved: NodeId,
        raw_content: InstNode,
    ) Allocator.Error!void {
        const root = self.find(reserved);
        const existing = self.nodes.items[@intFromEnum(root)];
        if (existing != .unresolved or existing.unresolved.origin != .construction_placeholder) {
            Common.invariant("produced node reservation was completed more than once");
        }
        const completed_content = if (raw_content == .named and self.checked_base_construction_depth == 0)
            if (try self.internNamedArguments(raw_content.named)) |interned|
                InstNode{ .named = interned }
            else
                raw_content
        else
            raw_content;
        const interned: ?NodeId = switch (completed_content) {
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
        if (interned) |node| {
            try self.redirectRoot(node, root);
            return;
        }
        _ = try self.replaceRootContent(root, completed_content);
        switch (completed_content) {
            .primitive => |primitive| self.primitive_nodes[@intFromEnum(primitive)] = root,
            .empty_tag_union => self.empty_tag_union_node = root,
            .empty_record => self.empty_record_node = root,
            .zst => self.zst_node = root,
            .list => |element| try self.registerListElement(element, root),
            .box => |element| try self.registerBoxElement(element, root),
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
        const reserved = try self.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
        try self.setContent(reserved, try fill(context, reserved));
        return reserved;
    }

    /// Intern an ordinary nominal from its complete identity before evaluating
    /// its declaration backing. The declaration plus exact public arguments
    /// are the identity; the backing is deterministic implementation data.
    /// Recording the named node first makes recursive occurrences direct
    /// identity hits and means an ordinary hit performs no backing work.
    pub fn reserveOrdinaryNamedBacking(
        self: *InstGraph,
        raw_named: InstNamed,
        backing_use: Type.BackingUse,
    ) Allocator.Error!OrdinaryNamedReservation {
        self.requireRelationProduction();
        if (raw_named.backing != null) {
            Common.invariant("ordinary nominal identity reservation received an existing backing");
        }
        if (raw_named.def.generated != null) {
            Common.invariant("generated nominal reached ordinary identity reservation");
        }

        // Exact argument roots are already sufficient identity evidence. Most
        // requests repeat those roots, so consult the interner before doing
        // any row normalization. A miss may still need normalization because
        // two distinct row-extension decompositions can describe the same
        // public argument.
        if (self.existingNamedIdentity(raw_named)) |existing| {
            return .{ .existing = existing };
        }

        const interned = if (try self.internNamedArguments(raw_named)) |normalized| blk: {
            if (self.existingNamedIdentity(normalized)) |existing| {
                return .{ .existing = existing };
            }
            break :blk normalized;
        } else raw_named;

        const backing = try self.appendDistinctNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
        var completed = interned;
        completed.backing = .{
            .node = backing,
            .use = backing_use,
            .authority = .checked_public,
        };
        // `interned` has already missed the identity table. Record it
        // directly: routing this vacant reservation through `newNode` would
        // normalize and query the same identity a second time.
        const named = try self.appendDistinctNode(.{ .named = completed });
        try self.registerNamedIdentity(named, completed);
        return .{ .vacant = .{ .named = named, .backing = backing } };
    }

    /// Complete declaration metadata for one newly reserved ordinary nominal.
    /// Declared field order is deterministic implementation data rather than
    /// nominal identity, so callers produce it only after an identity miss.
    pub fn completeOrdinaryNamedDeclaredOrder(
        self: *InstGraph,
        raw_named: NodeId,
        declared_order: []const InstDeclaredField,
    ) Allocator.Error!void {
        const named_node = self.find(raw_named);
        const node_content = self.nodes.items[@intFromEnum(named_node)];
        if (node_content != .named or node_content.named.def.generated != null) {
            Common.invariant("ordinary nominal declared order completed a non-ordinary named node");
        }
        if (node_content.named.backing == null) {
            Common.invariant("ordinary nominal declared order completed a node without reserved backing");
        }
        if (node_content.named.declared_order.len != 0) {
            Common.invariant("ordinary nominal declared order was completed more than once");
        }
        var completed = node_content.named;
        completed.declared_order = declared_order;
        _ = try self.replaceRootContent(named_node, .{ .named = completed });
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

    /// Whether two nominal nodes name the same exact identity.
    /// A nominal's declaration and immediate public argument nodes are its
    /// identity; backing is deterministic implementation data owned by that
    /// identity. This comparison is used only when two checker-declared exact
    /// occurrences meet at one flat specialization slot, so it neither
    /// searches the graph nor scans an interner bucket.
    pub fn sameNominalIdentity(self: *InstGraph, left: NodeId, right: NodeId) bool {
        const left_content = self.content(left);
        if (left_content != .named) return false;
        const right_content = self.content(right);
        if (right_content != .named) return false;
        return self.sameNamedIdentity(left_content.named, right_content.named);
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

    /// Whether a live graph type is closed without applying any unresolved
    /// variable or row default. Open requests remain graph-local until
    /// explicit recursive-edge identity or final body sealing resolves them.
    pub fn typeIsResolved(self: *InstGraph, root: NodeId) Allocator.Error!bool {
        return try self.typeIsResolvedInner(root);
    }

    fn typeIsResolvedInner(self: *InstGraph, root: NodeId) Allocator.Error!bool {
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
                        if (field.kind == .undetermined and self.resolvedFieldKind(field.kind) == null) return false;
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

    /// Return an exact node for an explicit checker-recorded default on an identity slot.
    /// Call dependency planning uses this for pathless numeric and open-row
    /// identities before lowering a contextual consumer. Returns false when
    /// the node has no checked default; callers must then wait for another
    /// recorded producer edge.
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
        if (variable.specialization_default) |default| {
            return try self.newNode(switch (default) {
                .empty_tag_union => .empty_tag_union,
            });
        }
        return null;
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
        var node = self.find(raw_node);
        var remaining = self.nodes.items.len;
        while (remaining > 0) : (remaining -= 1) {
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
        Common.invariant("instantiation " ++ noun ++ " read encountered a recursive named backing");
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

    /// Project a primitive leaf through explicit inspectable backing edges.
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

    /// Read the exact payload span for a named tag. A missing result means the
    /// exact row does not contain that constructor.
    pub fn tagPayloadNodesOrNull(
        self: *InstGraph,
        node: NodeId,
        name: names.TagNameId,
    ) Allocator.Error!?[]const NodeId {
        const structural = try self.shapeRoot(node, "tag payloads", .inspectable);
        if (self.content(structural) == .empty_tag_union) return null;
        if (self.content(structural) != .tag_union) {
            Common.invariant("instantiation tag payload span read had a non-tag-union node");
        }
        const row = try self.flattenTagRow(structural);
        const wanted = self.tagLabelText(name);
        for (row.tags) |tag| {
            if (Ident.textEql(wanted, self.tagLabelText(tag.name))) return tag.payloads;
        }
        return null;
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

    /// Return the exact field value cell named by a checker-authored evidence
    /// path. Generalized and optional fields keep this separate from the
    /// runtime presence slot returned by `recordFieldNode`.
    pub fn evidenceRecordFieldValueNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldValueNodeWithAccess(raw_record, name, .inspectable, "evidence record field value");
    }

    /// Apply one checked required-access judgment to the field-kind cell and
    /// return the inline value slot selected by that judgment.
    pub fn requiredRecordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.requiredRecordFieldNodeWithAccess(raw_record, name, .inspectable, "required record field access");
    }

    /// Select the checked open-row remainder after the checker-recorded
    /// fields. Row extension topology is intentionally irrelevant: checker
    /// output names the fields owned by the enclosing row, and this edge
    /// returns exactly the remaining flattened row.
    pub fn recordRemainderNode(
        self: *InstGraph,
        raw_record: NodeId,
        excluded: []const names.RecordFieldNameId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_record, "record remainder", .inspectable);
        if (self.content(structural) == .empty_record) return structural;
        if (self.content(structural) != .record) {
            Common.invariant("instantiation record-remainder edge had a non-record node");
        }
        if (excluded.len == 0) return structural;

        const row = try self.flattenRecordRow(structural);
        var retained = std.ArrayList(InstField).empty;
        defer retained.deinit(self.allocator);
        try retained.ensureTotalCapacity(self.allocator, row.fields.len);
        for (row.fields) |field| {
            var remove = false;
            for (excluded) |name| {
                if (field.name != name) continue;
                remove = true;
                break;
            }
            if (!remove) retained.appendAssumeCapacity(field);
        }
        if (retained.items.len == 0) return self.find(row.ext);
        return try self.newProducedRecord(retained.items, row.ext);
    }

    /// Select the checked open-tag-row remainder after the checker-recorded
    /// tags. The returned node contains only tags not owned by the enclosing
    /// checked row and preserves the exact produced tail node.
    pub fn tagRemainderNode(
        self: *InstGraph,
        raw_union: NodeId,
        excluded: []const names.TagNameId,
    ) Allocator.Error!NodeId {
        const structural = try self.shapeRoot(raw_union, "tag remainder", .inspectable);
        if (self.content(structural) == .empty_tag_union) return structural;
        if (self.content(structural) != .tag_union) {
            Common.invariant("instantiation tag-remainder edge had a non-tag-union node");
        }
        if (excluded.len == 0) return structural;

        const row = try self.flattenTagRow(structural);
        var retained = std.ArrayList(InstTag).empty;
        defer retained.deinit(self.allocator);
        try retained.ensureTotalCapacity(self.allocator, row.tags.len);
        for (row.tags) |tag| {
            var remove = false;
            for (excluded) |name| {
                if (tag.name != name) continue;
                remove = true;
                break;
            }
            if (!remove) retained.appendAssumeCapacity(tag);
        }
        if (retained.items.len == 0) return self.find(row.ext);
        return try self.newProducedTagUnion(retained.items, row.ext);
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
        return self.recordFieldValueNodeWithAccess(raw_record, name, .runtime_layout, "record constructor");
    }

    /// Return the exact field representation already carried by the requested
    /// record. Live checked fields retain their specialization evidence;
    /// completed producer fields have consumed that evidence into the stable
    /// `value_ty`/`default` representation stored on a sealed field.
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
                    // The occurrence's private slot is completed directionally
                    // from its exact value when field-kind relations freeze.
                    self.constrainUndeterminedFieldKind(id, .required);
                    return .required;
                },
                .sealed => return if (field.default) |default|
                    .{ .defaulted = default }
                else if (field.value_ty != null)
                    .optional
                else
                    .required,
                .required, .optional, .defaulted => unreachable,
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

    fn recordFieldValueNodeWithAccess(
        self: *InstGraph,
        raw_record: NodeId,
        name: names.RecordFieldNameId,
        access: BackingAccess,
        comptime noun: []const u8,
    ) Allocator.Error!NodeId {
        const field = try self.recordFieldWithAccess(raw_record, name, access, noun);
        return self.find(field.value_ty orelse field.ty);
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
        return self.find(field.value_ty orelse field.ty);
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

    /// Redirect `loser` into `winner`, moving row back references.
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
        try self.redirectRoot(winner, loser);
    }

    /// Redirect one reserved or related root while preserving every exact row
    /// back-reference to its interned target.
    fn redirectRoot(
        self: *InstGraph,
        winner: NodeId,
        loser: NodeId,
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
    /// form. Returns whether the stored graph content changed.
    fn replaceRootContent(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!bool {
        const root = self.find(raw_root);
        if (instNodeEql(self.nodes.items[@intFromEnum(root)], new_content)) return false;
        self.nodes.items[@intFromEnum(root)] = new_content;
        self.versions.items[@intFromEnum(root)] +%= 1;
        try self.registerRowParent(root, new_content);
        return true;
    }

    /// Replace a root's type content.
    fn setContent(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!void {
        const root = self.find(raw_root);
        if (self.checked_base_construction_depth == 0 and
            self.checked_base_nodes.items[@intFromEnum(root)])
        {
            Common.invariant("exact lowering attempted to rewrite an immutable checked base node");
        }
        if (new_content == .named) {
            if (try self.internNamedArguments(new_content.named)) |interned| {
                return try self.setContent(root, .{ .named = interned });
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
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
            self.primitive_nodes[@intFromEnum(primitive)] = root;
        }
        if (new_content == .empty_tag_union) {
            if (self.empty_tag_union_node) |raw_existing| {
                const existing = self.find(raw_existing);
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
            self.empty_tag_union_node = root;
        }
        if (new_content == .empty_record) {
            if (self.empty_record_node) |raw_existing| {
                const existing = self.find(raw_existing);
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
            self.empty_record_node = root;
        }
        if (new_content == .zst) {
            if (self.zst_node) |raw_existing| {
                const existing = self.find(raw_existing);
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
            self.zst_node = root;
        }
        if (new_content == .list) {
            if (self.existingListElement(new_content.list)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (new_content == .box) {
            if (self.existingBoxElement(new_content.box)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (new_content == .tuple) {
            if (self.existingTupleShape(new_content.tuple)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (new_content == .record) {
            if (self.existingRecordShape(new_content)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (new_content == .tag_union) {
            if (self.existingTagUnionShape(new_content.tag_union)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (new_content == .named) {
            const named = new_content.named;
            if (self.existingNamedIdentity(named)) |existing| {
                if (existing != root) {
                    if (!try self.replaceRootContent(root, .{ .redirect = existing })) return;
                    return;
                }
            }
        }
        if (!try self.replaceRootContent(root, new_content)) return;
        switch (new_content) {
            .list => |element| try self.registerListElement(element, root),
            .box => |element| try self.registerBoxElement(element, root),
            .tuple => |items| try self.registerTupleShape(root, items),
            .record => try self.registerRecordShape(root, new_content),
            .tag_union => |tag_union| try self.registerTagUnionShape(root, tag_union),
            .named => |named| try self.registerNamedIdentity(root, named),
            .redirect, .unresolved, .primitive, .func, .empty_tag_union, .empty_record, .erased, .zst => {},
        }
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
            .specialization_default = a.specialization_default orelse b.specialization_default,
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
        if (a == b) return a;
        if (a == .checked_variable or b == .checked_variable) return .checked_variable;
        if (a == .row_extension or b == .row_extension) return .row_extension;
        Common.invariant("exact lowering attempted to merge construction and producer placeholders");
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
                            std.meta.eql(left_named.def, right_named.def))
                        {
                            try self.union_(left, right);
                            return;
                        }
                        Common.invariant("distinct generated nominal declarations reached ordinary unification");
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
                                } else Common.invariant("instantiation named backing authorities were incompatible");
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
        if (isGeneratedPrivateRootContent(named_content)) {
            Common.invariant("generated nominal declaration reached structural backing unification");
        }
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
    /// their exact immediate children; flattening is therefore an exact row
    /// read, never a graph mutation.
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

        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
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
                const merged_kind = self.unifyFieldKinds(
                    left_field.kind,
                    left_field.default,
                    right_field.kind,
                    right_field.default,
                );
                try pending.append(self.allocator, .{
                    .left = left_field.value_ty orelse left_field.ty,
                    .right = right_field.value_ty orelse right_field.ty,
                });
                try pending.append(self.allocator, .{ .left = left_field.ty, .right = right_field.ty });
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
        if (self.imported_type_nodes.get(ty)) |existing| {
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
                try self.imported_type_nodes.put(ty, node);
                self.countDiagnostic("mono_import_hits");
                return node;
            }
        }
        self.countDiagnostic("mono_import_misses");
        const node = try self.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
        // One-way memo: every import is a finished Monotype from outside this
        // graph. This specialization copies its exact structure into graph
        // nodes and never exposes those mutable nodes as a TypeId view.
        try self.imported_type_nodes.put(ty, node);
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
                    const value_ty = if (field.value_ty) |value_ty|
                        try self.importMono(value_ty)
                    else
                        null;
                    inst_fields[index] = .{
                        .name = field.name,
                        .ty = try self.importMonoInner(field.ty),
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
        if (imported == .func) {
            _ = try self.internCompletedFunction(node);
        }
        return self.find(node);
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

    /// Materialize a frozen graph node directly into a final TypeId.
    pub fn sealNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        var sealer = GraphTypeFinals.init(self);
        defer sealer.deinit();
        return try sealer.sealNode(node);
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
};

/// Shared finalization state for materializing graph nodes into immutable
/// Monotype type ids.
pub const GraphTypeFinals = struct {
    graph: *InstGraph,
    sealed: collections.DenseMap(NodeId, Type.TypeId),
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

    fn initUnchecked(graph: *InstGraph) GraphTypeFinals {
        return .{
            .graph = graph,
            .sealed = collections.DenseMap(NodeId, Type.TypeId).init(graph.allocator),
            .generated_types_by_identity = null,
        };
    }

    pub fn deinit(self: *GraphTypeFinals) void {
        self.sealed.deinit();
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

    fn sealNodeSpan(self: *GraphTypeFinals, nodes: []const NodeId) Allocator.Error!Type.Span {
        if (nodes.len == 0) return .empty();
        const sealed_nodes = try self.graph.allocator.alloc(Type.TypeId, nodes.len);
        defer self.graph.allocator.free(sealed_nodes);
        for (nodes, 0..) |node, index| {
            sealed_nodes[index] = try self.sealNode(node);
        }
        return try self.graph.types.addSpan(sealed_nodes);
    }

    fn sealRecordRow(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.Span {
        const flat = try self.graph.flattenRecordRow(node);
        if (flat.fields.len == 0) return .empty();
        const fields = try self.graph.allocator.alloc(Type.Field, flat.fields.len);
        defer self.graph.allocator.free(fields);
        for (flat.fields, 0..) |field, index| {
            if (field.kind == .undetermined and self.graph.resolvedFieldKind(field.kind) == null) {
                Common.invariant("unresolved record field kind reached Monotype sealing");
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
                .default = field.default,
            };
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
};

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
            } else if (content.unresolved.specialization_default) |default| switch (default) {
                .empty_tag_union => .empty_tag_union,
            } else switch (content.unresolved.origin) {
                .checked_variable => Common.invariant("generated identity input contained a checked variable without an explicit default"),
                .row_extension => Common.invariant("generated identity input contained a row extension without its checked default"),
                .construction_placeholder => Common.invariant("generated identity input contained an incomplete construction placeholder"),
                .producer_placeholder => Common.invariant("generated identity input contained an incomplete producer placeholder"),
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
    if (variable.specialization_default) |default| switch (default) {
        .empty_tag_union => return .{ .tag_union = Type.Span.empty() },
    };
    return switch (variable.origin) {
        .checked_variable => Common.invariant("checked variable reached Monotype materialization without an explicit default"),
        .row_extension => Common.invariant("row extension reached Monotype materialization without row default"),
        .construction_placeholder => Common.invariant("instantiation construction placeholder reached Monotype materialization"),
        .producer_placeholder => Common.invariant("instantiation producer placeholder reached Monotype materialization"),
    };
}

fn instTagLessThan(name_store: *const names.NameStore, lhs: InstTag, rhs: InstTag) bool {
    return name_store.tagLabelTextLessThan(lhs.name, rhs.name);
}

fn instFieldLessThan(name_store: *const names.NameStore, lhs: InstField, rhs: InstField) bool {
    return name_store.recordFieldLabelTextLessThan(lhs.name, rhs.name);
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

fn updateFieldDefaultHash(hasher: *std.hash.Wyhash, default: Type.FieldDefault) void {
    var module = std.mem.nativeToLittle(u32, @intFromEnum(default.module));
    hasher.update(std.mem.asBytes(&module));
    var expr_node = std.mem.nativeToLittle(u32, default.expr_node);
    hasher.update(std.mem.asBytes(&expr_node));
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

    _ = try graph.newNode(.{ .primitive = .bool });
    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const str = try graph.newNode(.{ .primitive = .str });
    try graph.unify(unresolved, str);
    try graph.freezeRelations();
    try std.testing.expect(!graph.finalizesAsClosedEmptyTagUnion(str));
    try std.testing.expect(!graph.finalizesAsClosedEmptyTagUnion(unresolved));

    try std.testing.expectEqual(@as(u64, 3), diagnostics.nodes_created);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.unify_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.class_unions);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.closed_empty_finalization_requests);
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

test "function request identity distinguishes request seeds from produced edges" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const arg = try graph.newNode(.{ .primitive = .u64 });
    const ret = try graph.newNode(.{ .primitive = .bool });
    const left = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{arg}), .ret = ret } });
    const right = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{arg}), .ret = ret } });
    graph.registerFunctionResultRelation(left, .produced);
    graph.registerFunctionResultRelation(right, .produced);
    const key = CheckedBaseKey{
        .module_bytes = [_]u8{0xA5} ** 32,
        .checked = testCheckedTypeId(7),
    };
    try graph.recordDirectRequestSelections(left, &.{.{
        .base = key,
        .produced = ret,
        .authority = .request,
    }});
    try graph.recordDirectRequestSelections(right, &.{.{
        .base = key,
        .produced = ret,
        .authority = .produced,
    }});

    try std.testing.expect(!graph.sameDirectRequestSelections(left, right));
    try std.testing.expect(!try graph.sameFunctionRequestInputs(left, right));
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
    const left_ret = try graph.newNode(.{ .unresolved = InstVariable.producerPlaceholder() });
    const right_ret = try graph.newNode(.{ .unresolved = InstVariable.producerPlaceholder() });
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

    const row = try graph.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
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

test "undetermined record field freezes to its required inline representation" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("value");
    const value_ty = try graph.newNode(.{ .primitive = .u64 });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
    const kind = try graph.newUndeterminedFieldKind();
    try graph.registerUndeterminedFieldKindCells(kind, slot, value_ty);
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

    try std.testing.expect(graph.resolvedFieldKind(fields[0].kind) == null);
    try graph.freezeRelations();
    try std.testing.expectEqual(ResolvedFieldKind.required, graph.resolvedFieldKind(fields[0].kind).?);

    const sealed = try graph.sealNode(record);
    const sealed_fields = type_store.fieldSpan(type_store.get(sealed).record);
    try std.testing.expectEqual(@as(usize, 1), sealed_fields.len);
    const sealed_field = GuardedList.at(sealed_fields, 0);
    try std.testing.expect(sealed_field.value_ty == null);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(sealed_field.ty));
}

test "record interning preserves field specialization evidence" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("value");
    const value_ty = try graph.newNode(.{ .primitive = .u64 });
    const empty_record = try graph.newNode(.empty_record);

    const required_fields = try graph.arena().alloc(InstField, 1);
    required_fields[0] = .{
        .name = field_name,
        .ty = value_ty,
        .kind = .required,
        .default = null,
    };
    const required_record = try graph.newNode(.{ .record = .{
        .fields = required_fields,
        .ext = empty_record,
    } });

    const sealed_fields = try graph.arena().alloc(InstField, 1);
    sealed_fields[0] = .{
        .name = field_name,
        .ty = value_ty,
        .kind = .sealed,
        .default = null,
    };
    const sealed_record = try graph.newNode(.{ .record = .{
        .fields = sealed_fields,
        .ext = empty_record,
    } });

    try std.testing.expect(!graph.sameClass(required_record, sealed_record));
    try std.testing.expectEqual(ResolvedFieldKind.required, try graph.recordConstructionFieldKind(required_record, field_name));
    try std.testing.expectEqual(ResolvedFieldKind.required, try graph.recordConstructionFieldKind(sealed_record, field_name));
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

    try graph.freezeRelations();
    const alias_ty = try graph.sealNode(alias);
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
    try std.testing.expect(!graph.sameClass(nominal, backing));

    const named = graph.content(nominal).named;
    const retained_backing = named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(!graph.sameClass(nominal, retained_backing.node));
    try std.testing.expectEqual(InstNode{ .primitive = .u64 }, graph.content(retained_backing.node));
    try graph.freezeRelations();
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

test "producer root interning rekeys an immediate nominal child after argument selection" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const left_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const right_arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAF} ** 32));
    const type_name = try name_store.internTypeName("Recursive");
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };

    const left = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(5) },
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{left_arg}),
        .backing = null,
    } });
    const right = try graph.newNode(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(6) },
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{right_arg}),
        .backing = null,
    } });
    const left_box = try graph.newProducedBox(left);
    const right_box = try graph.newProducedBox(right);
    try std.testing.expect(!graph.sameClass(left_box, right_box));

    try graph.unify(left_arg, right_arg);
    try std.testing.expect(graph.sameNominalIdentity(left, right));
    try std.testing.expectEqual(
        try graph.internProducedIdentity(left_box),
        try graph.internProducedIdentity(right_box),
    );
}

test "final sealed graph node materializes directly" {
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
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);
}

test "imported recursive type preserves its exact immutable representation" {
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
    try std.testing.expectEqual(imported, try graph.importMono(exact));
    try graph.freezeRelations();
    const sealed = try graph.sealNode(imported);
    const tags = type_store.tagSpan(type_store.get(sealed).tag_union);
    try std.testing.expectEqual(@as(usize, 1), tags.len);
    const payloads = type_store.span(GuardedList.at(tags, 0).payloads);
    try std.testing.expectEqual(@as(usize, 1), payloads.len);
    try std.testing.expectEqual(sealed, GuardedList.at(payloads, 0));
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

test "unconstrained specialization default closes only without exact selection" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const defaulted = try graph.newNode(.{ .unresolved = InstVariable.checkedVariableAtKey(
        null,
        null,
        .empty_tag_union,
        [_]u8{0} ** 32,
    ) });
    const selected = try graph.newNode(.{ .unresolved = InstVariable.checkedVariableAtKey(
        null,
        null,
        .empty_tag_union,
        [_]u8{1} ** 32,
    ) });
    const exact = try graph.newNode(.{ .primitive = .str });
    try graph.completeProducedSelection(selected, exact);

    try graph.freezeRelations();
    const sealed_defaulted = try graph.sealNode(defaulted);
    const sealed_selected = try graph.sealNode(selected);

    try std.testing.expectEqual(Type.Span.empty(), type_store.get(sealed_defaulted).tag_union);
    try std.testing.expectEqual(Type.Primitive.str, type_store.get(sealed_selected).primitive);
}

test "relation mutation remains available only before freezing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const resolved = try graph.newNode(.{ .primitive = .u64 });
    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    try graph.unify(resolved, unresolved);

    try std.testing.expect(graph.acceptsRelationMutation());
    try std.testing.expect(graph.sameClass(resolved, unresolved));

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

test "imported singleton types share ordinary exact graph nodes" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const empty_record = try graph.newNode(.empty_record);
    const empty_record_ty = try type_store.add(.{ .record = Type.Span.empty() });
    try std.testing.expectEqual(empty_record, try graph.importMono(empty_record_ty));

    const empty_tag_union = try graph.newNode(.empty_tag_union);
    const empty_tag_union_ty = try type_store.add(.{ .tag_union = Type.Span.empty() });
    try std.testing.expectEqual(empty_tag_union, try graph.importMono(empty_tag_union_ty));

    const zst = try graph.newNode(.zst);
    const zst_ty = try type_store.add(.zst);
    try std.testing.expectEqual(zst, try graph.importMono(zst_ty));
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

test "recursive generated iterator reservation completes at its producer boundary" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const item_cell = try graph.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
    const exact_item = try graph.newNode(.{ .primitive = .u64 });
    const backing = try graph.newNode(.{ .primitive = .u8 });
    const def: Type.TypeDef = .{
        .module = try name_store.internModuleIdentity(&([_]u8{0xD3} ** 32)),
        .type_name = try name_store.internTypeName("Iter"),
        .source_decl = 17,
    };
    const Context = struct {
        graph: *InstGraph,
        item: NodeId,
        backing: NodeId,
        def: Type.TypeDef,

        fn fill(ctx: @This(), self_node: NodeId) Allocator.Error!InstNode {
            _ = self_node;
            return .{ .named = .{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(17) },
                .def = ctx.def,
                .kind = .nominal,
                .builtin_owner = null,
                .args = try ctx.graph.arena().dupe(NodeId, &.{ctx.item}),
                .backing = .{
                    .node = ctx.backing,
                    .use = .inspectable,
                    .authority = .generated_private,
                },
            } };
        }
    };
    const reservation = try graph.reserveRecursiveGeneratedIterator(
        def,
        item_cell,
        Context{
            .graph = graph,
            .item = item_cell,
            .backing = backing,
            .def = def,
        },
        Context.fill,
    );
    try std.testing.expectEqual(@as(usize, 1), graph.recursive_generated_iterator_item_keys.count());
    try std.testing.expect(graph.generated_iterators_by_item.contains(item_cell));

    try graph.completeProducedSelection(item_cell, exact_item);
    const completed = try graph.completeRecursiveGeneratedIterator(reservation, def, exact_item);
    try std.testing.expect(graph.sameClass(reservation, completed));
    try std.testing.expect(graph.content(completed).named.def.generated != null);
    try std.testing.expectEqual(@as(usize, 0), graph.recursive_generated_iterator_item_keys.count());
    try std.testing.expect(!graph.generated_iterators_by_item.contains(item_cell));
    try std.testing.expect(graph.generated_iterators_by_item.contains(exact_item));

    const lookup = try graph.lookupGeneratedIterator(def, exact_item);
    try std.testing.expectEqual(completed, lookup.existing.?);
    try graph.freezeRelations();
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

test "ordinary nominal reservation checks exact roots before normalizing row decompositions" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const empty = try graph.newNode(.empty_record);
    const label = try name_store.internRecordFieldLabel("value");
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{
        .name = label,
        .ty = try graph.newNode(.{ .primitive = .u64 }),
        .default = null,
    };
    const flat_argument = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = empty,
    } });
    const extended_argument = try graph.newNode(.{ .record = .{
        .fields = &.{},
        .ext = flat_argument,
    } });

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xD2} ** 32));
    const type_name = try name_store.internTypeName("Wrapper");
    var exact_args = [_]NodeId{flat_argument};
    var identity: InstNamed = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(16) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .builtin_owner = null,
        .args = &exact_args,
        .backing = null,
    };

    const first = try graph.reserveOrdinaryNamedBacking(identity, .inspectable);
    const named = switch (first) {
        .existing => return error.TestUnexpectedResult,
        .vacant => |reservation| reservation.named,
    };
    const declared_order = [_]InstDeclaredField{.{ .named = label }};
    try graph.completeOrdinaryNamedDeclaredOrder(named, &declared_order);
    try std.testing.expectEqualSlices(
        InstDeclaredField,
        &declared_order,
        graph.content(named).named.declared_order,
    );
    const before_exact_hit = graph.nodes.items.len;
    const exact_hit = try graph.reserveOrdinaryNamedBacking(identity, .inspectable);
    try std.testing.expectEqual(named, switch (exact_hit) {
        .existing => |existing| existing,
        .vacant => return error.TestUnexpectedResult,
    });
    try std.testing.expectEqual(before_exact_hit, graph.nodes.items.len);

    var extended_args = [_]NodeId{extended_argument};
    identity.args = &extended_args;
    const normalized_hit = try graph.reserveOrdinaryNamedBacking(identity, .inspectable);
    try std.testing.expectEqual(named, switch (normalized_hit) {
        .existing => |existing| existing,
        .vacant => return error.TestUnexpectedResult,
    });
}

test "produced records erase checked row topology at their construction boundary" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const empty = try graph.newNode(.empty_record);
    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");
    const a_node = try graph.newNode(.{ .primitive = .u8 });
    const b_node = try graph.newNode(.{ .primitive = .u16 });
    const tail = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = a_name,
            .ty = a_node,
            .default = null,
        }}),
        .ext = empty,
    } });

    const from_extension = try graph.newProducedRecord(&.{.{
        .name = b_name,
        .ty = b_node,
        .default = null,
    }}, tail);
    const from_flat_fields = try graph.newProducedRecord(&.{
        .{ .name = b_name, .ty = b_node, .default = null },
        .{ .name = a_name, .ty = a_node, .default = null },
    }, empty);

    try std.testing.expectEqual(from_extension, from_flat_fields);
    const produced = graph.content(from_extension).record;
    try std.testing.expectEqual(empty, graph.rootNode(produced.ext));
    try std.testing.expectEqual(@as(usize, 2), produced.fields.len);
    try std.testing.expectEqual(a_name, produced.fields[0].name);
    try std.testing.expectEqual(a_node, graph.rootNode(produced.fields[0].ty));
    try std.testing.expectEqual(b_name, produced.fields[1].name);
    try std.testing.expectEqual(b_node, graph.rootNode(produced.fields[1].ty));
}

test "exact producers convert a concrete checked recipe bottom-up once" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    graph.beginCheckedBaseConstruction();
    const item = try graph.newNode(.{ .primitive = .u8 });
    graph.markCheckedBase(item);
    const checked_list = try graph.newNode(.{ .list = item });
    graph.markCheckedBase(checked_list);
    const checked_default = try graph.newNode(.{ .unresolved = InstVariable.checkedVariableAtKey(
        null,
        null,
        .empty_tag_union,
        [_]u8{0} ** 32,
    ) });
    graph.markCheckedBase(checked_default);
    const checked_tuple_items = try graph.arena().dupe(NodeId, &.{ checked_list, checked_default });
    const checked_tuple = try graph.newNode(.{ .tuple = checked_tuple_items });
    graph.markCheckedBase(checked_tuple);
    graph.endCheckedBaseConstruction();

    const produced_box = try graph.newProducedBox(checked_tuple);
    const produced_tuple = graph.content(produced_box).box;
    const produced_list = graph.content(produced_tuple).tuple[0];
    const produced_default = graph.content(produced_tuple).tuple[1];

    try std.testing.expect(!graph.nodeIsCheckedBase(produced_box));
    try std.testing.expect(!graph.nodeIsCheckedBase(produced_tuple));
    try std.testing.expect(!graph.nodeIsCheckedBase(produced_list));
    try std.testing.expectEqual(item, graph.content(produced_list).list);
    try std.testing.expect(!graph.nodeIsCheckedBase(produced_default));
    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(produced_default));

    const node_count = graph.nodes.items.len;
    try std.testing.expectEqual(produced_box, try graph.newProducedBox(checked_tuple));
    try std.testing.expectEqual(node_count, graph.nodes.items.len);
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
    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
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

    const nominal = try graph.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
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

    const nominal = try graph.newNode(.{ .unresolved = InstVariable.constructionPlaceholder() });
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

test "permanently inhabited graph roots memoize only stable negative answers" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics = GraphDiagnostics{};
    graph.setDiagnostics(&diagnostics);

    const inhabited = try graph.newNode(.{ .primitive = .u64 });
    try std.testing.expect(!try graph.mayFinalizeAsUninhabited(inhabited));
    try std.testing.expect(!try graph.mayFinalizeAsUninhabited(inhabited));
    try std.testing.expectEqual(@as(u64, 2), diagnostics.permanent_inhabitedness_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.permanent_inhabitedness_hits);

    const unresolved = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    try std.testing.expect(try graph.mayFinalizeAsUninhabited(unresolved));
    try std.testing.expect(try graph.mayFinalizeAsUninhabited(unresolved));
    try std.testing.expectEqual(@as(u64, 4), diagnostics.permanent_inhabitedness_requests);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.permanent_inhabitedness_hits);
}
