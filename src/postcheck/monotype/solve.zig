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
const TypeDigestHasher = @import("base").TypeDigestHasher;
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
    generated_iterator: ?*const InstGeneratedIterator = null,
    /// Declared fields for a nominal/opaque record backing (empty otherwise).
    /// Padding field types are graph nodes so sealing maps them to immutable
    /// type ids with the rest of the named type.
    declared_order: []const InstDeclaredField = &.{},
};

/// Union-find node for nominal applications that an explicit relation proved
/// equivalent without joining their representation-owning type classes.
const RelatedNamedInstance = struct {
    parent: NodeId,
    rank: u8 = 0,
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
        /// Unique head tags; row readers establish lexicographic order once.
        tags: []InstTag,
        ext: NodeId,
        /// Representation-only state, not part of type equality. A new head
        /// starts unordered unless its producer already normalized the span.
        tags_sorted: bool = false,
    },
    record: struct {
        fields: []InstField,
        ext: NodeId,
    },
    empty_tag_union,
    empty_record,
    /// Stored out of line: a named payload is an order of magnitude larger
    /// than every other variant, and most nodes are not named.
    named: *const InstNamed,
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

/// Immutable constraints reachable from an interface's explicit input roots.
/// Local indices preserve variable sharing and cycles; settled Monotypes remain
/// interned leaves. This representation never applies unresolved defaults.
pub const InterfaceConstraints = struct {
    roots: []const NodeId,
    nodes: []const Node,
    open_nodes: []const OpenNode,
    kinds: []const Kind,

    // Settled leaves need only their interned identity. Keep the larger open
    // structure and producer evidence in a separate, densely indexed array.
    pub const Node = union(enum) { mono: Type.TypeId, open: u32 };
    pub const OpenNode = struct {
        content: InstNode,
        source: ?NodeId = null,
        recursive_slot: bool = false,
        forced_dynamic: bool = false,
        constructor_evidence: bool = false,
        related_group: ?u32 = null,
        private_backing: bool = false,
        finished: ?Type.TypeId = null,
    };
    pub const Kind = struct {
        resolved: ?ResolvedFieldKind,
        cells: ?FieldKindCells,
    };

    /// Maps and lists one capture fills. A graph keeps one set and every
    /// capture borrows it, so repeated captures keep their allocated chunks
    /// instead of allocating and zeroing them again; each capture leaves them
    /// empty.
    pub const CaptureScratch = struct {
        node_ids: collections.DenseMap(NodeId, NodeId),
        kind_ids: collections.DenseMap(FieldKindId, FieldKindId),
        shareable: collections.DenseMap(NodeId, bool),
        share_seen: collections.DenseMap(NodeId, void),
        related_ids: std.AutoHashMap(Capture.RelatedKey, u32),
        nodes: std.ArrayList(Node) = .empty,
        open_nodes: std.ArrayList(OpenNode) = .empty,
        kinds: std.ArrayList(Kind) = .empty,

        pub fn init(allocator: Allocator) CaptureScratch {
            return .{
                .node_ids = collections.DenseMap(NodeId, NodeId).init(allocator),
                .kind_ids = collections.DenseMap(FieldKindId, FieldKindId).init(allocator),
                .shareable = collections.DenseMap(NodeId, bool).init(allocator),
                .share_seen = collections.DenseMap(NodeId, void).init(allocator),
                .related_ids = std.AutoHashMap(Capture.RelatedKey, u32).init(allocator),
            };
        }

        pub fn deinit(self: *CaptureScratch, allocator: Allocator) void {
            self.node_ids.deinit();
            self.kind_ids.deinit();
            self.shareable.deinit();
            self.share_seen.deinit();
            self.related_ids.deinit();
            self.nodes.deinit(allocator);
            self.open_nodes.deinit(allocator);
            self.kinds.deinit(allocator);
        }
    };

    pub fn capture(graph: *InstGraph, allocator: Allocator, roots: []const NodeId) Allocator.Error!InterfaceConstraints {
        return try captureWithHoles(graph, allocator, roots, &.{}, &.{});
    }

    /// Capture `roots` with each representation-neutral class in `holes`
    /// recorded as an unconstrained variable. `hole_classes[i]` receives the
    /// class a hole stood for, or null when `holes[i]` carries representation
    /// authority and was captured as itself. Structure reaching a hole stays
    /// open rather than settling.
    pub fn captureWithHoles(
        graph: *InstGraph,
        allocator: Allocator,
        roots: []const NodeId,
        holes: []const NodeId,
        hole_classes: []?NodeId,
    ) Allocator.Error!InterfaceConstraints {
        std.debug.assert(holes.len == hole_classes.len);
        @memset(hole_classes, null);
        const hole_roots = try allocator.alloc(NodeId, holes.len);
        for (holes, hole_roots) |hole, *root| root.* = graph.find(hole);
        var retained = GraphTypeFinals.initRetainedTypeView(graph);
        defer retained.deinit();
        var settled = GraphTypeFinals.initSettledInterface(graph);
        defer settled.deinit();
        const scratch = &graph.capture_scratch;
        var builder = Capture{
            .settled = &settled,
            .retained = &retained,
            .graph = graph,
            .allocator = allocator,
            .holes = hole_roots,
            .hole_classes = hole_classes,
            .node_ids = scratch.node_ids,
            .kind_ids = scratch.kind_ids,
            .shareable = scratch.shareable,
            .share_seen = scratch.share_seen,
            .related_ids = scratch.related_ids,
            .nodes = scratch.nodes,
            .open_nodes = scratch.open_nodes,
            .kinds = scratch.kinds,
        };
        defer {
            builder.node_ids.clearRetainingCapacity();
            builder.kind_ids.clearRetainingCapacity();
            builder.shareable.clearRetainingCapacity();
            builder.share_seen.clearRetainingCapacity();
            builder.related_ids.clearRetainingCapacity();
            builder.nodes.clearRetainingCapacity();
            builder.open_nodes.clearRetainingCapacity();
            builder.kinds.clearRetainingCapacity();
            scratch.* = .{
                .node_ids = builder.node_ids,
                .kind_ids = builder.kind_ids,
                .shareable = builder.shareable,
                .share_seen = builder.share_seen,
                .related_ids = builder.related_ids,
                .nodes = builder.nodes,
                .open_nodes = builder.open_nodes,
                .kinds = builder.kinds,
            };
        }
        const captured_roots = try mapValue(&builder, []const NodeId, roots);
        return .{
            .roots = captured_roots,
            .nodes = try allocator.dupe(Node, builder.nodes.items),
            .open_nodes = try allocator.dupe(OpenNode, builder.open_nodes.items),
            .kinds = try allocator.dupe(Kind, builder.kinds.items),
        };
    }

    pub fn instantiate(self: InterfaceConstraints, graph: *InstGraph) Allocator.Error![]const NodeId {
        const node_ids = try graph.allocator.alloc(NodeId, self.nodes.len);
        defer graph.allocator.free(node_ids);
        const kind_ids = try graph.allocator.alloc(FieldKindId, self.kinds.len);
        defer graph.allocator.free(kind_ids);
        var instance = Instance{
            .graph = graph,
            .allocator = graph.arena(),
            .nodes = node_ids,
            .kinds = kind_ids,
        };
        for (self.nodes, instance.nodes) |node, *id| {
            id.* = switch (node) {
                .mono => |ty| try graph.importMono(ty),
                .open => try graph.newNode(.{ .unresolved = InstVariable.placeholder() }),
            };
        }
        for (instance.kinds) |*id| id.* = try graph.newUndeterminedFieldKind();
        for (self.kinds, instance.kinds) |kind, id| {
            const mapped = try mapValue(&instance, Kind, kind);
            graph.field_kinds.items[@intFromEnum(id)].resolved = mapped.resolved;
            graph.field_kinds.items[@intFromEnum(id)].cells = mapped.cells;
        }
        for (self.nodes, instance.nodes) |reference, id| {
            const node = switch (reference) {
                .mono => continue,
                .open => |index| self.open_nodes[index],
            };
            _ = try graph.replaceContentWithoutSnapshotInvalidation(id, try mapValue(&instance, InstNode, node.content));
            if (node.finished) |ty| try graph.recordImportedMono(id, ty);
            graph.private_backing_roots.items[@intFromEnum(id)] = graph.private_backing_roots.items[@intFromEnum(id)] or node.private_backing;
            if (node.recursive_slot) graph.markRecursiveValueSlot(id);
            if (node.forced_dynamic) graph.markForcedDynamicIteratorRoot(id);
            if (node.constructor_evidence) graph.registerConstructorEvidenceRequest(id);
            if (node.source) |source| graph.request_source_interfaces.items[@intFromEnum(id)] = instance.nodes[@intFromEnum(source)];
        }
        var related = collections.DenseMap(u32, NodeId).init(graph.allocator);
        defer related.deinit();
        for (self.nodes, instance.nodes) |reference, id| {
            const node = switch (reference) {
                .mono => continue,
                .open => |index| self.open_nodes[index],
            };
            const group = node.related_group orelse continue;
            const entry = try related.getOrPut(group);
            if (entry.found_existing) {
                try graph.relateNamedInstances(entry.value_ptr.*, id);
            } else {
                entry.value_ptr.* = id;
                try graph.ensureRelatedNamedInstanceNode(id);
                try graph.bindRelatedNamedBacking(id, graph.content(id).named);
            }
        }
        return try mapValue(&instance, []const NodeId, self.roots);
    }

    /// Copy into another cache's arena, relocating the interned leaves and
    /// interned names through its explicit store-domain mapping.
    pub fn copy(self: InterfaceConstraints, allocator: Allocator, context: anytype) Allocator.Error!InterfaceConstraints {
        var copier = Copier(@TypeOf(context)){ .allocator = allocator, .context = context };
        return try mapValue(&copier, InterfaceConstraints, self);
    }

    pub const Identity = struct {
        bytes: []const u8,
        leaves: []const Type.TypeId,

        pub fn eql(self: Identity, other: Identity, types: *Type.Store, name_store: *const names.NameStore) Allocator.Error!bool {
            if (!std.mem.eql(u8, self.bytes, other.bytes) or self.leaves.len != other.leaves.len) return false;
            for (self.leaves, other.leaves) |left, right| {
                if (!try types.typeEql(name_store, left, right)) return false;
            }
            return true;
        }

        pub fn copy(self: Identity, allocator: Allocator, context: anytype) Allocator.Error!Identity {
            var copier = Copier(@TypeOf(context)){ .allocator = allocator, .context = context };
            return try mapValue(&copier, Identity, self);
        }
    };

    pub fn identity(self: InterfaceConstraints, graph: *InstGraph) Allocator.Error!Identity {
        return self.identityInto(graph, graph.arena());
    }

    pub fn identityInto(self: InterfaceConstraints, graph: *InstGraph, allocator: Allocator) Allocator.Error!Identity {
        var writer = IdentityWriter{ .graph = graph };
        defer writer.bytes.deinit(graph.allocator);
        defer writer.leaves.deinit(graph.allocator);
        try writer.write(InterfaceConstraints, self);
        const bytes = try allocator.dupe(u8, writer.bytes.items);
        return .{ .bytes = bytes, .leaves = try allocator.dupe(Type.TypeId, writer.leaves.items) };
    }

    const IdentityWriter = struct {
        graph: *InstGraph,
        bytes: std.ArrayList(u8) = .empty,
        leaves: std.ArrayList(Type.TypeId) = .empty,

        fn raw(self: *IdentityWriter, bytes: []const u8) Allocator.Error!void {
            try self.bytes.appendSlice(self.graph.allocator, bytes);
        }
        fn text(self: *IdentityWriter, bytes: []const u8) Allocator.Error!void {
            try self.write(u64, @intCast(bytes.len));
            try self.raw(bytes);
        }
        fn write(self: *IdentityWriter, comptime T: type, value: T) Allocator.Error!void {
            const ns = self.graph.name_store;
            // Applied checked-type ids are provenance, not nominal identity.
            // Match Type.Store.typeEql; the declaration, arguments, backing
            // authority and all open constraints are encoded separately.
            if (T == Type.NamedType) return self.write(names.CheckedModuleDigest, value.module);
            if (T == Type.TypeId) {
                try self.leaves.append(self.graph.allocator, value);
                const digest = self.graph.types.specializationDigestCached(ns, value, null);
                return self.raw(&digest.bytes);
            }
            if (T == names.ModuleIdentityId) return self.raw(ns.moduleIdentityBytes(value));
            if (T == names.TypeNameId) return self.text(ns.typeNameText(value));
            if (T == names.RecordFieldNameId) return self.text(ns.recordFieldLabelText(value));
            if (T == names.TagNameId) return self.text(ns.tagLabelText(value));
            switch (@typeInfo(T)) {
                .@"struct" => |info| inline for (info.fields) |field| {
                    try self.write(field.type, @field(value, field.name));
                },
                .@"union" => |info| {
                    try self.write(info.tag_type.?, std.meta.activeTag(value));
                    inline for (info.fields) |field| {
                        if (std.meta.activeTag(value) == @field(info.tag_type.?, field.name)) {
                            try self.write(field.type, @field(value, field.name));
                            return;
                        }
                    }
                    unreachable;
                },
                .optional => |info| {
                    try self.write(bool, value != null);
                    if (value) |actual| try self.write(info.child, actual);
                },
                .pointer => |info| switch (info.size) {
                    .slice => {
                        try self.write(u64, @intCast(value.len));
                        for (value) |item| try self.write(info.child, item);
                    },
                    .one => try self.write(info.child, value.*),
                    .many, .c => @compileError("interface identity contains an unbounded pointer"),
                },
                .array => |info| {
                    if (info.child == u8) return self.raw(&value);
                    for (value) |item| try self.write(info.child, item);
                },
                .@"enum" => try self.write(u64, @intCast(@intFromEnum(value))),
                .int => {
                    // Local indices, lengths, and enum tags are predominantly
                    // small. A minimal varint keeps exact topology compact.
                    var bits: u64 = @intCast(value);
                    while (bits >= 0x80) : (bits >>= 7) try self.raw(&.{@as(u8, @truncate(bits)) | 0x80});
                    try self.raw(&.{@intCast(bits)});
                },
                .bool => try self.raw(&.{if (value) 1 else 0}),
                .void => {},
                .noreturn,
                .float,
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
                .type,
                => @compileError("unsupported interface identity scalar " ++ @typeName(T)),
            }
        }
    };

    const Capture = struct {
        retained: *GraphTypeFinals,
        settled: *GraphTypeFinals,
        graph: *InstGraph,
        allocator: Allocator,
        node_ids: collections.DenseMap(NodeId, NodeId),
        kind_ids: collections.DenseMap(FieldKindId, FieldKindId),
        shareable: collections.DenseMap(NodeId, bool),
        share_seen: collections.DenseMap(NodeId, void),
        related_ids: std.AutoHashMap(RelatedKey, u32),

        nodes: std.ArrayList(Node) = .empty,
        open_nodes: std.ArrayList(OpenNode) = .empty,
        kinds: std.ArrayList(Kind) = .empty,
        holes: []const NodeId = &.{},
        hole_classes: []?NodeId = &.{},

        // Backing groups can span declarations. Cache groups encode the full
        // identity predicate used by sameRelatedNamedInstance, including the
        // declaration checks required by relateNamedInstances during replay.
        const RelatedKey = struct {
            root: NodeId,
            module: names.ModuleIdentityId,
            type_name: names.TypeNameId,
            kind: Type.NamedKind,
            builtin_owner: ?static_dispatch.BuiltinOwner,
            arg_count: usize,
        };

        fn node(self: *Capture, raw: NodeId) Allocator.Error!NodeId {
            const root = self.graph.find(raw);
            if (self.node_ids.get(root)) |id| return id;
            const id: NodeId = @enumFromInt(self.nodes.items.len);
            try self.node_ids.put(root, id);
            try self.nodes.append(self.graph.allocator, undefined);
            if (self.holeIndex(root)) |hole| {
                if (self.graph.content(root) != .unresolved and try self.holeIsRepresentationNeutral(raw)) {
                    self.hole_classes[hole] = root;
                    const open_index: u32 = @intCast(self.open_nodes.items.len);
                    self.nodes.items[@intFromEnum(id)] = .{ .open = open_index };
                    try self.open_nodes.append(self.graph.allocator, .{ .content = .{ .unresolved = InstVariable.placeholder() } });
                    return id;
                }
            }
            // Representation authority and open field-kind cells forbid sharing
            // even when the runtime shape happens to be settled.
            // Source-interface evidence belongs to the original request node,
            // which may have redirected to a different class representative.
            if (self.graph.requestSourceInterface(raw) == null and try self.canShare(root)) {
                self.nodes.items[@intFromEnum(id)] = .{ .mono = try self.settled.sealNode(root) };
                return id;
            }
            const open_index: u32 = @intCast(self.open_nodes.items.len);
            self.nodes.items[@intFromEnum(id)] = .{ .open = open_index };
            try self.open_nodes.append(self.graph.allocator, undefined);
            var captured: OpenNode = .{ .content = try mapValue(self, InstNode, self.graph.content(root)) };
            if (self.graph.requestSourceInterface(raw)) |source| captured.source = try self.node(source);
            captured.recursive_slot = self.graph.recursive_value_slots.items[@intFromEnum(root)];
            captured.forced_dynamic = self.graph.forced_dynamic_iterator_roots.items[@intFromEnum(root)];
            captured.constructor_evidence = self.graph.requestPropagatesConstructorEvidence(raw);
            captured.private_backing = self.graph.private_backing_roots.items[@intFromEnum(root)];
            if (self.graph.classImportedMono(root)) |ty| captured.finished = try self.retained.sealType(ty);
            if (self.graph.content(root) == .named and self.graph.related_named_instances.contains(root)) {
                const named = self.graph.content(root).named;
                const next_group: u32 = @intCast(self.related_ids.count());
                const group = try self.related_ids.getOrPut(.{
                    .root = self.graph.relatedNamedInstanceRoot(root),
                    .module = named.def.module,
                    .type_name = named.def.type_name,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .arg_count = named.args.len,
                });
                if (!group.found_existing) group.value_ptr.* = next_group;
                captured.related_group = group.value_ptr.*;
            }
            self.open_nodes.items[open_index] = captured;
            return id;
        }

        fn holeIndex(self: *const Capture, root: NodeId) ?usize {
            for (self.holes, 0..) |hole, index| {
                if (hole == root) return index;
            }
            return null;
        }

        /// A hole stands for a class whose relation to the request is plain
        /// unification: nothing it reaches carries representation authority,
        /// source-interface or constructor evidence, or iterator identity.
        fn holeIsRepresentationNeutral(self: *Capture, raw: NodeId) Allocator.Error!bool {
            self.share_seen.clearRetainingCapacity();
            var scan = NeutralScan{ .graph = self.graph, .seen = &self.share_seen };
            return try scan.node(raw);
        }

        const NeutralScan = struct {
            graph: *InstGraph,
            seen: *collections.DenseMap(NodeId, void),

            fn node(self: *NeutralScan, raw: NodeId) Allocator.Error!bool {
                const graph = self.graph;
                const root = graph.find(raw);
                if ((try self.seen.getOrPut(root)).found_existing) return true;
                if (graph.private_backing_roots.items[@intFromEnum(root)]) return false;
                if (graph.requestSourceInterface(raw) != null or graph.requestPropagatesConstructorEvidence(raw)) return false;
                if (graph.forced_dynamic_iterator_roots.items[@intFromEnum(root)]) return false;
                const content = graph.content(root);
                switch (content) {
                    .named => |named| {
                        if (named.generated_iterator != null or named.def.generated != null or named.def.iterator_representation != .none or named.def.iterator_kind != .none) return false;
                        if (named.backing) |backing| if (backing.authority == .generated_private) {
                            return false;
                        };
                    },
                    .redirect,
                    .unresolved,
                    .primitive,
                    .list,
                    .box,
                    .tuple,
                    .func,
                    .tag_union,
                    .record,
                    .empty_tag_union,
                    .empty_record,
                    .erased,
                    .zst,
                    => {},
                }
                return self.value(InstNode, content);
            }

            fn value(self: *NeutralScan, comptime T: type, item: T) Allocator.Error!bool {
                if (T == NodeId) return self.node(item);
                if (T == InstFieldKind) return true;
                switch (@typeInfo(T)) {
                    .@"struct" => |info| inline for (info.fields) |field| {
                        if (!try self.value(field.type, @field(item, field.name))) return false;
                    },
                    .@"union" => |info| {
                        inline for (info.fields) |field| {
                            if (std.meta.activeTag(item) == @field(info.tag_type.?, field.name)) return self.value(field.type, @field(item, field.name));
                        }
                        unreachable;
                    },
                    .optional => |info| if (item) |actual| {
                        return self.value(info.child, actual);
                    },
                    .pointer => |info| switch (info.size) {
                        .slice => for (item) |child| {
                            if (!try self.value(info.child, child)) return false;
                        },
                        .one => return self.value(info.child, item.*),
                        .many, .c => @compileError("neutral scan reached an unbounded pointer"),
                    },
                    .array => |info| for (item) |child| {
                        if (!try self.value(info.child, child)) return false;
                    },
                    .type,
                    .void,
                    .bool,
                    .noreturn,
                    .int,
                    .float,
                    .comptime_float,
                    .comptime_int,
                    .undefined,
                    .null,
                    .error_union,
                    .error_set,
                    .@"enum",
                    .@"fn",
                    .@"opaque",
                    .frame,
                    .@"anyframe",
                    .vector,
                    .enum_literal,
                    => {},
                }
                return true;
            }
        };

        fn canShare(self: *Capture, root: NodeId) Allocator.Error!bool {
            if (self.shareable.get(root)) |known| return known;
            self.share_seen.clearRetainingCapacity();
            var scan = Shareability{ .capture = self, .seen = &self.share_seen };
            const result = try scan.node(root);
            if (result) {
                var it = scan.seen.keyIterator();
                while (it.next()) |node_id| try self.shareable.put(node_id.*, true);
            }
            return result;
        }

        const Shareability = struct {
            capture: *Capture,
            seen: *collections.DenseMap(NodeId, void),

            fn node(self: *Shareability, raw: NodeId) Allocator.Error!bool {
                const root = self.capture.graph.find(raw);
                if (self.capture.holeIndex(root) != null) return false;
                if (self.capture.shareable.get(root)) |known| return known;
                const result = try self.visit(raw, root);
                // A failed path proves every ancestor on that path reaches
                // non-shareable state. Successful cycles are cached only once
                // the entire root traversal has succeeded.
                if (!result) try self.capture.shareable.put(root, false);
                return result;
            }

            fn visit(self: *Shareability, raw: NodeId, root: NodeId) Allocator.Error!bool {
                const graph = self.capture.graph;
                if ((try self.seen.getOrPut(root)).found_existing) return true;
                if (graph.private_backing_roots.items[@intFromEnum(root)]) return false;
                if (graph.requestSourceInterface(raw) != null or graph.requestPropagatesConstructorEvidence(raw) or graph.related_named_instances.contains(root)) return false;
                if (graph.recursive_value_slots.items[@intFromEnum(root)]) return false;
                if (graph.forced_dynamic_iterator_roots.items[@intFromEnum(root)]) return false;
                const content = graph.content(root);
                switch (content) {
                    .unresolved => return false,
                    .named => |named| {
                        if (named.generated_iterator != null or named.def.generated != null or named.def.iterator_representation != .none or named.def.iterator_kind != .none) return false;
                        if (named.backing) |backing| if (backing.authority == .generated_private) {
                            return false;
                        };
                    },
                    .redirect,
                    .primitive,
                    .list,
                    .box,
                    .tuple,
                    .func,
                    .tag_union,
                    .record,
                    .empty_tag_union,
                    .empty_record,
                    .erased,
                    .zst,
                    => {},
                }
                return self.value(InstNode, content);
            }

            fn value(self: *Shareability, comptime T: type, item: T) Allocator.Error!bool {
                if (T == NodeId) return self.node(item);
                // Even a resolved field-kind cell still carries relation
                // evidence (required may join an explicit default). Only a
                // producer-sealed field has committed its slot representation.
                if (T == InstFieldKind) return item == .sealed;
                switch (@typeInfo(T)) {
                    .@"struct" => |info| inline for (info.fields) |field| {
                        if (!try self.value(field.type, @field(item, field.name))) return false;
                    },
                    .@"union" => |info| {
                        inline for (info.fields) |field| {
                            if (std.meta.activeTag(item) == @field(info.tag_type.?, field.name)) return self.value(field.type, @field(item, field.name));
                        }
                        unreachable;
                    },
                    .optional => |info| if (item) |actual| {
                        return self.value(info.child, actual);
                    },
                    .pointer => |info| switch (info.size) {
                        .slice => for (item) |child| {
                            if (!try self.value(info.child, child)) return false;
                        },
                        .one => return self.value(info.child, item.*),
                        .many, .c => @compileError("shareability reached an unbounded pointer"),
                    },
                    .array => |info| for (item) |child| {
                        if (!try self.value(info.child, child)) return false;
                    },
                    .type,
                    .void,
                    .bool,
                    .noreturn,
                    .int,
                    .float,
                    .comptime_float,
                    .comptime_int,
                    .undefined,
                    .null,
                    .error_union,
                    .error_set,
                    .@"enum",
                    .@"fn",
                    .@"opaque",
                    .frame,
                    .@"anyframe",
                    .vector,
                    .enum_literal,
                    => {},
                }
                return true;
            }
        };

        fn kind(self: *Capture, raw: FieldKindId) Allocator.Error!FieldKindId {
            const root = self.graph.findFieldKind(raw);
            if (self.kind_ids.get(root)) |id| return id;
            const id: FieldKindId = @enumFromInt(self.kinds.items.len);
            try self.kind_ids.put(root, id);
            try self.kinds.append(self.graph.allocator, undefined);
            const source = self.graph.field_kinds.items[@intFromEnum(root)];
            const mapped = try mapValue(self, Kind, .{ .resolved = source.resolved, .cells = source.cells });
            self.kinds.items[@intFromEnum(id)] = mapped;
            return id;
        }

        fn scalar(_: *Capture, comptime T: type, value: T) Allocator.Error!T {
            return value;
        }
    };

    const Instance = struct {
        graph: *InstGraph,
        allocator: Allocator,
        nodes: []NodeId,
        kinds: []FieldKindId,

        fn node(self: *Instance, id: NodeId) Allocator.Error!NodeId {
            return self.nodes[@intFromEnum(id)];
        }
        fn kind(self: *Instance, id: FieldKindId) Allocator.Error!FieldKindId {
            return self.kinds[@intFromEnum(id)];
        }
        fn scalar(_: *Instance, comptime T: type, value: T) Allocator.Error!T {
            return value;
        }
    };

    fn Copier(comptime Context: type) type {
        return struct {
            allocator: Allocator,
            context: Context,
            fn node(_: *@This(), id: NodeId) Allocator.Error!NodeId {
                return id;
            }
            fn kind(_: *@This(), id: FieldKindId) Allocator.Error!FieldKindId {
                return id;
            }
            fn scalar(self: *@This(), comptime T: type, value: T) Allocator.Error!T {
                return self.context.mapScalar(T, value);
            }
        };
    }

    /// Exhaustive structural mapping keeps nested backing provenance, default
    /// identities, row links, and future scalar evidence in the same format.
    fn mapValue(context: anytype, comptime T: type, value: T) Allocator.Error!T {
        if (T == NodeId) return context.node(value);
        if (T == FieldKindId) return context.kind(value);
        return switch (@typeInfo(T)) {
            .@"struct" => |info| blk: {
                var result: T = undefined;
                inline for (info.fields) |field| @field(result, field.name) = try mapValue(context, field.type, @field(value, field.name));
                break :blk result;
            },
            .@"union" => |info| blk: {
                inline for (info.fields) |field| {
                    if (std.meta.activeTag(value) == @field(info.tag_type.?, field.name)) {
                        break :blk @unionInit(T, field.name, try mapValue(context, field.type, @field(value, field.name)));
                    }
                }
                unreachable;
            },
            .optional => |info| if (value) |actual| try mapValue(context, info.child, actual) else null,
            .pointer => |info| blk: {
                switch (info.size) {
                    .slice => {
                        const result = try context.allocator.alloc(info.child, value.len);
                        for (value, result) |item, *out| out.* = try mapValue(context, info.child, item);
                        break :blk result;
                    },
                    .one => {
                        const result = try context.allocator.create(info.child);
                        result.* = try mapValue(context, info.child, value.*);
                        break :blk result;
                    },
                    .many, .c => @compileError("interface constraints contain an unbounded pointer"),
                }
            },
            .array => |info| blk: {
                var result: T = undefined;
                for (value, &result) |item, *out| out.* = try mapValue(context, info.child, item);
                break :blk result;
            },
            .type,
            .void,
            .bool,
            .noreturn,
            .int,
            .float,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .error_union,
            .error_set,
            .@"enum",
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            => try context.scalar(T, value),
        };
    }
};

/// Deterministic operation counts for diagnosing Monotype graph workloads.
/// `InstGraph.diagnostics` remains null unless detailed diagnostics were
/// requested, so ordinary lowering does not count hot-path operations.
pub const GraphDiagnostics = struct {
    /// Node identities retained by argument-class snapshots.
    argument_class_members_snapshotted: u64 = 0,
    /// Visited-set slots initialized or touched by structural backing walks.
    structural_backing_scan_slots: u64 = 0,
    nodes_created: u64 = 0,
    unify_requests: u64 = 0,
    class_unions: u64 = 0,
    active_type_requests: u64 = 0,
    imported_type_view_hits: u64 = 0,
    active_snapshot_cache_hits: u64 = 0,
    active_snapshot_cache_misses: u64 = 0,
    active_snapshot_nodes_materialized: u64 = 0,
    provisional_snapshot_nodes_materialized: u64 = 0,
    active_snapshot_invalidations: u64 = 0,
    active_snapshot_entries_invalidated: u64 = 0,
    mono_import_requests: u64 = 0,
    mono_import_hits: u64 = 0,
    mono_import_misses: u64 = 0,
    iterator_interface_scans: u64 = 0,
    iterator_interface_cache_hits: u64 = 0,
    iterator_interface_nodes_visited: u64 = 0,
    generated_private_guard_returns: u64 = 0,
    generated_private_scans: u64 = 0,
    generated_iterator_lookups: u64 = 0,
    generated_private_cache_hits: u64 = 0,
    generated_private_nodes_visited: u64 = 0,
    finished_mono_scans: u64 = 0,
    finished_mono_nodes_visited: u64 = 0,
    /// Declaration-backed nominal instantiation cache probes, and the
    /// candidate instances those probes examined. Scanned candidates must
    /// stay proportional to lookups rather than to instances created, or
    /// repeated constructions of one declaration turn quadratic (issue
    /// 10978 hit exactly that).
    nominal_backing_lookups: u64 = 0,
    nominal_backing_instances_scanned: u64 = 0,
    /// Nominal-backing deletions that leave tombstones in the lookup index.
    /// This must remain zero: rekeying cannot make lookup cost depend on the
    /// graph's history of root migrations.
    nominal_backing_tombstone_deletions: u64 = 0,
    /// Union-find resolutions across all graph operations: the broadest
    /// deterministic proxy for total solver work.
    union_find_resolutions: u64 = 0,
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

/// Exact iterator construction identity. Argument roots are maintained by
/// union_, while declaration and callable evidence are producer-owned data.
const GeneratedIteratorKey = struct {
    kind: Type.IteratorKind,
    named_kind: Type.NamedKind,
    module: names.ModuleIdentityId,
    type_name: names.TypeNameId,
    source_decl: ?u32,
    callable_evidence: ?names.TypeDigest,
    args: []NodeId,

    fn fromNamed(named: *const InstNamed) ?GeneratedIteratorKey {
        const provenance = named.generated_iterator orelse return null;
        return .{
            .kind = named.def.iterator_kind,
            .named_kind = named.kind,
            .module = named.def.module,
            .type_name = named.def.type_name,
            .source_decl = named.def.source_decl,
            .callable_evidence = provenance.callable_evidence,
            .args = named.args,
        };
    }
};

const GeneratedIteratorKeyContext = struct {
    pub fn hash(_: @This(), key: GeneratedIteratorKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.kind);
        std.hash.autoHash(&hasher, key.named_kind);
        std.hash.autoHash(&hasher, key.module);
        std.hash.autoHash(&hasher, key.type_name);
        std.hash.autoHash(&hasher, key.source_decl);
        std.hash.autoHash(&hasher, key.callable_evidence);
        std.hash.autoHash(&hasher, key.args.len);
        for (key.args) |arg| std.hash.autoHash(&hasher, arg);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: GeneratedIteratorKey, b: GeneratedIteratorKey) bool {
        return a.kind == b.kind and a.named_kind == b.named_kind and
            a.module == b.module and a.type_name == b.type_name and
            a.source_decl == b.source_decl and
            optionalInstDigestEql(a.callable_evidence, b.callable_evidence) and
            std.mem.eql(NodeId, a.args, b.args);
    }
};

const GeneratedIteratorIndex = collections.RekeyingHashMap(GeneratedIteratorKey, NodeId, GeneratedIteratorKeyContext, 80);
const GeneratedIteratorEntry = struct {
    key: GeneratedIteratorKey,
    previous: ?NodeId = null,
    next: ?NodeId = null,
    indexed: bool = false,
};
const GeneratedIteratorOccurrence = struct { node: NodeId, arg_index: usize };

/// Resolve a query's arguments without allocating a temporary root tuple.
const GeneratedIteratorLookup = struct {
    key: GeneratedIteratorKey,
    item: NodeId,
    components: []const NodeId,
};
const GeneratedIteratorLookupContext = struct {
    graph: *InstGraph,

    pub fn hash(self: @This(), lookup: GeneratedIteratorLookup) u64 {
        var key = lookup.key;
        key.args = &.{};
        // Use the same scalar prefix as the resident key's hash.
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.kind);
        std.hash.autoHash(&hasher, key.named_kind);
        std.hash.autoHash(&hasher, key.module);
        std.hash.autoHash(&hasher, key.type_name);
        std.hash.autoHash(&hasher, key.source_decl);
        std.hash.autoHash(&hasher, key.callable_evidence);
        std.hash.autoHash(&hasher, lookup.components.len + 1);
        std.hash.autoHash(&hasher, self.graph.find(lookup.item));
        for (lookup.components) |arg| std.hash.autoHash(&hasher, self.graph.find(arg));
        return hasher.final();
    }

    pub fn eql(self: @This(), lookup: GeneratedIteratorLookup, stored: GeneratedIteratorKey) bool {
        var prefix = stored;
        prefix.args = &.{};
        if (!GeneratedIteratorKeyContext.eql(.{}, lookup.key, prefix) or
            stored.args.len != lookup.components.len + 1 or
            stored.args[0] != self.graph.find(lookup.item)) return false;
        for (lookup.components, stored.args[1..]) |arg, root| {
            if (self.graph.find(arg) != root) return false;
        }
        return true;
    }
};

const NominalBackingKey = struct {
    declaration: NominalBackingDeclaration,
    args: []const NodeId,
};

/// Bucket selector only: `NominalBackingKeyContext.eql` compares the
/// declaration bytes and every argument, so a collision costs a probe.
fn hashNominalBackingKey(declaration: NominalBackingDeclaration, args: []const NodeId) u64 {
    var hasher = NominalBackingKeyHasher.init(declaration, args.len);
    for (args) |arg| hasher.add(arg);
    return hasher.state;
}

/// The module bytes are a cryptographic digest, so one of their words already
/// selects buckets uniformly; declaration and argument ids are mixed into it.
const NominalBackingKeyHasher = struct {
    state: u64,

    fn init(declaration: NominalBackingDeclaration, arity: usize) NominalBackingKeyHasher {
        const module_word = std.mem.readInt(u64, declaration.module_bytes[0..8], .little);
        const declaration_word = (@as(u64, declaration.declaration_id) << 32) | @as(u64, @as(u32, @truncate(arity)));
        return .{ .state = mix(module_word ^ declaration_word) };
    }

    fn add(self: *NominalBackingKeyHasher, arg: NodeId) void {
        self.state = mix(self.state ^ @intFromEnum(arg));
    }

    fn mix(value: u64) u64 {
        var mixed = value;
        mixed ^= mixed >> 33;
        mixed *%= 0xff51afd7ed558ccd;
        mixed ^= mixed >> 33;
        mixed *%= 0xc4ceb9fe1a85ec53;
        mixed ^= mixed >> 33;
        return mixed;
    }
};

fn nominalBackingDeclarationsEqual(left: NominalBackingDeclaration, right: NominalBackingDeclaration) bool {
    return std.mem.eql(u8, left.module_bytes[0..], right.module_bytes[0..]) and
        left.declaration_id == right.declaration_id;
}

const NominalBackingKeyContext = struct {
    pub fn hash(_: NominalBackingKeyContext, key: NominalBackingKey) u64 {
        return hashNominalBackingKey(key.declaration, key.args);
    }

    pub fn eql(_: NominalBackingKeyContext, left: NominalBackingKey, right: NominalBackingKey) bool {
        return nominalBackingDeclarationsEqual(left.declaration, right.declaration) and
            std.mem.eql(NodeId, left.args, right.args);
    }
};

const NominalBackingIndex = collections.RekeyingHashMap(
    NominalBackingKey,
    NominalBackingInstanceId,
    NominalBackingKeyContext,
    80,
);

const NominalBackingLookup = struct {
    declaration: NominalBackingDeclaration,
    args: []const NodeId,
};

/// Hash-map adapter that resolves a lookup's permanent argument ids without
/// allocating a temporary root tuple. Resident keys always contain live roots;
/// `union_` eagerly rekeys every tuple that mentions its losing root.
const NominalBackingLookupContext = struct {
    graph: *InstGraph,

    pub fn hash(self: NominalBackingLookupContext, lookup: NominalBackingLookup) u64 {
        var hasher = NominalBackingKeyHasher.init(lookup.declaration, lookup.args.len);
        for (lookup.args) |arg| hasher.add(self.graph.find(arg));
        return hasher.state;
    }

    pub fn eql(self: NominalBackingLookupContext, lookup: NominalBackingLookup, stored: NominalBackingKey) bool {
        if (!nominalBackingDeclarationsEqual(lookup.declaration, stored.declaration) or
            lookup.args.len != stored.args.len)
        {
            return false;
        }
        for (lookup.args, stored.args) |wanted, existing| {
            if (self.graph.find(wanted) != existing) return false;
        }
        return true;
    }
};

const NominalBackingInstanceId = enum(u32) { _ };

/// One indexed instantiation of a declaration backing. Argument ids are stable
/// arena storage and remain live union-find roots for as long as `active` is
/// true. A root merge can collapse two keys; the retired entry stays allocated
/// so reverse-index occurrences and previously returned node ids remain valid.
const NominalBackingInstance = struct {
    declaration: NominalBackingDeclaration,
    args: []NodeId,
    node: NodeId,
    active: bool,
};

const NominalBackingOccurrence = struct {
    instance: NominalBackingInstanceId,
    arg_index: u32,
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
    /// `InstGraph.structure_epoch` when the dependencies were last verified;
    /// while the epoch is unchanged no class has changed, so the answer is
    /// still valid without rechecking them.
    verified_epoch: u32 = 0,
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
    /// Sole owner domain for every `TypeId` retained by this graph, including
    /// imported monotypes, active snapshots, and final sealing caches. Callers
    /// must relocate external ids before passing them to graph operations.
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
    /// Explicit equivalence classes for matching nominal applications whose
    /// backing nodes must remain independently owned. This is deliberately
    /// separate from the main type union-find: consumers can use the proven
    /// nominal identity without collapsing declaration and request storage.
    related_named_instances: collections.DenseMap(NodeId, RelatedNamedInstance),
    /// One related wrapper for each exact declaration-backing witness. The
    /// nominal backing cache includes every type argument in its key, so a
    /// shared permanent backing id proves two independently allocated wrappers
    /// denote the same application without inspecting their shape.
    related_named_backings: collections.DenseMap(NodeId, NodeId),
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
    /// Reverse links for immutable active snapshots. Finished Monotype imports
    /// use a per-import memo instead: equal interned types at independent
    /// occurrences must not share mutable solver nodes.
    active_snapshot_nodes: collections.DenseMap(Type.TypeId, NodeId),
    /// Finished Monotypes already imported into the current ownership scope.
    imported_type_nodes: collections.DenseMap(Type.TypeId, NodeId),
    /// Exact immutable Monotype snapshot imported at each permanent node.
    /// Unlike `node_snapshots`, these are producer-owned representation
    /// witnesses. Keeping the direct node association lets consumers of an
    /// imported request use the exact finished TypeId rather than reconstructing
    /// an equivalent public shape.
    imported_monos: collections.DenseMap(NodeId, Type.TypeId),
    /// Per class root: the imported Monotype of the first class member, in
    /// member order, that has one. Unions keep the winner's, whose members
    /// precede the loser's.
    class_imported_monos: std.ArrayList(?Type.TypeId),
    /// Exact declaration-plus-current-argument-roots index for instantiated
    /// nominal backings. Keys point into the stable argument storage owned by
    /// `nominal_backing_instances`.
    nominal_backing_index: NominalBackingIndex,
    nominal_backing_instances: std.ArrayList(NominalBackingInstance),
    /// Argument positions by their current root. This is the precise
    /// invalidation index used to rekey affected instances in `union_`.
    nominal_backings_by_root: collections.DenseMap(NodeId, std.ArrayList(NominalBackingOccurrence)),
    /// Reused scratch for de-duplicating instances that mention a losing root
    /// in more than one argument position.
    nominal_backing_affected: std.ArrayList(NominalBackingInstanceId),
    nominal_backing_migration_epochs: std.ArrayList(u64),
    nominal_backing_migration_epoch: u64,
    /// Backing pairs whose keys became equal during root migration. Migration
    /// restores every index invariant before these pairs are unified.
    nominal_backing_collisions: std.ArrayList(NodePair),
    processing_nominal_backing_collisions: bool,
    /// Exact source function node from which each generated-private request
    /// function was constructed. A generic source interface may itself carry
    /// upstream generated-private arguments; retaining that producer node is
    /// what lets the callee instantiate those relations without reconstruction.
    request_source_interfaces: std.ArrayList(?NodeId),
    constructor_evidence_requests: std.ArrayList(bool),
    /// Producer-marked representation witnesses must retain request-local
    /// identity even when the backing's runtime structure is fully settled.
    private_backing_roots: std.ArrayList(bool),
    /// Per class root: a minted iterator whose relation graph proved that
    /// retaining the minted tier would create a recursive component identity.
    /// Unions carry the mark to the joined class; finalization constructs the
    /// single forced-dynamic fixed point for it.
    forced_dynamic_iterator_roots: std.ArrayList(bool),
    /// Per class root: a value slot that differs from the corresponding source
    /// slot on an explicit recursive edge. Function recursion and loop
    /// feedback both mark here; a later minted join touching one of these
    /// slots proves that recursion grows the representation rather than merely
    /// recurring over a fixed iterator. Unions carry the mark.
    recursive_value_slots: std.ArrayList(bool),
    /// Shared allocation-free scratch and cache for exact structural
    /// containment. The two queries share one conservative dependency list,
    /// while each walk can stop as soon as its requested property is found.
    containment_pending: std.ArrayList(NodeId),
    containment_visit_epochs: std.ArrayList(u32),
    containment_visit_epoch: u32,
    containment_cache: collections.DenseMap(NodeId, ContainmentCacheEntry),
    /// Pools for the visited sets the graph's walks create per query. Fresh
    /// maps re-allocate and re-zero sparse chunks across the node/type ID
    /// domains on every walk; pooled maps keep their chunks.
    node_set_pool: collections.DenseMapPool(NodeId, void),
    /// Maps and lists borrowed by every `InterfaceConstraints.capture`.
    capture_scratch: InterfaceConstraints.CaptureScratch,
    /// Roots whose every reachable node was found resolved, stamped with the
    /// `resolved_epoch` current at that walk. Resolvedness survives every
    /// union (a concrete class always wins over a variable), every content
    /// replacement of a class no snapshot could observe, and every
    /// observationally equivalent compression, so a stamp goes stale only
    /// when an observable class's content is replaced or a variable wins a
    /// union, each of which advances the epoch.
    resolved_roots: collections.DenseMap(NodeId, u32),
    resolved_epoch: u32,
    /// Advances on every union and every content change, so an equal epoch
    /// proves no class in the graph has changed since.
    structure_epoch: u32,
    /// Types known to reach no active snapshot. Store types are immutable
    /// and every snapshot is a freshly reserved slot, so no existing type can
    /// come to reference one: a negative answer never changes.
    snapshot_free_types: collections.DenseMap(Type.TypeId, void),
    /// At least how many times a node received a generated-private backing.
    /// While zero, no class can reach one and the containment query answers
    /// at once.
    generated_private_nodes: u32,
    /// Monotone provenance count: zero proves both finalization passes empty.
    generated_iterator_nodes: u32,
    generated_iterator_index: GeneratedIteratorIndex,
    generated_iterator_entries: collections.DenseMap(NodeId, GeneratedIteratorEntry),
    generated_iterators_by_root: collections.DenseMap(NodeId, std.ArrayList(GeneratedIteratorOccurrence)),
    /// Interned type per class root sealed in final mode, kept and dropped
    /// together with `current_snapshots`: the same relation changes that
    /// leave an active view current leave a committed type current.
    current_durable: collections.DenseMap(NodeId, Type.TypeId),
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
            .related_named_instances = collections.DenseMap(NodeId, RelatedNamedInstance).init(allocator),
            .related_named_backings = collections.DenseMap(NodeId, NodeId).init(allocator),
            .node_snapshots = collections.DenseMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .current_snapshots = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .current_snapshots_dirty = false,
            .active_snapshot_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .imported_type_nodes = collections.DenseMap(Type.TypeId, NodeId).init(allocator),
            .imported_monos = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .nominal_backing_index = NominalBackingIndex.init(allocator, .{}),
            .nominal_backing_instances = .empty,
            .nominal_backings_by_root = collections.DenseMap(NodeId, std.ArrayList(NominalBackingOccurrence)).init(allocator),
            .nominal_backing_affected = .empty,
            .nominal_backing_migration_epochs = .empty,
            .nominal_backing_migration_epoch = 0,
            .nominal_backing_collisions = .empty,
            .processing_nominal_backing_collisions = false,
            .request_source_interfaces = .empty,
            .constructor_evidence_requests = .empty,
            .private_backing_roots = .empty,
            .class_imported_monos = .empty,
            .forced_dynamic_iterator_roots = .empty,
            .recursive_value_slots = .empty,
            .containment_pending = .empty,
            .containment_visit_epochs = .empty,
            .containment_visit_epoch = 0,
            .containment_cache = collections.DenseMap(NodeId, ContainmentCacheEntry).init(allocator),
            .node_set_pool = collections.DenseMapPool(NodeId, void).init(allocator),
            .capture_scratch = InterfaceConstraints.CaptureScratch.init(allocator),
            .resolved_roots = collections.DenseMap(NodeId, u32).init(allocator),
            .resolved_epoch = 0,
            .structure_epoch = 0,
            .snapshot_free_types = collections.DenseMap(Type.TypeId, void).init(allocator),
            .generated_private_nodes = 0,
            .generated_iterator_nodes = 0,
            .generated_iterator_index = GeneratedIteratorIndex.init(allocator, .{}),
            .generated_iterator_entries = collections.DenseMap(NodeId, GeneratedIteratorEntry).init(allocator),
            .generated_iterators_by_root = collections.DenseMap(NodeId, std.ArrayList(GeneratedIteratorOccurrence)).init(allocator),
            .current_durable = collections.DenseMap(NodeId, Type.TypeId).init(allocator),
            .type_set_pool = collections.DenseMapPool(Type.TypeId, void).init(allocator),
        };
        return graph;
    }

    pub fn setDiagnostics(self: *InstGraph, diagnostics: *GraphDiagnostics) void {
        self.diagnostics = diagnostics;
    }

    /// Begin an unrelated specialization while retaining this lane's allocated
    /// graph capacity. Every prior node identity and snapshot becomes invalid;
    /// the cumulative destination type and name stores remain unchanged.
    pub fn reset(self: *InstGraph) void {
        self.relation_state = .producing;
        self.diagnostics = null;
        self.nodes.clearRetainingCapacity();
        self.field_kinds.clearRetainingCapacity();
        self.versions.clearRetainingCapacity();
        self.class_member_next.clearRetainingCapacity();
        self.class_member_head.clearRetainingCapacity();
        self.class_member_tail.clearRetainingCapacity();
        self.processed_relations.clearRetainingCapacity();
        self.related_named_instances.clearRetainingCapacity();
        self.related_named_backings.clearRetainingCapacity();
        var views = self.node_snapshots.valueIterator();
        while (views.next()) |list| list.deinit(self.allocator);
        self.node_snapshots.clearRetainingCapacity();
        self.current_snapshots.clearRetainingCapacity();
        self.current_snapshots_dirty = false;
        self.active_snapshot_nodes.clearRetainingCapacity();
        self.imported_type_nodes.clearRetainingCapacity();
        self.imported_monos.clearRetainingCapacity();
        self.nominal_backing_index.clearRetainingCapacity();
        self.nominal_backing_instances.clearRetainingCapacity();
        var backing_occurrences = self.nominal_backings_by_root.valueIterator();
        while (backing_occurrences.next()) |occurrences| occurrences.deinit(self.allocator);
        self.nominal_backings_by_root.clearRetainingCapacity();
        self.nominal_backing_affected.clearRetainingCapacity();
        self.nominal_backing_migration_epochs.clearRetainingCapacity();
        self.nominal_backing_migration_epoch = 0;
        self.nominal_backing_collisions.clearRetainingCapacity();
        self.processing_nominal_backing_collisions = false;
        self.request_source_interfaces.clearRetainingCapacity();
        self.constructor_evidence_requests.clearRetainingCapacity();
        self.private_backing_roots.clearRetainingCapacity();
        self.class_imported_monos.clearRetainingCapacity();
        self.forced_dynamic_iterator_roots.clearRetainingCapacity();
        self.recursive_value_slots.clearRetainingCapacity();
        self.containment_pending.clearRetainingCapacity();
        self.containment_visit_epochs.clearRetainingCapacity();
        self.containment_visit_epoch = 0;
        var containment_entries = self.containment_cache.valueIterator();
        while (containment_entries.next()) |entry| entry.deinit(self.allocator);
        self.containment_cache.clearRetainingCapacity();
        self.resolved_roots.clearRetainingCapacity();
        self.resolved_epoch = 0;
        self.structure_epoch = 0;
        self.snapshot_free_types.clearRetainingCapacity();
        self.generated_private_nodes = 0;
        self.generated_iterator_nodes = 0;
        self.generated_iterator_index.clearRetainingCapacity();
        self.generated_iterator_entries.clearRetainingCapacity();
        var iterator_occurrences = self.generated_iterators_by_root.valueIterator();
        while (iterator_occurrences.next()) |occurrences| occurrences.deinit(self.allocator);
        self.generated_iterators_by_root.clearRetainingCapacity();
        self.current_durable.clearRetainingCapacity();
        _ = self.arena_impl.reset(.retain_capacity);
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

    fn countNominalBackingIndexRemoval(self: *InstGraph) void {
        if (NominalBackingIndex.removal_leaves_tombstones) {
            self.countDiagnostic("nominal_backing_tombstone_deletions");
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
        var backing_occurrences = self.nominal_backings_by_root.valueIterator();
        while (backing_occurrences.next()) |occurrences| {
            occurrences.deinit(allocator);
        }
        self.nominal_backings_by_root.deinit();
        self.nominal_backing_collisions.deinit(allocator);
        self.nominal_backing_migration_epochs.deinit(allocator);
        self.nominal_backing_affected.deinit(allocator);
        self.nominal_backing_instances.deinit(allocator);
        self.nominal_backing_index.deinit();
        self.generated_iterator_index.deinit();
        self.generated_iterator_entries.deinit();
        var iterator_occurrences = self.generated_iterators_by_root.valueIterator();
        while (iterator_occurrences.next()) |occurrences| occurrences.deinit(allocator);
        self.generated_iterators_by_root.deinit();
        self.request_source_interfaces.deinit(allocator);
        self.constructor_evidence_requests.deinit(allocator);
        self.private_backing_roots.deinit(allocator);
        self.class_imported_monos.deinit(allocator);
        self.forced_dynamic_iterator_roots.deinit(allocator);
        self.recursive_value_slots.deinit(allocator);
        self.containment_pending.deinit(allocator);
        self.containment_visit_epochs.deinit(allocator);
        self.current_durable.deinit();
        self.snapshot_free_types.deinit();
        self.resolved_roots.deinit();
        self.node_set_pool.deinit();
        self.capture_scratch.deinit(allocator);
        self.type_set_pool.deinit();
        var containment_entries = self.containment_cache.valueIterator();
        while (containment_entries.next()) |entry| {
            entry.deinit(allocator);
        }
        self.containment_cache.deinit();
        self.imported_monos.deinit();
        self.active_snapshot_nodes.deinit();
        self.imported_type_nodes.deinit();
        self.related_named_backings.deinit();
        self.related_named_instances.deinit();
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

    /// Generated-iterator provenance, which lives in the graph arena.
    pub fn generatedIterator(self: *InstGraph, provenance: InstGeneratedIterator) Allocator.Error!*const InstGeneratedIterator {
        const stored = try self.arena().create(InstGeneratedIterator);
        stored.* = provenance;
        return stored;
    }

    /// Node content for a named payload, which lives in the graph arena.
    pub fn namedContent(self: *InstGraph, named: InstNamed) Allocator.Error!InstNode {
        const stored = try self.arena().create(InstNamed);
        stored.* = named;
        return .{ .named = stored };
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

    /// Return the source-language value node a generated codec reads or
    /// writes for one record field. This is intentionally distinct from the
    /// runtime slot: an optional field's `ty` is the compiler-reserved tagged
    /// presence slot, while its codec operates on the explicit `value_ty`.
    pub fn codecFieldValueNode(self: *InstGraph, field: InstField) NodeId {
        const value = switch (field.kind) {
            .required => blk: {
                if (field.value_ty != null or field.default != null) {
                    Common.invariant("required codec field carried optional or defaulted metadata");
                }
                break :blk field.ty;
            },
            .optional => blk: {
                if (field.default != null) Common.invariant("optional codec field carried a default identity");
                break :blk field.value_ty orelse
                    Common.invariant("optional codec field carried no source value node");
            },
            .defaulted => |default| blk: {
                if (field.value_ty != null or field.default == null or !std.meta.eql(field.default.?, default)) {
                    Common.invariant("defaulted codec field metadata disagreed with its field kind");
                }
                break :blk field.ty;
            },
            .undetermined => blk: {
                if (field.default != null) Common.invariant("undetermined codec field carried a default identity");
                break :blk field.value_ty orelse
                    Common.invariant("undetermined codec field carried no source value node");
            },
            .sealed => Common.invariant("sealed record field reached graph-native codec planning"),
        };
        return self.find(value);
    }

    /// Return the explicit field kind seen by generated-codec planning. An
    /// unresolved specialization-local presence cell has the declared
    /// Monotype default of `required`; relation freeze commits that same
    /// choice before any completed Monotype is emitted.
    pub fn codecFieldKind(self: *InstGraph, field: InstField) ResolvedFieldKind {
        return switch (field.kind) {
            .required => .required,
            .optional => .optional,
            .defaulted => |default| .{ .defaulted = default },
            .undetermined => self.resolvedFieldKind(field.kind) orelse .required,
            .sealed => Common.invariant("sealed record field reached graph-native codec planning"),
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
        self.assertPermanentNode(request_fn);
        self.assertPermanentNode(source_fn);
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

    pub fn registerConstructorEvidenceRequest(self: *InstGraph, request_fn: NodeId) void {
        self.requireRelationProduction();
        self.assertPermanentNode(request_fn);
        self.constructor_evidence_requests.items[@intFromEnum(request_fn)] = true;
        self.constructor_evidence_requests.items[@intFromEnum(self.find(request_fn))] = true;
    }

    pub fn requestPropagatesConstructorEvidence(self: *InstGraph, request_fn: NodeId) bool {
        return self.constructor_evidence_requests.items[@intFromEnum(request_fn)] or
            self.constructor_evidence_requests.items[@intFromEnum(self.find(request_fn))];
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
        self.countDiagnostic("generated_iterator_lookups");
        return self.generated_iterator_index.getAdapted(GeneratedIteratorLookup{
            .key = .{
                .kind = kind,
                .named_kind = public_named.kind,
                .module = public_named.def.module,
                .type_name = public_named.def.type_name,
                .source_decl = public_named.def.source_decl,
                .callable_evidence = callable_evidence,
                .args = &.{},
            },
            .item = public_named.args[0],
            .components = components,
        }, GeneratedIteratorLookupContext{ .graph = self });
    }

    /// Equal construction keys can coexist before an explicit relation joins
    /// them. Keep every candidate and retain the old scan's lowest-node choice.
    fn linkGeneratedIterator(self: *InstGraph, node: NodeId) void {
        const entry = self.generated_iterator_entries.getPtr(node).?;
        std.debug.assert(!entry.indexed);
        var previous: ?NodeId = null;
        var next = self.generated_iterator_index.get(entry.key);
        while (next) |candidate| {
            if (@intFromEnum(candidate) > @intFromEnum(node)) break;
            previous = candidate;
            next = self.generated_iterator_entries.get(candidate).?.next;
        }
        entry.previous = previous;
        entry.next = next;
        entry.indexed = true;
        if (previous) |prev| {
            self.generated_iterator_entries.getPtr(prev).?.next = node;
        } else {
            _ = self.generated_iterator_index.fetchRemove(entry.key);
            self.generated_iterator_index.putAssumeCapacityNoClobber(entry.key, node);
        }
        if (next) |following| self.generated_iterator_entries.getPtr(following).?.previous = node;
    }

    fn unlinkGeneratedIterator(self: *InstGraph, node: NodeId) void {
        const entry = self.generated_iterator_entries.getPtr(node).?;
        if (!entry.indexed) return;
        if (entry.previous) |previous| {
            self.generated_iterator_entries.getPtr(previous).?.next = entry.next;
        } else {
            const removed = self.generated_iterator_index.fetchRemove(entry.key).?;
            std.debug.assert(removed.value == node);
            if (entry.next) |next| {
                self.generated_iterator_index.putAssumeCapacityNoClobber(self.generated_iterator_entries.get(next).?.key, next);
            }
        }
        if (entry.next) |next| self.generated_iterator_entries.getPtr(next).?.previous = entry.previous;
        entry.indexed = false;
    }

    fn removeGeneratedIterator(self: *InstGraph, node: NodeId) void {
        const entry = self.generated_iterator_entries.get(node) orelse return;
        self.unlinkGeneratedIterator(node);
        for (entry.key.args, 0..) |root, arg_index| {
            const occurrences = self.generated_iterators_by_root.getPtr(root).?;
            for (occurrences.items, 0..) |occurrence, index| {
                if (occurrence.node == node and occurrence.arg_index == arg_index) {
                    _ = occurrences.swapRemove(index);
                    break;
                }
            } else unreachable;
        }
        _ = self.generated_iterator_entries.remove(node);
    }

    fn updateGeneratedIterator(self: *InstGraph, node: NodeId, content_: InstNode) Allocator.Error!void {
        const new_key = if (content_ == .named) GeneratedIteratorKey.fromNamed(content_.named) else null;
        if (new_key) |key| {
            if (self.generated_iterator_entries.get(node)) |existing| {
                var lookup_key = key;
                lookup_key.args = &.{};
                if (GeneratedIteratorLookupContext.eql(.{ .graph = self }, .{
                    .key = lookup_key,
                    .item = key.args[0],
                    .components = key.args[1..],
                }, existing.key)) return;
            }
            var stored = key;
            stored.args = try self.arena().alloc(NodeId, key.args.len);
            for (stored.args, key.args) |*root, arg| root.* = self.find(arg);
            try self.generated_iterator_index.ensureTotalCapacity(self.generated_iterator_index.count() + 1);
            // Stage reverse occurrences before replacing a live key. Roll them
            // back on allocation failure, including repeated argument roots.
            var staged: usize = 0;
            errdefer while (staged > 0) {
                staged -= 1;
                _ = self.generated_iterators_by_root.getPtr(stored.args[staged]).?.pop();
            };
            for (stored.args, 0..) |root, arg_index| {
                const occurrences = try self.generated_iterators_by_root.getOrPut(root);
                if (!occurrences.found_existing) occurrences.value_ptr.* = .empty;
                try occurrences.value_ptr.append(self.allocator, .{ .node = node, .arg_index = arg_index });
                staged += 1;
            }
            const entry = try self.generated_iterator_entries.getOrPut(node);
            if (entry.found_existing) self.removeGeneratedIterator(node);
            self.generated_iterator_entries.putAssumeCapacity(node, .{ .key = stored });
            self.linkGeneratedIterator(node);
        } else self.removeGeneratedIterator(node);
    }

    /// Only keys that explicitly reference the losing argument class move.
    /// Unlink all affected keys before changing their shared argument storage.
    fn migrateGeneratedIteratorRoot(self: *InstGraph, loser: NodeId, winner: NodeId) Allocator.Error!void {
        const occurrences = self.generated_iterators_by_root.get(loser) orelse return;
        if (occurrences.items.len == 0) return;
        const winner_occurrences = try self.generated_iterators_by_root.getOrPut(winner);
        if (!winner_occurrences.found_existing) winner_occurrences.value_ptr.* = .empty;
        try winner_occurrences.value_ptr.ensureUnusedCapacity(self.allocator, occurrences.items.len);
        try self.generated_iterator_index.ensureTotalCapacity(self.generated_iterator_index.count());
        var moved = self.generated_iterators_by_root.fetchRemove(loser).?.value;
        defer moved.deinit(self.allocator);
        for (moved.items) |occurrence| self.unlinkGeneratedIterator(occurrence.node);
        for (moved.items) |occurrence| {
            self.generated_iterator_entries.getPtr(occurrence.node).?.key.args[occurrence.arg_index] = winner;
        }
        self.generated_iterators_by_root.getPtr(winner).?.appendSliceAssumeCapacity(moved.items);
        for (moved.items) |occurrence| {
            if (!self.generated_iterator_entries.get(occurrence.node).?.indexed) self.linkGeneratedIterator(occurrence.node);
        }
    }

    pub fn acceptsRelationMutation(self: *const InstGraph) bool {
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

    /// An immutable segment of the append-only class-member list. Union only
    /// links a class tail to another class head, so no link inside this saved
    /// segment can change. Its last node remains the boundary after any union.
    pub const ArgumentClassSnapshot = struct {
        first: NodeId,
        last: NodeId,

        fn contains(self: ArgumentClassSnapshot, graph: *const InstGraph, node: NodeId) bool {
            var member = self.first;
            while (true) {
                if (member == node) return true;
                if (member == self.last) return false;
                member = graph.class_member_next.items[@intFromEnum(member)] orelse
                    Common.invariant("argument class snapshot lost an interior link");
            }
        }
    };

    /// Retain the exact initial membership without walking or copying the
    /// class. Recursive growth tests permanent node identity against this
    /// bounded segment, independently of later root choices and class joins.
    pub fn snapshotFunctionArgumentClasses(
        self: *InstGraph,
        fn_node: NodeId,
    ) Allocator.Error![]const ArgumentClassSnapshot {
        const args = (try self.functionNodes(fn_node)).args;
        const snapshots = try self.arena().alloc(ArgumentClassSnapshot, args.len);
        for (args, snapshots) |arg, *snapshot| {
            const root = @intFromEnum(self.find(arg));
            snapshot.* = .{
                .first = self.class_member_head.items[root],
                .last = self.class_member_tail.items[root],
            };
            self.countDiagnosticBy("argument_class_members_snapshotted", 2);
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
            if (!initial_class.contains(self, request_arg)) self.markRecursiveValueSlot(request_arg);
        }
        try self.unify(active_fn, recursive_request);
    }

    pub fn markRecursiveValueSlot(self: *InstGraph, slot: NodeId) void {
        self.requireRelationProduction();
        self.recursive_value_slots.items[@intFromEnum(self.find(slot))] = true;
    }

    pub fn isRecursiveValueSlot(self: *InstGraph, node: NodeId) bool {
        return self.recursive_value_slots.items[@intFromEnum(self.find(node))];
    }

    fn markForcedDynamicIteratorRoot(self: *InstGraph, node: NodeId) void {
        self.forced_dynamic_iterator_roots.items[@intFromEnum(self.find(node))] = true;
    }

    /// Record `node`'s finished Monotype and keep its class's first imported
    /// member current. A node that already shares its class may follow a
    /// member that imported earlier, so only then is the class order read.
    fn recordImportedMono(self: *InstGraph, node: NodeId, ty: Type.TypeId) Allocator.Error!void {
        try self.imported_monos.put(node, ty);
        const root = self.find(node);
        if (self.class_member_head.items[@intFromEnum(root)] == node) {
            self.class_imported_monos.items[@intFromEnum(root)] = ty;
            return;
        }
        var members = self.classMemberIterator(root);
        while (members.next()) |member| {
            if (self.imported_monos.get(member)) |first| {
                self.class_imported_monos.items[@intFromEnum(root)] = first;
                return;
            }
        }
    }

    /// The imported Monotype of the first member of `node`'s class that has one.
    fn classImportedMono(self: *InstGraph, node: NodeId) ?Type.TypeId {
        return self.class_imported_monos.items[@intFromEnum(self.find(node))];
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
        if (self.generated_iterator_nodes == 0) return;

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
                .named => |named| named.*,
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
                try self.rewriteGeneratedIteratorAsForcedDynamic(node, &named);
            } else {
                if (item.depth == 0) {
                    Common.invariant("minted iterator representation had zero producer depth");
                }
                named.def.iterator_representation = .minted;
                named.def.iterator_depth = item.depth;
                try self.setContent(node, try self.namedContent(named));
            }
        }
    }

    fn iteratorRootRequiresForcedDynamic(self: *InstGraph, node: NodeId) bool {
        return self.forced_dynamic_iterator_roots.items[@intFromEnum(self.find(node))];
    }

    fn rewriteGeneratedIteratorAsForcedDynamic(
        self: *InstGraph,
        node: NodeId,
        source_named: *const InstNamed,
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
        try self.setContent(node, try self.namedContent(.{
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
            .generated_iterator = try self.generatedIterator(.{
                .callable_evidence = null,
                .public_source = provenance.public_source,
            }),
            .declared_order = provenance.public_source.declared_order,
        }));
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
    /// order cannot affect identity. `typeEql` compares the stamped digest, so
    /// it digests the equivalence `typeEql` implements: requests that are
    /// equal under it mint equal iterator types.
    pub fn finalizeGeneratedIteratorIdentities(self: *InstGraph) Allocator.Error!void {
        self.requireRelationProduction();
        if (self.generated_iterator_nodes == 0) return;
        const Pending = struct { node: NodeId, digest: names.TypeDigest };
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        // A retained import is the request's original representation witness.
        // Producer identity instead describes the current graph after joins.
        var current_shape = GraphTypeFinals.initProvisionalSnapshot(self);
        defer current_shape.deinit();

        for (self.nodes.items, 0..) |_, raw_index| {
            const node = self.find(@enumFromInt(@as(u32, @intCast(raw_index))));
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            const named = switch (self.content(node)) {
                .named => |named| named,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => continue,
            };
            const provenance = named.generated_iterator orelse continue;
            var hasher = TypeDigestHasher.init();
            if (named.def.iterator_representation == .forced_dynamic) {
                if (named.args.len != 1) {
                    Common.invariant("forced-dynamic iterator identity did not have exactly one item argument");
                }
                const item = try current_shape.sealNode(named.args[0]);
                const item_digest = self.types.equalityDigest(self.name_store, peelAliasBacking(self.types, item));
                hasher.update("roc.generated_iterator.forced_dynamic_identity");
                hasher.update(&item_digest.bytes);
            } else {
                const final = try current_shape.sealNode(node);
                const shape = self.types.equalityDigest(self.name_store, peelAliasBacking(self.types, final));
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
                .named => |named| named.*,
                .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("generated iterator identity target stopped being named"),
            };
            named.def.generated = item.digest;
            try self.setContent(item.node, try self.namedContent(named));
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

    /// Node ids are permanent, including recursive placeholders after union.
    /// Evidence columns must always address the same append-only inventory.
    pub fn assertPermanentNode(self: *const InstGraph, node: NodeId) void {
        std.debug.assert(@intFromEnum(node) < self.nodes.items.len);
        std.debug.assert(self.request_source_interfaces.items.len == self.nodes.items.len);
        std.debug.assert(self.constructor_evidence_requests.items.len == self.nodes.items.len);
        std.debug.assert(self.private_backing_roots.items.len == self.nodes.items.len);
        std.debug.assert(self.forced_dynamic_iterator_roots.items.len == self.nodes.items.len);
        std.debug.assert(self.recursive_value_slots.items.len == self.nodes.items.len);
        std.debug.assert(self.class_imported_monos.items.len == self.nodes.items.len);
    }

    pub fn newNode(self: *InstGraph, node_content: InstNode) Allocator.Error!NodeId {
        self.requireRelationProduction();
        const id: NodeId = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        // Reserve every per-node column before appending a permanent identity.
        try self.nodes.ensureUnusedCapacity(self.allocator, 1);
        try self.versions.ensureUnusedCapacity(self.allocator, 1);
        try self.class_member_next.ensureUnusedCapacity(self.allocator, 1);
        try self.class_member_head.ensureUnusedCapacity(self.allocator, 1);
        try self.class_member_tail.ensureUnusedCapacity(self.allocator, 1);
        try self.containment_visit_epochs.ensureUnusedCapacity(self.allocator, 1);
        try self.request_source_interfaces.ensureUnusedCapacity(self.allocator, 1);
        try self.constructor_evidence_requests.ensureUnusedCapacity(self.allocator, 1);
        try self.private_backing_roots.ensureUnusedCapacity(self.allocator, 1);
        try self.forced_dynamic_iterator_roots.ensureUnusedCapacity(self.allocator, 1);
        try self.recursive_value_slots.ensureUnusedCapacity(self.allocator, 1);
        try self.class_imported_monos.ensureUnusedCapacity(self.allocator, 1);
        try self.updateGeneratedIterator(id, node_content);
        if (contentHasGeneratedPrivateBacking(node_content)) self.generated_private_nodes += 1;
        self.nodes.appendAssumeCapacity(node_content);
        self.versions.appendAssumeCapacity(0);
        self.class_member_next.appendAssumeCapacity(null);
        self.class_member_head.appendAssumeCapacity(id);
        self.class_member_tail.appendAssumeCapacity(id);
        self.containment_visit_epochs.appendAssumeCapacity(0);
        self.request_source_interfaces.appendAssumeCapacity(null);
        self.constructor_evidence_requests.appendAssumeCapacity(false);
        self.private_backing_roots.appendAssumeCapacity(false);
        self.forced_dynamic_iterator_roots.appendAssumeCapacity(false);
        self.recursive_value_slots.appendAssumeCapacity(false);
        self.class_imported_monos.appendAssumeCapacity(null);
        self.markPrivateBacking(node_content);
        if (node_content == .named and node_content.named.generated_iterator != null) self.generated_iterator_nodes += 1;
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
        self.countDiagnostic("nominal_backing_lookups");
        const instance_id = self.nominal_backing_index.getAdapted(
            NominalBackingLookup{
                .declaration = .{
                    .module_bytes = module_bytes,
                    .declaration_id = declaration_id,
                },
                .args = args,
            },
            NominalBackingLookupContext{ .graph = self },
        ) orelse return null;
        self.countDiagnostic("nominal_backing_instances_scanned");
        const instance = self.nominal_backing_instances.items[@intFromEnum(instance_id)];
        if (!instance.active) Common.invariant("nominal backing index referenced a retired instance");
        return instance.node;
    }

    pub fn putNominalBackingNode(
        self: *InstGraph,
        module_bytes: [32]u8,
        declaration_id: u32,
        args: []const NodeId,
        node: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const declaration = NominalBackingDeclaration{
            .module_bytes = module_bytes,
            .declaration_id = declaration_id,
        };
        const stored_args = try self.arena().alloc(NodeId, args.len);
        for (stored_args, args) |*stored, arg| {
            stored.* = self.find(arg);
        }
        const key = NominalBackingKey{ .declaration = declaration, .args = stored_args };
        if (self.nominal_backing_index.get(key) != null) {
            Common.invariant("nominal backing insertion duplicated an indexed instantiation");
        }

        const instance_id: NominalBackingInstanceId = @enumFromInt(@as(u32, @intCast(self.nominal_backing_instances.items.len)));
        try self.nominal_backing_instances.append(self.allocator, .{
            .declaration = declaration,
            .args = stored_args,
            .node = node,
            .active = true,
        });
        try self.nominal_backing_migration_epochs.append(self.allocator, 0);
        try self.nominal_backing_index.putNoClobber(key, instance_id);
        for (stored_args, 0..) |root, arg_index| {
            const occurrences = try self.nominal_backings_by_root.getOrPut(root);
            if (!occurrences.found_existing) occurrences.value_ptr.* = .empty;
            try occurrences.value_ptr.append(self.allocator, .{
                .instance = instance_id,
                .arg_index = @intCast(arg_index),
            });
        }
    }

    /// Rewrite every indexed nominal-backing key that mentions `loser`.
    /// Resident hash keys point into mutable instance argument storage, so all
    /// old keys are removed before any argument is changed and all new keys
    /// are restored before collision-driven backing unification can reenter.
    fn migrateNominalBackingRoot(self: *InstGraph, loser: NodeId, winner: NodeId) Allocator.Error!void {
        const loser_occurrences = self.nominal_backings_by_root.getPtr(loser) orelse return;
        const occurrence_count = loser_occurrences.items.len;

        const winner_occurrences = try self.nominal_backings_by_root.getOrPut(winner);
        if (!winner_occurrences.found_existing) winner_occurrences.value_ptr.* = .empty;
        try winner_occurrences.value_ptr.ensureUnusedCapacity(self.allocator, occurrence_count);
        try self.nominal_backing_affected.ensureUnusedCapacity(self.allocator, occurrence_count);
        try self.nominal_backing_collisions.ensureUnusedCapacity(self.allocator, occurrence_count);
        try self.nominal_backing_index.ensureTotalCapacity(self.nominal_backing_index.count());

        var removed_occurrences = self.nominal_backings_by_root.fetchRemove(loser).?;
        defer removed_occurrences.value.deinit(self.allocator);
        const moved = removed_occurrences.value.items;

        self.nominal_backing_affected.clearRetainingCapacity();
        self.nominal_backing_migration_epoch +%= 1;
        if (self.nominal_backing_migration_epoch == 0) {
            @memset(self.nominal_backing_migration_epochs.items, 0);
            self.nominal_backing_migration_epoch = 1;
        }
        const epoch = self.nominal_backing_migration_epoch;
        for (moved) |occurrence| {
            const instance_index = @intFromEnum(occurrence.instance);
            const instance = &self.nominal_backing_instances.items[instance_index];
            if (!instance.active) continue;
            const arg_index = occurrence.arg_index;
            if (arg_index >= instance.args.len) {
                Common.invariant("nominal backing reverse index had an invalid argument position");
            }
            // Retired collisions and root coalescing can leave stale or
            // duplicate occurrences. Only current references move to the
            // winner; every stale occurrence is discarded the first time its
            // recorded root loses, keeping cleanup amortized linear.
            if (instance.args[arg_index] != loser) continue;
            if (self.nominal_backing_migration_epochs.items[instance_index] != epoch) {
                self.nominal_backing_migration_epochs.items[instance_index] = epoch;
                self.nominal_backing_affected.appendAssumeCapacity(occurrence.instance);
            }
        }

        // Removing every old key first prevents one affected instance from
        // colliding with another instance's stale pre-migration key.
        for (self.nominal_backing_affected.items) |instance_id| {
            const instance = &self.nominal_backing_instances.items[@intFromEnum(instance_id)];
            const old_key = NominalBackingKey{
                .declaration = instance.declaration,
                .args = instance.args,
            };
            const removed = self.nominal_backing_index.fetchRemove(old_key) orelse
                Common.invariant("active nominal backing instance was absent from its index");
            self.countNominalBackingIndexRemoval();
            if (removed.value != instance_id) {
                Common.invariant("nominal backing key referenced the wrong instance");
            }
        }

        const current_winner_occurrences = self.nominal_backings_by_root.getPtr(winner).?;
        for (moved) |occurrence| {
            const instance = &self.nominal_backing_instances.items[@intFromEnum(occurrence.instance)];
            if (!instance.active) continue;
            const arg_index = occurrence.arg_index;
            if (instance.args[arg_index] != loser) continue;
            instance.args[arg_index] = winner;
            current_winner_occurrences.appendAssumeCapacity(occurrence);
        }

        for (self.nominal_backing_affected.items) |instance_id| {
            const instance = &self.nominal_backing_instances.items[@intFromEnum(instance_id)];
            if (!instance.active) continue;
            const new_key = NominalBackingKey{
                .declaration = instance.declaration,
                .args = instance.args,
            };
            if (self.nominal_backing_index.get(new_key)) |existing_id| {
                if (existing_id == instance_id) {
                    Common.invariant("nominal backing migration encountered its own indexed key");
                }
                const existing_index = @intFromEnum(existing_id);
                const existing = &self.nominal_backing_instances.items[existing_index];
                if (!existing.active) {
                    Common.invariant("nominal backing index referenced a retired collision");
                }

                const retained_id, const retired_id = if (@intFromEnum(instance_id) < existing_index)
                    .{ instance_id, existing_id }
                else
                    .{ existing_id, instance_id };
                const retained = &self.nominal_backing_instances.items[@intFromEnum(retained_id)];
                const retired = &self.nominal_backing_instances.items[@intFromEnum(retired_id)];
                if (retained_id == instance_id) {
                    const removed = self.nominal_backing_index.fetchRemove(new_key) orelse
                        Common.invariant("colliding nominal backing key disappeared during migration");
                    self.countNominalBackingIndexRemoval();
                    if (removed.value != existing_id) {
                        Common.invariant("colliding nominal backing key referenced the wrong instance");
                    }
                    self.nominal_backing_index.putAssumeCapacityNoClobber(new_key, instance_id);
                }
                retired.active = false;
                self.nominal_backing_collisions.appendAssumeCapacity(.{
                    .left = retained.node,
                    .right = retired.node,
                });
            } else {
                self.nominal_backing_index.putAssumeCapacityNoClobber(new_key, instance_id);
            }
        }
    }

    /// Process cache-key collapses only after migration has restored the
    /// primary and reverse indexes. Nested unions enqueue more pairs for this
    /// same drain instead of reentering it.
    fn drainNominalBackingCollisions(self: *InstGraph) Allocator.Error!void {
        if (self.processing_nominal_backing_collisions) return;
        self.processing_nominal_backing_collisions = true;
        defer self.processing_nominal_backing_collisions = false;
        while (self.nominal_backing_collisions.pop()) |collision| {
            try self.unify(collision.left, collision.right);
        }
    }

    fn find(self: *InstGraph, id: NodeId) NodeId {
        self.countDiagnostic("union_find_resolutions");
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

    fn relatedNamedInstanceRoot(self: *InstGraph, node: NodeId) NodeId {
        var root = node;
        while (self.related_named_instances.get(root)) |entry| {
            if (entry.parent == root) break;
            root = entry.parent;
        }

        var current = node;
        while (current != root) {
            const entry = self.related_named_instances.getPtr(current) orelse break;
            const next = entry.parent;
            entry.parent = root;
            current = next;
        }
        return root;
    }

    fn ensureRelatedNamedInstanceNode(self: *InstGraph, node: NodeId) Allocator.Error!void {
        const entry = try self.related_named_instances.getOrPut(node);
        if (!entry.found_existing) entry.value_ptr.* = .{ .parent = node };
    }

    fn unionRelatedNamedInstanceNodes(self: *InstGraph, left_node: NodeId, right_node: NodeId) void {
        var left_root = self.relatedNamedInstanceRoot(left_node);
        var right_root = self.relatedNamedInstanceRoot(right_node);
        if (left_root == right_root) return;
        if (self.related_named_instances.get(left_root).?.rank <
            self.related_named_instances.get(right_root).?.rank)
        {
            const temp = left_root;
            left_root = right_root;
            right_root = temp;
        }
        self.related_named_instances.getPtr(right_root).?.parent = left_root;
        const left_rank = self.related_named_instances.get(left_root).?.rank;
        if (left_rank == self.related_named_instances.get(right_root).?.rank) {
            self.related_named_instances.getPtr(left_root).?.rank = left_rank + 1;
        }
    }

    fn bindRelatedNamedBacking(
        self: *InstGraph,
        named_node: NodeId,
        named: *const InstNamed,
    ) Allocator.Error!void {
        const backing = named.backing orelse return;
        const entry = try self.related_named_backings.getOrPut(backing.node);
        if (entry.found_existing) {
            self.unionRelatedNamedInstanceNodes(named_node, entry.value_ptr.*);
        } else {
            entry.value_ptr.* = named_node;
        }
    }

    /// Record an exact nominal identity proved by a graph relation. Backed
    /// named applications intentionally keep distinct main type classes so
    /// each request retains its own representation witness; this parallel
    /// union-find exposes the proven source-level identity to later consumers.
    pub fn relateNamedInstances(
        self: *InstGraph,
        raw_left: NodeId,
        raw_right: NodeId,
    ) Allocator.Error!void {
        self.requireRelationProduction();
        const left_node = self.find(raw_left);
        const right_node = self.find(raw_right);
        const left = switch (self.content(left_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("named-instance relation received a non-named left node"),
        };
        const right = switch (self.content(right_node)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => Common.invariant("named-instance relation received a non-named right node"),
        };
        if (left.kind != right.kind or
            !sameTypeDef(left.def, right.def) or
            left.builtin_owner != right.builtin_owner or
            left.args.len != right.args.len)
        {
            Common.invariant("named-instance relation received different declarations");
        }

        try self.ensureRelatedNamedInstanceNode(left_node);
        try self.ensureRelatedNamedInstanceNode(right_node);
        try self.bindRelatedNamedBacking(left_node, left);
        try self.bindRelatedNamedBacking(right_node, right);
        self.unionRelatedNamedInstanceNodes(left_node, right_node);
    }

    /// Whether the main type relation or a backed-nominal relation proved
    /// that two named application cells have the same source-level identity.
    pub fn sameRelatedNamedInstance(self: *InstGraph, raw_left: NodeId, raw_right: NodeId) bool {
        const left = self.find(raw_left);
        const right = self.find(raw_right);
        const left_named = switch (self.content(left)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => return false,
        };
        const right_named = switch (self.content(right)) {
            .named => |named| named,
            .redirect, .unresolved, .primitive, .list, .box, .tuple, .func, .tag_union, .record, .empty_tag_union, .empty_record, .erased, .zst => return false,
        };
        if (left_named.kind != right_named.kind or
            !sameTypeDef(left_named.def, right_named.def) or
            left_named.builtin_owner != right_named.builtin_owner or
            left_named.args.len != right_named.args.len) return false;
        if (self.sameClass(left, right)) return true;

        const left_related = if (self.related_named_instances.contains(left))
            self.relatedNamedInstanceRoot(left)
        else if (left_named.backing) |backing|
            if (self.related_named_backings.get(backing.node)) |representative|
                self.relatedNamedInstanceRoot(representative)
            else
                return false
        else
            return false;
        const right_related = if (self.related_named_instances.contains(right))
            self.relatedNamedInstanceRoot(right)
        else if (right_named.backing) |backing|
            if (self.related_named_backings.get(backing.node)) |representative|
                self.relatedNamedInstanceRoot(representative)
            else
                return false
        else
            return false;
        return left_related == right_related;
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
        if (self.rootStampedResolved(root)) return true;
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        try pending.append(self.allocator, root);
        while (pending.pop()) |raw_node| {
            const node = self.find(raw_node);
            const entry = try seen.getOrPut(node);
            if (entry.found_existing) continue;
            // A stamped class is resolved throughout, so nothing below it can
            // change the answer.
            if (self.rootStampedResolved(node)) continue;
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
        if (!allow_field_kind_defaults) {
            // Every class the strict walk visited is resolved throughout.
            var visited = seen.keyIterator();
            while (visited.next()) |node| {
                try self.resolved_roots.put(node.*, self.resolved_epoch);
            }
        }
        return true;
    }

    /// Whether the node's class was stamped resolved at the current epoch.
    fn rootStampedResolved(self: *InstGraph, node: NodeId) bool {
        const stamp = self.resolved_roots.get(self.find(node)) orelse return false;
        return stamp == self.resolved_epoch;
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
        if (self.generated_private_nodes == 0) {
            self.countDiagnostic("generated_private_guard_returns");
            return false;
        }
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
        if (entry.valid and entry.verified_epoch != self.structure_epoch) {
            if (self.containmentQueryCacheValid(entry)) {
                entry.verified_epoch = self.structure_epoch;
            } else {
                entry.valid = false;
                entry.result = false;
                entry.dependencies.clearRetainingCapacity();
            }
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
                        entry.verified_epoch = self.structure_epoch;
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
        entry.verified_epoch = self.structure_epoch;
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
            if (self.classImportedMono(node) != null) return true;
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
            // Not `invariant`: a closed tag union that grows here changes the tag
            // discriminants the backend emits, so a release build would silently
            // read the wrong variant instead of hitting undefined behavior.
            Common.compilerBug("opaque interface relation widened a closed tag union");
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
        private_named: *const InstNamed,
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
        public_named: *const InstNamed,
        private_named: *const InstNamed,
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
        private_named: *const InstNamed,
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
        private_named: *const InstNamed,
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
        try self.setContent(public_node, try self.namedContent(.{
            .named_type = public_source.named_type,
            .def = public_source.def,
            .kind = public_source.kind,
            .builtin_owner = public_source.builtin_owner,
            .args = args,
            .backing = public_source.backing,
            .generated_iterator = null,
            .declared_order = public_source.declared_order,
        }));
    }

    fn materializeNamedRequestPublicInterface(
        self: *InstGraph,
        public_node: NodeId,
        public_var: InstVariable,
        private_named: *const InstNamed,
    ) Allocator.Error!*const InstNamed {
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
        try self.setContent(public_node, try self.namedContent(.{
            .named_type = private_named.named_type,
            .def = private_named.def,
            .kind = private_named.kind,
            .builtin_owner = private_named.builtin_owner,
            .args = args,
            .backing = private_named.backing,
            .generated_iterator = null,
            .declared_order = private_named.declared_order,
        }));
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

    /// Distinct current equivalence classes in a function's explicit
    /// interface. The caller must not merge classes during iteration; adding
    /// independent nodes and compressing find paths does not change the set.
    /// Each iterator owns its scratch until deinit, so overlapping
    /// queries on the same graph cannot clear one another's visited classes.
    pub const FunctionInterfaceClassIterator = struct {
        graph: *InstGraph,
        interface: FunctionInterfaceIterator,
        seen: collections.DenseMap(NodeId, void),

        pub fn deinit(self: *FunctionInterfaceClassIterator) void {
            self.graph.node_set_pool.release(&self.seen);
            self.* = undefined;
        }

        pub fn next(self: *FunctionInterfaceClassIterator) Allocator.Error!?NodeId {
            while (self.interface.next()) |node| {
                const root = self.graph.find(node);
                if ((try self.seen.getOrPut(root)).found_existing) continue;
                return root;
            }
            return null;
        }
    };

    /// Lookup may probe each class once, but persistent indexes must retain
    /// permanent node IDs through functionInterfaceIterator: later unions can
    /// change which class owns an indexed node.
    pub fn functionInterfaceClassIterator(self: *InstGraph, node: NodeId) Allocator.Error!FunctionInterfaceClassIterator {
        const interface = try self.functionInterfaceIterator(node);
        return .{
            .graph = self,
            .interface = interface,
            .seen = self.node_set_pool.acquire(),
        };
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

    /// Normalize a structural tag row before a relation producer reads its
    /// head and residual extension through `content`. Value consumers use
    /// `tagRowNodes`, which keeps the extension internal to the graph.
    pub fn normalizeTagRow(self: *InstGraph, row: NodeId) Allocator.Error!void {
        _ = try self.flattenTagRow(row);
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
        var named = named_content.named.*;
        const backing = named.backing orelse
            Common.invariant("named value witness had no checked backing");
        named.backing = .{
            .node = backing_node,
            .use = backing.use,
            .authority = backing.authority,
        };
        return self.newNode(try self.namedContent(named));
    }

    /// Return the graph node for one field of a record-shaped node. Field
    /// access is a type relation, so callers use this node directly instead of
    /// selecting a field from a temporary Monotype view and losing later row
    /// evidence.
    pub fn recordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        return self.recordFieldNodeWithAccess(raw_record, name, .inspectable, "record field access");
    }

    /// Follow a checked field-value type without selecting its storage kind.
    /// Evidence paths address this source value even while the runtime slot
    /// remains unresolved or carries an optional field's presence tag.
    pub fn recordFieldValueNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        const field = try self.recordFieldWithAccess(raw_record, name, .inspectable, "record field access");
        const value = switch (field.kind) {
            .required, .defaulted => blk: {
                if (field.value_ty != null) Common.invariant("inline record field carried a separate source value node");
                break :blk field.ty;
            },
            .optional, .undetermined => field.value_ty orelse
                Common.invariant("record field carried no source value node"),
            // Sealed Monotypes retain source-value metadata for optional slots;
            // its absence explicitly denotes an inline source value.
            .sealed => field.value_ty orelse field.ty,
        };
        return self.find(value);
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

    /// Whether the class is unresolved by inspection of its root alone: a
    /// variable, or a row whose extension is a variable. No active snapshot
    /// reaches such a class, so replacing its content cannot stale one.
    fn classProvablyUnresolved(self: *InstGraph, root: NodeId) bool {
        return switch (self.nodes.items[@intFromEnum(root)]) {
            .unresolved => true,
            .tag_union => |row| self.nodes.items[@intFromEnum(self.find(row.ext))] == .unresolved,
            .record => |row| self.nodes.items[@intFromEnum(self.find(row.ext))] == .unresolved,
            .redirect, .primitive, .list, .box, .tuple, .func, .empty_tag_union, .empty_record, .named, .erased, .zst => false,
        };
    }

    fn refreshActiveSnapshots(self: *InstGraph) void {
        if (!self.current_snapshots_dirty) return;
        self.current_snapshots.clearRetainingCapacity();
        self.current_durable.clearRetainingCapacity();
        self.current_snapshots_dirty = false;
    }

    /// Whether the node's class has a current active snapshot. Snapshots are
    /// taken only from resolved roots, resolvedness survives every join, and
    /// the cache is cleared by every observable content change, so a current
    /// snapshot proves the class is still resolved without walking it.
    fn hasCurrentSnapshot(self: *InstGraph, node: NodeId) bool {
        self.refreshActiveSnapshots();
        return self.current_snapshots.contains(self.find(node));
    }

    /// Redirect `loser` into `winner` and invalidate the current snapshot
    /// cache. Immutable snapshot provenance remains attached to permanent node
    /// ids and resolves through `find`.
    fn union_(self: *InstGraph, raw_winner: NodeId, raw_loser: NodeId) Allocator.Error!void {
        const winner = self.find(raw_winner);
        const loser = self.find(raw_loser);
        if (winner == loser) return;
        if (self.nodes.items[@intFromEnum(winner)] == .unresolved and self.nodes.items[@intFromEnum(loser)] != .unresolved) {
            // A variable absorbing concrete content can make classes that
            // reached the loser unresolved.
            self.resolved_epoch +%= 1;
        }
        try self.migrateNominalBackingRoot(loser, winner);
        // Nominal identity is attached to a type class, not whichever raw cell
        // happened to represent it when a checked/request relation was recorded.
        if (self.related_named_instances.contains(winner) or self.related_named_instances.contains(loser)) {
            try self.ensureRelatedNamedInstanceNode(winner);
            try self.ensureRelatedNamedInstanceNode(loser);
            self.unionRelatedNamedInstanceNodes(winner, loser);
        }
        try self.migrateGeneratedIteratorRoot(loser, winner);
        self.removeGeneratedIterator(loser);
        const winner_tail = self.class_member_tail.items[@intFromEnum(winner)];
        const loser_head = self.class_member_head.items[@intFromEnum(loser)];
        self.class_member_next.items[@intFromEnum(winner_tail)] = loser_head;
        self.class_member_tail.items[@intFromEnum(winner)] = self.class_member_tail.items[@intFromEnum(loser)];
        const winner_content = self.nodes.items[@intFromEnum(winner)];
        const loser_content = self.nodes.items[@intFromEnum(loser)];
        self.private_backing_roots.items[@intFromEnum(winner)] = self.private_backing_roots.items[@intFromEnum(winner)] or self.private_backing_roots.items[@intFromEnum(loser)];
        self.forced_dynamic_iterator_roots.items[@intFromEnum(winner)] = self.forced_dynamic_iterator_roots.items[@intFromEnum(winner)] or self.forced_dynamic_iterator_roots.items[@intFromEnum(loser)];
        self.recursive_value_slots.items[@intFromEnum(winner)] = self.recursive_value_slots.items[@intFromEnum(winner)] or self.recursive_value_slots.items[@intFromEnum(loser)];
        if (self.class_imported_monos.items[@intFromEnum(winner)] == null) {
            self.class_imported_monos.items[@intFromEnum(winner)] = self.class_imported_monos.items[@intFromEnum(loser)];
        }
        self.constructor_evidence_requests.items[@intFromEnum(winner)] =
            self.constructor_evidence_requests.items[@intFromEnum(winner)] or
            self.constructor_evidence_requests.items[@intFromEnum(loser)];
        const joins_nominal_with_structural = winner_content != .unresolved and loser_content != .unresolved and
            (winner_content == .named) != (loser_content == .named);
        const joins_iterator_representations = winner_content == .named and loser_content == .named and
            self.iteratorRelation(winner_content.named, loser_content.named) != .ordinary;
        self.nodes.items[@intFromEnum(loser)] = .{ .redirect = winner };
        self.versions.items[@intFromEnum(winner)] +%= 1;
        self.structure_epoch +%= 1;
        self.countDiagnostic("class_unions");
        // An active snapshot exists only for a resolved class and reaches
        // only resolved classes. A variable never wins a join, and two
        // resolved classes ordinarily unify equal content. Nominal/structural
        // joins and explicit iterator representation joins can instead select
        // different content, invalidating snapshots through either class.
        if (joins_nominal_with_structural or joins_iterator_representations) {
            self.invalidateActiveSnapshots(winner);
        } else {
            // The joined class keeps its current view under its new root.
            self.refreshActiveSnapshots();
            if (self.current_snapshots.get(loser)) |view| {
                if (!self.current_snapshots.contains(winner)) try self.current_snapshots.put(winner, view);
            }
            if (self.current_durable.get(loser)) |durable| {
                if (!self.current_durable.contains(winner)) try self.current_durable.put(winner, durable);
            }
        }
        try self.drainNominalBackingCollisions();
    }

    /// Replace a root's content with an observationally equivalent compressed
    /// form without invalidating snapshots or resolvedness stamps: the class
    /// still denotes the same type and reaches the same resolved state.
    /// Returns whether the stored graph content changed.
    fn markPrivateBacking(self: *InstGraph, node_content: InstNode) void {
        if (node_content == .named) if (node_content.named.backing) |backing| {
            if (backing.authority == .generated_private) self.private_backing_roots.items[@intFromEnum(self.find(backing.node))] = true;
        };
    }

    fn replaceContentWithoutSnapshotInvalidation(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!bool {
        const root = self.find(raw_root);
        if (instNodeEql(self.nodes.items[@intFromEnum(root)], new_content)) return false;
        try self.updateGeneratedIterator(root, new_content);
        if (new_content == .named and new_content.named.generated_iterator != null) self.generated_iterator_nodes += 1;
        if (contentHasGeneratedPrivateBacking(new_content)) self.generated_private_nodes += 1;
        self.nodes.items[@intFromEnum(root)] = new_content;
        self.markPrivateBacking(new_content);
        self.versions.items[@intFromEnum(root)] +%= 1;
        self.structure_epoch +%= 1;
        return true;
    }

    /// Replace a root's type content and invalidate every cached snapshot
    /// that could observe the class.
    fn setContent(self: *InstGraph, root: NodeId, new_content: InstNode) Allocator.Error!void {
        const observable = !self.classProvablyUnresolved(self.find(root));
        if (!try self.replaceContentWithoutSnapshotInvalidation(root, new_content)) return;
        if (observable) {
            self.invalidateActiveSnapshots(root);
            // The new content can reach unresolved nodes that classes
            // stamped resolved through this one never saw.
            self.resolved_epoch +%= 1;
        }
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

                            if (self.isRecursiveValueSlot(left) or self.isRecursiveValueSlot(right)) {
                                self.markForcedDynamicIteratorRoot(left);
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

    fn iteratorRelation(self: *InstGraph, left: *const InstNamed, right: *const InstNamed) Type.IteratorRelation {
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
            var compressed = named.*;
            compressed.backing = .{ .node = backing_node, .use = declared_backing.use, .authority = declared_backing.authority };
            _ = try self.replaceContentWithoutSnapshotInvalidation(named_node, try self.namedContent(compressed));
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

    fn structuralBackingNode(self: *InstGraph, raw: NodeId, owner: *const InstNamed) Allocator.Error!StructuralBacking {
        const result = try self.findStructuralBackingNode(raw, owner);
        if (!result.recursive) {
            try self.compressStructuralBacking(raw, owner, result.node);
        }
        return result;
    }

    fn findStructuralBackingNode(self: *InstGraph, raw: NodeId, owner: *const InstNamed) Allocator.Error!StructuralBacking {
        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        var current = self.find(raw);
        while (true) {
            self.countDiagnostic("structural_backing_scan_slots");
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) return .{ .node = current, .recursive = true };
            const next = self.structuralBackingNext(current, owner) orelse return .{ .node = current, .recursive = false };
            current = next;
        }
    }

    fn compressStructuralBacking(self: *InstGraph, raw: NodeId, owner: *const InstNamed, result: NodeId) Allocator.Error!void {
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
                var compressed = named.*;
                compressed.backing = .{ .node = result, .use = backing.use, .authority = backing.authority };
                _ = try self.replaceContentWithoutSnapshotInvalidation(current, try self.namedContent(compressed));
            }
            current = next;
        }
    }

    fn structuralBackingNext(self: *InstGraph, raw: NodeId, owner: *const InstNamed) ?NodeId {
        const current = self.find(raw);
        const node_content = self.nodes.items[@intFromEnum(current)];
        if (node_content != .named) return null;
        const named = node_content.named;
        if (named.kind != .alias and !self.sameNamedInstance(named, owner)) return null;
        const backing = named.backing orelse
            Common.invariant("named backing chain reached a named type without backing");
        return self.find(backing.node);
    }

    fn sameNamedInstance(self: *InstGraph, left: *const InstNamed, right: *const InstNamed) bool {
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

    /// Normalize only a head that a row reader actually consumes. This state
    /// changes neither type meaning nor snapshot dependencies, so recording it
    /// does not invalidate relation stamps or observable graph snapshots.
    fn sortTagHead(self: *InstGraph, root: NodeId) void {
        const row = &self.nodes.items[@intFromEnum(root)].tag_union;
        if (row.tags_sorted) return;
        const tags = row.tags;
        if (!std.sort.isSorted(InstTag, tags, self.name_store, instTagLessThan)) {
            std.mem.sortUnstable(InstTag, tags, self.name_store, instTagLessThan);
        }
        for (tags, 0..) |tag, index| {
            if (index != 0) std.debug.assert(tags[index - 1].name != tag.name);
        }
        row.tags_sorted = true;
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
        var ext = self.find(row.ext);
        const ext_content = self.nodes.items[@intFromEnum(ext)];
        if (ext_content == .unresolved or ext_content == .empty_tag_union) {
            self.sortTagHead(root);
            if (row.ext != ext) {
                const flattened: InstNode = .{ .tag_union = .{ .tags = row.tags, .ext = ext, .tags_sorted = true } };
                _ = try self.replaceContentWithoutSnapshotInvalidation(root, flattened);
            }
            return .{ .tags = row.tags, .ext = ext };
        }

        var tags = std.ArrayList(InstTag).empty;
        defer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, row.tags);

        var seen = self.node_set_pool.acquire();
        defer self.node_set_pool.release(&seen);
        try seen.put(root, {});

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

        // Gather the whole chain before sorting: repeatedly merging a growing
        // prefix makes a chain of singleton rows quadratic. Stable sorting
        // preserves the checker's head-before-extension payload precedence.
        std.mem.sort(InstTag, tags.items, self.name_store, instTagLessThan);
        var unique: usize = 0;
        for (tags.items) |tag| {
            if (unique != 0 and tags.items[unique - 1].name == tag.name) continue;
            tags.items[unique] = tag;
            unique += 1;
        }
        const flat_tags = try self.arena().dupe(InstTag, tags.items[0..unique]);
        const flattened: InstNode = .{ .tag_union = .{ .tags = flat_tags, .ext = ext, .tags_sorted = true } };
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

        // Flattened rows are sorted and unique. Partition
        // both spans in one pass without building per-relation label indexes.
        try merged.ensureTotalCapacity(self.allocator, flat_left.tags.len + flat_right.tags.len);
        var left_index: usize = 0;
        var right_index: usize = 0;
        while (left_index < flat_left.tags.len and right_index < flat_right.tags.len) {
            const left_tag = flat_left.tags[left_index];
            const right_tag = flat_right.tags[right_index];
            if (left_tag.name == right_tag.name) {
                if (left_tag.payloads.len != right_tag.payloads.len) {
                    Common.invariant("instantiation unified one tag at two different payload arities");
                }
                for (left_tag.payloads, right_tag.payloads) |left_payload, right_payload| {
                    try pending.append(self.allocator, .{ .left = left_payload, .right = right_payload, .row_width = row_width });
                }
                merged.appendAssumeCapacity(left_tag);
                left_index += 1;
                right_index += 1;
            } else if (instTagLessThan(self.name_store, left_tag, right_tag)) {
                merged.appendAssumeCapacity(left_tag);
                try only_left.append(self.allocator, left_tag);
                left_index += 1;
            } else {
                merged.appendAssumeCapacity(right_tag);
                try only_right.append(self.allocator, right_tag);
                right_index += 1;
            }
        }
        merged.appendSliceAssumeCapacity(flat_left.tags[left_index..]);
        merged.appendSliceAssumeCapacity(flat_right.tags[right_index..]);
        try only_left.appendSlice(self.allocator, flat_left.tags[left_index..]);
        try only_right.appendSlice(self.allocator, flat_right.tags[right_index..]);

        if (self.rowAdditionConflicts(flat_left.ext, only_right.items.len, .tag_union) or
            self.rowAdditionConflicts(flat_right.ext, only_left.items.len, .tag_union))
        {
            // Not `invariant`: see the opaque-interface check above. Widening a
            // closed tag union renumbers its discriminants, which changes emitted
            // code, so this has to hold in release builds too.
            Common.compilerBug("instantiation widened a closed tag union");
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
            .tags_sorted = true,
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

        // Both rows indexed by label text so each side pairs with the other
        // in one pass; the first row position wins for a repeated label.
        var right_by_text: std.StringHashMapUnmanaged(usize) = .empty;
        defer right_by_text.deinit(self.allocator);
        try right_by_text.ensureTotalCapacity(self.allocator, @intCast(flat_right.fields.len));
        for (flat_right.fields, 0..) |right_field, index| {
            const gop = right_by_text.getOrPutAssumeCapacity(self.fieldLabelText(right_field.name));
            if (!gop.found_existing) gop.value_ptr.* = index;
        }
        var left_texts: std.StringHashMapUnmanaged(void) = .empty;
        defer left_texts.deinit(self.allocator);
        try left_texts.ensureTotalCapacity(self.allocator, @intCast(flat_left.fields.len));
        for (flat_left.fields) |left_field| {
            left_texts.putAssumeCapacity(self.fieldLabelText(left_field.name), {});
        }

        for (flat_left.fields) |left_field| {
            var shared = false;
            if (right_by_text.get(self.fieldLabelText(left_field.name))) |right_index| {
                const right_field = flat_right.fields[right_index];
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
            }
            if (!shared) {
                try merged.append(self.allocator, left_field);
                try only_left.append(self.allocator, left_field);
            }
        }
        for (flat_right.fields) |right_field| {
            if (left_texts.contains(self.fieldLabelText(right_field.name))) continue;
            try merged.append(self.allocator, right_field);
            try only_right.append(self.allocator, right_field);
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

    /// Import one independent occurrence of a finished Monotype. Internal
    /// sharing and recursion are preserved by the import-local memo, while a
    /// second root import receives distinct mutable solver nodes. Active graph
    /// snapshots reconnect to their existing nodes.
    pub fn importMono(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        self.countDiagnostic("mono_import_requests");
        if (self.active_snapshot_nodes.get(ty)) |existing| {
            self.countDiagnostic("mono_import_hits");
            return self.find(existing);
        }
        if (self.imported_type_nodes.get(ty)) |existing| {
            self.countDiagnostic("mono_import_hits");
            return self.find(existing);
        }
        self.countDiagnostic("mono_import_misses");
        return try self.importMonoInner(ty, null);
    }

    /// Import a finished Monotype root as a distinct occurrence. Descendants
    /// reconnect through the ownership-scope memo, while a recursive edge back
    /// to the root reconnects through the import-local memo.
    pub fn importMonoIndependent(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        self.requireRelationProduction();
        var imported = collections.DenseMap(Type.TypeId, NodeId).init(self.allocator);
        defer imported.deinit();
        return try self.importMonoInner(ty, &imported);
    }

    fn importMonoInner(
        self: *InstGraph,
        ty: Type.TypeId,
        imported_types: ?*collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error!NodeId {
        if (imported_types) |local| {
            if (local.get(ty)) |existing| return existing;
            if (local.count() != 0) {
                // Only the explicitly requested occurrence root is
                // independent. Its component types still carry the
                // ownership-scope identity used by checked constructor slots
                // and their evidence.
                return try self.importMonoInner(ty, null);
            }
        } else if (self.imported_type_nodes.get(ty)) |existing| {
            return self.find(existing);
        }
        const node = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        // One-way memo: every import is a finished Monotype from outside this
        // graph (ids materialized here hit the memo above), so it enters as a
        // snapshot. Registering a view would let this specialization's
        // evidence rewrite another specialization's final type, destabilizing
        // every digest taken from it.
        if (imported_types) |local| {
            try local.put(ty, node);
        } else {
            try self.imported_type_nodes.put(ty, node);
        }
        try self.recordImportedMono(node, ty);

        const types = self.types;
        const imported: InstNode = switch (types.get(ty)) {
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.importMonoInner(elem, imported_types) },
            .box => |elem| .{ .box = try self.importMonoInner(elem, imported_types) },
            .tuple => |items| .{ .tuple = try self.importMonoSlice(types.span(items), imported_types) },
            .func => |func| .{ .func = .{
                .args = try self.importMonoSlice(types.span(func.args), imported_types),
                .ret = try self.importMonoInner(func.ret, imported_types),
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
                        .payloads = try self.importMonoSlice(types.span(tag.payloads), imported_types),
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
                        try self.importMonoInner(value_ty, imported_types)
                    else
                        null;
                    inst_fields[index] = .{
                        .name = field.name,
                        .ty = try self.importMonoInner(field.ty, imported_types),
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
            .named => |named| try self.namedContent(.{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try self.importMonoSlice(types.span(named.args), imported_types),
                .backing = if (named.backing) |backing| .{
                    .node = try self.importMonoInner(backing.ty, imported_types),
                    .use = backing.use,
                    .authority = backing.authority,
                } else null,
                .declared_order = try self.importDeclaredFields(named.declared_order, imported_types),
            }),
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentWithoutSnapshotInvalidation(node, imported);
        return node;
    }

    fn importMonoSlice(
        self: *InstGraph,
        tys: anytype,
        imported_types: ?*collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (0..tys.len) |index| {
            const ty = GuardedList.at(tys, index);
            out[index] = try self.importMonoInner(ty, imported_types);
        }
        return out;
    }

    fn importDeclaredFields(
        self: *InstGraph,
        span: Type.Span,
        imported_types: ?*collections.DenseMap(Type.TypeId, NodeId),
    ) Allocator.Error![]const InstDeclaredField {
        const fields = self.types.declaredFieldSpan(span);
        if (fields.len == 0) return &.{};
        const out = try self.arena().alloc(InstDeclaredField, fields.len);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            out[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.importMonoInner(ty, imported_types) },
            };
        }
        return out;
    }

    /// Materialize an immutable Monotype-shaped view of a node under the
    /// relations produced so far, applying defaults to unresolved leaves in
    /// the view only. The live graph is unchanged. This is collision authority
    /// for finalization probes; interface-replay identities must instead retain
    /// explicit unresolved constraints. The
    /// returned graph-owned scratch TypeId must not be emitted as output.
    pub fn provisionalTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        if (try self.settledTypeViewForNode(node)) |settled| return settled;
        var snapshot = GraphTypeFinals.initProvisionalSnapshot(self);
        defer snapshot.deinit();
        defer self.countDiagnosticBy("provisional_snapshot_nodes_materialized", snapshot.sealed.count());
        return try snapshot.sealNode(self.find(node));
    }

    /// Materialize a read-only specialization-key view of a graph type whose
    /// only open evidence is generalized field presence. Undetermined fields
    /// take their relation-freeze default (`required`) in this view, while the
    /// live field-kind cells remain open for subsequent graph relations.
    pub fn specializationTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        if (try self.settledTypeViewForNode(node)) |settled| return settled;
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
        self.countDiagnostic("active_type_requests");
        return (try self.settledTypeViewForNode(node)) orelse
            Common.invariant("active Monotype TypeId requested for an unresolved instantiation graph node");
    }

    /// The type a node with nothing left to default reads as: the durable
    /// Monotype it was imported from, or the snapshot of its resolved class.
    /// Every read-only view of a node starts here, so the active,
    /// provisional, and specialization views agree on every settled node
    /// and differ only in which defaults they apply to an open one.
    fn settledTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!?Type.TypeId {
        self.requireRelationProduction();
        if (self.types.hasSpeculativeConstruction()) {
            Common.compilerBug("Monotype type view requested inside a type transaction");
        }
        if (self.imported_monos.get(node)) |imported| {
            self.countDiagnostic("imported_type_view_hits");
            return imported;
        }
        if (self.hasCurrentSnapshot(node) or try self.typeIsResolved(node)) return try self.monoFor(node);
        return null;
    }

    fn monoFor(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        const root = self.find(node);
        // A current snapshot was taken from a resolved root, and the cache is
        // cleared by every union and content change, so a hit is still
        // resolved without walking the type again.
        self.refreshActiveSnapshots();
        if (self.current_snapshots.get(root)) |current| {
            self.countDiagnostic("active_snapshot_cache_hits");
            return current;
        }
        self.countDiagnostic("active_snapshot_cache_misses");
        if (!try self.typeIsResolved(root)) {
            Common.invariant("immutable Monotype snapshot requested for an unresolved instantiation graph node");
        }

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
            try self.active_snapshot_nodes.put(snapshot_ty, snapshot_node);
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
        if (self.snapshot_free_types.contains(ty)) return false;
        var seen = self.type_set_pool.acquire();
        defer self.type_set_pool.release(&seen);
        if (try self.typeContainsActiveSnapshot(ty, &seen)) return true;
        // Every type the walk reached is snapshot-free as well.
        var visited = seen.keyIterator();
        while (visited.next()) |visited_ty| {
            try self.snapshot_free_types.put(visited_ty.*, {});
        }
        return false;
    }

    fn typeContainsActiveSnapshot(
        self: *InstGraph,
        ty: Type.TypeId,
        seen: *collections.DenseMap(Type.TypeId, void),
    ) Allocator.Error!bool {
        if (self.isActiveSnapshotType(ty)) return true;
        if (self.snapshot_free_types.contains(ty)) return false;
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
        const raw_node = self.active_snapshot_nodes.get(ty) orelse return false;
        const views = self.node_snapshots.get(raw_node) orelse return false;
        for (views.items) |view| {
            if (view == ty) return true;
        }
        return false;
    }

    /// Return the current root node for a TypeId that is one of this graph's
    /// immutable active snapshots. Closed imported TypeIds return null.
    pub fn activeSnapshotNode(self: *InstGraph, ty: Type.TypeId) ?NodeId {
        const raw_node = self.active_snapshot_nodes.get(ty) orelse return null;
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
        settled_interface,
        active_snapshot,
        provisional_snapshot,
        specialization_snapshot,
        retained_type_view,
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

    /// Capture has established that these nodes contain no open cells or
    /// request-local evidence. Intern them directly, without retaining an
    /// intermediate active snapshot or finalizing the surrounding graph.
    fn initSettledInterface(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph, .settled_interface);
    }

    /// Intern immutable view content without consulting its former live graph
    /// cells. This preserves provisional field kinds and unresolved leaves.
    pub fn initRetainedTypeView(graph: *InstGraph) GraphTypeFinals {
        graph.requireRelationProduction();
        return initUnchecked(graph, .retained_type_view);
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
        if (self.mode == .retained_type_view) {
            if (self.sealed_types.get(ty)) |existing| return existing;
            if (try self.graph.types.isInterned(self.graph.name_store, ty)) return ty;
            return try self.sealStoreType(ty);
        }
        if (self.graph.active_snapshot_nodes.get(ty)) |raw_node| {
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
        std.debug.assert(self.mode != .retained_type_view);
        const node = self.graph.find(raw_node);
        if (self.sealed.get(node)) |existing| return existing;
        self.graph.refreshActiveSnapshots();
        if (self.mode != .final and self.mode != .settled_interface) {
            // A class with a current active snapshot has not changed since
            // that view was taken, so a snapshot reaching it reads that view.
            if (self.graph.current_snapshots.get(node)) |current| return current;
            if (self.mode == .provisional_snapshot or self.mode == .specialization_snapshot) {
                std.debug.assert(self.graph.types.active_transaction == null);
                if (try self.graph.typeIsResolved(node)) return try self.graph.monoFor(node);
            }
            return try self.sealNodeSpeculative(node);
        }
        // A class sealed to an interned type since its last observable
        // change still denotes that type.
        if (self.graph.current_durable.get(node)) |durable| return durable;
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
        try self.remapSealedTypes(result);
        return result.root;
    }

    fn sealNodeSpeculative(self: *GraphTypeFinals, node: NodeId) Allocator.Error!Type.TypeId {
        if (self.sealed.get(node)) |existing| return existing;
        if (self.mode == .final or self.mode == .settled_interface) {
            if (self.graph.current_durable.get(node)) |durable| return durable;
        }
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

    fn remapSealedTypes(self: *GraphTypeFinals, result: Type.Store.TransactionResult) Allocator.Error!void {
        for (self.transaction_sealed_nodes.items) |node| {
            const entry = self.sealed.getPtr(node) orelse
                Common.compilerBug("transaction-sealed node was missing from the sealed map at commit");
            entry.* = result.remapType(entry.*);
            try self.graph.current_durable.put(self.graph.find(node), entry.*);
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
            .unresolved => |variable| if (self.mode == .settled_interface)
                Common.invariant("open cell reached settled interface interning")
            else
                materializeUnresolved(variable),
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
        if (self.mode != .final and self.mode != .settled_interface and self.mode != .retained_type_view) return try self.sealStoreTypeSpeculative(ty);
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
        try self.remapSealedTypes(result);
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
                    .retained_type_view => unreachable,
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
                    .final, .settled_interface, .active_snapshot => Common.invariant("unresolved record field kind reached Monotype sealing"),
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
        return try self.graph.types.addSortedTagVariants(self.graph.name_store, tags);
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
    hasher: TypeDigestHasher,
    unresolved_ids: collections.DenseMap(NodeId, u32),
    visiting: std.ArrayList(NodeId),
    next_unresolved: u32 = 0,
    output: ?[]u8 = null,
    output_len: usize = 0,

    fn init(graph: *InstGraph) OpenFunctionInterfaceShapeWriter {
        return .{
            .graph = graph,
            .hasher = TypeDigestHasher.init(),
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
        self.writeBytes("roc.monotype.open_function_interface_shape.v3");
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
        return self.graph.isRecursiveValueSlot(node);
    }

    fn hasForcedDynamicIteratorRoot(self: *OpenFunctionInterfaceShapeWriter, node: NodeId) bool {
        return self.graph.iteratorRootRequiresForcedDynamic(node);
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
        generated_iterator: ?*const InstGeneratedIterator,
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

fn instTagLessThan(name_store: *const names.NameStore, lhs: InstTag, rhs: InstTag) bool {
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

fn contentHasGeneratedPrivateBacking(content: InstNode) bool {
    if (content != .named) return false;
    const backing = content.named.backing orelse return false;
    return backing.authority == .generated_private;
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

fn instGeneratedIteratorEql(left: ?*const InstGeneratedIterator, right: ?*const InstGeneratedIterator) bool {
    const a = left orelse return right == null;
    const b = right orelse return false;
    return optionalInstDigestEql(a.callable_evidence, b.callable_evidence) and
        std.meta.eql(a.public_source.named_type, b.public_source.named_type) and
        std.meta.eql(a.public_source.def, b.public_source.def) and
        a.public_source.kind == b.public_source.kind and
        a.public_source.builtin_owner == b.public_source.builtin_owner and
        backingEql(a.public_source.backing, b.public_source.backing) and
        instDeclaredFieldSliceEql(a.public_source.declared_order, b.public_source.declared_order);
}

fn instNamedEql(left: *const InstNamed, right: *const InstNamed) bool {
    return std.meta.eql(left.named_type, right.named_type) and
        std.meta.eql(left.def, right.def) and
        left.kind == right.kind and
        std.meta.eql(left.builtin_owner, right.builtin_owner) and
        nodeSliceEql(left.args, right.args) and
        backingEql(left.backing, right.backing) and
        instGeneratedIteratorEql(left.generated_iterator, right.generated_iterator) and
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
    // A variable joining a primitive changes no observable type.
    try std.testing.expectEqual(@as(u64, 0), diagnostics.active_snapshot_invalidations);
    try std.testing.expectEqual(@as(u64, 0), diagnostics.active_snapshot_entries_invalidated);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.generated_private_guard_returns);
    try std.testing.expectEqual(@as(u64, 0), diagnostics.generated_private_scans);
    // No node carries a generated-private backing, so no scan runs.
    try std.testing.expectEqual(@as(u64, 0), diagnostics.generated_private_nodes_visited);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.finished_mono_scans);
    try std.testing.expectEqual(@as(u64, 1), diagnostics.finished_mono_nodes_visited);
}

test "issue 11362: iterator-free finalization does no graph traversal" {
    const gpa = std.testing.allocator;
    var types = Type.Store.init(gpa);
    defer types.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &types, &name_store);
    defer graph.destroy();
    _ = try graph.newNode(.empty_record);
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expectEqual(@as(u64, 0), diagnostics.union_find_resolutions);
}

test "issue 11362: generated iterator index follows roots provenance and duplicate keys" {
    try testGeneratedIteratorMigration(std.testing.allocator);
}

test "issue 11362: generated iterator index releases allocations on failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testGeneratedIteratorMigration, .{});
}

fn assertGeneratedIteratorIndexConsistent(graph: *InstGraph) void {
    std.debug.assert(graph.nodes.items.len == graph.request_source_interfaces.items.len);
    std.debug.assert(graph.nodes.items.len == graph.constructor_evidence_requests.items.len);
    var entries = graph.generated_iterator_entries.iterator();
    while (entries.next()) |entry| {
        const node = entry.key_ptr.*;
        const key = entry.value_ptr.key;
        std.debug.assert(entry.value_ptr.indexed);
        std.debug.assert(@intFromEnum(node) < graph.nodes.items.len);
        const named = graph.nodes.items[@intFromEnum(node)].named;
        var expected = GeneratedIteratorKey.fromNamed(named).?;
        expected.args = key.args;
        std.debug.assert(GeneratedIteratorKeyContext.eql(.{}, expected, key));
        for (key.args, named.args, 0..) |root, arg, arg_index| {
            std.debug.assert(root == graph.find(arg));
            var occurrences: usize = 0;
            for (graph.generated_iterators_by_root.get(root).?.items) |occurrence| {
                if (occurrence.node == node and occurrence.arg_index == arg_index) occurrences += 1;
            }
            std.debug.assert(occurrences == 1);
        }
        var next: ?NodeId = graph.generated_iterator_index.get(key).?;
        while (next) |candidate| {
            if (candidate == node) break;
            next = graph.generated_iterator_entries.get(candidate).?.next;
        } else unreachable;
    }
    var roots = graph.generated_iterators_by_root.iterator();
    while (roots.next()) |entry| {
        for (entry.value_ptr.items) |occurrence| {
            const key = graph.generated_iterator_entries.get(occurrence.node).?.key;
            std.debug.assert(key.args[occurrence.arg_index] == entry.key_ptr.*);
        }
    }
}

fn testGeneratedIteratorMigration(gpa: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    var types = Type.Store.init(gpa);
    defer types.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &types, &name_store);
    defer graph.destroy();
    defer assertGeneratedIteratorIndexConsistent(graph);
    const item = try graph.newNode(.{ .primitive = .str });
    const component = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const backing = try graph.newNode(.empty_record);
    const source: InstIteratorPublicSource = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(9) },
        .def = .{
            .module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32)),
            .type_name = try name_store.internTypeName("Iter"),
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = backing, .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const public = try graph.newNode(try graph.namedContent(.{
        .named_type = source.named_type,
        .def = source.def,
        .kind = source.kind,
        .builtin_owner = source.builtin_owner,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = source.backing,
    }));
    var named = graph.content(public).named.*;
    named.def.iterator_kind = .list;
    named.def.iterator_representation = .minted;
    named.generated_iterator = try graph.generatedIterator(.{ .public_source = source, .callable_evidence = null });
    // Repeated component roots exercise deduplication of reverse dependencies.
    named.args = try graph.arena().dupe(NodeId, &.{ item, component, component });
    const first = try graph.newNode(try graph.namedContent(named));
    const duplicate = try graph.newNode(try graph.namedContent(named));
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ component, component }, null).?);
    const resolved = try graph.newNode(.empty_record);
    try graph.unify(component, resolved);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ resolved, resolved }, null).?);
    try std.testing.expect(!graph.sameClass(first, duplicate));
    // The representative key must not keep borrowing the removed node's roots.
    var changed = named;
    changed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = .{ .bytes = @splat(0xA1) }, .public_source = changed.generated_iterator.?.public_source });
    try graph.setContent(first, try graph.namedContent(changed));
    try std.testing.expectEqual(duplicate, graph.findGeneratedIterator(public, .list, &.{ component, component }, null).?);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ component, component }, changed.generated_iterator.?.callable_evidence).?);
    // Replacements can attach provenance to a reserved recursive node.
    const reserved = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    changed.def.iterator_kind = .forced_dynamic;
    changed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = changed.generated_iterator.?.public_source });
    changed.args = try graph.arena().dupe(NodeId, &.{item});
    try graph.setContent(reserved, try graph.namedContent(changed));
    try std.testing.expectEqual(reserved, graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null).?);
    try graph.setContent(reserved, .empty_record);
    try std.testing.expect(graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null) == null);
    try graph.setContent(first, try graph.namedContent(named));
    try graph.union_(duplicate, first);
    try std.testing.expectEqual(duplicate, graph.findGeneratedIterator(public, .list, &.{ component, component }, null).?);
    const other_component = try graph.newNode(.empty_record);
    var converging = named;
    converging.args = try graph.arena().dupe(NodeId, &.{ item, other_component, other_component });
    const other = try graph.newNode(try graph.namedContent(converging));
    try std.testing.expectEqual(other, graph.findGeneratedIterator(public, .list, &.{ other_component, other_component }, null).?);
    try graph.union_(resolved, other_component);
    try std.testing.expectEqual(duplicate, graph.findGeneratedIterator(public, .list, &.{ other_component, other_component }, null).?);
    try std.testing.expect(!graph.sameClass(duplicate, other));
    // Distinct source keys converge onto an occupied target bucket. Many
    // duplicate members force destination growth during allocation preflight.
    const merging = try graph.newNode(.empty_record);
    const merging_args = [_][3]NodeId{
        .{ item, merging, resolved },
        .{ item, resolved, merging },
        .{ item, merging, merging },
    };
    for (merging_args) |args| {
        var generated_source = named;
        generated_source.args = try graph.arena().dupe(NodeId, &args);
        for (0..16) |_| _ = try graph.newNode(try graph.namedContent(generated_source));
    }
    try graph.union_(resolved, merging);
    const merged_key = graph.generated_iterator_entries.get(duplicate).?.key;
    var chain_length: usize = 0;
    var chain: ?NodeId = graph.generated_iterator_index.get(merged_key);
    while (chain) |node| : (chain = graph.generated_iterator_entries.get(node).?.next) chain_length += 1;
    try std.testing.expectEqual(@as(usize, 50), chain_length);
    try std.testing.expectEqual(duplicate, graph.findGeneratedIterator(public, .list, &.{ merging, merging }, null).?);
    // A miss among many iterators of the same declaration and kind must probe
    // the complete key, rather than scanning nodes or a declaration bucket.
    for (0..32) |_| {
        const independent = try graph.newNode(.empty_record);
        var unrelated = named;
        unrelated.args = try graph.arena().dupe(NodeId, &.{ item, independent, independent });
        _ = try graph.newNode(try graph.namedContent(unrelated));
    }
    const absent = try graph.newNode(.empty_record);
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try std.testing.expectEqual(duplicate, graph.findGeneratedIterator(public, .list, &.{ resolved, resolved }, null).?);
    try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{ absent, absent }, null) == null);
    try std.testing.expectEqual(@as(u64, 2), diagnostics.generated_iterator_lookups);
    try std.testing.expect(diagnostics.union_find_resolutions < 20);
}

test "issue 10941: row extension class unions remain linear" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    // Repro for https://github.com/roc-lang/roc/issues/10941: re-rooting one
    // row-extension class must do work linear in the graph nodes and unions.
    const row_count = 128;
    var extension = try graph.newNode(.empty_record);
    for (0..row_count) |_| {
        _ = try graph.newNode(.{ .record = .{
            .fields = &.{},
            .ext = extension,
        } });
        const replacement = try graph.newNode(.empty_record);
        try graph.union_(replacement, extension);
        extension = replacement;
    }

    try std.testing.expectEqual(@as(u64, row_count * 2 + 1), diagnostics.nodes_created);
    try std.testing.expectEqual(@as(u64, row_count), diagnostics.class_unions);
    const linear_work_limit = (diagnostics.nodes_created + diagnostics.class_unions) * 16;
    if (diagnostics.union_find_resolutions > linear_work_limit) {
        std.debug.print(
            "row extension unions performed {d} union-find resolutions; expected at most {d}\n",
            .{ diagnostics.union_find_resolutions, linear_work_limit },
        );
    }
    try std.testing.expect(diagnostics.union_find_resolutions <= linear_work_limit);
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

test "argument class snapshots preserve entry membership through unions on both sides" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    // Both outsiders predate the snapshot: node age cannot stand in for
    // membership, and either side may become the representative later.
    const before = try graph.newNode(.empty_record);
    const after = try graph.newNode(.empty_record);
    const first = try graph.newNode(.empty_record);
    const last = try graph.newNode(.empty_record);
    try graph.union_(first, last);
    const ret = try graph.newNode(.{ .primitive = .bool });
    const active = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{first}),
        .ret = ret,
    } });
    const initial = try graph.snapshotFunctionArgumentClasses(active);
    try graph.union_(before, first);
    try graph.union_(before, after);
    const new_node = try graph.newNode(.empty_record);
    try graph.union_(new_node, before);
    const later = try graph.snapshotFunctionArgumentClasses(active);

    for ([_]NodeId{ first, last }) |member| {
        try std.testing.expect(initial[0].contains(graph, member));
        const request = try graph.newNode(.{ .func = .{
            .args = try graph.arena().dupe(NodeId, &.{member}),
            .ret = ret,
        } });
        try graph.unifyRecursiveFunctionInterface(active, initial, request);
    }
    for ([_]NodeId{ first, last }) |member| try std.testing.expect(!graph.isRecursiveValueSlot(member));
    for ([_]NodeId{ before, after, new_node }) |member| {
        try std.testing.expect(!initial[0].contains(graph, member));
        try std.testing.expect(later[0].contains(graph, member));
        const request = try graph.newNode(.{ .func = .{
            .args = try graph.arena().dupe(NodeId, &.{member}),
            .ret = ret,
        } });
        try graph.unifyRecursiveFunctionInterface(active, initial, request);
    }
    for ([_]NodeId{ before, after, new_node }) |member| try std.testing.expect(graph.isRecursiveValueSlot(member));
    // Re-reading a snapshot after another snapshot and recursive relations
    // must still stop at its original tail.
    try std.testing.expect(!initial[0].contains(graph, after));
}

test "structural backing traversal preserves cycle entry and clears only visited scratch" {
    const gpa = std.testing.allocator;
    for ([_]?usize{ null, 0, 1, 7 }) |cycle_entry| {
        var type_store = Type.Store.init(gpa);
        defer type_store.deinit();
        var name_store = names.NameStore.init(gpa);
        defer name_store.deinit();
        const graph = try InstGraph.create(gpa, &type_store, &name_store);
        defer graph.destroy();
        const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAC} ** 32));
        const type_name = try name_store.internTypeName("Chain");
        const terminal = try graph.newNode(.empty_record);
        var nodes: [12]NodeId = undefined;
        for (&nodes) |*node| node.* = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
        for (nodes, 0..) |node, index| {
            const next = if (index + 1 < nodes.len) nodes[index + 1] else if (cycle_entry) |entry| nodes[entry] else terminal;
            try graph.setContent(node, try graph.namedContent(.{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                .def = .{ .module = module_identity, .type_name = type_name },
                .kind = .nominal,
                .builtin_owner = null,
                .args = &.{},
                .backing = .{ .node = next, .use = .inspectable },
            }));
        }
        var diagnostics = GraphDiagnostics{};
        graph.diagnostics = &diagnostics;
        const owner = graph.content(nodes[0]).named;
        const expected = if (cycle_entry) |entry| nodes[entry] else terminal;
        var prior_steps: ?u64 = null;
        for (0..2) |_| {
            const before_steps = diagnostics.structural_backing_scan_slots;
            const result = try graph.findStructuralBackingNode(nodes[0], owner);
            try std.testing.expectEqual(expected, result.node);
            try std.testing.expectEqual(cycle_entry != null, result.recursive);
            const steps = diagnostics.structural_backing_scan_slots - before_steps;
            try std.testing.expect(steps <= 8 * nodes.len);
            if (prior_steps) |previous| try std.testing.expectEqual(previous, steps);
            prior_steps = steps;
            // Unrelated graph growth must not increase the next walk's work.
            for (0..1000) |_| _ = try graph.newNode(.empty_record);
        }
    }
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

test "function interface classes deduplicate aliases and refresh after unions" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const arg = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const alias = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    // The same unresolved shape is a distinct class until explicitly related.
    const other = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const function = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{ arg, arg, alias, other }),
        .ret = alias,
    } });
    try graph.unify(arg, alias);

    {
        var classes = try graph.functionInterfaceClassIterator(function);
        defer classes.deinit();
        var members = collections.DenseMap(NodeId, void).init(gpa);
        defer members.deinit();
        var count: usize = 0;
        while (try classes.next()) |class| {
            count += 1;
            var aliases = graph.classMemberIterator(class);
            while (aliases.next()) |member| {
                try std.testing.expect(!(try members.getOrPut(member)).found_existing);
            }
        }
        try std.testing.expectEqual(@as(usize, 2), count);
        try std.testing.expectEqual(@as(usize, 3), members.count());
        try std.testing.expect(members.contains(arg));
        try std.testing.expect(members.contains(alias));
        try std.testing.expect(members.contains(other));
    }

    try graph.unify(alias, other);
    // A new lookup must observe the union, with no visited state retained
    // from the previous query. Overlapping iterators own separate scratch.
    var classes = try graph.functionInterfaceClassIterator(function);
    defer classes.deinit();
    var concurrent = try graph.functionInterfaceClassIterator(function);
    defer concurrent.deinit();
    const root = graph.find(arg);
    try std.testing.expectEqual(root, (try classes.next()).?);
    try std.testing.expectEqual(root, (try concurrent.next()).?);
    try std.testing.expectEqual(null, try classes.next());
    try std.testing.expectEqual(null, try concurrent.next());

    const nullary = try graph.newNode(.{ .func = .{ .args = &.{}, .ret = arg } });
    var return_only = try graph.functionInterfaceClassIterator(nullary);
    defer return_only.deinit();
    try std.testing.expectEqual(root, (try return_only.next()).?);
    try std.testing.expectEqual(null, try return_only.next());
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
    const exact_bytes_digest = TypeDigestHasher.hash(left_shape.bytes);
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

    graph.markRecursiveValueSlot(left_arg);
    const recursive_left_shape = try graph.openFunctionInterfaceShape(left);
    const unmarked_right_shape = try graph.openFunctionInterfaceShape(right);
    try std.testing.expect(!std.mem.eql(u8, recursive_left_shape.bytes, unmarked_right_shape.bytes));

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xA7} ** 32));
    const type_name = try name_store.internTypeName("PrivateShape");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const private_left = try graph.newNode(try graph.namedContent(.{
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
    }));
    const private_right = try graph.newNode(try graph.namedContent(.{
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
    }));
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

test "provisional Monotype views share resolved argument subtrees" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    const value = try graph.newNode(.{ .primitive = .u64 });
    const record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = try name_store.internRecordFieldLabel("value"),
            .ty = value,
            .default = null,
        }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const function = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{record}),
        .ret = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) }),
    } });
    const first = try graph.provisionalTypeViewForNode(function);
    const second = try graph.provisionalTypeViewForNode(function);
    try std.testing.expect(first != second);
    const first_arg = GuardedList.at(type_store.span(type_store.get(first).func.args), 0);
    const second_arg = GuardedList.at(type_store.span(type_store.get(second).func.args), 0);
    try std.testing.expectEqual(first_arg, second_arg);
    try std.testing.expectEqual(try graph.monoFor(record), first_arg);
    try std.testing.expectEqual(@as(u64, 4), diagnostics.provisional_snapshot_nodes_materialized);
}

test "retained provisional view preserves content across later graph refinement" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const value = try graph.newNode(.{ .primitive = .u64 });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const kind = try graph.newUndeterminedFieldKind();
    graph.registerUndeterminedFieldKindCells(kind, slot, value);
    const record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = try name_store.internRecordFieldLabel("value"),
            .ty = slot,
            .value_ty = value,
            .kind = .{ .undetermined = kind },
            .default = null,
        }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const view = try graph.provisionalTypeViewForNode(record);
    var sealer = GraphTypeFinals.initRetainedTypeView(graph);
    defer sealer.deinit();
    const retained = try sealer.sealType(view);
    try std.testing.expect(try type_store.isInterned(&name_store, retained));
    try std.testing.expect(try type_store.typeEql(&name_store, view, retained));
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{record});
    const first = (try constraints.instantiate(graph))[0];
    const second = (try constraints.instantiate(graph))[0];
    try std.testing.expect(!graph.sameClass(first, second));
    try std.testing.expect(graph.resolvedFieldKind(.{ .undetermined = kind }) == null);
    try graph.freezeRelations();
    const final = try graph.sealNode(record);
    try std.testing.expect(!try type_store.typeEql(&name_store, retained, final));
    try std.testing.expect(try type_store.typeEql(&name_store, view, retained));
}

test "issue 11303: reading a field value preserves its undetermined storage until freeze" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("render");
    const arg = try graph.newNode(.{ .primitive = .u64 });
    const value = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{arg}),
        .ret = try graph.newNode(.{ .primitive = .str }),
    } });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const kind = try graph.newUndeterminedFieldKind();
    graph.registerUndeterminedFieldKindCells(kind, slot, value);
    const record = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{
            .name = field_name,
            .ty = slot,
            .value_ty = value,
            .kind = .{ .undetermined = kind },
            .default = null,
        }}),
        .ext = try graph.newNode(.empty_record),
    } });

    try std.testing.expectEqual(slot, try graph.recordFieldNode(record, field_name));
    const node_count = graph.nodes.items.len;
    try std.testing.expectEqual(value, try graph.recordFieldValueNode(record, field_name));
    try std.testing.expectEqual(node_count, graph.nodes.items.len);
    try std.testing.expect(graph.resolvedFieldKind(.{ .undetermined = kind }) == null);
    try std.testing.expect(graph.content(slot) == .unresolved);
    try std.testing.expect(!graph.sameClass(slot, value));

    try graph.freezeRelations();
    try std.testing.expect(graph.resolvedFieldKind(.{ .undetermined = kind }).? == .required);
    try std.testing.expect(graph.sameClass(slot, value));
    try std.testing.expectEqual(try graph.recordFieldNode(record, field_name), try graph.recordFieldValueNode(record, field_name));
}

test "issue 11303: field value reads preserve optional tagged storage and sealed metadata" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("render");
    const arg = try graph.newNode(.{ .primitive = .u64 });
    const value = try graph.newNode(.{ .func = .{
        .args = try graph.arena().dupe(NodeId, &.{arg}),
        .ret = try graph.newNode(.{ .primitive = .str }),
    } });
    const missing = try name_store.internTagLabel("#Missing");
    const present = try name_store.internTagLabel("#Present");
    const slot = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{
            .{ .name = missing, .checked_name = missing, .payloads = try graph.arena().alloc(NodeId, 0) },
            .{ .name = present, .checked_name = present, .payloads = try graph.arena().dupe(NodeId, &.{value}) },
        }),
        .ext = try graph.newNode(.empty_tag_union),
    } });

    const cases = [_]struct { kind: InstFieldKind, optional: bool }{
        .{ .kind = .optional, .optional = true },
        .{ .kind = .sealed, .optional = true },
        .{ .kind = .required, .optional = false },
        .{ .kind = .sealed, .optional = false },
    };
    for (cases) |case| {
        const field_slot = if (case.optional) slot else value;
        const record = try graph.newNode(.{ .record = .{
            .fields = try graph.arena().dupe(InstField, &.{.{
                .name = field_name,
                .ty = field_slot,
                .value_ty = if (case.optional) value else null,
                .kind = case.kind,
                .default = null,
            }}),
            .ext = try graph.newNode(.empty_record),
        } });
        try std.testing.expectEqual(value, try graph.recordFieldValueNode(record, field_name));
        try std.testing.expectEqual(field_slot, try graph.recordFieldNode(record, field_name));
        try std.testing.expect(graph.content(value) == .func);
        try std.testing.expect(graph.content(slot) == .tag_union);
        try std.testing.expect(!graph.sameClass(slot, value));
    }
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
    const named = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(11) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    }));

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
        owner.* = graph.active_snapshot_nodes.get(snapshot.*).?;
        try graph.union_(winner, node);
    }

    for (snapshots, owners) |snapshot, owner| {
        // The reverse index remains stable instead of rewriting every prior
        // snapshot on each union. Root resolution happens only when queried.
        try std.testing.expectEqual(owner, graph.active_snapshot_nodes.get(snapshot).?);
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
    const alias = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = try name_store.internModuleIdentity(&([_]u8{0xAB} ** 32)), .type_name = @enumFromInt(1) },
        .kind = .alias,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .inspectable },
    }));

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

test "reset starts an unrelated graph while retaining its stores" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const first = try graph.newNode(.{ .primitive = .u64 });
    _ = try graph.activeTypeViewForNode(first);
    try graph.freezeRelations();
    try std.testing.expect(!graph.acceptsRelationMutation());
    try std.testing.expect(graph.node_snapshots.count() != 0);

    graph.reset();
    try std.testing.expect(graph.acceptsRelationMutation());
    try std.testing.expectEqual(@as(usize, 0), graph.nodes.items.len);
    try std.testing.expectEqual(@as(usize, 0), graph.node_snapshots.count());

    const second = try graph.newNode(.{ .primitive = .str });
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(second));
    try graph.freezeRelations();
    const sealed = try graph.sealNode(second);
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(sealed));
}

test "reset discards nominal relationships and constructor evidence before reusing node ids" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const module = try name_store.internModuleIdentity(&([_]u8{0x81} ** 32));
    const type_name = try name_store.internTypeName("EpochNominal");

    for (0..2) |_| {
        const left_backing = try graph.newNode(.empty_record);
        const right_backing = try graph.newNode(.empty_record);
        var named: InstNamed = .{
            .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
            .def = .{ .module = module, .type_name = type_name },
            .kind = .nominal,
            .builtin_owner = null,
            .args = &.{},
            .backing = .{ .node = left_backing, .use = .inspectable },
        };
        const left = try graph.newNode(try graph.namedContent(named));
        const left_copy = try graph.newNode(try graph.namedContent(named));
        named.backing.?.node = right_backing;
        const right = try graph.newNode(try graph.namedContent(named));
        try std.testing.expect(!graph.sameRelatedNamedInstance(left, left_copy));
        try std.testing.expect(!graph.sameRelatedNamedInstance(left, right));
        try std.testing.expect(!graph.requestPropagatesConstructorEvidence(left));

        try graph.relateNamedInstances(left, right);
        graph.registerConstructorEvidenceRequest(left);
        try std.testing.expect(graph.sameRelatedNamedInstance(left_copy, right));
        try std.testing.expect(graph.requestPropagatesConstructorEvidence(left));
        graph.reset();
    }
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

    // A variable joining a resolved class leaves the class's type as it
    // was, so its current view stays.
    try std.testing.expect(graph.acceptsRelationMutation());
    try std.testing.expect(!graph.current_snapshots_dirty);
    const after_join = try graph.activeTypeViewForNode(resolved);
    try std.testing.expectEqual(before_mutation, after_join);

    // Replacing the class's content is observable through every view.
    try graph.setContent(graph.find(resolved), .{ .primitive = .u32 });
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

    // An unrelated generated-private node makes the traversal necessary;
    // without one the query answers without visiting anything.
    const module_identity = try name_store.internModuleIdentity(&([_]u8{0x47} ** 32));
    const type_name = try name_store.internTypeName("PrivateValue");
    _ = try graph.newNode(try graph.namedContent(.{
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
    }));
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
    const private_value = try graph.newNode(try graph.namedContent(.{
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
    }));
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
    try graph.setContent(child, try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(14) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = &.{},
        .backing = null,
    }));
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
                7 => try self.graph.newNode(try self.graph.namedContent(.{
                    .named_type = .{ .module = .{}, .ty = testCheckedTypeId(21) },
                    .def = .{ .module = self.module_identity, .type_name = self.wrapper_name },
                    .kind = .nominal,
                    .builtin_owner = null,
                    .args = try self.graph.arena().dupe(NodeId, &.{}),
                    .backing = .{ .node = leaf, .use = .inspectable },
                })),
                // A nominal wrapper reaching the leaf through a type argument.
                8 => try self.graph.newNode(try self.graph.namedContent(.{
                    .named_type = .{ .module = .{}, .ty = testCheckedTypeId(22) },
                    .def = .{ .module = self.module_identity, .type_name = self.wrapper_name },
                    .kind = .nominal,
                    .builtin_owner = null,
                    .args = try self.graph.arena().dupe(NodeId, &.{leaf}),
                    .backing = .{ .node = u64_node, .use = .inspectable },
                })),
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
            const leaf = if (iterator_leaf) try graph.newNode(try graph.namedContent(.{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(20) },
                .def = .{ .module = module_identity, .type_name = iter_name },
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{try graph.newNode(.{ .primitive = .u64 })}),
                .backing = .{
                    .node = try graph.newNode(.{ .primitive = .u64 }),
                    .use = .runtime_layout_only,
                },
            })) else try graph.newNode(.{ .primitive = .str });

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

test "independent closed tag-row imports have distinct solver nodes" {
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

    const request_node = try graph.importMonoIndependent(requested);
    const independent_request_node = try graph.importMonoIndependent(requested);
    try std.testing.expect(request_node != independent_request_node);

    const imported = graph.content(request_node).tag_union;
    const additional_tags = [_]InstTag{.{ .name = b, .checked_name = b, .payloads = &.{} }};
    try std.testing.expect(graph.rowAdditionConflicts(imported.ext, additional_tags.len, .tag_union));
    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(imported.ext));

    const retained = graph.content(independent_request_node).tag_union;
    try std.testing.expectEqual(@as(usize, 1), retained.tags.len);
    try std.testing.expectEqual(a, retained.tags[0].name);
    try std.testing.expect(retained.tags[0].name != b);
    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(retained.ext));
}

test "independent closed record-row imports have distinct solver nodes" {
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

    const request_node = try graph.importMonoIndependent(requested);
    const independent_request_node = try graph.importMonoIndependent(requested);
    try std.testing.expect(request_node != independent_request_node);

    const imported = graph.content(request_node).record;
    const additional_fields = [_]InstField{.{
        .name = extra,
        .ty = try graph.newNode(.{ .primitive = .u64 }),
        .default = null,
    }};
    try std.testing.expect(graph.rowAdditionConflicts(imported.ext, additional_fields.len, .record));
    try std.testing.expectEqual(InstNode.empty_record, graph.content(imported.ext));

    const retained = graph.content(independent_request_node).record;
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
    const public = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = public_args,
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    }));
    const private = try graph.newNode(try graph.namedContent(.{
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
    }));

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
    const public_evidence = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{public_arg}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    }));
    const private_evidence = try graph.newNode(try graph.namedContent(.{
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
    }));
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
    const named = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    }));

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
    const nominal = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = backing, .use = .inspectable },
    }));
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
    const public = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = .{ .module = module_identity, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{public_item}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    }));
    const private = try graph.newNode(try graph.namedContent(.{
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
    }));

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
    const private = try graph.newNode(try graph.namedContent(.{
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
        .generated_iterator = try graph.generatedIterator(.{
            .callable_evidence = null,
            .public_source = public_source,
        }),
    }));

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
    const private = try graph.newNode(try graph.namedContent(.{
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
    }));

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
    const left_iter = try graph.newNode(try graph.namedContent(.{
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
    }));
    const right_iter = try graph.newNode(try graph.namedContent(.{
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
    }));
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
    const private_arg = try graph.newNode(try graph.namedContent(.{
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
        .generated_iterator = try graph.generatedIterator(.{
            .callable_evidence = null,
            .public_source = public_source,
        }),
    }));
    const request = try graph.newNode(try graph.namedContent(.{
        .named_type = shell_named_type,
        .def = shell_def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{private_arg}),
        .backing = .{
            .node = try graph.newNode(.empty_record),
            .use = .runtime_layout_only,
        },
    }));

    try graph.relateOpaqueInterface(public, request);

    const retained_public = graph.content(public).named;
    try std.testing.expect(graph.sameClass(public, request));
    try std.testing.expectEqual(Type.BackingAuthority.checked_public, retained_public.backing.?.authority);
    try std.testing.expectEqual(@as(usize, 1), retained_public.args.len);
    try std.testing.expect(graph.sameClass(retained_public.args[0], private_arg));
}

test "generated iterator index reset discards replaced and rekeyed producers" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const item = try graph.newNode(.{ .primitive = .u64 });
    const component = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const backing = try graph.newNode(.empty_record);
    const public_named: InstNamed = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    };
    const public = try graph.newNode(try graph.namedContent(public_named));
    var minted = public_named;
    minted.def.iterator_kind = .list;
    minted.def.iterator_representation = .minted;
    minted.args = try graph.arena().dupe(NodeId, &.{ item, component });
    minted.backing.?.authority = .generated_private;
    minted.generated_iterator = try graph.generatedIterator(.{
        .callable_evidence = null,
        .public_source = .{
            .named_type = public_named.named_type,
            .def = public_named.def,
            .kind = public_named.kind,
            .builtin_owner = .iter,
            .backing = public_named.backing.?,
            .declared_order = &.{},
        },
    });
    const first = try graph.newNode(try graph.namedContent(minted));
    const RecursiveMinted = struct { graph: *InstGraph, named: InstNamed };
    const second = try graph.addRecursiveNode(RecursiveMinted{ .graph = graph, .named = minted }, struct {
        fn fill(context: RecursiveMinted, _: NodeId) Allocator.Error!InstNode {
            return context.graph.namedContent(context.named);
        }
    }.fill);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{component}, null).?);
    try std.testing.expectEqual(@as(u32, 1), graph.generated_iterator_index.count());
    try std.testing.expect(graph.generated_iterator_nodes > 0);
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .single, &.{component}, null));
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .list, &.{}, null));

    // Arguments are compared by their current class, not their minted id.
    const resolved = try graph.newNode(.{ .primitive = .str });
    try graph.unify(component, resolved);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{resolved}, null).?);
    try graph.union_(second, first);
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, null).?);

    // Changing callable evidence moves the live node to a different bucket.
    minted.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = .{ .bytes = @splat(0x62) }, .public_source = minted.generated_iterator.?.public_source });
    try graph.setContent(second, try graph.namedContent(minted));
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .list, &.{component}, null));
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, minted.generated_iterator.?.callable_evidence).?);
    try std.testing.expectEqual(@as(u32, 1), graph.generated_iterator_index.count());

    // Removing provenance removes membership; permanent request identities live on.
    graph.registerConstructorEvidenceRequest(second);
    try graph.registerRequestSourceInterface(second, public);
    try graph.setContent(second, try graph.namedContent(public_named));
    try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_index.count());
    try std.testing.expect(graph.requestPropagatesConstructorEvidence(second));
    try std.testing.expectEqual(public, graph.requestSourceInterface(second).?);
    try std.testing.expect(graph.generated_iterator_nodes > 0);

    // The next job may reuse these node ids without inheriting a producer.
    try graph.setContent(second, try graph.namedContent(minted));
    graph.reset();
    const next_item = try graph.newNode(.{ .primitive = .u64 });
    const next_component = try graph.newNode(.{ .primitive = .str });
    const next_backing = try graph.newNode(.empty_record);
    var next_public = public_named;
    next_public.args = try graph.arena().dupe(NodeId, &.{next_item});
    next_public.backing.?.node = next_backing;
    const next_public_node = try graph.newNode(try graph.namedContent(next_public));
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(
        next_public_node,
        .list,
        &.{next_component},
        minted.generated_iterator.?.callable_evidence,
    ));
    try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_nodes);
    try std.testing.expectEqual(@as(usize, 0), graph.generated_iterator_entries.count());
    try std.testing.expectEqual(@as(usize, 0), graph.generated_iterators_by_root.count());
    try std.testing.expectEqual(@as(usize, 0), graph.generated_iterator_index.count());
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
    const source = try graph.newNode(try graph.namedContent(.{
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
        .generated_iterator = try graph.generatedIterator(.{
            .callable_evidence = null,
            .public_source = public_source,
        }),
    }));

    // Put the only iterator-bearing child after the former 64-node walk
    // budget. Graph width must not change the adapter's representation.
    const wide_children = try graph.arena().alloc(NodeId, 65);
    for (wide_children[0..64]) |*child| {
        child.* = try graph.newNode(.{ .primitive = .u64 });
    }
    wide_children[64] = source;
    const wide_component = try graph.newNode(.{ .tuple = wide_children });
    const adapter = try graph.newNode(try graph.namedContent(.{
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
        .generated_iterator = try graph.generatedIterator(.{
            .callable_evidence = null,
            .public_source = public_source,
        }),
    }));

    try graph.finalizeGeneratedIteratorRepresentations();

    const finalized = graph.content(adapter).named.def;
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, finalized.iterator_representation);
    try std.testing.expectEqual(@as(u8, 2), finalized.iterator_depth);
}

test "generated iterator identity uses current graph content rather than an imported witness" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const module = try name_store.internModuleIdentity(&([_]u8{0x82} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const item_ty = try type_store.add(.{ .primitive = .u64 });
    const backing_ty = try type_store.add(.{ .record = .empty() });
    const source_ty = try type_store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module, .type_name = type_name, .iterator_representation = .minted, .iterator_kind = .list, .iterator_depth = 1 },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try type_store.addSpan(&.{item_ty}),
        .backing = .{ .ty = backing_ty, .use = .runtime_layout_only, .authority = .generated_private },
    } });
    var identities: [3]names.TypeDigest = undefined;
    for (&identities, 0..) |*identity, index| {
        const graph = try InstGraph.create(gpa, &type_store, &name_store);
        defer graph.destroy();
        const item = try graph.newNode(.{ .primitive = .u64 });
        const backing = try graph.newNode(.empty_record);
        const public_def: Type.TypeDef = .{ .module = module, .type_name = type_name };
        const owned: InstNode = try graph.namedContent(.{
            .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
            .def = .{ .module = module, .type_name = type_name, .iterator_representation = .minted, .iterator_kind = .range, .iterator_depth = 1 },
            .kind = .@"opaque",
            .builtin_owner = .iter,
            .args = try graph.arena().dupe(NodeId, &.{item}),
            .backing = .{ .node = backing, .use = .runtime_layout_only, .authority = .generated_private },
            .generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = .{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                .def = public_def,
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .backing = .{ .node = backing, .use = .runtime_layout_only },
                .declared_order = &.{},
            } }),
        });
        const node = if (index == 0) try graph.newNode(owned) else blk: {
            const imported = try graph.importMono(source_ty);
            const current = if (index == 1) replaced: {
                try graph.setContent(imported, owned);
                break :replaced imported;
            } else joined: {
                // A cached losing representation must not become the winner's view.
                _ = try graph.monoFor(imported);
                const producer = try graph.newNode(owned);
                try graph.unify(producer, imported);
                break :joined producer;
            };
            // The original request remains available as its exact witness.
            try std.testing.expectEqual(source_ty, try graph.provisionalTypeViewForNode(imported));
            break :blk current;
        };
        try graph.finalizeGeneratedIteratorIdentities();
        identity.* = graph.content(node).named.def.generated.?;
    }
    try std.testing.expectEqual(identities[0], identities[1]);
    try std.testing.expectEqual(identities[0], identities[2]);
}

test "generated iterator identity ignores checked provenance that type equality ignores" {
    // Two requests equal under `typeEql` must mint equal iterator types. The
    // item's checked type id is provenance: `typeEql` and specialization
    // identity ignore it, so producer identity must ignore it too.
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const module = try name_store.internModuleIdentity(&([_]u8{0x83} ** 32));
    const iter_name = try name_store.internTypeName("Iter");
    const item_name = try name_store.internTypeName("ByteRange");
    for ([_]Type.IteratorRepresentation{ .minted, .forced_dynamic }) |representation| {
        var identities: [2]names.TypeDigest = undefined;
        var items: [2]Type.TypeId = undefined;
        const provenances = [_]checked.CheckedTypeId{ testCheckedTypeId(10), testCheckedTypeId(11) };
        for (&identities, &items, provenances) |*identity, *item_ty, provenance| {
            const graph = try InstGraph.create(gpa, &type_store, &name_store);
            defer graph.destroy();
            const field = try graph.newNode(.{ .primitive = .u64 });
            const item = try graph.newNode(try graph.namedContent(.{
                .named_type = .{ .module = .{}, .ty = provenance },
                .def = .{ .module = module, .type_name = item_name, .source_decl = 1 },
                .kind = .@"opaque",
                .builtin_owner = null,
                .args = &.{},
                .backing = .{ .node = field, .use = .runtime_layout_only },
            }));
            const backing = try graph.newNode(.empty_record);
            const node = try graph.newNode(try graph.namedContent(.{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                .def = .{ .module = module, .type_name = iter_name, .iterator_representation = representation, .iterator_kind = .custom, .iterator_depth = 1 },
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{item}),
                .backing = .{ .node = backing, .use = .runtime_layout_only, .authority = .generated_private },
                .generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = .{
                    .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                    .def = .{ .module = module, .type_name = iter_name },
                    .kind = .@"opaque",
                    .builtin_owner = .iter,
                    .backing = .{ .node = backing, .use = .runtime_layout_only },
                    .declared_order = &.{},
                } }),
            }));
            try graph.finalizeGeneratedIteratorIdentities();
            identity.* = graph.content(node).named.def.generated.?;
            var finals = GraphTypeFinals.initProvisionalSnapshot(graph);
            defer finals.deinit();
            item_ty.* = try finals.sealNode(item);
        }
        // The items differ only in provenance that equality ignores.
        try std.testing.expect(try type_store.typeEql(&name_store, items[0], items[1]));
        try std.testing.expect(!std.meta.eql(type_store.typeDigest(&name_store, items[0]), type_store.typeDigest(&name_store, items[1])));
        try std.testing.expectEqual(identities[0], identities[1]);
    }
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
    const finished = try graph.newNode(try graph.namedContent(.{
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
    }));

    var owned_def = public_def;
    owned_def.iterator_representation = .minted;
    owned_def.iterator_kind = .list;
    owned_def.iterator_depth = 1;
    const owned = try graph.newNode(try graph.namedContent(.{
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
        .generated_iterator = try graph.generatedIterator(.{
            .callable_evidence = null,
            .public_source = public_source,
        }),
    }));

    // This order is significant: the finished type is the left side of the
    // recursive join, but only the graph-owned side can author the final
    // forced-dynamic representation.
    graph.markRecursiveValueSlot(finished);
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
    const public_arg = try graph.newNode(try graph.namedContent(.{
        .named_type = inner_named_type,
        .def = inner_def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = public_inner_backing, .use = .runtime_layout_only },
    }));
    const private_arg = try graph.newNode(try graph.namedContent(.{
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
    }));
    const public_backing = try graph.newNode(.empty_record);
    const private_backing = try graph.newNode(.empty_record);
    const public = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = .fields,
        .args = try graph.arena().dupe(NodeId, &.{public_arg}),
        .backing = .{ .node = public_backing, .use = .runtime_layout_only },
    }));
    const private = try graph.newNode(try graph.namedContent(.{
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
    }));

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

test "issue 11235: extension normalization preserves head payload and checked label provenance" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a = try name_store.internTagLabel("A");
    const head_provenance = try name_store.internTagLabel("HeadA");
    const head_payload = try graph.newNode(.{ .primitive = .str });
    const tail_payload = try graph.newNode(.{ .primitive = .u64 });
    const empty = try graph.newNode(.empty_tag_union);
    const tail = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = a,
            .checked_name = a,
            .payloads = try graph.arena().dupe(NodeId, &.{tail_payload}),
        }}),
        .ext = empty,
    } });
    const head = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{
            .name = a,
            .checked_name = head_provenance,
            .payloads = try graph.arena().dupe(NodeId, &.{head_payload}),
        }}),
        .ext = tail,
    } });

    // Like the checked unifier's mergeSortedExtensionTags, row composition
    // retains the head occurrence. It is not equality between the payloads
    // of a head and a shadowed extension, and must not mutate either payload.
    const view = try graph.provisionalTypeViewForNode(head);
    const tags = type_store.tagSpan(type_store.get(view).tag_union);
    try std.testing.expectEqual(@as(usize, 1), tags.len);
    const tag = GuardedList.at(tags, 0);
    try std.testing.expectEqual(head_provenance, tag.checked_name);
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(GuardedList.at(type_store.span(tag.payloads), 0)));
    try std.testing.expectEqual(InstNode{ .primitive = .u64 }, graph.content(tail_payload));
    try std.testing.expectEqual(tail_payload, graph.content(tail).tag_union.tags[0].payloads[0]);
}

test "issue 11235: normalized rows retain a shared tail through later extension" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a = try name_store.internTagLabel("A");
    const b = try name_store.internTagLabel("B");
    const other = try name_store.internTagLabel("Other");
    const shared = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    const head = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = a, .checked_name = a, .payloads = &.{} }}),
        .ext = shared,
    } });
    const sibling = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = other, .checked_name = other, .payloads = &.{} }}),
        .ext = shared,
    } });
    const before = try graph.provisionalTypeViewForNode(head);
    const rest = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    try graph.unify(shared, try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = a, .checked_name = a, .payloads = &.{} }}),
        .ext = rest,
    } }));
    const flat = try graph.flattenTagRow(head);
    try std.testing.expectEqual(@as(usize, 1), flat.tags.len);
    try std.testing.expectEqual(graph.find(rest), flat.ext);
    _ = try graph.flattenTagRow(sibling);
    try graph.unify(rest, try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = b, .checked_name = b, .payloads = &.{} }}),
        .ext = try graph.newNode(.empty_tag_union),
    } }));
    try graph.freezeRelations();
    const sealed_head = try graph.sealNode(head);
    const sealed_sibling = try graph.sealNode(sibling);
    const head_tags = type_store.tagSpan(type_store.get(sealed_head).tag_union);
    const sibling_tags = type_store.tagSpan(type_store.get(sealed_sibling).tag_union);
    try std.testing.expectEqual(@as(usize, 1), type_store.get(before).tag_union.len);
    try std.testing.expectEqual(@as(usize, 2), head_tags.len);
    try std.testing.expectEqual(@as(usize, 3), sibling_tags.len);
    for ([_]names.TagNameId{ a, b }, 0..) |label, index| {
        try std.testing.expectEqual(label, GuardedList.at(head_tags, index).name);
        try std.testing.expectEqual(label, GuardedList.at(sibling_tags, index).name);
    }
    try std.testing.expectEqual(other, GuardedList.at(sibling_tags, 2).name);
}

test "issue 11235: long overlapping row chains normalize once and repeated reads allocate nothing" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);

    const shared = try name_store.internTagLabel("Shared");
    const empty = try graph.newNode(.empty_tag_union);
    var root = empty;
    const count = 512;
    for (0..count) |index| {
        var buffer: [32]u8 = undefined;
        const label = try name_store.internTagLabel(try std.fmt.bufPrint(&buffer, "Tag{d:0>4}", .{index}));
        root = try graph.newNode(.{ .tag_union = .{
            .tags = try graph.arena().dupe(InstTag, &.{
                .{ .name = label, .checked_name = label, .payloads = &.{} },
                .{ .name = shared, .checked_name = shared, .payloads = &.{} },
            }),
            .ext = root,
        } });
    }
    diagnostics = .{};
    const flat = try graph.flattenTagRow(root);
    try std.testing.expectEqual(@as(usize, count + 1), flat.tags.len);
    try std.testing.expectEqual(shared, flat.tags[0].name);
    try std.testing.expectEqual(empty, flat.ext);
    try std.testing.expect(diagnostics.union_find_resolutions <= (count + 1) * 3);
    for (flat.tags[1..], 0..) |tag, index| {
        var buffer: [32]u8 = undefined;
        try std.testing.expectEqualStrings(try std.fmt.bufPrint(&buffer, "Tag{d:0>4}", .{index}), name_store.tagLabelText(tag.name));
    }

    var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = 0 });
    graph.allocator = failing.allocator();
    defer graph.allocator = gpa;
    for (0..100) |_| {
        const repeated = try graph.flattenTagRow(root);
        try std.testing.expectEqual(flat.tags.ptr, repeated.tags.ptr);
    }
    try std.testing.expectEqual(@as(usize, 0), failing.alloc_index);
}

test "issue 11235: sorted tag row unification retains shared payload equalities" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const a = try name_store.internTagLabel("A");
    const b = try name_store.internTagLabel("B");
    const shared = try name_store.internTagLabel("Shared");
    const payload = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const str = try graph.newNode(.{ .primitive = .str });
    const left = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{
            .{ .name = shared, .checked_name = shared, .payloads = try graph.arena().dupe(NodeId, &.{payload}) },
            .{ .name = a, .checked_name = a, .payloads = &.{} },
        }),
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) }),
    } });
    const right = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{
            .{ .name = shared, .checked_name = shared, .payloads = try graph.arena().dupe(NodeId, &.{str}) },
            .{ .name = b, .checked_name = b, .payloads = &.{} },
        }),
        .ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) }),
    } });
    try graph.unify(left, right);
    try std.testing.expectEqual(graph.find(payload), graph.find(str));
    const flat = try graph.flattenTagRow(left);
    try std.testing.expectEqual(@as(usize, 3), flat.tags.len);
    for ([_]names.TagNameId{ a, b, shared }, flat.tags) |label, tag| {
        try std.testing.expectEqual(label, tag.name);
    }
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

fn expectNominalBackingIndexConsistent(graph: *InstGraph) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    var active_count: u32 = 0;
    for (graph.nominal_backing_instances.items, 0..) |instance, instance_index| {
        if (!instance.active) continue;
        active_count += 1;
        const instance_id: NominalBackingInstanceId = @enumFromInt(@as(u32, @intCast(instance_index)));
        const key = NominalBackingKey{
            .declaration = instance.declaration,
            .args = instance.args,
        };
        try std.testing.expectEqual(instance_id, graph.nominal_backing_index.get(key).?);
        for (instance.args, 0..) |root, arg_index| {
            const occurrences = graph.nominal_backings_by_root.getPtr(root) orelse return error.TestUnexpectedResult;
            var matching_occurrences: usize = 0;
            for (occurrences.items) |occurrence| {
                if (occurrence.instance == instance_id and occurrence.arg_index == arg_index) {
                    matching_occurrences += 1;
                }
            }
            try std.testing.expectEqual(@as(usize, 1), matching_occurrences);
        }
    }
    try std.testing.expectEqual(active_count, graph.nominal_backing_index.count());

    var roots = graph.nominal_backings_by_root.iterator();
    while (roots.next()) |entry| {
        for (entry.value_ptr.items) |occurrence| {
            const instance = graph.nominal_backing_instances.items[@intFromEnum(occurrence.instance)];
            if (!instance.active) continue;
            try std.testing.expect(occurrence.arg_index < instance.args.len);
            try std.testing.expectEqual(entry.key_ptr.*, instance.args[occurrence.arg_index]);
        }
    }
}

test "nominal backing index rekeys root tuples and merges collisions" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_bytes = [_]u8{0xAB} ** 32;
    const other_module_bytes = [_]u8{0xCD} ** 32;
    const a = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const b = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const c = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const d = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });

    // Repeated argument positions must both migrate. Once a and b merge, the
    // two exact keys collapse and their already-published backing nodes become
    // one solver class.
    const repeated_left = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const repeated_right = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.putNominalBackingNode(module_bytes, 1, &.{ a, a }, repeated_left);
    try graph.putNominalBackingNode(module_bytes, 1, &.{ b, b }, repeated_right);
    try std.testing.expectEqual(repeated_left, graph.nominalBackingNode(module_bytes, 1, &.{ a, a }).?);
    try std.testing.expectEqual(repeated_right, graph.nominalBackingNode(module_bytes, 1, &.{ b, b }).?);
    try std.testing.expect(graph.nominalBackingNode(other_module_bytes, 1, &.{ a, a }) == null);
    try std.testing.expect(graph.nominalBackingNode(module_bytes, 2, &.{ a, a }) == null);

    // A partial tuple match stays distinct until every argument class agrees.
    const pair_left = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const pair_right = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.putNominalBackingNode(module_bytes, 2, &.{ a, c }, pair_left);
    try graph.putNominalBackingNode(module_bytes, 2, &.{ b, d }, pair_right);
    try expectNominalBackingIndexConsistent(graph);

    try graph.unify(a, b);

    try expectNominalBackingIndexConsistent(graph);
    try std.testing.expect(graph.sameClass(repeated_left, repeated_right));
    try std.testing.expectEqual(
        graph.find(repeated_left),
        graph.find(graph.nominalBackingNode(module_bytes, 1, &.{ b, a }).?),
    );
    try std.testing.expectEqual(pair_left, graph.nominalBackingNode(module_bytes, 2, &.{ a, c }).?);
    try std.testing.expectEqual(pair_right, graph.nominalBackingNode(module_bytes, 2, &.{ b, d }).?);
    try std.testing.expect(!graph.sameClass(pair_left, pair_right));

    try graph.unify(c, d);

    try expectNominalBackingIndexConsistent(graph);
    try std.testing.expect(graph.sameClass(pair_left, pair_right));
    try std.testing.expectEqual(
        graph.find(pair_left),
        graph.find(graph.nominalBackingNode(module_bytes, 2, &.{ b, c }).?),
    );

    // Make the already-migrated a/b root lose once more. This also consumes
    // stale occurrences left by the first collision instead of forwarding
    // them into the new winner's list.
    const e = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    try graph.unify(a, e);
    try expectNominalBackingIndexConsistent(graph);
    try std.testing.expectEqual(
        graph.find(repeated_left),
        graph.find(graph.nominalBackingNode(module_bytes, 1, &.{ e, b }).?),
    );

    // Empty argument tuples need no reverse-root entries and remain exact.
    const empty_backing = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.putNominalBackingNode(module_bytes, 3, &.{}, empty_backing);
    try std.testing.expectEqual(empty_backing, graph.nominalBackingNode(module_bytes, 3, &.{}).?);
    try expectNominalBackingIndexConsistent(graph);

    var active_instances: usize = 0;
    for (graph.nominal_backing_instances.items) |instance| {
        if (instance.active) active_instances += 1;
    }
    try std.testing.expectEqual(@as(usize, 3), active_instances);
    try std.testing.expectEqual(@as(u32, 3), graph.nominal_backing_index.count());
}

test "related named instances reuse exact backing witnesses" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xAC} ** 32));
    const type_name = try name_store.internTypeName("Wrap");
    const other_type_name = try name_store.internTypeName("Other");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module = module_identity, .type_name = type_name };
    const other_def: Type.TypeDef = .{ .module = module_identity, .type_name = other_type_name };

    const request_arg = try graph.newNode(.{ .primitive = .bool });
    const checked_arg = try graph.newNode(.{ .primitive = .bool });
    try graph.unify(request_arg, checked_arg);
    const request_backing = try graph.newNode(.empty_tag_union);
    const checked_backing = try graph.newNode(.empty_tag_union);
    const unrelated_backing = try graph.newNode(.empty_tag_union);

    const request = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{request_arg}),
        .backing = .{ .node = request_backing, .use = .inspectable },
    }));
    const same_request = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{request_arg}),
        .backing = .{ .node = request_backing, .use = .inspectable },
    }));
    const checked_node = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{checked_arg}),
        .backing = .{ .node = checked_backing, .use = .inspectable },
    }));
    const unrelated = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{request_arg}),
        .backing = .{ .node = unrelated_backing, .use = .inspectable },
    }));
    const other_definition = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = other_def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().dupe(NodeId, &.{request_arg}),
        .backing = .{ .node = request_backing, .use = .inspectable },
    }));

    try graph.relateNamedInstances(request, checked_node);

    try std.testing.expect(!graph.sameClass(request, checked_node));
    try std.testing.expect(graph.sameRelatedNamedInstance(request, checked_node));
    try std.testing.expect(graph.sameRelatedNamedInstance(same_request, checked_node));
    try std.testing.expect(!graph.sameRelatedNamedInstance(unrelated, checked_node));
    try std.testing.expect(!graph.sameRelatedNamedInstance(other_definition, checked_node));

    // A raw subject may retain its old side-table entry after its main class
    // redirects. Both the old entry and the new representative must retain the
    // identity proved above, whichever class wins the merge.
    try graph.relateNamedInstances(unrelated, unrelated);
    try graph.union_(unrelated, checked_node);
    try std.testing.expect(graph.sameRelatedNamedInstance(request, checked_node));
    try std.testing.expect(graph.sameRelatedNamedInstance(request, unrelated));
    try std.testing.expect(!graph.sameRelatedNamedInstance(other_definition, unrelated));

    const fresh = try graph.newNode(graph.content(unrelated));
    try graph.union_(fresh, unrelated);
    try std.testing.expect(graph.sameRelatedNamedInstance(request, fresh));
    try std.testing.expect(graph.sameRelatedNamedInstance(request, checked_node));
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

    const inner_named = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = empty_args,
        .backing = .{ .node = structural_backing, .use = .inspectable },
    }));
    const outer_named = try graph.newNode(try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = empty_args,
        .backing = .{ .node = inner_named, .use = .inspectable },
    }));

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
    try graph.setContent(nominal, try graph.namedContent(.{
        .named_type = named_type,
        .def = def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    }));

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
    try graph.setContent(nominal, try graph.namedContent(.{
        .named_type = nominal_type,
        .def = nominal_def,
        .kind = .nominal,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    }));

    const alias = try graph.newNode(try graph.namedContent(.{
        .named_type = alias_type,
        .def = alias_def,
        .kind = .alias,
        .builtin_owner = null,
        .args = try graph.arena().alloc(NodeId, 0),
        .backing = .{ .node = nominal, .use = .inspectable },
    }));

    const before_nodes = graph.nodes.items.len;
    try graph.unify(nominal, alias);

    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
    try std.testing.expectEqual(nominal, graph.find(nominal));
    try std.testing.expectEqual(alias, graph.find(alias));
}

test "issue 11362: iterator-free finalization performs no graph walks" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const leaf = try graph.newNode(.{ .primitive = .str });
    _ = try graph.newNode(.{ .box = leaf });
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expectEqual(@as(u64, 0), diagnostics.union_find_resolutions);
    try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_nodes);
}

test "issue 11362: iterator index tracks argument unions collisions and provenance replacement" {
    try testGeneratedIteratorIndex(std.testing.allocator);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testGeneratedIteratorIndex, .{});
}

fn testGeneratedIteratorIndex(gpa: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const source: InstIteratorPublicSource = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{
            .module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32)),
            .type_name = try name_store.internTypeName("Iter"),
            .source_decl = 1,
        },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .backing = .{ .node = try graph.newNode(.empty_record), .use = .runtime_layout_only },
        .declared_order = &.{},
    };
    const item = try graph.newNode(.{ .primitive = .u64 });
    const component = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const other = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const public = try graph.newNode(try graph.namedContent(.{
        .named_type = source.named_type,
        .def = source.def,
        .kind = source.kind,
        .builtin_owner = source.builtin_owner,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = source.backing,
    }));
    var named = graph.content(public).named.*;
    named.def.iterator_kind = .list;
    named.def.iterator_representation = .minted;
    named.def.iterator_depth = 1;
    named.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = source });
    named.args = try graph.arena().dupe(NodeId, &.{ item, component, component });
    const first = try graph.newNode(try graph.namedContent(named));
    named.args = try graph.arena().dupe(NodeId, &.{ item, other, other });
    const second = try graph.newNode(try graph.namedContent(named));
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ component, component }, null).?);
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{ other, other }, null).?);
    try graph.unify(component, other);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ other, component }, null).?);
    // Changing the first candidate's evidence must reveal the equal second
    // candidate, without joining their classes merely because keys coincide.
    const evidence: names.TypeDigest = .{ .bytes = @splat(7) };
    var changed = graph.content(first).named.*;
    changed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = evidence, .public_source = changed.generated_iterator.?.public_source });
    try graph.setContent(first, try graph.namedContent(changed));
    try std.testing.expect(!graph.sameClass(first, second));
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{ component, other }, null).?);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{ component, other }, evidence).?);
    // Content replacement changes the exact construction key, including tier.
    changed.def.iterator_kind = .forced_dynamic;
    changed.def.iterator_representation = .forced_dynamic;
    changed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = changed.generated_iterator.?.public_source });
    changed.args = try graph.arena().dupe(NodeId, &.{item});
    try graph.setContent(first, try graph.namedContent(changed));
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null).?);
    try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{ component, other }, evidence) == null);
    try std.testing.expect(graph.findGeneratedIterator(public, .map, &.{ component, other }, null) == null);
    // A probe's work is independent of unrelated graph nodes.
    for (0..100) |_| _ = try graph.newNode(.empty_record);
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{ component, other }, null).?);
    try std.testing.expect(diagnostics.union_find_resolutions < 15);
    const alias = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    try graph.unify(alias, second);
    try std.testing.expectEqual(graph.find(second), graph.findGeneratedIterator(public, .list, &.{ component, other }, null).?);
}

test "issue 11362: iterator-free finalization performs no graph traversal" {
    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(std.testing.allocator, &type_store, &name_store);
    defer graph.destroy();
    _ = try graph.newNode(.{ .primitive = .str });
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expectEqual(@as(u64, 0), diagnostics.union_find_resolutions);
}

test "issue 11362: generated iterator index follows root unions and producer replacement" {
    const gpa = std.testing.allocator;
    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();
    const module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32));
    const type_name = try name_store.internTypeName("Iter");
    const item = try graph.newNode(.{ .primitive = .u64 });
    const component = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const backing = try graph.newNode(.empty_record);
    const public_named: InstNamed = .{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{ .module = module, .type_name = type_name },
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = try graph.arena().dupe(NodeId, &.{item}),
        .backing = .{ .node = backing, .use = .runtime_layout_only },
    };
    const public = try graph.newNode(try graph.namedContent(public_named));
    var minted = public_named;
    minted.def.iterator_kind = .list;
    minted.def.iterator_representation = .minted;
    minted.args = try graph.arena().dupe(NodeId, &.{ item, component });
    minted.backing.?.authority = .generated_private;
    minted.generated_iterator = try graph.generatedIterator(.{
        .callable_evidence = null,
        .public_source = .{
            .named_type = public_named.named_type,
            .def = public_named.def,
            .kind = public_named.kind,
            .builtin_owner = .iter,
            .backing = public_named.backing.?,
            .declared_order = &.{},
        },
    });
    const first = try graph.newNode(try graph.namedContent(minted));
    const RecursiveMinted = struct { graph: *InstGraph, named: InstNamed };
    const second = try graph.addRecursiveNode(RecursiveMinted{ .graph = graph, .named = minted }, struct {
        fn fill(context: RecursiveMinted, _: NodeId) Allocator.Error!InstNode {
            return context.graph.namedContent(context.named);
        }
    }.fill);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{component}, null).?);
    try std.testing.expectEqual(@as(u32, 1), graph.generated_iterator_index.count());
    try std.testing.expect(graph.generated_iterator_nodes > 0);
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .single, &.{component}, null));
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .list, &.{}, null));

    // Arguments are compared by their current class, not their minted id.
    const resolved = try graph.newNode(.{ .primitive = .str });
    try graph.unify(component, resolved);
    try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{resolved}, null).?);
    try graph.union_(second, first);
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, null).?);

    // Changing callable evidence moves the live node to a different bucket.
    minted.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = .{ .bytes = @splat(0x62) }, .public_source = minted.generated_iterator.?.public_source });
    try graph.setContent(second, try graph.namedContent(minted));
    try std.testing.expectEqual(@as(?NodeId, null), graph.findGeneratedIterator(public, .list, &.{component}, null));
    try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, minted.generated_iterator.?.callable_evidence).?);
    try std.testing.expectEqual(@as(u32, 1), graph.generated_iterator_index.count());

    // Removing provenance removes membership; permanent request identities live on.
    graph.registerConstructorEvidenceRequest(second);
    try graph.registerRequestSourceInterface(second, public);
    try graph.setContent(second, try graph.namedContent(public_named));
    try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_index.count());
    try std.testing.expect(graph.requestPropagatesConstructorEvidence(second));
    try std.testing.expectEqual(public, graph.requestSourceInterface(second).?);
    try std.testing.expect(graph.generated_iterator_nodes > 0);
}

test "iterator finalization guards perform no graph walks without provenance" {
    const gpa = std.testing.allocator;
    var types = Type.Store.init(gpa);
    defer types.deinit();
    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();
    const graph = try InstGraph.create(gpa, &types, &name_store);
    defer graph.destroy();
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    for (0..100) |_| _ = try graph.newNode(.empty_record);
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expectEqual(@as(u64, 0), diagnostics.union_find_resolutions);
}

test "generated iterator index preserves evidence, argument classes, unions and replacements" {
    const Scenario = struct {
        fn run(allocator: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
            var types = Type.Store.init(allocator);
            defer types.deinit();
            var name_store = names.NameStore.init(allocator);
            defer name_store.deinit();
            const graph = try InstGraph.create(allocator, &types, &name_store);
            defer graph.destroy();
            const module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32));
            const type_name = try name_store.internTypeName("Iter");
            const item = try graph.newNode(.{ .primitive = .bool });
            const component = try graph.newNode(.empty_record);
            const other_component = try graph.newNode(.empty_record);
            const public_source: InstIteratorPublicSource = .{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                .def = .{ .module = module, .type_name = type_name },
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .backing = .{ .node = component, .use = .runtime_layout_only },
                .declared_order = &.{},
            };
            const public = try graph.newNode(try graph.namedContent(.{
                .named_type = public_source.named_type,
                .def = public_source.def,
                .kind = public_source.kind,
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{item}),
                .backing = public_source.backing,
            }));
            var generated = graph.content(public).named.*;
            generated.def.iterator_kind = .list;
            generated.def.iterator_representation = .minted;
            generated.def.iterator_depth = 1;
            generated.args = try graph.arena().dupe(NodeId, &.{ item, component });
            generated.backing.?.authority = .generated_private;
            generated.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = public_source });
            var without_provenance = generated;
            without_provenance.generated_iterator = null;
            const first = try graph.newNode(try graph.namedContent(without_provenance));
            try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_nodes);
            try graph.setContent(first, try graph.namedContent(generated));
            try std.testing.expectEqual(@as(u32, 1), graph.generated_iterator_nodes);
            const second = try graph.newNode(try graph.namedContent(generated));
            try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{component}, null).?);
            try std.testing.expect(graph.findGeneratedIterator(public, .map, &.{component}, null) == null);
            try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{other_component}, null) == null);
            const evidence: names.TypeDigest = .{ .bytes = @splat(7) };
            try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{component}, evidence) == null);
            generated.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = evidence, .public_source = generated.generated_iterator.?.public_source });
            const with_evidence = try graph.newNode(try graph.namedContent(generated));
            try std.testing.expectEqual(with_evidence, graph.findGeneratedIterator(public, .list, &.{component}, evidence).?);
            generated.def.source_decl = 17;
            const other_decl = try graph.newNode(try graph.namedContent(generated));
            try std.testing.expectEqual(other_decl, graph.findGeneratedIterator(other_decl, .list, &.{component}, evidence).?);
            try std.testing.expectEqual(with_evidence, graph.findGeneratedIterator(public, .list, &.{component}, evidence).?);

            var rekeyed = graph.content(first).named.*;
            rekeyed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = evidence, .public_source = rekeyed.generated_iterator.?.public_source });
            try graph.setContent(first, try graph.namedContent(rekeyed));
            try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{component}, evidence).?);
            rekeyed.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = rekeyed.generated_iterator.?.public_source });
            try graph.setContent(first, try graph.namedContent(rekeyed));
            try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{component}, null).?);
            try std.testing.expectEqual(with_evidence, graph.findGeneratedIterator(public, .list, &.{component}, evidence).?);

            try graph.unify(other_component, component);
            try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{other_component}, null).?);
            const request = try graph.newNode(.{ .func = .{
                .args = try graph.arena().dupe(NodeId, &.{first}),
                .ret = item,
            } });
            const source = try graph.newNode(.{ .func = .{
                .args = try graph.arena().dupe(NodeId, &.{public}),
                .ret = item,
            } });
            try graph.registerRequestSourceInterface(request, source);
            graph.registerConstructorEvidenceRequest(request);
            try graph.union_(second, first);
            try std.testing.expectEqual(source, graph.requestSourceInterface(request).?);
            try std.testing.expect(graph.requestPropagatesConstructorEvidence(request));
            try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, null).?);

            var dynamic = graph.content(second).named.*;
            dynamic.def.iterator_kind = .forced_dynamic;
            dynamic.def.iterator_representation = .forced_dynamic;
            dynamic.def.iterator_depth = 0;
            dynamic.args = try graph.arena().dupe(NodeId, &.{item});
            graph.setContent(second, try graph.namedContent(dynamic)) catch |err| {
                // Failed index growth must preserve both old content and key.
                try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .list, &.{component}, null).?);
                return err;
            };
            try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{component}, null) == null);
            try std.testing.expectEqual(second, graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null).?);
            try graph.setContent(second, .zst);
            try std.testing.expect(graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null) == null);
            try std.testing.expectEqual(@as(u32, 2), graph.generated_iterator_index.count());
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Scenario.run, .{});
}

test "iterator-free finalization performs no graph resolutions" {
    var types = Type.Store.init(std.testing.allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(std.testing.allocator, &types, &name_store);
    defer graph.destroy();
    _ = try graph.newNode(.{ .primitive = .str });
    var diagnostics: GraphDiagnostics = .{};
    graph.setDiagnostics(&diagnostics);
    try graph.finalizeGeneratedIteratorRepresentations();
    try graph.finalizeGeneratedIteratorIdentities();
    try std.testing.expectEqual(@as(u64, 0), diagnostics.union_find_resolutions);
}

test "generated iterator index follows content replacement and argument unions" {
    const Test = struct {
        fn run(gpa: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
            var types = Type.Store.init(gpa);
            defer types.deinit();
            var name_store = names.NameStore.init(gpa);
            defer name_store.deinit();
            const graph = try InstGraph.create(gpa, &types, &name_store);
            defer graph.destroy();
            // Every failed insertion/replacement must leave all previously created
            // nodes in their exact buckets, with no dangling candidate ids.
            errdefer {
                for (graph.generated_iterator_index.slots) |slot| {
                    const entry = slot orelse continue;
                    var next: ?NodeId = entry.value;
                    while (next) |node| {
                        std.debug.assert(@intFromEnum(node) < graph.nodes.items.len);
                        const named = graph.nodes.items[@intFromEnum(node)].named;
                        var key = GeneratedIteratorKey.fromNamed(named).?;
                        key.args = &.{};
                        std.debug.assert(GeneratedIteratorLookupContext.eql(.{ .graph = graph }, .{
                            .key = key,
                            .item = named.args[0],
                            .components = named.args[1..],
                        }, entry.key));
                        next = graph.generated_iterator_entries.get(node).?.next;
                    }
                }
                for (graph.nodes.items, 0..) |content, i| {
                    if (content == .named and GeneratedIteratorKey.fromNamed(content.named) != null) {
                        const key = graph.generated_iterator_entries.get(@enumFromInt(i)).?.key;
                        var next: ?NodeId = graph.generated_iterator_index.get(key).?;
                        while (next) |node| {
                            if (@intFromEnum(node) == i) break;
                            next = graph.generated_iterator_entries.get(node).?.next;
                        } else unreachable;
                    }
                }
            }
            const backing = try graph.newNode(.empty_record);
            const item = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
            const other_item = try graph.newNode(.{ .primitive = .str });
            const component = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
            const other_component = try graph.newNode(.{ .primitive = .u8 });
            const source: InstIteratorPublicSource = .{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(9) },
                .def = .{
                    .module = try name_store.internModuleIdentity(&([_]u8{0x62} ** 32)),
                    .type_name = try name_store.internTypeName("Iter"),
                },
                .kind = .@"opaque",
                .builtin_owner = .iter,
                .backing = .{ .node = backing, .use = .runtime_layout_only },
                .declared_order = &.{},
            };
            const public = try graph.newNode(try graph.namedContent(.{
                .named_type = source.named_type,
                .def = source.def,
                .kind = source.kind,
                .builtin_owner = .iter,
                .args = try graph.arena().dupe(NodeId, &.{other_item}),
                .backing = source.backing,
            }));
            var generated = graph.content(public).named.*;
            generated.def.iterator_kind = .list;
            generated.def.iterator_representation = .minted;
            generated.def.iterator_depth = 1;
            generated.args = try graph.arena().dupe(NodeId, &.{ item, component });
            generated.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = null, .public_source = source });
            const first = try graph.newNode(try graph.namedContent(generated));
            const second = try graph.newNode(try graph.namedContent(generated));
            try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{other_component}, null) == null);
            try graph.unify(item, other_item);
            try graph.unify(component, other_component);
            try std.testing.expectEqual(first, graph.findGeneratedIterator(public, .list, &.{other_component}, null).?);
            try graph.unify(second, first);
            try std.testing.expectEqual(graph.find(first), graph.findGeneratedIterator(public, .list, &.{other_component}, null).?);
            const root = graph.find(first);
            generated.def.iterator_kind = .forced_dynamic;
            generated.def.iterator_representation = .forced_dynamic;
            generated.args = try graph.arena().dupe(NodeId, &.{item});
            try graph.setContent(root, try graph.namedContent(generated));
            try std.testing.expect(graph.findGeneratedIterator(public, .list, &.{other_component}, null) == null);
            try std.testing.expectEqual(root, graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null).?);
            generated.generated_iterator = try graph.generatedIterator(.{ .callable_evidence = .{ .bytes = @splat(7) }, .public_source = generated.generated_iterator.?.public_source });
            try graph.setContent(root, try graph.namedContent(generated));
            try std.testing.expect(graph.findGeneratedIterator(public, .forced_dynamic, &.{}, null) == null);
            try std.testing.expectEqual(root, graph.findGeneratedIterator(public, .forced_dynamic, &.{}, generated.generated_iterator.?.callable_evidence).?);
            try graph.setContent(root, .zst);
            try std.testing.expectEqual(@as(u32, 0), graph.generated_iterator_index.count());
            try std.testing.expect(graph.generated_iterator_nodes > 0);
        }
    };
    try Test.run(std.testing.allocator);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Test.run, .{});
}

test "interface constraints preserve open rows and independent request variables" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const tail = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const label = try name_store.internTagLabel("InvalidJson");
    const row = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = label, .checked_name = label, .payloads = &.{} }}),
        .ext = tail,
    } });
    const function = try graph.newNode(.{ .func = .{ .args = try graph.arena().dupe(NodeId, &.{row}), .ret = row } });
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{ function, tail });
    const before = try constraints.identity(graph);
    const first = try constraints.instantiate(graph);
    const second = try constraints.instantiate(graph);
    const first_fn = try graph.functionNodes(first[0]);
    try std.testing.expect(graph.sameClass(first_fn.args[0], first_fn.ret));
    try std.testing.expect(graph.sameClass(graph.content(first_fn.ret).tag_union.ext, first[1]));
    try std.testing.expect(!graph.sameClass(first[1], second[1]));
    try std.testing.expectEqual(InstVariable.checkedVariable(null, .empty_tag_union), graph.content(first[1]).unresolved);
    const missing = try name_store.internTagLabel("MissingRequiredField");
    const added = try graph.newNode(.{ .tag_union = .{
        .tags = try graph.arena().dupe(InstTag, &.{.{ .name = missing, .checked_name = missing, .payloads = &.{} }}),
        .ext = try graph.newNode(.empty_tag_union),
    } });
    try graph.unify(first[1], added);
    try std.testing.expect(graph.content(second[1]) == .unresolved);
    const after = try constraints.identity(graph);
    try std.testing.expectEqualSlices(u8, before.bytes, after.bytes);
    const closed = try InterfaceConstraints.capture(graph, graph.arena(), first);
    const closed_identity = try closed.identity(graph);
    try std.testing.expect(!std.mem.eql(u8, before.bytes, closed_identity.bytes));
}

test "interface constraints preserve recursive and field-presence topology" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const value = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const slot = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const kind = try graph.newUndeterminedFieldKind();
    graph.registerUndeterminedFieldKindCells(kind, slot, value);
    const row = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const field = try name_store.internRecordFieldLabel("value");
    try graph.setContent(row, .{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = field, .ty = slot, .value_ty = value, .kind = .{ .undetermined = kind }, .default = null }}),
        .ext = try graph.newNode(.empty_record),
    } });
    try graph.setContent(value, .{ .list = row });
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{row});
    const first = (try constraints.instantiate(graph))[0];
    const second = (try constraints.instantiate(graph))[0];
    const first_field = graph.content(first).record.fields[0];
    const second_field = graph.content(second).record.fields[0];
    try std.testing.expectEqual(first, graph.content(first_field.value_ty.?).list);
    try std.testing.expect(first_field.kind.undetermined != second_field.kind.undetermined);
    graph.constrainUndeterminedFieldKind(first_field.kind.undetermined, .optional);
    try std.testing.expect(graph.resolvedFieldKind(second_field.kind) == null);
    const cells = graph.field_kinds.items[@intFromEnum(second_field.kind.undetermined)].cells.?;
    try std.testing.expectEqual(second_field.ty, cells.slot);
    try std.testing.expectEqual(second_field.value_ty.?, cells.value);
}

test "interface constraints retain settled producer evidence and exact leaf collision authority" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const child = try graph.newNode(.{ .primitive = .str });
    graph.markRecursiveValueSlot(child);
    const parent = try graph.newNode(.{ .list = child });
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{parent});
    const copied = (try constraints.instantiate(graph))[0];
    const copied_child = graph.content(copied).list;
    try std.testing.expect(!graph.sameClass(child, copied_child));
    try std.testing.expect(graph.isRecursiveValueSlot(copied_child));

    const str_node = try graph.newNode(.{ .primitive = .str });
    const bool_node = try graph.newNode(.{ .primitive = .bool });
    const str_constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{str_node});
    const bool_constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{bool_node});
    const str_identity = try str_constraints.identity(graph);
    const bool_identity = try bool_constraints.identity(graph);
    // Force the candidate-byte collision. Exact type equality must still
    // reject it, even if every digest in the serialized shape collides.
    const collision: InterfaceConstraints.Identity = .{ .bytes = str_identity.bytes, .leaves = bool_identity.leaves };
    try std.testing.expect(!try str_identity.eql(collision, &types, &name_store));
    try std.testing.expect(try str_identity.eql(str_identity, &types, &name_store));
}

test "interface constraints capture representation-neutral holes as variables" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();

    const u8_node = try graph.newNode(.{ .primitive = .u8 });
    const u8_list = try graph.newNode(.{ .list = u8_node });
    const str_node = try graph.newNode(.{ .primitive = .str });
    const str_list = try graph.newNode(.{ .list = str_node });

    var u8_classes: [1]?NodeId = undefined;
    const u8_constraints = try InterfaceConstraints.captureWithHoles(graph, graph.arena(), &.{ u8_list, u8_node }, &.{u8_node}, &u8_classes);
    var str_classes: [1]?NodeId = undefined;
    const str_constraints = try InterfaceConstraints.captureWithHoles(graph, graph.arena(), &.{ str_list, str_node }, &.{str_node}, &str_classes);
    try std.testing.expect(u8_classes[0] != null);
    try std.testing.expect(str_classes[0] != null);
    // Requests that differ only in a hole's settled instantiation share one identity.
    try std.testing.expect(try (try u8_constraints.identity(graph)).eql(try str_constraints.identity(graph), &types, &name_store));
    const exact_u8 = try InterfaceConstraints.capture(graph, graph.arena(), &.{ u8_list, u8_node });
    const exact_str = try InterfaceConstraints.capture(graph, graph.arena(), &.{ str_list, str_node });
    try std.testing.expect(!try (try exact_u8.identity(graph)).eql(try exact_str.identity(graph), &types, &name_store));

    // The hole instantiates as one fresh variable wherever the class occurs.
    const copied = try u8_constraints.instantiate(graph);
    try std.testing.expect(graph.content(copied[1]) == .unresolved);
    try std.testing.expect(graph.sameClass(graph.content(copied[0]).list, copied[1]));

    // A class carrying representation authority is captured as itself.
    const private_node = try graph.newNode(.{ .primitive = .u8 });
    graph.private_backing_roots.items[@intFromEnum(private_node)] = true;
    const private_list = try graph.newNode(.{ .list = private_node });
    var private_classes: [1]?NodeId = undefined;
    const private_constraints = try InterfaceConstraints.captureWithHoles(graph, graph.arena(), &.{ private_list, private_node }, &.{private_node}, &private_classes);
    try std.testing.expect(private_classes[0] == null);
    try std.testing.expect(!try (try private_constraints.identity(graph)).eql(try u8_constraints.identity(graph), &types, &name_store));
}

test "interface constraints distinguish variable sharing defaults and field-kind relationships" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const a = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const b = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_tag_union) });
    const c = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, .empty_record) });
    const shared = try InterfaceConstraints.capture(graph, graph.arena(), &.{ a, a });
    const fresh_shared = try InterfaceConstraints.capture(graph, graph.arena(), &.{ b, b });
    const distinct = try InterfaceConstraints.capture(graph, graph.arena(), &.{ a, b });
    const different_default = try InterfaceConstraints.capture(graph, graph.arena(), &.{ c, c });
    const numeric = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(.mono_specialization, null) });
    const numeric_constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{numeric});
    const numeric_copy = (try numeric_constraints.instantiate(graph))[0];
    try std.testing.expectEqual(InstVariable.checkedVariable(.mono_specialization, null), graph.content(numeric_copy).unresolved);
    const shared_identity = try shared.identity(graph);
    try std.testing.expect(try shared_identity.eql(try fresh_shared.identity(graph), &types, &name_store));
    try std.testing.expect(!try shared_identity.eql(try distinct.identity(graph), &types, &name_store));
    try std.testing.expect(!try shared_identity.eql(try different_default.identity(graph), &types, &name_store));

    const field = try name_store.internRecordFieldLabel("field");
    const kind = try graph.newUndeterminedFieldKind();
    const str = try graph.newNode(.{ .primitive = .str });
    graph.registerUndeterminedFieldKindCells(kind, str, str);
    graph.constrainUndeterminedFieldKind(kind, .required);
    const row = try graph.newNode(.{ .record = .{
        .fields = try graph.arena().dupe(InstField, &.{.{ .name = field, .ty = str, .value_ty = str, .kind = .{ .undetermined = kind }, .default = null }}),
        .ext = try graph.newNode(.empty_record),
    } });
    const required = try InterfaceConstraints.capture(graph, graph.arena(), &.{row});
    const copy = (try required.instantiate(graph))[0];
    const copied_kind = graph.content(copy).record.fields[0].kind;
    try std.testing.expect(copied_kind == .undetermined);
    try std.testing.expect(copied_kind.undetermined != kind);
    try std.testing.expectEqual(ResolvedFieldKind.required, graph.resolvedFieldKind(copied_kind).?);
}

test "interface constraints preserve nominal equality and independent private backing authority" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const def: Type.TypeDef = .{
        .module = try name_store.internModuleIdentity(&([_]u8{0x5A} ** 32)),
        .type_name = try name_store.internTypeName("Private"),
    };
    var roots: [2]NodeId = undefined;
    for (&roots) |*root| root.* = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = def,
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = try graph.newNode(.empty_record), .use = .runtime_layout_only, .authority = .generated_private },
    }));
    const first_identity = try (try InterfaceConstraints.capture(graph, graph.arena(), &.{roots[0]})).identity(graph);
    var renamed = graph.content(roots[0]).named.*;
    renamed.named_type.ty = testCheckedTypeId(2);
    const equivalent = try graph.newNode(try graph.namedContent(renamed));
    const equivalent_identity = try (try InterfaceConstraints.capture(graph, graph.arena(), &.{equivalent})).identity(graph);
    try std.testing.expect(try first_identity.eql(equivalent_identity, &types, &name_store));
    try graph.relateNamedInstances(roots[0], roots[1]);
    const backing_list = try graph.newNode(.{ .list = graph.content(roots[0]).named.backing.?.node });
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{ backing_list, roots[0], roots[1] });
    const first = try constraints.instantiate(graph);
    const second = try constraints.instantiate(graph);
    try std.testing.expect(graph.sameRelatedNamedInstance(first[1], first[2]));
    try std.testing.expect(!graph.sameClass(first[1], first[2]));
    try std.testing.expect(!graph.sameRelatedNamedInstance(first[1], second[1]));
    const first_backing = graph.content(first[1]).named.backing.?;
    const second_backing = graph.content(second[1]).named.backing.?;
    try std.testing.expect(graph.sameClass(graph.content(first[0]).list, first_backing.node));
    try std.testing.expectEqual(Type.BackingAuthority.generated_private, first_backing.authority);
    try std.testing.expect(!graph.sameClass(first_backing.node, second_backing.node));
}

test "interface constraints preserve finalized private representation witnesses" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const private = try graph.newNode(try graph.namedContent(.{
        .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
        .def = .{
            .module = try name_store.internModuleIdentity(&([_]u8{0xC3} ** 32)),
            .type_name = try name_store.internTypeName("FinalizedPrivate"),
        },
        .kind = .@"opaque",
        .builtin_owner = null,
        .args = &.{},
        .backing = .{ .node = try graph.newNode(.empty_record), .use = .runtime_layout_only, .authority = .generated_private },
    }));
    var retained = GraphTypeFinals.initRetainedTypeView(graph);
    defer retained.deinit();
    const durable = try retained.sealType(try graph.activeTypeViewForNode(private));
    const imported = try graph.importMono(durable);
    try std.testing.expect(try graph.containsFinishedMono(imported));
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{imported});
    const replayed = (try constraints.instantiate(graph))[0];
    try std.testing.expect(try graph.containsFinishedMono(replayed));
    try std.testing.expect(!graph.sameClass(imported, replayed));
}

test "interface constraints keep cycles open when a later edge reaches a variable" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const root = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const cycle = try graph.newNode(.{ .list = root });
    const variable = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    try graph.setContent(root, .{ .tuple = try graph.arena().dupe(NodeId, &.{ cycle, variable }) });
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &.{ root, cycle, variable });
    const first = try constraints.instantiate(graph);
    const second = try constraints.instantiate(graph);
    try std.testing.expectEqual(first[0], graph.content(first[1]).list);
    try std.testing.expectEqualSlices(NodeId, &.{ first[1], first[2] }, graph.content(first[0]).tuple);
    try std.testing.expect(!graph.sameClass(first[0], second[0]));
    try std.testing.expect(!graph.sameClass(first[2], second[2]));
    try graph.unify(first[2], try graph.newNode(.{ .primitive = .str }));
    try std.testing.expect(graph.content(second[2]) == .unresolved);
}

test "interface constraints separate declarations sharing a related backing group" {
    const allocator = std.testing.allocator;
    var types = Type.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    const graph = try InstGraph.create(allocator, &types, &name_store);
    defer graph.destroy();
    const module = try name_store.internModuleIdentity(&([_]u8{0xD2} ** 32));
    const shared_backing = try graph.newNode(.empty_record);
    var roots: [4]NodeId = undefined;
    for ([_][]const u8{ "First", "Second" }, 0..) |name, i| {
        const def: Type.TypeDef = .{ .module = module, .type_name = try name_store.internTypeName(name) };
        for (0..2) |j| {
            roots[i * 2 + j] = try graph.newNode(try graph.namedContent(.{
                .named_type = .{ .module = .{}, .ty = testCheckedTypeId(1) },
                .def = def,
                .kind = .nominal,
                .builtin_owner = null,
                .args = &.{},
                .backing = .{ .node = if (j == 0) shared_backing else try graph.newNode(.empty_record), .use = .inspectable, .authority = .generated_private },
            }));
        }
        try graph.relateNamedInstances(roots[i * 2], roots[i * 2 + 1]);
    }
    try std.testing.expectEqual(graph.relatedNamedInstanceRoot(roots[0]), graph.relatedNamedInstanceRoot(roots[2]));
    const constraints = try InterfaceConstraints.capture(graph, graph.arena(), &roots);
    const first = try constraints.instantiate(graph);
    const second = try constraints.instantiate(graph);
    for (roots, 0..) |left, i| {
        for (roots, 0..) |right, j| {
            try std.testing.expectEqual(graph.sameRelatedNamedInstance(left, right), graph.sameRelatedNamedInstance(first[i], first[j]));
        }
        try std.testing.expect(!graph.sameRelatedNamedInstance(first[i], second[i]));
    }
}
