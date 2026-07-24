//! Per-specialization type solver for Monotype lowering.
//!
//! Checked types instantiate into union-find nodes with explicit row
//! extension links; constraints unify nodes order-independently; Monotypes
//! are materialized views of solved nodes, refilled in place when their node
//! gains evidence. Cross-specialization edges import finished Monotypes as
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

/// A procedure template body request deferred to the end of the requesting
/// specialization, when that specialization's types are final. Requesting at
/// final types keeps specialization keys stable: two requests whose types
/// would later converge to one digest must resolve to one lowered body.
pub const DeferredTemplate = struct {
    fn_id: Ast.FnId,
    template_ref: names.ProcTemplate,
    /// Checked module whose method registry scope the specialization must use.
    method_scope: checked.ModuleId,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    /// The requester's live function type cell. `fn_ty` may be replaced by a
    /// sealed specialization-key snapshot, but callee body evidence must flow
    /// back through this node before the requester seals its body draft.
    requester_fn_node: NodeId,
    fn_ty: Type.TypeId,
    source_region_override: ?base.Region,
    current_entry_root: ?EntryRoot,
};

/// Identity of a node in a specialization's instantiation graph.
pub const NodeId = enum(u32) { _ };

// Current mutable Monotype output points to remove during graph sealing:
// `addMonoView`, `monoFor`, `fillMono`, `importMono`, row
// flattening, and the span materializers below. Completed Monotype views must
// expose only `TypeId`s and durable AST ids, never these graph-local ids.
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

const NodePair = struct {
    left: NodeId,
    right: NodeId,
};

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

const NominalBackingDeclarationContext = struct {
    pub fn hash(_: NominalBackingDeclarationContext, key: NominalBackingDeclaration) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(&key.module_bytes);
        var declaration_id = std.mem.nativeToLittle(u32, key.declaration_id);
        hasher.update(std.mem.asBytes(&declaration_id));
        return hasher.final();
    }

    pub fn eql(_: NominalBackingDeclarationContext, left: NominalBackingDeclaration, right: NominalBackingDeclaration) bool {
        return std.mem.eql(u8, left.module_bytes[0..], right.module_bytes[0..]) and
            left.declaration_id == right.declaration_id;
    }
};

/// One instantiated backing of a declaration, keyed by the argument cells
/// that seeded the declaration formal scope. Arguments are compared by their
/// current union-find roots: raw node ids drift as unification merges cells,
/// and comparing raw ids would treat the same instantiation as new each time
/// its arguments gain evidence.
const NominalBackingInstance = struct {
    args: []NodeId,
    node: NodeId,
};

/// Per-specialization type solver. Checked types instantiate into union-find
/// nodes with explicit row extension links; constraints unify nodes
/// order-independently; Monotypes are materialized views of solved nodes and
/// are refilled in place when their node gains evidence. Cross-specialization
/// edges import final Monotypes as closed structure, so a specialization that
/// tries to exceed its requested type is a unification conflict, not a silent
/// divergence.
pub const InstGraph = struct {
    allocator: Allocator,
    types: *Type.Store,
    name_store: *const names.NameStore,
    /// Monotypes lowered without body evidence (the builder-global type
    /// cache); empty tag unions inside them are unresolved slots.
    unsolved_monos: *const std.AutoHashMap(Type.TypeId, void),
    arena_impl: std.heap.ArenaAllocator,
    nodes: std.ArrayList(InstNode),
    versions: std.ArrayList(u32),
    processed_relations: std.AutoHashMap(RelationStamp, void),
    /// Materialized Monotype views per node root. The same root may have
    /// several views when roots with existing views are merged; all views of a
    /// root hold identical content.
    node_monos: std.AutoHashMap(NodeId, std.ArrayList(Type.TypeId)),
    /// Reverse view links, also the import memo: a Monotype already connected
    /// to this graph unifies with its node instead of being copied.
    mono_nodes: std.AutoHashMap(Type.TypeId, NodeId),
    /// Roots whose Monotype views need their content refilled. Drained when no
    /// constraint walk holds slices into the type store.
    dirty: std.ArrayList(NodeId),
    /// Set matching `dirty`, so each current root is queued at most once.
    dirty_set: std.AutoHashMap(NodeId, void),
    /// Current extension root for each row root. This is the authority for
    /// maintaining `row_parents`; stale extension edges are removed when row
    /// content changes.
    row_exts: std.AutoHashMap(NodeId, NodeId),
    /// Row nodes by the extension node they currently chain through. A change
    /// to an extension changes the flattened view of every row above it, so
    /// dirty marks propagate through these back references.
    row_parents: std.AutoHashMap(NodeId, std.ArrayList(NodeId)),
    /// Declaration-backed nominal backings already instantiated in this graph,
    /// bucketed by source declaration. Instances inside a bucket compare their
    /// argument cells by union-find root, so an instantiation stays shared
    /// after unification merges or redirects its argument nodes. This
    /// preserves per-argument correctness without rebuilding the same backing
    /// template repeatedly.
    nominal_backings: std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingDeclarationContext, 80),
    /// Template body requests made while lowering this specialization,
    /// processed once its body is complete and its types are final.
    deferred_templates: std.ArrayList(DeferredTemplate) = .empty,
    /// Worklist and per-walk relation dedup for `unify`. One walk is active at
    /// a time; a `unify` call issued while a walk is draining (a row-empty
    /// collapse inside `unifyConcrete`) queues its pair onto the active walk,
    /// which resolves the same constraints because unification is
    /// order-independent.
    unify_pending: std.ArrayList(NodePair) = .empty,
    unify_related: std.AutoHashMap(NodePair, void),
    unify_active: bool = false,
    /// Reused traversal state for `markDirty` (never re-entered: dirty marking
    /// only walks row back references).
    dirty_mark_pending: std.ArrayList(NodeId) = .empty,
    dirty_mark_visited: std.AutoHashMap(NodeId, void),
    /// Reused row-collection state for `flattenTagRow`/`flattenRecordRow`.
    /// Flattening reads node contents and allocates only fresh nodes and arena
    /// copies, so no call inside the collection window can start another
    /// flatten.
    flat_tags_scratch: std.ArrayList(InstTag) = .empty,
    flat_fields_scratch: std.ArrayList(InstField) = .empty,
    row_seen_scratch: std.AutoHashMap(NodeId, void),
    /// Reused chain-walk state for `findStructuralBackingNode`. Backing chains
    /// are short, so membership is a linear scan.
    backing_chain_scratch: std.ArrayList(NodeId) = .empty,

    pub fn create(
        allocator: Allocator,
        types: *Type.Store,
        name_store: *const names.NameStore,
        unsolved_monos: *const std.AutoHashMap(Type.TypeId, void),
    ) Allocator.Error!*InstGraph {
        const graph = try allocator.create(InstGraph);
        graph.* = .{
            .allocator = allocator,
            .types = types,
            .name_store = name_store,
            .unsolved_monos = unsolved_monos,
            .arena_impl = std.heap.ArenaAllocator.init(allocator),
            .nodes = .empty,
            .versions = .empty,
            .processed_relations = std.AutoHashMap(RelationStamp, void).init(allocator),
            .node_monos = std.AutoHashMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .mono_nodes = std.AutoHashMap(Type.TypeId, NodeId).init(allocator),
            .dirty = .empty,
            .dirty_set = std.AutoHashMap(NodeId, void).init(allocator),
            .row_exts = std.AutoHashMap(NodeId, NodeId).init(allocator),
            .row_parents = std.AutoHashMap(NodeId, std.ArrayList(NodeId)).init(allocator),
            .nominal_backings = std.HashMap(NominalBackingDeclaration, std.ArrayList(NominalBackingInstance), NominalBackingDeclarationContext, 80).init(allocator),
            .unify_related = std.AutoHashMap(NodePair, void).init(allocator),
            .dirty_mark_visited = std.AutoHashMap(NodeId, void).init(allocator),
            .row_seen_scratch = std.AutoHashMap(NodeId, void).init(allocator),
        };
        return graph;
    }

    pub fn destroy(self: *InstGraph) void {
        const allocator = self.allocator;
        self.backing_chain_scratch.deinit(allocator);
        self.row_seen_scratch.deinit();
        self.flat_fields_scratch.deinit(allocator);
        self.flat_tags_scratch.deinit(allocator);
        self.dirty_mark_visited.deinit();
        self.dirty_mark_pending.deinit(allocator);
        self.unify_related.deinit();
        self.unify_pending.deinit(allocator);
        self.deferred_templates.deinit(allocator);
        var views = self.node_monos.valueIterator();
        while (views.next()) |list| {
            list.deinit(allocator);
        }
        self.node_monos.deinit();
        var parents = self.row_parents.valueIterator();
        while (parents.next()) |list| {
            list.deinit(allocator);
        }
        var backing_buckets = self.nominal_backings.valueIterator();
        while (backing_buckets.next()) |bucket| {
            bucket.deinit(allocator);
        }
        self.nominal_backings.deinit();
        self.row_parents.deinit();
        self.row_exts.deinit();
        self.dirty_set.deinit();
        self.mono_nodes.deinit();
        self.dirty.deinit(allocator);
        self.processed_relations.deinit();
        self.versions.deinit(allocator);
        self.nodes.deinit(allocator);
        self.arena_impl.deinit();
        allocator.destroy(self);
    }

    pub fn arena(self: *InstGraph) Allocator {
        return self.arena_impl.allocator();
    }

    pub fn newNode(self: *InstGraph, node_content: InstNode) Allocator.Error!NodeId {
        const id: NodeId = @enumFromInt(@as(u32, @intCast(self.nodes.items.len)));
        try self.nodes.append(self.allocator, node_content);
        try self.versions.append(self.allocator, 0);
        try self.registerRowParent(id, node_content);
        return id;
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
                // Repoint the stored cell at its current root so repeated
                // lookups after heavy unification stay cheap.
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
        const stored_args = try self.arena().alloc(NodeId, args.len);
        for (stored_args, args) |*stored, arg| {
            stored.* = self.find(arg);
        }
        const bucket = try self.nominal_backings.getOrPut(.{
            .module_bytes = module_bytes,
            .declaration_id = declaration_id,
        });
        if (!bucket.found_existing) bucket.value_ptr.* = .empty;
        try bucket.value_ptr.append(self.allocator, .{
            .args = stored_args,
            .node = node,
        });
    }

    fn registerRowParent(self: *InstGraph, row: NodeId, node_content: InstNode) Allocator.Error!void {
        const row_root = self.find(row);
        const maybe_ext = switch (node_content) {
            .tag_union => |tag_union| tag_union.ext,
            .record => |record| record.ext,
            else => null,
        };
        const ext = if (maybe_ext) |raw_ext| self.find(raw_ext) else {
            try self.unregisterRowParent(row_root);
            return;
        };

        if (self.row_exts.get(row_root)) |old_ext| {
            if (old_ext == ext) {
                try self.addRowParent(ext, row_root);
                return;
            }
            self.removeRowParent(old_ext, row_root);
        }
        try self.row_exts.put(row_root, ext);
        try self.addRowParent(ext, row_root);
    }

    fn unregisterRowParent(self: *InstGraph, row: NodeId) Allocator.Error!void {
        const row_root = self.find(row);
        if (self.row_exts.fetchRemove(row_root)) |old| {
            self.removeRowParent(old.value, row_root);
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
            switch (self.nodes.items[@intFromEnum(current)]) {
                .redirect => |next| current = next,
                else => break,
            }
        }
        // Path compression: repoint every redirect on the chain at the root.
        var walk = id;
        while (walk != current) {
            const next = switch (self.nodes.items[@intFromEnum(walk)]) {
                .redirect => |next| next,
                else => unreachable,
            };
            self.nodes.items[@intFromEnum(walk)] = .{ .redirect = current };
            walk = next;
        }
        return current;
    }

    pub fn content(self: *InstGraph, id: NodeId) InstNode {
        return self.nodes.items[@intFromEnum(self.find(id))];
    }

    /// Return the graph node for one field of a record-shaped node. Field
    /// access is a type relation, so callers use this node directly instead of
    /// selecting a field from a temporary Monotype view and losing later row
    /// evidence.
    pub fn recordFieldNode(self: *InstGraph, raw_record: NodeId, name: names.RecordFieldNameId) Allocator.Error!NodeId {
        var seen = std.AutoHashMap(NodeId, void).init(self.allocator);
        defer seen.deinit();

        var current = self.find(raw_record);
        while (true) {
            const entry = try seen.getOrPut(current);
            if (entry.found_existing) {
                Common.invariant("instantiation record field access encountered a recursive named backing");
            }

            switch (self.nodes.items[@intFromEnum(current)]) {
                .named => |named| {
                    const backing = named.backing orelse
                        Common.invariant("instantiation record field access reached a named type without backing");
                    current = self.find((try self.structuralBackingNode(backing.node, named)).node);
                },
                .record => {
                    const row = try self.flattenRecordRow(current);
                    const wanted = self.fieldLabelText(name);
                    for (row.fields) |field| {
                        if (Ident.textEql(wanted, self.fieldLabelText(field.name))) return self.find(field.ty);
                    }
                    Common.invariant("instantiation record field access was absent from the receiver type");
                },
                else => Common.invariant("instantiation record field access had a non-record receiver type"),
            }
        }
    }

    /// Queue the views of a changed root and of every row whose flattened view
    /// chains through it for a content refill.
    fn markDirty(self: *InstGraph, changed: NodeId) Allocator.Error!void {
        self.dirty_mark_pending.clearRetainingCapacity();
        self.dirty_mark_visited.clearRetainingCapacity();
        try self.dirty_mark_pending.append(self.allocator, changed);
        while (self.dirty_mark_pending.pop()) |raw| {
            const root = self.find(raw);
            if (self.dirty_mark_visited.contains(root)) continue;
            try self.dirty_mark_visited.put(root, {});
            if (self.node_monos.contains(root)) {
                try self.queueDirty(root);
            }
            if (self.row_parents.get(root)) |parents| {
                try self.dirty_mark_pending.appendSlice(self.allocator, parents.items);
            }
            if (self.row_parents.get(raw)) |parents| {
                if (raw != root) try self.dirty_mark_pending.appendSlice(self.allocator, parents.items);
            }
        }
    }

    fn queueDirty(self: *InstGraph, raw_root: NodeId) Allocator.Error!void {
        const root = self.find(raw_root);
        const entry = try self.dirty_set.getOrPut(root);
        if (entry.found_existing) return;
        try self.dirty.append(self.allocator, root);
    }

    /// Redirect `loser` into `winner`, moving Monotype views and row back
    /// references across, and queueing affected views for a content refill.
    fn union_(self: *InstGraph, raw_winner: NodeId, raw_loser: NodeId) Allocator.Error!void {
        const winner = self.find(raw_winner);
        const loser = self.find(raw_loser);
        if (winner == loser) return;
        _ = self.dirty_set.remove(loser);
        try self.unregisterRowParent(loser);
        self.nodes.items[@intFromEnum(loser)] = .{ .redirect = winner };
        self.versions.items[@intFromEnum(winner)] +%= 1;
        if (self.node_monos.fetchRemove(loser)) |moved| {
            var moved_list = moved.value;
            const entry = try self.node_monos.getOrPut(winner);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.appendSlice(self.allocator, moved_list.items);
            for (moved_list.items) |ty| {
                try self.mono_nodes.put(ty, winner);
            }
            moved_list.deinit(self.allocator);
        }
        if (self.row_parents.fetchRemove(loser)) |moved| {
            var moved_list = moved.value;
            for (moved_list.items) |parent| {
                const parent_root = self.find(parent);
                try self.row_exts.put(parent_root, winner);
                try self.addRowParent(winner, parent_root);
            }
            moved_list.deinit(self.allocator);
        }
        try self.markDirty(winner);
    }

    /// Replace a root's content in place without queueing a Monotype refill.
    /// Returns whether the root's observable content changed.
    fn replaceContentNoDirty(self: *InstGraph, raw_root: NodeId, new_content: InstNode) Allocator.Error!bool {
        const root = self.find(raw_root);
        if (instNodeEql(self.nodes.items[@intFromEnum(root)], new_content)) return false;
        self.nodes.items[@intFromEnum(root)] = new_content;
        self.versions.items[@intFromEnum(root)] +%= 1;
        try self.registerRowParent(root, new_content);
        return true;
    }

    /// Replace a root's content in place and queue affected views for refill.
    fn setContent(self: *InstGraph, root: NodeId, new_content: InstNode) Allocator.Error!void {
        if (!try self.replaceContentNoDirty(root, new_content)) return;
        try self.markDirty(root);
    }

    pub fn unify(self: *InstGraph, a: NodeId, b: NodeId) Allocator.Error!void {
        try self.unify_pending.append(self.allocator, .{ .left = a, .right = b });
        // A unify issued while a walk is draining (a row-empty collapse inside
        // `unifyConcrete`) joins the active walk: constraints resolve
        // order-independently, so draining the pair later in the same walk is
        // equivalent to a nested walk, without nested traversal state.
        if (self.unify_active) return;
        self.unify_active = true;
        defer {
            self.unify_active = false;
            self.unify_pending.clearRetainingCapacity();
            self.unify_related.clearRetainingCapacity();
        }
        while (self.unify_pending.pop()) |pair| {
            try self.unifyRoots(pair.left, pair.right, &self.unify_pending, &self.unify_related);
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

        switch (left_content) {
            .redirect => unreachable,
            .unresolved => |left_var| switch (right_content) {
                .unresolved => |right_var| {
                    try self.setContent(right, .{ .unresolved = mergeVariables(left_var, right_var) });
                    try self.union_(right, left);
                },
                .named => |right_named| if (right_named.kind == .alias)
                    try self.unifyThroughBacking(right, right_content, left, pending)
                else
                    try self.union_(right, left),
                else => try self.union_(right, left),
            },
            else => switch (right_content) {
                .unresolved => switch (left_content) {
                    .named => |left_named| if (left_named.kind == .alias)
                        try self.unifyThroughBacking(left, left_content, right, pending)
                    else
                        try self.union_(left, right),
                    else => try self.union_(left, right),
                },
                else => try self.unifyConcrete(left, left_content, right, right_content, pending),
            },
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
            .primitive => |left_prim| switch (right_content) {
                .primitive => |right_prim| {
                    if (left_prim != right_prim) Common.invariant("instantiation unified two different primitive types");
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a primitive type with a non-primitive type"),
            },
            .list => |left_elem| switch (right_content) {
                .list => |right_elem| {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_elem });
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a List with a non-List type"),
            },
            .box => |left_elem| switch (right_content) {
                .box => |right_elem| {
                    try pending.append(self.allocator, .{ .left = left_elem, .right = right_elem });
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a Box with a non-Box type"),
            },
            .tuple => |left_items| switch (right_content) {
                .tuple => |right_items| {
                    if (left_items.len != right_items.len) Common.invariant("instantiation unified tuples of different arity");
                    for (left_items, right_items) |left_item, right_item| {
                        try pending.append(self.allocator, .{ .left = left_item, .right = right_item });
                    }
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a tuple with a non-tuple type"),
            },
            .func => |left_fn| switch (right_content) {
                .func => |right_fn| {
                    if (left_fn.args.len != right_fn.args.len) Common.invariant("instantiation unified functions of different arity");
                    for (left_fn.args, right_fn.args) |left_arg, right_arg| {
                        try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                    }
                    try pending.append(self.allocator, .{ .left = left_fn.ret, .right = right_fn.ret });
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a function with a non-function type"),
            },
            .tag_union => switch (right_content) {
                .tag_union => try self.unifyTagRows(left, right, pending),
                .empty_tag_union => try self.unifyRowWithEmpty(left, right, .tag_union),
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a tag union with a non-tag-union type"),
            },
            .empty_tag_union => switch (right_content) {
                .empty_tag_union => try self.union_(left, right),
                .tag_union => try self.unifyRowWithEmpty(right, left, .tag_union),
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified an empty tag union with an incompatible type"),
            },
            .record => switch (right_content) {
                .record => try self.unifyRecordRows(left, right, pending),
                .empty_record => try self.unifyRowWithEmpty(left, right, .record),
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a record with a non-record type"),
            },
            .empty_record => switch (right_content) {
                .empty_record => try self.union_(left, right),
                .record => try self.unifyRowWithEmpty(right, left, .record),
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified an empty record with an incompatible type"),
            },
            .named => |left_named| switch (right_content) {
                .named => |right_named| {
                    if (left_named.kind == .alias) {
                        try self.unifyThroughBacking(left, left_content, right, pending);
                        return;
                    }
                    if (right_named.kind == .alias) {
                        try self.unifyThroughBacking(right, right_content, left, pending);
                        return;
                    }
                    switch (Type.iteratorRelation(left_named, right_named)) {
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
                                try pending.append(self.allocator, .{
                                    .left = left_backing.node,
                                    .right = right_backing.node,
                                });
                            } else if (right_named.backing != null) {
                                Common.invariant("minted iterator join found backing on only one side");
                            }

                            // Close recursive `rest` references before the
                            // backing pair is drained. Otherwise each nominal
                            // unwrap creates another fresh structural node.
                            if (left_named.builtin_owner) |left_owner| {
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
                        if (!sameBuiltinOwner(left_named.builtin_owner, right_named.builtin_owner, .fields) and
                            !eitherBuiltinOwner(left_named.builtin_owner, right_named.builtin_owner, .iter) and
                            !eitherBuiltinOwner(left_named.builtin_owner, right_named.builtin_owner, .stream))
                        {
                            if (left_named.backing) |left_backing| {
                                if (right_named.backing) |right_backing| {
                                    try pending.append(self.allocator, .{ .left = left_backing.node, .right = right_backing.node });
                                }
                            }
                        }
                        try self.union_(left, right);
                        return;
                    }
                    try self.unifyThroughBacking(left, left_content, right, pending);
                },
                else => try self.unifyThroughBacking(left, left_content, right, pending),
            },
            .erased => |left_digest| switch (right_content) {
                .erased => |right_digest| {
                    if (!std.mem.eql(u8, left_digest.bytes[0..], right_digest.bytes[0..])) {
                        Common.invariant("instantiation unified two different erased types");
                    }
                    try self.union_(left, right);
                },
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified an erased type with an incompatible type"),
            },
            .zst => switch (right_content) {
                .zst => try self.union_(left, right),
                .named => try self.unifyThroughBacking(right, right_content, left, pending),
                else => Common.invariant("instantiation unified a zero-sized type with an incompatible type"),
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
        const named = switch (named_content) {
            .named => |named| named,
            else => unreachable,
        };
        const declared_backing = named.backing orelse
            Common.invariant("instantiation unified an opaque type without backing against a structural type");
        const backing = try self.structuralBackingNode(declared_backing.node, named);
        const backing_node = backing.node;
        if (backing.recursive) {
            if (named.kind == .alias) {
                Common.invariant("alias backing cycle reached Monotype instantiation");
            }
            switch (self.nodes.items[@intFromEnum(other)]) {
                .named => Common.invariant("recursive nominal backing met a different named type"),
                else => {
                    try self.union_(named_node, other);
                    return;
                },
            }
        }
        if (declared_backing.node != backing_node) {
            var compressed = named;
            compressed.backing = .{ .node = backing_node, .use = declared_backing.use };
            try self.setContent(named_node, .{ .named = compressed });
        }
        if (named.kind == .alias) {
            try pending.append(self.allocator, .{ .left = backing_node, .right = other });
            return;
        }
        switch (self.nodes.items[@intFromEnum(other)]) {
            .named => {
                try pending.append(self.allocator, .{ .left = backing_node, .right = other });
                return;
            },
            else => {},
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
        const seen = &self.backing_chain_scratch;
        seen.clearRetainingCapacity();
        var current = self.find(raw);
        while (true) {
            for (seen.items) |visited| {
                if (visited == current) return .{ .node = current, .recursive = true };
            }
            try seen.append(self.allocator, current);
            const next = self.structuralBackingNext(current, owner) orelse return .{ .node = current, .recursive = false };
            current = next;
        }
    }

    fn compressStructuralBacking(self: *InstGraph, raw: NodeId, owner: InstNamed, result: NodeId) Allocator.Error!void {
        var current = self.find(raw);
        while (current != result) {
            const named = switch (self.nodes.items[@intFromEnum(current)]) {
                .named => |named| named,
                else => Common.invariant("named backing compression reached a structural node before its result"),
            };
            if (named.kind != .alias and !self.sameNamedInstance(named, owner)) {
                Common.invariant("named backing compression reached a non-transparent named type");
            }
            const backing = named.backing orelse
                Common.invariant("named backing compression reached a named type without backing");
            const next = self.find(backing.node);
            if (backing.node != result) {
                var compressed = named;
                compressed.backing = .{ .node = result, .use = backing.use };
                try self.setContent(current, .{ .named = compressed });
            }
            current = next;
        }
    }

    fn structuralBackingNext(self: *InstGraph, raw: NodeId, owner: InstNamed) ?NodeId {
        const current = self.find(raw);
        switch (self.nodes.items[@intFromEnum(current)]) {
            .named => |named| {
                if (named.kind != .alias and !self.sameNamedInstance(named, owner)) return null;
                const backing = named.backing orelse
                    Common.invariant("named backing chain reached a named type without backing");
                return self.find(backing.node);
            },
            else => return null,
        }
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
            left.source_decl == right.source_decl and
            optionalDigestEql(left.generated, right.generated) and
            left.iterator_representation == right.iterator_representation and
            left.iterator_kind == right.iterator_kind and
            left.iterator_depth == right.iterator_depth;
    }

    fn optionalDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
        if (left == null and right == null) return true;
        if (left == null or right == null) return false;
        return std.mem.eql(u8, left.?.bytes[0..], right.?.bytes[0..]);
    }

    const RowKind = enum {
        tag_union,
        record,
    };

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
        const row = switch (self.nodes.items[@intFromEnum(root)]) {
            .tag_union => |row| row,
            else => Common.invariant("instantiation flattened a non-tag-union row"),
        };

        var ext = self.find(row.ext);
        switch (self.nodes.items[@intFromEnum(ext)]) {
            .unresolved, .empty_tag_union => {
                if (row.ext != ext) {
                    const flattened: InstNode = .{ .tag_union = .{ .tags = row.tags, .ext = ext } };
                    _ = try self.replaceContentNoDirty(root, flattened);
                }
                return .{ .tags = row.tags, .ext = ext };
            },
            else => {},
        }

        // The collection scratch never survives a call that could start
        // another flatten: the loop below only reads nodes and appends fresh
        // nodes, and the collected tags are copied into the arena before the
        // root is rewritten.
        const tags = &self.flat_tags_scratch;
        tags.clearRetainingCapacity();
        try tags.appendSlice(self.allocator, row.tags);

        const seen = &self.row_seen_scratch;
        seen.clearRetainingCapacity();
        try seen.put(root, {});

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further tags — every
                // tag on the cycle is already collected — but the row remains
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
                else => Common.invariant("instantiation tag row extended into a non-tag-union type"),
            }
        }

        const flat_tags = try self.arena().dupe(InstTag, tags.items);
        const flattened: InstNode = .{ .tag_union = .{ .tags = flat_tags, .ext = ext } };
        _ = try self.replaceContentNoDirty(root, flattened);
        return .{ .tags = flat_tags, .ext = ext };
    }

    fn flattenRecordRow(self: *InstGraph, raw_root: NodeId) Allocator.Error!FlatRecordRow {
        const root = self.find(raw_root);
        const row = switch (self.nodes.items[@intFromEnum(root)]) {
            .record => |row| row,
            else => Common.invariant("instantiation flattened a non-record row"),
        };

        var ext = self.find(row.ext);
        switch (self.nodes.items[@intFromEnum(ext)]) {
            .unresolved, .empty_record => {
                if (row.ext != ext) {
                    const flattened: InstNode = .{ .record = .{ .fields = row.fields, .ext = ext } };
                    _ = try self.replaceContentNoDirty(root, flattened);
                }
                return .{ .fields = row.fields, .ext = ext };
            },
            else => {},
        }

        // Same scratch discipline as `flattenTagRow`: the collection window
        // contains no call that could start another flatten.
        const fields = &self.flat_fields_scratch;
        fields.clearRetainingCapacity();
        try fields.appendSlice(self.allocator, row.fields);

        const seen = &self.row_seen_scratch;
        seen.clearRetainingCapacity();
        try seen.put(root, {});

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further fields —
                // every field on the cycle is already collected — but the row
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
                else => Common.invariant("instantiation record row extended into a non-record type"),
            }
        }

        const flat_fields = try self.arena().dupe(InstField, fields.items);
        const flattened: InstNode = .{ .record = .{ .fields = flat_fields, .ext = ext } };
        _ = try self.replaceContentNoDirty(root, flattened);
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
        switch (self.nodes.items[@intFromEnum(ext_root)]) {
            .unresolved => |variable| {
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
            },
            else => {
                const rest = try self.newNode(.{ .tag_union = .{
                    .tags = try self.arena().dupe(InstTag, tags),
                    .ext = tail_ext,
                } });
                try pending.append(self.allocator, .{ .left = ext_root, .right = rest });
            },
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
        switch (self.nodes.items[@intFromEnum(ext_root)]) {
            .unresolved => |variable| {
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
            },
            else => {
                const rest = try self.newNode(.{ .record = .{
                    .fields = try self.arena().dupe(InstField, fields),
                    .ext = tail_ext,
                } });
                try pending.append(self.allocator, .{ .left = ext_root, .right = rest });
            },
        }
    }

    /// Import a Monotype into the graph. A Monotype already linked to a node
    /// reconnects to it; an unlinked one copies in as closed structure, so a
    /// later attempt to widen it is a unification conflict rather than a silent
    /// mutation of another specialization's final type.
    pub fn importMono(self: *InstGraph, ty: Type.TypeId) Allocator.Error!NodeId {
        if (self.mono_nodes.get(ty)) |existing| return existing;
        const node = try self.newNode(.{ .unresolved = InstVariable.placeholder() });
        // One-way memo: every import is a finished Monotype from outside this
        // graph (ids materialized here hit the memo above), so it enters as a
        // snapshot. Registering a view would let this specialization's
        // evidence rewrite another specialization's final type, destabilizing
        // every digest taken from it.
        try self.mono_nodes.put(ty, node);

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
                // An empty tag union in a finished Monotype records a slot no
                // value reached: either genuinely uninhabited or a variable
                // defaulted at materialization. Local evidence supersedes it,
                // so it imports as an unresolved node rather than a closed row.
                if (span.len == 0) break :blk .{ .unresolved = InstVariable.row(.empty_tag_union) };
                const inst_tags = try self.arena().alloc(InstTag, span.len);
                for (0..span.len) |index| {
                    const tag = GuardedList.at(span, index);
                    inst_tags[index] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.importMonoSlice(types.span(tag.payloads)),
                    };
                }
                // A materialized row does not record whether its checked row
                // was open; rows narrowed per use position may gain tags from
                // the callee's own checked data, so imports stay extensible
                // and the requester observes any widening through the
                // post-lowering unification of request and definition types.
                break :blk .{ .tag_union = .{
                    .tags = inst_tags,
                    .ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) }),
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
                        .ty = try self.importMono(field.ty),
                    };
                }
                break :blk .{ .record = .{
                    .fields = inst_fields,
                    .ext = try self.newNode(.{ .unresolved = InstVariable.row(.empty_record) }),
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
                } else null,
                .declared_order = try self.importDeclaredFields(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentNoDirty(node, imported);
        return node;
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

    /// Register an existing Monotype as a view of a node, so the node's
    /// evidence refills that id in place.
    pub fn addMonoView(self: *InstGraph, node: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const root = self.find(node);
        try self.mono_nodes.put(ty, root);
        try self.registerMonoView(root, ty);
        try self.fillMono(root, ty);
        try self.drainDirty();
    }

    fn registerMonoView(self: *InstGraph, raw_node: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const root = self.find(raw_node);
        const entry = try self.node_monos.getOrPut(root);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        for (entry.value_ptr.items) |existing| {
            if (existing == ty) return;
        }
        try entry.value_ptr.append(self.allocator, ty);
    }

    /// Materialize the active Monotype view of a node, reserving the id first
    /// so recursive types tie their own knot. The returned TypeId is mutable
    /// graph state and must not be written to completed Monotype output.
    pub fn activeTypeViewForNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        return try self.monoFor(node);
    }

    fn monoFor(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        const root = self.find(node);
        if (self.node_monos.get(root)) |views| {
            if (views.items.len != 0) return views.items[0];
        }
        const ty = try self.types.add(.zst);
        const entry = try self.node_monos.getOrPut(root);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.allocator, ty);
        try self.mono_nodes.put(ty, root);
        try self.fillMono(root, ty);
        return ty;
    }

    /// Materialize a graph node directly into a final TypeId without first
    /// exposing or copying a mutable Monotype view.
    pub fn sealNode(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
        try self.drainDirty();
        var sealer = GraphTypeFinals.init(self);
        defer sealer.deinit();
        return try sealer.sealNode(node);
    }

    /// Materialize a TypeId into a final copy. If the TypeId is a graph view,
    /// this snapshots its current solved node instead of returning the mutable
    /// view id.
    pub fn sealType(self: *InstGraph, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        try self.drainDirty();
        var sealer = GraphTypeFinals.init(self);
        defer sealer.deinit();
        return try sealer.sealType(ty);
    }

    pub fn assertNoDeferredRequestsBeforeBodySeal(self: *InstGraph) void {
        if (self.deferred_templates.items.len != 0) {
            Common.invariant("Monotype body draft sealed before deferred specialization requests were drained");
        }
    }

    pub fn assertTypeHasNoGraphViews(self: *InstGraph, ty: Type.TypeId) Allocator.Error!void {
        if (try self.typeHasGraphViews(ty)) {
            Common.invariant("Monotype body draft retained an instantiation graph type view after sealing");
        }
    }

    pub fn typeHasGraphViews(self: *InstGraph, ty: Type.TypeId) Allocator.Error!bool {
        var seen = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer seen.deinit();
        return try self.typeContainsGraphView(ty, &seen);
    }

    fn typeContainsGraphView(
        self: *InstGraph,
        ty: Type.TypeId,
        seen: *std.AutoHashMap(Type.TypeId, void),
    ) Allocator.Error!bool {
        if (self.isGraphViewType(ty)) return true;
        const seen_entry = try seen.getOrPut(ty);
        if (seen_entry.found_existing) return false;
        return switch (self.types.get(ty)) {
            .primitive, .erased, .zst => false,
            .list => |elem| try self.typeContainsGraphView(elem, seen),
            .box => |elem| try self.typeContainsGraphView(elem, seen),
            .tuple => |items| try self.typeSpanContainsGraphView(items, seen),
            .func => |func| blk: {
                if (try self.typeSpanContainsGraphView(func.args, seen)) break :blk true;
                break :blk try self.typeContainsGraphView(func.ret, seen);
            },
            .record => |fields| blk: {
                const field_span = self.types.fieldSpan(fields);
                for (0..field_span.len) |index| {
                    const field = GuardedList.at(field_span, index);
                    if (try self.typeContainsGraphView(field.ty, seen)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tags| blk: {
                const tag_span = self.types.tagSpan(tags);
                for (0..tag_span.len) |index| {
                    const tag = GuardedList.at(tag_span, index);
                    if (try self.typeSpanContainsGraphView(tag.payloads, seen)) break :blk true;
                }
                break :blk false;
            },
            .named => |named| blk: {
                if (try self.typeSpanContainsGraphView(named.args, seen)) break :blk true;
                if (named.backing) |backing| {
                    if (try self.typeContainsGraphView(backing.ty, seen)) break :blk true;
                }
                const declared_fields = self.types.declaredFieldSpan(named.declared_order);
                for (0..declared_fields.len) |index| {
                    const field = GuardedList.at(declared_fields, index);
                    switch (field) {
                        .named => {},
                        .padding => |padding| if (try self.typeContainsGraphView(padding, seen)) break :blk true,
                    }
                }
                break :blk false;
            },
        };
    }

    fn typeSpanContainsGraphView(
        self: *InstGraph,
        span: Type.Span,
        seen: *std.AutoHashMap(Type.TypeId, void),
    ) Allocator.Error!bool {
        const children = self.types.span(span);
        for (0..children.len) |index| {
            const child = GuardedList.at(children, index);
            if (try self.typeContainsGraphView(child, seen)) return true;
        }
        return false;
    }

    fn isGraphViewType(self: *InstGraph, ty: Type.TypeId) bool {
        const raw_node = self.mono_nodes.get(ty) orelse return false;
        const node = self.find(raw_node);
        const views = self.node_monos.get(node) orelse return false;
        for (views.items) |view| {
            if (view == ty) return true;
        }
        return false;
    }

    /// Return the current root node for a TypeId that is one of this graph's
    /// temporary Monotype views. Closed TypeIds and stale view ids return null.
    pub fn monoViewNode(self: *InstGraph, ty: Type.TypeId) ?NodeId {
        const raw_node = self.mono_nodes.get(ty) orelse return null;
        const node = self.find(raw_node);
        const views = self.node_monos.get(node) orelse return null;
        for (views.items) |view| {
            if (view == ty) return node;
        }
        return null;
    }

    /// Write a node's current content into one of its Monotype views.
    fn fillMono(self: *InstGraph, raw_root: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const root = self.find(raw_root);
        const types = self.types;
        const previous = types.get(ty);
        const filled: Type.Content = switch (self.nodes.items[@intFromEnum(root)]) {
            .redirect => unreachable,
            .unresolved => |variable| materializeUnresolved(variable),
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.monoForWithReuse(elem, switch (previous) {
                .list => |old| old,
                else => null,
            }) },
            .box => |elem| .{ .box = try self.monoForWithReuse(elem, switch (previous) {
                .box => |old| old,
                else => null,
            }) },
            .tuple => |items| .{ .tuple = try self.monoSpanWithReuse(items, switch (previous) {
                .tuple => |span| span,
                else => null,
            }) },
            .func => |func| .{ .func = .{
                .args = try self.monoSpanWithReuse(func.args, switch (previous) {
                    .func => |old| old.args,
                    else => null,
                }),
                .ret = try self.monoForWithReuse(func.ret, switch (previous) {
                    .func => |old| old.ret,
                    else => null,
                }),
            } },
            .empty_tag_union => .{ .tag_union = Type.Span.empty() },
            .empty_record => .{ .record = Type.Span.empty() },
            .tag_union => blk: {
                const flat = try self.flattenTagRow(root);
                const existing = switch (previous) {
                    .tag_union => |span| span,
                    else => null,
                };
                var tags = std.ArrayList(PendingTag).empty;
                defer tags.deinit(self.allocator);
                try tags.ensureTotalCapacity(self.allocator, flat.tags.len);
                for (flat.tags) |tag| {
                    tags.appendAssumeCapacity(.{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.monoSliceWithReuse(tag.payloads, self.existingTagPayloads(existing, tag)),
                    });
                }
                break :blk .{ .tag_union = try self.tagSpanWithReuse(tags.items, existing) };
            },
            .record => blk: {
                const flat = try self.flattenRecordRow(root);
                const existing = switch (previous) {
                    .record => |span| span,
                    else => null,
                };
                var fields = std.ArrayList(Type.Field).empty;
                defer fields.deinit(self.allocator);
                try fields.ensureTotalCapacity(self.allocator, flat.fields.len);
                for (flat.fields) |field| {
                    fields.appendAssumeCapacity(.{
                        .name = field.name,
                        .ty = try self.monoForWithReuse(field.ty, self.existingRecordFieldType(existing, field.name)),
                    });
                }
                break :blk .{ .record = try self.recordSpanWithReuse(fields.items, existing) };
            },
            .named => |named| blk: {
                const backing: ?Type.NamedBacking = if (named.backing) |raw_backing| backing: {
                    const structural = try self.structuralBackingNode(raw_backing.node, named);
                    break :backing .{
                        .ty = try self.monoForWithReuse(structural.node, switch (previous) {
                            .named => |old| if (old.backing) |old_backing| old_backing.ty else null,
                            else => null,
                        }),
                        .use = raw_backing.use,
                    };
                } else null;
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.monoSpanWithReuse(named.args, switch (previous) {
                        .named => |old| old.args,
                        else => null,
                    }),
                    .backing = backing,
                    .declared_order = try self.declaredFieldSpanWithReuse(named.declared_order, switch (previous) {
                        .named => |old| old.declared_order,
                        else => null,
                    }),
                } };
            },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        types.replaceGraphView(ty, filled);
    }

    fn monoForWithReuse(
        self: *InstGraph,
        node: NodeId,
        existing: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const ty = existing orelse return try self.monoFor(node);
        const root = self.find(node);
        const previous_root = self.monoViewNode(ty) orelse return try self.monoFor(root);
        if (previous_root != root) return try self.monoFor(root);
        return ty;
    }

    fn monoSliceWithReuse(
        self: *InstGraph,
        nodes_slice: []const NodeId,
        existing: ?Type.Span,
    ) Allocator.Error![]Type.TypeId {
        const existing_copy = if (existing) |old_span| blk: {
            const old = self.types.span(old_span);
            break :blk if (old.len == nodes_slice.len) try GuardedList.dupe(self.allocator, Type.TypeId, old) else null;
        } else null;
        defer if (existing_copy) |old| self.allocator.free(old);

        const out = try self.arena().alloc(Type.TypeId, nodes_slice.len);
        for (nodes_slice, 0..) |node, index| {
            out[index] = try self.monoForWithReuse(
                node,
                if (existing_copy) |old| old[index] else null,
            );
        }
        return out;
    }

    fn monoSpanWithReuse(
        self: *InstGraph,
        nodes_slice: []const NodeId,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        const values = try self.monoSliceWithReuse(nodes_slice, existing);
        if (existing) |span| {
            if (typeSpanEql(self.types.span(span), values)) return span;
        }
        return try self.types.addSpan(values);
    }

    fn existingRecordFieldType(
        self: *InstGraph,
        existing: ?Type.Span,
        name: names.RecordFieldNameId,
    ) ?Type.TypeId {
        const span = existing orelse return null;
        const wanted = self.fieldLabelText(name);
        const fields = self.types.fieldSpan(span);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            if (Ident.textEql(wanted, self.fieldLabelText(field.name))) return field.ty;
        }
        return null;
    }

    fn existingTagPayloads(
        self: *InstGraph,
        existing: ?Type.Span,
        tag: InstTag,
    ) ?Type.Span {
        const span = existing orelse return null;
        const wanted = self.tagLabelText(tag.name);
        const tags = self.types.tagSpan(span);
        for (0..tags.len) |index| {
            const old_tag = GuardedList.at(tags, index);
            if (!Ident.textEql(wanted, self.tagLabelText(old_tag.name))) continue;
            const old_payloads = self.types.span(old_tag.payloads);
            return if (old_payloads.len == tag.payloads.len) old_tag.payloads else null;
        }
        return null;
    }

    fn declaredFieldSpanWithReuse(
        self: *InstGraph,
        fields: []const InstDeclaredField,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        if (fields.len == 0) return .empty();
        const materialized = try self.allocator.alloc(Type.DeclaredField, fields.len);
        defer self.allocator.free(materialized);
        for (fields, 0..) |field, index| {
            materialized[index] = switch (field) {
                .named => |name| .{ .named = name },
                .padding => |node| .{ .padding = try self.monoFor(node) },
            };
        }
        if (existing) |span| {
            if (declaredFieldSpanEql(self.types.declaredFieldSpan(span), materialized)) return span;
        }
        return try self.types.addDeclaredFields(materialized);
    }

    fn recordSpanWithReuse(
        self: *InstGraph,
        fields: []const Type.Field,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        const normalized = try self.allocator.dupe(Type.Field, fields);
        defer self.allocator.free(normalized);
        std.mem.sort(Type.Field, normalized, self.name_store, recordFieldLessThan);
        assertNoDuplicateRecordFields(self.name_store, normalized, "instantiation produced a record row with duplicate fields");

        if (existing) |span| {
            if (recordSpanEql(self.types.fieldSpan(span), normalized)) return span;
        }
        return try self.types.addFields(normalized);
    }

    const PendingTag = struct {
        name: names.TagNameId,
        checked_name: names.TagNameId,
        payloads: []const Type.TypeId,
    };

    fn tagSpanWithReuse(
        self: *InstGraph,
        tags: []const PendingTag,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        const normalized = try self.allocator.dupe(PendingTag, tags);
        defer self.allocator.free(normalized);
        std.mem.sort(PendingTag, normalized, self.name_store, pendingTagLessThan);
        assertNoDuplicatePendingTags(self.name_store, normalized, "instantiation produced a tag row with duplicate tags");

        if (existing) |span| {
            if (tagSpanEql(self.types, self.types.tagSpan(span), normalized)) return span;
        }

        var materialized = std.ArrayList(Type.Tag).empty;
        defer materialized.deinit(self.allocator);
        try materialized.ensureTotalCapacity(self.allocator, normalized.len);
        for (normalized) |tag| {
            materialized.appendAssumeCapacity(.{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.types.addSpan(tag.payloads),
            });
        }
        return try self.types.addTags(materialized.items);
    }

    /// Refill the Monotype views of every node that changed since the last
    /// drain. Run only when no constraint walk holds slices into the type
    /// store.
    pub fn drainDirty(self: *InstGraph) Allocator.Error!void {
        while (self.dirty.pop()) |raw_root| {
            const root = self.find(raw_root);
            if (!self.dirty_set.remove(root)) continue;
            const views = self.node_monos.get(root) orelse continue;
            for (views.items) |ty| {
                try self.fillMono(root, ty);
            }
        }
    }
};

/// Shared finalization state for materializing graph nodes into immutable
/// Monotype type ids.
pub const GraphTypeFinals = struct {
    graph: *InstGraph,
    sealed: std.AutoHashMap(NodeId, Type.TypeId),
    sealed_types: std.AutoHashMap(Type.TypeId, Type.TypeId),

    pub fn init(graph: *InstGraph) GraphTypeFinals {
        return .{
            .graph = graph,
            .sealed = std.AutoHashMap(NodeId, Type.TypeId).init(graph.allocator),
            .sealed_types = std.AutoHashMap(Type.TypeId, Type.TypeId).init(graph.allocator),
        };
    }

    pub fn deinit(self: *GraphTypeFinals) void {
        self.sealed_types.deinit();
        self.sealed.deinit();
    }

    pub fn sealType(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (self.isGeneratedIteratorEvidenceType(ty)) {
            if (self.graph.monoViewNode(ty) != null) return try self.sealStoreType(ty);
            if (try self.typeHasGraphViews(ty)) return try self.sealStoreType(ty);
            return ty;
        }
        if (self.graph.mono_nodes.get(ty)) |raw_node| {
            const node = self.graph.find(raw_node);
            if (self.graph.node_monos.get(node)) |views| {
                for (views.items) |view| {
                    if (view == ty) return try self.sealNode(node);
                }
            }
        }
        if (try self.typeHasGraphViews(ty)) return try self.sealStoreType(ty);
        return ty;
    }

    fn isGeneratedIteratorEvidenceType(self: *GraphTypeFinals, ty: Type.TypeId) bool {
        return switch (self.graph.types.get(ty)) {
            .named => |named| switch (named.def.iterator_representation) {
                .minted, .forced_dynamic => switch (named.builtin_owner orelse return false) {
                    .iter, .stream => true,
                    else => false,
                },
                .none => false,
            },
            else => false,
        };
    }

    pub fn sealNode(self: *GraphTypeFinals, raw_node: NodeId) Allocator.Error!Type.TypeId {
        const node = self.graph.find(raw_node);
        if (self.sealed.get(node)) |existing| return existing;

        const Context = struct {
            sealer: *GraphTypeFinals,
            node: NodeId,

            fn fill(context: @This(), reserved: Type.TypeId) Allocator.Error!Type.Content {
                try context.sealer.sealed.put(context.node, reserved);
                return try context.sealer.sealContent(context.node);
            }
        };
        return try self.graph.types.addRecursive(Context{ .sealer = self, .node = node }, Context.fill);
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
                    };
                } else null,
                .declared_order = try self.sealDeclaredFieldSpan(named.declared_order),
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
    }

    fn typeHasGraphViews(self: *GraphTypeFinals, ty: Type.TypeId) Allocator.Error!bool {
        var seen = std.AutoHashMap(Type.TypeId, void).init(self.graph.allocator);
        defer seen.deinit();
        return try self.graph.typeContainsGraphView(ty, &seen);
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

fn pendingTagLessThan(name_store: *const names.NameStore, lhs: InstGraph.PendingTag, rhs: InstGraph.PendingTag) bool {
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

fn assertNoDuplicatePendingTags(name_store: *const names.NameStore, tags: []const InstGraph.PendingTag, comptime message: []const u8) void {
    if (tags.len < 2) return;
    for (tags[1..], 1..) |tag, i| {
        if (name_store.tagLabelTextEql(tags[i - 1].name, tag.name)) {
            Common.invariant(message);
        }
    }
}

fn typeSpanEql(left: anytype, right: anytype) bool {
    if (left.len != right.len) return false;
    for (0..left.len) |index| {
        const left_ty = GuardedList.at(left, index);
        const right_ty = GuardedList.at(right, index);
        if (left_ty != right_ty) return false;
    }
    return true;
}

fn recordSpanEql(left: anytype, right: anytype) bool {
    if (left.len != right.len) return false;
    for (0..left.len) |index| {
        const left_field = GuardedList.at(left, index);
        const right_field = GuardedList.at(right, index);
        if (left_field.name != right_field.name or left_field.ty != right_field.ty) return false;
    }
    return true;
}

fn tagSpanEql(types: *const Type.Store, left: anytype, right: anytype) bool {
    if (left.len != right.len) return false;
    for (0..left.len) |index| {
        const left_tag = GuardedList.at(left, index);
        const right_tag = GuardedList.at(right, index);
        if (left_tag.name != right_tag.name or left_tag.checked_name != right_tag.checked_name) return false;
        if (!typeSpanEql(types.span(left_tag.payloads), right_tag.payloads)) return false;
    }
    return true;
}

fn declaredFieldSpanEql(left: anytype, right: anytype) bool {
    if (left.len != right.len) return false;
    for (0..left.len) |index| {
        const left_field = GuardedList.at(left, index);
        const right_field = GuardedList.at(right, index);
        switch (left_field) {
            .named => |left_name| switch (right_field) {
                .named => |right_name| if (left_name != right_name) return false,
                .padding => return false,
            },
            .padding => |left_ty| switch (right_field) {
                .named => return false,
                .padding => |right_ty| if (left_ty != right_ty) return false,
            },
        }
    }
    return true;
}

fn instNodeEql(left: InstNode, right: InstNode) bool {
    return switch (left) {
        .redirect => |left_next| switch (right) {
            .redirect => |right_next| left_next == right_next,
            else => false,
        },
        .unresolved => |left_var| switch (right) {
            .unresolved => |right_var| std.meta.eql(left_var, right_var),
            else => false,
        },
        .primitive => |left_primitive| switch (right) {
            .primitive => |right_primitive| left_primitive == right_primitive,
            else => false,
        },
        .list => |left_elem| switch (right) {
            .list => |right_elem| left_elem == right_elem,
            else => false,
        },
        .box => |left_elem| switch (right) {
            .box => |right_elem| left_elem == right_elem,
            else => false,
        },
        .tuple => |left_items| switch (right) {
            .tuple => |right_items| nodeSliceEql(left_items, right_items),
            else => false,
        },
        .func => |left_fn| switch (right) {
            .func => |right_fn| nodeSliceEql(left_fn.args, right_fn.args) and left_fn.ret == right_fn.ret,
            else => false,
        },
        .tag_union => |left_row| switch (right) {
            .tag_union => |right_row| left_row.ext == right_row.ext and instTagSliceEql(left_row.tags, right_row.tags),
            else => false,
        },
        .record => |left_row| switch (right) {
            .record => |right_row| left_row.ext == right_row.ext and instFieldSliceEql(left_row.fields, right_row.fields),
            else => false,
        },
        .empty_tag_union => switch (right) {
            .empty_tag_union => true,
            else => false,
        },
        .empty_record => switch (right) {
            .empty_record => true,
            else => false,
        },
        .named => |left_named| switch (right) {
            .named => |right_named| instNamedEql(left_named, right_named),
            else => false,
        },
        .erased => |left_digest| switch (right) {
            .erased => |right_digest| std.mem.eql(u8, left_digest.bytes[0..], right_digest.bytes[0..]),
            else => false,
        },
        .zst => switch (right) {
            .zst => true,
            else => false,
        },
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
        return left_backing.node == right_backing.node and left_backing.use == right_backing.use;
    }
    return right == null;
}

fn sameBuiltinOwner(left: ?static_dispatch.BuiltinOwner, right: ?static_dispatch.BuiltinOwner, owner: static_dispatch.BuiltinOwner) bool {
    const left_owner = left orelse return false;
    const right_owner = right orelse return false;
    return left_owner == owner and right_owner == owner;
}

fn eitherBuiltinOwner(left: ?static_dispatch.BuiltinOwner, right: ?static_dispatch.BuiltinOwner, owner: static_dispatch.BuiltinOwner) bool {
    if (left) |left_owner| {
        if (left_owner == owner) return true;
    }
    if (right) |right_owner| {
        if (right_owner == owner) return true;
    }
    return false;
}

fn testCheckedTypeId(comptime value: u32) checked.CheckedTypeId {
    comptime std.debug.assert(value != 0);
    return @enumFromInt(value);
}

test "completed monotype program view does not expose instantiation graph nodes" {
    @setEvalBranchQuota(10_000);
    comptime assertNoNodeId(Ast.ProgramView, "Ast.ProgramView");
}

fn assertNoNodeId(comptime T: type, comptime path: []const u8) void {
    if (T == NodeId) @compileError(path ++ " exposes instantiation graph NodeId");

    switch (@typeInfo(T)) {
        .array => |array| assertNoNodeId(array.child, path ++ "[]"),
        .optional => |optional| assertNoNodeId(optional.child, path ++ "?"),
        .pointer => |pointer| switch (pointer.size) {
            .slice => assertNoNodeId(pointer.child, path ++ "[]"),
            .one, .many, .c => {},
        },
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                assertNoNodeId(field.type, path ++ "." ++ field.name);
            }
        },
        .@"union" => |info| {
            inline for (info.fields) |field| {
                assertNoNodeId(field.type, path ++ "." ++ field.name);
            }
        },
        else => {},
    }
}

test "issue 9647: row refills do not duplicate dependencies or materialized spans" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const field_name = try name_store.internRecordFieldLabel("value");
    const field_ty = try graph.newNode(.{ .primitive = .u64 });
    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = field_name, .ty = field_ty };

    const ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = ext,
    } });

    const view = try graph.monoFor(row);
    for (0..32) |_| {
        try graph.fillMono(row, view);
    }

    const parents = graph.row_parents.get(graph.find(ext)) orelse
        return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(usize, 1), parents.items.len);
    try std.testing.expectEqual(@as(usize, 1), type_store.view().fields.len);
}

test "record field node carries contextual row evidence into receiver" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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

    const sealed_record = try graph.sealNode(record);
    const sealed_fields = type_store.fieldSpan(type_store.get(sealed_record).record);
    try std.testing.expectEqual(@as(usize, 1), sealed_fields.len);
    const sealed_fn = type_store.get(GuardedList.at(sealed_fields, 0).ty).func;
    const sealed_tags = type_store.tagSpan(type_store.get(sealed_fn.ret).tag_union);
    try std.testing.expectEqual(@as(usize, 2), sealed_tags.len);
    try std.testing.expectEqual(emit_failed, GuardedList.at(sealed_tags, 0).name);
    try std.testing.expectEqual(exit, GuardedList.at(sealed_tags, 1).name);
}

test "monotype reuse keeps views of different roots distinct" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const old_root = try graph.newNode(.{ .primitive = .u64 });
    const new_root = try graph.newNode(.{ .primitive = .str });
    const old_view = try graph.monoFor(old_root);
    const new_view = try graph.monoForWithReuse(new_root, old_view);

    try std.testing.expect(old_view != new_view);
    try std.testing.expectEqual(Type.Content{ .primitive = .u64 }, type_store.get(old_view));
    try std.testing.expectEqual(Type.Content{ .primitive = .str }, type_store.get(new_view));
}

test "alias unification does not make the alias its own backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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

    const alias_ty = try graph.monoFor(alias);
    const named = switch (type_store.get(alias_ty)) {
        .named => |named| named,
        else => return error.TestExpectedEqual,
    };
    const named_backing = named.backing orelse return error.TestExpectedEqual;
    try std.testing.expect(named_backing.ty != alias_ty);
}

test "sealed monotype copy is not refilled by later graph evidence" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });
    const b_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = ext,
    } });

    const draft = try graph.monoFor(row);
    try graph.drainDirty();
    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed = try finals.sealType(draft);
    try std.testing.expect(sealed != draft);
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);

    const extra_fields = try graph.arena().alloc(InstField, 1);
    extra_fields[0] = .{ .name = b_name, .ty = b_ty };
    const extra = try graph.newNode(.{ .record = .{
        .fields = extra_fields,
        .ext = try graph.newNode(.empty_record),
    } });
    try graph.unify(ext, extra);
    try graph.drainDirty();

    try std.testing.expectEqual(@as(usize, 2), type_store.fieldSpan(type_store.get(draft).record).len);
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);
}

test "sealed graph function copy recursively seals graph-owned argument views" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });
    const b_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = ext,
    } });

    const args = try graph.arena().alloc(NodeId, 1);
    args[0] = row;
    const fn_node = try graph.newNode(.{ .func = .{
        .args = args,
        .ret = row,
    } });
    const draft_fn = try graph.monoFor(fn_node);
    try graph.drainDirty();

    var finals = GraphTypeFinals.init(graph);
    defer finals.deinit();
    const sealed_fn = try finals.sealType(draft_fn);
    try std.testing.expect(sealed_fn != draft_fn);
    const sealed_arg = GuardedList.at(type_store.span(type_store.get(sealed_fn).func.args), 0);
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed_arg).record).len);

    const extra_fields = try graph.arena().alloc(InstField, 1);
    extra_fields[0] = .{ .name = b_name, .ty = b_ty };
    const extra = try graph.newNode(.{ .record = .{
        .fields = extra_fields,
        .ext = try graph.newNode(.empty_record),
    } });
    try graph.unify(ext, extra);
    try graph.drainDirty();

    const draft_arg = GuardedList.at(type_store.span(type_store.get(draft_fn).func.args), 0);
    try std.testing.expectEqual(@as(usize, 2), type_store.fieldSpan(type_store.get(draft_arg).record).len);
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed_arg).record).len);
}

test "sealed graph node does not allocate a mutable monotype view" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const a_name = try name_store.internRecordFieldLabel("a");
    const b_name = try name_store.internRecordFieldLabel("b");
    const a_ty = try graph.newNode(.{ .primitive = .u64 });
    const b_ty = try graph.newNode(.{ .primitive = .u64 });

    const fields = try graph.arena().alloc(InstField, 1);
    fields[0] = .{ .name = a_name, .ty = a_ty };
    const ext = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_record) });
    const row = try graph.newNode(.{ .record = .{
        .fields = fields,
        .ext = ext,
    } });

    const sealed = try graph.sealNode(row);
    try std.testing.expectEqual(@as(usize, 0), graph.node_monos.count());
    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);

    const extra_fields = try graph.arena().alloc(InstField, 1);
    extra_fields[0] = .{ .name = b_name, .ty = b_ty };
    const extra = try graph.newNode(.{ .record = .{
        .fields = extra_fields,
        .ext = try graph.newNode(.empty_record),
    } });
    try graph.unify(ext, extra);
    try graph.drainDirty();

    try std.testing.expectEqual(@as(usize, 1), type_store.fieldSpan(type_store.get(sealed).record).len);
}

test "unconstrained checked graph node seals to empty tag union" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const node = try graph.newNode(.{ .unresolved = InstVariable.checkedVariable(null, null) });
    const sealed = try graph.sealNode(node);
    const content = type_store.get(sealed);
    try std.testing.expectEqual(Type.Span.empty(), content.tag_union);
}

test "issue 10128: nominal backing cache follows argument roots" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const original_arg = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const resolved_arg = try graph.newNode(.{ .primitive = .u64 });
    const backing = try graph.newNode(.empty_record);
    const module_bytes = [_]u8{0xA1} ** 32;

    try graph.putNominalBackingNode(module_bytes, 17, &.{original_arg}, backing);
    try graph.unify(original_arg, resolved_arg);

    try std.testing.expectEqual(
        backing,
        graph.nominalBackingNode(module_bytes, 17, &.{resolved_arg}),
    );
}

test "minted iterator unification with its public type preserves the minted backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xA7} ** 32));
    const type_name = try name_store.internTypeName("Builtin.Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(5) };
    const public_def: Type.TypeDef = .{
        .module = module_identity,
        .type_name = type_name,
        .source_decl = 62,
    };
    var minted_def = public_def;
    minted_def.generated = .{ .bytes = [_]u8{0x5A} ** 32 };
    minted_def.iterator_representation = .minted;
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

    try graph.unify(public_iter, minted_iter);

    try std.testing.expectEqual(graph.find(public_iter), graph.find(minted_iter));
    const unified = switch (graph.content(minted_iter)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(Type.IteratorRepresentation.minted, unified.def.iterator_representation);
    try std.testing.expectEqual(minted_backing, unified.backing.?.node);
    try std.testing.expect(graph.find(public_backing) != graph.find(minted_backing));
}

test "issue 10170: distinct minted iterators join recursive backings" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const module_identity = try name_store.internModuleIdentity(&([_]u8{0xA8} ** 32));
    const type_name = try name_store.internTypeName("Builtin.Iter");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(6) };
    const source_def: Type.TypeDef = .{
        .module = module_identity,
        .type_name = type_name,
        .source_decl = 63,
    };
    var list_def = source_def;
    list_def.generated = .{ .bytes = [_]u8{0x6A} ** 32 };
    list_def.iterator_representation = .minted;
    list_def.iterator_kind = .list;
    list_def.iterator_depth = 1;
    var concat_def = source_def;
    concat_def.generated = .{ .bytes = [_]u8{0x6B} ** 32 };
    concat_def.iterator_representation = .minted;
    concat_def.iterator_kind = .concat;
    concat_def.iterator_depth = 2;

    const list_item = try graph.newNode(.{ .primitive = .u64 });
    const concat_item = try graph.newNode(.{ .primitive = .u64 });
    const list_iter = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const concat_iter = try graph.newNode(.{ .unresolved = InstVariable.placeholder() });
    const list_backing = try graph.newNode(.{ .box = list_iter });
    const concat_backing = try graph.newNode(.{ .box = concat_iter });
    const list_args = try graph.arena().alloc(NodeId, 1);
    list_args[0] = list_item;
    const concat_args = try graph.arena().alloc(NodeId, 1);
    concat_args[0] = concat_item;

    try graph.setContent(list_iter, .{ .named = .{
        .named_type = named_type,
        .def = list_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = list_args,
        .backing = .{ .node = list_backing, .use = .inspectable },
    } });
    try graph.setContent(concat_iter, .{ .named = .{
        .named_type = named_type,
        .def = concat_def,
        .kind = .@"opaque",
        .builtin_owner = .iter,
        .args = concat_args,
        .backing = .{ .node = concat_backing, .use = .inspectable },
    } });

    const before_nodes = graph.nodes.items.len;
    try graph.unify(list_iter, concat_iter);

    try std.testing.expectEqual(before_nodes, graph.nodes.items.len);
    try std.testing.expectEqual(graph.find(list_iter), graph.find(concat_iter));
    try std.testing.expectEqual(graph.find(list_item), graph.find(concat_item));
    try std.testing.expectEqual(graph.find(list_backing), graph.find(concat_backing));
    const joined = switch (graph.content(list_iter)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(Type.IteratorKind.list, joined.def.iterator_kind);
}

test "issue 9647: unresolved tag row extension absorbs rest without allocating a rest node" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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
    const rest = switch (left_ext_content) {
        .tag_union => |row| row,
        else => return error.TestUnexpectedResult,
    };
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

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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
    const compressed = switch (graph.content(outer_named)) {
        .named => |named| named,
        else => return error.TestUnexpectedResult,
    };
    try std.testing.expectEqual(structural_backing, compressed.backing.?.node);
}

test "issue 9647: recursive nominal backing cycle is not chased as structural backing" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
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
