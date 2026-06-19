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

const Common = @import("../common.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const Ident = @import("base").Ident;

/// A procedure template body request deferred to the end of the requesting
/// specialization, when that specialization's types are final. Requesting at
/// final types keeps specialization keys stable: two requests whose types
/// would later converge to one digest must resolve to one lowered body.
pub const DeferredTemplate = struct {
    template_ref: names.ProcTemplate,
    module: checked.ModuleId,
    source_fn_ty: checked.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    fn_ty: Type.TypeId,
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

/// Defaulting evidence carried by an unresolved instantiation-graph node until
/// unification resolves it or materialization applies the default.
pub const InstVariable = struct {
    numeric_default_phase: ?checked.NumericDefaultPhase = null,
    row_default: ?checked.RowDefault = null,
};

/// Backing of a named instantiation-graph node.
pub const InstBacking = struct {
    node: NodeId,
    use: Type.BackingUse,
};

/// Named (alias/nominal/opaque) instantiation-graph node.
pub const InstNamed = struct {
    named_type: Type.NamedType,
    def: Type.TypeDef,
    kind: Type.NamedKind,
    builtin_owner: ?static_dispatch.BuiltinOwner,
    args: []NodeId,
    backing: ?InstBacking,
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
    /// Template body requests made while lowering this specialization,
    /// processed once its body is complete and its types are final.
    deferred_templates: std.ArrayList(DeferredTemplate) = .empty,

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
        };
        return graph;
    }

    pub fn destroy(self: *InstGraph) void {
        const allocator = self.allocator;
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

    /// Queue the views of a changed root and of every row whose flattened view
    /// chains through it for a content refill.
    fn markDirty(self: *InstGraph, changed: NodeId) Allocator.Error!void {
        var pending = std.ArrayList(NodeId).empty;
        defer pending.deinit(self.allocator);
        var visited = std.AutoHashMap(NodeId, void).init(self.allocator);
        defer visited.deinit();
        try pending.append(self.allocator, changed);
        while (pending.pop()) |raw| {
            const root = self.find(raw);
            if (visited.contains(root)) continue;
            try visited.put(root, {});
            if (self.node_monos.contains(root)) {
                try self.queueDirty(root);
            }
            if (self.row_parents.get(root)) |parents| {
                try pending.appendSlice(self.allocator, parents.items);
            }
            if (self.row_parents.get(raw)) |parents| {
                if (raw != root) try pending.appendSlice(self.allocator, parents.items);
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

        switch (left_content) {
            .redirect => unreachable,
            .unresolved => |left_var| switch (right_content) {
                .unresolved => |right_var| {
                    try self.setContent(right, .{ .unresolved = mergeVariables(left_var, right_var) });
                    try self.union_(right, left);
                },
                else => try self.union_(right, left),
            },
            else => switch (right_content) {
                .unresolved => try self.union_(left, right),
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
            .numeric_default_phase = a.numeric_default_phase orelse b.numeric_default_phase,
            .row_default = a.row_default orelse b.row_default,
        };
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
                    if (std.meta.eql(left_named.def, right_named.def) and left_named.args.len == right_named.args.len) {
                        for (left_named.args, right_named.args) |left_arg, right_arg| {
                            try pending.append(self.allocator, .{ .left = left_arg, .right = right_arg });
                        }
                        if (left_named.backing) |left_backing| {
                            if (right_named.backing) |right_backing| {
                                try pending.append(self.allocator, .{ .left = left_backing.node, .right = right_backing.node });
                            }
                        }
                        try self.union_(left, right);
                        return;
                    }
                    if (left_named.kind == .alias) {
                        try self.unifyThroughBacking(left, left_content, right, pending);
                        return;
                    }
                    if (right_named.kind == .alias) {
                        try self.unifyThroughBacking(right, right_content, left, pending);
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
            const named = switch (self.nodes.items[@intFromEnum(current)]) {
                .named => |named| named,
                else => Common.invariant("named backing compression reached a structural node before its result"),
            };
            if (named.kind != .alias and !sameNamedInstance(named, owner)) {
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
                if (named.kind != .alias and !sameNamedInstance(named, owner)) return null;
                const backing = named.backing orelse
                    Common.invariant("named backing chain reached a named type without backing");
                return self.find(backing.node);
            },
            else => return null,
        }
    }

    fn sameNamedInstance(left: InstNamed, right: InstNamed) bool {
        return left.kind == right.kind and
            sameTypeDef(left.def, right.def) and
            left.builtin_owner == right.builtin_owner;
    }

    fn sameTypeDef(left: Type.TypeDef, right: Type.TypeDef) bool {
        return left.module_name == right.module_name and
            left.type_name == right.type_name and
            left.source_decl == right.source_decl;
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
        var tags = std.ArrayList(InstTag).empty;
        defer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, row.tags);

        var seen = std.AutoHashMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try seen.put(root, {});

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

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further tags — every
                // tag on the cycle is already collected — but the row remains
                // extensible, so the chain terminates open.
                ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } });
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
        var fields = std.ArrayList(InstField).empty;
        defer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, row.fields);

        var seen = std.AutoHashMap(NodeId, void).init(self.allocator);
        defer seen.deinit();
        try seen.put(root, {});

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

        while (true) {
            if (seen.contains(ext)) {
                // A cyclic extension chain contributes no further fields —
                // every field on the cycle is already collected — but the row
                // remains extensible, so the chain terminates open.
                ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_record } });
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
            const new_ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } });
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
            const new_ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_record } });
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
        const node = try self.newNode(.{ .unresolved = .{} });
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
                if (span.len == 0) break :blk .{ .unresolved = .{ .row_default = .empty_tag_union } };
                const inst_tags = try self.arena().alloc(InstTag, span.len);
                for (span, 0..) |tag, index| {
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
                    .ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } }),
                } };
            },
            .record => |fields| blk: {
                const span = types.fieldSpan(fields);
                if (span.len == 0) break :blk .empty_record;
                const inst_fields = try self.arena().alloc(InstField, span.len);
                for (span, 0..) |field, index| {
                    inst_fields[index] = .{
                        .name = field.name,
                        .ty = try self.importMono(field.ty),
                    };
                }
                break :blk .{ .record = .{
                    .fields = inst_fields,
                    .ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_record } }),
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
            } },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        _ = try self.replaceContentNoDirty(node, imported);
        return node;
    }

    fn importMonoSlice(self: *InstGraph, tys: []const Type.TypeId) Allocator.Error![]NodeId {
        const out = try self.arena().alloc(NodeId, tys.len);
        for (tys, 0..) |ty, index| {
            out[index] = try self.importMono(ty);
        }
        return out;
    }

    /// Register an existing Monotype as a view of a node, so the node's
    /// evidence refills that id in place.
    pub fn addMonoView(self: *InstGraph, node: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const root = self.find(node);
        try self.mono_nodes.put(ty, root);
        const entry = try self.node_monos.getOrPut(root);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        for (entry.value_ptr.items) |existing| {
            if (existing == ty) return;
        }
        try entry.value_ptr.append(self.allocator, ty);
    }

    /// Materialize the Monotype view of a node, reserving the id first so
    /// recursive types tie their own knot.
    pub fn monoFor(self: *InstGraph, node: NodeId) Allocator.Error!Type.TypeId {
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

    /// Write a node's current content into one of its Monotype views.
    fn fillMono(self: *InstGraph, raw_root: NodeId, ty: Type.TypeId) Allocator.Error!void {
        const root = self.find(raw_root);
        const types = self.types;
        const previous = types.get(ty);
        const filled: Type.Content = switch (self.nodes.items[@intFromEnum(root)]) {
            .redirect => unreachable,
            .unresolved => |variable| blk: {
                if (variable.numeric_default_phase) |phase| switch (phase) {
                    .mono_specialization => break :blk .{ .primitive = .dec },
                    .mono_specialization_str => break :blk .{ .primitive = .str },
                    .checking_finalized => Common.invariant("checking-finalized numeric variable reached Monotype unresolved"),
                };
                if (variable.row_default) |row_default| switch (row_default) {
                    .empty_record => break :blk .{ .record = Type.Span.empty() },
                    .empty_tag_union => break :blk .{ .tag_union = Type.Span.empty() },
                };
                break :blk .{ .tag_union = Type.Span.empty() };
            },
            .primitive => |primitive| .{ .primitive = primitive },
            .list => |elem| .{ .list = try self.monoFor(elem) },
            .box => |elem| .{ .box = try self.monoFor(elem) },
            .tuple => |items| .{ .tuple = try self.monoSpanWithReuse(items, switch (previous) {
                .tuple => |span| span,
                else => null,
            }) },
            .func => |func| .{ .func = .{
                .args = try self.monoSpanWithReuse(func.args, switch (previous) {
                    .func => |old| old.args,
                    else => null,
                }),
                .ret = try self.monoFor(func.ret),
            } },
            .empty_tag_union => .{ .tag_union = Type.Span.empty() },
            .empty_record => .{ .record = Type.Span.empty() },
            .tag_union => blk: {
                const flat = try self.flattenTagRow(root);
                var tags = std.ArrayList(PendingTag).empty;
                defer tags.deinit(self.allocator);
                try tags.ensureTotalCapacity(self.allocator, flat.tags.len);
                for (flat.tags) |tag| {
                    tags.appendAssumeCapacity(.{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.monoSlice(tag.payloads),
                    });
                }
                std.mem.sort(PendingTag, tags.items, self.name_store, pendingTagLessThan);
                assertNoDuplicatePendingTags(self.name_store, tags.items, "instantiation produced a tag row with duplicate tags");
                const existing = switch (previous) {
                    .tag_union => |span| span,
                    else => null,
                };
                break :blk .{ .tag_union = try self.tagSpanWithReuse(tags.items, existing) };
            },
            .record => blk: {
                const flat = try self.flattenRecordRow(root);
                var fields = std.ArrayList(Type.Field).empty;
                defer fields.deinit(self.allocator);
                try fields.ensureTotalCapacity(self.allocator, flat.fields.len);
                for (flat.fields) |field| {
                    fields.appendAssumeCapacity(.{
                        .name = field.name,
                        .ty = try self.monoFor(field.ty),
                    });
                }
                std.mem.sort(Type.Field, fields.items, self.name_store, recordFieldLessThan);
                assertNoDuplicateRecordFields(self.name_store, fields.items, "instantiation produced a record row with duplicate fields");
                const existing = switch (previous) {
                    .record => |span| span,
                    else => null,
                };
                break :blk .{ .record = try self.recordSpanWithReuse(fields.items, existing) };
            },
            .named => |named| blk: {
                const backing: ?Type.NamedBacking = if (named.backing) |raw_backing| backing: {
                    const structural = try self.structuralBackingNode(raw_backing.node, named);
                    break :backing .{
                        .ty = try self.monoFor(structural.node),
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
                } };
            },
            .erased => |digest| .{ .erased = digest },
            .zst => .zst,
        };
        types.types.items[@intFromEnum(ty)] = filled;
    }

    fn monoSlice(self: *InstGraph, nodes_slice: []const NodeId) Allocator.Error![]Type.TypeId {
        const out = try self.arena().alloc(Type.TypeId, nodes_slice.len);
        for (nodes_slice, 0..) |node, index| {
            out[index] = try self.monoFor(node);
        }
        return out;
    }

    fn monoSpanWithReuse(
        self: *InstGraph,
        nodes_slice: []const NodeId,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        const values = try self.monoSlice(nodes_slice);
        if (existing) |span| {
            if (typeSpanEql(self.types.span(span), values)) return span;
        }
        return try self.types.addSpan(values);
    }

    fn recordSpanWithReuse(
        self: *InstGraph,
        fields: []const Type.Field,
        existing: ?Type.Span,
    ) Allocator.Error!Type.Span {
        if (existing) |span| {
            if (recordSpanEql(self.types.fieldSpan(span), fields)) return span;
        }
        return try self.types.addFields(fields);
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
        if (existing) |span| {
            if (tagSpanEql(self.types, self.types.tagSpan(span), tags)) return span;
        }

        var materialized = std.ArrayList(Type.Tag).empty;
        defer materialized.deinit(self.allocator);
        try materialized.ensureTotalCapacity(self.allocator, tags.len);
        for (tags) |tag| {
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

fn typeSpanEql(left: []const Type.TypeId, right: []const Type.TypeId) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_ty, right_ty| {
        if (left_ty != right_ty) return false;
    }
    return true;
}

fn recordSpanEql(left: []const Type.Field, right: []const Type.Field) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_field, right_field| {
        if (left_field.name != right_field.name or left_field.ty != right_field.ty) return false;
    }
    return true;
}

fn tagSpanEql(types: *const Type.Store, left: []const Type.Tag, right: []const InstGraph.PendingTag) bool {
    if (left.len != right.len) return false;
    for (left, right) |left_tag, right_tag| {
        if (left_tag.name != right_tag.name or left_tag.checked_name != right_tag.checked_name) return false;
        if (!typeSpanEql(types.span(left_tag.payloads), right_tag.payloads)) return false;
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
        backingEql(left.backing, right.backing);
}

fn backingEql(left: ?InstBacking, right: ?InstBacking) bool {
    if (left) |left_backing| {
        const right_backing = right orelse return false;
        return left_backing.node == right_backing.node and left_backing.use == right_backing.use;
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

    const ext = try graph.newNode(.{ .unresolved = .{ .row_default = .empty_record } });
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
    try std.testing.expectEqual(@as(usize, 1), type_store.fields.items.len);
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

    const left_ext = try graph.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } });
    const right_ext = try graph.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } });

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

    const module_name = try name_store.internModuleName("Main");
    const type_name = try name_store.internTypeName("Role");
    const tag_name = try name_store.internTagLabel("Tile");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(1) };
    const def: Type.TypeDef = .{ .module_name = module_name, .type_name = type_name };
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

    const module_name = try name_store.internModuleName("Main");
    const type_name = try name_store.internTypeName("Recursive");
    const tag_name = try name_store.internTagLabel("Wrap");
    const named_type: Type.NamedType = .{ .module = .{}, .ty = testCheckedTypeId(2) };
    const def: Type.TypeDef = .{ .module_name = module_name, .type_name = type_name };

    const nominal = try graph.newNode(.{ .unresolved = .{} });
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

    const module_name = try name_store.internModuleName("Main");
    const nominal_name = try name_store.internTypeName("Role");
    const alias_name = try name_store.internTypeName("Wrapper.Role");
    const nominal_type: Type.NamedType = .{ .module = .{}, .ty = @enumFromInt(3) };
    const alias_type: Type.NamedType = .{ .module = .{}, .ty = @enumFromInt(4) };
    const nominal_def: Type.TypeDef = .{ .module_name = module_name, .type_name = nominal_name };
    const alias_def: Type.TypeDef = .{ .module_name = module_name, .type_name = alias_name };

    const nominal = try graph.newNode(.{ .unresolved = .{} });
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
