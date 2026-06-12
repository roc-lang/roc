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
            .node_monos = std.AutoHashMap(NodeId, std.ArrayList(Type.TypeId)).init(allocator),
            .mono_nodes = std.AutoHashMap(Type.TypeId, NodeId).init(allocator),
            .dirty = .empty,
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
        self.mono_nodes.deinit();
        self.dirty.deinit(allocator);
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
        try self.registerRowParent(id, node_content);
        return id;
    }

    fn registerRowParent(self: *InstGraph, row: NodeId, node_content: InstNode) Allocator.Error!void {
        const ext = switch (node_content) {
            .tag_union => |tag_union| tag_union.ext,
            .record => |record| record.ext,
            else => return,
        };
        const entry = try self.row_parents.getOrPut(self.find(ext));
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.allocator, row);
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
                try self.dirty.append(self.allocator, root);
            }
            if (self.row_parents.get(root)) |parents| {
                try pending.appendSlice(self.allocator, parents.items);
            }
            if (self.row_parents.get(raw)) |parents| {
                if (raw != root) try pending.appendSlice(self.allocator, parents.items);
            }
        }
    }

    /// Redirect `loser` into `winner`, moving Monotype views and row back
    /// references across, and queueing affected views for a content refill.
    fn union_(self: *InstGraph, raw_winner: NodeId, raw_loser: NodeId) Allocator.Error!void {
        const winner = self.find(raw_winner);
        const loser = self.find(raw_loser);
        if (winner == loser) return;
        self.nodes.items[@intFromEnum(loser)] = .{ .redirect = winner };
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
            const entry = try self.row_parents.getOrPut(winner);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.appendSlice(self.allocator, moved_list.items);
            moved_list.deinit(self.allocator);
        }
        try self.markDirty(winner);
    }

    /// Replace a root's content in place and queue affected views for refill.
    fn setContent(self: *InstGraph, root: NodeId, new_content: InstNode) Allocator.Error!void {
        self.nodes.items[@intFromEnum(root)] = new_content;
        try self.registerRowParent(root, new_content);
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
        const backing = named.backing orelse
            Common.invariant("instantiation unified an opaque type without backing against a structural type");
        if (named.kind == .alias) {
            try pending.append(self.allocator, .{ .left = backing.node, .right = other });
            return;
        }
        switch (self.nodes.items[@intFromEnum(other)]) {
            .named => {
                try pending.append(self.allocator, .{ .left = backing.node, .right = other });
                return;
            },
            else => {},
        }
        const moved = try self.newNode(self.nodes.items[@intFromEnum(other)]);
        try self.union_(named_node, other);
        try pending.append(self.allocator, .{ .left = backing.node, .right = moved });
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
        self.nodes.items[@intFromEnum(root)] = flattened;
        try self.registerRowParent(root, flattened);
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
        self.nodes.items[@intFromEnum(root)] = flattened;
        try self.registerRowParent(root, flattened);
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
            const right_rest = try self.newNode(.{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, only_right.items),
                .ext = flat_right.ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = right_rest });
            merged_ext = flat_right.ext;
        } else if (only_right.items.len == 0) {
            const left_rest = try self.newNode(.{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, only_left.items),
                .ext = flat_left.ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_right.ext, .right = left_rest });
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } });
            const left_rest = try self.newNode(.{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, only_left.items),
                .ext = new_ext,
            } });
            const right_rest = try self.newNode(.{ .tag_union = .{
                .tags = try self.arena().dupe(InstTag, only_right.items),
                .ext = new_ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = right_rest });
            try pending.append(self.allocator, .{ .left = flat_right.ext, .right = left_rest });
            merged_ext = new_ext;
        }

        try self.setContent(left, .{ .tag_union = .{
            .tags = try self.arena().dupe(InstTag, merged.items),
            .ext = merged_ext,
        } });
        try self.union_(left, right);
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
            const right_rest = try self.newNode(.{ .record = .{
                .fields = try self.arena().dupe(InstField, only_right.items),
                .ext = flat_right.ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = right_rest });
            merged_ext = flat_right.ext;
        } else if (only_right.items.len == 0) {
            const left_rest = try self.newNode(.{ .record = .{
                .fields = try self.arena().dupe(InstField, only_left.items),
                .ext = flat_left.ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_right.ext, .right = left_rest });
            merged_ext = flat_left.ext;
        } else {
            const new_ext = try self.newNode(.{ .unresolved = .{ .row_default = .empty_record } });
            const left_rest = try self.newNode(.{ .record = .{
                .fields = try self.arena().dupe(InstField, only_left.items),
                .ext = new_ext,
            } });
            const right_rest = try self.newNode(.{ .record = .{
                .fields = try self.arena().dupe(InstField, only_right.items),
                .ext = new_ext,
            } });
            try pending.append(self.allocator, .{ .left = flat_left.ext, .right = right_rest });
            try pending.append(self.allocator, .{ .left = flat_right.ext, .right = left_rest });
            merged_ext = new_ext;
        }

        try self.setContent(left, .{ .record = .{
            .fields = try self.arena().dupe(InstField, merged.items),
            .ext = merged_ext,
        } });
        try self.union_(left, right);
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
        self.nodes.items[@intFromEnum(node)] = imported;
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
        const filled: Type.Content = switch (self.nodes.items[@intFromEnum(root)]) {
            .redirect => unreachable,
            .unresolved => |variable| blk: {
                if (variable.numeric_default_phase) |phase| switch (phase) {
                    .mono_specialization => break :blk .{ .primitive = .dec },
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
            .tuple => |items| .{ .tuple = try self.monoSpan(items) },
            .func => |func| .{ .func = .{
                .args = try self.monoSpan(func.args),
                .ret = try self.monoFor(func.ret),
            } },
            .empty_tag_union => .{ .tag_union = Type.Span.empty() },
            .empty_record => .{ .record = Type.Span.empty() },
            .tag_union => blk: {
                const flat = try self.flattenTagRow(root);
                var tags = std.ArrayList(Type.Tag).empty;
                defer tags.deinit(self.allocator);
                try tags.ensureTotalCapacity(self.allocator, flat.tags.len);
                for (flat.tags) |tag| {
                    tags.appendAssumeCapacity(.{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = .empty(),
                    });
                }
                for (flat.tags, tags.items) |tag, *out| {
                    out.payloads = try types.addSpan(try self.monoSlice(tag.payloads));
                }
                std.mem.sort(Type.Tag, tags.items, self.name_store, tagLessThan);
                assertNoDuplicateTags(self.name_store, tags.items, "instantiation produced a tag row with duplicate tags");
                break :blk .{ .tag_union = try types.addTags(tags.items) };
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
                break :blk .{ .record = try types.addFields(fields.items) };
            },
            .named => |named| .{ .named = .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = try types.addSpan(try self.monoSlice(named.args)),
                .backing = if (named.backing) |backing| .{
                    .ty = try self.monoFor(backing.node),
                    .use = backing.use,
                } else null,
            } },
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

    fn monoSpan(self: *InstGraph, nodes_slice: []const NodeId) Allocator.Error!Type.Span {
        return try self.types.addSpan(try self.monoSlice(nodes_slice));
    }

    /// Refill the Monotype views of every node that changed since the last
    /// drain. Run only when no constraint walk holds slices into the type
    /// store.
    pub fn drainDirty(self: *InstGraph) Allocator.Error!void {
        while (self.dirty.pop()) |raw_root| {
            const root = self.find(raw_root);
            const views = self.node_monos.get(root) orelse continue;
            for (views.items) |ty| {
                try self.fillMono(root, ty);
            }
        }
    }
};

pub fn recordFieldLessThan(name_store: *const names.NameStore, lhs: Type.Field, rhs: Type.Field) bool {
    return name_store.recordFieldLabelTextLessThan(lhs.name, rhs.name);
}

pub fn tagLessThan(name_store: *const names.NameStore, lhs: Type.Tag, rhs: Type.Tag) bool {
    return name_store.tagLabelTextLessThan(lhs.name, rhs.name);
}

pub fn assertNoDuplicateRecordFields(name_store: *const names.NameStore, fields: []const Type.Field, comptime message: []const u8) void {
    if (fields.len < 2) return;
    for (fields[1..], 1..) |field, i| {
        if (name_store.recordFieldLabelTextEql(fields[i - 1].name, field.name)) {
            Common.invariant(message);
        }
    }
}

pub fn assertNoDuplicateTags(name_store: *const names.NameStore, tags: []const Type.Tag, comptime message: []const u8) void {
    if (tags.len < 2) return;
    for (tags[1..], 1..) |tag, i| {
        if (name_store.tagLabelTextEql(tags[i - 1].name, tag.name)) {
            Common.invariant(message);
        }
    }
}

test "monotype solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
