//! Executable monomorphic types.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const solved_type = @import("lambdasolved").Type;

/// Identifier for an executable monomorphic type node.
pub const TypeId = enum(u32) { _ };
/// Borrowed slice of executable type identifiers.
pub const TypeIds = []const TypeId;
/// Primitive executable types reused from the solved type layer.
pub const Prim = solved_type.Prim;
/// Interned symbol identifiers used in executable type tags.
pub const Symbol = @import("symbol").Symbol;

/// Executable nominal type metadata, including its lowered backing type.
pub const Nominal = struct {
    module_idx: u32,
    ident: base.Ident.Idx,
    is_opaque: bool,
    args: TypeIds,
    backing: TypeId,
};

/// Executable tag names, either source constructors or lambda tags.
pub const TagName = union(enum) {
    ctor: base.Ident.Idx,
    lambda: Symbol,
};

/// Executable tag-union variant metadata.
pub const Tag = struct {
    name: TagName,
    args: TypeIds,
};

/// Borrowed slice of executable tag descriptors.
pub const Tags = []const Tag;

/// Executable record field metadata.
pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeId,
};

/// Borrowed slice of executable record fields.
pub const Fields = []const Field;

/// Executable callable signature metadata.
pub const CallableSig = struct {
    args: TypeIds,
    ret: TypeId,
};

/// Executable monomorphic type content.
pub const Content = union(enum) {
    placeholder,
    unbd,
    link: TypeId,
    nominal: Nominal,
    list: TypeId,
    box: TypeId,
    erased_fn: struct {
        capture: ?TypeId,
        call: CallableSig,
    },
    tuple: TypeIds,
    tag_union: struct {
        tags: Tags,
        call: ?CallableSig = null,
    },
    record: struct {
        fields: Fields,
    },
    primitive: Prim,
};

/// Interning store for executable monomorphic types.
pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    interned_types: std.StringHashMap(TypeId),
    scratch_intern_key: std.ArrayList(u8),
    interned_by_raw: std.AutoHashMap(TypeId, TypeId),

    /// Initialize an empty executable type store.
    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .interned_types = std.StringHashMap(TypeId).init(allocator),
            .scratch_intern_key = .empty,
            .interned_by_raw = std.AutoHashMap(TypeId, TypeId).init(allocator),
        };
    }

    /// Release all memory owned by the executable type store.
    pub fn deinit(self: *Store) void {
        for (self.types.items) |content| {
            self.freeOwnedContent(content);
        }
        self.types.deinit(self.allocator);
        var keys = self.interned_types.keyIterator();
        while (keys.next()) |key| {
            self.allocator.free(key.*);
        }
        self.interned_types.deinit();
        self.scratch_intern_key.deinit(self.allocator);
        self.interned_by_raw.deinit();
    }

    /// Append a raw executable type node without interning it.
    pub fn addType(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, content);
        return @enumFromInt(idx);
    }

    /// Replace an existing executable type node in place.
    pub fn setType(self: *Store, id: TypeId, content: Content) void {
        self.replaceType(id, content);
    }

    /// Return the canonical key for an executable type when it is fully resolved.
    pub fn keyId(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (self.containsAbstractLeaf(root)) return root;
        if (!self.isFullyResolved(root)) return root;
        return try self.internTypeId(root);
    }

    pub fn structuralKeyOwned(self: *Store, id: TypeId) std.mem.Allocator.Error![]const u8 {
        const root = self.resolveLinks(id);
        try self.buildCanonicalKey(root);
        return try self.allocator.dupe(u8, self.scratch_intern_key.items);
    }

    /// Add and intern a fully resolved executable type node.
    pub fn internResolved(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const raw = try self.addType(content);
        return try self.internTypeId(raw);
    }

    /// Intern an existing executable type node and return its canonical id.
    pub fn internTypeId(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (self.containsAbstractLeaf(root)) return root;
        if (self.interned_by_raw.get(root)) |cached| {
            if (cached != root) {
                self.replaceType(root, .{ .link = cached });
            }
            return cached;
        }

        var active = std.AutoHashMap(TypeId, TypeId).init(self.allocator);
        defer active.deinit();
        return try self.internTypeIdInner(root, &active);
    }

    /// Resolve links and nominal backing types before returning executable content.
    pub fn getType(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                .nominal => |nominal| current = nominal.backing,
                else => |content| return content,
            }
        }
    }

    /// Resolve links while preserving outer nominal wrappers.
    pub fn getTypePreservingNominal(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                else => |content| return content,
            }
        }
    }

    /// Duplicate a borrowed executable type-id slice into store-owned memory.
    pub fn dupeTypeIds(self: *const Store, ids: []const TypeId) std.mem.Allocator.Error![]const TypeId {
        if (ids.len == 0) return &.{};
        return try self.allocator.dupe(TypeId, ids);
    }

    /// Duplicate executable tag metadata into store-owned memory.
    pub fn dupeTags(self: *const Store, tags: []const Tag) std.mem.Allocator.Error![]const Tag {
        if (tags.len == 0) return &.{};

        const out = try self.allocator.alloc(Tag, tags.len);
        errdefer {
            var i: usize = 0;
            while (i < tags.len) : (i += 1) {
                if (out[i].args.len > 0) self.allocator.free(out[i].args);
            }
            self.allocator.free(out);
        }

        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = tag.name,
                .args = try self.dupeTypeIds(tag.args),
            };
        }

        return out;
    }

    /// Duplicate executable field metadata into store-owned memory.
    pub fn dupeFields(self: *const Store, fields: []const Field) std.mem.Allocator.Error![]const Field {
        if (fields.len == 0) return &.{};
        return try self.allocator.dupe(Field, fields);
    }

    /// Duplicate executable callable signature metadata into store-owned memory.
    pub fn dupeCallableSig(self: *const Store, sig: CallableSig) std.mem.Allocator.Error!CallableSig {
        return .{
            .args = try self.dupeTypeIds(sig.args),
            .ret = sig.ret,
        };
    }

    /// Compare two executable types structurally.
    pub fn equalIds(self: *const Store, left: TypeId, right: TypeId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(left, right, &visited) catch false;
    }

    /// Report whether an executable type still contains abstract leaves.
    pub fn containsAbstractLeaf(self: *const Store, id: TypeId) bool {
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.containsAbstractLeafVisited(id, &visited) catch true;
    }

    /// Report whether an executable type is fully concrete and internable.
    pub fn isFullyResolved(self: *const Store, id: TypeId) bool {
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.isFullyResolvedVisited(id, &visited) catch false;
    }

    fn resolveLinks(self: *const Store, id: TypeId) TypeId {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                else => return current,
            }
        }
    }

    fn freeOwnedContent(self: *Store, content: Content) void {
        switch (content) {
            .nominal => |nominal| {
                if (nominal.args.len > 0) self.allocator.free(nominal.args);
            },
            .erased_fn => |erased_fn| {
                if (erased_fn.call.args.len > 0) self.allocator.free(erased_fn.call.args);
            },
            .tuple => |elems| {
                if (elems.len > 0) self.allocator.free(elems);
            },
            .tag_union => |tag_union| {
                if (tag_union.tags.len > 0) {
                    for (tag_union.tags) |tag| {
                        if (tag.args.len > 0) self.allocator.free(tag.args);
                    }
                    self.allocator.free(tag_union.tags);
                }
                if (tag_union.call) |call| {
                    if (call.args.len > 0) self.allocator.free(call.args);
                }
            },
            .record => |record| {
                if (record.fields.len > 0) self.allocator.free(record.fields);
            },
            else => {},
        }
    }

    fn replaceType(self: *Store, id: TypeId, content: Content) void {
        const idx = @intFromEnum(id);
        self.freeOwnedContent(self.types.items[idx]);
        self.types.items[idx] = content;
    }

    fn isFullyResolvedVisited(
        self: *const Store,
        id: TypeId,
        visited: *std.AutoHashMap(TypeId, void),
    ) std.mem.Allocator.Error!bool {
        const root = self.resolveLinks(id);
        if (visited.contains(root)) return true;
        try visited.put(root, {});
        return switch (self.types.items[@intFromEnum(root)]) {
            .placeholder => false,
            .unbd => true,
            .link => unreachable,
            .primitive => true,
            .nominal => |nominal| blk: {
                for (nominal.args) |arg| {
                    if (!try self.isFullyResolvedVisited(arg, visited)) break :blk false;
                }
                break :blk try self.isFullyResolvedVisited(nominal.backing, visited);
            },
            .list => |elem| try self.isFullyResolvedVisited(elem, visited),
            .box => |elem| try self.isFullyResolvedVisited(elem, visited),
            .erased_fn => |erased_fn| blk: {
                if (erased_fn.capture) |capture| {
                    if (!try self.isFullyResolvedVisited(capture, visited)) break :blk false;
                }
                for (erased_fn.call.args) |arg| {
                    if (!try self.isFullyResolvedVisited(arg, visited)) break :blk false;
                }
                break :blk try self.isFullyResolvedVisited(erased_fn.call.ret, visited);
            },
            .tuple => |tuple| blk: {
                for (tuple) |elem| {
                    if (!try self.isFullyResolvedVisited(elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (!try self.isFullyResolvedVisited(arg, visited)) break :blk false;
                    }
                }
                if (tag_union.call) |call| {
                    for (call.args) |arg| {
                        if (!try self.isFullyResolvedVisited(arg, visited)) break :blk false;
                    }
                    if (!try self.isFullyResolvedVisited(call.ret, visited)) break :blk false;
                }
                break :blk true;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (!try self.isFullyResolvedVisited(field.ty, visited)) break :blk false;
                }
                break :blk true;
            },
        };
    }

    fn internTypeIdInner(
        self: *Store,
        raw_id: TypeId,
        active: *std.AutoHashMap(TypeId, TypeId),
    ) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(raw_id);
        if (self.interned_by_raw.get(root)) |cached| {
            if (cached != root) {
                self.replaceType(root, .{ .link = cached });
            }
            return cached;
        }
        if (active.get(root)) |pending| return pending;

        const root_content = self.types.items[@intFromEnum(root)];
        switch (root_content) {
            .placeholder => debugPanic("lambdamono.type internTypeId encountered unresolved placeholder", .{}),
            .link => unreachable,
            else => {},
        }

        try active.put(root, root);
        const interned_content: Content = switch (root_content) {
            .placeholder, .link => unreachable,
            .unbd => .unbd,
            .primitive => |prim| .{ .primitive = prim },
            .nominal => |nominal| blk: {
                const lowered_args = try self.allocator.alloc(TypeId, nominal.args.len);
                errdefer self.allocator.free(lowered_args);
                for (nominal.args, 0..) |arg, i| {
                    lowered_args[i] = try self.internTypeIdInner(arg, active);
                }
                break :blk .{ .nominal = .{
                    .module_idx = nominal.module_idx,
                    .ident = nominal.ident,
                    .is_opaque = nominal.is_opaque,
                    .args = lowered_args,
                    .backing = try self.internTypeIdInner(nominal.backing, active),
                } };
            },
            .list => |elem| .{ .list = try self.internTypeIdInner(elem, active) },
            .box => |elem| .{ .box = try self.internTypeIdInner(elem, active) },
            .erased_fn => |erased_fn| blk: {
                const lowered_call_args = try self.allocator.alloc(TypeId, erased_fn.call.args.len);
                errdefer self.allocator.free(lowered_call_args);
                for (erased_fn.call.args, 0..) |arg, i| {
                    lowered_call_args[i] = try self.internTypeIdInner(arg, active);
                }
                break :blk .{ .erased_fn = .{
                    .capture = if (erased_fn.capture) |capture|
                        try self.internTypeIdInner(capture, active)
                    else
                        null,
                    .call = .{
                        .args = lowered_call_args,
                        .ret = try self.internTypeIdInner(erased_fn.call.ret, active),
                    },
                } };
            },
            .tuple => |tuple| blk: {
                const lowered_elems = try self.allocator.alloc(TypeId, tuple.len);
                errdefer self.allocator.free(lowered_elems);
                for (tuple, 0..) |elem, i| {
                    lowered_elems[i] = try self.internTypeIdInner(elem, active);
                }
                break :blk .{ .tuple = lowered_elems };
            },
            .record => |record| blk: {
                const lowered_fields = try self.allocator.alloc(Field, record.fields.len);
                errdefer self.allocator.free(lowered_fields);
                for (record.fields, 0..) |field, i| {
                    lowered_fields[i] = .{
                        .name = field.name,
                        .ty = try self.internTypeIdInner(field.ty, active),
                    };
                }
                break :blk .{ .record = .{
                    .fields = lowered_fields,
                } };
            },
            .tag_union => |tag_union| blk: {
                const lowered_tags = try self.allocator.alloc(Tag, tag_union.tags.len);
                errdefer {
                    var i: usize = 0;
                    while (i < tag_union.tags.len) : (i += 1) {
                        if (lowered_tags[i].args.len > 0) self.allocator.free(lowered_tags[i].args);
                    }
                    self.allocator.free(lowered_tags);
                }
                for (tag_union.tags, 0..) |tag, i| {
                    const lowered_args = try self.allocator.alloc(TypeId, tag.args.len);
                    errdefer self.allocator.free(lowered_args);
                    for (tag.args, 0..) |arg, j| {
                        lowered_args[j] = try self.internTypeIdInner(arg, active);
                    }
                    lowered_tags[i] = .{
                        .name = tag.name,
                        .args = lowered_args,
                    };
                }
                self.assertDistinctSortedTags(lowered_tags);
                break :blk .{ .tag_union = .{
                    .tags = lowered_tags,
                    .call = if (tag_union.call) |call| blk2: {
                        const lowered_call_args = try self.allocator.alloc(TypeId, call.args.len);
                        errdefer self.allocator.free(lowered_call_args);
                        for (call.args, 0..) |arg, i| {
                            lowered_call_args[i] = try self.internTypeIdInner(arg, active);
                        }
                        break :blk2 .{
                            .args = lowered_call_args,
                            .ret = try self.internTypeIdInner(call.ret, active),
                        };
                    } else null,
                } };
            },
        };

        self.replaceType(root, interned_content);
        try self.buildCanonicalKey(root);
        if (self.lookupInternedScratchKey()) |existing| {
            if (root != existing) {
                self.replaceType(root, .{ .link = existing });
            }
            try self.interned_by_raw.put(root, existing);
            const removed = active.remove(root);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(removed);
            } else if (!removed) {
                unreachable;
            }
            return existing;
        }
        try self.rememberScratchInternKey(root);
        try self.interned_by_raw.put(root, root);
        const removed = active.remove(root);
        if (comptime builtin.mode == .Debug) {
            std.debug.assert(removed);
        } else if (!removed) {
            unreachable;
        }
        return root;
    }

    fn appendInternKeyValue(self: *Store, value: anytype) std.mem.Allocator.Error!void {
        var copy = value;
        try self.scratch_intern_key.appendSlice(self.allocator, std.mem.asBytes(&copy));
    }

    fn lookupInternedScratchKey(self: *Store) ?TypeId {
        return self.interned_types.get(self.scratch_intern_key.items);
    }

    fn rememberScratchInternKey(self: *Store, id: TypeId) std.mem.Allocator.Error!void {
        if (self.interned_types.get(self.scratch_intern_key.items) != null) return;
        const owned_key = try self.allocator.dupe(u8, self.scratch_intern_key.items);
        errdefer self.allocator.free(owned_key);
        try self.interned_types.put(owned_key, id);
    }

    fn buildCanonicalKey(self: *Store, root: TypeId) std.mem.Allocator.Error!void {
        self.scratch_intern_key.clearRetainingCapacity();
        try self.scratch_intern_key.appendSlice(self.allocator, "LTY");

        const VisitState = enum(u8) { unseen, active, done };
        var states = std.AutoHashMap(TypeId, VisitState).init(self.allocator);
        defer states.deinit();
        var binder_ids = std.AutoHashMap(TypeId, u32).init(self.allocator);
        defer binder_ids.deinit();
        var next_binder: u32 = 0;

        const Builder = struct {
            store: *Store,
            states: *std.AutoHashMap(TypeId, VisitState),
            binder_ids: *std.AutoHashMap(TypeId, u32),
            next_binder: *u32,

            fn serializeTagName(self_builder: *@This(), name: TagName) std.mem.Allocator.Error!void {
                switch (name) {
                    .ctor => |ctor| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 30));
                        try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(ctor)));
                    },
                    .lambda => |lambda| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 31));
                        try self_builder.store.appendInternKeyValue(lambda.raw());
                    },
                }
            }

            fn serializeType(self_builder: *@This(), id: TypeId) std.mem.Allocator.Error!void {
                const root_id = self_builder.store.resolveLinks(id);
                const state = self_builder.states.get(root_id) orelse .unseen;
                switch (state) {
                    .active, .done => {
                        try self_builder.store.appendInternKeyValue(@as(u8, 1));
                        try self_builder.store.appendInternKeyValue(self_builder.binder_ids.get(root_id).?);
                        return;
                    },
                    .unseen => {},
                }

                const content = self_builder.store.types.items[@intFromEnum(root_id)];
                switch (content) {
                    .link => unreachable,
                    else => {},
                }

                try self_builder.states.put(root_id, .active);
                try self_builder.binder_ids.put(root_id, self_builder.next_binder.*);
                self_builder.next_binder.* += 1;

                try self_builder.store.appendInternKeyValue(@as(u8, 2));
                switch (content) {
                    .placeholder => {
                        try self_builder.store.appendInternKeyValue(@as(u8, 18));
                        try self_builder.store.appendInternKeyValue(self_builder.binder_ids.get(root_id).?);
                    },
                    .unbd => {
                        try self_builder.store.appendInternKeyValue(@as(u8, 19));
                        try self_builder.store.appendInternKeyValue(self_builder.binder_ids.get(root_id).?);
                    },
                    .link => unreachable,
                    .primitive => |prim| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 10));
                        try self_builder.store.appendInternKeyValue(@intFromEnum(prim));
                    },
                    .nominal => |nominal| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 11));
                        try self_builder.store.appendInternKeyValue(nominal.module_idx);
                        try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(nominal.ident)));
                        try self_builder.store.appendInternKeyValue(@as(u8, @intFromBool(nominal.is_opaque)));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(nominal.args.len)));
                        for (nominal.args) |arg| {
                            try self_builder.serializeType(arg);
                        }
                        try self_builder.serializeType(nominal.backing);
                    },
                    .list => |elem| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 12));
                        try self_builder.serializeType(elem);
                    },
                    .box => |elem| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 13));
                        try self_builder.serializeType(elem);
                    },
                    .erased_fn => |erased_fn| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 14));
                        try self_builder.store.appendInternKeyValue(@as(u8, if (erased_fn.capture != null) 1 else 0));
                        if (erased_fn.capture) |capture| {
                            try self_builder.serializeType(capture);
                        }
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(erased_fn.call.args.len)));
                        for (erased_fn.call.args) |arg| {
                            try self_builder.serializeType(arg);
                        }
                        try self_builder.serializeType(erased_fn.call.ret);
                    },
                    .tuple => |tuple| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 15));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tuple.len)));
                        for (tuple) |elem| {
                            try self_builder.serializeType(elem);
                        }
                    },
                    .record => |record| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 16));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(record.fields.len)));
                        for (record.fields) |field| {
                            try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(field.name)));
                            try self_builder.serializeType(field.ty);
                        }
                    },
                    .tag_union => |tag_union| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 17));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tag_union.tags.len)));
                        for (tag_union.tags) |tag| {
                            try self_builder.serializeTagName(tag.name);
                            try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tag.args.len)));
                            for (tag.args) |arg| {
                                try self_builder.serializeType(arg);
                            }
                        }
                        try self_builder.store.appendInternKeyValue(@as(u8, if (tag_union.call != null) 1 else 0));
                        if (tag_union.call) |call| {
                            try self_builder.store.appendInternKeyValue(@as(u32, @intCast(call.args.len)));
                            for (call.args) |arg| {
                                try self_builder.serializeType(arg);
                            }
                            try self_builder.serializeType(call.ret);
                        }
                    },
                }

                try self_builder.states.put(root_id, .done);
            }
        };

        var builder = Builder{
            .store = self,
            .states = &states,
            .binder_ids = &binder_ids,
            .next_binder = &next_binder,
        };
        try builder.serializeType(root);
    }

    fn assertDistinctSortedTags(self: *Store, tags: []const Tag) void {
        if (tags.len <= 1) return;

        var prev = tags[0];
        for (tags[1..]) |tag| {
            if (!tagNameEqual(prev.name, tag.name)) {
                prev = tag;
                continue;
            }

            if (prev.args.len != tag.args.len) {
                debugPanic("lambdamono.type duplicate tag constructor had different arity", .{});
            }
            for (prev.args, tag.args) |prev_arg, tag_arg| {
                if (!self.equalIds(prev_arg, tag_arg)) {
                    debugPanic("lambdamono.type duplicate tag constructor had different payload types", .{});
                }
            }
            debugPanic("lambdamono.type duplicate tag constructor reached interning", .{});
        }
    }

    const TypePair = struct {
        left: TypeId,
        right: TypeId,
    };

    fn equalIdsVisited(
        self: *const Store,
        left: TypeId,
        right: TypeId,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!bool {
        if (left == right) return true;

        for (visited.items) |entry| {
            if (entry.left == left and entry.right == right) return true;
        }
        try visited.append(self.allocator, .{ .left = left, .right = right });

        const left_content = self.getTypePreservingNominal(left);
        const right_content = self.getTypePreservingNominal(right);
        if (@as(std.meta.Tag(Content), left_content) != @as(std.meta.Tag(Content), right_content)) return false;

        return switch (left_content) {
            .placeholder => false,
            .unbd => true,
            .link => unreachable,
            .primitive => |prim| prim == right_content.primitive,
            .nominal => |nominal| blk: {
                const right_nominal = right_content.nominal;
                if (nominal.module_idx != right_nominal.module_idx) break :blk false;
                if (nominal.ident != right_nominal.ident) break :blk false;
                if (nominal.is_opaque != right_nominal.is_opaque) break :blk false;
                if (nominal.args.len != right_nominal.args.len) break :blk false;
                for (nominal.args, right_nominal.args) |left_arg, right_arg| {
                    if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                }
                break :blk try self.equalIdsVisited(nominal.backing, right_nominal.backing, visited);
            },
            .list => |elem| self.equalIdsVisited(elem, right_content.list, visited),
            .box => |elem| self.equalIdsVisited(elem, right_content.box, visited),
            .erased_fn => |erased_fn| blk: {
                const right_erased_fn = right_content.erased_fn;
                if ((erased_fn.capture == null) != (right_erased_fn.capture == null)) break :blk false;
                if (erased_fn.capture) |capture| {
                    if (!try self.equalIdsVisited(capture, right_erased_fn.capture.?, visited)) break :blk false;
                }
                if (erased_fn.call.args.len != right_erased_fn.call.args.len) break :blk false;
                for (erased_fn.call.args, right_erased_fn.call.args) |left_arg, right_arg| {
                    if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                }
                break :blk try self.equalIdsVisited(erased_fn.call.ret, right_erased_fn.call.ret, visited);
            },
            .tuple => |tuple| blk: {
                const right_elems = right_content.tuple;
                if (tuple.len != right_elems.len) break :blk false;
                for (tuple, right_elems) |left_elem, right_elem| {
                    if (!try self.equalIdsVisited(left_elem, right_elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .record => |record| blk: {
                const right_fields = right_content.record.fields;
                if (record.fields.len != right_fields.len) break :blk false;
                for (record.fields, right_fields) |left_field, right_field| {
                    if (left_field.name != right_field.name) break :blk false;
                    if (!try self.equalIdsVisited(left_field.ty, right_field.ty, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const right_tag_union = right_content.tag_union;
                const right_tags = right_tag_union.tags;
                if (tag_union.tags.len != right_tags.len) break :blk false;
                for (tag_union.tags, right_tags) |left_tag, right_tag| {
                    if (!tagNameEqual(left_tag.name, right_tag.name)) break :blk false;
                    if (left_tag.args.len != right_tag.args.len) break :blk false;
                    for (left_tag.args, right_tag.args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }
                }
                if ((tag_union.call == null) != (right_tag_union.call == null)) break :blk false;
                if (tag_union.call) |left_call| {
                    const right_call = right_tag_union.call.?;
                    if (left_call.args.len != right_call.args.len) break :blk false;
                    for (left_call.args, right_call.args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }
                    if (!try self.equalIdsVisited(left_call.ret, right_call.ret, visited)) break :blk false;
                }
                break :blk true;
            },
        };
    }

    fn containsAbstractLeafVisited(
        self: *const Store,
        id: TypeId,
        visited: *std.AutoHashMap(TypeId, void),
    ) std.mem.Allocator.Error!bool {
        const root = self.resolveLinks(id);
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        return switch (self.types.items[@intFromEnum(root)]) {
            .placeholder, .unbd => true,
            .link => unreachable,
            .primitive => false,
            // Executable nominal args are retained metadata; only the lowered backing
            // participates in executable layout/concreteness.
            .nominal => |nominal| try self.containsAbstractLeafVisited(nominal.backing, visited),
            .list => |elem| try self.containsAbstractLeafVisited(elem, visited),
            .box => |elem| try self.containsAbstractLeafVisited(elem, visited),
            .erased_fn => |erased_fn| blk: {
                if (erased_fn.capture) |capture| {
                    if (try self.containsAbstractLeafVisited(capture, visited)) break :blk true;
                }
                for (erased_fn.call.args) |arg| {
                    if (try self.containsAbstractLeafVisited(arg, visited)) break :blk true;
                }
                break :blk try self.containsAbstractLeafVisited(erased_fn.call.ret, visited);
            },
            .tuple => |tuple| blk: {
                for (tuple) |elem| {
                    if (try self.containsAbstractLeafVisited(elem, visited)) break :blk true;
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.containsAbstractLeafVisited(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.containsAbstractLeafVisited(arg, visited)) break :blk true;
                    }
                }
                if (tag_union.call) |call| {
                    for (call.args) |arg| {
                        if (try self.containsAbstractLeafVisited(arg, visited)) break :blk true;
                    }
                    if (try self.containsAbstractLeafVisited(call.ret, visited)) break :blk true;
                }
                break :blk false;
            },
        };
    }
};

test "lambdamono type tests" {
    std.testing.refAllDecls(@This());
}

fn tagNameEqual(left: TagName, right: TagName) bool {
    return switch (left) {
        .ctor => |left_ctor| switch (right) {
            .ctor => |right_ctor| left_ctor == right_ctor,
            .lambda => false,
        },
        .lambda => |left_lambda| switch (right) {
            .ctor => false,
            .lambda => |right_lambda| left_lambda == right_lambda,
        },
    };
}

fn debugPanic(comptime fmt: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(fmt, args);
    } else {
        unreachable;
    }
}
