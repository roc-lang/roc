//! Monomorphic type graph used by the first cor-style lowering pass.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
pub const TypeId = enum(u32) { _ };
pub const TypeIds = []const TypeId;

pub const Prim = enum(u16) {
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    erased,
};

pub const Tag = struct {
    name: base.Ident.Idx,
    args: TypeIds,
};

pub const Tags = []const Tag;

pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeId,
};

pub const Fields = []const Field;

pub const Nominal = struct {
    module_idx: u32,
    ident: base.Ident.Idx,
    is_opaque: bool,
    args: TypeIds,
    backing: TypeId,
};

pub const Content = union(enum) {
    placeholder,
    unbd,
    link: TypeId,
    func: struct {
        arg: TypeId,
        ret: TypeId,
    },
    nominal: Nominal,
    list: TypeId,
    box: TypeId,
    tuple: TypeIds,
    tag_union: struct {
        tags: Tags,
    },
    record: struct {
        fields: Fields,
    },
    primitive: Prim,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    interned_types: std.StringHashMap(TypeId),
    scratch_intern_key: std.ArrayList(u8),
    interned_by_raw: std.AutoHashMap(TypeId, TypeId),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .interned_types = std.StringHashMap(TypeId).init(allocator),
            .scratch_intern_key = .empty,
            .interned_by_raw = std.AutoHashMap(TypeId, TypeId).init(allocator),
        };
    }

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

    /// Append a raw type node. This is for explicit builder-time mutation only.
    pub fn addType(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, content);
        return @enumFromInt(idx);
    }

    pub fn setType(self: *Store, id: TypeId, content: Content) void {
        self.replaceType(id, content);
    }

    pub fn keyId(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (self.containsAbstractLeaf(root)) return root;
        return try self.internTypeId(root);
    }

    /// Convenience for callers creating an already-resolved one-off type.
    pub fn internResolved(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const raw = try self.addType(content);
        return try self.internTypeId(raw);
    }

    pub fn internTypeId(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (self.interned_by_raw.get(root)) |cached| return cached;

        var active = std.AutoHashMap(TypeId, TypeId).init(self.allocator);
        defer active.deinit();
        return try self.internTypeIdInner(root, &active);
    }

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

    pub fn getTypePreservingNominal(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                else => |content| return content,
            }
        }
    }

    pub fn dupeTypeIds(self: *const Store, ids: []const TypeId) std.mem.Allocator.Error![]const TypeId {
        if (ids.len == 0) return &.{};
        return try self.allocator.dupe(TypeId, ids);
    }

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

    pub fn dupeFields(self: *const Store, fields: []const Field) std.mem.Allocator.Error![]const Field {
        if (fields.len == 0) return &.{};
        return try self.allocator.dupe(Field, fields);
    }

    pub fn equalIds(self: *const Store, a: TypeId, b: TypeId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(a, b, &visited) catch false;
    }

    pub fn containsPlaceholder(self: *const Store, id: TypeId) bool {
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.containsPlaceholderVisited(id, &visited) catch true;
    }

    pub fn containsAbstractLeaf(self: *const Store, id: TypeId) bool {
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.containsAbstractLeafVisited(id, &visited) catch true;
    }

    pub fn isFullyResolved(self: *const Store, id: TypeId) bool {
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        return self.isFullyResolvedVisited(id, &visited) catch false;
    }

    pub fn debugValidateTypeGraph(self: *const Store, root: TypeId) void {
        if (builtin.mode != .Debug) return;
        var visited = std.AutoHashMap(TypeId, void).init(self.allocator);
        defer visited.deinit();
        self.debugValidateTypeGraphInner(root, &visited);
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

    fn debugValidateTypeGraphInner(
        self: *const Store,
        id: TypeId,
        visited: *std.AutoHashMap(TypeId, void),
    ) void {
        const root = self.resolveLinks(id);
        if (visited.contains(root)) return;
        visited.put(root, {}) catch unreachable;

        switch (self.types.items[@intFromEnum(root)]) {
            .placeholder,
            .unbd,
            .primitive,
            => {},
            .link => unreachable,
            .func => |func| {
                self.debugValidateTypeGraphInner(func.arg, visited);
                self.debugValidateTypeGraphInner(func.ret, visited);
            },
            .nominal => |nominal| {
                for (nominal.args) |arg| {
                    self.debugValidateTypeGraphInner(arg, visited);
                }
                self.debugValidateTypeGraphInner(nominal.backing, visited);
            },
            .list => |elem| self.debugValidateTypeGraphInner(elem, visited),
            .box => |elem| self.debugValidateTypeGraphInner(elem, visited),
            .tuple => |tuple| {
                for (tuple) |elem| {
                    self.debugValidateTypeGraphInner(elem, visited);
                }
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        self.debugValidateTypeGraphInner(arg, visited);
                    }
                }
            },
            .record => |record| {
                for (record.fields) |field| {
                    self.debugValidateTypeGraphInner(field.ty, visited);
                }
            },
        }
    }

    fn containsPlaceholderVisited(
        self: *const Store,
        id: TypeId,
        visited: *std.AutoHashMap(TypeId, void),
    ) std.mem.Allocator.Error!bool {
        const root = self.resolveLinks(id);
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        return switch (self.types.items[@intFromEnum(root)]) {
            .placeholder => true,
            .unbd, .primitive => false,
            .link => unreachable,
            .func => |func| try self.containsPlaceholderVisited(func.arg, visited) or
                try self.containsPlaceholderVisited(func.ret, visited),
            .nominal => |nominal| {
                for (nominal.args) |arg| {
                    if (try self.containsPlaceholderVisited(arg, visited)) return true;
                }
                return try self.containsPlaceholderVisited(nominal.backing, visited);
            },
            .list => |elem| try self.containsPlaceholderVisited(elem, visited),
            .box => |elem| try self.containsPlaceholderVisited(elem, visited),
            .tuple => |tuple| blk: {
                for (tuple) |elem| {
                    if (try self.containsPlaceholderVisited(elem, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.containsPlaceholderVisited(arg, visited)) break :blk true;
                    }
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.containsPlaceholderVisited(field.ty, visited)) break :blk true;
                }
                break :blk false;
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
            .primitive => false,
            .link => unreachable,
            .func => |func| try self.containsAbstractLeafVisited(func.arg, visited) or
                try self.containsAbstractLeafVisited(func.ret, visited),
            .nominal => |nominal| {
                for (nominal.args) |arg| {
                    if (try self.containsAbstractLeafVisited(arg, visited)) return true;
                }
                return try self.containsAbstractLeafVisited(nominal.backing, visited);
            },
            .list => |elem| try self.containsAbstractLeafVisited(elem, visited),
            .box => |elem| try self.containsAbstractLeafVisited(elem, visited),
            .tuple => |tuple| blk: {
                for (tuple) |elem| {
                    if (try self.containsAbstractLeafVisited(elem, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    for (tag.args) |arg| {
                        if (try self.containsAbstractLeafVisited(arg, visited)) break :blk true;
                    }
                }
                break :blk false;
            },
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (try self.containsAbstractLeafVisited(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
        };
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
            .func => |func| try self.isFullyResolvedVisited(func.arg, visited) and
                try self.isFullyResolvedVisited(func.ret, visited),
            .nominal => |nominal| {
                for (nominal.args) |arg| {
                    if (!try self.isFullyResolvedVisited(arg, visited)) return false;
                }
                return try self.isFullyResolvedVisited(nominal.backing, visited);
            },
            .list => |elem| try self.isFullyResolvedVisited(elem, visited),
            .box => |elem| try self.isFullyResolvedVisited(elem, visited),
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
        if (self.interned_by_raw.get(root)) |cached| return cached;
        if (active.get(root)) |pending| return pending;

        const root_content = self.types.items[@intFromEnum(root)];
        switch (root_content) {
            .placeholder => debugPanic("monotype.type internTypeId encountered unresolved placeholder", .{}),
            .link => unreachable,
            else => {},
        }

        try active.put(root, root);
        const interned_content: Content = switch (root_content) {
            .placeholder, .link => unreachable,
            .unbd => .unbd,
            .primitive => |prim| .{ .primitive = prim },
            .func => |func| .{ .func = .{
                .arg = try self.internTypeIdInner(func.arg, active),
                .ret = try self.internTypeIdInner(func.ret, active),
            } },
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
            .tuple => |tuple| blk: {
                const lowered_elems = try self.allocator.alloc(TypeId, tuple.len);
                errdefer self.allocator.free(lowered_elems);
                for (tuple, 0..) |elem, i| {
                    lowered_elems[i] = try self.internTypeIdInner(elem, active);
                }
                break :blk .{ .tuple = lowered_elems };
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
                break :blk .{ .tag_union = .{ .tags = lowered_tags } };
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

    fn appendInternKeyTypeId(self: *Store, id: TypeId) std.mem.Allocator.Error!void {
        try self.appendInternKeyValue(@as(u32, @intCast(@intFromEnum(id))));
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
        try self.scratch_intern_key.appendSlice(self.allocator, "MTY");

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
                    .placeholder, .unbd => {
                        try self_builder.store.appendInternKeyValue(@as(u8, 18));
                        try self_builder.store.appendInternKeyValue(self_builder.binder_ids.get(root_id).?);
                    },
                    .link => unreachable,
                    .primitive => |prim| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 10));
                        try self_builder.store.appendInternKeyValue(@intFromEnum(prim));
                    },
                    .func => |func| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 11));
                        try self_builder.serializeType(func.arg);
                        try self_builder.serializeType(func.ret);
                    },
                    .nominal => |nominal| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 12));
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
                        try self_builder.store.appendInternKeyValue(@as(u8, 13));
                        try self_builder.serializeType(elem);
                    },
                    .box => |elem| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 14));
                        try self_builder.serializeType(elem);
                    },
                    .tuple => |tuple| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 15));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tuple.len)));
                        for (tuple) |elem| {
                            try self_builder.serializeType(elem);
                        }
                    },
                    .tag_union => |tag_union| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 16));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tag_union.tags.len)));
                        for (tag_union.tags) |tag| {
                            try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(tag.name)));
                            try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tag.args.len)));
                            for (tag.args) |arg| {
                                try self_builder.serializeType(arg);
                            }
                        }
                    },
                    .record => |record| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 17));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(record.fields.len)));
                        for (record.fields) |field| {
                            try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(field.name)));
                            try self_builder.serializeType(field.ty);
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
            if (@as(u32, @bitCast(tag.name)) > @as(u32, @bitCast(prev.name))) {
                prev = tag;
                continue;
            }
            if (@as(u32, @bitCast(tag.name)) < @as(u32, @bitCast(prev.name))) {
                debugPanic("monotype.type tag constructors were not pre-sorted", .{});
            }
            if (prev.args.len != tag.args.len) {
                debugPanic("monotype.type duplicate tag constructor had different arity", .{});
            }
            for (prev.args, tag.args) |prev_arg, tag_arg| {
                if (!self.equalIds(prev_arg, tag_arg)) {
                    debugPanic("monotype.type duplicate tag constructor had different payload types", .{});
                }
            }
            debugPanic("monotype.type duplicate tag constructor reached interning", .{});
        }
    }

    const TypePair = struct {
        left: TypeId,
        right: TypeId,
    };

    fn equalIdsVisited(
        self: *const Store,
        a: TypeId,
        b: TypeId,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!bool {
        if (a == b) return true;

        for (visited.items) |entry| {
            if (entry.left == a and entry.right == b) return true;
        }
        try visited.append(self.allocator, .{ .left = a, .right = b });

        const left = self.getTypePreservingNominal(a);
        const right = self.getTypePreservingNominal(b);
        if (@as(std.meta.Tag(Content), left) != @as(std.meta.Tag(Content), right)) return false;

        return switch (left) {
            .placeholder => false,
            .unbd => false,
            .link => unreachable,
            .nominal => |nominal| blk: {
                const right_nominal = right.nominal;
                if (nominal.module_idx != right_nominal.module_idx) break :blk false;
                if (nominal.ident != right_nominal.ident) break :blk false;
                if (nominal.is_opaque != right_nominal.is_opaque) break :blk false;
                if (nominal.args.len != right_nominal.args.len) break :blk false;
                for (nominal.args, right_nominal.args) |left_arg, right_arg| {
                    if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                }
                break :blk try self.equalIdsVisited(nominal.backing, right_nominal.backing, visited);
            },
            .primitive => |prim| prim == right.primitive,
            .func => |func| blk: {
                const right_func = right.func;
                if (!try self.equalIdsVisited(func.arg, right_func.arg, visited)) break :blk false;
                break :blk try self.equalIdsVisited(func.ret, right_func.ret, visited);
            },
            .list => |elem| self.equalIdsVisited(elem, right.list, visited),
            .box => |elem| self.equalIdsVisited(elem, right.box, visited),
            .tuple => |tuple| blk: {
                const right_elems = right.tuple;
                if (tuple.len != right_elems.len) break :blk false;
                for (tuple, right_elems) |left_elem, right_elem| {
                    if (!try self.equalIdsVisited(left_elem, right_elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const right_tags = right.tag_union.tags;
                if (tag_union.tags.len != right_tags.len) break :blk false;
                for (tag_union.tags, right_tags) |left_tag, right_tag| {
                    if (left_tag.name != right_tag.name) break :blk false;
                    if (left_tag.args.len != right_tag.args.len) break :blk false;
                    for (left_tag.args, right_tag.args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }
                }
                break :blk true;
            },
            .record => |record| blk: {
                const right_fields = right.record.fields;
                if (record.fields.len != right_fields.len) break :blk false;
                for (record.fields, right_fields) |left_field, right_field| {
                    if (left_field.name != right_field.name) break :blk false;
                    if (!try self.equalIdsVisited(left_field.ty, right_field.ty, visited)) break :blk false;
                }
                break :blk true;
            },
        };
    }
};

test "monotype type tests" {
    std.testing.refAllDecls(@This());
}

test "nominal identity preserves generic arguments" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const bool_ty = try store.internResolved(.{ .primitive = .bool });
    const u8_ty = try store.internResolved(.{ .primitive = .u8 });
    const i64_ty = try store.internResolved(.{ .primitive = .i64 });
    const foo_ident: base.Ident.Idx = .{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = 1,
    };

    const foo_u8 = try store.internResolved(.{ .nominal = .{
        .module_idx = 7,
        .ident = foo_ident,
        .is_opaque = true,
        .args = try store.dupeTypeIds(&.{u8_ty}),
        .backing = bool_ty,
    } });
    const foo_i64 = try store.internResolved(.{ .nominal = .{
        .module_idx = 7,
        .ident = foo_ident,
        .is_opaque = true,
        .args = try store.dupeTypeIds(&.{i64_ty}),
        .backing = bool_ty,
    } });
    const foo_u8_again = try store.internResolved(.{ .nominal = .{
        .module_idx = 7,
        .ident = foo_ident,
        .is_opaque = true,
        .args = try store.dupeTypeIds(&.{u8_ty}),
        .backing = bool_ty,
    } });

    try std.testing.expect(foo_u8 == foo_u8_again);
    try std.testing.expect(!store.equalIds(foo_u8, foo_i64));
}

test "keyId does not intern abstract leaves" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    {
        const arg_ty = try store.addType(.placeholder);
        const ret_ty = try store.addType(.placeholder);
        const func_ty = try store.addType(.{ .func = .{
            .arg = arg_ty,
            .ret = ret_ty,
        } });

        try std.testing.expect(try store.keyId(func_ty) == func_ty);
        try std.testing.expect(store.containsAbstractLeaf(func_ty));
    }

    {
        const arg_ty = try store.addType(.unbd);
        const ret_ty = try store.addType(.unbd);
        const func_ty = try store.addType(.{ .func = .{
            .arg = arg_ty,
            .ret = ret_ty,
        } });

        try std.testing.expect(try store.keyId(arg_ty) == arg_ty);
        try std.testing.expect(try store.keyId(ret_ty) == ret_ty);
        try std.testing.expect(try store.keyId(func_ty) == func_ty);
        try std.testing.expect(store.containsAbstractLeaf(func_ty));
    }
}

fn debugPanic(comptime fmt: []const u8, args: anytype) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(fmt, args);
    } else {
        unreachable;
    }
}
