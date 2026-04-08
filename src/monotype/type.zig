//! Monomorphic type graph used by the first cor-style lowering pass.

const std = @import("std");
const base = @import("base");

pub const TypeId = enum(u32) { _ };
pub const TypeSpan = extern struct {
    start: u32,
    len: u32,

    pub fn empty() TypeSpan {
        return .{ .start = 0, .len = 0 };
    }
};

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
    args: TypeSpan,
};

pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeId,
};

pub const Content = union(enum) {
    placeholder,
    link: TypeId,
    func: struct {
        arg: TypeId,
        ret: TypeId,
    },
    nominal: TypeId,
    list: TypeId,
    box: TypeId,
    tuple: TypeSpan,
    tag_union: struct {
        tags: Span(Tag),
    },
    record: struct {
        fields: Span(Field),
    },
    primitive: Prim,
};

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    type_ids: std.ArrayList(TypeId),
    tags: std.ArrayList(Tag),
    fields: std.ArrayList(Field),
    interned_types: std.StringHashMap(TypeId),
    scratch_intern_key: std.ArrayList(u8),
    canonical_by_raw: std.AutoHashMap(TypeId, TypeId),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .type_ids = .empty,
            .tags = .empty,
            .fields = .empty,
            .interned_types = std.StringHashMap(TypeId).init(allocator),
            .scratch_intern_key = .empty,
            .canonical_by_raw = std.AutoHashMap(TypeId, TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *Store) void {
        self.types.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        var keys = self.interned_types.keyIterator();
        while (keys.next()) |key| {
            self.allocator.free(key.*);
        }
        self.interned_types.deinit();
        self.scratch_intern_key.deinit(self.allocator);
        self.canonical_by_raw.deinit();
    }

    /// Append a raw type node. This is for explicit builder-time mutation only.
    pub fn addType(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, content);
        return @enumFromInt(idx);
    }

    pub fn setType(self: *Store, id: TypeId, content: Content) void {
        self.types.items[@intFromEnum(id)] = content;
    }

    /// Intern a fully resolved type graph rooted at `id` and return its canonical id.
    pub fn canonicalizeResolved(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (self.canonical_by_raw.get(root)) |cached| return cached;

        var active = std.AutoHashMap(TypeId, TypeId).init(self.allocator);
        defer active.deinit();
        return try self.canonicalizeResolvedInner(root, &active);
    }

    pub fn keyId(self: *Store, id: TypeId) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(id);
        if (!self.isFullyResolved(root)) return root;
        return try self.canonicalizeResolved(root);
    }

    /// Convenience for callers creating an already-resolved one-off type.
    pub fn internResolved(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const raw = try self.addType(content);
        return try self.canonicalizeResolved(raw);
    }

    pub fn getType(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .link => |next| current = next,
                .nominal => |next| current = next,
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

    pub fn addTypeSpan(self: *Store, ids: []const TypeId) std.mem.Allocator.Error!TypeSpan {
        if (ids.len == 0) return .empty();
        const start: u32 = @intCast(self.type_ids.items.len);
        try self.type_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceTypeSpan(self: *const Store, span: TypeSpan) []const TypeId {
        if (span.len == 0) return &.{};
        return self.type_ids.items[span.start..][0..span.len];
    }

    pub fn addTags(self: *Store, tags: []const Tag) std.mem.Allocator.Error!Span(Tag) {
        if (tags.len == 0) return Span(Tag).empty();
        if (tags.len == 1) {
            const start_single: u32 = @intCast(self.tags.items.len);
            try self.tags.append(self.allocator, tags[0]);
            return .{ .start = start_single, .len = 1 };
        }

        const canonical = try self.allocator.dupe(Tag, tags);
        defer self.allocator.free(canonical);

        const canonical_len = self.canonicalizeSortedTags(canonical);
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, canonical[0..canonical_len]);
        return .{ .start = start, .len = @intCast(canonical_len) };
    }

    pub fn sliceTags(self: *const Store, span: Span(Tag)) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    pub fn addFields(self: *Store, fields: []const Field) std.mem.Allocator.Error!Span(Field) {
        if (fields.len == 0) return Span(Field).empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, fields);
        return .{ .start = start, .len = @intCast(fields.len) };
    }

    pub fn sliceFields(self: *const Store, span: Span(Field)) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    pub fn equalIds(self: *const Store, a: TypeId, b: TypeId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(a, b, &visited) catch false;
    }

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
            .link => unreachable,
            .primitive => true,
            .func => |func| try self.isFullyResolvedVisited(func.arg, visited) and
                try self.isFullyResolvedVisited(func.ret, visited),
            .nominal => |backing| try self.isFullyResolvedVisited(backing, visited),
            .list => |elem| try self.isFullyResolvedVisited(elem, visited),
            .box => |elem| try self.isFullyResolvedVisited(elem, visited),
            .tuple => |tuple| blk: {
                for (self.sliceTypeSpan(tuple)) |elem| {
                    if (!try self.isFullyResolvedVisited(elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                for (self.sliceTags(tag_union.tags)) |tag| {
                    for (self.sliceTypeSpan(tag.args)) |arg| {
                        if (!try self.isFullyResolvedVisited(arg, visited)) break :blk false;
                    }
                }
                break :blk true;
            },
            .record => |record| blk: {
                for (self.sliceFields(record.fields)) |field| {
                    if (!try self.isFullyResolvedVisited(field.ty, visited)) break :blk false;
                }
                break :blk true;
            },
        };
    }

    fn canonicalizeResolvedInner(
        self: *Store,
        raw_id: TypeId,
        active: *std.AutoHashMap(TypeId, TypeId),
    ) std.mem.Allocator.Error!TypeId {
        const root = self.resolveLinks(raw_id);
        if (self.canonical_by_raw.get(root)) |cached| return cached;
        if (active.get(root)) |pending| return pending;

        try self.buildCanonicalKey(root);
        if (self.lookupInternedScratchKey()) |existing| {
            if (root != existing) {
                self.setType(root, .{ .link = existing });
            }
            try self.canonical_by_raw.put(root, existing);
            return existing;
        }

        const root_content = self.types.items[@intFromEnum(root)];
        switch (root_content) {
            .placeholder => debugPanic("monotype.type canonicalizeResolved encountered unresolved placeholder"),
            .link => unreachable,
            else => {},
        }

        try active.put(root, root);
        const canonical_content: Content = switch (root_content) {
            .placeholder, .link => unreachable,
            .primitive => |prim| .{ .primitive = prim },
            .func => |func| .{ .func = .{
                .arg = try self.canonicalizeResolvedInner(func.arg, active),
                .ret = try self.canonicalizeResolvedInner(func.ret, active),
            } },
            .nominal => |backing| .{ .nominal = try self.canonicalizeResolvedInner(backing, active) },
            .list => |elem| .{ .list = try self.canonicalizeResolvedInner(elem, active) },
            .box => |elem| .{ .box = try self.canonicalizeResolvedInner(elem, active) },
            .tuple => |tuple| blk: {
                const elems = self.sliceTypeSpan(tuple);
                const lowered_elems = try self.allocator.alloc(TypeId, elems.len);
                defer self.allocator.free(lowered_elems);
                for (elems, 0..) |elem, i| {
                    lowered_elems[i] = try self.canonicalizeResolvedInner(elem, active);
                }
                break :blk .{ .tuple = try self.addTypeSpan(lowered_elems) };
            },
            .tag_union => |tag_union| blk: {
                const tags = self.sliceTags(tag_union.tags);
                const lowered_tags = try self.allocator.alloc(Tag, tags.len);
                defer self.allocator.free(lowered_tags);
                for (tags, 0..) |tag, i| {
                    const args = self.sliceTypeSpan(tag.args);
                    const lowered_args = try self.allocator.alloc(TypeId, args.len);
                    defer self.allocator.free(lowered_args);
                    for (args, 0..) |arg, j| {
                        lowered_args[j] = try self.canonicalizeResolvedInner(arg, active);
                    }
                    lowered_tags[i] = .{
                        .name = tag.name,
                        .args = try self.addTypeSpan(lowered_args),
                    };
                }
                break :blk .{ .tag_union = .{
                    .tags = try self.addTags(lowered_tags),
                } };
            },
            .record => |record| blk: {
                const fields = self.sliceFields(record.fields);
                const lowered_fields = try self.allocator.alloc(Field, fields.len);
                defer self.allocator.free(lowered_fields);
                for (fields, 0..) |field, i| {
                    lowered_fields[i] = .{
                        .name = field.name,
                        .ty = try self.canonicalizeResolvedInner(field.ty, active),
                    };
                }
                break :blk .{ .record = .{
                    .fields = try self.addFields(lowered_fields),
                } };
            },
        };

        self.setType(root, canonical_content);
        try self.buildCanonicalKey(root);
        try self.rememberScratchInternKey(root);
        try self.canonical_by_raw.put(root, root);
        _ = active.remove(root);
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
                    .placeholder => debugPanic("monotype.type buildCanonicalKey encountered unresolved placeholder"),
                    .link => unreachable,
                    else => {},
                }

                try self_builder.states.put(root_id, .active);
                try self_builder.binder_ids.put(root_id, self_builder.next_binder.*);
                self_builder.next_binder.* += 1;

                try self_builder.store.appendInternKeyValue(@as(u8, 2));
                switch (content) {
                    .placeholder, .link => unreachable,
                    .primitive => |prim| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 10));
                        try self_builder.store.appendInternKeyValue(@intFromEnum(prim));
                    },
                    .func => |func| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 11));
                        try self_builder.serializeType(func.arg);
                        try self_builder.serializeType(func.ret);
                    },
                    .nominal => |backing| {
                        try self_builder.store.appendInternKeyValue(@as(u8, 12));
                        try self_builder.serializeType(backing);
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
                        const elems = self_builder.store.sliceTypeSpan(tuple);
                        try self_builder.store.appendInternKeyValue(@as(u8, 15));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(elems.len)));
                        for (elems) |elem| {
                            try self_builder.serializeType(elem);
                        }
                    },
                    .tag_union => |tag_union| {
                        const tags = self_builder.store.sliceTags(tag_union.tags);
                        try self_builder.store.appendInternKeyValue(@as(u8, 16));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(tags.len)));
                        for (tags) |tag| {
                            try self_builder.store.appendInternKeyValue(@as(u32, @bitCast(tag.name)));
                            const args = self_builder.store.sliceTypeSpan(tag.args);
                            try self_builder.store.appendInternKeyValue(@as(u32, @intCast(args.len)));
                            for (args) |arg| {
                                try self_builder.serializeType(arg);
                            }
                        }
                    },
                    .record => |record| {
                        const fields = self_builder.store.sliceFields(record.fields);
                        try self_builder.store.appendInternKeyValue(@as(u8, 17));
                        try self_builder.store.appendInternKeyValue(@as(u32, @intCast(fields.len)));
                        for (fields) |field| {
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

    fn canonicalizeSortedTags(self: *const Store, tags: []Tag) usize {
        if (tags.len <= 1) return tags.len;

        var write_index: usize = 1;
        var prev = tags[0];

        for (tags[1..]) |tag| {
            if (tag.name != prev.name) {
                tags[write_index] = tag;
                write_index += 1;
                prev = tag;
                continue;
            }

            const prev_args = self.sliceTypeSpan(prev.args);
            const tag_args = self.sliceTypeSpan(tag.args);
            if (prev_args.len != tag_args.len) {
                debugPanic("monotype.type duplicate tag constructor had different arity");
            }
            for (prev_args, tag_args) |prev_arg, tag_arg| {
                if (!self.equalIds(prev_arg, tag_arg)) {
                    debugPanic("monotype.type duplicate tag constructor had different payload types");
                }
            }
        }

        return write_index;
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
            .link => unreachable,
            .nominal => |backing| self.equalIdsVisited(backing, right.nominal, visited),
            .primitive => |prim| prim == right.primitive,
            .func => |func| blk: {
                const right_func = right.func;
                if (!try self.equalIdsVisited(func.arg, right_func.arg, visited)) break :blk false;
                break :blk try self.equalIdsVisited(func.ret, right_func.ret, visited);
            },
            .list => |elem| self.equalIdsVisited(elem, right.list, visited),
            .box => |elem| self.equalIdsVisited(elem, right.box, visited),
            .tuple => |tuple| blk: {
                const left_elems = self.sliceTypeSpan(tuple);
                const right_elems = self.sliceTypeSpan(right.tuple);
                if (left_elems.len != right_elems.len) break :blk false;
                for (left_elems, right_elems) |left_elem, right_elem| {
                    if (!try self.equalIdsVisited(left_elem, right_elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const left_tags = self.sliceTags(tag_union.tags);
                const right_tags = self.sliceTags(right.tag_union.tags);
                if (left_tags.len != right_tags.len) break :blk false;
                for (left_tags, right_tags) |left_tag, right_tag| {
                    if (left_tag.name != right_tag.name) break :blk false;
                    const left_args = self.sliceTypeSpan(left_tag.args);
                    const right_args = self.sliceTypeSpan(right_tag.args);
                    if (left_args.len != right_args.len) break :blk false;
                    for (left_args, right_args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }
                }
                break :blk true;
            },
            .record => |record| blk: {
                const left_fields = self.sliceFields(record.fields);
                const right_fields = self.sliceFields(right.record.fields);
                if (left_fields.len != right_fields.len) break :blk false;
                for (left_fields, right_fields) |left_field, right_field| {
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

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
