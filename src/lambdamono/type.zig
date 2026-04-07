//! Executable monomorphic types.

const std = @import("std");
const base = @import("base");
const solved_type = @import("lambdasolved").Type;

pub const TypeId = enum(u32) { _ };
pub const Prim = solved_type.Prim;
pub const Symbol = @import("symbol").Symbol;

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

pub const TagName = union(enum) {
    ctor: base.Ident.Idx,
    lambda: Symbol,
};

pub const Tag = struct {
    name: TagName,
    args: Span(TypeId),
};

pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeId,
};

pub const Content = union(enum) {
    nominal: TypeId,
    list: TypeId,
    box: TypeId,
    tuple: Span(TypeId),
    tag_union: struct {
        tags: Span(Tag),
    },
    record: struct {
        fields: Span(Field),
    },
    primitive: Prim,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    type_ids: std.ArrayList(TypeId),
    tags: std.ArrayList(Tag),
    fields: std.ArrayList(Field),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .type_ids = .empty,
            .tags = .empty,
            .fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.types.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
    }

    pub fn addType(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const idx: u32 = @intCast(self.types.items.len);
        try self.types.append(self.allocator, content);
        return @enumFromInt(idx);
    }

    pub fn getType(self: *const Store, id: TypeId) Content {
        var current = id;
        while (true) {
            switch (self.types.items[@intFromEnum(current)]) {
                .nominal => |next| current = next,
                else => |content| return content,
            }
        }
    }

    pub fn getTypePreservingNominal(self: *const Store, id: TypeId) Content {
        return self.types.items[@intFromEnum(id)];
    }

    pub fn addTypeSpan(self: *Store, ids: []const TypeId) std.mem.Allocator.Error!Span(TypeId) {
        if (ids.len == 0) return Span(TypeId).empty();
        const start: u32 = @intCast(self.type_ids.items.len);
        try self.type_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceTypeSpan(self: *const Store, span: Span(TypeId)) []const TypeId {
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

        const sorted = try self.allocator.dupe(Tag, tags);
        defer self.allocator.free(sorted);
        std.mem.sort(Tag, sorted, {}, struct {
            fn key(name: TagName) u64 {
                return switch (name) {
                    .ctor => |ident| @as(u64, @as(u32, @bitCast(ident))),
                    .lambda => |symbol| (@as(u64, 1) << 32) | @as(u64, symbol.raw()),
                };
            }

            fn lessThan(_: void, a: Tag, b: Tag) bool {
                return key(a.name) < key(b.name);
            }
        }.lessThan);

        const canonical_len = self.canonicalizeSortedTags(sorted);
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, sorted[0..canonical_len]);
        return .{ .start = start, .len = @intCast(canonical_len) };
    }

    pub fn sliceTags(self: *const Store, span: Span(Tag)) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    pub fn addFields(self: *Store, fields: []const Field) std.mem.Allocator.Error!Span(Field) {
        if (fields.len == 0) return Span(Field).empty();
        if (fields.len == 1) {
            const start_single: u32 = @intCast(self.fields.items.len);
            try self.fields.append(self.allocator, fields[0]);
            return .{ .start = start_single, .len = 1 };
        }

        const sorted = try self.allocator.dupe(Field, fields);
        defer self.allocator.free(sorted);
        std.mem.sort(Field, sorted, {}, struct {
            fn lessThan(_: void, a: Field, b: Field) bool {
                return @as(u32, @bitCast(a.name)) < @as(u32, @bitCast(b.name));
            }
        }.lessThan);

        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, sorted);
        return .{ .start = start, .len = @intCast(fields.len) };
    }

    pub fn sliceFields(self: *const Store, span: Span(Field)) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    pub fn equalIds(self: *const Store, left: TypeId, right: TypeId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(left, right, &visited) catch false;
    }

    const TypePair = struct {
        left: TypeId,
        right: TypeId,
    };

    fn canonicalizeSortedTags(self: *const Store, tags: []Tag) usize {
        if (tags.len <= 1) return tags.len;

        var write_index: usize = 1;
        var prev = tags[0];

        for (tags[1..]) |tag| {
            if (!std.meta.eql(tag.name, prev.name)) {
                tags[write_index] = tag;
                write_index += 1;
                prev = tag;
                continue;
            }

            const prev_args = self.sliceTypeSpan(prev.args);
            const tag_args = self.sliceTypeSpan(tag.args);
            if (prev_args.len != tag_args.len) {
                debugPanic("lambdamono.type duplicate tag constructor had different arity");
            }
            for (prev_args, tag_args) |prev_arg, tag_arg| {
                if (!self.equalIds(prev_arg, tag_arg)) {
                    debugPanic("lambdamono.type duplicate tag constructor had different payload types");
                }
            }
        }

        return write_index;
    }

    fn equalIdsVisited(
        self: *const Store,
        left: TypeId,
        right: TypeId,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!bool {
        if (left == right) return true;
        for (visited.items) |pair| {
            if (pair.left == left and pair.right == right) return true;
        }
        try visited.append(self.allocator, .{ .left = left, .right = right });

        const left_content = self.getTypePreservingNominal(left);
        const right_content = self.getTypePreservingNominal(right);
        if (@as(std.meta.Tag(Content), left_content) != @as(std.meta.Tag(Content), right_content)) return false;

        return switch (left_content) {
            .nominal => |backing| self.equalIdsVisited(backing, right_content.nominal, visited),
            .primitive => |prim| prim == right_content.primitive,
            .list => |elem| self.equalIdsVisited(elem, right_content.list, visited),
            .box => |elem| self.equalIdsVisited(elem, right_content.box, visited),
            .tuple => |tuple| blk: {
                const left_elems = self.sliceTypeSpan(tuple);
                const right_elems = self.sliceTypeSpan(right_content.tuple);
                if (left_elems.len != right_elems.len) break :blk false;
                for (left_elems, right_elems) |left_elem, right_elem| {
                    if (!try self.equalIdsVisited(left_elem, right_elem, visited)) break :blk false;
                }
                break :blk true;
            },
            .record => |record| blk: {
                const left_fields = self.sliceFields(record.fields);
                const right_fields = self.sliceFields(right_content.record.fields);
                if (left_fields.len != right_fields.len) break :blk false;
                for (left_fields, right_fields) |left_field, right_field| {
                    if (left_field.name != right_field.name) break :blk false;
                    if (!try self.equalIdsVisited(left_field.ty, right_field.ty, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tag_union| blk: {
                const left_tags = self.sliceTags(tag_union.tags);
                const right_tags = self.sliceTags(right_content.tag_union.tags);
                if (left_tags.len != right_tags.len) break :blk false;
                for (left_tags, right_tags) |left_tag, right_tag| {
                    if (!std.meta.eql(left_tag.name, right_tag.name)) break :blk false;
                    const left_args = self.sliceTypeSpan(left_tag.args);
                    const right_args = self.sliceTypeSpan(right_tag.args);
                    if (left_args.len != right_args.len) break :blk false;
                    for (left_args, right_args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }
                }
                break :blk true;
            },
        };
    }
};

test "lambdamono type tests" {
    std.testing.refAllDecls(@This());
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
