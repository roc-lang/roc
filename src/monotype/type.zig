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
    func: struct {
        arg: TypeId,
        ret: TypeId,
    },
    list: TypeId,
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
        return self.types.items[@intFromEnum(id)];
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
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, tags);
        return .{ .start = start, .len = @intCast(tags.len) };
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

        const left = self.getType(a);
        const right = self.getType(b);
        if (@as(std.meta.Tag(Content), left) != @as(std.meta.Tag(Content), right)) return false;

        return switch (left) {
            .primitive => |prim| prim == right.primitive,
            .func => |func| blk: {
                const right_func = right.func;
                if (!try self.equalIdsVisited(func.arg, right_func.arg, visited)) break :blk false;
                break :blk try self.equalIdsVisited(func.ret, right_func.ret, visited);
            },
            .list => |elem| self.equalIdsVisited(elem, right.list, visited),
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
