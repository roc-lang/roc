//! Lambda-set solving types.

const std = @import("std");
const base = @import("base");
const symbol_mod = @import("symbol");
const lifted_type = @import("monotype_lifted").Type;

pub const Symbol = symbol_mod.Symbol;
pub const Prim = lifted_type.Prim;

pub const TypeVarId = enum(u32) { _ };

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }

        pub fn isEmpty(self: @This()) bool {
            return self.len == 0;
        }
    };
}

pub const Capture = struct {
    symbol: Symbol,
    ty: TypeVarId,
};

pub const Lambda = struct {
    symbol: Symbol,
    captures: Span(Capture),
};

pub const Tag = struct {
    name: base.Ident.Idx,
    args: Span(TypeVarId),
};

pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeVarId,
};

pub const Content = union(enum) {
    func: struct {
        arg: TypeVarId,
        lset: TypeVarId,
        ret: TypeVarId,
    },
    list: TypeVarId,
    box: TypeVarId,
    tuple: Span(TypeVarId),
    tag_union: struct {
        tags: Span(Tag),
    },
    record: struct {
        fields: Span(Field),
    },
    primitive: Prim,
    lambda_set: Span(Lambda),
};

pub const Node = union(enum) {
    link: TypeVarId,
    nominal: TypeVarId,
    unbd,
    for_a,
    content: Content,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),
    type_var_ids: std.ArrayList(TypeVarId),
    tags: std.ArrayList(Tag),
    fields: std.ArrayList(Field),
    captures: std.ArrayList(Capture),
    lambdas: std.ArrayList(Lambda),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .nodes = .empty,
            .type_var_ids = .empty,
            .tags = .empty,
            .fields = .empty,
            .captures = .empty,
            .lambdas = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.nodes.deinit(self.allocator);
        self.type_var_ids.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.captures.deinit(self.allocator);
        self.lambdas.deinit(self.allocator);
    }

    pub fn fresh(self: *Store, node: Node) std.mem.Allocator.Error!TypeVarId {
        const idx: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return @enumFromInt(idx);
    }

    pub fn freshUnbd(self: *Store) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.unbd);
    }

    pub fn freshForA(self: *Store) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.for_a);
    }

    pub fn freshContent(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.{ .content = content });
    }

    pub fn getNode(self: *const Store, id: TypeVarId) Node {
        return self.nodes.items[@intFromEnum(id)];
    }

    pub fn setNode(self: *Store, id: TypeVarId, node: Node) void {
        self.nodes.items[@intFromEnum(id)] = node;
    }

    pub fn unlink(self: *Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| blk: {
                const terminal = self.unlink(next);
                if (terminal != next) {
                    self.setNode(id, .{ .link = terminal });
                }
                break :blk terminal;
            },
            .nominal => |next| blk: {
                const terminal = self.unlink(next);
                if (terminal != next) {
                    self.setNode(id, .{ .nominal = terminal });
                }
                break :blk terminal;
            },
            else => id,
        };
    }

    pub fn unlinkPreservingNominal(self: *Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| blk: {
                const terminal = self.unlinkPreservingNominal(next);
                if (terminal != next) {
                    self.setNode(id, .{ .link = terminal });
                }
                break :blk terminal;
            },
            else => id,
        };
    }

    pub fn addTypeVarSpan(self: *Store, ids: []const TypeVarId) std.mem.Allocator.Error!Span(TypeVarId) {
        if (ids.len == 0) return Span(TypeVarId).empty();
        const start: u32 = @intCast(self.type_var_ids.items.len);
        try self.type_var_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceTypeVarSpan(self: *const Store, span: Span(TypeVarId)) []const TypeVarId {
        if (span.len == 0) return &.{};
        return self.type_var_ids.items[span.start..][0..span.len];
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span(Tag) {
        if (values.len == 0) return Span(Tag).empty();
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTags(self: *const Store, span: Span(Tag)) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span(Field) {
        if (values.len == 0) return Span(Field).empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceFields(self: *const Store, span: Span(Field)) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    pub fn addCaptures(self: *Store, values: []const Capture) std.mem.Allocator.Error!Span(Capture) {
        if (values.len == 0) return Span(Capture).empty();
        const start: u32 = @intCast(self.captures.items.len);
        try self.captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceCaptures(self: *const Store, span: Span(Capture)) []const Capture {
        if (span.len == 0) return &.{};
        return self.captures.items[span.start..][0..span.len];
    }

    pub fn addLambdas(self: *Store, values: []const Lambda) std.mem.Allocator.Error!Span(Lambda) {
        if (values.len == 0) return Span(Lambda).empty();
        const start: u32 = @intCast(self.lambdas.items.len);
        try self.lambdas.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceLambdas(self: *const Store, span: Span(Lambda)) []const Lambda {
        if (span.len == 0) return &.{};
        return self.lambdas.items[span.start..][0..span.len];
    }
};

test "lambdasolved type tests" {
    std.testing.refAllDecls(@This());
}
