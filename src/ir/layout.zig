//! Cor-style logical executable layouts, extended only where Roc needs explicit
//! list and opaque-pointer layouts.
//!
//! Recursive nominal structure stays explicit here and is only committed to
//! final boxed memory layout at the shared `IR -> LIR/layout` boundary.

const std = @import("std");
const base = @import("base");

pub const LayoutId = enum(u32) { _ };

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

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
    opaque_ptr,
};

pub const Content = union(enum) {
    primitive: Prim,
    nominal: LayoutId,
    list: LayoutId,
    struct_: Span(LayoutId),
    union_: Span(LayoutId),
    box: LayoutId,
};

pub const Node = union(enum) {
    unfilled,
    content: Content,
};

pub const Store = struct {
    const LayoutPair = struct {
        left: LayoutId,
        right: LayoutId,
    };

    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),
    layout_ids: std.ArrayList(LayoutId),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .nodes = .empty,
            .layout_ids = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.nodes.deinit(self.allocator);
        self.layout_ids.deinit(self.allocator);
    }

    pub fn addPlaceholder(self: *Store) std.mem.Allocator.Error!LayoutId {
        const idx: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, .unfilled);
        return @enumFromInt(idx);
    }

    pub fn addContent(self: *Store, content: Content) std.mem.Allocator.Error!LayoutId {
        const idx: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, .{ .content = content });
        return @enumFromInt(idx);
    }

    pub fn getNode(self: *const Store, id: LayoutId) Node {
        return self.nodes.items[@intFromEnum(id)];
    }

    pub fn getContent(self: *const Store, id: LayoutId) Content {
        return switch (self.getNode(id)) {
            .content => |content| content,
            .unfilled => debugPanic("ir.layout.Store.getContent reached unfilled layout"),
        };
    }

    pub fn setContent(self: *Store, id: LayoutId, content: Content) void {
        self.nodes.items[@intFromEnum(id)] = .{ .content = content };
    }

    pub fn addLayoutSpan(self: *Store, ids: []const LayoutId) std.mem.Allocator.Error!Span(LayoutId) {
        if (ids.len == 0) return Span(LayoutId).empty();
        const start: u32 = @intCast(self.layout_ids.items.len);
        try self.layout_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceLayoutSpan(self: *const Store, span: Span(LayoutId)) []const LayoutId {
        if (span.len == 0) return &.{};
        return self.layout_ids.items[span.start..][0..span.len];
    }

    pub fn equalIds(self: *const Store, left: LayoutId, right: LayoutId) bool {
        var visited = std.ArrayList(LayoutPair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(left, right, &visited) catch false;
    }

    fn equalIdsVisited(
        self: *const Store,
        left: LayoutId,
        right: LayoutId,
        visited: *std.ArrayList(LayoutPair),
    ) std.mem.Allocator.Error!bool {
        if (left == right) return true;

        for (visited.items) |pair| {
            if (pair.left == left and pair.right == right) return true;
        }
        try visited.append(self.allocator, .{ .left = left, .right = right });

        const left_node = self.getNode(left);
        const right_node = self.getNode(right);
        if (@as(std.meta.Tag(Node), left_node) != @as(std.meta.Tag(Node), right_node)) return false;

        return switch (left_node) {
            .unfilled => false,
            .content => |left_content| blk: {
                const right_content = right_node.content;
                if (@as(std.meta.Tag(Content), left_content) != @as(std.meta.Tag(Content), right_content)) {
                    break :blk false;
                }

                break :blk switch (left_content) {
                    .primitive => |prim| prim == right_content.primitive,
                    .nominal => |backing| try self.equalIdsVisited(backing, right_content.nominal, visited),
                    .list => |elem| try self.equalIdsVisited(elem, right_content.list, visited),
                    .box => |elem| try self.equalIdsVisited(elem, right_content.box, visited),
                    .struct_ => |fields| blk2: {
                        const left_fields = self.sliceLayoutSpan(fields);
                        const right_fields = self.sliceLayoutSpan(right_content.struct_);
                        if (left_fields.len != right_fields.len) break :blk2 false;
                        for (left_fields, right_fields) |left_field, right_field| {
                            if (!try self.equalIdsVisited(left_field, right_field, visited)) break :blk2 false;
                        }
                        break :blk2 true;
                    },
                    .union_ => |variants| blk2: {
                        const left_variants = self.sliceLayoutSpan(variants);
                        const right_variants = self.sliceLayoutSpan(right_content.union_);
                        if (left_variants.len != right_variants.len) break :blk2 false;
                        for (left_variants, right_variants) |left_variant, right_variant| {
                            if (!try self.equalIdsVisited(left_variant, right_variant, visited)) break :blk2 false;
                        }
                        break :blk2 true;
                    },
                };
            },
        };
    }
};

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

test "ir layout tests" {
    std.testing.refAllDecls(@This());
}
