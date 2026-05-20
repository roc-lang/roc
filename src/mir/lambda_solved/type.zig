//! Lambda-solved MIR logical type store.
//!
//! Every function type carries a fresh callable variable. Equal source type
//! shapes do not share callable representation unless value-flow records unify
//! them later.

const std = @import("std");
const builtin = @import("builtin");
const check = @import("check");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const lifted_type = @import("../lifted/mod.zig").Type;

const canonical = check.CanonicalNames;

pub const Prim = lifted_type.Prim;
/// Public `CallableVarId` declaration.
pub const CallableVarId = enum(u32) { _ };

/// Public `TypeVarId` declaration.
pub const TypeVarId = enum(u32) { _ };

/// Public `Span` function.
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

/// Public `Nominal` declaration.
pub const Nominal = struct {
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    is_opaque: bool,
    args: Span(TypeVarId),
    backing: TypeVarId,
};

/// Public `LambdaSolvedFnType` declaration.
pub const LambdaSolvedFnType = struct {
    fixed_arity: u32,
    args: Span(TypeVarId),
    ret: TypeVarId,
    callable: CallableVarId,
};

/// Public `Tag` declaration.
pub const Tag = struct {
    name: canonical.TagLabelId,
    args: Span(TypeVarId),
};

/// Public `Field` declaration.
pub const Field = struct {
    name: canonical.RecordFieldLabelId,
    ty: TypeVarId,
};

/// Public `Content` declaration.
pub const Content = union(enum) {
    func: LambdaSolvedFnType,
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
};

/// Public `Node` declaration.
pub const Node = union(enum) {
    link: TypeVarId,
    nominal: Nominal,
    unbd,
    for_a,
    flex_for_a,
    content: Content,
};

/// Public `Store` declaration.
pub const Store = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),
    type_var_ids: std.ArrayList(TypeVarId),
    tags: std.ArrayList(Tag),
    fields: std.ArrayList(Field),
    next_callable_var: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .nodes = .empty,
            .type_var_ids = .empty,
            .tags = .empty,
            .fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.type_var_ids.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    pub fn freshCallableVar(self: *Store) CallableVarId {
        const id: CallableVarId = @enumFromInt(self.next_callable_var);
        self.next_callable_var += 1;
        return id;
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

    pub fn freshFlexForA(self: *Store) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.flex_for_a);
    }

    pub fn freshContent(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.{ .content = content });
    }

    pub fn freshFunction(self: *Store, args: []const TypeVarId, ret: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const arg_span = try self.addTypeVarSpan(args);
        return try self.freshContent(.{ .func = .{
            .fixed_arity = @intCast(args.len),
            .args = arg_span,
            .ret = ret,
            .callable = self.freshCallableVar(),
        } });
    }

    pub fn getNode(self: *const Store, id: TypeVarId) Node {
        return self.nodes.items[@intFromEnum(id)];
    }

    pub fn setNode(self: *Store, id: TypeVarId, node: Node) void {
        self.nodes.items[@intFromEnum(id)] = node;
    }

    pub fn unlinkConst(self: *const Store, id: TypeVarId) TypeVarId {
        return switch (self.getNode(id)) {
            .link => |next| self.unlinkConst(next),
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
        assertDistinctTags(values);
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
        assertDistinctFields(values);
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceFields(self: *const Store, span: Span(Field)) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    pub fn fnShape(self: *const Store, ty: TypeVarId) LambdaSolvedFnType {
        const root = self.unlinkConst(ty);
        return switch (self.getNode(root)) {
            .content => |content| switch (content) {
                .func => |func| func,
                else => debugPanic("lambda_solved.type fnShape expected function"),
            },
            else => debugPanic("lambda_solved.type fnShape expected function"),
        };
    }
};

fn assertDistinctTags(values: []const Tag) void {
    for (values, 0..) |tag, i| {
        for (values[i + 1 ..]) |other| {
            if (tag.name == other.name) {
                debugPanic("lambda_solved.type duplicate tag constructor reached addTags");
            }
        }
    }
}

fn assertDistinctFields(values: []const Field) void {
    for (values, 0..) |field, i| {
        for (values[i + 1 ..]) |other| {
            if (field.name == other.name) {
                debugPanic("lambda_solved.type duplicate record field reached addFields");
            }
        }
    }
}

fn debugPanic(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "lambda_solved function types carry callable variables" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const i64_ty = try store.freshContent(.{ .primitive = .i64 });
    const fn_ty = try store.freshFunction(&.{i64_ty}, i64_ty);
    const shape = store.fnShape(fn_ty);
    try std.testing.expectEqual(@as(u32, 1), shape.fixed_arity);
}
