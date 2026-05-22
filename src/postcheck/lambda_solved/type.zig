//! Lambda Solved type graph.

const std = @import("std");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");

pub const names = check.CheckedNames;

pub const TypeVarId = enum(u32) { _ };

pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeVarId,
};

pub const Tag = struct {
    name: names.TagNameId,
    payloads: Span,
};

pub const Capture = struct {
    local: @import("../monotype_lifted/ast.zig").LocalId,
    symbol: Common.Symbol,
    binder: ?check.CheckedModule.PatternBinderId,
    ty: TypeVarId,
};

pub const FnMember = struct {
    lambda: Common.Symbol,
    captures: Span,
};

pub const Content = union(enum) {
    link: TypeVarId,
    unbound,
    forall,
    primitive: MonoType.Primitive,
    named: struct {
        named_type: MonoType.NamedType,
        def: MonoType.TypeDef,
        kind: MonoType.NamedKind,
        args: Span,
        backing: ?struct {
            ty: TypeVarId,
            use: MonoType.BackingUse,
        } = null,
    },
    record: Span,
    tuple: Span,
    tag_union: Span,
    list: TypeVarId,
    box: TypeVarId,
    func: struct {
        args: Span,
        callable: TypeVarId,
        ret: TypeVarId,
    },
    lambda_set: Span,
    erased: struct {
        source_fn_ty: names.TypeDigest,
    },
    zst,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    vars: std.ArrayList(Content),
    spans: std.ArrayList(TypeVarId),
    fields: std.ArrayList(Field),
    tags: std.ArrayList(Tag),
    captures: std.ArrayList(Capture),
    fn_members: std.ArrayList(FnMember),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .vars = .empty,
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
            .captures = .empty,
            .fn_members = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.fn_members.deinit(self.allocator);
        self.captures.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.vars.deinit(self.allocator);
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        const id: TypeVarId = @enumFromInt(@as(u32, @intCast(self.vars.items.len)));
        try self.vars.append(self.allocator, content);
        return id;
    }

    pub fn set(self: *Store, id: TypeVarId, content: Content) void {
        self.vars.items[@intFromEnum(id)] = content;
    }

    pub fn get(self: *const Store, id: TypeVarId) Content {
        return self.vars.items[@intFromEnum(id)];
    }

    pub fn root(self: *const Store, id: TypeVarId) TypeVarId {
        var current = id;
        while (true) {
            switch (self.get(current)) {
                .link => |next| current = next,
                else => return current,
            }
        }
    }

    pub fn rootContent(self: *const Store, id: TypeVarId) Content {
        return self.get(self.root(id));
    }

    pub fn addSpan(self: *Store, values: []const TypeVarId) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.spans.items.len);
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptures(self: *Store, values: []const Capture) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.captures.items.len);
        try self.captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addMembers(self: *Store, values: []const FnMember) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.fn_members.items.len);
        try self.fn_members.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn span(self: *const Store, span_: Span) []const TypeVarId {
        return self.spans.items[span_.start..][0..span_.len];
    }

    pub fn fieldSpan(self: *const Store, span_: Span) []const Field {
        return self.fields.items[span_.start..][0..span_.len];
    }

    pub fn tagSpan(self: *const Store, span_: Span) []const Tag {
        return self.tags.items[span_.start..][0..span_.len];
    }

    pub fn captureSpan(self: *const Store, span_: Span) []const Capture {
        return self.captures.items[span_.start..][0..span_.len];
    }

    pub fn memberSpan(self: *const Store, span_: Span) []const FnMember {
        return self.fn_members.items[span_.start..][0..span_.len];
    }
};

test "lambda solved type declarations are referenced" {
    std.testing.refAllDecls(@This());
}
