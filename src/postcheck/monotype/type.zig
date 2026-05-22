//! Monomorphic type store used by Monotype and Monotype Lifted IR.
//!
//! This store contains closed checked types after static dispatch and numeric
//! defaulting have been finalized. It has no lambda sets and no layout data.

const std = @import("std");
const check = @import("check");

const names = check.CheckedNames;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;

pub const TypeId = enum(u32) { _ };

pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

pub const Primitive = checked.CheckedPrimitive;

pub const OwnerHead = union(enum) {
    none,
    builtin: static_dispatch.BuiltinOwner,
    named_type: TypeDef,
};

pub const TypeDef = struct {
    module_name: names.ModuleNameId,
    type_name: names.TypeNameId,
};

pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked.CheckedTypeId,
};

pub const BackingUse = enum {
    inspectable,
    runtime_layout_only,
};

pub const NamedBacking = struct {
    ty: TypeId,
    use: BackingUse,
};

pub const NamedKind = enum {
    nominal,
    @"opaque",
    alias,
};

pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
};

pub const Tag = struct {
    name: names.TagNameId,
    payloads: Span,
};

pub const Content = union(enum) {
    primitive: Primitive,
    named: struct {
        named_type: NamedType,
        def: TypeDef,
        kind: NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: Span,
        backing: ?NamedBacking = null,
    },
    record: Span,
    tuple: Span,
    tag_union: Span,
    list: TypeId,
    box: TypeId,
    func: struct {
        args: Span,
        ret: TypeId,
    },
    erased: names.TypeDigest,
    zst,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    spans: std.ArrayList(TypeId),
    fields: std.ArrayList(Field),
    tags: std.ArrayList(Tag),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn addSpan(self: *Store, values: []const TypeId) std.mem.Allocator.Error!Span {
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

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const index = self.types.items.len;
        try self.types.append(self.allocator, content);
        return @enumFromInt(@as(u32, @intCast(index)));
    }

    pub fn get(self: *const Store, ty: TypeId) Content {
        return self.types.items[@intFromEnum(ty)];
    }

    pub fn span(self: *const Store, span_: Span) []const TypeId {
        return self.spans.items[span_.start..][0..span_.len];
    }

    pub fn fieldSpan(self: *const Store, span_: Span) []const Field {
        return self.fields.items[span_.start..][0..span_.len];
    }

    pub fn tagSpan(self: *const Store, span_: Span) []const Tag {
        return self.tags.items[span_.start..][0..span_.len];
    }

    pub fn ownerHead(self: *const Store, ty: TypeId) OwnerHead {
        return switch (self.get(ty)) {
            .primitive => |primitive| .{ .builtin = builtinOwner(primitive) },
            .named => |named| if (named.builtin_owner) |owner|
                .{ .builtin = owner }
            else
                .{ .named_type = named.def },
            else => .none,
        };
    }
};

fn builtinOwner(primitive: Primitive) static_dispatch.BuiltinOwner {
    return switch (primitive) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
    };
}

test "monotype type declarations are referenced" {
    std.testing.refAllDecls(@This());
}
