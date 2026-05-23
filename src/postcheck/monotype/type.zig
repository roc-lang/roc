//! Monomorphic type store used by Monotype and Monotype Lifted IR.
//!
//! This store contains closed checked types after static dispatch and numeric
//! defaulting have been finalized. It has no lambda sets and no layout data.

const std = @import("std");
const check = @import("check");

const names = check.CheckedNames;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;

/// Identifier for a monomorphic type in this store.
pub const TypeId = enum(u32) { _ };

/// Slice descriptor for type, field, or tag arrays in this store.
pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

/// Primitive type copied from checked module data.
pub const Primitive = checked.CheckedPrimitive;

/// Static-dispatch owner head for a monomorphic receiver type.
pub const OwnerHead = union(enum) {
    none,
    builtin: static_dispatch.BuiltinOwner,
    named_type: TypeDef,
};

/// Named type definition owner.
pub const TypeDef = struct {
    module_name: names.ModuleNameId,
    type_name: names.TypeNameId,
};

/// Named checked type instance.
pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked.CheckedTypeId,
};

/// How much of a named type's backing type later stages may inspect.
pub const BackingUse = enum {
    inspectable,
    runtime_layout_only,
};

/// Backing type for a named type when checking output one.
pub const NamedBacking = struct {
    ty: TypeId,
    use: BackingUse,
};

/// Kind of named type visible after checking.
pub const NamedKind = enum {
    nominal,
    @"opaque",
    alias,
};

/// Record field type entry.
pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
};

/// Tag-union variant type entry.
pub const Tag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: Span,
};

/// Monomorphic type content.
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

/// Store for monomorphic types and their shared spans.
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
            .list => .{ .builtin = .list },
            .box => .{ .builtin = .box },
            .named => |named| if (named.builtin_owner) |owner|
                .{ .builtin = owner }
            else
                .{ .named_type = named.def },
            else => .none,
        };
    }

    pub fn typeDigest(self: *const Store, name_store: *const names.NameStore, ty: TypeId) names.TypeDigest {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        self.writeTypeDigest(name_store, &hasher, ty);
        return .{ .bytes = hasher.finalResult() };
    }

    fn writeTypeDigest(
        self: *const Store,
        name_store: *const names.NameStore,
        hasher: *std.crypto.hash.sha2.Sha256,
        ty: TypeId,
    ) void {
        switch (self.get(ty)) {
            .primitive => |primitive| {
                writeBytes(hasher, "primitive");
                writeBytes(hasher, @tagName(primitive));
            },
            .named => |named| {
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, name_store.moduleNameText(named.def.module_name));
                writeBytes(hasher, name_store.typeNameText(named.def.type_name));
                writeBytes(hasher, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(hasher, "builtin");
                    writeBytes(hasher, @tagName(owner));
                } else {
                    writeBytes(hasher, "not-builtin");
                }
                self.writeTypeSpanDigest(name_store, hasher, named.args);
            },
            .record => |fields| {
                writeBytes(hasher, "record");
                const field_slice = self.fieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeBytes(hasher, name_store.recordFieldLabelText(field.name));
                    self.writeTypeDigest(name_store, hasher, field.ty);
                }
            },
            .tuple => |items| {
                writeBytes(hasher, "tuple");
                self.writeTypeSpanDigest(name_store, hasher, items);
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                const tag_slice = self.tagSpan(tags);
                writeU32(hasher, @intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    writeBytes(hasher, name_store.tagLabelText(tag.name));
                    self.writeTypeSpanDigest(name_store, hasher, tag.payloads);
                }
            },
            .list => |elem| {
                writeBytes(hasher, "list");
                self.writeTypeDigest(name_store, hasher, elem);
            },
            .box => |elem| {
                writeBytes(hasher, "box");
                self.writeTypeDigest(name_store, hasher, elem);
            },
            .func => |function| {
                writeBytes(hasher, "func");
                self.writeTypeSpanDigest(name_store, hasher, function.args);
                self.writeTypeDigest(name_store, hasher, function.ret);
            },
            .erased => |erased| {
                writeBytes(hasher, "erased");
                hasher.update(&erased.bytes);
            },
            .zst => writeBytes(hasher, "zst"),
        }
    }

    fn writeTypeSpanDigest(
        self: *const Store,
        name_store: *const names.NameStore,
        hasher: *std.crypto.hash.sha2.Sha256,
        span_: Span,
    ) void {
        const values = self.span(span_);
        writeU32(hasher, @intCast(values.len));
        for (values) |child| {
            self.writeTypeDigest(name_store, hasher, child);
        }
    }
};

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    hasher.update(&buf);
}

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
