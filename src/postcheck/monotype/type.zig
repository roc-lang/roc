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
    source_decl: ?u32 = null,
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

/// One entry of a nominal record's declared layout order. The backing row is
/// always lexicographic (for name resolution and digests); a nominal type
/// additionally carries this declared order, which the layout commit consumes to
/// place fields in source order with no internal padding. See design.md
/// "Nominal Record Field Order".
pub const DeclaredField = union(enum) {
    /// A named backing field, matched against the lexicographic backing row by
    /// name at layout time.
    named: names.RecordFieldNameId,
    /// An unnamed padding field reserving `sizeof(ty)` bytes at alignment 1. Its
    /// bytes are uninitialized and it is not accessible.
    padding: TypeId,
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
        /// Declared field order for a nominal/opaque record backing; empty for
        /// every other named type (consumed only by layout).
        declared_order: Span = Span.empty(),
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
    declared_fields: std.ArrayList(DeclaredField),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
            .declared_fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.declared_fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn addSpan(self: *Store, values: []const TypeId) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.spans.items.len);
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
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

    pub fn addDeclaredFields(self: *Store, values: []const DeclaredField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.declared_fields.items.len);
        try self.declared_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn declaredFieldSpan(self: *const Store, span_: Span) []const DeclaredField {
        return self.declared_fields.items[span_.start..][0..span_.len];
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
        var visiting = DigestVisiting{};
        self.writeTypeDigest(name_store, &hasher, ty, &visiting);
        return .{ .bytes = hasher.finalResult() };
    }

    /// Stack of types currently being digested. Recursive types reference a
    /// type already on this stack; the digest encodes such a back reference by
    /// stack position so cyclic content digests deterministically.
    const DigestVisiting = struct {
        items: [digest_visiting_max]TypeId = undefined,
        len: usize = 0,
    };

    const digest_visiting_max = 256;

    fn writeTypeDigest(
        self: *const Store,
        name_store: *const names.NameStore,
        hasher: *std.crypto.hash.sha2.Sha256,
        ty: TypeId,
        visiting: *DigestVisiting,
    ) void {
        for (visiting.items[0..visiting.len], 0..) |open_ty, position| {
            if (open_ty == ty) {
                writeBytes(hasher, "cycle");
                writeU32(hasher, @intCast(position));
                return;
            }
        }
        if (visiting.len == digest_visiting_max) {
            // Deeper nesting than the stack tracks cannot contain an
            // unrecorded cycle shorter than the stack, so digest the type's
            // identity instead of recursing further.
            writeBytes(hasher, "deep");
            writeU32(hasher, @intFromEnum(ty));
            return;
        }
        visiting.items[visiting.len] = ty;
        visiting.len += 1;
        defer visiting.len -= 1;
        switch (self.get(ty)) {
            .primitive => |primitive| {
                writeBytes(hasher, "primitive");
                writeBytes(hasher, @tagName(primitive));
            },
            .named => |named| {
                // Aliases are transparent: their digest is their backing's.
                if (named.kind == .alias) {
                    const backing = named.backing orelse {
                        writeBytes(hasher, "alias-without-backing");
                        return;
                    };
                    self.writeTypeDigest(name_store, hasher, backing.ty, visiting);
                    return;
                }
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, name_store.moduleNameText(named.def.module_name));
                writeOptionalU32(hasher, named.def.source_decl);
                if (named.def.source_decl == null) {
                    writeBytes(hasher, name_store.typeNameText(named.def.type_name));
                }
                writeBytes(hasher, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(hasher, "builtin");
                    writeBytes(hasher, @tagName(owner));
                    if (owner == .fields) {
                        writeBytes(hasher, "fields-backing");
                        if (named.backing) |backing| {
                            self.writeTypeDigest(name_store, hasher, backing.ty, visiting);
                        } else {
                            writeBytes(hasher, "none");
                        }
                    }
                } else {
                    writeBytes(hasher, "not-builtin");
                }
                self.writeTypeSpanDigest(name_store, hasher, named.args, visiting);
            },
            .record => |fields| {
                writeBytes(hasher, "record");
                const field_slice = self.fieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeBytes(hasher, name_store.recordFieldLabelText(field.name));
                    self.writeTypeDigest(name_store, hasher, field.ty, visiting);
                }
            },
            .tuple => |items| {
                writeBytes(hasher, "tuple");
                self.writeTypeSpanDigest(name_store, hasher, items, visiting);
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                const tag_slice = self.tagSpan(tags);
                writeU32(hasher, @intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    writeBytes(hasher, name_store.tagLabelText(tag.name));
                    self.writeTypeSpanDigest(name_store, hasher, tag.payloads, visiting);
                }
            },
            .list => |elem| {
                writeBytes(hasher, "list");
                self.writeTypeDigest(name_store, hasher, elem, visiting);
            },
            .box => |elem| {
                writeBytes(hasher, "box");
                self.writeTypeDigest(name_store, hasher, elem, visiting);
            },
            .func => |function| {
                writeBytes(hasher, "func");
                self.writeTypeSpanDigest(name_store, hasher, function.args, visiting);
                self.writeTypeDigest(name_store, hasher, function.ret, visiting);
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
        visiting: *DigestVisiting,
    ) void {
        const values = self.span(span_);
        writeU32(hasher, @intCast(values.len));
        for (values) |child| {
            self.writeTypeDigest(name_store, hasher, child, visiting);
        }
    }
};

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

fn writeOptionalU32(hasher: *std.crypto.hash.sha2.Sha256, value: ?u32) void {
    const present: u8 = if (value == null) 0 else 1;
    hasher.update(std.mem.asBytes(&present));
    if (value) |v| writeU32(hasher, v);
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

test "monotype named type digest includes generic arguments" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const module_name = try name_store.internModuleName("Test");
    const type_name = try name_store.internTypeName("Box");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const str = try store.add(.{ .primitive = .str });
    const i64_args = try store.addSpan(&.{i64_ty});
    const str_args = try store.addSpan(&.{str});
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const named_i64 = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module_name = module_name, .type_name = type_name },
        .kind = .nominal,
        .args = i64_args,
    } });
    const named_str = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module_name = module_name, .type_name = type_name },
        .kind = .nominal,
        .args = str_args,
    } });

    const i64_digest = store.typeDigest(&name_store, named_i64);
    const str_digest = store.typeDigest(&name_store, named_str);
    try std.testing.expect(!std.mem.eql(u8, i64_digest.bytes[0..], str_digest.bytes[0..]));
}

test "monotype store keeps function-containing shapes distinct" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const args = try store.addSpan(&.{unit});

    const fn_a = try store.add(.{ .func = .{ .args = args, .ret = unit } });
    const fn_b = try store.add(.{ .func = .{ .args = args, .ret = unit } });
    try std.testing.expect(fn_a != fn_b);

    const list_a = try store.add(.{ .list = fn_a });
    const list_b = try store.add(.{ .list = fn_a });
    try std.testing.expect(list_a != list_b);
}

test "monotype row entries retain checked label ids" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    const field_name = try name_store.internRecordFieldLabel("age");
    const tag_name = try name_store.internTagLabel("Adult");

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const i64_ty = try store.add(.{ .primitive = .i64 });
    const fields = try store.addFields(&.{.{ .name = field_name, .ty = i64_ty }});
    const payloads = try store.addSpan(&.{i64_ty});
    const tags = try store.addTags(&.{.{ .name = tag_name, .checked_name = tag_name, .payloads = payloads }});

    try std.testing.expectEqual(field_name, store.fieldSpan(fields)[0].name);
    try std.testing.expectEqual(tag_name, store.tagSpan(tags)[0].name);
}

test "monotype empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @enumFromInt(2), .checked_name = @enumFromInt(2), .payloads = nonempty_span }});
    try std.testing.expect(nonempty_span.len == 1);
    try std.testing.expect(nonempty_fields.len == 1);
    try std.testing.expect(nonempty_tags.len == 1);

    try std.testing.expectEqual(Span.empty(), try store.addSpan(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addTags(&.{}));
}

test "monotype digest terminates on recursive structural types" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const field_name = try name_store.internRecordFieldLabel("step");

    // A record whose field is a function returning the record itself.
    const rec_a = try store.add(.zst);
    const fn_a = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_a } });
    const fields_a = try store.addFields(&.{.{ .name = field_name, .ty = fn_a }});
    store.types.items[@intFromEnum(rec_a)] = .{ .record = fields_a };

    const first = store.typeDigest(&name_store, rec_a);
    const again = store.typeDigest(&name_store, rec_a);
    try std.testing.expect(std.mem.eql(u8, first.bytes[0..], again.bytes[0..]));

    // An isomorphic cycle at different ids digests identically: cycles are
    // encoded as back references by position, not by id.
    const rec_b = try store.add(.zst);
    const fn_b = try store.add(.{ .func = .{ .args = Span.empty(), .ret = rec_b } });
    const fields_b = try store.addFields(&.{.{ .name = field_name, .ty = fn_b }});
    store.types.items[@intFromEnum(rec_b)] = .{ .record = fields_b };

    const other = store.typeDigest(&name_store, rec_b);
    try std.testing.expect(std.mem.eql(u8, first.bytes[0..], other.bytes[0..]));
}

test "monotype digest treats aliases as their backing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const module_name = try name_store.internModuleName("Test");
    const type_name = try name_store.internTypeName("Pretty");
    const checked_ty: checked.CheckedTypeId = @enumFromInt(1);

    const str = try store.add(.{ .primitive = .str });
    const aliased = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module_name = module_name, .type_name = type_name },
        .kind = .alias,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });
    const nominal = try store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = checked_ty },
        .def = .{ .module_name = module_name, .type_name = type_name },
        .kind = .nominal,
        .args = Span.empty(),
        .backing = .{ .ty = str, .use = .inspectable },
    } });

    const str_digest = store.typeDigest(&name_store, str);
    const alias_digest = store.typeDigest(&name_store, aliased);
    const nominal_digest = store.typeDigest(&name_store, nominal);
    try std.testing.expect(std.mem.eql(u8, str_digest.bytes[0..], alias_digest.bytes[0..]));
    try std.testing.expect(!std.mem.eql(u8, str_digest.bytes[0..], nominal_digest.bytes[0..]));
}
