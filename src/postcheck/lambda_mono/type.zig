//! Lambda Mono type store.
//!
//! Function types do not exist in this store. Finite function values are
//! generated callable tag unions, and erased function values use the erased
//! callable layout selected by direct LIR lowering.

const std = @import("std");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");

/// Checked boundary name module used by Lambda Mono types.
pub const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;

/// Identifier for a Lambda Mono type in this store.
pub const TypeId = enum(u32) { _ };
/// Identifier for a Lambda Mono function body.
pub const FnId = enum(u32) { _ };
/// Identifier for a callable variant in this store.
pub const FnVariantId = enum(u32) { _ };

/// Slice descriptor for type, field, tag, or callable-variant arrays.
pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

/// Record field type entry.
pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
};

/// Capture record field entry.
pub const CaptureField = struct {
    symbol: Common.Symbol,
    binder: ?check.CheckedModule.PatternBinderId,
    capture_id: ?check.CheckedModule.CaptureId = null,
    ty: TypeId,
};

/// Tag-union variant type entry.
pub const Tag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: Span,
};

/// Callable variant entry.
pub const FnVariant = struct {
    id: FnVariantId,
    source: Common.Symbol,
    target: FnId,
    capture_ty: ?TypeId,
};

/// One entry of a nominal record's declared layout order. Mirrors
/// `MonoType.DeclaredField`; consumed only by layout selection.
pub const DeclaredField = union(enum) {
    named: names.RecordFieldNameId,
    padding: TypeId,
};

/// Lambda Mono type content.
pub const Content = union(enum) {
    primitive: MonoType.Primitive,
    named: struct {
        named_type: MonoType.NamedType,
        def: MonoType.TypeDef,
        kind: MonoType.NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: Span,
        backing: ?struct {
            ty: TypeId,
            use: MonoType.BackingUse,
        } = null,
        /// Declared field order for a nominal/opaque record backing; empty
        /// otherwise.
        declared_order: Span = Span.empty(),
    },
    record: Span,
    capture_record: Span,
    tuple: Span,
    tag_union: Span,
    callable: Span,
    list: TypeId,
    box: TypeId,
    erased_fn: struct {
        source_fn_ty: names.TypeDigest,
        members: Span = .empty(),
    },
    erased_capture_ptr,
    zst,
};

/// Store for Lambda Mono types and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    spans: std.ArrayList(TypeId),
    fields: std.ArrayList(Field),
    capture_fields: std.ArrayList(CaptureField),
    tags: std.ArrayList(Tag),
    fn_variants: std.ArrayList(FnVariant),
    declared_fields: std.ArrayList(DeclaredField),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .spans = .empty,
            .fields = .empty,
            .capture_fields = .empty,
            .tags = .empty,
            .fn_variants = .empty,
            .declared_fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.declared_fields.deinit(self.allocator);
        self.fn_variants.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.capture_fields.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const id: TypeId = @enumFromInt(@as(u32, @intCast(self.types.items.len)));
        try self.types.append(self.allocator, content);
        return id;
    }

    pub fn set(self: *Store, id: TypeId, content: Content) void {
        self.types.items[@intFromEnum(id)] = content;
    }

    pub fn get(self: *const Store, id: TypeId) Content {
        return self.types.items[@intFromEnum(id)];
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

    pub fn addCaptureFields(self: *Store, values: []const CaptureField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.capture_fields.items.len);
        try self.capture_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnVariants(self: *Store, values: []const FnVariant) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fn_variants.items.len);
        for (values, 0..) |variant, i| {
            var stored = variant;
            stored.id = @enumFromInt(@as(u32, @intCast(start + i)));
            try self.fn_variants.append(self.allocator, stored);
        }
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn span(self: *const Store, span_: Span) []const TypeId {
        return self.spans.items[span_.start..][0..span_.len];
    }

    pub fn fieldSpan(self: *const Store, span_: Span) []const Field {
        return self.fields.items[span_.start..][0..span_.len];
    }

    pub fn captureFieldSpan(self: *const Store, span_: Span) []const CaptureField {
        return self.capture_fields.items[span_.start..][0..span_.len];
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

    pub fn fnVariantSpan(self: *const Store, span_: Span) []const FnVariant {
        return self.fn_variants.items[span_.start..][0..span_.len];
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
            .capture_record => |fields| {
                writeBytes(hasher, "capture_record");
                const field_slice = self.captureFieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (field_slice) |field| {
                    writeU32(hasher, @intFromEnum(field.symbol));
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
            .callable => |variants| {
                writeBytes(hasher, "callable");
                const variant_slice = self.fnVariantSpan(variants);
                writeU32(hasher, @intCast(variant_slice.len));
                for (variant_slice) |variant| {
                    writeU32(hasher, @intFromEnum(variant.source));
                    writeU32(hasher, @intFromEnum(variant.target));
                    if (variant.capture_ty) |capture_ty| {
                        writeBytes(hasher, "capture");
                        self.writeTypeDigest(name_store, hasher, capture_ty);
                    } else {
                        writeBytes(hasher, "no_capture");
                    }
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
            .erased_fn => |erased| {
                writeBytes(hasher, "erased_fn");
                hasher.update(&erased.source_fn_ty.bytes);
                const variant_slice = self.fnVariantSpan(erased.members);
                writeU32(hasher, @intCast(variant_slice.len));
                for (variant_slice) |variant| {
                    writeU32(hasher, @intFromEnum(variant.source));
                    writeU32(hasher, @intFromEnum(variant.target));
                    if (variant.capture_ty) |capture_ty| {
                        writeBytes(hasher, "capture");
                        self.writeTypeDigest(name_store, hasher, capture_ty);
                    } else {
                        writeBytes(hasher, "no_capture");
                    }
                }
            },
            .erased_capture_ptr => writeBytes(hasher, "erased_capture_ptr"),
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
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

test "lambda mono type declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "lambda mono type content has callable shapes instead of function types" {
    try std.testing.expect(@hasField(Content, "callable"));
    try std.testing.expect(@hasField(Content, "erased_fn"));
    try std.testing.expect(!@hasField(Content, "func"));
}

test "lambda mono callable variants receive store-local ids" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const capture_ty = try store.add(.zst);
    const variants = try store.addFnVariants(&.{
        .{ .id = @enumFromInt(99), .source = @enumFromInt(7), .target = @enumFromInt(70), .capture_ty = capture_ty },
        .{ .id = @enumFromInt(99), .source = @enumFromInt(8), .target = @enumFromInt(80), .capture_ty = null },
    });
    const callable = try store.add(.{ .callable = variants });

    const stored_variants = store.fnVariantSpan(store.get(callable).callable);
    try std.testing.expectEqual(@as(FnVariantId, @enumFromInt(variants.start)), stored_variants[0].id);
    try std.testing.expectEqual(@as(FnVariantId, @enumFromInt(1)), stored_variants[1].id);
    try std.testing.expectEqual(capture_ty, stored_variants[0].capture_ty.?);
}

test "lambda mono empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit }});
    const nonempty_capture_fields = try store.addCaptureFields(&.{.{ .symbol = @enumFromInt(2), .binder = null, .ty = unit }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @enumFromInt(3), .checked_name = @enumFromInt(3), .payloads = nonempty_span }});
    const nonempty_variants = try store.addFnVariants(&.{.{ .id = @enumFromInt(99), .source = @enumFromInt(4), .target = @enumFromInt(40), .capture_ty = unit }});
    try std.testing.expect(nonempty_span.len == 1);
    try std.testing.expect(nonempty_fields.len == 1);
    try std.testing.expect(nonempty_capture_fields.len == 1);
    try std.testing.expect(nonempty_tags.len == 1);
    try std.testing.expect(nonempty_variants.len == 1);

    try std.testing.expectEqual(Span.empty(), try store.addSpan(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addCaptureFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addTags(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFnVariants(&.{}));
}
