//! Lambda Mono type store.
//!
//! Function types do not exist in this store. Finite function values are
//! generated callable tag unions, and erased function values use the erased
//! callable layout selected by direct LIR lowering.

const std = @import("std");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");

pub const names = check.CheckedNames;

pub const TypeId = enum(u32) { _ };
pub const FnVariantId = enum(u32) { _ };

pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
};

pub const CaptureField = struct {
    symbol: Common.Symbol,
    binder: ?check.CheckedModule.PatternBinderId,
    ty: TypeId,
};

pub const Tag = struct {
    name: names.TagNameId,
    payloads: Span,
};

pub const FnVariant = struct {
    id: FnVariantId,
    lambda: Common.Symbol,
    capture_ty: ?TypeId,
};

pub const Content = union(enum) {
    primitive: MonoType.Primitive,
    named: struct {
        named_type: MonoType.NamedType,
        def: MonoType.TypeDef,
        kind: MonoType.NamedKind,
        args: Span,
        backing: ?struct {
            ty: TypeId,
            use: MonoType.BackingUse,
        } = null,
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
    },
    zst,
};

pub const Store = struct {
    allocator: std.mem.Allocator,
    types: std.ArrayList(Content),
    spans: std.ArrayList(TypeId),
    fields: std.ArrayList(Field),
    capture_fields: std.ArrayList(CaptureField),
    tags: std.ArrayList(Tag),
    fn_variants: std.ArrayList(FnVariant),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .types = .empty,
            .spans = .empty,
            .fields = .empty,
            .capture_fields = .empty,
            .tags = .empty,
            .fn_variants = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
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
        const start: u32 = @intCast(self.spans.items.len);
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptureFields(self: *Store, values: []const CaptureField) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.capture_fields.items.len);
        try self.capture_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnVariants(self: *Store, values: []const FnVariant) std.mem.Allocator.Error!Span {
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

    pub fn fnVariantSpan(self: *const Store, span_: Span) []const FnVariant {
        return self.fn_variants.items[span_.start..][0..span_.len];
    }
};

test "lambda mono type declarations are referenced" {
    std.testing.refAllDecls(@This());
}
