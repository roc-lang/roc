//! Monomorphic type system for MIR.
//!
//! Monotypes are fully concrete types with no type variables, no extensions,
//! no aliases, and no nominal/opaque/structural distinction. Records, tag unions,
//! and tuples are just records, tag unions, and tuples — the distinctions that
//! existed for static dispatch and module boundary enforcement are no longer needed.
//!
//! Each MIR expression has exactly one Monotype via a 1:1 Expr.Idx → Monotype.Idx mapping.

const std = @import("std");
const base = @import("base");

const Ident = base.Ident;
const Allocator = std.mem.Allocator;

/// Index into the Store's monotypes array.
/// Since MIR has a 1:1 expr-to-type mapping, an Expr.Idx can be directly
/// reinterpreted as a Monotype.Idx.
pub const Idx = enum(u32) {
    _,

    pub const none: Idx = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: Idx) bool {
        return self == none;
    }
};

/// Span of Monotype.Idx values stored in the extra_data array.
pub const Span = extern struct {
    start: u32,
    len: u16,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: Span) bool {
        return self.len == 0;
    }
};

/// A monomorphic type — fully concrete, no type variables, no extensions.
pub const Monotype = union(enum) {
    /// Function type: args -> ret
    func: struct {
        args: Span,
        ret: Idx,
        effectful: bool,
    },

    /// Closed tag union (tags sorted by name)
    tag_union: struct {
        tags: TagSpan,
    },

    /// Closed record (fields sorted by name)
    record: struct {
        fields: FieldSpan,
    },

    /// Tuple
    tuple: struct {
        elems: Span,
    },

    /// List with element type
    list: struct {
        elem: Idx,
    },

    /// Primitive type
    prim: Prim,

    /// Box (heap-allocated wrapper)
    box: struct {
        inner: Idx,
    },

    /// Unit / empty record
    unit: void,

    /// Error type (for error recovery)
    err: void,
};

/// Primitive type kinds.
pub const Prim = enum {
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
};

/// A tag in a tag union.
pub const Tag = struct {
    name: Ident.Idx,
    /// Span of Monotype.Idx for payload types
    payloads: Span,
};

/// Span of Tags stored in the tags array.
pub const TagSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() TagSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: TagSpan) bool {
        return self.len == 0;
    }
};

/// A field in a record.
pub const Field = struct {
    name: Ident.Idx,
    type_idx: Idx,
};

/// Span of Fields stored in the fields array.
pub const FieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() FieldSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: FieldSpan) bool {
        return self.len == 0;
    }
};

/// Flat storage for monomorphic types.
pub const Store = struct {
    monotypes: std.ArrayListUnmanaged(Monotype),
    /// Monotype.Idx values for spans (func args, tuple elems)
    extra_idx: std.ArrayListUnmanaged(u32),
    tags: std.ArrayListUnmanaged(Tag),
    fields: std.ArrayListUnmanaged(Field),

    pub fn init() Store {
        return .{
            .monotypes = .empty,
            .extra_idx = .empty,
            .tags = .empty,
            .fields = .empty,
        };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.monotypes.deinit(allocator);
        self.extra_idx.deinit(allocator);
        self.tags.deinit(allocator);
        self.fields.deinit(allocator);
    }

    pub fn addMonotype(self: *Store, allocator: Allocator, mono: Monotype) !Idx {
        const idx: u32 = @intCast(self.monotypes.items.len);
        try self.monotypes.append(allocator, mono);
        return @enumFromInt(idx);
    }

    pub fn getMonotype(self: *const Store, idx: Idx) Monotype {
        return self.monotypes.items[@intFromEnum(idx)];
    }

    /// Add a span of Monotype.Idx values to extra_idx and return a Span.
    pub fn addIdxSpan(self: *Store, allocator: Allocator, ids: []const Idx) !Span {
        if (ids.len == 0) return Span.empty();
        const start: u32 = @intCast(self.extra_idx.items.len);
        for (ids) |id| {
            try self.extra_idx.append(allocator, @intFromEnum(id));
        }
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Get a slice of Monotype.Idx from a Span.
    pub fn getIdxSpan(self: *const Store, span: Span) []const Idx {
        if (span.len == 0) return &.{};
        const raw = self.extra_idx.items[span.start..][0..span.len];
        return @ptrCast(raw);
    }

    /// Add tags to the tags array and return a TagSpan.
    pub fn addTags(self: *Store, allocator: Allocator, tag_slice: []const Tag) !TagSpan {
        if (tag_slice.len == 0) return TagSpan.empty();
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(allocator, tag_slice);
        return .{ .start = start, .len = @intCast(tag_slice.len) };
    }

    /// Get a slice of Tags from a TagSpan.
    pub fn getTags(self: *const Store, span: TagSpan) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    /// Add fields to the fields array and return a FieldSpan.
    pub fn addFields(self: *Store, allocator: Allocator, field_slice: []const Field) !FieldSpan {
        if (field_slice.len == 0) return FieldSpan.empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(allocator, field_slice);
        return .{ .start = start, .len = @intCast(field_slice.len) };
    }

    /// Get a slice of Fields from a FieldSpan.
    pub fn getFields(self: *const Store, span: FieldSpan) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }
};
