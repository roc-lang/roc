//! The binary format of a Boxy type descriptor.
//!
//! A descriptor fully describes one type's runtime representation and the
//! operations a generic body cannot inline (design.md "Type Descriptors").
//! Descriptors are immutable and immortal. The compiler emits static
//! descriptors as readonly data with relocations for the target's pointer
//! width; the runtime composes descriptors in the same format at the native
//! width. Every struct here is therefore an `extern struct` whose pointer
//! fields are native pointers, and `ExternLayout` gives the byte offsets of
//! each field for either pointer width so the compiler emits bytes without
//! restating the format.

const std = @import("std");

/// What a descriptor describes, and so which of its fields are meaningful.
pub const Kind = enum(u8) {
    /// Zero-sized and not otherwise structured (the empty record, the empty
    /// tuple, a payload-free variant).
    zst,
    /// A number, a SIMD vector, or an opaque pointer; `scalar` says which.
    scalar,
    str,
    /// Bool: a one-byte discriminant, False = 0 and True = 1.
    bool,
    /// `child` is the element descriptor.
    list,
    /// `child` is the payload descriptor.
    box,
    /// `entries` is `count` `Field`s in memory order.
    record,
    /// `entries` is `count` `Field`s in memory order, each unnamed.
    tuple,
    /// `entries` is `count` `Variant`s in discriminant order.
    tag_union,
    /// A function value: the data pointer of a boxed erased callable.
    erased_callable,
    /// `child` is the backing descriptor, whose layout this one shares;
    /// `methods` holds the type's overrides and `name` its name.
    nominal,
    /// `RocBox(RocUnknown)`: only retained, moved, or shallow-dropped.
    opaque_unknown,
    /// The box a layout inserts to break a recursive type. `child` is the
    /// payload; inspection, equality, and hashing see through it.
    recursive_box,
};

/// A numeric literal's value under every builtin number type, for a literal
/// whose type a generic body knows only by its descriptor: a `u32` mask (bit
/// `s` set when the literal fits `Scalar` `s`) padded to 16 bytes, then one
/// 16-byte little-endian slot per number `Scalar` (`u8` through `dec`).
pub const NumeralTable = struct {
    pub const number_count = 13;
    pub const header_size = 16;
    pub const slot_size = 16;
    pub const size = header_size + number_count * slot_size;
    pub const alignment = 16;

    pub fn slotOffset(scalar: Scalar) u32 {
        return header_size + @as(u32, @backingInt(scalar)) * slot_size;
    }
};

/// Which scalar a `.scalar` descriptor is.
pub const Scalar = enum(u8) {
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
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,
    opaque_ptr,

    /// Whether this is a 128-bit SIMD vector.
    pub fn isVector(self: Scalar) bool {
        return switch (self) {
            .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => true,
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .opaque_ptr => false,
        };
    }
};

/// Facts about a type that walks read without descending into it.
pub const Flags = packed struct(u8) {
    /// Some value of this type directly or transitively holds a refcounted
    /// allocation: a `Str`, a `List`, a non-zero-sized `Box`, or a function.
    contains_refcounted: bool = false,
    /// A nominal that hides its structure: it inspects as `<opaque>` unless it
    /// has a `to_inspect` override.
    inspect_opaque: bool = false,
    /// A record or tuple laid out in declared order: a nominal record with
    /// `_` padding fields, or an erased-call argument record.
    declared_order: bool = false,
    /// A tag union that is an optional record field's presence slot
    /// (`[#Missing, #Present(payload)]`, Missing first): it inspects as its
    /// payload, or as `<missing>`.
    presence_slot: bool = false,
    _: u4 = 0,
};

/// A field label or tag name. Static and immortal.
pub const Name = extern struct {
    /// Program-wide interned id: two names are equal exactly when their ids
    /// are.
    id: u32,
    len: u32,
    /// UTF-8 text, used for ordering and inspection.
    bytes: [*]const u8,

    pub fn text(self: *const Name) []const u8 {
        return self.bytes[0..self.len];
    }
};

/// An immutable, immortal description of one type's representation.
pub const TypeDesc = extern struct {
    size: u32,
    /// In bytes.
    alignment: u32,
    kind: Kind,
    /// Meaningful only for `.scalar`.
    scalar: Scalar,
    /// The layout `SortKey`'s backing integer.
    sort_key: u8,
    flags: Flags,
    /// The number of fields (record, tuple) or variants (tag union).
    count: u32,
    /// Tag union only.
    discriminant_offset: u32,
    /// Tag union only: 0, 1, 2, 4, or 8 (`shape.discriminantSize`).
    discriminant_size: u32,
    /// List element, box payload, nominal backing, or recursive box payload.
    child: ?*const TypeDesc,
    /// `[*]const Field` (record, tuple) or `[*]const Variant` (tag union).
    entries: ?*const anyopaque,
    /// A nominal's name.
    name: ?*const Name,
    /// A nominal's overrides, or a SIMD vector's `to_inspect`.
    methods: ?*const Methods,

    pub fn fields(self: *const TypeDesc) []const Field {
        std.debug.assert(self.kind == .record or self.kind == .tuple);
        if (self.count == 0) return &.{};
        const first: [*]const Field = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    pub fn variants(self: *const TypeDesc) []const Variant {
        std.debug.assert(self.kind == .tag_union);
        if (self.count == 0) return &.{};
        const first: [*]const Variant = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    /// The field whose semantic index is `field_index`.
    pub fn fieldBySemanticIndex(self: *const TypeDesc, field_index: u32) *const Field {
        const all = self.fields();
        return &all[all[field_index].field_slot];
    }

    pub fn containsRefcounted(self: *const TypeDesc) bool {
        return self.flags.contains_refcounted;
    }

    /// Read a tag union value's discriminant.
    pub fn readDiscriminant(self: *const TypeDesc, value: [*]const u8) u32 {
        std.debug.assert(self.kind == .tag_union or self.kind == .bool);
        const at = value + self.discriminant_offset;
        return switch (self.discriminant_size) {
            0 => 0,
            1 => at[0],
            2 => std.mem.readInt(u16, at[0..2], .little),
            4 => std.mem.readInt(u32, at[0..4], .little),
            8 => @intCast(std.mem.readInt(u64, at[0..8], .little)),
            else => unreachable, // composition and the compiler only produce 0, 1, 2, 4, or 8
        };
    }

    pub fn layout(width: PointerWidth) ExternLayout(TypeDesc) {
        return externLayout(TypeDesc, width);
    }
};

/// One field of a record or tuple descriptor, in memory order.
pub const Field = extern struct {
    /// A record field's alphabetical rank among the record's named fields; a
    /// tuple element's index; for a padding field, a rank past every named
    /// field, in declared order.
    field_index: u32,
    offset: u32,
    /// 0 or 1.
    is_padding: u32,
    /// The memory slot (index into the record's fields) of the field whose
    /// `field_index` equals this entry's own index, so walks in semantic
    /// order need no search.
    field_slot: u32,
    /// Null for tuple elements and padding.
    name: ?*const Name,
    desc: *const TypeDesc,

    pub fn layout(width: PointerWidth) ExternLayout(Field) {
        return externLayout(Field, width);
    }
};

/// One variant of a tag union descriptor.
pub const Variant = extern struct {
    discriminant: u32,
    /// The number of tag arguments.
    payload_count: u32,
    name: *const Name,
    /// No arguments: a zero-sized descriptor. One: that argument's
    /// descriptor. More: a `.tuple` descriptor whose element `i` is argument
    /// `i`, laid out structurally.
    payload: *const TypeDesc,

    pub fn layout(width: PointerWidth) ExternLayout(Variant) {
        return externLayout(Variant, width);
    }
};

/// A nominal's method overrides. Each is the data pointer of a boxed erased
/// callable (`builtins.erased_callable`) or null. Every argument record is
/// laid out in declared order at natural alignment.
pub const Methods = extern struct {
    /// `(a, a) -> Bool`.
    is_eq: ?[*]u8,
    /// `(a, Hasher) -> Hasher`.
    to_hash: ?[*]u8,
    /// `a -> Str`.
    to_inspect: ?[*]u8,

    pub fn layout(width: PointerWidth) ExternLayout(Methods) {
        return externLayout(Methods, width);
    }
};

/// A target pointer width.
pub const PointerWidth = enum {
    u32,
    u64,

    pub fn bytes(self: PointerWidth) u32 {
        return switch (self) {
            .u32 => 4,
            .u64 => 8,
        };
    }

    pub const native: PointerWidth = switch (@sizeOf(usize)) {
        4 => .u32,
        8 => .u64,
        else => @compileError("Boxy descriptors exist only for 32-bit and 64-bit targets"),
    };
};

/// The C layout of an extern struct `T` at one pointer width.
pub fn ExternLayout(comptime T: type) type {
    return struct {
        const field_names = @typeInfo(T).@"struct".field_names;

        /// Byte offset of each field, in declaration order.
        offsets: [field_names.len]u32,
        size: u32,
        alignment: u32,

        pub fn of(self: @This(), comptime field: std.meta.FieldEnum(T)) u32 {
            return self.offsets[@backingInt(field)];
        }
    };
}

/// The size of one field of a descriptor struct at `width`: pointers take the
/// target's width, every other field its own fixed size.
fn fieldSize(comptime F: type, width: PointerWidth) u32 {
    return switch (@typeInfo(F)) {
        .pointer => width.bytes(),
        .optional => |optional| switch (@typeInfo(optional.child)) {
            .pointer => width.bytes(),
            .type, .void, .bool, .noreturn, .int, .float, .array, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .optional, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("descriptor fields are optional only when they are pointers"),
        },
        .type, .void, .bool, .noreturn, .int, .float, .array, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @sizeOf(F),
    };
}

/// The offsets and size of the extern struct `T` at pointer width `width`.
pub fn externLayout(comptime T: type, width: PointerWidth) ExternLayout(T) {
    const info = @typeInfo(T).@"struct";
    comptime std.debug.assert(info.layout == .@"extern");
    var result: ExternLayout(T) = undefined;
    var end: u32 = 0;
    var max_alignment: u32 = 1;
    inline for (info.field_names, info.field_types, 0..) |_, F, index| {
        const size = fieldSize(F, width);
        // Every descriptor field is a pointer or a power-of-two-sized scalar
        // no wider than a pointer, so its alignment is its size.
        const alignment = size;
        end = std.mem.alignForward(u32, end, alignment);
        result.offsets[index] = end;
        end += size;
        max_alignment = @max(max_alignment, alignment);
    }
    result.size = std.mem.alignForward(u32, end, max_alignment);
    result.alignment = max_alignment;
    return result;
}

fn expectNativeLayout(comptime T: type) error{TestExpectedEqual}!void {
    const native = externLayout(T, .native);
    inline for (@typeInfo(T).@"struct".field_names, 0..) |name, index| {
        try std.testing.expectEqual(@as(u32, @offsetOf(T, name)), native.offsets[index]);
    }
    try std.testing.expectEqual(@as(u32, @sizeOf(T)), native.size);
    try std.testing.expectEqual(@as(u32, @alignOf(T)), native.alignment);
}

test "descriptor layouts match the native extern layout" {
    try expectNativeLayout(TypeDesc);
    try expectNativeLayout(Field);
    try expectNativeLayout(Variant);
    try expectNativeLayout(Methods);
    try expectNativeLayout(Name);
}

test "descriptor layouts at both pointer widths" {
    const t32 = TypeDesc.layout(.u32);
    const t64 = TypeDesc.layout(.u64);
    try std.testing.expectEqual(@as(u32, 24), t32.of(.child));
    try std.testing.expectEqual(@as(u32, 36), t32.of(.methods));
    try std.testing.expectEqual(@as(u32, 40), t32.size);
    try std.testing.expectEqual(@as(u32, 24), t64.of(.child));
    try std.testing.expectEqual(@as(u32, 48), t64.of(.methods));
    try std.testing.expectEqual(@as(u32, 56), t64.size);
    try std.testing.expectEqual(@as(u32, 24), Field.layout(.u32).size);
    try std.testing.expectEqual(@as(u32, 32), Field.layout(.u64).size);
    try std.testing.expectEqual(@as(u32, 16), Variant.layout(.u32).size);
    try std.testing.expectEqual(@as(u32, 24), Variant.layout(.u64).size);
}
