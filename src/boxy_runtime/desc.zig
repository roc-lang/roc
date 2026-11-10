//! The binary format of a Boxy type descriptor.
//!
//! A descriptor fully describes one type's runtime representation and the
//! operations a generic body cannot inline (design.md "Type Descriptors").
//! Descriptors are immutable and immortal. The compiler emits static
//! descriptors as readonly data with relocations for the target's pointer
//! width, and the runtime reads them as these structs at the native width.
//! Every struct here is therefore an `extern struct` whose pointer fields are
//! native pointers, and `ExternLayout` gives the byte offsets of each field
//! for either pointer width so the compiler emits bytes without restating the
//! format.

const std = @import("std");
const builtins = @import("builtins");
const shape = @import("layout_shape");

const SortKey = shape.SortKey;
const RocStr = builtins.str.RocStr;

const pointer_size: u32 = @sizeOf(usize);

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
    /// `methods` holds the type's overrides.
    nominal,
    /// The box a layout inserts to break a recursive type. `child` is the
    /// payload; inspection, equality, and hashing see through it.
    recursive_box,
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
    /// This type is, or stores inline, a type on a recursive size cycle: only
    /// such a type can have recursion boxes among its components.
    recursive: bool = false,
    /// The builtin Iter/Stream record: logical fields are length then step.
    sequence: bool = false,
    _: u2 = 0,
};

/// A field label or tag name, which a names block holds (`Names`). Static
/// and immortal.
///
/// A name's identity is its content (design.md "Name Identity"): the SHA-256
/// digest of a label's or tag's text, or of a nominal declaration's declaring
/// module content identity and declaration. Two names are equal exactly when
/// their ids are, whichever object file or lowering each node came from, so
/// nodes are never interned, numbered, or merged at link time.
pub const Name = extern struct {
    /// The content digest, as four little-endian words.
    id: NameId,
    len: u32,
    /// UTF-8 text, which inspection prints.
    bytes: [*]const u8,

    pub fn text(self: *const Name) []const u8 {
        return self.bytes[0..self.len];
    }
};

/// A name's content digest: 32 bytes as four words, so a comparison is four
/// word compares.
pub const NameId = [4]u64;

/// The `NameId` of the 32-byte digest `digest`.
pub fn nameIdFromDigest(digest: [32]u8) NameId {
    var id: NameId = undefined;
    for (&id, 0..) |*word, index| word.* = std.mem.readInt(u64, digest[index * 8 ..][0..8], .little);
    return id;
}

/// The 32-byte digest of `id`.
pub fn nameIdDigest(id: NameId) [32]u8 {
    var digest: [32]u8 = undefined;
    for (id, 0..) |word, index| std.mem.writeInt(u64, digest[index * 8 ..][0..8], word, .little);
    return digest;
}

/// The domain a label's or tag's id is hashed under: the SHA-256 of this
/// text followed by the name's text.
pub const label_name_domain = "roc.boxy.name.label.v1";

/// The domain a nominal declaration's id is hashed under: the SHA-256 of
/// this text, the declaring module's 32-byte link name, the declaration's
/// little-endian `u32` position among the module's declarations sharing its
/// name, and the declaration's name text.
pub const nominal_name_domain = "roc.boxy.name.nominal.v1";

/// The id of the label or tag `text` (design.md "Name Identity"). The
/// compiler computes the same digest with its own SHA-256; this definition
/// is the one tests and the runtime's own fixtures use.
pub fn labelId(text: []const u8) NameId {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(label_name_domain);
    hasher.update(text);
    return nameIdFromDigest(hasher.finalResult());
}

/// The id of the `same_name_ordinal`th nominal declaration named `text` of
/// the module with link name `module_name` (design.md "Name Identity").
pub fn nominalId(module_name: [32]u8, same_name_ordinal: u32, text: []const u8) NameId {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(nominal_name_domain);
    hasher.update(&module_name);
    var index_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &index_bytes, same_name_ordinal, .little);
    hasher.update(&index_bytes);
    hasher.update(text);
    return nameIdFromDigest(hasher.finalResult());
}

test "a name id round-trips through its digest and tells names apart by content" {
    const a = labelId("a");
    try std.testing.expectEqual(a, nameIdFromDigest(nameIdDigest(a)));
    try std.testing.expectEqual(a, labelId("a"));
    try std.testing.expect(!std.meta.eql(a, labelId("b")));
    // A nominal named like a label is a different name.
    try std.testing.expect(!std.meta.eql(labelId("Point"), nominalId(@splat(1), 0, "Point")));
    // Two declarations of one name in two modules, or at two places in one
    // module, are different names.
    try std.testing.expect(!std.meta.eql(nominalId(@splat(1), 0, "Point"), nominalId(@splat(2), 0, "Point")));
    try std.testing.expect(!std.meta.eql(nominalId(@splat(1), 0, "Point"), nominalId(@splat(1), 1, "Point")));
}

/// An immutable, immortal description of one type's representation. It
/// holds no names: a type's field labels and tag names are in its names block
/// (`Names`), which only code that prints a value reads.
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
    /// A reference-containing record/tuple's fields are immediately followed
    /// by a u32 count and that many u32 memory-slot indices, in ascending
    /// order, naming exactly its non-padding reference-containing fields.
    entries: ?*const anyopaque,
    /// A nominal's overrides, or a SIMD vector's `to_inspect`.
    methods: ?*const Methods,

    pub fn fields(self: *const TypeDesc) []const Field {
        std.debug.assert(self.kind == .record or self.kind == .tuple);
        if (self.count == 0) return &.{};
        const first: [*]const Field = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    /// The exact fields reference counting visits, in memory order. This
    /// trailer shares the field table's allocation; other descriptors and
    /// reference-free records carry no additional storage.
    pub fn refcountedFieldSlots(self: *const TypeDesc) []const u32 {
        std.debug.assert(self.kind == .record or self.kind == .tuple);
        if (!self.flags.contains_refcounted) return &.{};
        const all = self.fields();
        const trailer: [*]const u32 = @ptrCast(@alignCast(all.ptr + all.len));
        const slots = trailer[1..][0..trailer[0]];
        if (@import("builtin").mode == .debug) {
            var next: usize = 0;
            for (all, 0..) |field, index| {
                if (field.is_padding == 0 and field.desc.flags.contains_refcounted) {
                    std.debug.assert(next < slots.len and slots[next] == index);
                    next += 1;
                }
            }
            std.debug.assert(next == slots.len);
        }
        return slots;
    }

    pub fn variants(self: *const TypeDesc) []const Variant {
        std.debug.assert(self.kind == .tag_union);
        if (self.count == 0) return &.{};
        const first: [*]const Variant = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    /// The offset of the record's field `field_index`.
    pub fn fieldOffset(self: *const TypeDesc, field_index: u32) u32 {
        return self.fieldBySemanticIndex(field_index).offset;
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
            else => unreachable, // the compiler only produces 0, 1, 2, 4, or 8
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

// Names blocks.

/// The field labels and tag names of one type, parallel to its descriptor
/// (design.md "Names Blocks"). Only code that reflects over a type reads
/// one: inspection prints a value's labels and tag names from it. A type
/// with no record field or tag in its reach has no names block (null), so a
/// program that prints no value of a type carries none of its names.
pub const Names = extern struct {
    /// Record, tuple: one `NamesEntry` per field, in the descriptor's memory
    /// order. Tag union: one per variant, in discriminant order.
    entries: ?*const anyopaque,
    /// A list element's, box or recursion box payload's, or nominal
    /// backing's names.
    child: ?*const Names,

    pub fn entry(self: *const Names, index: usize) *const NamesEntry {
        const first: [*]const NamesEntry = @ptrCast(@alignCast(self.entries.?));
        return &first[index];
    }

    pub fn layout(width: PointerWidth) ExternLayout(Names) {
        return externLayout(Names, width);
    }
};

/// One field's or variant's names.
pub const NamesEntry = extern struct {
    /// A record field's label or a variant's tag name; null for a tuple
    /// element or padding.
    label: ?*const Name,
    /// The names of the field's type or the variant's payload.
    names: ?*const Names,

    pub fn layout(width: PointerWidth) ExternLayout(NamesEntry) {
        return externLayout(NamesEntry, width);
    }
};

// Coercion plans.

/// What one node of a coercion plan does (`AdaptPlan`).
pub const AdaptKind = enum(u8) {
    /// Copy `to.size` bytes and retain what they reference: the two sides
    /// share one representation.
    copy,
    /// Wrap the value as `#Present` of the presence slot `to`: write
    /// `discriminant`, then convert the payload by `child`.
    wrap,
    /// Unwrap the presence slot `from`, whose `#Present` is `discriminant`
    /// (a `#Missing` value crashes), converting the payload by `child`.
    unwrap,
    /// Write the `#Missing` discriminant `discriminant` of the presence slot
    /// `to` over zeroed bytes: a destination field the source lacks.
    missing,
    /// Re-tag: `entries` is `from.count` `AdaptVariant`s, one per source
    /// discriminant, giving the destination's discriminant under `to` and
    /// the payload's plan. A Bool side is the union `[False, True]`.
    retag,
    /// A record or tuple: `entries` is `count` `AdaptField`s, one per
    /// destination field that is not padding.
    record,
    /// A list: each element converted by `child`, from `from.child` to
    /// `to.child`.
    list,
    /// A box or recursion box: its payload converted by `child`, from
    /// `from.child` to `to.child`.
    box,
    /// The two sides describe different kinds of value; reaching it is a
    /// compiler bug.
    mismatch,
};

/// One node of a coercion plan: how a value `from` describes is rebuilt as
/// the value `to` describes (design.md "Coercion Plans"). The compiler
/// builds a plan for each pair of descriptors a coercion converts between,
/// so the conversion follows indices and offsets only.
pub const AdaptPlan = extern struct {
    kind: AdaptKind,
    /// The number of `entries`.
    count: u32,
    /// `wrap`, `unwrap`, `missing`: the presence slot's discriminant.
    discriminant: u32,
    from: ?*const TypeDesc,
    to: ?*const TypeDesc,
    child: ?*const AdaptPlan,
    /// `[*]const AdaptField` (record) or `[*]const AdaptVariant` (retag).
    entries: ?*const anyopaque,

    pub fn fields(self: *const AdaptPlan) []const AdaptField {
        std.debug.assert(self.kind == .record);
        if (self.count == 0) return &.{};
        const first: [*]const AdaptField = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    pub fn variants(self: *const AdaptPlan) []const AdaptVariant {
        std.debug.assert(self.kind == .retag);
        if (self.count == 0) return &.{};
        const first: [*]const AdaptVariant = @ptrCast(@alignCast(self.entries.?));
        return first[0..self.count];
    }

    pub fn layout(width: PointerWidth) ExternLayout(AdaptPlan) {
        return externLayout(AdaptPlan, width);
    }
};

/// One destination field of a record plan: the bytes at `from_offset` in
/// the source become those at `to_offset` in the destination through
/// `plan`.
pub const AdaptField = extern struct {
    to_offset: u32,
    from_offset: u32,
    plan: *const AdaptPlan,

    pub fn layout(width: PointerWidth) ExternLayout(AdaptField) {
        return externLayout(AdaptField, width);
    }
};

/// The destination of one source variant of a re-tag plan: `discriminant`
/// under the destination's union, its payload converted by `plan` (null for
/// a variant without arguments). `absent` marks a source variant the
/// destination's row lacks, which no value reaching the conversion holds.
pub const AdaptVariant = extern struct {
    discriminant: u32,
    plan: ?*const AdaptPlan,

    pub const absent: u32 = std.math.maxInt(u32);

    pub fn layout(width: PointerWidth) ExternLayout(AdaptVariant) {
        return externLayout(AdaptVariant, width);
    }
};

/// A record update plan (`DynOp.record_update`) is one `u32` per memory slot
/// of the result's record descriptor: for a field the update sets, this
/// flag and the position of its value among the update's values; for any
/// other field, the memory slot of the base's field it copies. A padding
/// slot's entry is unread.
pub const update_value_flag: u32 = 1 << 31;

// Leaf descriptors.

/// A descriptor of no components: its kind, scalar, shape, and whether its
/// value holds a reference count.
pub fn leaf(kind: Kind, scalar: Scalar, size: u32, alignment: u32, sort_key: SortKey, contains_refcounted: bool) TypeDesc {
    return .{
        .size = size,
        .alignment = alignment,
        .kind = kind,
        .scalar = scalar,
        .sort_key = @backingInt(sort_key),
        .flags = .{ .contains_refcounted = contains_refcounted },
        .count = 0,
        .discriminant_offset = 0,
        .discriminant_size = 0,
        .child = null,
        .entries = null,
        .methods = null,
    };
}

const zst_leaf: TypeDesc = leaf(.zst, .u8, 0, 1, .align_1, false);
const str_leaf: TypeDesc = leaf(.str, .u8, RocStr.word_count * pointer_size, pointer_size, .pointer, true);
const erased_callable_leaf: TypeDesc = leaf(.erased_callable, .u8, pointer_size, pointer_size, .pointer, true);
const bool_leaf: TypeDesc = blk: {
    var bool_desc = leaf(.bool, .u8, 1, 1, .align_1, false);
    bool_desc.count = 2;
    bool_desc.discriminant_size = 1;
    break :blk bool_desc;
};

fn scalarSize(scalar: Scalar) u32 {
    return switch (scalar) {
        .u8, .i8 => 1,
        .u16, .i16 => 2,
        .u32, .i32, .f32 => 4,
        .u64, .i64, .f64 => 8,
        .u128, .i128, .dec => 16,
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => 16,
        .opaque_ptr => pointer_size,
    };
}

const scalar_leaves = blk: {
    const scalars = @typeInfo(Scalar).@"enum".field_names;
    var leaves: [scalars.len]TypeDesc = undefined;
    for (&leaves, 0..) |*slot, index| {
        const scalar: Scalar = @fromBackingInt(index);
        const size = scalarSize(scalar);
        const sort_key: SortKey = if (scalar == .opaque_ptr) .pointer else SortKey.fromAlignBytes(size);
        slot.* = leaf(.scalar, scalar, size, size, sort_key, false);
    }
    break :blk leaves;
};

/// The zero-sized descriptor: the empty record, the empty tuple, a
/// payload-free variant's payload.
pub fn zstDesc() *const TypeDesc {
    return &zst_leaf;
}

/// The descriptor of `Str`.
pub fn strDesc() *const TypeDesc {
    return &str_leaf;
}

/// The descriptor of `Bool`.
pub fn boolDesc() *const TypeDesc {
    return &bool_leaf;
}

/// The descriptor every function value shares.
pub fn erasedCallableDesc() *const TypeDesc {
    return &erased_callable_leaf;
}

/// A scalar's descriptor. A SIMD vector's carries no `to_inspect`; the
/// compiler emits vector descriptors with theirs.
pub fn scalarDesc(scalar: Scalar) *const TypeDesc {
    return &scalar_leaves[@backingInt(scalar)];
}

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

/// The size of one field of an extern struct at `width`: pointers, `usize` and
/// `isize` take the target's width, an array its element count times its
/// element's size, and every other field its own fixed size. A field whose
/// size this cannot know at another width (a nested extern struct or union,
/// or a C type such as `c_long`) is a compile error; packed structs such as
/// `Flags` have a fixed size and are allowed.
fn fieldSize(comptime F: type, width: PointerWidth) u32 {
    if (F == usize or F == isize) return width.bytes();
    if (F == c_long or F == c_ulong or F == c_longdouble) @compileError("extern struct fields cannot be " ++ @typeName(F) ++ ", whose size depends on the target");
    return switch (@typeInfo(F)) {
        .pointer => width.bytes(),
        .optional => |optional| switch (@typeInfo(optional.child)) {
            .pointer => width.bytes(),
            .type, .void, .bool, .noreturn, .int, .float, .array, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .optional, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @compileError("extern struct fields are optional only when they are pointers"),
        },
        .array => |array| @as(u32, @intCast(array.len)) * fieldSize(array.child, width),
        .@"struct" => |info| switch (info.layout) {
            .@"packed" => @sizeOf(F),
            .auto, .@"extern" => @compileError("extern struct fields cannot be a nested struct (" ++ @typeName(F) ++ "); its layout at another width is not known here"),
        },
        .@"union" => |info| switch (info.layout) {
            .@"packed" => @sizeOf(F),
            .auto, .@"extern" => @compileError("extern struct fields cannot be a union (" ++ @typeName(F) ++ "); its layout at another width is not known here"),
        },
        .type, .void, .bool, .noreturn, .int, .float, .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"enum", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => @sizeOf(F),
    };
}

/// The alignment of one field of an extern struct at `width`: an array
/// aligns as its element does; every other field is a pointer or a
/// power-of-two-sized scalar, whose alignment is its size. So an 8-byte
/// scalar aligns to 8 at both widths, as on arm32 and wasm32.
fn fieldAlignment(comptime F: type, width: PointerWidth) u32 {
    return switch (@typeInfo(F)) {
        .array => |array| fieldAlignment(array.child, width),
        .pointer, .optional, .type, .void, .bool, .noreturn, .int, .float, .@"struct", .comptime_float, .comptime_int, .undefined, .null, .error_union, .error_set, .@"enum", .@"union", .@"fn", .@"opaque", .frame, .@"anyframe", .vector, .enum_literal, .spirv => fieldSize(F, width),
    };
}

/// The offsets and size of the extern struct `T` at pointer width `width`.
/// Descriptors use it, and so does any other struct a backend shares with a
/// runtime built for that width.
pub fn externLayout(comptime T: type, width: PointerWidth) ExternLayout(T) {
    const info = @typeInfo(T).@"struct";
    comptime std.debug.assert(info.layout == .@"extern");
    var result: ExternLayout(T) = undefined;
    var end: u32 = 0;
    var max_alignment: u32 = 1;
    inline for (info.field_names, info.field_types, 0..) |_, F, index| {
        const size = fieldSize(F, width);
        const alignment = fieldAlignment(F, width);
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
    try expectNativeLayout(AdaptPlan);
    try expectNativeLayout(AdaptField);
    try expectNativeLayout(AdaptVariant);
    try expectNativeLayout(Names);
    try expectNativeLayout(NamesEntry);
}

test "descriptor layouts at both pointer widths" {
    const t32 = TypeDesc.layout(.u32);
    const t64 = TypeDesc.layout(.u64);
    try std.testing.expectEqual(@as(u32, 24), t32.of(.child));
    try std.testing.expectEqual(@as(u32, 32), t32.of(.methods));
    try std.testing.expectEqual(@as(u32, 36), t32.size);
    try std.testing.expectEqual(@as(u32, 24), t64.of(.child));
    try std.testing.expectEqual(@as(u32, 40), t64.of(.methods));
    try std.testing.expectEqual(@as(u32, 48), t64.size);
    try std.testing.expectEqual(@as(u32, 20), Field.layout(.u32).size);
    try std.testing.expectEqual(@as(u32, 24), Field.layout(.u64).size);
    try std.testing.expectEqual(@as(u32, 12), Variant.layout(.u32).size);
    try std.testing.expectEqual(@as(u32, 16), Variant.layout(.u64).size);
}

test "usize and isize fields take the pointer width" {
    const S = extern struct { flag: u32, len: usize, words: [2]usize, delta: isize };
    const s32 = externLayout(S, .u32);
    try std.testing.expectEqual(@as(u32, 4), s32.of(.len));
    try std.testing.expectEqual(@as(u32, 8), s32.of(.words));
    try std.testing.expectEqual(@as(u32, 16), s32.of(.delta));
    try std.testing.expectEqual(@as(u32, 20), s32.size);
    const s64 = externLayout(S, .u64);
    try std.testing.expectEqual(@as(u32, 8), s64.of(.len));
    try std.testing.expectEqual(@as(u32, 16), s64.of(.words));
    try std.testing.expectEqual(@as(u32, 32), s64.of(.delta));
    try std.testing.expectEqual(@as(u32, 40), s64.size);
    try expectNativeLayout(S);
}
