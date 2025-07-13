//! Memory layout representations for values in running Roc programs.
//!
//! See the Layout Store for how these representations actually get created
//! (using type and target information from previous steps in compilation).

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const target = @import("../base/target.zig");

/// Tag for Layout variants
pub const LayoutTag = enum(u4) {
    scalar,
    box,
    box_of_zst, // Box of a zero-sized type, e.g. Box({}) - needs a special-cased runtime implementation
    list,
    list_of_zst, // List of zero-sized types, e.g. List({}) - needs a special-cased runtime implementation
    record,
    tuple,
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 32;

/// Tag for scalar variants
///
/// The exact numbers here are important, because we use them to convert between
/// Scalar and Idx using branchless arithmetic instructions. Don't change them
/// lightly, and make sure to re-run tests if you do!
pub const ScalarTag = enum(u3) {
    opaque_ptr = 0, // Maps to Idx 0
    bool = 1, // Maps to Idx 1
    str = 2, // Maps to Idx 2
    int = 3, // Maps to Idx 3-12 (depending on precision)
    frac = 4, // Maps to Idx 13-15 (depending on precision)
};

/// The union portion of the Scalar packed tagged union.
///
/// Some scalars have extra information associated with them,
/// such as the precision of a particular int or frac. This union
/// stores that extra information.
pub const ScalarUnion = packed union {
    opaque_ptr: void,
    bool: void,
    str: void,
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
};

/// A scalar value such as a bool, str, int, frac, or opaque pointer type.
pub const Scalar = packed struct {
    // This can't be a normal Zig tagged union because it uses a packed union to reduce memory use,
    // and Zig tagged unions don't support being packed.
    data: ScalarUnion,
    tag: ScalarTag,
};

/// Index into a Layout Store
pub const Idx = enum(@Type(.{
    .int = .{
        .signedness = .unsigned,
        // Some Layout variants are just the Tag followed by Idx, so use as many
        // bits as we can spare from the Layout for Idx.
        .bits = layout_bit_size - @bitSizeOf(LayoutTag),
    },
})) {
    // Sentinel values for scalar builtin layouts. When we init the layout store, it automatically
    // adds entries for each of these at an index equal to the enum's value. That way, if you
    // look up one of these in the store, it's always returns the correct layout, and we can have
    // any type that resolves to one of these layouts use one of these hardcoded ones instead
    // of adding redundant layouts to the store.
    //
    // The layout store's idxFromScalar method relies on these exact numbers being what they are now,
    // so be careful when changing them! (Changing them will, at a minimum, cause tests to fail.)
    bool = 0,
    str = 1,
    opaque_ptr = 2,

    // ints
    u8 = 3,
    i8 = 4,
    u16 = 5,
    i16 = 6,
    u32 = 7,
    i32 = 8,
    u64 = 9,
    i64 = 10,
    u128 = 11,
    i128 = 12,

    // fracs
    f32 = 13,
    f64 = 14,
    dec = 15,

    // Regular indices start from here.
    // num_scalars in store.zig must refer to how many variants we had up to this point.
    _,
};

/// The union portion of the Layout packed tagged union (the tag being LayoutTag).
///
/// The largest variant must fit in 28 bits to leave room for the u4 tag
pub const LayoutUnion = packed union {
    scalar: Scalar,
    box: Idx,
    box_of_zst: void,
    list: Idx,
    list_of_zst: void,
    record: RecordLayout,
    tuple: TupleLayout,
};

/// Record field layout
pub const RecordField = struct {
    /// The interned string name of the field
    name: Ident.Idx,
    /// The layout of the field's value
    layout: Idx,

    /// A SafeMultiList for storing record fields
    pub const SafeMultiList = collections.SafeMultiList(RecordField);
};

/// Record layout - stores alignment and index to full data in Store
pub const RecordLayout = packed struct {
    /// Alignment of the record
    alignment: std.mem.Alignment,
    /// Index into the Store's record data
    idx: RecordIdx,
};

/// Index into the Store's record data
pub const RecordIdx = packed struct {
    int_idx: @Type(.{
        .int = .{
            .signedness = .unsigned,
            // We need to be able to fit this in a Layout along with the alignment field in the RecordLayout.
            .bits = layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(std.mem.Alignment),
        },
    }),
};

/// Record data stored in the layout Store
pub const RecordData = struct {
    /// Size of the record, in bytes
    size: u32,
    /// Range of fields in the record_fields list
    fields: collections.NonEmptyRange,

    pub fn getFields(self: RecordData) RecordField.SafeMultiList.Range {
        return self.fields.toRange(RecordField.SafeMultiList.Idx);
    }
};

/// Tuple field layout
pub const TupleField = struct {
    /// The index of the field in the original tuple (e.g. 0 would be the first element in the tuple)
    index: u16,
    /// The layout of the field's value
    layout: Idx,

    /// A SafeMultiList for storing tuple fields
    pub const SafeMultiList = collections.SafeMultiList(TupleField);
};

/// Tuple field layout type alias for compatibility
pub const TupleFieldLayout = TupleField;

/// Tuple layout - stores alignment and index to full data in Store
pub const TupleLayout = packed struct {
    /// Alignment of the tuple
    alignment: std.mem.Alignment,
    /// Index into the Store's tuple data
    idx: TupleIdx,
};

/// Index into the Store's tuple data
pub const TupleIdx = packed struct {
    int_idx: @Type(.{
        .int = .{
            .signedness = .unsigned,
            // We need to be able to fit this in a Layout along with the alignment field in the TupleLayout.
            .bits = layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(std.mem.Alignment),
        },
    }),
};

/// Tuple data stored in the layout Store
pub const TupleData = struct {
    /// Size of the tuple, in bytes
    size: u32,
    /// Range of fields in the tuple_fields list
    fields: collections.NonEmptyRange,

    pub fn getFields(self: TupleData) TupleField.SafeMultiList.Range {
        return self.fields.toRange(TupleField.SafeMultiList.Idx);
    }
};

/// Roc's version of alignment that is limited to a max alignment of 16B to save bits.
pub const RocAlignment = enum(u3) {
    @"1" = 0,
    @"2" = 1,
    @"4" = 2,
    @"8" = 3,
    @"16" = 4,
    _,

    pub fn toByteUnits(a: RocAlignment) usize {
        return @as(usize, 1) << @intFromEnum(a);
    }

    pub fn fromByteUnits(n: u16) RocAlignment {
        std.debug.assert(std.math.isPowerOfTwo(n));
        return @enumFromInt(@ctz(n));
    }
};

/// Size and alignment information
pub const SizeAlign = packed struct(u32) {
    size: u29, // u29 can represent sizes up to ~1GiB (is 1 byte shy of it).
    alignment: RocAlignment, // u3 bits

    /// Box size and alignment (pointer-sized)
    pub const box = SizeAlign{
        .size = @sizeOf(usize),
        .alignment = RocAlignment.fromByteUnits(@alignOf(usize)),
    };

    /// List size and alignment (3 pointer-sized fields)
    pub const list = SizeAlign{
        .size = 3 * @sizeOf(usize),
        .alignment = RocAlignment.fromByteUnits(@alignOf(usize)),
    };
};

test "Size of SizeAlign type" {
    try std.testing.expectEqual(32, @bitSizeOf(SizeAlign));
}

/// The memory layout of a value in a running Roc program.
///
/// A Layout can be created from a Roc type, given the additional information
/// of the build target's `usize`. Layouts cannot be created without knowing
/// that aspect of the build target, because pointers in layouts are different
/// sizes on 32-bit and 64-bit targets. No other target information is needed.
///
/// When a Roc type gets converted to a Layout, all zero-sized types
/// (e.g. empty records, empty tag unions, single-tag unions) get
/// dropped, because zero-sized values don't exist at runtime.
/// (Exception: we do allow things like List({}) and Box({}) because
/// the stack-allocated List and Box can be used at runtime even if
/// their elements cannot be accessed. For correctness, we need a
/// special runtime representation for those scenarios.)
///
/// Once a type has been converted to a Layout, there is no longer any
/// distinction between nominal and structural types, there's just memory.
/// Records and tuples have both been flattened (so, no more extension vars)
/// and converted into structs whose fields are sorted by alignment and then
/// alphabetically by field name (or numerically by tuple field index).
/// We still store their original field names (and tuple indices) for debuginfo later.
pub const Layout = packed struct {
    // This can't be a normal Zig tagged union because it uses a packed union to reduce memory use,
    // and Zig tagged unions don't support being packed.
    data: LayoutUnion,
    tag: LayoutTag,

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self.tag) {
            .scalar => switch (self.data.scalar.tag) {
                .int => self.data.scalar.data.int.alignment(),
                .frac => self.data.scalar.data.frac.alignment(),
                .bool => std.mem.Alignment.@"1",
                .str, .opaque_ptr => target_usize.alignment(),
            },
            .box, .box_of_zst => target_usize.alignment(),
            .list, .list_of_zst => target_usize.alignment(),
            .record => self.data.record.alignment,
            .tuple => self.data.tuple.alignment,
        };
    }

    /// int layout with the given precision
    pub fn int(precision: types.Num.Int.Precision) Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .int = precision }, .tag = .int } }, .tag = .scalar };
    }

    /// frac layout with the given precision
    pub fn frac(precision: types.Num.Frac.Precision) Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .frac = precision }, .tag = .frac } }, .tag = .scalar };
    }

    /// bool layout
    pub fn boolType() Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .bool = {} }, .tag = .bool } }, .tag = .scalar };
    }

    /// bool layout (alias for consistency)
    pub fn boolean() Layout {
        return boolType();
    }

    /// Check if this layout represents a boolean
    pub fn isBoolean(self: Layout) bool {
        return self.tag == .scalar and self.data.scalar.tag == .bool;
    }

    /// str layout
    pub fn str() Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .str = {} }, .tag = .str } }, .tag = .scalar };
    }

    /// box layout with the given element layout
    pub fn box(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .box = elem_idx }, .tag = .box };
    }

    /// box of zero-sized type layout (e.g. Box({}))
    pub fn boxOfZst() Layout {
        return Layout{ .data = .{ .box_of_zst = {} }, .tag = .box_of_zst };
    }

    /// list layout with the given element layout
    pub fn list(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .list = elem_idx }, .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return Layout{ .data = .{ .list_of_zst = {} }, .tag = .list_of_zst };
    }

    /// opaque pointer from the host's perspective (e.g. the void* that we pass to the host for flex vars etc.)
    pub fn opaquePtr() Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .opaque_ptr = {} }, .tag = .opaque_ptr } }, .tag = .scalar };
    }

    /// record layout with the given alignment and record metadata (e.g. size and field layouts)
    pub fn record(record_alignment: std.mem.Alignment, record_idx: RecordIdx) Layout {
        return Layout{ .data = .{ .record = .{ .alignment = record_alignment, .idx = record_idx } }, .tag = .record };
    }

    /// tuple layout with the given alignment and tuple metadata (e.g. size and field layouts)
    pub fn tuple(tuple_alignment: std.mem.Alignment, tuple_idx: TupleIdx) Layout {
        return Layout{ .data = .{ .tuple = .{ .alignment = tuple_alignment, .idx = tuple_idx } }, .tag = .tuple };
    }
};

test "Size of Layout type" {
    // The Layout should have small size since it's used a ton, so avoid letting this number increase!
    try std.testing.expectEqual(layout_bit_size, @bitSizeOf(Layout));
}

test "Layout.alignment() - scalar types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.int(.u8).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.int(.i8).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", Layout.int(.u16).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", Layout.int(.i16).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.int(.u32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.int(.i32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.int(.u64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.int(.i64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.int(.u128).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.int(.i128).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.frac(.f32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.frac(.f64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.frac(.dec).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.boolType().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.str().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.opaquePtr().alignment(target_usize));
    }
}

test "Layout.alignment() - types containing pointers" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(target_usize.alignment(), Layout.box(.bool).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.boxOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.list(.bool).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.listOfZst().alignment(target_usize));
    }
}

test "Layout.alignment() - record types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), Layout.record(std.mem.Alignment.@"4", RecordIdx{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), Layout.record(std.mem.Alignment.@"16", RecordIdx{ .int_idx = 1 }).alignment(target_usize));
    }
}

test "RecordData.getFields()" {
    const testing = std.testing;

    const record_data = RecordData{
        .size = 40,
        .fields = .{ .start = 10, .count = 5 },
    };

    const fields_range = record_data.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(fields_range.end));
}

test "Layout scalar data access" {
    const testing = std.testing;

    // Test int
    const int_layout = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_layout.tag);
    try testing.expectEqual(ScalarTag.int, int_layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, int_layout.data.scalar.data.int);

    // Test frac
    const frac_layout = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_layout.tag);
    try testing.expectEqual(ScalarTag.frac, frac_layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, frac_layout.data.scalar.data.frac);

    // Test bool
    const bool_layout = Layout.boolType();
    try testing.expectEqual(LayoutTag.scalar, bool_layout.tag);
    try testing.expectEqual(ScalarTag.bool, bool_layout.data.scalar.tag);
    try testing.expectEqual({}, bool_layout.data.scalar.data.bool);

    // Test str
    const str_layout = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_layout.tag);
    try testing.expectEqual(ScalarTag.str, str_layout.data.scalar.tag);
    try testing.expectEqual({}, str_layout.data.scalar.data.str);

    // Test opaque_ptr
    const opaque_ptr_layout = Layout.opaquePtr();
    try testing.expectEqual(LayoutTag.scalar, opaque_ptr_layout.tag);
    try testing.expectEqual(ScalarTag.opaque_ptr, opaque_ptr_layout.data.scalar.tag);
    try testing.expectEqual({}, opaque_ptr_layout.data.scalar.data.opaque_ptr);
}

test "Layout non-scalar types" {
    const testing = std.testing;

    // Test that non-scalar types have correct tags
    const box_layout = Layout.box(.bool);
    try testing.expectEqual(LayoutTag.box, box_layout.tag);

    const list_layout = Layout.list(.bool);
    try testing.expectEqual(LayoutTag.list, list_layout.tag);

    const record_layout = Layout.record(std.mem.Alignment.@"4", RecordIdx{ .int_idx = 0 });
    try testing.expectEqual(LayoutTag.record, record_layout.tag);
}

test "Layout scalar variants" {
    const testing = std.testing;

    // Test scalar type creation
    const int_scalar = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_scalar.tag);
    try testing.expectEqual(ScalarTag.int, int_scalar.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, int_scalar.data.scalar.data.int);

    const str_scalar = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_scalar.tag);
    try testing.expectEqual(ScalarTag.str, str_scalar.data.scalar.tag);

    const frac_scalar = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_scalar.tag);
    try testing.expectEqual(ScalarTag.frac, frac_scalar.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, frac_scalar.data.scalar.data.frac);

    const opaque_ptr_layout = Layout.opaquePtr();
    try testing.expectEqual(LayoutTag.scalar, opaque_ptr_layout.tag);
    try testing.expectEqual(ScalarTag.opaque_ptr, opaque_ptr_layout.data.scalar.tag);

    // Test zst variants separately
    const box_zst = Layout.boxOfZst();
    try testing.expectEqual(LayoutTag.box_of_zst, box_zst.tag);

    const list_zst = Layout.listOfZst();
    try testing.expectEqual(LayoutTag.list_of_zst, list_zst.tag);
}

test "Scalar memory optimization - comprehensive coverage" {
    const testing = std.testing;

    const bool_layout = Layout.boolType();
    try testing.expectEqual(LayoutTag.scalar, bool_layout.tag);
    try testing.expectEqual(ScalarTag.bool, bool_layout.data.scalar.tag);

    const str_layout = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_layout.tag);
    try testing.expectEqual(ScalarTag.str, str_layout.data.scalar.tag);

    const opaque_ptr_layout = Layout.opaquePtr();
    try testing.expectEqual(LayoutTag.scalar, opaque_ptr_layout.tag);
    try testing.expectEqual(ScalarTag.opaque_ptr, opaque_ptr_layout.data.scalar.tag);

    // Test ALL integer precisions
    const int_u8 = Layout.int(.u8);
    try testing.expectEqual(LayoutTag.scalar, int_u8.tag);
    try testing.expectEqual(ScalarTag.int, int_u8.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u8, int_u8.data.scalar.data.int);

    const int_i8 = Layout.int(.i8);
    try testing.expectEqual(LayoutTag.scalar, int_i8.tag);
    try testing.expectEqual(ScalarTag.int, int_i8.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i8, int_i8.data.scalar.data.int);

    const int_u16 = Layout.int(.u16);
    try testing.expectEqual(LayoutTag.scalar, int_u16.tag);
    try testing.expectEqual(ScalarTag.int, int_u16.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u16, int_u16.data.scalar.data.int);

    const int_i16 = Layout.int(.i16);
    try testing.expectEqual(LayoutTag.scalar, int_i16.tag);
    try testing.expectEqual(ScalarTag.int, int_i16.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i16, int_i16.data.scalar.data.int);

    const int_u32 = Layout.int(.u32);
    try testing.expectEqual(LayoutTag.scalar, int_u32.tag);
    try testing.expectEqual(ScalarTag.int, int_u32.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u32, int_u32.data.scalar.data.int);

    const int_i32 = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_i32.tag);
    try testing.expectEqual(ScalarTag.int, int_i32.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, int_i32.data.scalar.data.int);

    const int_u64 = Layout.int(.u64);
    try testing.expectEqual(LayoutTag.scalar, int_u64.tag);
    try testing.expectEqual(ScalarTag.int, int_u64.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u64, int_u64.data.scalar.data.int);

    const int_i64 = Layout.int(.i64);
    try testing.expectEqual(LayoutTag.scalar, int_i64.tag);
    try testing.expectEqual(ScalarTag.int, int_i64.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i64, int_i64.data.scalar.data.int);

    const int_u128 = Layout.int(.u128);
    try testing.expectEqual(LayoutTag.scalar, int_u128.tag);
    try testing.expectEqual(ScalarTag.int, int_u128.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u128, int_u128.data.scalar.data.int);

    const int_i128 = Layout.int(.i128);
    try testing.expectEqual(LayoutTag.scalar, int_i128.tag);
    try testing.expectEqual(ScalarTag.int, int_i128.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i128, int_i128.data.scalar.data.int);

    // Test ALL fraction precisions
    const frac_f32 = Layout.frac(.f32);
    try testing.expectEqual(LayoutTag.scalar, frac_f32.tag);
    try testing.expectEqual(ScalarTag.frac, frac_f32.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f32, frac_f32.data.scalar.data.frac);

    const frac_f64 = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_f64.tag);
    try testing.expectEqual(ScalarTag.frac, frac_f64.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, frac_f64.data.scalar.data.frac);

    const frac_dec = Layout.frac(.dec);
    try testing.expectEqual(LayoutTag.scalar, frac_dec.tag);
    try testing.expectEqual(ScalarTag.frac, frac_dec.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.dec, frac_dec.data.scalar.data.frac);
}

test "Non-scalar layout variants - fallback to indexed approach" {
    const testing = std.testing;

    // Test non-scalar box (should use .box tag with index)
    const box_non_scalar = Layout.box(@as(Idx, @enumFromInt(42)));
    try testing.expectEqual(LayoutTag.box, box_non_scalar.tag);
    try testing.expectEqual(@as(u28, 42), @intFromEnum(box_non_scalar.data.box));

    // Test non-scalar list (should use .list tag with index)
    const list_non_scalar = Layout.list(@as(Idx, @enumFromInt(123)));
    try testing.expectEqual(LayoutTag.list, list_non_scalar.tag);
    try testing.expectEqual(@as(u28, 123), @intFromEnum(list_non_scalar.data.list));

    // Test record layout (definitely non-scalar)
    const record_layout = Layout.record(std.mem.Alignment.@"8", RecordIdx{ .int_idx = 456 });
    try testing.expectEqual(LayoutTag.record, record_layout.tag);
    try testing.expectEqual(std.mem.Alignment.@"8", record_layout.data.record.alignment);
    try testing.expectEqual(@as(u19, 456), record_layout.data.record.idx.int_idx);
}

test "Layout scalar precision coverage" {
    const testing = std.testing;

    // Test all int precisions
    for ([_]types.Num.Int.Precision{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 }) |precision| {
        const int_layout = Layout.int(precision);
        try testing.expectEqual(LayoutTag.scalar, int_layout.tag);
        try testing.expectEqual(ScalarTag.int, int_layout.data.scalar.tag);
        try testing.expectEqual(precision, int_layout.data.scalar.data.int);
    }

    // Test all frac precisions
    for ([_]types.Num.Frac.Precision{ .f32, .f64, .dec }) |precision| {
        const frac_layout = Layout.frac(precision);
        try testing.expectEqual(LayoutTag.scalar, frac_layout.tag);
        try testing.expectEqual(ScalarTag.frac, frac_layout.data.scalar.tag);
        try testing.expectEqual(precision, frac_layout.data.scalar.data.frac);
    }

    // Test complex layout types have correct tags
    const complex_layouts = [_]Layout{
        Layout.box(.bool),
        Layout.boxOfZst(),
        Layout.list(.bool),
        Layout.listOfZst(),
        Layout.record(std.mem.Alignment.@"4", RecordIdx{ .int_idx = 0 }),
        Layout.tuple(std.mem.Alignment.@"8", TupleIdx{ .int_idx = 0 }),
    };

    const expected_tags = [_]LayoutTag{
        .box,
        .box_of_zst,
        .list,
        .list_of_zst,
        .record,
        .tuple,
    };

    for (complex_layouts, expected_tags) |layout, expected_tag| {
        try testing.expectEqual(expected_tag, layout.tag);
    }
}
