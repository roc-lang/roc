//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

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
    box_of_scalar, // Box of a scalar value, e.g. Box(Str) - has a more compact representation than .box
    list,
    list_of_zst, // List of zero-sized types, e.g. List({}) - needs a special-cased runtime implementation
    list_of_scalar, // List of scalar values, e.g. List(Str) - has a more compact representation than .list
    record,
    tuple,
    record_wrapping_scalar, // Record with exactly one scalar field - more compact than .record
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 32;

/// Tag for Scalar variants
pub const ScalarTag = enum(u3) {
    bool,
    str,
    int,
    frac,
    host_opaque, // void* on the host, e.g. a flex var that was allowed to pass through to the host
};

/// Scalar union data
pub const ScalarData = packed union {
    bool: void,
    str: void,
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    host_opaque: void,
};

/// Scalar types that can be stored directly in box_of_scalar or list_of_scalar
pub const Scalar = packed struct {
    data: ScalarData,
    tag: ScalarTag,
};

/// A compact identifier index that fits in 20 bits (max 1M identifiers per module)
/// TODO: change the normal Ident.Idx to be this instead.
pub const CompactIdentIdx = packed struct(u20) {
    idx: u20,
};

/// Record wrapping scalar - a record with exactly one scalar field
pub const RecordWrappingScalar = packed struct(u28) {
    scalar: Scalar, // u7 (u4 for data union + u3 for tag)
    field_name_idx: Ident.Idx, // u21 (u3 for attributes + Ident.IDX_BITS for idx)
};

/// Index into a Layout Store
pub const Idx = packed struct {
    int_idx: @Type(.{
        .int = .{
            .signedness = .unsigned,
            // Some Layout variants are just the Tag followed by Idx, so use as many
            // bits as we can spare from the Layout for Idx.
            .bits = layout_bit_size - @bitSizeOf(LayoutTag),
        },
    }),
};

/// Union of Layout data
/// The largest variant must fit in 28 bits to leave room for the u4 tag
pub const LayoutUnion = packed union {
    scalar: Scalar,
    box: Idx,
    box_of_zst: void,
    box_of_scalar: Scalar,
    list: Idx,
    list_of_zst: void,
    list_of_scalar: Scalar,
    record: RecordLayout,
    tuple: TupleLayout,
    record_wrapping_scalar: RecordWrappingScalar,
};

/// The runtime memory layout of a Roc value.
///
/// When a Roc type gets converted to a Layout, all zero-sized types
/// (e.g. empty records, empty tag unions, single-tag unions) must be
/// dropped, because zero-sized values don't exist at runtime.
/// (Exception: we do allow things like List({}) and Box({}) because
/// the stack-allocated List and Box can be used at runtime even if
/// their elements cannot be accessed. For correctness, we need a
/// special runtime representation for those scenarios.)
///
/// Once a type has been converted to a Layout, there is no longer any
/// distinction between nominal and structural types, there's just memory.
/// Records and tuples have both been flattened (to remove their extensions)
/// and converted into structs whose fields are sorted by alignment and then
/// alphabetically by field name (or numerically by tuple field index).
pub const Layout = packed struct {
    data: LayoutUnion,
    tag: LayoutTag,

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self.tag) {
            .scalar => switch (self.data.scalar.tag) {
                .int => self.data.scalar.data.int.alignment(),
                .frac => self.data.scalar.data.frac.alignment(),
                .bool, .str, .host_opaque => target_usize.alignment(),
            },
            .record => self.data.record.alignment,
            .tuple => self.data.tuple.alignment,
            .box, .box_of_zst, .box_of_scalar, .list, .list_of_zst, .list_of_scalar => target_usize.alignment(),
            .record_wrapping_scalar => switch (self.data.record_wrapping_scalar.scalar.tag) {
                .int => self.data.record_wrapping_scalar.scalar.data.int.alignment(),
                .frac => self.data.record_wrapping_scalar.scalar.data.frac.alignment(),
                .bool, .str, .host_opaque => target_usize.alignment(),
            },
        };
    }

    /// int layout with the given precision
    /// Create an int layout with the given precision
    pub fn int(precision: types.Num.Int.Precision) Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .int = precision }, .tag = .int } }, .tag = .scalar };
    }

    /// Create a frac layout with the given precision
    pub fn frac(precision: types.Num.Frac.Precision) Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .frac = precision }, .tag = .frac } }, .tag = .scalar };
    }

    /// Create a boolean layout
    pub fn booleanType() Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .bool = {} }, .tag = .bool } }, .tag = .scalar };
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

    /// box of scalar layout (e.g. Box(I32), Box(Str))
    pub fn boxOfScalar(scalar: Scalar) Layout {
        return Layout{ .data = .{ .box_of_scalar = scalar }, .tag = .box_of_scalar };
    }

    /// Create a list layout with the given element layout index
    pub fn list(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .list = elem_idx }, .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return Layout{ .data = .{ .list_of_zst = {} }, .tag = .list_of_zst };
    }

    /// list of scalar layout (e.g. List(I32), List(Str))
    pub fn listOfScalar(scalar: Scalar) Layout {
        return Layout{ .data = .{ .list_of_scalar = scalar }, .tag = .list_of_scalar };
    }

    /// host opaque layout (the void* that we pass to the host for e.g. flex vars)
    pub fn hostOpaque() Layout {
        return Layout{ .data = .{ .scalar = .{ .data = .{ .host_opaque = {} }, .tag = .host_opaque } }, .tag = .scalar };
    }

    /// record layout with the given alignment and RecordIdx
    pub fn record(record_alignment: std.mem.Alignment, idx: RecordIdx) Layout {
        return Layout{ .data = .{ .record = .{ .alignment = record_alignment, .idx = idx } }, .tag = .record };
    }

    /// tuple layout with the given alignment and TupleIdx
    pub fn tuple(tuple_alignment: std.mem.Alignment, idx: TupleIdx) Layout {
        return Layout{ .data = .{ .tuple = .{ .alignment = tuple_alignment, .idx = idx } }, .tag = .tuple };
    }

    /// record wrapping scalar layout - a record with exactly one scalar field
    pub fn recordWrappingScalar(scalar: Scalar, field_name_idx: Ident.Idx) Layout {
        return Layout{ .data = .{ .record_wrapping_scalar = .{
            .scalar = scalar,
            .field_name_idx = field_name_idx,
        } }, .tag = .record_wrapping_scalar };
    }

    /// Convert a layout to a scalar if possible, otherwise return an error
    pub fn asScalar(self: Layout) !Scalar {
        return switch (self.tag) {
            .scalar => self.data.scalar,
            .record_wrapping_scalar => self.data.record_wrapping_scalar.scalar,
            else => error.NotAScalar,
        };
    }
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
        try testing.expectEqual(target_usize.alignment(), Layout.booleanType().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.str().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.hostOpaque().alignment(target_usize));
    }
}

test "Layout.alignment() - types containing pointers" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(target_usize.alignment(), Layout.box(.{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.boxOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.boxOfScalar(.{ .data = .{ .int = .i32 }, .tag = .int }).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.list(.{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.listOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.listOfScalar(.{ .data = .{ .str = {} }, .tag = .str }).alignment(target_usize));
    }
}

test "Layout.alignment() - record types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), Layout.record(std.mem.Alignment.@"4", .{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), Layout.record(std.mem.Alignment.@"16", .{ .int_idx = 1 }).alignment(target_usize));
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

test "Layout.asScalar() - scalar types" {
    const testing = std.testing;

    // Test int conversion
    const int_layout = Layout.int(.i32);
    const int_scalar = try int_layout.asScalar();
    try testing.expectEqual(ScalarTag.int, int_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, int_scalar.data.int);

    // Test frac conversion
    const frac_layout = Layout.frac(.f64);
    const frac_scalar = try frac_layout.asScalar();
    try testing.expectEqual(ScalarTag.frac, frac_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, frac_scalar.data.frac);

    // Test bool conversion
    const bool_layout = Layout.booleanType();
    const bool_scalar = try bool_layout.asScalar();
    try testing.expectEqual(ScalarTag.bool, bool_scalar.tag);
    try testing.expectEqual({}, bool_scalar.data.bool);

    // Test str conversion
    const str_layout = Layout.str();
    const str_scalar = try str_layout.asScalar();
    try testing.expectEqual(ScalarTag.str, str_scalar.tag);
    try testing.expectEqual({}, str_scalar.data.str);

    // Test host_opaque conversion
    const host_opaque_layout = Layout.hostOpaque();
    const host_opaque_scalar = try host_opaque_layout.asScalar();
    try testing.expectEqual(ScalarTag.host_opaque, host_opaque_scalar.tag);
    try testing.expectEqual({}, host_opaque_scalar.data.host_opaque);
}

test "Layout.asScalar() - non-scalar types" {
    const testing = std.testing;

    // Test that non-scalar types return null
    const box_layout = Layout.box(.{ .int_idx = 0 });
    try testing.expectError(error.NotAScalar, box_layout.asScalar());

    const list_layout = Layout.list(.{ .int_idx = 0 });
    try testing.expectError(error.NotAScalar, list_layout.asScalar());

    const record_layout = Layout.record(std.mem.Alignment.@"4", .{ .int_idx = 0 });
    try testing.expectError(error.NotAScalar, record_layout.asScalar());
}

test "Layout scalar variants" {
    const testing = std.testing;

    // Test box_of_scalar
    const box_int_scalar = Layout.boxOfScalar(.{ .data = .{ .int = .i32 }, .tag = .int });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_int_scalar.tag);
    try testing.expectEqual(ScalarTag.int, box_int_scalar.data.box_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, box_int_scalar.data.box_of_scalar.data.int);

    const box_str_scalar = Layout.boxOfScalar(.{ .data = .{ .str = {} }, .tag = .str });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_str_scalar.tag);
    try testing.expectEqual(ScalarTag.str, box_str_scalar.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_str_scalar.data.box_of_scalar.data.str);

    // Test list_of_scalar
    const list_frac_scalar = Layout.listOfScalar(.{ .data = .{ .frac = .f64 }, .tag = .frac });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_frac_scalar.tag);
    try testing.expectEqual(ScalarTag.frac, list_frac_scalar.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, list_frac_scalar.data.list_of_scalar.data.frac);

    const list_host_opaque_scalar = Layout.listOfScalar(.{ .data = .{ .host_opaque = {} }, .tag = .host_opaque });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_host_opaque_scalar.tag);
    try testing.expectEqual(ScalarTag.host_opaque, list_host_opaque_scalar.data.list_of_scalar.tag);
    try testing.expectEqual({}, list_host_opaque_scalar.data.list_of_scalar.data.host_opaque);

    // Test zst variants separately
    const box_zst = Layout.boxOfZst();
    try testing.expectEqual(LayoutTag.box_of_zst, box_zst.tag);

    const list_zst = Layout.listOfZst();
    try testing.expectEqual(LayoutTag.list_of_zst, list_zst.tag);
}

test "Scalar memory optimization - comprehensive coverage" {
    const testing = std.testing;

    // Test all scalar variants in box_of_scalar
    const box_int = Layout.boxOfScalar(.{ .data = .{ .int = .u64 }, .tag = .int });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_int.tag);
    try testing.expectEqual(ScalarTag.int, box_int.data.box_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u64, box_int.data.box_of_scalar.data.int);

    const box_frac = Layout.boxOfScalar(.{ .data = .{ .frac = .f32 }, .tag = .frac });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_frac.tag);
    try testing.expectEqual(ScalarTag.frac, box_frac.data.box_of_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f32, box_frac.data.box_of_scalar.data.frac);

    const box_bool = Layout.boxOfScalar(.{ .data = .{ .bool = {} }, .tag = .bool });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_bool.tag);
    try testing.expectEqual(ScalarTag.bool, box_bool.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_bool.data.box_of_scalar.data.bool);

    const box_str = Layout.boxOfScalar(.{ .data = .{ .str = {} }, .tag = .str });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_str.tag);
    try testing.expectEqual(ScalarTag.str, box_str.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_str.data.box_of_scalar.data.str);

    const box_host_opaque = Layout.boxOfScalar(.{ .data = .{ .host_opaque = {} }, .tag = .host_opaque });
    try testing.expectEqual(LayoutTag.box_of_scalar, box_host_opaque.tag);
    try testing.expectEqual(ScalarTag.host_opaque, box_host_opaque.data.box_of_scalar.tag);
    try testing.expectEqual({}, box_host_opaque.data.box_of_scalar.data.host_opaque);

    const box_zst = Layout.boxOfZst();
    try testing.expectEqual(LayoutTag.box_of_zst, box_zst.tag);

    // Test all scalar variants in list_of_scalar
    const list_int = Layout.listOfScalar(.{ .data = .{ .int = .i16 }, .tag = .int });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_int.tag);
    try testing.expectEqual(ScalarTag.int, list_int.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i16, list_int.data.list_of_scalar.data.int);

    const list_frac = Layout.listOfScalar(.{ .data = .{ .frac = .f64 }, .tag = .frac });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_frac.tag);
    try testing.expectEqual(ScalarTag.frac, list_frac.data.list_of_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, list_frac.data.list_of_scalar.data.frac);

    const list_bool = Layout.listOfScalar(.{ .data = .{ .bool = {} }, .tag = .bool });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_bool.tag);
    try testing.expectEqual(ScalarTag.bool, list_bool.data.list_of_scalar.tag);
    try testing.expectEqual({}, list_bool.data.list_of_scalar.data.bool);

    const list_str = Layout.listOfScalar(.{ .data = .{ .str = {} }, .tag = .str });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_str.tag);
    try testing.expectEqual(ScalarTag.str, list_str.data.list_of_scalar.tag);
    try testing.expectEqual({}, list_str.data.list_of_scalar.data.str);

    const list_host_opaque = Layout.listOfScalar(.{ .data = .{ .host_opaque = {} }, .tag = .host_opaque });
    try testing.expectEqual(LayoutTag.list_of_scalar, list_host_opaque.tag);
    try testing.expectEqual(ScalarTag.host_opaque, list_host_opaque.data.list_of_scalar.tag);
    try testing.expectEqual({}, list_host_opaque.data.list_of_scalar.data.host_opaque);

    const list_zst = Layout.listOfZst();
    try testing.expectEqual(LayoutTag.list_of_zst, list_zst.tag);
}

test "Non-scalar layout variants - fallback to indexed approach" {
    const testing = std.testing;

    // Test non-scalar box (should use .box tag with index)
    const box_non_scalar = Layout.box(.{ .int_idx = 42 });
    try testing.expectEqual(LayoutTag.box, box_non_scalar.tag);
    try testing.expectEqual(@as(u27, 42), box_non_scalar.data.box.int_idx);

    // Test non-scalar list (should use .list tag with index)
    const list_non_scalar = Layout.list(.{ .int_idx = 123 });
    try testing.expectEqual(LayoutTag.list, list_non_scalar.tag);
    try testing.expectEqual(@as(u27, 123), list_non_scalar.data.list.int_idx);

    // Test record layout (definitely non-scalar)
    const record_layout = Layout.record(std.mem.Alignment.@"8", .{ .int_idx = 456 });
    try testing.expectEqual(LayoutTag.record, record_layout.tag);
    try testing.expectEqual(std.mem.Alignment.@"8", record_layout.data.record.alignment);
    try testing.expectEqual(@as(u19, 456), record_layout.data.record.idx.int_idx);
}

test "Layout.asScalar() - edge cases and error handling" {
    const testing = std.testing;

    // Test all int precisions
    for ([_]types.Num.Int.Precision{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 }) |precision| {
        const int_layout = Layout.int(precision);
        const scalar = try int_layout.asScalar();
        try testing.expectEqual(ScalarTag.int, scalar.tag);
        try testing.expectEqual(precision, scalar.data.int);
    }

    // Test all frac precisions
    for ([_]types.Num.Frac.Precision{ .f32, .f64 }) |precision| {
        const frac_layout = Layout.frac(precision);
        const scalar = try frac_layout.asScalar();
        try testing.expectEqual(ScalarTag.frac, scalar.tag);
        try testing.expectEqual(precision, scalar.data.frac);
    }

    // Test that complex layouts cannot be converted to scalars
    const complex_layouts = [_]Layout{
        Layout.box(.{ .int_idx = 0 }),
        Layout.list(.{ .int_idx = 0 }),
        Layout.boxOfZst(), // ZST containers are not scalars themselves
        Layout.listOfZst(), // ZST containers are not scalars themselves
        Layout.boxOfScalar(.{ .data = .{ .int = .i32 }, .tag = .int }), // This is already a scalar container, not a scalar itself
        Layout.listOfScalar(.{ .data = .{ .frac = .f64 }, .tag = .frac }), // This is already a scalar container, not a scalar itself
        Layout.record(std.mem.Alignment.@"4", .{ .int_idx = 0 }),
    };

    for (complex_layouts) |layout| {
        try testing.expectError(error.NotAScalar, layout.asScalar());
    }
}

test "Layout.alignment() - tuple types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        // Test tuple alignment
        const tuple_layout = Layout.tuple(std.mem.Alignment.@"8", .{ .int_idx = 0 });
        try testing.expectEqual(std.mem.Alignment.@"8", tuple_layout.alignment(target_usize));
    }
}

test "TupleData.getFields()" {
    const testing = std.testing;
    const tuple_data = TupleData{
        .size = 24,
        .fields = .{ .start = 10, .count = 3 },
    };

    const fields_range = tuple_data.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 13), @intFromEnum(fields_range.end));
}

test "Layout tuple variants" {
    const testing = std.testing;
    // Test tuple layout creation
    const tuple_idx = TupleIdx{ .int_idx = 42 };
    const tuple_layout = Layout.tuple(std.mem.Alignment.@"8", tuple_idx);

    try testing.expectEqual(LayoutTag.tuple, tuple_layout.tag);
    try testing.expectEqual(std.mem.Alignment.@"8", tuple_layout.data.tuple.alignment);
    try testing.expectEqual(@as(@TypeOf(tuple_idx.int_idx), 42), tuple_layout.data.tuple.idx.int_idx);
}

test "TupleField structure" {
    const testing = std.testing;
    const tuple_field = TupleField{
        .index = 1,
        .layout = Idx{ .int_idx = 5 },
    };

    try testing.expectEqual(@as(u24, 1), tuple_field.index);
    try testing.expectEqual(@as(@TypeOf(tuple_field.layout.int_idx), 5), tuple_field.layout.int_idx);
}

test "Tuple memory optimization - comprehensive coverage" {
    const testing = std.testing;

    // Test that tuple layouts properly handle different field types
    const tuple_layouts = [_]Layout{
        Layout.tuple(std.mem.Alignment.@"1", .{ .int_idx = 0 }),
        Layout.tuple(std.mem.Alignment.@"4", .{ .int_idx = 1 }),
        Layout.tuple(std.mem.Alignment.@"8", .{ .int_idx = 2 }),
        Layout.tuple(std.mem.Alignment.@"16", .{ .int_idx = 3 }),
    };

    for (tuple_layouts) |layout| {
        try testing.expectEqual(LayoutTag.tuple, layout.tag);

        // Test that tuple layouts cannot be converted to scalars
        try testing.expectError(error.NotAScalar, layout.asScalar());
    }
}

test "TupleIdx bit packing" {
    const testing = std.testing;

    // Test that TupleIdx can hold large values
    const large_value: u32 = 65535; // Large but reasonable value
    const tuple_idx = TupleIdx{ .int_idx = large_value };
    try testing.expectEqual(large_value, tuple_idx.int_idx);

    // Test tuple layout with large index
    const tuple_layout = Layout.tuple(std.mem.Alignment.@"1", tuple_idx);
    try testing.expectEqual(large_value, tuple_layout.data.tuple.idx.int_idx);
}

test "TupleData size calculation" {
    const testing = std.testing;

    // Test various tuple sizes
    const test_cases = [_]struct { size: u32, field_count: u32 }{
        .{ .size = 0, .field_count = 0 },
        .{ .size = 8, .field_count = 1 },
        .{ .size = 16, .field_count = 2 },
        .{ .size = 32, .field_count = 4 },
        .{ .size = 1024, .field_count = 64 },
    };

    for (test_cases) |case| {
        const tuple_data = TupleData{
            .size = case.size,
            .fields = .{ .start = 0, .count = case.field_count },
        };

        try testing.expectEqual(case.size, tuple_data.size);

        const fields_range = tuple_data.getFields();
        try testing.expectEqual(@as(u32, 0), @intFromEnum(fields_range.start));
        try testing.expectEqual(case.field_count, @intFromEnum(fields_range.end));
    }
}

test "record_wrapping_scalar layout creation" {
    const testing = std.testing;

    // Test with int scalar
    const int_scalar = Scalar{ .data = .{ .int = .i32 }, .tag = .int };
    const field_name_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 42 };
    const layout = Layout.recordWrappingScalar(int_scalar, field_name_idx);

    try testing.expectEqual(LayoutTag.record_wrapping_scalar, layout.tag);
    try testing.expectEqual(ScalarTag.int, layout.data.record_wrapping_scalar.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i32, layout.data.record_wrapping_scalar.scalar.data.int);
    try testing.expectEqual(@as(Ident.IdxFieldType, 42), layout.data.record_wrapping_scalar.field_name_idx.idx);
}

test "record_wrapping_scalar alignment" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        // Test with different scalar types
        const int_scalar = Scalar{ .data = .{ .int = .i64 }, .tag = .int };
        const int_layout = Layout.recordWrappingScalar(int_scalar, .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 });
        try testing.expectEqual(std.mem.Alignment.@"8", int_layout.alignment(target_usize));

        const bool_scalar = Scalar{ .data = .{ .bool = {} }, .tag = .bool };
        const bool_layout = Layout.recordWrappingScalar(bool_scalar, .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 });
        try testing.expectEqual(target_usize.alignment(), bool_layout.alignment(target_usize));

        const str_scalar = Scalar{ .data = .{ .str = {} }, .tag = .str };
        const str_layout = Layout.recordWrappingScalar(str_scalar, .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 });
        try testing.expectEqual(target_usize.alignment(), str_layout.alignment(target_usize));
    }
}

test "record_wrapping_scalar asScalar" {
    const testing = std.testing;

    // Test that asScalar returns the wrapped scalar
    const scalar = Scalar{ .data = .{ .frac = .f64 }, .tag = .frac };
    const layout = Layout.recordWrappingScalar(scalar, .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 100 });

    const extracted_scalar = try layout.asScalar();
    try testing.expectEqual(ScalarTag.frac, extracted_scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, extracted_scalar.data.frac);
}

test "Ident.Idx limits" {
    const testing = std.testing;

    // Test maximum value (2^18 - 1 = 262143)
    const max_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = std.math.maxInt(Ident.IdxFieldType) };
    try testing.expectEqual(@as(Ident.IdxFieldType, std.math.maxInt(Ident.IdxFieldType)), max_idx.idx);

    // Test that it's exactly 21 bits (3 for attributes + Ident.IDX_BITS for idx)
    try testing.expectEqual(@as(u32, @bitSizeOf(Ident.Idx)), @bitSizeOf(Ident.Attributes) + Ident.IDX_BITS);
}

test "RecordWrappingScalar size" {
    const testing = std.testing;

    // RecordWrappingScalar should be exactly 28 bits:
    // - Scalar: 7 bits (4 for data union + 3 for tag)
    // - Ident.Idx: 21 bits (3 for attributes + Ident.IDX_BITS for idx)
    // Total: 28 bits
    try testing.expectEqual(@as(u32, @bitSizeOf(RecordWrappingScalar)), 28);

    // Test with various scalars
    const test_scalars = [_]Scalar{
        .{ .data = .{ .int = .i32 }, .tag = .int },
        .{ .data = .{ .bool = {} }, .tag = .bool },
        .{ .data = .{ .str = {} }, .tag = .str },
        .{ .data = .{ .frac = .f64 }, .tag = .frac },
        .{ .data = .{ .host_opaque = {} }, .tag = .host_opaque },
    };

    for (test_scalars) |scalar| {
        const rws = RecordWrappingScalar{
            .scalar = scalar,
            .field_name_idx = .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 12345 },
        };

        // Verify we can round-trip the data
        try testing.expectEqual(scalar.tag, rws.scalar.tag);
        try testing.expectEqual(@as(Ident.IdxFieldType, 12345), rws.field_name_idx.idx);
    }
}

test "record_wrapping_scalar comprehensive" {
    const testing = std.testing;

    // Test all scalar types can be wrapped
    const scalars = [_]struct { scalar: Scalar, name: []const u8 }{
        .{ .scalar = .{ .data = .{ .int = .i8 }, .tag = .int }, .name = "i8" },
        .{ .scalar = .{ .data = .{ .int = .i16 }, .tag = .int }, .name = "i16" },
        .{ .scalar = .{ .data = .{ .int = .i32 }, .tag = .int }, .name = "i32" },
        .{ .scalar = .{ .data = .{ .int = .i64 }, .tag = .int }, .name = "i64" },
        .{ .scalar = .{ .data = .{ .int = .i128 }, .tag = .int }, .name = "i128" },
        .{ .scalar = .{ .data = .{ .frac = .f32 }, .tag = .frac }, .name = "f32" },
        .{ .scalar = .{ .data = .{ .frac = .f64 }, .tag = .frac }, .name = "f64" },
        .{ .scalar = .{ .data = .{ .bool = {} }, .tag = .bool }, .name = "bool" },
        .{ .scalar = .{ .data = .{ .str = {} }, .tag = .str }, .name = "str" },
        .{ .scalar = .{ .data = .{ .host_opaque = {} }, .tag = .host_opaque }, .name = "host_opaque" },
    };

    for (scalars, 0..) |test_case, i| {
        const field_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = @intCast(i * 1000) };
        const layout = Layout.recordWrappingScalar(test_case.scalar, field_idx);

        // Verify layout tag
        try testing.expectEqual(LayoutTag.record_wrapping_scalar, layout.tag);

        // Verify scalar data is preserved
        try testing.expectEqual(test_case.scalar.tag, layout.data.record_wrapping_scalar.scalar.tag);

        // Verify field name index is preserved
        try testing.expectEqual(@as(Ident.IdxFieldType, @intCast(i * 1000)), layout.data.record_wrapping_scalar.field_name_idx.idx);

        // Verify asScalar works
        const extracted = try layout.asScalar();
        try testing.expectEqual(test_case.scalar.tag, extracted.tag);
    }
}
