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
    list,
    list_of_zst, // List of zero-sized types, e.g. List({}) - needs a special-cased runtime implementation
    record,
    tuple,
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 32;

/// Tag for scalar variants - ordered to match Idx enum for arithmetic mapping
pub const ScalarTag = enum(u3) {
    bool = 0, // Maps to Idx 0
    str = 1, // Maps to Idx 1
    host_opaque = 2, // Maps to Idx 2
    int = 3, // Maps to Idx 3-12 (add precision value)
    frac = 4, // Maps to Idx 13-15 (add precision value + 11)
};

/// Scalar union data - ordered to match ScalarTag
pub const ScalarData = packed union {
    bool: void,
    str: void,
    host_opaque: void,
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
};

/// Scalar types that can be stored directly in box_of_scalar or list_of_scalar
pub const Scalar = packed struct {
    data: ScalarData,
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
    // Sentinel values for primitive types
    // Note: the layout store's idxForScalar method relies on these exact numbers!
    bool = 0,
    str = 1,
    host_opaque = 2,

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

    // Regular indices start from here
    // Must be kept in sync with PRIMITIVE_COUNT in store.zig
    _,
};

/// Union of Layout data
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
                .bool => std.mem.Alignment.@"1",
                .str, .host_opaque => target_usize.alignment(),
            },
            .box, .box_of_zst => target_usize.alignment(),
            .list, .list_of_zst => target_usize.alignment(),
            .record => self.data.record.alignment,
            .tuple => self.data.tuple.alignment,
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
    /// Create a list layout with the given element layout index
    pub fn list(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .list = elem_idx }, .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return Layout{ .data = .{ .list_of_zst = {} }, .tag = .list_of_zst };
    }

    /// list of scalar layout (e.g. List(I32), List(Str))
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

    /// Convert a layout to a scalar if possible, otherwise return an error
    /// Get the scalar value if this layout represents a scalar
    pub fn asScalar(self: Layout) !Scalar {
        return switch (self.tag) {
            .scalar => self.data.scalar,
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
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.booleanType().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.str().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.hostOpaque().alignment(target_usize));
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
    const box_layout = Layout.box(.bool);
    try testing.expectError(error.NotAScalar, box_layout.asScalar());

    const list_layout = Layout.list(.bool);
    try testing.expectError(error.NotAScalar, list_layout.asScalar());

    const record_layout = Layout.record(std.mem.Alignment.@"4", RecordIdx{ .int_idx = 0 });
    try testing.expectError(error.NotAScalar, record_layout.asScalar());
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

    const host_opaque_scalar = Layout.hostOpaque();
    try testing.expectEqual(LayoutTag.scalar, host_opaque_scalar.tag);
    try testing.expectEqual(ScalarTag.host_opaque, host_opaque_scalar.data.scalar.tag);

    // Test zst variants separately
    const box_zst = Layout.boxOfZst();
    try testing.expectEqual(LayoutTag.box_of_zst, box_zst.tag);

    const list_zst = Layout.listOfZst();
    try testing.expectEqual(LayoutTag.list_of_zst, list_zst.tag);
}

test "Scalar memory optimization - comprehensive coverage" {
    const testing = std.testing;

    // Test all scalar types
    const int_layout = Layout.int(.u64);
    try testing.expectEqual(LayoutTag.scalar, int_layout.tag);
    try testing.expectEqual(ScalarTag.int, int_layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.u64, int_layout.data.scalar.data.int);

    const frac_layout = Layout.frac(.f32);
    try testing.expectEqual(LayoutTag.scalar, frac_layout.tag);
    try testing.expectEqual(ScalarTag.frac, frac_layout.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f32, frac_layout.data.scalar.data.frac);

    const bool_layout = Layout.booleanType();
    try testing.expectEqual(LayoutTag.scalar, bool_layout.tag);
    try testing.expectEqual(ScalarTag.bool, bool_layout.data.scalar.tag);

    const str_layout = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_layout.tag);
    try testing.expectEqual(ScalarTag.str, str_layout.data.scalar.tag);

    const host_opaque_layout = Layout.hostOpaque();
    try testing.expectEqual(LayoutTag.scalar, host_opaque_layout.tag);
    try testing.expectEqual(ScalarTag.host_opaque, host_opaque_layout.data.scalar.tag);

    // Test different integer precisions
    const int_i128 = Layout.int(.i128);
    try testing.expectEqual(LayoutTag.scalar, int_i128.tag);
    try testing.expectEqual(ScalarTag.int, int_i128.data.scalar.tag);
    try testing.expectEqual(types.Num.Int.Precision.i128, int_i128.data.scalar.data.int);

    // Test different float precisions
    const frac_f64 = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_f64.tag);
    try testing.expectEqual(ScalarTag.frac, frac_f64.data.scalar.tag);
    try testing.expectEqual(types.Num.Frac.Precision.f64, frac_f64.data.scalar.data.frac);
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
        Layout.box(.bool),
        Layout.list(.bool),
        Layout.boxOfZst(), // ZST containers are not scalars themselves
        Layout.listOfZst(), // ZST containers are not scalars themselves
        Layout.record(std.mem.Alignment.@"4", RecordIdx{ .int_idx = 0 }),
    };

    for (complex_layouts) |layout| {
        try testing.expectError(error.NotAScalar, layout.asScalar());
    }
}

test "Layout.alignment() - tuple types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        // Test tuple alignment
        const tuple_layout = Layout.tuple(std.mem.Alignment.@"8", TupleIdx{ .int_idx = 0 });
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
        .layout = @as(Idx, @enumFromInt(5)),
    };

    try testing.expectEqual(@as(u24, 1), tuple_field.index);
    try testing.expectEqual(@as(u28, 5), @intFromEnum(tuple_field.layout));
}

test "Tuple memory optimization - comprehensive coverage" {
    const testing = std.testing;

    // Test that tuple layouts properly handle different field types
    const tuple_layouts = [_]Layout{
        Layout.tuple(std.mem.Alignment.@"1", TupleIdx{ .int_idx = 0 }),
        Layout.tuple(std.mem.Alignment.@"4", TupleIdx{ .int_idx = 1 }),
        Layout.tuple(std.mem.Alignment.@"8", TupleIdx{ .int_idx = 2 }),
        Layout.tuple(std.mem.Alignment.@"16", TupleIdx{ .int_idx = 3 }),
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
