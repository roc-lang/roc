//! Memory layout representations for values in running Roc programs.
//!
//! See the Layout Store for how these representations actually get created
//! (using type and target information from previous steps in compilation).

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const target = @import("../base/target.zig");

pub const SizeAlign = packed struct(u28) {
    size: u25, // u25 can represent sizes up to 32 MiB
    alignment: std.mem.Alignment, // u3 on 64-bit targets, u2 on 32-bit targets

    // Note: on 32-bit targets, there's an extra bit of uninitializde memory here,
    // because the size of std.mem.Alignment is based on usize. If we want this data
    // structure to be bit-for-bit identical on 32-bit and 64-bit builds of the Roc
    // compiler, e.g. for serialization purposes, then we should make our own version
    // of std.mem.Alignment which is hardcoded to be a u2 (which is enough, as Roc's
    // compiler never generates values that are more than 16B aligned.)
};

test "Size of SizeAlign type" {
    try std.testing.expectEqual(32, @bitSizeOf(SizeAlign));
}

/// Tag for Layout variants
pub const LayoutTag = enum(u4) {
    scalar,
    box,
    box_of_zst, // Box of a zero-sized type, e.g. Box({}) - needs a special-cased runtime implementation
    list,
    list_of_zst, // List of zero-sized types, e.g. List({}) - needs a special-cased runtime implementation
    record,
    tuple,
    tagged_union, // A union of alternatives plus a u8 tag (u16 if there are over 256 alternatives)
    boxed_closure, // A Box(fn)
    unboxed_closure, // A closure
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
    tagged_union: TaggedUnionLayout,
    unboxed_closure: UnboxedClosureLayout,
    boxed_closure: BoxedClosureLayout,
};

// Tuple of fn pointer and pointer to refcounted heap allocation of its capture
pub const BoxedClosureLayout = struct {
    //
};

pub const TupleLayout = struct {
    first_field_idx: TupleFieldLayout.Idx,
    num_fields: u14,
};

pub const RecordLayout = struct {
    first_field_idx: RecordFieldLayout.Idx,
    num_fields: u14,
};

pub const RecordFieldLayout = struct {
    field_name: Ident.Idx,
    field_layout: Layout.Idx,

    const Idx = struct { int_val: u18 };
};

/// A union of alternatives plus a u8 tag (u16 if there are over 256 alternatives)
const TaggedUnionLayout = struct {
    first_alternative_idx: u18,
    num_alternatives: u14,

    /// Returns 1 for 255 or fewer alternatives, 2 if more than 255 alternatives, etc.
    fn tagBytes(self: @This()) usize {
        const len = @as(usize, @intCast(self.num_alternatives));
        return @as(usize, 1) << std.math.log2_int(usize, len - 1);
    }
};

/// A union of alternative captures plus a tag for which fn to call
const UnboxedClosureLayout = struct {
    first_alternative_idx: u18,
    num_alternatives: u14,

    fn tagBytes(self: @This()) usize {
        const len = @as(usize, @intCast(self.num_alternatives));

        return @as(usize, 1) << std.math.log2_int(usize, len - 1);
    }
};

test "UnboxedClosureLayout.tagBytes()" {
    const testing = std.testing;

    // Test 1 byte tag (1-255 alternatives)
    const test_cases_1_bytes = [_]u14{ 0, 1, 2, 255 };
    for (test_cases_1_bytes) |num_alternatives| {
        const layout = UnboxedClosureLayout{
            .first_alternative_idx = 0,
            .num_alternatives = @intCast(num_alternatives),
        };

        try testing.expectEqual(@as(usize, 1), layout.tagBytes());
    }

    // Test 2 byte tag (256-65535 alternatives)
    const test_cases_2_bytes = [_]u14{ 256, 257, 1000, 32767, 65535 };
    for (test_cases_2_bytes) |num_alternatives| {
        const layout = UnboxedClosureLayout{
            .first_alternative_idx = 0,
            .num_alternatives = num_alternatives,
        };
        try testing.expectEqual(@as(usize, 2), layout.tagBytes());
    }

    // Test 4 byte tag (65536 and above)
    const test_cases_4_bytes = [_]u14{ 65536, 100000, 262143 };
    for (test_cases_4_bytes) |num_alternatives| {
        const layout = UnboxedClosureLayout{
            .first_alternative_idx = 0,
            .num_alternatives = num_alternatives,
        };
        try testing.expectEqual(@as(usize, 4), layout.tagBytes());
    }
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
    tag: LayoutTag,
    size_align: SizeAlign,
    data: LayoutUnion,

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
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 64;

/// Tag for scalar variants
///
/// The exact numbers here are important, because we use them to convert between
/// Scalar and Idx using branchless arithmetic instructions. Don't change them
/// lightly, and make sure to re-run tests if you do!
pub const ScalarTag = enum(u3) {
    // 0 is reserved because these map to Var, and Var has 0 reserved for a sentinel value.
    int = 1, // Maps to Idx 1-10 (depending on precision)
    frac = 2, // Maps to Idx 11-13 (depending on precision)
    bool = 3, // Maps to Idx 14
    str = 4, // Maps to Idx 15
    opaque_ptr = 5, // Maps to Idx 16
    box_of_zst = 6, // Maps to Idx 17
    list_of_zst = 7, // Maps to Idx 18
};

/// The union portion of the Scalar packed tagged union.
///
/// Some scalars have extra information associated with them,
/// such as the precision of a particular int or frac. This union
/// stores that extra information.
pub const ScalarUnion = packed union {
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    bool: void,
    str: void,
    opaque_ptr: void,
    box_of_zst: void,
    list_of_zst: void,
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
        // We intentionally build the layout store to allow a Var to translate directly into the
        // corresponding Layout in the layout store, so use the same number of bits as Var.
        .bits = @bitSizeOf(types.Var),
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
    //
    // TODO we need to have Var also have these same sentinel values, in the same order!

    // ints
    u8 = 1,
    i8 = 2,
    u16 = 3,
    i16 = 4,
    u32 = 5,
    i32 = 6,
    u64 = 7,
    i64 = 8,
    u128 = 9,
    i128 = 10,

    // fracs
    f32 = 11,
    f64 = 12,
    dec = 13,

    // others
    bool = 14,
    str = 15,
    opaque_ptr = 16,
    box_of_zst = 17,
    list_of_zst = 18,

    // Regular indices start from here.
    // num_scalars in store.zig must refer to how many variants we had up to this point.
    _,

    /// Get the sentinel Idx for a given scalar type using pure arithmetic - no branches!
    /// This relies on the careful ordering of ScalarTag and Idx enum values.
    pub fn fromScalar(scalar: Scalar) Idx {
        // TODO this arithmetic is wrong now! need to reorder this based on the new ordering.
        // Map scalar to idx using pure arithmetic:
        // opaque_ptr (tag 0) -> 0
        // bool (tag 1) -> 1
        // str (tag 2) -> 2
        // int (tag 3) with precision p -> 3 + p
        // frac (tag 4) with precision p -> 13 + (p - 2) = 11 + p

        // In the packed struct, ScalarData comes first (4 bits), then ScalarTag (3 bits)
        // For numeric types (int/frac), the precision enum is stored in ScalarData
        // For void types (bool/str/opaque_ptr), ScalarData is 0

        const tag = @intFromEnum(scalar.tag);

        // Get the precision bits directly from the packed representation
        // This works because in a packed union, all fields start at bit 0
        const scalar_bits = @as(u7, @bitCast(scalar));
        const precision = scalar_bits & 0xF; // Lower 4 bits contain precision for numeric types

        // Create masks for different tag ranges
        // is_numeric: 1 when tag >= 3, else 0
        const is_numeric = @as(u7, @intFromBool(tag >= 3));
        const is_frac = @as(u7, @intFromBool(scalar.tag == .frac));

        // Calculate the base index
        // For non-numeric (tag 0-2): base_idx = tag
        // For int (tag 3): base_idx = 3
        // For frac (tag 4): base_idx = 11 (to map to indices 13-15)
        const base_idx = tag + ((is_frac * 8) - is_frac); // Could also do (is_frac * 7) but * 8 can become a bit shift

        // Calculate the final index
        // For non-numeric: idx = base_idx (precision is 0)
        // For numeric: idx = base_idx + precision
        return @enumFromInt(base_idx + (is_numeric * precision));
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
    for ([_]types.Num.Frac.Precision{ .f32, .f64 }) |precision| {
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
