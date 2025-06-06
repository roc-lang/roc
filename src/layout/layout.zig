//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const target = @import("../base/target.zig");

/// Index into a Layout Store
pub const Idx = packed struct(u24) {
    idx: u24,
};

/// The runtime memory layout of a Roc value.
///
/// When a Roc type gets converted to a Layout, all zero-sized types
/// (e.g. empty records, empty tag unions, single-tag unions) must be
/// dropped, because zero-sized values don't exist at runtime.
///
/// Once a type has been converted to a layout, there is no longer any
/// distinction between nominal and structural types, there's just memory.
/// Records and tuples have both been flattened (to remove their extensions)
/// and converted into structs whose fields are sorted by alignment and then
/// alphabetically by field name (or numerically by tuple field index).
pub const Layout = union(enum) {
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    str,
    box: Idx,
    box_of_zst, // A Box of a zero-sized type, e.g. a Box({}) - this can come up, so we need an implementation for it.
    list: Idx,
    list_of_zst, // A List of a zero-sized type, e.g. a Box({}) - this can come up, so we need an implementation for it.
    host_opaque,
    record: RecordLayout,
    // TODO add `single_field_record: Idx` and `single_tag_union: Idx`

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self) {
            .int => |precision| precision.alignment(),
            .frac => |precision| precision.alignment(),
            .str, .box, .box_of_zst, .list, .list_of_zst, .host_opaque => target_usize.alignment(),
            .record => |rec| rec.alignment,
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
pub const RecordIdx = packed struct(u24) {
    idx: u24,
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

test "Size of Layout type" {
    // The Layout should have small size since it's used frequently, so avoid letting this number increase!
    // With the Record data moved to separate storage, we've reduced from 20 bytes to 12 bytes
    try std.testing.expectEqual(@sizeOf(Layout), 8);
}

test "Layout.alignment() - number types" {
    const testing = std.testing;

    // ints
    const u8_layout = Layout{ .int = .u8 };
    const i8_layout = Layout{ .int = .i8 };
    const u16_layout = Layout{ .int = .u16 };
    const i16_layout = Layout{ .int = .i16 };
    const u32_layout = Layout{ .int = .u32 };
    const i32_layout = Layout{ .int = .i32 };
    const u64_layout = Layout{ .int = .u64 };
    const i64_layout = Layout{ .int = .i64 };
    const u128_layout = Layout{ .int = .u128 };
    const i128_layout = Layout{ .int = .i128 };

    // floats
    const f32_layout = Layout{ .frac = .f32 };
    const f64_layout = Layout{ .frac = .f64 };
    const dec_layout = Layout{ .frac = .dec };

    for (target.TargetUsize.all()) |target_usize| {
        // Alignment
        try testing.expectEqual(std.mem.Alignment.@"1", u8_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"1", i8_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", u16_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", i16_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", u32_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", i32_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", u64_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", i64_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", u128_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", i128_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", f32_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", f64_layout.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", dec_layout.alignment(target_usize));
    }
}

test "Layout.alignment() - types containing pointers" {
    const testing = std.testing;

    const str_layout: Layout = .str;
    const host_opaque_layout: Layout = .host_opaque;
    const box_layout: Layout = .{ .box = .{ .idx = 0 } };
    const box_zero_layout: Layout = .box_of_zst;
    const list_layout: Layout = .{ .list = .{ .idx = 0 } };
    const list_zero_layout: Layout = .list_of_zst;

    for (target.TargetUsize.all()) |target_usize| {
        // Alignment
        try testing.expectEqual(target_usize.alignment(), str_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), box_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), box_zero_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), list_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), list_zero_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), host_opaque_layout.alignment(target_usize));
    }
}

test "Layout.alignment() - record types" {
    const testing = std.testing;

    const record_align4 = Layout{ .record = .{
        .alignment = std.mem.Alignment.@"4",
        .idx = .{ .idx = 0 },
    } };

    const record_align16 = Layout{ .record = .{
        .alignment = std.mem.Alignment.@"16",
        .idx = .{ .idx = 1 },
    } };

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), record_align4.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), record_align16.alignment(target_usize));
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
