//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const target = @import("../base/target.zig");

/// Index into a Layout Store
pub const Idx = packed struct(u32) {
    idx: u32,
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
    box_zero_sized, // e.g. a Box({}) - this can come up, so we need a special implementation for it.
    list: Idx,
    list_zero_sized, // e.g. a List({}) - this can come up, so we need to make a special implementation for it.
    host_opaque,
    record: Record,

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self) {
            .int => |precision| precision.alignment(),
            .frac => |precision| precision.alignment(),
            .str, .box, .box_zero_sized, .list, .list_zero_sized, .host_opaque => target_usize.alignment(),
            .record => |rec| rec.alignment,
        };
    }

    /// This layout's size in bytes, given a particular target usize.
    pub fn size(self: Layout, target_usize: target.TargetUsize) u32 {
        return switch (self) {
            .int => |precision| @as(u32, @intCast(precision.size())),
            .frac => |precision| @as(u32, @intCast(precision.size())),
            .host_opaque => @as(u32, @intCast(target_usize.size())), // a void* pointer
            .box, .box_zero_sized => @as(u32, @intCast(target_usize.size())), // a Box is just a pointer to refcounted memory
            .str, .list, .list_zero_sized => @as(u32, @intCast(target_usize.size())) * 3, // TODO: get this from RocStr.zig and RocList.zig
            .record => |rec| rec.size,
        };
    }
};

/// Record field layout
pub const RecordField = struct {
    /// The name of the field
    name: Ident.Idx,
    /// The layout of the field's value
    layout: Idx,

    /// A SafeMultiList for storing record fields
    pub const SafeMultiList = collections.SafeMultiList(RecordField);
};

/// Record layout
pub const Record = struct {
    fields: collections.NonEmptyRange,
    /// Alignment of the record
    alignment: std.mem.Alignment,
    /// Size of the record, in bytes
    size: u32,

    pub fn getFields(self: Record) RecordField.SafeMultiList.Range {
        return self.fields.toRange(RecordField.SafeMultiList.Idx);
    }
};

test "Size of Layout type" {
    // The Layout should have small size since it's used frequently, so avoid letting this number increase!
    try std.testing.expect(@sizeOf(Layout) <= 20);
}

test "Layout.size() and Layout.alignment() - number types" {
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

        // Size
        try testing.expectEqual(1, u8_layout.size(target_usize));
        try testing.expectEqual(1, i8_layout.size(target_usize));
        try testing.expectEqual(2, u16_layout.size(target_usize));
        try testing.expectEqual(2, i16_layout.size(target_usize));
        try testing.expectEqual(4, u32_layout.size(target_usize));
        try testing.expectEqual(4, i32_layout.size(target_usize));
        try testing.expectEqual(8, u64_layout.size(target_usize));
        try testing.expectEqual(8, i64_layout.size(target_usize));
        try testing.expectEqual(16, u128_layout.size(target_usize));
        try testing.expectEqual(16, i128_layout.size(target_usize));
        try testing.expectEqual(4, f32_layout.size(target_usize));
        try testing.expectEqual(8, f64_layout.size(target_usize));
        try testing.expectEqual(16, dec_layout.size(target_usize));
    }
}

test "Layout.size() and Layout.alignment()- types containing pointers" {
    const testing = std.testing;

    const str_layout: Layout = .str;
    const host_opaque_layout: Layout = .host_opaque;
    const box_layout: Layout = .{ .box = .{ .idx = 0 } };
    const box_zero_layout: Layout = .box_zero_sized;
    const list_layout: Layout = .{ .list = .{ .idx = 0 } };
    const list_zero_layout: Layout = .list_zero_sized;

    for (target.TargetUsize.all()) |target_usize| {
        // Alignment
        try testing.expectEqual(target_usize.alignment(), str_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), box_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), box_zero_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), list_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), list_zero_layout.alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), host_opaque_layout.alignment(target_usize));

        // Size
        try testing.expectEqual(target_usize.size(), host_opaque_layout.size(target_usize));
        try testing.expectEqual(target_usize.size(), box_layout.size(target_usize));
        try testing.expectEqual(target_usize.size(), box_zero_layout.size(target_usize));
        // TODO do something like (target_usize.size() * (@sizeOf(Str) / @sizeOf(usize))) once we have the builtins imported here
        try testing.expectEqual(target_usize.size() * 3, str_layout.size(target_usize));
        try testing.expectEqual(target_usize.size() * 3, list_layout.size(target_usize));
        try testing.expectEqual(target_usize.size() * 3, list_zero_layout.size(target_usize));
    }
}

test "Layout.alignment() - record types" {
    const testing = std.testing;

    const record_align4 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 2 },
        .alignment = std.mem.Alignment.@"4",
        .size = 8,
    } };

    const record_align16 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 1 },
        .alignment = std.mem.Alignment.@"16",
        .size = 16,
    } };

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), record_align4.alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), record_align16.alignment(target_usize));
    }
}

test "Record.getFields()" {
    const testing = std.testing;

    const record = Record{
        .fields = .{ .start = 10, .count = 5 },
        .alignment = std.mem.Alignment.fromByteUnits(8),
        .size = 40,
    };

    const fields_range = record.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(fields_range.end));
}
