//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const target = @import("../base/target.zig");

/// Tag for Layout variants
pub const LayoutTag = enum(u5) {
    int = 0,
    frac = 1,
    str = 2,
    box = 3,
    box_of_zst = 4,
    list = 5,
    list_of_zst = 6,
    host_opaque = 7,
    record = 8,
    // Note: this is intentionally u5 in anticipation of needing more than 16 variants
    // in the future, even though we don't have that many yet.
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 32;

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
/// The largest variant must fit in 27 bits to leave room for the 5-bit tag
pub const LayoutUnion = packed union {
    int: types.Num.Int.Precision,
    frac: types.Num.Frac.Precision,
    str: void,
    box: Idx,
    box_of_zst: void,
    list: Idx,
    list_of_zst: void,
    host_opaque: void,
    record: RecordLayout,
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
pub const Layout = packed struct {
    data: LayoutUnion,
    tag: LayoutTag,

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self.tag) {
            .int => self.data.int.alignment(),
            .frac => self.data.frac.alignment(),
            .record => self.data.record.alignment,
            .str, .box, .box_of_zst, .list, .list_of_zst, .host_opaque => target_usize.alignment(),
        };
    }

    /// int layout with the given precision
    /// Create an int layout with the given precision
    pub fn int(precision: types.Num.Int.Precision) Layout {
        return Layout{ .data = .{ .int = precision }, .tag = .int };
    }

    /// Create a frac layout with the given precision
    pub fn frac(precision: types.Num.Frac.Precision) Layout {
        return Layout{ .data = .{ .frac = precision }, .tag = .frac };
    }

    /// str layout
    pub fn str() Layout {
        return Layout{ .data = .{ .str = {} }, .tag = .str };
    }

    /// box layout with the given element layout
    pub fn box(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .box = elem_idx }, .tag = .box };
    }

    /// box of zero-sized type layout (e.g. Box({}))
    pub fn boxOfZst() Layout {
        return Layout{ .data = .{ .box_of_zst = {} }, .tag = .box_of_zst };
    }

    /// Create a list layout with the given element layout index
    pub fn list(elem_idx: Idx) Layout {
        return Layout{ .data = .{ .list = elem_idx }, .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return Layout{ .data = .{ .list_of_zst = {} }, .tag = .list_of_zst };
    }

    /// host opaque layout (the void* that we pass to the host for e.g. flex vars)
    pub fn hostOpaque() Layout {
        return Layout{ .data = .{ .host_opaque = {} }, .tag = .host_opaque };
    }

    /// record layout with the given alignment and RecordIdx
    pub fn record(record_alignment: std.mem.Alignment, idx: RecordIdx) Layout {
        return Layout{ .data = .{ .record = .{ .alignment = record_alignment, .idx = idx } }, .tag = .record };
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

test "Size of Layout type" {
    // The Layout should have small size since it's used a ton, so avoid letting this number increase!
    try std.testing.expectEqual(@bitSizeOf(Layout), layout_bit_size);
}

test "Layout.alignment() - number types" {
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
    }
}

test "Layout.alignment() - types containing pointers" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(target_usize.alignment(), Layout.str().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.box(.{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.boxOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.list(.{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.listOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.hostOpaque().alignment(target_usize));
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
