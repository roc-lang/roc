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
    tuple: Tuple,
    closure: Closure,
    tagged_union: TagUnion,

    /// Get the alignment of this layout in bytes.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self) {
            .int => |precision| precision.alignment(),
            .frac => |precision| precision.alignment(),
            .str, .box, .box_zero_sized, .list, .list_zero_sized, .host_opaque => target_usize.alignment(),
            .record => |rec| rec.alignment,
            .tuple => @panic("TODO: implement tuple alignment"),
            .closure => @panic("TODO: implement closure alignment"),
            .tagged_union => @panic("TODO: implement tagged_union alignment"),
        };
    }

    /// Get the size of this layout in bytes.
    /// The usize_bytes parameter refers to how many bytes we should treat
    /// `usize` as, based on the target we're building for.
    pub fn size(self: Layout, target_usize: target.TargetUsize) u32 {
        return switch (self) {
            .int => |precision| @as(u32, @intCast(precision.size())),
            .frac => |precision| @as(u32, @intCast(precision.size())),
            .host_opaque => @as(u32, @intCast(target_usize.size())), // a void* pointer
            .box, .box_zero_sized => @as(u32, @intCast(target_usize.size())), // a Box is just a pointer to refcounted memory
            .str, .list, .list_zero_sized => @as(u32, @intCast(target_usize.size())) * 3, // TODO: get this from RocStr.zig and RocList.zig
            .record => |rec| rec.size,
            .tuple => @panic("TODO: implement tuple size"),
            .closure => @panic("TODO: implement closure size"),
            .tagged_union => @panic("TODO: implement tagged_union size"),
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

/// Tuple layout (ordered collection of fields)
pub const Tuple = struct {
    // TODO: implement
};

/// Closure layout
pub const Closure = struct {
    // TODO: implement
};

/// Tagged union layout
pub const TagUnion = struct {
    // TODO: implement
};

test "Layout size in bytes" {
    const testing = std.testing;

    // Test the size of the Layout tagged union
    const layout_size = @sizeOf(Layout);

    // The Layout should be reasonably small since it's used frequently
    // A typical tagged union in Zig will be the size of the largest variant plus tag overhead
    try testing.expect(layout_size <= 20); // Reasonable upper bound
}

test "Layout.alignment() - primitive types" {
    const testing = std.testing;

    const u8_layout = Layout{ .int = .u8 };
    const u16_layout = Layout{ .int = .u16 };
    const u32_layout = Layout{ .int = .u32 };
    const u64_layout = Layout{ .int = .u64 };
    const u128_layout = Layout{ .int = .u128 };
    const f32_layout = Layout{ .frac = .f32 };
    const f64_layout = Layout{ .frac = .f64 };
    const dec_layout = Layout{ .frac = .dec };
    const str_layout: Layout = .str;

    const usize_alignment = std.mem.Alignment.fromByteUnits(@alignOf(usize));

    try testing.expectEqual(std.mem.Alignment.@"1", u8_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"2", u16_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"4", u32_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"8", u64_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"16", u128_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"4", f32_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"8", f64_layout.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.@"16", dec_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, str_layout.alignment(usize_alignment));
}

test "Layout.alignment() - container types" {
    const testing = std.testing;

    const usize_alignment = std.mem.Alignment.fromByteUnits(@alignOf(usize));

    // Test str, box, list, and host_opaque alignments
    const str_layout = Layout{ .str = {} };
    const box_layout = Layout{ .box = .{ .idx = 0 } };
    const box_zero_layout = Layout{ .box_zero_sized = {} };
    const list_layout = Layout{ .list = .{ .idx = 0 } };
    const list_zero_layout = Layout{ .list_zero_sized = {} };
    const host_opaque_layout = Layout{ .host_opaque = {} };

    try testing.expectEqual(usize_alignment, str_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, box_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, box_zero_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, list_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, list_zero_layout.alignment(usize_alignment));
    try testing.expectEqual(usize_alignment, host_opaque_layout.alignment(usize_alignment));
}

test "Layout.alignment() - record types" {
    const testing = std.testing;

    const usize_alignment = std.mem.Alignment.fromByteUnits(@alignOf(usize));

    // Test record with alignment 4
    const record_align4 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 2 },
        .alignment = std.mem.Alignment.@"4",
        .size = 8,
    } };

    // Test record with alignment 16
    const record_align16 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 1 },
        .alignment = std.mem.Alignment.@"16",
        .size = 16,
    } };

    try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), record_align4.alignment(usize_alignment));
    try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), record_align16.alignment(usize_alignment));
}

test "Layout.size() - primitive types" {
    const testing = std.testing;

    // integers
    const u8_layout = Layout{ .int = .u8 };
    const u16_layout = Layout{ .int = .u16 };
    const u32_layout = Layout{ .int = .u32 };
    const u64_layout = Layout{ .int = .u64 };
    const u128_layout = Layout{ .int = .u128 };

    const native: target.TargetUsize = target.TargetUsize.native;

    try testing.expectEqual(@as(u32, 1), u8_layout.size(native));
    try testing.expectEqual(@as(u32, 2), u16_layout.size(native));
    try testing.expectEqual(@as(u32, 4), u32_layout.size(native));
    try testing.expectEqual(@as(u32, 8), u64_layout.size(native));
    try testing.expectEqual(@as(u32, 16), u128_layout.size(native));

    // floats
    const f32_layout = Layout{ .frac = .f32 };
    const f64_layout = Layout{ .frac = .f64 };
    const dec_layout = Layout{ .frac = .dec };

    try testing.expectEqual(@as(u32, 4), f32_layout.size(native));
    try testing.expectEqual(@as(u32, 8), f64_layout.size(native));
    try testing.expectEqual(@as(u32, 16), dec_layout.size(native));

    // strings
    const str_layout: Layout = .str;

    try testing.expectEqual(@as(u32, @sizeOf(usize) * 3), str_layout.size(native));
}

test "Layout.size() - container types" {
    const testing = std.testing;

    const usize_bytes: u32 = @sizeOf(usize);
    const target_usize = target.TargetUsize.native;

    // Test host_opaque size (should be usize)
    const host_opaque_layout = Layout{ .host_opaque = {} };
    try testing.expectEqual(usize_bytes, host_opaque_layout.size(target_usize));

    // Test box sizes (should be usize - pointer size)
    const box_layout = Layout{ .box = .{ .idx = 0 } };
    const box_zero_layout = Layout{ .box_zero_sized = {} };
    try testing.expectEqual(usize_bytes, box_layout.size(target_usize));
    try testing.expectEqual(usize_bytes, box_zero_layout.size(target_usize));

    // Test str and list sizes (should be 3 * usize)
    const str_layout = Layout{ .str = {} };
    const list_layout = Layout{ .list = .{ .idx = 0 } };
    const list_zero_layout = Layout{ .list_zero_sized = {} };
    try testing.expectEqual(usize_bytes * 3, str_layout.size(target_usize));
    try testing.expectEqual(usize_bytes * 3, list_layout.size(target_usize));
    try testing.expectEqual(usize_bytes * 3, list_zero_layout.size(target_usize));
}

test "Record.getFields()" {
    const testing = std.testing;

    // Create a Record layout
    const record = Record{
        .fields = .{ .start = 10, .count = 5 },
        .alignment = std.mem.Alignment.fromByteUnits(8),
        .size = 40,
    };

    // Test getFields() method
    const fields_range = record.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(fields_range.end));
}
