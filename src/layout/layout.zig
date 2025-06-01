//! Memory layout representation for Roc types.
//! Converts high-level type information into concrete memory layouts used for code generation.

const std = @import("std");
const types = @import("../types/types.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

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
    func: Func, // TODO how does the closure fit into here?
    tagged_union: TagUnion,

    /// Get the alignment of this layout in bytes.
    pub fn alignment(self: Layout, usize_alignment: Alignment) Alignment {
        return switch (self) {
            .int => |precision| Alignment.fromLog2(precision.alignmentLog2()),
            .frac => |precision| Alignment.fromLog2(precision.alignmentLog2()),
            .str, .box, .box_zero_sized, .list, .list_zero_sized, .host_opaque => usize_alignment,
            .record => |rec| rec.alignment,
            .tuple => @panic("TODO: implement tuple alignment"),
            .func => @panic("TODO: implement func alignment"),
            .tagged_union => @panic("TODO: implement tagged_union alignment"),
        };
    }

    /// Get the size of this layout in bytes.
    /// The usize_bytes parameter refers to how many bytes we should treat
    /// `usize` as, based on the target we're building for.
    pub fn size(self: Layout, usize_bytes: u32) u32 {
        return switch (self) {
            .int => |precision| precision.size(),
            .frac => |precision| precision.size(),
            .host_opaque => usize_bytes, // a void* pointer
            .box, .box_zero_sized => usize_bytes, // a Box is just a pointer to refcounted memory
            .str, .list, .list_zero_sized => usize_bytes * 3, // TODO: get this from RocStr.zig and RocList.zig
            .record => |rec| rec.size.toBytes(),
            .tuple => @panic("TODO: implement tuple size"),
            .func => @panic("TODO: implement func size"),
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
    alignment: Alignment,
    /// Size of the record, in bytes
    size: Size,

    pub fn getFields(self: Record) RecordField.SafeMultiList.Range {
        return self.fields.toRange(RecordField.SafeMultiList.Idx);
    }
};

/// Tuple layout (ordered collection of fields)
pub const Tuple = struct {
    // TODO: implement
};

/// Function layout
pub const Func = struct {
    // TODO: implement
};

/// Tagged union layout
pub const TagUnion = struct {
    // TODO: implement
};

/// Memory alignment, efficiently stored in base-2 encoding.
///
/// (e.g. an alignment of 8 bytes is stored as `3` in memory, since 2^3 == 8)
pub const Alignment = struct {
    log2_value: u4,

    /// Create an Alignment from the actual alignment value in bytes.
    /// The alignment must be a power of 2.
    pub fn fromBytes(bytes: u32) Alignment {
        std.debug.assert(bytes > 0 and (bytes & (bytes - 1)) == 0); // Must be power of 2
        return .{ .log2_value = @intCast(std.math.log2_int(u32, bytes)) };
    }

    /// Create an Alignment from the log2 representation.
    pub fn fromLog2(log2_value: u4) Alignment {
        return .{ .log2_value = log2_value };
    }

    /// Get the alignment value in bytes.
    pub fn toBytes(self: Alignment) u32 {
        return @as(u32, 1) << @intCast(self.log2_value);
    }

    /// Get the maximum of two alignments.
    pub fn max(a: Alignment, b: Alignment) Alignment {
        return .{ .log2_value = @max(a.log2_value, b.log2_value) };
    }

    /// Check if two alignments are equal.
    pub fn eql(a: Alignment, b: Alignment) bool {
        return a.log2_value == b.log2_value;
    }
};

/// Size representation in bytes.
pub const Size = struct {
    bytes: u32,

    /// Create a Size from the actual size value in bytes.
    pub fn fromBytes(bytes: u32) Size {
        return .{ .bytes = bytes };
    }

    /// Get the size value in bytes.
    pub fn toBytes(self: Size) u32 {
        return self.bytes;
    }

    /// Get the maximum of two sizes.
    pub fn max(a: Size, b: Size) Size {
        return .{ .bytes = @max(a.bytes, b.bytes) };
    }

    /// Add two sizes together.
    pub fn add(a: Size, b: Size) Size {
        return .{ .bytes = a.bytes + b.bytes };
    }

    /// Check if two sizes are equal.
    pub fn eql(a: Size, b: Size) bool {
        return a.bytes == b.bytes;
    }

    /// Round up to the nearest multiple of alignment.
    pub fn alignForward(self: Size, alignment: Alignment) Size {
        const align_bytes = alignment.toBytes();
        // Check for potential overflow
        if (self.bytes > std.math.maxInt(u32) - align_bytes + 1) {
            // Already at or near max, can't align forward
            return self;
        }
        const aligned = (self.bytes + align_bytes - 1) & ~(align_bytes - 1);
        return .{ .bytes = aligned };
    }
};

test "Alignment - fromBytes and toBytes" {
    const testing = std.testing;

    // Test common alignments
    const align1 = Alignment.fromBytes(1);
    try testing.expectEqual(@as(u32, 1), align1.toBytes());
    try testing.expectEqual(@as(u4, 0), align1.log2_value);

    const align4 = Alignment.fromBytes(4);
    try testing.expectEqual(@as(u32, 4), align4.toBytes());
    try testing.expectEqual(@as(u4, 2), align4.log2_value);

    const align8 = Alignment.fromBytes(8);
    try testing.expectEqual(@as(u32, 8), align8.toBytes());
    try testing.expectEqual(@as(u4, 3), align8.log2_value);

    const align16 = Alignment.fromBytes(16);
    try testing.expectEqual(@as(u32, 16), align16.toBytes());
    try testing.expectEqual(@as(u4, 4), align16.log2_value);

    const align256 = Alignment.fromBytes(256);
    try testing.expectEqual(@as(u32, 256), align256.toBytes());
    try testing.expectEqual(@as(u4, 8), align256.log2_value);
}

test "Alignment - fromLog2" {
    const testing = std.testing;

    const align1 = Alignment.fromLog2(0);
    try testing.expectEqual(@as(u32, 1), align1.toBytes());

    const align4 = Alignment.fromLog2(2);
    try testing.expectEqual(@as(u32, 4), align4.toBytes());

    const align8 = Alignment.fromLog2(3);
    try testing.expectEqual(@as(u32, 8), align8.toBytes());
}

test "Alignment - max" {
    const testing = std.testing;

    const align1 = Alignment.fromBytes(1);
    const align4 = Alignment.fromBytes(4);
    const align8 = Alignment.fromBytes(8);

    const max1_4 = Alignment.max(align1, align4);
    try testing.expectEqual(@as(u32, 4), max1_4.toBytes());

    const max4_8 = Alignment.max(align4, align8);
    try testing.expectEqual(@as(u32, 8), max4_8.toBytes());

    const max8_1 = Alignment.max(align8, align1);
    try testing.expectEqual(@as(u32, 8), max8_1.toBytes());

    const max4_4 = Alignment.max(align4, align4);
    try testing.expectEqual(@as(u32, 4), max4_4.toBytes());
}

test "Alignment - eql" {
    const testing = std.testing;

    const align1a = Alignment.fromBytes(1);
    const align1b = Alignment.fromLog2(0);
    const align4 = Alignment.fromBytes(4);

    try testing.expect(Alignment.eql(align1a, align1b));
    try testing.expect(Alignment.eql(align4, align4));
    try testing.expect(!Alignment.eql(align1a, align4));
}

test "Size - basic operations" {
    const testing = std.testing;

    // Test creation and retrieval
    const size5 = Size.fromBytes(5);
    try testing.expectEqual(@as(u32, 5), size5.toBytes());

    const size12 = Size.fromBytes(12);
    try testing.expectEqual(@as(u32, 12), size12.toBytes());

    // Test add
    const size17 = Size.add(size5, size12);
    try testing.expectEqual(@as(u32, 17), size17.toBytes());

    // Test alignForward
    const align4 = Alignment.fromBytes(4);
    const size5_aligned = size5.alignForward(align4);
    try testing.expectEqual(@as(u32, 8), size5_aligned.toBytes()); // 5 rounds up to 8

    const size12_aligned = size12.alignForward(align4);
    try testing.expectEqual(@as(u32, 12), size12_aligned.toBytes()); // 12 is already aligned

    const align8 = Alignment.fromBytes(8);
    const size5_aligned8 = size5.alignForward(align8);
    try testing.expectEqual(@as(u32, 8), size5_aligned8.toBytes());

    const size12_aligned8 = size12.alignForward(align8);
    try testing.expectEqual(@as(u32, 16), size12_aligned8.toBytes());
}
test "Size - max" {
    const testing = std.testing;

    const size4 = Size.fromBytes(4);
    const size16 = Size.fromBytes(16);
    const size64 = Size.fromBytes(64);

    const max4_16 = Size.max(size4, size16);
    try testing.expectEqual(@as(u32, 16), max4_16.toBytes());

    const max16_64 = Size.max(size16, size64);
    try testing.expectEqual(@as(u32, 64), max16_64.toBytes());

    const max64_4 = Size.max(size64, size4);
    try testing.expectEqual(@as(u32, 64), max64_4.toBytes());
}

test "Size - eql" {
    const testing = std.testing;

    const size8a = Size.fromBytes(8);
    const size8b = Size.fromBytes(8);
    const size16 = Size.fromBytes(16);

    try testing.expect(Size.eql(size8a, size8b));
    try testing.expect(Size.eql(size16, size16));
    try testing.expect(!Size.eql(size8a, size16));
}

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

    // Test integer alignments
    const u8_layout = Layout{ .int = .u8 };
    const u16_layout = Layout{ .int = .u16 };
    const u32_layout = Layout{ .int = .u32 };
    const u64_layout = Layout{ .int = .u64 };
    const u128_layout = Layout{ .int = .u128 };

    const usize_alignment = Alignment.fromBytes(@alignOf(usize));

    try testing.expectEqual(Alignment.fromBytes(1), u8_layout.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(2), u16_layout.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(4), u32_layout.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(8), u64_layout.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(16), u128_layout.alignment(usize_alignment));

    // Test floating point alignments
    const f32_layout = Layout{ .frac = .f32 };
    const f64_layout = Layout{ .frac = .f64 };

    try testing.expectEqual(Alignment.fromBytes(4), f32_layout.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(8), f64_layout.alignment(usize_alignment));
}

test "Layout.alignment() - container types" {
    const testing = std.testing;

    const usize_alignment = Alignment.fromBytes(@alignOf(usize));

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

    const usize_alignment = Alignment.fromBytes(@alignOf(usize));

    // Test record with alignment 4
    const record_align4 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 2 },
        .alignment = Alignment.fromBytes(4),
        .size = Size.fromBytes(8),
    } };

    // Test record with alignment 16
    const record_align16 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 1 },
        .alignment = Alignment.fromBytes(16),
        .size = Size.fromBytes(16),
    } };

    try testing.expectEqual(Alignment.fromBytes(4), record_align4.alignment(usize_alignment));
    try testing.expectEqual(Alignment.fromBytes(16), record_align16.alignment(usize_alignment));
}

test "Layout.size() - primitive types" {
    const testing = std.testing;

    // Test integer sizes
    const u8_layout = Layout{ .int = .u8 };
    const u16_layout = Layout{ .int = .u16 };
    const u32_layout = Layout{ .int = .u32 };
    const u64_layout = Layout{ .int = .u64 };
    const u128_layout = Layout{ .int = .u128 };

    const usize_bytes: u32 = @sizeOf(usize);

    try testing.expectEqual(@as(u32, 1), u8_layout.size(usize_bytes));
    try testing.expectEqual(@as(u32, 2), u16_layout.size(usize_bytes));
    try testing.expectEqual(@as(u32, 4), u32_layout.size(usize_bytes));
    try testing.expectEqual(@as(u32, 8), u64_layout.size(usize_bytes));
    try testing.expectEqual(@as(u32, 16), u128_layout.size(usize_bytes));

    // Test floating point sizes
    const f32_layout = Layout{ .frac = .f32 };
    const f64_layout = Layout{ .frac = .f64 };

    try testing.expectEqual(@as(u32, 4), f32_layout.size(usize_bytes));
    try testing.expectEqual(@as(u32, 8), f64_layout.size(usize_bytes));
}

test "Layout.size() - container types" {
    const testing = std.testing;

    const usize_bytes: u32 = @sizeOf(usize);

    // Test host_opaque size (should be usize)
    const host_opaque_layout = Layout{ .host_opaque = {} };
    try testing.expectEqual(usize_bytes, host_opaque_layout.size(usize_bytes));

    // Test box sizes (should be usize - pointer size)
    const box_layout = Layout{ .box = .{ .idx = 0 } };
    const box_zero_layout = Layout{ .box_zero_sized = {} };
    try testing.expectEqual(usize_bytes, box_layout.size(usize_bytes));
    try testing.expectEqual(usize_bytes, box_zero_layout.size(usize_bytes));

    // Test str and list sizes (should be 3 * usize)
    const str_layout = Layout{ .str = {} };
    const list_layout = Layout{ .list = .{ .idx = 0 } };
    const list_zero_layout = Layout{ .list_zero_sized = {} };
    try testing.expectEqual(usize_bytes * 3, str_layout.size(usize_bytes));
    try testing.expectEqual(usize_bytes * 3, list_layout.size(usize_bytes));
    try testing.expectEqual(usize_bytes * 3, list_zero_layout.size(usize_bytes));
}

test "Layout.size() - record types" {
    const testing = std.testing;

    const usize_bytes: u32 = @sizeOf(usize);

    // Test record with size 8
    const record_8 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 2 },
        .alignment = Alignment.fromBytes(4),
        .size = Size.fromBytes(8),
    } };

    // Test record with size 32
    const record_32 = Layout{ .record = .{
        .fields = .{ .start = 0, .count = 4 },
        .alignment = Alignment.fromBytes(8),
        .size = Size.fromBytes(32),
    } };

    try testing.expectEqual(@as(u32, 8), record_8.size(usize_bytes));
    try testing.expectEqual(@as(u32, 32), record_32.size(usize_bytes));
}

test "Alignment - edge cases" {
    const testing = std.testing;

    // Test minimum alignment
    const align1 = Alignment.fromBytes(1);
    try testing.expectEqual(@as(u32, 1), align1.toBytes());

    // Test maximum valid alignment for u4 (2^15 = 32768)
    const align_max = Alignment.fromBytes(32768);
    try testing.expectEqual(@as(u32, 32768), align_max.toBytes());

    // Test all valid power-of-2 alignments
    var i: u4 = 0;
    while (i < 15) : (i += 1) {
        const bytes = @as(u32, 1) << @intCast(i);
        const alignment = Alignment.fromBytes(bytes);
        try testing.expectEqual(bytes, alignment.toBytes());
        try testing.expectEqual(i, alignment.log2_value);
    }
    // Test the last value separately to avoid overflow
    const bytes_15 = @as(u32, 1) << 15;
    const alignment_15 = Alignment.fromBytes(bytes_15);
    try testing.expectEqual(bytes_15, alignment_15.toBytes());
    try testing.expectEqual(@as(u4, 15), alignment_15.log2_value);
}

test "Size - edge cases and overflow protection" {
    const testing = std.testing;

    // Test zero size
    const size0 = Size.fromBytes(0);
    try testing.expectEqual(@as(u32, 0), size0.toBytes());

    // Test maximum size
    const size_max = Size.fromBytes(std.math.maxInt(u32));
    try testing.expectEqual(std.math.maxInt(u32), size_max.toBytes());

    // Test alignForward with edge cases
    const align16 = Alignment.fromBytes(16);

    // Already aligned
    const size16 = Size.fromBytes(16);
    try testing.expectEqual(@as(u32, 16), size16.alignForward(align16).toBytes());

    // One byte off
    const size17 = Size.fromBytes(17);
    try testing.expectEqual(@as(u32, 32), size17.alignForward(align16).toBytes());

    // Test alignForward with large values (but not so large as to overflow)
    const size_large = Size.fromBytes(0xFFFFFF00); // Large but safe value
    const aligned_large = size_large.alignForward(align16);
    try testing.expectEqual(@as(u32, 0xFFFFFF00), aligned_large.toBytes());
}

test "Record.getFields() method" {
    const testing = std.testing;

    // Create a Record layout
    const record = Record{
        .fields = .{ .start = 10, .count = 5 },
        .alignment = Alignment.fromBytes(8),
        .size = Size.fromBytes(40),
    };

    // Test getFields() method
    const fields_range = record.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(fields_range.end));
}
