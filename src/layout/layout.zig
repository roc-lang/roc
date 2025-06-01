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
