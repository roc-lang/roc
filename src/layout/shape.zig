//! The arithmetic that turns field and variant sizes into a committed memory
//! shape: the alignment class (`SortKey`), struct measuring with C-style
//! padding, tag union discriminant width and placement, and (through
//! `field_order`) the structural field order.
//!
//! This file and `field_order.zig` depend only on `std`, so the layout store
//! and the Boxy runtime compile the same code: a descriptor the runtime
//! composes for a type has exactly the shape the store commits for it.

const std = @import("std");

pub const field_order = @import("field_order.zig");

/// Target-independent key for ordering aggregate fields by alignment.
///
/// This is the target-independent alignment class of a value: it both orders
/// fields and (given a pointer alignment) yields the actual alignment in
/// bytes. A pointer is its own class, sorting strictly between 4-byte and
/// 8-byte alignment—because a pointer is the only type whose real alignment
/// varies by target (4 bytes on a 32-bit target, 8 on a 64-bit one), its fixed
/// slot between `align_4` and `align_8` makes a record/tuple's field order
/// identical on both targets and lets the alignment be stored
/// target-independently and resolved to bytes only when a target is known
/// (see `alignment`). This is the foundation for caching layout across pointer
/// widths.
pub const SortKey = enum(u3) {
    align_1 = 0,
    align_2 = 1,
    align_4 = 2,
    pointer = 3,
    align_8 = 4,
    align_16 = 5,

    /// Sort key for a fixed (non-pointer) power-of-two alignment in bytes.
    pub fn fromAlignBytes(bytes: u64) SortKey {
        return switch (bytes) {
            1 => .align_1,
            2 => .align_2,
            4 => .align_4,
            8 => .align_8,
            16 => .align_16,
            else => unreachable, // alignments are powers of two up to 16
        };
    }

    /// The actual alignment for a target whose pointers have
    /// `pointer_alignment`. A `.pointer` resolves to that; every other class
    /// is target-independent.
    pub fn alignment(self: SortKey, pointer_alignment: std.mem.Alignment) std.mem.Alignment {
        return switch (self) {
            .align_1 => .@"1",
            .align_2 => .@"2",
            .align_4 => .@"4",
            .pointer => pointer_alignment,
            .align_8 => .@"8",
            .align_16 => .@"16",
        };
    }

    /// `alignment` in bytes, for a target whose pointers are `pointer_size`
    /// bytes (and as aligned).
    pub fn alignmentBytes(self: SortKey, pointer_size: u32) u32 {
        return @intCast(self.alignment(std.mem.Alignment.fromByteUnits(pointer_size)).toByteUnits());
    }

    /// The greater of two sort keys (used to fold an aggregate's children).
    pub fn max(a: SortKey, b: SortKey) SortKey {
        return if (@backingInt(a) >= @backingInt(b)) a else b;
    }

    /// Whether `a` sorts before `b` in the field order (higher key first).
    pub fn sortsBefore(a: SortKey, b: SortKey) bool {
        return @backingInt(a) > @backingInt(b);
    }
};

/// The discriminant width in bytes for a tag union of `variant_count`
/// variants.
///
/// A single-variant tag union has an implicit discriminant (the tag is
/// statically known), so it reserves zero discriminant bytes in memory.
pub fn discriminantSize(variant_count: usize) u8 {
    return if (variant_count <= 1) 0 else if (variant_count <= 256) 1 else if (variant_count <= 65536) 2 else if (variant_count <= (1 << 32)) 4 else 8;
}

/// The alignment of a discriminant of `size` bytes.
pub fn alignmentForDiscriminantSize(size: u8) std.mem.Alignment {
    return switch (size) {
        0 => .@"1",
        1 => .@"1",
        2 => .@"2",
        4 => .@"4",
        8 => .@"8",
        else => unreachable, // discriminant_size is 0, 1, 2, 4, or 8
    };
}

/// One struct field as measuring reads it, at one pointer width.
pub const StructFieldShape = struct {
    size: u32,
    /// The field's own alignment in bytes. A padding field's is ignored.
    alignment: u32,
    sort_key: SortKey,
    /// An unnamed nominal-record padding spacer: it occupies `size` bytes at
    /// alignment 1 and contributes neither alignment nor sort key.
    is_padding: bool = false,
};

/// A measured struct at one pointer width.
pub const StructMetrics = struct {
    /// The end of the last field rounded up to `alignment`.
    size: u32,
    /// The largest alignment of a non-padding field, at least 1.
    alignment: u32,
    /// The largest sort key of a non-padding field; `.align_1` when there is
    /// none.
    sort_key: SortKey,
};

/// Measures a struct one field at a time, in memory order.
pub const StructMeasure = struct {
    end: u32 = 0,
    max_alignment: u32 = 1,
    sort_key: SortKey = .align_1,

    /// Place the next field in memory order and return its offset.
    pub fn place(self: *StructMeasure, field: StructFieldShape) u32 {
        const field_alignment: u32 = if (field.is_padding) 1 else field.alignment;
        if (!field.is_padding) self.sort_key = self.sort_key.max(field.sort_key);
        self.max_alignment = @max(self.max_alignment, field_alignment);
        const offset = std.mem.alignForward(u32, self.end, field_alignment);
        self.end = offset + field.size;
        return offset;
    }

    pub fn finish(self: StructMeasure) StructMetrics {
        return .{
            .size = std.mem.alignForward(u32, self.end, self.max_alignment),
            .alignment = self.max_alignment,
            .sort_key = self.sort_key,
        };
    }
};

/// Measure the struct of `fields`, given in memory order, writing each
/// field's offset to `out_offsets`.
pub fn measureStruct(fields: []const StructFieldShape, out_offsets: []u32) StructMetrics {
    std.debug.assert(fields.len == out_offsets.len);
    var measure: StructMeasure = .{};
    for (fields, out_offsets) |field, *offset| offset.* = measure.place(field);
    return measure.finish();
}

/// One tag union variant's payload as measuring reads it, at one pointer
/// width.
pub const PayloadShape = struct {
    size: u32,
    alignment: u32,
    sort_key: SortKey,
};

/// A measured tag union at one pointer width.
pub const TagUnionMetrics = struct {
    size: u32,
    alignment: u32,
    /// The discriminant follows the largest payload, aligned for its width.
    discriminant_offset: u32,
    sort_key: SortKey,
};

/// The alignment class a tag union starts from before its payloads: its
/// discriminant's.
pub fn discriminantSortKey(discriminant_size: u8) SortKey {
    return SortKey.fromAlignBytes(alignmentForDiscriminantSize(discriminant_size).toByteUnits());
}

/// Measures a tag union one variant payload at a time.
pub const TagUnionMeasure = struct {
    discriminant_size: u8,
    max_payload_size: u32 = 0,
    max_payload_alignment: u32 = 1,
    sort_key: SortKey,

    pub fn init(discriminant_size: u8) TagUnionMeasure {
        return .{ .discriminant_size = discriminant_size, .sort_key = discriminantSortKey(discriminant_size) };
    }

    pub fn addPayload(self: *TagUnionMeasure, payload: PayloadShape) void {
        self.max_payload_size = @max(self.max_payload_size, payload.size);
        self.max_payload_alignment = @max(self.max_payload_alignment, payload.alignment);
        self.sort_key = self.sort_key.max(payload.sort_key);
    }

    pub fn finish(self: TagUnionMeasure) TagUnionMetrics {
        const disc_align: u32 = @intCast(alignmentForDiscriminantSize(self.discriminant_size).toByteUnits());
        const disc_offset = std.mem.alignForward(u32, self.max_payload_size, disc_align);
        const alignment = @max(self.max_payload_alignment, disc_align);
        return .{
            .size = std.mem.alignForward(u32, disc_offset + self.discriminant_size, alignment),
            .alignment = alignment,
            .discriminant_offset = disc_offset,
            .sort_key = self.sort_key,
        };
    }
};

const testing = std.testing;

test "struct measuring pads fields to their alignment and rounds the total" {
    var offsets: [3]u32 = undefined;
    const metrics = measureStruct(&.{
        .{ .size = 1, .alignment = 1, .sort_key = .align_1 },
        .{ .size = 8, .alignment = 8, .sort_key = .align_8 },
        .{ .size = 2, .alignment = 2, .sort_key = .align_2 },
    }, &offsets);
    try testing.expectEqualSlices(u32, &.{ 0, 8, 16 }, &offsets);
    try testing.expectEqual(@as(u32, 24), metrics.size);
    try testing.expectEqual(@as(u32, 8), metrics.alignment);
    try testing.expectEqual(SortKey.align_8, metrics.sort_key);
}

test "padding fields contribute neither alignment nor sort key" {
    var offsets: [2]u32 = undefined;
    const metrics = measureStruct(&.{
        .{ .size = 1, .alignment = 1, .sort_key = .align_1 },
        .{ .size = 4, .alignment = 4, .sort_key = .align_4, .is_padding = true },
    }, &offsets);
    try testing.expectEqualSlices(u32, &.{ 0, 1 }, &offsets);
    try testing.expectEqual(@as(u32, 5), metrics.size);
    try testing.expectEqual(@as(u32, 1), metrics.alignment);
    try testing.expectEqual(SortKey.align_1, metrics.sort_key);
}

test "an empty struct measures as zero-sized with the smallest alignment class" {
    const metrics = measureStruct(&.{}, &.{});
    try testing.expectEqual(@as(u32, 0), metrics.size);
    try testing.expectEqual(@as(u32, 1), metrics.alignment);
    try testing.expectEqual(SortKey.align_1, metrics.sort_key);
}

test "a tag union places its discriminant after the largest payload" {
    var measure = TagUnionMeasure.init(discriminantSize(3));
    measure.addPayload(.{ .size = 0, .alignment = 1, .sort_key = .align_1 });
    measure.addPayload(.{ .size = 12, .alignment = 4, .sort_key = .align_4 });
    measure.addPayload(.{ .size = 1, .alignment = 1, .sort_key = .align_1 });
    const metrics = measure.finish();
    try testing.expectEqual(@as(u32, 12), metrics.discriminant_offset);
    try testing.expectEqual(@as(u32, 16), metrics.size);
    try testing.expectEqual(@as(u32, 4), metrics.alignment);
    try testing.expectEqual(SortKey.align_4, metrics.sort_key);
}

test "discriminant widths grow with the variant count" {
    try testing.expectEqual(@as(u8, 0), discriminantSize(1));
    try testing.expectEqual(@as(u8, 1), discriminantSize(2));
    try testing.expectEqual(@as(u8, 1), discriminantSize(256));
    try testing.expectEqual(@as(u8, 2), discriminantSize(257));
    try testing.expectEqual(@as(u8, 4), discriminantSize(65537));
}

test "a pointer sort key resolves to the target's pointer alignment" {
    try testing.expectEqual(@as(u32, 4), SortKey.pointer.alignmentBytes(4));
    try testing.expectEqual(@as(u32, 8), SortKey.pointer.alignmentBytes(8));
    try testing.expectEqual(@as(u32, 16), SortKey.align_16.alignmentBytes(4));
}

test "layout shape declarations are referenced" {
    testing.refAllDecls(@This());
}
