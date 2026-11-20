//! Canonical fixed-product bytes and their explicit committed-layout copy plans.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const layout = @import("layout");

/// One contiguous region in canonical field order. Scalar leaves are retained
/// separately so native-endian conversion never reverses an entire aggregate.
/// A discriminant is little-endian in a program's memory on every host
/// (`layout.TagUnionData.writeDiscriminantToPtr`), so only a scalar leaf is
/// in the program's byte order.
const Leaf = struct { packed_offset: u32, memory_offset: u32, width: u32, byte_order: enum { native, little } = .native };

/// A float leaf's offset in the canonical bytes, whose stored NaN is Roc's
/// one NaN whatever bits the evaluation produced.
const FloatLeaf = struct { packed_offset: u32, precision: enum { f32, f64 } };

/// A tag union's leaf, for a union every one of whose payloads is zero-sized
/// (`discriminantOnly`): its discriminant's `width` bytes at
/// `packed_offset` in the canonical bytes, which hold the index of the
/// item's tag among the union's `variant_count` tags in byte-wise name order,
/// the order that assigns discriminants, so the canonical bytes are the
/// discriminant's little-endian bytes.
const TagLeaf = struct { packed_offset: u32, width: u32, variant_count: u32 };

/// Reusable mapping between canonical product bytes and one committed layout.
pub const Plan = struct {
    allocator: std.mem.Allocator,
    leaves: std.ArrayList(Leaf) = .empty,
    regions: std.ArrayList(Leaf) = .empty,
    floats: std.ArrayList(FloatLeaf) = .empty,
    tags: std.ArrayList(TagLeaf) = .empty,
    packed_width: u32 = 0,
    memory_width: u32,

    /// Compile a plan for a producer-proven scalar, discriminant-only tag union
    /// (`discriminantOnly`), or fixed product of them (`layoutPacks`).
    pub fn init(allocator: std.mem.Allocator, layouts: *const layout.Store, element: layout.Idx) std.mem.Allocator.Error!Plan {
        var plan = Plan{ .allocator = allocator, .memory_width = layouts.layoutSize(layouts.getLayout(element)) };
        errdefer plan.deinit();
        try plan.appendLayout(layouts, element, 0);
        return plan;
    }

    /// Release the compiled copy regions.
    pub fn deinit(self: *Plan) void {
        self.leaves.deinit(self.allocator);
        self.regions.deinit(self.allocator);
        self.floats.deinit(self.allocator);
        self.tags.deinit(self.allocator);
    }

    /// Appends the product's scalar leaves in canonical field order. Nested
    /// products are expanded from a worklist: each struct's fields are pushed
    /// in reverse canonical order so the first field is expanded next.
    fn appendLayout(self: *Plan, layouts: *const layout.Store, root: layout.Idx, root_offset: u32) std.mem.Allocator.Error!void {
        const Pending = struct { layout_idx: layout.Idx, offset: u32 };
        const Field = struct { index: u32, layout_idx: layout.Idx, offset: u32 };
        var pending: std.ArrayList(Pending) = .empty;
        defer pending.deinit(self.allocator);
        var fields: std.ArrayList(Field) = .empty;
        defer fields.deinit(self.allocator);
        try pending.append(self.allocator, .{ .layout_idx = root, .offset = root_offset });
        while (pending.pop()) |next| {
            const offset = next.offset;
            const value = layouts.getLayout(next.layout_idx);
            switch (value.tag) {
                .hole => unreachable, // a hole sits only behind a List or Box, which a fixed product never holds
                .dynamic => unreachable, // a dynamic value is never a component of a closed layout
                .zst => {},
                .scalar => {
                    switch (value.getScalar().tag) {
                        .int, .vector => {},
                        .frac => switch (value.getScalar().getFrac()) {
                            .f32 => try self.floats.append(self.allocator, .{ .packed_offset = self.packed_width, .precision = .f32 }),
                            .f64 => try self.floats.append(self.allocator, .{ .packed_offset = self.packed_width, .precision = .f64 }),
                            .dec => {},
                        },
                        .str, .opaque_ptr => base.invariant("packed data invariant violated: fixed product holds a {s} scalar", .{@tagName(value.getScalar().tag)}),
                    }
                    try self.appendLeaf(offset, layouts.layoutSize(value), .native);
                },
                .tag_union => {
                    const data = layouts.getTagUnionData(value.getTagUnion().idx);
                    if (!discriminantOnly(data.*)) base.invariant("packed data invariant violated: fixed product holds a tag union with a payload that has bytes", .{});
                    const variants = layouts.getTagUnionVariants(data);
                    try self.tags.append(self.allocator, .{ .packed_offset = self.packed_width, .width = data.discriminant_size, .variant_count = @intCast(variants.len) });
                    try self.appendLeaf(offset + data.discriminant_offset.get(layouts.targetUsize()), data.discriminant_size, .little);
                },
                .struct_ => {
                    const info = layouts.getStructInfo(value);
                    fields.clearRetainingCapacity();
                    try fields.ensureTotalCapacity(self.allocator, info.fields.len);
                    for (0..info.fields.len) |i| {
                        const field = info.fields.get(@intCast(i));
                        if (field.is_padding) continue;
                        fields.appendAssumeCapacity(.{ .index = field.index, .layout_idx = field.layout, .offset = layouts.getStructFieldOffset(value.getStruct().idx, @intCast(i)) });
                    }
                    std.mem.sort(Field, fields.items, {}, struct {
                        fn less(_: void, a: Field, b: Field) bool {
                            return a.index > b.index;
                        }
                    }.less);
                    try pending.ensureUnusedCapacity(self.allocator, fields.items.len);
                    for (fields.items) |field| pending.appendAssumeCapacity(.{ .layout_idx = field.layout_idx, .offset = offset + field.offset });
                },
                .box, .box_of_zst, .list, .list_of_zst, .closure, .ptr, .erased_callable => base.invariant("packed data invariant violated: fixed product holds a {s} layout", .{@tagName(value.tag)}),
            }
        }
    }

    /// Whether a value of the tag union `data` is its discriminant alone on
    /// every target: every payload is zero-sized, so the union's size is the
    /// discriminant's and the discriminant is at its start. A union of one
    /// tag is zero-sized and never a tag union layout.
    pub fn discriminantOnly(data: layout.TagUnionData) bool {
        return data.discriminant_size != 0 and
            data.size.get(.u32) == data.discriminant_size and data.size.get(.u64) == data.discriminant_size and
            data.discriminant_offset.get(.u32) == 0 and data.discriminant_offset.get(.u64) == 0;
    }

    /// Whether the layout `root` is one a plan packs: scalars other than
    /// strings and opaque pointers, zero-sized values, discriminant-only tag
    /// unions, and structs of them.
    pub fn layoutPacks(allocator: std.mem.Allocator, layouts: *const layout.Store, root: layout.Idx) std.mem.Allocator.Error!bool {
        var pending: std.ArrayList(layout.Idx) = .empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |next| {
            const value = layouts.getLayout(next);
            switch (value.tag) {
                .zst => {},
                .scalar => switch (value.getScalar().tag) {
                    .int, .frac, .vector => {},
                    .str, .opaque_ptr => return false,
                },
                .tag_union => if (!discriminantOnly(layouts.getTagUnionData(value.getTagUnion().idx).*)) return false,
                .struct_ => {
                    const info = layouts.getStructInfo(value);
                    for (0..info.fields.len) |i| {
                        const field = info.fields.get(@intCast(i));
                        if (!field.is_padding) try pending.append(allocator, field.layout);
                    }
                },
                .hole, .dynamic, .box, .box_of_zst, .list, .list_of_zst, .closure, .ptr, .erased_callable => return false,
            }
        }
        return true;
    }

    /// Appends the `width` bytes at `offset` in the layout as the next leaf.
    fn appendLeaf(self: *Plan, offset: u32, width: u32, byte_order: @FieldType(Leaf, "byte_order")) std.mem.Allocator.Error!void {
        const leaf = Leaf{ .packed_offset = self.packed_width, .memory_offset = offset, .width = width, .byte_order = byte_order };
        try self.leaves.append(self.allocator, leaf);
        self.packed_width = std.math.add(u32, self.packed_width, width) catch unreachable; // leaves are disjoint bytes of one layout, whose size fits in u32
        if (self.regions.items.len != 0) {
            const last = &self.regions.items[self.regions.items.len - 1];
            if (last.memory_offset + last.width == offset) {
                last.width += width;
                return;
            }
        }
        try self.regions.append(self.allocator, leaf);
    }

    /// Whether canonical and little-endian target storage are byte-identical.
    pub fn isIdentity(self: *const Plan) bool {
        return self.packed_width == self.memory_width and
            (self.memory_width == 0 or (self.regions.items.len == 1 and self.regions.items[0].memory_offset == 0));
    }

    /// Whether a program's memory of `endian` already is the canonical bytes
    /// `encode` writes: storage is identical and no float leaf has a NaN to
    /// replace.
    pub fn memoryIsCanonical(self: *const Plan, endian: std.builtin.Endian) bool {
        return endian == .little and self.isIdentity() and self.floats.items.len == 0;
    }

    /// Encode CTFE memory, omitting padding and preserving canonical scalar
    /// bits: every scalar's bits as they are, except that a float NaN is
    /// Roc's one NaN.
    pub fn encode(self: *const Plan, out: []u8, memory: []const u8, count: usize, endian: std.builtin.Endian) void {
        std.debug.assert(out.len == count * self.packed_width);
        std.debug.assert(memory.len == count * self.memory_width);
        if (endian == .little and self.isIdentity()) {
            @memcpy(out, memory);
        } else for (0..count) |i| {
            const input = memory[i * self.memory_width ..][0..self.memory_width];
            const output = out[i * self.packed_width ..][0..self.packed_width];
            for (if (endian == .little) self.regions.items else self.leaves.items) |part| {
                const destination = output[part.packed_offset..][0..part.width];
                @memcpy(destination, input[part.memory_offset..][0..part.width]);
                if (endian == .big and part.byte_order == .native) std.mem.reverse(u8, destination);
            }
        }
        if (builtin.mode == .debug) self.assertTagsInRange(out, count);
        if (self.floats.items.len == 0) return;
        for (0..count) |i| {
            const output = out[i * self.packed_width ..][0..self.packed_width];
            for (self.floats.items) |float| switch (float.precision) {
                .f32 => {
                    const bits = output[float.packed_offset..][0..4];
                    std.mem.writeInt(u32, bits, builtins.float_bits.normalizeF32NanBits(std.mem.readInt(u32, bits, .little)), .little);
                },
                .f64 => {
                    const bits = output[float.packed_offset..][0..8];
                    std.mem.writeInt(u64, bits, builtins.float_bits.normalizeF64NanBits(std.mem.readInt(u64, bits, .little)), .little);
                },
            };
        }
    }

    /// Every tag leaf of the `count` items `canonical` holds names one of its
    /// union's tags.
    fn assertTagsInRange(self: *const Plan, canonical: []const u8, count: usize) void {
        for (self.tags.items) |tag| for (0..count) |i| {
            const bytes = canonical[i * self.packed_width + tag.packed_offset ..][0..tag.width];
            var index: u64 = 0;
            for (bytes, 0..) |byte, shift| index |= @as(u64, byte) << @intCast(shift * 8);
            std.debug.assert(index < tag.variant_count);
        };
    }

    /// Materialize storage for Roc's little-endian targets with zeroed padding.
    pub fn decode(self: *const Plan, out: []u8, canonical: []const u8, count: usize) void {
        std.debug.assert(out.len == count * self.memory_width);
        std.debug.assert(canonical.len == count * self.packed_width);
        if (builtin.mode == .debug) self.assertTagsInRange(canonical, count);
        if (self.isIdentity()) {
            @memcpy(out, canonical);
            return;
        }
        @memset(out, 0);
        for (0..count) |i| {
            const input = canonical[i * self.packed_width ..][0..self.packed_width];
            const output = out[i * self.memory_width ..][0..self.memory_width];
            for (self.regions.items) |part| @memcpy(output[part.memory_offset..][0..part.width], input[part.packed_offset..][0..part.width]);
        }
    }
};

test "packed products preserve semantic order and zero nested target padding" {
    const gpa = std.testing.allocator;
    inline for (.{ .u32, .u64 }) |target| {
        var layouts = try layout.Store.init(gpa, target);
        defer layouts.deinit();
        const inner = try layouts.putStructFields(&.{
            .{ .index = 0, .layout = .u16 },
            .{ .index = 1, .layout = .u32 },
        });
        const outer = try layouts.putStructFields(&.{
            .{ .index = 0, .layout = .u8 },
            .{ .index = 1, .layout = inner },
        });
        var plan = try Plan.init(gpa, &layouts, outer);
        defer plan.deinit();
        try std.testing.expectEqual(@as(u32, 7), plan.packed_width);
        try std.testing.expectEqual(@as(u32, 12), plan.memory_width);
        const canonical = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16, 17 };
        // Both nested and outer tail padding must be deterministic.
        const expected = [_]u8{ 4, 5, 6, 7, 2, 3, 0, 0, 1, 0, 0, 0, 14, 15, 16, 17, 12, 13, 0, 0, 11, 0, 0, 0 };
        var memory: [24]u8 = @splat(0xff);
        plan.decode(&memory, &canonical, 2);
        try std.testing.expectEqualSlices(u8, &expected, &memory);
        var roundtrip: [14]u8 = undefined;
        plan.encode(&roundtrip, &memory, 2, .little);
        try std.testing.expectEqualSlices(u8, &canonical, &roundtrip);

        // Big-endian CTFE changes each leaf's byte order, never the product's.
        const big_endian = [_]u8{ 7, 6, 5, 4, 3, 2, 0xff, 0xff, 1, 0xff, 0xff, 0xff, 17, 16, 15, 14, 13, 12, 0xff, 0xff, 11, 0xff, 0xff, 0xff };
        plan.encode(&roundtrip, &big_endian, 2, .big);
        try std.testing.expectEqualSlices(u8, &canonical, &roundtrip);
    }
}

test "packed products store every float NaN as Roc's one NaN" {
    const gpa = std.testing.allocator;
    var layouts = try layout.Store.init(gpa, .u64);
    defer layouts.deinit();
    const product = try layouts.putStructFields(&.{
        .{ .index = 0, .layout = .f64 },
        .{ .index = 1, .layout = .f32 },
        .{ .index = 2, .layout = .u32 },
    });
    var plan = try Plan.init(gpa, &layouts, product);
    defer plan.deinit();
    try std.testing.expect(plan.isIdentity());
    try std.testing.expect(!plan.memoryIsCanonical(.little));
    const Item = struct { f64_bits: u64, f32_bits: u32, int: u32 };
    const items = [_]Item{
        .{ .f64_bits = 0xfff8_0000_0000_0000, .f32_bits = 0xffc0_0000, .int = 0x7fc0_0001 },
        .{ .f64_bits = 0x7ff0_0000_0000_0001, .f32_bits = 0x7f80_0001, .int = 0xffff_ffff },
        .{ .f64_bits = 0x8000_0000_0000_0000, .f32_bits = 0x7f80_0000, .int = 0 },
        .{ .f64_bits = 0x3ff0_0000_0000_0000, .f32_bits = 0x0000_0001, .int = 7 },
    };
    var memory: [items.len * 16]u8 = undefined;
    var expected: [items.len * 16]u8 = undefined;
    for (items, 0..) |item, index| {
        const element = memory[index * 16 ..][0..16];
        std.mem.writeInt(u64, element[0..8], item.f64_bits, .little);
        std.mem.writeInt(u32, element[8..12], item.f32_bits, .little);
        std.mem.writeInt(u32, element[12..16], item.int, .little);
        const canonical = expected[index * 16 ..][0..16];
        std.mem.writeInt(u64, canonical[0..8], builtins.float_bits.normalizeF64NanBits(item.f64_bits), .little);
        std.mem.writeInt(u32, canonical[8..12], builtins.float_bits.normalizeF32NanBits(item.f32_bits), .little);
        std.mem.writeInt(u32, canonical[12..16], item.int, .little);
    }
    var out: [expected.len]u8 = undefined;
    plan.encode(&out, &memory, items.len, .little);
    try std.testing.expectEqualSlices(u8, &expected, &out);

    var big_endian: [memory.len]u8 = undefined;
    for (items, 0..) |item, index| {
        const element = big_endian[index * 16 ..][0..16];
        std.mem.writeInt(u64, element[0..8], item.f64_bits, .big);
        std.mem.writeInt(u32, element[8..12], item.f32_bits, .big);
        std.mem.writeInt(u32, element[12..16], item.int, .big);
    }
    plan.encode(&out, &big_endian, items.len, .big);
    try std.testing.expectEqualSlices(u8, &expected, &out);
}

test "packed products store a tag union with no payloads as its discriminant" {
    const gpa = std.testing.allocator;
    inline for (.{ .u32, .u64 }) |target| {
        var layouts = try layout.Store.init(gpa, target);
        defer layouts.deinit();
        // More tags than one byte numbers, so the discriminant takes two.
        var wide_variants: [300]layout.Idx = undefined;
        @memset(&wide_variants, .zst);
        const wide = try layouts.putTagUnion(&wide_variants);
        const three = try layouts.putTagUnion(&.{ .zst, .zst, .zst });
        const product = try layouts.putStructFields(&.{
            .{ .index = 0, .layout = .bool },
            .{ .index = 1, .layout = wide },
            .{ .index = 2, .layout = .u32 },
            .{ .index = 3, .layout = three },
        });

        var bool_plan = try Plan.init(gpa, &layouts, .bool);
        defer bool_plan.deinit();
        try std.testing.expectEqual(@as(u32, 1), bool_plan.packed_width);
        try std.testing.expect(bool_plan.memoryIsCanonical(.little));
        try std.testing.expectEqual(@as(usize, 1), bool_plan.tags.items.len);
        try std.testing.expectEqual(@as(u32, 2), bool_plan.tags.items[0].variant_count);

        var plan = try Plan.init(gpa, &layouts, product);
        defer plan.deinit();
        // Bool, two discriminant bytes, U32 and one discriminant byte.
        try std.testing.expectEqual(@as(u32, 8), plan.packed_width);
        try std.testing.expectEqual(@as(usize, 3), plan.tags.items.len);
        try std.testing.expectEqual(@as(u32, 300), plan.tags.items[1].variant_count);
        try std.testing.expectEqual(@as(u32, 2), plan.tags.items[1].width);
        const items = [_]struct { flag: u8, wide: u16, int: u32, three: u8 }{
            .{ .flag = 1, .wide = 299, .int = 0xdead_beef, .three = 2 },
            .{ .flag = 0, .wide = 256, .int = 7, .three = 0 },
        };
        var canonical: [items.len * 8]u8 = undefined;
        for (items, 0..) |item, index| {
            const bytes = canonical[index * 8 ..][0..8];
            bytes[0] = item.flag;
            std.mem.writeInt(u16, bytes[1..3], item.wide, .little);
            std.mem.writeInt(u32, bytes[3..7], item.int, .little);
            bytes[7] = item.three;
        }
        var memory_buffer: [items.len * 16]u8 = undefined;
        const memory = memory_buffer[0 .. items.len * plan.memory_width];
        plan.decode(memory, &canonical, items.len);
        for (items, 0..) |item, index| {
            const element = memory[index * plan.memory_width ..][0..plan.memory_width];
            for ([_]struct { field: u32, value: u32, width: u32 }{
                .{ .field = 0, .value = item.flag, .width = 1 },
                .{ .field = 1, .value = item.wide, .width = 2 },
                .{ .field = 2, .value = item.int, .width = 4 },
                .{ .field = 3, .value = item.three, .width = 1 },
            }) |expected| {
                const offset = layouts.getStructFieldOffsetByOriginalIndex(layouts.getLayout(product).getStruct().idx, expected.field);
                var value: u32 = 0;
                for (element[offset..][0..expected.width], 0..) |byte, shift| value |= @as(u32, byte) << @intCast(shift * 8);
                try std.testing.expectEqual(expected.value, value);
            }
        }
        var roundtrip: [canonical.len]u8 = undefined;
        plan.encode(&roundtrip, memory, items.len, .little);
        try std.testing.expectEqualSlices(u8, &canonical, &roundtrip);

        // A big-endian program's memory holds its integers in its own byte
        // order and its discriminants little-endian.
        var big_endian_buffer: [memory_buffer.len]u8 = undefined;
        const big_endian = big_endian_buffer[0..memory.len];
        @memcpy(big_endian, memory);
        const int_offset = layouts.getStructFieldOffsetByOriginalIndex(layouts.getLayout(product).getStruct().idx, 2);
        for (0..items.len) |index| std.mem.reverse(u8, big_endian[index * plan.memory_width + int_offset ..][0..4]);
        plan.encode(&roundtrip, big_endian, items.len, .big);
        try std.testing.expectEqualSlices(u8, &canonical, &roundtrip);
    }
}

test "a tag union packs exactly when its payloads are all zero-sized" {
    const gpa = std.testing.allocator;
    inline for (.{ .u32, .u64 }) |target| {
        var layouts = try layout.Store.init(gpa, target);
        defer layouts.deinit();
        const unit = try layouts.putStructFields(&.{});
        const units = try layouts.putStructFields(&.{ .{ .index = 0, .layout = .zst }, .{ .index = 1, .layout = unit } });
        const only = try layouts.putTagUnion(&.{.zst});
        // `[A({}), B]`, `[A({}, {}), B, C([Only])]` and `[A(U8), B]`.
        const unit_payload = try layouts.putTagUnion(&.{ unit, .zst });
        const nested = try layouts.putTagUnion(&.{ units, .zst, only });
        const byte_payload = try layouts.putTagUnion(&.{ .u8, .zst });
        const product = try layouts.putStructFields(&.{
            .{ .index = 0, .layout = nested },
            .{ .index = 1, .layout = .u16 },
            .{ .index = 2, .layout = unit_payload },
        });
        for ([_]layout.Idx{ unit_payload, nested, product }) |packs| {
            try std.testing.expect(try Plan.layoutPacks(gpa, &layouts, packs));
        }
        for ([_]layout.Idx{ byte_payload, try layouts.putStructFields(&.{ .{ .index = 0, .layout = byte_payload }, .{ .index = 1, .layout = .u16 } }) }) |unpacked| {
            try std.testing.expect(!try Plan.layoutPacks(gpa, &layouts, unpacked));
        }
        try std.testing.expect(!Plan.discriminantOnly(layouts.getTagUnionData(layouts.getLayout(byte_payload).getTagUnion().idx).*));

        var plan = try Plan.init(gpa, &layouts, product);
        defer plan.deinit();
        // Two discriminant bytes around the U16.
        try std.testing.expectEqual(@as(u32, 4), plan.packed_width);
        try std.testing.expectEqual(@as(usize, 2), plan.tags.items.len);
        try std.testing.expectEqual(@as(u32, 3), plan.tags.items[0].variant_count);
        try std.testing.expectEqual(@as(u32, 2), plan.tags.items[1].variant_count);
        const canonical = [_]u8{ 2, 0x34, 0x12, 1, 0, 0xff, 0xff, 0 };
        var memory_buffer: [16]u8 = undefined;
        const memory = memory_buffer[0 .. 2 * plan.memory_width];
        plan.decode(memory, &canonical, 2);
        var roundtrip: [canonical.len]u8 = undefined;
        plan.encode(&roundtrip, memory, 2, .little);
        try std.testing.expectEqualSlices(u8, &canonical, &roundtrip);
    }
}
