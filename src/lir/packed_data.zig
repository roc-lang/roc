//! Canonical fixed-product bytes and their explicit committed-layout copy plans.

const std = @import("std");
const layout = @import("layout");

/// One contiguous region in canonical field order. Scalar leaves are retained
/// separately so native-endian conversion never reverses an entire aggregate.
const Leaf = struct { packed_offset: u32, memory_offset: u32, width: u32 };

/// Reusable mapping between canonical product bytes and one committed layout.
pub const Plan = struct {
    allocator: std.mem.Allocator,
    leaves: std.ArrayList(Leaf) = .empty,
    regions: std.ArrayList(Leaf) = .empty,
    packed_width: u32 = 0,
    memory_width: u32,

    /// Compile a plan for a producer-proven scalar or fixed product.
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
    }

    fn appendLayout(self: *Plan, layouts: *const layout.Store, idx: layout.Idx, offset: u32) std.mem.Allocator.Error!void {
        const value = layouts.getLayout(idx);
        switch (value.tag) {
            .zst => {},
            .scalar => {
                switch (value.getScalar().tag) {
                    .int, .frac, .vector => {},
                    .str, .opaque_ptr => unreachable,
                }
                const width = layouts.layoutSize(value);
                const leaf = Leaf{ .packed_offset = self.packed_width, .memory_offset = offset, .width = width };
                try self.leaves.append(self.allocator, leaf);
                self.packed_width = std.math.add(u32, self.packed_width, width) catch unreachable;
                if (self.regions.items.len != 0) {
                    const last = &self.regions.items[self.regions.items.len - 1];
                    if (last.memory_offset + last.width == offset) {
                        last.width += width;
                        return;
                    }
                }
                try self.regions.append(self.allocator, leaf);
            },
            .struct_ => {
                const info = layouts.getStructInfo(value);
                const Field = struct { index: u16, layout_idx: layout.Idx, offset: u32 };
                var fields: std.ArrayList(Field) = .empty;
                defer fields.deinit(self.allocator);
                try fields.ensureTotalCapacity(self.allocator, info.fields.len);
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(@intCast(i));
                    if (field.is_padding) continue;
                    fields.appendAssumeCapacity(.{ .index = field.index, .layout_idx = field.layout, .offset = layouts.getStructFieldOffset(value.getStruct().idx, @intCast(i)) });
                }
                std.mem.sort(Field, fields.items, {}, struct {
                    fn less(_: void, a: Field, b: Field) bool {
                        return a.index < b.index;
                    }
                }.less);
                for (fields.items) |field| try self.appendLayout(layouts, field.layout_idx, offset + field.offset);
            },
            .box, .box_of_zst, .list, .list_of_zst, .closure, .tag_union, .ptr, .erased_box, .erased_callable => unreachable,
        }
    }

    /// Whether canonical and little-endian target storage are byte-identical.
    pub fn isIdentity(self: *const Plan) bool {
        return self.packed_width == self.memory_width and
            (self.memory_width == 0 or (self.regions.items.len == 1 and self.regions.items[0].memory_offset == 0));
    }

    /// Encode CTFE memory, omitting padding and preserving canonical scalar bits.
    pub fn encode(self: *const Plan, out: []u8, memory: []const u8, count: usize, endian: std.builtin.Endian) void {
        std.debug.assert(out.len == count * self.packed_width);
        std.debug.assert(memory.len == count * self.memory_width);
        if (endian == .little and self.isIdentity()) {
            @memcpy(out, memory);
            return;
        }
        for (0..count) |i| {
            const input = memory[i * self.memory_width ..][0..self.memory_width];
            const output = out[i * self.packed_width ..][0..self.packed_width];
            for (if (endian == .little) self.regions.items else self.leaves.items) |part| {
                const destination = output[part.packed_offset..][0..part.width];
                @memcpy(destination, input[part.memory_offset..][0..part.width]);
                if (endian == .big) std.mem.reverse(u8, destination);
            }
        }
    }

    /// Materialize storage for Roc's little-endian targets with zeroed padding.
    pub fn decode(self: *const Plan, out: []u8, canonical: []const u8, count: usize) void {
        std.debug.assert(out.len == count * self.memory_width);
        std.debug.assert(canonical.len == count * self.packed_width);
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
