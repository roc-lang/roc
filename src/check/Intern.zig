const std = @import("std");
const Allocator = std.mem.Allocator;

pub const InternId = union {
    small: u32,
    big: IndexAndLen,

    pub fn isInline(self: @This()) bool {
        // If the first byte is in the ASCII range (under 128), this is small and inline.
        return @as([4]u8, @bitCast(self))[0] <= @as(u8, @intCast(std.math.maxInt(i8)));
    }

    pub fn asSmall(self: @This()) [4]u8 {
        std.debug.assert(self.isInline());
        return self.small;
    }

    fn new(bytes: []const u8) InternId {
        // Branchlessly create the new InternId.
        const small = InternId{ .small = InlineInternId.new(bytes) };
        const big = InternId{ .big = IndexAndLen.new(bytes) };

        return if (bytes.len <= 4) small else big;
    }

    fn newSmall(bytes: [4]u8) InternId {
        return InternId{ .small = bytes };
    }

    fn newBig(index_and_len: IndexAndLen) InternId {
        return InternId{ .big = index_and_len };
    }
};

pub const InlineInternId = struct {
    num: u32,

    // Branchlessly create a u32 from a slice of 1-4 bytes,
    // by padding with zeroes.
    //
    // Slices of 5+ bytes are accepted, but bytes 5+ will
    // be ignored. Empty slices are not accepted.
    pub fn new(bytes: []const u8) InlineInternId {
        const len = bytes.len;

        // Get the addresses of bytes 2, 3, and 4, but don't dereference them yet
        // because depending on our length they might point to inaccessible memory!
        const addr1: *const u8 = @ptrCast(bytes.ptr + 1);
        const addr2: *const u8 = @ptrCast(bytes.ptr + 2);
        const addr3: *const u8 = @ptrCast(bytes.ptr + 3);

        // Branchlessly zero-pad a u32 by (branchlessly) choosing either these pointers,
        // or else a pointer to zero, depending on the length.
        var answer: u32 = undefined;
        var dest = @as([*]u8, @ptrCast(&answer));
        const zero: u8 = 0;

        // Always set the first byte, because we don't accept empty slices.
        std.debug.assert(len > 0);
        dest[0] = bytes[0];
        dest[1] = (if (len > 1) addr1 else &zero).*;
        dest[2] = (if (len > 2) addr2 else &zero).*;
        dest[3] = (if (len > 3) addr3 else &zero).*;

        return .{ .num = answer };
    }

    pub fn toBytes(self: InlineInternId) [4]u8 {
        return @as([4]u8, @bitCast(self.num));
    }
};

const IndexAndLen = struct {
    is_big: bool,
    len_category: LenCategory,
    len: u29,
};

const LenCategory = enum {
    UpToEight,
    NineToSixteen,
    OverSixteen,

    /// len must not be zero.
    fn fromLen(len: usize) LenCategory {
        std.debug.assert(len != 0);

        // Branchless implementation using bit shifts
        // 1-8 maps to 0 (UpToEight)
        // 9-16 maps to 1 (NineToSixteen)
        // 17+ maps to 2 (OverSixteen)

        // (len-1) >> 3 gives us:
        // 0 for lengths 1-8
        // 1 for lengths 9-16
        // 2+ for lengths 17+
        const shift_result = (len - 1) >> 3;

        // Clamp values over 2 to exactly 2.
        const clamped = @min(shift_result, 2);

        return @as(LenCategory, @enumFromInt(@as(u2, @intCast(clamped))));
    }
};

test "LenCategory.fromLen correctly categorizes lengths" {
    for (1..9) |i| {
        try std.testing.expectEqual(LenCategory.UpToEight, LenCategory.fromLen(i));
    }

    for (9..17) |i| {
        try std.testing.expectEqual(LenCategory.NineToSixteen, LenCategory.fromLen(i));
    }

    try std.testing.expectEqual(LenCategory.OverSixteen, LenCategory.fromLen(17));
    try std.testing.expectEqual(LenCategory.OverSixteen, LenCategory.fromLen(20));
    try std.testing.expectEqual(LenCategory.OverSixteen, LenCategory.fromLen(100));
    try std.testing.expectEqual(LenCategory.OverSixteen, LenCategory.fromLen(255));
}

test "InlineInternId handles strings of length 1 correctly" {
    const input = "a";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    try std.testing.expect(bytes[0] == 'a');
    try std.testing.expect(bytes[1] == 0);
    try std.testing.expect(bytes[2] == 0);
    try std.testing.expect(bytes[3] == 0);
}

test "InlineInternId handles strings of length 2 correctly" {
    const input = "ab";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    try std.testing.expect(bytes[0] == 'a');
    try std.testing.expect(bytes[1] == 'b');
    try std.testing.expect(bytes[2] == 0);
    try std.testing.expect(bytes[3] == 0);
}

test "InlineInternId handles strings of length 3 correctly" {
    const input = "abc";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    try std.testing.expect(bytes[0] == 'a');
    try std.testing.expect(bytes[1] == 'b');
    try std.testing.expect(bytes[2] == 'c');
    try std.testing.expect(bytes[3] == 0);
}

test "InlineInternId handles strings of length 4 correctly" {
    const input = "abcd";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    try std.testing.expect(bytes[0] == 'a');
    try std.testing.expect(bytes[1] == 'b');
    try std.testing.expect(bytes[2] == 'c');
    try std.testing.expect(bytes[3] == 'd');
}

test "InlineInternId handles non-ASCII characters correctly" {
    // Test with non-ASCII characters
    const input = "é";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    // 'é' is encoded as 0xC3 0xA9 in UTF-8
    try std.testing.expect(bytes[0] == 0xC3);
    try std.testing.expect(bytes[1] == 0xA9);
    try std.testing.expect(bytes[2] == 0);
    try std.testing.expect(bytes[3] == 0);
}

test "InlineInternId preserves exactly 4 bytes" {
    // Test with exactly 4 bytes
    const input = "abcd";
    const id = InlineInternId.new(input);
    const bytes = id.toBytes();

    try std.testing.expect(bytes[0] == 'a');
    try std.testing.expect(bytes[1] == 'b');
    try std.testing.expect(bytes[2] == 'c');
    try std.testing.expect(bytes[3] == 'd');

    // Make sure we're getting back exactly what we put in
    try std.testing.expectEqualSlices(u8, input, &bytes);
}

const Bucket = struct {
    capacity: u32,
    len: u32,
    elements: [*]align(16) anyopaque,

    pub fn init() Bucket {
        return .{
            .capacity = 0,
            .len = 0,
            .elements = undefined,
        };
    }

    pub fn insert(self: *Bucket, allocator: Allocator, comptime T: type, value: T) Allocator.Error!u32 {
        std.debug.assert(@typeInfo(T) == .Int);
        std.debug.assert(@sizeOf(T) <= 16);

        // Ensure we're accessing elements as T slices
        const elements = @as([*]T, @ptrCast(self.elements))[0..self.len];

        // Check if the value already exists using SIMD
        const Vector = std.meta.Vector;
        const bytes_per_simd = 16;
        const simd_len = bytes_per_simd / @sizeOf(T);

        // SIMD search loop
        var i: u32 = 0;
        while (i + simd_len <= self.len) {
            const chunk = @as(*align(16) const [simd_len]T, @ptrCast(elements.ptr + i)).*;
            const v_chunk: Vector(simd_len, T) = chunk;
            const v_value: Vector(simd_len, T) = @splat(value);
            const mask = v_chunk == v_value;

            const mask_bits = @as(std.meta.Int(.unsigned, simd_len), @bitCast(mask));
            if (mask_bits != 0) {
                // Found a match, find the first matching index
                const trailing_zeros = @ctz(mask_bits);
                return i + trailing_zeros;
            }

            i += simd_len;
        }

        // Check remaining elements (fewer than simd_len)
        while (i < self.len) {
            if (elements[i] == value) {
                return i;
            }
            i += 1;
        }

        // Value not found, need to insert it
        // Check if we need to resize
        if (self.len == self.capacity) {
            try self.grow(allocator, T);
        }

        // Insert the new element
        const elements_mut = @as([*]T, @ptrCast(self.elements))[0..self.capacity];
        elements_mut[self.len] = value;
        self.len += 1;

        return self.len - 1;
    }

    fn grow(self: *Bucket, allocator: Allocator, comptime T: type) !void {
        const new_capacity = if (self.capacity == 0) 16 else self.capacity * 2;

        // Always ensure capacity is a multiple of 16 for alignment
        const aligned_capacity = (new_capacity + 15) & ~@as(u32, 15);

        // Allocate new memory
        const new_elements = try allocator.alignedAlloc(u8, 16, aligned_capacity * @sizeOf(T));
        const new_elements_ptr = @as([*]align(16) anyopaque, @ptrCast(new_elements.ptr));

        // Copy over existing elements using SIMD if possible
        if (self.len > 0) {
            const Vector = std.meta.Vector;
            const simd_bytes = 16; // 128 bits = 16 bytes

            const old_elements_bytes = @as([*]const u8, @ptrCast(self.elements))[0 .. self.len * @sizeOf(T)];
            const new_elements_bytes = @as([*]u8, @ptrCast(new_elements_ptr))[0 .. self.len * @sizeOf(T)];

            var i: usize = 0;
            // Copy in 16-byte chunks
            while (i + simd_bytes <= old_elements_bytes.len) {
                const chunk = @as(*align(16) const [simd_bytes]u8, @ptrCast(old_elements_bytes.ptr + i)).*;
                const v_chunk: Vector(simd_bytes, u8) = chunk;
                @as(*align(16) [simd_bytes]u8, @ptrCast(new_elements_bytes.ptr + i)).* = v_chunk;
                i += simd_bytes;
            }

            // Copy remaining bytes
            while (i < old_elements_bytes.len) {
                new_elements_bytes[i] = old_elements_bytes[i];
                i += 1;
            }

            // Free old memory if we had any
            if (self.capacity > 0) {
                allocator.free(@as([*]u8, @ptrCast(self.elements))[0 .. self.capacity * @sizeOf(T)]);
            }
        }

        self.elements = new_elements_ptr;
        self.capacity = aligned_capacity;
    }
};
