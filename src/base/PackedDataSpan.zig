//! PackedDataSpan provides space-efficient storage of DataSpan information in a single u32.
//!
//! This is useful for NodeStore where we need to store many DataSpan references but have
//! limited node storage fields. The bit allocation is configurable at comptime to optimize
//! for different use cases.
//!
//! IMPORTANT LIMITATIONS:
//! - Only use this where you can guarantee the start/length fit in the allocated bits
//! - Consider the trade-offs between start range and length range for your use case
//! - Not suitable for very large spans that exceed the bit limits

const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const DataSpan = base.DataSpan;

/// Configurable packed DataSpan with customizable bit allocation
pub fn PackedDataSpan(comptime start_bits: u6, comptime length_bits: u6) type {
    comptime {
        if (start_bits + length_bits != 32) {
            @compileError("start_bits + length_bits must equal 32");
        }
        if (start_bits == 0 or length_bits == 0) {
            @compileError("Both start_bits and length_bits must be greater than 0");
        }
    }

    return packed struct(u32) {
        start: std.meta.Int(.unsigned, start_bits),
        len: std.meta.Int(.unsigned, length_bits),

        const Self = @This();

        /// Maximum value that can be stored in the start field
        pub const MAX_START: u32 = (1 << start_bits) - 1;

        /// Maximum value that can be stored in the length field
        pub const MAX_LENGTH: u32 = (1 << length_bits) - 1;

        /// Create a PackedDataSpan from a DataSpan, returns error if values exceed limits
        pub fn fromDataSpan(span: DataSpan) error{ StartTooLarge, LengthTooLarge }!Self {
            if (span.start > MAX_START) {
                return error.StartTooLarge;
            }
            if (span.len > MAX_LENGTH) {
                return error.LengthTooLarge;
            }

            return Self{
                .start = @intCast(span.start),
                .len = @intCast(span.len),
            };
        }

        /// Create a PackedDataSpan from a DataSpan, panics if values exceed limits
        /// Only use this when you're certain the values fit
        pub fn fromDataSpanUnchecked(span: DataSpan) Self {
            std.debug.assert(span.start <= MAX_START);
            std.debug.assert(span.len <= MAX_LENGTH);

            return Self{
                .start = @intCast(span.start),
                .len = @intCast(span.len),
            };
        }

        /// Create a PackedDataSpan from raw start and length values
        pub fn init(start: u32, len: u32) error{ StartTooLarge, LengthTooLarge }!Self {
            return fromDataSpan(DataSpan{ .start = start, .len = len });
        }

        /// Convert back to a DataSpan
        pub fn toDataSpan(self: Self) DataSpan {
            return DataSpan{
                .start = @as(u32, self.start),
                .len = @as(u32, self.len),
            };
        }

        /// Get the raw u32 value for storage
        pub fn toU32(self: Self) u32 {
            return @bitCast(self);
        }

        /// Create from a raw u32 value
        pub fn fromU32(value: u32) Self {
            return @bitCast(value);
        }

        /// Check if a DataSpan can fit in this packed representation
        pub fn canFit(span: DataSpan) bool {
            return span.start <= MAX_START and span.len <= MAX_LENGTH;
        }
    };
}

/// Common configurations for different use cases
/// Good for function arguments and method calls (up to 255 args, 16M+ start positions)
pub const FunctionArgs = PackedDataSpan(24, 8);

/// Good for small collections with high start range (up to 255 items, 16M+ start positions)
pub const SmallCollections = PackedDataSpan(24, 8);

/// Balanced allocation (up to 65K items, 65K+ start positions)
pub const Balanced = PackedDataSpan(16, 16);

/// Good for large collections with lower start range (up to 1M items, 4K+ start positions)
pub const LargeCollections = PackedDataSpan(12, 20);

test "PackedDataSpan basic functionality" {
    const Packed = PackedDataSpan(16, 16);

    // Test creation and conversion
    const original = DataSpan{ .start = 1000, .len = 50 };
    const packed_span = try Packed.fromDataSpan(original);
    const restored = packed_span.toDataSpan();

    try testing.expectEqual(original.start, restored.start);
    try testing.expectEqual(original.len, restored.len);
}

test "PackedDataSpan limits" {
    const Packed = PackedDataSpan(16, 16);

    // Test max values work
    try testing.expectEqual(@as(u32, 65535), Packed.MAX_START);
    try testing.expectEqual(@as(u32, 65535), Packed.MAX_LENGTH);

    const max_span = DataSpan{ .start = 65535, .len = 65535 };
    const packed_span = try Packed.fromDataSpan(max_span);
    const restored = packed_span.toDataSpan();

    try testing.expectEqual(max_span.start, restored.start);
    try testing.expectEqual(max_span.len, restored.len);
}

test "PackedDataSpan overflow detection" {
    const Packed = PackedDataSpan(16, 16);

    // Test start overflow
    const start_overflow = DataSpan{ .start = 65536, .len = 0 };
    try testing.expectError(error.StartTooLarge, Packed.fromDataSpan(start_overflow));

    // Test length overflow
    const len_overflow = DataSpan{ .start = 0, .len = 65536 };
    try testing.expectError(error.LengthTooLarge, Packed.fromDataSpan(len_overflow));
}

test "PackedDataSpan canFit" {
    const Packed = PackedDataSpan(16, 16);

    try testing.expect(Packed.canFit(DataSpan{ .start = 1000, .len = 50 }));
    try testing.expect(Packed.canFit(DataSpan{ .start = 65535, .len = 65535 }));
    try testing.expect(!Packed.canFit(DataSpan{ .start = 65536, .len = 0 }));
    try testing.expect(!Packed.canFit(DataSpan{ .start = 0, .len = 65536 }));
}

test "PackedDataSpan u32 conversion" {
    const Packed = PackedDataSpan(16, 16);

    const original = DataSpan{ .start = 1000, .len = 50 };
    const packed_span = try Packed.fromDataSpan(original);
    const raw = packed_span.toU32();
    const restored_packed = Packed.fromU32(raw);
    const restored = restored_packed.toDataSpan();

    try testing.expectEqual(original.start, restored.start);
    try testing.expectEqual(original.len, restored.len);
}

test "PackedDataSpan different configurations" {
    // Test FunctionArgs configuration (20, 12)
    const func_span = DataSpan{ .start = 1000000, .len = 100 };
    try testing.expect(FunctionArgs.canFit(func_span));

    const packed_func = try FunctionArgs.fromDataSpan(func_span);
    const restored_func = packed_func.toDataSpan();
    try testing.expectEqual(func_span.start, restored_func.start);
    try testing.expectEqual(func_span.len, restored_func.len);

    // Test SmallCollections configuration (24, 8)
    const small_span = DataSpan{ .start = 10000000, .len = 50 };
    try testing.expect(SmallCollections.canFit(small_span));

    const packed_small = try SmallCollections.fromDataSpan(small_span);
    const restored_small = packed_small.toDataSpan();
    try testing.expectEqual(small_span.start, restored_small.start);
    try testing.expectEqual(small_span.len, restored_small.len);
}

test "PackedDataSpan compile-time validation" {
    // These should compile fine
    _ = PackedDataSpan(16, 16);
    _ = PackedDataSpan(20, 12);
    _ = PackedDataSpan(24, 8);
    _ = PackedDataSpan(1, 31);
    _ = PackedDataSpan(31, 1);

    // These would cause compile errors if uncommented:
    // _ = PackedDataSpan(16, 15); // doesn't sum to 32
    // _ = PackedDataSpan(0, 32);  // zero bits not allowed
    // _ = PackedDataSpan(33, 0);  // doesn't fit in u32
}
