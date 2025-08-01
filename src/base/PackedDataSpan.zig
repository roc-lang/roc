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
const mod = @import("mod.zig");
const DataSpan = mod.DataSpan;

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
/// Good for function arguments and method calls (up to 4095 args, 1M+ start positions)
pub const FunctionArgs = PackedDataSpan(20, 12);

/// Good for small collections with high start range (up to 255 items, 16M+ start positions)
pub const SmallCollections = PackedDataSpan(24, 8);

/// Balanced allocation (up to 65K items, 65K+ start positions)
pub const Balanced = PackedDataSpan(16, 16);

/// Good for large collections with lower start range (up to 1M items, 4K+ start positions)
pub const LargeCollections = PackedDataSpan(12, 20);
