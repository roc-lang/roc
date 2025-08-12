//! Just a small struct to take a span of data in an array

const DataSpan = @This();

start: u32,
len: u32,

/// Creates an empty DataSpan with zero start and zero length.
pub fn empty() DataSpan {
    return DataSpan{ .start = 0, .len = 0 };
}

/// Creates a DataSpan with the specified start position and length.
pub fn init(start: u32, len: u32) DataSpan {
    return DataSpan{ .start = start, .len = len };
}

/// Converts this DataSpan into a type that contains a span field.
/// This is useful for creating wrapper types around DataSpan.
pub fn as(self: DataSpan, comptime T: type) T {
    return @as(T, .{ .span = self });
}
