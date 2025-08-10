//! Just a small struct to take a span of data in an array

const DataSpan = @This();

start: u32,
len: u32,

pub fn empty() DataSpan {
    return DataSpan{ .start = 0, .len = 0 };
}

pub fn init(start: u32, len: u32) DataSpan {
    return DataSpan{ .start = start, .len = len };
}

pub fn as(self: DataSpan, comptime T: type) T {
    return @as(T, .{ .span = self });
}
