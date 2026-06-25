//! Byte reader for coverage-guided fuzz generators.

const Self = @This();

data: []const u8,
position: usize,

pub fn init(data: []const u8) Self {
    return .{
        .data = data,
        .position = 0,
    };
}

pub fn readByte(self: *Self) u8 {
    if (self.position >= self.data.len) return 0;
    const byte = self.data[self.position];
    self.position += 1;
    return byte;
}

pub fn boolean(self: *Self) bool {
    return (self.readByte() & 1) != 0;
}

pub fn intRangeAtMost(self: *Self, comptime T: type, min: T, max: T) T {
    if (min >= max) return min;

    const info = @typeInfo(T).int;
    const range: u64 = if (info.signedness == .signed) blk: {
        const diff = @as(i128, max) - @as(i128, min);
        break :blk @intCast(diff + 1);
    } else @as(u64, max - min) + 1;

    const bytes_needed = bytesForRange(range);
    var value: u64 = 0;
    for (0..bytes_needed) |i| {
        value |= @as(u64, self.readByte()) << @intCast(i * 8);
    }

    const offset = value % range;
    return if (info.signedness == .signed)
        min + @as(T, @intCast(offset))
    else
        min + @as(T, @intCast(offset));
}

pub fn intRangeLessThan(self: *Self, comptime T: type, min: T, max: T) T {
    if (min >= max) return min;
    return self.intRangeAtMost(T, min, max - 1);
}

fn bytesForRange(range: u64) usize {
    if (range <= 0x100) return 1;
    if (range <= 0x10000) return 2;
    if (range <= 0x1000000) return 3;
    if (range <= 0x100000000) return 4;
    if (range <= 0x10000000000) return 5;
    if (range <= 0x1000000000000) return 6;
    if (range <= 0x100000000000000) return 7;
    return 8;
}
