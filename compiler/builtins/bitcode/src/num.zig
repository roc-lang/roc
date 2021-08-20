const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const math = std.math;
const RocList = @import("list.zig").RocList;

pub fn atan(num: f64) callconv(.C) f64 {
    return @call(.{ .modifier = always_inline }, math.atan, .{num});
}

pub fn isFinite(num: f64) callconv(.C) bool {
    return @call(.{ .modifier = always_inline }, math.isFinite, .{num});
}

pub fn powInt(base: i64, exp: i64) callconv(.C) i64 {
    return @call(.{ .modifier = always_inline }, math.pow, .{ i64, base, exp });
}

pub fn acos(num: f64) callconv(.C) f64 {
    return @call(.{ .modifier = always_inline }, math.acos, .{num});
}

pub fn asin(num: f64) callconv(.C) f64 {
    return @call(.{ .modifier = always_inline }, math.asin, .{num});
}

pub fn bytesToU16C(arg: RocList, position: usize) callconv(.C) u16 {
    return @call(.{ .modifier = always_inline }, bytesToU16, .{ arg, position });
}

fn bytesToU16(arg: RocList, position: usize) u16 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u16, [_]u8{ bytes[position], bytes[position + 1] });
}

pub fn bytesToU32C(arg: RocList, position: usize) callconv(.C) u32 {
    return @call(.{ .modifier = always_inline }, bytesToU32, .{ arg, position });
}

fn bytesToU32(arg: RocList, position: usize) u32 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u32, [_]u8{ bytes[position], bytes[position + 1], bytes[position + 2], bytes[position + 3] });
}

pub fn round(num: f64) callconv(.C) i64 {
    return @floatToInt(i32, (@round(num)));
}
