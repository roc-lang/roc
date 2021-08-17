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

/// TODO: Obviously, this function should do something more interesting
/// than return the number 40. Fix me!
pub fn bytesToU16C(arg: RocList, position: usize) callconv(.C) u16 {
    return @call(.{ .modifier = always_inline }, bytesToU16, .{arg, position});
}

fn bytesToU16(arg: RocList, position: usize) u16 {
    const exampleAnswer: u16 = 40;
    return 40;
}

/// TODO: Obviously, this function should do something more interesting
/// than return the number 40. Fix me!
pub fn bytesToU32C(arg: RocList, position: usize) callconv(.C) u32 {
    return @call(.{ .modifier = always_inline }, bytesToU32, .{arg, position});
}

fn bytesToU32(arg: RocList, position: usize) u32 {
    const exampleAnswer: u32 = 41;
    return 41;
}

pub fn round(num: f64) callconv(.C) i64 {
    return @floatToInt(i32, (@round(num)));
}
