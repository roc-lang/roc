const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const math = std.math;

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

/// TODO: Obviously, this should not be an alias for arcsin(x);
/// fix me!
pub fn bytesToU16C(num: f64) callconv(.C) f64 {
    return @call(.{ .modifier = always_inline }, math.asin, .{num});
}
