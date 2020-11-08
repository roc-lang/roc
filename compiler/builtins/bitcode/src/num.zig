const std = @import("std");
const math = std.math;

pub fn atan(num: f64) callconv(.C) f64 {
    return math.atan(num);
}

pub fn isFinite(num: f64) callconv(.C) bool {
    return math.isFinite(num);
}

pub fn powInt(base: i64, exp: i64) callconv(.C) i64 {
    return math.pow(i64, base, exp);
}

pub fn acos(num: f64) callconv(.C) f64 {
    return math.acos(num);
}

pub fn asin(num: f64) callconv(.C) f64 {
    return math.asin(num);
}
