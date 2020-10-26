const std = @import("std");
const testing = std.testing;

fn is_finite_(f: f64) bool {
    return std.math.isFinite(f);
}

fn atan(f: f64) f64 {
    return std.math.atan(f);
}

fn pow_int_(base: i64, exp: i64) i64 {
    return std.math.pow(i64, base, exp);
}

// test "basic add functionality" {
    // testing.expect(add(3, 7) == 10);
// }
