const std = @import("std");
const math = std.math;
// const testing = std.testing;


export fn atan_(num: f64) f64 {
    return math.atan(num);
}

export fn is_finite_(num: f64) bool {
    return math.isFinite(num);
}

export fn pow_int_(base: i64, exp: i64) i64 {
    return math.pow(i64, base, exp);
}

// test "basic add functionality" {
    // testing.expect(add(3, 7) == 10);
// }
