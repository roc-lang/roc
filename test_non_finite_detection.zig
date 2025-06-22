const std = @import("std");

pub fn main() void {
    const test_values = [_]struct { name: []const u8, value: f64 }{
        .{ .name = "NaN", .value = std.math.nan(f64) },
        .{ .name = "Infinity", .value = std.math.inf(f64) },
        .{ .name = "-Infinity", .value = -std.math.inf(f64) },
        .{ .name = "3.14", .value = 3.14 },
        .{ .name = "0.0", .value = 0.0 },
    };
    
    for (test_values) |tv| {
        const is_non_finite = std.math.isNan(tv.value) or std.math.isInf(tv.value);
        std.debug.print("{s}: is_non_finite = {}\n", .{ tv.name, is_non_finite });
    }
}
