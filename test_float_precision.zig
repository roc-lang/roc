const std = @import("std");

pub fn main() void {
    std.debug.print("f32 max: {e}\n", .{std.math.floatMax(f32)});
    std.debug.print("f32 min: {e}\n", .{std.math.floatMin(f32)});
    std.debug.print("f64 max: {e}\n", .{std.math.floatMax(f64)});
    std.debug.print("f64 min: {e}\n", .{std.math.floatMin(f64)});
    
    // Test some values
    const values = [_]f64{ 3.14, 1.0, -10.5, 1e10, 3.4e38, 3.5e38, 1e-40, 1.23456789012345 };
    for (values) |v| {
        const as_f32 = @as(f32, @floatCast(v));
        const back = @as(f64, @floatCast(as_f32));
        std.debug.print("{}: f32={e}, back={e}, equal={}\n", .{v, as_f32, back, v == back});
    }
}
