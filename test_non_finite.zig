const std = @import("std");
const types = @import("src/types/types.zig");

pub fn main() void {
    // Test that PrecisionNeeded has the expected values
    std.debug.print("PrecisionNeeded enum values:\n", .{});
    std.debug.print("  f32 = {}\n", .{@intFromEnum(types.Num.Frac.PrecisionNeeded.f32)});
    std.debug.print("  f64 = {}\n", .{@intFromEnum(types.Num.Frac.PrecisionNeeded.f64)});
    std.debug.print("  dec = {}\n", .{@intFromEnum(types.Num.Frac.PrecisionNeeded.dec)});
    std.debug.print("  non_finite = {}\n", .{@intFromEnum(types.Num.Frac.PrecisionNeeded.non_finite)});
    
    // Test that Requirements can store non_finite
    const req = types.Num.Frac.Requirements{ .precision_needed = .non_finite };
    std.debug.print("\nRequirements with non_finite: precision_needed = {}\n", .{@intFromEnum(req.precision_needed)});
    
    // Verify it still fits in 2 bits
    const size = @sizeOf(types.Num.Frac.Requirements);
    std.debug.print("Size of Requirements: {} bytes\n", .{size});
}
