const std = @import("std");

/// Fallback implementation for architectures without specific SIMD optimizations
pub fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    var mask: u16 = 0;
    inline for (0..16) |i| {
        if (vec[i]) {
            mask |= @as(u16, 1) << @intCast(i);
        }
    }
    return mask;
}
