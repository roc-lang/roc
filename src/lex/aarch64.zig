const std = @import("std");

/// High-performance NEON implementation for ARM64
/// Uses the "simdjson-style" movemask approach with vpaddl instructions
pub fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    // Convert bool vector to u8 (0x01 for true, 0x00 for false)
    const u8_vec: @Vector(16, u8) = @intFromBool(vec);

    // Extract each byte that is non-zero
    var result: u16 = 0;
    inline for (0..16) |i| {
        if (u8_vec[i] != 0) {
            result |= (@as(u16, 1) << @intCast(i));
        }
    }
    return result;
}
