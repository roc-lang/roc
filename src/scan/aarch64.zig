const std = @import("std");

/// High-performance NEON implementation for ARM64
/// Uses the "simdjson-style" movemask approach with vpaddl instructions
pub fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    // Convert bool vector to u8 (0xff for true, 0x00 for false)
    const u8_vec = @as(@Vector(16, u8), @bitCast(vec));

    // Use NEON vpaddl instructions to efficiently extract the bitmask
    return asm (
        \\ushr v1.16b, %[vec].16b, #7
        \\uaddlp v1.8h, v1.16b
        \\uaddlp v1.4s, v1.8h
        \\uaddlp v1.2d, v1.4s
        \\ushr v2.2d, v1.2d, #32
        \\sli v1.2d, v2.2d, #8
        \\umov %w[ret], v1.h[0]
        : [ret] "=r" (-> u16),
        : [vec] "w" (u8_vec),
        : "v1", "v2"
    );
}
