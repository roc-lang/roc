const std = @import("std");

/// Optimized x86-64 implementation using pmovmskb instruction
pub fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    // Convert bool vector to u8 (0xff for true, 0x00 for false)
    const u8_vec = @as(@Vector(16, u8), @bitCast(vec));

    // Use pmovmskb to extract the MSB of each byte into a bitmask
    return asm ("pmovmskb %[vec], %[ret]"
        : [ret] "=r" (-> u16),
        : [vec] "x" (u8_vec),
    );
}
