const std = @import("std");

/// Optimized x86-64 implementation using pmovmskb instruction
pub fn vectorToBitmask(vec: @Vector(16, bool)) u16 {
    // Convert bool vector to u8 (0x01 for true, 0x00 for false)
    // Then expand to 0xFF for true to work with pmovmskb
    const u8_vec: @Vector(16, u8) = @intFromBool(vec);
    const expanded = u8_vec * @as(@Vector(16, u8), @splat(0xFF));

    // Use pmovmskb to extract the MSB of each byte into a bitmask
    return asm ("pmovmskb %[vec], %[ret]"
        : [ret] "=r" (-> u16),
        : [vec] "x" (expanded),
    );
}
