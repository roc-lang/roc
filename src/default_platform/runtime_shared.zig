//! Declarations every default app platform runtime uses unchanged, whatever
//! operating system it targets.
//!
//! Nothing here reaches the kernel or a C runtime, so each per-OS runtime root
//! can import this file directly.

/// One source-level frame of a crash's Roc call stack, as compiled code passes
/// it to the runtime.
pub const SourceFrame = extern struct {
    name_ptr: [*]const u8,
    name_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    line: u32,
    column: u32,
};

/// The alignment an allocation is given: at least a word, so its header words
/// are themselves aligned.
pub fn normalizedAlignment(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

/// Rounds `value` up to a multiple of `alignment`, which is a power of two.
pub fn alignForward(value: usize, alignment: usize) usize {
    return (value + alignment - 1) & ~(alignment - 1);
}

/// `trunc` for targets whose runtime links no C math library.
pub fn defaultTrunc(value: f64) callconv(.c) f64 {
    const bits: u64 = @bitCast(value);
    const exponent_bits = (bits >> 52) & 0x7ff;
    const exponent: i32 = @as(i32, @intCast(exponent_bits)) - 1023;

    if (exponent >= 52) return value;
    if (exponent < 0) return @bitCast(bits & (@as(u64, 1) << 63));

    const fraction_bits: u6 = @intCast(52 - exponent);
    const fraction_mask = (@as(u64, 1) << fraction_bits) - 1;
    return @bitCast(bits & ~fraction_mask);
}
