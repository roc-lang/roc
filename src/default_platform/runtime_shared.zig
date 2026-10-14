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

/// Formats `value` in decimal into the end of `buf`, returning the digits.
pub fn formatUnsigned(buf: *[20]u8, value: u64) []const u8 {
    var index = buf.len;
    var n = value;
    while (true) {
        index -= 1;
        buf[index] = '0' + @as(u8, @intCast(n % 10));
        n /= 10;
        if (n == 0) break;
    }
    return buf[index..];
}

/// `memcpy` for runtimes that link no C library.
pub fn defaultMemcpy(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    var i: usize = 0;
    while (i < len) : (i += 1) dest[i] = src[i];
    return dest;
}

/// `memmove` for runtimes that link no C library.
pub fn defaultMemmove(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    if (@intFromPtr(dest) <= @intFromPtr(src)) {
        var i: usize = 0;
        while (i < len) : (i += 1) dest[i] = src[i];
    } else {
        var i = len;
        while (i != 0) {
            i -= 1;
            dest[i] = src[i];
        }
    }
    return dest;
}

/// `memset` for runtimes that link no C library. The stores are volatile so
/// the compiler cannot turn this loop back into a call to `memset` itself.
pub fn defaultMemset(dest: [*]u8, value: c_int, len: usize) callconv(.c) [*]u8 {
    const byte: u8 = @bitCast(@as(i8, @truncate(value)));
    const volatile_dest: [*]volatile u8 = dest;
    var i: usize = 0;
    while (i < len) : (i += 1) volatile_dest[i] = byte;
    return dest;
}
