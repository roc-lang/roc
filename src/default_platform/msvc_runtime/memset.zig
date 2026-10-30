//! `memset` for an MSVC-ABI image without vcruntime. See `memcpy.zig` for
//! why each of these routines is its own strong archive member.

export fn memset(dest: ?[*]u8, byte: c_int, len: usize) callconv(.c) ?[*]u8 {
    @disableIntrinsics();
    for (0..len) |i| dest.?[i] = @truncate(@as(c_uint, @bitCast(byte)));
    return dest;
}
