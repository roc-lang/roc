//! `memcmp` for an MSVC-ABI image without vcruntime. See `memcpy.zig` for
//! why each of these routines is its own strong archive member.

export fn memcmp(left: [*]const u8, right: [*]const u8, len: usize) callconv(.c) c_int {
    @disableIntrinsics();
    for (0..len) |i| {
        if (left[i] != right[i]) return @as(c_int, left[i]) - @as(c_int, right[i]);
    }
    return 0;
}
