//! `memmove` for an MSVC-ABI image without vcruntime. See `memcpy.zig` for
//! why each of these routines is its own strong archive member.

export fn memmove(dest: ?[*]u8, src: ?[*]const u8, len: usize) callconv(.c) ?[*]u8 {
    @disableIntrinsics();
    if (@intFromPtr(dest) < @intFromPtr(src)) {
        for (0..len) |i| dest.?[i] = src.?[i];
    } else {
        var i = len;
        while (i > 0) {
            i -= 1;
            dest.?[i] = src.?[i];
        }
    }
    return dest;
}
