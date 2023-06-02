const builtin = @import("builtin");
const arch = builtin.cpu.arch;

comptime {
    switch (arch) {
        .x86_64 => {
            asm (@embedFile("memcpy-x86_64.S"));
        },
        .i386 => {
            asm (@embedFile("memcpy-i386.S"));
        },
        // TODO: add assembly implementations for other platforms.
        else => {},
    }
}

pub const memcpy =
    switch (arch) {
    .x86_64, .i386 => musl_memcpy,
    else => fallback_memcpy,
};

pub extern fn musl_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;

// Note: this is written to only support little endian targets.
// To support big endian, `<<` and `>>` wold need to be swapped.
pub fn fallback_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    var d = dest;
    var s = src;
    var n = len;
    while (@ptrToInt(s) % 4 != 0 and n != 0) : (n -= 1) {
        d[0] = s[0];
        d += 1;
        s += 1;
    }

    if (@ptrToInt(d) % 4 == 0) {
        var d4 = @alignCast(4, d);
        var s4 = @alignCast(4, s);
        while (n >= 16) : (n -= 16) {
            var d_u32 = @ptrCast([*]u32, d4);
            var s_u32 = @ptrCast([*]const u32, s4);
            d_u32[0] = s_u32[0];
            d_u32[1] = s_u32[1];
            d_u32[2] = s_u32[2];
            d_u32[3] = s_u32[3];

            d4 += 16;
            s4 += 16;
        }
        if (n & 8 != 0) {
            var d_u32 = @ptrCast([*]u32, d4);
            var s_u32 = @ptrCast([*]const u32, s4);
            d_u32[0] = s_u32[0];
            d_u32[1] = s_u32[1];

            d4 += 8;
            s4 += 8;
        }
        if (n & 4 != 0) {
            var d_u32 = @ptrCast([*]u32, d4);
            var s_u32 = @ptrCast([*]const u32, s4);
            d_u32[0] = s_u32[0];

            d4 += 4;
            s4 += 4;
        }
        d = d4;
        s = s4;
        if (n & 2 != 0) {
            d[0] = s[0];
            d += 1;
            s += 1;
            d[0] = s[0];
            d += 1;
            s += 1;
        }
        if (n & 1 != 0) {
            d[0] = s[0];
        }
        return dest;
    }
    if (n >= 32) {
        switch (@ptrToInt(d) % 4) {
            1 => {
                var w = @ptrCast([*]const u32, @alignCast(4, s))[0];
                d[0] = s[0];
                d += 1;
                s += 1;
                d[0] = s[0];
                d += 1;
                s += 1;
                d[0] = s[0];
                d += 1;
                s += 1;
                n -= 3;
                while (n >= 17) : (n -= 16) {
                    var d_u32 = @ptrCast([*]u32, @alignCast(4, d));
                    var s_u32 = @ptrCast([*]const u32, @alignCast(4, s + 1));
                    var x = s_u32[0];
                    d_u32[0] = (w >> 24) | (x << 8);
                    w = s_u32[1];
                    d_u32[1] = (x >> 24) | (w << 8);
                    x = s_u32[2];
                    d_u32[2] = (w >> 24) | (x << 8);
                    w = s_u32[3];
                    d_u32[3] = (x >> 24) | (w << 8);

                    d += 16;
                    s += 16;
                }
            },
            2 => {
                var w = @ptrCast([*]const u32, @alignCast(4, s))[0];
                d[0] = s[0];
                d += 1;
                s += 1;
                d[0] = s[0];
                d += 1;
                s += 1;
                n -= 2;
                while (n >= 18) : (n -= 16) {
                    var d_u32 = @ptrCast([*]u32, @alignCast(4, d));
                    var s_u32 = @ptrCast([*]const u32, @alignCast(4, s + 2));
                    var x = s_u32[0];
                    d_u32[0] = (w >> 16) | (x << 16);
                    w = s_u32[1];
                    d_u32[1] = (x >> 16) | (w << 16);
                    x = s_u32[2];
                    d_u32[2] = (w >> 16) | (x << 16);
                    w = s_u32[3];
                    d_u32[3] = (x >> 16) | (w << 16);

                    d += 16;
                    s += 16;
                }
            },
            3 => {
                var w = @ptrCast([*]const u32, @alignCast(4, s))[0];
                d[0] = s[0];
                d += 1;
                s += 1;
                n -= 1;
                while (n >= 19) : (n -= 16) {
                    var d_u32 = @ptrCast([*]u32, @alignCast(4, d));
                    var s_u32 = @ptrCast([*]const u32, @alignCast(4, s + 3));
                    var x = s_u32[0];
                    d_u32[0] = (w >> 8) | (x << 24);
                    w = s_u32[1];
                    d_u32[1] = (x >> 8) | (w << 24);
                    x = s_u32[2];
                    d_u32[2] = (w >> 8) | (x << 24);
                    w = s_u32[3];
                    d_u32[3] = (x >> 8) | (w << 24);

                    d += 16;
                    s += 16;
                }
            },
            else => unreachable,
        }
    }
    if (n & 16 != 0) {
        comptime var i = 0;
        inline while (i < 16) : (i += 1) {
            d[0] = s[0];
            d += 1;
            s += 1;
        }
    }
    if (n & 8 != 0) {
        comptime var i = 0;
        inline while (i < 8) : (i += 1) {
            d[0] = s[0];
            d += 1;
            s += 1;
        }
    }
    if (n & 4 != 0) {
        comptime var i = 0;
        inline while (i < 4) : (i += 1) {
            d[0] = s[0];
            d += 1;
            s += 1;
        }
    }
    if (n & 2 != 0) {
        d[0] = s[0];
        d += 1;
        s += 1;
        d[0] = s[0];
        d += 1;
        s += 1;
    }
    if (n & 1 != 0) {
        d[0] = s[0];
    }
    return dest;
}
