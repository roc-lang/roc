const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;
const function_prefix = @import("../assembly_util.zig").function_prefix;

comptime {
    switch (arch) {
        .x86_64 => {
            asm (std.fmt.comptimePrint(@embedFile("memcpy-x86_64.S"), .{ .function_prefix = function_prefix }));
        },
        .x86 => {
            asm (std.fmt.comptimePrint(@embedFile("memcpy-x86.S"), .{ .function_prefix = function_prefix }));
        },
        // TODO: add assembly implementations for other platforms.
        else => {},
    }
}

pub const memcpy =
    switch (builtin.os.tag) {
    .windows => fallback_memcpy,
    else => switch (arch) {
        .x86_64, .x86 => musl_memcpy,
        else => fallback_memcpy,
    },
};

pub extern fn musl_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;

// Note: this is written to only support little endian targets.
// To support big endian, `<<` and `>>` wold need to be swapped.
pub fn fallback_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    var d = dest;
    var s = src;
    var n = len;
    switch (@min(n, @intFromPtr(s) % 4)) {
        1 => {
            d[0] = s[0];
            d += 1;
            s += 1;
            n -= 1;
        },
        2 => {
            d[0] = s[0];
            d[1] = s[1];
            d += 2;
            s += 2;
            n -= 2;
        },
        3 => {
            d[0] = s[0];
            d[1] = s[1];
            d[2] = s[2];
            d += 3;
            s += 3;
            n -= 3;
        },
        else => {},
    }

    if (@intFromPtr(d) % 4 == 0) {
        var d4 = @as([*]align(4) u8, @alignCast(d));
        var s4 = @as([*]align(4) const u8, @alignCast(s));
        while (n >= 16) : (n -= 16) {
            var d_u32 = @as([*]u32, @ptrCast(d4));
            var s_u32 = @as([*]const u32, @ptrCast(s4));
            d_u32[0] = s_u32[0];
            d_u32[1] = s_u32[1];
            d_u32[2] = s_u32[2];
            d_u32[3] = s_u32[3];

            d4 += 16;
            s4 += 16;
        }
        if (n & 8 != 0) {
            var d_u32 = @as([*]u32, @ptrCast(d4));
            var s_u32 = @as([*]const u32, @ptrCast(s4));
            d_u32[0] = s_u32[0];
            d_u32[1] = s_u32[1];

            d4 += 8;
            s4 += 8;
        }
        if (n & 4 != 0) {
            var d_u32 = @as([*]u32, @ptrCast(d4));
            var s_u32 = @as([*]const u32, @ptrCast(s4));
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
        switch (@intFromPtr(d) % 4) {
            1 => {
                var w = @as([*]const u32, @ptrCast(@alignCast(s)))[0];
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
                    var d_u32 = @as([*]u32, @ptrCast(@alignCast(d)));
                    var s_u32 = @as([*]const u32, @ptrCast(@alignCast(s + 1)));
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
                var w = @as([*]const u32, @ptrCast(@alignCast(s)))[0];
                d[0] = s[0];
                d += 1;
                s += 1;
                d[0] = s[0];
                d += 1;
                s += 1;
                n -= 2;
                while (n >= 18) : (n -= 16) {
                    var d_u32 = @as([*]u32, @ptrCast(@alignCast(d)));
                    var s_u32 = @as([*]const u32, @ptrCast(@alignCast(s + 2)));
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
                var w = @as([*]const u32, @ptrCast(@alignCast(s)))[0];
                d[0] = s[0];
                d += 1;
                s += 1;
                n -= 1;
                while (n >= 19) : (n -= 16) {
                    var d_u32 = @as([*]u32, @ptrCast(@alignCast(d)));
                    var s_u32 = @as([*]const u32, @ptrCast(@alignCast(s + 3)));
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
