//! TODO

const std = @import("std");
const builtins = @import("builtins");

const Wyhash = builtins.hash.Wyhash;

test "test vectors" {
    const hash = Wyhash.hash;

    try std.testing.expectEqual(hash(0, ""), 0x0);
    try std.testing.expectEqual(hash(1, "a"), 0xbed235177f41d328);
    try std.testing.expectEqual(hash(2, "abc"), 0xbe348debe59b27c3);
    try std.testing.expectEqual(hash(3, "message digest"), 0x37320f657213a290);
    try std.testing.expectEqual(hash(4, "abcdefghijklmnopqrstuvwxyz"), 0xd0b270e1d8a7019c);
    try std.testing.expectEqual(hash(5, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"), 0x602a1894d3bbfe7f);
    try std.testing.expectEqual(hash(6, "12345678901234567890123456789012345678901234567890123456789012345678901234567890"), 0x829e9c148b75970e);
}

test "test vectors streaming" {
    var wh = Wyhash.init(5);
    for ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") |e| {
        wh.update(std.mem.asBytes(&e));
    }
    try std.testing.expectEqual(wh.final(), 0x602a1894d3bbfe7f);

    const pattern = "1234567890";
    const count = 8;
    const result = 0x829e9c148b75970e;
    try std.testing.expectEqual(Wyhash.hash(6, pattern ** 8), result);

    wh = Wyhash.init(6);
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        wh.update(pattern);
    }
    try std.testing.expectEqual(wh.final(), result);
}

test "iterative non-divisible update" {
    var buf: [8192]u8 = undefined;
    for (&buf, 0..) |*e, i| {
        e.* = @as(u8, @truncate(i));
    }

    const seed = 0x128dad08f;

    var end: usize = 32;
    while (end < buf.len) : (end += 32) {
        const non_iterative_hash = Wyhash.hash(seed, buf[0..end]);

        var wy = Wyhash.init(seed);
        var i: usize = 0;
        while (i < end) : (i += 33) {
            wy.update(buf[i..@min(i + 33, end)]);
        }
        const iterative_hash = wy.final();

        try std.testing.expectEqual(iterative_hash, non_iterative_hash);
    }
}
