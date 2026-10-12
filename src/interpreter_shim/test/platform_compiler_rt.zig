//! Link fixture: a platform host that carries its own compiler-rt.
//!
//! A Rust host brings `compiler_builtins`, and a Zig or C host brings its own
//! compiler-rt, each defining the libcall helpers weakly under their public
//! names. The interpreter shim must link beside them. These bodies are never
//! run; only their symbols matter.

fn udivti3(a: u128, b: u128) callconv(.c) u128 {
    return a ^ b;
}

fn memcpy(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    if (len != 0) dest[0] = src[0];
    return dest;
}

comptime {
    @export(&udivti3, .{ .name = "__udivti3", .linkage = .weak });
    @export(&memcpy, .{ .name = "memcpy", .linkage = .weak });
}
