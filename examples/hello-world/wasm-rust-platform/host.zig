const std = @import("std");

//extern int rust_main();
extern fn rust_main() callconv(.C) i64;

//int main() { return rust_main(); }
pub fn main() u8 {
    const stdout = std.io.getStdOut().writer();

    const result = rust_main();

    stdout.print("{d}\n", .{result}) catch unreachable;

    return 0;
}