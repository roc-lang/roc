const std = @import("std");
const rocbuiltins = @import("rocbuiltins");

pub fn main() !void {
    var buffer: [256]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buffer);
    try stdout.interface.print(
        "size={d} byte_index={d} string={d} is_ok={d} problem_code={d} rocstr_size={d}\n",
        .{
            @sizeOf(rocbuiltins.str.FromUtf8Try),
            @offsetOf(rocbuiltins.str.FromUtf8Try, "byte_index"),
            @offsetOf(rocbuiltins.str.FromUtf8Try, "string"),
            @offsetOf(rocbuiltins.str.FromUtf8Try, "is_ok"),
            @offsetOf(rocbuiltins.str.FromUtf8Try, "problem_code"),
            @sizeOf(rocbuiltins.str.RocStr),
        },
    );
    try stdout.interface.flush();
}
