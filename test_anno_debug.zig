const std = @import("std");
const Repl = @import("src/repl/eval.zig").Repl;

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    var repl = try Repl.init(gpa);
    defer repl.deinit();

    // Line 1: Str.is_empty("")
    std.debug.print("Line 1: Str.is_empty(\"\")\n", .{});
    const out1 = try repl.processInput("Str.is_empty(\"\")");
    defer gpa.free(out1);
    std.debug.print("Output 1: {s}\n\n", .{out1});

    // Line 2: Str.is_empty("a")
    std.debug.print("Line 2: Str.is_empty(\"a\")\n", .{});
    const out2 = try repl.processInput("Str.is_empty(\"a\")");
    defer gpa.free(out2);
    std.debug.print("Output 2: {s}\n", .{out2});
}
