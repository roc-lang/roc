const std = @import("std");
const repl = @import("src/repl/eval.zig");

test "debug anno_only_crash" {
    const gpa = std.testing.allocator;

    // Initialize REPL
    var replInst = try repl.Repl.init(gpa);
    defer replInst.deinit();

    // First line: foo : Str -> Str
    const line1_output = try replInst.processInput("foo : Str -> Str");
    defer gpa.free(line1_output);
    std.debug.print("\nLine 1 output: {s}\n", .{line1_output});

    // Second line: foo("test")
    const line2_output = try replInst.processInput("foo(\"test\")");
    defer gpa.free(line2_output);
    std.debug.print("Line 2 output: {s}\n", .{line2_output});
}
