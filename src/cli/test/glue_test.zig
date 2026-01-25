//! Integration tests for the roc glue command.

const std = @import("std");
const util = @import("util.zig");

test "glue command with fx platform succeeds" {
    const allocator = std.testing.allocator;

    // Run: roc glue src/glue/src/DebugGlue.roc test/fx/platform/main.roc
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        "src/glue/src/DebugGlue.roc",
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully
    try std.testing.expect(result.term == .Exited and result.term.Exited == 0);

    // DBG output goes to stderr - should show the actual name, not empty string
    // If the small string encoding issue is present, we'd see name: ""
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "name: \"\"") == null);

    // Should show the actual entry point name from the platform header
    // For fx platform, the requires entry is "main!" with type "() => {}"
    // ROC DBG output goes to stderr
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "name: \"main!\"") != null);
}
