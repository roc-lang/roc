//! Integration tests for the roc glue command.

const std = @import("std");
const util = @import("util.zig");

test "glue command with DebugGlue succeeds" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc glue src/glue/src/DebugGlue.roc <tmp_path> test/fx/platform/main.roc
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        "src/glue/src/DebugGlue.roc",
        tmp_path,
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

test "glue command with CGlue generates expected C header" {
    const allocator = std.testing.allocator;

    // Create temp directory for output
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = tmp_dir.dir.realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(tmp_path);

    // Run: roc glue src/glue/src/CGlue.roc <tmp_path> test/fx/platform/main.roc
    const result = try util.runRocCommand(allocator, &.{
        "glue",
        "src/glue/src/CGlue.roc",
        tmp_path,
        "test/fx/platform/main.roc",
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    // Should not panic
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "PANIC") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.stderr, "unreachable") == null);

    // Should complete successfully
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("\nCGlue command failed!\nstderr:\n{s}\nstdout:\n{s}\n", .{ result.stderr, result.stdout });
        try std.testing.expect(false);
    }

    // Read the generated header file
    const generated_path = std.fs.path.join(allocator, &.{ tmp_path, "roc_platform_abi.h" }) catch unreachable;
    defer allocator.free(generated_path);

    const generated_content = std.fs.cwd().readFileAlloc(allocator, generated_path, 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read generated file '{s}': {}\n", .{ generated_path, err });
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(generated_content);

    // Read the expected header file
    const expected_content = std.fs.cwd().readFileAlloc(allocator, "test/glue/fx_platform_cglue_expected.h", 1024 * 1024) catch |err| {
        std.debug.print("\nFailed to read expected file: {}\n", .{err});
        try std.testing.expect(false);
        unreachable;
    };
    defer allocator.free(expected_content);

    // Compare generated vs expected
    if (!std.mem.eql(u8, generated_content, expected_content)) {
        std.debug.print("\n\nGenerated C header does not match expected!\n", .{});
        std.debug.print("\n--- Expected ({d} bytes) ---\n{s}\n", .{ expected_content.len, expected_content });
        std.debug.print("\n--- Generated ({d} bytes) ---\n{s}\n", .{ generated_content.len, generated_content });

        // Find first difference for easier debugging
        var i: usize = 0;
        while (i < @min(expected_content.len, generated_content.len)) : (i += 1) {
            if (expected_content[i] != generated_content[i]) {
                const start = if (i > 20) i - 20 else 0;
                const end_expected = @min(i + 20, expected_content.len);
                const end_generated = @min(i + 20, generated_content.len);
                std.debug.print("\nFirst difference at byte {d}:\n", .{i});
                std.debug.print("  Expected:  ...{s}...\n", .{expected_content[start..end_expected]});
                std.debug.print("  Generated: ...{s}...\n", .{generated_content[start..end_generated]});
                break;
            }
        }

        try std.testing.expect(false);
    }
}
