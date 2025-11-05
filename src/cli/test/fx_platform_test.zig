const std = @import("std");
const testing = std.testing;

test "fx platform effectful functions" {
    const allocator = testing.allocator;

    // Build the fx app
    const build_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{
            "./zig-out/bin/roc",
            "build",
            "test/fx/app.roc",
        },
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    switch (build_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Build failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
                std.debug.print("STDERR: {s}\n", .{build_result.stderr});
                return error.BuildFailed;
            }
        },
        else => {
            std.debug.print("Build terminated abnormally: {}\n", .{build_result.term});
            std.debug.print("STDOUT: {s}\n", .{build_result.stdout});
            std.debug.print("STDERR: {s}\n", .{build_result.stderr});
            return error.BuildFailed;
        },
    }

    // Run the app and capture stdout/stderr separately
    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{"./app"},
    });
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Clean up the app binary
    std.fs.cwd().deleteFile("./app") catch {};

    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Run failed with exit code {}\n", .{code});
                std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
                std.debug.print("STDERR: {s}\n", .{run_result.stderr});
                return error.RunFailed;
            }
        },
        else => {
            std.debug.print("Run terminated abnormally: {}\n", .{run_result.term});
            std.debug.print("STDOUT: {s}\n", .{run_result.stdout});
            std.debug.print("STDERR: {s}\n", .{run_result.stderr});
            return error.RunFailed;
        },
    }

    // Verify stdout contains expected messages
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Hello from stdout!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 1 to stdout") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 3 to stdout") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "ALL TESTS COMPLETED") != null);

    // Verify stderr contains expected messages
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Error from stderr!") != null);
    try testing.expect(std.mem.indexOf(u8, run_result.stderr, "Line 2 to stderr") != null);

    // Verify stderr messages are NOT in stdout
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Error from stderr!") == null);
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "Line 2 to stderr") == null);

    // Verify stdout messages are NOT in stderr (except the ones we intentionally put there for display)
    // Note: The host.zig writes "STDOUT: ..." to stdout and "STDERR: ..." to stderr
    // so we check that the actual content is separated correctly
}
