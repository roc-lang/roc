//! Cross-compilation test runner for fx platform tests.
//!
//! This standalone binary runs cross-compilation tests using the shared
//! test specs from fx_test_specs.zig. It:
//! 1. Cross-compiles each test app using `roc build --target=<target>`
//! 2. Reports success/failure for each compilation
//!
//! Usage:
//!   fx_cross_runner <roc_binary_path> <target>
//!
//! Example:
//!   fx_cross_runner ./zig-out/bin/roc x64musl

const std = @import("std");
const fx_test_specs = @import("fx_test_specs.zig");

/// Entry point for the cross-compilation test runner.
/// Iterates over all fx test specs and cross-compiles each one to the specified target.
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: fx_cross_runner <roc_binary_path> <target>\n", .{});
        std.debug.print("Example: fx_cross_runner ./zig-out/bin/roc x64musl\n", .{});
        std.process.exit(1);
    }

    const roc_binary = args[1];
    const target = args[2];

    std.debug.print("=== FX Platform Cross-Compilation Test Runner ===\n", .{});
    std.debug.print("Roc binary: {s}\n", .{roc_binary});
    std.debug.print("Target: {s}\n", .{target});
    std.debug.print("Total tests: {d}\n\n", .{fx_test_specs.io_spec_tests.len});

    var passed: usize = 0;
    var failed: usize = 0;

    for (fx_test_specs.io_spec_tests, 0..) |spec, i| {
        const test_num = i + 1;
        std.debug.print("[{d}/{d}] {s}... ", .{ test_num, fx_test_specs.io_spec_tests.len, spec.roc_file });

        // Generate output filename based on roc file and target
        const basename = std.fs.path.stem(spec.roc_file);
        const output_name = try std.fmt.allocPrint(allocator, "{s}_{s}", .{ basename, target });
        defer allocator.free(output_name);

        // Run roc build --target=<target> --output=<output> <roc_file>
        const result = std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{
                roc_binary,
                "build",
                try std.fmt.allocPrint(allocator, "--target={s}", .{target}),
                try std.fmt.allocPrint(allocator, "--output={s}", .{output_name}),
                spec.roc_file,
            },
        }) catch |err| {
            std.debug.print("FAIL (spawn error: {})\n", .{err});
            failed += 1;
            continue;
        };
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        switch (result.term) {
            .Exited => |code| {
                if (code == 0) {
                    std.debug.print("OK\n", .{});
                    passed += 1;

                    // Clean up the generated executable
                    std.fs.cwd().deleteFile(output_name) catch {};
                } else {
                    std.debug.print("FAIL (exit code {d})\n", .{code});
                    if (result.stderr.len > 0) {
                        // Print first few lines of error
                        var lines = std.mem.splitScalar(u8, result.stderr, '\n');
                        var line_count: usize = 0;
                        while (lines.next()) |line| {
                            if (line_count >= 5) {
                                std.debug.print("       ... (truncated)\n", .{});
                                break;
                            }
                            if (line.len > 0) {
                                std.debug.print("       {s}\n", .{line});
                                line_count += 1;
                            }
                        }
                    }
                    failed += 1;
                }
            },
            .Signal => |sig| {
                std.debug.print("FAIL (signal {d})\n", .{sig});
                failed += 1;
            },
            else => {
                std.debug.print("FAIL (abnormal termination)\n", .{});
                failed += 1;
            },
        }
    }

    // Print summary
    std.debug.print("\n=== Summary ===\n", .{});
    std.debug.print("Passed: {d}/{d}\n", .{ passed, passed + failed });
    std.debug.print("Failed: {d}/{d}\n", .{ failed, passed + failed });

    if (failed > 0) {
        std.debug.print("\nSome tests failed!\n", .{});
        std.process.exit(1);
    } else {
        std.debug.print("\nAll tests passed!\n", .{});
    }
}
