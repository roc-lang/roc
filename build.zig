const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Build and run the roc cli");
    run_step.dependOn(&run_cmd.step);

    const all_tests = b.addTest(.{
        .root_source_file = b.path("src/test.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Install the test binary so we can run separately
    // ```sh
    // $ zig build && ./zig-out/bin/test
    // ```
    b.installArtifact(all_tests);

    const run_tests = b.addRunArtifact(all_tests);

    const test_step = b.step("test", "Run all tests included in src/tests.zig");

    test_step.dependOn(&run_tests.step);
}
