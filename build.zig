const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const main_path = b.path("src/main.zig");

    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = main_path,
        .target = target,
        .optimize = optimize,
    });

    const main_tests = b.addTest(.{ .root_source_file = main_path });
    const test_cmd = b.addRunArtifact(main_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&test_cmd.step);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Build and run the roc cli");
    run_step.dependOn(&run_cmd.step);
}
