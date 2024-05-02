const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "fibonacci",
        .root_source_file = .{ .path = "host.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);
}
