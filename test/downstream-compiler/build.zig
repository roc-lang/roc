const std = @import("std");

/// Build an executable using only Roc’s exported Zig modules.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const roc = b.dependency("roc", .{ .target = target, .optimize = optimize });
    const exe = b.addExecutable(.{
        .name = "downstream-compiler-smoke",
        .root_module = b.createModule(.{
            .root_source_file = b.path("main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    inline for (.{ "compile", "lir", "eval", "check", "base", "ctx", "builtins", "roc_target", "build_options" }) |name| {
        exe.root_module.addImport(name, roc.module(name));
    }
    b.installArtifact(exe);
}
