const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "test_struct_sizes",
        .root_source_file = b.path("test_struct_sizes.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add dependencies
    const base_mod = b.createModule(.{ .root_source_file = b.path("src/base/base.zig") });
    const can_mod = b.createModule(.{ .root_source_file = b.path("src/canonicalize/can.zig") });
    const types_mod = b.createModule(.{ .root_source_file = b.path("src/types/types.zig") });
    const collections_mod = b.createModule(.{ .root_source_file = b.path("src/collections/collections.zig") });
    const serialization_mod = b.createModule(.{ .root_source_file = b.path("src/serialization/serialization.zig") });
    
    base_mod.addImport("collections", collections_mod);
    base_mod.addImport("serialization", serialization_mod);
    
    can_mod.addImport("base", base_mod);
    can_mod.addImport("types", types_mod);
    can_mod.addImport("collections", collections_mod);
    
    types_mod.addImport("base", base_mod);
    types_mod.addImport("collections", collections_mod);

    exe.root_module.addImport("base", base_mod);
    exe.root_module.addImport("can", can_mod);
    exe.root_module.addImport("types", types_mod);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run-test-sizes", "Run struct size test");
    run_step.dependOn(&run_cmd.step);
}
