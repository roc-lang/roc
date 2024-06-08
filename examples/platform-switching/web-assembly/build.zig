const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "host",
        .root_source_file = .{ .path = "host/main.zig" },
        // hardcode build for -Dtarget=wasm32-wasi-musl target
        .target = std.zig.CrossTarget{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
            .abi = .musl,
        },
        .optimize = optimize,
    });

    lib.dead_strip_dylibs = false;

    b.installArtifact(lib);
}
