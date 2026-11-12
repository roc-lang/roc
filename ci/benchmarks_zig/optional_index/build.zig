//! Compare optional-index representations using the production collections.
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const options = b.addOptions();
    options.addOption(bool, "dev_diagnostics", false);
    const collections = b.createModule(.{
        .root_source_file = b.path("../../../src/collections/mod.zig"),
        .target = target,
    });
    collections.addOptions("build_options", options);
    inline for (.{ false, true }) |direct| {
        const variant = b.addOptions();
        variant.addOption(bool, "direct", direct);
        const exe = b.addExecutable(.{
            .name = if (direct) "reserved-zero" else "biased",
            .root_module = b.createModule(.{
                .root_source_file = b.path("main.zig"),
                .target = target,
                .optimize = .fast,
                .link_libc = true,
            }),
        });
        exe.root_module.addImport("collections", collections);
        exe.root_module.addOptions("variant", variant);
        b.installArtifact(exe);
    }
}
