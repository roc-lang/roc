const std = @import("std");
const Build = std.Build;
const Import = Build.Module.Import;
const StackVmKind = enum { tailcall, labeled_switch };

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const enable_metering = b.option(bool, "meter", "Enable metering (default: false)") orelse false;
    const enable_debug_trace = b.option(bool, "debug_trace", "Enable debug tracing feature (default: false)") orelse false;
    const enable_debug_trap = b.option(bool, "debug_trap", "Enable debug trap features (default: false)") orelse false;
    const enable_wasi = b.option(bool, "wasi", "Enable wasi support (default: true if target has support)") orelse blk: {
        if (target.result.cpu.arch.isWasm() and target.result.os.tag != .wasi) {
            break :blk false;
        }
        break :blk true;
    };
    const vm_kind = b.option(
        StackVmKind,
        "vm_kind",
        "Determines which stack vm implementation to use. You may want to benchmark which one fits your usecase best.",
    ) orelse StackVmKind.labeled_switch;

    const options = b.addOptions();
    options.addOption(bool, "enable_metering", enable_metering);
    options.addOption(bool, "enable_debug_trace", enable_debug_trace);
    options.addOption(bool, "enable_debug_trap", enable_debug_trap);
    options.addOption(bool, "enable_wasi", enable_wasi);
    options.addOption(StackVmKind, "vm_kind", vm_kind);

    const stable_array = b.dependency("stable_array", .{
        .target = target,
        .optimize = optimize,
    });

    const stable_array_import = Import{ .name = "stable-array", .module = stable_array.module("zig-stable-array") };

    const bytebox_module: *Build.Module = b.addModule("bytebox", .{
        .root_source_file = b.path("src/core.zig"),
        .imports = &[_]Import{stable_array_import},
    });

    bytebox_module.addOptions("config", options);

    const smoke_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("roc_smoke_test.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "bytebox", .module = bytebox_module }},
        }),
    });
    const run_smoke_tests = b.addRunArtifact(smoke_tests);
    b.step("test", "Validate the vendored VM on a WebAssembly module").dependOn(&run_smoke_tests.step);
}
