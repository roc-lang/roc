const std = @import("std");
const builtin = @import("builtin");
const InstallDir = std.Build.InstallDir;
const Step = std.Build.Step;
const LazyPath = std.Build.LazyPath;
const ResolvedTarget = std.Build.ResolvedTarget;
const OptimizeMode = std.builtin.OptimizeMode;
const Import = std.Build.Module.Import;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{ .default_target = .{
        .abi = if (builtin.target.os.tag == .linux) .musl else null,
    } });
    const optimize = b.standardOptimizeOption(.{});

    // Zig unicode library - https://codeberg.org/atman/zg
    const zg = b.dependency("zg", .{});

    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    exe.root_module.addImport("GenCatData", zg.module("GenCatData"));

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
        .link_libc = true,
    });
    all_tests.root_module.addImport("GenCatData", zg.module("GenCatData"));

    // Install the test binary so we can run separately
    // ```sh
    // $ zig build && ./zig-out/bin/test
    // ```
    b.installArtifact(all_tests);

    const run_tests = b.addRunArtifact(all_tests);
    const test_step = b.step("test", "Run all tests included in src/tests.zig");
    test_step.dependOn(&run_tests.step);

    // Fmt zig code.
    const fmt = b.addFmt(.{ .paths = &.{ "src", "build.zig" } });
    const fmt_step = b.step("fmt", "Format all zig code");
    fmt_step.dependOn(&fmt.step);

    const fuzz = b.option(bool, "fuzz", "Build fuzz targets including AFL++ and tooling") orelse false;
    const valid_fuzz_target = target.query.isNativeCpu() and target.query.isNativeOs() and (target.query.isNativeAbi() or target.result.abi.isMusl()) and target.result.os.tag != .windows;
    if (fuzz and valid_fuzz_target) {
        const afl = b.lazyImport(@This(), "zig-afl-kit") orelse return;

        // TODO: this just builds the fuzz target. Afterwards, they are still awkward to orchestrate and run.
        // Make a script to manage the corpus and run the fuzzers (or at least some good docs)
        // Likely will should check in a minimal corpus somewhere so we don't always start from zero.
        add_fuzz_target(
            b,
            afl,
            test_step,
            target,
            "cli",
            b.path("src/fuzz/cli.zig"),
            &[_]Import{
                .{ .name = "cli", .module = b.createModule(.{ .root_source_file = b.path("src/cli.zig") }) },
            },
        );
    }
}

fn add_fuzz_target(
    b: *std.Build,
    afl: type,
    parent_step: *Step,
    target: ResolvedTarget,
    name: []const u8,
    root_source_file: LazyPath,
    imports: []const Import,
) void {
    const fuzz_obj = b.addObject(.{
        .name = b.fmt("{s}_obj", .{name}),
        .root_source_file = root_source_file,
        .target = target,
        .optimize = .ReleaseSafe,
    });

    for (imports) |import| {
        fuzz_obj.root_module.addImport(import.name, import.module);
    }

    // TODO: Once 0.14.0 is released, uncomment this. Will make fuzzing work better.
    // Until then, to get the best fuzzing result modify the std library as specified here:
    // https://github.com/kristoff-it/zig-afl-kit?tab=readme-ov-file#-------important-------
    // fuzz_obj.root_module.fuzz = true;
    fuzz_obj.root_module.stack_check = false; // not linking with compiler-rt
    fuzz_obj.root_module.link_libc = true; // afl runtime depends on libc

    const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, fuzz_obj);

    const name_repro = b.fmt("repro-{s}", .{name});
    const repro = b.addExecutable(.{
        .name = name_repro,
        .root_source_file = b.path("src/fuzz/repro.zig"),
        .target = target,
        .optimize = .ReleaseSafe,
        .link_libc = true,
    });
    repro.addObject(fuzz_obj);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const fuzz_step = b.step(name_exe, b.fmt("Generate fuzz executable for {s}", .{name}));
    fuzz_step.dependOn(&b.addInstallBinFile(fuzz_exe, name_exe).step);
    fuzz_step.dependOn(&b.addInstallBinFile(repro.getEmittedBin(), name_repro).step);

    parent_step.dependOn(fuzz_step);
}
