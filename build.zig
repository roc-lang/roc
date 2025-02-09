const std = @import("std");
const builtin = @import("builtin");
const afl = @import("zig-afl-kit");
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

    // Fuzz targets
    const fuzz = b.step("fuzz", "Generate all fuzz executables");

    // TODO: this just builds the fuzz target. Afterwards, they are still awkward to orchestrate and run.
    // Make a script to manage the corpus and run the fuzzers (or at least some good docs)
    // Likely will should check in a minimal corpus somewhere so we don't always start from zero.
    add_fuzz_target(
        b,
        fuzz,
        target,
        "cli",
        b.path("src/fuzz/cli.zig"),
        &[_]Import{
            .{ .name = "cli", .module = b.createModule(.{ .root_source_file = b.path("src/cli.zig") }) },
        },
    );
}

fn add_fuzz_target(
    b: *std.Build,
    fuzz: *Step,
    target: ResolvedTarget,
    name: []const u8,
    root_source_file: LazyPath,
    imports: []const Import,
) void {
    var name_obj = std.ArrayList(u8).init(b.allocator);
    defer name_obj.deinit();
    name_obj.writer().print("{s}_obj", .{name}) catch unreachable;

    var name_exe = std.ArrayList(u8).init(b.allocator);
    defer name_exe.deinit();
    name_exe.writer().print("fuzz-{s}", .{name}) catch unreachable;

    var name_repro = std.ArrayList(u8).init(b.allocator);
    defer name_repro.deinit();
    name_repro.writer().print("repro-{s}", .{name}) catch unreachable;

    var step_msg = std.ArrayList(u8).init(b.allocator);
    defer step_msg.deinit();
    step_msg.writer().print("Generate fuzz executable for {s}", .{name}) catch unreachable;

    const fuzz_obj = b.addObject(.{
        .name = name_obj.items,
        .root_source_file = root_source_file,
        .target = target,
        .optimize = .ReleaseSafe,
    });

    for (imports) |import| {
        fuzz_obj.root_module.addImport(import.name, import.module);
    }

    // TODO: Once 0.14.0 is released, uncomment this. Will make fuzzing work better.
    // fuzz_obj.root_module.fuzz = true;
    fuzz_obj.root_module.stack_check = false; // not linking with compiler-rt
    fuzz_obj.root_module.link_libc = true; // afl runtime depends on libc

    const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, fuzz_obj);

    const repro = b.addExecutable(.{
        .name = name_repro.items,
        .root_source_file = b.path("src/fuzz/repro.zig"),
        .target = target,
        .optimize = .ReleaseSafe,
        .link_libc = true,
    });
    repro.addObject(fuzz_obj);

    const fuzz_step = b.step(name_exe.items, step_msg.items);
    fuzz_step.dependOn(&b.addInstallBinFile(fuzz_exe, name_exe.items).step);
    fuzz_step.dependOn(&b.addInstallBinFile(repro.getEmittedBin(), name_repro.items).step);

    fuzz.dependOn(fuzz_step);
}
