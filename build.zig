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

    const download_deps_step = b.step("download-deps", "Download all dependencies needed to compile roc");
    const llvm_config_paths = downloadDeps(b, download_deps_step, target);

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
    const fuzz_step = b.step("fuzz", "Generate all fuzz executables");
    fuzz_step.dependOn(download_deps_step);

    // TODO: this just builds the fuzz target. Afterwards, they are still awkward to orchestrate and run.
    // Make a script to manage the corpus and run the fuzzers (or at least some good docs)
    // Likely will should check in a minimal corpus somewhere so we don't always start from zero.
    addFuzzTarget(
        b,
        fuzz_step,
        target,
        download_deps_step,
        llvm_config_paths,
        "cli",
        b.path("src/fuzz/cli.zig"),
        &[_]Import{
            .{ .name = "cli", .module = b.createModule(.{ .root_source_file = b.path("src/cli.zig") }) },
        },
    );
}

fn downloadDeps(
    b: *std.Build,
    download_deps: *Step,
    target: ResolvedTarget,
) []const []const u8 {
    const llvm_config_paths: ?[]const []const u8 = b.option(
        []const []const u8,
        "llvm-config-paths",
        "Paths to search for llvm-config (if not set, will download vendored llvm)",
    );
    if (llvm_config_paths) |paths| {
        return b.dupeStrings(paths);
    }

    const target_str = target.result.linuxTriple(b.allocator) catch @panic("OOM");

    const download_deps_exe = b.addExecutable(.{
        .name = "download_deps",
        .root_source_file = b.path("src/download_deps.zig"),
        .target = b.host,
        .optimize = .Debug,
    });
    const run_download_deps = b.addRunArtifact(download_deps_exe);
    run_download_deps.addArgs(&.{target_str});
    const deps_path = "zig-out/build-deps";
    run_download_deps.addDirectoryArg(b.path(deps_path));

    download_deps.dependOn(&run_download_deps.step);

    var out = b.allocator.alloc([]const u8, 1) catch @panic("OOM");
    out[0] = b.fmt("{s}/{s}/bin", .{ deps_path, target_str });
    return out;
}

fn addFuzzTarget(
    b: *std.Build,
    fuzz: *Step,
    target: ResolvedTarget,
    download_deps: *Step,
    llvm_config_paths: []const []const u8,
    name: []const u8,
    root_source_file: LazyPath,
    imports: []const Import,
) void {
    const name_obj = b.fmt("{s}_obj", .{name});
    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const step_msg = b.fmt("Generate fuzz executable for {s}", .{name});

    const fuzz_obj = b.addObject(.{
        .name = name_obj,
        .root_source_file = root_source_file,
        .target = target,
        .optimize = .ReleaseSafe,
    });
    fuzz_obj.step.dependOn(download_deps);

    for (imports) |import| {
        fuzz_obj.root_module.addImport(import.name, import.module);
    }

    // TODO: Once 0.14.0 is released, uncomment this. Will make fuzzing work better.
    // fuzz_obj.root_module.fuzz = true;
    fuzz_obj.root_module.stack_check = false; // not linking with compiler-rt
    fuzz_obj.root_module.link_libc = true; // afl runtime depends on libc

    const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, llvm_config_paths, fuzz_obj);

    const repro = b.addExecutable(.{
        .name = name_repro,
        .root_source_file = b.path("src/fuzz/repro.zig"),
        .target = target,
        .optimize = .ReleaseSafe,
        .link_libc = true,
    });
    repro.addObject(fuzz_obj);

    const fuzz_step = b.step(name_exe, step_msg);
    fuzz_step.dependOn(&b.addInstallBinFile(fuzz_exe, name_exe).step);
    fuzz_step.dependOn(&b.addInstallBinFile(repro.getEmittedBin(), name_repro).step);

    fuzz.dependOn(fuzz_step);
}
