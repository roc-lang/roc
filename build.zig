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

    const use_system_llvm = b.option(bool, "system-llvm", "Attempt to automatically detect and use system installed llvm") orelse false;
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to llvm. This path must contain the bin, lib, and include directory.");

    const fuzz = b.option(bool, "fuzz", "Build fuzz targets including AFL++ and tooling") orelse false;
    const valid_fuzz_target = target.query.isNativeCpu() and target.query.isNativeOs() and (target.query.isNativeAbi() or target.result.abi.isMusl()) and target.result.os.tag != .windows;
    if (fuzz and valid_fuzz_target) {
        const afl = b.lazyImport(@This(), "zig-afl-kit") orelse return;

        // Eventually, llvm will be needed for all of roc.
        // For now, it is only needed afl fuzzing.
        const llvm_paths = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return;
        b.addSearchPrefix(llvm_paths.lib);

        // TODO: this just builds the fuzz target. Afterwards, they are still awkward to orchestrate and run.
        // Make a script to manage the corpus and run the fuzzers (or at least some good docs)
        // Likely will should check in a minimal corpus somewhere so we don't always start from zero.
        add_fuzz_target(
            b,
            afl,
            test_step,
            target,
            llvm_paths.bin,
            "cli",
            b.path("src/fuzz/cli.zig"),
            &[_]Import{
                .{ .name = "cli", .module = b.createModule(.{ .root_source_file = b.path("src/cli.zig") }) },
            },
        );
    }
}

const LlvmPaths = struct {
    bin: []const u8,
    include: []const u8,
    lib: []const u8,
};

fn llvmPaths(
    b: *std.Build,
    target: ResolvedTarget,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
) ?LlvmPaths {
    if (use_system_llvm and user_llvm_path != null) {
        std.log.err("-Dsystem-llvm and -Dllvm-path cannot both be set", .{});
        std.process.exit(1);
    }

    if (use_system_llvm) {
        const llvm_config_path = b.findProgram(&.{"llvm-config"}, &.{""}) catch {
            std.log.err("Failed to find system llvm-config binary", .{});
            std.process.exit(1);
        };
        const llvm_bin_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--bindir" }), "\n");
        const llvm_lib_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--libdir" }), "\n");
        const llvm_include_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--includedir" }), "\n");

        return .{
            .bin = llvm_bin_dir,
            .include = llvm_include_dir,
            .lib = llvm_lib_dir,
        };
    }

    if (user_llvm_path) |llvm_path| {
        // We are just trust the user.
        return .{
            .bin = b.pathJoin(&.{ llvm_path, "bin" }),
            .include = b.pathJoin(&.{ llvm_path, "include" }),
            .lib = b.pathJoin(&.{ llvm_path, "lib" }),
        };
    }

    // No user specified llvm. Go download it from roc-boostrap.
    const raw_triple = target.result.linuxTriple(b.allocator) catch @panic("OOM");
    if (!supported_deps_triples.has(raw_triple)) {
        std.log.err("Target triple({s}) not supported by roc-bootstrap.\n", .{raw_triple});
        std.log.err("Please specify the either `-Dsystem-llvm` or `-Dllvm-config-path`.\n", .{});
        std.process.exit(1);
    }
    const triple = supported_deps_triples.get(raw_triple).?;
    const deps_name = b.fmt("roc-deps-{s}", .{triple});
    const deps = b.lazyDependency(deps_name, .{}) orelse return null;
    const lazy_llvm_path = deps.path(".");
    // TODO: Is this ok to do in the zig build system?
    // We aren't in the make phase, but our static dep doesn't have a make phase anyway.
    // Not sure how else to get a static path to the downloaded dependency.
    const llvm_path = lazy_llvm_path.getPath(deps.builder);
    return .{
        .bin = b.pathJoin(&.{ llvm_path, "bin" }),
        .include = b.pathJoin(&.{ llvm_path, "include" }),
        .lib = b.pathJoin(&.{ llvm_path, "lib" }),
    };
}

const supported_deps_triples = std.StaticStringMap([]const u8).initComptime(.{
    .{ "aarch64-macos-none", "aarch64-macos-none" },
    .{ "aarch64-linux-musl", "aarch64-linux-musl" },
    .{ "aarch64-windows-gnu", "aarch64-windows-gnu" },
    .{ "arm-linux-musleabihf", "arm-linux-musleabihf" },
    .{ "x86-linux-musl", "x86-linux-musl" },
    .{ "x86_64-linux-musl", "x86_64-linux-musl" },
    .{ "x86_64-macos-none", "x86_64-macos-none" },
    .{ "x86_64-windows-gnu", "x86_64-windows-gnu" },
    // We also support the gnu linux targets.
    // For those, we just map to musl.
    .{ "aarch64-linux-gnu", "aarch64-linux-musl" },
    .{ "arm-linux-gnueabihf", "arm-linux-musleabihf" },
    .{ "x86-linux-gnu", "x86-linux-musl" },
    .{ "x86_64-linux-gnu", "x86_64-linux-musl" },
});

fn add_fuzz_target(
    b: *std.Build,
    afl: type,
    parent_step: *Step,
    target: ResolvedTarget,
    llvm_bin_path: []const u8,
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

    const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, &.{llvm_bin_path}, fuzz_obj);

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
