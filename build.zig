const std = @import("std");
const builtin = @import("builtin");
const modules = @import("src/build/modules.zig");
const Dependency = std.Build.Dependency;
const Import = std.Build.Module.Import;
const InstallDir = std.Build.InstallDir;
const LazyPath = std.Build.LazyPath;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;
const Step = std.Build.Step;

pub fn build(b: *std.Build) void {
    // build steps
    const run_step = b.step("run", "Build and run the roc cli");
    const roc_step = b.step("roc", "Build the roc compiler without running it");
    const test_step = b.step("test", "Run all tests included in src/tests.zig");
    const fmt_step = b.step("fmt", "Format all zig code");
    const check_fmt_step = b.step("check-fmt", "Check formatting of all zig code");
    const snapshot_step = b.step("snapshot", "Run the snapshot tool to update snapshot files");
    const playground_step = b.step("playground", "Build the WASM playground");
    const playground_test_step = b.step("playground-test", "Build the integration test suite for the WASM playground");

    // general configuration
    const target = b.standardTargetOptions(.{ .default_target = .{
        .abi = if (builtin.target.os.tag == .linux) .musl else null,
    } });
    const optimize = b.standardOptimizeOption(.{});
    const strip = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const trace_eval = b.option(bool, "trace-eval", "Enable detailed evaluation tracing for debugging") orelse (optimize == .Debug);

    // llvm configuration
    const use_system_llvm = b.option(bool, "system-llvm", "Attempt to automatically detect and use system installed llvm") orelse false;
    const enable_llvm = !use_system_llvm; // removed build flag `-Dllvm`, we include LLVM libraries by default now
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to llvm. This path must contain the bin, lib, and include directory.");
    // Since zig afl is broken currently, default to system afl.
    const use_system_afl = b.option(bool, "system-afl", "Attempt to automatically detect and use system installed afl++") orelse true;

    if (user_llvm_path) |path| {
        // Even if the llvm backend is not enabled, still add the llvm path.
        // AFL++ may use it for building fuzzing executables.
        b.addSearchPrefix(b.pathJoin(&.{ path, "bin" }));
    }

    // tracy profiler configuration
    const flag_enable_tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");
    const flag_tracy_callstack = b.option(bool, "tracy-callstack", "Include callstack information with Tracy data. Does nothing if -Dtracy is not provided") orelse (flag_enable_tracy != null);
    const flag_tracy_allocation = b.option(bool, "tracy-allocation", "Include allocation information with Tracy data. Does nothing if -Dtracy is not provided") orelse (flag_enable_tracy != null);
    const flag_tracy_callstack_depth: u32 = b.option(u32, "tracy-callstack-depth", "Declare callstack depth for Tracy data. Does nothing if -Dtracy_callstack is not provided") orelse 10;
    if (flag_tracy_callstack) {
        std.log.warn("Tracy callstack is enable. This can significantly skew timings, but is important for understanding source location. Be cautious when generating timing and analyzing results.", .{});
    }

    // Create compile time build options
    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_tracy", flag_enable_tracy != null);
    build_options.addOption(bool, "trace_eval", trace_eval);
    build_options.addOption([]const u8, "compiler_version", getCompilerVersion(b, optimize));
    if (target.result.os.tag == .macos and flag_tracy_callstack) {
        std.log.warn("Tracy callstack does not work on MacOS, disabling.", .{});
        build_options.addOption(bool, "enable_tracy_callstack", false);
    } else {
        build_options.addOption(bool, "enable_tracy_callstack", flag_tracy_callstack);
    }
    build_options.addOption(bool, "enable_tracy_allocation", flag_tracy_allocation);
    build_options.addOption(u32, "tracy_callstack_depth", flag_tracy_callstack_depth);
    build_options.addOption(bool, "target_is_native", target.query.isNative());

    // We use zstd for `roc bundle` and `roc unbundle` and downloading .tar.zst bundles.
    const zstd = b.dependency("zstd", .{
        .target = target,
        .optimize = optimize,
    });

    const roc_modules = modules.RocModules.create(b, build_options, zstd);

    // add main roc exe
    const roc_exe = addMainExe(b, roc_modules, target, optimize, strip, enable_llvm, use_system_llvm, user_llvm_path, flag_enable_tracy, zstd) orelse return;
    roc_modules.addAll(roc_exe);
    install_and_run(b, no_bin, roc_exe, roc_step, run_step);

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_source_file = b.path("src/snapshot_tool/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    roc_modules.addAll(snapshot_exe);
    add_tracy(b, roc_modules.build_options, snapshot_exe, target, false, flag_enable_tracy);
    install_and_run(b, no_bin, snapshot_exe, snapshot_step, snapshot_step);

    const playground_exe = b.addExecutable(.{
        .name = "playground",
        .root_source_file = b.path("src/playground_wasm/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = optimize,
    });
    playground_exe.entry = .disabled;
    playground_exe.rdynamic = true;
    roc_modules.addAll(playground_exe);

    add_tracy(b, roc_modules.build_options, playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }), false, null);

    const playground_install = b.addInstallArtifact(playground_exe, .{});
    playground_step.dependOn(&playground_install.step);

    const bytebox = b.dependency("bytebox", .{
        .target = target,
        .optimize = optimize,
    });

    // Build playground integration tests - now enabled for all optimization modes
    const playground_test_install = blk: {
        const playground_integration_test_exe = b.addExecutable(.{
            .name = "playground_integration_test",
            .root_source_file = b.path("test/playground-integration/main.zig"),
            .target = target,
            .optimize = optimize,
        });
        playground_integration_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        playground_integration_test_exe.root_module.addImport("build_options", build_options.createModule());
        roc_modules.addAll(playground_integration_test_exe);

        const install = b.addInstallArtifact(playground_integration_test_exe, .{});
        // Ensure playground WASM is built before running the integration test
        install.step.dependOn(&playground_install.step);
        playground_test_step.dependOn(&install.step);

        const run_playground_test = b.addRunArtifact(playground_integration_test_exe);
        if (b.args) |args| {
            run_playground_test.addArgs(args);
        }
        run_playground_test.step.dependOn(&install.step);
        playground_test_step.dependOn(&run_playground_test.step);

        break :blk install;
    };

    // Create and add module tests
    const module_tests = roc_modules.createModuleTests(b, target, optimize, zstd);
    for (module_tests) |module_test| {
        b.default_step.dependOn(&module_test.test_step.step);
        test_step.dependOn(&module_test.run_step.step);
    }

    // Add snapshot tool test
    const snapshot_test = b.addTest(.{
        .name = "snapshot_tool_test",
        .root_source_file = b.path("src/snapshot_tool/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    roc_modules.addAll(snapshot_test);
    add_tracy(b, roc_modules.build_options, snapshot_test, target, false, flag_enable_tracy);

    const run_snapshot_test = b.addRunArtifact(snapshot_test);
    test_step.dependOn(&run_snapshot_test.step);

    // Add CLI test
    const cli_test = b.addTest(.{
        .name = "cli_test",
        .root_source_file = b.path("src/cli/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    roc_modules.addAll(cli_test);
    cli_test.linkLibrary(zstd.artifact("zstd"));
    add_tracy(b, roc_modules.build_options, cli_test, target, false, flag_enable_tracy);

    const run_cli_test = b.addRunArtifact(cli_test);
    test_step.dependOn(&run_cli_test.step);

    // Add watch tests
    const watch_test = b.addTest(.{
        .name = "watch_test",
        .root_source_file = b.path("src/watch/watch.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    roc_modules.addAll(watch_test);
    add_tracy(b, roc_modules.build_options, watch_test, target, false, flag_enable_tracy);

    // Link platform-specific libraries for file watching
    if (target.result.os.tag == .macos) {
        watch_test.linkFramework("CoreFoundation");
        watch_test.linkFramework("CoreServices");
    } else if (target.result.os.tag == .windows) {
        watch_test.linkSystemLibrary("kernel32");
    }

    const run_watch_test = b.addRunArtifact(watch_test);
    test_step.dependOn(&run_watch_test.step);

    b.default_step.dependOn(playground_step);
    {
        const install = playground_test_install;
        b.default_step.dependOn(&install.step);
    }

    // Fmt zig code.
    const fmt_paths = .{ "src", "build.zig" };
    const fmt = b.addFmt(.{ .paths = &fmt_paths });
    fmt_step.dependOn(&fmt.step);

    const check_fmt = b.addFmt(.{ .paths = &fmt_paths, .check = true });
    check_fmt_step.dependOn(&check_fmt.step);

    const fuzz = b.option(bool, "fuzz", "Build fuzz targets including AFL++ and tooling") orelse false;
    const is_native = target.query.isNativeCpu() and target.query.isNativeOs() and (target.query.isNativeAbi() or target.result.abi.isMusl());
    const is_windows = target.result.os.tag == .windows;

    var build_afl = false;
    if (!is_native) {
        std.log.warn("Cross compilation does not support fuzzing (Only building repro executables)", .{});
    } else if (is_windows) {
        std.log.warn("Windows does not support fuzzing (Only building repro executables)", .{});
    } else if (use_system_afl) {
        // If we have system afl, no need for llvm-config.
        build_afl = true;
    } else {
        // AFL++ does not work with our prebuilt static llvm.
        // Check for llvm-config program in user_llvm_path or on the system.
        // If found, let AFL++ use that.
        if (b.findProgram(&.{"llvm-config"}, &.{})) |_| {
            build_afl = true;
        } else |_| {
            std.log.warn("AFL++ requires a full version of llvm from the system or passed in via -Dllvm-path, but `llvm-config` was not found (Only building repro executables)", .{});
        }
    }

    const names: []const []const u8 = &.{
        "tokenize",
        "parse",
        "canonicalize",
    };
    for (names) |name| {
        add_fuzz_target(
            b,
            fuzz,
            build_afl,
            use_system_afl,
            no_bin,
            target,
            optimize,
            roc_modules,
            flag_enable_tracy,
            name,
        );
    }
}

const ModuleTest = modules.ModuleTest;

fn add_fuzz_target(
    b: *std.Build,
    fuzz: bool,
    build_afl: bool,
    use_system_afl: bool,
    no_bin: bool,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    tracy: ?[]const u8,
    name: []const u8,
) void {
    // We always include the repro scripts (no dependencies).
    // We only include the fuzzing scripts if `-Dfuzz` is set.
    const root_source_file = b.path(b.fmt("test/fuzzing/fuzz-{s}.zig", .{name}));
    const fuzz_obj = b.addObject(.{
        .name = b.fmt("{s}_obj", .{name}),
        .root_source_file = root_source_file,
        .target = target,
        // Work around instrumentation bugs on mac without giving up perf on linux.
        .optimize = if (target.result.os.tag == .macos) .Debug else .ReleaseSafe,
    });
    roc_modules.addAll(fuzz_obj);
    add_tracy(b, roc_modules.build_options, fuzz_obj, target, false, tracy);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const repro_step = b.step(name_repro, b.fmt("run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_source_file = b.path("test/fuzzing/fuzz-repro.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);

    install_and_run(b, no_bin, repro_exe, repro_step, repro_step);

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(name_exe, b.fmt("Generate fuzz executable for {s}", .{name}));
        b.default_step.dependOn(fuzz_step);

        const afl = b.lazyImport(@This(), "afl_kit") orelse return;
        const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, &.{}, use_system_afl, fuzz_obj) orelse return;
        const install_fuzz = b.addInstallBinFile(fuzz_exe, name_exe);
        fuzz_step.dependOn(&install_fuzz.step);
        b.getInstallStep().dependOn(&install_fuzz.step);
    }
}

fn addMainExe(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    strip: ?bool,
    enable_llvm: bool,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
    tracy: ?[]const u8,
    zstd: *Dependency,
) ?*Step.Compile {
    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = b.path("src/cli/main.zig"),
        .target = target,
        .optimize = optimize,
        .strip = strip,
        .link_libc = true,
    });

    // Create test platform host static library (str)
    const test_platform_host_lib = b.addLibrary(.{
        .name = "test_platform_str_host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/str/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    test_platform_host_lib.root_module.addImport("builtins", roc_modules.builtins);

    // Force bundle compiler-rt to resolve runtime symbols like __main
    test_platform_host_lib.bundle_compiler_rt = true;

    // Copy the test platform host library to the source directory
    const copy_test_host = b.addUpdateSourceFiles();
    const test_host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
    copy_test_host.addCopyFileToSource(test_platform_host_lib.getEmittedBin(), b.pathJoin(&.{ "test/str/platform", test_host_filename }));
    b.getInstallStep().dependOn(&copy_test_host.step);

    // Create test platform host static library (int)
    const test_platform_int_host_lib = b.addLibrary(.{
        .name = "test_platform_int_host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/int/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    test_platform_int_host_lib.root_module.addImport("builtins", roc_modules.builtins);
    // Force bundle compiler-rt to resolve runtime symbols like __main
    test_platform_int_host_lib.bundle_compiler_rt = true;

    // Copy the int test platform host library to the source directory
    const copy_test_int_host = b.addUpdateSourceFiles();
    const test_int_host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
    copy_test_int_host.addCopyFileToSource(test_platform_int_host_lib.getEmittedBin(), b.pathJoin(&.{ "test/int/platform", test_int_host_filename }));
    b.getInstallStep().dependOn(&copy_test_int_host.step);

    // Create builtins static library at build time with minimal dependencies
    const builtins_obj = b.addObject(.{
        .name = "roc_builtins",
        .root_source_file = b.path("src/builtins/static_lib.zig"),
        .target = target,
        .optimize = optimize,
        .strip = true,
        .pic = true, // Enable Position Independent Code for PIE compatibility
    });

    // Create shim static library at build time - fully static without libc
    //
    // NOTE we do NOT link libC here to avoid dynamic dependency on libC
    const shim_lib = b.addLibrary(.{
        .name = "roc_interpreter_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    // Add all modules from roc_modules that the shim needs
    roc_modules.addAll(shim_lib);
    // Link against the pre-built builtins library
    shim_lib.addObject(builtins_obj);
    // Bundle compiler-rt for our math symbols
    shim_lib.bundle_compiler_rt = true;
    // Install shim library to the output directory
    const install_shim = b.addInstallArtifact(shim_lib, .{});
    b.getInstallStep().dependOn(&install_shim.step);
    // Copy the shim library to the src/ directory for embedding as binary data
    // This is because @embedFile happens at compile time and needs the file to exist already
    // and zig doesn't permit embedding files from directories outside the source tree.
    const copy_shim = b.addUpdateSourceFiles();
    const interpreter_shim_filename = if (target.result.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
    copy_shim.addCopyFileToSource(shim_lib.getEmittedBin(), b.pathJoin(&.{ "src/cli", interpreter_shim_filename }));
    exe.step.dependOn(&copy_shim.step);

    const config = b.addOptions();
    config.addOption(bool, "llvm", enable_llvm);
    exe.root_module.addOptions("config", config);
    exe.root_module.addAnonymousImport("legal_details", .{ .root_source_file = b.path("legal_details") });

    if (enable_llvm) {
        const llvm_paths = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return null;

        exe.addLibraryPath(.{ .cwd_relative = llvm_paths.lib });
        exe.addIncludePath(.{ .cwd_relative = llvm_paths.include });
        try addStaticLlvmOptionsToModule(exe.root_module);
    }

    add_tracy(b, roc_modules.build_options, exe, target, enable_llvm, tracy);

    exe.linkLibrary(zstd.artifact("zstd"));

    return exe;
}

fn install_and_run(
    b: *std.Build,
    no_bin: bool,
    exe: *Step.Compile,
    build_step: *Step,
    run_step: *Step,
) void {
    if (run_step != build_step) {
        run_step.dependOn(build_step);
    }
    if (no_bin) {
        // No build, just build, don't actually install or run.
        build_step.dependOn(&exe.step);
        b.getInstallStep().dependOn(&exe.step);
    } else {
        const install = b.addInstallArtifact(exe, .{});
        build_step.dependOn(&install.step);
        b.getInstallStep().dependOn(&install.step);

        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        if (b.args) |args| {
            run.addArgs(args);
        }
        run_step.dependOn(&run.step);
    }
}

fn add_tracy(
    b: *std.Build,
    module_build_options: *std.Build.Module,
    base: *Step.Compile,
    target: ResolvedTarget,
    links_llvm: bool,
    tracy: ?[]const u8,
) void {
    base.root_module.addImport("build_options", module_build_options);
    if (tracy) |tracy_path| {
        const client_cpp = b.pathJoin(
            &[_][]const u8{ tracy_path, "public", "TracyClient.cpp" },
        );

        // On mingw, we need to opt into windows 7+ to get some features required by tracy.
        const tracy_c_flags: []const []const u8 = if (target.result.os.tag == .windows and target.result.abi == .gnu)
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
        else
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

        base.root_module.addIncludePath(.{ .cwd_relative = tracy_path });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = client_cpp }, .flags = tracy_c_flags });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = "src/build/tracy-shutdown.cpp" }, .flags = tracy_c_flags });
        if (!links_llvm) {
            base.root_module.linkSystemLibrary("c++", .{ .use_pkg_config = .no });
        }
        base.root_module.link_libc = true;

        if (target.result.os.tag == .windows) {
            base.root_module.linkSystemLibrary("dbghelp", .{});
            base.root_module.linkSystemLibrary("ws2_32", .{});
        }
    }
}

const LlvmPaths = struct {
    include: []const u8,
    lib: []const u8,
};

// This functions is not used right now due to AFL requiring system llvm.
// This will be used once we begin linking roc to llvm.
fn llvmPaths(
    b: *std.Build,
    target: ResolvedTarget,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
) ?LlvmPaths {
    if (use_system_llvm and user_llvm_path != null) {
        std.log.err("-Dsystem-llvm and -Dllvm-path cannot both be specified", .{});
        std.process.exit(1);
    }

    if (use_system_llvm) {
        const llvm_config_path = b.findProgram(&.{"llvm-config"}, &.{""}) catch {
            std.log.err("Failed to find system llvm-config binary", .{});
            std.process.exit(1);
        };
        const llvm_lib_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--libdir" }), "\n");
        const llvm_include_dir = std.mem.trimRight(u8, b.run(&.{ llvm_config_path, "--includedir" }), "\n");

        return .{
            .include = llvm_include_dir,
            .lib = llvm_lib_dir,
        };
    }

    if (user_llvm_path) |llvm_path| {
        // We are just trust the user.
        return .{
            .include = b.pathJoin(&.{ llvm_path, "include" }),
            .lib = b.pathJoin(&.{ llvm_path, "lib" }),
        };
    }

    // No user specified llvm. Go download it from roc-bootstrap.
    const raw_triple = target.result.linuxTriple(b.allocator) catch @panic("OOM");
    if (!supported_deps_triples.has(raw_triple)) {
        std.log.err("Target triple({s}) not supported by roc-bootstrap.\n", .{raw_triple});
        std.log.err("Please specify the either `-Dsystem-llvm` or `-Dllvm-path`.\n", .{});
        std.process.exit(1);
    }
    const triple = supported_deps_triples.get(raw_triple).?;
    const deps_name = b.fmt("roc_deps_{s}", .{triple});
    const deps = b.lazyDependency(deps_name, .{}) orelse return null;
    const lazy_llvm_path = deps.path(".");
    // TODO: Is this ok to do in the zig build system?
    // We aren't in the make phase, but our static dep doesn't have a make phase anyway.
    // Not sure how else to get a static path to the downloaded dependency.
    const llvm_path = lazy_llvm_path.getPath(deps.builder);
    return .{
        .include = b.pathJoin(&.{ llvm_path, "include" }),
        .lib = b.pathJoin(&.{ llvm_path, "lib" }),
    };
}

const supported_deps_triples = std.StaticStringMap([]const u8).initComptime(.{
    .{ "aarch64-macos-none", "aarch64_macos_none" },
    .{ "aarch64-linux-musl", "aarch64_linux_musl" },
    .{ "aarch64-windows-gnu", "aarch64_windows_gnu" },
    .{ "arm-linux-musleabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-musl", "x86_linux_musl" },
    .{ "x86_64-linux-musl", "x86_64_linux_musl" },
    .{ "x86_64-macos-none", "x86_64_macos_none" },
    .{ "x86_64-windows-gnu", "x86_64_windows_gnu" },
    // We also support the gnu linux targets.
    // For those, we just map to musl.
    .{ "aarch64-linux-gnu", "aarch64_linux_musl" },
    .{ "arm-linux-gnueabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-gnu", "x86_linux_musl" },
    .{ "x86_64-linux-gnu", "x86_64_linux_musl" },
});

// The following is lifted from the zig compiler.
fn addStaticLlvmOptionsToModule(mod: *std.Build.Module) !void {
    const cpp_cflags = exe_cflags ++ [_][]const u8{"-DNDEBUG=1"};
    mod.addCSourceFiles(.{
        .files = &cpp_sources,
        .flags = &cpp_cflags,
    });

    const link_static = std.Build.Module.LinkSystemLibraryOptions{
        .preferred_link_mode = .static,
        .search_strategy = .mode_first,
    };
    for (lld_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    for (llvm_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    mod.linkSystemLibrary("z", link_static);

    if (mod.resolved_target.?.result.os.tag != .windows or mod.resolved_target.?.result.abi != .msvc) {
        // TODO: Can this just be `mod.link_libcpp = true`? Does that make a difference?
        // This means we rely on clang-or-zig-built LLVM, Clang, LLD libraries.
        mod.linkSystemLibrary("c++", .{});
    }

    if (mod.resolved_target.?.result.os.tag == .windows) {
        mod.linkSystemLibrary("ws2_32", .{});
        mod.linkSystemLibrary("version", .{});
        mod.linkSystemLibrary("uuid", .{});
        mod.linkSystemLibrary("ole32", .{});
    }
}

const cpp_sources = [_][]const u8{
    "src/build/zig_llvm.cpp",
};

const exe_cflags = [_][]const u8{
    "-std=c++17",
    "-D__STDC_CONSTANT_MACROS",
    "-D__STDC_FORMAT_MACROS",
    "-D__STDC_LIMIT_MACROS",
    "-D_GNU_SOURCE",
    "-fno-exceptions",
    "-fno-rtti",
    "-fno-stack-protector",
    "-fvisibility-inlines-hidden",
    "-Wno-type-limits",
    "-Wno-missing-braces",
    "-Wno-comment",
};
const lld_libs = [_][]const u8{
    "lldMinGW",
    "lldELF",
    "lldCOFF",
    "lldWasm",
    "lldMachO",
    "lldCommon",
};
// This list can be re-generated with `llvm-config --libfiles` and then
// reformatting using your favorite text editor. Note we do not execute
// `llvm-config` here because we are cross compiling. Also omit LLVMTableGen
// from these libs.
const llvm_libs = [_][]const u8{
    "LLVMWindowsManifest",
    "LLVMXRay",
    "LLVMLibDriver",
    "LLVMDlltoolDriver",
    "LLVMTextAPIBinaryReader",
    "LLVMCoverage",
    "LLVMLineEditor",
    "LLVMXCoreDisassembler",
    "LLVMXCoreCodeGen",
    "LLVMXCoreDesc",
    "LLVMXCoreInfo",
    "LLVMX86TargetMCA",
    "LLVMX86Disassembler",
    "LLVMX86AsmParser",
    "LLVMX86CodeGen",
    "LLVMX86Desc",
    "LLVMX86Info",
    "LLVMWebAssemblyDisassembler",
    "LLVMWebAssemblyAsmParser",
    "LLVMWebAssemblyCodeGen",
    "LLVMWebAssemblyUtils",
    "LLVMWebAssemblyDesc",
    "LLVMWebAssemblyInfo",
    "LLVMVEDisassembler",
    "LLVMVEAsmParser",
    "LLVMVECodeGen",
    "LLVMVEDesc",
    "LLVMVEInfo",
    "LLVMSystemZDisassembler",
    "LLVMSystemZAsmParser",
    "LLVMSystemZCodeGen",
    "LLVMSystemZDesc",
    "LLVMSystemZInfo",
    "LLVMSparcDisassembler",
    "LLVMSparcAsmParser",
    "LLVMSparcCodeGen",
    "LLVMSparcDesc",
    "LLVMSparcInfo",
    "LLVMRISCVTargetMCA",
    "LLVMRISCVDisassembler",
    "LLVMRISCVAsmParser",
    "LLVMRISCVCodeGen",
    "LLVMRISCVDesc",
    "LLVMRISCVInfo",
    "LLVMPowerPCDisassembler",
    "LLVMPowerPCAsmParser",
    "LLVMPowerPCCodeGen",
    "LLVMPowerPCDesc",
    "LLVMPowerPCInfo",
    "LLVMNVPTXCodeGen",
    "LLVMNVPTXDesc",
    "LLVMNVPTXInfo",
    "LLVMMSP430Disassembler",
    "LLVMMSP430AsmParser",
    "LLVMMSP430CodeGen",
    "LLVMMSP430Desc",
    "LLVMMSP430Info",
    "LLVMMipsDisassembler",
    "LLVMMipsAsmParser",
    "LLVMMipsCodeGen",
    "LLVMMipsDesc",
    "LLVMMipsInfo",
    "LLVMLoongArchDisassembler",
    "LLVMLoongArchAsmParser",
    "LLVMLoongArchCodeGen",
    "LLVMLoongArchDesc",
    "LLVMLoongArchInfo",
    "LLVMLanaiDisassembler",
    "LLVMLanaiCodeGen",
    "LLVMLanaiAsmParser",
    "LLVMLanaiDesc",
    "LLVMLanaiInfo",
    "LLVMHexagonDisassembler",
    "LLVMHexagonCodeGen",
    "LLVMHexagonAsmParser",
    "LLVMHexagonDesc",
    "LLVMHexagonInfo",
    "LLVMBPFDisassembler",
    "LLVMBPFAsmParser",
    "LLVMBPFCodeGen",
    "LLVMBPFDesc",
    "LLVMBPFInfo",
    "LLVMAVRDisassembler",
    "LLVMAVRAsmParser",
    "LLVMAVRCodeGen",
    "LLVMAVRDesc",
    "LLVMAVRInfo",
    "LLVMARMDisassembler",
    "LLVMARMAsmParser",
    "LLVMARMCodeGen",
    "LLVMARMDesc",
    "LLVMARMUtils",
    "LLVMARMInfo",
    "LLVMAMDGPUTargetMCA",
    "LLVMAMDGPUDisassembler",
    "LLVMAMDGPUAsmParser",
    "LLVMAMDGPUCodeGen",
    "LLVMAMDGPUDesc",
    "LLVMAMDGPUUtils",
    "LLVMAMDGPUInfo",
    "LLVMAArch64Disassembler",
    "LLVMAArch64AsmParser",
    "LLVMAArch64CodeGen",
    "LLVMAArch64Desc",
    "LLVMAArch64Utils",
    "LLVMAArch64Info",
    "LLVMOrcDebugging",
    "LLVMOrcJIT",
    "LLVMWindowsDriver",
    "LLVMMCJIT",
    "LLVMJITLink",
    "LLVMInterpreter",
    "LLVMExecutionEngine",
    "LLVMRuntimeDyld",
    "LLVMOrcTargetProcess",
    "LLVMOrcShared",
    "LLVMDWP",
    "LLVMDebugInfoLogicalView",
    "LLVMDebugInfoGSYM",
    "LLVMOption",
    "LLVMObjectYAML",
    "LLVMObjCopy",
    "LLVMMCA",
    "LLVMMCDisassembler",
    "LLVMLTO",
    "LLVMPasses",
    "LLVMHipStdPar",
    "LLVMCFGuard",
    "LLVMCoroutines",
    "LLVMipo",
    "LLVMVectorize",
    "LLVMLinker",
    "LLVMInstrumentation",
    "LLVMFrontendOpenMP",
    "LLVMFrontendOffloading",
    "LLVMFrontendOpenACC",
    "LLVMFrontendHLSL",
    "LLVMFrontendDriver",
    "LLVMExtensions",
    "LLVMDWARFLinkerParallel",
    "LLVMDWARFLinkerClassic",
    "LLVMDWARFLinker",
    "LLVMGlobalISel",
    "LLVMMIRParser",
    "LLVMAsmPrinter",
    "LLVMSelectionDAG",
    "LLVMCodeGen",
    "LLVMTarget",
    "LLVMObjCARCOpts",
    "LLVMCodeGenTypes",
    "LLVMIRPrinter",
    "LLVMInterfaceStub",
    "LLVMFileCheck",
    "LLVMFuzzMutate",
    "LLVMScalarOpts",
    "LLVMInstCombine",
    "LLVMAggressiveInstCombine",
    "LLVMTransformUtils",
    "LLVMBitWriter",
    "LLVMAnalysis",
    "LLVMProfileData",
    "LLVMSymbolize",
    "LLVMDebugInfoBTF",
    "LLVMDebugInfoPDB",
    "LLVMDebugInfoMSF",
    "LLVMDebugInfoDWARF",
    "LLVMObject",
    "LLVMTextAPI",
    "LLVMMCParser",
    "LLVMIRReader",
    "LLVMAsmParser",
    "LLVMMC",
    "LLVMDebugInfoCodeView",
    "LLVMBitReader",
    "LLVMFuzzerCLI",
    "LLVMCore",
    "LLVMRemarks",
    "LLVMBitstreamReader",
    "LLVMBinaryFormat",
    "LLVMTargetParser",
    "LLVMSupport",
    "LLVMDemangle",
};

/// Get the compiler version string for cache versioning.
/// Returns a string like "debug-abc12345" where abc12345 is the git commit SHA.
/// If git is not available, falls back to "debug-no-git" format.
fn getCompilerVersion(b: *std.Build, optimize: OptimizeMode) []const u8 {
    const build_mode = switch (optimize) {
        .Debug => "debug",
        .ReleaseSafe => "release-safe",
        .ReleaseFast => "release-fast",
        .ReleaseSmall => "release-small",
    };

    // Try to get git commit SHA using std.process.Child.run
    const result = std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = &[_][]const u8{ "git", "rev-parse", "--short=8", "HEAD" },
    }) catch {
        // Git command failed, use fallback
        return std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
    };
    defer b.allocator.free(result.stdout);
    defer b.allocator.free(result.stderr);

    if (result.term == .Exited and result.term.Exited == 0) {
        // Git succeeded, use the commit SHA
        const commit_sha = std.mem.trim(u8, result.stdout, " \n\r\t");
        if (commit_sha.len > 0) {
            return std.fmt.allocPrint(b.allocator, "{s}-{s}", .{ build_mode, commit_sha }) catch
                std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
        }
    }

    // Git not available or failed, use fallback
    return std.fmt.allocPrint(b.allocator, "{s}-no-git", .{build_mode}) catch build_mode;
}
