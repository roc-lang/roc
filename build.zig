const std = @import("std");
const builtin = @import("builtin");
const modules = @import("src/build/modules.zig");
const glibc_stub_build = @import("src/build/glibc_stub.zig");
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
    const playground_test_step = b.step("test-playground", "Build the integration test suite for the WASM playground");

    // general configuration
    const target = b.standardTargetOptions(.{ .default_target = .{
        .abi = if (builtin.target.os.tag == .linux) .musl else null,
    } });
    const optimize = b.standardOptimizeOption(.{});
    const strip = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const trace_eval = b.option(bool, "trace-eval", "Enable detailed evaluation tracing for debugging") orelse (optimize == .Debug);

    const parsed_args = parseBuildArgs(b);
    const run_args = parsed_args.run_args;
    const test_filters = parsed_args.test_filters;

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

    // When cross-compiling, we also need a host zstd for the bootstrap compiler
    const is_cross_compile = target.result.cpu.arch != builtin.cpu.arch or target.result.os.tag != builtin.os.tag;
    const host_zstd = if (is_cross_compile)
        b.dependency("zstd", .{
            .target = b.graph.host,
            .optimize = optimize,
        })
    else
        zstd;

    const roc_modules = modules.RocModules.create(b, build_options, zstd);

    // add main roc exe
    const roc_exe = addMainExe(b, roc_modules, target, optimize, strip, enable_llvm, use_system_llvm, user_llvm_path, flag_enable_tracy, flag_tracy_callstack, zstd, host_zstd) orelse return;
    roc_modules.addAll(roc_exe);
    install_and_run(b, no_bin, roc_exe, roc_step, run_step, run_args);

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
    install_and_run(b, no_bin, snapshot_exe, snapshot_step, snapshot_step, run_args);

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
        if (run_args.len != 0) {
            run_playground_test.addArgs(run_args);
        }
        run_playground_test.step.dependOn(&install.step);
        playground_test_step.dependOn(&run_playground_test.step);

        break :blk install;
    };

    // Create and add module tests
    const module_tests = roc_modules.createModuleTests(b, target, optimize, zstd, test_filters);
    for (module_tests) |module_test| {
        if (run_args.len != 0) {
            module_test.run_step.addArgs(run_args);
        }

        // Create individual test step for this module
        const test_exe_name = module_test.test_step.name;
        const step_name = b.fmt("test-{s}", .{test_exe_name});
        const individual_test_step = b.step(step_name, b.fmt("Run {s} tests only", .{test_exe_name}));

        // Create run step that accepts command line args (including --test-filter)
        const individual_run = b.addRunArtifact(module_test.test_step);
        if (run_args.len != 0) {
            individual_run.addArgs(run_args);
        }
        individual_test_step.dependOn(&individual_run.step);

        b.default_step.dependOn(&module_test.test_step.step);
        test_step.dependOn(&module_test.run_step.step);
    }

    // Add snapshot tool test
    const enable_snapshot_tests = b.option(bool, "snapshot-tests", "Enable snapshot tests") orelse true;
    if (enable_snapshot_tests) {
        const snapshot_test = b.addTest(.{
            .name = "snapshot_tool_test",
            .root_source_file = b.path("src/snapshot_tool/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .filters = test_filters,
        });
        roc_modules.addAll(snapshot_test);
        add_tracy(b, roc_modules.build_options, snapshot_test, target, false, flag_enable_tracy);

        const run_snapshot_test = b.addRunArtifact(snapshot_test);
        if (run_args.len != 0) {
            run_snapshot_test.addArgs(run_args);
        }
        test_step.dependOn(&run_snapshot_test.step);
    }

    // Add CLI test
    const enable_cli_tests = b.option(bool, "cli-tests", "Enable cli tests") orelse true;
    if (enable_cli_tests) {
        const cli_test = b.addTest(.{
            .name = "cli_test",
            .root_source_file = b.path("src/cli/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .filters = test_filters,
        });
        roc_modules.addAll(cli_test);
        cli_test.linkLibrary(zstd.artifact("zstd"));
        add_tracy(b, roc_modules.build_options, cli_test, target, false, flag_enable_tracy);

        const run_cli_test = b.addRunArtifact(cli_test);
        if (run_args.len != 0) {
            run_cli_test.addArgs(run_args);
        }
        test_step.dependOn(&run_cli_test.step);
    }

    // Add watch tests
    const enable_watch_tests = b.option(bool, "watch-tests", "Enable watch tests") orelse true;
    if (enable_watch_tests) {
        const watch_test = b.addTest(.{
            .name = "watch_test",
            .root_source_file = b.path("src/watch/watch.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .filters = test_filters,
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
        if (run_args.len != 0) {
            run_watch_test.addArgs(run_args);
        }
        test_step.dependOn(&run_watch_test.step);
    }

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
            run_args,
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
    run_args: []const []const u8,
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

    install_and_run(b, no_bin, repro_exe, repro_step, repro_step, run_args);

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
    tracy_callstack: bool,
    zstd: *Dependency,
    host_zstd: *Dependency,
) ?*Step.Compile {
    // STAGE 0: Build shim library (or libraries if cross-compiling)
    // When cross-compiling, we need a host shim for bootstrap AND a target shim for final roc.
    // When not cross-compiling, we only need one shim.
    const is_cross_compile = target.result.cpu.arch != b.graph.host.result.cpu.arch or
        target.result.os.tag != b.graph.host.result.os.tag;

    // Build host shim only when cross-compiling
    const copy_host_shim = if (is_cross_compile) blk: {
        const host_builtins_obj = b.addObject(.{
            .name = "roc_builtins_host",
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .strip = true,
            .pic = true,
        });

        const host_shim_lib = b.addLibrary(.{
            .name = "roc_interpreter_shim_host",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/interpreter_shim/main.zig"),
                .target = b.graph.host,
                .optimize = optimize,
                .strip = true,
                .pic = true,
            }),
            .linkage = .static,
        });
        // Add only the modules the shim actually needs (not bundle/zstd)
        host_shim_lib.root_module.addImport("builtins", roc_modules.builtins);
        host_shim_lib.root_module.addImport("base", roc_modules.base);
        host_shim_lib.root_module.addImport("can", roc_modules.can);
        host_shim_lib.root_module.addImport("types", roc_modules.types);
        host_shim_lib.root_module.addImport("eval", roc_modules.eval);
        host_shim_lib.root_module.addImport("ipc", roc_modules.ipc);
        host_shim_lib.root_module.link_libc = true;
        host_shim_lib.addObject(host_builtins_obj);
        host_shim_lib.bundle_compiler_rt = true;

        const copy = b.addUpdateSourceFiles();
        const host_shim_filename = if (b.graph.host.result.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
        copy.addCopyFileToSource(host_shim_lib.getEmittedBin(), b.pathJoin(&.{ "src/cli", host_shim_filename }));
        break :blk copy;
    } else null;

    // Build TARGET shim library (always needed)
    const builtins_obj = b.addObject(.{
        .name = "roc_builtins",
        .root_source_file = b.path("src/builtins/static_lib.zig"),
        .target = target,
        .optimize = optimize,
        .strip = true,
        .pic = true,
    });

    const shim_lib = b.addLibrary(.{
        .name = "roc_interpreter_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
            .pic = true,
        }),
        .linkage = .static,
    });
    // Add only the modules the shim actually needs (not bundle/zstd)
    shim_lib.root_module.addImport("builtins", roc_modules.builtins);
    shim_lib.root_module.addImport("base", roc_modules.base);
    shim_lib.root_module.addImport("can", roc_modules.can);
    shim_lib.root_module.addImport("types", roc_modules.types);
    shim_lib.root_module.addImport("eval", roc_modules.eval);
    shim_lib.root_module.addImport("ipc", roc_modules.ipc);
    shim_lib.root_module.link_libc = true;
    shim_lib.addObject(builtins_obj);
    shim_lib.bundle_compiler_rt = true;

    // Install target shim to output directory
    const install_shim = b.addInstallArtifact(shim_lib, .{});
    b.getInstallStep().dependOn(&install_shim.step);

    // Copy target shim to src/ for final roc compiler to embed
    const copy_shim = b.addUpdateSourceFiles();
    const interpreter_shim_filename = if (target.result.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
    copy_shim.addCopyFileToSource(shim_lib.getEmittedBin(), b.pathJoin(&.{ "src/cli", interpreter_shim_filename }));

    // STAGE 1: Build bootstrap compiler with minimal builtins
    // Bootstrap must target the HOST platform (not target) so it can run during the build.
    // Create separate bootstrap_roc_modules using host_zstd
    const bootstrap_compiler = @import("src/build/bootstrap_compiler.zig");
    const bootstrap_build_options = b.addOptions();
    bootstrap_build_options.addOption(bool, "enable_tracy", tracy != null);
    bootstrap_build_options.addOption(bool, "enable_tracy_callstack", tracy != null and tracy_callstack);
    bootstrap_build_options.addOption(bool, "enable_tracy_allocation", false);
    bootstrap_build_options.addOption(u32, "tracy_callstack_depth", 0);
    bootstrap_build_options.addOption(bool, "target_is_native", true); // bootstrap always runs on host
    bootstrap_build_options.addOption([]const u8, "compiler_version", getCompilerVersion(b, optimize));
    var bootstrap_roc_modules = modules.RocModules.create(b, bootstrap_build_options, host_zstd);
    const bootstrap_exe = bootstrap_compiler.buildBootstrapCompiler(
        b,
        &bootstrap_roc_modules,
        b.graph.host,
        optimize,
        tracy,
    );
    bootstrap_roc_modules.addAll(bootstrap_exe);

    // Bootstrap compiler needs the shim library to be in place before it compiles
    // When cross-compiling, it needs the host shim. When not, it uses the target shim.
    if (copy_host_shim) |host_shim| {
        bootstrap_exe.step.dependOn(&host_shim.step);
    } else {
        bootstrap_exe.step.dependOn(&copy_shim.step);
    }

    // Note: We don't install the bootstrap compiler to avoid potential
    // dependency ordering issues on Windows. It's only used at build time.

    // STAGE 2: Compile builtins and generate embedded_envs.zig
    const compile_builtins = @import("src/build/compile_builtins.zig");
    const load_builtins = @import("src/builtins/load_builtins.zig");

    // Validate that the list in load_builtins.zig matches what's on disk
    compile_builtins.validateBuiltinList(
        b,
        &load_builtins.builtin_roc_files,
    ) catch |err| {
        std.debug.print("Builtin validation failed: {}\n", .{err});
        std.process.exit(1);
    };

    const compiled_builtins = compile_builtins.compileBuiltins(
        b,
        bootstrap_exe,
        &load_builtins.builtin_roc_files,
    );

    // Generate embedded_envs.zig in the build cache alongside the .env files
    const embedded_envs_path = compile_builtins.generateEmbeddedEnvs(
        b,
        compiled_builtins,
        &load_builtins.builtin_roc_files,
    ) catch |err| {
        std.debug.print("Failed to generate embedded_envs.zig: {}\n", .{err});
        std.process.exit(1);
    };
    _ = embedded_envs_path; // Will be used when integrating with Can.zig

    // STAGE 3: Build final roc compiler with embedded builtins
    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = b.path("src/cli/main.zig"),
        .target = target,
        .optimize = optimize,
        .strip = strip,
        .link_libc = true,
    });

    // Make final roc depend on compiled builtins and shim library being ready
    exe.step.dependOn(&compiled_builtins.step);
    exe.step.dependOn(&copy_shim.step);

    // Add build option for final compiler (not bootstrap)
    const final_options = b.addOptions();
    final_options.addOption(bool, "use_minimal_builtins", false);
    exe.root_module.addOptions("bootstrap_options", final_options);

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

    // Create test platform host static library (int) - native target
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

    // Cross-compile int platform host libraries for musl and glibc targets
    const cross_compile_targets = [_]struct { name: []const u8, query: std.Target.Query }{
        .{ .name = "x64musl", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "arm64musl", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "x64glibc", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu } },
        .{ .name = "arm64glibc", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu } },
    };

    for (cross_compile_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        // Create cross-compiled int host library
        const cross_int_host_lib = b.addLibrary(.{
            .name = b.fmt("test_platform_int_host_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/int/platform/host.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = true,
                .pic = true,
            }),
            .linkage = .static,
        });
        cross_int_host_lib.root_module.addImport("builtins", roc_modules.builtins);
        cross_int_host_lib.bundle_compiler_rt = true;

        // Copy to target-specific directory
        const copy_cross_int_host = b.addUpdateSourceFiles();
        copy_cross_int_host.addCopyFileToSource(cross_int_host_lib.getEmittedBin(), b.pathJoin(&.{ "test/int/platform/targets", cross_target.name, "libhost.a" }));
        b.getInstallStep().dependOn(&copy_cross_int_host.step);

        // Generate glibc stubs for gnu targets
        if (cross_target.query.abi == .gnu) {
            const glibc_stub = generateGlibcStub(b, cross_resolved_target, cross_target.name);
            if (glibc_stub) |stub| {
                b.getInstallStep().dependOn(&stub.step);
            }
        }
    }

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
    run_args: []const []const u8,
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
        if (run_args.len != 0) {
            run.addArgs(run_args);
        }
        run_step.dependOn(&run.step);
    }
}

const ParsedBuildArgs = struct {
    run_args: []const []const u8,
    test_filters: []const []const u8,
};

fn appendFilter(
    list: *std.ArrayList([]const u8),
    b: *std.Build,
    value: []const u8,
) void {
    const trimmed = std.mem.trim(u8, value, " \t\n\r");
    if (trimmed.len == 0) return;
    list.append(b.dupe(trimmed)) catch @panic("OOM while parsing --test-filter value");
}

fn parseBuildArgs(b: *std.Build) ParsedBuildArgs {
    const raw_args = b.args orelse return .{
        .run_args = &.{},
        .test_filters = &.{},
    };

    var run_args_list = std.ArrayList([]const u8).init(b.allocator);
    var filter_list = std.ArrayList([]const u8).init(b.allocator);

    var i: usize = 0;
    while (i < raw_args.len) {
        const arg = raw_args[i];

        if (std.mem.eql(u8, arg, "--test-filter")) {
            i += 1;
            if (i >= raw_args.len) {
                std.log.warn("ignoring --test-filter with no value", .{});
                break;
            }
            const value = raw_args[i];
            appendFilter(&filter_list, b, value);
            i += 1;
            continue;
        }

        if (std.mem.startsWith(u8, arg, "--test-filter=")) {
            const value = arg["--test-filter=".len..];
            appendFilter(&filter_list, b, value);
            i += 1;
            continue;
        }

        run_args_list.append(arg) catch @panic("OOM while recording build arguments");
        i += 1;
    }

    const run_args = run_args_list.toOwnedSlice() catch @panic("OOM while finalizing build arguments");
    const test_filters = filter_list.toOwnedSlice() catch @panic("OOM while finalizing test filters");

    return .{ .run_args = run_args, .test_filters = test_filters };
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

/// Generate glibc stubs at build time for cross-compilation
///
/// This is a minimal implementation that generates essential symbols needed for basic
/// cross-compilation to glibc targets. It creates assembly stubs with required symbols
/// like __libc_start_main, abort, getauxval, and _IO_stdin_used.
///
/// Future work: Parse Zig's abilists file to generate comprehensive
/// symbol coverage with proper versioning (e.g., symbol@@GLIBC_2.17). The abilists
/// contains thousands of glibc symbols across different versions and architectures
/// that could provide more complete stub coverage for complex applications.
fn generateGlibcStub(b: *std.Build, target: ResolvedTarget, target_name: []const u8) ?*Step.UpdateSourceFiles {

    // Generate assembly stub with comprehensive symbols using the new build module
    var assembly_buf = std.ArrayList(u8).init(b.allocator);
    defer assembly_buf.deinit();

    const writer = assembly_buf.writer();
    const target_arch = target.result.cpu.arch;
    const target_abi = target.result.abi;

    glibc_stub_build.generateComprehensiveStub(b.allocator, writer, target_arch, target_abi) catch |err| {
        std.log.warn("Failed to generate comprehensive stub assembly for {s}: {}, using minimal ELF", .{ target_name, err });
        // Fall back to minimal ELF
        const stub_content = switch (target.result.cpu.arch) {
            .aarch64 => createMinimalElfArm64(),
            .x86_64 => createMinimalElfX64(),
            else => return null,
        };

        const write_stub = b.addWriteFiles();
        const libc_so_6 = write_stub.add("libc.so.6", stub_content);
        const libc_so = write_stub.add("libc.so", stub_content);

        const copy_stubs = b.addUpdateSourceFiles();
        copy_stubs.addCopyFileToSource(libc_so_6, b.pathJoin(&.{ "test/int/platform/targets", target_name, "libc.so.6" }));
        copy_stubs.addCopyFileToSource(libc_so, b.pathJoin(&.{ "test/int/platform/targets", target_name, "libc.so" }));
        copy_stubs.step.dependOn(&write_stub.step);

        return copy_stubs;
    };

    // Write the assembly file to the targets directory
    const write_stub = b.addWriteFiles();
    const asm_file = write_stub.add("libc_stub.s", assembly_buf.items);

    // Compile the assembly into a proper shared library using Zig's build system
    const libc_stub = glibc_stub_build.compileAssemblyStub(b, asm_file, target, .ReleaseSmall);

    // Copy the generated files to the target directory
    const copy_stubs = b.addUpdateSourceFiles();
    copy_stubs.addCopyFileToSource(libc_stub.getEmittedBin(), b.pathJoin(&.{ "test/int/platform/targets", target_name, "libc.so.6" }));
    copy_stubs.addCopyFileToSource(libc_stub.getEmittedBin(), b.pathJoin(&.{ "test/int/platform/targets", target_name, "libc.so" }));
    copy_stubs.addCopyFileToSource(asm_file, b.pathJoin(&.{ "test/int/platform/targets", target_name, "libc_stub.s" }));
    copy_stubs.step.dependOn(&libc_stub.step);
    copy_stubs.step.dependOn(&write_stub.step);

    return copy_stubs;
}

/// Create a minimal ELF shared object for ARM64
fn createMinimalElfArm64() []const u8 {
    // ARM64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0xB7, 0x00, // e_machine - EM_AARCH64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}

/// Create a minimal ELF shared object for x86-64
fn createMinimalElfX64() []const u8 {
    // x86-64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0x3E, 0x00, // e_machine - EM_X86_64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}
