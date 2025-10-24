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

fn mustUseLlvm(target: ResolvedTarget) bool {
    return target.result.os.tag == .macos and target.result.cpu.arch == .x86_64;
}

fn configureBackend(step: *Step.Compile, target: ResolvedTarget) void {
    if (mustUseLlvm(target)) {
        step.use_llvm = true;
    }
}

const TestsSummaryStep = struct {
    step: Step,

    fn create(b: *std.Build) *TestsSummaryStep {
        const self = b.allocator.create(TestsSummaryStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "tests_summary",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn addRun(self: *TestsSummaryStep, run_step: *Step) void {
        self.step.dependOn(run_step);
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        _ = options;

        var passed: u64 = 0;
        for (step.dependencies.items) |dependency| {
            passed += @intCast(dependency.test_results.passCount());
        }

        std.debug.print("✅ All {d} tests passed.\n", .{passed});
    }
};

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
    const serialization_size_step = b.step("test-serialization-sizes", "Verify Serialized types have platform-independent sizes");

    // general configuration
    const target = blk: {
        var default_target_query: std.Target.Query = .{
            .abi = if (builtin.target.os.tag == .linux) .musl else null,
        };

        // Use baseline x86_64 CPU for Valgrind compatibility on CI (Valgrind 3.18.1 doesn't support AVX-512)
        const is_ci = std.process.getEnvVarOwned(b.allocator, "CI") catch null;
        if (is_ci != null and builtin.target.cpu.arch == .x86_64 and builtin.target.os.tag == .linux) {
            default_target_query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64 };
        }

        break :blk b.standardTargetOptions(.{ .default_target = default_target_query });
    };
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

    const roc_modules = modules.RocModules.create(b, build_options, zstd);

    const roc_exe = addMainExe(b, roc_modules, target, optimize, strip, enable_llvm, use_system_llvm, user_llvm_path, flag_enable_tracy, zstd) orelse return;
    roc_modules.addAll(roc_exe);
    install_and_run(b, no_bin, roc_exe, roc_step, run_step, run_args);

    // Build-time compiler for builtin .roc modules with caching
    //
    // Changes to .roc files in src/build/roc/ are automatically detected and trigger recompilation.
    // However, if you modify the compiler itself (e.g., parse, can, check modules) and want those
    // changes reflected in the builtin .bin files, you need to run: zig build rebuild-builtins
    //
    // We cache the builtin compiler executable to avoid ~doubling normal build times.
    // CI always rebuilds from scratch, so it's not affected by this caching.

    // Discover all .roc files in src/build/roc/
    const roc_files = discoverBuiltinRocFiles(b) catch |err| {
        std.debug.print("Failed to discover builtin .roc files: {}\n", .{err});
        return;
    };

    // Check if we need to rebuild builtins by comparing .roc and .bin file timestamps
    const should_rebuild_builtins = blk: {
        for (roc_files) |roc_path| {
            // Get the base name (e.g., "Dict" from "src/build/roc/Dict.roc")
            const roc_basename = std.fs.path.basename(roc_path);
            const name_without_ext = roc_basename[0 .. roc_basename.len - 4]; // Remove ".roc"

            // Check if corresponding .bin file exists and is up-to-date
            const bin_path = b.fmt("zig-out/builtins/{s}.bin", .{name_without_ext});

            const roc_stat = std.fs.cwd().statFile(roc_path) catch break :blk true;
            const bin_stat = std.fs.cwd().statFile(bin_path) catch break :blk true;

            // If .roc file is newer than .bin file, rebuild
            if (roc_stat.mtime > bin_stat.mtime) {
                break :blk true;
            }
        }

        // All .bin files exist and are up-to-date
        break :blk false;
    };

    const write_compiled_builtins = b.addWriteFiles();

    if (should_rebuild_builtins) {
        // Build and run the compiler
        const builtin_compiler_exe = b.addExecutable(.{
            .name = "builtin_compiler",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/build/builtin_compiler/main.zig"),
                .target = b.graph.host, // this runs at build time on the *host* machine!
                .optimize = .Debug, // No need to optimize - only compiles builtin modules
                // Note: libc linking is handled by add_tracy below (required when tracy is enabled)
            }),
        });
        configureBackend(builtin_compiler_exe, b.graph.host);

        // Add only the minimal modules needed for parsing/checking
        builtin_compiler_exe.root_module.addImport("base", roc_modules.base);
        builtin_compiler_exe.root_module.addImport("collections", roc_modules.collections);
        builtin_compiler_exe.root_module.addImport("types", roc_modules.types);
        builtin_compiler_exe.root_module.addImport("parse", roc_modules.parse);
        builtin_compiler_exe.root_module.addImport("can", roc_modules.can);
        builtin_compiler_exe.root_module.addImport("check", roc_modules.check);
        builtin_compiler_exe.root_module.addImport("reporting", roc_modules.reporting);
        builtin_compiler_exe.root_module.addImport("builtins", roc_modules.builtins);

        // Add tracy support (required by parse/can/check modules)
        add_tracy(b, roc_modules.build_options, builtin_compiler_exe, b.graph.host, false, flag_enable_tracy);

        // Run the builtin compiler to generate .bin files in zig-out/builtins/
        const run_builtin_compiler = b.addRunArtifact(builtin_compiler_exe);

        // Add all .roc files as explicit file inputs so Zig's cache tracks them
        for (roc_files) |roc_path| {
            run_builtin_compiler.addFileArg(b.path(roc_path));
        }

        write_compiled_builtins.step.dependOn(&run_builtin_compiler.step);

        // Copy all generated .bin files from zig-out to build cache
        for (roc_files) |roc_path| {
            const roc_basename = std.fs.path.basename(roc_path);
            const name_without_ext = roc_basename[0 .. roc_basename.len - 4];
            const bin_filename = b.fmt("{s}.bin", .{name_without_ext});

            _ = write_compiled_builtins.addCopyFile(
                .{ .cwd_relative = b.fmt("zig-out/builtins/{s}", .{bin_filename}) },
                bin_filename,
            );
        }
    } else {
        // Use existing .bin files from zig-out/builtins/
        for (roc_files) |roc_path| {
            const roc_basename = std.fs.path.basename(roc_path);
            const name_without_ext = roc_basename[0 .. roc_basename.len - 4];
            const bin_filename = b.fmt("{s}.bin", .{name_without_ext});

            _ = write_compiled_builtins.addCopyFile(
                .{ .cwd_relative = b.fmt("zig-out/builtins/{s}", .{bin_filename}) },
                bin_filename,
            );
        }
    }

    // Also copy builtin_indices.bin
    _ = write_compiled_builtins.addCopyFile(
        .{ .cwd_relative = "zig-out/builtins/builtin_indices.bin" },
        "builtin_indices.bin",
    );

    // Generate compiled_builtins.zig dynamically based on discovered .roc files
    const builtins_source_str = generateCompiledBuiltinsSource(b, roc_files) catch |err| {
        std.debug.print("Failed to generate compiled_builtins.zig: {}\n", .{err});
        return;
    };

    const compiled_builtins_source = write_compiled_builtins.add(
        "compiled_builtins.zig",
        builtins_source_str,
    );

    const compiled_builtins_module = b.createModule(.{
        .root_source_file = compiled_builtins_source,
    });

    roc_modules.repl.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.compile.addImport("compiled_builtins", compiled_builtins_module);

    // Manual rebuild command: zig build rebuild-builtins
    // Use this after making compiler changes to ensure those changes are reflected in builtins
    const rebuild_builtins_step = b.step(
        "rebuild-builtins",
        "Force rebuild of all builtin modules (*.roc -> *.bin)",
    );

    // Discover .roc files again for the rebuild command
    const roc_files_force = discoverBuiltinRocFiles(b) catch |err| {
        std.debug.print("Failed to discover .roc files for rebuild: {}\n", .{err});
        return;
    };

    // Always build and run the compiler for this command
    const builtin_compiler_exe_force = b.addExecutable(.{
        .name = "builtin_compiler",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/builtin_compiler/main.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    configureBackend(builtin_compiler_exe_force, b.graph.host);

    builtin_compiler_exe_force.root_module.addImport("base", roc_modules.base);
    builtin_compiler_exe_force.root_module.addImport("collections", roc_modules.collections);
    builtin_compiler_exe_force.root_module.addImport("types", roc_modules.types);
    builtin_compiler_exe_force.root_module.addImport("parse", roc_modules.parse);
    builtin_compiler_exe_force.root_module.addImport("can", roc_modules.can);
    builtin_compiler_exe_force.root_module.addImport("check", roc_modules.check);
    builtin_compiler_exe_force.root_module.addImport("reporting", roc_modules.reporting);
    builtin_compiler_exe_force.root_module.addImport("builtins", roc_modules.builtins);

    add_tracy(b, roc_modules.build_options, builtin_compiler_exe_force, b.graph.host, false, flag_enable_tracy);

    const run_builtin_compiler_force = b.addRunArtifact(builtin_compiler_exe_force);

    // Add all discovered .roc files as inputs
    for (roc_files_force) |roc_path| {
        run_builtin_compiler_force.addFileArg(b.path(roc_path));
    }

    rebuild_builtins_step.dependOn(&run_builtin_compiler_force.step);

    // Add the compiled builtins module to roc exe and make it depend on the builtins being ready
    roc_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    roc_exe.step.dependOn(&write_compiled_builtins.step);

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/snapshot_tool/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(snapshot_exe, target);
    roc_modules.addAll(snapshot_exe);
    snapshot_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    snapshot_exe.step.dependOn(&write_compiled_builtins.step);
    add_tracy(b, roc_modules.build_options, snapshot_exe, target, false, flag_enable_tracy);
    install_and_run(b, no_bin, snapshot_exe, snapshot_step, snapshot_step, run_args);

    const playground_exe = b.addExecutable(.{
        .name = "playground",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/playground_wasm/main.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = optimize,
        }),
    });
    configureBackend(playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }));
    playground_exe.entry = .disabled;
    playground_exe.rdynamic = true;
    playground_exe.link_function_sections = true;
    playground_exe.import_memory = false;
    roc_modules.addAll(playground_exe);
    playground_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    playground_exe.step.dependOn(&write_compiled_builtins.step);

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
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/playground-integration/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(playground_integration_test_exe, target);
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

    // Add serialization size check
    // This verifies that Serialized types have the same size on 32-bit and 64-bit platforms
    // using compile-time assertions
    {
        // Build for native - will fail at compile time if sizes don't match expected
        const size_check_native = b.addExecutable(.{
            .name = "serialization_size_check_native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = target,
                .optimize = .Debug,
            }),
        });
        configureBackend(size_check_native, target);
        roc_modules.addAll(size_check_native);

        // Build for wasm32 (32-bit) - will fail at compile time if sizes don't match expected
        const size_check_wasm32 = b.addExecutable(.{
            .name = "serialization_size_check_wasm32",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = .Debug,
            }),
        });
        configureBackend(size_check_wasm32, b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }));
        size_check_wasm32.entry = .disabled;
        size_check_wasm32.rdynamic = true;
        roc_modules.addAll(size_check_wasm32);

        // Run the native version to confirm (wasm32 build is enough to verify 32-bit)
        const run_native = b.addRunArtifact(size_check_native);

        // The test passes if both executables build successfully (compile-time checks pass)
        // and the native one runs without error
        serialization_size_step.dependOn(&size_check_native.step);
        serialization_size_step.dependOn(&size_check_wasm32.step);
        serialization_size_step.dependOn(&run_native.step);
    }

    // Create and add module tests
    const tests_summary = TestsSummaryStep.create(b);
    const module_tests = roc_modules.createModuleTests(b, target, optimize, zstd, test_filters);
    for (module_tests) |module_test| {
        // Add compiled builtins to check, repl, and eval module tests
        if (std.mem.eql(u8, module_test.test_step.name, "check") or std.mem.eql(u8, module_test.test_step.name, "repl") or std.mem.eql(u8, module_test.test_step.name, "eval")) {
            module_test.test_step.root_module.addImport("compiled_builtins", compiled_builtins_module);
            module_test.test_step.step.dependOn(&write_compiled_builtins.step);
        }

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
        tests_summary.addRun(&module_test.run_step.step);
    }

    // Add snapshot tool test
    const enable_snapshot_tests = b.option(bool, "snapshot-tests", "Enable snapshot tests") orelse true;
    if (enable_snapshot_tests) {
        const snapshot_test = b.addTest(.{
            .name = "snapshot_tool_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/snapshot_tool/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(snapshot_test);
        snapshot_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        snapshot_test.step.dependOn(&write_compiled_builtins.step);
        add_tracy(b, roc_modules.build_options, snapshot_test, target, false, flag_enable_tracy);

        const run_snapshot_test = b.addRunArtifact(snapshot_test);
        if (run_args.len != 0) {
            run_snapshot_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_snapshot_test.step);
    }

    // Add CLI test
    const enable_cli_tests = b.option(bool, "cli-tests", "Enable cli tests") orelse true;
    if (enable_cli_tests) {
        const cli_test = b.addTest(.{
            .name = "cli_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(cli_test);
        cli_test.linkLibrary(zstd.artifact("zstd"));
        add_tracy(b, roc_modules.build_options, cli_test, target, false, flag_enable_tracy);
        cli_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        cli_test.step.dependOn(&write_compiled_builtins.step);

        const run_cli_test = b.addRunArtifact(cli_test);
        if (run_args.len != 0) {
            run_cli_test.addArgs(run_args);
        }
        tests_summary.addRun(&run_cli_test.step);
    }

    // Add watch tests
    const enable_watch_tests = b.option(bool, "watch-tests", "Enable watch tests") orelse true;
    if (enable_watch_tests) {
        const watch_test = b.addTest(.{
            .name = "watch_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/watch/watch.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
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
        tests_summary.addRun(&run_watch_test.step);
    }

    test_step.dependOn(&tests_summary.step);

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

fn discoverBuiltinRocFiles(b: *std.Build) ![]const []const u8 {
    const builtin_roc_path = try b.build_root.join(b.allocator, &.{ "src", "build", "roc" });
    var builtin_roc_dir = try std.fs.openDirAbsolute(builtin_roc_path, .{ .iterate = true });
    defer builtin_roc_dir.close();

    var roc_files = std.ArrayList([]const u8).empty;
    errdefer roc_files.deinit(b.allocator);

    var iter = builtin_roc_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const full_path = b.fmt("src/build/roc/{s}", .{entry.name});
            try roc_files.append(b.allocator, full_path);
        }
    }

    return roc_files.toOwnedSlice(b.allocator);
}

fn generateCompiledBuiltinsSource(b: *std.Build, roc_files: []const []const u8) ![]const u8 {
    var builtins_source = std.ArrayList(u8).empty;
    errdefer builtins_source.deinit(b.allocator);
    const writer = builtins_source.writer(b.allocator);

    for (roc_files) |roc_path| {
        const roc_basename = std.fs.path.basename(roc_path);
        const name_without_ext = roc_basename[0 .. roc_basename.len - 4];
        // Use lowercase with underscore for the identifier
        const lower_name = try std.ascii.allocLowerString(b.allocator, name_without_ext);

        try writer.print("pub const {s}_bin = @embedFile(\"{s}.bin\");\n", .{
            lower_name,
            name_without_ext,
        });
    }

    // Also embed builtin_indices.bin
    try writer.writeAll("pub const builtin_indices_bin = @embedFile(\"builtin_indices.bin\");\n");

    return builtins_source.toOwnedSlice(b.allocator);
}

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
        .root_module = b.createModule(.{
            .root_source_file = root_source_file,
            .target = target,
            // Work around instrumentation bugs on mac without giving up perf on linux.
            .optimize = if (target.result.os.tag == .macos) .Debug else .ReleaseSafe,
        }),
    });
    configureBackend(fuzz_obj, target);
    // Required for fuzzing.
    fuzz_obj.root_module.link_libc = true;
    fuzz_obj.root_module.stack_check = false;

    roc_modules.addAll(fuzz_obj);
    add_tracy(b, roc_modules.build_options, fuzz_obj, target, false, tracy);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const repro_step = b.step(name_repro, b.fmt("run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/fuzzing/fuzz-repro.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(repro_exe, target);
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);

    install_and_run(b, no_bin, repro_exe, repro_step, repro_step, run_args);

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(name_exe, b.fmt("Generate fuzz executable for {s}", .{name}));
        b.default_step.dependOn(fuzz_step);

        const afl = b.lazyImport(@This(), "afl_kit") orelse return;
        const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, &.{}, use_system_afl, fuzz_obj, &.{"-lm"}) orelse return;
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
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .link_libc = true,
        }),
    });
    configureBackend(exe, target);

    // Create test platform host static library (str)
    const test_platform_host_lib = b.addLibrary(.{
        .name = "test_platform_str_host",
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/str/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = optimize != .Debug,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
    });
    configureBackend(test_platform_host_lib, target);
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
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/int/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = optimize != .Debug,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
    });
    configureBackend(test_platform_int_host_lib, target);
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
                .strip = optimize != .Debug,
                .pic = true,
            }),
            .linkage = .static,
        });
        configureBackend(cross_int_host_lib, cross_resolved_target);
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

    // Create builtins static library at build time with minimal dependencies
    const builtins_obj = b.addObject(.{
        .name = "roc_builtins",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
    });
    configureBackend(builtins_obj, target);

    // Create shim static library at build time - fully static without libc
    //
    // NOTE we do NOT link libC here to avoid dynamic dependency on libC
    const shim_lib = b.addLibrary(.{
        .name = "roc_interpreter_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = optimize != .Debug,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    configureBackend(shim_lib, target);
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
    list.append(b.allocator, b.dupe(trimmed)) catch @panic("OOM while parsing --test-filter value");
}

fn parseBuildArgs(b: *std.Build) ParsedBuildArgs {
    const raw_args = b.args orelse return .{
        .run_args = &.{},
        .test_filters = &.{},
    };

    var run_args_list = std.ArrayList([]const u8).empty;
    var filter_list = std.ArrayList([]const u8).empty;

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

        run_args_list.append(b.allocator, arg) catch @panic("OOM while recording build arguments");
        i += 1;
    }

    const run_args = run_args_list.toOwnedSlice(b.allocator) catch @panic("OOM while finalizing build arguments");
    const test_filters = filter_list.toOwnedSlice(b.allocator) catch @panic("OOM while finalizing test filters");

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
    "LLVMSPIRVAnalysis",
    "LLVMSPIRVCodeGen",
    "LLVMSPIRVDesc",
    "LLVMSPIRVInfo",
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
    "LLVMCGData",
    "LLVMHipStdPar",
    "LLVMCFGuard",
    "LLVMCoroutines",
    "LLVMSandboxIR",
    "LLVMipo",
    "LLVMVectorize",
    "LLVMLinker",
    "LLVMInstrumentation",
    "LLVMFrontendOpenMP",
    "LLVMFrontendAtomic",
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
    var assembly_buf = std.ArrayList(u8).empty;
    defer assembly_buf.deinit(b.allocator);

    const writer = assembly_buf.writer(b.allocator);
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
