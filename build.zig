const std = @import("std");
const builtin = @import("builtin");
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

    // general configuration
    const target = b.standardTargetOptions(.{ .default_target = .{
        .abi = if (builtin.target.os.tag == .linux) .musl else null,
    } });
    const optimize = b.standardOptimizeOption(.{});
    const strip = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;

    // llvm configuration
    const use_system_llvm = b.option(bool, "system-llvm", "Attempt to automatically detect and use system installed llvm") orelse false;
    const enable_llvm = b.option(bool, "llvm", "Build roc with the llvm backend") orelse use_system_llvm;
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to llvm. This path must contain the bin, lib, and include directory.");
    // Since zig afl is broken currently, default to system afl.
    const use_system_afl = b.option(bool, "system-afl", "Attempt to automatically detect and use system installed afl++") orelse true;

    if (user_llvm_path) |path| {
        // Even if the llvm backend is not enabled, still add the llvm path.
        // AFL++ may use it for building fuzzing executables.
        b.addSearchPrefix(b.pathJoin(&.{ path, "bin" }));
    }

    // tracy profiler configuration
    const tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");
    const tracy_callstack = b.option(bool, "tracy-callstack", "Include callstack information with Tracy data. Does nothing if -Dtracy is not provided") orelse (tracy != null);
    const tracy_allocation = b.option(bool, "tracy-allocation", "Include allocation information with Tracy data. Does nothing if -Dtracy is not provided") orelse (tracy != null);
    const tracy_callstack_depth: u32 = b.option(u32, "tracy-callstack-depth", "Declare callstack depth for Tracy data. Does nothing if -Dtracy_callstack is not provided") orelse 10;

    // Create compile time build options
    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_tracy", tracy != null);
    build_options.addOption(bool, "enable_tracy_callstack", tracy_callstack);
    build_options.addOption(bool, "enable_tracy_allocation", tracy_allocation);
    build_options.addOption(u32, "tracy_callstack_depth", tracy_callstack_depth);

    // add main roc exe
    const roc_exe = addMainExe(b, target, optimize, strip, enable_llvm, use_system_llvm, user_llvm_path, tracy) orelse return;
    roc_exe.root_module.addOptions("build_options", build_options);
    if (no_bin) {
        roc_step.dependOn(&roc_exe.step);
        b.getInstallStep().dependOn(&roc_exe.step);
    } else {
        const install_roc = b.addInstallArtifact(roc_exe, .{});
        b.getInstallStep().dependOn(&install_roc.step);
        roc_step.dependOn(&install_roc.step);
        const run_roc = b.addRunArtifact(roc_exe);
        run_roc.step.dependOn(&install_roc.step);
        if (b.args) |args| {
            run_roc.addArgs(args);
        }
        run_step.dependOn(&run_roc.step);
    }

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_source_file = b.path("src/snapshot.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    snapshot_exe.root_module.addOptions("build_options", build_options);
    add_tracy(b, snapshot_exe, target, false, tracy);
    if (no_bin) {
        snapshot_step.dependOn(&snapshot_exe.step);
        b.getInstallStep().dependOn(&snapshot_exe.step);
    } else {
        const install_snapshot = b.addInstallArtifact(snapshot_exe, .{});
        b.getInstallStep().dependOn(&install_snapshot.step);
        snapshot_step.dependOn(&install_snapshot.step);
        const run_snapshot = b.addRunArtifact(snapshot_exe);
        run_snapshot.step.dependOn(&install_snapshot.step);
        if (b.args) |args| {
            run_snapshot.addArgs(args);
        }
        snapshot_step.dependOn(&run_snapshot.step);
    }

    const all_tests = b.addTest(.{
        .root_source_file = b.path("src/test.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    all_tests.root_module.addOptions("build_options", build_options);

    if (!no_bin) {
        const run_tests = b.addRunArtifact(all_tests);
        test_step.dependOn(&run_tests.step);
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
            build_options,
            tracy,
            name,
        );
    }
}

fn add_fuzz_target(
    b: *std.Build,
    fuzz: bool,
    build_afl: bool,
    use_system_afl: bool,
    no_bin: bool,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    build_options: *Step.Options,
    tracy: ?[]const u8,
    name: []const u8,
) void {
    // We always include the repro scripts (no dependencies).
    // We only include the fuzzing scripts if `-Dfuzz` is set.
    const root_source_file = b.path(b.fmt("src/fuzz-{s}.zig", .{name}));
    const fuzz_obj = b.addObject(.{
        .name = b.fmt("{s}_obj", .{name}),
        .root_source_file = root_source_file,
        .target = target,
        // Work around instrumentation bugs on mac without giving up perf on linux.
        .optimize = if (target.result.os.tag == .macos) .Debug else .ReleaseSafe,
    });
    fuzz_obj.root_module.addOptions("build_options", build_options);
    add_tracy(b, fuzz_obj, target, false, tracy);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const repro_step = b.step(name_repro, b.fmt("run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_source_file = b.path("src/fuzz-repro.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);
    if (no_bin) {
        repro_step.dependOn(&repro_exe.step);
        b.getInstallStep().dependOn(&repro_exe.step);
    } else {
        const install_repro = b.addInstallArtifact(repro_exe, .{});
        b.getInstallStep().dependOn(&install_repro.step);
        const run_repro = b.addRunArtifact(repro_exe);
        run_repro.step.dependOn(&install_repro.step);
        if (b.args) |args| {
            run_repro.addArgs(args);
        }
        repro_step.dependOn(&run_repro.step);
        b.installArtifact(repro_exe);

        const run_cmd = b.addRunArtifact(repro_exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        repro_step.dependOn(&run_cmd.step);
    }

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
    target: ResolvedTarget,
    optimize: OptimizeMode,
    strip: ?bool,
    enable_llvm: bool,
    use_system_llvm: bool,
    user_llvm_path: ?[]const u8,
    tracy: ?[]const u8,
) ?*Step.Compile {
    const exe = b.addExecutable(.{
        .name = "roc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .strip = strip,
        .link_libc = true,
    });

    const config = b.addOptions();
    config.addOption(bool, "llvm", enable_llvm);
    exe.root_module.addOptions("config", config);

    if (enable_llvm) {
        const llvm_paths = llvmPaths(b, target, use_system_llvm, user_llvm_path) orelse return null;

        exe.addLibraryPath(.{ .cwd_relative = llvm_paths.lib });
        exe.addIncludePath(.{ .cwd_relative = llvm_paths.include });
        try addStaticLlvmOptionsToModule(exe.root_module);
    }

    add_tracy(b, exe, target, enable_llvm, tracy);
    return exe;
}

fn add_tracy(
    b: *std.Build,
    base: *Step.Compile,
    target: ResolvedTarget,
    links_llvm: bool,
    tracy: ?[]const u8,
) void {
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

    // No user specified llvm. Go download it from roc-boostrap.
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

    for (lld_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, .{});
    }

    for (llvm_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, .{});
    }

    mod.linkSystemLibrary("z", .{});
    mod.linkSystemLibrary("zstd", .{});

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
    "src/zig_llvm.cpp",
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
