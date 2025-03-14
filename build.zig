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
    const target = b.standardTargetOptions(.{ .default_target = .{
        .abi = if (builtin.target.os.tag == .linux) .musl else null,
    } });
    const optimize = b.standardOptimizeOption(.{});
    const strip = b.option(bool, "strip", "Omit debug information");

    const run_step = b.step("run", "Build and run the roc cli");
    const test_step = b.step("test", "Run all tests included in src/tests.zig");
    const fmt_step = b.step("fmt", "Format all zig code");
    const check_fmt_step = b.step("check-fmt", "Check formatting of all zig code");
    const snapshot_step = b.step("snapshot", "Run the snapshot tool to update snapshot files");

    // llvm configuration
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const use_system_llvm = b.option(bool, "system-llvm", "Attempt to automatically detect and use system installed llvm") orelse false;
    const enable_llvm = b.option(bool, "llvm", "Build roc with the llvm backend") orelse use_system_llvm;
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to llvm. This path must contain the bin, lib, and include directory.");
    const use_system_afl = b.option(bool, "system-afl", "Attempt to automatically detect and use system installed afl++") orelse false;

    if (user_llvm_path) |path| {
        // Even if the llvm backend is not enabled, still add the llvm path.
        // AFL++ may use it for building fuzzing executables.
        b.addSearchPrefix(b.pathJoin(&.{ path, "bin" }));
    }

    const roc_exe = addMainExe(b, target, optimize, strip, enable_llvm, use_system_llvm, user_llvm_path) orelse return;

    if (no_bin) {
        b.getInstallStep().dependOn(&roc_exe.step);
    } else {
        b.installArtifact(roc_exe);
        const run_cmd = b.addRunArtifact(roc_exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        run_step.dependOn(&run_cmd.step);
    }

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_source_file = b.path("src/snapshot.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    if (no_bin) {
        b.getInstallStep().dependOn(&snapshot_exe.step);
    } else {
        b.installArtifact(snapshot_exe);
        const run_snapshot = b.addRunArtifact(snapshot_exe);

        // Add a step to run the snapshot tool
        run_snapshot.step.dependOn(b.getInstallStep());
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

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const run_repro_step = b.step(name_repro, b.fmt("run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_source_file = b.path("src/fuzz-repro.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);
    if (no_bin) {
        b.getInstallStep().dependOn(&repro_exe.step);
    } else {
        b.installArtifact(repro_exe);

        const run_cmd = b.addRunArtifact(repro_exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        run_repro_step.dependOn(&run_cmd.step);
    }

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(name_exe, b.fmt("Generate fuzz executable for {s}", .{name}));
        b.default_step.dependOn(fuzz_step);

        const afl = b.lazyImport(@This(), "afl_kit") orelse return;
        const fuzz_exe = afl.addInstrumentedExe(b, target, .ReleaseSafe, &.{}, use_system_afl, fuzz_obj) orelse return;
        fuzz_step.dependOn(&b.addInstallBinFile(fuzz_exe, name_exe).step);
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

    return exe;
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
