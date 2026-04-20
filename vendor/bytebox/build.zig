const std = @import("std");
const CrossTarget = std.zig.CrossTarget;

const Build = std.Build;
const Module = Build.Module;
const Import = Module.Import;
const Compile = Build.Step.Compile;
const Step = Build.Step;
const ResolvedTarget = Build.ResolvedTarget;

const ExeOpts = struct {
    exe_name: []const u8,
    root_src: []const u8,
    step_name: []const u8,
    description: []const u8,
    always_install: bool = false,
    step_dependencies: ?[]const *Build.Step = null,
    emit_asm_step: ?*Build.Step = null,
    options: *Build.Step.Options,
};

const StackVmKind = enum {
    tailcall,
    labeled_switch,
};

const WasmArch = enum {
    wasm32,
    wasm64,
};

const WasmBuild = struct {
    compile: *Compile,
    install: *Step,
};

// At the time of this writing, zig's stage2 codegen backend can't handle tailcalls, so we'll default to using LLVM for simplicity
const use_llvm = true;

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

    const stable_array = b.dependency("zig-stable-array", .{
        .target = target,
        .optimize = optimize,
    });

    const add_one_wasm: WasmBuild = buildWasmExe(b, "bench/samples/add-one.zig", .wasm32);
    const fibonacci_wasm: WasmBuild = buildWasmExe(b, "bench/samples/fibonacci.zig", .wasm32);
    const mandelbrot_wasm: WasmBuild = buildWasmExe(b, "bench/samples/mandelbrot.zig", .wasm32);
    const json_wasm: WasmBuild = buildWasmExe(b, "bench/samples/json.zig", .wasm32);

    const stable_array_import = Import{ .name = "stable-array", .module = stable_array.module("zig-stable-array") };

    const bytebox_module: *Build.Module = b.addModule("bytebox", .{
        .root_source_file = b.path("src/core.zig"),
        .imports = &[_]Import{stable_array_import},
    });

    bytebox_module.addOptions("config", options);

    const emit_asm_step: *Build.Step = b.step("asm", "Emit assembly");

    const imports = [_]Import{
        .{ .name = "bytebox", .module = bytebox_module },
        .{ .name = "stable-array", .module = stable_array.module("zig-stable-array") },
    };

    const bytebox_exe_step = buildExeWithRunStep(b, target, optimize, &imports, .{
        .exe_name = "bytebox",
        .root_src = "run/main.zig",
        .step_name = "run",
        .description = "Run a wasm program",
        .always_install = true,
        .emit_asm_step = emit_asm_step,
        .options = options,
    });

    _ = buildExeWithRunStep(b, target, optimize, &imports, .{
        .exe_name = "bench",
        .root_src = "bench/main.zig",
        .step_name = "bench",
        .description = "Run the benchmark suite",
        .step_dependencies = &.{
            add_one_wasm.install,
            fibonacci_wasm.install,
            mandelbrot_wasm.install,
            json_wasm.install,
        },
        .options = options,
    });

    const lib_bytebox: *Compile = b.addLibrary(.{
        .name = "bytebox",
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cffi.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = use_llvm,
    });
    lib_bytebox.root_module.addImport(stable_array_import.name, stable_array_import.module);
    lib_bytebox.root_module.addOptions("config", options);
    lib_bytebox.installHeader(b.path("src/bytebox.h"), "bytebox.h");
    b.installArtifact(lib_bytebox);

    // Unit tests
    const unit_tests: *Compile = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = use_llvm,
    });
    unit_tests.root_module.addImport(stable_array_import.name, stable_array_import.module);
    unit_tests.root_module.addOptions("config", options);

    const run_unit_tests = b.addRunArtifact(unit_tests);
    run_unit_tests.step.dependOn(fibonacci_wasm.install);
    run_unit_tests.step.dependOn(mandelbrot_wasm.install);

    const unit_test_step = b.step("test-unit", "Run unit tests");
    unit_test_step.dependOn(&run_unit_tests.step);

    // wasm tests
    const wasm_testsuite_step = buildExeWithRunStep(b, target, optimize, &imports, .{
        .exe_name = "test-wasm",
        .root_src = "test/wasm/main.zig",
        .step_name = "test-wasm",
        .description = "Run the wasm testsuite",
        .options = options,
    });

    // wasi tests
    var maybe_wasi_testsuite_step: ?*Step = null;
    if (enable_wasi) {
        const wasi_testsuite = b.addSystemCommand(&.{"python3"});
        wasi_testsuite.addArg("test/wasi/run.py");
        wasi_testsuite.step.dependOn(bytebox_exe_step);

        maybe_wasi_testsuite_step = b.step("test-wasi", "Run wasi testsuite");
        maybe_wasi_testsuite_step.?.dependOn(&wasi_testsuite.step);
    }

    // mem64 test
    const compile_mem64_test: WasmBuild = buildWasmExe(b, "test/mem64/memtest.zig", .wasm64);

    const mem64_test_step: *Build.Step = buildExeWithRunStep(b, target, optimize, &imports, .{
        .exe_name = "test-mem64",
        .root_src = "test/mem64/main.zig",
        .step_name = "test-mem64",
        .description = "Run the mem64 test",
        .options = options,
        .step_dependencies = &.{compile_mem64_test.install},
    });

    // Cffi test
    const cffi_test = b.addExecutable(.{
        .name = "test-cffi",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = use_llvm,
    });
    cffi_test.addCSourceFile(.{
        .file = b.path("test/cffi/main.c"),
    });
    cffi_test.addIncludePath(b.path("src/bytebox.h"));
    cffi_test.linkLibC();
    cffi_test.linkLibrary(lib_bytebox);

    const ffi_guest: WasmBuild = buildWasmExe(b, "test/cffi/module.zig", .wasm32);

    const cffi_run_step = b.addRunArtifact(cffi_test);
    cffi_run_step.addFileArg(ffi_guest.compile.getEmittedBin());

    const cffi_test_step = b.step("test-cffi", "Run cffi test");
    cffi_test_step.dependOn(&cffi_run_step.step);

    // All tests
    const all_tests_step = b.step("test", "Run unit, wasm, and wasi tests");
    all_tests_step.dependOn(unit_test_step);
    all_tests_step.dependOn(wasm_testsuite_step);
    all_tests_step.dependOn(mem64_test_step);
    all_tests_step.dependOn(cffi_test_step);
    if (maybe_wasi_testsuite_step) |wasi_testsuite_step| {
        all_tests_step.dependOn(wasi_testsuite_step);
    }
}

fn buildExeWithRunStep(b: *Build, target: Build.ResolvedTarget, optimize: std.builtin.OptimizeMode, imports: []const Import, opts: ExeOpts) *Build.Step {
    const exe: *Compile = b.addExecutable(.{
        .name = opts.exe_name,
        .root_module = b.createModule(.{
            .root_source_file = b.path(opts.root_src),
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = use_llvm,
    });

    for (imports) |import| {
        exe.root_module.addImport(import.name, import.module);
    }
    exe.root_module.addOptions("config", opts.options);

    if (opts.emit_asm_step) |asm_step| {
        const asm_filename = std.fmt.allocPrint(b.allocator, "{s}.asm", .{opts.exe_name}) catch unreachable;
        asm_step.dependOn(&b.addInstallFile(exe.getEmittedAsm(), asm_filename).step);
    }

    if (opts.step_dependencies) |steps| {
        for (steps) |step| {
            exe.step.dependOn(step);
        }
    }

    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run.addArgs(args);
    }

    const install_exe = b.addInstallArtifact(exe, .{});

    const step: *Build.Step = b.step(opts.step_name, opts.description);
    step.dependOn(&install_exe.step);
    step.dependOn(&run.step);

    if (opts.always_install) {
        b.getInstallStep().dependOn(&install_exe.step);
    }

    return step;
}

fn buildWasmExe(b: *Build, filepath: []const u8, comptime arch: WasmArch) WasmBuild {
    var filename: []const u8 = std.fs.path.basename(filepath);
    const filename_no_extension: []const u8 = filename[0 .. filename.len - 4];

    const cpu_arch: std.Target.Cpu.Arch = if (arch == .wasm32) .wasm32 else .wasm64;

    var target_query: std.Target.Query = .{
        .cpu_arch = cpu_arch,
        .os_tag = .freestanding,
    };
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.bulk_memory));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.nontrapping_fptoint));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.multivalue));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.mutable_globals));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.reference_types));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.sign_ext));
    target_query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.simd128));

    var exe = b.addExecutable(.{
        .name = filename_no_extension,
        .root_module = b.createModule(.{
            .root_source_file = b.path(filepath),
            .target = b.resolveTargetQuery(target_query),
            .optimize = .ReleaseSmall,
        }),
        .use_llvm = use_llvm,
    });
    exe.rdynamic = true;
    exe.entry = .disabled;

    const install = b.addInstallArtifact(exe, .{});

    return WasmBuild{
        .compile = exe,
        .install = &install.step,
    };
}
