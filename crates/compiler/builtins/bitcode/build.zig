const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Build = std.Build;
const LazyPath = Build.LazyPath;
const CrossTarget = std.zig.CrossTarget;
const Arch = std.Target.Cpu.Arch;

pub fn build(b: *Build) void {
    // const mode = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });
    const mode = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });

    // Options
    const fallback_main_path = "./src/main.zig";
    const main_path_desc = b.fmt("Override path to main.zig. Used by \"ir\" and \"test\". Defaults to \"{s}\". ", .{fallback_main_path});
    const main_path = .{ .path = b.option([]const u8, "main-path", main_path_desc) orelse fallback_main_path };

    // Tests
    const main_tests = b.addTest(.{ .root_source_file = main_path, .link_libc = true });
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&b.addRunArtifact(main_tests).step);

    // Targets
    const host_target = b.standardTargetOptions(.{
        .default_target = CrossTarget{
            .cpu_model = .baseline,
            .os_tag = builtin.os.tag,
        },
    });
    const linux32_target = makeLinux32Target();
    const linux_x64_target = makeLinuxX64Target();
    const linux_aarch64_target = makeLinuxAarch64Target();
    const windows64_target = makeWindows64Target();
    const wasm32_target = makeWasm32Target();

    // LLVM IR
    generateLlvmIrFile(b, mode, host_target, main_path, "ir", "builtins-host");
    generateLlvmIrFile(b, mode, linux32_target, main_path, "ir-x86", "builtins-x86");
    generateLlvmIrFile(b, mode, linux_x64_target, main_path, "ir-x86_64", "builtins-x86_64");
    generateLlvmIrFile(b, mode, linux_aarch64_target, main_path, "ir-aarch64", "builtins-aarch64");
    generateLlvmIrFile(b, mode, windows64_target, main_path, "ir-windows-x86_64", "builtins-windows-x86_64");
    generateLlvmIrFile(b, mode, wasm32_target, main_path, "ir-wasm32", "builtins-wasm32");

    // Generate Object Files
    generateObjectFile(b, mode, host_target, main_path, "object", "builtins-host");
    generateObjectFile(b, mode, windows64_target, main_path, "windows-x86_64-object", "builtins-windows-x86_64");
    generateObjectFile(b, mode, wasm32_target, main_path, "wasm32-object", "builtins-wasm32");
}

// TODO zig 0.9 can generate .bc directly, switch to that when it is released!
fn generateLlvmIrFile(
    b: *Build,
    mode: std.builtin.Mode,
    target: CrossTarget,
    main_path: LazyPath,
    step_name: []const u8,
    object_name: []const u8,
) void {
    const obj = b.addObject(.{ .name = object_name, .root_source_file = main_path, .optimize = mode, .target = target, .use_llvm = true });
    obj.strip = true;

    // Generating the bin seems required to get zig to generate the llvm ir.
    _ = obj.getEmittedBin();
    const ir_file = obj.getEmittedLlvmIr();
    const bc_file = obj.getEmittedLlvmBc();
    const install_ir = b.addInstallFile(ir_file, b.fmt("{s}.ll", .{object_name}));
    const install_bc = b.addInstallFile(bc_file, b.fmt("{s}.bc", .{object_name}));

    const ir = b.step(step_name, "Build LLVM ir");
    ir.dependOn(&install_ir.step);
    ir.dependOn(&install_bc.step);
    b.getInstallStep().dependOn(ir);
}

// Generate Object File
// TODO: figure out how to get this to emit symbols that are only scoped to linkage (global but hidden).
// @bhansconnect: I believe anything with global scope will still be preserved by the linker even if it
// is never called. I think it could theoretically be called by a dynamic lib that links to the executable
// or something similar.
fn generateObjectFile(
    b: *Build,
    mode: std.builtin.Mode,
    target: CrossTarget,
    main_path: LazyPath,
    step_name: []const u8,
    object_name: []const u8,
) void {
    const obj = b.addObject(.{ .name = object_name, .root_source_file = main_path, .optimize = mode, .target = target, .use_llvm = true });
    obj.strip = true;
    obj.link_function_sections = true;
    obj.force_pic = true;

    const obj_file = obj.getEmittedBin();

    var suffix =
        if (target.os_tag == .windows)
        "obj"
    else
        "o";
    const install = b.addInstallFile(obj_file, b.fmt("{s}.{s}", .{ object_name, suffix }));

    const obj_step = b.step(step_name, "Build object file for linking");
    obj_step.dependOn(&obj.step);
    obj_step.dependOn(&install.step);
    b.getInstallStep().dependOn(obj_step);
}

fn makeLinux32Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = std.Target.Cpu.Arch.x86;
    target.os_tag = std.Target.Os.Tag.linux;
    target.abi = std.Target.Abi.musl;

    return target;
}

fn makeLinuxAarch64Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = std.Target.Cpu.Arch.aarch64;
    target.os_tag = std.Target.Os.Tag.linux;
    target.abi = std.Target.Abi.musl;

    return target;
}

fn makeLinuxX64Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = std.Target.Cpu.Arch.x86_64;
    target.os_tag = std.Target.Os.Tag.linux;
    target.abi = std.Target.Abi.musl;

    return target;
}

fn makeWindows64Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = std.Target.Cpu.Arch.x86_64;
    target.os_tag = std.Target.Os.Tag.windows;
    target.abi = std.Target.Abi.gnu;

    return target;
}

fn makeWasm32Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    // 32-bit wasm
    target.cpu_arch = std.Target.Cpu.Arch.wasm32;
    target.os_tag = std.Target.Os.Tag.freestanding;
    target.abi = std.Target.Abi.none;

    return target;
}
