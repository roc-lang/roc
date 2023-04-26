const std = @import("std");
const mem = std.mem;
const Builder = std.build.Builder;
const CrossTarget = std.zig.CrossTarget;
const Arch = std.Target.Cpu.Arch;
const log = std.log;

pub fn build(b: *Builder) void {
    const mode = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseFast,
    });

    // Options
    const fallback_main_path = "./src/main.zig";
    const main_path_desc = b.fmt("Override path to main.zig. Used by \"ir\" and \"test\". Defaults to \"{s}\". ", .{fallback_main_path});
    const main_path = b.option([]const u8, "main-path", main_path_desc) orelse fallback_main_path;

    // Tests
    var main_tests = b.addTest(.{
        .root_source_file = .{ .path = main_path },
        .optimize = mode,
    });
    main_tests.linkSystemLibrary("c");
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);

    // Targets
    const host_target = b.standardTargetOptions(.{
        .default_target = CrossTarget{
            .cpu_model = .baseline,
            // TODO allow for native target for maximum speed
        },
    });
    const linux32_target = makeLinux32Target();
    const linux64_target = makeLinux64Target();
    const windows64_target = makeWindows64Target();
    const wasm32_target = makeWasm32Target();

    // LLVM IR
    generateLlvmIrFile(b, mode, host_target, main_path, "ir", "builtins-host");
    generateLlvmIrFile(b, mode, linux32_target, main_path, "ir-i386", "builtins-i386");
    generateLlvmIrFile(b, mode, linux64_target, main_path, "ir-x86_64", "builtins-x86_64");
    generateLlvmIrFile(b, mode, windows64_target, main_path, "ir-windows-x86_64", "builtins-windows-x86_64");
    generateLlvmIrFile(b, mode, wasm32_target, main_path, "ir-wasm32", "builtins-wasm32");

    // Generate Object Files
    generateObjectFile(b, mode, host_target, main_path, "object", "builtins-host");
    generateObjectFile(b, mode, windows64_target, main_path, "windows-x86_64-object", "builtins-windows-x86_64");
    generateObjectFile(b, mode, wasm32_target, main_path, "wasm32-object", "builtins-wasm32");

    removeInstallSteps(b);
}

// TODO zig 0.9 can generate .bc directly, switch to that when it is released!
fn generateLlvmIrFile(
    b: *Builder,
    mode: std.builtin.Mode,
    target: CrossTarget,
    main_path: []const u8,
    step_name: []const u8,
    object_name: []const u8,
) void {
    const obj = b.addObject(.{
        .name = object_name,
        .root_source_file = .{ .path = main_path },
        .target = target,
        .optimize = mode,
    });
    obj.strip = true;
    obj.emit_llvm_ir = .emit;
    obj.emit_llvm_bc = .emit;
    const ir = b.step(step_name, "Build LLVM ir");
    ir.dependOn(&obj.step);
}

// Generate Object File
// TODO: figure out how to get this to emit symbols that are only scoped to linkage (global but hidden).
// @bhansconnect: I believe anything with global scope will still be preserved by the linker even if it
// is never called. I think it could theoretically be called by a dynamic lib that links to the executable
// or something similar.
fn generateObjectFile(
    b: *Builder,
    mode: std.builtin.Mode,
    target: CrossTarget,
    main_path: []const u8,
    step_name: []const u8,
    object_name: []const u8,
) void {
    const obj = b.addObject(.{
        .name = object_name,
        .root_source_file = .{ .path = main_path },
        .target = target,
        .optimize = mode,
    });
    obj.linkSystemLibrary("c");
    obj.setOutputDir(".");
    obj.strip = true;
    obj.target = target;
    obj.link_function_sections = true;
    const obj_step = b.step(step_name, "Build object file for linking");
    obj_step.dependOn(&obj.step);
    //    _ = b.step(step_name, "Build object file for linking");

    //    obj_step.dependOn(&obj.step);
}

fn makeLinux32Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = .x86;
    target.os_tag = .linux;
    target.abi = .musl;

    return target;
}

fn makeLinux64Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = std.Target.Cpu.Arch.x86_64;
    target.os_tag = std.Target.Os.Tag.linux;
    target.abi = std.Target.Abi.musl;

    return target;
}

fn makeWindows64Target() CrossTarget {
    var target = CrossTarget.parse(.{}) catch unreachable;

    target.cpu_arch = .x86_64;
    target.os_tag = .windows;
    target.abi = .gnu;

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

fn ignore(_: *Builder.Step, _: *std.Progress.Node) anyerror!void {}

fn removeInstallSteps(b: *Builder) void {
    for (b.top_level_steps.values()) |v| {
        const name = v.step.name;
        if (mem.eql(u8, name, "install") or mem.eql(u8, name, "uninstall")) {
            v.step.makeFn = ignore;
        }
    }
}
