const std = @import("std");
const mem = std.mem;
const Builder = std.build.Builder;
const CrossTarget = std.zig.CrossTarget;

pub fn build(b: *Builder) void {
    // b.setPreferredReleaseMode(builtin.Mode.Debug
    b.setPreferredReleaseMode(.ReleaseFast);
    const mode = b.standardReleaseOptions();

    // Options
    const fallback_main_path = "./src/main.zig";
    const main_path_desc = b.fmt("Override path to main.zig. Used by \"ir\" and \"test\". Defaults to \"{s}\". ", .{fallback_main_path});
    const main_path = b.option([]const u8, "main-path", main_path_desc) orelse fallback_main_path;

    // Tests
    var main_tests = b.addTest(main_path);
    main_tests.setBuildMode(mode);
    main_tests.linkSystemLibrary("c");
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);

    // LLVM IR
    const obj_name = "builtins-host";
    const llvm_obj = b.addObject(obj_name, main_path);
    llvm_obj.setBuildMode(mode);
    llvm_obj.linkSystemLibrary("c");
    llvm_obj.strip = true;
    llvm_obj.emit_llvm_ir = true;
    llvm_obj.emit_bin = false;
    const ir = b.step("ir", "Build LLVM ir");
    ir.dependOn(&llvm_obj.step);

    // 32-bit x86, useful for debugging
    var i386_target = CrossTarget.parse(.{}) catch unreachable;

    i386_target.cpu_arch = std.Target.Cpu.Arch.i386;
    i386_target.os_tag = std.Target.Os.Tag.linux;
    i386_target.abi = std.Target.Abi.musl;

    const obj_name_i386 = "builtins-i386";
    const llvm_obj_i386 = b.addObject(obj_name_i386, main_path);
    llvm_obj_i386.setBuildMode(mode);
    llvm_obj_i386.strip = true;
    llvm_obj_i386.emit_llvm_ir = true;
    llvm_obj_i386.emit_bin = false;
    llvm_obj_i386.target = i386_target;

    const ir_i386 = b.step("ir-i386", "Build LLVM ir for 32-bit targets (x86)");
    ir_i386.dependOn(&llvm_obj_i386.step);

    // LLVM IR 32-bit (wasm)
    var wasm32_target = CrossTarget.parse(.{}) catch unreachable;

    // 32-bit wasm
    wasm32_target.cpu_arch = std.Target.Cpu.Arch.wasm32;
    wasm32_target.os_tag = std.Target.Os.Tag.freestanding;
    wasm32_target.abi = std.Target.Abi.none;

    const obj_name_wasm32 = "builtins-wasm32";
    const llvm_obj_wasm32 = b.addObject(obj_name_wasm32, main_path);
    llvm_obj_wasm32.setBuildMode(mode);
    llvm_obj_wasm32.strip = true;
    llvm_obj_wasm32.emit_llvm_ir = true;
    llvm_obj_wasm32.emit_bin = false;
    llvm_obj_wasm32.target = wasm32_target;

    const ir_wasm32 = b.step("ir-wasm32", "Build LLVM ir for 32-bit targets (wasm)");
    ir_wasm32.dependOn(&llvm_obj_wasm32.step);

    // Object File
    // TODO: figure out how to get this to emit symbols that are only scoped to linkage (global but hidden).
    // Also, zig has -ffunction-sections, but I am not sure how to add it here.
    // With both of those changes, unused zig functions will be cleaned up by the linker saving around 100k.
    const obj = b.addObject("builtins-host", main_path);
    obj.setBuildMode(mode);
    obj.linkSystemLibrary("c");
    obj.setOutputDir(".");
    obj.strip = true;
    const obj_step = b.step("object", "Build object file for linking");
    obj_step.dependOn(&obj.step);

    b.default_step = ir;
    removeInstallSteps(b);
}

fn removeInstallSteps(b: *Builder) void {
    for (b.top_level_steps.items) |top_level_step, i| {
        if (mem.eql(u8, top_level_step.step.name, "install") or mem.eql(u8, top_level_step.step.name, "uninstall")) {
            const name = top_level_step.step.name;
            _ = b.top_level_steps.swapRemove(i);
        }
    }
}
