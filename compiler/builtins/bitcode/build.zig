const std = @import("std");
const mem = std.mem;
const Builder = std.build.Builder;

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
    const obj_name = "builtins";
    const llvm_obj = b.addObject(obj_name, main_path);
    llvm_obj.setBuildMode(mode);
    llvm_obj.linkSystemLibrary("c");
    llvm_obj.strip = true;
    llvm_obj.emit_llvm_ir = true;
    llvm_obj.emit_bin = false;
    llvm_obj.bundle_compiler_rt = true;
    const ir = b.step("ir", "Build LLVM ir");
    ir.dependOn(&llvm_obj.step);

    // Object File
    // TODO: figure out how to get this to emit symbols that are only scoped to linkage (global but hidden).
    // Also, zig has -ffunction-sections, but I am not sure how to add it here.
    // With both of those changes, unused zig functions will be cleaned up by the linker saving around 100k.
    const obj = b.addObject(obj_name, main_path);
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
