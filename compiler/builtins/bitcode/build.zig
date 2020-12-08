const builtin = @import("builtin");
const std = @import("std");
const mem = std.mem;
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    b.setPreferredReleaseMode(builtin.Mode.ReleaseFast);
    const mode = b.standardReleaseOptions();

    // Options
    const fallback_main_path = "./src/main.zig";
    const main_path_desc = b.fmt("Override path to main.zig. Used by \"ir\" and \"test\". Defaults to \"{}\". ", .{fallback_main_path});
    const main_path = b.option([]const u8, "main-path", main_path_desc) orelse fallback_main_path;

    // Tests
    var main_tests = b.addTest(main_path);
    main_tests.setBuildMode(mode);
    main_tests.linkSystemLibrary("c");
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&main_tests.step);

    // Lib
    const obj_name = "builtins";
    const obj = b.addObject(obj_name, main_path);
    obj.setBuildMode(mode);
    obj.linkSystemLibrary("c");
    obj.strip = true;
    obj.emit_llvm_ir = true;
    obj.emit_bin = false;
    const ir = b.step("ir", "Build LLVM ir");
    ir.dependOn(&obj.step);

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
