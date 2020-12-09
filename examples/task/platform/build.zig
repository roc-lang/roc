const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const obj = b.addObject("host", "src/main.zig");
    obj.setBuildMode(mode);
    obj.linkSystemLibrary("c");
    obj.addPackagePath("roc_str", "../../../compiler/builtins/bitcode/src/str.zig");

    // force emit the `.o` file
    obj.emit_bin = true;
    obj.setOutputDir(".");

    // unsure what to do with this
    // fixes `undefined reference to __zig_probe_stack`
    // but causes `ld.lld: error: duplicate symbol: __cmpdf2`
    obj.bundle_compiler_rt = true;

    // because the default will hit an `unreachable`
    obj.override_dest_dir = .{ .Custom = "." };
    obj.install();
}
