//! Regression tests for https://github.com/roc-lang/roc/issues/13342.
//!
//! A top-level value that reads an error-free component of an erroneous value
//! reports only the mistake that made the value erroneous, and the program
//! still lowers: the access is a checked crash, so post-check lowering never
//! reads the erroneous type of its receiver.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const harness = @import("lower_to_lir_harness.zig");
const BuildEnv = compile_build.BuildEnv;

const RequirementTestError = std.mem.Allocator.Error || compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

test "issue 13342: a constant reading an error-free field of an erroneous record lowers" {
    try harness.expectLowersToLirWithOptions(
        \\n = { bad: nope, n: 5.U64 }.n
        \\
        \\main! = |_args| {
        \\    _ = n
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}

test "issue 13342: a constant reading an error-free item of an erroneous tuple lowers" {
    try harness.expectLowersToLirWithOptions(
        \\n = (nope, 5.U64).1
        \\
        \\main! = |_args| {
        \\    _ = n
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true });
}

test "issue 13342: a platform constant reading a field of a requirement with an undeclared field type" {
    try expectPlatformReportsOnlyUndeclaredType(
        \\platform ""
        \\    requires { main : { bad : Nope, n : U64 } }
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\
        \\n = main.n
    );
}

test "issue 13342: a platform constant reading an item of a requirement with an undeclared item type" {
    try expectPlatformReportsOnlyUndeclaredType(
        \\platform ""
        \\    requires { main : (Nope, U64) }
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\
        \\n = main.1
    );
}

/// Check a platform module on its own, as `roc check` does, and require that
/// its only error is the undeclared type.
fn expectPlatformReportsOnlyUndeclaredType(platform_source: []const u8) RequirementTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = platform_source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const platform_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(platform_path);
    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss);
    defer build_env.deinit();
    try build_env.build(platform_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    var undeclared_types: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Undeclared Type")) {
                undeclared_types += 1;
            } else {
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), undeclared_types);
}
