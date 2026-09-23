//! Regression test for issue #11299.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const Issue11299TestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.WriteFileError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// Check `source` as `main.roc` and assert that the out-of-scope name in it is
/// the only thing reported.
fn expectChecksWithNameNotInScope(source: []const u8) Issue11299TestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var found_name_not_in_scope = false;
    for (drained) |mod| {
        for (mod.reports) |report| {
            try std.testing.expectEqualStrings("Name Not In Scope", report.title);
            found_name_not_in_scope = true;
        }
    }
    try std.testing.expect(found_name_not_in_scope);
}

// A poisoned enclosing expression must not remove the type root of a retained
// body-free method declaration. Both original cases reproduced the registry panic.
test "issue 11299: a block-local derived encoder publishes when the block tail is out of scope" {
    try expectChecksWithNameNotInScope(
        \\f = || {
        \\    Flag := Bool.{ encoder_for : _ }
        \\    alias
        \\}
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 11299: a block-local derived encoder publishes when an out-of-scope name is called" {
    try expectChecksWithNameNotInScope(
        \\f = || {
        \\    Flag := Bool.{ encoder_for : _ }
        \\    (alias(1), alias("a"))
        \\}
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 11299: a derived parser publishes through nested rejected bodies" {
    try expectChecksWithNameNotInScope(
        \\f = || {
        \\    inner = || {
        \\        Flag := [Off, On].{ parser_for : _ }
        \\        alias
        \\    }
        \\    inner
        \\}
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 11299: a used derived encoder retains its callable type in a rejected block" {
    try expectChecksWithNameNotInScope(
        \\f = || {
        \\    Flag := [Off, On].{ encoder_for : _ }
        \\    _ = Json.to_str(Flag.On)
        \\    alias
        \\}
        \\
        \\main! = |_| Ok({})
        \\
    );
}
