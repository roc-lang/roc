//! Regression test for issue #10765.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const Issue10765TestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.WriteFileError ||
    error{TestUnexpectedResult};

/// Check `source` as `main.roc` and assert that the out-of-scope name in it is
/// reported to the user.
fn expectChecksWithNameNotInScope(source: []const u8) Issue10765TestError!void {
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
    var found_missing_method = false;
    for (drained) |mod| {
        for (mod.reports) |report| {
            if (std.mem.eql(u8, report.title, "Name Not In Scope")) found_name_not_in_scope = true;
            if (std.mem.eql(u8, report.title, "Missing Method")) found_missing_method = true;
        }
    }
    try std.testing.expect(found_name_not_in_scope);
    try std.testing.expect(!found_missing_method);
}

// repro for https://github.com/roc-lang/roc/issues/10765
//
// Two calls to the same function, where one argument's numeric binop operand is
// an out-of-scope name and the other's is a literal, must publish as a checked
// artifact carrying the name-not-in-scope error.
test "issue 10765: numeric dispatch on an out-of-scope operand reports the name" {
    try expectChecksWithNameNotInScope(
        \\f = |n| f(n - d) + f(n - 2)
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 10765: a later erroneous dispatch does not retire an earlier valid sibling" {
    try expectChecksWithNameNotInScope(
        \\f = |n| f(n - 2) + f(n - d)
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 10765: nested erroneous dispatch operand reports the name" {
    try expectChecksWithNameNotInScope(
        \\f = |n| n.foo({ bad: d }) + n.foo({ bad: 2 })
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 10765: erroneous for iterable does not introduce iterator constraints" {
    try expectChecksWithNameNotInScope(
        \\f = || {
        \\    for _ in { bad: d } 0
        \\}
        \\
        \\main! = |_| Ok({})
        \\
    );
}

test "issue 10765: recursive fib with an out-of-scope operand reports the name" {
    try expectChecksWithNameNotInScope(
        \\fib = |n| if n <= !1 n else fib(n - d) + fib(n - 2)
        \\
        \\main! = |_| Ok({})
        \\
    );
}
