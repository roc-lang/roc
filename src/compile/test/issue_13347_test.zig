//! Regression test for issue #13347.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 13347: platform nominal backed by a for-clause abstract type checks and lowers" {
    // Repro for https://github.com/roc-lang/roc/issues/13347.
    // The platform has no imports, so the for-clause binder is the first
    // thing canonicalized in the module. A nominal declaration whose backing
    // is that abstract type must publish and lower like any other.
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main] { pf: platform "./platform/main.roc" }
        \\
        \\main : U64 -> U64
        \\main = |n| n
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for main : model -> model
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides { "entry": entry }
        \\
        \\S := Model
        \\
        \\entry : U64 -> U64
        \\entry = |n| n
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}
