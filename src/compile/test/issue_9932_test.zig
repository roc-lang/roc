//! Regression test for issue #9932: building an app where an unannotated
//! function provided to the platform's requires record compares its
//! for-clause model parameter against a numeric literal panicked with
//! "checked artifact invariant violated: platform/app relation digest
//! nominal mismatch" in PlatformAppRelationTypeDigestBuilder.writeMerge.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 9932: unannotated requires function comparing model to a numeric literal builds without digest panic" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [Model, program] { pf: platform "./platform/main.roc" }
        \\
        \\Model : I32
        \\
        \\program = { foo, bar }
        \\
        \\foo = "app"
        \\
        \\bar = |model|
        \\    if model < 3 "low" else "high" # boom!
        ,
    });
    // The requires record has a second entry (foo) only because a
    // single-field requires record currently fails platform requirement
    // matching outright, which would mask the digest panic this test is for.
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for program : {
        \\            foo : Str,
        \\            bar : model -> Str,
        \\        }
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides {
        \\        "roc_bar": bar_for_host,
        \\    }
        \\    hosted {}
        \\
        \\bar_for_host : Box(Model) -> Str
        \\bar_for_host = |boxed| (program.bar)(Box.unbox(boxed))
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}
