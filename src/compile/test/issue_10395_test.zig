//! Regression test for issue #10395.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 10395: platform requirement record with a function field lowers to LIR" {
    // Repro for https://github.com/roc-lang/roc/issues/10395.
    // A valid two-field requirement record must lower without invalidating its
    // field span while the function-valued field's type is lowered.
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [program] { pf: platform "./platform/main.roc" }
        \\
        \\program = { render, title }
        \\
        \\render = |_s| []
        \\
        \\title = "example"
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/Widget.roc",
        .data =
        \\Event : {
        \\    f1 : F64,
        \\    f2 : F64,
        \\    f3 : F64,
        \\    f4 : F64,
        \\    f5 : F64,
        \\    f6 : F64,
        \\    f7 : F64,
        \\    f8 : F64,
        \\    f9 : F64,
        \\    f10 : F64,
        \\    f11 : F64,
        \\    f12 : F64,
        \\    f13 : F64,
        \\    f14 : F64,
        \\}
        \\
        \\Widget := { on : Event -> {} }
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} {
        \\        program : {
        \\            render : Str -> List(Widget),
        \\            title : Str,
        \\        },
        \\    }
        \\    exposes [Widget]
        \\    packages {}
        \\    provides {
        \\        "roc_render": render_for_host,
        \\        "roc_title": title_for_host,
        \\    }
        \\    hosted {}
        \\
        \\import Widget exposing [Widget]
        \\
        \\render_for_host : Str -> List(Widget)
        \\render_for_host = |s| (program.render)(s)
        \\
        \\title_for_host : Str
        \\title_for_host = program.title
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}
