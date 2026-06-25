//! Regression test for issue #9704.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 9704: unannotated platform render root lowers to LIR" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [Model, program] { pf: platform "./platform/main.roc" }
        \\
        \\Model : { pins : List(U64) }
        \\
        \\program = { init!, render! }
        \\
        \\init! : {} => Try(Model, [Exit(I64), ..])
        \\init! = |_| Ok({ pins: [] })
        \\
        \\f! = |a, _b, c|
        \\    match a {
        \\        [] => c
        \\        [_, .. as rest] => f!(rest, c, c)
        \\    }
        \\
        \\render! = |model, _|
        \\    Ok({ pins: f!(model.pins, model.pins, model.pins) })
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for program : {
        \\            init! : {} => Try(model, [Exit(I64), ..]),
        \\            render! : model, {} => Try(model, [Exit(I64), ..]),
        \\        }
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides {
        \\        "roc_init_for_host": init_for_host!,
        \\        "roc_render_for_host": render_for_host!,
        \\    }
        \\    hosted {}
        \\
        \\init_for_host! : {} => I64
        \\init_for_host! = |state|
        \\    match (program.init!)(state) {
        \\        Ok(_) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        \\
        \\render_for_host! : Box(Model) => I64
        \\render_for_host! = |boxed_model|
        \\    match (program.render!)(Box.unbox(boxed_model), {}) {
        \\        Ok(_) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}
