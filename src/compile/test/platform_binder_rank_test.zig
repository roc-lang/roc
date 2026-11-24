//! A platform requirement's for-clause binder is an outermost-rank variable
//! shared by every requirement. An annotated required definition that is a
//! scheme (here `init`, whose output tag union opens a quantified row) is
//! related to that requirement after its body boundary generalized it, so the
//! relation must use an instance of the scheme. Otherwise the binder joins the
//! scheme's generalized classes, and the monomorphic definitions it reaches
//! (`loaded` and `refresh`) share generalized variables without being schemes,
//! which the stored record field `refresh(loaded(model))` in `step` exposes.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "platform requirement relates an annotated scheme definition through an instance" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [init, loaded, step] { pf: platform "./platform/main.roc" }
        \\
        \\Model : { mode : [Walk, Run] }
        \\
        \\init : {} -> Model
        \\init = |{}| { mode: Walk }
        \\
        \\refresh : Model -> Model
        \\refresh = |model| model
        \\
        \\loaded : Model -> Model
        \\loaded = |model| refresh(model)
        \\
        \\step : Model -> { model : Model, n : U64 }
        \\step = |model| { model: refresh(loaded(model)), n: 1 }
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for init : {} -> model,
        \\        loaded : Model -> Model,
        \\        step : Model -> { model : Model, n : U64 },
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides {
        \\        "roc_init": init_for_host,
        \\        "roc_loaded": loaded_for_host,
        \\        "roc_step": step_for_host,
        \\    }
        \\    hosted {}
        \\
        \\init_for_host : {} -> Box(Model)
        \\init_for_host = |{}| Box.box(init({}))
        \\
        \\loaded_for_host : Box(Model) -> Box(Model)
        \\loaded_for_host = |boxed| Box.box(loaded(Box.unbox(boxed)))
        \\
        \\step_for_host : Box(Model) -> U64
        \\step_for_host = |boxed| step(Box.unbox(boxed)).n
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);

    try harness.expectAppPathLowersToLir(app_path);
}
