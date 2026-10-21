//! A compile-time constant whose code reads a scalar through a struct field
//! of a record after every unit the record held has moved. Dev lowering reads
//! `height` where it is used, after the loop has taken `v.images` and
//! `append` has taken `v.drawing`; the record's bytes still hold the scalar,
//! and ARC certification must accept the read.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

test "a scalar read through a struct field after its record's units moved certifies" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, "app/.roc_test_platform");

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
        \\
        \\import Drawings exposing [added]
        \\import pf.Stdout
        \\
        \\main! = |_args| {
        \\    match added {
        \\        Ok(a) => Stdout.line!(a.height.to_str())
        \\        Err(_) => Stdout.line!("bad")
        \\    }
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/Drawings.roc",
        .data =
        \\Drawings := [].{
        \\    added : Try({ drawings : List({ height : U64, parts : List(Str) }), images : List(Str), height : U64 }, [Bad])
        \\    added = add([], ["a"], ["pic"])
        \\}
        \\
        \\make : List(Str) -> Try({ drawing : { height : U64, parts : List(Str) }, images : List(Str) }, [Bad])
        \\make = |c| Ok({ drawing: { height: 3, parts: c }, images: c.append("z") })
        \\
        \\add = |drawings, images, c| {
        \\    v = make(c)?
        \\    height = v.drawing.height
        \\    var $images = images
        \\    for image in v.images {
        \\        $images = $images.append(image)
        \\    }
        \\    Ok({ drawings: drawings.append(v.drawing), images: $images, height })
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/.roc_test_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Stdout]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted {
        \\        "roc_stdout_line": Stdout.line!,
        \\    }
        \\
        \\import Stdout
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/.roc_test_platform/Stdout.roc",
        .data =
        \\Stdout := [].{
        \\    line! : Str => {}
        \\}
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const roc_ctx = CoreCtx.os(gpa, gpa, io);
    var cache_manager = CacheManager.init(gpa, .{ .enabled = false }, roc_ctx);
    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_compatibility_id,
        &cache_manager,
        roc_ctx,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena_impl.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try coord.finishCheckedProgram(.none);
    try std.testing.expect(!coord.hasUserErrors());
}
