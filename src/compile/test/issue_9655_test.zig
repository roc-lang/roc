//! Regression tests for issue #9655: platform-exposed module aliases can carry
//! named tag-union extension parameters through platform relation validation,
//! while anonymous `..` in a type declaration remains a normal user error.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

test "issue 9655: platform exposed callback alias accepts named error extension" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [Model, program] { pf: platform "./platform/platform.roc" }
        \\
        \\import pf.App
        \\
        \\Model : {}
        \\AppErrors : [StartupFailed]
        \\
        \\program = { init!: init! }
        \\
        \\init! : App.Init(Model, AppErrors)
        \\init! = App.init(|_host| Err(StartupFailed))
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/platform.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for program : {
        \\            init! : {
        \\                config : {},
        \\                run! : {} => Try(model, [Exit(I64), ..]),
        \\            },
        \\        }
        \\    }
        \\    exposes [App]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted {}
        \\    targets: {
        \\        inputs: "../targets/",
        \\        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        \\    }
        \\
        \\main_for_host! : () => Try(Box(Model), I64)
        \\main_for_host! = || {
        \\    match (program.init!.run!)({}) {
        \\        Ok(model) => Ok(Box.box(model))
        \\        Err(Exit(code)) => Err(code)
        \\        Err(_) => Err(-1)
        \\    }
        \\}
        \\
        \\import App
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/App.roc",
        .data =
        \\App := [].{
        \\    InitCallback(model, err) : {} => Try(model, [Exit(I64), ..err])
        \\
        \\    Init(model, err) : {
        \\        config : {},
        \\        run! : InitCallback(model, err),
        \\    }
        \\
        \\    init : InitCallback(model, err) -> Init(model, err)
        \\    init = |callback!| { config: {}, run!: callback! }
        \\}
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(!coord.hasUserErrors());

    try coord.validatePlatformAppRelationsForCheck();
    try std.testing.expect(!coord.hasUserErrors());
}

test "issue 9655: platform exposed callback alias with anonymous extension reports instead of panicking" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "app.roc",
        .data =
        \\app [Model, program] { pf: platform "./platform/platform.roc" }
        \\
        \\import pf.App
        \\
        \\Model : {}
        \\
        \\program = { init!: init! }
        \\
        \\init! : App.Init(Model)
        \\init! = App.init(|_host| Err(StartupFailed))
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/platform.roc",
        .data =
        \\platform ""
        \\    requires {
        \\        [Model : model] for program : {
        \\            init! : {
        \\                config : {},
        \\                run! : {} => Try(model, [Exit(I64), ..]),
        \\            },
        \\        }
        \\    }
        \\    exposes [App]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted {}
        \\    targets: {
        \\        inputs: "../targets/",
        \\        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        \\    }
        \\
        \\main_for_host! : () => Try(Box(Model), I64)
        \\main_for_host! = || {
        \\    match (program.init!.run!)({}) {
        \\        Ok(model) => Ok(Box.box(model))
        \\        Err(Exit(code)) => Err(code)
        \\        Err(_) => Err(-1)
        \\    }
        \\}
        \\
        \\import App
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/App.roc",
        .data =
        \\App := [].{
        \\    InitCallback(model) : {} => Try(model, [Exit(I64), ..])
        \\
        \\    Init(model) : {
        \\        config : {},
        \\        run! : InitCallback(model),
        \\    }
        \\
        \\    init : InitCallback(model) -> Init(model)
        \\    init = |callback!| { config: {}, run!: callback! }
        \\}
        ,
    });

    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "app.roc", gpa);
    defer gpa.free(app_path);

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    if (!coord.hasUserErrors()) {
        try coord.validatePlatformAppRelationsForCheck();
    }
    try std.testing.expect(coord.hasUserErrors());

    var found_open_ext_error = false;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "OPEN EXT NOT ALLOWED IN TYPE DECLARATION")) {
            try std.testing.expectEqual(.runtime_error, entry.report.severity);
            found_open_ext_error = true;
        }
    }
    try std.testing.expect(found_open_ext_error);
}
