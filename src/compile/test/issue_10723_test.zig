//! Regression test for issue #10723.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;
const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 10723: rejected for-loop dispatch lowers to a checked crash with both strategies" {
    const source =
        \\n = || {
        \\    for () in [O(), 0("")] 0()
        \\}
        \\
        \\main! = |_args| {
        \\    n()
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .allow_user_errors = true, .specialization_strategy = .boxy });
}

test "issue 10723: rejected for-loop dispatch reports type errors" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, ".roc_test_platform");
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
        \\
        \\n = || {
        \\    for () in [O(), 0("")] 0()
        \\}
        \\
        \\main! = |_args| Ok({})
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = ".roc_test_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
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

    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
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
        CoreCtx.default(gpa, arena, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    try std.testing.expect(coord.hasUserErrors());
    var found_type_error = false;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Type Mismatch") or
            std.mem.eql(u8, entry.report.title, "Missing Method"))
        {
            found_type_error = true;
        }
    }
    try std.testing.expect(found_type_error);
}
