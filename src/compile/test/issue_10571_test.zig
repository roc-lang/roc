//! Regression test for issue #10571.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

test "issue 10571: repeated failed dispatch on the same receiver reports type mismatches" {
    // Repro for https://github.com/roc-lang/roc/issues/10571.
    // Two `len` calls on the same dispatch-produced receiver, each forced to a
    // type `List.len` cannot have, must both report TYPE MISMATCH. Publishing
    // the checked artifact has to tolerate a dispatch plan whose callable
    // failed to resolve instead of asserting it is a function.
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
        \\main! = |_args| {
        \\    names = "a,b".split_on(",")
        \\    _x = names.len() + 0.I64
        \\    _y = names.len() + 0.I64
        \\    Ok({})
        \\}
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
    var found_type_mismatch = false;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Type Mismatch")) {
            found_type_mismatch = true;
        }
    }
    try std.testing.expect(found_type_mismatch);
}
