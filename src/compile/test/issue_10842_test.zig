//! Regression test for issue #10842.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

test "issue 10842: where-clause method arity that no target satisfies reports type errors" {
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
        \\make_map : (a -> b) -> (I64 -> I64) where [a.decode : I64 -> a, b.encode : b -> I64]
        \\make_map = |f| {
        \\    wrapped : I64 -> I64
        \\    wrapped = |input| {
        \\        A : a
        \\        value : a
        \\        value = A.decode(input)
        \\
        \\        output : b
        \\        output = f(value)
        \\
        \\        output.encode()
        \\    }
        \\
        \\    wrapped
        \\}
        \\
        \\use_it = {
        \\    transform = make_map(|n| n + 1)
        \\    transform(41)
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
    var type_mismatch_count: usize = 0;
    var missing_method_count: usize = 0;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Type Mismatch")) {
            type_mismatch_count += 1;
        } else if (std.mem.eql(u8, entry.report.title, "Missing Method")) {
            missing_method_count += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 2), type_mismatch_count);
    try std.testing.expectEqual(@as(usize, 0), missing_method_count);
}
