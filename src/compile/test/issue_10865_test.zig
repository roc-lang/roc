//! Regression tests for issue #10865.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const reporting = @import("reporting");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const CoreCtx = @import("ctx").CoreCtx;

const Issue10865TestError = std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    Coordinator.AppDiscoveryError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    CoordinatorError ||
    error{TestUnexpectedResult};

fn expectTypeHeaderResult(source: []const u8, expected_title: ?[]const u8) Issue10865TestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, ".roc_test_platform");
    const app_source = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ pf: platform "./.roc_test_platform/main.roc" }}
        \\
        \\{s}
        \\
        \\main! = |_args| Ok({{}})
    , .{source});
    defer gpa.free(app_source);
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data = app_source,
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

    var found_expected = expected_title == null;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        try std.testing.expect(entry.report.severity != reporting.Severity.fatal);
        if (expected_title) |title| {
            if (std.mem.eql(u8, entry.report.title, title)) found_expected = true;
        }
    }

    try std.testing.expect((expected_title != null) == coord.hasUserErrors());
    try std.testing.expect(found_expected);
}

test "issue 10865: a bare underscore in an opaque type header is rejected without a compiler failure" {
    try expectTypeHeaderResult("O(_) :: Str", "Underscore In Opaque Type");
}

test "issue 10865: a bare underscore in a nominal type header is rejected without a compiler failure" {
    try expectTypeHeaderResult("O(_) := Str", "Underscore In Nominal Type");
}

test "issue 10865: applying an alias with a bare underscore header is rejected without a compiler failure" {
    try expectTypeHeaderResult(
        \\O(_) : Str
        \\value : O(Str)
        \\value = ""
    , "Underscore In Type Alias");
}

test "issue 10865: named underscore parameters remain valid phantom parameters" {
    try expectTypeHeaderResult(
        \\Opaque(_a) :: Str
        \\Nominal(_b) := Str
    , null);
}
