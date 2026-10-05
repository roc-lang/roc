//! Shared driver for import-cycle regressions: checks an app against a
//! minimal platform and finalizes it the way `roc check` does.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

/// Everything checking an import-cycle fixture can fail with.
pub const ImportCycleTestError = std.mem.Allocator.Error ||
    std.Thread.SpawnError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    eval.BuiltinModules.InitError ||
    Coordinator.AppDiscoveryError ||
    @import("../coordinator.zig").CoordinatorError ||
    error{TestUnexpectedResult};

/// One source file written relative to the test's temporary directory.
pub const SourceFile = struct {
    sub_path: []const u8,
    data: []const u8,
};

/// Check the app at `main.roc` among `files`, finalize it with executable
/// artifacts, and require an import-cycle report.
pub fn expectImportCycleReport(files: []const SourceFile) ImportCycleTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, ".roc_test_platform");
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
    for (files) |file| {
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.sub_path, .data = file.data });
    }

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
    try coord.finishCheckedProgram(.executable_artifacts);

    try std.testing.expect(coord.hasUserErrors());
    var found_cycle_report = false;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (std.mem.eql(u8, entry.report.title, "Import Cycle Detected")) found_cycle_report = true;
    }
    try std.testing.expect(found_cycle_report);
}
