//! Import cycles are user errors: every member of a cycle and every module
//! that imports one is still checked, and the rejected edges depend only on
//! the import graph.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const CycleBuildError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.mem.Allocator.Error ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError;

const SourceFile = struct {
    sub_path: []const u8,
    data: []const u8,
};

const CycleBuild = struct {
    tmp_dir: std.testing.TmpDir,
    build_env: BuildEnv,
    drained: []BuildEnv.DrainedModuleReports,

    fn init(self: *CycleBuild, files: []const SourceFile) CycleBuildError!void {
        const gpa = std.testing.allocator;
        const io = std.testing.io;

        self.tmp_dir = std.testing.tmpDir(.{});
        errdefer self.tmp_dir.cleanup();
        for (files) |file| {
            try self.tmp_dir.dir.writeFile(io, .{ .sub_path = file.sub_path, .data = file.data });
        }

        const cwd = try self.tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
        defer gpa.free(cwd);
        const root_main = try self.tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
        defer gpa.free(root_main);

        self.build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
        errdefer self.build_env.deinit();
        self.build_env.setSyntheticRootPackageIdentity();
        try self.build_env.build(root_main);
        self.drained = try self.build_env.drainReports();
    }

    fn deinit(self: *CycleBuild) void {
        self.build_env.freeDrainedReports(self.drained);
        self.build_env.deinit();
        self.tmp_dir.cleanup();
    }

    /// Count the error reports titled `title` on the module in `file_name`.
    fn errorCount(self: *const CycleBuild, file_name: []const u8, title: []const u8) usize {
        var count: usize = 0;
        for (self.drained) |module| {
            if (!std.mem.eql(u8, std.fs.path.basename(module.abs_path), file_name)) continue;
            for (module.reports) |report| {
                if (report.severity.isError() and std.mem.eql(u8, report.title, title)) count += 1;
            }
        }
        return count;
    }

    /// Count the error reports not titled `excluded_title` on the module in
    /// `file_name`.
    fn otherErrorCount(self: *const CycleBuild, file_name: []const u8, excluded_title: []const u8) usize {
        var count: usize = 0;
        for (self.drained) |module| {
            if (!std.mem.eql(u8, std.fs.path.basename(module.abs_path), file_name)) continue;
            for (module.reports) |report| {
                if (report.severity.isError() and !std.mem.eql(u8, report.title, excluded_title)) count += 1;
            }
        }
        return count;
    }

    fn expectChecked(self: *CycleBuild, file_name: []const u8) (std.Io.Dir.RealPathFileAllocError || error{TestUnexpectedResult})!void {
        const gpa = std.testing.allocator;
        const path = try self.tmp_dir.dir.realPathFileAlloc(std.testing.io, file_name, gpa);
        defer gpa.free(path);
        const mod = self.build_env.findModuleByPath(path) orelse return error.TestUnexpectedResult;
        try std.testing.expect(mod.checkedModule() != null);
    }
};

const cycle_title = "Import Cycle Detected";

test "import cycle: members and the modules importing them are checked" {
    var build: CycleBuild = undefined;
    try build.init(&.{
        .{ .sub_path = "main.roc", .data = "package [Foo] {}\n" },
        .{
            .sub_path = "Foo.roc",
            .data =
            \\import A
            \\
            \\Foo := [].{
            \\    x : Str
            \\    x = A.a
            \\
            \\    y : Str
            \\    y = {}
            \\}
            \\
            ,
        },
        .{
            .sub_path = "A.roc",
            .data =
            \\import B
            \\
            \\A := [].{
            \\    a : Str
            \\    a = B.b
            \\}
            \\
            ,
        },
        .{
            .sub_path = "B.roc",
            .data =
            \\import A
            \\
            \\B := [].{
            \\    b : Str
            \\    b = A.a
            \\}
            \\
            ,
        },
    });
    defer build.deinit();

    try std.testing.expectEqual(@as(usize, 1), build.errorCount("A.roc", cycle_title));
    try std.testing.expectEqual(@as(usize, 1), build.errorCount("B.roc", cycle_title));
    try std.testing.expectEqual(@as(usize, 0), build.errorCount("Foo.roc", cycle_title));
    // `Foo`'s own type error is reported even though it imports the cycle.
    try std.testing.expect(build.otherErrorCount("Foo.roc", cycle_title) > 0);

    try build.expectChecked("Foo.roc");
    try build.expectChecked("A.roc");
    try build.expectChecked("B.roc");
}

test "import cycle: a self-import is rejected and the module is checked" {
    var build: CycleBuild = undefined;
    try build.init(&.{
        .{ .sub_path = "main.roc", .data = "package [A] {}\n" },
        .{
            .sub_path = "A.roc",
            .data =
            \\import A
            \\
            \\A := [].{
            \\    a : Str
            \\    a = "a"
            \\}
            \\
            ,
        },
    });
    defer build.deinit();

    try std.testing.expectEqual(@as(usize, 1), build.errorCount("A.roc", cycle_title));
    try build.expectChecked("A.roc");
}

test "import cycle: every import inside the complete cycle is rejected" {
    // `A` imports `B` and `C`, `B` imports `A`, and `C` imports `B`, so all
    // three modules form one cycle. `B` is parsed before `C`, so the edge
    // `B -> A` closes a smaller cycle before `C -> B` is discovered; the
    // decision must still cover the whole component, including `C`.
    var build: CycleBuild = undefined;
    try build.init(&.{
        .{ .sub_path = "main.roc", .data = "package [A] {}\n" },
        .{
            .sub_path = "A.roc",
            .data =
            \\import B
            \\import C
            \\
            \\A := [].{
            \\    a : Str
            \\    a = Str.concat(B.b, C.c)
            \\}
            \\
            ,
        },
        .{
            .sub_path = "B.roc",
            .data =
            \\import A
            \\
            \\B := [].{
            \\    b : Str
            \\    b = A.a
            \\}
            \\
            ,
        },
        .{
            .sub_path = "C.roc",
            .data =
            \\import B
            \\
            \\C := [].{
            \\    c : Str
            \\    c = B.b
            \\}
            \\
            ,
        },
    });
    defer build.deinit();

    try std.testing.expectEqual(@as(usize, 1), build.errorCount("A.roc", cycle_title));
    try std.testing.expectEqual(@as(usize, 1), build.errorCount("B.roc", cycle_title));
    try std.testing.expectEqual(@as(usize, 1), build.errorCount("C.roc", cycle_title));
    try build.expectChecked("A.roc");
    try build.expectChecked("B.roc");
    try build.expectChecked("C.roc");
}
