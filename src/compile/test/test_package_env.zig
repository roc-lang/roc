//! Test file for package environment functionality (currently commented out/disabled).

// const std = @import("std");
// const cache = @import("cache");
// const reporting = @import("reporting");

// const Report = reporting.Report;
// const PackageEnv = @import("../compile_package.zig").PackageEnv;
// const ReportSink = @import("../compile_package.zig").ReportSink;
// const ScheduleHook = @import("../compile_package.zig").ScheduleHook;

// test "PackageEnv: parallel success across modules" {
//     const gpa = std.testing.allocator;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     // Layout:
//     // root_dir/Main.roc imports A and B; A imports C. All succeed.
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try write(tmp.dir, "Main.roc", "import A\n" ++ "import B\n\n" ++ "main = A.val + B.one\n");

//     try write(tmp.dir, "A.roc", "import C\n" ++ "val = C.ten\n");

//     try write(tmp.dir, "B.roc", "one = 1\n");

//     try write(tmp.dir, "C.roc", "ten = 10\n");

//     var sink = TestSink.init(gpa);
//     defer sink.deinit();

//     // Use empty package name for root modules (app/platform/package being checked)
//     var sched = PackageEnv.init(gpa, "", root_dir, .multi_threaded, 4, sink.sink(), ScheduleHook.noOp(), "test");
//     defer sched.deinit();

//     const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
//     defer gpa.free(main_path);

//     try sched.buildRoot(main_path);

//     // No reports expected
//     try std.testing.expectEqual(@as(usize, 0), sink.reports.items.len);
// }

// test "PackageEnv: deterministic error ordering by depth then name" {
//     const gpa = std.testing.allocator;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Main imports X and A; X imports Y.
//     // Introduce errors in A and Y; Y should come before A by depth (Y depth=2, A depth=1) -> actually A should come first (depth 1), then Y (depth 2).

//     try write(tmp.dir, "Main.roc", "import X\n" ++ "import A\n\n" ++ "main = X.other + A.bad\n");

//     try write(tmp.dir, "X.roc", "import Y\n" ++ "other = Y.val\n");

//     // A has a type mismatch: use string where number expected
//     try write(tmp.dir, "A.roc", "bad = \"oops\"\n" ++ "bad2 = bad + 1\n");

//     // Y has invalid if condition to produce error
//     try write(tmp.dir, "Y.roc", "val = if 42 then 1 else 2\n");

//     var sink = TestSink.init(gpa);
//     defer sink.deinit();

//     // Use empty package name for root modules (app/platform/package being checked)
//     var sched = PackageEnv.init(gpa, "", root_dir, .multi_threaded, 4, sink.sink(), ScheduleHook.noOp(), "test");
//     defer sched.deinit();

//     const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
//     defer gpa.free(main_path);

//     try sched.buildRoot(main_path);

//     // Expect reports from A (depth 1) then Y (depth 2) in module-name alphabetical within same depth
//     try std.testing.expect(sink.reports.items.len >= 2);
//     try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "A"));
//     // Y must appear later
//     var found_y = false;
//     for (sink.modules.items[1..]) |m| {
//         if (std.mem.eql(u8, m, "Y")) {
//             found_y = true;
//             break;
//         }
//     }
//     try std.testing.expect(found_y);
// }

// // Simple sink collecting reports and module names in order of emission
// test "PackageEnv: single-threaded success across modules" {
//     const gpa = std.testing.allocator;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try write(tmp.dir, "Main.roc", "import A\n" ++ "import B\n\n" ++ "main = A.v + B.w\n");
//     try write(tmp.dir, "A.roc", "v = 2\n");
//     try write(tmp.dir, "B.roc", "w = 3\n");

//     var sink = TestSink.init(gpa);
//     defer sink.deinit();

//     // Use empty package name for root modules (app/platform/package being checked)
//     var sched = PackageEnv.init(gpa, "", root_dir, .single_threaded, 1, sink.sink(), ScheduleHook.noOp(), "test");
//     defer sched.deinit();

//     const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
//     defer gpa.free(main_path);

//     try sched.buildRoot(main_path);

//     // No reports expected
//     try std.testing.expectEqual(@as(usize, 0), sink.reports.items.len);
// }

// test "PackageEnv: same-depth alphabetical order" {
//     const gpa = std.testing.allocator;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Main imports B and A; both have errors; order should be A then B (alphabetical) since same depth.
//     try write(tmp.dir, "Main.roc", "import B\n" ++ "import A\n\n" ++ "main = A.bad + B.bad\n");

//     // Both A and B produce type errors
//     try write(tmp.dir, "A.roc", "bad = \"str\" + 1\n");
//     try write(tmp.dir, "B.roc", "bad = if 0 then 1 else 2\n");

//     var sink = TestSink.init(gpa);
//     defer sink.deinit();

//     // Use empty package name for root modules (app/platform/package being checked)
//     var sched = PackageEnv.init(gpa, "", root_dir, .multi_threaded, 4, sink.sink(), ScheduleHook.noOp(), "test");
//     defer sched.deinit();

//     const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
//     defer gpa.free(main_path);

//     try sched.buildRoot(main_path);

//     // Expect at least one report from each, and A before B
//     try std.testing.expect(sink.reports.items.len >= 2);
//     try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "A"));

//     var saw_b = false;
//     for (sink.modules.items[1..]) |m| {
//         if (std.mem.eql(u8, m, "B")) {
//             saw_b = true;
//             break;
//         }
//     }
//     try std.testing.expect(saw_b);
// }

// test "PackageEnv: detect import cycle and fail fast" {
//     const gpa = std.testing.allocator;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Main imports A; A imports Main -> cycle
//     try write(tmp.dir, "Main.roc", "import A\n" ++ "main = A.x\n");
//     try write(tmp.dir, "A.roc", "import Main\n" ++ "x = 1\n");

//     var sink = TestSink.init(gpa);
//     defer sink.deinit();

//     // Use multi-threaded to ensure we detect cycles under concurrency
//     // Use empty package name for root modules (app/platform/package being checked)
//     var sched = PackageEnv.init(gpa, "", root_dir, .multi_threaded, 4, sink.sink(), ScheduleHook.noOp(), "test");
//     defer sched.deinit();

//     const main_path = try std.fs.path.join(gpa, &.{ root_dir, "Main.roc" });
//     defer gpa.free(main_path);

//     try sched.buildRoot(main_path);

//     // We expect cycle reports from both modules, and emission begins with Main (depth 0) then A (depth 1)
//     try std.testing.expect(sink.reports.items.len >= 2);
//     try std.testing.expect(std.mem.eql(u8, sink.modules.items[0], "Main"));

//     var saw_a = false;
//     for (sink.modules.items[1..]) |m| {
//         if (std.mem.eql(u8, m, "A")) {
//             saw_a = true;
//             break;
//         }
//     }
//     try std.testing.expect(saw_a);
// }

// const TestSink = struct {
//     gpa: std.mem.Allocator,
//     reports: std.ArrayList(Report),
//     modules: std.ArrayList([]const u8),

//     fn init(gpa: std.mem.Allocator) TestSink {
//         return .{ .gpa = gpa, .reports = std.ArrayList(Report).init(gpa), .modules = std.ArrayList([]const u8).init(gpa) };
//     }

//     fn deinit(self: *TestSink) void {
//         // Free reports
//         for (self.reports.items) |*r| r.deinit();
//         self.reports.deinit();
//         // Free duplicated module name strings
//         for (self.modules.items) |m| self.gpa.free(m);
//         self.modules.deinit();
//     }

//     fn sink(self: *TestSink) ReportSink {
//         return .{ .ctx = self, .emitFn = emitCb };
//     }

//     fn emitCb(ctx: ?*anyopaque, module_name: []const u8, report: Report) void {
//         var self: *TestSink = @ptrCast(@alignCast(ctx.?));
//         // Record; take ownership of report
//         _ = self.reports.append(report) catch return;
//         const owned = self.gpa.dupe(u8, module_name) catch return;
//         _ = self.modules.append(owned) catch return;
//     }
// };

// fn write(dir: std.fs.Dir, rel: []const u8, contents: []const u8) !void {
//     var f = try dir.createFile(rel, .{ .read = true, .truncate = true, .exclusive = false });
//     defer f.close();
//     try f.writeAll(contents);
// }
