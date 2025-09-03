//! Test file for build environment functionality (currently commented out/disabled).

// const std = @import("std");
// const cache = @import("cache");
// const reporting = @import("reporting");

// const Report = reporting.Report;
// const BuildEnv = @import("../compile_build.zig").BuildEnv;
// const OrderedSink = @import("../compile_build.zig").OrderedSink;
// const PackageEnv = @import("../compile_package.zig").PackageEnv;

// fn writeFile(dir: std.fs.Dir, rel: []const u8, contents: []const u8) !void {
//     var f = try dir.createFile(rel, .{ .read = true, .truncate = true, .exclusive = false });
//     defer f.close();
//     try f.writeAll(contents);
// }

// fn cleanupReports(gpa: std.mem.Allocator, drained: []BuildEnv.DrainedModuleReports) void {
//     var i: usize = 0;
//     while (i < drained.len) : (i += 1) {
//         for (drained[i].reports) |*rep| rep.deinit();
//         gpa.free(drained[i].reports);
//         gpa.free(drained[i].abs_path);
//     }
//     gpa.free(drained);
// }

// fn expectNoErrors(ws: *BuildEnv) !void {
//     const gpa = std.testing.allocator;
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);
//     try std.testing.expectEqual(@as(usize, 0), drained.len);
// }

// fn expectErrorCount(ws: *BuildEnv, expected_count: usize) !void {
//     const gpa = std.testing.allocator;
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);

//     var total_reports: usize = 0;
//     for (drained) |entry| {
//         total_reports += entry.reports.len;
//     }
//     try std.testing.expectEqual(expected_count, total_reports);
// }

// fn expectSpecificError(ws: *BuildEnv, comptime error_severity: reporting.Severity, module_substring: []const u8) !void {
//     const gpa = std.testing.allocator;
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);

//     var found = false;
//     for (drained) |entry| {
//         if (std.mem.indexOf(u8, entry.abs_path, module_substring) != null) {
//             for (entry.reports) |report| {
//                 if (report.severity == error_severity) {
//                     found = true;
//                     break;
//                 }
//             }
//         }
//     }
//     try std.testing.expect(found);
// }

// fn testAppWithShorthandsHelper(comptime mode: compile.package.Mode, thread_count: u32) !void {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try tmp.dir.makePath("platform");
//     try tmp.dir.makePath("foo");
//     try tmp.dir.makePath("app");

//     // Platform header and module
//     try writeFile(tmp.dir, "platform/PlatformMain.roc",
//         \\platform "Basic"
//         \\requires {} {}
//         \\exposes []
//         \\packages {}
//         \\provides []
//     );
//     try writeFile(tmp.dir, "platform/Stdout.roc",
//         \\module [Stdout]
//         \\one = 1
//     );

//     // Foo package header and module
//     try writeFile(tmp.dir, "foo/FooMain.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "foo/Util.roc",
//         \\module [Util]
//         \\two = 2
//     );

//     // App Main
//     const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "PlatformMain.roc" });
//     defer gpa.free(plat_path);
//     const foo_path = try std.fs.path.join(gpa, &.{ root_dir, "foo", "FooMain.roc" });
//     defer gpa.free(foo_path);

//     const app_main = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ cli: platform "{s}", foo: "{s}" }}
//         \\
//         \\import cli.Stdout
//         \\import foo.Util
//         \\
//         \\main! = |_| Stdout.one + Util.two
//     , .{ plat_path, foo_path });
//     defer gpa.free(app_main);

//     try writeFile(tmp.dir, "app/Main.roc", app_main);

//     var ws = BuildEnv.init(gpa, mode, thread_count);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     try ws.buildApp(app_path);

//     // Expect no reports
//     try expectNoErrors(&ws);
// }

// test "BuildEnv: app with platform and package shorthands (local-only) succeeds - multi-threaded" {
//     try testAppWithShorthandsHelper(.multi_threaded, 4);
// }

// test "BuildEnv: app with platform and package shorthands (local-only) succeeds - single-threaded" {
//     try testAppWithShorthandsHelper(.single_threaded, 1);
// }

// // OrderedSink should gate emission to a contiguous ready prefix and drain in order.
// test "OrderedSink: prefix gating and drain order" {
//     const gpa = std.testing.allocator;
//     var sink = OrderedSink.init(gpa);
//     defer sink.deinit();

//     const pkg_names = [_][]const u8{ "pkg", "pkg" };
//     const module_names = [_][]const u8{ "First", "Second" };
//     const depths = [_]u32{ 1, 1 };
//     try sink.buildOrder(&pkg_names, &module_names, &depths);

//     // Emit Second first; should not emit anything yet due to prefix gating (First not ready).
//     const r2 = Report.init(gpa, "Second report", .runtime_error);
//     sink.emitReport("pkg", "Second", r2);

//     const drained0 = try sink.drainEmitted(gpa);
//     defer gpa.free(drained0);
//     try std.testing.expectEqual(@as(usize, 0), drained0.len);

//     // Emit First; now both First and Second should emit (First unlocks the prefix, then Second).
//     const r1 = Report.init(gpa, "First report", .runtime_error);
//     sink.emitReport("pkg", "First", r1);

//     const drained1 = try sink.drainEmitted(gpa);
//     defer {
//         var i: usize = 0;
//         while (i < drained1.len) : (i += 1) {
//             for (drained1[i].reports) |*rep| rep.deinit();
//             gpa.free(drained1[i].reports);
//         }
//         gpa.free(drained1);
//     }
//     try std.testing.expectEqual(@as(usize, 2), drained1.len);
//     try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "First"));
//     try std.testing.expectEqual(@as(usize, 1), drained1[0].reports.len);
//     try std.testing.expect(std.mem.eql(u8, drained1[1].pkg_name, "pkg") and std.mem.eql(u8, drained1[1].module_name, "Second"));
//     try std.testing.expectEqual(@as(usize, 1), drained1[1].reports.len);
// }

// // BuildEnv.drainReports should return absolute paths and group multiple reports per module.
// test "BuildEnv: drainReports returns abs paths and aggregates multi-report module" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Two errors in a single module A.roc to exercise multi-report aggregation
//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [A]
//         \\bad = "oops" + 1
//         \\bad2 = if 42 then 1 else 2
//     );

//     // App Main imports A to force build
//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\main! = |_| A.bad + 1
//     );

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build the app and then drain reports
//     _ = ws.buildApp(app_path) catch {};
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);

//     // Find the A.roc module which should have exactly 2 type errors
//     var found_a = false;
//     var total_errors_in_a: usize = 0;

//     for (drained) |entry| {
//         if (std.mem.endsWith(u8, entry.abs_path, "A.roc")) {
//             found_a = true;
//             try std.testing.expect(std.fs.path.isAbsolute(entry.abs_path));

//             // Count specific error types
//             for (entry.reports) |report| {
//                 if (report.severity == .runtime_error) {
//                     total_errors_in_a += 1;
//                 }
//             }
//         }
//     }

//     try std.testing.expect(found_a);
//     try std.testing.expectEqual(@as(usize, 2), total_errors_in_a);
// }

// test "OrderedSink: early emit before order still drains after order build" {
//     const gpa = std.testing.allocator;
//     var sink = OrderedSink.init(gpa);
//     defer sink.deinit();

//     // Emit before any order is built; should not drain yet
//     const early = Report.init(gpa, "Early", .runtime_error);
//     sink.emitReport("pkg", "M", early);

//     const drained0 = try sink.drainEmitted(gpa);
//     defer gpa.free(drained0);
//     try std.testing.expectEqual(@as(usize, 0), drained0.len);

//     // Build order with the module
//     const pkg_names = [_][]const u8{"pkg"};
//     const module_names = [_][]const u8{"M"};
//     const depths = [_]u32{1};
//     try sink.buildOrder(&pkg_names, &module_names, &depths);

//     // Emit again to trigger tryEmitLocked; now drain should return the module
//     const later = Report.init(gpa, "Later", .runtime_error);
//     sink.emitReport("pkg", "M", later);

//     const drained1 = try sink.drainEmitted(gpa);
//     defer {
//         if (drained1.len > 0) {
//             var i: usize = 0;
//             while (i < drained1[0].reports.len) : (i += 1) {
//                 var rep = drained1[0].reports[i];
//                 rep.deinit();
//             }
//             gpa.free(drained1[0].reports);
//         }
//         gpa.free(drained1);
//     }
//     try std.testing.expectEqual(@as(usize, 1), drained1.len);
//     try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "M"));
//     try std.testing.expectEqual(@as(usize, 2), drained1[0].reports.len);
// }

// test "OrderedSink: case-insensitive sort of fq names at same depth" {
//     const gpa = std.testing.allocator;
//     var sink = OrderedSink.init(gpa);
//     defer sink.deinit();

//     const pkg_names = [_][]const u8{ "pkg", "pkg" };
//     const module_names = [_][]const u8{ "B", "a" };
//     const depths = [_]u32{ 0, 0 };
//     try sink.buildOrder(&pkg_names, &module_names, &depths);

//     const r_b = Report.init(gpa, "B", .runtime_error);
//     sink.emitReport("pkg", "B", r_b);
//     const r_a = Report.init(gpa, "a", .runtime_error);
//     sink.emitReport("pkg", "a", r_a);

//     const drained = try sink.drainEmitted(gpa);
//     defer {
//         var i: usize = 0;
//         while (i < drained.len) : (i += 1) {
//             var j: usize = 0;
//             while (j < drained[i].reports.len) : (j += 1) {
//                 var rep = drained[i].reports[j];
//                 rep.deinit();
//             }
//             gpa.free(drained[i].reports);
//         }
//         gpa.free(drained);
//     }

//     try std.testing.expectEqual(@as(usize, 2), drained.len);
//     // Case-insensitive compare should place "a" before "B"
//     try std.testing.expect(std.mem.eql(u8, drained[0].pkg_name, "pkg") and std.mem.eql(u8, drained[0].module_name, "a"));
//     try std.testing.expect(std.mem.eql(u8, drained[1].pkg_name, "pkg") and std.mem.eql(u8, drained[1].module_name, "B"));
// }

// test "BuildEnv: same-depth alphabetical ordering across packages via drainReports" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try tmp.dir.makePath("pkgA");
//     try tmp.dir.makePath("pkgB");
//     try tmp.dir.makePath("app");

//     // Package A (error in A/Bad.roc)
//     try writeFile(tmp.dir, "pkgA/AHeader.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "pkgA/Bad.roc",
//         \\module [Bad]
//         \\bad = "oops" + 1
//     );

//     // Package B (error in B/Bad.roc)
//     try writeFile(tmp.dir, "pkgB/BHeader.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "pkgB/Bad.roc",
//         \\module [Bad]
//         \\bad = if 42 then 1 else 2
//     );

//     const a_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgA", "AHeader.roc" });
//     defer gpa.free(a_path);
//     const b_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgB", "BHeader.roc" });
//     defer gpa.free(b_path);

//     // App imports both A.Bad and B.Bad so both are depth=1 from app
//     const app_main = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ A: "{s}", B: "{s}" }}
//         \\
//         \\import A.Bad
//         \\import B.Bad
//         \\
//         \\main! = |_| Bad.bad
//     , .{ a_path, b_path });
//     defer gpa.free(app_main);
//     try writeFile(tmp.dir, "app/Main.roc", app_main);

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     _ = ws.buildApp(app_path) catch {};

//     const drained = try ws.drainReports();
//     defer {
//         var i: usize = 0;
//         while (i < drained.len) : (i += 1) {
//             var j: usize = 0;
//             while (j < drained[i].reports.len) : (j += 1) {
//                 var rep = drained[i].reports[j];
//                 rep.deinit();
//             }
//             gpa.free(drained[i].reports);
//             gpa.free(drained[i].abs_path);
//         }
//         gpa.free(drained);
//     }

//     // Find indices of A.Bad and B.Bad and ensure A comes before B at same depth
//     var idx_a: ?usize = null;
//     var idx_b: ?usize = null;
//     var i: usize = 0;
//     while (i < drained.len) : (i += 1) {
//         const path = drained[i].abs_path;
//         if (std.mem.indexOf(u8, path, "A.") != null) idx_a = i;
//         if (std.mem.indexOf(u8, path, "B.") != null) idx_b = i;
//     }

//     try std.testing.expect(idx_a != null);
//     try std.testing.expect(idx_b != null);
//     try std.testing.expect(idx_a.? < idx_b.?);
// }

// test "BuildEnv: multi-threaded global queue end-to-end builds and drains" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create a trivial module with no errors and import it from Main
//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [A]
//         \\val = 1
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\main! = |_| A.val
//     );

//     // Build in multi-threaded mode to exercise the global queue wiring
//     var ws = BuildEnv.init(gpa, .multi_threaded, 4);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build should complete without deadlocks; drain should be callable (expect no reports)
//     try ws.buildApp(app_path);
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);
//     try std.testing.expectEqual(@as(usize, 0), drained.len);
// }

// test "BuildEnv: multi-threaded global queue drives all phases" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create a multi-module app with dependencies to test phase progression
//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import Helper
//         \\import Utils
//         \\
//         \\main! = |_| Helper.value + Utils.compute 10
//     );

//     try writeFile(tmp.dir, "app/Helper.roc",
//         \\module [value]
//         \\
//         \\import Utils
//         \\
//         \\value = Utils.compute 5
//     );

//     try writeFile(tmp.dir, "app/Utils.roc",
//         \\module [compute]
//         \\
//         \\compute = |n| n * 2
//     );

//     // Build in multi-threaded mode to exercise the global queue phase progression
//     var ws = BuildEnv.init(gpa, .multi_threaded, 4);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build should complete with all modules progressing through all phases
//     try ws.buildApp(app_path);

//     // Verify all modules reached Done phase
//     const main_sched = ws.schedulers.get("app").?;

//     main_sched.lock.lock();
//     defer main_sched.lock.unlock();

//     const main_id = main_sched.module_names.get("Main").?;
//     const helper_id = main_sched.module_names.get("Helper").?;
//     const utils_id = main_sched.module_names.get("Utils").?;

//     const main_state = main_sched.modules.items[main_id];
//     const helper_state = main_sched.modules.items[helper_id];
//     const utils_state = main_sched.modules.items[utils_id];

//     const Phase = PackageEnv.Phase;
//     try std.testing.expectEqual(Phase.Done, main_state.phase);
//     try std.testing.expectEqual(Phase.Done, helper_state.phase);
//     try std.testing.expectEqual(Phase.Done, utils_state.phase);

//     // Drain reports (expect no errors)
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);
//     try std.testing.expectEqual(@as(usize, 0), drained.len);
// }

// test "BuildEnv: multi-threaded concurrency stress test" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create many interconnected modules to stress concurrent processing
//     const module_count = 10;
//     var i: usize = 0;

//     // Create modules that all import each other (except Main)
//     while (i < module_count) : (i += 1) {
//         const mod_name = try std.fmt.allocPrint(gpa, "Mod{}", .{i});
//         defer gpa.free(mod_name);

//         var imports = std.ArrayList(u8).init(gpa);
//         defer imports.deinit();

//         // Each module imports the next two modules (circular style)
//         var j: usize = 1;
//         while (j <= 2) : (j += 1) {
//             const import_idx = (i + j) % module_count;
//             try imports.appendSlice("import Mod");
//             try std.fmt.formatInt(import_idx, 10, .lower, .{}, imports.writer());
//             try imports.append('\n');
//         }

//         const content = try std.fmt.allocPrint(gpa,
//             \\module [value{0}]
//             \\
//             \\{1s}
//             \\value{0} = {0}
//         , .{ i, imports.items });
//         defer gpa.free(content);

//         const filename = try std.fmt.allocPrint(gpa, "app/Mod{}.roc", .{i});
//         defer gpa.free(filename);

//         try writeFile(tmp.dir, filename, content);
//     }

//     // Create Main that imports several modules
//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import Mod0
//         \\import Mod1
//         \\import Mod2
//         \\import Mod3
//         \\import Mod4
//         \\
//         \\main! = |_| Mod0.value0 + Mod1.value1 + Mod2.value2 + Mod3.value3 + Mod4.value4
//     );

//     // Build with many threads to maximize concurrency
//     var ws = BuildEnv.init(gpa, .multi_threaded, 8);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build should complete without deadlocks or race conditions
//     try ws.buildApp(app_path);

//     // Verify no duplicate processing occurred by checking phase progression
//     const main_sched = ws.schedulers.get("app").?;

//     main_sched.lock.lock();
//     defer main_sched.lock.unlock();

//     // All modules should reach Done phase exactly once
//     for (main_sched.modules.items) |state| {
//         try std.testing.expectEqual(PackageEnv.Phase.Done, state.phase);
//         const working_val = if (@import("builtin").target.cpu.arch != .wasm32) state.working.load(.seq_cst) else state.working;
//         try std.testing.expectEqual(@as(u8, 0), working_val);
//     }

//     // Drain reports (may have cycle errors due to circular imports)
//     const drained = try ws.drainReports();
//     defer {
//         var idx: usize = 0;
//         while (idx < drained.len) : (idx += 1) {
//             for (drained[idx].reports) |*rep| rep.deinit();
//             gpa.free(drained[idx].reports);
//             gpa.free(drained[idx].abs_path);
//         }
//         gpa.free(drained);
//     }
// }

// test "BuildEnv: streaming with module chain" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create a chain of modules A -> B -> C -> D
//     try writeFile(tmp.dir, "app/D.roc",
//         \\module [valueD]
//         \\
//         \\valueD = 4
//     );

//     try writeFile(tmp.dir, "app/C.roc",
//         \\module [valueC]
//         \\
//         \\import D
//         \\
//         \\valueC = D.valueD + 3
//     );

//     try writeFile(tmp.dir, "app/B.roc",
//         \\module [valueB]
//         \\
//         \\import C
//         \\
//         \\valueB = C.valueC + 2
//     );

//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [valueA]
//         \\
//         \\import B
//         \\
//         \\valueA = B.valueB + 1
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\
//         \\main! = |_| A.valueA
//     );

//     // Use single-threaded mode for deterministic testing
//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build the app normally
//     try ws.buildApp(app_path);

//     // Should have successfully built all modules
//     // Verify by checking that we can drain reports
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);

//     // Expect no errors from this simple module chain
//     try std.testing.expectEqual(@as(usize, 0), drained.len);
// }

// test "BuildEnv: enforce rules (only apps -> platforms, no package -> app)" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try tmp.dir.makePath("platform");
//     try tmp.dir.makePath("pkg");
//     try tmp.dir.makePath("app");

//     // Platform header
//     try writeFile(tmp.dir, "platform/PlatformMain.roc",
//         \\platform "P"
//         \\requires {} {}
//         \\exposes []
//         \\packages {}
//         \\provides []
//     );

//     // Package header that incorrectly depends on platform
//     const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "PlatformMain.roc" });
//     defer gpa.free(plat_path);

//     const pkg_header = try std.fmt.allocPrint(gpa,
//         \\package []
//         \\{{ cli: "{s}" }}
//     , .{plat_path});
//     defer gpa.free(pkg_header);
//     try writeFile(tmp.dir, "pkg/PkgMain.roc", pkg_header);

//     // App header depends on pkg (which depends on platform) - allowed for app but pkg->platform should be rejected
//     const pkg_path = try std.fs.path.join(gpa, &.{ root_dir, "pkg", "PkgMain.roc" });
//     defer gpa.free(pkg_path);

//     const app_header = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ pkg: "{s}" }}
//         \\
//         \\main! = |_| 42
//     , .{pkg_path});
//     defer gpa.free(app_header);
//     try writeFile(tmp.dir, "app/Main.roc", app_header);

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Building should fail the dependency validation (invalid dependency)
//     const res = ws.buildApp(app_path);
//     try std.testing.expectError(error.InvalidDependency, res);
//     try expectSpecificError(&ws, .runtime_error, "PkgMain.roc");
// }

// test "BuildEnv: app header can reference absolute paths; package/platform sandboxed" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Create an external directory outside the workspace root
//     var external_dir = try std.fs.cwd().makeOpenPath("external_test_dir", .{});
//     defer external_dir.close();
//     const external_platform = try std.fs.path.join(gpa, &.{ "external_test_dir", "Plat.roc" });
//     defer gpa.free(external_platform);

//     // Write platform file outside workspace
//     try std.fs.cwd().writeFile(.{ .sub_path = external_platform, .data = "platform \"X\"\\nrequires {} {}\\nexposes []\\npackages {}\\nprovides []\\n" });

//     // App can reference absolute external path for platform (should be allowed by policy)
//     const app_header = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ cli: platform "{s}" }}
//         \\
//         \\main! = |_| 0
//     , .{external_platform});
//     defer gpa.free(app_header);

//     try tmp.dir.makePath("app");
//     try writeFile(tmp.dir, "app/Main.roc", app_header);

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Because we sandbox platform dependencies even when declared by app, this should error PathOutsideWorkspace
//     const res = ws.buildApp(app_path);
//     try std.testing.expectError(error.PathOutsideWorkspace, res);
// }

// test "BuildEnv: package header cannot reference paths outside workspace" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Exterior dir and file
//     var ext_dir = try std.fs.cwd().makeOpenPath("pkg_external_dir", .{});
//     defer ext_dir.close();
//     defer std.fs.cwd().deleteTree("pkg_external_dir") catch {};
//     const ext_pkg = try std.fs.path.join(gpa, &.{ "pkg_external_dir", "Pkg.roc" });
//     defer gpa.free(ext_pkg);
//     try std.fs.cwd().writeFile(.{ .sub_path = ext_pkg, .data = "package []\\n{ }\\n" });

//     try tmp.dir.makePath("app");
//     // App points to a local platform so sandbox roots are the app dir
//     try tmp.dir.makePath("platform");
//     try writeFile(tmp.dir, "platform/P.roc", "platform \"P\"\\nrequires {} {}\\nexposes []\\npackages {}\\nprovides []\\n");

//     // Package header attempts to import external path
//     try tmp.dir.makePath("pkg");
//     const pkg_header = try std.fmt.allocPrint(gpa,
//         \\package []
//         \\{{ ext: "{s}" }}
//     , .{ext_pkg});
//     defer gpa.free(pkg_header);
//     try writeFile(tmp.dir, "pkg/Main.roc", pkg_header);

//     // App imports pkg (which points outside) - should trigger PathOutsideWorkspace
//     const pkg_path = try std.fs.path.join(gpa, &.{ root_dir, "pkg", "Main.roc" });
//     defer gpa.free(pkg_path);
//     const plat_path = try std.fs.path.join(gpa, &.{ root_dir, "platform", "P.roc" });
//     defer gpa.free(plat_path);

//     const app_header = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ pf: platform "{s}", pkg: "{s}" }}
//         \\
//         \\import pkg.Foo
//         \\
//         \\main! = |_| 0
//     , .{ plat_path, pkg_path });
//     defer gpa.free(app_header);
//     try writeFile(tmp.dir, "app/Main.roc", app_header);

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     const res = ws.buildApp(app_path);
//     try std.testing.expectError(error.PathOutsideWorkspace, res);
// }

// test "BuildEnv: deterministic error ordering across packages" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     try tmp.dir.makePath("pkgA");
//     try tmp.dir.makePath("pkgB");
//     try tmp.dir.makePath("app");

//     // Package A header and an error in module A/Bad.roc (depth 1)
//     try writeFile(tmp.dir, "pkgA/AHeader.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "pkgA/Bad.roc",
//         \\module [Bad]
//         \\bad = "oops" + 1
//     );

//     // Package B header and an error in module B/Deep/Bad.roc (depth 2 via app -> B -> Deep.Bad)
//     try writeFile(tmp.dir, "pkgB/BHeader.roc",
//         \\package []
//         \\{ }
//     );
//     try tmp.dir.makePath("pkgB/Deep");
//     try writeFile(tmp.dir, "pkgB/Deep/Bad.roc",
//         \\module [Deep.Bad]
//         \\bad = if 42 then 1 else 2
//     );

//     // App header depends on A and B, and imports their modules to force build.
//     const a_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgA", "AHeader.roc" });
//     defer gpa.free(a_path);
//     const b_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgB", "BHeader.roc" });
//     defer gpa.free(b_path);

//     const app_main = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ A: "{s}", B: "{s}" }}
//         \\
//         \\import A.Bad
//         \\import B.Deep.Bad
//         \\
//         \\main! = |_| Bad.bad + Deep.Bad.bad
//     , .{ a_path, b_path });
//     defer gpa.free(app_main);
//     try writeFile(tmp.dir, "app/Main.roc", app_main);

//     var ws = BuildEnv.init(gpa, .multi_threaded, 4);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Build workspace; expect errors from A.Bad (depth 1) and then B.Deep.Bad (depth 2)
//     _ = ws.buildApp(app_path) catch {};

//     // Drain reports to check ordering
//     const drained = try ws.drainReports();
//     defer cleanupReports(gpa, drained);

//     // We should have exactly 2 modules with errors
//     try std.testing.expectEqual(@as(usize, 2), drained.len);

//     // First error should be from A/Bad.roc (depth 1)
//     try std.testing.expect(std.mem.endsWith(u8, drained[0].abs_path, "Bad.roc"));
//     try std.testing.expect(std.mem.indexOf(u8, drained[0].abs_path, "pkgA") != null);

//     // Second error should be from B/Deep/Bad.roc (depth 2)
//     try std.testing.expect(std.mem.endsWith(u8, drained[1].abs_path, "Bad.roc"));
//     try std.testing.expect(std.mem.indexOf(u8, drained[1].abs_path, "pkgB") != null);
//     try std.testing.expect(std.mem.indexOf(u8, drained[1].abs_path, "Deep") != null);

//     // Verify each has a type error
//     for (drained) |entry| {
//         try std.testing.expect(entry.reports.len > 0);
//         try std.testing.expectEqual(reporting.Severity.runtime_error, entry.reports[0].severity);
//     }
// }

// test "BuildEnv: package shorthands do not leak between packages" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();
//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);

//     // Create directory structure
//     try tmp.dir.makePath("app");
//     try tmp.dir.makePath("pkgA");
//     try tmp.dir.makePath("pkgB");
//     try tmp.dir.makePath("pkgC");
//     try tmp.dir.makePath("pkgD");

//     // Package B - will be referred to as "foo" by pkgA
//     try writeFile(tmp.dir, "pkgB/PkgB.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "pkgB/ModuleB.roc",
//         \\module [valueB]
//         \\valueB = "from package B"
//     );

//     // Package C - will be referred to as "foo" by pkgD (same shorthand, different package!)
//     try writeFile(tmp.dir, "pkgC/PkgC.roc",
//         \\package []
//         \\{ }
//     );
//     try writeFile(tmp.dir, "pkgC/ModuleC.roc",
//         \\module [valueC]
//         \\valueC = "from package C"
//     );

//     // Package A uses shorthand "foo" for package B
//     const b_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgB", "PkgB.roc" });
//     defer gpa.free(b_path);
//     const pkgA_header = try std.fmt.allocPrint(gpa,
//         \\package []
//         \\{{ foo: "{s}" }}
//     , .{b_path});
//     defer gpa.free(pkgA_header);
//     try writeFile(tmp.dir, "pkgA/PkgA.roc", pkgA_header);
//     try writeFile(tmp.dir, "pkgA/ModuleA.roc",
//         \\module [getFromB]
//         \\import foo.ModuleB
//         \\getFromB = ModuleB.valueB
//     );

//     // Package D uses shorthand "foo" for package C (different package!)
//     const c_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgC", "PkgC.roc" });
//     defer gpa.free(c_path);
//     const pkgD_header = try std.fmt.allocPrint(gpa,
//         \\package []
//         \\{{ foo: "{s}" }}
//     , .{c_path});
//     defer gpa.free(pkgD_header);
//     try writeFile(tmp.dir, "pkgD/PkgD.roc", pkgD_header);
//     try writeFile(tmp.dir, "pkgD/ModuleD.roc",
//         \\module [getFromC]
//         \\import foo.ModuleC
//         \\getFromC = ModuleC.valueC
//     );

//     // App imports both pkgA and pkgD, which both use "foo" for different packages
//     const a_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgA", "PkgA.roc" });
//     defer gpa.free(a_path);
//     const d_path = try std.fs.path.join(gpa, &.{ root_dir, "pkgD", "PkgD.roc" });
//     defer gpa.free(d_path);

//     // App tries to use "foo" shorthand (which should fail - shorthands don't leak!)
//     const app_main = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ A: "{s}", D: "{s}" }}
//         \\
//         \\import A.ModuleA
//         \\import D.ModuleD
//         \\import foo.SomeModule # This should fail - "foo" is not defined in app
//         \\
//         \\main! = |_| ModuleA.getFromB
//     , .{ a_path, d_path });
//     defer gpa.free(app_main);
//     try writeFile(tmp.dir, "app/Main.roc", app_main);

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Building should fail because app cannot use "foo" shorthand
//     _ = ws.buildApp(app_path) catch |err| {
//         // The build should fail, but we need to check that pkgA and pkgD
//         // can still use their respective "foo" shorthands without conflict
//         try std.testing.expect(err == error.Internal or err == error.InvalidDependency);
//     };

//     // Now test that packages can use their shorthands without interference
//     // Create a valid app that doesn't try to use undefined shorthands
//     const app_main2 = try std.fmt.allocPrint(gpa,
//         \\app [main!] {{ A: "{s}", D: "{s}" }}
//         \\
//         \\import A.ModuleA
//         \\import D.ModuleD
//         \\
//         \\main! = |_| Str.concat ModuleA.getFromB ModuleD.getFromC
//     , .{ a_path, d_path });
//     defer gpa.free(app_main2);
//     try writeFile(tmp.dir, "app/Main2.roc", app_main2);

//     var ws2 = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws2.deinit();

//     const app_path2 = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main2.roc" });
//     defer gpa.free(app_path2);

//     // This should succeed - each package can use "foo" for different things
//     try ws2.buildApp(app_path2);

//     // Verify that both packages were built successfully - should have no errors
//     try expectNoErrors(&ws2);
// }

// test "BuildEnv: CacheManager integration - cached modules not rebuilt" {
//     const gpa = std.testing.allocator;
//     const fs_mod = @import("fs");
//     const Filesystem = fs_mod.Filesystem;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");
//     try tmp.dir.makePath("cache");

//     // Create test modules
//     try writeFile(tmp.dir, "app/Utils.roc",
//         \\module [double]
//         \\
//         \\double = |n| n * 2
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import Utils
//         \\
//         \\main! = |_| Utils.double 21
//     );

//     // First build with caching enabled
//     var ws1 = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws1.deinit();

//     // Set up cache
//     const cache_dir = try std.fs.path.join(gpa, &.{ root_dir, "cache" });
//     defer gpa.free(cache_dir);
//     const cache_config = cache.CacheConfig{
//         .enabled = true,
//         .cache_dir = cache_dir,
//     };
//     const cache_manager = try gpa.create(cache.CacheManager);
//     defer gpa.destroy(cache_manager);
//     cache_manager.* = cache.CacheManager.init(gpa, cache_config, Filesystem.default());
//     defer cache_manager.deinit();

//     ws1.setCacheManager(cache_manager);

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // First build - modules should be compiled and cached
//     try ws1.buildApp(app_path);
//     try expectNoErrors(&ws1);

//     // Verify cache was populated by checking phase progression
//     const app_build1 = ws1.packages.get("app").?;
//     const main_sched1 = app_build1.schedulers.get("app").?;

//     main_sched1.lock.lock();
//     const utils_state1 = main_sched1.modules.get("Utils").?;
//     const main_state1 = main_sched1.modules.get("Main").?;
//     try std.testing.expectEqual(PackageEnv.Phase.Done, utils_state1.phase);
//     try std.testing.expectEqual(PackageEnv.Phase.Done, main_state1.phase);
//     main_sched1.lock.unlock();

//     // Second build with same cache - modules should be loaded from cache
//     var ws2 = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws2.deinit();

//     // Use the same cache manager
//     const cache_manager2 = try gpa.create(cache.CacheManager);
//     defer gpa.destroy(cache_manager2);
//     cache_manager2.* = cache.CacheManager.init(gpa, cache_config, Filesystem.default());
//     defer cache_manager2.deinit();

//     ws2.setCacheManager(cache_manager2);

//     // Second build - should use cached modules
//     try ws2.buildApp(app_path);
//     try expectNoErrors(&ws2);

//     // TODO: Once we have proper cache hit tracking, verify that modules were loaded from cache
//     // For now, just verify the build succeeded
// }

// test "BuildEnv: CacheManager integration - cache invalidated on file change" {
//     const gpa = std.testing.allocator;
//     const fs_mod = @import("fs");
//     const Filesystem = fs_mod.Filesystem;

//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");
//     try tmp.dir.makePath("cache");

//     // Create initial module
//     try writeFile(tmp.dir, "app/Math.roc",
//         \\module [add]
//         \\
//         \\add = \a, b -> a + b
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import Math
//         \\
//         \\main! = |_| Math.add 1 2
//     );

//     // First build with caching
//     var ws1 = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws1.deinit();

//     const cache_dir = try std.fs.path.join(gpa, &.{ root_dir, "cache" });
//     defer gpa.free(cache_dir);
//     const cache_config = cache.CacheConfig{
//         .enabled = true,
//         .cache_dir = cache_dir,
//     };
//     const cache_manager = try gpa.create(cache.CacheManager);
//     defer gpa.destroy(cache_manager);
//     cache_manager.* = cache.CacheManager.init(gpa, cache_config, Filesystem.default());
//     defer cache_manager.deinit();

//     ws1.setCacheManager(cache_manager);

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     try ws1.buildApp(app_path);
//     try expectNoErrors(&ws1);

//     // Modify the Math module
//     try writeFile(tmp.dir, "app/Math.roc",
//         \\module [add, multiply]
//         \\
//         \\add = \a, b -> a + b
//         \\multiply = \a, b -> a * b
//     );

//     // Second build - cache should be invalidated for Math.roc
//     var ws2 = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws2.deinit();

//     const cache_manager2 = try gpa.create(cache.CacheManager);
//     defer gpa.destroy(cache_manager2);
//     cache_manager2.* = cache.CacheManager.init(gpa, cache_config, Filesystem.default());
//     defer cache_manager2.deinit();

//     ws2.setCacheManager(cache_manager2);

//     // Build should succeed with the modified module
//     try ws2.buildApp(app_path);
//     try expectNoErrors(&ws2);

//     // Verify the module was rebuilt (has the new export)
//     const app_build = ws2.packages.get("app").?;
//     const main_sched = app_build.schedulers.get("app").?;

//     main_sched.lock.lock();
//     defer main_sched.lock.unlock();

//     const math_state = main_sched.modules.get("Math").?;
//     try std.testing.expectEqual(PackageEnv.Phase.Done, math_state.phase);
//     // TODO: Once we track exports properly, verify that 'multiply' is now exported
// }

// test "BuildEnv: cyclic dependency detection" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create modules with a direct cycle: A -> B -> A
//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [valueA]
//         \\
//         \\import B
//         \\
//         \\valueA = B.valueB + 1
//     );

//     try writeFile(tmp.dir, "app/B.roc",
//         \\module [valueB]
//         \\
//         \\import A
//         \\
//         \\valueB = A.valueA + 2
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\
//         \\main! = |_| A.valueA
//     );

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Building should fail due to cyclic dependency
//     const res = ws.buildApp(app_path);
//     try std.testing.expectError(error.CyclicImport, res);
// }

// test "BuildEnv: cyclic dependency detection - indirect cycle" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create modules with an indirect cycle: A -> B -> C -> A
//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [valueA]
//         \\
//         \\import B
//         \\
//         \\valueA = B.valueB + 1
//     );

//     try writeFile(tmp.dir, "app/B.roc",
//         \\module [valueB]
//         \\
//         \\import C
//         \\
//         \\valueB = C.valueC + 2
//     );

//     try writeFile(tmp.dir, "app/C.roc",
//         \\module [valueC]
//         \\
//         \\import A
//         \\
//         \\valueC = A.valueA + 3
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\
//         \\main! = |_| A.valueA
//     );

//     var ws = BuildEnv.init(gpa, .single_threaded, 1);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Building should fail due to cyclic dependency
//     const res = ws.buildApp(app_path);
//     try std.testing.expectError(error.CyclicImport, res);
// }

// test "BuildEnv: no false positive cycle detection" {
//     const gpa = std.testing.allocator;
//     var tmp = std.testing.tmpDir(.{});
//     defer tmp.cleanup();

//     const root_dir = try tmp.dir.realpathAlloc(gpa, ".");
//     defer gpa.free(root_dir);
//     try tmp.dir.makePath("app");

//     // Create modules with shared dependencies but no cycle
//     // Main -> A -> C
//     //      -> B -> C
//     try writeFile(tmp.dir, "app/C.roc",
//         \\module [valueC]
//         \\
//         \\valueC = 42
//     );

//     try writeFile(tmp.dir, "app/A.roc",
//         \\module [valueA]
//         \\
//         \\import C
//         \\
//         \\valueA = C.valueC + 1
//     );

//     try writeFile(tmp.dir, "app/B.roc",
//         \\module [valueB]
//         \\
//         \\import C
//         \\
//         \\valueB = C.valueC + 2
//     );

//     try writeFile(tmp.dir, "app/Main.roc",
//         \\app [main!] {}
//         \\
//         \\import A
//         \\import B
//         \\
//         \\main! = |_| A.valueA + B.valueB
//     );

//     var ws = BuildEnv.init(gpa, .multi_threaded, 4);
//     defer ws.deinit();

//     const app_path = try std.fs.path.join(gpa, &.{ root_dir, "app", "Main.roc" });
//     defer gpa.free(app_path);

//     // Building should succeed - no cycle exists
//     try ws.buildApp(app_path);
//     try expectNoErrors(&ws);
// }
