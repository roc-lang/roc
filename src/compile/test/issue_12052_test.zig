//! Regression coverage for checking a copy of the compiler's `Builtin.roc`
//! below an ancestor `main.roc` (#12052), and for which module of such a build
//! the build's entry settings apply to.
//!
//! A `Builtin.roc` file on disk is an ordinary user module: only the builtin
//! embedded in the compiler has the `.builtin` role. A byte-identical copy is
//! therefore a different module from the embedded builtin, with its own
//! identity, and the ancestor `main.roc` that supplies the package keeps its
//! auto-imported builtin types (so its synthetic `echo!` can be typed as
//! `Str => {}`).
const std = @import("std");
const roc_target = @import("roc_target");
const compiled_builtins = @import("compiled_builtins");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const ancestor_default_app_source =
    \\main! = |_args| {
    \\    echo!("hello")
    \\    Ok({})
    \\}
;

test "issue 12052: a Builtin.roc copy below an ancestor default-app main.roc is checked as an ordinary module" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(io, "sub");
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = ancestor_default_app_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "sub/Builtin.roc", .data = compiled_builtins.builtin_source });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);
    const builtin_path = try tmp.dir.realPathFileAlloc(io, "sub/Builtin.roc", gpa);
    defer gpa.free(builtin_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    build_env.buildResolvingMain(builtin_path, null) catch {};

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var main_errors: usize = 0;
    var copy_errors: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .runtime_error, .fatal => {
                    if (std.mem.eql(u8, module_reports.abs_path, builtin_path)) {
                        copy_errors += 1;
                    } else {
                        main_errors += 1;
                        std.debug.print("unexpected {s} in {s}: {s}\n", .{
                            report.severity.toString(), module_reports.abs_path, report.title,
                        });
                    }
                },
                .warning => {},
            }
        }
    }

    try std.testing.expectEqual(@as(usize, 0), main_errors);
    // The copy calls compiler-provided operations that only the embedded
    // builtin may use.
    try std.testing.expect(copy_errors > 0);
}

test "issue 12052: the owning main.roc of an explicit-roots entry build keeps ordinary validation" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // A headerless main.roc with no `main!` is a default app missing its
    // entrypoint under ordinary validation; only explicit-roots validation
    // would accept it as a plain module.
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = "x = 1\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "Child.roc", .data = "module [y]\n\ny = 2\n\nexpect y == 2\n" });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);
    const child_path = try tmp.dir.realPathFileAlloc(io, "Child.roc", gpa);
    defer gpa.free(child_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    // `roc test --main=main.roc Child.roc` relaxes validation for the tested
    // file only.
    build_env.setEntryValidation(.explicit_roots);

    build_env.buildWithMain(child_path, main_path) catch {};

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var main_errors: usize = 0;
    var child_errors: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .runtime_error, .fatal => {
                    if (std.mem.eql(u8, module_reports.abs_path, main_path)) {
                        main_errors += 1;
                    } else {
                        child_errors += 1;
                        std.debug.print("unexpected {s} in {s}: {s}\n", .{
                            report.severity.toString(), module_reports.abs_path, report.title,
                        });
                    }
                },
                .warning => {},
            }
        }
    }

    try std.testing.expectEqual(@as(usize, 1), main_errors);
    try std.testing.expectEqual(@as(usize, 0), child_errors);
}
