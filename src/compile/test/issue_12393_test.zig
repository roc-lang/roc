//! Regression test for issue #12393.
//!
//! Checking an app that calls into an imported type module whose method
//! declares an annotated `var`, loops over a list, and matches with
//! `$counter += 1` (not valid expression syntax) in one branch reports the parse
//! error as a diagnostic, rather than check crashing in post-check Monotype
//! lowering.
//! repro for https://github.com/roc-lang/roc/issues/12393

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const platform_source =
    \\platform ""
    \\    requires {
    \\        main! : () => {}
    \\    }
    \\    exposes []
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\
    \\main_for_host! : () => {}
    \\main_for_host! = main!
    \\
;

const app_source =
    \\app [main!] { pf: platform "./platform.roc" }
    \\
    \\import Evaluator exposing [eval]
    \\
    \\main! = || {
    \\    dbg eval([])
    \\}
    \\
;

const evaluator_source =
    \\Evaluator :: {}.{
    \\    eval : List([Inc, Echo]) -> Str
    \\    eval = |commands| {
    \\        var $counter : I32
    \\        var $counter = 0
    \\        for command in commands {
    \\            match command {
    \\                Inc => $counter += 1
    \\                Echo => dbg $counter
    \\            }
    \\        }
    \\        ""
    \\    }
    \\}
    \\
;

test "issue 12393: compound assignment in a match branch of an imported type module reports a parse error" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Evaluator.roc", .data = evaluator_source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var found_parse_error = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Unexpected Expression Syntax")) found_parse_error = true;
        }
    }
    try std.testing.expect(found_parse_error);
}
