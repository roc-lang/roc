//! Regression tests for custom literal conversions rejected at compile time.
//! A literal whose `from_quote` or `from_numeral` returns `Err` is reported
//! exactly once, as that literal's own "Invalid String" or "Invalid Number"
//! diagnostic, and never as a compile-time crash of a definition that uses the
//! literal. This holds whether the literal's target is concrete where it is
//! written or only once a generalized local function is instantiated.
//! Likewise a compile-time crash read by other constants is reported once, at
//! the constant that crashed.
//! repro for https://github.com/roc-lang/roc/issues/11670
//! repro for https://github.com/roc-lang/roc/issues/11675

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const rejecting_sql =
    \\Sql := { text : Str }.{
    \\    from_quote : Str -> Try(Sql, [BadQuotedBytes(Str)])
    \\    from_quote = |raw| if raw == "bad" Err(BadQuotedBytes("rejected")) else Ok(Sql.{ text: raw })
    \\}
    \\
    \\Db := { name : Str }.{
    \\    query : Db, Sql -> Str
    \\    query = |_, sql| sql.text
    \\}
    \\
;

test "issue 11670: a rejected from_quote literal in a top-level constant reports only the invalid string" {
    try expectOnlyReport(rejecting_sql ++
        \\sql : Sql
        \\sql = "bad"
        \\
        \\text = sql.text
        \\
    , "Invalid String");
}

test "issue 11670: a rejected from_numeral literal in a top-level constant reports only the invalid number" {
    try expectOnlyReport(
        \\Small := [Small(U8)].{
        \\    from_numeral : Numeral -> Try(Small, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Err(InvalidNumeral("rejected"))
        \\}
        \\
        \\small : Small
        \\small = 5
        \\
        \\value = match small {
        \\    Small(n) => n
        \\}
        \\
    , "Invalid Number");
}

test "issue 11675: a rejected from_quote literal passed to a method of an unannotated parameter is reported" {
    try expectOnlyReport(rejecting_sql ++
        \\run : {} -> Str
        \\run = |_| {
        \\    by_id = |db| db.query("bad")
        \\    by_id(Db.{ name: "pg" })
        \\}
        \\
    , "Invalid String");
}

test "issue 11675: a rejected from_quote literal in a generalized local of a top-level constant is reported" {
    try expectOnlyReport(rejecting_sql ++
        \\value = {
        \\    by_id = |db| db.query("bad")
        \\    by_id(Db.{ name: "pg" })
        \\}
        \\
    , "Invalid String");
}

test "issue 11675: a rejected from_quote literal reached through a local helper function is reported" {
    try expectOnlyReport(rejecting_sql ++
        \\run : {} -> Str
        \\run = |_| {
        \\    make = |raw| Sql.{ text: raw }
        \\    by_id = |db| db.query("bad")
        \\    helper = |db| by_id(db)
        \\    Str.concat(helper(Db.{ name: "pg" }), make("x").text)
        \\}
        \\
    , "Invalid String");
}

test "issue 11675: a rejected from_quote literal at a generalized return type is reported at its instantiation" {
    try expectOnlyReport(
        \\Sql(a) := { text : Str }.{
        \\    from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
        \\    from_quote = |_| Err(BadQuotedBytes("rejected"))
        \\}
        \\
        \\get : {} -> Sql(a)
        \\get = |_| "select 1"
        \\
        \\run : {} -> Str
        \\run = |_| {
        \\    s : Sql(I32)
        \\    s = get({})
        \\    s.text
        \\}
        \\
    , "Invalid String");
}

test "a crashing constant read by two other constants is reported once" {
    try expectOnlyReport(
        \\x : Str
        \\x = crash "boom"
        \\
        \\y = Str.concat(x, "!")
        \\
        \\z = x
        \\
    , "Compile Time Crash");
}

const RejectionTestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// Build `source` as a module and require that its only error report is one
/// report titled `report_title`.
fn expectOnlyReport(source: []const u8, report_title: []const u8) RejectionTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const module_source = try std.mem.concat(gpa, u8, &.{ "module []\n\n", source });
    defer gpa.free(module_source);
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = module_source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);

    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);

    var matching: usize = 0;
    var other_errors: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, report_title)) {
                matching += 1;
            } else if (report.severity != .warning) {
                std.debug.print("unexpected report: {s}\n", .{report.title});
                other_errors += 1;
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 0), other_errors);
    try std.testing.expectEqual(@as(usize, 1), matching);
}
