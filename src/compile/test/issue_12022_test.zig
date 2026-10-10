//! Regression tests for issues #12022 and #12023 (the same program): a method
//! dispatched on a receiver whose type is a transparent alias of a
//! where-constrained type variable resolves through the alias's backing at
//! checked-artifact publication.
//! repro for https://github.com/roc-lang/roc/issues/12022
//! repro for https://github.com/roc-lang/roc/issues/12023

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const BuildTestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ WriteFailed, TestExpectedEqual };

/// Build `source` as a single module and return the markdown rendering of
/// every non-warning report, concatenated.
fn renderedErrors(gpa: std.mem.Allocator, source: []const u8) BuildTestError![]u8 {
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Item.roc", .data = source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const module_path = try tmp_dir.dir.realPathFileAlloc(io, "Item.roc", gpa);
    defer gpa.free(module_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(module_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var rendered: std.Io.Writer.Allocating = .init(gpa);
    errdefer rendered.deinit();
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .warning => {},
                .runtime_error, .fatal => try report.render(&rendered.writer, .markdown),
            }
        }
    }
    return rendered.toOwnedSlice();
}

fn expectNoErrors(source: []const u8) BuildTestError!void {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, source);
    defer gpa.free(rendered);
    try std.testing.expectEqualStrings("", rendered);
}

const item_and_wrapper =
    \\Item := [Item(U64)].{
    \\    score : Item -> U64
    \\    score = |Item.Item(n)| n
    \\
    \\    is_eq : Item, Item -> Bool
    \\    is_eq = |Item.Item(a), Item.Item(b)| a % 10 == b % 10
    \\}
    \\
    \\Wrapper(a) : a
    \\
    \\Twice(a) : Wrapper(a)
    \\
;

test "issue 12022: method dispatch on a transparent alias of a where-constrained variable publishes" {
    try expectNoErrors(item_and_wrapper ++
        \\score_wrapped : Wrapper(a) -> U64 where [a.score : a -> U64]
        \\score_wrapped = |value| value.score()
        \\
        \\answer = score_wrapped(Item.Item(42))
        \\
    );
}

test "issue 12022: dispatch through nested transparent aliases and forwarded evidence publishes" {
    try expectNoErrors(item_and_wrapper ++
        \\score_twice : Twice(a) -> U64 where [a.score : a -> U64]
        \\score_twice = |value| value.score()
        \\
        \\forward : Wrapper(b) -> U64 where [b.score : b -> U64]
        \\forward = |value| score_twice(value) + 1
        \\
        \\answer = forward(Item.Item(5))
        \\
    );
}

test "issue 12022: equality on a transparent alias of a where-constrained variable publishes" {
    try expectNoErrors(item_and_wrapper ++
        \\same : Wrapper(a), Wrapper(a) -> Bool where [a.is_eq : a, a -> Bool]
        \\same = |x, y| x == y
        \\
        \\answer = same(Item.Item(1), Item.Item(11))
        \\
    );
}
