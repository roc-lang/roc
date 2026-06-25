//! Tests for compile-time diagnostics that are emitted while publishing checked modules.

const std = @import("std");
const check = @import("check");
const eval = @import("eval");

fn countProblemTag(problems: []const check.problem.Problem, tag: check.problem.Problem.Tag) usize {
    var count: usize = 0;
    for (problems) |problem| {
        if (std.meta.activeTag(problem) == tag) count += 1;
    }
    return count;
}

test "imported eligible top-level diagnostics run during checking" {
    const allocator = std.testing.allocator;
    const main_source =
        \\import Util
        \\main = Util.safe
    ;

    const imported_dbg =
        \\module [safe]
        \\
        \\hidden_dbg : I64
        \\hidden_dbg = {
        \\    dbg "imported unused dbg"
        \\    0.I64
        \\}
        \\
        \\safe = 42
    ;

    var dbg_resources = try eval.test_helpers.publishProgramKeepingReportedComptimeProblems(
        allocator,
        .module,
        main_source,
        &.{.{ .name = "Util", .source = imported_dbg }},
    );
    defer dbg_resources.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), dbg_resources.extra_modules.len);
    try std.testing.expectEqual(@as(usize, 0), countProblemTag(dbg_resources.checker.problems.problems.items, .comptime_dbg));
    try std.testing.expectEqual(@as(usize, 1), countProblemTag(dbg_resources.extra_modules[0].checker.problems.problems.items, .comptime_dbg));
    try std.testing.expectEqual(@as(usize, 0), countProblemTag(dbg_resources.extra_modules[0].checker.problems.problems.items, .comptime_expect_failed));
    try std.testing.expectEqual(@as(usize, 0), countProblemTag(dbg_resources.extra_modules[0].checker.problems.problems.items, .comptime_crash));

    const imported_expect =
        \\module [safe]
        \\
        \\hidden_expect : I64
        \\hidden_expect = {
        \\    expect False
        \\    0.I64
        \\}
        \\
        \\safe = 42
    ;
    try std.testing.expectEqual(
        eval.test_helpers.ComptimePublishOutcome.comptime_problems,
        try eval.test_helpers.publishProgramForComptimeProblems(
            allocator,
            .module,
            main_source,
            &.{.{ .name = "Util", .source = imported_expect }},
        ),
    );

    const imported_crash =
        \\module [safe]
        \\
        \\hidden_crash : I64
        \\hidden_crash = {
        \\    crash "imported unused crash"
        \\    0.I64
        \\}
        \\
        \\safe = 42
    ;
    try std.testing.expectEqual(
        eval.test_helpers.ComptimePublishOutcome.comptime_problems,
        try eval.test_helpers.publishProgramForComptimeProblems(
            allocator,
            .module,
            main_source,
            &.{.{ .name = "Util", .source = imported_crash }},
        ),
    );
}
