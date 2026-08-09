//! Regression test for issue #10346.

const std = @import("std");
const check = @import("check");
const helpers = @import("eval").test_helpers;

test "issue 10346: unsupported generated method warning survives publication" {
    // Repro for https://github.com/roc-lang/roc/issues/10346.
    // Publication must preserve the warning instead of treating the
    // annotation-only method as a callable dispatch target.
    const source =
        \\Thing := U64.{
        \\    foo : _
        \\}
        \\
        \\main = Thing.(42).foo()
    ;

    const gpa = std.testing.allocator;
    var resources = try helpers.parseAndCanonicalizeInspectedProgram(
        gpa,
        .module,
        source,
        &.{},
    );
    defer resources.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 1), resources.checker.problems.problems.items.len);
    try std.testing.expectEqual(
        check.problem.Problem.Tag.unsupported_generated_method,
        std.meta.activeTag(resources.checker.problems.problems.items[0]),
    );
    try std.testing.expectEqual(@as(usize, 0), resources.checked_artifact.method_registry.entries.len);
    try std.testing.expectEqual(@as(usize, 0), resources.checked_artifact.static_dispatch_plans.plans.len);
}
