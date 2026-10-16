//! Regression tests for issue 12151.

const std = @import("std");
const testing = std.testing;
const TestEnv = @import("./TestEnv.zig");

// https://github.com/roc-lang/roc/issues/12151
//
// Code written after a statement that never finishes (it always returns,
// crashes, breaks, or loops forever) can never run, so canonicalization
// reports it with an unreachable-code warning covering that code.

/// Check `source` and expect exactly one unreachable-code warning per entry
/// of `expected`, each covering that source text.
fn expectUnreachableCode(source: []const u8, expected: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    const source_text = test_env.module_env.getSourceAll();
    var found: usize = 0;
    for (diagnostics) |diagnostic| {
        if (diagnostic == .unreachable_code) {
            const region = diagnostic.unreachable_code.region;
            try testing.expect(found < expected.len);
            try testing.expectEqualStrings(expected[found], source_text[region.start.offset..region.end.offset]);
            found += 1;
        }
    }
    try testing.expectEqual(expected.len, found);
}

/// Count every canonicalization diagnostic and type problem, warnings
/// included, that checking `source` reports.
fn countReportedProblems(source: []const u8) TestEnv.TestEnvError!usize {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    return diagnostics.len + test_env.checker.problems.problems.items.len;
}

test "issue 12151: a block whose return or crash is not followed by code reports nothing" {
    const source =
        \\f = |x| {
        \\    if x {
        \\        return 1
        \\    }
        \\    2
        \\}
        \\
        \\g = |x| {
        \\    crash x
        \\}
    ;
    try testing.expectEqual(@as(usize, 0), try countReportedProblems(source));
}

test "issue 12151: code after return is reported" {
    const source =
        \\f = |x| {
        \\    return x
        \\    1
        \\}
    ;
    try expectUnreachableCode(source, &.{"1"});
}

test "issue 12151: code after crash is reported" {
    const source =
        \\g = |x| {
        \\    crash x
        \\    1
        \\}
    ;
    try expectUnreachableCode(source, &.{"1"});
}

test "issue 12151: everything after the first statement that never finishes is reported once" {
    const source =
        \\f = |x| {
        \\    return x
        \\    y = 1
        \\    z = y
        \\    z
        \\}
    ;
    try expectUnreachableCode(source, &.{
        \\y = 1
        \\    z = y
        \\    z
    });
}

test "issue 12151: an annotated definition after return is reported from its annotation" {
    const source =
        \\f = |x| {
        \\    return x
        \\    y : U64
        \\    y = 1
        \\    y
        \\}
    ;
    try expectUnreachableCode(source, &.{
        \\y : U64
        \\    y = 1
        \\    y
    });
}

test "issue 12151: code after an if whose branches all return is reported" {
    const source =
        \\f = |x| {
        \\    if x {
        \\        return 1
        \\    } else {
        \\        return 2
        \\    }
        \\    3
        \\}
    ;
    try expectUnreachableCode(source, &.{"3"});
}

test "issue 12151: code after a match whose branches all crash is reported" {
    const source =
        \\f = |x| {
        \\    match x {
        \\        Ok(_) => crash "ok"
        \\        Err(_) => crash "err"
        \\    }
        \\    3
        \\}
    ;
    try expectUnreachableCode(source, &.{"3"});
}

test "issue 12151: code after a match with a finishing branch is not reported" {
    const source =
        \\f = |x| {
        \\    match x {
        \\        Ok(_) => crash "ok"
        \\        Err(_) => {}
        \\    }
        \\    3
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}

test "issue 12151: code after break in a loop body is reported" {
    const source =
        \\f = |xs| {
        \\    for x in xs {
        \\        break
        \\        dbg x
        \\    }
        \\    xs
        \\}
    ;
    try expectUnreachableCode(source, &.{"dbg x"});
}

test "issue 12151: code after a declaration whose value crashes is reported" {
    const source =
        \\f = |_| {
        \\    x = crash "boom"
        \\    x
        \\}
    ;
    try expectUnreachableCode(source, &.{"x"});
}

test "issue 12151: a return inside a nested lambda does not make the outer block unreachable" {
    const source =
        \\f = |x| {
        \\    g = |y| {
        \\        return y
        \\    }
        \\    g(x)
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}

test "issue 12151: code after an inline expect is reachable" {
    const source =
        \\f = |x| {
        \\    expect {
        \\        crash "no"
        \\    }
        \\    x
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}

test "issue 12151: code after the try operator is reachable" {
    const source =
        \\f = |s| {
        \\    n = U64.from_str(s)?
        \\    Ok(n)
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}

test "issue 12151: only the left operand of a short-circuiting operator makes code unreachable" {
    const source =
        \\f = |x| {
        \\    ok = x and (return False)
        \\    ok
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}

test "issue 12151: a rejected return does not make later code unreachable" {
    const source =
        \\f = |x| {
        \\    expect {
        \\        return True
        \\    }
        \\    x
        \\}
    ;
    try expectUnreachableCode(source, &.{});
}
