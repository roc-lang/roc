//! Regression tests for rejections decided after the rejected value was
//! checked, and for consumers of erroneous values. A literal's conversion can
//! be rejected after the names bound from it were checked; every name bound
//! from it, through any chain of aliases and destructures, binds nothing, so
//! its uses add no report, even uses checked before the rejection that
//! conflict with each other; independent errors beside them are still
//! reported. An erroneous value already owns its report, so a
//! `return`, a branch, a function body, or an annotation that consumes it
//! reports nothing further, and no report prints the erroneous type.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

fn expectOnlyTypeProblems(source: []const u8, titles: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(titles);
    try test_env.assertNoReportRendersErrorType();
}

fn expectOnlyCanError(source: []const u8, title: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneCanError(title);
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
    try test_env.assertNoReportRendersErrorType();
}

test "a name `?` unwraps from a string literal rejected as a Try adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    y = "abc"?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name `?` unwraps from a numeral rejected as a Try adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    y = 5?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a match branch binder of a literal its patterns reject adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x|
        \\    match "abc" {
        \\        Ok(y) => y.to_str()
        \\        Err(_) => ""
        \\    }
    , &.{ "Unconditional Condition", "Missing Method" });
}

test "a destructured binder of a literal its pattern rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    (a, _b) = "abc"
        \\    a.to_str()
        \\}
    , &.{"Missing Method"});
}

test "a name bound from a local whose literal a later `?` rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    s = "abc"
        \\    y = s?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name bound through a chain of aliases from a literal a later `?` rejects adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    s = "abc"
        \\    t = s
        \\    y = t?
        \\    Ok(y.to_str())
        \\}
    , &.{"Type Mismatch"});
}

test "a name bound through a destructure of an alias of a rejected literal adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    p = "abc"
        \\    (a, _b) = p
        \\    c = a
        \\    c.to_str()
        \\}
    , &.{"Missing Method"});
}

test "a loop binder of a literal whose default is rejected adds no method report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    var $acc = ""
        \\    for y in 5 {
        \\        $acc = y.to_str()
        \\    }
        \\    $acc
        \\}
    , &.{"Type Not Determined"});
}

test "two uses of a loop binder of a rejected literal that conflict with each other add no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = Str.concat(y, "a")
        \\        b = List.len(y)
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "list elements that relate a rejected loop binder to conflicting literals add no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = [y, "s"]
        \\        b = [y, 1.U8]
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "conflicting annotations on aliases of a rejected loop binder add no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        z = y
        \\        a : Str
        \\        a = z
        \\        b : U8
        \\        b = z
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "an operator on a rejected loop binder whose other use determined its type adds no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = Str.concat(y, "a")
        \\        b = y + 1
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "comparisons of a rejected loop binder with conflicting literals add no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = if y == "q" 1 else 2
        \\        b = if y == 1.U8 1 else 2
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "a value computed from a rejected loop binder binds nothing" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = Str.concat(y, "a")
        \\        b = List.len(a)
        \\        _ = (a, b)
        \\    }
        \\    {}
        \\}
    , &.{"Type Not Determined"});
}

test "conflicting uses of the result of a dispatch rejected at its literal's default add no report" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    s = 5
        \\    y = s.foo()
        \\    a = Str.concat(y, "a")
        \\    b = List.len(y)
        \\    (a, b)
        \\}
    , &.{"Type Not Determined"});
}

test "independent errors beside a rejected loop binder's conflicting uses are still reported" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    for y in 5 {
        \\        a = Str.concat(y, "a")
        \\        b = List.len(y)
        \\        c = Str.concat(1.U8, "x")
        \\        d = [1.U8, "s"]
        \\        _ = (a, b, c, d)
        \\    }
        \\    {}
        \\}
    , &.{ "Type Mismatch", "Type Mismatch", "Type Not Determined" });
}

test "a branch that conflicts with a valid branch beside a rejected loop binder is still reported" {
    try expectOnlyTypeProblems(
        \\run = |c| {
        \\    for y in 5 {
        \\        e = if c y else if !c "s" else 1.U8
        \\        _ = e
        \\    }
        \\    {}
        \\}
    , &.{ "Type Mismatch", "Type Not Determined" });
}

test "the relation that rejects a name's literal still reports the rejection" {
    try expectOnlyTypeProblems(
        \\run = |_x| {
        \\    y = 5
        \\    a = Str.concat(y, "a")
        \\    b = List.len(y)
        \\    (a, b)
        \\}
    , &.{"Type Mismatch"});
}

test "a dispatch on a tag payload no value determines is reported as an undetermined type" {
    try expectOnlyTypeProblems(
        \\unwrap = |t| match t {
        \\    T0(v) => v.concat("0")
        \\    T7(v) => v.concat("7")
        \\}
        \\
        \\main = unwrap(T7("x"))
    , &.{"Type Not Determined"});
}

test "returning an erroneous value from a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\f : I64 -> [Blue]
        \\f = |x| {
        \\    if x > 0 {
        \\        return Err(undefined_fn(x))
        \\    }
        \\    Blue
        \\}
    , "Name Not In Scope");
}

test "an erroneous branch of a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\g : I64 -> [Blue]
        \\g = |x| if x > 0 Err(undefined_fn(x)) else Blue
    , "Name Not In Scope");
}

test "an erroneous function body of a closed result adds no mismatch" {
    try expectOnlyCanError(
        \\h : [Blue] -> [Blue]
        \\h = |_x| Err(undefined_fn(1))
    , "Name Not In Scope");
}

test "an annotation of a function whose body value is erroneous adds no mismatch" {
    try expectOnlyCanError(
        \\k : I64 -> I64
        \\k = |x| undefined_fn(x)
    , "Name Not In Scope");
}

test "a function whose body value is erroneous still checks its parameters against its annotation" {
    var test_env = try TestEnv.init("Test",
        \\k : I64, I64 -> I64
        \\k = |x| undefined_fn(x)
    );
    defer test_env.deinit();

    try test_env.assertOneCanError("Name Not In Scope");
    try test_env.assertTypeErrorTitles(&.{"Type Mismatch"});
    try test_env.assertNoReportRendersErrorType();
}

test "an unannotated function whose body value is erroneous adds no report" {
    var test_env = try TestEnv.init("Test",
        \\k = |x| undefined_fn(x)
    );
    defer test_env.deinit();

    try test_env.assertOneCanError("Name Not In Scope");
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());
}

test "a valid `?` on a numeral-producing call still checks" {
    var test_env = try TestEnv.init("Test",
        \\parse : Str -> Try(I64, [Bad])
        \\parse = |s| if s == "" Err(Bad) else Ok(41)
        \\
        \\run = |x| {
        \\    y = parse(x)?
        \\    Ok((y + 1).to_str())
        \\}
    );
    defer test_env.deinit();

    try test_env.assertDefType("run", "Str -> Try(Str, [Bad])");
}
