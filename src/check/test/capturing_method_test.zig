//! Tests for the rule that a method of a type declared in a function body
//! never captures a value of that function body.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const capture_title = "Method Captures a Local Value";

/// Expect one capture report per entry of `references`, in order, each
/// highlighting that source text and naming the value it reaches.
fn expectCapturesAt(source: []const u8, references: []const []const u8, captured: []const []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const titles = try std.testing.allocator.alloc([]const u8, references.len);
    defer std.testing.allocator.free(titles);
    @memset(titles, capture_title);
    try test_env.assertTypeErrorTitles(titles);
    const idents = test_env.module_env.getIdentStoreConst();
    for (test_env.checker.problems.problems.items, references, captured) |problem, reference, value| {
        const capture = problem.capturing_method;
        try std.testing.expectEqualStrings(reference, source[capture.region.start.offset..capture.region.end.offset]);
        try std.testing.expectEqualStrings(value, idents.getText(capture.captured_name));
    }
}

fn expectNoErrors(source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "method using a value of the enclosing body is reported at the use" {
    try expectCapturesAt(
        \\make = |n| {
        \\    offset = n * 2
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"offset"}, &.{"offset"});
}

test "method using a parameter of the enclosing function is reported" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"offset"}, &.{"offset"});
}

test "each enclosing value a method uses is reported once, at its first use" {
    try expectCapturesAt(
        \\make = |offset, scale, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count * scale + offset + scale
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{ "scale", "offset" }, &.{ "scale", "offset" });
}

test "closure inside a method using an enclosing value is reported" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| {
        \\            add = |x| x + offset
        \\            add(counter.count)
        \\        }
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"offset"}, &.{"offset"});
}

test "method calling a local function that captures is reported at the call" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    shift = |x| x + offset
        \\    Counter := { count : U64 }.{
        \\        value = |counter| shift(counter.count)
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"shift"}, &.{"offset"});
}

test "method calling a local function that captures through another is reported" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    shift = |x| x + offset
        \\    twice = |x| shift(shift(x))
        \\    Counter := { count : U64 }.{
        \\        value = |counter| twice(counter.count)
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"twice"}, &.{"offset"});
}

test "method bound to a capturing local function is reported at the binding" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    helper = |c| c.count + offset
        \\    alias = helper
        \\    Counter := { count : U64 }.{
        \\        value = alias
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"alias"}, &.{"offset"});
}

test "associated value that is no local procedure may use an enclosing value" {
    try expectNoErrors(
        \\make = |offset| {
        \\    Counter := { count : U64 }.{
        \\        start = Counter.{ count: offset }
        \\    }
        \\    Counter.start.count
        \\}
    );
}

test "capturing to_inspect is reported" {
    try expectCapturesAt(
        \\describe = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        to_inspect : Counter -> Str
        \\        to_inspect = |counter| (counter.count + offset).to_str()
        \\    }
        \\    Str.inspect(Counter.{ count: n })
        \\}
    , &.{"offset"}, &.{"offset"});
}

test "method calling a capturing sibling method reports only the sibling" {
    try expectCapturesAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| Counter.helper(counter) + counter.helper()
        \\        helper : Counter -> U64
        \\        helper = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    , &.{"offset"}, &.{"offset"});
}

test "methods may use their own parameters and locals, top-level values, sibling methods, and local functions that capture nothing" {
    var test_env = try TestEnv.init("Test",
        \\base = 10
        \\
        \\make = |n| {
        \\    double = |x| x * 2
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| {
        \\            local = counter.count + base
        \\            inner = |y| y + local
        \\            double(inner(1)) + counter.helper()
        \\        }
        \\        helper : Counter -> U64
        \\        helper = |counter| counter.count + 1
        \\    }
        \\    Counter.{ count: n }.value()
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 3), test_env.checker.promotedLocalProcedures().len);
}

test "method naming an enclosing type variable captures nothing" {
    var test_env = try TestEnv.init("Test",
        \\wrap : a -> List(a)
        \\wrap = |x| {
        \\    Holder := { v : a }.{
        \\        get : Holder -> a
        \\        get = |h| h.v
        \\    }
        \\    [Holder.{ v: x }.get()]
        \\}
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.promotedLocalProcedures().len);
}

test "ordinary local functions and closures may capture" {
    try expectNoErrors(
        \\make = |offset, n| {
        \\    shift = |x| x + offset
        \\    twice = |x| shift(shift(x))
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + 1
        \\    }
        \\    c = Counter.{ count: n }
        \\    adder = |x| x + c.value() + offset
        \\    twice(adder(n))
        \\}
    );
}

test "a value of a function-body type may leave its block" {
    try expectNoErrors(
        \\make = |n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + 1
        \\    }
        \\    Counter.{ count: n }
        \\}
        \\
        \\main = make(1).value()
    );
}
