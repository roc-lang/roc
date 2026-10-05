//! Tests for the rule that a type declared in a block, with a method that
//! captures local values, stays in that block.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const escape_title = "Local Type Escapes Its Block";

fn expectEscapeAt(source: []const u8, escaping: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(&.{escape_title});
    const escape = test_env.checker.problems.problems.items[0].capturing_local_type_escape;
    try std.testing.expectEqualStrings(escaping, source[escape.region.start.offset..escape.region.end.offset]);
}

fn expectNoErrors(source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "capturing local type returned from its block escapes" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }
        \\}
    , "Counter.{ count: n }");
}

test "capturing local type returned early from its block escapes" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    if n > 100 {
        \\        Counter := { count : U64 }.{
        \\            value = |counter| counter.count + offset
        \\        }
        \\        return Counter.{ count: n }
        \\    }
        \\    crash "small"
        \\}
    , "Counter.{ count: n }");
}

test "capturing local type assigned to an outer variable escapes" {
    try expectEscapeAt(
        \\run = |n| {
        \\    var $saved = []
        \\    {
        \\        offset = 1
        \\        Counter := { count : U64 }.{
        \\            value = |counter| counter.count + offset
        \\        }
        \\        $saved = [Counter.{ count: n }]
        \\    }
        \\    List.len($saved)
        \\}
    , "[Counter.{ count: n }]");
}

test "outer binding given a capturing local type escapes" {
    try expectEscapeAt(
        \\run = |offset, x| {
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| counter.count + offset
        \\    }
        \\    Counter.value(x)
        \\}
    , "x");
}

test "capturing local type inside another local type escapes" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + offset
        \\    }
        \\    Wrapper := { c : Counter }
        \\    Wrapper.{ c: Counter.{ count: n } }
        \\}
    , "Wrapper.{ c: Counter.{ count: n } }");
}

test "capturing local type leaving an inner block escapes" {
    try expectEscapeAt(
        \\run = |offset, n| {
        \\    c = {
        \\        Counter := { count : U64 }.{
        \\            value = |counter| counter.count + offset
        \\        }
        \\        Counter.{ count: n }
        \\    }
        \\    c.value()
        \\}
    , "Counter.{ count: n }");
}

test "method bound to a capturing local function makes its type stay in its block" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    helper = |c| c.count + offset
        \\    Counter := { count : U64 }.{
        \\        value = helper
        \\    }
        \\    Counter.{ count: n }
        \\}
    , "Counter.{ count: n }");
}

test "local type whose methods are all promoted may leave its block" {
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

test "closure using a capturing local type may leave its block" {
    try expectNoErrors(
        \\get = |c| c.value()
        \\
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value = |counter| counter.count + offset
        \\    }
        \\    c = Counter.{ count: n }
        \\    local = |x| x.value()
        \\    total = get(c) + local(c) + c.value()
        \\    |extra| c.value() + total + extra
        \\}
    );
}

test "local type whose methods only name it may leave its block" {
    var test_env = try TestEnv.init("Test",
        \\make = |n| {
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| counter.helper() + Counter.helper(counter)
        \\        helper : Counter -> U64
        \\        helper = |counter| counter.count + 1
        \\    }
        \\    Counter.{ count: n }
        \\}
        \\
        \\main = make(1).value()
    );
    defer test_env.deinit();

    try test_env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 2), test_env.checker.promotedLocalProcedures().len);
}

test "method dispatching to a capturing sibling keeps its type in its block" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| counter.helper()
        \\        helper : Counter -> U64
        \\        helper = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }
        \\}
    , "Counter.{ count: n }");
}

test "method naming a capturing sibling through its type keeps its type in its block" {
    try expectEscapeAt(
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        value : Counter -> U64
        \\        value = |counter| Counter.helper(counter)
        \\        helper : Counter -> U64
        \\        helper = |counter| counter.count + offset
        \\    }
        \\    Counter.{ count: n }
        \\}
    , "Counter.{ count: n }");
}

test "method comparing a type with a capturing is_eq keeps its type in its block" {
    var test_env = try TestEnv.init("Test",
        \\make = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        is_eq = |a, b| a.count + offset == b.count
        \\    }
        \\    Pair := { c : Counter }.{
        \\        same : Pair, Pair -> Bool
        \\        same = |a, b| { c: a.c } == { c: b.c }
        \\    }
        \\    Pair.{ c: Counter.{ count: n } }
        \\}
    );
    defer test_env.deinit();

    try test_env.assertTypeErrorTitles(&.{ escape_title, escape_title });
}

test "method of a local type naming an enclosing type variable is not promoted" {
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

test "to_inspect of a local type that captures a local is accepted without diagnostics" {
    try expectNoErrors(
        \\describe = |offset, n| {
        \\    Counter := { count : U64 }.{
        \\        to_inspect : Counter -> Str
        \\        to_inspect = |counter| (counter.count + offset).to_str()
        \\    }
        \\    c = Counter.{ count: n }
        \\    Str.concat(Str.inspect(c), c.to_inspect())
        \\}
    );
}

test "instantiating a generalized definition at its capturing local type escapes at the use" {
    try expectEscapeAt(
        \\total_of = |base, counts| {
        \\    offset = base * 10
        \\    Counter := { count : U64 }.{
        \\        value = |c| c.count + offset
        \\    }
        \\    counters = counts.map(|n| Counter.{ count: n })
        \\    List.sum(counters.map(|c| c.value()))
        \\}
        \\
        \\run = |_| total_of(1, [1, 2, 3])
    , "total_of");
}

test "a capturing local type leaving its block through a generalized variable escapes at the use" {
    try expectEscapeAt(
        \\total_of = |base, counts| {
        \\    counters = {
        \\        offset = base * 10
        \\        Counter := { count : U64 }.{
        \\            value = |c| c.count + offset
        \\        }
        \\        counts.map(|n| Counter.{ count: n })
        \\    }
        \\    List.sum(counters.map(|c| c.value()))
        \\}
        \\
        \\run = |_| total_of(1, [1, 2, 3])
    , "total_of");
}

test "an annotated definition fixes its capturing local type inside its block" {
    try expectNoErrors(
        \\total_of : U64, List(U64) -> U64
        \\total_of = |base, counts| {
        \\    offset = base * 10
        \\    Counter := { count : U64 }.{
        \\        value = |c| c.count + offset
        \\    }
        \\    counters = counts.map(|n| Counter.{ count: n })
        \\    List.sum(counters.map(|c| c.value()))
        \\}
        \\
        \\run = |_| total_of(1, [1, 2, 3])
    );
}

test "a generic literal converted through a capturing local type inside its block is accepted" {
    try expectNoErrors(
        \\conv = |_u| "abc"
        \\
        \\run = |extra| {
        \\    Name := { s : Str, n : U64 }.{
        \\        from_quote = |s| Ok(Name.{ s, n: extra })
        \\    }
        \\    x : Name
        \\    x = conv({})
        \\    x.n
        \\}
    );
}
