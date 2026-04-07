//! Runtime host-effects tests over the fixed production `host_abi` contract.

const eval = @import("eval");
const TestCase = @import("host_effects_runner.zig").TestCase;
const Termination = eval.RuntimeHostEnv.Termination;

fn dbg(bytes: []const u8) TestCase.ExpectedEvent {
    return .{ .dbg = bytes };
}

fn expectFailed(bytes: []const u8) TestCase.ExpectedEvent {
    return .{ .expect_failed = bytes };
}

fn crashed(bytes: []const u8) TestCase.ExpectedEvent {
    return .{ .crashed = bytes };
}

fn exprTest(
    name: []const u8,
    source: []const u8,
    expected_events: []const TestCase.ExpectedEvent,
    expected_termination: Termination,
) TestCase {
    return .{
        .name = name,
        .source = source,
        .expected_events = expected_events,
        .expected_termination = expected_termination,
    };
}

pub const tests = [_]TestCase{
    // Ported from origin/main:src/eval/test/interpreter_style_test.zig
    exprTest(
        "host effects: crash statement triggers crash callback",
        \\{
        \\    crash "boom"
        \\    0
        \\}
    ,
        &.{crashed("boom")},
        .crashed,
    ),
    exprTest(
        "host effects: crash at end of block in if branch does not fire on untaken path",
        \\{
        \\    f = |x| {
        \\        if x == 0 {
        \\            crash "division by zero"
        \\        }
        \\        42 / x
        \\    }
        \\    f(2)
        \\}
    ,
        &.{},
        .returned,
    ),
    exprTest(
        "host effects: expect expression succeeds",
        \\{
        \\    expect 1 == 1
        \\    {}
        \\}
    ,
        &.{},
        .returned,
    ),
    exprTest(
        "host effects: expect expression failure reports and continues",
        \\{
        \\    expect 1 == 0
        \\    {}
        \\}
    ,
        &.{expectFailed("expect failed")},
        .returned,
    ),
    exprTest(
        "host effects: dbg statement in block",
        \\{
        \\    x = 42
        \\    dbg x
        \\    x + 1
        \\}
    ,
        &.{dbg("42.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg statement with string",
        \\{
        \\    msg = "hello"
        \\    dbg msg
        \\    msg
        \\}
    ,
        &.{dbg("\"hello\"")},
        .returned,
    ),
    exprTest(
        "host effects: dbg integer literal",
        \\{
        \\    dbg 42
        \\    123
        \\}
    ,
        &.{dbg("42.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg negative integer",
        \\{
        \\    x = -99
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("-99.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg float value",
        \\{
        \\    x : F64
        \\    x = 3.14
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("3.14")},
        .returned,
    ),
    exprTest(
        "host effects: dbg boolean True",
        \\{
        \\    dbg True
        \\    False
        \\}
    ,
        &.{dbg("True")},
        .returned,
    ),
    exprTest(
        "host effects: dbg boolean False",
        \\{
        \\    dbg False
        \\    True
        \\}
    ,
        &.{dbg("False")},
        .returned,
    ),
    exprTest(
        "host effects: dbg empty string",
        \\{
        \\    dbg ""
        \\    "done"
        \\}
    ,
        &.{dbg("\"\"")},
        .returned,
    ),
    exprTest(
        "host effects: dbg list of integers",
        \\{
        \\    xs = [1.I64, 2.I64, 3.I64]
        \\    dbg xs
        \\    xs
        \\}
    ,
        &.{dbg("[1, 2, 3]")},
        .returned,
    ),
    exprTest(
        "host effects: dbg tuple",
        \\{
        \\    t = (1, "two", 3)
        \\    dbg t
        \\    t
        \\}
    ,
        &.{dbg("(1.0, \"two\", 3.0)")},
        .returned,
    ),
    exprTest(
        "host effects: dbg record",
        \\{
        \\    r = { name: "Alice", age: 30 }
        \\    dbg r
        \\    r
        \\}
    ,
        &.{dbg("{ age: 30.0, name: \"Alice\" }")},
        .returned,
    ),
    exprTest(
        "host effects: dbg empty record",
        \\{
        \\    r = {}
        \\    dbg r
        \\    r
        \\}
    ,
        &.{dbg("{}")},
        .returned,
    ),
    exprTest(
        "host effects: dbg tag without payload",
        \\{
        \\    x : [A, B, C]
        \\    x = B
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("B")},
        .returned,
    ),
    exprTest(
        "host effects: dbg tag with payload",
        \\{
        \\    x = Ok(42)
        \\    dbg x
        \\    match x { Ok(n) => n, Err(_) => 0 }
        \\}
    ,
        &.{dbg("Ok(42.0)")},
        .returned,
    ),
    exprTest(
        "host effects: dbg function value",
        \\{
        \\    f = |x| x + 1
        \\    dbg f
        \\    f(5)
        \\}
    ,
        &.{dbg("<fn>")},
        .returned,
    ),
    exprTest(
        "host effects: dbg expression form returns unit",
        \\{
        \\    x = 42
        \\    dbg x
        \\    x + 1
        \\}
    ,
        &.{dbg("42.0")},
        .returned,
    ),
    exprTest(
        "host effects: multiple dbg calls in sequence",
        \\{
        \\    x = 1
        \\    y = 2
        \\    z = 3
        \\    dbg x
        \\    dbg y
        \\    dbg z
        \\    x + y + z
        \\}
    ,
        &.{ dbg("1.0"), dbg("2.0"), dbg("3.0") },
        .returned,
    ),
    exprTest(
        "host effects: nested dbg calls",
        \\{
        \\    dbg(dbg(dbg(5)))
        \\}
    ,
        &.{ dbg("5.0"), dbg("{}"), dbg("{}") },
        .returned,
    ),
    exprTest(
        "host effects: dbg in if branch",
        \\{
        \\    x = 10
        \\    if x > 5 {
        \\        dbg "greater"
        \\        True
        \\    } else {
        \\        dbg "less or equal"
        \\        False
        \\    }
        \\}
    ,
        &.{dbg("\"greater\"")},
        .returned,
    ),
    exprTest(
        "host effects: dbg in match branch",
        \\{
        \\    x = 5
        \\    match x {
        \\        0 => {
        \\            dbg "zero"
        \\        }
        \\        _ => {
        \\            dbg "other"
        \\        }
        \\    }
        \\}
    ,
        &.{dbg("\"other\"")},
        .returned,
    ),
    exprTest(
        "host effects: dbg in for loop",
        \\{
        \\    items : List(I64)
        \\    items = [1, 2, 3]
        \\    for item in items {
        \\        dbg item
        \\    }
        \\    items
        \\}
    ,
        &.{ dbg("1"), dbg("2"), dbg("3") },
        .returned,
    ),
    exprTest(
        "host effects: dbg as final expression returns unit",
        \\{
        \\    dbg 42
        \\}
    ,
        &.{dbg("42.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg with arithmetic expression",
        \\{
        \\    dbg(2 + 3 * 4)
        \\}
    ,
        &.{dbg("14.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg inside function body",
        \\{
        \\    double = |x| {
        \\        dbg x
        \\        x * 2
        \\    }
        \\    double(21)
        \\}
    ,
        &.{dbg("21.0")},
        .returned,
    ),
    exprTest(
        "host effects: dbg function called multiple times",
        \\{
        \\    f = |x| {
        \\        dbg x
        \\        x
        \\    }
        \\    f(1) + f(2) + f(3)
        \\}
    ,
        &.{ dbg("1.0"), dbg("2.0"), dbg("3.0") },
        .returned,
    ),
    exprTest(
        "host effects: dbg string with special chars",
        \\{
        \\    dbg "hello\nworld"
        \\    "done"
        \\}
    ,
        &.{dbg(
            "\"hello\nworld\"",
        )},
        .returned,
    ),
    exprTest(
        "host effects: dbg large integer",
        \\{
        \\    x : I64
        \\    x = 9223372036854775807
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("9223372036854775807")},
        .returned,
    ),
    exprTest(
        "host effects: dbg variable after mutation in binding",
        \\{
        \\    x = 10
        \\    dbg x
        \\    y = x + 5
        \\    dbg y
        \\    y
        \\}
    ,
        &.{ dbg("10.0"), dbg("15.0") },
        .returned,
    ),
    exprTest(
        "host effects: dbg list of strings",
        \\{
        \\    xs = ["a", "b", "c"]
        \\    dbg xs
        \\    xs
        \\}
    ,
        &.{dbg("[\"a\", \"b\", \"c\"]")},
        .returned,
    ),
    exprTest(
        "host effects: issue 8729 tuple pattern var reassignment in while loop",
        \\{
        \\    get_pair = |n| ("word", n + 1)
        \\    var $index = 0
        \\    while $index < 3 {
        \\        (word, $index) = get_pair($index)
        \\        dbg word
        \\    }
        \\    $index
        \\}
    ,
        &.{ dbg("\"word\""), dbg("\"word\""), dbg("\"word\"") },
        .returned,
    ),

    // Additional fixed-ABI host-effects coverage.
    exprTest(
        "host effects: distinct dbg sites with identical bytes preserve order",
        \\{
        \\    dbg "same"
        \\    if True {
        \\        dbg "same"
        \\    }
        \\    0
        \\}
    ,
        &.{ dbg("\"same\""), dbg("\"same\"") },
        .returned,
    ),
    exprTest(
        "host effects: dbg before crash is preserved in order",
        \\{
        \\    dbg "before"
        \\    crash "boom"
        \\    0
        \\}
    ,
        &.{ dbg("\"before\""), crashed("boom") },
        .crashed,
    ),
    exprTest(
        "host effects: expect failure does not halt execution",
        \\{
        \\    expect 1 == 0
        \\    dbg "after"
        \\    0
        \\}
    ,
        &.{ expectFailed("expect failed"), dbg("\"after\"") },
        .returned,
    ),
    exprTest(
        "host effects: mixed dbg and expect ordering",
        \\{
        \\    dbg "before"
        \\    expect 1 == 0
        \\    dbg "after"
        \\    0
        \\}
    ,
        &.{ dbg("\"before\""), expectFailed("expect failed"), dbg("\"after\"") },
        .returned,
    ),
    exprTest(
        "host effects: repeated host effects from recursion",
        \\{
        \\    loop = |n| {
        \\        if n == 0 {
        \\            0
        \\        } else {
        \\            dbg "tick"
        \\            loop(n - 1)
        \\        }
        \\    }
        \\    loop(3)
        \\}
    ,
        &.{ dbg("\"tick\""), dbg("\"tick\""), dbg("\"tick\"") },
        .returned,
    ),
};
