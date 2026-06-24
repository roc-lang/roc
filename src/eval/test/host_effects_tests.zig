//! Runtime host-effects tests over the fixed production `host_abi` contract.

const eval = @import("eval");
const TestCase = @import("host_effects_runner.zig").TestCase;
const Termination = eval.RuntimeHostEnv.Termination;

fn dbg(bytes: []const u8) TestCase.ExpectedEvent {
    return .{ .dbg = bytes };
}

fn dbgContains(bytes: []const u8) TestCase.ExpectedEvent {
    return .{ .dbg_contains = bytes };
}

fn dbgAny() TestCase.ExpectedEvent {
    return .{ .dbg_any = {} };
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

fn exprTestWithLiveAllocations(
    name: []const u8,
    source: []const u8,
    expected_events: []const TestCase.ExpectedEvent,
    expected_termination: Termination,
    expected_live_allocations: u32,
) TestCase {
    return .{
        .name = name,
        .source = source,
        .expected_events = expected_events,
        .expected_termination = expected_termination,
        .expected_live_allocations = expected_live_allocations,
    };
}

fn moduleTestWithLiveAllocations(
    name: []const u8,
    source: []const u8,
    expected_events: []const TestCase.ExpectedEvent,
    expected_termination: Termination,
    expected_live_allocations: u32,
) TestCase {
    return .{
        .name = name,
        .source = source,
        .source_kind = .module,
        .expected_events = expected_events,
        .expected_termination = expected_termination,
        .expected_live_allocations = expected_live_allocations,
    };
}

/// Public value `tests`.
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
        \\    f : Dec -> Dec
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
        \\    x = 42.I64
        \\    dbg x
        \\    x + 1.I64
        \\}
    ,
        &.{dbg("42")},
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
        \\    dbg 42.I64
        \\    123.I64
        \\}
    ,
        &.{dbg("42")},
        .returned,
    ),
    exprTest(
        "host effects: dbg negative integer",
        \\{
        \\    x : I64
        \\    x = -99
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("-99")},
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
        \\    t = (1.I64, "two", 3.I64)
        \\    dbg t
        \\    t
        \\}
    ,
        &.{dbg("(1, \"two\", 3)")},
        .returned,
    ),
    exprTest(
        "host effects: dbg record",
        \\{
        \\    r = { name: "Alice", age: 30.I64 }
        \\    dbg r
        \\    r
        \\}
    ,
        &.{dbg("{ age: 30, name: \"Alice\" }")},
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
        \\    x = Ok(42.I64)
        \\    dbg x
        \\    match x { Ok(n) => n, Err(_) => 0.I64 }
        \\}
    ,
        &.{dbg("Ok(42)")},
        .returned,
    ),
    exprTest(
        "host effects: dbg function value",
        \\{
        \\    f : I64 -> I64
        \\    f = |x| x + 1.I64
        \\    dbg f
        \\    f(5.I64)
        \\}
    ,
        &.{dbg("<function>")},
        .returned,
    ),
    exprTest(
        "host effects: dbg expression form returns unit",
        \\{
        \\    x = 42.I64
        \\    dbg x
        \\    x + 1.I64
        \\}
    ,
        &.{dbg("42")},
        .returned,
    ),
    exprTest(
        "host effects: multiple dbg calls in sequence",
        \\{
        \\    x = 1.I64
        \\    y = 2.I64
        \\    z = 3.I64
        \\    dbg x
        \\    dbg y
        \\    dbg z
        \\    x + y + z
        \\}
    ,
        &.{ dbg("1"), dbg("2"), dbg("3") },
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
        \\    double : I64 -> I64
        \\    double = |x| {
        \\        dbg x
        \\        x * 2.I64
        \\    }
        \\    double(21.I64)
        \\}
    ,
        &.{dbg("21")},
        .returned,
    ),
    exprTest(
        "host effects: dbg function called multiple times",
        \\{
        \\    f : I64 -> I64
        \\    f = |x| {
        \\        dbg x
        \\        x
        \\    }
        \\    f(1.I64) + f(2.I64) + f(3.I64)
        \\}
    ,
        &.{ dbg("1"), dbg("2"), dbg("3") },
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
        \\    x = 10.I64
        \\    dbg x
        \\    y = x + 5.I64
        \\    dbg y
        \\    y
        \\}
    ,
        &.{ dbg("10"), dbg("15") },
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
        \\    get_pair : I64 -> (Str, I64)
        \\    get_pair = |n| ("word", n + 1.I64)
        \\    var $index = 0.I64
        \\    while $index < 3.I64 {
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
        \\    {}
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
        \\    {}
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
        \\    {}
        \\}
    ,
        &.{ dbg("\"before\""), expectFailed("expect failed"), dbg("\"after\"") },
        .returned,
    ),
    exprTest(
        "host effects: repeated host effects from recursion",
        \\{
        \\    loop : I64 -> I64
        \\    loop = |n| {
        \\        if n == 0.I64 {
        \\            0.I64
        \\        } else {
        \\            dbg "tick"
        \\            loop(n - 1.I64)
        \\        }
        \\    }
        \\    loop(3.I64)
        \\}
    ,
        &.{ dbg("\"tick\""), dbg("\"tick\""), dbg("\"tick\"") },
        .returned,
    ),
    // Legacy interpreter_style_test names preserved.
    exprTest(
        "interpreter: crash statement triggers crash error and message",
        \\{
        \\    crash "boom"
        \\    0
        \\}
    ,
        &.{crashed("boom")},
        .crashed,
    ),
    exprTest(
        "interpreter: crash at end of block in if branch",
        \\{
        \\    f : Dec -> Dec
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
        "interpreter: expect expression succeeds",
        \\{
        \\    expect 1 == 1
        \\    {}
        \\}
    ,
        &.{},
        .returned,
    ),
    exprTest(
        "interpreter: expect expression failure crashes with message",
        \\{
        \\    expect 1 == 0
        \\    {}
        \\}
    ,
        &.{expectFailed("expect failed")},
        .returned,
    ),
    exprTest(
        "interpreter: dbg statement in block",
        \\{
        \\    x = 42.I64
        \\    dbg x
        \\    x + 1.I64
        \\}
    ,
        &.{dbg("42")},
        .returned,
    ),
    exprTest(
        "interpreter: dbg statement with string",
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
        "debug List.len expression",
        \\{
        \\    dbg List.len([1.I64, 2.I64, 3.I64])
        \\    {}
        \\}
    ,
        &.{dbg("3")},
        .returned,
    ),
    exprTest(
        "dbg: integer literal",
        \\{
        \\    dbg 42.I64
        \\    123.I64
        \\}
    ,
        &.{dbg("42")},
        .returned,
    ),
    exprTest(
        "dbg: negative integer",
        \\{
        \\    x : I64
        \\    x = -99
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbg("-99")},
        .returned,
    ),
    exprTest(
        "dbg: float value",
        \\{
        \\    x : F64
        \\    x = 3.14
        \\    dbg x
        \\    x
        \\}
    ,
        &.{dbgContains("3.14")},
        .returned,
    ),
    exprTest(
        "dbg: boolean True",
        \\{
        \\    dbg True
        \\    False
        \\}
    ,
        &.{dbg("True")},
        .returned,
    ),
    exprTest(
        "dbg: boolean False",
        \\{
        \\    dbg False
        \\    True
        \\}
    ,
        &.{dbg("False")},
        .returned,
    ),
    exprTest(
        "dbg: empty string",
        \\{
        \\    dbg ""
        \\    "done"
        \\}
    ,
        &.{dbg("\"\"")},
        .returned,
    ),
    exprTest(
        "dbg: list of integers",
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
        "dbg: list of strings",
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
        "dbg: tuple",
        \\{
        \\    t = (1.I64, "two", 3.I64)
        \\    dbg t
        \\    t
        \\}
    ,
        &.{dbg("(1, \"two\", 3)")},
        .returned,
    ),
    exprTest(
        "dbg: record",
        \\{
        \\    r = { name: "Alice", age: 30.I64 }
        \\    dbg r
        \\    r
        \\}
    ,
        &.{dbg("{ age: 30, name: \"Alice\" }")},
        .returned,
    ),
    exprTest(
        "dbg: empty record",
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
        "dbg: tag without payload",
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
        "dbg: tag with payload",
        \\{
        \\    x = Ok(42.I64)
        \\    dbg x
        \\    match x { Ok(n) => n, Err(_) => 0.I64 }
        \\}
    ,
        &.{dbg("Ok(42)")},
        .returned,
    ),
    exprTest(
        "dbg: function prints as unsupported or function marker",
        \\{
        \\    f : I64 -> I64
        \\    f = |x| x + 1.I64
        \\    dbg f
        \\    f(5.I64)
        \\}
    ,
        &.{dbgAny()},
        .returned,
    ),
    exprTest(
        "dbg: expression form returns unit",
        \\{
        \\    x = 42.I64
        \\    dbg x
        \\    x + 1.I64
        \\}
    ,
        &.{dbg("42")},
        .returned,
    ),
    exprTest(
        "dbg: multiple dbg calls in sequence",
        \\{
        \\    x = 1.I64
        \\    y = 2.I64
        \\    z = 3.I64
        \\    dbg x
        \\    dbg y
        \\    dbg z
        \\    x + y + z
        \\}
    ,
        &.{ dbg("1"), dbg("2"), dbg("3") },
        .returned,
    ),
    exprTest(
        "dbg: nested dbg calls",
        \\{
        \\    dbg(dbg(dbg(5)))
        \\}
    ,
        &.{ dbg("5.0"), dbg("{}"), dbg("{}") },
        .returned,
    ),
    exprTest(
        "dbg: in if-then-else branch",
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
        "dbg: in match pattern",
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
        "dbg: in for loop",
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
        "dbg: as final expression returns unit",
        \\{
        \\    dbg 42
        \\}
    ,
        &.{dbg("42.0")},
        .returned,
    ),
    exprTest(
        "dbg: with arithmetic expression",
        \\{
        \\    dbg(2 + 3 * 4)
        \\}
    ,
        &.{dbg("14.0")},
        .returned,
    ),
    exprTest(
        "dbg: inside function body",
        \\{
        \\    double : I64 -> I64
        \\    double = |x| {
        \\        dbg x
        \\        x * 2.I64
        \\    }
        \\    double(21.I64)
        \\}
    ,
        &.{dbg("21")},
        .returned,
    ),
    exprTest(
        "dbg: function called multiple times",
        \\{
        \\    f : I64 -> I64
        \\    f = |x| {
        \\        dbg x
        \\        x
        \\    }
        \\    f(1.I64) + f(2.I64) + f(3.I64)
        \\}
    ,
        &.{ dbg("1"), dbg("2"), dbg("3") },
        .returned,
    ),
    exprTest(
        "dbg: with string containing special chars",
        \\{
        \\    dbg "hello\nworld"
        \\    "done"
        \\}
    ,
        &.{dbgContains("hello\nworld")},
        .returned,
    ),
    exprTest(
        "dbg: large integer",
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
        "dbg: variable after mutation in binding",
        \\{
        \\    x = 10.I64
        \\    dbg x
        \\    y = x + 5.I64
        \\    dbg y
        \\    y
        \\}
    ,
        &.{ dbg("10"), dbg("15") },
        .returned,
    ),
    exprTestWithLiveAllocations(
        "rc balance: nested list of heap strings is fully released",
        \\{
        \\    nested = [
        \\        [
        \\            "alpha string definitely long enough to allocate outside small-string storage",
        \\            "beta string definitely long enough to allocate outside small-string storage",
        \\        ],
        \\        [],
        \\        [
        \\            "gamma string definitely long enough to allocate outside small-string storage",
        \\        ],
        \\    ]
        \\    expect List.len(nested) == 3
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    exprTestWithLiveAllocations(
        "rc balance: aliased list inside record is released once per live reference",
        \\{
        \\    items = [
        \\        "shared first string definitely long enough to allocate outside small-string storage",
        \\        "shared second string definitely long enough to allocate outside small-string storage",
        \\    ]
        \\    record = { left: items, right: items }
        \\    expect List.len(record.left) == 2
        \\    expect List.len(record.right) == 2
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    exprTestWithLiveAllocations(
        "rc balance: boxed list of heap strings is released after unbox",
        \\{
        \\    boxed = Box.box([
        \\        "boxed first string definitely long enough to allocate outside small-string storage",
        \\        "boxed second string definitely long enough to allocate outside small-string storage",
        \\    ])
        \\    items = Box.unbox(boxed)
        \\    expect List.len(items) == 2
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    exprTestWithLiveAllocations(
        "rc balance: closure capture containing heap data is released",
        \\{
        \\    prefix = "captured prefix string definitely long enough to allocate outside small-string storage"
        \\    suffix = " captured suffix string definitely long enough to allocate outside small-string storage"
        \\    f = |tail| Str.concat(Str.concat(prefix, suffix), tail)
        \\    expect f("!") == "captured prefix string definitely long enough to allocate outside small-string storage captured suffix string definitely long enough to allocate outside small-string storage!"
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    exprTestWithLiveAllocations(
        "rc balance: boxed callable capture containing nested heap data is released",
        \\{
        \\    record = {
        \\        label: "boxed callable captured label definitely long enough to allocate outside small-string storage",
        \\        items: [
        \\            "boxed callable first item definitely long enough to allocate outside small-string storage",
        \\            "boxed callable second item definitely long enough to allocate outside small-string storage",
        \\        ],
        \\    }
        \\    boxed : Box((I64 -> I64))
        \\    boxed = Box.box(|x| x + List.len(record.items).to_i64_wrap())
        \\    f = Box.unbox(boxed)
        \\    expect f(40) == 42
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    exprTestWithLiveAllocations(
        "rc balance: boxed callable capture containing another boxed callable is released",
        \\{
        \\    inner = Box.box(|x| x + 1)
        \\    outer = Box.box(|x| {
        \\        f = Box.unbox(inner)
        \\
        \\        f(x) + 1
        \\    })
        \\    f = Box.unbox(outer)
        \\    expect f(40) == 42
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
    moduleTestWithLiveAllocations(
        "rc balance: recursive tag union with boxed children is fully released",
        \\Tree := [Leaf(Str), Node(Box(Tree), Box(Tree))]
        \\
        \\count : Tree -> I64
        \\count = |tree| match tree {
        \\    Leaf(_) => 1
        \\    Node(left, right) => count(Box.unbox(left)) + count(Box.unbox(right))
        \\}
        \\
        \\main = || {
        \\    tree = Node(
        \\        Box.box(Leaf("left recursive string definitely long enough to allocate outside small-string storage")),
        \\        Box.box(Node(
        \\            Box.box(Leaf("middle recursive string definitely long enough to allocate outside small-string storage")),
        \\            Box.box(Leaf("right recursive string definitely long enough to allocate outside small-string storage")),
        \\        )),
        \\    )
        \\    expect count(tree) == 3
        \\    {}
        \\}
    ,
        &.{},
        .returned,
        0,
    ),
};
