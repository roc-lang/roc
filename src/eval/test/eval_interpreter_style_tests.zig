//! Ported interpreter-style eval tests into the inspect-only runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "interpreter: F64 addition",
        .source =
        \\{
        \\    a = 1.5.F64
        \\    b = 2.25.F64
        \\    a + b
        \\}
        ,
        .expected = .{ .inspect_str = "3.75" },
    },
    .{
        .name = "interpreter: F32 multiplication",
        .source =
        \\{
        \\    a = 1.5.F32
        \\    b = 2.0.F32
        \\    a * b
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "interpreter: F64 division",
        .source =
        \\{
        \\    a = 2.0.F64
        \\    b = 4.0.F64
        \\    a / b
        \\}
        ,
        .expected = .{ .inspect_str = "0.5" },
    },
    .{ .name = "interpreter: True == False yields False", .source = "True == False", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: \"hi\" == \"hi\" yields True", .source = "\"hi\" == \"hi\"", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: (1, 2) == (1, 2) yields True", .source = "(1, 2) == (1, 2)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: (1, 2) == (2, 1) yields False", .source = "(1, 2) == (2, 1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: { x: 1, y: 2 } == { y: 2, x: 1 } yields True", .source = "{ x: 1, y: 2 } == { y: 2, x: 1 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: { x: 1, y: 2 } == { x: 1, y: 3 } yields False", .source = "{ x: 1, y: 2 } == { x: 1, y: 3 }", .expected = .{ .inspect_str = "False" } },
    .{
        .name = "interpreter: record update can update multiple fields",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, x: 2, y: 3 }
        \\    (updated.x, updated.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(2.0, 3.0)" },
    },
    .{
        .name = "interpreter: record update inside tuple",
        .source =
        \\{
        \\    point = { x: 4, y: 5 }
        \\    duo = { updated: { ..point, y: point.y + 1 }, original: point }
        \\    (duo.updated.x, duo.updated.y, duo.original.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(4.0, 6.0, 5.0)" },
    },
    .{
        .name = "interpreter: record update pattern match",
        .source =
        \\{
        \\    point = { x: 7, y: 8 }
        \\    updated = { ..point, y: point.y - 2 }
        \\    match updated { { x: newX, y: newY } => (newX, newY), _ => (0, 0) }
        \\}
        ,
        .expected = .{ .inspect_str = "(7.0, 6.0)" },
    },
    .{ .name = "interpreter: [1, 2, 3] == [1, 2, 3] yields True", .source = "[1, 2, 3] == [1, 2, 3]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: [1, 2, 3] == [1, 3, 2] yields False", .source = "[1, 2, 3] == [1, 3, 2]", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: Ok(1) == Err(1) yields False", .source = "Ok(1) == Err(1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: match empty list branch", .source = "match [] { [] => 42, _ => 0 }", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "interpreter: List.map with U64.from_str", .source = "List.map([\"2022\", \"22\"], U64.from_str)", .expected = .{ .inspect_str = "[Ok(2022), Ok(22)]" } },
    .{
        .name = "interpreter: map2 record builder drops intermediate concat result",
        .source =
        \\{
        \\    map2 = |ca, cb, f| {
        \\        value: f(ca.value, cb.value),
        \\        help: Str.concat(ca.help, cb.help),
        \\    }
        \\    option = |name, default| {
        \\        value: default,
        \\        help: "  --${name} <value>",
        \\    }
        \\    get_help = |c| c.help
        \\    p1 = option("a", "1")
        \\    p2 = option("b", "2")
        \\    get_help(map2(p1, p2, |a, b| { a, b }))
        \\}
        ,
        .expected = .{ .inspect_str = "\"  --a <value>  --b <value>\"" },
    },
    .{
        .name = "interpreter: projecting value from owned aggregate drops sibling help",
        .source =
        \\{
        \\    map2 = |ca, cb, f| {
        \\        value: f(ca.value, cb.value),
        \\        help: Str.concat(ca.help, cb.help),
        \\    }
        \\    option = |name, default| {
        \\        value: default,
        \\        help: "  --${name} <value>",
        \\    }
        \\    run = |c| c.value
        \\    p1 = option("a", "1")
        \\    p2 = option("b", "2")
        \\    run(map2(p1, p2, |a, b| { a, b }))
        \\}
        ,
        .expected = .{ .inspect_str = "{ a: \"1\", b: \"2\" }" },
    },
    .{
        .name = "interpreter: List.fold sum with inline lambda",
        .source =
        \\(|list, init, step| {
        \\    var state = init
        \\    for item in list {
        \\        state = step(state, item)
        \\    }
        \\    state
        \\})([1, 2, 3, 4], 0, |acc, x| acc + x)
        ,
        .expected = .{ .inspect_str = "10.0" },
    },
    .{
        .name = "interpreter: List.fold product with inline lambda",
        .source =
        \\(|list, init, step| {
        \\    var state = init
        \\    for item in list {
        \\        state = step(state, item)
        \\    }
        \\    state
        \\})([2, 3, 4], 1, |acc, x| acc * x)
        ,
        .expected = .{ .inspect_str = "24.0" },
    },
    .{
        .name = "interpreter: List.fold empty list with inline lambda",
        .source =
        \\(|list, init, step| {
        \\    var state = init
        \\    for item in list {
        \\        state = step(state, item)
        \\    }
        \\    state
        \\})([], 42, |acc, x| acc + x)
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter: List.fold count elements with inline lambda",
        .source =
        \\(|list, init, step| {
        \\    var state = init
        \\    for item in list {
        \\        state = step(state, item)
        \\    }
        \\    state
        \\})([10, 20, 30, 40], 0, |acc, _| acc + 1)
        ,
        .expected = .{ .inspect_str = "4.0" },
    },
    .{
        .name = "interpreter: recursive function with var does not clobber outer call's binding",
        .source =
        \\{
        \\    f = |n| {
        \\        var state = n
        \\        if n > 0 {
        \\            inner = f(n - 1)
        \\            state + inner
        \\        } else {
        \\            state
        \\        }
        \\    }
        \\    f(3)
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{ .name = "interpreter: tuples and records", .source = "((1, 2), { x: 1, y: 2 })", .expected = .{ .inspect_str = "((1.0, 2.0), { x: 1.0, y: 2.0 })" } },
    .{
        .name = "interpreter: F64 literal",
        .source =
        \\{
        \\    a : F64
        \\    a = 3.25
        \\    a
        \\}
        ,
        .expected = .{ .inspect_str = "3.25" },
    },
    .{ .name = "interpreter: f64 is_float_eq True", .source = "F64.is_float_eq(3.25.F64, 3.25.F64)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: decimal equality True", .source = "0.125 == 0.125", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "interpreter: simple break inside for loop",
        .source =
        \\{
        \\    var sum = 0
        \\    for i in [1, 2, 3, 4, 5] {
        \\        if i == 4 {
        \\            break
        \\        }
        \\        sum = sum + i
        \\    }
        \\    sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "interpreter: simple break inside while loop",
        .source =
        \\{
        \\    var i = 1
        \\    var sum = 0
        \\    while i <= 5 {
        \\        if i == 4 {
        \\            break
        \\        }
        \\        sum = sum + i
        \\        i = i + 1
        \\    }
        \\    sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "issue 8729: var reassignment in tuple pattern in while loop",
        .source =
        \\{
        \\    get_pair = |n| ("word", n + 1)
        \\    var index = 0
        \\    while index < 3 {
        \\        (word, index) = get_pair(index)
        \\        dbg word
        \\    }
        \\    index
        \\}
        ,
        .expected = .{ .inspect_str = "3.0" },
    },
};
