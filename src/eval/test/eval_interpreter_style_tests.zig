//! Ported interpreter-style eval tests into the inspect-only runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{ .name = "interpreter: (|x| x)(\"Hello\") yields \"Hello\"", .source = "(|x| x)(\"Hello\")", .expected = .{ .inspect_str = "\"Hello\"" } },
    .{ .name = "interpreter: (|n| n + 1)(41) yields 42", .source = "(|n| n + 1)(41)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "interpreter: (|a, b| a + b)(40, 2) yields 42", .source = "(|a, b| a + b)(40, 2)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "interpreter: 6 / 3 yields 2", .source = "6 / 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "interpreter: 7 % 3 yields 1", .source = "7 % 3", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "interpreter: 0.2 + 0.3 yields 0.5", .source = "0.2 + 0.3", .expected = .{ .inspect_str = "0.5" } },
    .{ .name = "interpreter: 0.5 / 2 yields 0.25", .source = "0.5 / 2", .expected = .{ .inspect_str = "0.25" } },
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
    .{ .name = "interpreter: literal tag renders as tag name", .source = "MyTag", .expected = .{ .inspect_str = "MyTag" } },
    .{ .name = "interpreter: True == False yields False", .source = "True == False", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: \"hi\" == \"hi\" yields True", .source = "\"hi\" == \"hi\"", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: (1, 2) == (1, 2) yields True", .source = "(1, 2) == (1, 2)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: (1, 2) == (2, 1) yields False", .source = "(1, 2) == (2, 1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: { x: 1, y: 2 } == { y: 2, x: 1 } yields True", .source = "{ x: 1, y: 2 } == { y: 2, x: 1 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: { x: 1, y: 2 } == { x: 1, y: 3 } yields False", .source = "{ x: 1, y: 2 } == { x: 1, y: 3 }", .expected = .{ .inspect_str = "False" } },
    .{
        .name = "interpreter: record update copies base fields",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: point.y }
        \\    (updated.x, updated.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(1.0, 2.0)" },
    },
    .{
        .name = "interpreter: record update overrides field",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: 3 }
        \\    (updated.x, updated.y)
        \\}
        ,
        .expected = .{ .inspect_str = "(1.0, 3.0)" },
    },
    .{
        .name = "interpreter: record update expression can reference base",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: point.y + 5 }
        \\    updated.y
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
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
    .{ .name = "interpreter: Ok(1) == Ok(1) yields True", .source = "Ok(1) == Ok(1)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: Ok(1) == Err(1) yields False", .source = "Ok(1) == Err(1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: match tuple pattern destructures", .source = "match (1, 2) { (1, b) => b, _ => 0 }", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "interpreter: match bool patterns", .source = "match True { True => 1, False => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "interpreter: match result tag payload", .source = "match Ok(3) { Ok(n) => n + 1, Err(_) => 0 }", .expected = .{ .inspect_str = "4.0" } },
    .{ .name = "interpreter: match record destructures fields", .source = "match { x: 1, y: 2 } { { x, y } => x + y }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "interpreter: render Try.Ok literal", .source = "match True { True => Ok(42), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok(42.0)" } },
    .{ .name = "interpreter: render Try.Err string", .source = "match True { True => Err(\"boom\"), False => Ok(42) }", .expected = .{ .inspect_str = "Err(\"boom\")" } },
    .{ .name = "interpreter: render Try.Ok tuple payload", .source = "match True { True => Ok((1, 2)), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok((1.0, 2.0))" } },
    .{ .name = "interpreter: match tuple payload tag", .source = "match Ok((1, 2)) { Ok((a, b)) => a + b, Err(_) => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "interpreter: match record payload tag", .source = "match Err({ code: 1, msg: \"boom\" }) { Err({ code, msg: _msg }) => code, Ok(_) => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "interpreter: match list pattern destructures", .source = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }", .expected = .{ .inspect_str = "6.0" } },
    .{ .name = "interpreter: match list rest binds slice", .source = "match [1, 2, 3] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "interpreter: match empty list branch", .source = "match [] { [] => 42, _ => 0 }", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "interpreter: List.len on literal", .source = "List.len([1, 2, 3])", .expected = .{ .inspect_str = "3" } },
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
        .name = "interpreter: simple for loop sum",
        .source =
        \\{
        \\    var total = 0
        \\    for n in [1, 2, 3, 4] {
        \\        total = total + n
        \\    }
        \\    total
        \\}
        ,
        .expected = .{ .inspect_str = "10.0" },
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
    .{ .name = "interpreter: List.fold from Builtin using numbers", .source = "List.fold([1, 2, 3], 0, |acc, item| acc + item)", .expected = .{ .inspect_str = "6.0" } },
    .{ .name = "interpreter: List.any True on integers", .source = "List.any([1, 0, 1, 0, -1], |x| x > 0)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: List.any False on unsigned integers", .source = "List.any([9, 8, 7, 6, 5], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: List.any False on empty list", .source = "List.any([], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: List.all False when some elements are False", .source = "List.all([9, 18, 7, 6, 15], |x| x < 10)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: List.all True on small integers", .source = "List.all([9, 8, 7, 6, 5], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: List.all False on empty list", .source = "List.all([], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: List.contains is False for a missing element", .source = "List.contains([-1, -2, -3, 1, 2, 3], 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: List.contains is True when element is found", .source = "List.contains([1, 2, 3, 4, 5], 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: List.contains is False on empty list", .source = "List.contains([], 3333)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: empty record expression renders {}", .source = "{}", .expected = .{ .inspect_str = "{}" } },
    .{ .name = "interpreter: tuples and records", .source = "((1, 2), { x: 1, y: 2 })", .expected = .{ .inspect_str = "((1.0, 2.0), { x: 1.0, y: 2.0 })" } },
    .{
        .name = "interpreter: simple early return from function",
        .source =
        \\{
        \\    f = |x| if x { return True } else { False }
        \\    f(True)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "interpreter: any function with early return in for loop",
        .source =
        \\{
        \\    f = |list| {
        \\        for item in list {
        \\            if item == 2 {
        \\                return True
        \\            }
        \\        }
        \\        False
        \\    }
        \\    f([1, 2, 3])
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{ .name = "interpreter: decimal literal renders 0.125", .source = "0.125", .expected = .{ .inspect_str = "0.125" } },
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
    .{ .name = "interpreter: f64 equality True", .source = "3.25.F64 == 3.25.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: decimal equality True", .source = "0.125 == 0.125", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int and f64 equality True", .source = "1 == 1.0.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int and decimal equality True", .source = "1 == 1.0", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int less-than yields True", .source = "3 < 4", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int greater-than yields False", .source = "5 > 8", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: 0.1 + 0.2 yields 0.3", .source = "0.1 + 0.2", .expected = .{ .inspect_str = "0.3" } },
    .{ .name = "interpreter: f64 greater-than yields True", .source = "3.5.F64 > 1.25.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: decimal less-than-or-equal yields True", .source = "0.5 <= 0.5", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int and f64 less-than yields True", .source = "1 < 2.0.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: int and decimal greater-than yields False", .source = "3 > 5.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: bool inequality yields True", .source = "True != False", .expected = .{ .inspect_str = "True" } },
    .{ .name = "interpreter: decimal inequality yields False", .source = "0.5 != 0.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: f64 equality False", .source = "3.25.F64 == 4.0.F64", .expected = .{ .inspect_str = "False" } },
    .{ .name = "interpreter: decimal equality False", .source = "0.125 == 0.25", .expected = .{ .inspect_str = "False" } },
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
