//! Data-driven eval test definitions for the inspect-only parallel runner.

const TestCase = @import("parallel_runner.zig").TestCase;
const closure_recursion_tests = @import("eval_closure_recursion_tests.zig");
const interpreter_style_tests = @import("eval_interpreter_style_tests.zig");
const low_level_tests = @import("eval_low_level_tests.zig");
const polymorphism_tests = @import("eval_polymorphism_tests.zig");
const recursive_data_tests = @import("eval_recursive_data_tests.zig");

/// All eval test cases, consumed by the parallel runner.
///
/// Every value-producing test is observed solely through `Str.inspect(...)`.
const core_tests = [_]TestCase{
    // Frontend problems
    .{ .name = "problem: undefined variable", .source = "undefinedVar", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec plus int type mismatch", .source = "1.0.Dec + 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec minus int type mismatch", .source = "1.0.Dec - 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec times int type mismatch", .source = "1.0.Dec * 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: dec div int type mismatch", .source = "1.0.Dec / 2.I64", .expected = .{ .problem = {} } },
    .{ .name = "problem: int plus dec type mismatch", .source = "1.I64 + 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int minus dec type mismatch", .source = "1.I64 - 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int times dec type mismatch", .source = "1.I64 * 2.0.Dec", .expected = .{ .problem = {} } },
    .{ .name = "problem: int div dec type mismatch", .source = "1.I64 / 2.0.Dec", .expected = .{ .problem = {} } },

    // Basic expressions and control flow
    .{ .name = "inspect: integer literal", .source = "42", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: negative integer literal", .source = "-1234", .expected = .{ .inspect_str = "-1234.0" } },
    .{ .name = "inspect: decimal literal", .source = "1.5", .expected = .{ .inspect_str = "1.5" } },
    .{ .name = "inspect: boolean true", .source = "True", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: boolean false", .source = "False", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: string literal", .source = "\"hello\"", .expected = .{ .inspect_str = "\"hello\"" } },
    .{ .name = "inspect: empty string literal", .source = "\"\"", .expected = .{ .inspect_str = "\"\"" } },
    .{ .name = "inspect: arithmetic", .source = "2 + 3 * 4", .expected = .{ .inspect_str = "14.0" } },
    .{ .name = "inspect: subtraction", .source = "5 - 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: multiplication", .source = "4 * 5", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: integer division", .source = "10 // 2", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: modulo", .source = "7 % 3", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: if expression", .source = "if (1 == 1) 42 else 99", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: if false branch", .source = "if (1 == 2) 42 else 99", .expected = .{ .inspect_str = "99.0" } },
    .{ .name = "inspect: nested if inner true", .source = "if (1 == 1) (if (2 == 2) 100 else 200) else 300", .expected = .{ .inspect_str = "100.0" } },
    .{ .name = "inspect: nested if inner false", .source = "if (1 == 1) (if (2 == 3) 100 else 200) else 300", .expected = .{ .inspect_str = "200.0" } },
    .{ .name = "inspect: nested if outer false", .source = "if (1 == 2) (if (2 == 2) 100 else 200) else 300", .expected = .{ .inspect_str = "300.0" } },
    .{ .name = "inspect: tuple literal", .source = "(1, 2)", .expected = .{ .inspect_str = "(1.0, 2.0)" } },
    .{ .name = "inspect: tuple arithmetic", .source = "(5 + 1, 5 * 3)", .expected = .{ .inspect_str = "(6.0, 15.0)" } },
    .{ .name = "inspect: record field", .source = "{ x: 42, y: 99 }.x", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: record field second", .source = "{ x: 10, y: 20 }.y", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: nested record access", .source = "{ outer: { inner: 42 } }.outer.inner", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: deeper nested record access", .source = "{ a: { b: { c: 100 } } }.a.b.c", .expected = .{ .inspect_str = "100.0" } },
    .{ .name = "inspect: record field order independence", .source = "{ x: 1, y: 2 }.x + { y: 2, x: 1 }.x", .expected = .{ .inspect_str = "2.0" } },
    .{
        .name = "inspect: mixed-alignment record survives closure and list round trip",
        .source =
        \\{
        \\    make = || { a: 1.U8, b: 2.U64, c: 3.U16, d: True, e: 4.U8 }
        \\    wrap = |value| { items: [value], keep: value }
        \\    wrapped = wrap(make())
        \\    wrapped.items == [wrapped.keep]
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: function returning record with dec field",
        .source =
        \\{
        \\    make = |n| { x: n }
        \\    make(5).x
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: function returning nested record with dec field",
        .source =
        \\{
        \\    make = |n| { outer: { inner: n } }
        \\    make(5).outer.inner
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{ .name = "inspect: unary minus literal", .source = "-5", .expected = .{ .inspect_str = "-5.0" } },
    .{ .name = "inspect: unary minus nested", .source = "-(-10)", .expected = .{ .inspect_str = "10.0" } },
    .{ .name = "inspect: unary minus arithmetic", .source = "-(3 + 4)", .expected = .{ .inspect_str = "-7.0" } },
    .{ .name = "inspect: precedence with parentheses", .source = "(2 + 3) * 4", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: subtraction associativity", .source = "100 - 20 - 10", .expected = .{ .inspect_str = "70.0" } },
    .{ .name = "inspect: subtraction grouping", .source = "100 - (20 - 10)", .expected = .{ .inspect_str = "90.0" } },
    .{ .name = "inspect: mixed add subtract associativity", .source = "1 - 2 + 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: chained multiplication", .source = "2 * 3 * 4 * 5", .expected = .{ .inspect_str = "120.0" } },
    .{ .name = "inspect: integer division associativity", .source = "80 // 8 // 2", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: integer division grouping", .source = "80 // (8 // 2)", .expected = .{ .inspect_str = "20.0" } },
    .{ .name = "inspect: modulo associativity", .source = "100 % 30 % 7", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: modulo grouping", .source = "100 % (30 % 7)", .expected = .{ .inspect_str = "0.0" } },
    .{ .name = "inspect: hexadecimal literal", .source = "0xFF", .expected = .{ .inspect_str = "255.0" } },
    .{ .name = "inspect: binary literal", .source = "0b1010", .expected = .{ .inspect_str = "10.0" } },
    .{
        .name = "inspect: interpolation",
        .source =
        \\{
        \\    hello = "Hello"
        \\    world = "World"
        \\    "${hello} ${world}"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{ .name = "inspect: large string literal", .source = "\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"", .expected = .{ .inspect_str = "\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"" } },
    .{ .name = "inspect: small string literal", .source = "\"Small string test\"", .expected = .{ .inspect_str = "\"Small string test\"" } },
    .{ .name = "inspect: conditional string", .source = "if True \"This is a large string that exceeds small string optimization\" else \"Short\"", .expected = .{ .inspect_str = "\"This is a large string that exceeds small string optimization\"" } },
    .{ .name = "inspect: nested conditional string", .source = "if True (if False \"Inner small\" else \"Inner large string that exceeds small string optimization\") else \"Outer\"", .expected = .{ .inspect_str = "\"Inner large string that exceeds small string optimization\"" } },
    .{ .name = "inspect: record field small string", .source = "{ foo: \"Hello\" }.foo", .expected = .{ .inspect_str = "\"Hello\"" } },
    .{ .name = "inspect: record field large string", .source = "{ foo: \"This is a very long string that definitely exceeds the small string optimization limit\" }.foo", .expected = .{ .inspect_str = "\"This is a very long string that definitely exceeds the small string optimization limit\"" } },

    // Equality and mutable record cases
    .{ .name = "inspect: empty record equality", .source = "{} == {}", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: mutable record equality",
        .source =
        \\{
        \\    var $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutable record rebind equality",
        .source =
        \\{
        \\    var $x = { sum: 0 }
        \\    $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutable record loop accumulator equality",
        .source =
        \\{
        \\    var $acc = { sum: 0 }
        \\    for item in [1, 2, 3] {
        \\        $acc = { sum: $acc.sum + item }
        \\    }
        \\    $acc == { sum: 6 }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{ .name = "inspect: string field equality true", .source = "{ name: \"hello\" } == { name: \"hello\" }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: string field equality false", .source = "{ name: \"hello\" } == { name: \"world\" }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: nested record equality true", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 1 }, b: 2 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: nested record equality false", .source = "{ a: { x: 1 }, b: 2 } == { a: { x: 2 }, b: 2 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: nested tuple equality true", .source = "((1, 2), 3) == ((1, 2), 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: nested tuple equality false", .source = "(1, (2, 3)) == (1, (2, 9))", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag no payload ok ok", .source = "Ok == Ok", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag no payload err err", .source = "Err == Err", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag no payload ok err", .source = "Ok == Err", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag no payload err ok", .source = "Err == Ok", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality same tag with payload equal", .source = "Ok(1) == Ok(1)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality same tag with payload not equal", .source = "Ok(1) == Ok(2)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union equality err payload equal", .source = "Err(1) == Err(1)", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: tag union equality different tags with payload",
        .source =
        \\{
        \\    x = Ok(1)
        \\    y = if Bool.False Ok(1) else Err(1)
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{ .name = "inspect: tag union match direct numeric payload", .source = "match Ok(10) { Ok(n) => n + 5, Err(_) => 0 }", .expected = .{ .inspect_str = "15.0" } },
    .{ .name = "inspect: tag union match direct record payload", .source = "match Ok({ value: 10 }) { Ok({ value }) => value + 5, Err(_) => 0 }", .expected = .{ .inspect_str = "15.0" } },
    .{ .name = "inspect: tag union equality string payloads equal", .source = "Ok(\"hello\") == Ok(\"hello\")", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union equality string payloads not equal", .source = "Ok(\"hello\") == Ok(\"world\")", .expected = .{ .inspect_str = "False" } },
    .{
        .name = "inspect: tag union equality three or more tags equal direct",
        .source =
        \\{
        \\    x = Red
        \\    y = Red
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: tag union equality three or more tags equal through if",
        .source =
        \\{
        \\    x = Red
        \\    y = if Bool.True Red else if Bool.True Green else Blue
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: tag union equality three or more tags not equal",
        .source =
        \\{
        \\    x = Red
        \\    y = if Bool.False Red else Green
        \\    x == y
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{ .name = "inspect: record inequality equal records", .source = "{ x: 1, y: 2 } != { x: 1, y: 2 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record inequality different records", .source = "{ x: 1, y: 2 } != { x: 1, y: 3 }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record inequality reordered equal records", .source = "{ x: 1, y: 2 } != { y: 2, x: 1 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tuple inequality equal tuples", .source = "(1, 2) != (1, 2)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tuple inequality different tuples", .source = "(1, 2) != (1, 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union inequality same tag no payload", .source = "Ok != Ok", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union inequality different tags no payload", .source = "Ok != Err", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: tag union inequality same payload", .source = "Ok(1) != Ok(1)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: tag union inequality different payload", .source = "Ok(1) != Ok(2)", .expected = .{ .inspect_str = "True" } },
    .{
        .name = "inspect: match with tag containing pattern-bound variable regression",
        .source =
        \\match Some("x") {
        \\    Some(a) => Tagged(a)
        \\    None => Tagged("")
        \\}
        ,
        .expected = .{ .inspect_str = "Tagged(\"x\")" },
    },
    .{
        .name = "inspect: nested match with Result type regression",
        .source =
        \\match ["x"] {
        \\    [a] => {
        \\        match Ok(a) {
        \\            Ok(val) => Ok(val),
        \\            _ => Err(Oops)
        \\        }
        \\    }
        \\    _ => Err(Oops)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok(\"x\")" },
    },
    .{ .name = "inspect: list equality single element regression", .source = "[1] == [1]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: list equality nested lists regression", .source = "[[1, 2]] == [[1, 2]]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: list equality single string element regression", .source = "[\"hello\"] == [\"hello\"]", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality unequal length regression", .source = "{ a: [1] } == { a: [1, 2] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality unequal values regression", .source = "{ a: [1] } == { a: [2] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality empty vs singleton regression", .source = "{ a: [] } == { a: [1] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality singleton vs empty regression", .source = "{ a: [1] } == { a: [] }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list equality mixed fields regression", .source = "{ a: [], b: 1 } == { a: [2], b: 1 }", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: record with list inequality unequal length regression", .source = "{ a: [1] } != { a: [1, 2] }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality equal singleton regression", .source = "{ a: [1] } == { a: [1] }", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: record with list equality equal empty regression", .source = "{ a: [] } == { a: [] }", .expected = .{ .inspect_str = "True" } },

    // Typed lambdas and captures from the old eval suite
    .{ .name = "inspect: typed simple lambda increment", .source = "(|x| x + 1.I64)(5.I64)", .expected = .{ .inspect_str = "6" } },
    .{ .name = "inspect: typed simple lambda arithmetic", .source = "(|x| x * 2.I64 + 1.I64)(10.I64)", .expected = .{ .inspect_str = "21" } },
    .{ .name = "inspect: typed multi parameter lambda", .source = "(|x, y| x + y)(3.I64, 4.I64)", .expected = .{ .inspect_str = "7" } },
    .{ .name = "inspect: typed three parameter lambda", .source = "(|a, b, c| a + b + c)(1.I64, 2.I64, 3.I64)", .expected = .{ .inspect_str = "6" } },
    .{ .name = "inspect: typed lambda if body positive", .source = "(|x| if x > 0.I64 x else 0.I64)(5.I64)", .expected = .{ .inspect_str = "5" } },
    .{ .name = "inspect: typed lambda if body negative", .source = "(|x| if x > 0.I64 x else 0.I64)(-3.I64)", .expected = .{ .inspect_str = "0" } },
    .{
        .name = "inspect: crash not taken when lambda condition true",
        .source =
        \\(|x| if x > 0.I64 x else {
        \\    crash "this should not execute"
        \\    0.I64
        \\})(10.I64)
        ,
        .expected = .{ .inspect_str = "10" },
    },
    .{
        .name = "crash: lambda else branch crash",
        .source =
        \\(|x| if x > 0.I64 x else {
        \\    crash "crash in else!"
        \\    0.I64
        \\})(-5.I64)
        ,
        .expected = .{ .crash = {} },
    },
    .{ .name = "inspect: typed lambda unary minus", .source = "(|x| -x)(5.I64)", .expected = .{ .inspect_str = "-5" } },
    .{ .name = "inspect: typed lambda ignore arg", .source = "(|_x| 5.I64)(99.I64)", .expected = .{ .inspect_str = "5" } },
    .{ .name = "inspect: typed curried lambda", .source = "(|a| |b| a * b)(5.I64)(10.I64)", .expected = .{ .inspect_str = "50" } },
    .{ .name = "inspect: typed triple curried lambda", .source = "(((|a| |b| |c| a + b + c)(100.I64))(20.I64))(3.I64)", .expected = .{ .inspect_str = "123" } },
    .{ .name = "inspect: typed multi-arg lambda returning lambda", .source = "(|a, b, c| |d| a + b + c + d)(10.I64, 20.I64, 5.I64)(7.I64)", .expected = .{ .inspect_str = "42" } },
    .{ .name = "inspect: typed nested captures", .source = "(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)", .expected = .{ .inspect_str = "6" } },
    .{
        .name = "inspect: typed captured lambda",
        .source =
        \\{
        \\    x = 10.I64
        \\    f = |y| x + y
        \\    f(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "15" },
    },
    .{
        .name = "inspect: typed captured lambda multiple vars",
        .source =
        \\{
        \\    x = 20.I64
        \\    y = 30.I64
        \\    f = |z| x + y + z
        \\    f(10.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: typed lambda many captures",
        .source =
        \\{
        \\    a = 100.I64
        \\    b = 200.I64
        \\    c = 300.I64
        \\    d = 400.I64
        \\    f = |n| a + b + c + d + n
        \\    f(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "1005" },
    },
    .{
        .name = "inspect: typed nested closure blocks",
        .source =
        \\(((|a| {
        \\    a_loc = a * 2.I64
        \\    |b| {
        \\        b_loc = a_loc + b
        \\        |c| b_loc + c
        \\    }
        \\})(100.I64))(20.I64))(3.I64)
        ,
        .expected = .{ .inspect_str = "223" },
    },
    .{ .name = "inspect: typed identity closure on string", .source = "(|s| s)(\"Test\")", .expected = .{ .inspect_str = "\"Test\"" } },

    // Untyped closures, HOFs, and recursion
    .{
        .name = "inspect: closure capturing one local variable",
        .source =
        \\{
        \\    y = 10
        \\    f = |x| x + y
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure capturing two local variables",
        .source =
        \\{
        \\    a = 3
        \\    b = 7
        \\    f = |x| x + a + b
        \\    f(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: closure capturing a string",
        .source =
        \\{
        \\    greeting = "Hello"
        \\    f = |name| Str.concat(greeting, name)
        \\    f(" World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{
        .name = "inspect: closure capturing multiple strings",
        .source =
        \\{
        \\    prefix = "Hello"
        \\    suffix = "!"
        \\    f = |name| Str.concat(Str.concat(prefix, name), suffix)
        \\    f(" World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World!\"" },
    },
    .{
        .name = "inspect: function returning closure",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    add5(10)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure factory reused twice",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    a = add5(10)
        \\    b = add5(20)
        \\    a + b
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: two closures from same factory",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add3 = make_adder(3)
        \\    add7 = make_adder(7)
        \\    add3(10) + add7(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: function returning string closure",
        .source =
        \\{
        \\    make_greeter = |greeting| |name| Str.concat(greeting, name)
        \\    greet = make_greeter("Hi ")
        \\    greet("Alice")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hi Alice\"" },
    },
    .{
        .name = "inspect: direct function with dec capture-shaped record arg",
        .source =
        \\{
        \\    apply = |x, captures| x + captures.n
        \\    apply(10, { n: 5 })
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: single-arg function with dec record field",
        .source =
        \\{
        \\    apply = |captures| captures.n + 10
        \\    apply({ n: 5 })
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: if expression joining dec result",
        .source =
        \\{
        \\    apply = |captures| captures.n + 10
        \\    if True apply({ n: 5 }) else 0
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: function returning tag union with dec payload",
        .source =
        \\{
        \\    make : Dec -> [Ok({ n : Dec })]
        \\    make = |n| Ok({ n: n })
        \\    make(5)
        \\}
        ,
        .expected = .{ .inspect_str = "Ok({ n: 5.0 })" },
    },
    .{
        .name = "inspect: two-level closure factory",
        .source =
        \\{
        \\    make_op = |a| |b| |x| x + a + b
        \\    add_3_and_4 = make_op(3)(4)
        \\    add_3_and_4(10)
        \\}
        ,
        .expected = .{ .inspect_str = "17.0" },
    },
    .{
        .name = "inspect: passing closure to higher-order function",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    y = 10
        \\    apply(|x| x + y, 5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: higher-order function with two closures",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r1 + r2
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: higher-order function returns first closure result",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    _r2 = apply(|x| x + b, 5)
        \\    r1
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: higher-order function returns second closure result",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    _r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r2
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: higher-order function applying twice",
        .source =
        \\{
        \\    apply_twice = |f, x| f(f(x))
        \\    y = 3
        \\    apply_twice(|x| x + y, 10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: higher-order function returning string",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    prefix = "Hello "
        \\    apply(|name| Str.concat(prefix, name), "World")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello World\"" },
    },
    .{
        .name = "inspect: polymorphic identity over closure result",
        .source =
        \\{
        \\    id = |x| x
        \\    y = 10
        \\    f = |x| x + y
        \\    id(f(5))
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: polymorphic closure function with int and string",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    n = 10
        \\    prefix = "Hi "
        \\    num_result = apply(|x| x + n, 5)
        \\    str_result = apply(|s| Str.concat(prefix, s), "Bob")
        \\    if (num_result > 0) str_result else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hi Bob\"" },
    },
    .{
        .name = "inspect: closure forwarding to captured closure",
        .source =
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x)
        \\    outer(10)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure capturing another closure",
        .source =
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x) * 2
        \\    outer(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: closure capturing factory-produced closure",
        .source =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    double_add5 = |x| add5(x) * 2
        \\    double_add5(10)
        \\}
        ,
        .expected = .{ .inspect_str = "30.0" },
    },
    .{
        .name = "inspect: if chooses first closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (True) |x| x + a else |x| x + b
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: if chooses second closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (False) |x| x + a else |x| x + b
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: closures with different capture counts",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    c = 30
        \\    f = if (True) |x| x + a else |x| x + b + c
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: closure in record field",
        .source =
        \\{
        \\    y = 10
        \\    rec = { f: |x| x + y }
        \\    f = rec.f
        \\    f(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: two closures in record",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_b = rec.add_b
        \\    add_a(5) + add_b(5)
        \\}
        ,
        .expected = .{ .inspect_str = "40.0" },
    },
    .{
        .name = "inspect: record field closure add_a preserves capture",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_a = rec.add_a
        \\    add_a(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: parenthesized record field closure",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    (rec.add_b)(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: record field closure add_b preserves capture",
        .source =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    add_b = rec.add_b
        \\    add_b(5)
        \\}
        ,
        .expected = .{ .inspect_str = "25.0" },
    },
    .{
        .name = "inspect: opaque function field lookup issue 9262",
        .source_kind = .module,
        .source =
        \\W(a) := { f : {} -> [V(a)] }.{
        \\    run : W(a) -> [V(a)]
        \\    run = |w| (w.f)({})
        \\
        \\    mk : a -> W(a)
        \\    mk = |val| { f: |_| V(val) }
        \\}
        \\
        \\main = W.run(W.mk("x")) == V("x")
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: compose two functions",
        .source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add1 = |x| x + 1
        \\    double_then_add1 = compose(add1, double)
        \\    double_then_add1(5)
        \\}
        ,
        .expected = .{ .inspect_str = "11.0" },
    },
    .{
        .name = "inspect: compose with captures",
        .source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    a = 3
        \\    b = 7
        \\    add_a = |x| x + a
        \\    add_b = |x| x + b
        \\    add_both = compose(add_a, add_b)
        \\    add_both(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: pipe with closure",
        .source =
        \\{
        \\    pipe = |x, f| f(x)
        \\    y = 10
        \\    pipe(5, |x| x + y)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: recursive lambda factorial",
        .source =
        \\{
        \\    factorial = |n| if (n <= 1.I64) 1.I64 else n * factorial(n - 1.I64)
        \\    factorial(5.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "120" },
    },
    .{
        .name = "inspect: recursive closure factorial untyped",
        .source =
        \\{
        \\    factorial = |n| if (n <= 1) 1 else n * factorial(n - 1)
        \\    factorial(5)
        \\}
        ,
        .expected = .{ .inspect_str = "120.0" },
    },
    .{
        .name = "inspect: mutual recursion in local lambdas",
        .source =
        \\{
        \\    is_even = |n| if (n == 0.I64) True else is_odd(n - 1.I64)
        \\    is_odd = |n| if (n == 0.I64) False else is_even(n - 1.I64)
        \\    is_even(6.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: mutual recursion in untyped closures",
        .source =
        \\{
        \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\    if (is_even(4)) 1 else 0
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: triple nested closure factory",
        .source =
        \\{
        \\    level1 = |a| |b| |c| |x| x + a + b + c
        \\    level2 = level1(1)
        \\    level3 = level2(2)
        \\    level4 = level3(3)
        \\    level4(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: closure capturing another closure two levels",
        .source =
        \\{
        \\    a = 1
        \\    f = |x| x + a
        \\    b = 2
        \\    g = |x| f(x) + b
        \\    g(10)
        \\}
        ,
        .expected = .{ .inspect_str = "13.0" },
    },
    .{
        .name = "inspect: closure capturing another closure three levels",
        .source =
        \\{
        \\    a = 1
        \\    f = |x| x + a
        \\    b = 2
        \\    g = |x| f(x) + b
        \\    c = 3
        \\    h = |x| g(x) + c
        \\    h(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: hof returns closure capturing argument closure",
        .source =
        \\{
        \\    make_doubler = |f| |x| f(f(x))
        \\    add3 = |x| x + 3
        \\    double_add3 = make_doubler(add3)
        \\    double_add3(10)
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: hof returns closure capturing closure with captures",
        .source =
        \\{
        \\    n = 5
        \\    add_n = |x| x + n
        \\    make_doubler = |f| |x| f(f(x))
        \\    double_add_n = make_doubler(add_n)
        \\    double_add_n(10)
        \\}
        ,
        .expected = .{ .inspect_str = "20.0" },
    },
    .{
        .name = "inspect: chained closure factories",
        .source =
        \\{
        \\    step1 = |a| |b| |c| a + b + c
        \\    step2 = step1(100)
        \\    step3 = step2(20)
        \\    step3(3)
        \\}
        ,
        .expected = .{ .inspect_str = "123.0" },
    },
    .{
        .name = "inspect: regression issue 8727 captured closure return",
        .source =
        \\{
        \\    make_adder = |n| |x| n + x
        \\    add_ten = make_adder(10)
        \\    add_ten(5)
        \\}
        ,
        .expected = .{ .inspect_str = "15.0" },
    },
    .{
        .name = "inspect: polymorphic hof with closures capturing different types",
        .source =
        \\{
        \\    apply = |f, x| f(x)
        \\    offset = 100
        \\    prefix = "Result: "
        \\    num = apply(|x| x + offset, 23)
        \\    if (num > 0) apply(|s| Str.concat(prefix, s), "yes") else "no"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Result: yes\"" },
    },
    .{
        .name = "inspect: closure capture duplication integer specialization",
        .source =
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_num = make_getter(42)
        \\    get_num(0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: closure capture duplication string specialization",
        .source =
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_str = make_getter("hello")
        \\    get_str(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: closure over bool used in conditional",
        .source =
        \\{
        \\    flag = True
        \\    choose = |a, b| if (flag) a else b
        \\    choose(42, 0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: deeply nested blocks add captures",
        .source =
        \\{
        \\    a = 1
        \\    r1 = {
        \\        b = 2
        \\        r2 = {
        \\            c = 3
        \\            f = |x| x + a + b + c
        \\            f(10)
        \\        }
        \\        r2
        \\    }
        \\    r1
        \\}
        ,
        .expected = .{ .inspect_str = "16.0" },
    },
    .{
        .name = "inspect: same capture used by independent closures",
        .source =
        \\{
        \\    shared = 10
        \\    f = |x| x + shared
        \\    g = |x| x * shared
        \\    f(5) + g(3)
        \\}
        ,
        .expected = .{ .inspect_str = "45.0" },
    },
    .{
        .name = "inspect: closure returning captured string composition",
        .source =
        \\{
        \\    make_greeter = |greeting|
        \\        |name|
        \\            Str.concat(Str.concat(greeting, ", "), name)
        \\    hello = make_greeter("Hello")
        \\    hi = make_greeter("Hi")
        \\    r1 = hello("Alice")
        \\    r2 = hi("Bob")
        \\    Str.concat(Str.concat(r1, " and "), r2)
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello, Alice and Hi, Bob\"" },
    },
    .{
        .name = "inspect: same closure applied to multiple arguments",
        .source =
        \\{
        \\    base = 100
        \\    f = |x| x + base
        \\    a = f(1)
        \\    b = f(2)
        \\    c = f(3)
        \\    a + b + c
        \\}
        ,
        .expected = .{ .inspect_str = "306.0" },
    },
    .{
        .name = "inspect: immediately invoked closure with capture",
        .source =
        \\{
        \\    y = 42
        \\    (|x| x + y)(8)
        \\}
        ,
        .expected = .{ .inspect_str = "50.0" },
    },
    .{
        .name = "inspect: closure ignores argument and uses capture",
        .source =
        \\{
        \\    val = 99
        \\    f = |_| val
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "99.0" },
    },
    .{
        .name = "inspect: closure ignores capture and uses argument",
        .source =
        \\{
        \\    _unused = 999
        \\    f = |x| x + 1
        \\    f(41)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: monomorphic str identity",
        .source =
        \\{
        \\    identity : Str -> Str
        \\    identity = |val| val
        \\    identity("Hello")
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: monomorphic dec identity",
        .source =
        \\{
        \\    identity : Dec -> Dec
        \\    identity = |val| val
        \\    identity(5)
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: monomorphic str identity if join",
        .source =
        \\{
        \\    str_id : Str -> Str
        \\    str_id = |val| val
        \\    num = 5
        \\    str = str_id("Hello")
        \\    if (num > 0) str else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: multi-use closure with short captured string",
        .source =
        \\{
        \\    s = "short"
        \\    f = |_x| s
        \\    _a = f(0)
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"short\"" },
    },
    .{
        .name = "inspect: multi-use closure with heap captured string",
        .source =
        \\{
        \\    s = "This string is definitely longer than twenty three bytes"
        \\    f = |_x| s
        \\    _a = f(0)
        \\    f(0)
        \\}
        ,
        .expected = .{ .inspect_str = "\"This string is definitely longer than twenty three bytes\"" },
    },
    .{
        .name = "inspect: large record chained higher order calls w",
        .source =
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.w
        \\}
        ,
        .expected = .{ .inspect_str = "\"w_val\"" },
    },
    .{
        .name = "inspect: large record chained higher order calls y",
        .source =
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.y
        \\}
        ,
        .expected = .{ .inspect_str = "\"y_val\"" },
    },

    // Loops
    .{
        .name = "inspect: for loop sums list",
        .source =
        \\{
        \\    var $sum = 0.I64
        \\    for item in [10.I64, 20.I64, 30.I64] {
        \\        $sum = $sum + item
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: for loop inside lambda body",
        .source =
        \\{
        \\    sum = |xs| {
        \\        var $sum = 0.I64
        \\        for item in xs {
        \\            $sum = $sum + item
        \\        }
        \\        $sum
        \\    }
        \\    sum([1.I64, 2.I64, 3.I64, 4.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "10" },
    },
    .{
        .name = "inspect: for loop early return",
        .source =
        \\{
        \\    f = |list| {
        \\        for _item in list {
        \\            if True { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: for loop closure early return",
        .source =
        \\{
        \\    f = |list, pred| {
        \\        for item in list {
        \\            if pred(item) { return True }
        \\        }
        \\        False
        \\    }
        \\    f([1.I64, 2.I64, 3.I64], |_x| True)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: lambda list param calling List.len",
        .source =
        \\{
        \\    get_len = |l| List.len(l)
        \\    get_len([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param calling List.append",
        .source =
        \\{
        \\    add_one = |l| List.len(List.append(l, 99.I64))
        \\    add_one([1.I64, 2.I64, 3.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "inspect: lambda list param and var declaration",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param and list literal creation",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param var and for loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: lambda list param var and List.append no for loop",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $acc = [0.I64]
        \\        $acc = List.append($acc, 42.I64)
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: minimal lambda list param and for loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param for loop with allocation inside loop",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0.I64
        \\        for e in l {
        \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1.I64, 2.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda for loop over internal list",
        .source =
        \\{
        \\    test_fn = |_x| {
        \\        var $total = 0.I64
        \\        for e in [1.I64, 2.I64, 3.I64] {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn(42.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "inspect: lambda list param internal loop allocation",
        .source =
        \\{
        \\    test_fn = |_l| {
        \\        var $total = 0.I64
        \\        for e in [1.I64, 2.I64] {
        \\            $total = match List.last([e]) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10.I64, 20.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "3" },
    },
    .{
        .name = "inspect: lambda list param for loop empty iteration",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([])
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: lambda list param for loop append single iteration",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: lambda list param var for loop and List.append",
        .source =
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "4" },
    },
    .{
        .name = "inspect: closure in for loop with List.last regression",
        .source =
        \\{
        \\    sum_with_last = |l| {
        \\        var $total = 0.I64
        \\        var $acc = [0.I64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    sum_with_last([10.I64, 20.I64, 30.I64])
        \\}
        ,
        .expected = .{ .inspect_str = "60" },
    },
    .{
        .name = "inspect: closure capturing for loop element with equality",
        .source =
        \\{
        \\    my_any = |lst, pred| {
        \\        for e in lst {
        \\            if pred(e) { return True }
        \\        }
        \\        False
        \\    }
        \\    check = |list| {
        \\        var $built = []
        \\        for item in list {
        \\            _x = my_any($built, |x| x == item)
        \\            $built = $built.append(item)
        \\        }
        \\        $built.len()
        \\    }
        \\    check([1, 2])
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },

    // Polymorphic source-level cases
    .{
        .name = "inspect: polymorphic identity returns string",
        .source =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: direct polymorphic function usage",
        .source =
        \\{
        \\    id = |x| x
        \\    num1 = id(10)
        \\    str1 = id("Test")
        \\    num2 = id(20)
        \\    if (num1 == 10)
        \\        if (num2 == 20)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Test\"" },
    },
    .{
        .name = "inspect: polymorphic return function then call int",
        .source = "(|_| (|x| x))(0)(42)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic return function then call string",
        .source = "(|_| (|x| x))(0)(\"hi\")",
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "inspect: polymorphic captured id applied to int",
        .source = "((|id| (|x| id(x)))(|y| y))(41)",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "inspect: polymorphic captured id applied to string",
        .source = "((|id| (|x| id(x)))(|y| y))(\"ok\")",
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: polymorphic higher-order apply then call",
        .source = "((|f| (|x| f(x)))(|n| n + 1))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic higher-order apply twice",
        .source = "((|f| (|x| f(f(x))))(|n| n + 1))(40)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic pass constructed closure and apply",
        .source = "(|g| g(41))((|f| (|x| f(x)))(|y| y))",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "inspect: polymorphic construct then pass then call",
        .source = "((|make| (|z| (make(|n| n + 1))(z)))(|f| (|x| f(x))))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic compose identity with plus one",
        .source = "(((|f| (|g| (|x| f(g(x)))))(|n| n + 1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic return function using captured increment",
        .source = "(((|n| (|id| (|x| id(x + n))))(1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: recursive countdown",
        .source =
        \\{
        \\    rec = |n| if (n == 0) 0 else rec(n - 1) + 1
        \\    rec(2)
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "inspect: else if chain selects middle branch",
        .source =
        \\{
        \\    n = 1
        \\    if (n == 0)
        \\        "zero"
        \\    else if (n == 1)
        \\        "one"
        \\    else
        \\        "other"
        \\}
        ,
        .expected = .{ .inspect_str = "\"one\"" },
    },
    .{
        .name = "inspect: mutable variable reassign",
        .source =
        \\{
        \\    var $x = 1
        \\    $x = $x + 1
        \\    $x
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "inspect: logical or short circuits",
        .source =
        \\if ((1 == 1) or { crash "nope" })
        \\    "ok"
        \\else
        \\    "bad"
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: logical and short circuits",
        .source =
        \\if ((1 == 0) and { crash "nope" })
        \\    "bad"
        \\else
        \\    "ok"
        ,
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "inspect: recursive fibonacci",
        .source =
        \\{
        \\    fib = |n| if (n == 0) 0 else if (n == 1) 1 else fib(n - 1) + fib(n - 2)
        \\    fib(5)
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "inspect: tag union one arg ok",
        .source = "Ok(42.0)",
        .expected = .{ .inspect_str = "Ok(42.0)" },
    },
    .{
        .name = "inspect: tag union multi arg point",
        .source = "Point(1.0, 2.0)",
        .expected = .{ .inspect_str = "Point(1.0, 2.0)" },
    },
    .{
        .name = "inspect: tag union nested in tuple regression",
        .source = "Ok((Name(\"hello\"), 5))",
        .expected = .{ .inspect_str = "Ok((Name(\"hello\"), 5.0))" },
    },
    .{
        .name = "inspect: multiple polymorphic instantiations",
        .source =
        \\{
        \\    id = |x| x
        \\
        \\    num1 = id(42)
        \\    str1 = id("Hello")
        \\    num2 = id(100)
        \\
        \\    if (num1 == 42)
        \\        if (num2 == 100)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
        ,
        .expected = .{ .inspect_str = "\"Hello\"" },
    },
    .{
        .name = "inspect: early return in closure passed to List.map",
        .source =
        \\{
        \\    result = [Ok(1), Err({})].map(|x| Ok(x?))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: early return in closure passed to List.fold",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    result = List.fold([Ok(1), Err({})], [], |acc, x| List.append(acc, compute(x)))
        \\    List.len(result)
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },
    .{
        .name = "inspect: lambda wrapping try suffix result in Ok",
        .source =
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: lambda with tag match called directly",
        .source =
        \\{
        \\    f = |child|
        \\        match child {
        \\            Aaa(_, _) => 10.I64
        \\            Bbb(_) => 1.I64
        \\        }
        \\    f(Bbb(42.I64))
        \\}
        ,
        .expected = .{ .inspect_str = "1" },
    },
    .{
        .name = "inspect: fold with simple addition lambda",
        .source =
        \\{
        \\    items = [1.I64, 2.I64, 3.I64]
        \\    List.fold(items, 0.I64, |acc, x| acc + x)
        \\}
        ,
        .expected = .{ .inspect_str = "6" },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution extract payload",
        .source =
        \\{
        \\    second : [Left(a), Right(b)], b -> b
        \\    second = |either, fallback| match either {
        \\        Left(_) => fallback
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(I64), Right(I64)]
        \\    input = Right(42.I64)
        \\    second(input, 0.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "42" },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution multiple type vars",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)], e -> e
        \\    get_err = |result, fallback| match result {
        \\        Ok(_) => fallback
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Err("hello")
        \\    get_err(val, "")
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: attached methods on transparent tag union alias issue 8637",
        .source_kind = .module,
        .source =
        \\Iter(s) :: [It(s)].{
        \\    identity : Iter(s) -> Iter(s)
        \\    identity = |It(s_)| It(s_)
        \\}
        \\
        \\count : Iter({})
        \\count = It({})
        \\
        \\main = count.identity() == It({})
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "problem and crash: polymorphic erroneous match branch",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| match result {
        \\        Ok(_) => ""
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Ok(42)
        \\    get_err(val)
        \\}
        ,
        .expected = .{ .problem_and_crash = {} },
    },
    .{
        .name = "problem and crash: polymorphic erroneous if else branch",
        .source =
        \\{
        \\    get_val : Bool, e -> e
        \\    get_val = |flag, val| if (flag) "" else val
        \\
        \\    get_val(Bool.true, 42)
        \\}
        ,
        .expected = .{ .problem_and_crash = {} },
    },
    .{
        .name = "problem and crash: polymorphic erroneous match in block",
        .source =
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| {
        \\        unused = 0
        \\        match result {
        \\            Ok(_) => ""
        \\            Err(e) => e
        \\        }
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Ok(42)
        \\    get_err(val)
        \\}
        ,
        .expected = .{ .problem_and_crash = {} },
    },
    .{
        .name = "inspect: polymorphic tag payload substitution wrap and unwrap",
        .source =
        \\{
        \\    second : [Left(a), Right(b)], b -> b
        \\    second = |either, fallback| match either {
        \\        Left(_) => fallback
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(Dec), Right(Dec)]
        \\    input = Right(42.0)
        \\    second(input, 0.0)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "inspect: polymorphic tag payload layout in match expression",
        .source =
        \\{
        \\    transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
        \\    transform_err = |try_val, transform| match try_val {
        \\        Err(a) => Err(transform(a))
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42.I32)
        \\
        \\    result = transform_err(err, |_e| "hello")
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: polymorphic tag transform with match",
        .source =
        \\{
        \\    transform_err = |try_val| match try_val {
        \\        Err(_) => Err("hello")
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42.I32)
        \\
        \\    result = transform_err(err)
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "\"hello\"" },
    },
    .{
        .name = "inspect: recursive function with record stack restoration",
        .source =
        \\{
        \\    f = |n|
        \\        if n <= 0
        \\            0
        \\        else
        \\            { a: n, b: n * 2, c: n * 3, d: n * 4 }.a + f(n - 1)
        \\    f(1000)
        \\}
        ,
        .expected = .{ .inspect_str = "500500.0" },
    },
    .{
        .name = "inspect: polymorphic sum in block called with U64",
        .source =
        \\{
        \\    sum = |a, b| {
        \\        tmp = a
        \\        tmp + b
        \\    }
        \\    sum(100.U64, 160.U64)
        \\}
        ,
        .expected = .{ .inspect_str = "260" },
    },
    .{
        .name = "inspect: polymorphic predicate with comparison in block",
        .source =
        \\{
        \\    at_least = |threshold, value| {
        \\        current = value
        \\        current >= threshold
        \\    }
        \\    at_least(3, 5)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic comparison lambda called directly",
        .source =
        \\{
        \\    greater_than = |lhs, rhs| lhs > rhs
        \\    greater_than(5, 3)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic comparison lambda passed to List.any",
        .source =
        \\{
        \\    greater_than = |lhs, rhs| lhs > rhs
        \\    List.any([1, 2, 3, 4], |x| greater_than(x, 3))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: polymorphic function called with two list types",
        .source =
        \\{
        \\    my_len = |list| list.len()
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    my_len(a) + my_len(b)
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: polymorphic function single call I64",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    r = contains(a, 2)
        \\    if r { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function single call Str",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    r = contains(b, "x")
        \\    if r { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function with List.contains called with two types",
        .source =
        \\{
        \\    contains = |list, item| list.contains(item)
        \\    a : List(I64)
        \\    a = [1, 2, 3]
        \\    b : List(Str)
        \\    b = ["x", "y"]
        \\    r1 = contains(a, 2)
        \\    r2 = contains(b, "x")
        \\    if r1 and r2 { 1 } else { 0 }
        \\}
        ,
        .expected = .{ .inspect_str = "1.0" },
    },
    .{
        .name = "inspect: polymorphic function with List.contains called with multiple types",
        .source =
        \\{
        \\    dedup = |list| {
        \\        var $out = []
        \\        for item in list {
        \\            if !$out.contains(item) {
        \\                $out = $out.append(item)
        \\            }
        \\        }
        \\        $out
        \\    }
        \\    nums : List(I64)
        \\    nums = [1, 2, 3, 2, 1]
        \\    u1 = dedup(nums)
        \\    strs : List(Str)
        \\    strs = ["a", "b", "a"]
        \\    u2 = dedup(strs)
        \\    u1.len() + u2.len()
        \\}
        ,
        .expected = .{ .inspect_str = "5" },
    },
    .{
        .name = "inspect: nested List.any true path with captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["a"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: nested List.any false path with captured Str",
        .source =
        \\{
        \\    out = ["a"]
        \\    List.any(["b"], |item| out.contains(item))
        \\}
        ,
        .expected = .{ .inspect_str = "False" },
    },
    .{
        .name = "inspect: direct List.contains captured Str control",
        .source =
        \\{
        \\    out = ["a"]
        \\    out.contains("a")
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: forwarding tag union with Str payload through proc call",
        .source =
        \\{
        \\    consume = |value| value == Ok({ x: "x" })
        \\    forward = |value| consume(value)
        \\    value = Ok({ x: "x" })
        \\    forward(value)
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "inspect: Str.inspect through polymorphic wrapper",
        .source =
        \\{
        \\    show = |x| Str.inspect(x)
        \\    show(42.I64)
        \\}
        ,
        .expected = .{ .inspect_str = "\"42\"" },
    },
    .{
        .name = "inspect: polymorphic additional specialization via List.append",
        .source =
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
        \\    _first_len = clone_via_fold([1.I64, 2.I64]).len()
        \\    clone_via_fold([[1.I64, 2.I64], [3.I64, 4.I64]]).len()
        \\}
        ,
        .expected = .{ .inspect_str = "2" },
    },

    // Typed arithmetic matrix from origin/main
    .{ .name = "inspect: U8 plus", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 50\n    a + b\n}", .expected = .{ .inspect_str = "250" } },
    .{ .name = "inspect: U8 minus", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 50\n    a - b\n}", .expected = .{ .inspect_str = "150" } },
    .{ .name = "inspect: U8 times", .source = "{ a : U8\n    a = 15\n    b : U8\n    b = 17\n    a * b\n}", .expected = .{ .inspect_str = "255" } },
    .{ .name = "inspect: U8 div", .source = "{ a : U8\n    a = 240\n    b : U8\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "120" } },
    .{ .name = "inspect: U8 rem", .source = "{ a : U8\n    a = 200\n    b : U8\n    b = 13\n    a % b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: U16 plus", .source = "{ a : U16\n    a = 40000\n    b : U16\n    b = 20000\n    a + b\n}", .expected = .{ .inspect_str = "60000" } },
    .{ .name = "inspect: U16 minus", .source = "{ a : U16\n    a = 50000\n    b : U16\n    b = 10000\n    a - b\n}", .expected = .{ .inspect_str = "40000" } },
    .{ .name = "inspect: U16 times", .source = "{ a : U16\n    a = 256\n    b : U16\n    b = 255\n    a * b\n}", .expected = .{ .inspect_str = "65280" } },
    .{ .name = "inspect: U16 div", .source = "{ a : U16\n    a = 60000\n    b : U16\n    b = 3\n    a // b\n}", .expected = .{ .inspect_str = "20000" } },
    .{ .name = "inspect: U16 rem", .source = "{ a : U16\n    a = 50000\n    b : U16\n    b = 128\n    a % b\n}", .expected = .{ .inspect_str = "80" } },

    .{ .name = "inspect: U32 plus", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 1000000000\n    a + b\n}", .expected = .{ .inspect_str = "4000000000" } },
    .{ .name = "inspect: U32 minus", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 1000000000\n    a - b\n}", .expected = .{ .inspect_str = "2000000000" } },
    .{ .name = "inspect: U32 times", .source = "{ a : U32\n    a = 65536\n    b : U32\n    b = 65535\n    a * b\n}", .expected = .{ .inspect_str = "4294901760" } },
    .{ .name = "inspect: U32 div", .source = "{ a : U32\n    a = 4000000000\n    b : U32\n    b = 1000\n    a // b\n}", .expected = .{ .inspect_str = "4000000" } },
    .{ .name = "inspect: U32 rem", .source = "{ a : U32\n    a = 3000000000\n    b : U32\n    b = 128\n    a % b\n}", .expected = .{ .inspect_str = "0" } },

    .{ .name = "inspect: U64 plus", .source = "{ a : U64\n    a = 10000000000000000000\n    b : U64\n    b = 5000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "15000000000000000000" } },
    .{ .name = "inspect: U64 minus", .source = "{ a : U64\n    a = 15000000000000000000\n    b : U64\n    b = 5000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "10000000000000000000" } },
    .{ .name = "inspect: U64 times", .source = "{ a : U64\n    a = 4294967296\n    b : U64\n    b = 4294967295\n    a * b\n}", .expected = .{ .inspect_str = "18446744069414584320" } },
    .{ .name = "inspect: U64 div", .source = "{ a : U64\n    a = 15000000000000000000\n    b : U64\n    b = 1000000\n    a // b\n}", .expected = .{ .inspect_str = "15000000000000" } },
    .{ .name = "inspect: U64 rem", .source = "{ a : U64\n    a = 10000000000000000000\n    b : U64\n    b = 256\n    a % b\n}", .expected = .{ .inspect_str = "0" } },

    .{ .name = "inspect: U128 plus", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 50000000000000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "150000000000000000000000000000" } },
    .{ .name = "inspect: U128 minus", .source = "{ a : U128\n    a = 150000000000000000000000000000\n    b : U128\n    b = 50000000000000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "100000000000000000000000000000" } },
    .{ .name = "inspect: U128 times", .source = "{ a : U128\n    a = 13043817825332782212\n    b : U128\n    b = 13043817825332782212\n    a * b\n}", .expected = .{ .inspect_str = "170141183460469231722567801800623612944" } },
    .{ .name = "inspect: U128 div", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 10000000000000000\n    a // b\n}", .expected = .{ .inspect_str = "10000000000000" } },
    .{ .name = "inspect: U128 rem", .source = "{ a : U128\n    a = 100000000000000000000000000000\n    b : U128\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "10" } },

    .{ .name = "inspect: I8 negate", .source = "{ a : I8\n    a = -127\n    -a\n}", .expected = .{ .inspect_str = "127" } },
    .{ .name = "inspect: I8 plus", .source = "{ a : I8\n    a = -100\n    b : I8\n    b = -20\n    a + b\n}", .expected = .{ .inspect_str = "-120" } },
    .{ .name = "inspect: I8 minus", .source = "{ a : I8\n    a = -50\n    b : I8\n    b = 70\n    a - b\n}", .expected = .{ .inspect_str = "-120" } },
    .{ .name = "inspect: I8 times", .source = "{ a : I8\n    a = -16\n    b : I8\n    b = 8\n    a * b\n}", .expected = .{ .inspect_str = "-128" } },
    .{ .name = "inspect: I8 div", .source = "{ a : I8\n    a = -128\n    b : I8\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-64" } },
    .{ .name = "inspect: I8 rem", .source = "{ a : I8\n    a = -128\n    b : I8\n    b = 7\n    a % b\n}", .expected = .{ .inspect_str = "-2" } },

    .{ .name = "inspect: I16 negate", .source = "{ a : I16\n    a = -32767\n    -a\n}", .expected = .{ .inspect_str = "32767" } },
    .{ .name = "inspect: I16 plus", .source = "{ a : I16\n    a = -20000\n    b : I16\n    b = -10000\n    a + b\n}", .expected = .{ .inspect_str = "-30000" } },
    .{ .name = "inspect: I16 minus", .source = "{ a : I16\n    a = -10000\n    b : I16\n    b = 20000\n    a - b\n}", .expected = .{ .inspect_str = "-30000" } },
    .{ .name = "inspect: I16 times", .source = "{ a : I16\n    a = -256\n    b : I16\n    b = 128\n    a * b\n}", .expected = .{ .inspect_str = "-32768" } },
    .{ .name = "inspect: I16 div", .source = "{ a : I16\n    a = -32768\n    b : I16\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-16384" } },
    .{ .name = "inspect: I16 rem", .source = "{ a : I16\n    a = -32768\n    b : I16\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-98" } },

    .{ .name = "inspect: I32 negate", .source = "{ a : I32\n    a = -2147483647\n    -a\n}", .expected = .{ .inspect_str = "2147483647" } },
    .{ .name = "inspect: I32 plus", .source = "{ a : I32\n    a = -1000000000\n    b : I32\n    b = -500000000\n    a + b\n}", .expected = .{ .inspect_str = "-1500000000" } },
    .{ .name = "inspect: I32 minus", .source = "{ a : I32\n    a = -1000000000\n    b : I32\n    b = 500000000\n    a - b\n}", .expected = .{ .inspect_str = "-1500000000" } },
    .{ .name = "inspect: I32 times", .source = "{ a : I32\n    a = -65536\n    b : I32\n    b = 32768\n    a * b\n}", .expected = .{ .inspect_str = "-2147483648" } },
    .{ .name = "inspect: I32 div", .source = "{ a : I32\n    a = -2147483648\n    b : I32\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-1073741824" } },
    .{ .name = "inspect: I32 rem", .source = "{ a : I32\n    a = -2147483648\n    b : I32\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-2" } },

    .{ .name = "inspect: I64 negate", .source = "{ a : I64\n    a = -9223372036854775807\n    -a\n}", .expected = .{ .inspect_str = "9223372036854775807" } },
    .{ .name = "inspect: I64 plus", .source = "{ a : I64\n    a = -5000000000000\n    b : I64\n    b = -3000000000000\n    a + b\n}", .expected = .{ .inspect_str = "-8000000000000" } },
    .{ .name = "inspect: I64 minus", .source = "{ a : I64\n    a = -5000000000000\n    b : I64\n    b = 3000000000000\n    a - b\n}", .expected = .{ .inspect_str = "-8000000000000" } },
    .{ .name = "inspect: I64 times", .source = "{ a : I64\n    a = -4294967296\n    b : I64\n    b = 2147483648\n    a * b\n}", .expected = .{ .inspect_str = "-9223372036854775808" } },
    .{ .name = "inspect: I64 div", .source = "{ a : I64\n    a = -9223372036854775808\n    b : I64\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-4611686018427387904" } },
    .{ .name = "inspect: I64 rem", .source = "{ a : I64\n    a = -9223372036854775808\n    b : I64\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-8" } },

    .{ .name = "inspect: I128 negate", .source = "{ a : I128\n    a = -85070591730234615865843651857942052864\n    -a\n}", .expected = .{ .inspect_str = "85070591730234615865843651857942052864" } },
    .{ .name = "inspect: I128 plus", .source = "{ a : I128\n    a = -100000000000000000000000\n    b : I128\n    b = -50000000000000000000000\n    a + b\n}", .expected = .{ .inspect_str = "-150000000000000000000000" } },
    .{ .name = "inspect: I128 minus", .source = "{ a : I128\n    a = -100000000000000000000000\n    b : I128\n    b = 50000000000000000000000\n    a - b\n}", .expected = .{ .inspect_str = "-150000000000000000000000" } },
    .{ .name = "inspect: I128 times", .source = "{ a : I128\n    a = -18446744073709551616\n    b : I128\n    b = 9223372036854775808\n    a * b\n}", .expected = .{ .inspect_str = "-170141183460469231731687303715884105728" } },
    .{ .name = "inspect: I128 div", .source = "{ a : I128\n    a = -170141183460469231731687303715884105728\n    b : I128\n    b = 2\n    a // b\n}", .expected = .{ .inspect_str = "-85070591730234615865843651857942052864" } },
    .{ .name = "inspect: I128 rem", .source = "{ a : I128\n    a = -170141183460469231731687303715884105728\n    b : I128\n    b = 99\n    a % b\n}", .expected = .{ .inspect_str = "-29" } },

    .{ .name = "inspect: F32 literal", .source = "3.14.F32", .expected = .{ .inspect_str = "3.140000104904175" } },
    .{ .name = "inspect: F32 variable assignment", .source = "{ a : F32\n    a = 3.14.F32\n    a\n}", .expected = .{ .inspect_str = "3.140000104904175" } },
    .{ .name = "inspect: F32 negate", .source = "{ a : F32\n    a = 3.14.F32\n    -a\n}", .expected = .{ .inspect_str = "-3.140000104904175" } },
    .{ .name = "inspect: F32 plus", .source = "{ a : F32\n    a = 1.5.F32\n    b : F32\n    b = 2.5.F32\n    a + b\n}", .expected = .{ .inspect_str = "4" } },
    .{ .name = "inspect: F32 minus", .source = "{ a : F32\n    a = 10.0.F32\n    b : F32\n    b = 3.5.F32\n    a - b\n}", .expected = .{ .inspect_str = "6.5" } },
    .{ .name = "inspect: F32 times", .source = "{ a : F32\n    a = 2.5.F32\n    b : F32\n    b = 4.0.F32\n    a * b\n}", .expected = .{ .inspect_str = "10" } },
    .{ .name = "inspect: F32 div", .source = "{ a : F32\n    a = 10.0.F32\n    b : F32\n    b = 2.0.F32\n    a / b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: F64 negate", .source = "{ a : F64\n    a = 3.141592653589793.F64\n    -a\n}", .expected = .{ .inspect_str = "-3.141592653589793" } },
    .{ .name = "inspect: F64 plus", .source = "{ a : F64\n    a = 1.5.F64\n    b : F64\n    b = 2.5.F64\n    a + b\n}", .expected = .{ .inspect_str = "4" } },
    .{ .name = "inspect: F64 minus", .source = "{ a : F64\n    a = 10.5.F64\n    b : F64\n    b = 3.25.F64\n    a - b\n}", .expected = .{ .inspect_str = "7.25" } },
    .{ .name = "inspect: F64 times", .source = "{ a : F64\n    a = 2.5.F64\n    b : F64\n    b = 4.0.F64\n    a * b\n}", .expected = .{ .inspect_str = "10" } },
    .{ .name = "inspect: F64 div", .source = "{ a : F64\n    a = 10.0.F64\n    b : F64\n    b = 2.0.F64\n    a / b\n}", .expected = .{ .inspect_str = "5" } },

    .{ .name = "inspect: Dec literal", .source = "3.14.Dec", .expected = .{ .inspect_str = "3.14" } },
    .{ .name = "inspect: Dec negate", .source = "{ a : Dec\n    a = 3.14.Dec\n    -a\n}", .expected = .{ .inspect_str = "-3.14" } },
    .{ .name = "inspect: Dec plus", .source = "{ a : Dec\n    a = 1.5.Dec\n    b : Dec\n    b = 2.5.Dec\n    a + b\n}", .expected = .{ .inspect_str = "4.0" } },
    .{ .name = "inspect: Dec minus", .source = "{ a : Dec\n    a = 10.0.Dec\n    b : Dec\n    b = 3.5.Dec\n    a - b\n}", .expected = .{ .inspect_str = "6.5" } },
    .{ .name = "inspect: Dec times", .source = "{ a : Dec\n    a = 2.5.Dec\n    b : Dec\n    b = 4.0.Dec\n    a * b\n}", .expected = .{ .inspect_str = "10.0" } },
    .{ .name = "inspect: Dec div", .source = "{ a : Dec\n    a = 10.0.Dec\n    b : Dec\n    b = 2.0.Dec\n    a / b\n}", .expected = .{ .inspect_str = "5.0" } },
    .{ .name = "inspect: Dec to_str", .source = "{ a : Dec\n    a = 100.0.Dec\n    Dec.to_str(a)\n}", .expected = .{ .inspect_str = "\"100.0\"" } },

    // Remaining semantic ports from interpreter_style_test.zig
    .{ .name = "inspect: match list rest binds slice", .source = "match [1, 2, 3] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{
        .name = "inspect: simple for loop sum",
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
    .{ .name = "inspect: inline identity lambda on string", .source = "(|x| x)(\"Hello\")", .expected = .{ .inspect_str = "\"Hello\"" } },
    .{ .name = "inspect: inline increment lambda on dec literal", .source = "(|n| n + 1)(41)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: inline binary add lambda on dec literals", .source = "(|a, b| a + b)(40, 2)", .expected = .{ .inspect_str = "42.0" } },
    .{ .name = "inspect: slash division defaults to Dec", .source = "6 / 3", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: decimal addition simple fraction", .source = "0.2 + 0.3", .expected = .{ .inspect_str = "0.5" } },
    .{ .name = "inspect: decimal division by integer literal", .source = "0.5 / 2", .expected = .{ .inspect_str = "0.25" } },
    .{ .name = "inspect: custom tag renders as tag name", .source = "MyTag", .expected = .{ .inspect_str = "MyTag" } },
    .{
        .name = "inspect: record update copies base fields",
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
        .name = "inspect: record update overrides field",
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
        .name = "inspect: record update expression references base",
        .source =
        \\{
        \\    point = { x: 1, y: 2 }
        \\    updated = { ..point, y: point.y + 5 }
        \\    updated.y
        \\}
        ,
        .expected = .{ .inspect_str = "7.0" },
    },
    .{ .name = "inspect: match tuple pattern destructures", .source = "match (1, 2) { (1, b) => b, _ => 0 }", .expected = .{ .inspect_str = "2.0" } },
    .{ .name = "inspect: match bool patterns", .source = "match True { True => 1, False => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: match result tag payload", .source = "match Ok(3) { Ok(n) => n + 1, Err(_) => 0 }", .expected = .{ .inspect_str = "4.0" } },
    .{ .name = "inspect: match record destructures fields", .source = "match { x: 1, y: 2 } { { x, y } => x + y }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: render Try.Ok literal", .source = "match True { True => Ok(42), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok(42.0)" } },
    .{ .name = "inspect: render Try.Err string", .source = "match True { True => Err(\"boom\"), False => Ok(42) }", .expected = .{ .inspect_str = "Err(\"boom\")" } },
    .{ .name = "inspect: render Try.Ok tuple payload", .source = "match True { True => Ok((1, 2)), False => Err(\"boom\") }", .expected = .{ .inspect_str = "Ok((1.0, 2.0))" } },
    .{ .name = "inspect: match tuple payload tag", .source = "match Ok((1, 2)) { Ok((a, b)) => a + b, Err(_) => 0 }", .expected = .{ .inspect_str = "3.0" } },
    .{ .name = "inspect: match record payload tag", .source = "match Err({ code: 1, msg: \"boom\" }) { Err({ code, msg: _msg }) => code, Ok(_) => 0 }", .expected = .{ .inspect_str = "1.0" } },
    .{ .name = "inspect: direct list pattern destructure sum", .source = "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }", .expected = .{ .inspect_str = "6.0" } },
    .{ .name = "inspect: List.len on literal", .source = "List.len([1, 2, 3])", .expected = .{ .inspect_str = "3" } },
    .{
        .name = "inspect: generic local List.len specialization stays resolved",
        .source_kind = .module,
        .source =
        \\measure = |xs| xs.len()
        \\
        \\main = (measure([1, 2, 3]), measure(["a", "bb"]))
        ,
        .expected = .{ .inspect_str = "(3, 2)" },
    },
    .{ .name = "inspect: List.fold builtin sum", .source = "List.fold([1, 2, 3], 0, |acc, item| acc + item)", .expected = .{ .inspect_str = "6.0" } },
    .{ .name = "inspect: List.any true on integers", .source = "List.any([1, 0, 1, 0, -1], |x| x > 0)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.any false on positive integers with negative predicate", .source = "List.any([9, 8, 7, 6, 5], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.any false on empty list", .source = "List.any([], |x| x < 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.all false when some elements fail", .source = "List.all([9, 18, 7, 6, 15], |x| x < 10)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.all true on small integers", .source = "List.all([9, 8, 7, 6, 5], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.all on empty list is True", .source = "List.all([], |x| x < 10)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.contains false for missing element", .source = "List.contains([-1, -2, -3, 1, 2, 3], 0)", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: List.contains true when element is found", .source = "List.contains([1, 2, 3, 4, 5], 3)", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: List.contains false on empty list", .source = "List.contains([], 3333)", .expected = .{ .inspect_str = "False" } },
    .{
        .name = "inspect: generic local attached method specialization on nominal",
        .source_kind = .module,
        .source =
        \\Counter := [Counter(U64)].{
        \\  get : Counter -> U64
        \\  get = |Counter.Counter(n)| n
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Counter.Counter(5)), read(Counter.Counter(8)))
        ,
        .expected = .{ .inspect_str = "(5, 8)" },
    },
    .{
        .name = "inspect: generic local attached method specialization picks different nominal targets",
        .source_kind = .module,
        .source =
        \\Box := [Box(U64)].{
        \\  get : Box -> U64
        \\  get = |Box.Box(n)| n
        \\}
        \\
        \\Count := [Count(U64)].{
        \\  get : Count -> U64
        \\  get = |Count.Count(n)| n + 100
        \\}
        \\
        \\read = |value| value.get()
        \\
        \\main = (read(Box.Box(5)), read(Count.Count(8)))
        ,
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: cross-module attached method specialization on imported nominal",
        .source_kind = .module,
        .source =
        \\import CounterMod
        \\
        \\main = CounterMod.Counter(41).get()
        ,
        .imports = &.{.{
            .name = "CounterMod",
            .source =
            \\Counter := [Counter(U64)].{
            \\  get : Counter -> U64
            \\  get = |Counter.Counter(n)| n
            \\}
            ,
        }},
        .expected = .{ .inspect_str = "41" },
    },
    .{
        .name = "inspect: cross-module polymorphic attached method specialization from helper module",
        .source_kind = .module,
        .source =
        \\import BoxMod
        \\import CountMod
        \\import Helpers
        \\
        \\main = (Helpers.read(BoxMod.Box(5)), Helpers.read(CountMod.Count(8)))
        ,
        .imports = &.{
            .{
                .name = "BoxMod",
                .source =
                \\Box := [Box(U64)].{
                \\  get : Box -> U64
                \\  get = |Box.Box(n)| n
                \\}
                ,
            },
            .{
                .name = "CountMod",
                .source =
                \\Count := [Count(U64)].{
                \\  get : Count -> U64
                \\  get = |Count.Count(n)| n + 100
                \\}
                ,
            },
            .{
                .name = "Helpers",
                .source =
                \\read = |value| value.get()
                ,
            },
        },
        .expected = .{ .inspect_str = "(5, 108)" },
    },
    .{
        .name = "inspect: record field access remains separate from method calls",
        .source =
        \\{
        \\    record = { get: |n| n + 1, value: 41 }
        \\    getter = record.get
        \\    getter(record.value)
        \\}
        ,
        .expected = .{ .inspect_str = "42.0" },
    },
    .{ .name = "inspect: empty record literal", .source = "{}", .expected = .{ .inspect_str = "{}" } },
    .{ .name = "inspect: decimal literal one eighth", .source = "0.125", .expected = .{ .inspect_str = "0.125" } },
    .{ .name = "inspect: decimal addition one tenth plus two tenths", .source = "0.1 + 0.2", .expected = .{ .inspect_str = "0.3" } },
    .{ .name = "inspect: int and f64 equality", .source = "1 == 1.0.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and decimal equality", .source = "1 == 1.0", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int less than", .source = "3 < 4", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int greater than false", .source = "5 > 8", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: f64 greater than", .source = "3.5.F64 > 1.25.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: decimal less than or equal", .source = "0.5 <= 0.5", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and f64 less than", .source = "1 < 2.0.F64", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: int and decimal greater than false", .source = "3 > 5.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: bool inequality", .source = "True != False", .expected = .{ .inspect_str = "True" } },
    .{ .name = "inspect: decimal inequality false", .source = "0.5 != 0.5", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: f64 equality false", .source = "3.25.F64 == 4.0.F64", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: decimal equality false", .source = "0.125 == 0.25", .expected = .{ .inspect_str = "False" } },
    .{ .name = "inspect: direct record literal render", .source = "{ x: 1, y: 2 }", .expected = .{ .inspect_str = "{ x: 1.0, y: 2.0 }" } },
    .{
        .name = "inspect: crash at end of if branch does not poison taken path",
        .source =
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
        .expected = .{ .inspect_str = "21.0" },
    },
    .{
        .name = "inspect: break inside for loop",
        .source =
        \\{
        \\    var $sum = 0
        \\    for i in [1, 2, 3, 4, 5] {
        \\        if i == 4 {
        \\            break
        \\        }
        \\        $sum = $sum + i
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
    .{
        .name = "inspect: break inside while loop",
        .source =
        \\{
        \\    var $i = 1
        \\    var $sum = 0
        \\    while $i <= 5 {
        \\        if $i == 4 {
        \\            break
        \\        }
        \\        $sum = $sum + $i
        \\        $i = $i + 1
        \\    }
        \\    $sum
        \\}
        ,
        .expected = .{ .inspect_str = "6.0" },
    },
};

pub const tests = core_tests ++ closure_recursion_tests.tests ++ recursive_data_tests.tests ++ low_level_tests.tests ++ polymorphism_tests.tests ++ interpreter_style_tests.tests;
