//! Ported interpreter polymorphism tests into the inspect-only runner.

const TestCase = @import("parallel_runner.zig").TestCase;

/// Public value `tests`.
pub const tests = [_]TestCase{
    .{
        .name = "interpreter poly: return a function then call (int)",
        .source = "(|_| (|x| x))(0)(42)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter poly: return a function then call (string)",
        .source = "(|_| (|x| x))(0)(\"hi\")",
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "interpreter captures (monomorphic): adder",
        .source = "(|n| (|x| n + x))(1)(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter captures (monomorphic): constant function",
        .source = "(|x| (|_| x))(\"hi\")(0)",
        .expected = .{ .inspect_str = "\"hi\"" },
    },
    .{
        .name = "interpreter captures (polymorphic): capture id and apply to int",
        .source = "((|id| (|x| id(x)))(|y| y))(41)",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "interpreter captures (polymorphic): capture id and apply to string",
        .source = "((|id| (|x| id(x)))(|y| y))(\"ok\")",
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "interpreter higher-order: apply f then call with 41",
        .source = "((|f| (|x| f(x)))(|n| n + 1))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter higher-order: apply f twice",
        .source = "((|f| (|x| f(f(x))))(|n| n + 1))(40)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter higher-order: compose id with +1",
        .source = "(((|f| (|g| (|x| f(g(x)))))(|n| n + 1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter higher-order: construct then pass then call",
        .source = "((|make| (|z| (make(|n| n + 1))(z)))(|f| (|x| f(x))))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter higher-order: pass constructed closure and apply",
        .source = "(|g| g(41))((|f| (|x| f(x)))(|y| y))",
        .expected = .{ .inspect_str = "41.0" },
    },
    .{
        .name = "interpreter higher-order: return poly fn using captured +n",
        .source = "(((|n| (|id| (|x| id(x + n))))(1))(|y| y))(41)",
        .expected = .{ .inspect_str = "42.0" },
    },
    .{
        .name = "interpreter if: else-if chain selects middle branch",
        .source =
        \\{
        \\    n = 1
        \\    if n == 0 { "zero" } else if n == 1 { "one" } else { "other" }
        \\}
        ,
        .expected = .{ .inspect_str = "\"one\"" },
    },
    .{
        .name = "interpreter logical or is short-circuiting",
        .source = "if ((1 == 1) or { crash \"nope\" }) { \"ok\" } else { \"bad\" }",
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "interpreter logical and is short-circuiting",
        .source = "if ((1 == 0) and { crash \"nope\" }) { \"bad\" } else { \"ok\" }",
        .expected = .{ .inspect_str = "\"ok\"" },
    },
    .{
        .name = "interpreter recursion: factorial 5 -> 120",
        .source =
        \\{
        \\    fact = (|n| if n == 0 { 1 } else { n * fact(n - 1) })
        \\    fact(5)
        \\}
        ,
        .expected = .{ .inspect_str = "120.0" },
    },
    .{
        .name = "interpreter recursion: fibonacci 5 -> 5",
        .source =
        \\{
        \\    fib = (|n| if n == 0 { 0 } else if n == 1 { 1 } else { fib(n - 1) + fib(n - 2) })
        \\    fib(5)
        \\}
        ,
        .expected = .{ .inspect_str = "5.0" },
    },
    .{
        .name = "interpreter recursion: simple countdown",
        .source =
        \\{
        \\    rec = (|n| if n == 0 { 0 } else { rec(n - 1) + 1 })
        \\    rec(2)
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
    .{
        .name = "interpreter tag union: one-arg tag Ok(42)",
        .source = "Ok(42.0)",
        .expected = .{ .inspect_str = "Ok(42.0)" },
    },
    .{
        .name = "interpreter tag union: multi-arg tag Point(1, 2)",
        .source = "Point(1.0, 2.0)",
        .expected = .{ .inspect_str = "Point(1.0, 2.0)" },
    },
    .{
        .name = "interpreter tag union: nested tag in tuple in tag (issue #8750)",
        .source = "Ok((Name(\"hello\"), 5))",
        .expected = .{ .inspect_str = "Ok((Name(\"hello\"), 5.0))" },
    },
    .{
        .name = "interpreter var and reassign",
        .source =
        \\{
        \\    var x = 1
        \\    x = x + 1
        \\    x
        \\}
        ,
        .expected = .{ .inspect_str = "2.0" },
    },
};
