//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

test "direct polymorphic identity usage" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;
    // The field 'a' has the same type as the dispatcher for from_numeral, so they should share the same name
    // Note: 'c' is used because 'a' and 'b' are already identifiers in the code
    // String literal is now polymorphic with try_from_str constraint
    try typeCheck(source, "{ a: c, b: d } where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.try_from_str : Str -> Try(d, [InvalidStr(Str)])]");
}

test "higher-order function with polymorphic identity" {
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;
    // String literal is now polymorphic with try_from_str constraint
    try typeCheck(source, "{ a: c, b: d } where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.try_from_str : Str -> Try(d, [InvalidStr(Str)])]");
}

test "let-polymorphism with function composition" {
    const source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    num_compose = compose(double, add_one)
        \\    result1 = num_compose(5)
        \\    { result1 }
        \\}
    ;
    try typeCheck(source, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "polymorphic empty list" {
    const source =
        \\{
        \\    empty = []
        \\    nums = [1, 2, 3]
        \\    strs = ["a", "b", "c"]
        \\    { empty, nums, strs }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ empty: List(_a), nums: List(b), strs: List(c) } where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.try_from_str : Str -> Try(c, [InvalidStr(Str)])]");
}

test "polymorphic cons function" {
    const source =
        \\{
        \\    cons = |x, xs| List.concat([x], xs)
        \\    list1 = cons(1, [2, 3])
        \\    list2 = cons("a", ["b", "c"])
        \\    { list1, list2 }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    // Note: both lists share the same type variable due to let-polymorphism
    try typeCheck(source, "{ list1: List(item), list2: List(item) } where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)]), item.try_from_str : Str -> Try(item, [InvalidStr(Str)])]");
}

test "polymorphic record constructor" {
    const source =
        \\{
        \\    make_pair = |x, y| { first: x, second: y }
        \\    pair1 = make_pair(1, "a")
        \\    pair2 = make_pair("b", 42)
        \\    pair3 = make_pair(True, False)
        \\    { pair1, pair2, pair3 }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ pair1: { first: a, second: b }, pair2: { first: c, second: d }, pair3: { first: [True]_others, second: [False]_others2 } } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.try_from_str : Str -> Try(b, [InvalidStr(Str)]), c.try_from_str : Str -> Try(c, [InvalidStr(Str)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]");
}

test "polymorphic identity with various numeric types" {
    const source =
        \\{
        \\    id = |x| x
        \\    int_val = id(42)
        \\    float_val = id(3.14)
        \\    bool_val = id(True)
        \\    { int_val, float_val, bool_val }
        \\}
    ;
    try typeCheck(source, "{ bool_val: [True]_others, float_val: a, int_val: b } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]");
}

test "nested polymorphic data structures" {
    const source =
        \\{
        \\    make_box = |x| { value: x }
        \\    box1 = make_box(42)
        \\    box2 = make_box("hello")
        \\    nested = make_box(make_box(100))
        \\    { box1, box2, nested }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ box1: { value: a }, box2: { value: b }, nested: { value: { value: c } } } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.try_from_str : Str -> Try(b, [InvalidStr(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]");
}

test "polymorphic function in let binding" {
    const source =
        \\{
        \\    result = {
        \\        id = |x| x
        \\        a = id(1)
        \\        b = id("test")
        \\        { a, b }
        \\    }
        \\    result
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ a: c, b: d } where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.try_from_str : Str -> Try(d, [InvalidStr(Str)])]");
}

test "polymorphic swap function" {
    const source =
        \\{
        \\    swap = |pair| { first: pair.second, second: pair.first }
        \\    pair1 = { first: 1, second: "a" }
        \\    pair2 = { first: True, second: 42 }
        \\    swapped1 = swap(pair1)
        \\    swapped2 = swap(pair2)
        \\    { swapped1, swapped2 }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ swapped1: { first: a, second: b }, swapped2: { first: c, second: [True]_others } } where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]");
}

test "polymorphic option type simulation" {
    const source =
        \\{
        \\    none = { tag: "None" }
        \\    some = |x| { tag: "Some", value: x }
        \\    opt1 = some(42)
        \\    opt2 = some("hello")
        \\    opt3 = none
        \\    { opt1, opt2, opt3 }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ opt1: { tag: a, value: b }, opt2: { tag: c, value: d }, opt3: { tag: e } } where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.try_from_str : Str -> Try(c, [InvalidStr(Str)]), d.try_from_str : Str -> Try(d, [InvalidStr(Str)]), e.try_from_str : Str -> Try(e, [InvalidStr(Str)])]");
}

test "polymorphic const function" {
    const source =
        \\{
        \\    const = |x| |_| x
        \\    always5 = const(5)
        \\    alwaysHello = const("hello")
        \\    num = always5(99)
        \\    str = alwaysHello(True)
        \\    { num, str }
        \\}
    ;
    // String literals are now polymorphic with try_from_str constraint
    try typeCheck(source, "{ num: a, str: b } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.try_from_str : Str -> Try(b, [InvalidStr(Str)])]");
}

test "polymorphic pipe function" {
    const source =
        \\{
        \\    pipe = |x, f| f(x)
        \\    double = |n| n * 2
        \\    length = |_s| 5
        \\    num_result = pipe(21, double)
        \\    str_result = pipe("hello", length)
        \\    { num_result, str_result }
        \\}
    ;
    try typeCheck(source, "{ num_result: a, str_result: b } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]");
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(comptime source_expr: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.initExpr("Test", source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}
