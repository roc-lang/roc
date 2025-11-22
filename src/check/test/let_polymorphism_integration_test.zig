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
    try typeCheck(source, "{ a: _field, b: Str } where [_c.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ a: _field, b: Str } where [_c.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "_a where [_b.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ empty: List(_a), nums: List(_b), strs: List(Str) } where [_c.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ list1: List(item), list2: List(Str) } where [item.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ pair1: { first: _field, second: Str }, pair2: { first: Str, second: _field2 }, pair3: { first: [True]_others, second: [False]_others2 } } where [_a.from_numeral : _arg -> _ret, _b.from_numeral : _arg2 -> _ret2]");
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
    try typeCheck(source, "{ bool_val: [True]_others, float_val: _field, int_val: _field2 } where [_a.from_numeral : _arg -> _ret, _b.from_numeral : _arg2 -> _ret2]");
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
    try typeCheck(source, "{ box1: { value: _field }, box2: { value: Str }, nested: { value: { value: _field2 } } } where [_a.from_numeral : _arg -> _ret, _b.from_numeral : _arg2 -> _ret2]");
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
    try typeCheck(source, "{ a: _field, b: Str } where [_c.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ swapped1: { first: Str, second: _field }, swapped2: { first: _field2, second: [True]_others } } where [_a.from_numeral : _arg -> _ret, _b.from_numeral : _arg2 -> _ret2]");
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
    try typeCheck(source, "{ opt1: { tag: Str, value: _field }, opt2: { tag: Str, value: Str }, opt3: { tag: Str } } where [_a.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ num: _field, str: Str } where [_a.from_numeral : _arg -> _ret]");
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
    try typeCheck(source, "{ num_result: _field, str_result: _field2 } where [_a.from_numeral : _arg -> _ret, _b.from_numeral : _arg2 -> _ret2]");
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(comptime source_expr: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.initExpr("Test", source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}
