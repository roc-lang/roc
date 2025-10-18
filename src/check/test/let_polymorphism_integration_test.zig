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
    try typeCheck(source, "{ a: Num(_size), b: Str }");
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
    try typeCheck(source, "{ a: Num(_size), b: Str }");
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
    try typeCheck(source, "Num(_size)");
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
    try typeCheck(source, "{ empty: List(_elem), nums: List(Num(_size)), strs: List(Str) }");
}

test "polymorphic cons function" {
    // This test is skipped because these features are missing:
    //   - Spread operator `..` in list literals [fails at parse stage - syntax not recognized]
    // TODO: Enable when spread operator is implemented in the parser
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    cons = |x, xs| [x, ..xs]
        \\    list1 = cons(1, [2, 3])
        \\    list2 = cons("a", ["b", "c"])
        \\    { list1, list2 }
        \\}
    ;
    try typeCheck(source, "TODO");
}

test "polymorphic map function" {
    // This test is skipped because these features are missing:
    //   - If-then-else expressions [fails at parse stage - syntax not recognized]
    //   - Recursive function calls [would fail at canonicalize stage - self-references not resolved]
    //   - List slicing `xs[1..]` [fails at parse stage - range syntax not recognized]
    //   - Spread operator `[x, ..xs]` [fails at parse stage - syntax not recognized]
    //   - List equality comparison `xs == []` [may fail at type-check stage]
    // Note: List indexing `xs[0]` does parse and canonicalize but may have type issues
    // TODO: Enable when conditional expressions, recursion, and list operations are implemented
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    map = |f, xs|
        \\        if xs == [] then
        \\            []
        \\        else
        \\            [f(xs[0]), ..map(f, xs[1..])]
        \\    double = |x| x * 2
        \\    nums = map(double, [1, 2, 3])
        \\    { nums }
        \\}
    ;
    try typeCheck(source, "TODO");
}

test "polymorphic record constructor" {
    const source =
        \\{
        \\    make_pair = |x, y| { first: x, second: y }
        \\    pair1 = make_pair(1, "a")
        \\    pair2 = make_pair("hello", 42)
        \\    pair3 = make_pair(True, False)
        \\    { pair1, pair2, pair3 }
        \\}
    ;
    try typeCheck(source, "{ pair1: { first: Num(_size), second: Str }, pair2: { first: Str, second: Num(_size2) }, pair3: { first: [True]_others, second: [False]_others2 } }");
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
    try typeCheck(source, "{ bool_val: [True]_others, float_val: Num(Frac(_size)), int_val: Num(_size2) }");
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
    try typeCheck(source, "{ box1: { value: Num(_size) }, box2: { value: Str }, nested: { value: { value: Num(_size2) } } }");
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
    try typeCheck(source, "{ a: Num(_size), b: Str }");
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
    try typeCheck(source, "{ swapped1: { first: Str, second: Num(_size) }, swapped2: { first: Num(_size2), second: [True]_others } }");
}

test "polymorphic fold function" {
    // This test is skipped because these features are missing:
    //   - If-then-else expressions [fails at parse stage - syntax not recognized]
    //   - Recursive function calls [would fail at canonicalize stage - self-references not resolved]
    //   - List equality comparison `xs == []` [may fail at type-check stage]
    //   - String concatenation operator `++` [fails at parse or canonicalize stage]
    //   - List slicing `xs[1..]` [fails at parse stage - range syntax not recognized]
    // Even if parsing succeeded, the canonicalizer doesn't support recursive
    // let-bindings, and the type checker doesn't handle recursive polymorphic functions.
    // TODO: Enable when conditional expressions, recursion, and list/string operations are implemented
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    fold = |f, acc, xs|
        \\        if xs == [] then
        \\            acc
        \\        else
        \\            fold(f, f(acc, xs[0]), xs[1..])
        \\    sum = fold(|a, b| a + b, 0, [1, 2, 3])
        \\    concat = fold(|a, b| a ++ b, "", ["a", "b", "c"])
        \\    { sum, concat }
        \\}
    ;
    try typeCheck(source, "TODO");
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
    try typeCheck(source, "{ opt1: { tag: Str, value: Num(_size) }, opt2: { tag: Str, value: Str }, opt3: { tag: Str } }");
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
    try typeCheck(source, "{ num: Num(_size), str: Str }");
}

test "shadowing of polymorphic values" {
    // This test is skipped because these features are missing:
    //   - Type checking for nested block expressions that return values
    //     [parses and canonicalizes successfully, fails at type-check stage]
    // The inner block `{ id = ...; b = ...; b }` should return `b` as its value.
    // The type checker fails to properly handle the combination of:
    //   1. A nested block that shadows a polymorphic identifier
    //   2. The block returning a value (the final `b` expression)
    //   3. Continuing to use the original polymorphic `id` after the block
    // TODO: Enable when nested block expressions with value returns are fully supported
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    inner = {
        \\        id = |x| x + 1  // shadows outer id, now monomorphic
        \\        b = id(2)
        \\        b
        \\    }
        \\    c = id("test")  // uses outer polymorphic id
        \\    { a, inner, c }
        \\}
    ;
    try typeCheck(source, "TODO");
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
    try typeCheck(source, "{ num_result: Num(_size), str_result: Num(_size2) }");
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(comptime source_expr: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.initExpr(source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}
