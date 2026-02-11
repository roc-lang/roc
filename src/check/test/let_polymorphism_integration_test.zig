//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const TestEnv = @import("./TestEnv.zig");

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
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
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
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
}

test "let-polymorphism with function composition" {
    const source =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add_one = |x| x + 1
        \\    compose(double, add_one)
        \\}
    ;
    try typeCheck(
        source,
        "a -> a where [a.plus : a, Dec -> a, a.times : a, Dec -> a]",
    );
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
    try typeCheck(
        source,
        "{ empty: List(_a), nums: List(Dec), strs: List(Str) }",
    );
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
    try typeCheck(
        source,
        "{ list1: List(Dec), list2: List(Str) }",
    );
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
    try typeCheck(
        source,
        "{ pair1: { first: Dec, second: Str }, pair2: { first: Str, second: Dec }, pair3: { first: [True, ..], second: [False, ..] } }",
    );
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
    try typeCheck(
        source,
        "{ bool_val: [True, ..], float_val: Dec, int_val: Dec }",
    );
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
    try typeCheck(
        source,
        "{ box1: { value: Dec }, box2: { value: Str }, nested: { value: { value: Dec } } }",
    );
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
    try typeCheck(
        source,
        "{ a: Dec, b: Str }",
    );
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
    try typeCheck(
        source,
        "{ swapped1: { first: Str, second: Dec }, swapped2: { first: Dec, second: [True, ..] } }",
    );
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
    try typeCheck(
        source,
        "{ opt1: { tag: Str, value: Dec }, opt2: { tag: Str, value: Str }, opt3: { tag: Str } }",
    );
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
    try typeCheck(
        source,
        "{ num: Dec, str: Str }",
    );
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
    try typeCheck(
        source,
        "{ num_result: Dec, str_result: Dec }",
    );
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(comptime source_expr: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.initExpr("Test", source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}
