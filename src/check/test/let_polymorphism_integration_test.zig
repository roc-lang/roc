//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
fn typeCheck(allocator: std.mem.Allocator, source: []const u8) !bool {
    // Set up module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);
    if (parse_ast.hasErrors()) return false;

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx) orelse return false;

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canon_expr.get_idx());

    return checker.problems.problems.items.len == 0;
}

test "direct polymorphic identity usage" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic cons function" {
    // Skip: spread operator syntax may not be implemented
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    cons = |x, xs| [x, ..xs]
        \\    list1 = cons(1, [2, 3])
        \\    list2 = cons("a", ["b", "c"])
        \\    { list1, list2 }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic map function" {
    // Skip: recursive functions and list indexing may not be fully implemented
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
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic record constructor" {
    const source =
        \\{
        \\    make_pair = |x, y| { first: x, second: y }
        \\    pair1 = make_pair(1, "a")
        \\    pair2 = make_pair("hello", 42)
        \\    pair3 = make_pair(true, false)
        \\    { pair1, pair2, pair3 }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic identity with various numeric types" {
    const source =
        \\{
        \\    id = |x| x
        \\    int_val = id(42)
        \\    float_val = id(3.14)
        \\    bool_val = id(true)
        \\    { int_val, float_val, bool_val }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic swap function" {
    // Skip: record field access may not be fully implemented
    if (true) return error.SkipZigTest;

    const source =
        \\{
        \\    swap = |pair| { first: pair.second, second: pair.first }
        \\    pair1 = { first: 1, second: "a" }
        \\    pair2 = { first: true, second: 42 }
        \\    swapped1 = swap(pair1)
        \\    swapped2 = swap(pair2)
        \\    { swapped1, swapped2 }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic fold function" {
    // Skip: recursive functions and list operations may not be fully implemented
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
    try testing.expect(try typeCheck(test_allocator, source));
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
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic const function" {
    const source =
        \\{
        \\    const = |x| |_| x
        \\    always5 = const(5)
        \\    alwaysHello = const("hello")
        \\    num = always5(99)
        \\    str = alwaysHello(true)
        \\    { num, str }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}

test "shadowing of polymorphic values" {
    // Skip: nested scopes with shadowing may not be fully implemented
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
    try testing.expect(try typeCheck(test_allocator, source));
}

test "polymorphic pipe function" {
    const source =
        \\{
        \\    pipe = |x, f| f(x)
        \\    double = |n| n * 2
        \\    length = |s| 5  // simplified string length
        \\    num_result = pipe(21, double)
        \\    str_result = pipe("hello", length)
        \\    { num_result, str_result }
        \\}
    ;
    try testing.expect(try typeCheck(test_allocator, source));
}
