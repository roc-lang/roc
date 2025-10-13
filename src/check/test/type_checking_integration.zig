//! Integration tests for let-polymorphism that parse, canonicalize, and type-check
//! actual code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const types_mod = @import("types");
const problem_mod = @import("../problem.zig");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

// primitives - nums //

test "check type - num - unbound" {
    const source =
        \\50
    ;
    try assertExprTypeCheckPass(source, "Num(_size)");
}

test "check type - num - int suffix 1" {
    const source =
        \\10u8
    ;
    try assertExprTypeCheckPass(source, "Num(Int(Unsigned8))");
}

test "check type - num - int suffix 2" {
    const source =
        \\10i128
    ;
    try assertExprTypeCheckPass(source, "Num(Int(Signed128))");
}

test "check type - num - int big" {
    const source =
        \\{
        \\  e : U128
        \\  e = 340282366920938463463374607431768211455
        \\
        \\  e
        \\}
    ;
    try assertExprTypeCheckPass(source, "Num(Int(Unsigned128))");
}

test "check type - num - float" {
    const source =
        \\10.1
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(_size))");
}

test "check type - num - float suffix 1" {
    const source =
        \\10.1f32
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Float32))");
}

test "check type - num - float suffix 2" {
    const source =
        \\10.1f64
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Float64))");
}

test "check type - num - float suffix 3" {
    const source =
        \\10.1dec
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Decimal))");
}

// primitives - strs //

test "check type - str" {
    const source =
        \\"hello"
    ;
    try assertExprTypeCheckPass(source, "Str");
}

// primitives - lists //

test "check type - list empty" {
    const source =
        \\[]
    ;
    try assertExprTypeCheckPass(source, "List(_elem)");
}

test "check type - list - same elems 1" {
    const source =
        \\["hello", "world"]
    ;
    try assertExprTypeCheckPass(source, "List(Str)");
}

test "check type - list - same elems 2" {
    const source =
        \\[100, 200]
    ;
    try assertExprTypeCheckPass(source, "List(Num(_size))");
}

test "check type - list - 1st elem more specific coreces 2nd elem" {
    const source =
        \\[100u64, 200]
    ;
    try assertExprTypeCheckPass(source, "List(Num(Int(Unsigned64)))");
}

test "check type - list - 2nd elem more specific coreces 1st elem" {
    const source =
        \\[100, 200u32]
    ;
    try assertExprTypeCheckPass(source, "List(Num(Int(Unsigned32)))");
}

test "check type - list  - diff elems 1" {
    const source =
        \\["hello", 10]
    ;
    try assertExprTypeCheckFail(source, "INCOMPATIBLE LIST ELEMENTS");
}

// number requirements //

test "check type - num - cannot coerce 500 to u8" {
    const source =
        \\[500, 200u8]
    ;
    try assertExprTypeCheckFail(source, "NUMBER DOES NOT FIT IN TYPE");
}

// records //

test "check type - record" {
    const source =
        \\{
        \\  hello: "Hello",
        \\  world: 10,
        \\}
    ;
    try assertExprTypeCheckPass(source, "{ hello: Str, world: Num(_size) }");
}

// tags //

test "check type - tag" {
    const source =
        \\MyTag
    ;
    try assertExprTypeCheckPass(source, "[MyTag]_others");
}

test "check type - tag - args" {
    const source =
        \\MyTag("hello", 1)
    ;
    try assertExprTypeCheckPass(source, "[MyTag(Str, Num(_size))]_others");
}

// blocks //

test "check type - block - return expr" {
    const source =
        \\{
        \\    "Hello"
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - block - implicit empty record" {
    const source =
        \\{
        \\    _test = "hello"
        \\}
    ;
    try assertExprTypeCheckPass(source, "{}");
}

test "check type - block - local value decl" {
    const source =
        \\{
        \\    test = "hello"
        \\
        \\    test
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

// function //

test "check type - def - value" {
    const source =
        \\pairU64 = "hello"
    ;
    try assertFileTypeCheckPass(source, "Str");
}

test "check type - def - func" {
    const source =
        \\id = |_| 20
    ;
    try assertFileTypeCheckPass(source, "_arg -> Num(_size)");
}

test "check type - def - id without annotation" {
    const source =
        \\id = |x| x
    ;
    try assertFileTypeCheckPass(source, "a -> a");
}

test "check type - def - id with annotation" {
    const source =
        \\id : a -> a
        \\id = |x| x
    ;
    try assertFileTypeCheckPass(source, "a -> a");
}

test "check type - def - func with annotation 1" {
    const source =
        \\id : x -> Str
        \\id = |_| "test"
    ;
    try assertFileTypeCheckPass(source, "x -> Str");
}

test "check type - def - func with annotation 2" {
    const source =
        \\id : x -> Num(_size)
        \\id = |_| 15
    ;
    try assertFileTypeCheckPass(source, "x -> Num(_size)");
}

test "check type - def - nested lambda" {
    const source =
        \\id = (((|a| |b| |c| a + b + c)(100))(20))(3)
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

// calling functions

test "check type - def - monomorphic id" {
    const source =
        \\idStr : Str -> Str
        \\idStr = |x| x
        \\
        \\test = idStr("hello")
    ;
    try assertFileTypeCheckPass(source, "Str");
}

test "check type - def - polymorphic id 1" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = id(5)
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

test "check type - def - polymorphic id 2" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = (id(5), id("hello"))
    ;
    try assertFileTypeCheckPass(source, "(Num(_size), Str)");
}

test "check type - def - polymorphic higher order 1" {
    const source =
        \\f = |g, v| g(v)
    ;
    try assertFileTypeCheckPass(source, "a -> b, a -> b");
}

test "check type - top level polymorphic function is generalized" {
    const source =
        \\id = |x| x
        \\
        \\main = {
        \\    a = id(42)
        \\    _b = id("hello")
        \\    a
        \\}
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

test "check type - let-def polymorphic function is generalized" {
    const source =
        \\main = {
        \\    id = |x| x
        \\    a = id(42)
        \\    _b = id("hello")
        \\    a
        \\}
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

test "check type - polymorphic function function param should be constrained" {
    const source =
        \\id = |x| x
        \\
        \\use_twice = |f| {
        \\    a = f(42)
        \\    b = f("hello")
        \\    a
        \\}
        \\result = use_twice(id)
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// type aliases //

test "check type - basic alias" {
    const source =
        \\main! = |_| {}
        \\
        \\MyAlias : Str
        \\
        \\x : MyAlias
        \\x = "hello"
    ;
    try assertFileTypeCheckPass(source, "MyAlias");
}

test "check type - alias with arg" {
    const source =
        \\main! = |_| {}
        \\
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Num(size))
        \\x = [15]
    ;
    try assertFileTypeCheckPass(source, "MyListAlias(Num(size))");
}

test "check type - alias with mismatch arg" {
    const source =
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Str)
        \\x = [15]
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// nominal types //

test "check type - basic nominal" {
    const source =
        \\main! = |_| {}
        \\
        \\MyNominal := [MyNominal]
        \\
        \\x : MyNominal
        \\x = MyNominal.MyNominal
    ;
    try assertFileTypeCheckPass(source, "MyNominal");
}

test "check type - nominal with tag arg" {
    const source =
        \\main! = |_| {}
        \\
        \\MyNominal := [MyNominal(Str)]
        \\
        \\x : MyNominal
        \\x = MyNominal.MyNominal("hello")
    ;
    try assertFileTypeCheckPass(source, "MyNominal");
}

test "check type - nominal with type and tag arg" {
    const source =
        \\main! = |_| {}
        \\
        \\MyNominal(a) := [MyNominal(a)]
        \\
        \\x : MyNominal(U8)
        \\x = MyNominal.MyNominal(10)
    ;
    try assertFileTypeCheckPass(source, "MyNominal(Num(Int(Unsigned8)))");
}

test "check type - nominal with with rigid vars" {
    const source =
        \\main! = |_| {}
        \\
        \\Pair(a) := [Pair(a, a)]
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(1, 2)
    ;
    try assertFileTypeCheckPass(source, "Pair(Num(Int(Unsigned64)))");
}

test "check type - nominal with with rigid vars mismatch" {
    const source =
        \\Pair(a) := [Pair(a, a)]
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(1, "Str")
    ;
    try assertFileTypeCheckFail(source, "INVALID NOMINAL TAG");
}

test "check type - nominal recursive type" {
    const source =
        \\main! = |_| {}
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x : ConsList(Str)
        \\x = ConsList.Cons("hello", ConsList.Nil)
    ;
    try assertFileTypeCheckPass(source, "ConsList(Str)");
}

test "check type - nominal recursive type anno mismatch" {
    const source =
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x : ConsList(Num(size))
        \\x = ConsList.Cons("hello", ConsList.Nil)
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

test "check type - two nominal types" {
    const source =
        \\main! = |_| {}
        \\
        \\Elem(a) := [Elem(a)]
        \\
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x = ConsList.Cons(Elem.Elem("hello"), ConsList.Nil)
    ;
    try assertFileTypeCheckPass(source, "ConsList(Elem(Str))");
}

test "check type - nominal recursive type no args" {
    const source =
        \\main! = |_| {}
        \\
        \\StrConsList := [Nil, Cons(Str, StrConsList)]
        \\
        \\x : StrConsList
        \\x = StrConsList.Cons("hello", StrConsList.Nil)
    ;
    try assertFileTypeCheckPass(source, "StrConsList");
}

test "check type - nominal recursive type wrong type" {
    const source =
        \\StrConsList := [Nil, Cons(Str, StrConsList)]
        \\
        \\x : StrConsList
        \\x = StrConsList.Cons(10, StrConsList.Nil)
    ;
    try assertFileTypeCheckFail(source, "INVALID NOMINAL TAG");
}

test "check type - nominal w/ polymorphic function with bad args" {
    const source =
        \\Pair(a) := [Pair(a, a)]
        \\
        \\mkPairInvalid : a, b -> Pair(a)
        \\mkPairInvalid = |x, y| Pair.Pair(x, y)
    ;
    try assertFileTypeCheckFail(source, "INVALID NOMINAL TAG");
}

test "check type - nominal w/ polymorphic function" {
    const source =
        \\main! = |_| {}
        \\
        \\Pair(a, b) : (a, b)
        \\
        \\swapPair : Pair(a, b) -> Pair(b, a)
        \\swapPair = |(x, y)| (y, x)
        \\
        \\test = |_| swapPair((1, "test"))
    ;
    try assertFileTypeCheckPass(source, "_arg -> Pair(Str, Num(_size))");
}

// bool

test "check type - bool unqualified" {
    const source =
        \\x : Bool
        \\x = True
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - bool qualified" {
    const source =
        \\x = Bool.True
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - bool lambda" {
    return error.SkipZigTest;
    // const source =
    //     \\x = (|x| !x)(Bool.True)
    // ;
    // try assertFileTypeCheckPass(source, "Bool");
}

// if-else

test "check type - if else" {
    const source =
        \\x : Str
        \\x = if True "true" else "false"
    ;
    try assertFileTypeCheckPass(source, "Str");
}

test "check type - if else - qualified bool" {
    return error.SkipZigTest; // Qualified tags from other modules don't work yet.
    // const source =
    //     \\x : Str
    //     \\x = if Bool.True "true" else "false"
    // ;
    // try assertFileTypeCheckPass(source, "Str");
}

test "check type - if else - invalid condition 1" {
    const source =
        \\x : Str
        \\x = if 5 "true" else "false"
    ;
    try assertFileTypeCheckFail(source, "INVALID IF CONDITION");
}

test "check type - if else - invalid condition 2" {
    const source =
        \\x : Str
        \\x = if 10 "true" else "false"
    ;
    try assertFileTypeCheckFail(source, "INVALID IF CONDITION");
}

test "check type - if else - invalid condition 3" {
    const source =
        \\x : Str
        \\x = if "True" "true" else "false"
    ;
    try assertFileTypeCheckFail(source, "INVALID IF CONDITION");
}

test "check type - if else - different branch types 1" {
    const source =
        \\x = if True "true" else 10
    ;
    try assertFileTypeCheckFail(source, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 2" {
    const source =
        \\x = if True "true" else if False "false" else 10
    ;
    try assertFileTypeCheckFail(source, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 3" {
    const source =
        \\x = if True "true" else if False 10 else "last"
    ;
    try assertFileTypeCheckFail(source, "INCOMPATIBLE IF BRANCHES");
}

// match

test "check type - match" {
    const source =
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => "false"
        \\  }
    ;
    try assertFileTypeCheckPass(source, "Str");
}

test "check type - match - diff cond types 1" {
    const source =
        \\x =
        \\  match "hello" {
        \\    True => "true"
        \\    False => "false"
        \\  }
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

test "check type - match - diff branch types" {
    const source =
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => 100
        \\  }
    ;
    try assertFileTypeCheckFail(source, "INCOMPATIBLE MATCH BRANCHES");
}

// unary not

test "check type - unary not" {
    const source =
        \\x = !True
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - unary not mismatch" {
    const source =
        \\x = !"Hello"
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// unary not

test "check type - unary minus" {
    const source =
        \\x = -10
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

test "check type - unary minus mismatch" {
    const source =
        \\x = "hello"
        \\
        \\y = -x
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// binops

test "check type - binops math plus" {
    const source =
        \\x = 10 + 10u32
    ;
    try assertFileTypeCheckPass(source, "Num(Int(Unsigned32))");
}

test "check type - binops math sub" {
    const source =
        \\x = 1 - 0.2
    ;
    try assertFileTypeCheckPass(source, "Num(Frac(_size))");
}

test "check type - binops ord" {
    const source =
        \\x = 10.0f32 > 15
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - binops and" {
    const source =
        \\x = True and False
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - binops and mismatch" {
    const source =
        \\x = "Hello" and False
    ;
    try assertFileTypeCheckFail(source, "INVALID BOOL OPERATION");
}

test "check type - binops or" {
    const source =
        \\x = True or False
    ;
    try assertFileTypeCheckPass(source, "Bool");
}

test "check type - binops or mismatch" {
    const source =
        \\x = "Hello" or False
    ;
    try assertFileTypeCheckFail(source, "INVALID BOOL OPERATION");
}

// record access

test "check type - record access" {
    const source =
        \\r =
        \\  {
        \\    hello: "Hello",
        \\    world: 10,
        \\  }
        \\
        \\x = r.hello
    ;
    try assertFileTypeCheckPass(source, "Str");
}

test "check type - record access func polymorphic" {
    const source =
        \\x = |r| r.my_field
    ;
    try assertFileTypeCheckPass(source, "{ my_field: a } -> a");
}

test "check type - record access - not a record" {
    const source =
        \\r = "hello"
        \\
        \\x = r.my_field
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// tags //

test "check type - patterns - wrong type" {
    const source =
        \\{
        \\  x = True
        \\
        \\  match(x) {
        \\    "hello" => "world",
        \\  }
        \\}
    ;
    try assertExprTypeCheckFail(source, "TYPE MISMATCH");
}

test "check type - patterns tag without payload" {
    const source =
        \\{
        \\  x = True
        \\
        \\  match(x) {
        \\    True => "true",
        \\    False => "false",
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - patterns tag with payload" {
    const source =
        \\{
        \\  x = Ok("ok")
        \\
        \\  match(x) {
        \\    Ok(val) => val,
        \\    Err(_) => "err",
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - patterns tag with payload mismatch" {
    const source =
        \\{
        \\  x = Ok("ok")
        \\
        \\  match(x) {
        \\    Ok(True) => 10 * 10,
        \\    Err(_) => 0,
        \\  }
        \\}
    ;
    try assertExprTypeCheckFail(source, "TYPE MISMATCH");
}

test "check type - patterns str" {
    const source =
        \\{
        \\  x = "hello"
        \\
        \\  match(x) {
        \\    "world" => "true",
        \\    _ => "false",
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - patterns num" {
    const source =
        \\{
        \\  x = 10
        \\
        \\  match(x) {
        \\    10 => "true",
        \\    _ => "false",
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - patterns int mismatch" {
    const source =
        \\{
        \\  x = 10u8
        \\
        \\  match(x) {
        \\    10u32 => "true",
        \\    _ => "false",
        \\  }
        \\}
    ;
    try assertExprTypeCheckFail(source, "TYPE MISMATCH");
}

test "check type - patterns frac 1" {
    const source =
        \\{
        \\  x = 10.0dec
        \\
        \\  match(x) {
        \\    10 => x,
        \\    _ => 15,
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Decimal))");
}

test "check type - patterns frac 2" {
    const source =
        \\{
        \\  x = 10.0
        \\
        \\  match(x) {
        \\    10f32 => x,
        \\    _ => 15,
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Float32))");
}

test "check type - patterns frac 3" {
    const source =
        \\{
        \\  x = 10.0
        \\
        \\  match(x) {
        \\    10 => x,
        \\    15f64 => x,
        \\    _ => 20,
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Num(Frac(Float64))");
}

test "check type - patterns list" {
    const source =
        \\{
        \\  x = ["a", "b", "c"]
        \\
        \\  match(x) {
        \\    [.. as b, _a]  => b,
        \\    [_a, .. as b]  => b,
        \\    []  => [],
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "List(Str)");
}

test "check type - patterns record" {
    const source =
        \\{
        \\  val = { x: "hello", y: True }
        \\
        \\  match(val) {
        \\    { y: False }  => "False",
        \\    { x }  => x,
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Str");
}

test "check type - patterns record 2" {
    const source =
        \\{
        \\  val = { x: "hello", y: True }
        \\
        \\  match(val) {
        \\    { y: False, x: "world" }  => 10
        \\    _  => 20,
        \\  }
        \\}
    ;
    try assertExprTypeCheckPass(source, "Num(_size)");
}

test "check type - patterns record field mismatch" {
    const source =
        \\{
        \\  val = { x: "hello" }
        \\
        \\  match(val) {
        \\    { x: False } => 10
        \\    _ => 20
        \\  }
        \\}
    ;
    try assertExprTypeCheckFail(source, "TYPE MISMATCH");
}

// vars

test "check type - var ressignment" {
    const source =
        \\main = {
        \\  var x = 1
        \\  x = x + 1
        \\  x
        \\}
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

// expect

test "check type - expect" {
    const source =
        \\main = {
        \\  x = 1
        \\  expect x == 1
        \\  x
        \\}
    ;
    try assertFileTypeCheckPass(source, "Num(_size)");
}

test "check type - expect not bool" {
    const source =
        \\main = {
        \\  x = 1
        \\  expect x
        \\  x
        \\}
    ;
    try assertFileTypeCheckFail(source, "TYPE MISMATCH");
}

// helpers  //

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that type checking the expr passes
fn assertExprTypeCheckPass(comptime source_expr: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.initExpr(source_expr);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that type checking the expr fails with exactly one problem, and the title of the problem matches the provided one.
fn assertExprTypeCheckFail(comptime source_expr: []const u8, expected_problem_title: []const u8) !void {
    var test_env = try TestEnv.initExpr(source_expr);
    defer test_env.deinit();
    return test_env.assertOneTypeError(expected_problem_title);
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that the type of the final definition in the source matches the one provided
fn assertFileTypeCheckPass(source: []const u8, expected_type: []const u8) !void {
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();
    return test_env.assertLastDefType(expected_type);
}

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
/// Asserts that the type of the final definition in the source matches the one provided
fn assertFileTypeCheckFail(source: []const u8, expected_problem_title: []const u8) !void {
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();
    return test_env.assertOneTypeError(expected_problem_title);
}
