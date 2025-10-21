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
    try checkTypesExpr(source, .pass, "Num(_size)");
}

test "check type - num - int suffix 1" {
    const source =
        \\10u8
    ;
    try checkTypesExpr(source, .pass, "Num(Int(Unsigned8))");
}

test "check type - num - int suffix 2" {
    const source =
        \\10i128
    ;
    try checkTypesExpr(source, .pass, "Num(Int(Signed128))");
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
    try checkTypesExpr(source, .pass, "Num(Int(Unsigned128))");
}

test "check type - num - float" {
    const source =
        \\10.1
    ;
    try checkTypesExpr(source, .pass, "Num(Frac(_size))");
}

test "check type - num - float suffix 1" {
    const source =
        \\10.1f32
    ;
    try checkTypesExpr(source, .pass, "Num(Frac(Float32))");
}

test "check type - num - float suffix 2" {
    const source =
        \\10.1f64
    ;
    try checkTypesExpr(source, .pass, "Num(Frac(Float64))");
}

test "check type - num - float suffix 3" {
    const source =
        \\10.1dec
    ;
    try checkTypesExpr(source, .pass, "Num(Frac(Decimal))");
}

// primitives - strs //

test "check type - str" {
    const source =
        \\"hello"
    ;
    try checkTypesExpr(source, .pass, "Str");
}

// primitives - lists //

test "check type - list empty" {
    const source =
        \\[]
    ;
    try checkTypesExpr(source, .pass, "List(_elem)");
}

test "check type - list - same elems 1" {
    const source =
        \\["hello", "world"]
    ;
    try checkTypesExpr(source, .pass, "List(Str)");
}

test "check type - list - same elems 2" {
    const source =
        \\[100, 200]
    ;
    try checkTypesExpr(source, .pass, "List(Num(_size))");
}

test "check type - list - 1st elem more specific coreces 2nd elem" {
    const source =
        \\[100u64, 200]
    ;
    try checkTypesExpr(source, .pass, "List(Num(Int(Unsigned64)))");
}

test "check type - list - 2nd elem more specific coreces 1st elem" {
    const source =
        \\[100, 200u32]
    ;
    try checkTypesExpr(source, .pass, "List(Num(Int(Unsigned32)))");
}

test "check type - list  - diff elems 1" {
    const source =
        \\["hello", 10]
    ;
    try checkTypesExpr(source, .fail, "INCOMPATIBLE LIST ELEMENTS");
}

// number requirements //

test "check type - num - cannot coerce 500 to u8" {
    const source =
        \\[500, 200u8]
    ;
    try checkTypesExpr(source, .fail, "NUMBER DOES NOT FIT IN TYPE");
}

// records //

test "check type - record" {
    const source =
        \\{
        \\  hello: "Hello",
        \\  world: 10,
        \\}
    ;
    try checkTypesExpr(source, .pass, "{ hello: Str, world: Num(_size) }");
}

// tags //

test "check type - tag" {
    const source =
        \\MyTag
    ;
    try checkTypesExpr(source, .pass, "[MyTag]_others");
}

test "check type - tag - args" {
    const source =
        \\MyTag("hello", 1)
    ;
    try checkTypesExpr(source, .pass, "[MyTag(Str, Num(_size))]_others");
}

// blocks //

test "check type - block - return expr" {
    const source =
        \\{
        \\    "Hello"
        \\}
    ;
    try checkTypesExpr(source, .pass, "Str");
}

test "check type - block - implicit empty record" {
    const source =
        \\{
        \\    _test = "hello"
        \\}
    ;
    try checkTypesExpr(source, .pass, "{}");
}

test "check type - block - local value decl" {
    const source =
        \\{
        \\    test = "hello"
        \\
        \\    test
        \\}
    ;
    try checkTypesExpr(source, .pass, "Str");
}

// function //

test "check type - def - value" {
    const source =
        \\pairU64 = "hello"
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - def - func" {
    const source =
        \\id = |_| 20
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "_arg -> Num(_size)");
}

test "check type - def - id without annotation" {
    const source =
        \\id = |x| x
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a -> a");
}

test "check type - def - id with annotation" {
    const source =
        \\id : a -> a
        \\id = |x| x
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a -> a");
}

test "check type - def - func with annotation 1" {
    const source =
        \\id : x -> Str
        \\id = |_| "test"
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "x -> Str");
}

test "check type - def - func with annotation 2" {
    const source =
        \\id : x -> Num(_size)
        \\id = |_| 15
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "x -> Num(_size)");
}

test "check type - def - nested lambda" {
    const source =
        \\id = (((|a| |b| |c| a + b + c)(100))(20))(3)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
}

// calling functions

test "check type - def - monomorphic id" {
    const source =
        \\idStr : Str -> Str
        \\idStr = |x| x
        \\
        \\test = idStr("hello")
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - def - polymorphic id 1" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = id(5)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
}

test "check type - def - polymorphic id 2" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = (id(5), id("hello"))
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "(Num(_size), Str)");
}

test "check type - def - polymorphic higher order 1" {
    const source =
        \\f = |g, v| g(v)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a -> b, a -> b");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
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
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyAlias");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyListAlias(Num(size))");
}

test "check type - alias with mismatch arg" {
    const source =
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Str)
        \\x = [15]
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyNominal");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyNominal");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyNominal(Num(Int(Unsigned8)))");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Pair(Num(Int(Unsigned64)))");
}

test "check type - nominal with with rigid vars mismatch" {
    const source =
        \\Pair(a) := [Pair(a, a)]
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(1, "Str")
    ;
    try checkTypesModule(source, .fail, "INVALID NOMINAL TAG");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "ConsList(Str)");
}

test "check type - nominal recursive type anno mismatch" {
    const source =
        \\ConsList(a) := [Nil, Cons(a, ConsList(a))]
        \\
        \\x : ConsList(Num(size))
        \\x = ConsList.Cons("hello", ConsList.Nil)
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "ConsList(Elem(Str))");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "StrConsList");
}

test "check type - nominal recursive type wrong type" {
    const source =
        \\StrConsList := [Nil, Cons(Str, StrConsList)]
        \\
        \\x : StrConsList
        \\x = StrConsList.Cons(10, StrConsList.Nil)
    ;
    try checkTypesModule(source, .fail, "INVALID NOMINAL TAG");
}

test "check type - nominal w/ polymorphic function with bad args" {
    const source =
        \\Pair(a) := [Pair(a, a)]
        \\
        \\mkPairInvalid : a, b -> Pair(a)
        \\mkPairInvalid = |x, y| Pair.Pair(x, y)
    ;
    try checkTypesModule(source, .fail, "INVALID NOMINAL TAG");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "_arg -> Pair(Str, Num(_size))");
}

// bool

test "check type - bool unqualified" {
    const source =
        \\x : Bool
        \\x = True
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - bool qualified" {
    const source =
        \\x = Bool.True
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - bool lambda" {
    return error.SkipZigTest;
    // const source =
    //     \\module []
    //     \\
    //     \\x = (|x| !x)(Bool.True)
    // ;
    // try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

// if-else

test "check type - if else" {
    const source =
        \\x : Str
        \\x = if True "true" else "false"
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - if else - qualified bool" {
    return error.SkipZigTest; // Qualified tags from other modules don't work yet.
    // const source =
    //     \\x : Str
    //     \\x = if Bool.True "true" else "false"
    // ;
    // try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - if else - invalid condition 1" {
    const source =
        \\x : Str
        \\x = if 5 "true" else "false"
    ;
    try checkTypesModule(source, .fail, "INVALID IF CONDITION");
}

test "check type - if else - invalid condition 2" {
    const source =
        \\x : Str
        \\x = if 10 "true" else "false"
    ;
    try checkTypesModule(source, .fail, "INVALID IF CONDITION");
}

test "check type - if else - invalid condition 3" {
    const source =
        \\x : Str
        \\x = if "True" "true" else "false"
    ;
    try checkTypesModule(source, .fail, "INVALID IF CONDITION");
}

test "check type - if else - different branch types 1" {
    const source =
        \\x = if True "true" else 10
    ;
    try checkTypesModule(source, .fail, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 2" {
    const source =
        \\x = if True "true" else if False "false" else 10
    ;
    try checkTypesModule(source, .fail, "INCOMPATIBLE IF BRANCHES");
}

test "check type - if else - different branch types 3" {
    const source =
        \\x = if True "true" else if False 10 else "last"
    ;
    try checkTypesModule(source, .fail, "INCOMPATIBLE IF BRANCHES");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - match - diff cond types 1" {
    const source =
        \\x =
        \\  match "hello" {
        \\    True => "true"
        \\    False => "false"
        \\  }
    ;
    try checkTypesModule(source, .fail, "INCOMPATIBLE MATCH PATTERNS");
}

test "check type - match - diff branch types" {
    const source =
        \\x =
        \\  match True {
        \\    True => "true"
        \\    False => 100
        \\  }
    ;
    try checkTypesModule(source, .fail, "INCOMPATIBLE MATCH BRANCHES");
}

// unary not

test "check type - unary not" {
    const source =
        \\x = !True
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - unary not mismatch" {
    const source =
        \\x = !"Hello"
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// unary not

test "check type - unary minus" {
    const source =
        \\x = -10
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
}

test "check type - unary minus mismatch" {
    const source =
        \\x = "hello"
        \\
        \\y = -x
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// binops

test "check type - binops math plus" {
    const source =
        \\x = 10 + 10u32
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(Int(Unsigned32))");
}

test "check type - binops math sub" {
    const source =
        \\x = 1 - 0.2
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(Frac(_size))");
}

test "check type - binops ord" {
    const source =
        \\x = 10.0f32 > 15
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - binops and" {
    const source =
        \\x = True and False
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - binops and mismatch" {
    const source =
        \\x = "Hello" and False
    ;
    try checkTypesModule(source, .fail, "INVALID BOOL OPERATION");
}

test "check type - binops or" {
    const source =
        \\x = True or False
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

test "check type - binops or mismatch" {
    const source =
        \\x = "Hello" or False
    ;
    try checkTypesModule(source, .fail, "INVALID BOOL OPERATION");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - record access func polymorphic" {
    const source =
        \\x = |r| r.my_field
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ my_field: a } -> a");
}

test "check type - record access - not a record" {
    const source =
        \\r = "hello"
        \\
        \\x = r.my_field
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesExpr(source, .fail, "INCOMPATIBLE MATCH PATTERNS");
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
    try checkTypesExpr(source, .pass, "Str");
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
    try checkTypesExpr(source, .pass, "Str");
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
    try checkTypesExpr(source, .fail, "INCOMPATIBLE MATCH PATTERNS");
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
    try checkTypesExpr(source, .pass, "Str");
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
    try checkTypesExpr(source, .pass, "Str");
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
    try checkTypesExpr(source, .fail, "INCOMPATIBLE MATCH PATTERNS");
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
    try checkTypesExpr(source, .pass, "Num(Frac(Decimal))");
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
    try checkTypesExpr(source, .pass, "Num(Frac(Float32))");
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
    try checkTypesExpr(source, .pass, "Num(Frac(Float64))");
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
    try checkTypesExpr(source, .pass, "List(Str)");
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
    try checkTypesExpr(source, .pass, "Str");
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
    try checkTypesExpr(source, .pass, "Num(_size)");
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
    try checkTypesExpr(source, .fail, "INCOMPATIBLE MATCH PATTERNS");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Num(_size)");
}

test "check type - expect not bool" {
    const source =
        \\main = {
        \\  x = 1
        \\  expect x
        \\  x
        \\}
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// static dispatch //

test "check type - static dispatch - polymorphic - annotation" {
    const source =
        \\module []
        \\
        \\main : a -> Str where [a.to_str : a -> Str]
        \\main = |a| a.to_str()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "a -> Str where [a.to_str : a -> Str]",
    );
}

test "check type - static dispatch - polymorphic - no annotation" {
    const source =
        \\module []
        \\
        \\main = |x| x.to_str()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "a -> b where [a.to_str : a -> b]",
    );
}

test "check type - static dispatch - concrete - annotation" {
    const source =
        \\module []
        \\
        \\Test := [Val(Str)].{
        \\  to_str : Test -> Str
        \\  to_str = |Test.Val(s)| s
        \\}
        \\
        \\main : Str
        \\main = Test.Val("hello").to_str()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "Str",
    );
}

test "check type - static dispatch - concrete - no annotation" {
    const source =
        \\Test := [Val(Str)].{
        \\  to_str = |Test.Val(s)| s
        \\}
        \\
        \\main = Test.Val("hello").to_str()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "Str",
    );
}

test "check type - static dispatch - concrete - wrong method name" {
    const source =
        \\module []
        \\
        \\Test := [Val(Str)].{
        \\  to_str = |Test.Val(s)| s
        \\}
        \\
        \\main = Test.Val("hello").to_num()
    ;
    try checkTypesModule(
        source,
        .fail,
        "MISSING METHOD",
    );
}

test "check type - static dispatch - concrete - args" {
    const source =
        \\module []
        \\
        \\Test := [Val(U8)].{
        \\  add = |Test.Val(a), b| Test.Val(a + b)
        \\}
        \\
        \\main = Test.Val(1).add(1)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "Test",
    );
}

test "check type - static dispatch - concrete - wrong args" {
    const source =
        \\module []
        \\
        \\Test := [Val(U8)].{
        \\  add = |Test.Val(a), b| Test.Val(a + b)
        \\}
        \\
        \\main = Test.Val(1).add("hello")
    ;
    try checkTypesModule(
        source,
        .fail,
        "TYPE MISMATCH",
    );
}

test "check type - static dispatch - concrete - indirection 1" {
    const source =
        \\module []
        \\
        \\Test := [Val(Str)].{
        \\  to_str = |Test.Val(s)| s
        \\  to_str2 = |test| test.to_str()
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.to_str2" } },
        "a -> b where [a.to_str : a -> b]",
    );
}

test "check type - static dispatch - concrete - indirection 2" {
    const source =
        \\module []
        \\
        \\Test := [Val(Str)].{
        \\  to_str = |Test.Val(s)| s
        \\  to_str2 = |test| test.to_str()
        \\}
        \\
        \\main = Test.Val("hello").to_str2()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "Str",
    );
}

// helpers - module //

const ModuleExpectation = union(enum) {
    pass: DefExpectation,
    fail,
};

const DefExpectation = union(enum) {
    last_def,
    def: []const u8,
};

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
///
/// Behavior depends on the expectation:
/// Pass: Asserts whole module type checks, and assert the specified def matches the expected type string
/// Fail: Asserts that there is exactly 1 type error in the module and it's title matches the expected string
fn checkTypesModule(
    comptime source_expr: []const u8,
    comptime expectation: ModuleExpectation,
    comptime expected: []const u8,
) !void {
    var test_env = try TestEnv.init("Test", source_expr);
    defer test_env.deinit();

    switch (expectation) {
        .pass => |def_expectation| {
            switch (def_expectation) {
                .last_def => {
                    return test_env.assertLastDefType(expected);
                },
                .def => |def_name| {
                    return test_env.assertDefType(def_name, expected);
                },
            }
        },
        .fail => {
            return test_env.assertOneTypeError(expected);
        },
    }

    return test_env.assertLastDefType(expected);
}

// helpers - expr //

const ExprExpectation = union(enum) {
    pass,
    fail,
};

/// A unified helper to run the full pipeline: parse, canonicalize, and type-check source code.
///
/// Behavior depends on the expectation:
/// Pass: Asserts expr type checks, and asserts that the expr's type match the expected type string
/// Fail: Asserts that there is exactly 1 type error and it's title matches the expected string
fn checkTypesExpr(
    comptime source_expr: []const u8,
    comptime expectation: ExprExpectation,
    comptime expected: []const u8,
) !void {
    var test_env = try TestEnv.initExpr("Test", source_expr);
    defer test_env.deinit();

    switch (expectation) {
        .pass => {
            return test_env.assertLastDefType(expected);
        },
        .fail => {
            return test_env.assertOneTypeError(expected);
        },
    }

    return test_env.assertLastDefType(expected);
}
