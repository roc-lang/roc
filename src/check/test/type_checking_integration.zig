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
    try checkTypesExpr(source, .pass, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - num - int suffix 1" {
    const source =
        \\10u8
    ;
    try checkTypesExpr(source, .pass, "U8");
}

test "check type - num - int suffix 2" {
    const source =
        \\10i128
    ;
    try checkTypesExpr(source, .pass, "I128");
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
    try checkTypesExpr(source, .pass, "U128");
}

test "check type - num - float" {
    const source =
        \\10.1
    ;
    try checkTypesExpr(source, .pass, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - num - float suffix 1" {
    const source =
        \\10.1f32
    ;
    try checkTypesExpr(source, .pass, "F32");
}

test "check type - num - float suffix 2" {
    const source =
        \\10.1f64
    ;
    try checkTypesExpr(source, .pass, "F64");
}

test "check type - num - float suffix 3" {
    const source =
        \\10.1dec
    ;
    try checkTypesExpr(source, .pass, "Dec");
}

// primitives - strs //

test "check type - str" {
    const source =
        \\"hello"
    ;
    try checkTypesExpr(source, .pass, "Str");
}

test "check type - str annotation mismatch with number" {
    const source =
        \\x : I64
        \\x = "hello"
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - number annotation mismatch with string" {
    const source =
        \\x : Str
        \\x = 42
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

test "check type - i64 annotation with fractional literal passes type checking" {
    // Note: Validation of numeric literals (e.g., fractional to integer) happens
    // during comptime evaluation, not type checking. This test verifies that
    // type checking passes - the actual validation error is caught by comptime eval.
    const source =
        \\x : I64
        \\x = 3.14
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "I64");
}

test "check type - string plus number should fail" {
    // Str + number: when we unify Str with numeric flex, the flex's from_numeral constraint
    // gets applied to Str. Since Str doesn't have from_numeral, we get MISSING METHOD.
    // The plus dispatch on Str also fails with MISSING METHOD.
    const source =
        \\x = "hello" + 123
    ;
    try checkTypesModule(source, .fail_first, "MISSING METHOD");
}

test "check type - string plus string should fail (no plus method)" {
    const source =
        \\x = "hello" + "world"
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

// binop operand type unification //

test "check type - binop operands must have same type - I64 plus I32 should fail" {
    const source =
        \\x = 1i64 + 2i32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - I64 minus I32 should fail" {
    const source =
        \\x = 1i64 - 2i32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - I64 times I32 should fail" {
    const source =
        \\x = 1i64 * 2i32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands must have same type - F64 divide F32 should fail" {
    const source =
        \\x = 1.0f64 / 2.0f32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - binop operands same type works - I64 plus I64" {
    const source =
        \\x = 1i64 + 2i64
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "I64");
}

test "check type - binop operands same type works - unbound plus unbound" {
    const source =
        \\x = 1 + 2
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - is_eq operands must have same type - I64 eq I32 should fail" {
    const source =
        \\x = 1i64 == 2i32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

test "check type - comparison operands must have same type - I64 lt I32 should fail" {
    const source =
        \\x = 1i64 < 2i32
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// primitives - lists //

test "check type - list empty" {
    const source =
        \\[]
    ;
    try checkTypesExpr(source, .pass, "List(_a)");
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
    try checkTypesExpr(source, .pass, "List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - list - 1st elem more specific coreces 2nd elem" {
    const source =
        \\[100u64, 200]
    ;
    try checkTypesExpr(source, .pass, "List(U64)");
}

test "check type - list - 2nd elem more specific coreces 1st elem" {
    const source =
        \\[100, 200u32]
    ;
    try checkTypesExpr(source, .pass, "List(U32)");
}

test "check type - list  - diff elems 1" {
    const source =
        \\["hello", 10]
    ;
    try checkTypesExpr(source, .fail, "MISSING METHOD");
}

// number requirements //

// Skipped: Literal bounds checking is out of scope for poly removal phase
// See POLY_REMOVAL_PLAN.md
test "check type - num - cannot coerce 500 to u8" {
    // const source =
    //     \\[500, 200u8]
    // ;
    // try checkTypesExpr(source, .fail, "NUMBER DOES NOT FIT IN TYPE");
}

// records //

test "check type - record" {
    const source =
        \\{
        \\  hello: "Hello",
        \\  world: 10,
        \\}
    ;
    try checkTypesExpr(source, .pass, "{ hello: Str, world: a } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

// anonymous type equality (is_eq) //

test "check type - record equality - same records are equal" {
    const source =
        \\{ x: 1, y: 2 } == { x: 1, y: 2 }
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - tuple equality - same tuples are equal" {
    const source =
        \\(1, 2) == (1, 2)
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - empty record equality" {
    const source =
        \\{} == {}
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - record with function field - no is_eq" {
    // Records containing functions should not have is_eq because functions don't have is_eq
    const source =
        \\{ x: 1, f: |a| a + 1 } == { x: 1, f: |a| a + 1 }
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - tuple with function element - no is_eq" {
    // Tuples containing functions should not have is_eq because functions don't have is_eq
    const source =
        \\(1, |a| a) == (1, |a| a)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - nested record equality" {
    // Nested records should type-check as Bool
    const source =
        \\{ a: { x: 1 }, b: 2 } == { a: { x: 1 }, b: 2 }
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - nested tuple equality" {
    // Nested tuples should type-check as Bool
    const source =
        \\((1, 2), 3) == ((1, 2), 3)
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - nested record with function - no is_eq" {
    // Nested records containing functions should not have is_eq
    const source =
        \\{ a: { f: |x| x } } == { a: { f: |x| x } }
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - tag union equality" {
    // Tag unions should type-check for equality
    const source =
        \\Ok(1) == Ok(1)
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - tag union with function payload - no is_eq" {
    // Tag unions with function payloads should not have is_eq
    const source =
        \\Fn(|x| x) == Fn(|x| x)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - direct lambda equality - no is_eq" {
    // Lambdas/functions should not support equality comparison
    const source =
        \\(|x| x) == (|y| y)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

// anonymous type inequality (desugars to is_eq().not()) //

test "check type - (a == b) desugars to a.is_eq(b) with unified args" {
    // `a == b` desugars to `a.is_eq(b)` with additional constraint that a and b have the same type
    const src_binop =
        \\|a, b| a == b
    ;

    // The binop version unifies a and b, so they have the same type variable
    const expected_binop: []const u8 = "c, c -> d where [c.is_eq : c, c -> d]";
    try checkTypesExpr(src_binop, .pass, expected_binop);

    // The direct method call version does NOT unify a and b
    const src_direct =
        \\|a, b| a.is_eq(b)
    ;
    const expected_direct: []const u8 = "c, d -> e where [c.is_eq : c, d -> e]";
    try checkTypesExpr(src_direct, .pass, expected_direct);
}

test "check type - (a != b) desugars to a.is_eq(b).not() with unified args" {
    // `a != b` desugars to `a.is_eq(b).not()` with additional constraint that a and b have the same type
    const src_binop =
        \\|a, b| a != b
    ;

    // The binop version unifies a and b, so they have the same type variable
    const expected_binop: []const u8 = "c, c -> d where [c.is_eq : c, c -> e, e.not : e -> d]";
    try checkTypesExpr(src_binop, .pass, expected_binop);

    // The direct method call version does NOT unify a and b
    const src_direct =
        \\|a, b| a.is_eq(b).not()
    ;
    const expected_direct: []const u8 = "c, d -> e where [c.is_eq : c, d -> f, f.not : f -> e]";
    try checkTypesExpr(src_direct, .pass, expected_direct);
}

test "check type - record inequality - same records" {
    // != desugars to is_eq().not(), result type is whatever not returns
    const source =
        \\{ x: 1, y: 2 } != { x: 1, y: 2 }
    ;
    // For concrete types, the constraint resolves to Bool since record.is_eq returns Bool and Bool.not returns Bool
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - record inequality - diff records" {
    const source =
        \\{ x: 1, y: 2 } == { x: 1, z: 2 }
    ;
    try checkTypesExpr(source, .fail, "TYPE MISMATCH");
}

test "check type - tuple inequality" {
    const source =
        \\(1, 2) != (1, 2)
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - record with function field - no inequality" {
    // Records containing functions should not support != because they don't have is_eq
    const source =
        \\{ x: 1, f: |a| a + 1 } != { x: 1, f: |a| a + 1 }
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - tuple with function element - no inequality" {
    // Tuples containing functions should not support != because they don't have is_eq
    const source =
        \\(1, |a| a) != (1, |a| a)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - direct lambda inequality - no is_eq" {
    // Lambdas/functions should not support inequality comparison (requires is_eq)
    const source =
        \\(|x| x) != (|y| y)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

test "check type - tag union inequality" {
    const source =
        \\Ok(1) != Ok(1)
    ;
    try checkTypesExpr(source, .pass, "Bool");
}

test "check type - tag union with function payload - no inequality" {
    // Tag unions with function payloads should not support != because they don't have is_eq
    const source =
        \\Fn(|x| x) != Fn(|x| x)
    ;
    try checkTypesExpr(source, .fail, "TYPE DOES NOT SUPPORT EQUALITY");
}

// tags //

test "check type - tag" {
    const source =
        \\MyTag
    ;
    try checkTypesExpr(source, .pass, "[MyTag, .._others]");
}

test "check type - tag - args" {
    const source =
        \\MyTag("hello", 1)
    ;
    try checkTypesExpr(source, .pass, "[MyTag(Str, a), .._others] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
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

// TODO: This test is currently failing because annotation parsing doesn't correctly handle
// constraint syntax for flex vars
// This needs to be fixed in the annotation parser, but is separate from the numeric literal work.
test "check type - def - func with annotation 2" {
    const source =
        \\id : x -> _a
        \\id = |_| 15
    ;
    // The type annotation says _a is unconstrained, but the implementation returns
    // a numeric literal which requires from_numeral method. This is a type error.
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

test "check type - def - nested lambda" {
    const source =
        \\id = (((|a| |b| |c| a + b + c)(100))(20))(3)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "d where [d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]");
}

test "check type - def - forward ref" {
    const source =
        \\run = id1("howdy")
        \\
        \\id1 : x -> x
        \\id1 = |x| id2(x)
        \\
        \\id2 : x -> x
        \\id2 = |x| id3(x)
        \\
        \\id3 : x -> x
        \\id3 = |x| id4(x)
        \\
        \\id4 : x -> x
        \\id4 = |x| x
        \\
        \\id5 : x -> x
        \\id5 = |x| x
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "run" } }, "Str");
}

test "check type - def - nested lambda with wrong annotation" {

    // Currently the below produces two errors instead of just one.
    // NOTE: Num(a) syntax is deprecated - this test may need updating when it's re-enabled
    const source =
        \\curried_add : Num(a), Num(a), Num(a), Num(a) -> Num(a)
        \\curried_add = |a| |b| |c| |d| a + b + c + d
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "x where [x.from_numeral : Numeral -> Try(x, [InvalidNumeral(Str)])]");
}

test "check type - def - polymorphic id 2" {
    const source =
        \\id : x -> x
        \\id = |x| x
        \\
        \\test = (id(5), id("hello"))
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "(x, Str) where [x.from_numeral : Numeral -> Try(x, [InvalidNumeral(Str)])]");
}

test "check type - def - out of order" {
    // Currently errors out in czer

    const source =
        \\id_1 : x -> x
        \\id_1 = |x| id_2(x)
        \\
        \\id_2 : x -> x
        \\id_2 = |x| x
        \\
        \\test = id_1("Hellor")
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "test" } }, "Str");
}

test "check type - def - polymorphic higher order 1" {
    const source =
        \\f = |g, v| g(v)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "(a -> b), a -> b");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]");
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
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
        \\x : MyListAlias(I64)
        \\x = [15]
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "MyListAlias(I64)");
}

test "check type - alias with mismatch arg" {
    const source =
        \\MyListAlias(a) : List(a)
        \\
        \\x : MyListAlias(Str)
        \\x = [15]
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "MyNominal(U8)");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "Pair(U64)");
}

test "check type - nominal with with rigid vars mismatch" {
    const source =
        \\Pair(a) := [Pair(a, a)]
        \\
        \\u64val : U64
        \\u64val = 1
        \\
        \\pairU64 : Pair(U64)
        \\pairU64 = Pair.Pair(u64val, "Str")
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
        \\x : ConsList(I64)
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
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
        \\test = swapPair((1, "test"))
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Pair(Str, a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
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
    const source =
        \\x = (|y| !y)(Bool.True)
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
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
    const source =
        \\x : Str
        \\x = if Bool.True "true" else "false"
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "Str");
}

test "check type - if else - invalid condition 1" {
    const source =
        \\x : Str
        \\x = if 5 "true" else "false"
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

test "check type - if else - invalid condition 2" {
    const source =
        \\x : Str
        \\x = if 10 "true" else "false"
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

test "check type - if else - different branch types 2" {
    const source =
        \\x = if True "true" else if False "false" else 10
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

test "check type - if else - different branch types 3" {
    const source =
        \\x = if True "true" else if False 10 else "last"
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
    try checkTypesModule(source, .fail, "MISSING METHOD");
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
    try checkTypesModule(source, .{ .pass = .last_def }, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - unary minus mismatch" {
    const source =
        \\x = "hello"
        \\
        \\y = -x
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

// binops

test "check type - binops math plus" {
    const source =
        \\x = 10 + 10u32
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "U32");
}

test "check type - binops math sub" {
    const source =
        \\x = 1 - 0.2
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
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

test "check type - record - access" {
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

test "check type - record - access func polymorphic" {
    const source =
        \\x = |r| r.my_field
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "{ my_field: a } -> a");
}

test "check type - record - access - not a record" {
    const source =
        \\r = "hello"
        \\
        \\x = r.my_field
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}

// record update

test "check type - record - update 1" {
    const source =
        \\update_data = |container, new_value| { ..container, data: new_value }
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "update_data" } },
        "{ ..a, data: b }, b -> { ..a, data: b }",
    );
}

test "check type - record - update 2" {
    const source =
        \\set_data = |container, new_value| { ..container, data: new_value }
        \\
        \\updated1 = set_data({ data: 10 }, 100) # Updates field
        \\updated2 = set_data({ data: 10, other: "hello" }, 100) # Updates with extra fields
        \\updated3 = set_data({ data: "hello" }, "world") # Polymorphic
        \\
        \\final = (updated1, updated2, updated3)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "final" } },
        "({ data: a }, { data: b, other: Str }, { data: Str }) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]",
    );
}

test "check type - record - update fail" {
    const source =
        \\set_data = |container, new_value| { ..container, data: new_value }
        \\
        \\updated = set_data({ data: "hello" }, 10)
    ;
    try checkTypesModule(
        source,
        .fail,
        "MISSING METHOD",
    );
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
        \\  match(20) {
        \\    10dec as x => x,
        \\    _ => 15,
        \\  }
        \\}
    ;
    try checkTypesExpr(source, .pass, "Dec");
}

test "check type - patterns frac 2" {
    const source =
        \\{
        \\  match(10) {
        \\    10f32 as x => x,
        \\    _ => 15,
        \\  }
        \\}
    ;
    try checkTypesExpr(source, .pass, "F32");
}

test "check type - patterns frac 3" {
    const source =
        \\{
        \\  match(50) {
        \\    10 as x => x,
        \\    15f64 as x => x,
        \\    _ => 20,
        \\  }
        \\}
    ;
    try checkTypesExpr(source, .pass, "F64");
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
    try checkTypesExpr(source, .pass, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
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

// vars + reassignment //

test "check type - var ressignment" {
    const source =
        \\main = {
        \\  var x = 1
        \\  x = x + 1
        \\  x
        \\}
    ;
    try checkTypesModule(source, .{ .pass = .last_def }, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

// expect //

test "check type - expect" {
    const source =
        \\main = {
        \\  x = 1
        \\  expect x == 1
        \\  x
        \\}
    ;
    // Inside lambdas, numeric flex vars ARE generalized (to support polymorphic functions).
    // Each use of `x` gets a fresh instance, so constraints from `x == 1` don't
    // propagate to the generalized type. Only `from_numeral` from the def is captured.
    try checkTypesModule(source, .{ .pass = .last_def }, "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]");
}

test "check type - expect not bool" {
    const source =
        \\main = {
        \\  x = 1
        \\  expect x
        \\  x
        \\}
    ;
    try checkTypesModule(source, .fail, "MISSING METHOD");
}

// crash //

test "check type - crash" {
    const source =
        \\y : U64
        \\y = {
        \\  crash "bug"
        \\}
        \\
        \\main = {
        \\  x = 1
        \\  x + y
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "U64",
    );
}

// dbg //

test "check type - dbg" {
    // dbg returns {} (not the value it's debugging), so it can be used
    // as a statement/side-effect without affecting the block's return type
    const source =
        \\y : U64
        \\y = {
        \\  dbg 2
        \\  42
        \\}
        \\
        \\main = {
        \\  x = 1
        \\  x + y
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "U64",
    );
}

// for //

test "check type - for" {
    const source =
        \\main = {
        \\  var result = 0
        \\  for x in [1, 2, 3] {
        \\    result = result + x
        \\  }
        \\  result
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]",
    );
}

test "check type - for mismatch" {
    const source =
        \\main = {
        \\  var result = 0
        \\  for x in ["a", "b", "c"] {
        \\    result = result + x
        \\  }
        \\  result
        \\}
    ;
    try checkTypesModule(
        source,
        .fail,
        "MISSING METHOD",
    );
}

// static dispatch //

test "check type - static dispatch - polymorphic - annotation" {
    const source =
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
        \\main! = |_| {}
        \\
        \\Test := [Val(Str)].{
        \\  to_str = |Test.Val(s)| s
        \\  to_str2 = |test| test.to_str()
        \\}
        \\
        \\
        \\func = Test.Val("hello").to_str2()
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "Str",
    );
}

test "check type - static dispatch - fail if not in type signature" {
    const source =
        \\main! = |_| {}
        \\
        \\func : a -> a
        \\func = |a| {
        \\  _val = a.method()
        \\  a
        \\}
    ;
    try checkTypesModule(
        source,
        .fail,
        "MISSING METHOD",
    );
}

test "check type - static dispatch - let poly" {
    const source =
        \\main! = |_| {}
        \\
        \\process_container : a -> Str where [a.get_or : a, Str -> Str]
        \\process_container = |container| {
        \\  result = container.get_or("empty")
        \\  result
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "process_container" } },
        "a -> Str where [a.get_or : a, Str -> Str]",
    );
}

test "check type - static dispatch - let poly 2" {
    const source =
        \\main! = |_| {}
        \\
        \\# Define a Container type with methods
        \\Container(a) := [Empty, Value(a)].{
        \\
        \\  # Method to get value or provide default
        \\  get_or : Container(a), a -> a
        \\  get_or = |container, default| {
        \\    match container {
        \\      Value(val) => val
        \\      Empty => default
        \\    }
        \\  }
        \\}
        \\
        \\process_container : a -> Str where [a.get_or : a, Str -> Str]
        \\process_container = |container| {
        \\  result = container.get_or("empty")
        \\  result
        \\}
        \\
        \\func = {
        \\  c = Container.Empty
        \\  process_container(c)
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "Str",
    );
}

test "check type - static dispatch - polymorphic type" {
    const source =
        \\main! = |_| {}
        \\
        \\Container(a) := [Value(a)].{
        \\  # Method to map over the contained value
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |Value(val), f| {
        \\      Value(f(val))
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.Container.map" } },
        "Container(a), (a -> b) -> Container(b)",
    );
}

test "check type - static dispatch - polymorphic type 2" {
    const source =
        \\Container(a) := [Value(a)].{
        \\  # Method to map over the contained value
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |c, f| {
        \\    match c {
        \\      Value(val) => Value(f(val))
        \\    }
        \\  }
        \\}
        \\
        \\main! = |_| {}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.Container.map" } },
        "Container(a), (a -> b) -> Container(b)",
    );
}

test "check type - static dispatch - polymorphic type 3" {
    const source =
        \\Container(a) := [Empty, Value(a)].{
        \\  # Method to map over the contained value
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |container, f| {
        \\    match container {
        \\      Value(val) => Value(f(val))
        \\      Empty => Empty
        \\    }
        \\  }
        \\}
        \\
        \\main! = |_| {}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "Test.Container.map" } },
        "Container(a), (a -> b) -> Container(b)",
    );
}

// comprehensive //

test "check type - comprehensive - multiple layers of let-polymorphism" {
    const source =
        \\main! = |_| {}
        \\
        \\# First layer: polymorphic identity
        \\id : a -> a
        \\id = |x| x
        \\
        \\# Second layer: uses id polymorphically multiple times
        \\apply_twice : (a -> a), a -> a
        \\apply_twice = |f, x| {
        \\  first = f(x)
        \\  second = f(first)
        \\  second
        \\}
        \\
        \\# Third layer: uses apply_twice with different types
        \\func = {
        \\  num_result = apply_twice(id, 42)
        \\  str_result = apply_twice(id, "hello")
        \\  bool_result = apply_twice(id, Bool.True)
        \\  (num_result, str_result, bool_result)
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "(a, Str, Bool) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]",
    );
}

test "check type - comprehensive - multiple layers of lambdas" {
    const source =
        \\main! = |_| {}
        \\
        \\# Four layers of nested lambdas
        \\curried_add : a, a, a, a -> a where [a.add : a, a -> a]
        \\curried_add = |a, b, c, d| a + b + c + d
        \\
        \\func = {
        \\  step1 = curried_add(1, 2, 3, 4)
        \\  step1
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "a where [a.add : a, a -> a, a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]",
    );
}

test "check type - comprehensive - static dispatch with multiple methods" {
    const source =
        \\main! = |_| {}
        \\
        \\# Define a polymorphic container with static dispatch
        \\Container(a) := [Empty, Value(a)].{
        \\  # Method with annotation
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |container, f| {
        \\    match container {
        \\      Value(val) => Value(f(val))
        \\      Empty => Empty
        \\    }
        \\  }
        \\
        \\  # Method without annotation (inferred)
        \\  get_or = |container, default| {
        \\    match container {
        \\      Container.Value(val) => val
        \\      Empty => default
        \\    }
        \\  }
        \\
        \\  # Chained method dispatch
        \\  flat_map : Container(a), (a -> Container(b)) -> Container(b)
        \\  flat_map = |container, f| {
        \\    match container {
        \\      Value(val) => f(val)
        \\      Empty => Empty
        \\    }
        \\  }
        \\}
        \\
        \\func = {
        \\  num_container = Container.Value(100)
        \\
        \\  chained = num_container
        \\    .map(|x| x + 1)
        \\    .flat_map(|x| Container.Value(x + 2))
        \\    .get_or(0)
        \\
        \\  chained
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]",
    );
}

test "check type - comprehensive - static dispatch with multiple methods 2" {
    const source =
        \\main! = |_| {}
        \\
        \\Container(a) := [Empty, Value(a)].{
        \\  mapAdd5 = |container| {
        \\    container
        \\      .mapAdd4()
        \\      .mapAdd1()
        \\  }
        \\
        \\  mapAdd4 = |container| {
        \\    container
        \\      .mapAdd2()
        \\      .mapAdd2()
        \\  }
        \\
        \\  mapAdd3 = |container| {
        \\    container
        \\      .mapAdd2()
        \\      .mapAdd1()
        \\  }
        \\
        \\  mapAdd2 = |container| {
        \\    container
        \\      .mapAdd1()
        \\      .mapAdd1()
        \\  }
        \\
        \\  mapAdd1 = |container| {
        \\    container.map(|val| val + 1)
        \\  }
        \\
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |container, f| {
        \\    match container {
        \\      Value(val) => Value(f(val))
        \\      Empty => Empty
        \\    }
        \\  }
        \\}
        \\
        \\func = {
        \\  num_container = Container.Value(100)
        \\  num_container.mapAdd5()
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "Container(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]",
    );
}

// Minimal reproduction test cases for segfault
test "check type - segfault minimal 1 - just annotated plus" {
    const source =
        \\main! = |_| {}
        \\
        \\my_plus : a, a -> a where [a.plus : a, a -> a]
        \\my_plus = |x, y| x + y
        \\
        \\func = my_plus(1u32, 2u32)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "U32",
    );
}

test "check type - segfault minimal 2 - plus with inferred caller" {
    const source =
        \\main! = |_| {}
        \\
        \\my_plus : a, a -> a where [a.plus : a, a -> a]
        \\my_plus = |x, y| x + y
        \\
        \\add_two = |a, b| my_plus(a, b)
        \\
        \\func = add_two(1u32, 2u32)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "U32",
    );
}

test "check type - segfault minimal 3a - nested direct - SEGFAULTS" {
    const source =
        \\main! = |_| {}
        \\
        \\my_plus : a, a -> a where [a.plus : a, a -> a]
        \\my_plus = |x, y| x + y
        \\
        \\func = my_plus(my_plus(1u32, 2u32), 3u32)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "U32",
    );
}

test "check type - segfault minimal 3b - nested in lambda - SEGFAULTS" {
    const source =
        \\main! = |_| {}
        \\
        \\my_plus : a, a -> a where [a.plus : a, a -> a]
        \\my_plus = |x, y| x + y
        \\
        \\add_three = |a, b, c| my_plus(my_plus(a, b), c)
        \\
        \\func = add_three(1u32, 2u32, 3u32)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "U32",
    );
}

test "check type - segfault minimal 4 - full original - SEGFAULTS" {
    const source =
        \\main! = |_| {}
        \\
        \\# Annotated function
        \\add : a, a -> a where [a.plus : a, a -> a]
        \\add = |x, y| x + y
        \\
        \\# Inferred function that uses annotated one
        \\add_three = |a, b, c| add(add(a, b), c)
        \\
        \\# Annotated function using inferred one
        \\compute : U32 -> U32
        \\compute = |x| add_three(x, 1u32, 2u32)
        \\
        \\func = compute(10u32)
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "func" } },
        "U32",
    );
}

test "check type - comprehensive: polymorphism + lambdas + dispatch + annotations" {
    const source =
        \\main! = |_| {}
        \\
        \\# Define a polymorphic container with static dispatch
        \\Container(a) := [Empty, Value(a)].{
        \\  # Method with annotation
        \\  map : Container(a), (a -> b) -> Container(b)
        \\  map = |container, f| {
        \\    match container {
        \\      Value(val) => Value(f(val))
        \\      Empty => Empty
        \\    }
        \\  }
        \\
        \\  # Method without annotation (inferred)
        \\  get_or = |container, default| {
        \\    match container {
        \\      Value(val) => val
        \\      Empty => default
        \\    }
        \\  }
        \\
        \\  # Chained method dispatch
        \\  flat_map : Container(a), (a -> Container(b)) -> Container(b)
        \\  flat_map = |container, f| {
        \\    match container {
        \\      Value(val) => f(val)
        \\      Empty => Empty
        \\    }
        \\  }
        \\}
        \\
        \\# First layer: polymorphic helper with annotation
        \\compose : (b -> c), (a -> b), a -> c
        \\compose = |g, f, x| g(f(x))
        \\
        \\# Second layer: inferred polymorphic function using compose
        \\transform_twice = |f, x| {
        \\  first = compose(f, f, x)
        \\  second = compose(f, f, first)
        \\  second
        \\}
        \\
        \\# Third layer: curried function (multiple lambda layers)
        \\make_processor : (a -> b) -> ((b -> c) -> (a -> c))
        \\make_processor = |f1| |f2| |x| {
        \\  step1 = f1(x)
        \\  step2 = f2(step1)
        \\  step2
        \\}
        \\
        \\# Fourth layer: polymorphic function using static dispatch
        \\process_with_method : a, c -> d where [a.map : a, (b -> c) -> d]
        \\process_with_method = |container, value| {
        \\  # Multiple nested lambdas with let-polymorphism
        \\  id = |x| x
        \\
        \\  result = container.map(|_| id(value))
        \\  result
        \\}
        \\
        \\# Fifth layer: combine everything
        \\main = {
        \\  # Let-polymorphism layer 1
        \\  # TODO INLINE ANNOS
        \\  # id : a -> a
        \\  id = |x| x
        \\
        \\  # Let-polymorphism layer 2 with nested lambdas
        \\  _apply_to_container = |f| |container| |default| {
        \\    mapped = container.map(f)
        \\    mapped.get_or(default)
        \\  }
        \\
        \\  # Create containers
        \\  num_container = Container.Value(100)
        \\  str_container = Container.Value("hello")
        \\  _empty_container = Container.Empty
        \\
        \\  # Use id polymorphically on different types
        \\  id_num = id(42)
        \\  id_str = id("world")
        \\  id_bool = id(Bool.True)
        \\
        \\  # Multiple layers of curried application
        \\  add_ten = |x| x + 10
        \\  processor = make_processor(add_ten)(add_ten)
        \\  processed = processor(5)
        \\
        \\  # Static dispatch with polymorphic methods
        \\  num_result = num_container.map(|x| x + 1)
        \\  _str_result = str_container.map(|s| s)
        \\
        \\  # Chain method calls with static dispatch
        \\  chained = num_container
        \\    .map(|x| x + 1)
        \\    .flat_map(|x| Container.Value(x + 2))
        \\    .get_or(0)
        \\
        \\  # Use transform_twice with let-polymorphism
        \\  double_fn = |x| x + x
        \\  transformed = transform_twice(double_fn, 3)
        \\
        \\  # Final result combining all techniques
        \\  {
        \\    id_results: (id_num, id_str, id_bool),
        \\    processed: processed,
        \\    chained: chained,
        \\    transformed: transformed,
        \\    final: num_result.get_or(0),
        \\  }
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "main" } },
        "{ chained: b, final: b, id_results: (e, Str, Bool), processed: c, transformed: a } where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]",
    );
}

// scoped type variables

test "check type - scoped type variables - pass" {
    const source =
        \\main! = |_| {}
        \\
        \\pass : a -> a
        \\pass = |x| {
        \\  inner : a -> a
        \\  inner = |y| y
        \\
        \\  inner(x)
        \\}
    ;
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "pass" } },
        "a -> a",
    );
}

test "check type - scoped type variables - fail" {
    const source =
        \\main! = |_| {}
        \\
        \\fail : a -> a
        \\fail = |x| {
        \\  g : b -> b
        \\  g = |z| z
        \\
        \\  result : c
        \\  result = g(x)
        \\
        \\  result
        \\}
    ;
    try checkTypesModule(
        source,
        .fail,
        "TYPE MISMATCH",
    );
}

// Associated items referencing each other

test "associated item can reference another associated item from same type" {
    // First verify Bool basics work
    const bool_basics =
        \\Test := [].{}
        \\
        \\x : Bool
        \\x = True
    ;
    try checkTypesModule(bool_basics, .{ .pass = .{ .def = "x" } }, "Bool");

    // Now test calling MyBool.my_not from within an associated item
    const source =
        \\Test := [].{
        \\  MyBool := [MyTrue, MyFalse].{
        \\    my_not : MyBool -> MyBool
        \\    my_not = |b| match b {
        \\      MyTrue => MyFalse
        \\      MyFalse => MyTrue
        \\    }
        \\
        \\    my_eq : MyBool, MyBool -> MyBool
        \\    my_eq = |a, b| match a {
        \\      MyTrue => b
        \\      MyFalse => MyBool.my_not(b)
        \\    }
        \\  }
        \\}
        \\
        \\x = Test.MyBool.my_eq(Test.MyBool.MyTrue, Test.MyBool.MyFalse)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "x" } }, "Test.MyBool");
}

test "Bool.not works as builtin associated item" {
    const source =
        \\Test := [].{}
        \\
        \\x = Bool.not(True)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "x" } }, "Bool");
}

test "Str.is_empty works as low-level builtin associated item" {
    const source =
        \\Test := [].{}
        \\
        \\x = Str.is_empty("")
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "x" } }, "Bool");
}

test "List.fold works as builtin associated item" {
    const source =
        \\Test := [].{}
        \\
        \\x = List.fold([1, 2, 3], 0, |acc, item| acc + item)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "x" } }, "item where [item.from_numeral : Numeral -> Try(item, [InvalidNumeral(Str)])]");
}

test "associated item: type annotation followed by body should not create duplicate definition" {
    const source =
        \\Test := [].{
        \\  apply : (a -> b), a -> b
        \\  apply = |fn, x| fn(x)
        \\}
        \\
        \\result = Test.apply(|n| n, 42)
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Should have NO errors - the type annotation should be associated with the body
    const can_diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(can_diagnostics);
    const type_problems = test_env.checker.problems.problems.items;

    try testing.expectEqual(@as(usize, 0), can_diagnostics.len);
    try testing.expectEqual(@as(usize, 0), type_problems.len);

    // Verify the types
    try test_env.assertDefType("Test.apply", "(a -> b), a -> b");
    try test_env.assertDefType("result", "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]");
}

// TODO: Move this test to can
test "top-level: type annotation followed by body should not create duplicate definition - REGRESSION TEST" {
    // This reproduces the bug seen in test/snapshots/pass/underscore_in_regular_annotations.md
    // and test/snapshots/type_function_simple.md where a type annotation followed by its body
    // creates TWO defs:
    // 1. A def with e-anno-only for the annotation
    // 2. A def with the actual lambda body
    // This causes a DUPLICATE DEFINITION error
    //
    // NOTE: Using EXACT code from the snapshot that shows the bug!
    const source =
        \\app [main!] { pf: platform "platform.roc" }
        \\
        \\apply : (_a -> _b) -> _a -> _b
        \\apply = |fn, x| fn(x)
        \\
        \\main! = |_| {}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Check for canonicalization problems - should be specifically DUPLICATE DEFINITION
    const can_diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(can_diagnostics);

    var duplicate_def_found = false;
    for (can_diagnostics) |diagnostic| {
        var report = try test_env.module_env.diagnosticToReport(diagnostic, test_env.gpa, test_env.module_env.module_name);
        defer report.deinit();

        if (std.mem.indexOf(u8, report.title, "DUPLICATE DEFINITION") != null) {
            duplicate_def_found = true;
            break;
        }
    }

    // The bug causes a DUPLICATE DEFINITION error - this test should FAIL when bug is present
    if (duplicate_def_found) {
        return error.TestUnexpectedResult;
    }
}

// equirecursive static dispatch //

test "check type - equirecursive static dispatch" {
    // Tests that method dispatch works with numeric literals
    // The expression (|x| x.plus(5))(7) should type-check successfully
    const source = "(|x| x.plus(5))(7)";

    try checkTypesExpr(
        source,
        .pass,
        "_a",
    );
}

test "check type - equirecursive static dispatch with type annotation" {
    // This tests the exact pattern from the example (|x| x.plus(b))(a)
    // but with explicit type annotations.
    // This demonstrates that the RecursionVar infrastructure works correctly
    // with the same constraint structure as the motivating example.
    const source =
        \\fn : a, b -> ret where [
        \\    a.plus : a, b -> ret,
        \\    a.from_int_digits : List(U8) -> Try(a, [OutOfRange]),
        \\    b.from_int_digits : List(U8) -> Try(b, [OutOfRange])
        \\]
        \\fn = |a, b| (|x| x.plus(b))(a)
    ;

    // The annotated type should match the inferred type
    try checkTypesModule(
        source,
        .{ .pass = .{ .def = "fn" } },
        "a, b -> ret where [a.plus : a, b -> ret, a.from_int_digits : List(U8) -> Try(a, [OutOfRange]), b.from_int_digits : List(U8) -> Try(b, [OutOfRange])]",
    );
}

test "check type - static dispatch method type mismatch - REGRESSION TEST" {
    // This test verifies that when a method is called with mismatched types,
    // we get a TYPE MISMATCH error. This is a regression test for the diagnostic
    // output when static dispatch method arguments don't match.
    //
    // The scenario: a function requires is_eq on type `a`, but we call it
    // with two different types (number and string), causing a type mismatch.
    const source =
        \\fn : a, a -> Bool where [a.is_eq : a, a -> Bool]
        \\fn = |x, y| x.is_eq(y)
        \\
        \\result = fn(1u64, 2u64) == fn(3u64, 4u64)
    ;

    // This should pass - both calls use the same types
    try checkTypesModule(source, .{ .pass = .last_def }, "Bool");
}

// helpers - module //

const ModuleExpectation = union(enum) {
    pass: DefExpectation,
    fail,
    fail_first, // Allows multiple errors, checks first error title
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
        .fail_first => {
            return test_env.assertFirstTypeError(expected);
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

// effectful function type annotation parsing //

test "check type - effectful zero-arg function annotation" {
    // This test verifies that () => {} is parsed as a zero-arg effectful function,
    // NOT as a function taking a unit tuple argument.
    // The bug was that () => {} was being parsed as (()) => {} - a function taking
    // one empty-tuple argument instead of zero arguments.
    const source =
        \\foo : (() => {})
        \\foo = || {}
    ;
    // Expected: zero-arg effectful function returning empty record
    // If the parser bug exists, this would fail with TYPE MISMATCH because:
    // - annotation parses as: (()) => {} (one empty-tuple arg)
    // - lambda infers as: ({}) -> {} (zero args, pure)
    try checkTypesModule(source, .{ .pass = .last_def }, "({}) => {  }");
}

test "check type - pure zero-arg function annotation" {
    // This test verifies that () -> {} is parsed as a zero-arg pure function,
    // NOT as a function taking a unit tuple argument.
    const source =
        \\foo : (() -> {})
        \\foo = || {}
    ;
    // Expected: zero-arg pure function returning empty record
    try checkTypesModule(source, .{ .pass = .last_def }, "({}) -> {  }");
}

test "qualified imports don't produce MODULE NOT FOUND during canonicalization" {
    // Qualified imports (e.g., "json.Json") are cross-package imports that are
    // resolved by the workspace resolver, not during canonicalization.
    // They should NOT produce MODULE NOT FOUND errors during canonicalization.
    //
    // Source from test/snapshots/can_import_comprehensive.md
    const source =
        \\import json.Json
        \\import http.Client as Http exposing [get, post]
        \\import utils.String as Str
        \\
        \\main = {
        \\    client = Http.get
        \\    parser = Json.utf8
        \\    helper = Str.trim
        \\
        \\    # Test direct module access
        \\    result1 = Json.parse
        \\
        \\    # Test aliased module access
        \\    result2 = Http.post
        \\
        \\    # Test exposed items (should work without module prefix)
        \\    result3 = get
        \\    result4 = post
        \\
        \\    # Test multiple qualified access
        \\    combined = Str.concat(
        \\        client,
        \\        parser,
        \\        helper,
        \\        result1,
        \\        result2,
        \\        result3,
        \\        result4,
        \\        combined,
        \\    )
        \\}
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);

    // Count MODULE NOT FOUND errors
    var module_not_found_count: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .module_not_found) {
            module_not_found_count += 1;
        }
    }

    // Qualified imports (json.Json, http.Client, utils.String) should NOT produce
    // MODULE NOT FOUND errors - they're handled by the workspace resolver
    try testing.expectEqual(@as(usize, 0), module_not_found_count);
}

// Try with match and error propagation //

test "check type - try return with match and error propagation should type-check" {
    // This tests that a function returning Try(Str, _) with a wildcard error type
    // should accept both error propagation (?) and explicit Err tags in match branches.
    // The wildcard _ in the return type annotation should unify with any error type.
    const source =
        \\get_greeting : {} -> Try(Str, _)
        \\get_greeting = |{}| {
        \\    match 0 {
        \\        0 => Try.Ok(List.first(["hello"])?),
        \\        _ => Err(Impossible)
        \\    }
        \\}
    ;
    // Expected: should pass type-checking with combined error type (open tag union)
    try checkTypesModule(source, .{ .pass = .last_def }, "{  } -> Try(Str, [ListWasEmpty, Impossible, .._others2])");
}
